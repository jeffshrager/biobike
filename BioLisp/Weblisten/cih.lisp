;;; -*- mode: Lisp; Syntax: Common-Lisp; Package: WEBLISTENER; -*-

(in-package :weblistener)

;;; +=========================================================================+
;;; | Copyright (c) 2002, 2003, 2004 JP Massar, Jeff Shrager, Mike Travers    |
;;; |                                                                         |
;;; | Permission is hereby granted, free of charge, to any person obtaining   |
;;; | a copy of this software and associated documentation files (the         |
;;; | "Software"), to deal in the Software without restriction, including     |
;;; | without limitation the rights to use, copy, modify, merge, publish,     |
;;; | distribute, sublicense, and/or sell copies of the Software, and to      |
;;; | permit persons to whom the Software is furnished to do so, subject to   |
;;; | the following conditions:                                               |
;;; |                                                                         |
;;; | The above copyright notice and this permission notice shall be included |
;;; | in all copies or substantial portions of the Software.                  |
;;; |                                                                         |
;;; | THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,         |
;;; | EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF      |
;;; | MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  |
;;; | IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY    |
;;; | CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,    |
;;; | TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE       |
;;; | SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.                  |
;;; +=========================================================================+

;;; Author:  JP Massar


;;;; Complete Input History (CIH) storage and retrieval mechanism

;;; All completion files have entries, one per line, that look like
;;; ("<<what the user typed>>" <<timestamp>>)

;;; Completion files are kept one per session.  When the system reboots, 
;;; and a user logs in for the first time, all the session
;;; files are merged into a global session file, ordered by timestamp,
;;; most recent first.

;;; The completions for a session are kept in a hash table local to the
;;; session. The hash table is key=string, value=timestamp.  As the
;;; user types things in they are stored in the session hash table and
;;; written out to the session completion file.


;;; INTERFACE

;;; INITIALIZE-CIH-FOR-USER (user) -- Called when a user logs in for the first 
;;; time. This merges all the session CIH files for a given user created by the 
;;; immediately prior weblistener process.  

;;; INITALIZE-CIH-FOR-SESSION (sessionid) -- Called whenever a new session is
;;; established. This creates a session file in a users CIH subdirectory and 
;;;  creates appropriate in-core data structures to keep track of this 
;;;  session's completion history.  

;;; CIH-STATE (sessionid) -- Returns data structure defining CIH state
;;; for the given session

;;; CIH-CURRENT-MATCH-STRING (sessionid) -- Returns the most recently used
;;; candidate match string, or NIL if none one exists.

;;; SET-CIH-CURRENT-MATCH-STRING (sessionid) -- Sets the value of the
;;; current candidate match string.

;;; CIH-CURRENT-MATCH-COUNT (sessionid) -- Returns the number of times
;;; the CIH-PREVIOUS-MATCH-STRING has been matched to different entries.

;;; SET-CIH-CURRENT-MATCH-COUNT (sessionid) --Sets the value of the
;;; current match count.

;;; MAYBE-SAVE-NEXT-INPUT-HISTORY-ITEM -- This gets called for every 
;;; evaluated user input.  Any input which causes an error during
;;; read is not stored (while input which causes an error during
;;; evaluation is stored).  If the user input is unique it is stored
;;; in-core and in the session file, otherwise it is ignored.  

;;; NTH-CIH-MATCH (initial-substring) -- Returns a unique match for 
;;; initial-substring found in the CIH history, given a unique 
;;; non-negative integer less than or equal to the number of 
;;; matches in the history.  Otherwise returns NIL to indicate
;;; that there are no more matches.  

;;; DO-INPUT-HISTORY-COMPLETION (string) -- Search the completion history
;;; matching STRING as an initial substring; the Nth such match is returned,
;;; where N is one more than the value of CIH-CURRENT-MATCH-COUNT, or 1
;;; if that value is NIL

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +cih-on-disk-limit+ 5000)
  (defconstant +cih-reorder-every+ 20)
  (defconstant +min-completion-user-input-length-to-save+ 10)
  (defvar *cih-directory-name* ".newcih")
  (defvar *cih-global-file-name* "global-cih.lisp")
  (defvar *cih-session-file-prefix* "cih-")
  )

(defstruct cih
  in-memory-hash
  session-file
  global-file
  current-iss
  match-count
  enabled?
  reorder?
  )

;;; A way to avoid dealing with bignums wrt time.  We use as a base
;;; the time the system was loaded instead of Jan 1, 1900.  This won't
;;; work if we eventually ever were to dump an executable.  Would
;;; need to reset this variable in some sort of system startup code.

(defvar *approximate-time-code-was-loaded*
  (multiple-value-bind (s m h date month year)
      (decode-universal-time (get-universal-time))
    (declare (ignore s))
    (encode-universal-time 0 m h date month year)
    ))

(defun seconds-since-code-was-loaded ()
  (- (get-universal-time) *approximate-time-code-was-loaded*))

(defun cih-directory (username)
  (append-subdir (user-directory username) *cih-directory-name*))
(defun cih-global-file-path (username)
  (merge-pathnames *cih-global-file-name* (cih-directory username)))
(defun cih-session-file-name (sessionid)
  (make-pathname
   :name (one-string *cih-session-file-prefix* (string-downcase sessionid))
   :type "lisp"))
(defun cih-session-file-path (username sessionid)
  (merge-pathnames
   (cih-session-file-name sessionid)
   (cih-directory username)
   ))
(defun cih-session-file? (filepath)
  (initial-subsequence-of?
   (pathname-name (pathname filepath)) *cih-session-file-prefix*))
(defun read-next-cih-entry (s) (read s nil nil))
(defun write-next-cih-entry (s entry) (format s "~S~%" entry))

(defun initialize-cih-for-user (username)
  #.(one-string-nl
     "Called by the login code when it detects that a user is logging in"
     "for the first time wrt a given weblistener process."
     "This function consolidates all the user's session cih files from"
     "the previous weblistener process into a single global file.  It returns"
     "the number of entries written to the global file if successful and NIL"
     "if some kind of problem occurred.")
  (block exit 
    (handler-case 
        (let* ((global-cih-file (cih-global-file-path username))
               (temp-cih-file
                (merge-pathnames 
                 (make-pathname :name "temp" :type "cih" :host nil)
                 global-cih-file)))
          ;; ensure that the .cih subdir of the user's home directory exists
          (ensure-directories-exist global-cih-file)
          ;; Find all the session CIH files
          (let* ((cih-session-files 
                  (remove-if-not 
                   (lambda (path)
                     ;; session files begin with "cih-"
                     (and (not (pathname-names-directory? path))
                          (cih-session-file? path)))
                   (directory-with-subdirs-in-directory-form 
                    (cih-directory username))))
                 ;; determine the last write date (time) for each session file
                 ;; and sort the session files by this time.
                 (sorted-cih-session-files
                  (mapcar
                   'first
                   (sort (loop for file in cih-session-files collect
                              (list file (file-write-date file)))
                        '> :key 'second)))
                 (files-to-merge
                  (append sorted-cih-session-files
                          (when (probe-file global-cih-file) 
                            (list global-cih-file)
                            )))
                 (entry-count 0)
                 )
            ;; Write all the entries from all the session files and the
            ;; global file (if it exists) to a temp file.
            ;; Only write at most +cih-on-disk-limit+ entries. 
            ;; When done, rename the temp file to be the global file, 
            ;; delete the existing session files,
            ;; and return number of entries written.
            (with-open-file
                (g temp-cih-file 
                   :direction :output
                   :if-exists :supersede
                   :if-does-not-exist :create)
              (format 
               g ";; Global completion file (re)created at ~A for user ~A~%" 
               (make-timestamp-string :mode :mmddyyhhmm) username)
              ;; (force-output g)
              (loop for entry in 
                    (create-unduplicated-cih-entries 
                     files-to-merge +cih-on-disk-limit+) do
                    (write-next-cih-entry g entry)
                    (incf entry-count)
                    )
              (force-output g))
            ;; In some Lisps rename-file fails if the target file exists
            #-(or :allegro :lispworks)
            (delete-file global-cih-file)
            (rename-file temp-cih-file global-cih-file)
            (loop for session-file in sorted-cih-session-files do
                  (delete-file session-file)) 
            (return-from exit (values entry-count nil))
            ))
      (error (c) (return-from exit (values nil c)))
      )))

;;; Read entries from all the SESSION-FILE until LIMIT number of
;;; non-duplicated entries is reached. Eliminate all the duplicate
;;; entries, maintaining the timestamp order of the entries.
;;; Return a list of at most LIMIT entries.

(defun create-unduplicated-cih-entries (session-files limit)
  (let ((ht (make-hash-table :test 'equal :size limit)))
    (loop for file in session-files 
          until (>= (hash-table-count ht) limit)
          do
          (with-open-file (s file :direction :input)
            (loop for entry = (read-next-cih-entry s)
                  as item = (first entry)
                  as timestamp = (second entry)
                  as ht-size = (hash-table-count ht)
                  until (or (null entry) (>= ht-size limit))
                  do
                  (let ((existing-timestamp (gethash item ht)))
                    (when (or (null existing-timestamp)
                              (> timestamp existing-timestamp))
                      (setf (gethash item ht) timestamp)
                      )))))
    (sort (hash-table-contents ht) '< :key 'second)
    ))


(defun initialize-cih-for-session (username sessionid)
  (let ((session-cih
         (make-cih
          :in-memory-hash (make-hash-table :test 'equal)
          :session-file (cih-session-file-path username sessionid)
          :global-file (cih-global-file-path username)
          :current-iss nil
          :match-count 0
          :enabled? t
          :reorder? nil
          )))
    (setf (get sessionid :cih) session-cih)
    (with-open-file 
        (p (cih-session-file session-cih) 
           :direction :output :if-exists :supersede)
      (format p ";; Session file created at ~A for user ~A~%" 
              (make-timestamp-string :mode :mmddyyhhmm) username)
      )))

(defun cih-state (sessionid) (get sessionid :cih))

(defun cih-current-match-string (sessionid)
  (cih-current-iss (cih-state sessionid)))

(defun set-cih-current-match-string (sessionid s)
  (let ((cih (cih-state sessionid)))
    (setf (cih-current-iss cih) s)
    ))

(defun cih-current-match-count (sessionid)
  (cih-match-count (cih-state sessionid)))

(defun set-cih-current-match-count (sessionid count)
  (let* ((cih (cih-state sessionid)))
    (setf (cih-match-count cih) count)
    ))
    

(defun maybe-save-next-input-history-item 
       (item &optional (sessionid *sessionid*))
  (let ((cih (cih-state sessionid)))
    (when (and cih 
               (cih-enabled? cih) 
               (>= (length item) +min-completion-user-input-length-to-save+))
      (handler-case
          ;; Determine if item is already in completion hash
          ;; and if so update its timestamp, otherwise create an entry
          ;; for the entry with a timestamp.
          (let* ((cih-ht (cih-in-memory-hash cih))
                 (existing? (gethash item cih-ht))
                 (timestamp (seconds-since-code-was-loaded)))
            (setf (gethash item cih-ht) timestamp)
            (if existing? 
                ;; Note that since the timestamp on at least one entry
                ;; has changed, a reordering of the session cache file
                ;; should eventually be done.
                (setf (cih-reorder? cih) t)
              ;; Since this is a new entry, add it to the session cache
              ;; file.  If the cache file needs reordering, and the
              ;; number of entries in the cache has grown by +cih-reorder-every+
              ;; then write out the session cache reordered by up-to-date
              ;; timestamps and turn off reorder flag.
              (let ((size (hash-table-count cih-ht)))
                (with-open-file 
                    (p (cih-session-file cih) 
                       :direction :output
                       :if-exists :append
                       :if-does-not-exist :create)
                  (write-next-cih-entry p (list item timestamp)))
                (when (and (cih-reorder? cih) 
                           (zerop (mod size +cih-reorder-every+)))
                  (write-ordered-cih-session-file cih)
                  (setf (cih-reorder? cih) nil)
                  ))))
        (error (c) (disable-cih-for-session sessionid c))
        ))))

(defun write-ordered-cih-session-file (cih)          
  (let ((entries 
         (sort (hash-table-contents (cih-in-memory-hash cih)) 
               '> :key 'second)))
    (with-open-file
        (p (cih-session-file cih)
           :direction :output 
           :if-exists :supersede
           :if-does-not-exist :error)
      (loop for entry in entries do (write-next-cih-entry p entry))
      )))

(defun nth-cih-match (sessionid initial-substring n wrap? &aux cih)
  (setq cih (cih-state sessionid))
  (when (and cih (cih-enabled? cih))
    (handler-case
        (let ((in-core-matches nil))
          ;; find all the matches to entries stored in the hash table
          (maphash
           (lambda (key timestamp)
             (when (initial-subsequence-of? key initial-substring)
               (push (list key timestamp) in-core-matches)
               ))
           (cih-in-memory-hash cih))
          ;; sort them in order of most recently entered first
          (setq in-core-matches (sort in-core-matches '> :key 'second))
          (let ((n-in-core-matches (length in-core-matches)))
            (if (<= n n-in-core-matches)
                ;; If the Nth match exists in our list of in-core matches
                ;; we're done, return it.
                (first (elt in-core-matches (1- n)))
              ;; Otherwise, search through the global file for the correctly
              ;; numbered match
              (multiple-value-bind (matched-string global-matches)
                  (nth-global-cih-match 
                   cih initial-substring (- n n-in-core-matches))
                (let ((total-matches 
                       (+ (length in-core-matches) (length global-matches))))
                  (cond
                   ;; We found it.  We're done, return it.
                   (matched-string matched-string)
                   ;; We didn't find it, but some matches exist, and
                   ;; we want to wrap, which means we really want the Nth
                   ;; match mod the total number of existing matches
                   ((and wrap? (plusp total-matches))
                    (let ((windex (mod (1- n) total-matches)))
                      (first (elt (nconc in-core-matches global-matches) windex))
                      ))
                   ;; No match, and either no matches exist, or no wrapping.
                   (t nil)
                   ))))))
      ;; Something bad happened. Log it, and turn off completion.
      (error (c) (disable-cih-for-session sessionid c))
      )))

(defun disable-cih-for-session (sessionid error-condition)
  (let ((cih (cih-state sessionid)))
    (setf (cih-enabled? cih) nil)
    (log-user-event 
     (formatn "Completion disabled.  Actual error: ~A" error-condition))
    (log-system-event
     (formatn 
      "Completion for user ~S, session ~S, disabled. Error: ~A"
      *username* sessionid error-condition
      ))))

(defun nth-global-cih-match (cih match n)
  (unless (and (integerp n) (plusp n))
    (error "Internal error.  Global matcher called with bad N: ~A" n))
  (block exit
    (let ((all-matches nil))
      (with-open-file (p (cih-global-file cih) :direction :input)
        (loop with match-count = 0
              as entry = (read p nil nil)
              as item = (first entry)
              until (null entry)
              do
              (when (initial-subsequence-of? item match)
                (incf match-count)
                (when (= match-count n) (return-from exit (values item nil)))
                (push entry all-matches)
                )))
      (values nil (reverse all-matches))
      )))
    

(defun new-do-input-history-completion (form-string)
  (let* ((sessionid (user-session-id))
         (nth-match-found (cih-current-match-count sessionid))
         (previous-match (cih-current-match-string sessionid)))
    ;; If the match parameters have been reset at a higher level,
    ;; or if we aren't matching exactly the same string as we did
    ;; the immediately previous completion attempt, reset things
    ;; to look for the first match.
    (when (or (null nth-match-found)
              (null previous-match)
              (not (string= form-string previous-match)))
      (setq nth-match-found 0)
      (set-cih-current-match-count sessionid 0))
    ;; Search over all previous input strings looking for the next match
    (let ((matching-entry
           (nth-cih-match sessionid form-string (1+ nth-match-found) t)))
      (if matching-entry
          (progn
            (set-cih-current-match-count sessionid (1+ nth-match-found))
            (set-cih-current-match-string sessionid form-string))
        (reset-input-completion-state))
      matching-entry
      )))

(defun new-reset-input-completion-state (&optional (sessionid *sessionid*))
  (set-cih-current-match-count sessionid 0)
  (set-cih-current-match-string sessionid nil))
