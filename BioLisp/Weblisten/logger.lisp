;;; -*- mode: Lisp; Syntax: Common-Lisp; Package: weblistener; -*-

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

;;; Author: JP Massar

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *user-log-message-size-limit* 20000)
(defvar *user-log-message-truncated-size* 200)

(defun username-from-userdir (dir) (lastelem (pathname-directory dir)))

(defun sessionlogs-dir-from-userdir (userdir) 
  (append-subdir userdir "session-logs"))

(defun user-logs-directory (user)
  "Returns pathname for directory containing user session logs"
  (append-subdir
   (append-subdir
    (cl-user:translate-simple-lp "logs:")
    (string-downcase user))
   "session-logs"
   )) 

(defun system-logs-directory () (user-logs-directory "system"))

(defun make-timestamp-logfile (user &key (suffix nil) (type "log"))
  #.(one-string-nl
     "Returns a string representing the full pathname of a potential log file"
     "whose name is based on the current time.")
  (one-string
   (namestring (user-logs-directory user))
   (cl-user::weblistener-logfile-name suffix type)
   ))

(defun truncated-log-message (s) 
  (s+ (formatn "TRUNCATED (> ~D chars): " *user-log-message-size-limit*)
      (limited-string s *user-log-message-truncated-size*)
      ))
  
(defun log-message-needs-newline? (msg)
  (null (char= #\Newline (elt msg (1- (length msg))))))

(defun log-user-event (format-string &rest format-args)
  "Write something to the current user's log file, prepended with a timestamp"
  (let ((limit *user-log-message-size-limit*))
    (when (and cl-user:*logs-directory* *log-file*)
      (handler-case
          (with-open-file
              (p *log-file*
                 :direction :output
                 :if-exists :append :if-does-not-exist :create)
            (let ((s (apply 'format nil format-string format-args)))
              (when (> (length s) limit)
                (setq s (truncated-log-message s)))
              (format p "~A : " (make-timestamp-string))
              (princ s p)
              (when (log-message-needs-newline? s) (terpri p))
              (force-output p)
              ))
        (error
         ()
         (log-system-event "Could not write to user log file ~A~%" *log-file*)
         )))))
                        
(defun log-system-event (format-string &rest format-args)
  "Write something to the system log, prepended with a timestamp"
  (let ((msg (apply 'format nil format-string format-args)))
    (when cl-user:*logs-directory*
      (handler-case
          (with-open-file
              (p *system-logfile*
                 :direction :output
                 :if-exists :append :if-does-not-exist :create)
            (format p "~A : " (make-timestamp-string))
            (format p "~A" msg)
            (when (log-message-needs-newline? msg) (terpri p))
            (force-output p)
            )
        (error 
         (c)
         (cformatt "*****> UNABLE TO WRITE TO SYSTEM LOG <*****")
         (cformatt "Error condition: ~A" c)
         (cformatt "Logfile file path: ~A" (namestring *system-logfile*))
         )))))

(defun log-vpl-help-request (help-string)
  (with-open-file 
      (p *system-vpl-help-request-log-file* 
         :direction :output :if-exists :append :if-does-not-exist :create)
    (format 
     p "~A ~A : ~A~%" (make-timestamp-string) wb::*username* help-string)
    ))
            


;;; Called when weblistener starts up and when a user logs in.

(defun purge-log-directory
       (user
        &key
        (days-past (if (string-equal (string user) "system")
                       cl-user:*days-until-system-logs-purged*
                     cl-user:*days-until-user-logs-purged*
                     ))
        (files-not-to-delete nil)
        )
  #.(one-string-nl
     "Removes all files in USER's log directory which were last written"
     "more than DAYS-PAST days ago.  Returns number of files deleted if"
     "successful and NIL if it encounters a problem.")
  (when cl-user:*logs-directory*
    (let* ((user-log-directory (user-logs-directory user))
           (delete-count 0)
           (do-not-delete (mapcar 'pathname files-not-to-delete)))
      (handler-case
          (let* ((log-files
                  (directory-with-subdirs-in-directory-form
                   user-log-directory))
                 (time-now (get-universal-time))
                 (delete-before-time (- time-now (* 60 60 24
                                                    days-past))))
            (dolist (file log-files delete-count)
              (when (not (pathname-names-directory? file))
                (unless (member 
                         file do-not-delete :test 
                         (lambda (x y) 
                           (and 
                            (equalp (pathname-name x) (pathname-name y))
                            (equalp (pathname-type x) (pathname-type y))
                            )))
                  (vif (time-file-created (file-write-date file))
                       (when (< time-file-created delete-before-time)
                         (delete-file file)
                         (incf delete-count))
                       (log-system-event 
                        "File ~A has NIL file-write-date??" file)
                       )))))
        (error
         (c)
         (cond
          ((string-equal (string user) "SYSTEM")
           (cformatt " *****> ERROR WHILE PURGING SYSTEM LOGS <*****")
           (cformatt " Error condition: ~A" c)
           (cformatt " System log directory: ~A" user-log-directory)
           (cformatt " Notify Sysadmin if possible."))
          (t
           (dolist (format-args
                    (list
                     (list " ***> system error <*** Error purging log files.")
                     (list " Error condition: ~A" c)
                     (list " Log directory: ~A" user-log-directory)
                     (list " Notify Sysadmin if possible.")
                     ))
             (apply 'log-system-event format-args)
             (apply 'cformatt format-args)
             )))
         nil
         )))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun default-log-filepath-sort-key (path) (or (file-write-date path) 1))


(defun build-html-index-file-for-directory
       (directory browser-prefix
                  &key
                  (output-file-name "index.html")
                  (input-pathname-predicate 'identity)
                  (title (formatn "Index for ~A" (namestring
                                                  directory)))
                  (sort-predicate '>)
                  (sort-key 'default-log-filepath-sort-key)
                  (description-function
                   (lambda (file) (declare (ignore file)) ""))
                  )
  #.(one-string-nl
     "Creates a file with HTML in it which lists all the files in DIRECTORY"
     "which are not subdirectories and whose pathnames satisfy"
     "INPUT-PATHNAME-PREDICATE."
     "The listed files are presented one per line, and are sorted using"
     "SORT-PREDICATE and SORT-KEY.  The (FILE-NAMESTRING <file>) of each"
     "file is printed, followed by a description string created by calling"
     "DESCRIPTION-FUNCTION on the file.  The listing is done using HTML"
     "<table> syntax."
     "The created file is written to DIRECTORY unless OUTPUT-FILE-NAME"
     "specifies a full pathname.  An attempt is made to write out the HTML"
     "that is generated in a human-readable format."
     "Each listed file is hyperlinked using a URL which is created by"
     "appending BROWSER-PREFIX to the file's name."
     "If TITLE is non-nil it must be a string and it is displayed in large"
     "font at the top of the HTML display.")
  (let* ((files-to-index
          ;; Create list of files satisfying INPUT-PATHNAME-PREDICATE
          ;; which are not subdirectories
          (remove-if-not
           (lambda (file)
             (and (not (pathname-names-directory? file))
                  (funcall input-pathname-predicate file)))
           (directory-with-subdirs-in-directory-form directory)
           ))
         ;; Sort them according to SORT-PREDICATE and SORT-KEY
         (sorted-files (sort files-to-index sort-predicate :key
                             sort-key))
         (output-path (merge-pathnames output-file-name directory)))
    (with-open-file
        (p output-path :direction :output :if-exists :supersede)
      (flet ((nli (n)
               (html
                (:princ-safe nl) (dotimes (j n) (html (:princ-safe " "))))))
        (html-stream
         p
         (:html
          (:body
           (:princ-safe nl)
           (when title
             (html
              (:b :br (:big (:princ-safe title))) (:princ-safe nl) :br
              :br))
           (:table
            (loop for file in sorted-files
                  as filename = (file-namestring file) do
                  (html
                   (nli 2)
                   (:tr
                    (nli 4)
                    (:td ((:a :href (one-string browser-prefix
                                                filename))
                          (:tt (:princ-safe (file-namestring file)))))
                    (nli 4)
                    (:td
                     (:tt (:princ-safe (funcall description-function
                                                file))))
                    ))))
           (:princ-safe nl)
           ))))
      output-path
      )))

(defun create-log-files-index-for-user (user)
  "Create an index file, index.html, in USER's session log directory"
  (when cl-user:*logs-directory*
    (build-html-index-file-for-directory
     (user-logs-directory user)
     (one-string
      cl-user:*host-machine-apache-logs-url*
      (string-downcase user)
      "/session-logs/")
     :input-pathname-predicate
     (lambda (p)
       (or (string-equal "log" (pathname-type p))
           (string-equal "bwlscript" (pathname-type p))))
     :title (formatn "EXISTING SESSION FILES FOR ~A" user)
     :description-function
     (lambda (p)
       (let ((fwd (file-write-date p)))
         (if fwd
             (one-string
              "(" (make-timestamp-string
                   :universal-time fwd :mode :mmddyyhhmm) ")")
           "(<creation date unknown>)"
           ))))))
           

(defun log-files-in-directory (directory)
  "Returns a list of all files ending in .log or .bwlscript in DIRECTORY"
  (remove-if
   (lambda (p)
     (or (pathname-names-directory? p)
         (and
          (not (string-equal "log" (pathname-type p)))
          (not (string-equal "bwlscript" (pathname-type p)))
          )))
   (directory-with-subdirs-in-directory-form directory)
   ))


;;; URL's that cause a listing of the log files in the user's 
;;; log directory to be displayed in chronological order 
;;; to the browser

(defvar *guru-logging* t)

(publish
 :path *previous-sessions-url*
 :content-type cl-user::*html-publish-content-type*
 :function
 (lambda (req ent)
   (let* ((input (request-query req))
          (package-name (url-parameter-value :pkg input))
          (package-symbol (keywordize package-name))
          )
     (execute-with-standard-weblistener-environment
      req ent package-symbol
      (lambda () (html-for-previous-sessions-url))
      ))))

(defun html-for-previous-sessions-url ()
  (let ((directory (user-logs-directory wb:*username*)))
    (if (and *guru-logging* (weblistener-guru-p))
        (guru-log-files-index-listing)
      (log-files-index-listing "Your Existing Session Files" directory)
      )))

(publish 
 :path *previous-system-logs-url*
 :content-type cl-user::*html-publish-content-type*
 :function
 (lambda (req ent)
   (let* ((input (request-query req))
          (package-name (url-parameter-value :pkg input))
          (package-symbol (keywordize package-name))
          )
     (execute-with-standard-weblistener-environment
      req ent package-symbol
      (lambda () (html-for-previous-system-logs-url))
      ))))

(defun html-for-previous-system-logs-url ()
  (let ((directory (user-logs-directory "system")))
    (if (weblistener-guru-p)
         (log-files-index-listing 
          "EXISTING SYSTEM LOG AND SCRIPT FILES" directory)
      (html 
       (:big 
        (:princ-safe "You do not have access permission to the system logs")
        )))))

(defun log-files-index-listing (title directory)
  (with-standard-weblistener-page-header (title)
    (let ((sorted-files-to-index 
           (sort 
            (log-files-in-directory directory)
            '>
            :key 'default-log-filepath-sort-key
            )))
      (html
       (:table
        ;; Table column headers
        (:tr 
         (:th (:princ "Link"))
         (:th) (:th)
         (:th (:princ "Last entry")) 
         (:th) (:th)
         (:th (:princ "Log file name")))
        (:tr)
        ;; Each existing log file.
        (loop for file in sorted-files-to-index 
              for j from 1 
              as filename = (file-namestring file) 
              as write-date = (file-write-date file)
              do
              (html
               (:tr
                (:td 
                 ((:a :href 
                   (make-log-viewer-url 
                    :pkg (user-session-id)
                    :file (url-safe-string (namestring file))))
                  (:tt (:princ (formatn "Log ~D" j)))))
                (:td) (:td)
                (:td 
                 (:tt
                  (:princ-safe
                   (if write-date
                       (utils::past-time-description 
                        write-date :mode :abbrev)
                     "<creation date unknown>"
                     ))))
                (:td) (:td)
                (:td (:tt (:princ-safe filename)))
                ))))))))


(publish 
 :path *log-viewer-url*
 :content-type cl-user::*html-publish-content-type*
 :function
 (lambda (req ent)
   (let* ((input (request-query req))
          (package-name (url-parameter-value :pkg input))
          (package-symbol (keywordize package-name))
          (file (url-parameter-value :file input))
          (mode (url-parameter-value :mode input))
          )
     (execute-with-standard-weblistener-environment
      req ent package-symbol
      (lambda () (html-for-log-viewer-url file mode))
      ))))

(defun html-for-log-viewer-url (file mode)
  (let ((vpl-session? (get wb::*sessionid* :vpl-session?))
        (guru? (wb::weblistener-guru-p)))
    (cond
     ((or (and mode (string-equal mode "exec"))
          (and (null mode) (not guru?) vpl-session?))
      (forward-package-funcall 
       :vpl :create-prettified-vpl-execution-log-html :file file
       ))
     (t
      (with-standard-weblistener-page-header
          ("SESSION LOG")
        (html 
         (when (or guru? vpl-session?)
           (html 
            ((:a :href 
              (wb::make-log-viewer-url 
               :file (url-safe-string file)
               :mode "exec"
               ))
             "Show execution log only"
             )))
         :br
         (let ((filedata (file-to-string file :max 3000000)))
           (if (null filedata)
               (html 
                (:big 
                 (:b 
                  (:princ-safe 
                   (format nil "FILE TOO BIG! (~D bytes)" (file-length file)))))
                (:p
                 (:princ-safe 
                  (one-string
                   "Use the File Browser and download it if you really want "
                   "to view it."))))
             (html (:pre (:princ-safe filedata)))
             ))))))))

    
    

(defun guru-log-files-index-listing ()

  (let* ((pkg (user-session-id))
         ;; Find all the directories under the root of the the user
         ;; directories for this instance of BioLingua.  
         (userdirs
          (remove-if-not
           'pathname-names-directory?
           (directory-with-subdirs-in-directory-form cl-user:*home-directory*)
           ))
         ;; Find those directories from the above list which have a
         ;; 'sessionlogs' subdirectory.  This should effectively be the list of
         ;; users who have ever used this instance.
         (userdirs-with-sessionlogs-dir
          (remove-if-not
           (lambda (d) (probe-file (sessionlogs-dir-from-userdir d)))
           userdirs
           ))
         ;; Find all the session log files of all the users.
         (users-session-logs
          (mapcar
           (lambda (d) 
             (log-files-in-directory (sessionlogs-dir-from-userdir d)))
           userdirs-with-sessionlogs-dir
           ))
         ;; Sort each user's log files according to date, most recent first.
         (sorted-users-session-logs
          (loop for logfiles in users-session-logs collect
                (sort logfiles '> :key 'default-log-filepath-sort-key)))
         ;; Pair up each user's log files with their name.
         (users-data 
          (mapcar
           'list
           (mapcar 'username-from-userdir userdirs-with-sessionlogs-dir)
           userdirs-with-sessionlogs-dir
           sorted-users-session-logs))
         ;; Find the listings for this user (the guru)
         (current-user-data
          (find (string *username*) users-data :key 'first :test 'string-equal))
         ;; Find the listing for the system
         (system-data
          (find "system" users-data :key 'first :test 'string-equal))
         (others-data nil))

    ;; Isolate the invoking user's data and the system data,
    ;; then sort the remaining users' data by time of most recent log
    ;; for ease in tracking recent activity.

    (setq others-data (remove current-user-data users-data))
    (setq others-data (remove system-data others-data))
    (setq others-data 
          (sort others-data 
                '>
                :key
                (lambda (data)
                  (let ((log-files (third data)))
                    (if (null log-files)
                        0
                      ;; Files are already sorted by timestamp, so
                      ;; most recent one is first.  Just use that timestamp.
                      (default-log-filepath-sort-key (first log-files))
                      )))))
          
    ;; First show the invoking user's recent logs, then the recent
    ;; system logs, then the recent other users' logs.

    (progn
      (with-standard-weblistener-page-header
       ("Most recent user log files (Guru mode listing)" :listener? nil)
       (html
        (:table
         ;; Table column headers
         (:tr 
          (:th (:princ "User"))
          (:th) (:th)
          (:th (:princ "Link"))
          (:th) (:th)
          (:th (:princ "Last entry")) 
          (:th) (:th)
          (:th (:princ "Log file name")))
         (:tr)
         (show-user-and-some-of-his-session-logs current-user-data pkg 4)
         (show-user-and-some-of-his-session-logs system-data pkg 8)
         (dolist (data others-data) 
           (show-user-and-some-of-his-session-logs data pkg 3)
           )))))

    ))


;;; Show the user and the most recent N session logs.
;;; The user name will be a hyperlink to the full session logs
;;; directory listing.

(defun show-user-and-some-of-his-session-logs (data pkg n)
  (let ((username (first data))
        (user-directory (second data)) 
        (session-logs (third data)))
    (declare (ignore user-directory))
    ;; For each of the first N existing log files
    (loop for file in session-logs
          ;; In the system session-logs directory there are both .log
          ;; and .bwlscript files, so show twice as many.
          for j from 0 below (if (integerp n) n most-positive-fixnum)
          as filename = (file-namestring file) 
          as write-date = (file-write-date file)
          do
          (html
           (:tr
            (:td 
             ;; Only list user's name on first line for better visual effect.
             (if (zerop j) 
                 (html
                  ((:a :href 
                    (make-guru-previous-sessions-url :pkg pkg :user username))
                   (:princ username)))
               (html (:princ ""))))
            (:td) (:td)
            (:td 
             ((:a :href 
               (make-log-viewer-url 
                :pkg pkg 
                :file (url-safe-string (namestring file))))
              (:tt (:princ (formatn "Log ~D" j)))))
            (:td) (:td)
            (:td 
             (:tt
              (:princ-safe
               (if write-date
                   (utils::past-time-description 
                    write-date :mode :abbrev)
                 "<creation date unknown>"
                 ))))
            (:td) (:td)
            (:td (:tt (:princ-safe filename)))
            )))))

(publish 
 :path *guru-previous-sessions-url*
 :content-type cl-user::*html-publish-content-type*
 :function
 (lambda (req ent)
   (let* ((input (request-query req))
          (package-name (url-parameter-value :pkg input))
          (package-symbol (keywordize package-name))
          (user (url-parameter-value :user input))
          )
     (execute-with-standard-weblistener-environment
      req ent package-symbol
      (lambda ()
        (let ((user (keywordize user))
              (directory (user-logs-directory user)))
          (log-files-index-listing 
           (formatn "SESSION LOGS FOR USER ~A" user)
           directory
           )))))))


