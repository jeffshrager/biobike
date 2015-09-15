;; -*- mode: Lisp; Syntax: Common-Lisp; Package: weblistener; -*-

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

;;; Author:  JP Massar.  

(defvar *system-standard-output* *standard-output*)
(defvar *system-error-output* *error-output*)
(defvar *system-trace-output* *trace-output*)

(defvar *current-weblistener-port* nil "Set by wb:start-weblistener")
(defvar *weblistener-pid* nil "Set by wb:start-weblistener")

(defvar *user->sessionids-ht* (make-hash-table :test 'eq))

(defvar *system-startup-time* (get-universal-time))

(defvar nl (string #\Newline))

(defvar *new-repl-enabled?* t)

(defparameter *enough-dummy-lines* 50
  "A hack for the redisplay algorithm to insure bottom of page is displayed")

(defparameter *logins* nil
  "list of users who have logged in since Weblistener was started.")

(defvar *system-logfile* nil
  "A physical pathname set up by start-weblistener, where system log msgs go")

(defvar *system-alogfile* nil
  "A physical pathname set up by start-weblistener, where aserve log msgs go")

(defvar *system-login-file* nil
  "A physical pathname set up by start-weblistener, where logins are recorded")

(defvar *system-vpl-help-request-log-file* nil
  "A pathname set up by start-weblistener, where help requests are recorded.")


(defparameter *user-display-id* -1)

(defvar *oneline-form-data* "" "Current contents of one-line text input")
(defvar *multiline-form-data* "" "Current contents of multiline text input")

(defparameter *default-compilation-timeout* 10 
  "Length of time compilation of user form can take without timing out.")

(defvar *protocol-names* nil)
(defvar *login-messages* nil)
(defvar *log-file* nil "A log file specific to each user")

(defvar *output-limit-enabled* nil
  #.(one-string-nl
     "Controls when output restrictions on line length and number of lines"
     "are applied, which is only when we are"
     "printing output / evaluation results to the screen or to the log."))

;; Debugging variable.  
(defvar *current-repl* nil)

#-:allegro
(warn "*** NOT REDEFINING PRINT-OBJECT on STRINGS TO LIMIT WEBLISTENER OUTPUT")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; New output display algorithm

(define-symbol-macro *output-limit* (output-limit-obsolete))

(defun output-limit-obsolete ()
  (cformatt ">>> *output-limit* is deprecated.  Use SET-OUTPUT-LIMITS.")
  (error "*output-limit* is no longer in use."))

(defsetf output-limit-obsolete setf-output-limit-obsolete)

(defun setf-output-limit-obsolete (foo)
  (declare (ignore foo))
  (output-limit-obsolete))

(defvar *output-chars-per-line-limit* nil)
(defvar *output-lines-limit* nil)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *minimum-chars-per-line-shown* 20)
  (defvar *minimum-lines-shown* 10))

(defparameter *valid-truncation-keys*
  '(:width :width-only :chars :characters 
    :number-of-lines :height-only :height :lines
    :both :all t
    :none :neither nil
    ))

(defun set-output-limits 
       (&key 
        (lines *output-lines-limit* lp?) 
        (chars-per-line *output-chars-per-line-limit* cplp?)
        (truncation-message-mode 
         (get *sessionid* :truncation-message-mode) tmmp?)
        )
  #.(one-string-nl
     "Sets a limit on the number of lines printed out"
     "and the width of each line."
     ""
     "Example: (SET-OUTPUT-LIMITS :LINES 100 :CHARS-PER-LINE 200)"
     ""
     "This means no more than 100 lines will be displayed"
     "(an excess is indicated by ... and a message), and any characters"
     "on a line beyond 200 will be truncated (indicated by ...)."
     "(Note that the actual results of a computation are not in any way"
     "affected, only the display is truncated.)"
     ""
     "Example: (SET-OUTPUT-LIMITS :CHARS-PER-LINE 120)"
     ""
     "This leaves the line truncation limit alone, and changes the"
     "line width truncation limit to be 120 characters."
     ""
     "The initial limit for the number of lines to be shown is 500."
     "The initial limit for the number of characters shown per line is 200."
     "Use (SHOW-OUTPUT-LIMITS) to see the current limits."
     "If either parameter is not provided, the current value is retained."
     "A value of NIL indicates no limit (not recommended)."
     ""
     "The number of lines limit applies individually to each component"
     "of your output -- all the output messages that appear as the result"
     "of PRINT (and similar) statements in your code are one component,"
     "and each value returned by executing your code is another component."
     ""
     "Another argument, TRUNCATION-MESSAGE-MODE, is used to designate"
     "whether a message is to be printed when either width or"
     "number of lines truncation takes place."
     "A value of :all (the initial value and default) will cause"
     "both width and number of lines truncation messages to be printed,"
     "as appropriate."
     "A value of :width causes only a width truncation message to be printed,"
     "should any line be truncated. A message will be printed immediately"
     "below only the first line to be truncated, not subsequent ones."
     "A value of :number-of-lines causes only a number of lines truncation"
     "message to be printed should the number of lines shown be truncated."
     "The message is printed at the point of truncation."
     "A value of :none causes no truncation message to appear. (If number"
     "of lines truncation takes place, three vertical '.'s are printed at"
     "the point of truncation.)")
  (flet ((oops (var value min)
           (unless (or (null value) (and (integerp value) (plusp value))) 
             (error 
              "SET-OUTPUT-LIMITS: ~A must be nil or a positive integer." var))
           (unless (>= value min) 
             (error "SET-OUTPUT-LIMITS: ~A must be at least ~D." var min))))
    ;; Check all the arguments are valid before changing anything.
    (when lp? (oops 'lines lines 10))
    (when cplp? (oops 'chars-per-line chars-per-line 20))
    (when tmmp?
      (unless (member truncation-message-mode *valid-truncation-keys*)
        (error "SET-OUTPUT-LIMITS: TRUNCATION-MESSAGE-MODE must be one of ~S"
               *valid-truncation-keys*)))
    ;; Set the new values.
    (when lp? (setq *output-lines-limit* lines))
    (when cplp? (setq *output-chars-per-line-limit* chars-per-line))
    (when tmmp? 
      (setf 
       (get *sessionid* :truncation-message-mode) 
       (case truncation-message-mode
         ((:width-only :width :chars :characters) :width)
         ((:height-only :height :lines :number-of-lines) :number-of-lines)
         ((:both :all t) :all)
         ((:none :neither nil) :none)
         )))
    (values lines chars-per-line (get *sessionid* :truncation-message-mode))
    ))

(defun show-output-limits ()
  #.(one-string-nl
     "Shows the current limits on number of lines output and"
     "number of characters per line before truncation.")
  (terpri)
  (cformatt "Maximum number of lines (vertical limit): ~A"
            (or *output-lines-limit* "Unlimited"))
  (cformatt "Maximum number of characters per line (horizontal limit): ~A" 
            (or *output-chars-per-line-limit* "Unlimited"))
  (cformatt "Truncation message mode: ~S" 
            (get *sessionid* :truncation-message-mode)))

(defun max-line-length-in-string-with-newlines (s)
  ;; Find the length of the longest line in S, where a line is terminated
  ;; by a Newline or the end of the string.
  (unless (simple-string-p s) (error "Oops, not a simple string!"))
  (locally
    (declare (simple-string s))
    (declare (optimize (speed 3) (safety 0)))
    (let* ((maxll 0) 
           (last-pos -1)
           (len (length s))
           (last-index (the fixnum (1- len))))
      (declare (fixnum maxll last-pos len last-index))
      (loop for j fixnum from 0 below len
            do
            (when (char= (schar s j) #\Newline) 
              (setq 
               maxll 
               (max maxll (the fixnum (1- (the fixnum (- j last-pos))))))
              (setq last-pos j))
            finally
            (when (char/= (schar s last-index) #\Newline)
              (setq
               maxll 
               (max maxll (the fixnum (- last-index last-pos)))
               )))
      maxll
      )))
          

#+:allegro
(excl:without-package-locks
  (defmethod print-object :around ((string string) stream)
    (let ((cpl *output-chars-per-line-limit*)
          (llimit *output-lines-limit*))
      (when (and *output-limit-enabled* 
                 (or cpl llimit) (plusp (length string))) 
        (unless (simple-string-p string) 
          (setq string (coerce string 'simple-string)))
          ;; (error "Internal error: expected only simple strings!"))
        (locally
          (declare (simple-string string))
          (let ((line-count (count #\Newline string))
                (last-char (lastelem string)))
            (unless (char= last-char #\Newline) (incf line-count))
            (setq
             string
             (cond 
              ((and llimit (> line-count llimit))
               (do-line-limit-and-cpl-limit line-count string llimit cpl))
              ((and cpl
                    (> (max-line-length-in-string-with-newlines string) cpl))
               (do-cpl-limit 
                string cpl 
                (member 
                 (get *sessionid* :truncation-message-mode)
                 '(:width :all)
                 )))
              (t string)
              ))))))
    (call-next-method string stream)
    ))

(defun do-cpl-limit (string cpl add-truncation-message?)
  ;; Pull the string apart into its component lines and truncate
  ;; each line if necessary. Add some lines with a truncation message
  ;; after the first truncation if a message was requested.
  ;; Then put all the lines back together again into a single string.
  (let* ((lines (simple-string-split string #\Newline))
         (how-many-truncated 0)
         (truncated-lines
          (loop for line in lines 
                nconc
                (if (<= (length line) cpl) 
                    (list line)
                  (progn
                    (incf how-many-truncated)
                    (list (limited-string line (- cpl 3)))
                    )))))
    (when (and add-truncation-message? (plusp how-many-truncated))
      (push
       (formatn
        (one-string
         ";; >>> ~D line~P ~A truncated to ~D chars. "
         "(Use PREFERENCES to adjust width.)")
        how-many-truncated how-many-truncated
        (if (> how-many-truncated 1) "were" "was") cpl
        )
       truncated-lines
       ))
    (string-join truncated-lines #\Newline)
    ))

(defconstant +lines-to-show-after+ 5)

(defun do-line-limit-and-cpl-limit
       (line-count string llimit cpl 
                   &aux 
                   first-block last-block line-width-truncated?
                   stop-line-last-char-index last-five-lines-start-char-index
                   )
  (declare (simple-string string))
  (declare (fixnum line-count llimit cpl))
  (let* ((after-trunc
          (or (get *username* :lines-to-show-after-truncation) 
              +lines-to-show-after+))
         (stop-line-index (- llimit after-trunc))
         (last-five-lines-start-index (- line-count after-trunc))
         (line-number 0))
    (declare (fixnum stop-line-index last-five-lines-start-index line-number))
    ;; Find the character index for where the STOP-LINE-INDEX'th line ends
    ;; Find the character index for the the LAST-FIVE-LINES-START-INDEX'th
    ;; line begins.
    (loop for j fixnum from 0 below (length string)
          as ch = (schar string j)
          do 
          (when (char= ch #\Newline) 
            (incf line-number)
            (when (= line-number stop-line-index) 
              (setq stop-line-last-char-index (1+ j)))
            (when (= line-number last-five-lines-start-index) 
              (setq last-five-lines-start-char-index (1+ j))
              )))
    ;; Create those truncated strings, then apply the CHARS-PER-LINE
    ;; algorithm to each block as necessary.
    (setq first-block (subseq string 0 stop-line-last-char-index))
    (setq last-block (subseq string last-five-lines-start-char-index))
    (when (> (max-line-length-in-string-with-newlines first-block) cpl)
      (setq line-width-truncated? t)
      (setq first-block (do-cpl-limit first-block cpl nil)))
    (when (> (max-line-length-in-string-with-newlines last-block) cpl)
      (setq line-width-truncated? t)
      (setq last-block (do-cpl-limit last-block cpl nil)))
    ;; Create the final string with an appropriate truncation message
    ;; near the end.
    (one-string 
     first-block
     (create-output-limit-truncation-message 
      line-count llimit cpl line-width-truncated?)
     last-block)
    ))


(defun create-output-limit-truncation-message 
       (line-count llimit cpl line-width-truncated?)
  (let ((message-mode (get *sessionid* :truncation-message-mode)))
    (case message-mode
      ((:none :width) (one-string-nl "" "." "." "." ""))
      (otherwise
       (one-string 
        (formatn 
         (one-string-nl 
          ""
          "..."
          ";; >>> Output of ~D lines limited to ~D lines"  
          ";; >>> Use SET-OUTPUT-LIMITS to adjust number of lines printed."
          ";; >>> (The last few lines of output are shown below.)"
          "..."
          "")
         line-count llimit) 
        (if (and line-width-truncated? (eq message-mode :all))
            (formatn 
             (one-string-nl
              ";; >>> Some lines were also truncated to ~D characters."
              ";; >>> Use SET-OUTPUT-LIMITS to adjust line width."
              "")
             cpl
             )
          ""
          ))))))


(defvar *execution-timelimit* nil
  "How many seconds a single command is allowed to take before it aborts")

(defun set-timelimit (seconds)
  #.(one-string-nl
     "Changes the timeout threshold for a single computation executed by"
     "the Weblistener.  The argument is given in seconds.  A value greater"
     "than *maximum-execution-timelimit* is not allowed.")
  (unless (and (integerp seconds) (plusp seconds)
               (or (weblistener-guru-p)
                   (<= seconds *maximum-execution-timelimit*)))
    (error 
     (formatn
      (one-string-nl
       "Cannot set timelimit to ~A.  Value must be a non-negative integer"
       "which is less than ~D seconds.  If you need to perform a"
       "computation that takes longer than this please use RUNJOB, which"
       "allows computations up to 2 hours, or contact the system support"
       "staff to set you up so that you can run very long computations."
       ) seconds *maximum-execution-timelimit*)))
  (setq *execution-timelimit* seconds)
  )
    

(defvar *compilation-mode* t
  "Whether and how to compile input forms. :total :most (= T) :minimal or nil")

(defvar *vpl-session-info* nil
  "Contains all session-specific server side VPL information.")


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun allegrocache-loaded? ()
    (and (boundp 'cl-user::*acache-loaded*)
                 (symbol-value 'cl-user::*acache-loaded*))))

;; Per session state variables we need to save away and restore.

(defparameter *weblistener-state-variables*
  `(
    *in-history* 
    *out-history* 
    *oneline-form-data* 
    *multiline-form-data*
    *user-display-id* 
    *username*
    *sessionid*
    *protocol-names*
    *login-messages*
    *log-file*
    *execution-timelimit*
    *compilation-mode*
    *output-chars-per-line-limit*
    *output-lines-limit*
    *safety* 
    *suppress-warnings*
    *vpl-session-info*
    
#||    
    ;; when we're running acache for real and we're running it
    ;; in multi user mode then we need db.ac::*allegrocache* 
    ;; to be a per-user variable, holding the database connection
    ;; to the allegrocache server for that user
    #-:sframes
    ,@(when (allegrocache-loaded?)
        (ecase (symbol-value 'cl-user::*acache-connection-mode*) 
          (:single nil)
          (:multi
           (list 
            (symbol-of-package 
             "*ALLEGROCACHE*" :db.ac :if-does-not-exist :error
             )))))
||#

    ))

;; All Common Lisp global variables plus our state variables above.
;; When we are finished evaluating a form, we store away the values
;; of all these variables in a visitor-specific place. When we begin 
;; evaluating a form, we first retrieve the values of these variables
;; and bind the symbols to these variables.
;; In this way the global system is not affected if a visitor decides to
;; change the value of a Common Lisp global, and the visitor's state
;; data is kept separate from all other visitors' state data.

(defparameter *weblistener-protected-global-vars*
    (append
     (let ((vars nil))
       (do-external-symbols (var (find-package :common-lisp) vars)
        (when (and (boundp var) (not (constantp var)))
          (push var vars))))
     *weblistener-state-variables*
     ))

;;; Descriptions of settable params for prefs

(defparameter *weblistener-prefs-variables*
  (labels ((+int (i) (and (integerp i) (plusp i)))
           (bool (b) (or (eq b t) (eq b nil)))
           (+int-or-nil (x) (or (+int x) (eq x nil)))
           (nil-or-+int-min (x min) 
             (or (eq x nil) (and (+int x) (>= x min)))))
    `((*execution-timelimit* 
       :number
       ,(lambda (x) 
          (and (integerp x) 
               (plusp x)
               (<= x cl-user::*maximum-execution-timelimit*)))
       ,cl-user::*default-execution-timelimit*)
      (*output-chars-per-line-limit* 
       :number 
       ,(lambda (x) (nil-or-+int-min x *minimum-chars-per-line-shown*))
       200)
      (*output-lines-limit* 
       :number 
       ,(lambda (x) (nil-or-+int-min x *minimum-lines-shown*)) 
       500)
      (*print-pretty* :boolean ,#'bool ,*print-pretty*)
      (*print-level* :number ,#'+int-or-nil ,*print-level*)
      (*print-length* :number ,#'+int-or-nil 100)
      (*safety* :boolean ,#'bool t)
      )))

(defun create-current-prefs-bindings ()
  (loop for (var) in *weblistener-prefs-variables* 
        collect (list var (symbol-value var))
        ))

(defun read-prefs (&key (create-saved-bindings t))
  (when (probe-file (prefs-file))
    (load (prefs-file)))
  (when create-saved-bindings 
    (setf (get *sessionid* :default-prefs) (create-current-prefs-bindings))))

(defun prefs-file ()
  (merge-pathnames "prefs.ini" (visitor-directory *username*)))

(defun save-prefs (&key (create-saved-bindings t))
  (with-open-file (o (prefs-file) :direction :output :if-exists :supersede)
    (loop for (var) in *weblistener-prefs-variables*
          do (format o "(setq ~a ~s)~%" var (symbol-value var))))
  (when create-saved-bindings 
    (setf (get *sessionid* :default-prefs) (create-current-prefs-bindings))))

(defun find-pref-value (var)
  (fourth (find var *weblistener-prefs-variables* :key #'first)))

