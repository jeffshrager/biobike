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

;;; Author:  JP Massar. 

(defun clear-weblistener-state ()
  (setq *in-history* nil)
  (setq *out-history* nil)
  (setq *oneline-form-data* "")
  (setq *multiline-form-data* "")
  )

;;; Any time the user doesn't invoke completion the intent is that the
;;; completion state should be reset


(defun reset-input-completion-state ()
  (forward-funcall 'new-reset-input-completion-state)
  )


;;; This URL handles anything that has an EVALSTRING argument which we
;;; want handled by the WebListener READ-EVAL-PRINT mechanism that was
;;; submitted from a form via the POST method.
;;; The URL has 
;;;  -- the standard PKG argument
;;;  -- a FORMID argument which identifies which form (if any) this URL
;;;     is a response for
;;;  -- the EVALSTRING argument, which is a string which gets REP'ed

(publish
 :path *weblistener-evalstring-form-response-url*
 :content-type cl-user::*html-publish-content-type*
 :timeout 3600
 :function
 (lambda (req ent)
   (let* ((body (get-request-body req))
          (url-assoc-list (form-urlencoded-to-query body))
          (package-name (url-parameter-value :pkg url-assoc-list))
          (text-input (url-parameter-value :evalstring url-assoc-list nil))
          (formid (url-parameter-value :formid url-assoc-list))
          (reformat? (url-parameter-value :mlreformat url-assoc-list))
          )
     (if reformat?
         (reformat-multiline-text 
          req ent package-name (keywordize package-name) text-input)
       (weblistener-standard-form-response 
        req ent url-assoc-list package-name text-input (keywordize formid))
       ))))
   
;;; This URL handles anything with an EVALSTRING argument to be REP'ed
;;; that was submitted as a normal URL.

(publish 
 :path *weblistener-evalstring-url*
 :content-type cl-user::*html-publish-content-type*
 :timeout 3600
 :function
 (lambda (req ent)
   ;; Get the data currently in the text input fields
   (let* ((url-assoc-list (request-query req))
          (text-input (url-parameter-value :evalstring url-assoc-list nil))
          (package-name (url-parameter-value :pkg url-assoc-list)))
     (weblistener-standard-form-response 
      req ent url-assoc-list package-name text-input nil)
     )))

(publish 
 :path *weblistener-eval-tutorial-string-url*
 :content-type cl-user::*html-publish-content-type*
 :timeout 3600
 :function
 (lambda (req ent)
   ;; Get the data currently in the text input fields
   (let* ((url-assoc-list (request-query req))
          (text-input (url-parameter-value :evalstring url-assoc-list nil))
          (package-name (url-parameter-value :pkg url-assoc-list)))
     (setq text-input 
           (forward-package-funcall 
            :help :replace-something-with-newlines text-input "@@"))
     (weblistener-standard-form-response 
      req ent url-assoc-list package-name text-input :wb-multiline)
     )))

;;; This eventually causes the REDISPLAY.HTML code to be triggered,
;;; via the INDIRECT-TO-REDISPLAY indirection mechanism.

(defun weblistener-standard-form-response 
       (req ent input package-name text-input which)
  (declare (ignore input))

  (let* ((package-symbol (keywordize package-name))
         (*modified-oneline-data* nil)
         (*modified-multiline-data* nil))
    (declare (special *modified-oneline-data* *modified-multiline-data*))
    
    (with-http-response-and-body (req ent)

      (with-internal-weblistener-errors-caught (req)

        (weblistener-high-level-repl 
         package-name package-symbol text-input which
         )))))

(defun weblistener-high-level-repl 
       (package-name package-symbol text-input which)
  
  (cond 

   ((eq (get package-symbol :username) :docuser) 
    (html 
     ((:font :color "red") 
      (:big
       "You cannot evaluate forms via documentation-only access!!"
       :br 
       "Please notify the system support staff that you got this message."
       ))))
        
   ((null (get package-symbol :username))
            
    ;; No package corresponding to the user means either 
    ;; a) system has been rebooted
    ;; b) someone is playing a nasty joke/hack 

    (html
     (:big 
      "Apparently the WebListener was restarted, or you logged out."
      :br
      "You need to "
      ((:a :href (login-webpage-url cl-user:*ai*)) "log back in")
      " again."
      )))

   (t 

    ;; Set up the user's environment and call the LOW-LEVEL-REPL code
          
    (let ((repl (make-default-repl)))

      (with-protected-globals-bound
          package-symbol

        ;; make sure weblistener is in language specified for session
        (let ((session-language (get package-symbol :session-language)))
          (handler-case 
              (case session-language 
                (:lisp (when (not (biolisp-mode?)) (biolisp-mode)))
                (:bbl (when (not (bbl-mode?)) (bbl-mode :verbose? nil)))
                (otherwise nil)
                )
            (error
             (c)
             (error "Session language ~A, could not switch!  Actual error: ~A"
                    session-language c
                    ))))

        ;; (setq *p1* (get-process-priority))

        (with-process-information-bound (text-input)

          ;; (setq *p2* (get-process-priority))
                
          (unless (string= "" text-input)

            (setq *current-repl* repl)
            (setf (repl-input-string repl) text-input)
            (setf (repl-evaluation-timeout-threshold repl)
                  *execution-timelimit*)
            (setf (repl-parentheses-mode-enabled? repl) t)
            (setf (repl-completion-enabled? repl) t)
            (setf (repl-completion-limit repl) 20)
            (setf (repl-compilation-mode repl) *compilation-mode*)
            
            ;; Log what the user typed in.  
            ;; this is now getting printed out BEFORE execution proceeds
            ;; so that if some horrible error occurs like runnning out of memory
            ;; we can still see what the user typed.  
            (log-user-event 
             "~A"
             (one-string
              nl (weblistener-input-marker 
                  cl-user::*ai* (1+ (current-history-index)))
              " " text-input nl)
             )
                  
            (low-level-repl repl)
            (weblistener-handle-repl-return repl)

            ))

        ;; Record fact that user entered something.

        (setf (get package-symbol :last-execution-time)
              (get-universal-time))

        ;; Adjust what user sees in input boxes when browser displays
        ;; result of evaluation.  What the user sees should be what he
        ;; last entered in each box unless completion is suggested.

        (unless (repl-completion-suggested? repl)
          (case which
            (:wb-oneline (setq *oneline-form-data* text-input))
            (:wb-multiline (setq *multiline-form-data* text-input))
            (otherwise nil)
            ))

        ;; Initiate the redisplay mechanism.  The code in
        ;; redisplay-listener.lisp gets called to compute 
        ;; the new page the user will see, and send the HTML
        ;; for that page back to the browser.

        (html 
         (:princ 
          (indirect-to-redisplay 
           (incf *user-display-id*) package-name)
          ))
        
        )))))





(defun pretty-log-user-event (prefix string)
  (if (find #\Newline string)
      (log-user-event (one-string prefix " ~%~A~%") string)
    (log-user-event (one-string prefix " ~A~%") string)
    ))

(defun input-string-to-show (repl)
  (let ((input-string-used (repl-input-string-used repl)))
    (ecase (repl-parentheses-extension-type repl)
      ((nil :explicit-bracket) input-string-used)
      (:not-enough-parens 
       (one-string input-string-used " ; closing paren(s) added"))
      )))

(defun maybe-log-original-input-string (repl input-string-to-show)
  (let ((input-string-used (repl-input-string-used repl))
        (original-input-string (repl-input-string repl)))
    (when (or (repl-input-string-modified? repl)
              (not (eq input-string-to-show input-string-used)))
      (pretty-log-user-event "Original input:" original-input-string)
      )))

(defun weblistener-handle-repl-return (repl &aux (*output-limit-enabled* t))
  
  (let ((input-string (repl-input-string repl))
        (input-string-used (repl-input-string-used repl))
        (input-form (repl-input-form repl))
        (error-condition (repl-error-condition repl))
        (output-values (repl-output-values repl))
        (output-strings (repl-output-value-strings repl))
        (suggested-completion (repl-suggested-completion repl))
        (compilation-printout (repl-compilation-printout repl))
        (evaluation-printout (repl-evaluation-printout repl))
        )

    ;; There are three possibilities.
    ;; First, a completion was suggested, and no evaluation took place.
    ;; Second, a successful evaluation took place, possibly producing output
    ;;   as well as return values.
    ;; Third, an error occurred at some point in the process.  Depending
    ;;   on the type of error output it may or may not be possible
    ;;   for output to have been produced.

    (cond

     ;; Completion suggested.  This is simple.
     ;; We display the suggested completion in the multiline box,
     ;; and the typed-in string in the oneline box.
     ;; Completion state is NOT reset.
     ;; The history mechanism is not updated.

     ((repl-completion-suggested? repl)
      (setf *multiline-form-data* suggested-completion)
      (setf *oneline-form-data* 
            (substitute #\Space #\Newline input-string-used))
      (pretty-log-user-event "Incomplete input:" (repl-input-string repl))
      (pretty-log-user-event "Suggested completion:" suggested-completion))

     ;; Successful evaluation.
     ;; Reset the completion state.
     ;; Store the user input into the completion records for posterity.
     ;; Adjust the +, /, * and friends variables.
     ;; Store the printout and results into the history display mechanism.
     ;; Log the output.

     ((not error-condition)

      (reset-input-completion-state)
      (maybe-save-next-input-history-item input-string-used)

      (setq +++ ++) (setq ++ +) (setq + input-form)
      (setq /// //) (setq // /) (setq / output-values)
      (setq *** **) (setq ** *) (setq * (first output-values))

      ;; Purge compiler warnings about free references to undeclared variables
      ;; Only do this for the interactive REPL

      (when (member (repl-compilation-mode repl) '(:most t))
        (hack-compilation-warnings repl)
        (setf compilation-printout (repl-compilation-printout repl)))

      (let* ((input-string-to-show (input-string-to-show repl))
             (printout 
              (one-string 
               (or compilation-printout "")
               (or evaluation-printout "")
               )))

        (when (fboundp 'next-history-index) 
          (forward-funcall 'next-history-index))

        (push (history-record input-string-to-show input-form) *in-history*)
        (push (output-history-record printout output-strings output-values)
              *out-history*)

        (maybe-log-original-input-string repl input-string-to-show)
        
        ))

     ;; An error occurred.
     ;; Reset the completion state.  Store user input into completion.
     ;; What else we do depends on the type of error.

     (t

      (reset-input-completion-state)
      (unless (repl-read-error? repl)
        (maybe-save-next-input-history-item input-string-used))

      ;; Purge compiler warnings about free references to undeclared variables
      ;; Only do this for the interactive REPL

      (when (member (repl-compilation-mode repl) '(:most t))
        (hack-compilation-warnings repl)
        (setf compilation-printout (repl-compilation-printout repl)))

      (let ((error-string (formatn "~A" error-condition)))
        
        (when (fboundp 'next-history-index) 
          (forward-funcall 'next-history-index))

        (cond
       
         ;; Read error.  The error condition is used as the return value.
         ;; There is no input form.

         ((repl-read-error? repl)
          (push (history-record input-string nil) *in-history*)
          (push (output-history-record 
                 ""
                 (list error-string)
                 (list error-condition))
                *out-history*))

         ;; Compilation error.  There is an input form, but no output,
         ;; nor any evaluation printout (there might be compiler printout).
         
         ((repl-compilation-error? repl)
          (let ((input-string-to-show (input-string-to-show repl)))
            (setq +++ ++) (setq ++ +) (setq + input-form)
            (push (history-record input-string-to-show input-form) 
                  *in-history*)
            (push (output-history-record
                   (or compilation-printout "") 
                   (list error-string)
                   (list error-condition))
                  *out-history*)
            (maybe-log-original-input-string repl input-string-to-show)
            ))

         ;; Evaluation error.  There is an input form, but no output.
         ;; There may be both compilation and evaluation printout.

         ((repl-eval-error? repl)
          (let ((input-string-to-show (input-string-to-show repl)))
            (setq +++ ++) (setq ++ +) (setq + input-form)
            (push (history-record input-string-to-show input-form)
                  *in-history*)
            (push (output-history-record
                   (one-string 
                    (or compilation-printout "")
                    (or evaluation-printout ""))
                   (list error-string)
                   (list error-condition))
                  *out-history*)
            (maybe-log-original-input-string repl input-string-to-show)
            ))

         ;; Printing error.  There's an input form, possibly compilation
         ;; and evaluation printout, and output forms.  Unfortunately,
         ;; we can't get a string representation!

         ((repl-print-error? repl)
          (let ((input-string-to-show (input-string-to-show repl)))
            (setq +++ ++) (setq ++ +) (setq + input-form)
            (setq /// //) (setq // /) (setq / output-values)
            (setq *** **) (setq ** *) (setq * (first output-values))
            (push (history-record input-string-to-show input-form)
                  *in-history*)
            (push (output-history-record
                   (one-string 
                    (or compilation-printout "")
                    (or evaluation-printout ""))
                   (list error-string)
                   (list error-condition))
                  *out-history*)
            (maybe-log-original-input-string repl input-string-to-show)
            ))
         
         (t 
          (log-user-event "Internal weblistener error.")
          (error 
           (formatn
            (one-string
             "Internal weblistener error. "
             "Error condition ~A noted, but error flag not set!")
            error-string
            )))

         ))))

    ;; Log input and output to user's log file 

    (unless (repl-completion-suggested? repl)
      ;; this is now getting printed out BEFORE execution proceeds
      ;; so that if some horrible error occurs like running out of memory
      ;; we can still see what the user typed.  
      ;; (output-input-record cl-user::*ai* :log (first *in-history*))
      (output-output-record cl-user::*ai* :log (first *out-history*)))

    ;; If compilation and/or execution took a measurable amount of time
    ;; log a timestamp to this effect.  

    (log-long-duration-forms repl)

    ))

(defun log-long-duration-forms (repl)
  (flet ((ts (ut) (make-timestamp-string :universal-time ut :mode :hhmmss)))
    (let ((rt (repl-start-repl-time repl))
          (ct (repl-end-compilation-time repl))
          (et (repl-end-execution-time repl)))
      (cond 
       ((= rt ct et) nil)
       ;; the compilation failed, so no times recorded for either compilation
       ;; or execution
       ((and (zerop ct) (zerop et)) nil)
       ;; execution failed.
       ((zerop et) 
        ;; compilation itself took a long time.  show how long.
        (when (> ct rt) 
          (log-user-event "repl compilation times: ~A, ~A~%" (ts rt) (ts ct))))
       ;; execution completed, but time for compilation/execution at least 
       ;; one second.  show how long each took in seconds.
       (t 
        (log-user-event "repl times: ~A, ~A, ~A~%" (ts rt) (ts ct) (ts et))
        )))))

(defun hack-compilation-warnings (repl)
  (if *new-cw-algorithm* 
      nil
    #+allegro
    (let ((cp (repl-compilation-printout repl)))
      (when (and cp (not (defining-form? (repl-input-form repl))))
        (setf (repl-compilation-printout repl)
              (remove-warnings-about-undeclared-variables cp)
              )))
    #-allegro
    repl
    ))

(defun defining-form? (form)
  (and (listp form) 
       (symbolp (first form))
       (member (keywordize (first form))
               '(:defun :defmacro :defmethod :define-function :define-macro
                  #+oops :defvar #+oops :defparameter))))


(defparameter *allegro-undeclared-variable-warning-pattern*
  (cl-ppcre:create-scanner
   "Warning: Free reference to undeclared variable.*?assumed.*?special\.(\\n)*"
   :single-line-mode t
   ))

(defun remove-warnings-about-undeclared-variables 
       (compiler-printout &aux result last-is-newline?)

  ;; Make sure terminating newline is preserved 
  (setq last-is-newline?
        (and (plusp (length compiler-printout)) 
             (eql (lastelem compiler-printout) #\Newline)))

  ;; Do the actual compiler warning hacking 
  (setq 
   result 
   (let ((revised-message-lines
          (remove-if 
           (lambda (x) (or (string= x "")) (every 'whitespacep x))
           (string-split 
            (cl-ppcre:regex-replace-all
             *allegro-undeclared-variable-warning-pattern*
             compiler-printout
             "")
            #\Newline))))
     (unless (and (= 1 (length revised-message-lines))
                  (initial-subsequence-of? 
                   (first revised-message-lines)
                   "; While compiling"
                   ))
       (string-join revised-message-lines #\Newline)
       )))

  ;; Make sure terminating newline is preserved 
  (when (and last-is-newline? 
             (plusp (length result))
             (not (eql (lastelem result) #\Newline)))
    (setq result (one-string result (string #\Newline)))
    ))

;;; We need this indirection so that we can pass the TAG
;;; into the browser, so it will display the HTML we generate
;;; so that the text identified by the TAG is visible.
;;; Now, we position the text identified by the tag at the very bottom
;;; of the page, forcing the display mechanism to show the bottom of
;;; the page we generate, not the top.

;;; (This could be done using Lisp-HTML.  No particular reason it
;;; is here rendered as straight HTML, other than historical)

(defun indirect-to-redisplay (uid pkgname)
  (concatenate 
   'string
   #.(one-string
      "<html>" nl
      "<head>" nl
      "<script type=\"text/javascript\">" nl
      "function locate()" nl
      "{" nl)
   (format nil "location=\"/redisplay.html?uid=~D&pkg=~A#TAG\"" uid pkgname) 
   nl
   #.(one-string-nl
      "}"
      "</script>"
      "</head>"
      "<body>"
      "<script type=\"text/javascript\">"
      "locate();"
      "</script>"
      "</body>"
      "</html>"
      )))

(defun indirect-to-vpl (pkgname)
  (let ((vpl-url (make-vpl-start-url :pkg pkgname)))
    (concatenate 
     'string
     #.(one-string
        "<html>" nl
        "<head>" nl
        "<script type=\"text/javascript\">" nl
        "function locate()" nl
        "{" nl)
     (formatn "location=\"~A\"" vpl-url)
     nl
     #.(one-string-nl
        "}"
        "</script>"
        "</head>"
        "<body>"
        "<script type=\"text/javascript\">"
        "locate();"
        "</script>"
        "</body>"
        "</html>"
        ))))

(defun indirect-to-session-redisplay (uid sessionid)
  (one-string-nl
   #.(one-string-nl
      "<html>" 
      "<head>" 
      "<script type=\"text/javascript\">" 
      "function locate()" 
      "{" )
   (formatn
    "location=\"/redisplay.html?uid=~D&sessionid=~A#TAG\"" uid sessionid) 
   #.(one-string-nl
      "}"
      "</script>"
      "</head>"
      "<body>"
      "<script type=\"text/javascript\">"
      "locate();"
      "</script>"
      "</body>"
      "</html>"
      )))

(defun indirect-to-function (function-name url)
  (concatenate 'string
    "<script type=\"text/javascript\">" nl
    (format nil "function ~A()" function-name) nl
    "{" nl
    (format nil "location=\"~A\"" url) nl
    "}" nl
    "</script>" nl
    ))

;;; The state of the user's input history completion is kept
;;; on his property list under :next-match-index and :previous-match.

;;; The idea is that if there is a previous match, and the user doesn't
;;; want it, he can try again and the system will automagically find a
;;; match previous to the one if found before.  

(defun do-input-history-completion (form-string)
  (new-do-input-history-completion form-string)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Here we do 'direct' evaluation of forms

;;; The user invokes the URL /biolingua-evalstring.html
;;; and provides it with arguments specifying the user, the string
;;; to be read/evaluated, and delimiters for the result components.

;;; Items that are returned consist of a user-specified set of prefix
;;; characters which default to ++=++, a header, the datum itself,
;;; and a user-specified set of suffix characters which also default to
;;; ++=++.

;;; One or more items make up the response.

;;; The header can be one of:

;;; ERROR: NO USER.
;;; ERROR: INTERNAL ERROR.
;;; ERROR: READ ERROR.
;;; ERROR: COMPILATION-ERROR.
;;; ERROR: EVALUATION ERROR.
;;; ERROR: PRINT ERROR.
;;; PRINTOUT:
;;; VALUES:
;;; VALUE <n>: 

;;; The ERROR headers are followed by a printout of the actual Lisp
;;; error that occurred.

;;; Anything that gets printed out by the compilation/evaluation of the form is
;;; returned in the PRINTOUT header.

;;; The number of values returned by the evaluation of the form is
;;; returned in the VALUES header.

;;; The nth value returned by the evaluation of the form is then
;;; returned in successive VALUE <n> headers.

;;; It is possible to have both a PRINTOUT header and an
;;; ERROR: COMPILATION-ERROR.  and/or ERROR: EVALUATION ERROR header 
;;; in the response.  All other errors are the sole header of a response.

;;; If a form evaluates without error and with no printout then there
;;; will always be at least two items, the VALUES: item and at least
;;; one VALUE <n> item.

;;; If the string cannot be turned into a form using READ-FROM-STRING,
;;; an ERROR: READ ERROR. header is returned.

;;; Here is an example URL:

;;; http://nostoc.stanford.edu:8002/biolingua-evalstring.html?pkg=MASSAR&evalstring=(progn (print 1) (values 2 3))&compile=T

;;; Here is the response (the #| and |# and the null lines below and above
;;; are not part of the response):

#|

++=++ PRINTOUT: 
1  ++=++
++=++ VALUES: 2 ++=++
++=++ VALUE 0: 2 ++=++
++=++ VALUE 1: 3 ++=++

|#

;;; There are four items in the response on five lines (because the
;;; PRINT function inserts a newline into the output stream)

;;; By specifying BEGIN=<prefix-chars) and/or END=<suffix-chars>
;;; in the URL one can change the prefix and suffix, respectively,
;;; for the items returned in the response.

;;; Here is another example URL showing how to use BEGIN and END
;;; and what happens when an error occurs.

;;; http://nostoc.stanford.edu:8002/biolingua-evalstring.html?pkg=MASSAR&evalstring=(error "oops")&begin=$$ &end= ***&compile=T

;;; Here is the result:

#|

$$ ERROR: EVALUATION ERROR. oops ***

|#


(publish 
 :path "/biolingua-evalstring.html"
 :content-type cl-user::*html-publish-content-type*
 :function
 (lambda (req ent)
   ;; Get the data currently in the text input fields
   (let* ((url-assoc-list (request-query req))
          (package-name (url-parameter-value :pkg url-assoc-list))
          (text-input (url-parameter-value :evalstring url-assoc-list nil))
          (begin-string (url-parameter-value :begin url-assoc-list nil))
          (end-string (url-parameter-value :end url-assoc-list nil))
          (compile? (url-parameter-value :compile url-assoc-list t))
          )
     (with-http-response-and-body (req ent)
       (with-internal-weblistener-errors-caught (req)
       (direct-evaluation-response 
        package-name text-input begin-string end-string compile?)
       )))))

(defun direct-evaluation-response 
       (package-name text-input begin end compile?)
  (declare (ignore input))

  (when (null begin) (setq begin "++=++ "))
  (when (null end) (setq end " ++=++"))
  (when (and compile? (string-equal compile? "T")) (setq compile? t))

  (handler-case

      (let* ((package-symbol (keywordize package-name)))

        (if (null (find-package package-symbol))

            ;; No package corresponding to the user means either 
            ;; a) system has been rebooted
            ;; b) user never logged before using this URL
            ;; c) someone has played a joke/hack by deleting the package

            (html
             (:princ-safe
              (surround
               (formatn
                (one-string-nl
                 "ERROR: NO USER. No user '~A' found."
                 "(The Weblistener may have been restarted.)")
                package-symbol)
               begin end)))

          ;; Set up the user's environment and call the READ-EVAL-STORE code

          (with-protected-globals-bound
              package-symbol
            (with-process-information-bound (text-input)
              (let ((repl (make-default-repl)))
                (setf (repl-input-string repl) (space-trim text-input))
                (setf (repl-evaluation-timeout-threshold repl)
                      *execution-timelimit*)
                (setf (repl-compilation-mode repl) compile?)
                (direct-evaluation-repl repl begin end)
                )))

          ))

    (error
     (c)
     (html
      (:princ-safe 
       (surround (formatn "ERROR: INTERNAL ERROR. ~A" c) begin end))))

    ))


(defun direct-evaluation-repl (repl begin end)

  (let ((in-string (repl-input-string repl))
        (error-condition (repl-error-condition repl)))

    (labels ((send-it (string &optional (br? t))
             (declare (ignore br?))
             (let ((output (surround string begin end)))
               (html (:princ-safe output) (:princ-safe nl))
               (log-user-event (formatn "~A~%" output))
               ))
           (maybe-send-printout ()
             (let ((printout
                    (one-string
                     (or (repl-compilation-printout repl) "")
                     (or (repl-evaluation-printout repl) "")
                     )))
               (when (plusp (length printout))
                 (send-it (formatn "PRINTOUT: ~A" printout))
                 ))))

      (log-user-event "Direct evaluation input: ~A~%" in-string)

      (low-level-repl repl)

      ;; Two cases.  Either there was an error detected or not.
      ;; (Completion is never enabled).

      (cond

       ((not error-condition)
        (maybe-send-printout)
        (let ((value-strings (repl-output-value-strings repl)))
          (send-it (formatn "VALUES: ~D" (length value-strings)))
          (loop for j fixnum from 0 for result in value-strings do
                (send-it (formatn "VALUE ~D: ~A" j result))
                )))

       (t
        (send-it
         (one-string
          "ERROR. "
          (cond
           ((repl-read-error? repl) "READ ERROR.")
           ((repl-compilation-error? repl) "COMPILATION ERROR.")
           ((repl-eval-error? repl) "EVALUATION ERROR.")
           ((repl-print-error? repl) "PRINT ERROR.")
           )
          " "
          (condition-to-string error-condition)
          ))
        (maybe-send-printout)
        )

       ))))
