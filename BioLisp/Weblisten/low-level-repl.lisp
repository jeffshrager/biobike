;;; -*- mode: Lisp; Syntax: Common-Lisp; Package: WEBLISTENER; -*-

(in-package :weblistener)

;;; +=========================================================================+
;;; | Copyright (c) 2002-2009 The BioBike Team                                |
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

;;; Author:  JP Massar, Jeff Shrager

(setq *new-repl-enabled?* t)

(defmacro with-all-standard-output-to-string 
          ((string-stream-symbol) &body body)
  `(with-output-to-string (,string-stream-symbol)
     (let ((*standard-output* ,string-stream-symbol) 
           (*error-output* ,string-stream-symbol) 
           (*trace-output* ,string-stream-symbol))
       ,@body
       )))

(defstruct repl
  ;; Input parameters
  input-string
  input-string-verification-function
  input-form-verification-function
  compilation-timeout-threshold
  evaluation-timeout-threshold
  parentheses-mode-enabled?
  completion-enabled?
  completion-limit
  compilation-mode
  ;; output parameters
  input-string-modified?
  input-string-used
  input-form
  parentheses-extension-type
  completion-suggested?
  suggested-completion
  compilation-printout
  evaluation-printout
  output-values
  output-value-strings
  error-condition
  read-error?
  compilation-error?
  eval-error?
  print-error?
  start-repl-time 
  end-compilation-time 
  end-execution-time
  compilation-warnings 
  )

(defvar *new-cw-algorithm* t)

(defun default-repl-input-string-verification-function (string)
  (repl-verify-input-string-method cl-user:*ai* string))

(defmethod repl-verify-input-string-method ((app t) string)
  ;; Must return non-nil if valid
  (declare (ignore string))
  t
  )

(defun default-repl-input-form-verification-function (form)
  (repl-verify-input-form-method cl-user::*ai* form))

(defmethod repl-verify-input-form-method ((app t) form)
  (repl-verify-input-subform form form))

(defun copy-repl-inputs (from-repl to-repl)
  (macrolet ((copy-slots (&rest slots)
               `(progn
                  ,@(loop for slot in slots collect
                          `(setf (,slot to-repl) (,slot from-repl))
                          ))))
    (copy-slots
     repl-input-string-verification-function
     repl-input-form-verification-function
     repl-compilation-timeout-threshold
     repl-evaluation-timeout-threshold
     repl-parentheses-mode-enabled?
     repl-completion-enabled?
     repl-completion-limit
     repl-compilation-mode
     )))

(defun make-default-repl ()
  (make-repl
   :input-string-verification-function 
   'default-repl-input-string-verification-function
   :input-form-verification-function
   'default-repl-input-form-verification-function
   :compilation-timeout-threshold *default-compilation-timeout*
   :evaluation-timeout-threshold nil
   :parentheses-mode-enabled? nil
   :completion-enabled? nil
   :completion-limit 25
   :compilation-mode nil
   :input-form nil
   :input-string-modified? nil
   :parentheses-extension-type nil
   :completion-suggested? nil
   :suggested-completion nil
   :compilation-printout nil
   :evaluation-printout nil
   :output-values nil
   :output-value-strings nil
   :error-condition nil
   :read-error? nil
   :compilation-error? nil
   :eval-error? nil
   :print-error? nil
   :start-repl-time 0
   :end-compilation-time 0 
   :end-execution-time 0
   ))


(define-condition repl-error (error) ())

(define-condition wb-unbound-error (repl-error)
  ((choices :initarg :choices :accessor choices)
   (name :initarg :name :accessor name)
   (error-string :initarg :error-string :accessor error-string)
   )
  )

(define-condition input-string-verification-failed (repl-error)
  ((reason :initarg :reason :reader input-verification-failed-reason))
  (:report 
   (lambda (condition stream)
     (format 
      stream 
      (one-string-nl
       "<<< Input string verification failed. Actual problem: >>>" 
       "<<< ~A >>>")
      (input-verification-failed-reason condition))
     )))

(define-condition read-failed (repl-error)
  ((reason :initarg :reason :reader read-failed-reason))
  (:report 
   (lambda (condition stream)
     (format 
      stream 
      (one-string-nl
       "<<< Read of input string failed. Actual error: >>>"
       "<<< ~A >>>")
      (read-failed-reason condition))
     )))

(define-condition repl-termination (condition) ())

(define-condition input-form-verification-failed (repl-error)
  ((reason :initarg :reason :reader input-verification-failed-reason))
  (:report 
   (lambda (condition stream)
     (format 
      stream 
      (one-string-nl
       "<<< Input form verification failed. Actual problem: >>>" 
       "~A")
      (input-verification-failed-reason condition)
      ))))

(define-condition timelimit-exceeded (repl-error)
  ((limit :initarg :limit :reader timeout-exceeded-limit))
  (:report 
   (lambda (condition stream)
     (let ((limit (timeout-exceeded-limit condition)))
       (if wb::*vpl-executing?* 
           (format 
            stream
            (one-string-nl
             ""
             "*** TIMEOUT ! TIMEOUT ! TIMEOUT ***"
             "*** COMPUTATION ABORTED AFTER ~D SECONDS ***"
             "*** YOU CAN: "
             "*** 1. Find out more about time limits by"
             "       clicking HELP and searching for time limit."
             "*** 2. Contact support for help: ~A"
             "*** 3. Ask yourself whether what you tried to do really ought to"
             "       take that long. Often times it is some silly mistake that"
             "       causes a process to take forever."
             "*** 4. Extend the execution time up to 1 hour by changing your"
             "       preferences. You can access your preferences through the"
             "       SESSIONS menu. Remember that the time limit is your friend."
             "       Be sure to change it back after you've completed what you"
             "       want to do."
             )
            limit
            cl-user::*default-support-email-address*
            )
         (format 
          stream 
          (one-string-nl
           ""
           "*** TIMEOUT ! TIMEOUT ! TIMEOUT ***"
           "*** COMPUTATION ABORTED AFTER ~D SECONDS ***"
           "*** YOU CAN: "
           "***   - Contact support for help: ~A"
           "***   - Use the TOOLS -> PREFS menu or the WB::SET-TIMELIMIT"
           "        function to extend your timeout up to 1 hour"
           "***   - Use RUNJOB to run your code in a separate process"
           "***   - Type (explain-timeout) at the weblistener for detailed info"
           )
          limit
          cl-user::*default-support-email-address*
          ))))))

(defun explain-timeout ()
  (formatt
   (one-string-nl 
    "  Your current time limit is ~d seconds. To permit longer execution"
    "use the TOOLS -> PREFS menu or the SET-TIMELIMIT function"
    "to increase your timeout threshold. e.g.: "
    " (set-timelimit ~d) "
    "  You may also investigate using RUNJOB which can run programs as "
    "independent processes for up to 2 hours.  Type"
    "(help runjob) "
    "for all the details."
    "  If your program is running for a relatively long time (more than several"
    "minutes, say) you may wish to consult with the support staff for ways"
    "in which to optimize your code.  (Improvements of more than a factor"
    "of 10 or more in certain cases have been achieved in this manner!)"
    "If you are running jobs which you know will take hours, then it is best"
    "to consult with the support staff as to other ways to execute it other"
    "than using the timeshared weblistener."
    "  The timelimit exists because you are not the only person using the"
    "system and the overall performance for everyone will degrade significantly"
    "as long computationally intensive programs are run.")
   *execution-timelimit* (* *execution-timelimit* 2)
   ))

(define-condition compilation-timelimit-exceeded (repl-error)
  ((limit :initarg :limit :reader compilation-timeout-exceeded-limit))
  (:report 
   (lambda (condition stream)
     (let ((limit (compilation-timeout-exceeded-limit condition)))
       (format 
        stream 
        (one-string-nl
         "<<< *** COMPILATION TIMEOUT ! COMPILATION TIMEOUT *** >>>"
         "<<< *** COMPUTATION ABORTED ***>>>"
         "<<< THE COMPILATION OF THE INPUT TOOK LONGER THAN ~D SECOND(S). >>>"
         "<<< TRY EXECUTING THE FORM AGAIN, AND IF IT CONTINUES TO HAPPEN >>>"
         "<<< THIS MAY MEAN THAT YOU'VE PUT THE COMPILER INTO AN INFINITE >>>"
         "<<< LOOP WITH AN INFINITELY RECURSIVE MACRO OR SOMESUCH. >>>"
         "<<< (IF THE PROBLEM CONTINUES TO HAPPEN AND 'RECURSIVE MACRO' >>>"
         "<<< DOESN'T MEAN ANYTHING TO YOU OR YOU BELIEVE THIS IS NOT YOUR >>>"
         "<<< FAULT PLEASE CONTACT THE SYSTEM SUPPORT STAFF. >>>")
         limit
         )))))


(define-condition compilation-failed (repl-error)
  ((reason :initarg :reason :reader compilation-failed-reason)
   (mode :initarg :mode :reader compilation-failed-mode))
  (:report 
   (lambda (condition stream)
     (format 
      stream 
      (one-string-nl
       "<<< Compilation of input form failed. Actual error: >>>"
       "<<< ~A>>>"
       (if (not (eq t (compilation-failed-mode condition)))
           "<<< (Weblistener compilation mode: ~S) >>>"
         ""))
       (compilation-failed-reason condition)
       (compilation-failed-mode condition)
       ))))

(define-condition evaluation-failed (repl-error)
  ((reason :initarg :reason :reader evaluation-failed-reason))
  (:report 
   (lambda (condition stream)
     (format 
      stream 
      (one-string-nl
       "<<< Evaluation failed. Actual error: >>>"
       "<<< ~A>>>"
       (case (user-mode cl-user:*ai*)
         (:bbl "")
         (otherwise "<<< Use (EXPLAIN) to see a stack trace. >>>")))
      (evaluation-failed-reason condition))
     )))

(define-condition print-failed (repl-error)
  ((reason :initarg :reason :reader print-failed-reason)
   (value-indicator 
    :initarg :value-indicator 
    :reader print-failed-value-indicator))
  (:report 
   (lambda (condition stream)
     (let ((vi (print-failed-value-indicator condition)))
       (format 
        stream 
        (one-string-nl
         "Ruh roh! Attempting to print out the ~D~A value (of ~D total values)"
         "resulting from the evaluation of the input form signaled an error."
         "Actual error: ~A")
        (first vi)
        (case (first vi)
          (1 "st") (2 "nd") (3 "rd") (otherwise "th"))
        (second vi)
        (print-failed-reason condition)
        )))))

(defun condition-to-string (c) (with-output-to-string (p) (princ c p)))


(defun low-level-repl (repl)

  (block exit

    (flet ((handle-condition (condition)
             ;; Specifically trap unbound vars for dwimming
             (when (typep condition 'evaluation-failed)
               (let ((actual-condition (evaluation-failed-reason condition)))
                 (return-from exit 
                   (dwimifiable-error-handler 
                    repl condition actual-condition))))
             ;; All the possible other ways the REPL can fail...
             (typecase condition
               ((or input-string-verification-failed read-failed)
                (setf (repl-read-error? repl) t))
               ((or input-form-verification-failed timelimit-exceeded)
                (setf (repl-eval-error? repl) t))
               ((or compilation-failed compilation-timelimit-exceeded)
                (setf (repl-compilation-error? repl) t))
               (print-failed
                (setf (repl-print-error? repl) t))
               ;; Used for completion, which is not an error.
               (repl-termination
                (setf (repl-error-condition repl) nil)
                (return-from exit repl))
               (error
                (setf (repl-eval-error? repl) t))
               (otherwise (signal condition))
               )
             (setf (repl-error-condition repl) condition)
             (return-from exit repl)
             ))

      (setf (repl-start-repl-time repl) (get-universal-time)) 
               
      (let ((input-string (space-trim (repl-input-string repl)))
            (input-form nil)
            (function-object nil)
            ;; For redo, possibly other uses.
            (*current-repl* repl)
            )

        (setf (repl-input-string-used repl) input-string)

        (handler-case
            (progn
              (repl-verify-input-string repl input-string)
              (setq input-form (repl-read-or-complete repl input-string))
              
              (repl-verify-input-form repl input-form)
              (setq 
               input-form
               (case (user-mode cl-user:*ai*)
                 (:bbl 
                  (forward-package-funcall 
                   :bbi :hack-code-for-bbl-toplevel input-form))
                 (otherwise input-form)
                 ))
              (multiple-value-setq (input-form function-object)
                  (repl-maybe-compile-form repl input-form))
              (setf (repl-end-compilation-time repl) (get-universal-time))
              (repl-evaluate-form-and-store-results
               repl function-object input-form)
              (setf (repl-end-execution-time repl) (get-universal-time))
              repl)
          ;; Make sure warnings don't get handled.
          (error (c) (handle-condition c))
          (repl-termination (c) (handle-condition c))
          )  
          

        ))))

(defun dwimifiable-error-handler (repl condition actual-condition) 
  (setf (repl-eval-error? repl) t)
  (flet ((new-unbound-condition (error-string)
           (let* ((name (cell-error-name actual-condition))
                  ;; CAR gets the symbols from (symbol score)
                  (choices 
                   (when (symbolp name) 
                     (mapcar #'car (near-matches (string-upcase name))))))
             (if t ;; choices 
                 (setf (repl-error-condition repl) 
                       (make-condition 
                        'wb-unbound-error
                        :choices 
                        (and choices (produce-choice-objects repl name choices))
                        :name name
                        :error-string error-string
                        ))
               (setf (repl-error-condition repl) condition)))))
    (cond ((typep actual-condition 'unbound-variable)
           (new-unbound-condition "variable"))
          ((typep actual-condition 'undefined-function)
           (new-unbound-condition "function"))
          (t (setf (repl-error-condition repl) condition))
          )
    repl))

(defvar *n-choices-to-show* 5)

(defun produce-choice-objects (repl bad-name choices)
  (loop for choice in choices 
        with input-form = (repl-input-form repl)
        as new-form = (subst choice bad-name input-form)
        as n from 1 to *n-choices-to-show*
        collect (make-dwim-choice
                 :symbol choice
                 ;; Obscurely if there's not change, subst will return the
                 ;; original tree, or a copy, which will be equal... UUU
                 :redo-form (unless (equal input-form new-form)
                              new-form)
                 )))

(defun make-dwim-choice (&rest args) args)

(defun null-string-to-nil (s) (if (zerop (length s)) nil s))


(defun repl-verify-input-string (repl input-string)

  ;; Do any verification of input string specified.  (The default method
  ;; simply returns T, so this whole clause is a noop standardly)
  ;; The verification function, if it detects a problem, should signal
  ;; an error of type INPUT-STRING-VERIFICATION-ERROR, a subclass of
  ;; REPL-ERROR, as defined above.  It should return NON-NIL if no problem
  ;; was detected.

  (handler-case
      (unless 
          (funcall (repl-input-string-verification-function repl) input-string)
        (signal
         (make-condition 
          'input-string-verification-failed
          :reason 
          "No error signaled. But input string verification returned NIL."
          )))
    (repl-error (c) (signal c))
    (error
     (c)
     (signal
      (make-condition
       'input-string-verification-failed
       :reason
       (formatn
        (one-string-nl
         "Possible verification function internal problem."
         "Unrecognized error occurred."
         "Actual error signaled:
         ~A")
        c
        ))))))

;;; The functions associated with each command in this list of 
;;; keyword weblistener commands are functions of one argument, a
;;; string which is the user's input.  The function must return
;;; a form which will then be evaluated by the weblistener.  
;;; The function therefore takes the place of the weblistener's
;;; READ part of its READ-EVAL-PRINT loop.  

;;; The function can parse the user's input to obtain arguments to 
;;; the command by using appropriate string functions such as 
;;; READ-FROM-STRING, STRING-SPLIT, SUBSEQ, etc.
 
(defparameter *weblistener-keyword-commands* 
  `(((":pa") ,(lambda (s) 
              (list 'in-package (keywordize (read-from-string (subseq s 4)))))
           "Executes an (in-package ...) form")
    ((":ex") ,(lambda (s) (declare (ignore s))  (list 'explain))
           "Executes an EXPLAIN command")
    ((":clt") ,(lambda (s) 
               (let ((file (second (string-split s))))
                 (list 'c/l (string-trim "\"" file) t)))
            "Executes a (load (compile-file ...)) form")
    ((":cl") ,(lambda (s) 
              (let ((file (second (string-split s))))
                (list 'c/l (string-trim "\"" file))))
           "Executes a (c/l ...) command (compile if necessary, then load)") 
    ((":cmds" ":?" ":help") 
     ,(lambda (s) 
	(let* ((words 
                (remove-if-not 
                 (lambda (string) 
                   (not (zerop (length (string-trim " " string)))))
                 (string-split s)))
	       (symbol-string (second words)))
	  (if symbol-string
	      ;; Despite documentation to the contrary, 
              ;; this call to help works slightly differently than
	      ;; a typical (help...) call. If you have provided just one word,
              ;; then it does (help word)
	      ;; but if you provide more than one, 
              ;; it does (help "all the words"). This permits you to 
	      ;; write a whole sentence after the initial :? ...
	      (if (null (third words))
		  `(help::help ,(read-from-string symbol-string))
		`(help::help ,(string-join (rest words) " "))
		)
	    `(progn (describe-keyword-commands) (help::help)))))
     "With an argument this does (help argument); with no argument this describes the keyword commands.")    
    ((":ask") 
     ,(lambda (s) 
	(let* ((words 
                (remove-if-not 
                 (lambda (string) 
                   (not (zerop (length (string-trim " " string)))))
                 (string-split s)))
	       (symbol-string (second words)))
	  (forward-funcall 
           'email-me
           (list
            (formatn 
             "~s asked ~s~%Click:~%~%http://nostoc.stanford.edu:8002/redisplay.html?uid=89&pkg=~a~%~%to connect to his session"
             *username* 
             words (string (wb::user-session-id))))
           :to "jshrager@stanford.edu"
           :subject "a biobike ask")
	  (if symbol-string
	      (if (null (third words))
		  `(help::help ,(read-from-string symbol-string))
		`(help::help ,(string-join (rest words) " "))
		)
	    `(progn (describe-keyword-commands) (help)))))
     "Describe what you are seeking! In addition to calling the HELP function, this also emails your request to the BioBike engineers, who will try to help you out.")
    ((":where") ,(lambda (s) 
                 (let ((symbol-string (second (string-split s))))
                   `(location-info ',(read-from-string symbol-string))))
              "Prints hyperlinked file location for symbol definition.")
    ((":procs" ":p") ,(lambda (s) 
		 (declare (ignore s))
                 `(all-my-processes))
     "List all my processes.")
    ((":stuff" ":mystuff" ":my-stuff" ":my" ":list")
     ,(lambda (s) 
	(declare (ignore s))
	`(my-stuff))
     "List my functions and variables.")
    ((":kill" ":k") ,(lambda (s) 
                 (let ((procno (second (string-split s))))
                   `(kill-my-process  ',(parse-integer procno))))
              "Kill the indicated process (by number).")
    ))

(defun weblistener-keyword-commands ()
  (mapcar (lambda (x) (mapcar #'read-from-string (first x)))
          *weblistener-keyword-commands*))

;;; Keyword can be a single key, or a list of keys.

(defun add-weblistener-keyword-command (keyword-or-keywords function doc)
  (let ((keywords (ensure-list keyword-or-keywords)))
    (delete-weblistener-keyword-command keywords)
    ;; (JS) ??? Why doesn't this just push ??? 
    (setq *weblistener-keyword-commands* 
	  (append *weblistener-keyword-commands*
		  (list (list (coerce-keyword-commands-to-strings keywords)
			      function doc))
		  )))
  (weblistener-keyword-commands))

(defun coerce-keyword-commands-to-strings (keywords)
  (loop for kw in keywords collect (one-string ":" (string-downcase kw))))

;;; Note that in order to replace a keyword you have to use the same set of 
;;; commands that you added them with, because otherwise the delete won't 
;;; be able to see them.  For example, if you want to replace (:foo :bar)
;;; you can't replace them with (:bar :foo) or anything else that isn't
;;; equal to (:foo :bar) -- One would think that one could just use set-equal
;;; but there are other screw cases, like (:foo :bar :baz) or (:bar) that
;;; we don't know what to do with... So screw it.

(defun delete-weblistener-keyword-command (keyword-or-keywords)
  (let ((keywords (ensure-list keyword-or-keywords)))
    (setq *weblistener-keyword-commands* 
	  (delete (coerce-keyword-commands-to-strings keywords)
		  *weblistener-keyword-commands* 
		  :test 'equal
		  :key 'first
		  )))
  (weblistener-keyword-commands))
  
(defun describe-keyword-commands ()
  (terpri)
  (loop for (cmds nil doc) in *weblistener-keyword-commands* do
        (formatt "~a => ~a~%" (string-join cmds " or ") doc)
	(terpri)))
           
(defun wb-command-or-read-from-string (s)
  (block exit
    ;; hack to make 'help foo' work like ':help foo'
    (when (initial-subsequence-of? s "help " :element-test 'char-equal)
      (setq s (s+ ":" s)))
    (loop for (keywords function) in *weblistener-keyword-commands* do
          (when (loop for kw in keywords
		      when (initial-subsequence-of? 
                            s kw :element-test 'char-equal)
		      do (return t))
            (return-from exit (funcall function s))))
    (case (user-mode cl-user:*ai*)
      (:bbl (forward-package-funcall :bbi :bbl-read-from-string s))
      (otherwise (read-from-string s))
      )))

(defun repl-read-or-complete (repl input-string)

  ;; Do the READ-FROM-STRING.
  ;; If we get an error reading, see if we can make sense of what the
  ;; user is trying to do and if so try the read again on a new string,
  ;; or indicate that evaluation is not to proceed because of completion.
  ;; In the default case we do the read and immediately exit the loop.

  (block exit
    (handler-case
        ;; this returns the input form if all goes well.
        (setf (repl-input-form repl) 
              (wb-command-or-read-from-string input-string))
      ;; in allegro 8.0 they came up with a new error, 
      ;; excl::extra-right-paren-error which does not
      ;; exist in allegro 7.0.  
      ;; In 7.0 when a superfluous right paren is encountered by the
      ;; reader, an end-of-file error is generated.  (in lispworks, 
      ;; a conditions::simple-reader-error is generated.  
      ;; so in 7.0, there is no way to distinguish between
      ;; a real eof and a superfluous right paren, while in 8.0
      ;; (and lispworks) there is (although the way to do it in 8.0
      ;; vs lispworks is different). 
      ;; This has no particular consequence in biolisp because a single 
      ;; form is read so the reader will stop before it gets to a 
      ;; superfluous right paren (unless one were to type a 
      ;; naked right paren) but in bbl
      ;; mode it's problematic because it tries to read multiple forms
      (end-of-file 
       ()
       (let ((eof-message 
              #.(formatn
                 (one-string-nl
                  "Unexpected end of input detected!"
                  "This could mean you forgot a paren or a double-quote,"
                  "or you typed too many closing parens."
                  ))))
         (when (repl-all-comments? input-string)
           (setf (repl-input-form repl) '(values))
           (return-from exit (repl-input-form repl)))
         ;; Try right parentheses extension.
         (when (repl-parentheses-mode-enabled? repl)
           (multiple-value-bind (new-string new-form fixup-type)
               (maybe-add-right-parentheses input-string)
             (when new-string
               (setf (repl-input-form repl) new-form)
               (setf (repl-parentheses-extension-type repl) fixup-type)
               (setf (repl-input-string-modified? repl) t)
               (setf (repl-input-string-used repl) new-string)
               (return-from exit new-form)
               )))
         ;; Give it up if no completion attempt asked for.
         (when (not (repl-completion-enabled? repl))
           (signal (make-condition 'read-failed :reason eof-message)))
         ;; Okay, try completion if string short enough.
         (when (< (length input-string) (repl-completion-limit repl))
           (let ((suggested-completion 
                  (do-input-history-completion input-string)))
             (when suggested-completion
               (setf (repl-completion-suggested? repl) t)
               (setf (repl-suggested-completion repl) suggested-completion)
               (signal (make-condition 'repl-termination))
               )))
         ;; got EOF and didn't handle it.  Give it up.
         (signal (make-condition 'read-failed :reason eof-message))
         ))
      ;; Some random error in the READ-FROM-STRING
      (error (c) (signal (make-condition 'read-failed :reason c)))
      )))

(defun repl-all-comments? (s)
  (or
   (every 
    (lambda (x) (or (zerop (length x)) (char= #\; (char x 0))))
    (string-split s #\Newline))
   (let ((len (length s)) (c1 #\#) (c2 #\|))
     (and (>= len 4) 
          (char= c1 (char s 0)) (char= c2 (char s 1))
          (char= c2 (char s (- len 2))) (char= c1 (char s (- len 1)))
          ))))
          


(defun repl-verify-input-form (repl input-form)

  ;; Read was successful and we have an input form.  Verify it.

  (handler-case
      ;; Verification form should return non-NIL or signal a REPL-ERROR.
      (unless (funcall (repl-input-form-verification-function repl) input-form)
        ;; But just in case.
        (signal
         (make-condition 
          'input-form-verification-failed
          :reason "No error signaled. But input form verification function NIL!"
          )))
    (repl-error (c) (signal c)) 
    (error
     (c)
     (signal
      (make-condition
       'input-form-verification-failed
       :reason 
       (formatn
        (one-string-nl
         "Possible verification function internal problem."
         "Unrecognized error occurred."
         "Actual error signaled: ~A")
        c
        ))))))


(defun repl-maybe-compile-form (repl input-form)

  ;; See if we are supposed to compile the form.
  ;; If so, capture all output to normal output channels that happens
  ;; during the evaluation of FORM and create a function object which
  ;; when called will be equivalent to evaluating the input form.
    
  (let ((function-object nil) (compilation-output nil))

    (flet ((handle-compiler-output (s)
             ;; If the compiler output doesn't end with a Newline, add one
             (when (plusp (length s))
               (when (not (eql (lastelem s) #\Newline))
                 (setq s (one-string s (string #\Newline)))))
             (setf (repl-compilation-printout repl) s)
             ))

      (setq compilation-output
            (null-string-to-nil
             (with-all-standard-output-to-string (compiler-output)
               (handler-case
                   (multiple-value-setq (input-form function-object)
                       (maybe-compile-form-to-function-object repl input-form))
                 (compilation-timelimit-exceeded (c) (signal c))
                 (error
                  (c)
                  (handle-compiler-output 
                   (get-output-stream-string compiler-output))
                  (signal
                   (make-condition 
                    'compilation-failed
                    :reason c :mode (repl-compilation-mode repl)
                    )))))))

      (handle-compiler-output compilation-output)

      (handle-compiler-no-nos compilation-output repl)

      (values input-form function-object)

      )))

(defparameter *compiler-message-regexes* 
  (list 
   (list (ppcre:create-scanner "Warning.+is a constant and.+be set")
         "(Trying to change the value of a constant)")
   ))

(defun handle-compiler-no-nos (compilation-output repl)
  (loop for (regex message) in *compiler-message-regexes* do
        (when (cl-ppcre:all-matches regex compilation-output)
          (signal
           (make-condition 
            'compilation-failed 
            :reason
            (formatn "Weblistener detected fatal compiler warning: ~A" message)
            :mode (repl-compilation-mode repl)
            )))))


(defun repl-evaluate-form-and-store-results (repl function-object input-form)

  ;; Capture all output to normal output channels that happens
  ;; during the evaluation of the input form, and capture all the values 
  ;; that come back from the evaluation of the input form.

  ;; REPL itself is returned, as all the computation results are stored
  ;; within its structure.

  (let ((printout nil))
    (unwind-protect
        (setq 
         printout
         (with-all-standard-output-to-string (form-output)
           (handler-case
               (setf (repl-output-values repl)
                     (multiple-value-list
                      (let ((- input-form))
                        #-:SBCL
                        (declare (special -))
                        (repl-execute-form-with-timelimit 
                         function-object
                         input-form
                         (repl-evaluation-timeout-threshold repl)
                         (if function-object t nil)
                         ))))
             ;; Must store form output that occurred before evaluation error!
             (repl-error 
              (c) 
              (setq printout (copy-seq (get-output-stream-string form-output)))
              (signal c))
             (error 
              (c)
              (setq printout (copy-seq (get-output-stream-string form-output)))
              (signal (make-condition 'evaluation-failed :reason c))
              ))))
      ;; Make sure the output is truncated as per *output-limit*
      (progn
        (setf (repl-evaluation-printout repl) (null-string-to-nil printout))
        (let ((*output-limit-enabled* t))
          (vwhen (s (repl-evaluation-printout repl))
            (setf (repl-evaluation-printout repl) (formatn "~A" s))
            )))
      ))

  ;; Convert each output value into a string

  (flet ((result-to-string (r) 
           ;; First, convert the object R to a 'readable' string
           ;; Then run the string through the redefined PRINT-OBJECT method
           ;; for strings, thereby truncating the output width and
           ;; number of lines.
           
           (let ((readable-string 
                  (let ((*output-limit-enabled* t)
                        (*print-right-margin* nil))
                    (formatn "~S" r)
                    )))
             (when (not (stringp r))
               (setq readable-string (formatn "~A" readable-string)))
             readable-string)
                    
           ))

    (let ((value-count 0))
      (handler-case
          (setf (repl-output-value-strings repl)
                (loop for value in (repl-output-values repl) 
                      do (incf value-count)
                      collect (result-to-string value)))
        (error
         (c)
         (signal 
          (make-condition 
           'print-failed
           :reason c
           :value-indictor 
           (list value-count (length (repl-output-values repl)))
           )))))

    ))

(defun reduce-string-per-output-limit (string) string)


;;; Returns a new string, new form and a fixup type as three values
;;; if it manages to create a string that can be read 
;;; to produce a form by adding right parentheses, 
;;; otherwise it returns NIL.

(defun maybe-add-right-parentheses (form-string)

  (block exit

    (let ((ch (lastelem form-string))
          (lp #\() (rp #\)) (rps ")") (rb #\])
          (unique (cons 1 1)))

      (flet ((try-string (s fix-type)
               (let ((new-form 
                      (handler-case
                          (case (user-mode cl-user:*ai*)
                            (:bbl 
                             (forward-package-funcall 
                              :bbi :bbl-read-from-string s))
                            (otherwise (read-from-string s))
                            )
                        (end-of-file () unique)
                        (error () (return-from exit nil))
                        )))
                 (unless (eq new-form unique)
                   (return-from exit (values s new-form fix-type))
                   ))))

        (cond

         ;; A right bracket at the very end.  Presumably the user
         ;; intended this to mean 'close all remaining parens'
         ((and (not (eq (user-mode cl-user:*ai*) :bbl)) (eql ch rb))
          (let* ((s (subseq form-string 0 (1- (length form-string))))
                 (lpcount (count lp s)))
            (loop for j fixnum from 0 below lpcount do
                  (setq s (one-string s rps))
                  (try-string s :explicit-bracket)
                  )))

         ;; A right paren.  Presumably the user just didn't provide
         ;; enough right parens to close the form.  Note this is
         ;; equivalent in semantics to ']'.
         ((eql ch rp)
          (let* ((s form-string)
                 (lpcount (count lp s))
                 (rpcount (count rp s))
                 (excess-lps (- lpcount rpcount)))
            (when (plusp excess-lps)
              (loop for j fixnum from 0 below excess-lps do
                    (setq s (one-string s rps))
                    (try-string s :not-enough-parens)
                    ))))

         (t nil)

         )))))


(defun repl-verify-input-subform (form main-form)
  (cond 
   ((null form) t)
   ((atom form) t)
   ((listp form)
    (or (eq 'quote (first form))
        (and
         (repl-verify-function-call-input-form form main-form)
         (repl-verify-input-subform (first form) main-form)
         (repl-verify-input-subform (rest form) main-form)
         t)))
   (t t)
   ))

#+:SBCL
(defparameter *implementation-specific-dangerous-functions*
  '(sb-ext::quit))

#+:ALLEGRO
(defparameter *implementation-specific-dangerous-functions*
  '(
    excl::run-shell-command 
    excl::shell 
    excl::exit sys::lispexit excl::exit-lisp excl::exit-1
    excl::filesys-delete-file
    excl::delete-directory excl::delete-directory-and-files
    ))

#+:LISPWORKS
(defparameter *implementation-specific-dangerous-functions* nil)

(defparameter *dangerous-functions*
  (append
  '(import 
    shadowing-import unexport delete-package 
    delete-file y-or-no-p yes-or-no-p
    )
  *implementation-specific-dangerous-functions*
  ))

(defun repl-verify-function-call-input-form (form main-form &aux symbol)

  (flet ((oops (r)
           (signal (make-condition 'input-form-verification-failed :reason r))
           ))

    (cond
     ;; Prevent TRACE of anything but user's own functions.
     ;; Otherwise TRACE would happen to everyone...
     ((and 
       (eq (first form) 'trace)
       (let ((user-package (find-package *username*)))
         (some
          (lambda (x) 
            (and (symbolp x) 
                 (not (eq (symbol-package (setq symbol x)) user-package))))
          (cdr form)
          )))
      (oops
       (formatn
        (one-string-nl
         "*** Invalid use of TRACE: ~A"
         "<<<<< You are trying to use TRACE on ~A. >>>>>"
         "Tracing a global function would mean that everyone using the system"
         "would see the trace output from it if they were to call that"
         "function!  This is probably not a good idea..."
         "Please use a different mechanism or talk to the support staff."
         "(If this message is being generated erroneously because you have"
         "a variable named TRACE please change its name -- sorry!)")
        main-form symbol
        )))
     ;; Prevent use of BREAK
     ((eq (first form) 'break)
      (oops
       (formatn
        (one-string-nl
         "*** Invalid use of BREAK: ~A"
         "<<<<< You are trying to use the Lisp function BREAK. >>>>>"
         "You can't use BREAK in the WebListener because it has no"
         "interactive debugger.  Please use other means of debugging, such"
         "as print statements, global variable assignements or (error ...)"
         "traps. For more information you may wish to consult the tutorial"
         "on program development and debugging."
         "(If this message is being generated erroneously because you have"
         "variable named BREAK please change its name -- sorry!)")
        main-form
        )))
     ;; Prevent calling functions with possible unintended side-effects.
     ((and (atom (first form))
           (member (first form) *dangerous-functions*)
           (not (weblistener-guru-p))
           )
      (oops
       (formatn
        (one-string-nl
         "*** Attempt to call restricted function: ~A"
         "<<<<< You are trying to use function ~A. >>>>>" 
         "That function either won't work in the Weblistener or could have"
         "dangerous side effects so the system will not let you execute it."
         "Please talk to the support staff if you feel you need to execute"
         "this function.  Thanks.")
        (first form) (first form)
        )))
     ;; Prevent anything else specific to the application
     (t (application-verify-function-call-form cl-user:*ai* form main-form))
     )

    t

    ))


;;; Some Common Lisp forms have been shadowed,
;;; so canonicalize to keywords for the test.

(defun minimal-compilation-form? (x)
  (and (symbolp x)
       (member (keywordize x)
               '(
                 :defun :defmacro :defmethod :loop :dotimes :dolist :do
                  :if :when :unless :cond
                  :let :let*
                  :time
                  ))))

(defun most-compilation-form? (form) (not (eq 'quote (first form))))

(defun maybe-compile-form-to-function-object (repl input-form)
  (case (user-mode cl-user:*ai*)
    (:bbl 
     (forward-package-funcall :bbi :bbl-compile-repl-form input-form repl))
    (otherwise
     (if (and (consp input-form)
              (ecase (repl-compilation-mode repl)
                ((nil) nil)
                ((:full :total :all) (not (eq 'quote (first input-form))))
                ((:most t) 
                 (most-compilation-form? input-form)
                 #+does-not-work-well
                 (when (most-compilation-form? input-form)
                   (let ((new-form (maybe-transform-setq/setf-form input-form)))
                     (setq *input-form* new-form)
                     ;; If we did transform a SETQ/SETF, don't compile, but
                     ;; return the new form.
                     (prog1 (eq new-form input-form) (setq input-form new-form))
                     )))
                (:minimal (minimal-compilation-form? (first input-form)))
                ))
         (values
          input-form
          (compile-form-to-function-object 
           (wrap-defun-with-locally-hack input-form) repl))
       (values input-form nil)
       ))))

(defun wrap-defun-with-locally-hack (input-form)
  ;; horrible hack to get rid of warning about undefined function
  ;; when the defun is recursive.  By first declaring the function, 
  ;; we fool the compiler into not issuing the warning (which 
  ;; probably shouldn't be issued in the first place, but there 
  ;; doesn't seem to be any other way of making it stop issuing
  ;; the warning). 
  (if (and (listp input-form) 
           (eq 'wlisp::defun (first input-form))
           (symbolp (second input-form)))
      `(locally 
         (declare (ftype function ,(second input-form)))
         ,input-form)
    input-form))

(defun compile-form-to-function-object (input-form repl)
  (if *new-cw-algorithm* 
      (new-compile-form-to-function-object input-form repl)
    (old-compile-form-to-function-object input-form repl)
    ))

(defun old-compile-form-to-function-object (input-form repl)
  (vif (ctt (repl-compilation-timeout-threshold repl))
       (with-timeout-limit
           (ctt
            (signal
             (make-condition 'compilation-timelimit-exceeded :limit ctt)
             ))
         (progn 
           (compile nil `(lambda () ,input-form))))
       (progn 
         (compile nil `(lambda () ,input-form)))
       ))

;;; New mechanism to handle compiler warnings.  
;;; Instead of just allowing them to print out, we capture each warning
;;; and examine it, then based on that examination either cause the warning
;;; text to be printed out (and hence shown to the user) or we suppress 
;;; the warning.  All warnings are now stored in the REPL structure slot
;;; COMPILATION-WARNINGS in case we ever want to refer to them further up.  

(defun new-compile-form-to-function-object (input-form repl)
  (vif (ctt (repl-compilation-timeout-threshold repl))
       (with-timeout-limit
        (ctt
         (signal
          (make-condition 'compilation-timelimit-exceeded :limit ctt)
          ))
        (compile-form-with-warnings-hacked input-form repl))
       (compile-form-with-warnings-hacked input-form repl)
       ))

(defun compile-form-with-warnings-hacked (form repl)
  (let ((saved-warnings nil))
    (flet ((compiler-warning-handler 
                   (c)
             (let ((r (find-restart 'muffle-warning c)))
               (unless r 
                 (error 
                  "Internal error. Should be a restart for muffle warning! ~A"
                  c
                  ))
               (when r 
                 (push c saved-warnings)
                 (invoke-restart r)
                 ))))
      (handler-bind ((warning #'compiler-warning-handler))
        (prog1 
            (compile nil `(lambda () ,form))
          (loop for w in (setq saved-warnings (reverse saved-warnings))
            as warning-text = (format nil "~A" w)
            do
            (case (wb-compiler-warning-action cl-user:*ai* w warning-text repl)
              ((:excise :delete :remove) nil)
              ((t nil :print) 
               ;; the wb-compiler-warning-action may have smashed the
               ;; warning message so recompute its text 
               (format t "Warning: ~A~%" w))
              (otherwise nil)
              ))
          (setf (repl-compilation-warnings repl) saved-warnings)
          )))))

;;; For any warning you want to deal with, test for it here, 
;;; and return :delete if you don't want it to be shown to the user, 
;;; or :print if you do.  Regardless, the warning is still captured and
;;; stored on the list of warnings (in the REPL structure).  

;;; You can customize this based on the application instance by writing
;;; a new method.  

(defmethod wb-compiler-warning-action ((app-instance t) warning text repl)
  (declare (ignorable warning))
  (let* ((match "Free reference to undeclared variable")
         (pos (search match text :test 'string-equal)))
    (cond 
     ((and pos (not (defining-form? (repl-input-form repl))))
      :delete)
     ;; change the allegro message to one that Jeff Elhai likes...
     #+:allegro
     (pos 
      (let* ((wordlist (string-split text #\Space))
             (varname (sixth wordlist)))
        (setf (slot-value warning 'excl::format-control) 
              (formatn
               (one-string
                "The variable '~A' was not defined within the function, "
                "and has not been defined using DEFINE.")
               varname
               ))
        (setf (slot-value warning 'excl::format-arguments) nil)
        :print 
        ))
     (t :print)
     )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
               
(defun repl-execute-form-with-timelimit 
       (function form timeout-limit compiled?)
  (if (null timeout-limit)
      (repl-execute-form cl-user:*ai* function form compiled?)
    (progn
      (when (> timeout-limit *default-execution-timelimit*)
        (set-process-priority -1))
      (setq timeout-limit (min *maximum-execution-timelimit* timeout-limit))
      (with-timeout-limit 
          (timeout-limit
           (signal (make-condition 'timelimit-exceeded :limit timeout-limit)))
        (repl-execute-form cl-user:*ai* function form compiled?)
        ))))

(defmethod repl-execute-form ((app t) function form compiled?) 
  (block exit
    (handler-bind
        ((error 
          (lambda (c)
            (repl-handle-eval-error c form) 
            (signal (make-condition 'evaluation-failed :reason c))
            )))
      (cond
       ;; hack to prevent allegro from destroying itself in the 
       ;; repl-handle-eval-error function
       ((and (consp form) (null (first form))) 
        (error "Cannot call NIL!!"))
       (compiled? (funcall function))
       (t (eval form))
       ))))

;;; We need a :count 100 here because if we get a stack overflow and
;;; try to print the entire stack the system seems to hang.



(defun repl-handle-eval-error (c form)
  "Store the stack trace so that an (EXPLAIN) command can retrieve it."
  (declare (ignore c))
  (setf (get (user-session-id) :stack-trace-string) 
        (stack-trace-to-string :count 100))
  (setf (get (user-session-id) :stack-backtrace-forms) (get-frames-list))
  (setf (get (user-session-id) :stack-trace-form) form)
  )

;;; From David Margolies  <dm@franz.com> 

#+:allegro
(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :sundebug))
 
#+:allegro
(defun get-frames-list ()
  (let ((*terminal-io* excl::*null-stream*)
        prev cur lis old frames-before-error)
    ;; This isn't supposed to be able to error out, but in 
    ;; a weird case that seems like every other error to us, but
    ;; must be different somehow, this function errors out
    ;; calling DEBUG:FRAME-EXPRESSION on CUR in the second loop,
    ;; generating a `NIL' is not of the expected type `REAL' error.
    ;; This unexpected signaling of an error was masking the real 
    ;; error we are trying to get information on and report.  
    ;; The only solution I could come up with was to trap this
    ;; bizarre error signal and ignore it.  The only effect doing this
    ;; has is that when this bizarre error occurs, typing (explain)
    ;; at the weblistener won't get you any stack listing.  
    (handler-case
        (progn 
          (setq prev (debug:newest-frame))
          (setq old (debug:oldest-frame))
          (loop
           (setq cur (debug:next-older-frame prev))
           (when (null cur)
             (return-from get-frames-list (nreverse frames-before-error)))
           (push (debug:frame-expression cur) frames-before-error)
           (when (eq 'error (car (debug:frame-expression cur)))
             (setq prev cur) (return))
           (setq prev cur))
          (loop
           (setq cur (debug:next-older-frame prev))
           ;; We want to see every frame and make a decision ourselves.
           (if t ;(debug:frame-visible-p cur) 
               (push (debug:frame-expression cur) lis))
           (if (debug:frame-reference-eq cur old)
               (return))
           (setq prev cur))
          )
      (error () (setq lis nil)))
    (nreverse lis)))

#+:lispworks
(defun get-frames-list ()
  nil)

#-(or :allegro :lispworks)
(cformatt "*** get-frames-list not implemented ***")

#+:allegro
(defun explain ()
  #.(one-string-nl
     "Prints out a stack trace from the most recently occurring error."
     "The stack trace is edited to remove some frames that are deemed"
     "useless before being printed.  You can view the entire stack trace"
     "by executing (wb::explain! :verbose? t), but you don't want this!"
     )
  (loop for expr in (get (user-session-id) :stack-backtrace-forms)
	until (and (listp expr) 
                   (eq 'REPL-EXECUTE-FORM-WITH-TIMELIMIT (car expr)))
	do (print expr)
	))

#+:lispworks
(defun explain () (formatt "Not implemented."))

#+:allegro
(defun explain! (&optional (verbose? nil))
  (formatt
   "~A~%"
   (let ((trace (get (user-session-id) :stack-trace-string)))
     (if verbose?
         trace
       (let* ((s1 "STACK-TRACE-TO-STRING)")
              (s2 "NET.ASERVE:PROCESS-ENTITY")
              (pos1 (search s1 trace))
              (pos2 (search s2 trace)))
         (subseq 
          trace
          (or (and pos1 (+ pos1 (length s1))) 0)
          (or (and pos2 (- pos2 10)) (length trace)))
         ))))
  nil)

(defmethod application-verify-function-call-form ((app t) form main-form)
  (declare (ignore form main-form))
  nil
  )

(defun weblistener-guru-p (&optional (user *username*))
  (eq :guru (user-status (find-login-record (string user)))))
(defun weblistener-demo-p (&optional (user *username*))
  (eq :demo (user-status (find-login-record (string user)))))

(defmacro guru-trace (&rest function-names) `(trace ,@function-names))


(defun maybe-transform-setq/setf-form (form)
  (if (and (consp form)
           (or (eq (first form) 'setq) (eq (first form) 'setf))
           ;; Make sure not a dotted pair
           (null (cdr (last form))))
      (flet ((compiled-setq (set-directive place value-form)
               (if (atom value-form)
                   `(,set-directive ,place, value-form)
                 `(,set-directive 
                   ,place 
                   (funcall (compile nil (lambda () ,value-form)))
                   ))))
        (let ((len (length form)))
          (cond
           ((or (< len 3) (not (oddp len))) form)
           ((= (length form) 3)
            (compiled-setq (first form) (second form) (third form)))
           (t
            `(progn
               ,@(loop for forms on (cdr form) by #'cddr collect
                       (compiled-setq (first form) (first forms) (second forms))
                       ))))))
    form
    ))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+test
(defun make-test-repl
       (input-string
        &key
        (isvf nil isvf-p)
        (ifvf nil ifvf-p)
        (ett nil ett-p)
        (pme nil pme-p)
        (ce nil ce-p)
        (cl nil cl-p)
        (cm nil cm-p))
  (let ((repl (make-default-repl)))
    (setf (repl-input-string repl) input-string)
    (when isvf-p (setf (repl-input-string-verification-function repl) isvf))
    (when ifvf-p (setf (repl-input-form-verification-function repl) ifvf))
    (when ett-p (setf (repl-evaluation-timeout-threshold repl) ett))
    (when pme-p (setf (repl-parentheses-mode-enabled? repl) pme))
    (when ce-p (setf (repl-completion-enabled? repl) ce))
    (when cl-p (setf (repl-completion-limit repl) cl))
    (when cm-p (setf (repl-compilation-mode repl) cm))
    repl
    ))
    
#+test
(defun compare-repl-result
       (test-id repl
        &key
        (ism nil ism-p)
        (isu nil isu-p)
        (if nil if-p)
        (pet nil pet-p)
        (cs nil cs-p)
        (sc nil sc-p)
        (cp nil cp-p)
        (ep nil ep-p)
        (ov nil ov-p)
        (ovs nil ovs-p)
        (ec nil ec-p)
        (re nil re-p)
        (ce nil ce-p)
        (ee nil ee-p)
        (pe nil pe-p)
        )
  (let ((oops-list nil))
    (flet ((test (provided? test-function expected slot-function)
             (when provided?
               (let ((actual (funcall slot-function repl)))
                 (unless (funcall test-function expected actual)
                   (push (list slot-function expected actual) oops-list)
                   )))))
      (test ism-p 'eq ism 'repl-input-string-modified?)
      (test isu-p 'string= isu 'repl-input-string-used)
      (test if-p 'equal if 'repl-input-form)
      (test pet-p 'eq pet 'repl-parentheses-extension-type)
      (test cs-p 'eq cs 'repl-completion-suggested?)
      (test sc-p 'string= sc 'repl-suggested-completion)
      (test cp-p 'equal cp 'repl-compilation-printout)
      (test ep-p 'equal ep 'repl-evaluation-printout)
      (test ov-p 'equalp ov 'repl-output-values)
      (test ovs-p 'equal ovs 'repl-output-value-strings)
      (when ec-p
        (unless (or (eq ec (repl-error-condition repl))
                    (subtypep ec (type-of (repl-error-condition repl))))
          (push 
           (list 'repl-error-condition ec (type-of (repl-error-condition repl)))
           oops-list
           )))
      (test re-p 'eq re 'repl-read-error?)
      (test ce-p 'eq ce 'repl-compilation-error?)
      (test ee-p 'eq ee 'repl-eval-error?)
      (test pe-p 'eq pe 'repl-print-error?)
      )
    (if oops-list
        (progn
          (cformatt "REPL test ~A failed." test-id)
          (loop for (slot expected actual) in oops-list do
                (cformatt "  Slot: ~A, Expected: ~S, Actual: ~S"
                          slot expected actual
                          )))
      (cformatt "Test ~A passed" test-id))
    repl
    ))

#+test
(defun test-repl ()
  ;; simplest test
  (compare-repl-result
   :repl-1
   (low-level-repl (make-test-repl "2"))
   :ism nil :isu "2" :if 2 :pet nil :cs nil :cp nil :ep nil
   :ov (list 2) :ovs (list "2") :ec nil :re nil :ce nil :ee nil :pe nil)
  ;; Form to be evaluated
  (compare-repl-result
   :repl-2
   (low-level-repl (make-test-repl "(+ 3 4) "))
   :ism nil :isu "(+ 3 4)" :if '(+ 3 4) :pet nil :cs nil :cp nil :ep nil
   :ov (list 7) :ovs (list "7") :ec nil :re nil :ce nil :ee nil :pe nil)
  ;; ']' test
  (compare-repl-result
   :repl-3
   (low-level-repl 
    (make-test-repl "(progn (progn (+ 3 4]" :pme t))
   :ism t :isu "(progn (progn (+ 3 4)))" :pet :explicit-bracket
   :ov (list 7) :ovs (list "7") :ec nil :re nil :ce nil :ee nil :pe nil)
  ;; right paren completion
  (compare-repl-result
   :repl-4
   (low-level-repl 
    (make-test-repl "(progn (progn (+ 3 4)" :pme t))
   :ism t :isu "(progn (progn (+ 3 4)))" :pet :not-enough-parens
   :ov (list 7) :ovs (list "7") :ec nil :re nil :ce nil :ee nil :pe nil)
  ;; Reader error
  (compare-repl-result
   :repl-5
   (low-level-repl 
    (make-test-repl "(progn (progn ,(+ 3 4)" :pme t))
   :ism nil :pet nil
   :ov nil :ovs nil :ec 'read-failed :re t :ce nil :ee nil :pe nil)
  ;; Input form verification error
  (compare-repl-result
   :repl-6
   (low-level-repl 
    (make-test-repl "(trace compile-file)" :pme t))
   :ism nil :pet nil
   :ov nil :ovs nil :ec 'input-form-verification-failed
   :re nil :ce nil :ee t :pe nil)
  ;; Evaluation error
  (compare-repl-result
   :repl-7
   (low-level-repl 
    (make-test-repl "(/ 9 0)" :pme t))
   :ism nil :pet nil
   :ov nil :ovs nil :ec 'evaluation-failed
   :re nil :ce nil :ee t :pe nil)
  ;; Evaluation timeout error
  #+:allegro
  (compare-repl-result
   :repl-8
   (low-level-repl 
    (make-test-repl "(sleep 4)" :pme t :ett 1))
   :ism nil :pet nil
   :ov nil :ovs nil :ec 'timelimit-exceeded
   :re nil :ce nil :ee t :pe nil)
  ;; Compilation mode
  (compare-repl-result
   :repl-9
   (low-level-repl 
    (make-test-repl "(let ((x 5)) (+ x 3))" :pme t :ett 1 :cm :total))
   :ism nil :pet nil
   :ov (list 8) :ovs (list "8") :ec nil
   :re nil :ce nil :ee nil :pe nil)
  ;; Insure form is being compiled
  (compare-repl-result
   :repl-10
   (low-level-repl 
    (make-test-repl 
     "(progn
        (defun foo (x) (1+ x))
        (if (compiled-function-p (symbol-function 'foo)) 1 0))"
     :pme t :ett 1 :cm :total))
   :ism nil :pet nil
   :ov (list 1) :ovs (list "1") :ec nil
   :re nil :ce nil :ee nil :pe nil)
  ;; Compilation error
  (compare-repl-result
   :repl-11
   (low-level-repl 
    (make-test-repl "(if)" :pme t :cm :total))
   :ism nil :pet nil
   :ov nil :ovs nil :ec nil
   :re nil :ce t :ee nil :pe nil)
  ;; Multiple values
  (compare-repl-result
   :repl-12
   (low-level-repl (make-test-repl "(values 1 2)" :cm :most))
   :ism nil :isu "(values 1 2)" :if '(values 1 2) 
   :pet nil :cs nil :cp nil :ep nil
   :ov (list 1 2) :ovs (list "1" "2") :ec nil :re nil :ce nil :ee nil :pe nil)
  ;; Printout during evaluation
  (compare-repl-result
   :repl-13
   (low-level-repl (make-test-repl "(progn (format t \"A\") 3)" :cm :most))
   :ism nil 
   :pet nil :cs nil :cp nil :ep "A"
   :ov (list 3) :ovs (list "3") :ec nil :re nil :ce nil :ee nil :pe nil)
  ;; Printout during evaluation
  (compare-repl-result
   :repl-13
   (low-level-repl (make-test-repl "(progn (format t \"A\") 3)" :cm :most))
   :ism nil 
   :pet nil :cs nil :cp nil :ep "A"
   :ov (list 3) :ovs (list "3") :ec nil :re nil :ce nil :ee nil :pe nil)
  ;; Printout during compilation
  (compare-repl-result
   :repl-14
   (low-level-repl 
    (make-test-repl 
     "(macrolet ((frob () (format t \"A\")))
        (frob) 
        (format t \"B\")
        0)"
     :cm :most))
   :ism nil 
   :pet nil :cs nil :cp "A" :ep "B"
   :ov (list 0) :ovs (list "0") :ec nil :re nil :ce nil :ee nil :pe nil)
  ;; Printout and error during evaluation
  (compare-repl-result
   :repl-15
   (low-level-repl 
    (make-test-repl "(progn (format t \"A\") (/ 4 0))"))
   :ism nil 
   :pet nil :cs nil :cp nil :ep "A"
   :ov nil :ovs nil :ec 'evaluation-failed :re nil :ce nil :ee t :pe nil)
  ;; Make sure - works
  (compare-repl-result
   :repl-16
   (low-level-repl (make-test-repl "(print -)"))
   :ism nil :isu "(print -)" :if '(print -) :pet nil :cs nil 
   :cp nil :ep (one-string (string #\Newline) "(PRINT -) ")
   :ov (list '(print -)) :ovs (list "(PRINT -)") 
   :ec nil :re nil :ce nil :ee nil :pe nil)
  ;; timeout during compilation
  #+:allegro
  (compare-repl-result
   :repl-17
   (low-level-repl 
    (make-test-repl 
     "(macrolet ((frob () (sleep 3))) (frob))"
     :cm :most))
   :ism nil 
   :pet nil :cs nil :cp nil :ep nil
   :ov nil :ovs nil :ec nil :re nil :ce t :ee nil :pe nil)

  nil
  )

