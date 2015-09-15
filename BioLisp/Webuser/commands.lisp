;;; -*- mode: Lisp; Syntax: Common-Lisp; Package: wb; -*-

(in-package :wb)

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

(defmacro runjob ((&key (output-file "runjob.output")
                        (timelimit (* 30 60))
                        (priority -100)
                        (quantum 0.1)
                        (error-flag :default)
                        (done-flag :default)
                        (output-file-symbol :default)
                        (process-name nil)
                        (from-weblistener? t)
                        (sleep-for nil)
                        (verbose? t))
                  &body body)
  #.(one-string-nl
     "Executes BODY in a separate thread.  Normal output from BODY "
     "(i.e., written to *standard-output* or *standard-error*)"
     "is diverted to OUTPUT-FILE.  The thread runs for a maximum of"
     "TIMELIMIT seconds before exiting."
     ""
     "RUNJOB returns the process data structure."
     "If the thread gets an error, it is aborted, and the symbol"
     "*RUNJOB-ERROR* (or the symbol which is the value of ERROR-FLAG)"
     "is set to T (initially it is set to NIL)."
     "If the thread completes, the symbol *RUNJOB-DONE* (or the symbol which"
     "is the value of DONE-FLAG) is set to T (initially it is set to NIL)."
     "The symbol *RUNJOB-OUTPUT-FILE* (or the symbol which is the value of"
     "OUTPUT-FILE-SYMBOL) is set to the full pathname of the output file"
     "before execution of the background thread starts."
     ""
     "By default, RUNJOB assumes it is executing from the WebListener."
     "If not, the caller must provide :FROM-WEBLISTENER? NIL."
     "If the user calls RUNJOB from the weblistener, he/she"
     "should not continue to use that weblistener to execute code."
     "This is because when the RUNJOB returns, all the output"
     "from subsequent commands that the user executes will disappear."
     "The user should log in to another session and execute from there."
     ""
     "RUNJOB sets the processes' priority and quantum to PRIORITY"
     "and QUANTUM.  Normally, a process has priority 0 and quantum 1.0,"
     "but RUNJOB by default sets the priority to -100 (very low priority)"
     "and 0.1 (interruptable every tenth of a second)."
     "If VERBOSE? is T (the default), information about the thread is"
     "printed out before it begins execution."
     "Use MP:PROCESS-KILL on this object to terminate the thread."
     "Use (LIST-FILE-CONTENTS *RUNJOB-OUTPUT-FILE*) to check on the"
     "status of the thread (assuming you cause it to do any output!)"
     )
  (declare (ignorable priority quantum))
  (let ((fsym (gensym "BACKGROUND-FUNCTION-"))
        (tlsym (gensym "TIMELIMIT-"))
        (ofsym (gensym "OUTPUT-FILE-"))
        (pnsym (gensym "PROCESS-NAME-"))
        (vbsym (gensym "VERBOSE-"))
        (efsym (gensym "ERROR-FLAG-SYMBOL-NAME-"))
        (dfsym (gensym "DONE-FLAG-SYMBOL-NAME-"))
        (ofssym (gensym "OUTPUT-FILE-SYMBOL-"))
        (streamsym (gensym "OUTPUT-FILE-STREAM-")))
    (unless (symbolp error-flag)
      (error "RUNJOB: Bad ERROR-FLAG arg (not a symbol): ~A" error-flag))
    (unless (symbolp done-flag)
      (error "RUNJOB: Bad DONE-FLAG arg (not a symbol): ~A" done-flag))
    `(let ((,tlsym ,timelimit) 
           (,ofsym ,output-file)
           (,pnsym ,process-name)
           (,vbsym ,verbose?)
           (,efsym 
            ,(if (eq error-flag :default) 
                 `(intern "*RUNJOB-ERROR*" *package*) `',error-flag))
           (,dfsym
            ,(if (eq done-flag :default) 
                 `(intern "*RUNJOB-DONE*" *package*) `',done-flag))
           (,ofssym
            ,(if (eq output-file-symbol :default)
                 `(intern "*RUNJOB-OUTPUT-FILE*" *package*) 
               `',output-file-symbol))
           )
       (unless (and (integerp ,tlsym) (plusp ,tlsym))
         (error "RUNJOB: Invalid TIMELIMIT: ~A" ,tlsym))
       (when (> ,tlsym (* 60 60 2))
         (error
          (one-string-nl
           "You are apparently trying to run a very long batch job."
           "(longer than two hours)."
           "Please contact the system adminstrators or support staff"
           "for other ways of doing this, rather than running from within"
           "the Weblistener.")))
       (unless (or (stringp ,ofsym) (pathnamep ,ofsym))
         (error "RUNJOB: Invalid required OUTPUT-FILE arg ~A" ,ofsym))
       (unless ,pnsym (setq ,pnsym (string (gensym "RUNJOB-"))))
       (setq ,ofsym (merge-pathnames ,ofsym))
       ,@(when error-flag `((set ,efsym nil)))
       ,@(when done-flag `((set ,dfsym nil)))
       ,@(when output-file-symbol `((set ,ofssym ,ofsym)))
       (when ,vbsym
         (terpri)
         (cformatt "RUNJOB output file: ~A" ,ofsym)
         (cformatt "RUNJOB time limit: ~A" ,tlsym)
         (cformatt "RUNJOB priority: ~A" ,priority)
         (cformatt "RUNJOB quantum: ~A" ,quantum)
         (cformatt "RUNJOB process name: ~A" , pnsym)
         (cformatt "RUNJOB error flag variable: ~A" ,efsym)
         (cformatt "RUNJOB done flag variable: ~A" ,dfsym)
         (cformatt "RUNJOB output file symbol: ~A" ,ofssym))
       (flet ((,fsym (username)
                (declare (ignorable username))
                (,(if from-weblistener? 
                      'with-protected-globals-bound 
                    'progn)
                 ,@(when from-weblistener? '(username))
                 (unwind-protect
                     (handler-case 
                         (progn
                           ,@(when sleep-for `((sleep ,sleep-for)))
                           (with-open-file 
                               (,streamsym 
                                ,ofsym 
                                :direction :output :if-exists :supersede)
                             (let ((*standard-output* ,streamsym)
                                   (*trace-output* ,streamsym)
                                   (*error-output* ,streamsym))
                               (with-timeout-limit
                                   (,tlsym (cformatt "*** TIMEOUT ***"))
                                 (handler-case
                                     (progn ,@body)
                                   (error 
                                    (c)
                                    ,@(when error-flag `((set ,efsym c)))
                                    (cformatt
                                     "*** Error in RUNJOB body: ~A" c))
                                   )))))
                       (error 
                        (c)
                        ,@(when error-flag `((set ,efsym c)))
                        ))
                   ,@(when done-flag `((set ,dfsym t)))
                   ))))
         (let ((process
                (run-function-as-process
                 ,pnsym #',fsym 
                 ,(when from-weblistener? (user-session-id-symbol)))))
           #+:Allegro
           ;; bbl code walker cannot handle these setf's 
           (,(if (wb::bbl-mode?) (intern "WITHOUT-CODE-WALKER" :bbi) 'progn)
            (setf (mp:process-priority process) ,priority)
            (setf (mp:process-quantum process) ,quantum))
           process
           )))))


;;; DEF-PROTOCOL

(defun hop (n &optional (value-index 1))
  #.(one-string-nl
     "Returns the nth result.  If the nth result consists of more than"
     "one value VALUE-INDEX (default the 1st value) specifies which value"
     "is returned."
     "CLEAR-HISTORY destroys HOP's ability to retrieve these forms.")
  (values (new-history-output-datum n :value value-index)))

(defun result (n &optional (value-index 1))
  #.(one-string-nl
     "Returns the nth result.  If the nth result consists of more than"
     "one value VALUE-INDEX (default the 1st value) specifies which value"
     "is returned."
     "CLEAR-HISTORY destroys HOP's ability to retrieve these forms.")
  (if *vpl-evaluating?* 
      (forward-package-funcall :wb :vpl-result n value-index)
    (hop n value-index)))

(defmacro redo (symbol &rest args)
  #.(one-string-nl
     "Re-evaluate the latest expression beginning with the indicated SYMBOL."
     "For example: (redo load).  (You can't redo non-list expressions.)"
     "If SYMBOL is an integer, redo the indicated history input expression."
     "You can give two step numbers and all the expressions from the first to"
     "the second inclusive will be reevaluated, as: (redo 34 39).")
  (cond 
   ((null args)
    (cond
     ((symbolp symbol) `(redo-history-symbol ',symbol))
     ((integerp symbol) `(redo-history-step ,symbol))
     (t (error "<< *** REDO argument must be a symbol or integer. *** >>"))
     )
    )
   (t 
    (unless (eql 1 (length args)) (error "Unknown REDO syntax"))
    `(redon ,symbol ,(first args))
    )))

(defun redon (from-step to-step)
  (loop for step from from-step to to-step
	as result = (redo-history-step step)
	finally (return result)))

(defun redo-history-symbol (symbol)
  (loop for in-record in *in-history* 
        as string = (inhist-string in-record)
        as form = (inhist-form in-record)
        do
        (when (and string (listp form) (eq symbol (first form)))
          (formatt "Re-evaluating: ~s~%" form)
          (return (redo-history-string string)))
        finally
        (error "<< *** No form starting with '~A' found. *** >>" symbol)
        ))

(defun redo-history-step (n)
  (let ((record (nth-input-history-record n)))
    (if (null record) 
        (cformatt "Could not find history input for index ~D" n) 
      (progn
        (cformatt "Re-evaluating: ~s~%" (inhist-form record))
        (redo-history-string (inhist-string record))
        ))))

(defun redo-history-string (string)
  (let ((repl (make-default-repl)))
    (copy-repl-inputs repl *current-repl*)
    (setf (repl-input-string repl) string)
    (low-level-repl repl)
    (when (repl-error-condition repl)
      (error
       (formatn
        (one-string-nl
         "Redo failed attempting to execute"
         "~A"
         "Actual problem:"
         "~A"
         (condition-to-string (repl-error-condition repl))
         ))))
    (apply 'values (repl-output-values repl))
    ))


;;; Each substitution form looks like (<varname> &rest (<linenum> <subform>))

;;; Def-protocol binds this dynamic variable to the forms in the
;;; input history list, but in reverse order for ease of access.
;;; In doing substitutions we thus replace the forms on this list, not
;;; the 'real' forms in the wb::*in-history* variable.

(defvar *in-history-forms* nil)

(defvar *collect-step-variables?* nil)
(defvar *step-variables-needed* nil)

(defun verify-linenum (n f)
  (unless (and (integerp n) (plusp n))
    (error "~A argument must be literal positive integer: ~A" f n))
  (when *in-history-forms*
    (unless (<= n (length *in-history-forms*))
      (error "~A refers to a line, ~D, that does not currently exist" f n)))
    n)

;;; Do a simple SUBST for each substitution.  Theoretically, this could
;;; be an arbitrarily complicated code walk / binding analysis.
;;; We get the form to be substituted into from the history list.
;;; The form we want is indexed by LINENUM.  We then substitute VAR
;;; for FORM-TO-BE-SUBSTITUTED-FOR everywhere in the form (even in
;;; quoted subforms, which is obviously wrong).

(defun create-substituted-form (var linenum form-to-be-substituted-for)
  (verify-linenum linenum 'parse-substitutions)
  (let* ((form-index (1- linenum))
         (form (nth form-index *in-history-forms*)))
    (setf (nth form-index *in-history-forms*)
          (subst var form-to-be-substituted-for form :test #'equal))))


;;; Check each substitution directive and create the substituted-into
;;; forms as directed.

(defun parse-substitutions (substitutions)
  (let ((parameter-names nil))
    (dolist (sub substitutions)
      (unless (listp sub) (error "Invalid substitution form: ~A" sub))
      (let ((varname (car sub))
            (sub-directives (cdr sub)))
        (unless (symbolp varname)
          (error "Invalid substitution parameter name: ~A" varname))
        (pushnew varname parameter-names)
        (loop for sub-directive in sub-directives do
              (unless (and (listp sub-directive)
                           (eql 2 (length sub-directive))
                           (integerp (first sub-directive)))
                (error "Invalid substitution directive: ~A" sub-directive))
              (create-substituted-form 
               varname (first sub-directive) (second sub-directive)))))
    (reverse parameter-names)))


(defun next-step-is-sequentially-next (linenums)
  (and (rest linenums) (= (first linenums) (1- (second linenums)))))

;;; The idea here is that if we have, e.g., (STEPS 1 3 4 5 10)
;;; that a reference to * in step 4 (that is, the result of step 3) be valid
;;; (and likewise with step 5) but that a reference to * in step 1, step 3
;;; or step 10 not be valid because the predecessor form is not being
;;; executed by this protocol.  When * is not valid we set it to
;;; :ILLEGAL-HISTORY-REFERENCE.  We could make it unbound, but that might
;;; be more confusing than having it bound to something that gives a clue
;;; about what the problem might be.

(defmacro steps (&rest linenums)
  (block exit
    (when (null linenums) (return-from exit nil))
    (loop for num in linenums do (verify-linenum num 'steps))
    (let ((generate-*-setqs 
           (maplist #'next-step-is-sequentially-next linenums)))
      `(progn
         (setq * :illegal-history-reference)
         ,@(loop for num in linenums
                 for gen* in generate-*-setqs 
                 as index = (1- num) append
                 (let ((form `(setq ,(step-variable num) 
                                    ,(nth index *in-history-forms*))))
                   (if gen* 
                       `(,form (setq * ,(step-variable num)))
                     `(,form (setq * :illegal-history-reference)))))
         ,(step-variable (first (last linenums)))))))

(defun step-variable (n) 
  (let ((var (intern (format nil "S~D-RESULT" n) (find-package :$$))))
    (when *collect-step-variables?* (pushnew var *step-variables-needed*))
    var))

(defun steps-expand (form)
  (if (listp form)
      (cond 
       ((eq (first form) 'steps) (macroexpand-1 form))
       ((eq (first form) 'step) (macroexpand-1 `(steps ,@(cdr form))))
       (t form))
    form))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun load-user-file (name &key (userid *username*))
  #.(one-string-nl
     "Load a lisp file from your, or another user's Weblistener home directory."
     "Example: (load-user-file \"myfile.lisp\" :userid \"jelhai\")"
     "(The default USERID is your own login/user id.")
  (let ((file (formatn "~a~a" (wb::visitor-directory userid) name)))
    (cond ((probe-file file)
	   (load file))
	  (t (error 
	      "The file called \"~a\" was not found in ~a's directory. "
              name userid)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *user-vars-not-to-print*
  '(*random-state* *features* *modules* *print-pprint-dispatch*
                   *in-history* *out-history*))

(defun show-per-user-variables 
       (user &key sessionid (which :all) (all? nil) &aux pkg varlist f)

  #.(one-string-nl
     "Pretty print a user's state variables and their values."
     "A specific sessionid may be specified using SESSIONID (if enabled)."
     "If ALL? is T (default NIL) then every state variable is shown, including"
     "some which print out very verbosely."
     "If WHICH is :WEBLISTENER (or :WB), then only variables pertaining"
     "to the user's weblistener state (not Common Lisp variables) are shown."
     "If WHICH is :LISP, only Common Lisp state variables are shown."
     "(The specification of ALL? as T supersedes WHICH).")

  (when (not (find-package (keywordize user)))
    (cformatt "There is no active user named ~A" user)
    (return-from show-per-user-variables nil))

  ;; Figure out the appropriate symbol to key off of.
  ;; If a random user is specified, and no sessionid, use that user's
  ;; first sessionid.
  (if sessionid 
      (setq pkg (keywordize sessionid))
    (if (eq *username* (keywordize user))
        (setq pkg *sessionid*)
      (setq pkg (first (gethash (keywordize user) *user->sessionids-ht*)))
      ))
  (when (null pkg)
    (cformatt "User ~A has no sessions. He is presumably logged off." user)
    (return-from show-per-user-variables nil))

  ;; Figure out which variables (and their values) should be shown.
  (if all?
      (setq varlist *weblistener-protected-global-vars*)
    (setq varlist
          (set-difference
           (ecase which
             (:all *weblistener-protected-global-vars*)
             ((:wb :wl :web :weblistener) *weblistener-state-variables*)
             ((:lisp :common-lisp)
              (set-difference 
               *weblistener-protected-global-vars*
               *weblistener-state-variables*
               )))
           *user-vars-not-to-print*
           )))

  ;; Put them in alphabetical order.
  (setq varlist (sort (copy-list varlist) 'string-lessp :key 'string))

  ;; Determine how we get each variable's value
  (let ((f1 (lambda (v) (saved-variable-value pkg v)))
        (f2 (lambda (v) (if (boundp v) (symbol-value v) :<unbound>))))
    (setq f (if (not (eq *sessionid* pkg)) f1 f2))
    )
  ;; Do it!
  (terpri)
  (cformatt "Variable name = <value>")
  (terpri)
  (loop for var in varlist 
        as  value = (funcall f var)
        do (cformatt "~A = ~S" var value))
  (terpri)

  )


(defun my-sessions () (sessions))

(defun sessions (&key (user *username*))
  (setq user (keywordize user))
  (let ((sessioninfo (user-session-info user))
        (first t))
    (terpri)
    (cformatt "Session info for user ~A" user)
    (terpri)
    (loop
     for (nil info) in sessioninfo
     as session-number = (getf info :session-number)
     as etime = (getf info :last-execution-time)
     as login-time = (getf info :session-creation-time)
     as log-file = (getf info :log-file)
     as last-input = (getf info :last-input)
     do 
     (when first 
       (setq first nil)
       (formatt
        "Session ID  Last Activity   Session Login Time      Log File                Last Form Evaluated")
       (terpri)
       (terpri))
     (formatt "~6D     ~16a ~15a    ~27a  ~s~%"
              session-number 
              (if etime 
                  (make-timestamp-string 
                   :universal-time etime :mode :mmddyyhhmm)
                "unknown")
              (if login-time 
                  (make-timestamp-string
                   :universal-time login-time :mode :mmddyyhhmm)
                "unknown")
              (if log-file (file-namestring log-file) "none")
              (if last-input (limited-string last-input 30) "-")
              ))
    (when first (cformatt "No session information for user ~A found." user))
    (terpri)
    ))

(add-weblistener-keyword-command 
 :sessions
 (lambda (s) (declare (ignore s)) `(sessions))
 "Lists information about the current user's sessions.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *vpl-initialized?* nil)

(defun init-vpl ()
  (when (not *vpl-initialized?*) 
    (load (translate-simple-lp "biol:initialize-ajax.lisp"))
    (setq *vpl-initialized?* t)))

(defun vpl () 
  (init-vpl)
  ;; start a new session for the requested vpl instance.  
  ;; if the user never actually invokes the vpl, the session
  ;; will still exist but be pretty much inaccessible. 
  (let ((*standard-output* *system-standard-output*)
        (*error-output* *system-error-output*)
        (*trace-output* *system-trace-output*))
    (let ((new-session-id (connect-to-new-session wb::*username* nil nil)))
      (make-url :path (make-vpl-start-url :pkg new-session-id) 
                :display-string "Invoke VPL window."
                :target "_blank"
                ))))



