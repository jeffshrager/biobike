;;; -*- mode: Lisp; Syntax: Common-Lisp; Package: WEBLISTENER; -*-

(in-package :weblistener)

;;; +=========================================================================+
;;; | Copyright (c) 2006, JP Massar, Jeff Shrager                             |
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

;;; What do we do about the session log since the VPL session has the same 
;;; session ID as the user's session it was spawned from?
;;; For now we're just going to write to the same log.
;;; We should be able to bind the log variable to a different log,
;;; and then things will 'just work', once we figure out what we want to
;;; write to the log.

;;; Per-session inits should set up the log file.

(defmacro with-internal-vpl-repl-errors-caught ((&rest options) &body body)
  (declare (ignore options))
  `(progn ,@body))
     
(defun new-vpl-high-level-repl (input-snippet form output-snippet)

  (nvpl::vdbg "In VPL high level REPL...~%")

  (with-internal-vpl-repl-errors-caught ()

    ;; make sure VPL user is in BBL mode
    (let ((previously-in-bbl-mode? (bbl-mode?)))
      (handler-case 
          (bbl-mode :verbose? nil)
        (error 
         ()
         (nvpl::vpl-internal-error 
          "Internal error: Can't get into BBL mode!  Please report this!")))

      (unwind-protect 

          ;; Set up the user's environment and call the LOW-LEVEL-REPL code
          
          (let ((repl (make-default-repl))
                (form-string (format nil "~S" form)))

            (with-process-information-bound (form-string)

              (setq *current-repl* repl)
              (setf (repl-input-string repl) nil)
              (setf (repl-input-form repl) form)
              (setf (repl-evaluation-timeout-threshold repl)
                    *execution-timelimit*)
              (setf (repl-parentheses-mode-enabled? repl) nil)
              (setf (repl-completion-enabled? repl) nil)
              (setf (repl-completion-limit repl) nil)
              (setf (repl-compilation-mode repl) *compilation-mode*)
          
              (vpl-low-level-repl repl)

              ;; Record fact that user entered something.
      
              (setf (get *sessionid* :last-execution-time)
                    (get-universal-time))
      
              (new-vpl-handle-repl-return input-snippet repl output-snippet)

              ))
      
        (when (not previously-in-bbl-mode?) 
          (handler-case (biolisp-mode)
            (error 
             ()
             (nvpl::vpl-internal-error 
              "Internal error: Can't get back into Biolisp mode!"
              )))))
      
      )))
        

(defun vpl-low-level-repl (repl)

  (nvpl::vdbg "In VPL low level REPL...~%")

  (block exit

    (flet ((handle-condition (condition)
             (typecase condition
               ;; standard evaulation error
               (evaluation-failed 
                (setf (repl-eval-error? repl) t)
                (let ((actual-condition (evaluation-failed-reason condition)))
                  (return-from exit 
                    (dwimifiable-error-handler 
                     repl condition actual-condition)))
                )
               ;; All the possible other ways the REPL can fail...
               ((or input-form-verification-failed timelimit-exceeded)
                (setf (repl-eval-error? repl) t))
               ((or compilation-failed compilation-timelimit-exceeded)
                (setf (repl-compilation-error? repl) t))
               (print-failed
                (setf (repl-print-error? repl) t))
               (error
                (setf (repl-eval-error? repl) t))
               (otherwise (signal condition))
               )
             (setf (repl-error-condition repl) condition)
             (return-from exit repl)
             ))

      (setf (repl-start-repl-time repl) (get-universal-time)) 
               
      (let ((function-object nil)
            (*current-repl* repl)
            (input-form (repl-input-form repl))
            )

        (handler-case
            (progn
              (multiple-value-setq (input-form function-object)
                  (repl-maybe-compile-form repl input-form))
              (setf (repl-end-compilation-time repl) (get-universal-time))
              (repl-evaluate-form-and-store-results
               repl function-object input-form)
              (setf (repl-end-execution-time repl) (get-universal-time))
              repl)
          ;; Make sure warnings don't get handled.
          (error (c) (handle-condition c))
          )  

        ))))

(defun new-vpl-handle-repl-return (snippet repl output-snippet)

  (declare (ignore snippet))

  (nvpl::vdbg "In VPL handle REPL return...~% ")
  
  (let* ((input-form (repl-input-form repl))
         (error-condition (repl-error-condition repl))
         (output-values (repl-output-values repl))
         (output-strings (repl-output-value-strings repl))
         (compilation-printout (repl-compilation-printout repl))
         (evaluation-printout (repl-evaluation-printout repl))
         )
    
    (nvpl::vdbg "ep: ~S~%" evaluation-printout)

    ;; There are two possibilities.
    ;; First, a successful evaluation took place, possibly producing output
    ;;   as well as return values.
    ;; Second, an error occurred at some point in the process.  Depending
    ;;   on the type of error output it may or may not be possible
    ;;   for output to have been produced.

    (cond

     ;; Successful evaluation.
     ;; Store the user input into the completion records for posterity.
     ;; Adjust the +, /, * and friends variables.
     ;; Store the printout and results into the history display mechanism.
     ;; Log the output.
     ;; Return an appropriate something...

     ((not error-condition)

      (setq +++ ++) (setq ++ +) (setq + input-form)
      (setq /// //) (setq // /) (setq / output-values)
      (setq *** **) (setq ** *) (setq * (first output-values))

      ;; Purge compiler warnings about free references to undeclared variables
      ;; Only do this for the interactive REPL

      (when (member (repl-compilation-mode repl) '(:most t))
        (hack-compilation-warnings repl)
        (setf compilation-printout (repl-compilation-printout repl)))

      (let* ((printout 
              (one-string 
               (or compilation-printout "")
               (or evaluation-printout "")
               )))

        ;; Create a new VPL history record from the input and all the 
        ;; output crud

        (add-vpl-execution-info 
         output-snippet :success printout output-values output-strings nil)

        ))

     ;; An error occurred.
     ;; What we do depends on the type of error.

     (t

      ;; Purge compiler warnings about free references to undeclared variables
      ;; Only do this for the interactive REPL

      (when (member (repl-compilation-mode repl) '(:most t))
        (hack-compilation-warnings repl)
        (setf compilation-printout (repl-compilation-printout repl)))

      (let ((error-string
             (formatn "~A" error-condition)
             #+wrong-level
             (vpl-error-to-string error-condition)
             ))
        
        (cond
       
         ;; Compilation error.  There is an input form, but no output,
         ;; nor any evaluation printout (there might be compiler printout).
         
         ((repl-compilation-error? repl)
          (setq +++ ++) (setq ++ +) (setq + input-form)
          (add-vpl-execution-info  
           output-snippet :compilation-error  
           (or compilation-printout "") nil nil error-condition))

         ;; Evaluation error.  There is an input form, but no output.
         ;; There may be both compilation and evaluation printout.

         ((repl-eval-error? repl)
          (setq +++ ++) (setq ++ +) (setq + input-form)
          (add-vpl-execution-info
           output-snippet :evaluation-error 
           (one-string 
            (or compilation-printout "")
            (or evaluation-printout ""))
           nil nil error-condition
           ))

         ;; Printing error.  There's an input form, possibly compilation
         ;; and evaluation printout, and output forms.  Unfortunately,
         ;; we can't get a string representation!

         ((repl-print-error? repl)
          (setq +++ ++) (setq ++ +) (setq + input-form)
          (setq /// //) (setq // /) (setq / output-values)
          (setq *** **) (setq ** *) (setq * (first output-values))
          (add-vpl-execution-info
           output-snippet :printer-error 
           (one-string 
            (or compilation-printout "")
            (or evaluation-printout ""))
           nil nil error-condition
           ))
         
         (t
          (nvpl::vpl-internal-error 
           (formatn
            "Error condition ~A noted, but error flag not set!" error-string
            )))

         ))))

    ;; If compilation and/or execution took a measurable amount of time
    ;; log a timestamp to this effect.  

    (log-long-duration-forms repl)

    output-snippet
        
    ))


(defun add-vpl-execution-info 
       (snippet result-type printout values value-strings error-condition)
  (nvpl::set-snippet-property snippet :result-type result-type)
  (nvpl::set-snippet-property snippet :printout printout)
  (nvpl::set-snippet-property snippet :values values)
  (nvpl::set-snippet-property snippet :nvalues (length values))
  (nvpl::set-snippet-property snippet :value-strings value-strings)
  (nvpl::set-snippet-property snippet :error-condition error-condition)
  (nvpl::set-snippet-property snippet :timestamp (get-universal-time))
  (nvpl::set-snippet-property 
   snippet 
   :crude-size 
   (reduce 
    '+
    (nconc
     (mapcar (lambda (x) (memory-object-crude-size x nil)) values)
     (mapcar (lambda (x) (memory-object-crude-size x nil)) value-strings)
     ))))

  