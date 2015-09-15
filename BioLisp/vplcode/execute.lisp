;;; -*- mode: Lisp; Syntax: Common-Lisp; Package: nvpl; -*-

(in-package :nvpl)

;;; +=========================================================================+
;;; | Copyright (c) 2006 John Myers, JP Massar                                |
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

;; Author: JP Massar.


;;;;;;;;;;;;;; EVAL FUNCTION ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Switch in the order of EXECUTION ERROR and EXECUTION PRINTOUT

(defun handle-eval-request (boxid &aux output-snippet code)
  (vdbg "In handle-eval-request...~%")
  (when *use-separate-execution-thread?*
    (unless *in-execution-process?*
      (let ((c (make-condition 'vpl-execution-condition :data wb::*sessionid*)))
        (signal c)
        )))
  
  (when (stringp boxid) (setq boxid (idstring->id boxid :snippet-id)))

  ;; Do the evaluation of the form represented by boxid.  
  ;; If the form returns at all, either the box clicked on is not meant 
  ;; to be executed (in which case, NIL is returned), or
  ;; the form represented by the box was indeed executed.  
  ;; (The execution itself may or may not have been successful.)  
  
  (multiple-value-setq (output-snippet code) (handle-eval-box-id boxid))
  (when (null output-snippet)
    (return-from handle-eval-request nil))
  (set-snippet-property output-snippet :eval-boxid boxid)
  (set-snippet-property output-snippet :eval-code code)
  (unless output-snippet (return-from handle-eval-request nil))
  
  ;; force the system to redisplay the value of all variables at toplevel
  ;; in the workspace.  
  (push '(redisplay-toplevel-box-variable-values) *more-client-commands*)
  
  (let ((result-type (get-snippet-property output-snippet :result-type))
        (input-form (get-snippet-property output-snippet :input-form)))
    (vdbg "Creating output box...~%")
    (vdbg "Result type: ~S~%" result-type)
    
    (case result-type
      
      ;; If the execution was successful, create OUTPUT-VALUE snippets 
      ;; for each value returned, and attach them as children to the 
      ;; toplevel output node
      (:success 
       (let ((nvalues (get-snippet-property output-snippet :nvalues))
             (values (get-snippet-property output-snippet :values)))
         (if (= 1 nvalues)
             (process-single-vpl-return-value (first values))
           (mapcar 'process-single-vpl-return-value values)
           ))
       (let ((value-strings
              (get-snippet-property output-snippet :value-strings)))
         (setf (snippet-children output-snippet) 
               (mapcar 
                (lambda (value-string) 
                  (create-output-value-snippet
                   output-snippet 
                   (wb::string-without-frame-prefixes value-string)))
                value-strings
                ))
         (if (= (length value-strings) 1) 
             (ulog "Value: ~A~%" (limited-string (first value-strings)))
           (loop for v in value-strings for j from 1 do
                 (ulog "Value ~D: ~A~%" j (limited-string v))))
         ;; show any printout that the execution of the form did
         ;; in a separate popup window
         (maybe-show-printout output-snippet)
         ;; display a popup window warning if the user generated 
         ;; a large data structure for the first time
         (maybe-show-size-warning output-snippet)
         ;; if the form was a definition, add the symbol defined 
         ;; to the user's list of his variables or functions if applicable
         (when (bbl-defining-form? input-form)
           (add-definition-for-user input-form))
         ))
      
      ;; the execution resulted in an error being signalled...
      (otherwise 
       (vdbg "Showing error condition~%")
       (let* ((econdition
               (get-snippet-property output-snippet :error-condition))
              (emessage 
               (vpl-error-to-string econdition)
               )
              (printout (get-snippet-property output-snippet :printout))
              (error-message-key "*** An error occurred..."))
         (declare (ignorable error-message-key))
         (ulog "Error message: ~A~%" emessage)
         ;; Pop up a window containing the error message
         (create-and-use-unique-file 
          (user-temp-vpl-dir)
          (lambda (file p) 
            (declare (ignore file))
            ;; Change in order of printout. Fixed original bug. I hope! -- JP
            (format 
             p "~A~A~%"
             (if (zerop (length printout)) 
                 (formatn "Execution error:~%~%~A~%" emessage)
               (formatn 
                "Execution error: ~%~%~A~%~%Execution printout:~%~%"
                emessage))
             printout))
          (lambda (file) 
            (show-vpl-popup-URL-window 
             (user-temp-vpl-dir-url file)
             :relative-p 0
             ))
          :name (s+ "error-" (string wb::*sessionid*))
          :type "txt"
          )
         ;; create a single OUTPUT-VALUE node which contains the error.  
         ;; attach it as the lone child to the toplevel node.
         (setf (snippet-children output-snippet)
               (list (create-output-value-snippet 
                      output-snippet 
                      (condition-symbol-to-display econdition)
                      ))))))

    ;; make this result part of the display and the history.  
    (add-to-results-toplevel output-snippet)
    (if (get wb::*sessionid* :vpl-results-order-reversed?)
        (redraw-results)
      (progn
        (add-results (toplevel-snippet->client-representation output-snippet))
        ;; make sure the focus is always on the output snippet 
        ;; that was most recently created.  
        (focus-box (snippet-id output-snippet))
        ))
    (setq *current-selected-boxid* nil)
    (unflashing-hilight-box boxid)
    ))

(defun condition-symbol-to-display (condition)
  (typecase condition
    (wb::evaluation-failed (type-of (wb::evaluation-failed-reason condition)))
    (otherwise (type-of condition))
    ))


(defun handle-eval-box-id (boxid &aux snippet code)

  #.(one-string-nl
     "this function looks up the given box, converts and snippet the box"
     "represents to code and evals it.")

  (block exit
  
    (vdbg "in handle-eval-box-id~%")

    ;; get the code snippet
    
    (setq snippet (find-snippet-in-workspace boxid))
    
    ;; aggregate nodes are not shown with box outlines.
    ;; the user might double click inside the invisible box meaning to execute
    ;; the parent node.  so we anticipate this and assume that's what he wants.
    (typecase snippet
      (aggregate-snippet 
       (if (snippet-parent snippet) 
           (setq snippet (snippet-parent snippet))
         (vpl-internal-error 
          (one-string-nl 
           "attempting to evaluate toplevel :aggregate node!"
           ))))
      ((or choice-snippet keyword-snippet flag-snippet 
           literal-snippet progn-snippet)
       (show-status "not an executable node!")
       (return-from exit nil))
      (otherwise nil)
      )

    ;; transform the snippet into code

    (vdbg "translating snippet to code~%")

    (setq code
          (handler-case
              (snippet-to-code snippet)
            (hole-error
             ()
             (ulog "hole found evaluating snippet id  ~d~%" boxid)
             (vpl-user-error
              (formatn
               (one-string-nl
                "You are trying to evaluate code from a box labeled ~A"
                "that is not completed!"
              
                "Possible causes:"
                ""
                "1. The most likely cause is that an entry box has not been"
                "closed. A function cannot be executed that contains an open"
                "entry box. If you see a white (open) box in the code"
                "you're trying to execute, then click in the box and tap"
                "either the enter or tab keys. The box should turn gray."
                "then try re-executing the function."
                ""
                "2. Alternatively, you may have neglected to put a value"
                "in a box that requires one. If you see a box with a red"
                "word in italics, click on that box and provide a value."
                )
               (string-upcase (snippet-label snippet))
               )))
            (vpl-user-error (c) (signal c))
            (error
             ()
             (vpl-internal-error 
              (formatn 
               (one-string-nl
                "in handle-eval-box-id.  snippet->code failing!"
                "snippet: ~s")
               snippet
               )))))

    (vdbg "evaluating snippet form~%")
    (ulogdbg "form: ~s~%" code)
    
    (let ((wb::*vpl-evaluating?* t)
          (hacked-code (bbi::hack-code-for-bbl-toplevel code)))
      (incf *vpl-execution-history-index*)
      (let ((output-snippet 
             (create-toplevel-output-snippet *vpl-execution-history-index*)
             ))
        (start-execution-message)
        (wb::new-vpl-high-level-repl snippet hacked-code output-snippet)
        (end-execution-message)
        ;; link the output node to the node that created it via being executed
        (set-snippet-property output-snippet :input-form code)
        ;; link the executing node to the output node(s) it produces
        (set-snippet-property  
         snippet
         :output-nodes
         (cons 
          (snippet-id output-snippet)
          (get-snippet-property snippet :output-nodes))
         )
        (values output-snippet code)
        ))))

(defun bbl-defining-form? (form)
  (and (listp form) 
       (or (eq (first form) 'bbl::define-function)
           (eq (first form) 'bbl::define)
           (eq (first form) 'wlisp::defun)
           nil
           )))

(defun add-definition-for-user (def)
  (let ((defname (first def)))
    (vdbg "adding definition to palette.~%")
    (case defname
      (bbl::define-function 
          (let ((symbol (second def)))
            (vdbg
             #.(one-string-nl
                "adding definition for ~a in package ~a"
                "onto my-functions palette.~%")
             symbol (symbol-package symbol))
            ;; get rid of aliases 
            (when (listp symbol) (setq symbol (first symbol)))
            ;; this creates a new template id only if one has not yet
            ;; been assigned to the symbol.
            (add-define-function-symbol-for-user symbol)
            (show-status 
             (formatn "~A added to Functions menu." (string symbol)))
            ))
      (wlisp::defun 
             (let ((symbol (second def)))
               (vdbg
                "adding definition for ~a as defun onto my-functions palette.~%"
                symbol)
               (eval (defun-symbol->define-template-form symbol))
               (augment-my-functions-menu 
                (string symbol)
                (symbol->template-id symbol))
               (show-status 
                (formatn "~A added to Functions menu." (string symbol)))
                ))
      (bbl::define
        (let* ((symbol (second def)))
          (when (listp symbol) 
            ;; then it should be a ref 
            (unless (eq (first symbol) 'ref)
              (vpl-internal-error "define symbol is list but not ref!"))
            (setq symbol (second symbol)))
          (vdbg "adding ~s onto ~s's variables hash~%" symbol wb::*username*)
          (add-define-symbol-for-user symbol)
          (show-status 
           (formatn "~A added to Variables menu." (string symbol))
           )))
      (otherwise 
       (vpl-internal-error "not yet dealt with.")
       ))))

(defun add-define-symbol-for-user (symbol)
  (let ((id (get-id-and-add-user-data-to-hash symbol)))
    (vdbg "adding definition for ~a onto my-variables palette.~%" 
          symbol)
    (augment-my-variables-menu (string-downcase symbol) id)
    ))

(defun add-define-function-symbol-for-user (symbol)
  (eval (df-symbol->define-template-form symbol))
  (augment-my-functions-menu
   (string symbol)
   (symbol->template-id symbol)
   ))

(defun maybe-show-printout (output-snippet)
  (let ((printout (get-snippet-property output-snippet :printout)))
    (when (> (length printout) 0)
      (create-and-use-unique-file 
       (user-temp-vpl-dir)
       (lambda (file p)
         (vdbg "writing printout to ~a~%" (namestring file))
         (with-html-to-stream p 
           (:html 
            (:title "vpl execution printout")
            (:pre 
             (wb::princ-with-frame-links 
              printout :pkg wb:*sessionid* :remove-frame-prefix? t)
             ))))
       (lambda (file) 
         (show-vpl-popup-url-window (user-temp-vpl-dir-url file)
                                    :menubar "yes" :relative-p 0))
       :name (s+ "printout-" (string wb::*sessionid*))
       :type "html"
       ))))

(defun maybe-show-size-warning (output-snippet)
  (unless (get wb::*sessionid* :vpl-size-warning)
    (when (> (get-snippet-property output-snippet :crude-size)
             *crude-size-threshold*)
      (setf (get wb::*sessionid* :vpl-size-warning) t)
      (user-info-message 
       (formatn 
        (one-string-nl
         "The output you just generated takes up a lot"
         "of space! Usually output (and any printout"
         "associated with it) is saved indefinitely until"
         "you clear the results area or do a vpl window"
         "refresh.  But because this output is so large,"
         "it will be removed in ~d minutes.  If you"
         "want to save it you should copy the output"
         "box to your workspace and store the data"
         "in a variable."
         ""
         "you will not see this message again, but the"
         "same warning applies to other large results"
         "you might generate henceforth!"
         )
        *vpl-reaper-object-duration*
        )))))
    
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; this is the function that the wb::result function calls. 
;;; wb::result is available from the vpl as it is imported and exported 
;;; from the bbl package.

(defun wb::vpl-result (n &optional (value-index 1))
  (let ((output-snippet
         (find n (snippet-children (uvs-rs *vpl-state*))
               :key (lambda (x) (get-snippet-property x :output-index))
               )))
    (unless output-snippet
      (vpl-user-error 
       (formatn "no existing result numbered with '~d' !" n)
       ))
    (let* ((values (get-snippet-property output-snippet :values))
           (vlen (length values)))
      (unless (eq (get-snippet-property output-snippet :result-type)
                  :success)
        (vpl-user-error
         (formatn "no actual result for ~d, evaluation resulted in an error" n)
         ))
      (unless (and (integerp value-index) (plusp value-index))
        (vpl-user-error 
         (formatn "vpl-result: bad 2nd argument, ~s" value-index)
         ))
      (when (> value-index vlen)
        (vpl-user-error
         (formatn 
          "vpl-result: you requested value ~d, but only ~d values exist"
          value-index vlen
          )))
      (nth (1- value-index) values)
      )))

(defmethod vpl-error-to-string ((error error)) (formatn "~a" error))

(defmethod vpl-error-to-string ((error wb::wb-unbound-error))
  (if (wb::choices error)
      (formatn 
       (one-string-nl
        "*** PROBLEM: I don't understand what you mean by '~A'"
        ""
        "*** ADVICE: Perhaps you misspelled the name of a ~A?"
        "            Perhaps you meant one of these:"
        ""
        "~A"
        "    Or perhaps you intended it to be a string, e.g., \"~A\"."
        )
       (wb::name error)
       (wb::error-string error)
       (loop
        with choices = ""
        for choice in (wb::choices error)
        as symbol = (getf choice :symbol)
        do 
        (setq 
         choices
         (s+ choices (formatn "              ~a ~%" symbol)))
        finally (return choices)
        )
       (wb::name error)
       )
    (formatn 
     (one-string-nl
      "*** PROBLEM: I don't understand what you mean by '~A'"
      ""
      "*** ADVICE: Perhaps you misspelled the name of a ~A?"
      "    Or perhaps you intended it to be a string, e.g., \"~A\"."
      )
     (wb::name error)
     (wb::error-string error)
     (wb::name error)
     )))
     

#||

   "<<< unbound ~a: ~s >>>~% maybe you meant one of these?~%~%~a"
   (wb::error-string error) 
   (wb::name error)
   (loop
    with choices = ""
    for choice in (wb::choices error)
    as symbol = (getf choice :symbol)
    do 
    (setq 
     choices
     (s+ choices (formatn "  ~a ~%" symbol)))
    finally (return choices)
    )))

||#


(defmethod print-object ((object wb::wb-unbound-error) stream)
  (format stream "< unbound error  ~A >" (vpl-error-to-string object)))