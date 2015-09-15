;;; -*- mode: Lisp; Syntax: Common-Lisp; Package: wb ; -*-

(in-package :wb)

;;; +=========================================================================+
;;; | Copyright (c) 2008 JP Massar                                            |
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

(defclass workflow ()
  ((result :accessor wf-result :initarg :wf-result :initform nil))
  )

(defun set-wf-result (wf result) (setf (wf-result wf) result))

(defclass serial-workflow (workflow)
  ((current-step
    :accessor swf-current-step :initarg :current-step :initform nil)
   (step-results
    :accessor swf-step-results :initarg :step-results :initform nil)
   ))

(defun set-swf-current-step (wf step) (setf (swf-current-step wf) step))

(defun workflow-track (counter result workflow-object)
  (setf (swf-current-step workflow-object) counter)
  (push (list counter result) (swf-step-results workflow-object)))
  

(defmacro in-sequence ((results-var &rest options) &body body)
  (declare (ignore options))
  (let ((wfo-symbol (gensym "SERIAL-WORKFLOW-"))
        (counter 0))
    `(let ((,wfo-symbol (make-instance 'serial-workflow :current-step 0)))
       (block exit
         (flet ((exit-workflow (value) 
                  (set-wf-result ,wfo-symbol value)
                  (set-swf-current-step ,wfo-symbol :done)
                  (return-from exit ,wfo-symbol)
                  ))
           (tagbody 
            ,@(mapcar 
               (lambda (step) 
                 (cond
                  ((symbolp step) step)
                  ((listp step) 
                   (incf counter)
                   `(progn 
                      (workflow-track 
                       ,counter (setq ,results-var ,step) ,wfo-symbol)
                      ))
                  (t (error "Unrecognized step: ~A, of type ~A"
                            step (type-of step)
                            ))))
               body
               ))
           (exit-workflow ,results-var)
           )))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defclass parallel-workflow (workflow)
  ((processes 
    :accessor pwf-processes :initarg :processes :initform nil)
   (results 
    :accessor pwf-results :initarg :results :initform nil)
   (status 
    :accessor pwf-status :initarg :status :initform nil)
   (info
    :accessor pwf-info :initarg :info :initform nil)
   ))


(defmacro in-parallel ((results-var &rest options) &body body)
  (unless body 
    (error "No body forms for IN-PARALLEL specified!"))
  (loop for body-form in body do
        (unless (listp body-form)
          (error "Invalid body form for IN-PARALLEL: ~A" body-form)))
  (multiple-value-bind (wait-for combiner)
      (parse-in-parallel-options options)
    (let ((n (length body)))
      (when (numberp wait-for)
        (unless (<= wait-for n)
          (error
           (one-string
            "There are only ~D body form processes, but you said to wait for "
            "at least ~D processes to terminate before halting execution!")
           n wait-for
           )))
      (generate-in-parallel-code results-var wait-for combiner body)
      )))

;;; (in-parallel (r) (+ 1 2) (+ 3 4))

(defun generate-in-parallel-code (results-var wait-for combiner body-forms)
  (let ((wfo-symbol (gensym "PARALLEL-WORKFLOW-"))
        (n (length body-forms)))
    `(let ((,wfo-symbol 
            (make-instance
             'parallel-workflow
             :info (list (function ,combiner) ,wait-for ,n)
             :results (make-list ,n :initial-element nil)
             :status (make-list ,n :initial-element :not-yet-executing)
             )))
       (setq ,results-var ,wfo-symbol)
       (spawn-parallel-processes
        ,results-var
        ,@(mapcar (lambda (form) `(lambda () ,form)) body-forms)
        )
       ,wfo-symbol
       )))

(defun spawn-parallel-processes (pw &rest process-functions)
  (loop for process-function in process-functions 
        for id-count from 0
        do
        (let ((process 
               (mp:process-run-function 
                (formatn "Workflow Parallel Process ~D" id-count)
                'execute-parallel-process 
                process-function
                pw
                id-count
                )))
          (push process (pwf-processes pw))
          )))

;;; This takes care of executing a parallel workflow step, FUNCTION

(defun execute-parallel-process (function pw id)
  ;; Execute the workflow step.  (Could implement a timeout here). 
  (set-pwf-status pw id :executing)
  (handler-case 
      (let ((result 
             (forward-package-funcall 
              :vpl :execute-function-in-bbl-context function
              )))
        (set-pwf-status pw id :complete)
        (set-pwf-result pw id result))
    (error 
     (c)
     (set-pwf-status pw id :error)
     (set-pwf-result pw id c)
     ))
  ;; Figure out if we're supposed to terminate the entire workflow
  ;; and combine the results now that this step has completed.
  (let ((terminate-workflow? nil)
        (combiner (first (pwf-info pw)))
        (wait-for (second (pwf-info pw))))
    (setq
     terminate-workflow?
     (cond
      ((eq wait-for :any) t)
      ((eq wait-for :every) (every 'process-terminated? (pwf-status pw)))
      ((integerp wait-for) 
       (>= 
        ;; number that have finished including this one
        (count-if 'process-terminated? (pwf-status pw))
        wait-for
        ))
      (t (error "This is impossible!"))
      ))
    (when terminate-workflow? 
      ;; Don't need to kill any if they're all complete...
      (unless (eq wait-for :every)
        (kill-all-the-other-workflow-processes pw))
      ;; Now do the combining of results.
      ;; Only operate on the results of processes that completed successfully
      (flet ((collect-completed-results () 
               (loop for status in (pwf-status pw)
                     for result in (pwf-results pw)
                     when (eq status :complete)
                     collect result
                     )))
        (cond
         ((eq combiner #'identity)
          ;; only put in results list actual results
          (set-wf-result pw (collect-completed-results)))
         (t 
          ;; Only combine results from processes that actually finished
          (set-wf-result
           pw
           (reduce 
            combiner
            (collect-completed-results)
            ))))))
    ))

  
;;; Valid returned values for WAIT-FOR: :any, :every, or an integer 
;;; Valid returned values for COMBINER: 
;;;   :list lisp:+, lisp:*, lisp:min, lisp:max, or some other symbol
;;;   representing a function of two arguments
(defun parse-in-parallel-options (options)
  (let ((wait-for-pos (position :wait-for options :test 'symbol=))
        (combiner-pos (position :combine-with options :test 'symbol=))
        (wait-for :every)
        (combiner 'lisp:identity))
    (when wait-for-pos 
      (setq wait-for (nth (1+ wait-for-pos) options))
      (cond
       ((or (symbol= wait-for :any) (symbol= wait-for :one))
        (setq wait-for :any))
       ((or (symbol= wait-for :every) (symbol= wait-for :all))
        (setq wait-for :every))
       ((integerp wait-for) 
        (unless (plusp wait-for)
          (error "Invalid WAIT-FOR option value: ~A" wait-for))
        nil)
       (t (error "Unrecognized WAIT-FOR option value: ~A" wait-for))
       ))
    (when combiner-pos 
      (setq combiner (nth (1+ combiner-pos) options))
      (cond
       ((or (symbol= combiner :plus) (symbol= combiner :+)
            (symbol= combiner :add) (symbol= combiner :sum))
        (setq combiner 'lisp:+))
       ((or (symbol= combiner :multiply) (symbol= combiner :*)
            (symbol= combiner :product) (symbol= combiner :times))
        (setq combiner 'lisp:*))
       ((or (symbol= combiner :min) (symbol= combiner :minimum))
        (setq combiner 'lisp:min))
       ((or (symbol= combiner :max) (symbol= combiner :maximum))
        (setq combiner 'lisp:max))
       ((or (symbol= combiner :list) (symbol= combiner :nil)
            (symbol= combiner :identity))
        (setq combiner 'lisp:identity))
       ((keywordp combiner) 
        (error "Unrecognized COMBINE-WITH option value: ~A" combiner))
       ((symbolp combiner) combiner)
       (t 
        (error "Unrecognized COMBINE-WITH form: ~A" combiner)
        )))
    (values wait-for combiner)
    ))

(defun set-pwf-status (pwf id status) 
  (setf (nth id (pwf-status pwf)) status))

(defun set-pwf-result (pwf id result)
  (setf (nth id (pwf-results pwf)) result))

(defun process-terminated? (status)
  (or (eq status :complete) (eq status :error)))

(defun kill-all-the-other-workflow-processes (pw)
  (loop for process in (pwf-processes pw)
        for status in (pwf-status pw)
        do
        (when (not (process-terminated? status))
          (mp:process-kill process :wait t)
          )))

        
;; (in-sequence (r) (+ 1 2) (+ 3 4) (+ 4 5))

;; (in-sequence (r) (assign x 0) tag (increment x) (when (< x 2) (go tag)))
