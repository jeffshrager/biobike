;;
;; Copyright (c) 2005, Gigamonkeys Consulting All rights reserved.
;;

(in-package :com.gigamonkeys.test)

(defvar *test-name* nil)
(defvar *report-passes* nil)

(defparameter *debug* t)

(defun listify (thing)
  (if (listp thing) thing (list thing)))

(defmacro with-test-results ((&key print summary) &body body)
  "Collect test results signaled in `body' (except from forms in
the dynamic extent of an IGNORE-RESULTS."
  (with-gensyms (pass fail abort)
    (once-only (print summary)
      `(let ((,pass 0)
	     (,fail 0)
	     (,abort 0)
	     (,print (listify ,print)))
	 (handler-bind ((test-result
			 #'(lambda (c) 
			     (ecase (result c)
			       (:pass (incf ,pass))
			       (:fail (incf ,fail))
			       (:abort (incf ,abort)))
			     (when (or (eql (car ,print) t) (member (result c) ,print))
			       (print-result c)))))
	     (handler-case
		 (handler-bind ((error #'report-unexpected-error)) ,@body)
	       (error ())))
	 (let ((ok (zerop (+ ,fail ,abort))))
	   (when ,summary (print-result-summary ok ,pass ,fail ,abort))
	   (values ok ,pass ,fail))))))

(defgeneric report-unexpected-error (error))

(defmethod report-unexpected-error :around (error)
  (if *debug* 
      (restart-case
	  (progn
	    (call-next-method)
	    (format t "About to invoke debugger: ~s on ~s~%" *debugger-hook* error)
	    (invoke-debugger error))
	(procede ()))
      (call-next-method)))

(defmethod report-unexpected-error ((error cell-error))
  (report-abort (format nil "~a ~a" (class-name (class-of error)) (cell-error-name error))))

(defmethod report-unexpected-error ((error error))
  (report-abort (format nil "~a" error)))

(defmacro deftest (name parameters &body body)
  "Define a test function. Within a test function we can call other
test functions or use `check' to run individual test cases."
  `(setf (get ',name 'test-function)
	 (lambda ,parameters
	   (with-test-results ()
	     (let ((*test-name* (append *test-name* (list ',name))))
	       ,@body)))
	 (get ',name 'test-function-parameters)
	 ',parameters))

(defmacro check (&body forms)
  "Run each expression in `forms' as a test case."
  `(progn
    ,@(loop for f in forms collect `(report-result ,f ',f))))

(defmacro test (name &rest arguments)
  "Run a named test."
  `(with-test-results (:print :fail :summary t)
     (run-test ',name ,@arguments)))

(defmacro with-test-cases ((&rest parameters) test-form &rest test-cases)
  "Check a single test form with multple test cases."
  (with-gensyms (test p)
    `(flet ((,test (,p) (destructuring-bind (,@parameters) ,p ,test-form)))
       ,@(loop for test-case in test-cases collect 
	      `(let ((*test-name* (append *test-name* ',test-case)))
		 (,test (list ,@test-case)))))))

(defmacro ignore-results (&body body)
  "Ignore results signaled in `body'. This is, at the very least,
handy when writing tests for the test framework itself."
  `(handler-bind ((test-result #'ignore-result)) ,@body))

(define-condition test-result ()
  ((result :initarg :result :reader result)
   (name :initarg :name :reader name)
   (form :initarg :form :reader form)))

(define-condition test-pass (test-result) ())
(define-condition test-fail (test-result) ())
(define-condition test-abort (test-result) ())

(defgeneric run-test (thing &rest arguments))

(defmethod run-test ((name symbol) &rest arguments)
  (let ((test-function (test-function name)))
    (if test-function
	(apply test-function arguments)
	(error "No test named ~a" name))))

(defun test-package (&key (print '(:abort :fail)) (summary t) (package *package*))
  (with-test-results (:print print :summary summary)
    (do-symbols (sym package)
      (when (and (test-function sym) (not (get sym 'test-function-parameters)))
	(run-test sym)))))

(defun clear-package-tests (&key (package *package*))
  (do-symbols (sym package)
    (when (test-function sym)
      (remove-test-function sym))))

(defun test-function (symbol) (get symbol 'test-function))

(defun remove-test-function (symbol) (remprop symbol 'test-function))

(defun report-result (result form)
  "Report the results of a single test case. Called by `check'."
  (restart-case 
      (signal (if result 'test-pass 'test-fail) 
	      :result (if result :pass :fail)
	      :name *test-name* 
	      :form form)
    (ignore-result () nil)))

(defun report-abort (message)
  (restart-case 
      (signal 'test-abort 
	      :result :abort
	      :name *test-name*
	      :form message)
    (ignore-result () nil)))

(defun ignore-result (&optional condition)
  (let ((restart (find-restart 'ignore-result condition)))
    (when restart (invoke-restart restart))))

(defun print-result (result)
  (format t "~a ... ~a: ~a~%" (result result) (name result) (form result)))


(defun print-result-summary (ok passes failures aborts)
  (format t "~:[NOT o~;O~]kay: ~d passes; ~d failures; ~d aborts.~%" ok passes failures aborts))

