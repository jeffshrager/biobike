;;; -*- mode: Lisp; Syntax: Common-Lisp; Package: user; -*-

;;; Author:  JP Massar.

(in-package :cl-user)

(eval-when (:compile-toplevel :load-toplevel :execute)

  (defparameter *tests-api-symbols*
    '(
      write-compile-and-test
      deftest
      deferrortest
      run-test
      run-chapter-tests
      delete-chapter
      delete-test-from-chapter
      signals-error-of-type
      ))

  (export *tests-api-symbols*)

  )


#-:USEFUL-STUFF-LOADED
(progn
  (defmacro filevar (x) (declare (ignore x)) nil)
  #-ALLEGRO
  (defmacro without-redefinition-warnings (&rest body) `(progn ,@body))
  (defun key-intern (string) (intern string (find-package :keyword)))  
  (defun fformat (&rest args)
    (apply #'format t args) (terpri t) (force-output t))

;;; Returns a new number X such that if VALUE is within the 
;;; inclusive bounds (LOW HIGH) then X <- VALUE, otherwise
;;; X is either LOW or HIGH.

;;; If WRAP? is enabled, and VALUE > HIGH, then X <- LOW.
;;; If WRAP? is enabled, and VALUE < LOW, then X <- HIGH.

  (defun constraint (value low high wrap?)
    (cond
     ((and (>= value low) (<= value high)) value)
     ((< value low) (if wrap? high low))
     ((> value high) (if wrap? low high))
     ))

;;; Side effects a variable to possibly have a new value
;;; within the bounds specified.

;;; Example usage, keeping a value between 1 and 10; when it
;;; exceeds 10, it goes back to 1.
;;; (dotimes (j 100000) (let ((c j)) (constrain c 1 10 :wrap t) ...))

  (defmacro constrain (variable low-inclusive high-inclusive &key (wrap nil))
    (unless (symbolp variable) (error "Must be symbol, not place."))
    `(setf ,variable 
       (constraint ,variable ,low-inclusive ,high-inclusive ,wrap)))

  )


(filevar testfile)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun make-function-object (form)
    (cond
     ((null form) nil)
     ((functionp form) form)
     (t `(function (lambda () ,form)))
     )))

;;; Simple mechanism to 
;;; -- write test code to a file, compile it and load it
;;; -- execute some other code that presumably calls the code just created
;;; -- execute some verification code to check up on the results

(defun write-test-file (filename package forms)
  (with-open-file
      (p filename :direction :output :if-exists :supersede 
       :if-does-not-exist :create)
    (with-standard-io-syntax
      (let* ((*package* (find-package package))
             (package-name (key-intern (string (package-name *package*)))))
        (dolist (form (cons `(in-package ,package-name) forms))
          (pprint form p) (terpri p) (terpri p))
        (terpri p)
	))))

(defun write-compile-and-test

    (test-name filepath package-name forms execution-form verify-form
     &key 
     (verbose t) (execution-info nil) (verify-info nil) (delete-source t))

  "Write code to a file and compile it, then run a test."

  (flet ((notify (format-string &rest args)
	   (when verbose
	     (apply #'format t (format nil "~&;;; ~A~%" format-string) args))))

    (if verbose
	(format t "~&~%;;; ***> Executing test ~S~%" test-name)
      (format t "~%;;; ~S~%" test-name)
      )
	  
    (let ((input-filestring (namestring (pathname filepath)))
	  (output-path nil)
	  )
      (unwind-protect
	  (progn
	    (notify "Writing test code to file ~A" input-filestring)
	    (write-test-file filepath package-name forms)
	    (notify "Compiling test code in file ~A" input-filestring)
	    (without-redefinition-warnings 
	     (setq output-path (compile-file filepath)))
	    (notify "Executing test code.")
	    (when execution-info (notify "Execution info: ~A" execution-info))
	    (eval execution-form)
	    (notify "Test ~S completed without error." test-name)
	    (notify "Running verification code.")
	    (when verbose (format t "~&;;; Running verification code...~%"))
	    (when verify-info (notify "Verify info: ~A" verify-info))
	    (eval verify-form)
	    (notify "Verification complete.")
	    )
	(when delete-source
	  (when (probe-file filepath) (delete-file filepath)))
	(when (probe-file output-path) (delete-file output-path))

	))))


;;; Simple test harness.
;;; Tests are defined using DEFTEST.  Each test belongs to a CHAPTER.
;;; All the tests in a CHAPTER can be run as a suite using RUN-CHAPTER-TESTS.

;;; Each test is given a NAME, and consist of 
;;; -- code to be executed to obtain an actual result
;;; -- code to generate an expected result
;;; -- the comparison function to use to compare actual vs expected
;;;    (defaults to #'eql)
;;; -- optionally, setup code to be executed before the test is run.

;;; Example:  (deftest foo (+ 3 4) 7)
;;; This creates a test FOO in the :DEFAULT chapter, which verifies
;;; when run that (+ 3 4) returns 7.



(defun make-test-object 
    (setup-function test-code-function expected-result-function comparison)
  (when (null test-code-function) (error "Test code must not be NIL"))
  (list setup-function test-code-function expected-result-function comparison)
  )

;; Replace an existing test by the same name, otherwise
;; add the test to the list of tests.
(defun add-test-to-chapter (chapter-name test-name test-object)
  (block exit
    (let ((testlist (get chapter-name :tests)))
      (dolist (test testlist)
	(let ((existing-name (first test)))
	  (when (eq existing-name test-name)
	    (setf (second test) test-object)
	    (return-from exit :replaced)
	    )))
      (setf (get chapter-name :tests) 
	(cons (list test-name test-object) testlist))
      :new
      )))

(defun delete-test-from-chapter (chapter-name test-name)
  (let ((testlist (get chapter-name :tests)))
    (setq testlist (delete test-name testlist :key 'first))
    (setf (get chapter-name :tests) testlist)
    test-name
    ))

(defun delete-chapter (chapter-name) 
  (let ((tests (get chapter-name :tests)))
    (format t "~%;; Deleting ~D tests."  (length tests)))
  (setf (get chapter-name :tests) nil))
  
(defmacro deftest 
    (name test-function-or-form expected-result-function-or-form
     &key (chapter :default) (setup-function-or-form nil)
	  ;; This problem is totally weird.  For some reason
	  ;; this version of Lispworks (4.1.2) chokes compiling
	  ;; the output of a macro which takes a function object
	  ;; as the default value of an argument.
          ;; (Sees to work in 4.2)
          ;; (comparison #'eql)
	  #-:LISPWORKS
	  (comparison #'eql)
	  #+:LISPWORKS 
	  (comparison '#'eql)
	  )
  "Define a test to be referenced using NAME"
  (when (symbolp comparison) (setq comparison `(function ,comparison)))
  `(add-test-to-chapter
    ',chapter
    ',name
    (make-test-object
     ,(make-function-object setup-function-or-form)
     ,(make-function-object test-function-or-form)
     ,(make-function-object expected-result-function-or-form)
     ,comparison
     )))

(defmacro deferrortest (name test-form condition &rest goo)
  `(deftest 
    ,name 
    (handler-case (progn ,test-form :form-returned-without-signalling-condition)
      (,condition () :form-signalled-condition))
    :form-signalled-condition
    ,@goo
    ))

(defun run-test (name &optional (chapter-name :default))
  "Run the test associated with NAME via DEFTEST"
  (block exit
    (let ((testlist (get chapter-name :tests)))
      (let ((test (find (string name) testlist 
                        :key (lambda (x) (string (first x))) :test 'string=)))
	(when (null test)
	  (warn "No test named ~S in chapter ~S." name chapter-name)
	  (return-from exit :not-found)
	  )
	(let* ((test-object (second test))
	       (setup (first test-object))
	       (testcode (second test-object))
	       (expected-result-code (third test-object))
	       (comparison (fourth test-object))
	       (actual-result nil)
	       (expected-result nil)
	       (comparison-result nil)
	       )
	  (unless (null setup)
	    (multiple-value-bind (setup-result condition)
		(ignore-errors (funcall setup))
	      (declare (ignore setup-result))
	      (when condition 
		(return-from exit (values :setup-error condition)))))
	  (setq actual-result
	    (multiple-value-bind (test-result condition)
		(ignore-errors (funcall testcode))
	      (when condition
		(return-from exit (values :test-error condition)))
	      test-result
	      ))
	  (setq expected-result
	    (cond
	     ((null expected-result-code) nil)
	     (t (multiple-value-bind (expected-result-code-result condition)
		    (ignore-errors (funcall expected-result-code))
		  (when condition 
		    (return-from exit 
		      (values :expected-result-error condition)))
		  expected-result-code-result
		  ))))
	  (setq comparison-result
	    (multiple-value-bind (result condition)
		(ignore-errors 
		 (funcall comparison actual-result expected-result))
	      (when condition
		(return-from exit (values :comparison-error condition)))
	      result
	      ))
	  (if comparison-result
	      :correct
	    (values :incorrect actual-result expected-result)
	    ))))))

		 

(defun note-execution-error (test-name which condition)
  (fformat "~&*** Test ~S failed executing ~A code." test-name which)
  (fformat "  Error condition signalled: ~A." condition)
  )

(defun chapter-report (name ntests verbose ok not-ok ee failed-names)
  (flet ((show-failed-tests ()
           (when failed-names
             (format t "~%Failed tests:~%~%")
             (loop for name in failed-names for j from 1 do
                   (format t "~20A " name)
                   (when (zerop (mod j 3)) (terpri))
                   finally (format t "~%~%")
                   ))))          
    (if (not verbose)
        (progn
          (format t "~%Done.  ~D ~D ~D~%" ok not-ok ee)
          (show-failed-tests)
          )
      (progn
        (format t "~&~%Tests for chapter ~S completed.~%" name)
        (if (= ok ntests)
            (format t "  STATUS: ALL ~D TESTS EXECUTED CORRECTLY.~%~%" ntests)
          (progn
            (format t "  STATUS: ~D TESTS RUN.~%" ntests)
            (format t "   CORRECT: ~D, INCORRECT: ~D, EXECUTION ERRORS: ~D.~%~%"
                    ok not-ok ee)
            (show-failed-tests)
            ))))
    (force-output t)))

(defun run-chapter-tests (chapter-name &key (verbose nil))
    "Run the before code, all the tests, and the after code for CHAPTER-NAME"
  (let ((before-code (get chapter-name :tests-before-code)))
    (when before-code (eval before-code))
    (unwind-protect
        (run-chapter-tests-internal chapter-name verbose)
      (let ((after-code (get chapter-name :tests-after-code)))
        (when after-code (eval after-code))
        ))))

(defun run-chapter-tests-internal (chapter-name verbose)
  "Run all the tests belonging to CHAPTER-NAME"
  (block exit
    (if verbose 
	(fformat "~%~%Running tests for chapter ~S." chapter-name)
      (fformat "~%Chapter ~S " chapter-name))
    (let* ((tests (get chapter-name :tests))
	   (ntests (length tests))
	   (correct-count 0) (incorrect-count 0) (execution-errors-count 0))
      (when (null tests)
	(fformat "~&No tests found for chapter ~S." chapter-name)
	(return-from exit nil)
	)
      ;; Tests are on list in reverse order of definition.
      ;; Do them in the order they were defined, so in reverse.
      (let ((test-names (reverse (mapcar #'first tests)))
	    (failed-test-names nil)
	    (dot-count 0)
	    )
	(dolist (test-name test-names)
	  (if verbose
	      (format t "~&  Running test ~S..." test-name)
	    (progn
	      (format t ".") (force-output t)
	      (incf dot-count)
	      (constrain dot-count 0 60 :wrap t)
	      (when (zerop dot-count) (terpri))
	      ))
	  (force-output t)
	  (multiple-value-bind (status actual-or-condition expected)
	      (run-test test-name chapter-name)
	    (push test-name failed-test-names)
	    (ecase status
	      (:correct 
	       (when verbose (format t "OK.~%"))
	       (incf correct-count)
	       (setq failed-test-names (delete test-name failed-test-names)))
	      (:incorrect
	       (fformat "~&*** Test ~S failed.  Expected: ~S, Actual: ~S."
		       test-name expected actual-or-condition)
	       (incf incorrect-count))
	      (:setup-error
	       (incf execution-errors-count)
	       (note-execution-error test-name "setup" actual-or-condition))
	      (:test-error	
	       (incf execution-errors-count)
	       (note-execution-error test-name "test" actual-or-condition))
	      (:expected-result-error
	       (incf execution-errors-count)
	       (note-execution-error test-name "result" actual-or-condition))
	      (:comparison-error
	       (incf execution-errors-count)
	       (note-execution-error test-name "compare" actual-or-condition))
	      )))
	(setq failed-test-names (reverse failed-test-names))
	(chapter-report 
	 chapter-name ntests verbose
	 correct-count incorrect-count execution-errors-count
	 failed-test-names
	 )))))

(defmacro signals-error-of-type ((type) &body body)
  "Returns T if BODY signals error of type TYPE, NIL if executes properly"
  `(handler-case 
       (progn (progn ,@body) nil)
     (,(if (eq type t) 'error type) 
      ()
      t
      )
     ,@(unless (member type '(t error)) `((error () nil)))
     ))


;;; Simple test set to demonstrate test harness.
;;; Execute (run-chapter-tests :xyzzy :verbose t)

#-:SBCL
(deftest addok (+ 3 4) 7 :chapter :xyzzy)
#-:SBCL
(deftest addwrong (+ 3 4) 8 :chapter :xyzzy :comparison #'=)
#-:SBCL
(deftest addoops (+ 3 4 (progn 2 (error "oops"))) 9 :chapter :xyzzy)


#||

Create a function which reads a set of test forms and executes each
one in turn, reporting at the end which tests succeeded and which
failed, keeping around those tests that fail so that they can be run
individually.

The function should ignore any (in-package ...) form.  

Each form in the file should be a DEFTEST form or a macro that expands
(eventually) into a DEFTEST form.  The restriction is that the second element
of the form must always name the test, so we can keep track of the names.   

||#

(defun run-test-file 
       (file package 
             &key 
             (chapter (package-name package))
             (verbose t))
  (setq chapter (utils::keywordize chapter))
  (unless (probe-file file)
    (error "File ~A does not exist!" file))
  (when verbose 
    (format t ";; Running tests for chapter ~A from tests file ~A"
            chapter file))
  (let ((*package* (find-package package))
        (unique (cons nil nil))
        (tests-with-compilation-errors nil)
        (tests-with-execution-errors nil)
        (failed-tests nil)
        (correct-tests nil)
        (test-count 0)
        )
    (with-open-file (p file :direction :input)
      (loop 
       for form = (read p nil unique)
       until (eq form unique)
       do 
       (if (valid-test-form? form)
           (let ((name (second form)))
             (incf test-count)
             (if (not verbose) 
                 (progn 
                   (format t ".")
                   (when (zerop (mod test-count 70)) (terpri)))
               (format t ";; Test ~A: " name))
             (multiple-value-bind (status actual-or-condition expected)
                 (execute-test-form form chapter)
               (ecase status
                 (:compilation-error 
                  (format 
                   t ";; Test named '~A' did not compile properly!~%" name)
                  (format t ";; Actual compilation error:~%")
                  (format t ";; ~A" actual-or-condition)
                  (push name tests-with-compilation-errors))
                 (:correct 
                  (when verbose (format t "OK.~%"))
                  (push name correct-tests))
                 (:incorrect
                  (fformat "~&*** Test ~S failed.  Expected: ~S, Actual: ~S."
                           name expected actual-or-condition)
                  (push name failed-tests))
                 ((:setup-error :test-error 
                   :expected-result-error :comparison-error)
                  (push name tests-with-execution-errors)
                  (note-execution-error name "setup" actual-or-condition))
                 )))
         (progn 
           (format t ";; Warning: Ignoring non-test form in test-file: ~%")
           (pprint form t)
           ))))
    (format t "~%~%;; Results for chapter ~A:~%" chapter)
    (format t ";;   Number run: ~D, Correct: ~D~%" 
            test-count (length correct-tests))
    (format 
     t ";;   Failed: ~D, Execution errors: ~D, Compilation errors: ~D~%"
     (length failed-tests)
     (length tests-with-execution-errors)
     (length tests-with-compilation-errors)
     )
    (flet ((show-bad-tests (label tests)
             (when tests 
               (format t "~%;; ~A: " label)
               (loop for remaining-tests on tests do
                     (format t "~A" (first remaining-tests))
                     (when (cdr remaining-tests) (format t ", "))
                     ))))
      (when verbose 
        (show-bad-tests "Compilation errors" tests-with-compilation-errors)
        (show-bad-tests "Execution errors" tests-with-execution-errors)
        (show-bad-tests "Failed" failed-tests)
        ))))
      

(defun valid-test-form? (form)
  (when (and (listp form) (symbolp (first form)))
    (cond 
     ((or (utils::symbol= (first form) 'defun)
          (utils::symbol= (first form) 'defmacro))
      (error 
       (utils::one-string-nl
        "Found definition form in tests file!  All auxiliary"
        "functions used in tests should be loaded in files"
        "separately from the test files!")))
     (t 
      (and (> (length form) 2) (symbolp (second form)))
      ))))


      
(defun execute-test-form (form chapter)
  (block exit
    (let ((test-name (second form)))
      (handler-case 
          (funcall (compile nil `(lambda () ,form)))
        (error (c) (return-from exit (values :compilation-error c nil))))
      (run-test test-name chapter)
      )))


    
                      
              
            
            
     
      
    