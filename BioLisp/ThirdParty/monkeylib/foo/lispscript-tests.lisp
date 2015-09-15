;;
;; Copyright (c) 2005, Gigamonkeys Consulting All rights reserved.
;;

(in-package :com.gigamonkeys.foo.lispscript-tests)

(defparameter *to-trim* (list #\Space #\Newline))

(defun make-test (string)
  (list string (string-trim *to-trim* (generate-from-string *lispscript* string))))

(defun testpage ()
  (html
    (:html
      (:head
       (:title "Lispscript tests")
       (:css
	 (:body :margin ".5in")
	 (:p :font-weight :bold)
	 (:table :width "100%": background "#bbbbff")
	 ((and :thead :tr) :font-weight :bold)
	 (:tr :background "#cccccc")
	 (:td :padding "3pt" :font-family "Courier"  :font-size "8pt"))
       (:script :type "text/javascript" :src "lispscript-tests.js")
       (:script :type "text/javascript" :src "lispscript-test-cases.js"))

      ((:body :onload "runTests();")
       (:p "Passes: " (:span :id "passes" "0") "; Failures: " (:span :id "failures" "0") " " (:character :mdash) " " (:span :id "done" "Tests didn't complete."))
       (:table
	(:thead
	 (:tr
	  (:td "Lispscript")
	  (:td "Javascript")
	  (:td "Expected")
	  (:td "Result")
	  (:td "Okay?")))
	((:tbody :id "results")))))))

(defun testcases ()
  (flet ((js (string) 
	   (string-trim *to-trim* (generate-from-string *lispscript* string))))
    (js 
     (with-output-to-string (out)
       (flet ((test (form expected)
		(let ((js (substitute #\Space #\Newline (js form))))
		  (format out "(dotest ~s ~s (eval ~s) (ignore-errors (eval ~s)))"
			  form js js expected))))
	 (write-string "(defun run-tests () " out)
	 (with-open-file (tests "lispscript-test-cases.txt")
	   (loop for test = (read-line tests nil nil)
	      while test 
	      unless (blank-or-comment-p test)
	      do (test test (read-line tests)))))
       (write-string "(all-tests-done))" out)))))

(defun blank-or-comment-p (line)
  (let ((trimmed (string-trim '(#\Space #\Tab) line)))
    (or (zerop (length trimmed))
	(char= (char trimmed 0) #\;))))

(defun maketests ()
  (let ((html-file (merge-pathnames "lispscript-tests.html"))
        (ls-file (merge-pathnames "lispscript-tests.ls"))
        (test-cases-file (merge-pathnames "lispscript-test-cases.js")))
    (with-html-to-file (html-file) (testpage))
    (generate-from-file *lispscript* ls-file)
    (with-open-file (out test-cases-file :direction :output :if-exists :supersede)
      (write-string (testcases) out))
    (terpri)
    (format t ";; Testing Lispscript found in ~A~%" (namestring ls-file))
    (format t ";; For test results view ~A~%" (namestring html-file))
    (format t ";;   using a Firefox broswer.~%")
    nil))


