(in-package :com.gigamonkeys.foo)

(defun generate-from-file (language input &optional output)
  (unless output
    (setf output (make-pathname :type (output-file-type language) :defaults input))
    (assert (not (equal (probe-file input) (probe-file output)))))
  (with-open-file (in input)
    (with-open-file (out output :direction :output :if-exists :supersede)
      ;;(write-line (comment language (format nil "Generated at ~a from ~a.~%" (format-iso-8601-time (get-universal-time)) (truename in))) out)
      (let ((*readtable* (input-readtable language))
	      (*package* (input-package language))
	    (env (top-level-environment language)))
	(with-foo-output (out :pretty t)
	  (loop with processor = (get-pretty-printer) 
	     for form = (read in nil in)
	     while (not (eql form in)) do
	       (process language processor form env)
	       (newline processor)
	       (newline processor)))))))

(defun generate-from-string (language string)
  (with-input-from-string (in string)
    (with-output-to-string (out)
      ;;(write-line (comment language (format nil "Generated at ~a from ~a.~%" (format-iso-8601-time (get-universal-time)) (truename in))) out)
      (let ((*readtable* (input-readtable language))
	    (*package* (input-package language))
	    (env (top-level-environment language)))
	(with-foo-output (out :pretty t)
	  (loop with processor = (get-pretty-printer) 
	     for form = (read in nil in)
	     while (not (eql form in)) do
	       (process language processor form env)
	       (newline processor)
	       (newline processor)))))))

(defun generate-from-sexp (language sexp)
  (with-output-to-string (out)
    (let ((env (top-level-environment language)))
      (with-foo-output (out :pretty t)
	(let ((processor (get-pretty-printer)))
	  (process language processor sexp env))))))



