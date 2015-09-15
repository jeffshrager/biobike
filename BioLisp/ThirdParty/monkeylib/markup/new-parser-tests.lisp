(in-package :com.gigamonkeys.markup)

(defmacro with-special-tags (&body body)
  `(let ((*paragraph-tags* '("myp"))
	 (*subdocument-tags* '("note")) )
     ,@body))

(deftest test-files ()
  (let ((files (list-directory #p"./testfiles/")))
    (dolist (file files)
      (when (and (string= (pathname-type file) "txt")
		 (file-exists-p (make-pathname :type "sexp" :defaults file)))
	(test-file file)))))

(defun test-file (text)
  (with-special-tags
    (format t "Testing ~a~%" text)
    (let ((got (parse-file text))
	  (expected (with-open-file (in (make-pathname :type "sexp" :defaults text)) (read in))))
      (check (equal got expected)))))



(defun look (number) 
  (with-special-tags
    (let* ((text (make-pathname :directory '(:relative "testfiles") :name (format nil "test~3,'0d" number) :type "txt"))
	   (sexp (make-pathname :type "sexp" :defaults text)))
    (with-open-file (in text)
      (loop for line = (read-line in nil nil) while line do (format t "~a~%" line)))
    (when (probe-file sexp)
      (with-open-file (in sexp)
	(loop for line = (read-line in nil nil) while line do (format t "~a~%" line))))
    (parse-file text))))

(defun ok (number) 
  (with-special-tags
    (let* ((text (make-pathname :directory '(:relative "testfiles") :name (format nil "test~3,'0d" number) :type "txt"))
	   (sexp (make-pathname :type "sexp" :defaults text)))
      (with-open-file (out sexp :direction :output :if-exists :supersede)
	(prin1 (parse-file text) out)))))
