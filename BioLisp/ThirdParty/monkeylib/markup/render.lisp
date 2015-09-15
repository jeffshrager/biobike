;;
;; Copyright (c) 2005, Gigamonkeys Consulting All rights reserved.
;;

(in-package :com.gigamonkeys.markup)

(defvar *preprocessors* nil)

(defun render (output-type input-file 
	       &optional (output-file (make-pathname :type (string-downcase output-type) :defaults input-file)))
  (let ((sexp (parse-file input-file)))
    (loop for preprocessor in *preprocessors* do
	 (setf sexp (funcall preprocessor sexp)))
    (render-as output-type (strip-invisible sexp) output-file)
    (truename output-file)))

(defmacro with-tag-translations ((&rest translations) &body body)
  `(let ((*tag-translations* (append ',translations *tag-translations*)))
     ,@body))

(defun render-sexp (output-type sexp output-file)
  (render-as output-type (strip-invisible sexp) output-file))

(defgeneric render-as (output-type sexp output-file))

(defun extract-notes (book)
  (let ((note-counter 0)
        (notes nil))
    (labels ((walk (sexp)
               (cond
                 ((atom sexp) sexp)
                 ((eql (first sexp) ':note)
                  (push sexp notes)
                  (list ':note-ref (incf note-counter)))
                 (t (mapcar #'walk sexp)))))
      (values (mapcar #'walk book) (nreverse notes)))))

(defun invisible-p (p)
  "Elements that are not output at all."
  (and (consp p)
       (or (member (first p) *invisible*)
           (and (stringp (second p))
                (char= (char (second p) 0) #\.)
                (alpha-char-p (char (second p) 1))))))

(defun strip-invisible (sexp)
  (if (atom sexp)
    sexp
    (mapcar #'strip-invisible (remove-if #'invisible-p sexp))))


