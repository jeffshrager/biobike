;;
;; Copyright (c) 2005, Gigamonkeys Consulting All rights reserved.
;;

(in-package :com.gigamonkeys.foo.css)

(defclass css (language)
  ()
  (:default-initargs
   :special-operator-symbol 'css-special-operator
    :macro-symbol 'css-macro
    :input-readtable (copy-readtable)
    :input-package (find-package :keyword)
    :output-file-type "css"))

(defparameter *css* (make-instance 'css))

(defun compile-css (input &key (output (make-pathname :type "css" :defaults input)))
  (assert (not (equal (pathname input) (pathname output))))
  (with-open-file (in input)
    (with-open-file (*text-output* output :direction :output :if-exists :supersede)
      (format *text-output* "/* Generated at ~a from ~a. */~2%" (format-iso-8601-time (get-universal-time)) (truename in))
      (loop for form = (read in nil in)
	 while (not (eql form in)) do
	   (emit-css form)))))

(defun emit-css (sexp) 
  (process *css* (get-pretty-printer) sexp nil))

(defmacro css (&whole whole &body body)
  (declare (ignore body))
  `(macrolet ((css (&body body) 
		(codegen-text (sexp->ops *css* body nil) ,*pretty*)))
     ,@(if *pretty*
	   `((let ((*text-pretty-printer* (get-pretty-printer))) ,whole))
	   `(,whole))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Language implementation


(defmethod identifier ((language css) form)
  (when (eql (car form) :import) :import))

(defmethod sexp-form-p ((language css) form)
  (or (self-evaluating-p form) (consp form)))

(defmethod embeddable-value-form ((language css) form)
  `(string-downcase (princ-to-string ,form)))

(defmethod process-sexp ((language css) processor form environment)
  (declare (ignore environment))
  (cond
    ((self-evaluating-p form)
     (raw-string processor (string-downcase (princ-to-string form)) t))
    ((eql (first form) :import)
     (emit-css-import processor form))
    (t 
     (process-non-import-css processor form))))

(defun emit-css-import (processor sexp)
  (let ((url (second sexp)))
    (freshline processor)
    (raw-string processor "@import ")
    (cond
      ((consp url)
       (raw-string processor "url(")
       (raw-string processor (second url))
       (raw-string processor ")"))
      (t (raw-string processor (format nil "\"~a\"" url))))
    (raw-string processor ";")))

(defun process-non-import-css (processor sexp)
  (destructuring-bind (selector &rest attributes) sexp
    (freshline processor)
    (emit-css-selector processor selector)
    (freshline processor)
    (raw-string processor "{")
    (indent processor)
    (freshline processor)
    (loop for (k v) on attributes by #'cddr do
         (process-css-key-or-value processor k)
         (raw-string processor ": ")
         (process-css-key-or-value processor v)
         (raw-string processor ";")
         (freshline processor))
    (unindent processor)
    (freshline processor)
    (raw-string processor "}")
    (freshline processor)))

(defun emit-css-selector (processor selector)
  (cond
    ((atom selector)
     (raw-string processor (string-downcase (string selector))))
    ((and (consp selector) (member (first selector) '(or :or and :and adjacent :adjacent > :>)))
     (loop with separator = (case (first selector) ((or :or) ", ") ((and :and) " ") ((adjacent :adjacent) " + ") ((> :>) " > "))
        for (x . rest) on (rest selector)
        do (emit-css-selector processor x)
        when rest do (raw-string processor separator)))
    (t
     (multiple-value-bind (tag class pseudo-class id) (parse-selector selector)
       (when tag
         (raw-string processor (string tag)))
       (when class
         (raw-string processor (format nil ".~(~a~)" class)))
       (when pseudo-class
         (raw-string processor (format nil ":~(~a~)" pseudo-class)))
       (when id
         (raw-string processor (format nil "#~(~a~)" id)))))))

(defun parse-selector (selector)
  (if (member (first selector) '(:class :pseudo-class :id))
    (destructuring-bind (&key class pseudo-class id) selector
      (values nil class pseudo-class id))
    (destructuring-bind (tag &key class pseudo-class id) selector
      (values tag class pseudo-class id))))

(defun process-css-key-or-value (processor form)
  (if (keywordp form)
    (raw-string processor (string-downcase form))
    (process *css* processor form nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; File compiler implementation

(defmethod comment ((language css) text)
  (format nil "~&/*~&~a~&*/" text))

(defmethod top-level-environment ((language css)) nil)
