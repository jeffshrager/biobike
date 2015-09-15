;;
;; Copyright (c) 2005, Gigamonkeys Consulting All rights reserved.
;;

(in-package :com.gigamonkeys.markup)

;;;; Render Markup data back into Markup's text format.

(defmethod render-as ((type (eql :markup)) sexp file)
  (with-open-file (out file :direction :output :if-exists :supersede)
    (dolist (e sexp) (emit-element out e))))

(defun emit-element (stream element)
  (let ((tag (first element)))
    (when (consp tag)
      (when (rest tag) (warn "Ignoring attributes: ~a of tag ~a" (rest tag) (first tag)))
      (setf tag (first tag))
      (setf element (cons tag (rest element)))
      (when (consp tag) (warn "Tag still a cons: ~a" tag)))
    (cond 
      ((member tag '(:h1 :h2 :h3 :h4 :h5 :h6 :h7 :h8 :h9))
       (emit-outline-paragraph stream element))
      ((member tag '(:bullets :numbered))
       (emit-list stream element))
      ((paragraph-tag-p tag)
       (emit-paragraph stream element))
      ((subdocument-tag-p tag)
       (emit-sub-document stream element))
      (t (emit-span stream element)))))

(defun emit-outline-paragraph (stream element)
  (let ((level (parse-integer (symbol-name (first element)) :start 1)))
    (fresh-line stream)
    (loop repeat level do (write-char #\* stream))
    (write-char #\Space stream)
    (dolist (e (rest element)) (emit-text stream e))
    (fresh-line stream)
    (terpri stream)))

(defun emit-list (stream element)
  (destructuring-bind (tag &rest items) element
    (let ((prefix (case tag (:bullets "@") (:numbered "#"))))
      (dolist (item items)
	(fresh-line stream)
	(format stream "  ~a " prefix)
	(dolist (e (rest item)) (emit-text stream e))
	(fresh-line stream)
	(terpri stream)))))

(defun emit-paragraph (stream paragraph)
  (case (first paragraph)
    ((:p :blockquote :example) 
     (emit-implicit-paragraph stream paragraph))
    (t (emit-regular-paragraph stream paragraph))))

(defun emit-implicit-paragraph (stream paragraph)
  (fresh-line stream)
  (let ((prefix
         (case (first paragraph)
           ((:p) "")
           ((:blockquote) "  ")
           ((:example) "    "))))
    (format stream "~a" prefix)
    (dolist (e (rest paragraph)) (emit-text stream e prefix))
    (fresh-line stream)
    (terpri stream)))

(defun emit-regular-paragraph (stream paragraph)
  (fresh-line stream)
  (emit-tagged-group stream paragraph)
  (fresh-line stream)
  (terpri stream))

(defun emit-text (stream text &optional (prefix ""))
  (cond
    ((stringp text) (format stream "~a" text))
    ((char= #\Newline text) (format stream "~%~a" prefix))
    ((member text '(#\\ #\{ #\}) :test #'char=) (format stream "\\~c" text))
    (t (emit-element stream text))))

(defun emit-span (stream span)
  (emit-tagged-group stream span))

(defun emit-tagged-group (stream group)
  (destructuring-bind (tag &body body) group
    (when (consp tag)
      (when (rest tag) (warn "Ignoring attributes: ~a of tag ~a" (rest tag) (first tag)))
      (setf tag (first tag))
      (when (consp tag) (warn "Tag still a cons: ~a" tag)))
    (format stream "\\~(~a~){"  tag)
    (when (eql tag :bullets) (format stream "~2%"))
    (dolist (e body) (emit-text stream e))
    (format stream "}")))

(defun emit-sub-document (stream subdoc)
  (format stream "\\~(~a~){" (first subdoc))
  (if (= 1 (length (rest subdoc)))
      (dolist (e (rest (first (rest subdoc)))) (emit-text stream e))
      (dolist (e (rest subdoc)) (emit-text stream e)))
  (format stream "}"))



