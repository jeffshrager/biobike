;;
;; Copyright (c) 2005, Gigamonkeys Consulting All rights reserved.
;;

(in-package :com.gigamonkeys.markup)

(defparameter *render-flavor* t)

(defparameter *tag-translations* `((:example     . :pre)
				   (:sidebarhead . (:div :class "sidebarhead"))
				   (:sidebar     . (:div :class "sidebar"))
				   (:note        . (:div :class "note"))
				   (:note-ref    . :sup)
				   (:bullets     . :ul)
				   (:numbered    . :ol)
				   (:item        . :li)
				   (:url         . htmlize-url)
				   (:link        . htmlize-link)))

(defun htmlize-url (tag body)
  (declare (ignore tag))
  `(:a :href ,@body ,@body))

(defun htmlize-link (tag body)
  (declare (ignore tag))
  (destructuring-bind ((hreftag href) (texttag &rest text)) body
    (declare (ignore hreftag texttag))
    `(:a :href ,href ,@text)))

(defmethod render-as ((type (eql :html)) sexp file)
  (with-html-to-file (file)
    (emit-html (make-foo-html (first (encode-characters sexp))))))

(defun make-foo-html (sexp)
  (let ((stylesheet (if (typep com.gigamonkeys.foo::*text-output* 'file-stream)
			(pathname-name com.gigamonkeys.foo::*text-output*)
			"style")))
    
    (multiple-value-bind (body notes) (extract-notes sexp)
      `(:html
	 (:head 
	  (:title ,(find-first-h1 sexp))
	  (:link :rel "stylesheet" :type "text/css" :href (:format "~a.css" ,stylesheet)))
       (:body
	,@(translate-tags body)
	,@(when notes
		`((:hr)
		  ,@(loop for note in (translate-tags notes) 
		       for number from 1 
			 do (push `(:sup ,(format nil "~d. " number)) (cdadr note))
			 collect note))))))))
		  
(defun find-first-h1 (parse)
  (labels ((walk (thing)
	     (when (consp thing)
	       (destructuring-bind (tag &rest body) thing
		 (if (eql tag :h1)
		   (return-from find-first-h1 (car body))
		   (dolist (child body)
		     (walk child)))))))
    (dolist (thing parse)
      (walk thing))))

(defun translate-tag (element)
  (if (atom element) 
      element
      (destructuring-bind (tag &rest body) element
	(translate-element tag body *render-flavor*))))

(defmethod translate-element :around ((tag (eql :h1)) (body t) (render-flavor (eql :with-icon)))
  `(:progn (:img :style "float: right;" :src "lisp-lizard.png")
	   ,(call-next-method)))

(defgeneric translate-element (tag body render-flavor))

(defmethod translate-element (tag body (render-flavor t))
  (let ((translation (cdr (assoc tag *tag-translations*))))
    (etypecase translation
      (null
       (cons tag (translate-tags body)))
      (keyword
       (cons translation (translate-tags body)))
      (cons
       (cons translation (translate-tags body)))
      ((or symbol function)
       (multiple-value-bind (translation recursive-p)
	   (funcall translation tag body)
	 (if recursive-p
	     (translate-tag translation)
	     translation))))))

(defun translate-tags (list)
  (mapcar #'translate-tag list))


(defun replace-characters (string)
  (loop for start = 0 then (+ end (length "--"))
     for end = (search "--" string :start2 start)
     collect (subseq string start end)
     while end collect '(:character :mdash)))

(defun encode-characters (tree)
  (cond
    ((stringp tree) (replace-characters tree))
    ((consp tree) (list (mapcan #'encode-characters tree)))
    (t (list tree))))

       
     


