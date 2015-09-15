;;
;; Copyright (c) 2005, Gigamonkeys Consulting All rights reserved.
;;

;;; TODO: actually make use of environment. Should be able to move
;;; *escapes* and *xhtml* in there. Should also move *block-elements*,
;;; et al. into environment so we can define special operators to
;;; change their values at compile time.

(in-package :com.gigamonkeys.foo.html)

(defclass html (language)
  ()
  (:default-initargs
   :special-operator-symbol 'html-special-operator
    :macro-symbol 'html-macro
    :input-readtable (copy-readtable)
    :input-package (find-package :keyword)
    :output-file-type "html"))

(defparameter *html* (make-instance 'html))

(defparameter *block-element-by-default* t)

(defparameter *block-elements*
  '(:body :colgroup :dl :fieldset :form :head :html :map :noscript :object
    :ol :optgroup :pre :script :select :style :table :tbody :tfoot :thead
    :tr :ul))

(defparameter *paragraph-elements*
  '(:area :base :blockquote :br :button :caption :col :dd :div :dt :h1
    :h2 :h3 :h4 :h5 :h6 :hr :input :li :link :meta :option :p :param
    :td :textarea :th :title))

(defparameter *inline-elements*
  '(:a :abbr :acronym :address :b :bdo :big :cite :code :del :dfn :em
    :i :img :ins :kbd :label :legend :q :samp :small :span :strong :sub
    :sup :tt :var))

(defparameter *empty-elements*
  '(:area :base :br :col :hr :img :input :link :meta :param))

(defparameter *preserve-whitespace-elements* '(:pre :script :style))

(defparameter *xhtml* nil)

(defvar *escapes* ""
  "String containing characters that need to be escaped in
  output. When compiling, this variable is used only at compile
  time; the generated code is hard wired to escape certain
  characters in embedded values.")

(defparameter *element-escapes* "<>&")

(defparameter *attribute-escapes* "<>&\"'")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Public API

(defmacro with-html-output ((stream &key (pretty *pretty*)) &body body)
  `(let* ((*text-output* ,stream)
          (*pretty* ,pretty))
    ,@body))

(defmacro with-html-to-file ((file &key (pretty *pretty*)) &body body)
  (with-gensyms (stream)
    `(with-open-file (,stream ,file :direction :output :if-exists :supersede)
      (with-html-output (,stream :pretty ,pretty)
        ,@body))))

(defmacro in-html-style (syntax)
  (eval-when (:compile-toplevel :load-toplevel :execute)
    (case syntax
      (:html (setf *xhtml* nil))
      (:xhtml (setf *xhtml* t)))))

(defun emit-html (sexp) 
  (let ((*escapes* *element-escapes*))
    (process *html* (get-pretty-printer) sexp nil)))

(defun emit-xml (sexp) 
  (let ((*xhtml* t)) (emit-html sexp)))

(defmacro html (&whole whole &body body)
  (declare (ignore body))
  `(macrolet ((html (&body body) 
		    (let ((*escapes* *element-escapes*)) 
		      (codegen-text (sexp->ops *html* body nil) ,*pretty*))))
     ,@(if *pretty*
	   `((let ((*text-pretty-printer* (get-pretty-printer))) ,whole))
	   `(,whole))))

(defmacro xml (&whole whole &body body)
  (declare (ignore body))
  `(macrolet ((xml (&body body) 
		   (let ((*escapes* *element-escapes*)
			 (*xhtml* t))
		     (codegen-text (sexp->ops *html* body nil) ,*pretty*))))
     ,@(if *pretty*
	   `((let ((*text-pretty-printer* (get-pretty-printer))) ,whole))
	   `(,whole))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; String escaping

(defun escape-char (char)
  (case char
    (#\& "&amp;")
    (#\< "&lt;")
    (#\> "&gt;")
    (#\' "&apos;")
    (#\" "&quot;")
    (t (format nil "&#~d;" (char-code char)))))

(defun escape (in to-escape)
  (flet ((needs-escape-p (char) (find char to-escape)))
    (with-output-to-string (out)
      (loop for start = 0 then (1+ pos)
            for pos = (position-if #'needs-escape-p in :start start)
            do (write-sequence in out :start start :end pos)
            when pos do (write-sequence (escape-char (char in pos)) out)
            while pos))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Language implementation

(defmethod identifier ((language html) form)
  (let ((first (car form)))
    (cond
      ((symbolp first) first)
      ((and (consp first) (symbolp (car first))) (car first))
      (t (error "Malformed html-sexp form: ~s" form)))))

(defmethod sexp-form-p ((language html) form)
  (or (self-evaluating-p form) (cons-form-p form)))

(defmethod embeddable-value-form ((language html) form)
  `(escape (princ-to-string ,form) ,*escapes*))

(defmethod process-sexp ((language html) processor form environment)
  (declare (ignore environment))
  (if (self-evaluating-p form)
      ;; Since the form is self-evaluating, we can escape it at
      ;; compile time which allows it to be included in the static
      ;; output optimization.
      (raw-string processor (escape (princ-to-string form) *escapes*) t)
      (process-cons-sexp-html processor form)))

(defun cons-form-p (form &optional (test #'keywordp))
  (and (consp form)
       (or (funcall test (car form))
           (and (consp (car form)) (funcall test (caar form))))))

(defun parse-cons-form (sexp)
  (if (consp (first sexp))
    (parse-explicit-attributes-sexp sexp)
    (parse-implicit-attributes-sexp sexp)))

(defun parse-explicit-attributes-sexp (sexp)
  (destructuring-bind ((tag &rest attributes) &body body) sexp
    (values tag attributes body)))

(defun parse-implicit-attributes-sexp (sexp)
  (loop with tag = (first sexp)
     for rest on (rest sexp) by #'cddr
     while (and (keywordp (first rest)) (rest rest))
     when (second rest)
     collect (first rest) into attributes and
     collect (second rest) into attributes
     end
     finally (return (values tag attributes rest))))

(defun process-cons-sexp-html (processor form)
  (when (string= *escapes* *attribute-escapes*)
    (error "Can't use cons forms in attributes: ~a" form))
  (multiple-value-bind (tag attributes body) (parse-cons-form form)
    (emit-open-tag     processor tag body attributes)
    (emit-element-body processor tag body)
    (emit-close-tag    processor tag body)))

(defun emit-open-tag (processor tag body-p attributes)
  (when (or (paragraph-element-p tag) (block-element-p tag))
    (freshline processor))
  (raw-string processor (format nil "<~(~a~)" tag))
  (emit-attributes processor attributes)
  (raw-string processor (if (and *xhtml* (not body-p)) "/>" ">")))

(defun emit-attributes (processor attributes)
  (loop for (k v) on attributes by #'cddr do
       (raw-string processor (format nil " ~(~a~)='" k))
       (let ((*escapes* *attribute-escapes*))
         (process *html* processor (if (eql v t) (string-downcase k) v) nil))
       (raw-string processor "'")))

(defun emit-element-body (processor tag body)
  (when (block-element-p tag)
    (freshline processor)
    (indent processor))
  (when (preserve-whitespace-p tag) (toggle-indenting processor))
  (dolist (item body)  (process *html* processor item nil))
  (when (preserve-whitespace-p tag) (toggle-indenting processor))
  (when (block-element-p tag)
    (unindent processor)
    (freshline processor)))

(defun emit-close-tag (processor tag body-p)
  (unless (and (or *xhtml* (empty-element-p tag)) (not body-p))
    (raw-string processor (format nil "</~(~a~)>" tag)))
  (when (or (paragraph-element-p tag) (block-element-p tag))
    (freshline processor)))

(defun block-element-p (tag) (find tag *block-elements*))

(defun paragraph-element-p (tag) (find tag *paragraph-elements*))

(defun empty-element-p (tag) (find tag *empty-elements*))

(defun preserve-whitespace-p (tag) (find tag *preserve-whitespace-elements*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; File compiler implementation

(defmethod comment ((language html) text)
  (format nil "~&<!--~&~a~&-->" text))

(defmethod top-level-environment ((language html)) nil)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Macros

(defmacro define-html-macro (name (&rest parameters) &body body)
  (multiple-value-bind (attributes parameters)
      (parse-html-macro-lambda-list parameters)
    (if attributes
      (generate-macro-with-attributes name attributes parameters body)
      `(define-macro ,name html-macro (,@parameters) ,@body))))

(defun generate-macro-with-attributes (name attributes parameters body)
  (with-gensyms (form all tag tag-body)
    `(define-macro ,name html-macro (&whole ,form &body ,all)
       (declare (ignore ,all))
       (multiple-value-bind (,tag ,attributes ,tag-body) (parse-cons-form ,form)
	 (declare (ignore ,tag))
	 (destructuring-bind (,@parameters) ,tag-body
	   ,@body)))))

(defun parse-html-macro-lambda-list (args)
  (let ((attr-cons (member '&attributes args)))
    (values 
     (cadr attr-cons)
     (nconc (ldiff args attr-cons) (cddr attr-cons)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Special Forms

(defmacro define-html-special-operator (name (processor &rest other-parameters) &body body)
  `(define-special-operator ,name html-special-operator (,processor ,@other-parameters) ,@body))

(define-html-special-operator :print (processor form)
  (cond
    ((self-evaluating-p form)
     (warn "Redundant :print of self-evaluating form ~s" form)
     (process-sexp *html* processor form nil))
    (t
     (embed-value processor (embeddable-value-form *html* form)))))

(define-html-special-operator :format (processor &rest args)
  (if (every #'self-evaluating-p args)
    (process-sexp *html* processor (apply #'format nil args) nil)
    (embed-value processor (embeddable-value-form *html* `(format nil ,@args)))))

(define-html-special-operator :progn (processor &rest body)
  (loop for exp in body do (process *html* processor exp nil)))

(define-html-special-operator :noescape (processor &rest body)
  (let ((*escapes* nil))
    (loop for exp in body do (process *html* processor exp nil))))

(define-html-special-operator :escape (processor &rest body)
  (let ((*escapes* *element-escapes*))
    (loop for exp in body do (process *html* processor exp nil))))

(define-html-special-operator :xml (processor &rest body)
  (let ((*xhtml* t))
    (loop for exp in body do (process *html* processor exp nil))))

(define-html-special-operator :not-xml (processor &rest body)
  (let ((*xhtml* nil))
    (loop for exp in body do (process *html* processor exp nil))))

(define-html-special-operator :attribute (processor &rest body)
  (let ((*escapes* *attribute-escapes*))
    (loop for exp in body do (process *html* processor exp nil))))

(define-html-special-operator :newline (processor)
  (newline processor))

;; This can be used to embed, say, CSS in the HTML page.
(define-html-special-operator :with-language (processor (language) &body body)
  (let ((language (if (boundp language)
		      (symbol-value language)
		      language)))
    (loop for exp in body do (process language processor exp (top-level-environment language)))))