;; -*- mode: common-lisp; package: net.xml.generator -*-
;;
;; generalized xml generator
;;
;; copyright (c) 2000-2001 Franz Inc, Berkeley, CA  - All rights reserved.
;;
;; Right to copy, modify, and use this software for all purposes is hereby granted to all
;; interested parties.  Comments and feedback to <cl@franz.com>.

;; This readtable hack provides the a palatable syntax for xml generation by lisp code.
;; By using an explicit #\! marker for a xml generation, the reader macro allows the xml
;; namespace syntax to be separated from lisp packages.  It also allows lisp and xml forms
;; to coexist and nest arbitrarily without further syntactic markers.

(defpackage :net.xml.generator
  (:export with-xml-generation xml-write emit-lxml-as-xml *xml-readtable*))

(in-package :net.xml.generator)

;; The #\! character is a syntactic marker that the following form should emit an XML
;; element, The #\@ character reads the next two subforms and generates an attribute.
;; (The #\@ character idiom is suggested by both XSLT usage and Lisp backquote usage.)
;; Both of these can appear anywhere and can be freely interspersed around and inside
;; arbitrary Lisp forms.  This use of #\@ is culturally compatible with the XSL world.
;; It makes no sense for a #\! to appear inside another tag, but the #\@
;; character can be used in element content as a shorthand to princ the result of
;; executing the following form (most often a string constant) to the XML stream.

;;   !foo ==> <foo/>
;;   !(foo !(bar)) ==> <foo><bar/></foo>
;;   !((foo @id "31415") !(bar)) ==> <foo id="31415"><bar/></foo>
;;   !((foo !bar) !(bar)) ==> illegal
;;   !(time @"The current UT is " !(ut @(get-universal-time))) ==>
;;       <time>The current UT is <ut>3192471502</ut></time>
;;    but the @ before the literal string in the previous example is optional since the
;;    string is at top-level of the ! element body.
;;   !((foo @name "xy&z") !(bar)) ==> <foo name="xy&amp;z"><bar/></foo>

;; Arbitrary lisp code can appear anywhere inside an XML element form, as all attributes
;; and internal elements are flagged syntactically.

;; The #\@ read macro takes exactly two subforms.  The first is the attribute name and the
;; second is the attribute value.  A comma preceding the attribute name causes the name
;; form to be evaluated.

;; The generated XML-generation code uses the pretty printer.  This may seem strange since
;; pretty whitespace is not only useless in XML source, it also both slows the generation
;; speed and increases the length of the generated XML.  However, pretty printed XML with
;; proper indenting can greatly enhance human readability during debugging.  Once an
;; application goes into production the pretty printer can be turned off by binding
;; *print-pretty* nil.  (But some runtime cost still remains in the printer functions.)
;; Someday we may provide a read-time switch that generates XML-writing code without
;; testing for *print-pretty* at all and therefore completely avoids any residual overhead
;; of the pretty printing capability, but so far this residue appears quite small and
;; probably not worth addressing.

;; To do:

;; An important motivation behind this code is to handle XML namespaces more or less
;; automagically.  But at this time, namespace support is not yet implemented.

;; What about DTD generation?  Integrate with the parser.

;; This code can easily generate illegal XML if (for instance) illegal characters appear
;; in Lisp symbols used as XML element or attribute names.  This would be an application
;; error, not the fault of this module, but should this code check and signal error?
;; There would be a run time cost, of course.  Perhaps there should be two versions of
;; this code or a mode switch so the programmer can use the suspicious version during
;; development.  Could alse verify against a DTD at generation time, preventing user code
;; from generating invalid XML.

;; Fill in more inline documentation details and examples.

(defmacro with-xml-generation ((stream-var &key) &body body)
  ;; This is a place holder for solving the multiple-stream problem.
  `(macrolet ((xml-stream-variable () ',stream-var)
	      (xml-stream-lambda (&body lambda-body) `(lambda (,',stream-var) ,@lambda-body)))
     ;; This is necessary to work around a bug that ACL socket streams don't properly
     ;; support detection of column position.
     #+allegro (setf (slot-value ,stream-var 'excl::charpos) nil)
     ,@body))

;; Should this be exported?  Probably...
(defmacro xml-stream-variable ()
  (error "XML generation code must be lexically enclosed in a ~s macro."
	 'with-xml-generation))

(defun read-xml-tag (stream)
  (let ((evalp (eql (peek-char t stream) #\,)))
    ;; Need to munge namespaces.
    (if evalp
	(progn (read-char stream)
	       (read stream t nil t))
      (with-output-to-string (s)
	(loop while (xml-namechar-p (peek-char nil stream nil nil))
	    do (write-char (read-char stream) s))))))

(defvar *attribute-context* nil)

(defun xml-excl (stream char)
  (declare (ignore char))
  (unless (eql (peek-char t stream) #\()
    ;; Simple !foo empty element.
    (return-from xml-excl
      `(pprint-element (xml-stream-variable) ,(read-xml-tag stream) nil nil)))
  (read-char stream)			; Eat the open paren.
  (let (element-name attribute-continuation)
    ;; Check for !((foo @bar "..") ...) form with attributes.
    (cond ((eql (peek-char t stream) #\()
	   (read-char stream)		; Eat the open paren.
	   (setq element-name (read-xml-tag stream))
	   (let* ((*attribute-context* t)
		  (attribute-body (read-delimited-list #\) stream t)))
	     (when attribute-body
	       (setq attribute-continuation
		 `(xml-stream-lambda ,@attribute-body)))))
	  ;; Simple non-list element !(foo ...).
	  (t (setq element-name (read-xml-tag stream))))
    ;; Now process the body.
    `(pprint-element
      (xml-stream-variable)
      ,element-name
      ,attribute-continuation
      ,(let ((body (read-delimited-list #\) stream t)))
	 (when body
	   ;; Wrap any top-level body strings with an automatic xml-write, except that
	   ;; we'll write as xml-write-1 so that the printer can recognize it as top level
	   ;; and suppress the #\@ for print-read consistency.
	   `(xml-stream-lambda ,@(loop for form in body
				     collect (if (stringp form)
						 `(xml-write-1 ,form (xml-stream-variable))
					       form))))))))

(defun xml-at (stream char)
  (declare (ignore char))
  (if *attribute-context*
      (let* ((evalp (eql (peek-char t stream) #\,))
	     (name (if evalp (read stream) (read-xml-tag stream)))
	     (val (read stream)))
	`(write-xml-attribute (xml-stream-variable) ,name ,val))
    `(xml-write ,(read stream) (xml-stream-variable))))

(defun write-xml-attribute (stream attribute value)
  (write-char #\space stream)
  (when *print-pretty* (pprint-newline :fill stream))
  (princ attribute stream)
  (write-char #\= stream)
  (let ((val (if (stringp value)
		 value
	       (princ-to-string value))))
    ;; Now find and eliminate any appearances the three forbidden attval characters: &lt;
    ;; &amp; &quot;.  This could be both smarter and faster, but perhaps not both.  A
    ;; smarter version would be clever about choosing between &quot; or &apos;.  But this
    ;; would require traversing the string an extra time, or doing more bookkeeping.  It
    ;; might also be a lot more efficient to accumulate a string and then print it once,
    ;; avoiding individual writes to the stream.
    (loop for c across val
	initially (write-char #\" stream)
	do (case c
	     (#\< (write-string "&lt;" stream))
	     (#\& (write-string "&amp;" stream))
	     (#\" (write-string "&quot;" stream))
	     (t (write-char c stream)))
	finally (write-char #\" stream))))

#+unused
(defun attributize (value stream)
  ;; Ensure the attribute is a string.
  (let ((val (if (stringp value)
		 value
	       (princ-to-string value))))
    ;; Now find and eliminate any appearances the three forbidden attval characters: &lt;
    ;; &amp; &quot;.  This could be both smarter and faster, but perhaps not both.  A
    ;; smarter version would be clever about choosing between &quot; or &apos;.  But this
    ;; would require traversing the string an extra time, or doing more bookkeeping.  It
    ;; might also be a lot more efficient to accumulate a string and then print it once,
    ;; avoiding individual writes to the stream.
    (loop for c across val
	initially (write-char #\" stream)
	do (case c
	     (#\< (write-string "&lt;" stream))
	     (#\& (write-string "&amp;" stream))
	     (#\" (write-string "&quot;" stream))
	     (t (write-char c stream)))
	finally (write-char #\" stream))))

;; This princs an arbitrary Lisp value (typically a string) to the XML stream,
;; substituting the &lt; and &amp; entities.
(defun xml-write (value stream)
  (declare (optimize speed))
  ;; Ensure the data is a string.
  (let ((val (if (simple-string-p value)
		 value
	       (princ-to-string value))))
    ;; Find and eliminate any appearances the two forbidden character data characters:
    ;; &lt; &amp;.  This code could be faster, perhaps by accumulating a stream and then
    ;; making only a single write-string call.
    (loop for c across (the simple-string val)
	do (case c
	     (#\< (write-string "&lt;" stream))
	     (#\& (write-string "&amp;" stream))
	     (t (write-char c stream))))))

;; This macro is equivalent to xml-write, except that the pretty printer recognizes it and
;; suppresses the #\@ character.
(defmacro xml-write-1 (&rest args) `(xml-write ,@args))

;; This version is clearer, but slower when not pretty printing.
#+never
(defun pprint-element (xml-stream tag-name attribute-continuation body-continuation)
  (pprint-newline :linear xml-stream)
  (pprint-logical-block (xml-stream nil) ; l-b for the element start/content/end
    (pprint-logical-block (xml-stream nil ; l-b for the element tag
				      :prefix "<"
				      :suffix (if body-continuation ">" " />"))
      (write-string tag-name xml-stream)
      (when attribute-continuation
	(pprint-indent :block 4 xml-stream) ; parameterize?
	(funcall attribute-continuation xml-stream)))
    (when body-continuation
      (pprint-indent :block 2 xml-stream) ; parameterize?
      (pprint-newline :linear xml-stream)
      (funcall body-continuation xml-stream)
      (pprint-indent :block 0 xml-stream)
      (format xml-stream "~_</~a>" tag-name)))
  (values))

(defun pprint-element (xml-stream tag-name attribute-continuation body-continuation)
  (cond (*print-pretty*
	 (pprint-newline :linear xml-stream)
	 (pprint-logical-block (xml-stream nil) ; l-b for the element start/content/end
	   (pprint-logical-block (xml-stream nil ; l-b for the element tag
					     :prefix "<"
					     ;; Some HTML parsers such as Nescape 4 parse
					     ;; incorrectly if an empty element doesn't
					     ;; have a space before the closing slash.
					     :suffix (if body-continuation ">" " />"))
	     (write-string tag-name xml-stream)
	     (when attribute-continuation
	       (pprint-indent :block 4 xml-stream) ; parameterize?
	       (funcall attribute-continuation xml-stream)))
	   (when body-continuation
	     (pprint-indent :block 2 xml-stream) ; parameterize?
	     (funcall body-continuation xml-stream)
	     (pprint-indent :block 0 xml-stream)
	     (format xml-stream "~_</~a>" tag-name))))
	(t
	 (write-char #\< xml-stream)
	 (write-string tag-name xml-stream)
	 (when attribute-continuation
	   (funcall attribute-continuation xml-stream))
	 (cond (body-continuation
		(write-char #\> xml-stream)
		(funcall body-continuation xml-stream)
		(write-char #\< xml-stream)
		(write-char #\/ xml-stream)
		(write-string tag-name xml-stream)
		(write-char #\> xml-stream))
	       ;; See above re space.
	       (t (write-string " />" xml-stream)))))
  (values))

;; Cheesy!  This depends on the implementation of ACL readtables, that all constituent
;; characters happen to have the same function as their macro-character dispatch.  It does
;; approximately the right thing (accepting some invalid characters) but should be
;; replaced with a serious XML-compliant definition.
(defun xml-namechar-p (char)
  (and char				; Handle eof elegantly.
       (eql (get-macro-character char)
	    (load-time-value (get-macro-character #\A)))))

;;;
;;; Pretty printing interface for reconstructing ! forms, similar to the backquote
;;; printer.
;;;

;; A pprint dispatch that can reconstruct the #\! source form.
(defun print-pprint-element (stream form)
  ;; Must bulletproof all this destructuring!!!
  (destructuring-bind (op stream-subform tag-name attribute-continuation body-continuation)
      form
    (declare (ignore op stream-subform))
    (cond (attribute-continuation
	   (destructuring-bind (l &body attribute-body) attribute-continuation
	     (declare (ignore l))
	     (if body-continuation
		 (destructuring-bind (l &body body-body) body-continuation
		   (declare (ignore l))
		   ;; Miser newline before first attr subform, linear between subforms.
		   (format stream "!~:@<~:<~a ~@_~:i~s~^~@{ ~_~s~}~:>~2I~{ ~_~s~}~:>"
			   (cons tag-name attribute-body)
			   body-body))
	       (format stream "!~:@<~:<~a~@{ ~_~s~}~:>~:>"
		       (cons tag-name attribute-body)))))
	  (body-continuation
	   (destructuring-bind (l &body body) body-continuation
	     (declare (ignore l))
	     (format stream "!~:@<~a~{ ~_~s~}~:>" tag-name body)))
	  (t (write-char #\! stream)
	     (princ tag-name stream)))))

(defun print-write-xml-attribute (stream form)
  ;; Must bulletproof all this destructuring!!!
  (destructuring-bind (op stream-subform attribute-form value-form)
      form
    (declare (ignore op stream-subform))
    ;; The ~a for the attribute name will need to be changed to ~s when attribute
    ;; names are read as symbols with packages.
    (format stream "~@<@~a~2I ~_~s~:>" attribute-form value-form)))

(defun print-xml-write (stream form)
  ;; Must bulletproof all this destructuring!!!
  (destructuring-bind (op arg stream-subform)
      form
    (declare (ignore stream-subform))
    (unless (eql op 'xml-write-1) (write-char #\@ stream))
    (write arg :stream stream)))

;; These pprint-dispatch entries assume that the :xml readtable will be in effect if the
;; printed forms are reread.  This is no different than what is done for backquote, except
;; that backquote is defined in the standard readtable.

(set-pprint-dispatch '(cons (member pprint-element)) #'print-pprint-element)
(set-pprint-dispatch '(cons (member xml-write)) #'print-xml-write)
(set-pprint-dispatch '(cons (member xml-write-1)) #'print-xml-write)
(set-pprint-dispatch '(cons (member write-xml-attribute)) #'print-write-xml-attribute)

;;;
;;; Define a readtable that handles the ! and @ chars.  Interface with the ACL
;;; named-readtable facility so Emacs and IDE tools can find the right readtable.
;;;

#+:ALLEGRO
(defparameter *xml-readtable*
    (let ((rt (or (excl:named-readtable :xml)
		  (setf (excl:named-readtable :xml) (copy-readtable)))))
      (set-macro-character #\! #'xml-excl nil rt)
      (set-macro-character #\@ #'xml-at nil rt)))

#+:LISPWORKS
(defparameter *xml-readtable*
    (let ((rt (copy-readtable)))
      (set-macro-character #\! #'xml-excl nil rt)
      (set-macro-character #\@ #'xml-at nil rt)))

;;;
;;; Support for printing lxml trees.
;;;

(defun emit-lxml-as-xml (stream lxml)
  (destructuring-bind (tag &rest contents) lxml
    (let ((body-continuation (and contents
				  (lambda (stream)
				    (loop for content in contents
					do (cond ((stringp content)
						  (xml-write content stream))
						 ((atom content)
						  (xml-write content stream))
						 (t (emit-lxml-as-xml stream content))))))))
      (if (consp tag)
	  (pprint-element stream
			  (string (car tag))
			  (lambda (stream)
			    (loop for (attribute value) on (cdr tag) by #'cddr
				do ;; (write-char #\space stream)
				   ;; (when *print-pretty* (pprint-newline :linear stream))
				   (write-xml-attribute stream attribute value)))
			  body-continuation)
	(pprint-element stream (string tag) nil body-continuation)))))

(provide :net.xml.generator)
