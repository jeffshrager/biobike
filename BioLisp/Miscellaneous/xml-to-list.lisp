(in-package :cllib)

;;; +============================================================================+
;;; | Copyright (c) 2001, 2002, 2003, 2004 JP Massar, Jeff Shrager, Mike Travers |
;;; |                                                                            |
;;; | Permission is hereby granted, free of charge, to any person obtaining      |
;;; | a copy of this software and associated documentation files (the            |
;;; | "Software"), to deal in the Software without restriction, including        |
;;; | without limitation the rights to use, copy, modify, merge, publish,        |
;;; | distribute, sublicense, and/or sell copies of the Software, and to         |
;;; | permit persons to whom the Software is furnished to do so, subject to      |
;;; | the following conditions:                                                  |
;;; |                                                                            |
;;; | The above copyright notice and this permission notice shall be included    |
;;; | in all copies or substantial portions of the Software.                     |
;;; |                                                                            |
;;; | THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,            |
;;; | EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF         |
;;; | MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.     |
;;; | IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY       |
;;; | CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,       |
;;; | TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE          |
;;; | SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.                     |
;;; +============================================================================+


;;;; *****>  THIS FILE BELONGS IN THE TOPLEVEL CLOCC DIRECTORY  <*****
;;;;
;;;; It is an addendum to the clocc;src;cllib;xml.lisp file which
;;;; gets loaded by the LOAD-XML.LISP file.

;;; Turn each STRUCT into a list, such that the first element
;;; of the list is the structure name, and the other elements
;;; are lists such that the first element of the list is the
;;; slot name and the second element of the list is the value
;;; of the slot, recursively changed into list form.

(defun xml-obj-to-lisp-form (obj)
  (flet ((make-list-from-xml-struct
	     (struct-name slot-names slot-values)
	   (cons struct-name
		 (mapcar 
		  #'(lambda (name value)
		      (list name (xml-obj-to-lisp-form value)))
		  slot-names 
		  slot-values
		  ))))
    (cond
     ((listp obj) (mapcar #'xml-obj-to-lisp-form obj))
     ;; An XML-OBJ includes an XML-TAG and also has a DATA slot.
     ;; Its conc-name is XMLO-.
     ((typep obj 'cllib:xml-obj)
      (make-list-from-xml-struct
       'cllib::xml-obj
       '(cllib::name cllib::args cllib::data)
       (list (xmlo-name obj) (xmlo-args obj) (xmlo-data obj))))
     ;; An XML-TAG has NAME and ARGS slots.
     ;; Its conc-name is XMLT-.
     ((typep obj 'cllib::xml-tag)
      (make-list-from-xml-struct
       'cllib::xml-tag
       '(cllib::name cllib::args)
       (list (xmlt-name obj) (xmlt-args obj))))
     ;; An XML-DECL just includes an XML-TAG and nothing else
     ((typep obj 'cllib::xml-decl)
      (make-list-from-xml-struct
       'cllib::xml-decl
       '(cllib::name cllib::args)
       (list (xml-decl-name obj) (xml-decl-args obj))))
     ;; An XML-NAME has LN (local-name) and NS (namespace) slots
     ;; Its conc-name is XMLN-.
     ((typep obj 'cllib::xml-name)
      (make-list-from-xml-struct
       'cllib::xml-name
       '(cllib::ln cllib::ns)
       (list (xmln-ln obj) (xmln-ns obj))))
     ;; An XML-COMMENT has a DATA slot, and that is all.
     ((typep obj 'cllib::xml-comment)
      (make-list-from-xml-struct
       'cllib::xml-comment
       '(cllib::data)
       (list (cllib::xml-comment-data obj))))
     ;; An XML-MISC has TYPE and DATA slots.
     ((typep obj 'cllib::xml-misc)
      (make-list-from-xml-struct
       'cllib::xml-misc
       '(cllib::type cllib::data)
       (list (xml-misc-type obj) (xml-misc-data obj))))
     ;; An XML-NAMESPACE has URI, PRE (?) and NHT (hash table) slots.
     ;; It's conc-name is XMLNS-.
     ((typep obj 'cllib:xml-namespace)
      (make-list-from-xml-struct
       'cllib::xml-namespace
       '(cllib::uri cllib::pre cllib::nht)
       (list (xmlns-uri obj) (xmlns-pre obj) (xmlns-nht obj))))
     (t obj)
     )))
    
