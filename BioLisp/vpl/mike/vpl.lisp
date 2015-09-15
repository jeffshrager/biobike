;;;; -*- mode: Lisp; Syntax: Common-Lisp; Package: bio ; -*-

(in-package :bio)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (use-package :clos*))

;;; +=========================================================================+
;;; | Copyright (c) 2005, 2006 Mike Travers                                   |
;;; |                                                                         |
;;; | Permission is hereby granted, free of charge, to any person obtaining   |
;;; | a copy of this software and associated documentation files (the         |
;;; | "Software"), to deal in the Software without restriction, including     |
;;; | without limitation the rights to use, copy, modify, merge, publish,     |
;;; | distribute, sublicense, and/or sell copies of the Software, and to      |
;;; | permit persons to whom the Software is furnished to do so, subject to   |
;;; | the following conditions:                                               |
;;; |                                                                         |
;;; | The above copyright notice and this permission notice shall be included |
;;; | in all copies or substantial portions of the Software.                  |
;;; |                                                                         |
;;; | THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,         |
;;; | EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF      |
;;; | MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  |
;;; | IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY    |
;;; | CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,    |
;;; | TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE       |
;;; | SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.                  |
;;; +=========================================================================+

;;; Author: Mike Travers

;;; Block type database

(defclass* block-type ()
  (name
   color)
  (:readable-instance-variables name color)
  (:initable-instance-variables name color))

(defclass* simple-block-type (block-type)
  ((generating-specs nil))
  (:readable-instance-variables generating-specs))

(defvar *block-types* '())

(defmethod block-type-color ((btype symbol))
  (assert (get btype :block-type))
  (block-type-color (get btype :block-type)))

(defun define-block-type (name color &optional (block-class 'simple-block-type))
  (pushnew name *block-types*)
  (setf (get name :block-type) 
    (make-instance block-class :name name :color color)))

;;; A block spec
(defclass* vblock-spec ()
  (form
   btype
   body)
  :initable-instance-variables
  (:readable-instance-variables form body btype))

(defmethod* vblock-spec-args ((spec vblock-spec))
  (loop for item in form
      unless (stringp item)
	     collect (if (symbolp item) item (car item))))

(defmacro defblock (type form &body body)
  `(let ((btype (get ',type :block-type)))
     (let ((spec (make-instance 'vblock-spec :btype btype :form ',form :body ',(cons 'progn body))))
       (add-spec btype spec)
       ',form)))

(defmethod* add-spec ((btype simple-block-type) spec)
  (push spec generating-specs))



;;; Actual block defs (in future, mostly generated from DEFINEs)

(define-block-type 'gene "#fbb3ad")
(define-block-type 'string "#7823AB")
(define-block-type 'sequence "#aa0066")
(define-block-type 'predicate "#7733cc")
(define-block-type 'number "#2299aa")
(define-block-type 'void "#FFFFFF")	;dunno about this

(defblock void ("define" string "as" anything)
  (setq (intern (string-upcase string))
    anything))

;;; for maybe-mapping over args...only works for one variable at present
(defmacro mappy (vars &body body)
  `(if (listp ,(car vars))
       (mapcar #'(lambda ,vars ,@body) ,(car vars))
     ,@body))


(defblock gene ("ortholog of" gene "in" organism)
  (mappy (gene)
	 (two-way-ortholog-of gene organism .1)))

;;; ok, lets assume set-singleton equivalency
(defblock gene ("genes of" organism)
  (slotv organism #$genes))

;;; random for debugging...
(defblock gene ("first" number "of" gene)
  (subseq gene 0 number))

;;; Reduce a set to a single item...
(defblock gene ("first" gene)
  (car gene))

(defblock sequence ("sequence of" gene)
  (mappy (gene)
	 (extract-gene-sequence gene)))

(defblock sequence ("sequence upstream from" gene "of size" number) ; "with length" number
  (mappy (gene)
	 ;; not sure this is right
	 (sequence-upstream-from gene number)))

(defblock predicate ("not" predicate)
  (not predicate))

(defblock predicate ("does not exist" anything)
  (error "not yet"))

(defblock predicate ((a number) ">" (b number))
  (> a b))

;;; debugging, duh
(defblock predicate (number "is even")
  (evenp number))

(defblock predicate (sequence "contains" (subsequence sequence))
  (mappy (sequence)
	 (search subsequence sequence)))

(defblock number ("square of" number)
  (* number number))

(defblock number ("Microarray ratio of" gene "in table" string)
  (error "not yet"))					;unless table is data object

;;; +++ could punt these now that we have typein
(defblock number ("1")
  1)

(defblock sequence ("TTA")
  "TTA")

(defblock sequence ("TTATTA")
  "TTATTA")

;;; this obviously wants to work on anything, not just genes...
(defblock gene ("select from" gene "where" predicate)
  (mappy (gene)
	 (remove-if-not predicate gene)))

(defblock gene ("gene")
  gene)

'(define-block-form 'anything
  `("set" (variable string) to (value anything)))

'(define-block-form 'anything
  `("loop for" (variable string) "in" (set anything) :newline
     "do" (command anything)
     "when" predicate
     "collect" (value anything)))

;;; not working yet...and this is essentially all of querying, if done fully
'(define-block-form 'any
    '("Subset of" (set set) "with" (slot slot) ))

;;; we may or may not want to do this, depending on how close to Lisp we are trying to be...
(defun trim-parens (string)
  (subseq string 1 (1- (length string))))

(defmethod block-type-menu-spec ((block-type simple-block-type))
  (cons '("type something" nil)		;+++ better text...
	(mapcar #'(lambda (spec)
		    (let ((form (vblock-spec-form spec)))
		      `(,(trim-parens (princ-to-string form)) ,(tagname spec))))
		(simple-block-type-generating-specs block-type))))

(defun print-dictionary ()
  (dolist (typesym *block-types*)
    (print typesym)
    (let ((btype (get typesym :block-type)))
      (if (typep btype 'simple-block-type)
	  (dolist (spec (simple-block-type-generating-specs btype))
	    (format t "~%  ~S" (vblock-spec-form spec)))
	(print "not a simple type")
	))))



(defmethod xml-menu ((btype symbol) stream)
  (xml-menu (get btype :block-type) stream))

(defmethod xml-menu ((btype block-type) stream)
  (net.xml.generator:emit-lxml-as-xml 
   stream
   `((|menu| |absolutePosition| "auto" |mode| "popup" |maxItems| "8" |globalCss| "contextMenu" |globalSecondCss|
	     "contextMenu" |globalTextCss| "contextMenuItem")
     ,@(mapcar #'(lambda (spec)
		   `((|MenuItem| |name| ,(car spec)  |id| ,(cadr spec))))
	       (block-type-menu-spec btype)))))

;;; URL has form /vpl/menudata/?type=ORGANISM
(publish-prefix 
 :prefix "/vpl/menudata/"
 :function #'(lambda (req ent)
	       (let* ((input (request-query req))
		      (typestring (url-parameter-value :type input))
		      (type (intern typestring (find-package "BIO"))))
		 (with-http-response (req ent :content-type "text/xml")
		   (with-http-body (req ent)
		     (if (and type (get type :block-type))
			 (xml-menu type *html-stream*)
		       (format *html-stream* "Can't find type ~A" typestring )))))))

;;; Mechanism for assigning names.  GC is problematic, making this table per-session woudl help a bit.
(defvar *tagname->object-ht* (make-hash-table :test #'equal))
(defvar *object->tagname-ht* (make-hash-table :test #'equal))
(defvar *tag-counter* 0)
(defun tagname (object)
  (or (gethash object *object->tagname-ht* nil)
      (let ((ntag (format nil "~A~A" (type-of object) (incf *tag-counter*))))
	(if (listp (type-of object)) (error "Can't tag objects of type ~A" (type-of object)))
	(setf (gethash object *object->tagname-ht*) ntag)
	(setf (gethash ntag *tagname->object-ht*) object)
	ntag)))

(defun object-named (tagname)
  (gethash tagname *tagname->object-ht* nil))

;;; debugging
(defun all-objects ()
  (mapcar #'cadr (ht-contents *tagname->object-ht*)))

(defmethod make-new-block ((block-type symbol) id)
  (make-new-block (get block-type :block-type) id))

(defmethod make-new-block ((block-type simple-block-type) id)
  (make-instance 'vblock :btype block-type :spec (object-named id)))

;;; NIL means a string block...not sure I like this very much.
(defmethod make-new-block ((block-type simple-block-type) (id null))
  (make-instance 'string-vblock :btype block-type))

;;; The back end of the Ajaxy part of all this...send a new block def on request
(defvar *newblock* nil)
(publish-prefix 
 :prefix "/vpl/newblock"
 :function #'(lambda (req ent)
	       (let* ((input (request-query req))
		      (id (url-parameter-value :id input))
		      ;; was hole-named
		      (forblock (object-named (url-parameter-value :for input)))
		      (newblock (make-new-block (vblock-btype forblock) id))) 
		 (setq *newblock* newblock)
		 (with-http-response (req ent :content-type "text/xml")
		   (with-http-body (req ent)
		     (fill-hole forblock newblock)
		     (htmlize-vblock newblock))))))


;;; Actual blocks

#|
A vblock is an actual instantiated block, with possibly some filled-in arguments.
Would be BLOCK but that is a special name which causes problems.
|#

(defclass* vblock ()
  (btype
   spec
   (argmap (make-hash-table))
   (parent nil)
   (argname nil)
   )
  (:initable-instance-variables btype spec argname)
  (:readable-instance-variables btype id spec argname)
  (:writable-instance-variables parent argname parent)
  )

(defmethod* vblock-form ((vblock vblock))
  (vblock-spec-form spec))

(defmethod* vblock-structure ((vblock vblock)) ;+++ temp, this should fill in holes
  (mapcar #'(lambda (elt)
	      (cond ((stringp elt) elt)
		    ((symbolp elt)
		     (if (gethash elt argmap)
			 (vblock-structure (gethash elt argmap))
		       nil)) ))
	  (vblock-form vblock)))

(defmethod* print-tag ((vblock vblock) stream)
  (if (tagname vblock)
      (format stream "[~A]" (tagname vblock))))

(defmethod* print-object ((vblock vblock) stream)
  (print-unreadable-object (vblock stream :type t :identity t)
    (print-tag vblock stream)
    (princ (vblock-structure vblock) stream)))

(defmethod* find-arg-block ((block vblock) aname)
  (gethash aname argmap)) 

(defmethod generate-code ((vblock vblock))
  (let ((spec (vblock-spec vblock)))
    `(let* ,(mapcar #'(lambda (arg)	;let* 
		       `(,arg ,(generate-code (find-arg-block vblock arg))))
	    (vblock-spec-args spec))
       ,(vblock-spec-body spec))))

(defclass* hole (vblock)
  ((filler nil)))

(defmethod* vblock-structure ((hole hole)) 
  (if filler
      (vblock-structure filler)
    (intern (format nil "<~A>" (or argname btype)) (find-package :bio))))

;;; this is a replacement
(defmethod* fill-hole ((block vblock) new-block)
  (if parent
      (fill-argument parent argname new-block)
    (error "No parent for ~A" block)))

(defmethod* fill-hole ((hole hole) new-block)
  (setq filler new-block)
  (when parent
    (fill-argument parent argname new-block)))

(defmethod* fill-argument ((block vblock) aname filler-block)
  (setf (gethash aname argmap) filler-block)
  (setf (vblock-parent filler-block) block)
  (setf (vblock-argname filler-block) aname))

	

(defmethod* generate-code ((hole hole))
  (if filler
      (generate-code filler)
    (error "Incomplete block structure ~A" hole)))

;;; Create holes for a block if necessary.
(defmethod* make-holes ((block vblock)) 
  (when spec
  (dolist (elt (vblock-spec-form spec))
    (cond ((or (symbolp elt) (listp elt))
	   (let* ((hole-name (if (symbolp elt) elt (car elt)))
		  (hole-type (if (symbolp elt) elt (cadr elt)))
		  (hole-filler (gethash hole-name argmap)))
	     (unless hole-filler
	       (fill-argument block hole-name (make-instance 'hole :btype hole-type :argname hole-name)))))))))


(defmethod* htmlize-vblock ((block vblock))
  (make-holes block)
  (let ((*html-stream* (or *html-stream* *standard-output*))) ;debugging hack
    (html      
     ((:span :class "block" "style" (format nil "background-color: ~A" (block-type-color btype))
	     :btype (block-type-name btype)
	     :id (tagname block))	;+++
      (dolist (elt (vblock-form block))
	(cond ((stringp elt) 
	       (html ((:span :class "label") (:princ elt))))
	      ((or (symbolp elt) (listp elt))
	       (let* ((hole-name (if (symbolp elt) elt (car elt)))
;		      (hole-type (if (symbolp elt) elt (cadr elt)))
		      (hole-filler (gethash hole-name argmap)))
		 (html ((:span)
			(htmlize-vblock hole-filler)))
		 ))
	      (t (error "Unknown vpl form ~A" (vblock-form block)))))))))



(defmethod* htmlize-vblock ((block hole))
  (if filler
      (htmlize-vblock filler)
    (html      
     ;; "onMouseDown" (format nil "nmenu(this, '~A', '~A');" btype (tagname block))
     ((:span :class "hole"  "style" (format nil "background-color: ~A" (block-type-color btype))
	     ;; new menus
	     :ID (tagname block)
	     :BTYPE btype
	     )			
      (html ((:span :class "label") (:princ (or argname btype)))))
     )))


;;; +++ or lisp-vblock
(defclass* string-vblock (vblock)
  ((string ""))
  (:writable-instance-variables))

(defmethod* htmlize-vblock ((block string-vblock))
  (let ((input-id (format nil "S~A" (tagname block))))
  (html      
   ((:span :class "block" "style" (format nil "background-color: ~A" (block-type-color btype))
	   :btype (block-type-name btype)
	   :id (tagname block))		;+++
    ((:input :type "TEXT" :value string 
	     :id  input-id
	     :name input-id
	     :onblur (format nil "sendText(this)")))))))

(defmethod* generate-code ((vblock string-vblock))
  (read-from-string string))

(defmethod* vblock-form ((vblock string-vblock))
  (list string))

;;; callback to update string
(publish-prefix 
 :prefix "/vpl/updatestrblock"
 :function #'(lambda (req ent)
               (declare (ignore ent))
	       (let* ((input (request-query req))
		      (id (url-parameter-value :id input))
		      (str (url-parameter-value :str input)))
		 (setf (string-vblock-string (object-named (subseq id 1))) str))))

;;; Types driven from Lisp rather than hand-made here
(defclass* computed-block-type (block-type)
  ())

;;; This is a singleton...
(defclass* organism-block-type (computed-block-type)
  ())

;;; An actual instance of an organism block
(defclass* organism-block (vblock)	; +++ actually we probably want to omit some of vblock, so refactor as the kids say.
  ((spec nil)
   frame)
  (:initable-instance-variables))

(defmethod* vblock-form ((block organism-block))
  (list (fname frame)))

(defmethod* generate-code ((block organism-block))
  (load-organism frame)			;+++ shouldn't have to do this, but we do!
  frame)

(defmethod block-type-menu-spec ((block-type organism-block-type))
  (mapcar #'(lambda (organism)
	      ;; can't use (tagname frame) because it contains a %!
	      (list (fname organism) (fname organism))) 
	  (available-organisms)))

(defmethod make-new-block ((block-type organism-block-type) id)
  (make-instance 'organism-block :btype block-type :frame (frame-fnamed id))) 

(define-block-type 'organism "#b3fbad" 'organism-block-type)

;;; Apparently there is no way to get a list of all gene frames?  It would be too big anyway, but still...

;;; Interface to listner output
(defmethod wb::out-record-to-html ((block vblock) (string string) &rest ignore)
  (declare (ignore ignore))
  (html 
   ((:div :class "vpl")
    (htmlize-vblock block)
    :p " ")
   ))


;;; Headers and footers in listener page.

;;; border not working.
;;; padding not workin
;;;  vertical-align: middle; doesn't do anything

(defun wb::vpl-style ()
  (format nil				;no actual args at present!
    "<style TYPE=\"text/css\">
.vpl {
}

.block {

   font-family: \"Helvetica, Verdana, Arial\";
   font-size: 14pt;
   background-color: #fbb3ad;
   padding: 5px;
   margin: 2px;
   border-width: 2px;
   border-color: #fbb3ad;
   border-style: outset;
   float: left;
}
.hole {
   background-color: #b3fbad;
   color: #ffffff;
   border-width: 2px;
   border-color: #ffffff;
   border-style: inset;
   padding: 5px;
   margin: 2px;
   float: left;
}
.label {
   float: left;
}

.menuTable{
	background-color : ButtonFace;
	border-bottom : solid #808080 1px;
	border-left : solid #FFFFFF 1px;
	border-right : solid #808080 1px;
	border-top : solid #FFFFFF 1px;
	margin : 0px;
	-moz-user-select : none;
	padding : 0px;

}



.secondMenuTable{
	background-color : ButtonFace;
	border: 2px outset #ffffff; 
	margin : 0px;
	-moz-user-select : none;
	padding : 0 1 0 2;
}


</style>

<script language=\"Javascript\" src=\"/vpl/vpl.js\"></script>

	<link rel=\"STYLESHEET\" type=\"text/css\" href=\"/vpl/menus/css/dhtmlXMenu_xp.css\">
	<link rel=\"STYLESHEET\" type=\"text/css\" href=\"/vpl/menus/css/Context.css\">
	<script language=\"JavaScript\" src=\"/vpl/menus/js/dhtmlXProtobar.js\"></script>
	<script language=\"JavaScript\" src=\"/vpl/menus/js/dhtmlXMenuBar.js\"></script>
	<script language=\"JavaScript\" src=\"/vpl/menus/js/dhtmlXMenuBar_cp.js\"></script>	
	<script language=\"JavaScript\" src=\"/vpl/menus/js/dhtmlXCommon.js\"></script>	

" ))


(defun wb::vpl-style-post ()
  (with-output-to-string (stream)
    (format stream "<script type=\"text/javascript\">
  var lispPackage = '~A';
"
	    (wb::user-session-id))
    ;; +++ woefully inefficient to do this over the whole page for each refresh!
    (format stream "menuizeRecursive(document);")
    (format stream "~%</script>")

    ))


;;; Publish

(publish-directory :prefix "/vpl/"
		   :destination (namestring (cl-user:translate-simple-lp "websrc:vpl;web;")))


(publish-directory :prefix "/vpl/menus/"
		   :destination (namestring (cl-user:translate-simple-lp "websrc:vpl;web;dhtmlxMenu;")))




;;; Anything blocks (incomplete implementation, but so is everything else here)


(defclass* anything-block-type (block-type)
  ())

(define-block-type 'anything "#a0a0a0" 'anything-block-type)

(defmethod xml-menu ((btype anything-block-type) stream)
  (net.xml.generator:emit-lxml-as-xml 
   stream
   `((|menu| |absolutePosition| "auto" |mode| "popup" |maxItems| "8" |globalCss| "contextMenu" |globalSecondCss|
	     "contextMenu" |globalTextCss| "contextMenuItem")
     ,@(mapcar #'(lambda (btype)
		   `((|MenuItem| |name| ,btype)
		     ,@(mapcar #'(lambda (spec)
				   `((|MenuItem| |name| ,(car spec)  |id| ,(cadr spec))))
			       (block-type-menu-spec (get btype :block-type)))))
	       (remove 'anything *block-types*)))))

(defmethod make-new-block ((block-type anything-block-type) id)
  (let ((vspec (object-named id)))
    (make-instance 'vblock :btype (vblock-spec-btype vspec) :spec vspec))
  )

(defmethod make-new-block ((block-type anything-block-type) (id null))
  (make-instance 'string-vblock :btype block-type))



;;; Listener interface

(defparameter *block-debug* t)

(defmethod block-eval ((block vblock))
  (when *block-debug*
    (format NET.ASERVE::*DEBUG-STREAM* "~%Block eval of ~A~%" block))
  (format t "Block eval: ~A" (vblock-structure block))
  (eval (generate-code block)))

(defmethod block-eval ((blockname string))
  (block-eval (object-named blockname)))

;;; Was top-hole, but its user-visible and sounds vaguely dirty
(defclass top (hole)
  ())

(defmethod htmlize-vblock ((hole top))
  (html ((:span)
	 (call-next-method))
	(:div
	 ((:input :type "SUBMIT" "onclick" (format nil "listenerEvalBlock('~A')" (tagname hole)) :name "data" :value "Eval")))))
  
(defun vpl ()
  (make-instance 'top :btype 'anything))

(defmethod vhtml ((block vblock))
  (let ((*html-stream* *standard-output*))
    (htmlize-vblock block)))

(defmethod vhtml ((blockname string))
  (vhtml (object-named blockname)))
