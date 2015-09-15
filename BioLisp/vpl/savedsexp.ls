;;; -*-lisp-*-
;;                             SEXP.LS
;;
;;	HISTORY
;;  !! Handler is not here, it's in function vpl-on-message in vpl.ls!!
;;
;;  Jul 21 '06	Menu.  MenuEntry. Menu.add-menu-entry(menu-entry).
;;    xml-element-to-object.
;;
;;
;;
;;
;;
;;  (MENU  "MenuTitle Name"
;;      (MENUENTRY  "MenuEntry Name"  ID# )
;;  )
;;
;;
;;  Format for XML SEXP nodes:  <nodeType nodeName> value
;;  <1 LIST>
;;    <1 SYMBOL> 
;;       MENU </SYMBOL>
;;    <1 STRING>                             Note:  Not a 3 "text node" type!
;;       "MenuTitle Name" </STRING>
;;    <1 LIST>
;;       <1 SYMBOL>
;;          MENUENTRY
;;       <1 STRING>
;;          "MenuEntry Name"  </STRING>
;;       <1 NUMBER>
;;           12
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; The following types are used by the XML Transport layer
;; to pack and unpack information.
;; They are not part of SEXP.

(defvar NodeTypes (new Object))
(set (@ NodeTypes ELEMENT_NODE) 1)
(set (@ NodeTypes ATTRIBUTE_NODE) 2)
(set (@ NodeTypes TEXT_NODE) 3)
(set (@ NodeTypes CDATA_SECTION_NODE) 4)
(set (@ NodeTypes ENTITY_REFERENCE_NODE) 5)
(set (@ NodeTypes ENTITY_NODE) 6)
(set (@ NodeTypes PROCESSING_INSTRUCTION_NODE) 7)
(set (@ NodeTypes COMMENT_NODE) 8)
(set (@ NodeTypes DOCUMENT_NODE) 9)
(set (@ NodeTypes DOCUMENT_TYPE_NODE) 10)
(set (@ NodeTypes DOCUMENT_FRAGMENT_NODE) 11)
(set (@ NodeTypes NOTATION_NODE) 12)

(set (@ NodeTypes names) (ARRAY
			  false
			  "ELEMENT_NODE"
			  "ATTRIBUTE_NODE"
			  "TEXT_NODE"
			  "CDATA_SECTION_NODE"
			  "ENTITY_REFERENCE_NODE"
			  "ENTITY_NODE"
			  "PROCESSING_INSTRUCTION_NODE"
			  "COMMENT_NODE"
			  "DOCUMENT_NODE"
			  "DOCUMENT_TYPE_NODE"
			  "DOCUMENT_FRAGMENT_NODE"
			  "NOTATION_NODE"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;;  ID stamp.
(defvar current-id 0)  ;;Was 1, which causes ++code to start at 2.  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; OBJECTS      Pseudo-constructors

(defun SexpList ()
  (set (@ this sexp-type) "list")
  (set (@ this unique-id) (++ current-id))
  (set (@ this list-items) (new Array))
  this)

(defun SexpMenu (title-string)  					 ;;JKM 07/21/06
  ;;; This Menu object represents a single menu.  It contains a list of MenuEntries.
  (set (@ this sexp-type) "menu")
  (set (@ this unique-id) (++ current-id))
  (set (@ this menu-entries) (new Array))
  (set (@ this title-string) title-string)
  this)

(defun SexpMenuEntry (command-id title-string)
  ;;; This low-level MenuEntry object represents a single entry line on a particular menu.
;;Note:  In actuality, args are ignored; actual contents are stuffed in build-from-xml.
  (set (@ this sexp-type) "menuentry")
  (set (@ this command-id) command-id)
  (set (@ this title-string) title-string)
  this)

(defun SexpSymbol (name package)
  (set (@ this sexp-type) "symbol")
  (when package
    (set (@ this package) package))
  (set (@ this name) name)
  this)

(defun SexpNumber (value) 
  (set (@ this sexp-type) "number")
  (set (@ this value) value)
  this)

(defun SexpString (value)
  (set (@ this sexp-type) "string")
  (set (@ this value) value)
  this)

(defun SexpT (value)
  (set (@ this sexp-type) "t")
  (set (@ this value) value)
  this)

;; N.B. A hole has 'content' while a multihole has 'contents'
(defun SexpHole () 
  (set (@ this sexp-type) "hole")
  (set (@ this content) false)
  this)

(defun SexpMultiHole () 
  (set (@ this sexp-type) "mhole")
  (set (@ this contents) false)
  this)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; METHODS   
;; LIST OPERATIONS

(defmethod (SexpList add-list-item) (item)
  (set (@ item unique-id) (++ current-id))
  (set (@ item parent) this)
  (set (@ item index) (@ this list-items length))
  (.push (@ this list-items) item))

(defmethod (SexpList replace-list-item) (item index)
  ;;; This method overwrites zero-based entry #index in the list, with 'item'.
  (set (@ item unique-id) (++ current-id))
  (set (@ item parent) this)
  (set (ref (@ this list-items) index) item))


(defmethod (SexpMenu add-menu-entry) (menu-entry)
  ;;; This Menu method pushes a unique menu-entry onto the end of the Menu's list.
  (set (@ entry parent) this)
  (set (@ entry index) (@ this menu-entries length))
  (.push (@ this menu-entries) menu-entry))

(defmethod (SexpMenu add-list-item) (menu-entry)
  ;;; A hack requires using add-list-item, so here it is for Menu.
  (@ this add-menu-entry menu-entry))


(defmethod (SexpMultiHole replace-list-item) (item index)
  (set (@ item unique-id) (++ current-id))
  (set (@ item parent) (@ this parent))
  (set (ref (@ this contents) index) item))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; fill holes with content

(defmethod (SexpHole fill) (content)
  (set (@ this content) content))

(defmethod (SexpHole fill-with-literal) (text)
  (let ((type (@ this type)))
    (unless (and (string= (ref text 0) "\"")
		 (string= (ref text (- (@ text length) 1)) "\""))
      (set text (.to-upper-case text)))
    (cond
      ((string= type "symbol")
       (.fill this (new SexpSymbol text)))
      ((string= type "string")
       (.fill this (new SexpString text)))
      ((string= type "number")
       (.fill this (new SexpNumber text)))
      ((string= type "boolean")
       (.fill this (new SexpT text)))
      ((string= type "t")
       (.fill this (new SexpT text)))
      (true (debug "Don't know how to fill hole of type " type)))))

(defmethod (SexpMultiHole fill) (contents)
  (set (@ this contents) contents))

(defmethod (SexpMultiHole fill-with-literal) (text)
  (set text (.to-upper-case text))
  (let ((type (@ this type)))
    (cond
      ((string= type "symbol")
       (.fill this (new SexpSymbol text)))
      ((string= type "string")
       (.fill this (new SexpString text)))
      ((string= type "number")
       (.fill this (new SexpNumber text)))
      ((string= type "boolean")
       (.fill this (new SexpT text)))
      ((string= type "t")
       (.fill this (new SexpT text)))
      (true (debug "Don't know how to fill hole of type " type)))))

(defun copy-sexp-up (sexp)
  (when (@ sexp parent)
    (.replace-list-item (@ sexp parent) sexp (@ sexp index))
    (copy-sexp-up (sexp parent))))

(defun get-empty-sexp ()
  (new SexpT "delete")
;  (new SexpHole)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; deep-copy

(defmethod (SexpT deep-copy) ()
  (new SexpT (@ this value)))

(defmethod (SexpString deep-copy) ()
  (new SexpString (@ this value)))

(defmethod (SexpNumber deep-copy) ()
  (new SexpNumber (@ this value)))

(defmethod (SexpSymbol deep-copy) ()
  (new SexpSymbol (@ this name)))

(defmethod (SexpList deep-copy) ()
  (let ((new-list (new SexpList)))
    (dolist (thing (@ this list-items))
;      (.add-list-item new-list (.deep-copy thing))
      (.add-list-item new-list thing)
      )
    new-list))

(defmethod (SexpHole deep-copy) ()
  (let ((sexp (new SexpHole)))
    (set (@ sexp name) (@ this name))
    (set (@ sexp type) (@ this type))
    (set (@ sexp inline) (@ this inline))
    (set (@ sexp editable) (@ this editable))
    (set (@ sexp content) (.deep-copy (@ this content)))
    sexp))

(defmethod (SexpMultiHole deep-copy) ()
  (let ((sexp (new SexpMultiHole)))
    (set (@ sexp name) (@ this name))
    (set (@ sexp type) (@ this type))
    (set (@ sexp inline) (@ this inline))
    (set (@ sexp editable) (@ this editable))
    (set (@ sexp contents) (.copy-contents this))
    (set (@ sexp template) (.copy-template this))
    sexp))

(defmethod (SexpMultiHole copy-contents) ()
  (let ((contents false))
    (cond
      ((> (@ this contents length) 0)
       (set contents (new Array))
       (dolist (item (@ this contents))
	 (.push contents (.deep-copy item))))
      ((@ this contents)
       (debug "Multi-hole with one item?")
       (set contents (.deep-copy (@ this contents))))
      (true 
       (debug "Multi-hole with no contents?")))
    contents))

(defmethod (SexpMultiHole copy-template) ()
  (let ((template false))
    (cond
      ((> (@ this template length) 0)
       (set template (new Array))
       (dolist (item (@ this template))
	 (.push template (.clone-node item true))))
      ((@ this template)
       (debug "Multi-hole with empty template?"))
      (true 
       (debug "Multi-hole with no template?")))
    template))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; to-HTML   METHODS

;;  list
(defmethod (SexpList to-html) ()
  (let ((list (html ((:div :class "list")))))
    (set (@ list sexp) this)
    (dotimes (i (@ this list-items length))
      (let ((thing (ref (@ this list-items) i)))
	(.append-child list (.to-html thing))))
    list))


;;  Menu      
;;        wanted:  <select name="selectName" size="1">   ...  </select>
(defmethod (SexpMenu to-html) ()
  (let ((themenu (html ((:select :class "menu" :name (@ this title-string) :size 1)))))
    (set (@ themenu sexp) this)
    (dotimes (i (@ this menu-entries length))
      (let ((this-menu-entry (ref (@ this menu-entries) i)))
	(.append-child themenu (.to-html this-menu-entry))))
    themenu))


;;  MenuEntry
;;      wanted:  <option value="id1">first</option>
;;
(defmethod (SexpMenuEntry to-html) ()
  (let ((command-id (@ this command-id))
        (title-string (@ this title-string)))
    (html ((:option :value "command-id") title-string))))




;;  symbol
(defmethod (SexpSymbol to-html) ()
  (let ((name (@ this name))
	(package (@ this package)))
    (html ((:span :class "symbol") name))))

;;  number
(defmethod (SexpNumber to-html) () 
  (let ((value (@ this value)))
    (html ((:span :class "value") value))))

;;  string
(defmethod (SexpString to-html) () 
  (let ((value (@ this value)))
    (html ((:span :class "value") value))))

;;  T
(defmethod (SexpT to-html) () 
  (let ((value (@ this value)))
    (html ((:span :class "value") value))))

;;  Hole
(defmethod (SexpHole to-html) ()
  ;;(debug "hole.toHtml: template: " (@ this template)  "; name: " (@ this name))
  (let ((hole (if (@ this inline) (html (:span :class "hole")) (html (:div :class "hole")))))
    (set (@ hole sexp) this)
    (let ((type (@ this type)))
;      (debug "hole type is: " type)
      (cond
	((@ this list-items)
;	 (debug "hole with list: ")
	 (dolist (item (@ this list-items))
	   (.append-child hole (.to-html item))))
	((@ this contents)
;	 (debug "hole with contents: ")
	 (.append-child hole (.to-html (@ this contents))))
	((@ this content)
	 (cond
	    ((string= type "symbol")
	     (.append-child hole (.to-html (@ this content))))
	    ((string= type "number")
	     (.append-child hole (.to-html (@ this content))))
	    ((string= type "string")
	     (.append-child hole (.to-html (@ this content))))
	    ((string= type "boolean")
	     (.append-child hole (.to-html (@ this content))))
	    ((string= type "t")
	     (.append-child hole (.to-html (@ this content))))
	    (true
	     (debug "Hole contains: " type))))
	((@ this value)
;	 (debug "hole with value: " (@ this value))
	 (.append-child (.to-html (@ this value))))
	((@ this name)
	 (let ((name (@ this name)))
	   (.append-child hole (html (:i name)))))))
    (setup-hole hole this)
    (add-menu-box hole)
    hole))


;;  MultiHole
(defmethod (SexpMultiHole to-html) ()
  ;;(debug "multihole.toHtml: template: " (@ this template)  "; name: " (@ this name))
  (let ((hole (if (@ this inline) (html (:span :class "mhole")) (html (:div :class "mhole")))))
    (set (@ hole sexp) this)
    (let ((contents (@ this contents))
	  (template (@ this template)))
      (cond
	((> (@ contents length) 0)
	 (dotimes (i (@ this contents length))
	   (.append-child hole (.to-html (ref (@ this contents) i)))
	   (setup-hole hole this))
#- (and)	 (if (or (string= (@ this type) "list")
		 (string= (@ this type) "multi"))
	     (let ((ellipses (html (:i "..."))))
	       (set (@ ellipses onclick) 
		    (lambda () 
		      (dotimes (i (@ template length))
			(let ((thing (xml-to-sexp (ref template i))))
			  (.push contents thing)
			  (.insert-before hole (.to-html thing) ellipses)))))
	       (.append-child hole ellipses))))
	((> (@ template length) 0)
	    (dotimes (i (@ this template length))
	      (let ((thing (xml-to-sexp (ref (@ this template) i))))
;		(set (@ thing parent) (@ this parent))
		(set (@ thing parent) this)
		(set (@ thing index) i)
		(.push (@ this contents) thing)
		(.append-child hole (.to-html thing))))
	 (if (or (string= (@ this type) "list")
		 (string= (@ this type) "multi"))
	     (let ((ellipses (html (:i "..."))))
	       (set (@ ellipses onclick) 
		    (lambda () 
		      (dotimes (i (@ template length))
			(let ((thing (xml-to-sexp (ref template i))))
			  (.push contents thing)
			  (.insert-before hole (.to-html thing) ellipses)))))
	       (.append-child hole ellipses))))
	(true
	 (debug "What the...?!?!"))))
    (add-menu-box hole)
    hole))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; to-xml

(defmethod (SexpList to-xml) ()
  (let ((xml-list (xml-element (document) (:list))))
    (dolist (thing (@ this list-items))
      ;; if multi hole, call other method to return xml list
      (if (string= (@ thing type) "multi")
	  (dolist (item (@ thing contents))
	    (.append-child xml-list (.to-xml item)))
	  (.append-child xml-list (.to-xml thing))))
    xml-list))

(defmethod (SexpMenu to-xml) ()
  ;;; Converts an existing Menu into an xml blob, on the client side.
;; FIX ME.  IS THIS NEEDED?
  (let ((xml-list (xml-element (document) (:list))))
    (dolist (thing (@ this menu-entries))
      (.append-child xml-list (.to-xml thing)))
    xml-list))

(defmethod (SexpMenuEntry to-xml) ()
  ;;; Converts an existing MenuEntry into an xml blob, on the client side.
;; FIX ME.  IS THIS NEEDED?
  (let ((package (@ this package))
	(name (@ this name)))
      (if package
	  (xml-element (document) ((:symbol :package package) name))
        (xml-element (document) (:symbol name)))))

(defmethod (SexpSymbol to-xml) ()
  (let ((package (@ this package))
	(name (@ this name)))
      (if package
	  (xml-element (document) ((:symbol :package package) name))
	  (xml-element (document) (:symbol name)))))

(defmethod (SexpNumber to-xml) ()
  (let ((value (@ this value)))
    (xml-element (document) (:number value))))

(defmethod (SexpString to-xml) () 
  (let ((value (@ this value)))
    (xml-element (document) (:string value))))

(defmethod (SexpT to-xml) () 
  (let ((value (@ this value)))
    (xml-element (document) (:t value))))

(defmethod (SexpHole to-xml) ()
  (if (@ this content)
      (.to-xml (@ this content))
      (xml-element (document) (:unfilled-hole))))

(defmethod (SexpMultiHole to-xml) ()
  (if (@ this contents)
      (.to-xml (@ this contents))
      (xml-element (document) (:unfilled-hole))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; from-xml Functions.

(defun xml-to-sexp (xml)
  (let ((type (@ xml nodeType)))
    (cond
      ((= type NodeTypes.ELEMENT_NODE) (xml-element-to-object xml))
      ((= type NodeTypes.TEXT_NODE) (@ xml data))
      ((= type NodeTypes.DOCUMENT_NODE) (xml-to-sexp (@ xml document-element)))
      (true
       (if (&& (<= 1 type) (<= type 12))
	   (alert (+ (ref (@ NodeTypes names) type) " not supported"))
	   (alert (+ "Unrecognized nodeType " type)))))))

(defun xml-element-to-object (element)
  (let ((tag (@ element node-name)))
;    (debug "Making Sexp out of XML element with tag: " tag)
    (let ((obj
           (cond
            ((string-equal tag "LIST") (new SexpList))
            ((string-equal tag "MENU") 
             (.append-child (.get-element-by-id document "palette")   (html "new SexpMenu...") )
             (new SexpMenu))
            ((string-equal tag "MENUENTRY") 
             (.append-child (.get-element-by-id document "palette")   (html "new SexpMenuEntry...") )
             (new SexpMenuEntry))
            ((string-equal tag "SYMBOL") (new SexpSymbol))
            ((string-equal tag "NUMBER") (new SexpNumber))
            ((string-equal tag "STRING") (new SexpString))
            ((string-equal tag "HOLE") (new SexpHole))
            ((string-equal tag "MHOLE") (new SexpMultiHole)))))
      (.build-from-xml obj element)
      obj)))

(defun string-equal (s1 s2)
  (string= (.to-upper-case s1) (.to-upper-case s2)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  BUILD-FROM-XML METHODS

(defmethod (SexpList build-from-xml) (element)
  (let ((children (@ element child-nodes)))
    (dotimes (i (@ children length))
      (let ((child (.item children i)))
	;; sort of a hack for the moment.
	(unless (and (= (@ child node-type) 3) (all-blank (@ child data)))
	  (let ((sexp (xml-to-sexp child)))
	    (.add-list-item this sexp)))))))

(defun all-blank (string)
  (let ((all-blank true))
    (dotimes (i (@ string length))
      (let ((char (.char-at string i)))
	(if (and (not (eq char #\Space))
		 (not (eq char #\Newline))
		 (not (eq char #\Tab)))
	    (set all-blank false))))))




(defmethod (SexpMenu build-from-xml) (element)
  ;;; This method fills in an existing menu node.
    (set (@ this title-string)   (xml-to-sexp (@ element first-child))  )
)


(defmethod (SexpMenuEntry build-from-xml) (element)
  ;;; This method fills in an existing menu entry node.
  (let* ((first (@ element first-child))
         (second (.nextSibling first)))
    (set (@ this command-id)     (xml-to-sexp second) )
    (set (@ this title-string)   (xml-to-sexp first)  )
  )
)





(defmethod (SexpSymbol build-from-xml) (element)
  (set (@ this name) (xml-to-sexp (@ element first-child)))
  (set (@ this package) (.get-attribute element "package")))

(defmethod (SexpNumber build-from-xml) (element)
  (set (@ this value) (xml-to-sexp (@ element first-child))))

(defmethod (SexpString build-from-xml) (element)
  (set (@ this value) (xml-to-sexp (@ element first-child))))

(defmethod (SexpHole build-from-xml) (element)
  (set (@ this name) (.get-attribute element "name"))
  (set (@ this type) (.get-attribute element "type"))
  (set (@ this editable) (.get-attribute element "editable"))
  (set (@ this inline) (.get-attribute element "inline"))
  (when (string= (@ this type) "list")
    (set (@ this min) (.get-attribute element "min"))
    (set (@ this max) (.get-attribute element "max"))))

(defmethod (SexpMultiHole build-from-xml) (element)
  (set (@ this name) (.get-attribute element "name"))
  (set (@ this type) (.get-attribute element "type"))
  (set (@ this editable) (.get-attribute element "editable"))
  (set (@ this inline) (.get-attribute element "inline"))
;  (when (string= (@ this type) "list")
;    (set (@ this min) (.get-attribute element "min"))
;    (set (@ this max) (.get-attribute element "max")))
  (let ((children (@ element child-nodes)))
    (when (< 0 (@ children length))
      (set (@ this contents) (new Array))
      (set (@ this template) (new Array))
      (dotimes (i (@ children length))
	(let ((child (.item children i)))
	  (.push (@ this template) child))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
