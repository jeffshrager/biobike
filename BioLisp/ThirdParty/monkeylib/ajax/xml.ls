;; Lispscript

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


;; take an xml node. It has a name and a type.
;; if it is an element, it should be added to it's parent 


(defun xml-node2object (xml)
  (let ((type (@ xml nodeType)))
    (case type
      (1 (xml-element2object xml))
      (2 (alert (+ (ref (@ NodeTypes names) type) " not supported")))
      (3 xml.data)
      (4 (alert (+ (ref (@ NodeTypes names) type) " not supported")))
      (5 (alert (+ (ref (@ NodeTypes names) type) " not supported")))
      (6 (alert (+ (ref (@ NodeTypes names) type) " not supported")))
      (7 (alert (+ (ref (@ NodeTypes names) type) " not supported")))
      (8 (alert (+ (ref (@ NodeTypes names) type) " not supported")))
      (9 (xml-document2object xml))
      (10 (alert (+ (ref (@ NodeTypes names) type) " not supported")))
      (11 (alert (+ (ref (@ NodeTypes names) type) " not supported")))
      (12 (alert (+ (ref (@ NodeTypes names) type) " not supported")))
      (t (alert (+ "Unrecoginize nodeType " (@ xml nodeType)))))))

(defun document-to-object (xml)
  (add-child-to-object (new Object) (@ xml documentElement)))

(defun add-child-to-object (object child)
  (let ((child-name (@ child nodeName))
	(child-object (element-to-object child))) ;; may return an object or a string
    ;;(alert (+ "adding " child-object " to " object " under name " child-name))
    (let ((slot-value (ref object child-name)))
      (cond
	((not slot-value)
	 ;;(alert (+ "setting " child-name " to " child-object))
	 (set (ref object child-name) child-object))
	((not (arrayp slot-value))
	 ;;(alert (+ "setting " child-name " to new array"))
	 (set (ref object child-name) (array slot-value child-object)))
	(true
	 ;;(alert (+ "pushing new element onto " child-name))
	 (.push slot-value child-object)))))
  object)

(defun element-to-object (element)
  (let ((children (@ element childNodes)))
    (cond
      ((and (= (@ children length) 1)
	    (= (@ (.item children 0) nodeType) (@ NodeTypes TEXT_NODE)))
       (@ (.item children 0) data))
      (true
       (let ((object (new Object)))
	 (dotimes (i (@ children length))
	   (let ((child (.item children i)))
	     (unless (= (@ child nodeType) NodeTypes.ELEMENT_NODE)
	       ;;(alert (+ "child " i " of " (@ element nodeName) " not an element"))
	       )
	     (add-child-to-object object child)))
	 ;;(alert (+ "returning object: " object))
	 object)))))
  
(defun arrayp (obj)
  (and (equal (TYPEOF obj) "object") (equal (@ obj constructor) Array)))

  