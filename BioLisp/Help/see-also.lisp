;;; -*- Package: help; mode: lisp; base: 10; Syntax: Common-Lisp; -*-

(in-package :help)

;;; +=========================================================================+
;;; | Copyright (c) 2002-2006 JP Massar, Jeff Shrager, Peter Seibel           |
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

;;; Authors:  JP Massar, Peter Seibel.

;;; Every SEE-ALSO entry in a documented object gets parsed into
;;; a SEE-ALSO object, consisting of the type of documented object
;;; pointed to and enough information to identify the object being
;;; referenced (which varies from type to type)

;;; Each SEE-ALSO entry as entered by the documenter can be:
;;;
;;; -- An external Common Lisp symbol; display as a link to the Hyperspec
;;;      with the symbol's name as the label.

;;; -- Some other symbol; a generic reference is created, which, when 
;;;      resolved (when the user clicks on the link) might end up pointing at:
;;;      -- a DOCUMENT-FUNCTION object, 
;;;      -- a symbol with function documentation,
;;;      -- a symbol with variable documentation, 
;;;      -- a symbol with type documentation
;;;      resolved in that order.  
;;;      Displayed as a link with the symbol's name as the label.

;;; -- A string beginning with "http://", taken to be an external URL,
;;;      displayed as a link with the label being the address.

;;; -- Some other string, which is displayed literally with no
;;;      link associated with it.

;;; -- A frame, displayed as a link with standard frame notation

;;; -- A list of exactly two strings, the first interpreted as a URL, the
;;;      second as the display label for the displayed link.

;;; -- A list whose first element is either a symbol or a string, and which,
;;;      when converted to a keyword, is EQ to one of the keywords associated
;;;      with one of the documentation types as specified below.  The
;;;      format of the rest of the list is dependent on the documentation 
;;;      type.  The specific formats are:
;;;
;;;      ( [ :module | :mod ] <name of module> )
;;;        -- The name is a symbol or string, converted to a string.

;;;      ( [ :topic | :top ] <name of topic )
;;;        -- The name is a symbol or string, converted to a string.

;;;      ( [ :glossary | :glos ] <name of glossary entry> )
;;;        -- The name is a symbol or string, converted to a string.

;;;      ( [ :tutorial | :tut ] <name of tutorial> )
;;;        -- The name is a symbol or string, converted to a string.

;;;      ( [ :df ] symbol )
;;;        -- The symbol must be a symbol or a string. If a string,
;;;           it must be of the form "<package>:<name>". (Use a string
;;;           to avoid package problems). (The referent is a documentation
;;;           entry created with DOCUMENT-FUNCTION.)

;;;      ( [ :url | :http ] <url> <label> )  
;;;        -- Both the url and the label must be strings.

;;;      ( [ :docfile | :filedoc ] <relative-pathname> &optional label )
;;;        -- the pathname must be a string, and is interpreted relative
;;;           to the Weblistener's documentation directory.  The optional
;;;           label, if present, must be a string which is used as the
;;;           URL's label; otherwise the filename is used.  

;;;      ( [ :symbol | :sym ] [ symbol | symbol <type> )
;;;        -- The symbol must be a symbol or a string. If a string,
;;;           it must be of the form "<package>:<name>". (Use a string
;;;           to avoid package problems).
;;;           Type is one of :function, :variable or :type (or any symbol
;;;           whose name is STRING-EQUAL to the names of those symbols, or
;;;           any string STRING-EQUAL to the names of those symbols)
;;;           indicating the type of documentation.

;;;      ( [ :common-lisp | :lisp | :cl ] symbol )
;;;        -- The symbol must be a symbol or string.  If a symbol, the
;;;        symbol's name is used. If a string, it is used directly.
;;;        Said string must name an external symbol in the Common Lisp package.
;;;        (The reference is to the Hyperspec).

;;;      ( :frame <frame-name> )
;;;        -- The name must be a string, being the Fname of the frame
;;;           being referenced.

;;; Examples:

;;; (:see-also (:url "http://www.google.com" "Google it!") utils:one-string)
;;;
;;; (:see-also 
;;;   (:cl "+") (:df +) (:module "bbl-arithmetic") (:tutorial "How to add"))
;;;
;;; (:see-also (:glossary "operating system") (:docfile "weblistener-os.txt"))
;;;
;;; (:see-also (:sym bbl:gene type) (:topic "Cyanobacterial Genes"))

;;; (:see-also #$Go.Molecular_Function (:top "The Go Ontology"))

;;; When SEE-ALSO entries are displayed (by displaying a documented entity
;;; which has SEE-ALSO information), the various types of references are
;;; distinguished and displayed with appropriate labels.


(defstruct see-also category id)

(defparameter *documented-symbol-types* '(:function :variable :type))

(defparameter *see-also-data* 

  '(

    ;; Actual documentation objects

    (help:documentation-file (:filedoc :docfile))
    (help:function-documentation (:df))
    (help:glossary-entry (:glossary :glos))
    ;; No macro-documentation, not used
    (help:module (:module :mod))
    (help:symbol-doc (:symbol :sym))
    (help:topic (:topic :top))
    (help:tutorial (:tutorial :tut))
    ;; No variable-documentation, not used

    ;; Other possible reference types

    (help:common-lisp (:common-lisp :lisp :cl))
    (help:frame (:frame))
    (help:other (:other))
    (help:url (:url :http))

    ))

(defun keyword->see-also-type (keyword)
  (loop for (type keywords) in *see-also-data*
        when (member keyword keywords)
        do (return type)
        ))

(defun see-also-type->keywords (type)
  (second (find type *see-also-data* :key 'first)))


;;; Make sure each entry in a SEE-ALSO list conforms to the legal possibilities
    
(defun verify-see-also-entries (entries)
  (block exit
    (values
     t
     (loop for entry in entries collect
           (multiple-value-bind (ok? value-or-oops)
               (verify-see-also-entry entry)
             (if ok? 
                 value-or-oops
               (return-from exit (values nil value-or-oops))
               ))))))

(defmethod verify-see-also-entry ((entry t))
  (if (frames::isframe? entry)
      (values t entry) 
    (values nil "Unknown object for SEE-ALSO entry: ~S" entry)
    ))

(defmethod verify-see-also-entry ((entry symbol)) (values t entry))

(defmethod verify-see-also-entry ((entry string))
  (if (zerop (length entry))
      (values nil "SEE-ALSO entry not allowed to be null string!")
    (values t entry)))

(defmethod verify-see-also-entry 
           ((entry list)
            &aux
            (len (length entry)) (name (second entry)) (info (third entry)))
  (block exit
    (flet ((oops (&rest format-args)
             (return-from exit 
               (values nil (apply 'format nil format-args))
               )))
      ;; the symbol NIL 
      (when (null entry) (return-from exit (values t entry)))
      (unless (> len 1) (oops "See-also entry must specify more information."))
      (unless (or (stringp (first entry)) (symbolp (first entry)))
        (oops "Invalid object ~S begins SEE-ALSO entry!" (first entry)))
      ;; a list of 2 strings, interpreted as a URL and label
      (when (and (= len 2) (every 'stringp entry)) 
        (return-from exit (values t entry)))
      (let ((see-also-type (keyword->see-also-type (keywordize (first entry)))))
        (case (keywordize see-also-type)
          ((:module :topic :tutorial :glossary-entry)
           (unless (or (stringp name) (symbolp name)) 
             (oops "Data must be a string or symbol!"))
           (unless (= len 2)
             (oops "Data must be a single string or symbol!")))
          (:function-documentation 
           (unless (= len 2)
             (oops "Data must be a single string or symbol!"))
           (cond 
            ((symbolp name) nil)
            ((stringp name) 
             (unless (position #\: name) 
               (oops "Data string must be of the form <package>:<name>")))
            (t (oops "Data must be either a string or symbol!"))))
          (:url 
           (unless (and (= len 3) (every 'stringp (rest entry)))
             (oops "Data must be two strings, a URL and a label!")))
          (:documentation-file 
           (unless (and (or (= len 2) (= len 3)) (every 'stringp (rest entry)))
             (oops 
              #.(one-string-nl
                 "Data must be a string (a relative file), and optionally"
                 "another string as a label."))))
          (:symbol-doc 
           (unless (or (= len 2) (= len 3))
             (oops "Data must be one or two elements, a symbol, and a type!"))
           (unless (or (symbolp name) (stringp name))
             (oops "First data item must name a symbol!"))
           (when (stringp name) 
             (unless (position #\: name) 
               (oops "Symbol string must be of the form <package>:<name>")))
           (unless (or (symbolp info) (stringp info))
             (oops "Second data item must name a type!"))
           (when (cddr entry) 
             (unless (member (string info) *documented-symbol-types* 
                             :test 'string-equal :key 'string)
               (oops "Second data item must be one of ~A" 
                     *documented-symbol-types*))))
          (:common-lisp 
           (unless (and (= len 2) (or (symbolp name) (stringp name))) 
             (oops 
              "Data must be a single element naming a Common Lisp symbol!"))
           (when (or (and (symbolp name) (not (find-symbol (string name) :cl)))
                     (and (stringp name) (not (find-symbol name :cl))))
             (oops "Data does not name a Common Lisp symbol!")))
          (:frame
           (unless (and (= len 2) (stringp name)) 
             (oops "Data must be a single string naming a frame!")))
          (:other
           (unless (and (= len 1) (stringp name))
             (oops "Data must be a single string!")))
          (otherwise 
           (oops "Unknown see-also type: ~S" (first entry)))
          )
        (values t entry)
        ))))



;;; Parse into SEE-ALSO structures where the ID is in canonical form.


(defun parse-see-also-entries (x) 
  (mapcar 'parse-see-also-entry (ensure-list x)))

(defgeneric parse-see-also-entry (entry)
  (:documentation "Creates a SEE-ALSO structure from a Lisp representation"))

(defmethod parse-see-also-entry ((entry t))
  (if (frames::isframe? entry)
      (make-see-also :category 'help:frame :id (frames:fname entry))
    (error "Internal error"))) 

(defmethod parse-see-also-entry ((entry symbol))
  ;; A symbol is either a Common Lisp symbol, a symbol 
  ;; defined by a DOCUMENT-FUNCTION definition, or a random
  ;; symbol.
  (cond 
   ((eq (find-package :cl) (symbol-package entry))
    (make-see-also :category 'help:common-lisp :id (string entry)))
   ((find-documentation entry 'help:function-documentation)
    (make-see-also 
     :category 'help:function-documentation
     :id (list (string entry) (package-name (symbol-package entry)))
     ))
   (t
    (make-see-also
     :category 'help:symbol-doc 
     :id 
     (list 
      (string entry)
      (package-name (symbol-package entry))
      (cond
       ((fboundp entry) :function)
       ((boundp entry) :variable)
       (t :function)
       ))))))

(defmethod parse-see-also-entry ((entry string))
  (let ((http-prefix "http://"))
    (cond
     ((utils:initial-subsequence-of? 
       entry http-prefix :element-test 'char-equal)
      (make-see-also
       :category 'help:url :id (list entry (subseq entry (length http-prefix)))
       ))
     (t (make-see-also :category 'help:other :id entry))
     )))

(defmethod parse-see-also-entry 
           ((entry list)   
            &aux
            (name (second entry)) (info (third entry)))
  (if (stringp (first entry))
      (make-see-also :category 'help:url :id entry)
    (let ((category (keyword->see-also-type (keywordize (first entry)))))
      (make-see-also
       :category (intern (string category) :help)
       :id
       (ecase (keywordize category)
         ;; ID is simply a string, naming the object
         ((:module 
           :tutorial 
           :glossary-entry
           :topic
           :frame
           )
          (string name))
         ;; ID is a list of the filename and a label for a URL (possibly NIL)
         (:documentation-file
          (when (char= #\/ (char name 0)) (setq name (subseq name 1)))
          (if info 
              (list name info)
            (list name name)
            ))
         ;; ID is a list of the symbol's name and the symbol's package's name
         (:function-documentation 
          (cond
           ((symbolp name) 
            (list (string name) (package-name (symbol-package name))))
           ((stringp name)
            (let ((pos (position #\: name)))
              (list 
               (string-upcase (subseq name (1+ pos)))
               (string-upcase (subseq name 0 pos)))
              ))
           (t (error "Internal error."))))
         ;; ID is a list of the URL string and a label
         (:url (list name info))
         ;; ID is a list of the symbol's name, the symbol's package's name,
         ;; and the type of documentation.
         (:symbol-doc
          (let (sname spackage symbol)
            (cond
             ((stringp name)
              (setq symbol (read-from-string name))
              (let ((pos (position #\: name)))
                (setq spackage (string-upcase (subseq name 0 pos)))
                (setq sname (string-upcase (subseq name (1+ pos))))))
             ((symbolp name)
              (setq symbol name)
              (setq sname (string name))
              (setq spackage (package-name (symbol-package name))))
             (t (error "Internal error"))
             )
            (if info 
                (setq info (keywordize info))
              (setq info (symbol-to-default-doc-type symbol)))
            (list sname spackage info)
            ))
         ;; ID is the symbol's name
         (:common-lisp (string name))
         (:other name)
         )))))

(defun make-see-also-function-documentation (symbol)
  (make-see-also 
   :category 'help:function-documentation
   :id (list (string symbol) (package-name (symbol-package symbol)))
   ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun see-also-refers-to-docobj? (see-also)
  (member (see-also-category see-also) *documentation-types*))

(defun see-also->docobj (see-also &optional origin)
  (let ((category (see-also-category see-also))
        (id (see-also-id see-also)))
    (case (keywordize category)
      ((:module :tutorial :glossary-entry :topic)
       (find-documentation id category))
      (:documentation-file
       (find-documentation (first id) category))
      (:function-documentation
       (vif (pkg (find-package (second id)))         
            (find-documentation (find-symbol (first id) pkg) category)
            (progn 
              (warn "No package ~S in see-also for ~S." (second id) origin)
              nil)))
      (:symbol-doc 
       (destructuring-bind (name package type) id
         (vif (pkg (find-package package))
              (vif (symbol (find-symbol name pkg))
                   (let ((all (find-documentation symbol category)))
                     (let ((docobj (find type all :key 'help:dtype)))
                       docobj
                       ))
                   (progn 
                     (warn "No symbol ~S in package ~S in see-also for ~S."
                           name package origin)
                     nil))
              (progn 
                (warn "No package ~S in see-also for ~S." package origin)
                nil
                ))))
      (otherwise nil)
      ))) 
          
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; For every see-also entry S in an docobj X

;;;  -- verify that the object being pointed at by S exists, or warn
;;;     that it does not, as appropriate
;;;  -- If S is a SYMBOL-DOC reference of the :function, see if
;;;     a FUNCTION-DOCUMENTATION object exists for the same symbol,
;;;     and if so change S to be a FUNCTION-DOCUMENTATION reference
;;;  -- Create backreferences.  If S (in X) points at docobj Y, then
;;;     pushnew onto Y's REFERRED-TO-BY slot X (Note that some S's don't
;;;     point at DOCOBJ's)

(defun see-also-second-pass ()
  (cformatt "Making 2nd pass over all SEE-ALSO references...")
  (let ((all-docobjs (mapcar 'help-match-ref (find-doc-items-if 'identity))))
    (cformatt "~D total documented objects." (length all-docobjs))
    (cformatt "Converting symbol-docs to funcdoc...")
    (loop 
     for docobj in all-docobjs
     with count = 0
     as see-also-list = (help:see-also docobj)
     do
     ;; See if SYMBOL-DOC referrals can be replaced 
     ;; with FUNCTION-DOCUMENTATION referrals.
     ;; Only objects of type DOCUMENTED (vs BASIC-DOC) have actual see-also 
     ;; slot
     (when (typep docobj 'help:documented)
       (setf
        (help:see-also docobj)
        (loop
         for referral in see-also-list 
         collect
         (if (not (eq (see-also-category referral) 'help:symbol-doc))
             referral
           (destructuring-bind (name package type)
               (see-also-id referral)
             (if (not (eq :function type)) 
                 referral
               (let ((symbol (find-symbol name (find-package package))))
                 (when (null symbol) 
                   (error "Internal error.  ~S does not exist!"
                          (s+ package "::" name)))
                 (if (find-documentation symbol 'help:function-documentation)
                     (progn 
                       (incf count)
                       (make-see-also-function-documentation symbol))
                   referral
                   ))))))))
     finally (cformatt "~D symbol-doc items converted to funcdoc" count)
     )
    (cformatt 
     "Verifying see-also references exist and creating back-references")
    (loop 
     with referent-count = 0
     for docobj in all-docobjs 
     as see-also-list = (help:see-also docobj)
     do
     ;; Verify that referred-to objects exist.
     (loop for referral in see-also-list
           as category = (see-also-category referral)
           when (member category *documentation-types*) 
           do
           (let ((referent (see-also->docobj referral docobj)))
             (if (null referent)
                 (progn
                   (cformatt "~S has a SEE ALSO entry~%~S," docobj referral)
                   (cformatt "  which is not a known, documented, entity.")
                   )
               ;; If a doc-object X has a see-also reference to doc-object Y,
               ;; then Y is given a REFERRED-TO-BY reference to X.
               (progn
                 (incf referent-count)
                 (pushnew docobj (help:referred-to-by referent))
                 ))))
     finally (cformatt "~D back references created." referent-count)
     ))
  ;; Clean out UNCATEGORIZED module of functions that belong to other modules.
  (cformatt "Cleaning up uncategorized module...")
  (let* ((uc (help:find-documentation *uncategorized-key* 'help:module))
         (other-modules (remove uc (modules)))
         (categorized-functions
          (loop for f in (help:functions uc) 
                when (categorized-function? f other-modules)
                collect f
                )))
    (setf (help:functions uc) 
          (set-difference (help:functions uc) categorized-functions))
    (cformatt "~D functions removed from uncategorized module."
              (length categorized-functions)
              ))    
  (cformatt "2nd pass complete.")
  )

(defun categorized-function? (f &optional (modules (modules)))
  (loop for module in modules do 
        (when (find f (help:functions module))
          (return t))
        finally (return nil)
        ))

(defun function-in-any-module-except-uncategorized? (f) 
  (let* ((uc (help:find-documentation *uncategorized-key* 'help:module))
         (other-modules (remove uc (modules))))
    (categorized-function? f other-modules)
    ))



