;;;; -*- mode: Lisp; Syntax: Common-Lisp; Package: nvpl; -*-

(in-package :nvpl) 

;;; +=========================================================================+
;;; | Copyright (c) 2005 JP Massar, Jeff Elhai, Mark Slupesky, Peter Seibel   |
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

;;; Author: JP Massar. 

(defun create-snippet-from-template (parent template &aux stype)
  (destructuring-bind 
      (identifier ttype subtemplates return-type &rest keys-and-flags)
      template
    (setq stype (snippet-keyword->snippet-type ttype))
    (unless (eq stype 'call-snippet)
      (vpl-internal-error "Templates must be call forms!"))
    (let ((*parent* parent)
          (*property-list* 
           (create-property-list-from-keys-and-flags stype keys-and-flags)))
      (let* ((new-snippet 
              (make-instance 
               stype 
               :value identifier
               :return-type return-type 
               )))
        (setf (snippet-label new-snippet) (string-downcase (string identifier)))
        (set-snippet-info new-snippet :call-type ttype)
        (setf (snippet-children new-snippet) 
              (create-subtemplate-snippets subtemplates new-snippet))
        (set-snippet-info new-snippet :template subtemplates)
        ;; (attach-hole-snippets-to-parent new-snippet)
        (attach-hidden-positions-to-parent new-snippet)
        new-snippet
        ))))

(defun create-property-list-from-keys-and-flags (snippet-type keys-and-flags)
  (multiple-value-bind (legal-keys legal-flags)
      (legal-keys-and-flags snippet-type)
    (loop 
     with kflist = keys-and-flags 
     as next = (first kflist)
     until (null kflist)
     nconc
     (cond
      ((member next legal-keys) 
       (prog1 
           (list next (second kflist))
         (pop kflist) (pop kflist)
         ))
      ((member next legal-flags) 
       (prog1
           (list next t)
         (pop kflist)
         ))
      (t (vpl-internal-error "Unrecognized key or flag: ~S" next))
      ))))

(defun separate-subtemplate-args-from-keys-and-flags (snippet-type subtemplate)
  (multiple-value-bind (legal-keys legal-flags)
      (legal-keys-and-flags snippet-type)
    (loop for elem in (cdr subtemplate)
          for j from 1 
          do 
          (when (or (member elem legal-keys) (member elem legal-flags))
            (return 
             (values 
              (subseq subtemplate 0 j)
              (subseq subtemplate j)
              )))
          finally (return (values subtemplate nil))
          )))

(defun create-subtemplate-snippets (subtemplates parent)
  (mapcar 
   (lambda (subtemplate) (create-subtemplate-snippet parent subtemplate))
   subtemplates
   ))

(defun create-subtemplate-snippet (parent subtemplate &aux stype)
  (unless (listp subtemplate)
    (vpl-internal-error 
     "Subtemplate is not a list!  (in create-subtemplate-snippet)"))
  (unless (keywordp (first subtemplate)) 
    (vpl-internal-error 
     "Subtemplate has no keyword!  (in create-subtemplate-snippet)"))
  (vif (f (get (first subtemplate) :subtemplate-alias-expander))
       (progn
         (vdbg "Doing alias expansion..~%")
         (setq subtemplate (funcall f subtemplate))
         (create-subtemplate-snippet parent subtemplate))
       (progn
         (setq stype (snippet-keyword->snippet-type (first subtemplate)))
         (vdbg "Stype: ~S~%" stype)
         (multiple-value-bind (subtemplate-args keys-and-flags)
             (separate-subtemplate-args-from-keys-and-flags stype subtemplate)
           (vdbg "subtemplate args: ~S, keys and flags: ~S~%" 
                   subtemplate-args keys-and-flags)
           (let ((property-list 
                  (create-property-list-from-keys-and-flags 
                   stype keys-and-flags)))
             (let ((*parent* parent)
                   (*subtemplate* subtemplate)
                   (*subtemplate-args* subtemplate-args)
                   (*property-list* property-list))
               (make-instance stype)
               ))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro def-snippet-init (snippet-type &body body)
  `(defmethod initialize-instance :after ((obj ,snippet-type) &rest initargs)
     (declare (ignore initargs))
     (unless (or *copy-in-progress* *disable-snippet-initialization*)
       ,@body
       )))

(def-snippet-init constant-snippet 
  (let ((c (second *subtemplate-args*)))
    (setf (snippet-value obj) c)
    (setf (snippet-return-type obj) (type-of (if (listp c) c (eval c))))
    obj
    ))

(def-snippet-init literal-snippet 
  (let ((string (second *subtemplate-args*)))
    (setf (snippet-value obj) string)
    obj
    ))

(def-snippet-init value-form-snippet 
  (destructuring-bind (stype label return-type) *subtemplate-args*
    (declare (ignore stype))
    (setf (snippet-value obj) +hole+)
    (setf (snippet-label obj) label)
    (setf (snippet-return-type obj) return-type)
    (set-snippet-property obj :hole-template *subtemplate*)
    (set-snippet-property obj :hole-open nil)
    (set-snippet-property obj :contents "")
    obj
    ))

(def-snippet-init argument-snippet 
  (let ((label (second *subtemplate-args*)))
    (setf (snippet-value obj) +hole+)
    (setf (snippet-label obj) label)
    (set-snippet-property obj :hole-open nil)        
    (set-snippet-property obj :contents "")
    obj
    ))

(def-snippet-init progn-snippet 
  (let ((subtemplates (cdr *subtemplate-args*)))
    (setf (snippet-value obj) nil)
    (setf (snippet-children obj) 
          (create-subtemplate-snippets subtemplates obj))
    obj
    ))

(def-snippet-init aggregate-snippet
  (destructuring-bind (stype label subtemplate) *subtemplate-args*
    (declare (ignore stype))
    (setf (snippet-value obj) subtemplate)
    (setf (snippet-label obj) label)
    (when (get-snippet-property obj :display-one-hole) 
      (setf (snippet-children obj)
            (list (create-subtemplate-snippet obj subtemplate))))
    obj
    ))

(def-snippet-init call-snippet obj)

(def-snippet-init choice-snippet obj)

(def-snippet-init uniform-choice-snippet 
  (vdbg "initializing uniform-choice-snippet~%")
  (destructuring-bind (stype label choice-list) *subtemplate-args*
    (declare (ignore stype))
    (setf (snippet-value obj) choice-list)
    (setf (snippet-label obj) label)
    (set-snippet-property 
     obj :selected-choices 
     (mapcar (lambda (x) (list x nil)) (iota (length choice-list))))
    (when (get-snippet-property obj :show-first-choice)
      (select-choice (snippet-id obj) 1)
      (setq *modified-snippet* nil))
  ; ******************   NEW
    (when (get-snippet-property obj :display-one-hole) 
      (setf (snippet-children obj)
            (list (create-subtemplate-snippet obj (SECOND (FIRST choice-list))))))
  ; ****************** 
    (vdbg "done uniform-choice-snippet~%")
    obj
    ))

(def-snippet-init keys-and-flags-snippet 
  (destructuring-bind (stype choice-list) *subtemplate-args*
    (declare (ignore stype))
    (setf (snippet-value obj) choice-list)
    (let* ((key-records 
            (remove-if-not (lambda (x) (eq (first x) :keyword)) choice-list))
           (flag-records
            (remove-if-not (lambda (x) (eq (first x) :flag)) choice-list))
           (keys (mapcar 'second key-records))
           (flags (mapcar 'second flag-records))
           (subtemplates 
            (mapcar 
             (lambda (info) 
               (ecase (first info) 
                 (:keyword
                  (destructuring-bind (which name default type possible-values)
                      info
                    (declare (ignore which))
                    `(:keyword ,name ,type 
                      (:literal ,name :class :keyword)
                      (:form "value" ,(or type t))
                      :default ,default
                      :possible-values ,possible-values
                      )))
                 (:flag info)
                 ))
             choice-list
             ))
           (ordered-names (mapcar 'second choice-list))
           (key-indices 
            (mapcar 
             (lambda (x) 
               (position x ordered-names :key 'string :test 'string-equal))
             keys
             ))
           (flag-indices 
            (mapcar 
             (lambda (x) 
               (position x ordered-names :key 'string :test 'string-equal))
             flags
             )))
      (set-snippet-property obj :flags-present nil)
      (set-snippet-property obj :keys-present nil)
      (set-snippet-property obj :subtemplates subtemplates)
      (set-snippet-property obj :ordered-names ordered-names)
      (set-snippet-property obj :keywords keys)
      (set-snippet-property obj :flags flags)
      (set-snippet-property obj :key-indices key-indices)
      (set-snippet-property obj :flag-indices flag-indices)
      obj
      )))

(def-snippet-init keyword-snippet 
  (destructuring-bind (stype name type literal-subtemplate value-subtemplate)
      *subtemplate-args* 
    (declare (ignore stype type))
    (setf (snippet-value obj) name)
    (setf (snippet-children obj) 
          (list 
           (create-subtemplate-snippet obj literal-subtemplate)
           (create-subtemplate-snippet obj value-subtemplate)
           ))))

(def-snippet-init flag-snippet
  (destructuring-bind (stype name) *subtemplate-args*
    (declare (ignore stype))
    (setf (snippet-value obj) name)
    ))

;; Symbol-snippets are not created by any templates 
(def-snippet-init symbol-snippet 
  (vdbg "Initialize-instance for symbol-snippet~%")
  (destructuring-bind (stype symbol) *subtemplate-args*
    (declare (ignore stype))
    (setf (snippet-value obj) symbol)
    (setf (snippet-return-type obj) (declare-symbol-type symbol))
    (vdbg "Done initalize-instance for symbol-snippet~%")
    obj
    ))
 
(defun declare-symbol-type (symbol)
  (declare (ignore symbol))
  t)

(defun create-constant-snippet (parent constant &rest keys-and-flags)
  (create-subtemplate-snippet 
   parent (copy-list `(:constant ,constant ,@keys-and-flags))
   ))

(defun create-symbol-snippet (parent symbol &rest keys-and-flags)
  (let ((*parent* parent)
        (*property-list* keys-and-flags))
    (make-instance 
     'symbol-snippet 
     :value symbol
     :return-type (declare-symbol-type symbol)
     )))

;; Toplevel output snippets are not created by templates...
(defun create-toplevel-output-snippet (index &rest keys-and-flags)
  (let ((*parent* (uvs-rs *vpl-state*))
        (*property-list* keys-and-flags))
    (let ((obj 
           (make-instance
            'toplevel-output-snippet
            :label (formatn "~D> " index)
            )))
      (set-snippet-property obj :output-index index)
      obj
      )))

;; Output value snippets are not created by templates...
(defun create-output-value-snippet (parent value &rest keys-and-flags)
  (let ((*parent* parent)
        (*property-list* keys-and-flags))
    (make-instance
     'output-value-snippet
     :value value
     :return-type (type-of value)
     )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun snippet-is-hole? (snippet)
  (typep snippet 'form-snippet)
  )

(defun snippet-is-open-hole? (snippet)
  (and (snippet-is-hole? snippet) (get-snippet-property snippet :hole-open)))

(defun snippet-is-closed-hole? (snippet)
  (and (snippet-is-hole? snippet) (not (snippet-is-open-hole? snippet))))

;; 1-based access of snippet children list
(defun nth-snippet-child (snippet n) (nth (1- n) (snippet-children snippet)))

;; Accessing the arguments of a function call (first child is
;; name of function)

(defun first-snippet-arg (snippet) (nth-snippet-child snippet 2)) 

(defun second-snippet-arg (snippet) (nth-snippet-child snippet 3))

(defun third-snippet-arg (snippet) (nth-snippet-child snippet 4))

(defun fourth-snippet-arg (snippet) (nth-snippet-child snippet 5))

(defun fifth-snippet-arg (snippet) (nth-snippet-child snippet 6))

(defun sixth-snippet-arg (snippet) (nth-snippet-child snippet 7))

(defun get-snippet-info (snippet property) 
  (get-snippet-property snippet property))

(defun get-snippet-property (snippet property) 
  (getf (snippet-properties snippet) property))

(defun set-snippet-info (snippet property new-value)
  (set-snippet-property snippet property new-value))

(defun set-snippet-property (snippet property new-value) 
  (setf (getf (snippet-properties snippet) property) new-value))

;; Search the current workspace for a snippet corresponding to the ID

(defun find-snippet-in-workspace (sid &optional (error-if-not-found? t))
  (find-ws-snippet-by-id sid error-if-not-found?))

(defun find-snippet-by-id-from-roots (sid &optional (error-if-not-found? t))
  (find-ws-snippet-by-id sid error-if-not-found?))

(defun find-snippet-in-output-history (sid &optional (error-if-not-found? t))
  (find-rs-snippet-by-id sid error-if-not-found?))

(defun find-snippet-in-results-area (sid &optional (error-if-not-found? t))
  (find-snippet-in-output-history sid error-if-not-found?))

(defun find-snippet-in-workspace-or-output-history 
       (sid &optional (error-if-not-found? t))
  ;; (setq *foo* 0)
  (block exit
    (vwhen (snippet (find-snippet-in-workspace sid nil))
      (return-from exit (values snippet :workspace)))
    ;; (setq *foo* 1)
    (vwhen (snippet (find-snippet-in-output-history sid nil))
      (return-from exit (values snippet :results)))
    ;; (setq *foo* 2)
    (when (null error-if-not-found?)
      (return-from exit (values nil :neither)))
    (progn
      ;; (print 'foo)
      ;; (setq *foo* 3)
      (vpl-internal-error
       "Could not find snippet corresponding to ID ~A anywhere" sid
       ))))

(defun find-snippet-anywhere (sid &optional (error-if-not-found? t))
  (find-snippet-by-id sid error-if-not-found?))

;; Search within a snippet for a subsnippet corresponding to the ID

(defun find-snippet-by-id-in-snippet 
       (snippet sid &optional (error-if-not-found? t))
  (block exit
    (if (eql sid (snippet-id snippet)) 
        snippet 
      (let ((value (snippet-value snippet))
            (children (snippet-children snippet)))
        (when (typep value 'snippet) 
          (vwhen (s (find-snippet-by-id-in-snippet value sid nil))
            (return-from exit s)
            ))
        (loop for child in children do 
              (vwhen (s (find-snippet-by-id-in-snippet child sid nil))
                (return-from exit s)
                ))
        (when error-if-not-found? 
          (vpl-internal-error 
           "Could not find snippet with ID ~A in ~A" sid snippet
           ))))))

(defun snippet-is-unexpanded? (snippet)
  (null (snippet-children snippet)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun copy-workspace ()
  ;; (treeprint-snippet (uvs-ws *vpl-state*))
  (pcopy-snippet (uvs-ws *vpl-state*) nil t))

(defun copy-results ()
  (pcopy-snippet (uvs-rs *vpl-state*) nil t))

(defmethod xcopy-snippet ((obj snippet) new-snippet) 
  (let ((v (snippet-value obj)))
    (setf (snippet-value new-snippet)
          (cond
           ((atom v) v)
           ((listp v) (copy-tree v))
           ))))
  
(defmethod xcopy-snippet ((obj return-value-snippet) new-snippet) 
  (setf (snippet-return-type new-snippet) (snippet-return-type obj))
  (call-next-method))

(defmethod xcopy-snippet ((obj labeled-snippet) new-snippet)
  (setf (snippet-label new-snippet) (copy-seq (snippet-label obj)))
  (call-next-method))

(defun pcopy-snippet (s new-parent &optional (new-id? nil))
  ;; (vdbg "In pcopy-snippet...~%")
  (let ((*new-snippet-id?* new-id?)
        (*parent* new-parent)
        (*property-list* nil)
        (*copy-in-progress* t))
    ;; (vdbg "Allocating new snippet of type ~A~%" (snippet-type s))
    (let ((new-snippet (make-instance (snippet-type s))))
      ;; (vdbg "Allocated new snippet of type ~A...~%" (snippet-type s))
      ;; copy the basic atomic snippet info 
      (xcopy-snippet s new-snippet)
      ;; copy the property list deeply
      ;; (vdbg "Copying properties...~%")
      (setf (snippet-properties new-snippet) 
            (mapcar
             (lambda (other-obj)
               (typecase other-obj
                 (snippet (pcopy-snippet other-obj new-parent new-id?))
                 (atom other-obj)
                 (list (copy-tree other-obj))
                 ))
             (snippet-properties s)
             ))
      ;; Now copy the children making them
      ;; point to the copied snippet as their parent instead of
      ;; the original (S)
      ;; (vdbg "Copying children...~%")
      (setf
       (snippet-children new-snippet)
       (mapcar 
        (lambda (child-snippet) 
          (pcopy-snippet child-snippet new-snippet new-id?))
        (snippet-children s)
        ))
      new-snippet
      )))
            
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; If we ever did surgery on snippets with hidden nodes this wouldn't work.
;;; But the claim is that we never do such a thing; the whole point of hidden
;;; nodes is to hide stuff that has to be there, the user cannot actually
;;; remove it.  

(defun attach-hidden-positions-to-parent (snippet)
  (let ((children (snippet-children snippet)))
    (unless (listp children)
      (vpl-internal-error "Children of snippet not a list!"))    
    (loop for child in children do 
          (when (get-snippet-property child :hidden-node)
            (tag-snippet-as-hidden child))
          (attach-hidden-positions-to-parent child)
          )))

;;; To determine whether a snippet replaced a hole, when a template
;;; or subtemplate is instantiated by creating a snippet, we store
;;; the template or subtemplate that created it on the property list of the
;;; snippet in the :template slot.  Then, if a hole is replaced by a snippet
;;; the information about how to recreate the hole still exists in the 
;;; parent; we just have to find it based on the type of
;;; parent node.  

(defun snippet-replaced-hole? (snippet)
  (let ((template (find-original-hole-subtemplate snippet :no-template-found)))
    (if (eq template :no-template-found) nil t)
    ))

(defun find-original-hole-subtemplate (snippet &optional (if-no-hole? nil))
  (vdbg "In find-original-hole...~%")
  (let ((original-template
         (find-original-template (snippet-parent snippet) snippet)))
    (cond 
     ((template-is-hole? original-template) original-template)
     (if-no-hole? if-no-hole?) 
     (t (vpl-internal-error 
         "Expected hole but original template is not a hole!"
         )))))

(defmethod find-original-template ((parent t) snippet)
  (get-snippet-property snippet :template)
  )

(defmethod find-original-template ((parent toplevel-ws-snippet) snippet)
  (get-snippet-property snippet :hole-template)
  )

(defmethod find-original-template ((parent toplevel-rs-snippet) snippet)
  (get-snippet-property snippet :hole-template)
  )

;;; An aggregate node stores the sole template of its multiple children 
;;; in the snippet-value slot.  We could get the information from the 
;;; :template slot as well.
(defmethod find-original-template 
           ((parent aggregate-snippet) snippet)
  (declare (ignore snippet))
  (snippet-value parent)
  )

;;; The template for a keyword currently does not contain an actual 
;;; subtemplate for the keyword value.  This is probably a mistake.  
;;; In any case when we instantiate the keyword we put on its 
;;; :template slot something that looks like 
;;; (:keyword <name> <type> (:literal <keyword>) (:form <label> <type>))
;;; Then we pull off the fourth or fifth element of the template as the
;;; subtemplate for the keyword literal or value
(defmethod find-original-template 
           ((parent keyword-snippet) (snippet snippet))
  (fifth (get-snippet-property parent :template)) 
  )

(defmethod find-original-template 
           ((parent keyword-snippet) (snippet literal-snippet))
  (fourth (get-snippet-property parent :template))
  )


(defmethod find-original-template 
           ((parent keys-and-flags-snippet) snippet)
  (get-snippet-property snippet :template))

(defmethod find-original-template 
           ((parent choice-snippet) snippet)
  (declare (ignore snippet))
  (vdbg "SV: ~S~%" (snippet-value parent))
  (case (get-snippet-property parent :choice-type)
    (:optional-arg (second (first (snippet-value parent))))
    (:var-or-list-of-vars (second (first (snippet-value parent))))
    (otherwise 
     (vpl-internal-error
      "Not implemented! Didn't think it happened! (find-original-template)")
     )))

(defmethod find-original-template ((parent call-snippet) snippet)
  (vdbg "find original template (call snippet)~%")
  (let ((pos (position snippet (snippet-children parent))))
    (unless (integerp pos) 
      (vpl-internal-error 
       "Not possible. Child is not in parent! (find-original-template 1)"))
    (let ((call-subtemplates (get-snippet-info parent :template)))
      (vdbg "Call subtemplates: ~S~%" call-subtemplates)
      (nth pos call-subtemplates)
      )))

(defmethod find-original-template ((parent progn-snippet) snippet)
  (let ((pos (position snippet (snippet-children parent))))
    (unless (integerp pos) 
      (vpl-internal-error 
       "Not possible.  Child is not in parent! (find-original-template 2)"))
    (let ((call-subtemplates (get-snippet-info parent :template)))
      ;; Add one to skip over the :progn in the template
      (nth (1+ pos) call-subtemplates)
      )))
  
(defun template-is-hole? (template)
  (case (first template)
    ((:form :arg :place) t)
    (otherwise nil)
    ))



             
             
