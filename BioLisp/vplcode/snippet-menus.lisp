;;; -*- mode: Lisp; Syntax: Common-Lisp; Package: nvpl; -*-

(in-package :nvpl)

;;; +=========================================================================+
;;; | Copyright (c) 2006 John Myers, JP Massar                                |
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

;; Author: JP Massar.

(defparameter *snippet-menu-info* 
  (create-hash-table 
   (mapcar 
    (lambda (x) (list (first x) (cdr x)))
    '(
      ;; universal
      (:help "Help" help-needed? help-function)
      ;; specific to calls and symbols and constants, so node-required 
      (:execute "Execute" execute-needed? execute-function)
      ;; only when not collapsed, so this is contextually-universal
      (:collapse "Collapse" collapse-needed? collapse-snippet-display)
      ;; only when not collapsed, so this is contextually-universal
      (:named-collapse 
       "Collapse with name" collapse-needed? named-collapse-snippet-display)
      ;; only when collapsed, so this is contextually-universal
      (:expand "Expand" expand-needed? expand-snippet-display)
      ;; only applicable to :agg nodes, and always applicable, so node-required
      (:add-another "Add another" add-another-needed? another-subform)
      ;; only applicable to :agg nodes, and always applicable, so node-required
      (:add-two-more "Add two more" add-two-more-needed? two-more-subforms)
      ;; only applicable to certain nodes that are immediate children
      ;; of :agg nodes, and so contextually-universal
      (:add-left "Add left" add-left-needed? add-subform-left)
      ;; only applicable to certain nodes that are immediate children
      ;; of :agg nodes, and so contextually-universal
      (:add-right "Add right" add-right-needed? add-subform-right)
      ;; specific to output nodes and always there so node-required
      (:describe "Describe" describe-needed? describe-output)
      ;; probably node-required
      (:copy "Copy" copy-needed? copy-function)
      ;; probably node-required
      (:cut "Cut" cut-needed? cut-function)
      ;; specific to holes and always allowed so node-required
      (:paste "Paste" paste-needed? paste-function)
      ;; specific to constants and symbols, but also context-specific 
      ;; (in the sense of the constant or symbol having been put there
      ;; in place of a hole), so node-optional
      (:edit "Edit" edit-needed? edit-snippet)
      ;; specific to toplevel output nodes and context-specific so node-optional
      (:show-printout "Show printout" show-printout-needed? show-printout)
      ;; specific to the node, so node-required
      (:insert "Insert" insert-needed? insert-function)
      ;; specific to the node but context dependent, so node-optional
      (:cut/insert "Cut/Insert" cut-insert-needed? cut-insert-function)
      ;; specific to particular nodes, so node-required 
      (:surround-with 
       "Surround with" surround-with-needed? surround-with-function)
      ;; specific to particular nodes but context dependent, so node-optional
      (:unsurround "Unsurround" unsurround-needed? unsurround-function)
      ;; specific to particular nodes, so node-required
      (:show-code "Show code" show-code-needed? show-code-function)
      ;; universal but context-specific (if the node has a :reveal-label value)
      (:hide "Hide" hide-needed? hide-function)
      ;; universal but context-specific (if any child is hidden)
      (:reveal-all "Reveal all" reveal-needed? reveal-function)
      ;; specific to particular nodes and context-dependent so node-optional
      (:clear "Clear" clear-needed? clear-snippet-op)
      ;; specific to particular nodes and context-dependent so node-optional
      (:delete "Delete" delete-needed? delete-snippet-op)
      ;; specific to value-form snippets, not context-dependent but 
      ;; possibly dependent on the type of value allowed in the hole,
      ;; so node-optional
      (:multiline-input "Multiline input"
       multiline-input-needed? multiline-input-function)
      ;; Applicable to all collapsed nodes 
      (:label-collapsed-box "(Re)Name box" 
       label-collapsed-box-needed? label-collapsed-box-function)
      (:unlabel-collapsed-box "Remove name"
       unlabel-collapsed-box-needed? unlabel-collapsed-box-function)
      ;; specific to particular nodes and context-dependent so node-optional
      (:monitor "Monitor (on/off)" monitor-needed? monitor-function)
      ;; specific to output nodes, so node-optional
      (:view "View" view-needed? view-function)
      ;; specific to output nodes, so node-optional
      (:view-code "View code" view-code-needed? view-code-function)
      ;; specific to nodes which can be executed, so node optional
      (:results "Results" results-needed? results-function)
      ))))

(defparameter *toplevel-snippet-menu-options* nil)

(defun create-snippet-menu-and-delete/clear-flag (snippet)
  (let ((visualization-type (snippet-visualization-type snippet)))
    (ecase visualization-type 
      (:jbml-main-box-menu 
       (values 
        (create-snippet-menu snippet) 
        (delete-or-clear-box-flag-for-snippet snippet)
        ))
      (:jbml-options-menu 
       (values
        (create-snippet-menu snippet)
        nil
        )))))

(defun snippet-menu-option-menu-entry (option)
  (let ((x (gethash option *snippet-menu-info*)))
    (menu-item (first x) (third x))
    ))

(defun snippet-menu-record (option) (gethash option *snippet-menu-info*))

(defun snippet-menu-record-menu-entry (menu-record)
  (menu-item (first menu-record) (third menu-record)))

(defun snippet-menu-record-predicate (menu-record)
  (second menu-record))

#+trace
(trace universal-snippet-menu-items 
       contextually-universal-snippet-menu-items
       node-required-menu-items
       node-optional-menu-items
       node-set-menu-items
       compile-snippet-menu)

(defmethod create-snippet-menu ((snippet t))
  (vdbg "Creating menu for snippet type ~A~%" (type-of snippet))
  (let ((collapsed? (collapsed? snippet)))
    (compile-snippet-menu 
     (list* 
      (new-useless-menu-id) 
      (snippet-visualization-type snippet)
      (snippet-menu-title snippet)
      (cond
       ((hidden? snippet)
        (vpl-internal-error 
         "Why are we calling CREATE-SNIPPET-MENU on a hidden snippet!"))
       (t
        (nconc 
         (universal-snippet-menu-items snippet)
         (node-required-menu-items snippet)
         (contextually-universal-snippet-menu-items snippet)
         (node-optional-menu-items snippet)
         (unless collapsed? (node-set-menu-items snippet))
         (create-reveal-particular-menu-items snippet)
         )))))))

(defun only-one-kf-left? (kf-snippet)
  (let* ((sv (snippet-value kf-snippet))
         (n (length sv))
         (keys-present (get-snippet-property kf-snippet :keys-present))
         (nkp (length keys-present))
         (flags-present (get-snippet-property kf-snippet :flags-present))
         (nfp (length flags-present)))
    (= 1 (- n (+ nkp nfp)))
    ))
  

(defmethod create-snippet-menu ((snippet keys-and-flags-snippet))
  (if (or (null *multiselect-enable*) (only-one-kf-left? snippet)) 
      (call-next-method)
    (progn
      (vdbg "Creating multimenu for snippet type ~A~%" (type-of snippet))
      (let ((collapsed? (collapsed? snippet)))
        (compile-snippet-multimenu 
         (list
          (new-useless-menu-id) 
          (snippet-visualization-type snippet)
          (snippet-menu-title snippet)
          (cond
           ((hidden? snippet)
            (vpl-internal-error 
             "Why are we calling CREATE-SNIPPET-MENU on a hidden snippet!"))
           (t
            ;; the set of direct action choices
            (nconc 
             (universal-snippet-menu-items snippet)
             (contextually-universal-snippet-menu-items snippet)
             (node-required-menu-items snippet)
             (node-optional-menu-items snippet)
             (create-reveal-particular-menu-items snippet)
             )))
          ;; The set of multimenu choices 
          (if collapsed? nil (node-set-menu-items snippet))
          ))))))

(defmethod snippet-visualization-type ((s snippet)) :jbml-main-box-menu)
(defmethod snippet-visualization-type ((s choice-snippet)) :jbml-options-menu)
(defmethod snippet-visualization-type ((s aggregate-snippet))
  :jbml-options-menu)

(defmethod snippet-visualization-type ((s keys-and-flags-snippet))
  (if (and *multiselect-enable* (not (only-one-kf-left? s)))
      :jbml-kf-multiselect-menu
    (call-next-method)
    ))

(defmethod snippet-menu-title ((s snippet)) "")
(defmethod snippet-menu-title ((s choice-snippet))
  (if (get-snippet-property s :collapsed?)
      (vif (cname (get-snippet-property s :collapsed-name))
           cname
           "Collapsed")
    (vif (text (snippet-label s))
         (string text)
         "Options"
         )))
(defmethod snippet-menu-title ((s aggregate-snippet)) 
  (vif (text (get-snippet-property s :options-label))
       (string text)
       "More..."
       ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *universal-snippet-menu-options* '(:help))

(defun universal-snippet-menu-items (snippet)
  (declare (ignore snippet))
  (list (menu-item "Help" 'help-function))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *contextually-universal-snippet-menu-options*
  '(:collapse :named-collapse :expand :hide :reveal-all :add-left 
    :add-right :label-collapsed-box :unlabel-collapsed-box))

(defun contextually-universal-snippet-menu-items (snippet)
  (nconc 
   (loop for option in *contextually-universal-snippet-menu-options* 
         nconc
         (let ((record (snippet-menu-record option)))
           (cond
            ((eq option :add-left) 
             (when (funcall (snippet-menu-record-predicate record) snippet)
               (list 
                (snippet-menu-option-menu-entry :add-left)
                (snippet-menu-option-menu-entry :add-right)
                )))
            ((eq option :add-right) nil)
            (t 
             (when (funcall (snippet-menu-record-predicate record) snippet)
               (list (snippet-menu-record-menu-entry record))
               )))))
   ))

(defun collapse-needed? (snippet)
  (collapse-or-expand-needed? snippet :collapse))

(defun expand-needed? (snippet)
  (collapse-or-expand-needed? snippet :expand))

(defun collapse-or-expand-needed? (snippet which)
  (flet ((c-or-e () 
           (ecase which
             (:collapse (not (collapsed? snippet)))
             (:expand (collapsed? snippet))
             )))
    (typecase snippet
      (aggregate-snippet 
       (when (snippet-children snippet) (c-or-e)))
      (choice-snippet 
       (cond 
        ((get-snippet-property snippet :literal) nil)
        (t (when (snippet-children snippet) (c-or-e)))
        ))
      (leaf-snippet nil)
      (form-snippet nil)
      (call-snippet (c-or-e))
      (otherwise nil)
      )))

(defun hide-needed? (snippet) (get-snippet-info snippet :reveal-label))

(defun reveal-needed? (snippet) (get-snippet-info snippet :hidden-nodes))

(defun add-left-needed? (snippet)
  (typep (snippet-parent snippet) 'aggregate-snippet))

(defun create-reveal-particular-menu-items (snippet)
  (let ((reveal-options 
         (loop for child in (snippet-children snippet)
               for index from 1 
               when (get-snippet-info child :hidden-node)
               collect 
               (let ((option-string 
                      (s+ "Reveal " (get-snippet-info child :reveal-label)))
                     (operator-name 
                      (create-reveal-component-operator index)
                      ))
                 (menu-item option-string operator-name)
                 ))))
    (when reveal-options 
      (append (list (menu-item "-----------------" 'noop)) reveal-options)
      )))

(defun create-reveal-component-operator (count)
  (numeric-opcode-operator 
   'reveal-component-operator
   (lambda (count) 
     (intern (formatn "REVEAL-COMPONENT~D-OPERATOR" count) *vpl-package*))
   count
   ))

;; Any box which needs an expand option is by definition collapsed and therefore
;; needs as well an option to name it
(defun label-collapsed-box-needed? (snippet) (expand-needed? snippet))

(defun unlabel-collapsed-box-needed? (snippet)
  (and (expand-needed? snippet) (get-snippet-property snippet :collapsed-name)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *node-required-snippet-menu-options*
  '(:execute :describe :copy :cut :paste :insert :surround-with :show-code 
    :add-another :add-two-more :results))

(defmethod node-required-menu-items ((s snippet)) nil)

(defmethod node-required-menu-items ((s constant-snippet))
  (node-required-constant-or-symbol))

(defmethod node-required-menu-items ((s symbol-snippet))
  (case (get-snippet-property s :symbol-node-type)
    (:place (node-required-constant-or-symbol))
    (:arg (node-required-arg s))
    (otherwise (node-required-constant-or-symbol))
    ))

(defun node-required-constant-or-symbol ()
  (mapcar 
   (lambda (option) (snippet-menu-option-menu-entry option))
   '(:execute :copy :cut :insert :surround-with)
   ))

(defun node-required-arg (s)
  (declare (ignore s))
  (mapcar 
   (lambda (option) (snippet-menu-option-menu-entry option))
   '(:copy :cut :insert)
   ))

(defmethod node-required-menu-items ((s value-form-snippet))
  (nconc 
   (mapcar 
    (lambda (option) (snippet-menu-option-menu-entry option))
    '(:paste :surround-with)
    )
   ;; In general you cannot cut holes, but it is possible to have a 
   ;; hole at toplevel, and then you should be able to excise it
   (when (toplevel-ws-snippet? s) 
     (list (snippet-menu-option-menu-entry :cut))
     )))

(defmethod node-required-menu-items ((s argument-snippet))
  (mapcar 
   (lambda (option) (snippet-menu-option-menu-entry option))
   '(:paste)
   ))

;; Technically should not be able to execute anything with holes.  
;; But to check whether a call form had any holes would require searching
;; the entire child tree.
(defmethod node-required-menu-items ((s call-snippet))
  (mapcar 
   (lambda (option) (snippet-menu-option-menu-entry option))
   '(:execute :copy :cut :insert :surround-with :show-code :results)
   ))

;; this technically makes :add-another and :add-two-more node-optional
;; instead of node-required... 
(defmethod node-required-menu-items ((s aggregate-snippet))
  (unless (collapsed? s)
    (mapcar 
     (lambda (option) (snippet-menu-option-menu-entry option))
     '(:add-another :add-two-more)
     )))

(defmethod node-required-menu-items ((s choice-snippet)) nil)

(defmethod node-required-menu-items ((s toplevel-output-snippet))
  (node-required-output))

(defmethod node-required-menu-items ((s output-value-snippet))
  (node-required-output))

(defun node-required-output ()
  (mapcar 
   (lambda (option) (snippet-menu-option-menu-entry option))
   '(:describe :copy)
   ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *node-optional-snippet-menu-options*
  '(:cut/insert :edit :show-printout :unsurround
    :clear :delete :multiline-input :revisit-url
    :monitor :view :view-code))

(defmethod node-optional-menu-items ((s snippet)) nil)

(defmethod node-optional-menu-items ((s constant-snippet))
  (let ((value (snippet-value s))
        (add-view? nil))
    (cond
     ((symbolp value) 
      (when (boundp value)
        (let ((constant-value (symbol-value value)))
          (unless (not-viewable? constant-value)
            (setq add-view? t)
            ))))
     ((not (or (numberp value) (characterp value) (stringp value)))
      (setq add-view? t)
      ))
    (if add-view? 
        (cons
         (snippet-menu-option-menu-entry :view)
         (node-optional-constant-or-symbol s)
         )
      (node-optional-constant-or-symbol s)
      )))
        
(defmethod node-optional-menu-items ((s symbol-snippet))
  (let ((symbol (snippet-value s)))
    (if (and (boundp symbol)
             (not (not-viewable? (symbol-value symbol))))
        (cons 
         (snippet-menu-option-menu-entry :view)
         (node-optional-constant-or-symbol s)
         )
      (node-optional-constant-or-symbol s)
      )))

(defun not-viewable? (value)
  (or (symbolp value) (numberp value) (characterp value) (stringp value)))

(defun node-optional-constant-or-symbol (s)
  (remove-if 
   'null
   (list 
    (when (snippet-is-cuttable? s)
      (snippet-menu-option-menu-entry :cut/insert))
    (when (snippet-replaced-hole? s) 
      (snippet-menu-option-menu-entry :edit))
    (when (snippet-is-unsurroundable? s) 
      (snippet-menu-option-menu-entry :unsurround))
    ;; no sense monitoring constants and cannot yet monitor 
    ;; variables which are being assigned to
    (unless (or (member (get-snippet-property s :symbol-node-type) 
                        '(:arg :place))
                (subtypep (type-of s) 'constant-snippet))
      (snippet-menu-option-menu-entry :monitor))
    )))

(defun snippet-is-unsurroundable? (s)
  (case (snippet-type (snippet-parent s))
    (call-snippet t)
    (progn-snippet 
     (let ((grandparent (snippet-parent (snippet-parent s))))
       (call-snippet-with-progn-unwrappable? grandparent)
       ))
    (otherwise nil)
    ))

(defun snippet-is-cuttable? (snippet)
  (not (null (delete-or-clear-box-flag-for-snippet snippet))))

(defmethod node-optional-menu-items ((s form-snippet))
  (when (snippet-is-cuttable? s) 
    (list (snippet-menu-option-menu-entry :cut/insert))))

(defmethod node-optional-menu-items ((s value-form-snippet))
  (append 
   (when t ;; (not (subtypep (snippet-return-type s) 'number))
     (list (snippet-menu-option-menu-entry :multiline-input)))
   (when (snippet-is-unsurroundable? s)
     (list (snippet-menu-option-menu-entry :unsurround)))
   (call-next-method)
   )) 

(defmethod node-optional-menu-items ((s call-snippet))
  (remove-if 
   'null
   (list 
    (when (snippet-is-cuttable? s) 
      (snippet-menu-option-menu-entry :cut/insert))
    (when (snippet-is-unsurroundable? s)
      (snippet-menu-option-menu-entry :unsurround))
    (when t
      (snippet-menu-option-menu-entry :monitor))
    )))

(defmethod node-optional-menu-items ((s aggregate-snippet))
  (remove-if 
   'null
   (list 
    (when (clear-needed? s) (snippet-menu-option-menu-entry :clear))
    (when (delete-needed? s) (snippet-menu-option-menu-entry :delete))
    )))

(defmethod node-optional-menu-items ((s choice-snippet))
  (remove-if 
   'null
   (list
    (when (clear-needed? s) (snippet-menu-option-menu-entry :clear))
    (when (delete-needed? s) (snippet-menu-option-menu-entry :delete))
    )))

(defmethod unwrap-needed? ((snippet t)) nil)

(defmethod unwrap-needed? ((snippet call-snippet))
  (when (not (collapsed? snippet))
    ;; if the object which it would be unwrapped to is a hole, 
    ;; we don't allow this for now, so we don't give the user 
    ;; the option.
    (let ((children (snippet-children snippet)))
      (and (> (length children) 1) (not (snippet-is-hole? (second children))))
      )))

(defmethod node-optional-menu-items ((s toplevel-output-snippet)) 
  (let* ((values (get-snippet-property s :values))
         (nvalues (length values))
         (value (first values)))
    (remove-if 
     'null 
     (list 
      (vwhen (p (get-snippet-property s :printout))
        ;; The null string indicates no printout.  
        ;; It is conceivable that a user
        ;; could cause a null string to be printed out and this does not 
        ;; distinguish between those two cases.  Too bad.  
        (when (plusp (length p)) 
          (snippet-menu-option-menu-entry :show-printout)
          ))
      (when (= 1 nvalues) 
        (unless (or (symbolp value) (numberp value) (characterp value))
          (snippet-menu-option-menu-entry :view)
          ))
      (snippet-menu-option-menu-entry :view-code)
      ))))


(defmethod node-optional-menu-items ((s output-value-snippet))
  (let* ((parent (snippet-parent s))
         (pos (position s (snippet-children parent)))
         (value (nth pos (get-snippet-property parent :values))))
    (remove-if 
     'null 
     (list 
      (unless (or (symbolp value) (numberp value) (characterp value))
        (snippet-menu-option-menu-entry :view))
      ))))
      

(defmethod delete-needed? ((s snippet)) nil)

(defmethod clear-needed? ((s snippet)) nil)

(defmethod clear-needed? ((s aggregate-snippet)) 
  (and (snippet-children s) 
       (or (not (= (length (snippet-children s)) 1))
           (not (get-snippet-property s :one-form-required))
           )))

(defmethod clear-needed? ((s choice-snippet))
  (not (null (snippet-children s))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     
(defmethod node-set-menu-items ((snippet snippet)) nil)

(defmethod node-set-menu-items ((s uniform-choice-snippet))
  (let* ((selected-choice-info (get-snippet-property s :selected-choices))
         (instantiated? (get-snippet-property s :instantiated))
         (literal? (get-snippet-property s :literal))
         (single? (get-snippet-property s :single))
         (multiple? (get-snippet-property s :multiple))
         (required? (get-snippet-property s :required))
         (optional? (get-snippet-property s :optional))
         (repeatable? (get-snippet-property s :repeatable))
         (non-repeatable? (get-snippet-property s :non-repeatable))
         (choice-info (snippet-value s))
         (choice-texts 
          (if literal? 
              (mapcar 'string choice-info)
            (mapcar 'first choice-info))))
    ;; (vdbg "*************************************~%")
    ;; (vdbg "non-repeatable: ~A~%" non-repeatable?)
    ;; (vdbg "selected-choices: ~A~%" selected-choice-info)
    (verify-choice-consistency
     literal? instantiated? single? multiple? 
     required? optional? repeatable? non-repeatable?)
    (let ((menu-items 
           (loop for choice-text in choice-texts 
                 for choice-index from 1 
                 collect
                 (menu-item 
                  choice-text
                  (create-choice-operator choice-index)
                  ))))
      ;; (vdbg "menu-items: ~A~%" menu-items)            
     ;; remove items that are already chosen if they cannot be chosen again
      (when non-repeatable? 
        (setq menu-items 
              (remove-items-at-positions menu-items selected-choice-info)
              ))
      ;; (vdbg "Menu items after: ~A~%" menu-items)
      (when single? 
        ;; Verify that at most one choice is selected 
        (let ((existing-choice-index nil))
          (loop for info in selected-choice-info 
                as menu-index = (first info)
                as menu-item-positions = (second info)
                do 
                (when menu-item-positions 
                  (when existing-choice-index 
                    (vpl-internal-error "Already found existing choice!"))
                  (setq existing-choice-index menu-index)
                  ;; (setf (second info) nil)
                  ))))
      menu-items
      )))

(defmethod node-set-menu-items ((s keys-and-flags-snippet))
  (if (or (null *multiselect-enable*) (only-one-kf-left? s))
      (node-set-menu-items-singleselect s)
    (node-set-menu-items-multiselect s)
    ))

(defun node-set-menu-items-singleselect (s)
  (let ((keys-and-flags-names (get-snippet-property s :ordered-names))
        (keys-selected (get-snippet-property s :keys-present))
        (flags-selected (get-snippet-property s :flags-present)))
    ;; (vdbg "************* Keys selected: ~S~%" keys-selected)
    ;; (vdbg "Flags selected: ~S~%" flags-selected)
    ;; (vdbg "k and f names: ~S~%" keys-and-flags-names)
    (let ((selected-indices 
           (mapcar 
            (lambda (x) 
              (position 
               (string x) 
               keys-and-flags-names :key 'string :test 'string-equal
               ))
            (append keys-selected flags-selected)
            )))
      ;; (vdbg "Selected indices: ~A~%" selected-indices)
      (when (some 'null selected-indices) 
        (vpl-internal-error "Some key or flag name not in :ordered-names!")) 
      (loop for choice-index from 0 
            for name in keys-and-flags-names
            unless (member choice-index selected-indices) 
            collect 
            (menu-item 
             (string name)
             (create-keys-and-flags-operator (1+ choice-index))
             )))))

;; part of multiselect changes 
(defmethod node-set-menu-items-multiselect (s)
  (let ((keys-and-flags-names (get-snippet-property s :ordered-names))
        (keys-selected (get-snippet-property s :keys-present))
        (flags-selected (get-snippet-property s :flags-present)))
    ;; (vdbg "************* Keys selected: ~S~%" keys-selected)
    ;; (vdbg "Flags selected: ~S~%" flags-selected)
    ;; (vdbg "k and f names: ~S~%" keys-and-flags-names)
    (let ((selected-indices 
           (mapcar 
            (lambda (x) 
              (position 
               (string x) 
               keys-and-flags-names :key 'string :test 'string-equal
               ))
            (append keys-selected flags-selected)
            )))
      ;; (vdbg "Selected indices: ~A~%" selected-indices)
      (when (some 'null selected-indices) 
        (vpl-internal-error "Some key or flag name not in :ordered-names!")) 
      (loop for choice-index from 0 
            for name in keys-and-flags-names
            unless (member choice-index selected-indices) 
            collect 
            (menu-item 
             (string name)
             (1+ choice-index)
             ;; (create-keys-and-flags-operator (1+ choice-index))
             )))))

;; Each menu item has information about whether it has been selected or not, 
;; and in what position the selection exists.  There is a one-to-one and ordered
;; correspondence between menu-items and the information in the 
;; selected choice info.  
(defun remove-items-at-positions (list selected-choice-info)
  (loop for item in list 
        for (nil menu-item-positions) in selected-choice-info
        when (null menu-item-positions) 
        collect item
        ))

    
