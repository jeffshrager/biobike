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

(defun-opcode surround-with-function (sid)
  (not-modified!)
  (vdbg "in surround-with-function")
  (find-snippet-in-workspace sid)
  (when *current-selected-boxid* 
    (unless (eql sid *current-selected-boxid*)
      (unflashing-hilight-box *current-selected-boxid*)))
  (flashing-hilight-box sid)
  (setq *current-selected-boxid* sid)
  (setq *current-wrap-target* sid)
  )

#||

Wrap algorithm.

1- Find the snippet to be wrapped, WRE.
3- Find the parent of the wrapee, P.
4- Create a wrapping snippet from template, WRR, whose parent is P.
5- Find the hole in the new wrapping snippet where the wrapee will
   be placed; save a pointer to this hole, call it WRR-HOLE.
6- Replace the wrapee, WRE, in the children (or grandchildren) 
   of the parent with the wrapper, WRR.
7- Replace WRR-HOLE somewhere in the descendants of the wrapper
   with the wrapee, WRE, and set the parent of WRE to be the parent of 
   WRR-HOLE.

Complications involve step 3, because some macros want to 
replace other than the first hole (eg, IF).

Also it is possible that a wrapper template has no hole in its immediate
children; for instance if + were defined using just an &rest arg this would
be an issue for + (as it is, + in the VPL has two required arguments).

||#


;;; WRAP TEMPLATE AROUND ANOTHER SNIPPET 

(defun wrap-template (template-id snippet-id)
  (vdbg "New wrap template~%")
  (let* ((template (template-id->template template-id))
         ;; Step 1, find snippet we are to wrap
         (wre (find-snippet-in-workspace snippet-id))
         ;; Step 3, get parent of wrapee
         (parent (snippet-parent wre))
         ;; Step 4, create snippet to wrap with from the template 
         ;; of the function the user clicked on
         (wrr (create-snippet-from-template parent template))
         )
    (wrap-consistency-check wre wrr template)
    (vdbg "Looking for hole..~%")
    ;; Step 5, find the hole in the newly-created snippet from the previous 
    ;; step we will replace with the wrapee
    (let ((wrr-hole (find-hole-to-embed-wrapee wrr template)))
      (vdbg "Found hole in new snippet: ~A~%" wrr-hole)
      ;; Step 6, replace the wrapee with the wrapper as its parent's child
      (setf (snippet-children parent)
            (substitute wrr wre (snippet-children parent)))
      (vdbg "Substituted new snippet for wrapee...~%")
      ;; Step 7, replace designated hole in wrapper snippet with wrapee
      ;; if it fails, undo the child replacement above.  
      (let ((error? t))
        (unwind-protect 
            (progn
              (find-wrr-hole-and-substitute-wrapee wrr wre wrr-hole)
              (setq error? nil))
          (when error? 
            (setf (snippet-children parent)
                  (substitute wre wrr (snippet-children parent))
                  ))))
        
      (vdbg "Put wrapee into hole of wrapper template...~%")
      )
    (setq *modified-snippet* parent)
    (setq *focus-snippet* wrr)
    ))

(defmethod wrap-consistency-check ((wrapee snippet) wrapper template)
  (declare (ignore wrapper template))
  t)

(defmethod wrap-consistency-check ((wrapee symbol-snippet) wrapper template)
  (declare (ignore wrapper))  
  (case (get-snippet-property wrapee :symbol-node-type)
    (:place 
     (unless (legal-place-template? template)
       (vpl-user-error 
        #.(one-string-nl
           "You can only surround this node with []'s (REF)."
           "Any other form is not permissible because this node can only"
           "legally hold a symbol or a table."
           ))))
    (:arg 
     (vpl-user-error 
      #.(one-string-nl
         "You cannot surround this node (even though the option exists)."
         "This node represents a function argument, and therefore must remain"
         "a symbol; it cannot be an expression."
         )))
    (otherwise t)
    ))

(defun find-hole-to-embed-wrapee (wrr template)
  (let* ((template-function (first template))
         (wrap-function
          (or (get template-function :hole-locator-function-for-wrap) 
              'find-or-create-hole-to-embed-wrapee-function 
              )))
    (funcall wrap-function wrr)
    ))

(defun find-or-create-hole-to-embed-wrapee-function (wrr)
  (vdbg "In find-or-create...~%")
  (when (null (cdr (snippet-children wrr)))
    (vpl-user-error "Cannot wrap a function of zero arguments!"))
  (block exit
    (loop for child in (snippet-children wrr)
          do
          (when (snippet-is-hole? child) (return-from exit child)))
    ;; Couldn't find a hole in the immediate children, see if 
    ;; the first child is an aggregate snippet with a hole for its
    ;; first child, or if no children exist, create a hole child.
    (or (find-or-create-wrapee-hole (first-snippet-arg wrr))
        (vpl-internal-error "Template has no hole to insert wrapped target!")
        )))

(defmethod find-or-create-wrapee-hole ((arg snippet)) nil)

(defmethod find-or-create-wrapee-hole ((arg aggregate-snippet))
  (vdbg "Searching aggregate node child...~%")
  (let ((first-child (first (snippet-children arg))))
    (cond 
     ((null first-child) 
      ;; create a child node if none exists
      (another-subform-of-snippet arg)
      (first (snippet-children arg)))
     ((snippet-is-hole? first-child) first-child)
     (t nil)
     )))
    
;; Recursively descends into parent's children to find WRR-HOLE
;; and substitutes the wrapee, WRE, for the hole
;; Errors out if it is not found anywhere.
(defun find-wrr-hole-and-substitute-wrapee (parent wre wrr-hole)
  (block exit
    (labels ((search-children (parent)
               (cond
                ((find wrr-hole (snippet-children parent))
                 (verify-paste-is-valid wre wrr-hole)
                 (setf (snippet-children parent)
                       (substitute wre wrr-hole (snippet-children parent)))
                 (setf (snippet-parent wre) parent)
                 (return-from exit t))
                (t 
                 (loop for child in (snippet-children parent) do 
                       (search-children child)
                       )))))
      (search-children parent)
      (vpl-internal-error "Could not find hole in wrapper snippet!")
      )))

;;; Hole locator functions for specific special forms

(defun find-wrapee-for-if-true-style-snippet (snippet)
  (let ((then-aggregate (third-snippet-arg snippet)))
    (maybe-create-first-child then-aggregate)
    (first (snippet-children then-aggregate))
    ))

(defun find-wrapee-for-when-stype-snippet (snippet)
  (let ((when-body (second-snippet-arg snippet)))
    (maybe-create-first-child when-body)
    (first (snippet-children when-body))
    ))

(defun find-wrapee-for-loop-snippet (snippet) 
  (declare (ignore snippet))
  (vpl-user-error "Sorry, cannot use LOOP or FOR-EACH to surround with!"))

(defun find-wrapee-for-assign-snippet (snippet)
  (let* ((aggregate-snippet (first-snippet-arg snippet))
         (progn-snippet (first (snippet-children aggregate-snippet)))
         (uniform-choice-snippet (first (snippet-children progn-snippet)))
         (hole-snippet (first (snippet-children uniform-choice-snippet))))
    hole-snippet
    ))

(defun maybe-create-first-child (aggregate)
  (let ((children (snippet-children aggregate)))
    (unless children (another-subform-of-snippet aggregate))
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; New algorithm to cause a chosen node to have its surrounding call 
;;; (or other special form) removed, causing an inverse wrap based on 
;;; the node that was originally wrapped (or surrounded with) vs. the
;;; node that the original node was surrounded with

(defun-opcode unsurround-function (sid)
  (vdbg "in unsurround-function")
  (let* ((snippet (find-snippet-in-workspace sid)))
    (typecase snippet 
      ((or call-snippet constant-snippet symbol-snippet value-form-snippet)
       (unsurround-snippet snippet)
       (setq *focus-snippet* snippet))
      (otherwise (vpl-internal-error "Should not have unsurround option here!"))
      )))

(defun unsurround-snippet (s)
  (cond
   ((toplevel-ws-snippet? s) 
    (vpl-internal-error 
     "Should not have unsurround option for toplevel snippet!"))
   ((toplevel-ws-snippet? (snippet-parent s))
    (unsurround-snippet-to-toplevel s))
   (t 
    (unwrap-parent-preserving-child (snippet-parent s) s)
    )))

(defun unsurround-snippet-to-toplevel (s)
  (set-hole-template s)
  (unwrap-parent-preserving-child (snippet-parent s) s)
  )

(defun set-hole-template (s)
  (set-snippet-property s :hole-template (find-original-hole-subtemplate s)))

(defmethod unwrap-parent-preserving-child ((parent snippet) child)
  (declare (ignore child))
  (vpl-internal-error 
   "Cannot unwrap snippet whose parent is of type ~S~%"
   (snippet-type parent)
   ))

(defmethod unwrap-parent-preserving-child ((parent call-snippet) child)
  (verify-child-can-replace-parent parent child)
  (let* ((grandparent (snippet-parent parent)))
    (setf (snippet-children grandparent) 
          (substitute child parent (snippet-children grandparent)))
    (setf (snippet-parent child) grandparent)
    (setq *modified-snippet* grandparent)
    ))

(defmethod unwrap-parent-preserving-child ((parent progn-snippet) child)
  (let ((grandparent (snippet-parent parent)))
    (case (snippet-type grandparent)
      (call-snippet 
       (if (call-snippet-with-progn-unwrappable? grandparent)
           (progn
             (when (toplevel-ws-snippet? grandparent)
               (set-hole-template child))
             (unwrap-parent-preserving-child grandparent child))
         (vpl-user-error "Sorry don't know how to do this!")
         ))
      (otherwise 
       (vpl-internal-error "Should not have unsurround option!")
       ))))

(defun call-snippet-with-progn-unwrappable? (call-snippet)
  (let ((call-symbol 
         (string (snippet-value (first (snippet-children call-snippet))))))
    (or (string-equal "if-true" call-symbol) 
        (string-equal "if-false" call-symbol)
        )))

(defun verify-child-can-replace-parent (parent child)
  (unless (toplevel-snippet? parent) 
    (let ((child-type (snippet-return-type child))
          (parent-original-hole-template
           (find-original-hole-subtemplate parent :no-hole)))
      (when (eq parent-original-hole-template :no-hole)
        (vpl-internal-error "Parent being replaced has no original hole!"))
      (let ((parent-hole-type (third parent-original-hole-template)))
        (unless (types-are-compatible? child-type parent-hole-type)
          (vpl-user-error 
           (one-string-nl
            "You are trying to substitute a node"
            "that is or returns something of type ~S,"
            "for a node that is allowed to be or return something of type ~S,"
            "but those two types are disjoint."
            "(E.g., If a node is requird to be a number,"
            "you cannot replace it with a function that returns a string"
            "as those two types are incompatible.)")
           (pretty-up-type child-type) (pretty-up-type parent-hole-type)
           ))))))

