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

#||

Clearing and deleting is complicated by the fact that the visual display
shows nodes in one of two ways: either as boxes or option menu icons and
associated children.  Things displayed as boxes have a clear/delete icon
in their upper right; while things displayed as option menu icons have 
nowhere to put such an icon, and so the clear and/or delete options are 
presented on the menu.

So there are two different code paths to decide whether a node should
have a clear or delete option: one if it is a box which is coded up
here, and one if it is an options node, in which case the logic is in
snippet-menus.lisp and is buried in the menu component determination
code.

Snippets can either be DELETED, or CLEARED, or both.  If a snippet can
be both DELETED and CLEARED, then CLEAR is done first.

The semantics of CLEAR depends on the node type, what the node replaced,
and possibly other things.

  Examples:

  -- If a node is an aggregate node (e.g., &rest, &keys-and-flags), all
     the children are removed.  
  -- If a node is atomic or some kind of function call, and the original node
     was a hole, the node reverts to being a hole.

Whether a node can be DELETED depends on whether the node is at
toplevel (in which case it can always be deleted), or if not, whether
it is a component of some kind of aggregate or choice node which is
not a standard function call, and whether the node is a required or
optional argument to a function call, and possibly other things. 

All boxes will have either a CLEAR, DELETE, or CLEAR-AND-DELETE
subbox/icon in the upper right of the box.  (It should the case that
no box is such that it can neither be deleted or cleared -- this needs
to be verified).  It is the server's job to specify either CLEAR,
DELETE or CLEAR-AND-DELETE to the client, which then displays the
appropriate subbox/icon.

If the user clicks the subbox/icon, the client transmits either a
HANDLE-CLEAR, HANDLE-DELETE, or HANDLE-CLEAR-AND-DELETE message to the
server, which then performs the action by calling
HANDLE-CLEAR-SNIPPET, etc.

Objects which are not displayed as boxes (eg, option arrow thingys)
and which have menus, will have either a CLEAR or DELETE option
available on the menu.  (These things probably should never have a
DELETE, only a CLEAR).

If the user selects one of these options, an operator is invoked which 
directly calls the HANDLE-CLEAR-SNIPPET, etc functions.  

||#

;;; Predicates which determine whether a box snippet needs a clear and/or
;;; delete icons

(defun delete-or-clear-box-flag-for-snippet (snippet)
  (let ((clear? (snippet-needs-clear-icon? snippet))
        (delete? (snippet-needs-delete-icon? snippet)))
    (cond
     ((and clear? delete?) :jbml-clear-delete)
     (clear? :jbml-clear)
     (delete? :jbml-delete)
     (t nil)
     )))

(defmethod snippet-needs-clear-icon? ((snippet t)) 
  (snippet-replaced-hole? snippet))

(defmethod snippet-needs-clear-icon? ((snippet call-snippet)) 
  (and (snippet-replaced-hole? snippet) (not (toplevel-ws-snippet? snippet))))

(defmethod snippet-needs-clear-icon? ((snippet choice-snippet))
  (vpl-internal-error "Should not be called on a choice snippet!"))

(defmethod snippet-needs-clear-icon? ((snippet aggregate-snippet))
  (vpl-internal-error "Should not be called on an aggregate snippet!"))

(defmethod snippet-needs-clear-icon? ((snippet form-snippet))
  (or (get-snippet-property snippet :hole-open)
      (get-snippet-property snippet :hole-open-multiline)))

(defmethod snippet-needs-clear-icon? ((snippet keyword-snippet)) nil)

(defmethod snippet-needs-clear-icon? ((snippet output-snippet)) nil)

(defmethod snippet-needs-clear-icon? ((snippet progn-snippet)) nil) 


#||

Whether a snippet can be deleted is not generally a function of itself but 
a function of its parent and its context.  

If a node is a keyword or flag node then it can always be deleted.  

If a node has no parent (it is a toplevel node) then it can be deleted.

If a node has as its immediate parent an aggregate node then in general
it can be deleted.  (If the node is the only child of an aggregate node that
needs at least one child, then the effect is as with CLEAR.)

If a node is the child of a keyword node then it can be deleted if
it is a hole, in which case it is equivalent to deleting the entire keyword
node.  

If a node is a child of a choice node then it can be deleted.  

If a node is a toplevel output node it can be deleted.  
  
||#

(defun snippet-needs-delete-icon? (snippet)
  (or (toplevel-snippet? snippet)
      (needs-delete-icon? snippet (snippet-parent snippet))
      ))

(defmethod needs-delete-icon? ((snippet snippet) (parent t)) nil)

(defmethod needs-delete-icon? ((snippet progn-snippet) (parent t))
  (get-snippet-property snippet :delete-box))

(defmethod needs-delete-icon? ((snippet choice-snippet) (parent t))
  (vpl-internal-error "Should not be called on a choice snippet!"))

(defmethod needs-delete-icon? ((snippet aggregate-snippet) (parent t))
  (vpl-internal-error "Should not be called on an aggregate snippet!"))

(defmethod needs-delete-icon? ((snippet keyword-snippet) (parent t)) t)

(defmethod needs-delete-icon? ((snippet flag-snippet) (parent t)) t)

(defmethod needs-delete-icon? ((snippet snippet) (parent aggregate-snippet))
  (not-single-child-one-form-required-node? parent))

; ************** NEW
(defmethod needs-delete-icon? ((snippet snippet) (parent uniform-choice-snippet))
  (not-single-child-one-form-required-node? parent))
; **************

(defmethod needs-delete-icon? 
           ((snippet progn-snippet) (parent aggregate-snippet))
  (not-single-child-one-form-required-node? parent))

(defun not-single-child-one-form-required-node? (parent)
  (not (and (= (length (snippet-children parent)) 1)
            (get-snippet-property parent :one-form-required)
            )))

(defmethod needs-delete-icon? ((snippet t) (parent choice-snippet)) t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Toplevel routines to execute clearing or deleting

(defun handle-clear-delete-snippet (snippet) (handle-clear-snippet snippet))

(defun handle-clear-snippet (snippet &key (origin :icon))
  (vdbg "In HANDLE-CLEAR-SNIPPET...~%")
  (when (eq origin :icon) 
    (let ((flag (delete-or-clear-box-flag-for-snippet snippet)))
      ;; consistency check 
      (unless (or (eq flag :jbml-clear) (eq flag :jbml-clear-delete))
        (vpl-internal-error
         (one-string-nl
          "Inconsistency! HANDLE-CLEAR-SNIPPET called but snippet does not"
          "trigger CLEAR box flag!"
          )))))
  (workspace-parent-modified snippet)
  (new-clear-existing-snippet snippet)
  (setq *current-selected-boxid* nil)
  )

(defun handle-delete-snippet (snippet &key (origin :icon))
  (vdbg "In HANDLE-DELETE-SNIPPET...~%")
  (when (eq origin :icon)
    (let ((flag (delete-or-clear-box-flag-for-snippet snippet)))
      (unless (or (eq flag :jbml-delete) (eq flag :jbml-clear-delete))
        (vpl-internal-error
         (one-string-nl
          "Inconsistency! HANDLE-DELETE-SNIPPET called but snippet does not"
          "trigger DELETE box flag!"
          )))))
  (workspace-parent-modified snippet)
  (vdbg "Modified snippet ~S~%" *modified-snippet*)
  (new-delete-existing-snippet snippet)
  (vdbg "Modified snippet ~S~%" *modified-snippet*)
  (setq *current-selected-boxid* nil)
  )

;;; In case we want to hang DELETE and CLEAR options off a menu at some point

(defun-opcode clear-snippet-op (sid)
  (vdbg "in clear-snippet-op")
  (let ((snippet (find-snippet-in-workspace-or-output-history sid)))
    (handle-clear-snippet snippet :origin :menu-item)
    ))

(defun-opcode delete-snippet-op (sid)
  (vdbg "in delete-snippet-op")
  (let ((snippet (find-snippet-in-workspace-or-output-history sid)))
    (handle-delete-snippet snippet :origin :menu-item)
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; Methods to handle clearing 

#||

Clear algorithm.  

If a snippet is in a place which was originally a hole, then when cleared
the snippet should be replaced by the hole.  (We obtain said hole via 
the function FIND-ORIGINAL-HOLE-SUBTEMPLATE).

If a snippet was not in a place which was originally a hole, then
the action of clearing is a function of what kind of node it is,
and possibly other things.  

By type:

  :constant -- If in place of a hole as above otherwise wrt parent
  :symbol -- If in place of a hole as above otherwise wrt parent
  :form -- If the hole is an open hole (that is, it is being displayed as a 
           type in box), then it should revert to being a regular hole.
           A form which is a regular hole cannot be cleared.
  :arg -- Same as :form
  :call -- If in place of a hole as above otherwise wrt parent
  :keys-and-flags -- Restored to state where no keys or flags specified.
  :keyword -- Should not have clear option
  :flag -- Should not have clear option
  :choice -- All selected choices cleared; node returned to original state
  :aggregate 
   If either :one-or-more or :display-one-node then cleared except for 
   single child, which is cleared itself.  Otherwise all children removed.
  :progn -- Should not have clear option
  :toplevel-output -- Should not have clear option
  :output-value -- Should not have clear option
  :literal -- Should not have clear option
  
||#

;; when you clear a snippet, it should be the only thing modified.  
;; But sometimes the snippet is replaced by another newly created snippet.
;; Therefore NEW-CLEAR-SNIPPET must return the snippet that was modified
;; or the parent if the snippet was replaced.  

(defun new-clear-existing-snippet (snippet)
  (vdbg "In new-clear-existing-snippet...~%")
  (let ((cleared-snippet (new-clear-snippet snippet)))
    (setq *modified-snippet* cleared-snippet)
    ;; any snippet that is cleared is, by fiat, uncollapsed 
    (set-snippet-property cleared-snippet :collapsed? nil)
    ))

(defmethod new-clear-snippet ((snippet snippet)) 
  (vpl-internal-error
   "Snippet of type ~A should not have clear option!" (type-of snippet)))

(defmethod new-clear-snippet ((snippet constant-snippet))
  (clear-if-hole-replaced snippet))

(defmethod new-clear-snippet ((snippet symbol-snippet))
  (clear-if-hole-replaced snippet))

(defmethod new-clear-snippet ((snippet call-snippet))
  (clear-if-hole-replaced snippet))

(defun clear-if-hole-replaced (snippet) 
  (vdbg "In clear-if-hole-replaced...~%")
  (let ((hole-template (find-original-hole-subtemplate snippet :none)))
    (vdbg "hole-template = ~S~%" hole-template)
    (if (not (eq hole-template :none))
        (let* ((parent (snippet-parent snippet))
               (new-hole-snippet 
                (create-subtemplate-snippet parent hole-template)))
          (replace-snippet new-hole-snippet snippet parent)
          (vdbg "After replace...~%")
          parent
          )
      (vpl-internal-error 
       "Snippet of type ~A in this context should not have clear option!"
       (type-of snippet)
       ))))

(defmethod new-clear-snippet ((snippet form-snippet))
  (cond
   ((get-snippet-property snippet :hole-open)
    (progn (set-snippet-property snippet :hole-open nil) snippet))
   ((get-snippet-property snippet :hole-open-multiline)
    (progn (set-snippet-property snippet :hole-open-multiline nil) snippet))
   (t 
    (vpl-internal-error 
     "Snippet of type ~A in this context should not have clear option!"
     (type-of snippet)
     ))))

(defmethod new-clear-snippet ((snippet uniform-choice-snippet))
  (setf (snippet-children snippet) nil)
  ;; This changes the internal list structure of the list which is the value
  ;; of this property.  
  (clear-selected-choices (get-snippet-property snippet :selected-choices))
  snippet
  )

(defun clear-selected-choices (selected-choice-info)
  (loop for info in selected-choice-info 
        do 
        (setf (second info) nil)
        ))

(defmethod new-clear-snippet ((snippet keys-and-flags-snippet))
  (setf (snippet-children snippet) nil)
  (set-snippet-property snippet :keys-present nil)
  (set-snippet-property snippet :flags-present nil)
  snippet)

(defmethod new-clear-snippet ((snippet aggregate-snippet))
  (setf (snippet-children snippet) nil)
  (if (or (get-snippet-property snippet :display-one-hole)
          (get-snippet-property snippet :one-form-required))
      (let ((child-template (snippet-value snippet)))
        (setf (snippet-children snippet)
              (list (create-subtemplate-snippet snippet child-template))
              )))
  snippet)
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Methods to handle deletion


#||

Delete algorithm.  

If a snippet is at toplevel we simply remove it.  

If a node is a keyword or flag node then we remove it from the children 
of the parent keys and flags node and we remove the keyword or flag from 
the list of currently present keys or flags kept on the property list of 
the keys and flags node. 

If a node is an immediate child of an aggregate node then unless it is 
the only node and the aggregate node requires a node we simply remove 
the node from the children of the aggregate node (it should not be possible
to get into the state where there's a delete option for the sole child 
of an aggregate node that requires at least one node).  

If a node is an immediate child of a choice node then we remove it
from the children of the choice node and modify the set of currently
selected choices.  The property :selected-choices must be modified; it
is a list of integers representing the choices in the order they
appear in the children list, so if we delete the number in the
:selected-choices list at the same position as the position of the
child we are deleting in the children list, we should always be able
to maintain the integrity of the orderings.  

We should never be able to delete a literal or arg nodes.  
  
||#

(defun new-delete-existing-snippet (snippet)
  (vdbg "In new-delete-existing-snippet...~%")
  (cond
   ((toplevel-rs-snippet? snippet)
    (vdbg "Deleting toplevel results snippet...~%")
    (remove-snippet-from-parent snippet)
    (not-modified!)
    (redraw-everything-in-results))
   ((toplevel-ws-snippet? snippet)
    (vdbg "Deleting toplevel workspace snippet...~%")
    ;; Unless the snippet being deleted is the only toplevel snippet,
    ;; set the new focus to be on the previous toplevel snippet.
    (let* ((top (workspace-root-node))
           (pos (position snippet (snippet-children top))))
      (unless pos 
        (vpl-internal-error "This is impossible!"))
      (when (plusp pos) 
        (vdbg "Setting focus to previous toplevel child...~%")
        (setq *focus-snippet* (nth (1- pos) (snippet-children top)))
        ))
    (remove-snippet-from-parent snippet)
    (setq *modified-snippet* nil))
   (t
    (let ((parent (snippet-parent snippet)))
      (new-delete-snippet snippet parent)
      (setq *modified-snippet* parent)
      (setq *focus-snippet* *modified-snippet*)
      ))))

;; Returns nil if first child.
(defun previous-child (snippet) 
  (let ((parent (snippet-parent snippet)))
    (unless parent 
      (vpl-internal-error 
       "Should not call PREVIOUS-CHILD on snippet without parent!"))
    (let ((pos (position snippet (snippet-children parent))))
      (unless pos (vpl-internal-error "This is impossible!"))
      (when (plusp pos) (nth (1- pos) (snippet-children parent)))
      )))


(defmethod new-delete-snippet ((snippet snippet) (parent snippet))
  (vpl-internal-error 
   #.(one-string-nl
      "No delete option should be present for snippet of type ~A with parent"
      "of type ~A.")
   (type-of snippet) (type-of parent)
   ))

(defmethod new-delete-snippet 
           ((snippet keyword-snippet) (parent keys-and-flags-snippet))
  (remove-key-or-flag snippet parent))

(defmethod new-delete-snippet 
           ((snippet flag-snippet) (parent keys-and-flags-snippet))
  (remove-key-or-flag snippet parent))

(defmethod new-delete-snippet 
           ((snippet snippet) (parent keys-and-flags-snippet))
  (vpl-internal-error "This should be impossible!"))

(defmethod new-delete-snippet 
           ((snippet snippet) (parent aggregate-snippet))
  (when (and (= (length (snippet-children parent)) 1)
             (get-snippet-property parent :one-form-required))
    (vpl-internal-error "No delete option should be present!"))
  (remove-snippet-from-parent snippet)
  )

(defmethod new-delete-snippet 
           ((snippet snippet) (parent uniform-choice-snippet))
  (vdbg "New delete snippet (t uniform-choice) ~%")
  (vdbg "Choice ID : ~A~%" (snippet-id parent))
  (vdbg "Selected choices: ~A~%" 
        (get-snippet-property parent :selected-choices))
  (let ((pos (position snippet (snippet-children parent))))
    (unless pos 
      (vpl-internal-error "Child is not in parent's children list!"))
    ;; In new-operators.lisp, along with record-choice-at-position
    (unrecord-choice-at-position parent pos)
    (remove-snippet-from-parent snippet)
    ))

(defun remove-key-or-flag (snippet parent)
  (let ((key-or-flag (snippet-value snippet)))
    (etypecase snippet 
      (keyword-snippet (delete-existing-keyword parent snippet key-or-flag))
      (flag-snippet (delete-existing-flag parent snippet key-or-flag))
      )))

(defun delete-existing-flag (parent snippet flag-name)
  (let ((flags-present (get-snippet-info parent :flags-present)))         
    (set-snippet-property 
     parent :flags-present 
     (remove (keywordize flag-name) flags-present :test 'symbol=))
    (remove-snippet-from-parent snippet)
    ))

(defun delete-existing-keyword (parent snippet keyword-name)
  (let ((keywords-present (get-snippet-info parent :keys-present)))         
    (set-snippet-property 
     parent :keys-present 
     (remove (keywordize keyword-name) keywords-present :test 'symbol=))
    (remove-snippet-from-parent snippet)
    ))


