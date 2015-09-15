;;; -*- mode: Lisp; Syntax: Common-Lisp; Package: nvpl; -*-

(in-package :nvpl)

;;; +=========================================================================+
;;; | Copyright (c) 2006 JP Massar, John Myers                                |
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

;; :snippet-id :template-id :menu-id :operator-id :data-id

(defun drag-and-drop (dragged-object-id target-object-id other-info)
  (vdbg "In drag-and-drop")
  (let ((target-type (unique-id-type target-object-id))
        (dragged-type (unique-id-type dragged-object-id)))
    (case target-type
      ;; Not one of recognized types, so the ID should be one of the ones
      ;; we made special for the workspace palette or results areas.  
      ((nil) 
       (case target-object-id
         ;; do we need to redraw everything?  
         (#.*palette-id* 
          (vpl-internal-error "Cannot drag to palette!"))
         (#.*workspace-id* 
          (error "Should never get here!")
          )
         ;; do we need to redraw everything?  
         (#.*results-id* 
          (vpl-internal-error "Cannot drag to results!"))
         ))
      ;; Dragging something into an existing snippet.  The snippet should
      ;; be a hole.  If not an error should occur.  
      (:snippet-id   
       (handle-drag-and-drop-to-snippet 
        dragged-object-id dragged-type target-object-id other-info))
      ((:menu-id :operator-id :data-id)
       (vpl-internal-error "Target cannot be menu, operator, or data!"))
      )))

(defun drop-between (dragged-object-id target-object-id position)
  ;; position is either :before or :after 
  ;; dragged object is the object being dragged
  ;; target object is the object the dragged object will be positioned 
  ;; before or after.  
  ;; The target object must be a toplevel workspace object.
  (vdbg "in drop-between")
  (block exit
    (when (eq dragged-object-id target-object-id)
      (return-from exit nil))
    (multiple-value-bind (target-snippet where)
        (find-snippet-in-workspace-or-output-history target-object-id)
      (let ((dragged-type (unique-id-type dragged-object-id)))
        (case dragged-type 
          (:snippet-id 
           (let ((dragged-snippet 
                  (find-snippet-in-workspace-or-output-history 
                   dragged-object-id
                   )))
             (typecase dragged-snippet
               ((or keyword-snippet flag-snippet)
                (show-status "You cannot drag or drop this box!"))
               (t
                (when (snippet-is-hole? dragged-snippet)
                  (show-status "You cannot drag an empty box!")
                  (return-from exit nil))
                (cond
                 ((or (snippet-in-workspace? dragged-snippet) 
                      (snippet-in-results-area? dragged-snippet))
                  (with-ws-node (ws) 
                    ;; this moves dragged snippet to clipboard 
                    ;; so save current contents of clipboard away so
                    ;; they can be restored
                    (with-clipboard-saved
                      (cond
                       ((snippet-in-workspace? dragged-snippet)
                        ;; Can't drag from workspace to results area
                        (when (eq where :results) 
                          (show-status
                           "Cannot drag workspace node to results area!")
                          (return-from exit nil))
                        (cut-function dragged-object-id))
                       ((snippet-in-results-area? dragged-snippet)
                        ;; Can't rearrange objects in results area yet
                        (when (eq where :results) (return-from exit nil))
                        (copy-function dragged-object-id)))
                      (setf (snippet-parent *current-clipboard-box*) ws)
                      (let* ((toplevel-children (snippet-children ws))
                             (pos 
                              (position
                               target-snippet toplevel-children)))
                        (unless pos 
                          (vpl-internal-error 
                           "Can't find target in toplevel wrkspc nodes!"))
                        (setf (snippet-children ws) 
                              (insert-at-pos 
                               toplevel-children *current-clipboard-box* 
                               pos position
                               ))
                        (setq *current-clipboard-box* nil)
                        ;; make sure workspace gets redrawn as we unwind
                        (setq *vpl-redraw-workspace?* t)
                        (setq *modified-snippet* nil)
                        (setq 
                         *function-from-palette-to-workspace-toplevel?*
                         nil)
                        (setq *vpl-workspace-modified?* t)
                        ;; (redraw-everything-in-workspace)
                        ;; (not-modified!)
                        ))))
                 (t (vpl-internal-error "Not in results or workspace!"))
                 )))))
          (#.*palette-id* 
           (vpl-internal-error "Cannot drag palette item yet!"))
          (otherwise 
           (vpl-internal-error "Cannot drag object of type ~A!" dragged-type)
           ))))))

(defun insert-at-pos (list object pos before-or-after)
  (let ((len (length list)))
    (flet ((insert-before ()
             (append (subseq list 0 pos) 
                     (list object)
                     (subseq list pos)
                     ))
           (insert-after ()
             (append (subseq list 0 (1+ pos))
                     (list object)
                     (subseq list (1+ pos))
                     )))
      (cond 
       ((zerop len) (list object))
       (t 
        (cond
         ((<= pos 0)
          (setq pos 0)
          (case before-or-after 
            (:before (cons object list))
            (:after (insert-after))))
         ((>= pos (1- len))
          (setq pos (1- len))
          (case before-or-after
            (:after (append list (list object)))
            (:before (insert-before))))
         (t 
          (case before-or-after 
            (:before (insert-before))
            (:after (insert-after))
            ))))))))
          
     

(defun handle-drag-and-drop-to-snippet 
       (dragged-object-id dragged-type target-object-id other-info)
  (declare (ignore other-info))
  ;; same question -- does everything have to be redrawn when the
  ;; release results in an error?
  (with-snippet-from-id-in-workspace 
      (target-snippet target-object-id "Cannot drop object into results area!")
    (unless (snippet-is-hole? target-snippet)
      (vpl-user-error "Cannot drop object into non-hole snippet!"))
    (case dragged-type 
      (:template-id 
       (let ((*current-selected-boxid* target-object-id))
         (handle-drag-and-drop-from-template dragged-object-id)
         ))
      (:snippet-id 
       (let ((dragged-snippet 
              (find-snippet-in-workspace-or-output-history dragged-object-id)))
         (with-clipboard-saved
           (typecase dragged-snippet
             ((or keyword-snippet flag-snippet)
              (show-status "You cannot drag or drop this box!"))
             (t 
              (when (snippet-is-hole? dragged-snippet)
                (show-status "You cannot drag an empty box!")
                (return-from handle-drag-and-drop-to-snippet nil))
              (cond
               ((snippet-in-workspace? dragged-snippet)
                (verify-non-recursive-dnd dragged-snippet target-snippet)
                (verify-legal-drop-target dragged-snippet target-object-id)
                (cond
                 ((toplevel-snippet? dragged-snippet) 
                  (cut-function dragged-object-id)
                  (redraw-everything-in-workspace)
                  )
                 (t 
                  (let ((parent (snippet-parent dragged-snippet)))
                    ;; this setqs *vpl-workspace-modified?* to T
                    (cut-function dragged-object-id)
                    (let ((*modified-snippet* parent))
                      (redraw-modified-box)
                      ))))
                (paste-function target-object-id))
               ((snippet-in-results-area? dragged-snippet)
                (copy-function dragged-object-id)
                ;; copy-function setqs this to nil but we need it to be T
                (setq *vpl-redraw-workspace?* t)
                ;; copy-function does NOT set this to T but we need it so
                (setq *vpl-workspace-modified?* t)
                (paste-function target-object-id)
                )
               (t 
                (vpl-internal-error 
                 "Snippet being dragged not in workspace or results!"
                 ))))))
         ))
      (:data-id nil)
      ((:menu-id :operator-id) 
       (vpl-internal-error "Cannot drag menu or operator into snippet!"))
      )))     

(defun verify-legal-drop-target (dragged-snippet target-object-id)
  (with-snippet-from-id-in-workspace 
      (hole-snippet target-object-id "Cannot paste object into results node!")
    (unless (snippet-is-hole? hole-snippet)
      (vpl-user-error 
       "Cannot paste into selected object which is not a hole!"))
    (verify-paste-is-valid dragged-snippet hole-snippet)
    ))

(defun verify-non-recursive-dnd (dragged-snippet target-snippet)
  (cond 
   ((eq dragged-snippet target-snippet)
    (vpl-user-error "Cannot drag box to its inner contents!"))
   ((toplevel-snippet? target-snippet)
    nil)
   (t 
    (verify-non-recursive-dnd dragged-snippet (snippet-parent target-snippet))
    )))
    
(defun handle-drag-and-drop-from-template (template-id)
  (multiple-value-bind (symbol found?)
      (template-id->symbol template-id nil)
    (unless found? 
      (vpl-internal-error 
       "ID ~A identified as template id but no associated symbol!"
       template-id
       ))
    (handle-template-insertion symbol template-id)
    (augment-favorites-info symbol)
    ))