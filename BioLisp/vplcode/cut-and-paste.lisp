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


(defun beep-at-user (s) 
  (declare (ignore s))
  nil)

(defun handle-copy-snippet ()
  (vdbg "Handle copy snippet~%")
  (not-modified!)
  (let ((*copy-successful?* t))
    (copy-currently-selected-box-into-clipboard)
    (when *copy-successful?* 
      (clipboard-status-message *current-clipboard-box* "Copied")
      )))

(defmethod clipboard-status-message ((snippet t) verb)
  (show-status (formatn "~A to clipboard..." verb)))

(defmethod clipboard-status-message ((snippet call-snippet) verb)
  (show-status 
   (formatn "~A ~S." 
            verb
            (snippet-value (first (snippet-children snippet))))))

;; Make sure clipboard message is short and on one line
(defmethod clipboard-status-message ((snippet constant-snippet) verb)
  (let ((value-string 
         (limited-string
          (substitute #\Space #\Newline (formatn "~S" (snippet-value snippet)))
          30
          )))
    (show-status (formatn "~A ~A." verb value-string))
    ))

;; Make sure clipboard message is short and on one line
(defmethod clipboard-status-message ((snippet symbol-snippet) verb)
  (let ((value-string 
         (limited-string (formatn "~S" (snippet-value snippet)) 30)))
  (show-status (formatn "~A ~A." verb value-string))
  ))
               

(defun copy-currently-selected-box-into-clipboard ()
  (block exit
    (unless *current-selected-boxid*
      (show-status "No box currently selected.")
      (setq *copy-successful?* nil)
      (return-from exit nil)
      )
    (multiple-value-bind (snippet-to-copy where)
        (find-snippet-in-workspace-or-output-history
         *current-selected-boxid*
         )
      (copy-snippet-method snippet-to-copy)
      (when *copy-successful?* 
        (set-snippet-property 
         *current-clipboard-box* :needs-quote?
         (snippet-needs-quote-when-pasted? *current-clipboard-box* where))
        (vdbg "Copied snippet of type ~S to clipboard~%"
              (type-of *current-clipboard-box*)
              )))))

(defun snippet-needs-quote-when-pasted? (snippet origin)
  (ecase origin
    (:results
     (typecase snippet
       (constant-snippet 
        (let ((value (snippet-value snippet)))
          (cond
           ((symbolp value) nil)
           ((and value (consp value)) t)
           (t nil)
           )))
       (symbol-snippet nil)
       (otherwise (error "In snqwp, this should be impossible!"))
       ))
    (:workspace
     (typecase snippet
       (constant-snippet 
        (let ((value (snippet-value snippet)))
          (and value (consp value) (not (eq (first value) 'quote)))
          ))
       (otherwise nil)
       ))))

(defmethod copy-snippet-method ((snippet t))
  (case *command-origin* 
    (:keypress 
     (setq *copy-successful?* nil)
     (unless (get-snippet-property snippet :hole-open)
       (vdbg "Beep!~%")
       (show-status "Cannot copy this node!")))
    (otherwise 
     (vpl-internal-error
      "There should not be a copy option available!"
      ))))

(defmethod copy-snippet-method ((snippet toplevel-output-snippet))
  (let* ((values (get-snippet-property snippet :values))
         (nvalues (length values))
         (one-value? (= nvalues 1))
         (first-value (first values)))
    (setq *current-clipboard-box*
          (if (and one-value?
                   (symbolp first-value) (not (constantp first-value)))
              (create-subtemplate-snippet 
               nil (copy-list `(:symbol ,first-value)))
            (create-constant-snippet 
             nil
             (if (not one-value?) `(,@values) first-value)
             )))))

(defmethod copy-snippet-method ((snippet output-value-snippet))
  (vdbg "Copy snippet method (output value)~%")
  (let* ((parent (snippet-parent snippet))
         (pos (position snippet (snippet-children parent))))
    (let* ((values (get-snippet-property (snippet-parent snippet) :values))
           (value (nth pos values)))
      (setq *current-clipboard-box* (create-constant-snippet nil value))
      )))

(defmethod copy-snippet-method ((snippet constant-snippet))
  (copy-workspace-snippet-to-clipboard snippet))

(defmethod copy-snippet-method ((snippet symbol-snippet))
  (copy-workspace-snippet-to-clipboard snippet))

(defmethod copy-snippet-method ((snippet call-snippet))
  (copy-workspace-snippet-to-clipboard snippet))

(defun copy-workspace-snippet-to-clipboard (snippet)
  (setq *current-clipboard-box* (pcopy-snippet snippet nil t)))

(defun handle-cut-snippet ()
  (vdbg "Handle cut snippet~%")
  (let ((*copy-successful?* t))
    (copy-currently-selected-box-into-clipboard)
    (if *copy-successful?* 
        (progn
          (clipboard-status-message *current-clipboard-box* "Cut")
          (with-snippet-from-id-in-workspace
              (snippet-to-cut *current-selected-boxid*)
            (let ((previous-child (previous-child snippet-to-cut))
                  (parent (snippet-parent snippet-to-cut))
                  (operation-result
                   (handle-cut-snippet-internal snippet-to-cut)))
              (declare (ignore operation-result))
              ;; Sometimes the cut snippet is not really cut,
              ;; it is just cleared.  
              ;; (ie, replaced by a hole-snippet). 
              ;; In this case, the focus should
              ;; really be on the replacing hole snippet, but there's no easy
              ;; way to get our hands on that new snippet from here.
              (cond
               (previous-child (setq *focus-snippet* previous-child))
               ((not (eq parent (workspace-root-node)))
                (setq *focus-snippet* parent))
               (t nil)
               ))))
      (not-modified!)
      )))

(defun handle-cut-snippet-internal (original-snippet)
  (vdbg "In handle-cut-snippet-internal...~%")
  (let ((top (workspace-root-node)))
    (cond
     ;; this clause should not be necessary because handle-delete-snippet 
     ;; should be able to delete a toplevel snippet and 
     ;; DELETE-OR-CLEAR-ACTION-FOR-SNIPPET should return only a :jbml-delete
     ;; option for any toplevel snippet.
     ((member original-snippet (snippet-children top))
      (setf (snippet-children top) 
            (delete original-snippet (snippet-children top)))
      :deleted)
     (t
      (vdbg "Cutting nontoplevel snippet ~%")
      (let ((action (delete-or-clear-box-flag-for-snippet original-snippet)))
        (case action
          ((:jbml-clear-delete :jbml-clear)
           (vdbg "Cut -> clear ~%")
           (handle-clear-snippet original-snippet)
           :cleared)
          (:jbml-delete 
           (vdbg "Cut -> delete~%")
           (handle-delete-snippet original-snippet)
           :deleted)
          (otherwise 
           (not-modified!)
           (vpl-internal-error 
            (one-string-nl
             "Snippet is not clearable or deleteable and therefore should not"
             "be copyable.  Therefore HANDLE-CUT-SNIPPET should have errored"
             "when it called COPY-CURRENTLY-SELECTED-BOX-INTO-CLIPBOARD!"
             )))))))))

;; Paste to the toplevel of the workspace if there's no box selected.  
;; Otherwise paste into the selected box.  
(defun handle-paste-shortcut ()
  (if *current-selected-boxid*
      (handle-paste-snippet)
    (paste-toplevel-function nil)
    ))

(defun handle-paste-snippet ()
  (vdbg "Handle paste snippet~%")
  (unless *current-clipboard-box*
    (vpl-user-error "No code object currently on clipboard!"))
  (let ((pasted-snippet (handle-paste-box-into-hole *current-clipboard-box*)))
    (setq *focus-snippet* pasted-snippet) 
    ))

;; SNIPPET is the thing we are pasting.  
;; The snippet that *CURRENT-SELECTED-BOXID* designates is the
;; hole we are replacing with SNIPPET.  
(defun handle-paste-box-into-hole (snippet)
  "Handles pasting a box into a hole which was previously selected."
  (vdbg "Handle paste box: ~S~%" snippet)
  (let ((csb *current-selected-boxid*))
    (with-snippet-from-id-in-workspace 
        (hole-snippet csb "Cannot paste object into results node!")
      (vdbg "Pasting into ~S ~%" hole-snippet)
      (unless (snippet-is-hole? hole-snippet)
        (vpl-user-error 
         "Cannot paste into selected object which is not a hole!"))
      (verify-paste-is-valid snippet hole-snippet)
      ;; return pasted snippet; might be a copy of SNIPPET
      (paste-snippet-into-hole snippet hole-snippet)
      )))

(defmethod paste-snippet-into-hole ((snippet snippet) hole-snippet)
  (declare (ignore hole-snippet))
  (vpl-internal-error 
   (one-string-nl
    "Attempt to paste a snippet of type ~A.  Should not have been allowed to"
    "copy/cut that kind of snippet!")
   (type-of snippet)))

(defmethod paste-snippet-into-hole ((snippet call-snippet) hole-snippet)
  (paste-snippet-into-hole-internal snippet hole-snippet))

(defmethod paste-snippet-into-hole ((snippet symbol-snippet) hole-snippet)
  (paste-snippet-into-hole-internal snippet hole-snippet))

(defun paste-snippet-into-hole-internal (snippet hole-snippet)
  (let ((copy (pcopy-snippet snippet (snippet-parent hole-snippet) t)))
    (insert-new-snippet-into-snippet-hole hole-snippet copy)
    copy
    ))

;; If you're trying to paste in a constant snippet whose value is a
;; symbol or a list, then presumably you want the literal symbol or list,
;; you don't want it to be evaluated if you execute.  This hack handles
;; that.  

(defmethod paste-snippet-into-hole ((snippet constant-snippet) hole-snippet)
  (vdbg "In paste-snippet-into-hole (constant)~%")
  (let ((copy (pcopy-snippet snippet (snippet-parent hole-snippet) t))
        (value (snippet-value snippet)))
    (when (get-snippet-property snippet :needs-quote?)
      (setf (snippet-value copy) (list 'quote value)))
    (vdbg "Inserting ~S ~%replacing ~S~%" copy hole-snippet)
    (insert-new-snippet-into-snippet-hole hole-snippet copy)
    copy
    ))

(defun verify-paste-is-valid (paste-snippet hole-snippet)
  (typecase hole-snippet 
    (value-form-snippet 
     (let ((prt (snippet-return-type paste-snippet))
           (hrt (snippet-return-type hole-snippet)))
       (when (and prt hrt)
         (unless (types-are-compatible? hrt prt)
           (vpl-user-error 
            (one-string-nl
             "You are trying to insert something"
             "that is or returns something of type ~A,"
             "into a hole that requires something of type ~A,"
             "but those two types are disjoint."
             "(E.g., If a hole specifies that it requires a number,"
             "you cannot replace it with a function that returns a string"
             "as those two types are incompatible.)")
            (type-to-english-string (pretty-up-type prt))
            (type-to-english-string (pretty-up-type hrt))
            ))))
     nil)
    (argument-snippet 
     (cond
      ((null (get-snippet-property hole-snippet :place))
       (typecase paste-snippet 
         (symbol-snippet nil)
         (otherwise 
          (vpl-user-error 
           "Cannot paste that into this hole!  This hole requires a symbol!"
           ))))
      (t 
       (unless (or (typep paste-snippet 'symbol-snippet) 
                   (ref-snippet? paste-snippet)
                   (range-ref-snippet? paste-snippet))
         (vpl-user-error 
          (one-string-nl
           "Cannot paste that into this hole!  This hole requires a symbol"
           "or a table reference!"
           ))))))
    (otherwise 
     (vpl-internal-error "There are no other types of hole snippet!")
     )))

(defun handle-insert-into-selected-hole (snippet)
  (setq *current-clipboard-box* (pcopy-snippet snippet nil))
  (handle-paste-snippet)
  )

;; the redraw here could be optimized to only redraw from the lowest common
;; ancestor of the node that is cut and the node that is inserted into, 
;; or if the two have no common ancestor other than the workspace root, 
;; in theory one could redraw only both of their toplevel nodes instead of 
;; the entire workspace (but the code isn't set up to do something like that).

(defun handle-cut-insert-into-selected-hole (snippet)
  (vdbg "in handle cut insert into selected hole...~%")
  (with-clipboard-saved
    (handle-insert-into-selected-hole snippet)
    (vdbg "after insert doing cut~%")
    (handle-cut-snippet-internal snippet)
    (clipboard-status-message *current-clipboard-box* "Cut")
    (setq *modified-snippet* nil)
    (redraw!)
    ))
  
