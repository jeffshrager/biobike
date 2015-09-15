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

#||

The workspace history consists of a list of TOPLEVEL-WS-SNIPPETs, at
most *vpl-workspace-history-length* long.  The workspace history is
stored in the WSH slot of *vpl-state*.  Workspace history information
also consists of an index specifying which element of the workspace
history is the current workspace (the one being displayed).  The
workspace history is added to every time the user causes the state of
the workspace to change.

When the history length is at max, and the state gets changed, the
oldest history is lost.

When the pointer is not pointing at the most recent state (because the
user has gone back in history), and the user changes the workspace
state, states created subsequent to the state being pointed at are
lost, as with a browser's BACK and FORWARD buttons.

See also the discussion about recovering history across a refresh in
the function VPL-INITIALIZE-SERVER.

||#

(defun add-to-workspace-history (old-copied-workspace)
  (vdbg "Adding to workspace history.~%")
  (let* ((workspace-history (uvs-wsh *vpl-state*))
         (current-workspace (uvs-ws *vpl-state*))
         (len (length workspace-history)))
    (cond
     ((zerop len)
      (setq workspace-history (list current-workspace))
      (setq *vpl-workspace-history-index* 0))
     ((zerop *vpl-workspace-history-index*)
      (vdbg "Adding new workspace history to top of stack...~%")
      ;; add the modified workspace to the top of the stack, and replace 
      ;; the modified current workspace with its unmodified copy
      (setq workspace-history 
            (cons current-workspace 
                  (cons old-copied-workspace (cdr workspace-history))))
      ;; ensure workspace history doesn't grow too large.
      (let ((newlen (length workspace-history)))
        (when (> newlen *vpl-workspace-history-length*)
          (vdbg "Dropping last workspace history...~%")
          (setq workspace-history 
                (subseq 
                 workspace-history 
                 0 *vpl-workspace-history-length*
                 )))))
     (t 
      (vdbg "Truncating workspace history above ~D~%" 
                 *vpl-workspace-history-index*)
      ;; get rid of all workspace histories subsequent to where we were.
      (setq workspace-history 
            (subseq workspace-history *vpl-workspace-history-index*))
      ;; add the modified workspace to the top of the stack, and replace 
      ;; the modified current workspace with its unmodified copy
      (setq workspace-history 
            (cons current-workspace  
                  (cons old-copied-workspace (cdr workspace-history))))
      (setq *vpl-workspace-history-index* 0)
      ))
    (setf (uvs-wsh *vpl-state*) workspace-history)
    ))

(defun handle-workspace-back ()
  (vdbg "Going back in workspace history. ~%")
  (let ((workspace-history (uvs-wsh *vpl-state*)))
    (cond 
     ;; The workspace history should only be NIL immediately after we've done
     ;; a browser refresh (or initially when we start the VPL).  
     ;; Instead of doing nothing when the user hits UNDO at this point, 
     ;; we see if there was a previous history from before the refresh,
     ;; and if so, we use it.  Note that there is no way of
     ;; fully undoing this action; you are stuck with the previous history set,
     ;; unless you do another refresh.
     ((null workspace-history) 
      (setq *vpl-workspace-history-index* 
            (get wb::*sessionid* :workspace-history-index))
      (setq workspace-history (get wb::*sessionid* :workspace-history))
      (when workspace-history 
        (setf (uvs-ws *vpl-state*) 
              (elt workspace-history *vpl-workspace-history-index*))
        (vwhen (stored-results (get wb::*sessionid* :stored-results))
          (setf (uvs-rs *vpl-state*) stored-results)
          (redraw-everything-in-results))
        (redraw-everything-in-workspace)
        (setq *current-selected-boxid* nil)
        (show-status "Workspace prior to browser refresh restored...") 
        ))
     ((= *vpl-workspace-history-index* (1- (length workspace-history)))
      (user-info-message "No previous workspace history..."))
     (t 
      (incf *vpl-workspace-history-index*) 
      (setf (uvs-ws *vpl-state*)
            (elt workspace-history *vpl-workspace-history-index*))
      (redraw-everything-in-workspace)
      (setq *current-selected-boxid* nil)
      ))
    (setf (uvs-wsh *vpl-state*) workspace-history)
    ))
  
(defun handle-workspace-forward ()
  (let ((workspace-history (uvs-wsh *vpl-state*)))
    (vdbg "Going forward in workspace history. ~%")
    ;; (paste-toplevel-function nil)
    ;; (redraw-everything-in-workspace)
    (cond 
     ((null workspace-history) nil)
     ((zerop *vpl-workspace-history-index*) 
      (show-status "No subsequent workspace history.")
      #+obsolete
      (user-info-message "No subsequent workspace history..."))
     (t 
      (decf *vpl-workspace-history-index*)
      (setf (uvs-ws *vpl-state*)
            (elt workspace-history *vpl-workspace-history-index*))
      (redraw-everything-in-workspace)
      (setq *current-selected-boxid* nil)
      ))))
