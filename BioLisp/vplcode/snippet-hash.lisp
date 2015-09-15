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

This code creates and maintains a hash table which references every
currently accessible snippet (and some that are no longer accessible).

This hash will be used instead of searching the workspace and/or
results areas via tree search to find a particular snippet given its
snippet ID.

The hash table will be on a per-user basis.

The problem with maintaining snippets in a hash table is that snippets
eventually become dereferenced and otherwise available for garbage
collection.  But if they remain in the hash table they will never get
garbage collected (this is why weak hash tables were invented, I
guess, but neither common lisp nor allegro has them).

The solution is to do a pseudo-garbage collection of snippets every
Nth time the user issues a command, removing those that are no longer
referenced from the hash table.

We do this by assuming that all snippets that are accessible are
accessible from a root structure which represents the user's VPL
workspace and results state.  By starting at this root structure and 
doing a tree search we can mark all the snippets we find.
Those snippets that are not marked are no longer accessible and hence
can be removed from the hash.

||#



(defun initialize-user-snippet-hash (&optional (force? t))
  (unless *vpl-state* 
    (vpl-internal-error "VPL state not initialized!"))
  (when (or force? (null (uvs-hash *vpl-state*)))
    (setf (uvs-hash *vpl-state*) (make-hash-table :test 'eql))
    ))

(defun register-snippet (snippet)
  (when (gethash (snippet-id snippet) (uvs-hash *vpl-state*))
    (vpl-internal-error "New snippet already exists in hash!"))
  (always-register-snippet snippet))

(defun always-register-snippet (snippet)
  (setf (gethash (snippet-id snippet) (uvs-hash *vpl-state*)) snippet))

(defun find-snippet-by-id (id &optional (error-if-not-found? t))
  (or (gethash id (uvs-hash *vpl-state*))
      (when error-if-not-found? 
        (vpl-internal-error
         "Could not find snippet corresponding to ID ~A anywhere" id
         ))))

(defun find-ws-snippet-by-id (id &optional (error-if-not-found? t))
  (vif (s (gethash id (uvs-hash *vpl-state*)))
       (if (snippet-in-workspace? s) 
           s
         (when error-if-not-found?
           (vpl-internal-error 
            "Could not find snippet corresponding to ID ~A in workspace" id
            )))
       (when error-if-not-found?
         (vpl-internal-error
          "Could not find snippet allegedly in workspace corresponding to ID ~A"
          id
          ))))

(defun find-rs-snippet-by-id (id &optional (error-if-not-found? t))
  (vif (s (gethash id (uvs-hash *vpl-state*)))
       (if (snippet-in-results-area? s) 
           s
         (when error-if-not-found?
           (vpl-internal-error 
            "Could not find snippet corresponding to ID ~A in results" id
            )))
       (when error-if-not-found?
         (vpl-internal-error
          "Could not find snippet allegedly in results corresponding to ID ~A"
          id
          ))))

(defun snippet-in-workspace? (s)
  (let ((p (snippet-parent s)))
    (cond
     ((eq p (uvs-ws *vpl-state*)) t)
     ((bad-snippet-parent s p) nil)
     ((typep p 'toplevel-ws-snippet) nil)
     ((typep p 'toplevel-rs-snippet) nil)
     (t (snippet-in-workspace? p))
     )))

(defun snippet-in-results-area? (s) 
  (let ((p (snippet-parent s)))
    (cond
     ((eq p (uvs-rs *vpl-state*)) t)
     ((bad-snippet-parent s p) nil)
     ((typep p 'toplevel-ws-snippet) nil)
     ((typep p 'toplevel-rs-snippet) nil)
     (t (snippet-in-results-area? p))
     )))

(defun snippet-in-active-area? (s) 
  (or (snippet-in-workspace? s) (snippet-in-results-area? s)))

(defun snippet-in-workspace-history? (s) 
  (let ((p (snippet-parent s)))
    (cond
     ((eq p (uvs-ws *vpl-state*)) nil)
     ((bad-snippet-parent s p) nil)
     ((typep p 'toplevel-ws-snippet) t)
     ((typep p 'toplevel-rs-snippet) nil)
     (t (snippet-in-workspace-history? p))
     )))

(defun bad-snippet-parent (s p)
  (cond
   ((null p) (vpl-null-parent-error s))
   ((not (typep p 'snippet))
      (vpl-internal-error "Snippet ~S has non-snippet parent!!" s))
   (t nil)
   ))

#||

This assumes that snippets are only found in the children or the value 
slot of a snippet.  

This assumes that snippets are only found in lists; no snippets are stored
in structures or arrays within a snippet's property list or otherwise.

This assumes that all snippets are either in the user's workspace or 
in his results area or in his history.  One exception is that a snippet
could be on the clipboard and nowhere else, so we have to handle that.

||#

(defun gc-snippets ()
  (vdbg "Marking workspace snippets ...~%")
  (gc-snippet (uvs-ws *vpl-state*))
  (vdbg "Marking results snippets ...~%")
  (gc-snippet (uvs-rs *vpl-state*))
  (mapcar 'gc-snippet (uvs-wsh *vpl-state*))
  (when *current-clipboard-box* (gc-snippet *current-clipboard-box*))
  (when (get wb::*sessionid* :stored-results)
    (gc-snippet (get wb::*sessionid* :stored-results)))
  (vdbg "Removing unmarked snippets from hash~%")
  (let ((hash (uvs-hash *vpl-state*)))
    (maphash 
     (lambda (id snippet) 
       (cond
        ((get-snippet-property snippet :gc-mark)
         (set-snippet-property snippet :gc-mark nil))
        (t (remhash id hash))
        ))
     hash
     )))

(defun gc-snippet (s)
  (traverse-snippet s (lambda (x) (set-snippet-property x :gc-mark t))))

(defun traverse-snippet (s f)
  (funcall f s)
  (loop for child in (snippet-children s) do (traverse-snippet child f))
  (when (typep (snippet-value s) 'snippet) 
    (traverse-snippet (snippet-value s) f))
  (traverse-list (snippet-properties s) f)
  )

(defun traverse-list (list f)
  (loop for thing in list do 
        (cond
         ((typep thing 'snippet) (traverse-snippet thing f))
         ((null thing) nil)
         ((listp thing) (traverse-list thing f))
         (t nil)
         )))

(defun add-snippet (s) 
  (traverse-snippet s (lambda (x) (register-snippet x))))