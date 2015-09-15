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

The goal is to construct a file which, when loaded, (via LOAD), will
reconstruct the saved workspace, results, and history, and reset the
VPL to be using that state.

The state of the VPL with respect to a user consists of all the nodes
in the current workspace, the current results area, and all the saved
history.  It also consists of state variables; currently only a single
state variable is saved, that being the pointer indicating which
history level is current.

In order to save out the VPL state, the VPL nodes (snippets) must be
serialized, and each written out in a way that they can be
reconstructed.

In theory, there are only two things we need to deal with with respect
to serialization.  First, VPL nodes point to each other (nodes point
to their parent and some nodes point to a list of children).  Second,
nodes contain lisp values, some of which are not directly writeable
(e.g., garrays).  All other data in a VPL node should be transparently
writeable to disk and rereadable.

Since the only places in a VPL node which have pointers to other VPL
nodes are the PARENT and CHILDREN slots, the way we deal with
serializing these pointers is to store the node IDs of the parent and
the children (if any) instead of the pointers themselves in the
respective slots.  Having reconstructed the VPL nodes except for their
PARENT and CHILDREN pointers, we can then make a second pass through
all the nodes, replacing the IDs in the PARENT and CHILDREN slots with
the actual node.

The two places we need worry about lisp values that may not be
directly writeable are the VALUE slot of a constant node and the
property list of any node.  In theory all other data should be
immediately writeable as is.

What we need to do is to construct a 'constants' array containing all
the unique structured constants we find in all the value nodes and all
the property lists of all the snippets in the user's current state.
The VALUE slot of each constant node will be replaced by an expression
that accesses the constants array (assuming the value has been placed
in the constants array -- otherwise it's just a simple scalar constant
which will be inserted in line).  The property list of each node will
be replaced by an expression that constructs the property list -- if
the property list contains structured constants, the structured
constant will be replaced by an expression that accesses the constants
array.

In this way we insure that constants in different parts a snippet
or in different snippets which are in fact EQ or EQL only get
stored out once.

As we go along processing all the snippets in the user's environment,
we will construct a hash table that maps the snippet ID to a list of
two items: the first item is the expression that is or accesses the
snippet value.  The second is the expression that recreates the
property list of the snippet (this may contain some subexpressions
that recreate structured constants and/or access the constants array).

Some constants can be stored directly as their printed representation;
some, like garrays, and perhaps other data structures, cannot.  For
those constants which cannot be stored directly there will either
exist a method which serializes the constant in the sense of returning
code that will recreate it, or the system will error, saying that it
cannot store a workspace that contains such a value.

The format of the file that is to be written is:

(in-package :nvpl)

<forms to save/restore any variable state>

<forms to set up the toplevel nodes>

<a form to define the values of the constants array>

<a form to create a list of all the snippets>

<a form that calls a routine to make a second pass through the
 snippets, restoring the parent and child pointers>

||#

(defvar *snippet-constants-array-next-index* nil)

(defvar *snippet-constants-hash* nil)

(defun n-save-vpl-workspace (file &optional (user-message? t))
  (ulog "Saving workspace to ~S~%" (namestring file))
  (with-open-file (p file :direction :output :if-exists :supersede)
    (let ((*package* (find-package :nvpl))
          (*print-readably* t)
          (*print-right-margin* 120)
          (*print-pretty* t)
          (*print-length* nil)
          (*print-level* nil)
          (*print-pprint-dispatch* bbi::*biolisp-print-pprint-dispatch*))
      (pprint `(in-package :nvpl) p)
      (terpri p)
      (let ((snippets-to-save (n-all-toplevel-snippets)))
        (vdbg "Got all snippets...~%")
        (if *save-results?* 
            (n-save-snippet-info p snippets-to-save)
          ;; if we're not saving the results, we need to
          ;; insure that the results root node has no children
          ;; while we are saving out all the nodes.  Then we make
          ;; sure to restore the results root node's children.  
          (let* ((results-root (results-root-node))
                 (results-nodes (snippet-children results-root)))
            (setf (snippet-children results-root) nil)
            (unwind-protect 
                (n-save-snippet-info p snippets-to-save)
              (setf (snippet-children results-root) results-nodes)
              )))
        ))
    (when user-message?
      (user-info-message
       (formatn "Workspace saved to ~A." (namestring file))))
    ))

(defun n-restore-vpl-workspace (file)
  (ulog "Restoring user session from ~S~%" (namestring file))
  (flet ((oops (c) 
           (initialize-user-vpl-state t)
           (vpl-internal-error 
            (one-string-nl
             "Could not restore workspace!"
             "Error loading file: ~A"
             "Load error: ~A" 
             "*** The system is trashed.  You must refresh the browser. ***")
            (namestring file) c
            )))
    (handler-case (load file)
      #+allegro
      (lisp:reader-error
       (c)
       (let ((message (formatn "~A" c)))
         ;; special case when we've gotten an error trying to read 
         ;; a symbol in a package that doesn't exist anymore (because
         ;; the system has been rebooted and the package hasn't been 
         ;; recreated, as with BBL::ENTER)
         (if (and (search "Package " message)
                  (zerop (search "Package " message))
                  (search " not found." message))
             (progn 
               (initialize-user-vpl-state t)
               (vpl-user-error 
                (one-string-nl
                 "Could not restore workspace!  Error loading file!"
                 "System state inconsistency: ~A "
                 "You cannot restore a user session if that user session"
                 "was saved out with data from a package that does not now"
                 "exist.  This seems to be the case here." 
                 ""
                 "(One solution might be to first use ENTER to create"
                 "~A, then try to restore your user session.)"
                 ""
                 "*** The system is trashed. ***"
                 "*** You must refresh the browser to continue. ***")
                c (first (second (simple-condition-format-arguments c)))
                ))
           (oops c)
           )))
      (error (c) (oops c))
      ))
  (ulog "User session restored from file ~S~%" (namestring file))
  )

;;; When we create an example we create it from a particular user's package,
;;; say, MASSAR.  Any variables that get written out are written out with 
;;; MASSAR::FOO syntax.  Therefore before we install the example we must
;;; change these package prefixes to some package which we know will exist, 
;;; in our case we choose the $$ package.  

;;; To make the display look reasonable, when a user reads in the example, 
;;; we need to replace all symbols in the $$ package with an equivalent symbol
;;; in that user's package.  

(defun load-vpl-example-workspace (file)
  (ulog "Restoring example workspace from ~S~%" (namestring file))
  (let ((*package* *package*)
        (form nil))
    (with-open-file (p file :direction :input) 
      (loop until (null (setq form (read p nil nil))) do 
            (eval (munge-$$-symbols form))
            ))))

(defun munge-$$-symbols (form)
  (cond 
   ((listp form) 
    (mapcar 'munge-$$-symbols form))
   ((symbolp form) 
    (if (eq (symbol-package form) (find-package :$$))
        (intern (symbol-name form) wb::*username*)
      form
      ))
   (t form)
   ))
  

(defun n-all-toplevel-snippets ()
  (append 
   (list (workspace-root-node))
   (list (results-root-node))
   (when *save-history?* (workspace-histories))
   ))

(defun save-workspace-state (stream)
  (pprint 
   `(progn
      (setq *vpl-workspace-history-index* 
            ,(if *save-history?* *vpl-workspace-history-index* 0))
      (setq *vpl-execution-history-index*
            ,(if *save-results?* *vpl-execution-history-index* 0)))
   stream))

(defun save-toplevel-nodes (stream)
  (vdbg "Saving workspace root~%")
  (pprint
   `(setf (uvs-ws *vpl-state*) ,(snippet-id (workspace-root-node)))
   stream)
  (terpri stream)

  (vdbg "Saving results root~%")
  (pprint
   `(setf (uvs-rs *vpl-state*) ,(snippet-id (results-root-node))) 
   stream)
  (terpri stream)
      
  (vdbg "Saving workspace history toplevel nodes...~%")
  (pprint
   (if *save-history?* 
       `(setf
         (uvs-wsh *vpl-state*)
         ',(mapcar 'snippet-id (workspace-histories)))
     `(setf (uvs-wsh *vpl-state*) nil)
     )
   stream)
  (terpri stream))

(defun vpl-gensym (string) 
  (let ((p #.*package*))
    (intern (string (gensym string)) p)))

(defun n-save-snippet-info (stream snippets)

  (vdbg "Saving workspace state...~%")
  (save-workspace-state stream) 
  (terpri stream)

  (vdbg "Saving toplevel nodes...~%")
  (save-toplevel-nodes stream)
  
  (vdbg "Saving snippets...~%")

  (let ((shash (make-hash-table :test 'equal))
        (*snippet-constants-hash* (make-hash-table :test 'eql))
        (*snippet-constants-array-next-index* 0)
        (constants-array-symbol (vpl-gensym "C-ARRAY-"))
        (snippets-list-symbol (vpl-gensym "S-LIST-")))

    (labels ((maybe-put-in-hash (s) 
               (cond
                ((gethash s shash) nil)
                (t (setf (gethash s shash) t) t)
                ))
             (process-snippet (s)
               (when (maybe-put-in-hash s)
                 (vwhen (associated-snippets (n-get-associated-snippets s))
                   (loop for a in associated-snippets 
                         do
                         (process-snippet a)
                         )))))
      
      ;; create the hash with all the snippets in it
      (vdbg "Creating snippet hash...~%")
      (loop for s in snippets do (process-snippet s))

      ;; create the array of constants we need to recreate.
      ;; Also for each snippet in our hash of all snippets, 
      ;; create the forms for the value and property list slots
      (vdbg "Creating constants hash...~%")
      (maphash 
       (lambda (snippet dummy) 
         (declare (ignore dummy))
         (let ((v (snippet-value snippet))
               (p (snippet-properties snippet))
               (value-form nil)
               (plist-form nil))
           (if (null (constant-needs-to-be-recreated? v))
               (setq value-form (quote-only-symbols v))
             (setq 
              value-form 
              (process-constant-for-save-workspace v constants-array-symbol)
              ))
           (setq 
            plist-form 
            (process-plist-for-save-workspace p constants-array-symbol))
           (setf (gethash snippet shash) (list value-form plist-form))
           ))
       shash
       )

      ;; save the constants array 
      (vdbg "Saving constants array...~%")
      (pprint 
       `(defparameter ,constants-array-symbol 
          (make-array (list ,*snippet-constants-array-next-index*)))
       stream
       )
      (let ((forms 
             (lmaphash 
              (lambda (constant data) 
                (declare (ignore constant))
                (destructuring-bind (recreation-form index) data
                  `(setf (aref ,constants-array-symbol ,index) 
                         ,recreation-form
                         )))
              *snippet-constants-hash*
              )))
        (setq forms (sort forms '< :key (lambda (x) (third (second x)))))
        (loop for form in forms do (pprint form stream))
        )
      (terpri stream)

      ;; save all the snippets to disk
      (vdbg "Saving ~D snippets...~%" (hash-table-count shash))
      (pprint 
       `(defparameter ,snippets-list-symbol
          (list 
           ,@(lmaphash
              (lambda (snippet data)
                (destructuring-bind (value-form plist-form) data
                  ;; Take out the :output-nodes property, 
                  ;; if it exists, from each node so that 
                  ;; no pointer exists to a results node 
                  ;; if we aren't saving the results area nodes.  
                  (when (not *save-results?*) 
                    (let ((plist (copy-list (cdr plist-form))))
                      (when plist 
                        (remf plist :output-nodes)
                        (setq plist-form `(list ,@plist))
                        )))
                  (let* ((basic-slots (n-basic-snippet-slots snippet))
                         (other-slots (n-other-snippet-slots snippet))
                         (snippet-type (type-of snippet)))
                    `(;; n-recreate-snippet 
                      nrs
                      ',snippet-type 
                      ,value-form
                      ,plist-form
                      ',basic-slots
                      ',other-slots
                      ))))
              shash
              )))
       stream 
       )
      (terpri stream)
      
      (pprint 
       `(n-reset-workspace-pointers ,snippets-list-symbol)
       stream)
      (terpri stream)
      
      
      (pprint 
       `(progn 
          (unintern ',snippets-list-symbol)
          (unintern ',constants-array-symbol)
          )
       stream
       )       
      (terpri stream)
      (unintern snippets-list-symbol)
      (unintern constants-array-symbol)
      
      )))

(defun quote-only-symbols (atom)
  (cond
   ((null atom) atom)
   ((eq atom t) atom)
   ((keywordp atom) atom)
   ((symbolp atom) `',atom)
   (t atom)
   ))

(defun n-get-associated-snippets (s)
  (let ((property-data (snippet-plist s)))
    (when (some (lambda (x) (typep x 'snippet)) property-data)
      (vpl-internal-error 
       "Why is there a snippet on the property list of ~S?!" s))
    (when (typep (snippet-value s) 'snippet)
      (vpl-internal-error 
       "Why is there a snippet in the value slot of ~S?!" s))
    (copy-list (snippet-children s))
    ))

(defun process-constant-for-save-workspace (v constants-array-symbol)
  (let ((constant-info (gethash v *snippet-constants-hash*)))
    (if constant-info 
        (destructuring-bind (serialized-form index) constant-info
          (declare (ignore serialized-form))
          `(aref ,constants-array-symbol ,index))
      (progn
        (let ((value-form (serialized-snippet-value v)))
          (setf (gethash v *snippet-constants-hash*)
                (list value-form *snippet-constants-array-next-index*))
          (prog1 
              `(aref ,constants-array-symbol 
                     ,*snippet-constants-array-next-index*)
            (incf *snippet-constants-array-next-index*)
            ))))))

(defun process-plist-for-save-workspace (plist constants-array-symbol)
  `(list 
    ,@(loop 
       for elem in plist 
       collect 
       (cond
        ((null (constant-needs-to-be-recreated? elem)) 
         (quote-only-symbols elem))
        ((consp elem) 
         (process-plist-for-save-workspace elem constants-array-symbol))
        (t (process-constant-for-save-workspace elem constants-array-symbol))
        ))))
            

(defun n-recreate-snippet 
       (type value plist basic-slot-info other-slot-info)
  ;; This binding hack prevents a new snippet ID from being generated, and any
  ;; of the snippet's slots from being initialized.
  ;; It also prevents the snipped from being registered in the snippet
  ;; hash table, so we have to do that ourselves using ALWAYS-REGISTER-SNIPPET
  (let ((*disable-snippet-initialization* t))
    (let ((recreated-snippet (make-instance type)))
      (n-recreate-basic-snippet-slots recreated-snippet basic-slot-info)
      ;; (always-register-snippet recreated-snippet)
      (setf (snippet-value recreated-snippet) value)
      (setf (snippet-properties recreated-snippet) plist)
      (n-recreate-other-snippet-slots recreated-snippet other-slot-info)
      recreated-snippet
      )))

(defun nrs (type value plist basic-slot-info other-slot-info)
  (n-recreate-snippet type value plist basic-slot-info other-slot-info))

(defun n-basic-snippet-slots (snippet)
  (list 
   (snippet-id snippet)
   (vwhen (p (snippet-parent snippet)) (snippet-id p))
   (mapcar 'snippet-id (snippet-children snippet))
   ))

(defun n-recreate-basic-snippet-slots (snippet basic-slots)
  (macrolet ((set-slot (slot-name) 
               `(progn
                  (setf (,slot-name snippet) (first basic-slots))
                  (pop basic-slots)
                  )))
    (set-slot snippet-id)
    (set-slot snippet-parent)
    (set-slot snippet-children)
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Everything but the raw constitutients of Lisp (strings, numbers,
;;; symbols and characters) needs to be re-consed up when restoring
;;; state.  The exception is frames, which are assumed to exist
;;; across reboots of the system, are are therefore uniquely identified
;;; by name.

(defmethod constant-needs-to-be-recreated? ((x t)) t)

(defmethod constant-needs-to-be-recreated? ((x string)) nil)

(defmethod constant-needs-to-be-recreated? ((x number)) nil)

(defmethod constant-needs-to-be-recreated? ((x symbol)) nil)

(defmethod constant-needs-to-be-recreated? ((x character)) nil)

(defmethod constant-needs-to-be-recreated? ((x cons)) t)

(defmethod constant-needs-to-be-recreated? ((x vector)) t)

#||  
#+:weblistener-aframes
(defmethod constant-needs-to-be-recreated? ((x frames::%aframe)) nil)
||#

(defun non-recreated-stored-constant-value (x)
  (typecase x
    ((or string number character) x)
    (symbol (quote-only-symbols x))
    (cons `',x)
    (vector x)
    (t (vpl-internal-error "Should never get here, code is inconsistent!"))
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod serialized-snippet-value ((x t))
  (vpl-user-error
   "Cannot save session in which workspace contains something of type ~A"
   (type-of x)
   ))

(defmethod serialized-snippet-value ((x string)) x)

(defmethod serialized-snippet-value ((x number)) x)

(defmethod serialized-snippet-value ((x character)) x)

(defmethod serialized-snippet-value ((x symbol)) (quote-only-symbols x))

(defmethod serialized-snippet-value ((x cons)) 
  `(list ,@(mapcar 'serialized-snippet-value x)))
  
(defmethod serialized-snippet-value ((x sequence))
  `(map 
    ',(type-of x) 
    'identity
    (list ,@(map 'list 'serialized-snippet-value x))
    ))

(defmethod serialized-snippet-value ((x error))
  `(list :saved-snippet-error-string ,(formatn "~A" x)))

(defmethod serialized-snippet-value ((g utils::garray))
  (utils::create-garray-creation-form g 'serialized-snippet-value))

(defmethod serialized-snippet-value ((x bbi::labeled-sequence))
  `(bbi::make-labeled-sequence 
    :label ,(serialized-snippet-value (bbi::labeled-sequence-label x))
    :sequence ,(serialized-snippet-value (bbi::labeled-sequence-sequence x))
    ))

(defmethod serialized-snippet-value ((x wb::url))
  `(wb::make-url 
    :path ,(serialized-path (wb::url-path x))
    :display-string ,(serialized-snippet-value (wb::url-display-string x))
    :target ,(serialized-snippet-value (wb::url-target x))
    ))

(defmethod serialized-snippet-value ((x wb::jpg))
  `(wb::make-jpg
    :path ,(serialized-path (wb::jpg-path x))
    :click-path ,(serialized-path (wb::jpg-click-path x))
    ))

(defmethod serialized-snippet-value ((x pathname))
  `(pathname ,(namestring x)))

(defun serialized-path (p)
  (serialized-snippet-value (if (pathnamep p) (namestring p) p)))

(defmethod serialized-snippet-value ((x wb::svs)) nil)

(defmethod serialized-snippet-value ((x wb::data-editor-object)) nil)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Methods to retrieve other information from specialized snippet
;;; subclasses.

(defmethod n-other-snippet-slots ((snippet snippet)) nil)

(defmethod n-other-snippet-slots ((snippet return-value-snippet))
  (append 
   (list :return-type (snippet-return-type snippet))
   (call-next-method)
   ))

(defmethod n-other-snippet-slots ((snippet labeled-snippet))
  (append 
   (list :label (snippet-label snippet))
   (call-next-method)
   ))

(defmethod n-recreate-other-snippet-slots ((snippet snippet) other-slot-info) 
  (declare (ignore other-slot-info))
  nil)

(defmethod n-recreate-other-snippet-slots 
           ((snippet return-value-snippet) other-slot-info)
  (setf (snippet-return-type snippet) 
        (n-find-slot-info-value :return-type other-slot-info))
  (call-next-method)
  )

(defmethod n-recreate-other-snippet-slots 
           ((snippet labeled-snippet) other-slot-info)
  (setf (snippet-label snippet) 
        (n-find-slot-info-value :label other-slot-info))
  (call-next-method)
  )

(defun n-find-slot-info-value (key info) (getf info key))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; This function is called as the last form of the file that gets loaded
;;; to restore the workspace.

;;; Its goal is to replace the snippet IDs currently representing the parent and
;;; children of each snippet with pointer to the actual snippet.  

;;; A problem with this is that the snippet IDs stored out to disk may conflict
;;; with existing snippet IDs (because the system has been rebooted and 
;;; completely independent IDs have been generated).  The solution to this
;;; problem is that once the recreated set of snippets has been completely
;;; 'repointerized' we go and replace the snippet IDs of all the recreated
;;; snippets with new IDs which, by definition, will not conflict with
;;; existing IDs.  Once we have generated these new IDs, we then and only
;;; then put our recreated snippets into the snippet hash table (keyed by
;;; ID), for later use.

(defun n-reset-workspace-pointers (snippet-list)
  (let ((shash (make-hash-table :test 'equal)))
    (loop for s in snippet-list do 
          (setf (gethash (snippet-id s) shash) s)
          ;; create a new unique id for each snippet, but don't 
          ;; make it the snippet's id quite yet
          (setf (gethash s shash) (new-unique-id :snippet-id)))
    (flet ((find-in-hash (pid) 
             (let ((p (gethash pid shash)))
               (unless p (vpl-internal-error "Parent not in WS hash!"))
               p)))
      (vdbg "Resetting workspace snippet pointers ~%")
      (loop for s in snippet-list 
            as pid = (snippet-parent s)
            as children = (snippet-children s)
            do 
            (when pid (setf (snippet-parent s) (find-in-hash pid)))
            (setf (snippet-children s) 
                  (loop for child in children collect (find-in-hash child))
                  ))
      (vdbg "Resetting snippet roots pointers ~%")
      (setf (uvs-ws *vpl-state*) (find-in-hash (uvs-ws *vpl-state*)))
      (vdbg "Resetting workspace history pointers ~%")
      (setf (uvs-rs *vpl-state*) (find-in-hash (uvs-rs *vpl-state*)))
      (setf (uvs-wsh *vpl-state*) 
            (mapcar #'find-in-hash (uvs-wsh *vpl-state*))
            ))
    ;; now that everything is recreated, change the snippet ID of each snippet
    ;; to the new id and register the snippet (put it in the hash)
    (loop for s in snippet-list do
          (let ((new-id (gethash s shash)))
            (setf (snippet-id s) new-id)
            (always-register-snippet s)
            ))))
  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


