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

(defun potential-hidden-node? (s)
  (get-snippet-property s :reveal-label))

(defun root-consistency-check (snippet)
  (typecase snippet
    ((or toplevel-rs-snippet toplevel-ws-snippet)
     (vpl-internal-error 
      "You should never be operating on the root snippets!"
      ))))

(defun hidden-consistency-check (snippet)
  (when (some 
         (lambda (x) (get-snippet-info x :reveal-label)) 
         (snippet-children snippet))
    (vpl-internal-error 
     (one-string-nl
      "You are trying to delete a node from a parent which has potential"
      "hidden nodes.  This should not be possible!"
      ))))

(defun parent-child-consistency-check (snippet parent)
  (when (not (member snippet (snippet-children parent)))
    (vpl-internal-error
     "***** SOMETHING IS VERY WRONG! SNIPPET NOT IN PARENT's VALUE!!!!"
     )))

(defun parent-descendants-consistency-check (snippet)
  (unless (null (snippet-children snippet))
    (loop for child in (snippet-children snippet)
          do
          (unless (snippet-parent child)
            (vpl-internal-error 
             "Child snippet ~A has no parent!" child))
          (unless (eq (snippet-parent child) snippet)
            (vpl-internal-error 
             #.(one-string-nl
                "Snippet"
                "~A"
                "has child snippet,"
                "~A,"
                "whose parent slot is not the first snippet!"
                "Its parent ID is ~D."
                )
             snippet child (snippet-id (snippet-parent child))
             ))
          (parent-descendants-consistency-check child)
          )))

;; This sets the parent of the replaced snippet to NIL.  
;; This implies that the replaced snippet must never be used again
;; because all valid snippets except our toplevel snippets have 
;; parents now.
(defun replace-snippet (new-snippet old-snippet parent-snippet)
  (vdbg "In replace-snippet~%")
  (when (potential-hidden-node? old-snippet)
    (vpl-internal-error 
     "Should not be able to replace a node that can be hidden!"))
  (root-consistency-check old-snippet)
  (setf (snippet-parent new-snippet) parent-snippet)
  (setf (snippet-children parent-snippet) 
        (substitute new-snippet old-snippet (snippet-children parent-snippet)))
  (setf (snippet-parent old-snippet) nil)
  )

(defvar *kill-parent-pointer* t)

(defun remove-snippet-from-parent (snippet)
  (let ((parent (snippet-parent snippet)))
    (unless (integerp (position snippet (snippet-children parent)))
      (vpl-internal-error "Snippet's parent does not contain it!"))
    (when *kill-parent-pointer* 
      (setf (snippet-parent snippet) nil))
    (setf (snippet-children parent) 
          (remove snippet (snippet-children parent))
          )))

(defun add-to-workspace-toplevel (snippet)
  (with-ws-node (ws) 
    (setf (snippet-children ws) (append (snippet-children ws) (list snippet)))
    ))

;; put results snippet at beginning of results list so earlier results 
;; scroll off page downward
(defun add-to-results-toplevel (snippet)
  (with-rs-node (rs)
    (setf (snippet-children rs) (append (list snippet) (snippet-children rs)))
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun-opcode noop (anyid)
  (declare (ignore anyid))
  (not-modified!)
  nil)

;;; CREATE NEW SNIPPET operator

(defun create-new-toplevel-ws-snippet (template-id &optional (pprint? nil))
  (declare (ignore pprint?))
  (with-ws-node (ws)
    ;; For server-side debugging ease.
    (when (symbolp template-id)
      (setq template-id (symbol->template-id template-id)))
    (let ((template (template-id->template template-id)))
      (let ((new-snippet (create-snippet-from-template ws template)))
        (add-to-workspace-toplevel new-snippet)
        (setq *focus-snippet* new-snippet)
        new-snippet
        ))))

(defun create-toplevel-ws-snippet-from-subtemplate (subtemplate)
  (with-ws-node (ws)
    (let ((new-snippet (create-subtemplate-snippet ws subtemplate)))
    (add-to-workspace-toplevel new-snippet)
    (setq *focus-snippet* new-snippet)
    new-snippet
    )))
    
;;; CREATE NEW TOPLEVEL HOLE SNIPPET 

(defun-opcode hole-toplevel-function (sid)
  "Creates a new data box in the workspace"
  (declare (ignore sid))
  (vdbg "In hole-toplevel-function")
  (create-toplevel-ws-snippet-from-subtemplate (copy-list '(:form "object" t))))

;;; CHANGE THE ORDER IN WHICH TOPLEVEL NODES OF THE WORKSPACE ARE DISPLAYED.

(defun-opcode reverse-workspace-order-function (sid)
              #.(utils::one-string
                 "Reverse the order of boxes in the workspace and change"
                 " the positioning (top or bottom) of new boxes"
                 )
  (declare (ignore sid))
  (vdbg "in reverse-workspace-order-function")
  (setf (get wb::*sessionid* :vpl-workspace-order-reversed?) 
        (not (get wb::*sessionid* :vpl-workspace-order-reversed?))
        )
  (setq *focus-snippet* (first (snippet-children (workspace-root-node)))))

;;; CHANGE THE ORDER IN WHICH TOPLEVEL NODES OF THE RESULTS AREA ARE DISPLAYED.

(defun-opcode reverse-results-order-function (sid)
              #.(utils::one-string
                 "Reverse the order of boxes in the results area and toggle"
                 " the positioning (top or bottom) of new boxes"
                 )
  (declare (ignore sid))
  (vdbg "in reverse-results-order-function")
  (setf (get wb::*sessionid* :vpl-results-order-reversed?) 
        (not (get wb::*sessionid* :vpl-results-order-reversed?))
        )
  (redraw-results)
  )

;;; CREATE NEW TOPLEVEL FUNCTION SNIPPET

(defun-opcode function-hole-toplevel-function (sid)
  (declare (ignore sid))
  (vdbg "in function-hole-toplevel-function")
  (create-toplevel-ws-snippet-from-subtemplate 
   (copy-list '(:arg "function name" :insert-type :function-call))))

;;; INSERT TEMPLATE INTO (SELECTED) HOLE operator

(defun verify-hole (snippet obj)
  (unless (snippet-is-hole? snippet)
    (vpl-internal-error
     "Trying to insert ~A into non-hole: ~%~A."
     obj snippet)))

(defun legal-place-template? (template)
  (case (first template)
    (ref t)
    (otherwise nil)
    ))

(defun place-snippet? (s) (get-snippet-property s :place))

(defun argument-snippet? (s) (typep s 'argument-snippet))

(defun insert-template (template-id snippet-id)
  (vdbg "In insert-template~%")
  (let* ((template (template-id->template template-id))
         (template-function (first template)) 
         (snippet (find-snippet-in-workspace snippet-id))
         (place? (place-snippet? snippet))
         (place-template? (legal-place-template? template))
         (argument? (argument-snippet? snippet))
         (ltype
          (case (snippet-type snippet)
            (argument-snippet 
             (unless (and place? place-template?)
               (vpl-user-error 
                (one-string
                 "You cannot insert a function from the palette into "
                 "this box -- you can only enter or type in symbols!"
                 ))))
            (otherwise (snippet-return-type snippet))
            )))
    (verify-hole snippet template)
    (setq *modified-snippet* (snippet-parent snippet))
    (when (and argument? (not place?))
      (vpl-user-error 
       "You cannot insert a form here! Only symbols are allowed!"))
    (cond
     ((and place? (not place-template?))
      (vpl-user-error 
       "Cannot insert ~A here!  Only symbols or table references are allowed!"
       (first template)))
     (t 
      (let* ((new-snippet 
              (create-snippet-from-template (snippet-parent snippet) template))
             (new-ltype (snippet-return-type new-snippet)))
        (when (and ltype new-ltype (not (or argument? place?)))
          (unless (types-are-compatible? ltype new-ltype)
            (vpl-user-error 
             (one-string-nl
              "You are trying to insert a function, ~S,"
              "that returns something of type ~A,"
              "into a hole that requires something of type ~A,"
              "but those two types are disjoint."
              "(E.g., If a hole specifies that it requires a number,"
              "you cannot replace it with a function that returns a string"
              "as those two types are incompatible.)")
             template-function 
             (type-to-english-string (pretty-up-type new-ltype))
             (type-to-english-string (pretty-up-type ltype))
             )))
        (insert-new-snippet-into-snippet-hole snippet new-snippet)
        )))))

(defparameter *possible-common-lisp-vpl-types*
  '(
    array atom
    hash-table simple-string              
    base-char integer          
    base-string keyword
    simple-vector              
    bignum list             
    bit single-float 
    standard-char            
    character null
    number complex
    string pathname              
    cons double-float 
    ratio symbol                     
    rational t
    real sequence           
    fixnum float
    short-float signed-byte
    vector simple-array
    ))

      
(defun types-are-compatible? (hole-ltype new-ltype)
  (or (subtypep hole-ltype new-ltype)
      (subtypep new-ltype hole-ltype)
      (cond 
       ((and (listp new-ltype) (eq 'or (first new-ltype)))
        (some
         (lambda (x) (types-are-compatible? x hole-ltype))
         (cdr new-ltype)
         ))
       ((and (listp hole-ltype) (eq 'or (first hole-ltype)))
        (some
         (lambda (x) (types-are-compatible? x new-ltype))
         (cdr hole-ltype)
         ))
       (t
        ;; If one or both of the types are not common lisp types but were
        ;; presumably defined using DEFTYPE, then say they are compatible
        ;; because we don't know anything about them. 
        (let ((hole-base-type 
               (if (listp hole-ltype) (first hole-ltype) hole-ltype))
              (new-base-type 
               (if (listp new-ltype) (first new-ltype) new-ltype)))
          (if (and (member hole-base-type *possible-common-lisp-vpl-types*)
                   (member new-base-type *possible-common-lisp-vpl-types*))
              nil
            t
            ))))))

;;; INSERT CONSTANT INTO HOLE operator (no type checking for now)

(defun insert-constant-into-hole 
       (snippet-id constant &key (eval-constant? t) (format "~S"))
  (let* ((snippet (find-snippet-in-workspace snippet-id))
         (place? (place-snippet? snippet))
         (argument? (argument-snippet? snippet)))
    (verify-hole snippet constant)
    (when (or place? argument?) 
      (vpl-user-error 
       "Cannot insert ~S here! Only symbols and tables are allowed here!" 
       constant))
    (let ((p (snippet-parent snippet)))
      (parent-child-consistency-check snippet p)
      ;; Do a type check 
      (let* ((stype (snippet-return-type snippet))
             (constant-value (if eval-constant? (eval constant) constant)))
        (when (and stype (not (typep constant-value stype)))
          (when (snippet-parent snippet)
            (handler-case 
                (let ((code (snippet-to-code (snippet-parent snippet) t)))
                  (ulog "Invalid data type error.  Context: ~S~%" code))
              (error () nil)
              ))
          (if (and (listp stype) (eq 'or (first stype)))
              ;; formulate an english-like expression for the (or ...) type
              (let ((types (type-to-english-string stype)))
                (vpl-user-error 
                 "Cannot insert ~S here!~%Only values of type ~A are allowed."
                 constant types))
            (vpl-user-error 
             "Cannot insert ~S here!~%Only values of type ~A are allowed."
             constant (pretty-up-type stype))))
        (insert-constant-internal snippet constant format)
        (setq *modified-snippet* p)
        ))))

(defun insert-constant-internal (snippet constant format)
  (vdbg "in insert-constant-internal!~%")
  (let ((parent (snippet-parent snippet))
        (new-snippet 
         (create-subtemplate-snippet 
          (snippet-parent snippet) (copy-list `(:constant ,constant)))))
    (insert-new-snippet-into-snippet-hole snippet new-snippet)
    (when (workspace-root-node? parent)
      (vdbg "doing toplevel insert ~%")
      ;; we're creating a constant node from a hole at toplevel.
      ;; The hole node goes away, so save the template which will
      ;; recreate it on the property list of the new constant node
      (set-snippet-property 
       new-snippet :hole-template 
       (get-snippet-property snippet :hole-template)
       ))
    ;; (set-snippet-info snippet :hole-open nil)
    (set-snippet-info new-snippet :format format)
    (setq *newly-inserted-snippet* new-snippet)
    ))

;;; INSERT SYMBOL INTO HOLE operator (no type checking for now)

(defun insert-symbol-into-hole (snippet-id symbol &optional (format "~S"))
  (let* ((snippet (find-snippet-in-workspace snippet-id))
         (p (snippet-parent snippet)))
    (verify-hole snippet symbol)
    (insert-symbol-internal snippet symbol format)
    (setq *modified-snippet* p)
    ))

(defun insert-symbol-internal (snippet symbol format)
  (let ((parent (snippet-parent snippet))
        (new-snippet 
         (create-subtemplate-snippet 
          (snippet-parent snippet)
          (copy-list `(:symbol ,symbol))))
        (arg-snippet? (argument-snippet? snippet))
        (place? (place-snippet? snippet)))
    (insert-new-snippet-into-snippet-hole snippet new-snippet)
    (when (workspace-root-node? parent)
      (vdbg "doing toplevel insert ~%")
      ;; we're creating a symbol node from a hole at toplevel.
      ;; The hole node goes away, so save the template which will
      ;; recreate it on the property list of the new constant node
      (set-snippet-property 
       new-snippet :hole-template 
       (get-snippet-property snippet :hole-template)
       ))
    (set-snippet-info new-snippet :format format)
    (set-snippet-info 
     new-snippet :symbol-node-type 
     (cond
      (place? :place)
      (arg-snippet? :arg)
      (t :value)
      ))
    (setq *newly-inserted-snippet* new-snippet)
    ))


(defun insert-new-snippet-into-snippet-hole (hole-snippet new-snippet)
  (let ((parent (snippet-parent hole-snippet)))
    (unless parent 
      (vpl-internal-error "All snippets should have parents now!"))
    (replace-snippet new-snippet hole-snippet parent)
    nil))

;;; CREATE NEW FORM AT END OF AGGREGATE NODE

(defmacro with-aggregate-node (snippet function-name &body body)
  (let ((snippet-symbol (gensym "SNIPPET-")))
    `(let ((,snippet-symbol ,snippet))
       (typecase ,snippet-symbol
         (aggregate-snippet ,@body)
         (otherwise 
          (vpl-internal-error 
           "~A called on non-aggregate node!" ',function-name)
          )))))

(defun aggregate-subtemplate (snippet)
  (or (snippet-value snippet) 
      (vpl-internal-error "Aggregate snippet has no subtemplate!")))

(defun-opcode another-subform (complex-sid)
  (vdbg "in another-subform")
  (let ((snippet (find-snippet-in-workspace complex-sid)))
    (with-aggregate-node snippet another-subform
      ;; Consistency check 
      (when (or (get-snippet-property snippet :one-form-required) 
                #+oops
                (get-snippet-property snippet :display-one-hole))
        (when (null (snippet-children snippet))
          (vpl-internal-error 
           "Aggregate snippet is supposed to have at least one child!"
           )))
      (let ((new-snippet (another-subform-of-snippet snippet)))
        (setq *modified-snippet* snippet)
        (setq *focus-snippet* new-snippet)
        ))))

(defun-opcode two-more-subforms (complex-sid)
  (vdbg "in two-more-subforms")
  (another-subform complex-sid) 
  (another-subform complex-sid) 
  t
  )

(defmethod another-subform-of-snippet ((snippet t))
  (vpl-internal-error 
   "Called another-subform-of-snippet on non-aggregate snippet!"
   ))

(defmethod another-subform-of-snippet ((snippet aggregate-snippet))
  (let* ((subtemplate (aggregate-subtemplate snippet))
         (new-snippet 
          (create-subtemplate-snippet snippet (copy-tree subtemplate))))
    (add-snippet-as-last-component new-snippet snippet)
    new-snippet
    ))

;;; CLEAR ALL CHILDREN OF AN AGGREGATE NODE 

(defun-opcode clear-all-subforms (complex-sid)
  (vdbg "Clearing all children of aggregate node!~%")
  (let ((snippet (find-snippet-in-workspace complex-sid)))
    (with-aggregate-node snippet clear-all-subforms 
      (setf (snippet-children snippet) nil)
      (when (get-snippet-property snippet :display-one-hole)
        (let* ((subtemplate (aggregate-subtemplate snippet))
               (new-first-child 
                (create-subtemplate-snippet snippet subtemplate)))
          (setf (snippet-children snippet) (list new-first-child))
          ))
      (setq *modified-snippet* snippet)
      (setq *focus-snippet* snippet)
      )))

;;; ADD A CHILD NODE TO THE LEFT OR RIGHT OF AN EXISTING CHILD NODE.

(defun-opcode add-subform-left (subform-sid)
  (vdbg "in add-subform-left")
  (multiple-value-bind (parent new old pos)
      (add-another-for-insertion subform-sid)
    (setf (snippet-children parent) 
          (append (subseq old 0 pos) (list new) (subseq old pos)))
    (setq *modified-snippet* parent)
    (setq *focus-snippet* new)
    ))

(defun-opcode add-subform-right (subform-sid)
  (vdbg "in add-subform-right")
  (multiple-value-bind (parent new old pos)
      (add-another-for-insertion subform-sid)
    (setf (snippet-children parent) 
          (append (subseq old 0 (1+ pos)) (list new) (subseq old (1+ pos))))
    (setq *modified-snippet* parent)
    (setq *focus-snippet* new)
    ))

(defun add-another-for-insertion (subform-sid)
  (let* ((snippet (find-snippet-in-workspace subform-sid))
         (parent (snippet-parent snippet)))
    (with-aggregate-node parent add-another-for-insertion
      (another-subform (snippet-id parent))
      (let* ((sv (snippet-children parent))
             (new (lastelem sv))
             (old (butlast sv))
             (pos (position snippet sv)))
        (values parent new old pos)
        ))))    

(defun add-snippet-as-last-component (snippet parent)
  (setf (snippet-children parent) 
        (append (snippet-children parent) (list snippet))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; Code to implement various CHOICE node operations.

(defun choice-ok? (n max type)
  (unless (and (plusp n) (<= n max))
    (vpl-internal-error
     "~A: Choice ~D selected, but there are only ~D choices." 
     type n max)))

(defmacro with-choice-node (snippet function-name &body body)
  (let ((snippet-symbol (gensym "SNIPPET-")))
    `(let ((,snippet-symbol ,snippet))
       (typecase ,snippet-symbol
         (choice-snippet ,@body)
         (otherwise 
          (vpl-internal-error 
           "~A called on non-choice node!" ',function-name)
          )))))


(defun verify-choice-consistency 
       (literal? instantiated? single? multiple? 
                 required? optional? repeatable? non-repeatable?)
  (flet ((oops (c1 c2 c1-value c2-value)
           (vpl-internal-error 
            "Choice options ~S (value ~A) and ~S (value ~A) are incompatible!"
            c1 c1-value c2 c2-value)))
    (when (and literal? instantiated?)
      (oops :instantiated :literal instantiated? literal?))
    (when (and single? multiple?)
      (oops :single :multiple single? multiple?))
    (when (and required? optional?)
      (oops :required :optional required? optional?))
    (when (and repeatable? non-repeatable?)
      (oops :repeatable :non-repeatable repeatable? non-repeatable?)
      )))

(defun select-choice (choice-id n)
  (let* ((snippet (find-snippet-in-workspace choice-id))
         (choices (snippet-value snippet))
         (selected-choices (get-snippet-property snippet :selected-choices))
         (literal? (get-snippet-property snippet :literal))
         (instantiated? (get-snippet-property snippet :instantiated))
         (single? (get-snippet-property snippet :single))
         (multiple? (get-snippet-property snippet :multiple))
         (required? (get-snippet-property snippet :required))
         (optional? (get-snippet-property snippet :optional))
         (repeatable? (get-snippet-property snippet :repeatable))
         (non-repeatable? (get-snippet-property snippet :non-repeatable))
         (class (get-snippet-property snippet :class))
         )
    (with-choice-node snippet select-choice 
      (verify-choice-consistency
       literal? instantiated? single? multiple? 
       required? optional? repeatable? non-repeatable?)
      (when single? 
        (when (> (length (snippet-children snippet)) 1)
          (vpl-internal-error 
           "Singular choice node but more than one choice/child selected."
           )))
      (when non-repeatable? 
        (when (member n selected-choices)
          (vpl-internal-error 
           "Nonrepeatable choice node provided option for existing choice!"
           )))
      ;; The choice is of the form ("<label>" <subtemplate>)
      ;; unless the choice node is a literal node in which case the choice
      ;; is simply a label
      (let* ((choice (nth (1- n) choices))
             (new-snippet 
              (create-subtemplate-snippet
               snippet
               (if literal? 
                   `(:literal ,(string choice) :class ,class)
                 (second choice)
                 ))))
        (cond
         (single?
          (setf (snippet-children snippet) (list new-snippet))
          ;; make sure any previous choice disappears
          (clear-all-choice-positions snippet)
          ;; record the current choice
          (record-choice-at-position snippet (1- n) 0))
         (multiple? 
          (add-snippet-as-last-component new-snippet snippet)
          (record-choice-at-position 
           snippet (1- n) (1- (length (snippet-children snippet))))
          ))
        (setq *modified-snippet* snippet)
        (setq *focus-snippet* new-snippet)
        ))))

;;; The :selected-choices data looks like:

;;; ((0 (1)) (1 nil) (2 (0 2)) ...)
;;; This means that choice 0 (the first choice) has been selected by the user
;;; and its instantiation is the second child of the choice node; 
;;; choice 1 (the second choice)
;;; has not been selected; and choice 2 (the third choice) has been selected
;;; twice by the user (assuming it is valid to do so), and its instantiations
;;; occupy the first and third child positions of the choice node. 

(defun clear-all-choice-positions (snippet)
  (let ((selected-choices (get-snippet-property snippet :selected-choices)))
    (loop for info in selected-choices 
          do 
          (setf (second info) nil)
          )))

(defun record-choice-at-position (snippet choice-number position)
  (let ((selected-choices (get-snippet-property snippet :selected-choices)))
    (vdbg "In record: Selected choices: ~S~%" selected-choices)
    (let ((particular-choice-info (assoc choice-number selected-choices)))
      (unless particular-choice-info 
        (vpl-internal-error "No info for choice ~D." choice-number))
      (push position (second particular-choice-info))
      (set-snippet-property snippet :selected-choices selected-choices)
      )))

(defun unrecord-choice-at-position (snippet position)
  (let ((selected-choices (get-snippet-property snippet :selected-choices)))
    (vdbg "In unrecord for ~D: Selected choices: ~S~%" 
          position selected-choices)
    ;; A position should occur exactly once in the choice information.  
    (let ((info-to-be-modified nil))
      (loop for info in selected-choices 
            as positions = (second info)
            do 
            (when (member position positions)
              (when info-to-be-modified
                (vpl-internal-error 
                 "Position ~D found more than once!" position))
              (setq info-to-be-modified info)
              ))
      (unless info-to-be-modified 
        (vpl-internal-error 
         "Position ~D not found in selected-choice info!" position))
      ;; Remove the position
      (setf (second info-to-be-modified) 
            (remove position (second info-to-be-modified))
            ))
    ;; Since the snippet at position POSITION is being removed, 
    ;; all the snippets subsequent to it will change their position.  
    ;; Modify all the positions greater than POSITION by -1.  
    (loop for info in selected-choices 
          as positions = (second info)
          do
          (setf 
           (second info) 
           (mapcar (lambda (x) (if (> x position) (1- x) x)) positions)
           ))
    ;; We don't have to store back into the :selected-choices property 
    ;; because the data was already destructively modified.  
    (vdbg "Choice at position ~D unrecorded.~%" position)
    nil
    ))
      
                                    

;;; CHANGE A SNIPPET BACK INTO AN INPUT BOX WITH THE CURRENT VALUE 
;;; AVAILABLE FOR EDITING.

(defun-opcode edit-snippet (sid) 
  (vdbg "In edit-snippet...~%")
  (let* ((snippet (find-snippet-in-workspace sid))
         (parent (snippet-parent snippet))
         (current-value (snippet-value snippet))
         (original-hole-subtemplate 
          (find-original-hole-subtemplate snippet :none)))
    (typecase snippet
      ((or constant-snippet symbol-snippet) nil)
      (otherwise 
       (vpl-internal-error 
        "Should not be able to edit a ~S snippet!" 
        (type-of snippet)
        )))                           
    (when (eq original-hole-subtemplate :none)
      (vpl-internal-error 
       #.(one-string-nl
          "Attempting to edit snippet which was not"
          "originally a hole which was filled in."
          )))
    (let ((new-hole-snippet 
           (create-subtemplate-snippet parent original-hole-subtemplate)))
      (if (get-snippet-property snippet :multiline-input)
          (progn
            (set-snippet-property new-hole-snippet :hole-open-multiline t)
            (set-snippet-property new-hole-snippet :hole-open nil))
        (progn
          (set-snippet-property new-hole-snippet :hole-open t)
          (set-snippet-property new-hole-snippet :hole-open-multiline nil)))
      (set-snippet-property
       new-hole-snippet :contents 
       (etypecase snippet 
         (constant-snippet (formatn "~S" current-value))
         (symbol-snippet (formatn "~A" (string-downcase current-value)))
         ))
      (replace-snippet new-hole-snippet snippet parent)
      ;; This doesn't work because when we redraw 
      ;; the selected box gets nulled out
      ;; (do-box-selected sid)
      (setq *modified-snippet* parent)
      (setq *focus-snippet* new-hole-snippet)
      new-hole-snippet
      )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The default method returns NIL, signaling that it wasn't really handled.
;; If the object has been handled, non-nil must be returned.
(defmethod describe-vpl-output ((obj t)) nil)

(defmethod describe-vpl-output ((obj frames::%frame))
  (show-vpl-popup-URL-window 
   (s+ "/" (forward-package-funcall :de :simple-frame-editor-url obj))
   :width "1000px" :height "800px"
   )
  t
  )

(defun-opcode describe-output (oid)
  (vdbg "in describe-output")
  (not-modified!) 
  (let* ((snippet (find-snippet-in-output-history oid)))
    (describe-output-node snippet)
    ))

(defmethod describe-output-node ((snippet t) &optional (index nil))
  (declare (ignore index))
  (vpl-internal-error "Calling describe-output-node on non-output node!")
  )

(defmethod describe-output-node 
           ((snippet output-value-snippet) &optional (index nil))
  (declare (ignore index))
  (let* ((children (snippet-children (snippet-parent snippet)))
         (pos (position snippet children))
         (values (get-snippet-property (snippet-parent snippet) :values))
         (value (nth pos values)))
    (ulog "Output node description")
    (create-and-use-unique-file 
     (user-temp-vpl-dir)
     (lambda (file p)
       (declare (ignore file))
       (format
        p "~A" 
        (with-output-to-string (s) 
          (format s "This is value ~D (of ~D)~%" (1+ pos) (length children))
          (vif (c (get-snippet-info snippet :error-condition))
               (format s "Error: ~A" c)
               (describe value s)
               ))))
     (lambda (file) (show-vpl-popup-URL-window (user-temp-vpl-dir-url file)))
     :name (s+ "describe-" (string wb::*sessionid*))
     :type "txt"
     )))

(defmethod describe-output-node 
           ((snippet toplevel-output-snippet) &optional (index 0))
  (let* ((values (get-snippet-property snippet :values))
         (value (nth index values))
         (vlen (length values)))
    ;; if there's only one value returned, and the type is handled
    ;; by a describe-output method (which signals that it handled
    ;; the value by returning t), then we're done.
    (when (= vlen 1) 
      (when (describe-vpl-output (first values)) 
        (return-from describe-output-node nil)
        ))
    (create-and-use-unique-file 
     (user-temp-vpl-dir)
     (lambda (file p)
       (declare (ignore file))
       (format
        p "~A" 
        (with-output-to-string (s) 
          (vif (c (get-snippet-info snippet :error-condition))
               (format s "Error: ~A" c)
               (cond
                ((= vlen 1) (describe value s))
                (t 
                 (format s "This output consists of ~D values.~%" vlen)
                 (loop for v in values
                       for j from 1
                       do 
                       (format s "  Value ~D is of type ~S~%" 
                               j (bbi::bbl-printed-type-of v)
                               ))
                 (format
                  s
                  "Select 'Describe' on each value's menu for more info.")
                 ))))))
     (lambda (file) (show-vpl-popup-URL-window (user-temp-vpl-dir-url file)))
     :name (s+ "describe-" (string wb::*sessionid*))
     :type "txt"
     )))

(defun-opcode show-printout (sid)
  (vdbg "in show-printout")
  (not-modified!)
  (let* ((snippet (find-snippet-in-output-history sid))
         (printout (get-snippet-property snippet :printout))
         (index (get-snippet-property snippet :output-index))
         (title "VPL execution printout"))
    (unless printout       
      (vpl-internal-error "SHOW-PRINTOUT called but no printout!"))
    (ulog title)
    (create-and-use-unique-file 
     (user-temp-vpl-dir)
     (lambda (file p) 
       (declare (ignore file))
       (with-html-to-stream p 
         (:html 
          (:title title)
          (:pre 
           (wb::princ-with-frame-links 
            (format nil "Printout for result ~D:~%~A~%" index printout)
            :pkg wb:*sessionid* :remove-frame-prefix? t)
           ))))
     ;; (format p "Printout for result ~D:~%~A~%" index printout))
     (lambda (file) 
       (show-vpl-popup-URL-window (user-temp-vpl-dir-url file) :relative-p 0))
     :name (s+ "printout-" (string wb::*sessionid*))
     :type "html"
     )))


;;; COLLAPSE, EXPAND AND BOX NAMING OPERATORS 

(defun collapse-snippet-display-aux (sid)
  (let ((snippet (find-snippet-in-workspace-or-output-history sid)))
    (set-snippet-info snippet :collapsed? t)
    (typecase snippet
      (output-snippet
       (redraw-everything-in-results))
      (otherwise 
       (setq *modified-snippet* snippet)
       (setq *focus-snippet* snippet)
       ))))
          

(defun-opcode collapse-snippet-display (sid)
  (vdbg "in collapse-snippet-display")
  (collapse-snippet-display-aux sid)
  )

(defun-opcode named-collapse-snippet-display (sid)
  (vdbg "in named-collapse-snippet-display")
  (collapse-snippet-display-aux sid)
  (label-collapsed-box-aux sid nil)
  )
  

(defun-opcode expand-snippet-display (sid)
  (vdbg "in expand-snippet-display")
  (let ((snippet (find-snippet-in-workspace-or-output-history sid)))
    (set-snippet-info snippet :collapsed? nil)
    (typecase snippet
      (output-snippet
       (redraw-everything-in-results))
      (otherwise 
       (setq *modified-snippet* snippet)
       (setq *focus-snippet* snippet)
       ))))

(defun label-collapsed-box-aux (sid not-modified?)
  (when not-modified? (not-modified!))
  (let ((label-function-symbol 
         (intern (s+ "LABEL-BOX-" (formatn "~D" sid)) :vpl)))
    (unless (fboundp label-function-symbol)
      (eval 
       `(defun ,label-function-symbol (new-name)
          (label-collapsed-box-function-action ,sid new-name))))
    (show-dialog 
     (symbol-name label-function-symbol)
     "Please enter a descriptive name for your collapsed box"
     "Node name:" 0
     )))

(defun-opcode label-collapsed-box-function (sid)
  (vdbg "in label-collapsed-box-function")
  (label-collapsed-box-aux sid t)
  )

(defun label-collapsed-box-function-action (sid new-name)
  (let ((node (find-snippet-in-workspace-or-output-history sid)))
    (set-snippet-property node :collapsed-name new-name)
    ))

(defun-opcode unlabel-collapsed-box-function (sid)
  (vdbg "in unlabel-collapsed-box-function")
  (let ((node (find-snippet-in-workspace-or-output-history sid)))
    (set-snippet-property node :collapsed-name nil)
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; EXECUTE


(defun-opcode execute-function (sid)
  (vdbg "in execute-function")
  (not-modified!)
  (handle-eval-request sid))


;;; COPY, CUT, PASTE, INSERT, AND CUT/INSERT

(defun-opcode copy-function (sid)
  (vdbg "in copy-function")
  (let ((*current-selected-boxid* sid))
    (handle-copy-snippet)
    ))

(defun workspace-parent-modified (snippet-or-id)
  (let ((snippet 
         (if (typep snippet-or-id 'snippet)
             snippet-or-id
           (find-snippet-in-workspace-or-output-history snippet-or-id))))
    (typecase snippet
      (output-snippet nil)
      (otherwise
       (setq *modified-snippet* (snippet-parent snippet)))
      )))

(defun-opcode cut-function (sid)
  (vdbg "in cut-function")
  (let ((*current-selected-boxid* sid))
    ;; The order of these operations is critical!
    (workspace-parent-modified sid)
    (handle-cut-snippet)
    ))

(defun-opcode paste-function (sid)
  (vdbg "In paste-function")
  (let ((*current-selected-boxid* sid))
    ;; The order of these operations is critical!
    (workspace-parent-modified sid)
    (handle-paste-snippet)
    ))

(defun-opcode insert-function (sid)
  (vdbg "in insert-function")
  (let ((snippet (find-snippet-in-workspace-or-output-history sid)))
    (unless *current-selected-boxid* 
      (vpl-user-error "No selected object to insert into!"))
    ;; The order of these operations is critical!
    (workspace-parent-modified *current-selected-boxid*)
    (handle-insert-into-selected-hole snippet)
    ))

;;; Cannot set *modified-snippet* because 2 snippets are being modified

(defun-opcode cut-insert-function (sid)
  (vdbg "in cut-insert-function")
  (let ((snippet (find-snippet-in-workspace-or-output-history sid)))
    (unless *current-selected-boxid* 
      (vpl-user-error "No selected object to insert into!"))
    (handle-cut-insert-into-selected-hole snippet)
    ))

(defun-opcode paste-toplevel-function (sid)
  "Paste a copied box directly to the workspace"
  (declare (ignore sid))
  (vdbg "in paste-toplevel-function")
  (unless *current-clipboard-box* 
    (vpl-user-error "Nothing in clipboard to paste!"))
  (let* ((root (workspace-root-node)) 
         (copy (pcopy-snippet *current-clipboard-box* root t)))
    (add-to-workspace-toplevel copy)
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun-opcode show-code-function (sid)
  (vdbg "in show-code-function")
  (not-modified!)
  (let (snippet code)
    (setq snippet (find-snippet-in-workspace sid))
    (setq code
          (handler-case
              (snippet-to-code snippet t)
            (hole-error
             ()
             (vpl-internal-error "Hole error but holes OK!"))
            (vpl-user-error (c) (signal c))
            (error
             (c)
             (vpl-internal-error 
              #.(one-string-nl
                 "In show-code-function:  Snippet->code failing!~%Snippet: ~S~%"
                 "Snippet->code error: ~A")
              snippet c
              ))))
    (show-code-function-aux code)
    ))

(defun show-code-function-aux (code)
  (ulog "Displaying show code window")
  (create-and-use-unique-file 
   (user-temp-vpl-dir)
   (lambda (file p) 
     (vdbg "Writing snippet to ~A~%" (namestring file))
      (format p "") ; (format p "(in-package ~S)~%~%" wb:*username*)
    ; (pprint code p)
     (FORWARD-FUNCALL 'bbi::DISPLAY-CODE code p)
     )
   (lambda (file) 
     (show-vpl-popup-URL-window 
      (user-temp-vpl-dir-lisp-listing-url file)
      :height "500px" :width "500px" :left "500px" :top "500px" :relative-p 0))
   :name (s+ "code-" (string wb::*sessionid*))
   :type "lisp"
   ))
     

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun create-add-gene-hole-operator (organism-fname)
  (intern
   (formatn "ADD-GENE-HOLE-FOR-~A" (string-upcase organism-fname))
   *vpl-package*))

(defun create-add-protein-hole-operator (organism-fname)
  (intern 
   (formatn "ADD-PROTEIN-HOLE-FOR-~A" (string-upcase organism-fname))
   *vpl-package*))

(defun add-protein-hole (sid organism-fname)
  (add-gene-or-protein-hole 
   sid organism-fname 'bbl::protein "protein" :protein))

(defun add-gene-hole (sid organism-fname)
  (add-gene-or-protein-hole 
   sid organism-fname 'bbl::gene "gene" :gene))

(defun add-gene-or-protein-hole (sid organism-fname bbl-type text insert-type)
  (declare (ignore sid))
  (vdbg "Creating ~A hole for ~S~%" text organism-fname)
  (let ((parent nil) (new-hole nil))
    (flet ((new-gene-hole () 
             (let ((hole 
                    (create-subtemplate-snippet 
                     parent 
                     (copy-list `(:form ,text ,bbl-type))
                     )))
               (set-snippet-info hole :insert-type insert-type) 
               (set-snippet-info hole :organism-fname organism-fname)
               hole
               )))
      (if (null *current-selected-boxid*)
          (progn
            (vdbg "Adding ~A hole at toplevel~%" text)
            (setq parent (workspace-root-node))
            (setq new-hole (new-gene-hole))
            (add-to-workspace-toplevel new-hole)
            (setq *modified-snippet* nil)
            )
        (let ((snippet (find-snippet-in-workspace-or-output-history
                        *current-selected-boxid*)))
          (unless (snippet-is-hole? snippet)
            (vpl-user-error "Cannot overwrite non-hole yet!"))
          (vdbg "Overwriting selected hole with ~A~%" text)
          (setq parent (snippet-parent snippet))
          (setq new-hole (new-gene-hole))
          (replace-snippet new-hole snippet parent)
          (setq *modified-snippet* parent)
          )))))

(defun find-gene-from-user-input (user-input organism-fname)
  (find-gene-or-protein-from-user-input :gene user-input organism-fname))

(defun find-protein-from-user-input (user-input organism-fname)
  (find-gene-or-protein-from-user-input :protein user-input organism-fname))

(defun find-gene-or-protein-from-user-input (type user-input organism-fname)
  (let ((orgf (frames:frame-fnamed organism-fname))
        (user-typein (remove-all-whitespace user-input)))
    (unless orgf (vpl-internal-error "No organism frame for ~S" organism-fname))
    (find-gene-or-protein-from-user-typein type user-typein orgf)
    ))

(defun find-gene-or-protein-from-user-typein (type user-input orgf &key no-err)
  (let ((list nil)
        (search-list
         (ecase type (:gene (#^genes orgf)) (:protein (#^proteins orgf)))))
    (unless (zerop (length user-input))
      (loop for item in search-list
            do
            (when (search user-input (frames:fname item) :test 'char-equal)
              (push item list)
              )))
    (handle-genes-or-proteins-found type list orgf user-input :no-err no-err)
    ))

(defun handle-genes-or-proteins-found (type items orgf input &key no-err)
  (let ((len (length items)) (limit 15))
    (cond
     ((zerop len)
      (unless no-err
        (vpl-user-error
         "Cannot find ~A in ~S which matches ~A"
         type orgf (if (zerop (length input)) "the null string" input)))
      )
     ((= len 1) (first items))
     (t
      (if no-err
          items
        (vpl-user-error
         (formatn
          (s+
           (apply
            's+
            "Ambiguous result.  More than one ~A in ~S matches ~A.~%"
            "The ~D ~As that match are: ~%"
            (loop for item in items
                  for j from 1 to limit
                  collect
                  (s+ "   " (frames:fname item) #\Newline)))
           (if (<= len limit) "" (formatn "   ... (~D more)~%" (- len limit)))
           "Please type in the full ~A name into the input box instead~%"
           "of just the numeric suffix or a substring.~%"
           )
          type (frames:fname orgf) input len type type
          )))))))

         
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun-opcode hide-function (sid)
  (vdbg "in hide-function")
  (let ((snippet (find-snippet-in-workspace-or-output-history sid)))
    (mark-snippet-as-hidden snippet)
    ))

(defun tag-snippet-as-hidden (snippet)
  (when (toplevel-snippet? snippet) 
    (vpl-internal-error "Trying to hide a snippet at toplevel!"))
  (let* ((parent (snippet-parent snippet))
         (hidden-nodes (get-snippet-info parent :hidden-nodes))
         (pos (position snippet (snippet-children parent))))
    (when (find pos hidden-nodes)
      (vpl-internal-error "Node is already hidden!"))
    (vdbg "Hiding snippet at position ~D~%" pos)
    (set-snippet-info 
     parent :hidden-nodes (sort (cons pos hidden-nodes) '<))
    (set-snippet-info snippet :hidden-node t)
    ))

(defun mark-snippet-as-hidden (snippet)
  (tag-snippet-as-hidden snippet)
  (setq *modified-snippet* (snippet-parent snippet))
  ) 
    
(defun-opcode reveal-function (sid)
  (vdbg "in reveal-function")
  (let ((snippet (find-snippet-in-workspace-or-output-history sid)))
    (set-snippet-info snippet :hidden-nodes nil)
    (loop for child in (snippet-children snippet) do 
          (set-snippet-info child :hidden-node nil))
    (setq *modified-snippet* snippet)
    ))

(defun reveal-component-operator (sid number)
  (vdbg "In reveal-component-operator, index = ~D~%" number)
  (let* ((snippet (find-snippet-in-workspace-or-output-history sid))
         (hidden-nodes (get-snippet-info snippet :hidden-nodes))
         (pos (1- number))
         )
    (vdbg "Hidden nodes list: ~A~%" hidden-nodes)
    (unless (find pos hidden-nodes :test '=)
      (vpl-internal-error 
       "Menu option for revealing component ~D but ~D not on hidden nodes list"
       pos pos
       ))
    (set-snippet-info 
     snippet :hidden-nodes (remove pos hidden-nodes :test '=))
    (let ((revealed-child (nth pos (snippet-children snippet))))
      (unless revealed-child 
        (vpl-internal-error 
         "~D on hidden node list but no child found at that index!"
         pos
         ))
      (set-snippet-info revealed-child :hidden-node nil)
      (setq *modified-snippet* snippet)
      )))
                          
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun example-code-operator (sid-ignore example-code-file-index)
  (declare (ignore sid-ignore))
  (vdbg "in example-code-operator")
  (not-modified!)
  (redraw!)
  (let ((example-subdir
         (elt *example-code-files* (1- example-code-file-index))))
    (restore-vpl-user-session example-subdir :type :example)
    )
  (redraw-results)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun-opcode multiline-input-function (sid)
  (vdbg "In multiline-input-function...~%")
  (let ((snippet (find-snippet-in-workspace sid)))
    (unless (typep snippet 'form-snippet)
      (vpl-internal-error "Trying to do multiline input on non form snippet!"))
    (hole-open-multiline snippet)
    (highlight-snippet-box snippet)
    (setq *modified-snippet* snippet)
    (setq *vpl-workspace-modified?* t)
    (setq *disable-selected-box-on-redraw?* nil)
    (redraw!)
    nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun-opcode remove-all-my-functions (sid)
              "Remove all functions from the 'My Functions' menu"
  (declare (ignore sid))
  (ulogdbg "removing all user functions from menu")
  (remove-all-variables-or-functions :functions)
  (show-status "All functions expunged.")
  )

(defun-opcode remove-all-my-variables (sid)
  "Remove all variables from the 'My Variables' menu"
  (declare (ignore sid))
  (ulogdbg "removing all user variables from menu")
  (remove-all-variables-or-functions :variables)
  (show-status "All variables expunged.")
  )

(defun remove-all-variables-or-functions (type)
  (let ((property nil) (function nil))
    (ecase type
      (:variables 
       (setq property :my-vpl-variables) 
       (setq function 'expunge-a-variable))
      (:functions 
       (setq property :my-vpl-functions) 
       (setq function 'expunge-a-function)))
    (not-modified!)
    (let ((menu (get wb::*username* property)))
      (destructuring-bind (menuid menu-name submenus entries)
          menu
        (declare (ignore menuid menu-name submenus))
        (let ((n (1- (length entries))))
          (loop for j from 0 below n 
                do
                (funcall function nil 0 :show-status? nil)
                )))))
  nil
  )

(defun expunge-a-variable 
       (sid-ignore variable-index 
                   &key (show-status? t) (error :internal-error))
  (declare (ignore sid-ignore))
  (block exit 
    (let ((menu (get wb::*username* :my-vpl-variables)))
      (unless menu 
        (ecase error
          (:internal-error (vpl-internal-error "No variables menu!"))
          (:ignore (return-from exit nil))
          ))
      (destructuring-bind (menuid menu-name submenus entries)
          menu
        (declare (ignore menu-name submenus))
        ;; one menu item (forget all) above the list of variables
        (let* ((menu-item (nth (1+ variable-index) entries))
               (variable-name (first menu-item))
               (variable 
                (intern (string-upcase variable-name) wb::*username*)))
          (unless (boundp variable) 
            (ecase error
              (:internal-error 
               (vpl-internal-error 
                "Variables menu variable ~A has no value!" variable))
              (:ignore (return-from exit nil))
              ))
          (ulogdbg "Expunging user variable ~S" variable)
          (makunbound variable)
          ;; cannot unintern symbol because the symbol may have
          ;; a function definition or other uses
          (if (= 2 (length entries))
              ;; this is the only variable, blow away entire menu
              (progn
                (remove-a-palette-menu menuid)
                (setf (get wb::*username* :my-vpl-variables) nil))
            (remove-item-from-my-variables-menu variable-name))
          (when show-status?
            (show-status (format nil "~S is now expunged." variable-name)))
          )))))

(defun expunge-a-function 
       (sid-ignore function-index &key (error :internal-error) (show-status? t))
  (declare (ignore sid-ignore))
  (block exit
    (let ((menu (get wb::*username* :my-vpl-functions)))
      (unless menu 
        (ecase error 
          (:internal-error (vpl-internal-error "No functions menu!"))
          (:ignore (return-from exit nil))
          ))
      (destructuring-bind (menuid menu-name submenus entries)
          menu
        (declare (ignore menu-name submenus))
        ;; one menu item (forget all) above the list of variables
        (let* ((menu-item (nth (1+ function-index) entries))
               (function-name (first menu-item))
               (function
                (intern (string-upcase function-name) wb::*username*)))
          (when (eq (find-package wb::*username*) (symbol-package function))
            (unless (fboundp function)
              (ecase error 
                (:internal-error
                 (vpl-internal-error 
                  "Functions menu function ~A has no definition!" function))
                (:ignore (return-from exit nil))
                ))
            (ulogdbg "Expunging user function ~S" function)
            (fmakunbound function)
            #+:allegro
            ;; stop the lisp compiler from complaining about wrong number
            ;; of arguments if a user tries to use the function that was just
            ;; expunged
            (setf (get function 'excl::.args.) nil))
          (if (= 2 (length entries))
              ;; this is the only function, blow the entire menu away
              ;; (the first menu item is 'forget all')
              (progn
                (remove-a-palette-menu menuid)
                (setf (get wb::*username* :my-vpl-functions) nil))
            ;; other functions, just eliminate this one from the menu
            (remove-item-from-my-functions-menu function-name))
          (when show-status?
            (show-status (format nil "~S is now expunged." function-name)))
          )))))

(defun expunge-symbol-from-my-menu (f type &key (error :ignore))
  (block exit
    (let ((menu (get wb::*username* type)))
      (destructuring-bind (menuid menu-name submenus entries)
          menu
        (declare (ignore menu-name menuid submenus))
        (loop for menu-item in (cdr entries)
              as function-name = (first menu-item)
              for j from 0
              do
              (when (string-equal function-name (string f))
                (ecase type 
                  (:my-vpl-functions (expunge-a-function nil j :error error))
                  (:my-vpl-variables (expunge-a-variable nil j :error error)))
                (return-from exit nil)
                ))
        (ecase error
          (:internal-error
           (vpl-internal-error "Function ~S not found on functions menu!" f))
          (:ignore nil)
          )))))
                               
                               
            

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Favorites operators 

(defun-opcode add-favorite-function (sid)
              "Add a function to your 'Favorites' palette menu for quick access"
  (declare (ignore sid))
  (vdbg "in add-favorite-function")
  (not-modified!)
  (show-dialog  
   'add-user-supplied-favorite
   ;; "add-user-supplied-favorite" 
   "Please enter an operator to add to your favorites list:"
   "VPL operator:" 0
   ))

(defun-opcode delete-favorite-function (sid)
              "Delete a function from your 'Favorites' palette menu"
  (declare (ignore sid))
  (vdbg "in delete-favorite-function")
  (not-modified!)
  (show-dialog 
   'delete-user-supplied-favorite
   ;; "delete-user-supplied-favorite" 
   "Please enter an operator to delete from your favorites list:"
   "VPL operator:" 0
   ))

(defun-opcode clear-favorites-function (sid)
              "Delete all functions from your 'Favorites' palette menu"
  (declare (ignore sid))
  (vdbg "in clear-favorites-function")
  (not-modified!)
  (clear-favorites-data)
  )

(defun-opcode forbid-favorite-function (sid)
              #.(one-string
                 "Forbid a function from appearing on your"
                 " 'Favorites' palette menu, no matter how many"
                 " times you use it"
                 )
  (declare (ignore sid))
  (vdbg "in forbid-favorite-function")
  (not-modified!)
  (show-dialog 
   'forbid-user-supplied-favorite
   ;; "forbid-user-supplied-favorite" 
   "Please enter an operator to keep off your favorites list:"
   "VPL operator:" 0
   ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun-opcode monitor-function (sid)
  (vdbg "in monitor-function")
  (let ((node (find-snippet-in-workspace sid)))
    (set-snippet-property 
     node :monitoring-enabled
     (not (get-snippet-property node :monitoring-enabled)))
    (setq *modified-snippet* node)
    ))

(defun-opcode view-function (sid)
  (vdbg "in view-function")
  (let ((node 
         (or (find-snippet-in-results-area sid nil) 
             (find-snippet-in-workspace sid nil)
             (vpl-internal-error 
              "Could not find snippet in workspace or results area!"
              )))
        (value nil))
    (etypecase node 
      (toplevel-output-snippet 
       (setq value (first (get-snippet-property node :values)))
       )
      (output-value-snippet
       (let* ((parent (snippet-parent node))
              (pos (position node (snippet-children parent))))
         (setq value (nth pos (get-snippet-property parent :values)))
         ))
      (constant-snippet 
       (setq value (snippet-value node))
       (when (and (symbolp value) (boundp value))
         (setq value (symbol-value value))
         ))
      (symbol-snippet
       (let ((symbol (snippet-value node)))
         (if (boundp symbol) 
             (setq value (symbol-value symbol))
           (setq value "This should not happen!")
           ))))
    (typecase value 
      ((or wb::jpg wb::url) (process-single-vpl-return-value value))
      (otherwise 
       (multiple-value-bind (editable? why-not)
           (de-edit-object? value)
         (if editable? 
             (process-single-vpl-return-value 
              (wb::make-data-editor-object :data value))
           (show-status why-not)
           ))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun snippet-still-attached? (snippet)
  (let ((p (snippet-parent snippet)))
    (and p (member snippet (snippet-children p)))
    ))

(defun-opcode view-code-function (sid)
  (vdbg "in view-code-function")
  (not-modified!)
  (let* ((output-snippet (find-snippet-in-results-area sid))
         (code (get-snippet-property output-snippet :eval-code))
         (execution-boxid (get-snippet-property output-snippet :eval-boxid)))
    (let ((input-snippet (find-snippet-by-id execution-boxid nil)))
      ;; snippet could have been GCed...
      (when input-snippet
        (when (snippet-still-attached? input-snippet)
          (let ((isid (snippet-id input-snippet)))
            (unless (equalp *current-selected-boxid* isid)
              (highlight-snippet-box input-snippet)
              ))))
      (show-code-function-aux code)
      )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun snippet-not-deleted-and-in-results-area? (s)
  (and (snippet-still-attached? s) (snippet-in-results-area? s)))
       

(defun-opcode results-function (sid)
  (vdbg "in results-function")
  (not-modified!)
  (let* ((snippet (find-snippet-in-workspace sid))
         (output-node-ids (get-snippet-property snippet :output-nodes))
         (nids (length output-node-ids)))
    (cond
     ((zerop nids) (show-status "No results"))
     ((= nids 1) 
      (let ((output-snippet (find-snippet-by-id (first output-node-ids) nil)))
        (if (snippet-not-deleted-and-in-results-area? output-snippet)
            (progn
              (unless (equalp *current-selected-boxid* 
                              (snippet-id output-snippet))
                (highlight-snippet-box output-snippet))
              (focus-box (snippet-id output-snippet)))
          (progn
            (show-status "Result has been deleted")
            (set-snippet-property snippet :output-nodes nil)
            ))))
     (t 
      (let* ((output-snippets 
              (mapcarnn 
               (lambda (x)
                 (let ((os (find-snippet-by-id x nil)))
                   (and os (snippet-not-deleted-and-in-results-area? os) os)
                   ))
               output-node-ids
               ))
             (nos (length output-snippets))
             (os-ids (mapcar 'snippet-id output-snippets)))
        (set-snippet-property snippet :output-nodes os-ids)
        (cond
         ((zerop nos) 
          (show-status "All results have been deleted."))
         ((= 1 nos) (results-function sid))
         (t 
          (let ((an-output-node (first output-snippets)))
            (unless (equalp *current-selected-boxid*
                            (snippet-id an-output-node))
              (highlight-snippet-box an-output-node)
              (focus-box (snippet-id an-output-node))
              ))
          (create-and-use-unique-file 
           (user-temp-vpl-dir)
           (lambda (file p)
             (declare (ignore file))
             (format p "~%This node has ~D results still extant.~%" nos)
             (when (< nos (length output-node-ids))
               (format p "  (~D other results existed but were deleted.)~%"
                       (- (length output-node-ids) nos)))
             (format p "~%The result nodes still extant are:~%~%")
             (loop for os in output-snippets do
                   (format 
                    p "~D>~%"
                    (get-snippet-property os :output-index))))
           (lambda (file) 
             (show-vpl-popup-URL-window (user-temp-vpl-dir-url file)))
           :name (s+ "results-" (string wb::*sessionid*))
           :type "txt"
           )))
        )))))
    
