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

In a key-and-flag subtemplate the second form is a list of keyword
and flag records.  A flag record is simply (:flag <flag-name>).  
A keyword record is more complicated: 

  (:keyword <name> <default-value> <type> <possible-values>)

The default value and possible values information is not currently used.

When a keyword subtemplate is instantiated this information is transformed.
The keyword snippet is given the following information: 

  -- A list of the possible keys, stored in the :keywords property.  
  -- A list of the possible flags, stored in the :flags property. 
  -- The list of keys and flag names in the order they are specified, 
     stored in the :ordered-names property.  
  -- A list of which choice positions are keywords, stored in the 
     :key-indices property 
  -- A list of which choice positions are flags, stored in the
     :flag-indices property
  -- The subtemplates for each possible keyword or flag, stored in the 
     :subtemplates property in the same order as the :ordered-names list

As a key-and-flag snippet is manipulated, keywords and their values,
and flags, are added and removed.  These nodes are maintained as the
snippet-children of the key-and-flag snippet.  

To keep track of which keys and flags the user has selected, lists
stored on the properties :flags-present and :keys-present are
maintained.

The name associated with a keyword or flag snippet is stored in the 
snippet value slot.  Since flag and keyword names are unique we can 
always determine the mapping between keyword/flag snippets and their 
properties.  

The operations on a key-and-flag snippet are:

  -- Clear it out (set to NIL the :flags-present and :keys-present lists, 
and set the children of the snippet to nil)
  -- Add a non-already-chosen flag (goes at the end)
  -- Add a non-already-chosen keyword (goes at the end).  A hole
     is also created an inserted for the keyword value.
  -- Delete a flag that is present
  -- Delete a key that is present along with its corresponding value
  
The key and flag snippets may be deleted (the key values may not be
deleted).  Right now there will be no DELETE option on the keyword
node itself; rather the user will have to first clear the value if the
value isn't a hole, and then delete the hole, causing the entire
keyword node to vanish.  The alternative is to have a box around the
(keyword value) box pair which would contain the delete icon.  This
takes more space but would be clearer.

Note: There is no easy way to rearrange the order of the
keywords/flags once the user has selected an order.  The descriptor
must be cleared and the keywords/flags reentered.

Note: A descriptor with no flags is used to represent a standard
Common Lisp keyword list

||#

;; This is the function that is called by the choice operators for 
;; keys-and-flags nodes.  POS is the index of the key or flag the user
;; selected; it is the index of the name of the flag in the list found
;; in the :ordered-names property of the keys-and-flags node.

(defun choose-another-flag-or-key (sid index)
  (let ((kf-snippet (find-snippet-in-workspace sid))
        (pos (1- index)))
    (cond
     ((member pos (get-snippet-property kf-snippet :key-indices))
      (let ((new-key-snippet (choose-another-keyword kf-snippet index)))
        (setq *focus-snippet* new-key-snippet)
        ))
     ((member pos (get-snippet-property kf-snippet :flag-indices))
      (choose-another-flag kf-snippet index)
      (setq *focus-snippet* kf-snippet))
     (t (vpl-internal-error 
         "Position ~D is neither on the key or flag indices lists" pos
         )))     
    (setq *modified-snippet* kf-snippet)  
    ))

(defun choose-a-set-of-flags-and-keys (sid indices)
  (setq sid (idstring->id sid :snippet-id))
  (setq indices 
        (loop for j from 0 below (length indices)
              as elem = (elt indices j)
              collect
              (handler-case 
                  (parse-integer elem)
                (error 
                 ()
                 (vpl-internal-error 
                  "Non-integer ~A provided by client for multimenu index!" elem
                  )))))
  (loop for index in indices do (choose-another-flag-or-key sid index))
  )

(defun choose-another-flag (snippet pos)
  (let ((flag-subtemplate 
         (nth (1- pos) (get-snippet-property snippet :subtemplates))))
    (unless flag-subtemplate 
      (vpl-internal-error "No subtemplate at position ~D" pos))
    (let ((flag-snippet (create-subtemplate-snippet snippet flag-subtemplate)))
      (add-snippet-as-last-component flag-snippet snippet)
      (set-snippet-property 
       snippet :flags-present 
       (append (get-snippet-property snippet :flags-present) 
               (list (snippet-value flag-snippet))
               )))))

(defun choose-another-keyword (snippet pos)
  (let ((key-subtemplate 
         (nth (1- pos) (get-snippet-property snippet :subtemplates))))
    (unless key-subtemplate 
      (vpl-internal-error "No subtemplate at position ~D" pos))
    (let* ((key-snippet (create-subtemplate-snippet snippet key-subtemplate))
           (key (snippet-value key-snippet)) 
           (key-literal (first (snippet-children key-snippet))))
      (add-snippet-as-last-component key-snippet snippet)
      (set-snippet-property 
       snippet :keys-present 
       (append (get-snippet-property snippet :keys-present) (list key)))
      (set-snippet-property 
       key-literal :format (proper-format-for-bbl-keyword key))
      key-snippet
      )))

(defun proper-format-for-bbl-keyword (function-name)
  (let ((p (symbol-package function-name)))
    (cond
     ((eq p (find-package :common-lisp)) "~S")
     ((eq p (find-package :bbl)) "~A")
     ((eq p (find-package :bbi)) "~A")
     (t "~S")
     )))


;;; The functions which clear the keys-and-flags node and delete the 
;;; keyword and flag nodes are found in (new) clear-and-delete.lisp