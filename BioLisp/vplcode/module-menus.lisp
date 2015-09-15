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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *bbl-functions-not-to-be-in-palette*
  '(
    clear-history
    bbl::matches-of
    bbl::match-of
    bbl::item
    bbl::items
    ))

(defun remove-undesired-module-functions (module-functions)
  (remove-if 
   (lambda (x) 
     (member (string x) *bbl-functions-not-to-be-in-palette* 
             :test 'string-equal :key 'string))
   module-functions
   ))

(defvar *all-functions-alphabetically* nil)


;;; The module tree is a list of module nodes, each node of the form
;;; (module-name submodule-list function-list)
;;; where the submodule-list is a list of module nodes, and
;;; function-list is a list of (function-name unique-id-number)
;;; The unique-id-number is the VPL template-id for the template
;;; identified by the function-name, or -1 if not found

(defun module-already-in-tree? (module-name tree)
  (block exit
    (loop for (name submodule-list nil) in tree do
          (when (string-equal module-name name) (return-from exit t))
          (when (module-already-in-tree? module-name submodule-list)
            (return-from exit t)
            ))))

(defvar *vpl-module-tree* nil)

(defun generate-vpl-module-tree (&optional (force? nil))
  (maybe-create-menu *vpl-module-tree* force?
    (generate-vpl-module-tree-aux)
    ))
      
(defun generate-vpl-module-tree-aux ()
  (let* ((module-item->id-hash (make-hash-table :test 'equal))
         (module-list 
          (hash-table-values (gethash 'help::module help::*documentation*)))
         (module-names-and-ids 
          (let ((id 0))
            (mapcar (lambda (x) (list (help::name x) (incf id))) module-list)))
         (bbl-modules 
          (remove-if-not 
           (lambda (x) (member :bbl (help::display-modes x)))
           module-list
           ))
         (count 0)
         ;; put the toplevel modules into a canonical (alphabetical) order
         (toplevel-modules
          (sort 
           (copy-list (remove-if-not 'help::toplevel? bbl-modules))
           'string-lessp 
           :key 'help::name 
           )))
    (flet ((module-template (m) 
             (list
              (help::name m)
              nil
              (mapcar 
               (lambda (symbol)  
                 (let* ((sname (symbol-name symbol))
                        (unique-id (gethash sname module-item->id-hash)))
                   (unless unique-id 
                     (setq unique-id (incf count))
                     (setf (gethash sname module-item->id-hash) unique-id))
                   (list sname unique-id)
                   ))
               ;; hack ordering for palette display
               (if (help::alpha-listing? m)
                   (hack-ordering-for-palette-display (help::functions m))
                 (remove-undesired-module-functions (help::functions m)))
                 )
              (cadr 
               (assoc (help::name m) module-names-and-ids :test 'string-equal))
              )))
      (let ((templates (mapcar #'module-template toplevel-modules)))
        (labels 
            ((do-one-module (m template) 
               (let ((submodules 
                      (sort 
                       (mapcar 'symbol-name (help::submodules m))
                       'string-greaterp)))
                 (loop
                  for submodule-name in submodules
                  as submodule = 
                  (help::find-documentation submodule-name 'help::module)
                  do
                  (unless (module-already-in-tree? submodule-name templates)
                    (push (do-one-module submodule (module-template submodule))
                          (second template)
                          )))
                 template
                 )))
          
          ;; Call VPL-MODULE-TEMPLATE-IDS on every module.  
          ;; This inserts the appropriate function name and TID.
          ;; It also creates the list of all functions and TIDs.  
          ;; Then we sort and remove duplicates from this list of
          ;; all functions.  Then we break up the list into ordered blocks 
          ;; of 25 functions, each one labeled by the first function
          ;; in the block.  Finally create an ALL module which 
          ;; incorporates this alphabetical hierarchy, and
          ;; append it to the existing list of defined modules.  
          
          (let* ((*all-functions-alphabetically* nil)
                 (defined-modules 
                  (mapcar 
                   (lambda (module-spec) 
                     (cons (new-unique-id :menu-id) module-spec))
                   ;; This modifies *all-functions-alphabetically*
                   (vpl-module-template-ids
                    (mapcar #'do-one-module toplevel-modules templates)
                    ))))
            ;; The list is of the form ((f1 id1) (f2 id2) ...), not
            ;; just a list of functions as the name might suggest.
            (setq *all-functions-alphabetically* 
                  (sort 
                   (purge-duplicates 
                    *all-functions-alphabetically* 
                    :key 'first 
                    :test 'string-equal)
                   'string-lessp
                   :key 'first
                   ))
            (multiple-value-bind (alpha-labels alpha-sublistings)
                (break-listing-into-blocks *all-functions-alphabetically* 20)
              (cons 
               (create-palette-menu 
                (new-unique-id :menu-id)
                "ALL"
                (mapcar 
                 (lambda (label sublisting) 
                   `(,(s+ label "") nil ,sublisting))
                 alpha-labels alpha-sublistings
                 )
                nil
                :compile? nil)
               defined-modules
               ))))))))


;;; Calling KEY of an element of FUNCTIONS must return a string.

(defun break-listing-into-blocks 

       (functions block-size &key (key 'first) &aux (alpha-sublists nil))

  (labels ((first-char (entry) (char (funcall key entry) 0))
           (first-char-string (entry) (string-upcase (first-char entry))))

    ;; Split off the functions that begin with alphabetic characters
    ;; from the others.  We put the others in a single block.
    ;; If their number exceeds BLOCK-SIZE we die for now.

    (multiple-value-bind (alphabetics non-alphabetics)

        (separate-into-lists 
         functions (lambda (x) (alpha-char-p (first-char x))))

      ;; Separate the alphabetics into 26 lists, one for each letter.

      (setq
       alpha-sublists
       (remove
        nil
        (loop for ch across "ABCDEFGHIJKLMNOPQRSTUVWXYZ" collect
              (remove-if-not 
               (lambda (x) (char-equal ch (first-char x)))
               alphabetics
               ))))

      ;; Now start processing down the sublists.  For each sublist, there
      ;; are four possibilities:
      ;;   1. It is exactly BLOCK-SIZE long.  Cool.
      ;;   2. It is more than BLOCK-SIZE long.  If so we split it into
      ;;      an appropriate number of almost equally long sublists, using
      ;;      EQUAL-SIZED-SUBLISTS
      ;;   3. It has no elements. Ignore it.
      ;;   4. It is less than BLOCK-SIZE long.  We see if we can combine
      ;;      it with the next sublist; if not, it remains independent.
      ;;      If so, we combine it with the next one and reprocess it.

      ;; We also need to create labels for each block.  A single-letter
      ;; block will have a label which is that letter.  A multiple-letter
      ;; block will have a label like "D -> F".  A partial single-letter
      ;; block will have a label like "A -> Ap" or "Aq -> Az".  The
      ;; sublettering has to be long enough to uniquely identify the
      ;; last entry of the first subblock from the first entry of the next.
            
      (let ((block-sublists nil) 
            (block-labels nil)
            (block-types nil)
            (alpha-sublists-pointer alpha-sublists)
            (start-range-texts nil)
            (end-range-texts nil)
            (block-type nil))

        (loop until (null alpha-sublists-pointer) 
              as next-sublist = (first alpha-sublists-pointer)
              as len = (length next-sublist)
              do
              ;; (pprint next-sublist)
              ;; (print len)
              (cond
               ((zerop len) 
                (pop alpha-sublists-pointer)
                nil
                )
               ((= len block-size)
                ;; (print '=)
                (pop alpha-sublists-pointer)
                (push next-sublist block-sublists)
                (push :one-letter block-types)
                )
               ((> len block-size)
                ;; (print '>)
                (pop alpha-sublists-pointer)
                (let* ((n-sublocks (ceiling len block-size))
                       (sublocks 
                        (equal-sized-sublists next-sublist n-sublocks)))
                  (loop for sublock in sublocks do
                        (push sublock block-sublists)
                        (push :one-letter-subdivided block-types)
                        )))
               ((< len block-size)
                ;; (print '<)
                (setq block-type :one-letter)
                (tagbody 
                 again
                 (let* ((subsequent-sublist (second alpha-sublists-pointer))
                        (sslen (length subsequent-sublist))
                        (combined-len (+ sslen len)))
                   ;; (pprint (list 'ss sslen subsequent-sublist))
                   (cond 
                    ((or (null subsequent-sublist) (> combined-len block-size))
                     ;; (print :no-combine)
                     (pop alpha-sublists-pointer)
                     (push next-sublist block-sublists)
                     (push block-type block-types)
                     )
                    ((<= combined-len block-size)
                     ;; (print :combine)
                     ;; Combine the two lists into one and reprocess.
                     (setq alpha-sublists-pointer
                           (cons 
                            (append next-sublist subsequent-sublist) 
                            (cddr alpha-sublists-pointer)
                            ))
                     (setq next-sublist (first alpha-sublists-pointer))
                     (setq len (length next-sublist))
                     ;; (print :after-combine)
                     ;; (print next-sublist)
                     ;; (print len)
                     (setq block-type :two-or-more-letters-combined)
                     (go again)
                     )
                    (t (error "Impossible."))
                    ))))
               (t (error "Impossible"))
               ))

        (setq block-sublists (nreverse block-sublists))
        (setq block-labels (nreverse block-labels))
        (setq block-types (nreverse block-types))

        (flet ((different-letter-block? (b1 b2)
                 (not (string-equal
                       (first-char-string (first b1))
                       (first-char-string (first b2))))))

          (setq
           start-range-texts
           (loop for previous-head in (cons nil block-sublists)
                 for head in block-sublists
                 for type in block-types
                 collect
                 (ecase type
                   ((:one-letter :two-or-more-letters-combined)
                    (first-char-string (first head)))
                   (:one-letter-subdivided
                    ;; if this is the first block or the first block 
                    ;; of the set of :one-letter-divided blocks
                    (cond 
                     ((or (null previous-head) 
                          (different-letter-block? previous-head head))
                      (first-char-string (first head)))
                     (t                                 
                      (determine-start-label previous-head head key)
                      ))))))
                
          (setq
           end-range-texts
           (loop for head in block-sublists
                 for subsequent-head in (cdr (append block-sublists (list nil)))
                 for type in block-types
                 ;; do
                 ;; (print (list 'head head))
                 ;; (print (list 'sub subsequent-head))
                 ;; (print (list 'type type))
                 collect
                 (ecase type
                   (:one-letter "")
                   (:two-or-more-letters-combined
                    (s+ " -> " (first-char-string (lastelem head))))
                   (:one-letter-subdivided
                    (cond
                     ((null subsequent-head)  " -> Zz")
                     ((different-letter-block? head subsequent-head)
                      (s+ " -> " (first-char-string (first head)) "z"))
                     (t 
                      (s+ " -> " (determine-end-label head subsequent-head key))
                      ))))))

          (setq 
           block-labels
           (mapcar (lambda (x y) (s+ x y)) start-range-texts end-range-texts))

          (if non-alphabetics 
              (values
               (cons "+-* etc" block-labels)
               (cons non-alphabetics block-sublists)
               )
            (values block-labels block-sublists)
            )

          )))))

(defun determine-start-label (previous-block current-block key)
  (string-capitalize
   (let* ((s1 (funcall key (lastelem previous-block)))
          (s2 (funcall key (first current-block))))
     (multiple-value-bind (diffpos which)
         (first-difference s1 s2)
       (ecase which
         (:both (subseq s2 0 (1+ diffpos)))
         (:first 
          (vpl-internal-error "DETERMINE-START-LABEL impossible!"))
         (:second (subseq s2 0 (1+ diffpos)))
         )))))

(defun determine-end-label (current-block subsequent-block key)
  (string-capitalize
   (let* ((s1 (funcall key (lastelem current-block)))
          (s2 (funcall key (first subsequent-block))))
     (multiple-value-bind (diffpos which)
         (first-difference s1 s2)
       (ecase which
         (:both (subseq s1 0 (1+ diffpos)))
         (:first 
          (vpl-internal-error "DETERMINE-END-LABEL impossible!"))
         (:second s1)
         )))))

;;; destructively modifies MODULE-LIST internals
(defun vpl-module-template-ids (module-list)
  (loop for (nil submodules functions nil) in module-list do
        (vpl-module-template-ids submodules)
        (loop for function-info in functions 
              as fname = (first function-info)
              as bbl-symbol = (find-symbol fname :bbl)
              as tid = -1
              do
              ;; special case {} and []
              (case bbl-symbol 
                (bbl::[]
                 (setq tid (symbol->template-id 'utils::ref nil)))
                (bbl::[->] 
                 (setq tid (symbol->template-id 'bbl::range-ref nil)))
                (bbl::{} 
                 (setq tid (symbol->template-id 'bbi::%curly-list% nil)))
                (otherwise                  
                 (if (null bbl-symbol)
                     (progn
                       (warn "No such BBL symbol as ~A !!!" fname)
                       (setq fname (s+ "** XXX " fname)))
                   (progn
                     (setq tid (symbol->template-id bbl-symbol nil))
                     (when (null tid)
                       (warn "No template (id) for BBL symbol ~A !!" bbl-symbol)
                       (setq fname (s+ "** XID " fname))
                       (setq tid -1))))))
              (setf (first function-info) fname)
              (setf (second function-info) tid)
              (push (list fname tid) *all-functions-alphabetically*)
              ))
  module-list
  )
                                                    

;;; Random function    

(defun functions-in-bbl-but-not-in-modules ()
  (let ((functions-in-modules nil))
    (maphash 
     (lambda (key module) 
       (declare (ignore key))
       (when (member :bbl (help::display-modes module)) 
         (setq functions-in-modules 
               (union functions-in-modules (help::functions module)))))
     (gethash 'help::module help::*documentation*))
    (let* ((bbl-external-symbols 
            (loop for symbol being the external-symbols of (find-package :bbl)
                  collect symbol
                  ))
           (set-difference 
            (set-difference bbl-external-symbols functions-in-modules))
           (*print-length* nil))
      (formatt "Functions in modules to date: ~%" functions-in-modules)
      (pprint functions-in-modules)
      (loop for p in (list-all-packages) 
            as sublist = 
            (remove-if-not
             (lambda (x) (eq (symbol-package x) (find-package p)))
             set-difference)
            when sublist 
            do 
            (formatt "~%~%Package: ~A, number of symbols: ~D~%"
                     (package-name p) (length sublist))
            (pprint sublist)
            ))))

(defun hack-ordering-for-palette-display (module-functions)
  (let ((alphabetical-ordering 
         (remove-undesired-module-functions 
          (sort (copy-list module-functions) 'string-lessp :key 'string))))
    (multiple-value-bind (alphas non-alphas)
        (separate-into-lists 
         alphabetical-ordering
         (lambda (x) (alpha-char-p (char (string x) 0))))
      (append non-alphas alphas)
      )))
