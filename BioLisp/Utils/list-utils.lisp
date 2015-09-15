;;; -*- Package: utils; mode: lisp; base: 10; Syntax: Common-Lisp; -*-

(in-package :utils)

;;; +=========================================================================+
;;; | Copyright (c) 2002, 2003, 2004 JP Massar, Jeff Shrager, Mike Travers    |
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

;;; Author:  JP Massar

(eval-when (:compile-toplevel :load-toplevel :execute)

  (defparameter *utility-list-user-symbols*
    '(
      iota
      ilist
      nlist
      first-n
      exactly-one-of?
      insert-into-ordered-list
      ensure-list
      flatten
      delete-last-element
      remove-nth-element
      mapcarnn
      all-unordered-pairs
      all-ordered-pairs
      separate-into-lists
      categorize
      length-circular-or-dotted?
      all-contiguous-sublists
      equivalent-tree-structure? 
      maptree
      remove-and-push
      delete-and-push
      equal-sized-sublists
      unquote
      ))

  (defparameter *utility-list-api-symbols* 
    (append
     *utility-list-user-symbols*
     '(
       extract-list-elements
       )))

  (export *utility-list-api-symbols* (find-package :utils)))


(defun iota (n) 
  "Returns a list (0 1 ... N-1).  (IOTA 0) --> NIL"
  (declare (fixnum n))
  (loop for j fixnum from 0 below n collect j))


(defun ilist (min limit &optional (step 1))
  #.(one-string-nl
     "Returns a list of integers, by default (min min+1 ... LIMIT-1)"
     "If LIMIT < MIN (min, min-1 ... LIMIT+1)."
     "If STEP is provided the interval between elements is STEP instead of 1."
     "The sign of STEP is ignored, its absolute value is used."
     "Examples:" 
     "(ilist 5 20 3) --> (5 8 11 14 17)"
     "(ilist 5 -5 2) --> (5 3 1 -1 -3)")
  (declare (fixnum min limit step))
  (setq step (abs step))
  (cond
   ((and (= step 1) (<= min limit))
    (loop for j fixnum from min below limit collect j))
   ((<= min limit)
    (loop for j fixnum from min below limit by step collect j))
   ((= step 1)
    (loop for j fixnum from min downto (1+ limit) collect j))
   (t
    (loop for j fixnum from min downto (1+ limit) by step collect j))
   ))

(defun nlist (min limit &optional (step 1))
  #.(one-string-nl
     "Returns a list of numbers, by default (min min+1 ... LIMIT-1)"
     "If LIMIT < MIN (min, min-1 ... LIMIT+1)."
     "If STEP is provided the interval between elements is STEP instead of 1."
     "The sign of STEP is ignored, its absolute value is used."
     "Examples:" 
     "(nlist 5 20 3) --> (5 8 11 14 17)"
     "(nlist 5.5 -5.5 2) --> (5.5 3.5 1.5 -0.5 -2.5 -4.5)")
  (setq step (abs step))
  (if (and (integerp min) (integerp limit) (integerp step))
      (ilist min limit step)
    (cond
     ((and (= step 1) (<= min limit))
      (loop for j from min below limit collect j))
     ((<= min limit)
      (loop for j from min below limit by step collect j))
     ((= step 1)
      (loop for j from min downto (1+ limit) collect j))
     (t
      (loop for j from min downto (1+ limit) by step collect j))
     )))


;;; This is more efficient than using SUBSEQ as there are
;;; no indices to bounds-check.

(defun first-n (n list)
  #.(one-string-nl
     "Efficient way to extract the first N elements of a list."
     "Example: (first-n 3 '(a b c d e f g)) --> (a b c)"
     "If N is greater than the number of elements in LIST, then a copy"
     "of LIST is returned.")
  #.(optimization-declaration)
  (declare (fixnum n))
  (loop for k fixnum from 1 to n for item in list collect item))


(defun exactly-one-of? (&rest args) 
  #.(one-string-nl
     "Returns T if exactly one of ARGS is non-nil (returns NIL for no args)."
     "Examples:"
     "(exactly-one-of? t nil t) --> NIL"
     "(exactly-one-of? 5 nil nil nil) --> T")
  (= 1 (count-if 'identity args)))


(defun insert-into-ordered-list (elem test list &key (key #'identity))
  #.(one-string-nl
     "A destructively modified LIST with ELEM inserted is returned. "
     "It is assumed that LIST is already ordered wrt TEST and KEY.")
  (flet ((compare (list-element) 
           (if (eq key #'identity)
               (funcall test elem list-element)
             (funcall test (funcall key elem) (funcall key list-element)))))
    (block exit
      (cond
       ((null list) (list elem))
       ((null (cdr list))
        (if (compare (first list)) (cons elem list) (nconc list (list elem))))
       (t 
        ;; Goes at beginning
        (when (compare (first list)) (return-from exit (cons elem list)))
        (let* ((last-cons-cell nil))
          (do ((sublist list (cdr sublist))) ((null (cdr sublist)))
            (when (compare (second sublist))
              ;; Goes in middle: list surgery
              (let ((cdr-cell (cdr sublist))
                    (elem-cons-cell (list elem)))
                (setf (cdr sublist) elem-cons-cell)
                (setf (cdr elem-cons-cell) cdr-cell)
                (return-from exit list)))
            (setq last-cons-cell (cdr sublist)))
          ;; Must go at the end
          (setf (cdr last-cons-cell) (list elem))
          list
          ))))))


(defun ensure-list (thing)
  #.(one-string-nl
     "If the argument thing is a list already, it is simply returned"
     "otherwise this returns (list thing).  This is often useful when a"
     "slot value is either a singleton or a list, but you don't know which."
     )
  (if (listp thing) thing (list thing)))


(defun flatten (obj)
  #.(one-string-nl
     "Remove all the nested structure in a list.  For example: "
     "(flatten '(((a) (((b)))))) => (a b)"
     "Dotted pairs get transformed into standard lists:"
     "(flatten '(a . b)) => (a b)")
  #.(optimization-declaration)
  (cond
   ((null obj) nil)
   ((listp obj) 
    (loop for list = obj then list
          as elem = (first list)
          as rest = (rest list)
          until (null list) 
          nconc
          (if (listp rest)
              (progn
                (pop list)
                (cond
                 ((null elem) elem)
                 ((atom elem) (list elem))
                 (t (flatten elem))
                 ))
            (progn
              (setq list nil)
              (nconc 
               (cond
                ((null elem) elem)
                ((atom elem) (list elem))
                (t (flatten elem))
                )
               (list rest)
               )))))
   (t (list obj))
   ))
          


(defun delete-last-element (list)
  #.(one-string-nl
     "Delete (by destructively modification) the last element of a list."
     "Returns nil if LIST is NIL or a single-element list, otherwise LIST"
     "is destructively modified and returned.")
  (cond
   ((null list) list)
   ((null (cdr list)) nil)
   (t (loop for sublist on list 
            until (null (cddr sublist))
            finally (return (progn (setf (cdr sublist) nil) list))
            ))))

(DEFUN Remove-nth-element (n list &KEY (plus 0))
  "Nondestructively removes nth element of a list, plus (optionally) contiguous elements"
  (DECLARE (TYPE (Integer 0) n)
           (TYPE List list))
  (IF (OR (ZEROP n) (NULL list))
    (LOOP WITH fragment = (CDR list)
          FOR i FROM 1 TO plus
          DO (POP fragment)
          FINALLY (RETURN fragment))
    (CONS (CAR list) 
          (FORWARD-FUNCALL 'Remove-nth-element (1- n) (CDR list) :PLUS plus))))


(defun mapcarnn (f &rest args)
  "Map F over ARGS returning only non-nil results as a list"
  #.(optimization-declaration)
  (let ((nargs (length args)))
    (declare (fixnum nargs))
    (cond
     ((= 0 nargs) nil)
     ((= 1 nargs)
      (loop for arg in (first args) 
            as result = (funcall f arg)
            when result collect result
            ))
     ((= 2 nargs)
      (loop for arg1 in (first args)
            for arg2 in (second args)
            as result = (funcall f arg1 arg2)
            when result collect result
            ))
     ((= 3 nargs)
      (loop for arg1 in (first args)
            for arg2 in (second args)
            for arg3 in (third args)
            as result = (funcall f arg1 arg2 arg3)
            when result collect result
            ))
     (t
      (let ((result nil)
            (list (make-list nargs))
            (argv (coerce args 'simple-vector)))
        (block exit
          (loop
           (loop for arg across argv
                 for j fixnum from 0 
                 for arglist on list do
                 (when (null arg) (return-from exit nil))
                 (setf (car arglist) (car arg))
                 (pop (aref argv j))
                 )
           (let ((datum (apply f list)))
             (when datum (push datum result)))
           ))
        (setq result (nreverse result))
        result
        )))))


(defun all-unordered-pairs (set)
  #.(one-string-nl
     "The elements of SET are assumed to be unique."
     "Returns a list of all pairs of elements in SET, such that if A and B"
     "are elements of SET and A occurs before B, then the pair (A B) is"
     "included in the returned list while (B A) is not."
     "(all-unordered-pairs '(a s d f)) => "
     "((a s) (a d) (a f) (s d) (s f) (d f))"
     "If SET is NIL or contains a single element, NIL is returned.")
  (when (cdr set)
    (let ((pair-element (first set)) (subset (rest set)))
      (nconc 
       (mapcar (lambda (elem) (list pair-element elem)) subset)
       (all-unordered-pairs subset)
       ))))


(defun all-ordered-pairs (set)
  #.(one-string-nl
     "Returns the crossproduct of SET, whose elements are assumed unique."
     "(all-ordered-pairs '(a s d)) => ((a s) (a d) (s d) (s a) (d a) (d s))"
     "If SET is NIL or contains a single element, NIL is returned.")
  (let ((temp (all-unordered-pairs set)))
    (nconc temp (mapcar 'reverse temp))
    ))


(defun separate-into-lists (list &rest predicates)
  #.(one-string-nl
     "In the typical case, with a single PREDICATE, divides"
     "LIST into two sublists, the first containing those elements of LIST"
     "that satisfy the function and the second the other elements.  The"
     "two lists are returned as two values."
     "In the general case, with more than a single PREDICATE,"
     "divides LIST into N sublists, where N is one more than the number of"
     "functions provided. N values are returned.  The items in LIST are"
     "tested against each PREDICATE in the order the PREDICATES occur."
     "If no predicate is satisfied the item is added to the 'other' list"
     "which is the last value returned."
     "The lists returned are order-preserving, the sense that if element A"
     "and element B both satisfy PREDICATE P, and A occurs before B in LIST,"
     "then element A will occur before element B in the returned list"
     "associated with PREDICATE.")
  (cond
   ((null predicates) (copy-list list))
   ;; A single discriminator.  Optimize the standard case.
   ((null (cdr predicates))
    (loop for item in list
          with f = (first predicates)
          if (funcall f item) collect item into a else collect item into b
          finally (return (values a b))
          ))
   (t
    (let* ((n (length predicates))
           (discriminated-lists (make-array (1+ n) :initial-element nil)))
      (loop for item in list do
            (loop for f in predicates
                  for j fixnum from 0 
                  when (funcall f item) do
                  (push item (aref discriminated-lists j))
                  (return)
                  finally (push item (aref discriminated-lists n))
                  ))
      (apply
       'values
       (map 'list 'nreverse discriminated-lists)
       )))))

(defun categorize 
       (objects function 
                &key (count? nil) (FAST-BUT-REDUNDANT? t) (hash-test 'equal))
  #.(one-string-nl
     "Given a list of objects, and a function that applies to those objects"
     "and returns a single category or list of categories, re-organize the"
     "objects by the categories. This uses EQUAL hash tables, so the function"
     "must return something that can be EQUAL tested."
     "Example: (categorize '(1 2 3 4 5) #'evenp) => ((NIL 5 3 1) (T 4 2))"
     "[The order of the result categorize and objects in each is"
     "indeterminate.]"
     "More complex example:"
     "(categorize"
     "  '(\"Jeff\" \"Joe\" \"Sam\" \"Sall\")"
     "  #'(lambda (object) (list (elt object 0) (length object))))"
     "  --> ((3 \"Sam\" \"Joe\") (4 \"Sall\" \"Jeff\")"
     "      (#\J \"Joe\" \"Jeff\") (#\S \"Sall\" \"Sam\"))"
     "Note that we get BOTH categories in this case by first letter"
     "AND by length.  By default this uses PUSH to add object to the"
     "category, which is fast, but doesn't protect against redundant entries."
     "If you want to protect for this, add the keyword"
     ":FAST-BUT-REDUNDANT? NIL (default is T)"
     "By default you get all the entries back, but sometimes you just"
     "want a count. Adding the keyword :COUNT? t (default = nil) will"
     "spare you the pain of the whole list!"
     )
  (let ((table (make-hash-table :test hash-test)))
    (loop for object in objects
	  as cats = (let ((cats (funcall function object)))
		      (if (and (listp cats) (not (null cats)))
			  cats (list cats)))
	  do (loop for cat in cats
		   do (if FAST-BUT-REDUNDANT? 
			  (push object (gethash cat table))
			(pushnew object (gethash cat table) :test hash-test))))
    (lmaphash 
     (lambda (k v) (if count? (list k (length v)) (cons k v)))
     table
     )))
                  
            
(defun length-circular-or-dotted? (list)
  #.(one-string-nl
     "Returns length of LIST or NIL if the list is circular, as first value."
     "Returns one of :proper, :dotted or :circular as second value."
     "If list is dotted the CDR of the last cons is counted as an element.")
  (handler-case
      (let ((len (list-length list)))
        (if (integerp len) (values len :proper) (values nil :circular)))
    (error 
     () 
     (values
      (loop for dlist on list for j fixnum from 1 do
            (when (not (listp (cdr dlist))) (return (1+ j))))
      :dotted)
     )))


(defun all-contiguous-sublists (list sublist-size &optional (interval 1))
  #.(one-string-nl
     "Returns a list of lists. Each sublist is a subsequence of LIST,"
     "consisting of SUBLIST-SIZE contiguous elements of LIST."
     "Interval specifies how many elements in LIST to skip before creating"
     "the next sublist. Returns NIL if SUBLIST-SIZE is bigger than the length"
     "of LIST.")
  (declare (fixnum sublist-size interval))
  #.(optimization-declaration)
  (let ((cdring-function (lambda (x) (nthcdr interval x)))
        (end-predicate 
         (lambda (x) (null (nthcdr (the fixnum (1- sublist-size)) x)))))
    (loop for remaining-elements on list by cdring-function
          until (funcall end-predicate remaining-elements)
          collect (first-n sublist-size remaining-elements)
          )))


(defun equivalent-tree-structure? (&rest trees)
  #.(one-string-nl
     "Determines if all the lists composing TREES have identical"
     "list structure.  That is, where one tree has a leaf the other"
     "trees also have leaves.  (The car of a (sub)tree is a leaf if"
     "it contains an atom, and the cdr of a (sub)tree is a leaf"
     "if it contains a non-NIL atom.)"
     "Examples:"
     "(equivalent-tree-structure? '(a b (c d)) '(d e (f g))) --> T"
     "(equivalent-tree-structure? '((a1 a2) b (c . d)) '((b2) e (f . g)))"
     "  --> NIL)")
  (cond
   ((null trees) t)
   ((null (cdr trees)) t)
   ((null (cddr trees)) (equiv-two-trees? (first trees) (second trees)))
   (t 
    (and (equiv-two-trees? (first trees) (second trees))
         (apply 'equivalent-tree-structure? (cdr trees))))
   ))
          
(defun equiv-two-trees? (t1 t2)
  (cond
   ((and (null t1) (null t2)) t)
   ((or (null t1) (null t2)) nil)
   ((atom t1) 
    (error "The object ~S is not a tree (a tree must be a list)" t1))
   ((atom t2)
    (error "The object ~S is not a tree (a tree must be a list)" t2))
   (t
    (and (equiv-car-two-trees? (car t1) (car t2))
         (equiv-cdr-two-trees? (cdr t1) (cdr t2))))))
         
(defun equiv-car-two-trees? (t1 t2)
  (or (and (atom t1) (atom t2))
      (and (consp t1) (consp t2) 
           (equiv-car-two-trees? (car t1) (car t2))
           (equiv-cdr-two-trees? (cdr t1) (cdr t2))
           )))
      
      
(defun equiv-cdr-two-trees? (t1 t2)
  (or (and (null t1) (null t2))
      (and (not (listp t1)) (not (listp t2)))
      (and (consp t1) (consp t2) 
           (equiv-car-two-trees? (car t1) (car t2))
           (equiv-cdr-two-trees? (cdr t1) (cdr t2))
           )))



(defvar *maptree-toplevel?* nil)

(defun maptree (f arg &rest args) 
  #.(one-string-nl
     "Maps F over ARG and ARGS in the sense that the successive leaves"
     "of each tree are used as arguments to calls to F.  The tree"
     "arguments are traversed in depth-first order."
     "Example:"
     "(maptree '+ '(1 (2 3)) '(3 (4 5)) '(5 (6 6))) --> (9 (12 14))"
     "If any of the trees' structures are not equivalent to the"
     "other trees' structures, then an error is signalled."
     "If X is a tree, then (car x) is a leaf if it is an atom,"
     "and (cdr x) is a leaf if it is a non-nil atom.")
  (let ((*maptree-toplevel?* t))
    (cond
     ((null args) (maptree-1arg f arg))
     ((null (cdr args)) (maptree-2args f arg (first args)))
     (t
      (maptree-gen f (cons arg args))))))

(defun maptree-1arg (f arg)
  (cond
   ((null arg) nil)
   ((atom arg) 
    (error 
     (one-string-nl
      "The object ~S is not a tree!  You must provide a list"
      "to MAPTREE, not a ~S.")
     arg (type-of arg)))
   (t 
    (mapcar (lambda (x) (maptree-car-1arg f x)) arg)
    )))

(defun maptree-car-1arg (f x)
  (cond
   ((atom x) (funcall f x))
   (t (maptree-1arg f x))
   ))

(defun maptree-2args (f t1 t2)
  (flet ((oops-null (p1 p2 tree)
           (if *maptree-toplevel?* 
               (error 
                (one-string-nl
                 "The two trees are not equivalent!"
                 "The ~:R is NIL while the ~:R is not.")
                p1 p2)
             (error
              (one-string-nl
               "The two trees are not equivalent!"
               "The ~:R has a branch that terminates while the ~:R"
               "has a similar branch that contains ~S.")
              p1 p2 tree)))
         (oops-atom (p1 p2 t1 t2)
           (if *maptree-toplevel?*
               (error 
                (one-string-nl
                 "The two tree arguments are not equivalent!"
                 "The ~:R argument, ~S, is not a tree!")
                p1 t1)
             (error 
              (one-string-nl
               "The two trees are not equivalent!"
               "The ~:R has a leaf, ~S,"
               "while the ~:R has a non-leaf, ~S"
               "in the equivalent position.")
              p1 t1 p2 t2)
             ))
         (oops-not-same-length (len1 len2)
           (if *maptree-toplevel?* 
               (error 
                (one-string-nl
                 "The two tree arguments are not equivalent!"
                 "The first is a list of length ~D, but the second has"
                 "length ~D.")
                len1 len2)
             (error 
              (one-string-nl
               "The two trees are not equivalent!"
               "The first has a sublist of length ~D,"
               "while the second has a sublist in the same position"
               "of length ~D.")
              len1 len2)
             )))
    (cond
     ((and (null t1) (null t2)) nil)
     ((null t1) (oops-null 1 2 t2))
     ((null t2) (oops-null 2 1 t1))
     ((and (listp t1) (listp t2))
      (let ((len1 (length t1)) (len2 (length t2)))
        (when (/= len1 len2) (oops-not-same-length len1 len2)))
      (let ((*maptree-toplevel?* nil))
        (mapcar (lambda (x y) (maptree-car-2args f x y)) t1 t2)
        ))
     ((atom t1) (oops-atom 1 2 t1 t2))
     ((atom t2) (oops-atom 2 1 t2 t1))
     (t 
      (error "This should be impossible!")
      ))))

(defun maptree-car-2args (f t1 t2)
  (cond
   ((and (atom t1) (atom t2)) (funcall f t1 t2))
   (t
    (maptree-2args f t1 t2))))

(defun maptree-gen (f trees)
  (cond
   ((every 'null trees) nil)
   ((some 'null trees) 
    (if *maptree-toplevel?* 
        (error
         (one-string-nl
          "One of the trees is not equivalent to the others!"
          "At least one of the trees is NIL while some others are not."))
      (error
       (one-string-nl
        "One of the trees is not equivalent to the others!"
        "At least one of the trees has a branch that terminates"
        "while some of the other trees continue."
        "(The ~:R tree argument provided terminates, whereas,"
        "for instance, the ~:R tree argument continues with value"
        "~S.")
       (1+ (position-if 'null trees)) 
       (1+ (position-if 'identity trees))
       (elt trees (position-if 'identity trees)))))
   ((every 'atom trees) (apply f trees))
   ((some 'atom trees) 
    (if *maptree-toplevel?* 
        (error
         (one-string-nl
          "Some of the tree arguments are not in fact trees!"
          "(Trees are lists; you must provide trees to MAPTREE.)"
          "These tree arguments are invalid: ~S")
         (remove-if-not 'atom trees))
      (error
       (one-string-nl
        "One of the trees is not equivalent to some of the others!"
        "At least one of the trees has a leaf ~S at a particular"
        "location while some of the other trees have a non-leaf"
        "at that location.")
       (elt trees (position-if 'atom trees)))))
   (t 
    (let ((*maptree-toplevel?* nil))
      (cons (maptree-car-gen f (mapcar 'car trees))
            (maptree-gen f (mapcar 'cdr trees))
            )))))

(defun maptree-car-gen (f trees)
  (cond
   ((every 'atom trees) (apply f trees))
   (t
    (maptree-gen f trees))))


(defun remove-and-push (item list &key (test 'eql))
  #.(one-string-nl
     "Removes ITEM from LIST if it is present, then pushes ITEM"
     "onto the beginning of the list; ITEM therefore always becomes"
     "the first element of the returned list.")
  (cons item (remove item list :test test)))

(defmacro delete-and-push (item place &key test)
  #.(one-string-nl
     "Deletes ITEM from PLACE (a list) if it is present, then pushes ITEM"
     "onto the beginning of that list; ITEM therefore always becomes"
     "the first element of the returned list, and the returned list"
     "is stored back into PLACE.")
  (let ((item-symbol (gensym "ITEM-"))
        (list-symbol (gensym "DEPLETED-LIST-")))
    `(let* ((,item-symbol ,item)
            (,list-symbol 
             (delete ,item-symbol ,place ,@(when test `(:test ,test)))))
       (setf ,place (push ,item-symbol ,list-symbol))
       )))

(defun extract-list-elements (list indices &optional (base 0))
  #.(one-string-nl
     "Extracts elements from LIST at INDICES (which must be a list"
     "of valid fixnum indices with respect to BASE)."
     "The extraction is done in linear time (the naive algorithm would"
     "take time proportional to the square of the length of LIST)." 
     "The exact algorithm depends on whether INDICES is ordered; if"
     "the indices are ordered the extraction is faster than if not.")
  (declare (fixnum base))
  (cond
   ((and (null list) (null indices)) nil)
   ((null indices) nil)
   ((null list) (error "Trying to extract elements from an empty list!"))
   (t  
    (if (ordered? indices)
        (let ((rest-of-list list)
              (current-head-index base))
          (declare (fixnum current-head-index))
          (loop 
           for ir fixnum in indices 
           collect
           (let ((delta (the fixnum (- ir current-head-index))))
             (declare (fixnum delta))
             (setf rest-of-list (nthcdr delta rest-of-list))
             (when (null rest-of-list)
               (error
                (one-string-nl
                 "The index ~D exceeds the index of the last element"
                 "of the input list, ~D ! You cannot index"
                 "beyond the end of a list.")
                ir (+ base (1- (length list)))
                ))
             (setq current-head-index (the fixnum (+ current-head-index delta)))
             (first rest-of-list)
             )))
      (let* ((indices-hash (make-hash-table :test 'eql))
             (len (length list))
             (max-index (+ base (1- len))))
        (declare (fixnum len max-index))
        (loop for i fixnum in indices 
              do 
              (when (or (< i base) (> i max-index))
                (error "Invalid index, ~D, for list of length ~D." i len))
              (setf (gethash i indices-hash) t))
        (loop for i fixnum from base
              for elem in list 
              do 
              (when (gethash i indices-hash) 
                (setf (gethash i indices-hash) elem)))
        (loop for i in indices collect (gethash i indices-hash))
        )))))

(defun equal-sized-sublists (list n)
  #.(one-string-nl
     "Separates LIST into N sublists.  Each sublist"
     "is approximately the same size; if the length of LIST"
     "is not an exact multiple of N, then the initial"
     "sublists are one element longer than the trailing sublists."
     "Returned is a list of lists.   Each list contains elements of"
     "the original list in strict order -- that is, if FLATTEN were called on"
     "the return value, the original list would be recreated.")
  (let ((len (length list)))
    (cond
     ((null list) nil)
     ((= n 1) (list list))
     ((> n len) 
      (error "You want ~D sublists but there are only ~D elements!" n len)) 
     (t 
      (multiple-value-bind (block-size r)
          (floor len n)
        (let ((sizes (make-list n :initial-element block-size)))
          ;; add 1 to initial sizes to make total be length of list
          (loop for j from 1 to r 
                for s on sizes 
                do 
                (incf (first s)))
          (unless (= (reduce '+ sizes) len)
            (error "Didn't work!"))
          (loop for remaining-sizes on sizes 
                as size = (first remaining-sizes)
                as more? = (cdr remaining-sizes)
                as limit = (if more? size nil)
                collect (subseq list 0 limit)
                do (when more? (setq list (nthcdr limit list)))
                )))))))

(defun unquote (x)
  "Removes the quote from a quoted list"
  (if (and (listp x) (eq (first x) 'quote)) (second x) x))

