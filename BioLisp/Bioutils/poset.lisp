;;; -*- mode: Lisp; Syntax: Common-Lisp; Package: bioutils; -*-

(in-package :bioutils)

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

;;; Author: JP Massar.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *poset-api-symbols*
    '(
      ancestor? 
      comparable?
      poset-height
      filter
      ancestors
      ideal
      descendants
      hourglass
      poset-node-generator
      next-poset-level
      poset-node-level-generator
      maximal
      minimal
      chain-set
      chain-links
      siblings
      cousins
      leaves-of-nodes
      forest-of-ancestors
      ))
  (export *poset-api-symbols* (find-package :bioutils))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; http://www.c3.lanl.gov/%7Ejoslyn/papers.html


;;; A poset is defined by a set of nodes and a relationship.
;;; We will denote the relationship by <=, and the set of nodes with P.

;;; The relationship must be a reflexive, anti-symmetric, 
;;; transitive binary function.

;;; While the relationship defines a predicate between any two nodes, for large
;;; sets of nodes (like the GO, which has some 20000 nodes) storing this
;;; in precomputed form would be prohibitively expensive in terms of space.  
;;; Therefore we compute the relationship in terms of two more primitive
;;; relationships called PARENTS and CHILDREN, having the obvious semantics.  
;;; A PARENT-FUNCTION returns a list of nodes which are the direct parents of
;;; a node and similarly a CHILD-FUNCTION returns a list of nodes which are 
;;; the direct children.  

;;; Using the PARENT relationship we can create an ANCESTOR? primitive
;;; on a given POSET.  

;;; In a poset, two nodes p, q  
;;; are comparable, denoted p ~ q, if either p <= q or q <= p;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ancestor? 
       (possible-ancestor
        node 
        parent-function
        &key (test 'eq) (if= nil))
  (labels ((ancestor-aux (node)
             (let ((parents (funcall parent-function node)))
               (cond
                ((null parents) nil)
                ((member possible-ancestor parents :test test) t)
                (t 
                 (some (lambda (n) (ancestor-aux n)) parents)
                 )))))
    (cond
     ((eq possible-ancestor node) if=)
     (t (ancestor-aux node))
     )))

(defun comparable? (p q parent-function)
  (or (ancestor? p q parent-function) (ancestor? q p parent-function)))

;;;  A chain, C, is a collection of comparable nodes; and the height H(P) is
;;;  the size of the largest chain.

(defun poset-height (root child-function)
  (let ((height 1))
    (labels ((ph (n h)
               (setq height (max height h))
               (loop with nexth = (1+ h) 
                     for child in (funcall child-function n)
                     do 
                     (ph child nexth)
                     )))
      (ph root height)
      height
      )))

;;; The filter is the set of nodes which are ancestors 
;;; (i.e., transitively parents)
;;; of a given node, inclusive of the node itself.  

(defun filter (node parent-function)
  (labels ((ideal-aux (p) 
             (cons 
              p 
              (let ((parents (funcall parent-function p)))
                (when parents 
                  (loop for p in parents nconc (ideal-aux p))
                  )))))
    (purge-duplicates (ideal-aux node))
    ))

(defun ancestors (node parent-function)
 (cdr (filter node parent-function)))

;;; The ideal is the set of nodes which are descendants (i.e., transitively
;;; children) of a given node, inclusive of the node itself.  

(defun ideal (node children-function)
  (labels ((filter-aux (p) 
             (cons 
              p 
              (let ((children (funcall children-function p)))
                (when children
                  (loop for c in children nconc (filter-aux c))
                  )))))
    (purge-duplicates (filter-aux node))
    ))

(defun descendants (node children-function)
  (cdr (ideal node children-function)))

;;; The hourglass is the union of the ideal and the filter for a given node.
;;; That is, all the nodes both 'above' and 'below' a given node, including the 
;;; node itself.  

(defun hourglass (node parent-function children-function)
  (cons node (nconc (delete node (copy-list (ideal node parent-function)))
                    (delete node (copy-list (filter node children-function)))
                    )))


(defun poset-node-generator (node generator-function terminator identity?)
  #.(one-string-nl 
     "Returns a generator function of one optional argument which, when called,"
     "returns the next of a set of nodes.  These nodes are the transitive"
     "closure of GENERATOR-FUNCTION applied to NODE."
     "When the set is exhausted, the function returns TERMINATOR."
     "If IDENTITY? is non-nil, then the set includes NODE, otherwise not."
     "Currently a node may be returned more than once if, for example,"
     "node A is a parent of nodes B and C which are both parents of node D;"
     "node A will be returned twice.")
  (let ((unprocessed-nodes 
         (if identity? (list node) (funcall generator-function node)))
        (node-ht (make-hash-table :test 'eq)))
    (lambda (&optional (test (lambda (x) (declare (ignore x)) t)))
      (labels ((next () 
                 (cond 
                  ((null unprocessed-nodes) terminator)
                  (t 
                   (let ((next-node (pop unprocessed-nodes)))
                     (if (and (funcall test node) 
                              (null (gethash next-node node-ht)))
                         (progn 
                           (setq unprocessed-nodes 
                                 (append (funcall generator-function next-node)
                                         unprocessed-nodes
                                         ))
                           (setf (gethash next-node node-ht) t)
                           next-node 
                           )
                       (next)))))))
        (next)
        ))))



(defun next-poset-level (nodes generator-function)
  (purge-duplicates
   (mapcan
    (lambda (x) (copy-list (funcall generator-function x)))
    nodes)))
  

(defun poset-node-level-generator 
       (nodes generator-function terminator identity?)
  #.(one-string-nl 
     "Returns a generator function of one optional argument which, when called,"
     "returns the next of a set of nodes.  These nodes are the transitive"
     "closure of GENERATOR-FUNCTION applied to NODE."
     "When the set is exhausted, the function returns TERMINATOR."
     "If IDENTITY? is non-nil, then the set includes NODE, otherwise not."
     "Currently a node may be returned more than once if, for example,"
     "node A is a parent of nodes B and C which are both parents of node D;"
     "node A will be returned twice.")
  (setq nodes (ensure-list nodes))
  (let ((unprocessed-nodes 
         (if identity? 
             nodes 
           (next-poset-level nodes generator-function)))
        (node-depth 0)
        (node-ht (make-hash-table :test 'eq)))
    (lambda (&optional (test (lambda (x) (declare (ignore x)) t)))
      (incf node-depth)
      (multiple-value-bind (old-nodes new-nodes)
          (separate-into-lists 
           unprocessed-nodes
           (lambda (x) (gethash x node-ht)))
        (if (and (null old-nodes) (null new-nodes))
            terminator 
          (progn 
            (dolist (node new-nodes) (setf (gethash node node-ht) t))
            (setq unprocessed-nodes 
                  (remove-if-not
                   test 
                   (next-poset-level unprocessed-nodes generator-function)))
            (values node-depth new-nodes old-nodes) 
            ))))))
            
                   

(defun maximal (node-set parent-function test)
  #.(one-string-nl 
     "Given a set of nodes (NODE-SET), the maximal function returns a subset"
     "of these nodes such that no node in this returned subset is a descendant"
     "of any other node in the original set. TEST is a function used to compare"
     "nodes for equality and must be suitable for use with MAKE-HASH-TABLE.")
  (let ((node-set-ht (create-hash-table node-set :test test :mode :singleton))
        (non-node-set-ht (make-hash-table :test test)))
    (loop for node in node-set 
          when 
          (let ((generator (poset-node-generator 
                            node parent-function nil nil)) 
                (continue-function 
                 (lambda (n) (not (gethash n non-node-set-ht)))))
            (loop as ancestor = (funcall generator continue-function)
                  until (null ancestor)
                  do 
                  (when (gethash ancestor node-set-ht) (return))
                  (setf (gethash ancestor non-node-set-ht) t)
                  finally (return t)
                  ))
          collect node 
          )))


(defun minimal (node-set children-function test)
  #.(one-string-nl 
     "Given a set of nodes (NODE-SET), the minimal function returns a subset"
     "of these nodes such that no node in this returned subset is an ancestor"
     "of any other node in the original set. TEST is a function used to compare"
     "nodes for equality and must be suitable for use with MAKE-HASH-TABLE.")
  (let ((node-set-ht (create-hash-table node-set :test test :mode :singleton))
        (non-node-set-ht (make-hash-table :test test)))
    (loop for node in node-set 
          when 
          (let ((generator (poset-node-generator 
                            node children-function nil nil)) 
                (continue-function 
                 (lambda (n) (not (gethash n non-node-set-ht)))))
            (loop as descendant = (funcall generator continue-function)
                  until (null descendant)
                  do 
                  (when (gethash descendant node-set-ht) (return))
                  (setf (gethash descendant non-node-set-ht) t)
                  finally (return t)
                  ))
          collect node 
          )))



(defun chain-set (parent child parent-function)
  #.(one-string-nl 
     "Returns the set of all nodes N such that N is an ancestor of CHILD and N"
     "is a descendant of PARENT.  PARENT and CHILD are included in this set.")
  (if (eq parent child)
      (list parent)
    (purge-duplicates 
     (append 
      (list parent child)
      (loop with parent-list = (next-poset-level (list child) parent-function)
            until (null parent-list)
            append 
            (prog1
                (setq parent-list 
                      (remove-if-not
                       (lambda (x) 
                         (ancestor? parent x parent-function))
                       parent-list))
              (setq parent-list (next-poset-level parent-list parent-function))
              ))))))
                                                            

(defun chain-links (parent child parent-function)
  (if (eq parent child)
      (list (list parent))
    (let* ((child-parents (funcall parent-function child))
           (candidates 
            (remove-if-not 
             (lambda (x) 
               (or (eq x parent) 
                   (ancestor? parent x parent-function)))
             child-parents
             )))
      (mapcar 
       (lambda (chain) (cons child chain))
       (mapcan 
        (lambda (candidate) (chain-links parent candidate parent-function))
        candidates
        )))))



(defun get-next-level (list next-level-function)
  (remove-duplicates (loop for x in list append 
                           (funcall next-level-function x))))

(defun siblings (node parent-function child-function)
  (let ((node-parents (funcall parent-function node)))
    (remove node (get-next-level node-parents child-function))))


(defun cousins (node parent-function child-function)
  (let* ((node-parents (funcall parent-function node))
        (grandparents (get-next-level node-parents parent-function))
        (parent-level (get-next-level grandparents child-function)))
    (set-difference (remove node (get-next-level parent-level child-function))
                    (siblings node parent-function child-function))))
            
                               
(defun leaves-of-nodes (node-set children-function)
  #.(one-string-nl
     "Walks down the poset starting at the nodes in NODE-SET, using the "
     "CHILDREN-FUNCTION, and returns"
     "the set of nodes that have no children under the given CHILDREN-FUNCTION")
  (labels ((doit (node-set children-function)
             (multiple-value-bind (leaves non-leaves)
                 (separate-into-lists 
                  node-set #'(lambda (x) (not (funcall children-function x))))
               (append leaves 
                       (when non-leaves 
                         (doit (get-next-level non-leaves children-function) 
                               children-function))))))
    (purge-duplicates (doit node-set children-function))))



;;; A forest is just a list of trees (see below).

(defun forest-of-ancestors (frame parents-function children-function)
  #.(one-string-nl
     "Returns a list of trees."
     "(A tree here is defined as a list whose first element is a node"
     "and whose other elements are trees.  A leaf is therefore a one-element list."
     "To get the children nodes of a tree mapcar 'FIRST over the CDR of a tree.)"
     "One tree is returned for every root node which is an ancestor of FRAME."
     "A returned tree contains all the paths from the root to FRAME.")
  (let* ((forest-elements (filter frame parents-function))
         (roots 
          (remove-if (lambda (x) (funcall parents-function x)) forest-elements)))
    (loop for root in roots collect 
          (tree-from-ancestor-to-node 
           forest-elements root frame children-function)
          )))

;;; A tree here is defined as a list whose first element is a node
;;; and whose other elements are trees.  A leaf is therefore a one-element list.
;;; To get the children nodes of a tree mapcar 'FIRST over the CDR of a tree.

(defun tree-from-ancestor-to-node (tree-elements root node cf)
  (let ((tree-elements-hash (make-hash-table :test 'eq)))
    (dolist (e tree-elements) (setf (gethash e tree-elements-hash) t))
    (labels ((tree-from-ancestor (a)
               (if (eq a node)
                   (list node)
                 (cons 
                  a
                  (loop for child in (funcall cf a) 
                        when (gethash child tree-elements-hash)
                        collect (tree-from-ancestor child)
                        )))))
      (tree-from-ancestor root)
      )))
          
                


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; http://en.wikipedia.org/wiki/Closeness_(graph_theory)

(defun closeness (node parent-function children-function)
  #.(one-string-nl
     "The closeness for a vertex NODE is the reciprocal of the sum of geodesic"
     "distances to all other vertices in the graph.")
  ;; Proceed from NODE to both the parents and children
  ;; marking these nodes as visited and summing the distance. Once no parent or
  ;; child nodes are not visited the algorithm terminates.
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (let ((distance-sum 0)
        (current-distance 1)
        (current-node-set (list node))
        (visited-nodes (make-hash-table :test 'eq))
        )
    (declare (fixnum distance-sum current-distance))
    (setf (gethash node visited-nodes) t)
    ;; Function to obtain all the unique nodes connected to NODE-SET
    ;; which have not already been visited.
    (flet ((unvisited-directly-connected-nodes (node-set)
             (remove-duplicates
              (remove-if
               (lambda (node) (gethash node visited-nodes))
               (loop for node in node-set nconc
                     (nconc
                      (copy-list (funcall parent-function node))
                      (copy-list (funcall children-function node))
                      ))))))
      ;; Each time through the loop the distance from the the original
      ;;  node to where we are now increases by 1.
      (loop as next-node-set = 
            (unvisited-directly-connected-nodes current-node-set)
            until (null next-node-set)
            for iter fixnum from 0
            do
            (incf distance-sum (* current-distance (length next-node-set)))
            (incf current-distance)
            (loop for node in next-node-set do
                  (setf (gethash node visited-nodes) t))
            (setq current-node-set next-node-set)
            )
      (float (/ 1.0 distance-sum))
      )))


;;; Example data
#|
(progn
(defparameter *cts* ; (closeness *cts*)
   (list (def-frame #$a #$ch '(#$b #$c))
         (def-frame #$b #$ch '(#$d #$e) #$pa #$a)
         (def-frame #$c #$ch '(#$f #$g) #$pa #$a)
         (def-frame #$d #$pa #$b)
         (def-frame #$e #$pa #$b)
         (def-frame #$f #$pa #$c)
         (def-frame #$g #$pa #$c)
         ))

(defun pf (f) (ensure-list (#^pa f)))
(defun cf (f) (#^ch f)))
|#
