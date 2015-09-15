;;; -*- mode: Lisp; Syntax: Common-Lisp; Package: user; -*-

(in-package :cl-user)

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

;;; Author:  JP Massar.

;;;; Implements EXACT MATCHING.  C.f. Gusfield, Chapter 1 & Chapter 5.

;;;; The toplevel routines are

;;;; EXACT-MATCHES (PATTERN TEXT &OPTIONAL JUST-FIRST?)
;;;; NAIVE-SUFFIX-TREE-BUILD (TEXT)
;;;; ST-MATCH (PATTERN SUFFIX-TREE &optional UNIQUE-CHAR)

;;; Compare the characters of the string S of length LEN 
;;; starting at position S1-1 < S2-1
;;; with the characters of the string starting at position S2-1 > S1-1.
;;; Return the number of matches (possibly 0) before a mismatch occurs
;;; (or the end of the string is reached).

(defun match-length (s len s1 s2)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (declare (simple-string s))
  (declare (fixnum len s1 s2))
  (let ((count 0))
    (declare (fixnum count))
    (do ((j (the fixnum (1- s1)) (the fixnum (1+ j)))
         (i (the fixnum (1- s2)) (the fixnum (1+ i))))
	((= i len))
      (declare (fixnum i j))
      (if (eql (schar s j) (schar s i)) (incf count) (return)))
    count
    ))

;;; The FUNDAMENTAL STRING PROCESSING algorithm as specified in
;;; Gusfield, pp 8-9.

;;; This computes the array Z, such that Z[i] is the length of the
;;; longest substring of S starting at position i that matches a
;;; prefix of S (that is, an initial substring).

(defun fundamental-processing (s)

  (let* ((len (length s))
	 (z (make-array (list (1+ len))))
	 (left 0) 
	 (right 0))
    (declare (fixnum len left right))

      ;; initial processing.  Compute Z[2]
    
      (let ((mc (match-length s len 1 2)))
	(setf (aref z 2) mc)
	(when (plusp mc) (setq right (1+ mc) left 2)))
    
    ;; Compute all Z[k>2]
    
    (do ((k 3 (1+ k))) ((= k (1+ len)))
      
      (if (> k right)
	  
          ;; Case 1, page 9

	  (let ((mc (match-length s len 1 k)))
	    (setf (aref z k) mc)
	    (when (plusp mc) (setq right (+ k mc -1) left k)))
	
        ;; Case 2, page 9

	(let ((k-prime (+ k (- left) 1))
	      (beta (+ right (- k) 1)))
	  (if (< (aref z k-prime) beta)
              ;; Case 2a
	      (setf (aref z k) (aref z k-prime))
            ;; Case 2b
	    (let* ((mc (match-length s len (+ beta 1) (+ right 1)))
		   (q (+ (+ right 1) mc)))
	      (setf (aref z k) (- q k))
	      (setq right (- q 1))
	      (setq left k)
	      )))
	
	))
    
    z
    
    ))
		

;;; The linear-time exact matching algorithm for finding a pattern
;;; in a text.  Described in Gusfield pp 10-11.

;;; Gusfield proposes the concatenation of PATTERN with TEXT.  Here
;;; we implement this notion such that the concatenation is never
;;; actually done; rather it is conceptually done by providing the
;;; PSEUDO-SCHAR function which gets a character at a conceptual
;;; position i from the conceptually concatenated string PATTERN$TEXT
	     
;;; If we index at the $ position
;;; NIL is returned (since we compare with EQL, this works correctly).
;;; In this way we do not have to deal with determining a character
;;; which is not part of the alphabet.

(defun psuedo-schar (pattern plen text index)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (declare (simple-string pattern text))
  (declare (fixnum plen index))
  (cond
   ((< index plen) (schar pattern index))
   ((= index plen) nil)
   (t (schar text (the fixnum (- index (the fixnum (1+ plen))))))
   ))

;;; Same as MATCH-LENGTH above, save that we look (conceptually)
;;; in PATTERN$TEXT.  PLEN is the length of the pattern, while LEN
;;; is the length of PATTERN$TEXT (not TEXT).

(defun psuedo-match-length (pattern plen text len s1 s2)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (declare (simple-string s))
  (declare (fixnum len s1 s2))
  (let ((count 0))
    (declare (fixnum count))
    (do ((j (the fixnum (1- s1)) (the fixnum (1+ j)))
         (i (the fixnum (1- s2)) (the fixnum (1+ i))))
	((= i len))
      (declare (fixnum i j))
      (if (eql (psuedo-schar pattern plen text j) 
               (psuedo-schar pattern plen text i))
          (incf count)
        (return)))
    count
    ))

	  
(defvar *z-array* nil)

;;; To simply find PATTERN in TEXT, we don't need a Z array for the
;;; entire string PATTERN$TEXT.  All we need is a Z array for PATTERN
;;; since we will never access Z[k], k > LENGTH(PATTERN).

;;; Instead of returning the Z array, this returns a list of positions
;;; in TEXT where PATTERN occurs (offset by +1, to get the actual index,
;;; subtract one from each number in the list).

;;; If JUST-FIRST? is true, then a single number is returned, not 
;;; a list (or NIL).

(defun fundamental-psuedo-processing 
       (pattern text &optional (just-first? nil))

  (block exit

    (let* ((plen (length pattern))
           (len (+ plen 1 (length text)))
           (match-positions nil)
	   (left 0) 
	   (right 0)
           (mc 0))
      (declare (fixnum plen len left right mc))

      ;; Create a big enough Z array

      (when (< (length *z-array*) (1+ plen))
        (setq *z-array* (make-array (list (1+ plen)))))

      (let ((z-array *z-array*))
        (declare (type simple-vector z-array))

        ;; First fill up the Z array up to the pattern length.
        ;; Then, just look to see if we found a match in the text
        ;; that is as long as the pattern.  If so, it is an occurrence
        ;; of pattern in text (by definition of Z[i]), so store the index of 
        ;; this match away, modified to index into just the text, not
        ;; PATTERN$TEXT.

        (flet ((z-and-match (k mc)
                 (declare (fixnum k mc))
                 (if (<= k plen)
                     (setf (aref z-array k) mc) 
                   (when (= mc plen) 
                     (if just-first?
                         (return-from exit (- k plen 1))
                       (push (- k plen 1) match-positions))
                     ))))

          ;; initial processing.  Compute Z[2]
    
          (setq mc (psuedo-match-length pattern plen text len 1 2))
          (setf (aref z-array 2) mc)
          (when (plusp mc) (setq right (1+ mc) left 2))
    
          ;; Compute all Z[k>2]
    
          (do ((k 3 (1+ k))) ((= k (1+ len)))
            (declare (fixnum k))
      
            (if (> k right)
	  
                (progn
	          (setq mc (psuedo-match-length pattern plen text len 1 k))
                  (z-and-match k mc)
	          (when (plusp mc) (setq right (+ k mc -1) left k)))
	
	      (let ((k-prime (+ k (- left) 1))
	            (beta (+ right (- k) 1)))
	        (if (< (aref z-array k-prime) beta)
                    (z-and-match k (aref z-array k-prime))
	          (let* ((mc (psuedo-match-length 
                              pattern plen text len (+ beta 1) (+ right 1)))
		         (q (+ (+ right 1) mc)))
                    (z-and-match k (- q k))
	            (setq right (- q 1))
	            (setq left k)
	            )))
	
	      ))))
    
      (nreverse match-positions)

      )))


(defun exact-matches (pattern text &optional (just-first? nil))
  (let ((len (length pattern)))
    (cond
     ((zerop len) nil)
     ((= len 1)
      (let ((pchar (schar pattern 0)))
        (loop for ch across text 
              for j from 1
              if (eql ch pchar) collect j
              )))
     (t (fundamental-psuedo-processing pattern text just-first?))
     )))


;;; The time to do the search is basically independent of
;;; the pattern length when using EXACT-MATCH, but it increases
;;; in proportion to the pattern length when using SEARCH.
;;; This is a demonstration of this phenomenon.

(defun time-match-vs-search (pattern text n)
  (time (dotimes (j n) (exact-matches pattern text)))
  (time (dotimes (j n) (search pattern text)))
  )

(defun demo-exact-match ()
  (format t "~&~%;; Using unlocated pattern of length 8:~%")  
  (let ((text (make-string 2000 :initial-element #\x))
        (pattern "xxxxxxxa"))
    (time-match-vs-search pattern text 200))
  (format t "~&~%;; Using unlocated pattern of length 16:~%")
  (let ((text (make-string 2000 :initial-element #\x))
        (pattern "xxxxxxxxxxxxxxa"))
    (time-match-vs-search pattern text 200))
  )


;;;; SUFFIX TREES.  Gusfield, Chapter 5.


;;; Leaf nodes have a numeric label but no out edges.
;;; Root has no in edge and is labelled :root.
;;; Other nodes have in and out edges but no (external) label.
;;; The index is not used yet; it is meant to be used to
;;; be able to search the edges in less than linear time.

(defstruct st-node in-edge out-edges label index)

(defun st-leaf-node (n) (null (st-node-out-edges n)))
(defun st-internal-node (n) (not (null (st-node-out-edges n))))

;;; An edge points into the string whose suffix tree
;;; this edge belongs to.  It defines a substring of the string,
;;; using START and END indices.  It points back to the node
;;; it was emitted from and points forward to the node it is going to.

(defstruct st-edge start end prev-node next-node)

;;; The suffix tree structure consists of the string, the root node
;;; of the suffix tree, and the unique character used to mark the end.

(defstruct suffix-tree root string endchar)

(defparameter *st-endchar* #\$)

;;; The string S, which is turned into a suffix tree, conceptually
;;; has the character *ST-ENDCHAR* concatenated to it at its tail.

(defun st-schar (s i) 
  (declare (simple-string s) (fixnum i))
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (if (= i (length s)) *st-endchar* (schar s i)))


;;; Returns the suffix tree for S$.
;;; YOU NEED TO HAVE *PRINT-CIRCLE* set to T if you try to print
;;; out the suffix tree returned!!

(defun naive-suffix-tree-build (s &optional (endchar #\$))

  ;; Build N[1], the initial suffix consisting of the entire string.
  ;; It is composed of one edge going from root to a single leaf.

  (let* ((*st-endchar* endchar)
         (len-s (length s))
         (len-s$ (1+ len-s))
         (root-node
          (make-st-node :in-edge nil :out-edges nil :label :root :index nil))
         (initial-leaf (make-leaf 1))
         (initial-edge 
          (make-st-edge 
           :start 0 :end len-s$ :prev-node root-node :next-node initial-leaf))
         (st (make-suffix-tree 
              :root root-node :string s :endchar *st-endchar*)))

    (setf (st-node-in-edge initial-leaf) initial-edge)
    (add-edge initial-edge root-node)
    
    ;; Now insert successive suffixes into the tree.
    ;; SUFFIX-START is one based, not zero based, so subtract one
    ;; from it when we use it to index into the string.

    (loop for suffix-start from 2 to len-s$ do
          
          ;; (print (list 'suffix-start suffix-start))
          ;; (pp-suffix-tree st)

          (let ((current-node root-node) (suffix-index suffix-start))

            (tagbody

             next-node

             ;; Look at each edge coming out of current-node.  
             ;; Either we find an edge whose first character is 
             ;; S[suffix-index] or there is no possible match.

             (let* ((first-suffix-char (st-schar s (1- suffix-index)))
                    (matching-edge 
                     (find-matching-edge current-node s first-suffix-char)))

               (if (null matching-edge)

                   ;; No initial match.  Create a new edge coming out of the
                   ;; current node which points to a new leaf node for label
                   ;; SUFFIX-START, and then on to next suffix.

                   (progn
                     (let* ((new-leaf (make-leaf suffix-start))
                            (new-edge 
                             (make-st-edge 
                              :start (1- suffix-index) :end len-s$
                              :prev-node current-node :next-node new-leaf)))
                       (setf (st-node-in-edge new-leaf) new-edge)
                       (add-edge new-edge current-node)))

                 ;; We found an initial match.
                 ;; Where does the match stop?

                 (progn
                   (let ((edge-start (st-edge-start matching-edge))
                         (edge-end (st-edge-end matching-edge))
                         (edge-mismatch-pos nil) (suffix-mismatch-pos nil))
                     ;;(print (st-subseq s edge-start edge-end))
                     ;;(print (st-subseq s suffix-start len-s$))
                     (do ((k (1+ edge-start) (1+ k))
                          (j suffix-start (1+ j)))
                         ((or (= k edge-end) (= j len-s$)))
                       (when (not (eql (st-schar s k) (st-schar s j)))
                         (setq edge-mismatch-pos k)
                         (setq suffix-mismatch-pos j)
                         (return)
                         ))
                     ;; It matches all the way down the edge.  On to
                     ;; the next node.
                     (when (null edge-mismatch-pos)
                       (setq current-node (st-edge-next-node matching-edge))
                       (incf suffix-index (- edge-end edge-start))
                       (go next-node))
                     ;; There is a mismatch.  Split the edge and insert
                     ;; a leaf.  Then on to next suffix.
                     (create-new-node-at 
                      matching-edge edge-mismatch-pos suffix-mismatch-pos
                      suffix-start len-s$)
                     ))

                 )))))

    st

    ))

                
;;; Find an edge coming out of NODE whose first character is CH.
;;; If no such node exists return NIL.

(defun find-matching-edge (node s ch)
  (dolist (edge (st-node-out-edges node))
    (let ((start-char (st-schar s (st-edge-start edge))))
      (when (eql ch start-char) (return edge))
      )))

(defun make-leaf (i) 
  (make-st-node :in-edge nil :out-edges nil :label i :index nil))

(defun add-edge (edge node) (push edge (st-node-out-edges node)))


;;; Split an edge into two edges and a new node between them.
;;; The first new edge starts at the old edge's START, and ends at POS.
;;; The second new edge starts at POS and ends at the old edge's END.

(defun split-edge (e pos)
  (let* ((prev-node (st-edge-prev-node e))
         (next-node (st-edge-next-node e))
         (start (st-edge-start e))
         (end (st-edge-end e))
         (new-node 
          (make-st-node :in-edge nil :out-edges nil :label nil :index nil))
         (new-before-edge 
          (make-st-edge 
           :start start :end pos :prev-node prev-node :next-node new-node))
         (new-after-edge
          (make-st-edge 
           :start pos :end end :prev-node new-node :next-node next-node))
         )
    (values new-before-edge new-after-edge new-node)
    ))


;;; Split the edge, making a new node in the middle of the edge.
;;; Create another edge coming out of the new node whose characters
;;; begin at SUFFIX-POS and end at LEN-S$ (the end of the string)
;;; and which terminates in a leaf node.a

;;; Example:  We have a suffix (... A B C X Y Z)
;;; which has matched the ... down to IN-NODE.
;;; An edge of out IN-NODE is (A B C D E F).  So

;;; We start with   IN-NODE -> (A B C D E F) -> OUT-NODE

;;; We end with     IN-NODE -> (A B C) -> NEW-NODE -> (D E F) -> OUT-NODE
;;;                                          |
;;;                                         \ /
;;;                                       (X Y Z) -> LEAF-NODE

(defun create-new-node-at (edge edge-pos suffix-pos label len-s$)
  (let ((prev-node (st-edge-prev-node edge))
        (new-leaf (make-leaf label)))
    (multiple-value-bind (new-before-edge new-after-edge new-node)
        (split-edge edge edge-pos)
      (setf (st-node-out-edges prev-node) 
            (remove edge (st-node-out-edges prev-node)))
      (add-edge new-before-edge prev-node)
      (add-edge new-after-edge new-node)
      (let ((new-branch-edge 
             (make-st-edge :start suffix-pos :end len-s$ 
                           :prev-node new-node :next-node new-leaf)))
        (setf (st-node-in-edge new-leaf) new-branch-edge)
        (add-edge new-branch-edge new-node)
        ))))


;;; Debugging tools.

(defun st-subseq (string start end)
  (let ((stlen (1+ (length string))))
    (cond 
     ((and (= start (1- stlen)) (= end stlen)) (string *st-endchar*))
     ((= end stlen) 
      (format nil "~A~A" (subseq string start (1- end)) (string *st-endchar*)))
     (t (subseq string start end))
     )))

(defun edge-characters (string edge)
  (st-subseq string (st-edge-start edge) (st-edge-end edge)))

(defun pp-suffix-tree (st)
  (let ((depth 0) 
        (root (suffix-tree-root st))
        (s (suffix-tree-string st))
        (endchar (suffix-tree-endchar st)))
    (format t "~%~%Suffix tree for string ~A~A~%~%" s endchar)
    (format t "ROOT~%")
    (labels ((indent (d) (dotimes (j d) (format t " ")))
             (pp-edge (e)
               (incf depth 2)
               (indent depth)
               (format 
                t "~A~%"
                (mapcar #'string (coerce (edge-characters s e) 'list)))
               (let ((next-node (st-edge-next-node e)))
                 (if (st-leaf-node next-node)
                     (progn
                       (indent (+ depth 2))
                       (format t "Leaf ~D~%" (st-node-label next-node)))
                   (dolist (e (st-node-out-edges next-node)) (pp-edge e))
                   ))
               (decf depth 2)))
      (dolist (e (st-node-out-edges root)) (pp-edge e))
      (terpri)
      )))


;;; Main routine, ST-MATCH.

;;; Find all occurrences of PATTERN in the string that produced ST,
;;; the suffix tree for that string.

(defun st-match (pattern st)
  (and (plusp (length pattern))
       (let ((root (suffix-tree-root st))
             (string (suffix-tree-string st)))
         (st-match-node pattern root string 0)
         )))
   
(defun find-leaves-below (x)
  (cond 
   ((st-edge-p x) (find-leaves-below (st-edge-next-node x)))
   ((st-node-p x) 
    (if (st-leaf-node x) 
        (list (st-node-label x))
      (mapcan #'find-leaves-below (st-node-out-edges x))
      ))))

(defun st-match-node (pattern node s pindex)
  (let* ((ch (schar pattern pindex))
         (matching-edge (find-matching-edge node s ch)))
    (and matching-edge
         (let ((edge-start (st-edge-start matching-edge))
               (edge-end (st-edge-end matching-edge))
               (plen (length pattern))
               (mismatch? nil)
               (match-count 1))
           (do ((k (1+ edge-start) (1+ k))
                (j (1+ pindex) (1+ j)))
               ((or (= k edge-end) (= j plen)))
             (when (not (eql (st-schar s k) (schar pattern j)))
               (setq mismatch? t)
               (return))
             (incf match-count))
           (cond
            (mismatch? nil)
            (t
             (let ((new-node (st-edge-next-node matching-edge))
                   (new-pindex (+ pindex match-count)))
               (cond
                ((>= new-pindex plen) (find-leaves-below new-node))
                ((st-leaf-node new-node) (error "Internal error"))
                (t (st-match-node pattern new-node s new-pindex))
                ))))))))
         

