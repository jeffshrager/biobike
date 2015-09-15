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

;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;

;;;; BLAST

;;; An attempt to demonstrate how the BLAST algorithm works, in part.

#|

The BLAST algorithm takes a relatively short QUERY sequence (say, 500
amino acids long), and tries to find good fits to this sequence in a
relatively large DATABASE sequence (say, 10**6 to 10**9+ amino acids).

To do this it finds TRIPLES (3-length amino acid chains) in the
DATABASE sequence which score high when aligned with some TRIPLE in
the QUERY sequence.   These regions of high similarity are then used
as starting points for a local matching algorithm.

Herein we develop code that finds such TRIPLES and then does a very
simple local matching algorithm.  The code is not efficient in either
time or space; it is pedagogical, and is meant to run on relatively
short DATABASE sequences (a few thousand long) and short QUERY sequences
of ten's of bases.

|#


;;; The BLOSUM-62 scoring (substitution) matrix, and functions for
;;; accessing it efficiently.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *amino-acid-letters* '(a r n d c q e g h i l k m f p s t w y v))
  (defvar *blosum-62*
      (make-array 
       '(20 20)
       :element-type '(signed-byte 8)
       :initial-contents
       '(
	 ;;A  R  N  D  C  Q  E  G  H  I  L  K  M  F  P  S  T  W  Y  V
	 ( 4 -1 -2 -2  0 -1 -1  0 -2 -1 -1 -1 -1 -2 -1  1  0 -3 -2  0) ;; A
	 (-1  5  0 -2 -3  1  0 -2  0 -3 -2  2 -1 -3 -2 -1 -1 -3 -2 -3) ;; R
	 (-2  0  6  1 -3  0  0  0  1 -3 -3  0 -2 -3 -2  1  0 -4 -2 -3) ;; N
	 (-2 -2  1  6 -3  0  2 -1 -1 -3 -4 -1 -3 -3 -1  0 -1 -4 -3 -3) ;; D
	 ( 0 -3 -3 -3  9 -3 -4 -3 -3 -1 -1 -3 -1 -2 -3 -1 -1 -2 -2 -1) ;; C
	 (-1  1  0  0 -3  5  2 -2  0 -3 -2  1  0 -3 -1  0 -1 -2 -1 -2) ;; Q
	 (-1  0  0  2 -4  2  5 -2  0 -3 -3  1 -2 -3 -1  0 -1 -3 -2 -2) ;; E
	 ( 0 -2  0 -1 -3 -2 -2  6 -2 -4 -4 -2 -3 -3 -2  0 -2 -2 -3 -3) ;; G
	 (-2  0  1 -1 -3  0  0 -2  8 -3 -3 -1 -2 -1 -2 -1 -2 -2  2 -3) ;; H
	 (-1 -3 -3 -3 -1 -3 -3 -4 -3  4  2 -3  1  0 -3 -2 -1 -3 -1  3) ;; I
	 (-1 -2 -3 -4 -1 -2 -3 -4 -3  2  4 -2  2  0 -3 -2 -1 -2 -1  1) ;; L
	 (-1  2  0 -1 -3  1  1 -2 -1 -3 -2  5 -1 -3 -1  0 -1 -3 -2 -2) ;; K
	 (-1 -1 -2 -3 -1  0 -2 -3 -2  1  2 -1  5  0 -2 -1 -1 -1 -1  1) ;; M
	 (-2 -3 -3 -3 -2 -3 -3 -3 -1  0  0 -3  0  6 -4 -2 -2  1  3 -1) ;; F
	 (-1 -2 -2 -1 -3 -1 -1 -2 -2 -3 -3 -1 -2 -4  7 -1 -1 -4 -3 -2) ;; P
	 ( 1 -1  1  0 -1  0  0  0 -1 -2 -2  0 -1 -2 -1  4  1 -3 -2 -2) ;; S
	 ( 0 -1  0 -1 -1 -1 -1 -2 -2 -1 -1 -1 -1 -2 -1  1  5 -2 -2  0) ;; T
	 (-3 -3 -4 -4 -2 -2 -3 -2 -2 -3 -2 -3 -1  1 -4 -3 -2 11  2 -3) ;; W
	 (-2 -2 -2 -3 -2 -1 -2 -3  2 -1 -1 -2 -1  3 -3 -2 -2  2  7 -1) ;; Y
	 ( 0 -3 -3 -3 -1 -2 -2 -3 -3  3  1 -2  1 -1 -2 -2  0 -3 -1  4) ;; V
	 )))
  (defun create-substitution-function-given-matrix
      (substitution-matrix ordered-constituents)
    (let ((ht (make-hash-table :test #'eql)))
      (loop for elem in ordered-constituents for count from 0 do
	    (setf (gethash elem ht) count))
      #'(lambda (elem1 elem2)
	  (aref substitution-matrix (gethash elem1 ht)  (gethash elem2 ht))
	  )))
  (defparameter *blosum-substitution-function*
      (create-substitution-function-given-matrix
       *blosum-62* *amino-acid-letters*
       ))
  (defun bscore (s1 s2) (funcall *blosum-substitution-function* s1 s2))
  )


;; Return the sum-score for an exact alignment of two sequences, S1 and S2.
;; S1 is used in its entirety.  Only LENGTH(S1) elements of S2 are used,
;; starting from S2START.  The sum-score is the sum of the individual
;; matchups with respect to some substitution matrix, 
;; here the BLOSUM-62 matrix.

(defun substitution-score (s1 s2 &key (s2start 0) (sf #'bscore))
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (cond
   ((and (listp s1) (vectorp s2))
    (loop for elem in s1 for index from s2start sum
	(funcall sf elem (aref s2 index))
	))
   ((and (listp s1) (listp s2))
    (setq s2 (nthcdr s2start s2))
    (loop for e1 in s1 for e2 in s2 sum (funcall sf e1 e2)))
   ((and (vectorp s1) (vectorp s2))
    (loop for elem across s1 for index from s2start sum
          (funcall sf elem (aref s2 index))))
   ((and (vectorp s1) (listp s2))
    (setq s2 (nthcdr s2start s2))
    (loop for e1 across s1 for e2 in s2 sum (funcall sf e1 e2)))
   ))
         

(defun random-amino-acid-chain (n)
  (let ((chain nil) (naa (length *amino-acid-letters*)))
    (dotimes (j n) (push (elt *amino-acid-letters* (random naa)) chain))
    chain
    ))

;;; Loop over all 8000 possible triples, scoring each against TRIPLE
;;; to see if the score matches or exceeds THRESHOLD.  
;;; Return the qualifying triples and their scores, sorted by score.

;;; For a given score matrix and threshold, these matching triples 
;;; could be precomputed and stored in a hash table, but we don't do that here.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (let ((tvec (vector 0 0 0)))
    (declare (type (simple-vector 3) tvec))
    (defun matching-triples->=-threshold (triple threshold)
      (declare (optimize (speed 3) (safety 0) (debug 0)))
      (declare (fixnum threshold))
      (let ((alphabet *amino-acid-letters*)
	    (matching-triples nil))
        (dolist (a alphabet)
	  (setf (aref tvec 0) a)
	  (dolist (b alphabet)
	    (setf (aref tvec 1) b)
	    (dolist (c alphabet)
	      (setf (aref tvec 2) c)
	      (let ((score (substitution-score triple tvec)))
	        (declare (fixnum score))
	        (when (>= score threshold)
		  (push (list score (list a b c)) matching-triples))
		))))
        (setq matching-triples (sort matching-triples #'> :key #'first))
        matching-triples
        ))))

;;; Compute all subsequences of length LEN whose elements are adjacent.
;;; (e.g., all substrings of length LEN, for a string).
;;; (e.g., Given '(A B C D E) and 3 we return ((A B C) (B C D) (C D E)))
;;; This is how we obtain all the triples from the QUERY sequence.

(defun contiguous-subsequences (sequence len)
  (let ((seqlen (length sequence)))
    (if (> len seqlen)
	nil
      (let ((subseqs nil) (stop (1+ (- seqlen len))))
	(dotimes (j stop)
	  (push (subseq sequence j (+ j len)) subseqs))
	(nreverse subseqs)
	))))

;;; For all the triples in our QUERY, find every possible
;;; triple (from the 8000 possible ones) which, when aligned with any of
;;; the triples from our SEQUENCE, produces a score >= THRESHOLD.

;;; Returns a list of lists.  Each sublist has as its third element
;;; a triple, as it's second element the score, and as it's first element
;;; an index into SEQUENCE identifying which triple from SEQUENCE it was
;;; aligned with to produce the qualifying score.  The list is sorted
;;; by qualifying score.

(defun match-candidates (sequence threshold)

  ;;; Call each triple in the query sequence a KERNEL triple.

  (let ((kernels (contiguous-subsequences sequence 3))
        (matching-triples-data nil))

    ;; First, make each kernel carry around a pointer to the 
    ;; query sequence position where the kernel came from
    (setq kernels
          (loop for kernel in kernels for count from 0 collect 
	        (list kernel (list count))))

    ;; Coalesce duplicate kernels, but keep track of all the
    ;; positions this kernel is found at in the QUERY sequence.
    (let ((list nil))
      (dolist (k kernels) 
        (let ((old-kernel (find (first k) list :key #'first :test #'equal)))
          (if old-kernel
              (push (car (second k)) (second old-kernel))
            (push k list)
            )))
      (setq kernels list))

    ;; Each kernel now points back to each place in the query
    ;; sequence that it occurs.

    ;; Get all the triples (from the 8000 possible) that score high 
    ;; enough when aligned with the kernel triples.

    (setq matching-triples-data
          (mapcan
           #'(lambda (kernel)
	       (let ((triple (first kernel)) (pos-list (second kernel)))
	         (mapcar
	          #'(lambda (match) (cons pos-list match))
	          (matching-triples->=-threshold triple threshold)
	          )))
           kernels
           ))

    ;; Matching-triples-data is of the form (pos-list score triple)

    ;; Coalesce duplicate matching triples, again keeping the
    ;; pointer information.
    (let ((list nil))
      (dolist (mt matching-triples-data)
        (let ((old-mt (find (third mt) list :key #'third :test #'equal)))
          (if old-mt
              (setf (first old-mt)
                    (remove-duplicates (append (first mt) (first old-mt))))
            (push mt list)
            )))
      (setq matching-triples-data list))
               
    ;; Each triple now points back to each place in the query sequence
    ;; that aligns with it so that the alignment score is at least
    ;; THRESHOLD.

    ;; the data is of the form (pos-list score triple)

    matching-triples-data

    ))



;;; Find all positions in LONG-SEQUENCE which begin a triple
;;; which is one of the triples in the list of MATCH-CANDIDATES.
;;; Returns lists of 

;;; (POSITION-IN-LONG-SEQUENCE POSITIONS-IN-QUERY-SEQUENCE SCORE)

(defun find-seed-positions (match-candidates long-sequence)

  ;; First make a hash table of all the match candidates, so we
  ;; don't have to look through the list of match-candidates
  ;; as we get each triple from the database sequence.

  (let ((h (make-hash-table :test #'equal)))

    (dolist (c match-candidates)
      (let ((triple (third c))) (setf (gethash triple h) c)))

    ;; Create successive triples from the database sequence and
    ;; see which ones are the same as any of the match candidates.
    ;; For those that are, store the index away along with the
    ;; information about which part(s) of the query sequence should
    ;; be aligned here.

    (let ((len (length long-sequence))
	  (st (list nil nil nil))
	  (seeds nil))
      (dotimes (j (1+ (- len 3)))
        (setf (first st) (aref long-sequence j))
        (setf (second st) (aref long-sequence (+ j 1)))
        (setf (third st) (aref long-sequence (+ j 2)))
        (let ((match (gethash st h)))
	  (when match (push (list j (first match) (second match)) seeds))))
      (nreverse seeds))))


;;; Returns SEEDS of the form
;;; (POSITION-IN-LONG-SEQUENCE POSITIONS-IN-QUERY-SEQUENCE SCORE)

(defun find-seeds (query-sequence database-sequence threshold)
  (let* ((qy query-sequence)
         (db database-sequence)
         (candidates (match-candidates qy threshold))
         (seeds (find-seed-positions candidates db)))
    ;; Pretty print info about the seeds
    (dolist (seed seeds)
      (let* ((dbpos (first seed))
             (qypos (second seed))
             (db-triple (coerce (subseq db dbpos (+ dbpos 3)) 'list))
             )
        ;; Sanity check first
        (unless (find db-triple candidates :key #'third :test #'equal)
          (error "HUH??  Seed ~A not in candidate list!" seed))
        (when t
          (format t "~%;; Seed ~A at db position ~D matches query triple(s)"
                  db-triple dbpos)
          (dolist (p qypos)
            (format t "~%;;  ~A at query position ~D"
                    (coerce (subseq qy p (+ p 3)) 'list) p))
          (terpri)
          )))
    seeds
    ))
              
;;; Simple-minded seed extension.  Keep extending left and right
;;; without using any spaces or gaps as long as alignment of
;;; previous or next symbols scores positive.

;;; Returns list of indices of subsequence of query and indices of
;;; subsequence of database.  So ((START-QUERY END-QUERY) (START-DB END-DB)) 

(defun maximize-seed (qypos dbpos qy db)
  (let* ((dlen (length db)) (qlen (length qy))
         (delta-left 0) (delta-right 0))
    (do ((i (1- qypos) (1- i)) (j (1- dbpos) (1- j)))
        ((or (minusp i) (minusp j)))
      (when (minusp (bscore (elt qy i) (elt db j))) (return))
      (incf delta-left))
    (do ((i (+ qypos 3) (1+ i)) (j (+ dbpos 3) (1+ j)))
        ((or (>= i qlen) (>= j dlen)))
      (when (minusp (bscore (elt qy i) (elt db j))) (return))
      (incf delta-right))
    (list
     (list (- qypos delta-left) (+ qypos 3 delta-right))
     (list (- dbpos delta-left) (+ dbpos 3 delta-right))
     )))

(defun score-maximized-alignment (positions qy db)
    (let* ((qpos (first positions)) (dpos (second positions))
           (qs (first qpos)) (qe (second qpos))
           (ds (first dpos))
           (sum 0))
      (do ((i qs (1+ i)) (j ds (1+ j)))
          ((= i qe))
        (incf sum (bscore (elt qy i) (elt db j))))
      sum
      ))
        
(defun run-blast (qy db &key (threshold 13) (show 3))
  (format t "~&~%Query sequence: ~A~%~%" qy)
  (let ((seeds (find-seeds qy db threshold))
        (alignments-and-scores nil)
        (max-score 0))
    (format t "~&~%;; Found ~D seeds~%" (length seeds))
    ;; For each seed
    (dolist (seed seeds)
      (destructuring-bind (dbpos qypos-list seed-triple) seed
        (declare (ignore seed-triple))
        ;; For each query kernel it is to be aligned with
        (dolist (qypos qypos-list)
          (let* ((maximized-alignment
                  (maximize-seed qypos dbpos qy db))
                 (maximized-score 
                  (score-maximized-alignment maximized-alignment qy db)))
            (push (list maximized-alignment maximized-score) 
                  alignments-and-scores
                  )))))
    (setq alignments-and-scores
          (remove-duplicates alignments-and-scores :key #'first :test #'equal))
    (setq alignments-and-scores 
          (sort alignments-and-scores #'> :key #'second))
    (setq max-score (second (first alignments-and-scores)))
    
    (do ((i 0 (1+ i)) (as alignments-and-scores (cdr as)))
        ((or (> i show) (null as)))

      (let* ((positions (first (first as)))
             (score (second (first as)))
             (qypos (first positions)) (dbpos (second positions))
             (qs (first qypos)) (qe (second qypos)) (ds (first dbpos))
             )

        (format t "~%;; A ~A alignment starts at position ~D"
                (if (= score max-score) "maximal" "lesser") ds)
        (format t "~%;; of the database, and at position ~D in the query." qs)
        (format t "~%;; The alignment has a score of ~D.~%~%" score)
        
        ;; Pretty print the actual alignment.

        (let ((alignment nil))
          (dotimes (j (length qy))
            (push
             (cond
              ((< j qs)
               (if (minusp (- ds (- qs j)))
                   (list '- (elt qy j))
                 (list (elt db (- ds (- qs j))) (elt qy j))))
              (t
               (list (elt db (+ ds (- j qs))) (elt qy j))
               ))
             alignment
             ))
          (setq alignment (nreverse alignment))
          (let ((db-header "DB MATCH: ") 
                (qy-header "QUERY   : "))
            (format t "~%~A" db-header)
            (dolist (a alignment) (format t "~A " (first a)))
            (format t "~%~A" qy-header)
            (dolist (a alignment) (format t "~A " (second a)))
            (terpri) 
            (dotimes (j (+ (* 2 qs) (length db-header))) (format t " "))
            (format t "^")
            (dotimes (j (1- (* 2 (- qe qs 1)))) (format t " "))
            (format t "^")
            (terpri) (terpri)
            ))

        ))))
      
(defvar *qy*)
(defvar *db*)

(defun run-test-blast ()
  (run-blast 
   (setq *qy* (random-amino-acid-chain 10))
   (setq *db* (coerce (random-amino-acid-chain 2000) 'simple-vector))
   :threshold 17
   ))
