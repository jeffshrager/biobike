;;; -*- mode: Lisp; Syntax: Common-Lisp; Package: user; -*-

(in-package :cl-user)

;;; Author:  Original code by Russ Altman.
;;; Signficant rewrite and optimization by JP Massar. 05/01/03.
;;; Algorithm description by JP Massar.

;;; TO RUN EXECUTE:
;;; (TEST-VERBOSE) to see the algorithm progress.
;;; (TIME-NW 20 25) to have algorithm align sequences of lengths 20,21,22,23,24
;;; (TEST-NW) to test algorithm against simple, one-path, cases.

#|

This algorithm finds a set of global best matches for two sequences.

That is, given A[0...N], and B[0...M]

it finds a set of solutions which consist of

  -- The elements of A, possibly interspersed with gaps (denoted by '-')
  -- The elements of B, possibly interspersed with gaps (denoted by '*')
  -- An alignment of A with gaps over B with gaps, such that for at least 
     one i and j, A(i) is aligned with some B(j).

  An example:

  Sequence A:  CGAGGAA
  Sequence B:  ACGTGA

One solution may look like

A:     CGAGGAA
       || | |
B:    ACGTG*A

So there is a single gap in Sequence B while Sequence A is shifted by
one position with respect to Sequence B.

Another might be

A:     AC-GTCA
       |  |  |
B:   CGA*GG*AA

(this actually has more gaps and less matching alignments, so is not
likely to rank as high as the first solution, using any kind of
reasonable metric, and therefore would not be a 'best match').

(Note that it is possible to have a gap of more than a single space,
and more than one gap per sequence, although the former is not illustrated
above)

(Note that there may be a large number of reasonable solutions,
especially as the sequences get longer.)
 
What constitutes a solution depends on how one defines what
constitutes a 'good' match.

There are three parameters generally employed to determine a good match:

   -- How much to reward element alignment
   -- How much to penalize for the existence of a gap
   -- How much to penalize as a gap gets longer

It is possible to specify different rewards for different matching
alignments (e.g., a match of T on top with T on bottom could be given
a higher value than matching G on top with G on bottom).  It is also
possible to reward aligments that don't 'match': e.g., having T on top
and T on the bottom could generate a very high score, while T on top
and A on the bottom a somewhat lesser score, and T on top and either C
or G on the bottom a poor score.  So TT > TA > TC = TG.

It is theoretically possible to specify different penalties for gaps
and gap lengths in the first sequence vs in the second sequence,
although this code does not provide that functionality.

Determining the reward for element alignment is done by constructing a
SUBSTITUTION MATRIX, and, in this code, the corresponding SUBSTITUTION
FUNCTION, SUBF.  For any two sequence elements X and Y, (SUBF X Y) is
used to obtain the reward for X and Y being aligned.  See below for
more details.

Determining the penalties for a gap is done by providing a
GAP-PENALTY-FUNCTION or providing parameters that allow construction
of a function which increases linearly with increasing gap length.

Typical gap functions are one of:

  -- no gap penalty, i.e, #'(lambda (gaplength) 0.0)
  -- constant gap penalty, e.g., #'(lambda (gaplength) -1.0)
  -- gap penalty proportional to gap length, e.g.,
     #'(lambda (gaplength) (* 0.5 gaplength))
  -- one penalty for opening a gap and another linear term for
     extending it, e.g.,
     #'(lambda (gaplength) (+ 1.0 (* 0.25 (1- gaplength))))

The latter form is typically referred to in the literature as the affine
gap penalty function, and is commonly used.

The algorithm to figure the set of best alignments consists of three parts:

   -- Generate and fill an NxM SCORE matrix, where M is the length of Sequence 
   A and N the length of Sequence B.  The matrix values are determined by the
   order of the sequence elements and the reward and penalty parameters.

   -- Generate and fill an NxM BACKPOINTER matrix.  Each element (I,J)
   of the BACKPOINTER matrix determines an optimal alignment for the
   subsequences A[0...I] and B[0...J].

   -- Use the generated matrices to then figure out a set of best alignments.

The first two steps are usually done concurrently, and are done that way
in this code.

Let us from now on denote our sequences as SEQA and SEQB.
Let length(SEQA) = N, and length(SEQB) = M.

Assume that SEQA = CGAGGAA, SEQB = ACGTGA

Then the SCORE matrix initially looks conceptually like this, once it's
elements have been initialized to zero.

                 SEQB -->

          A   C   G   T   G   A      

      C  0.0 0.0 0.0
   
      G  0.0 0.0 0.0
 S
 E    A  0.0 0.0 0.0 . . .
 Q
 A    G           .
                  .
 |    G           .
 |
\ /   A
 .
      A

Given an index (i,j) into this matrix, we can compute what are called
the RESIDUES, (the symbols above the column and the symbols to the
left of the row) for the (i,j)th element of the SCORE matrix (which
are just the sequence elements of the two sequences at positions I and
J, respectively).  So the RESIDUES for position (3,5) are G and A,
while the RESIDUES for (0,0) are C and A.

Note that in all the code and all the examples, the matrix is indexed
using (i = ROW, j = COLUMN).  Further, I is always the first index and
J the second index.  (In the code SEQA is called 'SEQ-DOWN-ROWS' and
SEQB is named 'SEQ-ACROSS-COLS', for clarity)

 The substitution matrix, SUB, for our sequences might look like this:

    A   C   G   T
 
 A  1   0   0   0

 C  0   1   0   0

 G  0   0   1   0

 T  0   0   0   1

In this metric, all matches are rewarded equally, and all non-matches
are neutral (neither rewarded nor penalized).

Now to the details of the algorithm.

For any (i,j), SEQA[i] and SEQB[j] give us the RESIDUES (as above) for
position (i,j).  Given the residues, we can (conceptually) index into
SUB to determine the score for alignment of the two sequences at
position (i,j).  (Of course to index into an actual matrix one needs
numerical indices, so we really need the mapping A -> 0, C -> 1, G ->
2, T -> 3, given our example SUB matrix.  This mapping is provided
transparently by the function CREATE-SUBSTITUTION-FUNCTION-AND-MATRIX,
which returns a function which is called with the two residues.)

The algorithm uses the SUBSTITUTION FUNCTION to determine a value when
dealing with position (I,J).  First it gets the RESIDUES for I and J,
and then the SUBSTITUTION FUNCTION is called to get the score for
aligning the two residues.

The algorithm must first fill in values for all of the elements of SCORE
according to the following recursion relationship:

 For any given element SCORE(i,j), assuming we have already filled in
 all elements SCORE(k<i,l<j), the value of SCORE(i,j) will be

 A.  The MAXIMUM of three values

   1.  SCORE(i-1,j-1)

   2.  The maximum over k, of SCORE(k,j-1) + W((i-1)-k), k = 0...i-2
       (That is, a maximum over the elements in column J-1, down to 
       and including row I-2).

   3.  The maximum over k, of SCORE(i-1,k) + W((j-1)-k), k = 0...j-2
       (That is, a maximum over the elements in row I-1, over to 
       and including column J-2).

 B.  PLUS the value of SUB(SEQB[i],SEQA[j])

So what is W?  W is our penalty function for gaps and W(x) is the
penalty value for having a gap of length x.  The value of k determines
how big the gap is.  (Starting from the left edge, and going across,
as in 3, the size of the gap is a maximum at the left edge (k=0) and
decreases as k increases, and correspondingly as we move down a column
in 2.)

Given that we are careful to special-case the leftmost column and
uppermost row (since there is no J-1 column or I-1 row in these cases)
this algorithm can then be used to sequentially fill in the rest of
the array.

The BACKPOINTER matrix is filled in by determining which of the cases
from above, 1, 2, or 3, had the maximum score, and setting the backpointer
to point from cell (I,J) back to the cell which held the maximum score.

So for the cell labelled '*' in the below matrix, the recursion formula
looks at all cells labelled '+'

- - - + - -
- - - + - -
+ + + + - -
- - - - * -
- - - - - -

For these cells, we find the cell that has the maximum value according
to the recursion formula, and set the backpointer to point to that
cell.  (The code handles the case where two or more cells hold maximal
values).

The SCORE matrix (*SCOREMAT*), and the BACKPOINTER matrix
(*POINTERMAT*) are computed by the GENERATE-NEEDLEMAN-WUNSCH-SCORES
function below.

Note: the recursion relationship described above is one of two such
global alignment recursion formulas found in the biology literature.
For a discussion of the differences, and an implementation of the
other one, see the files match-essay.txt and global-alignment.lisp in
this directory.

Note: why the algorithm works is beyond the scope of this explanation.
See, for example, Gusfield, "Algorithms on Strings, Trees and
Sequences", Chapter 11.)

What do the numbers in the SCORE matrix mean once we compute them?
Basically, the numbers tend to increase as we go down and right, and
the more positive an element (I,J) is, the better it is to align the
two sequences such that SEQA[I] is over SEQB[J].

To determine a best alignment, now all we need to do is follow the
backpointers we have set up.  What we do is start from a position with
the highest score in the bottom row or rightmost column, and work our
way up and to the left (towards the origin) via our backpointers,
creating paths through the matrix from this point back to the origin.
The geometry of these paths determine the alignment.  (Since there may
be more than one such maximal point along the bottom row or rightmost
column, and there may be more than one backpointer from a given cell,
there may be numerous best alignments).

Given a path through the matrix, how do we determine what sequence
alignment it specifies? 

This is how: For any given (I,J) in our path, the sequence lines up
at that point and therefore the RESIDUES at (I,J) constitute the
alignment at that point.

Given point (I,J) in the path, if the previous point in the path is at
(I-1,J-1), this means there is no gap.

Otherwise, there is a gap.  If the previous point is above (I-1,J-1),
then the gap is in the sequence which got indexed across the columns,
while if the previous point is to the left of (I-1,J-1), then the gap
is in the sequence which got indexed down the rows.

So suppose we have the following path back through the matrix:

(5,5) -> (3,4) -> (2,3) -> (1,2) -> (0,0), 

or graphically:

   A  C  G  A  C  T

A  *
G        *
A           *
A              *
A
T                 *


We start at the bottom right, at (5,5), with T aligned with T as the last 
characters of both sequences.  The next point is not at (4,4), but rather
at (3,4) which is above (4,4), so there is a gap in the sequence which got 
indexed across the columns (which means the element of the sequence indexed 
down the rows at position 4, A, is paired with the gap).  So we have
(remember, we are going backwards):
       
       AAT
       C*T

From (3,4) we go to (2,3), so there is no gap, and once more with no gap,
to (1,2), and we have:

     GAAAT
     GAC*T 

Finally, we go to (0,0) which is to the left of (0,1), so there is a gap
in the sequence indexed down the rows.  Our end result is therefore:

     A-GAAAT
     ACGAC*T

We can produce such alignments for every backpath as we choose.

|#

(defvar *pointermat*)
(defvar *scoremat*)
(defvar *best-matches*)
(defvar *bestmatch*)
(defvar *allpaths*)
(defvar *all-points-paths*)
(defvar *substitution-matrix*)
(defvar *substitution-function*)
(defvar *gap-penalty-function*)

(defvar *nw-verbose* t)

(defparameter *time-nw?* t)
(defparameter *time-bp?* t)
(defparameter *time-new?* nil)
(defparameter *time-old?* t)

(defparameter *testseq-a* '(T T G A C A C C C T C C C A A T T G T A))
(defparameter *testseq-b* '(A C C C C A G G C T T T A C A C A T))


;;; MACROS, ETC


;;; Abstraction/optimization of a 2-dimensional POINT,
;;; which is an index (ROW COL) into a matrix.  We pack
;;; ROW and COL into a single fixnum.

(defmacro nwpoint (row col) 
  `(the fixnum (+ (the fixnum (ash ,col 16)) ,row)))
(defmacro nwpoint-row (p) `(logand (the fixnum ,p) #x0000FFFF))
(defmacro nwpoint-col (p) `(ash (the fixnum ,p) -16))
(defmacro nwpoint-list (p) 
  (let ((psym (gensym "NWPOINT-")))
    `(let ((,psym ,p))
       (list (nwpoint-row ,psym) (nwpoint-col ,psym)))))
(deftype nwpoint () 'fixnum)

;;; Assumes BODY being timed returns a single result!
(defmacro maybetime ((time? &key (n 1) (notice nil)) &body body)
  `(flet ((do-body () ,@body))
     (if ,time?
	 (let ((result nil) (notice ,notice) (n ,n))
	   (when notice (format t "~&;; Timing ~A, ~D times~%" notice n))
	   (time (dotimes (j n) (setq result (do-body))))
	   result
	   )
       (do-body))))


;;;; TOPLEVEL ROUTINE

;;; Outline:
;;;  1.  Generate a substitution matrix function from parameters
;;;  2.  Generate a gap penalty function, if not supplied, from parameters
;;;  3.  Generate the SCORE matrix.
;;;  4.  Find all the points that end the best sequence alignments
;;;        and select one.
;;;  5.  Generate all the best paths from this point back through the matrix
;;;  6.  Pretty print (some of) the alignments corresponding to these 
;;;      best paths.


(defun run-nw 

       (&optional
        (seq-down-rows *testseq-a*)
        (seq-across-cols *testseq-b*)
        &key
        (constituents nil)
        (sub-function nil)
        (matchval 1.0)
        (nonmatchval 0.0)
        (pairwise-sub-values nil)
        (pairwise-symmetric? t)
        (gap-function nil)
        (gap-open-penalty 1.0)
        (gap-space-penalty 0.25)
        (show-alignments? t)
        (max-alignments-to-show 5)
        (verbose *nw-verbose*)
        )
        
  (setq *substitution-matrix* nil)

  (when verbose
    (format t "~&~%;; Sequence A:  ")
    (dolist (a seq-down-rows) (format t "~A " a)) (terpri)
    (format t "~%;; Sequence B:  ")
    (dolist (b seq-across-cols) (format t "~A " b)) (terpri) (terpri)
    )

  ;; Create substitution function

  (if sub-function
      (setq *substitution-function* sub-function)
    (multiple-value-setq (*substitution-function* *substitution-matrix*)
        (create-substitution-function-and-matrix
         (or constituents
             (setq constituents
                   (remove-duplicates 
                    (union seq-down-rows seq-across-cols))))
         :default-matching-value matchval
         :default-nonmatching-value nonmatchval
         :pairwise-values pairwise-sub-values
         :pairwise-symmetric? pairwise-symmetric?
         )))
  (setq sub-function *substitution-function*)

  (setq *gap-penalty-function* gap-function)

  ;; It is more efficient to have the sequences as vectors than as lists.

  (setq seq-down-rows (coerce seq-down-rows 'simple-vector))
  (setq seq-across-cols (coerce seq-across-cols 'simple-vector))

  ;; Generate and fill the SCORE matrix (*SCOREMAT*) and the
  ;; *POINTERMAT* array

  (if gap-function
      ;; General routine
      (generate-needleman-wunsch-scores
       seq-down-rows seq-across-cols sub-function gap-function)
    ;; Optimized routine for linear gap penalty function
    (progn
      (when verbose
        (format t "~%;; Gap penalty formula = P(n) = ~A + ((n-1) * ~A))~%"
                gap-open-penalty gap-space-penalty))
      (generate-needleman-wunsch-scores-linear-gap-function
       seq-down-rows seq-across-cols sub-function 
       gap-open-penalty gap-space-penalty
       )))
     
  (when verbose 
    (when *substitution-matrix*
      (pp-sub-matrix *substitution-matrix* constituents)
      (pp-score-matrix seq-down-rows seq-across-cols)))
     
  ;; Find a best point along the outer column or last row of SCORE.

  (setq *best-matches* (find-best-endpoints))
  (setq *bestmatch* (first *best-matches*))
  (when verbose 
    (format t "~&;; ~D Best endpoints.~%" (length *best-matches*))
    (format t ";; Best points: ~S~%"
            (mapcar #'(lambda (p) (nwpoint-list p)) *best-matches*))
    (format t ";; Using first endpoint ~S~%" (nwpoint-list *bestmatch*))
    )

  ;; Find all the best paths from that point.

  (setq *all-points-paths* 
        (get-best-paths
         (nwpoint-row *bestmatch*) (nwpoint-col *bestmatch*) *pointermat*))
     
  ;; Display some or all of the best alignments
  ;; anchored on the *bestmatch* point.

  (let ((npaths (length *all-points-paths*)))
    (when *nw-verbose* (format t "Number of best paths: ~D~%" npaths))
    (when show-alignments?
      (if (>= max-alignments-to-show npaths)
          (dotimes (i npaths)
            (format t "~&;; Alignment ~D (of ~D):~%" (1+ i) npaths)
            (pp-point-path 
             (elt *all-points-paths* i) seq-down-rows seq-across-cols))
        (let ((already-shown nil))
          (do ((i 0 (1+ i))) 
              ((= (length already-shown) max-alignments-to-show))
            (let ((index (random npaths)))
              (unless (member index already-shown)
                (format t "~&;; Alignment ~D (of ~D):~%" (1+ index) npaths)
                (pp-point-path 
                 (elt *all-points-paths* index) seq-down-rows seq-across-cols)
                (push index already-shown)
                )))))))

  )


;;; SUBSTITUTION MATRIX UTILITIES


;;; Create a matrix with two different values,
;;; one on the diagonal and one off the diagonal.

(defun float-diagonal-matrix (n exact-match-value non-match-value)
  (let ((m (make-array (list n n) 
		       :element-type 'single-float
		       :initial-element (float non-match-value 0.0))))
    (declare (type (simple-array single-float 2) m))
    (let ((fval (float exact-match-value 0.0)))
      (dotimes (j n) (setf (aref m j j) fval)))
    m
    ))


;;; Given the set of possible elements in the sequences, 
;;; return a function of two arguments F(C1,C2).  When called
;;; on any two sequence elements, returns the value of the substitution matrix
;;; for those two elements.

;;; So (CREATE-SUBSTITUTION-FUNCTION-AND-MATRIX '(A C G T))

;;; creates a 4x4 floating point identity matrix and returns a function
;;; F, such that (F 'A 'A) -> 1.0, (F 'C 'C) -> 1.0, (F 'C 'G) -> 0.0

;;; The key arguments allow one to customize the substitution matrix.
;;; For example:

#|
(create-substitution-function-and-matrix 
 '(A C G T) :default-matching-value 2.5 
 :default-nonmatching-value -1.0
 :pairwise-values '((a c 17.0)))

Produces a matrix which looks like

  2.5 17.0 -1.0 -1.0
 17.0  2.5 -1.0 -1.0 
 -1.0 -1.0  2.5 -1.0  
 -1.0 -1.0 -1.0  2.5
|#

;;; and (F 'A 'C) -> 17.0, etc.


(defun create-substitution-function-and-matrix
    (ordered-constituent-set
     &key (default-matching-value 1.0)
	  (default-nonmatching-value 0.0)
	  (pairwise-values nil)
	  (pairwise-symmetric? t)
	  )
  (dolist (triple pairwise-values)
    (unless (and (member (first triple) ordered-constituent-set)
                 (member (second triple) ordered-constituent-set))
      (error "Oops!  One of the elements in ~S is not a sequence element"
             triple)))
  (let* ((matrix-size (length ordered-constituent-set))
	 (sub (float-diagonal-matrix 
	       matrix-size 
	       default-matching-value
	       default-nonmatching-value))
	 (map (make-hash-table :test #'eql))
	 )
    (declare (type (simple-array single-float 2) sub))
    ;; Create mapping from elements of sequence alphabet to
    ;; indices of substitution matrix.
    (do ((set ordered-constituent-set (cdr set)) (index 0 (1+ index)))
	((null set))
      (setf (gethash (car set) map) index))
    ;; Store user-specified special values in substitution matrix.
    (dolist (triple pairwise-values)
      (let* ((c1 (first triple)) 
	     (c2 (second triple))
	     (val (float (third triple) 0.0))
	     (down-rows-index (gethash c1 map))
	     (across-cols-index (gethash c2 map))
	     )
	(setf (aref sub down-rows-index across-cols-index) val)
	(when pairwise-symmetric?
	  (setf (aref sub across-cols-index down-rows-index) val))
	))
    ;; Return substitution function as primary result.
    ;; It calculates the indices into the substitution matrix
    ;; (which it has closed over) and arefs the matrix to get the value.
    (values 
     #'(lambda (c1 c2)
	 (declare (optimize (speed 3) (safety 0) (debug 0)))
	 (if (eql c1 c2)
	     (let ((index (gethash c1 map))) 
	       (declare (fixnum index))
	       (aref sub index index))
	   (let ((down-rows-index (gethash c1 map))
		 (across-cols-index (gethash c2 map)))
	     (declare (fixnum down-rows-index across-cols-index))
	     (aref sub down-rows-index across-cols-index)
	     )))
     sub
     )))
	  

	
;;;; MATRIX GENERATION    


(defparameter *paths-hash-table* (make-hash-table :test #'eql))


(defun adjust-nw-arrays-to-size (nrows ncols)
  (setq *scoremat*
        (or (and (boundp '*scoremat*) 
                 (arrayp *scoremat*)
                 (equal (array-dimensions *scoremat*) (list nrows ncols))
                 *scoremat*)
            (make-array (list nrows ncols) :element-type 'single-float)
            ))
  (setq *pointermat*
        (or (and (boundp '*pointermat)
                 (arrayp *pointermat*)
                 (equal (array-dimensions *pointermat*) (list nrows ncols))
                 *pointermat*)
            (make-array (list nrows ncols))
            )))


(defun generate-needleman-wunsch-scores
       (seq-down-rows seq-across-cols sub-function gap-function)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (declare (type simple-vector seq-down-rows seq-across-cols))

  (let* ((nrows (length seq-down-rows)) 
         (ncols (length seq-across-cols))
	 (colmax 0.0) (rowmax 0.0) (max 0.0)
         (no-gap-score 0.0) 
         (subval 0.0)
         collist rowlist pointlist 
         )
    (declare (single-float colmax rowmax max no-gap-score subval))
    (declare (fixnum nrows ncols))

    (adjust-nw-arrays-to-size nrows ncols)

    ;; We are generating new scores, so cached paths will no 
    ;; longer be valid.
    (clrhash *paths-hash-table*)

    (let ((scoremat *scoremat*) (pointmat *pointermat*))
      (declare (type (simple-array t 2) pointmat))
      (declare (type (simple-array single-float 2) scoremat))

      ;; Make sure arrays are initialized.
      (dotimes (i nrows) 
        (dotimes (j ncols) 
	  (setf (aref scoremat i j) 0.0)
	  (setf (aref pointmat i j) nil)))

      ;; For each row
      (dotimes (i nrows) 
        (declare (fixnum i))

        ;; Across each column
        (dotimes (j ncols)
          (declare (fixnum j))

          ;; Get the residues at (I,J) and determine their match value
          ;; via the SUBSTITUTION FUNCTION.  This is SUBVAL.
          (setq subval (funcall sub-function (aref seq-down-rows i)
                                (aref seq-across-cols j)))

          ;; Get the previously computed score at SCORE(I-1,J-1)
          ;; This is the NO-GAP-SCORE.
                    
          (if (and (> i 0) (> j 0))
              (setq no-gap-score 
                    (aref scoremat (the fixnum (1- i)) (the fixnum (1- j))))
            (setq no-gap-score 0.0))

          ;; Find the best score in column j-1

          (setq colmax -100.0)
          (setq collist nil)
          (when (> j 0)
            ;; For all points in column j-1 ranging from row 0 to
            ;; row i-2, but in reverse (right to left)
            (loop for x from 2 to i do
                  (let* ((gaplen (the fixnum (1- x)))
                         (currow (the fixnum (- i x)))
                         (curcol (the fixnum (1- j)))
                         ;; W(k), the penalty function for a gap of length k.
                         (penalty (funcall gap-function gaplen))
                         ;; The previously computed score for this cell.
                         (col-point-score (aref scoremat currow curcol))
                         ;; The score for this cell to be maxxed against
                         ;; all the others in the same column
                         (colscore (+ col-point-score penalty)))
                    (declare (fixnum x gaplen currow curcol))
                    (declare (single-float penalty col-point-score colscore))
                    ;; Keep track of all points which have the maximum score,
                    ;; and what the maximum score is.
                    (when (>= colscore colmax)
                      (if (= colscore colmax)
                          (setq collist (cons (nwpoint currow curcol) collist))
                        (setq collist (list (nwpoint currow curcol))))
                      (setq colmax colscore)
                      ))))

          ;; colmax now contains the best score in column j-1.
          ;; collist contains all points in the column j-1 that equal colmax.

          ;; Find the best score in row i-1
          ;; Algorithm is identical to above, but with row and column switched.

          (setq rowmax -100.0)
          (setq rowlist nil)
          (when (> i 0)
            (loop for y from 2 to j do
                  (let* ((gaplen (the fixnum (1- y)))
                         (currow (the fixnum (1- i)))
                         (curcol (the fixnum (- j y)))
                         (penalty (funcall gap-function gaplen))
                         (row-point-score (aref scoremat currow curcol))
                         (rowscore (+ row-point-score penalty)))
                    (declare (fixnum y gaplen currow curcol))
                    (declare (single-float row-point-score penalty rowscore))
                    (when (>= rowscore rowmax)
                      (if (= rowscore rowmax)
                          (setq rowlist (cons (nwpoint currow curcol) rowlist))
                        (setq rowlist (list (nwpoint currow curcol))))
                      (setq rowmax rowscore)
                      ))))

          ;; rowmax now contains the best score in row i-1.
          ;; rowlist contains all points in the row i-1 that equal rowmax.

          ;; Now we have three potential best scores, NO-GAP-SCORE,
          ;; ROWMAX and COLMAX.  One or more of these values may be equal,
          ;; and each of these values is associated with a set of
          ;; points that have this value.  

          ;; So figure out THE maximum value, and append together all
          ;; the points that equal this maximum value.  Store this
          ;; pointlist away at index (I,J) is the POINTMAT array.

          ;; Finally, set SCORE(I,J) equal to this maximum value + SUB(I,J)
          ;; as the algorithm dictates.  

          (setq pointlist nil)
          (setq max (max no-gap-score rowmax colmax))

          (if (and (= max no-gap-score) (> i 0) (> j 0))
              (setq pointlist (cons (nwpoint (1- i) (1- j)) pointlist)))
          (if (= max rowmax) (setq pointlist (append pointlist rowlist)))
          (if (= max colmax) (setq pointlist (append pointlist collist)))

          (setf (aref pointmat i j) pointlist)
          (setf (aref scoremat i j) (+ subval max))

          )) ; Close loops 

      )))

     

;;; GAP-OPEN and GAP-DELTA must be non-negative numbers!
;;; (So they represent the magnitude of the penalty)
;;; Penalty function is - ((N*GAP-DELTA) + GAP-OPEN)

(defun generate-needleman-wunsch-scores-linear-gap-function
       (seq-down-rows seq-across-cols sub-function gap-open gap-delta)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (declare (type simple-vector seq-down-rows seq-across-cols))

  (let* ((nrows (length seq-down-rows)) 
         (ncols (length seq-across-cols))
         (large-negative-float (float most-negative-fixnum 0.0))
	 (rowmax 0.0) (themax 0.0) (no-gap-score 0.0) (subval 0.0)
         (colmax (make-array (list ncols) :element-type 'single-float))
         (colmax-points (make-array (list ncols)))
         (backpoints nil)
         rowmax-points 
         )
    (declare (single-float rowmax themax no-gap-score subval))
    (declare (fixnum nrows ncols))
    (declare (type (simple-array single-float 1) colmax))
    (declare (type simple-vector colmax-points))

    (adjust-nw-arrays-to-size nrows ncols)

    ;; We are generating new scores, so cached paths will no 
    ;; longer be valid.
    (clrhash *paths-hash-table*)

    (let ((scoremat *scoremat*) (pointmat *pointermat*))
      (declare (type (simple-array t 2) pointmat))
      (declare (type (simple-array single-float 2) scoremat))

      ;; Make sure arrays, etc. are initialized.
      (dotimes (i nrows) 
        (dotimes (j ncols) 
	  (setf (aref scoremat i j) 0.0)
	  (setf (aref pointmat i j) nil)))

      (dotimes (j ncols)
        (setf (aref colmax j) large-negative-float)
        (setf (aref colmax-points j) nil))

      ;; For each row
      (dotimes (i nrows) 
        (declare (fixnum i))

        (setq rowmax large-negative-float)
        (setq rowmax-points nil)

        ;; Across each element of the row
        (dotimes (j ncols)
          (declare (fixnum j))

          ;; Get the residues at (I,J) and determine their match value
          ;; via the SUBSTITUTION FUNCTION.  This is SUBVAL.
          (setq subval (funcall sub-function (aref seq-down-rows i)
                                (aref seq-across-cols j)))

          ;; Get the previously computed score at SCORE(I-1,J-1)
          ;; This is the NO-GAP-SCORE.
                    
          (if (and (> i 0) (> j 0))
              (setq no-gap-score 
                    (aref scoremat (the fixnum (1- i)) (the fixnum (1- j))))
            (setq no-gap-score 0.0))

          ;; Find the best score for all elements in column j-1 above
          ;; row i-1, and keep a list of the points in that column that
          ;; match that score.  The previous best score for all elements
          ;; in column j-1 above row i-2 is kept in COLMAX[j], and the
          ;; list of points that equal that score is in COLMAX-POINTS[j].

          (unless (or (< i 2) (zerop j))
            (let* ((newi (the fixnum (- i 2))) 
                   (newj (the fixnum (- j 1)))
                   (newpoint (nwpoint newi newj))
                   (addgapval (- (aref colmax j) gap-delta))
                   (newspotval (- (aref scoremat newi newj) gap-open)))
              (setf (aref colmax j) (max addgapval newspotval))
              (cond
               ((= addgapval newspotval)
                (push newpoint (aref colmax-points j)))
               ((> newspotval addgapval) 
                (setf (aref colmax-points j) (list newpoint)))
               ((< newspotval addgapval) nil)
               )
              (unless (aref colmax-points j) (error "Internal error col"))
              ))

          ;; Find the best score for all elements in row i-1 to the
          ;; left of column j-1, and keep a list of the
          ;; points in that row that match that score.
          ;; The best score is either the previous best score for the row
          ;; minus the penalty for one more space in the gap, or the score for
          ;; the new element in the row (at j-2) minus the penalty for 
          ;; starting a gap (since if we go back from (i,j) to (i-1,j-2)
          ;; we've opened a one-space gap)
          
          (unless (or (zerop i) (< j 2))
            (let* ((newi (the fixnum (- i 1))) 
                   (newj (the fixnum (- j 2)))
                   (newpoint (nwpoint newi newj))
                   (addgapval (- rowmax gap-delta))
                   (newspotval (- (aref scoremat newi newj) gap-open)))
              (setq rowmax (max addgapval newspotval))
              (cond
               ((= addgapval newspotval)
                (push newpoint rowmax-points))
               ((> newspotval addgapval)
                (setq rowmax-points (list newpoint)))
               ((< newspotval addgapval) nil))
              (unless rowmax-points (error "Internal error row"))
              ))

          (setf backpoints nil)
          (setq themax (max no-gap-score rowmax (aref colmax j)))
          (when (and (plusp i) (plusp j))
            (when (= themax no-gap-score)
              (push (nwpoint (1- i) (1- j)) backpoints))
            (when (= themax rowmax)
              (setq backpoints (append backpoints rowmax-points)))
            (when (= themax (aref colmax j))
              (setq backpoints (append backpoints (aref colmax-points j))))
            (when (null backpoints) (error "Internal error 27"))
            )
          
          (setf (aref pointmat i j) backpoints)
          (setf (aref scoremat i j) (+ themax subval))

          )) ; Close loops 

      )))


;;;; FINDING THE BEST PATHS GIVEN THE GENERATED MATRIX.


;;; Returns a list of all the points in last row and/or last column that 
;;; have the maximum score.

(defun find-best-endpoints ()
  (let* ((s *scoremat*)
         (nrows (array-dimension s 0))
         (ncols (array-dimension s 1))
         (lastrow (1- nrows))
         (lastcol (1- ncols))
         (bestscore (float most-negative-fixnum 0.0))
         (score 0.0)
         (pointlist nil))
    (declare (fixnum nrows ncols lastrow lastcol))
    (declare (single-float bestscore score))
    (declare (type (array single-float 2) s))
    ;; First search the last column for the highest score.
    (dotimes (currow nrows)
      (setq bestscore (max bestscore (aref s currow lastcol))))
    ;; Then search the last row.
    (dotimes (curcol ncols)
      (setq bestscore (max bestscore (aref s lastrow curcol))))
    ;; Okay, we have the best score.  Cons up a list of
    ;; all points that are equal to this best score.
    (dotimes (currow nrows)
      (setq score (aref s currow lastcol))
      (when (= score bestscore) (push (nwpoint currow lastcol) pointlist)))
    ;; Careful not to get bottom-right point twice!
    (dotimes (curcol (1- ncols))
      (setq score (aref s lastrow curcol))
      (when (= score bestscore) (push (nwpoint lastrow curcol) pointlist)))
    pointlist
    ))

;; Return a list of all optimal paths ending at (I,J).
;; The paths are lists of NWPOINTs.

;; For any given point (I,J), there is a unique set of best paths
;; that go back from it.  If two paths converge on point (I,J), then
;; from that point backwards they will encompass the same set of paths.
;; So once we compute the set of best paths from (I,J), we store that
;; set in a hash table and retrieve it if we ever hit (I,J) again.
;; This saves mucho computation for large matrices.

(defun get-best-paths (i j pointmat)
  (setq *all-points-paths*
    (mapcar 
     #'reverse
     (get-best-paths-internal i j pointmat)
     )))

(defun get-best-paths-internal (i j pointmat)
  (declare (fixnum i j))
  (declare (type (simple-array t 2) pointmat))
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  ;; If (I,J) is already hashed were done.
  (let ((hashval (gethash (nwpoint i j) *paths-hash-table*)))
    (when hashval (return-from get-best-paths-internal hashval)))
  (let* ((backcol 0) (backrow 0)
         (curpoint (nwpoint i j))
	 (best-paths nil) (result nil))
    (declare (fixnum backcol backrow))
    (declare (type nwpoint curpoint))
    (setq result
      (cond 
       ;; The path begins HERE! (since there is no previous point)
       ((null (aref pointmat i j)) (list (list (nwpoint i j))))
       (t 
	;; For each backpointer, find the best paths from there, then
	;; chain the current point on to all those paths.
	(loop for x in (aref pointmat i j) do
	      (setq backrow (nwpoint-row x))
	      (setq backcol (nwpoint-col x))
	      (setq best-paths 
                    (get-best-paths-internal backrow backcol pointmat))
            append 
            (mapcar #'(lambda (path) (cons curpoint path)) best-paths)
            ))))
    (setf (gethash curpoint *paths-hash-table*) result)
    result
    ))


;;;; PATHS TO ALIGNMENTS FUNCTIONALITY


;;; Returns a list of paths starting at (I,J) and traversing pointmat
;;; up and left toward the origin.  A path is a list of points.

;; Utility functions.  Not used elsewhere.

(defun pointslist-to-unpacked-pointslist (pointslist)
  (mapcar #'(lambda (p) (nwpoint-list p)) pointslist))

(defun unpacked-pointslist-to-residuelist (upl seq-across-cols seq-down-rows)
  (mapcar
   #'(lambda (lp) 
       (list (aref seq-down-rows (first lp)) 
             (aref seq-across-cols (second lp))))
   upl))

(defun points-to-residues (pointslist seq-across-cols seq-down-rows)
  (unpacked-pointslist-to-residuelist
   (pointslist-to-unpacked-pointslist pointslist)
   seq-across-cols seq-down-rows
   ))

;;; Main routine.
;;; Given a path consisting of an ordered set of points from the
;;; SCORE matrix, construct the alignment specified by those points.
;;; The path given is assumed to be ordered from top-left to bottom-right.
;;; Returns three values:
;;;  1.  The common alignment
;;;  2.  Any sequence elements prior to the common alignment
;;;  3.  Any sequence elements after the common alignment.

(defun path-to-alignment (path seq-down-rows seq-across-cols)
  (declare (simple-vector seq-down-rows seq-across-cols))

  (let* ((nrows (length seq-down-rows)) 
         (ncols (length seq-across-cols))
         (last-row (1- nrows))
         (last-col (1- ncols))
	 (alignment nil)
         (pre-alignment nil)
         (post-alignment nil)
	 (previous-point nil)
         (first-point (first path))
         (last-point (first (last path)))
         )
    (declare (fixnum nrows ncols last-row last-col))
    (declare (type nwpoint first-point last-point))
    
    (labels ((sdr-residue (i) (aref seq-down-rows i))
             (sac-residue (j) (aref seq-across-cols j))
             (add-current-alignment (row col)
               (push (list (sdr-residue row) (sac-residue col)) alignment)
               ))

      ;; Special case stuff before first point (PRE-ALIGNMENT)

      (let ((row (nwpoint-row first-point))
            (col (nwpoint-col first-point)))
        (declare (fixnum row col))
        (cond
         ((and (zerop row) (zerop col)) nil)
         ((zerop row)
          (dotimes (curcol col)
            (push (list '- (sac-residue curcol)) pre-alignment)))
         ((zerop col)
          (dotimes (currow row)
            (push (list (sdr-residue currow) '*) pre-alignment)))
         (t (error "Internal error 1"))
         ))
      (setq pre-alignment (nreverse pre-alignment))

      ;; Special case stuff beyond last point (POST-ALIGNMENT)
      ;; and do last point itself

      (let ((row (nwpoint-row last-point))
            (col (nwpoint-col last-point)))
        (declare (fixnum row col))
        (cond
         ((and (= row last-row) (= col last-col)) nil)
         ((= row last-row)
          (do ((curcol last-col (1- curcol))) ((= curcol col))
            (push (list '- (sac-residue curcol)) post-alignment)))
         ((= col last-col)
          (do ((currow last-row (1- currow))) ((= currow row))
            (push (list (sdr-residue currow) '*) post-alignment)))
         (t (error "Internal error 2")))
        (add-current-alignment row col)
        (setq previous-point last-point))


      ;; Do all points but last one in reverse order.

      (setq path (reverse path))
      (setq path (cdr path))

      (do ((points path (cdr points))) ((null points))
        (let* ((point (first points))
               (currow (nwpoint-row point))
               (curcol (nwpoint-col point))
               (prevrow (nwpoint-row previous-point))
               (prevcol (nwpoint-col previous-point)))
          (declare (fixnum currow curcol prevrow prevcol))
          (declare (type nwpoint point))
          (cond
           ((and (= curcol (1- prevcol)) (= currow (1- prevrow))) nil)
           ((= curcol (1- prevcol))
            (do ((i (1- prevrow) (1- i))) ((= i currow))
              (push (list (sdr-residue i) '*) alignment)))
           ((= currow (1- prevrow))
            (do ((j (1- prevcol) (1- j))) ((= j curcol))
              (push (list '- (sac-residue j)) alignment)))
           (t (error "Internal error 3")))
          (add-current-alignment currow curcol)
          (setq previous-point point)
          ))

      (values alignment pre-alignment post-alignment)

      )))


;;;; PRETTY-PRINT ROUTINES


(defun pp-sub-matrix (sub ordered-constituent-set)
  (terpri) (terpri)
  (format t "Substitution matrix = ~%~%")
  (let* ((size (first (array-dimensions sub))))
    (format t "     ")
    (dolist (c ordered-constituent-set)
      (format t "   ~a  " c))
    (format t "~%~%")
    (dotimes (i size)
      (format t "~a : " (elt ordered-constituent-set i))
      (dotimes (j size)
	(format t "~6,' d" (aref sub i j)))
      (format t "~%~%"))))

(defun pp-score-matrix (seq-down-rows seq-across-cols)
  (terpri) (terpri)
  (format t "Score matrix = ~%~%")
  (let* ((dims (array-dimensions *scoremat*))
	 (rows (first dims))
	 (cols (second dims)))
    (format t "     ")
    (dotimes (c cols)
      (format t "   ~a " (elt seq-across-cols c)))
    (format t "~%~%")
    (dotimes (i rows)
      (format t "~a : " (elt seq-down-rows i))
      (dotimes (j cols)
	(format t "~5,' d" (aref *scoremat* i j)))
      (format t "~%~%"))))

;;; Given a list of points describing a path, convert them to an
;;; alignment and pretty print that alignment.

(defun pp-point-path (path seq-down-rows seq-across-cols &optional (vbars t))
  (setq seq-down-rows (coerce seq-down-rows 'simple-vector))
  (setq seq-across-cols (coerce seq-across-cols 'simple-vector))
  (multiple-value-bind (common pre post)
      (path-to-alignment path seq-down-rows seq-across-cols)
    (flet ((print-seq (seq accessor print-gaps?)
	     (let ((pseq (mapcar accessor seq)))
	       (dolist (elem pseq)
	       (format t "~A"
		       (if (and (not print-gaps?) 
				(or (eql elem '*) (eq elem '-)))
			   " " elem))))))
      (format t "~&~%")
      (print-seq pre #'first nil)
      (print-seq common #'first t)
      (print-seq post #'first nil)
      (terpri)
      (when vbars
        (dolist (pair pre) (declare (ignore pair)) (format t " "))
	(dolist (pair common)
          (format t "~A" (if (eql (first pair) (second pair)) "|" " ")))
        (terpri))
      (print-seq pre #'second nil)
      (print-seq common #'second  t)
      (print-seq post #'second nil)
      (terpri)
      (terpri)
      )))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; Tests


#|
(defparameter *new-alignments* nil)

(defun verify-new-nw (seq-across-cols seq-down-rows)
  (let ((*sequence-a* seq-across-cols)
	(*sequence-b* seq-down-rows) 
	(*nw-verbose* nil))
    (let (npaths-old npaths-new old-paths new-paths)
      (maybetime 
          (*time-old?* :n 10 :notice "Old version of complete NW")
        (test))
      (setq npaths-old (length *allpaths*))
      (setq old-paths *allpaths*)
      (maybetime
          (*time-new?* :n 1 :notice "New version of complete NW")
        (nw-test))
      (setq npaths-new (length *all-points-paths*))
      (setq new-paths *all-points-paths*)
      (unless (= npaths-old npaths-new)
        (format t "~%;; Oops!  Old yields ~D paths, new ~D paths~%"
                npaths-old npaths-new))
      ;; Convert the point paths back to lists of residues for comparison
      (let ((new-path-alignments
             (mapcar 
              #'(lambda (x) (path-to-alignment x *sequence-a* *sequence-b*))
              new-paths)))
        (setq *new-alignments* new-path-alignments)
        (dolist (oldpath old-paths)
          (unless (member oldpath new-path-alignments :test #'equalp)
            (error "Oops: paths aren't the same~%~S" oldpath))
	  ;;(print "path the same")
	  )))))
|#

(defparameter zero-gpf 
  #'(lambda (n) (declare (ignore n)) 0.0))
(defparameter standard-gpf 
  #'(lambda (n) (- (+ 1.0 (* (- n 1) 0.25)))))

(defun test-verbose (&optional (down *testseq-a*) (across *testseq-b*))
  (let ((*time-nw?* nil) (*time-bp?* nil) 
	(*time-new?* nil) (*nw-verbose* t))
    (run-nw down across) ;; :gap-function zero-gpf)
    ))



;;; For test code in tests/match-tests.lisp

(defun nw-common (down across algorithm)
  (let ((*nw-verbose* nil) 
        (dv (coerce down 'simple-vector))
        (av (coerce across 'simple-vector))
        (gap-function #'(lambda (n) (declare (ignore n)) 0.0)))
    (ecase algorithm
      (:general 
       (run-nw down across :gap-function gap-function :show-alignments? nil))
      (:linear-gap 
       (run-nw down across
               :gap-open-penalty 0.0
               :gap-space-penalty 0.0
               :show-alignments? nil)))
    (unless (= (length *all-points-paths*) 1)
      (error "Oops: more than one optimal path!"))  
    (path-to-alignment (first *all-points-paths*) dv av)
    ))
  

(defun test-nw (&optional (verbose nil))
  (let ((*time-nw?* nil) (*time-bp?* nil) 
	(*time-new?* nil) (*nw-verbose* verbose)
        (gap-function #'(lambda (n) (declare (ignore n)) 0.0))
        )
    (labels 
        ((test-one-algorithm (name down across align algorithm)
           (format t "~&Testing ~A ~S..." algorithm name) (force-output t)
           (ecase algorithm
             (:general
              (run-nw down across 
                      :gap-function gap-function 
                      :show-alignments? nil))
             (:linear-gap
              (run-nw down across 
                      :gap-open-penalty 0.0
                      :gap-space-penalty 0.0
                      :show-alignments? nil)))
           (unless (= (length *all-points-paths*) 1)
             (error "Oops: more than one optimal path!"))
           (multiple-value-bind (common pre post)
               (path-to-alignment 
                (first *all-points-paths*) 
                (coerce down 'simple-vector)
                (coerce across 'simple-vector))
             (declare (ignore pre post))
             (unless (equalp common align)
               (format t "Oops: Actual path not same as expected path~%")
               (format t "Actual  : ~A~%" common)
               (format t "Expected: ~A~%" align)
               (format t "Points list: ~A~%" (first *all-points-paths*))
               (error "test failed")))
           (format t "OK~%"))
         (test-one-path (name down across align)
           (test-one-algorithm name down across align :general)
           (test-one-algorithm name down across align :linear-gap)))
      (test-one-path 
       "IDENTITY" 
       '(A C G T A C G T)
       '(A C G T A C G T) 
       '((A A) (C C) (G G) (T T) (A A) (C C) (G G) (T T)))
      (test-one-path
       "ONE GAP DOWN ROW" 
       '(A C G A C G T)
       '(A C G T A C G T)
       '((A A) (C C) (G G) (- T) (A A) (C C) (G G) (T T)))
      (test-one-path
       "ONE GAP ACROSS COL" 
       '(A C G T A C G T)
       '(A C G A C G T)
       '((A A) (C C) (G G) (T *) (A A) (C C) (G G) (T T)))
      (test-one-path
       "TWO GAP DOWN ROW" 
       '(A C G C G T)
       '(A C G T A C G T)
       '((A A) (C C) (G G) (- T) (- A) (C C) (G G) (T T)))
      (test-one-path
       "TWO GAP ACROSS COL" 
       '(A C G T A C G T)
       '(A C G C G T)
       '((A A) (C C) (G G) (T *) (A *) (C C) (G G) (T T)))
      (test-one-path
       "DOUBLE GAP ACROSS COL" 
       '(A C G T A C G T)
       '(A G T A G T)
       '((A A) (C *) (G G) (T T) (A A) (C *) (G G) (T T)))
      (test-one-path
       "ONE GAP EACH"
       '(A C G T T G C A)
       '(A G T T G A C A)
       '((A A) (C *) (G G) (T T) (T T) (G G) (- A) (C C) (A A)))
      (test-one-path
       "DOWN ROW SHIFTED"
       '(C G T A C G T A C)
       '(A C G T A C G T A)
       '((C C) (G G) (T T) (A A) (C C) (G G) (T T) (A A)))
      (test-one-path
       "ACROSS COLUMN SHIFTED"
       '(A C G T A C G T)
       '(C G T A C G T A)
       '((C C) (G G) (T T) (A A) (C C) (G G) (T T)))
      )))

(defun test-optimized-vs-general (title s1 s2 gap-open gap-delta)
  (format t ";; Testing ~A..." title)
  (let ((gap-function #'(lambda (n) (- (+ gap-open (* (- n 1) gap-delta)))))
        (len1 (length s1)) (len2 (length s2))
        non-optimized-scores optimized-scores
        non-optimized-backpointers optimized-backpointers
        )
    ;; Run non-optimized algorithm
    (run-nw s1 s2 :gap-function gap-function :show-alignments? nil)
    ;; Copy score matrix
    (setq non-optimized-scores
          (make-array (list len1 len2) :element-type 'single-float))
    (setq non-optimized-backpointers (make-array (list len1 len2)))
    (dotimes (i len1) 
      (dotimes (j len2)
        (setf (aref non-optimized-scores i j) (aref *scoremat* i j))
        (setf (aref non-optimized-backpointers i j) (aref *pointermat* i j))
        ))
    ;; Run optimized algorithm
    (run-nw s1 s2 
            :gap-open-penalty gap-open
            :gap-space-penalty gap-delta
            :show-alignments? nil)
    (setq optimized-scores *scoremat*)
    (setq optimized-backpointers *pointermat*)
    ;; Test that two score matrices are equivalent
    (dotimes (i (length s1))
      (dotimes (j (length s2))
        (let ((a1 (aref non-optimized-scores i j))
              (a2 (aref optimized-scores i j))
              (p1 (aref non-optimized-backpointers i j))
              (p2 (aref optimized-backpointers i j)))
          (unless (< (abs (- a1 a2)) 0.001)
            (error "Oops:  Score matrices not the same!!"))
          (unless (equalp (sort (copy-list p1) '<) (sort (copy-list p2) '<))
            (error "Oops: Backpointers not the same at (~D,~D), ~S, ~S" 
                   i j p1 p2
                   )))))
    (format t "OK~%")
    ))
      
(defun test-optimized ()
  (terpri)
  (let ((*nw-verbose* nil))
    (test-optimized-vs-general 
     "Very simple 4x4 no-gap penalty" '(A C G T) '(A C C T) 0.0 0.0)
    (test-optimized-vs-general 
     "Very simple 4x4 standard gap penalty" '(A C G T) '(A C C T) 1.0 0.25)
    (test-optimized-vs-general
     "12x12 standard gap penalty"
     '(A C C G A T T C A G G C)
     '(C C G T A T C G A A A A)
     1.0 0.25)
    (test-optimized-vs-general
     "Standard test sequences with no gap penalty"
     *testseq-a* *testseq-b* 0.0 0.0)
    (test-optimized-vs-general
     "Standard test sequences with standard gap penalty"
     *testseq-a* *testseq-b* 1.0 0.25)
    ))

(defun time-nw (min max)
  (let ((down '(A C G A T T G A A C))
        (across '(C A T T A A A A G C))
        (*time-new?* t)
        (*time-nw?* nil)
        (*time-bp?* nil)
        (*nw-verbose* nil))
    (unless (>= min 10) (setq min 10))
    (flet ((add-base ()
	     (push (elt '(A C G T) (random 4)) down)
	     (push (elt '(A C G T) (random 4)) across)))
      (do ((j 10 (1+ j))) ((>= j min)) (add-base))
      (do ((j min (1+ j))) ((>= j max))
        (let ((notice (format nil "Timing length ~D" (length down))))
          (maybetime (*time-new?* :n 1 :notice notice)  
            (run-nw down across 
                    :gap-function zero-gpf 
                    :show-alignments? nil))
          (add-base)
	  )))))


(defun demo-nw ()
  (format t "~&~%~%;; Sequence A:  ")
  (dolist (a *testseq-a*) (format t "~A " a)) (terpri)
  (format t "~%;; Sequence B:  ")
  (dolist (b *testseq-b*) (format t "~A " b)) (terpri) (terpri)
  (let ((*nw-verbose* nil))
    (format t "~&~%;; Sample alignments with no gap penalties:~%~%")
    (run-nw *testseq-a* *testseq-b*
            :gap-function zero-gpf
            :show-alignments? t
            :max-alignments-to-show 2)
    (format t "~%;; Sample alignments with constant 0.5 penalty~%~%")
    (run-nw *testseq-a* *testseq-b*
            :gap-open-penalty 0.5
            :gap-space-penalty 0.0
            :show-alignments? t
            :max-alignments-to-show 2)
    (format t "~%;; Sample alignments with 2.0+0.25*(n-1) penalty~%~%")
    (run-nw *testseq-a* *testseq-b*
            :gap-open-penalty 2.0
            :gap-space-penalty 0.25
            :show-alignments? t
            :max-alignments-to-show 2)
    ))

