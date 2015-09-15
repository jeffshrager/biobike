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

;;;; Implemention of a general gap-allowed Needleman-Wunsch string 
;;;; alignment dynamic programming algorithm.

;;;; The recursion formula used here is:

#|
> 
>SCORE(i,j) = 
> 
> MAX { SCORE(i-1,j-1) + SUBST(i,j), 
> 
>       MAX    SCORE(k,j) - W(i-k) 
>     k=0,i-1 
> 
>       MAX    SCORE(i,k) - W(j-k) 
>     k=0,j-1 
|#

;;;; This is different than the recursion formula used in the demo
;;;; code.  This difference is discussed in the file match-essay.txt.

;;;; Further differences are that in this code the matrices are one row
;;;; and one column larger.  This allows the specification of
;;;; initial conditions, and this ability is provided the user herein.
;;;; The algorithm to produce an alignment from a backpath is different,
;;;; owing to the somewhat different semantics the change in recursion
;;;; formula implies.  Finally, only the general cubic-time algorithm is 
;;;; implemented.

;;;; The input to the kernel alignment routine are:

;;;;   1.  Two sequences, SEQA and SEQB, as either lists or vectors of 
;;;;       one-character symbols which are elements of an alphabet.
;;;;   2.  A function, SUBST(X,Y), which returns the value of aligning
;;;;       symbols X and Y, each elements of the alphabet in 1.
;;;;   3.  A function INIT of (I,J) defining the initial conditions, which 
;;;;       is used to initialize the first row and first column of the 
;;;;       SCORE matrix.
;;;;   4.  A function defining the value of a gap in alignment of length x,
;;;;       known as W(x).
;;;;   5.  Whether the SCORE MATRIX computation is to be done in fixnum
;;;;       (signed-byte 16) or single-float arithmetic.  Currently,
;;;;       only fixnum arithmetic is allowed.

;;;; The immediate output is an array of pointer lists, POINTERMAT, 
;;;; and a list of coordinates into the POINTERMAT array.  Each coordinate
;;;; represent the last alignment of two symbols, one from each sequence.
;;;; The POINTERMAT pointers at coordinate (I,J) point back to coordinates
;;;; of lesser I and/or J, determining various complete alignments of the
;;;; two sequences.

;;;; The toplevel routine is RUN-GLOBAL-ALIGNMENT, which calls the kernel
;;;; routine and then possibly computes and outputs some number of
;;;; optimal alignments.

;;; By convention, index I represents SEQA which is thought of as displayed
;;; down the left side (down the rows) of the SCORE matrix, while J represents
;;; SEQB which is thought of as displayed across the top (across the columns)
;;; of the SCORE matrix.  Index I is always the first index, while index J
;;; is always the second index.

;;; The SCORE matrix is LENGTH(SEQA)+1 by LENGTH(SEQB)+1 elements.  The
;;; extra row and column represent the initial conditions.

;;; A representation, given SEQA = A C C T,  SEQB = C A T, with 
;;; an initialization function INIT(I,J) = 0.

;;;       C  A  T
;;;    0  0  0  0
;;; A  0
;;; C  0
;;; C  0
;;; T  0


(defvar *score-matrix* nil)
(defvar *pointer-matrix* nil)

(defvar *maximum-score-points* nil)
(defvar *maximum-score* nil)

(defvar *backpaths-hash-table* (make-hash-table :test #'eql))
(defvar *all-paths-to-point* nil)
(defvar *all-points-paths* nil)

(defparameter *align-verbose* t)

;; Defined in NEEDLEMAN-WUNSCH.LISP
;; (defvar *testseq-a* '(T T G A C A C C C T C C C A A T T G T A))
;; (defvar *testseq-b* '(A C C C C A G G C T T T A C A C A T))


;;; Abstraction/optimization of a 2-dimensional ARRAY LOCATION,
;;; which is an index (ROW COL) into a matrix.  We pack
;;; ROW and COL into a single fixnum.

(defmacro aloc (row col) 
  `(the fixnum (+ (the fixnum (ash ,col 16)) ,row)))
(defmacro aloc-row (p) `(logand (the fixnum ,p) #x0000FFFF))
(defmacro aloc-col (p) `(ash (the fixnum ,p) -16))
(defmacro aloc-list (p) 
  (let ((psym (gensym "ALOC-")))
    `(let ((,psym ,p))
       (list (aloc-row ,psym) (aloc-col ,psym)))))
(deftype aloc () 'fixnum)


(defun maybe-create-score-matrix (nrows ncols type)
  (unless (or (eq type 'single-float) (equal type '(signed-byte 16)))
    (error "Cannot use type ~S for score matrix array element type" type))
  (setq *score-matrix*
        (or (and (boundp '*score-matrix*) 
                 (arrayp *score-matrix*)
                 (equal (array-dimensions *score-matrix*) (list nrows ncols))
                 *score-matrix*)
            (make-array (list nrows ncols) :element-type type)
            )))

(defun maybe-create-pointer-matrix (nrows ncols)
  (setq *pointer-matrix*
        (or (and (boundp '*pointer-matrix)
                 (arrayp *pointer-matrix*)
                 (equal (array-dimensions *pointer-matrix*) (list nrows ncols))
                 *pointer-matrix*)
            (make-array (list nrows ncols))
            )))


(defun global-alignment-kernel

       (seqa seqb subst-function init-function gap-function
             integer-or-float)

  (let ((lena (length seqa)) 
        (lenb (length seqb))
        (prev-row 0)
        (prev-col 0)
        (gaplen 0)
        (backpointer-list nil)
        (maxlist nil)
        ij-alignment-score diag-score max-score aval gapscore score
        residue-a residue-b
        )
    (declare (fixnum lena lenb prev-row prev-col gaplen))

    (maybe-create-score-matrix (1+ lena) (1+ lenb) integer-or-float)
    (maybe-create-pointer-matrix (1+ lena) (1+ lenb))
  
    (let ((scores *score-matrix*) (pointers *pointer-matrix*))
      (declare (type (simple-array t 2) pointers))

      ;; Initialize pointer matrix.

      (loop for i from 1 to lena do 
            (setf (aref pointers i 0) (list (aloc (1- i) 0))))
      (loop for j from 1 to lenb do 
            (setf (aref pointers 0 j) (list (aloc 0 (1- j)))))
      (loop for i from 1 to lena do
            (loop for j from 1 to lenb do
                  (setf (aref pointers i j) nil)))

      ;; Initialize score matrix.

      (dotimes (i (1+ lena)) 
        (setf (aref scores i 0) (funcall init-function i 0)))
      (dotimes (j (1+ lenb))
        (setf (aref scores 0 j) (funcall init-function 0 j)))

      ;; Down each row, across each column ...

      (loop for i fixnum from 1 to lena do
            (setq prev-row (the fixnum (1- i)))
            (loop for j fixnum from 1 to lenb do
                  (setq prev-col (the fixnum (1- j)))
                  
                  ;; Process cell diagonally up and left of current cell
                  ;; (Sequence indices start at 0, matrix indices start at 1)
                  (setq residue-a (aref seqa (1- i)))
                  (setq residue-b (aref seqb (1- j)))
                  (setq ij-alignment-score 
                        (funcall subst-function residue-a residue-b))
                  (setq diag-score (aref scores prev-row prev-col))
                  (setq max-score (+ ij-alignment-score diag-score))
                  (setq backpointer-list (list (aloc prev-row prev-col)))

                  ;; Process all cells to left of current cell
                  (loop for k fixnum from 0 to prev-col do
                        (setq aval (aref scores i k))
                        (setq gaplen (the fixnum (- j k)))
                        (setq gapscore (funcall gap-function gaplen))
                        (setq score (+ aval gapscore))
                        (cond
                         ((> score max-score)
                          (setq max-score score)
                          (setq backpointer-list (list (aloc i k))))
                         ((= score max-score)
                          (push (aloc i k) backpointer-list))
                         ))

                  ;; Process all cells above current cell.
                  (loop for k fixnum from 0 to prev-row do
                        (setq aval (aref scores k j))
                        (setq gaplen (the fixnum (- i k)))
                        (setq gapscore (funcall gap-function gaplen))
                        (setq score (+ aval gapscore))
                        (cond
                         ((> score max-score)
                          (setq max-score score)
                          (setq backpointer-list (list (aloc k j))))
                         ((= score max-score)
                          (push (aloc k j) backpointer-list))
                         ))

                  ;; Set score of current cell along with its backpointer list.
                  (setf (aref scores i j) max-score)
                  (setf (aref pointers i j) backpointer-list)

                  ))

      ;; Find all maxima along right and bottom edges.

      (setq maxlist (list (aloc lena lenb)))
      (setq max-score (aref scores lena lenb))

      (loop for i from 1 to (1- lena) do
            (setq score (aref scores i lenb))
            (cond
             ((> score max-score)
              (setq max-score score)
              (setq maxlist (list (aloc i lenb))))
             ((= score max-score)
              (push (aloc i lenb) maxlist))))
      (loop for j from 1 to (1- lenb) do
            (setq score (aref scores lena j))
            (cond
             ((> score max-score)
              (setq max-score score)
              (setq maxlist (list (aloc lena j))))
             ((= score max-score)
              (push (aloc lena j) maxlist))))

      (setq *maximum-score-points* maxlist)
      (setq *maximum-score* max-score)
      (values)

      )))


(defun create-subst-function
    (ordered-alphabet
     &key (match-value 1)
	  (mismatch-value 0)
	  (pairwise-values nil)
	  (pairwise-symmetric? t)
	  )
  (dolist (triple pairwise-values)
    (unless (and (member (first triple) ordered-alphabet)
                 (member (second triple) ordered-alphabet))
      (error "Oops!  One of the elements in ~S is not in the alphabet ~S"
             triple ordered-alphabet)))
  (let* ((matrix-size (length ordered-alphabet))
	 (sub (diagonal-matrix matrix-size match-value mismatch-value))
	 (map (make-hash-table :test #'eql))
	 )
    (declare (type (simple-array t 2) sub))
    ;; Create mapping from elements of sequence alphabet to
    ;; indices of substitution matrix.
    (do ((set ordered-alphabet (cdr set)) (index 0 (1+ index)))
	((null set))
      (setf (gethash (car set) map) index))
    ;; Store user-specified special values in substitution matrix.
    (dolist (triple pairwise-values)
      (let* ((c1 (first triple)) 
	     (c2 (second triple))
	     (val (third triple))
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

(defun diagonal-matrix (n match-value mismatch-value)
  (let ((m (make-array (list n n) :initial-element mismatch-value)))
    (declare (type (simple-array t 2) m))
    (dotimes (j n) (setf (aref m j j) match-value))
    m))


;; Return a list of all optimal paths ending at (I,J).
;; The paths are lists of ALOC's.

;; For any given point (I,J), there is a unique set of best paths
;; that go back from it.  If two paths converge on point (I,J), then
;; from that point backwards they will encompass the same set of paths.
;; So once we compute the set of best paths from (I,J), we store that
;; set in a hash table and retrieve it if we ever hit (I,J) again.
;; This saves mucho computation for large matrices.

(defun get-best-paths-to (i j)
  (setq *all-paths-to-point*
    (mapcar 
     #'reverse
     (get-best-paths-to-internal i j)
     )))

(defun get-best-paths-to-internal (i j)
  (declare (fixnum i j))
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (let* ((curloc (aloc i j))
         (hashval (gethash curloc *backpaths-hash-table*))
         (pointmat *pointer-matrix*))
    (declare (type (simple-array t 2) pointermat))
    (declare (type aloc location))
    ;; If (I,J) is already hashed were done.
    (when hashval (return-from get-best-paths-to-internal hashval))
    (let* ((backcol 0) (backrow 0) (best-paths nil) (result nil))
      (declare (fixnum backcol backrow))
      (setq result
            (cond 
             ;; The path begins HERE! (since there is no previous point)
             ((null (aref pointmat i j)) (list (list curloc)))
             (t 
	      ;; For each backpointer, find the best paths from there, then
	      ;; chain the current point on to all those paths.
	      (loop for p in (aref pointmat i j) do
	            (setq backrow (aloc-row p))
	            (setq backcol (aloc-col p))
	            (setq best-paths 
                          (get-best-paths-to-internal backrow backcol))
                    append 
                    (mapcar #'(lambda (path) (cons curloc path)) best-paths)
                    ))))
      (setf (gethash curloc *backpaths-hash-table*) result)
      result
      )))

(defun get-a-path-to (i j) (reverse (get-a-path-to-internal i j)))

(defun get-a-path-to-internal (i j)
  (let ((curloc (aloc i j)))
    (if (null (aref *pointer-matrix* i j)) 
        (list curloc)
      (let* ((backpointers (aref *pointer-matrix* i j))
             (p (nth (random (length backpointers)) backpointers)))
        (cons curloc (get-a-path-to-internal (aloc-row p) (aloc-col p)))
        ))))
          

;;; Returns three lists.  Each list consists of two-element lists.
;;; The first element is the upper sequence symbol, the second element
;;; the lower sequence symbol.

(defun path-to-pre-common-post (path seq-down-rows seq-across-cols)
  (declare (simple-vector seq-down-rows seq-across-cols))

  (let* ((nrows (1+ (length seq-down-rows)))
         (ncols (1+ (length seq-across-cols)))
         (last-row (1- nrows))
         (last-col (1- ncols))
         (rest nil)
	 (alignment nil)
         (pre-alignment nil)
         (post-alignment nil)
         (first-point (first path))
         (last-point (first (last path)))
         )
    
    ;; All paths should trace back to origin.

    (unless (and (zerop (aloc-row first-point))
                 (zerop (aloc-col first-point)))
      (error "Internal error 1"))

    ;; Sequence indices are 0 based, while array indices are 1 based.
    ;; So to get sequence symbol from array index, subtract one from
    ;; array index and then access sequence.

    (labels ((sdr-residue (i) (aref seq-down-rows (1- i)))
             (sac-residue (j) (aref seq-across-cols (1- j)))
             (add-current-alignment (row col)
               (push (list (sdr-residue row) (sac-residue col)) alignment)
               ))

      ;; Special case stuff beyond last point (POST-ALIGNMENT).

      (let ((row (aloc-row last-point))
            (col (aloc-col last-point)))
        (cond
         ((and (= row last-row) (= col last-col)) nil)
         ((= row last-row)
          (do ((curcol last-col (1- curcol))) ((= curcol col))
            (push (list '- (sac-residue curcol)) alignment)))
         ((= col last-col)
          (do ((currow last-row (1- currow))) ((= currow row))
            (push (list (sdr-residue currow) '*) alignment)))
         (t (error "Internal error 2"))))


      ;; Do all points but first point in reverse order.

      (setq path (reverse path))

      (do ((points path (cdr points))) ((null (cdr points)))
        (let* ((point (first points))
               (currow (aloc-row point))
               (curcol (aloc-col point))
               (previous-point (second points))
               (prevrow (aloc-row previous-point))
               (prevcol (aloc-col previous-point)))
          (cond
           ;; Diagonal pointer to previous point
           ((and (= currow (1+ prevrow)) (= curcol (1+ prevcol)))
            (add-current-alignment currow curcol))
           ;; Up pointer to previous point
           ((= curcol prevcol)
            (do ((i currow (1- i))) ((= i prevrow))
              (push (list (sdr-residue i) '*) alignment)))
           ;; Left pointer to previous point
           ((= currow prevrow)
            (do ((j curcol (1- j))) ((= j prevcol))
              (push (list '- (sac-residue j)) alignment)))
           (t (error "Internal error 3")))
          ))

      ;; Separate alignment into three parts.  The pre-alignment,
      ;; which is the alignment prior to one of the sequences starting,
      ;; the common alignment, where both overlap, and the post-alignment,
      ;; which is the alignment after one of the sequences ends.

      (labels ((is-gap (x) (member x '(* -)))
               (has-no-top-gap (x) (not (is-gap (first x))))
               (has-no-bot-gap (x) (not (is-gap (second x))))
               (get-pre-alignment (seq)
                 (let ((start (first seq)) (pos 0))
                   (cond
                    ;; No gap before first alignment
                    ((and (has-no-top-gap start) (has-no-bot-gap start))
                     (values nil seq))
                    ;; Starting top gap, find first top alignment
                    ((null (has-no-top-gap start))
                     (setq pos (1+ (position-if #'has-no-top-gap (cdr seq))))
                     (values (subseq seq 0 pos) (subseq seq pos)))
                    ;; Starting bottom gap, find first bottom alignment
                    ((null (has-no-bot-gap start))
                     (setq pos (1+ (position-if #'has-no-bot-gap (cdr seq))))
                     (values (subseq seq 0 pos) (subseq seq pos)))
                    (t (error "Internal error 4")))
                   )))
        (multiple-value-setq (pre-alignment rest) 
            (get-pre-alignment alignment))
        (setq rest (nreverse rest))
        (multiple-value-setq (post-alignment alignment)
            (get-pre-alignment rest))
        (setq alignment (nreverse alignment))
        (setq post-alignment (nreverse post-alignment))
        )

      (values alignment pre-alignment post-alignment)

      )))


(defun pp-subst-matrix (sub ordered-alphabet)
  (terpri) (terpri)
  (format t "Substitution matrix = ~%~%")
  (let* ((size (length ordered-alphabet)) x y)
    (format t "     ")
    (dolist (c ordered-alphabet)
      (format t "   ~a  " c))
    (format t "~%~%")
    (dotimes (i size)
      (setq x (elt ordered-alphabet i))
      (format t "~a : " x)
      (dotimes (j size)
        (setq y (elt ordered-alphabet j))
	(format t "~6,' d" (if (arrayp sub) (aref sub i j) (funcall sub x y))))
      (format t "~%~%"))))

(defun pp-scoring-matrix (seq-down-rows seq-across-cols)
  (terpri) (terpri)
  (format t "Score matrix = ~%~%")
  (let* ((dims (array-dimensions *score-matrix*))
	 (nrows (first dims))
	 (ncols (second dims)))
    (format t "     ")  (format t "     ")
    (dotimes (j ncols)
      (unless (zerop j) (format t "   ~a " (elt seq-across-cols (1- j)))))
    (format t "~%~%")
    (dotimes (i nrows)
      (if (zerop i) 
          (format t "    ")
        (format t "~a : " (elt seq-down-rows (1- i))))
      (dotimes (j ncols)
	(format t "~5,' d" (aref *score-matrix* i j)))
      (format t "~%~%"))))


(defun pp-matrix-path (path seq-down-rows seq-across-cols)
  (terpri) (terpri)
  (let ((last-point (first (last path))))
    (format t "Path to ~S~%~%" (aloc-list last-point)))
  (let* ((dims (array-dimensions *score-matrix*))
	 (nrows (first dims))
	 (ncols (second dims))
         )
    (format t "       ")
    (dotimes (j ncols)
      (unless (zerop j) (format t "~a " (elt seq-across-cols (1- j)))))
    (format t "~%~%")
    (dotimes (i nrows)
      (if (zerop i)
          (format t "     ")
        (format t "~a :  " (elt seq-down-rows (1- i))))
      (dotimes (j ncols)
        (if (member (aloc i j) path) (format t "* ") (format t "  ")))
      (terpri))
    (terpri) (terpri)
    ))

(defun pp-alignment (path seq-down-rows seq-across-cols &optional (vbars t))
  (setq seq-down-rows (coerce seq-down-rows 'simple-vector))
  (setq seq-across-cols (coerce seq-across-cols 'simple-vector))
  (multiple-value-bind (common pre post)
      (path-to-pre-common-post path seq-down-rows seq-across-cols)
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


(defun try-nw (seqa seqb)
  (let* ((alphabet '(A C G T))
         (subst-function (create-subst-function alphabet :mismatch-value -2))
         (init-function #'(lambda (x y) (declare (ignore x y)) 0))
         (gap-function #'(lambda (x) (declare (ignore x)) -1))
         (type '(signed-byte 16))
         )
    (format t "~%~%SEQA: ") (loop for x in seqa do (format t "~A " x)) (terpri)
    (format t "~%~%SEQB: ") (loop for y in seqb do (format t "~A " y)) (terpri)
   (pp-subst-matrix subst-function alphabet)
    (setq seqa (coerce seqa 'simple-vector))
    (setq seqb (coerce seqb 'simple-vector))
    (clrhash *backpaths-hash-table*)
    (global-alignment-kernel 
     seqa seqb subst-function init-function gap-function type)
    (pp-scoring-matrix seqa seqb)
    (format t "Number of optimal endpoints: ~D~%" 
            (length *maximum-score-points*))
    (dolist (p *maximum-score-points*)
      (apply #'get-best-paths-to (aloc-list p))
      (format t "Number of optimal paths to ~S: ~D~%"
              (aloc-list p) (length *all-paths-to-point*))
      (dolist (path *all-paths-to-point*)
        (pp-matrix-path path seqa seqb)
        (pp-alignment path seqa seqb)
        ))))

(defun test-global-alignment (&optional (verbose nil))
  (let ((*align-verbose* verbose)
        (gap-function #'(lambda (n) (declare (ignore n)) 0))
        )
    (labels 
        ((test-one-algorithm (name down across align algorithm n)
           (format t "~&Testing ~A ~S..." algorithm name) (force-output t)
           (ecase algorithm
             (:general
              (run-global-alignment
               :seqa down :seqb across 
               :match-score 1
               :mismatch-score 0
               :gap-function gap-function 
               :find-all-paths t
               :show-alignments? nil))
             (:linear-gap
              (run-global-alignment
               :seqa down :seqb across 
               :gap-open-score 0.0
               :gap-space-score 0.0
               :find-all-paths t
               :show-alignments? nil)))
           (unless (= (length *all-points-paths*) n)
             (force-output t)
             (error "Oops: Expected ~D optimal paths, found ~D"
                    n (length *all-points-paths*)))
           (multiple-value-bind (common pre post)
               (path-to-pre-common-post 
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
         (test-one-path (name down across align &optional (n 1))
           (test-one-algorithm name down across align :general n)
           ;; (test-one-algorithm name down across align :linear-gap)
           ))
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
       '((A A) (C C) (G G) (- T) (- A) (C C) (G G) (T T)) 2)
      (test-one-path
       "TWO GAP ACROSS COL" 
       '(A C G T A C G T)
       '(A C G C G T)
       '((A A) (C C) (G G) (T *) (A *) (C C) (G G) (T T)) 2)
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
       '((C C) (G G) (T T) (A A) (C C) (G G) (T T) (A A)) 2)
      (test-one-path
       "ACROSS COLUMN SHIFTED"
       '(A C G T A C G T)
       '(C G T A C G T A)
       '((C C) (G G) (T T) (A A) (C C) (G G) (T T)) 2)
      )))


;;;; TOPLEVEL ROUTINE

;;; Outline:
;;;  1.  Generate a substitution matrix function from parameters
;;;  2.  Generate a gap penalty function, if not supplied, from parameters
;;;  3.  Generate the SCORE matrix and find all the points that end
;;;      the optimal paths.
;;;  4.  Pretty print (some of) the alignments corresponding to these 
;;;      best paths.
;;;  5.  Maybe generate all the optimal paths.

(defun run-global-alignment 

       (&key
        (seqa *testseq-a*)
        (seqb *testseq-b*)
        (computation-type :integer)
        (alphabet nil)
        (subst-function nil)
        (match-score 4)
        (mismatch-score -2)
        (pairwise-subst-scores nil)
        (pairwise-symmetric? t)
        (init-function #'(lambda (i j) (declare (ignore i j)) 0))
        (gap-function nil)
        (gap-open-cost 4)
        (gap-space-cost 1)
        (show-seqs? nil sseqs)
        (show-subst? nil ssubstp)
        (show-scores? nil sscoresp)
        (show-matrix-paths? nil)
        (show-alignments? t)
        (alignments-to-show 5)
        (find-all-paths nil)
        (verbose *align-verbose*)
        &aux seqa-vec seqb-vec
        )
        
  (cond
   ((and (vectorp seqa) (vectorp seqb))
    (setq seqa-vec seqa)
    (setq seqb-vec seqb)
    (setq seqa (coerce seqa 'list))
    (setq seqb (coerce seqb 'list)))
   ((and (listp seqa) (listp seqb))
    (setq seqa-vec (coerce seqa 'simple-vector))
    (setq seqb-vec (coerce seqb 'simple-vector)))
   (t (error "Both SEQA and SEQB must be lists or vectors")))

  (when (or show-seqs? (and verbose (null sseqs)))
    (format t "~&~%;; Sequence A:  ")
    (dolist (a seqa) (format t "~A " a)) (terpri)
    (format t "~%;; Sequence B:  ")
    (dolist (b seqb) (format t "~A " b)) (terpri) (terpri)
    )

  ;; Create substitution function

  (setq alphabet (or alphabet (remove-duplicates (union seqa seqb))))
  (setq subst-function
        (or subst-function
            (create-subst-function 
             alphabet
             :match-value match-score
             :mismatch-value mismatch-score
             :pairwise-values pairwise-subst-scores
             :pairwise-symmetric? pairwise-symmetric?
             )))

  (when (or show-subst? (and verbose (null ssubstp)))
    (pp-subst-matrix subst-function alphabet))

  (when verbose
    (if gap-function
        (format t ";; User supplied arbitrary gap function...~%")
      (format t "~%;; Gap penalty formula W(x) = ~A + ((x-1) * ~A))~%"
              gap-open-cost gap-space-cost)))

  ;; Generate and fill the SCORE matrix (*SCORE-MATRIX*) and the
  ;; *POINTER-MATRIX* array

  ;; Until we have an optimized global alignment function for
  ;; linear functions...

  (unless gap-function
    (setq gap-function 
          #'(lambda (x) (- (+ gap-open-cost (* gap-space-cost (- x 1)))))))

  (clrhash *backpaths-hash-table*)
  (global-alignment-kernel
   seqa-vec seqb-vec subst-function init-function gap-function
   (ecase computation-type
     (:integer '(signed-byte 16))
     (:single-float 'single-float)))

  (when (or show-scores? (and verbose (null sscoresp)))
    (pp-scoring-matrix seqa seqb))
     
  (when verbose
    (format t ";; Number of optimal endpoints: ~D~%" 
            (length *maximum-score-points*)))

  ;; Show up to ALIGNMENTS-TO-SHOW alignments, going to the various
  ;; different endpoints, and each one different.
  ;; Make sure we don't go into an infinite loop looking for
  ;; different paths to display.

  (when show-alignments?
    (let ((paths-shown-count 0) (paths-shown nil) (loop-count 0))
      (block exit
        (loop
         (incf loop-count)
         (dolist (p *maximum-score-points*)
           (let ((path (get-a-path-to (aloc-row p) (aloc-col p))))
             (unless (member path paths-shown :test #'equal)
               (when show-matrix-paths? (pp-matrix-path path seqa seqb))
               (pp-alignment path seqa seqb)
               (push path paths-shown)
               (incf paths-shown-count)
               )))
         (when (or (> loop-count 10) (>= paths-shown-count alignments-to-show))
           (return-from exit nil)
           )))))

  (when find-all-paths
    (setq *all-points-paths* nil)
    (dolist (p *maximum-score-points*)
      (let ((paths (get-best-paths-to (aloc-row p) (aloc-col p))))
        (when verbose
          (format t ";; To point ~A there are ~D optimal paths~%" 
                  (aloc-list p) (length paths)))
        (setq *all-points-paths* (append paths *all-points-paths*))
        ))
    (when verbose
      (format t ";; There are a total of ~D optimal paths~%"
              (length *all-points-paths*))))
            
  )
