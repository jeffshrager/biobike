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

;;; Author:   JP Massar.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;; SMITH-WATERMAN.  

;;; This code is similar to the NEEDLEMAN-WUNSCH code.  For a general
;;; explanation of what is going on see that file.  The SW algorithm
;;; finds local maximal alignments, as opposed to global ones.

;;; With SMITH-WATERMAN, the substitution matrix elements not on the diagonal
;;; (i.e., non-matches) should be negative, and the gap function
;;; should be non-zero.

;;; Furthermore, no element of the SCORE matrix is allowed to dip below 0.

;;; The optimal paths start at any maximal point in the SCORE matrix, not
;;; just maximal points along the bottom row or rightmost column.  Finally,
;;; optimal paths end when a SCORE of 0 is encountered.


(defun run-sw

       (&optional
        (seq-down-rows *testseq-a*)
        (seq-across-cols *testseq-b*)
        &key
        (constituents nil)
        (sub-function nil)
        (matchval 1.0)
        (nonmatchval -0.25)
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
    (format t "~&~%;; Sequence A: ")
    (dolist (a seq-down-rows) (format t "~A " a)) (terpri)
    (format t "~%;; Sequence B: ")
    (dolist (b seq-across-cols)  (format t "~A " b)) (terpri))

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

  ;; Create gap penalty function
  ;; This should go away as in NW if we have optimized algorithm.

  (unless gap-function
    (let ((open-penalty (float gap-open-penalty 0.0))
          (size-penalty (float gap-space-penalty 0.0)))
      (declare (single-float open-penalty size-penalty))
      (setq gap-function
            #'(lambda (gap-size)
                (declare (fixnum gap-size))
                (- (+ open-penalty 
                      (* size-penalty (the fixnum (1- gap-size)))))))
      (when verbose
        (format t "~%;; Gap penalty formula = P(n) = ~A + ((n-1) * ~A))~%"
                open-penalty size-penalty))
      ))
  (setq *gap-penalty-function* gap-function)

  ;; It is more efficient to have the sequences as vectors than as lists.

  (setq seq-down-rows (coerce seq-down-rows 'simple-vector))
  (setq seq-across-cols (coerce seq-across-cols 'simple-vector))

  ;; Generate and fill the SCORE matrix (*SCOREMAT*) 

  (generate-smith-waterman-scores
   seq-down-rows seq-across-cols sub-function gap-function)
     
  (when verbose 
    (when *substitution-matrix*
      (pp-sub-matrix *substitution-matrix* constituents)
      (pp-score-matrix seq-down-rows seq-across-cols)))
     
  ;; Find the highest scoring points.

  (setq *best-matches* (find-max-score-points))
  (when verbose 
    (format t "~&;; ~D Best endpoints.~%" (length *best-matches*)))

  (when show-alignments?
    (do ((matches *best-matches* (cdr matches)) (count 1 (1+ count)))
        ((or (null matches)) (> count max-alignments-to-show))
      ;; Find all the best paths from the next highest scoring point
      (let* ((point (first matches))
             (row (nwpoint-row point))
             (col (nwpoint-col point)))
        (setq *all-points-paths* (get-best-local-paths row col))
        (format t "~&;; Alignment ending at point (~D,~D):~%" row col)
        (pp-local-point-path 
         (first *all-points-paths*) seq-down-rows seq-across-cols)
        )))

  )


;;; The only difference here is that we don't let *scoremat*
;;; go negative, and if we do end up setting an entry of *scoremat*
;;; to 0.0, then there are no backpointers from that entry -- the
;;; alignment stops, so it doesn't necessarily go all the way back
;;; to the origin, as in NW.


(defun generate-smith-waterman-scores
       (seq-down-rows seq-across-cols sub-function gap-function)
  ;;(declare (optimize (speed 3) (safety 0) (debug 0)))
  (declare (type simple-vector seq-down-rows seq-across-cols))

  (let* ((nrows (length seq-down-rows)) 
         (ncols (length seq-across-cols))
	 (colmax 0.0) (rowmax 0.0) (high-score 0.0)
         (no-gap-score 0.0) 
         (subval 0.0)
         collist rowlist pointlist 
         )
    (declare (single-float colmax rowmax high-score no-gap-score subval))
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

          (setq colmax 0.0)
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

          (setq rowmax 0.0)
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

          (when (or (< no-gap-score 0.0) (< rowmax 0.0) (< colmax 0.0))
            (error "Internal inconsistency."))

          (if (and (= no-gap-score 0.0) (= rowmax 0.0) (= colmax 0.0))
              ;; No backpointers, since no score > 0.0 to go back to.
              ;; Make sure SCORE(I,J) is set to at least 0.0
              (progn
                (setf (aref pointmat i j) nil)
                (setf (aref scoremat i j) (max subval 0.0)))
            ;; At least one backpointer... Create a list of all of them
            ;; for this point, unless it's score is 0.0.  Set the score.
            (progn
              (setq pointlist nil)
              (setq high-score (max no-gap-score rowmax colmax))
              (setf (aref scoremat i j) (max (+ subval high-score) 0.0))
              (when (> (aref scoremat i j) 0.0)
                (when (and (= high-score no-gap-score) (> i 0) (> j 0))
                  (setq pointlist (cons (nwpoint (1- i) (1- j)) pointlist)))
                (when (= high-score rowmax) 
                  (setq pointlist (append pointlist rowlist)))
                (when (= high-score colmax) 
                  (setq pointlist (append pointlist collist)))
                (setf (aref pointmat i j) pointlist)
                )))

              )) ; Close loops 

      )))


;;; Returns a list of all the points in the matrix that
;;; have the maximum score.

(defun find-max-score-points ()
  (let* ((s *scoremat*)
         (nrows (array-dimension s 0))
         (ncols (array-dimension s 1))
         (bestscore 0.0)
         (pointlist nil))
    (declare (fixnum nrows ncols lastrow lastcol))
    (declare (single-float bestscore score))
    (declare (type (array single-float 2) s))
    ;; First search the matrix for the highest score.
    (dotimes (i nrows)
      (dotimes (j ncols)
        (setq bestscore (max bestscore (the single-float (aref s i j))))))
    ;; Then make a list of all points with that highest score.
    (dotimes (i nrows)
      (dotimes (j ncols)
        (when (= bestscore (the single-float (aref s i j)))
          (push (nwpoint i j) pointlist)
          )))
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

(defun get-best-local-paths (i j)
  (setq *all-points-paths*
    (mapcar 
     #'reverse
     (get-best-local-paths-internal i j)
     )))

(defun get-best-local-paths-internal (i j)
  (declare (fixnum i j))
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  ;; If (I,J) is already hashed were done.
  (let* ((curpoint (nwpoint i j))
         (hashval (gethash curpoint *paths-hash-table*)))
    (declare (ignorable hashval))
    (or hashval
        (let* ((backcol 0) (backrow 0)
               (curpoint (nwpoint i j))
	       (result nil))
          (declare (fixnum backcol backrow))
          (declare (type nwpoint curpoint))
          (setq 
           result
           (let ((backpoints (aref *pointermat* i j)))
             ;; Alignment starts here, at (I,J), as we can go no further back.
             (if (null backpoints)
                 (list (list curpoint))
               ;; Otherwise, for each backpoint, find the best paths to it,
               ;; then chain the current point on to all those paths.
               (mapcan
                #'(lambda (backpoint)
                    (setq backrow (nwpoint-row backpoint))
                    (setq backcol (nwpoint-col backpoint))
                    (mapcar
                     #'(lambda (path) (cons curpoint path))
                     (get-best-local-paths-internal backrow backcol)))
                backpoints))))
          (setf (gethash curpoint *paths-hash-table*) result)
          result
          ))))


;;; Returns the local alignment, and the coordinates of the 
;;; first point in the alignment and last point in the alignment
;;; as 2nd and 3rd values.

(defun path-to-local-alignment (path seq-down-rows seq-across-cols)
  (declare (simple-vector seq-down-rows seq-across-cols))

  (let* ((alignment nil)
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

      ;; Start with last point.

      (add-current-alignment 
       (nwpoint-row last-point) (nwpoint-col last-point))
      (setq previous-point last-point)

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

      (values alignment (nwpoint-list first-point) (nwpoint-list last-point))

      )))


(defun path-to-ij (path)  (mapcar #'(lambda (x) (nwpoint-list x)) path))

;;; Given a list of points describing a path, convert them to an
;;; alignment and pretty print that alignment.

(defun pp-local-point-path
       (path seq-down-rows seq-across-cols &optional (vbars t))
  (setq seq-down-rows (coerce seq-down-rows 'simple-vector))
  (setq seq-across-cols (coerce seq-across-cols 'simple-vector))
  (multiple-value-bind (common start end)
      (path-to-local-alignment path seq-down-rows seq-across-cols)
    (flet ((print-seq (seq accessor print-gaps?)
	     (let ((pseq (mapcar accessor seq)))
	       (dolist (elem pseq)
	       (format t "~A"
		       (if (and (not print-gaps?) 
				(or (eql elem '*) (eq elem '-)))
			   " " elem))))))
      (format t "~&~%;; Local alignment starting at: SEQA[~D], SEQB[~D]~%"
              (first start) (second start))
      (format t     ";;  and ending at:              SEQA[~D], SEQB[~D]~%~%"
              (first end) (second end))
      (print-seq common #'first t)
      (terpri)
      (when vbars
	(dolist (pair common)
          (format t "~A" (if (eql (first pair) (second pair)) "|" " ")))
        (terpri))
      (print-seq common #'second  t)
      (terpri)
      (terpri)
      )))

(defun test-sw ()
  (let ((*nw-verbose* t))
    ;;(run-sw '(A C G T A C G T) '(A C G T A C G T))
    ;;(run-sw '(G T A C) '(A C G T A C G T))
    ;;(run-sw '(A C G T A C G T) '(T T G T T C A A A))
    ;;(run-sw '(A C G T A A C G T T T A C) '(C G T C A))
    (run-sw *testseq-a* *testseq-b*)
    ))


(defun demo-sw ()
  (format t "~&~%~%;; Sequence A:  ")
  (dolist (a *testseq-a*) (format t "~A " a)) (terpri)
  (format t "~%;; Sequence B:  ")
  (dolist (b *testseq-b*) (format t "~A " b)) (terpri) (terpri)
  (let ((*nw-verbose* nil))
    (format t "~&~%;; Sample alignments with standard gap penalty:~%~%")
    (run-sw *testseq-a* *testseq-b*
            :show-alignments? t
            :max-alignments-to-show 2)
    (format t "~%;; Sample alignments with 2.0+0.25*(n-1) penalty~%~%")
    (run-sw *testseq-a* *testseq-b*
            :gap-function 
            #'(lambda (n) 
                (declare (fixnum n))
                (- (+ 2.0 (* 0.25 (1- (float n 0.0))))))
            :show-alignments? t
            :max-alignments-to-show 2)
    (format t "~%;; Sample alignments with -3.0 mismatch plus large gap")
    (run-sw *testseq-a* *testseq-b*
            :nonmatchval -3.0
            :gap-function
            #'(lambda (n) 
                (declare (fixnum n))
                (- (+ 10.0 (* 0.50 (1- (float n 0.0))))))
            :show-alignments? t
            :max-alignments-to-show 2)
    ))

