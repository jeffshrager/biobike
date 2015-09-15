;;; -*- mode: Lisp; Syntax: Common-Lisp; Package: biolisp; -*-

(in-package :bio)

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


;;;; VECTOR OPERATORS WE NEED FOR DOING EM.


(defarrayop sf-v-reciprocal / 1 2 single-float)
(defarrayop sf-v-difference - 1 3 single-float)
(defarrayop sf-v-quotient / 1 3 single-float)
(defarrayop sf-v-add + 1 3 single-float)
(defarrayop sf-v-product * 1 3 single-float)
(defarrayop sf-v-product3 * 1 4 single-float)
(defarrayop sf-v-min min 1 3 single-float)
(defarrayop sf-v-max max 1 3 single-float)


;;;; SINGLE FLOAT STATISTICAL FUNCTIONS ON VECTOR DATA


(defun sf-mean-vector (data)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (and data
       (let* ((len (length data))
              (ncomponents (length (elt data 0)))
              (mean (sf-make-array ncomponents 0.0)))
         (map nil (lambda (datum) (sf-v-add mean mean datum)) data)
         (sf-v-quotient mean mean (float len 0.0))
         mean
         )))

(defun sf-variance-vector (data mean-vector)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (and data
       (let* ((len (length data))
              (ncomponents (length (elt data 0)))
              (variance (sf-make-array ncomponents 0.0))
              (temp (sf-make-array ncomponents)))
         (map nil (lambda (datum) 
                      (sf-v-difference temp datum mean-vector)
                      (sf-v-product temp temp temp)
                      (sf-v-add variance variance temp))
              data)
         (sf-v-quotient variance variance (float len 0.0))
         variance
         )))
              

;;;; GENERATING TEST DATA FOR EM


(defun dgaussian-random-pair (mean sigma)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (let ((fmean (float mean 0.0d0))
        (fsigma (float sigma 0.0d0))
        (x1 0.0d0) 
        (x2 0.0d0)
        (w 2.0d0)
        )
    (declare (double-float fmean sigma x1 x2 w))
    (do () ((< w 1.0))
      (setq x1 (- (* 2.0d0 (random 1.0d0)) 1.0d0))
      (setq x2 (- (* 2.0d0 (random 1.0d0)) 1.0d0))
      (setq w (+ (* x1 x1) (* x2 x2)))
      )
    (setq w (sqrt (/ (* -2.0 (log w)) w)))
    (setq x1 (* x1 w))
    (setq x2 (* x2 w))
    (values (+ (* x1 fsigma) fmean)
            (+ (* x2 fsigma) fmean)
            )))
    
(defun gaussian-random-pair (mean sigma)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (let ((fmean (float mean 0.0))
        (fsigma (float sigma 0.0))
        (x1 0.0) 
        (x2 0.0)
        (w 2.0)
        )
    (declare (single-float fmean sigma x1 x2 w))
    (do () ((< w 1.0))
      (setq x1 (- (* 2.0 (random 1.0)) 1.0))
      (setq x2 (- (* 2.0 (random 1.0)) 1.0))
      (setq w (+ (* x1 x1) (* x2 x2)))
      )
    (setq w (the single-float (sqrt (/ (* -2.0 (the single-float (log w))) w))))
    (setq x1 (* x1 w))
    (setq x2 (* x2 w))
    (values (+ (* x1 fsigma) fmean)
            (+ (* x2 fsigma) fmean)
            )))

    
;;; Create N random data vectors.  Each vector is of length D.
;;; For each random vector V, for each component i, each V[i]
;;; is given a random value normally distributed with mean MEANS[i]
;;; and standard-deviation SIGMAS[i].  (So there is no covariance
;;; among the components).

(defun generate-gaussian-random-vector (size means sigmas)
  (let ((result (make-array size :element-type 'single-float)))
    (loop for j from 0 below size 
          for mean across means
          for sigma across sigmas do
          (setf (aref result j) (gaussian-random-pair mean sigma)))
    result
    ))

(defun generate-gaussian-random-vectors (n size means sigmas)
  (loop for j below n collect 
        (generate-gaussian-random-vector size means sigmas)))


;;; Create N data points of dimensionality D.
;;; The points are centered around NCLUSTERS different clusters.
;;; Each cluster has a MEAN vector which is randomly generated as follows:
;;;   each component of every MEAN vector is drawn from a gaussian
;;;   distribution with mean 0.0 and standard deviation 2.0.
;;; Each data point is created first by selecting one of the cluster MEANs
;;; at random and then by using a gaussian distribution centered on that
;;; mean vector with standard deviations SIGMAS, with no covariance.

(defun generate-test-em-data (n d nclusters sigmas)
  (let* ((mean-mean (make-array d :initial-element 0.0))
         (mean-sigmas (make-array d :initial-element 2.0))
         (cluster-means
          (loop for j from 0 below nclusters 
                collect
                (generate-gaussian-random-vector d mean-mean mean-sigmas))))
    (values
     (coerce
      (loop for j from 0 below n collect
            (let* ((which-cluster (random nclusters))
                   (cluster-mean (nth which-cluster cluster-means)))
              (generate-gaussian-random-vector d cluster-mean sigmas)
              ))
      'vector)
     cluster-means
     )))

(defvar *data*)
(defun generate-standard-test-em-data (n sigmas)
  (let ((mean1 #(2.0 2.0)) (mean2 #(-2.0 -2.0)))
    (values
     (setq *data*
           (coerce
            (loop for j below (floor n 2) append
                  (list
                   (generate-gaussian-random-vector 2 mean1 sigmas)
                   (generate-gaussian-random-vector 2 mean2 sigmas)
                   ))
            'vector))
     (list mean1 mean2)
     )))

(defun test-em-data-stats (data)
  (let* ((n (length data))
         (ncomponents (length (aref data 0)))
         (mean1-vector (sf-make-array ncomponents))
         (mean2-vector (sf-make-array ncomponents))
         (c1-data (loop for j from 0 below n by 2 collect (aref data j)))
         (c2-data (loop for j from 1 below n by 2 collect (aref data j)))
         )
    (format t "~&~%;; Data mean vector: ~A~%" (sf-mean-vector data))
    (format t "~%;; Cluster 1 mean vector: ~A~%" 
            (setq mean1-vector (sf-mean-vector c1-data)))
    (format t "~%;; Cluster 2 mean vector: ~A~%" 
            (setq mean2-vector (sf-mean-vector c2-data)))
    (format t "~%;; Cluster 1 variance: ~A~%" 
            (sf-variance-vector c1-data mean1-vector))
    (format t "~%;; Cluster 2 variance: ~A~%" 
            (sf-variance-vector c2-data mean2-vector))
    ))



;;;; EM ALGORITHM from SQLEM paper, Ordonez & Cereghini


;; data = Y, ncomponents = p, nclusters = k

(defun em (data ncomponents nclusters epsilon max-iterations)
  (declare (ignore epsilon))

  (let* (
        ;; data-size = n
         (n-data-points (length data))
         ;; C(p,k), R(p,p), W(k), X(n,k)
         (c-matrix (sf-make-array (list ncomponents nclusters)))
         (r-vector (sf-make-array ncomponents))
         (w-vector (sf-make-array nclusters))
         (x-matrix (sf-make-array (list n-data-points nclusters)))
         ;; C'(p,k), R'(p,p), W'(k)
         (ctemp-matrix (sf-make-array (list ncomponents nclusters)))
         (rtemp-vector (sf-make-array ncomponents))
         (wtemp-vector (sf-make-array nclusters))
         ;; P[i,j], but we only need a vector, not the entire matrix
         (pdf-vector (sf-make-array nclusters))
         ;; C[*;j]
         (cluster-mean-vector (sf-make-array ncomponents))
         (r-inverse-vector (sf-make-array ncomponents))
         ;; X[i;*]
         (x-vector (sf-make-array nclusters))
         (temp-vector (sf-make-array ncomponents))
         (pi-factor 0.0)
         (determinant-sqrt 0.0)
         (pij-denominator 0.0)
         (pij 0.0)
         (llh 0.0)
         (sumpi 0.0)
         (mahalanobis-distance 0.0)
         (equal-weight (/ 1.0 nclusters))
         (data-vector nil)
         )
    (declare (single-float pi-factor determinant-sqrt pij-denominator))
    (declare (single-float pij llh sumpi mahalanobis-distance equal-weight))
    
    ;;; Initialize C, R and W to initial guesses.
    ;;; For now, guess that the means are 0.0, the variances all 1.0
    ;;; (covariances 0.0) and the weights are all equal.

    (setf (aref c-matrix 0 0) 1.0 (aref c-matrix 1 0) 1.0)
    (setf (aref c-matrix 0 1) -1.0 (aref c-matrix 1 1) -1.0)
    ;;(init-array c-matrix)
    (init-array r-vector 1.0)
    (init-array w-vector equal-weight)

    ;; (2pi)**(p/2)
    (setq pi-factor (expt (* 2 pi) (/ ncomponents 2.0)))

    (format t "~%;; Initial guess for cluster means:~%")
    (dotimes (j nclusters)
      (format t ";;   Cluster ~D: " (1+ j))
      (matrix-col-slice cluster-mean-vector c-matrix j)
      (dotimes (i ncomponents)
        (format t "~5,2F " (aref cluster-mean-vector i)))
      (terpri))
    (format t "~%;; Initial variance estimates: ~%")
    (dotimes (j ncomponents)
      (format t ";;   Component ~D: ~5,2F~%" (1+ j) (aref r-vector j)))
    (terpri)

    (dotimes (iteration max-iterations)

      ;; E Step

      ;; C' = 0, R' = 0, W' = 0
      (init-array ctemp-matrix 0.0)
      (init-array rtemp-vector 0.0)
      (init-array wtemp-vector 0.0)
      ;; llh = 0
      (setq llh 0.0)

      ;; compute determinant of R and the entire denominator
      ;; for the pij expression outside of the loop.
      ;; The determinant of a diagonal matrix is just the product of 
      ;; the diagonal values (I hope).
      (setq determinant-sqrt (sqrt (reduce #'* r-vector)))
      (setq pij-denominator (* pi-factor determinant-sqrt))

      ;; Compute R-inverse outside of loop.  Only need diagonals.
      (sf-v-reciprocal r-inverse-vector r-vector)

      ;; For i = 1 to n   (for each data point)
      (dotimes (data-vector-index n-data-points) 

        ;; sumpi = 0
        (setq sumpi 0.0)

        ;; DATA-VECTOR = Y[i]
        (setq data-vector (aref data data-vector-index))
        
        ;; for j = 1 to k   (for each cluster)
        (dotimes (cluster-index nclusters)
          (declare (fixnum cluster-index))

          ;; Extract C[*;j]
          (matrix-col-slice cluster-mean-vector c-matrix cluster-index)
          ;;(format t "~%;; Cluster mean vector[~D]: ~A~%" 
          ;;cluster-index cluster-mean-vector)

          ;; (Y[i;*] - C[*;j])$TRANSPOSE * R-INVERSE *  (Y[i;*] - C[*;j])

          ;; Compute TEMP = (Y[i;*] - C[*;j])
          (sf-v-difference temp-vector data-vector cluster-mean-vector)
          ;; Compute SUM over n of TEMP[n] * R-INVERSE[n] * TEMP[n]
          (setq mahalanobis-distance 
                (sf-inner-product-n temp-vector r-inverse-vector temp-vector))

          ;; Compute P[i,j], i fixed.  Don't need an entire P[n,k]
          ;; matrix as stated in algorithm description, only need a vector.
          (setq pij
                (* (/ (aref w-vector cluster-index) pij-denominator)
                   (exp (* -0.5 mahalanobis-distance))
                   ))
          (setf (aref pdf-vector cluster-index) pij)

          ;; sumpi = sumpi + pij
          (incf sumpi pij)
            
          )

        ;; X[i;*] = P[i;*] / sumpi
        (sf-v-quotient x-vector pdf-vector sumpi)
        (slice-to-matrix-row x-matrix x-vector data-vector-index)

        ;; llh = llh + ln(sumpi)
        (incf llh (log sumpi))

        ;; C' = C' + Y[i;*] * X[i;*]$transpose
        (sf-matmult-vv ctemp-matrix data-vector x-vector 'addto)

        ;;  W' = W' + X[i;*]
        (sf-v-add wtemp-vector wtemp-vector x-vector)

        )

      ;; M step
                  
      ;; For j = 1 to k
      (dotimes (cluster-index nclusters)
        (declare (fixnum cluster-index))
      
        ;; Create CLUSTER-MEAN-VECTOR = C[*;j]
        (matrix-col-slice cluster-mean-vector ctemp-matrix cluster-index)

        ;; C[*;j] = C'[*;j] / W[j]
        (sf-v-quotient cluster-mean-vector cluster-mean-vector wtemp-vector)
        (slice-to-matrix-col c-matrix cluster-mean-vector cluster-index)

        ;; for i = 1 to n
        (dotimes (data-vector-index n-data-points)
          (declare (fixnum data-vector-index))
        
          (setq data-vector (aref data data-vector-index))

          ;; R' = R' + (Y[i;*] - C[*;j]) * X[i,j] * (Y[i;*] - C[*;j])

          (let ((xij (aref x-matrix data-vector-index cluster-index)))
            (declare (single-float xij))
            ;; TEMP = Y[i;*] - C[*;j]
            (sf-v-difference temp-vector data-vector cluster-mean-vector)
            (sf-v-product3 temp-vector temp-vector xij temp-vector)
            (sf-v-add rtemp-vector rtemp-vector temp-vector))

          ))

      ;; R = R'/n, W = W'/n
      (sf-v-quotient r-vector rtemp-vector (float n-data-points 0.0))
      (sf-v-quotient w-vector wtemp-vector (float n-data-points 0.0))

      (format t "~&~%;; Results after ~D iterations~%~%" (1+ iteration))
      (format t ";; Log likelihood: ~5,2F~%" llh)
      (format t ";; New cluster mean estimates: ~%")
      (dotimes (j nclusters)
        (format t ";;   Cluster ~D: " (1+ j))
        (matrix-col-slice cluster-mean-vector c-matrix j)
        (dotimes (i ncomponents)
          (format t "~5,2F " (aref cluster-mean-vector i)))
        (terpri))
      (format t "~%;; New variance estimates: ~%")
      (dotimes (j ncomponents)
        (format t ";;   Component ~D: ~5,2F~%" (1+ j) (aref r-vector j)))
      (terpri)
      
      )

    (let ((membership-lists (make-array nclusters :initial-element nil))
          (membership-probability-vector (make-array nclusters)))
      (loop for data-vector across data
            for i from 0 do
            (matrix-row-slice membership-probability-vector x-matrix i)
            (let* ((max (reduce #'max membership-probability-vector))
                   (cluster-index 
                    (position max membership-probability-vector :test #'=)))
              (push (list data-vector max) 
                    (aref membership-lists cluster-index)
                    )))
      (format t "~%;; Number of data points in each cluster:~%")
      (loop for members across membership-lists
            for j from 1 do
            (format t ";;   Cluster ~D: ~D~%" j (length members))
            )
      (terpri)
      )
                  
    (values c-matrix r-vector w-vector x-matrix)

    ))


(defun test-em (n d nclusters sigmas max-iterations)
  (multiple-value-bind (data actual-means)
      (generate-standard-test-em-data n sigmas)
    ;;(generate-test-em-data n d nclusters sigmas)
    (flet ((report ()
             (format t "~&~%~%")
             (format t ";; Running EM using a ~D-dimensional space,~%" d)
             (format t ";; with ~D clusters,~%" nclusters)
             (format t ";; and ~D total data vectors.~%" n)
             (terpri)
             (format t ";; Actual cluster means used to generate data are:~%")
             (loop for j from 0 by 1 for mean in actual-means do
                   (format t ";;    Cluster ~D mean: " (1+ j))
                   (loop for i below d do (format t "~5,2F " (aref mean i)))
                   (terpri)
                   )
             (terpri)
             (format t ";; Actual variances used to generate data are: ~%")
             (loop for j from 0 by 1 for sigma across sigmas do
                   (format t ";;    Component ~D: ~5,2F~%" 
                           (1+ j) (* sigma sigma)))
             (terpri)
             (terpri)))
      (report)
      (em data d nclusters 0.0 max-iterations)
      (report)
      )))


;;;; EM KMEANS ALGORITHM FOR 1-DIMENSIONAL DATA


;;; Generate N data points.  For each point, first randomly pick a cluster
;;; (one of the means in MEAN-VECTOR), then randomly generate
;;; a data point about that mean with standard deviation SIGMA.

(defun generate-kmeans-test-data (n mean-sequence sigma)
  (coerce
   (let ((result-list nil) (nmeans (length mean-sequence)))
     (dotimes (j n result-list)
       (let ((rmean (elt mean-sequence (random nmeans))))
         (push (gaussian-random-pair rmean sigma) result-list))))
   'sfa1
   ))

(defun first-guess-at-1d-cluster-means (data mean-vector)
  (declare (type sfa1 mean-vector))
  (let* ((nclusters (length mean-vector))
         (min-data (reduce #'min data))
         (max-data (reduce #'max data))
         (equal-sep (/ (- max-data min-data) (1+ nclusters))))
    (declare (fixnum nclusters) (single-float min-data max-data equal-sep))
    (loop for j fixnum below nclusters do
          (setf (aref mean-vector j) (+ min-data (* equal-sep (1+ j)))))))
          

;;; Implement the EM algorithm to find only the MEAN values for N
;;; clusters in the 1-dimensional DATA provided.  
;;; Each cluster's data is assumed to have
;;; a different mean but the same standard deviation (SIGMA).


(defun em-kmeans-1d 
       (data sigma nclusters max-iterations &key (verbose t) (debug nil))
  (declare (type sfa1 data))
  (declare (single-float sigma) (fixnum nclusters max-iterations))

  ;; Create the arrays and vectors we will need.

  (let* ((n-data-points (length data))
         ;; The current best-guess as to the solution to our problem.
         (mean-vector (sf-make-array nclusters))
         ;; For a given data point i, for a given cluster j,
         ;; the probability that data point I belongs to cluster j
         ;; given MEAN-VECTOR and SIGMA.  This is what we compute.
         (relative-probability-vectors
          (vector-of-sf-vectors n-data-points nclusters))
         ;; A constant needed in the E step inner loop.
         (exp-constant (/ -1.0 (* 2.0 (* sigma sigma))))
         ;; A list of data points far away from all of our guessed means.
         (far-out-list nil)
         (no-cluster-info-list nil)
         )
    (declare (fixnum n-data-points))
    (declare (type sfa1 mean-vector pdf-vector sum-of-weights-vector))
    (declare (single-float exp-constant))
    
    ;; Initialize the first guess for the mean of each cluster to 
    ;; something reasonable.
    ;; We'll find the min and max of the data, and put the means
    ;; equally spaced in that interval.

    (multiple-value-bind (min-data max-data)
        (first-guess-at-1d-cluster-means data mean-vector)
      (format t "~&~%~%;; Input summary: ~%")
      (format t ";;   Number of data points: ~D~%" n-data-points)
      (format t ";;   Number of clusters requested: ~D~%" nclusters)
      (format t ";;   Variance of data: ~5,2F~%" (* sigma sigma))
      (format t ";;   Minimum data point: ~5,2F~%" min-data)
      (format t ";;   Maximum data point: ~5,2F~%" max-data)    
      (format t ";;   Initial guess for cluster means:~%")
      (dotimes (j nclusters)
        (format t ";;     Cluster ~D: " (1+ j))
        (format t "~5,2F " (aref mean-vector j))
        (terpri))
      (terpri)
      )

    (dotimes (iteration max-iterations)

      ;; E Step.  Compute values for CLUSTER-PROBABILITY-MATRIX.

      (setq far-out-list 
            (em-kmeans-1d-e-kernel
             data mean-vector relative-probability-vectors exp-constant))

      (when debug
        (dotimes (i 10)
          (format t ";; P(Datum ~2D): " (1+ i))
          (pp-sf-vector (aref relative-probability-vectors i)))
        (terpri))

      ;; Now we have the relative probability that each datum belongs
      ;; to a given cluster in CLUSTER-PROBABILITY-MATRIX.

      ;; M Step.  Compute new guess for MEAN-VECTOR.

      (setq no-cluster-info-list
            (em-kmeans-1d-m-kernel
             data mean-vector relative-probability-vectors far-out-list))

      (when (and debug no-cluster-info-list)
        (dolist (cluster-info no-cluster-info-list)
          (format t ";; No cluster data found for cluster ~D~%"
                  (second cluster-info))
          (format t ";;   New guess, ~F, taken from ~A~%"
                  (third cluster-info) (first cluster-info))))

      (when verbose
        (format t ";; Guess for means after ~D iterations:~%" (1+ iteration))
        (dotimes (j nclusters)
          (format t ";;   Cluster ~D: " (1+ j))
          (format t "~5,2F " (aref mean-vector j))
          (terpri))
        (terpri))

      ;; We've computed a new guess for MEAN-VECTOR.  Now repeat E & M steps.

      )

    ;; Report how many data elements in each cluster.

    (let ((membership-lists (make-array nclusters :initial-element nil)))
      (loop for datum across data
            for i from 0 do
            (let* ((rpv (aref relative-probability-vectors i))
                   (max (reduce #'max rpv))
                   (cluster-index (position max rpv :test #'=)))
              (push (list datum max) (aref membership-lists cluster-index))
              ))
      (format t "~%;; Number of data points in each cluster:~%")
      (loop for members across membership-lists
            for j from 1 do
            (format t ";;   Cluster ~D: ~D~%" j (length members)))
      (terpri))
                  
    ;; Return best estimates of cluster means.

    (values mean-vector)

    ))


(defun em-kmeans-1d-e-kernel 
       (data mean-vector rpvs exp-constant)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (declare (type sfa1 data mean-vector))
  (declare (single-float exp-constant))

  (let ((far-out-list nil) 
        (n-data-points (length data))
        (nclusters (length mean-vector)))
    (declare (fixnum n-data-points nclusters))

    ;; For each datum, calculate the relative probability that
    ;; it belongs to the Jth cluster.  To do this we evaluate
    ;; the PDF for the datum given the cluster's mean (sigma is fixed),
    ;; which is MEAN-VECTOR[j].
    ;; We do this for each cluster.  Then the relative probability
    ;; for cluster J is just the PDF value at J divided by the
    ;; sum of the PDF's for all the clusters.

    ;; It could be that a particular datum is so far from the
    ;; current guess of the mean that the pdf formula returns 0.
    ;; This is okay -- however, if the datum is so far from EVERY
    ;; guessed mean that all the pdf's return zero we would be
    ;; in trouble.  In this case we don't do the division to
    ;; calculate the relative probabilities; rather, we set the relative
    ;; probabilities to 0.0, which has the effect of ignoring this
    ;; data point for this iteration.

    (loop for i fixnum below n-data-points do
          (let ((rpv (aref rpvs i))
                (datum (aref data i))
                (sum-of-pdfs 0.0))
            (declare (type sfa1 rpv) (single-float datum sum-of-pdfs))
            (loop for j fixnum below nclusters do
                  (let* ((x-minus-m (- datum (aref mean-vector j))))
                    (declare (single-float x-minus-m sum-of-pdfs))
                    (setf (aref rpv j) 
                          (exp (* exp-constant (* x-minus-m x-minus-m))))
                    (incf sum-of-pdfs (aref rpv j))
                    ))
            ;; If SUM-OF-PDFS is 0.0, then every element of RPV must
            ;; be 0.0, since the values are always non-negative.
            (if (/= sum-of-pdfs 0.0)
                (sf-v-quotient rpv rpv sum-of-pdfs)
              (push datum far-out-list)
              )))
            
    far-out-list

    ))

(defun em-kmeans-1d-m-kernel 
       (data mean-vector rpvs far-out-list)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (declare (type sfa1 data mean-vector sum-of-weights-vector))
  (declare (type sfa2 cluster-probability-matrix))
  ;; Calculate a new guess for MEAN-VECTOR, based on 
  ;; relative probabilities.  The new guess is the weighted average
  ;; of the data, weighted by these relative probabilities we
  ;; computed in the E step.
  (let ((n-data-points (length data))
        (nclusters (length mean-vector))
        (no-cluster-info-list nil)
        (sum-of-weights 0.0))
    (declare (fixnum n-data-points nclusters))
    (declare (single-float sum-of-weights))
    ;; Calculate over each cluster...
    (loop for j fixnum below nclusters do
          (setf (aref mean-vector j) 0.0)
          (setf sum-of-weights 0.0)
          ;; Weighing each datum appropriately...
          (loop for i fixnum below n-data-points do
                (let* ((rpv (aref rpvs i))
                       (cluster-weight (aref rpv j))
                       (datum (aref data i)))
                  (declare (type sfa1 rpv))
                  (declare (single-float cluster-weight datum))
                  (incf (aref mean-vector j) (* datum cluster-weight))
                  (incf sum-of-weights cluster-weight)
                  ))
          ;; Normalize the new mean by the sum of the weights.
          ;; If all the weights for a cluster are 0.0, all the data
          ;; was very, very far away from our guess for the mean for that
          ;; cluster.  So guess again, this time picking one of the data
          ;; points at random, either from the list of points that are very
          ;; far away from every cluster mean guess, or at random if necessary.
          (if (/= 0.0 sum-of-weights)
              (setf (aref mean-vector j) 
                    (/ (aref mean-vector j) sum-of-weights))
            (cond
             (far-out-list
              (setf (aref mean-vector j)
                    (elt far-out-list (random (length far-out-list))))
              (push (list 'far-out j (aref mean-vector j))
                    no-cluster-info-list))
             (t
              (setf (aref mean-vector j) (aref data (random n-data-points)))
              (push (list 'random j (aref mean-vector j)) 
                    no-cluster-info-list)))))
    no-cluster-info-list
    ))


#+testem
(deftest 
 em-kmeans-1d
 (let* ((sigma 3.0)
        (n-iterations 20)
        (means '(-10.0 0.0 10.0))
        (data (generate-kmeans-test-data 1000 means sigma)))
   (em-kmeans-1d data sigma (length means) n-iterations :verbose nil))
 #(-10.0 0.0 10.0)
 :chapter :em
 :comparison
 #'(lambda (v1 v2) (every #'(lambda (x y) (< (abs (- x y)) 0.5)) v1 v2))
 )
   

;;;; EM KMEANS ALGORITHM FOR N-DIMENSIONAL DATA


;;; Generate N data points.  For each point, first randomly pick a cluster
;;; (one of the means in MEAN-SEQUENCE), then randomly generate
;;; a data point about that mean with standard deviation SIGMA.

(defun generate-nd-kmeans-test-data (n mean-sequence sigma)
  (let ((result-vector (make-array 0 :fill-pointer t :adjustable t))
        (nmeans (length mean-sequence))
        (ndims (length (elt mean-sequence 0))))
    (dotimes (j n)
       (let ((cluster-mean (elt mean-sequence (random nmeans)))
             (datum-vector (sf-make-array ndims)))
         (dotimes (i ndims)
           (setf (aref datum-vector i) 
                 (gaussian-random-pair (elt cluster-mean i) sigma)))
         (vector-push-extend datum-vector result-vector)
         ))
    result-vector
    ))


;;; Imagine surrounding all the data by a hyperrectangle such that
;;; some data points touch the walls and/or vertices  of the hyperrectangle
;;; but none are are outside it.   Now think of a line from the 'lower left'
;;; of this hyperrectangle to its 'upper right' (using a two-dimensional
;;; analog).  Think of N points evenly spaced along this line within
;;; the hyperrectangle.

;;; The coordinates of these N points are what we are computing here.

(defun first-guess-at-nd-cluster-means (data cluster-mean-vectors)
  (let* ((n-data-elements (length data))
         (ndims (length (aref data 0)))
         (nclusters (length cluster-mean-vectors))
         (min-vector (sf-make-array ndims))
         (max-vector (sf-make-array ndims))
         (delta-vector (sf-make-array ndims))
         (datum (aref data 0)))
    (declare (fixnum n-data-elements ndims nclusters))
    (declare (type sfa1 min-vector max-vector datum))
    (loop for i fixnum below ndims do
          (setf (aref min-vector i) (aref datum i))
          (setf (aref max-vector i) (aref datum i)))
    (loop for j fixnum below n-data-elements do
          (setq datum (aref data j))
          (sf-v-min min-vector min-vector datum)
          (sf-v-max max-vector max-vector datum))
    (sf-v-difference delta-vector max-vector min-vector)
    (sf-v-quotient delta-vector delta-vector (float (1+ nclusters) 0.0))
    (loop for mean-vector across cluster-mean-vectors do
          (sf-v-add mean-vector min-vector delta-vector)
          (sf-v-add delta-vector delta-vector delta-vector))
    (values min-vector max-vector)
    ))
  

;;; Implement the EM algorithm to find the MEAN values for N
;;; clusters in the 1-dimensional DATA provided.  
;;; Each cluster's data is assumed to have
;;; a different mean but the same standard deviation (SIGMA).


(defun em-kmeans-nd 
       (data sigma nclusters max-iterations &key (verbose t) (debug nil))
  (declare (single-float sigma) (fixnum nclusters max-iterations))

  (let* ((n-data-points (length data))
         (ndims (length (aref data 0)))
         ;; The current best-guess as to the solution to our problem.
         (mean-vectors (vector-of-sf-vectors nclusters ndims))
         ;; A temporary vector needed in the E & M steps
         (temp-vector (sf-make-array nclusters))
         ;; For a given data point i, for a given cluster j,
         ;; the probability that data point I belongs to cluster j
         ;; given MEAN-VECTOR and SIGMA.  This is what we compute.
         (relative-probability-vectors
          (vector-of-sf-vectors n-data-points nclusters))
         ;; A list of data points far away from all of our guessed means.
         (far-out-list nil)
         (no-cluster-info-list nil)
         )
    (declare (fixnum n-data-points ndims))
    (declare (type sfa1 pdf-vector))
    
    ;; Initialize the first guess for the mean of each cluster to 
    ;; something reasonable and print a summary of the initial conditions.

    (multiple-value-bind (min-vector max-vector)
	(first-guess-at-nd-cluster-means data mean-vectors)
      (format t "~&~%~%;; Input summary: ~%")
      (format t ";;   Number of data points: ~D~%" n-data-points)
      (format t ";;   Number of clusters requested: ~D~%" nclusters)
      (format t ";;   Variance of data: ~5,2F~%" (* sigma sigma))
      (pp-sf-vector min-vector :title ";;   Lower left:")
      (pp-sf-vector max-vector :title ";;   Upper right:")
      (format t ";;   Initial guess for cluster means:~%")
      (dotimes (j nclusters)
        (format t ";;     Cluster ~D: " (1+ j))
	(pp-sf-vector (aref mean-vectors j)))
      (terpri))

    (dotimes (iteration max-iterations)

      ;; E Step

      (setq far-out-list 
            (em-kmeans-nd-e-kernel
             data mean-vectors temp-vector
             relative-probability-vectors sigma))

      (when debug
        (dotimes (i 10)
          (format t ";; P(Datum ~2D): " (1+ i))
	  (pp-sf-vector (aref relative-probability-vectors i)))
        (terpri))

      ;; Now we have the relative probability that each datum belongs
      ;; to a given cluster.

      ;; M Step

      (setq no-cluster-info-list
            (em-kmeans-nd-m-kernel
             data mean-vectors relative-probability-vectors
             temp-vector far-out-list
             ))

      (when (and debug no-cluster-info-list)
        (dolist (cluster-info no-cluster-info-list)
          (format t ";; No cluster data found for cluster ~D~%"
                  (second cluster-info))
          (format t ";;   New guess, ~F, taken from ~A~%"
                  (third cluster-info) (first cluster-info))))

      (when verbose
        (format t ";; Guess for means after ~D iterations:~%" (1+ iteration))
        (dotimes (j nclusters)
          (format t ";;   Cluster ~D: " (1+ j))
	  (pp-sf-vector (aref mean-vectors j)))
        (terpri))

      ;; We've computed a new guess for MEAN-VECTOR.  Now repeat.

      )

    ;; Report how many data elements in each cluster.

    (let ((membership-lists (make-array nclusters :initial-element nil)))
      (loop for datum across data
            for i from 0 do
	    (let* ((rpv (aref relative-probability-vectors i))
		   (max (reduce #'max rpv))
                   (cluster-index (position max rpv :test #'=)))
              (push (list datum max) 
                    (aref membership-lists cluster-index)
                    )))
      (format t "~%;; Number of data points in each cluster:~%")
      (loop for members across membership-lists
            for j from 1 do
            (format t ";;   Cluster ~D: ~D~%" j (length members))
            )
      (terpri)
      )
                  
    (values mean-vectors)

    ))


(defun em-kmeans-nd-e-kernel 
       (data mean-vectors x-minus-m-vector rpvs sigma)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (declare (single-float sigma))

  (let ((far-out-list nil) 
        (n-data-points (length data))
        (nclusters (length mean-vectors))
	(exp-constant (* (/ -1.0 2.0) (/ 1.0 sigma)))
	(exponent 0.0)
	)
    (declare (fixnum n-data-points nclusters))
    (declare (single-float exp-constant exponent))

    ;; For each datum, calculate the relative probability that
    ;; it belongs to the Jth cluster.  To do this we evaluate
    ;; the PDF for the datum given the cluster's mean (sigma is fixed),
    ;; which is MEAN-VECTORS[j].
    ;; We do this for each cluster.  Then the relative probability
    ;; for cluster J is just the PDF value at J divided by the
    ;; sum of the PDF's for all the clusters.

    ;; It could be that a particular datum is so far from the
    ;; current guess of the mean that the pdf formula returns 0.
    ;; This is okay -- however, if the datum is so far from EVERY
    ;; guessed mean that all the pdf's return zero we would be
    ;; in trouble.  In this case we don't do the division to
    ;; calculate the relative probabilities; rather, we set the relative
    ;; probabilities to 0.0, which has the effect of ignoring this
    ;; data point for this iteration.

    (loop for i fixnum below n-data-points do

	  (let ((data-vector (aref data i))
		(rpv (aref rpvs i))
                (sum-of-weights 0.0))
	    (declare (type sfa1 data-vector rpv))
            (declare (single-float sum-of-weights))

	    (loop for j fixnum below nclusters do

		  ;; Compute (X-M) * INVERSE(COVARIANCE MATRIX) * (X-M)
		  ;; which is a scalar.

		  ;; Now, the covariance matrix is, in this case, simply
		  ;; a diagonal matrix whose elements are all SIGMA.
		  ;; So the inverse is a diagonal matrix with elements
		  ;; that are all 1/SIGMA.

		  ;; The result is then simply the dot-product of X-M
		  ;; with itself times 1/SIGMA

		  ;; (Note that we don't worry about the constant factor
		  ;; in front of the e**exponent term in the n-dimensional
		  ;; Gaussian distribution formula.  That's because we 
		  ;; normalize the results and it would just get divided 
		  ;; out by this normalization anyway)

		  (let ((cluster-mean (aref mean-vectors j)))
		    (declare (type sfa1 cluster-mean))
		    (sf-v-difference x-minus-m-vector data-vector cluster-mean)
		    (setq exponent 
		          (* exp-constant 
			     (the single-float
			          (sf-inner-product
                                   x-minus-m-vector x-minus-m-vector))))
		    (setf (aref rpv j) (exp exponent))
                    (incf sum-of-weights (aref rpv j))
		    ))

	    ;; Compute the relative probabilities, or leave as all zeroes.

            (if (/= 0.0 sum-of-weights)
                (sf-v-quotient rpv rpv sum-of-weights)
              (push data-vector far-out-list))

            ))

    far-out-list

    ))

(defun em-kmeans-nd-m-kernel 
    (data mean-vectors rpvs temp-vector far-out-list)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (declare (type sfa1 temp-vector))
  ;; Calculate a new guess for each of the MEAN-VECTORS, based on 
  ;; relative probabilities.  The new guess is the weighted average
  ;; of the data, weighted by these relative probabilities we
  ;; computed in the E step.
  (let ((n-data-points (length data))
        (nclusters (length mean-vectors))
        (no-cluster-info-list nil))
    (declare (fixnum n-data-points nclusters))
    (loop for j fixnum below nclusters do
	  (let ((mean-vector (aref mean-vectors j))
		(sum-of-weights 0.0))
	    (declare (single-float sum-of-weights))
	    (init-array mean-vector 0.0)
	    (loop for i fixnum below n-data-points do
		  (let* ((data-vector (aref data i))
			 (rpv (aref rpvs i))
			 (rcp (aref rpv j)))
		    (declare (single-float rcp))
		    (sf-v-product temp-vector data-vector rcp)
		    (sf-v-add mean-vector mean-vector temp-vector) 
		    (incf sum-of-weights rcp)
		    ))
	    ;; If the sum of weights isn't 0.0, then normalize the new guess
	    ;; by the sum of the weights.
	    ;; If all the weights for a cluster are 0.0, all the data
	    ;; was very, very far away from our guess for the mean for that
	    ;; cluster.  So guess again, this time picking one of the data
	    ;; points at random, either from the list of points that are 
	    ;; very far away from every cluster mean guess, or at random 
	    ;; if necessary.
	    (if (not (zerop sum-of-weights))
		(sf-v-quotient mean-vector mean-vector sum-of-weights)
	      (cond
	       (far-out-list
		(sf-copy-vector 
		 mean-vector (elt far-out-list (random (length far-out-list))))
		(push (list 'far-out j mean-vector) no-cluster-info-list))
	       (t
		(sf-copy-vector mean-vector (aref data (random n-data-points)))
		(push (list 'random j mean-vector) no-cluster-info-list)))
	      )))
    no-cluster-info-list
    ))



;;;; GOO FOR ANALYZING ASSEMBLER OUTPUT
;;;; ONLY WORKS FOR LISPWORKS
;;;; Works for SBCL too - mas, 9/23/03

#+disassemble
(progn
(defun disassembly (f)
  (with-output-to-string (p)
    (let ((*standard-output* p)) (disassemble f))))

(defun lines-of-assembler (f) (count #\Newline (disassembly f)))

(defun find-assembler-call-lines 
       (assembly-output &optional (search-string "call"))
  (let ((call-lines nil)
        (start 0))
    (loop
     (let ((endpos (position #\Newline assembly-output :start start)))
       (when (null endpos) (return))
       (when (search search-string assembly-output :start2 start :end2 endpos)
         (push (subseq assembly-output start endpos) call-lines))
       (setq start (1+ endpos))
       (when (>= start (length assembly-output)) (return))))
    (nreverse call-lines)
    ))
         
(defun assembler-call-comment (assembler-line)
  (let ((comment-pos (position #\; assembler-line)))
    (if (or (null comment-pos) (= comment-pos (1- (length assembler-line))))
        ""
      (string-trim '(#\Space #\Tab) (subseq assembler-line (1+ comment-pos)))
      )))

(defun assembler-calls-to (f)
  (format t "~&~%;; Function calls in compiled code for function ~A:~%" f)
  (mapcar
   #'(lambda (x) (format t ";;   ~A~%" x))
   (mapcar #'assembler-call-comment 
           (find-assembler-call-lines (disassembly f))
           ))
  (terpri)
  (values))
              
(defun assembler-report (f)
  (format t "~&~%;; Lines of assembler for function ~A: ~D~%" 
          f (lines-of-assembler f))
  (assembler-calls-to f)
  )
)




