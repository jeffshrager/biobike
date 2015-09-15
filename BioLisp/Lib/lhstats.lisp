;;; This comes in large part from Larry Hunter but has been significantly
;;; augmented by Jeff Shrager, partly from his own stats libraries, and 
;;; partly from open source stuff borrowed from around the web.  Citations
;;; and acknowledgements are given where possible.

;;; ---------------------- Larry Hunter's Code ----------------------

;;; Statistical functions in Common Lisp.  Version 1.01 July 27, 2001
;;;
;;; This code is copyright (c) 2000 by Larry Hunter (Larry.Hunter@uchsc.edu)
;;; except where otherwise noted.

;;; Thanks to Paul Cohen for the original CLASP package.  Thanks also to bug
;;; reports from Rob St. Amant.
;;;
;;; This program is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by the
;;; Free Software Foundation; either version 2 of the License, or (at your
;;; option) any later version.

;;; This program is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.

;;; You should have received a copy of the GNU General Public License along
;;; with this program; if not, write to the Free Software Foundation, Inc.,
;;; 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA

;;; The formulas and methods used are largely taken from Bernard Rosner,
;;; "Fundamentals of Biostatistics," 5th edition.  "Rosner x" is a page
;;; number.  Some numeric functions were taken from CLASP, a 1994 common
;;; lisp package that implemented some of the statistical functions from
;;; "Numeric recipes in C" For CLASP functions, see copyright notice below.

;;;  These abreviations used in function and variable names:
;;;    ci = confidence interval
;;;    cdf = cumulative density function
;;;    ge = greater than or equal to
;;;    le = less than or equal to
;;;    pdf = probability density function
;;;    sd = standard deviation
;;;    rxc = rows by columns
;;;    sse = sample size estimate


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Functions provided:
;;;
;;;  Descriptive statistics
;;;   Mean, median, mode, geometric mean, range, percentile, variance,
;;;   standard-deviation (sd), coefficient-of-variation,
;;;   standard-error-of-the-mean
;;;
;;;  Distributional functions
;;;   Poisson & Binomial
;;;    binomial-probability, binomial-cumulative-probability,
;;;    binomial-ge-probability, poisson-probability,
;;;    poisson-cumulative-probability, poisson-ge-probability, Normal
;;;    normal-pdf, convert-to-standard-normal, phi, z, t-distribution,
;;;    chi-square, chi-square-cdf
;;;
;;;  Confidence Intervals

;;;   binomial-probability-ci, poisson-mu-ci, normal-mean-ci,
;;;   normal-mean-ci-on-sequences, normal-variance-ci,
;;;   normal-variance-ci-on-sequence, normal-sd-ci
;;;
;;;  Hypothesis tests (parametric)
;;;   z-test, z-test-on-sequence, t-test-one-sample,
;;;   t-test-one-sample-on-sequence, t-test-paired,
;;;   t-test-paired-on-sequences, t-test-two-sample,
;;;   t-test-two-sample-on-sequences, chi-square-test-one-sample, f-test,
;;;   binomial-test-one-sample, binomial-test-two-sample, fisher-exact-test,
;;;   mcnemars-test, poisson-test-one-sample
;;;
;;;  Hypothesis tests (non-parametric)
;;;   sign-test, sign-test-on-sequence, wilcoxon-signed-rank-test,
;;;   chi-square-test-rxc, chi-square-test-for-trend

;;;  Sample size estimates
;;;   t-test-one-sample-sse, t-test-two-sample-sse
;;;   t-test-paired-sse, binomial-test-one-sample-sse,
;;;   binomial-test-two-sample-sse,
;;;   binomial-test-paired-sse, correlation-sse

;;;  Correlation and Regression
;;;   linear-regression, correlation-coefficient,
;;;   correlation-test-two-sample, spearman-rank-correlation

;;;  Significance test functions
;;;   t-significance, f-significance (chi square significance is calculated
;;;   from chi-square-cdf in various ways depending on the problem).

;;;  Utilities
;;;   random-sample, random-pick, bin-and-count, fishers-z-transform,
;;;   mean-sd-n, square, choose, permutations, round-float


(eval-when (:compile-toplevel)
  (declaim (optimize (speed 3) (safety 1) (debug 1))))

(defpackage :statistics
  (:nicknames :stats)
  (:use :common-lisp)
  (:export #:mean #:median #:mode #:geometric-mean #:range #:percentile
           #:variance #:standard-deviation #:sd #:coefficient-of-variation
           #:standard-error-of-the-mean #:permutations #:choose
           #:binomial-probability #:binomial-cumulative-probability
           #:binomial-ge-probability #:poisson-probability
           #:poisson-cumulative-probability #:poisson-ge-probability
           #:poisson-cdf #:normal-pdf #:convert-to-standard-normal #:phi #:z
           #:t-distribution #:chi-square #:chi-square-cdf
           #:binomial-probability-ci #:poisson-mu-ci #:normal-mean-ci
           #:normal-mean-ci-on-sequence #:normal-variance-ci
           #:normal-variance-ci-on-sequence #:normal-sd-ci
           #:normal-sd-ci-on-sequence #:z-test #:z-test-on-sequence 
           #:t-test-one-sample #:t-test-one-sample-on-sequence
           #:t-test-paired #:t-test-paired-on-sequences #:t-test-two-sample
           #:t-test-two-sample-on-sequences #:chi-square-test-one-sample
           #:f-test #:binomial-test-one-sample #:binomial-test-two-sample
           #:fisher-exact-test #:mcnemars-test #:poisson-test-one-sample
           #:sign-test #:sign-test-on-sequences #:wilcoxon-signed-rank-test
           #:wilcoxon-signed-rank-test-on-sequences
           #:chi-square-test-rxc #:chi-square-test-for-trend
           #:t-test-one-sample-sse #:t-test-two-sample-sse
           #:t-test-paired-sse #:binomial-test-one-sample-sse
           #:binomial-test-two-sample-sse #:binomial-test-paired-sse
           #:correlation-sse #:linear-regression #:correlation-coefficient
           #:correlation-test-two-sample
           #:correlation-test-two-sample-on-sequences #:spearman-rank-correlation
           #:t-significance #:f-significance #:random-sample #:random-pick #:test-variables
           #:bin-and-count #:fisher-z-transform #:mean-sd-n #:square
           #:round-float #:false-discovery-correction
           #:random-normal))

(in-package :statistics)

;;;;; Macros

;; This macro makes assertions more readable.  There are several special
;; types defined: :probability (:prob), :positive-integer (:posint),
;; :positive-number (:posnum), :number-sequence (:numseq),
;; :positive-integer-sequence (:posintseq), :probability-sequence
;; (:probseq), :nonzero-number-sequence (:nonzero-numseq) and :percentage
;; (:percent).  Other assertions are assumed to be internal types.  The
;; arguments to test-variables are lists.  The first element of the list is
;; a variable name, and the second element is either a special or built-in
;; type.  If the variable binding is not of the type specified, and error is
;; signalled indicating the problem.  One variable may have multiple type
;; requirements, which are conjunctive.

(eval-when (:compile-toplevel :load-toplevel :execute)

(defmacro test-variables (&rest args)
  (let ((assertions nil))
    (dolist (arg args (append `(or ,@(nreverse assertions))))
      (let* ((name (first arg))
             (type (second arg))
             (test (case type
                     ((:probability :prob)
                      `(and (numberp ,name) (not (minusp ,name)) (<= ,name 1)))
                     ((:positive-integer :posint)
                      `(and (integerp ,name) (plusp ,name)))
                     ((:positive-number :posnum)
                      `(and (numberp ,name) (plusp ,name)))
                     ((:number-sequence :numseq)
                      `(and (typep ,name 'sequence) (every #'numberp ,name)
                            (not (null ,name))))
                     ((:nonzero-number-sequence :nonzero-numseq)
                      `(and (typep ,name 'sequence) (not (null ,name))
                        (every #'(lambda (x) (and (numberp x) (not (= 0 x))))
                         ,name)))
                     ((:probability-sequence :probseq)
                      `(and (typep ,name 'sequence) (not (null ,name))
                        (every #'(lambda (x) (and (numberp x) (not (minusp x))
                                                  (<= x 1.0))) ,name)))
                     ((:positive-integer-sequence :posintseq)
                      `(and (typep ,name 'sequence) (not (null ,name))
                        (every #'(lambda (x) (and (typep x 'integer) (plusp
                                                                      x)))
                         ,name)))
                     (:percentage
                      `(and (numberp ,name) (plusp ,name) (<= ,name 100)))
                     (:test (third arg))
                     (t `(typep ,name ',type))))
             (message `(error
                        ,(if (eql type :test)
                             name
                             (format nil "~a = ~~a is not a ~a" name
                                     (case type
                                       ((:positive-integer :posint)
                                        "positive integer")
                                       ((:positive-number :posnum)
                                        "positive number")
                                       ((:probability :prob) "probability")
                                       ((:number-sequence :numseq)
                                        "sequence of numbers")
                                       ((:nonzero-number-sequence
                                         :nonzero-numseq)
                                        "sequence of non-zero numbers")
                                       ((:positive-integer-sequence :posintseq)
                                        "sequence of positive integers")
                                       ((:probability-sequence :probseq)
                                        "sequence of probabilities")
                                       ((:percent :percentile) "percent")
                                       (t type))))
                        ,name)))
        (push `(unless ,test ,message) assertions)))))


;; SQUARE

(defmacro square (x)
  `(* ,x ,x))


(defmacro underflow-goes-to-zero (&body body)
  "Protects against floating point underflow errors and sets the value to 0.0 instead."
  `(handler-case 
       (progn ,@body)
       (floating-point-underflow (condition)
				 (declare (ignore condition))
				 (values 0.0d0))))


) ;end eval-when

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Descriptive statistics
;;;

;; MEAN 
;; Rosner 10 

(defun mean (sequence)
  (test-variables (sequence :numseq))
  (/ (reduce #'+ sequence) (length sequence)))

;; MEDIAN
;; Rosner 12 (and 19)

(defun median (sequence)
  (test-variables (sequence :numseq))
  (percentile sequence 50))

;; MODE
;; Rosner 14
;; returns two values: a list of the modes and the number of times they
;; occur.   Rob St. Amant <stamant@csc.ncsu.edu> suggested using a hash
;; table instead of an alist.

(defun mode (sequence)
  (test-variables (sequence :numseq))
  (let ((count-table (make-hash-table))
        mode (mode-count 0))
    (map nil (lambda (elt) (incf (gethash elt count-table 0))) sequence)
    (maphash (lambda (key value)
               (when (> value mode-count)
                 (setf mode key
                       mode-count value)))
             count-table)
    (values mode mode-count)))


;; GEOMETRIC-MEAN
;; Rosner 16

(defun geometric-mean (sequence &optional (base 10))
  (test-variables (sequence :nonzero-numseq) (base :posnum))
  (expt base (mean (map 'list #'(lambda (x) (log x base)) sequence))))

;; RANGE
;; Rosner 18

(defun range (sequence)
  (test-variables (sequence :numseq))
  (- (reduce #'max sequence) (reduce #'min sequence)))

;; PERCENTILE
;; Rosner 19
;; NB: Aref is 0 based!

(defun percentile (sequence percent)
  (test-variables (sequence :numseq) (percent :percentage))
  (let* ((sorted-vect (coerce (sort (copy-seq sequence) #'<) 'simple-vector))
         (n (length sorted-vect))
         (k (* n (/ percent 100)))
         (floor-k (floor k)))
    (if (= k floor-k)
        (/ (+ (aref sorted-vect k)
              (aref sorted-vect (1- k)))
           2)
        (aref sorted-vect floor-k))))
      
;; VARIANCE
;; Rosner 21

(defun variance (sequence)
  (test-variables (sequence :numseq))
  (let ((mean (mean sequence))
        (n (length sequence)))
    (/ (reduce #'+ (map 'list #'(lambda (x) (square (- mean x))) sequence))
       (1- n))))

;; STANDARD-DEVIATION (SD)
;; Rosner 21

(defun standard-deviation (sequence)
  (test-variables (sequence :numseq))
  (let ((mean (mean sequence))
        (n (length sequence)))
    (sqrt (/ (reduce #'+ (map 'list #'(lambda (x) (square (- mean x))) sequence))
             (1- n)))))


(defun sd (sequence)
  (test-variables (sequence :numseq))
  (let ((mean (mean sequence))
        (n (length sequence)))
    (sqrt (/ (reduce #'+ (map 'list #'(lambda (x) (square (- mean x))) sequence))
             (1- n)))))


;; COEFFICIENT-OF-VARIATION
;; Rosner 24

(defun coefficient-of-variation (sequence)
  (* 100 (/ (standard-deviation sequence) (mean sequence))))

;; STANDARD-ERROR-OF-THE-MEAN
;; Rosner 172

(defun standard-error-of-the-mean (sequence)
  (/ (standard-deviation sequence) (sqrt (length sequence))))
(defun standard-error (sequence)
  (standard-error-of-the-mean sequence))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Distributional functions
;;;

;;;
;;; Binomial and Poisson distributions
;;;

;; BIONOMIAL-PROBABILITY 
;; exact: Rosner 93, approximate 105

;; P(X=k) for X a binomial random variable with parameters n & p.
;; Binomial expectations for seeing k events in N trials, each having
;; probability p.  Use the Poisson approximation if N>100 and P<0.01. 

(defun binomial-probability (n k p)
  (test-variables (n :posint) (p :prob) 
                  ("K must be between 0 and N (inclusive)" :test (and (>= k 0) (<= k n))))
  (if (and (> n 100) (< p 0.01))
      (poisson-probability (* n p) k)
      (let ((p (coerce p 'double-float)))
        (* (choose n k)
           (expt p k)
           (expt (- 1 p) (- n k))))))

;; BINOMIAL-CUMULATIVE-PROBABILITY
;; Rosner 94

;; P(X<k) for X a binomial random variable with parameters n & p.
;; Bionomial expecations for fewer than k events in N trials, each having
;; probability p.

(defun binomial-cumulative-probability (n k p)
  (test-variables (n :posint) (k :posint) (p :prob)
             ("K must be less than or equal to N" :test (<= k n)))
  (let ((sum-up-to-k-1 0d0))
    (dotimes (i k sum-up-to-k-1)
      (incf sum-up-to-k-1 (binomial-probability n i p)))))

;; BINOMIAL-GE-PROBABILITY
;; Rosner 94

;; The probability of k or more occurances in N events, each with
;; probability p.

(defun binomial-ge-probability (n k p)
  (- 1 (binomial-cumulative-probability n k p)))

;; Just for convenience later, binomial-le-probability

(defun binomial-le-probability (n k p)
  (test-variables (n :posint) (k :posint) (p :prob)
             ("K must be less than or equal to N" :test (<= k n)))
  (let ((sum-up-to-k 0d0))
    (dotimes (i (1+ k) sum-up-to-k)
      (incf sum-up-to-k (binomial-probability n i p)))))


;; POISSON-PROBABILITY
;; Rosner 100

;; Probability of seeing k events over a time period when the expected
;; number of events over that time is mu.

(defun poisson-probability (mu k)
  (test-variables (mu :posnum) (k :posint))
  (let ((mu (coerce mu 'double-float)))
    (/ (* (exp (- mu)) (expt mu k))
       (factorial k))))

;; POISSON-CUMULATIVE-PROBABILITY
;; Rosner 197

;; Probability of seeing fewer than K events over a time period when the
;; expected number events over that time is mu.

(defun poisson-cumulative-probability (mu k)
  (test-variables (mu :posnum) (k :posint))
  (if (< k 170)
      (let ((sum 0d0))
        (dotimes (x k sum)
          (incf sum (poisson-probability mu x))))
      (let ((mu (coerce mu 'double-float))
            (k (coerce k 'double-float)))
        (- 1d0 (gamma-incomplete k mu)))))
  

;; POISSON-GE-PROBABILITY
;; Probability of X or more events when expected is mu.

(defun poisson-ge-probability (mu x)
  (- 1 (poisson-cumulative-probability mu x)))

;;;
;;;  Normal distributional functions
;;;

;; NORMAL-PDF
;; The probability density function (PDF) for a normal distribution with
;; mean mu and variance sigma at point x.

;; Rosner 115

(defun Normal-pdf (x mu sigma)
  (test-variables (x number) (mu number) (sigma :posnum))
  (* (/ (* (sqrt (* 2 pi)) sigma))
     (exp (* (- (/ (* 2 (square sigma)))) (square (- x mu))))))


;; CONVERT-TO-STANDARD-NORMAL
;; Rosner 130
;; Convert X from a Normal distribution with mean mu and variance sigma to
;; standard normal

(defun convert-to-standard-normal (x mu sigma)
  (test-variables (x number) (mu number) (sigma :posnum))
  (/ (- x mu) sigma))

;; PHI
;; the CDF of standard normal distribution
;; Rosner 125

(defun phi (x)
  "Adopted from CLASP 1.4.3, see copyright notice at <a href=\"http://eksl-www.cs.umass.edu/clasp.html\">http://eksl-www.cs.umass.edu/clasp.html</a>"
  (test-variables (x number))
  (* .5 (+ 1.0 (error-function (/ x (sqrt 2.0))))))

;; Z
;; The inverse normal function, P(X<Zu) = u where X is distributed as the
;; standard normal.  Uses binary search.
;; Rosner 128. 

(defun z (percentile &key (epsilon 1d-15))
  (test-variables (percentile :prob))
  (let ((target (coerce percentile 'double-float)))
    (do ((min -9d0 min)
         (max 9d0 max)
         (guess 0d0 (+ min (/ (- max min) 2d0))))
        ((< (- max min) epsilon) guess)
      (let ((result (coerce (phi guess) 'double-float)))
        (if (< result target)
            (setq min guess)
            (setq max guess))))))
            
;; T-DISTRIBUTION
;; Rosner 178
;; Returns the point which is the indicated percentile in the T distribution
;; with dof degrees of freedom

(defun t-distribution (dof percentile)
  "Adopted from CLASP 1.4.3, <a href=\"http://eksl-www.cs.umass.edu/clasp.html\">http://eksl-www.cs.umass.edu/clasp.html</a>"
  (test-variables (dof :posint) (percentile :prob))
  (find-critical-value
   #'(lambda (x) (t-significance x dof :tails :positive))
   (- 1 percentile)))


;; CHI-SQUARE
;; Rosner 187
;; Returns the point which is the indicated percentile in the Chi Square
;; distribution with dof degrees of freedom.

(defun chi-square (dof percentile)
  (test-variables (dof :posint) (percentile :prob))
  (find-critical-value #'(lambda (x) (chi-square-cdf x dof))
                       (- 1 percentile)))

;; Chi-square-cdf computes the left hand tail area under the chi square
;; distribution under dof degrees of freedom up to X. 

(defun chi-square-cdf (x dof)
  "Adopted from CLASP 1.4.3, <a href=\"http://eksl-www.cs.umass.edu/clasp.html\">http://eksl-www.cs.umass.edu/clasp.html</a>"
  (test-variables (x :posnum) (dof :posint))
  (multiple-value-bind (cdf ignore)
      (gamma-incomplete (* 0.5 dof) (* 0.5 x))
    (declare (ignore ignore))
    cdf))
   
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Confidence intervals
;;;

;; BINOMIAL-PROBABILITY-CI
;; Rosner 193 (approximate) & 194 (exact)

;; Confidence intervals on a binomial probability.  If a binomial
;; probability of p has been observed in N trials, what is the 1-alpha
;; confidence interval around p?  Approximate (using normal theory
;; approximation) when npq >= 10 unless told otherwise

(defun binomial-probability-ci (n p alpha &key exact?)
  (test-variables (n :posint) (p :prob) (alpha :prob))
  (if (and (> (* n p (- 1 p)) 10) (not exact?))
      (let ((difference (* (z (- 1 (/ alpha 2)))
                           (sqrt (/ (* p (- 1 p)) n)))))
        (values (- p difference) (+ p difference)))
      (values (find-critical-value
               (lambda (p1) (binomial-cumulative-probability n (floor (* p n)) p1))
               (- 1 (/ alpha 2)))
              (find-critical-value
               (lambda (p2) (binomial-cumulative-probability n (1+ (floor (* p n))) p2))
               (/ alpha 2)))))

;; POISSON-MU-CI
;; Confidence interval for the Poisson parameter mu
;; Rosner 197
;;
;; Given x observations in a unit of time, what is the 1-alpha confidence
;; interval on the Poisson parameter mu (= lambda*T)?
;;
;; Since find-critical-value assumes that the function is monotonic
;; increasing, adjust the value we are looking for taking advantage of
;; reflectiveness

(defun poisson-mu-ci (x alpha)
  (test-variables (x :posnum) (alpha :prob))
  (values
   (find-critical-value
    #'(lambda (mu) (poisson-cumulative-probability mu (1- x)))
    (- 1 (/ alpha 2)))
   (find-critical-value
    #'(lambda (mu) (poisson-cumulative-probability mu x))
    (/ alpha 2))))
                 

;; NORMAL-MEAN-CI
;; Confidence interval for the mean of a normal distribution
;; Rosner 180

;; The 1-alpha percent confidence interval on the mean of a normal
;; distribution with parameters mean, sd & n. 

(defun normal-mean-ci (mean sd n alpha)
  (test-variables (mean number) (sd :posnum) (n :posint) (alpha :prob))
  (let ((t-value (t-distribution (1- n) (- 1 (/ alpha 2)))))
    (values (- mean (* t-value (/ sd (sqrt n))))
            (+ mean (* t-value (/ sd (sqrt n)))))))

;; NORMAL-MEAN-CI-ON-SEQUENCE
;;
;; The 1-alpha confidence interval on the mean of a sequence of numbers
;; drawn from a Normal distribution.

(defun normal-mean-ci-on-sequence (sequence alpha)
  (test-variables (alpha :prob)) ; sequence tested by mean-sd-n
  (multiple-value-call #'normal-mean-ci (mean-sd-n sequence) alpha)) 

;; NORMAL-VARIANCE-CI
;; Rosner 190
;; The 1-alpha confidence interval on the variance of a sequence of numbers
;; drawn from a Normal distribution.

(defun normal-variance-ci (variance n alpha)
  (test-variables (variance :posnum) (n :posint) (alpha :prob))
  (let ((chi-square-low (chi-square (1- n) (- 1 (/ alpha 2))))
        (chi-square-high (chi-square (1- n) (/ alpha 2)))
        (numerator (* (1- n) variance)))
    (values (/ numerator chi-square-low) (/ numerator chi-square-high))))

;; NORMAL-VARIANCE-CI-ON-SEQUENCE

(defun normal-variance-ci-on-sequence (sequence alpha)
  (test-variables (sequence :numseq) (alpha :prob))
  (let ((variance (variance sequence))
        (n (length sequence)))
    (normal-variance-ci variance n alpha)))

;; NORMAL-SD-CI
;; Rosner 190
;; As above, but a confidence inverval for the standard deviation.

(defun normal-sd-ci (sd n alpha)
  (multiple-value-bind (low high)
      (normal-variance-ci (square sd) n alpha)
    (values (sqrt low) (sqrt high))))

;; NORMAL-SD-CI-ON-SEQUENCE

(defun normal-sd-ci-on-sequence (sequence alpha)
  (test-variables (sequence :numseq) (alpha :prob))
  (let ((sd (standard-deviation sequence))
        (n (length sequence)))
    (normal-sd-ci sd n alpha)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Hypothesis testing
;;;

;; Z-TEST
;; Rosner 228

;; The significance of a one sample Z test for the mean of a normal
;; distribution with known variance.  mu is the null hypothesis mean, x-bar
;; is the observed mean, sigma is the standard deviation and N is the number
;; of observations.  If tails is :both, the significance of a difference
;; between x-bar and mu.  If tails is :positive, the significance of x-bar
;; is greater than mu, and if tails is :negative, the significance of x-bar
;; being less than mu.  Returns a p value.

(defun z-test (x-bar n &key (mu 0) (sigma 1) (tails :both))
  (test-variables (x-bar number) (n :posint) (mu number) (sigma :posnum))
  (let ((z (/ (- x-bar mu) (/ sigma (sqrt n)))))
    (ecase tails
      (:both (if (< z 0)
                 (* 2 (phi z))
                 (* 2 (- 1 (phi z)))))
      (:negative (phi z))
      (:positive (- 1 (phi z))))))

;; Z-TEST-ON-SEQUENCE

(defun z-test-on-sequence (sequence &key (mu 0) (sigma 1) (tails :both))
  (test-variables (sequence :numseq)) ; the rest handled by z-test
  (let ((x-bar (mean sequence))
        (n (length sequence)))
    (z-test x-bar n :mu mu :sigma sigma :tails tails)))

;; T-TEST-ONE-SAMPLE
;; T-TEST-ONE-SAMPLE-ON-SEQUENCE
;; Rosner 216

;; The significance of a one sample T test for the mean of a normal
;; distribution with unknown variance.  X-bar is the observed mean, sd is
;; the observed standard deviation, N is the number of observations and mu
;; is the test mean.  -ON-SAMPLE is the same, but calculates the observed
;; values from a sequence of numbers.

(defun t-test-one-sample (x-bar sd n mu &key (tails :both))
  (test-variables (x-bar number) (sd :posnum) (n :posint) (mu number))
  (t-significance  (/ (- x-bar mu) (/ sd (sqrt n))) (1- n) :tails tails))

(defun t-test-one-sample-on-sequence (sequence mu &key (tails :both))
  (multiple-value-call #'t-test-one-sample
    (mean-sd-n sequence) mu :tails tails))

;; T-TEST-PAIRED
;; Rosner 276

;; The significance of a paired t test for the means of two normal
;; distributions in a longitudinal study.  D-bar is the mean difference, sd
;; is the standard deviation of the differences, N is the number of pairs. 

(defun t-test-paired (d-bar sd n &key (tails :both))
  (test-variables (d-bar number) (sd :posnum) (n :posint))
  (t-significance (/ d-bar (/ sd (sqrt n))) (1- n) :tails tails))

;; T-TEST-PAIRED-ON-SEQUENCES
;; Rosner 276

;; The significance of a paired t test for means of two normal distributions
;; in a longitudinal study.  Before is a sequence of before values, after is
;; the sequence of paired after values (which must be the same length as the
;; before sequence).

(defun t-test-paired-on-sequences (before after &key (tails :both))
  (test-variables (before :numseq) (after :numseq)
             ("Before and after sequences must be of equal length"
              :test (= (length before) (length after))))
  (multiple-value-call #'t-test-paired
     (mean-sd-n (map 'list #'- before after)) :tails tails))

;; T-TEST-TWO-SAMPLE
;; Rosner  282, 294, 297 

;; The significance of the difference of two means (x-bar1 and x-bar2) with
;; standard deviations sd1 and sd2, and sample sizes n1 and n2 respectively.
;; The form of the two sample t test depends on whether the sample variances
;; are equal or not.   If the variable variances-equal? is :test, then we
;; use an F test and the variance-significance-cutoff to determine if they
;; are equal.  If the variances are equal, then we use the two sample t test
;; for equal variances.  If they are not equal, we use the Satterthwaite
;; method, which has good type I error properties (at the loss of some
;; power).  

(defun t-test-two-sample (x-bar1 sd1 n1 x-bar2 sd2 n2 &key
                          (variances-equal? :test)
                          (variance-significance-cutoff 0.05)
                          (tails :both))
  (test-variables (x-bar1 number) (sd1 :posnum) (n1 :posint)
             (x-bar2 number) (sd2 :posnum) (n2 :posint))
  (let (t-statistic dof)
    (if (ecase variances-equal?
          (:test (> (f-test (square sd1) n1 (square sd2) n2 :tails tails)
                    variance-significance-cutoff))
          ((t :yes :equal) t)
          ((nil :no :unequal) nil))
        (let ((s (sqrt (/ (+ (* (1- n1) (square sd1))
                             (* (1- n2) (square sd2)))
                          (+ n1 n2 -2)))))
          (setq t-statistic  (/ (- x-bar1 x-bar2)
                                (* s (sqrt (+ (/ n1) (/ n2)))))
                dof (+ n1 n2 -2)))
        (let* ((variance-ratio1 (/ (square sd1) n1))
               (variance-ratio2 (/ (square sd2) n2)))
          (setq t-statistic (/ (- x-bar1 x-bar2)
                               (sqrt (+ variance-ratio1 variance-ratio2)))
                dof (round (/ (square (+ variance-ratio1 variance-ratio2))
                              (+ (/ (square variance-ratio1) (1- n1))
                                 (/ (square variance-ratio2) (1- n2))))))))
    (t-significance t-statistic dof :tails tails)))


  
;; T-TEST-TWO-SAMPLE-ON-SEQUENCES
;; Same as above, but providing the sequences rather than the summaries.

(defun t-test-two-sample-on-sequences (sequence1 sequence2 &key
                                       (variance-significance-cutoff 0.05)
                                       (tails :both))
  (multiple-value-call #'t-test-two-sample
    (mean-sd-n sequence1) (mean-sd-n sequence2) :tails tails
    :variance-significance-cutoff variance-significance-cutoff))


;; F-TEST
;; Rosner 290
;; F test for the equality of two variances

(defun f-test (variance1 n1 variance2 n2 &key (tails :both))
  (test-variables (variance1 :posnum) (n1 :posint) (variance2 :posnum) (n2 :posint))
  (let ((significance (f-significance (/ variance1 variance2) (1- n1) (1- n2)
                                      (not (eql tails :both)))))
    (ecase tails
      (:both significance)
      (:positive (if (> variance1 variance2) significance (- 1 significance)))
      (:negative (if (< variance1 variance2) significance (- 1 significance))))))


;; CHI-SQUARE-TEST-ONE-SAMPLE
;; Rosner 246
;; The significance of a one sample Chi square test for the variance of a
;; normal distribution.  Variance is the observed variance, N is the number
;; of observations, and sigma-squared is the test variance.

(defun chi-square-test-one-sample (variance n sigma-squared &key (tails :both))
  (test-variables (variance :posnum) (n :posint) (sigma-squared :posnum))
  (let ((cdf (chi-square-cdf (/ (* (1- n) variance) sigma-squared) (1- n))))
    (ecase tails
      (:negative cdf)
      (:positive (- 1 cdf))
      (:both (if (<= variance sigma-squared) 
                 (* 2 cdf)
                 (* 2 (- 1 cdf)))))))


;; BINOMIAL-TEST-ONE-SAMPLE
;; Rosner 249
;; The significance of a one sample test for the equality of an observed
;; probability p-hat to an expected probability p under a binomial
;; distribution with N observations.  Use the normal theory approximation if
;; n*p*(1-p) > 10 (unless the exact flag is true). 

(defun binomial-test-one-sample (p-hat n p &key (tails :both) (exact? nil))
  (test-variables (p-hat :prob) (n :posint) (p :prob))
  (let ((q (- 1 p)))
    (if (and (> (* n p q) 10) (not exact?))
        (let ((z (/ (- p-hat p) (sqrt (/ (* p q) n)))))
          (ecase tails
            (:negative (phi z))
            (:positive (- 1 (phi z)))
            (:both (* 2 (if (<= p-hat p) (phi z) (- 1 (phi z)))))))
        (let* ((observed (round (* p-hat n)))
               (probability-more-extreme
                (if (<= p-hat p)
                    (binomial-cumulative-probability n observed p)
                    (binomial-ge-probability n observed p))))
          (ecase tails
            ((:negative :positive) probability-more-extreme)
            (:both (min (* 2 probability-more-extreme) 1.0)))))))

;; BINOMIAL-TEST-TWO-SAMPLE
;; Rosner 357

;; Are the observed probabilities of an event (p-hat1 and p-hat2) in N1/N2
;; trials different? The normal theory method implemented here.  The exact
;; test is Fisher's contingency table method, below. 

(defun binomial-test-two-sample (p-hat1 n1 p-hat2 n2 &key (tails :both)
                                 (exact? nil))
  (test-variables (p-hat1 :prob) (n1 :posint) (p-hat2 :prob) (n2 :posint))
  (let* ((p-hat (/ (+ (* p-hat1 n1) (* p-hat2 n2)) (+ n1 n2)))
         (q-hat (- 1 p-hat))
         (z (/ (- (abs (- p-hat1 p-hat2)) (+ (/ (* 2 n1)) (/ (* 2 n2))))
               (sqrt (* p-hat q-hat (+ (/ n1) (/ n2)))))))
    (if (and (> (* n1 p-hat q-hat) 5)
             (> (* n2 p-hat q-hat) 5)
             (not exact?))
        (* (- 1 (phi z)) (if (eql tails :both) 2 1))
        (let ((contingency-table (make-array '(2 2))))
          (setf (aref contingency-table 0 0) (* p-hat1 n1)
                (aref contingency-table 0 1) (- 1 (* p-hat1 n1))
                (aref contingency-table 1 0) (* p-hat2 n2)
                (aref contingency-table 1 1) (- 1 (* p-hat2 n2)))
          (fisher-exact-test contingency-table :tails tails)))))
               
;; FISHER-EXACT-TEST
;; Rosner 371
;; Fisher's exact test.  Gives a p value for a particular 2x2 contingency table

(defun fisher-exact-test (contingency-table &key (tails :both))
  (flet ((table-probability (a b c d)
           (let ((n (+ a b c d)))
             (/ (* (factorial (+ a b)) (factorial (+ c d))
                   (factorial (+ a c)) (factorial (+ b d)))
                (* (factorial n) (factorial a) (factorial b)
                   (factorial c) (factorial d))))))

    (let ((a (aref contingency-table 0 0))
          (b (aref contingency-table 0 1))
          (c (aref contingency-table 1 0))
          (d (aref contingency-table 1 1)))
      (test-variables (a number) (b number) (c number) (d number))
      (let* ((row-margin1 (+ a b))
             (row-margin2 (+ c d))
             (column-margin1 (+ a c))
             (column-margin2 (+ b d))
             (n (+ a b c d))
             (table-probabilities
              (make-array (1+ (min row-margin1 row-margin2 column-margin1
                                   column-margin2)))))

        ;; rearrange so that the first row and column marginals are
        ;; smallest.  Only need to change first margins and a.
    
        (cond ((and (< row-margin2 row-margin1) (< column-margin2 column-margin1))
               (psetq a d 
                      row-margin1 row-margin2
                      column-margin1 column-margin2))
              ((< row-margin2 row-margin1)
               (psetq a c
                      row-margin1 row-margin2))
              ((< column-margin2 column-margin1)
               (psetq a b
                      column-margin1 column-margin2)))
        (dotimes (i (length table-probabilities))
          (let* ((test-a i)
                 (test-b (- row-margin1 i))
                 (test-c (- column-margin1 i))
                 (test-d (- n (+ test-a test-b test-c))))
            (setf (aref table-probabilities i)
                  (table-probability test-a test-b test-c test-d))))
        (let ((above (reduce #'+ (subseq table-probabilities 0 (1+ a))))
              (below (reduce #'+ (subseq table-probabilities a))))
          (float
           (ecase tails
             ((:both) (* 2 (min above below)))
             ((:positive) below)
             ((:negative) above))
           1d0))))))

;; MCNEMARS-TEST
;; Rosner 379 and 381

;; McNemar's test for correlated proportions, used for longitudinal
;; studies. Look only at the number of discordant pairs (one treatment is
;; effective and the other is not).  If the two treatments are A and B,
;; a-discordant-count is the number where A worked and B did not, and
;; b-discordant-count is the number where B worked and A did not.

(defun mcnemars-test (a-discordant-count b-discordant-count &key (exact? nil))
  (test-variables (a-discordant-count :posint) (b-discordant-count :posint))
  (let ((n (+ a-discordant-count b-discordant-count)))
    (if (and (> n 20) (not exact?))
        (let ((x2 (/ (square
                      (- (abs (- a-discordant-count b-discordant-count)) 1))
                     n)))
          (- 1 (chi-square-cdf x2 1)))
        (cond ((= a-discordant-count b-discordant-count) 1.0)
              ((< a-discordant-count b-discordant-count)
               (* 2 (binomial-le-probability n a-discordant-count 1/2)))
              (t (* 2 (binomial-ge-probability n a-discordant-count 1/2)))))))

;; POISSON-TEST-ONE-SAMPLE
;; Rosner 256 (approximation on 259)
;; The significance of a one sample test for the equality of an observed
;; number of events (observed) and an expected number mu under the poisson
;; distribution.  Normal theory approximation is not that great, so don't
;; use it unless told.

(defun poisson-test-one-sample (observed mu &key (tails :both) (approximate? nil))
  (test-variables (observed :posnum) (mu :posnum))
  (if approximate?
      (let ((x-square (/ (square (- observed mu)) mu)))
        (- 1 (chi-square-cdf x-square 1)))
      (let ((probability-more-extreme
             (if (< observed mu)
                 (poisson-cumulative-probability mu observed)
                 (poisson-ge-probability mu observed))))
        (ecase tails
          ((:negative :positive) probability-more-extreme)
          (:both (min (* 2 probability-more-extreme) 1.0))))))

;;;
;;; Non-parametric hypothesis testing
;;;

;; SIGN-TEST
;; Rosner 335-7.
;; Really just a special case of the binomial one sample test with p = 1/2.
;; The normal theory version has a correction factor to make it a better
;; approximation. 

(defun sign-test (plus-count minus-count &key (exact? nil) (tails :both))
  (test-variables (plus-count :posint) (minus-count :posint))
  (let* ((n (+ plus-count minus-count))
         (p-hat (/ plus-count n)))
    (if (or (< n 20) exact?)
        (binomial-test-one-sample p-hat n 0.5 :tails tails :exact? t)
        (let ((area (- 1 (phi (/ (1- (abs (- plus-count minus-count)))
                                 (sqrt n))))))
          (if (eql tails :both)
              (* 2 area)
              area)))))

;; SIGN-TEST-ON-SEQUENCE

;; Same as above, but takes two sequences and tests whether the entries in
;; one are different (greater or less) than the other.

(defun sign-test-on-sequences (sequence1 sequence2 &key (exact? nil) (tails :both))
  (test-variables (sequence1 :numseq) (sequence2 :numseq)
              ("Sequences must be of equal length"
               :test (= (length sequence1) (length sequence2))))
  (let* ((differences (map 'list #'- sequence1 sequence2))
         (plus-count (count #'plusp differences))
         (minus-count (count #'minusp differences)))
    (sign-test plus-count minus-count :exact? exact? :tails tails)))

;; WILCOXON-SIGNED-RANK-TEST
;; Rosner 341
;; A test on the ranking of positive and negative differences (are the
;; positive differences significantly larger/smaller than the negative
;; ones). Assumes a continuous and symmetric distribution of differences,
;; although not a normal one.  This is the normal theory approximation,
;; which is only valid when N > 15.

;; This test is completely equivalent to the Mann-Whitney test.

(defun wilcoxon-signed-rank-test (differences &optional (tails :both))
  (let* ((nonzero-differences (remove 0 differences :test #'=))
         (sorted-list (sort (mapcar #'(lambda (dif)
                                        (list (abs dif) (sign dif)))
                                    nonzero-differences)
                            #'<
                            :key #'first))
         (distinct-values (delete-duplicates (mapcar #'first sorted-list)))
         (ties nil))

    (when (< (length nonzero-differences) 16)
      (error "This Wilcoxon Signed-Rank Test (normal approximation method) requires nonzero N > 15"))

    (unless (member tails '(:positive :negative :both))
      (error "tails must be one of :positive, :negative or :both, not ~a" tails))
    
    ; add avg-rank to the sorted values
    
    (dolist (value distinct-values)
      (let ((first (position value sorted-list :key #'first))
            (last (position value sorted-list :key #'first :from-end t)))
        (if (= first last)
            (nconc (find value sorted-list :key #'first) (list (1+ first)))
            (let ((number-tied (1+ (- last first)))
                  (avg-rank (1+ (/ (+ first last) 2)))) ; +1 since 0 based
              (push number-tied ties)
              (dotimes (i number-tied)
                (nconc (nth (+ first i) sorted-list) (list avg-rank)))))))
    (setq ties (nreverse ties))
    (let* ((direction (if (eq tails :negative) -1 1))
           (r1 (reduce #'+ (mapcar #'(lambda (entry)
                                       (if (= (second entry) direction)
                                           (third entry)
                                           0))
                                   sorted-list)))
           (n (length nonzero-differences))
           (expected-r1 (/ (* n (1+ n)) 4))
           (ties-factor (if ties
                            (/ (reduce #'+ (mapcar #'(lambda (ti)
                                                       (- (* ti ti ti) ti))
                                                   ties))
                               48)
                            0))
           (var-r1 (- (/ (* n (1+ n) (1+ (* 2 n))) 24) ties-factor))
           (T-score (/ (- (abs (- r1 expected-r1)) 1/2) (sqrt var-r1))))
      (* (if (eq tails :both) 2 1) (- 1 (phi T-score))))))


(defun wilcoxon-signed-rank-test-on-sequences (sequence1 sequence2
                                               &optional (tails :both))
  (test-variables (sequence1 :numseq) (sequence2 :numseq)
              ("Sequences must be of equal length"
               :test (= (length sequence1) (length sequence2))))
  (wilcoxon-signed-rank-test (map 'list #'- sequence1 sequence2) tails))


;; CHI-SQUARE-TEST-RXC
;; Rosner 395
;; Takes contingency-table, an RxC array, and returns the significance of
;; the relationship between the row variable and the column variable.  Any
;; difference in proportion will cause this test to be significant --
;; consider using the test for trend instead if you are looking for a
;; consistent change.

(defun chi-square-test-rxc (contingency-table)
  (let* ((rows (array-dimension contingency-table 0))
         (columns (array-dimension contingency-table 1))
         (row-marginals (make-array rows :initial-element 0.0))
         (column-marginals (make-array columns :initial-element 0.0))
         (total 0.0)
         (expected-lt-5 0)
         (expected-lt-1 0)
         (expected-values (make-array (list rows columns)
                                      :element-type 'single-float))
         (x2 0.0))
    (dotimes (i rows)
      (dotimes (j columns)
        (let ((cell (aref contingency-table i j)))
          (incf (svref row-marginals i) cell)
          (incf (svref column-marginals j) cell)
          (incf total cell))))
    (dotimes (i rows)
      (dotimes (j columns)
        (let ((expected (/ (* (aref row-marginals i) (aref column-marginals j))
                           total)))
          (when (< expected 1) (incf expected-lt-1))
          (when (< expected 5) (incf expected-lt-5))
          (setf (aref expected-values i j) expected))))
    (when (plusp expected-lt-1)
      (error "This test cannot be used when an expected value is less than one"))
    (when (> expected-lt-5 (/ (* rows columns) 5))
      (error "This test cannot be used when more than 1/5 of the expected values are less than 5."))
    (dotimes (i rows)
      (dotimes (j columns)
        (incf x2 (/ (square (- (aref contingency-table i j)
                               (aref expected-values i j)))
                    (aref expected-values i j)))))
    (- 1 (chi-square-cdf x2 (* (1- rows) (1- columns))))))
                 
            
;; CHI-SQUARE-TEST-FOR-TREND
;; Rosner 398

;; This test works on a 2xk table and assesses if there is an increasing or
;; decreasing trend.  Arguments are equal sized lists counts.  Optionally,
;; provide a list of scores, which represent some numeric attribute of the
;; group.  If not provided, scores are assumed to be 1 to k.

(defun chi-square-test-for-trend (row1-counts row2-counts &optional scores)
  (unless scores (setq scores (dotimes (i (length row1-counts) (nreverse scores))
                                (push (1+ i) scores))))
  (test-variables (row1-counts :posintseq) (row2-counts :posintseq) (scores :numseq)
              ("Sequences must be of equal length"
               :test (= (length row1-counts) (length row2-counts))))
  (let* ((ns (map 'list #'+ row1-counts row2-counts))
         (p-hats (map 'list #'/ row1-counts ns))
         (n (reduce #'+ ns))
         (p-bar (/ (reduce #'+ row1-counts) n))
         (q-bar (- 1 p-bar))
         (s-bar (mean scores))
         (a (reduce #'+ (mapcar (lambda (p-hat ni s)
                                  (* ni (- p-hat p-bar) (- s s-bar)))
                                p-hats ns scores)))
         (b (* p-bar q-bar (- (reduce #'+ (mapcar (lambda (ni s) (* ni (square s)))
                                                  ns scores))
                              (/ (square (reduce #'+ (mapcar (lambda (ni s) (* ni s))
                                                             ns scores)))
                                 n))))
         (x2 (/ (square a) b))
         (significance (- 1 (chi-square-cdf (float x2) 1)))) 
    (when (< (* p-bar q-bar n) 5)
      (error "This test is only applicable when N * p-bar * q-bar >= 5"))
    (format t "~%The trend is ~a, p = ~f"
            (if (< a 0) "decreasing" "increasing")
            significance)
    significance))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Sample size estimates
;;;

;; T-TEST-ONE-SAMPLE-SSE
;; Rosner 238
;; Returns the number of subjects needed to test whether the mean of a
;; normally distributed sample mu is different from a null hypothesis mean
;; mu-null and variance variance, with alpha, 1-beta and tails as specified.

(defun t-test-one-sample-sse (mu mu-null variance &key
                                    (alpha 0.05) (1-beta .95) (tails :both))
  (test-variables (mu number) (mu-null number) (variance :posnum)
              (alpha :prob) (1-beta :prob))
  (let ((z-beta (z 1-beta))
        (z-alpha (z (- 1 (if (eql tails :both) (/ alpha 2) alpha)))))
    (round-up (/ (* variance (square (+ z-beta z-alpha)))
                (square (- mu-null mu))))))

;; T-TEST-TWO-SAMPLE-SSE
;; Rosner 308

;; Returns the number of subjects needed to test whether the mean mu1 of a
;; normally distributed sample (with variance variance1) is different from a
;; second sample with mean mu2 and variance variance2, with alpha, 1-beta
;; and tails as specified.  It is also possible to set a sample size ratio
;; of sample 1 to sample 2.

(defun t-test-two-sample-sse (mu1 variance1 mu2 variance2 &key
                                        (sample-ratio 1) (alpha 0.05)
                                        (1-beta .95) (tails :both))
  (test-variables (mu1 number) (variance1 :posnum) (mu2 number)
              (variance2 :posnum) (sample-ratio :posnum) (alpha :prob)
              (1-beta :prob))
  (let* ((delta2 (square (- mu1 mu2)))
         (z-term (square (+ (z 1-beta)
                           (z (- 1 (if (eql tails :both)
                                       (/ alpha 2)
                                       alpha))))))
         (n1 (round-up (/ (* (+ variance1 (/ variance2 sample-ratio)) z-term)
                          delta2)))) 
    (values n1 (round-up (* sample-ratio n1)))))

     
;; T-TEST-PAIRED-SSE
;; Rosner 311

;; Returns the number of subjects needed to test whether the differences
;; with mean difference-mu and variance difference-variance, with alpha,
;; 1-beta and tails as specified.

(defun t-test-paired-sse (difference-mu difference-variance
                                    &key (alpha 0.05) (1-beta 0.95)
                                    (tails :both))
  (test-variables (difference-mu number) (difference-variance :posnum)
              (alpha :prob) (1-beta :prob))
  (round-up (/ (* 2 difference-variance
                 (square (+ (z 1-beta)
                            (z (- 1 (if (eql tails :both)
                                        (/ alpha 2)
                                        alpha))))))
              (square difference-mu))))                                                              


;; BINOMIAL-TEST-ONE-SAMPLE-SSE
;; Rosner 254

;; Returns the number of subjects needed to test whether an observed
;; probability is significantly different from a particular binomial null
;; hypothesis with a significance alpha and a power 1-beta.

(defun binomial-test-one-sample-sse (p-estimated p-null &key
                                               (alpha 0.05) (1-beta 0.95)
                                               (tails :both))
  (test-variables (p-estimated :prob) (p-null :prob) (alpha :prob) (1-beta :prob))
  (let ((q-null (- 1 p-null))
        (q-estimated (- 1 p-estimated)))
    (round-up 
     (/ (* p-null q-null
           (square (+ (z (- 1 (if (eql tails :both) (/ alpha 2) alpha)))
                      (* (z 1-beta)
                         (sqrt (/ (* p-estimated q-estimated)
                                  (* p-null q-null)))))))
        (square (- p-estimated p-null))))))

;; BINOMIAL-TEST-TWO-SAMPLE-SSE
;; Rosner 384

;; The number of subjects needed to test if two binomial probabilities are
;; different at a given significance alpha and power 1-beta.  The sample
;; sizes can be unequal; the p2 sample is sample-sse-ratio * the size of
;; the p1 sample.  It can be a one tailed or two tailed test.  

(defun binomial-test-two-sample-sse (p1 p2 &key (alpha 0.05)
                                               (sample-ratio 1)
                                               (1-beta .95) (tails :both))
  (test-variables (p1 :prob) (p2 :prob) (alpha :prob) (1-beta :prob)
              (sample-ratio :posnum))
  (let* ((q1 (- 1 p1))
         (q2 (- 1 p2))
         (delta (abs (- p1 p2)))
         (p-bar (/ (+ p1 (* sample-ratio p2)) (1+ sample-ratio)))
         (q-bar (- 1 p-bar))
         (z-alpha (z (- 1 (if (eql tails :both) (/ alpha 2) alpha))))
         (z-beta (z 1-beta))
         (n1 (round-up 
              (/ (square (+ (* (sqrt (* p-bar q-bar (1+ (/ sample-ratio))))
                               z-alpha)
                            (* (sqrt (+ (* p1 q1) (/ (* p2 q2) sample-ratio)))
                               z-beta)))
                 (square delta)))))
    (values n1 (round-up (* sample-ratio n1)))))
    

;; BINOMIAL-TEST-PAIRED-SSE
;; Rosner 387

;; Sample size estimate for the McNemar (discordant pairs) test.  Pd is the
;; projected proportion of discordant pairs among all pairs, and Pa is the
;; projected proportion of type A pairs among discordant pairs.  alpha,
;; 1-beta and tails are as above.  Returns the number of individuals
;; necessary; that is twice the number of matched pairs necessary.

(defun binomial-test-paired-sse (pd pa &key (alpha 0.05)
                                 (1-beta 0.95) (tails :both))
  (test-variables (pd :prob) (pa :posnum) (alpha :prob) (1-beta :prob))
  (let ((qa (- 1 pa))
        (z-alpha (z (- 1 (if (eql tails :both) (/ alpha 2) alpha))))
        (z-beta (z 1-beta)))
    (round-up (/ (square (+ z-alpha (* 2 z-beta (sqrt (* pa qa)))))
                 (* 2 (square (- pa 1/2)) pd)))))
                                                                   
;; CORRELATION-SSE
;; Rosner 463
;;
;; Returns the size of a sample necessary to find a correlation of expected
;; value rho with significance alpha and power 1-beta. 

(defun correlation-sse (rho &key (alpha 0.05) (1-beta 0.95))
  (test-variables (rho :prob) (alpha :prob) (1-beta :prob))
  (round-up (+ 3 (/ (square (+ (z (- 1 alpha)) (z 1-beta)))
                    (square (fisher-z-transform rho))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Correlation and Regression
;;;

;; LINEAR-REGRESSION
;; Rosner 431, 441 for t-test

;; Computes the regression equation for a least squares fit of a line to a
;; sequence of points (each a list of two numbers, e.g. '((1.0 0.1) (2.0 0.2)))
;; and report the intercept, slope, correlation coefficient r, R^2, and the
;; significance of the difference of the slope from 0. 

(defun linear-regression (points)
  (test-variables (points sequence))
  (let  ((xs (map 'list #'first points))
         (ys (map 'list #'second points)))
    (test-variables (xs :numseq) (ys :numseq))
    (let* ((x-bar (mean xs))
           (y-bar (mean ys))
           (n (length points))
           (Lxx (reduce #'+ (mapcar (lambda (xi) (square (- xi x-bar))) xs)))
           (Lyy (reduce #'+ (mapcar (lambda (yi) (square (- yi y-bar))) ys)))
           (Lxy (reduce #'+ (mapcar (lambda (xi yi) (* (- xi x-bar) (- yi y-bar)))
                                    xs ys)))
           (b (/ Lxy Lxx))
           (a (- y-bar (* b x-bar)))
           (reg-ss (* b Lxy))
           (res-ms (/ (- Lyy reg-ss) (- n 2)))
           (r (/ Lxy (sqrt (* Lxx Lyy))))
           (r2 (/ reg-ss Lyy))
           (t-test (/ b (sqrt (/ res-ms Lxx)))) 
           (t-significance (t-significance t-test (- n 2) :tails :both)))
      (format t "~%Intercept = ~f, slope = ~f, r = ~f, R^2 = ~f, p = ~f"
              a b r r2 t-significance)
      (values a b r r2 t-significance)))) 

;; CORRELATION-COEFFICIENT
;; just r from above.  Also called Pearson Correlation

(defun correlation-coefficient (points)
  (test-variables (points sequence))
  (let ((xs (map 'list #'first points))
        (ys (map 'list #'second points)))
    (test-variables (xs :numseq) (ys :numseq))
    (let ((x-bar (mean xs))
          (y-bar (mean ys)))
      (/ (reduce #'+ (mapcar #'(lambda (xi yi) (* (- xi x-bar) (- yi y-bar)))
                             xs ys))
         (sqrt (* (reduce #'+ (mapcar #'(lambda (xi) (square (- xi x-bar)))
                                      xs))
                  (reduce #'+ (mapcar #'(lambda (yi) (square (- yi y-bar)))
                                      ys))))))))

;; CORRELATION-TEST-TWO-SAMPLE
;; Rosner 464
;; Test if two correlation coefficients are different.  Users Fisher's Z
;; test.

(defun correlation-test-two-sample (r1 n1 r2 n2 &key (tails :both))
  (test-variables (r1 :prob) (n1 :posint) (r2 :prob) (n2 :posint))
  (let* ((z1 (fisher-z-transform r1))
         (z2 (fisher-z-transform r2))
         (lambda (/ (- z1 z2) (sqrt (+ (/ (- n1 3)) (/ (- n2 3)))))))
    (ecase tails
      (:both (* 2 (if (<= lambda 0) (phi lambda) (- 1 (phi lambda)))))
      (:positive (- 1 (phi lambda)))
      (:negative (phi lambda)))))
         

(defun correlation-test-two-sample-on-sequences (points1 points2 &key (tails :both))
  (test-variables (points1 sequence) (points2 sequence))
  (let ((r1 (correlation-coefficient points1))
        (n1 (length points1))
        (r2 (correlation-coefficient points2))
        (n2 (length points2)))
    (correlation-test-two-sample r1 n1 r2 n2 :tails tails)))

;; SPEARMAN-RANK-CORRELATION
;; Rosner 498

;; Spearman rank correlation computes the relationship between a pair of
;; variables when one or both are either ordinal or have a distribution that
;; is far from normal.   It takes a list of points (same format as
;; linear-regression) and returns the spearman rank correlation coefficient
;; and its significance. 

(defun spearman-rank-correlation (points)
  (test-variables (points sequence))
  (let ((xis (mapcar #'first points))
        (yis (mapcar #'second points)))
    (test-variables (xis :numseq) (yis :numseq))
    (let* ((n (length points))
           (sorted-xis (sort (copy-seq xis) #'<))
           (sorted-yis (sort (copy-seq yis) #'<))
           (average-x-ranks (mapcar (lambda (x) (average-rank x sorted-xis)) xis))
           (average-y-ranks (mapcar (lambda (y) (average-rank y sorted-yis)) yis))
           (mean-x-rank (mean average-x-ranks))
           (mean-y-rank (mean average-y-ranks))
           (Lxx (reduce #'+ (mapcar (lambda (xi-rank) (square (- xi-rank mean-x-rank)))
                                    average-x-ranks)))
           (Lyy (reduce #'+ (mapcar (lambda (yi-rank) (square (- yi-rank mean-y-rank)))
                                    average-y-ranks)))
           (Lxy (reduce #'+ (mapcar (lambda (xi-rank yi-rank)
                                      (* (- xi-rank mean-x-rank)
                                         (- yi-rank mean-y-rank)))
                                    average-x-ranks average-y-ranks)))
           (rs (/ Lxy (sqrt (* Lxx Lyy))))
           (ts (/ (* rs (sqrt (- n 2))) (sqrt (- 1 (square rs)))))
           (p (t-significance ts (- n 2) :tails :both)))
      (format t "~%Spearman correlation coefficient ~f, p = ~f" rs p)
      (values rs p))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Significance functions
;;;

;; T-SIGNIFICANCE
;;  Lookup table in Rosner; this is adopted from CLASP/Numeric Recipes

(defun t-significance (t-statistic dof &key (tails :both))
  "Adopted from CLASP 1.4.3, <a href=\"http://eksl-www.cs.umass.edu/clasp.html\">http://eksl-www.cs.umass.edu/clasp.html</a>"
  (test-variables (t-statistic number) (dof :posint))
  (setf dof (float dof t-statistic))
  (let ((a (beta-incomplete (* 0.5 dof) 0.5 (/ dof (+ dof (square t-statistic))))))
    ;; A is 2*Integral from (abs t-statistic) to Infinity of t-distribution
    (ecase tails
      (:both a)
      (:positive (if (plusp t-statistic)
		          (* .5 a)
		        (- 1.0 (* .5 a))))
      (:negative (if (plusp t-statistic)
		          (- 1.0 (* .5 a))
		        (* .5 a))))))

;; F-SIGNIFICANCE
;; From CLASP

(defun f-significance
       (f-statistic numerator-dof denominator-dof &optional one-tailed-p)
  "Adopted from CLASP, but changed to handle F < 1 correctly in the
one-tailed case.  The `f-statistic' must be a positive number.  The degrees
of freedom arguments must be positive integers.  The `one-tailed-p' argument
is treated as a boolean.

This implementation follows Numerical Recipes in C, section 6.3 and the `ftest'
function in section 13.4."
  (setq f-statistic (float f-statistic))
  (test-variables (f-statistic :posnum) (numerator-dof :posint) (denominator-dof :posint))
  (let ((tail-area (beta-incomplete
		         (* 0.5d0 denominator-dof)
			      (* 0.5d0 numerator-dof)
			           (float (/ denominator-dof
					            (+ denominator-dof
						         (* numerator-dof f-statistic))) 1d0))))
    (if one-tailed-p
        (if (< f-statistic 1) (- 1 tail-area) tail-area)
      (progn (setf tail-area (* 2.0 tail-area))
	            (if (> tail-area 1.0)
			   (- 2.0 tail-area)
		         tail-area)))))

;; CHI-SQUARE and NORMAL (Gaussian) significance are calculated from their
;; respective CDF functions.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Utilities of potential external use
;;;

;; RANDOM-SAMPLE
;; Return a random sample of size N from sequence, without replacement.  If
;; N is equal to or greater than the length of the sequence, return the
;; entire sequence. 

(defun random-sample (n sequence)
  (test-variables (n integer) (sequence sequence))
  (cond ((null sequence) nil)
        ((<= n 0) nil)
        ((< (length sequence) n) sequence)
        (t (let ((one (random-pick sequence)))
             (cons one (random-sample (1- n) (remove one sequence :count 1)))))))

;; RANDOM-PICK
;; Random selection from sequence

(defun random-pick (sequence)
  (test-variables (sequence sequence))
  (when sequence (elt sequence (random (length sequence)))))

;; RANDOM-NORMAL
;; returns a random number with mean and standard-distribution as specified.

(defun random-normal (&key (mean 0) (sd 1))
  (test-variables (mean number) (sd :posnum))
  (let ((random-standard (z (random 1d0))))
    (+ mean (* sd random-standard))))
                              

;; BIN-AND-COUNT

;; Make N equal width bins and count the number of elements of sequence that
;; belong in each.

(defun bin-and-count (sequence n)
  (let* ((min  (reduce #'min sequence))
         (increment (/ (- (reduce #'max sequence) min) n))
         (bins (make-array n :initial-element 0)))
    (dotimes (bin n bins)
      (setf (aref bins bin)
            (count-if #'(lambda (x) (and (>= x (+ min (* bin increment)))
                                         (< x (+ min (* (1+ bin) increment)))))
                      sequence)))))

;; FISHER-Z-TRANSFORM
;; Rosner 458
;; Transforms the correlation coefficient to an approximately normal
;; distribution. 


(defun fisher-z-transform (r)
  (test-variables (r :prob))
  (* 1/2 (log (/ (1+ r) (- 1 r)))))
  
;; PERMUTATIONS
;; How many ways to take n things taken k at a time, when order matters
;; Rosner 88

(defun permutations (n k)
  (test-variables (n :posint) (k :posint)
             ("K must be less than or equal to N" :test (<= k n)))
  (let ((p 1))
    (dotimes (i (1+ k) p)
      (setq p (* p (- n i))))))

;; COMBINATIONS
;; How may ways to take n things taken k at a time, when order doesn't matter
;; Rosner 90

(defun choose (n k) 
  (test-variables (n :posint) 
             ("K must be between 0 and N (inclusive)" :test (and (>= k 0) (<= k n))))
  (/ (factorial n) (* (factorial k) (factorial (- n k)))))

;; MEAN-SD-N
;; A combined calculation that is often useful.  Takes a sequence and
;; returns three values: mean, standard deviation and N.

(defun mean-sd-n (sequence)
  (test-variables (sequence :numseq))
  (values (mean sequence) (standard-deviation sequence) (length sequence)))


;; ROUND-FLOAT
;;
;; Rounds a floating point number to a specified number of digits precision. 


(defun round-float (x &key (precision 5))
  (test-variables (x number) (precision :posint))
  (/ (round x (expt 10 (- precision))) (expt 10 precision)))



;; FALSE-DISCOVERY-CORRECTION
;;
;; A multiple testing correction that is less conservative than Bonferroni.
;; Takes a list of p-values and a false discovery rate, and returns the
;; number of p-values that are likely to be good enough to reject the null
;; at that rate.  Returns a second value which is the p-value cutoff. See
;;
;;   Benjamini Y and Hochberg Y. "Controlling the false discovery rate: a
;;   practical and powerful approach to multiple testing." J R Stat Soc Ser
;;   B 57: 289 300, 1995.


(defun false-discovery-correction (p-values &key (rate 0.05))
  (let ((number-of-tests (length p-values))
        (sorted-p-values (sort p-values #'>)))
    (do ((p-value (pop sorted-p-values) (pop sorted-p-values))
         (tests-to-go number-of-tests (1- tests-to-go)))
        ((or (null p-value)
             (<= p-value (* rate (/ tests-to-go number-of-tests))))
         (values tests-to-go (* rate (/ tests-to-go number-of-tests)))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Internal functions
;;;



(defun round-up (x)
  (multiple-value-bind (rounded ignore) (ceiling x)
    (declare (ignore ignore))
    rounded))

(defun sign (x)
  (cond ((minusp x) -1)
        ((plusp x) 1)
        ((zerop x) 0)
        (t nil)))

(defun factorial (number)
  (if (not (and (integerp number) (>= number 0)))
      (error "factorial: ~a is not a positive integer" number)
    (labels ((fact (num) (if (= 0 num) 1 (* num (fact (1- num))))))
       (fact number))))

;; Average rank calculation for non-parametric tests.  Ranks are 1 based,
;; but lisp is 0 based, so add 1!
         
(defun average-rank (value sorted-values)
  (let ((first (position value sorted-values))
        (last (position value sorted-values :from-end t)))
    (1+ (if (= first last)
        first
        (/ (+ first last) 2)))))

;;; CLASP utilities:

;;; Copyright (c) 1990 - 1994 University of Massachusetts
;;; Department of Computer Science
;;; Experimental Knowledge Systems Laboratory
;;; Professor Paul Cohen, Director.
;;; All rights reserved.

;;; Permission to use, copy, modify and distribute this software and its
;;; documentation is hereby granted without fee, provided that the above
;;; copyright notice of EKSL, this paragraph and the one following appear
;;; in all copies and in supporting documentation.
;;; EKSL makes no representation about the suitability of this software for any
;;; purposes.  It is provided "AS IS", without express or implied warranties
;;; including (but not limited to) all implied warranties of merchantability
;;; and fitness for a particular purpose, and notwithstanding any other
;;; provision contained herein.  In no event shall EKSL be liable for any
;;; special, indirect or consequential damages whatsoever resulting from loss
;;; of use, data or profits, whether in an action of contract, negligence or
;;; other tortuous action, arising out of or in connection with the use or
;;; performance of this software, even if EKSL is advised of the possiblity of
;;; such damages.

;;; For more information write to clasp-support@cs.umass.edu

(defun error-function (x)
  "Adopted from CLASP 1.4.3, <a href=\"http://eksl-www.cs.umass.edu/clasp.html\">http://eksl-www.cs.umass.edu/clasp.html</a>"
  (let ((erf (gamma-incomplete .5 (square x))))
    (if (>= x 0.0) erf (- erf))))

(defun gamma-incomplete (a x)
  "Adopted from CLASP 1.4.3, <a href=\"http://eksl-www.cs.umass.edu/clasp.html\">http://eksl-www.cs.umass.edu/clasp.html</a>"
  (declare (optimize (safety 3)))
  (setq a (coerce a 'double-float))
  (let ((gln (the double-float (gamma-ln a))))
    (when (= x 0.0)
      (return-from gamma-incomplete (values 0.0d0 gln)))
    (if (< x (+ a 1.0d0))
        ;; Use series representation.  The following is the code of what
        ;; Numerical Recipes in C calls ``GSER'
        (let* ((itmax 1000)
               (eps   3.0d-7)
               (ap    a)
               (sum   (/ 1d0 a))
               (del sum))
          (declare (type double-float ap sum del))
          (dotimes (i itmax)
            (incf ap 1.0d0)
            (setf del (* del (/ x ap)))
            (incf sum del)
            (if (< (abs del) (* eps (abs sum)))
                (let ((result (underflow-goes-to-zero
                               (* sum (safe-exp (- (* a (log x)) x gln))))))
                  (return-from gamma-incomplete (values result gln)))))
          (error "Series didn't converge:~%~
                  Either a=~s is too large, or ITMAX=~d is too small." a itmax))
        ;; Use the continued fraction representation.  The following is the
        ;; code of what Numerical Recipes in C calls ``GCF.'' Their code
        ;; computes the complement of the desired result, so we subtract from
        ;; 1.0 at the end.
        (let ((itmax 1000)
              (eps   3.0e-7)
              (gold 0d0) (g 0d0) (fac 1d0) (b1 1d0) (b0 0d0)
              (anf 0d0) (ana 0d0) (an 0d0) (a1 x) (a0 1d0))
          (declare (type double-float gold g fac b1 b0 anf ana an a1 a0))
          (dotimes (i itmax)
            (setf an  (float (1+ i))
                  ana (- an a)
                  a0  (* fac (+ a1 (* a0 ana)))
                  b0  (* fac (+ b1 (* b0 ana)))
                  anf (* fac an)
                  a1  (+ (* x a0) (* anf a1))
                  b1  (+ (* x b0) (* anf b1)))
            (unless (zerop a1)
              (setf fac (/ 1.0d0 a1)
                    g   (* b1 fac))
              (if (< (abs (/ (- g gold) g)) eps)
                  (let ((result (underflow-goes-to-zero
                                 (* (safe-exp (- (* a (log x)) x gln)) g))))
                    (return-from
                     gamma-incomplete (values (- 1.0 result) gln)))
                  (setf gold g))))
          (error "Continued Fraction didn't converge:~%~
                  Either a=~s is too large, or ITMAX=~d is too small." a
                  itmax)))))

(defun gamma-ln (x)
  "Adopted from CLASP 1.4.3, <a href=\"http://eksl-www.cs.umass.edu/clasp.html\">http://eksl-www.cs.umass.edu/clasp.html</a>"
  (cond ((<= x 0) (error "arg to gamma-ln must be positive:  ~s" x))
	((> x 1.0d302)
	  (error "Argument too large:  ~e" x))
	((= x 0.5d0)
	  ;; special case for this arg, since it is used by the error-function
	  (log (sqrt pi)))
	((< x 1)
	  ;; Use reflection formula:  Gamma(1-z) = z*pi/(Gamma(1+z)sin(pi*z))
	  (let ((z (- 1.0d0 x)))
	       (- (+ (log z) (log pi)) (+ (gamma-ln (+ 1.0 z)) (log (sin (* pi z)))))))
	(t (let* ((xx  (- x 1.0d0))
		    (tmp (+ xx 5.5d0))
		      (ser 1.0d0))
	          (declare (type double-float xx tmp ser))
		       (decf tmp (* (+ xx 0.5d0) (log tmp)))
		            (dolist (coef '(76.18009173d0 -86.50532033d0 24.01409822d0
							     -1.231739516d0 0.120858003d-2 -0.536382d-5))
			             (declare (type double-float coef))
				            (incf xx 1.0d0)
					           (incf ser (/ coef xx)))
			         (- (log (* 2.50662827465d0 ser)) tmp)))))

(defun error-function-complement (x)
  "Adopted from CLASP 1.4.3, <a href=\"http://eksl-www.cs.umass.edu/clasp.html\">http://eksl-www.cs.umass.edu/clasp.html</a>"
  (let* ((z   (abs x))
	  (y   (/ 1d0 (+ 1d0 (* 0.5 z))))   ; instead of t
	   (ans
	          (* y (exp (+ (* (- z) z)
			          -1.26551223
				     (* y
					      (+ 1.00002368
						  (* y
						         (+ 0.37409196
							           (* y
								        (+ 0.09678418
									        (* y
										   (+ -0.18628806
										         (* y
											          (+ 0.27886807
												      (* y
													     (+ -1.13520398
														       (* y
															    (+ 1.48851587
															            (* y
																       (+ -0.82215223
																	     (* y 0.17087277))))))))))))))))))))))
    (declare (type double-float z y ans))
    (if (>= x 0.0)
	ans
      (- 2.0 ans))))

(defun find-critical-value
       (p-function p-value &optional (x-tolerance .00001) (y-tolerance .00001))
  "Adopted from CLASP 1.4.3, <a href=\"http://eksl-www.cs.umass.edu/clasp.html\">http://eksl-www.cs.umass.edu/clasp.html</a>"
  (let* ((x-low 0d0)
	  (fx-low 1d0)
	   (x-high 1d0)
	    (fx-high (coerce (funcall p-function x-high) 'double-float)))
    ;; double up
    (declare (type double-float x-low fx-low x-high fx-high))
    (do () (nil)
      ;; for general functions, we'd have to try the other way of bracketing,
      ;; and probably have another way to terminate if, say, y is not in the
      ;; range of f.
      (when (>= fx-low p-value fx-high)
	(return))
      (setf x-low x-high
	        fx-low fx-high
		    x-high (* 2.0 x-high)
		        fx-high (funcall p-function x-high)))
    ;; binary search
    (do () (nil)
      (let* ((x-mid  (/ (+ x-low x-high) 2.0))
	          (fx-mid (funcall p-function x-mid))
		       (y-diff (abs (- fx-mid p-value)))
		            (x-diff (- x-high x-low)))
	(when (or (< x-diff x-tolerance)
		    (< y-diff y-tolerance))
	    (return-from find-critical-value x-mid))
	;; Because significance is monotonically decreasing with x, if the
	;; function is above the desired p-value...
	(if (< p-value fx-mid)
	        ;; then the critical x is in the upper half
	        (setf x-low x-mid
		        fx-low fx-mid)
	      ;; otherwise, it's in the lower half
	      (setf x-high x-mid
		      fx-high fx-mid))))))

(defun beta-incomplete (a b x)
  "Adopted from CLASP 1.4.3, <a href=\"http://eksl-www.cs.umass.edu/clasp.html\">http://eksl-www.cs.umass.edu/clasp.html</a>"
   (flet ((betacf (a b x)
		      ;; straight from Numerical Recipes in C, section 6.3
		      (declare (type double-float a b x))
		          (let ((ITMAX 1000)
				  (EPS   3.0d-7)
				    (qap 0d0) (qam 0d0) (qab 0d0) (em  0d0) (tem 0d0) (d 0d0)
				      (bz  0d0) (bm  1d0) (bp  0d0) (bpp 0d0)
				        (az  1d0) (am  1d0) (ap  0d0) (app 0d0) (aold 0d0))
			          (declare (type double-float qap qam qab em tem d
						      bz bm bp bpp az am ap app aold))
				        (setf qab (+ a b)
					          qap (+ a 1d0)
						      qam (- a 1d0)
						          bz  (- 1d0 (/ (* qab x) qap)))
					      (dotimes (m ITMAX)
						(setf em   (float (1+ m))
						            tem  (+ em em)
							          d    (/ (* em (- b em) x)
									        (* (+ qam tem) (+ a tem)))
								        ap   (+ az (* d am))
									      bp   (+ bz (* d bm))
									            d    (/ (* (- (+ a em)) (+ qab em) x)
											          (* (+ qap tem) (+ a tem)))
										          app  (+ ap (* d az))
											        bpp  (+ bp (* d bz))
												      aold az
												            am   (/ ap bpp)
													          bm   (/ bp bpp)
														        az   (/ app bpp)
															      bz   1d0)
						(if (< (abs (- az aold)) (* EPS (abs az)))
						        (return-from betacf az)))
					            (error "a=~s or b=~s too big, or ITMAX too small in BETACF"
							        a b))))
      (declare (notinline betacf))
      (setq a (coerce a 'double-float) b (coerce b 'double-float)
            x (coerce x 'double-float))
      (when (or (< x 0d0) (> x 1d0))
	 (error "x must be between 0d0 and 1d0:  ~f" x))
      ;; bt is the factors in front of the continued fraction
      (let ((bt (if (or (= x 0d0) (= x 1d0))    
		        0d0
		      (exp (+ (gamma-ln (+ a b))
			          (- (gamma-ln a))
				      (- (gamma-ln b))
				          (* a (log x))
					      (* b (log (- 1d0 x))))))))
	 (if (< x (/ (+ a 1d0) (+ a b 2.0)))
	          ;; use continued fraction directly
	          (/ (* bt (betacf a b x)) a) 
	        ;; use continued fraction after making the symmetry transformation
	        (- 1d0 (/ (* bt (betacf b a (- 1d0 x))) b))))))


(defun safe-exp (x)
  "Eliminates floating point underflow for the exponential function.
Instead, it just returns 0.0d0"
  (setf x (coerce x 'double-float))
  (if (< x (log least-positive-double-float))
      0.0d0
      (exp x)))

;;; ---------------------- Added hereafter by Jeff Shrager ----------------------

;;; Jeff Shrager's General Lisp Numerical Utilities

(defmacro display (&rest l)
  `(progn ,@(loop for i in l
		  collect `(format t "~a = ~a~%" ',i ,i)))) 


;;; --- Various math and stats routines.

(defun pround (n v)
  "Returns a string that is rounded to the appropriate number of 
   digits, but the only thing you can do with it is print it.  It's
   just a convenience hack for rounding recursive lists."
  (if (listp v)
      (mapcar #'(lambda (v) (pround n v)) v)
    (if (numberp v)
	(format nil (format nil "~~,~af" n) v)
      v)))

;;; And some conveniences:

(defun p2 (v) (pround 2 v))

(defun t1-test (values target &optional (warn? t))
  "One way t-test to see if a group differs from a numerical mean
   target value.  From Misanin & Hinderliter p. 248."
  (let ((t1-value (t1-value values target)))
    (values t1-value
	    (t-p-value (abs t1-value) (1- (length values)) warn?))))

(defun t1-value (values target)
  (/ (- (mean values) target)
     (/ (standard-deviation values)
	(sqrt (length values)))))

(defun n-random (n l &aux r)
  "Select n random sublists from a list, without replacement.  This
   copies the list and then destroys the copy.  N better be less than
   or equal to (length l)."
  (setq l (copy-list l))
  (dotimes (k n)
    (if (null (cdr l)) ; this has to be the last one
        (progn (push (car l) r)
	       (return nil))
      ;; If we're taking the first, it's easy, otherwise 
      ;; the first item get shoved into the middle.
      (let ((take (random (length l))))
        (if (zerop take)
	    (push (car l) r)
	  (let ((nc (nthcdr take l)))
	    (push (car nc) r)
	    (rplaca nc (car l))
	    )
	  )
	(pop l)
	) ; let
      ) ; if 
    ) ; dotimes
  r)

(defun t2-test (l1 l2)
  "T2-test calculates an UNPAIRED t-test.
   From Misanin & Hinderliter p. 268.   The t-cdf part is inherent in 
   xlispstat, and I'm not entirely sure that it's really the right
   computation since it doens't agree entirely with Table 5 of M&H, but
   it's close, so I assume that M&H have round-off error."
  (if (or (equal l1 l2) ; protect against 0 result from sdiff!
	  (null (cdr l1)) ; and other stupidity!
	  (null (cdr l2)))
    (values "n/a" "n/a")
    (let ((tval (t2-value l1 l2)))
      (values tval (t-p-value (abs tval) (- (+ (length l1) (length l2)) 2))))))

(defun t2-value (l1 l2)
  (let* ((n1 (float (length l1)))
	 (n2 (float (length l2)))
	 (s21 (s2 l1 n1))
	 (s22 (s2 l2 n2))
	 (m1 (mean l1))
	 (m2 (mean l2))
 	 (s2p (/ (+ (* s21 (1- n1))
		    (* s22 (1- n2)))
		 (+ (1- n1) (1- n2))))
	 )
    (/ (- m1 m2)
       (sqrt (+ (/ s2p n1) (/ s2p n2))))
    ))

(defun s2 (l n)
  (/ (- (sum (mapcar #'(lambda (a) (* a a)) l))
	(/ (let ((r (sum l))) (* r r)) n))
     (1- n)))

;;; For non-xlispstat, we compute t-cdf the hard way.  Sometime I
;;; ought to put in a real computation, but for the moment, it's a
;;; total cheat, just returning the symbol '>.05 or '<.05.  Each
;;; Entry is a df followed by the critical point at that df.

(defvar *t-cdf-critical-points-table-for-.05*
  '((1 . 6.31)
    (2 . 2.92)
    (3 . 2.35)
    (4 . 2.13)
    (5 . 2.02)
    (6 . 1.94)
    (7 . 1.89)
    (8 . 1.86)
    (9 . 1.83)
   (10 . 1.81)
   (15 . 1.75)
   (20 . 1.72)
   (25 . 1.71)
   (30 . 1.70)
   (40 . 1.68)
   (60 . 1.67)
  (120 . 1.66)))

(defun t-p-value (x df &optional (warn? t))
  (if (> df 120)
      (progn 
	(if warn? 
	    (format t "Warning df (~a) is off the scale.  Using df=120~%" df))
	(setq df 120)))
  (dolist (tcp *t-cdf-critical-points-table-for-.05*)
    (if (or (= df (car tcp))
	    (< df (car tcp)))
	(return (if (> x (cdr tcp)) '*/p<.05! nil))))
  )

;;; --- This emulates some of the xlispstat functions, and other stats
;;; utilities. 

;;; Some math ops that take many args.

(defun sum (l &aux (sum 0))
  (dolist (n l) (incf sum n)) sum)

(defun sqr (a)
  (if (numberp a) (expt a 2)
      (mapcar #'* a a)))

;;; This version of min and max operate on a list if there's only one arg.

(defun max* (l &rest ll &aux m)
  (if ll (setq l (cons l ll)))
  (setq m (pop l))
  (dolist (i l)
    (if (> i m) (setq m i)))
  m)

(defun min* (l &rest ll &aux m)
  (if ll (setq l (cons l ll)))
  (setq m (pop l))
  (dolist (i l)
    (if (< i m) (setq m i)))
  m)

;;; Warning, you can't use apply on most of the math operations
;;; because the arglist limits a too small for any large list.


;;; LH's version is better?
#+superseded
(defun mean (l)
  (/ (sum l) (float (length l))))

(defun protected-mean (l)
  "Computes a mean protected where there will be a divide by zero, and gives us n/a in that case."
  (loop with sum = 0
	with n = 0
	as value in l
	when (numberp value)
	do (incf sum value) (incf n)
	finally (return (cond ((not (zerop n)) (/ sum (float n)))
			      (t 'n/a)))))


#+superseded ; LH's version is better?
(defun standard-deviation (l)
  "This is specially protected for zero divisors."
  (if (null (cdr l))
      0.0
    (sqrt
     (let ((m (mean l)))
       (* (/ 1.0 (1- (length l)))
	  (sum (mapcar #'(lambda (x) (expt (- x m) 2)) l))
	  )))))

#+superseded ; LH's version is better?
(defun standard-error (l)
  (/ (standard-deviation l) (sqrt (length l))))

(defun lmean (ll)
  "Lmean takes the mean of entries in a list of lists vertically.  So:
   (lmean '((1 2) (5 6))) -> (3 4)  The args have to be the same length."
  (let* ((n (length ll))
	 (sums (copy-list (pop ll))) ; copy so that incf doesn't wreck us!
	 (len (length sums)))
    (dolist (l ll)
      (dotimes (k len)
	(incf (nth k sums) (nth k l))
	))
    (setq n (float n))
    (mapcar #'(lambda (v) (/ v n)) sums)
    )
  )

(defun anova2 (a1b1 a1b2 a2b1 a2b2)
  "Two-Way Anova.  (From Misanin & Hinderliter, 1991, p. 367-) This
   is specialized for four groups of equal n, called by their plot
   location names: left1 left2 right1 right2."
  (let* ((n (length a1b1)) ; All are the same, so any will do here.
         (npq (* 4 n)) ; This is specialized for a 2x2 design; always 4 levels.
	 (a1 (append a1b1 a1b2))
	 (suma1 (sum a1))
	 (a2 (append a2b1 a2b2))
	 (suma2 (sum a2))
	 (b1 (append a1b1 a2b1))
	 (sumb1 (sum b1))
	 (b2 (append a1b2 a2b2))
	 (sumb2 (sum b2))
	 (allscores (append a1 a2))
	 (sym1 (float (/ (sqr (sum allscores)) npq)))
	 (sym2 (float (sum (sqr allscores))))
	 (np (* n 2))
	 (sym3 (float (/ (+ (sqr suma1) (sqr suma2)) np)))
	 ;(nq np) ; Apparently never used???
	 (sym4 (float (/ (+ (sqr sumb1) (sqr sumb2)) np)))
	 (sym5 (float (/ (+ (sqr (sum a1b1)) (sqr (sum a2b1)) 
			    (sqr (sum a1b2)) (sqr (sum a2b2)))
			 n)
		      ))
	 (ssbg (- sym5 sym1))
	 (ssa (- sym3 sym1))
	 (ssb (- sym4 sym1))
	 (ssab (+ sym1 (- (- sym5 sym4) sym3)))
	 (sswg (- sym2 sym5))
	 (sstot (- sym2 sym1))
	 (df1 3)
	 (df2 1)
	 (df3 1)
	 (df4 1)
	 (df5 (* 4 (1- n)))
	 (dftot (1- npq))
	 (msbg (float (/ ssbg df1)))
	 (msa (float (/ ssa df2)))
	 (msb (float (/ ssb df3)))
	 (msab (float (/ ssab df4)))
	 (mswg (float (/ sswg df5)))
	 )
    (list :ssbg ssbg :dfbg df1 :msbg msbg :fbg (/ msbg mswg)
	  :ssa ssa :dfa df2 :msa msa :fa (/ msa mswg)
	  :ssb ssb :dfb df3 :msb msb :fb (/ msb mswg)
	  :ssab ssab :dfab df4 :msab msab :fab (/ msab mswg)
	  :sswg sswg :dfwg df5 :mswg mswg
	  :sstot sstot :dftot dftot
	  :a1b1 a1b1 :a1b2 a1b2 :a2b1 a2b1 :a2b2 a2b2)
    ))

(defun all-squares (as bs &aux squares)
  (dolist (a as)
    (dolist (b bs)
      (push (sqr (* a b)) squares)
      )))

(defun testanova2 ()
  (let ((a1b1 '(1 2 3 4 5))
	(a1b2 '(3 4 5 6 7))
	(a2b1 '(5 6 7 8 9))
	(a2b2 '(4 5 6 7 8))
	)
    (anova2 a1b1 a1b2 a2b1 a2b2)))

#| From VassarStats:

(In the above, A are rows, B are cols)

--------------------------------------------------------------------------------
ANOVA SUMMARY
Source   SS     df       MS     F        P 
bg       43.75   3   
rows     31.25   1    31.25  12.5 0.002749 
columns   1.25   1     1.25   0.5 0.489672 
r x c    11.25   1    11.25   4.5 0.049865 
wg       40     16     2.5  
Total    83.75  19   

(bg = between groups; wg = within groups (error))


Results are summarised in ANOVA summary table:

Source Sum of Squares Df Mean Squares  F-ratio p 
Age 11.25 1 11.25 37.5 .000 
Sex 101.25 1 101.25 337.5 .000 
Age * sex 1.25 1 1.25 4.167 .058 
Error 4.8 16 .3     
Total 118.55 20       

1. SS for factors and total calculated by formula
2. SS (error) = SS (total) – SS (factors): 118.55 – 11.25 – 101.25 – 1.25 = 4.8
Df calculated from N and number of conditions 
MS calculated: SS / DF (for each factor) 
Age: 11.25 / 1 = 11.25
Sex: 101.25 / 1 = 101.25
Interaction Age * Sex (not notation): 1.25 /1 = 1.25
Error: 4.8 / 16 = 0.3
F ratio calculated: MS (factor) / MS (error) 
Age: 11.25 / 0.3 = 37.5
Sex: 101.25 / 0.3 = 337.5
Interaction Age * Sex: 1.25 / 0.3 = 4.167

How to report:

Main effect of Age: F(1,16) = 37.5, p < 0.0001
Main effect of Sex: F(1,16) = 337.5, p < 0.0001
Interaction between Age and Sex (or: Age x Sex interaction): F(1,16) = 4.167, p = 0.058



|#

(defun anova2r (g1 g2)
  "Two way ANOVA with repeated measures on one dimension.  From
  Ferguson & Takane, 1989, p. 359.  Data is organized differently
  for this test.  Each group (g1 g2) contains list of all subjects'
  repeated measures, and same for B.  So, A: ((t1s1g1 t2s1g1 ...)
  (t1s2g2 t2s2g2 ...) ...)  Have to have the same number of test
  repeats for each subject, and this assumes the same number of
  subject in each group."
  (let* ((c (length (car g1)))
	 (r 2) ; only two groups in this special case
	 (tsr1 (mapcar #'sum g1))
	 (t1 (sum tsr1))
	 (t1c (let (temp)
		(dotimes (n c temp)
		 (push (sum (mapcar #'(lambda (s) (nth n s)) g1)) temp))))
	 (tsr2 (mapcar #'sum g2))
	 (t2 (sum tsr2))
	 (t2c (let (temp)
		(dotimes (n c temp)
		 (push (sum (mapcar #'(lambda (s) (nth n s)) g2)) temp))))
	 (tc (mapcar #'+ t1c t2c))
	 (total (+ t1 t2))
	 (n (length g1))
	 (q1 (* (/ 1.0 c) (sum (append (sqr tsr1) (sqr tsr2)))))
	 (q2 (* (/ 1.0 (* c n)) (+ (sqr t1) (sqr t2))))
	 (q3 (* (/ 1.0 (* 2 n)) (sum (sqr tc))))
	 (q4 (* (/ 1.0 n) (sum (append (sqr t1c) (sqr t2c)))))
	 (q5 (sum (append (mapcar #'sum (mapcar #'sqr g1))
			  (mapcar #'sum (mapcar #'sqr g2)))))
	 (q6 (/ (sqr total) (* n 2.0 c)))
	 (ssbs (- q1 q6))
	 (ssr (- q2 q6))
	 (sssr (- q1 q2))
	 (ssws (- q5 q1))
	 (ssc (- q3 q6))
	 (ssrc (+ q6 (- (- q4 q2) q3)))
	 (sscsr (+ q2 (- (- q5 q1) q4)))
	 (sstotal (- q5 q6))
	 (dfr (1- r))
	 (dfc (1- c))
	 (dfrc (* (1- r) (1- c)))
	 (dfsr (* r (1- n)))
	 (dfcsr (* r (1- n) (1- c)))
	 (msr (/ ssr dfr))
	 (mssr (/ sssr dfsr))
	 (msc (/ ssc dfc))
	 (msrc (/ ssrc dfrc))
	 (mscsr (/ sscsr dfcsr))
	 (dftotal (+ dfr dfc dfrc dfsr dfcsr))
	 (fr (/ msr mssr))
	 (fc (/ msc mscsr))
	 (frc (/ ssrc mscsr))
	 )
    (if (not (= (length g1) (length g2)))
	(format t "Warning, ANOVA2R design has unbalanced cells!~%"))
    (list :ssbs ssbs :ssr ssr :sssr sssr :ssws ssws :ssc ssc 
	  :ssrc ssrc :sscsr sscsr :sstotal sstotal
	  :dfr dfr :dfsr dfsr :dfc dfc :dfrc dfrc :dfcsr dfcsr :dftotal dftotal
	  :msr msr :mssr mssr :msc msc :msrc msrc :mscsr mscsr
	  :fr fr :fc fc :frc frc)
    ) ; close let*
  )

#+example-data
(setq an2rd1 '( (2 7 6 7 9)
      (4 3 7 12 14)
      (7 6 4 12 10)
      (1 3 3 6 6)))
#+example-data
(setq an2rd2 '( (4 4 7 9 1)
      (10 12 12 12 16)
      (8 7 8 12 10)
      (5 7 6 7 8)))

;;; Example from Neter p.677
#+example-data
(setq neter677data 
      '((11 17 16 14 15) (12 10 15 19 11) (23 20 18 17) (27 33 22 26 28)))

(defun anova1 (d) ; Note that dots are replaced by dashes, so: Y.. = Y-- here
  "One way simple ANOVA, from Neter, et al. p677+.
   Data is give as a list of lists, each one representing a treatment, and each containing
   the observations."
  (let* ((meanYi- (mapcar #'mean d))
	 (serrYi- (mapcar #'standard-error d))
	 (r (length d))
	 (Yi- (mapcar #'sum d))
	 (ni (mapcar #'length d))
	 (Y-- (sum Yi-))
	 (meanY-- (mean meanYi-))
	 (ntotal (sum ni))
	 (SSTO (sum (mapcar #'(lambda (treatment) 
				(sum (mapcar #'(lambda (Oij) 
						 (expt (- Oij meanY--) 2)) treatment))) d)))
	 (SSE (sum (mapcar #'(lambda (treatment meanYi-) 
			       (sum (mapcar #'(lambda (Oij) 
						(expt (- Oij meanYi-) 2)) treatment))) d meanYi-)))
	 (SSTR (sum (mapcar #'(lambda (meanYi- ni) (* ni (expt (- meanYi- meanY--) 2))) meanYi- ni)))
	 (SSTRdf (- r 1))
	 (SSEdf (- ntotal r))
	 (SSTOdf (- ntotal 1))
	 (MSTR (/ SSTR SSTRdf))
	 (MSE (/ SSE SSedf))
	 (F* (/ MSTR MSE))
	 )
	 (list :SUMMARY (format nil "F(.95,~a,~a) = ~a" SSTRdf SSEdf F*) 
	       :r r
	       :meanYi- meanYi-
	       :serrYi- serrYi-
	       :Yi- Yi- 
	       :ni ni
	       :Y-- Y--
	       :meanY-- meanY--
	       :ntotal ntotal
	       :SSTO SSTO
	       :SSE SSE
	       :SSTR SSTR 
	       :SSTRdf SSTRdf
	       :SSEdf SSEdf
	       :SSTOdf SSTOdf
	       :MSTR MSTR
	       :MSE MSE
	       :F* F*
	       )))

;;; Q-table for tukey HSD.  These are upside down because we 
;;; want to walk them backwards to find the stopping point.

(defvar *q-table*
  '(
    (5 ; K=5
;     DF    0.05   0.01
     (90     3.94   4.76)
     (60     3.98   4.82)
     (50     4.00   4.87)
     (40     4.04   4.93)
     (35     4.06   4.98)
     (30     4.10   5.05)
     (27     4.13   5.10)
     (24     4.17   5.17)
     (22     4.19   5.22)
     (20     4.23   5.29)
     (19     4.25   5.33)
     (18     4.28   5.38)
     (17     4.30   5.43)
     (16     4.33   5.49)
     (15     4.37   5.56)
     (14     4.41   5.63)
     (13     4.45   5.73)
     (12     4.51   5.84)
     (11     4.57   5.97)
     (10     4.65   6.14)
     (9     4.76   6.35)
     )
    (4 ; K=4
;     DF    0.05   0.01
     (90     3.70   4.54)
     (60     3.74   4.59)
     (50     3.76   4.63)
     (40     3.79   4.70)
     (35     3.81   4.74)
     (30     3.85   4.80)
     (27     3.87   4.85)
     (24     3.90   4.91)
     (22     3.92   4.96)
     (20     3.96   5.02)
     (19     3.98   5.05)
     (18     4.00   5.09)
     (17     4.02   5.14)
     (16     4.05   5.19)
     (15     4.08   5.25)
     (14     4.11   5.32)
     (13     4.15   5.40)
     (12     4.20   5.50)
     (11     4.26   5.62)
     (10     4.33   5.77)
     (9     4.41   5.96)
     )
    (3 ; K=3
;     DF    0.05   0.01
     (90     3.37   4.23)
     (60     3.40   4.28)
     (50     3.41   4.32)
     (40     3.44   4.37)
     (35     3.46   4.39)
     (30     3.49   4.45)
     (27     3.50   4.49)
     (24     3.53   4.55)
     (22     3.55   4.59)
     (20     3.58   4.64)
     (19     3.59   4.67)
     (18     3.61   4.70)
     (17     3.63   4.74)
     (16     3.65   4.79)
     (15     3.67   4.84)
     (14     3.70   4.89)
     (13     3.73   4.96)
     (12     3.77   5.05)
     (11     3.82   5.15)
     (10     3.88   5.27)
     (9     3.95   5.43)
     )
    ))

#| This example comes from: http://vassun.vassar.edu/~lowry/ch14pt1.html

(setq data 
      '(
	(27.0 26.2 28.8 33.5 28.8)
	(22.8 23.1 27.7 27.6 24.0)
	(21.9 23.4 20.1 27.8 19.3)
	(23.5 19.6 23.7 20.8 23.9)))
|#

(defun Anova1+TukeyHSD (data)
  "Tukey's HSD post hoc, using info from http://vassun.vassar.edu/~lowry/ch14pt2.html
   Calculates q values for all pairwise comparisons of the means in the one-way ANOVA
   on the given data.  The results are returned in a :HSD-QS field, added to the ANOVA1
   result, which looks like: ((q sig0.05 sig0.01 (datapos1 mean1) (datapos2 mean2))...) 
   where the datapos are the nth position in the data, and q is the Q value.  
   The 0.05 and 0.01 sigs are '- or '+, based upon the K, DFwg and Q values.
   (Note that the number of treatments is (length data), which is  called K in the 
    HSD calculation, and = the between df+1.)"
  (let* ((result (anova1 data))
	 (k (getf result :r))
	 (dfwg (getf result :SSEdf))
	 (denomenator (sqrt (/ (getf result :mse) (harmonic-mean (mapcar #'length data)))))
	 (qs (tukey-q k dfwg))
	 (q0.05 (first qs))
	 (q0.01 (second qs))
	 (hsd0.05 (* q0.05 denomenator))
	 (hsd0.01 (* q0.01 denomenator))
	 ;; Cross subsets will work the same way in all cases, enabling
	 ;; us to create the mean combinations, and their indexes:
	 (mean-combinations (bio::cross-subsets (getf result :MEANYI-)))
	 (index-combinations (bio::cross-subsets (loop for i below k collect i)))
	 )
    (setf (getf result :tukey-params)
	  (list :denomenator denomenator
		:q0.05 q0.05 
		:q0.01 q0.01
		:hsd0.05 hsd0.05
		:hsd0.01 hsd0.01
		:dfwg dfwg))
    (setf (getf result :tukey-hsds)
	  (loop for (m1 m2) in mean-combinations
		as (i1 i2) in index-combinations
		as mean-diff = (float (abs (- m1 m2)))
		as Q = (/ mean-diff denomenator)
		collect (list :q q
			      :sig0.05 (if (> mean-diff hsd0.05) '+ '-)
			      :sig0.01 (if (> mean-diff hsd0.01) '+ '-)
			      :i1 i1 :m1 (float m1)
			      :i2 i2 :m2 (float m2)
			      )))
    result))
    
;;; This finds the Q table for the appopriate K, and then walks
;;; BACKWARDS through it (in a kind of ugly way!) to find the
;;; appropriate place in the table for the DFwg, and then uses the
;;; level (which must be 0.01 or 0.05, indicating the first, or second
;;; col of the table) to determine if the Q value reaches
;;; significance, and gives us a + or - final result.

(defun tukey-q (k dfwg)
  (loop for (dflimit . qs) in (or (cdr (assoc k *q-table*))
				  ;; Use the highest if K isn't there.
				  (cdr (car *q-table*)))
	until (<= dflimit dfwg)
	finally (return qs)))

(defun harmonic-mean (seq)
  "See: http://mathworld.wolfram.com/HarmonicMean.html"
  (/ (float (length seq))
     (loop for n in seq
	   sum (/ 1.0 n))))

;;; ----------------------------------------------------------------------------

(defun regress (x y)
  "Simple linear regression."
  (let* ((n (float (length x)))
	 (sumx (sum x))
	 (sumy (sum y))
	 (sumxy (sum (mapcar #'* x y)))
	 (sumx2 (sum (mapcar #'* x x)))
	 (m (/ (- (* n sumxy) (* sumx sumy))
	       (- (* n sumx2) (expt sumx 2))))
	 (b (+ (/ (* (- m) sumx) n)
	       (/ sumy n)))
	 (sumy2 (sum (mapcar #'* y y)))
	 (resids (mapcar #'(lambda (x y) (- y (+ (* x m) b))) x y))
	 (r (/ (- (* n sumxy) (* sumx sumy))
	       (sqrt (* 
		      (- (* n sumx2) (expt (sum x) 2))
		      (- (* n sumy2) (expt (sum y) 2))
		      ))))
	 (r2 (expt r 2))
	 )
    (list :m m :b b :resids resids :r r :r2 r2)))

(defun correlate (x y)
  "Correlation of two sequences, as in Ferguson & Takane, 1989,
   p. 125.  Assumes NO MISSING VALUES!"
  (if (not (= (length x) (length y)))
      (break "Can only correlate equal-sized sets."))
  (let* ((mx (mean x))
         (my (mean y))
	 (n (length x))
         (devx (mapcar #'(lambda (v) (- v mx)) x))
         (devy (mapcar #'(lambda (v) (- v my)) y))
	 (sumdevxy (sum (mapcar #'* devx devy)))
	 (sumsqdevx (sum (sqr devx)))
	 (sumsqdevy (sum (sqr devy)))
	 (r (/ sumdevxy (sqrt (* sumsqdevx sumsqdevy))))
	 )
    (list :r r :r2 (sqr r) :n n :p (2-tailed-correlation-significance n (abs r)))
    ))

;;;  Critical Values of r

;;; One tailed is half of this:
(defvar *critical-values-of-r-two-tailed-column-interpretaion* '(0.2 0.1 0.05 0.02 0.01 0.001))

(defvar *critical-values-of-r* '(
; n ... 2-tailed testing / (1-tailed testing)
;   0.2 (0.1) 0.1 (0.05) 0.05 (0.025) 0.02 (0.01) 0.01 (0.005) 0.001 (0.0005)
(5 0.687 0.805 0.878 0.934 0.959 0.991)
(6 0.608 0.729 0.811 0.882 0.917 0.974)
(7 0.551 0.669 0.754 0.833 0.875 0.951)
(8 0.507 0.621 0.707 0.789 0.834 0.925)
(9 0.472 0.582 0.666 0.750 0.798 0.898)
(10 0.443 0.549 0.632 0.715 0.765 0.872)
(11 0.419 0.521 0.602 0.685 0.735 0.847)
(12 0.398 0.497 0.576 0.658 0.708 0.823)
(13 0.380 0.476 0.553 0.634 0.684 0.801)
(14 0.365 0.458 0.532 0.612 0.661 0.780)
(15 0.351 0.441 0.514 0.592 0.641 0.760)
(16 0.338 0.426 0.497 0.574 0.623 0.742)
(17 0.327 0.412 0.482 0.558 0.606 0.725)
(18 0.317 0.400 0.468 0.543 0.590 0.708)
(19 0.308 0.389 0.456 0.529 0.575 0.693)
(20 0.299 0.378 0.444 0.516 0.561 0.679)
(21 0.291 0.369 0.433 0.503 0.549 0.665)
(22 0.284 0.360 0.423 0.492 0.537 0.652)
(23 0.277 0.352 0.413 0.482 0.526 0.640)
(24 0.271 0.344 0.404 0.472 0.515 0.629)
(25 0.265 0.337 0.396 0.462 0.505 0.618)
(26 0.260 0.330 0.388 0.453 0.496 0.607)
(27 0.255 0.323 0.381 0.445 0.487 0.597)
(28 0.250 0.317 0.374 0.437 0.479 0.588)
(29 0.245 0.311 0.367 0.430 0.471 0.579)
(30 0.241 0.306 0.361 0.423 0.463 0.570)
(40 0.207 0.264 0.312 0.367 0.403 0.501)
(50 0.184 0.235 0.279 0.328 0.361 0.451)
(60 0.168 0.214 0.254 0.300 0.330 0.414)
(80 0.145 0.185 0.220 0.260 0.286 0.361)
(100 0.129 0.165 0.197 0.232 0.256 0.324)
(120 0.118 0.151 0.179 0.212 0.234 0.297)
(140 0.109 0.140 0.166 0.196 0.217 0.275)
(160 0.102 0.130 0.155 0.184 0.203 0.258)
(180 0.096 0.123 0.146 0.173 0.192 0.243)
(200 0.091 0.117 0.139 0.164 0.182 0.231)
(300 0.074 0.095 0.113 0.134 0.149 0.189)
(400 0.064 0.082 0.098 0.116 0.129 0.164)
(500 0.057 0.074 0.088 0.104 0.115 0.147)
))


(defun 2-tailed-correlation-significance (n r)
  ;; We use the first line for anything less than 5, and the last line for anything over 500
  ;; Otherwise, find the nearest value (maybe we should interpolate ... too much bother!)
  (let ((target-row (first *critical-values-of-r*)))
    (when (> n 5)
	  (loop for row in *critical-values-of-r*
		as (row-n . nil) = row
		with last-row-n = 5
		with last-row = target-row
		do
		(cond ((= row-n n)
		       (setq target-row row)
		       (return t))
		      ((> row-n n)
		       (cond ((< (abs (- n row-n))
				 (abs (- n last-row-n)))
			      (setq target-row row))
			     (t (setq target-row last-row)))
		       (return t)))
		(setq last-row row)
		(setq last-row-n row-n)
		finally (progn (setq target-row row)
			       (return t))))
    (pop target-row) ; removes the N header
    (cond ((< r (car target-row)) ">0.2")
	  (t (loop for crit in (cdr target-row)
		   as p in (cdr *critical-values-of-r-two-tailed-column-interpretaion*)
		   with last-p = (car *critical-values-of-r-two-tailed-column-interpretaion*)
		   when (< r crit)
		   do (return (format nil "<~a" last-p))
		   else do (setq last-p p)
		   finally (return (format nil "<~a" p))
		   )))
    ))



(defun even-power-of-two? (n)
  (zerop (mod (/ (log n) (log 2)) 1)))

(defun normalize (v)
  "Normalize a vector by dividing it through by subtracting its min
   and then dividing through by its range (max-min).  If the numbers
   are all the same, this would screw up, so we check that first and
   just return a long list of 0.5 if so!"
  (let* ((mx (reduce #'max v))
	 (mn (reduce #'min v))
	 (range (float (- mx mn)))
	 )
    (mapcar #'(lambda (i) (if (zerop range) 0.5
			      (/ (- i mn) range))) v)
    ))

(defun dumplot (v &optional show-values)
  "A dumb terminal way of plotting data."
  (let* ((d1 (normalize v))
	 (d2 (mapcar #'(lambda (i) (* 50.0 i)) d1))
	 (min (reduce #'min v))
	 (max (reduce #'max v))
	 )
    (format t "~a~50t~a~%" min max)
    (dolist (i d2)
      (dotimes (k (round i))
        (format t " ")
	)
      (if show-values
	  (format t "* = ~a~%" (p2 (car v)))
	  (format t "*~%")
	  )
      (pop v) ; follow along for showing values
      )
    ))

(defun cross-mean (l &aux k r)
  "Cross mean takes a list of lists, as ((1 2 3) (4 3 2 1) ...) and
   produces a list with mean and standard error for each VERTICLE
   entry, so, as: ((2.5 . 1) ...) where the first pair is computed
   from the nth 1 of all the sublists in the input set, etc.  This is
   useful in some cases of data cruching.  Note that missing data is
   assumed to be always at the END of lists.  If it isn't, you've
   got to do something previously to interpolate."
  (let* ((nmax (reduce #'max (mapcar #'length l)))
	 (vs (make-array nmax)))
    (dolist (i l)
      (setq k 0)
      (dolist (v i)
	(push v (aref vs k))
	(incf k)))
    (dotimes (i nmax)
      (push (cons (mean (aref vs i))
		  (standard-error (aref vs i)))
	    r))
    (reverse r)))

(defmacro z/protect (expr testvar)
  "Macro to protect from division by zero."
  `(if (zerop ,testvar) "[/0!]" ,expr))

(defun histovalues (v* &key (nbins 10))
  "Take a set of values and produce a histogram binned into n groups, so that 
   you can get a report of the distribution of values.  There's a large 
   chance for off-by-one errores here!"
  (let* ((min (min* v*))
	 (max (max* v*))
	 (inc (round (/ (abs (- min max)) nbins)))
	 (bins (loop with i = min
		     for n from 1 to nbins
		     collect (list i (incf i inc) 0)
		     ))
	 )
    (loop for v in v*
	  do (loop for bin in bins
		   if (and (>= v (first bin))
			   (< v (second bin)))
		   do (incf (third bin))))
    bins))
  

(defun x2test ()
  "Simple Chi-Squares From Clarke & Cooke p. 431; should = ~7.0"
  (chi-square-1 '(100 100 100 100 100 100 100 100 100 100)
		'(106  88  97 101  92 103  96 112 114  91)))

(defun chi-square-1 (expected observed)
  `(:x2 ,(loop for e in expected as o in observed
	       sum (/ (expt (- o e) 2) (float e)))
    :df ,(1- (length expected))))
  

;;; I'm not sure what the setup is supposed to be for this one, 
;;; since, like a moron I didn't give an example....

(defun chi-square-2 (table)
  (let* ((row-mars (mapcar #'(lambda (row) (apply #'+ row)) table))
	 (col-mars (loop for col from 0 to (1- (length (car table)))
			 collect (apply #'+ (loop for row in table
						  collect (nth col row)))))
	 (total (float (apply #'+ row-mars)))
	 (expectable (loop for row in table
			   as rowmar in row-mars
			   collect (loop for col in row
					 as colmar in col-mars
					 collect (cons col (/ (* rowmar colmar) total)))))
	 )
    (loop for row in expectable
	  sum (loop for entry in row
		    sum (/ (expt (- (car entry) (cdr entry)) 2) (cdr entry))))))

    
;;; This are the F score limit tables for anovas in the form: F(A,B)
;;; (From http://www.itl.nist.gov/div898/handbook/eda/section3/eda3673.htm)

;;; I *think* that A is the column df = number of columns - 1
;;;                B is the row df = number of samples - number of columns

(defun f-score>p-limit? (df1 df2 f-score limits-table)
  (let ((limit (nth df1 (assoc df2 limits-table))))
    (cond (limit (> f-score limit))
	  (t (format t "Warning! F-score>p-limit? can't find an entry for F(~a,~a)!~%" df1 df2)))))


(defvar *F0.05* '(
;   A:         1       2       3       4       5       6       7       8       9      10
;B:
(  1      161.448 199.500 215.707 224.583 230.162 233.986 236.768 238.882 240.543 241.882)
(  2       18.513  19.000  19.164  19.247  19.296  19.330  19.353  19.371  19.385  19.396)
(  3       10.128   9.552   9.277   9.117   9.013   8.941   8.887   8.845   8.812   8.786)
(  4        7.709   6.944   6.591   6.388   6.256   6.163   6.094   6.041   5.999   5.964)
(  5        6.608   5.786   5.409   5.192   5.050   4.950   4.876   4.818   4.772   4.735)
(  6        5.987   5.143   4.757   4.534   4.387   4.284   4.207   4.147   4.099   4.060)
(  7        5.591   4.737   4.347   4.120   3.972   3.866   3.787   3.726   3.677   3.637)
(  8        5.318   4.459   4.066   3.838   3.687   3.581   3.500   3.438   3.388   3.347)
(  9        5.117   4.256   3.863   3.633   3.482   3.374   3.293   3.230   3.179   3.137)
( 10        4.965   4.103   3.708   3.478   3.326   3.217   3.135   3.072   3.020   2.978)
( 11        4.844   3.982   3.587   3.357   3.204   3.095   3.012   2.948   2.896   2.854)
( 12        4.747   3.885   3.490   3.259   3.106   2.996   2.913   2.849   2.796   2.753)
( 13        4.667   3.806   3.411   3.179   3.025   2.915   2.832   2.767   2.714   2.671)
( 14        4.600   3.739   3.344   3.112   2.958   2.848   2.764   2.699   2.646   2.602)
( 15        4.543   3.682   3.287   3.056   2.901   2.790   2.707   2.641   2.588   2.544)
( 16        4.494   3.634   3.239   3.007   2.852   2.741   2.657   2.591   2.538   2.494)
( 17        4.451   3.592   3.197   2.965   2.810   2.699   2.614   2.548   2.494   2.450)
( 18        4.414   3.555   3.160   2.928   2.773   2.661   2.577   2.510   2.456   2.412)
( 19        4.381   3.522   3.127   2.895   2.740   2.628   2.544   2.477   2.423   2.378)
( 20        4.351   3.493   3.098   2.866   2.711   2.599   2.514   2.447   2.393   2.348)
( 21        4.325   3.467   3.072   2.840   2.685   2.573   2.488   2.420   2.366   2.321)
( 22        4.301   3.443   3.049   2.817   2.661   2.549   2.464   2.397   2.342   2.297)
( 23        4.279   3.422   3.028   2.796   2.640   2.528   2.442   2.375   2.320   2.275)
( 24        4.260   3.403   3.009   2.776   2.621   2.508   2.423   2.355   2.300   2.255)
( 25        4.242   3.385   2.991   2.759   2.603   2.490   2.405   2.337   2.282   2.236)
( 26        4.225   3.369   2.975   2.743   2.587   2.474   2.388   2.321   2.265   2.220)
( 27        4.210   3.354   2.960   2.728   2.572   2.459   2.373   2.305   2.250   2.204)
( 28        4.196   3.340   2.947   2.714   2.558   2.445   2.359   2.291   2.236   2.190)
( 29        4.183   3.328   2.934   2.701   2.545   2.432   2.346   2.278   2.223   2.177)
( 30        4.171   3.316   2.922   2.690   2.534   2.421   2.334   2.266   2.211   2.165)
( 31        4.160   3.305   2.911   2.679   2.523   2.409   2.323   2.255   2.199   2.153)
( 32        4.149   3.295   2.901   2.668   2.512   2.399   2.313   2.244   2.189   2.142)
( 33        4.139   3.285   2.892   2.659   2.503   2.389   2.303   2.235   2.179   2.133)
( 34        4.130   3.276   2.883   2.650   2.494   2.380   2.294   2.225   2.170   2.123)
( 35        4.121   3.267   2.874   2.641   2.485   2.372   2.285   2.217   2.161   2.114)
( 36        4.113   3.259   2.866   2.634   2.477   2.364   2.277   2.209   2.153   2.106)
( 37        4.105   3.252   2.859   2.626   2.470   2.356   2.270   2.201   2.145   2.098)
( 38        4.098   3.245   2.852   2.619   2.463   2.349   2.262   2.194   2.138   2.091)
( 39        4.091   3.238   2.845   2.612   2.456   2.342   2.255   2.187   2.131   2.084)
( 40        4.085   3.232   2.839   2.606   2.449   2.336   2.249   2.180   2.124   2.077)
( 41        4.079   3.226   2.833   2.600   2.443   2.330   2.243   2.174   2.118   2.071)
( 42        4.073   3.220   2.827   2.594   2.438   2.324   2.237   2.168   2.112   2.065)
( 43        4.067   3.214   2.822   2.589   2.432   2.318   2.232   2.163   2.106   2.059)
( 44        4.062   3.209   2.816   2.584   2.427   2.313   2.226   2.157   2.101   2.054)
( 45        4.057   3.204   2.812   2.579   2.422   2.308   2.221   2.152   2.096   2.049)
( 46        4.052   3.200   2.807   2.574   2.417   2.304   2.216   2.147   2.091   2.044)
( 47        4.047   3.195   2.802   2.570   2.413   2.299   2.212   2.143   2.086   2.039)
( 48        4.043   3.191   2.798   2.565   2.409   2.295   2.207   2.138   2.082   2.035)
( 49        4.038   3.187   2.794   2.561   2.404   2.290   2.203   2.134   2.077   2.030)
( 50        4.034   3.183   2.790   2.557   2.400   2.286   2.199   2.130   2.073   2.026)
( 51        4.030   3.179   2.786   2.553   2.397   2.283   2.195   2.126   2.069   2.022)
( 52        4.027   3.175   2.783   2.550   2.393   2.279   2.192   2.122   2.066   2.018)
( 53        4.023   3.172   2.779   2.546   2.389   2.275   2.188   2.119   2.062   2.015)
( 54        4.020   3.168   2.776   2.543   2.386   2.272   2.185   2.115   2.059   2.011)
( 55        4.016   3.165   2.773   2.540   2.383   2.269   2.181   2.112   2.055   2.008)
( 56        4.013   3.162   2.769   2.537   2.380   2.266   2.178   2.109   2.052   2.005)
( 57        4.010   3.159   2.766   2.534   2.377   2.263   2.175   2.106   2.049   2.001)
( 58        4.007   3.156   2.764   2.531   2.374   2.260   2.172   2.103   2.046   1.998)
( 59        4.004   3.153   2.761   2.528   2.371   2.257   2.169   2.100   2.043   1.995)
( 60        4.001   3.150   2.758   2.525   2.368   2.254   2.167   2.097   2.040   1.993)
( 61        3.998   3.148   2.755   2.523   2.366   2.251   2.164   2.094   2.037   1.990)
( 62        3.996   3.145   2.753   2.520   2.363   2.249   2.161   2.092   2.035   1.987)
( 63        3.993   3.143   2.751   2.518   2.361   2.246   2.159   2.089   2.032   1.985)
( 64        3.991   3.140   2.748   2.515   2.358   2.244   2.156   2.087   2.030   1.982)
( 65        3.989   3.138   2.746   2.513   2.356   2.242   2.154   2.084   2.027   1.980)
( 66        3.986   3.136   2.744   2.511   2.354   2.239   2.152   2.082   2.025   1.977)
( 67        3.984   3.134   2.742   2.509   2.352   2.237   2.150   2.080   2.023   1.975)
( 68        3.982   3.132   2.740   2.507   2.350   2.235   2.148   2.078   2.021   1.973)
( 69        3.980   3.130   2.737   2.505   2.348   2.233   2.145   2.076   2.019   1.971)
( 70        3.978   3.128   2.736   2.503   2.346   2.231   2.143   2.074   2.017   1.969)
( 71        3.976   3.126   2.734   2.501   2.344   2.229   2.142   2.072   2.015   1.967)
( 72        3.974   3.124   2.732   2.499   2.342   2.227   2.140   2.070   2.013   1.965)
( 73        3.972   3.122   2.730   2.497   2.340   2.226   2.138   2.068   2.011   1.963)
( 74        3.970   3.120   2.728   2.495   2.338   2.224   2.136   2.066   2.009   1.961)
( 75        3.968   3.119   2.727   2.494   2.337   2.222   2.134   2.064   2.007   1.959)
( 76        3.967   3.117   2.725   2.492   2.335   2.220   2.133   2.063   2.006   1.958)
( 77        3.965   3.115   2.723   2.490   2.333   2.219   2.131   2.061   2.004   1.956)
( 78        3.963   3.114   2.722   2.489   2.332   2.217   2.129   2.059   2.002   1.954)
( 79        3.962   3.112   2.720   2.487   2.330   2.216   2.128   2.058   2.001   1.953)
( 80        3.960   3.111   2.719   2.486   2.329   2.214   2.126   2.056   1.999   1.951)
( 81        3.959   3.109   2.717   2.484   2.327   2.213   2.125   2.055   1.998   1.950)
( 82        3.957   3.108   2.716   2.483   2.326   2.211   2.123   2.053   1.996   1.948)
( 83        3.956   3.107   2.715   2.482   2.324   2.210   2.122   2.052   1.995   1.947)
( 84        3.955   3.105   2.713   2.480   2.323   2.209   2.121   2.051   1.993   1.945)
( 85        3.953   3.104   2.712   2.479   2.322   2.207   2.119   2.049   1.992   1.944)
( 86        3.952   3.103   2.711   2.478   2.321   2.206   2.118   2.048   1.991   1.943)
( 87        3.951   3.101   2.709   2.476   2.319   2.205   2.117   2.047   1.989   1.941)
( 88        3.949   3.100   2.708   2.475   2.318   2.203   2.115   2.045   1.988   1.940)
( 89        3.948   3.099   2.707   2.474   2.317   2.202   2.114   2.044   1.987   1.939)
( 90        3.947   3.098   2.706   2.473   2.316   2.201   2.113   2.043   1.986   1.938)
( 91        3.946   3.097   2.705   2.472   2.315   2.200   2.112   2.042   1.984   1.936)
( 92        3.945   3.095   2.704   2.471   2.313   2.199   2.111   2.041   1.983   1.935)
( 93        3.943   3.094   2.703   2.470   2.312   2.198   2.110   2.040   1.982   1.934)
( 94        3.942   3.093   2.701   2.469   2.311   2.197   2.109   2.038   1.981   1.933)
( 95        3.941   3.092   2.700   2.467   2.310   2.196   2.108   2.037   1.980   1.932)
( 96        3.940   3.091   2.699   2.466   2.309   2.195   2.106   2.036   1.979   1.931)
( 97        3.939   3.090   2.698   2.465   2.308   2.194   2.105   2.035   1.978   1.930)
( 98        3.938   3.089   2.697   2.465   2.307   2.193   2.104   2.034   1.977   1.929)
( 99        3.937   3.088   2.696   2.464   2.306   2.192   2.103   2.033   1.976   1.928)
(100        3.936   3.087   2.696   2.463   2.305   2.191   2.103   2.032   1.975   1.927)
))

(defvar *F0.10* '(

;A:   1      2      3      4      5      6      7      8      9      10
;B:   
(  1       39.863  49.500  53.593  55.833  57.240  58.204  58.906  59.439  59.858  60.195)
(  2        8.526   9.000   9.162   9.243   9.293   9.326   9.349   9.367   9.381   9.392)
(  3        5.538   5.462   5.391   5.343   5.309   5.285   5.266   5.252   5.240   5.230)
(  4        4.545   4.325   4.191   4.107   4.051   4.010   3.979   3.955   3.936   3.920)
(  5        4.060   3.780   3.619   3.520   3.453   3.405   3.368   3.339   3.316   3.297)
(  6        3.776   3.463   3.289   3.181   3.108   3.055   3.014   2.983   2.958   2.937)
(  7        3.589   3.257   3.074   2.961   2.883   2.827   2.785   2.752   2.725   2.703)
(  8        3.458   3.113   2.924   2.806   2.726   2.668   2.624   2.589   2.561   2.538)
(  9        3.360   3.006   2.813   2.693   2.611   2.551   2.505   2.469   2.440   2.416)
( 10        3.285   2.924   2.728   2.605   2.522   2.461   2.414   2.377   2.347   2.323)
( 11        3.225   2.860   2.660   2.536   2.451   2.389   2.342   2.304   2.274   2.248)
( 12        3.177   2.807   2.606   2.480   2.394   2.331   2.283   2.245   2.214   2.188)
( 13        3.136   2.763   2.560   2.434   2.347   2.283   2.234   2.195   2.164   2.138)
( 14        3.102   2.726   2.522   2.395   2.307   2.243   2.193   2.154   2.122   2.095)
( 15        3.073   2.695   2.490   2.361   2.273   2.208   2.158   2.119   2.086   2.059)
( 16        3.048   2.668   2.462   2.333   2.244   2.178   2.128   2.088   2.055   2.028)
( 17        3.026   2.645   2.437   2.308   2.218   2.152   2.102   2.061   2.028   2.001)
( 18        3.007   2.624   2.416   2.286   2.196   2.130   2.079   2.038   2.005   1.977)
( 19        2.990   2.606   2.397   2.266   2.176   2.109   2.058   2.017   1.984   1.956)
( 20        2.975   2.589   2.380   2.249   2.158   2.091   2.040   1.999   1.965   1.937)
( 21        2.961   2.575   2.365   2.233   2.142   2.075   2.023   1.982   1.948   1.920)
( 22        2.949   2.561   2.351   2.219   2.128   2.060   2.008   1.967   1.933   1.904)
( 23        2.937   2.549   2.339   2.207   2.115   2.047   1.995   1.953   1.919   1.890)
( 24        2.927   2.538   2.327   2.195   2.103   2.035   1.983   1.941   1.906   1.877)
( 25        2.918   2.528   2.317   2.184   2.092   2.024   1.971   1.929   1.895   1.866)
( 26        2.909   2.519   2.307   2.174   2.082   2.014   1.961   1.919   1.884   1.855)
( 27        2.901   2.511   2.299   2.165   2.073   2.005   1.952   1.909   1.874   1.845)
( 28        2.894   2.503   2.291   2.157   2.064   1.996   1.943   1.900   1.865   1.836)
( 29        2.887   2.495   2.283   2.149   2.057   1.988   1.935   1.892   1.857   1.827)
( 30        2.881   2.489   2.276   2.142   2.049   1.980   1.927   1.884   1.849   1.819)
( 31        2.875   2.482   2.270   2.136   2.042   1.973   1.920   1.877   1.842   1.812)
( 32        2.869   2.477   2.263   2.129   2.036   1.967   1.913   1.870   1.835   1.805)
( 33        2.864   2.471   2.258   2.123   2.030   1.961   1.907   1.864   1.828   1.799)
( 34        2.859   2.466   2.252   2.118   2.024   1.955   1.901   1.858   1.822   1.793)
( 35        2.855   2.461   2.247   2.113   2.019   1.950   1.896   1.852   1.817   1.787)
( 36        2.850   2.456   2.243   2.108   2.014   1.945   1.891   1.847   1.811   1.781)
( 37        2.846   2.452   2.238   2.103   2.009   1.940   1.886   1.842   1.806   1.776)
( 38        2.842   2.448   2.234   2.099   2.005   1.935   1.881   1.838   1.802   1.772)
( 39        2.839   2.444   2.230   2.095   2.001   1.931   1.877   1.833   1.797   1.767)
( 40        2.835   2.440   2.226   2.091   1.997   1.927   1.873   1.829   1.793   1.763)
( 41        2.832   2.437   2.222   2.087   1.993   1.923   1.869   1.825   1.789   1.759)
( 42        2.829   2.434   2.219   2.084   1.989   1.919   1.865   1.821   1.785   1.755)
( 43        2.826   2.430   2.216   2.080   1.986   1.916   1.861   1.817   1.781   1.751)
( 44        2.823   2.427   2.213   2.077   1.983   1.913   1.858   1.814   1.778   1.747)
( 45        2.820   2.425   2.210   2.074   1.980   1.909   1.855   1.811   1.774   1.744)
( 46        2.818   2.422   2.207   2.071   1.977   1.906   1.852   1.808   1.771   1.741)
( 47        2.815   2.419   2.204   2.068   1.974   1.903   1.849   1.805   1.768   1.738)
( 48        2.813   2.417   2.202   2.066   1.971   1.901   1.846   1.802   1.765   1.735)
( 49        2.811   2.414   2.199   2.063   1.968   1.898   1.843   1.799   1.763   1.732)
( 50        2.809   2.412   2.197   2.061   1.966   1.895   1.840   1.796   1.760   1.729)
( 51        2.807   2.410   2.194   2.058   1.964   1.893   1.838   1.794   1.757   1.727)
( 52        2.805   2.408   2.192   2.056   1.961   1.891   1.836   1.791   1.755   1.724)
( 53        2.803   2.406   2.190   2.054   1.959   1.888   1.833   1.789   1.752   1.722)
( 54        2.801   2.404   2.188   2.052   1.957   1.886   1.831   1.787   1.750   1.719)
( 55        2.799   2.402   2.186   2.050   1.955   1.884   1.829   1.785   1.748   1.717)
( 56        2.797   2.400   2.184   2.048   1.953   1.882   1.827   1.782   1.746   1.715)
( 57        2.796   2.398   2.182   2.046   1.951   1.880   1.825   1.780   1.744   1.713)
( 58        2.794   2.396   2.181   2.044   1.949   1.878   1.823   1.779   1.742   1.711)
( 59        2.793   2.395   2.179   2.043   1.947   1.876   1.821   1.777   1.740   1.709)
( 60        2.791   2.393   2.177   2.041   1.946   1.875   1.819   1.775   1.738   1.707)
( 61        2.790   2.392   2.176   2.039   1.944   1.873   1.818   1.773   1.736   1.705)
( 62        2.788   2.390   2.174   2.038   1.942   1.871   1.816   1.771   1.735   1.703)
( 63        2.787   2.389   2.173   2.036   1.941   1.870   1.814   1.770   1.733   1.702)
( 64        2.786   2.387   2.171   2.035   1.939   1.868   1.813   1.768   1.731   1.700)
( 65        2.784   2.386   2.170   2.033   1.938   1.867   1.811   1.767   1.730   1.699)
( 66        2.783   2.385   2.169   2.032   1.937   1.865   1.810   1.765   1.728   1.697)
( 67        2.782   2.384   2.167   2.031   1.935   1.864   1.808   1.764   1.727   1.696)
( 68        2.781   2.382   2.166   2.029   1.934   1.863   1.807   1.762   1.725   1.694)
( 69        2.780   2.381   2.165   2.028   1.933   1.861   1.806   1.761   1.724   1.693)
( 70        2.779   2.380   2.164   2.027   1.931   1.860   1.804   1.760   1.723   1.691)
( 71        2.778   2.379   2.163   2.026   1.930   1.859   1.803   1.758   1.721   1.690)
( 72        2.777   2.378   2.161   2.025   1.929   1.858   1.802   1.757   1.720   1.689)
( 73        2.776   2.377   2.160   2.024   1.928   1.856   1.801   1.756   1.719   1.687)
( 74        2.775   2.376   2.159   2.022   1.927   1.855   1.800   1.755   1.718   1.686)
( 75        2.774   2.375   2.158   2.021   1.926   1.854   1.798   1.754   1.716   1.685)
( 76        2.773   2.374   2.157   2.020   1.925   1.853   1.797   1.752   1.715   1.684)
( 77        2.772   2.373   2.156   2.019   1.924   1.852   1.796   1.751   1.714   1.683)
( 78        2.771   2.372   2.155   2.018   1.923   1.851   1.795   1.750   1.713   1.682)
( 79        2.770   2.371   2.154   2.017   1.922   1.850   1.794   1.749   1.712   1.681)
( 80        2.769   2.370   2.154   2.016   1.921   1.849   1.793   1.748   1.711   1.680)
( 81        2.769   2.369   2.153   2.016   1.920   1.848   1.792   1.747   1.710   1.679)
( 82        2.768   2.368   2.152   2.015   1.919   1.847   1.791   1.746   1.709   1.678)
( 83        2.767   2.368   2.151   2.014   1.918   1.846   1.790   1.745   1.708   1.677)
( 84        2.766   2.367   2.150   2.013   1.917   1.845   1.790   1.744   1.707   1.676)
( 85        2.765   2.366   2.149   2.012   1.916   1.845   1.789   1.744   1.706   1.675)
( 86        2.765   2.365   2.149   2.011   1.915   1.844   1.788   1.743   1.705   1.674)
( 87        2.764   2.365   2.148   2.011   1.915   1.843   1.787   1.742   1.705   1.673)
( 88        2.763   2.364   2.147   2.010   1.914   1.842   1.786   1.741   1.704   1.672)
( 89        2.763   2.363   2.146   2.009   1.913   1.841   1.785   1.740   1.703   1.671)
( 90        2.762   2.363   2.146   2.008   1.912   1.841   1.785   1.739   1.702   1.670)
( 91        2.761   2.362   2.145   2.008   1.912   1.840   1.784   1.739   1.701   1.670)
( 92        2.761   2.361   2.144   2.007   1.911   1.839   1.783   1.738   1.701   1.669)
( 93        2.760   2.361   2.144   2.006   1.910   1.838   1.782   1.737   1.700   1.668)
( 94        2.760   2.360   2.143   2.006   1.910   1.838   1.782   1.736   1.699   1.667)
( 95        2.759   2.359   2.142   2.005   1.909   1.837   1.781   1.736   1.698   1.667)
( 96        2.759   2.359   2.142   2.004   1.908   1.836   1.780   1.735   1.698   1.666)
( 97        2.758   2.358   2.141   2.004   1.908   1.836   1.780   1.734   1.697   1.665)
( 98        2.757   2.358   2.141   2.003   1.907   1.835   1.779   1.734   1.696   1.665)
( 99        2.757   2.357   2.140   2.003   1.906   1.835   1.778   1.733   1.696   1.664)
(100        2.756   2.356   2.139   2.002   1.906   1.834   1.778   1.732   1.695   1.663)
))


(defun wilcoxon-1 (initial-values target)
  "Nonparametric one-sample (signed) rank test (Wilcoxon)
   From :http://www.graphpad.com/instatman/HowtheWilcoxonranksumtestworks.htm"
  (let ((n 0) 
	r/v/d* 
	(sum+ranks 0)
	(sum-ranks 0))
    ;; 1. Calculate how far each value is from the hypothetical value.
    ;; 2. Ignore values that exactly equal the hypothetical value. Call the
    ;;   number of remaining values N.
    (dolist (v initial-values)
      (when (not (= target v))
	(push (list 'rank v (abs (- target v))) r/v/d*)
	(incf n)))
    ;; 3. Rank these distances, paying no attention to whether the values are
    ;;    higher or lower than the hypothetical value.
    (setq r/v/d* (sort r/v/d* #'(lambda (a b) (< (third a) (third b)))))
    ;; Ranking doesn't deal with ties!!!
    (loop for entry in r/v/d*
	  as rank from 1 by 1
	  do (setf (car entry) rank))
    ;; 4. For each value that is lower than the hypothetical value, multiply
    ;;    the rank by negative 1.
    (loop for entry in r/v/d*
	  as (rank value nil) = entry
	  when (< value target)
	  do (setf (car entry) (- rank)))
    ;; 5. Sum the positive ranks. InStat reports this value.
    ;; 6. Sum the negative ranks. InStat also reports this value.
    ;; 7. Add the two sums together. This is the sum of signed ranks, which
    ;;    InStat reports as W.
    (print r/v/d*)
    ;; If the data really were sampled from a population with the
    ;; hypothetical mean, you'd expect W to be near zero. If W (the sum of
    ;; signed ranks) is far from zero, the P value will be small. The P value
    ;; answers this question: Assume that you randomly sample N values from a
    ;; population with the hypothetical median. What is the chance that W
    ;; will be as far from zero (or further) as you observed?
    (loop for (rank nil nil) in r/v/d*
	  do (cond ((> rank 0) (incf sum+ranks rank))
		   ((< rank 0) (incf sum-ranks rank))
		   )
	  finally (return (values sum+ranks sum-ranks (+ sum+ranks sum-ranks))))
    ))

#+not-completed
(defun rank-values (v*)
   "Rank a list properly, including dealing with ties.  This takes a 
   value list (30 20 30 40 50 60 ...) and returns the list sorted and 
   ranked in pairs, as (for the above): ((20 . 1) (30 . 2.5) (30 . 2.5) (40 . 4) (50 . 5)...)
   Notice that the two 30's split the rank position for 2 and 3 equally; Any number 
   of ties will split the positional difference, so, three ties will end up on an integer.
   This is done in two passes.  First we sort and assign sequential integers.  Then
   we re-assign ties."
  (let* ((v/r* (loop for v in v* collect (cons v 'rank)))
	 (v/r* (sort v/r* #'(lambda (a b) (< (car a) (car b)))))
	 (ignore (loop for v/r in v/r* as rank from 1 by 1 
		       do (setf (cdr v/r) rank)))
	 )

    (loop for v/r+ on v/r*
	  as major-rank from 1 by 1
	  with minor-rank = 1
	  as count = (loop with target = (caar v/r+)
			   as (next . minor-rank) in (cdr v/r+)
			   as k from 1 by 1
			   until (not (= next target))
			   finally (return k))
	  do 
	  (print (list (car v/r+) minor-rank count))
	  (when (not (= 1 count))
		(let ((new-rank (+ minor-rank (/ count 2.0))))
		  (loop with target = (caar v/r+)
			as entry in (cdr v/r+)
			as (next . minor-rank) = entry
			until (not (= next target))
			do (setf (cdr entry) new-rank))))
	  (setf minor-rank (+ rank count))
	  )
    v/r*))
