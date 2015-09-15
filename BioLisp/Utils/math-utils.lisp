;;; -*- Package: utils; mode: lisp; base: 10; Syntax: Common-Lisp; -*-

 (in-package :utils)

;;; +=========================================================================+
;;; | Copyright (c) 2002-3005 JP Massar, Jeff Shrager, Mike Travers           |
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

;;; Authors:  JP Massar, Jeff Shrager

(eval-when (:compile-toplevel :load-toplevel :execute)
  
  (defparameter *utility-math-user-symbols* 
    '(
      fixnump
      mean
      sigma
      factorial
      combinations-of
      integrate-numerically
      linear-least-squares-fit
      ))

  (defparameter *utility-math-api-symbols*
    (append
     *utility-math-user-symbols*
    '(
      thesum
      sqr
      correlate
      correlate-fast
      )))

  (export *utility-math-api-symbols* (find-package :utils)))

;;; ================================================================
;;; Some statistical functions.  Although some of these are trivial, they
;;; variously protect against weird conditions, and handle lists of numbers.


(defun fixnump (n)
  "Returns T if N is an integer in the fixnum range."
  (and (integerp n) 
       (>= n #.most-negative-fixnum) (<= n #.most-positive-fixnum)))

(defun thesum (l &aux (sum 0))
  "the arithmetic sum of a list of numbers"
  (dolist (n l) (incf sum n)) sum)

(defun sqr (a)
  "The square of a number, or the elementwise square of a numeric sequence"
  (if (numberp a) (expt a 2)
      (map (type-of a) #'* a a)))

(defun mean (seq)
  "the arithmetic mean of a list of numbers"
  (/ (reduce '+ seq) (float (length seq))))

;;; --- Correlation of two sequences, as in Ferguson & Takane, 1989,
;;; p. 125.  Assumes NO MISSING VALUES!

(defun correlate (x y)
  "Mathematical correlation function"
  (if (not (= (length x) (length y)))
      (error "Can only correlate equal-sized sets."))
  (let* ((mx (mean x))
         (my (mean y))
         (devx (mapcar #'(lambda (v) (- v mx)) x))
         (devy (mapcar #'(lambda (v) (- v my)) y))
         (sumdevxy (thesum (mapcar #'* devx devy)))
         (sumsqdevx (thesum (sqr devx)))
         (sumsqdevy (thesum (sqr devy)))
         (r (/ sumdevxy (sqrt (* sumsqdevx sumsqdevy))))
         )
    (list :r r :r2 (sqr r))
    ))

;; Coerces all data to single-float and does all
;; computation in single float.
(defun correlate-fast (x y)
  "Optimized mathematical correlation function using floating point"
  #.(optimization-declaration)
  (let ((xlen (length x)) (ylen (length y))
        (mx 0.0) (my 0.0)
        (sumdevxy 0.0)
        (sumsqdevx 0.0) (sumsqdevy 0.0)
        (xelem 0.0) (yelem 0.0)
        (xdev 0.0) (ydev 0.0)
        (r 0.0))
    (declare (fixnum xlen ylen))
    (declare (single-float mx my sumdevxy sumsqdevx))
    (declare (single-float sumsqdevy xelem yelem))
    (declare (single-float xdev ydev r))
    (unless (= xlen ylen) 
      (error "Can only correlate equal-length data"))
    (labels ((thesum (x) 
	       (let ((sum 0.0))
		 (declare (single-float sum))
		 (dolist (elem x sum) (incf sum (float elem 0.0)))))
             (mean (x) (/ (the single-float (thesum x)) xlen)))
      (setq mx (mean x) my (mean y))
      (do ((xdata x (cdr xdata)) (ydata y (cdr ydata)))
          ((null xdata))
        (setq xelem (float (car xdata) 0.0)
	      yelem (float (car ydata) 0.0))
        (setq xdev (- xelem mx) ydev (- yelem my))
        (incf sumdevxy (* xdev ydev))
        (incf sumsqdevx (* xdev xdev))
        (incf sumsqdevy (* ydev ydev)))
      (setq r (/ sumdevxy 
		 (the single-float (sqrt (* sumsqdevx sumsqdevy)))
		 )))
    (list :r r :r2 (* r r))
    ))
          
;;; Algorithm due to Richard Fateman, posted on comp.lang.lisp
;;; (Completely inscrutable)

(defun factorial (n)
  "Computes factorial using optimized algorithm."
  #.(optimization-declaration)
  (declare (fixnum n))
  (cond
   ((not (integerp n)) (error "Cannot compute factorial of non-integer"))
   ((minusp n) (error "Cannot compute factorial of negative number"))
   ((zerop n) 1)
   (t
    (let ((shift 0))
      (declare (fixnum shift))
      (labels
          ((kg1 (n m)
             (declare (fixnum n m))
             (cond ((and (evenp n) (> m 1))
                    (incf shift (ash n -1))
                    (kg1 (ash n -1) (ash m -1)))
                   ((<= n m) n)
                   (t (* (kg1 n (ash m 1))
                         (kg1 (- n m) (ash m 1)))))))
        (let ((kg-result (kg1 n 1)))
          (ash kg-result shift)
          ))))))

#+test
(defun ! (n) 
  (cond
   ((zerop n) 1)
   ((= n 1) 1)
   (t (* n (! (1- n))))))


(defun combinations-of (n k)
   "Calculates (N choose K), or N!/(K!(N-K)!) in a semi-efficient manner"
   #.(optimization-declaration)
   (unless (and (integerp n) (integerp k) (<= 0 n) (<= 0 k) (<= k n))
     (error "Invalid arguments: ~S, ~S" n k))
   (locally
     (declare (fixnum n k))
     (cond
      ((or (= k 0) (= k n)) 1)
      ((or (= k 1) (= k (the fixnum (1- n)))) n)
      ((or (= k 2) (= k (the fixnum (- n 2))))
       (/ (* n (the fixnum (1- n))) 2))
      ((< k (/ n 2)) (combinations-of n (the fixnum (- n k))))
      (t
       (let ((result 1))
         (loop for j fixnum from n downto (the fixnum (1+ k))
               for i fixnum from (the fixnum (- n k)) downto 1
               do
               (setq result (* result (/ j i)))
               finally (return result)
               ))))))

#+test
(defun choose (n k) (/ (! n) (* (! k) (! (- n k)))))

(defun sigma (data-list)
  (let ((mean (mean data-list)) (sum 0.0) (len 0))
    (loop for d in data-list 
          as temp = (- d mean)
          do
          (incf sum (* temp temp))
          (incf len)
          finally (return (sqrt (/ sum len)))
          )))

(defun integrate-numerically (f lower-limit upper-limit dx)
  (unless (>= upper-limit (+ lower-limit dx))
    (error "Delta not smaller than integration interval!"))
  (let ((lower-value (funcall f lower-limit))
        (next-value (funcall f (+ lower-limit dx)))
        (x (+ lower-limit dx))
        (sum 0.0d0))
    (loop until (< upper-limit x) do
          (setq next-value (funcall f x))
          (incf sum (* dx (/ (+ lower-value next-value) 2.0d0)))
          (setq x (+ x dx))
          (setq lower-value next-value)
          finally 
          (incf sum (* (- upper-limit (- x dx)) 
                       (/ (+ (funcall f (- x dx)) (funcall f upper-limit)) 
                          2.0d0
                          ))))
    sum
    ))

;; http://en.wikipedia.org/wiki/Linear_least_squares#Motivational_example
(defun linear-least-squares-fit (x-sequence y-sequence)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (let ((xs 0.0) (x2s 0.0) (ys 0.0) (xys 0.0) (n 0))
    (declare (single-float xs x2s ys xys) (fixnum n))
    (map 
     nil
     (lambda (x y)
       (let ((x (float x)) (y (float y)))
         (declare (single-float x y))
         (incf n)
         (incf xs x)
         (incf x2s (* x x))
         (incf ys y)
         (incf xys (* x y))
         ))
     x-sequence y-sequence
     )
    (let* ((slope (/ (- (* n xys) (* ys xs)) (- (* n x2s) (* xs xs))))
           (intercept (- (/ ys n) (* slope (/ xs n)))))
      (declare (single-float slope intercept))
      (values slope intercept)
      )))
     
       
