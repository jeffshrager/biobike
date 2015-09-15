;;; -*- mode: Lisp; Syntax: Common-Lisp; Package: utils; -*-

;;; Author:  JP Massar.

(in-package :utils)


;;;; TESTS

;; standard 1d Lisp array as GARRAY
(defeatest gref-la-1
         (let ((x nil) (value 5) (index 3))
           (setf (gref x index) value) 
           (setf (gref x 0) 10)
           (list (gref x index) (gref x 0)))
         (list 5 10))
;; standard 2d Lisp array as GARRAY
(defeatest gref-la-2
         (let ((x nil) (value 5) (i0 3) (i1 10))
           (setf (gref x i0 i1) value) 
           (setf (gref x 0 0) 10)
           (list (gref x i0 i1) (gref x 0 0)))
         (list 5 10))
;; extending an existing Lisp array GARRAY
(defeatest gref-la-3
         (let ((x nil) (value 5) (i0 3) (i1 10))
           (setf (gref x i0 i1) value) 
           (setf (gref x 20 20) 10)
           (list (gref x i0 i1) (gref x 20 20)))
         (list 5 10))
           
;; single axis HASH
(defeatest gref-hash-1
         (let ((x nil) (value 5) (index "abc"))
           (setf (gref x index) value) 
           (setf (gref x 0) 10)
           (list (gref x index) (gref x 0)))
         (list 5 10))
;; multiple axis HASH
(defeatest gref-hash-2
         (let ((x nil) (value 5) (i0 "abc") (i1 :apple) (i2 #\x))
           (setf (gref x i0 i1 i2) value) 
           (setf (gref x 0 0 0) 10)
           (list (gref x i0 i1 i2) (gref x 0 0 0)))
         (list 5 10))

;; Single array axis, single hash axis.
(defeatest gref-la-hash-1
         (let ((x nil) (value 5) (i0 10) (i1 :orange))
           (setf (gref x i0 i1) value) 
           (setf (gref x 0 0) 10)
           (list (gref x i0 i1) (gref x 0 0)))
         (list 5 10))

;; Single hash axis, single array axis
(defeatest gref-hash-la-1
         (let ((x nil) (value 5) (i0 :orange) (i1 10))
           (setf (gref x i0 i1) value) 
           (setf (gref x 0 0) 10)
           (list (gref x i0 i1) (gref x 0 0)))
         (list 5 10))  

;; Two array indices followed by three hashes
(defeatest gref-multiple-la-hash-1
           (let ((x 2) (value 5)
                 (i0 4) (i1 6) (i2 :apple) (i3 #\x) (i4 '(a b)))
             (setf (gref x i0 i1 i2 i3 i4) value)
             (setf (gref x 0 0 0 0 0) 10)
             (list (gref x i0 i1 i2 i3 i4) (gref x 0 0 0 0 0)))
           (list 5 10))

;; Two hash indices follow by three arrays
(defeatest gref-multiple-hash-la-1
           (let ((x 2) (value 5)
                 (i0 :apple) (i1 #\x) (i2 10) (i3 8) (i4 3))
             (setf (gref x i0 i1 i2 i3 i4) value)
             (setf (gref x 0 0 0 0 0) 10)
             (list (gref x i0 i1 i2 i3 i4) (gref x 0 0 0 0 0)))
           (list 5 10))


;; MAKE-GARRAY simple test
(defeatest gref-ma-la-1
           (let ((x (make-garray '(5 6) :adjustable t))
                 (value 5) (i0 2) (i1 3))
             (setf (gref x i0 i1) value)
             (setf (gref x 0 0) 10)
             (list (gref x i0 i1) (gref x 0 0)))
           (list 5 10))
             

;; 1d extended array as GARRAY
(defeatest gref-ma-xa-1
           (let ((x (make-garray '((-10 10))))
                 (i0 -10) (i1 -3) (i2 0) (i3 9))
             (setf (gref x i0) i0)
             (setf (gref x i1) i1)           
             (setf (gref x i2) i2)
             (setf (gref x i3) i3)
             (list (gref x i0) (gref x i1) (gref x i2) (gref x i3)))
           (list -10 -3 0 9)
           )
;; 2d extended array as GARRAY
(defeatest gref-ma-xa-2
           (let ((x (make-garray '((-10 10) (-8 -1))))
                 (i0 -10) (i1 -3) (i2 0) (i3 -2))
             (setf (gref x i0 i1) i0)
             (setf (gref x i2 i3) i1)           
             (list (gref x i0 i1) (gref x i2 i3)))
             (list -10 -3)
             )

;; Single negative numeric value
(defeatest 
 gref-ma-xa-3
 (let ((x (make-garray '(-10 3 -2))))
   (setf (gref x -10 0 0) 3)
   (setf (gref x 0 2 -2) 4)
   (list (gref x -10 0 0) (gref x 0 2 -2))
   )
 '(3 4)
 equal)

;; Num, hash, num, hash
(defeatest gref-nhnh-1
           (let ((x nil))
             (setf (gref x 3 :foo 4 :bar) 23)
             (gref x 3 :foo 4 :bar))
           23)

;; hash, num, hash, num, hash
(defeatest gref-hnhnh-1
           (let ((x nil))
             (setf (gref x #\a 3 :foo 4 :bar) 23)
             (gref x #\a 3 :foo 4 :bar))
           23)

;; Simple ENUM array
(defeatest gref-enum-1
           (let ((x (make-garray '((:fred :barney :wilma :betty)))))
             (setf (gref x :barney) 23)
             (gref x :barney))
           23)
;; 2d ENUM  array
(defeatest gref-enum-2
           (let ((x (make-garray '((enum 1 2 10) (:foo :bar :baz :quux)))))
             (setf (gref x 10 :baz) 23)
             (gref x 10 :baz))
           23)

;; Four different kinds of axes.
(defeatest gref-complex-1
           (let ((x (make-garray
                     '(10 (-20 20) (enum :x :y :z) (enum #\a #\b) $))))
             (setf (gref x 5 -15 :y #\a "a random string") 23)
             (gref x 5 -15 :y #\a "a random string"))
           23)

;; Extending an array with an extended axis.
(defeatest gref-extend-xa-1
           (let ((x (make-garray '((-3 3)) :initial-element 23)))
             (loop for j from -3 below 3 do (setf (gref x j) j))
             (setf (gref x -5) 100)
             (loop for j from -5 below 3 collect (gref x j)))
           '(100 23 -3 -2 -1 0 1 2))

;; Extending a 2d array.
(defeatest gref-extend-2d-1
           (let ((x (make-garray '(5 5))))
             (setf (gref x 0 0) 1)
             (setf (gref x 4 4) 2)
             (setf (gref x 7 7) 3)
             (list (gref x 0 0) (gref x 4 4) (gref x 7 7)))
           (list 1 2 3))

;; Setting and accessing every element in a garray.
(defeatest gref-loop-1
           (let ((x (make-garray '((-3 3) 2 (:foo :bar :baz)))))
             (loop for i0 from -3 below 3 do
                   (loop for i1 from 0 below 2 do
                         (loop for i2 in '(:foo :bar :baz) do
                               (setf (gref x i0 i1 i2) 0)
                               (unless (zerop (gref x i0 i1 i2))
                                 (error "Ruh roh in GREF-LOOP-1: 1")))))
             (loop for i0 from -3 below 3 do
                   (loop for i1 from 0 below 2 do
                         (loop for i2 in '(:foo :bar :baz) do
                               (unless (zerop (gref x i0 i1 i2))
                                 (error "Ruh roh in GREF-LOOP-1: 2")))))
             t)
           t)


;; lisp axis access error
(defeatest gref-error-1
           (handler-case
               (let ((x nil))
                 (setf (gref x 5) 10) (gref x 6) t)
             (error () nil))
           nil)

(defeatest gref-error-1a
           (handler-case
               (let ((x (make-garray '(7) :if-accessed-location-not-set 23)))
                 (setf (gref x 5) 10) (gref x 6))
             (error () nil))
           23)

;; extended axis access error
(defeatest 
 gref-error-2
 (handler-case
     (let ((x (make-garray '((-3 3)))))
       (setf (gref x 0) 10) (gref x 3) t)
   (error () nil))
 nil)

(defeatest 
 gref-error-2a
 (handler-case
     (let ((x (make-garray '((-3 3)) :if-accessed-location-not-set :default)))
       (setf (gref x 0) 10) (gref x 2))
   (error () nil))
 *garray-default-value*)

;; enum axis access error
(defeatest 
 gref-error-3
 (handler-case
     (let ((x (make-garray '((:enum :foo :bar :baz)) 
                           :if-accessed-location-not-set :error)))
       (setf (gref x :foo) 10) (gref x :quux) t)
   (error () nil))
 nil)

(defeatest 
 gref-error-3a
 (handler-case
     (let ((x (make-garray '((:enum :foo :bar :baz))
                           :if-accessed-location-not-set :blarf)))
       (setf (gref x :foo) 10) (gref x :bar))
   (error () nil))
 :blarf)

;; complicated access error
(defeatest 
 gref-error-4
 (handler-case
     (let ((x (make-garray '((:enum :foo :bar :baz) 10 $ (-3 3)))))
       (setf (gref x :foo 5 "aaa" 0) 10)
       (gref x :foo 5 "aaa" -10)
       t)
   (error () nil))
 nil)

(defeatest 
 gref-error-4
 (handler-case
     (let ((x (make-garray '((:enum :foo :bar :baz) 10 $ (-3 3)))))
       (setf (gref x :foo 5 "aaa" 0) 10)
       (gref x :foo 5 "aaa" 0)
       )
   (error () nil))
 10)

(defeatest gref-nested-1
           (let ((x (make-array 10)))
             (setf (aref x 0) (make-garray '(5 5)))
             (setf (gref (aref x 0) 2 2) 3)
             (gref (aref x 0) 2 2))
           3)

(defeatest gref-nested-2
           (let ((x (list (make-array 10) (make-array 10))))
             (setf (aref (first x) 0) (make-garray '($ $)))
             (setf (gref (aref (first x) 0) :foo :bar) :baz)
             (gref (aref (first x) 0) :foo :bar))
           :baz)


(defeatest gts-la-1
           (let ((x (make-garray '((10 20) (-3 5)))))
             (garray-potential-total-size x))
           80)

(defeatest gts-hash-1
           (let ((x (make-garray '($))))
             (setf (gref x 3) 4) (setf (gref x 4) 3)
             (garray-current-total-size x))
           2)

(defeatest gts-num-hash-1
           (let ((x (make-garray '((:enum :a :b :c) 4 $))))
             (setf (gref x :a 0 :foo) 1)
             (setf (gref x :b 0 :bar) 1)
             (setf (gref x :b 3 :bar) 1)
             (setf (gref x :b 3 :baz) 1)
             (garray-current-total-size x))
           4)

(defeatest gts-hash-num-1
           (let ((x (make-garray '($ (:enum :a :b :c)))))
             (setf (gref x :foo :a) 1)
             (setf (gref x :bar :c) 2)
             (garray-potential-total-size x))
           :infinite)


(defeatest gmap-la-1
           (let ((x (make-garray '(5 5))))
             (gmapset (lambda (y) (declare (ignore y)) 1) x)
             (apply '+ (gmap '1+ x :flatten? t)))
           50)

(defeatest gmap-hash-1
           (let ((x (make-garray '($ $))))
             (setf (gref x 'a 'b) 3)
             (setf (gref x 'b 'c) 4)
             (gmapset (lambda (y) (1+ y)) x)
             (apply '+ (gmap '1+ x :flatten? t)))
           11)

(defeatest gmap-num-hash-1
           (let ((x (make-garray '(2 2 $))))
             (setf (gref x 0 0 'a) 3)
             (setf (gref x 0 0 'b) 4)
             (setf (gref x 1 0 'c) 5)
             (list
              (apply '+ (gmap '1+ x :flatten? t))
              (progn
                (gmapset '1+ x)
                (apply '+ (gmap '1+ x :flatten? t))
                )))
           '(15 18)
           )

(defeatest gmap-hash-num-1
           (let ((x (make-garray '($ 2 2))))
             (setf (gref x 'a 0 0) 3)
             (setf (gref x 'b 0 0) 4)
             (setf (gref x 'c 1 0) 5)
             ;; set all 3x2x2 elements to 1
             (gmapset (lambda (y) (declare (ignore y)) 1) x)
             (apply '+ (gmap '1+ x :flatten? t :arm-missing-value 0))
             )
           12
           )

(defeatest 
 gci-1
 (let ((x (make-garray '(5 $))))
   (setf (gref x 0 12) 4)
   (setf (gref x 0 13) 4)
   (sort (garray-component-indices x 0) '<))
 '(12 13)
 equal)

(defeatest 
 gci-2
 (let ((x (make-garray '(5 4 (:foo :bar :baz)))))
   (garray-component-indices x 0 3))
 '(:foo :bar :baz)
 equal)

(defeatest 
 gci-3
 (let ((x (make-garray '(5 4 (-3 2)))))
   (garray-component-indices x 0 3))
 '(-3 -2 -1 0 1)
 equal)

(defeatest 
 gci-4
 (let ((x (make-garray '($ 3))))
   (setf (gref x "foo" 0) 1)
   (setf (gref x "zzz" 2) 1)
   (sort (garray-component-indices x) 'string<))
   '("foo" "zzz")
 equal)

(defeatest
 gcopy-1
 (let ((x (make-garray '(3 $ 3))))
   (setf (gref x 1 :foo 2) 23)
   (setf (gref x 0 "abc" 0) 24)
   (let ((y (garray-copy x)))
     (list (gref y 1 :foo 2) (gref y 0 "abc" 0))
     ))
 '(23 24)
 equal
 )

(defeatest
 gcopy-2
 (let ((x (make-garray '(2 $) :initial-element -1)))
   (setf (gref x 0 :foo) 1)
   (let ((y (garray-copy x)))
     (list (gref y 0 :foo) (gref y 1 :bar) (gref x 0 :baz))
     ))
 '(1 -1 -1)
 equal
 )

(defeatest
 gcg-1
 (let ((x (make-garray '(6 5 4 3 2) :initial-element 23)))
   (let ((y (garray-component-garray x 0 0)))
     (list (garray-rank y) (gref y 3 2 1))
     ))
 '(3 23)
 equal
 )

(defeatest
 gcg-2
 (let ((x (make-garray '(4 3 2) :initial-element 23)))
   (setf (gref x 0 0 0) (vector 1 2 3))
   (let ((y (garray-component-garray x 0 0)))
     (setf (aref (gref y 0) 0) 17)
     (aref (gref x 0 0 0) 0)
     ))
 17)

(defeatest
 gframes-1
 (when (find-package :frames)
   (let ((x (make-garray '(($ equalp))))
         (ff (symbol-of-package :frame-fnamed :frames)))
     (setf (gref x (funcall ff "test.xyzzy" t)) 3)
     (gref x (funcall ff "test.xyzzy" t))
     ))
 (when (find-package :frames) 3)
 )

(defeatest
 gframes-2
 (when (find-package :frames)
   (let ((x (make-garray '(($ equalp) ($ equalp))))
         (ff (symbol-of-package :frame-fnamed :frames)))
     (setf (gref x (funcall ff "test.xyzzy" t) (funcall ff "test.plugh" t)) 3)
     (gref x (funcall ff "test.xyzzy" t) (funcall ff "test.plugh" t))
     ))
 (when (find-package :frames) 3)
 )

(defeatest
 gframes-3
 (when (find-package :frames)
   (let ((x (make-garray '(($ equalp))))
         (ff (symbol-of-package :frame-fnamed :frames)))
     (setf (gref x (funcall ff "test.xyzzy" t)) 3)
     (setf (gref x (funcall ff "test.plugh" t)) 4)
     (sort (gmap (lambda (x) (1+ x)) x) '<)
     ))
 (when (find-package :frames) (list 4 5))
 )

(defeatest 
 extended-nonzero-based-1
  (let ((x (make-garray '((1 4)))))
    (setf (gref x 2) 3)
    (setf (gref x 10) 4)
    (list (gref x 2) (gref x 10)))
  '(3 4))
  