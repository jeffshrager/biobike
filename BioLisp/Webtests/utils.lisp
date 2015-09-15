;;; -*- mode: Lisp; Syntax: Common-Lisp; Package: weblistener; -*-

;;; Author:  JP Massar. 

(in-package :weblistener)

;;;;;;;;;;

;; stuff for utils-tests

(defmacro defutiltest (name &body body)
  `(tests:deftest ,name ,@body :chapter :utils))

(defmacro defutiltest= (name &body body)
  `(tests:deftest ,name ,@body :chapter :utils :comparison 'equal))

(defmacro defutiltest== (name &body body)
  `(tests:deftest ,name ,@body :chapter :utils :comparison 'equalp))

(defun distance-test-function (x y) (sqrt (+ (* x x) (* y y))))

(defmemoize memoized-distance-test-function (x y) (sqrt (+ (* x x) (* y y))))

(defmemoize 1-arg-test-function (x) (isqrt x))

(defun r+ (x) (reduce '+ x))
(defun squarep (x) (let ((y (isqrt x))) (= x (* y y))))

(defun stoken? (seq i)
  #.(optimization-declaration)
  (declare (simple-string seq) (fixnum i))
  (and (not (eql (schar seq i) #\Space))
       (or (zerop i) (eql (aref seq (the fixnum (1- i))) #\Space))))

(defun etoken? (seq i)
  #.(optimization-declaration)
  (declare (simple-string seq) (fixnum i))
  (and (eql (schar seq i) #\Space)
       (not (zerop i))
       (not (eql (schar seq (the fixnum (1- i))) #\Space))
       ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; stuff for garray-tests.lisp


(eval-when (:compile-toplevel :load-toplevel :execute)
  (import 'defeatest :utils))

(defmacro defeatest (name actual expected &optional (comparison 'equalp))
  `(tests:deftest 
    ,name 
    ,actual ,expected 
    :chapter :garrays
    :comparison ',comparison))

