;;; -*- mode: Lisp; Syntax: Common-Lisp; Package: utils; -*-

;;; Author:  JP Massar.

(in-package :utils)


(tests:deftest sf-inner-product-1 
         (sf-inner-product (sf-iota-vector 5) (sf-iota-vector 5)) 30.0
         :comparison #'= :chapter :arrayops)
(tests:deftest sf-inner-product-n-1
         (sf-inner-product-n 
          (sf-iota-vector 3) (sf-iota-vector 3) (sf-iota-vector 3)) 9.0
         :comparison #'= :chapter :arrayops)
(tests:deftest sf-matmult-1
         (let ((dest (sf-make-array '(3 3))))
           (sf-matmult dest (sf-identity 3) (sf-identity 3))
           dest)
         (sf-identity 3)
         :comparison #'equalp :chapter :arrayops)
(tests:deftest sf-matmult-2
         (let ((dest (sf-make-array '(2 2))))
           (sf-matmult dest (list-to-sf-matrix '((1 2) (3 4)))
                       (list-to-sf-matrix '((3 4) (1 2))))
           dest)
         (list-to-sf-matrix '((5 8) (13 20)))
         :comparison #'equalp :chapter :arrayops)                       
(tests:deftest sf-matmult-vv-1
         (let ((dest (sf-make-array '(3 2))))
           (sf-matmult dest (sf-iota-vector 3) (sf-iota-vector 2))
           dest)
         (list-to-sf-matrix '((0 0) (0 1) (0 2)))
         :comparison #'equalp :chapter :arrayops)                       
(tests:deftest sf-matmult-vm-1
         (let ((dest (sf-constant-vector 2 0.0)))
           (sf-matmult dest (sf-iota-vector 3) 
                       (list-to-sf-matrix '((2 3) (1 2) (3 1))))
           dest)
         #(7.0 4.0)
         :comparison #'equalp :chapter :arrayops)
(tests:deftest sf-matmult-mv-1
         (let ((dest (sf-constant-vector 2 0.0)))
           (sf-matmult dest (list-to-sf-matrix '((1 2 3) (3 2 1)))
                       (sf-iota-vector 3))
           dest)
         #(8.0 4.0)
         :comparison #'equalp :chapter :arrayops)
