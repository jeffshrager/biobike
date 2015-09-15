;;; -*- mode: Lisp; Syntax: Common-Lisp; Package: bioutils; -*-

;;; Author:  JP Massar.

(in-package :bioutils)

(defvar *td*)

(defparameter *test-table3-data*
  (make-array 
   '(4 9)
   :initial-contents
   ;; CODE	NAME	SSN	LEVEL0	LEVEL1	LEVEL2	LEVEL3	R1	R2
   '(("a" "Fred" "62-055"   1.0	  2.0   -3.0  50.0e-2 23 "the big man")
     ("v" "Wilma" "37-999"  2.05  3.0   17.0  0.50    24 "his wife")
     ("c" "Barney" "12-171" 3.0	  4.0   5.0   0       89 "his friend")
     ("d" "Dino" "37-999"   11.0  21.0  31.0  0.005e2 10 "his dog")
     )))

(defun ltt2 ()
  (setq *td*
        (let ((*warn-on-data-coercion* nil))
         (read-table-data
          (cl-user:translate-simple-lp "biol:Bioutils;test-table.txt")
          :name 'flintsones
          :n-doc-lines 2
          :n-posthdr-lines 4
          :n-predata-fields 3
          :n-postdata-fields 2
          :other-rdrfuncs `((:name ,#'(lambda (x) (bio::frame-fnamed x t))))
          :global-data-type 'single-float
          :missing-value 17.0
          :key-columns '(1 2 7)
          ))))

;;; Lispworks hack to compare scalar single floats (which are actually
;;; double floats) to single floats in arrays (which, if the element type
;;; of the array is 'single-float, are really single floats)

(defun vsf-equiv (v1 v2)
  (let ((sfv1 (make-array 1 :element-type 'single-float))
        (sfv2 (make-array 1 :element-type 'single-float)))
    (and (= (length v1) (length v2))
         (every #'(lambda (x y) 
                    (setf (aref sfv1 0) x)
                    (setf (aref sfv2 0) y)
                    (= (aref sfv1 0) (aref sfv2 0)))
                v1 v2
                ))))
