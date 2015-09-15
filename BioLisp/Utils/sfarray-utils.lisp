;;; -*- mode: Lisp; Syntax: Common-Lisp; Package: utils; -*-

(in-package :utils)

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


(deftype sfa (n) `(simple-array single-float ,n))
(deftype sfa1 () `(simple-array single-float 1))
(deftype sfa2 () `(simple-array single-float 2))


;;;; SIMPLE SINGLE-FLOAT VECTOR/ARRAY UTILITIES


(defun sf-make-array (dims &optional (init 0.0 init-provided))
  "Create an array of type single float, optionally initialized"
  (if init-provided
      (make-array dims :element-type 'single-float :initial-element init)
    (make-array dims :element-type 'single-float)))

(defun pp-sf-vector 
    (v &key (title nil) (format "~5,2F ") (prefix nil) (suffix nil))
  "Pretty print a vector of single floats to standard output"
  (when title (format t "~A: " title))
  (when prefix (format t "~A" prefix))
  (dotimes (j (length v)) (format t format (aref v j)))
  (when suffix (format t "~A" suffix))
  (terpri))

(defun pp-sf-matrix (m)
  "Pretty print a matrix of single floats to standard output"
  (terpri)
  (dotimes (i (array-dimension m 0))
    (dotimes (j (array-dimension m 1)) (format t "~5,2F " (aref m i j)))
    (terpri))
  (terpri))

(defun sf-constant-array (dims c)
  "Create an array of single floats with dimensions DIMS initialized to C"
  (make-array dims :element-type 'single-float :initial-element (float c 0.0)))

(defun sf-constant-vector (n c) 
  "Create a vector of single floats of length N initialized to C"
  (sf-constant-array n c))

(defun sf-copy-vector (vdest vsrc)
  "Copy the elements of a single float vector VSRC into VDEST"
  #.(optimization-declaration)
  (declare (type sfa1 vdest vsrc))
  (let ((len (length vdest)))
    (declare (fixnum len))
    (loop for j fixnum below len do (setf (aref vdest j) (aref vsrc j)))))

(defun sf-iota-vector (n)
  "Create a single float vector of length N whose Jth element is (float j)"
  #.(optimization-declaration)
  (declare (fixnum n))
  (let ((v (sf-make-array n)))
    (declare (type sfa1 v))
    (do ((j 0 (the fixnum (1+ j))) (fj 0.0 (1+ fj))) ((>= j n))
      (declare (fixnum j) (single-float fj))
      (setf (aref v j) fj))
    v))

;; Create an identity matrix.
(defun sf-identity (n)
  "Create an identity matrix of width and height N"
  #.(optimization-declaration)
  (declare (fixnum n))
  (let ((im (sf-make-array (list n n) 0.0)))
    (declare (type sfa2 im))
    (loop for j fixnum below n do (setf (aref im j j) 1.0) 
          finally (return im))))

(defun seq-to-sf-vector (seq)
  "Copy the elements of SEQ into a new single float vector"
  #.(optimization-declaration)
  (let ((n (length seq)))
    (declare (fixnum n))
    (let ((sfv (sf-make-array n)))
      (declare (type sfa1 sfv))
      (if (listp seq)
          (loop for element in seq
                for j fixnum from 0 do
                (setf (aref sfv j) (float element 0.0)))
        (loop for element across seq
              for j fixnum from 0 do
              (setf (aref sfv j) (float element 0.0))
              ))
      sfv)))

;; turn a list of lists into a 2d array.
(defun list-to-sf-matrix (list)
  #.(one-string-nl
     "Take a list of lists and make a single float 2d array out of"
     "the elements.  The elements of each sublist become the elements "
     "of one row of the array.")
  #.(optimization-declaration)
  (let ((nrows (length list)) (ncols (length (first list))))
    (let ((sfm (sf-make-array (list nrows ncols))))
      (declare (type sfa2 sfm))
      (loop for rows in list
            for i fixnum from 0 do
            (loop for element in rows
                  for j fixnum from 0 do
                  (setf (aref sfm i j) (float element 0.0))
                  ))
      sfm)))

(defun vector-of-sf-vectors (n-vectors sf-vector-size)
  #.(one-string-nl
     "Create a vector of length N-VECTORS, each containing a single float"
     "vector of length SF-VECTOR-SIZE initialized to 0.0.  (Can be used as an "
     "alternative representation of a matrix.")
  (let ((v (make-array n-vectors)))
    (dotimes (j n-vectors)
      (setf (aref v j) (sf-make-array sf-vector-size 0.0)))
    v))

(defun sf-matrix-row-slice (dest src row)
  #.(one-string-nl
     "Extract a row out of a single float matrix SRC into a single "
     "float vector DEST.")
  #.(optimization-declaration)
  (declare (type sfa1 dest) (type sfa2 src) (fixnum row))
  (loop for j fixnum below (length dest) do
        (setf (aref dest j) (aref src row j))))


;;;; SINGLE-FLOAT MATRIX and VECTOR MULTIPLICATION

;;; Inner product of vectors.  (Cross product not implemented).

;;; Matrix multiplication, with specific routines to do
;;; Vector x Vector, Vector x Matrix, Matrix x Vector, Matrix x Matrix
;;  and a general routine, SF-MATMULT which handles all these cases.

(defun sf-inner-product (v1 v2)
  "Return the inner (dot) product of single float vectors V1 and V2"
  #.(optimization-declaration)
  (declare (type sfa1 v1 v2))
  (let ((sum 0.0) (n (length v1)))
    (declare (single-float sum) (fixnum n))
    (loop for j fixnum below n do
          (incf sum (* (aref v1 j) (aref v2 j))))
    sum
    ))

(defun sf-inner-product-n (v1 v2 &rest more-vs)
  "Return the inner (dot) product of multiple single float vectors"
  (declare (type sfa1 v1 v2))
  (if (null more-vs)
      (sf-inner-product v1 v2)
    (let ((sum 0.0) (n (length v1)))
      (declare (single-float sum) (fixnum n))
      (dotimes (j n)
        (declare (fixnum j))
        (let ((product (* (aref v1 j) (aref v2 j))))
          (declare (single-float product))
          (dolist (v more-vs)
            (locally (declare (type sfa1 v)))
            (setq product (* product (aref v j))))
          (incf sum product)))
      sum
      )))

(defun sf-matmult-vv (dest src1 src2 &optional (addto nil))
  #.(one-string-nl
     "Matrix multiplication using two single float vectors, the first treated "
     "as a column vector, the second as a row vector.")
  (declare (type sfa2 dest))
  (declare (type sfa1 src1 src2))
  (let ((nrows-in-result (length src1)) (ncols-in-result (length src2)))
    (declare (fixnum nrows-in-result ncols-in-result))
    (loop for i fixnum below nrows-in-result
          for val of-type single-float across src1 do
          (loop for j fixnum below ncols-in-result do
                (if addto
                    (incf (aref dest i j) (* val (aref src2 j)))
                  (setf (aref dest i j) (* val (aref src2 j)))
                  )))))

(defun sf-matmult-va (dest src1 src2)
  "Single float matrix multiplication of a row vector SRC1 and a matrix SRC2"
  (declare (type sfa1 src1))
  (declare (type sfa2 src2))
  (let ((dest-is-vector? (vectorp dest))
        (ncols (array-dimension src2 1))
        (vlen (length src1))
        (sum 0.0))
    (declare (fixnum ncols vlen) (single-float sum))
    (loop for j fixnum below ncols do
          (setq sum 0.0)
          (loop for i fixnum below vlen do
                (incf sum (* (aref src1 i) (aref src2 i j))))
          (if dest-is-vector?
              (setf (aref (the sfa1 dest) j) sum)
            (setf (aref (the sfa2 dest) 0 j) sum)
            ))))
                
(defun sf-matmult-av (dest src1 src2)
  "Single float matrix multiplication of matrix SRC1 and column vector SRC2"
   (declare (type sfa2 src1))
   (declare (type sfa1 src2))
   (let ((dest-is-vector? (vectorp dest))
         (nrows (array-dimension src1 0))
         (vlen (length src2))
         (sum 0.0))
     (declare (fixnum nrows vlen) (single-float sum))
     (loop for i fixnum below nrows do
           (setq sum 0.0)
           (loop for j fixnum below vlen do
                 (incf sum (* (aref src1 i j) (aref src2 j))))
           (if dest-is-vector?
              (setf (aref (the sfa1 dest) i) sum)
            (setf (aref (the sfa2 dest) i 0) sum)
            ))))

(defun sf-matmult-aa (dest src1 src2 &optional (addto nil))
  "Standard matrix multiplication of two single float 2d arrays"
  (declare (type sfa2 dest src1 src2))
  (let ((nrows (array-dimension src1 0))
        (ncols (array-dimension src2 1))
        (nmults (array-dimension src1 1))
        (sum 0.0))
    (declare (fixnum nrows ncols nmults) (single-float sum))
    (loop for i fixnum below nrows do
          (loop for j fixnum below ncols do
                (setq sum 0.0)
                (loop for k fixnum below nmults do
                      (incf sum (* (aref src1 i k) (aref src2 k j))))
                (if addto 
                    (incf (aref dest i j) sum)
                  (setf (aref dest i j) sum)
                  )))))

(defun sf-matmult (dest src1 src2)
  "General single float matrix multiply for any combo of vectors and matrices" 
  (cond
   ((and (vectorp src1) (vectorp src2))
    (sf-matmult-vv dest src1 src2))
   ((vectorp src1)
    (sf-matmult-va dest src1 src2))
   ((vectorp src2)
    (sf-matmult-av dest src1 src2))
   (t
    (sf-matmult-aa dest src1 src2))
   ))
          

