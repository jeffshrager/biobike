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

;;; Author:  JP Massar


(eval-when (:compile-toplevel :load-toplevel :execute)

  (defparameter *utility-array-api-symbols*
    '(
      ;; general array ops
      init-array
      copy-array
      matrix-row-slice
      slice-to-matrix-row
      matrix-col-slice
      slice-to-matrix-col
      defarrayop
      defgenarrayop
      ;; float array ops
      sf-make-array
      pp-sf-vector
      pp-sf-matrix
      sf-constant-vector
      sf-constant-array
      sf-copy-vector
      sf-iota-vector
      sf-identity
      list-to-sf-matrix
      seq-to-sf-vector
      vector-of-sf-vectors
      sf-matrix-row-slice
      sf-inner-product
      sf-inner-product-n
      sf-matmult-vv
      sf-matmult-va
      sf-matmult-av
      sf-matmult-aa
      sf-matmult
      ))

  (export *utility-array-api-symbols* (find-package :utils))

  )


;;;; SIMPLE GENERAL VECTOR/ARRAY UTILITIES


(defun init-array (x &optional (v 0.0))
  "Initialize the elements of an arbitrary-dimensioned array X to V"
  (loop for j fixnum below (array-total-size x) do
        (setf (row-major-aref x j) v)))

(defun copy-array (d s)
  #.(one-string-nl
     "Copy the elements of an arbitrary-dimensioned array S to an array D"
     "of arbitrary dimensions and size.  Copying is done in row major order"
     "until D is full.  S must have at least as many elements as D.")
  (loop for j fixnum below (array-total-size d) do
	(setf (row-major-aref d j) (row-major-aref s j))))


;;;; RETRIEVING INDIVIDUAL ROWS AND COLUMNS FROM AN ARBITRARY MATRIX


(defun matrix-row-slice (dest src row)
  "Copy a row of a 2d array SRC to a vector, DEST"
  (dotimes (j (array-dimension dest 0))
    (setf (aref dest j) (aref src row j))))

(defun slice-to-matrix-row (dest src row)
  "Copy a vector SRC to a row of a 2d array DEST"
  (dotimes (j (length src)) (setf (aref dest row j) (aref src j))))

(defun matrix-col-slice (dest src col)
  "Copy a column of a 2d array SRC to a vector, DESt"
  (dotimes (j (array-dimension dest 0))
    (setf (aref dest j) (aref src j col))))

(defun slice-to-matrix-col (dest src col)
  "Copy a vector SRC to a column of a 2d array DEST"
  (dotimes (j (length src)) (setf (aref dest j col) (aref src j))))


;;;; MACROS TO GENERATE ARRAY/VECTOR OPERATORS


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun generate-multiple-dotimes (n index-names limits body)
    `(dotimes (,(first index-names) ,(first limits))
       (declare (fixnum ,(first index-names)))
       ,(if (= n 1)
            body
          (generate-multiple-dotimes 
           (1- n) (cdr index-names) (cdr limits) body)
          ))))
     
(defmacro defarrayop (name op rank nargs etype)

  #.(one-string-nl
     "Create a function of NARGS arrays of rank RANK into a destination array"
     "of the same rank.  The function performs element-wise OP.")

  (let ((funcall? 
         (cond
          ((symbolp op) nil)
          ((and (listp op) (eql (length op) 2) (eq 'quote (first op)))
           (setq op (second op)) nil)
          ((and (listp op) (eql (length op) 2) (eq 'function (first op)))
           (cond
            ((symbolp (second op))
             (setq op (second op)) nil)
            (t t)
            ))
          (t (error "Unknown OP form: ~A" op))
          ))
        (argnames nil)
        (index-names nil)
        (limit-names nil)
        (is-scalar-names nil))

    (dotimes (j (1- nargs))
      (push (intern (format nil "SRC~D" (1+ j))) argnames)
      (push (intern (format nil "IS-SCALAR-~D" (1+ j))) is-scalar-names))
    (setq argnames (nreverse argnames))
    (push 'dest argnames)
    (setq is-scalar-names (nreverse is-scalar-names))
    (dotimes (j rank)
      (push (intern (format nil "I~D" (1+ j))) index-names)
      (push (intern (format nil "LIMIT-~D" (1+ j))) limit-names))
    (setq index-names (nreverse index-names))
    (setq limit-names (nreverse limit-names))

    (let* ((limit-var-let-forms
            (mapcar 
             #'(lambda (limit-name array-dimension)
                 `(,limit-name (array-dimension dest ,array-dimension)))
             limit-names
             (iota rank)))
           (limit-var-declare-form `(declare (fixnum ,@limit-names)))
           (is-scalar-let-forms
            (mapcar 
             #'(lambda (x argname) `(,x (not (arrayp ,argname))))
             is-scalar-names (cdr argnames)))
           (function-arguments
            (mapcar
             #'(lambda (isn argname) 
                 `(the ,etype
                       (if ,isn 
                           (the ,etype ,argname)
                         (aref (the (simple-array ,etype ,rank) ,argname)
                               ,@index-names))))
             is-scalar-names (cdr argnames))))

      `(defun ,name ,argnames
         #.(optimization-declaration)
         (declare (type (simple-array ,etype ,rank) dest))
         (let (,@limit-var-let-forms ,@is-scalar-let-forms)
           ,limit-var-declare-form
           ,(generate-multiple-dotimes 
             rank index-names limit-names
             `(setf (aref dest ,@index-names)
                    ,(if funcall?
                         `(funcall ,op ,@function-arguments)
                       `(,op ,@function-arguments)
                       )))))

      )))


(defmacro defgenarrayop (name op nargs etype)

  #.(one-string-nl
     "Create a function of NARGS any-single-rank arrays of type ETYPE"
     "into a destination array of the same rank. The function performs"
     "element-wise OP.")

  (let ((funcall? 
         (cond
          ((symbolp op) nil)
          ((and (listp op) (eql (length op) 2) (eq 'quote (first op)))
           (setq op (second op)) nil)
          ((and (listp op) (eql (length op) 2) (eq 'function (first op)))
           (cond
            ((symbolp (second op))
             (setq op (second op)) nil)
            (t t)
            ))
          (t (error "Unknown OP form: ~A" op))
          ))
        (argnames nil)
        (is-scalar-names nil))

    (dotimes (j (1- nargs))
      (push (intern (format nil "SRC~D" (1+ j))) argnames)
      (push (intern (format nil "IS-SCALAR-~D" (1+ j))) is-scalar-names))
    (setq argnames (nreverse argnames))
    (push 'dest argnames)
    (setq is-scalar-names (nreverse is-scalar-names))

    (let* ((is-scalar-let-forms
            (mapcar 
             #'(lambda (x argname) `(,x (not (arrayp ,argname))))
             is-scalar-names (cdr argnames)))
           (function-arguments
            (mapcar
             #'(lambda (isn argname) 
                 `(the ,etype
                       (if ,isn 
                           (the ,etype ,argname)
                         (row-major-aref
                          (the (simple-array ,etype) ,argname) 
                          rmi
                          ))))
             is-scalar-names (cdr argnames))))

      `(defun ,name ,argnames
         (declare (type (simple-array ,etype) dest))
         (let (,@is-scalar-let-forms (size (array-total-size dest)))
           (declare (fixnum size))
           (dotimes (rmi size)
             (declare (fixnum rmi))
             (setf (row-major-aref dest rmi)
                   ,(if funcall?
                        `(funcall ,op ,@function-arguments)
                      `(,op ,@function-arguments)
                      )))))

      )))
