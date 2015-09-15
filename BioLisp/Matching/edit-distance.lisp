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

;;; Author:  JP Massar

;;; Compute the edit distance between two strings.

#|

Computing the edit distance, i.e., the minimum number of insertions,
deletions and replacements necessary to turn string A into string B,
is a simple and useful example of dynamic programming.

For instance, to turn "fireman" into "policeman" requires 4 changes:

1. Turn the F into P
2. Add an O
3. Add an L
4. Turn the R into C

A dynamic programming algorithm typically uses an array S of dimensions 
LENGTH(A)+1 x LENGTH(B)+1.

A dynamic programming algorithm specifies initial conditions on this
array and a recursion relationship defining the value of S(i,j) in
terms of one or more previously computed elements S(k<=i,l<=j) of the array.

For all i and j, S(i,j) represents the edit distance of the two substrings
A[1...i], B[1...j].  Therefore, when we are done, the lower right corner
cell of the array, S(LENGTH(A),LENGTH(B)), is our answer.

The initial conditions are

S(i,0) = i
S(0.j) = j

Because a 0 index represents a null string, and it clearly takes N
insertion operations to change a null string into any string of length N.

The recursion relationship is

S(i,j) = MIN { S(i-1,j)+1, S(i,j-1)+1, S(i-1,j-1) + mismatch(A[i],B[j] }

where mismatch(c1,c2) = (if (= c1 c2) 0 1)

In English, this says that to find the edit distance for A[1...I] and
B[1...J], we can use the edit score of A[1...(I-1)] and B[1...J] and 
add a character to A, increasing the edit distance by 1, or we can use the 
edit score of A[1...I] and B[1...J-1] and delete a character from A, 
increasing the edit distance by 1, or we use the edit score of
A[1...I-1] and B[1...J-1], and see if A[I] and B[J] match or not
(if they match, the edit distance is not increased).

(Dynamic programming can also involve TRACEBACK, but to simply compute edit
distance this part of the algorithm is not needed.  See the file
needleman-wunsch.lisp, which presents a more complicated dynamic programming
problem, for a detailed discussion of TRACEBACK.)

|#



(defmacro tfn (x) `(the fixnum ,x))

(deftype edttype () `(simple-array (unsigned-byte 16) 2))
(defvar *edt* nil)


;;; Create the dynamic programming array, if we don't already have one
;;; lying around that is big enough.

(defun make-edit-distance-table (n m)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (let ((edt *edt*))
    (declare (type edttype edit))    
    (unless (and (arrayp edt)
                 (>= (array-dimension edt 0) n)
                 (>= (array-dimension edt 1) m))
      (setq *edt*
            (make-array (list n m) 
                        :element-type '(unsigned-byte 16)
                        :initial-element 0
                        )))))


;;; Fill in the dynamic programming array, using the initial conditions.

(defun edt-initial-conditions ()
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (let ((edt *edt*))
    (declare (type edttype edit))
    (dotimes (i (array-dimension edt 0)) (setf (aref edt i 0) i))
    (dotimes (j (array-dimension edt 1)) (setf (aref edt 0 j) j))
    ))


;;; Fill in the dynamic programming array, using the
;;; recurrence relationship.

(defun edt-recurrence (s1 s2 s1len s2len)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (declare (simple-string s1 s2))
  (declare (fixnum s1len s2len))
  (let ((edt *edt*))
    (declare (type edttype edt))
    (loop for i fixnum from 1 to s1len do
          (loop for j fixnum from 1 to s2len do
                (let ((prev-i (the fixnum (1- i)))
                      (prev-j (the fixnum (1- j))))
                  (declare (fixnum prev-i prev-j))
                  (setf (aref edt i j)
                        (min 
                         (tfn (1+ (aref edt prev-i j)))
                         (tfn (1+ (aref edt i prev-j)))
                         (tfn
                          (+ (aref edt prev-i prev-j)
                             (if (eql (schar s1 prev-i) (schar s2 prev-j)) 0 1)
                             )))))))))


;;; Show the resulting array.

(defun pp-edt (s1 s2)
  (terpri) (terpri)
  (format t "    ")
  (loop for ch across s2 do (format t "~A " ch)) (terpri)
  (loop for i from 0 to (length s1) do
        (if (zerop i)
            (format t "  ")
          (format t "~A " (aref s1 (1- i))))
        (loop for j from 0 to (length s2) do
              (format t "~A " (aref *edt* i j)))
        (terpri)
        )
  (terpri)
  )
                

;;; Main routine.

(defun compute-edit-distance (s1 s2)
  (declare (simple-string s1 s2))
  (let ((s1len (length s1)) (s2len (length s2)))
    (declare (fixnum s1len s2len))
    (make-edit-distance-table (1+ s1len) (1+ s2len))
    (edt-initial-conditions)
    (edt-recurrence s1 s2 s1len s2len)
    (aref *edt* s1len s2len)
    ))
        

              
(defun time-ced (slen n)
  (let ((s1 (make-string slen)) (s2 (make-string slen)))
    (loop for i from 0 to (1- slen) do
          (setf (aref s1 i) (code-char (+ #.(char-code #\a) (random 26))))
          (setf (aref s2 i) (code-char (+ #.(char-code #\a) (random 26)))))
    (time (dotimes (j n) (compute-edit-distance s1 s2)))
    ))
