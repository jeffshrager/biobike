;;;; -*- mode: Lisp; Syntax: Common-Lisp; Package: nvpl; -*-

(in-package :nvpl) 

;;; +=========================================================================+
;;; | Copyright (c) 2005 JP Massar, Jeff Elhai, Mark Slupesky, Peter Seibel   |
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

;;; Author: JP Massar. 

#||

A mechanism to define a set of infinite sets of unique IDs.
If there are N infinite set of unique IDs to be defined, 
then each set is of the form {init, init+N, init+2N ...}
where INIT is the argument START for the first infinite set, 
INIT+1 for the second infinite set, etc.  

This guarantees that all infinite sets contain unique elements
and that it is easy to figure out from an ID which infinite set 
it belongs to (using MOD).  

The function NEW-UNIQUE-ID produces a new ID given the infinite set
label.  The function UNIQUE-ID-TYPE gives the infinite set label
given the ID.  

The CLASS-KEYWORDS argument to the macro defines the infinite set labels.

||#

(defmacro define-unique-id-classes 
          ((&key (start 0) (limit most-positive-fixnum)) &rest class-keywords)
  (let ((n (length class-keywords)))
    `(progn 
       (defparameter *unique-id-classes* '(,@class-keywords))
       (defparameter *unique-id-increment* ,n)
       (eval-when (:compile-toplevel :load-toplevel :execute)
         (defun reset-unique-id-array () 
           (make-array 
            (list ,n) :initial-contents ',(ilist start (+ start n)))))
       #+allegro
       (defvar *unique-id-array-lock*
         (mp:make-process-lock :name "Unique id array lock"))
       (defvar *unique-id-array* (reset-unique-id-array))
       (defun reset-unique-ids () (reset-unique-id-array))
       (defun new-unique-id (type) 
         (with-lock (*unique-id-array-lock*)
           (let* ((index 
                   (ecase type
                     ,@(loop for key in class-keywords
                             for count from 0 
                             collect `(,key ,count)
                             )))
                  (next (aref *unique-id-array* index)))
             (when (< (- ,limit next) ,n)
               (error "Ran out of unique IDs; about to exceed limit ~D" 
                      ,limit))
             (incf (aref *unique-id-array* index) ,n)
             next
             )))
       (defun unique-id-type (id) 
         (when (>= id ,start) 
           (nth 
            (mod (- id ,start) *unique-id-increment*)
            *unique-id-classes*
            ))))))
         
(define-unique-id-classes 
 (:start 28)
 :snippet-id :template-id :menu-id :operator-id :data-id 
 :subsystem-id :organism-id)
