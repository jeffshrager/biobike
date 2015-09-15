;;; -*- Package: bioutils; mode: lisp; base: 10; Syntax: Common-Lisp; -*-

(in-package :bioutils)

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

(defstruct fasta-iter stream next-line header-fp body-fp return-body?)

(defmethod iter-init ((obj fasta-iter))
  (loop
   with s = (fasta-iter-stream obj)
   with state = :initial
   ;; These next two must remain in order
   as fp = (file-position s)
   as ch = (read-char s nil nil)
   do
   (cond 
    ((null ch) (error "No records found in fasta stream!"))
    ((eql ch #\Newline) (setq state :newline))
    ((eql ch #\>)
     (if (or (eq state :initial) (eq state :newline))
         (progn
           (setf (fasta-iter-header-fp obj) fp)
           (let ((header (read-line s nil nil)))
             (when (null header) (error "No content after initial '>' !"))
             (setf (fasta-iter-next-line obj) (s+ ">" header))
             (setf (fasta-iter-body-fp obj) (file-position s))
             (return obj)
             ))
       (setq state :header)
       ))
    (t (setq state :header))
    )))

(defmethod iter-next? ((obj fasta-iter)) 
  (not (null (fasta-iter-next-line obj))))

(defmethod iter-next ((obj fasta-iter)) 
  (let ((body-lines nil)
        (header (fasta-iter-next-line obj))
        (header-fp (fasta-iter-header-fp obj))
        (body-fp (fasta-iter-body-fp obj))
        (return-body? (fasta-iter-return-body? obj)))
    (loop 
     with s = (fasta-iter-stream obj)
     as fp = (file-position s)
     as line = (read-line s nil nil)
     do
     (cond
      ((null line) 
       (setf (fasta-iter-next-line obj) nil)
       (return))
      ((or (= (length line) 0) (not (char= #\> (char line 0))))
       (when return-body? (push line body-lines)))
      (t 
       (setf (fasta-iter-header-fp obj) fp)
       (setf (fasta-iter-body-fp obj) (file-position s))
       (setf (fasta-iter-next-line obj) line)
       (return)
       )))
    (list header (nreverse body-lines) header-fp body-fp)
    ))
  

(defun fasta-headers (file) 
  (with-open-file (p file :direction :input)
    (let ((fasta-iter (make-fasta-iter :stream p :return-body? nil)))
      (xloop for (header nil nil nil) in fasta-iter do 
             (cformatt "~A" header)
             ))))

(defun fasta-index-info 
       (file &optional (header-hash-value (lambda (x) (subseq x 1))))
  (with-open-file (p file :direction :input)
    (let ((fasta-iter (make-fasta-iter :stream p :return-body? nil)))
      (xloop for (header nil header-fp body-fp) in fasta-iter 
             collect
             (list (funcall header-hash-value header) header-fp body-fp)
             ))))
             
           
(defun fasta-data (file)
  (with-open-file (p file :direction :input)
    (let ((fasta-iter (make-fasta-iter :stream p :return-body? t)))
      (xloop for (header body-lines nil nil) in fasta-iter
             collect
             (list
              (subseq (first (string-split header)) 1)
              (string-join body-lines "")
              )))))