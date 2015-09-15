;; -*- Package: aframes; mode: lisp; base: 10; Syntax: Common-Lisp; -*-

(in-package :aframes)

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

;;; Author: JP Massar.

(defconstant *max-buffer-size* 40) 
(defparameter *frame-name-canonicalization-buffers*
  (let ((a (make-array (list (1+ *max-buffer-size*)))))
    (loop for j from 0 below (1+ *max-buffer-size*) do
          (setf (aref a j) (make-string j :initial-element #\Space))
          finally (return a)
          ))
  "Canonicalization buffers for conversion of frame names without consing")


(defgeneric %string->frame (map string)
  (:documentation 
   "Return canonicalized form of STRING (downcased) for frame system usage."))

(defmethod %string->frame ((frame-table ac-map) string)
  (declare (simple-string string))
  (let ((fname (%string->acache-fname string)))
    (values (map-value frame-table fname) fname)
    ))

(defmethod %string->frame ((frame-table hash-table) string)
  (values (gethash string frame-table) string))


(defun %string->acache-fname (string)
  (declare (simple-string string))
  #.(utils:optimization-declaration)
  (let* ((slen (length string))
         (conversion-buffer
          (if (<= slen (the fixnum *max-buffer-size*))
              (svref *frame-name-canonicalization-buffers* slen)
            (make-string slen)
            )))
    (declare (fixnum slen) (simple-string conversion-buffer))
    (loop for j fixnum below slen do
          (setf (schar conversion-buffer j) 
                (char-downcase (schar string j))))
    conversion-buffer
    ))

(defun %string->fname (string)
   "Return canonicalized form of STRING for frame system usage."
  (if user::*acache-running* (%string->acache-fname string) string))

(defun %substring->fname (string start end)
  (declare (simple-string string))
  (declare (fixnum start end))
  #.(utils:optimization-declaration)
  (let* ((sslen (the fixnum (- end start)))
         (conversion-buffer
          (if (<= sslen *max-buffer-size*)
              (svref *frame-name-canonicalization-buffers* sslen)
            (make-string sslen)
            )))
    (declare (fixnum sslen) (simple-string conversion-buffer))
    (if user::*acache-running*
        (loop for j fixnum below sslen
              for i fixnum from start below end do
              (setf (schar conversion-buffer j)
                    (char-downcase (schar string i))
                    ))
      (loop for j fixnum below sslen
            for i fixnum from start below end do
            (setf (schar conversion-buffer j) (schar string i))
            ))
    conversion-buffer
    ))

(defun %string->fname-same? (string) 
  (let ((fname (%string->fname string)))
    (values fname (string= fname string))))

