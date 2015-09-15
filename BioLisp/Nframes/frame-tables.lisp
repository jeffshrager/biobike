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

;;; Authors:  Mike Travers, JP Massar.


;;;; BASICS FOR FRAMES TABLE


;;; By default create a small frame table.  Application initialization code 
;;; can call RESIZE-FRAME-TABLE as appropriate before creating shitloads 
;;; of frames.

(defparameter *default-frame-table-size* 800)

;;; The actual frames table will be created in bootstrap.lisp

(defvar *frame-table* nil)

;;; The frames created by the frames system itself.

(defvar *frames-system-frames* nil)

(defparameter *standard-frames-map-name* "standard-frames-map")

;;; use an equalp hash table for case-insensitivity.

(defun create-a-frame-table
       (&key
        (size *default-frame-table-size*)
        (name *standard-frames-map-name*)
        )
  (declare (ignorable size name))
  (if user::*acache-running* 
      (forward-funcall 
       'open-map name :if-does-not-exist :create :if-exists :open)
    (vif (existing-table (get (keywordize name) :frame-table))
         existing-table
         (let ((new-table (make-string-equal-hash-table :size size)))
           (setf (get (keywordize name) :frame-table) new-table)
           new-table
           ))))


(defparameter *max-buffer-size* 30) 
(defparameter *frame-name-canonicalization-buffers*
  (let ((a (make-array (list (1+ *max-buffer-size*)))))
    (loop for j from 0 below (1+ *max-buffer-size*) do
          (setf (aref a j) (make-string j :initial-element #\Space))
          finally (return a)
          )))
          

(defmethod %string->frame ((frame-table ac-map) string)
  (declare (simple-string string))
  (let ((fname (%string->acache-fname string)))
    (values (map-value frame-table fname) fname)
    ))

(defmethod %string->frame ((frame-table hash-table) string)
  (values (gethash string frame-table) string))


(defun %string->acache-fname (string)
  (declare (simple-string string))
  #.(optimization-declaration)
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
  (if user::*acache-running* (%string->acache-fname string) string))

(defun %string->fname-same? (string) 
  (let ((fname (%string->fname string)))
    (values fname (string= fname string))))

(defmethod frame-table-value ((frame-table ac-map) name-key) 
  (map-value frame-table name-key))

(defmethod frame-table-value ((frame-table hash-table) name-key)
  (gethash name-key frame-table))


(defmethod %set-frame-table-value 
           ((frame-table ac-map) name-key frame-value)
  (setf (map-value frame-table name-key) frame-value))

(defmethod %set-frame-table-value 
           ((frame-table hash-table) name-key frame-value)
  (setf (gethash name-key frame-table) frame-value))


(defmethod %add-frame-table-value 
           ((frame-table ac-map) name-key value &key (test 'equal))
  (pushnew value (map-value frame-table name-key) :test test))

(defmethod %add-frame-table-value 
           ((frame-table hash-table) name-key value &key (test 'equal))
  (pushnew value (gethash name-key frame-table) :test  test))


(defmethod %remhash-frame-table-value 
           ((frame-table ac-map) name-key)
  (warn "Cannot remove entries from map yet."))
   
(defmethod %remhash-frame-table-value 
           ((frame-table hash-table) name-key) 
  (remhash name-key frame-table))


(defmethod %expunge-frame-table-entry 
           ((frame-table ac-map) name-key)
  (remhash name-key frame-table))

(defmethod %expunge-frame-table-entry 
           ((frame-table hash-table) name-key)
  (remhash name-key frame-table))


(defun %intern-frame (frame-table frame name)
  (%set-frame-table-value frame-table name frame))

(defun %unintern-frame (frame-table fname)
  (%remhash-frame-table-value frame-table fname))

    




 

  
