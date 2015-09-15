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

(defun pretty-up-type (type) 
  (cond
   ((listp type) 
    (let ((header (first type)))
      (cond
       ((or (eq header 'lisp:array) (eq header 'lisp:simple-array))
        (let ((element-type (second type)))
          (cond
           ((eq element-type 'lisp:character) 'string)
           (t type)
           )))
       (t type)
       )))
   (t type)
   ))
          
(defun looks-like-url? (path)
  (and (> (length path) #.(length "http://"))
       (let ((pos (search "http://" path)))
         (and pos (zerop pos))
         )))
  
(defun create-vpl-popup-window-and-display 
       (object &key (type nil) (width "800px") (height "800px") (relative-p 0))
  (typecase object
    (string
     (case type 
       ((:html-file :text-file) 
        (show-vpl-popup-URL-window
         (wb::publish-path-for-file-in-viewable-subdir object)
         :relative-p relative-p
         :width width :height height
         ))
       (:image-file 
        (create-and-use-unique-file 
         (user-temp-vpl-dir)
         (lambda (file p)
           (declare (ignore file))
           (with-html-to-stream p 
             (html ((:img :src object)))
             ))
         (lambda (file) 
           (show-vpl-popup-URL-window
            (wb::publish-path-for-file-in-viewable-subdir file)
            :relative-p relative-p
            :width width :height height
            ))
         :name (s+ "image" (string wb::*sessionid*))
         :type "html"
         ))
       (:url 
        (show-vpl-popup-URL-window
         object
         :relative-p relative-p
         :width width :height height
         ))))
    (wb::jpg 
     (let ((path (wb::jpg-path object)))
       (cond 
        ((looks-like-url? path)
         (create-and-use-unique-file 
          (user-temp-vpl-dir)
          (lambda (file p)
            (declare (ignore file))
            (with-html-to-stream p 
              (html ((:img :src path)))
              ))
          (lambda (file) 
            (show-vpl-popup-URL-window
             (wb::publish-path-for-file-in-viewable-subdir file)
             :relative-p relative-p
             :width width :height height :menubar "yes"
             ))
          :name (s+ "jpg-" (string wb::*sessionid*))
          :type "html"
          ))
        (t 
         (show-vpl-popup-URL-window
          (wb::publish-path-for-file-in-viewable-subdir path)
          :relative-p relative-p
          :width width :height height
          )))))
    (wb::url 
     (show-vpl-popup-URL-window
      (wb::url-path object)
      :relative-p relative-p
      :width width :height height
      ))
    (otherwise 
     (error "Don't know how to display object of type ~A" (type-of object))
     )))


;; Doesn't really belong here but can't be put anywhere before 
;; the BBL package is defined 
(defun execute-function-in-bbl-context (f &rest args)
  (bbi::with-bbl-form (apply f args)))




