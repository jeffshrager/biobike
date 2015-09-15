;;; -*- mode: Lisp; Syntax: Common-Lisp; Package: nvpl; -*-

(in-package :nvpl)

;;; +=========================================================================+
;;; | Copyright (c) 2009 JP Massar                                            |
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

;; Author: JP Massar.


(defmethod process-single-vpl-return-value ((value t)) nil)

(defmethod process-single-vpl-return-value ((value wb::url))
  (show-vpl-popup-url-window 
   (wb::url-path value) 
   :relative-p 1
   :location "yes"
   :directories "yes"
   :status "yes"
   :menubar "yes"
   :width "inherit"
   :height "inherit"
   ))

(defmethod process-single-vpl-return-value ((value wb::svs))
  (show-vpl-popup-url-window
   (wb::create-url-from-svs value)
   :relative-p 1
   :location "yes"
   :directories "yes"
   :status "yes"
   :menubar "yes"
   :width "1000px"
   :height "inherit"
   ))

(defmethod process-single-vpl-return-value 
           ((value help::single-word-help-results))
  (create-and-use-unique-file 
   (user-temp-vpl-dir)
   (lambda (file p)
     (declare (ignore file))
     (with-html-to-stream p 
       (if (help::use-jhelp?)
           (help::display-single-word-jhelp-results value)  ;;JKM
         (progn
           (help::html-for-complete-help-listing)
           ))))
   (lambda (file) 
     (show-vpl-popup-url-window
      (wb::publish-path-for-file-in-viewable-subdir file)
      :relative-p 0
      :width "1000px" :height "800px"
      ))
   :name (s+ "value-" (string wb::*sessionid*))
   :type "html"
   ))

(defmethod process-single-vpl-return-value ((value wb::jpg))
  (create-vpl-popup-window-and-display 
   value :width "800px" :height "800px" :relative-p 0
   ))

(defmethod process-single-vpl-return-value ((value wb::function-for-html))
  (create-and-use-unique-file 
   (user-temp-vpl-dir)
   (lambda (file p)
     (declare (ignore file))
     (with-html-to-stream p 
       (apply (wb::function-for-html-f value) 
              (wb::function-for-html-args value))
       ))
   (lambda (file) 
     (show-vpl-popup-url-window
      (wb::publish-path-for-file-in-viewable-subdir file)
      :relative-p 0
      :width "800px" :height "800px"
      ))
   :name (s+ "help-" (string wb::*sessionid*))
   :type "html"
   ))

(defstruct (wb::data-editor-object (:constructor wb::make-data-editor-object))
  data)

(defun enter-obj-and-return-de-url (obj url-function-key)
  (let ((key 
         (forward-package-funcall 
          :de :enter-object-into-or-update-de-table obj
          )))
    (s+ "/" (forward-package-funcall :de url-function-key key))
    ))

(defmethod process-single-vpl-return-value ((value wb::data-editor-object))
  (show-vpl-popup-url-window 
   (let ((obj (data-editor-object-data value)))
     (typecase obj
       (frames::%frame 
        (wb::make-weblistener-frames-editor-url 
         :name (#^fname obj)
         :noshow "fname"
         ))
       ((or cons vector hash-table)
        (enter-obj-and-return-de-url obj :simple-sequence-editor-url))
       (array
        (enter-obj-and-return-de-url obj :simple-array-editor-url))
       (utils::garray
        (case (garray-rank obj)
          (1
           (enter-obj-and-return-de-url obj :simple-sequence-editor-url))
          (otherwise 
           (enter-obj-and-return-de-url obj :simple-array-editor-url)
           )))
       (otherwise 
        (enter-obj-and-return-de-url obj :simple-object-editor-url))
       ))
   :width "900px"
   :height "1000px"
   :location "yes"
   :menubar "yes"
   ))

(bbi::define-function bbi::edit-gene 
  summary "Brings up the frame editor for the gene"
  required gene-name 
  body 
  (wb::make-data-editor-object 
   :data
   (cond
    ((stringp gene-name) (frames::frame-fnamed gene-name))
    ((frames::isframe? gene-name) gene-name)
    (t (error "Bad argument to edit-gene"))
    )))

(defun de-edit-object? (obj)
  (typecase obj
    ((or number character) 
     (values nil "Atomic objects cannot be edited!"))
    ((or string symbol stream package) 
     (values 
      nil
      (formatn "Don't know how to edit an object of type ~A yet!" 
               (if (stringp obj) "String" (type-of obj))
               )))
    ((or cons vector hash-table frames::%frame) t)
    (utils::garray 
     (case (garray-rank obj)
       ((1 2) t)
       (otherwise 
        (values nil "Cannot edit a table of more than 2 dimensions yet!")
        )))
    (array 
     (if (> (array-rank obj) 2)
         (values nil "Cannot edit an array of more than 2 dimensions yet!")
       t
       ))
    (otherwise
     (let ((value-type (type-of obj)))
       (cond
        ;; structures specially set up to be able to be viewed by
        ;; data editor 
        ((and (symbolp value-type) (get value-type :object-slots)) t)
        ;; clos objects 
        ((eq 'standard-class (type-of (class-of obj))) t)
        (t 
         (values 
          nil
          (formatn 
           "Don't know how to edit an object of type ~A yet!" value-type
           ))))))))

#||

  (typecase obj
    ((or number character) (formatt "Atomic objects cannot be edited!") nil)
    ((or string symbol stream package) 
     (formatt "Don't know how to edit an object of type ~A yet!" 
              (if (stringp obj) "String" (type-of obj)))
     nil)
    ((or cons vector hash-table frames::%frame)
     (wb::make-data-editor-object :data obj))
    (utils::garray 
     (case (garray-rank obj)
       ((1 2) (wb::make-data-editor-object :data obj))
       (otherwise 
        (formatt "Cannot edit a table of more than 2 dimensions yet!")
        nil
        )))
    (array 
     (if (> (array-rank obj) 2)
         (progn 
           (formatt "Cannot edit an array of more than 2 dimensions yet!")
           nil)
       (wb::make-data-editor-object :data obj)
       ))
    (otherwise (wb::make-data-editor-object :data obj))
    ))

||#



     
                                
            