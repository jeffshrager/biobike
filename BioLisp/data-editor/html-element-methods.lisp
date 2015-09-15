 ;;; -*- Package: data-editor; mode: lisp; base: 10; Syntax: Common-Lisp; -*-

(in-package :data-editor)

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

;;; Author: JP Massar


(defun html-for-non-linked-display-element 
       (ds tts pbs &key (visible-class "visible-any"))
  (cond
   ((and (null tts) (null pbs)) (html (:princ-safe ds)))
   ((null pbs) (html ((:a :title tts) (html (:princ-safe ds)))))
   ((null tts)
    (html 
     :newline
     ((:div :class visible-class)
      :newline
      (:princ-safe ds)
      :newline
      ((:div :class "hidden-string-box")
       :newline
       ((:div :class *hidden-box-class*
         :style (formatn "height: ~Dpx;" (popup-height (length pbs))))
        :newline
        (let ((lines (string-split pbs #\Newline)))
          (loop for line in lines do (html (:princ-safe line) :br))))
       :newline
       ))))
   (t (error "Shouldn't get here!"))
   ))

(defun html-for-linked-display-element
       (ds tts pbs url url-class &key (visible-class "visible-any"))
  (flet ((frame-url-html (&optional title)
           (if title 
               (html
                ((:a :href url :class url-class :title title)
                 (:princ-safe ds)
                 ))
             (html
              ((:a :href url :class url-class)
               (:princ-safe ds)
               )))))
    (cond
     ((and (null tts) (null pbs)) (frame-url-html))
     ((null pbs) (frame-url-html tts))
     ((null tts)
      (html 
       :newline
       ((:div :class visible-class)
        :newline
        (frame-url-html)
        :newline
        ((:div :class "hidden-string-box")
         :newline
         ((:div :class *hidden-box-class*
           :style (formatn "height: ~Dpx;" (popup-height (length pbs))))
          :newline
          (let ((lines (string-split pbs #\Newline)))
            (loop for line in lines do (html (:princ-safe line) :br))))
         :newline
         )))))))

(defun html-for-generic-display-object (value)
  (multiple-value-bind (ds tts pbs)
      (limited-display-strings value *data-editor-slot-value-length*)
    (html-for-non-linked-display-element ds tts pbs)
    ))

(defun html-for-clos-object (value)
  (multiple-value-bind (ds tts pbs)
      (limited-display-strings value *data-editor-slot-value-length*)
    (let ((key (enter-object-into-or-update-de-table value)))
      (html-for-linked-display-element 
       ds tts pbs 
       (component-url-generator 'simple-object-editor-url key)
       "visiblesequence"
       ))))

(defun component-url-generator (function &rest args)
  (let ((*parent-key* *key-for-this-page*)) (apply function args)))

;;; Generic function to display slot values.

(defgeneric html-for-display-element (value))

(defmethod html-for-display-element ((value t))
  (let ((value-type (type-of value)))
    (cond 
     ((and (symbolp value-type) (get value-type :object-slots))
      (html-for-structure-or-class-display-element value))
     ((eq 'standard-class (type-of (class-of value)))
      (html-for-clos-object value))
     (t (html-for-generic-display-object value))
     )))

(defmethod html-for-display-element ((value null))
  ;; added tooltip instead of just printing out '-'
  (html-for-generic-display-object value)
  ;; (html (:princ-safe "-"))
  )

(defgeneric html-for-edit-frame-slot-value (value &key &allow-other-keys))

(defmethod html-for-display-element ((value symbol))
  (html-for-generic-display-object value))

(defmethod html-for-display-element ((value %frame))
  (multiple-value-bind (ds tts pbs)
      (limited-display-strings value *data-editor-slot-value-length*)
    (html-for-linked-display-element 
     ds tts pbs 
     (component-url-generator 'simple-frame-editor-url value)
     "frame-value"
     )))

(defmethod html-for-display-element ((value package))
  (html-for-generic-display-object value))

(defmethod html-for-display-element ((value character))
  (html-for-generic-display-object value))

(defmethod html-for-display-element ((value string))
 (LET* ((http-found (SEARCH "http://" value :TEST 'EQUALP))
        (space-found (SEARCH " " value))
        (external-site? (AND http-found (= http-found 0)
             (NOT space-found)))
        )
  (IF external-site?
      (multiple-value-bind (ds tts pbs)
        (limited-display-strings value *data-editor-slot-value-length*)
        (html-for-linked-display-element ds tts pbs value
          "visible-string"))
      (multiple-value-bind (ds tts pbs)
        (limited-display-strings value *data-editor-slot-value-length*)
        ;; make null strings visible and able to have a tooltip
        (when (nullstring? ds) (setq ds "\"\""))
        (html-for-non-linked-display-element 
           ds tts pbs :visible-class "visible-string")
        )
      )
))

(defmethod html-for-display-element ((value number))
  (html-for-generic-display-object value))

(defmethod html-for-display-element ((value cons))
  (let ((key (enter-object-into-or-update-de-table value)))
    (multiple-value-bind (ds tts pbs)
        (limited-display-strings value *data-editor-slot-value-length*)
      (html-for-linked-display-element 
       ds tts pbs
       (component-url-generator 'simple-sequence-editor-url key)
       "visiblesequence"
       ))))

(defmethod html-for-display-element ((value hash-table))
  (if (zerop (hash-table-count value))
      (html-for-non-linked-display-element
       "#<Empty Hash Table>" 
       (formatn 
        "An empty (no keys or values) hash table using ~A"
       (hash-table-test value))
       nil)
    (let ((key (enter-object-into-or-update-de-table value)))
      (multiple-value-bind (ds tts pbs)
          (limited-display-strings value *data-editor-slot-value-length*)
        (html-for-linked-display-element 
         ds tts pbs 
         (component-url-generator 'simple-sequence-editor-url key)
         "visiblesequence"
         )))))

(defmethod html-for-display-element ((value vector))
  (if (zerop (length value))
      (html-for-non-linked-display-element
       "#()" "A null (zero-length) vector" nil)
    (let ((key (enter-object-into-or-update-de-table value)))
      (multiple-value-bind (ds tts pbs)
          (limited-display-strings value *data-editor-slot-value-length*)
        (html-for-linked-display-element 
         ds tts pbs
         (component-url-generator 'simple-sequence-editor-url key)
         "visiblesequence"
         )))))

(defmethod html-for-display-element ((value utils::garray))
  (case (garray-rank value)
    (1 
     (let ((n (garray-current-total-size value)))
       (multiple-value-bind (ds tts pbs)
           (limited-display-strings value *data-editor-slot-value-length*)
         (if (zerop n)
             (html-for-non-linked-display-element 
              ds tts (formatn "Contents: None~%~A" pbs))
           (let ((key (enter-object-into-or-update-de-table value)))
             (html-for-linked-display-element
              ds tts 
              (formatn "Contents: ~D element~P~%~A" n n pbs)
              (component-url-generator 'simple-sequence-editor-url key)
              "visiblesequence"
              ))))))
    (2
     (let ((n (garray-current-total-size value)))
       (multiple-value-bind (ds tts pbs)
           (limited-display-strings value *data-editor-slot-value-length*)
         (if (zerop n)
             (html-for-non-linked-display-element 
              ds tts (formatn "Contents: None~%~A" pbs))
           (let ((key (enter-object-into-or-update-de-table value)))
             (html-for-linked-display-element
              ds tts 
              (formatn "Contents: ~D element~P~%~A" n n pbs)
              (component-url-generator 'simple-array-editor-url key)
              "visiblesequence"
              ))))))
    (otherwise (html-for-generic-display-object value))
    ))

(defmethod html-for-display-element ((value array))
  (case (array-rank value)
    (2
     (let ((n (array-total-size value)))
       (multiple-value-bind (ds tts pbs)
           (limited-display-strings value *data-editor-slot-value-length*)
         (if (zerop n)
             (html-for-non-linked-display-element 
              ds tts (formatn "Contents: None~%~A" pbs))
           (let ((key (enter-object-into-or-update-de-table value)))
             (html-for-linked-display-element
              ds tts pbs
              (component-url-generator 'simple-array-editor-url key)
              "visiblesequence"
              ))))))
    (otherwise (html-for-generic-display-object value))
    ))
     
(defun html-for-structure-or-class-display-element (value)
  (let ((key (enter-object-into-or-update-de-table value)))
    (multiple-value-bind (ds tts pbs)
        (limited-display-strings value *data-editor-slot-value-length*)
      (html-for-linked-display-element
       ds tts pbs
       (component-url-generator 'simple-object-editor-url key)
       "visiblesequence"
       ))))

