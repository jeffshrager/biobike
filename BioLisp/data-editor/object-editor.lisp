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


(defparameter *object-editor-url* "object-editor")
(defvar *object-type* nil)
(defvar *object-accessors* nil)
(defvar *object-setters* nil)
(defvar *object-slots* nil)
(defvar *slot-index* nil)
(defvar *clos-object?* nil)

(defun simple-object-editor-url (key)
  (if *parent-key* 
      (formatn "~A?pkg=~A&objectkey=~A&pkey=~D" 
               *object-editor-url* wb::*sessionid* key *parent-key*)
    (formatn "~A?pkg=~A&objectkey=~A" *object-editor-url* wb::*sessionid* key)
    )
  )
    

(defun object-editor-url-for-editing-slot (slot-key)
  (s+ 
   (simple-object-editor-url *object-key*)
   (formatn "&seb=1")
   (formatn "&editedslot=~D" slot-key)
   ))

(publish 
 :path (s+ "/" *object-editor-url*)
 :content-type cl-user::*html-publish-content-type*
 :function
 (lambda (req ent) 
   (let* ((input (request-query req))
          (key (url-parameter-value :objectkey input))
          (*object-key* (parse-integer key))
          (object (object-from-unique-id *object-key*)))
     (if (null object) 
         (wb::with-http-response-and-body (req ent)
           (html
            (:big
             (:b
              (:princ-safe
               (formatn
                (one-string-nl
                 "The object you requested is no longer in the data editor's"
                 "object cache.  Objects you view vanish after ~D minutes.")
                *data-editor-cache-duration*
                ))))))
       (with-parent-key-bindings
         (object-editor-page req ent input)
         ))
     (purge-de-table (* 60 *data-editor-cache-duration*))
     (purge-pid-table (* 60 *data-editor-cache-duration*))
     )))


(defun object-editor-page (req ent input)
  (let* ((package-name (url-parameter-value :pkg input))
         (package-symbol (keywordize package-name))
         ;; is this request coming from the edit box SUBMIT button
         ;; or the edit box CANCEL button?
         (editsubmit (url-parameter-value :editsubmit input))
         (editcancel (url-parameter-value :editcancel input))
         ;; index of slot that is being edited
         (slotindex (content? (url-parameter-value :editedslot input)))
         )
    ;; (setq *input* input)
    (wb::execute-with-standard-weblistener-environment
     req ent package-symbol
     (lambda () 
       (log-data-editor-use "Object editor on key: ~A~%" *object-key*)
       (let* ((*object-type* (type-of *edit-object*))
              (*clos-object?* 
               (eq 'standard-class (type-of (class-of *edit-object*))))
              (*object-slots* (de-object-slots))
              (*object-accessors* (de-object-accessors))
              (*object-setters* (de-object-setters))
              (*slot-index* (when slotindex (parse-integer slotindex)))
              (*display-edit-box?*
               (and slotindex (null editsubmit) (null editcancel))
               ))
         (html 
          ;; enable modern CSS processing
          (:princ *transitional-html-doctype-header*)
          (:html
           ;; HTML head (which includes loading the CSS and doing <title>)
           (html-for-data-editor-head *object-editor-css-file* "Object Editor")
           (html
            :newline
            (:body 
             ((:div :id "everything")
              ;; "Data Frame <name>" at top
              (html-for-edit-object-title)
              (when editsubmit 
                (unless slotindex (error "internal error!"))
                (handle-edit-object-submit input))
              (html-for-edit-object)
              ))))))))))

(defun de-object-slots ()
  (if *clos-object?* 
      (let ((slots-and-values (utils:slot-names-and-slot-values *edit-object*)))
        (mapcar 'first slots-and-values))
    (get *object-type* :object-slots)
    ))

(defun de-object-accessors ()
  (if *clos-object?* 
      (mapcar 
       (lambda (slot-name) (lambda (x) (slot-value x slot-name)))
       *object-slots*
       )
    (get *object-type* :object-accessors)
    ))

(defun de-object-setters ()
  (if *clos-object?* 
      (mapcar 
       (lambda (slot-name) (lambda (x v) (setf (slot-value x slot-name) v)))
       *object-slots*
       )
    (get *object-type* :object-setters)
    ))
   
;;; Get the (possibly new) value the user entered
;;; for the edit box, covert it appropriately to a Lisp object,
;;; and insert it into the corresponding frame slot.

(defun handle-edit-object-submit (input)
  (let ((newdata (content? (url-parameter-value :editboxcontents input nil)))
        (interpretation (url-parameter-value :interpretation input))
        (setter-function (nth *slot-index* *object-setters*))
        )
    (when newdata 
      (cond
       ((string-equal interpretation "String") 
        (funcall setter-function *edit-object* newdata))
       ((string-equal interpretation "Form")
        (funcall 
         setter-function *edit-object* (read-from-string-or-error-text newdata))
        )))))

;;; Top panel, currently just displaying type of object being edited

(defun html-for-edit-object-title ()
  (html 
   ((:div :id "titleframe") 
    ((:div :id "titletext")
     ((:h2 :style "text-align: center;")
      (:princ-safe 
       (formatn 
        "~AObject of type ~A" (if *clos-object?* "CLOS " "") 
        (string-capitalize *object-type*))
       ))
     ((:div :class "frame-controls")
      (html-for-frame-editor-controls)
      )
     ))))

(defun html-for-edit-object ()
  (html 
   :newline
   ((:div :id "frames-and-edit-boxes")
    (let* ((nslots (length *object-slots*))
           (nrows 
            (if (>= nslots *frame-editor-column-split*) 
                (ceiling nslots 2) 
              nslots))
           (classes 
            (if (null *display-edit-box?*)
                (list "fsize4" "esize4")
              (nrows->classes nrows)
              )))
        ;; top panel
        (html-for-edit-object-slot-table nrows (first classes))
        ;; bottom panel
        (when *display-edit-box?*
          (html
           ((:div :id "editboxes" :class (second classes))
            ((:form :method "get" :action "object-editor")
             (html-for-edit-object-edit-slot)
             ))))))))


;;; Display the frame slot names and the frame slot values in 
;;; a two-column format (one column if only a few slots), 
;;; with special handling of various types of slot values

(defun html-for-edit-object-slot-table (nrows fclass)
  (labels ((html-for-slot-name (count slot-name)
             (html 
              ((:a :class "slotname-text" 
                :href (object-editor-url-for-editing-slot count))
               (:princ-safe slot-name) ":"
               )))
           (emit-object-slot-name-td (count slot-name)
             (html 
              ((:td :class "slotname") (html-for-slot-name count slot-name))
              :newline
              ))
           (emit-slot-value-td (td-class value value-class)
             (let ((*hidden-box-class* value-class))
               (html
                ((:td :class td-class)
                 (html-for-display-element value))
                :newline
                ))))
    (html
     :newline
     ((:div :id "frametable" :class fclass)
      :newline
      ((:table :border 0 :cellspacing 0 :cellpadding 0)
       :newline
       ;; single column
       (if (= nrows (length *object-slots*))
           (loop
            for slot in *object-slots*
            for accessor in *object-accessors*
            for count from 0
            as slot-name = 
            (ellipsis-string (string slot) *frame-editor-slot-name-length*)
            do 
            (html
             (:tr
              (emit-object-slot-name-td count slot-name)
              (emit-slot-value-td 
               "slotvalue" 
               (object-slot-value-or-unbound accessor)
               "hidden-string-left"
               ))))
         ;; two column format
         (loop
          for slots on *object-slots* by #'cddr
          for accessors on *object-accessors* by #'cddr
          for count from 0 by 2
          as slot1 = (first slots)
          as slot2 = (second slots)
          as accessor1 = (first accessors)
          as accessor2 = (second accessors)
          as slot-name1 = (limited-slot-name slot1)
          as slot-name2 = (and slot2 (limited-slot-name slot2))
          as value1 = (object-slot-value-or-unbound accessor1)
          as value2 = (and slot2 (object-slot-value-or-unbound accessor2))
          do 
          (html
           (:tr
            (emit-object-slot-name-td count slot-name1) 
            (emit-slot-value-td "slotvalue" value1 "hidden-string-left")
            (when slot2 
              (emit-object-slot-name-td (1+ count) slot-name2)
              (emit-slot-value-td "slotvalue" value2 "hidden-string-right")
              ))))))
      :newline :br
      ))))

(defun object-slot-value-or-unbound (accessor)
  (handler-case (funcall accessor *edit-object*)
    (unbound-slot () "*** <Unbound> ***")
    (error 
     (c) 
     (error "Internal error.  This should not happen!  Actual error: ~A" c)
     )))


;;; Retransmit the basic state information via the form submit
;;; using hidden values.
(defun html-for-object-editor-hidden-variables ()
  (html
   (hidden "PKG" (string wb::*sessionid*))
   :newline
   (hidden "objectkey" *object-key*)
   :newline
   (when *parent-key* 
     (hidden "pkey" (formatn "~D" *parent-key*)))
   :newline
   ))


;;; Display the edit box, along with SUBMIT and CANCEL buttons and
;;; radio buttons for the box to select how what the user types in is
;;; to be interpreted for the box.


(defun html-for-edit-object-edit-slot ()
  (html 
   (html-for-object-editor-hidden-variables)
   :br
   :newline
   (hidden "editedslot" *slot-index*)
   (let ((val 
          (if (slot-boundp *edit-object* (nth *slot-index* *object-slots*))
              (funcall 
               (nth *slot-index* *object-accessors*)
               *edit-object*
               )
            nil
            )))
     (html 
      :newline
      ((:div :class "editbox")
       :newline
       ((:div :class "editbox-label") 
        (html-for-object-element-label)
        (html-for-edit-box-controls val)
        )
       (html-for-edit-box val)
       )))
   ;; workaround for " " --> "+" bug as last parameter
   (hidden "dummy" "dummy")
   ))
          
(defun html-for-object-element-label ()
  (let ((slotname (string (nth *slot-index* *object-slots*))))
    (html 
     (:b 
      (:princ-safe 
       (formatn
        "The ~A slot of ~A ~A" 
        slotname (if (vowel? (char slotname 0)) "an" "a") *object-type*
        ))))))









