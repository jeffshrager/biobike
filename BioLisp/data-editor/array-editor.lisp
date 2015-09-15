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


(defvar *array-object-first-row-index* nil)
(defvar *array-object-first-col-index* nil)
(defvar *array-object-last-row-index* nil)
(defvar *array-object-last-col-index* nil)
(defvar *array-object-nrows* nil)
(defvar *array-object-ncols* nil)
(defvar *garray-second-axis-hash-keys-if-same* nil)
(defvar *garray-enum-axes?* nil)
(defvar *garray-enum-axis1-possibles* nil)
(defvar *garray-enum-axis2-possibles* nil)

(defvar *edit-element-key1* nil)
(defvar *edit-element-key2* nil)
(defvar *array-edit-element-type1* nil)
(defvar *array-edit-element-type2* nil)

(defvar *default-rows* 40)
(defvar *default-cols* 5)

(defvar *array-object-row-extent* nil)
(defvar *array-object-col-extent* nil)
(defvar *array-start-row-index* nil)
(defvar *array-start-col-index* nil)
(defvar *array-end-row-index* nil)
(defvar *array-end-col-index* nil)

(publish 
 :path (s+ "/" *array-editor-url*)
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
         (array-editor-page req ent input)
         ))
     (purge-de-table (* 60 *data-editor-cache-duration*))
     (purge-pid-table (* 60 *data-editor-cache-duration*))
     )))

(defun array-editor-page (req ent input)
  (let* ((package-name (url-parameter-value :pkg input))
         (package-symbol (keywordize package-name))
         ;; row index of element to be edited
         (key1 (content? (url-parameter-value :key1 input)))
         ;; column index of element to be edited
         (key2 (content? (url-parameter-value :key2 input)))
         ;; are the indices a string, a form, or a form to be evaluated?
         ;; by default, they are forms which we do a read-from-string on 
         (type1 (content? (url-parameter-value :key1type input)))
         (type2 (content? (url-parameter-value :key2type input)))
         ;; is this request coming from the edit box SUBMIT button
         ;; or the edit box CANCEL button?
         (editsubmit (url-parameter-value :editsubmit input))
         (editcancel (url-parameter-value :editcancel input))
         ;; what part of the 2d array/gtable to display
         (startrow (content? (url-parameter-value :startrow input)))
         (startcol (content? (url-parameter-value :startcol input)))
         (nrows (content? (url-parameter-value :nrows input)))
         (ncols (content? (url-parameter-value :ncols input)))
         )
    (wb::execute-with-standard-weblistener-environment
     req ent package-symbol
     (lambda () 
       ;; (setq *input* input)
       (let* ((*user-base* (if (wb::bbl-mode?) 1 0))
              (*object-display-type* (array-display-type))
              (*garray-second-axis-hash-keys-if-same*
               (case *object-display-type*
                 ((:garray-sequence-hash :garray-hash-hash)
                  (garray-second-axis-hashes-have-same-keys? *edit-object*))
                 (otherwise nil)
                 ))
              (*garray-enum-axes?* 
               (case *object-display-type*
                 (:lisp-array (list nil nil))
                 (otherwise 
                  (mapcar 
                   (lambda (axis) (typep axis 'utils::enum-axis))
                   (utils::garray-axes *edit-object*)
                   ))))
              (*garray-enum-axis1-possibles* 
               (when (first *garray-enum-axes?*)
                 (garray-enum-axis-possibles *edit-object* 0)))
              (*garray-enum-axis2-possibles* 
               (when (second *garray-enum-axes?*)
                 (garray-enum-axis-possibles *edit-object* 1)))
              (*array-object-first-row-index* (array-first-index 0))
              (*array-object-first-col-index* (array-first-index 1))
              (*array-object-last-row-index* (array-last-index 0))
              (*array-object-last-col-index* (array-last-index 1))
              (*array-object-row-extent* 
               (1+ (- *array-object-last-row-index* 
                      *array-object-first-row-index*
                      )))
              (*array-object-col-extent* 
               (1+ (- *array-object-last-col-index* 
                      *array-object-first-col-index*
                      )))
              (*edit-element-key1* key1)
              (*edit-element-key2* key2)
              (*array-edit-element-type1* (parse-edit-element-type type1))
              (*array-edit-element-type2* (parse-edit-element-type type2))
              (*display-edit-box?* 
               (and *edit-element-key1* *edit-element-key2*
                    (not editsubmit) (not editcancel)))
              (*array-object-nrows* 
               (let ((dr *default-rows*))
                 (if (null nrows) 
                     (min *array-object-row-extent* dr)
                   (let ((nr (or (ignore-errors (parse-integer nrows)) dr)))
                     (if (not (plusp nr)) dr nr)
                     ))))
              (*array-object-ncols* 
               (let ((dc *default-cols*))
                 (if (null ncols) 
                     (min *array-object-col-extent* dc)
                   (let ((nc (or (ignore-errors (parse-integer ncols)) dc)))
                     (if (not (plusp nc)) dc nc)
                     )))))
         (multiple-value-bind
             (*array-start-row-index*
              *array-start-col-index*
              *array-end-row-index*
              *array-end-col-index*
              *array-object-nrows* 
              *array-object-ncols*
              )
             (array-start-end startrow startcol)
           (log-data-editor-use 
            "Array editor on object with key = ~D, indices ~A, ~A~%" 
            *object-key* key1 key2)
           (html 
            ;; enable modern CSS processing
            (:princ *transitional-html-doctype-header*)
            (:html
             (block exit
               ;; HTML head (which includes loading the CSS and doing <title>)
               (html-for-data-editor-head 
                *object-editor-css-file* "Table Editor" :req req)
               (html
                :newline
                (:body 
                 ((:div :id "everything")
                  ;; do the redirect if we're coming in via an edit box, 
                  ;; so we don't need the title
                  (unless editsubmit 
                    ;; "Data Frame <name>" at top
                    (html-for-edit-array-title))
                  (when editsubmit 
                    (unless (and key1 key2) (error "internal error!"))
                    (handle-edit-array-submit input))
                  (if editsubmit 
                      (insert-redirect-code)
                    (html-for-edit-array)
                    )))))))))))))

(defun handle-edit-array-submit (input)
  (let ((key1 (keystring-to-key *edit-element-key1* *array-edit-element-type1*))
        (key2 (keystring-to-key *edit-element-key2* *array-edit-element-type2*))
        (newdata (content? (url-parameter-value :editboxcontents input nil)))
        (interpretation (url-parameter-value :interpretation input))
        )
    (when newdata 
      (cond
       ((string-equal interpretation "String") 
        (array-setf key1 key2 newdata))
       ((string-equal interpretation "Form")
        (array-setf key1 key2 (read-from-string-or-error-text newdata))
        )))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; HTML generation

(defun html-for-edit-array-title ()
  (flet ((add-garray-name (s g) 
           (let ((n (garray-named g)))
             (if (null n) 
                 s
               (formatn "~A named ~A" s n)
               ))))
    (html 
     ((:div :id "titleframe") 
      ((:div :id "titletext")
       ((:h2 :style "text-align: center;")
        (:princ-safe
         (case *object-display-type*
           (:lisp-array 
            (formatn
             "An Array of size ~D by ~D" 
             (array-dimension *edit-object* 0)
             (array-dimension *edit-object* 1)
             ))
           (:garray-sequence-sequence 
            (add-garray-name 
             (formatn 
              "A table of size ~D by ~D"
              (garray-axis-extent *edit-object* 0)
              (garray-axis-extent *edit-object* 1)
              )
             *edit-object*
             ))
           (:garray-sequence-hash 
            (if *garray-second-axis-hash-keys-if-same*
                (add-garray-name
                 (formatn "A table of size ~D by ~D keys"
                          (garray-axis-extent *edit-object* 0)
                          (length *garray-second-axis-hash-keys-if-same*))
                 *edit-object*
                 )
              (add-garray-name
               (formatn 
                "A table of size ~D by varying lengths (keys)"
                (garray-axis-extent *edit-object* 0)
                )
               *edit-object*
               )))
           (:garray-hash-sequence
            (add-garray-name
             (formatn
              "A table of ~D keys, each with ~D elements"
              (hash-table-count (utils::garray-data *edit-object*))
              (garray-axis-extent *edit-object* 1)
              )
             *edit-object*
             ))
           (:garray-hash-hash
            (if *garray-second-axis-hash-keys-if-same*
                (add-garray-name
                 (formatn "A table of ~D keys by ~D keys"
                          (hash-table-count (utils::garray-data *edit-object*))
                          (length *garray-second-axis-hash-keys-if-same*))
                 *edit-object*
                 )
              (add-garray-name
               (formatn "A table of paired keys")
               *edit-object*))
            ))))
       ((:div :class "standard-controls")
        (html-for-array-editor-controls))
       )))))

(defun html-for-edit-array ()
  (html 
   ((:div :id "frames-and-edit-boxes")
    (let* ((nrows *array-object-nrows*)
           (classes 
             (if (null *display-edit-box?*)
                 (list "fsize4" "esize4")
               (nrows->classes nrows)
               )))
        ;; top panel
        (html-for-2d-table-display (first classes))
        ;; bottom panel
        (when *display-edit-box?*
          (html
           ((:div :id "editboxes" :class (second classes))
            ((:form :method "get" :action "array-editor")
             (html-for-edit-array-edit-element)
             ))))))))

(defun html-for-edit-array-edit-element () 
  (html 
   (html-for-array-editor-hidden-variables)
   :br
   (let* ((key1
           (keystring-to-key *edit-element-key1* *array-edit-element-type1*))
          (key2
           (keystring-to-key *edit-element-key2* *array-edit-element-type2*))
          (val (array-access key1 key2 :if-error "")))
     (html 
      :newline
      ((:div :class "editbox")
       :newline
       ((:div :class "editbox-label") 
        (html-for-2d-element-label key1 key2)
        (html-for-edit-box-controls val)
        )
       (html-for-edit-box val)
       )))
   ;; workaround for " " --> "+" bug as last parameter
   (hidden "dummy" "dummy")
   ))

(defun html-for-2d-element-label (key1 key2)
  (let ((d (array-descriptor)))
    (html
     (:b 
      (:princ-safe 
       (formatn "Element (~S, ~S) of ~A ~A" key1 key2 (a-or-an? d) d)
       )))))

(defun html-for-array-editor-hidden-variables ()
  (html-for-array-display-hidden-variables)
  (html
   (hidden "startrow" *array-start-row-index*)
   :newline
   (hidden "startcol" *array-start-col-index*)
   :newline
   (hidden "nrows" *array-object-nrows*)
   :newline
   (hidden "ncols" *array-object-ncols*)
   :newline
   (when (and *edit-element-key1* *edit-element-key2*)
     (html 
      (hidden "key1" *edit-element-key1*)
      :newline
      (hidden "key2" *edit-element-key2*)
      :newline
      (hidden "key1type" *array-edit-element-type1*)
      :newline
      (hidden "key2type" *array-edit-element-type2*)
      :newline
      ))))

(defun html-for-array-display-hidden-variables ()
  (html
   (hidden "PKG" (string wb::*sessionid*))
   :newline
   (hidden "objectkey" *object-key*)
   :newline
   (when *parent-key* 
     (hidden "pkey" (formatn "~D" *parent-key*)))
   :newline
   ))

(defun html-for-2d-table-display (fclass)
  (html
   ((:div :id "frametable" :class fclass)
    :newline
    ((:table :border 0 :cellspacing 0 :cellpadding 0)
     :newline
     (case *object-display-type*
       ((:lisp-array :garray-sequence-sequence)
        (when (second *garray-enum-axes?*)
          (html-for-second-axis-hash-keys 
           *garray-enum-axis2-possibles*
           (if (first *garray-enum-axes?*) 2 1)
           ))
        ;; bbbb
        ;; (print (list 's *array-start-row-index* 'e *array-end-row-index*))
        ;; (print *user-base*)
        (loop 
         for i from *array-start-row-index* to *array-end-row-index*
         for enum-index from 0
         for label from *user-base*
         do
         (html 
          (:tr 
           (if (first *garray-enum-axes?*)
               (html-for-enum-label "right" enum-index label)
             (html-for-enum-or-hash-label "right" i))
           (loop 
            for k from *array-start-col-index* to *array-end-col-index*
            do
            (emit-array-element-tds i k k :both)
            )))))
       (:garray-hash-sequence 
        (let ((sorted-hash-keys 
               (sort-garray-toplevel-hash-keys *edit-object*)))
          (when (second *garray-enum-axes?*)
            (html-for-second-axis-hash-keys *garray-enum-axis2-possibles* 2))
          (loop 
           for i from *array-start-row-index* to *array-end-row-index*
           for keys on 
           (nthcdr 
            (- *array-start-row-index* *array-object-first-row-index*)
            sorted-hash-keys)
           as key = (first keys)
           do 
           (html 
            (:tr 
             (html-for-enum-or-hash-label "right" key i)
             (loop 
              for j from *array-start-col-index* to *array-end-col-index*
              do
              (emit-array-element-tds key j j :col)
              ))))))
       (:garray-hash-hash
        (let ((sorted-first-axis-hash-keys 
               (sort-garray-toplevel-hash-keys *edit-object*)))
          (if (null *garray-second-axis-hash-keys-if-same*)
              (let* ((keys (all-second-axis-hash-keys *edit-object*))
                     (sorted-keys (sort-as-number-strings-and-goo keys))
                     (second-axis-keys
                      (nthcdr 
                       (- *array-start-col-index*
                          *array-object-first-col-index*)
                       sorted-keys)))
                (html-for-second-axis-hash-keys second-axis-keys 2)
                (html-for-hash-hash-rows 
                 sorted-first-axis-hash-keys second-axis-keys "---" 
                 ))
            (let* ((sorted-second-axis-hash-keys 
                    (sort-as-number-strings-and-goo
                     *garray-second-axis-hash-keys-if-same*))
                   (second-axis-keys
                    (nthcdr 
                     (- *array-start-col-index* *array-object-first-col-index*)
                     sorted-second-axis-hash-keys)))
              (html-for-second-axis-hash-keys second-axis-keys 2)
              (html-for-hash-hash-rows 
               sorted-first-axis-hash-keys second-axis-keys :error)
              ))))
       (:garray-sequence-hash
        (if (null *garray-second-axis-hash-keys-if-same*)
            (let* ((keys (all-second-axis-hash-keys *edit-object*))
                   (sorted-keys (sort-as-number-strings-and-goo keys))
                   (second-axis-keys
                    (nthcdr 
                     (- *array-start-col-index* *array-object-first-col-index*)
                     sorted-keys)))
              (html-for-second-axis-hash-keys 
               second-axis-keys (if (first *garray-enum-axes?*) 2 1)) 
              (html-for-sequence-hash-rows second-axis-keys "---"))
          (let* ((sorted-second-axis-hash-keys 
                  (sort-as-number-strings-and-goo 
                   *garray-second-axis-hash-keys-if-same*))
                 (second-axis-keys
                  (nthcdr 
                   (- *array-start-col-index* *array-object-first-col-index*)
                   sorted-second-axis-hash-keys)))
            (html-for-second-axis-hash-keys 
             second-axis-keys (if (first *garray-enum-axes?*) 2 1))
            (html-for-sequence-hash-rows second-axis-keys :error)
            )))
       (otherwise (error "not implemented yet!"))
       )))))

(defun html-for-hash-hash-rows
       (sorted-first-axis-hash-keys second-axis-keys if-error)
  (loop 
   for i from *array-start-row-index* to *array-end-row-index*
   for first-key in  
   (nthcdr 
    (- *array-start-row-index* *array-object-first-row-index*)
    sorted-first-axis-hash-keys)
   do
   (html 
    (:tr
     (html-for-enum-or-hash-label "right" first-key i)
     (loop for j from *array-start-col-index* to *array-end-col-index*
           for second-key in second-axis-keys
           do
           (emit-array-element-tds 
            first-key second-key 
            j
            :colpos 
            :if-error if-error
            ))))))

(defun html-for-second-axis-hash-keys 
       (second-axis-keys &optional (initial-blank-td? nil))
  (html 
   (:tr
    (when initial-blank-td?
      (let ((n (if (integerp initial-blank-td?) initial-blank-td? 1)))
        (loop for j from 1 to n do (html (:td "&nbsp;")))
        ))
    (loop for j from *array-start-col-index* to
          *array-end-col-index*
          for display-key in second-axis-keys
          do
          (html 
           ((:th :class "rcindex")
            (:princ "&nbsp;&nbsp;&nbsp;")
            (:princ-safe (formatn "~D" j)))
           (html-for-enum-or-hash-label "left" display-key)
           )))))

(defun html-for-sequence-hash-rows (second-axis-keys if-error)
  (loop 
   for i from *array-start-row-index* to *array-end-row-index*
   for enum-index from 0
   for label from *user-base*
   do
   (html 
    (:tr 
     (if (first *garray-enum-axes?*)
         (html-for-enum-label "right" enum-index label)
       (html-for-enum-or-hash-label "right" i))
     (loop 
      for k from *array-start-col-index* to *array-end-col-index*
      for display-key in second-axis-keys 
      do
      (emit-array-element-tds i display-key k :row :if-error if-error)
      )))))

(defun html-for-enum-label (align index &optional (label-index nil))
  (html-for-enum-or-hash-label 
   align (nth index *garray-enum-axis1-possibles*) label-index
   ))
   
(defun html-for-enum-or-hash-label (align datum &optional (label-index nil))
  (let ((*data-editor-slot-value-length* 
         (max 20 (- *data-editor-slot-value-length* 5))))
    (html 
     (when label-index 
       (html ((:th :class "rcindex")
              (:princ-safe (formatn "~D" label-index))
              "&nbsp;&nbsp;"
              )))
     ((:th :align align)
      "&nbsp;"
      (html-for-display-element datum)
      ))))

(defun garray-second-axis-hashes-have-same-keys? (garray)
  (block exit
    (let ((gdata (utils::garray-data garray)))
      (let ((firsthash nil))
        (flet ((test (value) 
                 (cond
                  ((null firsthash) (setq firsthash value))
                  ((null (hash-tables-have-identical-keys? firsthash value))
                   (return-from exit nil))
                  (t nil)
                  )))
          (etypecase (first (garray-axes garray))
            (utils::hash-axis 
             (maphash 
              (lambda (key value) (declare (ignore key)) (test value))
              gdata
              ))
            (utils::enum-axis
             (loop for j from 0 below (garray-axis-extent garray 0)
                   as value = (aref gdata j)
                   do 
                   (test value)
                   ))
            (utils::num-axis 
             (loop 
              with first-index = (garray-axis-first-index garray 0)
              with last-index = (garray-axis-last-index garray 0)
              for j from first-index to last-index
              as value = (aref gdata (- j first-index))
              do
              (test value)
              )))
          (hash-table-keys firsthash)
          )))))

(defun sort-garray-toplevel-hash-keys (garray)
  (sort-as-number-strings-and-goo (garray-component-indices garray)))

(defun html-for-array-editor-controls ()
  (html
   ((:form :method "get" :action *array-editor-url*
     :class "sequencenavform")
    (root-link-generator)
    (html-for-generic-navigator-url
     :parent *parent-key* 
     (lambda (type) (declare (ignore type)) (parent-url *parent-key*)))
    (html-for-array-navigator-url 
     :start (or (/= *array-object-first-row-index* *array-start-row-index*)
                (/= *array-object-first-col-index* *array-start-col-index*)))
    (html-for-array-navigator-url 
     :left (/= *array-object-first-col-index* *array-start-col-index*))
    (html-for-array-navigator-url 
     :right (/= *array-object-last-col-index* *array-end-col-index*))
    (html-for-array-navigator-url 
     :up (/= *array-object-first-row-index* *array-start-row-index*))
    (html-for-array-navigator-url 
     :down (/= *array-object-last-row-index* *array-end-row-index*))
    (html-for-array-navigator-url 
     :end (or (/= *array-object-last-row-index* *array-end-row-index*)
              (/= *array-object-last-col-index* *array-end-col-index*)))
    (html-for-de-enter-box-control 
     "Row" "startrow" *array-start-row-index* :pre-spacing 4)
    (html-for-de-enter-box-control 
     "Col" "startcol" *array-start-col-index* :pre-spacing 3)
    (html-for-de-enter-box-control 
     "# of rows" "nrows" *array-object-nrows* :pre-spacing 3)
    (html-for-de-enter-box-control 
     "# of cols" "ncols" *array-object-ncols* :pre-spacing 3)
    ((:input :type "submit" :value "Display"))
    (html-for-array-display-hidden-variables)
    )))

(defun html-for-de-enter-box-control
       (title name value &key (class "sequence-control-box") (pre-spacing nil))
  (when pre-spacing (loop for j from 1 to pre-spacing do (html "&nbsp;")))
  (html 
   (:i (:princ-safe (s+ title ": ")))
   ((:input :type "text" :name name :class class :value (formatn "~A" value)))
   :newline
   ))

(defun html-for-array-navigator-url (type enable?)
  (html-for-generic-navigator-url type enable? 'array-navigator-url))

(defun emit-array-element-tds 
       (row column column-position key-display-mode &key (if-error :error))
  (let ((*hidden-box-class* 
         (if (> (1+ (- column-position *array-start-col-index*))
                (round (1+ (- *array-end-col-index* *array-start-col-index*)) 2)
                )
             "hidden-string-right"
           "hidden-string-left"
           )))
    (html 
     :newline
     ((:td :class "slotname")
      ((:div :class "sequence-index")
       ((:a :href 
         (array-editor-url-for-editing-element
          *object-key*
          *array-start-row-index* *array-start-col-index*
          *array-object-nrows* *array-object-ncols*
          row column
          )
         :class "sequence-index-url"
         )
        (:princ-safe
         (ecase key-display-mode 
           (:both (formatn "(~A,~A): " row column))
           (:row (formatn "(~A): " row))
           (:col (formatn "(~A): " column))
           (:colpos (formatn "(~A): " column-position))
           ((nil :neither) "+")
           )))))
     :newline
     ((:td :class "slotvalue")
      ((:div :class "sequence-element")
       (html-for-display-element (array-access row column :if-error if-error))
       ;; "&nbsp;&nbsp;"
       )))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; URL generation

(defun simple-array-editor-url (key)
  (if *parent-key*
      (formatn "~A?pkg=~A&objectkey=~D&pkey=~D"
               *array-editor-url* wb::*sessionid* key *parent-key*)
    (formatn "~A?pkg=~A&objectkey=~D" *array-editor-url* wb::*sessionid* key)
    )
  )

(defun array-editor-url-for-editing-element 
       (object-key startrow startcol nrows ncols key1 key2)
  (multiple-value-bind (keystring1 type1)
      (key-to-keystring-and-type key1)
    (multiple-value-bind (keystring2 type2)
        (key-to-keystring-and-type key2)
      (s+ 
       (simple-array-editor-url object-key)
       (formatn 
        (one-string
         "&startrow=~D"
         "&startcol=~D"
         "&nrows=~D"
         "&ncols=~D"
         "&seb=1"
         "&key1=~A"
         "&key2=~A"
         "&key1type=~A"
         "&key2type=~A"
         )
        startrow startcol
        nrows ncols 
        (url-safe-string keystring1)
        (url-safe-string keystring2)
        (string type1)
        (string type2)
        )))))

(defun array-navigator-url (type)
  (let ((params "&startrow=~D&startcol=~D&nrows=~D&ncols=~D"))
    (s+ 
     (simple-array-editor-url *object-key*)
     (case type
       (:start 
        (formatn 
         params
         *array-object-first-row-index* *array-object-first-col-index*
         *array-object-nrows* *array-object-ncols*
         ))
       (:left
        (formatn 
         params
         *array-start-row-index*
         (max 
          *array-object-first-col-index*
          (- *array-start-col-index* *array-object-ncols*))
         *array-object-nrows* *array-object-ncols*
         ))
       (:right
        (formatn 
         params
         *array-start-row-index*
         (min 
          (max 
           *array-object-first-col-index*
           (1+ (- *array-object-last-col-index* *array-object-ncols*)))
          (+ *array-start-col-index* *array-object-ncols*)
          )
         *array-object-nrows* *array-object-ncols*
         ))
       (:up
        (formatn 
         params
         (max 
          *array-object-first-row-index*
          (- *array-start-row-index* *array-object-nrows*))
         *array-start-col-index*
         *array-object-nrows* *array-object-ncols*
         ))
       (:down
        (formatn 
         params
         (min 
          (max 
           *array-object-first-row-index*
           (1+ (- *array-object-last-row-index* *array-object-nrows*)))
          (+ *array-start-row-index* *array-object-nrows*)
          )
         *array-start-col-index*
         *array-object-nrows* *array-object-ncols*
         ))
       (:end 
        (formatn
         params
         (max 
          *array-object-first-row-index*
          (1+ (- *array-object-last-row-index* *array-object-nrows*))
          )
         (max 
          *array-object-first-col-index*
          (1+ (- *array-object-last-col-index* *array-object-ncols*))
          )
         *array-object-nrows* *array-object-ncols*
         ))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Array/garray information

(defun array-display-type ()
  (etypecase *edit-object*
    (array :lisp-array)
    (utils::garray
     (let ((a1-extent (garray-axis-extent *edit-object* 0))
           (a2-extent (garray-axis-extent *edit-object* 1)))
       (cond
        ((and (null a1-extent) (null a2-extent)) :garray-hash-hash)
        ((null a1-extent) :garray-hash-sequence)
        ((null a2-extent) :garray-sequence-hash)
        (t :garray-sequence-sequence)
        )))))

(defun array-first-index (axis)
  (case *object-display-type*
    (:lisp-array *user-base*)
    (:garray-sequence-sequence 
     (numeric-axis-first-index *edit-object* axis))
    (:garray-sequence-hash 
     (ecase axis
       (0 (numeric-axis-first-index *edit-object* 0))
       (1 *user-base*)
       ))
    (:garray-hash-sequence
     (ecase axis
       (0 *user-base*)
       (1 (numeric-axis-first-index *edit-object* 1))
       ))
    (:garray-hash-hash *user-base*)
    ))

(defun array-last-index (axis)
  (let ((gd (unless (arrayp *edit-object*)
              (utils::garray-data *edit-object*))))
    (ecase *object-display-type*
      (:lisp-array
       (- 
        (array-dimension *edit-object* axis)
        (if (zerop *user-base*) 1 0)
        ))
      (:garray-sequence-sequence
       (numeric-axis-last-index *edit-object* axis))
      (:garray-sequence-hash 
       (ecase axis
         (0 (numeric-axis-last-index *edit-object* 0))
         (1 (loop for h across gd maximize (hash-table-count h)))
         ))
      (:garray-hash-sequence 
       (ecase axis
         (0 (hash-table-count gd))
         (1 (numeric-axis-last-index *edit-object* 1))
         ))
      (:garray-hash-hash 
       (ecase axis
         (0 (hash-table-count gd))
         (1 
          (loop
           for h being the hash-values of gd 
           maximize (hash-table-count h)
           )))))))

(defun array-descriptor ()
  (etypecase *edit-object*
    (array "Array")
    (utils::garray "2d Table")
    ))

(defun numeric-axis-first-index (garray axis)
  (typecase (nth axis (garray-axes garray))
    (utils::enum-axis *user-base*)
    (otherwise (garray-axis-first-index garray axis))
    ))

(defun numeric-axis-last-index (garray axis)
  (typecase (nth axis (garray-axes garray))
    (utils::enum-axis 
     (- (garray-axis-extent garray axis) (if (zerop *user-base*) 1 0)))
    (otherwise (garray-axis-last-index garray axis))
    ))
    
(defun array-start-end (startrow startcol)
  (let ((a0len *array-object-row-extent*)
        (a1len *array-object-col-extent*))
    (when (or (zerop a0len) (zerop a1len))
      (error "Array or Table should not be zero size!"))
    (labels ((clamp (val min max)
               (cond 
                ((< val min) min)
                ((> val max) max)
                (t val)
                ))
             (parse-and-clamp (n axis)
               (clamp 
                n
                (ecase axis 
                  (0 *array-object-first-row-index*)
                  (1 *array-object-first-col-index*))
                (ecase axis
                  (0 *array-object-last-row-index*)
                  (1 *array-object-last-col-index*)
                  )))
             (parse-start (s axis)
               (setq s (ignore-errors (parse-integer s)))
               (when s (setq s (parse-and-clamp s axis)))
               s)
             )
      ;; 1.  Come up with reasonable values for startrow and startcol
      (setq startrow 
            (if (null startrow) 
                *array-object-first-row-index*
              (parse-start startrow 0)))
      (setq startcol
            (if (null startcol)
                *array-object-first-col-index*
              (parse-start startcol 1)))
      (when (null startrow) (setq startrow *array-object-first-row-index*))
      (when (null startcol) (setq startcol *array-object-first-col-index*))
        
      (let ((endrow (parse-and-clamp (+ startrow (1- *array-object-nrows*)) 0))
            (endcol (parse-and-clamp (+ startcol (1- *array-object-ncols*)) 1)))
        (values 
         startrow startcol endrow endcol 
         (1+ (- endrow startrow)) (1+ (- endcol startcol))
         )))))

(defun all-second-axis-hash-keys (garray)
  (let* ((gdata (utils::garray-data garray))
         (test nil)
         (all-keys 
          (etypecase (first (garray-axes garray))
            (utils::hash-axis 
             (setq test (hash-table-test gdata))
             (loop for val being the hash-values of gdata
                   nconc
                   (hash-table-keys val)
                   ))
            (utils::num-axis 
             (loop 
              with first-index = (garray-axis-first-index garray 0)
              with last-index = (garray-axis-last-index garray 0)
              for j from first-index to last-index
              as value = (aref gdata (- j first-index))
              nconc
              (hash-table-keys value)
              do
              (when (= j first-index) (setq test (hash-table-test value)))
              )))))
    (purge-duplicates all-keys :test test)
    ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Retrieve a value from *edit-object* given indices.
;; Convert indices to enum values if appropriate.
;; If IF-ERROR is :error, error out if location has never been set
;; and garray calls for an error to be generated; otherwise return
;; the value of IF-ERROR.

(defun array-access (i1 i2 &key (if-error :error))
  (when (first *garray-enum-axes?*)
    (setq i1 (enum-from-index *edit-object* 0 i1)))
  (when (second *garray-enum-axes?*)
    (setq i2 (enum-from-index *edit-object* 1 i2)))
  (flet ((doit ()
           (funcall (if (zerop *user-base*) 'cref 'ref) *edit-object* i1 i2)))
    (case if-error 
      (:error (doit))
      (otherwise (handler-case (doit) (error () if-error)))
      )))

(defun array-setf (i1 i2 value)
  (when (first *garray-enum-axes?*)
    (setq i1 (enum-from-index *edit-object* 0 i1)))
  (when (second *garray-enum-axes?*)
    (setq i2 (enum-from-index *edit-object* 1 i2)))
  (if (zerop *user-base*)
      (setf (cref *edit-object* i1 i2) value)
    (setf (ref *edit-object* i1 i2) value)
    ))




