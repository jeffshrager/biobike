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

#||

Parameters we have to deal with for a sequence editor: 

Key: Identifies the object in the *de-table* 
Start: The index of where to start displaying elements
End: The index of where to end displaying elements
NToShow: 
Ncolumns: The number of columns the data is to be laid out in 
Base: One or zero (Biobike uses 1)
Add-after: Where to put a new element (only works for lists, and then not
  before the first element!)
Edit-element: The index of the element to be edited
Edited-element: The index of the element that was edited
Edit-submit: Request is coming from the submit button
Edit-cancel: Request is coming from the cancel button

||#



(publish 
 :path (s+ "/" *sequence-editor-url*)
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
                 "The sequence you requested is no longer in the data editor's"
                 "object cache.  Objects you view vanish after ~D minutes.")
                *data-editor-cache-duration*
                ))))))
       (with-parent-key-bindings
         (sequence-editor-page req ent input)
         ))
     (purge-de-table (* 60 *data-editor-cache-duration*))
     (purge-pid-table (* 60 *data-editor-cache-duration*))
     )))

(defun sequence-editor-page (req ent input)
  (let* ((package-name (url-parameter-value :pkg input))
         (package-symbol (keywordize package-name))
         ;; index of element to be edited
         (edit-element (content? (url-parameter-value :edit-element input)))
         ;; is the index a string, a form, or a form to be evaluated?
         ;; by default, it is a form which we do a read-from-string on 
         (edit-element-type 
          (content? (url-parameter-value :edit-element-type input)))
         ;; is this request coming from the edit box SUBMIT button
         ;; or the edit box CANCEL button?
         (editsubmit (url-parameter-value :editsubmit input))
         (editcancel (url-parameter-value :editcancel input))
         ;; index of element that was edited
         (edited-element 
          (content? (url-parameter-value :edited-element input)))
         ;; did user want to add a slot?
         (add-after (content? (url-parameter-value :add-after input)))
         (ncolumns (url-parameter-value :ncolumns input))
         (start (content? (url-parameter-value :start input)))
         ;; (end (content? (url-parameter-value :end input)))
         (ntoshow (content? (url-parameter-value :ntoshow input)))
         (base (content? (url-parameter-value :base input)))
         )
    (declare (ignore editcancel))
    (wb::execute-with-standard-weblistener-environment
     req ent package-symbol
     (lambda () 
       ;; (setq *input* input)
       (let* ((*user-base* 
               (if base (parse-integer base) (if (wb::bbl-mode?) 1 0)))
              (*object-display-type* (sequence-display-type))
              (*sequence-object-first-index* (sequence-first-index))
              (*sequence-object-last-index* (sequence-last-index))
              (*sequence-object-size* (sequence-size))
              (*sequence-edit-element* edit-element)
              (*sequence-edit-element-type* 
               (parse-edit-element-type edit-element-type))
              (*display-edit-box?* edit-element)
              (*sequence-ncolumns* 
               (let ((nc (or (ignore-errors (parse-integer ncolumns)) 3)))
                 (if (not (plusp nc)) 3 (min 12 nc))
                 ))
              (*sequence-ntoshow* 
               (let* ((default-to-show 
                       (* *sequence-ncolumns* *sequence-max-rows-to-show*))
                      (nts
                       (or (ignore-errors (parse-integer ntoshow))
                           default-to-show
                           )))
                 (if (not (plusp nts)) 
                     default-to-show
                   (min nts *sequence-object-size*)
                   )))
              (*sequence-start* 
               (let ((ss 
                      (or (ignore-errors (parse-integer start))
                          *sequence-object-first-index*
                          )))
                 (cond
                  ((< ss *sequence-object-first-index*)
                   *sequence-object-first-index*)
                  ((> ss *sequence-object-last-index*)
                   *sequence-object-last-index*)
                  (t ss)
                  )))
              (*sequence-end* (1- (+ *sequence-start* *sequence-ntoshow*))))
         (when (> *sequence-end* *sequence-object-last-index*)
           (setq *sequence-end* *sequence-object-last-index*)
           (setq *sequence-ntoshow* (1+ (- *sequence-end* *sequence-start*))))
         #+debug
         (print (list 'see *sequence-edit-element*
                      'SK *object-key*
                      'SB *user-base*
                      'SC *sequence-ncolumns*
                      'SO *edit-object*
                      'ss *sequence-start*
                      'se *sequence-end*
                      'snts *sequence-ntoshow*
                      ))
         (log-data-editor-use "Sequence editor on key: ~A~%" *object-key*)
         (html 
          ;; enable modern CSS processing
          (:princ *transitional-html-doctype-header*)
          (:html
           (block exit
             ;; HTML head (which includes loading the CSS and doing <title>)
             (html-for-data-editor-head 
              *object-editor-css-file* "Sequence Editor" :req req)
             (html
              :newline
              (:body 
               ((:div :id "everything")
                ;; do the redirect if we're coming in via an edit box, 
                ;; so we don't need the title
                (unless editsubmit 
                  ;; "Data Frame <name>" at top
                  (html-for-edit-sequence-title))
                (when editsubmit 
                  (unless edited-element (error "internal error!"))
                  (when edit-element (error "internal error 2!"))
                  (handle-edit-sequence-submit edited-element input))
                (when add-after (error "Not implemented yet!"))
                (if editsubmit
                    (insert-redirect-code)
                  (html-for-edit-sequence)
                  ))))))))))))

(defun handle-edit-sequence-submit (keystring input)
  ;; (print (list 'ks keystring 'i input))
  (let ((key (keystring-to-key keystring *sequence-edit-element-type*))
        (newdata (content? (url-parameter-value :editboxcontents input nil)))
        (interpretation (url-parameter-value :interpretation input))
        )
    (when newdata 
      (cond
       ((string-equal interpretation "String") 
        (sequence-setf key newdata))
       ((string-equal interpretation "Form")
        (sequence-setf key (read-from-string-or-error-text newdata))
        )))))

(defun adjust-for-max-rows ()
  (let ((nrows (ceiling *sequence-ntoshow* *sequence-ncolumns*)))
    (when (> nrows *sequence-max-rows-to-show*)
      (let ((ntoshow (* *sequence-max-rows-to-show* *sequence-ncolumns*)))
        (setq *sequence-end* (1- (+ *sequence-start* ntoshow)))
        (setq *sequence-ntoshow* ntoshow)
        ))))

(defun html-for-edit-sequence-title ()
  (html 
   ((:div :id "titleframe") 
    ((:div :id "titletext")
     ((:h2 :style "text-align: center;")
      (:princ-safe
       (formatn 
        "A ~A of ~D element~P" (sequence-descriptor) *sequence-object-size* 
        *sequence-object-size*
        )))
     ((:div :class "standard-controls")
      (html-for-sequence-editor-controls))
     ))))

(defun html-for-edit-sequence ()
  (html 
   ((:div :id "frames-and-edit-boxes")
    (let* ((nrows (ceiling *sequence-ntoshow* *sequence-ncolumns*))
           (classes 
            (if (null *sequence-edit-element*) 
                (list "fsize4" "esize4")
              (nrows->classes nrows)
              )))
        ;; top panel
        (html-for-edit-sequence-table (first classes))
        ;; bottom panel
        (when *display-edit-box?*
          (html
           ((:div :id "editboxes" :class (second classes))
            ((:form :method "get" :action "sequence-editor")
             (html-for-edit-sequence-edit-element)
             ))))))))

(defun html-for-edit-sequence-edit-element () 
  (html 
   (html-for-sequence-editor-hidden-variables)
   :br
   (let* ((key (keystring-to-key 
                *sequence-edit-element* *sequence-edit-element-type*))
          (val (sequence-access key)))
     (html 
      :newline
      ((:div :class "editbox")
       :newline
       ((:div :class "editbox-label") 
        (html-for-1d-element-label key)
        (html-for-edit-box-controls val)
        )
       (html-for-edit-box val)
       )))
   ;; workaround for " " --> "+" bug as last parameter
   (hidden "dummy" "dummy")
   ))

(defun html-for-1d-element-label (key)
  (html
   (:b (:princ-safe (s+ (formatn "Element ~S of " key) (sequence-descriptor))))
   ))

(defun html-for-sequence-editor-hidden-variables ()
  (html-for-sequence-display-hidden-variables)
  (html
   (hidden "start" *sequence-start*)
   :newline
   (hidden "ncolumns" *sequence-ncolumns*)
   :newline
   (hidden "ntoshow" *sequence-ntoshow*)
   :newline
   ;; (hidden "end" *sequence-end*)
   ;; :newline
   (when *sequence-edit-element* 
     (html 
      (hidden "edited-element" *sequence-edit-element*)
      :newline
      (hidden "edit-element-type" *sequence-edit-element-type*) 
      :newline
      ))
   ))

(defun html-for-sequence-display-hidden-variables ()
  (html
   (hidden "PKG" (string wb::*sessionid*))
   :newline
   (hidden "base" *user-base*)
   :newline
   (hidden "objectkey" *object-key*)
   :newline
   (when *parent-key* 
     (hidden "pkey" (formatn "~D" *parent-key*)))
   :newline
   ))

(defun html-for-edit-sequence-table (fclass)
  (html
   ((:div :id "frametable" :class fclass)
    :newline
    ;; (unless *sequence-edit-element* (html-for-sequence-editor-controls))
    ((:table :border 0 :cellspacing 0 :cellpadding 0)
     :newline
     (let ((nrows (ceiling *sequence-ntoshow* *sequence-ncolumns*)))
       (ecase *object-display-type* 
         ((:lisp-sequence :garray-sequence)
          (loop
           for j from 1 to nrows 
           with index = *sequence-start* 
           do
           (html 
            (:tr 
             (loop 
              for col from 1 to *sequence-ncolumns* do
              (unless (> index *sequence-end*)
                (emit-sequence-element-tds index col)
                (incf index)
                ))))))
         ((:hash-table :garray-hash)
          (loop 
           for j from 1 to nrows
           with index = *sequence-start*
           with keys = 
           (hash-table-elements 
            (case *object-display-type*
              (:hash-table *edit-object*)
              (:garray-hash (utils::garray-data *edit-object*)))
            *sequence-start* *sequence-end*)
           do
           (html 
            (:tr 
             (loop 
              for col from 1 to *sequence-ncolumns* do
              (unless (> index *sequence-end*)
                (emit-sequence-element-tds (first keys) col)
                (incf index)
                (pop keys)
                ))))))))))))

(defun html-for-sequence-editor-controls ()
  (html
   ((:form :method "get" :action *sequence-editor-url*
     :class "sequencenavform")
    (root-link-generator)
    (html-for-generic-navigator-url
     :parent *parent-key* 
     (lambda (type) (declare (ignore type)) (parent-url *parent-key*)))
    (html-for-sequence-navigator-url 
     :start (/= *sequence-object-first-index* *sequence-start*))
    (html-for-sequence-navigator-url 
     :prev (/= *sequence-object-first-index* *sequence-start*))
    (html-for-sequence-navigator-url 
     :next (/= *sequence-object-last-index* *sequence-end*))
    (html-for-sequence-navigator-url 
     :end (/= *sequence-object-last-index* *sequence-end*))
    (:i "&nbsp;&nbsp;&nbsp;&nbsp; Start: ")
    ((:input :type "text" :name "start" :class "sequence-control-box"
      :value (formatn " ~D" *sequence-start*)))
    :newline
    (:i "&nbsp;&nbsp;&nbsp;" "Show ")
    ((:input :type "text" :name "ntoshow" :class "sequence-control-box"
      :value (formatn " ~D" *sequence-ntoshow*)))
    (:i "&nbsp;items")
    :newline
    (:i "&nbsp;&nbsp;&nbsp;" "# of columns: ")
    ((:input :type "text" :name "ncolumns" :class "sequence-control-box"
      :value (formatn " ~D" *sequence-ncolumns*)))
    "&nbsp;&nbsp;"
    :newline
    ((:input :type "submit" :value "Display"))
    (html-for-sequence-display-hidden-variables)
    ;; :br :br
    )))


   

(defun html-for-sequence-navigator-url (type enable?)
  (html-for-generic-navigator-url type enable? 'sequence-navigator-url))

(defun emit-sequence-element-tds (index column)
  (let ((*hidden-box-class* 
         (if (> column (round *sequence-ncolumns* 2))
             "hidden-string-right"
           "hidden-string-left"
           )))
    (html 
     :newline
     ((:td :class "slotname")
      ((:div :class "sequence-index")
       ((:a :href 
         (sequence-editor-url-for-editing-element 
          *object-key* *sequence-start* *sequence-ntoshow*  
          index *sequence-ncolumns*)
         :class "sequence-index-url"
         )
        (:princ-safe (formatn "~A: " index)))))
     :newline
     ((:td :class "slotvalue")
      ((:div :class "sequence-element")
       (html-for-display-element (sequence-access index))
       ;; "&nbsp;&nbsp;"
       )))))
      
(defun sequence-start-end (s e nts)
  (let ((len *sequence-object-size*))
    (when (zerop len) (error "Sequence should not be zero length!"))
    (labels ((clamp (val min max)
               (cond
                ((< val min) min)
                ((> val max) max)
                (t val)
                ))
             (parse-and-clamp (n) 
               (clamp 
                n
                *sequence-object-first-index* 
                *sequence-object-last-index*
                ))
             (parse-start (s)
               (setq s (ignore-errors (parse-integer s)))
               (when s (setq s (parse-and-clamp s)))
               s)
             (parse-end (e) 
               (block exit
                 (setq e (string-trim *whitespace* e))
                 (when (and (= (length e) 1) (char= #\* (char e 0)))
                   (return-from exit *sequence-object-last-index*))
                 (setq e (ignore-errors (parse-integer e)))
                 (when e (setq e (parse-and-clamp e)))
                 e
                 )))
      ;; 1.  Compute actual number of elements to be shown = N based on
      ;; LEN and NTS (if NTS is provided and sensible).  
      (let ((n len))
        (when nts
          (setq nts (ignore-errors (parse-integer nts)))
          (when nts
            (when (plusp nts) (setq n (min n nts)))
            ))
        ;; 2.  Come up with reasonable values for S and E.  
        (cond
         ((and (null s) (null e))
          (setq s *sequence-object-first-index*)
          (setq e *sequence-object-last-index*))
         ((null e) (setq s (parse-start s)))
         ((null s) (setq e (parse-end e)))
         (t 
          (setq s (parse-start s))
          (setq e (parse-end e))
          ))
        ;; if the parse of S or E failed, provide default values
        (cond
         ;; No valid S or E?  Use start of sequence and how many to
         ;; show added to start (but don't go beyond end of sequence).
         ((and (null s) (null e))
          (setq s *sequence-object-first-index*)
          (setq e (min (+ s n) *sequence-object-first-index*)))
         ;; No value E?  Use how many to show added to start
         ;; (but don't go beyond end of sequence).
         ((null e) 
          (setq e (min *sequence-object-last-index* (1- (+ s n)))))
         ;; No valid S? Use how many to show subtracted off end
         ;; (but don't go below start of sequence).
         ((null s)
          (setq s (max *sequence-object-first-index* (1+ (- e n)))))
         (t nil)
         )
        ;; if start > end, swap them 
        (when (> s e)
          (let ((temp s))
            (setq s e)
            (setq e temp)
            ))
        ;; When the number of elements to be shown as specified
        ;; is less than the number in the range (S E), decrease
        ;; E an appropriate amount.
        (let ((ne (1+ (- e s))))
          (when (> ne n)
            (setq e (- e (- ne n)))
            (setq ne (1+ (- e s)))
            )
          ;; Okay, we have valid START, END and NUMBER OF ELEMENTS TO SHOW values
          (values s e ne)
          )))))

        
#||

For small hash tables the key/value pairs displayed should be sorted
using STRING-LESSP on the string representations of the keys.

So the positions the user specifies apply to the sorted order.

For large hash tables the ordering should just be however they come
out of MAPHASH.  We don't want to create string representations of
millions of keys and sort them each time we display 200 elements of
the hash table!

||#

(defparameter *max-sorted-keys-hash-size* 1000)

(defun hash-table-elements (table start end)
  (if (< (hash-table-count table) *max-sorted-keys-hash-size*)
      (let* ((keys (hash-table-keys table))
             (sorted-keys (sort-as-number-strings-and-goo keys)))
        (subseq sorted-keys start (1+ end))
        )
    (let ((count 0) (keys nil))
      (block exit
        (maphash
         (lambda (key value)
           (declare (ignore value))
           (cond
            ((< count start) (incf count))
            ((> count end) (return-from exit nil))
            (t 
             (incf count)
             (push key keys)
             )))
         table
         ))
      (reverse keys)
      )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Information about the sequence 

(defun sequence-display-type ()
  (etypecase *edit-object*
    ((or list vector) :lisp-sequence)
    (utils::garray
     (if (garray-axis-extent *edit-object* 0) 
         :garray-sequence
       :garray-hash
       ))
    (hash-table :hash-table)
    ))

(defun sequence-first-index ()
  (case *object-display-type*
    (:lisp-sequence  (if (zerop *user-base*) 0 1))
    (:garray-sequence (garray-axis-first-index *edit-object* 0))
    ((:garray-hash :hash-table) 0)
    ))

(defun sequence-last-index ()
  (ecase *object-display-type*
    (:lisp-sequence
     (let ((len (length *edit-object*)))
       (if (zerop *user-base*) (1- len) len)
       ))
    (:garray-sequence (garray-axis-last-index *edit-object* 0))
    (:garray-hash (1- (garray-current-total-size *edit-object*)))
    (:hash-table (1- (hash-table-count *edit-object*)))
    ))

;;; Assumes *sequence-object-first-index*, *sequence-object-last-index*
;;; and *object-display-type* have been set, so we don't have to 
;;; call LENGTH more than once!

(defun sequence-size ()
  (ecase *object-display-type*
    ((:lisp-sequence :garray-sequence)
     (1+ (- *sequence-object-last-index* *sequence-object-first-index*)))
    (:hash-table (hash-table-count *edit-object*))
    (:garray-hash (garray-current-total-size *edit-object*))
    ))
     
(defun sequence-descriptor ()
  (typecase *edit-object*
    (list "List")
    (vector "Vector")
    (hash-table "Hash table")
    (utils::garray "1d Table")
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sequence-access (index)
  (funcall (if (zerop *user-base*) 'cref 'ref) *edit-object* index))

(defun sequence-setf (index value)
  (if (zerop *user-base*)
      (setf (cref *edit-object* index) value)
    (setf (ref *edit-object* index) value)
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; URL generation

(defun simple-sequence-editor-url (key)
  (if *parent-key* 
      (formatn "~A?pkg=~A&objectkey=~D&pkey=~D" 
               *sequence-editor-url* wb::*sessionid* key *parent-key*)
    (formatn "~A?pkg=~A&objectkey=~D" *sequence-editor-url* wb::*sessionid* key)
    )
  )

(defun sequence-editor-url-for-editing-element 
       (key start ntoshow edit-element ncolumns)
  (multiple-value-bind (keystring type)
      (key-to-keystring-and-type edit-element)
    (s+ 
     (simple-sequence-editor-url key)
     (formatn 
      (one-string
       "&start=~D"
       "&ntoshow=~D"
       "&ncolumns=~D"
       "&seb=1"
       "&edit-element=~A"
       "&edit-element-type=~A"
       )
      start ntoshow ncolumns 
      (url-safe-string keystring) 
      (string type)
      ))))

(defun sequence-navigator-url (type)
  (s+ 
   (simple-sequence-editor-url *object-key*)
   (formatn "&ncolumns=~D" *sequence-ncolumns*)
   (formatn "&ntoshow=~D" *sequence-ntoshow*)
   (case type
     (:start 
      (formatn "&start=~D" *sequence-object-first-index*))
     (:prev 
      (formatn 
       "&start=~D" 
       (max 
        *sequence-object-first-index* 
        (- *sequence-start* *sequence-ntoshow*)
        )))
     (:next
      (formatn 
       "&start=~D" 
       (min 
        (max 
         *sequence-object-first-index* 
         (1+ (- *sequence-object-last-index* *sequence-ntoshow*)))
        (+ *sequence-start* *sequence-ntoshow*)
        )))
     (:end 
      (formatn
       "&start=~D"
       (max 
        *sequence-object-first-index* 
        (1+ (- *sequence-object-last-index* *sequence-ntoshow*))
        ))))))



#||

A Parent link should be enabled unless
  a) user has edit box showing (to keep things simple, you don't
want someone going back to a page with an edit box, I think)
  b) there is no parent
If not enabled the link should be greyed out.  It should not disappear to
avoid rearrangement of other links.

When the code to handle a data editor URL is entered, the URL will
either have a PKEY parameter or not.  It will only not have a PKEY
parameter if the object being edited was not clicked on inside the
data editor, but rather the URL for the data editor was generated from
outside, e.g., the VPL.  *parent-key* is bound dynamically to the
integer value of the PKEY parameter, or NIL if it it not present.

Every new page view should generate a unique ID.  This ID, bound to
*key-for-this-page* gets associated with the exact URL for the page in
a hash table.

There is no need to put the association into the hash table unless the
object being edited has components that are editable, although there
is no particular harm in doing so if the hash eventually gets cleaned
up via a timestamp mechanism, which should be done on each page view.

The Parent button uses the URL associated with the PKEY parameter in
the hash table.  The PKEY parameter must be preserved (passed through
in the URL) as the user navigates around the object being displayed,
and as the user edits the object.

When a URL for a component of the current object is being consed up,
the *key-for-this-page* key value is added as a a parameter to the URL
using the PKEY key.  This is the only place the PKEY parameter is
changed.  

When a URL for the same object (with different viewing instructions,
such as NEXT) is being consed up, the *parent-key* value is added as a
parameter to the URL using the PKEY value if it is non-NIL, otherwise
no PKEY parameter is provided, thereby preserving the parent pointer.
In these cases the PKEY parameter is not changed, merely passed
through.




;;; sequences 
;;with
http://localhost:8000/sequence-editor?pkg=MASSAR86063&objectkey=27&pkey=45&start=0&ntoshow=120&ncolumns=3&edit-element=0&edit-element-type=FORM


;;without

http://localhost:8000/sequence-editor?pkg=MASSAR86063&objectkey=27&pkey=45

;;; frames

;without
http://localhost:8000/frame-editor?pkg=MASSAR86063&name=Npun.NpF0001&pkey=50

;;with 
http://localhost:8000/frame-editor?pkg=MASSAR86063&name=Npun.NpF0001&pkey=50&edit=Start-Unknown&delimiter=!

;;; array

;;without
http://localhost:8000/array-editor?pkg=MASSAR86063&objectkey=59&pkey=56

;;with 
http://localhost:8000/array-editor?pkg=MASSAR86063&objectkey=59&pkey=56&startrow=0&startcol=0&nrows=3&ncols=4&key1=0&key2=%3aFRED&key1type=FORM&key2type=FORM

;;; objects
;;without
http://localhost:8000/object-editor?pkg=MASSAR86063&objectkey=57&pkey=56

;;with
http://localhost:8000/object-editor?pkg=MASSAR86063&objectkey=57&pkey=56&editedslot=0

||#