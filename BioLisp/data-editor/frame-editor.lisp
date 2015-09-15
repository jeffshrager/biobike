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


(defun simple-frame-editor-url (frame)
  (let ((base-url (subseq wb::*weblistener-frames-editor-url* 1))
        (safe-fname (url-safe-string (#^fname frame))))
    (if *parent-key* 
        (formatn "~A?pkg=~A&name=~A&pkey=~D" 
                 base-url wb::*sessionid* safe-fname *parent-key*)
      (formatn "~A?pkg=~A&name=~A" base-url wb::*sessionid* safe-fname)
      ))
  )

(defun frame-editor-url-for-editing-slot (frame slot-name)
  (s+ 
   (simple-frame-editor-url frame)
   (formatn "&seb=1")
   (formatn "&edit=~A" (url-safe-string slot-name))
   (if *noshow-slot-names*
     (formatn "&noshow=~A"
              (url-safe-string
               (string-join *noshow-slot-names* *default-fesn-delimiter*)
               ))
     ""
     )
   (formatn "&delimiter=~A" (string *default-fesn-delimiter*))
   ))

(defun determine-new-frame-name (nfn)
  (block exit
    (let* ((s (string-trim *whitespace* nfn))
           (slen (length s))
           (fname nil))
      (cond
       ((zerop slen) (return-from exit :blank))
       ((and (> slen 2) (initial-subsequence-of? s "#$")) 
        (setq fname (subseq s 2)))
       ((initial-subsequence-of? s "#$") (return-from exit :pound-dollar))
       ((not (every 'valid-frame-char? s)) (return-from exit :bad-char))
       (t (setq fname s))
       )
      (if (frame-fnamed fname) fname :noframe)
      )))

(publish 
 :path wb::*weblistener-frames-editor-url*
 :content-type cl-user::*html-publish-content-type*
 :function
 (lambda (req ent) 
   (let* ((input (request-query req))
          (name (url-parameter-value :name input))
          (frame (frame-fnamed name))
          (object frame)
          (gotonewframe (url-parameter-value :gotonewframe input)))
     (block exit 
       (flet ((oops (msg &rest msgargs)
                (wb::with-http-response-and-body (req ent)
                  (html
                   (:big
                    (:b
                     (:princ-safe
                      (apply 'format nil msg msgargs)
                      )))))
                (return-from exit nil)
                ))

         (when gotonewframe
           (let ((new-frame-name (determine-new-frame-name gotonewframe)))
             (case new-frame-name 
               (:blank 
                (oops
                 "No frame name entered!  Use the back button and try again."))
               (:pound-dollar 
                (oops
                 (one-string
                  "No frame name entered (only '#$')! "
                  "Use the back button and try again."
                  )))
               (:bad-char 
                (oops 
                 (one-string
                  "Invalid frame name.  Names cannot contain parens or spaces. "
                  "Use the back button and try again."
                  )))
               (:noframe 
                (oops 
                 (one-string
                  "No frame named '~A' exists in the system. "
                  "Use the back button and try again."
                  )
                 gotonewframe
                 ))
               (otherwise 
                (setq frame (frame-fnamed new-frame-name))
                (setq object frame)
                ))))
               
         (if (null frame)
             (oops 
              (one-string-nl
               "The frame you requested (named ~A) does not exist."
               "Perhaps you misspelled it?")
              name
              )
           ;; make sure organisms don't autoload 
           (let ((bio::*disable-seed-organism-autoload* t))
             (with-parent-key-bindings
               (frame-editor-page req ent input)
               )))
         (purge-de-table (* 60 *data-editor-cache-duration*))
         (purge-pid-table (* 60 *data-editor-cache-duration*))
         )))))

(defun frame-editor-page (req ent input)
  (let* ((package-name (url-parameter-value :pkg input))
         (package-symbol (keywordize package-name))
         (delimiter (url-parameter-value :delimiter input))
         ;; slots not to show, as ! or 'delimiter' separated string
         (noshow (content? (url-parameter-value :noshow input)))
         ;; name of slot to be edited
         (edit (content? (url-parameter-value :edit input)))
         ;; is this request coming from the edit box SUBMIT button
         ;; or the edit box CANCEL button?
         (editsubmit (url-parameter-value :editsubmit input))
         (editcancel (url-parameter-value :editcancel input))
         ;; name of slot that is being edited
         (editedslot (content? (url-parameter-value :editedslot input)))
         ;; did user want to add a slot?
         (addaslot (content? (url-parameter-value :addaslot input)))
         (name (fname *edit-object*))
         )
    (declare (ignore editcancel))
    ;; (setq *input* input)
    (wb::execute-with-standard-weblistener-environment
     req ent package-symbol
     (lambda () 
       (log-data-editor-use "Frame editor on: ~A~%" name)
       (let ((*default-fesn-delimiter* 
              (or (and delimiter (char delimiter 0))
                  *default-fesn-delimiter*
                  ))
             (*edit-slot-name* nil) 
             (*edit-slot* nil) 
             (*display-edit-box?* edit)
             (*frame-editor-error-message* nil))
         (setq delimiter *default-fesn-delimiter*)
         (html 
          ;; enable modern CSS processing
          (:princ *transitional-html-doctype-header*)
          (:html
           ;; HTML head (which includes loading the CSS and doing <title>)
           (html-for-data-editor-head 
            *object-editor-css-file* "Frame Editor" :req req)
           (html
            :newline
            (:body 
             ((:div :id "everything")
              ;; going to do the redirect if we're coming in via an edit box, 
              ;; so we don't need the title
              (unless editsubmit 
                ;; "Data Frame <name>" at top
                (html-for-edit-frame-title *edit-object* name))
              (when editsubmit 
                (unless editedslot (error "internal error!"))
                (when edit (error "internal error 2!"))
                (handle-edit-frame-submit editedslot input *edit-object*))
              ;; if we're adding a slot, handle-add-a-slot sets up
              ;; *edit-slot-name* and *edit-slot*, otherwise
              ;; we set them up using the EDIT url parameter
              (if addaslot 
                  (handle-add-a-slot addaslot *edit-object*)
                (progn
                  (setq *edit-slot-name* edit)
                  (setq *edit-slot* 
                        (when edit (frame-fnamed *edit-slot-name* t)))
                  ))
              (if editsubmit 
                  ;; execute the redirect if we've come in via an edit box
                  (insert-redirect-code)
                (let* ((*noshow-slot-names*
                        (url-slots-to-slots-list noshow))
                       (*slots-to-show* 
                        (frame-slots-to-show *edit-object*)))
                  (html-for-edit-frame *edit-object*)
                  ))))))))))))

;;; Get the (possibly new) value the user entered
;;; for the edit box, convert it appropriately to a Lisp object,
;;; and insert it into the corresponding frame slot.

(defun handle-edit-frame-submit (editedslot input frame &key (commit? nil))
  (let ((change-data 
         (list 
          (frame-fnamed editedslot)
          (content? (url-parameter-value :editboxcontents input nil))
          (url-parameter-value :interpretation input)
          )))
    (destructuring-bind (slot newdata interpretation) 
        change-data 
      (unless slot (error "No frame found named ~A" editedslot))
      (when newdata 
        (cond
         ((string-equal interpretation "String") 
          (setf (slotv frame slot) newdata))
         ((string-equal interpretation "Form")
          (setf (slotv frame slot) (read-from-string-or-error-text newdata))
          ))
        (when commit? 
          (case user::*frame-system-version* 
            (:sframes (forward-package-funcall :db.ac :commit))
            (otherwise nil)
            ))))))

;;; Top panel, currently just displaying frame name

(defun html-for-edit-frame-title (frame name)
  (html 
   ((:div :id "titleframe") 
    ((:div :id "titletext")
     ((:h2 :style "text-align: center;")
      (let ((ftype (type-of frame)))
        (if (or (eq ftype 'frames::%frame) (eq ftype 'frames::aframe))
            (html (:princ-safe "Data Frame "))
          (html (:princ-safe (string-capitalize (s+ (string ftype) " "))))
          ))
      (:princ-safe (string-capitalize name))
      (when (and frame (= 1 (length (frame-slots-of frame))))
        (html (:princ-safe " (No data)"))
        ))
     ((:div :class "frame-controls")
      (html-for-frame-editor-controls)
      )
     ))))

(defun html-for-frame-editor-controls ()
  (html 
   (:table 
    (:tr 
     (:td (root-link-generator))
     (:td       
      (html-for-generic-navigator-url
       :parent *parent-key* 
       (lambda (type) (declare (ignore type)) (parent-url *parent-key*))
       ))
     (:td (html-for-enter-frame-box))
     ))))
  
(defun handle-add-a-slot (slot-name frame)
  (handler-case 
      (let ((newslot (frame-fnamed slot-name t)))
        (unless (frame-has-slot? newslot frame)
          (setf (slotv frame newslot) nil))
        (setq *edit-slot-name* slot-name)
        (setq *edit-slot* newslot)
        (setq *display-edit-box?* t)
        )
    (error 
     (c)
     (setq *frame-editor-error-message* (formatn "~A" c))
     )))

;;; Display the body of the page (the frame itself,
;;; and, if appropriate, either the edit box
;;; in the bottom half or a box to add a slot)

(defun html-for-edit-frame (frame)
  (html 
   :newline
   ((:div :id "frames-and-edit-boxes")
    (let ((nslots (length *slots-to-show*)))
      (multiple-value-bind (list1 list2)
          (if (> nslots *frame-editor-column-split*)
              (first-and-second-halves *slots-to-show*)
              ;; (every-other *slots-to-show*) 
            *slots-to-show*)
        (let ((classes 
               (if (null *edit-slot*)
                   (list "fsize4" "esize4")
                 (nrows->classes (length list1)))))
          ;; top panel
          (html-for-edit-frame-slot-table frame list1 list2 (first classes))
          ;; bottom panel
          (cond
           (*display-edit-box?* 
            (html
             ((:div :id "editboxes" :class (second classes))
              ((:form :method "get" :action "frame-editor")
               (html-for-edit-frame-edit-slot frame)
               ))))
           (t nil)
           )))))))


;;; Display the frame slot names and the frame slot values in 
;;; a two-column format (one column if only a few slots), 
;;; with special handling of various types of slot values

(defun html-for-edit-frame-slot-table (frame list1 list2 fclass)
  (labels ((html-for-slot-name (slot-name)
             (html 
              ((:a :class "slotname-text" 
                :href (frame-editor-url-for-editing-slot frame slot-name))
               (:princ-safe slot-name) ":"
               )))
           (html-for-slotname-link (slot slot-name)
             ;; If the slot is a frame with content, supply
             ;; a link to it.  If it has no content, do not.
             (let ((slots (frame-slots-of slot)))
               (if (> (length slots) 
                      (ecase user::*frame-system-version*
                        (:old 1)
                        (:sframes 2)
                        ))
                   (html
                    ((:a :class "slotname-link" 
                      :href (simple-frame-editor-url slot)) "+")
                    "&nbsp;"
                    (html-for-slot-name slot-name))
                 (html-for-slot-name slot-name)
                 )))
           (emit-slot-name-td (slot slot-name)
             (html 
              ((:td :class "slotname") 
               (html-for-slotname-link slot slot-name))
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
       (if (null list2)
           (loop
            for slot in list1
            as slot-name = (limited-slot-name slot)
            do 
            (html
             (:tr
              (emit-slot-name-td slot slot-name)
              (emit-slot-value-td 
               "slotvalue" (slotv frame slot) "hidden-string-left")
              )))
         ;; two column format
         (loop
          for count from 0
          for slots1 = list1 then (cdr slots1)
          for slots2 = list2 then (cdr slots2)
          while (or slots1 slots2)
          as slot1 = (first slots1)
          as slot2 = (first slots2)
          as slot-name1 = (limited-slot-name slot1)
          as slot-name2 = (and slot2 (limited-slot-name slot2))
          as value1 = (slotv frame slot1)
          as value2 = (and slot2 (slotv frame slot2))
          do 
          (html
           (:tr
            (emit-slot-name-td slot1 slot-name1) 
            (emit-slot-value-td "slotvalue" value1 "hidden-string-left")
            (when slot2 
              (emit-slot-name-td slot2 slot-name2)
              (emit-slot-value-td "slotvalue" value2 "hidden-string-right")
              ))))))
      :newline :br
      ;; If we are just presenting the frame, with no edit box,
      ;; Present user with a form to add slot
      (unless *edit-slot* 
        (html
         ((:form :method "get" :action "frame-editor")
          (html-for-basic-form-hidden-variables frame)
          (html-for-add-slot)
          (maybe-emit-frame-editor-error-message)
          )))))))

;;; Retransmit the basic state information via the form submit
;;; using hidden values.
(defun html-for-basic-form-hidden-variables (frame)
  (html
   (hidden "PKG" (string wb::*sessionid*))
   :newline
   (hidden-list "noshow" *noshow-slot-names*)
   :newline
   (hidden "name" (#^fname frame))
   :newline
   (hidden "delimiter" (string *default-fesn-delimiter*))
   :newline
   (when *parent-key* 
     (hidden "pkey" (formatn "~D" *parent-key*)))
   :newline
   ))

(defun html-for-add-slot ()
  (html
   (:i "&nbsp;&nbsp;&nbsp;&nbsp; Add a slot: ")
   ((:input :type "text" :name "addaslot" :class "addaslot"))
   "&nbsp;&nbsp;"
   ((:input :type "submit" :value "Add"))
   ))

(defun maybe-emit-frame-editor-error-message ()
  (when *frame-editor-error-message* 
    (html 
     ((:span :class "error-message") "*** Error: "
      (:princ-safe *frame-editor-error-message*)
      " ***"
      ))))


;;; Display the edit box, along with SUBMIT and CANCEL buttons and
;;; radio buttons for the box to select how what the user types in is
;;; to be interpreted for the box.

(defun html-for-edit-frame-edit-slot (frame)
  (html 
   (html-for-basic-form-hidden-variables frame)
   (maybe-emit-frame-editor-error-message)
   :br
   :newline
   (hidden "editedslot" *edit-slot-name*)
   (let ((val (slotv frame *edit-slot*)))
     (html 
      :newline
      ((:div :class "editbox")
       :newline
       ((:div :class "editbox-label") 
        (:b (:princ-safe (string-capitalize (#^fname *edit-slot*))))
        (html-for-edit-box-controls val)
        )
       (html-for-edit-box val)
       )))
   ;; workaround for " " --> "+" bug as last parameter
   (hidden "dummy" "dummy")
   ))
          
(defun hidden-list (name list) 
  (when list 
    (hidden name (string-join list *default-fesn-delimiter*))
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric frame-slots-to-show (frame)
  (:documentation "Returns list of slots to be shown"))
  
(defmethod frame-slots-to-show ((frame t))
  (let ((all-slots (frame-slots-of frame)))
    (flet ((names-to-slots (names)
             (mapcan 
              (lambda (x) 
                (vwhen 
                    (fs (member x all-slots :test 'string-equal :key #^fname)) 
                  (list (first fs))
                  ))
              names
              )))
      (let ((noshow-frames (names-to-slots *noshow-slot-names*)))
        (pushnew #$fname noshow-frames)
        (let ((remaining-frames (set-difference all-slots noshow-frames)))
          (setq remaining-frames 
                (remove-if 
                 (lambda (x) (hide-from-frame-viewer? x frame))
                 remaining-frames
                 ))
          (sort remaining-frames 'string-lessp :key #^fname)
          )))))

;; Figure out if the #^hide-from-frame-viewer slot of the 
;; slot of the frame being displayed says to display the slot and
;; its value or not.
;; If the slot value is T, don't display it.  
;; If the slot value is NIL or doesn't exist, display it.  
;; If the slot value is a symbol, it is assumed to be a frame type. 
;;   If the frame being displayed is of that type, don't display the slot.
;; If the slot value is a list, it is assumed to be a list of frame types.
;; If the frame being displayed is any of these types, don't display the slot.
;; If the slot value is a lambda expression, then call the lambda expression
;;  on the slot and the frame.  
(defun hide-from-frame-viewer? (slot frame)
  (let ((value (#^hide-from-frame-viewer slot)))
    (cond
     ((null value) nil)
     ((eq value t) t)
     ((symbolp value) 
      (ignore-errors (typep frame value)))
     ((listp value) 
      (if (eq 'lambda (first value)) 
          (ignore-errors 
            #+:allegro
            (funcall value slot frame)
            #-:allegro
            (funcall (eval value) slot frame)
            )
        (block exit
          (ignore-errors 
            (loop for type in value 
                  do 
                  (when (typep frame type) (return-from exit t))
                  finally
                  (return nil)
                  )))))
     (t nil)
     )))
                
                
   

;; don't use #+:sframes because need this file to be binary compatible
(when (member :sframes *features*)
  (eval 
   '(defmethod de::frame-slots-to-show ((frame bio::seed-gene))
      (sort 
       (append 
        '(#$description #$genetic-name #$subsystem-role)
        (call-next-method))
       'string-lessp :key #^fname
       ))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Utilities

(defun url-slots-to-slots-list (url-slots)
  (when url-slots (string-split url-slots *default-fesn-delimiter*)))

(defun slots-to-url-slots-list (slots-list)
  (string-join 
   (mapcar 'fname slots-list)
   *default-fesn-delimiter*
   ))

(defun limited-slot-name 
       (slot &optional (length *frame-editor-slot-name-length*))
  (limited-string (string-capitalize (#^fname slot)) length))









