;;; -*- mode: Lisp; Syntax: Common-Lisp; Package: WEBLISTENER; -*-

(in-package :weblistener)

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

;;; Author:  JP Massar 

;;; Display the recomputed page, with all the history 
;;; and the text input boxes.  This gets called each and every time
;;; the user does anything and recalculates/regenerates the entire page
;;; each and every time.  Inefficient?  Yes.  Does it matter?  Not clear.

(publish
 :path *redisplay-url*
 :content-type cl-user::*html-publish-content-type*
 :function
 (lambda (req ent)
     
   ;; Determine the package the visitor is operating in.
   ;; The property list of the name of the package contains
   ;; the visitor's parameters.
     
   (let* ((input (request-query req))
          (package-name (url-parameter-value :pkg input))
          (package-symbol (keywordize package-name))
          (*log-dynamic-webpage-accesses* nil))
       
     ;; Bind all Common Lisp globals and the visitor's weblistener data.
     ;; (All we really need to compute the new page is the visitor's session
     ;; data -- i.e., the history lists and the values of the text input)
       
     (execute-with-standard-weblistener-environment
      req ent package-symbol
      (lambda () (weblistener-redisplay-function))
      ))))


;;; Gets triggered when the user hits the CLEAR button.
;;; Crude, but effective.  Clear the variables that specify the 
;;; values for the input text areas, then redisplay everything.

(publish
 :path *clearhack-url*
 :content-type cl-user::*html-publish-content-type*
 :function
 (lambda (req ent)
   (let* ((input (request-query req))
          (pkgname (url-parameter-value :pkg input))
          (package-symbol (keywordize pkgname)))
     (execute-with-standard-weblistener-environment
      req ent package-symbol
      (lambda ()
       (setq *oneline-form-data* "")
       (setq *multiline-form-data* "")
       (html 
        (:princ 
         (indirect-to-redisplay (incf *user-display-id*) (user-session-id))))
         )))))


(defun reformat-multiline-text (req ent package-name package-symbol evalstring)
  (with-protected-globals-bound
      package-symbol
    (let ((mltext evalstring))
      (setq *multiline-form-data* (pretty-indent-lisp-form-string mltext))
      (with-http-response-and-body (req ent)
        (html 
         (:princ 
          (indirect-to-redisplay (incf *user-display-id*) package-name)
          ))))))

(defun useful-machine-name-for-title ()
  (let* ((host cl-user:*host-machine-apache-url*)
         (http-prefix "http://")
         (prefix-length (length http-prefix)))
    (cond 
     ((null host) "")
     ((initial-subsequence-of? host http-prefix)
      (let ((dotpos (position #\. host)))
        (if (null dotpos)
            ""
          (string-capitalize (subseq host prefix-length dotpos))
          )))
     ((initial-subsequence-of? host "FILE:") "Localhost")
     (t "Unknown")
     )))

(defun machine-name-letter-for-title ()
  (let ((machine-name (useful-machine-name-for-title)))
    (if (plusp (length machine-name)) (subseq machine-name 0 1) "")
    ))

(defun useful-weblistener-title ()
  (let ((program-name (weblistener-redisplay-page-title cl-user:*ai*))
        (machine-name-letter (machine-name-letter-for-title))
        (port (formatn "~D" wb::*current-weblistener-port*))
        (pid
         (if (boundp '*weblistener-pid*) 
             (formatn " ~A " (symbol-value '*weblistener-pid*))
           ""
           ))
        (username (if *username* (formatn "~A" *username*) "") )
        (sessionid (if *sessionid* (sessionid->title-display *sessionid*) ""))
        )
    (one-string 
     machine-name-letter
     " "
     port 
     " "
     program-name
     " "
     pid
     " "
     username
     " "
     sessionid
     )))

(defun sessionid->title-display (id)
  (formatn 
   "~D:~D" 
   (subseq (string id)
           (position-if 'digit-char-p (string id)))
   (get id :session-number)
   ))


(defun weblistener-redisplay-function ()
  ;; Compute the new page.
  (html
   (:html
    (:head
     (:title (:princ-safe (useful-weblistener-title)))
     :newline
     (:princ +javascript-newminieditor-include-string+)
     :newline
     (:princ (user-specific-javascript-arglist-include-string wb:*username*))
     :newline
     ;; A horrible hack to make the CLEAR button actually clear 
     ;; the text input fields, instead of resetting them to their 
     ;; initial values.
     (:princ (indirect-to-function "clearhack" (make-clearhack-url)))
     ;; Javascript for what the Clear History button triggers.
     (:princ (indirect-to-function "clrhist" (make-clear-history-url)))
     (:princ (indirect-to-function "mlreformat" (make-ml-reformat-url)))
     (:princ (forward-funcall 'vpl-style))		
     (:title (weblistener-redisplay-page-title cl-user:*ai*)))
    ;; Put the focus on the single line input box.
    ;; This doesn't seem to work in IE.  Grumble, grumble.
    ((:body :onload (weblistener-onload-action cl-user:*ai*))
     ;; Yet another hack to force the display to always appear
     ;; such that the input boxes are at the bottom of the window.
     ;; (Simply put in enough dummy lines so the window is filled.)
     (:pre 
      (dummy-line-padding)
      ;; All the user's forms and the results of
      ;; evaluating them.
      (user-login-messages)
      (user-interactions-history-2 cl-user::*ai*)
      (output-announcements cl-user:*ai*)
      )
     :newline
     (weblistener-input-forms cl-user:*ai*)
     :newline
     (html-for-weblistener-links cl-user:*ai*)
     :newline
     ;; The anchor tag that allows us to force the redisplay
     ;; mechanism to show the bottom of the page, not the top.
     ((:a :name "TAG") (html-for-weblistener-title cl-user:*ai*))
     :newline
     (:princ (forward-funcall 'vpl-style-post))
     ))))

;;; These get overwritten by VPL code
#+causes-redefinition-warning
(defun vpl-style () "")
#+causes-redefinition-warning
(defun vpl-style-post () "")

;;; So the compiler doesn't issue redefinition warnings, I hope.
(setf (symbol-function 'vpl-style) (lambda () ""))
(setf (symbol-function 'vpl-style-post) (lambda () ""))

(defmethod weblistener-redisplay-page-title ((app t))
  "WebListener v4.0")

(defmethod weblistener-onload-action ((app t))
  "document.onelineform.evalstring.focus();")

(defparameter *min-multiline-text-nrows* 8)
(defparameter *max-multiline-text-nrows* 32)

(defmethod weblistener-input-forms ((app t))
  (let* ((pkgname (string (user-session-id)))
         (linecount (count #\Newline *multiline-form-data*))
         (multiline-nrows 
          (max *min-multiline-text-nrows* 
               (min *max-multiline-text-nrows* linecount))))
    (html
     ;; The one line text input box.
     ((:form :name "onelineform" :method "POST"
       :action *weblistener-evalstring-form-response-url*)
      :newline
      ((:input :type "text" :name "evalstring" 
        :value *oneline-form-data* :size 85
        :onkeyup "minieditorkeyup(event, this, window.document.multilineform)"
        ))
      :newline
      ((:input :type "SUBMIT" :name "data" :value "[Enter]"))
      :newline
      ((:input :type "HIDDEN" :name "formid" :value "wb-oneline"))
      :newline
      ((:input :type "HIDDEN" :name "PKG" :value pkgname)))
     :newline
     ;; The multiline text box and its associated buttons,
     ;; along with the paren-matching display.
     ((:form :name "multilineform" :method "POST"
       :action *weblistener-evalstring-form-response-url*)
      :newline
      ((:textarea :name "evalstring" :rows multiline-nrows :cols 80
        :onkeyup "minieditorkeyup(event, this, window.document.multilineform)"
        :onmouseup "minieditorkeyup(event, this, window.document.multilineform)"
        )
       (:princ-safe *multiline-form-data*))
      :br
      :newline
      ((:input :type "SUBMIT" :name "data" :value "Eval"))
      :newline
      ((:input :type "SUBMIT" :name "mlreformat" :value "Reindent"))
      :newline
      " Info: " 
      :newline
      ((:input :type "text" Name "closer" 
        :size 48 :value (one-string "Package: " (package-name *package*))))
      :newline
      ((:input :type "BUTTON" :name "hack" :value "Clear"
        :OnClick "clearhack();"))
      :newline
      ((:input :type "BUTTON" :name "fred" :value "Clear History"
        :OnClick "clrhist();"))
      :newline
      ((:input :type "HIDDEN" :name "formid" :value "wb-multiline"))
      :newline
      ((:input :type "HIDDEN" :name "PKG" :value pkgname))
      :newline
      ))))

(defun dummy-line-padding ()
  (let* ((hlen (length *in-history*))
         (newline-count (- *enough-dummy-lines* (* 2 hlen))))
    (when (plusp newline-count)
      (html 
       (:princ 
        (make-string newline-count :initial-element #\Newline)
        )))))


(defun user-login-messages ()
  (when (= 0 *user-display-id*)
    (loop for message in (reverse *login-messages*) do
          (html (:princ-safe (formatn "~A~%" message)))))
  (html
   (:princ "     ")
   (:princ nl)
   ))


(defun user-interactions-history (pkgname)

  ;; For each input/output pair stored in the user's history list

  (loop 

   with output-log = nil
   with app = cl-user::*ai*
   for in-record in (reverse *in-history*)
   for out-record in (reverse *out-history*)
   as history-index = (inhist-index in-record)
   as mode = (out-record-output-mode out-record)
   as in-string = (inhist-string in-record)
   as in-form = (inhist-form in-record)
   as printout-string = (outhist-printout out-record)
   as value-strings = (outhist-strings out-record)
   as values-returned = (outhist-forms out-record)
   as logged? = (inhist-logged? in-record)
   as output-marker = (weblistener-output-marker app history-index)
   as input-marker = (weblistener-input-marker app history-index)
   do
   
   (setq
    output-log
    (with-output-to-string (log) 

      (flet ((output-prompt (nl?)
               (let ((marker output-marker))
                 (unless logged? 
                   (format log "~A" (one-string marker " " (if nl? nl ""))))
                 (html
                  ((:a :href (out-history-url nil history-index 0 pkgname))
                   (:princ-safe marker))
                  (:princ " ")
                  (when nl? (html (:princ nl))))))
             (output-value-prompt (vindex)
               (unless logged?
                 (format log "~A" (one-string ">" " ")))
               (html
                ((:a :href (out-history-url nil history-index vindex pkgname))
                 (:princ-safe ">"))
                (:princ " ")))
             (output-form (form string)
               (unless logged? 
                 (format log "~A" (one-string string nl)))
	       (html
		(out-record-to-html form string :pkg pkgname)
		(:princ nl)))
             (output-nl ()
               (unless logged? (terpri log))
               (html (:princ nl))
               )
             (output-input-record ()
               ;; If it's a new input record log it.
               (unless logged?
                 (log-user-event 
                  (one-string
                   nl 
                   (weblistener-input-marker 
                    app history-index) " " in-string nl)))
               (html
                ;; display the input string and a link to reference it.
                ((:a :href (in-history-url nil history-index pkgname))
                 (:princ-safe input-marker))
                (:princ " ")
                (in-record-to-html in-form in-string :pkg pkgname)
                (:princ nl)
                )))
        
        (output-input-record)

        (unless logged? (terpri log))

        ;; display the output as a function of whether there was
        ;; printout before the value(s) were returned, and how many
        ;; values were returned.

        (ecase mode
          (:single-value-no-printout
           (output-prompt nil)
           (output-form (first values-returned) (first value-strings)))
          (:single-value-printout
           (output-prompt t)
           (out-record-to-html nil printout-string :pkg pkgname)
           (output-nl)
           (output-value-prompt 1)
           (output-form (first values-returned) (first value-strings)))
          ((:multiple-values-no-printout :multiple-values-printout)
           (output-prompt t)
           (when (eq mode :multiple-values-printout)
             (out-record-to-html nil printout-string :pkg pkgname)
             (output-nl))
           (loop for k from 1 
                 for val in values-returned 
                 for string in value-strings 
                 do
                 (output-value-prompt k)
                 (output-form val string)
                 )))

        )))

   (unless logged? (log-user-event output-log))
   (setf (inhist-logged? in-record) t)
     
   ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *log-string-stream*)

(defmethod user-interactions-history-2 ((app t))

  ;; For each input/output pair stored in the user's history list
  ;; generate HTML for the weblistener window for it and if this is
  ;; the newest interaction log the input/output as well.

  (loop for in-record in (reverse *in-history*)
        for out-record in (reverse *out-history*)
        ;; as logged? = (inhist-logged? in-record)
        do
        (flet ((output-user-interaction (output-sink)
                 (output-input-record app output-sink in-record)
                 (output-output-record app output-sink out-record)
                 ))
          (output-user-interaction :html)
          )))

(defmethod output-input-record ((app t) (sink (eql :html)) in-record)
  (let* ((history-index (inhist-index in-record))
         (in-form (inhist-form in-record))
         (in-string (inhist-string in-record))
         (input-marker (weblistener-input-marker app history-index)))
    (html
     ;; display the input string and a link to reference it.
     ((:a :href (in-history-url nil history-index *sessionid*))
      (:princ-safe input-marker))
     (:princ " ")
     ;; This puts links in wherever it finds #$ notation.
     (in-record-to-html in-form in-string :pkg *sessionid*)
     (:princ nl)
     )))

(defmethod output-input-record ((app t) (sink (eql :log)) in-record)
  (let* ((history-index (inhist-index in-record))
         (in-string (inhist-string in-record)))
    (log-user-event 
     "~A"
     (one-string
      nl (weblistener-input-marker app history-index) " " in-string nl)
     )))

(defmethod output-output-record ((app t) (sink (eql :html)) out-record)
  (output-output-record-internal app sink out-record))

(defmethod output-output-record ((app t) (sink (eql :log)) out-record)
  (let ((log-record
         (with-output-to-string (*log-string-stream*)
           (terpri *log-string-stream*)
           (output-output-record-internal app sink out-record)
           )))
    (log-user-event "~A" log-record)
    ))

;; so output routines can access their own history index
(defvar *history-index* nil) 

(defun output-output-record-internal (app sink out-record)
  ;; display the output as a function of whether there was
  ;; printout before the value(s) were returned, and how many
  ;; values were returned.
  (let ((mode (out-record-output-mode out-record))
	(*history-index* (outhist-index out-record)))
    (ecase mode
      (:single-value-no-printout
       (output-output-prompt app sink out-record nil)
       (output-output-form app sink out-record 1))
      (:single-value-printout
       (output-output-prompt app sink out-record t)
       (output-printout-string app sink out-record)
       (output-value-prompt app sink out-record 1)
       (output-output-form app sink out-record 1))
      ((:multiple-values-no-printout :multiple-values-printout)
       (output-output-prompt app sink out-record t)
       (when (eq mode :multiple-values-printout)
         (output-printout-string app sink out-record))
       (loop for k from 1 to (length (outhist-forms out-record)) do
             (output-value-prompt app sink out-record k)
             (output-output-form app sink out-record k)
             )))))

(defmethod output-output-prompt 
           ((app t) (sink (eql :html)) out-record nl?)
  (let* ((history-index (outhist-index out-record))
         (output-marker (weblistener-output-marker app history-index)))
    (html
     ((:a :href (out-history-url nil history-index 0 *sessionid*))
      (:princ-safe output-marker))
     (:princ " ") 
     (when nl? (html (:princ nl)))
     )))

(defmethod output-output-prompt 
           ((app t) (sink (eql :log)) out-record nl?)
  (let* ((history-index (outhist-index out-record))
         (output-marker (weblistener-output-marker app history-index)))
    (format *log-string-stream* 
            "~A" (one-string output-marker " " (if nl? nl ""))
            )))
      

(defmethod output-value-prompt ((app t) (sink (eql :html)) out-record vindex)
  (let ((history-index (outhist-index out-record)))
    (html
     ((:a :href (out-history-url nil history-index vindex *sessionid*))
      (:princ-safe ">"))
     (:princ " ")
     )))

(defmethod output-value-prompt ((app t) (sink (eql :log)) out-record vindex)
  (declare (ignore out-record vindex))
  (format *log-string-stream* "~A" (one-string ">" " "))
  )


(defmethod output-output-form ((app t) (sink (eql :html)) out-record vindex)
  (let ((form (nth (1- vindex) (outhist-forms out-record)))
        (string (nth (1- vindex) (outhist-strings out-record))))
    (html
     (out-record-to-html form string :pkg *sessionid*)
     (:princ nl)
     )))

(defmethod output-output-form ((app t) (sink (eql :log)) out-record vindex)
  (let ((string (nth (1- vindex) (outhist-strings out-record))))
    (format *log-string-stream* "~A~%" string)
    ))

(defmethod output-printout-string ((app t) (sink (eql :html)) out-record)
  (let ((printout-string (outhist-printout out-record)))
    (out-record-to-html nil printout-string :pkg *sessionid*)
    (html (:princ nl))
    ))

(defmethod output-printout-string ((app t) (sink (eql :log)) out-record)
  (let ((printout-string (outhist-printout out-record)))
    (format *log-string-stream* "~A~%" printout-string)
    ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun out-record-output-mode (out-record)
  (let* ((printout-string (outhist-printout out-record))
         (forms (outhist-forms out-record))
         (no-printout? (zerop (length printout-string)))
         (one-value? (eql 1 (length forms))))
    (if one-value?
        (if no-printout?
            :single-value-no-printout
          :single-value-printout)
      (if no-printout?
          :multiple-values-no-printout
        :multiple-values-printout
        ))))

(defmethod weblistener-input-marker ((app t) (history-number t))
  (formatn "<~D>>" history-number))

(defmethod weblistener-output-marker ((app t) (history-number t))
  "::"
  )

