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

(defmacro with-parent-key-bindings (&body body)
  (let ((parent-key-symbol (gensym "PARENT-KEY-"))
        (page-url-symbol (gensym "PAGE-URL-"))
        (seb-symbol (gensym "SEB-")))
    `(let* ((*edit-object* object)
            (,parent-key-symbol (url-parameter-value :pkey input))
            (*parent-key* 
             (when ,parent-key-symbol (read-from-string ,parent-key-symbol)))
            (,seb-symbol (url-parameter-value :seb input))
            (,page-url-symbol 
             (if ,seb-symbol 
                 (remove-de-edit-box-params (url-from-req req))
               (url-from-req req)
               ))
            (*key-for-this-page* (enter-url-into-pid-table ,page-url-symbol)))
       ,@body
       )))

(defun remove-de-edit-box-params (url)
  (cond
   ((search "frame-editor" url)
    (remove-parameter-from-de-url url "edit"))
   ((search "sequence-editor" url)
    (remove-parameter-from-de-url 
     (remove-parameter-from-de-url url "edit-element")
     "edit-element-type"
     ))
   ((search "object-editor" url)
    (remove-parameter-from-de-url url "editedslot"))
   ((search "array-editor" url)
    (remove-parameter-from-de-url
     (remove-parameter-from-de-url
      (remove-parameter-from-de-url 
       (remove-parameter-from-de-url url "key2type")
       "key1type")
      "key2"
      )
     "key1"
     ))
   (t url)
   ))

(defun parse-edit-element-type (eet)
  (cond
   ((null eet) :form)
   ((string-equal eet "string") :string)
   ((string-equal eet "form") :form)
   ((string-equal eet "eform") :eform)
   (t (error "Impossible"))
   ))

(defun keystring-to-key (keystring type)
  (ecase type 
    (:string keystring)
    (:form (read-from-string keystring))
    (:eform (eval (read-from-string keystring)))
    ))

(defgeneric key-to-keystring-and-type (key))

(defmethod key-to-keystring-and-type ((key t))
  (values (formatn "~S" key) :form))

(defmethod key-to-keystring-and-type ((key string))
  (values key :string))

(defmethod key-to-keystring-and-type ((key package))
  (values (formatn "(find-package :~A)" (package-name key)) :eform))


(defun every-other (list)
  (values 
   (loop for elem in list by #'cddr collect elem)
   (loop for elem in (cdr list) by #'cddr collect elem)
   ))

(defun first-and-second-halves (list)
  (let* ((len (length list))
         (firstlen (ceiling len 2)))
    (values 
     (subseq list 0 firstlen)
     (subseq list firstlen)
     )))
   

(defun popup-height (textlen)
  (cond
   ((< textlen 50) 75)
   ((< textlen 300) 150)
   (t 225)
   ))

(defun nrows->classes (nrows)
  (cond
   ((<= nrows 10) (list "fsize1" "esize1"))
   ((<= nrows 14) (list "fsize2" "esize2"))
   (t (list "fsize3" "esize3"))
   ))

(defun content? (x) (and x (not (whitespacep x)) x))


(defun html-for-edit-box-controls (val)
  (html
   "&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;"
   "&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;"
   ((:input :class "edit-submit"
     :type "submit" :value "Enter" :name "editsubmit"))
   ((:input :class "edit-submit"
     :type "submit" :value "Cancel" :name "editcancel"))
   ((:span :class "interpret")
    "&nbsp;&nbsp;&nbsp; Interpret as:&nbsp;"
    (if (stringp val)
        (progn
          (radio-button "String" "interpretation" "String" t) 
          (radio-button "Form" "interpretation" "Form" nil)
          )
      (progn
        (radio-button "String" "interpretation" "String" nil)
        (radio-button "Form" "interpretation" "Form" t)
        )))
   :newline
   ))

(defun html-for-edit-box (val)
  (html
   ((:textarea :class "editarea" :name "editboxcontents" :rows 12 :cols 80)
    (:princ-safe (if val (formatn (if (stringp val) "~A" "~S") val) ""))
    )))

(defun read-from-string-or-error-text (s)
  (handler-case (read-from-string s)
    (error 
     (c)
     (formatn "*** ~A" c)
     )))

(defun hidden (name value) 
  (html 
   ((:input :type "HIDDEN" :name name :value value))
   ))

(defun radio-button (label name value checked?)
  (when label (html (:princ-safe label)))
  (if checked?
      (html ((:input :type "radio" :name name :value value :checked "yes")))
    (html ((:input :type "radio" :name name :value value)))
    ))
   
(defun a-or-an? (s) (if (vowel? (char s 0)) "an" "a"))

(defun vowel? (char) 
  (member char '(#\a #\e #\i #\o #\u) :test 'char-equal))

(defun sort-as-number-strings-and-goo (list &key (string-test 'string-lessp))
  (let ((numbers nil) (strings nil) (goo nil))
    (loop for elem in list
          do
          (typecase elem
            (complex (push (list (formatn "~S" elem) elem) goo))
            (number (push (list elem elem) numbers))
            ((or string symbol) (push (list (string elem) elem) strings))
            (t (push (list (formatn "~S" elem) elem) goo))
            ))
    (mapcar 
     'second
     (nconc 
      (sort numbers '< :key 'first)
      (sort strings string-test :key 'first)
      (sort goo string-test :key 'first)
      ))))

(defun garray-enum-axis-possibles (garray axis)
  (utils::axis-possibles (nth axis (garray-axes garray))))

(defun enum-from-index (garray axis index)
  (let ((axis-structure (nth axis (garray-axes garray))))
    (typecase axis-structure
      (utils::enum-axis 
       (elt (utils::axis-possibles axis-structure) (- index *user-base*)))
      (otherwise (error "Internal enum-from-index error!"))
      )))
      
(defun log-data-editor-use (format-string &rest format-args)
  (when (find-package :wb)
    (apply 'forward-package-funcall 
           :Wb :log-user-event (cons format-string format-args)
           )))

;;; Include the associated CSS file.

(defun html-for-data-editor-head (css-file title &key (req nil))
  (html 
   (:head 
    :newline
    :newline
    (:princ
     (formatn
      (s+
       "<link rel=\"stylesheet\" "
       "type=\"text/css\" "
       "href=\"weblistenerdocs/css/~A.css\" "
       ">"
       ;; adding redirect for edit box to prevent refresh from
       ;; reexecuting the edit instructions
       (if req 
           (s+ 
            "<script type=\"text/javascript\">" wb::nl
            "function locate()" wb::nl
            "{" wb::nl
            (format nil "location=~S" (redirect-to-pure-url req)) wb::nl
            "}" wb::nl
            "</script>" wb::nl
            )
         "")
       ;; end add of redirect
       "<title>~A</title>"
       )
      css-file (data-editor-page-title title)
      ))
    :newline
    )))

(defun data-editor-page-title (title)
  (let ((machine-name (wb::useful-machine-name-for-title)))
    (one-string
     (if (plusp (length machine-name)) (subseq machine-name 0 1) "")
     " "
     (formatn "~D" wb::*current-weblistener-port*)
     " "
     title
     )))

(defun redirect-to-pure-url (req)
  (let ((input 
         (remove-if
          (lambda (x) 
            (member 
             x
             '("editsubmit" 
               "editedslot" "editcancel" "delimiter" "edit-element"
               "interpretation" "editboxcontents" "dummy"
               "key1" "key2" "key1type" "key2type" "edit")
             :test 'string-equal))
          (net.aserve::request-query-alist req)
          :key 'first 
          ))
        (page (net.aserve::request-decoded-uri-path req)))
    (s+ 
     page "?"
     (string-join 
      (loop for (slot . value) in input collect 
            (s+ slot "=" value))
      "&"
      ))))

(defun insert-redirect-code ()
  (html 
   ((:script :type "text/javascript")
    (:princ "locate();")
    )))

(defun html-for-generic-navigator-url (type enable? url-generator)
  (flet ((html-for-text () 
           (html 
            (:princ-safe (string-capitalize (symbol-name type)))
            )))
    (if enable?
        (html
         ((:a :href (funcall url-generator type) 
           :class "sequence-navigator-button")
          (html-for-text))
         "&nbsp;"
         )
      (html ((:span :class "sequence-navigator-disabled") 
             (html-for-text) "&nbsp;"))
      )))

(defun html-for-enter-frame-box ()
  (html
   ((:form :method "get" :action "frame-editor")
    (hidden "PKG" (string wb::*sessionid*))
    "Go to new frame: &nbsp;"
    ((:input :type "text" :name "gotonewframe" :class "addaslot"))
    "&nbsp;&nbsp;"
    )))

(defun url-from-req (req)
  (second (string-split (request-raw-request req) #\Space)))

(defun parent-url (pkey)
  (let ((url (first (url-from-pid-table pkey))))
    (unless url (error "internal error: No entry for pkey ~D" pkey))
    url
    ))

(defun remove-parameter-from-de-url (url parameter)
  (let ((search-string (s+ parameter "=")))
    (let ((startpos (search search-string url :test 'string-equal)))
      (if (null startpos) 
          url
        (let ((endpos (position #\& url :start startpos)))
          (if (null endpos)
              (subseq url 0 (1- startpos))
            (s+ (subseq url 0 startpos) (subseq url (1+ endpos)))
            ))))))
        
(defun root-url-from-parent-key (parent-key)
  (let* ((pkey-search "pkey=")
         (plen (length pkey-search)))
    (when parent-key 
      (handler-case
          (let* ((parent-url (parent-url parent-key))
                 (pos (search pkey-search parent-url)))
            (cond
             (pos 
              (let* ((s (subseq parent-url (+ pos plen)))
                     (pkey (read-from-string (first (string-split s #\&)))))
                (root-url-from-parent-key pkey)
                ))
             (t parent-url)
             ))
        (error () nil)
        ))))
            
(defun root-link-generator ()
  (html-for-generic-navigator-url 
   :root *parent-key*
   (lambda (type) 
     (declare (ignore type))
     (let ((root-url (root-url-from-parent-key *parent-key*)))
       (or root-url "")
       ))))
       

                                 