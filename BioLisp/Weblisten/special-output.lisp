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


(defgeneric in-record-to-html (form string &key pkg)
  (:documentation
   #.(one-string-nl
      "Called to transform what is stored in each IN-HISTORY record (i.e., "
      "the forms the user has typed in) into HTML which is then sent back"
      "to the browser.")))

(defmethod in-record-to-html ((form t) (string string) &key (pkg nil))
  (princ-with-frame-links string :pkg pkg))
		    

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		     

;;; OUTPUT FORMS

;;; Calling PRINC-WITH-FRAME-LINKS should probably be conditional
;;; on something.  There is no reason the frame system has to be
;;; 'enabled' in all applications.


(defun princ-with-frame-links 
       (string &key (pkg nil) (remove-frame-prefix? nil))
  (declare (ignorable pkg))
  (do ((frame-start 
        (or (search "#&" string) (search "#$" string))
        (or (search "#&" string :start2 finger) 
            (search "#$" string :start2 finger)))
       (finger 0)
       fname
       frame)
      ((null frame-start)
       (html (:princ-safe (subseq string finger))))
    (html (:princ-safe (subseq string finger frame-start)))
    (setq fname 
          ;; The :index takes a PLACE, so FINGER actually gets modified.
          (with-input-from-string 
              (stream string :start (+ frame-start 2) :index finger)
            (frames:read-fname stream)))
    (when fname 
      (setq frame 
	    (handler-case
                (frames:frame-fnamed fname)
              (error nil) ; Drops through the next clause blankly
              )))
    (if frame
	(html
	 ((:a :href 
           (s+ "/" (forward-package-funcall :de :simple-frame-editor-url frame))
           :target "_blank"
           )
          (unless remove-frame-prefix?
            (html (:princ-safe (frames::frame-print-prefix frame))))
	  (:princ-safe fname)))
      (html "#$" (:princ-safe fname)))))

(defun string-without-frame-prefixes (string)
  (with-output-to-string (s) 
    (do ((frame-start 
          (or (search "#&" string) (search "#$" string))
          (or (search "#&" string :start2 finger) 
              (search "#$" string :start2 finger)))
         (finger 0)
         fname
         frame)
        ((null frame-start)
         (format s "~A" (subseq string finger))
         )
      (format s "~A" (subseq string finger frame-start))
      (setq fname 
            ;; The :index takes a PLACE, so FINGER actually gets modified.
            (with-input-from-string 
                (stream string :start (+ frame-start 2) :index finger)
              (frames:read-fname stream)))
      (when fname 
        (setq frame 
              (handler-case
                  (frames:frame-fnamed fname)
                (error nil) ; Drops through the next clause blankly
                )))
      (if frame
          (format s "~A" fname)
        (format s "#$~A" fname)
        ))))


(defgeneric out-record-to-html (form string &key pkg)
  (:documentation
   #.(one-string-nl
      "Called to transform what is stored in each OUT-HISTORY record (i.e., "
      "the result of evaluating the forms the user has typed in) into HTML"
      "which is then sent back to the browser.  By creating a new type with"
      "DEFSTRUCT or DEFCLASS and creating a function which returns an object "
      "of that type, one can gain complete control (subject to the limitations"
      "of HTML) over how any object is displayed.  FORM is the actual"
      "result of evaluation, while STRING is FORM converted to a string"
      "representation.")))


;;; The default method creates hyperlinks to the frame browser for any 
;;; frame-like patterns it finds in STRING.

(defmethod out-record-to-html ((form t) (string string) &key (pkg nil))
  (princ-with-frame-links string
   :pkg pkg))

(defmethod out-record-to-html :around ((form t) (string string) &rest ignore)
  (declare (ignore ignore))
  (handler-case
      (call-next-method)
    (error 
     (e)
     (html 
      (:p (:b "Internal error:"))
      (:p (:princ-safe 
           (formatn 
            #.(one-string-nl
               "Can't display object of type ~s:"
               "~s"
               "because of error in display code: ~a")
            (class-name (class-of form)) form e
            )))))))

;;; =====================================================
;;; Arbitrary HTML output.

(defstruct function-for-html f args)

(defmethod out-record-to-html 
           ((form function-for-html) (string string) &rest ignore)
  (apply (function-for-html-f form) (function-for-html-args form)))

(defmethod print-object ((hr function-for-html) stream)
  (format stream "#<biobike html generation object>"))

;;; =====================================================
;;; A JPG image, outputs a hyperlink to a JPG image, which causes the
;;; actual image to appear!

(defstruct jpg path click-path)

(defmethod out-record-to-html ((form jpg) (string string) &rest ignore)
  (declare (ignore ignore))
  (if (jpg-click-path form)
      (html 
       (:princ 
        (formatn "<a href=~a><img src=~a></a>" 
                 (jpg-click-path form) (jpg-path form))))
    (html ((:img :src (jpg-path form)))))
  )

;;; =====================================================
;;; An iframe just creates a frame in the listener

(defstruct iframe path)

(defmethod out-record-to-html ((form iframe) (string string) &rest ignore)
  (html (:princ (formatn "<iframe src=~s width=500 height=500></iframe>" (iframe-path form)))))

;;; Have the frame viewer have a link to be able to view the image
;;; represented by the jpg

(defmethod frames::emit-value 
           ((object jpg) &optional (print-limit nil))
  (declare (ignore print-limit))
  (html
   ((:a :href (wb::jpg-path object)) (:princ-safe "view image"))
   (:princ "&nbsp;&nbsp"))
  (call-next-method)
  )

;;; =====================================================
;;; An href anywhere you like!

(defstruct url path display-string target)

(defmethod out-record-to-html
           ((obj url) (string string) &key (pkg nil))
  (declare (ignore pkg))
  (if (url-target obj)
      (html 
       :br
       ((:a :href (url-path obj)
         :target (url-target obj) 
         ;; :height foo
         ;; :width bar
         )
        (:princ-safe (url-display-string obj))))
    (html 
     :br
     ((:a :href (url-path obj)) 
      (:princ-safe (url-display-string obj))))))

(defmethod print-object ((url url) stream)
  (let ((ds (url-display-string url))
        (path (url-path url)))
    (if wb::*vpl-executing?*
        (format stream "<Url: ~A>" (if ds ds path))
      (call-next-method)
      )))

;;; Have the frame viewer have a link to be able to go to the page
;;; represented by the URL

(defmethod frames::emit-value 
           ((object url)
            &optional 
            (print-limit nil))
  (declare (ignore print-limit))
  (html
   ((:a :href (wb::url-path object))
    (:princ-safe (wb::url-display-string object)))
   (:princ "&nbsp;&nbsp"))
  (call-next-method)
  )
    
;;; =====================================================

(defmethod out-record-to-html 
           ((form wb-unbound-error) (string string) &rest ignore)
  (declare (ignore ignore))
  (html 
   (:princ-safe 
    (formatn 
     "<<< Unbound ~a: ~s >>>~% ~A"
     (error-string form) (name form)
     (if (choices form) 
         "Maybe you meant one of these?"
       "Perhaps you misspelled it?"
       )))
   :br
   (:table
    (loop 
     for choice in (choices form)
     as symbol = (getf choice :symbol)
     as redo-form = (getf choice :redo-form)
     do 
     (html 
      (:tr
       (:td 
        (:princ "&nbsp;&nbsp;") (:princ-safe symbol) (:princ "&nbsp;&nbsp;"))
       (:td
        (:princ-safe
         (let ((fb? (fboundp symbol)) (b? (boundp symbol)))
           (cond 
            ((and fb? b?) "[f,v]")
            (fb? "[f]")
            (b? "[v]")
            (t "&nbsp;")
            )))
        (:princ "&nbsp;&nbsp;"))
       (if redo-form 
           (html-for-redo-form-options redo-form)
         (html (:td (:princ "&nbsp;")))
         )))))))


(defun html-for-redo-form-options (redo-form)
  (let ((*print-pretty* nil))
    (html 
     (:td
      ((:a :href 
        (make-weblistener-evalstring-url 
         :evalstring 
         (url-safe-string 
          (formatn "(wb::into-editbox '~s)" redo-form))))
       (:princ-safe "[show modified form]"))
      (:princ "&nbsp;&nbsp;"))
     (:td
      ((:a :href
        (make-weblistener-evalstring-url 
         :evalstring (url-safe-string (formatn "~s" redo-form))))
       (:princ-safe "[execute modified form]")
       )))))

(defun into-editbox (form)
  (setq *multiline-form-data* 
        (forward-funcall 'pretty-print-to-string form :downcase))
  t)

;;; =====================================================
;;; A file in webtmp:; outputs a hyperlink to that file.
;;; Clicking on the link causes the browser to do its normal thing
;;; with the file being pointed to.  

(defstruct webtmp-file path display-string)

(defun webtmp-url (path)
  (ecase (os?)
    (:windows (ierror "Not implemented"))
    (:unix 
     (if cl-user:*webtmp-url*
         (formatn "~A~A" cl-user:*webtmp-url* 
		  (if (char-equal #\/ (aref path 0))
		      (subseq path 1)
		    path))
       (error "Must have defined USER:WEBTMP-URL* but it is NIL")
       ))))

(defmethod out-record-to-html
           ((obj webtmp-file) (string string) &rest ignore)
  (declare (ignore ignore))
  (let ((path (webtmp-file-path obj))
        (visible-link (webtmp-file-display-string obj)))
    (html ((:a :href (webtmp-url path)) (:princ-safe visible-link)))
    ))
     


;;; =====================================================
;;; A file path to a file in the user's local directory.

(defstruct local-url path display-string)

(defmethod out-record-to-html
           ((obj local-url) (string string) &key (pkg nil))
  (declare (ignore ignore))
  (princ-with-frame-links string :pkg pkg)
  (html 
   :br
   ((:a :href (local-url-path obj)) 
    (:princ-safe (local-url-display-string obj)))))

;;; Form for interactive buttons.
;;; 

(defstruct ibuttons
  heading callback choices)

(defmethod out-record-to-html
           ((obj ibuttons) (string string) &key (pkg nil))
  (declare (ignore pkg))
  (html
   ((:table border 1 bordercolor "gray" bgcolor "white" cellpadding 3)
    (:tr
     ((:th :colspan (format nil "~a" (length (ibuttons-choices obj))))
      (:big (:princ-safe (ibuttons-heading obj)))))
    (:tr
     (loop for choice in (ibuttons-choices obj) do
	   (html 
            (:td
             ((:form :name choice
               :method "POST"
               :action *weblistener-evalstring-form-response-url*)
              ((:input :type "HIDDEN" :name "PKG"
                :value (string (user-session-id))))
              ((:input :type "HIDDEN" :name "evalstring" 
                :value (format nil "(~a ~s)" (ibuttons-callback obj) choice)))
              ((:input :type "SUBMIT" :value choice))
              ))))))))


;;; A java applet
;;; The codebase value should probably be the result of calling
;;; a method on the application.

(defstruct applet class-name width height)

(defmethod out-record-to-html ((obj applet) (string string) &key (pkg nil))
  (declare (ignore pkg))
  (html
   (:center
    ((:applet :CODEBASE "http://nostoc.stanford.edu/" 
	      :CODE (applet-class-name obj)
	      :WIDTH (applet-width obj)
	      :HEIGHT (applet-height obj))
     ((:param :NAME "pkg" :VALUE (string (user-session-id)))))
    )))



(defstruct docspec symbol furl floc vurl vloc)

(defmethod out-record-to-html ((obj docspec) (string string) &key (pkg nil))
  (declare (ignore pkg))
  (let ((symbol (docspec-symbol obj)))
    (when (docspec-floc obj)
      (html
       (:princ-safe (formatn "~S [function] : " symbol))
       ((:a :href (docspec-furl obj))
        (:princ-safe (namestring (docspec-floc obj))))))
    (when (docspec-vloc obj)
      (html
       (:princ-safe (formatn "~S [variable] : " symbol))
       #+does-not-work
       ((:a :href (docspec-vurl obj))
        (:princ-safe (namestring (docspec-vloc obj))))
       (:princ-safe (namestring (docspec-vloc obj)))
       ))))


(defun location-info (symbol)
  (when symbol  
    (let ((floc (system-specific-source symbol :function))
          (vloc (system-specific-source symbol :variable)))
      (when (or floc vloc)
        (make-docspec 
         :symbol symbol 
         :furl (and floc (function-source-url symbol))
         :floc (and floc (translate-logical-pathname floc))
         :vurl (and vloc (full-variable-url symbol))
         :vloc (and vloc (translate-logical-pathname vloc))
         )))))





