;;; -*- mode: Lisp; Syntax: Common-Lisp; Package: weblistener; -*-

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

;;; Author:  JP Massar.

;;;;;;;;;;;;;;;;;;; UPLOAD FACILITY


(defparameter *weblistener-upload-limit* 50000000)

(publish
 :path *upload-form-url*
 :content-type (s+ cl-user::*html-publish-content-type* "; charset=utf-8")
 :function 
 (lambda (req ent)
   (let* ((input (request-query req))
          (pkgname (url-parameter-value :pkg input))
          (package-symbol (keywordize pkgname))
          )
     (execute-with-standard-weblistener-environment
      req ent package-symbol
      (lambda () (getfile-function pkgname)))
     )))


;;; The page with the form on it to do an upload.

(defun getfile-function (pkgname)
  (with-standard-weblistener-page-header
      ("Weblistener Upload Facility" :listener? nil)
    (html
     (:body
      ((:form :enctype "multipart/form-data"
        :method "post"
        :action *upload-form-response-url*)
       ((:font :color "green")
        (:i 
         "If you want to upload a very large file (more than one million "
         "characters, say) we suggest you use ftp, scp, or similar, as "
         "it will probably be faster and less subject to failure."))
       :p
       (:big "Full pathname of file to be uploaded: ")
       :br 
       "(be sure to use backslash, '\\', not slash, '/',"
       " if you are using Windows)"
       :br :br
       ((:input :type "file" :name "thefile" :value "*.txt"
         :size 50))
       :br :br
       (:big "Save it as: ")
       "(Default filename is the same as the uploaded file. "
       (:princ-safe 
        (formatn "Default directory is ~A)" (visitor-directory *username*)))
       :br :br
       ((:input :type "text" :name "storepathname" :size 50 :value ""))
       :br :br
       ((:input :type "checkbox" :name "convert" :value "yes" :checked '||)
        (:princ-safe "Convert Return\\Newline -> Newline?"))
       :br :br
       ((:input :type "checkbox" :name "makeloadable" :value "yes")
        (:princ-safe "Make the data available via (ensure-table ...)?"))
       :br :br
       (:princ-safe "For loadable tables, you must provide a variable name into which the data will be loaded:")
       :br
       ((:input :type "text" :name "loadvarname" :size 30))
       :br :br
       ((:input :type "submit" :value "Upload it"))
       ((:input :type "HIDDEN" :name "pkg" :value pkgname))
       )))))


;;; Handler for the form data that comes back from the Biolisp Upload form.
;;; Based loosely on code found in the Allegro 6.2 examples/aserve/examples.cl
;;; file.

(publish 
 :path *upload-form-response-url*
 :content-type (s+ cl-user::*html-publish-content-type* "; charset=utf-8")
 :function
 (lambda (req ent)
   (with-internal-weblistener-errors-caught 
       (req)
     (with-http-response
      (req ent)
      (let ((input-filename nil)
            (contents nil)
            (other-form-data nil)
            (overlimit? nil))
            
        ;; Obtain successive pieces of the form response via 
        ;; GET-MULTIPART-HEADER.
        ;; Get the output file name if the user provided us with one
        ;; via the :data piece of the form response, and the input file name 
        ;; and the contents of that file via the :file piece of the form
        ;; response.

        (loop
         (multiple-value-bind (kind name filename content-type)
             (parse-multipart-header (get-multipart-header req))
           (declare (ignore content-type))
           ;;(format t ";; Kind = ~A~%" kind)
           (case kind
             (:eof (return)) 
             (:data
              ;;(format t ";; DATA, NAME=~A~%" name)
              (push (cons name (get-all-multipart-data req)) other-form-data))
             (:file
              (setq input-filename filename)
              ;;(format t ";; FILE: ~A~%" filename)
              (setq contents 
                    (get-all-multipart-data 
                     req :type :binary :limit *weblistener-upload-limit*))
              ;;(format t ";; File size: ~D~%" (length contents))
              (when (eq contents :limit) 
                (setq overlimit? t) 
                ;;(return)
                ))
             ;; all else ignore but read to next header
             (t 
              (get-all-multipart-data req :limit 1000)
              ))))
			  
        ;; (format t ";; OTHER-FORM-DATA = ~A~%" other-form-data)

        ;; now send back a response for the browser (and if everything is OK,
        ;; write out the uploaded file). Include a link back to biolistener.
	       
        (let* ((pkgname (url-parameter-value :pkg other-form-data))
               (convert? (url-parameter-value :convert other-form-data))
               (make-loadable?
                (url-parameter-value :makeloadable  other-form-data))
               (loadvarname (url-parameter-value :loadvarname other-form-data))
               (package-symbol (keywordize pkgname)))
          (with-protected-globals-bound
              package-symbol
            (with-http-body 
             (req ent :external-format :utf8-base)
             (html 
              (:html (:head (:title "Weblistener upload report"))
               (:body 
                :br
                ((:font :size 5) "Weblistener upload report")
                :br :br
                (weblistener-upload-report-body
                 input-filename other-form-data 
                 contents overlimit? pkgname convert?
		 make-loadable? loadvarname)
                :br :br
                ((:a :href
                  (one-string
                   (make-redisplay-url 
                    :pkg pkgname :uid (incf *user-display-id*))
                   "#TAG"
                   ))
                 (:b "Back to WebListener"))
                ))))))

        )))))


(defun upload-output-filename-from-input (input-filename username)
  (multiple-value-bind (pathname error?)
      (ignore-errors (pathname input-filename))
    (unless (or error? (null (pathname-name pathname)))
      (namestring 
       (merge-pathnames 
        (visitor-directory username)
        pathname)))))

(defun upload-output-file-from-save-as-and-defaults (save-as username)
  (multiple-value-bind (pathname error?)
      (ignore-errors (pathname save-as))
    (unless error?
      (namestring 
       (merge-pathnames
        pathname
        (visitor-directory username))))))


(defun weblistener-upload-report-body
  (input-filename other-form-data contents overlimit? pkgname convert?
		  make-loadable? loadvarname
		  &aux output-filename full-output-path)
  (declare (ignore pkgname))
  (block 
   exit
    
   (cond
     
    ;; No file data ever came...
     
    ((null contents)
     (html "*** No Upload file provided ***"))
     
    ;; Amount of data exceeded our limit.
     
    (overlimit?
     (html 
      "*** OOPS ***" :br
      "Upload file " (:b (:prin1-safe input-filename)) :br
      "exceeds the Weblistener upload limit of "
      (:prin1-safe *weblistener-upload-limit*) " bytes."
      ))
     
    ;; We got something!
     
    (t
      
     ;; See if user provided us with an output file name or try
     ;; to figure one out.
      
     (setq output-filename
	   (cdr (assoc "storepathname" other-form-data :test #'string=)))
     (when output-filename 
       (setq output-filename (space-trim output-filename)))

     ;; (format t ";; Output file = ~A~%" output-filename)

     (cond 
      ;; The user put something in the 'Save it as' field
      ((and output-filename (not (string= "" output-filename)))
       (setq full-output-path
	     (upload-output-file-from-save-as-and-defaults
	      output-filename *username*))
       (unless full-output-path
	 (return-from exit
		      (html
		       "*** Cannot parse output file as given ***" :br
		       (:princ-safe (format nil "As provided: ~A" output-filename))
		       ))))
      ;; Nothing in the 'Save it as' field.  Determine a name from
      ;; the name of the upload file and the user's directory.
      (t
       (setq full-output-path 
	     (upload-output-filename-from-input input-filename *username*))
       (unless full-output-path
	 ;; Can't figure out where to write the data.  Give it up.
	 (return-from
             exit
           (html 
            "*** No output file provided ***" :br
            (:princ-safe
             (format nil
                     "(Nor could a file name be derived from the input file ~A)"
                     input-filename
                     )))))))

     ;; Make sure we can write to where we are supposed to be able to write.

     (let ((directory-to-write-into
	    (make-pathname
	     :host (pathname-host full-output-path)
	     :device (pathname-device full-output-path)
	     :directory (pathname-directory full-output-path)
	     :name nil :type nil :version nil
	     )))
       (handler-case
	(ensure-directories-exist full-output-path)
	(error
	 ()
	 (return-from 
             exit
           (html 
            (:princ-safe
             (format nil "*** Could not access ~A ***" 
                     (namestring directory-to-write-into)))
            :br
            "(Directories may not exist and could not be created," :br
            "and/or permissions may be inadequate)")))))

     ;; Make sure the write actually worked!

     (handler-case
      (with-open-file 
       (p full-output-path 
	  :direction :output :if-exists :supersede
	  :element-type '(unsigned-byte 8))
       (write-sequence contents p)
       t)
      (error
       ()
       (return-from exit
		    (html
		     (:princ-safe 
		      (format nil "*** Could not open output file ~A ***"
			      (namestring full-output-path))) :br
		     "(Permissions may be inadequate)"
		     ))))

     (let ((conversion-done? nil) (n-conversions nil))

       (when convert?
	 (handler-case
	  (multiple-value-setq (conversion-done? n-conversions)
			       (strip-file-of-returns-preceding-newlines 
				full-output-path :verbose nil))
	  (error 
	   (c)
	   (return-from exit
			(html
			 (:big (:princ-safe "Ruh Roh!")) :br
			 (:princ-safe 
			  (formatn 
                           "Error doing return/newline conversion: ~A" c)) :br
			 )))))

       (when make-loadable?
	 (handler-case
	  (let ((index-file (merge-pathnames 
			     (visitor-directory *username*)
			     "loadable-tables.index")))
	    (progn 
	      (html
	       (:big 
                (:princ-safe (format nil "uploading ~a in ~a as ~a" 
                                     full-output-path index-file loadvarname)) 
		     :br))
	      (with-open-file 
	       (o index-file :direction :output
		  :if-exists :append
		  :if-does-not-exist :create)
	       (print (list (read-from-string loadvarname)
			    full-output-path) o))))
	  (error 
	   (c)
	   (return-from 
	    exit
	    (html
	     (:big (:princ-safe "Ruh Roh!")) :br
	     (:princ-safe 
	      (formatn "Error making table file loadable: ~A" c)) :br
	     )))))

       ;; Everything is OK 

       (html 
	"Upload file: " (:b (:prin1-safe input-filename)) :br
	"Saved as: " (:b (:prin1-safe (namestring full-output-path))) :br
	(:princ-safe
	 (one-string
	  (formatn "File size: ~D" (length contents))
	  (if (zerop (length contents))
	      "  *** Possible problem -- no data transferred ***"
	    ""))) :br
	(when convert?
	  (html
	   (:princ-safe "Return/Newline conversion status: ")
	   (:princ-safe 
	    (if conversion-done?
		(formatn "File modified, ~D conversions." n-conversions)
	      "File was not modified (no conversions needed)."
	      ))))))

     ))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(publish
 :path *feedback-form-url*
 :content-type cl-user::*html-publish-content-type*
 :function 
 (lambda (req ent)
   (let* ((input (request-query req))
          (pkgname (url-parameter-value :pkg input))
          (package-symbol (keywordize pkgname))
          )
     (execute-with-standard-weblistener-environment
      req ent package-symbol
      (lambda () (feedback-form-function pkgname)))
     )))

(defun user-email-from-user-package-name (userpkg)
  (let ((address (get (keywordize userpkg) :email)))
    (if (and address 
             (not (zerop (length address)))
             (not (every (lambda (ch) (char= ch #\Space)) address))
             )
        (values address t)
      (values (formatn "~A@unknown.mail" userpkg) nil)
      )))

(defun feedback-subject (userpkg)
  (formatn
   "Feedback ~A from ~A, sessionid = ~a, app = ~A, port = ~A, machine = ~A"
   (get-universal-time)
   (user-email-from-user-package-name userpkg) 
   *sessionid*
   cl-user::*application-instance*
   user::*weblistener-port*
   user::*weblistener-machine-name*
   ))

;;; The page with the form on it to get feedback.

(defun feedback-form-function (pkgname)
  (let* ((app (application-name cl-user::*ai*))
         (title (formatn "~A Feedback Facility" app)))
    (multiple-value-bind (email-address provided?)
        (user-email-from-user-package-name *username*)
      (with-standard-weblistener-page-header
          (title :listener? nil)
        (html
         (:body
          ((:form :method "post" :action *feedback-form-response-url*)
           :br
           ((:font :color "red")
            (:big 
             (one-string
              "Please give us your comments, criticisms, questions "
              "and/or suggestions.")))
           :br
           ((:font :color "red")
            (:big
              "Help us help you by:"
	          (:UL
                  (:LI "Describing the circumstances leading up to the issue")
	              (:LI "Providing the text of any error message you encountered")
	         	  (:LI "Telling us the result you wish you had obtained"))))
           :br 
           (:b (:i "From: "))
           (:princ-safe email-address)
           :br
           (:b (:i "To: "))
           (:princ-safe cl-user:*default-support-email-address*)
           :br
           (:b (:i "Subject: ")) 
           (:princ-safe (feedback-subject *username*))
           :br :br
           ((:textarea :name "feedback" :rows 18 :cols 80)
            (:princ-safe
             (if provided?
                 "" 
               (one-string-nl
                "Please include a return email address in your message "
                "if you would like a response!"
                ))))
           :br
           (:princ "Send your message to the system support staff: ")
           ((:input :type "submit" :value "Send"))
           ((:input :type "HIDDEN" :name "pkg" :value pkgname))
           )))))))


;;; Handler for the form data that comes back from the Biolisp feedback form.

(publish 
 :path *feedback-form-response-url*
 :content-type cl-user::*html-publish-content-type*
 :function
 (lambda (req ent)
   (let* ((input (request-query req))
          (pkgname (url-parameter-value :pkg input))
          (package-symbol (keywordize pkgname))
          (feedback (url-parameter-value :feedback input))
          )
     (execute-with-standard-weblistener-environment
      req ent package-symbol
      (lambda () (feedback-form-response-function pkgname feedback))
      ))))

(defun feedback-form-response-function (pkgname feedback)
  (declare (ignore pkgname))
  (let ((config-data 
         (with-output-to-string (s)
           (let ((*standard-output* s))
             (describe-configuration-variables))))
        (room-data 
         (with-output-to-string (s)
           (let ((*standard-output* s))
             (room t)))))
    (handler-case 
        (progn
          (forward-funcall
           'send-file-string-as-email
           nil
           (one-string-nl
            ""
            feedback
            ""
            "==============================================="
            ""
            config-data
            "==============================================="
            ""
            room-data
            )
           cl-user:*default-support-email-address*
           :subject (feedback-subject *username*)
           )
          (html 
           (:princ 
            (html-for-indirect-to-url
             (url-and-args 
              *weblistener-evalstring-url*
              :pkg *sessionid* 
              :evalstring 
              (formatn 
               (s+ "\"Message Sent to ~A!\"") 
               cl-user:*default-support-email-address*
               ))))))
      (error 
       (c) 
       (html 
        :br
        (:h2 (:b (:center (:princ-safe "Attempt to send email failed!"))))
        :br
        (:princ-safe "actual error from email program:  ")
        :p
        (:princ-safe (formatn "~A" c))
        :p
        (:princ-safe 
         "(Use the back button twice to get back to the weblistener.)")
        :p
        ((:font :color "red") 
         (:princ-safe "Please report this to the system administrators.")))))
    ))
