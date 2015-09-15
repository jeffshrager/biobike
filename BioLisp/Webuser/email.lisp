;;; -*- mode: Lisp; Syntax: Common-Lisp; Package: wb; -*-

(in-package :wb)

;;; +=========================================================================+
;;; | Copyright (c) 2002-2005 JP Massar, Jeff Shrager, Mike Travers           |
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

;;; Author:  JP Massar, Jeff Shrager

(defun get-smtp-host () 
  (if (and (boundp 'cl-user:*smtp-server*) (stringp cl-user:*smtp-server*))
      cl-user:*smtp-server*
    (progn
      (warn "Must define config variable USER:*SMTP-SERVER* to use email.")
      nil
      )))

(defun set-email-address (email-string) 
  #.(one-string-nl
     "Set a users' email address, overriding the one stored in the password"
     "file, and/or entered at login time.")
  (setf (get *username* :email) email-string))

(defun current-user-email-address ()
  (let ((stored-email (get *username* :email)))
    (if (and stored-email 
             (not (zerop (length (string-trim " " stored-email)))))
        stored-email
      (let ((account-email 
             (user-email (find-login-record (string *username*)))))
        account-email
        ))))

(defun canonicalize-email-to-string (to)
  (cond
   ((null to)
    (or (current-user-email-address)
        (error "Cannot find email address for user ~S" *username*)))
   ((and (stringp to) (position #\@ to)) to)
   ((symbolp to) (canonicalize-email-to-string (string to)))
   ((stringp to)
    (let ((user-record (find-login-record to)))
      (if user-record
          (user-email user-record)
        (vif (email (get (keywordize (string-upcase to)) :email))
             email
             (error "'~A' is neither a valid email address nor login name" to)
             ))))
   ((listp to)
    (mapcar 'canonicalize-email-to-string to))
   (t (error "~S is not recognizable as an email address" to))
   ))

(defun send-file-string-as-email (filename file-string to &key subject)
  (let ((smtp-host (get-smtp-host))
        (from (application-from-email-address cl-user:*ai*)))
    (cond
     ((null smtp-host)
      (error "Cannot email, no SMTP host defined."))
     ((null from)
      (error "Cannot email, no FROM address defined for application!"))
     (t
      (funcall 
       ;; So we can compile/load into Lispworks.
       ;; XXX Why does this work like this? Seems like it should punt
       ;; - mas 9/22/04
       (intern "SEND-LETTER" (find-package :net.post-office))
       ;; Changed per allegro documentation for net.post-office::send-letter
       ;; because call was failing on nostoc (but working on biobike).  
       ;; This call works on both machines and hopefully would work 
       ;; on edwards.  
       (list smtp-host :starttls t)
       from
       to
       file-string
       :subject 
       (or subject (format nil "Contents of ~A" filename))
       )))))


    
(defun probe-file-no-error (string)
  (handler-case
      (probe-file string)
    (error () nil)
    ))

(defconstant email-string-as-file-name-size 256)

(defgeneric email-me (object &key to subject &allow-other-keys)
  (:documentation
  #.(one-string-nl
     "Emails a file, e.g., (email-me \"/home/visitors/guest/myfile.tbl\"), "
     "or a function definition (email-me 'my-function)"
     "or any Lisp object, e.g., (email-me a-very-long-list), "
     "to the current user (by default) or to anyone using the :to keyword, "
     "with a default subject or as specified by the :subject keyword."
     "If a Lisp object is being mailed, this function uses PPRINT to write"
     "OBJECT to a file and then emails the file."
     "If OBJECT is a string, if it is a short string (< 256 character)"
     "EMAIL-ME interprets the string as a filename and attempts" 
     "to email you the contents of that file.  If the string"
     "is longer, then the string itself is emailed to you."
     )))

(defmethod email-me 
           ((object string) 
            &key 
            (to nil)
            (subject object)
            &allow-other-keys)
  (setq to (canonicalize-email-to-string to))
  (cond
   ((> (length object) email-string-as-file-name-size)
    (cformatt "Sending string as data, not interpreting as a filename.")
    (email-me (list object) :to to :subject subject))
   ((null (probe-file-no-error object)) 
    (if (> (length object) 80)
        (cformatt "Cannot email file.")
      (cformatt "Cannot email file ~A." object))
    (cformatt "File does not exist or is not accessible.")
    (cformatt "If you are really trying to email a string put the")
    (cformatt "string into a list: e.g.,  (email-me (list my-string))")
    nil
    )
   (t
    (let* ((buffer-size (file-size-in-lisp-chars object))
           (buffer (make-string buffer-size)))
      (with-open-file (p object)
        (let ((n-chars-read (read-sequence buffer p)))
          (unless (= n-chars-read buffer-size)
            (error 
             (one-string
              "Internal read error.  File had ~D characters when read "
              "sequentially but READ-SEQUENCE read ~D characters ??")
             buffer-size n-chars-read
             ))
          (send-file-string-as-email 
           object
           (s+ 
            (formatn "~%******************  START  ******************~%~%")
            buffer
            (formatn "~%~%******************  END  ******************~%"))
           to :subject subject)
          (cformatt "Email sent to ~A." to)
          t
          ))))))

(defmethod email-me 
           ((object symbol)
            &key 
            (to nil)
            (subject "Requested BioBike function")
            &allow-other-keys
            &aux source
            )
  (setq to (canonicalize-email-to-string to))
  (block exit
    ;; We have the source as an s-expression
    (when (setq source (get object :procedure-definition))
      (if (listp source)
          (email-me source :to to :subject subject)
        (error "Weird source for function definition for symbol ~S, ~S"
               object source
               ))
      (return-from exit t))
    ;; See if we can get the source as text.
    (setq source (function-definition-source-text object))
    (cond
     ((null source) 
      (formatn "No source found for ~S. Cannot email." object))
     ((initial-subsequence-of? source "***")
      (formatn "~A.~%Cannot email." source))
     ;; Yes, we have it as text.  Write it to a temp file and email it.
     (t
      (let ((tmpdir (cl-user:translate-simple-lp "tmp:")))
        (with-temp-file-in (filepath tmpdir :prefix "EMAIL" :if-exists :again)
          (with-open-file 
              (p filepath :direction :output :if-exists :supersede)
            (format p "~%~A~%" source))
          (email-me (namestring filepath) :to to :subject subject)
          t
          ))))))

(defmethod email-me 
           ((object t)
            &key 
            (to nil)
            (subject "Requested BioBike object")
            &allow-other-keys)
  (setq to (canonicalize-email-to-string to))
  (let ((tmpdir (cl-user:translate-simple-lp "tmp:")))
    (with-temp-file-in (filepath tmpdir :prefix "EMAIL" :if-exists :again)
      (with-open-file 
          (p filepath :direction :output :if-exists :supersede)
        (let ((user-package *package*))
          (with-standard-io-syntax 
            (let ((*package* user-package)) (pprint object p))
            )))
      (email-me (namestring filepath) :to to :subject subject))
    ))



(defun email-me-my-password (email-address login password)
  (email-me 
   (formatn 
    "Hello, ~A!~%Your Biobike password for machine ~A, port ~A is:~%~A~A"
    login *weblistener-machine-name* *weblistener-port* password 
    (make-string email-string-as-file-name-size :initial-element #\Space)
    )
   :to email-address
   :subject "What you forgot"
   ))
   
     