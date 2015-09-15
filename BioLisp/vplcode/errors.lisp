;;; -*- mode: Lisp; Syntax: Common-Lisp; Package: nvpl; -*-

(in-package :nvpl)

;;; +=========================================================================+
;;; | Copyright (c) 2006 JP Massar, John Myers                                |
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

;; Author: JP Massar, John Myers.

;; We could call user-error-message here, but the window that pops up is 
;; even more annoying than the pseudo-popup that show-vpl-error-message 
;; causes to appear.
(defun user-info-message (s) (show-vpl-error-message s))

(defun user-error-message (s &optional (prefix "error-") &key (type "txt"))
  (create-and-use-unique-file 
   (user-temp-vpl-dir)
   (lambda (file p) 
     (declare (ignore file))
     (format p "~A~%" s)
     )
   (lambda (file) (show-vpl-popup-URL-window (user-temp-vpl-dir-url file)))
   :name (s+ prefix (string wb::*sessionid*))
   :type type
   ))


(define-condition vpl-error (error) ())  

(define-condition vpl-internal-error (vpl-error)
  ((reason :initarg :reason :reader vpl-internal-error-reason)
   (user :initarg :user :reader vpl-internal-error-user :initform nil)
   )
  (:report 
   (lambda (condition stream)
     (format 
      stream 
      (one-string-nl
       "$$>>> VPL internal system error! Please report this to the sys admins."
       "Timestamp: ~A, User: ~S"
       "Actual error: ~A")
      (make-timestamp-string) 
      (vpl-internal-error-user condition)
      (vpl-internal-error-reason condition)
      ))))

(define-condition vpl-null-parent-error (vpl-internal-error)
  ())

(defun vpl-internal-error (reason-format-string &rest format-args)
  (error
   (make-condition
    'vpl-internal-error 
    :reason (apply 'format nil reason-format-string format-args)
    :user wb::*username*
    )))

(defun vpl-null-parent-error (snippet)
  (error 
   (make-condition 
    'vpl-null-parent-error
    :reason (format nil "Snippet ~S has null parent!" snippet)
    :user wb::*username*
    )))
   
(define-condition vpl-user-error (error) 
  ((reason :initarg :reason :reader vpl-user-error-reason))
  (:report
   (lambda (condition stream)
     (format
      stream
      "Ruh Roh.  User error...~%~A"
      (vpl-user-error-reason condition)
      ))))

(defun vpl-user-error (reason-format-string &rest format-args)
  (error 
   (make-condition
    'vpl-user-error 
    :reason (apply 'format nil reason-format-string format-args)
    )))

(define-condition vpl-invalid-input-type (vpl-error)
  ((data :initarg :data :reader vpl-invalid-input-type-data)
   (expected-type 
    :initarg :expected-type :reader vpl-invalid-input-type-expected-type))
  (:report 
   (lambda (condition stream)
     (format 
      stream 
      (one-string-nl
       "User input error.  Expected data of type ~S, but user input"
       "'~A' could not be interpreted as something of that type."
       "Please reenter with correctly formatted data.")
      (vpl-invalid-input-type-expected-type condition)
      (vpl-invalid-input-type-data condition)
      ))))

(define-condition vpl-input-not-parseable (vpl-error)
  ((data :initarg :data :reader vpl-input-not-parseable-data))
  (:report 
   (lambda (condition stream)
     (format 
      stream 
      (one-string-nl
       "User input error.  The data which you typed in, '~A',"
       "is not recognizable as a valid BBL constant, symbol, or"
       "other expression.")
      (vpl-input-not-parseable-data condition)
      ))))

(defun vpl-not-logged-in-error (sessionid-symbol)
  (let ((message 
         (formatn 
          (one-string
           "Session ID ~S not recognized! <br> "
           "This may mean that the BioBike system has been rebooted. <br> "
           "If so, you will have to log in again and restart your VPL session."
           " <br> "
           "<a href=~S>Try to log in again</a>"
           )
          sessionid-symbol (wb::login-webpage-url cl-user:*ai*)
          )))
    #+this-doesnt-work
    (send-json-without-sessionid "show-status" "message" "not logged in")
    ;; This doesn't work either!  Fortunately, this code
    ;; is almost impossible to get to.  
    (create-and-use-unique-file 
     (user-temp-vpl-dir)
     (lambda (file p) 
       (declare (ignore file))
       (format p "~A~%" message)
       )
     (lambda (file) 
       (show-vpl-popup-URL-window 
        (user-temp-vpl-dir-url file)
        :location "yes"
        :directories "yes"
        :status "yes"
        :menubar "yes"
        :height "inherit"
        :width "inherit"
        ))
     :name (s+ "error-" (string wb::*sessionid*))
     :type "html"
     )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-condition hole-error (simple-error) ())

(define-condition vpl-execution-condition (condition)
  ((data :initarg :data :accessor vpl-execution-condition-data)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun idstring->id (idstring type &aux value)
  (handler-case 
      (progn 
	(setq value
	      (etypecase idstring
		(string (read-from-string idstring))
		(number idstring)))
        (unless (integerp value)
          (vpl-internal-error 
           "The ID string ~S is not an integer!" idstring))
        (unless (or (null type) (eq type (unique-id-type value)))
          (vpl-internal-error 
           "The ID ~D is supposed to be a ~S, but UNIQUE-ID-TYPE returns ~S !"
           value type (unique-id-type value)
           ))
        value
        )
    (vpl-internal-error (c) (signal c))
    (error 
     ()
     (vpl-internal-error
      (one-string-nl
       "The ID string ~S is supposed to be an integer, but"
       "it is not even readable using READ-FROM-STRING !")
      idstring
      ))))
