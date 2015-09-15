;;; -*- mode: Lisp; Syntax: Common-Lisp; Package: wb; -*-

(in-package :wb)

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

(defun return-hack-url ()
  (forward-funcall 
   'make-weblistener-evalstring-url
   :evalstring "T" :pkg (forward-funcall 'wb::user-session-id)))


(defun urlval (key url-alist &optional (nilstring-to-nil? t))
  (url-parameter-value key url-alist nilstring-to-nil?))


(defmacro with-standard-weblistener-page-header 
          ((title &key (body-title t) (listener? t)) &body body)
  #.(one-string-nl
     "Generate HTML for a web page with a particular 'look and feel'"
     "The 'look and feel' is basically what gets displayed as the header"
     "to the page, and is created by the method WEBPAGE-TOPLINKS which is"
     "specified according to the value of USER::*AI*."
     "There are also methods WEBPAGE-HEAD-HTML, WEPAGE-STYLE-HTML and"
     "WEBPAGE-FONT-FACES which specify what goes in the <head> HTML clause,"
     "what the page <style> is, if anything, and various font faces."
     "This macro must be executed within the scope of an"
     "EXECUTE-WITH-STANDARD-WEBLISTENER-EVIRONMENT"
     "call, which binds *username*, *req* and *ent*, or the equivalent."
     "BODY-TITLE specifies that TITLE is to be printed out at the very top"
     "of the page in large font.")
  (let ((title-symbol (gensym "TITLE-")))
    `(let ((,title-symbol ,title))
       (html
        (:head (webpage-head-html cl-user::*ai* ,title-symbol))
        :newline
        (webpage-style-html cl-user:*ai*)
        :newline
        (:body
         ((:font :face (webpage-font-faces cl-user:*ai*))
          :newline
          (when ,body-title 
            (html (:h2 (:center (:princ-safe ,title-symbol)))))
          " "
          :newline
          (webpage-toplinks cl-user:*ai* :listener? ,listener?)
          :newline
          )
         :newline
         :p
         (html ,@body)
         )))))


(defun princ-safe-nl (&rest strings)
  (dolist (string strings) (html (:princ-safe string) :br)))

(defvar *debug-internal-errors* nil)

(defun generate-html-for-and-log-internal-weblistener-error (req c)
  (let ((error-string (format nil "~A" c)))
    (let ((user (request-query-value "pkg" req))
          (stack-trace 
           (if (and (not *debug-internal-errors*)
                    (suppress-stack-trace? error-string))
               (s+
                "<< Stack trace suppressed.  "
                "Set wb::*debug-internal-errors* to T and rerun "
                "to see the trace. >>")
             (stack-trace-to-string)
             )))
      (when (and (not *debug-internal-errors*)
                 (> (length stack-trace) 5000))
        (setq stack-trace (limited-string stack-trace 5000)))
      (forward-funcall 
       'log-system-event
       (formatn
        (one-string
         "WEBLISTENER INTERNAL ERROR.  USER <~A>~%"
         "ACTUAL ERROR: ~A~%STACK TRACE:~%  ~A")
        user c stack-trace))
      (html
       (:h2 
        (princ-safe-nl
         "INTERNAL WEBLISTENER ERROR!"
         "(THIS SHOULD NOT HAPPEN)"
         "PLEASE REPORT THIS TO THE SYSTEM ADMINISTRATORS"
         ))
       :br
       (:princ-safe (format nil "ERROR CONDITION: ~A" c))
       :br :br
       (:princ-safe "STACK TRACE: ")
       :br
       (:pre (:princ-safe stack-trace))
       ))))

(defun suppress-stack-trace? (error-string)
  (cond
   ((search "Connection reset by peer" error-string :test 'string-equal) t)
   ((search "MULTIVALENT hiper socket closed" error-string :test 'string-equal)
    t)
   (t nil)
   ))

(defun generate-html-for-and-log-storage-condition (req c)
  (let ((user (request-query-value "pkg" req)))
    (forward-funcall 
     'log-system-event
     "LISP SYSTEM SIGNAL 'STORAGE-CONDITION' RECEIVED!  USER <~A>~%"
     user c)
    (html-for-storage-condition c)
    (setf (symbol-value (symbol-of-package "*IN-HISTORY*" :wb)) nil)
    (setf (symbol-value (symbol-of-package "*OUT-HISTORY*" :wb)) nil)
    ))

(defun html-for-storage-condition (c)
  (html
   (:h2 
    (:princ-safe "STORAGE CONDITION SIGNALLED -- OUT OF MEMORY ERROR!!") 
    :br)
   (:h3 (:princ-safe (formatn "Actual condition: ~A" c)) :br)
   (:h3
    (princ-safe-nl
     "YOUR CODE MAY BE IN AN INFINITE LOOP, EITHER TRYING TO"
     "CREATE AN INFINITELY BIG DATA STRUCTURE, OR PERHAPS YOU"
     "ARE TRYING TO COMPILE AN INFINITELY RECURSIVE MACRO."
     "(IT'S ALSO POSSIBLE YOU ARE COMPLETELY INNOCENT, THAT"
     "SOMEONE ELSE CAUSED THE PROBLEM, AND YOU JUST HAPPENED"
     "TO BE THE ONE WHO TRIGGERED THE REQUEST FOR MEMORY THAT"
     "DID NOT SUCCEED BECAUSE THE LISP PROCESS WAS VERY CLOSE"
     "ALREADY TO BEING OUT OF MEMORY.)"
     ))
   (:h2
    ((:font :color "red")
     (princ-safe-nl
      "IN ANY CASE PLEASE NOTIFY THE SYSTEM ADMINISTRATORS!"
      "THE SYSTEM MAY NEED TO BE RESTARTED..."
      )))
   (:h3
    (princ-safe-nl
     "As a precaution your history display will be cleared,"
     "in case there is some huge data structure cached there"
     "that would otherwise have been garbage-collected."
     "If you are using the weblistener, you be able to continue working"
     "by using the browser's BACK button, but it is possible"
     "the Lisp system is in an unrecoverable state."
     "If you are using the VPL, you may be able to continue working by"
     "doing a browser refresh, but it is possible"
     "that the Biobike system is in an unrecoverable state."
     ))))
   

;;; This should only get executed if the system code itself caused
;;; a stack overflow.  User code generating this error should get
;;; caught by the normal error-catching mechanisms.

(defun generate-html-for-and-log-probable-stack-overflow (req c)
  (let ((user (request-query-value "pkg" req))
        (stack-trace (stack-trace-to-string :count 50)))
    (forward-funcall 
     'log-system-event
     "LISP SYSTEM SIGNAL '~A' RECEIVED WRT USER ~S~%" c user)
    (html
     (:h2 
      (:princ-safe "~A SIGNALLED -- PROBABLE STACK OVERFLOW!!" c) 
      :br)
     (:h3
      (princ-safe-nl
       "YOUR CODE MAY BE IN AN INFINITE LOOP, OR IN AN INFINITE RECURSION."
       ))
     (:h3
      :p
      (:princ-safe "STACK TRACE: "))
     :p
     (:pre (:princ-safe stack-trace))
     )))


(defun stack-trace-to-string
       (&key (count t) (show-all t) (level :moderate))
  (with-output-to-string (s) 
    (forward-funcall 
     'cl-user::write-stack-trace 
     s :count count :show-all show-all :level level)))



#+test
(publish 
 :path "/error"
 :content-type cl-user::*html-publish-content-type*
 :function
 (lambda (req ent)
   (with-http-response-and-body (req ent)
     (with-internal-weblistener-errors-caught (req)
       (html (error "An error to demonstrate internal error catching."))
       ))))

#+test
(publish 
 :path "/request-info"
 :content-type cl-user::*html-publish-content-type*
 :function
 (lambda (req ent)
   (with-http-response-and-body  (req ent)
     (with-internal-weblistener-errors-caught (req)
       (html
        (:title "REQUEST and ENTITY information")
        (:big (:b (:princ "REQ, ENT information:")))
        :p
        (:princ (formatn "Randomness: ~D" (random 1000000)))
        :br
        (:princ-safe 
         (formatn
          "User Agent: ~A" (net.aserve:header-slot-value req :user-agent)))
        :p
        (:princ-safe (formatn "~A" req))
        :p
        (:princ-safe (formatn "~A" ent)) 
        :p
        (:pre
         (:princ-safe
          (with-output-to-string (s)
            (let ((*standard-output* s) 
                  (*error-output* s)
                  (*print-length* 32))
              (describe req)
              (terpri s)
              (describe ent)
              )))))))))
