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

;;; Miscellaneous macros for the Weblistener.

(defmacro with-http-response-and-body ((req ent) &body body)
  #.(one-string-nl
     "Combines WITH-HTTP-RESPONSE and WITH-HTTP-BODY, which is the"
     "normal way we use those macros.  In doing this we also gain in that"
     "Lispworks will now indent this new macro properly, whereas for some"
     "reason it won't indent WITH-HTTP-RESPONSE or WITH-HTTP-BODY sanely.")
  `(with-http-response 
    (,req ,ent)
    (with-http-body 
     (,req ,ent)
     ,@body
     )))  

(defmacro with-process-information-bound ((input-string) &body body)
  #.(one-string-nl
     "Used by the Weblistener REP loop to mark a currently executing process"
     "and to set its initial priority and quantum.")
  (let ((p-symbol (gensym "PROCESS-"))
        (q-symbol (gensym "QUANTUM-")))
    `(let ((,p-symbol (get-process-priority))
           (,q-symbol (get-process-quantum)))
       (unwind-protect
           (progn 
             (set-process-priority (1- *highest-priority*))
             (set-process-quantum *initial-worker-quantum*) 
             (forward-funcall 'set-webuser-process-pdl ,input-string) 
             ,@body)
         (progn
           (set-process-priority ,p-symbol)
           (set-process-quantum ,q-symbol)
           (forward-funcall 'clear-webuser-process-pdl)
           )))))


(defparameter *storage-condition* 0)

(defmacro with-internal-weblistener-errors-caught ((req) &body body)
  #.(one-string-nl
     "Catch any error executing BODY and generate HTML for a web page"
     "displaying the error and if possible, a stack track.")
  (let ((exitsym (gensym "EXIT-")))
    `(block ,exitsym
       (handler-bind
           (
            ;; Trap this specially because we don't want to print
            ;; out the whole stack on a stack overflow!!
            #+:allegro
            (excl:synchronous-operating-system-signal
             (lambda (c) 
               (generate-html-for-and-log-probable-stack-overflow ,req c)
               ))
            (error 
             (lambda (c) 
               (generate-html-for-and-log-internal-weblistener-error
                ,req c)
               (return-from ,exitsym nil)
               ))
            (storage-condition
             (lambda (c) 
               (declare (ignorable c))
               (incf *storage-condition*)
               (generate-html-for-and-log-storage-condition ,req c)
               (return-from ,exitsym nil)
               ))
            )
         ,@body
         ))))

;; This macro does the restoring (binding) and saving of our
;; protected variables.

(defmacro with-protected-globals-bound (package-symbol &body body)
  `(with-saved-variables-values (,package-symbol) ,@body))

(defvar *req*)
(defvar *ent*)

(defparameter *log-dynamic-webpage-accesses* t)

(defun maybe-log-access () 
  (when *log-dynamic-webpage-accesses*
    (forward-funcall 
     'log-user-event 
     "Webpage access: ~A~%" 
     (net.aserve::request-decoded-uri-path *req*)
     )))

(defun execute-with-standard-weblistener-environment 
       (req ent package-symbol closure)
  (with-http-response-and-body (req ent)
    (with-internal-weblistener-errors-caught (req)
      (with-protected-globals-bound
          package-symbol
        (let ((*req* req) (*ent* ent))
          (maybe-log-access)
          (funcall closure)
          )))))

