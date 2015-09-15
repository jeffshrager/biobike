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

#|

XML-RPC server to handle evaluation of forms sent as strings to the
Weblistener via the XML-RPC protocol.

The URL that accepts the XML-RPC evaluation requests is

  <host-the-webserver-is-running-on:port>/XmlRpcEvalStringUrl.html

  The server supports three methods:
   LoginMethod
   EvalStringMethod
   EvalEchoMethod

----------------------------------------------------------------


LoginMethod(username, password)

It returns a structure:
   
   status         : integer
   sessionid      : string
   error          : string

   status is 0 on success, nonzero on failure.
   on success, sessionid is a string reprenting the session which can be 
   passed to EvalStringMethod.
   

----------------------------------------------------------------


EvalStringMethod(sessionid-string,form-string,compile?)

returns a structure with named components of the specified XML-RPC types:

  status          : integer
  errorpresent    : boolean
  errorstring     : string
  printoutpresent : boolean
  printoutstring  : string
  nvalues         : integer
  values          : array

The semantics of the other components depend on STATUS:

  STATUS:  a status code, currently -1, 0, 1, 2, 3 or 4.

    -1 :  An internal error occurred in the Weblistener.  No other
          information is available.  (Should not happen...)
     0 :  Successful execution.  ERRORPRESENT should be false.
          NVALUES and VALUES will contain the result information.
     1 :  Read error.  An error was detected attempting to convert
          the form string to a form to be evaluated using READ.
          ERRORPRESENT will be be true and ERRORSTRING should contain
          the text of the error message.
     2 :  Evaluation error.  An error was detected while evaluating
          the form (after successfully converting the string into a form).
          ERRORPRESENT will be be true and ERRORSTRING should contain the
          text of the error message.        
     3 :  Print error.  An error was detecting attempting to convert
          the result form(s) resulting from the evaluation back into strings
          (so that they can be sent back across the XML-RPC interface)
          ERRORPRESENT will be be true and ERRORSTRING should contain the
          text of the error message. 
     4 :  Not logged in error.  The SESSIONID-STRING passed as the first
          argument to EvalStringMethod does not represent a logged-in
          user of the Weblistener, so the form string to be read and 
          evaluated has no context in which it can be done.  No other 
          information is available.
     5 :  Compilation error.  An error was detected which compiling the
          form (assuming compiliation is enabled).  ERRORPRESENT will be
          true and ERRORSTRING should contain the text of the error message.
      
   NVALUES:  If evaluation is successful (STATUS=0), contains the number
     of values returned (as in Lisp's ability to return multiple values)

   VALUES:  If evaluation is successful (STATUS=0) contains NVALUES elements,
     the jth element being the jth multiple value returned by the evaluation.
     (Usually NVALUE=1 so VALUES will typically be a single-element array)

   PRINTOUTPRESENT:  If evaluation is successful (STATUS=0), this is true
     if the evaluation caused printout to happen (as a side effect).
   
   PRINTOUTSTRING:  If evaluation is successful (STATUS=0), and PRINTOUTPRESENT
     is true, this contains a string which is the result of concatenating
     all the output caused as a side effect of the evaluation.  No distinction
     is made between output directed to *standard-output* vs. *error-output*

----------------------------------------------------------------

Here's how to use the interface from Python:

import xmlrpclib
s = xmlrpclib.Server("http://nostoc.stanford.edu:8002/XmlRpcEvalStringUrl.html")
session = s.LoginMethod("username", "password"_)
s.EvalStringMethod(session, '(+ 1 2)','NIL')

|#
#-:SBCL

(eval-when (:compile-toplevel :load-toplevel :execute) 
  (require :xml-rpc))

(defparameter *xml-rpc-evalstring-url* "/XmlRpcEvalStringUrl.html")
(defparameter *xml-rpc-evalstring-method* "EvalStringMethod")
(defparameter *xml-rpc-eval-echo-method* "EvalEchoMethod")
(defparameter *xml-rpc-login-method* "LoginMethod")

;;; IF YOU ADD ANY STATUS CONSTANTS MAKE SURE TO
;;; UPDATE VERIFY-XML-RPC-EVAL-STATUS !!
(defconstant xml-rpc-min-error-code -1)
(defconstant xml-rpc-eval-internal-error -1)
(defconstant xml-rpc-eval-ok 0)
(defconstant xml-rpc-eval-read-error 1)
(defconstant xml-rpc-eval-eval-error 2)
(defconstant xml-rpc-eval-print-error 3)
(defconstant xml-rpc-eval-not-logged-in 4)
(defconstant xml-rpc-eval-compilation-error 5)
(defconstant xml-rpc-max-error-code 5)

(defun verify-xml-rpc-eval-status (status)
  (unless (and (integerp status)
               (>= status xml-rpc-min-error-code)
               (<= status xml-rpc-max-error-code))
    (error "Invalid status: ~A, in XML-RPC-EVAL module" status)
    ))

(defun make-xml-rpc-eval-response-struct
       (status error-message printout value-strings)
  "The information that gets sent back to the client"
  (let ((errorpresent 
         (and (stringp error-message) (not (string= error-message ""))))
        (printoutpresent
         (and (stringp printout) (not (string= printout "")))))
    ;; The data pertaining to a REAL-EVAL execution is sent back
    ;; in one of these 'structures', which is converted on the end
    ;; to an appropriate data structure depending on the language
    ;; being used to make the XMLRPC call on the other end.
    (net.xml-rpc:make-xml-rpc-struct
     '("status" :int) status
     '("errorpresent" :boolean) errorpresent
     '("errorstring" :string) (if errorpresent error-message "<none>")
     '("printoutpresent" :boolean) printoutpresent
     '("printoutstring" :string) (if printoutpresent printout "<none>")
     '("nvalues" :int) (length value-strings)
     '("values" :array) value-strings
     )))
  
(defun log-xml-rpc-eval-response 
    (status error-message printout value-strings)
  "Create and Log XML-RPC response,.  Return the response structure."
  (let* ((response (make-xml-rpc-eval-response-struct
                    status error-message printout value-strings))
         (xml-encoding (net.xml-rpc:make-xml-rpc-encoding response)))
    (log-user-event "XML-RPC RESPONSE: ~A~%" xml-encoding)
    response
    ))
    

(defun make-xml-rpc-eval-server ()
  #.(one-string-nl
     "Called during initialization AFTER AllegroServe has been started."
     "This defines an XMLRPC 'submechanism' of AllegroServe that responds"
     "to XML-RPC requests sent to the URL which is the value of"
     "*XML-RPC-EVALSTRING-URL*."
     "This particular instantiation has two remote procedure call methods"
     "available, which are the values of *XML-RPC-EVALSTRING-METHOD*"
     "and *XML-RPC-LOGIN-METHOD*."
     "It also provides a 'test' method, which simply echos back the string" 
     "argument it is called with.")
  (let ((s (net.xml-rpc:make-xml-rpc-server 
            :start nil :enable t
            :publish `(:path ,*xml-rpc-evalstring-url*))))
    (net.xml-rpc:export-xml-rpc-method 
     s 
     `(,*xml-rpc-evalstring-method* xml-rpc-evalstring)
     :struct :string :string :string)
    (net.xml-rpc:export-xml-rpc-method
     s
     `(,*xml-rpc-login-method* xml-login)
     :struct :string :string)
    (net.xml-rpc:export-xml-rpc-method
     s
     `(,*xml-rpc-eval-echo-method* xml-rpc-eval-echo)
     :string :string)
    s
    ))


;; Shorthand.
(defun make-es () (make-xml-rpc-eval-server))


(defun xml-login (uname password) 
  (handler-case 
      (let* ((userid (login-and-create-package uname password nil))
	     (sessionid (connect-to-new-session userid nil "xml-rpc")))
	(log-user-event "XML-RPC LOGIN ~S/~A" uname sessionid)
	(net.xml-rpc:make-xml-rpc-struct
	 '("status" :int) 0
	 '("sessionid" :string) sessionid)
	)
    (error (c)
      (net.xml-rpc:make-xml-rpc-struct
       '("status" :int) 2
       '("error" :string) (princ-to-string c)))))


(defun xml-rpc-evalstring (sessionid-string text-input compile?)

  (handler-case

      (let* ((sessionid (keywordize sessionid-string))
             (compile-flag (string-equal compile? "T")))

        (log-system-event "XML-RPC SESSIONID: ~S~%" sessionid)
        (log-system-event "XML-RPC EVALSTRING: ~A~%" text-input)
        (log-system-event "XML-RPC COMPILE?: ~A~%" compile-flag)

        (if (null (get sessionid :username))

            (log-xml-rpc-eval-response
             xml-rpc-eval-not-logged-in "Not logged in" nil nil)

          ;; Set up the user's environment and call the READ-EVAL-STORE code

          (with-protected-globals-bound
              sessionid
            (with-process-information-bound (text-input)
              (log-user-event "XML-RPC SESSIONID: ~S~%" sessionid)
              (log-user-event "XML-RPC EVALSTRING: ~A~%" text-input)
              (let ((repl (make-default-repl)))
                (setf (repl-input-string repl) (space-trim text-input))
                (setf (repl-evaluation-timeout-threshold repl)
                      *execution-timelimit*)
                (setf (repl-compilation-mode repl) compile-flag)
                (low-level-repl repl)
                (xml-rpc-eval-response repl)
                )))
          ))

    (error
     (c)
     (log-xml-rpc-eval-response
      xml-rpc-eval-internal-error
      (formatn "ERROR: INTERNAL ERROR. ~A" c)
      nil nil
      ))))


(defun xml-rpc-eval-echo (string) string)


(defun xml-rpc-eval-response (repl)

  (cond

   ((null (repl-error-condition repl))
    (log-xml-rpc-eval-response
     xml-rpc-eval-ok
     nil
     (one-string
      (or (repl-compilation-printout repl) "")
      (or (repl-evaluation-printout repl) ""))
     (repl-output-value-strings repl)
     ))

   (t
    (log-xml-rpc-eval-response
     (cond
      ((repl-read-error? repl) xml-rpc-eval-read-error)
      ((repl-compilation-error? repl) xml-rpc-eval-compilation-error)
      ((repl-eval-error? repl) xml-rpc-eval-read-error)
      ((repl-print-error? repl) xml-rpc-eval-print-error)
      (t (error "Oops. Error condition ~A but no error code.")))
     (formatn "~A" (condition-to-string (repl-error-condition repl)))
     (one-string
      (or (repl-compilation-printout repl) "")
      (or (repl-evaluation-printout repl) ""))     
     nil
     ))

   ))

;;;;;;;;;;;;;;;;;;;;;;;;;


(defun test-xml-rpc-call-1 
       (where who &key (what "(+ 1 2)") (verbose t))
  (setq where
        (one-string
         (if (stringp where)
             where
           (ecase where
             (:nostoc "http://nostoc.stanford.edu:8002")
             (:nostoc-jp "http://nostoc.stanford.edu:8001")
             (:ffx "http://ffx.stanford.edu:8002")
             (:local "http://localhost:8000")
             ))
         *xml-rpc-evalstring-url*
         ))
  (when verbose
    (cformatt "Attempting XML RPC eval echo method")
    (cformatt "Host URL is: ~A" where))
  (cformatt 
   "Result: ~A" 
   (net.xml-rpc:xml-rpc-call
    (net.xml-rpc:encode-xml-rpc-call *xml-rpc-eval-echo-method* "Hello, World!")
    :url where
    ))
  (when verbose
    (cformatt "Attempting XML RPC evaluation of form ~S in package ~A"
              what who)
    (cformatt "Method being invoked is: ~A" *xml-rpc-evalstring-method*))
  (net.xml-rpc:xml-rpc-call 
   (net.xml-rpc:encode-xml-rpc-call *xml-rpc-evalstring-method* who what)
   :url where
   ))

;;; Version for testing on a clean (non-biolingua) lisp:

(defun test-xml-rpc-call-remote 
       (where who
        &key
        (what "(+ 1 2)")
        (compile? "t") 
        (verbose t))
  (let ((*xml-rpc-evalstring-url* "/XmlRpcEvalStringUrl.html")
	(*xml-rpc-evalstring-method* "EvalStringMethod")
	(*xml-rpc-eval-echo-method* "EvalEchoMethod"))
    (setq where
	  (concatenate 'string
		       (if (stringp where)
			   where
			 (ecase where
			   (:nostoc "http://nostoc.stanford.edu:8002")
			   (:nostoc-jp "http://nostoc.stanford.edu:8001")
			   (:ffx "http://ffx.stanford.edu:8002")
			   (:local "http://localhost:8000")
			   ))
		       *xml-rpc-evalstring-url*
		       ))
    (when verbose
      (cformatt "Attempting XML RPC eval echo method.")
      (cformatt "Host URL is: ~A" where))
    (format t "Result: ~A" 
            (net.xml-rpc:xml-rpc-call
             (net.xml-rpc:encode-xml-rpc-call 
              *xml-rpc-eval-echo-method* "Hello, World!")
             :url where
             ))
    (when verbose
      (cformatt "Attempting XML RPC evaluation of form ~S in package ~A"
                what who)
      (cformatt "Compiler enabled? ~A" compile?)
      (format t "Method being invoked is: ~A~%" *xml-rpc-evalstring-method*))
    (net.xml-rpc:xml-rpc-call 
     (net.xml-rpc:encode-xml-rpc-call 
      *xml-rpc-evalstring-method* who what compile?)
     :url where
     )))
