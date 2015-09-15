;;; -*- Package: cl-user; mode: lisp; base: 10; Syntax: Common-Lisp; -*-

(in-package :cl-user)

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

(format t "~%;; BioWebListener startup initialization file executing...~%")

(multiple-value-bind (second minute hour date month year)
    (get-decoded-time)
  (format t "~%;; Startup at ~A~%"
       (format nil "~2,'0D/~2,'0D/~2,'0D ~2,'0D:~2,'0D:~2,'0D"
               month date (mod year 100) hour minute second
               )))

(format t "~%;; Listing of Allegro patches:~%~%")
(pprint sys::*patches*)

(defparameter *biowebload-file* nil)
(defparameter *bioweblistener-pid* (excl.osi:getpid))

(format t "~%;; Process ID of Lisp process: ~A~%" *bioweblistener-pid*)

(format t "~%~%;; User level command line arguments being processed:~%")
(format t ";; Command line arguments:  ~A~%" (sys:command-line-arguments))

;; Parse the command line arguments

(sys:with-command-line-arguments 
 (("port" :long port :optional-companion)
  ("webload" :long webload :required-companion))
 (restvar)
 (format t ";; HTTP PORT ARGUMENT = ~A~%" port)
 (format t ";; WEBLOAD ARGUMENT = ~A~%" webload)
 (cond
  ;; If no port was specified, and no configuration file has
  ;; previous SETQ'ed the port, set it to the default (8002) port.
  ((eq port t) 
   (when (not (boundp 'cl-user::*weblistener-port*))
     (setq cl-user::*weblistener-port* 8002)
     ))
  (t 
   (handler-case
       (let ((port-number (parse-integer port)))
         (unless (and (plusp port-number) (< port-number (expt 2 16)))
           (format t "Port number, ~A, out of legal range." port-number)
           (excl:exit -1))
         (setq cl-user::*weblistener-port* port-number)
         )
     (error (c)
            (format t ";; Error parsing port argument.~%")
            (format t ";; Error condition: ~A~%" c)
            (excl:exit -1)
            ))))
 (unless (probe-file webload)
   (format t ";; Cannot find webload file. PROBE-FILE fails on ~A~%" webload)
   (excl:exit -1))
 (setq *biowebload-file* webload)
 )
(format t "~%;; Processed command line arguments.~%")
(force-output t)

;;; Load WebListener software
;;; This causes application-specific initializations to occur.

(format t "~%~%;; Weblistener/BioBike software being loaded.~%")
(format t ";;  Using loader file ~A~%" *biowebload-file*)
(load *biowebload-file*)
(format t "~%;; Weblistener/BioBike software loaded.~%")
(force-output t)

(format t "~%~%;; Conducting sanity check on password file.~%")
(password-file-sanity-check)
(format t "~%~%;; Password file looks OK.~%")
(force-output t)

;;; Start SLIME server

(format t "~%~%;; SLIME server being started.~%")
(let* ((port-key 
        (format nil "~A:~A" *weblistener-machine-name* *weblistener-port*))
       (table
        '(("localhost:8000" 40001)
          ("nostoc.stanford.edu:8001" 40002)
          ("nostoc.stanford.edu:8002" 40003)
          ("nostoc.stanford.edu:8003" 40004)
          ("biobike.csbc.vcu.edu:8001" 40005)
          ("biobike.csbc.vcu.edu:8002" 40006)
          ("biobike.csbc.vcu.edu:8003" 40007)
          ("biobike.csbc.vcu.edu:8007" 40008)
          ("biobike.csbc.vcu.edu:9002" 40009)
          ("biobike.csbc.vcu.edu:9003" 40010)
          ))
       (slime-port
        (cadr (assoc port-key table :test 'string-equal))
        ))
  (handler-case
      (progn
        (wb::start-biobike-slime-server (or slime-port 0))
        (format t "~%;; SLIME server started, port ~d.~%" 
                wb::*slime-server-port*
                ))
    (error 
     (c) 
     (format t "~%;; Could not start SLIME server, port ~D.~%;;   Reason: ~A~%"
             wb::*slime-server-port* c
             ))))
(force-output t)


;;; And start the Web Listener.

(format t "~%~%;; AllegroServe Weblistener being started.~%")
(wb:start-weblistener :port *weblistener-port*)
(format t "~%;; AllegroServe Weblistener started.~%")
(force-output t)


;;;; Optionally enable various services.

;;; Programatic and documentation access service

(wb::system-dummy-users)

(force-output t)

;;; XML-RPC service.

(when (and cl-user:*enable-default-user-for-programatic-eval*
           cl-user:*enable-xml-rpc-at-startup?*)
  (format t "~%~%;; XML-RPC eval server being enabled~%")
  (wb::make-xml-rpc-eval-server)
  (format t "~%;; XML-RPC eval server enabled.~%"))
(force-output t)

;;; Temporary files cleanup

(let ((tmpdir (cl-user:translate-simple-lp "tmp:")))
  (format t "~%;; Cleaning up files in ~A older than 1 week." tmpdir )
  (utils::purge-files-before tmpdir :n-days-ago 7 :recursive? t))

(format t "~%~%;; Bioweblistener startup initialization file complete.~%")

(format t "~%~%;; Complete list of enabled features (*features*): ~%")
(pprint *features*)
(terpri) (terpri)

(format t ";; Going into sleep loop after GC.  Goodnight.~%")
(force-output t)
(sys::gc t)

(loop 
 (let ((memory-used (wb::capture-current-memory-usage)))
   (wb::add-latest-memory-stats memory-used)
   (wb::log-system-event "Current memory usage (bytes): ~D~%" memory-used)
   (format t ";; Tick, ~A~%" (utils::make-timestamp-string :mode :MMDDYYHHMM))
   (force-output t)
   (sleep 900)
   ))
