;;; -*- Package: cl-user; mode: lisp; base: 10; Syntax: Common-Lisp; -*-

;; Specification of interface (possibly not up to date)
;; can be found at http://ws.nmpdr.org/

;; Phage list http://bioseed.mcs.anl.gov/~redwards/FIG/Phage.cgi

;; seed glossary: http://www.theseed.org/wiki/Glossary#PEG

;; Biobike on seed machine: http://seed.sdsu.edu:7001/biologin

;; new wsdl: http://seed.sdsu.edu/FIG/wsdl_seed_complex.cgi

;; good test gid is 217.1
;; good virus test gid is 12021.1

#||


||#

(in-package :cl-user)

(defpackage :seed (:use :common-lisp))

#||

;; These are now weblistener configuration variables
(defparameter *seed-mysql-data-root* "/mnt/u03/robs_data/FIGdisk/")
(defvar *seed-access-mode* :soap)

||#

(defun seedapi ()
  (format t ";; Creating SOAP Seed interface...~%")
  (format t ";;   URL: ~A~%" *seed-wsdl-url*)
  (process-wsdl-file *seed-wsdl-url* "seedapi.lisp" :ipackage :seed)
  (seed-array-of-string-bugfix)
  )

;; This is dependent on the seed interface using the WSDL-2 package!  
;; Also note that the type is ArrayOfString, vs in the analogous KEGG
;; bug fix code, it is ArrayOfstring (lowercase 'string').  
(defun seed-array-of-string-bugfix ()
  #-lispworks
  (handler-case
      (with-output-to-string (s)
        (let ((*error-output* s))
          (eval 
           (read-from-string
            (utils::s+
             "(net.xmp.soap:define-soap-type nil 'wsdl-2:|ArrayOfString| "
             "'(:array net.xmp.schema:|string| :send-atype t))" 
             )))))
    (error
     (c)
     (format t ";;; *** Problem trying to execute KEGG bug fix...~%")
     (format t ";;; *** Actual problem:  ~A~%" c)
     (format
      t ";;; *** Perhaps a problem with loading the SOAP goo previously.")
     (format t ";;; *** System load continuing...")
     )))
