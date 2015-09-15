;;; -*- Package: cl-user; mode: lisp; base: 10; Syntax: Common-Lisp; -*-

(in-package :cl-user)

(defpackage :kegg (:use :common-lisp))

(defvar *kegg-url* "http://soap.genome.jp/KEGG.wsdl")

(defun keggapi ()
  (format t ";; Creating SOAP Kegg interface...~%")
  (process-wsdl-file *kegg-url* "keggapi.lisp" :ipackage :kegg)
  (kegg-array-of-string-bugfix)
  )

;; This is dependent on the kegg interface using the WSDL-1 package!  
;; Also note that the type is ArrayOfstring, vs in the analogous seed
;; bug fix code, it is ArrayOfString (uppercase 'String').  
(defun kegg-array-of-string-bugfix ()
  #-lispworks
  (handler-case
      (with-output-to-string (s)
        (let ((*error-output* s))
          (eval 
           (read-from-string
            (utils::s+
             "(net.xmp.soap:define-soap-type nil 'wsdl-1:|ArrayOfstring| "
             "'(:array net.xmp.schema:|string| :send-atype t))" 
             )))))
    (error
     (c)
     (format t ";;; *** Problem trying to execute KEGG bug fix...~%")
     (format t ";;; *** Actual problem:  ~A~%" c)
     (format t ";;; *** Perhaps a problem with loading the SOAP goo previously.")
     (format t ";;; *** System load continuing...")
     )))



(defun kegg-test ()
  (funcall 'kegg::client-57 :pathway_id "path:pma00020")
  (funcall 'kegg::get-genes-by-pathway :pathway_id "path:pma00020")
  )