(in-package :cl-user)

;;; Original code potentially fails with a package error
;;; if KEGG interface doesn't get set up properly

;;;; #-lispworks
;;;; (net.xmp.soap:define-soap-type nil 'wsdl-1:|ArrayOfstring|
;;;;   '(:array net.xmp.schema:|string| :send-atype t))


#-lispworks
(handler-case
    (eval 
     (read-from-string
      (utils::s+
      "(net.xmp.soap:define-soap-type nil 'wsdl-1:|ArrayOfstring| "
      "'(:array net.xmp.schema:|string| :send-atype t))" 
      )))
  (error
   (c)
   (format t ";;; *** Problem trying to execute KEGG bug fix...~%")
   (format t ";;; *** Actual problem:  ~A~%" c)
   (format t ";;; *** Perhaps a problem with loading the SOAP goo previously.")
   (format t ";;; *** System load continuing...")
   ))

