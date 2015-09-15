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


#||

(load "C:/lispcode/biolisp/soap-hacking.lisp")
(process-wsdl-file *kegg-url* "keggapi.lisp" :ipackage :kegg)
(process-wsdl-file *seed-url* "seedapi.lisp" :ipackage :seed)

||#

(defvar *soap-interface-file-directory* cl-user::*source-root*)

#+allegro
(eval-when (:compile-toplevel :load-toplevel :execute)
  (lisp:require :soap))

(defun process-wsdl-file 
       (wsdl-url interface-file-name 
                 &key 
                 (interface-file-directory *soap-interface-file-directory*)
                 (ipackage :cl-user)
                 )
  #-:allegro
  (declare 
   (ignore wsdl-url interface-file-name interface-file-directory ipackage))
  #+:allegro
  (let* ((interface-file 
          (merge-pathnames interface-file-name interface-file-directory))
         (new-interface-file (mnemonic-interface-file interface-file)))
    (unless (find-package ipackage)
      (make-package ipackage :use :common-lisp))
    (handler-case 
        (progn
          (net.xmp.soap:make-client-interface
           (wb::with-timeout-limit 
               (20 (error "SOAP connection timeout after 20 seconds..."))
             (net.xmp.soap:decode-wsdl-at-uri wsdl-url))
           nil
           interface-file
           :built-in-arrays :collapse :send-atype t :file-package ipackage
           )
          (format t ";; Created interface in ~A~%" interface-file)
          (format t ";; Creating mnemonic interface file~%")
          (create-mnemonic-soap-interface-functions 
           interface-file wsdl-url ipackage)
          (format t ";; compile loading mnemonic interface file: ~A"
                  (namestring new-interface-file))
          (load (compile-file new-interface-file))
          )
      (error 
       (c)
       (format t "*** Soap interface error: ~A~%" c)
       (let ((fasl (cl-user::fasl-file new-interface-file)))
         (flet ((cont () 
                  (format t "*** Soap interface for wsdl file ~A~%" wsdl-url)
                  (format t "*** not loaded, continuing BioLingua load.~%")
                  ))
           (if (probe-file fasl)
               (progn
                 (format t "*** Trying to load existing soap interface file~%")
                 (format t "*** Interface may not be up to date!")
                 (handler-case 
                     (load fasl)
                   (error 
                    (c)
                    (format t "Load failed: ~A" c)
                    (cont)
                    )))
             (cont)
             )))))))

(defun mnemonic-interface-file (interface-file)
  (let ((ifp (pathname interface-file)))
    (merge-pathnames 
     (pathname 
      (concatenate 
       'string 
       "mnemonic-" (pathname-name ifp) "." (pathname-type ifp)
       ))
     ifp
     )))


;; For every client-function create a function that has a mnemonic name
;; that calls the client function.  The mnenomic name is derived from the
;; name of the function that is called in the body of the client function by 
;; net.xmp.soap:call-soap-method.  

;; We create a new file which contains all the forms of the original interface
;; file plus these new mnemonic functions.  

(defun create-mnemonic-soap-interface-functions 
       (interface-file 
        wsdl-url
        package 
        &key 
        (new-interface-file (mnemonic-interface-file interface-file))
        (verbose? nil)
        &aux (interface-specs nil))
  (format t ";; Creating mnemonic interface file: ~A~%"
          (namestring new-interface-file))
  (with-open-file (p interface-file :direction :input)
    (with-open-file 
        (q new-interface-file :direction :output :if-exists :supersede)
      (format q "~%;;; WSDL URL: ~A~%" wsdl-url)
      (format q ";;; Mnemonic interface function specs at bottom of file~%")
      (loop 
       with *package* = (find-package package)
       as form = (read p nil :eof)
       while (not (eq form :eof))
       do
       (cond
        ((not (and (listp form) (eq (first form) 'defun)))
         (pprint form q))
        ((not 
          (zerop 
           (search "client-" (string (second form)) :test 'string-equal)))
         (pprint form q))
        (t 
         (let* ((wsdl-function (extract-wsdl-name-from-client-function form))
                (client-function (second form))
                (arglist (third form))
                (key-args (rest arglist))
                (mnemonic-name 
                 (intern 
                  (string-upcase (substitute #\- #\_ (string wsdl-function)))
                  (find-package package)
                  ))
                (client-args-symbol (intern "CLIENT-ARGS" package)))
           (unless wsdl-function 
             (error "Huh?  Could not find call-soap-method form in ~A" form))  
           (unless (eq '&key (first arglist))
             (error "Huh?  Client function arglist is not &key: ~A" form))
           (unless (every 'symbolp key-args)
             (error "Huh?  Key arguments are not all symbols!  ~A" form))
           (when verbose? 
             (format t "Creating ~A from ~A~%" mnemonic-name client-function))
           (pprint form q)
           (terpri q)
           (pprint 
            `(defun ,mnemonic-name (&rest ,client-args-symbol ,@arglist)
               (declare (ignore ,@key-args))
               (apply ',client-function ,client-args-symbol))
            q)
           (push (list mnemonic-name arglist) interface-specs)
           )))
       (terpri q)
       finally 
       (progn
         (format q "~%~%#||~%")
         (format q "~%~%;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;~%~%")
         (loop
          for (ifunction iargs) in
          (sort 
           interface-specs 'string-lessp :key (lambda (x) (string (first x))))
          do
          (format 
           q "~A~%" 
           (format nil ";; ~S~%" `(,ifunction ,@iargs))
           ))
         (format q "~%~%||#~%"))
       )))
  new-interface-file
  )
         
(defun extract-wsdl-name-from-client-function (form)
  (block exit
    (cond 
     ((not (listp form)) nil)
     ((not (eq (first form) 
               #+allegro 'net.xmp.soap:call-soap-method 
               #-allegro nil
               ))
      (loop for elem in (rest form) 
            as name = (extract-wsdl-name-from-client-function elem)
            do
            (when name (return-from exit name))
            ))
     (t 
      (unless (and (listp (third form)) 
                   (eq 'quote (first (third form)))
                   (symbolp (second (third form))))
        (error "Huh?  Found call-soap-method but third element not symbol!  ~A"
               form))
      (second (third form))
      ))))




