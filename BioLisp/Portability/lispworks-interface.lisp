;;; -*- Package: user; mode: lisp; base: 10; Syntax: Common-Lisp; -*-

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

;;; Author:  JP Massar.

(eval-when (:compile-toplevel :load-toplevel :execute)

  ;; This is to be a configuration variable.

  ;; You must change this pathname appropriately!

  #-:lispworks4.4
  (defparameter *portableaserve-path* "C:/Lispcode/portableaserve/")

  #-:lispworks4.4
  (if (find-package :net.aserve.client)
      (format t "~&~%;; Portable Allegroserve already loaded, not loading.~%")
    (load (concatenate 'string *portableaserve-path* "load.lisp"))
    )

  ;; This should probably be a configuration variable too...

  ;; You must change this pathname appropriately!
  ;; And you must have a load-xml.lisp file and other stuff,
  ;; which is not part of the CLOCC distribution.

  (defparameter *clocc-path* "C:/Lispcode/clocc/")

  #+:lispworks4.4
  (format  t "~%;; *** Not loading CLOCC, not loading XML parser ***~%")
  #-:lispworks4.4
  (if (find-package :cllib)
      (format t "~&~%;; CLOCC XML code already loaded, not loading.~%")
    (load (concatenate 'string *clocc-path* "load-xml.lisp"))
    )
  )

#-:lispworks4.4
(defun wb:parse-xml (string) (xml-string-to-clxml string))
#+:lispworks4.4
(defun wb:parse-xml (string) 
  (error "not implemented") (xml-string-to-clxml string))

(defun wb:web-page-contents (url &rest keys-and-values)
  "Return the contents of (e.g., the HTML) for a web page as a string"
  (if (member :protocol keys-and-values) 
      (apply 'net.aserve.client:do-http-request url keys-and-values)
    (apply 'net.aserve.client:do-http-request 
           url :protocol :http/1.0 keys-and-values
           )))

(defmacro wb:with-open-web-page 
    ((stream-symbol url &optional (port 80)) &body body)
  (let ((web-page-string-symbol (gensym "WEB-PAGE-STRING-")))
    `(let ((,web-page-string-symbol (web-page-contents ,url :port ,port)))
       (with-input-from-string (,stream-symbol ,web-page-string-symbol)
	 ,@body
	 ))))

(defun write-stack-trace (&rest args)
  (declare (ignore args))
  (format t "No stack trace available"))

(defun user::run-shell-command (command  &rest args &key &allow-other-keys)
  (declare (ignore command args))
  (error "RUN-SHELL-COMMAND not implemented in Lispworks yet."))

(defun generic-copy-file (from to)
  (sys::copy-file from to))

(defun environment-variable-value (string)
  (lispworks::environment-variable string))

#-:lispworks4.4

(progn


;;;; XML stuff.

;;; JP Massar.  Simple parser to convert CLOCC's CLLIB XML format into
;;; something resembling LXML, at least sufficiently for the
;;; example to work.

;;; Hack alert.  Prevent the necessity of having the CLLIB package
;;; defined when we compile and load this file.  Also done around
;;; XML-STRING-TO-CLXML below.

(defun cllib-symbol (s) (intern (symbol-name s) (find-package :cllib)))

(let ((xml-obj-symbol nil))
  (defun clxml-obj? (x) 
    (when (null xml-obj-symbol) 
      (setq xml-obj-symbol (cllib-symbol :xml-obj)))
    (and (listp x) (eq (first x) xml-obj-symbol))
    ))

;;; These two functions work on XML-OBJ lists, which are conversions
;;; from the CLLIB::XML-OBJ structure into a list, done using the
;;; CLLIB::XML-OBJ-TO-LISP-FORM function.

(defun clxml-obj-data-values (xml-obj) (second (fourth xml-obj)))

(defun clxml-obj-name-string (xml-obj)
  (let* ((name (second xml-obj))
	 (xml-name (second name))
	 (local-name (second xml-name))
	 (local-name-string (second local-name)))
    local-name-string
    ))

;;; Toplevel routine XML-STRING-TO-CLXML which takes a string of XML 
;;; and converts it into LXML in three stages:
;;; 1. the string gets converted in CLOCC CLLIB XML format
;;; 2. That format gets converted into CXML format, which basically
;;;    changes the DEFSTRUCT structures into list structure.
;;; 3. CXML gets parsed into a very simple LXML structure, which
;;;    throws away a lot of the information in CXML.


(defun xml-temp-dir ()
  (namestring (cl-user:translate-simple-lp "tmp:")))

(let ((xml-read-from-file-symbol nil)
      (xml-obj-to-lisp-form-symbol nil))
  (defun xml-string-to-clxml 
         (xml-string &optional (name-package (find-package :cl-user)))
    (when (null xml-read-from-file-symbol)
      (setq xml-read-from-file-symbol (cllib-symbol :xml-read-from-file)))
    (when (null xml-obj-to-lisp-form-symbol)
      (setq xml-obj-to-lisp-form-symbol (cllib-symbol :xml-obj-to-lisp-form)))
    ;; I can't figure out how to use the damned CLLIB XML package to read
    ;; the XML directly from a string, or string stream.  But this works...
    (let ((tempdir (xml-temp-dir)))
      (utils:with-temp-file-in 
       (file tempdir :prefix "XML-" :name "temp" :type "xml" :if-exists :again)
       (with-open-file (f file :direction :output :if-exists :supersede)
	 (with-standard-io-syntax (format f "~A" xml-string)))
       (let* ((clxml (funcall xml-read-from-file-symbol file))
	      (clxml (funcall xml-obj-to-lisp-form-symbol clxml)))
	 (if (clxml-obj? clxml)
	     (simple-clxml-parse-obj clxml name-package)
	   (mapcar (lambda (x) (simple-clxml-parse-obj x name-package))
		   (remove-if-not #'clxml-obj? clxml)
		   )))))))

(defun simple-clxml-parse-obj (xml-obj name-package)
  (let ((name (clxml-obj-name-string xml-obj))
	(data (clxml-obj-data-values xml-obj)))
    (cons (intern name name-package) 
	  (simple-clxml-data-values-parse data name-package))))

(defun simple-clxml-data-values-parse (data-list name-package)
  (flet ((one-space-string? (x) (and (stringp x) (string= x " "))))
    (if (and (= (length data-list) 1) (one-space-string? (first data-list)))
	data-list
      (let ((despaced-list (remove-if #'one-space-string? data-list)))
	(mapcan
	 #'(lambda (datum)
	     (cond
	      ((stringp datum) (list datum))
	      ((clxml-obj? datum) 
	       (list (simple-clxml-parse-obj datum name-package)))
	      (t
	       (warn "Found ~S in XML-OBJ data.  It will be ignored." datum)
	       nil
	       )))
	 despaced-list
	 )))))


)

