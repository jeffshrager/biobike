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


;;;; **** ALL CODE SHOULD BE WRITTEN TO 

;;;; **** -- CALL TRANSLATE-SIMPLE-LP on the appropriate 'simple' host
;;;; ****    and subdirectories string at ***>> EXECUTION <<*** time.
;;;; **** -- Then manipulate the resulting path using portable pathname
;;;; ****    functions such as those found in the utilities.

;;;; Actual logical hosts are defined in weblistener-configuration.lisp
;;;; and in the application configuration file.


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun weblistener-system-load-error (format-string &rest args)
    (format t "~%;; *** SYSTEM ERROR WHILE LOADING WEBLISTENER/APPLICATION.~%")
    (apply 'format t format-string args)
    (error "*** SYSTEM ERROR WHILE LOADING WEBLISTENER/APPLICATION.")
    ))


;;; Try to insure that the pathnames provided for the translations of
;;; the logical pathname hosts are existing, usually writeable directories.

(defun verify-pathname-symbol 
       (symbol &key (nil-ok? t) (directory-must-be-writeable? t))
  (cond
   ((not (boundp symbol))
    (weblistener-system-load-error 
     "The symbol ~A is not bound! Set it to NIL in your init file to stop ~A" 
     symbol "attempts by BioLingua to use that facility."))
   (t
    ;; Get the actual pathname pointed at by SYMBOL
    (let ((value (symbol-value symbol)))
      (cond
       ((null value)
        (cond
         (nil-ok? t)
         (t (weblistener-system-load-error
             "The symbol ~A cannot be NIL.  Provide a value for it ~A"
             symbol "in the configuration file."))))
       ((stringp value)
        (handler-case (setq value (pathname value))
          (error 
           ()
           (weblistener-system-load-error 
            "The pathname string ~S which is the value of the symbol ~A ~A"
            value symbol "is not a valid pathname."))))
       ((pathnamep value) nil)
       (t 
        (weblistener-system-load-error
         "The symbol ~A has a value ~A, ~A" symbol value
         "which is neither a pathname nor pathname-string")))
      (unless (null value)
        ;; It's now guarenteed to be a pathname
        (let ((pathname-string (namestring value))
              (dir-component (pathname-directory value)))
          ;; Make sure it's a directory pathname, not a filename
          (when (or (and (pathname-name value)
                         (not (eq :unspecific (pathname-name value))))
                    (and (pathname-type value)
                         (not (eq :unspecific (pathname-type value)))
                         ))
            (weblistener-system-load-error
             "The symbol ~A ~A ~A, ~A ~A ~A" symbol
             "is bound to a pathname whose namestring is" pathname-string
             "which has a NAME or TYPE component,"
             "but it should specify a directory, not a file."
             "(You might need a slash or backslash at the end of the string.)"
             ))
          ;; Make sure its an absolute pathname
          (when (or (null dir-component) 
                    (not (listp dir-component))
                    (not (eq :absolute (first dir-component))))
            (weblistener-system-load-error
             "The symbol ~A ~A ~S, ~A ~A ~S" symbol
             "is bound to a pathname whose namestring is" pathname-string
             "which does not have an absolute directory component."
             "The directory component is" dir-component
             ))
          ;; Make sure the directory exists (or create it if it does not)
          (handler-case
              (ensure-directories-exist value)
            (error
             ()
             (weblistener-system-load-error
              "The symbol ~A ~A ~A, ~A ~A" symbol
              "is bound to a pathname whose namestring is" pathname-string
              "but the directory so designated either does not exist"
              "or cannot be accessed by this program."
              )))
          ;; Make sure we have write access to the directory
          (handler-case
              (let ((test-file-path (merge-pathnames "tmp.tmp" value)))
                (with-open-file (p test-file-path
                                   :direction :output :if-exists :supersede)
                  #+:LISPWORKS
                  (declare (ignore p))
                  )
                (delete-file test-file-path))
            (error
             ()
             (cond
              ((or (and directory-must-be-writeable? 
                        ;; hack for biotools which doesn't need to be 
                        ;; writeable by anyone ever
                        (not (eq :never directory-must-be-writeable?))
                        ;; Make compiler not generate warning about variable
                        (symbol-value 
                         'cl-user::*readable-directories-writeable?*)))
               (weblistener-system-load-error
                "Write access denied to directory ~A, pointed at by symbol ~A"
                pathname-string symbol
                ))
              (t 
               (warn "*** Directory ~A is not writeable." pathname-string)
               ))))))
          
      ;; Okay, the directory is as valid as we can make it.
      ;; Set the symbol to the directory's pathname.

      (set symbol value)

      ))))


(defun create-simple-logical-pathname-host

       (host-string physical-pathname-symbol 
                    &key (nil-ok? nil) (directory-must-be-writeable? t))

  (verify-pathname-symbol 
   physical-pathname-symbol 
   :nil-ok? nil-ok? 
   :directory-must-be-writeable? directory-must-be-writeable?)

  (when (symbol-value physical-pathname-symbol)
    (add-simple-lp-host 
     host-string
     (namestring (symbol-value physical-pathname-symbol)))
    (setf (get physical-pathname-symbol :logical-pathname)
          (concatenate 'string host-string ":")
          ))

  )


