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

;;; CREATE-USER-SPECIFIC-ARGLIST-DATA

(in-package :wb)

;;; The file with all the javascript code in it for editing 
;;; multiline boxes.

(defparameter +javascript-minieditor-name+ "minieditor")
(defparameter +javascript-newminieditor-name+ "newminieditor")

(defparameter +javascript-minieditor-source-file+
 (make-pathname 
  :name +javascript-minieditor-name+ 
  :type "js"
  :defaults (cl-user:translate-simple-lp "websrc:Webdefs;")
  ))

(defparameter +javascript-newminieditor-generated-source-file+
 (make-pathname 
  :name +javascript-newminieditor-name+ 
  :type "js"
  :defaults (cl-user:translate-simple-lp "websrc:Webdefs;")
  ))

(defparameter +javascript-newminieditor-real-source-file+
 (make-pathname 
  :name +javascript-newminieditor-name+ 
  :type "ls"
  :defaults (cl-user:translate-simple-lp "websrc:Webdefs;")
  ))

(defparameter +javascript-minieditor-url+
  (formatn "/~A" +javascript-minieditor-name+))

(defparameter +javascript-newminieditor-url+
  (formatn "/~A" +javascript-newminieditor-name+))

;;; This should be used by the redisplay listener code.

(defparameter +javascript-minieditor-include-string+
  (formatn
   "<SCRIPT LANGUAGE=\"Javascript\" SRC=~S> </SCRIPT>"
   +javascript-minieditor-url+
   ))

(defparameter +javascript-newminieditor-include-string+
  (formatn
   "<SCRIPT LANGUAGE=\"Javascript\" SRC=~S> </SCRIPT>"
   +javascript-newminieditor-url+
   ))

(publish-file
 :path +javascript-minieditor-url+
 :file +javascript-minieditor-source-file+
 :content-type cl-user::*html-publish-content-type*
 )

(defun compile-lispscript-file (input-file)
  (let ((physical-path (utils::webpath->lisp-physical-pathname input-file)))
    (unless (probe-file physical-path)
      (error 
       "The file ~A, translated to ~A, does not exist or is not accessible"
       input-file physical-path
       ))
    (unless (string-equal "LS" (pathname-type physical-path))
      (error "Lispscript files must end in .ls !!"))
    (com.gigamonkeys.foo::generate-from-file 
     com.gigamonkeys.foo.lispscript::*lispscript* physical-path)
    (cformatt "File ~A compiled from lispscript to javascript." input-file)
    ))

#+allegro
(com.gigamonkeys.twas::publish-generated-file
 :path +javascript-newminieditor-url+
 :file +javascript-newminieditor-generated-source-file+
 :source-file +javascript-newminieditor-real-source-file+
 :generator 
 (lambda (input output)
   (com.gigamonkeys.foo::generate-from-file
    com.gigamonkeys.foo.lispscript::*lispscript* input output))
 :content-type "text/plain")
 

;;; The file with all the javascript data for arglist list lookup.
;;; There is a file for each user, so we have functions instead
;;; of fixed parameters as above.

(defparameter +javascript-arglist-file-name+ "arglist-data")

(defun user-specific-arglist-javascript-file (username)
  (make-pathname
   :name +javascript-arglist-file-name+
   :type "js"
   :defaults (visitor-directory (string username))
   ))
   
(defun user-specific-arglist-javascript-url (username)
  (formatn 
   "/~A-~A"
   (string-downcase (string username))
   +javascript-arglist-file-name+
   ))

;;; This should be used by the redisplay listener code.

(defun user-specific-javascript-arglist-include-string (username)
  (formatn
   "<SCRIPT LANGUAGE=\"Javascript\" SRC=~S> </SCRIPT>"
   (user-specific-arglist-javascript-url (string-downcase (string username)))
   ))

;;; This should get called at user login time. It does the following:
;;; -- Creates a file called arglist-data.js in the user's
;;; home directory.
;;; -- Publishes a URL which will access that page.  The name of the URL
;;;  is "/<<username>>-arglist-data"

;;; It can also be called any time you want to update the arglist data.
;;; (There is no harm in republishing a URL)

(defun create-user-specific-arglist-data (username)
  (user-specific-arglist-javascript 
   username (user-specific-arglist-javascript-file username))
  (publish-file 
   :path (user-specific-arglist-javascript-url username)
   :file (user-specific-arglist-javascript-file username)
   :content-type "text/plain"
   ))

(defun user-specific-arglist-javascript (user-package file)
  (with-open-file (p file :direction :output :if-exists :supersede)
    (flet ((jsout (format-string &rest format-args)
             (apply 'format p format-string format-args)
             (terpri p)
             ))
      (multiple-value-bind (sorted-fbound-symbols-per-package packages)
          (accessible-fbound-symbols-of-package user-package)
        (jsout "var arglistaa = new Array();")
        (jsout "var arglistaainitialized = false;")
        (jsout "")
        (jsout "function initarglistaa () {")
        (jsout "")
        (loop for sorted-symbol-list in sorted-fbound-symbols-per-package
              for pkg in packages do
              (jsout 
               (if (eq pkg user-package)
                   "  /* Fbound symbols in user package ~A */"
                 "  /* Fbound symbols exported from used package ~A */")
               (package-name pkg))
              (dolist (s sorted-symbol-list)
                ;; Can't index into an associative array in javascript
                ;; using 'length' because it is essentially a reserved word.
                (unless (eq s 'length)
                  (let* ((arglist (system-specific-arglist-of s))
                         (arglist-string (arglist-to-pretty-string arglist)))
                    (jsout "  arglistaa[~S] = ~S;" 
                           (string-downcase (symbol-name s))
                           (string-downcase arglist-string)
                           ))))
              (jsout "")
              ))
      (jsout "  arglistaainitialized = true;")
      (format p "}~%")
      )))

(defun arglist-to-pretty-string (arglist)
  (cond 
   ((null arglist) "()")
   (t
    ;; Hack to make #' go away in arglist display... (e.g., for maphash)
    (when (eq (first arglist) 'function) (setf (first arglist) 'afunction))
    (string-join
     (remove-if 
      (lambda (x) (zerop (length x)))
      (string-split 
       (substitute #\Space #\Newline (formatn "~A" arglist))
       ))))))
             
(defun accessible-fbound-symbols-of-package (package)
  ;; We put symbols into a hash table to make sure we deal with
  ;; each unique symbol exactly once henceforth
  (let* ((ht (make-hash-table :test 'eq))
         (used-packages (package-use-list package))
         (all-packages (cons (find-package package) used-packages))
         )
    ;; Put ALL fbound symbols found in PACKAGE into a hash table
    (do-symbols (s package)
      (when (fboundp s) (setf (gethash s ht) package)))
    ;; Put the fbound EXTERNAL symbols in all the used packages into a hash
    (dolist (pkg used-packages)
      (do-external-symbols (s pkg)
        (when (fboundp s) (setf (gethash s ht) pkg))))
    
    (values
     (mapcar
      (lambda (symbol-list) (sort symbol-list 'string< :key 'symbol-name))
      (loop for p in all-packages
            collect
            (lmaphashnn (lambda (k v) (and (eq v p) k)) ht)
            ))
     all-packages
     )))

#|

Not implemented unless we need this.

;;; Provide a mechanism to disable arglist processing / download of
;;; javascript in case it is burdensome.

(defun enable-arglist-processing ()
  (setf (get (user-session-id) :arglist-processing) :enabled))
(defun disable-arglist-processing ()
  (setf (get (user-session-id) :arglist-processing) :disabled))
(defun arglist-processing-disabled? ()
  (eq :disabled (get (user-session-id) :arglist-processing)))
  
|#