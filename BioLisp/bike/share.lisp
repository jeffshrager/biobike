;;; -*- mode: Lisp; Syntax: Common-Lisp; Package: bbi; -*-

(in-package :bbi)

;;; +=========================================================================+
;;; | Copyright (c) 2007 Mark Slupesky, JP Massar                             | 
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

;; Author: Mark Slupesky, JP Massar.

(defparameter *share-directory-name* "share94703")
(defparameter *share-info-file* "share94703")
(defparameter *share-directory* 
  (append-subdir wb::*home-directory* *share-directory-name*))
(defparameter *share-info-file-path* 
  (merge-pathnames *share-info-file* *share-directory*))

(defun share-directory () *share-directory*)
(defun share-directory-index-file () *share-info-file-path*)

(defun find-share-record (current-share-info name)
  (find name current-share-info :test 'string-equal :key 'first))

(defun share-record-value (record property) (getf (cdr record) property))

(defun maybe-create-share-directory ()
  ;; Create the directory where all the shared packages 
  ;; will go if it doesn't exist
  (ensure-directories-exist 
   (merge-pathnames 
    "foo" 
    (share-directory)
    ))
  ;; Create the index file for all the shared packages, if it doesn't exist 
  (unless (probe-file (share-directory-index-file))
    (with-open-file 
        (p (share-directory-index-file)
           :direction :output
           :if-exists :supersede 
           :if-does-not-exist :create)
      (pprint nil p)
      )))

(defun read-shared-info ()
  (with-open-file 
      (p (share-directory-index-file)
         :direction :input 
         :if-does-not-exist :error)
    (read p nil nil nil)
    ))

;;; This doesn't prevent two users from simultaneously modifying the 
;;; index file 
(defun write-shared-info-data (data)
  (with-open-file 
      (p (share-directory-index-file)
         :direction :output
         :if-exists :supersede 
         :if-does-not-exist :create)
    (let ((*print-length* nil)
          (*print-level* nil))
      (pprint data p)
      )))

(defun add-shared-info-record (record)
  (let ((current-info (read-shared-info))
        (name (first record)))
    (write-shared-info-data 
     (cons
      record
      (delete name current-info :key 'first :test 'string-equal)
      ))))

;;; This doesn't prevent two users from simultaneously modifying the 
;;; index file 
(defun remove-share-package (name)
  (block exit
    (let* ((current-info (read-shared-info))
           (record (find-share-record current-info name)))
      (if record
          (let ((user (keywordize (share-record-value record :user)))
                (file (share-record-value record :file)))
            (unless (eq user wb::*username*)
              (unless (wb::weblistener-guru-p) 
                (formatt 
                 (one-string-nl
                  "You are not the owner of this package."
                  "You cannot remove it!"
                  ))
                (return-from exit nil)
                ))
            (write-shared-info-data 
             (delete name current-info :key 'first :test 'string-equal))
            (handler-case 
                (delete-file (merge-pathnames file (share-directory)))
              (error 
               (c)
               (formatt "Could not delete share package.~%Actual error: ~A" c)
               ))
            (formatn "Shared package ~A no longer exists" name)
            )
        (formatt "No user-contributed package called ~A exists..." name)
        ))))

(defun use-share-function (name &key (delete-load-files? t))
  (maybe-create-share-directory)
  (let* ((share-info (read-shared-info))
         (canonical-name (string-upcase name))
         (shared-record (find-share-record share-info name)))
    (unless shared-record 
      (error "No user-contribued package called ~A seems to exist!" 
             canonical-name))
    (let* ((file (share-record-value shared-record :file))
           (functions (share-record-value shared-record :functions))
           (variables (share-record-value shared-record :variables))
           (copied-file 
            (merge-pathnames file (wb::user-directory wb::*username*)))
           (filepath (merge-pathnames file (share-directory))))
      (unless (string-equal (pathname-type file) "bike")
        (error "Shared files must be .bike files!"))
      (copy-text-file 
       filepath copied-file 
       :prepend (formatn "(in-package ~S)~%~%" wb::*username*))
      (handler-case 
          (bbi::bbcl copied-file)
        (error 
         (c)
         (error 
          (formatn
           (one-string-nl
            "Problem attempting to use shared package ~A."
            ""
            "Actual error: ~A"
            ""
            "You might want to contact the owner of the"
            "package, ~A, or contact the system administrators."
            )
           name c (share-record-value shared-record :user)
           ))))
      (handler-case 
          (when delete-load-files?
            (let ((lisp-file (pathname-of-new-type copied-file "lisp"))
                  (fasl-file (pathname-of-new-type copied-file "fasl")))
              (delete-file fasl-file)
              (delete-file lisp-file)
              (delete-file copied-file)
              ))
        (error 
         (c)
         (wb::log-user-event 
          (one-string-nl
           "Problem deleting residual shared package files for package ~A"
           "Actual error: ~A"
           )
          name c
          )))
      (when wb::*vpl-executing?* 
        (loop for f in functions do 
              (forward-package-funcall 
               :vpl :add-define-function-symbol-for-user f))
        (loop for v in variables do
              (forward-package-funcall :vpl :add-define-symbol-for-user v)
              )))
    ;; keep track of which packages the user is sharing 
    (setf (get wb::*username* :shared-packages) 
          (delete name (get wb::*username* :shared-packages) 
                  :test 'string-equal :key 'first
                  ))
    (push shared-record (get wb::*username* :shared-packages))
    shared-record
    ))

(defun share-function 
       (name define-functions-list variables-list description remove? 
             &aux (canonical-name (string-upcase name)))
  (maybe-create-share-directory)
  (block exit
    (when (and remove? (or define-functions-list variables-list))
      (formatt 
       (one-string-nl
        "Do not provide functions or variables when removing a package!"
        "If you wish to change the package definition, re-share the package"
        "using the same name and different functions and variables."
        ))
      (return-from exit nil))
    (when remove? 
      (return-from exit (remove-share-package canonical-name)))
    (let* ((foops nil)
           (fdefs 
            (loop for f in define-functions-list 
                  as def = (get f :procedure-definition)
                  if (and def (eq (first def) 'bbi::define-function))
                  collect def  
                  else do (push def foops) 
                  ))
           (voops nil)
           (vvalues 
            (loop for v in variables-list 
                  if (boundp v) 
                  collect (list v (symbol-value v))
                  else do (push v voops)
                  ))
           (share-file 
            (pathname-of-new-type 
             (merge-pathnames canonical-name (share-directory))
             "bike"
             )))
      (when (or foops voops)
        (formatt "Cannot share the following: ~%")
        (when foops
          (loop for f in foops do (formatt "~S is not a define-function!" f)))
        (when voops
          (loop for v in voops do (formatt "~S has no value defined!" v)))
        (return-from exit nil)
        )
      (when (and (null fdefs) (null vvalues))
        (formatt "You cannot share a package with no functions or variables!")
        (return-from exit nil))
      ;; Don't let anyone overwrite a package that isn't theirs.  
      (let* ((shared-info (read-shared-info))
             (existing-record (find-share-record shared-info canonical-name)))
        (when existing-record 
          (let ((user (share-record-value existing-record :user)))
            (unless (eq user wb::*username*)
              (unless (wb::weblistener-guru-p) 
                (formatt 
                 (one-string-nl
                  "There is already a shared package named ~A, owned by ~A."
                  "You are not allowed to overwrite it."
                  "Please choose a different name."
                  )
                 name user
                 )
                (return-from exit nil)
                )))))
      (let ((*print-length* nil)
            (*print-level* nil))
        (with-open-file (p share-file :direction :output :if-exists :supersede)
          (loop for (v val) in vvalues do 
                (format p "(lisp:defvar ~A)~%" v)
                (typecase val
                  (cons (format p "(define ~A = '~S)~%~%" v val))
                  (otherwise (format p "(define ~A = ~S)~%~%" v val))))
          (loop for def in fdefs do (pprint def p) (terpri p))
          ))
      (let ((record 
             (list
              canonical-name 
              :file (file-namestring share-file)
              :user wb::*username*
              :functions define-functions-list 
              :variables variables-list 
              :description (or description "")
              )))
        (add-shared-info-record record)
        )
      canonical-name
      )))

(define-macro share
  required name 
  keyword functions = nil
  keyword variables = nil
  keyword docstring = nil
  flag remove 
  type name = (or string symbol)
  type functions = (or list string symbol)
  type variables = (or list string symbol)
  type docstring = (or null string)
  body 
  (unless (or (stringp name) (symbolp name) (quoted-symbol-p name))
    (error "First argument to SHARE must be a name, not a ~A" (type-of name)))
  (let ((name (string (unquote name)))
        (functions (unquote (ensure-list functions)))
        (variables (unquote (ensure-list variables))))
    (flet ((canonicalize-symbol-list (list type)
             (loop for f in list
                   do
                   (unless (or (symbolp f) (stringp f))
                     (error "Cannot recognize ~A as a ~A name!" f type))
                   collect 
                   (find-symbol (string-upcase (string f)) wb::*username*)
                   )))
      (setq functions (canonicalize-symbol-list functions "functions"))
      (setq variables (canonicalize-symbol-list variables "variables"))
      (when docstring
        (unless (stringp docstring)
          (error "Docstring must be a string!")))
      `(share-function ,name ',functions ',variables ,docstring ,remove)
      )))

(define-macro unshare
  required name
  type name = (or string symbol)
  body 
  (typecase name 
    (string `(unshare-shared-package ,name))
    (symbol `(unshare-shared-package ,(string name)))
    (otherwise (error "Don't know how to unshare ~A!" name))
    ))

(defun unshare-shared-package (name)
  (block exit
    (let* ((current-info (get wb::*username* :shared-packages))
           (share-record (find-share-record current-info name)))
      (unless share-record
        (formatt "You are not currently using a shared package named ~A" name)
        (return-from exit nil)
        )
      (let ((funcs (share-record-value share-record :functions))
            (vars (share-record-value share-record :variables)))
        (if wb::*vpl-executing?*
            (progn 
              (loop for f in funcs do 
                    (forward-package-funcall 
                     :vpl :expunge-symbol-from-my-menu 
                     f :my-vpl-functions :error :ignore
                     ))
              (loop for v in vars do 
                    (forward-package-funcall
                     :vpl :expunge-symbol-from-my-menu 
                     v :my-vpl-variables :error :ignore
                     )))
          (progn
            (loop for f in funcs do (when (fboundp f) (fmakunbound f)))
            (loop for v in vars do (when (boundp v) (makunbound v)))
            ))
        (setf (get wb::*username* :shared-packages)
              (delete name (get wb::*username* :shared-packages) 
                      :test 'string-equal :key 'first
                      ))
        (formatn "Shared package ~A no longer in use." name)
        ))))
              


             
    

    


  

