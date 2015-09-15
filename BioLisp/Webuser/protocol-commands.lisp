;;; -*- mode: Lisp; Syntax: Common-Lisp; Package: wb; -*-

(in-package :wb)

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

(defparameter *system-protocols-directory* 
  (cl-user:translate-simple-lp "etc:protocols;"))

(defparameter *default-published-protocols-filename* "pp.lisp")

(defparameter *user-protocols-filename* "protocols.lisp")


(defun default-published-protocols-file ()
  (let ((file
         (merge-pathnames 
          *default-published-protocols-filename*
          *system-protocols-directory*)))
    (ensure-directories-exist file)
    file
    ))

(defun delete-protocol (name &rest names)
  "Delete existing protocol definitions"
  (flet ((obliterate-protocol (name)
           (fmakunbound name)
           (setq *protocol-names* (delete name *protocol-names*))))
    (let ((deleted-name nil))
      (cond
       ((get name :defprotocol)
        (setq deleted-name name)
        (remprop name :defprotocol)
        (remprop name :defprotocol-form)
        (remprop name :defprotocol-steps)
        (obliterate-protocol name))
       ((get name :def-protocol)
        (setq deleted-name name)
        (remprop name :def-protocol)
        (obliterate-protocol name))
       (t (format t "Not a known protocol: ~A~%" name)))
      (cons deleted-name (mapcan #'delete-protocol names))
      )))
        

(defun show-protocol (name &optional (p *standard-output*))
  "Pretty print a protocol's information"
  (cond
   ((get name :defprotocol)
    (let ((source (get name :defprotocol))
          (original-form (get name :defprotocol-form))
          (original-steps (get name :defprotocol-steps)))
      (let ((*print-level* nil) (*print-length* nil))
        (format p "~&~%;; Protocol substitution function: ~%")
        (terpprint source p)
        (format p "~%;; Original history lines used:~%")
        (dolist (step original-steps)
          (format p "    Line ~D:  ~S~%" (first step) (second step)))
        (terpri p)
        (format p ";; Original DEFPROTOCOL form:~%")
        (terpprint original-form p)
        )))
   ((get name :def-protocol)
    (let ((source (get name :def-protocol)))
      (let ((*print-level* nil) (*print-length* nil))
        (terpprint source p))))
   (t (format p "Not a known protocol: ~A~%" name)))
  nil)

(defun list-protocols () *protocol-names*)

(defun save-protocols 
       (&optional 
        (pnames :all)
        &key 
        (file (merge-pathnames 
               *user-protocols-filename* *default-pathname-defaults*))
        (mode (if (eq pnames :all) :supersede :append))
        (package (intern (package-name *package*) :keyword))
        )
  #.(one-string-nl 
     "Save a defined set of protocol definitions.  If PNAMES is :all, "
     "all currently defined protocols are saved.  The protocols are saved"
     "to FILE.")
  (when (not (keywordp package))
    (setq package (intern (package-name (find-package package)) :keyword)))
  (cond
   ((eq pnames :all)
    (save-protocols *protocol-names* file :mode mode :package package))
   ((symbolp pnames) 
    (save-protocols (list pnames) file :mode mode :package package))
   ((and pnames (listp pnames))
    (loop for name in pnames do
          (unless (symbolp name) (error "Invalid protocol name: ~A" name))
          (unless (member name *protocol-names*)
            (error "Unknown protocol: ~A" name)))
    (with-open-file 
        (p file :direction :output :if-exists mode :if-does-not-exist :create)
      (terpprint `(in-package ,package) p)
      (dolist (name pnames)
        (cond
         ((get name :def-protocol) (store-def-protocol name p))
         ((get name :defprotocol) (store-defprotocol name p))
         (t (error "No protocol definition for ~A" name))
         ))))
   (t (error "Unrecognized protocol names: ~A" pnames)))
  (namestring file))
          
(defun store-def-protocol (name p)
  (let* ((source (get name :def-protocol))
         (*print-level* nil) 
         (*print-length* nil))
    (terpprint source p)
    (terpprint `(setf (get ',name :def-protocol) ',source) p)
    (terpprint `(pushnew ',name *protocol-names*) p)
    ))

(defun store-defprotocol (name p)
  (let* ((source (get name :defprotocol))
         (original-form (get name :defprotocol-form))
         (original-steps (get name :defprotocol-steps))
         (*print-level* nil) 
         (*print-length* nil))
    (store-defprotocol-forms p name source original-form original-steps)
    ))

(defun store-defprotocol-forms (p name source original-form original-steps)
  (terpprint source p)
  (terpprint `(setf (get ',name :defprotocol) ',source) p)
  (terpprint `(setf (get ',name :defprotocol-form) ',original-form) p)
  (terpprint `(setf (get ',name :defprotocol-steps) ',original-steps) p)
  (terpprint `(pushnew ',name *protocol-names*) p)
  (terpri p)
  )

(defun load-protocols 
       (&key 
        (file *user-protocols-filename* file-provided?)
        (system nil)
        (verbose? t)
        )
  #.(one-string-nl
     "Load a file of protocol definitions.  FILE should be just a filename,"
     "not a full pathname.  If SYSTEM is T and FILE is not"
     "provided the system protocol file is loaded.  If SYSTEM is T and FILE"
     "is provided a protocol file named FILE in the system's protocols"
     "directory is loaded (if it exists).  If SYSTEM is NIL a file called"
     "FILE is loaded from a default directory (usually the user's home"
     "directory).")
  (cond
   (system
    (if (not file-provided?) 
        (setq file (default-published-protocols-file)))
    (setq file (merge-pathnames file *system-protocols-directory*)))
   (t (setq file (merge-pathnames file *default-pathname-defaults*))))
  (let ((filename (namestring file)))
    (unless (probe-file file)
      (error "Protocol file ~A does not exist or is not accessible" filename))
    (when verbose? (cformatt "Loading protocols file ~A" filename))
    (c/l file)
    filename
    ))


;;; This probably isn't going to work if you try to publish
;;; to a package other than :pp

(defun publish-protocol
       (name
        &key
        (file (default-published-protocols-file))
        (mode :append)
        (package (find-package :pp))
        (really? nil)
        (verbose t)
        &aux file-exists?
        )
  (when (not (keywordp package))
    (setq package (keywordize (package-name (find-package package)))))
  (unless (and (symbolp name) (member name *protocol-names*))
    (error "Unknown protocol: ~A" name))
  (ensure-directories-exist file :verbose verbose)
  (setq file-exists? (probe-file file))
  (when verbose
    (let ((filepath 
           (namestring (translate-logical-pathname (pathname file)))))
      (if file-exists?
          (progn
            (cformatt "~File ~A already exists." filepath)
            (cformatt "Appending protocol ~S to end." name))
        (format t "~&;; Creating file ~A~%" filepath))))
  (unless (eq mode :append)
    (when file-exists?
      (unless really?
        (error "Do you REALLY want to replace the existing file???~%~A"
               "(If so, use :really? t in your call to PUBLISH-PROTOCOL)"))))
  (with-open-file 
      (p file :direction :output :if-exists mode :if-does-not-exist :create)
    (let* ((user-package *package*)
           (*package* package)
           (source (pp-packageize (get name :defprotocol)))
           (original-form (pp-packageize (get name :defprotocol-form)))
           (original-steps (pp-packageize (get name :defprotocol-steps)))
           (*print-level* nil) 
           (*print-length* nil)
           (pp-name (second source)))
      (unless (and (eq mode :append) file-exists?)
        (terpprint `(in-package ,package) p))
      (store-defprotocol-forms p pp-name source original-form original-steps)
      (terpprint 
       `(eval-when (:compile-toplevel :load-toplevel :execute)
          (export ',pp-name)) p)
      (terpri p)
      (when verbose (cformatt "Protocol ~A written" (symbol-name pp-name)))

      ;; Now make the published protocol symbol completely obliterate
      ;; NAME with respect to the user's current package.
      ;; This so that one can load the file the function was published
      ;; to without getting a package conflict.

      (setf (symbol-function pp-name) (symbol-function name))
      (setf (get pp-name :defprotocol) source)
      (setf (get pp-name :defprotocol-form) original-form)
      (setf (get pp-name :defprotocol-steps) original-steps)
      (shadowing-import pp-name user-package)

      pp-name

      )))


(defun pp-packageize (form)
  (let ((ok-external-packages 
         (mapcar 
          'find-package 
          (append
           (wb::application-packages-to-use cl-user:*ai*)
           '(:common-lisp :utils :frames))))
        (ok-internal-packages
         (mapcar #'find-package '(:$$ :webuser)))
        (warn-list nil))
    (labels ((ok-external-symbol (s)
             (dolist (package ok-external-packages)
               (do-external-symbols (var package)
                 (when (eq s var) (return-from ok-external-symbol t))))
             nil)
           (ok-internal-symbol (s) 
             (member (symbol-package s) ok-external-packages))
           (do-it (form)
             (cond
              ((symbolp form)
               (cond
                ((or (keywordp form) (eq form nil) (eq form t)) form)
                ((member (symbol-package form) ok-internal-packages) form)
                ((ok-external-symbol form) form)
                ((ok-internal-symbol form)
                 (unless (member form warn-list)
                   (warn "PROTOCOL source contains ~S, an internal symbol ~A"
                         form (format nil "in the ~A package"
                                (package-name (symbol-package form)))))
                 (pushnew form warn-list)
                 form)
                (t (intern (symbol-name form) (find-package :pp)))))
              ((not (consp form)) form)
              (t
               (cons (do-it (car form)) (do-it (cdr form)))
               ))))
      (do-it form)
      )))
