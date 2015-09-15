;;; -*- Package: weblistener; mode: lisp; base: 10; Syntax: Common-Lisp; -*-

(in-package :weblistener) 

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


;;; CODE THAT PREVENTS PEOPLE FROM REDEFINING FUNCTIONS AND VARIABLES
;;; WHICH DON'T EXPLICITLY BELONG TO THEM.

(defparameter *guru-allow-arbitrary-redefinitions* nil)

(defun redefinition-check (symbol type &key (generate-error? t))
  (block exit
    (unless (user-session-id) (return-from exit nil))
    (ecase (user-mode)
      (:biolisp 
       (let* ((cp *package*)
              (sp (symbol-package symbol))
              (packages-to-protect (package-use-list cp)))
         ;; If 
         ;;   -- we're in the context of the weblistener, and
         ;;   -- we're either not a guru or the flag allowing redefinitions
         ;;      for gurus is disabled, and
         ;;   -- we're not in the package of the symbol
         ;;      we're trying to define, and
         ;;   -- the symbol we're trying to redefine is in one of the packages
         ;;      used by our package
         ;; Then
         ;;   -- we have a potential problem.
         ;;   -- If we're redefining an existing functional or variable 
         ;;      definition
         ;;      then we have a real problem, otherwise allow definition.
         ;; Else
         ;;  allow the (re)definition.
         (if (and (user-session-id)
                  (or (not (weblistener-guru-p))
                      (not *guru-allow-arbitrary-redefinitions*))
                  (not (eq sp cp))
                  (member sp packages-to-protect))
             (cond
              ((and (fboundp symbol)
                    (member type '(:defun :defmacro :defgeneric)))
               (invalid-function-redefinition-message symbol type)
               (when generate-error? (redefinition-error symbol))
               nil)
              ((and (boundp symbol)
                    (member type '(:defconstant :defparameter :defvar)))
               (invalid-variable-redefinition-message symbol type)
               (when generate-error? (redefinition-error symbol))
               nil
               )
              (t t)
              )
           t)))
      (:bbl 
       (when (eq (symbol-package symbol) (find-package *username*))
         (return-from exit nil))
       (when (weblistener-guru-p) 
         (when *guru-allow-arbitrary-redefinitions*
           (return-from exit nil))
         (when (not (eq (symbol-package symbol) (find-package :bbi)))
           (error 
            #.(one-string-nl
               "Even though you are a guru, you need to set"
               "wb::*guru-allow-arbitrary-redefinitions* in order"
               "to change the definition of ~A.")
            symbol))
         (return-from exit nil))
       (cond
        ((and (fboundp symbol) 
              (member type '(:defun :defmacro :defgeneric :define-function 
                              :define-macro)))
         (invalid-bbl-redefinition-message symbol type)
         (when generate-error? (redefinition-error symbol))
         )
        ((and (boundp symbol)
              (member type '(:defconstant :defparameter :defvar)))
         (invalid-bbl-redefinition-message symbol type)
         (when generate-error? (redefinition-error symbol))
         )
        (t 
         (warn "(Re)defining ~S as a ~S, but ~S is not in your package!"
               symbol type symbol
               )))
       nil
       ))))
        
(defun invalid-bbl-redefinition-message (symbol type)
  (formatt
   (one-string-nl
    ""
    "***** >>>   ERROR TRYING TO (RE)DEFINE '~A'   <<< *****"
    ""
    "You are trying to define a ~A named ~S, but there already exists a"
    "definition for ~S in the BBL language or in the Weblistener software."
    "You are not allowed to redefine any existing definition other than"
    "your own!"
    ""
    "You probably don't really want to be redefining ~S."
    "A likely solution is simply to use a different name."
    "Please email/call the sysadmins if you don't understand this or"
    "need additional help/explanations."
    )
   symbol type symbol symbol symbol)
  (redefinition-error symbol)
  )       

(defun redefinition-error (s) (error "Redefinition failure: ~S" s))
      
(defun invalid-function-redefinition-message (symbol type)
  (let ((pts
         (ecase type
           (:defun "function")
           (:defgeneric "generic function")
           (:defmacro "macro")
           ))
        (ets
         (cond
          ((special-operator-p symbol) "special common lisp operator")
          ((macro-function symbol) "macro")
          ((typep (symbol-function symbol) 'generic-function) 
           "generic function")
          (t "function")
          )))
    (generic-invalid-redefinition-message symbol pts ets)
    ))

(defun invalid-variable-redefinition-message (symbol type)
  (let ((pts
         (ecase type
           (:defvar "global variable")
           (:defparameter "global parameter")
           (:defconstant "global constant")
           ))
        (ets
         (cond
          ((constantp symbol) "global constant")
          (t "global variable")
          )))
    (generic-invalid-redefinition-message symbol pts ets)
    ))

(defun generic-invalid-redefinition-message (symbol pts ets)
  (let ((pn (package-name (symbol-package symbol))))
    (formatt
     (one-string-nl
      ""
      "***** >>>   ERROR TRYING TO (RE)DEFINE '~S'   <<< *****"
      ""
      "You are trying to define a ~A ~A, but there already exists a"
      "~A called ~A in package ~A, and this ~A"
      "is used by your workspace. By redefining this ~A you would redefine it"
      "throughout the entire system, possibly rendering the system unusable."
      ""
      "You may not have realized this and probably don't want to be doing this!"
      "If so, just use a name other than ~A for your ~A."
      ""
      "If you really want your own ~A named ~A (and you don't care that"
      "you will not have access to the existing definition of ~A), then"
      "typing the following incantation will allow you to execute the"
      "definition you just typed without seeing this message again:"
      ""
      "(shadow '~A)"
      ""
      "This will eliminate the symbol ~S from your workspace (aka package)"
      "(but not from the entire system!) and give you your own symbol"
      "by the same name which you can then define as a ~A."
      ""
      (when (weblistener-guru-p)
        (one-string-nl
         "Another way to avoid seeing this message is to do:"
         ""
         "(in-package :~A)"
         ""
         "and then resubmit your definition form. But you should NOT do this"
         "unless you understand EXACTLY what you are doing because this WILL"
         "redefine ~A throughout the entire system!"
         ))
      "Please email/call the sysadmins if you don't understand this or"
      "need additional help/explanations."
      )
     symbol
     pts symbol ets symbol pn ets ets
     symbol pts
     pts symbol ets
     symbol
     symbol pts
     pn
     symbol
     )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; Auxiliary routines for dealing with workspace file naming conventions


(defconstant +vchar+ #\@)
(defparameter *vsuffix* "-wks")

(defun canonicalize-package-name (package)
  (cond
   ((packagep package) (canonicalize-package-name (package-name package)))
   ((or (stringp package) (symbolp package)) (keywordize package))
   (t (error "Not a package or possible package name: ~A" package))
   ))

(defun default-workspace-file (name)
  (setq name (canonicalize-package-name name))
  (pathname (formatn "~A~A.lisp" (string-downcase name) *vsuffix*)))

(defun versioned-workspace-file (name version)
  (setq name (canonicalize-package-name name))
  (pathname (formatn "~A~A~A~D.lisp" 
                     (string-downcase name) *vsuffix* +vchar+ version)))

(defun existing-workspace-file-full-pathname 
       (&key (package-name (package-name *package*))
             (user *username* us?)
             (directory (visitor-directory *username*) ds?))
  #.(one-string-nl
     "Find an existing unversioned workspace file."
     "If USER or DIRECTORY is specified, the search is limited to that"
     "user's home directory or the directory specified."
     "If neither is specified, the user's home directory is searched and"
     "then a system-wide location is searched, so that 'global' workspace"
     "files can be loaded.")
  (setq package-name (canonicalize-package-name package-name))
  (cond
   ((and ds? us?)
    (error "Do not specify both USER and DIRECTORY."))
   (us?
    (let ((path (merge-pathnames 
                 (visitor-directory (keywordize user))
                 (default-workspace-file package-name)
                 )))
      (and (probe-file path) path)
      ))
   (ds?
    (let ((path 
           (merge-pathnames 
            (pathname directory)
            (default-workspace-file package-name))))
      (and (probe-file path) path)
      ))
   (t
    (or (existing-workspace-file-full-pathname
         :package-name package-name :cl-user *username*)
        (existing-workspace-file-full-pathname
         :package-name package-name :cl-user :system)
        ))))
  

(defun unversioned-workspace-file? (pathname)
  (setq pathname (pathname pathname))
  (let ((name (pathname-name pathname))
        (type (pathname-type pathname)))
    (and (string-equal type "LISP")
         (> (length name) 4)
         (string-equal
          (reverse *vsuffix*) 
          (subseq (reverse name) 0 (length *vsuffix*)))
         pathname
         )))

(defun versioned-workspace-file? (pathname)
  (setq pathname (pathname pathname))
  (let* ((name (pathname-name pathname))
         (type (pathname-type pathname))
         (len (length name)))
    (and (string-equal type "LISP")
         (> len 6)
         (let* ((rname (reverse name))
                (vpos (position +vchar+ rname))
                (ms (1+ vpos))
                (me (+ ms (length *vsuffix*))))
           (when vpos
             (and (not (zerop vpos))
                  (string-equal (reverse *vsuffix*) (subseq rname ms me))
                  (every 'digit-char-p (subseq rname 0 vpos))
                  pathname
                  ))))))

(defun workspace-file-version (pathname)
  "Assumes PATHNAME is known to be a versioned workspace file name"
  (let* ((name (pathname-name (pathname pathname)))
         (vpos (position +vchar+ name)))
    (parse-integer (subseq name (1+ vpos)))
    ))
    
(defun workspace-file-identifier (pathname)
  "Assumes PATHNAME is known to be a workspace file name"
  (let* ((name (pathname-name (pathname pathname)))
         (vpos (position +vchar+ name)))
    (if vpos 
        (subseq name 0 (- vpos (length *vsuffix*)))
      (subseq name 0 (- (length name) (length *vsuffix*)))
      )))
              

(defun increment-workspace-version (pathname)
  (let ((pathname (pathname pathname)))
    (cond
     ((unversioned-workspace-file? pathname)
      (make-pathname 
       :name (one-string (pathname-name pathname) (string +vchar+) "1")
       :defaults pathname
       ))
     ((versioned-workspace-file? pathname)
      (make-pathname
      :name 
      (one-string 
       (workspace-file-identifier pathname)
       *vsuffix*
       (string +vchar+)
       (formatn "~D" (1+ (workspace-file-version pathname))))
      :defaults pathname
      ))
     (t (error "Not a workspace filename: ~A" pathname))
     )))
      
(defun create-next-workspace-version-pathname (workspace-pathname)
  (let ((next (increment-workspace-version workspace-pathname)))
    (if (probe-file next)
        (create-next-workspace-version-pathname next)
      next
      )))
        
(defun save-to-new-version (unversioned-workspace-pathname)
  (let ((new-version 
         (create-next-workspace-version-pathname 
          unversioned-workspace-pathname
          )))
    (rename-file unversioned-workspace-pathname new-version)
    new-version
    ))


;;; Auxiliary routines for saving a workspace to disk.


(defmacro with-workspace-syntax ((&optional (pkg *package*)) &body body)
  `(with-standard-io-syntax
     (let ((*print-readably* t)
           (*print-circle* t)
           (*print-array* t)
           (*print-gensym* t)
           (*package* ,pkg))
       ,@body
       )))

(defun form-is-not-printable? (form &optional (pkg *package*))
  (with-workspace-syntax (pkg)
    (handler-case (progn (formatn "~S" form) nil)
      (print-not-readable (c) (values :print-not-readable c))
      (error (c) (values :other-error c))
      )))

(defun pprint-definition-form-carefully 
       (form output-stream &optional (error-stream *error-output*))
  (multiple-value-bind (problem c)
      (form-is-not-printable? form)
    (if (null problem)
        (pprint form output-stream)
      (ecase problem
        (:print-not-readable 
         (format 
          error-stream
          (one-string-nl
           "*** Ruh roh.  The ~A definition form for ~A"
           "cannot be printed readably (i.e., so that it can be read back"
           "in to Lisp).  A likely problem is that the form contains an"
           "unprintable object like a hash table or a stream.  This definition"
           "will not be stored in your workspace and thus won't be available"
           "to you if the system restarts and you load from your workspace.")
          (first form) (second form)
          ))
        (:other-error
         (format
          error-stream
          (one-string-nl
           "*** Ruh roh.  The ~A definition form for ~A"
           "caused the printer to error out when trying to print it to your"
           "workspace file.  The error condition raised was: ~A"
           "This definition will not be stored in your workspace and thus will"
           "not be available to you if the system restarts and you load from"
           "your workspace.")
          (first form) (second form) c
          ))))))
                        

(defun symbol-sort-variable-by-time-key (s) 
  (or (get s :variable-definition-time) 0))
(defun symbol-sort-procedure-by-time-key (s)
  (or (get s :procedure-definition-time) 0))


(defun obtain-function-definition (fname)
  (when (fboundp fname)
    (cond
     ((special-operator-p fname) nil)
     ((get fname :procedure-definition))
     ((macro-function fname) 
      (function-lambda-expression (macro-function fname)))
     ((function-lambda-expression (symbol-function fname)))
     (t nil)
     )))

(defun obtain-arglist-from-function-def (fdef)
  (when (symbolp (first fdef))
    (ecase (keywordize (first fdef))
      ((:defun :defmacro) (third fdef))
      (:lambda (second fdef))
      )))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *workspace-known-symbol-types*
    '(known-constants 
      known-deftypes
      known-variables
      known-macros
      known-functions
      ))
  (defparameter *workspace-unknown-symbol-types*
    '(constants 
      variables 
      macros 
      generic-functions 
      functions 
      everything-else))
  (defparameter *package-classification-symbols*
    (append *workspace-known-symbol-types* *workspace-unknown-symbol-types*))
  (setf (get 'known-constants :deftime-property) :variable-definition-time)
  (setf (get 'known-variables :deftime-property) :variable-definition-time)
  (setf (get 'known-macros :deftime-property) :procedure-definition-time)
  (setf (get 'known-functions :deftime-property) :procedure-definition-time)
  (setf (get 'known-deftypes :deftime-property) :deftype-definition-time)
  )


(defun classify-package-symbols (pkg &key (sort-by nil))

  ;; generate ((a nil) (b nil) ...)
  (let #.(mapcar 
          (lambda (x) (list x nil))
          (copy-list *package-classification-symbols*))

    (let ((pkg (find-package pkg)))
      (do-symbols (s pkg)
        (let ((classified? nil))
          (when (eq (symbol-package s) pkg)
            ;; a DEFVAR with no initial value.
            (when (and (get s :variable-definition) (not (boundp s)))
              (push s known-variables)
              (setq classified? t))
            ;; Anything with a value
            (when (boundp s)
              (cond
               ((constantp s) 
                (cond
                 ((get s :variable-definition) (push s known-constants))
                 (t (push s constants))
                 ))
               ((get s :variable-definition) (push s known-variables))
               (t (push s variables)))
              (setq classified? t))
            ;; Anything that has some sort of function definition
            (when (fboundp s)
              (cond
               ((special-operator-p s) nil)
               ((macro-function s)
                (cond
                 ((get s :procedure-definition) (push s known-macros))
                 (t (push s macros))
                 ))
               ((typep (symbol-function s) 'generic-function)
                (push s generic-functions))
               ((get s :procedure-definition) 
                (push s known-functions))
               (t (push s functions)))
              (setq classified? t))
            ;; Not implemented yet.
            (when (get s :deftype-definition) 
              (push s known-deftypes)  
              (setq classified? t))
            ;; Anything we didn't catch otherwise.
            (when (not classified?) (push s everything-else))
            ))))

    (let ((symbol-lists-list 
           #.(cons 'list (copy-list *package-classification-symbols*))))
      (apply
       'values
       (ecase sort-by
         ((nil) nil)
         (:name
          (mapcar
           (lambda (list) 
             (sort (copy-list list) 'string-lessp :key 'symbol-name))
           symbol-lists-list
           ))
         (:time
          (mapcar
           (lambda (symbol-classification symbol-list)
             (vif 
              (p (get symbol-classification :deftime-property))
              (sort symbol-list '< :key (lambda (s) (or (get s p) 0)))
              (sort symbol-list 'string-lessp :key 'symbol-name)
              ))
           *package-classification-symbols*
           symbol-lists-list
           ))
         ))

      )))


(defun save-workspace-to-file (package file)
  (let* ((pkg (find-package package))
         (pname (string-downcase (package-name pkg))))
    (with-open-file (wks file :direction :output :if-exists :supersede)
      (format wks ";;; Created ~A~%~%" (make-timestamp-string))
      (format wks 
              "(cl:eval-when (:compile-toplevel :load-toplevel :execute)~%")
      (format wks "  (cl:when (cl:not (cl:find-package :~A))~%" pname)
      (format wks "    (wb::create-user-package :~A)))~%~%" pname)
      (format wks "(cl:in-package :~A)~%~%~%" pname)

      (multiple-value-bind
          #.(copy-list *package-classification-symbols*)
          (classify-package-symbols package :sort-by :time)          
        (declare (ignore everything-else))
        (with-workspace-syntax (pkg)
          (labels ((do-known-items (list property-key label)
                     (when list 
                       (title label)
                       (loop for sym in list do
                             (pprint-definition-form-carefully
                              (get sym property-key)
                              wks)
                             (nl 1))
                       (nl 1)))
                   (warn-not-available (sym type)
                     (cformatt
                      (one-string
                       "Warning: ~A is a ~A in the workspace but its"
                       "definition is not available.  It will not be saved.")
                      sym type))
                   (title (s) (format wks "~%;;; ~A~%~%" s))
                   (nl (n) (dotimes (j n) (terpri wks)))
                   )
            (do-known-items 
             known-constants :variable-definition "RECORDED CONSTANTS")
            (when constants
              (title "OTHER CONSTANTS PRESENT IN PACKAGE")
              (loop for sym in constants do
                    (pprint-definition-form-carefully 
                     `(defconstant ,sym ,(symbol-value sym)) wks)
                    (nl 1))
              (nl 1))
            (do-known-items 
             known-deftypes :deftype-definition "RECORDED TYPE DEFINITONS")
            (do-known-items 
             known-variables :variable-definition "RECORDED VARIABLES")
            (do-known-items 
             known-macros :procedure-definition "RECORDED MACRO DEFINITIONS")
            (loop for sym in macros do (warn-not-available sym "macro"))
            (do-known-items 
             known-functions :procedure-definition 
             "RECORDED FUNCTION DEFINITONS")
            (when functions
              (title "OTHER FUNCTION DEFINITIONS PRESENT IN PACKAGE")
              (loop for sym in functions do
                    (let ((def (obtain-function-definition sym)))
                      (if def
                          ;; FUNCTION-LAMBDA-DEFINITION returned a form.
                          (progn
                            (pprint-definition-form-carefully                 
                             `(defun ,sym ,@(rest def)) wks)
                            (nl 1))
                        (warn-not-available sym "function")
                        )))
              (nl 1))
            (loop for sym in generic-functions do
                  (warn-not-available sym "generic function"))
            (when variables
              (title "UNRECORDED VARIABLES WITH VALUES")
              (loop for sym in variables do
                    (pprint-definition-form-carefully 
                     `(setq ,sym ',(symbol-value sym)) wks)
                    (nl 1))
              (nl 1)
              )))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; INTERFACE ROUTINES

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun show-workspace 

       (&key (package *package*) (stream *standard-output*) 
             (all nil) (sort-by :time) (report-problems? t))

  #.(one-string-nl
     "Pretty print the contents of workspace PACKAGE to STREAM."
     "By default, only symbols with a definition attached to them are"
     "listed, but if ALL is T, all symbols in PACKAGE are listed."
     "The listing consists of the defining form for each symbol."
     "The listing is arranged by definition type, and within definition"
     "type by SORT-BY which can be :TIME (the default), or :NAME, which"
     "arranges them alphabetically."
     "If REPORT-PROBLEMS? is T (the default), definition forms which cannot"
     "be written out in a readable form are flagged.")

  (setq package (canonicalize-package-name package))

  (multiple-value-bind
      #.*package-classification-symbols*
      (classify-package-symbols package :sort-by sort-by)
    
    (let ((unprintable-vars nil) 
          (unprintable-defs nil)
          (s stream))

      (labels 
          ((longest (list)
             (let ((size 0))
               (loop for elem in list do 
                     (setq size (max size (length (string elem))))
                     finally (return size)
                     )))
           (show-vars-and-values (label-string symbol-names)
             (when symbol-names
               (format s "~A:~%" label-string)
               (let* ((size (longest symbol-names))
                      (format-string (formatn "  ~~~DA  ~~S~~%" size)))
                 (loop for sym in symbol-names do 
                       (when (and (boundp sym) 
                                  (form-is-not-printable? (symbol-value sym)))
                         (push sym unprintable-vars))
                       (format 
                        s format-string sym 
                        (if (boundp sym) (symbol-value sym) "<<Unbound>>")))

                 (terpri s)
                 )))
           (show-functions-and-maybe-args (label-string symbol-names)
             (when symbol-names
               (format s "~A:~%" label-string)
               (let* ((size (longest symbol-names))
                      (format-string (formatn "  ~~~DA" size)))
                 (loop for sym in symbol-names do
                       (format s format-string sym)
                       (vwhen (def (obtain-function-definition sym))
                         (when (form-is-not-printable? def)
                           (push sym unprintable-defs))
                         (let ((args (obtain-arglist-from-function-def def)))
                           (format s "  ~S" args)
                           ))
                       (terpri s)
                       ))
               (terpri s)
               )))

        (terpri s) (terpri s)
        (show-vars-and-values "KNOWN CONSTANTS" known-constants)
        (show-vars-and-values "OTHER CONSTANTS" constants)
        (show-vars-and-values "KNOWN DEFINED VARIABLES" known-variables)
        (show-vars-and-values "OTHER SYMBOLS WITH VALUES" variables)
        (show-functions-and-maybe-args "KNOWN MACRO DEFINITIONS" known-macros)
        (show-functions-and-maybe-args "OTHER MACRO DEFINITIONS" macros)
        (show-functions-and-maybe-args 
         "KNOWN FUNCTION DEFINITIONS" known-functions)
        (show-functions-and-maybe-args "OTHER FUNCTION DEFINITIONS" functions)
        (show-functions-and-maybe-args "GENERIC FUNCTIONS" generic-functions)
        (when known-deftypes nil)
        (when (and all everything-else)
          (format s "OTHER SYMBOLS PRESENT IN WORKSPACE:~%")
          (loop for s in everything-else do (format s "  ~A~%" s))
          (terpri))
        (when report-problems?
          (when unprintable-vars
            (format s "PROBLEMATIC VARIABLES.  THE VALUES OF THESE SYMBOLS~%")
            (format s "ARE NOT PRINTABLE SUCH THAT THEY CAN BE READ BACK.~%")
            (format s "CONSEQUENTLY THESE SYMBOL VALUES WILL NOT BE STORED~%")
            (format s "WHEN THE WORKSPACE IS SAVED.~%")
            (terpri s)
            (loop for sym in unprintable-vars do (format s "  ~A~%" sym)))
          (when unprintable-defs
            (format s "PROBLEMATIC DEFINITIONS.  THE DEFINITION FORMS FOR~%")
            (format s "THESE SYMBOLS ARE NOT PRINTABLE SUCH THAT THEY CAN~%")
            (format s "BE READ BACK IN. CONSEQUENTLY THESE DEFINITIONS~%")
            (format s "WILL NOT BE STORED WHEN THE WORKSPACE IS SAVED.~%")
            (terpri s)
            (loop for sym in unprintable-defs do (format s "  ~A~%" sym))))
        (terpri s)
        (values)
        ))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; Called by Weblistener login procedure

(defun enable-workspace (&optional (pkg *package*))
  "Enable tracking of DEF* forms for later saving to a workspace."
  (setq pkg (find-package (canonicalize-package-name pkg)))
  (shadowing-import *shadowing-import-def-form-symbols* pkg)
  (setf (get (keywordize (package-name pkg)) :workspace-tracking-enabled?) t)
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        

(defun create-workspace 
       (name &key (subpackage-of *package*) (separator "$"))
  #.(one-string-nl
     "Create a new workspace. The workspace will have a name formed"
     "by concatenating SUBPACKAGE-OF, SEPARATOR and NAME.  NAME is always"
     "uppercased. If SUBPACKAGE-OF is NIL, the workspace name is simply NAME."
     "E.g., If the current user is MASSAR, (CREATE-WORKSPACE :genes) creates a"
     "workspace named MASSAR&GENES.  If the workspace already exists no new"
     "workspace is created.  In either case the current package, *package*,"
     "is set to the new workspace's package.")
  (let ((new-package-name
         (one-string
          (if subpackage-of 
              (string (canonicalize-package-name subpackage-of))
            "")
          (if subpackage-of separator "")
          (string-upcase name)
          )))
    (prog1
        (setq
         *package*
         (vif 
          (pkg (find-package new-package-name))
          (progn
            (cformatt "Workspace ~A already exists." new-package-name)
            pkg)
          (create-user-package new-package-name)
          ))
      (cformatt "Now in workspace package  ~A" (package-name *package*))
      )))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun relative-directory? (pathname)
  (eq :relative (first (pathname-directory (pathname pathname)))))


(defun save-workspace 
       (&key
        (package *package*)
        (user *username* us?)
        (directory-path (visitor-directory *username*) ds?)
        (verbose? t)
        (version? t)
        &aux output-path output-name package-name
        )

  #.(one-string-nl
     "Save the contents of PACKAGE out to disk as a workspace file."
     "The file is created in USER's home directory or in DIRECTORY-PATH."
     "If the file already exists and VERSION? is T (the default), then the"
     "old file is saved as a version, otherwise it is overwritten."
     "Only certain defining forms are currently saved, along with the"
     "values of all variables.  If a form to be written out contains "
     "data that cannot be read back in, it is flagged and not written out.")

  (cond
   ((and us? ds?) (error "Do not specify both USER and DIRECTORY-PATH."))
   ((not ds?) (setq directory-path (visitor-directory user)))
   ((and ds? (relative-directory? directory-path))
    (setq directory-path 
          (merge-pathnames directory-path (visitor-directory user))))
   (t nil))

  (setq package-name (canonicalize-package-name package))
  (setq output-path 
        (merge-pathnames directory-path (default-workspace-file package)))
  (handler-case 
      (ensure-directories-exist output-path)
    (error 
     (c)
     (cformatt "Problem trying to access ~A" output-path)
     (cformatt "Actual file error: ~A" c)
     (cformatt "Unable to save workspace!")
     (return-from save-workspace nil)
     ))
  (setq output-name (namestring output-path))
  (when verbose?
    (cformatt "Saving workspace defined by package ~A" package-name)
    (cformatt "  into file ~A" output-path))
  (when (probe-file output-path) 
    (cond
     (version?
      (when verbose?
        (cformatt "Workspace file ~A exists... Being versioned." output-name))
      (save-to-new-version output-path))
     (t 
      (when verbose?
        (cformatt "Workspace file ~A exists... Being overwritten." output-name)
        ))))
  (save-workspace-to-file package-name output-path)
  (when verbose? 
    (cformatt "Workspace ~A saved to ~A" package-name output-name))
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun purge-workspace-versions

       (&key
        (package *package*)
        (user *username* us?)
        (directory-path (visitor-directory *username*) ds?)
        (delete-everything? nil)
        (save-n-versions 1)
        (verbose? t)
        &aux 
        workspace-file-path workspace-file-name 
        next-workspace-version version-count
        package-name
        )

  #.(one-string-nl
     "Remove 'surplus' backup workspace versions for workspace PACKAGE"
     "in either USER's home directory or in DIRECTORY-PATH.  By default"
     "the most recent backup version is saved, and renamed to be backup"
     "version 1.  If DELETE-EVERYTHING? is T (default NIL), then all the"
     "backup versions AND THE WORKSPACE FILE ITSELF are deleted."
     "Otherwise, SAVE-N-VERSIONS (default 1) backup versions are saved"
     "and renumbered appropriately; the rest are deleted.")

  (cond
   ((and us? ds?) (error "Do not specify both USER and DIRECTORY-PATH."))
   ((not ds?) (setq directory-path (visitor-directory user)))
   (t nil))

  (block exit

    (setq package-name (canonicalize-package-name package))
    (setq workspace-file-path 
          (merge-pathnames 
           directory-path (default-workspace-file package-name)))
    (setq workspace-file-name (namestring workspace-file-path))
    (when verbose? 
      (cformatt 
       "Purging workspace files in directory ~A for package ~A"
       (namestring directory-path) package-name))
    (unless (probe-file workspace-file-path)
      (cformatt "No workspace file ~A exists. No files are being deleted."
                workspace-file-name)
      (return-from exit nil))

    (setq next-workspace-version 
          (increment-workspace-version workspace-file-path))

    (cond

     (delete-everything?
      (delete-file workspace-file-path)
      (when verbose?
        (cformatt "Deleted current workspace file ~A" workspace-file-name))

      (loop for j from 1 do
            (cond
             ((probe-file next-workspace-version)
              (delete-file next-workspace-version)
              (setq next-workspace-version 
                    (increment-workspace-version next-workspace-version)))
             (t
              (when verbose?
                (cond
                 ((= j 1)
                  (cformatt "No previous versions found, none deleted."))
                 ((= j 2)
                  (cformatt "Deleted sole previous version."))
                 (t
                  (cformatt "Deleted previous versions ~A~D through ~A~D."
                            +vchar+ 1 +vchar+ (1- j))
                  )))
              (return)
              ))))

     (t
      (setq version-count
            (loop for j from 0 do
                  (unless (probe-file next-workspace-version)
                    (return j))
                  (setq next-workspace-version 
                        (increment-workspace-version next-workspace-version))))
      (cond
       ((< version-count save-n-versions)
        (cformatt 
         (one-string
          "You asked to save ~D version~P, while only ~D version~P "
          "exist.  Therefore no previous versions will be deleted.")
         save-n-versions save-n-versions version-count version-count)
        (return-from exit nil))
       ((= version-count save-n-versions)
        (when verbose?
          (let ((vc version-count))
            (cformatt 
             (one-string
              "You asked to save ~D version~P, and ~D version~P exist~P. "
              "No versions will be deleted.")

             vc vc vc vc (if (> vc 1) 1 2))))
        (return-from exit nil))
       (t 
        (setq next-workspace-version 
              (increment-workspace-version workspace-file-path))       
        (let ((ndel (- version-count save-n-versions)))
          (loop for j from 1 to ndel do
                (delete-file next-workspace-version)
                (setq next-workspace-version 
                      (increment-workspace-version next-workspace-version)))
          (when verbose?
            (cformatt "~D version~P deleted." ndel ndel))
          (loop for j from 1 to save-n-versions do
                (rename-file
                 next-workspace-version
                 (make-pathname
                  :name 
                  (pathname-name (versioned-workspace-file package-name j))
                  :defaults next-workspace-version
                  )))
          (when verbose?
            (cformatt "~D version~P saved and renumbered." 
                      save-n-versions save-n-versions))
          )))

      ))))
              

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun restore-workspace 
       (&key 
        (package *package*)
        (version nil)
        (user *username* us?)
        (directory-path (visitor-directory *username*) ds?)
        (compile? t)
        (compile-verbose t)
        (compile-print t)
        (enter-workspace? t)
        (verbose? t)
        &aux package-name workspace-file workspace-name success?
        )
        
  #.(one-string-nl
     "Load a workspace file designated by PACKAGE and VERSION and located"
     "in USER's directory (by default the current user) or in DIRECTORY-PATH"
     "(by default the current user's home directory)."
     "If VERSION is NIL (the default) the standard workspace file will be"
     "used, otherwise VERSION should be a small positive integer, indicating"
     "the backup version to be loaded. (Use (SHOW-WORKSPACE-VERSIONS ...)"
     "to see the backup veersions available.)"
     "If COMPILE? is T (the default) an attempt will first be made to"
     "compile the file, and if successful, load the binary."
     "The COMPILE-VERBOSE and COMPILE-PRINT options are given to COMPILE-FILE."
     "If compilation is attempted and fails, an attempt is made to load the"
     "source file.  If the workspace file (binary or source) gets loaded"
     "successfully, and if ENTER-WORKSPACE? is T, then the current package, "
     "*package*, gets set to the loaded workspace's package.")

  (cond
   ((and us? ds?)
    (error "Do not specify both USER and DIRECTORY-PATH."))
   ((not ds?)
    (setq directory-path (visitor-directory user)))
   ((and ds? (relative-directory? directory-path))
    (setq directory-path 
          (merge-pathnames directory-path (visitor-directory user))))
   (t nil))
  
  (setq

   success?

   (block exit

     (setq package-name (canonicalize-package-name package))

     (setq workspace-file
           (merge-pathnames
            directory-path
            (if version 
                (versioned-workspace-file package-name version)
              (default-workspace-file package-name)
              )))
     (setq workspace-name (namestring workspace-file))
     (unless (probe-file workspace-file)
       (error "No workspace file ~A exists." workspace-name))
     (when verbose?
       (cformatt "Restoring workspace for package ~A from file ~A"
                 package-name workspace-name))

     (when compile?
       (when verbose? (cformatt "Attempting to compile workspace file"))
       (multiple-value-bind (fasl-file warnings? failures?)
           (compile-file 
            workspace-file :verbose compile-verbose :print compile-print)
         (declare (ignore warnings?))
         (let ((options-string
                (one-string-nl
                 "Apparently the SAVE-WORKSPACE command that created the"
                 "workspace file did not create a consistent set of forms."
                 "It is possible that by executing"
                 "(RESTORE-WORKSPACE ... :COMPILE? NIL)"
                 "the source file will be able to load successfully."
                 "Your other option is to edit the source file by hand"
                 "(or give up trying to restore from this workspace file!)."
                 )))
           (when failures?
             (cond
              ((null fasl-file)
               (cformatt
                (one-string-nl
                 "Errors occurred while compiling the workspace file."
                 "The compiler did not produce a binary file."
                 options-string
                 ))
               (return-from exit nil))
              (t
               (cformatt
                (one-string-nl
                 "Problems were noted compiling the workspace file, but the"
                 "compiler was able to produce a binary file. An attempt will"
                 "now be made to load this binary file..."
                 )))))
           (when verbose? 
             (cformatt "Loading binary workspace file ~A" fasl-file))
           (handler-case 
               (progn 
                 (load fasl-file) 
                 (when verbose? 
                   (cformatt "Workspace for package ~A restored from file ~A"
                             package-name (namestring fasl-file)))
                 (return-from exit t))
             (error 
              (c)
              (cformatt
               (one-string-nl
                "An error occurred loading the binary workspace file."
                options-string
                ))
              (cformatt "Actual error condition: ~S" c)
              (return-from exit nil)
              )))))
                
     (handler-case
         (progn 
           (load workspace-file)
           (when verbose? 
             (cformatt "Workspace for package ~A restored from file ~A"
                       package-name workspace-name))
           (return-from exit t))
       (error
        (c)
        (cformatt
         (one-string-nl
          "An error occurred loading the workspace file."
          "The actual error was ~A."
          "Apparently the SAVE-WORKSPACE command that created the"
          "workspace file did not create a consistent set of forms."
          "You can try editing the workspace source file by hand and then"
          "try the RESTORE-WORKSPACE command again, or give up trying to"
          "restore from this workspace file.")
         c)
        (return-from exit nil)
        ))

     ))

  (when success?
    (when enter-workspace?
      (setq *package* (find-package package-name))
      (when verbose? (cformatt "Entering workspace package ~A" package-name))
      ))
  *package*
  )
            

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun show-workspace-versions 
       (&key
        (package *package*)
        (user *username* us?)
        (directory-path (visitor-directory *username*) ds?)
        &aux 
        package-name workspace-file workspace-name)

  #.(one-string-nl
     "List all the files storing versions of a workspace designated by"
     "PACKAGE, for a particular USER (defaulting to the current user) or"
     "in a particular DIRECTORY-PATH (defaulting to the current user's home"
     "directory).  Also show the date/time each file was created.")

  (cond
   ((and us? ds?)
    (error "Do not specify both USER and DIRECTORY-PATH."))
   ((not ds?)
    (setq directory-path (visitor-directory user)))
   (t nil))

  (block exit
    (setq package-name (canonicalize-package-name package))
    (setq workspace-file 
          (merge-pathnames 
           directory-path
           (default-workspace-file package-name)))
    (setq workspace-name (namestring workspace-file))
    (unless (probe-file workspace-file)
      (cformatt "No workspace file ~A for package ~A exists." 
                workspace-name package-name)
      (return-from exit nil))
    (let ((list nil)
          (next-version (increment-workspace-version workspace-file)))
      (loop
       (when (null (probe-file next-version))
         (push (list workspace-file (file-write-date workspace-file)) list)
         (return))
       (push (list next-version (file-write-date next-version)) list)
       (setq next-version (increment-workspace-version next-version)))
      (let* ((max-size
              (apply 
               'max
               (mapcar (lambda (x) (length (namestring (first x)))) list)
               ))
             (format-string (formatn "~~~DA   ~~A~~%" max-size)))
        (terpri) (terpri)
        (loop for (file timestamp) in list 
              as filename = (namestring file)
              as date = (make-timestamp-string 
                         :universal-time timestamp :mode :mmddyyhhmm)
              do
              (formatt format-string filename date))
        (terpri)
        ))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun show-available-workspaces

       (&key (user *username* us?)
             (directory-path (visitor-directory *username*) ds?))
             
  #.(one-string-nl
     "Show all the workspace files in USER's toplevel directory or in"
     "DIRECTORY-PATH, along with the date each workspace file was created."
     "Only shows the non-versioned workspace files. Also shows the date/time"
     "each workspace file was created.  Use"
     "(SHOW-WORKSPACE-VERSIONS ...)"
     "to see the versions of a given workspace.")

  (cond
   ((and us? ds?)
    (error "Do not specify both USER and DIRECTORY"))
   ((not ds?) (setq directory-path (visitor-directory user)))
   (t nil))

  (let* ((files (directory (directory-wildcard directory-path)))
         (rsuffix (reverse *vsuffix*))
         (workspace-files
          (mapcarnn
           (lambda (f)
             (and (string-equal (pathname-type f) "LISP")
                  (vwhen (pos (search rsuffix (reverse (pathname-name f))))
                    (and (zerop pos) f)
                    )))
           files
           )))
         
    (when workspace-files
      (let* ((longest
              (reduce 
               'max
               workspace-files
               :key (lambda (x) (length (file-namestring x)))
               ))
             (format-string (formatn "  ~~~DA   ~~A~~%" longest)))
        (format t "~%~%AVAILABLE WORKSPACES in ~A~%~%" 
                (namestring directory-path))
        (loop for file in workspace-files do
              (format t format-string
                      (file-namestring file)
                      (make-timestamp-string 
                       :universal-time (file-write-date file)
                       :mode :mmddyyhhmm
                       )))
        (terpri)
        ))

    (length workspace-files)
    
    ))


