;;; -*- Package: weblistener; mode: lisp; base: 10; Syntax: Common-Lisp; -*-

(in-package :weblistener) 

;;; +=========================================================================+
;;; | Copyright (c) 2002-2005 JP Massar, Jeff Shrager                         |
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

(defun load-module 
       (name 
        &key
        (use-module? :if-file)
        (search-directories nil)
        (verbose? t)
        )
  #.(one-string-nl
     "Loads a module, possibly created by CREATE-LOADABLE-MODULE, or"
     "represented by a directory called NAME.  The module"
     "is found by calling (FIND-MODULE NAME ...).  If the module is"
     "found in a directory, then LOAD is called on the load.lisp file"
     "in that directory.  Otherwise the file representing the module"
     "is compiled/loaded directly."
     "USE-MODULE? determines whether (USE-MODULE ...) is called after"
     "the module is loaded.  By default, USE-MODULE is called only"
     "if a module file, as opposed to a module directory, is found."
     "If USE-MODULE? is T, then USE-MODULE will be always be called,"
     "and if USE-MODULE? is NIL, it will not be called."
     "The rationale for calling USE-MODULE on module files by default"
     "is that they should be created by CREATE-LOADABLE-MODULE, which"
     "defines a package to be used, whereas a directory with a load.lisp file"
     "may or may not define a new package."
     )
  (multiple-value-bind (module-path type search-paths)
      (if search-directories 
          (find-module name :search-path search-directories)
        (find-module name))
    (when (and type verbose?)
      (cformatt "Loading module ~S from ~A" name module-path))
    (ecase type
      (:directory (load module-path))
      (:module-file (c/l module-path))
      ((nil) 
       (cformatt "No module named ~S could be found in: " name)
       (dolist (path search-paths) (cformatt "  ~A" (namestring path)))
       (error "Cannot load module named ~S" name)
       ))
    (when verbose? (cformatt "Module ~S loaded." name))
    (flet ((use () (use-module (keywordize name))))
      (ecase use-module? 
        ((t) (use))
        ((nil) nil)
        (:if-file (when (eq type :module-file) (use)))
        (:if-directory (when (eq type :directory) (use)))
        ))
    name
    ))
         
(defun use-module 
       (package-name-to-use 
        &key 
        (package-name-doing-the-using wb:*username*)
        (shadowing-import-function-name
         (keywordize
          (formatn "~A-SYMBOLS-TO-SHADOWING-IMPORT" package-name-to-use)
          )))

  #.(one-string-nl
     "Cause a package (by default, the user's home package) to use"
     "another package, PACKAGE-NAME-TO-USE."
     "USE-MODULE smashes any undefined symbols in "
     "PACKAGE-NAME-DOING-THE-USING"
     "in favor of symbols in PACKAGE-NAME-TO-USE.  If any conflicting"
     "symbols are actually bound or fbound the function prints a warning"
     "and returns without actually using the package."
     "SHADOWING-IMPORT-FUNCTION-NAME should name a function which, when"
     "called, returns a list of symbols to SHADOWING-IMPORT into"
     "PACKAGE-NAME-DOING-THE-USING before USE-PACKAGE is called (thus"
     "potentially avoiding name conflicts).  If the function so named does"
     "not in fact exist no symbols are shadowing-imported.")

  (block exit

    (let* ((used-package (find-package package-name-to-use))
           (using-package (find-package package-name-doing-the-using)))

      (unless used-package
        (error 
         (one-string-nl
          "The ~S system does not exist!!!"
          "Did you forget to load it? If that is not the problem, "
          "Please notify the person responsible for this system.")
         package-name-to-use))
      (unless using-package
        (error
         (one-string-nl
          "The ~S package (the using package you specified) does not exist!"
          "Cannot perform this operation without an existing using package.")
         package-name-doing-the-using
         ))
            
      (let* ((used-package-name (package-name used-package))
             (using-package-name (package-name using-package))
             (shadowing-import-function
              (intern (string shadowing-import-function-name) used-package))
             (shadowing-import-symbols
              (when (fboundp shadowing-import-function)
                (funcall shadowing-import-function)
                ))
             (functions-oops nil)
             (values-oops nil)
             )

        (when (member used-package (package-use-list using-package))
          (cformatt "The ~S system is already in use by package ~S" 
                    used-package-name using-package-name)
          (return-from exit t))

        ;; This should be recoded using SYMBOLS-IN-CONFLICT 
        ;; from symbol-utils.lisp.  First collect the potentially
        ;; problematic symbols and then loop again to do the uninterning.
        
        (do-external-symbols (s used-package)
          ;; See if another symbol with the same name as S exists already
          ;; in the USING-PACKAGE and will not be overridden by one of
          ;; the to-be shadowing-import'ed symbols?
          (let* ((psym (find-symbol (symbol-name s) using-package)))
            (when (and psym 
                       (not (eq used-package (symbol-package psym)))
                       (not (member (string psym)
                                    shadowing-import-symbols
                                    :key 'symbol-name :test 'string=
                                    )))
              ;; Yes, we may have a problem.
              (cond
               ;; Does it have a value or a definition ?
               ((and (not (boundp psym)) (not (fboundp psym)))
                ;; No. Warn the user, but proceed by uninterning the symbol.
                (warn 
                 (formatn
                  (one-string-nl
                   "Symbol ~A, present in package ~A"
                   "conflicts with existing symbol ~A, present in package ~A."
                   "The symbol in package ~A is being deleted (uninterned) and"
                   "the symbol in the ~A package will replace it.")
                  (package-and-symbol-string s)
                  used-package-name
                  (package-and-symbol-string psym)
                  using-package-name
                  using-package-name
                  used-package-name
                  ))
                (unintern psym))
               (t
                ;; Yes, it has a value or a definition. 
                ;; Issue a warning and do not try to use the package.
                (when (fboundp psym) (push psym functions-oops))
                (when (boundp psym) (push psym values-oops))
                )))))

        (when functions-oops
          (formatt
           (one-string-nl
            " "
            "The following symbols present in package ~A have function"
            "definitions but these symbols conflict with identically-named"
            "symbols in the ~A package:") 
           using-package-name
           used-package-name)
          (dolist (f functions-oops) 
            (formatt "  ~A~%" (package-and-symbol-string f)))
          (terpri)
          (formatt
           (one-string-nl
            "You cannot use the ~A package under these condition."
            "You need to 'undefine' these function definitions by executing"
            " "
            "(fmakunbound '<<conflicting-symbol>>)"
            " "
            "for each conflicting symbol listed above."
            "(The system will not do this for you because that would cause you"
            "to lose these function definitions without a chance to do"
            "something about it.)"
            "(Please consult a system administrator if this does not make"
            "sense, rather than doing something (like using FMAKUNBOUND)"
            "you don't understand the consequences of.)"
            " "
            )
           used-package-name
           ))

        (when values-oops
          (formatt
           (one-string-nl
            "The following symbols present in package ~A have values"
            "but these symbols conflict with identically-named symbols in the"
            "~A package:")
           using-package-name
           used-package-name
           )
          (dolist (v values-oops) 
            (formatt "  ~A~%" (package-and-symbol-string v)))
          (terpri)
          (cformatt
           (one-string-nl
            "You cannot use the ~A package under these conditions."
            "You need to 'undefine' these variable values by executing"
            " "
            "(makunbound '<<conflicting-symbol>>)"
            " "
            "for each conflicting symbol listed below."
            "(The system will not do this for you because that would cause you"
            "to lose the values of these these symbols without a chance to do"
            "something about it.)"
            "(Please consult a system administrator if this does not make"
            "sense, rather than doing something (like using MAKUNBOUND)"
            "you don't understand the consequences of.)"
            " "
            )
           used-package-name)
          )

        (if (or functions-oops values-oops)
            (progn 
              (cformatt "The ~A package is NOT being used." used-package-name) 
              nil)
          (progn
            (when shadowing-import-symbols
              (formatt
               (one-string-nl
                "The following symbols are being 'shadowing-imported'"
                "into package ~A"
                "(because a function ~A"
                "exists which was called and returned this list of symbols)."
                " ")
               using-package-name
               (package-and-symbol-string shadowing-import-function)
               )
              (dolist (s shadowing-import-symbols)
                (formatt "  ~A~%" (package-and-symbol-string s)))
              (terpri)
              (shadowing-import shadowing-import-symbols using-package)
              )
            (use-package (list used-package) using-package)
            (cformatt 
             "The ~A package is now being used by package ~A" 
             (package-name used-package)
             (package-name using-package)
             )
            t
            ))))))

(defun default-single-file-system-name (package-name)
  (formatn "~A.lisp" (string-downcase package-name)))

(defun default-single-file-system-path (system-file)
  (merge-pathnames 
   system-file
   (first (application-shared-files-directories cl-user::*ai*))
   ))

(defun create-loadable-module
       (package-name
        function-list
        &key
        (template-package (find-package (or wb::*username* *package*)))
        (variable-list nil)
        (file (default-single-file-system-name (string package-name)))
        (use-function-name 
         (keywordize (one-string "USE-" (string package-name)))
         )
        (verbose? t)
        (nicknames nil)
        )
  #.(one-string-nl
     "Creates a file containing a package definition and a set of function"
     "and variable definitions.  The intent is to create an independent"
     "module that others can use."
     "The functions and variables are defined in package PACKAGE-NAME"
     "and the file created is by default named <package-name>.lisp"
     "The file is by default created in the system's shared directory,"
     "where it can be accessed by other users." 
     "By default, the package PACKAGE-NAME is created as a mirror of"
     "the user's package.  (This can be overridden by using the"
     "TEMPLATE-PACKAGE keyword.)"  
     "Besides defining the specified functions in FUNCTION-LIST and"
     "optionally the variables in VARIABLE-LIST a function by the name"
     "of USE-<PACKAGE-NAME> is created.  When called, this function"
     "causes the current package to use the loaded module."
     "A module can be loaded by using the function (LOAD-MODULE ...)"
     "Nicknames for the module package can be specified using the"
     "NICKNAMES keyword argument.")
  (let ((system-path (default-single-file-system-path file)))
    (when verbose?
      (cformatt 
       "Creating system ~A in file ~A~%"
       package-name (namestring system-path)
       ))
    (with-open-file
        (s system-path :direction :output :if-exists :supersede)
      (with-standard-io-syntax
        (let ((*readtable* (frames-readtable)))
          (pprint `(in-package :cl-user) s) (terpri s)
          (pprint 
           (create-defpackage-form 
            template-package :target-package package-name :nicknames nicknames)
           s)
          (terpri s)
          (pprint `(in-package ,(keywordize package-name)) s)
          (terpri s)
          (terpri s)
          (format s "(EVAL-WHEN (:COMPILE-TOPLEVEL :LOAD-TOPLEVEL :EXECUTE)~%")
          (format s "  (EXPORT '(")
          (loop for symbol in (append function-list variable-list)
                do (format s "~a " symbol)) 
          (format s "~%         )))~%")
          (terpri s)
          (let ((*package* template-package))
            (loop for var in variable-list
                  do (pprint 
                      (let ((defn (get var :variable-definition)))
                        (cond (defn defn)
                              ((boundp var)
                               `(defvar ,var ,(symbol-value var)))
                              (t `(defvar ,var))))
                      s)
                  (terpri s)
                  )
            (terpri s)
            (loop for fn in function-list
                  do (pprint 
                      (get fn :procedure-definition)
                      s)
                  (terpri s)
                  )
            (terpri s)
            (pprint `(let ((package (or wb::*username*
                                        *package*)))
                       (eval `(defun ,(intern ,use-function-name package) ,()
                                (wb::use-system ,(keywordize ,package-name)))))
                    s)
            (when verbose?
              (cformatt 
               "Created system ~A in file ~A~%"
               package-name (namestring system-path)
               ))
            ))))))


(defun find-module 
       (module-name 
        &key 
        (instance-home? t)
        (home? t)
        (system? t)
        (app? t)
        (applib? t)
        (shared? t)
        (search-paths
         (default-module-directories 
          instance-home? home? system? app? applib? shared?))
        (additional-paths nil)
        )
  #.(one-string-nl
     "Returns the pathname of a file which, when loaded, should cause"
     "module MODULE-NAME to be created and loaded."
     "The file loaded can either be a file called <module-name>.lisp"
     "found in one of the search directories, or a file called"
     "load.lisp found in a subdirectory of one of the search"
     "directories called <module-name>."
     "A second value is returned if a module is found.  The second"
     "value is :directory if a directory with a load.lisp file is found,"
     "or :module-file if a file by the name of <module-name>.lisp is found."
     "If SEARCH-PATHS is provided, then if must be a list of directories,"
     "and these directories are searched in order for the module."
     "If SEARCH-PATHS is not provided then INSTANCE-HOME?, HOME?, SYSTEM?,"
     "APP?, APPLIB? and SHARED? specify whether the module search should encompass"
     "the application's home directory, the user's home directory," 
     "the weblistener source directory,"
     "the application source directory, the 'Lib' subdirectory of the"
     "application source directory, and/or the shared files directory,"
     "respectively.  (These are always searched in the given order)"
     "Finally, if ADDITIONAL-PATHS is non-nil, these directories"
     "will be added to the end of SEARCH-PATHS.")
  (setq module-name (string module-name)) 
  (flet ((file-named? (filepath name type)
           (and (string-equal name (pathname-name filepath))
                (string-equal type (pathname-type filepath)))))
    (block found
      (loop with paths = (append search-paths additional-paths)
            for dir in paths 
            as listing = (directory-with-subdirs-in-directory-form dir) do
            (loop for entry in listing do
                  (if (pathname-names-directory? entry)
                      (when (string-equal 
                             module-name 
                             (lastelem (pathname-directory entry)))
                        (loop for file in 
                              (directory-with-subdirs-in-directory-form entry)
                              do
                              (when (file-named? file "load" "lisp")
                                (return-from found (values file :directory paths))
                                )))
                    (when (file-named? entry module-name "lisp")
                      (return-from found (values entry :module-file paths))
                      )))
            finally (return (values nil nil paths))
            ))))
                
(defun default-module-directories 
       (instance-home? home? system? app? applib? shared?)
  (let ((dirs nil))
    (when shared? 
      (setq 
       dirs 
       (append dirs (application-shared-files-directories user:*ai*))))
    (when app?
      (push user:*application-directory* dirs))
    (when applib? 
      (push (append-subdir user:*application-directory* "Lib") dirs) 
      (push (append-subdir user:*application-directory* "lib") dirs)) 
    (when system?
      (push user:*weblistener-directory* dirs))
    (when (and home? wb:*username*) 
      (push (visitor-directory wb:*username*) dirs))
    (when (and instance-home? user:*instance-home-directory*)
      (push user:*instance-home-directory* dirs))
    dirs))
