;;; -*- Package: utils; mode: lisp; base: 10; Syntax: Common-Lisp; -*-

(in-package :utils)

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

  (defparameter *utility-symbols-user-symbols* 
    '(
      keywordize 
      symbol= 
      sym-member
      package-external-symbols
      package-and-symbol-string 
      external-symbol-of-package?
      aprps
      ))

  (defparameter *utility-symbols-api-symbols*
    (append 
     *utility-symbols-user-symbols*
     '(
      ;; miscellaneous routines dealing with symbols
       forward-funcall
       forward-package-funcall
       symbol-of-package
       symbol-value-in-package
       symbol-package-value
       ;; the state-variable mechanism
       def-state-var 
       instantiate-state-variables 
       ;; the saved-variable mechanism
       initialize-saved-variables
       saved-variable-value
       with-saved-variables-values
       symbols-in-conflict
       import-then-export
       with-lock
       )))

  (export *utility-symbols-api-symbols* (find-package :utils)))


(defun keywordize (symbol &optional (case :upper))
  #.(one-string-nl
     "Return a symbol in the KEYWORD package with the same name as SYMBOL "
     "(modified appropriately for CASE).  SYMBOL may also be a string.")
  (let ((p (find-package :keyword)))
    (flet ((doit (string)
             (intern 
              (ecase case
                (:upper (string-upcase string))
                (:lower (string-downcase string))
                ((:preserve nil) string))
              p)))
      (cond 
       ((stringp symbol) (doit symbol))
       ((symbolp symbol) (doit (symbol-name symbol)))
       (t (error "Invalid argument to KEYWORDIZE: ~A" symbol))
       ))))


(defun symbol= (s1 s2)
  #.(one-string-nl
     "True if S1 and S2 are both symbols and their SYMBOL-NAME's are STRING="
     "Examples:  (SYMBOL= 'fred :fred) -> t, (SYMBOL= 'fred '|fred|) -> nil")
  (and (symbolp s1) (symbolp s2)
       (string= (symbol-name s1) (symbol-name s2))))

(defun sym-member (symbol list) 
  #.(one-string-nl
     "Returns non-nil if SYMBOL or any symbol with the same name"
     "(but different package) is found in LIST.")
  (member symbol list :test 'symbol=))

(defun external-symbol-of-package? (symbol package)
  (let ((p (find-package package)))
    (multiple-value-bind (s type)
        (find-symbol (string symbol) p)
      (declare (ignore s))
      (eq type :external)
      )))


;; Don't use LOOP to stop annoying Allegro compiler warning about gensym.
(defun package-external-symbols (package)
  "Returns a list of all the external symbols of a package"
  (let ((symbols nil))
    (do-external-symbols (symbol (find-package package) (nreverse symbols))
      (push symbol symbols))))


(defun forward-funcall (f &rest args) 
  #.(one-string-nl
     "Allows function calls to functions that have not yet been defined"
     "without the compiler emitting a warning.")
  (apply f args))


(defun forward-package-funcall (package name &rest args)
  #.(one-string-nl
     "Allows function calls to functions in packages that have not yet been"
     "defined without a reader error or having the compiler emit a warning.")
  (let ((f (find-symbol (string name) package))) (apply f args)))


(defun symbol-of-package (name package &key (if-does-not-exist :intern))
  #.(one-string-nl
     "Locates a symbol whose name is NAME is in particular package PACKAGE."
     "Like FIND-SYMBOL except that:"
     "  -- NAME does not have to be a string, it can be a symbol"
     "  -- If IF-DOES-NOT-EXIST is :intern (the default), the symbol will"
     "     be created."
     "IF-DOES-NOT-EXIST must either be :intern or :error.")
  (multiple-value-bind (symbol status)
      (find-symbol (string name) package)
    (if status
        (values symbol status)
      (ecase if-does-not-exist
        ((:intern :create) (intern (string name) package))
        (:error 
         (error 
          "Symbol named '~A' does not exist in package ~A" 
          (string name) package
          ))))))

(defun symbol-value-in-package 
       (name 
        package 
        &key
        (if-does-not-exist :error)
        (if-unbound :error)
        (if-unbound-value nil))
  #.(one-string-nl
     "Returns the value of the symbol whose name is NAME in package PACKAGE."
     "If the symbol does not exist in PACKAGE, IF-DOES-NOT-EXIST determines"
     "whether an error is signaled (value :error) or the symbol is created"
     "(value :create).  These two are IF-DOES-NOT-EXIST's only valid values."
     "If the symbol is UNBOUND in PACKAGE, IF-UNBOUND determines whether an"
     "error is signaled (value :error, the default), or whether the symbol has"
     "its value set to IF-UNBOUND-VALUE (value :set) and returned, or whether"
     "IF-UNBOUND-VALUE is simply returned without the symbol's value being"
     "set (value :return).  Any other value for IF-UNBOUND is invalid.")
  (multiple-value-bind (symbol status)
      (symbol-of-package name package :if-does-not-exist if-does-not-exist)
    (declare (ignore status))
    (if (boundp symbol)
        (symbol-value symbol)
      (ecase if-unbound
        (:error
         (error 
          "Symbol named '~A' in package ~A is unbound" (string name) package))
        (:set
         (setf (symbol-value symbol) if-unbound-value))
        (:return if-unbound-value)
        ))))


(defun symbol-package-value (name package &rest keyargs)
  "Another name for (SYMBOL-VALUE-IN-PACKAGE ...)"
  (apply 'symbol-value-in-package name package keyargs))


;;;; IMPLEMENTATION OF STATE VARIABLES MECHANISM


(defvar *new-state-variables-hash-table* (make-hash-table :test 'equal))


(defmacro def-state-var
          (package-name
           deftype var
           &optional
           initvalue
           (subkeyfn
            (lambda ()
              (symbol-value-in-package 
               :*sessionid* :wb :if-does-not-exist :error))))
  #.(one-string-nl
     "Define a pseudo-variable VAR which, through the magic of symbol macros,"
     "takes on different values in different contexts."
     "PACKAGE-NAME defines a grouping such that all pseudo variables which"
     "belong to the same group are initialized at the same time using"
     "(instantiate-state-variables package-name)"
     "DEFTYPE defines whether the pseudo variable is reinitialized if"
     "INSTANTIATE-STATE-VARIABLES is called a subsequent time.  If"
     "DEFTYPE is 'defvar, it is not reinitialized and if DEFTYPE is"
     "'defparameter it is reinitialized."
     "INITVALUE is the value the pseudo-variable is initialized to."
     "SUBKEYFN is a function which returns the context in which the"
     "pseudo-variable's value is initialized, looked up and/or changed." 
     "Example: "
     "(in-package :foo)"
     "(defvar *current-key*)"
     "(def-state-var :foo defvar *xyz* 3 (lambda () *current-key*))"
     "(let ((*current-key* :massar))"
     "  (instantiate-state-variables :foo))"
     "(let ((*current-key* :jshrager))"
     "  (instantiate-state-variables :foo) (setq *xyz* 4))"
     ";; Now XYZ has value 3 wrt :massar and 4 wrt :jshrager"
     "(list (let ((*current-key* :massar)) *xyz*)"
     "      (let ((*current-key* :jshrager)) *xyz*))"
     "(3 4)"
     "(Note that SUBKEYFN defaults to a weblistener-specific symbol so that"
     "if you are using this facility without the weblistener you MUST"
     "provide the SUBKEYFN argument.)")
  (LET* ((pkg (keywordize package-name))
	 (varstring (string var))
	 (app-key (cons pkg varstring))
	 (symbol-macro-symbol (intern (string var) pkg))
	 )
    `(progn
       ;; Store the VARSPEC information.
       (setf (gethash ',app-key *new-state-variables-hash-table*)
             (list 
               :deftype ',deftype 
               :varstring ,varstring
               :initvalue ,initvalue
               :subkeyfn ,(if (symbolp subkeyfn) `',subkeyfn subkeyfn)
               ))

       ;; ',(list :deftype deftype   ; either defparameter or defvar
       ;;     :varstring varstring
       ;; :initvalue initvalue  ; hopefully acts like the deftype
       ;; :subkeyfn subkeyfn 
       ;; ))
       ;; define symbol macro
       (define-symbol-macro 
	 ,symbol-macro-symbol (get-state-variable-value ',app-key))
       ',var
       )))
  
(defun instantiate-state-variables (package-name)
  #.(one-string-nl
     "(Re)Initialize all pseudo-variables defined by DEF-STATE-VAR"
     "that are keyed off of PACKAGE-NAME.  See DEF-STATE-VAR for details.")
  (let ((pkg (keywordize (package-name (find-package package-name)))))
    (loop for (k spec) in (hash-table-contents *new-state-variables-hash-table*)
	  do (when (and (find-package (car k))
			(eq (keywordize 
                             (package-name (find-package (car k)))) pkg))
	       (let* ((key (state-variable-subkey k)))
		 (multiple-value-bind
		  (value exists?)
		  (gethash key *new-state-variables-hash-table*)
		  (declare (ignore value))
		  (when (or (not exists?)
			    (eq 'defparameter (getf spec :deftype)))
		    (setf (gethash key *new-state-variables-hash-table*) 
                          (getf spec :initvalue)))))))))

(defun state-variable-subkey (app-key)
  (let* ((spec (gethash app-key *new-state-variables-hash-table*))
	 (subkey (funcall (getf spec :subkeyfn))))
    (cons subkey (getf spec :varstring))))

(defun get-state-variable-value (key)
  (gethash (state-variable-subkey key) *new-state-variables-hash-table*))

(defun set-state-variable-value (key value)
  (setf (gethash (state-variable-subkey key) *new-state-variables-hash-table*)
        value))

;;; If *foo* is a state variable, this causes
;;; (setf *foo* 3)
;;; to turn into a call to SET-STATE-VARIABLE-VALUE
;;; (because *FOO* will first get symbol-macroexpanded into 
;;; a call to GET-STATE-VARIABLE-VALUE

(defsetf get-state-variable-value set-state-variable-value)



;;;; IMPLEMENTATION OF SAVED VARIABLES MECHANISM

;;; Here is where the saved variables and their values get stored away
;;; (In the old implementation, this data was stored on the property list
;;; of the package-name symbol)

#+allegro
(defvar *saved-variables-lock*
  (mp:make-process-lock :name "Saved Variables Lock"))

(defmacro with-lock ((lock) &body body)
  (declare (ignorable lock))
  #+:allegro
  `(mp:with-process-lock (,lock) ,@body)
  #-:allegro
  `(progn ,@body))

#-:allegro
(warn "Need mp:with-process-lock equivalent for get/set-saved-variable")

(defvar *saved-variables-hash-table* 
  (create-hash-table 
   (list (cons :unique-value (list nil)))
   :test 'eq
   :mode :dotted-pair))


(defun get-saved-variable (key)
  (with-lock (*saved-variables-lock*)
    (gethash key *saved-variables-hash-table*)))

(defun set-saved-variable (key value)
  (with-lock (*saved-variables-lock*)
    (setf (gethash key *saved-variables-hash-table*) value)))

(defun initialize-saved-variables (key variables-list)
  #.(one-string-nl
     "Defines of set of variables, VARIABLE-LIST, whose values will be"
     "saved and restored using (WITH-SAVED-VARIABLES-VALUES ...)  KEY is any"
     "unique index into an EQ hash table (presumably a symbol), and is used"
     "to identify this particular set of variables.")
  (let* ((unique-value (get-saved-variable :unique-value))
         (values-list
          (mapcar 
           (lambda (v) (if (boundp v) (symbol-value v) unique-value))
           variables-list
           )))
    (set-saved-variable key (list variables-list values-list))))

(defun saved-variable-value 
       (key variable-name 
            &key 
            (if-no-key :error)
            (if-no-variable :error)
            (if-unbound :<unbound>))
  #.(one-string-nl
     "Retrieve the stored-away value of VARIABLE-NAME associated with KEY."
     "This is equivalent to (WITH-SAVED-VARIABLES-VALUES (key) variable-name)"
     "but is less typing, is more efficient, and provides options for what"
     "happens if the KEY does not exist, if the VARIABLE-NAME is not found,"
     "or if the variable has no value.  If such a condition exists, and the"
     "appropriate keyword value is :ERROR, an error is signalled, otherwise"
     "that keyword value is returned.  E.g.,:"
     "(SAVED-VARIABLE-VALUE :NO-KEY 'fred :if-no-key 23) --> 23")
  (block exit
    (let* ((ht *saved-variables-hash-table*)
           (saved-list (get-saved-variable key)))
      (when (null saved-list)
        (when (not (eq if-no-key :error)) (return-from exit if-no-key))
        (error "No saved variables found for key ~S" key))
      (destructuring-bind (vars values) saved-list
        (let ((pos (position 
                    (string variable-name) vars 
                    :key 'symbol-name :test 'string=)))
          (when (null pos)
            (when (not (eq if-no-variable :error))
              (return-from exit if-no-variable))
            (error "No saved variable named ~A for key ~S" variable-name key))
          (let ((value (nth pos values))
                (unique-value (gethash :unique-value ht)))
            (if (eq value unique-value)
                (if (not (eq if-unbound :error))
                    if-unbound
                  (error "Saved variable ~A is unbound" variable-name))
              value
              )))))))
          
         
(defmacro with-saved-variables-values ((key) &body body)
  #.(one-string-nl
     "Executes BODY with all the variables associated with KEY via an"
     "INITIALIZE-SAVED-VARIABLES call bound to their stored values."
     "When BODY is finished executing, all the (possibly changed) variable"
     "values are saved (so that a subsequent invocation will see the changed"
     "values.  Variables associated with KEY can be initially UNBOUND or"
     "made unbound (using MAKUNBOUND), and they will be saved and restored"
     "properly.")
  (let ((hash-value-symbol (gensym "HASH-VALUE-"))
        (key-symbol (gensym "KEY-"))
        (unique-value-symbol (gensym "UNIQUE-VALUE-"))
        (vars-symbol (gensym "VARS-"))
        (vals-symbol (gensym "VALS-"))
        )
    `(let* ((,key-symbol ,key)
            (,hash-value-symbol (get-saved-variable ,key-symbol))
            (,vars-symbol (first ,hash-value-symbol))
            (,vals-symbol (second ,hash-value-symbol))
            (,unique-value-symbol (get-saved-variable :unique-value))
            )
       (unless ,hash-value-symbol
         (error "No saved variables found for key ~S" ,key-symbol))
       (progv ,vars-symbol ,vals-symbol
         (make-unbound-variables-really-unbound 
          ,vars-symbol ,vals-symbol ,unique-value-symbol)
           (unwind-protect 
               (progn ,@body)
             (save-saved-variables-values 
              ,vars-symbol ,vals-symbol ,unique-value-symbol)
             )))))

(defun show-saved-variables (sessionid) 
  (let ((data (gethash sessionid *saved-variables-hash-table*)))
    (loop for var in (first data)
          for value in (second data)
          do 
          (when (not (member var '(* ** *** + ++ +++ / // ///)))
            (formatt "~S : ~S~%" var value))
          )))

(defun instantiate-saved-variables-values (key)
  (let* ((val (get-saved-variable key))
         (vars (first val))
         (values (second val))
         (unique (get-saved-variable :unique-value)))
    (loop for var in vars for value in values do 
          (setf (symbol-value var) value))
    (make-unbound-variables-really-unbound vars values unique)
    ))
    
       
(defun make-unbound-variables-really-unbound (vars values unbound-designator)
  (loop for var in vars for val in values do
        (when (eq val unbound-designator) (makunbound var))))

(defun save-saved-variables-values (vars values unbound-designator)
  (loop for var in vars
        for values-list on values do
        (setf (first values-list)
              (if (not (boundp var)) unbound-designator (symbol-value var))
              )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun package-and-symbol-string (symbol)
  #.(one-string-nl 
     "Returns a string with the symbol's package prefix and the symbol's name.")
  (let ((p (symbol-package symbol)) (n (symbol-name symbol)))
    (cond
     ((null p) (error "Should not call this function on uninterned symbol"))
     ((eq p (find-package :keyword)) (formatn ":~A" n))
     (t (formatn "~A::~A" (package-name p) n))
     )))

(defun symbols-in-conflict 
       (using-package used-package &optional (shadowing-import-symbols nil))
  (let ((conflicting-symbols nil))
    (do-external-symbols (s used-package)
      ;; See if another symbol with the same name as S exists already
      ;; in the USING-PACKAGE and will not be overridden by one of
      ;; the to-be shadowing-import'ed symbols?
      (let* ((psym (find-symbol (symbol-name s) using-package)))
        (when (and psym 
                   (not (eq psym s))
                   (not (eq used-package (symbol-package psym)))
                   (not (member (string psym)
                                shadowing-import-symbols
                                    :key 'symbol-name :test 'string=
                                    )))
          (push psym conflicting-symbols)
          )))
    conflicting-symbols))

(defun import-then-export 
       (package &key (symbols nil) (packages nil) (protected-symbols nil))
  #.(one-string-nl
     "Import then export all the symbols in SYMBOLS, and all the external"
     "symbols in all the packages in PACKAGES. But do NOT import/export any"
     "symbol with the same symbol name as as symbol found in"
     "PROTECTED-SYMBOLS.")
  (let ((shadows (mapcar 'string protected-symbols))
        (package (find-package package)))
    (flet ((maybe-ie (s)
             (unless (find (symbol-name s) shadows :test 'string=)
               ;; If S = NIL, this would be a noop without
               ;; wrapping the LIST around S.
               (import (list s) package)
               (export (list s) package)
               )))
      (loop for s in symbols do (maybe-ie s))
      (loop for p in packages do
            (do-external-symbols (var (find-package p))
              (maybe-ie var)
              )))))

(defun aprps (name &optional package)
  "Do an APROPOS using uppercase regardless of what you type." 
  (apropos (string-upcase (string name)) package))