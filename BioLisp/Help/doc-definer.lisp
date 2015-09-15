;;; -*- Package: help; mode: lisp; base: 10; Syntax: Common-Lisp; -*-

(in-package :help)

;;; +=========================================================================+
;;; | Copyright (c) 2002-2006 JP Massar, Jeff Shrager, Peter Seibel           |
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

;;; Authors:  JP Massar, Peter Seibel.

;;; A mechanism to generalize the parsing of specifications for
;;; documentation entries.

;;; A documented object definition macro is created which creates a macro
;;; which parses things of the form:

;;; (<macro name> <object name>
;;;   (<slot1> &rest <slot1-data> ...)
;;;   (<slot2> &rest <slot2-data> ...)
;;;   ...

;;; and turns them into CLOS objects which are subclasses of HELP:BASICDOC

;;; The data from each slot is verified and parsed, and the result of
;;; the parse is stored as the slot value.  The creation of the documention
;;; object and further processing can also be specified.

;;; a SPEC a provided for each slot of a documentation class that
;;; that can be provided in the definition macro.  The form of a SPEC is:

;;; (key data-form verification-function parsing-function accessor)

;;; KEY is one of the <slotn> symbols
;;; DATA-FORM is one of: :list, :non-nil-list, :exactly-one, :one-or-none
;;; A verification function must be a symbol whose function binding is
;;; a function of one argument which returns 2 values: 
;;;   -- The first, T or NIL, says whether the verification passed.  
;;;   -- The second is the data if the first value is T and a string 
;;;      saying what is wrong if the first value is NIL.

;;; The parsing function must be a symbol whose function binding is a function
;;; of one argument.  It must return a single value which is stored in the 
;;; slot represented accessed by ACCESSOR and stored into by (SETF ACCESSOR)

;;; DEFINE-DOC-DEFINER defines a creation/verification/parsing/post-processing
;;; for TYPE, a subclass of HELP:DOCUMENTED.  (In theory, it could be
;;; used for any CLOS class). The arguments and options are:

;;;   * MACRO-NAME -- The macro that creates an object of type TYPE
;;;   * FUNCTION-NAME -- The function that the macro calls to do
;;;     the object creation, parsing and post-processing
;;;   * SPECS -- Specifications for each slot as described above
;;;   * OBJECT-VAR -- The name of the variable bound to the created
;;;     object, which can be used in the AFTER-CODE
;;;   * CREATION-CODE -- The code which creates the object of type TYPE.
;;;     OBJECT-VAR is bound to the result of this code.
;;;   * AFTER-CODE -- code which is executed after the object is created
;;;     and its slots initialized according to the SPECs.  Within this
;;;     code, OBJECT-VAR is bound to the object created.

(defmacro define-doc-definer
          (type macro-name function-name specs 
                &key 
                (object-var 'obj)
                (creation-code `(intern-documentation name ',type))
                (after-code nil)
                (other-specs-allowed? nil)
                )
  `(progn
     (setf (get ',type :ddspec) 
           (list ,@(mapcar (lambda (x) `(list ,@(mapcar 'arghack x))) specs)))
     (defmacro ,macro-name (name &body info) 
       (let* ((specs (get ',type :ddspec))
              (verified-arguments
               (verify-doc-definer-info
                specs info ,other-specs-allowed? ',macro-name)))
         `(progn
            (,',function-name ',name ,@verified-arguments)
            ',name
            )))
     (defun ,function-name ,(ddd-function-name-arglist specs)
       (let ((,object-var ,creation-code))
         ,@(loop for (key nil nil parser accessor) in specs 
                 collect 
                 `(when ,(ddd-key-to-provided-arg key) 
                    (setf (,accessor ,object-var) 
                          (funcall ',parser ,(ddd-key-to-arg key))
                          )))
         ,@(when after-code (list after-code))
         ))))


;;; Some basic verification functions.

(defun ddd-identity (x) (values t x))

(defun ddd-string (x) 
  (if (stringp x) (values t x) (values nil "Not a string!")))

(defun ddd-symbol (x) 
  (if (symbolp x) (values t x) (values nil "Not a symbol!")))

(defun ddd-integer (x) 
  (if (integerp x) (values t x) (values nil "Not an integer!")))

(defun ddd-keyword (x)
  (if (keywordp x) (values t x) (values nil "Not a keyword!")))

(defun ddd-boolean (x) (or (eq x t) (eq x nil)))

(defun ddd-string-or-nil (x)
  (if (or (null x) (stringp x)) 
      (values t x)
    (values nil "Not a string or NIL!")))

(defun ddd-all-strings (x) 
  (if (every 'stringp x) 
      (values t x)
    (values nil "Every item must be a string!")))

(defun ddd-all-keywords (x) 
  (if (every 'keywordp x) 
      (values t x)
    (values nil "Every item must be a keyword!")))

(defun ddd-all-symbols (x) 
  (if (every 'symbolp x) 
      (values t x)
    (values nil "Every item must be a string!")))

(defun ddd-all-symbols-or-strings (x) 
  (if (every (lambda (y) (or (stringp y) (symbolp y))) x)
      (values t x)
    (values nil "Every item must be a string or symbol!")))


;;; Utilities to help create the argument list for the function
;;; the macro calls.  The function has a keyword for each SPEC,
;;; and each keyword has an associated PROVIDED argument.

(defun ddd-function-name-arglist (specs) 
  `(name 
    &key 
    ,@(loop for (key) in specs collect 
            (list (ddd-key-to-arg key) nil (ddd-key-to-provided-arg key)))))

(defun ddd-key-to-arg (key) (intern (string key)))
(defun ddd-key-to-provided-arg (key) (intern (s+ (string key) "-P")))
  
  
(defun arghack (x)
  (cond
   ((keywordp x) x)
   ((symbolp x) `(quote ,x))
   ;; ((and (listp x) (eq (first x) 'lambda)) x)
   (t (error "Bad define-doc-definer spec element: ~S" x))
   ))
    
(defun verify-doc-definer-info (specs info other-specs-allowed? macro-name)
  (declare (ignore other-specs-allowed?))
  (loop for (key data-form verifier nil nil) in specs 
        as record = (find key info :key 'first)
        as data = (rest record)
        when data  
        nconc
        (progn 
          (ecase data-form 
            (:list nil)
            (:non-nil-list 
             (unless data 
               (error "~S field for ~S is empty but must contain something!"
                      key macro-name)))
            (:exactly-one 
             (unless (= (length data) 1) 
               (error 
                #.(one-string-nl
                   "~S field for ~S must contain a single element but it in"
                   "fact contains ~D elements!")
                key macro-name (length data)))
             (setq data (first data)))
            (:one-or-none 
             (unless (< (length data) 2) 
               (error 
                #.(one-string-nl
                   "~S field for ~S must either be empty or contain a"
                   "single element but in fact it contains ~D elements!")
                key macro-name (length data)))
             (setq data (first data)))
            )
          (multiple-value-bind (ok? verified-data-or-oops)
              (funcall verifier data)
            (if ok? 

                (list key `(quote ,verified-data-or-oops))
              (error 
               #.(one-string-nl
                  "~S data, ~S,"
                  "for ~S, does not satisfy ~S!"
                  "Explanation: ~A")
               key data macro-name verifier verified-data-or-oops
               )))
                   
          )))

