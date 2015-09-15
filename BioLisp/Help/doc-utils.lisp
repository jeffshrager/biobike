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

(defparameter *new-text-extraction-mode* nil)


(defun extract-text (foo-sexp &key include-attributes)
  "Extract the text from a FOO sexp, optionally including text of attributes."
  (with-output-to-string (text)
    (labels 
        ((extract (thing)
           (typecase thing
             (cons
              (multiple-value-bind (tag attrs body)
                  (com.gigamonkeys.foo::parse-cons-form thing)
                (when (member tag com.gigamonkeys.foo.html:*block-elements*)
                  (write-char #\Newline text))
                (when include-attributes
                  (loop for (nil v) on attrs by #'cddr do
                        (write-string v text)
                        (write-char #\Space text)))
                (loop for subthing in body do (extract subthing))
                (when (member tag com.gigamonkeys.foo.html:*paragraph-elements*)
                  (write-char #\Newline text))))
             (string (write-string thing text)))))
      (extract foo-sexp))))

(defun a-or-an (string)
  (if (find (char-downcase (char (string string) 0)) "aeiou") "an" "a"))

(defun contains-newline? (s) (find #\newline s))  
            
(defun symbol-to-default-doc-type (s)
  (cond
   ((fboundp s) :function)
   ((boundp s) :variable)
   ((ignore-errors (typep nil s)) :type)
   (t :function)
   ))
    
(defun print-symbol-docobj (obj stream type-abbrev)
  (let* ((symbol (help:name obj))
         (package (symbol-package symbol)))
    (format stream "<Docobj ~A::~A (~A)>"
            (let ((nicks (copy-list (package-nicknames package))))
              (if nicks
                  (first (sort nicks '< :key 'length))
                (package-name package)
                ))
            (string symbol)
            type-abbrev
            )))


(defgeneric abbr-for-doc-item (doc-item)
  (:documentation
   "Returns a 3 or 4 letter abbrev indicating the type of documentation object."
   ))

(defmethod abbr-for-doc-item ((doc-item t))
  (let ((type (type-of doc-item)))
    (unless (member type *documentation-types*)
      (error "Internal error: Type ~S is not a documentation type!" type))
    (subseq (string type) 0 3)
    ))

(defmethod abbr-for-doc-item ((doc-item help:function-documentation)) "FUNCTION")
(defmethod abbr-for-doc-item ((doc-item help:glossary-entry)) "GLOSSARY")
(defmethod abbr-for-doc-item ((doc-item help:documentation-file)) 
  (if (descriptor doc-item)
      (string-upcase (string (descriptor doc-item)))
    "FILE"
    ))
(defmethod abbr-for-doc-item ((doc-item help:topic)) "TOPIC")
(defmethod abbr-for-doc-item ((doc-item help:module)) "MENU")
(defmethod abbr-for-doc-item ((doc-item help::tutorial)) "TUTORIAL")
(defmethod abbr-for-doc-item ((doc-item help::symbol-doc)) 
  (ecase (help::dtype doc-item)
    (:function "FUNCTION")
    (:variable "VARIABLE")
    (:type "TYPE")
    ))

