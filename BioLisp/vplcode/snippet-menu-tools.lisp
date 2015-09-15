;;; -*- mode: Lisp; Syntax: Common-Lisp; Package: nvpl; -*-

(in-package :nvpl)

;;; +=========================================================================+
;;; | Copyright (c) 2006 John Myers, JP Massar                                |
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

;; Author: JP Massar.

(defun compile-snippet-menu (menu)
  (destructuring-bind (id type title &rest menu-items) menu
    `(,id ,type ,title ,@(compile-snippet-menu-items menu-items))))

(defun compile-snippet-multimenu (menu)
  (destructuring-bind (id type title single-action-items multiselect-items)
      menu
    `(,id ,type ,title 
          ,(compile-snippet-menu-items single-action-items)
          ,(compile-snippet-menu-items multiselect-items)
          )))

(defun compile-snippet-menu-items (menu-items)
  (mapcar 'compile-snippet-menu-item menu-items))

(defun compile-snippet-menu-item (menu-item)
  (destructuring-bind (text operator) menu-item
    `(,(if (integerp operator) operator (operator->opcode operator))
      :jbml-menu-entry 
      ,text
      )))

(defun create-choice-operator (count)
  (numeric-opcode-operator 
   'select-choice 
   (lambda (count) (intern (formatn "CHOICE~D" count) *vpl-package*))
   count
   ))

(defun create-keys-and-flags-operator (count)
  (numeric-opcode-operator 
   'choose-another-flag-or-key 
   (lambda (count)
     (intern (formatn "CHOOSE~D-ANOTHER-FLAG-OR-KEY" count) *vpl-package*))
   count
   ))