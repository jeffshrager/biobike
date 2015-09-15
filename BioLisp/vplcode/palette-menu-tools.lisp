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


;;; Tools specific to palette menus 

(defun create-palette-menu (id menu-title submenus entries &key (compile? t))
  (let ((menu (list id menu-title submenus entries)))
    (setq menu (if compile? (compile-palette-menu menu) menu))
    menu
    ))

(defun create-palette-submenu (title submenus entries)
  (list title submenus entries))

(defun compile-palette-menu (menu)
  (destructuring-bind (id title submenus entries) menu
    (create-palette-menu 
     id title 
     (compile-palette-submenus submenus)
     (compile-menu-items entries)
     :compile? nil
     )))

(defun compile-palette-submenus (submenus)
  (loop for (title sub-submenus entries) in submenus 
        collect
        (create-palette-submenu 
         title
         (when sub-submenus (compile-palette-submenus sub-submenus))
         (compile-menu-items entries)
         )))

(defun create-example-code-operator (count)
  (numeric-opcode-operator 
   'example-code-operator 
   (lambda (count) (intern (formatn "EXAMPLE-CODE-~D" count) *vpl-package*))
   count
   ))

(defun create-expunge-variable-operator (count)
  (numeric-opcode-operator 
   'expunge-a-variable
   (lambda (count)
     (intern (formatn "EXPUNGE-VARIABLE-~D" count) *vpl-package*))
   count
   ))

(defun create-expunge-function-operator (count)
  (numeric-opcode-operator 
   'expunge-a-function
   (lambda (count)
     (intern (formatn "EXPUNGE-FUNCTION-~D" count) *vpl-package*))
   count
   ))