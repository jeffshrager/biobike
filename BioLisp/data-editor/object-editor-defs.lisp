 ;;; -*- Package: data-editor; mode: lisp; base: 10; Syntax: Common-Lisp; -*-

(in-package :data-editor)

;;; +=========================================================================+
;;; | Copyright (c) 2009 JP Massar                                            |
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

;;; Author: JP Massar

(defmacro create-object-editor-defs (object-type slots &key accessors setters)
  `(progn
     (setf (get ',object-type :object-slots) ',slots)
     (setf (get ',object-type :object-accessors) 
           ,(if accessors 
                `',accessors
              `',(mapcar 
                  (lambda (x)
                    (intern 
                     (formatn "~A-~A" object-type x)
                     (symbol-package object-type)
                     ))
                  slots
                  )))
     (setf (get ',object-type :object-setters)
           ,(if setters
                `(list ,@setters)
              `(list 
                ,@(mapcar 
                   (lambda (x) 
                     `(lambda (obj val)
                        (setf 
                         (,(intern 
                            (formatn "~A-~A" object-type x)
                            (symbol-package object-type))
                          obj)
                         val
                         )))
                   slots
                   ))))))

#+test
(defstruct foobar foo bar baz)
#+test
(create-object-editor-defs foobar (foo bar baz))

(create-object-editor-defs bbi::labeled-sequence (bbi::label sequence))
              
(create-object-editor-defs 
 bbi::domain 
 (gene domain start stop score evalue definition))
