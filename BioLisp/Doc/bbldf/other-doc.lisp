;;; -*- Package: bbi; mode: lisp; base: 10; Syntax: Common-Lisp; -*-

(in-package :bbi)

;;; +=========================================================================+
;;; | Copyright (c) 2002, 2003, 2004 JP Massar, Jeff Shrager, Mike Travers |
;;; | |
;;; | Permission is hereby granted, free of charge, to any person obtaining |
;;; | a copy of this software and associated documentation files (the |
;;; | "Software"), to deal in the Software without restriction, including |
;;; | without limitation the rights to use, copy, modify, merge, publish, |
;;; | distribute, sublicense, and/or sell copies of the Software, and to |
;;; | permit persons to whom the Software is furnished to do so, subject to |
;;; | the following conditions: |
;;; | |
;;; | The above copyright notice and this permission notice shall be included |
;;; | in all copies or substantial portions of the Software. |
;;; | |
;;; | THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, |
;;; | EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF |
;;; | MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. |
;;; | IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY |
;;; | CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, |
;;; | TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE |
;;; | SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE. |
;;; +=========================================================================+

;;; Author: JP Massar, Arnaud Taton, Bogdan Mihai, and Jeff Elhai.

(eval-when (:compile-toplevel :load-toplevel :execute)
(import '(com.biobike.help:document-module
com.biobike.help:document-function) :bbi))

(DOCUMENT-MODULE other-commands
  "Functions dealing with useful commands"
  (:keywords :email)
  (:display-modes :bbl)
  (:alpha-listing? nil)
  (:submodules internal)
  #.`(:functions Comment Convert
        Enter My-session My-variables My-files Previous-result Result
      who-is-here? share unshare annotate-seed-feature change-subsystem-role
      send-message time-space-usage
      ))

(DOCUMENT-MODULE internal
   "Functions used by developers"  
   (:keywords :genome :description :name)  
   (:display-modes :bbl)  
   (:toplevel? nil)
   (:submodules collabrx)
   #.`(:FUNCTIONS
        bbl::{} bbl::[] bbl::[->]
      Clear-history wlisp:defun lisp:floor
      lisp:identity lisp:list 
      lisp:print lisp:progn utils:ref  
      lisp:sleep bbl::bbl-code 
      sequence-viewer calc edit-gene edit-object view-seed-gene
      + - * /
      true?
      ;; #.(intern "GOOGLE" cl-user::*vpl-package*)
      ))

(document-module collabrx
  "Functions that Jeff Shrager wants for Collabrx"
  (:display-modes :bbl)
  (:alpha-listing? t)
  (:toplevel? nil)
  (:functions 
   wb::create-vpl-web-service
   wb::help-me-program-this
   wb::in-sequence
   wb::in-parallel
   lisp:go
   wb::exit-workflow
   ))

