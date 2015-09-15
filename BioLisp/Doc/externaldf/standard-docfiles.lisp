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

(def-documentation-file 
    "bbl-description.html"
  (:summary "A semi-formal description of the BBL language.")
  (:author "JP Massar")
  (:see-also 
   (glossary "Bracket notation")
   ;; (df "bbi:define-function")
   )
  (:keywords "syntax" "semantics" "1-based indexing")
  )

(def-documentation-file 
    "workspaces.txt"
  (:summary "A tutorial on using the BioBike workspace facility.")
  (:author "JP Massar")
  (:see-also 
   webuser:create-workspace
   webuser:save-workspace
   webuser:restore-workspace
   webuser:purge-workspace-versions
   webuser:show-workspace
   webuser:show-workspace-versions
   webuser:show-available-workspaces)
  (:keywords "packages" "saving" "save" "versions")
  )

(def-documentation-file 
    "jp.pdf"
  (:summary "This is a pdf file that describes biobike language syntax")
  (:author "JP Massar")
  (:see-also 
   bbi::sequence-of
   webuser:create-workspace
   webuser:save-workspace
   webuser:restore-workspace
   webuser:purge-workspace-versions
   webuser:show-workspace
   webuser:show-workspace-versions
   webuser:show-available-workspaces)
  (:keywords "apollo" "packages" "saving" "save" "versions" "sequence-of")
  (:descriptor "test")
  )

(def-documentation-file 
   ;; this file name needs to be consistent with the defun-opcode
   ;; entry for troubleshooting-operator in
   ;; .../biolisp/vplcode/utility-operators.lisp
    "externaldf/textfiles/troubleshooting.html"
  (:summary "This is the troubleshooting file")
  (:author "JP Massar")
  (:keywords "zeus" "troubleshooting" "bugs" "known")
  )

#+obsolete
(def-documentation-file 
   ;; this file name needs to be consistent with the defun-opcode
   ;; entry for troubleshooting-operator in
   ;; .../biolisp/vplcode/utility-operators.lisp
    "externaldf/textfiles/ts-and-kb2.html"
  (:summary "This is the second troubleshooting file")
  (:author "JP Massar")
  (:keywords "zeus" "troubleshooting" "bugs" "known")
  )


   
  
