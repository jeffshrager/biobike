;;; -*- Package: cl-user; mode: lisp; base: 10; Syntax: Common-Lisp; -*-

(in-package :cl-user)

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


(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :aserve))

(defpackage :com.biobike.help
  (:use :cl
   :net.aserve
   :net.html.generator
   :utils)
  (:nicknames :help)
  (:import-from :weblistener 
   :execute-with-standard-weblistener-environment
   :with-standard-weblistener-page-header
   :*sessionid*
   )
  (:export
   :make-help-topic-url
   :make-help-glossary-entry-url
   :make-help-module-url
   :make-help-documentation-file-url
   :make-help-symbol-doc-url
   :make-help-function-documentation-url
   :make-help-glossary-url
   :make-help-modules-url
   :make-help-tutorial-url
   :alpha-listing?
   :apropos+
   :author
   :can-convert-from
   :common-lisp
   :default-value
   :def-documentation-file
   :def-glossary-entry
   :def-live-tutorial
   :def-module
   :def-topic
   :description
   :descriptor
   :dfdoc
   :display-modes
   :docstring
   :document-function
   :document-module
   :documentation-file 
   :documented
   :dtype
   :extract-text
   :examples
   :examples-package
   :explicitly-documented-p
   :find-documentation
   :filename
   :file-type
   :flavor
   :frame
   :function-documentation
   :functions
   :glossary-entry
   :help
   :keyword-name
   :keywords
   :label
   :lhtml-function
   :macro-documentation
   :macros
   :mapping-style
   :module
   :modules
   :name
   :other
   :parameter-type
   :parameter-documentation
   :parameters
   :parse-document-function-example
   :publish-help-pages
   :referred-to-by
   :return-values
   :section-header
   :see-also
   :sort-order
   :start-function
   :stype
   :submodules
   :symbol-doc
   :synonyms
   :syntax
   :text
   :topic
   :toplevel?
   :tutorial
   :undocument-function
   :undocument-module
   :url
   :user-mode
   :value-type
   :variable-documentation
   :variables
   :vpl-syntax
   ))
