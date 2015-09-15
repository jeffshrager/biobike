;;; -*- Package: help; mode: lisp; base: 10; Syntax: Common-Lisp; -*-

(in-package :help)

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

(document-module web-internals
  "Internal weblistener functions"
  (:display-modes :biolisp)
  (:functions
   function-documentation
   parameter-documentation
   extract-text))

(document-function help:function-documentation
  (:summary "Get at parts of the documentation of a named function.")
  (:syntax (function-documentation function-name documentation-element))
  (:parameters
   (function-name 
    :value-type symbol :docstring "The function whose documentation we want.")
   (documentation-element
    :value-type symbol :docstring "The documentation element we want."))
  (:returns
   "The named documentation element associated with the named function.")
  (:text
   (:p "Access function documentation created with DOCUMENT-FUNCTION.")
   (:p "The  valid values for " (:code "documentation-element") " are:"
       (:ul
	(:li (:code "name"))
	(:li (:code "docstring"))
	(:li (:code "text"))
	(:li (:code "keywords"))
	(:li (:code "see-also"))
	(:li (:code "author"))
	(:li (:code "explicitly-documented-p"))
	(:li (:code "module"))
	(:li (:code "parameters"))
	(:li (:code "return-values"))
	(:li (:code "syntax"))
	(:li (:code "examples"))
	(:li (:code "synonyms"))
	(:li (:code "flavor")))))
  (:see-also document-function))

(document-function help:parameter-documentation
  (:summary "Get at parts of the documentation of a function parameter.")
  (:syntax (parameter-documentation object documentation-element))
  (:parameters
   (object 
    :value-type parameter-documentation 
    :docstring
    "An element of a list returned by (function-documentation fn 'parameters)")
   (documentation-element
    :value-type symbol :docstring "The documentation element we want."))
  (:returns 
   "The named documentation element associated with the named function.")
  (:text
   (:p "Access function documentation created with DOCUMENT-FUNCTION.")
   (:p "The  valid values for " (:code "documentation-element") " are:"
       (:ul
	(:li (:code "name"))
	(:li (:code "docstring"))
	(:li (:code "text"))
	(:li (:code "keywords"))
	(:li (:code "see-also"))
	(:li (:code "author"))
	(:li (:code "explicitly-documented-p"))
	(:li (:code "default-value"))
	(:li (:code "value-type"))
	(:li (:code "parameter-type"))
	(:li (:code "keyword-name"))
	(:li (:code "synonyms"))
	(:li (:code "mapping-style"))
	(:li (:code "can-convert-from")))))
  (:see-also document-function function-documentation))

(document-function help:extract-text
  (:summary 
   #.(one-string-sp
      "Extract the text from a FOO/HTML sexp, optionally"
      "including the text of attributes."))
  (:syntax (foo-sexp &key include-attributes))
  (:parameters
   (foo-sexp :value-type t :docstring "An s-expression in FOO/HTML syntax.")
   (include-attributes 
    :value-type boolean
    :docstring "When true, include the text of attributes in result."))
  (:text
   "Text is returned pretty much as is, except with newlines
   separating block and paragraph level elements. The text may or
   may not be suitable for display to a human being; the main
   purpose of this function is to extract text for use in building a 
   full-text index of documentation.")
  (:examples
   ("(extract-text '(:p \"sometext\"))" (:string "sometext"))
   ("(extract-text '(:body (:p \"sometext\") (:p \"some more text\")))" 
    (:string "
sometext
some more text
"))
  ("(extract-text '(:a :href \"http://www.gigamonkeys.com/\" \"Gigamonkeys\"))"
   (:string "Gigamonkeys"))
  ("(extract-text '(:a :href \"http://www.gigamonkeys.com/\" \"Gigamonkeys\") :include-attributes t )"
   (:string "http://www.gigamonkeys.com/ Gigamonkeys")))
  (:returns "The text of the FOO/HTML without HTML tags."))


