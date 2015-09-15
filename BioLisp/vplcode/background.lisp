;;;; -*- mode: Lisp; Syntax: Common-Lisp; Package: nvpl; -*-

(in-package :nvpl) 

;;; +=========================================================================+
;;; | Copyright (c) 2005 JP Massar, Jeff Elhai, Mark Slupesky, Peter Seibel   |
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

;;; Author: JP Massar. 


(defparameter *elhai-yellow-background-color* "#ffffc0")
(defparameter *elhai-orange-background-color* "#ffc000")

(defparameter *elhai-key-and-flag-background-color* "#70ffff")

(defparameter *unopened-hole-background-color* "#FFFFFF")

(defun toggle-background-color (bc)
  (ecase bc 
    (:elhai-yellow :elhai-orange)
    (:elhai-orange :elhai-yellow)
    ))

(defun provide-background-color-for-snippet-and-children (s)
  (provide-background-colors s))
     
(defmethod provide-background-colors ((snippet call-snippet))
  (maybe-toggle-background snippet t)
  (provide-background-for-children snippet))

(defmethod provide-background-colors ((snippet snippet))
  (maybe-toggle-background snippet nil)
  (provide-background-for-children snippet)
  )

(defun maybe-toggle-background (snippet toggle?)
  (set-snippet-property 
   snippet :background-color 
   (if (toplevel-snippet? snippet)
       :elhai-yellow
     (funcall 
      (if toggle? 'toggle-background-color 'identity) 
      (get-snippet-property (snippet-parent snippet) :background-color)
      ))))

(defun provide-background-for-children (snippet)
  (mapc 
   'provide-background-color-for-snippet-and-children 
   (snippet-children snippet)
   ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun literal-snippet-text-and-text-flags (snippet)
  (multiple-value-bind (function flags)
      (case (get-snippet-property snippet :class)
        (:function-name
         (values
          'snippet-string-value
          `(:jbml-color ,function-name-color :jbml-b)))
        (:define-function-name
         (values
          'snippet-string-value
          `(:jbml-color ,define-function-name-color :jbml-b)))
        (:macro-name
         (values
          'snippet-string-value
          `(:jbml-color ,macro-name-color :jbml-b)))
        (:loop-tag 
         (values
          'snippet-string-value
          `(:jbml-color ,loop-tag-color)))
        (:loop-toplevel-constituent-tag 
         (values
          'snippet-string-value
          `(:jbml-color ,loop-toplevel-constituent-color)))
        (:curly-tag
         (values
          'snippet-string-value
          `(:jbml-color ,curly-bracket-color)))
        (:ref-tag
         (values
          'snippet-string-value
          `(:jbml-color ,ref-bracket-color)))
        (:optional-tag 
         (values
          'snippet-string-value
          `(:jbml-color ,optional-tag-color :jbml-b :jbml-i)))
        (:define-function-tag
         (values
          'snippet-string-value
          `(:jbml-color ,define-function-tag-color)))
        (:define-function-token 
         (values
          'snippet-string-value
          `(:jbml-color ,define-function-token-arg-color)))
        (:keyword
         (values
          'snippet-string-value
          `(:jbml-color ,keyword-name-color)))
        (:flag
         (values
          'snippet-string-value
          `(:jbml-color ,flag-name-color)))
        (:equal-sign 
         (values
          'snippet-string-value
          `(:jbml-color ,equal-sign-color)))
        (otherwise 
         (values
          'snippet-string-value
          `(:jbml-color ,default-literal-color))))
    (values 
     (funcall function snippet)
     (copy-list flags)
     )))

(defun snippet-string-value (snippet)
  (string (snippet-value snippet)))

(defun snippet-uppercase-string-value (snippet)
  (string-upcase (snippet-value snippet)))

(defun snippet-lowercase-string-value (snippet)
  (string-downcase (snippet-value snippet)))

(defun keyword-text (snippet)
  (let ((sv (snippet-value snippet)))
    (typecase (snippet-parent snippet)
      (keyword-snippet 
       (if (non-bbl-function-node? 
            (snippet-parent (snippet-parent (snippet-parent snippet))))
           (formatn "~S" sv)
         (formatn "~A" sv)
         ))
      (otherwise (vpl-internal-error "Only called on keyword literals!"))
      )))