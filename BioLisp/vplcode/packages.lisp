;;; -*- mode: Lisp; Syntax: Common-Lisp; Package: user; -*-

(in-package :cl-user)

;;; +=========================================================================+
;;; | Copyright (c) 2006 JP Massar, John Meyers                               |
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

;; Author: JP Massar, John Meyers.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :aserve)
  #-:lispworks 
  (require :process)
  )

#+:allegro
(defpackage :vpl
  (:nicknames :nvpl)
  (:use :cl :utils :com.gigamonkeys.json) 
  (:import-from :com.gigamonkeys.ajax :send :register-message-handler)
  #.`(:import-from :bbl ,@bbi::*bbl-loop-tokens*)
  #.`(:import-from :net.html.generator :html)
  (:export :*ajax-directories*)
  )

#+lispworks
(defpackage :vpl
  (:nicknames :nvpl)
  (:use :cl :utils :com.gigamonkeys.json)
  #.`(:import-from :bbl ,@bbi::*bbl-loop-tokens*)
  #.`(:import-from :net.html.generator :html)
  (:export :*ajax-directories*))
