;;; -*- mode: Lisp; Syntax: Common-Lisp; Package: user; -*-

(in-package :cl-user)

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

;;;; Packages & miscellaneous setup for Biolisp WebListener 

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :aserve)
  #+:allegro
  (require :smtp)
  )

(defpackage :weblistener 
  (:nicknames :wb)
  (:use
   :cl-user
   :frames
   :genericdb
   :net.aserve
   :net.html.generator 
   :utils
   :wlisp)
  (:export 
   "START-BIOWEBLISTENER" "START-WEBLISTENER"
   "STOP-BIOWEBLISTENER" "STOP-WEBLISTENER"
   "PARSE-XML" "WEB-PAGE-CONTENTS" "WITH-OPEN-WEB-PAGE"
   "WEBPAGE" "*USERNAME*" "*SESSIONID*")
  #.`(:shadowing-import-from :cl-user 
      cl-user::in 
      cl-user::os?
      ,@(mapcar 'config-varname *configuration-variables*)))

(defparameter wb:*username* nil 
  #.(utils:one-string-nl
     "Login name symbol and package name symbol of user currently executing."
     "Everything keys off this symbol and it needs to be rebound in every"
     "toplevel published URL.  Its value is obtained from the PKG argument"
     "passed through to (almost) every URL."
     ))

(defparameter wb:*sessionid* nil
  #.(utils:one-string-nl
     "Session ID of user's current session.  This replaces wb:*username*"
     "as the symbol many things key off of.  Its value is obtained from"
     "the PKG argument passed through to (almost) every URL, and"
     "the value of wb:*username* is obtained now by lookup from"
     "this variable.  *** This is not implemented yet. *** "
     ))

;; This is bound to T whenever the VPL itself receives a message from the 
;; client and starts to process it.  
(defvar wb::*vpl-executing?* nil)

;; This is bound to T when the VPL is executing user code.  
(defvar wb::*vpl-evaluating?* nil)

;; What used to be VISTOOLS
(defpackage :webuser
  (:use :wlisp :utils :weblistener))

(defpackage published-protocols
  (:nicknames :pp)
  (:use :wlisp :webuser)
  )

;;; For persistent 'gensyms'

(defpackage $$)

