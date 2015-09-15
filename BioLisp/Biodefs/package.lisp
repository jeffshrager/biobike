;;; -*- Package: cl-user; mode: lisp; base: 10; Syntax: Common-Lisp; -*-

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

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :aserve))


(defpackage "BIOUTILS"
  (:nicknames "BUTILS")
  (:use :wlisp :utils :frames)
  (:import-from :wb :*safety*)
  )


(defpackage "BIOLISP"
  (:nicknames "BIO")
  #.`(:shadowing-import-from :cl-user 
      cl-user::in 
      cl-user::os?
      ,@ (mapcar 'config-varname *configuration-variables*)
      )
  (:shadowing-import-from :net.aserve :start)

  #+:sframes 
  (:shadowing-import-from :wlisp
   :setq :defparameter :multiple-value-setq :push :pushnew :defmacro
   :defconstant :defvar :setf :pop :defun)

  (:use
   :cl-user
   :webuser 
   :wlisp
   :utils
   :frames
   :genericdb 
   :net.html.generator
   :net.aserve
   :bioutils
   ))

;; Import XML/web functions from Weblistener into BioLisp and export them.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (import '(wb:parse-xml wb:web-page-contents wb:with-open-web-page) :biolisp)
  (export '(wb:parse-xml wb:web-page-contents wb:with-open-web-page) :biolisp)
  )

;; Make all the external symbols of UTILS also external wrt BIOLISP,
;; so a package using BIOLISP doesn't have to also use UTILS.
;; Likewise for FRAMES.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (do-external-symbols (var (find-package :utils))
    (export var (find-package :biolisp)))
  (do-external-symbols (var (find-package :frames))
    (export var (find-package :biolisp)))
  )

(eval-when (:compile-toplevel :load-toplevel :execute)
  (ecase user::*frame-system-version*
    (:old nil)
    (:sframes 
     (shadowing-import 
      '(wlisp::setq 
        wlisp::defparameter wlisp::multiple-value-setq
        wlisp::push wlisp::pushnew wlisp::defmacro
        wlisp::defconstant wlisp::defvar wlisp::setf
        wlisp::pop wlisp::defun
        )
      :biolisp 
      ))))

(defpackage "ORG-ALIASES" 
  (:nicknames "OA")
  (:use)
  )

(defparameter bio::*organism-nickname-package* :oa)

;;; So quasi-independent packages like FASTAS can have a simple
;;; mechanism to conditionalize code as to whether they are running
;;; with all the BIOLISP functionality or not.

(pushnew :biolisp *features*)


