;;;; -*- mode: Lisp; Syntax: Common-Lisp; Package: wb; -*-

(in-package :wb)

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

;;; Author: JP Massar. 

;;; Methods for the BIOVCU instance of BioLingua.

(defparameter *instance-string* "BIOVCU")

;;; This file isn't actually loaded.  It's contents must be moved to the Biovcu
;;; directory's biodemo-instance-init.lisp file.

(cformatt 
 (one-string
  "If you see this the instance-init file for BioLingua instance " 
  "~A is being loaded")
 *instance-string*)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cformatt "Defining methods for ~A instance." *instance-string*)

;;; This file can't be loaded by the standard system because the USER:biovcu
;;; class only exists when the BIODEMO instance is run.

(defmethod login-header ((app cl-user:biovcu))
  (html
   (:head 
    ((:font :size 6)
     (:center
      (:princ-safe 
       (formatn
        "Welcome to ~A" 
        (or *application-instance-pretty-name* 
            "the Virginia Commonwealth University BioBike Server")))
      :br
      (:princ-safe 
       (s+ "Cyanobacterial Edition " (application-version app :mode :short))
       ))))))

(cformatt "Method definitions for ~A complete." *instance-string*)
(cformatt "~A instance-init file loaded." *instance-string*)

