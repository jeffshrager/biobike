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

;;; Author: JP Massar. Foo.

;;; Methods for the BIODEMO instance of BioLingua.

;;; This file isn't actually loaded.  It's contents must be moved to the BioDemo
;;; directory's biodemo-instance-init.lisp file.

(cformatt "If you see this the instance-init file for BioDemo is being loaded")

(cformatt "Defining methods for BioDemo instance.")

;;; This file can't be loaded by the standard system because the USER:BIODEMO
;;; class only exists when the BIODEMO instance is run.

(defmethod login-header ((app cl-user:biodemo))
  (html
   (:head 
    ((:font :size 6)
     (:center
      (:princ-safe 
       (s+ 
        (formatn
         "Welcome to ~A (" 
         (or *application-instance-pretty-name* "the Original BioBIKE Server"))
        (application-version app :mode :short)
        ")"
        ))
      :br
      (:princ-safe "Cyanobacterial Edition"))))))


(defmethod viewable-application-toplevel-directories ((app cl-user:biodemo))
  (list "Bioetc:" "home:"))

(cformatt "Method definitions for BioDemo complete.")
(cformatt "Biodemo instance-init file loaded.")
