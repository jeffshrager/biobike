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

;;; Author: Jeff Shrager. 

;;; Methods for the TRBL instance of BioLingua.

(cformatt "If you see this the instance-init file for Trbl is being loaded")
(cformatt "Defining methods for Trbl instance.")

;;; This file can't be loaded by the standard system because the USER:TRBL
;;; class only exists when the TRBL instance is run.

(defmethod login-header ((app cl-user:trbl))
  (html
   (:head 
    ((:font :size 6)
     (:center
      (:princ-safe 
       (s+ 
        (formatn
         "Welcome to ~A (" 
         (or *application-instance-pretty-name*  
             "the TAIR/BioBike Server (TRBL))"))
        (application-version app :mode :short) 
        ")"))
      :br
      (:princ-safe "Arabidopsis thaliana Edition"))))))


(defmethod viewable-application-toplevel-directories ((app cl-user:trbl))
  (list "Bioetc:" "home:"))


(defmethod wb::application-packages-to-use ((application cl-user:trbl))
  (append (list :pb) (call-next-method)))

(defmethod bio::kdb-source-files ((kdb (eql :ocelot)))
  (list (merge-pathnames 
         (make-pathname :name "aracyc.ocelot")
         (bio::kdb-directory kdb)
         )))


(cformatt "Method definitions for Trbl complete.")
(cformatt "Trbl instance-init file loaded.")

