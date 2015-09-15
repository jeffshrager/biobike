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

;;; Methods for the BIODEMO instance of BioLingua.

;;; This file isn't actually loaded. It's contents must be moved to the BioDemo
;;; directory's biodemo-instance-init.lisp file.

(cformatt 
 "If you see this the instance-init file for BioDemoVCU is being loaded")

;;; This file can't be loaded by the standard system because the USER:BIODEMO
;;; class only exists when the BIODEMO instance is run.

(defmethod login-header ((app cl-user:biodemovcu))
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
       (s+ "Public Cyanobacterial Edition " 
           (application-version app :mode :short)
           )))))))


(defmethod viewable-application-toplevel-directories ((app cl-user:biodemovcu))
  (list "Bioetc:" "home:"))

(defmethod login-addenda ((app cl-user:biodemovcu))
  (html
   (:h3 (:princ-safe "What is BioBike?") :br)
   (:ul
    (:li (:b "A knowledge resource") :br
     (:princ-safe
      (one-string
       "BioBike brings together available genomic, metabolic, and "
       "experimental data pertinent to a given research community"))
     :br)
    (:li (:b "A programming environment") :br
     (:princ-safe
      (one-string
       "BioBike provides a programming language accessible to biologists "
       "without programming experience."))
     :br 
     (:princ-safe
      (one-string
       "Built into it are concepts familiar to molecular biologists and "
       "powerful tools to manipulate and analyze biological data.")
      )))
   ((:a :href bio::*biobike-help-url*)
    "Click here for help")
   (:princ "&nbsp;&nbsp;&nbsp;")
   ((:a :href (forward-funcall 'make-doc-directory-url))
    "Click here for other BioBike resources")
   :br
   (:h3 (:b "Please Note:"))
   (:ul
    (:li "This is a fully operational BioBike server except as noted below.")
    (:li 
     "The duration of computations that can be executed "
     "on the demo server is limited to 40 seconds per computation. "
     "(Although your time online is not limited.)")
    (:li 
     "Your account and any information you may leave on the server "
     "may be erased at any time, unless you've made prior arrangements.")
    (:li 
     "All interactions with the server are recorded and may be perused "
     "by the developers and system administrators.")
    (:li 
     "We do not guarantee the validity of the data, "
     "nor the stability of the server.")
    (:li 
     "By logging in you implicitly acknowledge this "
     "and agree to the legalese here")
    )
   :p
   :hr
   ))


(cformatt "Method definitions for BioDemoVCU complete.")
(cformatt "BiodemoVCU instance-init file loaded.")









