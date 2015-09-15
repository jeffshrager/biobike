;;; -*- Package: bio; mode: lisp; base: 10; Syntax: Common-Lisp; -*-

(in-package :bio)

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


(defparameter *ec-ref-url-template*
    "http://us.expasy.org/cgi-bin/nicezyme.pl?~A")

(defparameter *kegg-ref-url-template*
  "http://www.genome.ad.jp/dbget-bin/www_bget?~A+~A")

(defparameter *metacyc-ref-url-template*
    "http://biocyc.org:1555/META/new-image?object=~A")

(defparameter *biolingua-help-url*
  "http://nostoc.stanford.edu/Docs/index.html")

;;; 20051205 JS removed this reference to 
;;; "http://ramsites.net/~biolingua/help/help.html"
;;; because it refers to "BioLingua"
(defparameter *biobike-help-url* 
  "http://nostoc.stanford.edu/Docs/index.html")

;;; 20051205 JS removed this reference to 
;;; "http://ramsites.net/~biolingua/logid-request-form.html"
;;; because it refers to "BioLingua"
(defparameter *biobike-login-request-url*
  "mailto:jshrager@stanford.edu")

