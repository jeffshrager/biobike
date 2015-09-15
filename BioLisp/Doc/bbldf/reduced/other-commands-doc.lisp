;;; -*- Package: bbi; mode: lisp; base: 10; Syntax: Common-Lisp; -*-

(in-package :bbi)

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

;;; Author: JP Massar, Arnaud Taton, Bogdan Mihai and Jeff Elhai.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (import '(
            com.biobike.help:document-module
               com.biobike.help:document-function) :bbi))

(DOCUMENT-MODULE other-commands
  "Functions dealing with useful commands"
  (:keywords :email)
  (:display-modes :bbl)
  (:alpha-listing? nil)
  #.`(:functions BBL-version Comment Convert
      Enter who-is-here? bbl::bbl-code
      ))



;;================================= WHO-IS-HERE? ==============================

(DOCUMENT-FUNCTION WHO-IS-HERE?
(:PARAMETERS)
(:VPL-SYNTAX ((:img :src "/weblistenerdocs/snagglepuss.jpg")
              (:img :src "/weblistenerdocs/snagglepuss.jpg")))
(:EXAMPLES 

"
      (WHO-IS-HERE?)
      :: 
      USER NAME        LAST ACTIVITY

      ATATON           5 minutes ago
      System startup at 05/30/06 08:57

      --> NIL
"
)
                  
(:TEXT (:p "Print the User Names logged onto the server, their last activity and when the system started up."))
(:SEE-ALSO USERS #+not-yet MESSAGE-TO ANNOUNCE))


