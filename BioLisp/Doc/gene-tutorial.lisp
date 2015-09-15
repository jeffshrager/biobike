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

;;; Author:  JP Massar, Mark Slupesky.  


(eval-when (:compile-toplevel :load-toplevel :execute)
  (export 'what-is-a-gene-tutorial-link))


(publish
 :path "/genetutorial.html" 
 :content-type cl-user:*html-publish-content-type*
 :function
 (lambda (req ent)
   (let* ((input (request-query req))
          (package-name (url-parameter-value :pkg input))
          (package-symbol (keywordize package-name))
          )
     (wb::execute-with-standard-weblistener-environment
      req ent package-symbol
      (lambda () (html-for-gene-tutorial))
      ))
   ))

(defun what-is-a-gene-tutorial-link ()
  (cformatt "Please click the below link.")
  (wb::make-url 
   :path (wb::url-and-args "/genetutorial.html" :pkg wb:*sessionid*)
   :display-string "Begin the tutorial."
   :target "_blank"))

(defun make-javascript-for-gene-tutorial ()
  (one-string
   (javascript-header)
   (javascript-opener-display-new-url :name "weblistenerexecute")
   (javascript-trailer)
   ))

(defun html-for-gene-tutorial ()
  (html
   (:head 
    (:princ (make-javascript-for-gene-tutorial))
    ((:font :size 4 :color "blue")
     (:b
      (:center
       (:princ-safe "Welcome to Professor Elhai's gene tutorial using Biobike")
       :br
       (:princ-safe "A C G T . . . . . . . . 01000001 01000011 01000111 01010100")))))
      :br
     (:body
      ((:font :size 3 :color "olive")
       (:center
        (:b (:princ-safe "Introduction"))))
      ((:font :size 3)
       (:princ-safe 
        (one-string-nl 
         "You may have trouble finding a matching pair of socks in the morning.  "
         "Well, think about finding a gene amongst the few billion nucleotides "
         "that comprise the human genome! In this tour, we'll "
         "confront the problem of how to find a gene, which will lead us to the question: "
         "What is a gene anyway? To answer this question, we have to look at real "
         "sequences and real genes. Perhaps some generalities will pop out that enable "
         "us to come up with a definition."))
       :br
       :br
       (:princ-safe
        (one-string-nl
         "In this guide, you are in control of two windows: this Tutorial window,"
         "and the Biobike Listener window which led you here.  Your goal is to get"
         "comfortable using the Listener.  To that end, the following demonstrations"
         "give you a nice look at some of Biobike's powerful features."
         "Each demonstration has a 'Do It' button, which is a quick way to see"
         "each feature in action.  Clicking this button is eqivalent to "
         "your typing in the expression into the input box of the Listener window"
         "and hitting Enter."))
       ))
     :br
     :br
     (html-for-gene-tutorial-1)
     :br
     :br
     (html-for-gene-tutorial-2)
     :br
     :br
     (html-for-gene-tutorial-3)
     :br
     :br
     (html-for-gene-tutorial-4)
     :br
     :br
     (html-for-gene-tutorial-5) 
     :br
     :br
     (html-for-gene-tutorial-6)
     :br
     :br
     (html-for-gene-tutorial-7)
     :br
     :br
     ))


(defmacro gt (name step text eval)
  `(html
    ((:font :size 3 :color "olive")
     (:center
      (:b (:princ-safe ,name))))
    (:b (:princ-safe (formatn "Step ~D." ,step)))
    (:princ-safe ,text)
    :br
    (:princ
     (html-for-button-to-execute-lisp-expression ,eval))))

(defun html-for-gene-tutorial-1 ()
  (gt "*all-organisms*" 1
      (one-string
       "What organisms does Biobike know about? "
       "Typing *all-organisms* at the command line will let us know. ") 
      "*all-organisms*"))

(defun html-for-gene-tutorial-2 ()
  (html
   ((:font :size 3 :color "olive")
    (:center
     (:b (:princ-safe "NICKNAMES-OF"))))
    (:b "Step 2. ")
    (:princ-safe 
     (formatn 
      (one-string-nl
      "But who wants to type those long names?  You can find out the nicknames of all"
      "organisms by entering (nicknames-of *).  * is a shortcut...it simply refers to the last output"
      "that the Listener gave you.  In this case, (nicknames-of *) is exactly the same as (nicknames-of ~S)")
      *loaded-organisms*
      )
     )
   :br
    (:princ
     (html-for-button-to-execute-lisp-expression "(nicknames-of *)"))))

(defun html-for-gene-tutorial-3 ()
  (gt "More about nicknames" 3 
      (one-string-nl
      "Let's focus on one of the smallest cyanobacteria, "
      "Prochlorococcus marinus ss120."
      "A conveniently short nickname of the organism is ss120." 
      "What do you get when you simply"
      " type in this nickname?") 
      "ss120"))

(defun html-for-gene-tutorial-4 ()
  (gt "SEQUENCE-OF" 4 
      (one-string-nl
       "\"Small\" is a relative term.  How much DNA does ss120 have?"
       "We can ask for its sequence by entering (sequence-of ss120)") 
      "(sequence-of ss120)"))

(defun html-for-gene-tutorial-5 ()
  (gt "LENGTH-OF" 5 
      (one-string-nl
       "Perhaps that wasn't very edifying.  If we can't see the whole sequence,"
       "then let's at least find out how big it is.  We do this by entering"
       "(length-of ss120)")
      "(length-of ss120)" ))

(defun html-for-gene-tutorial-6 ()
  (gt "GENES-OF" 6
      (one-string-nl
       "That's how big it is.  How many genes does it have?"
       "We can enter (genes-of ss120)")
      "(genes-of ss120)"))


(defun html-for-gene-tutorial-7 ()
  (gt "COUNT-OF" 7 
      (one-string-nl
       "Again, too many to count!  Get a count of the genes by"
       "entering (count-of *).")
      "(count-of *)"))


(defun html-for-button-to-execute-lisp-expression (expression-as-string)
  (formatn
      (one-string 
       "<input type=BUTTON value=\"Do It\" " 
       "onclick=weblistenerexecute"
       "(\"/weblistener-evalstring.html?PKG=~a&evalstring=~a\")>") 
      wb::*sessionid* (url-safe-string expression-as-string)))







 
