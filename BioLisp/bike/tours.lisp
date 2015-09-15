;;;; -*- mode: Lisp; Syntax: Common-Lisp; Package: bbi; -*-

(in-package :bbi) 

;;; Tours 

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant bbi-td (translate-simple-lp "biol:bike;Tutorials;")))

(progn
  (help:def-live-tutorial "Sticks"
    (:file-type :html) 
    (:user-mode :bbl)
    (:filename #.(s+ bbi-td "sticks-tutorial")) 
    (:sort-order 1)
    (:description "What is a functional language?")
    (:text
     "Guides the user through what a functional language is."
     "There would be more detail here if somebody put it in."))
  (help:def-live-tutorial "Genes"
    (:file-type :html) 
    (:user-mode :bbl)
    (:filename #.(s+ bbi-td "genes-tutorial")) 
    (:sort-order 2)
    (:description "What are genes?"))
  (help:def-live-tutorial "Alien DNA" 
    (:file-type :html) 
    (:user-mode :bbl)
    (:filename #.(s+ bbi-td "alien-world"))
    (:sort-order 3)
    (:description "How to crack the genetic code?"))
  )



