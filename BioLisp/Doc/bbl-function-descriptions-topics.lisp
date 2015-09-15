;;; -*- Package: help; mode: lisp; base: 10; Syntax: Common-Lisp; -*-

(in-package :help)

;;; +=========================================================================+
;;; | Copyright (c) 2002-2006 JP Massar, Jeff Shrager, Peter Seibel           |
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

;;; Authors:   Emily Niman, Bogdan Mihai, Arnaud Taton, Jeff Elhai, JP Massar, Peter Seibel.     


(def-topic "Description of functions"
(:text
(:table    :align "middle" :width "80%"
(:tr (:td ((:a :href (:print (make-help-module-url :name "ARITHMETIC-AGGREGATES")))"ARITHMETIC-AGGREGATES")) (:td "Arithmetic functions that operate on lists or tables of numbers"))
(:tr (:td ((:a :href (:print (make-help-module-url :name "ARITHMETIC-BASIC")))"ARITHMETIC-BASIC")) (:td "Arithmetic functions that operate on numbers "))
(:tr (:td ((:a :href (:print (make-help-module-url :name "string-functions")))"STRING FUNCTIONS")) (:td "Functions that operate on strings"))
(:tr (:td ((:a :href (:print (make-help-module-url :name "gene-protein")))"GENE-PROTEIN")) (:td "Functions that operate on genes and proteins"))
(:tr (:td ((:a :href (:print (make-help-module-url :name "genome")))"GENOME")) (:td "Functions that operate on genomes"))
(:tr (:td ((:a :href (:print (make-help-module-url :name "list-and-table-functions"))) "LIST/TABLE FUNCTIONS")) (:td "Functions that operate on lists and tables"))
(:tr (:td ((:a :href (:print (make-help-module-url :name "logic")))"LOGIC"))(:td "Logic functions that operate with boolean values"))
(:tr (:td ((:a :href (:print (make-help-module-url :name "other-commands")))"OTHER COMMANDS")) (:td "Some useful functions"))
(:tr (:td ((:a :href (:print (make-help-module-url :name "sequence-functions")))"SEQUENCE FUNCTIONS")) (:td "Functions that operate on sequences"))
(:tr (:td ((:a :href (:print (make-help-module-url :name "input-output")))"INPUT/OUTPUT")) (:td "Functions for INPUT and OUTPUT data"))) 

(:p)   
   
(:table :align "middle" :width "80%"  
(:tr  
(:td  :width "26.7%" :align "left" ((:a :style "text-decoration:none" :href (:print (make-help-topic-url :name "Basic Syntax")))(:button "Previous Page")))
(:td :align "middle"  :width "26.7%"((:a :style "text-decoration:none" :href (:print (make-help-topic-url :name "HELP for BBL users"))) (:button "Help for BBL Users")))
(:td :align "right" :width "26.7%"((:a :style "text-decoration:none" :href (:print (make-help-topic-url :name "How to Reference")))(:button "Next Page")))))
)) 