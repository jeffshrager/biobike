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
;;;            Rachel Walstead

;; ================= EXAMPLES RELATED TO PROTEINS AND TRANSLATION - MENU ==================

(def-topic "Examples related to proteins and translation"
(:text

(:p ((:a :style "color:#0000EE" :href (:print (make-help-topic-url :name "BioBIKE Examples"))) "BioBIKE Examples") " > "
((:a :style "color:#0000EE" :href (:print (make-help-topic-url :name "Examples related to proteins and translation"))) "Examples related to proteins and translation"))

(:table :width "80%" :align "middle"
(:tr
(:td (:small

(:ol :type "1"
(:li ((:a :style "color:#0000EE" :href (:print (make-help-topic-url :name "Examples related to sequences"))) (:b "Examples related to sequences")))
(:p) (:li ((:a :style "color:#0000EE" :href (:print (make-help-topic-url :name "Examples related to proteins and translation"))) (:b (:font :color "red" "Examples related to proteins and translation"))))

(:ul ; :ol :type "a"
(:p) (:li ((:a :style "color:#0000EE" :href (:print (MAKE-HELP-TOPIC-URL :name "Predict the molecular weight of a given protein"))) "Predict the molecular weight of a given protein"))
(:p) (:li (:u "Find all proteins in your favorite organism with a given approximate molecular weight") " (coming soon!)")
(:p) (:li (:u "Translate a DNA sequence in all six reading frames")" (coming soon!)")
(:p) (:li (:u "Translation of upstream sequences to check the annotation of a gene's start site")" (coming soon!)"))

(:p) (:li ((:a :style "color:#0000EE" :href (:print (make-help-topic-url :name "Examples related to orthologous relationships"))) (:b "Examples related to orthologous relationships")))
(:p) (:li ((:a :style "color:#0000EE" :href (:print (make-help-topic-url :name "Examples related to searching for queries"))) (:b"Examples related to searching for queries")))
(:p) (:li ((:a :style "color:#0000EE" :href (:print (make-help-topic-url :name "Examples related to microarray data"))) (:b "Examples related to microarray data")))

)))))

(:p)
(:table :align "middle" :width "80%" (:b
(:tr
(:td :width "26.7%" :align "left" ((:a :style "text-decoration:none" :href (:print (make-help-topic-url :name "What fraction of the genome lies within genes")))(:button " Previous Page ")))
(:td :width "26.7%" :align "middle" ((:a :style "text-decoration:none" :href (:print (make-help-topic-url :name "Help for BBL Users")))(:button "Help for BBL Users")))
(:td :align "right" :width "26.7%"((:a :style "text-decoration:none" :href (:print (make-help-topic-url :name "Predict the molecular weight of a given protein")))(:button " Next Page "))))
))
))

;; ================= EXAMPLES RELATED TO PROTEINS AND TRANSLATION ==================


;;================= Predict the molecular weight of a given protein ====================

(def-topic "Predict the molecular weight of a given protein"
(:text

(:p ((:a :style "color:#0000EE" :href (:print (make-help-topic-url :name "BioBIKE Examples"))) "BioBIKE Examples") " > "
((:a :style "color:#0000EE" :href (:print (make-help-topic-url :name "Examples related to proteins and translation"))) "Examples related to proteins and translation") " > "
((:a :style "color:#0000EE" :href (:print (MAKE-HELP-TOPIC-URL :name "Predict the molecular weight of a given protein"))) "Predict the molecular weight of a given protein"))

(:table :width "80%" :align "middle"
(:tr
(:td (:small
(:B (:I "(Example designed for CyanoBIKE)"))
(:p "The predicted molecular weight of a single protein is given in a straightforward fashion:"))
(:pre (:img :src "/weblistenerdocs/img-bbldoc/ex-molecular-weight.jpg"))
(:small  "And the predicted molecular weights of a list of proteins is similarly straightforward:")

(:pre (:img :src "/weblistenerdocs/img-bbldoc/ex-molecular-weight-2.jpg"))

(:small(:ul(:i "Translation: The "
((:a :href (:print (make-help-function-documentation-url :name "MW-OF" :package "bbi" ))) "MW-OF")
" function figures that you don't really want the molecular weight of the gene so instead provides the molecular weight of the corresponding protein. Like most BioBIKE functions, "
((:a :href (:print (make-help-function-documentation-url :name "MW-OF" :package "bbi" ))) "MW-OF")
" acts on sets of proteins as well as individual proteins. The second statement provides the molecular weights of all cyanobacterial "
((:a :href (:print (make-help-function-documentation-url :name "ORTHOLOG/S-OF" :package "bbi" ))) "ORTHOLOGS-OF")
" alr0346 available to BioBIKE."))

(:p "p-Alr0346 appears to have a much smaller molecular weight than all of its orthologs... or does it? You suspect that the annotation may be an error, and therefore look for alternative start sites for the gene (See "
((:a :href (:print (make-help-function-documentation-url :name "Translation of upstream sequence to check a gene's start site" :package "bbi" ))) "Translation of upstream sequence to check a gene's start site")
" to see how this might be done). You find one 108 amino acids upstream from the annotated start site. Using this site, you obtain the predicted molecular weight like so:"))

((:img :src "/weblistenerdocs/img-bbldoc/ex-molecular-weight-4.png"))
(:BR)
((:img :src "/weblistenerdocs/img-bbldoc/ex-molecular-weight-6.png"))
(:BR)
((:img :src "/weblistenerdocs/img-bbldoc/ex-molecular-weight-6-result.png"))

(:small (:ul (:i "Translation: A variable is "
((:a :href (:print (make-help-function-documentation-url :name "DEFINE" :package "bbi" ))) "DEFINE")
"d to contain the "
((:a :href (:print (make-help-function-documentation-url :name "SEQUENCE-OF" :package "bbi" ))) "SEQUENCE-OF")
" the protein, starting 108 amino acids upstream from the putative beginning. The name of the protein differs from the name of the gene by a 'p-' prefix. The "
((:a :href (:print (make-help-function-documentation-url :name "MW-OF" :package "bbi" ))) "MW-OF")
" function next returns the molecular weight of this newly defined protein."))

(:p "It's certainly peculiar that such a large stretch in the upstream region lacks a stop codon and that the predicted molecular weight of the alternate protein matches the the molecular weights of the orthologs. Maybe the current annotation of alr0346 is wrong. To help assess this possibility, you compare the orthologs of the gene with each other and with the alternate version:")

((:img :src "/weblistenerdocs/img-bbldoc/ex-molecular-weight-5.png")) " "
(:img :src "/weblistenerdocs/img-bbldoc/zrightarrow.gif") " "
((:A :Target "blank" :Href "/weblistenerdocs/img-bbldoc/ex-molecular-weight-predict-result.txt")
  (:img :src "/weblistenerdocs/img-bbldoc/ex-molecular-weight-alignment-result-thumb.jpg"
        :border 1 ))
(:BR)
"(Click result icon to expand result)")

(:small(:ul (:i "Translation: The "
((:a :href (:print (make-help-function-documentation-url :name "ADD-SET" :package "bbi" ))) "ADD-SET")
" function combines the set of "
((:a :href (:print (make-help-function-documentation-url :name "ORTHOLOG/S-OF" :package "bbi"))) "ORTHOLOGS-OF")
" p-Alr0346 (including p-Alr0346 itself) with the alternative version of p-Alr0346 previously defined. An "
((:a :href (:print (make-help-function-documentation-url :name "ALIGNMENT-OF" :package "bbi" ))) "ALIGNMENT-OF")
" this set is then produced."))

(:p "The amino acid sequence of the alternate version of p-Alr0346 matches well the sequences of the orthologs. It is evident that the annotated start site of the gene is not correct. A billion years of conserved sequence can't be wrong.")
))))
(:P)

(:table :align "middle" :width "80%" 
(:b
(:tr 
(:td  :width "26.7%" :align "left" 
  ((:a :style "text-decoration:none" :href (:print (make-help-topic-url :name "Examples related to proteins and translation")))
   (:button " More translation examples ")))
(:td :width "26.7%":align "middle"   
  ((:a :style "text-decoration:none" :href (:print (make-help-topic-url :name "BioBIKE Examples")))
   (:button " More examples ")))
(:td :align "right" :width "26.7%"
  ((:a :style "text-decoration:none" :href (:print (make-help-topic-url :name "Help for BBL Users")))
   (:button "Help"))))
))  
))

