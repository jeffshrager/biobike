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
;;;            Rachel Walstead, Zalak Shah   

;; ================= EXAMPLES RELATED TO SEQUENCES - MENU ==================

(def-topic "Examples related to sequences"

(:text

(:p ((:a :style "color:#0000EE" :href (:print (make-help-topic-url :name "BioBIKE Examples"))) "BioBIKE Examples") " > "
((:a :style "color:#0000EE" :href (:print (make-help-topic-url :name "Examples related to sequences"))) "Examples related to sequences"))

(:table :align "middle" :width "80%"
(:tr
(:td  (:small

(:ol :type "1"
#|
(:li ((:a :style "color:#0000EE" :href (:print (make-help-topic-url :name "Examples related to arithmetic and statistics"))) (:b "Examples related to arithmetic and statistics"))) 
|#
(:p) (:li ((:a :style "color:#0000EE" :href (:print (make-help-topic-url :name "Examples related to sequences"))) (:b (:font :color "red" "Examples related to sequences"))))

(:ul ; :ol :type "a"
(:p) (:li ((:a :style "color:#0000EE" :href (:print (MAKE-HELP-TOPIC-URL :name "Display the DNA sequence of a gene" ))) "Display the DNA sequence of a gene" ))
(:p) (:li ((:a :style "color:#0000EE" :href (:print (MAKE-HELP-TOPIC-URL :name "Display the DNA sequence of a region" ))) "Display the DNA sequence of a region" ))
(:p) (:li ((:a :style "color:#0000EE" :href (:print (MAKE-HELP-TOPIC-URL :name "Display the amino acid sequence of a protein" ))) "Display the amino acid sequence of a protein" ))
(:p) (:li ((:a :style "color:#0000EE" :href (:print (MAKE-HELP-TOPIC-URL :name "Collect the upstream regions of from a set of coregulated genes" ))) "Collect the upstream regions
from a set of coregulated genes"))
(:p) (:li ((:a :style "color:#0000EE" :href (:print (MAKE-HELP-TOPIC-URL :name "Show the alignment of multiple proteins"))) "Show the alignment of multiple proteins" ))
(:p) (:li (:u "Show the alignment of multiple genes") " (coming soon!)")
(:p) (:li ((:a :style "color:#0000EE" :href (:print (MAKE-HELP-TOPIC-URL :name "What fraction of the genome lies within genes" ))) "What fraction of the genome lies within genes?" ))
(:p) (:li (:u "Predict the bands in a pulsed-field gel of the genome cut with a given enzyme") " (coming soon!)"))

(:p) (:li ((:a :style "color:#0000EE" :href (:print (make-help-topic-url :name "Examples related to proteins and translation"))) (:b "Examples related to proteins and translation")))
(:p) (:li ((:a :style "color:#0000EE" :href (:print (make-help-topic-url :name "Examples related to orthologous relationships"))) (:b "Examples related to orthologous relationships")))
(:p) (:li ((:a :style "color:#0000EE" :href (:print (make-help-topic-url :name "Examples related to searching for queries"))) (:b"Examples related to searching for queries")))
(:p) (:li ((:a :style "color:#0000EE" :href (:print (make-help-topic-url :name "Examples related to microarray data"))) (:b "Examples related to microarray data")))
)))))

(:p) 
(:table :align "middle" :width "80%"   (:b
(:tr  
(:td  :width "26.7%" :align "left" ((:a :style "text-decoration:none" :href (:print (make-help-topic-url :name "BioBIKE Examples")))(:button " Previous Page ")))
(:td :width "26.7%" :align "middle" ((:a :style "text-decoration:none" :href (:print (make-help-topic-url :name "Help for BBL Users")))(:button "Help for BBL Users")))
(:td :width "26.7%" :align "right" ((:a :style "text-decoration:none" :href (:print (make-help-topic-url :name "Display the DNA sequence of a gene")))(:button " Next Page "))) )
))
))

;; ================= EXAMPLES RELATED TO SEQUENCES ==================

;; ============= DNA sequence of a gene ==========

(def-topic "Display the DNA sequence of a gene"
(:text

(:p ((:a :style "color:#0000EE" :href (:print (make-help-topic-url :name "BioBIKE Examples"))) "BioBIKE Examples") " > "

((:a :style "color:#0000EE" :href (:print (make-help-topic-url :name "Examples related to sequences"))) "Examples related to sequences") " > "
((:a :style "color:#0000EE" :href (:print (MAKE-HELP-TOPIC-URL :name "Display the DNA sequence of a gene" ))) "Display the DNA sequence of a gene"))

(:table :width "80%" :align "middle"
(:tr
(:td (:small
(:p "Suppose you're interested in the gene all4312 from Anabaena PCC 7120.")
(:p "You'd like to see its nucleotide sequence and use that sequence for some purpose.")
(:p "Here's an easy way you can display the "
((:a :Target "blank" :href (:print (make-help-function-documentation-url :name "SEQUENCE-OF" :package "bbi" ))) "SEQUENCE-OF")
" the gene.")
((:a :Target "blank" :href (:print (make-help-function-documentation-url :name "SEQUENCE-OF" :package "bbi" )))
(:img :src "/weblistenerdocs/img-bbldoc/ex-sequence-of-all4312.jpg")) " "
(:img :src "/weblistenerdocs/img-bbldoc/zrightarrow.gif") " "
((:A :Target "blank" :Href "/weblistenerdocs/img-bbldoc/ex-sequence-of-all4312-result.html")
  (:img :src "/weblistenerdocs/img-bbldoc/ex-sequence-of-all4312-result-thumb.jpg"
        :border 1 )) ; :bordercolor doesn't work
(:p "The result should pop-up in a new window (click result icon to enlarge)")
))))

(:table :width "80%" :align "middle"
(:tr
(:td (:small
(:p "If you want the result in a way that you can directly copy and paste it into another application, here is a way you can do that. This is will give you the result in FastA format, in a new window."))
((:a :Target "blank" :href (:print (make-help-function-documentation-url :name "SEQUENCE-OF" :package "bbi" )))
  (:img :src "/weblistenerdocs/img-bbldoc/ex-sequence-of-all4312-fasta.jpg")) " "
(:img :src "/weblistenerdocs/img-bbldoc/zrightarrow.gif") " "
((:A :Target "blank" :Href "/weblistenerdocs/img-bbldoc/ex-sequence-of-all4312-fasta-result.htm")
  (:img :src "/weblistenerdocs/img-bbldoc/ex-sequence-of-all4312-fasta-result-thumb.jpg"
        :border 1 )) ; :bordercolor doesn't work

)))

(:table :align "middle" :width "80%" 
(:b
(:tr
(:td :align "left" 
((:a :style "text-decoration:none" :href (:print (make-help-topic-url :name "Examples related to sequences")))(:button " More examples "))
((:a :style "text-decoration:none" :href (:print (make-help-topic-url :name "Help for BBL Users")))(:button "Help"))
))))
))


;------ Amino Acid Sequence of a protein ---

(def-topic "Display the amino acid sequence of a protein"
(:text

(:p ((:a :style "color:#0000EE" :href (:print (make-help-topic-url :name "BioBIKE Examples"))) "BioBIKE Examples") " > "
((:a :style "color:#0000EE" :href (:print (make-help-topic-url :name "Examples related to sequences"))) "Examples related to sequences") " > "
((:a :style "color:#0000EE" :href (:print (MAKE-HELP-TOPIC-URL :name "Display the amino acid sequence of a protein" ))) "Display the amino acid sequence of a protein"))

(:table :width "80%" :align "middle"
(:tr
(:td (:small
(:p "This example works in CyanoBIKE and PhAnToMe/BioBIKE.")
(:p " ")
(:p "You're interested in the protein al4312 from Anabaena PCC 7120.")
(:p "You'd like to see its amino acid sequence and use that sequence for some purpose.")
(:p "First, determine the name of the protein in BioBIKE by using the PROTEIN-NAMED function."))
(:pre " (PROTEIN-NAMED all4312)")
(:pre (:img :src "/weblistenerdocs/img-bbldoc/ex-sequence-of-amino-acid-1.jpg"))
(:small
(:p "Then find the amino acid sequence of the protein by using the protein name in the SEQUENCE-OF fucntion."))
(:pre " (SEQUENCE-OF p-all4312)")
(:pre (:img :src "/weblistenerdocs/img-bbldoc/ex-sequence-of-amino-acid-2.jpg"))
)))
(:P)
(:table :align "middle" :width "80%" (:b
(:tr (:td :width "26.7%" :align "left" ((:a :style "text-decoration:none" :href (:print (make-help-topic-url :name "Display the DNA sequence of a region")))(:button " Previous Page ")))
(:td :width "26.7%":align "middle" ((:a :style "text-decoration:none" :href (:print (make-help-topic-url :name "Help for BBL Users")))(:button "Help for BBL Users")))
(:td :align "right" :width "26.7%"((:a :style "text-decoration:none" :href (:print (make-help-topic-url :name "Collect the upstream regions of from a set of coregulated genes")))(:button " Next Page "))))

))
))

;---- DNA Sequence of a region --- 
(def-topic "Display the DNA sequence of a region"
(:text

(:p ((:a :style "color:#0000EE" :href (:print (make-help-topic-url :name "BioBIKE Examples"))) "BioBIKE Examples") " > "
((:a :style "color:#0000EE" :href (:print (make-help-topic-url :name "Examples related to sequences"))) "Examples related to sequences") " > "
((:a :style "color:#0000EE" :href (:print (MAKE-HELP-TOPIC-URL :name "Display the DNA sequence of a region" ))) "Display the DNA sequence of a region"))

(:table :width "80%" :align "middle"
(:tr
(:td (:small
(:p "This example works in CyanoBIKE and PhAnToMe/BioBIKE.")
(:p " ")
(:p "You're interested in the region upstream from the gene all4312 from Anabaena PCC 7120. You'd like to see its sequence and use that sequence for some purpose.")
(:p "To see the sequence, use DISPLAY-SEQUENCE-OF and in the entity box, insert UPSTREAM-SEQUENCE/S-OF all4312"))
(:pre " (DISPLAY-SEQUENCE-OF ("((:a :href (:print (make-help-function-documentation-url :name "UPSTREAM-SEQUENCES-OF" :package "bbi" )))"UPSTREAM-SEQUENCES-OF")" all4312))")
(:pre (:img :src "/weblistenerdocs/img-bbldoc/ex-sequence-of-region.jpg"))
(:small (:p "To use the sequence in another fucntion, use the DEFINE function to create a variable. This variable will contain the sequence you need to use. After you define your first variable, a blue VARIABLES menu will appear in your pallete. You can retrieve all of your variables for this menu."))
(:pre " ("((:a :href (:print (make-help-function-documentation-url :name "DEFINE" :package "bbi" ))) "DEFINE" )" upstream-sequence ("((:a href (:print (make-help-function-documentation-url :name "UPSTREAM-SEQUENCES-OF" :package "bbi" ))) "UPSTREAM-SEQUENCES-OF") " all4312 ))")
(:pre (:img :src "/weblistenerdocs/img-bbldoc/ex-sequence-of-region-define.jpg")))))
(:p )
(:table :align "middle" :width "80%" (:b
(:tr
(:td :width "26.7%" :align "left" ((:a :style "text-decoration:none" :href (:print (make-help-topic-url :name "Display the DNA sequence of a gene")))(:button " Previous Page ")))

(:td :width "26.7%":align "middle"((:a :style "text-decoration:none" :href (:print (make-help-topic-url :name "Examples related to sequences")))(:button "Up one level")))
(:td :width "26.7%":align "right" ((:a :style "text-decoration:none" :href (:print (make-help-topic-url :name "Display the amino acid sequence of a protein")))(:button " Next Page "))))

))
))




;;=============Collect the upstream regions of from a set of coregulated genes==========

(def-topic "Collect the upstream regions of from a set of coregulated genes"
(:text

(:p ((:a :style "color:#0000EE" :href (:print (make-help-topic-url :name "BioBIKE Examples"))) "BioBIKE Examples")  " > " 
((:a :style "color:#0000EE" :href (:print (make-help-topic-url :name "Examples related to sequences")))  "Examples related to sequences") " > " 
((:a :style "color:#0000EE" :href (:print (MAKE-HELP-TOPIC-URL :name "Collect the upstream regions of from a set of coregulated genes"))) "Collect the upstream regions from a set of coregulated genes")) 

(:table :width "80%" :align "middle"
(:tr
(:td (:small

      (:p "You've collected a set of genes you think may be coregulated.")
    (:p "You'd like to see if you can identify a common sequence element upstream from these genes.")
    (:p "The first step is to collect these upstream sequences."))
    (:pre "     ("((:a :href (:print (make-help-function-documentation-url :name "UPSTREAM-SEQUENCES-OF" :package "bbi" ))) "UPSTREAM-SEQUENCES-OF") " {all4312 all4252 alr5333 all1424})")
(:pre "     -->(\"AACTGCTGATTTATGTCTTTGGAATGAGAATTGTAAATGATTCAGGCTGAAATGTGAAGGGTAATTTCTGCTTCAATTTCAGATGCAATTCCTTTT\.\.\.
        \"TAATTGTTAAGCTATTACGCTTTTAAATTACATAGTCAAAAGAGGCAAGGGAGCAAGGGGGATAAATTGAGACGGAAGCTCAGACTCCGCCCCAATTAA\.\.\.
        \"TAATTTGACTTATAGAGGAATATAAATTT\" \"ATAGGATTGTAACAGAG\")
        NIL\""))))
(:p)
(:table :align "middle" :width "80%" (:b  
   (:tr  (:td  :width "26.7%" :align "left" ((:a :style "text-decoration:none" :href (:print (make-help-topic-url :name "Display the amino acid sequence of a protein")))(:button " Previous Page ")))
(:td :width "26.7%":align "middle" ((:a :style "text-decoration:none" :href (:print (make-help-topic-url :name "Help for BBL Users")))(:button "Help for BBL Users")))
(:td :align "right" :width "26.7%"((:a :style "text-decoration:none" :href (:print (make-help-topic-url :name "Show the alignment of multiple proteins")))(:button " Next Page "))))

))
))

 ;;=============Show the alignment of multiple proteins==========

(def-topic "Show the alignment of multiple proteins"
(:text

(:p ((:a :style "color:#0000EE" :href (:print (make-help-topic-url :name "BioBIKE Examples"))) "BioBIKE Examples")  " > " 
((:a :style "color:#0000EE" :href (:print (make-help-topic-url :name "Examples related to sequences"))) "Examples related to sequences") " > " 
((:a :style "color:#0000EE" :href (:print (MAKE-HELP-TOPIC-URL :name "Show the alignment of multiple proteins"))) "Show the alignment of multiple proteins"))

(:table   :align "middle" :width "80%"
(:tr
(:td (:small
(:p (:b "Problem"))
(:p "You've found a gene (Or4231) in Trichodesmium that you think might be its cyanophycin synthetase,\.\.\. at least BLAST seems to think so, but it's annotated as D-alanine-D-alanine ligase. You've found another in Anabaena PCC 7120 (alr1779) annotated as \"similar to cyanophycin synthetase\". How can you tell who's right?")

(:p (:b "Strategy"))
(:p "You gather together all orthologs of a proven cyanophycin synthetase, add Trichdesmium's candidate protein to the mix, and align all of them. If the Trichodesmium protein has the regions conserved amongst cyanophycin synthetases in general, you'll declare success.")

(:p (:b "Step1: Find a proven cyanophycin synthetase"))
(:p " Ziegler et al [(1988) Eur J Biochem 254:154-159] reported a partial amino acid sequence of cyanophycin synthetase from Anabaena variabilis and demonstrated that the protein indeed had synthetase activity. Which gene encodes the protein?")
(:p "You could search the annotation of Anabaena variabilis for \"cyanophycin synthetase\" or \"cph\" (the name of the gene), but who's to say whether the automated annotation is accurate? Better to search directly for the proven amino acid sequence.")) 
	(:pre"     (FOR-EACH protein IN (("((:a :href (:print (make-help-function-documentation-url :name "PROTEINS-OF" :package "bbi"))) "PROTEINS-OF") "  Avar)
          AS sequence = ("((:a :href (:print (make-help-function-documentation-url :name "SEQUENCE-OF" :package "bbi"))) "SEQUENCE-OF") "  protein)
        WHEN (SUCCESSFUL (PATTERN-MATCH \"RILKIQTL\" sequence))
       COLLECT protein)" )
    (:pre "     --> (#$a29413.p-Av?3038)")
    (:small (:ul (:i "Translation: Consider each protein in the set of all proteins encoded by Anabaena variabilis (Avar is one of its several official nicknames). Take the sequence of each protein and search it for part of the sequence reported by Ziegler et al. When an exact match is found, save the name of that protein. In fact, only one matching protein was found (in about 8 seconds)."))
    (:p (:b "Step 2: Find orthologs of Cyanophycin synthetase p-Av?3038a, add candidates to the list, and align them")))
    (:pre "     (" ((:a :href (:print (make-help-function-documentation-url :name "DEFINE" :package "bbi")))"DEFINE") " cph-orthologs (ALL-ORTHOLOGS-OF p-av?3038))") 
    (:pre  "      -->(#$Tery.p-Te?4231 #$a29413.p-Av?3038 #$A7120.p-All3879
      #$Npun.p-NpR5823 #$S6803.p-Slr2002)")
    
	(:pre "     (ADD-TO-SET cph-orthologs (PROTEIN-NAMED alr1779))") 
      (:pre "     -->(#$Tery.p-Te?4231 #$a29413.p-Av?3038 #$A7120.p-All3879
     #$Npun.p-NpR582 #$S6803.p-Slr2002 #$A7120.p-Alr1779) ")
(:small(:ul (:i "Translation: Take the orthologs of p-Av?3038 in all available cyanobacteria and store them in the variable called cph-orthologs. I note that the Trichodesmium protein is already part of this set, but I do have to add the Anabaena protein encoded by alr1779, which I do.")))
	(:pre "     (ALLIGNMET-OF *)")
	(:pre "     -->"((:a :target "blank" :href (:print (make-help-topic-url :name "results1")))"RESULTS"))  
 
(:small(:ul (:i "Translation: The asterisk indicates that what should be aligned is the result of the previous operation (i.e. adding the sixth sequence to the set)."))

(:p (:b "Conclusion"))
(:p "The Trichodesmium protein looks every inch a cyanophycin synthase (annotation be damned), while the added Anabaena sequence is a very poor specimen, similar to only one small portion of the protein. ") )
)))

(:table :width "80%" :align "middle" (:b   
   (:tr  (:td  :width "26.7%" :align "left" ((:a :style "text-decoration:none" :href (:print (make-help-topic-url :name "Collect the upstream regions of from a set of coregulated genes")))(:button " Previous Page ")))
   (:td :width "26.7%":align "middle" ((:a :style "text-decoration:none" :href (:print (make-help-topic-url :name "Help for BBL Users")))(:button "Help for BBL Users")))
   (:td :align "right" :width "26.7%"((:a :style "text-decoration:none" :href (:print (make-help-topic-url :name "What fraction of the genome lies within genes")))(:button " Next Page "))) )

))
)) 


  ;;=============What fraction of the genome lies within genes?==========

(def-topic "What fraction of the genome lies within genes"
(:text

(:p ((:a :style "color:#0000EE" :href (:print (make-help-topic-url :name "BioBIKE Examples"))) "BioBIKE Examples")  " > " 
((:a :style "color:#0000EE" :href (:print (make-help-topic-url :name "Examples related to sequences"))) "Examples related to sequences") " > " 
((:a :style "color:#0000EE" :href (:print (MAKE-HELP-TOPIC-URL :name "What fraction of the genome lies within genes"))) "What fraction of the genome lies within genes?"))

(:table :align "middle" :width "80%"
(:tr
(:td (:small
(:p "Only 3% of the human genome lies within genes. What's the story with cyanobacterial genomes?")
(:p "Take, for example, the genome of Synechocystis PCC 6803."))
	(:pre "     ("((:a :href (:print (make-help-function-documentation-url :name "DEFINE" :package "bbi" ))) "DEFINE") " gene-length-sum ("((:a :href (:print (make-help-function-documentation-url :name "SUM-OF" :package "bbi" ))) "SUM-OF") " ("((:a :href (:print (make-help-function-documentation-url :name "LENGTHS-OF" :package "bbi" ))) "LENGTHS-OF") " ("((:a :href (:print (make-help-function-documentation-url :name "GENES-OF" :package "bbi" ))) "GENES-OF") " S6803)))" )
      (:pre "     --> 3453575")
      (:pre "     (/ gene-length-sum ("((:a :href (:print (make-help-function-documentation-url :name "LENGTH-OF" :package "bbi" ))) "LENGTH-OF") " S6803) 1.0)")
      (:pre "     --> 0.8727858")
        
(:small(:p (:i "The first statement finds the sum of the lengths of all genes in Synechocystis PCC6803. The second divides that sum by the full length of the Synechocystis genome (and by 1.0 to force the result to be a decimal fraction)."))
(:p " Or, shorter but perhaps less intelligibly\.\.\." ))
     (:pre "     (/ ( " ((:a :href (:print (make-help-function-documentation-url :name "SUM-OF" :package "bbi" ))) "SUM-OF") " (" ((:a :href (:print (make-help-function-documentation-url :name "LENGTHS-OF" :package "bbi" ))) "LENGTHS-OF") " ("((:a :href (:print (make-help-function-documentation-url :name "GENES-OF" :package "bbi" ))) "GENES-OF")" S6803))))" ) 
     (:pre "     ("((:a :href (:print (make-help-function-documentation-url :name "LENGTH-OF" :package "bbi" ))) "LENGTH-OF") " S6803 ) 1.0 )")
     (:pre"     -->0.8727858 ")
)))
(:P)
(:table :align "middle" :width "80%" (:b 
   (:tr  (:td  :width "26.7%" :align "left" ((:a :style "text-decoration:none" :href (:print (make-help-topic-url :name "Show the alignment of multiple proteins")))(:button " Previous Page ")))
   (:td :width "26.7%":align "middle" ((:a :style "text-decoration:none" :href (:print (make-help-topic-url :name "Help for BBL Users")))(:button "Help for BBL Users"))) 
   (:td :align "right" :width "26.7%"((:a :style "text-decoration:none" :href (:print (make-help-topic-url :name "Examples related to proteins and translation")))(:button " Next Page "))) )

))
))

