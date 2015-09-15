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

;; ================= EXAMPLES RELATED TO SEARCHES - MENU ==================

(def-topic "Examples related to searching for queries"

(:text

(:p ((:a :style "color:#0000EE" :href (:print (make-help-topic-url :name "BioBIKE Examples"))) "BioBIKE Examples") " > "
((:a :style "color:#0000EE" :href (:print (make-help-topic-url :name "Examples related to searching for queries"))) "Examples related to searching for queries"))

(:table :align "middle" :width "80%"
(:tr
(:td (:small

(:ol :type "1"
(:li ((:a :style "color:#0000EE" :href (:print (make-help-topic-url :name "Examples related to sequences"))) (:b "Examples related to sequences")))
(:p) (:li ((:a :style "color:#0000EE" :href (:print (make-help-topic-url :name "Examples related to proteins and translation"))) (:b "Examples related to proteins and translation")))
(:p) (:li ((:a :style "color:#0000EE" :href (:print (make-help-topic-url :name "Examples related to orthologous relationships"))) (:b "Examples related to orthologous relationships")))
(:p) (:li ((:a :style "color:#0000EE" :href (:print (make-help-topic-url :name "Examples related to searching for queries"))) (:b (:font :color "red" "Examples related to searching for queries"))))

(:ul ; :ol :type "a"
(:p) (:li (:u "Find all instances of matches in a genome to a given degenerate consensus sequence") " (coming soon!)")
(:p) (:li (:u "Find all instances of matches to a given sequence, allowing for mismatches") " (coming soon!)")
(:p) (:li ((:a :style "color:#0000EE" :href (:print (MAKE-HELP-TOPIC-URL :name "Find all instances of an amino acid motif in proteins of a given organism" ))) "Find all instances of an amino acid motif in proteins of a given organism" ))
(:p) (:li (:u "Find candidate protein-binding sites in a genome, given an alignment of known sites") " (coming soon!)")
(:p) (:li (:u "Find the full name of a gene, given a partial name") " (coming soon!)")
(:p) (:li ((:a :style "color:#0000EE" :href (:print (MAKE-HELP-TOPIC-URL :name "Find desired genes using a keyword search of gene descriptions"))) "Find desired genes using a keyword search of gene descriptions"))
(:p) (:li (:u "Find all genes within a given metabolic category") " (coming soon!)")
(:p) (:li (:u "Blast a sequence against a database") " (coming soon!)"))

(:p) (:li ((:a :style "color:#0000EE" :href (:print (make-help-topic-url :name "Examples related to microarray data"))) (:b "Examples related to microarray data"))))
))))
(:p)
(:table :align "middle" :width "80%"
(:tr (:td :width "26.7%" :align "left" ((:a :style "text-decoration:none" :href (:print (make-help-topic-url :name "Find all genes with orthologs in all cyanobacteria, and export the set to Excel")))(:button " Previous Page ")))
(:td :width "26.7%" :align "middle" ((:a :style "text-decoration:none" :href (:print (make-help-topic-url :name "Help for BBL Users")))(:button "Help for BBL Users")))
(:td :align "right" :width "26.7%"((:a :style "text-decoration:none" :href (:print (make-help-topic-url :name "Find all instances of an amino acid motif in proteins of a given organism")))(:button " Next Page "))))

)))

;; ================= EXAMPLES RELATED TO SEARCHES - MENU ==================

;; ====== Find all instances of an amino acid motif in proteins of a given organism ======

(def-topic "Find all instances of an amino acid motif in proteins of a given organism"
(:text

(:p ((:a :style "color:#0000EE" :href (:print (make-help-topic-url :name "BioBIKE Examples"))) "BioBIKE Examples") " > "
((:a :style "color:#0000EE" :href (:print (make-help-topic-url :name "Examples related to searching for queries"))) "Examples related to searching for queries") " > "
((:a :style "color:#0000EE" :href (:print (MAKE-HELP-TOPIC-URL :name "Find all instances of an amino acid motif in proteins of a given organism" ))) "Find all instances of an amino acid motif in proteins of a given organism" ))

(:table :width "80%" :align "middle"
(:tr
(:td (:small
(:p "You want to find all DNA methyltransferases in an Anabaena PCC 7120. Of course, you could search the annotation, but unless an expert on such enzymes participated in the annotation, it is doubtful that this approach will be very effective. Alternatively, you could blast the organism's proteins with sequences of proven DNA methyltransferase sequences, but some desired protein will be missed.")
(:p "What operationally defines this class of protein are conserved motifs at key sites. For example, one subclass (group gamma) of DNA methyltransferases possesses these two motifs, known from protein crystal structure to be involved in either catalysis or the binding of S-adenosyl methionine:"))
(:pre " NPP[YFW] (read : NPPY or NPPF or NPPW)
L[ED][PA]\*\*[GA]*G" )

(:small(:p "Searching for each of these amongst Anabaena protein :"))

(:pre (:img :src "/weblistenerdocs/img-bbldoc/ex-aa-motifs.jpg"))
(:pre (:img :src "/weblistenerdocs/img-bbldoc/ex-aa-motifs-2.jpg"))
(:pre (:img :src "/weblistenerdocs/img-bbldoc/ex-aa-motifs-3.jpg"))

(:small
(:ul (:i "Translation : Each protein of Anabaena PCC 7120 is considered and the sequence is extracted. When a pattern match is found between the motif and the sequence\, the gene corresponding to the protein is added to the set. The protein name is extracted by dragging the results into the FIRST function, and lists are created using these proteins."))
(:p "Combining the two sets :"))

(:pre (:img :src "/weblistenerdocs/img-bbldoc/ex-aa-motifs-4.jpg"))

(:small(:p "Upon further examination\, these two proteins bare many signs of being DNA methyltransferases\, class gamma\, but sufficiently far from the norm to have escaped detection by Blast. What of the other genes that contained the NPPY motif?" ) )
(:pre (:img :src "/weblistenerdocs/img-bbldoc/ex-aa-motifs-5.jpg"))
(:small(:p"They're mostly DNA methyltransferases of other classes.")
(:p "Searching for motif patterns appears to be an effective way of finding genes too disimilar to other proteins of its class to be picked up by Blast.")))))
(:P)
(:table :align "middle" :width "80%" (:b
(:tr (:td :width "26.7%" :align "left" ((:a :style "text-decoration:none" :href (:print (make-help-topic-url :name "Examples related to searching for queries")))(:button " Previous Page ")))
(:td :width "26.7%":align "middle" ((:a :style "text-decoration:none" :href (:print (make-help-topic-url :name "Help for BBL Users")))(:button "Help for BBL Users")))
(:td :align "right" :width "26.7%"((:a :style "text-decoration:none" :href (:print (make-help-topic-url :name "Find desired genes using a keyword search of gene descriptions")))(:button " Next Page "))))

))
))

;; ====== Find desired genes using a keyword search of gene descriptions ======

(def-topic "Find desired genes using a keyword search of gene descriptions"
(:text

(:p ((:a :style "color:#0000EE" :href (:print (make-help-topic-url :name "BioBIKE Examples"))) "BioBIKE Examples") " > "
((:a :style "color:#0000EE" :href (:print (make-help-topic-url :name "Examples related to searching for queries"))) "Examples related to searching for queries") " > "
((:a :style "color:#0000EE" :href (:print (MAKE-HELP-TOPIC-URL :name "Find desired genes using a keyword search of gene descriptions"))) "Find desired genes using a keyword search of gene descriptions"))

(:table :width "80%" :align "middle"
(:tr
(:td (:small
(:p "You want to find the gene encoding cyanophycin synthetase in Trichodesmium erythraeum (nicknamed Tery). The straightforward approach : " ))
(:pre " ("((:a :href (:print (make-help-function-documentation-url :name "GENE-DESCRIBED-BY" :package "bbi" ))) "GENE-DESCRIBED-BY")" \"cyanophycin synth\" IN Tery)")
(:pre " -->NIL")(:small
(:p " produces the highly dubious result that Trichodesmium does not possess the enzyme. More likely\, the annotation of the genome is faulty -- all too common. (\"synth\" is used so that it will match \"synthetase\" \"synthase\" or \"synthesis\")")
(:p "You need another strategy. If some cyanophycin synthetase is correctly annotated\, you can use that gene to Blast the set of Trichodesmuim proteins. So\, you look for cyanophycin synthetase in any cyanobacterium."))

(:pre " *all-organisms*")
(:pre " -->#$pro1375 #$trichodesmium_erythraeum #$anabaena_variabilis_atcc29413
#$anabaena_pcc7120 #$nostoc_punctiforme_atcc29133
#$synechocystis_pcc6803")
(:pre " ("((:a :href (:print (make-help-function-documentation-url :name "ASSIGN" :package "bbi" ))) "ASSIGN")" cph-annotated (" ((:a :href (:print (make-help-function-documentation-url :name "GENE-DESCRIBED-BY" :package "bbi"))) "GENE-DESCRIBED-BY") " \"cyanophycin synth\"))")
(:pre " -->(NIL NIL (#$A29413.Av?2455 #$A29413.Av?2424 #$A29413.Av?3038)
(#$A7120.alr0573 #$A7120.alr1779 #$A7120.all3879)
(#$Npun.NpR1819 #$Npun.NpR2361 #$Npun.NpR5823)
(#$S6803.slr2002))")
(:small
(:ul (:i "Translation: At the time of writing\, BioBIKE knows of the six listed cyanobacteria. The set\, cph-annotated\, is defined as those genes with \"cyanophycin synth\" in the annotation. The set is subdivided by organism\, covering all organisms known to BioBIKE (the default procedure when GENE-DESCRIBED-BY is invoked without a specified organism). The first two organisms\, Prochlorococcus SS120 and Trichodesmium erythraeum\, do not have any such genes -- is this likely?"))
(:p "From no gene to too many! Which one has biochemical evidence supporting its annotation? That one would be the logical choice to Blast Trichodesmium. In a better world than ours\, experimental evidence would be part of the annotation record. Your present recourse is to find from amongst the present candidates the protein of Anabaena variabilis studied and partially sequenced by Ziegler et al (1998). From that sequence you take the amino acid sequence \"RILKIQTL\":"))

(:pre " (FOR-EACH protein IN (" ((:a :href (:print (make-help-function-documentation-url :name "PROTEINS-OF" :package "bbi")))"PROTEINS-OF") " Avar)
AS temp = (DISPLAY-LINE protein)
AS sequence = ("((:a :href (:print (make-help-function-documentation-url :name "SEQUENCE-OF" :package "bbi" ))) "SEQUENCE-OF")" protein)
WHEN (EXISTS ("((:a :href (:print (make-help-function-documentation-url :name "POSITION-OF" :package "bbi")))"POSITION-OF")" \"RILKIQTL\" IN sequence))
COLLECT protein)")

(:pre" -->#$A29413.p-Av?6746
#$A29413.p-Av?6747
#$A29413.p-Av?6748
#$A29413.p-Av?6749
#$A29413.p-Av?6750")(:small

(:ul (:i "Translation: Consider each protein of Anabaena variabilis (nicknamed Avar) and extract its sequence. When the search within that sequence for the oligopeptide identified by Ziegler et al is successful\, save that protein. Only one protein comes out of this search\, likely to be the true cyanophycin synthetase."))
(:p "You gather the orthologs of this protein:"))

(:pre " ("((:a :href (:print (make-help-function-documentation-url :name "ASSIGN" :package "bbi" ))) "ASSIGN")" (" ((:a :href (:print (make-help-function-documentation-url :name "COMMON-ORTHOLOGS-OF" :package "bbi")))"COMMON-ORTHOLOGS-OF")" p-Av?3038))" )
(:pre" -->(#$Tery.p-Te?4231 #$A29413.p-Av?3038 #$A7120.p-All3879 #$Npun.p-NpR5823
#$S6803.p-Slr2002)")
(:small
(:P "noting with satisfaction that there is a Trichodesmium protein in their number (though there is no such protein from SS120). However\, its annotation is:"))
(:pre " (" ((:a :href (:print (make-help-function-documentation-url :name "DESCRIPTION-OF" :package "bbi")))"DESCRIPTION-OF") " p-Te?4231)")

(:pre " -->\"COG1181: D-alanine-D-alanine ligase and related ATP-grasp enzymes\.\.\.\" ")
(:small
(:p "So who's right -- the identity suggested by the ortholog table or the original annotation? Is the Trichodesmium protein as close in sequence to the true cyanophycin synthetase as other proteins annotated \"cyanophycin synthetase\"? To address this question\, you combine the set of orthologs with the set of genes identified by annotation and produce a sequence alignment: ") )
(:pre " (UNION-OF cph-orthologs (PROTEINS-NAMED (" ((:a :href (:print (make-help-function-documentation-url :name "FLATTEN" :package "bbi")))"FLATTEN")" cph-annotated)))")
(:pre " -->(#$Tery.p-Te?4231 #$A29413.p-Av?2455 #$A29413.p-Av?2424
#$A29413.p-Av?3038 #$A7120.p-Alr0573 #$A7120.p-Alr1779
#$A7120.p-All3879 #$Npun.p-NpR1819 #$Npun.p-NpR2361 #$Npun.p-NpR5823
#$S6803.p-Slr2002)")
(:pre " (ALIGNMENT-OF *)" )
(:pre " -->"((:a :target "blank" :href (:print (make-help-topic-url :name "results11")))"RESULTS"))

(:small(:ul (:i "Translation: The set cph-proteins is defined as the union of the previously two defined sets -- the one containing orthologs of cyanophycin synthetase and the one containing genes identified by annotation. There are two complications\, however. First\, one set contains proteins while the other contains genes\, so the gene designations are converted into protein designations. Second\, cph-annotated is a complex list (parentheses within parentheses) which must be flattened to a simple list. The asterisk refers to the previous result\, i.e. the set of proteins to be aligned."))
(:p "The alignment clearly shows three families of sequences. The middle group includes the true cyanophycin sequence\.\.\. and the sequence from Trichodesmium. On this and other evidence, you conclude that the annotation is wrong and that you've found the gene encoding cyanophycin synthetase.")))))
(:P)

(:table :align "middle" :width "80%" (:b
(:tr (:td :width "26.7%" :align "left" ((:a :style "text-decoration:none" :href (:print (make-help-topic-url :name "Find all instances of an amino acid motif in proteins of a given organism")))(:button " Previous Page ")))
(:td :width "26.7%":align "middle" ((:a :style "text-decoration:none" :href (:print (make-help-topic-url :name "Help for BBL Users")))(:button "Help for BBL Users")))
(:td :align "right" :width "26.7%"((:a :style "text-decoration:none" :href (:print (make-help-topic-url :name "Examples related to microarray data")))(:button " Next Page "))))

))
))
