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

;; ================= EXAMPLES RELATED TO ORTHOLOGOUS RELATIONSHIPS - MENU ==================

(def-topic "Examples related to orthologous relationships"
(:text

(:p ((:a :style "color:#0000EE" :href (:print (make-help-topic-url :name "BioBIKE Examples"))) "BioBIKE Examples") " > "
((:a :style "color:#0000EE" :href (:print (make-help-topic-url :name "Examples related to orthologous relationships"))) "Examples related to orthologous relationships"))

(:table :align "middle" :width "80%"
(:tr
(:td (:small

(:ol :type "1"
(:li ((:a :style "color:#0000EE" :href (:print (make-help-topic-url :name "Examples related to sequences"))) (:b "Examples related to sequences")))
(:p) (:li ((:a :style "color:#0000EE" :href (:print (make-help-topic-url :name "Examples related to proteins and translation"))) (:b "Examples related to proteins and translation")))
(:p) (:li ((:a :style "color:#0000EE" :href (:print (make-help-topic-url :name "Examples related to orthologous relationships"))) (:b (:font :color "red" "Examples related to orthologous relationships"))))

(:ul ; :ol :type "a"
(:p) (:li ((:a :style "color:#0000EE" :href (:print (MAKE-HELP-TOPIC-URL :name "Get all available orthologs of your favorite gene and align them" ))) "Get all available orthologs of your favorite gene and align them"))
(:p) (:li ((:a :style "color:#0000EE" :href (:print (MAKE-HELP-TOPIC-URL :name "Find all genes in organism A that lack orthologs in organism B"))) "Find all genes in organism A that lack orthologs in organism B
"))
(:p) (:li ((:a :style "color:#0000EE" :href (:print (MAKE-HELP-TOPIC-URL :name "Find all genes in all heterocystous cyanobacteria that are lacking in all other cyanobacteria" ))) "Find all genes in all heterocystous cyanobacteria that are lacking in all other cyanobacteria" ))
(:p) (:li ((:a :style "color:#0000EE" :href (:print (MAKE-HELP-TOPIC-URL :name "Find all genes with orthologs in all cyanobacteria, and export the set to Excel"))) "Find all genes with orthologs in all cyanobacteria, and export the set to Excel")))

(:p) (:li ((:a :style "color:#0000EE" :href (:print (make-help-topic-url :name "Examples related to searching for queries"))) (:b"Examples related to searching for queries")))
(:p) (:li ((:a :style "color:#0000EE" :href (:print (make-help-topic-url :name "Examples related to microarray data"))) (:b "Examples related to microarray data"))))

(:p (:i "Two proteins are considered to be orthologs if:"))
(:ul
(:p) (:li (:i "Protein 1 in organism A is the best Blast hit when the Protein 2 in Organism B is used as the query"))
(:p) (:li (:i "Protein 2 in organism B is the best Blast hit when the Protein 1 in Organism A is used as the query"))
(:p) (:li (:i "The E-value for the hit is better than 1e-10")))))))

(:p)
(:table :align "middle" :width "80%"
(:tr
(:td :width "26.7%" :align "left" ((:a :style "text-decoration:none" :href (:print (make-help-topic-url :name "Predict the molecular weight of a given protein")))(:button " Previous Page ")))
(:td :width "26.7%" :align "middle" ((:a :style "text-decoration:none" :href (:print (make-help-topic-url :name "Help for BBL Users")))(:button "Help for BBL Users")))
(:td :align "right" :width "26.7%"((:a :style "text-decoration:none" :href (:print (make-help-topic-url :name "Get all available orthologs of your favorite gene and align them")))(:button " Next Page "))))

)))


;; ================= EXAMPLES RELATED TO ORTHOLOGOUS RELATIONSHIPS ==================

;; ====== Get all available orthologs of your favorite gene and align them ======

(def-topic "Get all available orthologs of your favorite gene and align them"
(:text

(:p ((:a :style "color:#0000EE" :href (:print (make-help-topic-url :name "BioBIKE Examples"))) "BioBIKE Examples") " > "
((:a :style "color:#0000EE" :href (:print (make-help-topic-url :name "Examples related to orthologous relationships"))) "Examples related to orthologous relationships") " > "
((:a :style "color:#0000EE" :href (:print (MAKE-HELP-TOPIC-URL :name "Get all available orthologs of your favorite gene and align them" ))) "Get all available orthologs of your favorite gene and align them"))

(:table :width "80%" :align "middle"
(:tr
(:td (:small
(:p "This example works in CyanoBIKE.")
(:p " ")
(:p(:b "Problem"))
(:p "Let’s say you’ve found a gene (tery_19665)  in Trichodesmium that you think is a cyanophycin synthetase… or at least BLAST seems to think so. But what if it has been annotated as D-alanine-D-alanine ligase. You’ve found another gene in Anabaena PCC 7120 (alr1779) annotated as “similar to cyanophycin synthetase”. How can you tell who is right?")
(:p (:b "Strategy"))
(:p "You gather together all orthologs of a proven cyanophycin synthetase, add Trichdesmium's candidate protein to the mix, and align all of them. If the Trichodesmium protein has the regions conserved amongst cyanophycin synthetases in general, you'll declare success.")
(:p (:b "Step 1: Find a cyanophycin synthetase"))
(:p "Ziegler et al [(1988) Eur J Biochem 254:154-159] reported a partial amino acid sequence of cyanophycin synthetase from Anabaena variabilis and demonstrated that the protein indeed had synthetase activity.")
(:p "Since you know the proven amino acid sequence in Anabaena variabilis, why not see if you can find that sequence in Anabaena PCC 7120?"))

(:pre (:img :src "/weblistenerdocs/img-bbldoc/ex-orthologs-favorite.jpg"))


(:small(:ul (:i "Translation : Consider each protein in the set of all proteins encoded by Anabaena PCC 7120 (a7120 is one of its several official nicknames). Take the sequence of each protein and search it for part of the sequence reported by Ziegler et al. When an exact match is found, save the name of that protein. In fact, only one matching protein was found (in about 8 seconds). "))
(:p (:b "Step 2: Find orthologs of Cyanophycin synthetase p-All3879, add candidates to the list, and align them")))
(:pre (:img :src "/weblistenerdocs/img-bbldoc/ex-orthologs-favorite-2.jpg"))


(:small (:ul (:i "Take the orthologs of p-All3879 in all available cyanobacteria and store them in the variable called cph-orthologs. I note that the Trichodesmium protein is already part of this set, but I do have to add the Anabaena protein encoded by alr1779, which I do.")))
(:pre (:img :src "/weblistenerdocs/img-bbldoc/ex-orthologs-favorite-3.jpg"))
(:pre " -->"((:a :target "blank" :href (:print (make-help-topic-url :name "results3")))"RESULTS"))

(:small (:ul (:i " Translation: The asterisk indicates that what should be aligned is the result of the previous operation (i.e. adding the sixth sequence to the set)."))
(:p (:b "Conclusion "))
(:p "The Trichodesmium protein looks every inch a cyanophycin synthase (annotation be damned), while the added Anabaena sequence is a very poor specimen, similar to only one small portion of the protein. ")))))
(:P)
(:table :align "middle" :width "80%" (:b
(:tr (:td :width "26.7%" :align "left" ((:a :style "text-decoration:none" :href (:print (make-help-topic-url :name "Examples related to orthologous relationships")))(:button " Previous Page ")))
(:td :width "26.7%" :align "middle"((:a :style "text-decoration:none" :href (:print (make-help-topic-url :name "Examples related to orthologous relationships")))(:button "Up one level")))
(:td :align "right" :width "26.7%"((:a :style "text-decoration:none" :href (:print (make-help-topic-url :name "Find all genes in organism A that lack orthologs in organism B")))(:button " Next Page "))))

))
))


;;==Find all genes in organism A that lack orthologs in organism B==

(def-topic "Find all genes in organism A that lack orthologs in organism B"
(:text

(:p ((:a :style "color:#0000EE" :href (:print (make-help-topic-url :name "BioBIKE Examples"))) "BioBIKE Examples") " > "
((:a :style "color:#0000EE" :href (:print (make-help-topic-url :name "Examples related to orthologous relationships"))) "Examples related to orthologous relationships") " > "
((:a :style "color:#0000EE" :href (:print (MAKE-HELP-TOPIC-URL :name "Find all genes in organism A that lack orthologs in organism B"))) "Find all genes in organism A that lack orthologs in organism B"))

(:table :width "80%" :align "middle"
(:tr
(:td (:small
(:p (:b "Problem"))
(:p "Anabaena PCC 7120 and Anabaena variabilis are similar in most respects, but they do have some important differences. For one thing, Anabaena variabilis forms akinetes. You hope that by identifying the set of genes that are found in A. variabilis but not PCC 7120, you might find within the set those genes related to akinete formation.")
(:p (:b "Strategy"))
(:p "Very simple. Take the set of all protein of A. variabilis and subtract out those that are in common with PCC 7120."))
(:pre " ("((:a :href (:print (make-help-function-documentation-url :name "ASSIGN" :package "bbi" ))) "ASSIGN" )" Avar-A7120-difference ("((:a :href (:print (make-help-function-documentation-url :name "SET-DIFFERENCE" :package "bbi"))) "SET-DIFFERENCE") " ("((:a :href (:print (make-help-function-documentation-url :name "PROTEINS-OF" :package "bbi"))) "PROTEINS-OF") " Avar )
(" ((:a :href (:print (make-help-function-documentation-url :name "COMMON-ORTHOLOGS-OF" :package "bbl" )))"COMMON-ORTHOLOGS-OF") " { Avar A7120 })))")
(:pre " -->"((:a :target "blank" :href (:print (make-help-topic-url :name "results4")))"RESULTS"))

(:small (:ul (:i "Translation: Subtract one set of proteins from another, i.e. the set of genes in common between Avar and A7120 (nicknames for A. variabilis and A. PCC 7120) from the complete set of proteins from A. variabilis. This new set is given the name Avar-A7120-difference."))
(:p "Click on the results and you'll see that you got a lot of protein in A. variabilis that aren't in A. PCC 7120!
... How many?"))
(:pre " ("((:a :href (:print (make-help-function-documentation-url :name "LENGTH-OF" :package "bbi" ))) "LENGTH-OF" )" ("((:a :href (:print (make-help-function-documentation-url :name "PROTEINS-OF" :package "bbi"))) "PROTEINS-OF") " Avar))")
(:pre " -->6750")
(:pre " ("((:a :href (:print (make-help-function-documentation-url :name "LENGTH-OF" :package "bbi" ))) "LENGTH-OF" )" (COMMON-ORTHOLOGS Avar A7120))")
(:pre " -->4447")
(:pre " ("((:a :href (:print (make-help-function-documentation-url :name "LENGTH-OF" :package "bbi" ))) "LENGTH-OF" )" ("((:a :href (:print (make-help-function-documentation-url :name "PROTEINS-OF" :package "bbi"))) "PROTEINS-OF") " Avar-A7120-difference))")
(:pre " -->2303")

(:small(:ul (:i "Translation : Each set is considered a list, whose length is the number of elements within it. LENGTH reports the number of elements of a list."))
(:p (:b "Conclusion"))
(:p "About 35% of the protein of A. variabilis do not have orthologs in A. PCC 7120. If akinete-specific genes are hidden in this number, it's going to be a long search. You have the set stored now, and maybe you'll think of a way to cut down the size of the list of candidates, perhaps by comparing this list to the list of genes identified by an appropriate microarray.")))))
(:P)
(:table :align "middle" :width "80%" (:b
(:tr (:td :width "26.7%" :align "left" ((:a :style "text-decoration:none" :href (:print (make-help-topic-url :name "Get all available orthologs of your favorite gene and align them")))(:button " Previous Page ")))
(:td :width "26.7%":align "middle" ((:a :style "text-decoration:none" :href (:print (make-help-topic-url :name "Help for BBL Users")))(:button "Help for BBL Users")))
(:td :align "right" :width "26.7%"((:a :style "text-decoration:none" :href (:print (make-help-topic-url :name "Find all genes in all heterocystous cyanobacteria that are lacking in all other cyanobacteria")))(:button " Next Page "))))

))
))

;;==Find all genes in all heterocystous cyanobacteria that are lacking in all other cyanobacteria==

(def-topic "Find all genes in all heterocystous cyanobacteria that are lacking in all other cyanobacteria"
(:text

(:p ((:a :style "color:#0000EE" :href (:print (make-help-topic-url :name "BioBIKE Examples"))) "BioBIKE Examples") " > "
((:a :style "color:#0000EE" :href (:print (make-help-topic-url :name "Examples related to orthologous relationships"))) "Examples related to orthologous relationships") " > "
((:a :style "color:#0000EE" :href (:print (MAKE-HELP-TOPIC-URL :name "Find all genes in all heterocystous cyanobacteria that are lacking in all other cyanobacteria" ))) "Find all genes in all heterocystous cyanobacteria that are lacking in all other cyanobacteria" ))

(:table :width "80%" :align "middle"
(:tr
(:td (:small
(:p(:b "Problem"))
(:p "The heterocystous cyanobacteria form a coherent taxon, and it would be of interest to know what set of genes define them. Perhaps in this set lies many unidentified genes related to the differentiation and function of heterocysts.")
(:p (:b "Strategy"))
(:p "Look for all genes that are:")
(:ul "1. Found in all cyanobacteria that make heterocysts")
(:ul "2. Absent in all cyanobacteria that diverged from heterocystous cyanobacteria before heterocysts arose in evolution.")
(:p (:b "Step 1:")" Find all proteins common amongst heterocystous cyanobacteria.
For consistency, use the names of one organism (Anabaena PCC 7120)"))
(:pre " ("((:a :href (:print (make-help-function-documentation-url :name "ASSIGN" :package "bbi" ))) "ASSIGN")" het-cb = { A7120 Avar Npun })")
(:pre " -->(#$anabaena_pcc7120 #$anabaena_variabilis_atcc29413 #$nostoc_punctiforme_atcc29133)")
(:pre " ("((:a :href (:print (make-help-function-documentation-url :name "ASSIGN" :package "bbi" ))) "ASSIGN")" het-core-proteins
("((:a :href (:print (make-help-function-documentation-url :name "COMMON-ORTHOLOGS-OF" :package "bbi" ))) "COMMON-ORTHOLOGS-OF")" het-cb))")
(:pre " -->"((:a :target "blank" :href (:print (make-help-topic-url :name "results5")))"RESULTS"))

(:small(:p (:b "Step 2:")" The core genes contain not only those genes required for heterocyst function but also those genes required simply for life. To remove many of those, subtract from the core set any gene also found in nonheterocystous cyanobacteria."))
(:pre " ("((:a :href (:print (make-help-function-documentation-url :name "ASSIGN" :package "bbi" ))) "ASSIGN")" non-het-cb (REMOVE-FROM-SET *all-organisms* het-cb))")
(:pre " -->(#$prochlorococcus_marinus_ss120 #$synechocystis_pcc6803 #$trichodesmium_erythraeum)")
(:small (:ul (:i "Translation: This code defines a set, called non-het-cb, found by removing from all cyanobacteria known to BioBIKE those that form heterocysts. This operation gives a feeble set of three cyanobacteria [sorry, we're working on increasing that number!].")))
(:pre " ("((:a :href (:print (make-help-function-documentation-url :name "ASSIGN" :package "bbi")))"ASSIGN") " non-het-specific-proteins
(FOR-EACH organisms IN non-het-cb
APPEND ("((:a :href (:print (make-help-function-documentation-url :name "COMMON-ORTHOLOGS-OF" :package "bbi" ))) "COMMON-ORTHOLOGS-OF")" { A7120 organisms })) ")
(:pre "-->
1121 common orthologs found
2157 common orthologs found
2298 common orthologs found
(long list)")

(:pre " ("((:a :href (:print (make-help-function-documentation-url :name "ASSIGN" :package "bbi")))"ASSIGN")" het-specific-proteins (SET-DIFFERENCE het-core-proteins non-het-specific-proteins))")
(:pre " -->"((:a :target "blank" :href (:print (make-help-topic-url :name "results6")))"RESULTS"))

(:pre " ("((:a :href (:print (make-help-function-documentation-url :name "LENGTH-OF" :package "bbi" ))) "LENGTH-OF" )" het-specific-proteins)" )
(:pre "-->632")
(:small (:ul (:i "Translation : A new set is defined, the set called het-specific-protein. It is derived by subtracting from those proteins of A7120 found with orthologs in all heterocystous cyanobacteria the set or proteins also found in at least one nonheterocystous cyanobacteria. The set consists of 1090 proteins."))
(:p (:b "Step 3: ") "The list is nice but not very informative. Here we extract for each core gene possibly useful information and put it in a tab-delimited format readable by Excel."))
(:pre " ("((:a :href (:print (make-help-function-documentation-url :name "ASSIGN" :package "bbi" ))) "ASSIGN" )" het-specific-gene-info
(FOR-EACH protein IN het-specific-proteins
AS gene = ("((:a :href (:print (make-help-function-documentation-url :name "GENE-OF" :package "bbi" ))) "GENE-OF" )" protein)
AS gene-info = gene[\.contigous-sequence \.from \.to]
AS description = ("((:a :href (:print (make-help-function-documentation-url :name "DESCRIPTION-OF" :package "bbi" ))) "DESCRIPTION-OF" )" gene)
DO ("((:a :href (:print (make-help-function-documentation-url :name "INSERT" :package "bbi" ))) "INSERT" )" description INTO gene-info AT \"END\")
("((:a :href (:print (make-help-function-documentation-url :name "INSERT" :package "bbi" ))) "INSERT" )" gene INTO gene-info AT \"BEGIN\")
COLLECT gene-info ))")
(:pre " -->"((:a :target "blank" :href (:print (make-help-topic-url :name "results7")))"RESULTS"))

(:pre " (WRITE-TAB-DELIMITED-FILE \"het-specific-gene-info.txt\" het-specific-gene-info)")
(:pre " -->T")
(:small (:ul (:i "Translation : This code collects information on each gene. First, since the desired information is associated with the gene, not the protein, the gene name is taken from the protein name. Then information is extracted from the gene frame. The description of the gene is special, as it may come from a variety of sources : e.g. the annotation, if available, or if not, then the best blast hit description. The resulting information is written to a tab-delimited file, which can be downloaded to your computer and uploaded into Excel."))
(:p (:b "Conclusion " ))
(:p " A significant fraction of the genes of heterocystous cyanobacteria are specific to that class. Presumably, many of those proteins responsible for the adaptations that evolved after the divergence of hetercystous cyanobacteria are in this set.")
) )))
(:p)
(:table :align "middle" :width "80%" (:b
(:tr (:td :width "26.7%" :align "left" ((:a :style "text-decoration:none" :href (:print (make-help-topic-url :name "Find all genes in organism A that lack orthologs in organism B")))(:button " Previous Page ")))
(:td :width "26.7%":align "middle" ((:a :style "text-decoration:none" :href (:print (make-help-topic-url :name "Help for BBL Users")))(:button "Help for BBL Users")))
(:td :align "right" :width "26.7%"((:a :style "text-decoration:none" :href (:print (make-help-topic-url :name "Find all genes with orthologs in all cyanobacteria, and export the set to Excel")))(:button " Next Page "))))

))
))

;;==Find all genes with orthologs in all cyanobacteria, and export the set to Excel==

(def-topic "Find all genes with orthologs in all cyanobacteria, and export the set to Excel"
(:text

(:p ((:a :style "color:#0000EE" :href (:print (make-help-topic-url :name "BioBIKE Examples"))) "BioBIKE Examples") " > "
((:a :style "color:#0000EE" :href (:print (make-help-topic-url :name "Examples related to orthologous relationships"))) "Examples related to orthologous relationships") " > "
((:a :style "color:#0000EE" :href (:print (MAKE-HELP-TOPIC-URL :name "Find all genes with orthologs in all cyanobacteria, and export the set to Excel"))) "Find all genes with orthologs in all cyanobacteria, and export the set to Excel"))

(:table :align "middle" :width "80%"
(:tr
(:td (:small
(:p (:b "Problem"))
(:p "Although cyanobacteria emerged billions of years ago, there are some characteristics that have not changed, foremost amongst them are elements of the photosynthetic apparatus. What is the full set of properties at the core of being a cyanobacterium?")
(:p (:b "Strategy"))
(:p (:b "Step 1: ")"I'll address a simpler but related question: What genes are in common amongst all available cyanobacteria? BioBIKE answers this question in one step:"))
(:pre " ("((:a :href (:print (make-help-function-documentation-url :name "COMMON-ORTHOLOGS-OF" :package "bbi" ))) "COMMON-ORTHOLOGS-OF")" *all-organisms* PRIMARY s6803)")
(:pre " -->"((:a :target "blank" :href (:print (make-help-topic-url :name "results8")))"RESULTS"))

(:pre " ("((:a :href (:print (make-help-function-documentation-url :name "ASSIGN" :package "bbi" ))) "ASSIGN" )" cb-core-proteins *)")
(:pre " -->"((:a :target "blank" :href (:print (make-help-topic-url :name "results9")))"RESULTS"))

(:pre " *all-organisms*")
(:pre " -->(#$prochlorococcus_marinus_ss120 #$trichodesmium_erythraeum #$anabaena_variabilis_atcc29413
#$anabaena_pcc7120 #$nostoc_punctiforme_atcc29133 #$synechocystis_pcc6803)")

(:small(:ul (:i "Translation: I decided to end up with a set of core protein in terms of Synechocystis PCC 6803 (nickname S6803) and declared its protein to be the primary reference set. COMMON-ORTHOLOGS-OF finds all proteins in the primary organism with orthologs in each of the organisms specified, in this case all organisms known to BioBIKE. Clicking on results will show you the set of proteins found. This looks like a useful set, so I assign it a name by which it can be remembered. I'd like to know what all organisms known to BioBIKE are, and entering *loaded-organisms* gives me that list. So there are 957 common orthologs amongst the six currently available cyanobacteria."))
(:p (:b "Step 2: ") "I'd like to have the list sitting in an Excel file on my own computer. "))
(:pre " (FOR-EACH protein IN cb-core-proteins
AS gene = ("((:a :href (:print (make-help-function-documentation-url :name "GENE-OF" :package "bbi" ))) "GENE-OF" )" protein)
AS gene-info = gene[\.fname \.contigous-sequence \.from \.to]
DO ("((:a :href (:print (make-help-function-documentation-url :name "INSERT" :package "bbi" ))) "INSERT" )" description INTO gene-info AT \"END\")
COLLECT gene-info )")
(:pre " -->"((:a :target "blank" :href (:print (make-help-topic-url :name "results10")))"RESULTS"))

(:pre " (WRITE-TAB-DELIMITED-FILE \"cb-core-gene-info.txt\")" )
(:pre " -->T")
(:small
(:p (:i "Translation : I consider each gene in the the set of core protein, determine the name of the corresponding gene, and save pertinent information (\"FName\" refers to the name of the gene, also the name of the frame). Finally, the information is converted to a tab-delimited file for later upload into Excel."))

(:p (:b "Step 3: ") "Out of curiousity \.\.\. how many of these core protein are annotated as unknown or hypothetical?"))
(:pre " (FOR-EACH gene-info IN **
AS description = (FIFTH gene-info)
COLLECT gene-info )")
(:pre " -->258")
(:small
(:ul (:i "Translation : I consider each set of gene-info (** refers to the result two commands ago), taking the fifth element of the set (you'll see this is the description of the gene if you click on the results of line 4). If the word \"hypothetical\" or \"unknown\" appears in the description, then the gene is counted. 258 such genes are found."))
(:p (:b "Conclusion " ))
(:p " Nearly a thousand genes in Synechocystis PCC 6803 have orthologs in five other diverse cyanobacteria and may be considered a tentative set of core cyanobacterial genes. About a fourth of these have unknown functions.")))))
(:P)
(:table :align "middle" :width "80%" (:b
(:tr (:td :width "26.7%" :align "left" ((:a :style "text-decoration:none" :href (:print (make-help-topic-url :name "Find all genes in all heterocystous cyanobacteria that are lacking in all other cyanobacteria")))(:button " Previous Page ")))
(:td :width "26.7%":align "middle" ((:a :style "text-decoration:none" :href (:print (make-help-topic-url :name "Help for BBL Users")))(:button "Help for BBL Users")))
(:td :align "right" :width "26.7%"((:a :style "text-decoration:none" :href (:print (make-help-topic-url :name "Examples related to searching for queries")))(:button " Next Page "))))

))
))
