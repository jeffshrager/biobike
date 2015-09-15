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

;; ================= EXAMPLES RELATED TO MICROARRAYS - MENU ==================

;;=======examples related to microarray data =====

(def-topic "Examples related to microarray data"
(:text

(:p ((:a :style "color:#0000EE" :href (:print (make-help-topic-url :name "BioBIKE Examples"))) "BioBIKE Examples") " > "
((:a :style "color:#0000EE" :href (:print (make-help-topic-url :name "Examples related to microarray data"))) "Examples related to microarray data"))

(:table :width "80%" :align "middle"
(:tr
(:td (:small

(:ol :type "1"
(:li ((:a :style "color:#0000EE" :href (:print (make-help-topic-url :name "Examples related to sequences"))) (:b "Examples related to sequences")))
(:p) (:li ((:a :style "color:#0000EE" :href (:print (make-help-topic-url :name "Examples related to proteins and translation"))) (:b "Examples related to proteins and translation")))
(:p) (:li ((:a :style "color:#0000EE" :href (:print (make-help-topic-url :name "Examples related to orthologous relationships"))) (:b "Examples related to orthologous relationships")))
(:p) (:li ((:a :style "color:#0000EE" :href (:print (make-help-topic-url :name "Examples related to searching for queries"))) (:b"Examples related to searching for queries")))
(:p) (:li ((:a :style "color:#0000EE" :href (:print (make-help-topic-url :name "Examples related to microarray data"))) (:b (:font :color "red" "Examples related to microarray data"))))


(:ul ; :ol :type "a"
(:p) (:li ((:a :style "color:#0000EE" :href (:print (MAKE-HELP-TOPIC-URL :name "Get a description of all sets of microarray data known to BioBIKE"))) "Get a description of all sets of microarray data known to BioBIKE"))
(:p) (:li ((:a :style "color:#0000EE" :href (:print (MAKE-HELP-TOPIC-URL :name "Get a list of the most highly induced and repressed genes plus their annotations"))) "Get a list of the most highly induced and repressed genes plus their annotations"))
(:p) (:li ((:a :style "color:#0000EE" :href (:print (MAKE-HELP-TOPIC-URL :name "Find all highly expressed genes of a certain molecular weight class" ))) "Find all highly expressed genes of a certain molecular weight class" )))

(:p (:i "BioBIKE is capable of much more regarding the analysis and manipulation of large data sets, as it contains all the abilities of the analytical language R. However, its powers have not yet been captured by an easy interface. More coming! "))
)))))

(:table :align "middle" :width "80%"
(:tr
(:td :width "26.7%" :align "left" ((:a :style "text-decoration:none" :href (:print (make-help-topic-url :name "Find desired genes using a keyword search of gene descriptions")))(:button " Previous Page ")))
(:td :width "26.7%" :align "middle" ((:a :style "text-decoration:none" :href (:print (make-help-topic-url :name "Help for BBL Users")))(:button "Help for BBL Users")))
(:td :align "right" :width "26.7%"((:a :style "text-decoration:none" :href (:print (make-help-topic-url :name "Get a description of all sets of microarray data known to BioBIKE")))(:button " Next Page "))))

)
))


;; ================= EXAMPLES RELATED TO MICROARRAYS ==================

(def-topic "Get a description of all sets of microarray data known to BioBIKE"
(:text

(:p ((:a :style "color:#0000EE" :href (:print (make-help-topic-url :name "BioBIKE Examples"))) "BioBIKE Examples") " > "
((:a :style "color:#0000EE" :href (:print (make-help-topic-url :name "Examples related to microarray data"))) "Examples related to microarray data") " > "
((:a :style "color:#0000EE" :href (:print (MAKE-HELP-TOPIC-URL :name "Get a description of all sets of microarray data known to BioBIKE"))) "Get a description of all sets of microarray data known to BioBIKE"))

(:table :width "80%" :align "middle"
(:tr
(:td (:small
(:P "Microarray data is accessed via the Microarrays menu in the Data menu."))
(:pre (:img :src "/weblistenerdocs/img-bbldoc/ex-marray-microarray-access-1.jpg")) 
(:small
))))
(:P)
(:table :align "middle" :width "80%" (:b
(:tr (:td :width "26.7%" :align "left" ((:a :style "text-decoration:none" :href (:print (make-help-topic-url :name "Examples related to microarray data")))(:button " Previous Page ")))
(:td :width "26.7%":align "middle" ((:a :style "text-decoration:none" :href (:print (make-help-topic-url :name "Help for BBL Users")))(:button "Help for BBL Users")))
(:td :align "right" :width "26.7%"((:a :style "text-decoration:none" :href (:print (make-help-topic-url :name "Get a list of the most highly induced and repressed genes plus their annotations")))(:button " Next Page "))))

))
))

;;==Get a list of the most highly induced and repressed genes plus their annotations==

(def-topic "Get a list of the most highly induced and repressed genes plus their annotations"
(:text

(:p ((:a :style "color:#0000EE" :href (:print (make-help-topic-url :name "BioBIKE Examples"))) "BioBIKE Examples") " > "
((:a :style "color:#0000EE" :href (:print (make-help-topic-url :name "Examples related to microarray data"))) "Examples related to microarray data") " > "
((:a :style "color:#0000EE" :href (:print (MAKE-HELP-TOPIC-URL :name "Get a list of the most highly induced and repressed genes plus their annotations"))) "Get a list of the most highly induced and repressed genes plus their annotations"))

(:table :width "80%" :align "middle"
(:tr
(:td(:small
(:p (:b "Problem"))
(:P "You want to get an overall feel for what a microarray data set is reporting, and one thought you have is to identify the extreme values -- those genes whose expression most strongly increases under the experimental condition and those whose expression most strongly decreases. Using the data set from Hihara et al (2001), examining gene expression in Synechocystis PCC 6803 after a shift in light intensity :"))

(:pre (:img :src "/weblistenerdocs/img-bbldoc/ex-marray-highly-induced-1.jpg"))
  (:pre " -->"
    ((:a :target "blank" :href "/weblistenerdocs/ex-marray-highly-induced-1.txt")
       "RESULTS"))

(:small
(:ul (:i "Translation: A set called genes-and-ratios is constructed by considering each protein in Synechocystis and, when a ratio exists in the data set of Hihara (2001), adding it to the set, along with the ratio and the annotation of the gene."))
(:p "You can sort these results, first putting the genes with greatest increase expression on top, then the reverse, like so:"))
(:pre (:img :src "/weblistenerdocs/img-bbldoc/ex-marray-highly-induced-2.jpg"))
  (:pre " -->"
    ((:a :target "blank" :href "/weblistenerdocs/ex-marray-highly-induced-2.txt")
       "RESULTS"))

(:pre (:img :src "/weblistenerdocs/img-bbldoc/ex-marray-highly-induced-3.jpg"))
  (:pre " -->"
    ((:a :target "blank" :href "/weblistenerdocs/ex-marray-highly-induced-3.txt")
       "RESULTS"))

(:small
(:ul (:i "Translation: Sort is a complicated function that will eventually be replaced by something more intuitive, but for now, it sorts the list according to the first item in each grouping (i.e. the ratio) and sorts it from greatest value to least value. (Reverse *) simply reverses the order of this list, producing one that goes from least value to greatest value."))
(:ul (:i "It is comforting to see that the class of genes whose expression most increases after a shift to high light includes many stress-related genes and those related to carbon-uptake. Likewise, the class whose expression most decreases includes many related to phycobilisomes."))))))
(:P)
(:table :align "middle" :width "80%" (:b
(:tr (:td :width "26.7%" :align "left" ((:a :style "text-decoration:none" :href (:print (make-help-topic-url :name "Get a description of all sets of microarray data known to BioBIKE")))(:button " Previous Page")))
(:td :width "26.7%":align "middle" ((:a :style "text-decoration:none" :href (:print (make-help-topic-url :name "Help for BBL Users")))(:button "Help for BBL Users")))
(:td :align "right" :width "26.7%"((:a :style "text-decoration:none" :href (:print (make-help-topic-url :name "Find all highly expressed genes of a certain molecular weight class")))(:button " Next Page "))))

))
))


;;==Find all highly expressed genes of a certain molecular weight class==

(def-topic "Find all highly expressed genes of a certain molecular weight class"
(:text

(:p ((:a :style "color:#0000EE" :href (:print (make-help-topic-url :name "BioBIKE Examples"))) "BioBIKE Examples") " > "
((:a :style "color:#0000EE" :href (:print (make-help-topic-url :name "Examples related to microarray data"))) "Examples related to microarray data") " > "
((:a :style "color:#0000EE" :href (:print (MAKE-HELP-TOPIC-URL :name "Find all highly expressed genes of a certain molecular weight class" ))) "Find all highly expressed genes of a certain molecular weight class" ))

(:table :width "80%" :align "middle"
(:tr
(:td (:small
(:p (:b "Problem"))
(:P "You've identified a spot in a 2-dimensional gel of protein from Synechocystis PCC 6803 that increases in intensity when the cyanobacterium is shifted to bright light. The protein has a molecular weight close to 50 MDal. With that information you hope to find candidate proteins that might correspond to the spot."))

(:img :src "/weblistenerdocs/img-bbldoc/ex-microarray-mw1.PNG" :height "350") (:BR)
   (help::HTML "&nbsp")" " (help::HTML "&nbsp") " " (help::HTML "&nbsp") " " 
   (:img :src "/weblistenerdocs/img-bbldoc/zrightarrow.gif") " "
   (:img :src "/weblistenerdocs/img-bbldoc/ex-microarray-mw1-result.PNG" 
      :height "18" :border "1")
(:small
(:ul (:i "Translation: Each protein of Synechocystis PCC 6803 is considered. Its molecular weight is looked up, along with the ratio of expression given in the first column (15 minutes after shift) in the data table of Hihara et al (2001). When the molecular weight lies between 48 Kdal and 52 Kdal and the expression ratio exceeds 2, the protein is put into the final list that gets reported. The first condition (EXISTS ratio) is necessary because not every gene of Synechocystis appears in the data table."))
(:p "What are these protein? To get some clue:"))
 (:img :src "/weblistenerdocs/img-bbldoc/ex-microarray-mw2.PNG") (:BR)
 (help::HTML "&nbsp")" " (help::HTML "&nbsp") " " (help::HTML "&nbsp") " "  
 (:img :src "/weblistenerdocs/img-bbldoc/zrightarrow.gif") " "
 (:img :src "/weblistenerdocs/img-bbldoc/ex-microarray-mw2-result.PNG" 
   :height "58" :border "1")
)

))
(:P)
(:table :align "middle" :width "80%"(:b
(:tr (:td :width "26.7%" :align "left" ((:a :style "text-decoration:none" :href (:print (make-help-topic-url :name "Get a list of the most highly induced and repressed genes plus their annotations")))(:button " Previous Page ")))
(:td :width "26.7%":align "middle" ((:a :style "text-decoration:none" :href (:print (make-help-topic-url :name "Help for BBL Users")))(:button "Help for BBL Users")))
(:td :align "right" :width "26.7%"((:a :style "text-decoration:none" :href (:print (make-help-topic-url :name "BioBIKE Listener")))(:button " Next Page "))))

))
))

