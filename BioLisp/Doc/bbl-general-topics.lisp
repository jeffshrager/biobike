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
;;; Jul 11 '12. J.Myers.  Added (namestring) to Windows pathname used in concatenate call. 

(ecase (user::os?)
  (:unix nil)
  (:windows
   (net.aserve:publish-directory
    :prefix "/vpl-tutorial/"
    :destination
    (concatenate 'string (namestring user:*source-root*) "vpl/tutorial/")
    )))

(defun vpl-tutorial-file-url (file)
  (ecase (user::os?)
    (:unix
     (concatenate
      'string
      "http://"
      user::*weblistener-machine-name*
      "/"
      "biobike-doc/vpl-tutorial-dir/"
      file
      ))
    (:windows
     (concatenate 
      'string 
      "http://localhost:" 
      (format nil "~D" user::*weblistener-port*)
      "/vpl-tutorial/" 
      file
      ))))

(def-topic "HELP for BBL Users"
  (:text 
   (:table :align "middle" :width "80%"
    (:tr 
     (:td   

      (:P (:OL :TYPE "I" 
           (:LI (:b 
                 ((:a :href (:print (make-help-topic-url :name "Introduction")))
                  "Introduction"))
            (:OL :TYPE "A"
             (:LI
              ((:a :href 
                (:print (make-help-topic-url :name "Overview of BioBIKE")))
               "Overview of BioBIKE")" - What is BioBIKE? " 
              (:U "Why") " is BioBIKE?")
             (:LI
              ((:a :href 
                (:print (make-help-topic-url :name "How to get started")))
               "How to get started?") 
              " - Step-by-step, what you need to do")
             (:LI
              ((:a :href 
                (:print (make-help-topic-url :name "Ways of getting help")))
               "Ways of getting help") 
              " - From go-it-alone to human interaction")
             (:LI
              ((:a :href 
                (:PRINT 
                 (vpl-tutorial-file-url "vpl2-tutorial.html")))
               "Graphical interface tutorial") 
              " - How the interface works")
             (:LI
              ((:a :href 
                (:print (make-help-topic-url :name "BioBIKE Examples")))
               "Examples")
              " - BioBIKE in action solving biological problems"
              (:BR) (HTML "&nbsp;"))))
           (:LI 
            (:b 
             ((:a :href (:print (make-help-topic-url :name "Basic Syntax")))
              "Basic Syntax"))
            (:OL :TYPE "A"
             (:LI "Introduction to BioBIKE syntax")
             (:LI "Object types")
             (:LI "Mapping and loops")
             (:LI "Functions")
             (:LI "Maintaining your sanity")
             (:LI "Saving/retrieving your work")
             (:LI "File management"
              (:BR) (HTML "&nbsp;"))
             ))
           (:LI 
            (:B ((:A :href 
                  (:print (make-help-topic-url :name "Other BioBIKE issues")))
                 "Other BioBIKE issues"))
            (:OL :TYPE "A"
             (:LI
              ((:a :href (:print (make-help-modules-url)):target "blank")
               "Description of functions")
              " - How to use each command")
             (:LI
              ((:a :href 
                (:print (make-help-topic-url :name "BioBIKE listener")))
               "BioBIKE Listener")
              " - The text-based interactive environment")
             (:LI
              ((:a :href 
                (:print (make-help-topic-url :name "How to reference")))
               "Referencing BioBIKE")
              " - What to cite when your efforts bear fruit"
              (:BR) (HTML "&nbsp;"))
             ))
           (:LI (:b ((:a :href (:print (wb::make-feedback-form-url)))
                     "What else do you want to know?"))
            (:BR) " (I'm sure we haven't thought of everything)")
           ))
      )))

   (:p)
   (:table :align "middle"  :width "80%"  
    (:b
     (:tr  
      (:td :width "26.7%" :align "left" 
       (html ((:a :style "text-decoration:none"  
               :href  (make-new-help-options-url ))(:button "Previous Page"))))
      (:td :align "middle"  :width "26%" 
       (html ((:a :style "text-decoration:none"  
               :href  (make-new-help-options-url ))(:button "Main Help Page"))))
      (:td :width "26.7%" :align "right" 
       ((:a :style "text-decoration:none"  
         :href (:print (make-help-topic-url :name "Overview of BioBIKE"))) 
        (:button "Next Page"))))))
   ))

(def-topic "Overview of BioBIKE"
(:text 
(:table :align "middle" :width "80%" 
(:tr :align "middle" 
(:td (:img :src "http://ramsites.net/~biobike/help/knowledge-pyramid.gif")
(:td (:img :src "http://ramsites.net/~biobike/help/language-pyramid.gif")))
(:tr :align "top" 
(:td "The foundation of the knowledge base is built upon all available informatic resources pertinent to a given research community. These include genomic sequences and their annotation, metabolic knowledge, and experimental results.") 
(:td "The foundation of the programming environment is the general purpose language Lisp. Programs in Lisp can often execute as fast as C programs, and Lisp is unsurpassed in facilitating user's efforts to extend the language."))
(:tr
(:td (:b "BioBIKE") " provides primitive structures built upon a foundation derived from concepts such as genes, upstream sequences, and metabolic pathways.") 
(:td "BioLisp is a dialect of Lisp that incorporates functions useful to the manipulation and analysis of bioinformatic data."))
(:tr 
(:td "From these, derived structures are provided by " (:b "BioBIKE") " or invented by users. Some built-in derived structures are Blast scores of every protein against every organism and sets of orthologs for each pair of organisms. ") 
(:td (:b "BioBIKE") " language is derived from BioLisp, offering a simplified syntax and reduction of Lisp's and BioLisp's functions into a few powerful commands. It is designed to appeal to biologists with little if any programming experience, at the cost of some of Lisp's functionality.")))
 )
(:p) 
(:table :align "middle" :width "80%"
(:tr (:b 
(:td :width "26.7%" :align "left" ((:a :style "text-decoration:none" :href (:print (make-help-topic-url :name "Help for BBL Users")))(:button "Previous Page")))
(:td :width "26.7%" :align "middle" ((:a :style "text-decoration:none" :href (:print (make-help-topic-url :name "Help for BBL Users")))(:button "Help for BBL Users")))
(:td :width "26.7%" :align "right" ((:a :style "text-decoration:none" :href (:print (make-help-topic-url :name "How to get started")))(:button "Next Page"))))))
))



(def-topic "Ways of Getting Help"
(:text
(:table :width "80%" :align "middle"
(:tr
(:td  
(:P "There are help screens on specific topics, broad presentations of BioBIKE concepts, tutorials, and, of course, humans. How to get help depends on what kind of help you are trying to get.")
(:ol :type "1"

(:li (:B "Search for help on a specific topic")(:BR)
"Your greatest friend is the help box at the upper right of the workspace." (:BR)
   (:img :src "/weblistenerdocs/bbldf/images/help-box-sm.jpg") (:BR)
"Type in terms or phrases associated with your topic of interest and press the Enter key. You might be rewarded with links to pertinent documentation or perhaps a specific function. If a function looks promising, click its graphical representation to bring it into your workspace.")
(:P)

(:LI (:B "Search for help on a specific function") (:BR)
"If you find a BioBIKE function that looks promising but you don't know how to use it, get function-specific help on that function by clicking the question mark next to its name in a menu" (:BR) (:img :src "/weblistenerdocs/bbldf/images/GC-FRACTION-OF-help-with-arrow.jpg")) 
(:P)

(:LI (:B "Read or browse the BioBIKE Manual") (:BR)
"Some prefer an organized presentation, going from basic concepts to more complex usage. For those, there is the "
((:a :href (:print (make-help-topic-url :name "Basic Syntax"))) (:b "BioBIKE Manual")) ".")
(:P)
(:LI (:B "Look over the shoulder of a power user to see how BioBIKE works") (:BR)
"Follow click-by-click tutorials at on a variety of topics at " ((:a :href "http://biobike.csbc.vcu.edu/tours.html") (:b "BioBIKE Tutorials")) ". Or go through a few " ((:a :href (:print (make-help-topic-url :name "BioBIKE Examples"))) (:b "BioBIKE Examples")) 
    ", to see how BioBIKE can address specific biological problems. Try the same examples in BioBIKE with your own fingers.")
(:p)

(:li (:B "Ask a human") (:BR)
"If you need help (and who doesn't?) describe your problem to real human beings through the "
(:b ((:a :href (:print (wb::make-feedback-form-url) ))
    "problem interface")) " accessible here or through the HELP menu:" 
	(:BR)(:img :src "/weblistenerdocs/bbldf/images/ask-for-help-with-arrow.jpg"))
))))  

(:table :align "middle"   :width "80%" 
(:b
(:tr 
(:td  :width "26.7%" :align "left" ((:a :style "text-decoration:none" :href (:print (make-help-topic-url :name "Brief Overview")))(:button "Previous Page")))
(:td :width "26.7%":align "middle" ((:a :style "text-decoration:none" :href (:print (make-help-topic-url :name "Help for BBL Users")))(:button "Help for BBL Users")))
(:td :align "right" :width "26.7%" ((:a :style "text-decoration:none" :href (:print (make-help-topic-url :name "BioBIKE Tutorials")))(:button "Next Page"))))))
))


(def-topic "More Online Resources"
(:text 
(:table :align "middle" :width "80%" 
(:tr (:td 
(:p (:b "Biolingua Resources")) 
(:ul (:li ((:a :href "http://nostoc.stanford.edu/Docs/index.html")"BioLingua Help")" - Help and tutorials"))
(:p  (:b "Lisp Resources")) 
(:ul 
  (:li ((:a :href "http://www.franz.com")"Franz, Inc")" - Download trial version of Common Lisp; Links to Lisp resources")
  (:li ((:a :href "http://psg.com/~dlamkins/sl/cover.html")"Successful Lisp")"  - Online Lisp tutorial by David Lamkins")
  (:li ((:a :href "http://www.gigamonkeys.com/book/")"Practical Common Lisp")" - Book by Peter Seibel")
  (:li ((:a :href "http://www.lispworks.com/documentation/HyperSpec/")"Common Lisp HyperSpec")" - Technical definitions of all Lisp symbols")
  (:li ((:a :href "http://cl-cookbook.sourceforge.net/")"Common Lisp Cookbook")" - User-written How-To collection. Section on string functions particularly good")
  (:li ((:a :href "http://clocc.sourceforge.net/")"Common Lisp Open Code Collection")" - Downloadable programs, some useful")
  (:li ((:a :href "http://opensource.franz.com/")"Common Lisp Open Source Center")" (Franz) - Another collection of free programs")
  (:li ((:a :href "http://www.cliki.net/index")"CLiki (Common Lisp wiki)")" - Collection of Lisp resources and links to other resources"))


(:p (:b "Other Resources")) 
(:ul 
   (:li ((:a :href "http://biocyc.org/")"BioCyc")" - Pathway/genome databases") 
   (:li ((:a :href "http://cyano.genome.jp/")"Cyorf")" - Cyanobacterial gene annotation database") 
   (:li ((:a :href "http://www.kazusa.or.jp/cyano/cyano.html")"CyanoBase")" - Cyanobacterial genome database")
   (:li ((:a :href "http://www.geneontology.org/")"Gene Ontology (GO) Consortium")" - Nexus of controlled vocabulary")
   (:li ((:a :href "http://www.genome.ad.jp/kegg/")"Kyoto Encyclopedia of Genes and Genomes (KEGG)")" - Pathway/genome database")
   (:li ((:a :href "http://www.ncbi.nlm.nih.gov/")"National Center for Biotechnology Information (NCBI)")" - GenBank, Blast, etc")))))
  
  
(:p) 
(:table :align "middle" :width "80%" (:b
(:tr 
(:td :width "26.7%" :align "left"  ((:a :style "text-decoration:none" :href  (:print (make-help-topic-url :name "How to get help"))) (:button "Previous Page")))
(:td :width "26.7%" :align "middle" ((:a :style "text-decoration:none" :href (:print (make-help-topic-url :name "HELP for BBL users"))) (:button "Help for BBL Users")))
(:td :width "26.7%" :align "right" ((:a :style "text-decoration:none" :href (:print (make-help-topic-url :name "HELP for BBL users"))) (:button "Next Page")))
)))))
           

(def-topic "How to Get Started"
(:text
(:table :width "80%" :align "middle"
(:tr
(:td  
(:p "Different people learn in different ways. Consider the different strategies listed below.")
(:p)
(:ol :type "1"
(:li "Follow click-by-click tutorials at on a variety of topics at " ((:a :href "http://biobike.csbc.vcu.edu/tours.html") (:b "BioBIKE Tutorials")) ". At the same time, reproduce or vary the clicks in BioBIKE.")                  
(:p) 
(:li "Go through a few " ((:a :href (:print (make-help-topic-url :name "BioBIKE Examples"))) (:b "BioBIKE Examples")) 
    ", to see how BioBIKE can address specific biological problems. Try the same examples in BioBIKE with your own fingers.")
(:p)
(:li "Some prefer an organized presentation, going from basic concepts to more complex usage. For those, there is the "
((:a :href (:print (make-help-topic-url :name "Basic Syntax"))) (:b "BioBIKE Manual")) ".")
(:p)
(:li "Search all tutorials, examples, and documentation for whatever interests you by entering terms into the " (:B "help box") " at the upper right of the workspace." (:BR)
   (:img :src "/weblistenerdocs/bbldf/images/help-box-sm.jpg"))
(:p)
(:li "Try doing something you want to do -- you can't break anything! If you find a BioBIKE function that looks promising but you don't know how to use it, get " (:B "function-specific help") " on that function by clicking the question mark next to its name in a menu" (:BR) (:img :src "/weblistenerdocs/bbldf/images/GC-FRACTION-OF-help-with-arrow.jpg")) 
(:p)
(:li "If you need help (and who doesn't?) describe your problem to " 
     (:B "real human beings") ", whom you can reach through the "
	 (:b ((:a :href (:print (wb::make-feedback-form-url) )) "problem interface"))
	 ", also accessible through the HELP menu:" (:BR)(:img :src "/weblistenerdocs/bbldf/images/ask-for-help-with-arrow.jpg"))
)))) 

(:table :align "middle"   :width "80%" 
(:b
(:tr 
(:td  :width "26.7%" :align "left" ((:a :style "text-decoration:none" :href (:print (make-help-topic-url :name "Brief Overview")))(:button "Previous Page")))
(:td :width "26.7%":align "middle" ((:a :style "text-decoration:none" :href (:print (make-help-topic-url :name "Help for BBL Users")))(:button "Help for BBL Users")))
(:td :align "right" :width "26.7%" ((:a :style "text-decoration:none" :href (:print (make-help-topic-url :name "BioBIKE Tutorials")))(:button "Next Page"))))))
))

(def-topic "BioBIKE Tutorials"
(:text
(:table :width "80%" :align "middle"
(:tr
(:td  
(:ul 
(:li ((:a :href (:print (make-help-tutorial-url :name "Sticks"))) "Sticks" ) " - What is a functional language?") 
(:li ((:a :href (:print (make-help-tutorial-url :name "Genes"))) "Genes" ) " - What are genes?") 
(:li ((:a :href (:print (make-help-tutorial-url :name "Alien DNA"))) "Alien DNA" ) " - How to crack the genetic code?")))))  

(:table :align "middle"   :width "80%" 
(:b
(:tr 
(:td  :width "26.7%" :align "left" ((:a :style "text-decoration:none" :href (:print (make-help-topic-url :name "How to Get Started")))(:button "Previous Page")))
(:td :width "26.7%":align "middle" ((:a :style "text-decoration:none" :href (:print (make-help-topic-url :name "Help for BBL Users")))(:button "Help for BBL Users")))
(:td :align "right" :width "26.7%" ((:a :style "text-decoration:none" :href (:print (make-help-topic-url :name "BioBIKE Examples")))(:button "Next Page"))))))
)) 


(def-topic "How to Reference"
(:text
(:table :width "80%" :align "middle"
(:tr
(:td 
(:p (:b "How to reference BioLingua for the analysis or manipulation of data"))
(:p "Elhai J, Taton A, Massar J, Myers JK, Travers M, Casey J, Slupesky M, Shrager J. (2009). BioBIKE: A Web-based, programmable, integrated biological knowledge base." ((:A :TARGET "blank" :href "http://nar.oxfordjournals.org/content/37/suppl_2/W28.full") "Nucl Acids Res 37:W28-W32") ".")
  
(:p (:b "How to reference BioBIKE as a source of data "))
(:p "DON'T DO IT!!")

(:p "You may not cite BioBIKE as a source of data. Cite the original publication or source, which should be available through BioBIKE (Type in the name of the pertinent organism and click on the frame that appears in the History Window). If it is not evident who the source is -- Shame on us! -- please contact the " ((:a :href (:print (wb::make-feedback-form-url))) "Help Desk") ".")

(:p (:ul (:i "This policy enables you to analyze your unpublished data in BioBIKE without fear that others will use the data without your permission. What reputable journal would publish data without methods describing how it was obtained or a legitimate reference to the source or permission from the source?"))))  ))

(:table :align "middle" :width "80%" (:b
(:tr 
(:td  :width "26.7%" :align "left" ((:a :style "text-decoration:none" :href (:print (make-help-modules-url)):target "blank")(:button "Previous Page")))
(:td :width "26.7%":align "middle" ((:a :style "text-decoration:none" :href (:print (make-help-topic-url :name "Help for BBL Users")))(:button "Help for BBL Users")))
(:td :align "right" :width "26.7%" ((:a :style "text-decoration:none" :href (:print (make-help-topic-url :name "How to get help")))(:button "Next Page"))))))
))


(def-topic "Upstream/Downstream Sequences" 
  (:summary "DNA sequences that are adjacent to the beginning or end of a gene.")
  (:text 
   (:p
    #.(one-string-sp
        "A DNA sequence that lies before the beginning of a gene is said to be"
	"upstream of the gene, and a DNA sequence that lies after the end of"
	"the gene is said to be downstream of it."
	"Sequences between genes are called intergenic sequences."
  ))
  (:img :src "/weblistenerdocs/bbldf/images/upstream-downstream-1.jpg")
  (:p
    #.(one-string-sp
        "If the gene is oriented left-to-right (low coordinate to high"
	"coordinate), then the upstream sequence is to the left (has lower"
	"coordinates) than the beginning of the gene, and the downstream"
	"sequence is to the right (has higher coordinates) than the end"
	"of the gene. All of this is reversed if the gene is oriented"
	"right-to-left (high coordinate to low coordinate)."
	"Upstream and downstream sequences are always represented as"
	"5' to 3'"
  ))
  (:P	 
    #.(one-string-sp
	"A sequence upstream of one or more genes can be retrieved using"
	"the ")
   ((:A :HREF (:PRINT (MAKE-HELP-FUNCTION-DOCUMENTATION-URL 
                       :NAME "SEQUENCE-UPSTREAM-OF" :PACKAGE "bbi")))
        "SEQUENCE-UPSTREAM-OF")
   #.(one-string-sp
       " function. Similarly, a sequence downstream"
       "of one or more genes can be retrieved using the ")
   ((:A :HREF (:PRINT (MAKE-HELP-FUNCTION-DOCUMENTATION-URL 
                     :NAME "SEQUENCE-DOWNSTREAM-OF" :PACKAGE "bbi")))
        "SEQUENCE-DOWNSTREAM-OF")
   #.(one-string-sp
      " function. All upstream or downstream sequences in a genome can be"
      "retrieved using the ")
   ((:A :HREF (:PRINT (MAKE-HELP-FUNCTION-DOCUMENTATION-URL 
  	              :NAME "UPSTREAM-SEQUENCE/S-OF" :PACKAGE "bbi")))
        "UPSTREAM-SEQUENCE/S-OF")
   " function or "
   ((:A :HREF (:PRINT (MAKE-HELP-FUNCTION-DOCUMENTATION-URL 
		       :NAME "DOWNSTREAM-SEQUENCE/S-OF" :PACKAGE "bbi")))
       "DOWNSTREAM-SEQUENCE/S-OF")
   " function, respectively."
   )
  (:P	 
    #.(one-string-sp
	"These functions by default consider the upstream and downstream"
	"sequences to extend up to the nearest boundary of the nearest gene."
	"However, the LENGTH option in ")
   ((:A :HREF (:PRINT (MAKE-HELP-FUNCTION-DOCUMENTATION-URL 
		       :NAME "SEQUENCE-UPSTREAM-OF" :PACKAGE "bbi")))
       "SEQUENCE-UPSTREAM-OF")
   " and "
   ((:A :HREF (:PRINT (MAKE-HELP-FUNCTION-DOCUMENTATION-URL 
		       :NAME "SEQUENCE-DOWNSTREAM-OF" :PACKAGE "bbi")))
      "SEQUENCE-DOWNSTREAM-OF")
   #.(one-string-sp
       " will override this behavior and enable you"
       "to specify these sequences to be any arbitrary length."
  ))
  (:p
    #.(one-string-sp    
	"Sometimes it's useful to be able to retrieve the sequences to the"
	"left or right of a gene without regard to the gene's orientation"
	"(see figure above). In such cases, the ")
   ((:A :HREF (:PRINT (MAKE-HELP-FUNCTION-DOCUMENTATION-URL 
		       :NAME "SEQUENCE-LEFT-OF" :PACKAGE "bbi")))
       "SEQUENCE-LEFT-OF")
   " and "	
   ((:A :HREF (:PRINT (MAKE-HELP-FUNCTION-DOCUMENTATION-URL 
		       :NAME "SEQUENCE-RIGHT-OF" :PACKAGE "bbi")))
      "SEQUENCE-RIGHT-OF")
   " may prove helpful."
   )
  )
  (:keywords "upstream" "downstream" "intergenic" "gene" "regulatory"
             "untranslated" "UTR")
	
  (:see-also 
     (DF "bbl:sequence-upstream-of")
     (DF "bbl:sequence-downstream-of")
     (DF "bbl:sequence-left-of")
     (DF "bbl:sequence-right-of")
     (DF "bbl:upstream-sequence/s-of")
     (DF "bbl:downstream-sequence/s-of")
     (DF "bbl:intergenic-sequences-of")
 ))
 
 
 (DEF-TOPIC "Extended nucleotide symbols" 
  (:summary "Symbols to represent classes of nucleotides.")
  (:text 
   (:p
    #.(one-string-sp
        "A standard alphabet is available to represent degenerate positions"
        "in a DNA sequence. The alphabet is used by ")
        (:FOO ((:A :HREF (:PRINT (MAKE-HELP-FUNCTION-DOCUMENTATION-URL 
		       :NAME "MATCHES-OF-PATTERN" :PACKAGE "bbi")))
           "MATCHES-OF-PATTERN"))
       ", when the AS-DNA option is invoked.")
        
   (:TABLE 
     (:TR
       (:TD :STYLE "padding-right: 30px" (:B (:U "Symbol")))
       (:TD :STYLE "padding-right: 30px" (:B (:U "Equivalent")))
       (:TD (:B (:U "Mnemonic"))))
    (:TR       
       (:TD "A")(:TD "A")(:TD "Adenosine"))
     (:TR       
       (:TD "B")(:TD "C, G, T")(:TD "Not A (letter after A)"))
     (:TR       
       (:TD "C")(:TD "C")(:TD "Cytidine"))
     (:TR       
       (:TD "D")(:TD "A, G, T")(:TD "Not C (letter after C)"))
     (:TR       
       (:TD "G")(:TD "G")(:TD "Guanidine"))
     (:TR       
       (:TD "H")(:TD "A, C, T")(:TD "Not G (letter after G)"))
     (:TR       
       (:TD "K")(:TD "G, T")(:TD "Keto nucleotides"))
     (:TR       
       (:TD "M")(:TD "A, C")(:TD "aMino nucleotides"))
     (:TR       
       (:TD "N")(:TD "A, C, G, T")(:TD "aNy nucleotide"))
     (:TR       
       (:TD "R")(:TD "A, G")(:TD "puRine"))
     (:TR       
       (:TD "S")(:TD "C, G")(:TD "Strong bonding"))
     (:TR       
       (:TD "T")(:TD "T")(:TD "Thymidine"))
     (:TR       
       (:TD "V")(:TD "A, C, G")(:TD "Not T (letter after T, skipping U)"))
     (:TR       
       (:TD "W")(:TD "A, T")(:TD "Weak bonding"))
     (:TR       
       (:TD "Y")(:TD "C, T")(:TD "pYrimidine"))
  ))
 (:keywords "degenerate" "restriction")
	
 (:see-also 
     (DF "bbl:matches-of-pattern")     
 )
)

(def-topic "How to build a recursive function" 
  (:summary "This page walks through the process of building a recursive function."
			)
  (:text 
  (:TD (:B (:U "Problem Overview")))
   (:p
    #.(one-string-sp
		"We will use as an example of a recursive factorial function."
        "A basic mathematical function, the factorial provides"
		"a good example of a function that can be expressed recursively."
		"The factorial of a number n is the product of the sequence"
		"1 x 2 x 3 x ... x n, or alternatively n x (n-1) x (n-2) x ... x 1."
		"For example, the factorial of 5 is 5! = 5 x 4 x 3 x 2 x 1 = 120.")
		(:BR)(:BR)
	#.(one-string-sp
		"Such a function can be expressed the following way in mathematical"
		"notation:"
	  ))
  (:img :src "/weblistenerdocs/bbldf/images/recursivefactorial0.jpg")
  (:p
    #.(one-string-sp
        "What this tells us is that if the value of n is equal to 0,"
		"the returned result is 1. For any value above 0, we return n"
		"multiplied by the factorial of n-1, which unfolds until it reaches 1:"
		)(:BR)(:BR)
	(:TT (:B "5!")" = 5 x "(:B "4!")(:BR)
	(:B "5!")" = 5 x 4 x "(:B "3!")(:BR)
	(:B "5!")" = 5 x 4 x 3 x "(:B "2!")(:BR)
	(:B "5!")" = 5 x 4 x 3 x 2 x "(:B "1!")(:BR)
	(:B "5!")" = 5 x 4 x 3 x 2 x 1
	"))
(:TD (:B (:U "Implementation")))
(:BR)
(:OL
 (:LI "Define the function")
 (:p
	"This is a tricky step, since in order to define a recursive function, it is necessary to refer to that function before it has been defined. In BioBIKE, functions are always drawn from menus. It is necesssary, "
	"therefore to place a recursive function on the menu before the functionality has been fully described. The solution is to define the function twice -- first partially completed and finally in its completed state." 
	"To implement a recursive factorial function in BioBIKE:" 
(:UL
(:LI
	"Mouse over the"(:B " DEFINITION")
	" menu and click on "(:B "DEFINE-FUNCTION")
	". A "(:B "DEFINE-FUNCTION")
	" box will appear in the workspace. "
)(:BR)
(:LI
	"Click the " (:I "name") " box, and type "  (:TT "recursive-factorial") ". Press Enter."
)(:BR)
(:LI
	"Click the " (:I "arg") " (argument) box, and type " (:TT "n") ". Press Enter."
))

(:p
"BioBIKE requires that functions do " (:B(:I "something")) " before the definition can be accepted, so it is necessary to enter something in the body of the function, such as the number 1."
)
(:UL
(:LI
	"Click the " (:I "form") " box in the " (:B "body") " field, and type " (:TT "1") ". Press Enter."
)(:BR)
(:LI
	"Execute the partially defined function by either double-clicking " (:B "DEFINE-FUNCTION") " or clicking " (:B "Execute") " on the Action Menu (green wedge) of " (:B "DEFINE-FUNCTION") ". Your recursive factorial function should now appear in the "(:B "FUNCTIONS") " menu."
)
)(:BR)
 (:LI "Set the termination conditions")
 (:p
	   "Recursive functions are common in computer science because they allow"
	   " programmers to write efficient programs using a minimal amount of code. "
	   "The downside is that they can cause infinite loops and other unexpected"
	   " results if not written properly. If proper cases are not included in the" 
	   "function to stop the execution, the recursion will repeat forever."
	   " Bring down a "(:B "IF-TRUE") 
	   " box from the "(:B "FLOW-LOGIC")
		" menu, and drag it in the form box that constitutes the body of the function."
		" In our case, we want to know if n is equal to 1. To test this equality, select the "
		"function "(:B "ORDER") 
		" from the "(:B "FLOW-LOGIC") 
		" menu. Enter n in the"
		" first box and 1 in the second box. Select = from the comparison options."
		" Drag the entire function to the condition box of the "(:B "IF-TRUE") 
		" function. In the form box corresponding to then, enter 1. In the form box corresponding"
		" to else, drag the "(:B "PRODUCT-OF") 
		" function from the "(:B "ARITHMETIC")
		" menu. In the first number box, enter n."
		" Now that you have defined the "(:B "RECURSIVE-FACTORIAL")
		" function, you can use it in the body of your function. We want to apply the "(:B "RECURSIVE-FACTORIAL") 
		" function to n-1. To do so, bring down the "(:B "DIFFERENCE-OF") 
		" function from the "(:B "ARITHMETIC")
		" menu."
		" Type in n in the first number box and 1 in the second."
	)
	(:LI "Body of the function")
	(:p
		"Here is what your "(:B "RECURSIVE-FACTORIAL")
		" would look like:"
	  )
	  
 (:img :src "/weblistenerdocs/bbldf/images/recursivefactorial1.jpg")
  (:p
		"Re-execute " (:B "DEFINE-FUNCTION")
		" function."
	)
 (:img :src "/weblistenerdocs/bbldf/images/recursivefactorial2.jpg")
 (:BR)(:BR)
   (:LI "Testing the function")
  (:p
    	"Now, you can test your newly defined recursive factorial function."
		"Let us calculatate the factorial of 5."
  )
 (:img :src "/weblistenerdocs/bbldf/images/recursivefactorial3.jpg")
 (:p
    	"The function returns:"
  )
  (:img :src "/weblistenerdocs/bbldf/images/recursivefactorial4.jpg")
  ))
  )
  (:keywords "define-function" "recursive-function")
  
  (:see-also 
     (DF "bbl:define-function")
	 (glossary "recursive-function")
   ))

