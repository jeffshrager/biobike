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
