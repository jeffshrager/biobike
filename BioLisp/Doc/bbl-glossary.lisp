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

;;; Authors:  JP Massar, Peter Seibel.

(def-glossary-entry "Amino acids"
  (:summary "A monomeric subunit of proteins.")
  (:text 
   (:p
    #.(one-string-sp
       "Proteins are poly-amino acids connected by peptide bonds. Each amino acid consists of")
      (:UL
        (:LI "a " (:B "carboxylate") " end")
        (:LI "an " (:B "alpha carbon") " to which is attached an amino group")
        (:LI "an " (:B "side group") " which is particular to each kind of amino acid"))
      "The structures of the amino acids can be found "
      ((:A :HREF "http://upload.wikimedia.org/wikipedia/commons/a/a9/Amino_Acids.svg")"here")
      (:BR)
      "Abbreviations for the nucleotides and their combinations can be found "
      ((:A :HREF "/weblistenerdocs/amino-acid-abbreviations.htm")"here")
     )
   (:P 
      #.(one-string-sp
          "'Amino acid' (abbrevaiated 'aa') is often used as a unit of the length"
          "of protein.")
     )
   (:P
      "The molecular weights of amino acids, polypeptides, and proteins, can be found using the "
      ((:a :Target "blank" 
           :href (:print (make-help-function-documentation-url 
                             :name "MW-OF" :package "bbi" )))
      "MW-OF") " function.")
   (:P
     "The amino acid sequence of a protein can be accessed through the "
      ((:a :Target "blank" 
           :href (:print (make-help-function-documentation-url 
                             :name "SEQUENCE-OF" :package "bbi" )))
      "SEQUENCE-OF") " function.")
  )
  (:keywords "protein")
  (:see-also 
     (URL "http://en.wikipedia.org/wiki/Amino_acid" "'Amino acid' Wikipedia")
     (URL "/weblistenerdocs/amino-acid-abbreviations.htm" "Amino acid abbreviations")
     bbi:MW-OF
     bbi:SEQUENCE-OF
   )
)

(def-glossary-entry "Conditionals" 
  (:summary "Choose their result from among a set of alternatives.")
  (:text 
   (:p "Conditional functions or decision making functions (e.g. IF, IF-TRUE, IF-FALSE...) are those" (:br "that choose their result from among a set of alternatives based on the value of predicate expressions."))

    )
  (:keywords "alternative" "decision" "predicate" )
  (:see-also 
   (Glossary "True and False")
   bbi:ALL-TRUE
   bbi:ALL-FALSE
   bbi:ANY-TRUE
   bbi:ANY-FALSE
   bbi:IF-TRUE
   bbi:IF-FALSE
   ))

(def-glossary-entry "Nucleotide"
  (:summary "A monomeric subunit of DNA and RNA.")
  (:text 
   ; (:table :align "middle" :width "80%"
    ; (:tr (:td
   (:p
    #.(one-string-sp
       "DNA and RNA are polynucleotides, each nucleotide subunit consisting of:")
      (:UL
        (:LI "a " (:B "base") ": adenine (A), cytosine (C), guanine (G), or thymine (A) (or uracil (U), in the case of RNA)")
        (:LI "a " (:B "sugar") ": deoxyribose or ribose, in the case of DNA or RNA, respectively")
        (:LI (:B "phosphate") ": generally one but possibly three at the 5' end of the polynucleotide"))
      "Abbreviations for the nucleotides and their combinations can be found "
      ((:A :HREF "/weblistenerdocs/nucleotide-abbreviations.htm")"here")
     )
   (:P 
      #.(one-string-sp
          "'Nucleotide' (abbrevaiated 'nt') is often used as a unit of the length"
          "of DNA and RNA. In the case of"
          "double-stranded DNA, 'base-pair' (abbreviated 'bp') is a synonymous unit.")
     )
   )
  (:keywords "basepair" "base-pair")
  (:see-also 
     (URL "http://en.wikipedia.org/wiki/Nucleotide" "'Nucleotide' Wikipedia")
     (URL "/weblistenerdocs/nucleotide-abbreviations.htm" "Nucleotide abbreviations")
   )
)

(def-glossary-entry "ortholog"
  (:summary "A gene or protein uniquely related to the one under consideration.")
  (:text 
   ; (:table :align "middle" :width "80%"
    ; (:tr (:td
   (:p
    #.(one-string-sp
       "Two genes or proteins are in an orthologous relationship if:"))
   (:UL (:LI "They are in different organisms")
        (:LI "They share a common ancestral gene")
        (:LI #.(ONE-STRING-SP 
                "The phylogenetic tree encompassing the two is superimposable"
                "on the phylogenetic tree encompassing their organisms")))
   (:P #.(ONE-STRING-SP
         "Genes within a single organism that arose from the same ancestral"
         "gene are termed paralogs. The divergence of paralogs can lead to"
         "paralogs appearing in"
         "different organisms."))
   (:P   "If protein" (:B " A ") 
         "is an ortholog of protein" (:B " B ")
         ", then" (:B " B ") "is an ortholog of" (:B " A") ". However,"
         "if" (:B " A ") "is an ortholog of" (:B " B ") 
         "and" (:B " B ") "an ortholog of protein"
         (:B " C") ", it may be that"
         (:B " A ") "is not an ortholog of" (:B " C") ".")
   (:P (:U "Implementation in BioBIKE")(:BR)
       #.(ONE-STRING-SP 
         "An orthologous relationship is a fact of nature but one that may"
         "be very difficult to determine. Different methods have"
         "been used to predict orthology, generally relying on sequence"
         "similarity. Users of BioBIKE are free to employ any desired"
         "method to define orthology, but the default method is what is"
         "sometimes called bidirectional-best-hit. Unless the user specifies"
         "a different test within the ORTHOLOG-OF function, an ortholog is"
         "operationally defined as follows. Protein")
         (:B " A ")
         "in organism" 
         (:B " X ")
         "is an ortholog of protein"
         (:B " B ")
         "in organism" 
         (:B " Y ")
         "so long as:")
   (:UL (:LI (:B " X ") "and" (:B " Y ") "do not represent the same organism")
        (:LI "Protein" (:B " B ") 
         "is the protein in organism" (:B " Y ") "most similar to protein"
         (:B " A"))
        (:LI "Protein" (:B " A ") 
         "is the protein in organism" (:B " X ") "most similar to protein"
         (:B " B"))
        (:LI "In both cases, the degree of similarity as determined by the Blast "
             "is better than E = 10^-6")) 
  )
  (:keywords "Blast" "Similarity" "Sequence" "Paralog")
  (:see-also 
   ;; (glossary "paralog")
   ;; (tutorial "orthologs") 
   (df "bbi:ortholog-of")
   (DF "bbi:sequence-similar-to") 
   (URL "http://dx.doi.org/10.1016/S0168-9525(00)02005-9"
        "'Homology: A personal view on some of the problems' Walter M Fitch")
   (URL "http://www.pubmedcentral.nih.gov/articlerender.fcgi?artid=138949"
        "'Orthologs and Paralogs - We need to get it right' Roy A Jensen")
   ;; (:symbol "bio:two-way-ortholog-of")
   ))


(def-glossary-entry "Recursive function"
  (:summary "A function that refers to itself during its execution.")
  (:text 
   ; (:table :align "middle" :width "80%"
    ; (:tr (:td
   (:p
    #.(one-string-sp
       "A recursive function is a function that calls itself during its execution."
	   "This enables the function to repeat itself several times.")
	   (:BR)
	#.(one-string-sp
	   "Below is an example of a recursive function:")(:BR)
	   (:pre"
				function Factorial(n)
				if (n = 1) return 1
				else return n * Factorial(n-1)"
		)
		(:BR)
	#.(one-string-sp  "The function Factorial() above uses recursion to calculate the factorial"
	   "of any number. For example, Factorial(1) would return 1. Factorial(3)"
	   "would return 6.")
	)
	(:p
       "Recursive functions are common in computer science because they allow
	   programmers to write programs using a minimal amount of code.
	   The downside is that they can cause infinite loops and other unexpected
	   results if not written properly. If proper cases are not included in the
	   function to stop the execution, the recursion will repeat forever. 
	   The end condition is known as the " (:b "base case")".
	   BioBIKE protects the user by stopping the execution after a certain amount of time."
	)
	(:p "In general terms, a recursive function works like this:"
		(:ol
			(:li "The calling code calls the recursive function.")
			(:li "The function does any processing or calculations required.")
			(:li "If the "(:b "base case")" has not yet been reached, the function calls itself to continue the recursion. This creates a new instance of the function.")
			(:li "If the "(:b "base case")" is reached, the function just returns control to the code that called it, thereby ending the recursion.")
		)	
	)
   );)))
  (:keywords "Define-function" "Recursive-function")
  (:see-also 
    (topic "How to build a recursive function") 
   ))


(def-glossary-entry "True and False" 
  (:summary "")
  (:text 
   (:p "Two symbols have a special meaning:" )
   (:ul 
 	  (:li "T   - True, exists, \"yes\"  ")
	  (:li "NIL - False, emptiness, \"no\" "))
   (:p "Certain functions act conditionally, based on the value of a variable. "
       "For example, execution of the form"
      (:BLOCKQUOTE "IF-TRUE x THEN [take some action] ")
       "will lead to no action being taken if x has the value NIL. If x has "
       (:B (:I "any other value" ))
       ", then the action will be taken. The value may be T, or 47, or "
       "\"platypus\", it doesn't matter.")
  )
  (:keywords "True" "Exist" "False" "Emptiness" "NIL" "meaning")
  (:see-also 
   (Glossary "Conditionals")
   bbi:IF-TRUE
   bbi:IF-FALSE
   bbi:ALL-TRUE
   bbi:ALL-FALSE
   bbi:ANY-TRUE
   bbi:ANY-FALSE
   ))

(def-glossary-entry "Variable"
  (:summary "A symbol representing a place to store a value or values.")
  (:text 
   ; (:table :align "middle" :width "80%"
    ; (:tr (:td
   (:p
    #.(one-string-sp
       "The significance of 'variable' in computer programming differs somewhat"
       "from its familiar use in mathematics. There, a variable refers to an unknown"
       "quantity. In programming, if a quantity is unknown at the time a program is"
       "executed, this is not good -- you'll get an error."))
   (:P
    #.(one-string-sp
       "In programming, a variable is different, "
       "a symbol that refers to a place in computer memory that holds one or more"
       "values. This usage is still a bit similar to that in mathematics, because the value"
       "of a variable may not be known to the programmer."))
   (:P
	#.(one-string-sp
	   "Below is an example of the use of a variable, x:")(:BR)
	   (:img :src "/weblistenerdocs/img-bbldoc/GL-variable-1.jpg")(:BR)
    ((:a :Target "blank" 
           :href (:print (make-help-function-documentation-url 
                             :name "DEFINE-FUNCTION" :package "bbi" )))
      "DEFINE-FUNCTION") 
    #.(one-string-sp
      " specifies that the value 47 be stored in a place in memory"
      "represented by the variable x. Where? That's the computer's business."
      "All you need to know is that it exists."))
   (:P
	#.(one-string-sp
	  "It would be an error to enclose x within quotes. Then it would no longer"
      "be a symbol but rather a string. Variables are always symbols, not strings.")
   ))
  (:keywords "Define-function" "Assign")
 ; (:see-also (topic "Symbols") )
   )