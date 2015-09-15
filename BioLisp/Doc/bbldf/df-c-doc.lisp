;;; -*- Package: bbi; mode: lisp; base: 10; Syntax: Common-Lisp; -*-

(in-package :bbi)

;;; +=========================================================================+
;;; | Copyright (c) 2002, 2003, 2004 JP Massar, Jeff Shrager, Mike Travers |
;;; | |
;;; | Permission is hereby granted, free of charge, to any person obtaining |
;;; | a copy of this software and associated documentation files (the |
;;; | "Software"), to deal in the Software without restriction, including |
;;; | without limitation the rights to use, copy, modify, merge, publish, |
;;; | distribute, sublicense, and/or sell copies of the Software, and to |
;;; | permit persons to whom the Software is furnished to do so, subject to |
;;; | the following conditions: |
;;; | |
;;; | The above copyright notice and this permission notice shall be included |
;;; | in all copies or substantial portions of the Software. |
;;; | |
;;; | THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, |
;;; | EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF |
;;; | MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. |
;;; | IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY |
;;; | CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, |
;;; | TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE |
;;; | SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE. |
;;; +=========================================================================+

;;; Author: Tara Nulton


;================================CALC=====================================

(DOCUMENT-FUNCTION CALC

  (:SUMMARY "Allows infix arithmetic calculations."
  )
  
  (:SYNTAX (calc arithmetic-expression)
  )
  
  (:PARAMETERS
		(INFIX-EXPRESSION 
			:VALUE-TYPE STRING\,LIST\,SYMBOL
			:DOCSTRING "The arithmetic expression to be evaluated."
		)
  
       (MINUS-ACTION
			:DOCSTRING "ALWAYS treats all '-'s as minus signs. (fu-bar + 3) would yield 'fu' - 'bar' + 3. UNARY-AND-WHITESPACE 
			allows terms such as 'fu-bar' to be treated as single symbol, while the '-' in expressions such as as x+-y is handled as a minus 
			<pre>
			</pre>
			because the '-' is preceded by an operator. ONLY-WITH-WHITESPACE treats only '-'s that have a space to their immediate 
			left AND a space to their immediate right as minus signs.  So in the above expression, we would have
			the symbol 'fu-bar' + 3."
			
       )
    )
	
  (:RETURNS "The result of the calculation, presumably a number"
  )
  
  (:EXAMPLES (:FOO
		(:b (:p "1.-3. Notice these examples are treated the same \(the infix order of operations is repected)\."))
		(:img :src "/weblistenerdocs/bbldf/images/CALC-1.jpg") 
		(:img :src "/weblistenerdocs/bbldf/images/CALC-3.jpg") 
		(:img :src "/weblistenerdocs/bbldf/images/CALC-4.jpg") 
		(:b (:p "Results of above yield the same:"))
		(:img :src "/weblistenerdocs/bbldf/images/CALC-2.jpg")
		(:b (:p "4. Define a variable, execute the calc function."))
		(:img :src "/weblistenerdocs/bbldf/images/CALC-5.jpg")
		(:img :src "/weblistenerdocs/bbldf/images/CALC-6.jpg")
		(:b (:p "Results:"))
		(:img :src "/weblistenerdocs/bbldf/images/CALC-9.jpg") 
		(:b (:p "5. Define a variable, execute the calc function."))
		(:img :src "/weblistenerdocs/bbldf/images/CALC-5.jpg")
		(:img :src "/weblistenerdocs/bbldf/images/CALC-7.jpg")
		(:b (:p "Results:"))
		(:img :src "/weblistenerdocs/bbldf/images/CALC-8.jpg")
		(:b (:p "5. Define variables, then execute the calc function."))
		(:img :src "/weblistenerdocs/bbldf/images/CALC-10.jpg")
		(:img :src "/weblistenerdocs/bbldf/images/CALC-11.jpg")
		(:img :src "/weblistenerdocs/bbldf/images/CALC-12.jpg")
		(:b (:p "Results: Note that the function returns the same answer if the flag ALWAYS or UNARY-WITH-WHITESPACE
		is specified because the minus sign is always treated as such."))
		(:img :src "/weblistenerdocs/bbldf/images/CALC-13.jpg")
		(:b (:p "Note that the flag ONLY-WITH-WHITESPACES returns the error report below because 		
		only a minus with a space to the left and right is treated as a minus sign."))
		(:img :src "/weblistenerdocs/bbldf/images/CALC-14.jpg")
		(:b (:p "5. Define variables, then execute the calc function."))
		(:img :src "/weblistenerdocs/bbldf/images/CALC-15.jpg")
		(:img :src "/weblistenerdocs/bbldf/images/CALC-16.jpg")
		(:img :src "/weblistenerdocs/bbldf/images/CALC-17.jpg")
		(:b (:p "Results: no flag"))
		(:img :src "/weblistenerdocs/bbldf/images/CALC-18.jpg")
		(:b (:p "Results: flag ALWAYS"))
		(:img :src "/weblistenerdocs/bbldf/images/CALC-18.jpg")
		(:b (:p "Results: flag ONLY-WITH-WHITESPACE"))
		(:img :src "/weblistenerdocs/bbldf/images/CALC-19.jpg")
		(:b (:p "Results: flag UNARY-WITH-WHITESPACE"))
		(:img :src "/weblistenerdocs/bbldf/images/CALC-20.jpg")
	)
)
	 (:TEXT
		(:p (:li "A tool which allows the use of standard arithmetic notation.")) 
		(:p (:li "The expression may be typed as a string, a symbol, or a list."))
		(:p (:li "Because of certain limitations of syntax in the VPL, occasionally a string will have to be used there. 
		In particular, bracket notation, [ ], will not work unless it is within a string."))

		(:p (:li "A problem arises in that an expression like (fu-bar+3) is ambiguous.
		Does it mean the symbol 'fu-bar' plus 3? Or does it mean the symbol 'fu' minus the symbol 'bar' plus 3? 
		To control this interpretation, the MINUS-ACTION optional argument is provided."))) 
  
(:KEYWORDS
		"Calc, Add, Arithmetic"
)
  
(:SEE-ALSO + - / *
))
			
;========================= CHOOSE-FROM =========================

(DOCUMENT-FUNCTION CHOOSE-FROM

	(:SUMMARY
		"Selects at random an element from a string, vector, or list"
	)
	
	(:SYNTAX
		(CHOOSE-FROM sequence [WITHOUT-REPLACEMENT] [TIMES nonnegative-number])
	)

	(:PARAMETERS
		(SEQUENCE 
			:PARAMETER-TYPE REQUIRED 
			:VALUE-TYPE LABELED-SEQUENCE\,SEQUENCE  
			:DOCSTRING "the sequence you use"
		)

		(WITHOUT-REPLACEMENT 
			:PARAMETER-TYPE :FLAG
		)		
 
		(TIMES 
			:PARAMETER-TYPE KEYWORD 
			:VALUE-TYPE NONNEGATIVE-NUMBER
			:DOCSTRING "The number of times random selection occurs." 
		)
	)

	(:RETURNS
		"Random element/s from a sequence"
	)
	
	(:EXAMPLES
		"are shown below"
	)

 
(:TEXT
	(:p (:b "The function selects a random element from a sequence. Almost each time the function is used, the result is different (especially when you have long sequences). You
cannot use more than one sequence at a time.  Also randomly chooses an element from a list (see example 2)."))
	(:p (:b "1.  The same function executed three times"))
	(:img :src "/weblistenerdocs/bbldf/images/CHOOSE-FROM-1.jpg")
	(:p (:b "Results:"))
	(:img :src "/weblistenerdocs/bbldf/images/CHOOSE-FROM-2.jpg")
	(:img :src "/weblistenerdocs/bbldf/images/CHOOSE-FROM-3.jpg")
	(:img :src "/weblistenerdocs/bbldf/images/CHOOSE-FROM-4.jpg")
	(:p (:b "Note:  The sequence above containes spaces and each space is treated as a character"))
	(:p (:b "2.  Biological example"))
	(:img :src "/weblistenerdocs/bbldf/images/CHOOSE-FROM-5.jpg")
	(:p (:b "Results:"))
	(:img :src "/weblistenerdocs/bbldf/images/CHOOSE-FROM-6.jpg")
	(:p (:b "3.  Biological example choosing from photosynthesis genes and utilizing options"))
	(:img :src "/weblistenerdocs/bbldf/images/CHOOSE-FROM-7.jpg")
	(:p (:b "Results:"))
	(:img :src "/weblistenerdocs/bbldf/images/CHOOSE-FROM-8.jpg")
	
	)
	(:KEYWORDS
		"Randomize, select, selection, order"
	)

(:SEE-ALSO
		FIRST SECOND THIRD FOURTH FIFTH SIXTH SEVENTH EIGHTH RANDOMIZE
)
)



;========================= CHROMOSOME-NAMED =========================

(DOCUMENT-FUNCTION CHROMOSOME-NAMED (a.k.a. CONTIG-NAMED REPLICON-NAMED)

(:SUMMARY
	"Returns the contiguous sequence frame from a string or symbol"
)

(:SYNTAX
	(CHROMOSOME-NAMED contig-name)
	(CONTIG-NAMED contig-name)
	(REPLICON-NAMED contig-name)
)

(:PARAMETERS
	(CONTIG-NAME 
	:PARAMETER-TYPE REQUIRED 
	:VALUE-TYPE STRING\,SYMBOL 
	:DOCSTRING "A string containing the name of a chromosome."
	)
)

(:RETURNS "The name of the chromosome"
)

(:EXAMPLES 
	(:FOO
	(:p (:b "1. A simple example.")
		(:blockquote(:img :src "/weblistenerdocs/bbldf/images/CHROMOSOME-NAMED-1.jpg")
		(:blockquote(:img :src "/weblistenerdocs/bbldf/images/CHROMOSOME-NAMED-2.jpg")))
	))		
)

(:TEXT
	(:p
		(:ul (:li "This is a useful function if you had collected a list of names of genes and 
		the chromosomes they belong to and then saved it to a file. The list would unfortunately be 
		translated into strings. This function can be used to translate the names in frame  
		form back into entities."))
		)
)
(:SEE-ALSO PROTEIN/S-NAMED ORGANISM-NAMED
)
)

	

;========================= CHROMOSOME/S-OF =========================
(DOCUMENT-FUNCTION CHROMOSOME/S-OF

(:SUMMARY "Gives the associated chromosome of the ogranism, protein, etc." 
)

(:SYNTAX
)

(:PARAMETERS
	(ENTITY 
	:DOCSTRING "The chromosome/s you're looking for."
	)
	
	(NOWARNINGS
	:DOCSTRING "Will suppress the warnings associated with undefined chromosomes, or not fully sequenced
	chromosomes."
	)
	
)

(:RETURNS "A chromosome"
)

(:EXAMPLES "Detailed below, graphically."
)
(
:TEXT

(:p (:b "CHROMOSOME/S-OF will take the name of an organism, gene, or protein, and return the 
chromosome associated."))
(:p (:b "1. Simple Example"))
(:img :src "/weblistenerdocs/bbldf/images/chromosomes-of1.jpg") 
(:p (:b "Returns:"))
(:img :src "/weblistenerdocs/bbldf/images/chromosomes-of2.jpg")
(:p (:b "2. Another example"))
(:img :src "/weblistenerdocs/bbldf/images/CHROMOSOMES-OF-1.jpg")
(:p (:b "Returns:"))
(:img :src "/weblistenerdocs/bbldf/images/CHROMOSOMES-OF-2.jpg") 

)
(:KEYWORDS "Chromosome, Contig, Name"
)

(:SEE-ALSO CHROMOSOME-NAMED
)

)


;;============================= CODING-GENES-OF ============================

(DOCUMENT-FUNCTION CODING-GENES-OF

(:SUMMARY
	"Returns those genes that encode proteins"
)

(SYNTAX
)

(:PARAMETERS
	(ENTITY 
	:PARAMETER-TYPE REQUIRED
	:VALUE-TYPE GENE\,PROTEIN\,CONTIGUOUS-SEQUENCE\,ORGANISM\,NULL
	:DOCSTRING "Organism, replicon, or gene list in which coding genes are sought."
	)
)

(:RETURNS "An object of type GENE, LIST or NULL"
)

(:EXAMPLES "are shown below"
)

(:TEXT
	(:p (:b "1.If given an organism or a replicon"))
	(:img :src "/weblistenerdocs/bbldf/images/CODING-GENES-OF-1.jpg")
	(:p (:b "Returns:"))
	(:img :src "/weblistenerdocs/bbldf/images/CODING-GENES-OF-2.jpg") 
	(:p (:b "2.Another example"))
	(:img :src "/weblistenerdocs/bbldf/images/CODING-GENES-OF-3.jpg")
	(:p (:b "Returns:"))
	(:img :src "/weblistenerdocs/bbldf/images/CODING-GENES-OF-4.jpg")
	(:p (:b "3. Given an argument as list made by {} notation"))
	(:img :src "/weblistenerdocs/bbldf/images/CODING-GENES-OF-5.jpg")
	(:p (:b "Returns:"))
	(:img :src "/weblistenerdocs/bbldf/images/CODING-GENES-OF-6.jpg")
	(:p (:b "4. Given an argument as a list of coding and non-coding genes"))
	(:img :src "/weblistenerdocs/bbldf/images/CODING-GENES-OF-7.jpg")
	(:p (:b "Returns:"))
	(:p (:b  "Note: Several genes in genomes do not encode proteins"))
	(:img :src "/weblistenerdocs/bbldf/images/CODING-GENES-OF-8.jpg")
)

(:KEYWORDS "Encodes, Genes, Proteins"
)

(:SEE-ALSO NONCODING-GENES-OF GENES-OF)

)


; ================= CODON-FREQUENCIES-OF ====================

(DOCUMENT-FUNCTION CODON-FREQUENCIES-OF

(:SUMMARY
	"Calculates codon frequencies of a sequence, per Karlin"
)

(:SYNTAX
	(CODON-FREQUENCIES-OF [EACH] entity [ABSOLUTE] [LABELED] [LABEL-FREQUENCIES] [SORT-BY-CODON])
)

(:PARAMETERS
	(ENTITY 
	:PARAMETER-TYPE REQUIRED 
	:VALUE-TYPE STRING\,LABELED-SEQUENCE\,GENE\,PROTEIN\,ORGANISM\,CONTIGUOUS-SEQUENCE\,LIST  
	:DOCSTRING "Sequence or set of sequences to be analyzed"
	)

	(EACH 
	:PARAMETER-TYPE TOKEN 
	:DOCSTRING "Calculate codon frequencies separately for entities in a list"
	)

	(ABSOLUTE 
	:PARAMETER-TYPE :FLAG 
	:DOCSTRING "Calculate absolute codon frequencies"
	)

	(LABELED 
	:PARAMETER-TYPE :FLAG 
	:DOCSTRING "Precede set of frequencies by name of entity"
	)

	(LABEL-FREQUENCIES 
	:PARAMETER-TYPE :FLAG 
	:DOCSTRING "Precede each frequency with name of codon"
	)

	(SORT-BY-CODON 
	:PARAMETER-TYPE :FLAG 
	:DOCSTRING "List frequencies alphabetically by codon"
	)
)

(:RETURNS
	"A list of relative codon frequencies, organized by amino acid"
)

(:EXAMPLES "are shown below"
)

(:TEXT 
	(:p (:li "The function produces a list of codon frequencies, either relative frequencies (default) or absolute frequencies 
	(if the ABSOLUTE option is specified)."))
	(:p (:li "The relative frequencies of every codon for an amino acid add up to 1.")) 
	(:p (:li "If an amino acid has only one codon (i.e. methionine 
	and tryptophan), then the relative frequency of its codon is always 1."))
	(:p (:li "In contrast, the absolute frequencies of ALL the codons add up to 1. The formulas used to calculate 
	each are given below:"))

                                               (:pre "COUNT-OF XYZ in codons of entity
            relative frequency of codon XYZ = -------------------------------------
                                               COUNT-OF aa in amino acids of entity


                                                COUNT-OF XYZ in codons of entity
            absolute frequency of codon XYZ = -------------------------------------
                                                  COUNT-OF all codons in entity ")
        

      (:p "where XYZ is a codon that encodes the amino acid aa.")
	(:p (:li "Entity can be anything associated with a DNA sequence, including an explicit 
	sequence.")) 
	(:p (:li "Multiple sequences are not analyzed separately (unless EACH is specified). 
	Instead, their codons are lumped together and analyzed collectively.")) 
	(:p (:li "If an organism, plasmid, or contiguous sequence is specified, then the codons of all protein-encoding
	genes on the entity are considered together."))
	(:p (:b "1. A simple example."))
	(:img :src "/weblistenerdocs/bbldf/images/CODON-FREQ-OF-1.jpg")
	(:p (:b "Results:"))
	(:img :src "/weblistenerdocs/bbldf/images/CODON-FREQ-OF-2.jpg")
	(:p (:b "2.  An example utilizing the ABSOLUTE flag"))
	(:img :src "/weblistenerdocs/bbldf/images/CODON-FREQ-OF-3.jpg")
	(:p (:b "Results:"))
	(:img :src "/weblistenerdocs/bbldf/images/CODON-FREQ-OF-4.jpg")
	(:p (:b "3.  An example utilizing the LABELED-FREQUENCIES flag"))
	(:img :src "/weblistenerdocs/bbldf/images/CODON-FREQ-OF-5.jpg")
	(:p (:b "Results:"))
	(:img :src "/weblistenerdocs/bbldf/images/CODON-FREQ-OF-6.jpg")
	(:p (:b "4.  An example utilizing the  SORT-BY-CODON flag"))
	(:img :src "/weblistenerdocs/bbldf/images/CODON-FREQ-OF-7.jpg")
	(:p (:b "Results:"))
	(:img :src "/weblistenerdocs/bbldf/images/CODON-FREQ-OF-8.jpg")
	 (:LI (:U "Entity")
          ": Entity can be anything associated with a DNA sequence, including an explicit sequence. Multiple sequences are not analyzed separately (unless EACH is specified). Instead, their codons are lumped together and analyzed collectively. If an organism, plasmid, or contiguous sequence is specified, then the codons of all protein-encoding genes on the entity are considered together."(:BR)(:BR))

     (:LI (:U "Format of results; LABEL-FREQUENCIES; SORT-BY-CODON")
          ": The results of the analysis as a list of frequencies, one for each codon, presented in 20 clusters of synonomous codons. The 20 sublists are in alphabetical order by encoded amino acid (an alphabetical list of amino acids is available in the constant *amino-acids*). Within each cluster, the codons are presented in alphabetical order. If LABEL-FREQUENCIES is specified, then each frequency is preceded by the name of the codon. If SORT-BY-CODON is specified, then each frequency is preceded by the name of the codon, alphabetized by the name of the codon instead of the amino acid."
   (:PRE
"    No flag           ((ala1-freq ala2-freq ala3-freq ala4-freq)(cys1-freq cys2-freq)...)
    LABEL-FREQUENCIES (((\"GCT\" ala1-freq)(\"GCC\" ala2-freq)...)((\"TGT\" cys1-freq)...)...)
    SORT-BY-CODON     ((\"AAA\" lys1-freq)(\"AAC\" asn1-freq)(\"AAG\" lys2-freq)...)" ) )
 (:LI (:U "Entity")
          ": Entity can be anything associated with a DNA sequence, including an explicit sequence. Multiple sequences are not analyzed separately (unless EACH is specified). Instead, their codons are lumped together and analyzed collectively. If an organism, plasmid, or contiguous sequence is specified, then the codons of all protein-encoding genes on the entity are considered together."(:BR)(:BR))

     (:LI (:U "Format of results; LABEL-FREQUENCIES; SORT-BY-CODON")
          ": The results of the analysis as a list of frequencies, one for each codon, presented in 20 clusters of synonomous codons. The 20 sublists are in alphabetical order by encoded amino acid (an alphabetical list of amino acids is available in the constant *amino-acids*). Within each cluster, the codons are presented in alphabetical order. If LABEL-FREQUENCIES is specified, then each frequency is preceded by the name of the codon. If SORT-BY-CODON is specified, then each frequency is preceded by the name of the codon, alphabetized by the name of the codon instead of the amino acid."
   (:PRE
"    No flag           ((ala1-freq ala2-freq ala3-freq ala4-freq)(cys1-freq cys2-freq)...)
    LABEL-FREQUENCIES (((\"GCT\" ala1-freq)(\"GCC\" ala2-freq)...)((\"TGT\" cys1-freq)...)...)
    SORT-BY-CODON     ((\"AAA\" lys1-freq)(\"AAC\" asn1-freq)(\"AAG\" lys2-freq)...)" ) )

)
	
(:KEYWORDS "Codon frequency, Coding frequency, Absolute, Relative"
)

(:SEE-ALSO "AMINO-ACID-FREQUENCIES-OF, CODING-GENES-OF, CODON-FREQUENCY-COMPARISON"
)

) 	

; ================= CODON-FREQUENCY-COMPARISON ====================

(DOCUMENT-FUNCTION CODON-FREQUENCY-COMPARISON

(:SUMMARY 
	"Calculates codon frequencies of a sequence and compares it to a set of frequencies"
)

(:SYNTAX
	(CODON-FREQUENCY-COMPARISON entity1 [TO] entity2 [LABELED])
)

(:PARAMETERS
	(ENTITY1
	:PARAMETER-TYPE REQUIRED
	:VALUE-TYPE STRING\,LABELED-SEQUENCE\,GENE\,PROTEIN\,ORGANISM\,CONTIGUOUS-SEQUENCE\,LIST  
	:DOCSTRING "Sequence or set of sequences to be compared with entity2"
	)

	(ENTITY2 
	:PARAMETER-TYPE REQUIRED 
	:VALUE-TYPE STRING\,LABELED-SEQUENCE\,GENE\,PROTEIN\,ORGANISM\,CONTIGUOUS-SEQUENCE\,LIST  
	:DOCSTRING "Sequence or set of sequences to be compared with entity1"
	)

	(LABELED 
	:PARAMETER-TYPE  :FLAG   
	:DOCSTRING "Precede comparison score with name of entity2"
	)
)

(:RETURNS "A score or list of scores, each optionally preceded by the name of entity2"
)


(:EXAMPLES "are shown below"
)

(:TEXT
(:UL
(:LI
(:U "Meaning of comparison score")
": The function produces a score for each entity1/entity2 pair that is the difference 
in their dinucleotide biases, averaged over all 16 dinucleotides (see DINUCLEOTIDE-BIAS-Of 
for a discussion of what bias means). The score is calculated according to:"
(:PRE
" comparison score = SUM {freq1(aa) * [SUM | freq1(XYZ) - freq2(XYZ) | ] }")
"where freq1(XYZ) is the frequency of the codon XYZ in entity1, freq2(XYZ) is similarly
 defined for entity2, and freq1(aa) is the frequency of a given amino acid in entity1. 
 The inner sum is over all codons for the amino acid aa, and the outer sum is over all 
 20 amino acids. Termination codons are not considered. The comparison score is therefore 
 0 when the sequences are identical and an increasing positive number as the codon usages 
 of the two entities become more discordant. It is yours to judge for yourself whether a 
 comparison score differs more from 0 than what might be expected from a random sequence." 
 (:BR)(:BR)))
 (:p (:b "1. A simple example showin comparison with itself"))
 (:img :src "/weblistenerdocs/bbldf/images/CODON-COMP-1.jpg")
 (:p (:b "Results:"))
 (:img :src "/weblistenerdocs/bbldf/images/CODON-COMP-2.jpg")
 (:p (:b "2. A simple example"))
 (:img :src "/weblistenerdocs/bbldf/images/CODON-COMP-3.jpg")
 (:p (:b "Results:"))
 (:img :src "/weblistenerdocs/bbldf/images/CODON-COMP-4.jpg")
 (:p (:b "3.  Example utilizing flag LABELED."))
 (:img :src "/weblistenerdocs/bbldf/images/CODON-COMP-5.jpg")
 (:p (:b "Results:"))
 ((:img :src "/weblistenerdocs/bbldf/images/CODON-COMP-6.jpg")(:BR)(:BR))
(:LI (:U "Entity1 and entity2")
": Entity1 and entity2 can be anything that is associated with a DNA sequence, including 
an explicit sequence. In the case of entity1, multiple sequences are not analyzed 
separately. Instead, their codons are lumped together and analyzed collectively. If an 
organism, plasmid, or contiguous sequence is specified, then the codons of all 
protein-encoding genes on the entity are considered together. Entity2 is treated 
differently, to facilitate comparison with multiple genomes. If entity2 is a list, then each 
element is compared with entity1 separately, producing a list of comparison scores. Entity2
 may be a set of codon frequencies, as provided by CODON-FREQUENCIES-OF."
 (:BR)(:BR))

(:LI (:U "Format of results; LABELED")
": The results of the analysis is a score (if entity1 and entity2 are single entities) 
or a list of scores. If LABELED is specified, then each score is preceded by the name of 
entity2.")
)

(:KEYWORDS "Codon frequency, Comparison, Score, Amino acid"
)

(:SEE-ALSO AMINO-ACID-FREQUENCIES-OF BACKGROUND-FREQUENCIES-OF CODON-FREQUENCIES-OF
)
)

;======================================CODONS-OF================================

(DOCUMENT-FUNCTION CODONS-OF


(:SUMMARY
	"Returns codons that encode a given amino acid"
)

(:SYNTAX
	(CODONS-OF amino-acid [TO-DNA-CODON-LIST] [TO-RNA-CODON-LIST] [SINGLE-AA] [SEQUENCE])
)

(:PARAMETERS
	(AMINO-ACID 
	:PARAMETER-TYPE REQUIRED
	:VALUE-TYPE STRING\,SYMBOL
	:DOCSTRING "The amino acid name/s, or one letter abbreviations used"
	)
 
	(TO-DNA-CODON-LIST 
	:PARAMETER-TYPE :FLAG
	:DOCSTRING "Translates amino acids into codons used for DNA"
	)

	(TO-RNA-CODON-LIST 
	:PARAMETER-TYPE :FLAG
	:DOCSTRING "Translates amino acids into codons used for RNA"
	)

	(SINGLE-AA 
	:PARAMETER-TYPE :FLAG
	:DOCSTRING "Specify this if the argument is intended to be recognized as a single amino acid."
	)
  
	(SEQUENCE 
	:PARAMETER-TYPE :FLAG
	:DOCSTRING "Specify this if the argument is intended to be recognized as a sequence of amino acids."
	)
)
  
(:RETURNS
	"An object of type LIST or T"
)

(:EXAMPLES
	(:FOO
		(:p (:b "1.  Example using name of amino acid."))
		(:blockquote
		(:img :src "/weblistenerdocs/bbldf/images/CODONS-OF-1.jpg")
		(:blockquote
		(:img :src "/weblistenerdocs/bbldf/images/CODONS-OF-2.jpg")))
		(:p (:b "2.  Example using one letter abbreviation of amino acid."))
		(:blockquote
		(:img :src "/weblistenerdocs/bbldf/images/CODONS-OF-3.jpg")
		(:blockquote
		(:img :src "/weblistenerdocs/bbldf/images/CODONS-OF-4.jpg")))
		(:p (:b "3.  Utilization three letter abbreviation and flag RNA CODON LIST."))
		(:blockquote
		(:img :src "/weblistenerdocs/bbldf/images/CODONS-OF-5.jpg")
		(:blockquote
		(:img :src "/weblistenerdocs/bbldf/images/CODONS-OF-6.jpg")))
		(:p (:b "4.  Argument as a string of amino acids in abbreviated format."))
		(:blockquote
		(:img :src "/weblistenerdocs/bbldf/images/CODONS-OF-7.jpg")
		(:blockquote
		(:img :src "/weblistenerdocs/bbldf/images/CODONS-OF-8.jpg")))
		(:p (:b "5.  Utilization of flag SEQUENCE."))
		(:p " Specify if you intend the argument to mean H(istidine)-I(soleucine)-S(erine) instead of Histidine.")
		(:blockquote
		(:img :src "/weblistenerdocs/bbldf/images/CODONS-OF-9.jpg")
		(:blockquote
		(:img :src "/weblistenerdocs/bbldf/images/CODONS-OF-10.jpg")))
	)
)

(:TEXT
	(:p
	(:ul (:li  "The default setting translates amino acids into codons used to transcribe DNA."))
	(:ul (:li "If the string histidine is to mean the amino acid histidine, then specify SINGLE AA.  
	If the string is intended to mean""" (:b "H")"istidine-"(:b "I")"soleucine-"(:b "S")"erine..., then specify SEQUENCE."))
    (:blockquote (:li "The above above also holds true with the three letter abbreviations."))
	(:blockquote (:li "If the string doesn't make any sense as a single amino acid, then the
	function just does the right thing"))
	(:blockquote (:li "If the string is just a single letter, it makes no difference 
	which option, if any, you specify."))
	)
	
	
	
	)

(:KEYWORDS
	"Codons, translate, translation, DNA, RNA"
)

(:SEE-ALSO
	CODON-FREQUENCIES-OF CODON-FREQUENCY-COMPARISON AMINO-ACID-COUNTS-OF AMINO-ACID-FREQUENCIES-OF
)

)

; ================= COG-ID/S-OF ====================
(DOCUMENT-FUNCTION COG-ID/S-OF

(:SUMMARY
	"Returns the COG ID (if it exists) of given gene or protein"
)

(:SYNTAX)
`
(:PARAMETERS
	(GENE-OR-PROTEIN 
	:DOCSTRING "Gene or protein to be evaluated"
	)
)

(:EXAMPLES
	(:foo
		(:p (:b "1. If given a gene."))
		(:blockquote
		(:img :src "/weblistenerdocs/bbldf/images/COG-ID-1.jpg")
		(:blockquote
		(:img :src "/weblistenerdocs/bbldf/images/COG-ID-2.jpg")))
		(:blockquote
		(:img :src "/weblistenerdocs/bbldf/images/COG-ID-5.jpg")
		(:blockquote
		(:img :src "/weblistenerdocs/bbldf/images/COG-ID-6.jpg")))
		(:blockquote
		(:img :src "/weblistenerdocs/bbldf/images/COG-ID-7.jpg")
		(:blockquote
		(:img :src "/weblistenerdocs/bbldf/images/COG-ID-8.jpg")))
		(:p (:b "2. If given a protein."))
		(:blockquote
		(:img :src "/weblistenerdocs/bbldf/images/COG-ID-3.jpg")
		(:blockquote
		(:img :src "/weblistenerdocs/bbldf/images/COG-ID-4.jpg")))
		(:p (:b "3. If given a list of genes."))
		(:blockquote
		(:img :src "/weblistenerdocs/bbldf/images/COG-ID-9.jpg")
		(:blockquote
		(:img :src "/weblistenerdocs/bbldf/images/COG-ID-10.jpg")))
		(:p (:b "4. Used in a loop."))
		(:blockquote
		(:img :src "/weblistenerdocs/bbldf/images/COG-ID-13.jpg")
		(:blockquote
		(:img :src "/weblistenerdocs/bbldf/images/COG-ID-11.jpg")
		(:blockquote
		(:img :src "/weblistenerdocs/bbldf/images/COG-ID-12.jpg"))))
)
)

(:TEXT 
	(:p 
		(:UL (:LI "COGs (Clusters of Orthologous Groups) was developed to cluster annotated 
		genes into functionally related groups in order to facilitate the transfer of functional 
		annotations among organisms"((:a href "http://www.sciencemag.org/cgi/content/abstract/278/5338/631") 
		"(Tatusov et al. 1997)")"."))

		(:ul (:li "The function returns the COG-Id number if it exits, otherwise \"ND\" or NIL is returned."))
	)
)

(:KEYWORDS
)

(:SEE-ALSO DESCRIPTION-OF
)
)

; ================= COMMON-ORTHOLOGS-OF ====================

(DOCUMENT-FUNCTION COMMON-ORTHOLOGS-OF
(:PARAMETERS
(organisms :docstring "List of organisms to be evaluated")
(no-display :docstring "To hide the number of common orthologs found")
(primary :docstring "To specify the organism... ")
(not-in :docstring "To exclude common-orthologs shared with given organism")
)
(:EXAMPLES "are shown below"
)


(:TEXT 

(:p "\"Ortholog\" is operationally defined as Two-way-ortholog-of, where
protein A in organism X is the best blast hit of protein B in organism Y and
vice versa, both hits better than a cutoff E-value of 1E-6.")
(:ul
(:li "Given organisms must be at least a list of 2 organisms.")
(:li "If PRIMARY followed by an organism is specified, the common orthologs are
given in terms of that organism;otherwise the first organism in the list is used.")
(:li "If NOT-IN followed by an organism or a a list of organisms is specified, then
common orthologs shared with these organisms are excluded from the results.")
)
(:p (:b "1.  Simple example utilizing {} notation to make a list."))
(:img :src "/weblistenerdocs/bbldf/images/COMMON-ORTHO-1.jpg")
(:p (:b "Results:"))
(:img :src "/weblistenerdocs/bbldf/images/COMMON-ORTHO-2.jpg")
(:img :src "/weblistenerdocs/bbldf/images/COMMON-ORTHO-3.jpg")
(:p (:b "2. Example using flag PRIMARY.")(:br)(:br))
(:img :src "/weblistenerdocs/bbldf/images/COMMON-ORTHO-4.jpg")
(:p (:b "Results:"))
(:img :src "/weblistenerdocs/bbldf/images/COMMON-ORTHO-5.jpg")
(:img :src "/weblistenerdocs/bbldf/images/COMMON-ORTHO-6.jpg")
(:p (:b "3. Example using flags PRIMARY and NOT-IN.")(:br)(:br))
(:img :src "/weblistenerdocs/bbldf/images/COMMON-ORTHO-7.jpg")
(:p (:b "Results:"))
(:img :src "/weblistenerdocs/bbldf/images/COMMON-ORTHO-8.jpg")
(:img :src "/weblistenerdocs/bbldf/images/COMMON-ORTHO-9.jpg")
)

(:SEE-ALSO 
	ORTHOLOGS-OF GENE-SIMILAR-TO PROTEIN-SIMILAR-TO SEQUENCE-SIMILAR-TO 
)
)
	
;=================================CONDITION===========================
(DOCUMENT-FUNCTION CONDITION

(:SUMMARY
	"Directs the logical flow based on meeting conditions"
)

(:SYNTAX)

(:PARAMETERS
	(CONDITION
	:DOCSTRING "the logical condition to be met for an action to be performed"
    
	)
	
	
	(FORM
	:DOCSTRING "One or more actions to be performed if the logical condition is met"
	)
)
	
(:RETURNS "Results of any value type"
)

(:EXAMPLES 
	(:FOO 
	(:p (:b " Usage in FOR-EACH loop."))
	(:p "This function looks at the first three nucleotides of the coding genes of ss120 and assigns them
as the start codons. The CONDITION section in the BODY then looks at each start codon, determines which
catagory it falls into, and then increments the appropriate count.")
		(:blockquote
		(:img :src "/weblistenerdocs/bbldf/images/CONDITION-2.jpg")
		(:img :src "/weblistenerdocs/bbldf/images/CONDITION-3.jpg")
		(:img :src "/weblistenerdocs/bbldf/images/CONDITION-4.jpg")
		(:img :src "/weblistenerdocs/bbldf/images/CONDITION-5.jpg")
		(:p (:b " A pop-up window returns:"))
		(:blockquote
		(:img :src "/weblistenerdocs/bbldf/images/CONDITION-1.jpg"))))
		)
		
	(:TEXT 
		(:p "CONDITION is a logical functions that can accept different 
		types of conditions and forms within the same function." (:br)(:br))
		(:p "WHEN VALUE OF is another logical function that is more specialized
		in that it only accepts conditions and forms of the same type.")
		
	)
	
	(:KEYWORDS "Flow, Logic, Case, Function"
	)
	
	(:SEE-ALSO WHEN-VALUE-OF
	)
	
	)

;==================================CONTEXT/S-OF==============================

(DOCUMENT-FUNCTION CONTEXT/S-OF

(:SUMMARY
"Returns the genes in the neighborhood of a gene, or if given
a coordinate, a description of the genes in the neighborhood of the
coordinate."
)

(:SYNTAX)

(:PARAMETERS
	(GENE-OR-COORDINATE
	:PARAMETER-TYPE required 
	:DOCSTRING "The gene or coordinate specifying the neighborhood to describe"
	)
	
	(IN
	:PARAMETER-TYPE keyword 
	:VALUE-TYPE contiguous-sequence
	:DOCSTRING "Used only when a coordinate is specified. Specifies the contiguous 
	sequence the coordinate is relative to."
	)

	(GENE-WIDTH
	:PARAMETER-TYPE keyword 
	:VALUE-TYPE number
	:DOCSTRING "When a gene is specified, number of genes to either side to be returned"
	)

	(WIDTH
	:PARAMETER-TYPE keyword 
	:VALUE-TYPE number
	:DOCSTRING "Not used yet"
	)

	(PRIMARY 
	:PARAMETER-TYPE keyword 
	:VALUE-TYPE organism
	:DOCSTRING "Used to control which genes are drawn first"
	)

	(NO-DISPLAY
	:PARAMETER-TYPE flag
	:DOCSTRING "Turns off drawing of gene map"
	)
	
	(DRAW
	:PARAMETER-TYPE flag
	:DOCSTRING "Controls graphical display of gene map"
	)
)

(:RETURNS
	"When given a gene returns:(gene[-k] gene[-(k+1)]... gene[-1] gene gene[+1]... gene[+k]).
	When given a coordinate(string frame1 frame2 number1 number2 dir)"
)

(:EXAMPLES
	(:FOO
		(:P (:B "1. If given a gene."))
		(:blockquote
			(:img :src "/weblistenerdocs/bbldf/images/CONTEXTS-OF-1.jpg")
		(:blockquote
			(:img :src "/weblistenerdocs/bbldf/images/CONTEXTS-OF-2.jpg")))
		(:P (:B "2. Utilization of flag DRAW and GENE-WIDTH."))
		(:blockquote
			(:img :src "/weblistenerdocs/bbldf/images/CONTEXTS-OF-3.jpg")
		(:blockquote
			(:img :src "/weblistenerdocs/bbldf/images/CONTEXTS-OF-4.jpg")
		(:blockquote
			(:img :src "/weblistenerdocs/bbldf/images/CONTEXTS-OF-5.jpg"))))
		(:P (:B "3. If given a genome coordinate."))
		(:blockquote
			(:img :src "/weblistenerdocs/bbldf/images/CONTEXTS-OF-6.jpg")
		(:blockquote
			(:img :src "/weblistenerdocs/bbldf/images/CONTEXTS-OF-8.jpg")
		(:blockquote
			(:img :src "/weblistenerdocs/bbldf/images/CONTEXTS-OF-7.jpg"))))
))


(:TEXT
	(:P
	(:UL 
	"CONTEXT-OF has two basic forms:"
	(:UL (:LI "(CONTEXT-OF gene)"))
	(:UL (:LI "(CONTEXT-OF coordinate IN contig)"(:BR)(:BR)))
	(:UL (:UL "If the form of CONTEXT-OF is a gene or list of genes it returns: 
	gene[-k] gene[-(k+1)]... gene[-1] gene gene[+1]... gene[+k]"))
	(:UL (:UL "In other words the k genes to the left and k genes to the right of the 
	given gene plus the gene itself"))
	(:UL (:UL (:LI " -> and <- signifies the direction of the gene (F or B)")))
    (:UL (:UL (:LI (:B "gene") 
	" is the gene frame")))
	(:UL (:UL (:LI (:B "description") 
	" is the first 30 nt of the gene's description")))
	(:UL (:UL (:LI (:B "number") 
	" is the length of the region separating the gene from
	the left-most nucleotide of the gene to the right")))
	(:UL (:UL (:LI (:B "GENE-WIDTH") 
	" k controls the value of k, default is 2" (:BR)(:BR))))

	(:UL (:UL "If the form of CONTEXT-OF is cordinate IN contig it returns: string frame1
	frame2 number1 number2 dir where string is"))
	(:UL (:UL (:LI"\"I\" if the coordinate lies within a gene")))
	(:UL (:UL (:LI" \"P\" if the coordinate lies between parallel genes")))
	(:UL (:UL (:LI" \"C\" if the coordinate lies between convergent genes")))
	(:UL (:UL (:LI " \"D\" if the coordinate lies between divergent genes")))
	(:UL (:UL (:LI " \"U\" if the coordinate lies upstream from a gene and between the 
	gene and the end of its linear contig")))
	(:UL (:UL (:LI " \"N\" if the coordinate lies downstream from a gene and between the 
	gene and the end of its linear contig")))
	(:UL (:UL (:LI "Frame1 is (if the coordinate lies within a gene) the gene, (if not) the 
	gene to the left of the coordinate. Frame2 is (if the coordinate lies within a gene) 
	NIL, (if not) the gene to the right of the coordinate")))
	(:UL (:UL (:LI "Number1 is (if the coordinate lies within a gene) the distance from 
	the coordinate to the beginning of the gene,(if not) the distance from the coordinate to the
	end of frame1")))
	(:UL (:UL (:LI "Number2 is (if the coordinate lies within a gene) the distance from the coordinate to the 
	end of the gene.(if not) the distance from the coordinate to the beginning of frame2.")))
	(:UL (:UL (:LI  (:B "Dir") 
	" is the direction of frame1")))
	(:UL (:UL (:LI " GENE-WIDTH and WIDTH are ignored" (:BR)(:BR))))

	(:UL (:LI "NO-DISPLAY prevents the display of the map"))
	(:UL (:LI "DRAW (in VPL only) causes production of a graphical map"))
	(:UL (:LI " If (CONTEXT-OF gene-list DRAW PRIMARY org) then the gene in the gene-list that's
	within the organism org is placed at the top of the list as the reference context."))
	(:UL (:LI " The reference context determines the colors."))
)))

(:SEE-ALSO gene-left-of gene-right-of gene-upstream-of gene-downstream-of)
)

;============================CONVERT=============================
(DOCUMENT-FUNCTION CONVERT

(:SUMMARY
	"Converts from one form to another"
)

(:SYNTAX
	(CONVERT [EACH] entity [TO] type [IF-POSSIBLE])
)

(:PARAMETERS
	(ENTITY 
	:PARAMETER-TYPE REQUIRED 
	:VALUE-TYPE any
	:DOCSTRING "the element/s you wish to convert"
	)
  
	(TYPE 
	:PARAMETER-TYPE REQUIRED 
	:VALUE-TYPE any
	:DOCSTRING "the final format you wish the results to be in"
	)
  

	(EACH 
	:PARAMETER-TYPE TOKEN
	:DOCSTRING "will convert each entity"
	)
  

	(TO 
	:PARAMETER-TYPE TOKEN 
	)
	
 

	(IF-POSSIBLE 
	:PARAMETER-TYPE FLAG 
	:DOCSTRING "Currently not in use"
	)
)

(:RETURNS "the formatted element/s, if the conversion is possible"
)

(:EXAMPLES
	(:FOO 
		(:p (:b "1. Conversion of number to a string."))
		(:blockquote
		(:img :src "/weblistenerdocs/bbldf/images/CONVERT-1.jpg")
		(:blockquote
		(:img :src "/weblistenerdocs/bbldf/images/CONVERT-2.jpg")))
		(:p (:b "2. Conversion of string to number."))
		(:blockquote
		(:img :src "/weblistenerdocs/bbldf/images/CONVERT-3.jpg")
		(:blockquote
		(:img :src "/weblistenerdocs/bbldf/images/CONVERT-4.jpg")))
		(:p (:b "3. Conversion of table to list."))
		(:blockquote
		(:img :src "/weblistenerdocs/bbldf/images/CONVERT-5.jpg")
		(:blockquote
		(:img :src "/weblistenerdocs/bbldf/images/CONVERT-6.jpg")))
		(:p (:b "Conversion that is not defined"))
		(:blockquote
		(:img :src "/weblistenerdocs/bbldf/images/CONVERT-7.jpg")
		(:blockquote
		(:img :src "/weblistenerdocs/bbldf/images/CONVERT-8.jpg")))
	)
)

(:Text
	(:p "Below is a listing of possible conversions:")
	(:p (:li "number to integer"))
	(:p (:li "number to string"))
	(:p (:li "positive-number to integer"))
	(:p (:li "symbol to string"))
	(:p (:li "character/s to string/s"))
	(:p (:li "protein to gene"))
	(:p (:li "gene to protein"))
	(:p (:li "gene to string"))
	(:p (:li "gene to organism"))
	(:p (:li "gene to contiguous-sequence"))
	(:p (:li "protein to string"))
	(:p (:li "protein to gene"))
	(:p (:li "protein to organism"))
	(:p (:li "protein to contiguous-sequence"))
	(:p (:li "contiguous-sequence to string"))
	(:p (:li "contiguous-sequence to organism")) 
	(:p (:li "organism to string"))
	(:p (:li "labeled-sequence/s to string/s"))
	(:p (:li "string to number"))
	(:p (:li "table to list"))
	(:p (:li "table to simple-list"))
	(:p (:li "frame to string"))
)

(:KEYWORDS "Switch, Change, Alter"
)

(:SEE-ALSO CONVERT-AMINO-ACID/S
)
) 



;==================================CONVERT-AMINO-ACID/S=====================

(DOCUMENT-FUNCTION CONVERT-AMINO-ACID/S


(:SUMMARY
	"Converts amino acid to symbol, name, or codons"
)

(:Syntax
	(CONVERT-AMINO-ACID/S amino-acid [TO-1-LETTER] [TO-3-LETTER] [TO-FULL-NAME] [TO-DNA-CODON-LIST] [TO-RNA-CODON-LIST] [SINGLE-AA] [SEQUENCE])
)

(:PARAMETERS
	(AMINO-ACID 
	:PARAMETER-TYPE REQUIRED 
	:VALUE-TYPE STRING\,SYMBOL
	:DOCSTRING "The amino acid/s under consideration."
	)

	(TO-1-LETTER 
	:PARAMETER-TYPE :FLAG
	:DOCSTRING "Converts to the one letter abbreviation."
	)

	(TO-3-LETTER 
	:PARAMETER-TYPE :FLAG
	:DOCSTRING "Converts to the three letter abbreviation."
	)
 
	(TO-FULL-NAME 
	:PARAMETER-TYPE :FLAG
	:DOCSTRING "Converts a one or three letter abbreviation to the full name."
	)

	(TO-DNA-CODON-LIST 
	:PARAMETER-TYPE  :FLAG
	:DOCSTRING "Converts to the codons used in DNA replication."
	)
 
	(TO-RNA-CODON-LIST 
	:PARAMETER-TYPE :FLAG 
	:DOCSTRING "Converts to the codons used in RNA synthesis."
	)

	(SINGLE-AA 
	:PARAMETER-TYPE :FLAG
	:DOCSTRING "Specify this if the argument is intended to be interpreted as 
	a single amino acid."
	)
 
	(SEQUENCE 
	:PARAMETER-TYPE :FLAG
	:DOCSTRING "Specify this if the argument is intended to be interpreted as a sequence of multiple
	amino acids."
	))
  
(:RETURNS " An object of type LIST."
)

(:EXAMPLES
	(:FOO
		(:p
			(:p (:b "1. A series of examples demonstrating options."))
            (:blockquote
      	      (:img :src "/weblistenerdocs/bbldf/images/CONVERT-AA-1.jpg")
			(:blockquote
                (:img :src "/weblistenerdocs/bbldf/images/CONVERT-AA-2.jpg")))
			(:blockquote
      	      (:img :src "/weblistenerdocs/bbldf/images/CONVERT-AA-5.jpg")
			(:blockquote
                (:img :src "/weblistenerdocs/bbldf/images/CONVERT-AA-2.jpg")))
			(:blockquote
      	      (:img :src "/weblistenerdocs/bbldf/images/CONVERT-AA-6.jpg")
			(:blockquote
                (:img :src "/weblistenerdocs/bbldf/images/CONVERT-AA-7.jpg")))
			(:blockquote
      	      (:img :src "/weblistenerdocs/bbldf/images/CONVERT-AA-8.jpg")
			(:blockquote
                (:img :src "/weblistenerdocs/bbldf/images/CONVERT-AA-9.jpg")))
			(:blockquote
      	      (:img :src "/weblistenerdocs/bbldf/images/CONVERT-AA-10.jpg")
			(:blockquote
                (:img :src "/weblistenerdocs/bbldf/images/CONVERT-AA-11.jpg")))
			(:blockquote
      	      (:img :src "/weblistenerdocs/bbldf/images/CONVERT-AA-12.jpg")
			(:blockquote
                (:img :src "/weblistenerdocs/bbldf/images/CONVERT-AA-2.jpg")))
			(:blockquote
      	      (:img :src "/weblistenerdocs/bbldf/images/CONVERT-AA-3.jpg")
			(:blockquote
                (:img :src "/weblistenerdocs/bbldf/images/CONVERT-AA-4.jpg")))
		)
	)
)

(:KEYWORDS "Convert, Amino acid, Conversion, Codon, DNA, RNA"
)

(:SEE-ALSO NAME-OF AMINO-ACID-FREQUENCIES-OF AMINO-ACID-COUNTS-OF CODON-FREQUENCIES-OF CODON-FREQUENCY-COMPARISON
)
)

;==================================COS==========================
(DOCUMENT-FUNCTION COS

(:SUMMARY "Returns the cosine (cos)")

(:SYNTAX (COS n))

(:PARAMETERS
(n 
:VALUE-TYPE number 
:DOCSTRING "the angle, in radians, you want the cosine of")
)

(:RETURNS " a number")

(:EXAMPLES
	(:FOO
		(:P (:B "A set of simple examples"))
		(:blockquote
      	      (:img :src "/weblistenerdocs/bbldf/images/COS-1.jpg")
	      (:blockquote
                (:img :src "/weblistenerdocs/bbldf/images/COS-2.jpg")))
		(:blockquote
      	      (:img :src "/weblistenerdocs/bbldf/images/COS-3.jpg")
	      (:blockquote
                (:img :src "/weblistenerdocs/bbldf/images/COS-4.jpg")))
		(:p "Note: The \"d\" in the next example indicates an exponent. See Representations 
		of Numbers for more explanation.")
		(:blockquote
      	      (:img :src "/weblistenerdocs/bbldf/images/COS-5.jpg")
	      (:blockquote
                (:img :src "/weblistenerdocs/bbldf/images/COS-6.jpg")))
		(:blockquote
      	      (:img :src "/weblistenerdocs/bbldf/images/COS-7.jpg")
	      (:blockquote
                (:img :src "/weblistenerdocs/bbldf/images/COS-9.jpg")))
				
))

(:TEXT
(:p "This function returns the cosine of the angle provided in radians.")
(:p "The angle provided is usually between 0 and 2PI. (2PI = 6.28319)")
(:p "The cosine will be between 0 and 1.")
)
(:SEE-ALSO SIN TAN ACOS ASIN ATAN)
)
;=================================COUNT-OF================================
(DOCUMENT-FUNCTION COUNT-OF

(:SUMMARY
	"Returns the number of times the query appears in the target"
)

(:SYNTAX
	(COUNT-OF [EACH] [NOT] query [CASE-SENSITIVE] [CASE-INSENSITIVE] [BOTH-STRANDS] [LABELED] [IN string-or-list-or-gene-or-protein-or-contiguous-sequence-or-organism-or-labeled-sequence] [IN-EACH string-or-list-or-gene-or-protein-or-contiguous-sequence-or-organism-or-labeled-sequence])
)

(:PARAMETERS
	(QUERY 
	:PARAMETER-TYPE REQUIRED
	:VALUE-TYPE any   
	:DOCSTRING "the element that you must find"
	)

	(EACH
	:PARAMETER-TYPE TOKEN  
	:DOCSTRING "will return a list with the count of each element of a provided list"
	)

	(NOT 
	:PARAMETER-TYPE TOKEN   
	:DOCSTRING "will count all elements except specified value"
	)

	(CASE-SENSITIVE 
	:PARAMETER-TYPE FLAG   
	:DOCSTRING "will consider the case of the element searching for"
	)

	(CASE-INSENSITIVE 
	:PARAMETER-TYPE FLAG   
	:DOCSTRING "will consider the case of the element searching for"
	)

	(BOTH-STRANDS 
	:PARAMETER-TYPE FLAG
	:DOCSTRING "Considers the count of the query in both strands"
	)

	(LABELED 
	:PARAMETER-TYPE FLAG
	:DOCSTRING "labels the results with the entity that was counted"
	)

	(IN 
	:PARAMETER-TYPE KEYWORD  
	:VALUE-TYPE STRING\,LIST\,GENE\,PROTEIN\,CONTIGUOUS-SEQUENCE\,ORGANISM\,LABELED-SEQUENCE  
	:DOCSTRING "the entity that contains the elements your are counting"
	)

	(IN-EACH 
	:PARAMETER-TYPE KEYWORD 
	:VALUE-TYPE STRING\,LIST\,GENE\,PROTEIN\,CONTIGUOUS-SEQUENCE\,ORGANISM\,LABELED-SEQUENCE  
	:DOCSTRING "a list of entities that contain the element you are counting"
	))

(:RETURNS
	"An object of type LIST or NONNEGATIVE-NUMBER"
)

(:Examples

	(:FOO
		(:P (:B "1. Example returns the number of \"G\"'s IN pNpA"))
		(:blockquote
      	      (:img :src "/weblistenerdocs/bbldf/images/COUNT-OF-1.jpg")
	      (:blockquote
                (:img :src "/weblistenerdocs/bbldf/images/COUNT-OF-2.jpg")))
		(:P (:B "2. Another example"))
		(:blockquote
      	      (:img :src "/weblistenerdocs/bbldf/images/COUNT-OF-3.jpg")
	      (:blockquote
                (:img :src "/weblistenerdocs/bbldf/images/COUNT-OF-4.jpg")))
		(:P (:B "3. Counting a query in a list of entities"))
		(:blockquote
      	      (:img :src "/weblistenerdocs/bbldf/images/COUNT-OF-5.jpg")
	      (:blockquote
                (:img :src "/weblistenerdocs/bbldf/images/COUNT-OF-6.jpg")))
		(:P (:B "4.  Example utilizing flags "))
		(:blockquote
      	      (:img :src "/weblistenerdocs/bbldf/images/COUNT-OF-7.jpg")
	      (:blockquote
                (:img :src "/weblistenerdocs/bbldf/images/COUNT-OF-8.jpg")))
		(:blockquote
      	      (:img :src "/weblistenerdocs/bbldf/images/COUNT-OF-9.jpg")
	      (:blockquote
                (:img :src "/weblistenerdocs/bbldf/images/COUNT-OF-10.jpg")))
		(:blockquote
      	      (:img :src "/weblistenerdocs/bbldf/images/COUNT-OF-11.jpg")
	      (:blockquote
                (:img :src "/weblistenerdocs/bbldf/images/COUNT-OF-12.jpg")))
		(:P (:B "5. Simple function followed by usage of NOT"))
		(:blockquote
      	      (:img :src "/weblistenerdocs/bbldf/images/COUNT-OF-13.jpg")
	      (:blockquote
                (:img :src "/weblistenerdocs/bbldf/images/COUNT-OF-14.jpg")))
		(:blockquote
      	      (:img :src "/weblistenerdocs/bbldf/images/COUNT-OF-15.jpg")
	      (:blockquote
                (:img :src "/weblistenerdocs/bbldf/images/COUNT-OF-16.jpg")))
		(:P (:B "5. Counting mutiple elements"))
		(:blockquote
      	      (:img :src "/weblistenerdocs/bbldf/images/COUNT-OF-17.jpg")
	      (:blockquote
                (:img :src "/weblistenerdocs/bbldf/images/COUNT-OF-18.jpg")))
		
))
(:TEXT
	(:P (:UL

(:li "Returns the number of times the query appears in the target."(:br)(:br))

(:li "If no target is given, then the number of elements in the list is returned 
(and if the object of COUNT-OF is not a list, an error is created)."(:br)(:br))

(:li "Query may be a sequence that appears in a larger sequence, a string that appears 
in a larger string, or an item that appears in a list or a list of sequences or items."(:br)(:br))

(:li "Target may be a sequence or anything that has a sequence (e.g. an organism) or a list."(:br)(:br))

(:li "When IN-LIST is used, the list is searched for all instances of query."(:br)(:br))

(:li "When IN-EACH is used, each element of the list is searched for all instances of query."(:br)(:br))

(:li "IN is synonymous with IN-LIST.")))
)


(:KEYWORDS "Count"
)

(:SEE-ALSO
  COUNTS-OF LENGTH-OF FOR-EACH FROM
))

;============================COUNTS-OF===================================
(DOCUMENT-FUNCTION COUNTS-OF (a.k.a. COUNTS-OF-EACH COUNT-OF-EACH)


(:SUMMARY
	"Returns the number of times the query or queries appear in the target or targets"
)

(:SYNTAX
(COUNTS-OF [EACH] [NOT] query [CASE-SENSITIVE] [CASE-INSENSITIVE] [BOTH-STRANDS] [LABELED] [IN string-or-list-or-gene-or-protein-or-contiguous-sequence-or-organism-or-labeled-sequence] [IN-EACH list-or-organism])

(COUNTS-OF-EACH [EACH] [NOT] query [CASE-SENSITIVE] [CASE-INSENSITIVE] [BOTH-STRANDS] [LABELED] [IN string-or-list-or-gene-or-protein-or-contiguous-sequence-or-organism-or-labeled-sequence] [IN-EACH list-or-organism])

(COUNT-OF-EACH [EACH] [NOT] query [CASE-SENSITIVE] [CASE-INSENSITIVE] [BOTH-STRANDS] [LABELED] [IN string-or-list-or-gene-or-protein-or-contiguous-sequence-or-organism-or-labeled-sequence] [IN-EACH list-or-organism])
)

(:PARAMETERS

(QUERY
:PARAMETER-TYPE REQUIRED  
:VALUE-TYPE any
:DOCSTRING "The elements you wish to count"
)   

(EACH 
:PARAMETER-TYPE TOKEN 
:DOCSTRING "Will count each element specified"
)  
  
(NOT 
:PARAMETER-TYPE TOKEN 
:DOCSTRING "will count all elements except specified value"
)  
 
(CASE-SENSITIVE 
:PARAMETER-TYPE FLAG 
:DOCSTRING "will consider the case of the element searching for"
)  
 
(CASE-INSENSITIVE 
:PARAMETER-TYPE FLAG
:DOCSTRING "will consider the case of the element searching for"
)   
 
(BOTH-STRANDS 
:PARAMETER-TYPE FLAG
:DOCSTRING "Considers the count of the query in both strands"
)  
  
(LABELED 
:PARAMETER-TYPE FLAG
:DOCSTRING "labels the results with the entity that was counted"
)   

(IN 
:PARAMETER-TYPE KEYWORD 
:VALUE-TYPE STRING\,LIST\,GENE\,PROTEIN\,CONTIGUOUS-SEQUENCE\,ORGANISM\,LABELED-SEQUENCE
:DOCSTRING "the entity that contains the elements your are counting"
)  
  
(IN-EACH 
:PARAMETER-TYPE KEYWORD 
:VALUE-TYPE LIST\,ORGANISM
:DOCSTRING "a list of entities that contain the element you are counting"
))  

(:RETURNS
	"An object of type NONNEGATIVE-NUMBER or LIST"
)

(:EXAMPLES
	(:FOO
		(:P (:B "1. Example returns the number of \"G\"'s and \"A\"'s IN pNpA"))
		(:blockquote
      	      (:img :src "/weblistenerdocs/bbldf/images/COUNTS-OF-1.jpg")
	      (:blockquote
                (:img :src "/weblistenerdocs/bbldf/images/COUNTS-OF-2.jpg")))
		(:P (:B "2. Usage of flag LABELED"))
		(:blockquote
      	      (:img :src "/weblistenerdocs/bbldf/images/COUNTS-OF-3.jpg")
	      (:blockquote
                (:img :src "/weblistenerdocs/bbldf/images/COUNTS-OF-4.jpg")))
		(:P (:B "3. Counting multiple elements in a list of entities"))
		(:blockquote
      	      (:img :src "/weblistenerdocs/bbldf/images/COUNT-OF-5.jpg")
	      (:blockquote
                (:img :src "/weblistenerdocs/bbldf/images/COUNT-OF-6.jpg")))
		
))

(:KEYWORDS "Count, quantify, tally, sum, total")

(:SEE-ALSO COUNT-OF
))



