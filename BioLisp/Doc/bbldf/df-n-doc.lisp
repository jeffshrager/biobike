;;; -*- Package: bbi; mode: lisp; base: 10; Syntax: Common-Lisp; -*-

(in-package :bbi)

;;; +=========================================================================+
;;; | Copyright (c) 2002, 2003, 2004 JP Massar, Jeff Shrager, Mike Travers    |
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

;;; Authors: Victor Clarke, Joe Anderson, Jeff Elhai, Bogdan Mihai, Craig Noe
;;;          Arnaud Taton

; ================= NAME-OF ====================
	(DOCUMENT-FUNCTION NAME-OF
		(:SUMMARY
			"Provides simple name of entity." 
		)
		(:SYNTAX
			(NAME-OF entity [SHORT])
			(NAMES-OF entity [SHORT])
		)
		(:PARAMETERS
			(entity
				:DOCSTRING "Gene, protein, organism or contig (incl. 
				chromosome or replicon) to be evaluated."
				:PARAMETER-TYPE required
				:VALUE-TYPE GENE\,PROTEIN\,CONTIGUOUS-SEQUENCE\,ORGANISM\,LABELED-SEQUENCE\,STRING
			)
		)
		(:RETURNS
			"A name of type LIST or STRING if there is a match.  Otherwise an error
			will appear if there is no match."
		)
		(:EXAMPLES
			"Detailed below, graphically."
		)
		(:TEXT
			(:p (:ul (:li "Few genomes are not yet completed or the names of these 
			genomes are not yet defined in BioBIKE. For these genomes the function 
			returns the contigs.")
			(:li "The given entity may be a gene, a protein, an organism, or the 
			name itself. A simple or a complex list of these entities 
			is also valid.")))
		
			(:p (:b "How to use this function."))
			(:p (:b (:u "Single entity:")))
			(:p "1. A gene:")
			(:img :src "/weblistenerdocs/bbldf/images/Name-Of_1.jpg")
			(:p "")
			(:ul (:img :src "/weblistenerdocs/bbldf/images/Name-Of_1_Result.jpg"))
			(:p "2. A protein:")
			(:img :src "/weblistenerdocs/bbldf/images/Name-Of_2.jpg")
			(:p "")
			(:ul (:img :src "/weblistenerdocs/bbldf/images/Name-Of_2_Result.jpg"))
			(:p "3. An organism:")
			(:img :src "/weblistenerdocs/bbldf/images/Name-Of_3.jpg")
			(:p "")
			(:ul (:img :src "/weblistenerdocs/bbldf/images/Name-Of_3_Result.jpg"))
			(:p "4. A chromosome:")
			(:img :src "/weblistenerdocs/bbldf/images/Name-Of_4.jpg")
			(:p "")
			(:ul (:img :src "/weblistenerdocs/bbldf/images/Name-Of_4_Result.jpg"))
			
			(:p (:b (:u "A list of entities:")))
			(:p "1. Creates a list of orthologs of p-all5306 then finds the
			names for that list.")
			(:img :src "/weblistenerdocs/bbldf/images/Name-Of_5.jpg")
			(:p "")
			(:ul (:img :src "/weblistenerdocs/bbldf/images/Name-Of_5_Result.jpg"))
		)
		(:KEYWORDS
			Name Gene Protein Contig
		)
		(:SEE-ALSO
			CHROMOSOME-OF CONTIGS-OF GENE-OF GENES-OF HOMOLOG-OF* PROTEIN-OF
			REPLICON-OF SEQUENCE-OF
		)
	)

; ================= NEGATION-OF ====================
	(DOCUMENT-FUNCTION BBL::NEGATION-OF
		(:SUMMARY
			"Returns the negation of a number or each of a list of numbers."
		)
		(:SYNTAX
			(NEGATION-OF (NUMBER))
		)
		(:PARAMETERS
			(number
				:DOCSTRING "The number(s) to be changed."
				:PARAMETER-TYPE required
				:VALUE-TYPE number\,list
			)
		)
		(:RETURNS
			"The negation of a number or list of numbers."
		)
		(:EXAMPLES
			"Detailed below, graphically."
		)
		(:TEXT
			(:p (:b "1. This function can negate a list of positive numbers."))
			(:img :src "/weblistenerdocs/bbldf/images/Negation_Postive.jpg")
			(:p "")
			(:ul (:img :src "/weblistenerdocs/bbldf/images/Negation_Postive_Result.jpg"))
			
			(:p (:b "2. This function can negate a list of negative numbers."))
			(:img :src "/weblistenerdocs/bbldf/images/Negation_Negative.jpg")
			(:p "")
			(:ul (:img :src "/weblistenerdocs/bbldf/images/Negation_Negative_Result.jpg"))
		
			(:p (:b "3. This function can negate a list of positive and negative numbers."))
			(:img :src "/weblistenerdocs/bbldf/images/Negation_Positive_Negative.jpg")
			(:p "")
			(:ul (:img :src "/weblistenerdocs/bbldf/images/Negation_Positive_Negative_Result.jpg"))
		)
		(:KEYWORDS
			Negation Opposite Number Negative Positive
		)
		(:SEE-ALSO
			NUMBER-LIST RANDOM-NUMBER RANDOM-INTEGER
		)
	)

	; ================= NEW-TABLE ====================
	(DOCUMENT-FUNCTION NEW-TABLE
		(:SUMMARY
			"Creates a table according to users specifications."
		)
		(:SYNTAX
			(NEW-TABLE specs [NOT-ADJUSTABLE] [INITIALIZE any])
		)
		(:PARAMETERS
			(specs
				:DOCSTRING "Number of columns and number of rows; format (columns rows)"
				:PARAMETER-TYPE required
				:VALUE-TYPE list
			)
			(not-adjustable
				:DOCSTRING "An option to restrict the change of the size of the
				table."
				:PARAMETER-TYPE :FLAG
			)
			(initialize
				:DOCSTRING "The intial values of the table components."
				:PARAMETER-TYPE keyword
				:VALUE-TYPE any
			)
		)
		(:RETURNS)
		(:EXAMPLES
			"Detailed below, graphically."
		)
		(:TEXT
			(:p (:b "1. An non-intialized table"))
			(:img :src "/weblistenerdocs/bbldf/images/New-Table_1.jpg")
			(:p (:li (:i "will default all empty components with NIL.")))
			(:ul (:img :src "/weblistenerdocs/bbldf/images/New-Table_1.1.jpg"))
			(:ul (:img :src "/weblistenerdocs/bbldf/images/New-Table_1.2.jpg"))
			
			(:p (:b "2. An intialized table with 2"))
			(:img :src "/weblistenerdocs/bbldf/images/New-Table_2.jpg")
			(:p (:li (:i "will default all empty components with the number 2.")))
			(:ul (:img :src "/weblistenerdocs/bbldf/images/New-Table_2.1.jpg"))
			(:ul (:img :src "/weblistenerdocs/bbldf/images/New-Table_2.2.jpg"))
			
			(:p (:b "3. An intialized table with the sequence of pcc7120 from 1 to
			3"))
			(:img :src "/weblistenerdocs/bbldf/images/New-Table_3.jpg")
			(:p (:li (:i "will fill each empty component with the first three 
			nucleotides of each sequence defined by pcc7120.")))
			(:ul (:img :src "/weblistenerdocs/bbldf/images/New-Table_3.1.jpg"))
			(:ul (:img :src "/weblistenerdocs/bbldf/images/New-Table_3.2.jpg"))
		)
		(:KEYWORDS
			Table Matrice
		)
		(:SEE-ALSO
			LABELS-OF ASSIGN IS-TABLE? TRANSPOSE-LIST DISPLAY-TABLE
		)
	)

	; ================= NINTH ====================
	(DOCUMENT-FUNCTION BBL::NINTH
		(:SUMMARY
			"Returns the ninth item of a list or ninth character of a string."
		)
		(:SYNTAX)
		(:PARAMETERS
			(entity
				:DOCSTRING "The list or string in which the user is seeking the 
				ninth element."
				:PARAMETER-TYPE required
				:VALUE-TYPE list
			)
			(in
				:DOCSTRING "Will take the ninth element of a string or a list, 
				ignoring the internal structure of the list."
				:PARAMETER-TYPE :TOKEN
			)
			(in-each
				:DOCSTRING "When used on a list of strings, the ninth element of each
				string is returned; when used on a list of lists, the ninth element 
				of each list is returned; (if a list contains strings and lists, the 
				ninth element of each is returned)."
				:PARAMETER-TYPE :TOKEN
			)
			(nonstrict
				:DOCSTRING "If a list does not have at least 9 items, NIL will be 
				returned."
				:PARAMETER-TYPE :FLAG
			)
			(strict
				:DOCSTRING "(default) Will return an error if this is used and 
				there are not at least 9 items in each list."
				:PARAMETER-TYPE :FLAG
			)
		)
		(:RETURNS
			"The ninth object of any type."
		)
		(:EXAMPLES
			"Detailed below, graphically."
		)
		(:TEXT
			(:p (:b "1. A list of numbers."))
			(:img :src "/weblistenerdocs/bbldf/images/Ninth_1.jpg")
			(:p "")
			(:ul (:img :src "/weblistenerdocs/bbldf/images/Ninth_1.1.jpg"))
			
			(:p (:b "2. A list of letters."))
			(:img :src "/weblistenerdocs/bbldf/images/Ninth_1.jpg")
			(:p "")
			(:ul (:img :src "/weblistenerdocs/bbldf/images/Ninth_1.1.jpg"))
			
			(:p (:b "3a. A list of replicons using 'STRICT'."))
			(:img :src "/weblistenerdocs/bbldf/images/Ninth_1.jpg")
			(:p (:li (:i "This causes an error because some organisms don't have at 
			least nine replicons so there is not a ninth element in the list to 
			return.")))
			(:ul (:img :src "/weblistenerdocs/bbldf/images/Ninth_1.1.jpg"))
			
			(:p (:b "3b. To fix this error 'NONSTRICT' is used."))
			(:img :src "/weblistenerdocs/bbldf/images/Ninth_1.jpg")
			(:p (:li (:i "This allows for any organism that does not have nine 
			replicons to return " (:b "NIL") ".")))
			(:ul (:img :src "/weblistenerdocs/bbldf/images/Ninth_1.1.jpg"))
		)
		(:KEYWORDS
			Ninth Element
		)
		(:SEE-ALSO
			BBL::FIRST BBL::SECOND BBL::THIRD BBL::FOURTH BBL::FIFTH BBL::SIXTH 
			BBL::SEVENTH BBL::EIGHTH BBL::TENTH
		)
	)

	; ================= NONCODING-GENES-OF ====================
	(DOCUMENT-FUNCTION NONCODING-GENES-OF
		(:SUMMARY
			"Returns those genes that do not encode proteins."
		)
		(:SYNTAX
			(NONCODING-GENES-OF entity)
		)
		(:PARAMETERS
			(entity
				:DOCSTRING "Organism, replicon, or gene list in which noncoding genes
				are sought."
				:PARAMETER-TYPE required
				:VALUE-TYPE GENE\,CONTIGUOUS-SEQUENCE\,ORGANISM\,NULL
			)
		)
		(:RETURNS
			"An object of type LIST, GENE, or NILL."
		)
		(:EXAMPLES
			"Detailed below, graphically."
			
			"> 4. Illustrate within a 'complex' code
			(ASSIGN complete-genomes 
			AS (FOR-EACH organism IN *all-organisms*
                       WHEN (EQUAL organism[.completed] T)
                       COLLECT organism))
			--> (#$synechococcus_elongatus_pcc7942 #$prochlorococcus_marinus_mit9313
				#$synechocystis_pcc6803 #$nostoc_punctiforme_atcc29133
				#$anabaena_pcc7120 #$prochlorococcus_marinus_ss120
				#$synechococcus_wh8102 #$gloeobacter_violaceus_pcc7421
				#$thermosynechococcus_elongatus_bp1 #$prochlorococcus_marinus_med4)
			(FOR-EACH replicon IN (FLATTEN (REPLICONS-OF complete-genomes))
				AS coding-only-replicon = (IF-TRUE (NONCODING-GENES-OF replicon)
                                        THEN NIL
                                        ELSE replicon)
				COLLECT coding-only-replicon)
			--> (NIL #$S7942.pANL #$S7942.pANS NIL NIL #$S6803.pSYSA #$S6803.pSYSG
				#$S6803.pSYSM #$S6803.pSYSX #$S6803.pCC5.2 #$S6803.pCA2.4
				#$S6803.pCB2.4 NIL #$Npun.pNpA #$Npun.pNpB #$Npun.pNpC #$Npun.pNpD
				#$Npun.pNpE NIL #$A7120.pALPHA #$A7120.pBETA #$A7120.pGAMMA NIL
				#$A7120.pEPSILON #$A7120.pZETA NIL NIL NIL NIL NIL)"
		)
		(:TEXT
			(:p (:b "Several genes in genomes do not encode proteins, 
			NONCODING-GENES-OF returns these genes."))
		
			(:p (:b "1a. If given an organism or a replicon."))
			(:img :src "/weblistenerdocs/bbldf/images/Noncoding-Genes-Of_1a.jpg")
			(:p "")
			(:ul (:img :src "/weblistenerdocs/bbldf/images/Noncoding-Genes-Of_1a.1.jpg"))
			
			(:p (:b "1b."))
			(:img :src "/weblistenerdocs/bbldf/images/Noncoding-Genes-Of_1b.jpg")
			(:p "")
			(:ul (:img :src "/weblistenerdocs/bbldf/images/Noncoding-Genes-Of_1b.1.jpg"))
			
			(:p (:b "2a. If given a list of genes, organisms or replicons."))
			(:img :src "/weblistenerdocs/bbldf/images/Noncoding-Genes-Of_2a.jpg")
			(:p "")
			(:ul (:img :src "/weblistenerdocs/bbldf/images/Noncoding-Genes-Of_2a.1.jpg"))
			(:p "")
			(:ul (:img :src "/weblistenerdocs/bbldf/images/Noncoding-Genes-Of_2a.2.jpg"))
			(:p (:li "Next, the function " (:b "PREVIOUS-RESULT") " is used to find the 
			noncoding genes. Also, an astrix (*) can be used in place of this 
			function to obtain the same results."))
			(:img :src "/weblistenerdocs/bbldf/images/Noncoding-Genes-Of_2a.3.jpg")
			(:p "")
			(:ul (:img :src "/weblistenerdocs/bbldf/images/Noncoding-Genes-Of_2a.4.jpg"))
			
			(:p (:b "2b."))
			(:img :src "/weblistenerdocs/bbldf/images/Noncoding-Genes-Of_2b.jpg")
			(:p "")
			(:ul (:img :src "/weblistenerdocs/bbldf/images/Noncoding-Genes-Of_2b.1.jpg"))
		)
		(:KEYWORDS
			Noncoding Gene Protein
		)
		(:SEE-ALSO
			CODING-GENES-OF GENES-OF
		)
	)	
	
	; ================= NUCLEOTIDE-DISTANCE ====================
	(DOCUMENT-FUNCTION NUCLEOTIDE-DISTANCE
		(:SUMMARY
			"Calculates number of nucleotides between coordinates of a possibly 
			circular sequence."
		)
		(:SYNTAX
			(NUCLEOTIDE-DISTANCE [FROM] start [TO] end [IN] contig [TRUNCATE] [WRAP])
		)
		(:PARAMETERS
			(start
				:DOCSTRING "The first nucleotide position to begin from."
				:PARAMETER-TYPE required
				:VALUE-TYPE number
			)
			(end
				:DOCSTRING "The last nucleotide position to end at."
				:PARAMETER-TYPE required
				:VALUE-TYPE number
			)
			(contig
				:DOCSTRING "The contig or organism that the nucleotide distance 
				is to calculate."
				:PARAMETER-TYPE required
				:VALUE-TYPE CONTIGUOUS-SEQUENCE\,ORGANISM
			)
			(truncate
				:DOCSTRING "This option is applicable only to linear sequences 
				(i.e. contigs). When this option is specified, coordinates less 
				than 1 or greater than the length of the sequence are forced into the 
				sequence's range by setting coordinates that are too low to 1 and 
				those that are too high to the length of the sequence."
				:PARAMETER-TYPE :FLAG
			)
			(wrap
				:DOCSTRING "This option is applicable only to circular sequences 
				(i.e. replicons). When this option is specified, coordinates less 
				than 1 or greater than the length of the sequence are forced into the 
				sequence's range by wrapping around the circle."
				:PARAMETER-TYPE :FLAG
			)
		)
		(:RETURNS
			"The distance from the start position nucleotide to end position
			nucleotide."
		)
		(:EXAMPLES
			"Detailed below, graphically."
		)
		(:TEXT
			(:p (:b "1. Starting from the first nucleotide and ending at the 10001th
			nucleotide of the contig sequence."))
			(:img :src "/weblistenerdocs/bbldf/images/Nucleotide-Distance_1.jpg")
			(:p "")
			(:ul (:img :src "/weblistenerdocs/bbldf/images/Nucleotide-Distance_1.1.jpg"))
			
			(:p (:b "2. Starting from the 1000th nucleotide and ending at the first
			nucleotide of the contig sequence."))
			(:img :src "/weblistenerdocs/bbldf/images/Nucleotide-Distance_2.jpg")
			(:p "")
			(:ul (:img :src "/weblistenerdocs/bbldf/images/Nucleotide-Distance_2.1.jpg"))

			(:p (:b "3. A loop can be run if there is a list of contigs within an
			organism."))
			(:img :src "/weblistenerdocs/bbldf/images/Nucleotide-Distance_3.jpg")
			(:p (:ul (:li (:i "This results in the names of the contigs and nucleotide 
			distances."))))
			(:ul (:img :src "/weblistenerdocs/bbldf/images/Nucleotide-Distance_3.1.jpg"))
		)
		(:KEYWORDS
			Nucleotide Distance Start End
		)
		(:SEE-ALSO
			CONTIGS-OF ORGANISM-OF
		)
	)

	; ================= NUMBER-LIST ====================
	(DOCUMENT-FUNCTION BBL::NUMBER-LIST 
		(:SUMMARY
			"Returns a list of numbers from NUM1 to NUM2."
		)
		(:SYNTAX)
		(:PARAMETERS
			(num1
				:DOCSTRING "The first number in the list of numbers to start from."
				:PARAMETER-TYPE required
				:VALUE-TYPE number
			)
			(num2
				:DOCSTRING "The last number in the list of numbers to end at."
				:PARAMETER-TYPE required
				:VALUE-TYPE number
			)
			(limit
				:DOCSTRING "Restricts the length of the list of numbers."
				:PARAMETER-TYPE keyword
				:VALUE-TYPE number
			)
			(by
				:DOCSTRING "Restricts the numbers returned within the list of numbers
				with incrementation." 
				:PARAMETER-TYPE keyword
				:VALUE-TYPE number
			)
		)
		(:RETURNS
			"A list of numbers or objects."
		)
		(:EXAMPLES
			"Detailed below, graphically."
		)
		(:TEXT
			(:p (:b "1."))
			(:img :src "/weblistenerdocs/bbldf/images/Number-List_1.jpg")
			(:p "")
			(:ul (:img :src "/weblistenerdocs/bbldf/images/Number-List_1.1.jpg"))
			
			(:p (:b "2."))
			(:img :src "/weblistenerdocs/bbldf/images/Number-List_2.jpg")
			(:p "")
			(:ul (:img :src "/weblistenerdocs/bbldf/images/Number-List_2.1.jpg"))
			
			(:p (:b "3."))
			(:img :src "/weblistenerdocs/bbldf/images/Number-List_3.jpg")
			(:p "")
			(:ul (:img :src "/weblistenerdocs/bbldf/images/Number-List_3.1.jpg"))
			
			(:p (:b "4."))
			(:img :src "/weblistenerdocs/bbldf/images/Number-List_4.jpg")
			(:p "")
			(:ul (:img :src "/weblistenerdocs/bbldf/images/Number-List_4.1.jpg"))
			
			(:p (:b "5. If you have list of coding genes within a table and only
			want to display particular genes with their lengths."))
			(:img :src "/weblistenerdocs/bbldf/images/Number-List_5.jpg")
			(:p (:ul (:li (:i "This results in displaying every 10th coding gene
			starting from the first."))))
			(:ul (:img :src "/weblistenerdocs/bbldf/images/Number-List_5.1.jpg"))
		)
		(:KEYWORDS
			List Numbers
		)
		(:SEE-ALSO
			RANDOM-INTEGER RANDOM-NUMBER IS-NUMBER?
		)
	)