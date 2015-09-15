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

;;; Authors: Joe Anderson, Victor Clarke, Jeff Elhai, Bogdan Mihai, Craig Noe
;;;          Arnaud Taton, Robel Wolde

;================================= ORDER ==============================
(DOCUMENT-FUNCTION BBL::ORDER
	(:SUMMARY "This function compares values that are numbers and/or strings.")
	(:SYNTAX )
	(:PARAMETERS
		(any 
			:DOCSTRING "Number and/or string according to the comparison function utilized."
			:PARAMETER-TYPE required 
			:VALUE-TYPE number\,string
		)
		(< 
			:DOCSTRING "Compares if the first number is less than the second number."
			:PARAMETER-TYPE optional 
			:VALUE-TYPE number )
		(<= 
			:DOCSTRING "Compares if the first number is less than or equal to the second number."
			:PARAMETER-TYPE optional 
			:VALUE-TYPE number )
		(= 
			:DOCSTRING "Compares if the first number is equal to the second number."
			:PARAMETER-TYPE optional 
			:VALUE-TYPE number )
		(>= 
			:DOCSTRING "Compares if the first number is greater than or equal to the second number."
			:PARAMETER-TYPE optional 
			:VALUE-TYPE number )
		(> 
			:DOCSTRING "Compares if the first number is greater than the second number."
			:PARAMETER-TYPE optional 
			:VALUE-TYPE number )
		(same 
			:DOCSTRING "Compares if the first value is exactly the same to the second value."
			:PARAMETER-TYPE keyword 
			:VALUE-TYPE number\,string )
		(greater-than 
			:DOCSTRING "Compares if the first value is greater than the second value."
			:PARAMETER-TYPE keyword 
			:VALUE-TYPE number\,string )
		(less-than 
			:DOCSTRING "Compares if the first value is less than the second value."
			:PARAMETER-TYPE keyword 
			:VALUE-TYPE number\,string )
		(equal 
			:DOCSTRING "Compares if the first value is exactly the same to the second value."
			:PARAMETER-TYPE keyword 
			:VALUE-TYPE number\,string )
		(equalp 
			:DOCSTRING "Compares if the first value is exactly the same to the second value."
			:PARAMETER-TYPE keyword 
			:VALUE-TYPE number\,string )
	)
	(:RETURNS 
		"True (T) if comparison statement is valid, otherwise NIL"
	)
	(:EXAMPLES
		"Detailed below, graphically."
	)
	(:TEXT
		(:p (:b "1. This function compares two or more values and returns true if the comparison 
		statement is valid, otherwise NIL."))
		(:p "The >, >=, =, <=, and < options will only compare numbers:")
		(:img :src "/weblistenerdocs/bbldf/images/Order_Symbol_Number.jpg")
		(:p "")
		(:ul (:img :src "/weblistenerdocs/bbldf/images/Order_Result_True.jpg"))
		(:p (:b "2. The SAME, GREATER-THAN, LESS-THAN, EQUAL, and EQUALP options will compare the following:"))
		(:p (:u "Numbers:"))
		(:img :src "/weblistenerdocs/bbldf/images/Order_GreaterWord_Number_vs_Number.jpg")
		(:p (:u "Strings with numbers:"))
		(:img :src "/weblistenerdocs/bbldf/images/Order_GreaterWord_Number_and_Word.jpg")
		(:p (:u "Strings with numbers vs. numbers"))
		(:img :src "/weblistenerdocs/bbldf/images/Order_GreaterWord_Number_and_Word_vs_Number.jpg")
		(:p "All Return:")
		(:ul (:img :src "/weblistenerdocs/bbldf/images/Order_Result_True.jpg"))
		(:p (:b "3. Possible error(s) that can result is when comparing numbers and strings."))
		(:p "In the following example 'biobike.6' is compared to check if it is less than 'biobike.10'.")
		(:img :src "/weblistenerdocs/bbldf/images/Order_Less-Than_NIL-error.jpg")
		(:ul(:li(:p (:i "This results in a 'NIL'."))))
		(:p "To fix this false-negative a '0' is entered before the '6'.")
		(:img :src "/weblistenerdocs/bbldf/images/Order_Less-Than_NIL-error_FIX.jpg")
		(:p "")
		(:ul (:img :src "/weblistenerdocs/bbldf/images/Order_Result_True.jpg"))
	)
	(:KEYWORDS 
		Compare Comparison Order Equal Greater Less
	)
	(:SEE-ALSO)
)



;================================= ORFS-IN ==============================
(DOCUMENT-FUNCTION BBL::ORFS-IN
   ; JE: Fixed parameters. Needs examples for NO-STARTS, RETURN-SEQUENCES
   ;         ALL-ORFS
	(:SUMMARY "This function searches for open reading frames (ORFS).")
	(:SYNTAX)
	(:PARAMETERS
		(entity 
			:DOCSTRING "Sequence to be searched for ORFs."
			:PARAMETER-TYPE required 
			:VALUE-TYPE string
		)
		(no-starts 
			:DOCSTRING "ORFS need not begin with start codon."
			:PARAMETER-TYPE optional 
			:VALUE-TYPE number
		)
		(return-sequences 
			:DOCSTRING "Displays sequence of ORFS in result window."
			:PARAMETER-TYPE optional 
			:VALUE-TYPE number
		)
		(one-strand 
			:DOCSTRING "ORFS only from left to right strand of the entity."
			:PARAMETER-TYPE optional 
			:VALUE-TYPE number
		)
		(all-orfs 
			:DOCSTRING "Returns all possible ORFS that share same stop codon."
			:PARAMETER-TYPE optional 
			:VALUE-TYPE number
		)
		(longer-than 
			:DOCSTRING "Requires that ORF exceeds given length."
			:PARAMETER-TYPE keyword 
			:VALUE-TYPE number
		)
		(start-codons 
			:DOCSTRING "Overrides list of standard start codons."
			:PARAMETER-TYPE keyword 
			:VALUE-TYPE string\,list
		)
	)
	(:EXAMPLES
		"Detailed below, graphically."
	)
	(:TEXT
		(:p (:b "1. When searching for ORFS within a labeled-sequence without a chosen option:"))
		(:img :src "/weblistenerdocs/bbldf/images/ORFS-IN_Plain.jpg")
		(:p "Returns forward (F) and reverse (B) start and end locations within parenthesis:")
		(:ul (:img :src "/weblistenerdocs/bbldf/images/ORFS-IN_Plain_Result.jpg"))
		(:p (:b "2. A search can be done on the forward (F) sequence by choosing 'One-Strand':"))
		(:img :src "/weblistenerdocs/bbldf/images/ORFS-IN_One-Strand.jpg")
		(:p "")
		(:ul (:img :src "/weblistenerdocs/bbldf/images/ORFS-IN_One-Strand_Result.jpg"))
	)
	(:KEYWORDS 
		"Open Reading Frame, ORF, Start"
	)
	(:SEE-ALSO
		SORT INVERSION-OF
	)
)


; ================= ORGANISM-NAMED ====================
(DOCUMENT-FUNCTION ORGANISM-NAMED
	(:SUMMARY 
		"Finds an organism by name, alias, or partial match."
	)
	(:RETURN 
		"An organism (frame), a list of organisms, or NIL"
		:TYPE (or frame list) 
		:DISPLAY-TYPE nil
	)
	(:PARAMETERS
		(name 
			:DOCSTRING "A string, symbol, or frame identifying the organism"
			:PARAMETER-TYPE required
		)
		(in-part 
			:DOCSTRING "Specify to allow partial matches of the name to the full
			organism name or alias."
			:PARAMETER-TYPE :FLAG
		)
		(no-aliases
			:DOCSTRING "Specify to prevent matches to the organism's aliases."
			:PARAMETER-TYPE :FLAG
		)
	)
	(:EXAMPLES 
		"Detailed below, graphically."
	)
	(:RETURNS
		"A single organism, a list of organisms, or 'NIL' if no organsim match
		can	be found."
	)
	(:TEXT
		(:p "Searches through all known organisms finding those associated with
		the argument " (:i "'NAME'") ", either via an exact match to the 
		organsim's name, or an exact match to one of the organism's aliases, 
		or a partial match to the organism's name.")
		(:p (:b "1."))
		(:img :src "/weblistenerdocs/bbldf/images/Organism-Named_Full_Name.jpg" " (NOTE: Full name entered)")
		(:p "")
		(:img :src "/weblistenerdocs/bbldf/images/Organism-Named_Partial_Name-npun.jpg")
		(:p "")
		(:img :src "/weblistenerdocs/bbldf/images/Organism-Named_Partial_Name-puncti_In-Part.jpg")
		(:p "All return:")
		(:ul (:img :src "/weblistenerdocs/bbldf/images/Organism-Named_Full_Name_Result.jpg"))
		(:p "-----------------------------------------------------------------")
		(:p (:b "2."))
		(:img :src "/weblistenerdocs/bbldf/images/Organism-Named_Partial_Name-npun_No-Aliases.jpg")
		(:p "")
		(:img :src "/weblistenerdocs/bbldf/images/Organism-Named_Partial_Name-puncti.jpg")
		(:p "")
		(:img :src "/weblistenerdocs/bbldf/images/Organism-Named_Not-an-organism.jpg")
		(:p "All return:")
		(:ul (:img :src "/weblistenerdocs/bbldf/images/Organism-Named_Result_NIL.jpg"))
		(:p "-----------------------------------------------------------------")
		(:p (:b "3."))
		(:img :src "/weblistenerdocs/bbldf/images/Organism-Named_Partial_Name-aena_In-Part.jpg")
		(:p "")
		(:ul (:img :src "/weblistenerdocs/bbldf/images/Organism-Named_Partial_Name-aena_In-Part_Result.jpg"))
	)
	(:KEYWORDS 
		Organism Name
	)
	(:SEE-ALSO
		gene-named replicon-named protein-named chromosome-named contig-named
		organism-of PROTEIN-OF GENE-OF NAME-OF
	)
)


; ================= ORGANISM-OF ====================
(DOCUMENT-FUNCTION ORGANISM-OF
	(:SUMMARY
		"Returns organism's full or short name."
	)
	(:PARAMETERS
		(entity 
			:DOCSTRING "Gene, protein, contig (incl. chromosome or replicon) or 
			organism to be evaluated"
		)
		(short 
			:DOCSTRING "Returns the short name of the organism."
		)
	)
	(:EXAMPLES
		"Detailed below, graphically."
	)
	(:TEXT 
		(:p "Much of the knowledge stored in BioBIKE is made available to you 
		through " 
		((:a href "http://ramsites.net/~biobike/help/basic-syntax/basic-syntax-
		frames.html")"frames") " and " 
		((:a href "http://ramsites.net/~biobike/help/basic-syntax/basic-syntax-
		frames.html")" slots ")"." 
		(:br "The organism' s frame is the highest frame level, from which you 
		can access all the data available for a given organism.") 
		"ORGANISM-OF returns the frame of an organism from a given entity.")
  
		(:ul
		(:li "The given entity may be a gene, a protein, a replicon (chromosome,
		plasmid or contig) or the organism itself."
		(:br "A simple or a complex list of these entities is also valid."))
		(:li "Each organism has several available names in the slot called: 
		alternatives-names, nicknames and organism-symbols that also can
		be used."))
		(:p (:b "1a. If given a gene, a protein, or the organism itself"))
		(:img :src "/weblistenerdocs/bbldf/images/Organism-Of_all0004.jpg")
		(:p "")
		(:img :src "/weblistenerdocs/bbldf/images/Organism-Of_p-all7160.jpg")
		(:p "")
		(:img :src "/weblistenerdocs/bbldf/images/Organism-Of_a7120-pzeta.jpg")
		(:p "")
		(:img :src "/weblistenerdocs/bbldf/images/Organism-Of_a7120.jpg")
		(:p "All return:")
		(:ul (:img :src "/weblistenerdocs/bbldf/images/Organism-Of_Non-List_Result.jpg"))
		(:p (:b "1b. If the 'SHORT' option is chosen:"))
		(:img :src "/weblistenerdocs/bbldf/images/Organism-Of_p-all7160_Short.jpg")
		(:ul (:img :src "/weblistenerdocs/bbldf/images/Organism-Of_p-all7160_Short_Result.jpg"))
		(:p "-----------------------------------------------------------------")
		(:p (:b "2. If given a list of entities:"))
		(:img :src "/weblistenerdocs/bbldf/images/Organism-Of_GVP-list.jpg")
		(:p "Returns a list in the display window and a pop-up window:")
		(:ul (:img :src "/weblistenerdocs/bbldf/images/Organism-Of_GVP-list_Result.jpg"))
	)
	(:KEYWORDS 
		Organism Gene Protein Domain Replicon Chromosome Plasmid Contig)
	(:SEE-ALSO 
		CHROMOSOME-OF REPLICON-OF GENE-OF ORGANISM-OF PROTEIN-NAMED PROTEIN-OF
	)
)


; ================= ORTHOLOG/S-OF ====================
(DOCUMENT-FUNCTION ORTHOLOG/S-OF*
	(:SUMMARY 
		"Finds orthologs to the specified gene, protein or string.")
	(:RETURNS
		"A list or string of orthologs."
		:TYPE (or frame list) 
		:DISPLAY-TYPE nil
	)
	(:PARAMETERS
		(gene-or-protein
			:DOCSTRING "A string, symbol, or organism name (frame) identifying 
			the organism."
			:PARAMETER-TYPE required
			:VALUE-TYPE gene\,protein\,string
		)
		(best-best 
			:DOCSTRING "To specify a uni-latteral blast in the forward direction 
			returning the best results."
			:PARAMETER-TYPE :FLAG
		)
		(forward-best
			:DOCSTRING "To specify a uni-lateral blast in the forward direction 
			returning all hits."
			:PARAMETER-TYPE :FLAG
		)
		(reverse-best
			:DOCSTRING "To specify a uni-lateral blast in the reverse direction."
			:PARAMETER-TYPE :FLAG
		)
		(kegg-best-best
			:DOCSTRING "A specification for identifications that will be given to 
			the BEST-BEST results using the Kyoto Encyclopedia of Genes and 
			Genomes."
			:PARAMETER-TYPE :FLAG
		)
		(kegg-forward-best
			:DOCSTRING "A specification for identifications that will be given to 
			the FORWARD-BEST results using the Kyoto Encyclopedia of Genes and 
			Genomes."
			:PARAMETER-TYPE :FLAG
		)
		(values
			:DOCSTRING "A specification that will return the hit values of the 
			blast."
			:PARAMETER-TYPE :FLAG
		)
		(in
			:DOCSTRING "A limiting specification of which organism(s) to blast."
			:PARAMETER-TYPE keyword
			:VALUE-TYPE organism\,organism-list
		)
		(per
			:DOCSTRING "(Description pending.)"
			:PARAMETER-TYPE keyword
			:VALUE-TYPE function
		)
		(cutoff
			:DOCSTRING "(Description pending.)"
			:PARAMETER-TYPE keyword
			:VALUE-TYPE number
		)
		(min-identity
			:DOCSTRING "A limiting specification of percent identification match."
			:PARAMETER-TYPE keyword
			:VALUE-TYPE number
		)
		(number-of-hits
			:DOCSTRING "A limiting specification of matches to be displayed. Must 
			be used along with the following options: 'BEST-BEST', 'FOWARD-BEST' or 
			'REVERSE-BEST'."
			:PARAMETER-TYPE keyword
			:PARAMETER-TYPE number
		)
	)
	(:EXAMPLES 
		"Described below, graphically.")
	(:TEXT
		(:p (:b "1."))
		(:img :src "/weblistenerdocs/bbldf/images/Ortholog_In.jpg")
		(:p "")
		(:ul (:img :src "/weblistenerdocs/bbldf/images/Ortholog_In_Result.jpg"))
		(:p (:b "2. When using the option 'VALUES'"))
		(:img :src "/weblistenerdocs/bbldf/images/Ortholog_Values.jpg")
		(:p "")
		(:ul (:img :src "/weblistenerdocs/bbldf/images/Ortholog_Values_Result.jpg"))
		(:p "The match displays with the values in the following order:")
		(:ul (:li(:p "Percent Identity"))
		(:li(:p "Query Start"))
		(:li(:p "Query End"))
		(:li(:p "Subject Start"))
		(:li(:p "Subject End"))
		(:li(:p "Expected Value"))
		(:li(:p "Bit Score")))
	)
	(:KEYWORDS 
		ortholog gene protein)
	(:SEE-ALSO
		IS-GENE? IS-PROTEIN? GENE-OF GENES-OF ORGANISM-OF PROTEIN-SIMILAR-TO 
		HOMOLOG-OF*
	)
)


; ================= PLOT ====================
(DOCUMENT-FUNCTION PLOT
	(:SUMMARY 
		"Plots points along a graph."
	)
	(:SYNTAX 
		(PLOT data-list)
	)
	(:PARAMETERS
		(data-list 
			:DOCSTRING "A list or string of numbers to be plotted on graph."
			:PARAMETER-TYPE required 
			:VALUE-TYPE list
		)
		(points
			:DOCSTRING "A dot-graph representation of the given data-list or string
			of numbers."
			:PARAMETER-TYPE :FLAG
		)
		(box-histogram
			:DOCSTRING "A box-histogram representation of the given data-list or 
			string of numbers."
			:PARAMETER-TYPE :FLAG
		)
		(line-histogram
			:DOCSTRING "A line-histogram representation of the given data-list or 
			string of numbers."
			:PARAMETER-TYPE :FLAG
		)
		(lines
			:DOCSTRING "A line-graph representation of the given data-list or string
			of numbers."
			:PARAMETER-TYPE :FLAG
		)
		(counts
			:DOCSTRING "A graphical representation of how many times an item 
			appears in the data-list or string of numbers."
			:PARAMETER-TYPE :FLAG
		)
		(bin-interval
			:DOCSTRING "(Description pending)."
			:PARAMETER-TYPE keyword
			:VALUE-TYPE positive-number
		)
		(min
			:DOCSTRING "A limiting specification on the lower end of the x-axis 
			length."
			:PARAMETER-TYPE keyword
			:VALUE-TYPE number
		)
		(max
			:DOCSTRING "A limiting specification on the higher end of the x-axis 
			length."
			:PARAMETER-TYPE keyword
			:VALUE-TYPE number
		)
		(ymin
			:DOCSTRING "A limiting specification on the lower end of the y-axis 
			length."
			:PARAMETER-TYPE keyword
			:VALUE-TYPE number
		)
		(ymax
			:DOCSTRING "A limiting specificaiton on the higher end of the y-axis 
			length."
			:PARAMETER-TYPE keyword
			:VALUE-TYPE number
		)
		(xlabel
			:DOCSTRING "The title of the x-axis."
			:PARAMETER-TYPE keyword
			:VALUE-TYPE any
		)
		(ylabel
			:DOCSTRING "The title of the y-axis."
			:PARAMETER-TYPE keyword
			:VALUE-TYPE any
		)
	)
	(:RETURNS 
		"A graphed plot of points."
	)
	(:EXAMPLES 
		"Detailed below, graphically."
	)
	(:TEXT
		(:p "Plot produces a graph of either histogram type (line or box), line type,
		or graphs the count an item appears in the data-list.")
		(:p "")
		(:p "In this case, to get a count graph of the amount of times a number appears 
		in a data-list:")
		(:img :src "/weblistenerdocs/bbldf/images/Plot_Count_Labels.jpg") 
		(:p "Returns urls for the data that was inputed and a diagram of the graph:")
		(:ul (:img :src "/weblistenerdocs/bbldf/images/Plot_Count_Labels_Result2.jpg"))
		(:p "")
		(:ul (:img :src "/weblistenerdocs/bbldf/images/Plot_Count_Labels_Result1.jpg"))
	)
	(:KEYWORDS 
		graph histogram line plot
	)
	(:SEE-ALSO
		LIST IS-NUMBER IS-POSITIVE-INTEGER? IS-POSITIVE-NUMBER? IS-NONNEGATIVE?
		RANDOM-NUMBER RANDOM-INTEGER
	)
)

; ================= POP ====================
(DOCUMENT-FUNCTION bbl::POP
	(:SUMMARY 
		"Removes and displays the first item from a list or string."
	)
	(:SYNTAX 
		(POP data-list)
	)
	(:PARAMETERS
		(place
			:DOCSTRING "A list or string."
			:PARAMETER-TYPE required 
			:VALUE-TYPE list
		)
	)
	(:RETURNS 
		"The first item of the list is displayed and is removed from the original
		string or list."
	)
	(:EXAMPLES 
		"Detailed below, graphically."
	)
	(:TEXT
		(:p "The variable " (:i "list-of-numbers ") "was created using the function " (:b "'DEFINE'."))
		(:img :src "/weblistenerdocs/bbldf/images/Pop.jpg")
		(:p "--------------------------------------------------------------------------------")
		(:p "Excuting the 'POP' function on the variable " (:i "list-of-numbers") ":")
		(:img :src "/weblistenerdocs/bbldf/images/Pop-2.jpg")
		(:p "Returns the first item on the list:")
		(:ul (:img :src "/weblistenerdocs/bbldf/images/Pop-2_Result.jpg"))
		(:p "Also, the list-of-numbers variable does not contain the returned result:")
		(:ul (:img :src "/weblistenerdocs/bbldf/images/Pop-2_Result-2.jpg"))
	)
	(:KEYWORDS 
		graph histogram line plot
	)
	(:SEE-ALSO
		PUSH
	)
)

; ================= PREVIOUS-RESULT ====================
(DOCUMENT-FUNCTION PREVIOUS-RESULT
	(:SUMMARY 
		"Displays result of the last operation."
	)
	(:SYNTAX 
		(PREVIOUS-RESULT)
	)
	(:PARAMETERS)
	(:RETURNS 
		"The last results of the last operation."
	)
	(:EXAMPLE "")
	(:KEYWORDS 
		Previous Last Result
	)
	(:SEE-ALSO
		Return RETURN-FROM
	)
)

; ================= PRODUCT-OF ====================
(DOCUMENT-FUNCTION PRODUCT-OF
	(:SUMMARY 
		"Produces the product of numbers or lists of numbers."
	)
	(:SYNTAX 
		(PRODUCT-OF number )
	)
	(:PARAMETERS
		(number 
			:DOCSTRING "The number or list that are multiplied."
			:VALUE-TYPE number/list
		)
	)
	(:RETURNS 
		"The product numbers or list of numbers."
	)
	(:EXAMPLES 
		"Detailed below, graphically."
	)
	(:TEXT
		(:p " 1. The multiplication of two lists is not possible (i.e.(PRODUCT-OF {1 2 3} {1 2 3})) . ")
		(:p " 2. The multiplication of a list by a number is not possible (i.e. (PRODUCT-OF {1 2 3} 3) . ")
		(:p " 3. Not like the bigger_brother_function \"MULTIPLY\" , this function multiplies the elements of a list between them .")
		(:p " 4. The notations 1 2 3 and {1 2 3} are equivalent for this function, as seen in the following example. ")
		(:img :src "/weblistenerdocs/bbldf/images/Product-Of_Individual.jpg")
		(:p "")
		(:img :src "/weblistenerdocs/bbldf/images/Product-Of_List.jpg")
		(:p "Both return:")
		(:ul (:img :src "/weblistenerdocs/bbldf/images/Product-Of_Result.jpg"))
	)
	(:KEYWORDS 
		product multiply
	)
	(:SEE-ALSO 
		* / SUM-OF MULTIPLY QUOTIENT-OF
	)
)


; ================= PROTEIN/S-NAMED ====================
(DOCUMENT-FUNCTION PROTEIN/S-NAMED
	(:SUMMARY 
		"Finds a protein by partial match."
	)
	(:RETURNS "The full name of a protein"
		:TYPE (or frame list) :DISPLAY-TYPE nil)
	(:PARAMETERS
		(protein-name 
			:DOCSTRING "A string, symbol, or organism (frame) identifying the
			protein"
			:PARAMETER-TYPE required
			:VALUE-TYPE STRING\,SYMBOL\,GENE\,PROTEIN
		)
	)
	(:EXAMPLES
		"Detailed below, graphically."
	)
	(:TEXT
		(:img :src "/weblistenerdocs/bbldf/images/Protein-Named-1.jpg")
		(:p "")
		(:ul (:img :src "/weblistenerdocs/bbldf/images/Protein-Named-1_Result.jpg"))
	)
	(:KEYWORDS 
		protein name
	)
	(:SEE-ALSO
		gene-named replicon-named organism-named chromosome-named contig-named
		organism-of is-gene? is-protein?
	)
)


;================================= PROTEIN/S-OF ==============================
(DOCUMENT-FUNCTION PROTEIN/S-OF
	(:SUMMARY 
		"Returns the full name of a protein."
	)
	(:PARAMETERS
		(entity 
			:DOCSTRING "The item to be evaluated."
			:PARAMETER-TYPE	required
			:VALUE-TYPE GENE\,PROTEIN\,ORGANISM\,CONTIGUOUS-SEQUENCE\,LIST\,NULL
		)
		(wrap 
			:DOCSTRING "Wraps coordinate around circular sequence."
			:PARAMETER-TYPE :FLAG
		)
		(truncate 
			:DOCSTRING "Sets negative coordinate to 1 and exceeding coordinate 
			to the length of contig."
			:PARAMETER-TYPE :FLAG
		)
		(exclude-overlaps 
			:DOCSTRING "Excludes proteins that overlap coordinates."
			:PARAMETER-TYPE :FLAG
		)
		(from 
			:DOCSTRING "Replicon's coordinate from which proteins are collected."
			:PARAMETER-TYPE keyword
			:VALUE-TYPE number
		)
		(to 
			:DOCSTRING "Replicon's coordinate to which proteins are collected."
			:PARAMETER-TYPE keyword
			:VALUE-TYPE number
		)
		(length 
			:DOCSTRING "Segment length of given replicon in which proteins are 
			collected."
			:PARAMETER-TYPE keyword
			:VALUE-TYPE number
		)
	)
	(:EXAMPLES 
		"Detailed below, graphically."
	)
	(:TEXT
		(:p (:b (:u "1. If given a gene:")))
		(:img :src "/weblistenerdocs/bbldf/images/Protein-Of-1.jpg")
		(:p "")
		(:ul (:img :src "/weblistenerdocs/bbldf/images/Protein-Of-1_Result.jpg"))
		
		(:p (:b (:u "2. If given a protein:")))
		(:img :src "/weblistenerdocs/bbldf/images/Protein-Of-2.jpg")
		(:p "")
		(:ul (:img :src "/weblistenerdocs/bbldf/images/Protein-Of-1_Result.jpg"))
		
		(:p (:b (:u "3. If given a organism:")))
		(:img :src "/weblistenerdocs/bbldf/images/Proteins-Of-3.jpg")
		(:p "")
		(:ul (:img :src "/weblistenerdocs/bbldf/images/Proteins-Of-3_Result.jpg"))
		
		(:p (:b (:u "4. If given a replicon or contig:")))
		(:p "4a.")
		(:img :src "/weblistenerdocs/bbldf/images/Proteins-Of-4a.jpg")
		(:p "")
		(:ul (:img :src "/weblistenerdocs/bbldf/images/Proteins-Of-4a_Result.jpg"))
		(:p "--------------------------------------------------------------------------------")
		(:img :src "/weblistenerdocs/bbldf/images/Proteins-Of-4b.jpg")
		(:p "")
		(:ul (:img :src "/weblistenerdocs/bbldf/images/Proteins-Of-4b_Result.jpg"))
		
		(:p (:b (:u "5. If given a list of genes, proteins, organisms, replicons or contigs:")))
		(:p "5a.")
		(:img :src "/weblistenerdocs/bbldf/images/Protein-Of-5a.jpg")
		(:p "")
		(:ul (:img :src "/weblistenerdocs/bbldf/images/Protein-Of-5a_Result.jpg"))
		(:p "--------------------------------------------------------------------------------")
		(:p "5b.")
		(:img :src "/weblistenerdocs/bbldf/images/Protein-Of-5b.jpg")
		(:p "")
		(:ul (:img :src "/weblistenerdocs/bbldf/images/Protein-Of-5b_Result.jpg"))
		(:p "--------------------------------------------------------------------------------")
		(:p "5c.")
		(:img :src "/weblistenerdocs/bbldf/images/Protein-Of-5c.jpg")
		(:p "")
		(:ul (:img :src "/weblistenerdocs/bbldf/images/Protein-Of-5c_Result.jpg"))
		
		(:p (:b (:u "6I. WRAP")))
		(:p "6a. If given a circular replicon")
		(:img :src "/weblistenerdocs/bbldf/images/Protein-Of-6a.jpg")
		(:p "Returns an error:")
		(:ul (:img :src "/weblistenerdocs/bbldf/images/Protein-Of-6a_Result.jpg"))
		(:p (:b (:i "To remove this error the 'WRAP' option is chosen:")))
		(:img :src "/weblistenerdocs/bbldf/images/Protein-Of-6b.jpg")
		(:p "")
		(:ul (:img :src "/weblistenerdocs/bbldf/images/Protein-Of-6b_Result.jpg"))
		(:p "--------------------------------------------------------------------------------")
		(:p (:b "Another example:"))
		(:img :src "/weblistenerdocs/bbldf/images/Protein-Of-6c.jpg")
		(:p "Returns an error:")
		(:ul (:img :src "/weblistenerdocs/bbldf/images/Protein-Of-6c_Result.jpg"))
		(:p (:b (:i "To remove this error the 'WRAP' option is chosen:")))
		(:img :src "/weblistenerdocs/bbldf/images/Protein-Of-6d.jpg")
		(:p "")
		(:ul (:img :src "/weblistenerdocs/bbldf/images/Protein-Of-6d_Result.jpg"))

		(:p (:b (:u "6II. TRUNCATE")))
		(:p "6b. If given a linear contig")
		(:img :src "/weblistenerdocs/bbldf/images/Protein-Of-6e.jpg")
		(:p "Returns an error:")
		(:ul (:img :src "/weblistenerdocs/bbldf/images/Protein-Of-6e_Result.jpg"))
		(:p (:b (:i "In this case we change the -1000 to a 0:")))
		(:img :src "/weblistenerdocs/bbldf/images/Protein-Of-6f.jpg")
		(:p "Returns an error:")
		(:ul (:img :src "/weblistenerdocs/bbldf/images/Protein-Of-6f_Result.jpg"))
		(:p (:b (:i "To remove this error the 'TRUNCATE' option is chosen:")))
		(:img :src "/weblistenerdocs/bbldf/images/Protein-Of-6g.jpg")
		(:p "")
		(:ul (:img :src "/weblistenerdocs/bbldf/images/Protein-Of-6g_Result.jpg"))
		
		(:p (:b (:u "7. Illustrate 'EXCLUDE-OVERLAPS'")))
		(:p "7a.")
		(:img :src "/weblistenerdocs/bbldf/images/Protein-Of-7a.jpg")
		(:p "")
		(:ul (:img :src "/weblistenerdocs/bbldf/images/Protein-Of-7a_Result.jpg"))
		(:p "--------------------------------------------------------------------------------")
		(:p "7b. Now with the 'EXCLUED-OVERLAPS' option:")
		(:img :src "/weblistenerdocs/bbldf/images/Protein-Of-7b.jpg")
		(:p "")
		(:ul (:img :src "/weblistenerdocs/bbldf/images/Protein-Of-7b_Result.jpg"))
;CHART
		(:p "The function PROTEIN-OF (PROTEINS-OF) returns clickable proteins' frames of:")
		(:ul
		(:li "all proteins encoded by a given entity.")
		(:li "the proteins encoded by a segment within specified coordinates of a given entity."))
		(:p "When coordinates are specified, additional options are available:")
		(:ul
		(:li "to WRAP coordinates of the evaluated segment around circular replicon.")
		(:li "to TRUNCATE the evaluated segment at the beginning and the end of a given replicon or contig.")
		(:li "to EXCLUDE-OVERLAPS, i.e. proteins that overlap specified coordinates of the evaluated segment."))
		
		(:ul (:img :src "/weblistenerdocs/bbldf/images/Proteins-Of_Chart.jpg"))
		
		(:p "NOTE:" )
		(:ul
		(:li "If there is no protein encoded by the segment between specified coordinates the function returns NIL.")
		(:li "When PROTEIN-OF is given an organism, coordinates can only be interpreted if this organism has only one replicon." (:br "Otherwise use organism's replicons (see contiguous-sequences in the organism's frame)."))
		(:li "When PROTEIN-OF is given a "((:a href "http://ramsites.net/~biobike/help/basic-syntax/basic-syntax-aggregates-lists-p1.html")"simple list")", it returns a simple list composed of the function applied to each element of the list.")
		(:li "When PROTEIN-OF is given a "((:a href "http://ramsites.net/~biobike/help/basic-syntax/basic-syntax-aggregates-lists-p2.html")"complex list")", it returns a complex list of the same structure."))
	)
	(:KEYWORDS
		Name Protein Replicon Truncate Wrap
	)
	(:SEE-ALSO 
		GENE-OF CODING-GENES-OF NONCODING-GENES-OF
		PROTEIN-NAMED ORGANISM-NAMED IS-PROTEIN? IS-GENE? 
		ORGANISM-OF ORTHOLOG-OF ORTHOLOG-OF TRANSLATION-OF
	)
)

;================================= PROTEIN/S-SIMILAR-TO ==============================
(DOCUMENT-FUNCTION PROTEIN/S-SIMILAR-TO
	(:SUMMARY
		"Finds sequences similar to others, by e-value or mismatches"
	)
	(:PARAMETERS
		(query
			:DOCSTRING "The sequences a user chooses to BLAST with."
			:PARAMETER-TYPE required
			:VALUE-TYPE GENE\,PROTEIN\,CONTIGUOUS-SEQUENCE\,ORGANISM\,LABELED-SEQUENCE\,STRING\,LIST
		)
		(target 
			:DOCSTRING "The group of sequences to BLAST against."
			:PARAMETER-TYPE required
			:VALUE-TYPE GENE\,PROTEIN\,CONTIGUOUS-SEQUENCE\,ORGANISM\,LABELED-SEQUENCE\,STRING\,LIST
		)
		(each
			:DOCSTRING "A comparison of each item in the query to the target value(s)."
			:PARAMETER-TYPE token
		)
		(pattern
			:DOCSTRING "A comparison of a particular pattern to the target value(s)."
			:PARAMETER-TYPE token
		)
		(in
			:DOCSTRING "A directional reference to the target."
			:PARAMETER-TYPE token
		)
		(protein-vs-protein
			:DOCSTRING "A comparison of protein sequences."
			:PARAMETER-TYPE :FLAG
		)
		(dna-vs-dna
			:DOCSTRING "A comparison of DNA sequences."
			:PARAMETER-TYPE :FLAG
		)
		(protein-vs-translated-dna
			:DOCSTRING "A comparison of proteins sequence against translated DNA 
			sequences."
			:PARAMETER-TYPE :FLAG
		)
		(translated-dna-vs-protein
			:DOCSTRING "A comparison of translated DNA sequences against protein 
			sequence."
			:PARAMETER-TYPE :FLAG
		)
		(translated-dna-vs-translated-dna
			:DOCSTRING "A comparison of translated DNA sequences"
			:PARAMETER-TYPE :FLAG
		)
		(mismatches
			:DOCSTRING "A Limiting specification on the maximum mismatches that are 
			allowed."
			:PARAMETER-TYPE keyword
			:VALUE-TYPE number
		)
		(threshold
			:DOCSTRING "A Limiting specification referring to the expected value 
			(E-value)."
			:PARAMETER-TYPE keyword
			:VALUE-TYPE number
		)
		(word-size
			:DOCSTRING "(Description pending.)"
			:PARAMETER-TYPE keyword
		)
		(return
			:DOCSTRING "A Limiting specification on how many sequences that match 
			will be displayed from each organism."
			:PARAMETER-TYPE keyword
			:VALUE-TYPE positive-number
		)
	)
	(:RETURNS
		"A List or Table"
	)
	(:TEXT
		(:p (:b "1. To find similar proteins."))
		(:img :src "/weblistenerdocs/bbldf/images/Proteins-Similar-To_Pattern.jpg")
		(:ul (:p (:li (:i "A pop-up window will display:"))))
		(:ul (:img :src "/weblistenerdocs/bbldf/images/Proteins-Similar-To_Pattern_Result.jpg"))
	)
	(:KEYWORDS
		"Similar, Protein, Mismatches, Threshold, BLAST, Translated DNA"
	)
	(:SEE-ALSO
		IS-GENE? IS-PROTEIN? GENE-OF GENES-OF ORGANISM-OF SEQUENCE-SIMILAR-TO
		PROTEIN-SIMILAR-TO HOMOLOG-OF* TRANSLATION-OF
	)
)

; ================= PUSH ====================
(DOCUMENT-FUNCTION bbl::PUSH
	(:SUMMARY 
		"Attaches a value or value(s) to the beginning of a list or string."
	)
	(:PARAMETERS
		(value
			:DOCSTRING "The item(s) that will be attached."
			:PARAMETER-TYPE required
			:VALUE-TYPE any
		)
		(place
			:DOCSTRING "The defined list, table, or string to which the value 
			will be attached."
			:PARAMETER-TYPE required
			:VALUE-TYPE list\,table
		)
	)
	(:RETURNS
		"A string or list."
	)
	(:TEXT
		(:p (:b "The sequnce of a7120p-all7002 is saved to the variable 'sequence-of-p-all7002'."))
		(:img :src "/weblistenerdocs/bbldf/images/Push_Define.jpg")
		(:p "")
		(:ul (:img :src "/weblistenerdocs/bbldf/images/Push_Old_Seq.jpg"))
		(:p (:b "Now the string '>p-all7002' will be added to the beginning of the sequence."))
		(:p (:b "NOTE: ") "The function " (:b "'DISPLAY' ") "does not need to be used.")
		(:img :src "/weblistenerdocs/bbldf/images/Push_Display-Line.jpg")
		(:p "")
		(:ul (:img :src "/weblistenerdocs/bbldf/images/Push_New_Seq.jpg"))
	)
	(:KEYWORDS
		Push Attach
	)
	(:SEE-ALSO
		INSERT POP JOIN
	)
)

; ================= QUOTIENT-OF ====================
(DOCUMENT-FUNCTION QUOTIENT-OF
	(:SUMMARY 
		"Produces the quotient of numbers or lists of numbers."
	)
	(:SYNTAX 
		(PRODUCT-OF number)
	)
	(:PARAMETERS
		(number 
			:DOCSTRING "the number or list that are divided"
			:PARAMETER-TYPE required
			:VALUE-TYPE number/list
		)
	)
	(:RETURNS 
		"A number or list of numbers."
	)
	(:EXAMPLES 
		"Detailed below, graphically."
	)
	(:TEXT
		(:p " 1. The division of two lists is not possible (i.e.(QUOTIENT-OF {1 2 3} {1 2 3})). ")
		(:p " 2. The division of a list by a number is not possible (i.e. (QUOTIENT-OF {1 2 3} 3). ")
		(:p " 3. Not like the bigger_brother_function \"DIVIDE\", this function divides the elements of a list between them.")
		(:p " 4. The notations 1 2 3 and {1 2 3} are equivalent for this function. ")
		(:img :src "/weblistenerdocs/bbldf/images/Quotient-Of_Ind.jpg")
		(:p "")
		(:img :src "/weblistenerdocs/bbldf/images/Quotient-Of_List.jpg")
		(:p "Both return:")
		(:ul (:img :src "/weblistenerdocs/bbldf/images/Quotient-Of_Result.jpg"))
	)
	(:KEYWORDS 
		Quotient Divide
	)
	(:SEE-ALSO 
		* / SUM-OF MULTIPLY PRODUCT-OF
	)
)

; ================= RANDOM-DNA ====================
(DOCUMENT-FUNCTION RANDOM-DNA
	(:SUMMARY
		"Produces a random DNA sequence."
	)
	(:PARAMETERS
		(like 
			:DOCSTRING "Sequence to be emulated in nucleotide content and/or 
			length"
			:PARAMETER-TYPE keyword
			:VALUE-TYPE GENE\,ORGANISM\,CONTIGUOUS-SEQUENCE\,LABELED-SEQUENCE\,STRING\,LIST
		)
		(frequencies 
			:DOCSTRING "List of nucleotide frequencies to be applied"
			:PARAMETER-TYPE keyword
			:VALUE-TYPE list
		)
		(length 
			:DOCSTRING "Length of random sequence to be produced"
			:PARAMETER-TYPE keyword
			:VALUE-TYPE positive-number
		)
	)
	(:EXAMPLES 
		"Detailed below, graphically."
	)
	(:RETURNS
		"An object of type STRING or LIST."
	)
	(:TEXT
		(:p (:b "For the following examples the acutal results will not be shown, but an explanation
		of what will occur is italicized."))
		(:p "1. Emulate a gene:")
		(:img :src "/weblistenerdocs/bbldf/images/Random-DNA_ss120.jpg")
		(:ul (:li (:p (:i "Sequence of same length and nucleotide content as 
		ss120."))))
		
		(:p "2. Emulate a specific sequence:")
		(:img :src "/weblistenerdocs/bbldf/images/Random-DNA_all4312.jpg")
		(:ul (:li (:p (:i "Sequence of same length an nucleotide content as 
		the region upstream from the gene."))))
		
		(:p "3. Produce a sequence according to a set of frequencies")
		(:img :src "/weblistenerdocs/bbldf/images/Random-DNA_Frequencies.jpg")
		(:ul (:li (:p (:i "1000-nt sequence with 30% A, 20% C, 20% G, 
		and 30% T."))))
		
		(:p "4. Produce a sequence according to a simple nucleotide content")
		(:img :src "/weblistenerdocs/bbldf/images/Random-DNA_Length.jpg")
		(:ul (:li (:p (:i "100-nt sequence with A:C:G:T in the ratio of 
		2:1:1:2."))))
	
		(:p "--------------------------------------------------------------------------------")
		(:center (:img :src "/weblistenerdocs/bbldf/images/Random-DNA.jpg"))

		(:p "The function " (:b "RANDOM-DNA") " produces a random nucleotide sequence according to 
		the following rules:")
		
		(:center (:img :src "/weblistenerdocs/bbldf/images/Random-DNA_Chart.jpg"))
		
		(:p (:b "RANDOM-DNA ") "is useful to generate sequences that are similar in nucleotide content 
		to natural sequences of interest. For example, if an interesting short sequence occurs upstream 
		from a gene, you may want to determine how often it would occur by chance in a region with the 
		same size and nucleotide content. The sequence to be emulated is provided using the " 
		(:b "'LIKE' ") "option.")

		(:p "If a sequence is not provided using the "(:b "'LIKE' ") "option, then a random sequence is 
		generated according to a set of nucleotide frequencies. These frequencies may be provided using the " 
		(:b "'FREQUENCIES' ") "option, which calls for a list of four numbers, corresponding to the relative 
		ratios of A, C, G, and T, respectively. The numbers need not be fractions. If frequencies are not 
		provided, then a frequency of 25% is presumed for all four nucleotides.")

		(:p (:b "'LIKE' ") "and " (:b "'FREQUENCIES' " ) "options are incompatible with each other. 
		However, " (:b "'LENGTH' ") "can be used with either of the two or alone.")
	)
	(:KEYWORDS 
		Random Sequence Nucleotides Frequencies)
	(:SEE-ALSO
		LIST SEQUENCE-OF IS-DNA-SEQUENCE
	)
)

; ================= RANDOM-INTEGER ====================
(DOCUMENT-FUNCTION RANDOM-INTEGER
	(:SUMMARY
		"Returns a random integer between possibly specified limits."
	)
	(:PARAMETERS
		(from
			:DOCSTRING "The beginning boundary for the function to choose."
			:PARAMETER-TYPE keyword
			:VALUE-TYPE number
		)
		(to
			:DOCSTRING "The ending boundary for the function to choose."
			:PARAMETER-TYPE keyword
			:VALUE-TYPE number
		)
		(in
			:DOCSTRING "Will return a random integer from the provided list."
			:PARAMETER-TYPE keyword
			:VALUE-TYPE list
		)
		(list-of
			:DOCSTRING "The amount of random integers to be returned."
			:PARAMETER-TYPE positive-number
			:VALUE-TYPE number
		)
	)
	(:RETURNS
		"Unlike the function 'RANDOM-NUMBER', the current function will return whole numbers."
	)
	(:TEXT
		(:p (:b "For the following examples the actual results will not be shown, but an explanation
		of what will occur is italicized."))
		
		(:p "1. A boundary can be set by the functions " (:b "'FROM' ") "and " (:b "'TO' ") ":")
		(:img :src "/weblistenerdocs/bbldf/images/Random-Integer_From_To.jpg")
		(:ul (:li (:p (:i "A random integer between the 1 and 10 boundry."))))
		
		(:p "2. A integer can be chosen from a list with the function " (:b "'IN' ") ":")
		(:img :src "/weblistenerdocs/bbldf/images/Random-Integer_In.jpg")
		(:ul (:li (:p (:i "A random integer within the defined number-list variable."))))
		
		(:p "3. A set amount of random integers can be chosen with the function " (:b "'LIST-OF' ") ":")
		(:img :src "/weblistenerdocs/bbldf/images/Random-Integer_List-Of.jpg")
		(:ul (:li (:p (:i "A list of five random integers."))))
	)
	(:KEYWORDS
		Integer Random
	)
	(:SEE-ALSO
		Random-Number LIST ROUND IS-NUMBER IS-POSITIVE-INTEGER? 
		IS-POSITIVE-NUMBER? IS-NONNEGATIVE?
	)
)

; ================= RANDOM-NUMBER ====================
(DOCUMENT-FUNCTION RANDOM-NUMBER
	(:SUMMARY
		"Returns a random number between possibly specified limits."
	)
	(:PARAMETERS
		(from
			:DOCSTRING "The beginning boundary for the function to choose."
			:PARAMETER-TYPE keyword
			:VALUE-TYPE number
		)
		(to
			:DOCSTRING "The ending boundary for the function to choose."
			:PARAMETER-TYPE keyword
			:VALUE-TYPE number
		)
		(in
			:DOCSTRING "Will return a random number from the provided list."
			:PARAMETER-TYPE keyword
			:VALUE-TYPE list
		)
		(list-of
			:DOCSTRING "The amount of random numbers to be returned."
			:PARAMETER-TYPE positive-number
			:VALUE-TYPE number
		)
	)
	(:RETURNS
		"Unlike the function 'RANDOM-INTEGER', the current function can return 
		whole or fractional	numbers."
	)
	(:TEXT
		(:p (:b "For the following examples the actual results will not be shown, 
		but an explanation of what will occur is italicized."))
		
		(:p "1. A boundary can be set by the functions " (:b "'FROM' ") "and " 
		(:b "'TO' ") ":")
		(:img :src "/weblistenerdocs/bbldf/images/Random-Number_From_To.jpg")
		(:ul (:li (:p (:i "A random number between the 1 and 10 boundary."))))
		
		(:p "2. A number can be chosen from a list with the function " 
		(:b "'IN' ")":")
		(:img :src "/weblistenerdocs/bbldf/images/Random-Number_In.jpg")
		(:ul (:li (:p (:i "A random number within the defined number-list 
		variable."))))
		
		(:p "3. A set amount of random number can be chosen with the function " 
		(:b "'LIST-OF' ") ":")
		(:img :src "/weblistenerdocs/bbldf/images/Random-Number_List-Of.jpg")
		(:ul (:li (:p (:i "A list of five random numbers."))))
	)
	(:KEYWORDS
		Number Random
	)
	(:SEE-ALSO
		Random-Integer LIST ROUND IS-NUMBER IS-POSITIVE-INTEGER? 
		IS-POSITIVE-NUMBER? IS-NONNEGATIVE?
	)
)


; ================= READ ====================
	(DOCUMENT-FUNCTION bbl::READ
		(:SUMMARY
			"Reads material from a possibly formatted file."
		)
		(:SYNTAX
			(READ [FROM] file-name [SHARED] [FASTA] [TAB-DELIMITED] [TABBED] [TEXT] [JOIN-LINES] [JOIN-LINES-BY-SPACES] [CONVERT-NUMBERS])
		)
		(:PARAMETERS
			(file-name
				:DOCSTRING "The name of the file to be read."
				:PARAMETER-TYPE required
				:VALUE-TYPE string\,pathname
			)
			(shared
				:DOCSTRING "An option only to be used if the file-name is within
				a common folder for all users."
				:PARAMETER-TYPE :FLAG
			)
			(fasta
				:DOCSTRING "An option to read files in fasta format."
				:PARAMETER-TYPE :FLAG
			)
			(tab-delimited
				:DOCSTRING "An option to remove any tabs within the file."
				:PARAMETER-TYPE :FLAG
			)
			(tabbed
				:DOCSTRING "(Description pending)."
				:PARAMETER-TYPE :FLAG
			)
			(text
				:DOCSTRING "An option to read files in a text format."
				:PARAMETER-TYPE :FLAG
			)
			(join-lines
				:DOCSTRING "An option to join each line into on continuous line
				of charachters."
				:PARAMETER-TYPE :FLAG
			)
			(join-lines-by-spaces
				:DOCSTRING "An option to join each line with a space between each
				new line creating a continuous line of characters."
				:PARAMETER-TYPE :FLAG
			)
			(convert-numbers
				:DOCSTRING "(Description pending)."
				:PARAMETER-TYPE :FLAG
			)
		)
		(:RETURNS
			"The contents of a file."
		)
		(:EXAMPLES
			"Detailed below, graphically."
		)
		(:TEXT
			(:p (:b (:u "How to use this function.")))
			
			(:p (:b "NOTE:") " The files " (:u "Random-DNA") " and " (:u "Tabbed")
			" are files created or uploaded within BioBIKE by a user and are " 
			(:u "not") " accessible by other BioBIKE users (files not shared).")
			
			(:p (:b "1. ") "If no options are chosen...")
			(:img :src "/weblistenerdocs/bbldf/images/Read_Random-DNA_1.jpg")
			(:p (:li (:i "the file will be displayed as it appears in the original 
			file.")))
			(:ul (:img :src "/weblistenerdocs/bbldf/images/Read_Random-DNA_Default_Result.jpg"))
			
			(:p (:b "2. ") "If the " (:b "'FASTA'") " option is chosen...")
			(:img :src "/weblistenerdocs/bbldf/images/Read_Random-DNA_FASTA.jpg")
			(:p (:li (:i "the file will separate the organism label from the 
			sequence.")))
			(:ul (:img :src "/weblistenerdocs/bbldf/images/Read_Random-DNA_FASTA_Result.jpg"))
			
			(:p (:b "3. ") "If the " (:b "'JOIN-LINES'") " option is chosen...")
			(:img :src "/weblistenerdocs/bbldf/images/Read_Random-DNA_JL.jpg")
			(:p (:li (:i "the file will join each line of the file without any 
			separation.")))
			(:ul (:img :src "/weblistenerdocs/bbldf/images/Read_Random-DNA_JL_Result.jpg"))
			
			(:p (:b "4. ") "If the " (:b "'JOIN-LINES-BY-SPACES'") " option is chosen...")
			(:img :src "/weblistenerdocs/bbldf/images/Read_Random-DNA_JLBS.jpg")
			(:p (:li (:i "the file will join each line of the file with a space 
			between each original line of the file.")))
			(:ul (:img :src "/weblistenerdocs/bbldf/images/Read_Random-DNA_JLBS_Result.jpg"))
			
			(:p (:b (:u "A tabbed file") ":"))
			(:p (:b "5a. ") "Characters or strings are separated by tabs.")
			(:img :src "/weblistenerdocs/bbldf/images/Read_Tabbed_2.jpg")
			(:p "")
			(:ul (:img :src "/weblistenerdocs/bbldf/images/Read_Tabbed_Original_Result.jpg"))
			
			(:p (:b "5b. ") "To remove tabs a file that contains tabs between 
			characters and/or strings.")
			(:img :src "/weblistenerdocs/bbldf/images/Read_Tabbed_Tabbed-Delimited.jpg")
			(:p "")
			(:ul (:img :src "/weblistenerdocs/bbldf/images/Read_Tabbed_Tabbed-Delimited_Result.jpg"))
			
			(:p (:b "Currently any .doc, .docx, .xls or .xlsx are not able to be 
			read by this function and will display an error message."))
		)
		(:KEYWORDS
			Read File FASTA Text Tab-Deliminated Open
		)
		(:SEE-ALSO
			bbl::WRITE bbl::RUN-FILE
		)
	)

	
; ================= READING-FRAMES-OF ====================
(DOCUMENT-FUNCTION READING-FRAMES-OF
	(:SUMMARY
		"Translates sequence in all six reading frames."
	)
	(PARAMETERS
		(entity
			:DOCSTRING "DNA sequence to be translated."
			:PARAMETER-TYPE required
			:VALUE-TYPE gene\,protein\,contiguous-sequence\,labeled-sequence\,string
		)
		(do-not-display
			:DOCSTRING "To avoid resulting sequences to be displayed."
			:PARAMETER-TYPE :FLAG
		)
		(nowarnings
			:DOCSTRING  "To prevent any warnings to be displayed."
			:PARAMETER-TYPE :FLAG
		)
		(line-length
			:DOCSTRING "To modify the format, in which resulting sequences are displayed."
			:PARAMETER-TYPE keyword
			:VALUE-TYPE number
		)
		(segment-length
			:DOCSTRING "To modify the format, in which resulting sequences are displayed."
			:PARAMETER-TYPE keyword
			:VALUE-TYPE number
		)
	)
	(:RETURNS
		"An object type LIST or LIST."
	)
	(:EXAMPLES
		"Detailed below, graphically"
	)
	(:TEXT
		(:p (:b "How to use the function:"))
		(:p "1a. An undefined sequence can be entered:")
		(:img :src "/weblistenerdocs/bbldf/images/Reading-Frames-Of_Plain.jpg")
		(:p "")
		(:ul (:img :src "/weblistenerdocs/bbldf/images/Reading-Frames-Of_Plain_Result.jpg"))
		(:p "1b. A defined sequence can also be entered:")
		(:img :src "/weblistenerdocs/bbldf/images/Reading-Frames-Of_all0004.jpg")
		(:p "")
		(:ul (:img :src "/weblistenerdocs/bbldf/images/Reading-Frames-Of_all0004_Result.jpg"))
		
		(:p "2a. Illustrate the options " (:b "'LINE-LENGTH'") ", " (:b "'SEGMENT-LENGTH'") ":")
		(:img :src "/weblistenerdocs/bbldf/images/Reading-Frames-Of_Line_Seg.jpg")
		(:p "")
		(:ul (:img :src "/weblistenerdocs/bbldf/images/Reading-Frames-Of_Line_Seg_Result.jpg"))
		(:p "")
		(:p "2b. Illustrate the option " (:b "'DO-NOT-DISPLAY'") ":")
		(:img :src "/weblistenerdocs/bbldf/images/Reading-Frames-Of_No-Display.jpg")
		(:p "")
		(:ul (:img :src "/weblistenerdocs/bbldf/images/Reading-Frames-Of_No-Display_Result.jpg"))
		
		(:p "--------------------------------------------------------------------------------")
		(:p (:b "READING-FRAMES-OF") " translates DNA sequences to 6 possible amino-acid sequences.")

		(:ul
		(:li "The function translates the given sequence and its complement.")
		(:li "The translation starts at the positions 1, 2 and 3 of the given and complemented sequences.")
		(:li "The given DNA sequence may be a string, a gene or a protein (in this case the gene of the 
		given protein is translated by the function.")
		(:li "A list of strings, genes or proteins may be given.")
		(:li "The options " (:b "'LINE-LENGTH'") " and " (:b "'SEGMENT-LENGTH'") " followed by a number 
		may be used to modify the format in which the resulting sequences are displayed.")
		(:li "If you do not want the resulting sequences displayed in a pretty format, use the flag " 
		(:b "'DO-NOT-DISPLAY'") "."))

	)
	(:KEYWORDS
		Reading Frames
	)
	(:SEE-ALSO
		Translation-Of SEQUENCE-OF GENE-OF GENES-OF 
		PROTEIN-OF ORTHOLOG-OF ORGANISM-OF
	)
)


; ================= REPEAT ====================
(DOCUMENT-FUNCTION REPEAT
	(:SUMMARY "Repeats a given string a specified number of times")
	(:SYNTAX (REPEAT string [TIMES] number))
	(:PARAMETERS
		(string-or-item 
			:DOCSTRING "The source you would like to modify."
			:PARAMETER-TYPE required
			:VALUE-TYPE any
		)
		(number 
			:DOCSTRING "Number of repeats or length."
			:PARAMETER-TYPE required
			:VALUE-TYPE nonnegativenumber
		)
		(each 
			:DOCSTRING "Used with a list of items."
			:PARAMETER-TYPE :TOKEN
		)
		(times 
			:DOCSTRING "Number of times a string or item is repeated."
			:PARAMETER-TYPE :TOKEN
		)
		(until-length
			:DOCSTRING "The length of result."
			:PARAMETER-TYPE :TOKEN
		)
		(as-list
			:DOCSTRING "Groups each individual repeat."
			:PARAMETER-TYPE :FLAG
		)
		(as-string
			:DOCSTRING "Displays the repeats in one continuous string."
			:PARAMETER-TYPE :FLAG
		)
		(as-unit
			:DOCSTRING "Takes string or item as a single group."
			:PARAMETER-TYPE :FLAG
		)
	)
	(:RETURNS 
		"A list or string.")
	(:EXAMPLES
		"Described below, graphically.")
	(:TEXT
		(:p "1. Using the option " (:b "'AS-LIST'") ":")
		(:img :src "/weblistenerdocs/bbldf/images/Repeat_As-List.jpg")
		(:ul (:p "Will group the repeats within quotations."))
		(:ul (:img :src "/weblistenerdocs/bbldf/images/Repeat_As-List_Result.jpg"))
		(:p "2. Using the same string, but with the option " (:b "'AS-STRING'") ":")
		(:img :src "/weblistenerdocs/bbldf/images/Repeat_As-String.jpg")
		(:ul (:p "Will display one continuous string."))
		(:ul (:img :src "/weblistenerdocs/bbldf/images/Repeat_As-String_Result.jpg"))
		(:p "3. When inserting a list and using the option " (:b "'AS-UNIT'") ":")
		(:img :src "/weblistenerdocs/bbldf/images/Repeat_As-Unit.jpg")
		(:ul (:p "Will group each repeat within parenthesis."))
		(:ul (:img :src "/weblistenerdocs/bbldf/images/Repeat_As-Unit_Result.jpg"))
		(:p "4. When using a string and the option " (:b "'UNTIL-LENGTH'") " and changing NUMBER to 3:")
		(:img :src "/weblistenerdocs/bbldf/images/Repeat_Until-Length.jpg")
		(:ul (:p "Will only display the first three items of the string."))
		(:ul (:img :src "/weblistenerdocs/bbldf/images/Repeat_Until-Length_Result.jpg"))
	)
	(:KEYWORDS
		Repeat
	)
	(:SEE-ALSO
		Repeat-Function LOOP
	)
)


; ================= REPEAT-FUNCTION ====================
(DOCUMENT-FUNCTION REPEAT-FUNCTION
	(:SUMMARY 
		"Applies a function a given number of times and returns a list of 
		results."
	)
	(:PARAMETERS
		(function
			:DOCSTRING "The function(s) to be repeated."
			:PARAMETER-TYPE required
			:VALUE-TYPE function
		)
		(iterations
			:DOCSTRING "The number of times that the function will be repeated."
			:PARAMETER-TYPE required
			:VALUE-TYPE nonnegative-number
		)
		(times
			:DOCSTRING ""
			:PARAMETER-TYPE :TOKEN
		)
	)
	(:RETURNS
		"A list or string of items."
	)
	(:TEXT
		(:p (:b "'REPEAT-FUNCTION' ") "can be used with a number of other functions to 
		to produce a random sequence:")
		(:p "In this example the functions " (:b "'AND'") ", " (:b "'DEFINE'") ", and " (:b "'JOIN'") 
		" are used.")
		(:img :src "/weblistenerdocs/bbldf/images/Repeat-Functions.jpg")
		(:p (:u "Explanation:"))
		(:ul (:p "1. The first "(:b "'DEFINE' ") "sets the initial *number* variable to 1."))
		(:ul (:p "2. The function "(:b "'MULTIPLY' ") " multiplies 5 by the *number* variable:"))
		(:ul (:p "3. The second "(:b "'DEFINE' ") "resets the initial *number* variable to the multiple
		5 times the *number* variable."))
		(:ul (:p "4. The functions "(:b "'REPEAT-FUNCTION' ") "and" (:b "'JOIN' ") "work in tandem.
		While the steps 2 and 3 are being repeated " (:b "'JOIN' ") "is attaching the result to a list
		separated by a comma, which is seen in the following result:"))
		(:ul (:img :src "/weblistenerdocs/bbldf/images/Repeat-Function_Number_Result.jpg"))
	)
	(:KEYWORDS
		Repeat Function Iteration
	)
	(:SEE-ALSO
		Repeat LOOP DEFINE-FUNCTION APPLY-FUNCTION
	)
)


; ============================ REPLACE ===========================
(DOCUMENT-FUNCTION bbl::replace
	(:SUMMARY
		"Replace part of a string or list with something else.")
	(:RETURNS 
		"The modified string or list, or a copy thereof."
		:TYPE (or list string)
	)
	(:SYNTAX
		(replace
		(into into-each into-copy-of into-copy-of-each)
		target
		(replacing-every replacing-first replacing-each)
		stuff-to-be-replaced
		from
		to
		at
		(with with-each)
		replacing-stuff
		(strict relaxed)
		(case-sensitive)
		)
	)
	(:PARAMETERS
		(mode
			:DOCSTRING "One of into, into-each, into-copy-of, or into-copy-of-each.
			Specifies whether the target string is to be copied or
			if possible, the replace is to be done to the target string
			instead of a copy. When specifying INTO-EACH or INTO-COPY-OF-EACH
			a list of strings or lists to be operated on must be provided."
			:PARAMETER-TYPE :TOKEN
			:VALUE-TYPE :boolean
			:DEFAULT-VALUE :into
		)
		(target
			:DOCSTRING "The string or list to operate on. The replace may be 
			done on the original string or list or a copy (see MODE)."
			:PARAMETER-TYPE required
			:VALUE-TYPE (or string list))
		(replacing-mode
			:DOCSTRING "One of replacing-every, replacing-first, or 
				replacing-each. Replace either every occurence of the 'TARGET', 
				the first occurence of the 'TARGET', or multiple different 
				subsequences. If REPLACING-EACH is specified, all occurences of 
				each subsequence are replaced; there is no way to replace only 
				the first occurence of multiple	subsequences using REPLACING-EACH. 
				If not specified, you must specify a range using either 'FROM' or 
				'TO', or 'AT'."
			:PARAMETER-TYPE :TOKEN
			:DEFAULT-VALUE nil
		)
		(stuff-to-be-replaced
			:DOCSTRING "The part of the sequence you want to be modified. 
			If not provided, you must provide a range using either FROM 
			or TO, or AT. This argument immediately follows REPLACING-MODE and 
			can only be supplied there."
			:PARAMETER-TYPE optional
			:DEFAULT-VALUE nil
		)
		(from
			:DOCSTRING "Where to begin the modification of the target."
			:PARAMETER-TYPE optional
			:VALUE-TYPE integer
			:DEFAULT-VALUE nil
		)
		(to
			:DOCSTRING "Where to end the modification of the target."
			:PARAMETER-TYPE optional
			:VALUE-TYPE integer
			:DEFAULT-VALUE nil
		)
		(at
			:DOCSTRING "Where to modify the target. You cannot specify 'AT'
			and also specify 'FROM' and/or 'TO'."
			:PARAMETER-TYPE optional
			:VALUE-TYPE integer
			:DEFAULT-VALUE nil
		)
		(with-mode
			:DOCSTRING "One of 'WITH' or 'WITH-EACH'. If 'WITH-EACH', 
			'REPLACING-STUFF' must be a list."
			:PARAMETER-TYPE :TOKEN
			:DEFAULT-VALUE :WITH
		)
		(replacing-stuff
			:DOCSTRING "Specifies what the stuff or range to be replaced
			is in fact replaced with. This must be provided."
			:PARAMETER-TYPE required
			:VALUE-TYPE any
		)
		(error-mode
			:DOCSTRING "One of Relaxed or Strict. If strict, out of bounds ranges
			result in an error. If relaxed, an index less than 1 is treated as 1
			and an index greater than the length of the string or list is treated
			as the length of the string or list."
			:PARAMETER-TYPE :FLAG
			:DEFAULT-VALUE :RELAXED
		)
		(case-sensitive
			:DOCSTRING "Specifies that the modification is case-sensitive.
			If this is not specified, then, for example, telling the function
			to replace a 'b' will replace both 'b' and 'B'."
			:PARAMETER-TYPE :FLAG
			:DEFAULT-VALUE nil)
	)
	(:EXAMPLES
		"Detailed below, graphically."
	)
	(:TEXT
		(:p (:b "How to use the function:"))
		(:p (:b "1."))(:img :src "/weblistenerdocs/bbldf/images/Replace_1s.jpg")
		(:p "")
		(:ul (:img :src "/weblistenerdocs/bbldf/images/Replace_1_Result.jpg"))
		(:p (:b "2."))(:img :src "/weblistenerdocs/bbldf/images/Replace_2.jpg")
		(:p "")
		(:ul (:img :src "/weblistenerdocs/bbldf/images/Replace_2_Result.jpg"))
		(:p (:b "3."))(:img :src "/weblistenerdocs/bbldf/images/Replace_3.jpg")
		(:p "")
		(:ul (:img :src "/weblistenerdocs/bbldf/images/Replace_3_Result.jpg"))
		(:p (:b "4."))(:img :src "/weblistenerdocs/bbldf/images/Replace_4.jpg")
		(:p "")
		(:ul (:img :src "/weblistenerdocs/bbldf/images/Replace_4_Result.jpg"))
		(:p (:b "5."))(:img :src "/weblistenerdocs/bbldf/images/Replace_5.jpg")
		(:p "")
		(:ul (:img :src "/weblistenerdocs/bbldf/images/Replace_5_Result.jpg"))
		(:p (:b "6."))(:img :src "/weblistenerdocs/bbldf/images/Replace_6.jpg")
		(:p "")
		(:ul (:img :src "/weblistenerdocs/bbldf/images/Replace_6_Result.jpg"))
		(:p (:b "7."))(:img :src "/weblistenerdocs/bbldf/images/Replace_End.jpg")
		(:p "")
		(:ul (:img :src "/weblistenerdocs/bbldf/images/Replace_End_Result.jpg"))
	)
	(:KEYWORDS
		Substitute Replace
	)
	(:SEE-ALSO 
		lisp:replace bbl::insert bbl::join bbl::make
	)
)


; ============================ REPLICON-NAMED ===========================
	(DOCUMENT-FUNCTION bbl::REPLICON-NAMED
		(:SUMMARY
			"Returns chromosome, replicon (plasmid) or contig's name (frame)."
		)
		(:SYNTAX
			(REPLICON-OF (CONTIG-NAME))
		)
		(:PARAMETERS
			(contig-name
				:DOCSTRING "Gene, protein, organism or contig (incl. chromosome 
				or replicon) to be evaluated"
				:PARAMETER-TYPE required
				:VALUE-TYPE string\,symbol
			)
		)
		(:RETURNS
			"A chromosome, replicon (plasmid) or contig's name (frame)."
		)
		(:EXAMPLES
			"Detailed below, graphically."
		)
		(:TEXT
			(:p (:b "How to use this function."))
                  (:OL
		      	(:li (:p "The full name can be entered...")
  			 		(:img :src "/weblistenerdocs/bbldf/images/Replicon-Named_1.jpg"))
				(:LI (:p "or a partial name can be entered.")
					(:img :src "/weblistenerdocs/bbldf/images/Replicon-Named_2.jpg"))
			)
			(:p "Both with the same result.")
			(:ul (:img :src "/weblistenerdocs/bbldf/images/Replicon-Named_Result.jpg"))
		)
		(:KEYWORDS
			replicon plasmid chromosome contig
		)
		(:SEE-ALSO
			CONTIG-OF REPLICON-OF ORGANISM-OF
		)
	)


; ============================ REPLICON/S-OF ===========================
	(DOCUMENT-FUNCTION REPLICON/S-OF
		(:SUMMARY
			"Returns chromosome, replicon (plasmid) or contig's name."
		)
		(:SYNTAX)
		(:PARAMETERS
			(entity
				:DOCSTRING "Gene, protein, organism or contig (incl. 
				chromosome or replicon) to be evaluated."
				:PARAMETER-TYPE required
				:VALUE-TYPE CONTIGUOUS-SEQUENCE\,ORGANISM\,GENE\,PROTEIN\,NULL
			)
		)
		(:RETURNS
			"An object of type LIST, CONTIGUOUS-SEQUENCE, LIST or NILL."
		)
		(:EXAMPLES
			"Detailed below, graphically."
		)
		(:TEXT
			(:p "In addition to the chromosome, cyanobacterial genomes may be 
			composed of other replicons (plasmids). The function REPLICON-OF 
			returns the replicons (incl. chromosome) of a given entity."
			(:ul (:li "Few genomes are not yet completed or the replicons of these 
			genomes are not yet defined in BioBIKE. For these genomes the function 
			returns the contigs.")
			(:li "The given entity may be a gene, a protein, an organism, or the 
			replicon (contig) itself. A simple or a complex list of these entities 
			is also valid.")))
		
			(:p (:b "How to use this function."))
			(:p (:b (:u "Single entity:")))
			(:p "1. A gene:")
			(:img :src "/weblistenerdocs/bbldf/images/Replicon-Of_1.jpg")
			(:p "")
			(:ul (:img :src "/weblistenerdocs/bbldf/images/Replicon-Of_1_Result.jpg"))
			(:p "2. A protein:")
			(:img :src "/weblistenerdocs/bbldf/images/Replicon-Of_2.jpg")
			(:p "")
			(:ul (:img :src "/weblistenerdocs/bbldf/images/Replicon-Of_2_Result.jpg"))
			(:p "3. An organism:")
			(:img :src "/weblistenerdocs/bbldf/images/Replicon-Of_3.jpg")
			(:p "")
			(:ul (:img :src "/weblistenerdocs/bbldf/images/Replicon-Of_3_Result.jpg"))
			(:p "4. A chromosome:")
			(:img :src "/weblistenerdocs/bbldf/images/Replicon-Of_4.jpg")
			(:p "")
			(:ul (:img :src "/weblistenerdocs/bbldf/images/Replicon-Of_4_Result.jpg"))
			
			(:p (:b (:u "A list of entities:")))
			(:p "1. Creates a list of orthologs of p-all5306 then finds the
			replicons for that list.")
			(:img :src "/weblistenerdocs/bbldf/images/Replicon-Of_5.jpg")
			(:p "")
			(:ul (:img :src "/weblistenerdocs/bbldf/images/Replicon-Of_5_Result.jpg"))
		)
		(:KEYWORDS
			Replicon Chromosome Contig Plasmid
		)
		(:SEE-ALSO
			ORGANISM-OF REPLICON-OF CONTIG-OF
		)
	)


; ============================ RESULT ===========================
(DOCUMENT-FUNCTION RESULT
	(:SUMMARY
		"Returns the Nth result."
	)
	(:PARAMETERS
		(n
			:DOCSTRING "The history index from which to retrieve the result."
			:PARAMETER-TYPE required
			:VALUE-TYPE integer
		)
		(value-index
			:DOCSTRING "Which multiple value to return"
			:PARAMETER-TYPE optional
			:VALUE-TYPE integer
		)
	)
	(:RETURNS
		"One of the values (default first) of the result indicated by N."
	)
	(:TEXT
		(:ul (:b "NOTE: ") (:i "This function is best utilized when there are many results to scroll 
		through in the result window."))
		(:p "A result can be called upon by its result number. For instance if a user wanted to see
		what the result was for result number '48>' in the following results which is 35:")
		(:img :src "/weblistenerdocs/bbldf/images/Result_Result.jpg")
		(:p "Then a user would input 48.")
		(:img :src "/weblistenerdocs/bbldf/images/Result.jpg")
		(:p "And the result will be displayed again (represented by '51>').")
		(:img :src "/weblistenerdocs/bbldf/images/Result_Result2.jpg")
	)
	(:KEYWORDS
		Answer Result
	)
	(:SEE-ALSO
		Previous-Result RETURN RETURN-FROM
	)
)


; ================================== REVERSE =========================================
(DOCUMENT-FUNCTION REVERSE
	(:SUMMARY 
		"Return a new string or list, containing the same elements, but in 
		reversed order."
	)
	(:SYNTAX
		(REVERSE string)
	)
	(:PARAMETERS
		(string 
			:DOCSTRING "The series of characters you would like to reverse the 
			order of."
			:VALUE-TYPE string\,list
		)
	)
	(:RETURNS 
		"A string or list."
	)
	(:EXAMPLES
		"Detailed below, graphically."
	)
	(:TEXT
		(:p (:b "1. A string being reversed."))
		(:img :src "/weblistenerdocs/bbldf/images/Reverse.jpg")
		(:p "")
		(:ul (:img :src "/weblistenerdocs/bbldf/images/Reverse_Result.jpg"))
		(:ul (:p (:b "NOTE: ") " An error message of \"type-error\" would be sent if the 
		string enter is not a proper string type."))
		(:p (:b "2. A list of items can also be reversed."))
		(:p "The variable defined " (:i "f-cyano ") "contains a list of proteins.")
		(:img :src "/weblistenerdocs/bbldf/images/Reverse_List_Before.jpg")
		(:p "After executing the function...")
		(:img :src "/weblistenerdocs/bbldf/images/Reverse_List.jpg")
		(:p "the list has been reversed.")
		(:ul (:img :src "/weblistenerdocs/bbldf/images/Reverse_List_After.jpg"))
	)
	(:KEYWORDS
		reverse backwards
	)
	(:SEE-ALSO
		Inversion-Of
	)
)

; ================================== ROUND =========================================
(DOCUMENT-FUNCTION bbl::ROUND
	(:SUMMARY
		"Rounds a number up or down."
	)
	(:PARAMETERS
		(number
			:DOCSTRING "The number to be rounded."
			:PARAMETER-TYPE required
			:VALUE-TYPE number
		)
		(down
			:DOCSTRING "Rounds number down to nearest whole number."
			:PARAMETER-TYPE :FLAG
		)
		(up
			:DOCSTRING "Rounds number up to nearest whole number"
			:PARAMETER-TYPE :FLAG
		)
		(to-nearest
			:DOCSTRING "Rounds to the nearest multiple."
			:PARAMETER-TYPE optional
			:VALUE-TYPE number
		)
	)
	(:TEXT
		(:p (:b "1."))(:img :src "/weblistenerdocs/bbldf/images/Round_Down.jpg")
		(:p "")
		(:ul (:img :src "/weblistenerdocs/bbldf/images/Round_Down_Result.jpg"))
		(:p (:b "2."))(:img :src "/weblistenerdocs/bbldf/images/Round_Up.jpg")
		(:p "")
		(:ul (:img :src "/weblistenerdocs/bbldf/images/Round_Up_Result.jpg"))
		(:p (:b "3. In this example 350.8 is rounded to the nearest multiple of 20."))
		(:img :src "/weblistenerdocs/bbldf/images/Round_To-Nearest.jpg")
		(:p "")
		(:ul (:img :src "/weblistenerdocs/bbldf/images/Round_To-Nearest_Result.jpg"))
	)
	(:KEYWORDS
		Rounding Nearest
	)
	(:SEE-ALSO
		RANDOM-NUMBER LIST RANDOM-INTEGER IS-NUMBER
	)
)