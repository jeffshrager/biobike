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

;;; Authors: Tara Nulton, Jeff Elhai

;================================= ABS (ABSOLUTE VALUE) ==============================
(DOCUMENT-FUNCTION bbl::ABS
	
	(:SUMMARY
			"Returns the absolute value of a number."
	)
	
	
	(:SYNTAX (ABS number))
			
	(:PARAMETERS 
		(number
			:PARAMETER-TYPE number
			:VALUE-TYPE number 
			:DOCSTRING "the number you are taking the absolute value of")
	)
	
	(:RETURNS "A number" )

	(:EXAMPLES "are illustrated below."
	)
	
	(:TEXT
	(:p (:b "1. Shows simplistic function."))
	(:img :src "/weblistenerdocs/bbldf/images/ABS-1.jpg")
	(:p (:b "Results:"))
	(:img :src "/weblistenerdocs/bbldf/images/ABS-1-RESULT.jpg")
	(:p (:b "2. Another example."))
	(:img :src "/weblistenerdocs/bbldf/images/ABS-2.jpg")
	(:p (:b "Results:"))
	(:img :src "/weblistenerdocs/bbldf/images/ABS-2-RESULT.jpg")
	(:p "If the number is a real, the result is the same type of number.
		If the number is complex, the result is a positive real with the same magnitude as the number.  
		The result can be a floating number even if the number's components are rational 
		and an exact rational result would have been possible.")
	(:p "It is unnecessary to use ABS with the SQRT function because SQRT does not return the 
		positive and negative values of an answer.")
	)

	(:KEYWORDS
		"Absolute value, real, integer, magnitude"
	)
	
	(:SEE-ALSO * - )
	
)

;================================= ACOS (ARC COSINE) ==============================


(DOCUMENT-FUNCTION ACOS
	
	(:SUMMARY "Returns the inverse (arc) cosine (acos) in radians")
  
	(:SYNTAX (ACOS n))
		
	(:PARAMETERS 
		(n 
		:VALUE-TYPE number 
		:DOCSTRING "the number in radians you want the arc 
		cosine of (usually between -1 and 1)"
		)
	)
		
	(:RETURNS " a number between 0 and 3.14159")
  
	(:EXAMPLES "are shown below."
	)
  
  (:TEXT
	(:p  (:b "1. A simple example."))
	(:img :src "/weblistenerdocs/bbldf/images/ACOS-1.jpg")
	(:p  (:b "Result:"))
	(:img :src "/weblistenerdocs/bbldf/images/ACOS-1-RESULT.jpg")
	(:p  (:b "2. Divides the square root of 3 by 2, then takes the arc cosine."))
	(:img :src "/weblistenerdocs/bbldf/images/ACOS-2.jpg")
	(:p  (:b "Result:"))
	(:img :src "/weblistenerdocs/bbldf/images/ACOS-2-RESULT.jpg")
   (:p "Arc cosine is synonymous with inverse cosine.")
   (:p "This function for arc cosine requires an argument in radians and returns the angle in radians whose cosine is the argument you provide.")
   (:p "The argument provided is usually between -1 and 1, if not you will get a complex number.")
   (:p "The returned values will be between 0 and PI. (PI = 3.14159)")
	)

	(:SEE-ALSO COS ASIN ATAN
	)  


	(:KEYWORDS
	" Cosine, inverse cosine, trigonometric function, trig, arc cosine, acos"
	)
)

;================================= ADD-SET ==============================
(DOCUMENT-FUNCTION ADD-SET
	
	(:SUMMARY
			"Combines lists or items."
	)
	
	(:SYNTAX (ADD-SET)
		(ADD-SET set [AND | BY | TO] set-or-item [CASE-SENSITIVE])
		(ADD-SETS set [AND | BY | TO] set-or-item [CASE-SENSITIVE])
	)
	
	(:PARAMETERS
		(SET 
			:PARAMETER-TYPE required
			:VALUE-TYPE LIST  
			:DOCSTRING "The argument that the user starts with."
		)

		(SET-OR-ITEM 
			:PARAMETER-TYPE any  
			:DOCSTRING "Can be a list or an item"
		)

		(CASE-SENSITIVE 
			:PARAMETER-TYPE :FLAG 
			:DOCSTRING "Notices whether the characters belong to uppercase and lowercase families."
		)
	)

	(:RETURNS 
		"A list which removes duplicates."
	)
	
	(:TEXT
		(:p (:b "1. Suppose you wanted to know whether the codon frequencies of organism x are reasonably 
		similar to Avar and Npun.  This function allows manipulation of data at the same time."))
		(:img :src "/weblistenerdocs/bbldf/images/ADD-SET-1.jpg")
		(:p (:b "Result:"))
		(:img :src "/weblistenerdocs/bbldf/images/ADD-SET-1-RESULT.jpg")
		(:ul (:p (:li (:b "Note: The results are too large to be viewed in entirety.
		DISPLAY DATA makes for an easier read, but the default output limit must still be remedied by going 
		to the black File tab and selecting Preferences."))))
		(:img :src "/weblistenerdocs/bbldf/images/ADD-SET-2.jpg")
		(:img :src "/weblistenerdocs/bbldf/images/ADD-SET-2-RESULT.jpg")
		(:ul (:p (:li (:b "2.  This function combines the coding genes of Avar and Npun, calculates the codon frequencies of the two
		combined, and then interleaves (like shuffling cards) the names of of codons."))))
		(:ul (:img :src "/weblistenerdocs/bbldf/images/ADD-SET-3.jpg"))
		(:ul (:img :src "/weblistenerdocs/bbldf/images/ADD-SET-3-RESULT.jpg"))
		
	)

(:KEYWORDS
"list, set, adding set, making list, making set, adding item, 
adding sequence, adding organism" 
)

(:SEE-ALSO * - )
)



;================================= ALIGN-BLAST-RESULT ==============================
(DOCUMENT-FUNCTION ALIGN-BLAST-RESULT

	
	(:SUMMARY
			"Visual alignment of BLAST result. " 
	)
	
	(:SYNTAX
			(ALIGN-BLAST-RESULT blast-result [FROM positive-number] [TO positive-number] 
			[ITEMS positive-number-or-list] [LINE-LENGTH any] [GROUP-LENGTH any])
	)	
		
	(:PARAMETERS
		(BLAST-RESULT
			:PARAMETER-TYPE required  
			:VALUE-TYPE any
			:DOCSTRING "Use SEQUENCE-SIMILAR-TO to obtain BLAST results."
		)
		
		(FROM
			:PARAMETER-TYPE keyword
			:VALUE-TYPE POSITIVE-NUMBER  
			:DOCSTRING "Only includes a subset of BLAST results, e.g. FROM 1 TO 10."  
		)

		(TO 
			:PARAMETER-TYPE keyword
			:VALUE-TYPE POSITIVE-NUMBER  
			:DOCSTRING "Only include portions of the BLAST results, e.g. FROM 1 TO 5."  
		)

		(ITEMS 
			:PARAMETER-TYPE keyword
			:VALUE-TYPE POSITIVE-NUMBER\,LIST  
		)

		(LINE-LENGTH 
			:PARAMETER-TYPE keyword
			:VALUE-TYPE any 
			:DOCSTRING  "Used to view the alignment by groups of 10, 20, etc."
		  
		)

		(GROUP-LENGTH 
			:PARAMETER-TYPE keyword
			:VALUE-TYPE any 
			:DOCSTRING "Groups amino acids according to the value used."  
		)
	)
  
	(:RETURNS "a visual alignment.")


	(:EXAMPLES "See Below for illustrations."
	)
  
	(:TEXT
		(:p (:b  "BLAST searches for sequences similarities, either nucleotide or protein.  This
		function is known in bioBIKE as SEQUENCE-SIMILAR-TO."))
		(:ul (:p (:li (:b "1. What should appear on screen in the most simplistic 
		example of ALIGN-BLAST-RESULT"))))
		(:ul (:img :src "/weblistenerdocs/bbldf/images/ALIGN-BLAST-RESULT-1.jpg"))
		(:ul (:p (:li (:b "Results will display in a pop-up window:"))))
		(:ul (:img :src "/weblistenerdocs/bbldf/images/ALIGN-BLAST-RESULT-2.jpg"))
		(:p (:b "Note: The order of the alignment is determined by the numbered results obtained 
		from SEQUENCE-SIMILAR-TO. The genome coordinates are shown in the second column."))
		(:ul (:p (:li (:b "2. Option utilization."))))
		(:img :src "/weblistenerdocs/bbldf/images/ALIGN-BLAST-RESULT-4.1.jpg")
		(:ul (:p (:li (:b "Results:"))))
		(:ul (:img :src "/weblistenerdocs/bbldf/images/ALIGN-BLAST-RESULT-4.2.jpg"))	
	)
	
  
  (:KEYWORDS "Align, line up, Sequence-Similar-To, BLAST, line-up, Arrange, Result, Conserved"
  )
  
  (:SEE-ALSO SEQUENCE-SIMILAR-TO ALIGNMENT-OF
  )
 )  


;================================= ALIGNMENT-OF ==============================
(DOCUMENT-FUNCTION ALIGNMENT-OF

	(:SUMMARY
			"Uses CLUSTAL to align given sequences." 
	)
	
	(:SYNTAX
			(ALIGNMENT-OF sequence-list [REMOVE-DUPLICATES] [NO-CONSENSUS] [NO-RETURN] 
			[NO-DISPLAY] [COLORED] [LABEL-WITH-ORGANISM] [LABEL-WITH-NICKNAME] [NO-GAPPED-COLUMNS] 
			[LINE-LENGTH null-or-nonnegative-number] [GROUP-LENGTH null-or-nonnegative-number] 
			[GAP-OPEN-PENALTY nonnegative-number] [GAP-EXTENSION-PENALTY nonnegative-number])
	)
			
	(:PARAMETERS
		(SEQUENCE-LIST 
			:PARAMETER-TYPE required  
			:VALUE-TYPE LIST
			:DOCSTRING "The list must be a gene, protein, organism, contiguous sequence, labeled sequence
			frame, domain, string, or symbol."
		)

		(REMOVE-DUPLICATES 
			:PARAMETER-TYPE :FLAG
			:DOCSTRING "Removes duplicate sequences that may be represented more than one time."
		)
		
		(NO-CONSENSUS 
			:PARAMETER-TYPE :FLAG
			:DOCSTRING "Removes the consensus line which can be seen in the first example."
		)
	
		(NO-RETURN 
			:PARAMETER-TYPE :FLAG
			:DOCSTRING "Results will not display in a pop-up window."
		)
		
		(NO-DISPLAY 
			:PARAMETER-TYPE :FLAG
			:DOCSTRING "Will not display the results."
		)

		(COLORED 
			:PARAMETER-TYPE :FLAG
			:DOCSTRING "Presents colored consensus, conservation scores, and quality in bar graph format.  See
			examples two and three."
		)
	
		(LABEL-WITH-ORGANISM 
			:PARAMETER-TYPE :FLAG
			:DOCSTRING "Labels sequences with name of organisms."
		)
	
		(LABEL-WITH-NICKNAME
			:PARAMETER-TYPE :FLAG
			:DOCSTRING "Labels sequences with nickname of organisms."
		)
	
		(NO-GAPPED-COLUMNS 		
			:PARAMETER-TYPE :FLAG
			:DOCSTRING "Aligns sequences at every place, by removing gaps that the program feels
			creates a more pleasing alignment."
		)

		(LINE-LENGTH 
			:PARAMETER-TYPE keyword
			:VALUE-TYPE NONNEGATIVE-NUMBER 
			:DOCSTRING "Length of sequence per line."  
		)
		
		(GROUP-LENGTH 
			:PARAMETER-TYPE keyword
			:VALUE-TYPE NONNEGATIVE-NUMBER 
			:DOCSTRING "Groups sequences for easier viewing."  
		)

		(GAP-OPEN-PENALTY 
			:PARAMETER-TYPE keyword
			:VALUE-TYPE NONNEGATIVE-NUMBER
			 
		)

		(GAP-EXTENSION-PENALTY 
			:PARAMETER-TYPE keyword
			:VALUE-TYPE NONNEGATIVE-NUMBER
			
		)
	)
	
	(:RETURNS "an alignment.")


	(:EXAMPLES "Illustrated below."
	)
	
	
	(:TEXT
		(:p (:b "The argument MUST be of the type GENE,PROTEIN,ORGANISM, CONTIGUOUS-SEQUENCE, 
		LABELED-SEQUENCE, DOMAIN, STRING, SYMBOL, or FRAME."))
		(:p (:b "Note:  This function looks at the specifics of the amino acid substution in order to determine
		which are poor, fair, or complete matches e.g., serine is very rarely substituted by tryptophan, serine is more often substituted by glycine,
		serine is very frequently substituted by threonine.  Those conservation scores are represented by
		the asterick signifies which signifies a complete match, the colon represents high conservation, 
		and the period represents low conservation. The -- between a sequence signifies a gap in the alignment."))
		(:ul (:p (:li (:b "1. Alignment of two proteins."))))
		(:img :src "/weblistenerdocs/bbldf/images/ALIGNMENT-OF-1.jpg")
		(:ul (:p (:li (:b "Result displayed in pop-up window:"))))
		(:img :src "/weblistenerdocs/bbldf/images/ALIGNMENT-OF-1-RESULT.jpg")
		 
		(:ul (:p (:li (:b "Results when the option COLORED is specified:"))))
		(:img :src "/weblistenerdocs/bbldf/images/ALIGNMENT-OF-2.jpg")
		(:img :src "/weblistenerdocs/bbldf/images/ALIGNMENT-OF-3.jpg")
		(:ul (:p (:li (:b "2.  Same function with options specified."))))
		(:img :src "/weblistenerdocs/bbldf/images/ALIGNMENT-OF-4.jpg")
		(:ul (:p (:li (:b "Result displayed in pop-up window:"))))
		(:img :src "/weblistenerdocs/bbldf/images/ALIGNMENT-OF-5.jpg")
	)
	
	(:KEYWORDS
	"Alignment, position, arrangement, configuration"
	)
	
	(:SEE-ALSO ALIGN-BLAST-RESULT 
	)
)

;================================= ALL-COMBINATIONS-OF ==============================
(DOCUMENT-FUNCTION ALL-COMBINATIONS-OF
	
	(:SUMMARY
		"Generates all combinations of a given alphabet of a specified length."
	)

	(:SYNTAX
	)
	
	(:PARAMETERS
		(ALPHABET 
			:PARAMETER-TYPE required
			:VALUE-TYPE STRING\,LIST\,LABELED-SEQUENCE
			:DOCSTRING "To make a string use quotes, for a list, use parentheses or the LIST function."
		)

		(GIVEN-LENGTH 
			:PARAMETER-TYPE required
			:VALUE-TYPE POSITIVE-NUMBER  
			:DOCSTRING "The length of the combinations."  
		)
		
		(I-MEAN-IT 
			:PARAMETER-TYPE :FLAG
			:DOCSTRING "This function has the capability of generating unlimited data.  Specify
			this option if you are certain that possibly endless data what you require."
		)
			
		(ALPHABETIZED
			:PARAMETER-TYPE :FLAG 
			:DOCSTRING "Helpful when wanting to view results in an alphabetical fashion."  
		)
	)
	
	(:RETURNS "An object of type LIST or T."
	)
	
	(:EXAMPLES "Examples shown below."
	)
	
	(:TEXT
		(:p (:b "1. A simple example."))
		(:ul (:img :src "/weblistenerdocs/bbldf/images/ALL-COMBINATIONS-OF-5.jpg"))
		(:p (:b "Results:"))
		(:ul (:img :src "/weblistenerdocs/bbldf/images/ALL-COMBINATIONS-OF-5-RESULT.jpg"))
		(:p (:b "2. The function with length increased to five."))
		(:ul (:img :src "/weblistenerdocs/bbldf/images/ALL-COMBINATIONS-OF.jpg"))
		(:p (:b "Results:"))
		(:ul (:img :src "/weblistenerdocs/bbldf/images/ALL-COMBINATIONS-OF-2.jpg"))
		(:p (:b  "Note: By mousing over to the green arrow on the left of the results pane and
		selecting \"view\", one can see all the output in numbered column fashion."))
		(:ul (:img :src "/weblistenerdocs/bbldf/images/ALL-COMBINATIONS-OF-3.jpg"))
		(:p (:b "3. By using the DISPLAY-DATA function and specifying the EACH option, the data
		can be viewed in entirety within one screen."))
		(:ul (:img :src "/weblistenerdocs/bbldf/images/ALL-COMBINATIONS-OF-6.jpg"))
		(:p (:b "Results:"))
		(:ul (:img :src "/weblistenerdocs/bbldf/images/ALL-COMBINATIONS-OF-6-RESULT.jpg"))
		(:p (:b "4. Realize that if spaces are put between characters, the spaces will be treated
		as a characters themselves, as shown below."))
		(:ul (:img :src "/weblistenerdocs/bbldf/images/ALL-COMBINATIONS-OF4.jpg"))
		(:ul (:img :src "/weblistenerdocs/bbldf/images/ALL-COMBINATIONS-OF4-RESULT.jpg"))
		(:p (:b "5. This function receives the error report shown as a pop-up message, due to
		the length of combinations specified."))
		(:ul (:img :src "/weblistenerdocs/bbldf/images/ALL-COMBI.jpg"))
		(:p (:b "Results (error report:"))
		(:ul (:img :src "/weblistenerdocs/bbldf/images/I-MEAN-IT-REPORT.jpg"))
		

		
	)

	
	(:KEYWORDS "Mixture, Grouping, Blend, Combination, Combine, Pattern, Order"
	)
	
	(:SEE-ALSO
	)
)

;================================= ALL-DNA-SEQUENCES ==============================
(DOCUMENT-FUNCTION ALL-DNA-SEQUENCES

	(:SUMMARY "Generates all oligomeric DNA sequences of a specified length."
	)

	(:SYNTAX
	)
	
	(:PARAMETERS
		(GIVEN-LENGTH 
			:PARAMETER-TYPE required
			:VALUE-TYPE POSITIVE-NUMBER
			:DOCSTRING "Specifies the length of the sequence."  
		)

		(I-MEAN-IT 
			:PARAMETER-TYPE :FLAG 
			:DOCSTRING "This function has the capability of producing gi-normous amounts of 
			data.  Specify this option when size makes no matter."
		)		
	)
	
	(:RETURNS "an object of type LIST or T."
	)

	(:EXAMPLES "Shown below graphically."
	)
	
	(:TEXT
		(:p (:b "1. A simple example."))
		(:img :src "/weblistenerdocs/bbldf/images/ALL-DNA-SEQ-1.jpg")
		(:p (:b "Results:"))
		(:img :src "/weblistenerdocs/bbldf/images/ALL-DNA-SEQ-1-RESULT.jpg")
		(:p (:b "2.  ALL-DNA-SEQUENCES of length 8."))
		(:img :src "/weblistenerdocs/bbldf/images/ALL-DNA-SEQ-2.jpg")
		(:p (:b "Results:"))
		(:img :src "/weblistenerdocs/bbldf/images/ALL-DNA-SEQ-2-RESULT.jpg")
		(:p (:b "Note:  The results can be viewed in entirety by mousing over the green arrow in the 
		results section and selecting \"View\"."))
		(:p (:b "3. This function receives the error report shown as a pop-up message, due to
		the length of combinations specified."))
		(:img :src "/weblistenerdocs/bbldf/images/ALL-COMBINATIONS-OF-8.jpg")
		(:p (:b "Results (error report:"))
		(:ul (:img :src "/weblistenerdocs/bbldf/images/I-MEAN-IT-REPORT.jpg"))
	)
	
	(:KEYWORDS "DNA, Sequence, Combinations"
	)
	
	(:SEE-ALSO ALL-COMBINATIONS-OF
	)
	
)
	
;================================= ALL-FALSE ==============================

(DOCUMENT-FUNCTION ALL-FALSE (a.k.a. NONE-TRUE NONE-NIL)

	(:SUMMARY 
		"Returns TRUE (T) if all of the arguments are NIL; otherwise returns FALSE (NIL)."
	)

	(:SYNTAX
		(ALL-FALSE arguments)
		(ALL-NIL arguments)
		(NONE-TRUE arguments)
	)

	(:PARAMETERS
		(ARGUMENTS 
			:PARAMETER-TYPE required
			:VALUE-TYPE LABELED-SEQUENCE\,LIST  
			:DOCSTRING "A list of arguments. A list can be made by using the list function, or
			by enclosing data in parentheses."
		)
	)

	(:RETURNS "An object of type T or NULL."
	)

	(:EXAMPLES "are illustrated below."
	)
	
	(:TEXT
		(:p (:b "The function ALL-FALSE returns TRUE (T) if all of the arguments within a list")) 
		(:p (:b "are FALSE; otherwise returns FALSE (NIL)."))

		(:p (:b " ALL-FALSE follows the conventions of true and false."))
		(:p (:b " ALL-FALSE is often called by conditionals."))
		
		(:p (:b "1. A simple example using arithmetic expression."))  
		(:img :src "/weblistenerdocs/bbldf/images/ALL-FALSE.jpg")
		(:p (:b "Results:"))
		(:img :src "/weblistenerdocs/bbldf/images/ALL-FALSE-RESULT.jpg")
		(:p (:b "Note: Notice that the lists (enclosed in parentheses) are nested within the {} function."))
		(:p (:b "2. Example using arithmetic expressions returning T."))  
		(:img :src "/weblistenerdocs/bbldf/images/ALL-FALSE-2.jpg")
		(:p (:b "Results:"))
		(:img :src "/weblistenerdocs/bbldf/images/ALL-FALSE-2-RESULT.jpg")
		
	)
	
	(:KEYWORDS "False, Conditionals, True and False"
	)
	
	(:SEE-ALSO
	"IF, ALL-TRUE, ANY-TRUE, ANY-FALSE, IF-TRUE, IF-FALSE, True and False, Conditionals."
	)

)


;================================= ALL-PROTEIN-SEQUENCES ==============================
(DOCUMENT-FUNCTION ALL-PROTEIN-SEQUENCES

	(:SUMARY "Generates all oligomeric protein sequences of a specified length."
	)

	(:SYNTAX 
	)

	(:PARAMETERS
		(GIVEN-LENGTH 
			:PARAMETER-TYPE required
			:VALUE-TYPE POSITIVE-NUMBER
			:DOCSTRING "Generates sequences of specified length." 
		)
	
		(I-MEAN-IT 
			:PARAMETER-TYPE :FLAG
			:DOCSTRING "This function has the capability of creating more bytes of data than
			electrons in the universe.  Specify this option if size doesn't matter."
		)
	)

	(:RETURNS "An object of type LIST or T."
	)

	(:EXAMPLES "examples are illustrated below."
	)
	
	(:TEXT
		(:p (:b "1. A simple example of length 2."))  
		(:img :src "/weblistenerdocs/bbldf/images/ALL-PROTEIN-SEQUENCES-1.jpg")
		(:p (:b "Results:"))
		(:img :src "/weblistenerdocs/bbldf/images/ALL-PROTEIN-SEQUENCES-1-RESULT.jpg")
		(:p (:b "Note:  The output is already too large to be displayed in entirety, use DISPLAY-LIST and specify the EACH option
		to view total results."))
		(:img :src "/weblistenerdocs/bbldf/images/ALL-PROTEIN-SEQUENCES-2.jpg")
		(:p (:b "Results:"))
		(:img :src "/weblistenerdocs/bbldf/images/ALL-PROTEIN-SEQUENCES-2-RESULT.jpg")
		
	)
	
	(:KEYWORDS "Protein, Protein Sequence, Random, RANDOMIZE, All sequences."
	)

	(:SEE-ALSO "ALL-COMBINATIONS-OF ALL-DNA-SEQUENCES"
	)

)

;================================= ALL-SAME ==============================

(DOCUMENT-FUNCTION ALL-SAME

	(:SUMMARY "Returns true if all elements in the list have the same value."
	)

	(:SYNTAX(ALL-SAME thing [CASE-SENSITIVE])
	)

	(:PARAMETERS
		(THING 
		:PARAMETER-TYPE required
		:VALUE-TYPE LABELED-SEQUENCE\,LIST  
		:DOCSTRING  "Use the LIST function in order to make a list or use parentheses." 
	)
	
	(CASE-SENSITIVE 
		:PARAMETER-TYPE :FLAG  
		:DOCSTRING "Differentiates between upper and lower case lettering."
	)	
	)

	(:RETURNS "T for true or NIL for false."
	)

	(:EXAMPLES "Shown below, graphically."
	)


	(:TEXT
		(:p (:b "1. A simple example."))
		(:img :src "/weblistenerdocs/bbldf/images/ALL-SAME.jpg")
		(:p (:b "Results:"))
		(:img :src "/weblistenerdocs/bbldf/images/ALL-SAME-RESULT.jpg")
		(:p (:b "2. Another example when the elements of the list are not the same."))
		(:ul (:img :src "/weblistenerdocs/bbldf/images/ALL-SAME-2.jpg"))
		(:p (:b "Results:"))
		(:img :src "/weblistenerdocs/bbldf/images/ALL-SAME-2-RESULT.jpg")
		
		)


(:KEYWORDS "Equivalent, same, identical, comparison, ALL-TRUE"
)

(:SEE-ALSO "ALL-TRUE, ALL-FALSE"
)

)

;================================= ALL-TRUE ==============================

(DOCUMENT-FUNCTION ALL-TRUE (a.k.a. NONE-FALSE NONE-NIL)


	(:SUMMARY
		"Returns TRUE (T) if all of the arguments are true; otherwise returns FALSE (NIL)."
	)

	(:SYNTAX
		(ALL-TRUE arguments)
		(NONE-FALSE arguments)
		(NONE-NIL arguments)
	)

	(:PARAMETERS
		(ARGUMENTS 
		:PARAMETER-TYPE required
		:VALUE-TYPE LABELED-SEQUENCE\,LIST  
		:DOCSTRING "Use the LIST function or enclose a single data object in parentheses."
		)
	)
	
	(:RETURNS
		"An object of type T or NULL."
	)
	
	(:EXAMPLES "are illustrated below."
	)
	
	(:TEXT
		(:p (:b "ALL-TRUE follows the conventions of true and false. ALL-TRUE is often 
		called by conditionals."))
		(:p (:b "1. If given a list of arithmetic expressions"))
		(:img :src "/weblistenerdocs/bbldf/images/ALL-TRUE-1.jpg")
		(:p (:b "Results:"))
		(:img :src "/weblistenerdocs/bbldf/images/ALL-TRUE-2.jpg")
		(:p (:b "2. An example when at least one argument is false."))
		(:img :src "/weblistenerdocs/bbldf/images/ALL-TRUE-3.jpg")
		(:p (:b "Results:"))
		(:img :src "/weblistenerdocs/bbldf/images/ALL-TRUE-4.jpg")
	)
		
	
	(:KEYWORDS
		"ANY-FALSE, ALL-FALSE, ANY-TRUE, True and False, Conditionals, Equivalent, same, 
		identical, comparison"
	)
 
	(:SEE-ALSO
		"ALL-FALSE, ANY-TRUE, ANY-FALSE, IF-TRUE, IF-FALSE"
	)

)

;================================= ALPHABET ==============================

(DOCUMENT-FUNCTION ALPHABET-OF


	(:SUMMARY
		"Returns minimal string of characters contained in input."
	)

	(:SYNTAX(ALPHABET-OF string-or-list)
	)

	(:PARAMETERS
		(STRING-OR-LIST 
			:PARAMETER-TYPE REQUIRED  
			:VALUE-TYPE STRING\,LIST\,LABELED-SEQUENCE  
			:DOCSTRING "A string can be made by enclosing with quotes. 
			A list can be made by parentheses or the LIST function."
		)
	)

	(:RETURNS 
		"An object of type STRING."
	)

	(:EXAMPLES 
		"Illustrated below."
	)
	
	(:TEXT
		(:p (:b "1. A simple example of ALPHABET-OF"))
		(:img :src "/weblistenerdocs/bbldf/images/ALPHABET-OF.jpg")
		(:ul (:p (:b "Results:")))
		(:ul (:img :src "/weblistenerdocs/bbldf/images/ALPHABET-OF-RESULT.jpg"))
		(:p (:b "2.  Another example."))
		(:img :src "/weblistenerdocs/bbldf/images/ALPHABET-OF-2.jpg")
		(:ul (:p (:b "Results:")))
		(:ul(:img :src "/weblistenerdocs/bbldf/images/ALPHABET-OF-2-RESULT.jpg"))
	)
	
	(:KEYWORDS
		"alphabet, removing alphabet, one occurence, no duplicates, letters."
	)
	
	(:SEE-ALSO
	)
		
)

;============================== AMINO-ACID-COUNTS-OF =======================
(DOCUMENT-FUNCTION AMINO-ACID-COUNTS-OF


	(:SUMMARY
		"Returns a list, with each number corresponding to the count of the 20 amino 
		acids in the concatenation of all the sequences."
	)
	
	(:SYNTAX
		(AMINO-ACID-COUNTS-OF entity [TRANSLATE])
	)

	(:PARAMETERS
		(ENTITY
			:PARAMETER-TYPE required
			:VALUE-TYPE STRING\,LABELED-SEQUENCE\,GENE\,PROTEIN\,ORGANISM\,CONTIGUOUS-SEQUENCE\,LIST    
			
		)

		(TRANSLATE
			:PARAMETER-TYPE :FLAG 
			:DOCSTRING "Use to translate DNA into amino acids."
		)
	)
	
	(:RETURNS "A list in alphabetical order by amino acid."
	)
	
	(:EXAMPLES "are illustrated below."
	)
	
	(:TEXT
		(:p (:b "1. A simple example."))
		(:img :src "/weblistenerdocs/bbldf/images/AMINO-ACID-COUNTS.jpg")
		(:p (:b "Results:"))
		(:img :src "/weblistenerdocs/bbldf/images/AMINO-ACID-COUNTS-OF-1-R.jpg")
		(:p (:b "2.  An example using the translate option."))
		(:img :src "/weblistenerdocs/bbldf/images/AMINO-ACID-COUNTS-2.jpg")
		(:p (:b "Results:"))
		(:img :src "/weblistenerdocs/bbldf/images/AMINO-ACID-COUNTS-OF-2R.jpg")
	)
	(:KEYWORDS "amino acids, count, counting, counts, number, codons."
	)
	
	(:SEE-ALSO "COUNT-OF, COUNTS-OF"
	)	
)
	
;=================================AMINO-ACID-FREQUENCIES-OF  ==============================
(DOCUMENT-FUNCTION AMINO-ACID-FREQUENCIES-OF

(:SUMMARY
	"Returns a list, with each number corresponding to the frequency 
	of the 20 amino acids in the concatenation of all the sequences."
)

(:SYNTAX
	(AMINO-ACID-FREQUENCIES-OF entity [TRANSLATE])
)

(:PARAMETERS
	(ENTITY 
	:PARAMETER-TYPE required
	:VALUE-TYPE any
	:DOCSTRING "The sequence (can be amino acid or DNA)."
	)

	(TRANSLATE
	:PARAMETER-TYPE :FLAG
	:DOCSTRING "When specified, will translate DNA into amino acids."
	)
)

(:RETURNS "A list in alphabetical order of amino acid."
)

(:EXAMPLES "are illustrated below."
)

(:TEXT
		(:p (:b "1. A simple example."))
		(:img :src "/weblistenerdocs/bbldf/images/AMINO-ACID-FREQUENCIES-OF-1.jpg")
		(:p (:b "Results:"))
		(:img :src "/weblistenerdocs/bbldf/images/AMINO-ACID-FREQUENCIES-OF-1-RESULT.jpg")
		(:ul(:p (:li(:b "Note: The frequencies are not labeled with the name of the amino acid.
		See the next example."))))
		(:p (:b "2. This example INTERLEAVES (which is like shuffling cards) AMINO-ACID-FREQUENCIES-OF
		with NAME-OF.  The argument of NAME-OF is amino-acids, which can only be found in the blue DATA
		tab along the top of the bioBIKE screen."))
		(:img :src "/weblistenerdocs/bbldf/images/AMINO-ACID-FREQUENCIES-OF-2.jpg")
		(:p (:b "Results:"))
		(:img :src "/weblistenerdocs/bbldf/images/AMINO-ACID-FREQUENCIES-OF-2-RESULT.jpg")
)

(:KEYWORDS "frequencies, frequency, amino acid, codon frequency, occurences"
)

(:SEE-ALSO "COUNT-OF, COUNTS-OF, AMINO-ACID-COUNTS-OF"
)

)

;================================= ANY-FALSE ==============================
(DOCUMENT-FUNCTION ANY-FALSE (a.k.a. NONE-NIL)


	(:SUMMARY
		"Returns TRUE (T) if at least one of the arguments is NIL; otherwise 
		returns FALSE (NIL). "
	)
	
	(:SYNTAX
		(ANY-FALSE arguments)
		(ANY-NIL arguments)
	)

	(:PARAMETERS
		(ARGUMENTS 
		:PARAMETER-TYPE required
		:VALUE-TYPE LABELED-SEQUENCE\,LIST  
		:DOCSTRING " A list of arguments.  A list can be generated using the LIST function or the {} function."
		)
	)
	
	(:RETURNS
		"An object of type T or NULL."
	)

	(:EXAMPLES "are shown below."
	)
	
	(:TEXT 
		(:p (:b "ANY-FALSE follows the conventions of true and false and
		is often called by conditionals."))
		(:p (:b "1. Arithmetic example using the LIST function in which at least one argument is False."))
		(:img :src "/weblistenerdocs/bbldf/images/ANY-FALSE-1.jpg")
		(:p (:b "Results:"))
		(:img :src "/weblistenerdocs/bbldf/images/ANY-FALSE-1-RESULT.jpg")
		(:p (:b "2.  Arithmetic example using the {} function in which no arguments are False."))
		(:img :src "/weblistenerdocs/bbldf/images/ANY-FALSE-2.jpg")
		(:p (:b "Results:"))
		(:img :src "/weblistenerdocs/bbldf/images/ANY-FALSE-2-RESULT.jpg")
	)
 
	(:KEYWORDS 
		"ALL-FALSE, ANY-TRUE, ALL-TRUE, True and False, Conditionals"
	)

	(:SEE-ALSO
		"IF, ALL-TRUE, ALL-FALSE, ANY-TRUE, IF-TRUE, IF-FALSE"
	)

)

;================================= ANY-TRUE ==============================

(DOCUMENT-FUNCTION ANY-TRUE


	(:SUMMARY
		"Returns TRUE (T) if at least one of the arguments is T; otherwise 
		returns FALSE (NIL)."
	)
		
	(:SYNTAX
		(ANY-TRUE arguments)
	)

	(:PARAMETERS
		(ARGUMENTS 
		:PARAMETER-TYPE required
		:VALUE-TYPE LABELED-SEQUENCE\,LIST  
		:DOCSTRING "A list of arguments."
		)
	)	
		
	(:RETURNS
		"An object of type T or NULL."
	)
	
	(:EXAMPLES "are shown below."
	)
	
	(:TEXT
		(:p (:b "ANY-TRUE follows the conventions of true and false.
        ANY-TRUE is often called by conditionals."))
		(:p (:b "1. A simple example which all arguments are true."))
		(:img :src "/weblistenerdocs/bbldf/images/ANY-TRUE-2.jpg")
		(:p (:b "Results:"))
		(:img :src "/weblistenerdocs/bbldf/images/ANY-TRUE-1-RESULT.jpg")
		(:p (:b "2.  Example which two of the arguments are false."))
		(:img :src "/weblistenerdocs/bbldf/images/ANY-TRUE-1.jpg")
		(:p (:b "Results:"))
		(:img :src "/weblistenerdocs/bbldf/images/ANY-TRUE-1-RESULT.jpg")
		(:p (:b "3.  Example in which all arguments are false."))
		(:img :src "/weblistenerdocs/bbldf/images/ANY-TRUE-3.jpg")
		(:p (:b "Results:"))
		(:img :src "/weblistenerdocs/bbldf/images/ANY-FALSE-2-RESULT.jpg")
	)
 
 

  
	(:KEYWORDS "ANY-FALSE, ALL-FALSE, ALL-TRUE, True and False, Conditionals."
	)
 
 (:SEE-ALSO
   "System functions: ANY-FALSE, ALL-TRUE, ALL-FALSE, IF-TRUE, IF-FALSE, True and False,
   Conditionals."
  )

  )




;;============================== APPLY-FUNCTION =======================

(DOCUMENT-FUNCTION APPLY-FUNCTION

(:SUMMARY 
	"Applies a function to a list of arguments, yielding a list of results. "
)

(:SYNTAX) 	

(:PARAMETERS
	(FUNCTION 
		:VALUE-TYPE form 
		:DOCSTRING "Form containing function to be applied"
	)

	(VARIABLE 
		:VALUE-TYPE list 
		:DOCSTRING "Names of variables used in function"
	)
	
	(LIST 
		:VALUE-TYPE list 
		:PARAMETER-TYPE variable-number 
		:DOCSTRING "List of values to be given function"
	)
)

(:RETURNS "A list")

(:EXAMPLES "are shown below."
)

(:TEXT
	(:p (:b "Makes it possible to map any function over a list or multiple lists. This means that the 
	given function will be given each element of a list or lists, and the results will be 
	collected into a new list, which is the returned result. The number of elements in the 
	list of results equals the number of items in the shortest list provided. Elements are 
	taken from the list one at a time, from first to last.  The variable(s) provided as the 
	first argument are used within the function for replacement by elements of the list(s) 
	and have no meaning outside the function. The variable list may be empty, if the function
	doesn't take any arguments. The function will be executed as many times as there are 
	elements in the provided list."))
	(:p (:b "1.  First define the variable as below, then apply the function as shown."))
	(:img :src "/weblistenerdocs/bbldf/images/APPLY-FUNCTION-1.jpg")
	(:img :src "/weblistenerdocs/bbldf/images/APPLY-FUNCTION-2.jpg")
	(:p (:b " The output with display in a pop-up window as illustrated."))
	(:img :src "/weblistenerdocs/bbldf/images/APPLY-FUNCTION-3.jpg")
	(:p (:b "2.  Define a variable, execute the function one time, and then map it over multiple times 
	as shown in these successive steps."))
	(:img :src "/weblistenerdocs/bbldf/images/APPLY-FUNCTION-4.jpg")
	(:img :src "/weblistenerdocs/bbldf/images/APPLY-FUNCTION-5.jpg")
	(:img :src "/weblistenerdocs/bbldf/images/APPLY-FUNCTION-6.jpg")
	(:img :src "/weblistenerdocs/bbldf/images/APPLY-FUNCTION-7.jpg")
	(:p (:b " The results will display in a pop-up window as shown below."))
	(:img :src "/weblistenerdocs/bbldf/images/APPLY-FUNCTION.8.jpg")
	
	
)

(:KEYWORDS "Application, multiple, maps, mapping."
)

(:SEE-ALSO "APPLY-FUNCTION, LOOP, FOR-EACH"
)

)

;================================= ASIN ==============================

(DOCUMENT-FUNCTION ASIN


	(:SUMMARY
		"Returns the arc sine (asin), a.k.a. inverse sine, in radians."
	)

	(:DESCRIPTION
		"Arc sine is synonymous with inverse sine.  This function for arc sine 
		requires an argument in radians and returns the angle in radians whose 
		sine is the argument you provide. The argument provided is usually between 
		-1 and 1, if not you will get a complex number.
		The returned values will be between -PI/2 and PI/2. (PI = 3.14159)."
	)

	(:SYNTAX
		(asin n)
	)

	(:PARAMETERS
		(N 
			:PARAMETER-TYPE required
			:VALUE-TYPE number
			:DOCSTRING "The number in radians you want the arc sine of (usually between
			-1 and 1)."
		)
	)
	
	(Returns
		"a number."
	)

	(:EXAMPLES "are shown below."
	)
	
	(:TEXT
		(:p (:b "1. Simple example"))
		(:img :src "/weblistenerdocs/bbldf/images/ASIN-1.jpg")
		(:ul (:p (:b "Results:")))
		(:ul (:img :src "/weblistenerdocs/bbldf/images/ASIN-1-RESULT.jpg"))
		(:p (:b "2.  Another example"))
		(:img :src "/weblistenerdocs/bbldf/images/ASIN-2.jpg")
		(:ul (:p (:b "Results:")))
		(:ul (:img :src "/weblistenerdocs/bbldf/images/ASIN-2-RESULT.jpg"))
		(:p (:b "3.  Example using arithmetic functions"))
		(:img :src "/weblistenerdocs/bbldf/images/ASIN-3.jpg")
		(:ul (:p (:b "Results:")))
		(:ul (:img :src "/weblistenerdocs/bbldf/images/ASIN-3-RESULT.jpg"))
		
	)
	
	(:KEYWORDS "asin, inverse sine, arc sine"
	)

	(:SEE-ALSO "SIN, ACOS, ATAN."
	)

)

;================================= ASSIGN ==============================

(DOCUMENT-FUNCTION ASSIGN

  (:SUMMARY "Sets one or more variables to given values."
   )

  (:SYNTAX (ASSIGN target [AS | = | <-] assignment [DISPLAY-OFF]) 
   )
	
  (:PARAMETERS
   (TARGET
    :PARAMETER-TYPE required 
    :VALUE-TYPE (or symbol table)
    :DOCSTRING "variable(s)."
    )

   (ASSIGNMENT 
    :PARAMETER-TYPE required 
    :VALUE-TYPE t
    :DOCSTRING "value(s) given to the variable(s)."
    )

   (DISPLAY-OFF
    :PARAMETER-TYPE  :flag 
    :VALUE-TYPE boolean
    :DOCSTRING "When specified, display is suppressed. This is useful to 
			avoid filling the history screen with large amounts of output."
    )
   )
	
  (:RETURNS "The value(s) assigned to variable(s) and the output will be a list or string. The output 
	of the function is the value given to the first (perhaps only) variable. Execute the
	arguments one at a time to view the results of each assignment."
	
   )

  (:EXAMPLES "are shown below."
   )
	
  (:TEXT
   (:p (:b "1. Assignment of one value to one variable."))
   (:img :src "/weblistenerdocs/bbldf/images/ASSIGN-1.jpg")
   (:p (:b "Results:"))
   (:img :src "/weblistenerdocs/bbldf/images/ASSIGN-1-RESULT.jpg")
   (:p (:b "2. Assignment of a list of values to one variable."))
   (:img :src "/weblistenerdocs/bbldf/images/ASSIGN-2.jpg")
   (:p (:b "Results:"))
   (:img :src "/weblistenerdocs/bbldf/images/ASSIGN-2-RESULT.jpg")
   (:p (:b "3. Assignment of a list of values to a list of variables. Assignment can be checked by executing the variable
		boxes one at a time as shown below."))
   (:img :src "/weblistenerdocs/bbldf/images/ASSIGN-3.jpg")
   (:p (:b "Results:"))
   (:img :src "/weblistenerdocs/bbldf/images/ASSIGN-3-RESULT.jpg")
   (:p (:b "4. Assignment of one value to a list of variables."))
   (:img :src "/weblistenerdocs/bbldf/images/ASSIGN-4.jpg")
   (:p (:b "Results:"))
   (:img :src "/weblistenerdocs/bbldf/images/ASSIGN-4-RESULT.jpg")
   )
		

  (:KEYWORDS "Define, assign, assignment, variables, multiple variables, increment"
   )

  (:SEE-ALSO DEFINE INCREMENT NEW-TABLE)

  )

;========================= ATAN  =====================================

(DOCUMENT-FUNCTION ATAN

	(:SUMMARY
		"Returns the arc tangent (atan) which is synonymous with inverse tangent."
	)
	
	(:SNYTAX
		(atan n1 n2)
	)

	(:PARAMETERS
		(N1 
		:PARAMETER-TYPE required 
		:VALUE-TYPE number
		:DOCSTRING "The number in radians you want the arc tangent of. 
		If n2 is also included, n1 is the numerator of the complete argument 
		you are providing."
		)
	
		(N2 
		:PARAMETER-TYPE optional
		:VALUE-TYPE number
		:DOCSTRING "The denominator of the complete argument you are providing."
		)
	)

	(:RETURNS "a number."
	)
	
	(:EXAMPLES "are illustrated below."
	)
	
	(:TEXT
		(:p (:b "This function for arc tangent requires an argument in radians and returns 
		the angle in radians whose tangent is the argument you provide.
		If one number is provided, then the arc tangent of that number will be returned.
		If two numbers are provided, the first is the numerator and the 
		second is the denominator and they form one complex argument for ATAN.
		The returned values will be between -PI/2 and PI/2. (PI = 3.14159)"))
		(:p (:b "1. Simple example."))
		(:img :src "/weblistenerdocs/bbldf/images/ATAN-1.jpg")
		(:p (:b "Results:"))
		(:img :src "/weblistenerdocs/bbldf/images/ATAN-1-RESULT.jpg")
		(:p (:b "2. Another simple example."))
		(:img :src "/weblistenerdocs/bbldf/images/ATAN-2.jpg")
		(:p (:b "Results:"))
		(:img :src "/weblistenerdocs/bbldf/images/ATAN-2-RESULT-.jpg")
		(:p (:b "3. Another simple example."))
		(:img :src "/weblistenerdocs/bbldf/images/ATAN-3.jpg")
		(:p (:b "Results:"))
		(:img :src "/weblistenerdocs/bbldf/images/ATAN-3-RESULT.jpg")
	)
 
	(:KEYWORDS "inverse tangent, arc tangent, trig, trigonometic function, inverse"
	)
	
	(:SEE-ALSO
			"TAN, ASIN, ACOS"
	)

)