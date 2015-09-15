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


; ================== BACKGROUND-FREQUENCIES-OF ======================

(DOCUMENT-FUNCTION BACKGROUND-FREQUENCIES-OF

	(:SUMMARY
		"Returns a list of the frequencies corresponding to either nucleotides or amino acids, 
		depending on the users specifications."
	)
	
	(:SYNTAX	
	)

	(:PARAMETERS
		(SEQUENCE-SOURCE 
		:PARAMETER-TYPE required
		:VALUE-TYPE ORGANISM\,CONTIGUOUS-SEQUENCE\,STRING\,LIST  
		:DOCSTRING "To make a list, use the LIST function or {} function.  To make a string,
		enclose in quotes."
		)
		
		(DNA 
		:PARAMETER-TYPE token 
		:DOCSTRING "Specifiy this option if the argument is in DNA format."
		)

		(PROTEIN 
		:PARAMETER-TYPE token
		:DOCSTRING "Specify this option if the argument is in protein format."
		)
		
		(BOTH-STRANDS 
		:PARAMETER-TYPE :FLAG   
		:DOCSTRING "Will calculate the frequencies in both strands."
		)
		
		(AS-TABLE 
		:PARAMETER-TYPE :FLAG 
		:DOCSTRING "The results will display as \<Table 1d (Enum)>\ in the results pane, 
		and can then be viewed by using the DISPLAY-TABLE function (see example below.)"
		)
	)
	
	(:RETURNS "a list."
	)
	
	(:EXAMPLES 
		(:foo
		(:p (:b "Notice that this function DOES NOT automatically translate DNA into protein, or vice versa."))
		(:p (:b "1. Background frequencies of single-stranded DNA."))
		(:img :src "/weblistenerdocs/bbldf/images/BACKGROUND-FREQUENCIES-1.jpg")
		(:p (:b "Results:"))
		(:img :src "/weblistenerdocs/bbldf/images/BACKGROUND-FREQUENCIES-2.jpg")
		(:p (:b "2. Background frequencies of double-stranded DNA from the same argument."))
		(:img :src "/weblistenerdocs/bbldf/images/BACKGROUND-FREQUENCIES-3.jpg")
		(:p (:b "Results:"))
		(:img :src "/weblistenerdocs/bbldf/images/BACKGROUND-FREQUENCIES-4.jpg")
		(:p (:b "3. Notice that this function DOES NOT translate DNA into protein when specifying the 
		protein option.  The function mistakenly identifies the A,T,C,and G, as the one letter abbreviations
		for amino acids."))
		(:img :src "/weblistenerdocs/bbldf/images/BACKGROUND-FREQUENCIES-5.jpg")
		(:p (:b "Results:"))
		(:img :src "/weblistenerdocs/bbldf/images/BACKGROUND-FREQUENCIES-6.jpg")
		(:p (:b "4. Correct usage of the protein option when proteins are used in the argument, with results
		shown below."))
		(:img :src "/weblistenerdocs/bbldf/images/BACKGROUND-FREQUENCIES-7.jpg")
		(:p (:b "Results:"))
		(:img :src "/weblistenerdocs/bbldf/images/BACKGROUND-FREQUENCIES-8.jpg")
		(:p (:b "5. The output shown above is not easily labeled, but this can be remedied by interleaving (like
		shuffling cards) the names of the amino acids with the results as shown below."))
		(:img :src "/weblistenerdocs/bbldf/images/BACKGROUND-FREQUENCIES-9.jpg")
		(:p (:b "Results:"))
		(:img :src "/weblistenerdocs/bbldf/images/BACKGROUND-FREQUENCIES-10.jpg")
		)
	)
	
	(:TEXT
	)
	
	(:KEYWORDS "Frequencies, Frequency, Background, DNA frequencies, Occurences"
	)
	
	(:SEE-ALSO "AMINO-ACID-FREQUENCIES-OF, AMINO-ACIDS-COUNTS-OF"
	)
	
)

; ================== BIN-DATA-OF ======================

(DOCUMENT-FUNCTION BIN-DATA-OF

	(:SUMMARY
		"Puts data from list into bins of specified width."
	)
	
	(:SYNTAX
		(BIN-DATA-OF list [LOWEST-VALUE] min [HIGHEST-VALUE] max [BIN-WIDTH] interval)
	)
	
	(:PARAMETERS
		(LIST 
		:PARAMETER-TYPE required
		:VALUE-TYPE LIST  
		:DOCSTRING "To make a list, enclose data in parentheses or use the LIST function." 
		)
		
		(MIN 
		:PARAMETER-TYPE required
		:VALUE-TYPE NUMBER
		:DOCSTRING "The minimum value that is represented in the argument supplied." 
		)
		
		(MAX 
		:PARAMETER-TYPE required
		:VALUE-TYPE NUMBER  
		:DOCSTRING "The maximum value represented in the argument supplied."
		)

		(INTERVAL 
		:PARAMETER-TYPE required
		:VALUE-TYPE POSITIVE-NUMBER  
		:DOCSTRING "The distance between bins."
		)
	)

	(:RETURNS "a set of numbers, the first corresponding to the bin interval, and the second 
	cooresponding to the number of entries in the argument supplied that fall into that
	bin."
	)

	(:EXAMPLES 
		(:foo
		(:p (:b "Note: The data set that was used as the argument.  The data was sorted, so that
		a min and max value could be easily determined."))
		(:img :src "/weblistenerdocs/bbldf/images/BIN-DATA-OF-1.jpg")
		(:p (:b "1. An example of what the function may look like, with a bin interval of 0.01"))
		(:img :src  "/weblistenerdocs/bbldf/images/BIN-DATA-OF-2.jpg")
		(:p (:b "Results: (The first number is the bin number,
		the second number is the number of data that fell into the bin):"))
		(:img :src "/weblistenerdocs/bbldf/images/BIN-DATA-OF-3.jpg")
		)
	)
	
	(:TEXT
	)
	
	(:KEYWORDS
		"Bin, Sort, Intervals, Data sort"
	)
	
	(SEE-ALSO SORT
	)
	
)
		
; ================== BLAST-VALUE ======================

(DOCUMENT-FUNCTION BLAST-VALUE

	(:SUMMARY
		"Extracts one or more cells from a blast table"
	)
	
	(:SYNTAX
		(BLAST-VALUE blast-table [LINE#] line [COLUMN-LABELED] column [TRUNCATE-AT positive-number])
	)
	
	(:PARAMETERS
		(BLAST-TABLE
		:PARAMETER-TYPE required
		:VALUE-TYPE TABLE  
		:DOCSTRING "A BLAST table can be created by executing the SEQUENCE-SIMILAR-TO"
		)

		(LINE 
		:PARAMETER-TYPE required
		:VALUE-TYPE POSITIVE-NUMBER  
		:DOCSTRING "The line of the BLAST results the user is interested in."
		)

		(COLUMN 
		:PARAMETER-TYPE required
		:VALUE-TYPE STRING\,SYMBOL\,LIST    
		:DOCSTRING "The column of the BLAST results the user is interested in."
		)

		(LINE# 
		:PARAMETER-TYPE TOKEN  
		:DOCSTRING "The number of the line the user is interested in."
		)

		(COLUMN-LABELED 
		:PARAMETER-TYPE TOKEN  
		:DOCSTRING "The label of the column that the user is interested in."
		)

		(TRUNCATE-AT 
		:PARAMETER-TYPE KEYWORD 
		:VALUE-TYPE POSITIVE-NUMBER  
		)
	)

	(:RETURNS
		"An object of type LIST."
	)
	
	(:EXAMPLES (:foo
				(:p (:b "Note: BLAST looks for sequence similarities, this function is known as SEQUENCE-SIMILAR-TO."))
		(:p (:b "Note: The results returned by SEQUENCE-SIMILAR-TO (a.k.a. BLAST) that return to 
		the user in a pop-up window.  The line and column of interest is highlighted in blue."))
		(:img :src "/weblistenerdocs/bbldf/images/BLAST-VALUE-1.jpg")
		(:p (:b "1. Simple example."))
		(:img :src "/weblistenerdocs/bbldf/images/BLAST-VALUE-2.jpg")
		(:p (:b "Results:"))
		(:img :src "/weblistenerdocs/bbldf/images/BLAST-VALUE-3.jpg")
		)
	)
				
	
	
	(:TEXT
		
	)
	
	(:KEYWORDS "BLAST, sequence, table extraction, similar, value"
	)
	
	(:SEE-ALSO "SEQUENCE-SIMILAR-TO"
	)
	
)
