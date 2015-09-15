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

;;; Authors: Joe Anderson, Victor Clarke, Jeff Elhai, Bogdan Mihai, Craig Noe
;;; Arnaud Taton, Robel Wolde

;========================= EIGHTH =========================
(DOCUMENT-FUNCTION EIGHTH
(:SUMMARY "Returns the eighth element in a list." )
(:SYNTAX)
(:PARAMETERS
(entity :VALUE-TYPE LIST :DOCSTRING "The list to get an element from.")
(in :VALUE-TYPE token :DOCSTRING "Non-functional modifier.")
(in-each :VALUE-TYPE token :DOCSTRING "Indicates that the function should look for lists within the list or multiple lists.")
(strict :VALUE-TYPE FLAG :DOCSTRING "If selected, will throw an error if you ask for an element that doesn't exist. On by default.")
(nonstrict :VALUE-TYPE FLAG :DOCSTRING "If selected, will return NIL if you ask for an element that doesn't exist.")
)
(:RETURNS "The eighth element of a list, or NIL.")
(:EXAMPLES "Detailed below, graphically.")
(:TEXT
(:p "EIGHTH returns the eighth element in a list.")
(:p "")
(:img :src "/weblistenerdocs/bbldf/images/eighth1.jpg")
(:p "Returns:")
(:img :src "/weblistenerdocs/bbldf/images/eighth2.jpg")
(:p "")
(:img :src "/weblistenerdocs/bbldf/images/eighth3.jpg")
(:p "Returns:")
(:img :src "/weblistenerdocs/bbldf/images/fourth4.jpg")
; Not a typo, reusing another function's NIL return to save space.
(:p "")
)
(:KEYWORDS list element order cardinal ordinal)
(:SEE-ALSO FIRST SECOND THIRD FOURTH FIFTH SIXTH SEVENTH NINTH TENTH INSIDE-LIST CHOOSE-FROM)
)

;========================= EITHER =========================
(DOCUMENT-FUNCTION EITHER
(:SUMMARY "Returns a boolean, depending upon the values entered." )
(:SYNTAX)
(:PARAMETERS
(form :VALUE-TYPE ANY :DOCSTRING "The element to be used in determining whether the statement is true or false.")
)
(:RETURNS "A boolean.")
(:EXAMPLES "Detailed below, graphically.")
(:TEXT
(:p "EITHER is similar to OR in some programming languages. If all arguments are FALSE, then EITHER will "
"return FALSE. If any element is true, than EITHER will return TRUE. If one element is TRUE and one "
"element is FALSE, EITHER will return TRUE.")
(:p "")
(:img :src "/weblistenerdocs/bbldf/images/either1.jpg")
(:p "Returns:")
(:img :src "/weblistenerdocs/bbldf/images/either2.jpg")
(:p "This function can currently only be used with two arguments.")
)
(:KEYWORDS EITHER OR EQUIVALENCE)
(:SEE-ALSO TRUE FALSE)
)

;========================= ELEMENT/S-OF-FRAME =========================
(DOCUMENT-FUNCTION ELEMENT/S-OF-FRAME
(:SUMMARY "Returns the data stored in an object's FRAME." )
(:SYNTAX)
(:PARAMETERS
(target :VALUE-TYPE FRAME :DOCSTRING "The gene, organism, protein, or contiguous sequence you wish to know more about.")
(display :VALUE-TYPE FLAG :DOCSTRING "When selected, pops up a more readable version of the results in a new window.")
(item :VALUE-TYPE KEYWORD :DOCSTRING "The particular element in the FRAME you would like to see.")
)
(:RETURNS "A list of FRAME attributes.")
(:EXAMPLES "Detailed below, graphically.")
(:TEXT
(:p "As a brief background, the FRAME is the most common data type in BioBIKE. All genes, organisms, "
"contiguous sequences, and proteins have a FRAME as their underlying source of information. This "
"function allows you to peek into that FRAME, to find out more about the object.")
(:p "")
(:p "To examine the FRAME of the Anabaena gene, alr2009:")
(:img :src "/weblistenerdocs/bbldf/images/elements-of-frame1.jpg")
(:p "Returns:")
(:img :src "/weblistenerdocs/bbldf/images/elements-of-frame2.jpg")
(:p "")
)
(:KEYWORDS FRAME gene protein contiguous-sequence organism)
(:SEE-ALSO DESCRIPTION-OF NAME-OF LENGTH-OF MW-OF HYDROPHOBICITY-OF)
)

;========================= EMAIL-ME =========================
(DOCUMENT-FUNCTION EMAIL-ME
(:SUMMARY "Allows the user to e-mail themselves a copy of a function's results." )
(:SYNTAX)
(:PARAMETERS
(function :DOCSTRING "The function you'd like to be e-mailed when complete.")
(TO :VALUE-TYPE KEYWORD :DOCSTRING "The email address to send to. Currently non-functional.")
(SUBJECT :VALUE-TYPE KEYWORD :DOCSTRING "Specifies the subject line of the e-mail. Currently non-functional.")
)
(:RETURNS "NIL")
(:EXAMPLES "None, simply e-mails results to the user.")
(:TEXT
(:p "If you specified an e-mail address when you logged in to BioBIKE, this function will allow the "
"results of a function to be e-mailed to you, instead of appearing on-screen. Currently, it will only "
"e-mail the original address used when the current session began.")
)
(:KEYWORDS email e-mail output)
(:SEE-ALSO DISPLAY READ WRITE)
)

;========================= ENCODES-PROTEIN? =========================
(DOCUMENT-FUNCTION ENCODES-PROTEIN?
(:SUMMARY "Returns whether or not a gene is protein-encoding." )
(:SYNTAX)
(:PARAMETERS
(gene :VALUE-TYPE GENE :DOCSTRING "The gene or list of genes you wish to know more about.")
)
(:RETURNS "A boolean.")
(:EXAMPLES "Detailed below, graphically.")
(:TEXT
(:p "ENCODES-PROTEIN? allows you to determine if a gene is protein-encoding, or perhaps some other type. "
"An RNA-encoding gene will return NIL, while a protein-encoding gene will return TRUE. It is important "
"to note that ENCODES-PROTEIN? has no protein-finding capability, it does not attempt to translate a "
"gene. It merely checks the gene's FRAME to see if it is specified as protein-encoding.")
(:p "")
(:img :src "/weblistenerdocs/bbldf/images/encodes-protein1.jpg")
(:p "Returns:")
(:img :src "/weblistenerdocs/bbldf/images/encodes-protein2.jpg")
(:p "This function can be used against a list of genes, in which case it will return a list of booleans.")
)
(:KEYWORDS PROTEIN peg gene protein-encoding)
(:SEE-ALSO PROTEIN-OF TRANSLATION-OF ORFS-IN DESCRIPTION-OF)
)

;========================= ENTER =========================
(DOCUMENT-FUNCTION ENTER
(:SUMMARY "Allows the user to implement a package of functions and variables." )
(:SYNTAX)
(:PARAMETERS
(package-name :VALUE-TYPE STRING :DOCSTRING "The package you wish to utilize.")
)
(:RETURNS "A boolean.")
(:EXAMPLES "Detailed below, graphically.")
(:TEXT
(:p "ENTER will allow you to use prepared packages of functions and variables, often as part of a tutorial. "
"Once completed, a pop-up will show you what environment changes have taken place.")
(:p "")
(:img :src "/weblistenerdocs/bbldf/images/enter1.jpg")
(:p "Returns:")
(:img :src "/weblistenerdocs/bbldf/images/enter2.jpg")
(:p "If you're inclined to use BBI, this is very similar to the IN-PACKAGE function.")
)
(:KEYWORDS package tutorial enter)
(:SEE-ALSO DEFINE-FUNCTION DEFINE MY-VARIABLES MY-FUNCTIONS IN-PACKAGE)
)

;========================= EXIT-WORKFLOW =========================
(DOCUMENT-FUNCTION EXIT-WORKFLOW
(:SUMMARY "A deprecated function that used to halt the system's efforts at a problem." )
(:SYNTAX)
(:PARAMETERS
(value :DOCSTRING "The process to kill.")
)
(:RETURNS "NIL")
(:EXAMPLES "None, non-operational function.")
(:TEXT
(:p "Long ago, when BioBIKE did not have timeout settings that users could modify, this function was "
"useful for ending an infinite loop, or mistakenly-huge scope of a function. Now that the default "
"timeout for a function is 10 minutes, and is user-modifiable under File -> Preferences, this function "
"has outlived its utility.")
)
(:KEYWORDS deprecated)
(:SEE-ALSO)
)

;========================= EXP =========================
(DOCUMENT-FUNCTION EXP
(:SUMMARY "Performs the exponential function, e raised to some number." )
(:SYNTAX)
(:PARAMETERS
(number :VALUE-TYPE NUMBER :DOCSTRING "The exponent you wish to use.")
)
(:RETURNS "A number.")
(:EXAMPLES "Detailed below, graphically.")
(:TEXT
(:p "EXP allows you to return the exponential function, that is, Euler's number raised to a given number.")
(:p "")
(:img :src "/weblistenerdocs/bbldf/images/exp1.jpg")
(:p "Returns:")
(:img :src "/weblistenerdocs/bbldf/images/exp2.jpg")
(:p "")
(:p "The exponential function is critical for analyzing natural data. A graph is included below, to help "
"demonstrate the utility of natural logarithms and exponential functions.")
(:img :src "/weblistenerdocs/bbldf/images/expgraph.gif")
)
(:KEYWORDS e exponential logarithm)
(:SEE-ALSO LOG ^)
)

;========================= FIFTH =========================
(DOCUMENT-FUNCTION FIFTH
(:SUMMARY "Returns the fifth element in a list." )
(:SYNTAX)
(:PARAMETERS
(entity :VALUE-TYPE LIST :DOCSTRING "The list to get an element from.")
(in :VALUE-TYPE token :DOCSTRING "Non-functional modifier.")
(in-each :VALUE-TYPE token :DOCSTRING "Indicates that the function should look for lists within the list or multiple lists.")
(strict :VALUE-TYPE FLAG :DOCSTRING "If selected, will throw an error if you ask for an element that doesn't exist. On by default.")
(nonstrict :VALUE-TYPE FLAG :DOCSTRING "If selected, will return NIL if you ask for an element that doesn't exist.")
)
(:RETURNS "The fifth element of a list, or NIL.")
(:EXAMPLES "Detailed below, graphically.")
(:TEXT
(:p "FIFTH returns the fifth element in a list.")
(:p "")
(:img :src "/weblistenerdocs/bbldf/images/fifth1.jpg")
(:p "Returns:")
(:img :src "/weblistenerdocs/bbldf/images/fifth2.jpg")
(:p "")
(:img :src "/weblistenerdocs/bbldf/images/fifth3.jpg")
(:p "Returns:")
(:img :src "/weblistenerdocs/bbldf/images/fourth4.jpg")
; Not a typo, reusing another function's NIL return to save space.
(:p "")
)
(:KEYWORDS list element order cardinal ordinal)
(:SEE-ALSO FIRST SECOND THIRD FOURTH SIXTH SEVENTH EIGHTH NINTH TENTH INSIDE-LIST CHOOSE-FROM)
)

;========================= FILTER =========================

(DOCUMENT-FUNCTION bbl::FILTER
  (:CANONICAL NIL)
  (:SUMMARY "Extracts values from a list that meets given criteria")
  (:VPL-SYNTAX (:FOO (:img :src "/weblistenerdocs/bbldf/images/FILTER-syntax.PNG"                      :HEIGHT "200")))
  (:PARAMETERS
    (list :VALUE-TYPE List :DOCSTRING "List from which values are to be extracted")
    (not :PARAMETER-TYPE flag 
       :DOCSTRING "Specifies that the result is to be determined by the negation of the test")
    (test :VALUE-TYPE symbol :DOCSTRING "Operation used to compare values")
    (value :VALUE-TYPE any :PARAMETER-TYPE optional 
            :DOCSTRING "value to be compared against list (or name of function if TRUE-PER test used)")
    (case-sensitive :PARAMETER-TYPE flag :DOCSTRING 
       "Specifies that comparison is to be performed without regard to capitalization")
    (inclusive :PARAMETER-TYPE flag :DOCSTRING 
       "When the IS-BETWEEN test is used, a value that is equal to one of the boundary values will be considered to pass the test. The flag is ignored with any other test.")
    (by-position :VALUE-TYPE Number :PARAMETER-TYPE keyword :DOCSTRING 
       "Specifies that if values are lists, then the comparison is to be performed using the element of the list with the given index")
  )
 (:RETURNS "List")
 (:EXAMPLES
    (:FOO
    (:OL 
      (:LI)
    ))
  )
 
 (:TEXT
    (:P 
      (:TABLE
        (:TR 
          (:TD :VALIGN "top"
            ((:FONT :FACE "Verdana" :SIZE 2)
              #.(ONE-STRING-SP 
                 "Sometimes when you want to pick out desired elements from a list,"
                 "the easiest way is to look at the list and choose the elements you"
                 "want. However, with large lists or repetitive processes, this"
                 "strategy may be impractical. FILTER provides automated ways of"
                 "choosing the desired elements, according to a ")
                (:B "test")
                " icon that specifies which flavor of comparison you want, "
                "from the list shown on the right."
                
              (:P (:B "Numeric and alpha/numeric tests") (:BR) 
                  "For a description of these most basic comparison operations, including use of the "
                  "INCLUSIVE flag, see "
                 ((:A :HREF (:PRINT (help::MAKE-HELP-TOPIC-URL 
                      :NAME "How to compare two quantities (numeric vs alphabetical)")))
                   "How to compare two quantities (numeric vs alphabetical)")
                 ".")

              (:P (:B "Aggregate tests") (:BR)
                  "For a description of comparison operations that "
                 "look for partial matches of items within lists or strings, see "
                 ((:A :HREF (:PRINT (help::MAKE-HELP-TOPIC-URL 
                      :NAME "How to compare two quantities (within strings or lists)")))
                   "How to compare two quantities (within strings or lists)")
                 ".")

              (:P (:B "Single value tests") (:BR)
                  "All of the tests considered thus far compare one value to another. "
                  "However, there are cases where you're interested in the characteristics of "
                  "a single value, for example, whether a number is odd or whether "
                  "a variable has a non-NIL value. Even functions you make up yourself "
                  "can be used for this purpose. "
                  "For a description of these operations, see "
                  ((:A :HREF (:PRINT (help::MAKE-HELP-TOPIC-URL 
                       :NAME "How to assess an item for a certain characteristic")))
                    "How to assess an item for a certain characteristic")
                  ".")

              (:P (:B "Sensitivity of tests to capitalization") (:BR)
                  "All tests are case-insensitive (capitalization doesn't matter) "
                  "unless the CASE-SENSITIVE flag is specified."
                  )
          ))
          (:TD :VALIGN "top"
            (:img :src "/weblistenerdocs/bbldf/images/FILTER-test-menu-labeled.jpg" :height "360"))
        )))
  )

 (:SEE-ALSO TRUE)
)

;========================= FIRST =========================
(DOCUMENT-FUNCTION FIRST
(:SUMMARY "Returns the first element or elements in a list." )
(:SYNTAX)
(:PARAMETERS
(number :VALUE-TYPE KEYWORD :DOCSTRING "The number of elements to retrieve.")
(entity :VALUE-TYPE LIST :DOCSTRING "The list to get an element from.")
(in :VALUE-TYPE token :DOCSTRING "Non-functional modifier.")
(in-each :VALUE-TYPE token :DOCSTRING "Indicates that the function should look for lists within the list or multiple lists.")
(strict :VALUE-TYPE FLAG :DOCSTRING "If selected, will throw an error if you ask for an element that doesn't exist. On by default.")
(nonstrict :VALUE-TYPE FLAG :DOCSTRING "If selected, will return NIL if you ask for an element that doesn't exist.")
)
(:RETURNS "The specified number of elements from a list, or NIL.")
(:EXAMPLES "Detailed below, graphically.")
(:TEXT
(:p "FIRST returns the first element in a list:")
(:p "")
(:img :src "/weblistenerdocs/bbldf/images/first1.jpg")
(:p "Returns:")
(:img :src "/weblistenerdocs/bbldf/images/first2.jpg")
(:p "")
(:p "Or, it can return the first few elements in a list:")
(:img :src "/weblistenerdocs/bbldf/images/first3.jpg")
(:p "Returns:")
(:img :src "/weblistenerdocs/bbldf/images/first4.jpg")
(:p "")
)
(:KEYWORDS list element order cardinal ordinal)
(:SEE-ALSO SECOND THIRD FOURTH FIFTH SIXTH SEVENTH EIGHTH NINTH TENTH INSIDE-LIST CHOOSE-FROM)
)

;========================= FIT =========================
(DOCUMENT-FUNCTION FIT
(:SUMMARY "Fits a string to a specific width in columns." )
(:SYNTAX)
(:PARAMETERS
(pre-string :VALUE-TYPE string :DOCSTRING "The string (usually a function returning a string) you'll be adjusting.")
(columns :VALUE-TYPE number :DOCSTRING "The number of columns in width to make the returned string.")
(center :VALUE-TYPE FLAG :DOCSTRING "Centers the output within the specified width.")
(centered :VALUE-TYPE FLAG :DOCSTRING "Centers the output within the specified width.")
(flush-right :VALUE-TYPE FLAG :DOCSTRING "Left-justifies the output within the specified width.")
(flush-left :VALUE-TYPE FLAG :DOCSTRING "Right-justifies the output within the specified width.")
(if-too-big :VALUE-TYPE KEYWORD :DOCSTRING "If the element is wider than the number of columns, display this instead.")
(with :VALUE-TYPE KEYWORD :DOCSTRING "Allows the use of a character other than space for padding.")
)
(:RETURNS "A string of specified width.")
(:EXAMPLES "Detailed below, graphically.")
(:TEXT
(:p "FIT will make columns line up more prettily:")
(:p "")
(:img :src "/weblistenerdocs/bbldf/images/fit1.jpg")
(:p "Returns:")
(:img :src "/weblistenerdocs/bbldf/images/fit2.jpg")
(:p "")
)
(:KEYWORDS string format column)
(:SEE-ALSO DISPLAY-DATA DISPLAY-LINE FOR-EACH)
)

;========================= FLOOR =========================
(DOCUMENT-FUNCTION FLOOR
(:SUMMARY "A deprecated version of a common lisp function.")
(:SYNTAX)
(:PARAMETERS)
(:RETURNS "The number as an integer quotient, and a decimal remainder.")
(:EXAMPLES "Detailed below, graphically.")
(:TEXT
(:p "FLOOR is one of a variety of functions used to manipulate quotients and remainders in LISP, the "
"language which BioBIKE is built upon. FLOOR is not currently in use by VPL, or being actively "
"maintained."))
(:KEYWORDS deprecated)
(:SEE-ALSO)
)

;========================= FOR-EACH =========================
(DOCUMENT-FUNCTION FOR-EACH
(:SUMMARY "A powerful loop function, allowing many different use options." )
(:SYNTAX)
(:PARAMETERS)
(:RETURNS "Almost anything you desire.")
(:EXAMPLES
(:foo
(:li((:a :href "/weblistenerdocs/Basic-syntax-D2-Intro-loops.pdf") "A guide to iteration and loops."))
)
)
(:TEXT
(:p "FOR-EACH is a looping function, with many, many options. So many, in fact, that it's better ")
"to provide a guide than a documentation page.")

(:KEYWORDS loop iteration iterate)
(:SEE-ALSO LOOP APPLY-FUNCTION)
)
;========================= FOURTH =========================
(DOCUMENT-FUNCTION FOURTH
(:SUMMARY "Returns the fourth element in a list." )
(:SYNTAX)
(:PARAMETERS
(entity :VALUE-TYPE LIST :DOCSTRING "The list to get an element from.")
(in :VALUE-TYPE token :DOCSTRING "Non-functional modifier.")
(in-each :VALUE-TYPE token :DOCSTRING "Indicates that the function should look for lists within the list or multiple lists.")
(strict :VALUE-TYPE FLAG :DOCSTRING "If selected, will throw an error if you ask for an element that doesn't exist. On by default.")
(nonstrict :VALUE-TYPE FLAG :DOCSTRING "If selected, will return NIL if you ask for an element that doesn't exist.")
)
(:RETURNS "The fourth element of a list, or NIL.")
(:EXAMPLES "Detailed below, graphically.")
(:TEXT
(:p "FOURTH returns the fourth element in a list.")
(:p "")
(:img :src "/weblistenerdocs/bbldf/images/fourth1.jpg")
(:p "Returns:")
(:img :src "/weblistenerdocs/bbldf/images/fourth2.jpg")
(:p "")
(:img :src "/weblistenerdocs/bbldf/images/fourth3.jpg")
(:p "Returns:")
(:img :src "/weblistenerdocs/bbldf/images/fourth4.jpg")
(:p "")
)
(:KEYWORDS list element order cardinal ordinal)
(:SEE-ALSO FIRST SECOND THIRD FIFTH SIXTH SEVENTH EIGHTH NINTH TENTH INSIDE-LIST CHOOSE-FROM)
)



;========================= FROM =========================
(DOCUMENT-FUNCTION FROM
(:SUMMARY "Counts from one number to another, inclusively." )
(:SYNTAX)
(:PARAMETERS
(start :VALUE-TYPE number :DOCSTRING "The number at which to begin counting.")
(end :VALUE-TYPE number :DOCSTRING "The number at which to end counting.")
(by :VALUE-TYPE keyword :DOCSTRING "The interval for the count.")
(limit :VALUE-TYPE keyword :DOCSTRING "The maximum number of results to return, currently broken.")
)
(:RETURNS "A list of numbers.")
(:EXAMPLES "Detailed below, graphically.")
(:TEXT
(:p "FROM counts from one number to another, by any specified interval, defaulting to 1.")
(:p "")
(:img :src "/weblistenerdocs/bbldf/images/from1.jpg")
(:p "Returns:")
(:img :src "/weblistenerdocs/bbldf/images/from2.jpg")
(:p "")
(:img :src "/weblistenerdocs/bbldf/images/from3.jpg")
(:p "Returns:")
(:img :src "/weblistenerdocs/bbldf/images/from4.jpg")
(:p "")
)
(:KEYWORDS set pair combination permutation)
(:SEE-ALSO MAKE LIST COUNT-OF COUNTS-OF INCREMENT DECREMENT)
)

;========================= GC-FRACTION-OF =========================
(DOCUMENT-FUNCTION GC-FRACTION-OF
(:SUMMARY "Gives the GC fraction of a given gene, sequence, or protein." )
(:SYNTAX)
(:PARAMETERS
(entity :DOCSTRING "The gene, protein, or sequence you're examining.")
)
(:RETURNS "A number from 0 to 1.")
(:EXAMPLES "Detailed below, graphically.")
(:TEXT
(:p "GC-FRACTION-OF gives you one of the most important measures of any sequence, its ratio of G+C/A+T."
"This number determines the annealing temperature of the sequence, and helps determine what "
"environment this sequence is likely to be found in.")
(:p "")
(:p "In this case, you want to know the G-C fraction of alr2009:")
(:img :src "/weblistenerdocs/bbldf/images/gc-fraction-of1.jpg")
(:p "Returns:")
(:img :src "/weblistenerdocs/bbldf/images/gc-fraction-of2.jpg")
(:p "")
)
(:KEYWORDS gc-fraction fraction gc)
(:SEE-ALSO #+unknown DINUCLEOTIDE-BIASES-IN MW-OF HYDROPHOBICITY-OF)
)

;========================= GENE/S-DESCRIBED-BY =========================
(DOCUMENT-FUNCTION GENE/S-DESCRIBED-BY
(:SUMMARY "Searches for genes by their description." )
(:SYNTAX)
(:PARAMETERS
(query :VALUE-TYPE list :DOCSTRING "The descriptive term you're searching based on.")
(display-off :VALUE-TYPE FLAG :DOCSTRING "Disables the pop-up list of results.")
(domain :VALUE-TYPE FLAG :DOCSTRING "Not currently implemented.")
(full :VALUE-TYPE FLAG :DOCSTRING "Not currently implemented.")
(in :VALUE-TYPE KEYWORD :DOCSTRING "Allows you to select a specific group of entities to search from.")
(return :VALUE-TYPE KEYWORD :DOCSTRING "The number of results to return, default is 10,000.")
)
(:RETURNS "A list of genes, and a pop-up table of genes and functions.")
(:EXAMPLES "Detailed below, graphically.")
(:TEXT
(:p "GENE/S-DESCRIBED-BY searches for genes that include the search terms in their description. ")
(:p "")
(:p "In this case, you want a list of all DnaA genes, to compare them:")
(:img :src "/weblistenerdocs/bbldf/images/genes-described-by1.jpg")
(:p "Returns:")
(:img :src "/weblistenerdocs/bbldf/images/genes-described-by2.jpg")
(:p "")
(:p "In your research, you found a wide variance. Now you'd like a more restricted set, within "
"a typical family of organisms:")
(:img :src "/weblistenerdocs/bbldf/images/genes-described-by3.jpg")
(:p "Returns:")
(:img :src "/weblistenerdocs/bbldf/images/genes-described-by4.jpg")
(:p "")
)
(:KEYWORDS gene description function search)
(:SEE-ALSO DESCRIPTION-OF GENE/S-NAMED)
)

;========================= GENE/S-DOWNSTREAM-OF =========================
(DOCUMENT-FUNCTION GENE/S-DOWNSTREAM-OF
(:SUMMARY "Gives the gene immediately downstream of the given gene." )
(:SYNTAX)
(:PARAMETERS
(entity :DOCSTRING "The gene you're talking about.")
)
(:RETURNS "A gene.")
(:EXAMPLES "Detailed below, graphically.")
(:TEXT
(:p "GENE/S-DOWNSTREAM-OF will take a gene, list of genes, or protein, and return the gene immediately "
"downstream, on the same strand as the original gene.")
(:p "")
(:p "In this case, you want to know what's immediately after alr2009:")
(:img :src "/weblistenerdocs/bbldf/images/gene-downstream-of1.jpg")
(:p "Returns:")
(:img :src "/weblistenerdocs/bbldf/images/gene-downstream-of2.jpg")
(:p "")
)
(:KEYWORDS gene upstream downstream)
(:SEE-ALSO ORFS-IN CONTEXT-OF GENE-UPSTREAM-OF GENE-LEFT-OF GENE-RIGHT-OF SEQUENCE-UPSTREAM-OF SEQUENCE-DOWNSTREAM-OF SEQUENCE-LEFT-OF SEQUENCE-RIGHT-OF)
)

;========================= GENE/S-LEFT-OF =========================
(DOCUMENT-FUNCTION GENE/S-LEFT-OF
(:SUMMARY "Gives the gene prior to the given gene, by starting coordinate." )
(:SYNTAX)
(:PARAMETERS
(entity :DOCSTRING "The gene you're talking about.")
(in :DOCSTRING "Narrows the search to a particular organism, required when searching for a coordinate.")
)
(:RETURNS "A gene.")
(:EXAMPLES "Detailed below, graphically.")
(:TEXT
(:p "GENE/S-LEFT-OF will take a coordinate, gene, list of genes, or protein, and return the gene immediately "
"preceding the query on a genetic map.")
(:p "")
(:p "In this case, you want to know what's immediately before alr2009 along its sequence:")
(:img :src "/weblistenerdocs/bbldf/images/gene-left-of1.jpg")
(:p "Returns:")
(:img :src "/weblistenerdocs/bbldf/images/gene-left-of2.jpg")
(:p "")
)
(:KEYWORDS gene upstream downstream)
(:SEE-ALSO ORFS-IN CONTEXT-OF GENE-UPSTREAM-OF GENE-DOWNSTREAM-OF GENE-RIGHT-OF SEQUENCE-UPSTREAM-OF SEQUENCE-DOWNSTREAM-OF SEQUENCE-LEFT-OF SEQUENCE-RIGHT-OF)
)

;========================= GENE/S-NAMED =========================
(DOCUMENT-FUNCTION GENE/S-NAMED
(:SUMMARY "Searches for genes by a descriptive name keyword." )
(:SYNTAX)
(:PARAMETERS
(GENE-NAME :DOCSTRING "The name you're searching for.")
(IN :VALUE-TYPE KEYWORD :DOCSTRING "Allows you to select a specific group of entities to search from.")
)
(:RETURNS "A list of genes.")
(:EXAMPLES "Detailed below, graphically.")
(:TEXT
(:p "GENE/S-NAMED is very similar to GENES-DESCRIBED-BY, but it provides no pop-up table. GENE/S-NAMED "
"strictly returns a list of the genes with the search phrase in their name.")
(:p "")
(:p "In this case, you want a list of all DnaA genes, to compare them:")
(:img :src "/weblistenerdocs/bbldf/images/genes-named1.jpg")
(:p "Returns:")
(:img :src "/weblistenerdocs/bbldf/images/genes-named2.jpg")
(:p "")
)
(:KEYWORDS gene description name function search)
(:SEE-ALSO DESCRIPTION-OF GENES-DESCRIBED-BY GENE-DESCRIBED-BY)
)

;========================= GENE/S-OF =========================
(DOCUMENT-FUNCTION GENE/S-OF
(:SUMMARY "Returns a list of all of the genes of an organism." )
(:SYNTAX (GENES-OF entity [WRAP] [TRUNCATE] [EXCLUDE-OVERLAPS] [FROM] [TO] [LENGTH]))
(:PARAMETERS
(entity :DOCSTRING "The organism, list of organisms, or contiguous sequence to be evaluated.")
(wrap :VALUE-TYPE FLAG :DOCSTRING "If selected, wraps coordinates around a circular genome.")
(truncate :VALUE-TYPE FLAG :DOCSTRING "Cuts off coordinates beyond the length of the sequence being examined.")
(from :DOCSTRING "The numerical coordinate on a sequence to begin your search from.")
(to :DOCSTRING "The numerical coordinate on a sequence at which to end your search.")
(length :DOCSTRING "The length of the segment to check for genes.")
)
(:RETURNS "A gene or list of genes.")
(:EXAMPLES
(:foo
(:p (:img :src "/weblistenerdocs/bbldf/images/genes-of3.jpg"))
(:p (:img :src "/weblistenerdocs/bbldf/images/genes-of4.jpg"))
"Detailed below, graphically."
)
"Detailed below, Graphically."
)
(:TEXT
(:p "GENE/S-OF returns a list of the genes present in a a contiguous sequence or organism.")
(:p "")
(:p "In this case, you want a list of all the Anabaena genes, since you're out of reading material:")
(:img :src "/weblistenerdocs/bbldf/images/genes-of1.jpg")
(:p "Returns:")
(:img :src "/weblistenerdocs/bbldf/images/genes-of2.jpg")
(:p "")
(:p "Now you'd like to narrow the search a bit, to look at a location on the Anabaena chromosome:")
(:img :src "/weblistenerdocs/bbldf/images/genes-of3.jpg")
(:p "Returns:")
(:img :src "/weblistenerdocs/bbldf/images/genes-of4.jpg")
(:p "")
(:p "GENE/S-OF can also display the gene responsible for a protein.")
(:p "This function is among the set of functions that act on organisms. A uniform behavior for these "
"functions is in development, so this behavior may change slightly.")
)
(:KEYWORDS gene genes contig chromosome)
(:SEE-ALSO ORFS-IN CONTEXT-OF GENE-UPSTREAM-OF GENE-DOWNSTREAM-OF GENE-LEFT-OF GENE-RIGHT-OF SEQUENCE-UPSTREAM-OF SEQUENCE-DOWNSTREAM-OF SEQUENCE-LEFT-OF SEQUENCE-RIGHT-OF)
)

;========================= GENE/S-RIGHT-OF =========================
(DOCUMENT-FUNCTION GENE/S-RIGHT-OF
(:SUMMARY "Gives the next gene after the given gene, by starting coordinate." )
(:SYNTAX)
(:PARAMETERS
(entity :DOCSTRING "The gene you're talking about.")
(in :DOCSTRING "Narrows the search to a particular organism, required when searching for a coordinate.")
)
(:RETURNS "A gene.")
(:EXAMPLES "Detailed below, graphically.")
(:TEXT
(:p "GENE/S-RIGHT-OF will take a coordinate, gene, list of genes, or protein, and return the gene immediately "
"following the query on a genetic map.")
(:p "")
(:p "In this case, you want to know what's immediately after alr2009 along its sequence:")
(:img :src "/weblistenerdocs/bbldf/images/gene-right-of1.jpg")
(:p "Returns:")
(:img :src "/weblistenerdocs/bbldf/images/gene-right-of2.jpg")
(:p "")
)
(:KEYWORDS gene upstream downstream)
(:SEE-ALSO ORFS-IN CONTEXT-OF GENE-UPSTREAM-OF GENE-DOWNSTREAM-OF GENE-LEFT-OF SEQUENCE-UPSTREAM-OF SEQUENCE-DOWNSTREAM-OF SEQUENCE-LEFT-OF SEQUENCE-RIGHT-OF)
)

;========================= GENE/S-SIMILAR-TO =========================
(DOCUMENT-FUNCTION GENE/S-SIMILAR-TO
(:SUMMARY "Searches for similar genes using BLAST.")
(:SYNTAX)
(:PARAMETERS
(query :DOCSTRING "The gene or list of genes, or sequence you're searching from.")
(EACH :VALUE-TYPE token :DOCSTRING "If selected, parses across a list for the query.")
(PATTERN :VALUE-TYPE token :DOCSTRING "Not currently implemented.")
(IN :VALUE-TYPE token :DOCSTRING "Not currently implemented.")
(target :DOCSTRING "The entity you're searching for commonalities with, usually a group of organisms.")
(DNA-VS-DNA :VALUE-TYPE flag :DOCSTRING "The usual type of search, compares DNA sequence of the query against the DNA sequence of the target.")
(PROTEIN-VS-PROTEIN :VALUE-TYPE flag :DOCSTRING "Searches the protein product of the query against the proteins present in the target.")
(PROTEIN-VS-TRANSLATED-DNA :VALUE-TYPE flag :DOCSTRING "Searches the protein product of the query against the translated genes of the target.")
(TRANSLATED-DNA-VS-PROTEIN :VALUE-TYPE flag :DOCSTRING "Searches the translated DNA product of the query against the proteins of the target.")
(TRANSLATED-DNA-VS-TRANSLATED-DNA :VALUE-TYPE flag :DOCSTRING "Searches the translated DNA product of the query against the translated DNA product of the target.")
(MISMATCHES :DOCSTRING "When specified, allows for a given number of mismatches when considering hits.")
(THRESHOLD :DOCSTRING "When specified, returns only hits with an EXPECT value above the specified threshold.")
(WORD-SIZE :DOCSTRING "Not currently documented.")
(RETURN :DOCSTRING "When specified, limits the number of returned hits to the given number.")
)
(:RETURNS "A pop-up window with the BLAST query results.")
(:EXAMPLES "Detailed below, graphically.")
(:TEXT
(:p "GENE/S-SIMILAR-TO uses BLAST to find genes similar to the query in a specified group of organisms. "
"By performing the search in BioBIKE, you get the same results as you would from the NCBI website or "
"any other BLAST implementation, without having to import the data when you're done.")
(:p "")
(:p "In this case, you want to search for genes similar to Anabaena gene alr2009 in all Cyanobacteria:")
(:img :src "/weblistenerdocs/bbldf/images/genes-similar-to1.jpg")
(:p "Returns:")
(:img :src "/weblistenerdocs/bbldf/images/genes-similar-to2.jpg")
(:p "")
(:p "Or maybe the actual codon selection doesn't matter to you, and you want protein similarities:")
(:img :src "/weblistenerdocs/bbldf/images/genes-similar-to3.jpg")
(:p "Returns:")
(:img :src "/weblistenerdocs/bbldf/images/genes-similar-to4.jpg")
(:p "")
(:p "Thats kind of an overwhelming result, why not just work on the 5 best matches?")
(:img :src "/weblistenerdocs/bbldf/images/genes-similar-to5.jpg")
(:p "Returns:")
(:img :src "/weblistenerdocs/bbldf/images/genes-similar-to6.jpg")
(:p "")
(:p "PATTERN searching (using regular expressions) is not currently implemented. If you wish to use "
"regular expressions, MATCHES-OF-PATTERN provides that capability with BLAST.")
)
(:KEYWORDS BLAST ortholog homolog paralog similar genes)
(:SEE-ALSO MATCHES-OF-PATTERN SEQUENCE-SIMILAR-TO)
)

;========================= GENE/S-UPSTREAM-OF =========================
(DOCUMENT-FUNCTION GENE/S-UPSTREAM-OF
(:SUMMARY "Gives the gene immediately upstream of the given gene." )
(:SYNTAX)
(:PARAMETERS
(entity :DOCSTRING "The gene you're talking about.")
)
(:RETURNS "A gene.")
(:EXAMPLES "Detailed below, graphically.")
(:TEXT
(:p "GENE/S-UPSTREAM-OF will take a gene, list of genes, or protein, and return the gene immediately "
"upstream, on the same strand as the original gene.")
(:p "")
(:p "In this case, you want to know what's immediately before alr2009:")
(:img :src "/weblistenerdocs/bbldf/images/gene-upstream-of1.jpg")
(:p "Returns:")
(:img :src "/weblistenerdocs/bbldf/images/gene-upstream-of2.jpg")
(:p "")
)
(:KEYWORDS gene upstream downstream)
(:SEE-ALSO ORFS-IN CONTEXT-OF GENE-DOWNSTREAM-OF GENE-LEFT-OF GENE-RIGHT-OF SEQUENCE-UPSTREAM-OF SEQUENCE-DOWNSTREAM-OF SEQUENCE-LEFT-OF SEQUENCE-RIGHT-OF)
)

;========================= GENES-IN-PATHWAY/S =========================
(DOCUMENT-FUNCTION GENES-IN-PATHWAY/S
(:SUMMARY "Returns a list of genes from a given organism, active in a given KEGG pathway." )
(:SYNTAX)
(:PARAMETERS
(pathways :VALUE-TYPE list :DOCSTRING "The pathway or list of pathways you're interested in.")
(organism-or-replicon :DOCSTRING "The organism, contiguous sequence, or list of organisms/contigs you're examining.")
(in :VALUE-TYPE token :DOCSTRING "Not currently implemented.")
(display :VALUE-TYPE flag :DOCSTRING "Enables a pop-up table of the genes and functions.")
)
(:RETURNS "A list of genes.")
(:EXAMPLES "Detailed below, graphically.")
(:TEXT
(:p "GENES-IN-PATHWAY/S will return a list of genes from a given organism, that are present in a given "
"KEGG (Kyoto Encyclopedia of Genes and Genomes) pathway. A selection of pathways is available from "
"the DATA menu, and each has component pathways that can be addressed by name.")
(:p "")
(:p "In this case, you want to see the Cell Motility genes in Anabaena, with a list of their functions:")
(:img :src "/weblistenerdocs/bbldf/images/genes-in-pathways1.jpg")
(:p "Returns:")
(:img :src "/weblistenerdocs/bbldf/images/genes-in-pathways2.jpg")
(:img :src "/weblistenerdocs/bbldf/images/genes-in-pathways3.jpg")
(:p "")
(:p "This function will give you a table and list of functions. If you prefer a more visual output, "
"you can use the HIGHLIGHT-GENE-IN-PATHWAY function to generate very presentable diagrams with "
"highlighted locations of genes.")
)
(:KEYWORDS kegg gene pathway)
(:SEE-ALSO HIGHLIGHT-GENE-IN-PATHWAY KEGG-ID-OF KEGG-ORGANISMS-OF)
)

;========================= GREATER-THAN =========================
(DOCUMENT-FUNCTION GREATER-THAN
(:SUMMARY "Compares two objects alphanumerically.")
(:SYNTAX (greater-than [each] x y [case-sensitive] [give-position] ))
(:PARAMETERS
(x :DOCSTRING "The first string to compare." :value-type string)
(y :DOCSTRING "The second string to compare." :value-type string)
(case-sensitive :DOCSTRING "Indicates whether to factor case into comparison." :value-type token)
(give-position :DOCSTRING "When selected, returns the position that determines the relationship." :value-type token)
)
(:EXAMPLES "Detailed below, graphically."
)
(:RETURNS "A boolean.")
(:TEXT
(:p "GREATER-THAN can be used like a heavy-duty version of '>':")
(:img :src "/weblistenerdocs/bbldf/images/greater-than1.jpg")
(:p "Returns:")
(:img :src "/weblistenerdocs/bbldf/images/greater-than2.jpg")
(:p "")
(:p "But it does things '>' can't, like comparing strings:")
(:img :src "/weblistenerdocs/bbldf/images/greater-than3.jpg")
(:p "Returns:")
(:img :src "/weblistenerdocs/bbldf/images/greater-than4.jpg")
(:p "This function is a much more flexible comparator function, and can compare just about any type of "
"object. The price for this flexibility, however, is increased processing time. If you're performing "
"millions of compares in a program, using '>' will yield much faster results.")
)
(:KEYWORDS string comparison compare greater than)
(:SEE-ALSO > < LESS-THAN)
)

;========================= GROUPS-FROM =========================
(DOCUMENT-FUNCTION GROUPS-FROM
(:SUMMARY "Takes a list, and gives all possible combinations of that list." )
(:SYNTAX (GROUPS-FROM set [OF-LENGTH] length))
(:PARAMETERS
(set :VALUE-TYPE list :DOCSTRING "The set of elements.")
(length :VALUE-TYPE number :DOCSTRING "The length of the groups you wish to generate.")
)
(:RETURNS "A list of groups.")
(:EXAMPLES "Detailed below, graphically.")
(:TEXT
(:p "GROUPS-FROM will take any list, and give all possible combinations of the specified length. ")
(:p "")
(:p "In this case, you want to generate all possible ordered pairs of letters from A to E:")
(:img :src "/weblistenerdocs/bbldf/images/groups-from1.jpg")
(:p "Returns:")
(:img :src "/weblistenerdocs/bbldf/images/groups-from2.jpg")
(:p "")
)
(:KEYWORDS set pair combination permutation)
(:SEE-ALSO GROUP LENGTH-OF)
)

;========================= HELP =========================
(DOCUMENT-FUNCTION HELP
(:SUMMARY "Allows you to search for help on a topic." )
(:SYNTAX)
(:PARAMETERS
(name :DOCSTRING "The help topic you're searching for.")
)
(:RETURNS "A pop-up screen with help options.")
(:EXAMPLES "Detailed below, graphically.")
(:TEXT
(:p "This now-deprecated function will still allow you to search for help, but has been largely "
"by the HELP menu at the top of the VPL screen.")
(:p "")
(:p "In this case, you want to get some help with the topic of orthologs:")
(:img :src "/weblistenerdocs/bbldf/images/help1.jpg")
(:p "Returns:")
(:img :src "/weblistenerdocs/bbldf/images/help2.jpg")
(:p "")
)
(:KEYWORDS help deprecated)
(:SEE-ALSO #+unknown GLOSSARY)
)

;========================= HIGHLIGHT-GENE-IN-PATHWAY =========================
(DOCUMENT-FUNCTION HIGHLIGHT-GENE-IN-PATHWAY
(:SUMMARY "Compares a list of genes against the KEGG diagrams, and give graphical output." )
(:SYNTAX (HIGHLIGHT-GENE-IN-PATHWAY gene-list IN kegg-pathway))
(:PARAMETERS
(gene-list :VALUE-TYPE list :DOCSTRING "The list of genes you'd like to chart.")
(kegg-pathway :VALUE-TYPE number :DOCSTRING "The pathway you think those genes are involved in.")
)
(:RETURNS "A graphical representation of the pathway.")
(:EXAMPLES "Detailed below, graphically.")
(:TEXT
(:p "HIGHLIGHT-GENES-IN-PATHWAY will take a list of genes, and show them highlighted on the standard "
"KEGG (Kyoto Encyclopedia of Genes and Genomes) diagram of specific systems. A list of pathways "
"is provided under the DATA menu.")
(:p "")
(:p "In this case, you want to highlight the genes of Anabaena involved in oxidative phosphorylation:")
(:img :src "/weblistenerdocs/bbldf/images/highlight-gene-in-pathway1.jpg")
(:p "Returns:")
(:img :src "/weblistenerdocs/bbldf/images/highlight-gene-in-pathway2.jpg")
(:p "")
(:p "Note, this is an extremely powerful function, in current development. Your mileage may vary.")
)
(:KEYWORDS KEGG diagram pathway system)
(:SEE-ALSO KEGG-ID-OF KEGG-ORGANISMS-OF GENES-IN-PATHWAY/S)
)

;========================= HYDROPHOBICITY-OF =========================
(DOCUMENT-FUNCTION HYDROPHOBICITY-OF
(:SUMMARY "Determines the hydrophobicity of amino acids on the Kyte/Doolittle scale." )
(:SYNTAX (HYDROPHOBICITY-OF entity NOWARNINGS SEQUENCE AMINO-ACID))
(:PARAMETERS
(entity :VALUE-TYPE any :DOCSTRING "The amino acid sequence or protein you are studying.")
(nowarnings :VALUE-TYPE flag :DOCSTRING "Turns off warnings if a non-AA character is in the sequence.")
(sequence :VALUE-TYPE flag :DOCSTRING "Disables recognizing abbreviations as amino acids. (e.g. ALA)")
(amino-acid :VALUE-TYPE flag :DOCSTRING "Turns off warnings when part of the sequence could also be an abbreviation.")
)
(:RETURNS "A list of numeric hydrophobicities.")
(:EXAMPLES "Detailed below, graphically.")
(:TEXT
(:p "Each amino acid is given a hydrophobicity score between 4.6 and -4.6 following the scale of Kyte and "
"Doolittle. A score of 4.6 is the most hydrophobic and a score of -4.6 is the most hydrophilic.")
(:p "")
(:p "To find the mean hydrophobicity of a protein product:")
(:img :src "/weblistenerdocs/bbldf/images/hydrophobicity-of1.jpg")
(:p "Returns:")
(:img :src "/weblistenerdocs/bbldf/images/hydrophobicity-of2.jpg")
(:p "")
(:p "This function is most useful when determining the likely location of domains, or discrete areas of a protein."))
(:KEYWORDS hydrophobicity domain)
(:SEE-ALSO DINUCLEOTIDE-BIASES-OF GC-FRACTION-OF MW-OF)
)
