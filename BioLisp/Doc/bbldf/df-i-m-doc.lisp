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

; *****************

;;======================IF-FALSE=========================


(DOCUMENT-FUNCTION IF-FALSE
(:SUMMARY "Executes a set of functions depending if a condition is TRUE or FALSE ")
(:SYNTAX (IF-FALSE (test) [THEN] statements1 [ELSE] statements2))
(:PARAMETERS
(test :VALUE-TYPE boolean :DOCSTRING "condition evaluated")
(then :VALUE-TYPE any :PARAMETER-TYPE keyword :DOCSTRING "statements evaluated if the condition is false")
(else :VALUE-TYPE any :PARAMETER-TYPE keyword :DOCSTRING "statements evaluated if the condition is true"))
(:RETURNS "Value of last statement executed")
(:EXAMPLES
"1. Finds the elements in the list with the length shorter than 5:

    (ASSIGN  LIST {\"LISTA\" \"LISTB\" \"ME\" \"YOU\"})

    (FOR-EACH I FROM 1 TO (LENGTH-OF LIST)
         (IF-FALSE (>(LENGTH-OF LIST[I] ) 5)
         (PRINT LIST[I])) 

	--> \"ME\" 

	    \"YOU\""
"2. Finds the molecular weigths less than 7000:

	(ASSIGN list (MW-OF (PROTEIN-OF a7120)   ))
	
	(FOR-EACH item in list
     	     (IF-FALSE (> item  7000)
           (PRINT item)))

	--> will print all molecular weigths less than 7000 of the proteins of a7120"
"3. Verifyes if two sequences are equal :

   (IF-FALSE(EQUAL (SEQUENCE-OF alr0346 FROM 12 TO 30 )(SEQUENCE-OF alr0347 FROM 12 TO 30))
	 THEN (PRINT \"The sequences are not equal . \")	
	 ELSE (PRINT \"The sequences are equal .\")
 
	--> \"The sequences are not equal .\" "
"4. Finds the molecular weigths less than 7000:

	(ASSIGN list (MW-OF (PROTEIN-OF a7120)   ))
	
	(FOR-EACH item in list
           (IF-FALSE (> item  7000)
    	     (PRINT item)))

	--> will print all molecular weigths less than 7000 of the proteins of a7120"

)

(:TEXT
(:p " If the result of the test is NIL , the statements from THEN are skipped . Otherwise , the statements from ELSE are skipped .")
(:p " ELSE is optional but THEN is required .")
(:p " The function allows multiple statements to be executed .")
)
(:SEE-ALSO IF-TRUE IF-NOT-TRUE))




;;======================IF-TRUE=========================

(DOCUMENT-FUNCTION IF-TRUE
(:SUMMARY "Executes a set of functions depending if a condition is TRUE or FALSE ")
(:VPL-SYNTAX (:FOO (:img :src "/weblistenerdocs/bbldf/images/IF-TRUE-syntax.PNG"
                      :HEIGHT "200")))
(:PARAMETERS
  (value1 :VALUE-TYPE any :DOCSTRING "First value to be compared")
  (is/is-not :PARAMETER-TYPE flag 
       :DOCSTRING "Determines whether the result is to be determined directly by the associated test (IS) or is to be determined by the negation of the test (IS-NOT). The default is IS.")
  (test :VALUE-TYPE symbol :DOCSTRING "Operation used to compare values")
  (value2 :VALUE-TYPE any :PARAMETER-TYPE optional 
       :DOCSTRING "Second value to be compared")
  (then-forms :VALUE-TYPE any :PARAMETER-TYPE keyword :DOCSTRING "Statements to be evaluated if the condition is true")
  (else-forms :VALUE-TYPE any :PARAMETER-TYPE keyword 
       :DOCSTRING "Statements (optional) to be evaluated if the condition is not true")
  (case-sensitive :PARAMETER-TYPE flag :DOCSTRING 
       "Specifies that comparison is to be performed without regard to capitalization")
  (T-if-all-T :PARAMETER-TYPE flag :DOCSTRING 
       "Specifies that when either value1 or value2 is a list, the condition is T if all the comparisons are T and NIL if at least one comparison is NIL")
  (T-if-any-T :PARAMETER-TYPE flag :DOCSTRING 
       "Specifies that when either value1 or value2 is a list, the condition is T if at least one comparisons is T and NIL if all comparison are NIL")
  (by-position :VALUE-TYPE Number :PARAMETER-TYPE keyword :DOCSTRING 
       "Specifies that if values are lists, then the comparison is to be performed using the element of the list with the given index")
)
(:RETURNS "Value of last statement executed")
(:EXAMPLES
  (:FOO
    (:OL
      (:LI (:B "IF-THEN") (:BR)
        (:img :src "/weblistenerdocs/bbldf/images/IF-TRUE-THEN-1stmt.png" 
            :HEIGHT "150" :style "margin: 10px 0px") (:BR)
        (:I (:U "Translation") ": If the length of the specified gene is "
        "greater than 4000 nucleotides, then display the name of the gene "
        "followed by its description. Otherwise do nothing. In either case, "
        "the IF-TRUE function will return NIL (because DISPLAY-LIST pops up "
        "a display window but always returns NIL). If, for example, the value of "
        (:I "gene") " is all4312 (with a length of 771 nucleotides), then there "
        "will be no display, but if the value is alr2680 (with a length of 7557 "
        "nucleotides), the name and description of the gene will be displayed.")
        (:BR) (help::HTML "&nbsp")
       )
       
      (:LI (:B "IF-THEN-ELSE") (:BR)
        (:img :src "/weblistenerdocs/bbldf/images/IF-TRUE-THEN-ELSE-2stmts.png" 
            :HEIGHT "150" :style "margin: 10px 0px") (:BR)
        (:I (:U "Translation") ": If the organism is found in the set of marine "
        "cyanobacteria, then its name is displayed, preceded by \"Marine:\", and T "
        "is returned. Otherwise, its name is displayed, preceded by \"Non-marine:\", "
        "and NIL is returned.")
        (:BR) (help::HTML "&nbsp")
       )
  ))
 )
(:TEXT
(:p "If the result of the test is true, the statements in the THEN block are executed and those "
    "in the ELSE block are skipped. Alternatively, if the result of the test is false, the statements "
    "in the THEN block are skipped and those in the ELSE block are executed. The "
    "meaning of 'true' and 'false' is discussed "
    ((:A :HREF (:PRINT (help::MAKE-HELP-GLOSSARY-ENTRY-URL 
                      :NAME "True and False"))) "here") ".")
    
(:p "The THEN block and the ELSE block may contain any number of forms, including zero. Use the "
    (:B "More") " icon to add additional forms and the " (:B "x") " icon to eliminate a form. "
    "The result returned by the last executed form in the block is the result returned by IF-TRUE.")
    
(:p "Much more pertinent to IF-TRUE can be found on the page concerning the "
    ((:A :HREF (:PRINT (help::MAKE-HELP-FUNCTION-DOCUMENTATION-URL :NAME "TRUE%3F" :PACKAGE :bbl)))
                "TRUE?")
    " function, including:"
   (:UL
     (:LI "The nature of the various tests")
     (:LI "The behavior of lists given as arguments (value1 and value2)")
     (:LI "The meaning and use of the different options")
     )
   )
)

(:SEE-ALSO IF-FALSE TRUE?
       (glossary "True and False"))
)

(DOCUMENT-FUNCTION INCREMENT
(:PARAMETERS
(entity :DOCSTRING "the start value")
(by :PARAMETER-TYPE optional :VALUE-TYPE number :DOCSTRING "the value that the incrementation is done with" )
)

(:EXAMPLES

"1. Incrementation of a variable :

(INCREMENT x)

--> 1"

"2. Incrementation of a variable with a certain value ( 3 ) :

(INCREMENT x by 3)

--> 3"

"3. Incrementation of a list of variables ;

(INCREMENT {x y z} BY 3)

--> 3 3 3"

"4. Incrementation of a variable that has been given a previous value :

(ASSIGN x 3)

(INCREMENT x BY 2)

--> 5"

"5. Incrementation of a variable that has been given a previous value, with the result of a function :

(ASSIGN x 3)

(INCREMENT x BY (LENGTH \"BIOBIKE\"))

--> 3"

)
(:RETURNS "number")
(:TEXT
(:P " 1. The incrementation can be done only on variables or lists of variables " )
(:P " 2. If we don't use a specific value for the incrementation, the initial value would be incremented with 1 ")
(:p " 3. We can use as an increment a function that returns a number ")
)
(:SEE-ALSO + ADD - SUBTRACT DECREMENT SUM-OF))

;=================================INSERT================================


(DOCUMENT-FUNCTION bbl::insert
(:summary "Insert something into a string or list")
(:returns "The modified string or list, or a copy thereof"
:type (or list string))
(:syntax
 (insert
  (into into-each into-copy-of into-copy-of-each)
  target
  (each)
  stuff-to-be-inserted
  before
  after
  (after-end)
  (strict relaxed)
  (case-sensitive)
  ))

(:PARAMETERS
(mode
:docstring
#.(one-string-nl
"One of into, into-each, into-copy-of, or into-copy-of-each."
"Specifies whether the target string is to be copied or"
"if possible, the insert is to be done to the target string"
"instead of a copy. When specifying INTO-EACH or INTO-COPY-OF-EACH"
"a list of strings or lists to be operated on must be provided")
:parameter-type :token
:value-type :boolean
:default-value :into
)
(target
:docstring
#.(one-string-nl
"The string or list to operate on."
"The insert may be done on the original string"
"or list or a copy (see MODE).")
:value-type (or string list))
(each
:docstring
#.(one-string-nl
"If present, indicates that the stuff to be inserted is a list"
"to be mapped over")
:parameter-type :token
:value-type boolean
:default-value nil)
(stuff-to-be-inserted
:docstring
"The thing you want inserted into the target."
)
(before
:docstring
#.(one-string-nl
"If provided, the insertion begins immediately before this"
"location or object")
:parameter-type &key
:value-type t
:default-value nil)
(before
:docstring
#.(one-string-nl
"If provided, the insertion begins immediately after this"
"location or object")
:parameter-type &key
:value-type t
:default-value nil)
(after-end
:docstring
#.(one-string-nl
"If provided, this is equivalent to appending what is to be inserted"
"to the end of the target"))
(error-mode
:docstring
#.(one-string-nl
"One of Relaxed or Strict. If strict, out of bounds ranges"
"result in an error. If relaxed, an index less than 1 is treated as 1"
"and an index greater than the length of the sequence is treated"
"as the length of the sequence.")
:parameter-type :flag
:default-value :relaxed)
(case-sensitive
:docstring
#.(one-string-nl
"Specifies that the modification is case-sensitive."
"If this is not specified, then, for example, telling the function"
"to insert after 'b' will do so for both 'b' and 'B'.")
:parameter-type :flag
:default-value nil)
)
(:EXAMPLES "Detailed below, graphically:")
(:TEXT
(:p "To insert a substring into a string after position 1:")
(:img :src "/weblistenerdocs/bbldf/images/insert1.jpg") 
(:p "Returns:")
(:img :src "/weblistenerdocs/bbldf/images/insert2.jpg")
(:p "")
(:p "To add one string to the end of another:")
(:img :src "/weblistenerdocs/bbldf/images/insert3.jpg") 
(:p "Returns:")
(:img :src "/weblistenerdocs/bbldf/images/insert4.jpg")
(:p "")
(:p "To form two alternate lists, with additions:")
(:img :src "/weblistenerdocs/bbldf/images/insert5.jpg") 
(:p "Returns:")
(:img :src "/weblistenerdocs/bbldf/images/insert6.jpg")
)
(:KEYWORDS insert string list manipulation)
(:see-also bbl::replace bbl::join bbl::make)
)

;=======================INTERGENIC-SEQUENCES-OF============================

(DOCUMENT-FUNCTION INTERGENIC-SEQUENCES-OF
(:SUMMARY "Returns a list of the non-gene DNA in an organism." )
(:SYNTAX (INTERGENIC-SEQUENCES-OF entity [LABELED] [MINIMUM-SIZE]))
(:PARAMETERS
(entity :VALUE-TYPE any :DOCSTRING "The organism for which you want the intergenic sequences.")
(LABELED :VALUE-TYPE FLAG :DOCSTRING "Provides more detailed results.")
(MINIMUM-SIZE :VALUE-TYPE FLAG :DOCSTRING "If selected, allows a minimum size threshold for results.")
)
(:RETURNS "A list of sequences.")
(:EXAMPLES "Detailed below, graphically.")
(:TEXT
(:p "To find the intergenic sequences in an organism:")
(:img :src "/weblistenerdocs/bbldf/images/intergenic-sequences-of1.jpg") 
(:p "Returns this large list of sequences:")
(:img :src "/weblistenerdocs/bbldf/images/intergenic-sequences-of2.jpg")
(:p "In its simplest form, this function will take an organism, and return all genetic material "
	"not accounted for by a gene."))
(:KEYWORDS gene intergenic trans-gene)
(:SEE-ALSO SEQUENCE-OF GENES-OF UPSTREAM-SEQUENCES-OF)
)

;=================================INTERLEAVE=====================================

(DOCUMENT-FUNCTION INTERLEAVE
(:SUMMARY "Returns a mixed ordered set of two or more lists.")
(:SYNTAX (INTERLEAVE list list... [TRUNCATE] [SIMPLIFY]))
(:PARAMETERS
(list :DOCSTRING "A list of any type." :value-type any)
(truncate :DOCSTRING "Allows the interleaving of lists with uneven length, by removing the trailing elements." :value-type FLAG)
(simplify :DOCSTRING "Removes any NIL elements from the result." :value-type FLAG)
)
(:EXAMPLES
"Detailed below, graphically:"
)
(:RETURNS "A list")
(:TEXT
(:p "This function returns a list of ordered pairs when used with two lists: "
	"(The nil element is removed by SIMPLIFY)")
(:img :src "/weblistenerdocs/bbldf/images/interleave1.jpg") 
(:p "Returns:")
(:img :src "/weblistenerdocs/bbldf/images/interleave2.jpg")
(:p "")
(:p "This function can also be used with more lists, as shown truncated:")
(:img :src "/weblistenerdocs/bbldf/images/interleave3.jpg") 
(:p "Returns:")
(:img :src "/weblistenerdocs/bbldf/images/interleave4.jpg")
(:p "") 
)
(:KEYWORDS list join ordered pairs)
(:SEE-ALSO JOIN INSIDE-LIST INSERT)
)

;====================================INTERSECTION-OF===============================

(DOCUMENT-FUNCTION INTERSECTION-OF
(:SUMMARY "Returns the intersection of any number of lists.")
(:SYNTAX (INTERSECTION-OF list1 list2...))
(:PARAMETERS
(list1 :DOCSTRING "A list of any type." :value-type list)
(list2 :DOCSTRING "A list of any type." :value-type list)
)
(:EXAMPLES
"Detailed below, graphically:"
)
(:RETURNS "A string or list.")
(:TEXT
(:p "This function will find the common elements in two or more lists and return them.")
(:img :src "/weblistenerdocs/bbldf/images/intersection-of1.jpg") 
(:p "Returns:")
(:img :src "/weblistenerdocs/bbldf/images/intersection-of2.jpg")
(:p "")
(:p "If there are no common element, this function returns NIL:")
(:img :src "/weblistenerdocs/bbldf/images/intersection-of3.jpg") 
(:p "Returns:")
(:img :src "/weblistenerdocs/bbldf/images/intersection-of4.jpg")
(:p "")
)
(:KEYWORDS inversion complement sequence)
(:SEE-ALSO SEQUENCE-OF)
)

;===================================INVERSION-OF================================

(DOCUMENT-FUNCTION INVERSION-OF
(:SUMMARY "Returns the reverse complement of a sequence.")
(:SYNTAX (INVERSION-OF entity))
(:PARAMETERS
(entity :DOCSTRING "The sequence or list of sequences to be inverted." :value-type any)
)
(:EXAMPLES
"Detailed below, graphically:"
)
(:RETURNS "A string or list.")
(:TEXT
(:p "This function will find the complementary sequence to the input, and reverse it.  Very useful for "
	"promoter regions and other binding sites.")
(:img :src "/weblistenerdocs/bbldf/images/inversion-of1.jpg") 
(:p "Returns:")
(:img :src "/weblistenerdocs/bbldf/images/inversion-of2.jpg")
(:p "")
(:p "This function can also be used across a list, as shown:")
(:img :src "/weblistenerdocs/bbldf/images/inversion-of3.jpg") 
(:p "Returns:")
(:img :src "/weblistenerdocs/bbldf/images/inversion-of4.jpg")
(:p "")
(:p "INVERSION-OF will ignore any non-nucleotide characters in a string, and will work against a list.") 
)
(:KEYWORDS inversion complement sequence)
(:SEE-ALSO SEQUENCE-OF)
)

;==========================IS-DNA-SEQUENCE?=======================================

(DOCUMENT-FUNCTION IS-DNA-SEQUENCE?
(:SUMMARY "Returns whether or not a string is a DNA sequence." )
(:SYNTAX (IS-DNA-SEQUENCE string))
(:PARAMETERS
(string :VALUE-TYPE string :DOCSTRING "The string that you think might be a DNA Sequence.")
(extended :VALUE-TYPE flag :DOCSTRING "If selected, use the IUPAC extended system of nomenclature for nucleotides.")
)
(:RETURNS "A boolean, sort of.")
(:EXAMPLES "Detailed below, graphically.")
(:TEXT
(:p "Sometimes, an entity is a DNA sequence:")
(:img :src "/weblistenerdocs/bbldf/images/is-dna-sequence1.jpg") 
(:p "Returns:")
(:img :src "/weblistenerdocs/bbldf/images/is-dna-sequence2.jpg")
(:p "")
(:p "As a rule, all of the other times, it's not:")
(:img :src "/weblistenerdocs/bbldf/images/is-dna-sequence3.jpg") 
(:p "Returns:")
(:img :src "/weblistenerdocs/bbldf/images/is-dna-sequence4.jpg")
(:p "")
(:p "A common problem with this function is getting unexpected results when you're analyzing genes.  "
	"It's important to remember that a gene itself is not a DNA-SEQUENCE in the strict sense of the term, "
	"but a FRAME element that contains a DNA-SEQUENCE.  To put it more clearly, the construct below will "
	"return an error, since it's checking something that's not of the correct type:")
(:img :src "/weblistenerdocs/bbldf/images/is-dna-sequence5.jpg")
(:p "While the actual structure that will help in this situation follows:")
(:img :src "/weblistenerdocs/bbldf/images/is-dna-sequence6.jpg")
(:p "")
(:p "An entity may or may not be an RNA sequence, but after this function, you'll know for sure.  "
	"This would be most useful as a type-check in a programming construct.")
(:p "If EXTENDED is not selected, a string composed entirely of A, T, C, and G is a DNA sequence, "
	"and a string containing only A, U, C, and G is an RNA sequence.")
(:p "If EXTENDED is selected, you can use a limited subset of IUPAC nomenclature, as detailed "
	"in the glossary page below."))
(:KEYWORDS DNA RNA sequence)
(:SEE-ALSO is-rna-sequence? sequence-of #+unknown IUPAC-NOMENCLATURE)
)

;===================================IS-GENE?=================================

(DOCUMENT-FUNCTION IS-GENE?
(:SUMMARY "Returns whether or not an entity is a gene." )
(:SYNTAX (IS-GENE? any))
(:PARAMETERS
(any :VALUE-TYPE ANY :DOCSTRING "The entity that you think might be a gene.")
)
(:RETURNS "A boolean, sort of.")
(:EXAMPLES "Detailed below, graphically.")
(:TEXT
(:p "Sometimes, an entity is a list:")
(:img :src "/weblistenerdocs/bbldf/images/is-gene1.jpg") 
(:p "Returns:")
(:img :src "/weblistenerdocs/bbldf/images/is-gene2.jpg")
(:p "")
(:p "As a rule, all of the other times, it's not:")
(:img :src "/weblistenerdocs/bbldf/images/is-gene3.jpg") 
(:p "Returns:")
(:img :src "/weblistenerdocs/bbldf/images/is-gene4.jpg")
(:p "")
(:p "A common problem encountered by users is attempting to feed this function something it's not "
	"designed for.  This function will only tell you if the target object is of type GENE.  It will "
	"not analyze a sequence and predict a gene, nor will it realize that the sequence you gave it is "
	"that of a gene object.  For example, the two constructs below will return NIL, since they are "
	"pointing at a string or sequence, not a gene object.  They may well be genes in the colloquial "
	"sense, but that's not what this function checks.")
(:img :src "/weblistenerdocs/bbldf/images/is-gene5.jpg")
(:img :src "/weblistenerdocs/bbldf/images/is-gene6.jpg")
(:p "")
(:p "Whereas this item below will return TRUE, since it is an object of type GENE:")
(:img :src "/weblistenerdocs/bbldf/images/is-gene7.jpg")
(:p "")
(:p "An entity may or may not be a gene, but after this function, you'll know for sure.  "
	"This would be most useful as a type-check in a programming construct.")
(:p ""))
(:KEYWORDS gene type)
(:SEE-ALSO IS-GENE? IS-PROTEIN? SEQUENCE-OF)
)

;=================================IS-LIST?=======================================

(DOCUMENT-FUNCTION IS-LIST?
(:SUMMARY "Returns whether or not an entity is a list." )
(:SYNTAX (IS-LIST? any))
(:PARAMETERS
(list? :VALUE-TYPE ANY :DOCSTRING "The entity that you think might be a list.")
(each :VALUE-TYPE TOKEN :DOCSTRING "Analyzes each element of the entity.")
(nil-ok :VALUE-TYPE FLAG :DOCSTRING "Allows for NIL entries when evaluating the target.")
)
(:RETURNS "A boolean, sort of.")
(:EXAMPLES "Detailed below, graphically.")
(:TEXT
(:p "Sometimes, an entity is a list:")
(:img :src "/weblistenerdocs/bbldf/images/is-list1.jpg") 
(:p "Returns:")
(:img :src "/weblistenerdocs/bbldf/images/is-list2.jpg")
(:p "")
(:p "As a rule, all of the other times, it's not:")
(:img :src "/weblistenerdocs/bbldf/images/is-list3.jpg") 
(:p "Returns:")
(:img :src "/weblistenerdocs/bbldf/images/is-list4.jpg")
(:p "")
(:p "Using the EACH token, you can evaluate each item in a list:")
(:img :src "/weblistenerdocs/bbldf/images/is-list5.jpg") 
(:p "Returns:")
(:img :src "/weblistenerdocs/bbldf/images/is-list6.jpg")
(:p "")
(:p "An entity may or may not be a list, but after this function, you'll know for sure.  "
	"This would be most useful as a type-check in a programming construct.")
(:p ""))
(:KEYWORDS list type)
(:SEE-ALSO IS-STRING? IS-TABLE? MAKE)
)

;===============================IS-NONNEGATIVE?=================================

(DOCUMENT-FUNCTION IS-NONNEGATIVE?
(:SUMMARY "Returns whether or not an entity is a non-negative number." )
(:SYNTAX (IS-NONNEGATIVE? any))
(:PARAMETERS
(any :VALUE-TYPE ANY :DOCSTRING "The entity that you think might be a non-negative number.")
)
(:RETURNS "A boolean, sort of.")
(:EXAMPLES "Detailed below, graphically.")
(:TEXT
(:p "Sometimes, an entity is a non-negative number:")
(:img :src "/weblistenerdocs/bbldf/images/is-nonnegative1.jpg") 
(:p "Returns:")
(:img :src "/weblistenerdocs/bbldf/images/is-nonnegative2.jpg")
(:p "")
(:p "As a rule, all of the other times, it's not:")
(:img :src "/weblistenerdocs/bbldf/images/is-nonnegative3.jpg") 
(:p "Returns:")
(:img :src "/weblistenerdocs/bbldf/images/is-nonnegative4.jpg")
(:p "")
(:p "This function will also work on a variable:")
(:img :src "/weblistenerdocs/bbldf/images/is-nonnegative5.jpg") 
(:p "Returns:")
(:img :src "/weblistenerdocs/bbldf/images/is-nonnegative2.jpg")
(:p "An entity may or may not be a non-negative number, but after this function, you'll know for sure.  "
	"This would be most useful as a type-check in a programming construct.")
(:p "")
(:p "Note: This function differs from IS-POSITIVE-NUMBER by including Zero in the non-negative set.  "
	"This will return True for all number, zero to infinity, and NIL for all other numbers."))
(:KEYWORDS non-negative positive number)
(:SEE-ALSO IS-POSITIVE-NUMBER? IS-POSITIVE-INTEGER? IS-NUMBER)
)

;=======================================IS-NUMBER?=====================================

(DOCUMENT-FUNCTION IS-NUMBER?
(:SUMMARY "Returns whether or not an entity is a number." )
(:SYNTAX (IS-NUMBER? any))
(:PARAMETERS
(any :VALUE-TYPE ANY :DOCSTRING "The entity that you think might be a number.")
)
(:RETURNS "A boolean, sort of.")
(:EXAMPLES "Detailed below, graphically.")
(:TEXT
(:p "Sometimes, an entity is a number:")
(:img :src "/weblistenerdocs/bbldf/images/is-number1.jpg") 
(:p "Returns:")
(:img :src "/weblistenerdocs/bbldf/images/is-number2.jpg")
(:p "")
(:p "As a rule, all of the other times, it's not:")
(:img :src "/weblistenerdocs/bbldf/images/is-number3.jpg") 
(:p "Returns:")
(:img :src "/weblistenerdocs/bbldf/images/is-number4.jpg")
(:p "This function will also work on a variable:")
(:img :src "/weblistenerdocs/bbldf/images/is-number5.jpg") 
(:p "Returns:")
(:img :src "/weblistenerdocs/bbldf/images/is-number2.jpg")
(:p "")
(:p "An entity may or may not be a number, but after this function, you'll know for sure.  "
	"This would be most useful as a type-check in a programming construct.")
(:p "")
)
(:KEYWORDS number)
(:SEE-ALSO IS-POSITIVE-NUMBER? IS-POSITIVE-INTEGER?)
)

;==============================IS-POSITIVE-INTEGER?=================================

(DOCUMENT-FUNCTION IS-POSITIVE-INTEGER?
(:SUMMARY "Returns whether or not an entity is a positive integer." )
(:SYNTAX (IS-POSITIVE-INTEGER? any))
(:PARAMETERS
(any :VALUE-TYPE ANY :DOCSTRING "The entity that you think might be a positive integer.")
)
(:RETURNS "A boolean, sort of.")
(:EXAMPLES "Detailed below, graphically.")
(:TEXT
(:p "Sometimes, an entity is a positive integer:")
(:img :src "/weblistenerdocs/bbldf/images/is-positive-integer1.jpg") 
(:p "Returns:")
(:img :src "/weblistenerdocs/bbldf/images/is-positive-integer2.jpg")
(:p "")
(:p "As a rule, all of the other times, it's not:")
(:img :src "/weblistenerdocs/bbldf/images/is-positive-integer3.jpg") 
(:p "Returns:")
(:img :src "/weblistenerdocs/bbldf/images/is-positive-integer4.jpg")
(:p "")
(:p "This function will also work on a variable:")
(:img :src "/weblistenerdocs/bbldf/images/is-positive-integer5.jpg") 
(:p "Returns:")
(:img :src "/weblistenerdocs/bbldf/images/is-positive-integer2.jpg")
(:p "An entity may or may not be a positive integer, but after this function, you'll know for sure.  "
	"This would be most useful as a type-check in a programming construct.")
(:p "")
(:p "Note: This function returns a NIL value for the number zero.  You may disagree with that.  "
	"In that case, you may be more satisfied with the IS-NONNEGATIVE? function."))
(:KEYWORDS positive number)
(:SEE-ALSO IS-POSITIVE-NUMBER? IS-NONNEGATIVE? IS-NUMBER?)
)

;=======================================IS-POSITIVE-NUMBER?===========================

(DOCUMENT-FUNCTION IS-POSITIVE-NUMBER?
(:SUMMARY "Returns whether or not an entity is a positive number." )
(:SYNTAX (IS-POSITIVE-NUMBER any))
(:PARAMETERS
(any :VALUE-TYPE ANY :DOCSTRING "The entity that you think might be a positive number.")
)
(:RETURNS "A boolean, sort of.")
(:EXAMPLES "Detailed below, graphically.")
(:TEXT
(:p "Sometimes, an entity is a positive number:")
(:img :src "/weblistenerdocs/bbldf/images/is-positive-number1.jpg") 
(:p "Returns:")
(:img :src "/weblistenerdocs/bbldf/images/is-positive-number2.jpg")
(:p "")
(:p "As a rule, all of the other times, it's not:")
(:img :src "/weblistenerdocs/bbldf/images/is-positive-number3.jpg") 
(:p "Returns:")
(:img :src "/weblistenerdocs/bbldf/images/is-positive-number4.jpg")
(:p "")
(:p "This function will also work on a variable:")
(:img :src "/weblistenerdocs/bbldf/images/is-positive-number5.jpg") 
(:p "Returns:")
(:img :src "/weblistenerdocs/bbldf/images/is-positive-number2.jpg")
(:p "An entity may or may not be a positive number, but after this function, you'll know for sure.  "
	"This would be most useful as a type-check in a programming construct.")
(:p "")
(:p "Note: This function returns a NIL value for the number zero.  You may disagree with that.  "
	"In that case, use the IS-NONNEGATIVE? function for better results."))
(:KEYWORDS positive number)
(:SEE-ALSO IS-NONNEGATIVE? IS-POSITIVE-INTEGER? IS-NUMBER)
)

;========================================IS-PROTEIN?=============================

(DOCUMENT-FUNCTION IS-PROTEIN?
(:SUMMARY "Returns whether or not an entity is a protein." )
(:SYNTAX (IS-PROTEIN string))
(:PARAMETERS
(any :VALUE-TYPE ANY :DOCSTRING "The entity that you think might be a protein.")
)
(:RETURNS "A boolean, sort of.")
(:EXAMPLES "Detailed below, graphically.")
(:TEXT
(:p "Sometimes, an entity is a protein:")
(:img :src "/weblistenerdocs/bbldf/images/is-protein1.jpg") 
(:p "Returns:")
(:img :src "/weblistenerdocs/bbldf/images/is-protein2.jpg")
(:p "")
(:p "As a rule, all of the other times, it's not:")
(:img :src "/weblistenerdocs/bbldf/images/is-protein3.jpg") 
(:p "Returns:")
(:img :src "/weblistenerdocs/bbldf/images/is-protein4.jpg")
(:p "")
(:p "A common problem encountered by users is attempting to feed this function something it's not "
	"designed for.  This function will only tell you if the target object is of type PROTEIN.  It will "
	"not analyze a translated sequence and predict a protein, nor will it realize that the sequence "
	"you gave it is that of a protein.  For example, the two constructs below will return NIL, since they are "
	"pointing at a string or protein sequence, not a protein object.  They may well be proteins in the "
	"colloquial sense, but that's not what this function checks.")
(:img :src "/weblistenerdocs/bbldf/images/is-protein5.jpg")
(:img :src "/weblistenerdocs/bbldf/images/is-protein6.jpg")
(:p "")
(:p "Whereas this item below will return TRUE, since it is an object of type PROTEIN:")
(:img :src "/weblistenerdocs/bbldf/images/is-protein7.jpg")
(:p "")
(:p "An entity may or may not be a protein, but after this function, you'll know for sure.  "
	"This would be most useful as a type-check in a programming construct."))
(:KEYWORDS protein type)
(:SEE-ALSO protein-of sequence-of)
)

;=======================================IS-PROTEIN-SEQUENCE?============================

(DOCUMENT-FUNCTION IS-PROTEIN-SEQUENCE?
(:SUMMARY "Returns whether or not an entity is a protein sequence." )
(:VPL-SYNTAX 
    (:FOO
      (:img :src "/weblistenerdocs/bbldf/images/IS-PROTEIN-SEQUENCE-syntax.PNG"))
)
(:PARAMETERS
(string :VALUE-TYPE STRING :DOCSTRING "The sequence to be tested. Note that a protein itself is not a sequence.")
(extended :VALUE-TYPE extended
   :DOCSTRING "Specifies that the string is to be considered using the extended nucleotide and amino acid alphabets")
)
(:RETURNS "T or NIL")
(:EXAMPLES 
  (:FOO 
	(:P (:B "1. A bona fide protein sequence:"))
            (:blockquote
      	      (:img :src "/weblistenerdocs/bbldf/images/is-protein-sequence01.png")
              (:img :src "/weblistenerdocs/img-bbldoc/zrightarrow.gif")
              " T" (:BR)
              "The sequence of a protein is definitely a protein sequence (note that 'all0002' is preceded by 'p-')." 
              (:P "However,...")
      	      (:img :src "/weblistenerdocs/bbldf/images/is-protein-sequence02.png")
              (:img :src "/weblistenerdocs/img-bbldoc/zrightarrow.gif")
              " ERROR!" (:BR)
              "...a protein itself is not the same as its sequence.")

	(:P (:B "2. An apparent DNA sequence:"))
            (:blockquote
      	      (:img :src "/weblistenerdocs/bbldf/images/is-protein-sequence07.png")
              (:img :src "/weblistenerdocs/img-bbldoc/zrightarrow.gif")
              " NIL" (:BR)
              "The sequence could be interpreted as a protein sequence, but since since all the letters could represent nucleotides, it is deemed to be DNA and not protein."
              (:P "However, if that sequence contains even a single letter that cannot be a nucleotide but "
                  (:B (:I "could")) " be an amino acid (note second letter below)...")
      	      (:img :src "/weblistenerdocs/bbldf/images/is-protein-sequence09.png")
              (:img :src "/weblistenerdocs/img-bbldoc/zrightarrow.gif")
              " T" (:BR)
              "...then the sequence is deemed to be a protein, unless..."
      	      (:img :src "/weblistenerdocs/bbldf/images/is-protein-sequence10.png")
              (:img :src "/weblistenerdocs/img-bbldoc/zrightarrow.gif")
              " NIL" (:BR)
              "...the EXTENDED flag is invoked and the letter lies within the "
              ((:A :HREF "/weblistenerdocs/nucleotide-abbreviations.htm" :TARGET "_blank")
              "set of extended nucleotides")
              ", then the sequence is deemed to be DNA, not a protein.")

	(:P (:B "3. A conceivable protein sequence:"))
            (:blockquote
      	      (:img :src "/weblistenerdocs/bbldf/images/is-protein-sequence11.png")
              (:img :src "/weblistenerdocs/img-bbldoc/zrightarrow.gif")
              " NIL" (:BR)
              "The sequence cannot be interpreted as a protein sequence, because 'O' does not represent a standard amino acid."(:P "But if the EXTENDED flag is invoked,...")
      	      (:img :src "/weblistenerdocs/bbldf/images/is-protein-sequence12.png")
              (:img :src "/weblistenerdocs/img-bbldoc/zrightarrow.gif")
              " T" (:BR)
              "...then the sequence " (:B (:I "is")) " deemed to be a protein, because 'O' is a member of the "
              ((:A :HREF "/weblistenerdocs/amino-acid-abbreviations.htm" :TARGET "_blank")
              "set of extended amino acids") " (signifying the rare amino acid, pyrrolysine).")
))

(:TEXT
(:p #.(ONE-STRING-SP 
         "There may be occasions in which your program needs to handle a sequence that may or"
         "may not be a protein sequence. IS-PROTEIN-SEQUENCE? will analyze the sequence and"
         "determine whether it is likely to be one."))

(:p #.(ONE-STRING-SP 
         "The function makes its decision by choosing amongst three sequence types: Protein, DNA, and RNA."
         "If a sequence can be interpreted as only one of these types, then the decision is straightforward."
         "If it can be interpreted as more than one, then the decision is based on likelihood. Either way,"
         "a sequence is deemed to be no more than one of these types."
         "If, for example, a sequence can be interpreted as both a protein and a DNA sequence"
         "(as in Example #2), it is considered to be DNA if at least 50% of the letters can be"
         "interpreted as DNA."))

(:p #.(ONE-STRING-SP 
         "The EXTENDED flag changes the definition of DNA, RNA, and protein. If it is not specified, then"
         "the legal letters for a protein sequence are the letters corresponding to the 20 canonical"
         "amino acids. If the flag ")
         (:B (:I "is")) 
    #.(ONE-STRING-SP  
         " specified, then additional letters are considered, i.e., those that represent rare amino acids"
         "or sets of amino acids (see Example #3 and a ")
         ((:A :HREF "/weblistenerdocs/amino-acid-abbreviations.htm" :TARGET "_blank")
               "listing of the extended amino acids") 
    ").")
)
(:KEYWORDS sequence protein type)
(:SEE-ALSO protein-of sequence-of)
)

;=======================================IS-RNA-SEQUENCE?==============================

(DOCUMENT-FUNCTION IS-RNA-SEQUENCE?
(:SUMMARY "Returns whether or not a string is an RNA sequence." )
(:SYNTAX (IS-RNA-SEQUENCE string))
(:PARAMETERS
  (string :VALUE-TYPE any :DOCSTRING "The string that you think might be an RNA Sequence.")
)
(:RETURNS "A boolean, sort of.")
(:EXAMPLES "Detailed below, graphically.")
(:TEXT
  (:p "Sometimes, an entity is a RNA sequence:")
  (:img :src "/weblistenerdocs/bbldf/images/is-rna-sequence1.jpg") 
  (:p "Returns:")
  (:img :src "/weblistenerdocs/bbldf/images/is-rna-sequence2.jpg")
  (:p "")
  (:p "As a rule, all of the other times, it's not:")
  (:img :src "/weblistenerdocs/bbldf/images/is-rna-sequence3.jpg") 
  (:p "Returns:")
  (:img :src "/weblistenerdocs/bbldf/images/is-rna-sequence4.jpg")
  (:p "")
  (:p "An entity may or may not be an RNA sequence, but after this function, you'll know for sure.  "
	"This would be most useful as a type-check in a programming construct.")
  (:p "For the purposes of this environment, a string composed entirely of A, T, C, and G is a DNA sequence, "
	"and a string containing only A, U, C, and G is an RNA sequence."))
(:KEYWORDS DNA RNA sequence)
(:SEE-ALSO sequence-of is-DNA-sequence?)
)

;=================================IS-SIMPLE-LIST?=======================================

(DOCUMENT-FUNCTION IS-SIMPLE-LIST?
(:SUMMARY "Returns whether or not an entity is a simple-list." )
(:SYNTAX (IS-SIMPLE-LIST simple-list?))
(:PARAMETERS
(simple-list? :VALUE-TYPE any :DOCSTRING "The entity that you think might be a simple-list.")
)
(:RETURNS "A boolean, sort of.")
(:EXAMPLES "Detailed below, graphically.")
(:TEXT
(:p "Sometimes, an entity is a simple-list:")
(:img :src "/weblistenerdocs/bbldf/images/is-simple-list1.jpg") 
(:p "Returns:")
(:img :src "/weblistenerdocs/bbldf/images/is-simple-list2.jpg")
(:p "")
(:p "As a rule, all of the other times, it's not:")
(:img :src "/weblistenerdocs/bbldf/images/is-simple-list3.jpg") 
(:p "Returns:")
(:img :src "/weblistenerdocs/bbldf/images/is-simple-list4.jpg")
(:p "")
(:p "An entity may or may not be a simple-list, but after this function, you'll know for sure.  "
	"This would be most useful as a type-check in a programming construct."))
(:KEYWORDS SIMPLE-LIST type)
(:SEE-ALSO MAKE #+unknown SIMPLE-LIST LIST)
)

;=======================================IS-STRING?====================================

(DOCUMENT-FUNCTION IS-STRING?
(:SUMMARY "Returns whether or not an entity is a string." )
(:SYNTAX (IS-STRING string?))
(:PARAMETERS
(string? :VALUE-TYPE any :DOCSTRING "The entity that you think might be a string.")
)
(:RETURNS "A boolean, sort of.")
(:EXAMPLES "Detailed below, graphically.")
(:TEXT
(:p "Sometimes, an entity is a string:")
(:img :src "/weblistenerdocs/bbldf/images/is-string1.jpg") 
(:p "Returns:")
(:img :src "/weblistenerdocs/bbldf/images/is-string2.jpg")
(:p "")
(:p "As a rule, all of the other times, it's not:")
(:img :src "/weblistenerdocs/bbldf/images/is-string3.jpg") 
(:p "Returns:")
(:img :src "/weblistenerdocs/bbldf/images/is-string4.jpg")
(:p "")
(:p "An entity may or may not be a string, but after this function, you'll know for sure.  "
	"This would be most useful as a type-check in a programming construct."))
(:KEYWORDS string type)
(:SEE-ALSO MAKE STRING-OF)
)

;=====================================IS-TABLE?=============================================

(DOCUMENT-FUNCTION IS-TABLE?
(:SUMMARY "Returns whether or not an entity is a table." )
(:SYNTAX (IS-TABLE table?))
(:PARAMETERS
(table? :VALUE-TYPE any :DOCSTRING "The entity that you think might be a table.")
)
(:RETURNS "A boolean, sort of.")
(:EXAMPLES "Detailed below, graphically.")
(:TEXT
(:p "Sometimes, an entity is a table:")
(:img :src "/weblistenerdocs/bbldf/images/is-table1.jpg") 
(:p "Returns:")
(:img :src "/weblistenerdocs/bbldf/images/is-table2.jpg")
(:p "")
(:p "As a rule, all of the other times, it's not:")
(:img :src "/weblistenerdocs/bbldf/images/is-table3.jpg") 
(:p "Returns:")
(:img :src "/weblistenerdocs/bbldf/images/is-table4.jpg")
(:p "")
(:p "An entity may or may not be a table, but after this function, you'll know for sure.  "
	"This would be most useful as a type-check in a programming construct."))
(:KEYWORDS table type)
(:SEE-ALSO MAKE NEW-TABLE)
)

;==================================ITEM/S-OF-RANK========================================

(DOCUMENT-FUNCTION ITEM/S-OF-RANK
(:SUMMARY "Returns the item of a given rank in a list." )
(:SYNTAX (ITEM-OF-RANK rank IN [PERCENT] [FRACTION] list [RETURN-POSITION] [POSITION]))
(:PARAMETERS
(rank :VALUE-TYPE number :DOCSTRING "The rank, or cardinal position of the item you're looking for.")
(list :VALUE-TYPE list :DOCSTRING "The list you're using as a target.")
(PERCENT :VALUE-TYPE token :DOCSTRING "If selected, evaluates rank as a percentage.")
(FRACTION :VALUE-TYPE token :DOCSTRING "If selected, evaluates rank as a fraction.")
(RETURN-POSITION :VALUE-TYPE flag :DOCSTRING "Returns the position of the rank in the list, instead of the rank.")
)
(:RETURNS "An item from a list.")
(:EXAMPLES "Detailed below, graphically.")
(:TEXT
(:p "To find the element at a specific rank in a list:")
(:img :src "/weblistenerdocs/bbldf/images/item-of-rank1.jpg") 
(:p "Returns:")
(:img :src "/weblistenerdocs/bbldf/images/item-of-rank2.jpg")
(:p "In its simplest form, this function will take a list, sort it in ascending order, "
	"and return the element at the nth position, as determined by rank."))
(:KEYWORDS order rank list)
(:SEE-ALSO SORT)
)

;==========================================JOIN============================================

(DOCUMENT-FUNCTION JOIN
(:SUMMARY "Combines given elements into one string or one list" )
(:SYNTAX (JOIN list-of-elements [BY separator] [AS-LIST] [AS-STRING]))
(:SYNTAX (JOIN element1 element2 (etc) [BY separator] [AS-LIST] [AS-STRING]))

(:PARAMETERS
(list-of-elements :VALUE-TYPE list :DOCSTRING "The list of elements to be joined")
(item :VALUE-TYPE any :DOCSTRING "One element to be joined")
(BY :VALUE-TYPE any :PARAMETER-TYPE keyword :DOCSTRING "To be inserted between each element")
(AS-LIST :PARAMETER-TYPE flag :DOCSTRING "Combine elements into a list")
(AS-STRING :PARAMETER-TYPE flag :DOCSTRING "Combine elements into a string"))
(:RETURNS "A string or a list")
(:EXAMPLES "Detailed below, graphically.")
(:TEXT
(:p "To combine a list into a string:")
(:img :src "/weblistenerdocs/bbldf/images/join1.jpg") 
(:p "Returns:")
(:img :src "/weblistenerdocs/bbldf/images/join2.jpg")
(:p "")
(:p "To generate a list, by combining a list and strings:")
(:img :src "/weblistenerdocs/bbldf/images/join3.jpg") 
(:p "Returns:")
(:img :src "/weblistenerdocs/bbldf/images/join4.jpg")
(:p " If given individual elements, they are combined into a string (converted to a string, if necessary." (:BR "However, if AS-LIST is specified, then the elements are combined into a list."))
(:p " If any element is a list, the elements are combined into a list."
(:BR "However, if AS-STRING is specified, then all elements, even those within the list(s) are combined into a string.")))
(:KEYWORDS join merge concatenate cat)
(:SEE-ALSO MERGE INSERT))

;===============================KEGG-ID-OF================================================

(DOCUMENT-FUNCTION KEGG-ID-OF
(:SUMMARY "Returns the KEGG ID of a BioBIKE organism.")
(:SYNTAX (KEGG-ID-OF bborf))
(:PARAMETERS
(bborf :DOCSTRING "The BioBIKE organism you want to find the KEGG ID for." :value-type string)
)
(:EXAMPLES
"Currently non-functional."
)
(:RETURNS "The organism name(s).")
(:TEXT
(:p "KEGG IDs are a static reference, used by the Kyoto Encyclopedia of Genes and Genomes.")
)
(:KEYWORDS KEGG Organism)
(:SEE-ALSO HIGHLIGHT-GENE-IN-PATHWAY KEGG-ORGANISMS-OF)
)



;===================================LABELS-OF=============================================

(DOCUMENT-FUNCTION LABELS-OF
(:SUMMARY "Returns the labels used in a specified table.")
(:SYNTAX (LABELS-OF table [DIMENSION positive-integer]))
(:PARAMETERS
(table :DOCSTRING "The table you want to retrieve labels from." :value-type table)
(dimension :DOCSTRING "The dimension for which you want to retrieve labels." :value-type number)
)
(:EXAMPLES
"Detailed below, graphically."
)
(:RETURNS "A list.")
(:TEXT
(:p "Given the following table:")
(:img :src "/weblistenerdocs/bbldf/images/labels-of1.jpg") 
(:p "You can use this logic on that table object:")
(:img :src "/weblistenerdocs/bbldf/images/labels-of2.jpg")
(:p "To return the labels of the table:")
(:img :src "/weblistenerdocs/bbldf/images/labels-of3.jpg")
(:p "")
(:p "Or, you can use this construction to retrieve just one axis' labels:")
(:img :src "/weblistenerdocs/bbldf/images/labels-of4.jpg")
(:p "Returns:")
(:img :src "/weblistenerdocs/bbldf/images/labels-of5.jpg")
(:p "DIMENSION can be 1 or 2, 1 being the Y axis, and 2 being the X axis.")
)
(:KEYWORDS table axis axes labels)
(:SEE-ALSO MAKE NEW-TABLE)
)

;========================================LAST===========================================

(DOCUMENT-FUNCTION BBL::last
(:SUMMARY "Returns the last elements in a list or string, as specified.")
(:SYNTAX (last [number] [in] [in-each] entity [nonstrict] [strict]))
(:PARAMETERS
(number :DOCSTRING "The number of results to return." :value-type number)
(entity :DOCSTRING "The target of the function." :value-type required)
(in :DOCSTRING "When specified, ignores the internal structure of the target." :value-type token)
(in-each :DOCSTRING "When specified, searches within each inner list of a complex list." :value-type token)
(nonstrict :DOCSTRING "Deprecated")
(strict :DOCSTRING "Deprecated")
)
(:EXAMPLES
"Detailed below, graphically."
)
(:RETURNS "The values of the last [number] entities from the target list or string.")
(:TEXT
(:p "To get the last 5 genes on a chromosome:")
(:img :src "/weblistenerdocs/bbldf/images/last1.jpg") 
(:p "Returns:")
(:img :src "/weblistenerdocs/bbldf/images/last2.jpg")
(:p "")
(:p "LAST will return the specified number of string or list elements, starting from the end.")
)
(:KEYWORDS last sort end)
(:SEE-ALSO LOG10 LOG2)
)

;==========================================LENGTH-OF==================================

(DOCUMENT-FUNCTION LENGTH-OF  
  (:RETURNS "A number of elements.")  
  (:PARAMETERS     
    (entity :docstring "Object to be evaluated" :parameter-type required)     
    (each :docstring "If specified, evaluates each element of a list." :parameter-type token :value-type boolean))  
  (:EXAMPLES "Detailed below, graphically.")
  (:TEXT (:p "The function LENGTH-OF counts the number of elements within a given entity.")
  (:ul ((:table border 1 rules "all" cellpadding 4)
      	(:tr (:td (:small (:b "IF GIVEN")))
           (:td (:small (:b "RETURNS"))))
		(:tr (:td (:small "string"))
	     (:td (:small "the number of characters (spaces included)")))
	(:tr (:td (:small "a gene, a protein or a contiguous-sequence"))
           (:td (:small "the number of nucleotides or amino-acids that compose their sequences")))  (:tr (:td (:small "an organism"))
           (:td (:small "the number of nucleotides that compose its genome")))
  (:tr (:td (:small "list"))
	     (:td (:small "the number of elements (sublists, strings, genes, proteins, contiguous-sequences or organisms)  within the list")))))
(:p "")
(:p "To take the length of list (How many genes are in this list?):")
(:img :src "/weblistenerdocs/bbldf/images/length-of1.jpg") 
(:p "Returns:")
(:img :src "/weblistenerdocs/bbldf/images/length-of2.jpg")
(:p "")
(:p "To take the length of each gene in a group of genes:")
(:img :src "/weblistenerdocs/bbldf/images/length-of3.jpg") 
(:p "Returns:")
(:img :src "/weblistenerdocs/bbldf/images/length-of4.jpg")
(:p "")
(:p "Note: The option EACH counts the number of elements within entities themselves within a list.")
     )
(:KEYWORDS length count)
(:SEE-ALSO lengths-of))

;============================================LENGTHS-OF===================================

(DOCUMENT-FUNCTION LENGTHS-OF
(:SUMMARY "Returns a distilled count of the basic element in a complex list or organism.")
(:PARAMETERS
(entity :docstring "The list/string/organism/table to be evaluated.")
(each :docstring "Deprecated."))
(:EXAMPLES
"Detailed below, graphically."
)
(:TEXT
(:img :src "/weblistenerdocs/bbldf/images/lengths-of1.jpg") 
(:p "Returns:")
(:img :src "/weblistenerdocs/bbldf/images/lengths-of2.jpg")
(:p "")
(:p "'LENGTHS-OF' distills down to the basic element, a nested series of lists.  It then returns "
	"the count of that basic element.  In this case, an organism is composed of chromosomes, which "
	"are composed of genes, which are composed of nucleotides.  LENGTHS-OF the organism will give "
	"the combined lengths of all genes, which is the count of all nucleotides."))
(:KEYWORDS length list loop)
(:SEE-ALSO LENGTH-OF FOR-EACH LOOP))

;============================================LESS-THAN==================================

(DOCUMENT-FUNCTION LESS-THAN
(:SUMMARY "Compares two strings alphanumerically.")
(:SYNTAX (less-than [each] x y [case-sensitive] [give-position] ))
(:PARAMETERS
(x :DOCSTRING "The first string to compare." :value-type string)
(y :DOCSTRING "The second string to compare." :value-type string)
(case-sensitive :DOCSTRING "Indicates whether to factor case into comparison." :value-type token)
(give-position :DOCSTRING "When selected, returns the position that determines the relationship." :value-type token)
)
(:EXAMPLES
"(LESS-THAN 'abracadabra' 'xylophone') Returns: T"
"(LESS-THAN 'xylophone' 'abracadabra') Returns: NIL" 
"(LESS-THAN 'aardvark' 'aardwolf') Returns: 5"
)
(:RETURNS "A boolean (sort of.)")
(:TEXT
(:p "Two alpha comparisons:")
(:img :src "/weblistenerdocs/bbldf/images/less-than1.jpg") 
(:p "Returns:")
(:img :src "/weblistenerdocs/bbldf/images/less-than2.jpg")
(:p "")
(:img :src "/weblistenerdocs/bbldf/images/less-than3.jpg") 
(:p "Returns:")
(:img :src "/weblistenerdocs/bbldf/images/less-than4.jpg")
(:p "An alpha comparison with the give-position token:")
(:img :src "/weblistenerdocs/bbldf/images/less-than5.jpg") 
(:p "Returns:")
(:img :src "/weblistenerdocs/bbldf/images/less-than6.jpg")
(:p "This function performs an alphanumeric comparison of two strings, "
    "and returns TRUE if the first value is lower than the second, and NIL "
	"otherwise."
    )
)
(:KEYWORDS string comparison compare less than)
(:SEE-ALSO > < GREATER-THAN)
)

;=======================================LIST=========================================

(DOCUMENT-FUNCTION LIST
(:SUMMARY "Creates a list composed of the specified items.")
(:SYNTAX (list item ))
(:PARAMETERS
(item :DOCSTRING "The first item in the list.  More boxes may be opened to add more elements." :value-type item)
)
(:EXAMPLES
"(LIST 5) Returns: (5)" 
"(LIST 'A' 'T' 'C' 'G') Returns: ('A' 'T' 'C' 'G')"
)
(:RETURNS "A list")
(:TEXT
(:p "A numeric list:")
(:img :src "/weblistenerdocs/bbldf/images/list1.jpg") 
(:p "Returns:")
(:img :src "/weblistenerdocs/bbldf/images/list2.jpg")
(:p "A list of strings:")
(:img :src "/weblistenerdocs/bbldf/images/list3.jpg") 
(:p "Returns:")
(:img :src "/weblistenerdocs/bbldf/images/list4.jpg")
(:p "This function returns a list of the specified length, with the required contents.  "
	"More entries can be added with the ADD ANOTHER or ADD TWO MORE drop-down functions, "
	"accessible from the MORE menu."
    )
)
(:KEYWORDS list)
(:SEE-ALSO JOIN MAKE)
)

;=======================================LOG==========================================

(DOCUMENT-FUNCTION bbl::log
(:SUMMARY "Calculates the natural log of an entity unless another base is specified.")
(:SYNTAX (log entity ))
(:PARAMETERS
(number :DOCSTRING "The number for which you plan to take the natural log." :value-type number)
(base :DOCSTRING "The base by which you will take the logarithm." :value-type number)
)
(:EXAMPLES
"(LOG 2.77) Returns: 1.0188473 (a close approximation of taking the natural log of e" 
"(LOG 125 5) Returns: 3.0"
)
(:RETURNS "A number")
(:TEXT
(:p "The case of a natural log:")
(:img :src "/weblistenerdocs/bbldf/images/log1.jpg") 
(:p "Returns:")
(:img :src "/weblistenerdocs/bbldf/images/log2.jpg")
(:p "The case of a log base 5:")
(:img :src "/weblistenerdocs/bbldf/images/log3.jpg") 
(:p "Returns:")
(:img :src "/weblistenerdocs/bbldf/images/log4.jpg")
(:p "This function returns the natural logarithm of the number specified, "
    "unless another base is indicated.  "  
    "It will accept a number, or any expression which returns a number.  "
    "Lists of numbers are also acceptable, with the result being a list of logarithms, of the respective numbers.")
)
(:KEYWORDS logarithm exponential log)
(:SEE-ALSO LOG10 LOG2)
)

;=======================================LOG10============================================

(DOCUMENT-FUNCTION LOG10
(:SUMMARY "Calculates the logarithm base 10 of an entity ")
(:SYNTAX (log10 entity ))
(:PARAMETERS
(number :DOCSTRING "The number for which you plan to take the log base 10." :value-type number)
)
(:EXAMPLES
"(LOG 100) Returns: 2.0"
"(LOG10 '(1 10 100)) Returns: (0.0 1.0 2.0)"
)
(:RETURNS "A number")
(:TEXT
(:img :src "/weblistenerdocs/bbldf/images/log101.jpg") 
(:p "Returns:")
(:img :src "/weblistenerdocs/bbldf/images/log102.jpg")
(:p "This function returns the logarithm base 10 of the number specified.  "  
    "It will accept a number, or any expression which returns a number.  "
    "Lists of numbers are also acceptable, with the result being a list of logarithms, base 10, of the respective numbers.")
)
(:KEYWORDS logarithm exponential log)
(:SEE-ALSO LOG LOG2)
)

;=====================================LOG2============================================

(DOCUMENT-FUNCTION LOG2
(:SUMMARY "Calculates the logarithm base 2 of an entity ")
(:SYNTAX (log2 entity ))
(:PARAMETERS
(number :DOCSTRING "The number for which you plan to take the log base 2." :value-type number)
)
(:EXAMPLES
"(LOG2 8) Returns: 3.0"
"(LOG2 '(1 2 3)) Returns: (0.0 1.0 1.5849625)"
)
(:RETURNS "A number")
(:TEXT
(:img :src "/weblistenerdocs/bbldf/images/log21.jpg") 
(:p "Returns:")
(:img :src "/weblistenerdocs/bbldf/images/log22.jpg")
(:p "This function returns the logarithm base 2 of the number specified.  It will accept a number, or any expression which returns a number.")
(:p "Lists of numbers are also acceptable, with the result being a list of logarithms, base 2, of the respective numbers.")
)
(:KEYWORDS logarithm exponential log)
(:SEE-ALSO LOG LOG10)
)

;=======================================MAKE=============================================
(DOCUMENT-FUNCTION MAKE
(:SUMMARY "A simple generator, to build lists, strings, or tables of any description.")
(:RETURNS "A list, string, or table" :type (or list string table) :display-type nil)
(:PARAMETERS 
(what? :DOCSTRING "A dropdown menu, used to specify whether you are building a list, string, or table.")
(how-big :DOCSTRING "Used to specify the size of the entity you are creating, can accept integers, lists of integers, or regular expressions.  (regular expressions currently not implemented for MAKE)")
(initial-element :DOCSTRING "The value you wish to set as the default contents of the entity.")
)
(:EXAMPLES
)

(:TEXT
(:p "To generate a list of 6 'x'")
(:img :src "/weblistenerdocs/bbldf/images/make1.jpg")
(:p "Returns:")
(:img :src "/weblistenerdocs/bbldf/images/make2.jpg")
(:p "")
(:p "To generate a list that will contain descriptions of all the genes of an organism:")
(:img :src "/weblistenerdocs/bbldf/images/make3.jpg")
(:p "")
(:img :src "/weblistenerdocs/bbldf/images/make4.jpg")
(:p "Returns:")
(:img :src "/weblistenerdocs/bbldf/images/make5.jpg")
(:p "")
(:p "To create a string of three y's:")
(:img :src "/weblistenerdocs/bbldf/images/make6.jpg")
(:p "Returns:")
(:img :src "/weblistenerdocs/bbldf/images/make7.jpg")
(:p "To build a 2x2 table, composed of 0's:")
(:img :src "/weblistenerdocs/bbldf/images/make8.jpg")
(:p "Returns:")
(:img :src "/weblistenerdocs/bbldf/images/make9.jpg")
)
(:KEYWORDS generation create string table list)
(:SEE-ALSO LIST INSERT INSIDE-LIST INSIDE-STRING REPLACE STRING-OF ELEMENT-OF-TABLE)
)




;=======================================MAKE-PSSM-FROM===================================

(DOCUMENT-FUNCTION MAKE-PSSM-FROM
(:SUMMARY "Produces a position-specific scoring matrix from an aligned list of sequences")
(:SYNTAX (MAKE-PSSM-FROM alignment [DNA] [PROTEIN] [PSEUDOCOUNTS] [BACKGROUND-FROM] [FROM] [TO]))
(:PARAMETERS
(alignment :DOCSTRING "The alignment of a list of DNA or Protein sequences, generally produced by ALIGNMENT-OF" :value-type ALIGNMENT)
)
(:EXAMPLES)
(:RETURNS "A pop-up, with the Position-Specific Scoring Matrix of the specified alignment.")
(:TEXT
(:img :src "/weblistenerdocs/bbldf/images/make-pssm-from1.jpg") 
(:p "Returns:")
(:img :src "/weblistenerdocs/bbldf/images/make-pssm-from2.jpg")
(:p "This function allows the creation of PSSMs, a basic Bioinformatics tool for the comparison of DNA and Proteins, especially in")
(:p "orthologous relationships, between species.")
)
(:KEYWORDS pssm matrix alignment ortholog)
(:SEE-ALSO ALIGNMENT-OF APPLY-PSSM-TO #+unknown PSSM (glossary ORTHOLOG))
)

;=========================MATCHES-OF-ITEM================================================

(DOCUMENT-FUNCTION MATCHES-OF-ITEM
(:SUMMARY "Finds match of some part of a list or string in a target.")
(:SYNTAX (MATCHES-OF-ITEM [EACH] query [IN] [IN-EACH] target))
(:PARAMETERS
(query :DOCSTRING "A string or part of a list to search for." :value-type string)
(target :DOCSTRING "The target entity to be searched.")
(each :DOCSTRING "Indicates that the search should be re-evaluated against multiple elements of a target list." :value-type token)
(in :DOCSTRING "Chooses whether to search the object specified or the contents of the object specified." :value-type token)
(in-each :DOCSTRING "Indicates that the search should be re-evaluated against multiple element-contents of a target list." :value-type token)
(partial :DOCSTRING "Indicates that an incomplete match is acceptable." :value-type flag)
(case-sensitive :DOCSTRING "Indicates that case is a mandatory search factor." :value-type flag)
(position-only :DOCSTRING "Restricts results to the positions, without returning the match." :value-type flag)
(match-only :DOCSTRING "Restricts results to the matches, without returning the positions." :value-type flag)
)
(:EXAMPLES
"(MATCHES-OF-ITEM 'ATAG' IN 'CATAGC') Returns: ((2 'A'))"
)
(:RETURNS "A location in the target.")
(:TEXT
(:img :src "/weblistenerdocs/bbldf/images/matches-of-item1.jpg") 
(:p "Returns:")
(:img :src "/weblistenerdocs/bbldf/images/matches-of-item2.jpg")
(:p "A more restricted version of MATCHES-OF-PATTERN, this function does not allow for regular expressions, "
    "but strings can be used to search within the target string or list.  This is most useful when searching "
	"for a particular sequence i.e. a promoter region within a gene or genome.")
)
(:KEYWORDS search match item)
(:SEE-ALSO MATCHES-OF MATCHES-OF-PATTERN)
)

;====================================MATCHES-OF-PATTERN================================


(DOCUMENT-FUNCTION MATCHES-OF-PATTERN
  (:SUMMARY "Scans a string or list of strings for possibly loose matches of a pattern.")
 
  (:VPL-SYNTAX
     (:FOO
        (:img :src "/weblistenerdocs/bbldf/images/MATCHES-OF-PATTERN-syntax.PNG"))
     ) 

  (:PARAMETERS
    (pattern :DOCSTRING "Pattern used in the search (sometimes called a 'regular expression'). May be a list of patterns."
       :value-type string)
    (target :DOCSTRING "The entity to be searched or list of entities. See DESCRIPTION for how the entities are interpreted."
       :value-type string)
    (as-DNA :DOCSTRING "Pattern is to be interpretted according to the conventions of DNA (see DESCRIPTION)." :value-type flag)
    (as-REGEX :DOCSTRING "Pattern is to be interpretted as a regular expression, similar to those recognized by Perl."
       :value-type flag)
    (both-strands :DOCSTRING "Both the given sequence and its complementary strand will be searched (DEFAULT when the sequence is recognized as DNA)." :value-type flag)
    (case-sensitive :DOCSTRING "Matches found must agree with the pattern in capitalization." :value-type flag)
    (cross-lines :DOCSTRING "Matches are found even if it is necessary to search over multiple lines of text." 
       :value-type flag)
    (one-strand :DOCSTRING "Only the given string is searched (DEFAULT unless the target is recognized as DNA)." 
       :value-type flag)
    (text :DOCSTRING "Targets are interpreted as text, not DNA." :value-type flag)
    (+1st-match-only :DOCSTRING "If multiple matches are found, only the first is reported, and the results are presented as a single element, not a list." :value-type flag)
    (+compression :DOCSTRING "NIL results (failure to match) are omitted (DEFAULT for lists of greater than 20 element)."
       :value-type flag)
    (-compression :DOCSTRING "All results are shown, even NIL (failure to match) (DEFAULT for lists with at most 20 elements)." :value-type flag)
    (+coordinates :DOCSTRING "Coordinates of full matches and sub-matches will be included in the results (DEFAULT)."
       :value-type flag)
    (-coordinates :DOCSTRING "Coordinates of full matches and sub-matches will not be included in the results." 
       :value-type flag)
    (+full-coordinates :DOCSTRING "Coordinates of full matches but not sub-matches will be included in the results."
       :value-type flag)
    (-full-coordinates :DOCSTRING "Coordinates of full matches wil not be included in the results." :value-type flag)
    (+sub-coordinates :DOCSTRING "Coordinates of sub-matches but not full matches will be included in the results."
       :value-type flag)
    (-sub-coordinates :DOCSTRING "Coordinates of sub-matches will not be included in the results." :value-type flag)
    (+matches :DOCSTRING "Sequences of full matches and sub-matches will be included in the results (DEFAULT)." 
       :value-type flag)
    (-matches :DOCSTRING "Sequences of full matches and sub-matches will not be included in the results." :value-type flag)
    (+full-matches :DOCSTRING "Sequences of full matches but not sub-matches will be included in the results." 
       :value-type flag)
    (-full-matches :DOCSTRING "Sequences of full matches will not be included in the results." :value-type flag)
    (labeled :DOCSTRING "Provides context information with results, including organism and location within genome."
       :value-type flag)
    (+sub-matches :DOCSTRING "Sequences of sub-matches but not full matches will be included in the results." :value-type flag)
    (-sub-matches :DOCSTRING "Sequences of sub-matches will not be included in the results." :value-type flag)
    (+target-label :DOCSTRING "Each result is preceded by the name of the target or its position in a list (DEFAULT when target is a list)." :value-type flag)
    (-target-label :DOCSTRING "The name of the target is omitted (DEFAULT when target is not a list)." :value-type flag)
    (+pattern-label :DOCSTRING "Each result is preceded by the position of the pattern in a list of patterns." 
       :value-type flag)
    (-pattern-label :DOCSTRING "The position of the pattern is omitted (DEFAULT)." :value-type flag)
    (+display :DOCSTRING "The result will be displayed in a popup window (DEFAULT if the result is a list and the function is invoked directly)." :value-type flag)
    (-display :DOCSTRING "The result will not be displayed in a popup window." :value-type flag)
    )

 (:EXAMPLES
     (:FOO
      (:BLOCKQUOTE
        (:img :src "/weblistenerdocs/bbldf/images/MATCHES-OF-PATTERN-ex-1.jpg") 
        (:BR)"--> ((\"GAAT\" \"GTAT\") (\"GATA\" \"AGTT\") (\"CTGA\" \"TCTA\") (\"GTGG\" \"CTAT\")...)"
        (:P "This function displays the sequences in Anabaena PCC 7120 (A7120) flanking "
            "the highly repeated sequence, GCGATCGC. "
            "The pattern directs that the search capture the four unrestricted nucleotides " 
            "preceding and following the fixed sequence GCGSTCGC.")
        (:P "Why the three options?"
        (:BR)"- ONE-STRAND to avoid double-counting the palindrome"
        (:BR)"- SUB-MATCHES to avoid displaying coordinates and full matches"
        (:BR)"- -TARGET-LABEL to avoid labeling each sub-match with the name of the replicon"
        ))
      (:BLOCKQUOTE
        (:img :src "/weblistenerdocs/bbldf/images/MATCHES-OF-PATTERN-ex-2.jpg")
        (:BR)"--> (S6803.p-Sll0031 S6803.p-Sll0520 S6803.p-Sll0741 S6803.p-Sll0823 S6803.p-Sll1223...)"
        (:P "This function identifies proteins in Synechocystis PCC 6803 (S6803) that "
             "contain an iron-binding motif common to many FeS oxidoreductases. "
             "The pattern directs that the search find any amino acid sequence with four consecutive "
             "cysteines separated by 2, 2, and 3 unspecified amino acids.")
        (:P "The -MATCHES and -COORDINATES options were employed so that the result "
             "would contain only the name of the protein, not coordinates and sequences.")
        )
      (:BLOCKQUOTE
        (:img :src "/weblistenerdocs/bbldf/images/MATCHES-OF-PATTERN-ex-3.jpg")
        (:BR)
        (:PRE 
"        8547      8615   NNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNN...
      734581    734755   NNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNN...
      964609    965136   NNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNN...
     1036878   1037433   NNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNN...
     1368498   1368826   NNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNN...")
        "This function determines whether there are regions of the genome of Synechococcus BL107 "
        "that remain incomplete. Evidently, there are five such regions. "
        "The pattern directs that the search find any number of consecutive "
        "nonstandard nucleotides, i.e. letters that are " (:I "not") " A, C, G, or T."
      )

   ))
 (:RETURNS "A list of matched patterns, with the details specified in the flags.")

 (:TEXT
    "MATCHES-OF-PATTERN applies a pattern to a target, or if the target is a list, then to each element of the list. If one or more matches are found, the function returns a report of what it found according to rules discussed below."
    (:P (:U (:B "How matches are found -- the pattern language"))
      (:BR) "BioBIKE uses its own pattern syntax, which is simpler but almost as expressive as the "
            ((:A :HREF "http://regexlib.com/CheatSheet.aspx")
            "syntax used by Perl and other languages")
            ". Those wishing to use that syntax should specify the AS-REGEX option."
        (:P "The pattern can be thought of as consisting of a mixture of three " 
            "types of components, all of which are optional:"
        (:UL
          (:LI (:U "Literals") ": These mean exactly what they appear to mean. For example, "
               "pattern consisting of \"Apricot\" would search the target for this string of letters.")
         (:LI (:U "Character sets") ": These indicate groups of symbols that would be permissible in "
                  "a match. For example, a pattern \"###-##-####\" would match any string of numbers "
                  "separated as shown by hyphens, such as \"123-45-6789\".")
         (:LI (:U "Repetition specifications") ": These indicate how a symbol or character set should be "
                 "repeated. For example, the pattern \"[ACGT]...\" matches any number of consecutive "
                 "members of the defined character set, i.e. the nucleotides.")
        (:LI (:U "Other directives") ": These indicate how the search is to be conducted. The most important "
                 "directive is ( ), parentheses, which indicate a captured sub-match, i.e. a portion of the match that "
                 "is to be returned separately."
        ))
        "A more complete description may be found "
        ((:A :HREF "/weblistenerdocs/externaldf/BioBIKE-Pattern-Matching.pdf")
           "here") ".")
        (:P "Either the BioBIKE or the conventional pattern syntax may be supplemented by invoking "
            "the AS-DNA option. This causes certain letters to refer to nucleotide sets. "
            "For example, \"R\" would refer to the purines, \"A\" and \"G\". A full list of sets "
            "can be found "
            ((:A :HREF (:PRINT (help::MAKE-HELP-TOPIC-URL :NAME "Extended nucleotide symbols")))
                "here") "."
            ))
            
    (:P (:U (:B "What is returned by the function -- the formatting flags"))
      (:BR) "Flags that begin + or - indicate how the results are to be displayed or returned, "
            "either generating the indicated information or following the indicated procedure (+) "
            " or not doing so (-). The flags can be considered as part of the following groups:"
        (:UL
          (:LI (:U "Coordinates and matches") ": These flags govern the appearance of the "
            "coordinates of the full match (FULL-COORDINATES) and the sequence of that match (FULL-MATCHES) "
            "and the coordinates "
            "of any matches captured by the pattern (SUB-COORDINATES) and their sequences (SUB-MATCHES). "
            "The COORDINATES flag governs both FULL-COORDINATES and SUB-COORDINATES and likewise, "
            "the MATCHES flag governs both FULL-MATCHES and SUB-MATCHES."
            (:BR) 
            (:BR) "If any flag in this group is specified, then all other categories are deemed to "
            "be turned off. For example, Specifying +SUB-MATCHES implies -FULL-MATCHES & -COORDINATES, "
            "unless you specify otherwise. If no flag in this group is specified, then it is presumed "
            "that " (:I "all") " information is to presented (i.e. +MATCHES +COORDINATES)." (:BR) (help::HTML "&nbsp;")
            )
         (:LI (:U "Target and pattern labels") ": If you search a list of targets, it is helpful to "
                  "keep track of which target produced which hit. If +TARGET-LABEL is specified or "
                  "implied, then the match information is preceded by the name of the target. If the "
                  "target has no name, then the position of the target in the list is used instead. "
                  "+TARGET-LABEL is the default behavior when the target is a list."
                  (:BR) 
                  (:BR) "It is also possible to search one or more targets with a list of patterns, "
                  "and as with targets, it is possible to associate the each result with the position "
                  " of the pattern used. If both +PATTERN-LABEL and +TARGET-LABEL are specified, then "
                  "the pattern label is presented first in the results." (:BR) (help::HTML "&nbsp;"))
         (:LI (:U "COMPRESSION") ": If the search is over a long list of targets, then the story the results "
                  "have to tell may be obscured by an overwhelming number of nonmatches. +COMPRESSION causes "
                  "nonmatches to be omitted from the results. It is the default behavior for long lists of targets." 
                  (:BR) (help::HTML "&nbsp;")) 
         (:LI (:U "1st-MATCH-ONLY") ": Unless this flag is specified, the search leaves open the possibility that "
                  "it may find multiple matches. Therefore the results of the search will be given as a list, even "
                  "if in fact only one match is found. If you know that there is only one match, or if you're "
                  "interested in only the first match, you can simplify the results by specifying this flag. "
                  (:BR) (help::HTML "&nbsp;"))
         (:LI (:U "DISPLAY") ": This flag turns on and off the popup window that displays results in "
                  "human-readable format. +DISPLAY is the default when MATCHES-OF-PATTERN is invoked directly, "
                  "and -DISPLAY is the default otherwise.")			  
         )
            
      )
     )
 (:KEYWORDS regexp regular expression pattern match search)
 (:SEE-ALSO MATCHES-OF-ITEM SEQUENCE-SIMILAR-TO 
    (URL "http://biobike-8002.csbc.vcu.edu/weblistenerdocs/externaldf/BioBIKE-Pattern-Matching.pdf"
          "BioBIKE Pattern Matching")
   )
 )

;===========================================MAX-OF========================================

(DOCUMENT-FUNCTION MAX-OF
(:SUMMARY "Returns the largest argument from a list. ")
(:SYNTAX (MAX-OF list))
(:PARAMETERS
(list :VALUE-TYPE list :DOCSTRING "A numeric list of arbitrary length.  More than one list can be specified, the function will choose the smallest item from all lists.  Only one box need be completed for the function to work, but more can be specified.")
)
(:RETURNS "A number.")
(:EXAMPLES
"(MAX-OF {0.5 2 3 4 5 6 7 8 9 }) Returns: 9"
)
(:TEXT
(:img :src "/weblistenerdocs/bbldf/images/max-of1.jpg") 
(:p "Returns:")
(:img :src "/weblistenerdocs/bbldf/images/max-of2.jpg")
(:p "To determine the largest genome in a group of organisms:")
(:img :src "/weblistenerdocs/bbldf/images/max-of3.jpg") 
(:p "Returns:")
(:img :src "/weblistenerdocs/bbldf/images/max-of4.jpg")
(:p " 1. The function returns the largest element in the list regardless of its position within the list.")
(:p " 2. The elements of the list must be numbers or functions that return numbers. ")
)
(:KEYWORDS greatest largest biggest)
(:SEE-ALSO MIN-OF)) 

;=====================================MEAN=============================================

(DOCUMENT-FUNCTION BBL::Mean
(:SUMMARY "Takes the mean of a list of numbers.")
(:SYNTAX (MEAN list))
(:PARAMETERS
(list :DOCSTRING "The list of numbers." :value-type list))
(:EXAMPLES
"(MEAN '(0 1 2 3 4)) Returns: 2")
(:RETURNS "A number.")
(:TEXT
(:img :src "/weblistenerdocs/bbldf/images/mean1.jpg") 
(:p "Returns:")
(:img :src "/weblistenerdocs/bbldf/images/mean2.jpg")
(:p "To find the mean gene length of all the genes in an organism:")
(:img :src "/weblistenerdocs/bbldf/images/mean3.jpg")
(:p "Returns:")
(:img :src "/weblistenerdocs/bbldf/images/mean4.jpg") 
(:p "This function will take the mean of a list of numbers, or a list of functions that return numbers."))
(:KEYWORDS mean statistics average)
(:SEE-ALSO MEDIAN)
)

;=========================================MEDIAN========================================

(DOCUMENT-FUNCTION MEDIAN
(:SUMMARY "Takes the median of a list of numbers.")
(:SYNTAX (MEDIAN list))
(:PARAMETERS
(list :DOCSTRING "The list of numbers." :value-type list))
(:EXAMPLES
"(MEDIAN '(0 1 2 3 4)) Returns: 2")
(:RETURNS "A number.")
(:TEXT
(:img :src "/weblistenerdocs/bbldf/images/median1.jpg") 
(:p "Returns:")
(:img :src "/weblistenerdocs/bbldf/images/median2.jpg")
(:p "To find the median gene length of all the genes in an organism:")
(:img :src "/weblistenerdocs/bbldf/images/median3.jpg")
(:p "Returns:")
(:img :src "/weblistenerdocs/bbldf/images/median4.jpg") 
(:p "This function will take the median of a list of numbers, or a list of functions that return numbers."))
(:KEYWORDS median statistics mean)
(:SEE-ALSO MEAN)
)

;====================================MIN-OF===========================================

(DOCUMENT-FUNCTION MIN-OF
(:SUMMARY "Returns the smallest argument from a list. ")
(:SYNTAX (MIN-OF list))
(:PARAMETERS
(list :VALUE-TYPE list :DOCSTRING "A numeric list of arbitrary length.  More than one list can be specified, the function will choose the smallest item from all lists.  Only one box need be completed for the function to work, but more can be specified.")
)
(:RETURNS "A number.")
(:EXAMPLES
"(MIN-OF {0.5 2 3 4 5 6 7 8 9 }) Returns: 0.5"
)
(:TEXT
(:img :src "/weblistenerdocs/bbldf/images/min-of1.jpg") 
(:p "Returns:")
(:img :src "/weblistenerdocs/bbldf/images/min-of2.jpg")
(:p "To determine the smallest genome in a group of organisms:")
(:img :src "/weblistenerdocs/bbldf/images/min-of3.jpg") 
(:p "Returns:")
(:img :src "/weblistenerdocs/bbldf/images/min-of4.jpg")
(:p " 1. The function returns the smallest element in the list regardless of its position within the list.")
(:p " 2. The elements of the list must be numbers or functions that return numbers. ")
)
(:KEYWORDS smallest lowest littlest)
(:SEE-ALSO MAX-OF)) 

;===========================================MOD=========================================

(DOCUMENT-FUNCTION MOD
(:SUMMARY "Takes the modulo function of a dividend and a divisor.  
     Often used in larger logical constructs.")
(:SYNTAX (MOD number divisor))
(:PARAMETERS
(number :DOCSTRING "The dividend." :value-type number)
(divisor :DOCSTRING "The divisor." :value-type number))
(:EXAMPLES
"(MOD 5 2) Returns: 1")
(:RETURNS "A number.")
(:TEXT
(:img :src "/weblistenerdocs/bbldf/images/mod1.jpg") 
(:p "Returns:")
(:img :src "/weblistenerdocs/bbldf/images/mod2.jpg")
(:p "One common use of this function is to check if a number is even:")
(:img :src "/weblistenerdocs/bbldf/images/mod3.jpg") 
(:p "Returns:")
(:img :src "/weblistenerdocs/bbldf/images/mod4.jpg")
(:p "In this example, if the function returns 0, the variable being checked is even.  Any non-zero result indicates an odd variable.")
(:p "This function performs modulus division on a number, returning the remainder only."))
(:KEYWORDS division remainder modulus)
(:SEE-ALSO / DIVIDE)
)



;=======================================MY-SESSION=====================================

(DOCUMENT-FUNCTION MY-SESSION
(:SUMMARY "Displays information about the user's active sessions.")
(:SYNTAX (MY-SESSION))
(:PARAMETERS)
(:EXAMPLES
"MY-SESSION Displays:   Your user ID:  SOMEUSER         |Your login name
                        Your session ID:  SOMEUSER43226    |Your unique session number
                   Your active sessions:  5                |How many sessions you have open
                           Active users:  5                |The number of users logged into the system
                     Users in last hour:  1                |
                         System started:  2009-05-25 09:09 |The most recent server boot-up time
                            BBL version:  2009-04-26 11:43 |The most recent server compile
                            VPL version:  2                |The version of the visual environment")
(:RETURNS "A display of current session information.")
(:TEXT
(:img :src "/weblistenerdocs/bbldf/images/my-session1.jpg") 
(:p "Returns:")
(:img :src "/weblistenerdocs/bbldf/images/my-session2.jpg")
(:p "This function displays in a pop-up the current session ID, user ID, and information about the system.")
(:p "If you're sharing a session, the session ID is extremely useful, so that the person on the other end")
(:p "can connect to your workspace.")
)
(:KEYWORDS user session share)
(:SEE-ALSO #+unknown SHARE-SESSION)
)

;=====================================MY-VARIABLES======================================

(DOCUMENT-FUNCTION MY-VARIABLES
(:SUMMARY "Returns a list of the user's functions created during any sessions which are still open.")
(:SYNTAX (MY-VARIABLES))
(:PARAMETERS
(as-list :DOCSTRING "A deprecated function, not in current use." :parameter-type token :value-type boolean)
(verbose :DOCSTRING "A deprecated function, not in current use." :parameter-type token :value-type boolean))
(:EXAMPLES
"MY-VARIABLES Returns: '(TEMP SHANK RIEMANN)")
(:RETURNS "A list of the user's functions.")
(:TEXT
(:img :src "/weblistenerdocs/bbldf/images/my-variables1.jpg") 
(:p "Returns:")
(:img :src "/weblistenerdocs/bbldf/images/my-variables2.jpg")
(:p "In VPL, the user can select the VARIABLES drop-down menu to see these results in a more meaningful and selectable context.")
(:p "Since the introduction of VPL, this function is useful only in BBL."))
(:KEYWORDS user-defined variable)
(:SEE-ALSO DEFINE)
) 
