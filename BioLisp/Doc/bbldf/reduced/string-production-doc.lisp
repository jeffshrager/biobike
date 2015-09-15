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

;;; Author:  Michiko Kato, Arnaud Taton, Bogdan Mihai, Jeff Elhai


(eval-when (:compile-toplevel :load-toplevel :execute)
  (import '(com.biobike.help:document-module
               com.biobike.help:document-function) :bbi))

(DOCUMENT-MODULE STRING-PRODUCTION 
  "Functions that manipulate and produce strings"
  (:keywords :string :convert :replace :permute :combinations :combine :break)
  (:display-modes :bbl)
  (:toplevel? nil)  
#.`(:functions insert split join all-combinations-of shuffle string-of item items
         reverse repeat Fit Transliterate Trim inside-string))


(DOCUMENT-FUNCTION RANDOM-DNA  
  (:PARAMETERS     
    (like :docstring "Sequence to be emulated in nucleotide content and/or length")     
    (frequencies :docstring "List of nucleotide frequencies to be applied")     
    (length :docstring "Length of random sequence to be produced"))

  (:EXAMPLES "1. Emulate a genome
       (RANDOM-DNA LIKE ss120) 
       --> Sequence of same length and nucleotide content as ss120"
             "2. Emulate a specific sequence
       (RANDOM-DNA LIKE (SEQUENCE-UPSTREAM-OF all4312)
       --> Sequence of same length an nucleotide content as the region upstream from the gene"
             "3. Produce a sequence according to a set of frequencies 
       (RANDOM-DNA FREQUENCIES {.3 .2 .2 .3})
       --> 1000-nt sequence with 30% A, 20% C, 20% G, and 30% T"
             "4. Produce a sequence according to a simple nucleotide content
       (RANDOM-DNA LIKE \"AATTCG\" LENGTH 100)
       --> 100-nt sequence with A:C:G:T in the ratio of 2:1:1:2"
    )

  (:TEXT 
(:center (:img :src "http://ramsites.net/~biobike/temp/random-dna.jpg"))
    
(:p "The function RANDOM-DNA produces a random nucleotide sequence according to the following rules:")
       ((:table border 1 rules "all" cellpadding 4)
      	(:tr (:td (:small (:b "OPTION")))
             (:td (:small (:b "NUCLEOTIDE FREQUENCIES")))
             (:td (:small (:b "SEQUENCE LENGTH"))))
	(:tr (:td (:small (:i "none")))
	     (:td (:small "[A] = [C] = [G] = [T]"))
	     (:td (:small 1000)))
	(:tr (:td (:small "LIKE"))
	     (:td (:small "same as sequence"))
	     (:td (:small 1000)))
	(:tr (:td (:small "FREQUENCIES"))
	     (:td (:small "given by FREQUENCIES list"))
	     (:td (:small 1000)))
	(:tr (:td (:small "LIKE + LENGTH"))
	     (:td (:small "same as sequence"))
	     (:td (:small "given by LENGTH")))
	(:tr (:td (:small "FREQUENCIES + LENGTH"))
	     (:td (:small "given by FREQUENCIES list"))
	     (:td (:small "given by LENGTH")))
        ) 
(:p "RANDOM-DNA is useful to generate sequences that are similar in nucleotide content to natural sequences of interest. For example, if an interesting short sequence occurs upstream from a gene, you may want to determine how often it would occur by chance in a region with the same size and nucleotide content. The sequence to be emulated is provided using the LIKE option.")

(:p "If a sequence is not provided using the LIKE option, then a random sequence is generated according to a set of nucleotide frequencies. These frequencies may be provided using the FREQUENCIES option, which calls for a list of four numbers, corresponding to the relative ratios of A, C, G, and T, respectively. The numbers need not be fractions. If frequencies are not provided, then a frequency of 25% is presumed for all four nucleotides.")

(:p "LIKE and FREQUENCIES options are incompatible with each other. However, LENGTH can be used with either of the two or alone.")

     )
(:SEE-ALSO )
)

;; ================================== REVERSE =========================================

(DOCUMENT-FUNCTION REVERSE
  (:SUMMARY "Return a new sequence, containing the same elements, but in reversed order.")
  (:SYNTAX (REVERSE string)
	   (NREVERSE string))
  (:PARAMETERS
    (string :VALUE-TYPE string :DOCSTRING "the series of characters you would like to reverse the order of"))
  (:RETURNS "A list")
  (:EXAMPLES
    "(REVERSE \"ABC\") --> \"CBA\""
    "(NREVERSE \"ABC\") --> \"CBA\"")
  (:TEXT
    (:center (:img :src "http://ramsites.net/~biobike/temp/REVERSE.JPG"))
    (:center (:img :src "http://ramsites.net/~biobike/temp/NREVERSE.JPG"))
    (:p "error message of \"type-error\" would be sent if the string enter is not a proper string type.")
    (:p "nreverse might either create a new sequence or modify the argument sequence, or both, while reverse does not modify sequence."))
  (:SEE-ALSO ))

;; ================================== REPEAT ==========================================

(DOCUMENT-FUNCTION REPEAT
  (:SUMMARY "Repeats a given string a specified number of times")
  (:SYNTAX (REPEAT string [TIMES] number))
  (:PARAMETERS
    (string :DOCSTRING "the sourse you would like to modify")
    (number :DOCSTRING "numbers of repeats"))
  (:RETURNS " ")
  (:EXAMPLES
    "(REPEAT \"ABCDE\" TIMES 3) --> \"ABCDEABCDEABCDE\"")
  (:TEXT
    (:center (:img :src "http://ramsites.net/~biobike/temp/REPEAT.JPG")))
  (:SEE-ALSO))

;; =============================== TRANSLITERATE ======================================

(DOCUMENT-FUNCTION TRANSLITERATE
  (:SUMMARY "Replace one set of characters with another.")
  (:SYNTAX (TRANSLITERATE string [FROM] from-characters [TO] to-characters))
  (:PARAMETERS
    (string :DOCSTRING "the sourse you would like to modify")
    (from-characters :DOCSTRING "characters you would like to replace")
    (to-characters :DOCSTRING "new characters that are used to replace the old ones"))
  (:RETURNS " ")
  (:EXAMPLES
    "Replace a character with another
            (TRANSLITERATE \"AAABBBCCBB\" FROM \"B\" TO \"V\") --> \"AAAVVVCCVV\""
    "Replace spaces with symbols
            (TRANSLITERATE \"AAA  BBCC  BB\" FROM \" \" TO \"$\") --> \"AAA$$BBCC$$BB\""
    "Replace a upper case letter with an lower case one or vice-versa.
            (TRANSLITERATE \"aaaAAAaaa\" FROM \"A\" TO \"a\") --> \"aaaaaaaaa\"
            (TRANSLITERATE \"aaaAAAaaa\" FROM \"a\" TO \"A\") --> \"AAAAAAAAA\""
    "Replace a set of letters with another set(the number of characters inside the two set much be equal)
    The first character from the first set would be replaced by the first character of the second set, 
    the second character from the first set would be replaced by the second character of the second set, etc.
            (TRANSLITERATE \"ABCDEDCBACA\" FROM \"ABC\" TO \"efg\") --> \"efgDEDgfege\"")
  (:TEXT
    (:center (:img :src "http://ramsites.net/~biobike/temp/TRANSLITERATE.JPG"))
    (:p "This function is used to replace one set of characters in the string with another set"))
  (:SEE-ALSO))

;; =================================== TRIM ===========================================

(DOCUMENT-FUNCTION TRIM
  (:SUMMARY "Removes spaces (or other characters) from left and/or right.")
  (:SYNTAX (TRIM string [LEFT] [RIGHT] [CUTTING null-or-string]))
  (:PARAMETERS
    (string :DOCSTRING "the sourse you would like to trim")
    (left :DOCSTRING "remove space from the left of the string")
    (right :DOCSTRING "remove space from the right of the string")
    (cutting :DOCSTRING "space or characters to be removed from the string"))
  (:RETURNS " ")
  (:EXAMPLES
    "Without any specification, the spaces at both end would be trimmed
    	    (TRIM \" AAA BBC \") --> \"AAA BBC\""
    "If choose LEFT from the option menu, space at the left end would be trimmed.
   	    (TRIM \" AAA BBC \" LEFT) --> \"AAA BBC \""
    "If choose RIGHT from the option menu, space at the right end would be trimmed.
    	    (TRIM \" AAA BBC \" RIGHT) --> \" AAA BBC\""
    "The CUTTING option is used to remove characters other than space from the ends.
    	    (TRIM \"AAA BBC \" CUTTING \"A\") --> \" BBC \""
    "The same string is returned if the character specified in the CUTTING option is not the first character of the string.
    	    (TRIM \" AAA BBC \" CUTTING \"A\") --> \" AAA BBC \" 
		note that \"A\" is not the first character, the space \" \" is.")
  (:TEXT
    (:center (:img :src "http://ramsites.net/~biobike/temp/TRIM.JPG"))
    (:p "This function returns a modified list of characters by trimming the space or specific characters at both ends")
    (:p "If the list you enter does not have any spaces at the beginning or the end of the string, the same list will be returned.")
    (:p "If methods (LEFT/RIGHT/CUTTING) are not specified, only the spaces at the beginning and the end of the string will be cut out.")
    (:p "If there is no space at the beginning or the end, the same string of characters will be returned.")
    (:p "The CUTTING method can be used to cut out only starting or ending characters from the list."))
  (:SEE-ALSO ))
