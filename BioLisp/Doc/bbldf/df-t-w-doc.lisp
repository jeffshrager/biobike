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



;;====================t-test==========================

(DOCUMENT-FUNCTION T-TEST
(:PARAMETERS
(list1 :DOCSTRING "data set 1")
(list2 :DOCSTRING "data set 2")
(from-means :DOCSTRING "When specified, a t-test is performed using given means and standard deviations")
(paired :DOCSTRING "When specified, a paired t-test is performed"))
(:EXAMPLES
"1. If given two sets of data and no flags

(DEFINE list1 AS {1 2 3 4 5})
(DEFINE list2 AS {6 7 8 9 10})
(T-TEST list1 list2)
--> -5.0

(DEFINE ss120-lengths AS (LENGTHS-OF (GENES-OF ss120)))
(DEFINE A7120-lengths AS (LENGTHS-OF (GENES-OF A7120)))
(T-TEST ss120-lengths A7120-lengths)
--> -7.074859"

"2. If given two sets of data and the PAIRED flag
(DEFINE list1 AS {1 2 3 4 5})
(DEFINE list2 AS {6 7 8 9 10})
(T-TEST list1 list2 PAIRED)
--> 3.2e+38"

"3. If given two sets of statistics and the FROM-MEANS flag
(DEFINE number-of-ss120-genes AS (COUNT-OF (GENES-OF ss120)))
--> 1928
(DEFINE mean-length-ss120 AS (MEAN (LENGTHS-OF (GENES-OF ss120))))
--> 811.27594
(DEFINE SD-length-ss120 AS (STD-DEV (LENGTHS-OF (GENES-OF ss120))))
--> 609.972
(DEFINE number-of-A7120-genes AS (COUNT-OF (GENES-OF A7120)))
--> 6218
(DEFINE mean-length-A7120 AS (MEAN (LENGTHS-OF (GENES-OF A7120))))
--> 957.1904
(DEFINE SD-length-A7120 AS (STD-DEV (LENGTHS-OF (GENES-OF A7120))))
--> 839.4694
(T-TEST {number-of-ss120-genes mean-length-ss120 SD-length-ss120}
{number-of-A7120-genes mean-length-A7120 SD-length-A7120}
FROM-MEANS)
--> -7.074859

Note that while this example is needlessly complicated (see Example 1)
there may well be instances where the n, mean, and SD of two data sets
are known but the raw data is not.")

(:RETURNS "A number")
(:TEXT
(:p (:ul "T-tests are useful in assessing whether two sets of data may represent two samples of the same underlying population. The t-score produced by the t-test can be used to find a probability that random samplings of a single population of numbers might have produced two data sets with means as divergent as observed in the given sets."))
(:p (:ul "Paired t-tests are useful when the each element in a set stands in a special relationship with an element at the corresponding position of the other set. For example, when assessing the efficacy of a diet, a paired t-test will compare the starting weights and ending weights of each person. By retaining the relationship between pairs of elements, a paired t-test might find a significant difference where an unpaired t-test would not. The test may also be used in assessing microarray data, where in each replicate an experimental condition has a specific corresponding control condition."))
(:p (:ul "If no keyword is supplied, then the two lists may be of different lengths. The values in the lists must be numbers, the two sets of data from the t-score is calculated. The result returned by T-TEST is the t-score, not a p value. To find the p value, it is necessary to use a table of p values, armed with the t-score and the degrees of freedom (the sum of the lengths of the lists minus 2)"))
(:p (:ul "If the PAIRED flag is used, then the user must give two lists consisting of the same number of elements, the two sets of data from which the paired t-score is calculated."))
(:p (:ul "If the FROM-MEANS flag is used, then the user must give two lists consisting of precisely three elements: {n mean SD}, where n is the number of elements in the set and mean and SD are the set's mean and standard deviation.")))
(:SEE-ALSO MEAN STD-DEV)
)

(DOCUMENT-FUNCTION TAN
(:SUMMARY "Returns the tangent (tan)")
(:SYNTAX (TAN n))
(:PARAMETERS
(n :VALUE-TYPE number :DOCSTRING "the angle, in radians, you want the tangent of")
)
(:RETURNS " a number")
(:EXAMPLES
"(TAN 0)
--> 0.0"
"(TAN PI)
--> -1.2246467991473532d-16
(Note: \"d\" indicates exponent. See Representations of Numbers for more explanation."
"(TAN (/ PI 4))
--> 0.9999999999999999d0"
"(TAN (/ (* 5 PI) 3))
--> -1.732050807568877d0"
"(TAN .5235988)
--> 0.57735026"
)

(:TEXT
(:p "This function returns the tangent of the angle provided in radians.")
(:p "The angle provided is usually between 0 and 2PI. (2PI = 6.28319)")
(:p "TAN(x) = SIN(x)/COS(x)")
)
(:SEE-ALSO SIN COS)
)
;;================================= TENTH ==============================
(DOCUMENT-FUNCTION BBL::TENTH
(:SUMMARY "Returns the tenth item of a list or tenth character of a string.")
(:PARAMETERS
(entity :DOCSTRING "the list or string in which you are seeking the element")
(in :DOCSTRING "will take the tenth element of a string or a list, ignoring the internal structure of the list")
(in-each :DOCSTRING "when used on a list of strings, the tenth element of each string is returned; when used on a list of lists, the tenth element of each list is returned; (if a list contains strings and lists, the tenth element of each is returned)")
(nonstrict :DOCSTRING "if a list does not have at least 10 items, NIL will be returned")
(strict :DOCSTRING "(default) will return an error if this is used and there are not at least 10 items in each list")
)
(:RETURNS "An object of any type")
(:EXAMPLES
"(TENTH IN {5 8 4 6 11 17 2 3 8 21 6})
    --> 21"
"(TENTH IN \"ACCTGTGAACGG\")
    --> \"C\""
"(TENTH IN-EACH (REPLICONS-OF *all-organisms*) STRICT)
    --> ***ERROR***
    (This causes an error because some organisms don't have at least ten replicons so there
    is not a tenth element in the list to return.)

    (TENTH IN-EACH (REPLICONS-OF *all-organisms*) NONSTRICT)
    --> (NIL NIL NIL NIL NIL #$Tery.Contig66 NIL NIL NIL NIL NIL
    #$Cwat.Contig054 #$A29413.Contig200)
    (When NONSTRICT is used, the organisms that don't have at least ten replicons return NIL.)"
)
(:TEXT 
(:p "This function returns the tenth item of a list or the tenth character of a string.")
(:p "If IN-EACH is used, the tenth item of each list is returned.")
(:p "Use NONSTRICT to return NIL when there is not a tenth item in the list(s) that is being searched.")
)
(:SEE-ALSO BBL:FIRST BBL:SECOND BBL:THIRD BBL:FOURTH BBL:FIFTH BBL:SIXTH BBL:SEVENTH BBL:EIGHTH BBL:NINTH)
)

;;================================= THIRD ==============================
(DOCUMENT-FUNCTION BBL::THIRD
(:SUMMARY "Returns the third item of a list or third character of a string.")
(:PARAMETERS
(entity :DOCSTRING "the list or string in which you are seeking the element")
(in :DOCSTRING "will take the third element of a string or a list, ignoring the internal structure of the list")
(in-each :DOCSTRING "when used on a list of strings, the third element of each string is returned; when used on a list of lists, the third element of each list is returned; (if a list contains strings and lists, the third element of each is returned)")
(nonstrict :DOCSTRING "if a list does not have at least 3 items, NIL will be returned")
(strict :DOCSTRING "(default) will return an error if this is used and there are not at least 3 items in each list")
)
(:RETURNS "An object of any type")
(:EXAMPLES
"(THIRD IN {5 8 4 6 11 17 2 3 8 21 6})
    --> 4"
"(THIRD IN \"ACCTGTGAACGG\")
    \"C\""

"(THIRD IN-EACH (REPLICONS-OF *all-organisms*) STRICT)
    --> ***ERROR***
    (This causes an error because some organisms don't have at least three replicons so there
    is not a third element in the list to return.)

    (THIRD IN-EACH (REPLICONS-OF *all-organisms*) NONSTRICT)
    --> (#$S7942.pANS NIL #$S6803.pSYSG #$Npun.pNpB #$A7120.pBETA
    #$Tery.Contig48 NIL NIL NIL NIL NIL #$Cwat.Contig043
    #$A29413.Contig193)
    (When NONSTRICT is used, the organisms that don't have at least three replicons return NIL.)"

)
(:TEXT 
(:p "This function returns the third item of a list or the third character of a string.")
(:p "If IN-EACH is used, the third item of each list is returned.")
(:p "Use NONSTRICT to return NIL when there is not a third item in the list(s) that is being searched.")
)
(:SEE-ALSO BBL:FIRST BBL:SECOND BBL:FOURTH BBL:FIFTH BBL:SIXTH BBL:SEVENTH BBL:EIGHTH BBL:NINTH BBL:TENTH)
)

; ================= TRANSLATION-OF ====================



(DOCUMENT-FUNCTION TRANSLATION-OF
(:PARAMETERS 
      (entity :docstring "DNA sequence to be translate, see the value type column for details")
      (if-bad-codon :docstring "To replace the default character \"-\" by another when a codon is not recognize as amino-acid")
      (noncoding :docstring "To translate sequence of gene that does not encode protein")
      (nowarnings :docstring "To avoid warnings")
      
)
(:EXAMPLES 
"1. If given a gene or a string
      (TRANSLATION-OF  NpR0388)
      --> \"MKSTQGKINELLTETGCEHNQHKQAEKKNKSCAQQAQPGAAQGGCAFDGAMIALVPIADAAHLVHGPIACAGNSWGSRGSLSSGPQLYKMGFTTDLGENDVIFGGEKKLYKAILELKERY...
      ;;    >>> Line truncated to 200 (was 460). Use SET-OUTPUT-LIMITS to adjust width\"
  or  (SEQUENCE-OF  p-NpR0388)
      --> \"MKSTQGKINELLTETGCEHNQHKQAEKKNKSCAQQAQPGAAQGGCAFDGAMIALVPIADAAHLVHGPIACAGNSWGSRGSLSSGPQLYKMGFTTDLGENDVIFGGEKKLYKAILELKERY...
      ;; >>> Line truncated to 200 (was 461). Use SET-OUTPUT-LIMITS to adjust width\"

      (TRANSLATION-OF \"ATGCGTCGGGTCCCCGTCGCTCGGGAAATGATAGTACGATAGATGATAGATCCCGCTCGATCGATCGCAAACT\")
      -->\"MRRVPVAREMIVR*MIDPARSIAN\"

     (TRANSLATION-OF (SEQUENCE-OF A7120.CHROMOSOME FROM 434550 TO 434662))
     --> \"GFTLFLPPFP*VRLQRRRTLT*PGLYQFTIERH*K*R\""


"2. If given a list of genes or strings
      (TRANSLATION-OF (GENES-DESCRIBED-BY \"cpcA\" NOWARNINGS))
      --> ((\"MSKTPLTEAVAAADSQGRFLSSTELQVAFGRFRQAASGLAAAKALANNADSLVNGAANAVYSKFPYTTSTPGNNFASTPEGKAKCARDIGYYLRIVTYALVAGGTGPIDEYLLAGLDEINKTFDLAPSWYVEALKYIKANHGLSGDSRDEANSYIDYLINALS\"
       \"MSKTPLTEAVAAADSQGRFLSSTELQVAFGRFRQAASGLAAAKALANNADSLVNGAANAVYSKFPYTTSTPGNNFASTPEGKAKCARDIGYYLRIVTYALVAGGTGPIDEYLLAGLDEINKTFDLAPSWYVEALKYIKANHGLSGDSRDEANSYIDYLINALS\")

      (\"MKTPLTEAVSTADSQGRFLSSTELQIAFGRLRQANAGLQAAKALTDNAQSLVNGAAQAVYNKFPYTTQTQGNNFAADQRGKDKCARDIGYYLRIVTYCLVAGGTGPLDEYLIAGIDEINRTFDLSPSWYVEALKYIKANHGLSGDARDEANSYLDYAINALS\")
      (\"MVKTPITEAIAAADTQGRFLGNTELQSARGRYERAAASLEAARGLTSNAQRLIDGATQAVYQKFPYTTQTPGPQFAADSRGKSKCARDVGHYLRIITYSLVAGGTGPLDEYLIAGLAEINSTFDLSPSWYVEALKHIKANHGLSGQAANEANTYIDYAINALS\")
      (\"MKTVITEVIASADSQGRFLNNTELQAANGRFQRATASMEAARALTSNADSLVKGAVQEVYNKFPYLTQPGQMGYGDTNQAKCARDISHYLRFITYSLVAGGTGPLDDYIVAGLREVNRTFNLSPSWYIEALKHIKGKVGSQLSGQPLTEANAYIDYCINALS\"
       \"MKTVITEVIASADSQGRFLNNTELQAANGRFQRATASMEAARALTSNADSLVKGAVQEVYNKFPYLTQPGQMGYGDTNQAKCARDISHYLRFITYSLVAGGTGPLDDYIVAGLREVNRTFNLSPSWYIEALKHIKGKVGSQLSGQPLTEANAYIDYCINALS\")
      (\"MKTPITEAIAAADTQGRFLSNTELQAVDGRFKRAVASMEAARALTNNAQSLIDGAAQAVYQKFPYTTTMQGSQYASTPEGKAKCARDIGYYLRMVTYCLVAGGTGPMDEYLIAGLSEINSTFDLSPSWYIEALKYIKANHGLTGQAAVEANAYIDYAINALS\"))"



"3. Illustrate NOWARNINGS, IF-BAD-CODON and NONCODING 
      (TRANSLATION-OF \"ATGCGTCGGGTCCCCzGTCGCTCGGGAAATGATAGTACGATAGATGATAGATCCCGCTCGATCGATCGCAAACT\")
      :: Warning: Unknown codon \"zGT\", position 15
      --> \"MRRVP-RSGNDSTIDDRSRSIDRK\"

      (TRANSLATION-OF \"ATGCGTCGGGTCCCCzGTCGCTCGGGAAATGATAGTACGATAGATGATAGATCCCGCTCGATCGATCGCAAACT\")
      --> \"MRRVP-RSGNDSTIDDRSRSIDRK\"


      (TRANSLATION-OF \"ATGCGTCGGGTCCCCzGTCGCTCGGGAAATGATAGTACGATAGATGATAGATCCCGCTCGATCGATCGCAAACT\" IF-BAD-CODON \"#\")
      --> \"MRRVP#RSGNDSTIDDRSRSIDRK\"

      (TRANSLATION-OF rrn5s)
      :: Warning: 
      You are trying to translate noncoding gene #$PRO1375.rrn5S!
      If you want this gene translated, use the NONCODING option.
      If you do not want this warning to appear use the NOWARNINGS option
      --> NIL

      (TRANSLATION-OF rrn5s NOWARNINGS)
      --> NIL

      (TRANSLATION-OF rrn5s NONCODING)
      --> \"SWCSSRCGPTPIHPELGCETHQRRRYLGGSPLRK*LNA\""

)
                  
(:TEXT (:p "Translates DNA sequence into amino-acid sequence.")
(:ul  
  (:li "Sequence may be provided directly or as a gene name")
  (:li "If a nucleotide besides G, A, T, or C, is encountered, then a hyphen is inserted in the position of the current amino acid" (:br "and a warning
   issued, unless the flag IF-BAD-CODON is specified with a value, for example: \"#\"."))
  (:li "If given a non-coding gene the function returns NIL and a warning issued, unless the flag NONCODING is specified.")
  (:li "If given a sequence with a non-nucleotide character or a non-coding gene and if the flags IF-BAD-CODON or NONCODING" (:br "are not specified, a warning issued unless specified NOWARNINGS."))))
  (:SEE-ALSO TRANSLATE-D/RNA-TO-AA SEQUENCE-OF PROTEIN-OF))


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




;;================================= TRANSPOSE-LIST ==============================

(DOCUMENT-FUNCTION TRANSPOSE-LIST
(:PARAMETERS 
      (list :docstring "List with sublists, in which the elements must be transposed")
)
(:EXAMPLES 
"1. How to use this function?
      (TRANSPOSE-LIST {{\"a\" \"b\" \"c\"} {1 2 3}})
      --> ((\"a\" 1) (\"b\" 2) (\"c\" 3))
      (TRANSPOSE-LIST *)
      --> ((\"a\" \"b\" \"c\") (1 2 3))

      (TRANSPOSE-LIST {{\"a\" \"b\" \"c\" \"d\"} {1 2}})
      --> ((\"a\" 1) (\"b\" 2) (\"c\" NIL) (\"d\" NIL))

      (TRANSPOSE-LIST {{asr9501 asl9502 asr9503 alr9504 alr9505} {1 2 3 4 5 }})
      --> ((#$A7120.asr9501 1) (#$A7120.asl9502 2) (#$A7120.asr9503 3)
      (#$A7120.alr9504 4) (#$A7120.alr9505 5))

"
"2. Illustrate the result of TRANSPOSE-LIST, when the list is convert in a table
      (PROGN 
      (DEFINE test-table AS (NEW-TABLE {1 1}))
      (FOR-EACH sublist IN {{\"a\" \"b\" \"c\"} {1 2 3} {asr9501 asl9502 asr9503}}
           FOR row FROM 0
           (FOR-EACH element IN sublist
                FOR col FROM 0
                (ASSIGN test-table[col row] = element)))
      (DISPLAY-TABLE test-table))
      :: 
                    0             1             2             
      0             a             b             c             
      1             1             2             3             
      2             A7120.asr9501 A7120.asl9502 A7120.asr9503 
      --> NIL

      (PROGN 
      (DEFINE test-table AS (NEW-TABLE {1 1}))
      (FOR-EACH sublist IN (TRANSPOSE-LIST {{\"a\" \"b\" \"c\"} {1 2 3} {asr9501 asl9502 asr9503}})
           FOR row FROM 0
           (FOR-EACH element IN sublist
                FOR col FROM 0
                (ASSIGN test-table[col row] = element)))
      (DISPLAY-TABLE test-table))
      :: 
                    0             1             2             
      0             a             1             A7120.asr9501 
      1             b             2             A7120.asl9502 
      2             c             3             A7120.asr9503 
     --> NIL

"
)
                  
(:TEXT (:p "The function transposes the elements of sublists within a list.")
(:ul  
  (:li "The function requires a list that comprises at least one sublist.")
  (:li "If given only one sublist, each element of the sublist will constitute a new sublist.")
  (:li "If the sublists do not include the same number of elements, NIL is returned for the missing elements.")
))
  (:SEE-ALSO LIST NEW-TABLE))
  
  
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
  
;=========================== TRUE? ===================================================

(DOCUMENT-FUNCTION TRUE?
  (:SUMMARY "Compares two values in one of a variety of ways")
  (:VPL-SYNTAX (:FOO (:img :src "/weblistenerdocs/bbldf/images/TRUE-syntax.PNG"
                      :HEIGHT "200")))
  (:PARAMETERS
    (each/the-entity :PARAMETER-TYPE flag 
       :DOCSTRING "Determines whether the associated argument (if a list) is to be treated EACH element, one at a time, or instead as a single ENTITY, as a whole. The default depends on the test, as discussed below.")
    (value1 :VALUE-TYPE any :DOCSTRING "First value to be compared")
    (is/is-not :PARAMETER-TYPE flag 
       :DOCSTRING "Determines whether the result is to be determined directly by the associated test (IS) or is to be determined by the negation of the test (IS-NOT). The default is IS.")
    (test :VALUE-TYPE symbol :DOCSTRING "Operation used to compare values")
    (value2 :VALUE-TYPE any :PARAMETER-TYPE optional 
            :DOCSTRING "Second value to be compared")
    (case-sensitive :PARAMETER-TYPE flag :DOCSTRING 
       "Specifies that comparison is to be performed without regard to capitalization")
    (inclusive :PARAMETER-TYPE flag :DOCSTRING 
       "When the IS-BETWEEN test is used, a value that is equal to one of the boundary values will be considered to pass the test. The flag is ignored with any other test.")
    (T-if-all-T :PARAMETER-TYPE flag :DOCSTRING 
       "Specifies that when either value1 or value2 is a list, the function returns T if all the comparisons are T and returns NIL if at least one comparison is NIL")
    (T-if-any-T :PARAMETER-TYPE flag :DOCSTRING 
       "Specifies that when either value1 or value2 is a list, the function returns T if at least one comparisons is T and returns NIL if all comparison are NIL")
    (count :PARAMETER-TYPE flag :DOCSTRING 
       "Specifies that when either value1 or value2 is a list, the function returns a count of the number of instances that the comparisons are T")
    (by-position :VALUE-TYPE Number :PARAMETER-TYPE keyword :DOCSTRING 
       "Specifies that if values are lists, then the comparison is to be performed using the element of the list with the given index")
  )
  (:RETURNS "T or NIL or list of T/NIL values")
  (:EXAMPLES
    (:FOO
    (:OL
      (:LI (:B "Simple numeric comparison") (:BR)
        (:img :src "/weblistenerdocs/bbldf/images/TRUE-length-of-gt.png" :HEIGHT "70") (:BR)
        (:I (:U "Translation") ": Is the length of the organism's genome greater than 5,000,000 nucleotides? If so then the function returns T. If not, it returns NIL. GREATER-THAN could also have been used in place of '>'. "
        "For example, if ") 
        (:TT "organism")
        (:I " has the value ")
        (:TT "A7120")
        (:I " then the function returns ")
        (:TT "T")
        (:I ".") 
        (:BR) (help::HTML "&nbsp"))
                                                          
      (:LI (:B "Simple alphanumeric comparison") (:BR) 
         (:img :src "/weblistenerdocs/bbldf/images/TRUE-gt.png" :HEIGHT "50") (:BR)
         (:I (:U "Translation") ": Two sequences are compared with each other. If the first is greater (comes first in alphabetical order), then the function returns T. If not, it returns NIL. This might be useful in sorting sequences "
         "(although the ") 
         ((:A :HREF (:PRINT (help::MAKE-HELP-FUNCTION-DOCUMENTATION-URL :NAME "bbl:SORT")))
                "SORT")
         (:I " function would be even more useful).")
         (:BR) (help::HTML "&nbsp"))

      (:LI (:B "Comparing a number to a range of numbers") (:BR) 
        (:img :src "/weblistenerdocs/bbldf/images/TRUE-is-between-inclusive.png" :HEIGHT "54") (:BR)
         (:I (:U "Translation") ": If the ")
         ((:A :HREF (:PRINT (help::MAKE-HELP-FUNCTION-DOCUMENTATION-URL :NAME "LENGTH-OF" :PACKAGE :bbl)))
                "LENGTH-OF")
         (:I " the ")
         ((:A :HREF (:PRINT (help::MAKE-HELP-FUNCTION-DOCUMENTATION-URL :NAME "SEQUENCE-UPSTREAM-OF" :PACKAGE :bbl)))
                "SEQUENCE-UPSTREAM-OF")
         (:I " the given gene lies between 50 and 300 nucleotides (including 50 and 300), "
             "then the function returns T. If not, it returns NIL. "
         "For example, if ") 
         (:TT "gene")
         (:I " has the value ")
         (:TT "all4312")
         (:I " then the function returns ")
         (:TT "NIL")
         (:I " (because the upstream sequence is 537 nt). Without the INCLUSIVE flag, the length would "
             "need to be " (:U "greater than") " 50 and " (:U "less than") " 300.")
         (:BR) (help::HTML "&nbsp"))

      (:LI (:B "Determining the (lack of) identity of two entities") (:BR) 
        (:img :src "/weblistenerdocs/bbldf/images/TRUE-is-not.PNG" :HEIGHT "50") (:BR)
         (:I (:U "Translation") ": If the organism is not Anabaena PCC 7120 (nicknamed A7120), "
           "then the function returns T. If it is, then it returns NIL. If identity is desired, "
           "then IS would be used in place of IS-NOT. In both cases, specifying IS or IS-NOT "
           "implies that the test is SAME-AS unless otherwise specified.")
          (:BR) (help::HTML "&nbsp"))

      (:LI (:B "Determining whether an entity (is | is not) found in a list") (:BR) 
        (:img :src "/weblistenerdocs/bbldf/images/TRUE-is-not-subset-of.png" :HEIGHT "50") (:BR)
         (:I (:U "Translation") ": If the organism is not found within the premade list of marine "
            "cyanobacteria, then the function returns T. If it is part of the list, the function returns NIL. "
            "Of course you would use IS A-SUBSET-OF (rather than IS-NOT) if you wanted to test for membership "
            "in the list.")
          (:BR) (help::HTML "&nbsp"))

      (:LI (:B "Determining whether a string (is | is not) identical to part of another string") (:BR) 
        (:img :src "/weblistenerdocs/bbldf/images/TRUE-contained-in-string.png" :HEIGHT "54") (:BR)
         (:I (:U "Translation") ": If at least one instance of 'N' (representing an unknown "
             "nucleotide) is found in the ")
         ((:A :HREF (:PRINT (help::MAKE-HELP-FUNCTION-DOCUMENTATION-URL :NAME "SEQUENCE-OF" :PACKAGE :bbl)))
                "SEQUENCE-OF")
         (:I " the gene, then the function returns T. If there are no instances of 'N', then it "
              "returns NIL. For example, the function will return NIL for most genes but T for Cwat-8501-4008 "
              "(in PhAnToMe) and for Cw?0829 (in CyanoBIKE). IS-NOT CONTAINED-IN would be used as the test "
              "if you wanted T returned when the sequence didn't have any instances of 'N'.")
          (:BR) (help::HTML "&nbsp"))

      (:LI (:B "Determining whether part of a string matches a given pattern") (:BR) 
        (:img :src "/weblistenerdocs/bbldf/images/TRUE-matches-pattern.PNG" :HEIGHT "50") (:BR)
         (:I (:U "Translation") ": If the protein contains the FeS-binding motif, C**C**C***C, "
               "then the function returns T, otherwise NIL. For example, the function will return "
               "T for the protein p-Slr2059. DOES-NOT-MATCH-PATTERN reverses T and NIL in the results.")
          (:BR) (help::HTML "&nbsp"))

      (:LI (:B "Comparing a list of values to a single value") (:BR) 
         (:img :src "/weblistenerdocs/bbldf/images/TRUE-list-vs-value.png" :HEIGHT "50") (:BR)
         " "(help::HTML "&nbsp")" "(help::HTML "&nbsp")" "(help::HTML "&nbsp") 
         (:img :src "/weblistenerdocs/img-bbldoc/zrightarrow.gif")
         (:TT " (NIL NIL T T NIL T T NIL T NIL NIL T T T T T T NIL T T)") (:BR)(:BR)
         (:I (:U "Translation") ": The ")
         ((:A :HREF (:PRINT (help::MAKE-HELP-FUNCTION-DOCUMENTATION-URL :NAME "HYDROPHOBICITY-OF" :PACKAGE :bbl)))
                "HYCROPHOBICITY-OF")
         (:I " each amino acid is compared to 0. "
             "If the hydrophobicity is less than 0 (i.e. the amino acid is hydrophilic), then T is returned. "
             "Otherwise NIL is returned. The list of 20 amino acids will result in a list of 20 values, "
             "all T or NIL. Their order corresponds to the order of amino acids.")
          (:BR) (help::HTML "&nbsp"))

      (:LI (:B "Counting the number of times a condition is met") (:BR) 
         (:img :src "/weblistenerdocs/bbldf/images/TRUE-count.png" :HEIGHT "50") (:BR)
         (:I (:U "Translation") ": The ")
         ((:A :HREF (:PRINT (help::MAKE-HELP-FUNCTION-DOCUMENTATION-URL :NAME "LENGTH-OF" :PACKAGE :bbl)))
                "LENGTH-OF")
         (:I " each genome known to the specific instance of BioBIKE is compared against 2 million. "
             "The function returns the number of instances in which the genome is less than that value.")
          (:BR) (help::HTML "&nbsp"))

      (:LI (:B "Comparing a list of values to obtain a single T/NIL value") (:BR) 
         (:img :src "/weblistenerdocs/bbldf/images/TRUE-combine-results.png" :HEIGHT "50") (:BR)
         (:I (:U "Translation") ": The ")
         ((:A :HREF (:PRINT (help::MAKE-HELP-FUNCTION-DOCUMENTATION-URL :NAME "LENGTH-OF" :PACKAGE :bbl)))
                 "LENGTH-OF")
         (:I " each gene of the cyanobacterium Anabaena PCC 7120 "
             "nicknamed A7120) is examined to see if it is a multiple of three "
             "((")
         ((:A :HREF (:PRINT (help::MAKE-HELP-FUNCTION-DOCUMENTATION-URL :NAME "MOD" :PACKAGE :bbl)))
                "MOD")
         (:I " multiple-of-3 3) = 0). The COMBINE-RESULTS flag specifies that T is returned so long as "
             "all genes meet this condition.")
         (:BR) (help::HTML "&nbsp"))

      (:LI (:B "Determining whether a condition you provide is met for a list of entities") (:BR) 
        (:img :src "/weblistenerdocs/bbldf/images/TRUE-run-through.PNG") (:BR)
         (:I (:U "Translation") ": If the gene begins with a start codon, as determined by the function, ")
         (:TT "has-start-codon")
         (:I ", that you defined yourself, then the function will return T, otherwise NIL. "
             "The single quote is essential if ")
         (:TT "has-start-codon")
         (:I " is meant to indicate the name of the test function. Without the quote, it would be "
             "interpreted as a variable whose value is the name of the test function.")
         (:BR) (help::HTML "&nbsp"))
  )))

  (:TEXT
   
    (:P 
      (:TABLE
        (:TR 
          (:TD :VALIGN "top"
            ((:FONT :FACE "Verdana" :SIZE 2)
              #.(ONE-STRING-SP 
                 "Every computer language has a collection of comparison operations."
                 "BioBIKE attempts to gather together all of your comparison needs into"
                 "a single function: TRUE? (True)."
                 "The upside is that the language is simplified -- many functions collapsed into one."
                 "The downside is that you need to mouse over the ")
                (:B "test")
                " icon to specify which flavor of comparison you want, from the list shown on the right."
                
              (:P "Most will use the basic capabilities of TRUE? as incorporated into the "
                  ((:A :HREF (:PRINT (help::MAKE-HELP-FUNCTION-DOCUMENTATION-URL :NAME "IF-TRUE" :PACKAGE :bbl)))
                      "IF-TRUE")
                  " or "
                  ((:A :HREF (:PRINT (help::MAKE-HELP-FUNCTION-DOCUMENTATION-URL :NAME "FILTER" :PACKAGE :bbl)))
                      "FILTER")                  
                  " functions. However, the full functionality of TRUE? may also be useful on its own." 
                  )
 
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
            (:img :src "/weblistenerdocs/bbldf/images/TRUE-test-menu-labeled.jpg" :height "360"))
        )))
    (:P (:B "Comparisons across lists") (:BR)
        "The operations described above to compare one entity against another (or one entity "
        "against a property) may be applied to all of the entities within a list. Example 8 "
        "illustrates how a each number within a list of numbers may be compared to a single number. In a like fashion, "
        "a number may be compared to each number within a list of numbers or a each element of a list "
        "compared to each element of another list.")
     
    (:P "In all these cases, the result is a list of T and NIL "
        "values, with their number being equal to the length of the list or lists. "
        "A comparison of two lists implies a comparison of the elements of the list, in order. "
        "For example, IS (1 2 3) < (2 2 2) produces a result of three values, (T NIL NIL), because "
        "1 < 2, but neither 2 nor 3 is < 2. It follows that the two lists must be of equal lengths.")
        
    (:P (:U "Choice between EACH and THE-ENTITY") (:BR)
        "Whenever an argument to TRUE? is a list, BioBIKE must decide whether to use EACH of "
        "the entities within the list in the comparison or rather to use THE ENTITY itself, i.e. the "
        "list as an independent object. In many cases, only one of these two choices make sense. For "
        "example, the concept of LESS-THAN does not obviously apply to lists. Therefore, BioBIKE "
        "uses EACH of the entities within the list, unless told otherwise. Conversely, A-SUBSET-OF "
        "makes little sense unless the second argument is interpretted as a list as an ENTITY, "
        "not EACH entity within the list.")
        
    (:P "The table below gives the default choices of BioBIKE for each of the tests:")
    (:BLOCKQUOTE
      (:TABLE :border 1 :CELLPADDING 4
        (:TR
           (:TD ((:FONT :FACE "Verdana" :SIZE 2) (:B (:U "Operation"))))
           (:TD ((:FONT :FACE "Verdana" :SIZE 2) (:B (:U "Atom vs List"))))
           (:TD ((:FONT :FACE "Verdana" :SIZE 2) (:B (:U "List vs Atom"))))
           (:TD ((:FONT :FACE "Verdana" :SIZE 2) (:B (:U "List vs List")))))
        (:TR
           (:TD ((:FONT :FACE "Verdana" :SIZE 2) "SAME-AS"))
           (:TD ((:FONT :FACE "Verdana" :SIZE 2) "THE-ENTITY"))
           (:TD ((:FONT :FACE "Verdana" :SIZE 2) "THE-ENTITY"))
           (:TD ((:FONT :FACE "Verdana" :SIZE 2) "THE-ENTITY, THE-ENTITY")))
        (:TR
           (:TD ((:FONT :FACE "Verdana" :SIZE 2) 
                "LESS-THAN" (:BR)
                "GREATER-THAN" (:BR)
                "= < >"))
           (:TD ((:FONT :FACE "Verdana" :SIZE 2) "EACH"))
           (:TD ((:FONT :FACE "Verdana" :SIZE 2) "EACH"))
           (:TD ((:FONT :FACE "Verdana" :SIZE 2) "EACH, EACH")))
        (:TR
           (:TD ((:FONT :FACE "Verdana" :SIZE 2) "BETWEEN"))
           (:TD ((:FONT :FACE "Verdana" :SIZE 2) "THE-ENTITY"))
           (:TD ((:FONT :FACE "Verdana" :SIZE 2) (:I "Error")))
           (:TD ((:FONT :FACE "Verdana" :SIZE 2) "EACH, THE-ENTITY")))
        (:TR
           (:TD ((:FONT :FACE "Verdana" :SIZE 2) "MATCHED-BY-PATTERN"))
           (:TD ((:FONT :FACE "Verdana" :SIZE 2) "EACH"))
           (:TD ((:FONT :FACE "Verdana" :SIZE 2) "EACH"))
           (:TD ((:FONT :FACE "Verdana" :SIZE 2) "EACH, EACH")))
        (:TR
           (:TD ((:FONT :FACE "Verdana" :SIZE 2) "CONTAINED-IN" (:BR)
                "A-SUBSET-OF"))
           (:TD ((:FONT :FACE "Verdana" :SIZE 2) "THE-ENTITY"))
           (:TD ((:FONT :FACE "Verdana" :SIZE 2) "THE-ENTITY"))
           (:TD ((:FONT :FACE "Verdana" :SIZE 2) "THE-ENTITY, THE-ENTITY")))
        (:TR
           (:TD ((:FONT :FACE "Verdana" :SIZE 2) "TRUE-PER"))
           (:TD ((:FONT :FACE "Verdana" :SIZE 2) "EACH"))
           (:TD ((:FONT :FACE "Verdana" :SIZE 2) "EACH"))
           (:TD ((:FONT :FACE "Verdana" :SIZE 2) "EACH, EACH")))
        (:TR
           (:TD ((:FONT :FACE "Verdana" :SIZE 2) "NON-NIL"))
           (:TD ((:FONT :FACE "Verdana" :SIZE 2) (:I "Error")))
           (:TD ((:FONT :FACE "Verdana" :SIZE 2) "THE-ENTITY"))
           (:TD ((:FONT :FACE "Verdana" :SIZE 2) (:I "Error"))))
           
         ))
    (:P "It is always possible to override these default decisions by selecting EACH "
        "or THE-ENTITY from the drop down menu preceding the argument you want to specify.")
     
    (:P (:U "Selecting an element within a list using the BY-POSITION keyword") (:BR)
        "")
        
    (:P (:U "Interpreting a list of T/NIL results as a single T/NIL result or as a COUNT of T results") (:BR)
        "An argument that is a list ordinarily "
        "produces a list of T/NIL results. It is possible, however, to override this behavior in " 
        "three ways:"
        (:UL
          (:LI "If the T-IF-ALL-T flag is selected, then a single "
               "T or NIL result is produced, determined by whether "
               (:B (:I "all")) " of the intermediate results are T." (:BR)
               "Example: (2 3 4) > 1 T-IF-ALL-T "
               (:img :src "/weblistenerdocs/img-bbldoc/zrightarrow.gif")
               " T" (:BR)
               "Example: (1 2 3) > 1 T-IF-ALL-T "
               (:img :src "/weblistenerdocs/img-bbldoc/zrightarrow.gif")
               " NIL" (:BR) 
               (:BR)
               "Example 10 illustrates the use of COMBINE-RESULTS."
               (:BR) (help::HTML "&nbsp"))
          (:LI "If the T-IF-ANY-T flag is selected, then a single "
               "T or NIL result is produced, determined by whether "
               (:B (:I "any")) " of the intermediate results are T." (:BR)
               "Example: (1 2 3) > 1 T-IF-ANY-T "
               (:img :src "/weblistenerdocs/img-bbldoc/zrightarrow.gif")
               " T" (:BR)
               "Example: (1 1 1) > 1 T-IF-ALL-T "
               (:img :src "/weblistenerdocs/img-bbldoc/zrightarrow.gif")
               " NIL" (:BR) (help::HTML "&nbsp"))
          (:LI "If the COUNT flag is selected, then the result is the number of T "
               "values in the list." (:BR)
               "Example: (1 2 3) > 1 COUNT "
               (:img :src "/weblistenerdocs/img-bbldoc/zrightarrow.gif")
               " 2" (:BR) 
               (:BR)
               "Example 9 illustrates the use of COUNT.")
           )
      )
  )    
  (:SEE-ALSO IF-TRUE IF-FALSE FILTER
         (glossary "True and False"))
)

;===========================UNSHARE===================================================

 (document-function unshare
  (:summary "Removes a shared package you have imported from use")
  (:returns "A message that the shared package is no longer being used" 
   :type string)
  (:examples
   "(unshare trna-calculator--> \"Shared package TRNA-CALULATOR no longer in use.\""
   "(unshare \"loop-nitro-fix\") --> \"Shared package LOOP-NITRO-FIX no longer in use.\""
   "(unshare frob) --> A popup window with the message:
     \"You are not currently using a shared package named FROB\"")
  (:text 
   (:p 
    #.(one-string-sp
       "Removes a shared package you are currently using."
       "All the functions and variables that are part of that package are"
       "removed from your FUNCTIONS and VARIABLES menus and are no longer"
       "for you to use in any way."
       )))
  (:parameters 
   (name :docstring "The name of the shared package to remove from use")
   )
  (:see-also share enter)
  )


  


(DOCUMENT-FUNCTION WHEN-VALUE-OF
  (:RETURNS 
   #.(one-string-nl
      "What the evaluation of the last clause associated with the test"
      "that succeeds returns"
      ))
  (:SUMMARY 
   #.(one-string-nl
      "Executes statements conditionally depending on the value"
      "of the first argument"
      ))
  (:PARAMETERS 
   (object :docstring "Value used to test against the various cases" 
           :parameter-type required :value-type any)
   (clauses 
    :docstring 
    "Values to test against and clauses to execute if the test succeeds"
    :parameter-type required :value-type any
    )
   (case-sensitive
    :docstring "Considers the case of the object and the values" 
    :parameter-type flag :value-type boolean))
  (:EXAMPLES
   ((progn 
      (setq x 5)
      (when-value-of x
        is 2 then (setq y 1)
        is 5 or 6 then (setq y 2)
        otherwise (setq y 3)
        )
      y
      )
    2
    )
   ((progn
      (setq x "a")
      (when-value-of x
        is "A" then (setq y 1)
        is "a" then (setq y 3)
        case-sensitive
        )
      y
      )
    3
    ))
  (:TEXT 
   (:p
    "When given a form as the first argument, the form is evaluated to "
    "produce a value V.  This value is compared to the IS and OR elements "
    "of each clause.  If V is the same as one of the elements, then "
    "the action statements of each clause are executed, and the value "
    "of the last statement is returned.")
   (:p
    "The user may specify an OTHERWISE clause; if this clause is "
	
	
    "specified and no elements match V, then the action statements "
    "of the OTHERWISE clause are executed and the value of the last "
    "such statement is returned.  If no OTHERWISE clause is specified "
    "and no elements match V, NIL is returned.")
   (:p
    "If the user specifies case-sensitive, V is compared to the elements "
    "using a case-sensitive test (EQUAL), otherwise a non-case-sensitive "
    "test (EQUALP) is used.")
   )
  (:SEE-ALSO cond case bbl::condition if-true if-false if)
  )
  
  ;;================================= WHO-IS-HERE? ==============================

(DOCUMENT-FUNCTION WHO-IS-HERE?
(:PARAMETERS)
(:VPL-SYNTAX ((:img :src "/weblistenerdocs/snagglepuss.jpg")
              (:img :src "/weblistenerdocs/snagglepuss.jpg")))
(:EXAMPLES 

"
      (WHO-IS-HERE?)
      :: 
      USER NAME        LAST ACTIVITY

      ATATON           5 minutes ago
      System startup at 05/30/06 08:57

      --> NIL
"
)
                  
(:TEXT (:p "Print the User Names logged onto the server, their last activity and when the system started up."))
(:SEE-ALSO USERS #+not-yet MESSAGE-TO ANNOUNCE))
