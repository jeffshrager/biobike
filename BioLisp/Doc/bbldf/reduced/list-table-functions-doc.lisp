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

;;; Author:  JP Massar, Arnaud Taton, Bogdan Mihai, Jeff Elhai.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (import '(
            com.biobike.help:document-module
               com.biobike.help:document-function) :bbi))

(DOCUMENT-MODULE lists-tables
  "Functions to work with lists and tables"
  (:keywords :table :transpose :random)
  (:display-modes :bbl)
  (:submodules list-analysis
               list-extraction
               list-production
               list-table-type-checks)
  (:functions lisp:list bbl::{} bbl::[] 
              Element-of-table Element/s-of-frame) )

;;================================= NEW-TABLE  ==============================


(DOCUMENT-FUNCTION NEW-TABLE 
(:PARAMETERS 
(specs :DOCSTRING "no. of columns and no. of rows")
(not-adjustable :DOCSTRING "adjustable or not")
(initialize :DOCSTRING "the initial values of all the table components")
)
(:EXAMPLES
" 1. (ASSIGN TABLE (NEW-TABLE {3 4} INITIALIZE 2))

	(DISPLAY-TABLE TABLE)

	-->   1   2   3   
          1   2   2   2   
          2   2   2   2   
          3   2   2   2   
          4   2   2   2  "
" 2. (ASSIGN TABLE (NEW-TABLE {3 4} INITIALIZE  (SEQUENCE-OF PCC7120 FROM 1 TO 2))) 

	(DISPLAY-TABLE TABLE)"

	)
(:TEXT
(:p " The function is useful to create matrices ."))
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

;;================================= SHUFFLE ==============================

(DOCUMENT-FUNCTION SHUFFLE
(:PARAMETERS 
      (sequence :docstring "A string or a list")
      (in-place :docstring "To reduce the processing time"))
(:EXAMPLES 
"1. If given a string or a list of atoms 
      (SHUFFLE \"ACDEFGHIKLMNPQRSTVWY\")
      --> \"DVTAHQSKIMRNEFPCGWYL\"
      (SHUFFLE \"ACDEFGHIKLMNPQRSTVWY\")
      --> \"IFECDGASYNLWQKRHPTMV\"

      (SHUFFLE {0 1 2 3 4 5 6 7 8 9})
      --> (8 7 3 6 5 2 4 1 9 0)
      (SHUFFLE {0 1 2 3 4 5 6 7 8 9})
      --> (8 5 1 7 2 3 4 0 6 9)

      (SHUFFLE {\"a\" \"b\" \"c\" \"d\" \"e\" \"f\" \"g\" \"h\" \"i\" \"j\"})
      --> (\"i\" \"g\" \"c\" \"e\" \"d\" \"a\" \"f\" \"b\" \"h\" \"j\")
      (SHUFFLE {\"a\" \"b\" \"c\" \"d\" \"e\" \"f\" \"g\" \"h\" \"i\" \"j\"})
      --> (\"e\" \"h\" \"f\" \"d\" \"b\" \"c\" \"a\" \"j\" \"i\" \"g\")

      (SHUFFLE {Ser0327 PMT0352 Pro1357 SynW1615 tlr0462 PMM1283 NpF1341 Sll1150})
      --> (#$TeBP1.tlr0462 #$Npun.NpF1341 #$PRO1375.Pro1357 #$P9313.PMT0352
      #$PMED4.PMM1283 #$S6803.sll1150 #$S8102.SynW1615 #$S7942.ser0327)
      (SHUFFLE {Ser0327 PMT0352 Pro1357 SynW1615 tlr0462 PMM1283 NpF1341 Sll1150})
      --> (#$S6803.sll1150 #$S8102.SynW1615 #$P9313.PMT0352 #$S7942.ser0327
      #$PMED4.PMM1283 #$PRO1375.Pro1357 #$TeBP1.tlr0462 #$Npun.NpF1341)
"

"2. If given a list of sublists
      (SHUFFLE {{1 2 3} {\"a\" \"b\" \"c\"} {Ser0327 PMT0352 Pro1357}})
      --> ((\"a\" \"b\" \"c\") (1 2 3)
      (#$S7942.ser0327 #$P9313.PMT0352 #$PRO1375.Pro1357))
      (SHUFFLE {{1 2 3} {\"a\" \"b\" \"c\"} {Ser0327 PMT0352 Pro1357}})
      --> ((1 2 3) (#$S7942.ser0327 #$P9313.PMT0352 #$PRO1375.Pro1357)
      (\"a\" \"b\" \"c\"))
"

"3. If given a mixed list of atoms and sublists
      (SHUFFLE {\"ATCG\" 1  \"a\" Ser0327 {1 2 3} {\"a\" \"b\" \"c\"} {Ser0327 PMT0352 Pro1357}})
      --> ((#$S7942.ser0327 #$P9313.PMT0352 #$PRO1375.Pro1357) \"ATCG\" (1 2 3)
      #$S7942.ser0327 \"a\" (\"a\" \"b\" \"c\") 1)
      (SHUFFLE {\"ATCG\" 1  \"a\" Ser0327 {1 2 3} {\"a\" \"b\" \"c\"} {Ser0327 PMT0352 Pro1357}})
      --> (\"ATCG\" 1 (#$S7942.ser0327 #$P9313.PMT0352 #$PRO1375.Pro1357)
      #$S7942.ser0327 \"a\" (1 2 3) (\"a\" \"b\" \"c\"))
"
"4. Illustrate the option IN-PLACE
      (SHUFFLE \"ACDEFGHIKLMNPQRSTVWY\" IN-PLACE)
      --> \"AWGFKLSPEVRMCYHDITNQ\"
      (SHUFFLE \"ACDEFGHIKLMNPQRSTVWY\" IN-PLACE)
      --> \"VIWNPHYKCMELSTGDARFQ\"

      (SHUFFLE {0 1 2 3 4 5 6 7 8 9} IN-PLACE)
      --> (6 7 1 9 4 3 0 2 8 5)
      (SHUFFLE {0 1 2 3 4 5 6 7 8 9} IN-PLACE)
      --> (9 7 3 8 6 4 2 1 0 5)
 "

)
                  
(:TEXT (:p "The function SHUFFLE takes each character of a string or each element of a list and returns them in a random order.")

(:ul  
  (:li "If given a string, SHUFFLE returns a string.")
  (:li "If given a list, SHUFFLE returns a list. The elements within the list may be numbers, strings, genes, proteins, replicons, organisms,...")
  (:li "The use of the flag IN-PLACE does not make any difference for the user in terms of the result that is returned." (:br "However, it will be different in terms of system' s resources used by BioBIKE. Indeed, when using the flag IN-PLACE," (:br "the given data are replaced by the randomized data within the function definition." (:br "This cost less in terms of system' s resources used by BioBIKE. Therefore this flag may be useful when given a huge amount of data since it will reduce the processing time."))))

))
  (:SEE-ALSO REVERSE TRANSPOSE-LIST))


;;================================= FIRST ==============================
(DOCUMENT-FUNCTION BBL::FIRST
(:SUMMARY "Returns the first item of a list or first character of a string.")
(:SYNTAX (FIRST \[number\] [IN] [IN-EACH] entity [NONSTRICT] [STRICT]))
(:PARAMETERS
(entity :PARAMETER-TYPE required :VALUE-TYPE (or list string) :DOCSTRING "the list or string in which you are seeking the element")
(in :PARAMETER-TYPE token :VALUE-TYPE boolean :DOCSTRING "will take the first element of a string or a list, ignoring the internal structure of the list")
(in-each :PARAMETER-TYPE token :VALUE-TYPE boolean :DOCSTRING "when used on a list of strings, the first element of each string is returned; when used on a list of lists, the first element of each list is returned; (if a list contains strings and lists, the first element of each is returned)")
(nonstrict :PARAMETER-TYPE flag :VALUE-TYPE boolean :DOCSTRING "if a list does not have at least 1 item, NIL will be returned")
(strict :PARAMETER-TYPE flag :VALUE-TYPE boolean :DOCSTRING "(default) will return an error if this is used and there is not at least 1 item in each list")
(number :PARAMETER-TYPE number :VALUE-TYPE boolean :DOCSTRING "The number of items of the list or letters of the string to be returned.  Or when working with a list of lists, the number of items of each list or of each string to be returned.")
)
(:RETURNS (a number letter or list))
(:EXAMPLES
"(FIRST IN {5 8 4 6 11 17 2 3 8 21 6})
    --> 5"
"(FIRST IN \"ACCTGTGAACGG\")
    --> \"A\""
"(FIRST IN-EACH (REPLICONS-OF *all-organisms*))
    --> (#$S7942.chromosome #$P9313.chromosome #$S6803.chromosome
    #$Npun.chromosome #$A7120.CHROMOSOME #$Tery.Contig43
    #$PRO1375.CHROMOSOME #$S8102.chromosome #$Gvi.chromosome
    #$TeBP1.chromosome #$PMED4.chromosome #$Cwat.Contig041
    #$A29413.Contig185)"
"(FIRST 3 IN (GENES-OF A7120))
    --> (#$A7120.all0002 #$A7120.asl0003 #$A7120.ssrA)"
"(FIRST 3 IN-EACH (SEQUENCES-OF (GENES-OF A7120)))
    --> (\"ATG\" \"ATG\" \"GGG\" \"ATG\" \"ATG\" \"ATG\" \"ATG\" \"ATG\" \"ATG\" \"ATG\" \"ATG\" \"ATG\" \"ATG\" 
    \"ATG\" \"GTG\" \"ATG\" \"ATG\" \"ATG\" \"GTG\"...)"
)
(:TEXT 
(:p "This function returns the first item of a list or the first character of a string.")
(:p "If IN-EACH is used, the first item of each list is returned.")
(:p "Use NONSTRICT to return NIL when there is not an item in the list(s) that is being searched.")
(:p "This function can also return the first few items of a list or string or a list of lists or a list of strings.  The amount of items returned is determined by the number specified after FIRST.")
)
(:SEE-ALSO BBL:LAST BBL:SECOND BBL:THIRD BBL:FOURTH BBL:FIFTH BBL:SIXTH BBL:SEVENTH BBL:EIGHTH BBL:NINTH BBL:TENTH)
)



;;================================= SECOND ==============================
(DOCUMENT-FUNCTION BBL::SECOND
(:SUMMARY "Returns the second item of a list or second character of a string.")
(:PARAMETERS
(entity :DOCSTRING "the list or string in which you are seeking the element")
(in :DOCSTRING "will take the second element of a string or a list, ignoring the internal structure of the list")
(in-each :DOCSTRING "when used on a list of strings, the second element of each string is returned; when used on a list of lists, the second element of each list is returned; (if a list contains strings and lists, the second element of each is returned)")
(nonstrict :DOCSTRING "if a list does not have at least 2 items, NIL will be returned")
(strict :DOCSTRING "(default) will return an error if this is used and there are not at least 2 items in each list")
)
(:RETURNS (a number letter or list))
(:EXAMPLES
"(SECOND IN {5 8 4 6 11 17 2 3 8 21 6})
    --> 8"
"(SECOND IN \"ACCTGTGAACGG\")
    \"C\""
"(SECOND (GENES-OF A7120))
    --> #$A7120.asl0003"
"(PROTEINS-SIMILAR-TO P-ALL4312 IN S6803)
    --> ((#$S6803.p-Sll1330 2.5d-93) (#$S6803.p-Slr0947 1.0d-32)
    (#$S6803.p-Sll0649 2.7d-31) (#$S6803.p-Slr0115 6.4d-30)
    (#$S6803.p-Slr0081 3.6d-26) (#$S6803.p-Slr1584 1.7d-24)
    (#$S6803.p-Sll0396 6.1d-24) (#$S6803.p-Slr1837 2.0d-20)...)

    (SECOND IN-EACH (PROTEINS-SIMILAR-TO P-ALL4312 IN S6803))
    --> (2.5d-93 1.0d-32 2.7d-31 6.4d-30 3.6d-26 1.7d-24 6.1d-24 2.0d-20...)"
"(SECOND IN-EACH (REPLICONS-OF *all-organisms*) STRICT)
    --> ***ERROR***
    (This causes an error because some organisms don't have at least two replicons so there
    is not a second element in the list to return.)

    (SECOND IN-EACH (REPLICONS-OF *all-organisms*) NONSTRICT)
    --> (#$S7942.pANL NIL #$S6803.pSYSA #$Npun.pNpA #$A7120.pALPHA
    #$Tery.Contig44 NIL NIL NIL NIL NIL #$Cwat.Contig042
    #$A29413.Contig192)
    (When NONSTRICT is used, the organisms that don't have at least two replicons return NIL.)"

)
(:TEXT 
(:p "This function returns the second item of a list or the second character of a string.")
(:p "If IN-EACH is used, the second item of each list is returned.")
(:p "Use NONSTRICT to return NIL when there is not a second item in the list(s) that is being searched.")
)
(:SEE-ALSO BBL:FIRST BBL:THIRD BBL:FOURTH BBL:FIFTH BBL:SIXTH BBL:SEVENTH BBL:EIGHTH BBL:NINTH BBL:TENTH)
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
(:RETURNS (a number letter or list))
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



;;================================= FOURTH ==============================
(DOCUMENT-FUNCTION BBL::FOURTH
(:SUMMARY "Returns the fourth item of a list or fourth character of a string.")
(:PARAMETERS
(entity :DOCSTRING "the list or string in which you are seeking the element")
(in :DOCSTRING "will take the fourth element of a string or a list, ignoring the internal structure of the list")
(in-each :DOCSTRING "when used on a list of strings, the fourth element of each string is returned; when used on a list of lists, the fourth element of each list is returned; (if a list contains strings and lists, the fourth element of each is returned)")
(nonstrict :DOCSTRING "if a list does not have at least 4 items, NIL will be returned")
(strict :DOCSTRING "(default) will return an error if this is used and there are not at least 4 items in each list")
)
(:RETURNS (a number letter or list))
(:EXAMPLES
"(FOURTH IN {5 8 4 6 11 17 2 3 8 21 6})
    --> 6"
"(FOURTH IN \"ACCTGTGAACGG\")
    \"T\""
"(FOURTH IN-EACH (REPLICONS-OF *all-organisms*) STRICT)
    --> ***ERROR***
    (This causes an error because some organisms don't have at least four replicons so there
    is not a fourth element in the list to return.)

    (FOURTH  IN-EACH (REPLICONS-OF *all-organisms*) NONSTRICT)
    --> (NIL NIL #$S6803.pSYSM #$Npun.pNpC #$A7120.pGAMMA #$Tery.Contig51 NIL
     NIL NIL NIL NIL #$Cwat.Contig044 #$A29413.Contig194)
    (When NONSTRICT is used, the organisms that don't have at least four replicons return NIL.)"
)
(:TEXT 
(:p "This function returns the fourth item of a list or the fourth character of a string.")
(:p "If IN-EACH is used, the fourth item of each list is returned.")
(:p "Use NONSTRICT to return NIL when there is not a fourth item in the list(s) that is being searched.")
)
(:SEE-ALSO BBL:FIRST BBL:SECOND BBL:THIRD BBL:FIFTH BBL:SIXTH BBL:SEVENTH BBL:EIGHTH BBL:NINTH BBL:TENTH)
)



;;================================= FIFTH ==============================
(DOCUMENT-FUNCTION BBL::FIFTH
(:SUMMARY "Returns the fifth item of a list or fifth character of a string.")
(:PARAMETERS
(entity :DOCSTRING "the list or string in which you are seeking the element")
(in :DOCSTRING "will take the fifth element of a string or a list, ignoring the internal structure of the list")
(in-each :DOCSTRING "when used on a list of strings, the fifth element of each string is returned; when used on a list of lists, the fifth element of each list is returned; (if a list contains strings and lists, the fifth element of each is returned)")
(nonstrict :DOCSTRING "if a list does not have at least 5 items, NIL will be returned")
(strict :DOCSTRING "(default) will return an error if this is used and there are not at least 5 items in each list")
)
(:RETURNS (a number letter or list))
(:EXAMPLES
"(FIFTH IN {5 8 4 6 11 17 2 3 8 21 6})
    --> 11"
"(FIFTH IN \"ACCTGTGAACGG\")
    --> \"G\""
"(FIFTH IN-EACH (REPLICONS-OF *all-organisms*) STRICT)
    --> ***ERROR***
    (This causes an error because some organisms don't have at least five replicons so there
    is not a fifth element in the list to return.)

    (FIFTH IN-EACH (REPLICONS-OF *all-organisms*) NONSTRICT)
    --> (NIL NIL #$S6803.pSYSX #$Npun.pNpD #$A7120.pDELTA #$Tery.Contig57 NIL
    NIL NIL NIL NIL #$Cwat.Contig045 #$A29413.Contig195)
    (When NONSTRICT is used, the organisms that don't have at least five replicons return NIL.)"
)
(:TEXT 
(:p "This function returns the fifth item of a list or the fifth character of a string.")
(:p "If IN-EACH is used, the fifth item of each list is returned.")
(:p "Use NONSTRICT to return NIL when there is not a fifth item in the list(s) that is being searched.")
)
(:SEE-ALSO BBL:FIRST BBL:SECOND BBL:THIRD BBL:FOURTH BBL:SIXTH BBL:SEVENTH BBL:EIGHTH BBL:NINTH BBL:TENTH)
)



;;================================= SIXTH ==============================
(DOCUMENT-FUNCTION BBL::SIXTH
(:SUMMARY "Returns the sixth item of a list or sixth character of a string.")
(:PARAMETERS
(entity :DOCSTRING "the list or string in which you are seeking the element")
(in :DOCSTRING "will take the sixth element of a string or a list, ignoring the internal structure of the list")
(in-each :DOCSTRING "when used on a list of strings, the sixth element of each string is returned; when used on a list of lists, the sixth element of each list is returned; (if a list contains strings and lists, the sixth element of each is returned)")
(nonstrict :DOCSTRING "if a list does not have at least 6 items, NIL will be returned")
(strict :DOCSTRING "(default) will return an error if this is used and there are not at least 6 items in each list")
)
(:RETURNS (a number letter or list))
(:EXAMPLES
"(SIXTH IN {5 8 4 6 11 17 2 3 8 21 6})
    --> 17"
"(SIXTH IN \"ACCTGTGAACGG\")
    --> \"T\""
"(SIXTH IN-EACH (REPLICONS-OF *all-organisms*) STRICT)
    --> ***ERROR***
    (This causes an error because some organisms don't have at least six replicons so there
    is not a sixth element in the list to return.)

    (SIXTH IN-EACH (REPLICONS-OF *all-organisms*) NONSTRICT)
    --> (NIL NIL #$S6803.pCC5.2 #$Npun.pNpE #$A7120.pEPSILON #$Tery.Contig58

    NIL NIL NIL NIL NIL #$Cwat.Contig046 #$A29413.Contig196)
    (When NONSTRICT is used, the organisms that don't have at least six replicons return NIL.)"
)
(:TEXT 
(:p "This function returns the sixth item of a list or the sixth character of a string.")
(:p "If IN-EACH is used, the sixth item of each list is returned.")
(:p "Use NONSTRICT to return NIL when there is not a sixth item in the list(s) that is being searched.")
)
(:SEE-ALSO BBL:FIRST BBL:SECOND BBL:THIRD BBL:FOURTH BBL:FIFTH BBL:SEVENTH BBL:EIGHTH BBL:NINTH BBL:TENTH)
)




;;================================= SEVENTH ==============================
(DOCUMENT-FUNCTION BBL::SEVENTH
(:SUMMARY "Returns the seventh item of a list or seventh character of a string.")
(:PARAMETERS
(entity :DOCSTRING "the list or string in which you are seeking the element")
(in :DOCSTRING "will take the seventh element of a string or a list, ignoring the internal structure of the list")
(in-each :DOCSTRING "when used on a list of strings, the seventh element of each string is returned; when used on a list of lists, the seventh element of each list is returned; (if a list contains strings and lists, the seventh element of each is returned)")
(nonstrict :DOCSTRING "if a list does not have at least 7 items, NIL will be returned")
(strict :DOCSTRING "(default) will return an error if this is used and there are not at least 7 items in each list")
)
(:RETURNS (a number letter or list))
(:EXAMPLES
"(SEVENTH IN {5 8 4 6 11 17 2 3 8 21 6})
    --> 2"
"(SEVENTH IN \"ACCTGTGAACGG\")
    --> \"G\""
"(SEVENTH IN-EACH (REPLICONS-OF *all-organisms*) STRICT)
    --> ***ERROR***
    (This causes an error because some organisms don't have at least seven replicons so there
    is not a seventh element in the list to return.)

    (SEVENTH IN-EACH (REPLICONS-OF *all-organisms*) NONSTRICT)
    --> (NIL NIL #$S6803.pCA2.4 NIL #$A7120.pZETA #$Tery.Contig59 NIL NIL NIL
    NIL NIL #$Cwat.Contig049 #$A29413.Contig197)
    (When NONSTRICT is used, the organisms that don't have at least seven replicons return NIL.)"
)
(:TEXT 
(:p "This function returns the seventh item of a list or the seventh character of a string.")
(:p "If IN-EACH is used, the seventh item of each list is returned.")
(:p "Use NONSTRICT to return NIL when there is not a seventh item in the list(s) that is being searched.")
)
(:SEE-ALSO BBL:FIRST BBL:SECOND BBL:THIRD BBL:FOURTH BBL:FIFTH BBL:SIXTH BBL:EIGHTH BBL:NINTH BBL:TENTH)
)



;;================================= EIGHTH ==============================
(DOCUMENT-FUNCTION BBL::EIGHTH
(:SUMMARY "Returns the eighth item of a list or eighth character of a string.")
(:PARAMETERS
(entity :DOCSTRING "the list or string in which you are seeking the element")
(in :DOCSTRING "will take the eighth element of a string or a list, ignoring the internal structure of the list")
(in-each :DOCSTRING "when used on a list of strings, the eighth element of each string is returned; when used on a list of lists, the eighth element of each list is returned; (if a list contains strings and lists, the eighth element of each is returned)")
(nonstrict :DOCSTRING "if a list does not have at least 8 items, NIL will be returned")
(strict :DOCSTRING "(default) will return an error if this is used and there are not at least 8 items in each list")
)
(:RETURNS (a number letter or list))
(:EXAMPLES
"(EIGHTH IN {5 8 4 6 11 17 2 3 8 21 6})
    --> 3"
"(EIGHTH IN \"ACCTGTGAACGG\")
    --> \"A\""
"(EIGHTH IN-EACH (REPLICONS-OF *all-organisms*) STRICT)
    --> ***ERROR***
    (This causes an error because some organisms don't have at least eight replicons so there
    is not a seighth element in the list to return.)

    (EIGHTH IN-EACH (REPLICONS-OF *all-organisms*) NONSTRICT)
    --> (NIL NIL #$S6803.pCB2.4 NIL NIL #$Tery.Contig60 NIL NIL NIL NIL NIL
    #$Cwat.Contig051 #$A29413.Contig198)
    (When NONSTRICT is used, the organisms that don't have at least eight replicons return NIL.)"
)
(:TEXT 
(:p "This function returns the eighth item of a list or the eighth character of a string.")
(:p "If IN-EACH is used, the eighth item of each list is returned.")
(:p "Use NONSTRICT to return NIL when there is not a eighth item in the list(s) that is being searched.")
)
(:SEE-ALSO BBL:FIRST BBL:SECOND BBL:THIRD BBL:FOURTH BBL:FIFTH BBL:SIXTH BBL:SEVENTH BBL:NINTH BBL:TENTH)
)



;;================================= NINTH ==============================
(DOCUMENT-FUNCTION BBL::NINTH
(:SUMMARY "Returns the ninth item of a list or ninth character of a string.")
(:PARAMETERS
(entity :DOCSTRING "the list or string in which you are seeking the element")
(in :DOCSTRING "will take the ninth element of a string or a list, ignoring the internal structure of the list")
(in-each :DOCSTRING "when used on a list of strings, the ninth element of each string is returned; when used on a list of lists, the ninth element of each list is returned; (if a list contains strings and lists, the ninth element of each is returned)")
(nonstrict :DOCSTRING "if a list does not have at least 9 items, NIL will be returned")
(strict :DOCSTRING "(default) will return an error if this is used and there are not at least 9 items in each list")
)
(:RETURNS (a number letter or list))
(:EXAMPLES
"(NINTH {5 8 4 6 11 17 2 3 8 21 6})
    --> 8"
"(NINTH IN \"ACCTGTGAACGG\")
    --> \"A\""
"(NINTH IN-EACH (REPLICONS-OF *all-organisms*) STRICT)
    --> ***ERROR***
    (This causes an error because some organisms don't have at least nine replicons so there
    is not a ninth element in the list to return.)

    (NINTH IN-EACH (REPLICONS-OF *all-organisms*) NONSTRICT)
    --> (NIL NIL NIL NIL NIL #$Tery.Contig65 NIL NIL NIL NIL NIL
    #$Cwat.Contig052 #$A29413.Contig199)
    (When NONSTRICT is used, the organisms that don't have at least nine replicons return NIL.)"
)
(:TEXT 
(:p "This function returns the ninth item of a list or the ninth character of a string.")
(:p "If IN-EACH is used, the ninth item of each list is returned.")
(:p "Use NONSTRICT to return NIL when there is not a ninth item in the list(s) that is being searched.")
)
(:SEE-ALSO BBL:FIRST BBL:SECOND BBL:THIRD BBL:FOURTH BBL:FIFTH BBL:SIXTH BBL:SEVENTH BBL:EIGHTH BBL:TENTH)
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
(:RETURNS (a number letter or list))
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

