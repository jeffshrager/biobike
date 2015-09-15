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

;;; Author: JP Massar, Arnaud Taton and Jeff Elhai.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (import '(
            com.biobike.help:document-module
               com.biobike.help:document-function) :bbi))

(DOCUMENT-MODULE definition
  "Functions that define or redefine variables or functions"
  (:keywords :equal :definition :variable :constant)
  (:display-modes :bbl)
  #.`(:functions assign define swap my-symbols my-variables my-functions define-function apply-function repeat-function new-table forget forget-all program everyones))

;;================================= ASSIGN ==============================


(DOCUMENT-FUNCTION ASSIGN
  (:SUMMARY "Sets one or more variables to given values")
  (:SYNTAX (ASSIGN target [AS | = | <-] assignment [DISPLAY-OFF]))
  (:RETURNS "value(s) assigned to variable(s)" :type "list or string")
  (:SUMMARY "Sets one or more variables to given values")
  (:PARAMETERS 
     (target :docstring "variable(s)" :parameter-type required :value-type any)
     (assignment :docstring "value(s) given to the variable(s)" :parameter-type required :value-type any)
     (display-off :docstring "When specified, display is suppressed" :parameter-type flag :value-type boolean))
    (:EXAMPLES "1. Assignment of one value to one variable
(ASSIGN BamHI AS \"GGATCC\") --> \"GGATCC\""
"2. Assignment of a list of values to one variable
(ASSIGN fav-orgs as {A7120 Npun Avar})
-->(#$anabaena_pcc7120 #$nostoc_punctiforme_atcc29133 #$anabaena_variabilis_atcc29413)"
"3. Assignment of a list of values to a list of variables
(ASSIGN (start-codon stop-codon) = {\"GTG\" \"TAA\"})
-->\"TAA\" ;; \"GTG\" is assigned as start-codon, and \"TAA\" is assigned as stop-codon"
"4. Assignment of one value to a list of variables
(ASSIGN BamHI-sites AS (PATTERN-MATCH-ALL \"GGATCC\" (SEQUENCE-OF A7120.chromosome)))
--> ((2861 2867) (8233 8239) (215025 215031) (300908 300914)
    (676018 676024) (1413871 1413877) (1854886 1854892) (5412536 5412542)
    (5425681 5425687) (5528794 5528800) (6154073 6154079))"
"5. Assignment of one value to a cell of a table

Step 1: (DEFINE mutations AS (NEW-TABLE {20 *nucleotides*} INITIALIZE 0))
            --> <Garray 2d (Numeric,Enum)> ;A 2D table consisting of 4 nucleotides and numbers 1-20 is created
             
Step 2: (ASSIGN mutations [7 \"A\"] = 1) --> 1 ; Numerical value 1 is assigned to position 7-A

Step 3: (DISPLAY-TABLE mutations) 
-->
      1   2   3   4   5   6   7   8   9   10  11  12  13  14  15  16  17  18  19  20  
 A   NIL NIL NIL NIL NIL NIL 1   NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL 
 C   NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL 
 G   NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL 
 T   NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL 
")              
  (:TEXT (:p "The output of the function is the value given to the first (perhaps only) variable")
         (:p "The output of the function is suppressed when DISPLAY is set to off. This is useful to avoid filling the history screen with large amounts of output")
         )
  (:SEE-ALSO DEFINE INCREMENT NEW-TABLE)
  )

;;================================= DEFINE ==============================


(DOCUMENT-FUNCTION DEFINE
(:PARAMETERS
(target :DOCSTRING "variable that takes the values")
(assignment :DOCSTRING "" )
(as\,=\,<- :PARAMETER-TYPE keyword :DOCSTRING "keyword for assigning the value")
(display-off :PARAMETER-TYPE keyword  :DOCSTRING "when specified, display is suppressed")) 
(:EXAMPLES
" 1. (DEFINE X AS 3)

	--> 3"
" 2. ((DEFINE LIST <- {\"X\" \"Y\" \"Z\"})
  
	-->\"List of length 3 suppressed\""
" 3.  (DEFINE cb-core = (COMMON-ORTHOLOGS-OF *all-organisms*)DIPLAY-OFF)

	-->\"List of length 776 suppressed\""
" 4. 	(DEFINE BamHI AS \"GGATCC\")
	
	-->\"GGATCC\"")
(:RETURNS "Assigned value .")
(:TEXT 
(:p " 1. The function is synonymous with ASSIGN.")
(:p " 2. It may be useful to reserve DEFINE for the first definition of a variable and ASSIGN for subsequent assignments.")
(:p " 3. The display of the output can be avoided by using DISPLAY-OFF. This is useful when you have long lists to define."))
(:SEE-ALSO ASSIGN ))

;;============================== APPLY-FUNCTION-OF =======================

(DOCUMENT-FUNCTION APPLY-FUNCTION-OF
 (:SUMMARY "Applies a function to a list of arguments, yielding a list of results" )
 (:SYNTAX (APPLY-FUNCTION-OF (variable-list) = function TO list-of-elements [list-of-elements]))
 
 (:PARAMETERS
   (variable-list :VALUE-TYPE list :DOCSTRING "Names of variables used in function")
   (function         :VALUE-TYPE form  :DOCSTRING "Form containing function to be applied")
   (list-of-elements :VALUE-TYPE list :PARAMETER-TYPE variable-number :DOCSTRING "List of values to be given function")
   )

   (:RETURNS "A list")

   (:EXAMPLES 
"1. Single variable (simple calculation)
     (APPLY-FUNCTION-OF (x) = (* x x)
        TO (FROM 1 TO 10))
     -->  (1 4 9 16 25 36 49 64 81 100)"

"2. Single variable (complex calculation)
     (APPLY-FUNCTION-OF (organism)
        = (DIVIDE (SUM-OF (LENGTHS-OF (INTERGENIC-SEQUENCES-OF organism)))
               BY (LENGTH-OF organism))
        TO *all-cyanobacteria*)
     -->  (0.1755734 0.117856435 0.11420659 0.10217614 0.17522822 ...)"

"3. Single variable (quick display)
     (APPLY-FUNCTION-OF (gene) 
         = (DISPLAY-LINE (ORGANISM-OF gene SHORT) *tab* 
              (NAME-OF gene SHORT) *tab* 
              (DESCRIPTION-OF gene))
         TO (ORTHOLOGS-OF ssr1600))
     --> A7120	asr1156	 hypothetical protein. 
         Npun	NpR1192	 hypothetical protein [Nostoc s
         S6803	ssr1600	 similar to anti-sigma f factor
         ..."

"4. Multiple variables (write multiple FastA files)
     (APPLY-FUNCTION-OF (header sequence) 
         = (WRITE FROM sequence TO (JOIN header \".txt\") HEADER header)
         TO (header-list sequence-list))
     --> [writes a file in FastA format for each header/sequence pair]")

(:TEXT
  (:p "APPLY-FUNCTION-OF makes it possible to map any function over a list or multiple lists. This means that the given function will be given each element of a list or lists, and the results will be collected into a new list, which is the returned result. The number of elements in the list of results equals the number of items in the shortest list provided. Elements are taken from the list one at a time, from first to last.")

  (:P "The variable(s) provided as the first argument are used within the function for replacement by elements of the list(s) and have no meaning outside the function. The variable list may be empty, if the function doesn't take any arguments. The function will be executed as many times as there are elements in the provided list. However, APPLY-FUNCTION rather than APPLY-FUNCTION-OF is more convenient for this application.")
 
  (:P "The number of lists provided after TO must equal the number of variables in the variable list.  If there are multiple variables and multiple lists, the lists need not be of the same length. Excess values in the longer list(s) will be ignored")

  (:P "APPLY-FUNCTION-OF may be a relatively quick alternative to certain kinds of loops.")

  
)

(:SEE-ALSO APPLY-FUNCTION LOOP FOR-EACH))
