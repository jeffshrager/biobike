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

(DOCUMENT-MODULE flow-logic
  "Functions dealing with program flow and logical choices"
  (:keywords :logic :predicate :true :false)
  (:display-modes :bbl)
  (:submodules logical-comparison
               logical-connectives
               loops-and-conditional-execution
               other-logic-functions)
  #.`(:FUNCTIONS apply-function repeat-function if-false if-true loop for-each order either both not))
;;================================= SAME ==============================


(DOCUMENT-FUNCTION SAME
  (:RETURNS "Returns T if two inputs are the same, Nil if not")
  (:SUMMARY "Compares two items that may or may not be of the same type") 
  (:PARAMETERS (x :docstring "The first input to be compared" :parameter-type required :value-type Any)
               (y :docstring "The second input to be compared" :parameter-type required :value-type Any)
               (case-insensitive :docstring "(Default) Ignores the case of the inputs" :parameter-type token :value-type boolean)
               (case-sensitive :docstring "Considers the case of the inputs" :parameter-type flag :value-type boolean))
  (:EXAMPLES  
"1. (SAME \"cat\" \"dog\")  --> NIL"
"2. (SAME \"cat\" 'CAT)  --> T ;; two inputs are evaluated case-insensitive."
"3. (SAME \"cat\" as \"CAT\" case-sensitive)  --> NIL"
"4. (DEFINE even-to-10 AS (FROM 2 TO 10 BY 2))  --> (2 4 6 8 10)
   Then,   
   (SAME even-to-10 {2 4 6 8 10})  --> T"
"5. (SAME \"AAGCG\" 'AAGCG)  --> T"
"6. (SAME 54 (* 6 9))  --> T"
"7." (#.(one-string-nl 
	"(FOR-EACH gene IN (GENES-OF A7120)"
        "     AS name = (description-of gene)"
        "     WHEN (SAME name \"WD-40 repeat protein\")"
        "     COLLECT gene)" )
   (:values #$A7120.alr0029 #$A7120.alr0247 #$A7120.all0283 #$A7120.all0284
 #$A7120.all0478 #$A7120.all0664 #$A7120.all2124 #$A7120.all2352
 #$A7120.alr3466 "....")
      "Looks for and returns the list of all genes described as \"WD-40 repeat protein\" in Anabaena PCC 7120"))  
  (:TEXT (:p "Items may be strings, symbols, or frames")
         (:p "Case is ignored if neither case-sensitive nor case-insensitive is specified")
         (:p "If inputs are lists, then compares them item by item"))
  )

;;================================= ALL-TRUE ==============================


(DOCUMENT-FUNCTION ALL-TRUE
(:PARAMETERS 
      (arguments :docstring "A list of arguments")
 )
(:EXAMPLES 
"1. If given a list of arithmetic expressions
      (ALL-TRUE {(EQUAL (+ 5 2) 7) (= (* 2 2) 4) (< 7 8)})
      --> T
      (ALL-TRUE {(EQUAL (+ 5 2) 7) (= (* 2 2) 4) (< 9 8)})
      --> NIL"

"2. If given a list of biological expressions
      (ALL-TRUE (FOR-EACH organism IN *all-organisms*
      COLLECT organism[.completed]))
      --> NIL, indeed: 
                 (FOR-EACH organism IN *all-organisms*
                 COLLECT organism[.completed])
                 --> (T T T T T NIL T T T T T NIL NIL)"

"3. When called by conditional keyword within a loop
      (FOR-EACH gene IN (FLATTEN (genes-of A7120))
         AS encodes-protein? = gene[.encodes-protein]
         AS length =  (LENGTHS-OF gene)
      WHEN (ALL-TRUE {(> length 50) (< length 100) encodes-protein?})  
      COLLECT gene)
      --> (#$A7120.asl1922 #$A7120.asl2301 #$A7120.asl4263)"
)
(:TEXT 
(:p  "The function ALL-TRUE returns TRUE (T) if all of the arguments within a list are TRUE; otherwise returns FALSE (NIL).")
(:ul 
(:li "ALL-TRUE follows the conventions of " ((:a :href (:print (help::make-help-glossary-entry-url :name "True and False")))
     "true and false")".")
(:li "ALL-TRUE is often called by " ((:a :href (:print (help::make-help-glossary-entry-url :name "Conditionals")))
     "conditionals") "."))
)
(:SEE-ALSO ALL-FALSE ANY-TRUE ANY-FALSE IF-TRUE IF-FALSE))

;;================================= ANY-TRUE ==============================

(DOCUMENT-FUNCTION ANY-TRUE
(:PARAMETERS 
      (arguments :docstring "A list of arguments")
 )
(:EXAMPLES 
"1. If given a list of arithmetic expressions
      (ANY-TRUE {(EQUAL (+ 5 2) 8) (= (* 2 2) 10) (< 7 8)})
      --> T
      (ANY-TRUE {(EQUAL (+ 5 2) 7) (= (* 2 2) 4) (< 7 8)})
      --> T
      (ANY-TRUE {(EQUAL (+ 5 2) 6) (= (* 2 2) 5) (< 9 8)})
      --> NIL"

"2. If given a list of biological expressions
      (ANY-TRUE (FOR-EACH organism IN *all-organisms*
      COLLECT organism[.completed]))
      --> T, indeed: 
               (FOR-EACH organism IN *all-organisms*
               COLLECT organism[.completed])
               --> (T T T T T NIL T T T T T NIL NIL)"
"3. When called by conditional keyword within a loop
      (FOR-EACH gene IN (FLATTEN (genes-of A7120))
         AS encodes-protein? = gene[.encodes-protein]
         AS length =  (LENGTHS-OF gene)
         WHEN (AND (EQUAL  encodes-protein? T) (ANY-TRUE {(> length 5000) (< length 100)})) 
      COLLECT gene)
      --> (#$A7120.alr0276 #$A7120.all0283 #$A7120.all0284 #$A7120.all0323
      #$A7120.alr0354 #$A7120.all0462 #$A7120.all0478 #$A7120.alr0709
      #$A7120.alr0710 #$A7120.all0886 #$A7120.alr0900 #$A7120.all0926
      #$A7120.alr1121 #$A7120.alr1229 #$A7120.all1625 #$A7120.all1696
      #$A7120.asl1922 #$A7120.all2124 #$A7120.all2239 #$A7120.alr2258
      #$A7120.all2282 #$A7120.asl2301 #$A7120.all2430 #$A7120.all2644
      #$A7120.all2645 #$A7120.all2648 #$A7120.all2655 #$A7120.alr2679
      #$A7120.alr2680 #$A7120.alr2682 #$A7120.all2875 #$A7120.all3557
      #$A7120.all3691 #$A7120.alr4238 #$A7120.asl4263 #$A7120.all4687
      #$A7120.all5100 #$A7120.all7128 #$A7120.all7130 #$A7120.alr7304
      #$A7120.alr7649)"

)
(:TEXT 
(:p "The function ANY-TRUE returns TRUE (T) if at least one argument within a list is TRUE; otherwise returns FALSE (NIL).")
(:ul 
(:li "ANY-TRUE follows the conventions of " ((:a :href (:print (help::make-help-glossary-entry-url :name "True and False")))
     "true and false")".")
(:li "ANY-TRUE is often called by " ((:a :href (:print (help::make-help-glossary-entry-url :name "Conditionals")))
     "conditionals") "."))
)
(:SEE-ALSO ANY-FALSE ALL-TRUE ALL-FALSE IF-TRUE IF-FALSE))


;;================================ ALL-FALSE =============================


(DOCUMENT-FUNCTION ALL-FALSE
(:PARAMETERS 
      (arguments :docstring "A list of arguments")
 )
(:EXAMPLES 
"1. If given a list of arithmetic expressions
      (ALL-FALSE {(EQUAL (+ 5 2) 6) (= (* 2 2) 5) (< 9 8)})
      --> T
      (ALL-FALSE {(EQUAL (+ 5 2) 6) (= (* 2 2) 4) (< 7 8)})
      --> NIL"

"2. If given a list of biological predicate expressions
      (ALL-FALSE (FOR-EACH organism IN *all-organisms*
                 COLLECT organism[.completed]))
      --> NIL, indeed: 
                  (FOR-EACH organism IN *all-organisms*
                  COLLECT organism[.completed])
                  --> (T T T T T NIL T T T T T NIL NIL)"

"3. When called by conditional keyword within a loop
      (FOR-EACH gene IN (FLATTEN (GENES-OF *all-organisms*))
         AS encode-protein? =  gene[.encodes-protein]
         AS description? = (description-of gene)
         AS transmembrane-reg? = gene[.TRANSMEMBRANE-REGIONS]
      WHEN (ALL-FALSE {encode-protein? description? transmembrane-reg?})  
      COLLECT gene)
      --> (#$P9313.RNA_53 #$P9313.RNA_54 #$P9313.RNA_55 #$P9313.RNA_52
      #$P9313.RNA_51 #$P9313.RNA_50 #$S8102.RNA_35 #$S8102.RNA_48)"

)
                  
(:TEXT 
(:p "The function ALL-FALSE returns TRUE (T) if all of the arguments within a list are FALSE; otherwise returns FALSE (NIL).")
(:ul 
(:li "ALL-FALSE follows the conventions of " ((:a :href (:print (help::make-help-glossary-entry-url :name "True and False")))
     "true and false")".")
(:li "ALL-FALSE is often called by " ((:a :href (:print (help::make-help-glossary-entry-url :name "Conditionals")))
     "conditionals") "."))
)
(:SEE-ALSO ALL-TRUE ANY-TRUE ANY-FALSE IF IF-TRUE IF-FALSE))

;;================================= ANY-FALSE ==============================


(DOCUMENT-FUNCTION ANY-FALSE
(:PARAMETERS 
      (arguments :docstring "A list of arguments")
 )
(:EXAMPLES 
"1. If given a list of arithmetic expressions
      (ANY-FALSE {(EQUAL (+ 5 2) 7) (= (* 2 2) 4) (< 9 8)})
      --> T
      (ANY-FALSE {(EQUAL (+ 5 2) 6) (= (* 2 2) 5) (< 9 8)})
      --> T
      (ANY-FALSE {(EQUAL (+ 5 2) 7) (= (* 2 2) 4) (< 7 8)})
      --> NIL"

"2. If given a list of biological predicate expressions
      (ANY-FALSE (FOR-EACH organism IN *all-organisms*
                 COLLECT organism[.completed]))
      --> T, indeed: 
                 (FOR-EACH organism IN *all-organisms*
                 COLLECT organism[.completed])
                 --> (T T T T T NIL T T T T T NIL NIL)"
)                  
(:TEXT 
(:p "The function ANY-FALSE returns TRUE (T) if at least one argument within a list is FALSE; otherwise returns FALSE (NIL).")
(:ul 
(:li "ANY-FALSE follows the conventions of " ((:a :href (:print (help::make-help-glossary-entry-url :name "True and False")))
     "true and false")".")
(:li "ANY-FALSE is often called by " ((:a :href (:print (help::make-help-glossary-entry-url :name "Conditionals")))
     "conditionals") "."))
)
(:SEE-ALSO ALL-TRUE ALL-FALSE ANY-TRUE IF IF-TRUE IF-FALSE))

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

    (ASSIGN  LIST {\"SUPERMAN\" \"SPIDERMAN\" \"ME\" \"YOU\"})

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
(:SYNTAX (IF-TRUE (test) [THEN] stasements1 [ELSE] statements2))
(:PARAMETERS
(test :VALUE-TYPE boolean :DOCSTRING "condition evaluated")
(then :VALUE-TYPE any :PARAMETER-TYPE keyword :DOCSTRING "statements evaluated if the condition is true")
(else :VALUE-TYPE any :PARAMETER-TYPE keyword :DOCSTRING "statements evaluated if the condition is not true"))
(:RETURNS "Value of last statement executed")
(:EXAMPLES
"1. Finds the elements in the list with the length shorter than 5:

    (ASSIGN  LIST {\"SUPERMAN\" \"SPIDERMAN\" \"ME\" \"YOU\"})

    (FOR-EACH I FROM 1 TO (LENGTH-OF LIST)
            (IF-TRUE (<(LENGTH-OF LIST[I] ) 5)
    		(PRINT LIST[I])) 

	--> \"ME\" 

	    \"YOU\""
"2. Finds the molecular weigths less than 7000:

	(ASSIGN list (MW-OF (PROTEIN-OF a7120)   ))
	
	(FOR-EACH item in list
 	     (IF-TRUE (< item  7000)
    	         THEN (PRINT item)))

	--> will print all molecular weigths less than 7000 of the proteins of a7120"
)
(:TEXT
(:p " If the result of the test is NIL , the statements from THEN are skipped . Otherwise , the statements from ELSE are skipped .")
(:p " THEN is optional and has no effect .")
(:p " The function allows multiple statements to be executed .")
)
(:SEE-ALSO IF-FALSE))


;;================ = < > <= >= ===================

(DOCUMENT-FUNCTION =
(:SUMMARY "Determines if numbers in a list are in equal.")
(:SYNTAX (= list))
(:PARAMETERS 
(list  :VALUE-TYPE list :DOCSTRING "the list you are assessing")
)
(:RETURNS "  T or NIL")
(:EXAMPLES
"(771 771)
    -->T"
"(= 0.0 -0.0)
    -->T"
"(= 3 3.0)
    -->T"
"(= 3 3 3 3)
    -->T"
"(= 3 6 5 2)
    -->NIL"
"(= (COUNT-OF \"T\" IN (SEQUENCE-OF SSR1600)) (COUNT-OF \"A\" IN (SEQUENCE-OF SSR1600)))
    -->T"
"(= (LENGTH-OF SSR1600) (LENGTH-OF ALL4312))
    -->NIL"
)
(:TEXT
(:p "This function will return T if all elements in the provided list are in decreasing order")
(:p "If the elements in the list are equal, the function will return NIL.")
(:p "This function will return NIL if all element in the provided list are not in decreasing order")
)
(:SEE-ALSO >= > <= <)
)



(DOCUMENT-FUNCTION <
(:SUMMARY "Determines if numbers in a list are in increasing order.")
(:SYNTAX (< list))
(:PARAMETERS 
(list  :VALUE-TYPE list :DOCSTRING "the list you are assessing")
)
(:RETURNS "  T or NIL")
(:EXAMPLES
"
    (< 351 351)
        --> NIL"
"
    (< 351 450)
        -->T"
"
    (< 0 3 4 6 7)
        -->T"
"
    (< 0 3 6 4 7)
        -->NIL"
"
    {(DEFINE X AS 430)
        (< 400 X 500)}
        -->(430 T)"
"
    (< (LENGTH-OF ALL4312) (LENGTH-OF SSR1600))
        -->NIL"
"
    (IF-TRUE (< 2 4)
      THEN DISPLAY \"smaller\")
        --> \"smaller\""
"
    (FOR-EACH gene IN (GENES-OF ss120)
      WHEN(< (LENGTH-OF gene) 200)
      COUNT gene)
        --> 208"
"
    (IF-TRUE (< (LENGTH-OF pro1186) 200)
      THEN (DISPLAY (SEQUENCE-OF pro1186))
        ELSE (DISPLAY \"Longer than or equal to 200\"))
        --> ATGGCAAATGAAGAGTCAGGAGGAATGGTTCCAGGATTATTTGCTATTGGCATATCAG..."

)
(:TEXT
(:p "This function will return T if all elements in the provided list are in increasing order")
(:p "If the elements in the list are equal, the function will return NIL.")
(:p "This function will return NIL if all element in the provided list are not in increasing order")
)
(:SEE-ALSO <= > >=)
)



(DOCUMENT-FUNCTION <=
(:SUMMARY "Determines if numbers in a list are in nondecreasing order.")
(:SYNTAX (<= list))
(:PARAMETERS 
(list :VALUE-TYPE list :DOCSTRING "the list you are assessing")
)
(:RETURNS "  T or NIL")
(:EXAMPLES
"
    (<= 351 351)
        -->T"
"
    (<= 450 351)
        -->NIL"
"
    (<= 0 3 4 4 6)
        -->T"
"
    (<= 0 3 4 7 6)
        -->NIL"
"
    {(DEFINE X AS 450)
        (<= 450 X 500)}
        -->(450 T)"
"
    (<= (COUNT-OF \"C\" IN (SEQUENCE-OF SSR1600))  (COUNT-OF \"A\" IN (SEQUENCE-OF SSR1600)))
        -->T"
"
    (IF-TRUE (<= 2 4)
      THEN DISPLAY \"yes\")
        --> \"yes\""
"
    (FOR-EACH gene IN (GENES-OF ss120)
      WHEN(<= (LENGTH-OF gene) 200)
      COUNT gene)
        --> 208"
"
    (IF-TRUE (<= (LENGTH-OF pro1186) 200)
      THEN (DISPLAY (SEQUENCE-OF pro1186))
        ELSE (DISPLAY \"Longer than 200\"))
        --> ATGGCAAATGAAGAGTCAGGAGGAATGGTTCCAGGATTATTTGCTATTGGCATATCAG..."
)
(:TEXT
(:p "This function will return T if all elements in the provided list are in nondecreasing order")
(:p "If the elements in the list are equal, the function will return T.")
(:p "This function will return NIL if all element in the provided list are in decreasing order")
)
(:SEE-ALSO < > >=)
)



(DOCUMENT-FUNCTION >
(:SUMMARY "Determines if numbers in a list are in decreasing order.")
(:SYNTAX (> list))
(:PARAMETERS 
(list  :VALUE-TYPE list :DOCSTRING "the list you are assessing")
)
(:RETURNS "  T or NIL")
(:EXAMPLES
"
    (> 771 771)
        --> NIL"
"
    (> 840 771)
        -->T"
"
    (> 4 3 2 1 0)
        -->T"
"
    (> 4 4 2 1 0)
        -->NIL"
"
    {(DEFINE X AS 430)
        (> 500 X 400)}
        -->(430 T)"
"
    (> (LENGTH-OF ALL4312) (LENGTH-OF SSR1600))
        -->T"
"
    (IF-TRUE (> 4 2)
      THEN DISPLAY \"bigger\")
        --> \"bigger\""
"
    (FOR-EACH gene IN (GENES-OF ss120)
      WHEN(> (LENGTH-OF gene) 200)
      COUNT gene)
        --> 1720"
"
    (IF-TRUE (> (LENGTH-OF all4312) 200)
      THEN (DISPLAY \"Longer than or equal to 200\"))
        ELSE (DISPLAY (SEQUENCE-OF all4312))
        --> Longer than or equal to 200"
)
(:TEXT
(:p "This function will return T if all elements in the provided list are in decreasing order")
(:p "If the elements in the list are equal, the function will return NIL.")
(:p "This function will return NIL if all element in the provided list are not in decreasing order")
)
(:SEE-ALSO >= < <=)
)



(DOCUMENT-FUNCTION >=
(:SUMMARY "Determines if numbers in a list are in nonincreasing order.")
(:SYNTAX (>= list))
(:PARAMETERS 
(list  :VALUE-TYPE list :DOCSTRING "the list you are assessing")
)
(:RETURNS "  T or NIL")
(:EXAMPLES
"    
    (>= 771 771)
        -->T"
"
    (>= 771 351)
        -->T"
"
    (>= 4 4 2 1 0)
        -->T"
"
    (>= 4 5 2 1 0)
        -->NIL"
"
    {(DEFINE X AS 430)
        (>= 430 X 500)}
        -->(430 T)"
"
    (>= (COUNT-OF \"C\" IN (SEQUENCE-OF SSR1600))  (COUNT-OF \"A\" IN (SEQUENCE-OF SSR1600)))
        -->NIL"
"
   (IF-TRUE (>= 4 2)
      THEN DISPLAY \"yes\")
        --> \"yes\""
"
    (FOR-EACH gene IN (GENES-OF ss120)
      WHEN(>= (LENGTH-OF gene) 200)
      COUNT gene)
        --> 1720"
"
    (IF-TRUE (>= (LENGTH-OF all4312) 200)
      THEN (DISPLAY \"Longer than 200\"))
        ELSE (DISPLAY (SEQUENCE-OF all4312))
        --> Longer than 200"
)
(:TEXT
(:p "This function will return T if all elements in the provided list are in nonincreasing order")
(:p "If the elements in the list are equal, the function will return T.")
(:p "This function will return NIL if all element in the provided list are in increasing order")
)
(:SEE-ALSO > < <=)
)



