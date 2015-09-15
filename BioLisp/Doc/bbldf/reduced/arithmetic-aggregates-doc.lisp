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

;;; Author:  Bogdan Mihai and Jeff Elhai.


(eval-when (:compile-toplevel :load-toplevel :execute)
  (import '(com.biobike.help:document-module
		com.biobike.help:document-function) :bbi))

(DOCUMENT-MODULE AGGREGATE-ARITHMETIC
  "Arithmetic functions that operate on lists of numbers"
  (:keywords :number :list)
  (:display-modes :bbl)
  (:toplevel? nil)
#.`(:functions sum-of divide multiply max-of min-of product-of add negative 
        bbl::Round subtract))

(DOCUMENT-FUNCTION SUM-OF
 (:SUMMARY "Sums all the numbers from a list of numbers")
 (:SYNTAX (SUM-OF list))
   (:PARAMETERS
	(list :DOCSTRING "list of elements"))
(:EXAMPLES 

"1. Summation of numbers :

	(SUM-OF {1 2 3})

	--> 6"

"2. Summation of  arithmetical operations :

	(SUM-OF {(+ (* 1 2) 3)(LENGTH \"BIOBIKE\")})

	--> 12"
)
(:RETURNS "number")
(:TEXT
(:p " The list must contain numbers or functions that return numbers")
)
(:SEE-ALSO + ADD PRODUCT-OF))

(DOCUMENT-FUNCTION DIVIDE
(:PARAMETERS 
(entity1 :DOCSTRING "the number or list of numbers to be divided ")
(by      :DOCSTRING "the value/values to be divided by ")
(into    :DOCSTRING "") 
)
(:EXAMPLES

" 1. (DIVIDE 1 BY 2)

	--> 0.5"
"2. (DIVIDE {1 2 3} BY 3)

	--> (0.33333334 0.6666667 1)"
"3. (DIVIDE {3 6 9} BY {1 2 3 })

	--> ( 3 3 3 )"

)

(:TEXT 
(:p " 1. The division is done element by element . ")
(:p " 2. If you divide two lists , they must have the same number of elements .")
(:p " 3. Division by zero will give an error . "))
(:SEE-ALSO  - + ADD PRODUCT-OF))



(DOCUMENT-FUNCTION ADD
(:PARAMETERS
(entity1 :DOCSTRING "number or list of numbers that are added")
(to      :DOCSTRING "number or list of numbers to add to"))
(:EXAMPLES 
"1. (ADD 3 TO 4)


	--> 7"
"2. (ADD {1 2 3} TO {4 5 6})

	-->(5 7 9)"
"3. (define table1 as (new-table {3 3} initialize 4))
    
    (ADD table1 to 3)

	-->   1    2  
     1    7.0  7.0 
     2    7.0  7.0
     3    7.0  7.0" 
)

(:TEXT 
(:p " 1. The numbers to add to and that are added cannot be anykind of functions , nor characters .")
(:p " 2. The function works only with numbers , lists or tables of numbers .")
(:p " 3. If the function adds two lists or tables , they must have the same number of elements . ")
(:p " 4. The addition of lists is done element by element . ")
(:p " 5. Not like the little_brother_function \"SUM-OF\" , this function adds two lists or tables between them .")
)
(:SEE-ALSO DIVIDE - + PRODUCT-OF))


(DOCUMENT-FUNCTION PRODUCT-OF
(:SUMMARY "Makes the product of numbers or lists of numbers .")
(:SYNTAX (PRODUCT-OF entity1 ))
(:PARAMETERS 
(entity1 :VALUE-TYPE number/list :DOCSTRING "the number or list that are multiplied")
)
(:RETURNS "A number or list of numbers .")
(:EXAMPLES
"1. (PRODUCT-OF 1 23 5)

	-->115"
"2. (PRODUCT-OF {1 23 5})

	-->115"
)
(:TEXT 
(:p " 1. The multiplication of two lists is not possible (i.e.(PRODUCT-OF {1 2 3} {1 2 3})) . ")
(:p " 2. The multiplication of a list by a number is not possible (i.e. (PRODUCT-OF {1 2 3} 3) . ")
(:p " 3. The notations 1 2 3 and {1 2 3} are equivalent for this function . ")
(:p " 4. Not like the bigger_brother_function \"MULTIPLY\" , this function multiplies the elements of a list between them .")
)
(SEE-ALSO * / SUM-OF MULTIPLY))

(DOCUMENT-FUNCTION MULTIPLY
(:PARAMETERS 
(entity1 :DOCSTRING "number,list or table to be multiplied")
(by :DOCSTRING"number, list or table to multiply with ")
)
(:EXAMPLES 
"1. (MULTIPLY 3 BY 4)

	-->12"
"2. (MULTIPLY {1 2 3} BY 3)

	-->(3 6 9)"
"3. (MULTIPLY {1 2 3} BY {4 5 6})

	-->(4 10 18)"
"4. (DEFINE TABLE1 AS(NEW-TABLE {3 3} INITIALIZE 3))
    
    (DEFINE TABLE2 AS(NEW-TABLE {3 3} INITIALIZE 4))

    (MULTIPLY TABLE1 BY TABLE2)
 
	-->    1  2  3
	  1   12 12 12
	  2   12 12 12
	  3   12 12 12"
"5. (MULTIPLY {(LENGTH-OF \"SUPERMAN\") (LENGTH-OF \"SPIDERMAN\")} BY {(LENGTH-OF \"LOUIS LANE\") (LENGTH-OF \"MARY JANE\")})

	-->(80 81)"
)
(:TEXT
(:p " 1. You can multiply lists or tables element by element only if they are the same length .")
(:p " 2. You can multiply lists of functions that return numbers , but the lists have to have the same number of elements .")
(:p " 3. Not like the little_brother_function \"PRODUCT-OF\" , this function multiplies element by element of two lists ."))
(:SEE-ALSO PRODUCT-OF SUM-OF DIVIDE))

(DOCUMENT-FUNCTION MAX-OF
  (:SUMMARY "Returns the largest argument from a list . ")    
  (:SYNTAX (MAX-OF list))
  (:PARAMETERS
    (list   :VALUE-TYPE list :DOCSTRING "list of elements")
    )
  (:RETURNS "A number . ")
  (:EXAMPLES

 " 1. (MAX-OF {1 2 3 4 5 6 7 8 9 })
      
             --> 9"
             
 " 2. (MAX-OF (LAST-N 4 {1 2 3 4 5 6 7 8 9 11.55})) 
      
             --> 11.55 "
 " 3. (MAX-OF {1 3 2})   
 
             -->  3 "  
" 4. (MAX-OF {5.44 (/ 4 6.1006)  -.005}))
		
		 --> 5.44 "
" 5. (MAX-OF {(LENGTH-OF \"MARINA\")(LENGTH-OF \"BIOBIKE\")}) 

		 --> 7 "
                                                    )
(:TEXT
  (:p " 1. The function returns the largest element in the list regardless of it's position whithin the list .")
  (:p " 2. The elements of the list must be numbers or functions that return numbers . ")

  )
  (:SEE-ALSO MIN-OF))   


(DOCUMENT-FUNCTION MIN-OF
  (:SUMMARY "Returns the smallest argument from a list . ")    
  (:SYNTAX (MIN-OF list))
  (:PARAMETERS
    (list   :VALUE-TYPE list :DOCSTRING "list of elements")
    )
  (:RETURNS "A number . ")
  (:EXAMPLES
 " 1. (MIN-OF {0.5 2 3 4 5 6 7 8 9 })
      
             --> 0.5"
             
 " 2. (MIN-OF (LAST-N 4 {1 2 3 4 5 6 7 8 9 10})) 
      
             --> 7 "
 " 3. (MIN-OF {3 1 2 -1})   
 
             --> -1 " 
 " 4. (MIN-OF {(LENGTH-OF \"MARINA\")(LENGTH-OF \"BIOBIKE\")}) 

	     --> 6 "
                                                    )
(:TEXT
  (:p " 1. The function returns the smallest element in the list regardless of it's position whithin the list .")
  (:p " 2. The elements of the list must be numbers or functions that return numbers . ")
  )
  (:SEE-ALSO MAX-OF))   



(DOCUMENT-FUNCTION COUNT-OF 
(:PARAMETERS
(query :VALUE-TYPE string :DOCSTRING "the element that you must find" )
(not :DOCSTRING "will count all elements except specified value")
(each :DOCSTRING "will return a list with the count of each element of a provided list")
(case-sensitive :DOCSTRING "will consider the case of the element searching for")
(case-insensitive :DOCSTRING "will not consider the case of the element searching for")
(in :PARAMETER-TYPE keyword :DOCSTRING "the entity that contains the elements your are counting")
(in-each :PARAMETER-TYPE keyword :DOCSTRING "a list of entities that contain the element you are counting")
(entity :VALUE-TYPE string\,list :DOCSTRING "the thing you search in"))

(:RETURNS " A number" )
(:EXAMPLES
"(COUNT-OF \"G\" IN pNpA)       

	--> 72949 (Number of \"G\"s IN pNpA)" 

"(COUNT-OF \"ATG\" IN (FIRST (GENES-OF a7120))) 
	--> 10"

"(COUNT-OF \"ATG\" IN-EACH (SEQUENCES-OF (GENES-OF A7120)) )

	--> (10 5 4 9 17 2 8 8 3 8 6 20 7 18 11 12 10 4 2 30 8 8 3 17 5 4 8 5 75 36 38 20...)" 

"(COUNT-OF \"unknown protein\" IN (DESCRIPTION-OF (GENES-OF a7120)))
	--> 1754
    (COUNT-OF \"Unknown Protein\" CASE-SENSITIVE IN (DESCRIPTION-OF (GENES-OF a7120)))
	--> 0
    (COUNT-OF \"Unknown Protein\" CASE-INSENSITIVE IN (DESCRIPTION-OF (GENES-OF a7120)))
	--> 1754
"

"(COUNT-OF \"a\" IN (SEQUENCE-OF ssr1600))
	--> 89
    (COUNT-OF NOT \"a\" IN (SEQUENCE-OF ssr1600))
	--> 262
    (LENGTH-OF (SEQUENCE-OF ssr1600))
   	--> 351
"

"(COUNT-OF EACH {\"A\" \"T\"} IN (SEQUENCE-OF ssr1600))
	--> (89 89)
    (COUNT-OF EACH NOT {\"A\" \"T\"} IN (SEQUENCE-OF ssr1600))
	--> 173
    (COUNT-OF EACH {\"C\" \"G\"} IN (SEQUENCE-OF ssr1600))
	--> (86 87)
"
) 
(:TEXT
(:p "  Returns the number of times the query appears in the target.")
(:p " If no target is given, then the number of elements in the list is returned (and if the object of COUNT-OF is not a list, an error is created).")
(:p " Query may be a sequence that appears in a larger sequence, a string that appears in a larger string, or an item that appears in a list or a list of sequences or items.")
(:p " Target may be a sequence or anything that has a sequence (e.g. an organism) or a list.")
(:p " When IN-LIST is used, the list is searched for all instances of query.")
(:p " When IN-EACH is used, each element of the list is searched for all instances of query.")
(:p " IN is synonymous with IN-LIST."))
(:SEE-ALSO COUNTS-OF LENGTH-OF FOR-EACH))


(DOCUMENT-FUNCTION SUBTRACT
  (:PARAMETERS
    (entity1 :DOCSTRING "the entity to be substracted or subtracted from")
    (by :DOCSTRING "If given, the second entity is subtracted from the first")
    (from :DOCSTRING "If given, the first entity is subtracted from the second")
    )
  (:RETURNS "A number, list or table")
  (:EXAMPLES
    "1. Simple subtraction
        (SUBTRACT 100 BY 10)
        --> 90
        (SUBTRACT 100 FROM 10)
        --> -90"
     
    "2. Subtraction of a number from a list of numbers
        (SUBTRACT {2 4 6} BY 4)
        --> (-2 0 2}
        (SUBTRACT 4 FROM {2 4 6})
        --> (-2 0 2}

        (DEFINE alternate-origin AS 57067)
        (DEFINE start-coords AS (ITEM .start IN-EACH (GENES-OF S7942)))
        (DEFINE new-start-coords 
            AS (SUBTRACT alternate-origin from start-coords))"
                
    "3. Subtraction of a list of numbers from a number
        (SUBTRACT {2 4 6} FROM 4)
        --> (2 0 -2)
        (SUBTRACT 4 BY {2 4 6})
        --> (2 0 -2)"

    "4. Subtraction of a list of numbers from another list of numbers
        (DEFINE signal-replicates AS {548 550 499 823 745 421})
        --> (548 550 499 823 745 421)  
        (DEFINE background-replicates AS {58 98 78 35 11 32})
        --> (58 98 78 35 11 32)  
        (SUBTRACT signal-replicates BY background-replicates)
        --> (490 452 421 788 734 389)

        (Element #1 of the second list is subtracted from Element #1 of
        first list, and so on)"

    "5. Subtraction of a table of numbers by a number or vice versa
        (DEFINE test-table AS (NEW-TABLE {3 2} INITIALIZE 47))
        --> <Table 2d (Numeric,Numeric)>
        (DISPLAY-TABLE test-table)
        -->
                1   2   3   
            1   47  47  47  
            2   47  47  47
        (DISPLAY-TABLE (SUBTRACT test-table BY 18))
        -->
                1   2   3   
            1   29  29  29  
            2   29  29  29"
)
  (:TEXT
    (:p (:ul "SUBTRACT differs from the - function in the following ways:"
        (:UL "The syntax of SUBTRACT may be more readable than the syntax of -"
             "SUBTRACT works on lists and tables, while - does not"
             "SUBTRACT is slower than -, although you will probably not notice the difference unless working with very large sets or looping over many iterations")))

    (:P (:UL "BY and FROM clauses are mutually incompatible -- you may use only one of these. Use of the BY and FROM clauses cause the following transformations:
        (SUBTRACT a BY b)   -->  a minus b  (a - b)
        (SUBTRACT a FROM b) -->  b minus a  (b - a)"))

    (:P (:UL "SUBTRACT works only on numbers and lists of numbers. If you want to subtract one set of elements from another set of elements, use SUBTRACT-SET"))
)
  (:SEE-ALSO - SUBTRACT-SET)
  )