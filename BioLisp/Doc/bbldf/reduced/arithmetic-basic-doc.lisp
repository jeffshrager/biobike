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

;;; Author:  Emily Niman, Bogdan Mihai, and Jeff Elhai

(eval-when (:compile-toplevel :load-toplevel :execute)
  (import '(com.biobike.help:document-module
		com.biobike.help:document-function) :bbi))

(DOCUMENT-MODULE BASIC-ARITHMETIC
  "Basic arithmetic functions that operate on numbers or functions"
  (:keywords :number )
  (:display-modes :bbl)
  (:toplevel? nil)
  #.`(:functions abs decrement increment log log10 log2 round ! ^  
        * + - / Add multiply subtract divide negative sqrt exp mod
        Random-number Random-integer))


(DOCUMENT-FUNCTION ABS
  (:SUMMARY "Returns the absolute value of a number.")
  (:SYNTAX (ABS number))
  (:PARAMETERS
    (number  :VALUE-TYPE number :DOCSTRING "the number you are taking the absolute value of")
   )
  (:RETURNS "A number" )
  (:EXAMPLES
"(ABS -.7999)
    --> 0.7999"
"(ABS (-(/ 12 13)))
    --> 0.9230769"
"(ABS (SIN -1))
    --> 0.84147096"
"(- (LENGTH-OF ssr1600) (LENGTH-OF all4312))
    --> - 420

    (ABS (- (LENGTH-OF ssr1600) (LENGTH-OF all4312)))
    --> 420"
)
(:TEXT 
(:p "This function returns the absolute value of a number.")
(:p "If the number you enter is negative\, the same number will be returned without a negative sign.")
(:p "If the number you enter is positive\, the same number will be returned.")
(:p "It is unnecessary to use ABS with the SQRT function because SQRT does not return the positive and negative values of an answer.")
)
(:SEE-ALSO * - )
)



(DOCUMENT-FUNCTION INCREMENT
(:PARAMETERS 
	(entity  :DOCSTRING "the start value")
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

	-->  3"

)
(:RETURNS "number")
(:TEXT 
(:P " 1. The incrementation can be done only on variables or lists of variables " )
(:P " 2. If we don't use a specific value for the incrementation, the initial value would be incremented with 1 ")
(:p " 3. We cam use as an increment a function that returns a number ")
)
(:SEE-ALSO + ADD - SUBTRACT DECREMENT SUM-OF))


(DOCUMENT-FUNCTION LOG10
(:SUMMARY "Calculates the logarithm base10 of an entyty ")
(:SYNTAX (log10 entity ))
(:PARAMETERS 
	(entity :VALUE-TYPE number :docstring "number ")
)
(:EXAMPLES
"1. Logarithm base 10 from  number :
	
	(LOG10 3)

	--> 0.47712126

2. Logarithm base 10 from number :

	(LOG10 (sqrt (* 2 3 )))

	-->  0.38907564"
)
(:RETURNS "a number")
(:TEXT
(:p " 1. The function uses numbers or other functions that return numbers ")
)
(:SEE-ALSO LOG LOG2)
)



(DOCUMENT-FUNCTION LOG2
(:SUMMARY "Calculates the logarithm base 2 of an entity ")
(:SYNTAX (log10 entity ))
(:PARAMETERS 
	(entity :VALUE-TYPE number :docstring "number ")
)
(:EXAMPLES
"1. Logarithm base 2 from  number :
	
	(LOG2 5)

	--> 2.321928

2. Logarithm base 2 from number :

	(LOG2 (sqrt (* 46 67 )))

	-->  5.7948256"
)
(:RETURNS "a number")
(:TEXT
(:p " 1. The function uses numbers or other functions that return numbers ")
)
(:SEE-ALSO LOG LOG10)
)



(DOCUMENT-FUNCTION ROUND
(:PARAMETERS
(number  :DOCSTRING "the number you are rounding" )
(down :DOCSTRING "will round the number down")
(up :DOCSTRING "will round the number up")
(to :DOCSTRING "rounds number to nearest multiple of number indicated after keyword")
)
(:RETURNS "A number" )
(:EXAMPLES
"(ROUND .875)
    --> 1"
"(ROUND .667 DOWN)
    --> 0"
"(ROUND 565 TO 50)
    --> 550"
"(ROUND 1032 DOWN TO 50)
    --> 1000"
"(ROUND .65 UP TO-NEAREST .25)
    --> 0.75"
"(FOR-EACH gene IN (GENES-OF ss120)
  AS length = (LENGTH-OF gene)
  AS bin = (/ (ROUND length TO-NEAREST 50) 50)
    (INCREMENT gene-length[bin]))
    --> NIL
(For each gene in ss120 take the length and round it to the nearest multiple of 50.  Then divide by 50 
to get the related bin number the gene belongs in.  Each bin holds the number of genes that have a length within 
the range of gene-lengths the bin holds, each range is 50 units long.  When a gene-length is converted to 
a bin number, that bin's value goes up by 1.)

    (DISPLAY-TABLE gene-length)
    -->
     1   2   3   4   5   6   7   8   9   10  11  12  13  14  15  16  17  18  19  20...
    ;; >>> Line truncated to 200 (was 732). Use SET-OUTPUT-LIMITS to adjust width
     168 222 444 762 720 504 486 414 414 372 396 450 456 288 414 300 222 396 348 312...
    --> NIL
(Displays the table \"gene-length\" which contains the bin number and the corresponding number of genes 
it holds.  From the table we can see that there are 168 genes with lengths between 0 and 50.   The most genes 
fall within the range of 150 to 200 nucleotides long, represented by bin 4.)"
)

(:TEXT 
(:p "This function will round a non-integer to the nearest integer") 
(:p "When the flags UP or DOWN are specified, the number will be rounded in the direction indicated, regardless of the value of the nearest integer.")
(:p "When the keyword TO is specified, the number will be rounded to the nearest number that is an increment of the number specified after TO.")
(:p "The default value for TO is 1.")
(:p "TO and TO-NEAREST are synonymous.")
(:p "The TO keyword can be useful when placing data values into bins especially when creating graphs or tables.")
)
)



