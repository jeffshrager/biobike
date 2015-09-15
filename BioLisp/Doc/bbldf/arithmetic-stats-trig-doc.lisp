
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

;;; Author: Bogdan Mihai, Emily Niman, and Jeff Elhai

(eval-when (:compile-toplevel :load-toplevel :execute)
(import '(com.biobike.help:document-module
com.biobike.help:document-function) :bbi))

(DOCUMENT-MODULE ARITHMETIC
"Arithmetic functions"
(:keywords :number )
(:submodules more-arithmetic
other-numeric
trigonometric-functions
statistics
numerical-type-checks)
#.`(:functions Sum-of Difference-of Product-of Quotient-of calc abs)
(:display-modes :bbl))

#|
(DOCUMENT-MODULE AGGREGATE-ARITHMETIC
"Arithmetic functions that operate on lists of numbers"
(:keywords :number :list)
(:display-modes :bbl)
(:toplevel? nil)
#.`(:functions sum-of quotient-of difference-of divide multiply max-of min-of product-of add negative negation-of
bbl::Round subtract))

(DOCUMENT-MODULE BASIC-ARITHMETIC
"Basic arithmetic functions that operate on numbers or functions"
(:keywords :number )
(:display-modes :bbl)
(:toplevel? nil)
#.`(:functions abs decrement increment log log10 log2 round ! ^
* + - / Add multiply subtract divide negative sqrt exp mod
Random-number Random-integer number-list))
|#

(DOCUMENT-MODULE MORE-ARITHMETIC
"Basic arithmetic functions that operate on numbers or functions"
(:keywords :number )
(:display-modes :bbl)
(:toplevel? nil)
#.`(:functions abs decrement increment log log10 log2 ! ^
negative sqrt exp mod
))

(DOCUMENT-MODULE OTHER-NUMERIC
"Arithmetic functions that operate on lists of numbers"
(:keywords :number :list)
(:display-modes :bbl)
(:toplevel? nil)
#.`(:functions max-of min-of Random-integer Random-number Round
Number-list Permutation-counts-of Combination-counts-of))

(DOCUMENT-MODULE TRIGONOMETRIC-FUNCTIONS
"Basic trigonometric functions that operate on numbers or degrees in radians "
(:keywords :number :trig :trigonometry)
(:display-modes :bbl)
(:toplevel? nil)
#.`(:functions
ACOS ASIN ATAN COS SIN TAN
;; COSH SINH TANH ACOSH ASINH ATANH
))

(DOCUMENT-MODULE STATISTICS
"Basic statistical functions"
(:keywords :number :statistical)
(:display-modes :bbl)
(:toplevel? nil)
#.`(:functions mean median item/s-of-rank std-dev t-test bin-data-of))


(DOCUMENT-MODULE NUMERICAL-TYPE-CHECKS
"Functions that determine what type of number is given"
(:keywords :number :type :integer)
(:display-modes :bbl)
(:toplevel? nil)
#.`(:functions is-number? is-positive-integer? is-positive-number? is-nonnegative?
     is-integer? is-even? is-odd? is-negative-number?
))


(DOCUMENT-FUNCTION IS-INTEGER?
  (:SUMMARY "Returns whether or not an entity is an integer.")
  (:SYNTAX (IS-INTEGER? ANY))
  (:PARAMETERS
   (any :VALUE-TYPE ANY
        :DOCSTRING 
        "The entity that you want to test whether it is an integer or not."
        ))
  (:RETURNS "A boolean (T or NIL).")
  (:EXAMPLES 
   ((is-integer? 3) t)
   ((is-integer? 2.1) nil)
   ((is-integer? 2.0) t)
   ((is-integer? "This is not an integer") nil)
   )
  (:KEYWORDS number)
  (:SEE-ALSO is-number? is-nonnegative? is-even?  
   is-odd? is-negative-number? 
   is-positive-number? is-positive-integer?)
  )
  

(DOCUMENT-FUNCTION IS-EVEN?
  (:SUMMARY "Returns whether the entity is an even number")
  (:SYNTAX (IS-EVEN? ANY))
  (:PARAMETERS
   (any :VALUE-TYPE ANY
        :DOCSTRING 
        "The entity that you want to test whether it is even."
        ))
  (:RETURNS "A boolean (T or NIL).")
  (:EXAMPLES 
   ((is-even? 2) t)
   ((is-even? 1) nil)
   ((is-even? -3) nil)
   ((is-even? -4) t)
   ((is-even? 0) t)
   ((is-even? 2.1) nil)
   ((is-even? 2.0) t)
   ((is-even? "This is not a number") nil)
   )
  (:KEYWORDS number)
  (:SEE-ALSO is-number? is-nonnegative? is-integer?
   is-odd? is-negative-number? 
   is-positive-number? is-positive-integer?)
  )

(DOCUMENT-FUNCTION IS-ODD?
  (:SUMMARY "Returns whether the entity is an odd number")
  (:SYNTAX (IS-ODD? ANY))
  (:PARAMETERS
   (any :VALUE-TYPE ANY
        :DOCSTRING 
        "The entity that you want to test whether it is odd"
        ))
  (:RETURNS "A boolean (T or NIL).")
  (:EXAMPLES 
   ((is-odd? 2) nil)
   ((is-odd? 1) t)
   ((is-odd? -3) t)
   ((is-odd? -4) nil)
   ((is-odd? 0) nil)
   ((is-odd? 2.1) nil)
   ((is-odd? 2.0) nil)
   ((is-odd? "This is not a number") nil)
   )
  (:KEYWORDS number)
  (:SEE-ALSO is-number? is-nonnegative? is-even?  
   is-integer? is-negative-number? 
   is-positive-number? is-positive-integer?)
  )
  