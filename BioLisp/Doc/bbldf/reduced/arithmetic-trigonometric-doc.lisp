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

;;; Author:  Bogdan Mihai, Emily Niman, and Jeff Elhai

(eval-when (:compile-toplevel :load-toplevel :execute)
  (import '(com.biobike.help:document-module
	    com.biobike.help:document-function) :bbi))

(DOCUMENT-MODULE TRIGONOMETRIC-FUNCTIONS
  "Basic trigonometric functions that operate on numbers or degrees in radians "
  (:keywords :number )
  (:display-modes :bbl)
  (:toplevel? nil)
  #.`(:functions 
      ACOS ASIN ATAN COS SIN TAN
      ;; COSH SINH TANH ACOSH ASINH ATANH
      ))

(DOCUMENT-FUNCTION ACOS
  (:SUMMARY "Returns the arc cosine (acos) in radians")
  (:SYNTAX (ACOS n))
  (:PARAMETERS 
   (n :VALUE-TYPE number :DOCSTRING "the number in radians you want the arc cosine of (usually between -1 and 1)")
   )
  (:RETURNS " a number")
  (:EXAMPLES
   "(ACOS -1)
    --> 3.1415927"
   "(ACOS (/ (SQRT 3) 2))
    --> 0.5235988"
   "(ACOS -.707)
    --> 2.3560436"
   )
  (:TEXT
   (:p "Arc cosine is synonymous with inverse cosine.")
   (:p "This function for arc cosine requires an argument in radians and returns the angle in radians whose cosine is the argument you provide.")
   (:p "The argument provided is usually between -1 and 1, if not you will get a complex number.")
   (:p "The returned values will be between 0 and PI. (PI = 3.14159)")
   )
  (:SEE-ALSO COS ASIN ATAN)
  )


#+not-needed
(DOCUMENT-FUNCTION ACOSH
(:SUMMARY "Returns the hyperbolic arc cosine (acosh)")
(:SYNTAX (ACOSH n))
(:PARAMETERS 
(n :VALUE-TYPE number :DOCSTRING "the number you want the hyperbolic arc cosine of")
)
(:RETURNS " a number")
(:EXAMPLES
"(ACOSH 1)
    --> 0.0"
"(ACOSH 1.5430806)
    --> 0.99999994"
"(ACOSH 2.3524096)
    --> 1.5"
)
(:TEXT
(:p "Hyperbolic arc cosine is synonymous with hyperbolic inverse cosine.")
(:p "This function for hyperbolic arc cosine requires an argument and returns the angle whose hyperbolic cosine is the argument you provide.")
)
(:SEE-ALSO COS ASINH ATANH)
)



(DOCUMENT-FUNCTION ASIN
(:SUMMARY "Returns the arc sine (asin) in radians")
(:SYNTAX (ASIN n))
(:PARAMETERS 
(n :VALUE-TYPE number :DOCSTRING "the number in radians you want the arc sine of (usually between -1 and 1)")
)
(:RETURNS " a number")
(:EXAMPLES
"(ASIN 0)
    --> 0.0"
"(ASIN 1)
    --> 1.5707964"
"(ASIN (/(SQRT 2) 2))
    --> 0.7853981"
"(ASIN .5)
    --> 0.5235988"
)
(:TEXT
(:p "Arc sine is synonymous with inverse sine.")
(:p "This function for arc sine requires an argument in radians and returns the angle in radians whose sine is the argument you provide.")
(:p "The argument provided is usually between -1 and 1, if not you will get a complex number.")
(:p "The returned values will be between -PI/2 and PI/2. (PI = 3.14159)")
)
(:SEE-ALSO SIN ACOS ATAN)
)


#+not-needed
(DOCUMENT-FUNCTION ASINH
(:SUMMARY "Returns the hyperbolic arc sine (asinh)")
(:SYNTAX (ASINH n))
(:PARAMETERS 
(n :VALUE-TYPE number :DOCSTRING "the number in you want the hyperbolic arc sine of")
)
(:RETURNS " a number")
(:EXAMPLES
"(ASINH 0)
    --> 0.0"
"(ASINH 2.1292794)
    --> 1.5"
"(ASINH -10.017875)
    --> -3.0"
)
(:TEXT
(:p "Hyperbolic arc sine is synonymous with hyperbolic inverse sine.")
(:p "This function for hyperbolic arc sine requires an argument and returns the angle whose hyperbolic sine is the argument you provide.")
)
(:SEE-ALSO SIN ACOSH ATANH)
)



(DOCUMENT-FUNCTION ATAN
(:SUMMARY "Returns the arc tangent (atan)")
(:SYNTAX (ATAN n1 n2))
(:PARAMETERS 
(n1 :VALUE-TYPE number :DOCSTRING "The number in radians you want the arc tangent of.  If n2 is also included, n1 is the numerator of the complete argument you are providing.")
(n2 :PARAMETER-TYPE optional :VALUE-TYPE number :DOCSTRING "The denominator of the complete argument you are providing.")
)
(:RETURNS " a number")
(:EXAMPLES
"(ATAN 0)
    --> 0.0"
"(ATAN 0 1)
   --> 0.0"
"(ATAN 1)
    --> 0.7853982"
"(ATAN (SQRT 3))
    --> 1.0471976"
"(ATAN (/ (SQRT 3) 2) .5)
    --> 1.0471976"
)
(:TEXT
(:p "Arc tangent is synonymous with inverse tangent.")
(:p "This function for arc tangent requires an argument in radians and returns the angle in radians whose tangent is the argument you provide.")
(:p "If one number is provided, then the arc tangent of that number will be returned.")
(:p "If two numbers are provided, the first is the numerator and the second is the denominator and they form one complex argument for ATAN.")
(:p "The returned values will be between -PI/2 and PI/2. (PI = 3.14159)")
)
(:SEE-ALSO TAN ASIN ACOS)
)


#+not-needed
(DOCUMENT-FUNCTION ATANH
(:SUMMARY "Returns the hyperbolic arc tangent (atanh)")
(:SYNTAX (ATANH n))
(:PARAMETERS 
(n :VALUE-TYPE number :DOCSTRING "the number you want the hyperbolic arc tangent of")
)
(:RETURNS " a number")
(:EXAMPLES
"(ATANH 0)
    --> 0.0"
"(ATANH -0.7615942)
    --> -1.0"
"(ATANH .986661435)
    --> 2.5017757"
)
(:TEXT
(:p "Hyperbolic arc tangent is synonymous with hyperbolic inverse tangent.")
(:p "This function for hyperbolic arc tangent requires an argument and returns the angle whose hyperbolic tangent is the argument you provide.")
)
(:SEE-ALSO TAN ASINH ACOSH)
)



(DOCUMENT-FUNCTION COS
(:SUMMARY "Returns the cosine (cos)")
(:SYNTAX (COS n))
(:PARAMETERS 
(n :VALUE-TYPE number :DOCSTRING "the angle, in radians, you want the cosine of")
)
(:RETURNS " a number")
(:EXAMPLES
"(COS 0)
    --> 1.0"
"(COS 3.14159)
    --> -1.0"
"(COS (/ PI 2))
    --> 6.123233995736766d-17
    (Note: \"d\" indicates exponent.  See Representations of Numbers for more explanation."
"(COS 1.047198)
    --> 0.49999955"
"(COS (/ (* 5 PI) 4))
    --> -0.7071067811865477d0"
)

(:TEXT
(:p "This function returns the cosine of the angle provided in radians.")
(:p "The angle provided is usually between 0 and 2PI. (2PI = 6.28319)")
(:p "The cosine will be between 0 and 1.")
)
(:SEE-ALSO SIN TAN)
)


#+not-needed
(DOCUMENT-FUNCTION COSH
(:SUMMARY "Returns the hyperbolic cosine (cosh)")
(:SYNTAX (COSH n))
(:PARAMETERS 
(n :VALUE-TYPE number :DOCSTRING "the angle you want the hyperbolic cosine of")
)
(:RETURNS " a number")
(:EXAMPLES
"(COSH 0)
    --> 1.0"
"(COSH -1)
    --> 1.5430806"
"(COSH 1.5)
    --> 2.3524096"
)
(:TEXT
(:p "This function returns the hyperbolic cosine of the angle given.")
)
(:SEE-ALSO COS SINH TANH)
)




(DOCUMENT-FUNCTION SIN
(:SUMMARY "Returns the sine (sin)")
(:SYNTAX (SIN n))
(:PARAMETERS 
(n :VALUE-TYPE number :DOCSTRING "the angle, in radians, you want the sine of")
)
(:RETURNS " a number")
(:EXAMPLES
"(SIN 0)
    --> 0.0"
"(SIN (/ pi 2))
    --> 1.0d0"
"(SIN PI)
    --> 1.2246467991473532d-16
   (Note: \"d\" indicates exponent.  See Representations of Numbers for more explanation."
"(SIN .523599)
    --> 0.50000024"
"(SIN (/ (* 2 PI) 3))
    --> 0.8660254037844387d0"
)
(:TEXT
(:p "This function returns the sine of the angle provided in radians.") 
(:p "The angle provided is usually between 0 and 2PI. (2PI = 6.28319)")
(:p "The sine will be between 0 and 1.")
)
(:SEE-ALSO COS TAN)
)


#+not-needed
(DOCUMENT-FUNCTION SINH
(:SUMMARY "Returns the hyperbolic sine (sin)")
(:SYNTAX (SINH n))
(:PARAMETERS 
(n :VALUE-TYPE number :DOCSTRING "the angle you want the hyperbolic sine of")
)
(:RETURNS " a number")
(:EXAMPLES
"(SINH 0)
    --> 0.0"
"(SINH -1)
    --> -1.1752012"
"(SINH .5)
    --> 0.5210953"
"(SINH 2.5)
    --> 6.0502048"
)
(:TEXT
(:p "This function returns the hyperbolic sine of the angle given")
)
(:SEE-ALSO SIN COSH TANH)
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
    (Note: \"d\" indicates exponent.  See Representations of Numbers for more explanation."
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


#+not-needed
(DOCUMENT-FUNCTION TANH
(:SUMMARY "Returns the hyperbolic tangent (tanh)")
(:SYNTAX (TANH n))
(:PARAMETERS 
(n :VALUE-TYPE number :DOCSTRING "the angle you want the hyperbolic tangent of")
)
(:RETURNS " a number")
(:EXAMPLES
"(TANH 0)
    --> 0.0"
"(TANH -1)
    --> -0.7615942"
"(TANH 2.5)
    --> 0.98661435"
)
(:TEXT
(:p "This function returns the hyperbolic tangent of the angle given")
)
(:SEE-ALSO TAN SINH COSH)
)
