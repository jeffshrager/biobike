
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

(DOCUMENT-MODULE STATISTICS
  "Basic statistical functions"
  (:keywords :number )
  (:display-modes :bbl)
  (:toplevel? nil)
  #.`(:functions mean median item-of-rank std-dev t-test bin-data-of))

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

;;============================STD-DEV==============

(DOCUMENT-FUNCTION STD-DEV
  (:PARAMETERS
    (list :DOCSTRING "list of numbers"))
  (:EXAMPLES
    "1. (ASSIGN list {1 2 3 4 5})
        (STD-DEV list)
         --> 1.5811388"

    "2. (STD-DEV (RANDOM-INTEGERS FROM 1 TO 100 LIST-OF 10000))
         --> 28.908432  ; (Your answer will be different, but not by much)")
                 
  (:RETURNS "A number")
  (:TEXT 
    (:p (:ul "The result is the standard deviation of the list of numbers."))
    (:p (:ul "Standard deviation is a useful measure of the variability within a set of numbers. The greater the standard deviation, the greater the average distance of points from the mean."))
  ))


