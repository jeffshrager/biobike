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

;;; Author:  Michiko Kato, Arnaud Taton, Bogdan Mihai, Hien Truong, Jeff Elhai


(eval-when (:compile-toplevel :load-toplevel :execute)
  (import '(
            com.biobike.help:document-module
               com.biobike.help:document-function) :bbi))

(DOCUMENT-MODULE STRING-EXTRACTION 
  "Functions that deliver parts of strings"
  (:keywords :string :extract :subseq :substring :part)
  (:display-modes :bbl)
  (:alpha-listing? nil) 
  (:toplevel? nil)  
#.`(:functions first second third fourth fifth sixth seventh eighth ninth tenth last inside-string item items split))


(DOCUMENT-FUNCTION INSIDE-STRING
  (:SUMMARY "Extracts characters from a string")
  
  (:PARAMETERS
    (target :DOCSTRING "the string whose characters will be extracted")
    (randomize :DOCSTRING "randomizes the order of characters of the string")
    (randomized :DOCSTRING "identical to randomize")
    (reverse :DOCSTRING "reverses the order of characters of the string")
    (reversed :DOCSTRING "identical to reverse")
    (from :DOCSTRING "specifies start of extraction")
    (to :DOCSTRING "specifies end of extraction")
    (by :DOCSTRING "extracts every i-th character")
    (length :DOCSTRING "specifies the length of the string extracted")
    (item :DOCSTRING "specifies location(s) of characters extracted")
  )
  
  (:EXAMPLES
    "(ASSIGN x AS 'abcdefg')
      --> abcdefg"
    "(INSIDE-STRING x FROM 2 TO 6)
      --> bcdef"
    "(INSIDE-STRING x REVERSE)
      --> gfedcba"
    "(INSIDE-STRING x RANDOMIZE)
      --> bcagfed"
    "(INSIDE-STRING x BY 2)
      --> aceg"
    "(INSIDE-STRING x FROM 2 TO 4 REVERSE)
      --> fedcb"
    "(INSIDE-STRING x FROM 2 TO 4 RANDOMIZE)
      --> bdcfe"
    "(INSIDE-STRING x FROM 2 LENGTH 3)
       --> bcd"
    "(INSIDE-STRING x TO 5 LENGTH 3)
       --> cde"
    "(INSIDE-STRING x ITEM 3)
       --> c"
    "(INSIDE-STRING x ITEM '(1 3 5)
       --> ace     //[in VPL you don't have to put ' in front of the (...)]"
  )
    
  
  (:TEXT
    
    
    (:p "This function extracts characters from a string.")  
    (:p "The FROM and TO options specifies the positions where string extraction begins and ends.")
    (:p "The REVERSE and REVERSED options are identical and reverse the order of the characters string.  The RANDOMIZE and RANDOMIZED options are identical and randomize the order of the characters of a string.")
    (:p "The BY option allows the user to extract every n-th character.  For example, (INSIDE-STRING x BY 3) extracts every third character.")  
    (:p "The LENGTH option is used to indicate how long a string or sequence is desired.")
    (:p "The ITEM option is used to pick out specific elements within the string.")
    
    (:p "___________________________________________________________ ")
    (:img :src "http://ramsites.net/~biobike/temp/inside-string.JPG")
    (:p "____________________________________________________________")
    (:img :src "http://ramsites.net/~biobike/temp/inside-string2.JPG")
  )  
    
)
  