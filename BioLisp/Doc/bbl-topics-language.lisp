;;; -*- Package: help; mode: lisp; base: 10; Syntax: Common-Lisp; -*-

(in-package :help)

;;; +=========================================================================+
;;; | Copyright (c) 2002-2006 JP Massar, Jeff Shrager, Peter Seibel           |
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

;;; Authors: Emily Niman, Bogdan Mihai, Arnaud Taton, Jeff Elhai, JP Massar, Peter Seibel
;;;          Sandrine Thominet.    
;;; Jul 11 '12. J.Myers.  Added (namestring) to Windows pathname used in concatenate call. 


(def-topic "How to build a recursive function" 
  (:summary "This page walks through the process of building a recursive function."
			)
  (:text 
  (:TD (:B (:U "Problem Overview")))
   (:p
    #.(one-string-sp
		"We will use as an example of a recursive factorial function."
        "A basic mathematical function, the factorial provides"
		"a good example of a function that can be expressed recursively."
		"The factorial of a number n is the product of the sequence"
		"1 x 2 x 3 x ... x n, or alternatively n x (n-1) x (n-2) x ... x 1."
		"For example, the factorial of 5 is 5! = 5 x 4 x 3 x 2 x 1 = 120.")
		(:BR)(:BR)
	#.(one-string-sp
		"Such a function can be expressed the following way in mathematical"
		"notation:"
	  ))
  (:img :src "/weblistenerdocs/bbldf/images/recursivefactorial0.jpg")
  (:p
    #.(one-string-sp
        "What this tells us is that if the value of n is equal to 0,"
		"the returned result is 1. For any value above 0, we return n"
		"multiplied by the factorial of n-1, which unfolds until it reaches 1:"
		)(:BR)(:BR)
	(:TT (:B "5!")" = 5 x "(:B "4!")(:BR)
	(:B "5!")" = 5 x 4 x "(:B "3!")(:BR)
	(:B "5!")" = 5 x 4 x 3 x "(:B "2!")(:BR)
	(:B "5!")" = 5 x 4 x 3 x 2 x "(:B "1!")(:BR)
	(:B "5!")" = 5 x 4 x 3 x 2 x 1
	"))
(:TD (:B (:U "Implementation")))
(:BR)
(:OL
 (:LI "Define the function")
 (:p
	"This is a tricky step, since in order to define a recursive function, it is necessary to refer to that function before it has been defined. In BioBIKE, functions are always drawn from menus. It is necesssary, "
	"therefore to place a recursive function on the menu before the functionality has been fully described. The solution is to define the function twice -- first partially completed and finally in its completed state." 
	"To implement a recursive factorial function in BioBIKE:" 
(:UL
(:LI
	"Mouse over the"(:B " DEFINITION")
	" menu and click on "(:B "DEFINE-FUNCTION")
	". A "(:B "DEFINE-FUNCTION")
	" box will appear in the workspace. "
)(:BR)
(:LI
	"Click the " (:I "name") " box, and type "  (:TT "recursive-factorial") ". Press Enter."
)(:BR)
(:LI
	"Click the " (:I "arg") " (argument) box, and type " (:TT "n") ". Press Enter."
))

(:p
"BioBIKE requires that functions do " (:B(:I "something")) " before the definition can be accepted, so it is necessary to enter something in the body of the function, such as the number 1."
)
(:UL
(:LI
	"Click the " (:I "form") " box in the " (:B "body") " field, and type " (:TT "1") ". Press Enter."
)(:BR)
(:LI
	"Execute the partially defined function by either double-clicking " (:B "DEFINE-FUNCTION") " or clicking " (:B "Execute") " on the Action Menu (green wedge) of " (:B "DEFINE-FUNCTION") ". Your recursive factorial function should now appear in the "(:B "FUNCTIONS") " menu."
)
)(:BR)
 (:LI "Set the termination conditions")
 (:p
	   "Recursive functions are common in computer science because they allow"
	   " programmers to write efficient programs using a minimal amount of code. "
	   "The downside is that they can cause infinite loops and other unexpected"
	   " results if not written properly. If proper cases are not included in the" 
	   "function to stop the execution, the recursion will repeat forever."
	   " Bring down a "(:B "IF-TRUE") 
	   " box from the "(:B "FLOW-LOGIC")
		" menu, and drag it in the form box that constitutes the body of the function."
		" In our case, we want to know if n is equal to 1. To test this equality, select the "
		"function "(:B "ORDER") 
		" from the "(:B "FLOW-LOGIC") 
		" menu. Enter n in the"
		" first box and 1 in the second box. Select = from the comparison options."
		" Drag the entire function to the condition box of the "(:B "IF-TRUE") 
		" function. In the form box corresponding to then, enter 1. In the form box corresponding"
		" to else, drag the "(:B "PRODUCT-OF") 
		" function from the "(:B "ARITHMETIC")
		" menu. In the first number box, enter n."
		" Now that you have defined the "(:B "RECURSIVE-FACTORIAL")
		" function, you can use it in the body of your function. We want to apply the "(:B "RECURSIVE-FACTORIAL") 
		" function to n-1. To do so, bring down the "(:B "DIFFERENCE-OF") 
		" function from the "(:B "ARITHMETIC")
		" menu."
		" Type in n in the first number box and 1 in the second."
	)
	(:LI "Body of the function")
	(:p
		"Here is what your "(:B "RECURSIVE-FACTORIAL")
		" would look like:"
	  )
	  
 (:img :src "/weblistenerdocs/bbldf/images/recursivefactorial1.jpg")
  (:p
		"Re-execute " (:B "DEFINE-FUNCTION")
		" function."
	)
 (:img :src "/weblistenerdocs/bbldf/images/recursivefactorial2.jpg")
 (:BR)(:BR)
   (:LI "Testing the function")
  (:p
    	"Now, you can test your newly defined recursive factorial function."
		"Let us calculatate the factorial of 5."
  )
 (:img :src "/weblistenerdocs/bbldf/images/recursivefactorial3.jpg")
 (:p
    	"The function returns:"
  )
  (:img :src "/weblistenerdocs/bbldf/images/recursivefactorial4.jpg")
  ))
  )
  (:keywords "define-function" "recursive-function")
  
  (:see-also 
     (DF "bbl:define-function")
	 (glossary "recursive-function")
   ))


(def-topic "How to compare two quantities (numeric vs alphabetical)" 
  (:summary "Different ways of doing comparisons")
  (:text 
    (:p 
      #.(ONE-STRING-SP 
         "Comparisons are useful to determine which logical path to follow"
         "(built into the ")
        ((:A :HREF (:PRINT (help::MAKE-HELP-FUNCTION-DOCUMENTATION-URL :NAME "bbl:if-true")))
                "IF-TRUE")
      #.(ONE-STRING-SP 
         " function). They can also help you pick out of from list the elements you"
         "want to work with (using the ")
        ((:A :HREF (:PRINT (help::MAKE-HELP-FUNCTION-DOCUMENTATION-URL :NAME "bbl:filter")))
                "FILTER")
        " function). The general "
        ((:A :HREF (:PRINT (help::MAKE-HELP-FUNCTION-DOCUMENTATION-URL :NAME "bbl:True?")))
                "TRUE?")
        " function can take care of the many other applications of comparison.")

    (:p 
      #.(ONE-STRING-SP 
         "In all of these cases, numbers may be compared with numbers, strings with strings,"
         "strings with numbers, values with lists, and lists with lists."
         "For the most part, the functions should behave as you intuitively expect,"
         "but there may be cases where it will help to understand in some detail how"
         "the comparisons work."))

     (:p 
       #.(ONE-STRING-SP 
         "For example, we can agree that 110 is greater than 90, but what about the strings \"110\" and \"90\"?"
         "If you think \"110\" should also be greater than \"90\", then what about \"11A\" or \"A11\"?"
         "These are all legal strings, even though they're not legal numbers."
         "BioBIKE provides two sets of comparison operators: one set that performs only numeric"
         "comparisons and another more intelligent set that performs ") (:U "either")
       #.(ONE-STRING-SP 
         " numeric or alphabetical comparisons, depending on what you give it. If you use only"
         "the latter set, you'll almost always get the answer you expect."))

    (:P "For specific examples of comparisons, see the " 
        (:B "Examples")
        " section of "
        ((:A :HREF (:PRINT (help::MAKE-HELP-FUNCTION-DOCUMENTATION-URL :NAME "bbl:True?")))
                "TRUE?"))

    (:P 
      (:TABLE
        (:TR 
          (:TD :VALIGN "top"
            ((:FONT :FACE "Verdana" :SIZE 2)
              (:B "Numeric comparison operators")(:BR)
              #.(ONE-STRING-SP 
                 "The operators shown at the right all have their expected mathematical meanings."
                 "They return T (for true) if the comparison holds (e.g. '2 < 3'), and NIL otherwise,"
                 "unless you provide the operator with a non-number (including strings, such as \"1\","
                 "that look like numbers). In that case, you'll get an error message.")
                (:P "Note that '<=' (less than or equal) is equivalent to 'IS-NOT >'." (:BR) 
                    "Similarly '>=' (greater than or equal) is equivalent to 'IS-NOT <'")
             ))
          (:TD :VALIGN "top" 
            (help::HTML "&nbsp")(help::HTML "&nbsp")(help::HTML "&nbsp")
            (:img :src "/weblistenerdocs/bbldf/images/TRUE-numerical-operators.png" :height "60" :border "0")
            (help::HTML "&nbsp")(help::HTML "&nbsp")(help::HTML "&nbsp"))
        )))

     (:P 
      (:TABLE
        (:TR 
          (:TD :VALIGN "top"
            ((:FONT :FACE "Verdana" :SIZE 2)
              (:B "Alphabetical/Numeric comparison operators")(:BR)
              #.(ONE-STRING-SP 
                 "The behaviors of the operators shown at the right depend on what you give them."
                 "If the two entities to be compared are numbers, then a numeric comparison is performed."
                 "If either of the two entities are strings, then an alphabetical comparison is performed, "
                 "converting any number into a string (e.g. 123 is converted to \"123\")."
                 "As with the numeric operators, the alphanumeric operators are governed by the choice"
                 "of the optional IS or IS-NOT that precedes them.")
                )
          )
          (:TD :VALIGN "top" 
            (help::HTML "&nbsp")(help::HTML "&nbsp")(help::HTML "&nbsp")
            (:img :src "/weblistenerdocs/bbldf/images/TRUE-alphanumeric-operators.png" :height "85" :border "0")
            (help::HTML "&nbsp")(help::HTML "&nbsp")(help::HTML "&nbsp"))
      )))
       
   #.(ONE-STRING-SP 
       "An alphabetical comparison is performed by comparing each character, from left to right,"
       "and applying the following rules:")
 
     (:UL
        (:LI 
          #.(ONE-STRING-SP 
               "Capitalization is disregarded (i.e., \"A\" is the same as \"a\")"
               "unless the CASE-SENSITIVE flag is specified")
            (:BR) (help::HTML "&nbsp"))
 
        (:LI
          #.(ONE-STRING-SP 
              "As soon as the comparison finds a character in the first string"
              "that has a lower numeric representation (according to this ")
              ((:A :HREF "http://www.cdrummond.qc.ca/cegep/informat/professeurs/alain/images/ASCII1.GIF" 
                 :TARGET "_blank") "table")
          #.(ONE-STRING-SP 
              ") than the corresponding character in the other string, the first string is declared"
              "to be less than the second.")
            (:P "Example: \"110\" IS-NOT GREATER-THAN \"90\", because \"1\" is less than \"9\"")
            )
        (:LI
          #.(ONE-STRING-SP 
              "As soon as the comparison finds a character in the first string"
              "that has a higher numeric representation (according to this ")
              ((:A :HREF "http://www.cdrummond.qc.ca/cegep/informat/professeurs/alain/images/ASCII1.GIF" 
                 :TARGET "_blank") "table")
          #.(ONE-STRING-SP 
              ") than the corresponding character in the other string, the first string is declared"
              "to be greater than the second.")
            (:BR) (help::HTML "&nbsp"))
        (:LI
          #.(ONE-STRING-SP 
              "Otherwise, the shorter string is declared to be less than the longer.")
            (:P "Example: \"at\" is LESS-THAN \"atom\"")
            )
        (:LI
          #.(ONE-STRING-SP 
              "If the strings are of the same length, and a character-by-character comparison"
              "shows no difference, then they are declared to be equal."))
        )
        
     (:P (:U "BETWEEN") (:BR)
         "SAME-AS, GREATER-THAN, and LESS-THEN all compare one value with another. However, "
         "BETWEEN is different, comparing a value against a "
         (:B (:I "range")) " of values. The range must be provided as a list, which may consist "
         "of any of the following:"
        (:UL
         (:LI "A minimal and maximal value, in that order. Example: (20 40)" (:BR)
              "The boundaries are not themselves considered matches unless "
              "the INCLUSIVE flag is specified." (:BR)
              "Example: 20 IS BETWEEN (20 40) "
              (:img :src "/weblistenerdocs/img-bbldoc/zrightarrow.gif")
              " NIL" (:BR)
              "Example: 20 IS BETWEEN (20 40) INCLUSIVE "
              (:img :src "/weblistenerdocs/img-bbldoc/zrightarrow.gif")
              " T" 
              (:BR) (help::HTML "&nbsp"))
 
         (:LI "A list of ranges (the order of the ranges doesn't matter). Example: ((50 70) (20 40))"
             (:BR) (help::HTML "&nbsp"))
 
         (:LI "A list of ranges, including single values. Example: ((20 40) 45 (50 70))")
         )
        "Ranges may consist of characters. Example ((\"A\" \"Z\") \"-\" \".\")" (:BR)
        "This set of ranges includes all letters and two punctuation symbols.")
        
     (:P 
     #.(ONE-STRING-SP 
         "These comparison operators work on entities besides numbers and strings -- "
         "in fact, practically anything."))

     (:P "Example: all4312 IS \"a7120.all4312\"" (:BR)
         "Example: all4312 IS LESS-THAN \"B\"")
 )
          
  (:keywords "equal" "greater-than" "less-than" "between" ">" "<" "=" "NOT=" "<=" ">=" "member" "contains")
  
  (:see-also 
     (DF "bbl:TRUE?")
     (DF "bbl:FILTER")
     (DF "bbl:IF-TRUE")
	 (glossary "True and False")
   ))


(def-topic "How to compare two quantities (within strings or lists)" 
  (:summary "How to perform comparisons that act on elements of strings or lists")
  (:text 
    (:P 
      (:TABLE
        (:TR 
          (:TD :VALIGN "top"
            ((:FONT :FACE "Verdana" :SIZE 2)
       "The functions "
       ((:A :HREF (:PRINT (help::MAKE-HELP-FUNCTION-DOCUMENTATION-URL 
                             :NAME "TRUE%3F" :PACKAGE :bbl)))
                "TRUE?") ", "
       ((:A :HREF (:PRINT (help::MAKE-HELP-FUNCTION-DOCUMENTATION-URL 
                             :NAME "IF-TRUE" :PACKAGE :bbl)))
                "IF-TRUE") ", and "
       ((:A :HREF (:PRINT (help::MAKE-HELP-FUNCTION-DOCUMENTATION-URL 
                             :NAME "FILTER" :PACKAGE :bbl)))
                "FILTER")
       " all permit the use of the powerful comparisons shown at the right "
       "that act on aggregates. "
       "They allow you to look inside of lists and strings to determine whether they "
       "have desired characteristics.")
             )
          (:TD :VALIGN "top" 
            (help::HTML "&nbsp")(help::HTML "&nbsp")(help::HTML "&nbsp")
            (:img :src "/weblistenerdocs/bbldf/images/TRUE-aggregate-operators.png" :height "60" :border "1")
            (help::HTML "&nbsp")(help::HTML "&nbsp")(help::HTML "&nbsp"))
        )))

    (:P (:B "CONTAINED-IN")(:BR)
       (:BR)
       "CONTAINED-IN compares two entities (value-1 and value-2), considering two cases:"
       (:UL
         (:LI (:U "Value-2 is a list") ": Is value-1 an element of that list?")
         (:LI (:U "Value-2 is a string") ": Does value-1 lie within that string?"))
       "For example, considering the case where " (:U "value-2 is a list") ":"
       (:UL
         (:LI "3 is CONTAINED-IN in the list (1 2 3 4)" (:BR)
              (:I "(because 3 is an element of the list)")
              (:BR) (help::HTML "&nbsp"))
         (:LI "all4312 is CONTAINED-IN the GENES-OF A7120)"(:BR)
              (:I "(because the gene all4312 is an element of the list of genes)")
              (:BR) (help::HTML "&nbsp"))
         (:LI "\"A\" is CONTAINED-IN the list (\"A\" \"B\")"(:BR)
              (:I "(because \"A\" is an element of the list)")
              (:BR) (help::HTML "&nbsp"))
         (:LI "\"A\" IS-NOT CONTAINED-IN the list (\"AB\" \"CD\")"(:BR)
              (:I "(because \"A\", while an element of the string \"AB\", "
                  "is " (:B (:I "not")) " an element of the list. "
                  "Neither of the two elements of the list is \"A\")")
              (:BR) (help::HTML "&nbsp"))
          )

       "Value-1, which is compared against the list, value-2, may itself "
       "be a list or any other type of entity:"
       (:UL
         (:LI "(1 2) is CONTAINED-IN in the list ((1 2)(3 4))"(:BR)
              (:I "(because (1 2) is an element of the list)")
              (:BR) (help::HTML "&nbsp"))
         (:LI "(1 2) IS-NOT CONTAINED-IN the list (1 2 3 4)" (:BR)
              (:I "(because while each element of (1 2) is an element "
                  "of (1 2 3 4), the list (1 2) itself is " (:B (:I "not")) 
                  " any of the four elements of the list. ")
              (:BR) (help::HTML "&nbsp"))
          )
      )
    (:P "Considering the case where " (:U "value-2 is a string") ":"
       (:UL
         (:LI "\"BC\" is CONTAINED-IN \"ABCD\"" (:BR)
              (:I "(because \"BC\" lies within the string \"ABCD\")")
              (:BR) (help::HTML "&nbsp"))
         (:LI "\"CB\" IS-NOT CONTAINED-IN \"ABCD\"" (:BR)
              (:I "(because \"CB\" -- in that order --  lies nowhere "
                  "within the string \"ABCD\")"))
         )
       "Note that " (:U "order") " is important in the CONTAINED-IN test."
       )

    (:P (:B "A-SUBSET-OF")(:BR)
       (:BR)
       "A-SUBSET-OF compares two entities (value-1 and value-2), considering two cases:"
       (:UL
         (:LI (:U "Value-2 is a list") ": Is value-1 a subset of that list?")
         (:LI (:U "Value-2 is a string") ": Is value-1 or its contained letters "
              "a subset of the letters within value-2?"))
       "Strictly speaking, a single item is not a set and so cannot be a "
       "subset of anything, but for the sake of simplicity, A-SUBSET-OF "
       "considers a single item to be a set consisting of that single item. "
       "For example, the number 3 is considered to be the list (3).")

   (:P "For example, considering the case where " (:U "value-2 is a list") ":"
       (:UL
         (:LI "3 is A-SUBSET-OF the list (1 2 3 4)" (:BR)
              (:I "(because (3) is a subset of the list)")
              (:BR) (help::HTML "&nbsp"))
         (:LI "all4312 is A-SUBSET-OF the GENES-OF A7120)"(:BR)
              (:I "(because the set consisting of the gene all4312 is an "
                  "element of the list of genes)")
              (:BR) (help::HTML "&nbsp"))
         (:LI "(2 1) is A-SUBSET-OF the list (1 2 3 4)"(:BR)
              (:I "(because both 1 and 2 are elements of the list -- "
                  "order doesn't matter)")
              (:BR) (help::HTML "&nbsp"))
         (:LI "(2 1) IS-NOT A-SUBSET-OF the list ((1 2)(3 4))" (:BR)
              (:I "(because neither 1 nor 2 are elements "
                  "of (1 2 3 4)")
              (:BR) (help::HTML "&nbsp"))
         (:LI "((2 1)) " (:B (:I "is")) " A-SUBSET-OF the list ((1 2)(3 4))" (:BR)
              (:I "(because neither the lone element of value-1, (2 1), is "
                  "the first element of the list ((1 2)(3 4))")
              (:BR) (help::HTML "&nbsp"))
         )
     )
    (:P "Considering the case where " (:U "value-2 is a string") ":"
       (:UL
         (:LI "\"BC\" is A-SUBSET-OF \"ABCD\"" (:BR)
              (:I "(because the set (\"B\" \"C\") is a subset of (\"A\" \"B\" \"C\" \"D\"), "
                  "i.e., the letters of \"ABCD\". In other words, both letters, "
                  "\"B\" and \"C\" are letters in the string \"ABCD\")")
              (:BR) (help::HTML "&nbsp"))
         (:LI "\"CB\" is A-SUBSET-OF \"ABCD\"" (:BR)
              (:I "(because both letters, \"B\" and \"C\" are letters in the "
                  "string \"ABCD\" -- order doesn't matter)")
              (:BR) (help::HTML "&nbsp"))
         (:LI "(\"C\" \"B\") is A-SUBSET-OF \"ABCD\"" (:BR)
              (:I "(because both letters, \"B\" and \"C\" are letters in the "
                  "string \"ABCD\" -- order doesn't matter)"))
         )
       "Note that " (:U "order") " is not important in the A-SUBSET-OF test."
       )

    (:P (:B "MATCHED-BY-PATTERN")(:BR)
       (:BR)
       "MATCHED-BY-PATTERN determines whether the string value-1 is matched by the "
       "pattern specified by value-2. The pattern (value-2) must be a string, "
       "and the target (value-1) must be either a string or something (like a gene or protein) "
       "from which a string or sequence can be derived. A description of pattern-matching "
       "can be found in the documentation page of "
       ((:A :HREF (:PRINT (help::MAKE-HELP-FUNCTION-DOCUMENTATION-URL 
                             :NAME "MATCHES-OF-PATTERN" :PACKAGE :bbl)))
                "MATCHES-OF-PATTERN") 
       
       ", and the symbols used in pattern-matching are described in "
       ((:A :HREF "/weblistenerdocs/externaldf/BioBIKE-Pattern-Matching.pdf")
          "BioBIKE Pattern Matching") "."
       )

    (:P "For example:"
      (:UL
        (:LI "\"alr1152\" is MATCHED-BY-PATTERN \"$$$####\"" (:BR)
              (:I "(because the string \"alr1152\" contains, in order, "
                  "three word characters and four numeric characters)")
              (:BR) (help::HTML "&nbsp"))
        (:LI "1152 IS-NOT MATCHED-BY-PATTERN \"####\" (an error is raised)" (:BR)
              (:I "(because 1152 is a number, not a string)")
              (:BR) (help::HTML "&nbsp"))
        (:LI "alr1152 is MATCHED-BY-PATTERN \"ATG[ACGT]...(TAA|TAG|TGA)\"" (:BR)
              (:I "(because the sequence derived from the gene alr1152 happens "
                  "to contain a sequence that begins with ATG, continues with "
                  "some number of A, C, G, or G's, and ends with either a "
                  "\"TAA\", \"TAG\", or \"TGA\" triplet)")) 
       )
    )
 )
          
  (:keywords "member" "substring" "sublist" "pattern" "regular expression")
  
  (:see-also 
     (DF "bbl:TRUE?")
     (DF "bbl:FILTER")
     (DF "bbl:IF-TRUE")
	 (glossary "True and False")
   ))