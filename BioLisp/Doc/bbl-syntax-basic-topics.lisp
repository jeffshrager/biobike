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

;;; Authors:   Emily Niman, Bogdan Mihai, Arnaud Taton, Jeff Elhai, JP Massar, Peter Seibel.

;;======== BASIC SYNTAX ===========   

(def-topic "Basic Syntax"
(:text
(:table :align "middle" :width "80%"
(:tr
(:td (:small
       (:p  "These pages are intended to provide just what you need to know to get started using the BioBIKE Language (BBL). A good way to learn the few lessons contained in these pages is to read a bit at a time and then immediately test your understanding in BioBIKE. It would be best if you had a BioBIKE screen open in a separate window.")

   (:p (:b "Table of Contents"))
   (:ol :type "A"
     (:li ((:a :href (:print (make-help-topic-url :name "Introduction to BioBIKE Syntax")))"Introduction to BioBIKE Syntax"))

     (:ol :type "1"
       (:li "English syntax as a model for the syntax of computer languages")
       (:li "Basic syntax and conventions of BioBIKE Language (BBL)")
       (:LI "How to construct functions")
       (:LI "Some general principles of BioBIKE language")
       (:li "Textual representation of BBL forms"))
     (:BR)
 ;; Simple object types
     (:li ((:a :href (:print (make-help-topic-url 
                   :name "Strings")))
                   "Simple object types") (:I " (old)")) 
     (:ol :type "1"
       (:li "Strings")
       (:li "Numbers")
       (:li "Symbols and variables"))
     (:BR)
     
     (:li ((:a :href (:print (make-help-topic-url :name "The need for data aggregates")))"Data aggregates (lists and tables)") (:I " (old)"))
     (:ol :type "1"
       (:li "The need for data aggregates")
       (:li "Lists")
       (:li "Tables") 
       (:li ((:a :href (:print (make-help-topic-url :name "What are frames")))"Frames")))
     (:BR)
     
     (:li ((:a :href (:print (make-help-topic-url :name "iteration")))"Mapping and Loops"))
     )

(:p "You may also take a look at the " ((:a :href (:print (make-help-modules-url)) :target "blank") "Description of functions") ".") 


)))) 
 
(:table :align "middle" :width "80%" (:b 
(:tr  
(:td  :width "26.7%" :align "left" ((:a :style "text-decoration:none" :href  (:print (make-help-topic-url :name "BioBIKE Listener"))) (:button "Previous Page ")))
(:td :width "26.7%" :align "middle" ((:a :style "text-decoration:none" :href (:print (make-help-topic-url :name "HELP for BBL users"))) (:button "Help for BBL Users")))
(:td :align "right" :width "26.7%" ((:a :style "text-decoration:none" :href (:print (make-help-topic-url :name "Introduction to BioBIKE Syntax")))(:button " Next Page")))))) 
))

;;======== FORMS ===========

(def-topic "Forms"
(:text
(:table :align "middle" :width "80%"
(:tr 
(:td (:small
(:p (:b "C. "(:u "Forms")))
(:p "Consider the various ways sentences may be formed in natural human languages.
Rejoice that in BBL (and in Lisp) there are only two: the "(:u "object") " and the "(:u "function") ".")

(:ol
(:li ((:a :style "color:#0000EE" :href (:print (make-help-topic-url :name "The Object"))) "The Object"))
(:li ((:a :style "color:#0000EE" :href (:print (make-help-topic-url :name "The Function"))) "The Function"))
(:li ((:a :style "color:#0000EE" :href (:print (make-help-topic-url :name "Output of Functions"))) "Output of Functions"))
(:li ((:a :style "color:#0000EE" :href (:print (make-help-topic-url :name "Compound Forms"))) "Compound Forms"))))))) 


;
(:table :align "middle" :width "80%" (:b 
  (:tr  (:td  :width "26.7%" :align "left" ((:a :style "text-decoration:none" :href (:print (make-help-topic-url :name "Textual representation of BBL forms")))(:button " Previous Page")))
(:td :width "26.7%" :align "middle"((:a :style "text-decoration:none" :href (:print (make-help-topic-url :name "Basic Syntax")))(:button "Up to Table of Contents")))
(:td :align "right" :width "26.7%"((:a :style "text-decoration:none" :href (:print (make-help-topic-url :name "The Object")))(:button " Next Page")))) 
   
(:tr (:td :width "26.7%")
(:td :align "middle"  :width "26.7%"((:a :style "text-decoration:none" :href (:print (make-help-topic-url :name "HELP for BBL users"))) (:button "Help for BBL Users")))
(:td :width "26.7%")))) 
)) 



(def-topic "The Object"
(:text
(:table :align "middle" :width "80%"
(:tr 
(:td (:small
(:p (:b "C. "(:u "Forms")))
(:p "Consider the various ways sentences may be formed in natural human languages.
Rejoice that in BBL (and in Lisp) there are only two: the "(:u "object") " and the "(:u "function") ".")

(:ol
(:li ((:a :style "color:#0000EE" :href (:print (make-help-topic-url :name "The Object"))) (:font :color "red" "The Object"))) 
(:li ((:a :style "color:#0000EE" :href (:print (make-help-topic-url :name "The Function"))) "The Function"))
(:li ((:a :style "color:#0000EE" :href (:print (make-help-topic-url :name "Output of Functions"))) "Output of Functions"))
(:li ((:a :style "color:#0000EE" :href (:print (make-help-topic-url :name "Compound Forms"))) "Compound Forms")))

(:p (:b "C1. "(:u "The Object")))
(:p "A lone symbol or value, nothing more. BBL returns the value or the value represented by the symbol.")
(:p "If the object is a value, BBL returns the same value."))
  
  (:p (:ul (:pre (:u "Example:") "  47         " (:i "a number"))))
  (:p (:ul (:pre (:u "Example:") "  \"Hi mom!\"  " (:i "a literal string"))))
  (:p (:ul (:pre (:u "Example:") "  'A         " (:i "a literal symbol"))))

(:small (:p "If it is a variable, BBL returns the value of the variable.")

(:p "Try it! Go to the Listener and enter 47 into the command window and then return.
Then enter x into the command window and then return.")

(:p (:i "(A pause while you go to the BioBIKE Web Listener)"))

(:p)
(:p)
(:p "Your session probably looked a good deal like this:"))

  (:p (:ul (:pre "<1>> 47
:: 47
<2>> x
:: Error: Attempt to take the value of the unbound variable `X'.")))

(:small (:p "Error? Why error? Well, BBL returns the value of a variable only if such a value exists. Try this:"))

  (:p (:ul (:pre "<3>> ("((:a :href (:print (make-help-function-documentation-url :name "DEFINE" :package "bbi"))) "DEFINE")" x AS 29)
:: 29
<4>> x
:: 29")))

(:small 
(:p "Now all should be well.")
(:p "If it is, then you've learned about all you need to know about objects."))
)))

(:table :align "middle" :width "80%" (:b 
  (:tr  (:td  :width "26.7%" :align "left" ((:a :style "text-decoration:none" :href (:print (make-help-topic-url :name "Textual representation of BBL forms")))(:button " Previous Page")))
(:td :width "26.7%" :align "middle"((:a :style "text-decoration:none" :href (:print (make-help-topic-url :name "Basic Syntax")))(:button "Up to Table of Contents")))
(:td :align "right" :width "26.7%"((:a :style "text-decoration:none" :href (:print (make-help-topic-url :name "The Function")))(:button " Next Page")))) 
   
  (:tr (:td :width "26.7%")
(:td :align "middle"  :width "26.7%"((:a :style "text-decoration:none" :href (:print (make-help-topic-url :name "HELP for BBL users"))) (:button "Help for BBL Users")))
(:td :width "26.7%")))) 
))



(def-topic "The Function"
(:text
(:table :align "middle" :width "80%"
(:tr 
(:td (:small
(:p (:b "C. "(:u "Forms")))
(:p "Consider the various ways sentences may be formed in natural human languages.
Rejoice that in BBL (and in Lisp) there are only two: the "(:u "object") " and the "(:u "function") ".")

(:ol
(:li ((:a :style "color:#0000EE" :href (:print (make-help-topic-url :name "The Object"))) "The Object"))
(:li ((:a :style "color:#0000EE" :href (:print (make-help-topic-url :name "The Function"))) (:font :color "red" "The Function"))) 
(:li ((:a :style "color:#0000EE" :href (:print (make-help-topic-url :name "Output of Functions"))) "Output of Functions"))
(:li ((:a :style "color:#0000EE" :href (:print (make-help-topic-url :name "Compound Forms"))) "Compound Forms")))

(:p (:b "C2. "(:u "The Function")))
(:p "The second format can be broken down into four components:")


  (:p (:ul (:li "The opening parenthesis")))
  (:p (:ul (:li "The name of a function")))
  (:p (:ul (:li "The argument list (what the function acts on)")))
  (:p (:ul (:li "The closing parenthesis ")))

(:p "Take a familar function, the sin of x. Forcing it into the above format, sin(x) becomes:"))

  (:p (:ul (:pre "(SIN x)")))

(:small (:p "Or a less familiar function (but one you'll use far more often):"))

  (:p (:ul (:pre "("((:a :href (:print (make-help-function-documentation-url :name "LENGTH-OF" :package "bbi"))) "LENGTH-OF")" all4312)")))

(:small (:p "Try them!")
(:p)
(:p)
(:p "All is well if you got something like:"))

  (:p (:ul (:pre "<5>> (SIN x)
:: -0.6636339
<6>> ("((:a :href (:print (make-help-function-documentation-url :name "LENGTH-OF" :package "bbi"))) "LENGTH-OF")" all4312)
:: 771")))

(:small (:p "(...and if you got an error message for (SIN x), you probably didn't DEFINE x as suggested two screens ago)")

(:p "There you have functions, as simple as 2 + 2!")

(:p "...I mean (+ 2 2). That's right. Even arithmetic is fit into this general syntax. The symbol \"+\" refers to a function no less so than \"sin\" (If you want to see how other arithmetic functions are represented, click "(:u "here")").")

(:p "If you understand the nature of the functions you want to use, the arguments they take, and the nature of the output, you know about all you need to know about BBL syntax."))

)))

(:table :align "middle" :width "80%" (:b 
  (:tr  (:td  :width "26.7%" :align "left" ((:a :style "text-decoration:none" :href (:print (make-help-topic-url :name "The Object")))(:button " Previous Page")))
(:td :width "26.7%" :align "middle"((:a :style "text-decoration:none" :href (:print (make-help-topic-url :name "Basic Syntax")))(:button "Up to Table of Contents")))
(:td :align "right" :width "26.7%"((:a :style "text-decoration:none" :href (:print (make-help-topic-url :name "Output of Functions")))(:button " Next Page")))) 
   
  (:tr (:td :width "26.7%")
(:td :align "middle"  :width "26.7%"((:a :style "text-decoration:none" :href (:print (make-help-topic-url :name "HELP for BBL users"))) (:button "Help for BBL Users")))
(:td :width "26.7%")))) 
))



(def-topic "Output of Functions"
(:text
(:table :align "middle" :width "80%"

(:tr 
(:td (:small
(:p (:b "C. "(:u "Forms")))
(:p "Consider the various ways sentences may be formed in natural human languages.
Rejoice that in BBL (and in Lisp) there are only two: the "(:u "object") " and the "(:u "function") ".")

(:ol
(:li ((:a :style "color:#0000EE" :href (:print (make-help-topic-url :name "The Object"))) "The Object"))
(:li ((:a :style "color:#0000EE" :href (:print (make-help-topic-url :name "The Function"))) "The Function"))
(:li ((:a :style "color:#0000EE" :href (:print (make-help-topic-url :name "Output of Functions"))) (:font :color "red" "Output of Functions"))) 
(:li ((:a :style "color:#0000EE" :href (:print (make-help-topic-url :name "Compound Forms"))) "Compound Forms")))

(:p (:b "C3. "(:u "Output of Functions")))
(:p "Every function outputs one or more values. This is obvious in the case of (SIN x)\, but it is equally true in less obvious situations\:"))

(:p (:ul (:pre "<7>> (PRINT (SIN x))
::
-0.6636339
-0.6636339")))

(:small (:p "Why two numbers\? PRINT\, as you might imagine\, is intended to display whatever value it's given\, and it does so. But apart from what it does\, it is also a function that outputs a value. In another world it might output a message \"Printing accomplished\!\"\, but in this world\, it outputs the value it printed. So that value appears twice. This may strike you as foolish\, but it is a very useful behavior when constructing compound functions (to be discussed shortly)."))

  (:p (:ul (:pre "<8>> ("((:a :href (:print (make-help-function-documentation-url :name "ASSIGN" :package "bbi"))) "ASSIGN")" biggest-length = 0)
:: 0
<9>> (FOR-EACH gene IN ("((:a :href (:print (make-help-function-documentation-url :name "GENES-OF" :package "bbi"))) "GENES-OF")" S6803)
          AS length = ("((:a :href (:print (make-help-function-documentation-url :name "LENGTH-OF" :package "bbi"))) "LENGTH-OF")" gene)
          DO ("((:a :href (:print (make-help-function-documentation-url :name "ASSIGN" :package "bbi"))) "ASSIGN")" biggest-length = (MAX biggest-length length)))
:: NIL")))

(:small (:p "Never mind for the moment what this statement does... it ought to do " (:i "something")"\, and so it might be pretty discouraging to see "(:b "NIL")" emerge as the result. The statement "(:i "did")" do something \-\- it found the length of the biggest gene of Synechocystis PCC 6803 (Try entering "(:b "biggest-length")" at the command line). But the number was "(:i "not")" the output of the function "(:b "FOR-EACH")". The output was " (:b "NIL")".")

(:p "You generally don't have to concern yourself with such issues. However\, appreciating the distinction between the action of a function (also called the side\-effect) and the output of the function may sometimes help you explain otherwise mysterious results."))
)))

(:table :align "middle" :width "80%" (:b 
  (:tr  (:td  :width "26.7%" :align "left" ((:a :style "text-decoration:none" :href (:print (make-help-topic-url :name "The Function")))(:button " Previous Page")))
(:td :width "26.7%" :align "middle"((:a :style "text-decoration:none" :href (:print (make-help-topic-url :name "Basic Syntax")))(:button "Up to Table of Contents")))
(:td :align "right" :width "26.7%"((:a :style "text-decoration:none" :href (:print (make-help-topic-url :name "Compound Forms")))(:button " Next Page")))) 
   
  (:tr (:td :width "26.7%")
(:td :align "middle"  :width "26.7%"((:a :style "text-decoration:none" :href (:print (make-help-topic-url :name "HELP for BBL users"))) (:button "Help for BBL Users")))
(:td :width "26.7%")))) 
))



(def-topic "Compound Forms"
(:text
(:table :align "middle" :width "80%"

(:tr 
(:td (:small
(:p (:b "C. "(:u "Forms")))
(:p "Consider the various ways sentences may be formed in natural human languages.
Rejoice that in BBL (and in Lisp) there are only two: the "(:u "object") " and the "(:u "function") ".")
(:ol
(:li ((:a :style "color:#0000EE" :href (:print (make-help-topic-url :name "The Object"))) "The Object"))
(:li ((:a :style "color:#0000EE" :href (:print (make-help-topic-url :name "The Function"))) "The Function"))
(:li ((:a :style "color:#0000EE" :href (:print (make-help-topic-url :name "Output of Functions"))) "Output of Functions"))
(:li ((:a :style "color:#0000EE" :href (:print (make-help-topic-url :name "Compound Forms"))) (:font :color "red" "Compound Forms")))) 

(:p (:b "C4. "(:u "Compound Forms")))
(:p "The fact that functions output values that may be used by other functions makes it possible for the simple syntax of the language to produce complex thoughts. The following example may give you the idea:"))

  (:p (:ul (:pre "<9>> ("((:a :href (:print (make-help-function-documentation-url :name "ASSIGN" :package "bbi"))) "ASSIGN")" (a b c) = (1 2 1))
:: 1
<10>> (/ (+ b (SQRT (- (* b b) (* 4 a c)))) (* 2 a))
:: 1.0")))

(:small (:p "In this example, the output of the first multiplication function " (:b "(* b b)")", outputting " (:b "b2")", and the second multiplication function " (:b "(* 4 a c)")", outputting " (:b "4ac")", are used by the subtraction function... and so forth. (Note that the Listener has a quirk, only displaying the first value of a multiple assignment -- in reality, all assignments are performed)")

(:p "Unless you enjoy disentangling parentheses, you should seldom have to concern yourself with this kind of complication. Rather you can use indentation to help you sort out what is inside of what. For example:"))

  (:p (:ul (:pre "<11>> ("((:a :href (:print (make-help-function-documentation-url :name "ASSIGN" :package "bbi"))) "ASSIGN")" cog-ids-of-S6803 =
          (FOR-EACH gene IN ("((:a :href (:print (make-help-function-documentation-url :name "GENES-OF" :package "bbi"))) "GENES-OF")" S6803)
               AS cog-id = ("((:a :href (:print (make-help-function-documentation-url :name "COG-ID-OF" :package "bbi"))) "COG-ID-OF")" gene)
               WHEN (EXISTS cog-id)
                  COLLECT cog-id))
:: (" ((:a :href (:print (make-help-topic-url :name "Compound Forms Results1"))) "Results")")")))


  (:p (:p (:pre (:i (:u "Translation") ":
      Assign to the variable named cog-ids-of-S6803 the result of...
          Considering each gene within the set of all genes of Synechocystis PCC 6803
               After extracting the COG-ID from the gene's information... 
               And only when the gene HAS a COG-ID... 
               Putting it in the list"))))

(:small 
(:p "The result of the loop is given over to the function that assigns a value to a variable.")
(:p "If you prefer to work linearly, you can accomplish the same result as follows (taking advantage of the asterisk symbol, which represents the immediately preceding result):"))

  (:p (:ul (:pre "<12>> (FOR-EACH gene IN ("((:a :href (:print (make-help-function-documentation-url :name "GENES-OF" :package "bbi"))) "GENES-OF")" S6803)
               AS cog-id = ("((:a :href (:print (make-help-function-documentation-url :name "COG-ID-OF" :package "bbi"))) "COG-ID-OF")" gene)
               WHEN (EXISTS cog-id)
                  COLLECT cog-id))
:: (" ((:a :href (:print (make-help-topic-url :name "Compound Forms Results2"))) "Results")")


<13>> ("((:a :href (:print (make-help-function-documentation-url :name "ASSIGN" :package "bbi"))) "ASSIGN")" cog-ids-of-S6803 = *)
:: (" ((:a :href (:print (make-help-topic-url :name "Compound Forms Results3"))) "Results")")")))
)))

(:table :align "middle" :width "80%" (:b 
  (:tr  (:td  :width "26.7%" :align "left" ((:a :style "text-decoration:none" :href (:print (make-help-topic-url :name "Output of Functions")))(:button " Previous Page")))
(:td :width "26.7%" :align "middle"((:a :style "text-decoration:none" :href (:print (make-help-topic-url :name "Basic Syntax")))(:button "Up to Table of Contents")))
(:td :align "right" :width "26.7%"((:a :style "text-decoration:none" :href (:print (make-help-topic-url :name "The need for data aggregates")))(:button " Next Page")))) 
   
  (:tr (:td :width "26.7%")
(:td :align "middle"  :width "26.7%"((:a :style "text-decoration:none" :href (:print (make-help-topic-url :name "HELP for BBL users"))) (:button "Help for BBL Users")))
(:td :width "26.7%")))) 
))



(def-topic "Compound Forms Results1"
(:text
(:table :align "middle" :width "80%"

(:tr 
(:td
(:pre "<11>> (ASSIGN cog-ids-of-S6803 =
          (FOR-EACH gene IN (GENES-OF S6803)
               AS cog-id = (COG-ID-OF gene)
               WHEN (EXISTS cog-id)
                  COLLECT cog-id))

:: (\"COG1187\" \"COG0451\" \"COG1089\" \"COG1166\" \"COG0272\" \"COG4636\" \"COG0609\"
 \"COG0609\" \"COG1120\" \"COG0614\" \"COG1629\" \"COG2207\" \"COG0477\" \"COG4759\"
 \"COG0614\" \"COG1629\" \"COG2207\" \"COG0500\" \"COG1629\" \"COG0848\" \"COG0811\"
 \"COG4642\" \"COG1132\" \"COG2207\" \"COG1629\" \"COG0614\" \"COG0614\" \"COG1132\"
 \"COG4634\" \"COG2442\" \"COG1199\" \"COG3335\" \"COG0409\" \"COG1091\" \"COG0225\"
 \"COG0297\" \"COG1309\" \"COG0454\" \"COG1131\" \"COG0500\" \"COG2318\" \"COG0500\"
 \"COG1922\" \"COG1214\" \"COG2755\" \"COG1615\" \"COG1989\" \"COG0563\" \"COG0289\"
 \"COG0526\" \"COG0046\" \"COG0494\" \"COG0845\" \"COG0194\" \"COG0406\" \"COG1819\"
 \"COG0730\" \"COG1413\" \"COG1305\" \"COG0330\" \"COG1530\" \"COG0164\" \"COG0494\"
 \"COG1185\" \"COG0681\" \"COG0281\" \"COG0031\" \"COG1947\" \"COG1401\" \"COG0030\"
 \"COG1385\" \"COG2002\" \"COG3307\" \"COG1104\" \"COG3431\" \"COG1249\" \"COG0582\"
 \"COG0665\" \"COG3335\" \"COG3335\" \"COG0642\" \"COG0147\" \"COG0142\" \"COG1266\"
 \"COG0704\" \"COG0779\" \"COG0195\" \"COG0532\" \"COG0625\" \"COG1523\" \"COG3335\"
 \"COG2875\" \"COG3682\" \"COG0501\" \"COG1225\" \"COG0011\" \"COG0589\" \"COG0123\"
 \"COG0010\" \"COG0652\" ...)")
)))))



(def-topic "Compound Forms Results2"
(:text
(:table :align "middle" :width "80%"
(:tr 
(:td
(:pre "<12>> (FOR-EACH gene IN (GENES-OF S6803)
               AS cog-id = (COG-ID-OF gene)
               WHEN (EXISTS cog-id)
                  COLLECT cog-id)

:: (\"COG1187\" \"COG0451\" \"COG1089\" \"COG1166\" \"COG0272\" \"COG4636\" \"COG0609\"
 \"COG0609\" \"COG1120\" \"COG0614\" \"COG1629\" \"COG2207\" \"COG0477\" \"COG4759\"
 \"COG0614\" \"COG1629\" \"COG2207\" \"COG0500\" \"COG1629\" \"COG0848\" \"COG0811\"
 \"COG4642\" \"COG1132\" \"COG2207\" \"COG1629\" \"COG0614\" \"COG0614\" \"COG1132\"
 \"COG4634\" \"COG2442\" \"COG1199\" \"COG3335\" \"COG0409\" \"COG1091\" \"COG0225\"
 \"COG0297\" \"COG1309\" \"COG0454\" \"COG1131\" \"COG0500\" \"COG2318\" \"COG0500\"
 \"COG1922\" \"COG1214\" \"COG2755\" \"COG1615\" \"COG1989\" \"COG0563\" \"COG0289\"
 \"COG0526\" \"COG0046\" \"COG0494\" \"COG0845\" \"COG0194\" \"COG0406\" \"COG1819\"
 \"COG0730\" \"COG1413\" \"COG1305\" \"COG0330\" \"COG1530\" \"COG0164\" \"COG0494\"
 \"COG1185\" \"COG0681\" \"COG0281\" \"COG0031\" \"COG1947\" \"COG1401\" \"COG0030\"
 \"COG1385\" \"COG2002\" \"COG3307\" \"COG1104\" \"COG3431\" \"COG1249\" \"COG0582\"
 \"COG0665\" \"COG3335\" \"COG3335\" \"COG0642\" \"COG0147\" \"COG0142\" \"COG1266\"
 \"COG0704\" \"COG0779\" \"COG0195\" \"COG0532\" \"COG0625\" \"COG1523\" \"COG3335\"
 \"COG2875\" \"COG3682\" \"COG0501\" \"COG1225\" \"COG0011\" \"COG0589\" \"COG0123\"
 \"COG0010\" \"COG0652\" ...)")
)))))



(def-topic "Compound Forms Results3"
(:text
(:table :align "middle" :width "80%"
(:tr 
(:td
(:pre "<13>> (ASSIGN cog-ids-of-S6803 = *)

:: (\"COG1187\" \"COG0451\" \"COG1089\" \"COG1166\" \"COG0272\" \"COG4636\" \"COG0609\"
 \"COG0609\" \"COG1120\" \"COG0614\" \"COG1629\" \"COG2207\" \"COG0477\" \"COG4759\"
 \"COG0614\" \"COG1629\" \"COG2207\" \"COG0500\" \"COG1629\" \"COG0848\" \"COG0811\"
 \"COG4642\" \"COG1132\" \"COG2207\" \"COG1629\" \"COG0614\" \"COG0614\" \"COG1132\"
 \"COG4634\" \"COG2442\" \"COG1199\" \"COG3335\" \"COG0409\" \"COG1091\" \"COG0225\"
 \"COG0297\" \"COG1309\" \"COG0454\" \"COG1131\" \"COG0500\" \"COG2318\" \"COG0500\"
 \"COG1922\" \"COG1214\" \"COG2755\" \"COG1615\" \"COG1989\" \"COG0563\" \"COG0289\"
 \"COG0526\" \"COG0046\" \"COG0494\" \"COG0845\" \"COG0194\" \"COG0406\" \"COG1819\"
 \"COG0730\" \"COG1413\" \"COG1305\" \"COG0330\" \"COG1530\" \"COG0164\" \"COG0494\"
 \"COG1185\" \"COG0681\" \"COG0281\" \"COG0031\" \"COG1947\" \"COG1401\" \"COG0030\"
 \"COG1385\" \"COG2002\" \"COG3307\" \"COG1104\" \"COG3431\" \"COG1249\" \"COG0582\"
 \"COG0665\" \"COG3335\" \"COG3335\" \"COG0642\" \"COG0147\" \"COG0142\" \"COG1266\"
 \"COG0704\" \"COG0779\" \"COG0195\" \"COG0532\" \"COG0625\" \"COG1523\" \"COG3335\"
 \"COG2875\" \"COG3682\" \"COG0501\" \"COG1225\" \"COG0011\" \"COG0589\" \"COG0123\"
 \"COG0010\" \"COG0652\" ...)")
)))))


;;========= DATA AGGREGATES=========

(def-topic "The need for data aggregates"
(:text
(:table :align "middle" :width "80%"
(:tr 
(:td (:small
(:p (:b "D. "(:u "Data Aggregates (lists and tables)")))

(:ol
(:li ((:a :style "color:#0000EE" :href (:print (make-help-topic-url :name "The need for data aggregates"))) (:font :color "red" "The need for data aggregates"))) 
(:li ((:a :style "color:#0000EE" :href (:print (make-help-topic-url :name "Strings"))) "Strings"))
(:li ((:a :style "color:#0000EE" :href (:print (make-help-topic-url :name "Lists"))) "Lists")) 
(:ol :type "a"  
  (:li ((:a :style "color:#0000EE" :href (:print (make-help-topic-url :name "Simple lists and how to make them"))) "Simple lists - how to make them"))
  (:li ((:a :style "color:#0000EE" :href (:print (make-help-topic-url :name "Complex Lists"))) "Complex Lists"))
  (:li ((:a :style "color:#0000EE" :href (:print (make-help-topic-url :name "Accessing information from lists"))) "Accessing information from lists"))
  (:li ((:a :style "color:#0000EE" :href (:print (make-help-topic-url :name "Sets"))) "Sets"))) 
(:li ((:a :style "color:#0000EE" :href (:print (make-help-topic-url :name "Tables"))) "Tables")))

(:p (:b "D1. "(:u "The need for data aggregates")))
(:p "It is extraordinarily helpful to be able to refer to aggregates of data. Suppose you want to extract the upstream region of cpcB (the first gene of the operon devoted to formation of phycocyanin) from all cyanobacteria. You've found the names of all available cpcB genes (here's "((:a :href (:print (make-help-topic-url :name "Find desired genes using a keyword search of gene descriptions"))) "how you could have done it")"). Now, you could get the sequences this way, one tedious gene at a time:"))

 (:p (:ul (:pre "("((:a :href (:print (make-help-function-documentation-url :name "DEFINE" :package "bbi"))) "DEFINE")" upstream-of-A7120-cpcB AS (SEQUENCE-UPSTREAM-OF alr0528))
:: \"GGAACAGAGAGCAAGGGGGAGAAACTTAAAGATTTCCCCAATCCCAATTCCTCATCTCTCATCCCTGG...\"

("((:a :href (:print (make-help-function-documentation-url :name "DEFINE" :package "bbi"))) "DEFINE")" upstream-of-S6803-cpcB AS (SEQUENCE-UPSTREAM-OF sll1577))
:: \"GACTAAAAAAAGACTTGAATGTCACTAACTACATCCAGTCTTTGCCATGGCCAGTCTTTCCACCAGAG...\"

("((:a :href (:print (make-help-function-documentation-url :name "DEFINE" :package "bbi"))) "DEFINE")" upstream-of-Npun-cpcB AS (SEQUENCE-UPSTREAM-OF npf5289))
:: \"TAACCACAAGTTTGCAAAATTACAGATATACCTGTATAAATCAAAAAACTACGGCAATTTTATCAAAA...\"
...")))

(:small (:p "... and on and on. What is merely tedious here is a practical impossibility if you want to apply an operation to, say, every gene in an organism!")
(:p "You can do operations one item at a time, but it is much more satisfying do them all at once. Here's how the operations could be combined:")) 

(:ul (:pre "(DEFINE cpcB-orthologs AS {alr0528 sll1577 npf5289 Av?5998 Te?5392})
:: (ALR0528 SLL1577 NPF5289 AV?5998 TE?5392)

(SEQUENCES-UPSTREAM-OF cpcB-orthologs)
:: (\"GGAACAGAGAGCAAGGGGGAGAAACTTAAAGATTTCCCCAATCCCAATTCCTCATCTCTCATCC...\"
    \"GACTAAAAAAAGACTTGAATGTCACTAACTACATCCAGTCTTTGCCATGGCCAGTCTTTCCACC...\"
    \"TAACCACAAGTTTGCAAAATTACAGATATACCTGTATAAATCAAAAAACTACGGCAATTTTATC...\"
    ...)")
)))) 

(:table :align "middle" :width "80%" (:b 
(:tr  
(:td  :width "26.7%" :align "left" ((:a :style "text-decoration:none" :href (:print (make-help-topic-url :name "Compound Forms")))(:button " Previous Page")))
(:td :width "26.7%" :align "middle"((:a :style "text-decoration:none" :href (:print (make-help-topic-url :name "Basic Syntax")))(:button "Up to Table of Contents")))
(:td :align "right" :width "26.7%"((:a :style "text-decoration:none" :href (:print (make-help-topic-url :name "Strings")))(:button " Next Page")))) 
   
(:tr 
(:td :width "26.7%")
(:td :align "middle"  :width "26.7%"((:a :style "text-decoration:none" :href (:print (make-help-topic-url :name "HELP for BBL users"))) (:button "Help for BBL Users")))
(:td :width "26.7%")))) 
))


(def-topic "Strings"
(:text
(:table :align "middle" :width "80%"
(:tr 
(:td (:small
(:p (:b "D. "(:u "Data Aggregates (lists and tables)")))

(:ol
(:li ((:a :style "color:#0000EE" :href (:print (make-help-topic-url :name "The need for data aggregates"))) "The need for data aggregates")) 
(:li ((:a :style "color:#0000EE" :href (:print (make-help-topic-url :name "Strings"))) (:font :color "red" "Strings"))) 
(:li ((:a :style "color:#0000EE" :href (:print (make-help-topic-url :name "Lists"))) "Lists")) 
(:ol :type "a"  
  (:li ((:a :style "color:#0000EE" :href (:print (make-help-topic-url :name "Simple lists and how to make them"))) "Simple lists - how to make them"))
  (:li ((:a :style "color:#0000EE" :href (:print (make-help-topic-url :name "Complex Lists"))) "Complex Lists"))
  (:li ((:a :style "color:#0000EE" :href (:print (make-help-topic-url :name "Accessing information from lists"))) "Accessing information from lists"))
  (:li ((:a :style "color:#0000EE" :href (:print (make-help-topic-url :name "Sets"))) "Sets"))) 
(:li ((:a :style "color:#0000EE" :href (:print (make-help-topic-url :name "Tables"))) "Tables")))

(:p (:b "D2. "(:u "Strings")))

(:p "Strings are collections of characters in a given order. They are delimited by double quotation marks. Here are examples of strings:")) 
(:ul (:pre "\"Hello there\" 
(SEQUENCE-OF ssr1600) --> \"TTGACGGTCAGTTTGCGC...\""))))) 


(:table :align "middle" :width "80%" (:b 
  (:tr  (:td  :width "26.7%" :align "left" ((:a :style "text-decoration:none" :href (:print (make-help-topic-url :name "The need for data aggregates")))(:button " Previous Page")))
(:td :width "26.7%" :align "middle" ((:a :style "text-decoration:none" :href (:print (make-help-topic-url :name "Basic Syntax")))(:button "Up to Table of Contents")))
(:td :align "right" :width "26.7%" ((:a :style "text-decoration:none" :href (:print (make-help-topic-url :name "Lists")))(:button " Next Page")))) 
(:tr (:td :width "26.7%")
(:td :align "middle"  :width "26.7%"((:a :style "text-decoration:none" :href (:print (make-help-topic-url :name "HELP for BBL users"))) (:button "Help for BBL Users")))
(:td :width "26.7%")))) 
)) 



;;=========DATA AGGREGATES CONT'D with C2 LISTS===============      

(def-topic "Lists"
(:text
(:table :align "middle" :width "80%"
(:tr 
(:td (:small
(:p (:b "D. "(:u "Data Aggregates (lists and tables)")))

(:ol
(:li ((:a :style "color:#0000EE" :href (:print (make-help-topic-url :name "The need for data aggregates"))) "The need for data aggregates")) 
(:li ((:a :style "color:#0000EE" :href (:print (make-help-topic-url :name "Strings"))) "Strings"))
(:li ((:a :style "color:#0000EE" :href (:print (make-help-topic-url :name "Lists"))) (:font :color "red" "Lists"))) 
(:ol :type "a"  
  (:li ((:a :style "color:#0000EE" :href (:print (make-help-topic-url :name "Simple lists and how to make them"))) "Simple lists - how to make them"))
  (:li ((:a :style "color:#0000EE" :href (:print (make-help-topic-url :name "Complex Lists"))) "Complex Lists"))
  (:li ((:a :style "color:#0000EE" :href (:print (make-help-topic-url :name "Accessing information from lists"))) "Accessing information from lists"))
  (:li ((:a :style "color:#0000EE" :href (:print (make-help-topic-url :name "Sets"))) "Sets"))) 
(:li ((:a :style "color:#0000EE" :href (:print (make-help-topic-url :name "Tables"))) "Tables"))) 

(:p (:b "D3. "(:u "Lists"))) 

(:p "Lists are collections of things (any type). They are delimited by curly brackets. Here are some examples of lists:")) 

(:ul (:pre "{Npun Avar A7120}
{\"A\" 1 \"B\" 2 \"C\" 3}
(ITEMS {.From .To .Direction} IN-EACH {all4312 npf0001 ssr1600})
--> ((5166997 5167767 :B) (167 1546 :F) (2024126 2024476 :F))")) 
(:small (:p "When lists are displayed by BioBIKE as the result of a function, they are delimited by parentheses rather than curly brackets. While you can't have strings within strings, you can have lists within lists, as shown in the result of the last example."))))) 



(:table :align "middle" :width "80%" (:b 
  (:tr  (:td  :width "26.7%" :align "left" ((:a :style "text-decoration:none" :href (:print (make-help-topic-url :name "Strings")))(:button " Previous Page")))
(:td :width "26.7%" :align "middle" ((:a :style "text-decoration:none" :href (:print (make-help-topic-url :name "Basic Syntax")))(:button "Up to Table of Contents")))
(:td :align "right" :width "26.7%" ((:a :style "text-decoration:none" :href (:print (make-help-topic-url :name "Simple lists and how to make them")))(:button " Next Page")))) 
(:tr (:td :width "26.7%")
(:td :align "middle"  :width "26.7%"((:a :style "text-decoration:none" :href (:print (make-help-topic-url :name "HELP for BBL users"))) (:button "Help for BBL Users")))
(:td :width "26.7%")))) 
))



(def-topic "Simple lists and how to make them"
(:text
(:table :align "middle"  :width "80%"
(:tr 
(:td (:small
(:p (:b "D. "(:u "Data Aggregates (lists and tables)")))

(:ol
(:li ((:a :style "color:#0000EE" :href (:print (make-help-topic-url :name "The need for data aggregates"))) "The need for data aggregates")) 
(:li ((:a :style "color:#0000EE" :href (:print (make-help-topic-url :name "Strings"))) "Strings"))
(:li ((:a :style "color:#0000EE" :href (:print (make-help-topic-url :name "Lists"))) "Lists")) 
(:ol :type "a"  
  (:li ((:a :style "color:#0000EE" :href (:print (make-help-topic-url :name "Simple lists and how to make them"))) (:font :color "red" "Simple lists - how to make them"))) 
  (:li ((:a :style "color:#0000EE" :href (:print (make-help-topic-url :name "Complex Lists"))) "Complex Lists"))
  (:li ((:a :style "color:#0000EE" :href (:print (make-help-topic-url :name "Accessing information from lists"))) "Accessing information from lists"))
  (:li ((:a :style "color:#0000EE" :href (:print (make-help-topic-url :name "Sets"))) "Sets"))) 
  (:li ((:a :style "color:#0000EE" :href (:print (make-help-topic-url :name "Tables"))) "Tables")))

(:p (:b "D3. "(:u "Lists")))
(:p (:b "D3.a. "(:u "Simples lists and how to make them")))
(:p "Lists are the most common data aggregate you'll encounter in BBL.")
(:p "There are two methods you'll generally use to make lists:")
  (:p (:ul (:li "Explicit reference ")))
  (:p (:ul (:li "Functions producing lists ")))
(:p "Suppose you have a set of favorite genes. You can make their names into a list in the following way:"))

  (:p (:ul (:pre "("((:a :href (:print (make-help-function-documentation-url :name "DEFINE" :package "bbi"))) "DEFINE")" favorite-gene-names AS \{\"psbA\" \"psbB\" \"psbC\"\} )
:: (\"psbA\" \"psbB\" \"psbC\")")))

(:small (:p "Notice that the list in the "((:a :href (:print (make-help-function-documentation-url :name "DEFINE" :package "bbi"))) "DEFINE")" function is enclosed by curly brackets, distinguishing it from functions, which are enclosed by parentheses. (In pure Lisp, both are enclosed by parentheses, and it is this blurring of the line between function and data that gives Lisp its special flavor, but that's another story).")

(:p "You could also have produced a list of genes through a function that returns a list, for example:"))

  (:p (:ul (:pre "("((:a :href (:print (make-help-function-documentation-url :name "DEFINE" :package "bbi"))) "DEFINE")" favorite-family AS (ORTHOLOGS-OF all0041))
:: (\#\$P9313.PMT2033 \#\$Tery.Te\?2171 \#\$A7120.all0041) ")))
)))

(:table :align "middle" :width "80%" (:b 
  (:tr  (:td  :width "26.7%" :align "left" ((:a :style "text-decoration:none" :href (:print (make-help-topic-url :name "Lists")))(:button " Previous Page")))
(:td :width "26.7%" :align "middle"((:a :style "text-decoration:none" :href (:print (make-help-topic-url :name "Basic Syntax")))(:button "Up to Table of Contents")))
(:td :align "right" :width "26.7%"((:a :style "text-decoration:none" :href (:print (make-help-topic-url :name "Complex Lists")))(:button " Next Page")))) 
   
  (:tr (:td :width "26.7%")
(:td :align "middle"  :width "26.7%"((:a :style "text-decoration:none" :href (:print (make-help-topic-url :name "HELP for BBL users"))) (:button "Help for BBL Users")))
(:td :width "26.7%")))) 
))



(def-topic "Complex Lists"
(:text
(:table :align "middle" :width "80%"
(:tr 
(:td (:small
(:p (:b "D. "(:u "Data Aggregates (lists and tables)")))

(:ol
(:li ((:a :style "color:#0000EE" :href (:print (make-help-topic-url :name "The need for data aggregates"))) "The need for data aggregates")) 
(:li ((:a :style "color:#0000EE" :href (:print (make-help-topic-url :name "Strings"))) "Strings"))
(:li ((:a :style "color:#0000EE" :href (:print (make-help-topic-url :name "Lists"))) "Lists")) 
(:ol :type "a"  
  (:li ((:a :style "color:#0000EE" :href (:print (make-help-topic-url :name "Simple lists and how to make them"))) "Simple lists - how to make them"))
  (:li ((:a :style "color:#0000EE" :href (:print (make-help-topic-url :name "Complex Lists"))) (:font :color "red" "Complex Lists"))) 
  (:li ((:a :style "color:#0000EE" :href (:print (make-help-topic-url :name "Accessing information from lists"))) "Accessing information from lists"))
  (:li ((:a :style "color:#0000EE" :href (:print (make-help-topic-url :name "Sets"))) "Sets"))) 
(:li ((:a :style "color:#0000EE" :href (:print (make-help-topic-url :name "Tables"))) "Tables")))

(:p (:b "D3. "(:u "Lists")))
(:p (:b "D3.b. "(:u "Complex Lists")))
(:p "If you wanted to get to a gene from its gene name\, you could do this\:"))

(:ul (:pre "("((:a :href (:print (make-help-function-documentation-url :name "GENES-DESCRIBED-BY" :package "bbi"))) "GENES-DESCRIBED-BY")" \"psbA\" IN A7120)
:: (" (:u "\#\$A7120.all3572")" "(:u "\#\$A7120.alr3727") " "(:u "\#\$A7120.alr3742") " "(:u "\#\$A7120.alr4592")"
"(:u "\#\$A7120.alr4866") ")")) 

(:small (:p "In general\, if a BBL function allows you to do it with one thing\, you can also do it with a list of things. So to get a list of genes from a list of gene-names\:")) 

(:ul (:pre "("((:a :href (:print (make-help-function-documentation-url :name "GENES-DESCRIBED-BY" :package "bbi"))) "GENES-DESCRIBED-BY")" favorite-gene-names IN A7120)
:: ((" (:u "\#\$A7120.all3572")" "(:u "\#\$A7120.alr3727") " "(:u "\#\$A7120.alr3742") " "(:u "\#\$A7120.alr4592")"
"(:u "\#\$A7120.alr4866")") (" (:u "\#\$A7120.all0138") " "(:u "\#\$A7120.all4000") " "(:u "\#\$A7120.all4002")"
"(:u "\#\$A7120.all4003") " "(:u "\#\$A7120.alr4291")"))")) 

(:small (:p "This may seem a result\, but if you mentally reformat it, with each left-parenthesis representing a rightward margin-shift and each right-parenthesis representing a left-ward margin-shift\, it makes much more sense:"))

(:ul (:pre "( ; THE LIST 
     ( ; psbA
        " (:u "\#\$A7120.all3572")" "(:u "\#\$A7120.alr3727") " "(:u "\#\$A7120.alr3742") " "(:u "\#\$A7120.alr4592")"
        "(:u "\#\$A7120.alr4866")")
     ( ; psbB
        "(:u "\#\$A7120.all0138")")
     ( ; psbC
        "(:u "\#\$A7120.all4000") " "(:u "\#\$A7120.all4002")" "(:u "\#\$A7120.all4003") " "(:u "\#\$A7120.alr4291")")")) 

(:small (:p "A still more complex example:"))

(:ul (:pre "("((:a :href (:print (make-help-function-documentation-url :name "GENES-DESCRIBED-BY" :package "bbi"))) "GENES-DESCRIBED-BY")" favorite-gene-designations IN (A7120 Npun S6803))
:: (((" (:u "\#\$A7120.all3572")" "(:u "\#\$A7120.alr3727") " "(:u "\#\$A7120.alr3742") " " (:u "\#\$A7120.alr4592")" 
"(:u "\#\$A7120.alr4866")") ("(:u "\#\$A7120.all0138")") ("(:u "\#\$A7120.all4000") " " (:u "\#\$A7120.all4002")"
"(:u "\#\$A7120.all4003") " "(:u "\#\$A7120.alr4291")")) (NIL (" (:u "\#\$Npun.NpR2471") ") (" (:u "\#\$Npun.NpR3636") "))
((" (:u "\#\$S6803.slr1311") " "(:u "\#\$S6803.sll1867") " " (:u "\#\$S6803.slr1181") ") (" (:u "\#\$S6803.slr0906") ")
(" (:u "\#\$S6803.sll0851") " " (:u "\#\$S6803.sll0247") ")))")) 

(:small (:p "becomes after mental formatting the outline shown " ((:a :href (:print (make-help-topic-url :name "Complex lists - here"))) "here")".")

(:p "Lists provide a powerful way of organizing information\, but without the reformatting\, the jumble of parentheses can be very confusing."))
)))

(:table :align "middle" :width "80%" (:b 
  (:tr  (:td  :width "26.7%" :align "left" ((:a :style "text-decoration:none" :href (:print (make-help-topic-url :name "Simple lists and how to make them")))(:button " Previous Page")))
(:td :width "26.7%" :align "middle"((:a :style "text-decoration:none" :href (:print (make-help-topic-url :name "Basic Syntax")))(:button "Up to Table of Contents")))
(:td :align "right" :width "26.7%"((:a :style "text-decoration:none" :href (:print (make-help-topic-url :name "Accessing information from lists")))(:button " Next Page")))) 
   
  (:tr (:td :width "26.7%")
(:td :align "middle"  :width "26.7%"((:a :style "text-decoration:none" :href (:print (make-help-topic-url :name "HELP for BBL users"))) (:button "Help for BBL Users")))
(:td :width "26.7%")))) 
))



(def-topic "Complex lists - here"
(:text
(:table :align "middle" :width "80%"
(:tr 
(:td
(:pre "( ; THE LIST 
    ( ; Anabaena PCC 7120
        ( ; psbA
            #$A7120.all3572 #$A7120.alr3727 #$A7120.alr3742 #$A7120.alr4592
            #$A7120.alr4866)
        ( ; psbB
            #$A7120.all0138)
        ( ; psbC
            #$A7120.all4000 #$A7120.all4002 #$A7120.all4003 #$A7120.alr4291))
    ( ; Nostoc punctiforme
        ( ; psbA
            NIL)
        ( ; psbB
            #$Npun.NpR2471)
        ( ; psbC
            #$Npun.NpR3636))
    ( ; Synechocystis PCC 6803
        ( ; psbA
            #$S6803.slr1311 #$S6803.sll1867 #$S6803.slr1181)
        ( ; psbB
            #$S6803.slr0906)
        ( ; psbC
            #$S6803.sll0851 #$S6803.sll0247)))

[Note: The annotation of the Nostoc psb genes undoubtedly is incomplete]")
)))))



(def-topic "Accessing information from lists"
(:text
(:table :align "middle" :width "80%"
(:tr 
(:td (:small
(:p (:b "D. "(:u "Data Aggregates (lists and tables)")))

(:ol
(:li ((:a :style "color:#0000EE" :href (:print (make-help-topic-url :name "The need for data aggregates"))) "The need for data aggregates")) 
(:li ((:a :style "color:#0000EE" :href (:print (make-help-topic-url :name "Strings"))) "Strings"))
(:li ((:a :style "color:#0000EE" :href (:print (make-help-topic-url :name "Lists"))) "Lists")) 
(:ol :type "a"  
  (:li ((:a :style "color:#0000EE" :href (:print (make-help-topic-url :name "Simple lists and how to make them"))) "Simple lists - how to make them"))
  (:li ((:a :style "color:#0000EE" :href (:print (make-help-topic-url :name "Complex Lists"))) "Complex Lists"))
  (:li ((:a :style "color:#0000EE" :href (:print (make-help-topic-url :name "Accessing information from lists"))) (:font :color "red" "Accessing information from lists"))) 
  (:li ((:a :style "color:#0000EE" :href (:print (make-help-topic-url :name "Sets"))) "Sets"))) 
(:li ((:a :style "color:#0000EE" :href (:print (make-help-topic-url :name "Tables"))) "Tables")))   

(:p (:b "D3. "(:u "Lists")))
(:p (:b "D3.c. "(:u "Accessing information from lists")))

(:p "Many tools are provided to get information from lists, but direct extraction is simply accomplished by specifying the desired elements within square brackets. Here are simple examples:"))

(:ul (:pre "("((:a :href (:print (make-help-function-documentation-url :name "DEFINE" :package "bbi"))) "DEFINE")" test-list AS ("((:a :href (:print (make-help-function-documentation-url :name "SPLIT" :package "bbi"))) "SPLIT")" \"ABCDEFGHIJ\"))
:: (\"A\" \"B\" \"C\" \"D\" \"E\" \"F\" \"G\" \"H\" \"I\" \"J\")

test-list[3]
:: \"C\"

test-list[5 9]
:: (\"E\" \"I\")

test-list[(FROM 2 TO 5)]
:: (\"B\" \"C\" \"D\" \"E\")"))

(:small (:p "Here are some biological examples:"))

(:ul (:pre "*all-organisms*
:: (\#\$synechococcus_elongatus_pcc7942 \#\$prochlorococcus_marinus_mit9313
    \#\$prochlorococcus_marinus_ss120 \#\$synechococcus_wh8102
    \#\$gloeobacter_violaceus_pcc7421 \#\$thermosynechococcus_elongatus_bp1
    \#\$prochlorococcus_marinus_med4 \#\$crocosphaera_watsonii_wh8501
    \#\$trichodesmium_erythraeum \#\$anabaena_variabilis_atcc29413
    \#\$anabaena_pcc7120 \#\$nostoc_punctiforme_atcc29133
    \#\$synechocystis_pcc6803)

("((:a :href (:print (make-help-function-documentation-url :name "DEFINE" :package "bbi"))) "DEFINE")" marine-cb AS *all-organisms*[2 3 4 7 8 9])
:: (\#\$prochlorococcus_marinus_mit9313 \#\$prochlorococcus_marinus_ss120
    \#\$synechococcus_wh8102 \#\$prochlorococcus_marinus_med4
    \#\$crocosphaera_watsonii_wh8501 \#\$trichodesmium_erythraeum)

("((:a :href (:print (make-help-function-documentation-url :name "REPLICONS-OF" :package "bbi"))) "REPLICONS-OF")" A7120)
:: (\#\$A7120.CHROMOSOME \#\$A7120.pALPHA \#\$A7120.pBETA \#\$A7120.pGAMMA
    \#\$A7120.pDELTA \#\$A7120.pEPSILON \#\$A7120.pZETA)

("((:a :href (:print (make-help-function-documentation-url :name "DEFINE" :package "bbi"))) "DEFINE")" plasmids AS (REPLICONS-OF A7120)[2 -> 6] )
:: (\#\$A7120.pALPHA \#\$A7120.pBETA \#\$A7120.pGAMMA \#\$A7120.pDELTA 
    \#\$A7120.pEPSILON \#\$A7120.pZETA)"))
)))

(:table :align "middle" :width "80%" (:b 
  (:tr  (:td  :width "26.7%" :align "left" ((:a :style "text-decoration:none" :href (:print (make-help-topic-url :name "Complex lists")))(:button " Previous Page")))
(:td :width "26.7%" :align "middle"((:a :style "text-decoration:none" :href (:print (make-help-topic-url :name "Basic Syntax")))(:button "Up to Table of Contents")))
(:td :align "right" :width "26.7%"((:a :style "text-decoration:none" :href (:print (make-help-topic-url :name "Sets")))(:button " Next Page")))) 
   
  (:tr (:td :width "26.7%")
(:td :align "middle"  :width "26.7%"((:a :style "text-decoration:none" :href (:print (make-help-topic-url :name "HELP for BBL users"))) (:button "Help for BBL Users")))
(:td :width "26.7%")))) 
))



(def-topic "Sets"
(:text
(:table :align "middle" :width "80%"
(:tr 
(:td (:small
(:p (:b "D. "(:u "Data Aggregates (lists and tables)")))

(:ol
(:li ((:a :style "color:#0000EE" :href (:print (make-help-topic-url :name "The need for data aggregates"))) "The need for data aggregates")) 
(:li ((:a :style "color:#0000EE" :href (:print (make-help-topic-url :name "Strings"))) "Strings"))
(:li ((:a :style "color:#0000EE" :href (:print (make-help-topic-url :name "Lists"))) "Lists")) 
(:ol :type "a"  
  (:li ((:a :style "color:#0000EE" :href (:print (make-help-topic-url :name "Simple lists and how to make them"))) "Simple lists - how to make them"))
  (:li ((:a :style "color:#0000EE" :href (:print (make-help-topic-url :name "Complex Lists"))) "Complex Lists"))
  (:li ((:a :style "color:#0000EE" :href (:print (make-help-topic-url :name "Accessing information from lists"))) "Accessing information from lists"))
  (:li ((:a :style "color:#0000EE" :href (:print (make-help-topic-url :name "Sets"))) (:font :color "red" "Sets")))) 
(:li ((:a :style "color:#0000EE" :href (:print (make-help-topic-url :name "Tables"))) "Tables"))) 

(:p (:b "D3. "(:u "Lists")))
(:p (:b "D3.d. "(:u "Sets")))

(:p "Sets are lists where the order of the items is unimportant. For example\, a list of the proteins of Synechocystis might be thought of as a set. Internally\, there is no distinction between \"lists\" and \"sets\".")

(:p "The language provides many functions that act on sets. Here's an example: "))

(:ul (:pre "("((:a :href (:print (make-help-function-documentation-url :name "DEFINE" :package "bbi"))) "DEFINE")" set-of-het-cb-genes AS
    ("((:a :href (:print (make-help-function-documentation-url :name "GENES-NAMED" :package "bbi"))) "GENES-NAMED")" ("((:a :href (:print (make-help-function-documentation-url :name "COMMON-ORTHOLOGS-OF" :package "bbi"))) "COMMON-ORTHOLOGS-OF")" (A7120 Avar Npun))))
:: (" (:i "genes common to three heterocystous cyanobacteria")")

("((:a :href (:print (make-help-function-documentation-url :name "DEFINE" :package "bbi"))) "DEFINE")" set-of-hypotheticals AS
    ("((:a :href (:print (make-help-function-documentation-url :name "GENES-DESCRIBED-BY" :package "bbi"))) "GENES-DESCRIBED-BY")" \"hypothetical\" IN A7120))
:: (" (:i "genes given as \"hypothetical\" in Anabaena PCC 7120")")

("((:a :href (:print (make-help-function-documentation-url :name "DEFINE" :package "bbi"))) "DEFINE")" set-of-hypothetical-het-cb-genes AS
    (INTERSECTION-OF (set-of-het-cb-genes set-of-hypotheticals))
:: (" (:i "genes found in both sets")")"))
)))

(:table :align "middle" :width "80%" (:b 
  (:tr  (:td  :width "26.7%" :align "left" ((:a :style "text-decoration:none" :href (:print (make-help-topic-url :name "Accessing information from lists")))(:button " Previous Page")))
(:td :width "26.7%" :align "middle"((:a :style "text-decoration:none" :href (:print (make-help-topic-url :name "Basic Syntax")))(:button "Up to Table of Contents")))
(:td :align "right" :width "26.7%"((:a :style "text-decoration:none" :href (:print (make-help-topic-url :name "Tables")))(:button " Next Page")))) 
   
  (:tr (:td :width "26.7%")
(:td :align "middle"  :width "26.7%"((:a :style "text-decoration:none" :href (:print (make-help-topic-url :name "HELP for BBL users"))) (:button "Help for BBL Users")))
(:td :width "26.7%")))) 
))



(def-topic "Tables"
(:text
(:table :align "middle" :width "80%"
(:tr 
(:td (:small
(:p (:b "D. "(:u "Data Aggregates (lists and tables)")))


(:ol
(:li ((:a :style "color:#0000EE" :href (:print (make-help-topic-url :name "The need for data aggregates"))) "The need for data aggregates")) 
(:li ((:a :style "color:#0000EE" :href (:print (make-help-topic-url :name "Strings"))) "Strings"))
(:li ((:a :style "color:#0000EE" :href (:print (make-help-topic-url :name "Lists"))) "Lists")) 
(:ol :type "a"  
  (:li ((:a :style "color:#0000EE" :href (:print (make-help-topic-url :name "Simple lists and how to make them"))) "Simple lists - how to make them"))
  (:li ((:a :style "color:#0000EE" :href (:print (make-help-topic-url :name "Complex Lists"))) "Complex Lists"))
  (:li ((:a :style "color:#0000EE" :href (:print (make-help-topic-url :name "Accessing information from lists"))) "Accessing information from lists"))
  (:li ((:a :style "color:#0000EE" :href (:print (make-help-topic-url :name "Sets"))) "Sets"))) 
(:li ((:a :style "color:#0000EE" :href (:print (make-help-topic-url :name "Tables"))) (:font :color "red" "Tables")))) 

(:p (:b "D4. "(:u "Tables")))
(:p "Tables are collections of things (any type) organized by subscripts or labels.") 
(:p "For some information, it is helpful to visualize them as tables with columns and rows (although they may be of any dimensionality) rather than lists.")
 


(:p "Here's an example of a two-dimensional table:"))
(:p (:ul (:pre "  
      1  2  3  4  5  6  7  8  9  10 11 12 13 
nirA  A  A  T  T  T  T  G  T  A  G  C  T  A 
ntcB  A  A  A  G  C  T  G  T  A  A  C  A  A 
glnA  A  A  A  T  C  T  G  T  A  A  C  A  T 
devB  C  G  T  T  C  T  G  T  A  A  C  A  A 
 urt  A  A  T  T  T  A  G  T  A  T  C  A  A"))) 

(:small (:p "The labels are shown in bold, with row-labels being \"nirA\", \"ntcB\", etc, and the column-labels 1 through 13.
While the information within each cell usually is of the same type, it doesn't have to be. You could build a table looking like this:")) 
(:p (:ul (:pre   
(:b
"        Size         Gene-number  Habitat" )" 
"
(:b "Npun")"    9059191      7412         \"terrestrial\" 
"
(:b "ss120")"   1751080      1928         \"marine, low-light\" 
"
(:b "S6803")"   3956956      3722         \"fresh water\""))) 


(:small (:p "You could refer to any cell in the table by its row and column. For example in column Gene-number and row ss120 is 1928. BBL facilitates working with tables (of any dimension) with the elements named by numeric position or by label, as you wish."))
)))

(:table :align "middle" :width "80%" (:b 
  (:tr  (:td  :width "26.7%" :align "left" ((:a :style "text-decoration:none" :href (:print (make-help-topic-url :name "Sets")))(:button " Previous Page")))
(:td :width "26.7%" :align "middle"((:a :style "text-decoration:none" :href (:print (make-help-topic-url :name "Basic Syntax")))(:button "Up to Table of Contents")))
(:td :align "right" :width "26.7%"((:a :style "text-decoration:none" :href (:print (make-help-topic-url :name "What are frames")))(:button " Next Page")))) 
   
  (:tr (:td :width "26.7%")
(:td :align "middle"  :width "26.7%"((:a :style "text-decoration:none" :href (:print (make-help-topic-url :name "HELP for BBL users"))) (:button "Help for BBL Users")))
(:td :width "26.7%")))) 
))

;;========= FRAMES =============


(def-topic "What are frames"
(:text
(:table :align "middle" :width "80%"
(:tr 
(:td (:small
(:p (:b "E. "(:u "Frames")))
(:ol
(:li ((:a :style "color:#0000EE" :href (:print (make-help-topic-url :name "What are frames"))) (:font :color "red" "What are frames?"))) 
(:li ((:a :style "color:#0000EE" :href (:print (make-help-topic-url :name "Frame naming conventions"))) "Frame naming conventions"))
(:li ((:a :style "color:#0000EE" :href (:print (make-help-topic-url :name "How to get things from frames"))) "How to get things from frames?"))) 

(:p (:b "E1. "(:u "What are frames?")))
(:p "Much of the knowledge stored in BioBIKE is made available to you through objects called frames. You can see what one looks at by entering either of the following into the command window:"))

(:ul (:pre "all4312
:: "(:u "\#\$A7120.all4312")))    

(:small (:p "or (equivalently):"))

(:ul (:pre "A7120.all4312
:: "(:u "\#\$A7120.all4312")))

(:small (:p "The underlined text is the name of the frame containing information about the gene all4312 and serves (in the history window, not on this screen) as a link to that frame. Click on it to see what a frame looks like."))

)))

(:table :align "middle" :width "80%" (:b 
  (:tr  (:td  :width "26.7%" :align "left" ((:a :style "text-decoration:none" :href (:print (make-help-topic-url :name "Tables")))(:button " Previous Page")))
(:td :width "26.7%" :align "middle"((:a :style "text-decoration:none" :href (:print (make-help-topic-url :name "Basic Syntax")))(:button "Up to Table of Contents")))
(:td :align "right" :width "26.7%"((:a :style "text-decoration:none" :href (:print (make-help-topic-url :name "Frame naming conventions")))(:button " Next Page")))) 
   
  (:tr (:td :width "26.7%")
(:td :align "middle"  :width "26.7%"((:a :style "text-decoration:none" :href (:print (make-help-topic-url :name "HELP for BBL users"))) (:button "Help for BBL Users")))
(:td :width "26.7%")))) 
))
	


(def-topic "Frame naming conventions"
(:text
(:table :align "middle" :width "80%"
(:tr 
(:td (:small
(:p (:b "E. "(:u "Frames")))

(:ol
(:li ((:a :style "color:#0000EE" :href (:print (make-help-topic-url :name "What are frames"))) "What are frames?"))
(:li ((:a :style "color:#0000EE" :href (:print (make-help-topic-url :name "Frame naming conventions"))) (:font :color "red" "Frame naming conventions"))) 
(:li ((:a :style "color:#0000EE" :href (:print (make-help-topic-url :name "How to get things from frames"))) "How to get things from frames?")))

(:p (:b "E2. "(:u "Frame naming conventions")))
(:p (:li "All frame names are unique and all begin with the characters ") "\#\$" (:b ".")) 
  
(:p (:li "You needn't concern yourself with those prefix characters\, since BioBIKE recognizes as a frame name anything containing a period. Thus\, the following are all interpretted as names of frames:")))

(:ul (:pre "GO.glycolysis
S6803.sll0632
S6803.pSYMA
.Direction"))

(:small (:p (:li "In addition\, BLL recognizes and expands the nicknames of legitimate genes\, proteins\, and organisms to their full frame names:")))

(:ul (:pre "slll0632   is expanded to   s6803.sll0632
p-sll0632  is expanded to   s6803.p-sll0632
s6803      is expanded to   synechocystis_pcc6803
syn6803    is expanded to   synechocystis_pcc6803"))

(:small (:p (:li "Organism frames are associated with several nicknames. Use the " ((:a :href (:print (make-help-function-documentation-url :name "NICKNAMES-OF" :package "bbi"))) "NICKNAMES-OF") " function to see the full list for an organism."))
  
(:p (:li "To ensure uniqueness and to organize the frames, many are given prefixes. For example, all frames within the metabolic hierarchy of the Ocelot database are given the prefix " (:b "OC")", while those within the description of chemicals by the Gene Ontology database are given the prefix " (:b "MOL")". Examples:")))

(:ul (:p (:pre ""(:u "OC.Gdp-D-Glucose")"
" (:u "MOL.Gdp-D-Glucose")"
" (:u "S6803.chromosome")"
" (:u "A7120.chromosome")))) 

(:small (:p "The official prefix of a particular organism can be seen in the frame of that organism. You can view the prefixes of all organisms by entering at the command line (within BioBIKE): "))

(:p (:pre "(NAMES-OF *all-organisms* SHORT)     [" (:i "At present only in beta-release") "]")) 

(:small 
(:p (:li "Gene and protein frame names differ from each other in a systematic way.
The protein frame name can always be constructed from the gene frame name by adding \"p\-\" after the prefix.
Example:")))

(:ul (:p (:pre "(DISPLAY-SEQUENCE-OF all4312)
:: (result) " (:b "; Sequence of gene all4312")"
(DISPLAY-SEQUENCE-OF p-all4312)
:: (result) " (:b "; Sequence of protein p-all4312"))))          


))) 

(:table :align "middle" :width "80%" (:b 
  (:tr  (:td  :width "26.7%" :align "left" ((:a :style "text-decoration:none" :href (:print (make-help-topic-url :name "What are frames")))(:button " Previous Page")))
(:td :width "26.7%" :align "middle"((:a :style "text-decoration:none" :href (:print (make-help-topic-url :name "Basic Syntax")))(:button "Up to Table of Contents")))
(:td :align "right" :width "26.7%"((:a :style "text-decoration:none" :href (:print (make-help-topic-url :name "How to get things from frames")))(:button " Next Page")))) 
   
  (:tr (:td :width "26.7%")
(:td :align "middle"  :width "26.7%"((:a :style "text-decoration:none" :href (:print (make-help-topic-url :name "HELP for BBL users"))) (:button "Help for BBL Users")))
(:td :width "26.7%")))) 
)) 



(def-topic "How to get things from frames"
(:text
(:table :align "middle" :width "80%"
(:tr 
(:td (:small
(:p (:b "E. "(:u "Frames")))

(:ol
(:li ((:a :style "color:#0000EE" :href (:print (make-help-topic-url :name "What are frames"))) "What are frames?"))
(:li ((:a :style "color:#0000EE" :href (:print (make-help-topic-url :name "Frame naming conventions"))) "Frame naming conventions"))
(:li ((:a :style "color:#0000EE" :href (:print (make-help-topic-url :name "How to get things from frames"))) (:font :color "red" "How to get things from frames?")))) 

(:p (:b "E3. "(:u "How to get things from frames?")))

(:p "You can view the information within a frame by displaying its name and then clicking on it. However, you'll more often want to access the information from within a command or program. To do this, you can use any of the following methods:"))

(:p (:ul (:pre "frame" (:b "[") "name-of-slot1 name-of-slot2 ..." (:b "]")))) 

(:small (:p "Like lists, frames use square brackets to extract items from within them. For example:"))

(:p (:ul (:pre "all4312[.Organism]
:: " (:u "\#\$anabaena_pcc7120")"

all4312[.From .To .Direction]
:: (5166997 5167767 :B)")))  

(:small (:p "In the first case\, one item from the frame\, the organism\, is returned. The word organism must be preceded by a period to denote that it is itself a frame name and not the name of a variable. In the second case\, multiple items are returned. 
You can also get multiple elements from multiple frames\, using a special function:"))

(:p (:ul (:pre "(GET-FRAME-ELEMENTS (.From .To .Direction) FROM (ORTHOLOGS-OF all4312))
:: ((963546 964253 :F) (2438065 2438769 :F) (1385184 1385930 :F)
    (6298 7077 :B) (106037 106795 :B) (27844 28614 :F)
    (5166997 5167767 :B) (4926062 4926835 :B) (3294498 3295250 :B))"))) 

)))

(:table :align "middle" :width "80%" (:b 
(:tr  
(:td  :width "26.7%" :align "left" ((:a :style "text-decoration:none" :href (:print (make-help-topic-url :name "Frame naming conventions")))(:button " Previous Page")))
(:td :width "26.7%" :align "middle"((:a :style "text-decoration:none" :href (:print (make-help-topic-url :name "Basic Syntax")))(:button "Up to Table of Contents")))
(:td :align "right" :width "26.7%"((:a :style "text-decoration:none" :href (:print (make-help-topic-url :name "BioBIKE Principles")))(:button " Next Page"))))    
(:tr 
(:td :width "26.7%")
(:td :align "middle"  :width "26.7%"((:a :style "text-decoration:none" :href (:print (make-help-topic-url :name "HELP for BBL users"))) (:button "Help for BBL Users")))
(:td :width "26.7%")))) 
))


;;========= BIOBIKE PRINCIPLES ===========

(def-topic "BioBIKE Principles"
(:text
(:table :align "middle" :width "80%"
(:tr 
(:td (:small
(:p (:b "F. "(:u "BioBIKE Principles")))
(:p (:li "Permits use of names familiar to the biologist"))
(:p "BBL allows the user to refer to genes, proteins, replicons, and contigs by their common names:"))

(:ul (:pre "(PROTEINS-SIMILAR-TO p-All4312 IN PCC6803
:: ((\"S6803.p-Sll1330\" 2.6d-93) (\"S6803.p-Slr0947\" 1.2d-32) . . .

("((:a :href (:print (make-help-function-documentation-url :name "SEQUENCE-OF" :package "bbi" ))) "SEQUENCE-OF")" pSYSA)
:: \"GGCGCGCCACCGTGTACCCAGTTTCCTCGGAATGGAACGTAATGCGCTCA...\"

\*nucleotides\*
:: (\"A\" \"C\" \"G\" \"T\")"))

(:small (:p (:li "Names functions in a way familiar to the biologist")))

(:ul (:pre "(SEQUENCE-UPSTREAM-OF (ORTHOLOGS-OF (GENE-CALLED \"glnA\" IN Npun)))
:: " (:i "sequences upstream of several glnA orthologs") "]

("((:a :href (:print (make-help-function-documentation-url :name "COUNT-OF" :package "bbi" ))) "COUNT-OF")" \*nucleotides\* IN A7120.chromosome)
:: (1878490 1323814 1328123 1883344)"))

(:small (:p (:li "Interconverts between gene and protein as appropriate")))

(:ul (:pre "("((:a :href (:print (make-help-function-documentation-url :name "MW-OF" :package "bbi" ))) "MW-OF")" all4312)        [" (:i "Figures you must want the molecular weight of the encoded protein") "]
:: 33742

("((:a :href (:print (make-help-function-documentation-url :name "GENES-UPSTREAM-OF" :package "bbi" ))) "GENES-UPSTREAM-OF")" (ORTHOLOGS-OF p-all4312))       [" (:i "Realizes that gene upstream of protein makes no sense") "]
:: (" (:u "\#\$S7942.ser1031") " "(:u "\#\$Gvi.gsr2273") " " (:u "\#\$TeBP1.tll1329") " "(:u "\#\$Cwat.Cw\?1563") "...)"))

(:small (:p (:li "Allows input information to be single entities or lists of entities.")))

(:ul (:pre "("((:a :href (:print (make-help-function-documentation-url :name "MW-OF" :package "bbi" ))) "MW-OF")" p-NpF0001)
:: 60335

("((:a :href (:print (make-help-function-documentation-url :name "MW-OF" :package "bbi" ))) "MW-OF")" (ORTHOLOGS-OF p-NpF0001))
:: (59857 60278 60366 60335 57866)"))

)))

(:table :align "middle" :width "80%" (:b 
(:tr  
(:td  :width "26.7%" :align "left" ((:a :style "text-decoration:none" :href (:print (make-help-topic-url :name "How to get things from frames")))(:button "Previous Page")))
(:td :width "26.7%" :align "middle"((:a :style "text-decoration:none" :href (:print (make-help-topic-url :name "Basic Syntax")))(:button "Up to Table of Contents")))
(:td :width "26.7%" :align "right"((:a :style "text-decoration:none" :href (:print (make-help-topic-url :name "General Considerations")))(:button "Next Page"))))

(:tr (:td :width "26.7%")
(:td :align "middle"  :width "26.7%"((:a :style "text-decoration:none" :href (:print (make-help-topic-url :name "HELP for BBL users"))) (:button "Help for BBL Users")))
(:td :width "26.7%")))) 
))
  
;;======== GENERAL CONSIDERATIONS ===========

(def-topic "General Considerations"
(:text
(:table :align "middle" :width "80%"
(:tr 
(:td (:small
(:p (:b "B. "(:u "General Considerations")))

(:p "1. "(:u "Upper/Lower case:")" It doesn't matter to the language whether you type in upper case\, lower case\, or a mixture (except\, sometimes\, when you're typing something within quotation marks). Do whatever is easiest and\/or clearest for you. ")

(:p "2. "(:u "Spacing:")" The language also doesn't care how many spaces or lines you put between symbols. The following are equivalent (to the computer):"))
    
  (:p (:ul (:pre "(for-each protein in (proteins-of avar) as sequence = (sequence-of protein) when 
(successful (search \"rilkiqtl\" sequence)) collect protein)")))

    (:small (:ul (:ul (:ul "OR"))))
   
  (:p (:ul (:pre "(FOR-EACH protein IN ("((:a :href (:print (make-help-function-documentation-url :name "PROTEINS-OF" :package "bbi" ))) "PROTEINS-OF")" Avar)
AS sequence = ("((:a :href (:print (make-help-function-documentation-url :name "SEQUENCE-OF" :package "bbi" ))) "SEQUENCE-OF")" protein)
WHEN (SUCCESSFUL (SEARCH \"RILKIQTL\" sequence))
COLLECT protein)")))

(:small
(:p "3. "(:u "Variables:")" Variables at minimum help organize your thoughts.  At maximum they allow you to do in general what you could not hope to do in a lifetime of specific instructions.")
(:p "Variables may be defined explicitly with DEFINE or implicitly with ASSIGN or INCREMENT.  If an attempt is made to ASSIGN or INCREMENT a variable that has not previously been defined, then it is created on the spot.  All of these functions returns the value it ends up assigning."))

	(:ul (:pre "<1>> ("((:a :href (:print (make-help-function-documentation-url :name "DEFINE" :package "bbi")))"DEFINE") " x AS 47)
:: 47
[" (:i "The variable ") "x" (:i " is defined and given the value 47") "]

<2>> ("((:a :href (:print (make-help-function-documentation-url :name "DEFINE" :package "bbi")))"DEFINE") " my-favorite-genes AS {all4312 npf0001})
:: (#$A7120.all4312 #$Npun.NpF0001)
[" (:i "The variable ") "my-favorite-genes" (:i " is defined and given the value a list consisting 
of two genes") "]

<3>> ("((:a :href (:print (make-help-function-documentation-url :name "DEFINE" :package "bbi")))"DEFINE") " glnA-genes AS
ORTHOLOGS-OF ("((:a :href (:print (make-help-function-documentation-url :name "GENE-DESCRIBED-BY" :package "bbi")))"GENE-DESCRIBED-BY") " \"glnA\" IN A7120)))
:: (#$S7942.ser0686 #$P9313.PMT0601 #$S6803.slr1756 #$Npun.NpR5387 . . .)
[" (:i "The variable ") "glnA-genes" (:i " is defined and given the value a list consisting of all
genes orthologous (similar in a way we'll no doubt discuss later) to a gene of
Anabaena PCC 7120 that has the name \"glnA\"") "]"))

(:small (:p "Click "((:a :href (:print (make-help-topic-url :name "General Considerations - here"))) "here")" for a more complex example illustrating the advantages of using variables to provide clarity.")

(:p "4. "(:u "Names of variables:")" There are almost no reserved words in the language -- use any name you like for a variable. However, the language will protect itself (and you) if you try to redefine a core function (e.g. redefining \"+\" to mean subtract). Also BBL can sometimes get confused if you use a function name as the name of a variable.")

(:p "5. "(:u "Legal characters:")" You can use in variable or function names the usual letters and numerals (except that a name cannot consist solely of numerals). In addition, you can skate along the top of your keyboard and make names with the following characters: \~\!\@\$\%\^\&\*\_\?\/\-\+\=. But there are limitations. You can't put the following symbols in names: .\,\:\;\|\()[]{} or any manner of quotation mark. Thus the following is legal:"))

  (:p (:ul (:pre "("((:a :href (:print (make-help-function-documentation-url :name "DEFINE" :package "bbi"))) "DEFINE")" @$%&! AS All4312)	;" (:i "(perhaps an unclonable gene)"))))

(:small 
(:p "6. "(:u "Data types:")" Lisp is a highly typed language. BBL does behind the scene type conversions so that users seldom have to consider data types. (If you don't know what I'm talking about, it probably won't matter)")

(:p "7. "(:u "Comments:")" There are two ways to designate comments (text that's ignored by the language, there just to document your thoughts). First, a semicolon signifies that anything remaining in the line is to be ignored. Second the pair of symbols #| and |# specify that anything in between is to be ignored. Examples:")) 

  (:p (:ul (:pre "(CONTEXT-OF (2376879 2501670 4918619 5946036)
       IN #$A7120.chromosome)	; (All the SphI sites in Anabaena PCC 7120)

("((:a :href (:print (make-help-function-documentation-url :name "DEFINE" :package "bbi"))) "DEFINE")" marine-cb AS {PMed4 SS120 MIT9414 S8102} )
#| Define a set of marine cyanobacteria consisting of
     Prochlorococcus marinus MED4
     Prochlorococcus marinus SS120
     Prochlorococcus marinus MIT9414
     Synechococcus WH8102 |#")))
)))

(:table :align "middle" :width "80%" (:b 
  (:tr  (:td  :width "26.7%" :align "left" ((:a :style "text-decoration:none" :href (:print (make-help-topic-url :name "BioBIKE Principles")))(:button " Previous Page")))
(:td :width "26.7%" :align "middle"((:a :style "text-decoration:none" :href (:print (make-help-topic-url :name "Basic Syntax")))(:button "Up to Table of Contents")))
(:td :align "right" :width "26.7%"((:a :style "text-decoration:none" :href (:print (make-help-topic-url :name "Mapping and Loops")))(:button " Next Page")))) 
   
  (:tr (:td :width "26.7%")
(:td :align "middle"  :width "26.7%"((:a :style "text-decoration:none" :href (:print (make-help-topic-url :name "HELP for BBL users"))) (:button "Help for BBL Users")))
(:td :width "26.7%")))) 
))

(def-topic "General Considerations - here"
(:text 
(:table :align "middle" :width "80%"
(:tr 
(:td (:small
(:p (:b (:u "Example:")))
(:p "To illustrate how defining variables helps to organize your thoughts and make it easier to see later what you did, consider this situation:")
(:p (:i "You want to find out what genes, if any, are peculiar to those cyanobacteria that live in the ocean. Perhaps these will include genes to resist the high concentration of salt, etc.  Then you want to download these marine-specific genes, along with a descriptive phrase.  Here's one way to do it:")) )
	
	(:ul (:pre "<1>> *all-organisms*
:: (#$synechococcus_elongatus_pcc7942 #$prochlorococcus_marinus_mit9313
#$synechocystis_pcc6803 #$nostoc_punctiforme_atcc29133 . . .)"))
(:small (:ul (:i "Entering the name of an object asks BioBIKE to return the value of the object. Here it gives
us a list of all the organisms know to the system.")))

	(:ul (:pre "<2>> (ITEMS .habitat IN-EACH-OF *all-organisms*)
:: (\"fresh water\" \"marine\" \"fresh water\" \"terrestrial\" . . )"))
(:small (:ul (:i "ITEMS is used to go inside of informational structures. Here it asks for the retrieval from every organism the information stored under the category \"habitat\".")))

	(:ul (:pre "<3>> ("((:a :href (:print (make-help-function-documentation-url :name "DEFINE" :package "bbi"))) "DEFINE")" marine-organisms AS *all-organisms*[2 6 7 8 11 12])
:: (#$prochlorococcus_marinus_mit9313 #$trichodesmium_erythraeum . . .)"))
(:small (:ul (:i "The variable ") "marine-organisms" (:i " is defined as the 2nd, 6th, 7th, 8th, 11th, and 12th organism within the list of all organisms known to the system.")))

	(:ul (:pre "<4>> ("((:a :href (:print (make-help-function-documentation-url :name "DEFINE" :package "bbi"))) "DEFINE")" non-marine-organisms AS
	(SET-DIFFERENCE *all-organisms* marine-organisms))
:: (#$anabaena_variabilis_atcc29413 #$thermosynechococcus_elongatus_bp1...)"))
(:small (:ul (:i "The variable ") "non-marine-organisms" (:i " is defined as what's left when you remove the organisms of ") "marine-organisms" (:i " from the set of all available organisms."))) 

(:ul (:pre "<5>> ("((:a :href (:print (make-help-function-documentation-url :name "DEFINE" :package "bbi"))) "DEFINE")" marine-genes AS
	("((:a :href (:print (make-help-function-documentation-url :name "COMMON-ORTHOLOGS-OF" :package "bbi"))) "COMMON-ORTHOLOGS-OF")" marine-organisms NOT-IN non-marine-organisms))
:: (#$P9313.p-PMT1856 #$P9313.p-PMT0643 #$P9313.p-PMT0605
#$P9313.p-PMT0340 #$P9313.p-PMT0339 #$P9313.p-PMT0268)"))
(:small (:ul (:i "The variable ") "marine-genes" (:i " is defined as the set of all genes common to marine organisms not found in non-marine organisms.")))

(:ul (:pre "<6>> ("((:a :href (:print (make-help-function-documentation-url :name "DEFINE" :package "bbi"))) "DEFINE")" marine-genes-info AS
	(INTERLEAVE marine-genes ("((:a :href (:print (make-help-function-documentation-url :name "DESCRIPTIONS-OF" :package "bbi"))) "DESCRIPTIONS-OF")" marine-genes))
:: ((#$P9313.p-PMT1856 \"possible Photosystem II reacti\")
(#$P9313.p-PMT0643 \"Integral membrane protein\, DU\") ...)"))
(:small (:ul (:i "The variable ") "marine-genes-info" (:i " is defined as a set of paired values -- the name of a gene and its description - for each gene in the set called marine-genes.")))

(:ul (:pre "<7>> (WRITE-TAB-DELIMITED-FILE "marine-genes-info.txt" marine-genes-info)"))
(:small (:ul (:i "The list of paired values in ") "marine-genes-info" (:i " is written out in tab-delimited format (readable by Excel) into a file called \"marine-genes-info.txt\".")))

(:small (:p "You " (:b (:i "could")) " have accomplished the same end without defining any variables, but I'll bet you'd never be able to understand the code a week later."))
)))

(:table :align "middle" :width "80%" (:b 
  (:tr  (:td  :width "26.7%" :align "left" ((:a :style "text-decoration:none" :href (:print (make-help-topic-url :name "General Considerations")))(:button " Back to General Considerations ")) )) ))
))         