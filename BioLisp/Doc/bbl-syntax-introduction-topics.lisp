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



(def-topic "Introduction to BioBIKE Syntax"
(:text 
(:table :align "middle" :width "80%"
(:tr (:td (:small

(:p (:b "A. "(:u "Introduction to BioBIKE Syntax")))

(:ol
(:li ((:a :style "color:#0000EE"  
:href #.(UTILS::s+ wb::*doc-directory-url* "Basic-syntax-A1-English-syntax.pdf")) "English syntax as a model for the syntax of computer languages")
     (:BR) "(Exercises)" (:BR)(:BR))
(:li ((:a :style "color:#0000EE" 
:href #.(UTILS::s+ wb::*doc-directory-url* "Basic-syntax-A2-Basic-conventions.pdf")) "Basic conventions of BioBIKE Language (BBL)")
     (:BR) "a. BioBIKE syntax - Overview"
     (:BR) "b. How to find and invoke BioBIKE functions"
     (:BR) "c. How to work with a BioBIKE function"
     (:BR) "d. Filling in holes of arguments and keywords"
     (:BR) "e. Execution of functions - Results and displays"
     (:BR) "(Exercises)"(:BR)(:BR))
(:li ((:a :style "color:#0000EE" :href (:print (make-help-topic-url :name "Textual representation of BBL forms"))) "Textual representation of BBL forms")))
)))) 



(:table :align "middle" :width "80%" (:b 
(:tr  (:td  :width "26.7%" :align "left" ((:a :style "text-decoration:none" :href (:print (make-help-topic-url :name "Basic Syntax")))(:button "Previous Page")))
(:td :width "26.7%" :align "middle"((:a :style "text-decoration:none" :href (:print (make-help-topic-url :name "Basic Syntax")))(:button "Up to Table of Contents")))
(:td :align "right" :width "26.7%"((:a :style "text-decoration:none" :href (:print (make-help-topic-url :name "English syntax as a model for the syntax of computer languages")))(:button " Next Page")))) 
   
(:tr (:td :width "26.7%")
(:td :align "middle"  :width "26.7%"((:a :style "text-decoration:none" :href (:print (make-help-topic-url :name "HELP for BBL users"))) (:button "Help for BBL Users")))
(:td :width "26.7%")))) 
))






(def-topic "English syntax as a model for the syntax of computer languages"
(:text 
(:table :align "middle" :width "80%"
(:tr (:td (:small

(:p (:b "A. "(:u "Introduction to BioBIKE Syntax")))

(:ol
(:li ((:a :style "color:#0000EE" :href (:print (make-help-topic-url :name "English syntax as a model for the syntax of computer languages"))) (:font :color "red" "English syntax as a model for the syntax of computer languages"))) 
(:li ((:a :style "color:#0000EE" :href (:print (make-help-topic-url :name "Syntactical forms in BioBIKE Language (BBL)"))) "Syntactical forms in BioBIKE Language (BBL)"))
(:li ((:a :style "color:#0000EE" :href (:print (make-help-topic-url :name "Textual representation of BBL forms"))) "Textual representation of BBL forms"))) 

(:p (:b "1. English syntax as a model for the syntax of computer languages")) 
(:blockquote (:i "You might think that people who seem to know a computer language either possess some special knowledge or are endowed with some magical ability to sense what's right.")) 
(:p "You were probably able to extract some meaning from that complicated sentence, and if so, you have all the basic tools necessary to understand a computer language. You did it, no doubt, without thinking, so let's try it again, this time WITH thinking. So wipe out your knowledge of English.") 
(:p "NOW how do you understand the sentence?") 
(:p "Well, of course you'd need an English dictionary. You look up 'you' and get the basic meaning and the fact that it is a noun. You could look up the rest the words as well, one by one, but that clearly isn't enough. You also need to know some rules as to how the words work together,... syntax!
Suppose you learn that English sentences may take a variety of structures, one of the simplest being:") ; Fig 1

(:ul (:img :src "/weblistenerdocs/img-bbldoc/BBL_GS_Fig1.jpg"))

(:p "'You' is a noun, 'might' is a verb modifier, and 'think' is a verb, says your dictionary. So far:") ; Fig 2

(:ul (:img  :src "/weblistenerdocs/img-bbldoc/BBL_GS_Fig2.jpg"))

(:p "But what about the other 25 words of the sentence? Looking more closely at the dictionary entry for 'think', you find that it can serve as either a transitive verb (taking an object) or an intransitive verb (taking no object). So either of the following structures is legal:") ; Fig 3

(:ul (:img  :src "/weblistenerdocs/img-bbldoc/BBL_GS_Fig3.jpg"))


(:p "This is useful, because your dictionary says that the fourth word, 'that', can introduce phrases that can replace nouns, perhaps like so:") ; Fig 4

(:ul (:img  :src "/weblistenerdocs/img-bbldoc/BBL_GS_Fig4.jpg"))

(:p "Look back on these boxes:") 
(:ul
(:li "Boxes with dotted boundaries represent defined holes")  
(:li "Boxes with solid boundaries represent holes filled with an object")
(:li "Boxes with thick solid boundaries represent holes filled with functions that return an object")) 
 
(:p "Complicated! Fortunately, computer languages are much simpler (otherwise computers couldn't understand them), and BioBIKE Language (BBL) is about as simple as you can get and still retain the ability to express everything you need in a language. However, like human languages, computer languages increase their powers of expression by permitting forms to be placed within forms multiple times.")
)))) 

(:table :align "middle" :width "80%" (:b 
(:tr  
(:td  :width "26.7%" :align "left" ((:a :style "text-decoration:none" :href (:print (make-help-topic-url :name "Introduction to BioBIKE Syntax")))(:button "Previous Page")))
(:td :width "26.7%" :align "middle"((:a :style "text-decoration:none" :href (:print (make-help-topic-url :name "Basic Syntax")))(:button "Up to Table of Contents")))
(:td :align "right" :width "26.7%"((:a :style "text-decoration:none" :href (:print (make-help-topic-url :name "Syntactical forms in BioBIKE Language (BBL)")))(:button " Next Page"))))    
(:tr 
(:td :width "26.7%")
(:td :align "middle"  :width "26.7%"((:a :style "text-decoration:none" :href (:print (make-help-topic-url :name "HELP for BBL users"))) (:button "Help for BBL Users")))
(:td :width "26.7%"))))
)) 

;***********************

(def-topic "Syntactical forms in BioBIKE Language (BBL)"
(:text 
(:table :align "middle" :width "80%"
(:tr (:td (:small

(:p (:b "A. "(:u "Introduction to BioBIKE Syntax")) (:BR)
    (:b "A.2. Syntactical forms in BioBIKE Language (BBL)"))

(:ol :TYPE :|a|
  (:li ((:a :style "color:#0000EE" :href (:print (make-help-topic-url 
  :name "Syntactical forms in BioBIKE Language (BBL)"))) 
    (:font :color "red" "Objects"))
    " (" ((:A :STYLE "color:#0000EE" :href (:PRINT (make-help-topic-url
    :name "Syntactical forms in BioBIKE Language (BBL)-Object exercises")))
    "exercises") ")" ) 
  (:li ((:a :style "color:#0000EE" :href (:print (make-help-topic-url 
    :name "Syntactical forms in BioBIKE Language (BBL)-Functions"))) 
    "Functions")
    " (" ((:A :STYLE "color:#0000EE" :href (:PRINT (make-help-topic-url
    :name "Syntactical forms in BioBIKE Language (BBL)-Function exercises")))
    "exercises") ")" ) 
)

(:P (:B "A.2.a. " (:U "Objects")) (:BR)
    "BBL in its entirety may be compressed into a single form:") ; Fig 5

(:ul (:img  :src "/weblistenerdocs/img-bbldoc/BBL_GS_Fig5.jpg"))
     
(:p "which represents a hole to be filled by an object or something that produces an object. An object is a number, a string (a collection of characters), a list (a collection of objects), and many other things we needn't concern ourselves with now.")

(:P "Try a few. Go into BioBIKE (the graphical interface) and bring down what is called a data box. You can find it on the DATA menu:")

(:ul (:img  :src "/weblistenerdocs/img-bbldoc/BBL_GS_data-box.gif"))

(:P 
 (:TABLE :ALIGN "LEFT"
  (:TR
    (:TD 
     (:SMALL
      (:P
      "Click on the box (i.e., on the word" (:I " object") "), type 47 in the box "
      "and press the Enter key on your keyboard. Then" (:B (:I " execute "))
      "the object (evaluate it) by clicking the green Action Icon and clicking"
      (:I " execute") ".")
     (:P "You'll see a '47' appear in the Result pane at the bottom. "
         (:B (:I "Numbers")) " are legal objects")
     (:P "Erase the data box, by clicking on the red x in the upper right corner, "
         "then try evaluating a "
         "string, like \"banana\" (being sure to enclose it within "
         "double quotes). " (:B (:I "Strings")) " are legal objects.")))
    (:TD (:img  :src "/weblistenerdocs/img-bbldoc/BBL_GS_execute-data-box.gif")))))
    
(:P "Finally, erase the data box and evaluate a list, like (47 \"banana\"). "
    (:B (:I "Lists")) " are legal objects.")

(:P "There are many more types of objects, but that's enough for now.")

)))) 

(:table :align "middle" :width "80%" (:b 
(:tr  
(:td  :width "26.7%" :align "left" 
  ((:a :style "text-decoration:none" 
    :href (:print (make-help-topic-url 
      :name "English syntax as a model for the syntax of computer languages")))
   (:button "Previous Page")))
(:td :width "26.7%" :align "middle"
  ((:a :style "text-decoration:none" 
    :href (:print (make-help-topic-url :name "Basic Syntax")))
    (:button "Up to Table of Contents")))
(:td :align "right" :width "26.7%"
  ((:a :style "text-decoration:none" 
    :href (:print (make-help-topic-url 
      :name "Syntactical forms in BioBIKE Language (BBL)-Functions")))
    (:button " Next Page")))) 
   
(:tr (:td :width "26.7%")
(:td :align "middle"  :width "26.7%"
  ((:a :style "text-decoration:none" 
    :href (:print (make-help-topic-url :name "HELP for BBL users")))
    (:button "Help for BBL Users")))
(:td :width "26.7%"))))
))

; *****************

(def-topic "Syntactical forms in BioBIKE Language (BBL)-Functions"
(:text 
(:table :align "middle" :width "80%"
(:tr (:td (:small

(:p (:b "A. "(:u "Introduction to BioBIKE Syntax")) (:BR)
    (:b "A.2. Syntactical forms in BioBIKE Language (BBL)"))

(:ol :TYPE :|a|
  (:li ((:a :style "color:#0000EE" :href (:print (make-help-topic-url 
  :name "Syntactical forms in BioBIKE Language (BBL)"))) 
    "Objects")
    " (" ((:A :STYLE "color:#0000EE" :href (:PRINT (make-help-topic-url
    :name "Syntactical forms in BioBIKE Language (BBL)-Object exercises")))
    "exercises") ")" ) 
  (:li ((:a :style "color:#0000EE" :href (:print (make-help-topic-url 
    :name "Syntactical forms in BioBIKE Language (BBL)-Functions"))) 
    (:font :color "red" "Functions"))
    " (" ((:A :STYLE "color:#0000EE" :href (:PRINT (make-help-topic-url
    :name "Syntactical forms in BioBIKE Language (BBL)-Function exercises")))
    "exercises") ")" ) 
)

(:P (:B "A.2.b. " (:U "Functions")) (:BR)

  "As in English, the object box may be filled with something that produces an object of the appropriate type. In BBL, all legal functions produce objects of one type or another. There is a large number of functions, but you will be pleased to learn that all BBL functions (unlike English generational structures) have a single general format:") ; Fig 6

(:ul (:img  :src "/weblistenerdocs/img-bbldoc/BBL_GS_Fig6.jpg"))

(:P "composed of the following parts:")

(:UL (:LI (:B "Function name:")
          " The name identifies a function, which is a set of operations that accepts whatever objects the user may provide, acts on them, and returns an object that fills the hole in which the function box is sitting." (:BR)(HTML "&nbsp;"))
     (:LI (:B "Argument(s):")
          " A function may require 0, 1, or more objects to act on, but whatever that number is, that specific function ALWAYS requires that number of objects and will not work unless they ARE provided by the user." (:BR)(HTML "&nbsp;"))
     (:LI (:B "Keyword clauses:")
          " A function may also accept optional objects that are not necessary but if provided give additional information to the function." (:BR)(HTML "&nbsp;"))
     (:LI (:B "Flags:")
          " A function may also accept optional directives that changes the way the function operates."))

(:p "You might think of this format as verb - noun - modifiers.")

(:P "A function is like a black box: you feed it objects and by some magic it spits out an object on the other end (only our function boxes are yellow, not black, perhaps to dissipate the haze of magic by the light of reason). A function box fits into an object hole, because always produces an object.")

(:P "Let's fill an object hole with a function. Clear the workspace (by clicking the pink X button at the upper right of the workspace), and bring down a fresh data box (as described in the previous section). Open the box for input by clicking the middle of the box (the word 'object'), but don't type anything. Instead, bring down the RANDOM-INTEGER function, as shown below:")

(:ul (:img  :src "/weblistenerdocs/img-bbldoc/BBL_GS_get-random-integer.gif"))

(:P "The function fills the object hole, and when executed... well, execute it!")

(:P "...a number appears in the result pane just as it would if you had typed in the number by hand. This function produces a number as its result, and the result takes its place in the object box.")

(:TABLE (:TR 
(:TD (:SMALL
  (:p (:B (:U "Keywords"))(:BR)
   "Many functions, perhaps most, allow optional keyword clauses to supply the function with additional values and optional flags to modify the operation of the function. To see them, click the green option arrow in the function box. In the function you just brought down click the arrow and then click FROM (see right). Then repeat the operation and click TO. This should get you a function with two keyword boxes to fill in:")
))
(:TD
  (:ul (:img  :src "/weblistenerdocs/img-bbldoc/BBL_GS_random-integer-options.gif"))
)))

(:ul (:img  :src "/weblistenerdocs/img-bbldoc/BBL_GS_random-integer-from-to.gif"))

(:P "Enter numbers into the two boxes (being sure to press Tab or Enter after each number) and then execute the function as before. Try different numbers and execute the function enough times until you understand how it works.")

(:P (:B (:U "Nesting Functions")) (:BR)
"If all the language could do is perform a necessarily limited number of functions, it would be as impoverished as English would be if limited to simple subject-predicate-object sentences. But like English, BBL allows you to replace any object with a function that produces an object. This makes complex expression possible. For example:") 

(:ul (:img  :src "/weblistenerdocs/img-bbldoc/BBL_GS_sum-of-log10-from.gif"))

(:P "Notice the pleasing alternation of citrus colors to help you see the level of each nested function. Each simple function replaces itself with its result, so while you can read this complex function from left to right and gain some meaning from it, in fact, BBL reads it from the inside out:")

(:UL (:LI "(FROM 1 TO 100 ) replaces itself with the numbers from 1 to 100.")
     (:LI "Then (LOG10 ...) replaces itself with the list of logarithms (base 10) of those 100 numbers.")
     (:LI "Then (SUM-OF ...) replaces itself with the sum of those 100 logarithms.")
     (:LI "Then, since the box contains only an object, that number is returned to you."))

(:P "Here's a more biological example:")

(:ul (:img  :src "/weblistenerdocs/img-bbldoc/BBL_GS_mean-lengths-of-genes-of.gif"))

(:P "Again, presuming you understand that genes have lengths, you can make sense of this complex function just by reading it left to right. BBL, however, first delivers all the genes of the organism a7120, then it returns a list consisting of the lengths of the genes, and finally it returns the mean of those lengths.")

(:P (:B (:U "How to Find the Function You Need")) (:BR)
"The goal of the language is to match common biological concepts with simple functions. If you think that the operation you're looking for is one that people frequently need, there may be a BBL function that does the job. Try to put your need in words: \"I want the sequence of my favorite gene\"... sounds common enough. You should be able to find a function into which you can feed the name of your favorite gene and get back its sequence. But \"I want all amino acid sequences that happen to form English words\"... entirely do-able, but you'll probably have to figure out how to combine BBL functions to accomplish the task.")

(:P "Once you've put your need into words, how to find the appropriate BBL function? Here are some strategies:"
(:TABLE (:TR 
 (:TD (:SMALL 
  (:UL 
    (:LI "BROWSE THROUGH MENUS - The menus in the Menu Palette are arranged by topic. If you can fit your need into one of the topics, you may be able to find what you want quickly in the appropriate menu." (:BR)(HTML "&nbsp;"))

    (:LI "SEARCH - The HELP facility supports simple searches (complex search capabilities will come later). The facility will show you functions whose names or descriptions contain word you're searching for. It will also show you glossary entries for words, which may point you towards relevant functions." (:BR)(HTML "&nbsp;"))

    (:LI "EXAMPLES - If you find the right example in the Help facility's collection of biological examples, you may find within it the function you want."))))

  (:TD (:img  :src "/weblistenerdocs/img-bbldoc/BBL_GS_help-search.gif")))))

(:TABLE (:TR 
  (:TD (:SMALL
    (:B (:U "Learning About Specific Functions")) (:BR)
"It is possible to get help about any specific function by clicking on the Help item in the function's action menu (see right). This will first show you a one-line description of what the function does. To learn more, click on the Full Documentation link. Alternatively, you can get to documentation for any function by clicking on red Help button, then General Help, and finally Descriptions of Functions."))
  (:TD (:ul (:img  :src "/weblistenerdocs/img-bbldoc/BBL_GS_genes-of-help.gif")))))

)))) 

(:table :align "middle" :width "80%" (:b 
(:tr  
(:td  :width "26.7%" :align "left" 
  ((:a :style "text-decoration:none" 
    :href (:print (make-help-topic-url 
      :name "Syntactical forms in BioBIKE Language (BBL)")))
   (:button "Previous Page")))
(:td :width "26.7%" :align "middle"
  ((:a :style "text-decoration:none" 
    :href (:print (make-help-topic-url :name "Basic Syntax")))
    (:button "Up to Table of Contents")))
(:td :align "right" :width "26.7%"
  ((:a :style "text-decoration:none" 
    :href (:print (make-help-topic-url 
      :name "Textual representation of BBL forms")))
    (:button " Next Page")))) 
   
(:tr (:td :width "26.7%")
(:td :align "middle"  :width "26.7%"
  ((:a :style "text-decoration:none" 
    :href (:print (make-help-topic-url :name "HELP for BBL users")))
    (:button "Help for BBL Users")))
(:td :width "26.7%"))))
))

; *****************

(def-topic "Syntactical forms in BioBIKE Language (BBL)-Object exercises"
(:text 
(:table :align "middle" :width "80%"
(:tr (:td (:small

(:p (:b "A. "(:u "Introduction to BioBIKE Syntax")) (:BR)
    (:b "A.2. Syntactical forms in BioBIKE Language (BBL)"))

(:ol :TYPE :|a|
  (:li ((:a :style "color:#0000EE" :href (:print (make-help-topic-url 
  :name "Syntactical forms in BioBIKE Language (BBL)"))) 
    "Objects")
    " (" ((:A :STYLE "color:#0000EE" :href (:PRINT (make-help-topic-url
    :name "Syntactical forms in BioBIKE Language (BBL)-Object exercises")))
    (:font :color "red" "exercises")) ")" ) 
  (:li ((:a :style "color:#0000EE" :href (:print (make-help-topic-url 
    :name "Syntactical forms in BioBIKE Language (BBL)-Functions"))) 
    "Functions")
    " (" ((:A :STYLE "color:#0000EE" :href (:PRINT (make-help-topic-url
    :name "Syntactical forms in BioBIKE Language (BBL)-Function exercises")))
    "exercises") ")" ) 
)

(:P (:B "A.2.a. " (:U "Objects")) " - EXERCISES"(:BR))
   
(:OL
(:LI "Bring down a data box and cause an object -- your name -- to be put in the result pane." (:BR) (HTML "&nbsp;"))

(:LI "Bring down a data box and enter into it the following: \"A\" \"B\" \"C\"" 
     (:BR)"Execute (evaluate) the box. " (:BR)
     "What object appears in the result pane? " (:BR)
     "Why don't all three letters appear?"
     " (" ((:A :STYLE "color:#0000EE" 
               :href (:print (make-help-topic-url 
                        :name "Syntactical forms in BioBIKE Language (BBL)-Object exercises Hint 1"))) 
          "hint") ")"
     (:BR) (HTML "&nbsp;"))


(:LI "Which of the following are legal objects?" (:BR)
     (HTML "&nbsp;&nbsp;") " a. \"fruit\"" (:BR)
     (HTML "&nbsp;&nbsp;") " b. apple" (:BR)
     (HTML "&nbsp;&nbsp;") " c. \"29\"" (:BR)
     (HTML "&nbsp;&nbsp;") " d. (29 47 lemon)" (:BR)
     "If you're not sure, test your answer by copying and pasting the choice "
     "into a data box and evaluating it." (:BR)
     "(If you're surprised by the answer to d, click "
     ((:A :href (:print (make-help-topic-url 
                        :name "Syntactical forms in BioBIKE Language (BBL)-Object exercises Hint 2")))  "here") ")" )
)
)))) 

(:table :align "middle" :width "80%" (:b 
(:tr  
(:td  :width "26.7%" :align "left" 
  ((:a :style "text-decoration:none" 
    :href (:print (make-help-topic-url 
      :name "Syntactical forms in BioBIKE Language (BBL)")))
   (:button "Previous Page")))
(:td :width "26.7%" :align "middle"
  ((:a :style "text-decoration:none" 
    :href (:print (make-help-topic-url :name "Basic Syntax")))
    (:button "Up to Table of Contents")))
(:td :align "right" :width "26.7%"
  ((:a :style "text-decoration:none" 
    :href (:print (make-help-topic-url 
      :name "Textual representation of BBL forms")))
    (:button " Next Page")))) 
   
(:tr (:td :width "26.7%")
(:td :align "middle"  :width "26.7%"
  ((:a :style "text-decoration:none" 
    :href (:print (make-help-topic-url :name "HELP for BBL users")))
    (:button "Help for BBL Users")))
(:td :width "26.7%"))))
))
 
; *****************
(def-topic "Syntactical forms in BioBIKE Language (BBL)-Object exercises Hint 1"
(:text 
(:table :align "middle" :width "80%"
(:tr (:td (:small

(:p (:b "A. "(:u "Introduction to BioBIKE Syntax")) (:BR)
    (:b "A.2. Syntactical forms in BioBIKE Language (BBL)"))

(:ol :TYPE :|a| 
  (:li ((:a :style "color:#0000EE" :href (:print (make-help-topic-url 
  :name "Syntactical forms in BioBIKE Language (BBL)"))) 
    "Objects")
    " (" ((:A :STYLE "color:#0000EE" :href (:PRINT (make-help-topic-url
    :name "Syntactical forms in BioBIKE Language (BBL)-Object exercises")))
    (:font :color "red" "exercises")) ")" ) 
  (:li ((:a :style "color:#0000EE" :href (:print (make-help-topic-url 
    :name "Syntactical forms in BioBIKE Language (BBL)-Functions"))) 
    "Functions")
    " (" ((:A :STYLE "color:#0000EE" :href (:PRINT (make-help-topic-url
    :name "Syntactical forms in BioBIKE Language (BBL)-Function exercises")))
    "exercises") ")" ) 
)

(:P (:B "A.2.a. " (:U "Objects")) " - EXERCISES"(:BR))
   
(:OL :START 2
(:LI "Bring down a data box and enter into it the following: \"A\" \"B\" \"C\"" 
     (:BR)"Execute (evaluate) the box. " (:BR)
     "What object appears in the result pane? " (:BR)
     "Why don't all three letters appear?"
     " (" ((:A :STYLE "color:#0000EE" :href "bbl-syntax-hint1.txt") "hint") ")"
     (:BR) 
     (:BR)
     "------------------------------" (:BR)
     (:BR)
     (:FONT :COLOR "green" 
     (:I "Recall:"
     (:UL
       (:LI "A box can contain only one object")
       (:LI "A letter is a single object")
       (:LI "A list is a single object")
       (:LI "A list is bounded by parentheses"))
     (:P 
     "When evaluating the contents of a box, BBL searches for the first object." (:BR) 
     "Once it finds it, it discards any others.")
     (:P "Try changing the entry so that all three letters DO appear when the box is executed."))))
         
)
)))) 

(:table :align "middle" :width "80%" (:b 
(:tr  
(:td  :width "26.7%" :align "left" 
  ((:a :style "text-decoration:none" 
    :href (:print (make-help-topic-url 
      :name "Syntactical forms in BioBIKE Language (BBL)-Object exercises")))
   (:button "Previous Page")))
(:td :width "26.7%" :align "middle"
  ((:a :style "text-decoration:none" 
    :href (:print (make-help-topic-url :name "Basic Syntax")))
    (:button "Up to Table of Contents")))
(:td :align "right" :width "26.7%"
  ((:a :style "text-decoration:none" 
    :href (:print (make-help-topic-url 
      :name "Textual representation of BBL forms")))
    (:button " Next Page")))) 
   
(:tr (:td :width "26.7%")
(:td :align "middle"  :width "26.7%"
  ((:a :style "text-decoration:none" 
    :href (:print (make-help-topic-url :name "HELP for BBL users")))
    (:button "Help for BBL Users")))
(:td :width "26.7%"))))
))

; *****************

(def-topic "Syntactical forms in BioBIKE Language (BBL)-Object exercises Hint 2"
(:text 
(:table :align "middle" :width "80%"
(:tr (:td (:small

(:p (:b "A. "(:u "Introduction to BioBIKE Syntax")) (:BR)
    (:b "A.2. Syntactical forms in BioBIKE Language (BBL)"))

(:ol :TYPE :|a|
  (:li ((:a :style "color:#0000EE" :href (:print (make-help-topic-url 
  :name "Syntactical forms in BioBIKE Language (BBL)"))) 
    "Objects")
    " (" ((:A :STYLE "color:#0000EE" :href (:PRINT (make-help-topic-url
    :name "Syntactical forms in BioBIKE Language (BBL)-Object exercises")))
    (:font :color "red" "exercises")) ")" ) 
  (:li ((:a :style "color:#0000EE" :href (:print (make-help-topic-url 
    :name "Syntactical forms in BioBIKE Language (BBL)-Functions"))) 
    "Functions")
    " (" ((:A :STYLE "color:#0000EE" :href (:PRINT (make-help-topic-url
    :name "Syntactical forms in BioBIKE Language (BBL)-Function exercises")))
    "exercises") ")" ) 
)

(:P (:B "A.2.a. " (:U "Objects")) " - EXERCISES"(:BR))
   
(:OL :START 3
(:LI "Which of the following are legal objects?" (:BR)
     (HTML "&nbsp;&nbsp;") " a. \"fruit\"" (:BR)
     (HTML "&nbsp;&nbsp;") " b. apple" (:BR)
     (HTML "&nbsp;&nbsp;") " c. \"29\"" (:BR)
     (HTML "&nbsp;&nbsp;") " d. (29 47 lemon)" (:BR)
     "If you're not sure, test your answer by copying and pasting the choice "
     "into a data box and evaluating it." (:BR)
(:BR) 
     (:BR)
     "------------------------------" (:BR)
     (:BR)
     (:FONT :COLOR "green" 
     (:I "This one's subtle. Lemon by itself (without quotation marks) is not an object, but when placed within a list it " (:B "is") " an object."
       (:P (:U "Why isn't it an object by itself?")) 
       (:UL
         (:LI "It isn't a string - not enclosed by double quotes")
         (:LI "It isn't a number - letters aren't numbers"))
       (:P "It isn't any of the legal objects you know about. It is a symbol that refers to a variable (like LET x = 47), but that variable has never been defined. ERROR!")
       (:P (:U "Why is a list containing it legal?") (:BR)
   "Every item of a list must be a legal object, BUT the parentheses direct BBL to consider everything in the list literally. That's not surprising for 29 and 47. 29 and 47, literally, mean 29 and 47. But with lemon, treating it literally means that the list contains not the contents of the variable lemon (which doesn't exist) but rather the symbol 'lemon' itself. That does exist. Any symbol exists.")
       (:P "As I said, this is subtle. If you want to burn this page, go right ahead."))))
      
)
)))) 

(:table :align "middle" :width "80%" (:b 
(:tr  
(:td  :width "26.7%" :align "left" 
  ((:a :style "text-decoration:none" 
    :href (:print (make-help-topic-url 
      :name "Syntactical forms in BioBIKE Language (BBL)-Object exercises")))
   (:button "Previous Page")))
(:td :width "26.7%" :align "middle"
  ((:a :style "text-decoration:none" 
    :href (:print (make-help-topic-url :name "Basic Syntax")))
    (:button "Up to Table of Contents")))
(:td :align "right" :width "26.7%"
  ((:a :style "text-decoration:none" 
    :href (:print (make-help-topic-url 
      :name "Textual representation of BBL forms")))
    (:button " Next Page")))) 
   
(:tr (:td :width "26.7%")
(:td :align "middle"  :width "26.7%"
  ((:a :style "text-decoration:none" 
    :href (:print (make-help-topic-url :name "HELP for BBL users")))
    (:button "Help for BBL Users")))
(:td :width "26.7%"))))
))


; *****************

(def-topic "Syntactical forms in BioBIKE Language (BBL)-Function exercises"
(:text 
(:table :align "middle" :width "80%"
(:tr (:td (:small

(:p (:b "A. "(:u "Introduction to BioBIKE Syntax")) (:BR)
    (:b "A.2. Syntactical forms in BioBIKE Language (BBL)"))

(:ol :TYPE :|a|
  (:li ((:a :style "color:#0000EE" :href (:print (make-help-topic-url 
  :name "Syntactical forms in BioBIKE Language (BBL)"))) 
    "Objects")
    " (" ((:A :STYLE "color:#0000EE" :href (:PRINT (make-help-topic-url
    :name "Syntactical forms in BioBIKE Language (BBL)-Object exercises")))
    (:font :color "red" "exercises")) ")" ) 
  (:li ((:a :style "color:#0000EE" :href (:print (make-help-topic-url 
    :name "Syntactical forms in BioBIKE Language (BBL)-Functions"))) 
    "Functions")
    " (" ((:A :STYLE "color:#0000EE" :href (:PRINT (make-help-topic-url
    :name "Syntactical forms in BioBIKE Language (BBL)-Function exercises")))
    "exercises") ")" ) 
)

(:P (:B "A.2.a. " (:U "Functions")) " - EXERCISES"(:BR))
   
(:OL
 (:LI "What's 47 times 29?"
    (:UL
      (:LI "Find MULTIPLY on an appropriate menu")
      (:LI "In the appropriate boxes, type in a list consisting of the four numbers")
      (:LI "Note that commas must not be placed between numbers of a list"))
    (:BR) (HTML "&nbsp;"))

 (:LI "What are the squares of 1, 2, 3, and 4?" 
    (:UL
      (:LI "Use MULTIPLY again")
      (:LI "Take advantage of the BY option"))
    "(" 
    ((:A :STYLE "color:#0000EE" 
         :href (:print (make-help-topic-url 
                  :name "Syntactical forms in BioBIKE Language (BBL)-Function exercises Answer 2")))
         "Click here for answer") ")" 
    (:BR) (HTML "&nbsp;"))

 (:LI "Find the orthologs of the gene Npf0001. If you don't know what an ortholog is (and who does?), then use the search facility to learn about it and about functions that might be of use to you." (:BR)
    "(" 
    ((:A :STYLE "color:#0000EE" 
         :href (:print (make-help-topic-url 
                  :name "Syntactical forms in BioBIKE Language (BBL)-Function exercises Answer 3")))
             "Click here for answer") ")" 
    (:BR) (HTML "&nbsp;"))

 (:LI "How many genes are there in Anabaena PCC 7120 (nicknamed A7120), to the nearest 100 genes?" 
    (:UL
      (:LI "To find the genes of A7120, look in the GENOME menu for an appropriate function")
      (:LI "To find the counts of those genes, look in the LIST-TABLES menu, under List-analyze")
      (:LI "To round the number of genes to the nearest 100, look in the ARITHMETIC menu under Basic-arithmetic")
      (:LI "To learn how to use the rounding function, look at its help screen")
      (:LI "Finally, put all these functions together by nesting them"))
    "("
    ((:A :STYLE "color:#0000EE" 
         :href (:print (make-help-topic-url 
                  :name "Syntactical forms in BioBIKE Language (BBL)-Function exercises Answer 4")))
         "Click here for answer") ")" 
    (:BR) (HTML "&nbsp;"))
)
)))) 

(:table :align "middle" :width "80%" (:b 
(:tr  
(:td  :width "26.7%" :align "left" 
  ((:a :style "text-decoration:none" 
    :href (:print (make-help-topic-url 
      :name "Syntactical forms in BioBIKE Language (BBL)")))
   (:button "Previous Page")))
(:td :width "26.7%" :align "middle"
  ((:a :style "text-decoration:none" 
    :href (:print (make-help-topic-url :name "Basic Syntax")))
    (:button "Up to Table of Contents")))
(:td :align "right" :width "26.7%"
  ((:a :style "text-decoration:none" 
    :href (:print (make-help-topic-url 
      :name "Textual representation of BBL forms")))
    (:button " Next Page")))) 
   
(:tr (:td :width "26.7%")
(:td :align "middle"  :width "26.7%"
  ((:a :style "text-decoration:none" 
    :href (:print (make-help-topic-url :name "HELP for BBL users")))
    (:button "Help for BBL Users")))
(:td :width "26.7%"))))
))

; *********************

(def-topic "Textual representation of BBL forms"
(:text 
(:table :align "middle" :width "80%"
(:tr (:td (:small

(:p (:b "A. "(:u "Introduction to BioBIKE Syntax")))

(:ol
(:li ((:a :style "color:#0000EE" :href (:print (make-help-topic-url :name "English syntax as a model for the syntax of computer languages"))) "English syntax as a model for the syntax of computer languages"))
(:li ((:a :style "color:#0000EE" :href (:print (make-help-topic-url :name "Syntactical forms in BioBIKE Language (BBL)"))) "Syntactical forms in BioBIKE Language (BBL)"))
(:li ((:a :style "color:#0000EE" :href (:print (make-help-topic-url :name "Textual representation of BBL forms"))) (:font :color "red" "Textual representation of BBL forms")))) 

(:p (:b "3. Textual representation of BBL forms")) 
(:p "With colors and boxes, it is possible to represent BBL forms so that their structure is evident. However, at present, there are times where it is helpful to represent using text instead of boxes. For example, help screens have limited ability to show boxes and must often resort to text. For you to understand the help screens, you need to know how the two representations are related to each other.") 

(:P "Consider a nested function that you saw earlier:")

(:ul (:img  :src "/weblistenerdocs/img-bbldoc/BBL_GS_mean-lengths-of-genes-of.gif"))

(:P "If you take all the text from the boxes,you'd get:")

(:ul (:big (:pre "MEAN LENGTHS-OF GENES-OF a7120")))
 
(:p "The bare text is more or less intelligible in this case, but sometimes it is not. Even in the present situation there are ambiguities. You might intend MEAN to take the average of the three numbers contained by the variables named 'LENGTHS-OF', 'GENES-OF', and 'a7120'. How can we show unambiguously that 'LENGTHS-OF' is intend as a function, not a variable? Here's how.")

(:P "Rules:") 
(:ol :type "1"
(:li "Place a left parenthesis at the left side of any yellow or orange box (i.e. functions)") 
(:li "Place a right parenthesis at the right side of any yellow or orange box") 
(:li "Erase all the lines, colors, and icons, leaving only parentheses and text")) 
(:p "Applying these rules yields:") 

(:ul (:big (:pre "(MEAN (LENGTHS-OF (GENES-OF a7120)))"))) 

(:p "BBL translates boxes that you create into its internal language of parentheses. Humans can too, and in the opposite direction as well: when you see a left parenthesis, look for the corresponding right parenthesis, which may or may not be the nearest right parenthesis. Connect corresponding parentheses to form a box.") 

(:P "An important consequence of BBL syntax and the conversion rules is that in both graphical and text representations, the order is the same:") 

(:ul (:big (:pre "( Function-name   argument(s)   keyword-clause(s)   flag(s) )")))
)))) 

(:table :align "middle" :width "80%" (:b 
(:tr  
(:td  :width "26.7%" :align "left" ((:a :style "text-decoration:none" :href (:print (make-help-topic-url :name "Syntactical forms in BioBIKE Language (BBL)")))(:button " Previous Page")))
(:td :width "26.7%" :align "middle"((:a :style "text-decoration:none" :href (:print (make-help-topic-url :name "Basic Syntax")))(:button "Up to Table of Contents")))
(:td :align "right" :width "26.7%"((:a :style "text-decoration:none" :href (:print (make-help-topic-url :name "Forms")))(:button " Next Page")))) 
   
(:tr (:td :width "26.7%")
(:td :align "middle"  :width "26.7%"((:a :style "text-decoration:none" :href (:print (make-help-topic-url :name "HELP for BBL users"))) (:button "Help for BBL Users")))
(:td :width "26.7%"))))
)) 
