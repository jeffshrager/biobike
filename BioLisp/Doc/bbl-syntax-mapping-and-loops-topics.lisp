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
;;; Authors:   Emily Niman, Bogdan Mihai, Arnaud Taton, Jeff Elhai

(def-topic "iteration"
(:text 
(:table :align "middle" :width "80%"
(:tr (:td (:small

(:p (:b "A. "(:u "Introduction to Mapping and Loops")))

(:ol
(:li ((:a :style "color:#0000EE" 
:href  #.(UTILS::s+ wb::*doc-directory-url* "Basic-syntax-D1-Intro-mapping.pdf"))
  "The need for iteration and introduction to mapping")
     (:BR) "a. Why iteration?"
     (:BR) "b. Implicit mapping"
     (:BR) "c. Explicit mapping"
     (:BR) "(Exercises)" (:BR)(:BR))

(:li ((:a :style "color:#0000EE" 
:href #.(UTILS::s+ wb::*doc-directory-url* "Basic-syntax-D2-Intro-loops.pdf"))
 "Introduction to loops")
     (:BR) "a. Overview of loops by example"
     (:BR) "b. Overview of anatomy of the loop"
     (:BR) "c. Primary iteration control"
     (:BR) "d. Additional control section"
     (:BR) "e. Initialization section"
     (:BR) "f. Variable update section"
     (:BR) "g. Body"
     (:BR) "h. Results section"
     (:BR) "i. Final action"
     (:BR) "(Exercises)"(:BR)(:BR))
)))))

(:table :align "middle" :width "80%" (:b 
(:tr  (:td  :width "26.7%" :align "left" ((:a :style "text-decoration:none" :href (:print (make-help-topic-url :name "Basic Syntax")))(:button "Previous Page")))
(:td :width "26.7%" :align "middle"((:a :style "text-decoration:none" :href (:print (make-help-topic-url :name "Basic Syntax")))(:button "Up to Table of Contents")))
(:td :align "right" :width "26.7%"((:a :style "text-decoration:none" :href (:print (make-help-topic-url :name "English syntax as a model for the syntax of computer languages")))(:button " Next Page")))) 
   
(:tr (:td :width "26.7%")
(:td :align "middle"  :width "26.7%"((:a :style "text-decoration:none" :href (:print (make-help-topic-url :name "HELP for BBL users"))) (:button "Help for BBL Users")))
(:td :width "26.7%")))) 
))
(def-topic "Mapping and Loops"(:text (:table :align "middle" :width "80%"(:tr (:td (:small (:p "Working with large numbers of items: Mapping and Loops") 
(:p "Bioinformatics, by definition (to the extent it has a definition), means considering large data sets: large sequences, large numbers of genes, large sets of measurements. You already confronted the problem raised by large data sets in the first guided tour, What is a gene? in which you wanted to find out what sequence begins genes. It wouldn’t have done any good, of course, to examine just one gene. You could have examined many, one at a time…")
 (:ul (:pre (:big"(SEQUENCE-OF pmm0001 FROM 1 TO 10)(SEQUENCE-OF pmm0002 FROM 1 TO 10)(SEQUENCE-OF pmm0003 FROM 1 TO 10)...(SEQUENCE-OF pmm1655 FROM 1 TO 10)")))
  (:p "… but that wouldn’t have been practical at all! How can you get the sequences from all 1900+ genes without killing yourself?BioBIKE language (BBL) allows you to choose between two approaches to handle repetitive operations: mapping and loops. Mapping is generally simpler, so simple that you may use it without realizing it. Loops are more general, helpful when direct mapping of a process is not available.")
 (:p (:b "I. Mapping"))
 (:p "You're starting off in a new job working in a library. Your new supervisor tells you the first thing to do is to put magnetic strips on all the new books that have come in. Here's how… and she shows you how to put a magnetic strip in a book. Now do that for this stack of books.If you've done something like this, then you know how to map a function, that is, to apply the function defined for one object to a list of objects. You’ve already had experience with mapping within BioBIKE. Instead of typing out 1900 lines of code to extract sequences from 1900 genes, you applied the SEQUENCE-OF function to all genes at once:")
(:ul (:pre (:big "(SEQUENCE-OF pro0029 FROM 1 TO 10)")))
  (:ul (:i "; the function applied to one object."))
 (:ul (:pre (:big  "(SEQUENCE-OF (GENES-OF ss120) FROM 1 TO 10)")))
  (:ul (:i "; the function applied to a list of objects."))
 (:p "What you did was to map the function SEQUENCE-OF over the set of ss120 genes. Because BioBIKE was designed with bioinformatics in mind, it is generally possible to map BioBIKE functions over sets in this way, whenever mapping makes sense.Here’s another example. Suppose you want to find out what’s the average size of a protein in Prochlorococcus ss120. This requires that you calculate the molecular weight of each protein, sum the molecular weights, and divide the sum by the total number of proteins. With mapping, the strategy can be translated into BBL code directly:")
 (:ul (:img :src "/weblistenerdocs/img-bbldoc/BBL_Mapping.jpg"))
 (:p "Or in text form:")
 (:ul (:pre (:big "(/ (SUM-OF (MW-OF (PROTEINS-OF ss120)))	   (COUNT-OF (PROTEINS-OF ss120)))")))
 (:p "Recall that division, like all BioBIKE functions, begins with the name of the function, followed by the items the function acts on. Of course, you could do this in multiple steps, perhaps more intelligibly, executing each line one at a time:")
 (:ul (:pre (:big "(DEFINE protein-MWs AS (MW-OF (PROTEINS-OF ss120)))(DEFINE total-MW AS (SUM-OF protein-MWs))(DEFINE protein-count AS (COUNT-OF (PROTEINS-OF ss120))(/ total-MW protein-count)")))
 (:p "When mapping works, it often makes a process simple to write and simple to understand. Some processes, however, cannot be mapped. Then loops can save the day.")
 (:p (:b "II. Loops") 
(:p
 (:b "II.A. Overview of loops by example")
 (:p "Mapping is simple: just replace a single item with a set of items. In contrast, looping uses what seems like a separate language. A loop executes one set of instructions repeatedly. Each time through the instructions is called an iteration. Here’s the previous example rendered as a loop:")
 (:ul (:pre (:big "(FOR-EACH protein IN (PROTEINS-OF ss120)     SUM (MW-OF protein))")))
 (:i(:p (:u "Translation:")) (:ol :type "a"(:li "Consider each protein in the set of all proteins of ss120, one at a time.")(:li "Accumulate the molecular weights of each protein.")(:li "When the last protein has been considered, return the sum ")))
 (:p "That gets you the total molecular weight. Or this code gets you the entire answer by means of a more complicated loop:")
 (:ul (:pre (:big "(FOR-EACH protein IN (PROTEINS-OF ss120)     INITIALIZE total-MW = 0     INITIALIZE protein-count = (COUNT-OF (PROTEINS-OF ss120))     AS mw = (MW-OF protein)     (INCREMENT total-MW BY mw)     FINALLY (RETURN (/ total-MW protein-count)))")))
 (:i(:p (:u "Translation:")) (:ol :type "a"(:li "Consider each protein in the set of all proteins of ss120, one at a time.") (:li "Before the loop begins, set the sum of molecular weights to zero. The initialization occurs only once.") (:li "Before the loop begins, set the number of proteins. This will be a constant.") (:li "Find the molecular weight of the one protein you’re considering at the moment. This assignment is repeated each time through the loop.") (:li "Add that molecular weight to the growing total.") (:li "(Loop) Repeat steps d and e until you’ve considered each protein in the set.") (:li "When you’ve finished considering each protein, calculate the average molecular weight and use that as the value returned by the FOR-EACH function.")))
 (:p "Those of you who are familiar with loops from other computer languages may have been expecting something more along the lines of:")
 (:ul (:pre (:big "(FOR-EACH n FROM 1 TO 10     (DISPLAY-LINE n *tab* (* n n)))")))
 (:i(:p (:u "Translation:")) (:ol :type "a"(:li "Consider each number n from 1 to 10, one at a time.")(:li "Display the number you’re considering at the moment as well as its square.")(:li "(Loop) Repeat step b with a new n each time until n reaches 10.")))
 (:p "BBL can do this kind of loop, but they’re not common in bioinformatics applications. It’s more common to go through lists of things, like genes or organisms, as in the first example.")
(:p (:b "II.B. Anatomy of the loop"))
 (:p "Loops can be divided into the following (mostly optional) parts:")
(:ul(:li (:b "Iteration control:") " Determines how the loop begins and ends and sets up the iteration variable")(:li (:b "Loop-specific initialization:") " Set variables before the loop begins, to be available throughout the lifetime of the loop")(:li (:b "Iteration-specific assignment:") " Set temporary variables used within one iteration")(:li (:b "Body:") " Instructions to be executed each iteration")(:li (:b "Return value:") " Set or accumulate values to be returned when the loop is finished")(:li (:b "Final actions:") " One last hurrah before the loop function is completed"))
 (:p "There are two loop functions supported by BBL: LOOP and FOR-EACH (a special case of LOOP). The latter is diagrammed on the next page. Don't be put off by the complex possibilities. Let this page serve as a reference. For now, it might be prudent for you to gain familiarity with a small fraction of it rather than try to comprehend the whole at once.")
(:ul (:img :src "/weblistenerdocs/img-bbldoc/BBL_Loops.jpg"))
 (:p
 (:b "II.C. Iteration control")
(:p "Loops repeat instructions over and over. How many times? This may be determined by the length of a list:")
 (:ul (:pre (:big "(FOR-EACH organism IN *all-organisms*     (DISPLAY-LINE organism *tab* (LENGTH-OF organism)))")))
 (:p "If BioBIKE knows of 13 organisms, then the body of the loop will execute 13 times, and the loop variable organism will assume the identity of each of the organisms, one at a time, for each time through the loop. Copy and paste the loop into BioBIKE and see what it does.")
 (:p "The iterations of the loop may be controlled by numbers as well:")
 (:ul (:pre (:big "(FOR-EACH coordinate FROM 1 TO (LENGTH-OF all4312) BY 3     AS end-of-codon = (+ coordinate 2)     AS codon = (SEQUENCE-OF all4312 FROM coordinate TO end-of-codon)     AS aa = (TRANSLATION-OF codon)     COLLECT {codon aa} )")))
  (:i (:p (:u "Translation:")) (:ol :type "a"(:li "Consider each coordinate starting with 1, then 4, then 7, … until you’ve exceeded the length of the gene all4312")(:li "Each time through the loop, determine the end of the codon, by adding 2 to the current coordinate")(:li "Each time through the loop, determine the codon by extracting the sequence from the gene all4312, from the coordinate to the end of the codon")(:li "Translate the codon sequence to an amino acid")(:li "Add to a growing list of codons and corresponding amino acids")(:li "(Loop) Repeat steps b through e for each coordinate")))
 (:p "If you are not clear about how this loop works, then ask the program to tell you its inner thoughts as it goes. This is often a good strategy to see how code works:")
 (:ul (:pre (:big " (FOR-EACH coordinate FROM 1 TO (LENGTH-OF all4312) BY 3     AS end-of-codon = (+ coordinate 2)     AS codon = (SEQUENCE-OF all4312 FROM coordinate TO end-of-codon)     AS aa = (TRANSLATION-OF codon)     "      (:font :color "red" "(DISPLAY-DATA coordinate end-of-codon codon aa)")      "     COLLECT {codon aa})")))
 (:p "This loop now displays all of the iteration-variables as it goes along, so you can see the loop in action.")
 (:p "Or the loop may be open ended, continuing until a condition is met:")
(:ul (:pre (:big  (:font :color "red" "(FOR-EACH coordinate FROM 1 BY 3     UNTIL (> coordinate (LENGTH-OF all4312))")      "     AS end-of-codon = (+ coordinate 2)     AS codon = (SEQUENCE-OF all4312 FROM coordinate TO end-of-codon)     AS aa = (TRANSLATION-OF codon)     COLLECT {codon aa})")))
 (:i(:p (:u "Translation:")) (:ol :type "a"(:li "Consider each coordinate starting with 1, then 4, then 7, …open ended for the moment")(:li "Continue through the loop only so long as the coordinate currently under consideration is less than the length of the gene")(:li "Extract from the gene the triplet codon at the position of the coordinate")(:li "Add to a growing list of codons and corresponding amino acids")(:li "(Loop) Repeat steps c and d for each coordinate")))
 (:p "But be warned! Open ended loops are open to abuse:")
(:ul (:pre (:big "(LOOP INITIALIZE hell-freezes-over = false       UNTIL hell-freezes-over       (DISPLAY \"It’s hot down here!\"))")))
 (:p "Don't do it!!! This loop will go on forever… until BioBIKE reaches the conclusion that you’re yanking its chain and stops the proceedings.")
 (:p "Note that LOOP differs from FOR-EACH in that it allows you to construct loops that don't use iteration variables. Another difference illustrated in this example is that LOOP, unlike FOR-EACH, insists that all initialization clauses precede all iteration clauses.")
(:p (:b "II.D. Loop-variable initialization"))
 (:p "Variables may be initialized before the loop begins using the INITIALIZE variable = value clause (INIT is a legal nickname). Variables thus initialized are created at the beginning of the loop and destroyed when the loop is finished. This is valuable when you want to initialize a variable that will be incremented by each iteration.")
(:p "For example:")
(:ul (:pre (:big "(FOR-EACH gene IN (GENES-OF ss120)"     (:font :color "red" "     INITIALIZE ATG-count = 0     INITIALIZE non-ATG-count = 0")      "     AS start-codon = (SEQUENCE-OF gene FROM 1 TO 3)     (IF-TRUE (SAME start-codon \"ATG\")         THEN (INCREMENT ATG-count)         ELSE (INCREMENT non-ATG-count))     FINALLY (RETURN (LIST ATG-count non-ATG-count)))")))
 (:i(:p (:u "Translation:")) (:ol  :type "a"(:li "Consider each gene  within the set of genes of Prochlorococcus marinus SS120 ")(:li "Initialize to zero two variables, one used to count ATG start codons and the other to count all other start codons")(:li "Extract the start codon from the gene under consideration")(:li "If the start codon is “ATG”, then add 1 to the number of ATG start codons")(:li "Otherwise add 1 to the number of non-ATG start codons")(:li "(Loop) Repeat steps c through e until the set of genes has been exhausted.")))
 (:p "Note that the initializations (step b) are NOT repeated.")
(:p "If you mistakenly tried to initialize the two variables with an AS clause (see next section), the results will not be as you might hope (try it!).")
 (:p 
(:b "II.E. Loop-variable assignment")
 (:p "The values of variables may be revised each iteration using the AS variable = value clause. Variables thus assigned persist only for one iteration and then are destroyed. You therefore want to use this clause to initialize variables whose values differ from one iteration to the next.")
 (:ul (:pre (:big "(FOR-EACH gene IN (ORTHOLOGS-OF all4312)"     (:font :color "red" "      AS organism = (ORGANISM-OF gene)     AS length = (LENGTH-OF gene)")     "      AS short-descr = (FIRST 20 IN (DESCRIPTION-OF gene))     COLLECT {organism gene length short-descr})")))
 (:i(:p (:u "Translation:")) (:ol  :type "a"(:li "Consider each gene  amongst those with shared evolutionary antecedents as all4312")(:li "Identify the organism of the gene")(:li "Identify the length of the gene ")(:li "Identify the first 20 characters of the description of the gene")(:li "Add to a growing list of identifiers the information collected for the gene under consideration")(:li "(Loop) Repeat steps b through f until the set of genes has been exhausted. ")))
 (:p "Note that the assignments (steps b through d) are redone each iteration.")
(:p (:b "II.F. Body"))
 (:p "The body of the loop is defined as the collection of statements not governed by a keyword. This collection must be in one piece, must come after all iteration, initialization, and assignment clauses, and must precede the accumulation clause and final action (if they exist). The body may be preceded by the optional keyword DO, if you think that aids readability. You can put any number of statements you like in the body, and each is executed during each iteration. Here’s an example, illustrating the difference between initializing a loop-variable and assigning a value to a loop-variable within an iteration.")
 (:ul (:pre (:big "(LOOP INITIALIZE position = 1      INITIALIZE letters = \"ABCDE\"      INITIALIZE initialized-variable = letters[position]      UNTIL (> position 5)      AS assigned-variable = letters[position]"     (:font :color "red" "       DO (INCREMENT position)         (DISPLAY-LINE \"Here I am in the loop with the \"               \"initialized variable = \" initialized-variable               \" and the changing variable = \"               assigned-variable))"))))
 (:p (:b "II.G. Result accumulation"))
 (:p "Like all BioBIKE functions, loops return a value. If you pay no attention to what it returns -- for example if you’re concerned only what the loop does in its body (as in the last example) -- then NIL will be returned. More often, however, you’ll want the loop to return a value or a list of values. You’ve already seen several examples where a list of values was made and ultimately returned using the COLLECT clause. Here are three other ways of returning values:")
(:ul(:li "RETURN, which immediately exits the loop and returns a given value")(:li "COUNT, which returns the number of times the clause is invoked")(:li "SUM, which returns the sum of a number of items."))
 (:p "Note in the following examples how each of these methods (and COLLECT) work with WHEN, which governs whether or not the following clause is executed.")
(:p "; Find very large genes")
(:ul (:pre (:big "(FOR-EACH gene IN (GENES-OF A7120)     AS length = (LENGTH-OF gene)"     (:font :color "red" "      WHEN")" (> length 2000)"     (:font :color "red" "        COLLECT")" {gene length})")))
  (:p "; Count very small proteins")
(:ul (:pre (:big "(FOR-EACH protein IN (PROTEINS-OF ss120)     AS MW = (MW-OF protein)"     (:font :color "red" "      WHEN")" (<= MW 10000)"     (:font :color "red" "        COUNT")" protein)")))
  (:p "; How many nucleotides are in small genes?")
(:ul (:pre (:big "(FOR-EACH gene IN (GENES-OF ss120)     AS length = (LENGTH-OF gene)"     (:font :color "red" "      WHEN")" (< length 2000)"     (:font :color "red" "        SUM")" length)")))
 (:p "; What's the first small gene?")
(:ul (:pre (:big "(FOR-EACH gene IN (GENES-OF ss120)     AS length = (LENGTH-OF gene)"     (:font :color "red" "      WHEN")" (< length 2000)       ("(:font :color "red" "RETURN")" gene))")))
 ))))
))))(:p) 
(:table :align "middle" :width "80%"
 (:b (:tr 
 (:td  :width "26.7%" :align "left" ((:a :style "text-decoration:none" :href (:print (make-help-topic-url :name "BioBIKE Principles")))(:button " Previous Page")))
(:td :width "26.7%" :align "middle"((:a :style "text-decoration:none" :href (:print (make-help-topic-url :name "Basic Syntax")))(:button "Up to Table of Contents")))
(:td :align "right" :width "26.7%"((:a :style "text-decoration:none" :href (:print (make-help-modules-url)) :target "blank")(:button " Next Page")))
)
  (:tr 
   (:td :width "26.7%")
   (:td :align "middle"  :width "26.7%"((:a :style "text-decoration:none" :href (:print (make-help-topic-url :name "HELP for BBL users"))) (:button "Help for BBL Users")))
   (:td :width "26.7%"))
)))) 
