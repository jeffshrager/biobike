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

;;; Authors: Joe Anderson, Victor Clarke, Jeff Elhai, Bogdan Mihai, Craig Noe
;;; Arnaud Taton, Robel Wolde

;===================================DECREMENT===============================
(DOCUMENT-FUNCTION DECREMENT
(:SUMMARY
	"Decrements entity by 1 or by a specified value"
)

(:SYNTAX
	(DECREMENT entity [BY any])
)

(:PARAMETERS
	(ENTITY 
	:PARAMETER-TYPE REQUIRED   
	)

	(BY 
	:PARAMETER-TYPE KEYWORD 
	:VALUE-TYPE any 
	:DOCSTRING "The value assigned to decrement"
	)
)
	
(:RETURNS
	"a number"
)

(:EXAMPLES 
	(:FOO
	(:P (:B "1. Decrementation of a variable:"))
            (:blockquote
      	      (:img :src "/weblistenerdocs/bbldf/images/DECREMENT-1.jpg")
	      (:blockquote
                (:img :src "/weblistenerdocs/bbldf/images/DECREMENT-2.jpg")))
	(:P (:B "2. Decrementation of a variable with a certain value ( 3 ) :"))
            (:blockquote
      	      (:img :src "/weblistenerdocs/bbldf/images/DECREMENT-3.jpg")
	      (:blockquote
                (:img :src "/weblistenerdocs/bbldf/images/DECREMENT-4.jpg")))
		(:P (:B "3. Decrementation of a variable that has been given a previous value : "))
            (:blockquote
      	      (:img :src "/weblistenerdocs/bbldf/images/DECREMENT-5.jpg")
	      (:blockquote
                (:img :src "/weblistenerdocs/bbldf/images/DECREMENT-6.jpg")))
		(:blockquote
      	      (:img :src "/weblistenerdocs/bbldf/images/DECREMENT-7.jpg")
	      (:blockquote
                (:img :src "/weblistenerdocs/bbldf/images/DECREMENT-8.jpg")))
				))
				
(:TEXT

(:P (:LI "1. Decrementation can be done only on variables or lists of variables"(:BR)(:BR)))

(:P (:LI "2. The initial value would decremented with 1"(:BR)(:BR)))

(:P (:LI "3. Use to decrement a function that returns a number"(:BR)(:BR)))
)

(:KEYWORDS)

(:SEE-ALSO)

(:REFERRED-TO-BY INCREMENT FROM)
)

;;================================= DEFINE ==============================

(DOCUMENT-FUNCTION DEFINE
(:PARAMETERS
 (target :DOCSTRING "variable that is assigned the value")
 (assignment :DOCSTRING "the new value" )
 (As\,=\,<- 
  :PARAMETER-TYPE keyword :DOCSTRING "Keyword for assigning the value")
 (display-off
  :PARAMETER-TYPE flag :DOCSTRING "When specified, display is suppressed")
 (labeled 
  :parameter-type flag :docstring "If LABELED is provided, ASSIGNMENT must be a string, and the value assigned to TARGET is coerced into a LABELED-STRING whose
label is TARGET.")) 
(:RETURNS "Assigned value or a notification string if DISPLAY-OFF is enabled.")
(:EXAMPLES
 (:FOO
  (:P (:B "1. Simplistic example."))
  (:blockquote
   (:img :src "/weblistenerdocs/bbldf/images/DEFINE-1.jpg")
   (:blockquote
    (:img :src "/weblistenerdocs/bbldf/images/DEFINE-2.jpg")))
  (:P (:B "2. Typical usage."))
  (:blockquote
   (:img :src "/weblistenerdocs/bbldf/images/DEFINE-3.jpg")
   (:blockquote
    (:img :src "/weblistenerdocs/bbldf/images/DEFINE-4.jpg")))
  (:P (:B "3. Simplistic example."))
  (:blockquote
   (:img :src "/weblistenerdocs/bbldf/images/DEFINE-5.jpg")
   (:blockquote
    (:img :src "/weblistenerdocs/bbldf/images/DEFINE-6.jpg")))
  (:P (:B "4. Definition of a variable."))
  (:blockquote
   (:img :src "/weblistenerdocs/bbldf/images/DEFINE-7.jpg")
   (:P (:B "The defined variables are located under the VARIABLES button:"))    
   (:blockquote
    (:img :src "/weblistenerdocs/bbldf/images/DEFINE-8.jpg"))
   (:P (:B "Demonstration of potential usage:")) 
   (:blockquote
    (:img :src "/weblistenerdocs/bbldf/images/DEFINE-9.jpg")))
  )
 ((define a-label "This is a label" labeled)
  "#S(LABELED-SEQUENCE :LABEL \"A-LABEL\" :SEQUENCE \"This is a label\")")
 ((define my-variable (loop for j from 1 to 1000 collect j) display-off)
  "\"List of length 1000 suppressed\"")
 ((define my-table[5] = 4) 4
  "Creates a 5 element table whose 5th element is 4 and other elements are NIL."
  )
 )
(:TEXT
(:p " 1. The function is similar to ASSIGN, but declares to the world that this variable is global.  DEFINE also executes much more slowly than ASSIGN and 
should not be used within loops or within function definitions.")
(:p " 2. It may be useful to reserve DEFINE for the first definition of a variable and ASSIGN for subsequent assignments.")
(:p " 3. The display of the output can be avoided by using DISPLAY-OFF. This is useful when you have long lists to define."))
(:SEE-ALSO ASSIGN ))


;==================================DESCRIPTION-OF===============================
(DOCUMENT-FUNCTION DESCRIPTION-OF
(:RETURNS "Description of given gene or protein as a string. Returns NIL if no description available")
(:SUMMARY "Returns best description of given gene[s] or protein[s]")
(:PARAMETERS (gene-or-protein :docstring "name of gene or protein (or list of same)" :parameter-type required)
(length :docstring "The number of characters to be displayed")
(display :docstring "If specified, returns the result in a labeled format")
(labeled :docstring "Shows the name of the gene with description"))
(:EXAMPLES
(:FOO
(:P (:B "1. Protein description"))
       (:blockquote
	 (:img :src "/weblistenerdocs/bbldf/images/DESCRIPTION-1.jpg")
	      (:blockquote
                (:img :src "/weblistenerdocs/bbldf/images/DESCRIPTION-2.jpg"))) 
(:P (:B "2. Organism description"))
       (:blockquote
	 (:img :src "/weblistenerdocs/bbldf/images/DESCRIPTION-3.jpg")
	      (:blockquote
                (:img :src "/weblistenerdocs/bbldf/images/DESCRIPTION-4.jpg")))
(:P (:B "3. Descriptions of a list of entities and usage of flag DISPLAY"))
       (:blockquote
	 (:img :src "/weblistenerdocs/bbldf/images/DESCRIPTION-5.jpg")
	      (:blockquote
                (:img :src "/weblistenerdocs/bbldf/images/DESCRIPTION-6.jpg")))
(:P (:B "4. Usage of flag LABELED"))
       (:blockquote
	 (:img :src "/weblistenerdocs/bbldf/images/DESCRIPTION-7.jpg")
	      (:blockquote
                (:img :src "/weblistenerdocs/bbldf/images/DESCRIPTION-8.jpg")))
))

(:TEXT (:p "Looks at fields in the following order: Annotation, Best-hit-descr EC-description COG-description")
(:p "Returns first field that has a value")))


;======================================DIFFERENCE-OF========================================

(DOCUMENT-FUNCTION DIFFERENCE-OF
(:SUMMARY "Subtracts one or more numbers from the first number (or subtracts a list)")
(:SYNTAX (DIFFERENCE-OF list))
(:PARAMETERS
(number 
			:DOCSTRING "the number or list that are subtracted from one another"
			:PARAMETER-TYPE required
			:VALUE-TYPE number/list
		))
(:RETURNS "number")
(:EXAMPLES "Detailed below, graphically.")
(:TEXT
"Both arguments can be numbers or functions that return a number or a list of numbers." (:br)
 "A list can appear in either position."
	(:OL
		(:LI "Difference of numbers :")
			(:p "")
			(:img :src "/weblistenerdocs/bbldf/images/differenceof1.jpg")
			(:p "Returns:")
			(:img :src "/weblistenerdocs/bbldf/images/differenceof2.jpg")
			(:br)(:br)(:br)
		(:LI "Number subtracted from a list :")
			(:p "")
			(:img :src "/weblistenerdocs/bbldf/images/differenceof5.jpg")
			(:p "Returns:")
			(:img :src "/weblistenerdocs/bbldf/images/differenceof6.jpg")
			(:br)(:br)(:br)
		(:LI "Difference of lists of numbers :")
			(:p "")
			(:img :src "/weblistenerdocs/bbldf/images/differenceof3.jpg")
			(:p "Returns:")
			(:img :src "/weblistenerdocs/bbldf/images/differenceof4.jpg")
			(:br)(:br)(:br)
		(:LI "Difference of arithmetical operations :")
			(:p "")
			(:img :src "/weblistenerdocs/bbldf/images/differenceof9.jpg")
			(:p "Returns:")
			(:img :src "/weblistenerdocs/bbldf/images/differenceof10.jpg")
	)
)
(:KEYWORDS arithmetic subtraction)
(:SEE-ALSO - SUM-OF PRODUCT-OF QUOTIENT-OF CALC))


; ================= DINUCLEOTIDE-BIASES-OF ====================

(DOCUMENT-FUNCTION Dinucleotide-biases-of
(:SUMMARY "Calculates dinucleotide biases of a sequence")
(:PARAMETERS
(entity :DOCSTRING "sequence to be analyzed")
(labeled :DOCSTRING "precede biases with name of sequence")
(label-biases :DOCSTRING "precede each bias with name of dinucleotide")
(one-strand :DOCSTRING "do not analyze complementary strand")
)
(:RETURNS "A score or list of scores")

(:EXAMPLES
	(:foo
	(:P (:B "1. Using an organism"))
       (:blockquote
	 (:img :src "/weblistenerdocs/bbldf/images/DINUC-BIAS-1.jpg")
	      (:blockquote
                (:img :src "/weblistenerdocs/bbldf/images/DINUC-BIAS-2.jpg"))) 
	(:P (:B "2.  Usage of flag LABEL BIASES"))
       (:blockquote
	 (:img :src "/weblistenerdocs/bbldf/images/DINUC-BIAS-3.jpg")
	      (:blockquote
                (:img :src "/weblistenerdocs/bbldf/images/DINUC-BIAS-4.jpg"))) 
	(:P (:B "3.  Usage with multiple organisms"))
       (:blockquote
	 (:img :src "/weblistenerdocs/bbldf/images/DINUC-BIAS-5.jpg")
	      (:blockquote
                (:img :src "/weblistenerdocs/bbldf/images/DINUC-BIAS-6.jpg")))
))				
	

(:TEXT
(:UL
(:LI
(:U "Meaning of bias")
": The bias of a dinucleotide within a sequence is the ratio of the observed frequency of the dinucleotide to the product of the frequencies of its component mononucleotides:"
(:PRE
" freq of XY in seq*
bias of XY in seq* = -------------------------------------
(freq of X in seq*)(freq of Y in seq*)")
"where seq* is both strands of a sequence (unless ONE-STRAND is specified). In a random sequence, the bias will be close to 1. In a biological sequence, an underrepresented dinucleotide will have a bias between 0 and 1, while an overrepresented dinucleotide will have a bias greater than 1."(:BR)(:BR))

(:LI (:U "Order of biases")
": The function returns 16 numbers, representing the biases of the 16 dinucleotides given in alphabetical order: \"AA\", \"AC\", \"AG\",... (this is the same order returned by the function ALL-DNA-SEQUENCES). Complementary dinucleotides (e.g. \"AC\" and \"GT\") will have the same biases, unless ONE-STRAND was specified, because for every dinucleotide on one strand there is a corresponding complementary dinucleotide on the other."(:BR)(:BR))

(:LI (:U "ONE-STRAND")
": If the ONE-STRAND option is used, then only one strand of the given sequence is analyzed (the given strand, upper strand, or forward strand). Otherwise the results of analyzing both strands are averaged. Generally it makes more biological sense to analyze both strands unless one is specifically examining strand differences."(:BR)(:BR))

(:LI (:U "Format of results; LABELED and LABEL-BIASES")
": The results of the analysis are returned as a list consisting of 16 numbers, one for each dinucleotide. If LABELED is specified, the list is preceded by the name of the sequence. If LABEL-BIASES is specified, then each bias is preceded by the dinucleotide that produced it."
(:PRE
" No flag (bias1 bias2 bias3...)
LABELED (\"name of sequence\" (bias1 bias2 bias3...))
LABEL-BIASES ((\"AA\" bias1)(\"AC\" bias2)...)" )

)
)
))

; ================= DINUCLEOTIDE-COMPARISON ====================

(DOCUMENT-FUNCTION Dinucleotide-comparison
(:SUMMARY "Calculates dinucleotide biases of a sequence and compares it to those of another sequence")
(:PARAMETERS
(entity1 :DOCSTRING "sequence or set of sequences to be compared with entity2")
(entity2 :MAPPING-STYLE Mapcar :DOCSTRING "sequence or set of sequences to be compared with entity1")
(labeled :DOCSTRING "precede comparison score with name of entity2")
; (one-strand :DOCSTRING "do not aalyze complementary strand")
)
(:RETURNS "A score or list of scores, each optionally preceded by the name of entity2")

(:EXAMPLES
("(DINUCLEOTIDE-COMPARISON mx8 TO mx8)"
"0")

("(DINUCLEOTIDE-COMPARISON mx8 TO *known-viruses* LABELED)"
"((\"NC_007409\" 0.12813489) (\"NC_005830\" 0.13634507) (\"NC_001447\" 0.1513235)...)")

)

(:TEXT
(:UL
(:LI
(:U "Meaning of comparison score")
": The function produces a score for each entity1/entity2 pair that is the difference in their dinucleotide biases, averaged over all 16 dinucleotides (see DINUCLEOTIDE-BIAS-Of for a discussion of what bias means). The score is calculated according to:"
(:PRE
" SUM | bias1(XY) - bias2(XY) |
comparison score = -------------------------------
16")
"where bias1(XY) is the bias of the dinucleotide XY in entity1 and bias2(XY) is similarly defined for entity2. The comparison score is therefore 0 when the sequences are identical and an increasing positive number as the dinucleotide biases of the two entities become more discordant. It is yours to judge for yourself whether a comparison score differs more from 0 than what might be expected from a random sequence. Bias is calculated over both DNA strands of each entity." (:BR)(:BR))

(:LI (:U "Entity1 and entity2")
": Entity1 and entity2 can be anything that is associated with a DNA sequence, including an explicit sequence. In addition, entity2 may be a set of biases, as provided by DINUCLEOTIDE-BIASES-OF."(:BR)(:BR))

(:LI (:U "Format of results; LABELED")
": The results of the analysis as a score (if entity1 and entity2 are single entities) or a list. If LABELED is specified, then each score is preceded by the name of entity2."(:BR)(:BR))

)
))
;============================DISPLAY==============================
(DOCUMENT-FUNCTION DISPLAY
(:SUMMARY "Displays given text on screen with optional formatting ." )
(:SYNTAX (display text))
(:PARAMETERS
(text :VALUE-TYPE number\,string\,list  :DOCSTRING " the text to be displayed " )
)
(:RETURNS nil)
(:EXAMPLES
" 1. (DISPLAY 1 2 3)

	-->123"
" 2. (DISPLAY 1 *tab* 2 *tab 3)

	-->1   2   3 "
" 3. (DISPLAY (PROTEIN-OF A7120))

	-->will display all proteins of A7120 "
)
(:TEXT
(:p " The displayed elements are displayed without intervening spaces . ")
(:p " *tab* can be used to display the elements with tab between them .")
(:p " *newline* puts a line between elements .")
)
(:SEE-ALSO DISPLAY-LINE DISPLAY-LIST DISPLAY-DATA))


;============================DISPLAY-LIST==============================
(DOCUMENT-FUNCTION DISPLAY-LIST
(:SUMMARY "Displays list in tabular format." )
(:VPL-SYNTAX (:FOO (:img :src "/weblistenerdocs/bbldf/images/DISPLAY-LIST-syntax.PNG"
                      :HEIGHT "150")))
(:PARAMETERS
  (each :PARAMETER-TYPE token 
       :DOCSTRING "Specifies that the associated argument (if a list) is to be displayed on  multiple lines (each element to a line)")
  (the-entity :PARAMETER-TYPE token 
       :DOCSTRING "Specifies that the associated argument is to be displayed on one line (each element to a column)")
  (list :VALUE-TYPE any :DOCSTRING "The of items to be displayed, either as a list or boxes of individual values" )
  (flush-left :PARAMETER-TYPE flag :DOCSTRING 
       "Specifies that each item is to aligned at the left edge of the column, unless otherwise specified by the ALIGNMENT option")
  (centered :PARAMETER-TYPE flag :DOCSTRING 
       "Specifies that each item is to aligned at the center of the column, unless otherwise specified by the ALIGNMENT option")
  (flush-right :PARAMETER-TYPE flag :DOCSTRING 
       "Specifies that each item is to aligned at the right edge of the column, unless otherwise specified by the ALIGNMENT option")
  (alignment :VALUE-TYPE list :PARAMETER-TYPE keyword :DOCSTRING 
       "Provides a list, each element specifying the alignment (L=Flush-left, C=Center, R=Flush-right) for a particular column.") 
  (columns-of-length :VALUE-TYPE (OR number list) :PARAMETER-TYPE keyword :DOCSTRING 
       "If a number, specifies the width of all the columns. If a list, specifies possibly different widths for each column.")
  (labels :VALUE-TYPE list :PARAMETER-TYPE keyword :DOCSTRING
       "A list of labels to appear at the top of the displayed columns.")
  (padding :VALUE-TYPE Number :PARAMETER-TYPE keyword :DOCSTRING 
       "Specifies the number of spaces that separate each column.")
  )
(:RETURNS nil)
(:EXAMPLES
   (:FOO
    (:OL
      (:LI (:B "Simple list") (:BR)
        (:img :src "/weblistenerdocs/bbldf/images/DISPLAY-LIST-basic.png" 
            :HEIGHT "70" :style "margin: 10px 0px") " "
        (:img :src "/weblistenerdocs/img-bbldoc/zrightarrow.gif"
            :style "margin: 12px 0px") " "
        (:img :src "/weblistenerdocs/bbldf/images/DISPLAY-LIST-basic-result.png" 
            :HEIGHT "14" :border 1 :style "margin: 15px 0px") (:BR)
        (:I (:U "Translation") ": A simple list of three numbers is displayed "
            "with each element of the list separated by a constant spacing. " 
            "The two forms are equivalent in this case, but this is not always so "
            "(see below).")
        (:BR) (help::HTML "&nbsp")
       )

      (:LI (:B "Complex list") (:BR)
        (:img :src "/weblistenerdocs/bbldf/images/DISPLAY-LIST-complex-list.png" 
            :HEIGHT "70" :style "margin: 10px 0px") " "
        (:img :src "/weblistenerdocs/img-bbldoc/zrightarrow.gif"
            :style "margin: 32px 0px") " "
        (:img :src "/weblistenerdocs/bbldf/images/DISPLAY-LIST-complex-list-result.png" 
            :HEIGHT "42" :border 1 :style "margin: 25px 0px") (:BR)
        (:I (:U "Translation") ": A complex list (a list of lists) is displayed. "
            "Notice that the columns are flush-left (left-justified), except for "
            "columns populated only by numbers, which are flush-right. "
            "The entry box was left open so that you could see the entire list "
            "as it was entered. It would need to be closed before the form could "
            "be executed.")
        (:BR) (help::HTML "&nbsp")
       )

      (:LI (:B "Complex list (biological example)") (:BR)
        (:img :src "/weblistenerdocs/bbldf/images/DISPLAY-LIST-complex-list-biological.png" :HEIGHT "90"  :style "margin: 10px 0px" :border 1) (:BR)
        (help::HTML "&nbsp")" " (help::HTML "&nbsp") " " (help::HTML "&nbsp") " " 
        (:img :src "/weblistenerdocs/img-bbldoc/zrightarrow.gif" :style "margin: 22px 0px") " "
        (:img :src "/weblistenerdocs/bbldf/images/DISPLAY-LIST-complex-list-biological-result.png" 
              :HEIGHT "70" :border 1 ) (:BR)(:BR)
        (:I (:U "Translation") ": The ")
        ((:A :HREF (:PRINT (help::MAKE-HELP-FUNCTION-DOCUMENTATION-URL :NAME "INFORMATION-ABOUT GENE/S" :PACKAGE :bbl)))
                "INFORMATION-ABOUT-GENES")
        (:I " in the ")
        ((:A :HREF (:PRINT (help::MAKE-HELP-FUNCTION-DOCUMENTATION-URL :NAME "CONTEXT/S-OF" :PACKAGE :bbl)))
                "CONTEXT-OF")
        (:I " the gene all4312 (that is, the genes on either side of the gene) "
             "is displayed in tabular form.")
        (:BR) (help::HTML "&nbsp")
       )

      (:LI (:B "Complex list, custom alignment, labels") (:BR)
        (:img :src "/weblistenerdocs/bbldf/images/DISPLAY-LIST-complex-list-ALIGNMENT-LABELS.png" :HEIGHT "45"  :style "margin: 10px 0px") (:BR)
        (help::HTML "&nbsp") " " (help::HTML "&nbsp") " " (help::HTML "&nbsp") " "
        (:img :src "/weblistenerdocs/img-bbldoc/zrightarrow.gif" :style "margin: 27px 0px") " "
        (:img :src "/weblistenerdocs/bbldf/images/DISPLAY-LIST-complex-list-ALIGNMENT-LABELS-result.PNG"      :HEIGHT "56" :border 1 :style "margin: 10px 0px") (:BR)
        (:I (:U "Translation") ": The same list used in Example 2 is displayed, "
            "but this time the alignment of the first two columns are specified as "
            "centered (C) and right-justified (R). The alignments of the remaining "
            "two columns are unspecified and so left as the default. The LABELS "
            "option provides the headers for the columns.")
       )
   ))
 )
(:TEXT
  (:P
    "BioBIKE provides many ways of displaying whatever you want in popup windows. "
    "DISPLAY-LIST allows you to format a table with minimal effort. It may also "
    "be used to display a single line, separating the elements with spaces. "
#|
    "See "
     ((:A :HREF 
           (:PRINT (help::MAKE-HELP-TOPIC-URL :NAME "How to display information")))
       "How to display information")
    " for a comparison of alternate methods."  |#
    )

  (:P (:B "Input to DISPLAY-LIST") (:BR)
    "DISPLAY-LIST expects to see one of three things: "
    (:UL
      (:LI (:U "A simple list of items") 
         ": The list will be displayed on a single line, "
         "with spaces between each item of the list" (:BR)
        (help::HTML "&nbsp"))

      (:LI (:U "A complex list of lists") ": "
         "Each inner list will be displayed as a single line, with spaces between "
         "each of its items. There will be as many rows as there are inner lists." (:BR)
        (help::HTML "&nbsp"))

      (:LI (:U "A single object") ": The object will be displayed. In this case, one could have "
         "equally well used "
        ((:A :HREF (:PRINT (help::MAKE-HELP-FUNCTION-DOCUMENTATION-URL :NAME "DISPLAY-LINE" :PACKAGE :bbl)))
                "DISPLAY-LINE"))
      )
    "A list of lists may be treated as a simple list whose elements happen to be lists, "
    "to be displayed on one line, or as a complex list, to be displayed on multiple lines. "
    "By default, if each inner list has the same number of items, the list is treated as "
    "complex and otherwise it is treated as simple. Thus ((1 2) (3 4)) would by default "
    "be displayed as a 2 by 2 table, while ((1 2) (3 4 5)) would be displayed on a single "
    "line.")

  (:P "However, the default behavior can be overridden, with the pull-down menu preceding "
      "the list argument. If EACH is selected, then lists of lists are displayed on "
      "multiple lines, no matter how many elements the sublists possess. If THE-ENTITY "
      "is selected, then lists of lists are displayed on a single line, each item "
      "of the list considered a single item.")

  (:P (:B "Widths of displayed columns") (:BR)
     "The width reserved for the item (in the case of simple lists) " 
     "or the width reserved for the column (in the case of complex lists) "
     "by default are determined by their contents. The column width is the "
     "width of the item (in the first case) or the width of the widest item "
     "(in the second case).")

   (:P "The default width can be overridden with the COLUMNS-OF-LENGTH option. "
       "If this option is provided with a single number, then all columns will have "
       "the width given by that number. If the option is provided with a list of "
       "numbers, then the first number of the list will be the width of the first "
       "column, the second number the width of the second column, and so forth. "
       "Any column not specified (or given NIL as its value) will revert to the "
       "default width. For example, COLUMNS-OF-LENGTH (5 NIL 10) specifies a length "
       "of 5 for the first column and 10 for the third column. The width of "
       "column 2 and any column beyond column 3 will be determined as by default, "
       "i.e., by the contents of those columns.")
       
  (:P (:B "Padding between columns") (:BR)
     "Items in simple lists and columns derived from complex lists are separated "
     "by a fixed number of spaces. By default, this number is 3, but any number "
     "may be specified using the PADDING option. For example, PADDING 0 indicates "
     "that all items displayed of a simple list are to be jammed together without "
     "separation.")

  (:P (:B "Alignment of items within columns") (:BR)
     "When displaying a complex list on multiple lines, each item displayed within "
     "a column by default begins at the far left of the column range (i.e. flush-left)."
     "However, if the column consists solely of numbers, then the default behavior is "
     "to end each item at the far right of of the column range (i.e. flush-right).")

  (:P "This default behavior can be overridden with any one of the FLUSH-LEFT, CENTERED, "
      "or FLUSH-RIGHT flags, which act on all columns. It can also be overridden more "
      "flexibly by the ALIGNMENT option, which calls for a list of specifications, "
      "L for flush-left, C for centered, and R for flush-right. The first specification "
      "in the list acts on the first column, the second specification on the second "
      "column, and so forth. For example ALIGNMENT (C NIL R) indicates that the first "
      "column is to be centered, the third column is to be flush-right, and all other "
      "columns will be governed by the default behavior.")
 )
(:SEE-ALSO DISPLAY DISPLAY-LINE DISPLAY-DATA)
) 	


;=================================DISPLAY-TABLE=====================
(DOCUMENT-FUNCTION 
 display-table
 (:summary
  "A powerful way to display a table (or g-array)"
  )
 (:parameters
  (table :docstring "The table you wish to display")
  (details? 
   :docstring
   "Whether to provide obscure details about the table")
  (invert
   :docstring
   #.(one-string
      "If selected, turns the horizontal elements of the table into "
      "the vertical ones, and vice versa"))
  (name :docstring "Allows you to display your table by name")
  (starting-row :docstring "Which row to begin displaying your table")
  (starting-column :docstring "Which column to begin displaying your table")
  (max-rows :docstring "How many rows of your table to display")
  (max-columns :docstring "How many columns of your table to display")
  (specific-rows
   :docstring
   "By passing a list, you can select which rows of your table to display")
  (specific-columns
   :docstring
   "By passing a list, you can select which columns of your table to display")
  (maximum-column-width :docstring "How wide each column of your table will be")
  (maximum-width 
   :docstring "The maximum allowed width the display of your table will be")
  (if-not-there? 
   :docstring
   "What to display if a particular element in your table has no value")
  (column-widths :docstring "How wide each column of your table will be")
  (lines-between-rows 
   :docstring 
   "How many lines of padding will appear between each row of your table")
  (space-between-columns
   :docstring
   "How many spaces will appear between each column of your table")
  (alignment
   :docstring
   "How to align the elements of your table with respect to the column header"
   )
  )
 (:returns "The table (and creates a popup window displaying the table)")
 (:text
  (:p
   #.(one-string-nl
      "Display-table is a powerful way to display a table or garray"
      "with many different options."))
  (:p
   #.(one-string-nl
      "There is only one required argument, TABLE.  This is the table"
      "or garray that you want to display."))
  (:p
   #.(one-string-nl
      "If you enable the DETAILS? flag, the display will include more obscure"
      "information such as what to do if you try to access a non-existant"
      "location in the table."))
  (:p
   #.(one-string-nl
      "The INVERT flag turns the horizontal elements of the table"
      "into the vertical ones, and vice versa."))
  (:p
   #.(one-string-nl
      "The NAME keyword allows you to name your table by passing a string."))
  (:p
   #.(one-string-nl
      "The STARTING-ROW and STARTING-COLUMN keywords allow you to select"
      "at which row or column to begin displaying your table, and can be"
      "used together."))
  (:p
   #.(one-string-nl
      "The MAX-ROWS and MAX-COLUMNS keywords allow you to select"
      "the maximum number of rows or columns to display, and can be"
      "used together.  If your table has less than this number,"
      "the entire table will be displayed"))
  (:p
   #.(one-string-nl
      "You can pass a list of numbers or keys to the SPECIFIC-ROWS and"
      "SPECIFIC-COLUMNS keywords.  When you do this, only"
      "those particular rows and columns will be displayed."
      "These two keywords can be used together."))
  (:p
   #.(one-string-nl
      "The MAXIMUM-COLUMN-WIDTH keyword allows you to control the maximum"
      "width of each column.  If one of your"
      "column widths is greater than this number, the column will be truncated."
      "Similarly the MAXIMUM-WIDTH keyword allows you to control the overall"
      "width of the display.  If the overall width of your table is less"
      "than this number, the entire table will be displayed."
      "However if your overall column width exceeds this number, the"
      "appropriate number of columns will be chopped off in the display,"
      "so that only whole columns are displayed.  Thus it is possible that"
      "your table is 60 characters wide, you select a MAXIMUM-WIDTH of 50,"
      "and the display is only 40 characters wide."))
  (:p
   #.(one-string-nl
      "The COLUMN-WIDTHS keyword allows you to pass a number, which becomes"
      "the width for each column, or a list of numbers, where each"
      "number represents the width of a column."))
  (:p
   #.(one-string-nl
      "The IF-NOT-THERE? keyword allows you to select an object to be"
      "displayed if a particular location in your table has not been set yet."))
  (:p
   #.(one-string-nl
      "The LINES-BETWEEN-ROWS and SPACES-BETWEEN-COLUMNS keywords"
      "allow you to insert any additional padding between your rows or columns."
      "These two keywords can be used together."))
  (:p
   #.(one-string-nl
      "The ALIGNMENT keyword allows you to control how the data is aligned"
      "relative to the column header.  Appropriate arguments are :center"
      "(the default), :left, and :right."))
  )
 (:examples
  "
    (define x = (new-table '(3 3)))
    --> <Table 2d (Numeric,Numeric)>

    (progn
      (setf (utils::gref x 1 1) 1)
      (setf (utils::gref x 1 2) 2)
      (setf (utils::gref x 1 3) 3)
      (setf (utils::gref x 2 1) 4)
      (setf (utils::gref x 2 2) 5)
      (setf (utils::gref x 2 3) 6)
      (setf (utils::gref x 3 1) 7)
      (setf (utils::gref x 3 2) 8)
      (setf (utils::gref x 3 3) 9))
     --> 9

    (display-table x)
    Popup:

     1 2 3 
   1 1 2 3 
   2 4 5 6 
   3 7 8 9 

 
    (display-table x invert)
    Popup:

      1 2 3 
    1 1 4 7 
    2 2 5 8 
    3 3 6 9 

    (display-table x starting-row 2 staring-column 2)
    Popup:

      2 3 
    2 5 6 
    3 8 9 

    (display-table x specific-rows '(1 3) specific-column '(2))
    Popup:

      2 
    1 2 
    3 8 

    (display-table x lines-between-rows 2 spaces-between-columns 2)
    Popup:

     1  2  3  


  1  1  2  3  


  2  4  5  6  


  3  7  8  9  

     (display-table x column-widths '(2 5 8 10))
     ;; X has 3 columns...the first number in the list represents
     ;; the width of the header column.  Passing '(2 5 8) would 
     ;; cut off the third column.
     Popup:

      1      2         3      
  1   1      2         3      
  2   4      5         6      
  3   7      8         9    


"

  

  )
 (:see-also 
   display display-line display-list display-data display-sequence-of
   new-table 
   utils::make-garray utils::pprint-garray utils::describe-garray
   )
 

 )
  
