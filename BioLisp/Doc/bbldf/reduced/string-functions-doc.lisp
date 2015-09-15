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

;;; Author:  Michiko Kato, Arnaud Taton, Bogdan Mihai, Jeff Elhai


(eval-when (:compile-toplevel :load-toplevel :execute)
  (import '(com.biobike.help:document-module
               com.biobike.help:document-function) :bbi))

(DOCUMENT-MODULE strings-sequences 
  "Functions dealing with strings"
  (:keywords :text :string :length :letters :character :random)
  (:display-modes :bbl)
  (:submodules string-analysis
               string-extraction
               string-production
               string-type-checks)
 #.`(:functions LENGTH-OF INSIDE-STRING JOIN)
     )

(DOCUMENT-FUNCTION LENGTH-OF  
  (:RETURNS "number of elements")  
  (:PARAMETERS     
    (entity :docstring "Thing to be evaluated" :parameter-type required)     
    (each :docstring "If specified, evaluates each element of a list." 
      :parameter-type token :value-type boolean))  
  (:EXAMPLES "1. Gene: length in nucleotides (including stop codon)
       (LENGTH-OF NpF0047) --> 984"
             "2. Protein: length in amino acids
       (LENGTH-OF p-NpF0047) or (LENGTH-OF (PROTEIN-OF NpF0047))
       --> 327""3. Contig: length in nucleotides
       (LENGTH-OF #$Tery.Te?0085)
       --> 1002""4. Organism: sum of length of all contigs
       (LENGTH-OF ATCC29413)
       --> 7067898"
             "5. String: length of string 
      (LENGTH-OF \"ATGATACAAAATGATACAGAACTATTGACAACATCTGTTGCGGCTATGCCTATTCTGATTACAGGTGGTGCAGGCTTTATTGGTTCCAATTTCGTCCATCACTGGTATGAAA\")
       --> 112"
"6. Returns the length of a list (# of elements in the list)
       (LENGTH-OF '((a b) (c d) (e f g)))
       --> 3
       (LENGTH-OF *ALL-ORGANISMS*)
       --> 13"
"7. Returns the length of each element of a list
       (LENGTH-OF each '((a b) (c d) (e f g)))
       --> (2 2 3)
       (LENGTH-OF each *ALL-ORGANISMS*)
        --> (2750104 2410873 3956956 9059191 7211789 7764024 1751080 2434428       469019 2593857 1657990 6220104 7067898)
       ; Result is the length of each organism""8. The code extracts the gene-length in each replicon of Synechocystis PCC6803.
       (FOR-EACH REPLICON IN (REPLICONS-OF PCC6803)
        AS NAME = REPLICON
       AS LENGTH = (LENGTH-OF EACH REPLICON)
       DO (DISPLAY-LINE NAME *TAB* LENGTH))
       --->
       #$S6803.chromosome	3573470
       #$S6803.pSYSA	103307
       #$S6803.pSYSG	44343
       #$S6803.pSYSM	119895
       #$S6803.pSYSX	106004
       #$S6803.pCC5.2	5214
       #$S6803.pCA2.4	2378
       #$S6803.pCB2.4	2345")
  (:TEXT (:p "The function LENGTH-OF counts the number of elements within a given entity.")
  (:ul ((:table border 1 rules "all" cellpadding 4)
      	(:tr (:td (:small (:b "IF GIVEN")))
           (:td (:small (:b "RETURNS"))))
		(:tr (:td (:small "string"))
	     (:td (:small "the number of characters (spaces included)")))
	(:tr (:td (:small "a gene, a protein or a contiguous-sequence"))
           (:td (:small "the number of nucleotides or amino-acids that compose their sequences")))  (:tr (:td (:small "an organism"))
           (:td (:small "the number of nucleotides that compose its genome")))
  (:tr (:td (:small "list"))
	     (:td (:small "the number of elements (sublists, strings, genes, proteins, contiguous-sequences or organisms)  within the list")))))
(:p "Note: The option EACH permits to count the number of elements within entities themselves within a list.")
     )
(:SEE-ALSO lengths-of))

(DOCUMENT-FUNCTION LENGTHS-OF
  (:PARAMETERS
      (entity :docstring "Thing to be evaluated")
     (each :docstring "Not required & if added for convenience, does not have any effect on the function"))
  (:EXAMPLES "Introducing comment:
    - should be applied on something that contains something else, like a list. If applied on one string,
      one gene, one protein or one organism, this function gives the same answer than 'LENGTH-OF'
    - a list can be expressed as:
           * {...}               
           * '(...)              
           * the result of a function                    
           * a predefine result"
"1. A list of strings: lengths in characters (with spaces) of each string in a list of strings
    (LENGTHS-OF {\"How many characters (with spaces) compose this question?\" \"What is the length                          of this question?\"})
    --> (56 36)"
"2. A list of genes:
     - lengths in nucleotides of each gene in a list 
       (LENGTHS-OF {NpF0047 slr1063 Te?5178})
       --> (984 2070 1131)  
     - lengths in nucleotides of each gene in a list, which is the result of a function 
       (LENGTHS-OF (ORTHOLOGS-OF NpF0047 IN *all-organisms*)[1 -> 3])                   
       --> (2070 984 1131)"
"3. A list of proteins:  lengths in amino acids of each protein in a list              
       (LENGTHS-OF {p-NpF0047 p-slr1063 p-Te?5178})
       --> (327 689 376)"
"4. A list of contigs:
     - lengths in nucleotides of each contig or replicon in a list            
       (LENGTHS-OF {Npun.chromosome Npun.pNpA Npun.pNpB Npun.pNpC Npun.pNpD Npun.pNpE})   
       --> (8234322 354564 254918 123028 65940 26419) 
     - lengths in nucleotides of each contig or replicon in a list, which is the result of a function
       (LENGTHS-OF (REPLICONS-OF nostoc_punctiforme_atcc29133))
       --> (8234322 354564 254918 123028 65940 26419)"
"5. A list of organisms:
     - sum of lengths of all contigs or replicons for each organism in a list
       (LENGTHS-OF {S7942 P9313 SS120})
       --> (2750104 2410873 1751080)
     - sum of lengths of all contigs for each organism in the predined list *all-organism* 
       (LENGTHS-OF *all-organisms*)
       --> (2750104 2410873 3956956 9059191 7211789 7764024 1751080 2434428 4659019 2593857 1657990
            6220104 7067898)"
"6. A list of lists:     numbers of elements of each list in a list
    (LENGTHS-OF {{1} {2 3} {4 5 6}}) OR (LENGTHS-OF '((a) (b c) (d e f)))
    --> (1 2 3)"
"7. A list of labeled sequences: labeled sequences not available yet"
"8. A list of vectors:   vectors not available yet"
"9. Notes:
    - Comparison to 'LENGTH-OF':
      (LENGTH-OF *all-organisms*)
      --> 13, the number of organisms in the list *all-organisms*
    - by adding 'EACH' to 'LENGTH-OF', the same answer is obtained than with 'LENGTHS-OF'
      (LENGTH-OF EACH *all-organisms*)
      --> (2750104 2410873 3956956 9059191 7211789 7764024 1751080 2434428 4659019 2593857 1657990            6220104 7067898)
    - by applying 'LENGTH-OF' within a loop to each entity that composes an entity of a higher level,
      the same answer is obtain than with 'LENGTHS-OF'
      (FOR-EACH organism IN *all-organisms*
           COLLECT (LENGTH-OF organism))
      --> (2750104 2410873 3956956 9059191 7211789 7764024 1751080 2434428 4659019 2593857 1657990
           6220104 7067898)")

(:TEXT (:p "'LENGTHS-OF' counts the number of elements within entities, which are themselves within an entity of a higher level."))
  (:SEE-ALSO LENGTH-OF FOR-EACH LOOP))


;;================================= JOIN ==============================

(DOCUMENT-FUNCTION JOIN
 (:SUMMARY "Combines given elements into one string or one list" )
 (:SYNTAX (JOIN list-of-elements [BY separator] [AS-LIST] [AS-STRING]))
 (:SYNTAX (JOIN element1 element2 (etc) [BY separator] [AS-LIST] [AS-STRING]))

 (:PARAMETERS
   (list-of-elements :VALUE-TYPE list :DOCSTRING "the list of elements to be joined")
   (elementX         :VALUE-TYPE any  :DOCSTRING "One element to be joined")
   (BY :VALUE-TYPE any :PARAMETER-TYPE optional :DOCSTRING "To be inserted between each element")
   (AS-LIST :VALUE-TYPE boolean :DOCSTRING "Combine elements into a list")
   (AS-STRING :VALUE-TYPE boolean :DOCSTRING "Combine elements into a string"))
   (:RETURNS "A string or a list")
   (:EXAMPLES 
"1. Combine a list into a string
       (JOIN *amino-acids* BY \"|\")
           -->\"A|C|D|E|F|G|H|I|K|L|M|N|P|Q|R|S|T|V|W|Y\"  "

"2. Combine strings and non-strings into a string
       (FOR-EACH amino-acid IN *amino-acids*
        FOR-EACH number FROM 1
            (DISPLAY-LINE (JOIN number \". \" amino-acid))
        --> 1. A
            2. C
            ..."

"3. Combine a list and an element into a new list
       (JOIN *amino-acids* \"Z\")
        --> (\"A\" \"C\" \"D\" \"E\" \"F\" \"G\" \"H\" \"I\" \"K\" \"L\" \"M\" \"N\" \"P\" \"Q\" \"R\" \"S\" \"T\"
                \"V\" \"W\" \"Y\" \"Z\") "
"4. Combine a list and an element into a string
       (JOIN *amino-acids* \"Z\" AS-STRING)
        --> \"ACDEFGHIKLMNPQRSTVWYZ\" "
 )

(:TEXT
  (:p " If given individual elements, they are combined into a string (converted to a string, if necessary." (:BR "However, if AS-LIST is specified, then the elements are combined into a list."))
  (:p " If any element is a list, the elements are combined into a list."
    (:BR "However, if AS-STRING is specified, then all elements, even those within the list(s) are combined into a string.")))
(:SEE-ALSO MERGE))

;;================================= SPLIT ==============================

(DOCUMENT-FUNCTION SPLIT 
 (:PARAMETERS 
   (string  :DOCSTRING "the source you are splitting")
   (at :DOCSTRING "where to split string")
   (between-words :DOCSTRING "makes the split at space")
   (no-compress :DOCSTRING "option  for no compression"))
 (:EXAMPLES"
  1. (DEFINE seq AS (SEQUENCE-OF all4312))(SPLIT seq AT 50)
     --> (\"GTGGGTTCGGTTTGTATTGAAATCGTTGAGGGGAATCCCCATCTGAGGTC\" 
          \"GTTGCTGGGTTGGCACTTGCAACAATTGGAATACCGTGTGCATCAAGCCG\" 
          \"CCAGCATATATCAAGCAAGGGAAGCCTTTTTGAGCCATCAGCCAACTCTA\"
          \"GTGATTCTGGATGCTGATTTGCCAGATGGTGACGGTATTGAATTTTGCCG\"
          \"TTGGCTGCATCGTCAGCAACAGCCGCTAATTCTCATGTTATCTGCTCGGA\"
          \"CTAATGAGGCTGATATCGTTGCCGGGTTGAAGGCGGGAGCTGATGATTAC\"
          \"TTGAGCAAACCATTTGGGATGCAGGAGTTTTTGGCTAGGGTAGAGGCATT\" 
          \"AATCCGCCGCAAGCGCACACCTACTGCTCCTGCTTATTTGGATTATGGTA\" 
          \"CTTTGCAAATCGATTTAGTCCAACGCCGTGTACGATTCCAAGGGGAGTTT\" 
          \"ATCGACCTGACTCCACAGGAATTTAGTTTGTTGTACGTTTTGGCGCAAGC\" 
          \"TGGTGGAGTACCTTTGAGCCGATCAGAGTTGCTACGTCGTGCGTGGCCTG\" 
          \"ACGCTATCGACAATCCGCGTACCATTGACACTCATGTTCTATCGTTACGT\" 
          \"AAAAAAGTAGAACTTGATCCCCGCCAACCTAGCCTCATTCAGACTATCCG\" 
          \"CAATGTTGGATACCGATTTAACATGGAAATTTTGAATGCTAATCCTCCAC\" 
          \"AAACACAAGCAAAGTTAACAAAAGAAAGATTTAGCAACCAACGCTCAACT\" 
          \"CTAAGTGGGCAGAGGGTGTAG\") 
    (note: this usage of SPLIT is helpful for a more readable display)

2. (DEFINE seq AS (SEQUENCE-OF all4312 FROM 10 TO 20))
   (SPLIT seq)
    --> (\"G\" \"T\" \"G\" \"G\" \"G\" \"T\" \"T\" \"C\" \"G\" \"G\" \"T\" \"T\"
         \"T\" \"G\" \"T\" \"A\" \"T\" \"T\" \"G\" \"A\")
   (note: this sequence is broken at single-character delimiter because \"at\" is not specified)

3. (SPLIT (DESCRIPTION-OF all4312) BETWEEN-WORDS)
   (\"two-component\" \"system\" \"response\")

4. (SPLIT (DESCRIPTION-OF all4312) AT \" \")
   (\"two-component\" \"system\" \"response\")
   (note: Examples 3 and 4 are interchangeable and return the same display)

5. (DEFINE input-line AS \"all4312\, Anabaena PCC7120\, chromosome\, 5166997\, 5167767\, b\")
   (SPLIT input-line at \",\")
   --> (\"all4312\" \" Anabaena PCC7120\" \" chromosome\" \" 5166997\" \" 5167767\" \" b\")

6. (DEFINE input-line AS \"all4312\, Anabaena PCC7120\, chromosome\,  \, 5167767\, b\")
   (SPLIT input-line AT \"\, \")-->(\"all4312\" \"Anabaena PCC7120\" \"chromosome\" \"5167767\" \"b\")

7. (DEFINE input-line AS \"all4312\, Anabaena PCC7120\, chromosome\,  \, 5167767\, b\")
   (SPLIT input-line AT \"\, \" NO-COMPRESS)
   -->(\"all4312\" \"Anabaena PCC7120\" \"chromosome\" \" \" \"5167767\" \"b\")")

(:TEXT
  (:P "SPLIT is often used to customize a display on a screen. (example 1)")
  (:P "When AT is absent\, the string is broken up into individual characters and returned as a list of strings.  If BETWEEN-WORDS is specified\, the string is split up at each space. (examples 2 and 3)")(:P "When  AT has a number value\, the string is split into strings of length equal to number specified or less.  If BETWEEN-WORDS is also specified then string will also split at spaces.")
  (:P "When AT has a string value\, \"$\"\, the string is split into strings with endpoints where \"$\" occurs. (example 4)")
  (:P "When splitting a string using a delimiter such as \"\,\" NO-COMPRESS is used to account for blank spaces so the number of strings in the returned list is constant. (examples 5\, 6\, and 7)")
  (:P "Multiple contiguous delimiters are compressed to one delimiter including all contiguous delimeters."))
(:SEE-ALSO JOIN))

;;================================= STRING-OF ==============================
(DOCUMENT-FUNCTION STRING-OF 
  (:PARAMETERS 
    (entity :DOCSTRING " the subject of conversion"))
    (:EXAMPLES 
"1. Transforming a list of numbers
    (STRING-OF {1 2 3 4 5 })
    --> (\"1\" \"2\" \"3\" \"4\" \"5\")"

"2. Transforming the non-string elements of a list
    (PROGN
      (ASSIGN list {1 2 3 \"a\" \"bp\" \"texaco\" \"123\" 123})
      (FOR-EACH item IN list
           (UNLESS (IS-STRING? item)
           (PUSH (STRING-OF item) list)
           (DELETE-FROM-LIST list item)))
    (PRINT list))
    -->(\"123\" \"3\" \"2\" \"1\" \"a\" \"bp\" \"texaco\" \"123\")"

"3. (STRING-OF 12)
    -->\"12\""

"4. (STRING-OF {1 2 \"A\" \"B\"})
    -->\"(1 2 A B)\""

"5. (STRING-OF (CONTEXT-OF 3500000 IN S6803.chromosome))
    --> \"(I #$S6803.slr0452 NIL 1097 588 F)\"")

(:TEXT(:p "It cannot transform a text.")
(:p "If there are more than one argument to be transformed, they must be put in a list.")
(:P "You can change multiple elements into strings only if they are in a list.")
(:p "The un-cool program from example 2 can be simple written as (STRING-OF list), after assigning the list."))
(:SEE-ALSO))

;;================================= INSERT ==============================
(DOCUMENT-FUNCTION INSERT
  (:SUMMARY "Modifies a string or list by inserting a new character or element" )
  (:SYNTAX  
   (INSERT insertion INTO sequence [AT|FROM] insertion-point [DELETE length|TO end-point]))
  (:PARAMETERS
   (insertion :VALUE-TYPE string :DOCSTRING "the insertion")
   (into :PARAMETER-TYPE keyword :DOCSTRING "")
   (sequence :VALUE-TYPE string :DOCSTRING "the insertion ")
   (at\|from :PARAMETER-TYPE keyword )
   (insertion-point :VALUE-TYPE number :DOCSTRING "insertion point")
   (delete\|to :VALUE-TYPE number :DOCSTRING "the length to be deleted or replaced "))
  (:RETURNS "A list or string ")
  (:EXAMPLES
   "(ASSIGN short-gene \"ATGGAATTCCTGCAATTAA\")
 (INSERT \"TTTT\" INTO short-gene FROM 3 TO 4)
 -->\"ATGTTTTAATTCCTGCAATTAA\"")
  (:TEXT
   (:p "The insertion point may be specified by AT insertion-point or FROM insertion-point.")
   (:p "Either way, insertion-point represents the coordinate after which the insertion will appear.")
   (:P "If AT insertion-point is used to specify the insertion point, then:")
   (:p " DELETE length may be used to specify how much of sequence is to be replaced by insertion.")
   (:p " If DELETE is not specified, then a clean insertion (without deletion) is made.")
   (:p "If FROM insertion-point is used to specify the insertion-point, then:")     (:p " TO end-point specifies the end coordinate of sequence to be replaced by insertion.")       (:p " If TO is not specified, then insertion is inserted into sequence at the insertion-point,")
   (:p " replacing the remainder of sequence.")
   (:p " The insertion into a list may be a list or any value.")
   (:p " The insertion into a string must be a string or a character.")))


;;============================== SEQUENCE-OF ==========================
(DOCUMENT-FUNCTION Sequence-of
(:TEXT
  (:P ((:PRE)
"(SEQUENCE-OF entity [FROM n | FROM-START n | FROM-END] 
                    [TO n | TO-START | TO-END n | LENGTH n] 
                    [DNA | PROTEIN] 
                    [INVERT] 
                    [WRAP | TRUNCATE])" ))

  (:P ((:PRE) "
                                      Legal Options
ENTITY                FROM FROM-END TO TO-END LENGTH DNA|PROTEIN INVERT WRAP TRUNCATE
-------------------------------------------------------------------------------------
Organism                *      *     *   *       +                 +     +      +
Replicon/contig         *      *     *   *       +                 +     +      +
Gene                    +      +     +   +       +                 +            +
Protein                 +      +     +   +       +                              +
String/Labeled-sequence *      *     *   *       +        +        #     +      +
-------------------------------------------------------------------------------------
   *Behavior dependent on whether either WRAP or TRUNCATE is specified
   #Illegal if PROTEIN is specified"))

  ((:PRE) "
SEQUENCE-OF is useful for extracting all or part of a specified sequence.

If the entity is an organism, then a list of sequences are returned for each 
of the organism's contigs or replicons.

If the entity is a string, then all numeric characters, spaces, tabs, and 
line-feeds are removed, and what remains is converted to upper-case. This 
may be useful for cleaning up sequences cut-and-pasted from a variety of 
sources. If DNA or PROTEIN is specified, then the resulting sequence is 
tested to make sure that all characters are legal.

If a list of entities if given, then a list of sequences is returned.

If FROM (or FROM-START) is specified, the sequence returned begins at the given 
   coordinate. If it is not specified, the sequence begins at its first position.
If FROM-END is specified, the sequence returned begins at the given offset
   from the end of the sequence. For example:
        FROM-END -10 --> " ((:I)"start-point 10 positions to left of end of sequence") "
FROM and FROM-END may not both be specified, but neither is necessary.
     ")
  ((:PRE) "
If TO (or TO-START) is specified, the sequence returned ends at the given 
   coordinate. If it is not specified, the sequence ends at its last position.
If TO-END is specified, the sequence returned ends at the given offset 
   from the end of the sequence. For example:
        TO-END -10 --> " ((:I)"end-point 10 positions to left of end of sequence") "
If LENGTH is specified, the sequence returned ends at the given offset 
   from the specified start. For example:
        FROM 100 LENGTH 50 --> " ((:I)"end-point is at coordinate 149") "
Only one among TO, TO-END, and LENGTH may be specified, but it is not necessary to
   specify any.")
  ((:PRE) "  
Negative coordinates and coordinates that go beyond the limits of replicons, 
   contigs, and strings raise errors unless WRAP or TRUNCATE are specified 
   (see below).
Negative coordinates and coordinates that go beyond the limits of genes and proteins
   are legal. The sequence returned may begin before the beginning of the gene
   or protein and may end beyond its end. In the case of protein sequences, 
   negative coordinates specify that the region upstream from the nominal start of

   the corresponding gene is to be translated. This may be useful in looking for
   start codons of misannotated genes.

INVERT causes SEQUENCE-OF to return the reverse complement of the DNA sequence 
produced, exactly as if (INVERSION-OF ...) had been applied to the result.
It is not a legal option for protein sequences.

WRAP is applicable only to circular sequences (i.e. replicons). Using it with 
a string indicates that the string is to be considered a circular sequence.
When WRAP is specified, coordinates less than 1 or greater than the length of
the sequence are forced into the sequence's range by wrapping around the circle. 
This is done " ((:I) "almost") " as follows:
     MOD " ((:I) "coordinate") " (" ((:I) "length-of-replicon") " - 1)
taking into account the peculiarities of BioBIKE's peculiar " 
((:A href "http://link-to-explanation-of-coords") "sequence coordinate system") ".

TRUNCATE is applicable only to linear sequences (i.e. contigs). Using it with
a string indicates that the string is to be considered a linear sequence.
When TRUNCATE is specified, coordinates less than 1 or greater than the length
of the sequence are forced into the sequence's range by setting coordinates that
are too low to 1 and those that are too high to the length of the sequence.
")
))

;;============================= INVERSION-OF ============================

(DOCUMENT-FUNCTION INVERSION-OF
(:PARAMETERS 
      (entity :docstring "Thing to be evaluated"))
(:EXAMPLES 
"1.If given a string
      (INVERSION-OF \"ATCGAAAATTTTCCCCGGGGATCG\")
      --> \"CGATCCCCGGGGAAAATTTTCGAT\"
      (INVERSION-OF \"qwertyqwertyqwerty\")
      --> \"yarewqyarewqyarewq\""
"2. If given a gene or a protein
      (INVERSION-OF NpF5291)
      --> \"TTATACGGCAGTAATGCTAATAACTCTACTGCCCTTACGATTCAACTGCTGTAATTTACTAGAAAGTTGCTCGT...
      ;; >>> Line truncated to 200 (was 813). Use SET-OUTPUT-LIMITS to adjust width\"
      (INVERSION-OF p-NpF5291)
      --> \"TTATACGGCAGTAATGCTAATAACTCTACTGCCCTTACGATTCAACTGCTGTAATTTACTAGAAAGTTGCTCGT...
      ;; >>> Line truncated to 200 (was 813). Use SET-OUTPUT-LIMITS to adjust width\" "

"3. Within a loop
      (GENES-DESCRIBED-BY \"16S\" IN (NONCODING-GENES-OF *all-organisms*))
      --> (((#$S7942.Se16SA) (#$S7942.Se16SB))
      ((#$S6803.rrn16Sa) (#$S6803.rrn16Sb))
      ((#$Npun.NpF9016) (#$Npun.NpR9054) (#$Npun.NpF9062) (#$Npun.NpR9071))...

      (ASSIGN 16SrRNA-genes AS (GENES-DESCRIBED-BY \"rRNA\" IN *))
      --> ((((#$S6803.rrn16Sa)) ((#$S6803.rrn16Sb)))
      (((#$Npun.NpF9016)) ((#$Npun.NpR9054)) ((#$Npun.NpF9062))
      ((#$Npun.NpR9071)))...

      (FOR-EACH gene IN (FLATTEN (GENES-OF 16SrRNA-genes))
         AS seq = (IF (EQUAL gene[.direction] :B)  
                         (INVERSION-OF gene) 
                         (SEQUENCE-OF gene))
      COLLECT {gene gene[.direction] seq})
      --> ((#$S6803.rrn16Sa :B
      \"AAAGGAGGTGATCCAGCCACACCTTCCGGTACGGCTACCTTGTTACGACTTCACCCCAGTCACTAGCCCTGCCTTCGGCGCCCTCCTCCCTAAGGTT...
      ;; >>> Line truncated to 200 (was 1489). Use SET-OUTPUT-LIMITS to adjust width\")
      (#$S6803.rrn16Sb :F
      \"ACAATGGAGAGTTTGATCCTGGCTCAGGATGAACGCTGGCGGTATGCCTAACACATGCAAGTCGAACGGAGTTCTTCGGAACTTAGTGGCGGAC...
      ;; >>> Line truncated to 200 (was 1489). Use SET-OUTPUT-LIMITS to adjust width\")
      (#$Npun.NpF9016 :F
      \"AGAGTTTGATCCTGGCTCAGGATGAACGCTGGCGGTATGCTTAACACATGCAAGTCGAACGGTGTCTTCGGACACAGTGGCGGACGGGTGAGTA...
      ;; >>> Line truncated to 200 (was 1482). Use SET-OUTPUT-LIMITS to adjust width\")
      (#$Npun.NpR9054 :B
      \"AAAGGAGGTGATCCAGCCACACCTTCCGGTACGGCTACCTTGTTACGACTTCACCCCAGTCACCAGTCCTGCCTTAGGCATCCTCCTCCCCGAA...
      ;; >>> Line truncated to 200 (was 1482). Use SET-OUTPUT-LIMITS to adjust width\")
      (#$Npun.NpF9062 :F
      \"AGAGTTTGATCCTGGCTCAGGATGAACGCTGGCGGTATGCTTAACACATGCAAGTCGAACGGTGTCTTCGGACACAGTGGCGGACGGGTGAGTA...    
      ;; >>> Line truncated to 200 (was 1482). Use SET-OUTPUT-LIMITS to adjust width\")
      (#$Npun.NpR9071 :B
      \"AAAGGAGGTGATCCAGCCACACCTTCCGGTACGGCTACCTTGTTACGACTTCACCCCAGTCACCAGTCCTGCCTTAGGCATCCTCCTCCCCGAA...
      ;; >>> Line truncated to 200 (was 1482). Use SET-OUTPUT-LIMITS to adjust width\")
      ..."
)
                  
(:TEXT (:p "INVERSION-OF reverse and complement DNA sequences, from strings, genes or proteins")
(:p "Notes")

(:ul  
  (:li "Characters that are not recognized as nucleotides (ATCG) are not complemented.")
  (:li "The function is mapped, it may therefore be applied on simple and complex lists.")))
 (:SEE-ALSO SEQUENCE-OF)
)

; ================= TRANSLATION-OF ====================



(DOCUMENT-FUNCTION TRANSLATION-OF
(:PARAMETERS 
      (entity :docstring "DNA sequence to be translate, see the value type column for details")
      (if-bad-codon :docstring "To replace the default character \"-\" by another when a codon is not recognize as amino-acid")
      (noncoding :docstring "To translate sequence of gene that does not encode protein")
      (nowarnings :docstring "To avoid warnings")
      
)
(:EXAMPLES 
"1. If given a gene or a string
      (TRANSLATION-OF  NpR0388)
      --> \"MKSTQGKINELLTETGCEHNQHKQAEKKNKSCAQQAQPGAAQGGCAFDGAMIALVPIADAAHLVHGPIACAGNSWGSRGSLSSGPQLYKMGFTTDLGENDVIFGGEKKLYKAILELKERY...
      ;;    >>> Line truncated to 200 (was 460). Use SET-OUTPUT-LIMITS to adjust width\"
  or  (SEQUENCE-OF  p-NpR0388)
      --> \"MKSTQGKINELLTETGCEHNQHKQAEKKNKSCAQQAQPGAAQGGCAFDGAMIALVPIADAAHLVHGPIACAGNSWGSRGSLSSGPQLYKMGFTTDLGENDVIFGGEKKLYKAILELKERY...
      ;; >>> Line truncated to 200 (was 461). Use SET-OUTPUT-LIMITS to adjust width\"

      (TRANSLATION-OF \"ATGCGTCGGGTCCCCGTCGCTCGGGAAATGATAGTACGATAGATGATAGATCCCGCTCGATCGATCGCAAACT\")
      -->\"MRRVPVAREMIVR*MIDPARSIAN\"

     (TRANSLATION-OF (SEQUENCE-OF A7120.CHROMOSOME FROM 434550 TO 434662))
     --> \"GFTLFLPPFP*VRLQRRRTLT*PGLYQFTIERH*K*R\""


"2. If given a list of genes or strings
      (TRANSLATION-OF (GENES-DESCRIBED-BY \"cpcA\" NOWARNINGS))
      --> ((\"MSKTPLTEAVAAADSQGRFLSSTELQVAFGRFRQAASGLAAAKALANNADSLVNGAANAVYSKFPYTTSTPGNNFASTPEGKAKCARDIGYYLRIVTYALVAGGTGPIDEYLLAGLDEINKTFDLAPSWYVEALKYIKANHGLSGDSRDEANSYIDYLINALS\"
       \"MSKTPLTEAVAAADSQGRFLSSTELQVAFGRFRQAASGLAAAKALANNADSLVNGAANAVYSKFPYTTSTPGNNFASTPEGKAKCARDIGYYLRIVTYALVAGGTGPIDEYLLAGLDEINKTFDLAPSWYVEALKYIKANHGLSGDSRDEANSYIDYLINALS\")

      (\"MKTPLTEAVSTADSQGRFLSSTELQIAFGRLRQANAGLQAAKALTDNAQSLVNGAAQAVYNKFPYTTQTQGNNFAADQRGKDKCARDIGYYLRIVTYCLVAGGTGPLDEYLIAGIDEINRTFDLSPSWYVEALKYIKANHGLSGDARDEANSYLDYAINALS\")
      (\"MVKTPITEAIAAADTQGRFLGNTELQSARGRYERAAASLEAARGLTSNAQRLIDGATQAVYQKFPYTTQTPGPQFAADSRGKSKCARDVGHYLRIITYSLVAGGTGPLDEYLIAGLAEINSTFDLSPSWYVEALKHIKANHGLSGQAANEANTYIDYAINALS\")
      (\"MKTVITEVIASADSQGRFLNNTELQAANGRFQRATASMEAARALTSNADSLVKGAVQEVYNKFPYLTQPGQMGYGDTNQAKCARDISHYLRFITYSLVAGGTGPLDDYIVAGLREVNRTFNLSPSWYIEALKHIKGKVGSQLSGQPLTEANAYIDYCINALS\"
       \"MKTVITEVIASADSQGRFLNNTELQAANGRFQRATASMEAARALTSNADSLVKGAVQEVYNKFPYLTQPGQMGYGDTNQAKCARDISHYLRFITYSLVAGGTGPLDDYIVAGLREVNRTFNLSPSWYIEALKHIKGKVGSQLSGQPLTEANAYIDYCINALS\")
      (\"MKTPITEAIAAADTQGRFLSNTELQAVDGRFKRAVASMEAARALTNNAQSLIDGAAQAVYQKFPYTTTMQGSQYASTPEGKAKCARDIGYYLRMVTYCLVAGGTGPMDEYLIAGLSEINSTFDLSPSWYIEALKYIKANHGLTGQAAVEANAYIDYAINALS\"))"



"3. Illustrate NOWARNINGS, IF-BAD-CODON and NONCODING 
      (TRANSLATION-OF \"ATGCGTCGGGTCCCCzGTCGCTCGGGAAATGATAGTACGATAGATGATAGATCCCGCTCGATCGATCGCAAACT\")
      :: Warning: Unknown codon \"zGT\", position 15
      --> \"MRRVP-RSGNDSTIDDRSRSIDRK\"

      (TRANSLATION-OF \"ATGCGTCGGGTCCCCzGTCGCTCGGGAAATGATAGTACGATAGATGATAGATCCCGCTCGATCGATCGCAAACT\")
      --> \"MRRVP-RSGNDSTIDDRSRSIDRK\"


      (TRANSLATION-OF \"ATGCGTCGGGTCCCCzGTCGCTCGGGAAATGATAGTACGATAGATGATAGATCCCGCTCGATCGATCGCAAACT\" IF-BAD-CODON \"#\")
      --> \"MRRVP#RSGNDSTIDDRSRSIDRK\"

      (TRANSLATION-OF rrn5s)
      :: Warning: 
      You are trying to translate noncoding gene #$PRO1375.rrn5S!
      If you want this gene translated, use the NONCODING option.
      If you do not want this warning to appear use the NOWARNINGS option
      --> NIL

      (TRANSLATION-OF rrn5s NOWARNINGS)
      --> NIL

      (TRANSLATION-OF rrn5s NONCODING)
      --> \"SWCSSRCGPTPIHPELGCETHQRRRYLGGSPLRK*LNA\""

)
                  
(:TEXT (:p "Translates DNA sequence into amino-acid sequence.")
(:ul  
  (:li "Sequence may be provided directly or as a gene name")
  (:li "If a nucleotide besides G, A, T, or C, is encountered, then a hyphen is inserted in the position of the current amino acid" (:br "and a warning
   issued, unless the flag IF-BAD-CODON is specified with a value, for example: \"#\"."))
  (:li "If given a non-coding gene the function returns NIL and a warning issued, unless the flag NONCODING is specified.")
  (:li "If given a sequence with a non-nucleotide character or a non-coding gene and if the flags IF-BAD-CODON or NONCODING" (:br "are not specified, a warning issued unless specified NOWARNINGS."))))
  (:SEE-ALSO TRANSLATE-D/RNA-TO-AA SEQUENCE-OF PROTEIN-OF))

;;================================= READING-FRAMES-OF ==============================

(DOCUMENT-FUNCTION READING-FRAMES-OF
(:PARAMETERS 
      (entity :docstring "DNA sequence to be translated")
      (line-length :docstring "To modidify the format, in which resulting sequences are displayed")
      (segment-length :docstring "To modidify the format, in which resulting sequences are displayed")
      (do-not-display :docstring "To avoid resulting sequences to be displayed")

)
(:EXAMPLES
"1. How to use the function?
      (READING-FRAMES-OF \"ATCGTAGCTACCGACTACGGACTACGATCGTACGATCGACTACGATCGCATCATTCAGACTACT\")
      :: 
                 SEQUENCE     1 ATCGTAGCTA CCGACTACGG ACTACGATCG TACGATCGAC TACGATCGCA  
      TRANSLATION-FRAME-1     1 I  V  A  T   D  Y  G   L  R  S   Y  D  R  L   R  S  H   
      TRANSLATION-FRAME-2     1  S  *  L   P  T  T  D   Y  D  R   T  I  D   Y  D  R  I   
      TRANSLATION-FRAME-3     1   R  S  Y   R  L  R   T  T  I  V   R  S  T   T  I  A    
               COMPLEMENT     1 TAGCATCGAT GGCTGATGCC TGATGCTAGC ATGCTAGCTG ATGCTAGCGT  
      TRANSLATION-FRAME-4     1    R  L  *   R  S  R   V  V  I   T  R  D  V   V  I  A   
      TRANSLATION-FRAME-5     1   D  Y  S   G  V  V   S  *  S  R   V  I  S   *  S  R    
      TRANSLATION-FRAME-6     1     T  A   V  S  *  P   S  R  D   Y  S  R   S  R  D  C  

                 SEQUENCE    51 TCATTCAGAC TACT 
      TRANSLATION-FRAME-1    51  H  S  D   Y 
      TRANSLATION-FRAME-2    51   I  Q  T   T 
      TRANSLATION-FRAME-3    51 S  F  R  L 
               COMPLEMENT    51 AGTAAGTCTG ATGA 
      TRANSLATION-FRAME-4    51  D  N  L   S  S 
      TRANSLATION-FRAME-5    51 M  M  *  V   V 
      TRANSLATION-FRAME-6    51   *  E  S   * 
      --> NIL


      (READING-FRAMES-OF all0004)
      :: 
                 SEQUENCE     1 ATGCCTAATC TCAAATCAAT ACGCGATCGC ATTCAGTCGG TCAAAAACAC  
      TRANSLATION-FRAME-1     1 M  P  N  L   K  S  I   R  D  R   I  Q  S  V   K  N  T   
      TRANSLATION-FRAME-2     1  C  L  I   S  N  Q  Y   A  I  A   F  S  R   S  K  T  P  
      TRANSLATION-FRAME-3     1   A  *  S   Q  I  N   T  R  S  H   S  V  G   Q  K  H    
               COMPLEMENT     1 TACGGATTAG AGTTTAGTTA TGCGCTAGCG TAAGTCAGCC AGTTTTTGTG  
      TRANSLATION-FRAME-4     1   H  R  I   E  F  *   Y  A  I  A   N  L  R   D  F  V    
      TRANSLATION-FRAME-5     1     G  L   R  L  D  I   R  S  R   M  *  D   T  L  F  V  
      TRANSLATION-FRAME-6     1    A  *  D   *  I  L   V  R  D   C  E  T  P   *  F  C   

                 SEQUENCE    51 CAAGAAAATC ACAGAAGCCA TGCGGCTGGT AGCGGCGGCG CGTGTACGTC  
      TRANSLATION-FRAME-1    51  K  K  I   T  E  A  M   R  L  V   A  A  A   R  V  R  R  
      TRANSLATION-FRAME-2    51   R  K  S   Q  K  P   C  G  W  *   R  R  R   V  Y  V    
      TRANSLATION-FRAME-3    51 Q  E  N  H   R  S  H   A  A  G   S  G  G  A   C  T  S   
               COMPLEMENT    51 GTTCTTTTAG TGTCTTCGGT ACGCCGACCA TCGCCGCCGC GCACATGCAG  
      TRANSLATION-FRAME-4    51 G  L  F  D   C  F  G   H  P  Q   Y  R  R  R   T  Y  T   
      TRANSLATION-FRAME-5    51   L  F  I   V  S  A   M  R  S  T   A  A  A   R  T  R    
      TRANSLATION-FRAME-6    51  W  S  F   *  L  L  W   A  A  P   L  P  P   A  H  V  D  

                 SEQUENCE   101 GCGCCCAAGA ACAAGTAATC GCTACTCGTC CTTTTGCTGA CCGTTTGGCA ...  
      TRANSLATION-FRAME-1   101   A  Q  E   Q  V  I   A  T  R  P   F  A  D   R  L  A   ... 
      TRANSLATION-FRAME-2   101 A  P  K  N   K  *  S   L  L  V   L  L  L  T   V  W  H  ... 
      TRANSLATION-FRAME-3   101  R  P  R   T  S  N  R   Y  S  S   F  C  *   P  F  G  T ... 
               COMPLEMENT   101 CGCGGGTTCT TGTTCATTAG CGATGAGCAG GAAAACGACT GGCAAACCGT ... 
      TRANSLATION-FRAME-4   101  A  G  L   F  L  Y  D   S  S  T   R  K  S   V  T  Q  C ... 
      TRANSLATION-FRAME-5   101 R  A  W  S   C  T  I   A  V  R   G  K  A  S   R  K  A  ... 
      TRANSLATION-FRAME-6   101   R  G  L   V  L  L   R  *  E  D   K  Q  Q   G  N  P   ... 
      --> NIL

"
"2. Illustrate LINE-LENGTH, SEGMENT-LENGTH and DO-NOT-DISPLAY
      (READING-FRAMES-OF \"ATCGTAGCTACCGACTACGGACTACGATCGTACGATCGACTACGATCGCATCATTCAGACTACT\" LINE-LENGTH 100 SEGMENT-LENGTH 50)
      :: 
                 SEQUENCE     1 ATCGTAGCTACCGACTACGGACTACGATCGTACGATCGACTACGATCGCA TCATTCAGACTACT 
      TRANSLATION-FRAME-1     1 I  V  A  T  D  Y  G  L  R  S  Y  D  R  L  R  S  H   H  S  D  Y 
      TRANSLATION-FRAME-2     1  S  *  L  P  T  T  D  Y  D  R  T  I  D  Y  D  R  I   I  Q  T  T 
      TRANSLATION-FRAME-3     1   R  S  Y  R  L  R  T  T  I  V  R  S  T  T  I  A   S  F  R  L 
               COMPLEMENT     1 TAGCATCGATGGCTGATGCCTGATGCTAGCATGCTAGCTGATGCTAGCGT AGTAAGTCTGATGA 
      TRANSLATION-FRAME-4     1    R  L  *  R  S  R  V  V  I  T  R  D  V  V  I  A   D  N  L  S  S 
      TRANSLATION-FRAME-5     1   D  Y  S  G  V  V  S  *  S  R  V  I  S  *  S  R   M  M  *  V  V 
      TRANSLATION-FRAME-6     1     T  A  V  S  *  P  S  R  D  Y  S  R  S  R  D  C   *  E  S  * 
      --> NIL
      
      (READING-FRAMES-OF \"ATCGTAGCTACCGACTACGGACTACGATCGTACGATCGACTACGATCGCATCATTCAGACTACT\" DO-NOT-DISPLAY)
      --> (\"IVATDYGLRSYDRLRSHHSDY\" \"S*LPTTDYDRTIDYDRIIQTT\" \"RSYRLRTTIVRSTTIASFRL\"
       \"SSLNDAIVVDRTIVVRSR*LR\" \"VV*MMRS*SIVRS*SVVGSYD\" \"*SE*CDRSRSYDRSP*SVAT\")
"


)
                  
(:TEXT (:p "READING-FRAMES-OF translates DNA sequences to 6 possible amino-acid sequences.")

(:ul  
  (:li "The function translates the given sequence and its complement.")
  (:li "The translation starts at the positions 1, 2 and 3 of the given and complemented sequences.")
  (:li "The given DNA sequence may be a string, a gene or a protein (in this case the gene of the given protein is translated by the function.")
  (:li "A list of strings, genes or proteins may be given.")
  (:li "The options LINE-LENGTH and SEGMENT-LENGTH followed by a number may be used to modify the format in which the resulting sequences are displayed.")
  (:li "If you do not want the resulting sequences displayed in a pretty format, use the flag DO-NOT-DISPLAY."))
)
  (:SEE-ALSO TRANSLATION-OF))




;;============================= CHOOSE-FROM ============================
(DOCUMENT-FUNCTION CHOOSE-FROM 
(:PARAMETERS 
(sequence :DOCSTRING "the sequence you use")
)
(:RETURNS "element from sequence")
(:EXAMPLES 
"1. (CHOOSE-FROM {\"A\" 1 2 \"3\"})
    -->2"
"2. (CHOOSE-FROM (SEQUENCE-OF A7120 FROM 10 TO 20))
    -->\"CTATAACTAAT\""
"3. (CHOOSE-FROM (GENES-OF A7120))
    --> #$A7120.all1123
    (CHOOSE-FROM (GENES-OF A7120))
    --> #$A7120.all5040")
(:TEXT
(:P "1. The function selects a random element from a sequence. Almost each time the function is used, the result is different (especially when you have long sequences). ")
(:P "2. You cannot use more than one sequence at a time.")
(:p "3. Also randomly chooses an element from a list (see example 2).")
 ))
