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

;;; Authors: Victor Clarke, Joe Anderson, Jeff Elhai, Bogdan Mihai, Craig Noe
;;; Arnaud Taton, Robel Wolde

; ================= SAME ====================
(DOCUMENT-FUNCTION SAME
(:SUMMARY
"Compares two items that may or may not be of the same type."
)
(:SYNTAX
(SAME x [AS] y [CASE-SENSITIVE][CASE-INSENSITIVE])
)
(:PARAMETERS
(x
:DOCSTRING "The first input to be compared."
:PARAMETER-TYPE required
:VALUE-TYPE any
)
(y
:DOCSTRING "The second input to be compared."
:PARAMETER-TYPE required
:VALUE-TYPE any
)
(case-sensitive
:DOCSTRING "Considers the case of the inputs."
:PARAMETER-TYPE :FLAG
)
(case-insensitive
:DOCSTRING "(Default) Ignores the case of the inputs."
:PARAMETER-TYPE :FLAG
)
)
(:RETURNS
"Returns (T)rue if two inputs are the same, NIL if not."
)
(:EXAMPLES
"Detailed below, graphically."
)
(:TEXT
(:p (:li "Items may be strings, symbols, or frames.")
(:li "If inputs are lists, then the function compares them item by item."))

(:p (:b "1. Comparing strings:"))
(:p "1a.")
(:img :src "/weblistenerdocs/bbldf/images/Same_1.jpg")
(:p "")
(:ul (:img :src "/weblistenerdocs/bbldf/images/Same_NIL_Result.jpg"))

(:p "1b. Two inputs are evaluated case-insensitive.")
(:img :src "/weblistenerdocs/bbldf/images/Same_2.jpg")
(:p (:b "NOTE: ") "Case is ignored if neither case-sensitive nor
case-insensitive is specified")
(:ul (:img :src "/weblistenerdocs/bbldf/images/Same_True_Result.jpg"))

(:p "1c.")
(:img :src "/weblistenerdocs/bbldf/images/Same_3.jpg")
(:p "")
(:ul (:img :src "/weblistenerdocs/bbldf/images/Same_True_Result.jpg"))

(:p "1d.")
(:img :src "/weblistenerdocs/bbldf/images/Same_6.jpg")
(:p "")
(:ul (:img :src "/weblistenerdocs/bbldf/images/Same_True_Result.jpg"))

(:p (:b "2. Comparing numbers:"))
(:p "2a. The variable " (:i "even-to-10 ") "is defined first.")
(:img :src "/weblistenerdocs/bbldf/images/Same_4.jpg")
(:p "")
(:img :src "/weblistenerdocs/bbldf/images/Same_5.jpg")
(:p "")
(:ul (:img :src "/weblistenerdocs/bbldf/images/Same_True_Result.jpg"))

(:p "2b.")
(:img :src "/weblistenerdocs/bbldf/images/Same_7.jpg")
(:p "")
(:ul (:img :src "/weblistenerdocs/bbldf/images/Same_True_Result.jpg"))

(:p (:b "3. Using a looping process:"))
(:p "Looks for and returns the list of all genes described as
\"WD-40 repeat protein\" in Anabaena PCC 7120")
(:img :src "/weblistenerdocs/bbldf/images/Same_8.jpg")
(:p "")
(:ul (:img :src "/weblistenerdocs/bbldf/images/Same_8_Result.jpg"))
)
(:KEYWORDS
Same Compare
)
(:SEE-ALSO
ORDER LIST
)
)

;;================================= SECOND ==============================
(DOCUMENT-FUNCTION BBL::SECOND
(:SUMMARY
"Returns the second item of a list or second character of a string."
)
(:PARAMETERS
(entity
:DOCSTRING "The list or string in which you are seeking the element."
)
(in
:DOCSTRING "Will take the second element of a string or a list,
ignoring the internal structure of the list."
)
(in-each
:DOCSTRING "When used on a list of strings, the second element of
each string is returned; when used on a list of lists, the second
element of each list is returned; (If a list contains strings and
lists, the second element of each is returned)."
)
(nonstrict
:DOCSTRING "If a list does not have at least 2 items, NIL will
be returned."
)
(strict
:DOCSTRING "(Default) Will return an error if this is used and
there are not at least 2 items in each list."
)
)
(:RETURNS
"The second object of any type within a list or string."
)
(:EXAMPLES
"Detailed below, graphically."
)
(:TEXT
(:p (:b "1. Number lists."))
(:img :src "/weblistenerdocs/bbldf/images/Second_1.jpg")
(:p "")
(:ul (:img :src "/weblistenerdocs/bbldf/images/Second_1_Result.jpg"))

(:p (:b "2. String of characters."))
(:img :src "/weblistenerdocs/bbldf/images/Second_2.jpg")
(:p "")
(:ul (:img :src "/weblistenerdocs/bbldf/images/Second_2_Result.jpg"))

(:p (:b "3. Symbols"))
(:img :src "/weblistenerdocs/bbldf/images/Second_3.jpg")
(:p "")
(:ul (:img :src "/weblistenerdocs/bbldf/images/Second_3_Result.jpg"))

(:p (:b "If " (:u "IN-EACH") " is used, the second item of each list is
returned seen in the following examples:"))
(:p (:b "4a."))
(:img :src "/weblistenerdocs/bbldf/images/Second_4a.jpg")
(:p "")
(:ul (:img :src "/weblistenerdocs/bbldf/images/Second_4a_Result.jpg"))

(:p (:b "4b."))
(:img :src "/weblistenerdocs/bbldf/images/Second_4b.jpg")
(:p "")
(:ul (:img :src "/weblistenerdocs/bbldf/images/Second_4b_Result.jpg"))

(:p (:b "5a."))
(:img :src "/weblistenerdocs/bbldf/images/Second_5a.jpg")
(:ul (:img :src "/weblistenerdocs/bbldf/images/Second_5a_Result.jpg"))
(:ul (:p (:li "This causes an error because some organisms do not have at
least two replicons so there is not a second element in the list to
return.")))

(:p (:b "5b. ")"Use NONSTRICT to return NIL when there is not a second
item in the list(s) that is being searched.")
(:img :src "/weblistenerdocs/bbldf/images/Second_5b.jpg")
(:ul (:img :src "/weblistenerdocs/bbldf/images/Second_5b_Result.jpg"))
(:ul (:p (:li "When 'NONSTRICT' is used, the organisms that do not have at
least two replicons return NIL.")))
)
(:KEYWORDS
Second
)
(:SEE-ALSO
BBL:FIRST BBL:THIRD BBL:FOURTH BBL:FIFTH BBL:SIXTH BBL:SEVENTH
BBL:EIGHTH BBL:NINTH BBL:TENTH
)
)

;;=============================SEQUENCE-DOWNSTREAM-OF=========================
(DOCUMENT-FUNCTION SEQUENCE-DOWNSTREAM-OF
(:SUMMARY
"Returns nucleotide sequence downstream from given gene."
)
(:SYNTAX
(SEQUENCE-DOWNSTREAM-OF gene [LABELED] [LENGTH number])
(SEQUENCES-DOWNSTREAM-OF gene [LABELED] [LENGTH number])
)
(:PARAMETERS
(gene
:DOCSTRING "Gene whose downstream sequence is looked at."
:PARAMETER-TYPE required
:VALUE-TYPE gene
)
(labeled
:DOCSTRING "Attaches labels to the output."
:PARAMETER-TYPE :FLAG
)
(length
:DOCSTRING "Specifies the length of the sequence."
:PARAMETER-TYPE keyword
:VALUE-TYPE number
)
)
(:RETURNS
"This function returns the nucleotide sequence downstream of a given gene."
)
(:EXAMPLES
"Detailed below, graphically."
)
(:TEXT
(:p (:b "1. The whole sequence is displayed if no options are chosen."))
(:img :src "/weblistenerdocs/bbldf/images/Sequence-Downstream-Of_1.jpg")
(:p "")
(:ul (:img :src "/weblistenerdocs/bbldf/images/Sequence-Downstream-Of_1_Result.jpg"))

(:p (:b "2. Will only show the first 5 nucleotides of the gene."))
(:img :src "/weblistenerdocs/bbldf/images/Sequence-Downstream-Of_2.jpg")
(:p "")
(:ul(:img :src "/weblistenerdocs/bbldf/images/Sequence-Downstream-Of_2_Result.jpg"))

(:p (:b "3. The option 'LABELED' will tag the sequence."))
(:img :src "/weblistenerdocs/bbldf/images/Sequence-Downstream-Of_3.jpg")
(:p "")
(:ul(:img :src "/weblistenerdocs/bbldf/images/Sequence-Downstream-Of_3_Result.jpg"))
)
(:KEYWORDS
Downstream Sequence Gene
)
(:SEE-ALSO
GENE-UPSTREAM-OF GENE-LEFT-OF GENE-RIGHT-OF GENE-OF
SEQUENCE-UPSTREAM-OF GENE-DOWNSTREAM-OF
)
)

; ================= SEQUENCE-LEFT-OF ====================
(DOCUMENT-FUNCTION BBL::SEQUENCE-LEFT-OF
(:SUMMARY
"Returns nucleotide sequence left of given gene."
)
(:SYNTAX
(SEQUENCE-LEFT-OF gene [LABELED] [LENGTH number])
(SEQUENCES-LEFT-OF gene [LABELED] [LENGTH number])
)
(:PARAMETERS
(gene
:DOCSTRING "A nucelotide sequence will be displayed that is to left
of this gene."
:PARAMETER-TYPE required
:VALUE-TYPE gene
)
(labeled
:DOCSTRING "An option to label the sequence."
:PARAMETER-TYPE :FLAG
)
(length
:DOCSTRING "A limiting specification of the sequence length
that will be displayed."
:PARAMETER-TYPE keyword
:VALUE-TYPE number
)
)
(:RETURNS
"A list, string, labeled sequence, or NILL."
)
(:EXAMPLES
"Detailed below, graphically."
)
(:TEXT
(:p (:b "1. Choosing to label and restrict the length of the
sequence."))
(:img :src "/weblistenerdocs/bbldf/images/Sequence-Left-Of_1.jpg")
(:p (:ul (:li (:i "Only 50 nucleotides of the sequence left of the gene
chosen will be displayed."))))
(:ul (:img :src "/weblistenerdocs/bbldf/images/Sequence-Left-Of_1.1.jpg"))
)
(:KEYWORDS
Sequence Left
)
(:SEE-ALSO
SEQUENCE-UPSTREAM-OF SEQUENCE-DOWNSTREAM-OF GENE-RIGHT-OF GENE-LEFT-OF
SEQUENCE-RIGHT-OF SEQUENCE-OF SEQUENCE-SIMILAR-TO
)
)

; ================= SEQUENCE-OF ====================
(DOCUMENT-FUNCTION SEQUENCE/S-OF
(:SUMMARY
"Returns the sequence of the given gene, protein, contig, replicon, or
genome."
)
(:VPL-SYNTAX
  (:FOO 
     (:img :src "/weblistenerdocs/bbldf/images/SEQUENCE-OF-syntax.PNG"))
)

(:PARAMETERS
(entity
:DOCSTRING "The item for which a user wants the genetic sequence."
:PARAMETER-TYPE required
:VALUE-TYPE GENE\,PROTEIN\,ORGANISM\,CONTIGUOUS-SEQUENCE\,LABELED-SEQUENCE\,DOMAIN\,STRING\,SYMBOL\,FRAME
)
(dna
:DOCSTRING "When specified, the resulting sequence is tested to
make sure that all characters are legal for a DNA sequence."
:PARAMETER-TYPE :FLAG
)
(protein
:DOCSTRING "When specified, then the resulting sequence is tested to
make sure that all characters are legal for a protein sequence."
:PARAMETER-TYPE :FLAG
)
(invert
:DOCSTRING "When specified, this will return the reverse complement
of the DNA sequence produced. It is not a legal option for protein
sequences."
:PARAMETER-TYPE :FLAG
)
(truncate
:DOCSTRING "This option is applicable only to linear sequences
(i.e. contigs). When this option is specified, coordinates less
than 1 or greater than the length of the sequence are forced into the
sequence's range by setting coordinates that are too low to 1 and
those that are too high to the length of the sequence."
:PARAMETER-TYPE :FLAG
)
(wrap
:DOCSTRING "This option is applicable only to circular sequences
(i.e. replicons). When this option is specified, coordinates less
than 1 or greater than the length of the sequence are forced into the
sequence's range by wrapping around the circle."
:PARAMETER-TYPE :FLAG
)
(sequence-ok
:DOCSTRING "(Description pending)."
:PARAMETER-TYPE :FLAG
)
(no-stops
:DOCSTRING "(Description pending)."
:PARAMETER-TYPE :FLAG
)
(labeled
:DOCSTRING "When specified, attaches a label to the output if the
sequence has already been defined."
:PARAMETER-TYPE :FLAG
)
(from-genbank
:DOCSTRING "When specified, a GenBank accession number, in quotes,
MUST be entered as the entity and the sequence defined within
GenBank will be returned."
:PARAMETER-TYPE :FLAG
)
(from-kegg
:DOCSTRING "When specified, KEGG (Kyoto Encyclopedia of Genes and
Genomes) is used to obtain the genomic sequence."
:PARAMETER-TYPE :FLAG
)
(from
:DOCSTRING "When specified, the sequence returned begins at the given
coordinate."
:PARAMETER-TYPE keyword
:VALUE-TYPE number
)
(from-end
:DOCSTRING "When specified, the sequence returned begins at the given
offset from the end of the sequence."
:PARAMETER-TYPE keyword
:VALUE-TYPE number
)
(to
:DOCSTRING "When specified, the sequence returned ends at the
given coordinate."
:PARAMETER-TYPE keyword
:VALUE-TYPE number
)
(to-end
:DOCSTRING "When specified, the sequence returned ends at the given
offset from the end of the sequence."
:PARAMETER-TYPE keyword
:VALUE-TYPE number
)
(length
:DOCSTRING "When specified, the resulting sequence's length is
limited by the amount indicated."
:PARAMETER-TYPE keyword
:VALUE-TYPE positive-number
)
(with-label
:DOCSTRING "When specified, a label, chosen by the user, will be
resulted with the sequence."
:PARAMETER-TYPE keyword
:VALUE-TYPE string
)
)
(:RETURNS
"A string or list."
)
(:EXAMPLES
"Detailed below, graphically."
)
(:TEXT
(:p (:u (:b "This function is useful for extracting all or part of a
specified sequence.")))

(:p (:b "1. If a list of entities if given"))
(:img :src "/weblistenerdocs/bbldf/images/Sequence-Of_1.jpg")
(:p (:ul (:li (:i "A list of sequences are returned for each of the
organism's contigs or replicons."))))
(:ul (:img :src "/weblistenerdocs/bbldf/images/Sequence-Of_1.1.jpg"))

(:p (:b "2. If the entity is a string"))
(:img :src "/weblistenerdocs/bbldf/images/Sequence-Of_2.jpg")
(:p (:ul (:li (:i "then all numeric
characters, spaces, tabs, and line-feeds are removed before a
subsequence is extracted from the original sequence, and what remains is
converted to upper-case."))))
(:ul (:img :src "/weblistenerdocs/bbldf/images/Sequence-Of_2.1.jpg")
(:p (:ul (:li "Therefore options " (:b "FROM") " and "(:b "TO") " specify
coordinates in the string with these characters removed, not necessarily
the original string.")
(:li "This can be useful for sequences cut-and-pasted from a variety of
sources.")
(:li "If "(:b "DNA") " or "(:b "PROTEIN") " is specified, then the
resulting sequence is tested to make sure that all characters are
legal."))))

(:p (:b (:u "Legal Operations")))
(:img :src "/weblistenerdocs/bbldf/images/Sequence-Of_Chart.jpg")

(:p (:b (:u "NOTES:"))
(:p "")
(:ul (:li (:b "FROM") " and "(:b "FROM-END") " may not both be specified, but
neither is necessary.")
(:p "")
(:li "Only one among "(:b "TO") " or "(:b "TO-END") " and " (:b "LENGTH")
" may be specified, but it is not necessary to specify any.")
(:p "")
(:li "Negative coordinates and coordinates that go beyond the limits of
replicons, contigs, and strings raise errors unless " (:b "WRAP") " or "
(:b "TRUNCATE") " are specified"
(:ul (:li "Negative coordinates and coordinates that go beyond the
limits of genes and proteins are legal. The only limitation is that a
user may not specify a " (:b "FROM") " or "(:b "FROM-END") " coordinate
beyond the gene's end without also specifying a "(:b "TO") " or "
(:b "TO-END") " coordinate.")
(:li "A user may not specify " (:b "FROM") " greater than "(:b "TO") ";
use the "(:b "INVERT") " lag to return a complemented and reversed
sequence.")
(:li "In the case of protein sequences, negative coordinates specify that
the region upstream from the nominal start of the corresponding gene is
to be translated.")
(:li "This may be useful in looking for start codons of mis-annotated
genes.")))))
)
(:KEYWORDS
Sequence GenBank KEGG
)
(:SEE-ALSO
ORGANISM-OF PROTEINS-OF PROTEIN-OF GENES-OF GENE-OF CONTIG-NAMED
CONTIGS-OF REPLICON-NAMED REPLICON-OF
)
)

; ================= SEQUENCE-RIGHT-OF ====================
(DOCUMENT-FUNCTION BBL::SEQUENCE-RIGHT-OF
(:SUMMARY
"Returns nucleotide sequence right of given gene."
)
(:SYNTAX
(SEQUENCE-RIGHT-OF gene [LABELED] [LENGTH number])
(SEQUENCES-RIGHT-OF gene [LABELED] [LENGTH number])
)
(:PARAMETERS
(gene
:DOCSTRING "A nucelotide sequence will be displayed that is to right
of this gene."
:PARAMETER-TYPE required
:VALUE-TYPE gene
)
(labeled
:DOCSTRING "An option to label the sequence."
:PARAMETER-TYPE :FLAG
)
(length
:DOCSTRING "A limiting specification of the sequence length
that will be displayed."
:PARAMETER-TYPE keyword
:VALUE-TYPE number
)
)
(:RETURNS
"A list, string, labeled sequence, or NILL."
)
(:EXAMPLES
"Detailed below, graphically."
)
(:TEXT
(:p (:b "1. If given a list of genes."))
(:img :src "/weblistenerdocs/bbldf/images/Sequence-Right-Of_1.jpg")
(:p (:ul (:li (:i "Only 30 nucleotides of the sequence right of the gene
chosen will be displayed."))))
(:ul (:img :src "/weblistenerdocs/bbldf/images/Sequence-Right-Of_1.1.jpg"))
)
(:KEYWORDS
Sequence Right
)
(:SEE-ALSO
SEQUENCE-UPSTREAM-OF SEQUENCE-DOWNSTREAM-OF GENE-RIGHT-OF GENE-LEFT-OF
SEQUENCE-LEFT-OF SEQUENCE-OF SEQUENCE-SIMILAR-TO
)
)

;================================= SEQUENCE/S-SIMILAR-TO ==============================
(DOCUMENT-FUNCTION SEQUENCE/S-SIMILAR-TO
(:SUMMARY
"Finds sequences similar to others, by e-value or mismatches."
)
#+nil
(:SYNTAX
(SEQUENCE-SIMILAR-TO [EACH | PATTERN] query [IN] target [PROTEIN-VS-PROTEIN] [DNA-VS-DNA] [PROTEIN-VS-TRANSLATED-DNA] [TRANSLATED-DNA-VS-PROTEIN] [TRANSLATED-DNA-VS-TRANSLATED-DNA] [CASE-SENSITIVE] [NO-DISPLAY] [REMAKE-DATABASE] [MISMATCHES positive-number] [THRESHOLD positive-number] [WORD-SIZE positive-number] [RETURN positive-number])

(SEQUENCES-SIMILAR-TO [EACH | PATTERN] query [IN] target [PROTEIN-VS-PROTEIN] [DNA-VS-DNA] [PROTEIN-VS-TRANSLATED-DNA] [TRANSLATED-DNA-VS-PROTEIN] [TRANSLATED-DNA-VS-TRANSLATED-DNA] [CASE-SENSITIVE] [NO-DISPLAY] [REMAKE-DATABASE] [MISMATCHES positive-number] [THRESHOLD positive-number] [WORD-SIZE positive-number] [RETURN positive-number])
)
(:PARAMETERS
(query
:DOCSTRING "The sequences a user chooses to BLAST with."
:PARAMETER-TYPE required
:VALUE-TYPE GENE\,PROTEIN\,CONTIGUOUS-SEQUENCE\,ORGANISM\,LABELED-SEQUENCE\,STRING\,LIST
)
(target
:DOCSTRING "The group of sequences to BLAST against."
:PARAMETER-TYPE required
:VALUE-TYPE GENE\,PROTEIN\,CONTIGUOUS-SEQUENCE\,ORGANISM\,LABELED-SEQUENCE\,STRING\,LIST
)
(each
:DOCSTRING "A comparison of each item in the query to the target
value(s)."
:PARAMETER-TYPE token
)
(pattern
:DOCSTRING "A comparison of a particular pattern to the target value(s)."
:PARAMETER-TYPE token
)
(protein-vs-protein
:DOCSTRING "Forces a comparison of protein sequences, converting if necessary."
:PARAMETER-TYPE :FLAG
)
(dna-vs-dna
:DOCSTRING "Forces a comparison of DNA sequences, converting if necessary."
:PARAMETER-TYPE :FLAG
)
(protein-vs-translated-dna
:DOCSTRING "A comparison of sequence against translated DNA sequences."
:PARAMETER-TYPE :FLAG
)
(translated-dna-vs-protein
:DOCSTRING "A comparison of translated DNA sequences against protein
sequence."
:PARAMETER-TYPE :FLAG
)
(translated-dna-vs-translated-dna
:DOCSTRING "A comparison of translated DNA sequences"
:PARAMETER-TYPE :FLAG
)
(case-sensitive
:DOCSTRING "(Description pending.)"
)
(no-display
:DOCSTRING "When specified, the matches found will not display in the
pop-up window."
:PARAMETER-TYPE :FLAG
)
(remake-database
:DOCSTRING "Forces an existing database to be recomputed."
:parameter-type :flag
)
(mismatches
:DOCSTRING "A Limiting specification on the maximum mismatches that are
allowed."
:PARAMETER-TYPE keyword
:VALUE-TYPE positive-number
)
(threshold
:DOCSTRING "A Limiting specification referring to the expected value
(E-value)."
:PARAMETER-TYPE keyword
:VALUE-TYPE number
)
(word-size
:DOCSTRING "(Description pending.)"
:PARAMETER-TYPE keyword
)
(return
:DOCSTRING "When specified, this option restricts the amount of similar
matches found to amount specified by user."
:PARAMETER-TYPE keyword
:VALUE-TYPE positive-number
)
(use-database
:docstring
"Specifies a blast database to use or create"
:parameter-type keyword
:value-type string
))

(:RETURNS
"A List or Table"
)

(:TEXT
(:p (:b "1. To find similar sequences."))
(:img :src "/weblistenerdocs/bbldf/images/Sequence-Similar-To_1.jpg")
(:ul (:p (:li (:i "A pop-up window will display:"))))
(:ul (:img :src "/weblistenerdocs/bbldf/images/Sequence-Similar-To_1.1.jpg"))
(:p
"The USE-DATABASE keyword has two purposes:"
(:ul
(:li
"Create a blast database where none existed before for future use")
(:li
"Use a blast database that you or another user created")))
(:p
"The reason for doing this is efficiency. It takes a significant "
"amount of time to create a blast database; if you are running "
"multiple blasts against the same target, you can create the blast "
"database once and use it multiple times instead of having the system "
"create a temporary blast database for each of your runs.")
(:p "To create a new blast database of your own:")
(:p
(:i
"(sequence-similar-to query target :use-database \"my-new-name\")"
))
(:p "To use a blast database that you've previously created:")
(:p
(:i
"(sequence-similar-to query target :use-database \"existing-name\")"
))
(:p
"If the name provided doesn't denote an existing database, one "
"will be created, and you will be notified of this. "
"If the name provided does exist, you will be notified of "
"the fact that the database is being used."
)
(:p "To use a blast database that another user has created:")
(:p
(:i
"(sequence-similar-to query target :use-database \"other-user:db-name\")"
))
(:p
"If 'other-user' has a database called 'db-name', it will be used. "
"If 'other-user' does not have a database called 'db-name', "
"you will get an error."
)
(:p
"The biobike system creates its own set of saved databases, one for "
"each organism, and some for various combinations of organisms. "
"When a blast is run, if the blast uses one of these system-wide "
"saved databases (because the target is a single organism or one of the "
"pre-defined combinations), you will be informed of this."
)
(:p
"If you are a guru, Biobike allows you to create other system-wide "
"stored databases. E.g., ")
(:p
(:i
"(sequence-similar-to query target :use-database \"system:new-db\")"
))
(:p
"and other users can use these databases. A user need not prefix "
"the database name with 'system' as long as the user doesn't have "
"their own database with the same name; the blast code will "
"search the system-wide databases as well as the ones the user "
"has created"
)
(:p
"User databases are stored in subdirectories of the 'blast-database' "
"subdirectory of the user's biobike home directory. System databases "
"are stored in subdirectories of the 'blast-database' subdirectory "
"of the system biobike home directory."
)
(:p
"The REMAKE-DATABASE flag forces an existing database specified "
"by USE-DATABASE to be recomputed from the TARGET data. "
"You are only allowed to remake databases that you have created. "
"However, if you are a guru you can remake any database."
)
)

(:keywords
"Similar, Protein, Mismatches, Threshold, BLAST, Translated DNA"
)

(:SEE-ALSO
IS-GENE? IS-PROTEIN? GENE-OF GENES-OF ORGANISM-OF
PROTEIN-SIMILAR-TO HOMOLOG-OF* TRANSLATION-OF
)
)

;; ================= SEQUENCE-TYPE-OF ====================
(DOCUMENT-FUNCTION SEQUENCE-TYPE-OF
(:SUMMARY
"Determines whether a sequence is DNA, RNA, or protein."
)
(:SYNTAX
(SEQUENCE-TYPE-OF sequence [EXTENDED])
(SEQUENCE-TYPES-OF sequence [EXTENDED])
)
(:PARAMETERS
(sequence
:DOCSTRING "The string to be tested."
:PARAMETER-TYPE required
:VALUE-TYPE string
)
(extended
:DOCSTRING "To be used if protien is suspected."
:PARAMETER-TYPE :FLAG
)
)
(:RETURNS
"A DNA, RNA or protien identifier."
)
(:EXAMPLES
"Detailed below, graphically."
)
(:TEXT
(:p (:b (:u "This function is useful when there are many unknown types of
sequences within a large query.")))

(:p (:b "1. If DNA is suspected."))
(:img :src "/weblistenerdocs/bbldf/images/Sequence-Type-Of_1.jpg")
(:p "")
(:ul (:img :src "/weblistenerdocs/bbldf/images/Sequence-Type-Of_1.1.jpg"))

(:p (:b "2. If a protein is suspected the EXTENDED option must used."))
(:img :src "/weblistenerdocs/bbldf/images/Sequence-Type-Of_2.jpg")
(:p "")
(:ul (:img :src "/weblistenerdocs/bbldf/images/Sequence-Type-Of_2.1.jpg"))

(:p (:b "3. If RNA is suspected."))
(:img :src "/weblistenerdocs/bbldf/images/Sequence-Type-Of_3.jpg")
(:p "")
(:ul (:img :src "/weblistenerdocs/bbldf/images/Sequence-Type-Of_3.1.jpg"))

(:p (:b "NOTES:")
(:ul (:li "If the sequence contians protein, RNA and/or DNA components
within the string, NIL will be returned.")
(:li "If protein is suspected, but the option " (:b "EXTENDED") " is
not chosen, NIL will be returned.")))
)

(:KEYWORDS
DNA RNA protein type kind
)
(:SEE-ALSO
SEQUENCE-DOWNSTREAM-OF SEQUENCE-LEFT-OF SEQUENCE-RIGHT-OF SEQUENCE-OF
SEQUENCE-SIMILAR-TO SEQUENCE-UPSTREAM-OF
)
)

;;==========================SEQUENCE-UPSTREAM-OF============================
(DOCUMENT-FUNCTION SEQUENCE-UPSTREAM-OF
  (:SUMMARY "Returns nucleotide sequence upstream from given gene")
  
  (:PARAMETERS
    (gene :DOCSTRING "gene whose upstream sequence is looked at")
    (length :DOCSTRING "specifies the length of the sequence")
  )
  
  (:EXAMPLES
    "(SEQUENCE-UPSTREAM-OF slr0725)
      --> ATTTGCCAATAGATAAGCCTTTATTGATTAATGGGTGGAAGATATTTGCCCATCCCCTATTTCTTGAGCAGGTTGAAGAACTT"
          "(SEQUENCE-UPSTREAM-OF slr0725 LENGTH 2)
      --> TT"
      "(SEQUENCE-UPSTREAM-OF slr0725 LENGTH 200)
      --> 
      GATAAAAATCCTCAGCATATACAAGCGGTCAGTTCTGAGTTGGTTGATCGTCTGCAATCGCTAGTCTCCGATGTTGATGTAGAT
      CTAGATGCGCCATTGCTGGACGAGGATGAATAGATTTGCCAATAGATAAGCCTTTATTGATTAATGGGTGGAAGATATTTGCCC
      ATCCCCTATTTCTTGAGCAGGTTGAAGAACTT"
          
  )
    
  
  (:TEXT
    (:img :src "http://ramsites.net/~biobike/temp/sequence-upstream-of.JPG")
    
    (:p "This function returns the nucleotide sequence upstream of a given gene.")  
    (:p "The 'LENGTH' option specifies the length of the resulting upstream sequence")
   )   
   
    
)
; ================= SEVENTH ====================
(DOCUMENT-FUNCTION BBL::SEVENTH
(:SUMMARY
"Returns the seventh item of a list or seventh character of a string."
)
(:SYNTAX
(SEVENTH [IN] [IN-EACH] entity [NONSTRICT] [STRICT])
)
(:PARAMETERS
(entity
:DOCSTRING "The list or string in which you are seeking the element."
:PARAMETER-TYPE required
:VALUE-TYPE list\,string
)
(in
:DOCSTRING "Will take the seventh element of a string or a list,
ignoring the internal structure of the list."
:PARAMETER-TYPE :TOKEN
)
(in-each
:DOCSTRING "When used on a list of strings, the seventh element of
each string is returned; when used on a list of lists, the seventh
element of each list is returned; (if a list contains strings and
lists, the seventh element of each is returned)."
:PARAMETER-TYPE :TOKEN
)
(nonstrict
:DOCSTRING "If a list does not have at least 7 items, NIL will be
returned."
:PARAMETER-TYPE :FLAG
)
(strict
:DOCSTRING "(default) Will return an error if this is used and there
are not at least 7 items in each list."
:PARAMETER-TYPE :FLAG
)
)
(:RETURNS
"The seventh object from a string or list."
)
(:EXAMPLES
"Detailed below, graphically."
)
(:TEXT
(:p (:b "1. Using a string of numbers."))
(:img :src "/weblistenerdocs/bbldf/images/Seventh_1.jpg")
(:p "")
(:ul (:img :src "/weblistenerdocs/bbldf/images/Seventh_1.1.jpg"))

(:p (:b "2. Using a string of letters."))
(:img :src "/weblistenerdocs/bbldf/images/Seventh_2.jpg")
(:p "")
(:ul (:img :src "/weblistenerdocs/bbldf/images/Seventh_2.1.jpg"))

(:p (:b "3a. Using the STRICT option."))
(:img :src "/weblistenerdocs/bbldf/images/Seventh_3a.jpg")
(:ul (:p (:li (:i "This causes an error because some organisms don't have
at least seven replicons so there is not a seventh element in the list to
return."))))
(:ul (:img :src "/weblistenerdocs/bbldf/images/Seventh_3.1a.jpg"))

(:p (:b "3b. Using the NON-STRICT option will remove the previous error."))
(:img :src "/weblistenerdocs/bbldf/images/Seventh_3b.jpg")
(:p "")
(:ul (:img :src "/weblistenerdocs/bbldf/images/Seventh_3.1b.jpg"))

(:p (:b "NOTES:"))
(:ul (:p (:li "If " (:b "IN-EACH") " is used, the seventh item of each
list is returned.")
(:li "Use " (:b "NONSTRICT") " to return NIL when there is not a seventh
item in the list(s) that is being searched.")))
)
(:KEYWORDS
seventh
)
(:SEE-ALSO
FIRST SECOND THIRD FOURTH FIFTH SIXTH EIGHTH NINTH TENTH
)
)

(document-function share 
  (:summary 
   "Specify a set of functions and/or variables (a package) for others to use")
  (:returns "The name you gave to the package" :type string)
  (:examples
   "(share trna-calculator variables (a b) functions (foo bar)) --> \"TRNA-CALCULATOR\""
   "(share \"loop-nitrogen-fix\" functions baz) --> \"LOOP-NITROGEN-FIX\""
   "(share chromosome-loop functions (name contigs) docstring \"Useful information\")
    --> \"CHROMOSOME-LOOP\""
   "(share trna-calculator remove) --> \"Shared package TRNA-CALCULATOR no longer exists\""
   )
  (:text
   (:p
    #.(one-string-sp
       "Either causes a set of functions and/or variables (a package)"
       "to be stored so that others may use them, or, with"
       "the REMOVE flag, causes an existing shared package to be deleted."
       ))
   (:p 
    #.(one-string-sp
       "The user must provide a name for the package to be shared,"
       "and at least one variable or function name."
       "If there is more than one variable or more than one function"
       "a list may be provided."
       "The user may also provide documentation describing the purpose"
       "of the package by using the DOCSTRING keyword."
       ))
   (:p 
    #.(one-string-sp
       "If a shared package of name FOO already exists, owned"
       "by someone else, and you attempt to share your own"
       "package named FOO, you will be asked to choose a different name."
       "You are also not allowed to remove a shared package that is not yours."
       "If you own a shared package named FOO, and attempt to share it again"
       "with the same or different functions and variables, your old definition"
       "will be overwritten."
       ))
   (:p
    #.(one-string-sp
       "Once you have shared a package, anyone can access it"
       "by using the File --> User contributed stuff menu option in the VPL."
       "(There is no documented way to use a shared package from"
       "the weblistener at the moment.)"
       ))
   (:p
    #.(one-string-sp
       "When you remove a shared package that you created, the other"
       "users who are already sharing your package are still able to use it,"
       "but no one not currently using it will be able to access it."
       )))
  ;; There's some bug with providing :value-type for parameters in
  ;; define-macros vs define-functions.  But the types can be provided 
  ;; in the define-macro definition itself.  Weird.  
  (:parameters 
   (name 
    :parameter-type required
    :docstring "What to call your set of shared functions and/or variables")
   (functions
    :parameter-type keyword
    :docstring "What functions to include in your shared package")
   (variables
    :parameter-type keyword
    :docstring "What variables to include in your shared package")
   (docstring
    :parameter-type keyword
    :docstring "A description of the package you will be sharing")
   (remove 
    :parameter-type flag 
    :docstring 
    #.(one-string-sp
       "Used to remove an existing shared package, and cannot be used"
       "with the FUNCTIONS, VARIABLES, or DOCSTRING keywords."
       )))
  (:see-also unshare enter)
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




;;================================= SIXTH ==============================
(DOCUMENT-FUNCTION BBL::SIXTH
(:SUMMARY "Returns the sixth item of a list or sixth character of a string.")
(:PARAMETERS
(entity :DOCSTRING "the list or string in which you are seeking the element")
(in :DOCSTRING "will take the sixth element of a string or a list, ignoring the internal structure of the list")
(in-each :DOCSTRING "when used on a list of strings, the sixth element of each string is returned; when used on a list of lists, the sixth element of each list is returned; (if a list contains strings and lists, the sixth element of each is returned)")
(nonstrict :DOCSTRING "if a list does not have at least 6 items, NIL will be returned")
(strict :DOCSTRING "(default) will return an error if this is used and there are not at least 6 items in each list")
)
(:RETURNS "An object of any type")
(:EXAMPLES
"(SIXTH IN {5 8 4 6 11 17 2 3 8 21 6})
    --> 17"
"(SIXTH IN \"ACCTGTGAACGG\")
    --> \"T\""
"(SIXTH IN-EACH (REPLICONS-OF *all-organisms*) STRICT)
    --> ***ERROR***
    (This causes an error because some organisms don't have at least six replicons so there
    is not a sixth element in the list to return.)

    (SIXTH IN-EACH (REPLICONS-OF *all-organisms*) NONSTRICT)
    --> (NIL NIL #$S6803.pCC5.2 #$Npun.pNpE #$A7120.pEPSILON #$Tery.Contig58

    NIL NIL NIL NIL NIL #$Cwat.Contig046 #$A29413.Contig196)
    (When NONSTRICT is used, the organisms that don't have at least six replicons return NIL.)"
)
(:TEXT 
(:p "This function returns the sixth item of a list or the sixth character of a string.")
(:p "If IN-EACH is used, the sixth item of each list is returned.")
(:p "Use NONSTRICT to return NIL when there is not a sixth item in the list(s) that is being searched.")
)
(:SEE-ALSO BBL:FIRST BBL:SECOND BBL:THIRD BBL:FOURTH BBL:FIFTH BBL:SEVENTH BBL:EIGHTH BBL:NINTH BBL:TENTH)
)

;;================================= SORT ==============================

(DOCUMENT-FUNCTION bbl:SORT
  (:SUMMARY "Sorts lists alphabetically or numerically")
  (:VPL-SYNTAX (:FOO (:img :src "/weblistenerdocs/bbldf/images/SORT-syntax.PNG")))
  (:PARAMETERS
    (list :VALUE-TYPE any :DOCSTRING "List to be sorted")
    (ascending :PARAMETER-TYPE flag 
        :DOCSTRING "List is to be sorted in ascending alphabetical order (default)")
    (descending :PARAMETER-TYPE flag 
            :DOCSTRING "List is to be sorted in descending alphabetical order")
    (case-sensitive :PARAMETER-TYPE flag :DOCSTRING 
       "Specifies that comparison is to be performed without regard to capitalization")
    (by-position :VALUE-TYPE Positive-integer :PARAMETER-TYPE keyword :DOCSTRING 
       "Specifies that the comparison is to be performed using the element of the list with the given index")
    (then-sort-ascending-by :VALUE-TYPE Positive-integer :PARAMETER-TYPE keyword :DOCSTRING 
       "Specifies that when sorting a complex list, if the first comparison indicates equality, then a second comparison is to be performed with the given index, sorting the values in ascending order")
    (then-sort-descending-by :VALUE-TYPE Positive-integer :PARAMETER-TYPE keyword :DOCSTRING 
       "Specifies that when sorting a complex list, if the first comparison indicates equality, then a second comparison is to be performed with the given index, sorting the values in descending order")
  )
  (:RETURNS "List")
  (:EXAMPLES
    (:FOO
    (:OL
      (:LI (:B "Simple numeric sort") (:BR)
        (:img :src "/weblistenerdocs/bbldf/images/SORT-simple-numeric.png") (:BR)
        (help::HTML "&nbsp")" " (help::HTML "&nbsp") " " (help::HTML "&nbsp") " " 
        (:img :src "/weblistenerdocs/img-bbldoc/zrightarrow.gif") " "
        (:img :src "/weblistenerdocs/bbldf/images/SORT-simple-numeric-result.png" :border "1") (:BR)
        (:I (:B "Translation: ") "The list of numbers is sorted in (by default) ascending order.") 
        (:BR) (help::HTML "&nbsp"))
        
      (:LI (:B "Simple alphabetical sort") (:BR)
        (:img :src "/weblistenerdocs/bbldf/images/SORT-simple-string.png") (:BR)
        (help::HTML "&nbsp")" " (help::HTML "&nbsp") " " (help::HTML "&nbsp") " " 
        (:img :src "/weblistenerdocs/img-bbldoc/zrightarrow.gif") " "
        (:img :src "/weblistenerdocs/bbldf/images/SORT-simple-string-result.png" :border "1") (:BR)
        (:I (:B "Translation: ") "The sentence is ")
        ((:A :HREF (:PRINT (help::MAKE-HELP-FUNCTION-DOCUMENTATION-URL :NAME "SPLIT" :PACKAGE :bbl)))
                "SPLIT")
        (:I " into its component words, and those words are sorted in (by default) ascending order. "
            "Note that \"Twas\" sorts between \"toves\" and \"ye\".")                
        (:BR) (help::HTML "&nbsp"))
        
      (:LI (:B "Simple alphabetical sort(case sensitive)") (:BR)
        (:img :src "/weblistenerdocs/bbldf/images/SORT-simple-string-CASE-SENSITIVE.png") (:BR)
        (help::HTML "&nbsp")" " (help::HTML "&nbsp") " " (help::HTML "&nbsp") " " 
        (:img :src "/weblistenerdocs/img-bbldoc/zrightarrow.gif") " "
        (:img :src "/weblistenerdocs/bbldf/images/SORT-simple-string-CASE-SENSITIVE-result.png" :border "1") (:BR)
        (:I (:B "Translation: ") "The sentence is ")
        ((:A :HREF (:PRINT (help::MAKE-HELP-FUNCTION-DOCUMENTATION-URL :NAME "SPLIT" :PACKAGE :bbl)))
                "SPLIT")
        (:I " into its component words, and those words are sorted in (by default) ascending order, "
            "taking case (capitalization) into account. "
            "Note that \"Twas\" sorts before any word beginning with a lower case letter, because "
            "all capital letters come before all lower-case letters in a CASE-SENSITIVE sort.")                
        (:BR) (help::HTML "&nbsp"))
        
      (:LI (:B "Simple numeric sort (biological example)") (:BR)
        (:img :src "/weblistenerdocs/bbldf/images/SORT-LENGTHS-OF-GENES-OF.png") (:BR)
        (help::HTML "&nbsp")" " (help::HTML "&nbsp") " " (help::HTML "&nbsp") " " 
        (:img :src "/weblistenerdocs/img-bbldoc/zrightarrow.gif") " "
        (:img :src "/weblistenerdocs/bbldf/images/SORT-LENGTHS-OF-GENES-OF-result.png" :border "1") (:BR)
        (:I (:B "Translation: ") "A list consisting of the ")
        ((:A :HREF (:PRINT (help::MAKE-HELP-FUNCTION-DOCUMENTATION-URL :NAME "LENGTHS-OF" :PACKAGE :bbl)))
                "LENGTHS-OF")
        (:I " the ")
        ((:A :HREF (:PRINT (help::MAKE-HELP-FUNCTION-DOCUMENTATION-URL :NAME "GENES-OF" :PACKAGE :bbl)))
                "GENES-OF")
        (:I " the organism Anabaena PCC 7120 (nicknamed 'A7120') is sorted in DESCENDING order. "
            "The first five lengths are shown, but the list contains all of the lengths.")
        (:BR) (help::HTML "&nbsp"))
        
      (:LI (:B "Sort of complex list by position") (:BR)
        (:img :src "/weblistenerdocs/bbldf/images/SORT-BY-POSITION.png") (:BR)
        (help::HTML "&nbsp")" " (help::HTML "&nbsp") " " (help::HTML "&nbsp") " " 
        (:img :src "/weblistenerdocs/img-bbldoc/zrightarrow.gif") " "
        (:img :src "/weblistenerdocs/bbldf/images/SORT-BY-POSITION-result.png" :border "1") (:BR)
         (:I (:B "Translation: ") "A list of lists is sorted by the second elements of the sublists. "
             "The sorted list is therefore sorted by 1, 1, and 3. There is no assurance which element "
             "will be sorted first in case of tie.")         
        (:BR) (help::HTML "&nbsp"))
        
      (:LI (:B "Sort of complex list by one position and secondary position") (:BR)
        (:img :src "/weblistenerdocs/bbldf/images/SORT-BY-POSITION-THEN-ASCENDING.png") (:BR)
        (help::HTML "&nbsp")" " (help::HTML "&nbsp") " " (help::HTML "&nbsp") " " 
        (:img :src "/weblistenerdocs/img-bbldoc/zrightarrow.gif") " "
        (:img :src "/weblistenerdocs/bbldf/images/SORT-BY-POSITION-THEN-ASCENDING-result.png" :border "1") (:BR)
         (:I (:B "Translation: ") "A list of lists is sorted by the second elements of the sublists. "
             "The sorted list is therefore sorted by 1, 1, and 3. In case of tie (as there is in this case "
             "between 1 and 1), a second comparison between the item in position one determines the order, "
             "with lower numbers sorting earlier because ASCENDING order is specified. Therefore (1 1) and "
             "(2 1) both sort before (1 3) because the item in the second position, 1, is smaller than 3; and "
             "(1 1) sorts before (2 1) because the first element, 1, is smaller than 2.")
        (:BR) (help::HTML "&nbsp"))
        
     ))
  )

  (:TEXT
    (:B "Numerical vs Alphabetical Sorts")(:BR)
  #.(ONE-STRING-sp
        "Simple lists are sorted alphabetically or numerically, as appropriate,"
        "as described in ")
    ((:A :HREF (:PRINT (help::MAKE-HELP-TOPIC-URL 
                    :NAME "How to compare two quantities (numeric vs alphabetical)")))
                 "How to compare two quantities (numeric vs alphabetical)")
    ". A numerical tests is used to decide the position of number pairs, otherwise "
    "an alphabetical tests is used. Alphabetical tests are case-insentitive (i.e., "
    "capitalization is ignored) unless the CASE-SENSITIVE flag is used."

     (:P (:B "Ascending vs Descending Sorts") (:BR)
        "By default, items are sorted in ascending order. The DESCENDING flag "
        "may be used to reverse the order. The ASCENDING flag is provided for "
        "sake of completeness. It has no effect.")

     (:P (:B "Sorting lists of lists and multiple criteria") (:BR)
    
       "When sorting lists of lists, items are sorted by default according to the "
       "first element. You can use the BY-POSITION option to specify the position "
       "in the sublists to determine the sort.")

     (:P "When a list of lists is sorted, there is the possibility that two elements "
         "may be nonidentical but nonetheless be equal with regards to the sub-item "
         "determining the order of the sort. In such cases, the order is not "
         "predictable: either item may appear first in the sorted list. The original "
         "order of the list does not determine the ultimate order. However, it is "
         "possible to break ties by comparing a different position in each sub-list, "
         "as shown in Example #6 (compare this example with Example #5). You can "
         "specify a second position for a comparison used only in case of ties, and "
         "that position may be used to sort in ascending or descending order. "
         "The THEN-SORT-ASCENDING-BY and THEN-SORT-DESCENDING-BY are options used "
         "for this purpose. There is currently no easy way at present to sort a "
         "complex list according to more than two criteria.")
  )
 ;(:SEE-ALSO )
)

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

;;============================STD-DEV==============

(DOCUMENT-FUNCTION STD-DEV
(:PARAMETERS
(list :DOCSTRING "list of numbers"))
(:EXAMPLES
"1. (ASSIGN list {1 2 3 4 5})
(STD-DEV list)
--> 1.5811388"

"2. (STD-DEV (RANDOM-INTEGERS FROM 1 TO 100 LIST-OF 10000))
--> 28.908432 ; (Your answer will be different, but not by much)")

(:RETURNS "A number")
(:TEXT
(:p (:ul "The result is the standard deviation of the list of numbers."))
(:p (:ul "Standard deviation is a useful measure of the variability within a set of numbers. The greater the standard deviation, the greater the average distance of points from the mean."))
));;============================STD-DEV==============

(DOCUMENT-FUNCTION STD-DEV
(:PARAMETERS
(list :DOCSTRING "list of numbers"))
(:EXAMPLES
"1. (ASSIGN list {1 2 3 4 5})
(STD-DEV list)
--> 1.5811388"

"2. (STD-DEV (RANDOM-INTEGERS FROM 1 TO 100 LIST-OF 10000))
--> 28.908432 ; (Your answer will be different, but not by much)")

(:RETURNS "A number")
(:TEXT
(:p (:ul "The result is the standard deviation of the list of numbers."))
(:p (:ul "Standard deviation is a useful measure of the variability within a set of numbers. The greater the standard deviation, the greater the average distance of points from the mean."))
))
;===================================SUBLIST=================================
(DOCUMENT-FUNCTION SUBLIST
(:SUMMARY "Extracts elements from a list")
(:PARAMETERS
(target :DOCSTRING "the list whose elements will be extracted")
(randomize :DOCSTRING "randomizes the order of elements of the list")
(randomized :DOCSTRING "identical to randomize")
(reverse :DOCSTRING "reverses the order of elements of the list")
(reversed :DOCSTRING "identical to reverse")
(from :DOCSTRING "specifies start of extraction")
(to :DOCSTRING "specifies end of extraction")
(by :DOCSTRING "extracts every i-th character")
(length :DOCSTRING "specifies the length of the list extracted")
(item :DOCSTRING "specifies location(s) of elements extracted")
)

(:EXAMPLES "Detailed below, graphically:")
(:TEXT
(:p "This function extracts elements from a list.")
(:p "The FROM and TO options specifies the positions where list extraction begins and ends.")
(:p "The REVERSE and REVERSED options are identical and reverse the order of the elements list. The RANDOMIZE and RANDOMIZED options are identical and randomize the order of the elements of a string.")
(:p "The BY option allows the user to extract every n-th element. For example,
 (SUBLIST x BY 3) extracts every third element.")
(:p "The LENGTH option is used to indicate how long a list is desired.")
(:p "The ITEM option is used to pick out specific elements within the list.")
(:p "")
(:p "To find some elements in a list of amino acids:")
(:img :src "/weblistenerdocs/bbldf/images/inside-list1.jpg") 
(:p "Returns:")
(:img :src "/weblistenerdocs/bbldf/images/inside-list2.jpg")
(:p "")
(:p "To find every other amino acid in a list:")
(:img :src "/weblistenerdocs/bbldf/images/inside-list3.jpg") 
(:p "Returns:")
(:img :src "/weblistenerdocs/bbldf/images/inside-list4.jpg")
)

(:KEYWORDS list extraction sublist)
(:SEE-ALSO SEQUENCE-OF)

)

;===========================SUBLIST====================================
(DOCUMENT-FUNCTION SUBLIST-OF
  (:SUMMARY "Extracts items from a list")
  (:VPL-SYNTAX (:FOO (:img :src "/weblistenerdocs/bbldf/images/SUBLIST-OF-syntax.jpg")))

  (:PARAMETERS
    (target :DOCSTRING "the list whose items will be extracted")
    (each :DOCSTRING "When selected, sublists are extracted from each item in target (which must be a list)")
    (by :DOCSTRING "extracts every nth item")
    (from :DOCSTRING "specifies the starting position of the extraction")
    (item/s :DOCSTRING "specifies location(s) of item(s) extracted")
    (length :DOCSTRING "specifies the length of the sublist, i.e. the number of items extracted")
    (randomized :DOCSTRING "randomizes the order of items of the sublist")
    (reversed :DOCSTRING "reverses the order of items of the sublist")
    (to :DOCSTRING "specifies the last position of the extraction")
  )
  (:RETURNS "list")
  
  (:EXAMPLES
     (:FOO
      (:BLOCKQUOTE
        (:img :src "/weblistenerdocs/bbldf/images/SUBLIST-OF-ex1.jpg") 
        (:BR)"--> (\"b\" \"c\" \"d\")"
        (:BR) "Extracts the items between positions 2 and 5, inclusive"

        (:P (:img :src "/weblistenerdocs/bbldf/images/SUBLIST-OF-ex2.jpg") 
        (:BR)"--> (\"d\" \"c\" \"b\")"
        (:BR) "Extracts the items between positions 5 down to 2, inclusive")

        (:P (:img :src "/weblistenerdocs/bbldf/images/SUBLIST-OF-ex3.jpg") 
        (:BR)"--> (\"b\" \"d\" \"f\")"
        (:BR) "Extracts the items from positions 2 to the end, counting by 2")

        (:P (:img :src "/weblistenerdocs/bbldf/images/SUBLIST-OF-ex4.jpg") 
        (:BR)"--> (\"d\" \"c\" \"b\")"
        (:BR) "Extracts the items from position 2 and onwards, until 3 "
            "items have been extracted." 
        (:BR) "Then the substring is reversed.")

        (:P (:img :src "/weblistenerdocs/bbldf/images/SUBLIST-OF-ex6.jpg") 
        (:BR)"--> (\"a\" \"c\" \"e\" \"d\" \"b\")"
        (:BR) "Extracts the items at positions 1, 3, 5, 4, and 2.")

        (:P (:img :src "/weblistenerdocs/bbldf/images/SUBLIST-OF-ex5.jpg") 
        (:BR)"--> (\"d\" \"b\" \"c\")"
        (:BR) "Extracts the items from the beginning of the list to position 3."
        (:BR) "Then the sublist is put in a random order")

        (:P (:img :src "/weblistenerdocs/bbldf/images/SUBLIST-OF-ex7.jpg") 
        (:BR)"--> ((\"b\" \"a\")(\"d\" \"c\"))"
        (:BR) "Extracts the items at positions 2 and 1, separately for each list in the target.")

        )))
  (:TEXT
    (:p "This function extracts items from a list, producing a sublist.")  

    (:p "The FROM, TO, and LENGTH options specifies the positions where list extraction begins and ends. The values must be positive numbers. If the specified end of the substring goes beyond the end of the list, extraction will end with the end of the list.")

    (:p "The REVERSED option causes the sublist to be returned from its end to its beginning. This can also be achieved by providing a FROM value that is greater than the TO value.")

    (:P "The RANDOMIZED option causes the sublist to be returned with its items in a random order.")

    (:p "The BY and ITEM options allows noncontiguous characters of the string to be extracted.")  

  )

  (:SEE-ALSO ELEMENT/S-OF)
    
)

;===========================SUBSTRING====================================
(DOCUMENT-FUNCTION SUBSTRING-OF
  (:SUMMARY "Extracts characters from a string")
  (:VPL-SYNTAX (:FOO (:img :src "/weblistenerdocs/bbldf/images/SUBSTRING-OF-syntax.PNG")))

  (:PARAMETERS
    (target :DOCSTRING "the string whose characters will be extracted")
    (randomized :DOCSTRING "randomizes the order of characters of the substring")
    (reversed :DOCSTRING "reverses the order of characters of the substring")
    (from :DOCSTRING "specifies the start of extraction")
    (to :DOCSTRING "specifies the end of extraction")
    (by :DOCSTRING "extracts every nth character")
    (length :DOCSTRING "specifies the length of the string extracted")
    (item/s :DOCSTRING "specifies location(s) of characters extracted")
  )
  (:RETURNS "String or list of strings")
  
  (:EXAMPLES
     (:FOO
      (:BLOCKQUOTE
        (:img :src "/weblistenerdocs/bbldf/images/SUBSTRING-OF-ex-1.PNG") 
        (:BR)"--> \"bcde\""
        (:BR) "Extracts the characters between positions 2 and 5, inclusive"

        (:P (:img :src "/weblistenerdocs/bbldf/images/SUBSTRING-OF-ex-2.PNG") 
        (:BR)"--> \"edcb\""
        (:BR) "Extracts the characters between positions 5 down to 2, inclusive")

        (:P (:img :src "/weblistenerdocs/bbldf/images/SUBSTRING-OF-ex-3.PNG") 
        (:BR)"--> \"bdf\""
        (:BR) "Extracts the characters from positions 2 to the end, counting by 2")

        (:P (:img :src "/weblistenerdocs/bbldf/images/SUBSTRING-OF-ex-4.PNG") 
        (:BR)"--> \"gfe\""
        (:BR) "Extracts the characters from position 5 and onwards, until 3 "
            "characters have been extracted." 
        (:BR) "Then the substring is reversed.")

        (:P (:img :src "/weblistenerdocs/bbldf/images/SUBSTRING-OF-ex-5.PNG") 
        (:BR)"--> \"acedb\""
        (:BR) "Extracts the characters at positions 1, 3, 5, 4, and 2, and joins them")

        (:P (:img :src "/weblistenerdocs/bbldf/images/SUBSTRING-OF-ex-6.PNG") 
        (:BR)"--> \"cab\""
        (:BR) "Extracts the characters from the beginning of the string to position 3."
        (:BR) "Then the substring is put in a random order")
  )))
  (:TEXT
    (:p "This function extracts characters from a string, producing a substring.")  

    (:p "The FROM, TO, and LENGTH options specifies the positions where string extraction begins and ends. The values must be positive numbers. If the specified end of the substring goes beyond the end of the string, extraction will end with the end of the string. Wrapping around to the beginning is possible only with contigs specified as circular and only using the SEQUENCE-OF function.")

    (:p "The REVERSED option causes the substring to be returned from its end to its beginning. This can also be achieved by providing a FROM value that is greater than the TO value.")

    (:P "The RANDOMIZED option causes the substring to be returned with its charactes in a random order.")

    (:p "The BY and ITEM options allows noncontiguous characters of the string to be extracted.")  

  )

  (:SEE-ALSO SEQUENCE-OF)
    
)
;======================================SUM-OF========================================
(DOCUMENT-FUNCTION SUM-OF
(:SUMMARY "Sums all the numbers from a list of numbers")
(:SYNTAX (SUM-OF list))
(:PARAMETERS
(list :DOCSTRING "list of elements"))
(:EXAMPLES

"1. Summation of numbers :

(SUM-OF {1 2 3})

--> 6"

"2. Summation of arithmetical operations :

(SUM-OF {(+ (* 1 2) 3)(LENGTH \"BIOBIKE\")})

--> 12"
)
(:RETURNS "number")
(:TEXT
(:p " The list must contain numbers or functions that return numbers")
)
(:SEE-ALSO + ADD PRODUCT-OF))
  


