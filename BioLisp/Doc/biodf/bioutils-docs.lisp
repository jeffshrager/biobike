;;; -*- Package: bioutils; mode: lisp; base: 10; Syntax: Common-Lisp; -*-

(in-package :bioutils)

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

;;; Author:  JP Massar.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (import '(help:document-module help:document-function) :bioutils))

(document-module loop-utility
  "The XLOOP looping macro."
  (:keywords :loop :xloop :iterate :do :dotimes :dolist :repeat :map)
  (:display-modes :biolisp)
  #.`(:functions xloop))


(document-function xloop
  (:summary 
   #.(one-string-sp
      "An iteration facility similar to LISP:LOOP with some"
      "additional features and some restrictions."))
  (:returns "An arbitrary object, or NIL" :type t)
  (:examples
   ((xloop for j from 1 to 10 collect j) (1 2 3 4 5 6 7 8 9 10))
   ((xloop for j from 10 downto 1 collect j) (10 9 8 7 6 5 4 3 2 1))
   ((xloop for j from 1 to 10 by 3 collect j) (1 4 7 10))
   ((xloop for j in (iota 10) sum j) 45)
   ((xloop for j in (iota 10) max j) 9)
   ((xloop for j in (iota 10) min j) 0)
   ((xloop for j in (iota 10) when (oddp j) count j) 5)
   ((xloop for j in (iota 10) unless (zerop j) count j) 9)
   ((xloop for list on (iota 3) append list) (0 1 2 1 2 2)) 
   ((xloop for list on (iota 3) nconc list) (0 1 2 1 2 2))
   ((xloop init k = 1 while (< k 3) collect (prog1 (* k k) (incf k)))
    (1 4))
   ((xloop init k = 1 until (> k 3) collect (prog1 (* k k) (incf k)))
    (1 4 9))
   ((xloop for ch in "xyzzy" collect (char-code ch)) (120 121 122 122 121))
   ((xloop for elem in #(a b c) collect (symbol-name elem))
    ("A" "B" "C"))
   ((let ((h (create-hash-table '((a 5) (b 4) (c 2)))))
      (xloop for (key value) in h sum value)) 11)
   ((progn 
      (setf (slotv #$test.temp #$slot1) 'fred)
      (setf (slotv #$test.temp #$slot2) 23)
      (xloop for (slot-frame slot-value) in #$test.temp
             when (symbolp slot-value) 
             collect slot-value))
    (FRED))
   ((let ((x 10)) (xloop for j from 10 downto 0 do (decf x)) x) -1)
   ((let ((x 10))
      (xloop for j from 10 downto 0
             do (decf x)
             finally (return (list j x))
             )) 
    (0 -1))
   ((xloop for j from 1 to 5 as k = (- j) sum (+ j k)) 0)
   ((let ((x 10) (y 0))
      (xloop for j from 0 below 10 
             (incf x)
             (incf y)
             finally (return (+ x y))))
    30 
    "(The DO is implied)"
    )
   ((xloop for j from 1 to 5 
           for (first second) in '((a b) (c d) (e f))
           collect
           (list second first j))
    ((B A 1) (D C 2) (F E 3)))
   ((xloop for j = 32 then (/ j 2) until (< j 1) collect j)
    (32 16 8 4 2 1))
   )
  (:text
   (:p 
    #.(one-string-sp
       "XLOOP (aka LOOP in BBL) is an iteration facility similar to"
       "LISP:LOOP.  Its goals are to provide more straightforward syntax,"
       "better error messages, and in certain cases more generality (such as"
       "being able to iterate over vectors, hash tables, and frames"
       "using the same syntax as for lists.)"))
   (:p 
    #.(one-string-sp
       "For a complete description of XLOOP's syntax, see the documentation"
       "file .../Doc/loop-syntax.txt"))
   (:p 
    "Some of the major restrictions that XLOOP has that LISP:LOOP does not "
    "have are:"
    (:br)
    "  -- All INIT clauses must precede all other clauses."
    (:br)
    "  -- No INITIALLY clause."
    (:br)
    "  -- No IF-THEN-ELSE conditional, only WHEN and UNLESS."
    (:br)
    "  -- Either an aggregation clause or a set of DO statements, "
    "but not both."       
    (:br)
    " -- No iteration over the symbols of packages."
    (:br)
    " -- No INTO aggregation facility."
    )
   (:p
    #.(one-string-sp
       "It is possible to define iteration methods which then extend"
       "the capabilities of XLOOP.  Refer to the utility methods ITER-INIT,"
       "ITER-NEXT, and ITER-NEXT?")))
  (:see-also lisp:loop lisp:map lisp:mapcar lisp:maphash lisp:do lisp:dotimes
   lisp:dolist lisp:maplist lisp:mapcan lisp:do-symbols 
   frames:for-each-frame-slot
   ("http://www.biobike.org/loop-syntax.txt" "Formal syntax for Xloop")
   ("http://www.gigamonkeys.com/book/loop-for-black-belts.html" 
    "Lisp:loop chapter from 'Practical Common Lisp'") 
   )
  )
 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(document-module amino-acid-utils
  "Functions dealing with d/rna <--> amino acids"
  (:keywords :utilities :amino :amino-acids :dna :rna :translation)
  (:display-modes :biolisp)
  #.`(:functions ,@*amino-acid-conversion-api-symbols*))


(document-function dna-to-rna-sequence 
  (:summary "Converts DNA to RNA ('Tt' --> 'Uu')")
  (:returns  "Either a copy or the original" :type string)
  (:examples
   ((dna-to-rna-sequence "acgt") (:string "acgu"))
   ((dna-to-rna-sequence "ACGT") (:string "ACGU"))
   ((dna-to-rna-sequence "AGAC") (:string "AGAC")))
  (:text
   (:p 
    #.(one-string-sp
       "Translates a DNA sequence into its RNA counterpart."
       "The input must be a string.  If the keyword IN-PLACE? is non-NIL"
       "the original string is destructively modified, otherwise a"
       "modified copy of the string is returned.  Case is preserved."
       )))
  (:parameters
   (s :docstring "The DNA sequence to be translated." :value-type string)
   (in-place? :docstring "Whether to destructively modify the input."
              :value-type boolean
              :parameter-type &key
              :default-value nil
              ))
  (:see-also rna-to-dna-sequence ncomplement-base-pairs lisp:substitute)
  )

(document-function rna-to-dna-sequence 
  (:summary "Converts RNA to DNA ('Uu' --> 'Tt')")
  (:returns  "Either a copy or the original" :type string)
  (:examples
   ((dna-to-rna-sequence "acgu") (:string "acgt"))
   ((dna-to-rna-sequence "ACGU") (:string "ACGT"))
   ((dna-to-rna-sequence "AGAC") (:string "AGAC")))
  (:text
   (:p 
    #.(one-string-sp
       "Translates a RNA sequence into its DNA counterpart."
       "The input must be a string.  If the keyword IN-PLACE? is non-NIL"
       "the original string is destructively modified, otherwise a"
       "modified copy of the string is returned.  Case is preserved."
       )))
  (:parameters
   (s :docstring "The RNA sequence to be translated." :value-type string)
   (in-place? :docstring "Whether to destructively modify the input."
              :value-type boolean
              :parameter-type &key
              :default-value nil
              ))
  (:see-also dna-to-rna-sequence ncomplement-base-pairs lisp:substitute)
  )

(document-function amino-acid-designator?
  (:summary 
   "True if X is a symbol, string or char that denotes an amino acid")
  (:returns  "T or NIL" :type boolean)
  (:examples
   ((amino-acid-designator? "R") T)
   ((amino-acid-designator? "Z") NIL)
   ((amino-acid-designator? #\r) T)
   ((amino-acid-designator? #\Z) NIL)
   ((amino-acid-designator? :n) T)
   ((amino-acid-designator? :b) NIL)
   ((amino-acid-designator? :pro) T) 
   ((amino-acid-designator? :xxx) NIL)
   ((amino-acid-designator? "valine") T)
   ((amino-acid-designator? "cheeseburger") NIL)
   ((amino-acid-designator? "ValInE") T)
   ((amino-acid-designator? 'pro) T)
   )
  (:text
   (:p 
    #.(one-string-sp
       "Returns true if the input is any of a number of reasonable"
       "representations for an amino acid.  Reasonable representations"
       "include the one-letter symbol, the three-letter abbreviation,"
       "and the name (using a '-' to separate 'acid', eg, aspartic-acid)."
       )
    ))
  (:parameters
   (x :docstring "The alleged amino acid designator." 
      :value-type (or keyword string character)))
  (:see-also aa-to-1-letter-code aa-to-3-letter-code aa-to-codons 
   translate-d/rna-to-aa)
  )
   


(document-function aa-to-1-letter-code
  (:summary 
   #.(one-string-sp
   "Translates an amino acid designator into a 1-letter code symbolizing the"
   "amino acid."))
  (:returns "A 1-letter code." :type t)
  (:examples
   ((aa-to-1-letter-code :ala) (:string "A"))
   ((aa-to-1-letter-code :alanine :lcstring) (:string "a"))
   ((aa-to-1-letter-code #\A :keyword) :A)
   ((aa-to-1-letter-code "ALA" :char) #\A)
   ((aa-to-1-letter-code "XXX" :lc-char) :nil "Error condition")
   ((aa-to-1-letter-code 'pro :symbol) bioutils::p)
   )
  (:text
   (:p 
    #.(one-string-sp
       "Takes a valid amino acid designator and returns a 1-character"
       "symbol, string, or character which is the standard 1-letter"
       "abbreviation for the amino acid.  The result can be returned"
       "either as an uppercase string (:string, the"
       "default), a lowercase string (:lcstring), an uppercase character"
       "(:char), a lowercase character (:lcchar),"
       "a symbol in the bioutils package (:symbol), or a keyword"
       "symbol (:keyword).   If the designator is not valid, an error is"
       "signalled."
       )
    ))
  (:parameters
   (aa :docstring "The amino acid designator." 
       :value-type (or keyword string character))
   (as :docstring "What form to return the result as."
       :value-type keyword 
       :parameter-type &optional
       :default-value :string))
  (:see-also aa-to-long-name aa-to-3-letter-code aa-to-codons)
  )

(document-function aa-to-3-letter-code
  (:summary 
   #.(one-string-sp
   "Translates an amino acid designator into a 3-letter code symbolizing the"
   "amino acid."))
  (:returns "A 3-letter code." :type t)
  (:examples
   ((aa-to-3-letter-code :ala) (:string "ALA"))
   ((aa-to-3-letter-code :alanine :lcstring) (:string "ala"))
   ((aa-to-3-letter-code #\A :keyword) :ALA)
   ((aa-to-3-letter-code "Alanine" :capstring) (:string "Ala"))
   ((aa-to-3-letter-code "XXX" :symbol) :nil "Error condition")
   ((aa-to-3-letter-code 'p :symbol) bioutils::pro)
   )
  (:text
   (:p 
    #.(one-string-sp
       "Takes a valid amino acid designator and returns a 3-character"
       "symbol or string which is the standard 3-letter"
       "abbreviation for the amino acid.  The result can be returned"
       "either as an uppercase string (:string, the"
       "default), a lowercase string (:lcstring), a capitalized string"
       "(:capstring), a symbol in the bioutils package (:symbol), or a keyword"
       "symbol (:keyword).  If the designator is not valid, an error is"
       "signalled.")
    ))
  (:parameters
   (aa :docstring "The amino acid designator." 
       :value-type (or keyword string character))
   (as :docstring "What form to return the result as."
       :value-type keyword 
       :parameter-type &optional
       :default-value :string))
  (:see-also aa-to-long-name aa-to-1-letter-code aa-to-codons)
  )
   
(document-function aa-to-codons
  (:summary 
   #.(one-string-sp
      "Translates an amino acid designator into the codon (the 3-letter"
      "sequence of ACGT (or ACGU)) or codons that produce the amino acid."
      ))
  (:returns "A list of codons" :type t)
  (:examples
   ((aa-to-codons :ala) ("GCT" "GCC" "GCA" "GCG"))
   ((aa-to-codons :alanine :lcstring) ("gct" "gcc" "gca" "gcg"))
   ((aa-to-codons #\A :keyword) (:GCT :GCC :GCA :GCG))
   ((aa-to-codons "trp" :capstring) ("Tgg"))
   ((aa-to-codons "XXX" :symbol) :nil "Error condition")
   ((aa-to-codons 'p :rna-symbol) 
    (bioutils::ccu bioutils::ccc bioutils::cca bioutils::ccg))
   ((aa-to-codons 'p :rna-keyword) (:CCU :CCC :CCA :CCG))
   )
  (:text
   (:p 
    #.(one-string-sp
       "Translates an amino acid designator into the codon (the 3-letter"
       "sequence of ACGT (or ACGU)) or codons that produce the amino acid."
       "The result is always a list of codons."
       "The result can be returned either as uppercase strings (:string, the"
       "default), lowercase strings (:lcstring), capitalized strings"
       "(:capstring), symbols in the bioutils package (:symbol), or keyword"
       "symbols (:keyword).  These codons all use ACGT (DNA).  To return"
       "RNA codons (ACGU) prefix the return keywords with rna-, eg"
       ":rna-lcstring.  If the designator is not valid, an error is signalled."
       )
    ))
  (:parameters
   (aa :docstring "The amino acid designator." 
       :value-type (or keyword string character))
   (as :docstring "What form to return the result as."
       :value-type keyword 
       :parameter-type &optional
       :default-value :string))
  (:see-also codon-to-aa aa-to-long-name
   aa-to-1-letter-code aa-to-3-letter-code)
  )


(document-function aa-to-molecular-weight
  (:summary 
   "Calculates the molecular weight of an amino acid.")
  (:returns "The molecular weight in a format specified by the user." :type t)
  (:examples
   ((aa-to-molecular-weight :ala :integer) 89)
   ((aa-to-molecular-weight :pro :single-float) 115.131)
   ((aa-to-molecular-weight :g :double-float) 75.0669d0)   
   )
  (:text
   (:p 
    #.(one-string-sp
       "Takes an amino acid designator and returns the molecular weight"
       "of the amino acid.  The molecular weight can be returned as an integer"
       "(:integer), a floating point number (:single-float or :double-float)."
       )
    ))
  (:parameters
   (aa :docstring "The amino acid designator." 
       :value-type (or keyword string character))
   (as :docstring "What form to return the result as."
       :value-type keyword 
       :parameter-type &optional
       :default-value :integer))
  (:see-also molecular-weight-of amino-acid-designator?)
  )



(document-function aa-to-long-name
  (:summary 
   "Translates an amino acid designator into the full name of the amino acid.")
  (:returns "The amino acid's full name, or NIL." :type t)
  (:examples
   ((aa-to-long-name "A") (:string "ALANINE"))
   ((aa-to-long-name :a :lcstring) (:string "alanine"))
   ((aa-to-long-name #\A :keyword) :ALANINE)
   ((aa-to-long-name "ALA" :capstring) (:string "Alanine"))
   ((aa-to-long-name "XXX" :capstring) :nil "Error condition")
   ((aa-to-long-name 'p :symbol) bioutils::proline)
   )
  (:text
   (:p 
    #.(one-string-sp
       "Takes a valid amino acid designator and returns the full name"
       "of the amino acid either as an uppercase string (: string, the"
       "default), a lowercase string (:lcstring), a capitalized string"
       "(:capstring), a symbol in the bioutils package (:symbol), or a keyword"
       "symbol (:keyword).  If the designator is not valid, an error is"
       "signalled."
       )
    ))
  (:parameters
   (aa :docstring "The amino acid designator." 
       :value-type (or keyword string character))
   (as :docstring "What form to return the result as."
       :value-type keyword 
       :parameter-type &optional
       :default-value :string))
  (:see-also aa-to-1-letter-code aa-to-3-letter-code aa-to-codons)
  )

(document-function codon-to-aa
  (:summary 
   #.(one-string-sp
      "Takes a codon (a 3-element DNA sequence) and returns the amino acid"
      "it codes to."
      ))
  (:returns "The amino acid, in a form the user requests." :type t)
  (:examples
   ((codon-to-aa "acg") "T")
   ((codon-to-aa "ACG" :char) #\T)
   ((codon-to-aa "GGG" :keyword) :G)
   ((codon-to-aa "TAA") "*")
   ((codon-to-aa "TAA" :symbol) *)
   ((codon-to-aa "XYZ") "-")
   ((codon-to-aa "cgt" :lcstring) "r")
   ((codon-to-aa "cgt" :lcchar) #\r)
   )
  (:text
   (:p 
    #.(one-string-sp
       "Takes a codon (a 3-element DNA sequence) and returns the amino acid"
       "it codes to.  The codon may be a 3-character string or a symbol with"
       "3 characters in its name.  If the codon is a stop codon '*' is"
       "returned.  If the string is not a codon '-' is returned."
       )
    )
   (:p 
    #.(one-string-sp
       "The user can specify the form of the return value: as a"
       "single element uppercase string (:string, the default), as a"
       "single element lowercase string (:lcstring), as a"
       "single uppercase character (:char), as a single lowercase character"
       "(:lcchar), as a keyword (:keyword) or as a symbol (:symbol).")))
  (:parameters
   (codon :docstring "A symbol or a string with 3 characters"
          :value-type (or symbol string))
   (as :docstring "What form to return the result as."
       :value-type keyword 
       :parameter-type &optional
       :default-value :string))
  (:see-also aa-to-codons aa-to-1-letter-code aa-to-3-letter-code)
  )

(document-function molecular-weight-of
  (:summary "Calculates the molecular weight of a sequence of amino acids.")
  (:returns "The molecular weight" :type single-float)
  (:examples
   ((molecular-weight-of "AGRP") 399.4931)
   ((molecular-weight-of "PET") 345.38058)
   ((molecular-weight-of "zzzzzzzzzzz") :nil "Error condition")
   ((molecular-weight-of "arndceqghilkmfpstwyv*") 2378.0166)
   )
  (:text
   (:p 
    #.(one-string-sp
       "Takes a sequence of amino acids as a string and calculates the"
       "molecular weight of that collection.  If any character is not a valid"
       "amino acid single character designator, an error is signalled.")))
  (:parameters
   (aa-sequence :docstring "The sequence of amino acids"
                :value-type string))
  (:see-also aa-to-molecular-weight amino-acid-designator? 
   ))

(document-function translate-d/rna-to-aa 
  (:summary "Translates a DNA or RNA sequence to an amino acid sequence.")
  (:returns "A string of amino acids and separators" :type string)
  (:examples
   ((translate-d/rna-to-aa "acgtacgtacgt") (:string "TYVR"))
   ((translate-d/rna-to-aa "acgtacgtacgt" :as :3codes)
    (:string "THR TYR VAL ARG"))
   ((translate-d/rna-to-aa "acguacguacgt" :as :3codes :separator " : ")
   (:string  "THR : TYR : VAL : ARG"))
   ((translate-d/rna-to-aa "acgt") :nil "Error condition")
   ((translate-d/rna-to-aa "acgt" :if-partial-codon :ignore) (:string "T"))
   ((translate-d/rna-to-aa "acgtax") :nil "Error condition")
   ((translate-d/rna-to-aa "acgtax" :if-unknown-codon "*") (:string "T*"))
   ((translate-d/rna-to-aa "acgucc") (:string "TS"))
   ((translate-d/rna-to-aa "acgucc" :insist-on :dna) :nil "Error condition"))
  (:text
   (:p 
    #.(one-string-sp
       "Translates a DNA or RNA sequence to an amino acid sequence."
       "Returns a string of amino acids, either as 1-letter codes "
       "(:as :1codes), the default, or as 3-letter codes (:as :3codes). "
       "Each amino acid is separated by nothing (default for :1codes) or a"
       "single space (default for :3codes) or the provided separator string"
       "SEPARATOR."))
   (:p
    #.(one-string-sp
       "If the sequence isn't a multiple of 3 characters long, IF-PARTIAL-CODON"
       "specifies whether to signal an error (:error), a warning (:warn)"
       "or ignore the excess terminating characters (:ignore)."
       "IF-UNKNOWN-CODON specifies what to do if an unknown codon is detected"
       "in the sequence.  Possibilities are to signal an error (:error), a"
       "warning (:warn), or, if IF-UNKNOWN-CODON is a character or string, to"
       "use that value as the translation, or if it is anything else to use"
       "the default (#\- or '---') as the translation."))
   (:p
    #.(one-string-sp   
       "If INSIST-ON is :DNA or :RNA, each codon is checked to insure that it"
       "doesn't contain 'U' or 'T' respectively. The default, NIL, allows both."
       )))
  (:parameters
   (d/rna-sequence :docstring "The sequence of DNA/RNA."
                   :value-type string)
   (as :docstring "What form to return the result as."
       :parameter-type &key :default-value :1codes)
   (separator
    :docstring 
    "What, if anything, each amino acid is separated by in the returned string."
    :parameter-type &key :default-value "")
   (if-partial-codon
    :docstring 
    "What to do if the DNA/RNA sequence is not a multiple of 3 base pairs long."
    :parameter-type &key :default-value :error)
   (if-unknown-codon
    :docstring 
    "What to do if the DNA/RNA sequence contains an illegal base pair."
    :parameter-type &key :default-value :error)
   (insist-on
    :docstring 
    "Require the syntax to only DNA or only RNA base pairs."
    :parameter-type &key :default-value nil))
  (:see-also translate-to-aa ncomplement-base-pairs translate-string
   lisp:replace))

(document-function translate-to-aa
  (:summary "Translates a DNA or RNA sequence to an amino acid sequence.")
  (:text
   (:p 
    #.(one-string-sp
       "This is an alias for TRANSLATE-D/RNA-TO-AA.  See that documentation"
       "for details.")))
  (:see-also translate-d/rna-to-aa))   


(document-function ncomplement-base-pairs
  (:summary "Complements (in the DNA sense) a simple string in place.")
  (:returns "The input string destructively modified via DNA complementing."
   :type string)
  (:examples
   ((ncomplement-base-pairs "acgtt") (:string "aacgt"))
   ((ncomplement-base-pairs "aCGTTg") (:string "cAACGt"))
   ((ncomplement-base-pairs "achugt") (:string "achugt"))
   ((ncomplement-base-pairs "achugt" :reverse? nil) (:string "tghuca"))
   )
  (:text
   (:p
    #.(one-string-sp
       "Complements (in the DNA sense) a simple string in place."
       ))
   (:p
    #.(one-string-sp
       "By default, the complemented string's characters are returned"
       "in reverse order.  If REVERSE? is NIL (default T), the complemented"
       "string's characters are returned in the original order."
       ))
   (:p
    #.(one-string-sp
       "Characters which are not ACGTacgt are not complemented. However if"
       "EXTENDED-ALPHABET? is T, then an extended alphabet is permitted,"
       "which is complemented as follows:"
       ))
   (:pre
    #.(one-string-nl
       "  N -> N  (Any base pair)"
       "  R -> Y  (A or G -> C or T)"
       "  Y -> R  (C or T -> A or G)"
       "  W -> W  (A or T -> A or T)"
       "  S -> S  (G or C -> G or C)"
       "  M -> K  (A or C -> G or T)"
       "  K -> M  (G or C -> A or T)"
       "  B -> V  (C, G, or T -> A, C, or G)"
       "  V -> B (A, C, or G -> C, G, or T)"
       "  D -> H (A, G, or T -> A, C, or T)"
       "  H -> D (A, C, or T -> A, G, or T)"
       "(Lowercase also permitted)"
       ))
   (:p
    #.(one-string-sp
       "IF-INVALID-CHAR? determines the behavior if an invalid character"
       "is detected.  The default is NIL which means leave the character in"
       "place.  The other options are: :error, which causes an error to be"
       "signaled, and :warn, which causes a warning to be issued."
       ))
   (:p
    #.(one-string-sp
       "Note: The operation is destructive, the string is modified in place."
       "Use COPY-SEQ to copy the string before calling this function"
       "if this is not desirable."
       ))
   )
  (:parameters
   (string :docstring "The DNA sequence to be complemented."
           :value-type simple-string)
   (reverse? 
    :docstring "Whether the returned string's characters are in reverse order"
    :parameter-type &key :default-value t)
   (extended-alphabet?
    :docstring 
    "Whether to complement an extended alphabet (characters representing AAs)"
    :parameter-type &key :default-value nil)
   (if-invalid-char?
    :docstring 
    "What to do if the function doesn't know how to complement a character"
    :parameter-type &key :default-value nil)
   )
  (:see-also translate-d/rna-to-aa translate-string lisp:substitute))

