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

;;; Authors:  JP Massar, Peter Seibel.

#| sample template
(DEF-GLOSSARY-ENTRY "name of entry"
  (:SUMMARY "brief definition")
  (:TEXT 
     (:P ...))
  (:KEYWORDS "Blast" "Similarity" "Sequence")
  (:SEE-ALSO 
   lisp:multiple-value-bind
   utils:one-string 
   utils:cl
   bio:*loaded-organisms*
   "http://www.google.com"
   "this is a random string."
   #$go.molecular_function
   ("http://www.google.com" "google link")
   (module "hash-utils")
   (topic "Contiguous Sequences")
   (glossary "operating system")
   ;; (tutorial "orthologs") 
   (df utils:s+) 
   (tutorial "Functions")
   (df "utils:one-string-nl")
   (url #.(s+ wb::*doc-directory-url* "index.html") "main docs")
   (docfile "bbl-description.html" "BBL semantics")
   (:symbol "bio:two-way-ortholog-of")
   (:lisp "CAR")
   (:frame "go.molecular_function")
|#

(def-glossary-entry 
    "Anabaena PCC 7120"
  (:summary "A cyanobacterial organism")
  (:text
   (:p 
    #.(one-string-sp
       "One of a number of cyanobacterial organisms whose genomes are"
       "included in the current BioBike system."))) 
  (:keywords "organism" "cyanobacteria")
  (:see-also 
   (df "bio:available-organisms")
   (df "bio:loaded-organisms")
   ))

(def-glossary-entry 
    "operating system"
  (:summary "The program that runs your computer.")
  (:text (:p "More explanation."))
  (:keywords "Program" "Controller" "OS" "Windows" "Linux")
  (:see-also (glossary "KnowOS"))
  )

(def-glossary-entry 
    "KnowOS"
  (:summary "The underlying code that runs the Weblistener and BioBike.")
  (:text (:p "More explanation."))
  (:see-also (glossary "operating system"))
  )

(def-glossary-entry 
    "VPL"
  (:summary "Visual Programming Language -- Sometimes used to refer to BioBIKE's graphical interface.")
  ; (:text (:p ""))
  (:see-also (glossary "BBL"))
  )
  
(def-glossary-entry 
    "BBL"
  (:summary "BioBIKE Language -- The language used by the weblistener and constructed by the graphical interface.")
  (:text (:p "BioBIKE Language (BBL) is a computer programming language that incorporates the vocabulary and functionality of molecular biology and genome analysis. It is a derivative of BioLisp, which itself is a derivative of Lisp.")
(:P "Forms made within the graphical environment (VPL) are translated into BBL and then executed. BBL is used directly in the BioBIKE weblistener."))
  (:see-also (glossary "VPL") (glossary "BioLisp"))
  )
  
(def-glossary-entry 
    "BioLisp"
  (:summary "An extension of the programming language Lisp designed to facilitate genome analysis.")
  (:text (:p "Once known as 'BioLingua'."))
  (:see-also ("http://bioinformatics.oxfordjournals.org/cgi/reprint/21/2/199"
      "Massar et al (2005)"))
  )
  
(def-glossary-entry 
    "Bracket notation"
  (:summary "Using X[y] instead of (aref x y).  Used by the BBL language.")
  (:text (:p "More explanation."))
  (:see-also (docfile "bbl-description.html"))
  )

  (def-glossary-entry 
    "mapping"
  (:summary "Application of a function to a list of values.")
  #|
  (:text 
   ; (:table :align "middle" :width "80%"
    ; (:tr (:td
   (:p
    #.(one-string-sp
       "Two genes or proteins are in an orthologous relationship if:"))
   (:UL (:LI "They are in different organisms")
        (:LI "They share a common ancestral gene")
        (:LI #.(ONE-STRING-SP 
                "The phylogenetic tree encompassing the two is superimposable"
                "on the phylogenetic tree encompassing their organisms")))
   (:P #.(ONE-STRING-SP
         "Genes within a single organism that arose from the same ancestral"
         "gene are termed paralogs. The divergence of paralogs can lead to"
         "paralogs appearing in"
         "different organisms.")) 
) |#
  (:keywords "APPLY-FUNCTION" "MAPTREE")
  (:see-also 
   (df "bbi:APPLY-FUNCTION")
   ))
   


(def-glossary-entry 
    "Trichodesmium erythraeum"
  (:summary "A cyanobacterial organism")
  (:text
   (:p 
    #.(one-string-sp
       "One of a number of cyanobacterial organisms whose genomes are"
       "included in the current BioBike system."))) 
  (:keywords "organism" "cyanobacteria")
  (:see-also 
   (df "bio:available-organisms")
   (df "bio:loaded-organisms")
   ))



