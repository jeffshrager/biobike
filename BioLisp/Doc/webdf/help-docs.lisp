;;; -*- Package: help; mode: lisp; base: 10; Syntax: Common-Lisp; -*-

(in-package :help)

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

(document-module help
  "Help and documentation."
  (:keywords :help)
  (:functions
   help
   ;; function-documentation
   ;; parameter-documentation
   ;; extract-text
   apropos+))


(document-function help
  (:summary "Describes how to get help or finds relevant information.")
  (:syntax (help term1 term2... &key various))
  (:returns "Executed for side effect.  Displays links to relevant information."
   :type t :display-type nil)
  (:examples
   ((help) :nil "Displays information on how to get more help.")
   ((help string) :nil "Displays links relevant to or matching 'string'")
   ((help string :scope :all) 
    "Displays all links relevant to or matching 'string'")
   ((help running programs) :nil 
    "Displays links relevant to 'running' and 'programs'.")
   ((help help) :nil "Displays a link to this documentation.")
   )
  (:text
   (:p 
    #.(one-string-sp
       "HELP searches for documented objects in the BioBike system which it"
       "deems relevant to the user's search query.  The search query can"
       "consist of a single word or multiple words.  Currently, the multiple"
       "word search is implemented as a set of single-word searches, rather"
       "than as a Google-like indexed search over the text of all the"
       "documentation."
       ))
   (:p 
    #.(one-string-sp
       "If HELP is given no search term, it prints out information on how to"
       "obtain additional help."))
   (:p 
    #.(one-string-sp
       "If HELP is given the search term HELP it prints out a short discussion"
       "discussion, a few examples, and provides a link to this documentation."
       ))
   (:p 
    #.(one-string-sp
       "When given a single search term, HELP searches its entire database"
       "of documented objects (symbols, tutorials, glossary entries, files"
       "topics, and modules).  Each such documented object has a name, and"
       "HELP matches the search term against each name.  A match can either"
       "be 1) exact, 2) close, or 3) partial."
       ))
   (:p 
    #.(one-string-sp
       "A close match is a name that is similar to the search term:"
       "e.g., STRINGS and STRINE are close to STRING.  A partial match"
       "is a name that contains the search term: e.g., MULTIPLE-VALUE-BIND"
       "is a partial match for VALUE."
       ))
   (:p 
    #.(one-string-sp
       "HELP also matches keywords associated with a documented object against"
       "the search term: e.g., if the symbol ONE-STRING was documented so that"
       "one of its keywords was CONCATENATE, then searching for CONCATENATE"
       "would match ONE-STRING (in fact, searching for CATENATE would also"
       "match albeit to a lesser degree)."
       ))
   (:p 
    #.(one-string-sp
       "HELP displays a small number of the most relevant hits, and provides"
       "a link to display the entire suite of matches."
       ))
   (:p 
    #.(one-string-sp
       "HELP only displays entries for symbols that are currently accessible"
       "to the user.  E.g., if the user is in BBL mode, only symbols in the"
       "BBL package and the user's own package are displayed.  If you wish to"
       "see every match, use the :SCOPE :ALL option as shown in the example."
       ))
   (:p 
    #.(one-string-sp
       "If HELP is given a single string of the form \"package:name\", e.g."
       "\"utils:one-string\", then HELP will display information about"
       "that symbol and no other entries."
       ))
   )
  (:see-also lisp:apropos lisp:describe frames:search-frames)
  )


(document-function apropos+
  (:summary "Finds symbols closely matching (textually) the search string.")
  (:returns "Executed for side effect.  Displays links to relevant information."
   :type t :display-type nil)
  (:examples
   ((apropos+ "ortholog") :nil "Displays all reasonable matches.")
   ((apropos+ "ortholog" :limit 10) :nil "Displays up to 10 matches.")
   ((apropos+ "gene protein") :nil 
    "Searches documentation strings rather than symbol names for matches.")
   )
  (:text
   (:p 
    #.(one-string-sp
       "APROPOS+ is really two functions: one that searches symbol names"
       "for near matches based on edit distance and one that searches"
       "documentation strings for similar text based on word homology."
       ))
   (:p 
    #.(one-string-sp
       "When a symbol or a string is provided which has no embedded spaces"
       "the search is done over symbol names using edit distance and"
       "substring matching.  When"
       "embedded spaces exist in the search term the search is done over"
       "the documentation strings of symbols, not their names, and is done"
       "using word homology."
       ))
   (:p 
    #.(one-string-sp
       "The results may be limited by using the :LIMIT keyword.  If not"
       "specified, no more than 50 matches are displayed."
       ))
   (:p 
    #.(one-string-sp
       "The result links go to the system source for the matching function"
       "or variable; they do not go to any DOCUMENT-FUNCTION description."
       ))
   )
  (:parameters 
   (search-term
    :docstring "The term to be searched for."
    :value-type (or symbol string)
    )
   (package-or-packages 
    :docstring "The packages whose symbols will be searched."
    :parameter-type &key :default-value cl-user::*biobike-packages*
    :value-type list)
   (limit 
    :docstring "The maximum number of matches to be displayed."
    :parameter-type &key :default-value 50
    :value-type integer)   
   )
  (:see-also lisp:apropos lisp:describe help frames:search-frames)
  )


