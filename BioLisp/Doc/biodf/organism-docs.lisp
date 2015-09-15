;;; -*- Package: bio; mode: lisp; base: 10; Syntax: Common-Lisp; -*-

(in-package :bio)

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

;;; Author:  JP Massar.  Mark Slupesky.  


(help:document-module organism-ops
  "Functions for manipulating organisms and their components."
  (:keywords genes contig bio sequences)
  (:display-modes :biolisp)
  (:functions 
   extract-gene-sequence
   extract-contig-sequence
   extract-protein-sequence
   available-organisms
   preload-organisms
   load-organisms
   load-organism
   purge-organism
   add-organism-alias
   organism
   pprint-organism
   loaded-organisms))

(help:document-function extract-gene-sequence
  (:summary 
   "Extract a base pairs representing a given GENE from its CONTIG.")

  (:syntax (extract-gene-sequence
            gene
            &key
            (cache? nil)
            (start-offset 0)
            (end-offset 0)
            (complement-backwards? t)
            (if-no-sequence? :null-string)
            (if-wrapped-but-not-circular :error)
            (ignore-architecture? nil)
            (safely? t)))

  (:returns "The extracted sequence" :type string)

  (:examples
   "(extract-gene-sequence (first (#^genes npun)))"
   "(extract-gene-sequence (second (#^genes a7120)) :start-offset -10 :end-offset 20)")

  (:text 
   (:p "The sequence extracted is defined by the gene's DIRECTION and
   either its FROM and TO properties, or its ARCHITECTURE
   property.")

   (:p "The sequence is returned as a concatentation of one or more
   subsequences of the CONTIG's sequence (one, if the gene has no
   architecture, possibly many if the gene has an ARCHITECTURE).")

   (:p "If the gene has ARCHITECTURE it must be a list of (A-FROM
     A-TO) pairs.  The first (A-FROM A-TO) pair must have A-FROM =
   FROM, and the last pair must have A-TO = TO.  Each pair
   represents a subsequence between FROM and TO, and A-FROM and
   A-TO are inclusive just as FROM and TO are.")

   (:p "Extended regions around the gene may be accessed using
   START-OFFSET and/or END-OFFSET."))

  (:parameters
   (gene :docstring "The gene whose sequence should be extracted." 
         :value-type gene-frame)

   (complement-backwards? :docstring "When true, and when the gene's
   DIRECTION is B (backwards), the extracted sequence is reversed
   and each base is complemented."
    :value-type boolean :parameter-type &key :default-value t)

   (ignore-architecture? 
    :docstring "When true, the GENE's ARCHITECTURE
   property is ignored, and the sequence is defined solely by the
   FROM and TO properties (possibly as modified by START-OFFSET
   and END-OFFSET)." 
    :value-type boolean :parameter-type &key :default-value nil)

   (start-offset 
    :docstring "Value added to FROM when reading forward or
   substracted from TO when reading backward. If supplied,
   implies IGNORE-ARCHITECTURE? is true." 
    :value-type integer :parameter-type &key :default-value 0)

   (end-offset 
    :docstring "Value added to TO when reading forward or
   substracted from FROM when reading backward.  If supplied,
   implies IGNORE-ARCHITECTURE? is true." 
    :value-type integer :parameter-type &key :default-value 0)

   (cache? :docstring "If true, the sequence is retrieved from the gene's
   #$Sequence slot if there, or stored there once retrieved if
   not." :value-type boolean :parameter-type &key :default-value nil)
   
   (if-wrapped-but-not-circular :docstring "Determines what happens if FROM
   > TO and the CONTIG is not defined as CIRCULAR. Possible
   values are :ERROR (the default) which signals an error, :WARN
   which warns but extracts the sequence as if the CONTIG was
   defined as CIRCULAR and, NIL, which extracts the sequence as
   if the CONTIG was defined as CIRCULAR without issuing a
   warning." :value-type symbol :parameter-type &key :default-value :error)
   
   (if-no-sequence? :parameter-type &key :default-value :null-string )

   (safely? :parameter-type &key :default-value t :value-type boolean))

  (:see-also extract-contig-sequence extract-protein-sequence))


(help:document-function extract-contig-sequence 
  (:summary "Extract a subsequence of base pairs from a CONTIG.")

  (:returns "The extracted sequence" :type string)

  (:text)

  (:parameters
   (contig 
    :docstring
    "The contig from which the sequence should be extracted." :value-type frame)

   (from :docstring "The 1-based, inclusive index of the first base pair
   to be extracted." :value-type integer)

   (to :docstring "The 1-based, inclusive index of the last base pair to be
   extracted." :value-type integer)

   (direction :docstring "The direction to read the sequence, either F or B." :value-type string-designator)

   (complement-backwards?
    :docstring "When true, and when DIRECTION is
   B (backwards), the extracted sequence is reversed and each
   base is complemented."
    :value-type boolean :parameter-type &key :default-value t)
   
   (if-wrapped-but-not-circular
    :docstring "Determines what happens if FROM
   > TO and the CONTIG is not defined as CIRCULAR. Possible
   values are :ERROR (the default) which signals an error, :WARN
   which warns but extracts the sequence as if the CONTIG was
   defined as CIRCULAR and, NIL, which extracts the sequence as
   if the CONTIG was defined as CIRCULAR without issuing a
   warning." 
    :value-type symbol :parameter-type &key :default-value :error)

   (safely? :parameter-type &key :default-value t :value-type boolean))

  (:see-also extract-gene-sequence extract-protein-sequence))

(help:document-function extract-protein-sequence
  
  (:summary "Extract a sequence of base pairs that encode for a given PROTEIN.")
  
  (:returns "The extracted sequence" :type string)

  (:text)

  (:parameters 

   (protein)

   (strip-trailing-star? 
    :docstring "When true, any trailing '*' is removed
   unless CACHE? is true and the sequence is returned from the cache." 
    :value-type boolean :default-value t)

   (cache? :docstring "If true, the sequence is retrieved from the protein's
   #$Sequence slot if there, or stored there once retrieved if
   not." :value-type boolean))

  (:see-also extract-gene-sequence extract-contig-sequence))



(help:document-function 
    available-organisms
  (:summary 
   "Returns a list of organisms able to be loaded for analysis.")
  (:returns 
   "The set of organisms -- all frames, strings, or symbols -- available for analysis."
   :type list)
  (:examples
   #.(one-string-sp
      "(available-organisms) -->  (#$anabaena_variabilis_atcc29413 #$crocosphaera_watsonii_wh8501"
      "#$prochlorococcus_marinus_med4 #$thermosynechococcus_elongatus_bp1)")
   "(available-organisms :as :symbols) --> (GLOEOBACTER_VIOLACEUS_PCC7421 SYNECHOCOCCUS_WH8102)"
   #.(one-string-sp
      "(available-organisms :as :strings) --> (\"prochlorococcus_marinus_ss120\""
      "\"trichodesmium_erythraeum\")")
   )
  (:text
   #.(one-string-sp
      "Returns a list of organisms potentially available for analysis,"
      "as specified in /bioetc/data/"
      "If AS is :frames or :frame (the default) a list of frames is returned."
      "If AS is :strings or :string a list of strings is returned."
      "If AS is :symbols or :symbol symbols in the BIOLISP package are returned."
      "See *LOADED-ORGANISMS* for a list of organisms which are fully loaded."
      "To make organisms accessible see (LOAD-ORGANISMS ....)"
      ))
  (:parameters
   (as
    :docstring
    #.(one-string-sp
       "Controls how each organism is represented in the ouput list.  By default, they are represented"
       "as frames.  But :strings outputs them as strings and :symbols outputs them as symbols."
       )
    :parameter-type &key :default-value :frames :value-type keyword)
   (dir
    :docstring
    #.(one-string-sp
       "The directory where to find available-organisms.lisp."
       "Default value is /bioetc/data/"
       )
    :parameter-type &key :default-value nil :value-type (or string null))
   )
  (:see-also preload-organisms load-organisms organism loaded-organisms)
  )


(help:document-function 
    preload-organisms
  (:summary 
   "Loads auxiliary information for all available organisms.")
  (:returns 
   "The preloaded organism frames"
   :type list)
  (:examples
   "(preload-organisms) --> (#$anabaena_pcc7120 #$synechocystis_pcc6803)"
   "(preload-organisms :dir \"/home/foo/\") --> (#$anabaena_pcc7120 #$synechocystis_pcc6803)"
   "(preload-organisms :only #$anabaena_pcc7120) --> (#$anabaena_pcc7120)"
   "(preload-organisms :only #$badframe) --> ;; error")
  (:text
   (:p
    #.(one-string-sp
       "Loads auxiliary information for all the organisms available to"
       "this instance of Biolingua (via the AVAILABLE-ORGANISMS function),"
       "found in each organism's plist.lisp file"))
   (:p
    #.(one-string-sp
       "This function is run once automatically when the system loads."
       "Preloading does NOT load the genome, gene, and protein information."))
   (:p
    #.(one-string-sp
       "A frame is created for each organism, along with exported symbols bound to"
       "that frame whose names are the nicknames of the organism."
       "Finally a list of the preloaded organisms is returned."))
   (:p
    #.(one-string-sp
       "To preload only certain organisms, use the ONLY keyword.  It takes"
       "a list of organism frames."
       )))
  (:parameters
   (dir
    :docstring
    #.(one-string-sp
       "The directory where to find available-organisms.lisp."
       "It default to looking in the organism's toplevel directory." 
       )
    :parameter-type &key :default-value nil :value-type (or string null))
   (only
    :docstring
    #.(one-string-sp
       "Must either be NIL, which is the default value,"
       "or a list of organism frames, where each organism is also in"
       "available-organisms.lisp")
    :parameter-type &key :default-value nil :value-type list))
  (:see-also  available-organisms load-organisms organism)
  )


(help:document-function 
    load-organisms
  (:summary 
   #.(one-string-nl
      "Creates genome, protein, transcript, and gene frames for"
      "all available organisms."
      ))
  (:returns 
   "The frame of the last organism loaded, NIL if none."
   :type (or frame  nil))  
  (:examples
   #.(one-string-sp
      "(available-organisms) --> (#$prochlorococcus_marinus_mit9313 #$anabaena_pcc7120 #$synechocystis_pcc6803)"
      "(load-organisms :only '(\"ss120\" \"a7120\")) --> #$anabaena_pcc7120"
      ))
  (:text
   (:p
    #.(one-string-sp
       "Calls LOAD-ORGANISM on each function specified in available-organisms.lisp,"
       "or a select few by using the DIR keyword with a list of organism designators.")))

  (:parameters
   (organisms :docstring "The organisms to be loaded."
              :parameter-type &key :default-value (available-organisms)
              :value-type list)
   (verbose? 
    :docstring 
    #.(one-string-sp
       "When NIL, few updates are provided on the progress of the load;"
       "non-NIL (the default) for lots of updates.")
    :parameter-type &key :default-value t :value-type boolean)
   (reload?
    :docstring
    #.(one-string-sp
       "When NIL (the default), only information which has not"
       "already been loaded will be loaded.  If non-NIL, all"
       "specified components of the organism will be reloaded.")
    :parameter-type &key :default-value nil :value-type boolean)
   (rebuild?
    :docstring
    #.(one-string-sp
       "When NIL (the default), the sequences built from the fasta files"'
      "are only regenerated if they don't exist or are out of date."
       "When T, they generated regardless.")
    :parameter-type &key :default-value nil :value-type boolean)
   (postload?
    :docstring
    #.(one-string-sp
       "An organism can have a postload file in its toplevel data directory."
       "When POSTLOAD? is T (the default), this file is checked for"
       "and, if found, is loaded (using LISP:LOAD).")
    :parameter-type &key :default-value t :value-type boolean)
   (pprint
    :docstring
    "When T, gives a pretty print summary of the organism's information."
    :parameter-type &key :default-value nil :value-type boolean)
   (pprint-limit
    :docstring
    #.(one-string-sp
       "Limits how many of each contig, gene, transcript, and protein"
       "are printed, with a default value of 10.")
    :parameter-type &key :default-value 10 :value-type integer)
   (test?
    :docstring
    #.(one-string-sp
       "An organism can have a test file in its toplevel data directory."
       "When TEST? is T (the default), a simple test defined by this file is run.")
    :parameter-type &key :default-value T :value-type boolean)
   (test-all-genes?
    :docstring
    #.(one-string-sp
       "Only matters when the TEST? keyword is T."
       "Given this and that TEST-ALL-GENES? is T, every gene's sequence"
       "is extracted as a more robust test of the organism.")
    :parameter-type &key :default-value T :value-type boolean)
   (descriptions?
    :docstring
    #.(one-string-sp
       "When T, loads any included additional textual information"
       "about the  organism.")
    :parameter-type &key :default-value nil :value-type boolean)
   (private?
    :docstring
    #.(one-string-sp
       "If PRIVATE? is T (the default is NIL), you must also pass the DIR keyword argument"  
       "the pathname of the directory where the organism's data is stored."
       "The organism is then loaded from this directory, creating all frames"
       "as normal, but is NOT added to the system's list of organisms,"
       "*loaded-organisms*, thus sparing it from any actions aimed to effect"
       "all organisms.  You can still easily reference all the frame's information"
       "by typing in the frame name.")
    :parameter-type &key :default-value nil :value-type boolean)
   (dir
    :docstring
    #.(one-string-sp
       "Only matters when PRIVATE? is T.  DIR must be the directory"
       "in which this organism's data is kept.") 
    :parameter-type &key :default-value nil :value-type (or string pathname)))
  (:see-also  available-organisms load-organisms organism)
  )

(help:document-function 
    load-organism
  (:summary 
   "Creates genome, gene, transcript and protein frames for the organism.")
  (:returns 
   "The organism's frame"
   :type frame)
  (:examples
   "(load-organism a7120) --> #$anabaena_pcc7120")
  (:text
   (:p
    #.(one-string-sp
       "Creates, at the very least, frames for the organism's contiguous sequences."
       "If gene, transcript and protein subdirectories exist, those frames"
       "are created as well.  It is safe to call this function multiple times;"
       "nothing will happen if the organism is already loaded unless :reload?"
       "or :rebuild? are T.")))
  (:parameters
   (org :docstring "An organism designator." :value-type (or string organism))
   (verbose? 
    :docstring 
    #.(one-string-sp
       "When NIL, few updates are provided on the progress of the load;"
       "non-NIL (the default) for lots of updates.")
    :parameter-type &key :default-value t :value-type boolean)
   (reload?
    :docstring
    #.(one-string-sp
       "When NIL (the default), only information which has not"
       "already been loaded will be loaded.  If non-NIL, all"
       "specified components of the organism will be reloaded.")
    :parameter-type &key :default-value nil :value-type boolean)
   (rebuild?
    :docstring
    #.(one-string-sp
       "When NIL (the default), the sequences built from the fasta files"'
      "are only regenerated if they don't exist or are out of date."
       "When T, they generated regardless.")
    :parameter-type &key :default-value nil :value-type boolean)
   (postload?
    :docstring
    #.(one-string-sp
       "An organism can have a postload file in its toplevel data directory."
       "When POSTLOAD? is T (the default), this file is checked for"
       "and, if found, is loaded (using LISP:LOAD).")
    :parameter-type &key :default-value t :value-type boolean)
   (pprint
    :docstring
    "When T, gives a pretty print summary of the organism's information."
    :parameter-type &key :default-value nil :value-type boolean)
   (pprint-limit
    :docstring
    #.(one-string-sp
       "Limits how many of each contig, gene, transcript, and protein"
       "are printed, with a default value of 10.")
    :parameter-type &key :default-value 10 :value-type integer)
   (test?
    :docstring 
    #.(one-string-sp
       "An organism can have a test file in its toplevel data directory."
       "When TEST? is T (the default), a simple test defined by this file is run.")
    :parameter-type &key :default-value T :value-type boolean)
   (test-all-genes?
    :docstring 
    #.(one-string-sp
       "Only matters when the TEST? keyword is T."
       "Given this and that TEST-ALL-GENES? is T, every gene's sequence"
       "is extracted as a more robust test of the organism.")
    :parameter-type &key :default-value T :value-type boolean)
   (descriptions?
    :docstring 
    #.(one-string-sp
       "When T, loads any included additional textual information"
       "about the  organism.")
    :parameter-type &key :default-value nil :value-type boolean)
   (private?
    :docstring 
    #.(one-string-sp
       "If PRIVATE? is T (the default is NIL), you must also pass the DIR keyword argument"  
       "the pathname of the directory where the organism's data is stored."
       "The organism is then loaded from this directory, creating all frames"
       "as normal, but is NOT added to the system's list of organisms,"
       "*loaded-organisms*, thus sparing it from any actions aimed to effect"
       "all organisms.  You can still easily reference all the frame's information"
       "by typing in the frame name.")
    :parameter-type &key :default-value nil :value-type boolean)
   (dir
    :docstring 
    #.(one-string-sp
       "Only matters when PRIVATE? is T.  DIR must be the directory"
       "in which this organism's data is kept.") 
    :parameter-type &key :default-value nil :value-type string)
   )
   (:see-also load-organisms available-organisms organism)
   )

(help:document-function 
    purge-organism
  (:summary 
   #.(one-string-sp
      "Removes gene, protein, and contiguous sequence data from an organism,"
      "optionally removing the organism itself from biobike."
      ))
  (:returns "The organism's frame or T" :type (or frame t))
  (:examples
   "(purge-organism a7120) --> #$anabaena_pcc7120)"
   "(purge-organism <my-private-organism> :delete-frame? t) --> T"
   )
  (:text
   (:p
    #.(one-string-sp
       "Removes the gene, protein, and contiguous sequence frames from an"
       "organism, and optionally removes the organism itself from biobike"
       "if the DELETE-FRAME? keyword is enabled."
       ))
   (:p
    #.(one-string-sp
       "If the organism is a public organism, the organism is removed"
       "from the set of loaded organisms.  In any case the organism"
       "is designated as not loaded"
       "(The #$organism-loaded? slot is set to nil)."
       "Note: It is not generally considered acceptable to purge"
       "non-private organisms unless you are a system administrator!"))
   (:p
    #.(one-string-sp
       "If the biobike system is running under acache the organism data is"
       "removed from the acache database unless the COMMIT? keyword"
       "is disabled (the default is to enable it).  If the DELETE-FRAME?"
       "keyword is enabled, the organism itself is also removed from acache."
       )))
  (:parameters
   (organism :docstring "An organism frame." :value-type organism)
   (verbose? 
    :docstring 
    #.(one-string-sp
       "When NIL, few updates are provided on the progress of the purge;"
       "non-NIL (the default) for lots of updates.")
    :parameter-type &key :default-value t :value-type boolean)
   (commit?
    :docstring
    #.(one-string-sp
       "Only useful when using acache.  Saves the changes to the acache"
       "database if enabled (the default)"
       )
    :parameter-type &key :default-value t :value-type boolean)
   (delete-frame?
    :docstring
    #.(one-string-sp
       "When NIL (the default), the organism frame itself and its"
       "descriptive information are preserved.  When T, the organism"
       "frame itself is deleted from the system, and the organism"
       "would need to be preloaded before being loaded if it is not private"
       "and restoration is desired."
       )
    :parameter-type &key :default-value nil :value-type boolean)
   )
  (:see-also load-organism load-organisms available-organisms organism)
  )   

(help:document-function
    add-organism-alias
  (:summary 
   "Gives a new nickname to an organism.")
  (:returns 
   "The organism's frame"
   :type frame)
  (:examples
   "(progn (add-organism-alias 'anny a7120) anny) --> #$anabaena_pcc7120"
   )
  (:text
   (:p
    #.(one-string-sp
       "Adds a new nickname to the existing list of nicknames or an organism."
       "The alias must be a symbol or a string (if a symbol, its"
       "SYMBOL-NAME is used.)  The organism argument must be a frame."
       ))
   (:p
    #.(one-string-sp
       "In addition to adding the nickname to the organism's list of"
       "nicknames, the symbol represented by the uppercase characters"
       "of ALIAS is given a value which is the organism (if no such"
       "symbol currently exists, one is created).  This symbol"
       "is defined as a DEFCONSTANT.") 
    ))
  (:parameters
   (alias
    :docstring
    "An organism's new nickname as a string or symbol."
    :value-type (or symbol string)
    )
   (orgf
    :docstring
    "An organism frame." 
    :value-type frame))
  (:see-also organism available-organisms load-organism))

(help:document-function
    organism
  (:summary 
   "Converts an organism designator to an organism frame.")
  (:returns 
   "The organism's frame."
   :type frame)
  (:examples
   "(organism a7120) --> #$anabaena_pcc7120"
   "(organism \"a7120\") --> #$anabaena_pcc7120"
   )
  (:text
   (:p
    #.(one-string-sp
       "Takes as input an organism designator and returns the"
       "organism's actual frame."
       "An organism designator can be either a string or a symbol."
       "If a string, it must be one of the organism's nicknames, found"
       "in the organism's NICKNAMES slot.  Try (#^nicknames org)."
       "If a symbol, it must be one of the organism's nicknames, found"
       "in the organism's NICKNAMES slot.  Try (#^nicknames org)."
       )))
  (:parameters
   (org
    :docstring
    "An organism's nickname (a string) or object-symbol (a symbol)."
    :value-type (or string symbol)))
  )

(help:document-function pprint-organism
  (:summary 
   "Pretty prints information about an organism.")
  (:returns 
   "NIL (executed for side effect)"
   :type boolean)
  (:examples
   "(pprint-organism npun) --> ;; lots of information about nostoc punctiforme"
   )
  (:text
   (:p
    #.(one-string-sp
       "Pretty prints information about ORGF.  PPRINT-LIMIT determines"
       "how many of the organism's genes, proteins, and contiguous sequences"
       "are shown."
       )))
  (:parameters
   (orgf
    :docstring "An organism frame." :value-type organism)
   (pprint-limit
    :docstring 
    "A limit on numbers of genes, proteins, and contiguous sequences."
    :parameter-type &key :default-value 10 :value-type integer))
  (:see-also frames:df frames:slotv))
   
   
(help:document-function loaded-organisms
  (:summary 
   "List of organisms currently loaded into the BioBike system.")
  (:returns 
   "Organism frames"
   :type list)
  (:examples
   ((loaded-organisms) :nil "The current list of loaded organisms.")
   )
  (:text
   (:p
    #.(one-string-sp
       "A BioBike system can have available any number of organisms,"
       "but only some of them may actually be loaded and ready for"
       "computation on at any one time.  This function returns those"
       "organisms that are actually loaded, as opposed to those which"
       "are available, but not yet loaded."
       )))
  (:see-also available-organisms load-organism 
   load-organisms unload-organism preload-organisms)
  )
   
   
