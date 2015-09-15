(in-package :com.biobike.help)

(document-module extract
  "Functions for manipulating contigs."
  (:keywords genes contig bio sequences)
  (:functions extract-gene-sequence
	      extract-contig-sequence
	      extract-protein-sequence))

;; Might be generated automatically from lambda list and docstring.
(document-function foo
  (:canonical t)
  (:summary "A sample function.")
  (:returns nil :type integer)
  (:parameters
   (x)
   (args :parameter-type &rest)
   (y :default-value 10 :parameter-type &key)
   (z :default-value 20 :parameter-type &key :keyword-name :the-z))
  (:examples
   "(foo \"xxx\" t 20) ==> 'frab")
  (:see-also extract-contig-sequence)))

;; Written by hand or generated from DECLARES.
(document-function foo
  (:parameters
   (x :value-type (or string frob))
   (args)
   (y :value-type boolean)
   (z :value-type integer)))

;; Written by hand.
(document-function foo
  (:returns "A fooey value")
  (:summary "A sample function with a better explanation.")
  (:parameters
   (x :value-type (or string frob) :docstring "The first argument.")
   (args :docstring "All the keyword arguments.")
   (y :value-type boolean :docstring "The second argument.")
   (z :value-type integer :docstring "Some other thing."))
  (:examples
   "(foo \"xxx\" t 10) ==> 'frob")
  (:see-also extract-protein-sequence)))

(document-function extract-gene-sequence  (gene
					   &key
					   (cache? nil)
					   (start-offset 0)
					   (end-offset 0)
					   (complement-backwards? t)
					   (if-no-sequence? :null-string)
					   (if-wrapped-but-not-circular :error)
					   (ignore-architecture? nil)
					   (safely? t))
  
  "Extract a sequence of base pairs representing a given GENE from its CONTIG."

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
   (gene "The gene whose sequence should be extracted." :type gene-frame)

   (complement-backwards? "When true, and when the gene's
   DIRECTION is B (backwards), the extracted sequence is reversed
   and each base is complemented." :type boolean)

   (ignore-architecture? "When true, the GENE's ARCHITECTURE
   property is ignored, and the sequence is defined solely by the
   FROM and TO properties (possibly as modified by START-OFFSET
   and END-OFFSET)." :type boolean)

   (start-offset "Value added to FROM when reading forward or
   substracted from TO when reading backward. If supplied,
   implies IGNORE-ARCHITECTURE? is true." :type integer)

   (end-offset "Value added to TO when reading forward or
   substracted from FROM when reading backward.  If supplied,
   implies IGNORE-ARCHITECTURE? is true." :type integer)

   (cache? "If true, the sequence is retrieved from the gene's
   #$Sequence slot if there, or stored there once retrieved if
   not." :type boolean)
   
   (if-wrapped-but-not-circular "Determines what happens if FROM
   > TO and the CONTIG is not defined as CIRCULAR. Possible
   values are :ERROR (the default) which signals an error, :WARN
   which warns but extracts the sequence as if the CONTIG was
   defined as CIRCULAR and, NIL, which extracts the sequence as
   if the CONTIG was defined as CIRCULAR without issuing a
   warning." :type symbol))

  (:see-also extract-contig-sequence extract-protein-sequence))


(document-function extract-contig-sequence (contig from to direction 
						   &key
						   (complement-backwards? t)
						   (if-wrapped-but-not-circular :error) 
						   (safely? t))
  "Extract a subsequence of base pairs from a CONTIG."

  (:returns "The extracted sequence" :type string)

  (:text)

  (:parameters
   (contig "The contig from which the sequence should be extracted." :type  frame)

   (from "The 1-based, inclusive index of the first base pair
   to be extracted." :type integer)

   (to "The 1-based, inclusive index of the last base pair to be
   extracted." :type integer)

   (direction "The direction to read the sequence, either F or B." :type string-designator)

   (complement-backwards? "When true, and when DIRECTION is
   B (backwards), the extracted sequence is reversed and each
   base is complemented." :type boolean)

   (if-wrapped-but-not-circular "Determines what happens if FROM
   > TO and the CONTIG is not defined as CIRCULAR. Possible
   values are :ERROR (the default) which signals an error, :WARN
   which warns but extracts the sequence as if the CONTIG was
   defined as CIRCULAR and, NIL, which extracts the sequence as
   if the CONTIG was defined as CIRCULAR without issuing a
   warning." :type symbol)

   )

  (:see-also extract-gene-sequence extract-protein-sequence))

(document-function extract-protein-sequence  (protein &key (strip-trailing-star? t) (cache? nil))

  "Extract a sequence of base pairs that encode for a given PROTEIN."
  
  (:returns "The extracted sequence" :type string)

  (:text)

  (:parameters 
   (cache? "If true, the sequence is retrieved from the protein's
   #$Sequence slot if there, or stored there once retrieved if
   not." :type boolean)

   (strip-trailing-star? "When true, any trailing '*' is removed
   unless CACHE? is true and the sequence is returned from the cache." :type boolean))

  (:see-also extract-gene-sequence extract-contig-sequence))
