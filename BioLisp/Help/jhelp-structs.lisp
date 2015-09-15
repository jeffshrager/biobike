;;; -*- Package: help; mode: lisp; base: 10; Syntax: Common-Lisp; -*-
(in-package :help)

;; Move here to avoid compiler warning -- JP.  
(defparameter *jpage-count* 0)
(defparameter *jsymbol-count* 0)

(defun stringify (mess)
	(if mess
	(format nil "~a" mess)
	""  ;; not "NIL".
	)
)


;;  Version 2.  Input side.

;; Currently used for incoming probe.
;;
;;Structure of a bag:  ( (wordID . count) (wordID . count) ...  ) i.e., Assoc-list;  and wordIDs are sorted.
;;
(defstruct wordbag bag total bigrams)  ;;deprecated.  Delete soon.
;;  (wordbag-bag myWB)     (wordbag-total myWB)  (wordbag-bigrams myWB)
;;  (setf (wordbag-bag myWB) newList)
;;  (make-wordbag :bag NIL  :total 0 :bigrams nil)


(defvar *Bundle-count* 0)    ;;deprecated.  Delete soon.
(defstruct Bundle   ID   titleWB descriptionWB keywordsWB textWB   DocObj)    ;;deprecated.  Delete soon.

;; inBundles is an unsorted list of Bundles that have that word contained in them.
(defstruct dicwordentry   wordstring wordID  inBundles)      ;;deprecated.  Delete soon.
;;(defstruct JDictionary     wordentries)
;;(defvar *the-jdictionary* (make-JDictionary :wordentries NIL))
(defvar *the-jdictionary* (make-hash-table))      ;;deprecated.  Delete soon.

;;---------------------------------------------------------------------
;; Version 3.  Run side.

(defstruct JDocument   ID   doctype   thedoc  functor-url text)
;; Main output item of system.
;; This is a wrapper that holds whatever the real contents should be.
;;  Atomic entity.
;; The ID is unique (a positive integer), and allows it to be sorted.
;; Doctypes:  'documentation-file 'function-documentation 'glossary-entry 'module
;;	'symbol-doc 'topic 'tutorial         'jpage
;;  thedoc:  stores an arbitrary documentation object, often the legacy docobj
;;  functor-url is an expression that holds the documentation URL, that must be eval'd at run-time. 
;;      It is not a box command but is in fact a true URL that will often go to the legacy doc page.  But quoted.
;;
;;  New slot:  text.  Possibly null string that lists all interesting stuff in Document.
;;             Used for searching on output to give context lines.
;;             This is redundant with the document itself, but various flavors of document have different texts,
;;             so it's easier to keep a copy.  Sigh.  Next time do this with 10 different flavors of methods.
;;             Primarily used for function documentation.



(defparameter *JDocument-count* 0)

; Singleton array for keeping track of all of the known unique Documents.  Make this a Hash soon.
(defparameter *JDocuments*  (make-array 10000 :fill-pointer 0 :adjustable T))
(defun add-JDocument (JDoc)  (vector-push-extend JDoc *JDocuments*))
(defun JDocuments-count () (length *JDocuments*))  ;;this is the boots-on-ground version.  We also keep track in a var.
(defun get-JDocument (JDocID) (elt *JDocuments* JDocID))  ; throws error if out of bounds, fix this.  0-based.

;; JKM Mar 1 '13  Have to add fields "name" and "parameters" so as to shadow the functions' slots.
;; "name" stores a simple symbol; "parameters" stores a list of parameter objects, get it from the function.
;; no, won't work, needs function "name" to get stuff, not JPage-name.
;; (defstruct JPage name title subtitle keywords logical-form summary text parameters  )
(defstruct JPage  title subtitle keywords logical-form summary text )


;;  Barrel Entry is an assoc couplet of  ( JDoc-IDint  .  score[/count] ).
;;  It is simple enough that we do not declare a struct for it.
;;
;;  Note that Barrels sort on the ID, whereas Scoring will later sort on the score, which will be reversed.
;;  Score is absolute and is a float; it should be normalized for relevance, frequency, specificity.
;;  We'll compare apples against other apples from other Words' barrels.

(defstruct JSummary   results barrels)
;; A temporary holding system for the summarized results of the system.  *One per query*, so sort of a singleton.
;;  Summary is necessary as the merge interleaving must trickily be done later, after gathering, not inline.
;; "results" is a single list of SWAPPED Barrel Entries, with score in front, 
;;  that will get MERGED, sorted/ranked, truncated, and used to return.
;;     As the final step in currying this, the scores are thrown away, and instead of a dual-level list of conses,
;;     it is boiled down into a single list of JDocs.  But this is done as the last step.
;; "barrels" is a list of 0 or more Barrel structures, which each contain a list of Barrel Entries (JDocID . score).
;;  We own the results.  We do *not* own the Barrels.  Result entries must be *copied* from Barrel lists.
;;  Results must be GC'd in a system that worries about such.  Barrels do *not* get GC'd.
;;
;;  (make-JSummary :results nil  :barrels nil)

(defstruct JBarrel  entries  count)
;;  entries is a list of Barrel Entries.  count is the length of the list, for speed.
;;  So a Barrel is effectively a list of JDocuments--all the JDocs that have this Word/Concept in them. 
;;  The Barrels are compiled permanently at the beginning of JHelp startup,
;;  and are fixed constant thereafter (assuming documentation is constant).
;;  A Barrel does not get added to, after it has been put together.
;;  Barrels are result entries that get hashed by Word, Bigram, or Concept.
;;  Google probably means something different with their Barrels
;;  as to what I'm calling mine this year, caveat.  Gotta call these something, though.
;;
;;  (make-JBarrel :entries nil  :count 0)

(defstruct JHelp-Engine  wordhash  bigramhash categorieshash learnedhash)
;;  Definition of the singleton object that runs everything.


(defparameter *JHelp-Engine*     nil)

(defun clear-JHelp-Engine ()
  (setq *JDocuments*  (make-array 10000 :fill-pointer 0 :adjustable T))
  (setq *JDocument-count* 0)
  (setq *jpage-count* 0)
  (setq *JHelp-Engine*     
          (make-JHelp-Engine	:wordhash    (make-hash-table :test #'equalp) 
			:bigramhash (make-hash-table :test #'equalp) 
			;; Extend system here if you need more tables.
			:categorieshash   (make-hash-table :test #'equalp)  ;; Stub.  Not supported yet.
			:learnedhash       (make-hash-table :test #'equalp)  ;; "  ".
)))
;; Note that equalp is agnostic w/r/t lower and upper case, whereas equal is strict.
;; So we don't need to smallify.    Fix this if this becomes significant for this application.

(defstruct stemmer tree) 
(defparameter *JHelp-suffix-taster*     nil)

(clear-JHelp-Engine)  ;;now called inside (jhelp-init). Also, MUST be called here, as some params get reset.
