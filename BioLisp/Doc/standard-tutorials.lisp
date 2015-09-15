;;; -*- Package: webuser; mode: lisp; base: 10; Syntax: Common-Lisp; -*-

(in-package :webuser)

;;; A set of live tutorials is specified by providing a directory that
;;; they live in (either as a logical pahthname or a hardcoded
;;; pathname) and then a set of live tutorial definitions.

;;; A live tutorial definition consists of a name (as a string) and
;;; then a set of :property value pairs.  You need to provide the
;;; :mode and :filename property values, and if the :mode property is
;;; :lhtml, you need to provide the :lhtml-function value.  The
;;; :description property value is used when the system prints out a
;;; list of all the available tutorials (see below).

;;; Example:  If the directory is "/home/massar/live/", the :mode is :html, 
;;; and the :filename is "dino-tutorial" then the actual file where the
;;; system will look for the html for the tutorial will be

;;; /home/massar/live/dino-tutorial.html

;(when wb::*live-tutorial-hash*
;  (clrhash wb::*live-tutorial-hash*))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant td (translate-simple-lp "websrc:Doc;livetutorials;")))

(progn
  (help:def-live-tutorial
   "A Tour of BioBike"
   (:sort-order 10)
   (:user-mode :biolisp)
   (:file-type :html )
   (:filename #.(s+ td "tour"))
   (:description 
    "Getting one's feet wet with BioLisp and the BioBike Listener"))
  (help:def-live-tutorial
   "Lisp I: Evaluation"
   (:sort-order 20)
   (:user-mode :biolisp)
   (:file-type :html )
   (:filename #.(s+ td "lispintro"))
   (:description "The basics of Lisp evaluation"))
  (help:def-live-tutorial
   "Lisp II: Data and Iteration"
   (:sort-order 30)
   (:user-mode :biolisp)
   (:file-type :html )
   (:filename #.(s+ td "iteration"))
   (:description "Lisp data structures and iteration (loops)"))
  (help:def-live-tutorial
   "Lisp III: Functions"
   (:sort-order 40)
   (:user-mode :biolisp)
   (:file-type :html )
   (:filename #.(s+ td "functions"))
   (:description "Lisp functions are first class"))
  (help:def-live-tutorial
   "(Lisp Macros)"
   (:sort-order 45)
   (:user-mode :biolisp)
   (:file-type :html )
   (:filename #.(s+ td "macros"))
   (:description "(Optional) Lisp macros let you create new syntax"))
  (help:def-live-tutorial
   "Simple Tables"
   (:sort-order 50)
   (:user-mode :biolisp)
   (:file-type :html )
   (:filename #.(s+ td "tables"))
   (:description "Working with simple data tables"))
  (help:def-live-tutorial
   "Biocomputing I"
   (:sort-order 60)
   (:user-mode :biolisp)
   (:file-type :html )
   (:filename #.(s+ td "elhaiproblem1"))
   (:description 
    "Comparing gene function across organisms using annotation + microarrays"))
  (help:def-live-tutorial
   "Graph R&R"
   (:sort-order 70)
   (:user-mode :biolisp)
   (:file-type :html )
   (:filename #.(s+ td "graphsearch"))
   (:description "Representation and Reasoning about Graphs."))
  (help:def-live-tutorial
   "GO Analysis I"
   (:sort-order 80)
   (:user-mode :biolisp)
   (:file-type :html )
   (:filename #.(s+ td "GO1"))
   (:description "Simple symbolic biocomputing based on the Gene Ontology"))
  (help:def-live-tutorial
   "Microarray Analysis"
   (:sort-order 90)
   (:user-mode :biolisp)
   (:file-type :html )
   (:filename #.(s+ td "arrayclustering"))
   (:description "Simple microarray data clustering"))
  (help:def-live-tutorial
   "Metabolic Simulation I" 
   (:sort-order 100)
   (:user-mode :biolisp)
   (:file-type :html )
   (:filename #.(s+ td "MBCSmeta1"))
   (:description "Introduction to dynamic analysis of metabolic pathways"))
  (help:def-live-tutorial
   "Metabolic Simulation II" 
   (:sort-order 110)
   (:user-mode :biolisp)
   (:file-type :html )
   (:filename #.(s+ td "GO3"))
   (:description 
    "Dynamic pathway analysis using the Gene Ontology knowledge base"))
  (help:def-live-tutorial
   "Regulation Simulation"
   (:sort-order 120)
   (:file-type :html )
   (:user-mode :biolisp)
   (:filename #.(s+ td "jssymbiolietal"))
   (:description 
    "Simulating the cell cycle (based upon a 2004 PNAS publication)"))
  (help:def-live-tutorial
   "Natural Language I"
   (:sort-order 130)
   (:user-mode :biolisp)
   (:file-type :html )
   (:filename #.(s+ td "bionlp"))
   (:description 
    "Extracting biological relationships from PubMed using simple statistics"))
  (help:def-live-tutorial
   "Functional Phylogeny"
   (:sort-order 150)
   (:file-type :html )
   (:user-mode :biolisp)
   (:filename #.(s+ td "phylotops"))
   (:description "Exploring phylogeny based upon gene function"))
  (help:def-live-tutorial
   "Model Discovery I"
   (:sort-order 160)
   (:user-mode :biolisp)
   (:file-type :html )
   (:filename #.(s+ td "regdiscovery"))
   (:description "Reverse engineering of regulatory networks"))
  (help:def-live-tutorial
   "Model Discovery II"
   (:sort-order 165)
   (:user-mode :biolisp)
   (:file-type :html )
   (:filename #.(s+ td "partialcorr"))
   (:description 
    "Model discovery through partial correlations in microarray data"))
  (help:def-live-tutorial
   "Reasoning Intro"
   (:sort-order 170)
   (:user-mode :biolisp)
   (:file-type :html )
   (:filename #.(s+ td "reasoning1"))
   (:description "Introduction to automated reasoning using Snark"))
  (help:def-live-tutorial
   "More BioReasoning"
   (:sort-order 185)
   (:user-mode :biolisp)
   (:file-type :html )
   (:filename #.(s+ td "snarkelhai"))
   (:description "Re-understanding the Elhai problem in reasoning terms"))
  (help:def-live-tutorial
   "Complex Regulatory Reasoning"
   (:sort-order 190)
   (:user-mode :biolisp)
   (:file-type :html )
   (:filename #.(s+ td "regsim2"))
   (:description "Using Snark to reason about the cell cycle"))
  )



