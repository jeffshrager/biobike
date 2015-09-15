;;; -*- Package: help; mode: lisp; base: 10; Syntax: Common-Lisp; -*-
(in-package :help)

(defun JHelp-stats  ()
	(format T  "Number of Documents in JHelp system: ~a  ~%" (JDocuments-count) )
	(format T  "Number of discrete Words in JHelp Engine: ~a ~%"   (hash-table-count (JHelp-Engine-wordhash *JHelp-Engine*)))
	(format T  "Number of discrete meaningful Bigrams in JHelp Engine: ~a ~%"   (hash-table-count (JHelp-Engine-bigramhash *JHelp-Engine*)))
)


(defun JHelp-?  ()
	(JHelp-stats))

(defun jhelp?  ()
	(JHelp-stats))