;;; -*- mode: Lisp; Syntax: Common-Lisp; Package: biolisp; -*-

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

;;; Authors:  Jeff Elhai, JP Massar.

#|

1. Find genes related to adaptation to specific environment What is
the basis for the ability of some cyanobacteria to thrive under the
intense light of the open ocean?  To answer this, one might find those
genes that are present in Prochlorococcus MED4 (which lives on the
ocean surface) but absent in a sister organism, Prochlorococcus MIT
9313, that cannot survive except under low light intensity. From that
set, we select those genes whose orthologs in Synechocystis PCC 6803
are induced by exposure to high light intensity by at least a factor
of two. (Such experimental data is available for the well-studied
Synechocystis but not for either Prochlorococcus).

;;; Jeff E's original code.

(LOOP FOR gene IN (Genes-of promed4)
       AS syn-ortholog = (Two-way-ortholog-of gene syn6803)
       WHEN (AND (EXISTS syn-ortholog)
                 (DOES-NOT-EXIST (Two-way-ortholog-of gene Pro9313))
                 (> (Microarray-ratio-of syn-ortholog "Hihara2001") 2.0))
       COLLECT gene)

Translation: Consider every gene in Prochlorococcus MED4. For each
gene, find its ortholog (if it exists) in Synechocystis PCC 6803. Keep
the gene when three conditions hold: (a) an ortholog in Synechocystis
exists, (b) an ortholog in the low-light Prochlorococcus does NOT
exist, and (c) the Synechocystis ortholog is induced by high light at
least a factor of 2, according to the microarray data of Hihara et al
(2001).  

Genes-of (organism-frame): Returns all gene-frames of organism

Two-way-ortholog-of (gene-frame [FROM|IN] organism-frame):
Returns frame of ortholog of given gene within given organism, using a
precomputed lookup table 

Microarray-ratio-of (gene-frame [FROM|IN] microarray-data-table):
Returns ratio value for given gene looked up in data table

;;; JP's generalization and interpretation

(defbio sample-program-1 
        ((org1 :organism) 
         (org2 :organism)
         (org3 :organism)
         (hihara-table (:uarray-table load-hihara-format-uarray-table))
         (data-predicate :function)
         (ortholog-threshold :float))
  (loop for gene in (genes-of org1)
        as best-match = (two-way-ortholog-of gene org2)
        when (and (exists best-match)
                  (does-not-exist (two-way-ortholog-of gene org3))
                  (table-data-row-satisfies?
                   hihara-table best-match data-predicate))
        collect gene
        ))
        
;;; What the DEFBIO macro might generate

(defun sample-program-1
       (org1 org2 org3 hihara-table data-predicate ortholog-threshold)

 (with-organisms-db (db)

  ;; Convert the organism designators to frames if necessary.

  (setq org1 (canonicalize-organism-designator org1))
  (setq org2 (canonicalize-organism-designator org2))
  (setq org3 (canonicalize-organism-designator org3))

  ;; Read in the uarray table if necessary.

  (setq hihara-table
        (canonicalize-table-data-designator 
         org2-uarray-table 'load-hihara-format-uarray-table))

  (assert (valid-funcallable-object? data-predicate) ()
    "Ruh roh. DATA-PREDICATE argument to SAMPLE-PROGRAM-1, ~A, not a function"
    data-predicate)

  (setq ortholog-threshold (float ortholog-threshold 0.0d0))

  (loop for gene in (genes-of org1) 
        as best-match = (best-blast-ortholog-of gene org2 ortholog-threshold)
        when (and (exists best-org2-match)
                  (does-not-exist
                   (best-blast-ortholog-of gene org3 ortholog-threshold))
                  (table-data-row-satisfies?
                   hihara-table best-match data-predicate
                   ))
        collect gene
        )))

;;;; Sample call

(sample-program-1 
 :promed4 :syn6803 :pro9313 "Hihara2001"
 'sample-program-1-data-predicate
 1.0d-50
 )

|#

;;;; Code necessary to support the above functionality.

(defmacro exists (x) x)
(defmacro does-not-exist (x) `(null ,x))

;;; An organism-designator (orgd) is anything that matches uniquely
;;; the name of an existing organism, loaded or in our database,
;;; depending on interpretation.
;;; It can be an organism frame itself, or a string which matches
;;; the name of an organism, or a symbol who's symbol name matches.


(defun valid-funcallable-object? (x)
  (or (functionp x) (and (symbolp x) (fboundp x))))

(defun canonicalize-table-data-designator 
       (x &key (load-function 'read-table-data))
  #.(one-string-nl
     "If X is already a table of data, return it. Otherwise X should be "
     "the (path)name of a file which can be loaded using READ-TABLE-DATA"
     "to create the table of data.")
  (cond
   ((typep x 'cl-user::table-data) x)
   ((stringp x)
    (funcall load-function (biolingua-table-file-named x)))
   ((pathnamep x)
    (canonicalize-table-data-designator 
     (namestring x) :load-function load-function))
   (t (error "Don't know how to convert ~A into a table data object" x))
   ))

(defun biolingua-table-file-named (name)
  #.(one-string-nl
     "Find a data table named NAME.  If NAME is a full pathname, use it."
     "Otherwise create a path using *default-pathname-defaults* (usually "
     "a user's home directory) and see if a file named NAME.tbl is there."
     "Otherwise look in BioLingua's library of data tables for the file.")
  (if (probe-file name)
      name
    (let ((uarray-file
           (namestring
            (make-pathname
             :name name 
             :type ".tbl"
             :defaults
             (cl-user:translate-simple-lp "bioetc:data;tables;")
             ))))
      (unless (probe-file uarray-file)
        (error "No microarray file named ~A found." name))
      uarray-file
      )))

(defun table-data-row-satisifies? (table key predicate)
  #.(one-string-nl
     "Find the unique row in TABLE indexed by KEY, and determine if"
     "PREDICATE is TRUE when called with that row as its argument."
     "If no row with that key exists return NIL.")
  (handler-case
      (let ((table-data-row (table-data-row table key)))
        (funcall predicate table-data-row))
    (error () nil)
    ))

(defun load-hihara-format-uarray-table (file)
  "Read the hihara data table we have, turning gene names into frames"
  (read-table-data 
   file 
   :n-predata-fields 1
   :key-columns '(0)
   :missing-value -100.0
   :other-rdrfuncs #'(lambda (x) (fff x))
   ))

;; Note that the vector we are given includes the initial (key/gene) data
(defun sample-program-1-data-predicate (x)
  "At least two out of the four data points of vector X must be 2.0 or higher"
  (>=
   (loop for j fixnum from 1 below (length x) sum
         (if (>= (aref x j) 2.0) 1 0))
   2))



(defun maybe-create-two-way-orthologs-table 
       (&key (table-name "two_way_orthologs"))
  (dosql
   (formatn
    (one-string
     "create table if not exists ~A ( "
     "p1 varchar(~D) not null, "
     "p2 varchar(~D) not null, "
     "primary key (p1,p2) "
     ")")
    table-name
    *standard-key-max-length* *standard-key-max-length*
    )))
   
  

(defun augment-two-way-orthologs-table 

       (blast-threshold &key (verbose? t)
                        &aux
                        all-organism-names
                        all-organism-frames
                        (table-name "two_way_orthologs")
                        organisms-already-indexed
                        organisms-not-yet-indexed
                        proteins-in-table
                        )

  (with-organisms-db (db)

    (when verbose? (cformatt "Running AUGMENT-TWO-WAY-ORTHOLOGS-TABLE"))

    (flet ((firsts (x) (mapcar 'first x)))

      ;; Step 1.  Get a list of all the organisms.
      (setq all-organism-names (available-organisms))

      ;; Step 2.  Create an empty table if it doesn't exist.
      (maybe-create-two-way-orthologs-table :table-name table-name)

      ;; Step 3.  Load all the organisms (if they aren't already loaded)
      ;; We could just load the proteins here, but this probably isn't
      ;; the program's bottleneck.

      (setq all-organism-frames
            (loop for orgd in all-organism-names collect (load-organism orgd)))

      (when verbose? 
        (cformatt "All organisms to be considered: ")
        (loop for orgf in all-organism-frames do (cformatt "  ~A" orgf)))

      ;; Step 4.  Determine all the organisms whose proteins 
      ;; have not already been analyzed.   This might take a long time!

      (when verbose? (cformatt "Determining all unique proteins..."))
      (setq proteins-in-table
            (firsts 
             (dosql (formatn "select distinct p1 from ~A" table-name))))
      (when verbose? 
        (cformatt "~D distinct proteins found." (length proteins-in-table)))
      (loop for protein in proteins-in-table do
            (let ((protein-frame (frame-fnamed protein)))
              (unless protein-frame) 
              (ierror 
               "Protein ~A is not a frame but all organisms loaded!" protein)
              (pushnew 
               (slotv protein-frame #$Organism) organisms-already-indexed
               )))
      (setq organisms-not-yet-indexed
            (set-difference all-organism-frames organisms-already-indexed))
      (when verbose?
        (cformatt "Organisms not yet processed for two-way orthologs: ")
        (loop for orgf in organisms-not-yet-indexed do (cformatt "  ~A" orgf)))
  
      ;; Step 5.  Compute a two-way-ortholog for each protein of
      ;; each organism that hasn't been indexed yet, against every
      ;; other organism.  When a two-way ortholog is found, insert
      ;; an entry for it into the table.

      (loop for orgf in organisms-not-yet-indexed do
            (when verbose? (cformatt "Processing organism ~A" orgf))
            (loop for other-orgf in (remove orgf all-organism-frames) 
                  as count = 0 do
                  (loop for protein in (slotv orgf #$Proteins) 
                        for pcount from 1 do
                        (when (and verbose? (zerop (mod pcount 200))) 
                          (format t "."))
                        (let ((match 
                               (two-way-ortholog-of 
                                protein other-orgf blast-threshold))
                              (*db-verbose* nil))
                          (when match
                            (incf count)
                            (dosql
                             (formatn
                              "insert into ~A values ('~A','~A')"
                              table-name
                              (mysql-escape (fname protein))
                              (mysql-escape match)
                              )))))
                  (when verbose? 
                    (cformatt "~D proteins of ~A have 2-way orthologs in ~A"
                              count orgf other-orgf
                              ))))

      )))

