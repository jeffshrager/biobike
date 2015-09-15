;;; -*- Package: bbl-test-user; mode: lisp; base: 10; Syntax: Common-Lisp; -*-

(in-package :bbl-test-user) 

;;; +=========================================================================+
;;; | Copyright (c) 2005 JP Massar, Jeff Elhai, Mark Slupesky, Peter Seibel   |
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

;;; Author: JP Massar, Mark Slupesky.  

;;;;;;;;;;;;;;;;;;;;;    BLAST TESTS     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defbblssttest= 
 simto1
 (ecase (user::os?)
   (:unix 
    (let ((result nil))
      (lisp:with-output-to-string (p) 
        (let ((*standard-output* p))
          (setq 
           result 
           (typep 
            (genes-similar-to some-gene in (#^organism some-gene))
            'utils::garray
            ))))
      result 
      ))
   (:windows t))
 t)

;; this test is guaranteed to match (and therefore return a garray)
(defbblssttest=
 simto1a
 (seqsim-wrapper (:garray)
   (sequences-similar-to 
     "ggggggggggggggggggggggg" in "ggggggggggggggggggggggggggg")
   )
 t
 )
  
(defbblssttest= 
 simto2
 (ecase (user::os?)
   (:unix 
    (let ((result nil))
      (lisp:with-output-to-string (p)
        (let ((lisp:*standard-output* p))
          (setq 
           result 
           (let ((prots-tbl
                  (proteins-similar-to some-corresponding-protein in
                    (#^organism some-corresponding-protein))))
             (loop for row from (utils::garray-axis-first-index prots-tbl 0)
               to (utils::garray-axis-last-index prots-tbl 0)
               do
               (when (eq (blast-value prots-tbl row column-labeled "query")
                         some-corresponding-protein)
                 (return some-corresponding-protein))
               finally (return nil)
               )))))
      result
      ))
   (:windows some-corresponding-protein)
   )
 some-corresponding-protein
 )

;; This should create a fasta organism database
;; for some-organism if none exists.
(defbblssttest= 
 simto3
 (seqsim-wrapper (:garray)
   (sequences-similar-to 
     (lisp:first (#^genes some-organism)) in some-organism)
   )
 t
 )

(defbblssttest=
 simto4
 #+:sframes
 (seqsim-wrapper (:garray)
   (sequences-similar-to 
     (lisp:first (#^proteins some-organism)) in 
     some-organism protein-vs-protein)
   )
 #-:sframes
 (seqsim-wrapper (:list)
   (sequences-similar-to 
     (lisp:first (#^proteins some-organism)) in 
     some-organism protein-vs-protein)
   )
 t
 )

(defbblssttest=
 simto4a
 (seqsim-wrapper (:garray)
   (sequences-similar-to 
     (lisp:first (#^proteins some-organism)) in some-organism
     :threshold 0.001 protein-vs-protein)
   )
 t
 )

(defbblssttest=
 simto4b
 (seqsim-wrapper (:garray)
   (sequences-similar-to 
     (lisp:first (#^proteins some-organism)) in (#^proteins some-organism)
     :threshold 0.001)
   )
 t
 )

(defbblssttest=
 simto5
 (seqsim-wrapper (:garray-or-nil)
   (sequences-similar-to 
     (lisp:first (#^genes some-organism)) in some-contig
     ))
 t
 )

(defbblssttest=
 simto6
 (seqsim-wrapper (:list-of-garray-or-list-of-nil)
   (sequences-similar-to 
     each
     (list
      (bbi::make-labeled-sequence :label "foo" :sequence "acgtttactgac")
      (bbi::make-labeled-sequence :label "bar" :sequence "ttgattgagcaa"))
     in 
     (lisp:subseq (#^genes some-organism) 0 2)
     ))
 t
 )

(defbblssttest=
 simto7
 (seqsim-wrapper (:list-of-garray-or-list-of-nil)
   (sequences-similar-to 
     each
     (lisp:subseq (#^genes some-organism) 0 2)
     in
     (random-dna :length 10000)
     ))
 t
 )

;; This test is intended to force the code to run that creates and uses a
;; .pal or .nal file for the 2 organisms (the low level blast
;; code uses this mechanism to avoid creating large databases
;; of multiple organisms)
(defbblssttest= 
 simto8
 (seqsim-wrapper (:list-of-garray)
   (if (> (length bio::*loaded-organisms*) 1)
       (let* ((sorted-orgs (loaded-organisms-by-total-sequence-size))
              (org1 (lisp:first sorted-orgs))
              (org2 (lisp:second sorted-orgs))
              (g1 (lisp:first (#^genes org1)))
              (g2 (lisp:first (#^genes org2))))
         ;; sequences-similar-to returns a list of garrays when
         ;; given a list of target organisms
         (sequences-similar-to each (list g1 g2) in (list org1 org2))
         )
     (list (utils::make-garray '($)))
     ))
 t
 )

(defbblssttest= 
 simto9
 #-:sframes
 (seqsim-wrapper (:list-or-nil)
   (if (> (length bio::*loaded-organisms*) 1)
       (sequences-similar-to 
         (lisp:first (#^proteins some-organism)) 
         in 
         (lisp:subseq (loaded-organisms-by-total-sequence-size) 0 2)
         protein-vs-protein
         )
     '(1 2)
     ))
 #+:sframes
 (seqsim-wrapper (:garray-or-nil)
   (if (> (length bio::*loaded-organisms*) 1)
       (sequences-similar-to 
         (lisp:first (#^proteins some-organism)) 
         in
         (lisp:subseq (loaded-organisms-by-total-sequence-size) 0 2)
         protein-vs-protein
         )
     (utils::make-garray '($))
     ))
 t
 )


;; So, first we create the leonardo database, and make sure the message
;; is correct
(defbblssttest=
 simto10
 (ecase (user::os?)
   (:unix
    (let ((wb::*username* :evaluser)
          (bbi::*debug-blast-aux* t))
      (let ((sstring 
             (lisp:with-output-to-string (p)
               (let ((*standard-output* p))
                 (sequences-similar-to 
                   btu::some-gene in (#^genes (#^organism btu::some-gene))
                   :use-database "leonardo" remake-database
                   )))))
        (and 
         (numberp (lisp:search "calling make-blast-database" sstring))
         (not (lisp:search "Using blast database" sstring))
         ))))
   (:windows t)
   )
 t
 )

;; then, we try to use the database, and check the message
(defbblssttest=
 simto11
 (ecase (user::os?)
   (:unix
    (let ((wb::*username* :evaluser)
          (bbi::*debug-blast-aux* t))
      (let ((sstring 
             (lisp:with-output-to-string (p)
               (let ((*standard-output* p))
                 (sequences-similar-to 
                   btu::some-gene in (#^genes (#^organism btu::some-gene))
                   :use-database "leonardo"
                   )))))
        (and 
         (not (lisp:search "calling make-blast-database" sstring))
         (numberp (lisp:search "Using blast database" sstring))
         ))))
   (:windows t)
   )
 t
 )



