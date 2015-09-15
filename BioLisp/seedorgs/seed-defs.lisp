;;; -*- Package: bio; mode: lisp; base: 10; Syntax: Common-Lisp; -*-

(in-package :bio)

;;; +=========================================================================+
;;; | Copyright (c) 2009 JP Massar
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

;;; Author:  JP Massar

(defvar *features-descriptions-map* nil)
(defparameter *features-descriptions-map-name* "fdmap")

(defparameter *seed-relative-organisms-path* "FIG/Data/Organisms/")

(defparameter *contig-sequence-segment-size* 16384)

(defvar *new-seed-genome-frames* nil)

(defparameter *data-directory* (utils::s+ *bioetc-directory* "data/"))

(defvar *fids-for-gid* nil)
(defvar *pegs-for-gid* nil)
(defvar *annotations-hash* nil)

(defvar *seed-load-verbose* t)

(defvar *bike-genome-name* nil)
(defvar *genome-info* nil)
(defvar *genes-info* nil) 
(defvar *genes-descriptions-hash* nil)
(defvar *genes-descriptions* nil)
(defvar *proteins-info* nil)
(defvar *plist-info* nil)

(defvar *features-info-hash* nil)

(defvar *extract-sequence-info-bootstrap* nil)

(defvar *seed-genome-sequence-file* nil)
(defvar *seed-proteins-sequence-file* nil)
(defvar *seed-gene-annotations-file* nil)

(defparameter *acache-organism-structure* :external-lists)

(defparameter *attached-organism-slots*
  '(#$genes #$proteins #$contiguous-sequences #$noncoding-genes))

(defparameter *attached-contiguous-sequence-slots*
  '(#$genes-sorted-by-position #$fragments-sorted-by-position))

(defparameter *seed-gid-table-path* 
  #+add-complete
  (ignore-errors 
    (merge-pathnames "seed-organism-table.txt" *load-pathname*))
  #-add-complete
  (ignore-errors 
    (merge-pathnames "seed-organism-table-merged.txt" *load-pathname*)))

(defvar *use-new-ramy-table* t)

(defparameter *aux-seed-genome-info-path* 
  (ignore-errors (merge-pathnames "aux-seed-genome-info.lisp" *load-pathname*)))

(defparameter *ramy-phage-table-path* 
  (ignore-errors (merge-pathnames "phages.txt" *load-pathname*)))
        
(defvar *seed-genome-table-info* nil)
(defvar *existing-prefix-hash* nil)

;;; Genome and Gname must remain as first and second elements!
(defvar *seed-genome-table-rows*
  (one-string
   "genome,gname,szdna,maindomain,pegs,rnas,"
   "complete,restrictions,taxonomy"
   ))

(defvar *seed-genome-table-slot-names*
  (mapcar 
   (lambda (x) (string-capitalize x))
   (string-split *seed-genome-table-rows* #\,)
   ))

(defvar *seed-gid-table* nil)
(defvar *gid->frames-map* nil)
(defvar *gid->gid-table-info* nil)

(defvar *acache-organism-dir* nil)
(defvar *acache-genome-dir* nil)
(defvar *acache-proteins-dir* nil)
(defvar *acache-seqinfo-dir* nil)


;;; The categories the SEED breaks its organisms into (along with "Other").
;;; The associated variables are initialized in bioseed-instance-init.lisp
;;; Biobike creates the concept of TYPE of category.  For now, there are
;;; two types: ALL and LATEST.  ALL is all the organisms in a category, while
;;; LATEST is the most recent SEED version of the organism.

(defparameter *seed-genome-types*
  '(("Virus" *seed-virus-frames* *latest-seed-virus-frames*) 
    ("Bacteria" *seed-bacteria-frames* *latest-seed-bacteria-frames*)
    ("Plasmid" *seed-plasmid-frames* *latest-seed-plasmid-frames*)
    ("Eukaryota"
     *seed-eukaryota-frames* *latest-seed-eukaryota-frames*)
    ("Archaea" *seed-archaea-frames* *latest-seed-archaea-frames*)
    ))

(defvar *seed-virus-frames* nil)
(defvar *seed-bacteria-frames* nil)
(defvar *seed-plasmid-frames* nil)
(defvar *seed-eukaryota-frames* nil)
(defvar *seed-archaea-frames* nil)
(defvar *latest-seed-genome-frames* nil)
(defvar *latest-seed-virus-frames* nil)
(defvar *latest-seed-bacteria-frames* nil)
(defvar *latest-seed-plasmid-frames* nil)
(defvar *latest-seed-eukaryota-frames* nil)
(defvar *latest-seed-archaea-frames* nil)
(defvar *seed-other-frames* nil)

(defun clear-seed-genome-lists ()
  (setq *seed-virus-frames* nil 
        *seed-bacteria-frames* nil
        *seed-plasmid-frames* nil
        *seed-eukaryota-frames* nil
        *seed-archaea-frames* nil
        *latest-seed-genome-frames* nil
        *latest-seed-virus-frames* nil
        *latest-seed-bacteria-frames* nil
        *latest-seed-plasmid-frames* nil
        *latest-seed-eukaryota-frames* nil
        *latest-seed-archaea-frames* nil
        *seed-other-frames* nil
        ))
  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *seed-genomes-with-B-characters*

#||

    #$Vibrio-cholerae-O1-biovar-eltor-str-N16961
    #$Haemophilus-influenzae-Rd-KW20
    #$Streptococcus-agalactiae-515
    #$Escherichia-coli-CFT073
    #$Vibrio-cholerae-MO10
    #$Streptococcus-mitis-NCTC-12261
    #$Treponema-pallidum-subsp-pallidum-str-Nichols
    #$Streptococcus-agalactiae-18RS21
    #$Listonella-pelagia-phage-phiHSIC
    #$Neodiprion-Lecontei-Nucleopolyhedrovirus

||#

  '(
    "243277.1" "71421.1" "342614.3" "199310.1" "345072.3" "246201.1"
    "243276.1" "342613.3" "310539.3" "249151.3"
    ))

(defparameter *seed-genomes-without-pegs*

#||

    #$Medicago-Truncatula
    #$Enterovirus-Yanbian-96-83csf
    #$Comamonas-Testosteroni
    #$Streptomyces-Nigrifaciens
    #$Leptolyngbya-Boryana
    #$Hollyhock-Leaf-Crumple-Virus-Satellite-Dna
    #$Tomato-Leaf-Curl-Virus-Satellite-Dna
    #$Neurospora-Intermedia
    #$Cryphonectria-Parasitica
    #$Synechococcus-Sp.
    #$Mouse-Cytomegalovirus-1
    #$Goatpox-Virus
    #$Oryza-Sativa
    #$Phytoplasma-Sp.
    #$Thanatephorus-Cucumeris
    #$Ralstonia-Pickettii

||#

  '("3880.1" "123738.1" "285.1" "44289.1" "1184.1" "169696.1" "53991.1"
             "5142.1" "5116.1" "1131.1" "10366.1" "186805.1" "4530.1"
             "2155.1" "107832.1" "329.1"
             ))

