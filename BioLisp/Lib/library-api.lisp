;; -*- Package: biolisp; mode: lisp; base: 10; Syntax: Common-Lisp; -*-

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

(eval-when (:compile-toplevel :load-toplevel :execute)

  ;; The filenames below don't correctly track the location of the code because
  ;; it's moved around so much! %%%

  (defparameter *library-api-symbols*
    '(
      hash-table
      aloop
      ;; orthologs.lisp
      two-way-ortholog-of
      best-blast-ortholog-of
      go-orthologs-of
      ordered-blast-orthologs-of
  ;; functions required by ORTHOLOG-OF* and HOMOLOG-OF*, Arnaud June 6 2008
     forward-best-blast-ortholog-of-tmp
     reverse-best-blast-ortholog-of-tmp
     two-way-best-blast-ortholog-of-tmp
     forward-blast-orthologs-of-tmp
     reverse-blast-orthologs-of-tmp

      ;; organism-gene-frames.lisp
      frame-designators-equivalent?
      restricted-gene-table
      ;; phylip.lisp
      run-phylip
      ;; uberprims.lisp
      ;; replaced by ncomplement-base-pairs in bioutils/amino-conversions.lisp
      ;; nt-complement
      report-alignment-summary
      summarize-alignment
      goid-as-string->number.frame
      show-system-defun
      render-pathway-in-organism-terms
      find-pathways
      render-all-pathways
      count-commonalities
      sort-genes-near-position
      gene-centroid
      adjacent-genes
      go-membership-count
      gene->all-related-go-frames-and-isas
      is-gene-related-to-go-frame?
      genes->common-go
      select-microarray-table-data
      append-to-hypothesis-space
      assert-constraint
      assert-qrelation
      add-hypo-expr
      list-constraints
      align
      cross-blast-short
      xml->temp-frame
      cross-blast-long
      ncbi-blast-sequence
      safely-read-from-string-if-the-result-will-be-a-number-otherwise-just-pass-through-the-input-string
      meme
      cross-subsets
      consolve      
      consolve-avg
      avgraphs
      seegraph
      group-by-type
      with-temporary-fasta-file
      google-search
      run-rnaz
      external-executable-path
      categorize
      ;; pubmed.lisp
      raw-pubmed-query
      pubmed-query
      unearth-slots
      extract-xml-token
      extract-headed-lists
      ;; crossmetrics.lisp
      create-crossmetric-table
      crossmatched-entities-of
      blastgene
      give-graph
      ;; rtable
      r-cluster-kmeans
      genes-in-cluster
      display-clusters
      r-anova
      r-do-d2
      ;; pwys
      go->pwys
      mol->pwys
      *pwys*
      *gene-prefix->organism*
      *organism->prefix*
      ;;
      ensure-table
      share-table
      ))
  (export *library-api-symbols* (find-package :biolisp))
  )



