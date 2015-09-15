;;; -*- mode: Lisp; Syntax: Common-Lisp; Package: bio; -*-

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

;;; Author:  JP Massar.  

(defun phylip-demo (go-node &key (type :dna))
  #.(one-string-nl
     "Given a GO node, this finds the orthologs to the first Synechocystis6803"
     "gene annotated to that node, aligns these, and then uses Phylip to form"
     "a phylogenetic tree from the aligned results. The :TYPE argument" 
     "(default :DNA) can be :DNA or :PROTEIN.  In the :PROTEIN case," 
     "the first protein derived from each indicated gene is used.")
  (block exit
    (unless (#^go.related-genes go-node)
      (cformatt "No go related genes for this go node.")
      (return-from exit))
    (let ((syngenes 
           (remove-if-not
            (lambda (g) (not (eq (#^organism g) #$synechocystis_pcc6803)))
            (#^go.related-genes go-node)
            )))
      (unless syngenes 
        (cformatt "No synechocystis genes for go node.")
        (return-from exit))
      (let* ((demogene (first syngenes)) 
             (orthologs (bbl::orthologs-of demogene in *loaded-organisms*)))
	(pushnew demogene orthologs)
        (unless orthologs 
          (cformatt "No orthologs found for gene ~A" demogene)
          (return-from exit))
	(ecase type 
	  (:dna nil)
	  ((:protein :proteins) 
	   (setq orthologs
		 (mapcar (lambda (gene) (car (#^proteins gene))) orthologs))))
        (let ((aligned-sequences (#^alignments (align orthologs))))
          (let ((tree (bio::run-phylip aligned-sequences :type type
				       :labelfn #'car :seqfn #'second)))
            (terpri)
            (cformatt "Ancestor tree for cyano organisms wrt ~A" go-node)
            (terpri)
            (cformatt "~a                  Organism"
		      (ecase type
			(:dna "Gene")
			((:protein :proteins) "Protein")))
            (loop for gene in orthologs do
                  (format t "~20A ~A ~%" gene (#^organism gene)))
            (terpri)
            (seegraph tree)
            ))
	  ))))


      