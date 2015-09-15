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

3.  Find the core set of genes that defines cyanobacteria. All
cyanobacteria are able to fix carbon dioxide at the expense of light
energy and water-derived electrons. They do this in almost every
imaginable environment on earth. A comparison amongst the metabolic
capabilities of all available cyanobacteria may yield the core set of
genes required for the photosynthetic lifestyle. How to find those
genes?

  (ASSIGN gene-set (Genes-of Ana7120))
  (LOOP FOR organism IN (Remove-from-set *loaded-organisms* Ana7120) 
        DO (ASSIGN gene-set (Two-way-ortholog-of gene-set organism)))
  (GO-category-of gene-set)

Translation: Start out with genes from Anabaena PCC 7120 (an arbitrary
choice), then go through every organism known by BioLingua EXCEPT
Anabaena and find all genes orthologous to those in the current
gene-set (where orthology is defined as two-way best Blast hit).
Revise the gene-set to include only those genes where orthologs were
found in the organism. What's left at the end are those genes common
to all cyanobacteria. The last operation returns the Gene Ontology
categories of these genes.  GO-category-of (gene-frames): Returns
contents of GO-category slot of each given gene-frame. The argument
may be either a single frame or a list of frames).

|#

(defun core-genes-of (&optional (organism-set (available-organisms)))
  (when organism-set
    (let ((gene-set (#^Genes (first organism-set)))
          (other-organisms (rest organism-set)))
      (loop for organism in other-organisms
            as orgf = (load-organism organism) do
            (setq gene-set (two-way-orthologs-of gene-set orgf)))
      (go-categories-of gene-set)
      )))

(defun two-way-orthologs-of (gene-set orgf)
  (mapcarnn (lambda (gene) (two-way-ortholog-of gene orgf)) gene-set))

(defun go-categories-of (gene-set)
  (remove-duplicates (mapcan '#^Go-Id gene-set)))


   
      
