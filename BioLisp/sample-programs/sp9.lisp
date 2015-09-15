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

9. Align sequences One of the most common things molecular biologists
like to do is to align sequences looking for similarities. ClustalW is
perhaps the most widely used program for that purpose, and (who would
have guessed) it's on BioLingua! Suppose you wanted to align all
orthologs of your favorite gene, hoping to see what regions are
conserved over evolution (and thus probably important for function)
and what regions are just evolutionary fluff.

(ASSIGN my-favorite-gene (Get-gene all4312 FROM Ana7120))
(LOOP FOR organism IN (*loaded-organisms*)
      DO (ASSIGN ortholog (Two-way-ortholog-of my-favorite-gene IN organism))
      COLLECT (EXTRACT-SEQUENCE (PROTEIN-OF ortholog))
(ASSIGN orthologs *)
(DISPLAY (CLUSTAL orthologs) :FORMAT Clustal)

Get-gene ([FROM|IN] organism-frame gene-descriptors): Get-gene is an
alias of Get-genes. As with the latter function, the arguments can be
reversed so long as FROM|IN is in the right place.  Clustal
(list-of-sequences): Wrapper for Clustal. Provides program with
sequences in proper format and returns output appropriately.  

Display
(&REST text-strings &KEY (format NIL)): Same function as described
above, serving additional duty of displaying output string according
to built-in formats. [Can &REST and &KEY be used in the same argument
list?]

|#


(defun align-two-way-orthologs-via-clustal 
       (gene &key (organism nil) (blast-threshold 1.0e-10)
             &aux gene-frame ortholog)
  #.(one-string-nl
     "Determine all the two-way orthologs of GENE in all the currently"
     "loaded organisms, extract their protein sequences and align all"
     "these sequences via the CLUSTAL program.")
  (setq gene-frame
        (if (null organism)
            (canonicalize-gene-designator gene)
          (gene-of-organism gene organism)))
  (align
   (mapcarnn
    (lambda (organism)
      (setq ortholog (two-way-ortholog-of gene-frame organism blast-threshold))
      (and ortholog (extract-protein-sequence ortholog)))
    *loaded-organisms*
    )))


(defun entity-of-organism (entity organism entity-list-function)
  (if (isframe? entity)
      entity
    (let ((entity-name (fstring entity))
          (orgf (canonicalize-organism-designator organism)))
      (declare (simple-string entity-name))
      (find-if
       (lambda (x)
         (let* ((name (#^Fname x)) (dotpos (position #\. name)))
           (declare (simple-string name))
           (when dotpos
             (let* ((startpos (the fixnum (1+ (the fixnum dotpos))))
                    (matchpos (search entity-name name :start2 startpos)))
               (declare (fixnum startpos))
               (and matchpos 
                    (= (the fixnum matchpos) startpos)
                    (= (length entity-name) (- (length name) startpos))
                    )))))
       (funcall entity-list-function orgf)
       ))))

(defun gene-of-organism (gene-nickname organism)
  #.(one-string-nl
     "GENE-NICKNAME is either a gene frame, in which case it is returned,"
     "or a string or symbol which matches the part of a gene frame name found "
     "after the '.' -- so :all4312 is a nickname for #$A7120.all4312."
     "All the genes of ORGANISM are searched until the nickname matches"
     "and the gene so matched is returned, or NIL if no match is found.")
  (entity-of-organism gene-nickname organism 'genes-of-organism))

(defun protein-of-organism (protein-nickname organism)
  #.(one-string-nl
     "PROTEIN-NICKNAME is either a protein frame, in which case it is"
     "returned or a string or symbol which matches the part of a protein frame"
     "name found after the '.' -- so :pall4312 is a nickname for "
     "#$A7120.pall4312."
     "All the proteins of ORGANISM are searched until the nickname matches"
     "and the protein so matched is returned, or NIL if no match is found.")
  (entity-of-organism protein-nickname organism 'proteins-of-organism))

(defun contig-of-organism (contig-nickname organism)
  (entity-of-organism contig-nickname organism 'contigs-of-organism))

