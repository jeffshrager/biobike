;;; -*- mode: Lisp; Syntax: Common-Lisp; Package: bio; -*-

(in-package :aframes)

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


;;; Definition of KEGG.KeggNode frametype.

(def-frame-class 
 #$KEGG.KeggNode ()
 ((#$Kegg.accession.s :allocation :instance :initform nil)
  (#$Kegg.source.s :allocation :instance :initform nil)))

(def-frame-class 
 #$Kegg.Compound (#$Kegg.KeggNode)
 ((#$Kegg.name.s :allocation :instance :initform nil)
  (#$Kegg.formula.s :allocation :instance :initform nil)
  (#$Kegg.dblinks.s :allocation :instance :initform nil)
  (#$Kegg.reaction.s :allocation :instance :initform nil)
  (#$Kegg.enzyme.s :allocation :instance :initform nil)
  ))

(def-frame-class 
 #$Kegg.Glycan (#$Kegg.KeggNode)
 ((#$Kegg.composition.s :allocation :instance :initform nil)
  (#$Kegg.dblinks.s :allocation :instance :initform nil)
  (#$Kegg.mass.s :allocation :instance :initform nil)
  (#$Kegg.class.s :allocation :instance :initform nil)
  ))

(def-frame-class 
 #$Kegg.Enzyme (#$Kegg.KeggNode)
 ((#$Kegg.name.s :allocation :instance :initform nil)
  (#$Kegg.product.s :allocation :instance :initform nil)
  (#$Kegg.class.s :allocation :instance :initform nil)
  (#$Kegg.dblinks.s :allocation :instance :initform nil)
  (#$Kegg.substrate.s :allocation :instance :initform nil)
  (#$Kegg.reactstring.s :allocation :instance :initform nil)
  (#$Kegg.reference.s :allocation :instance :initform nil)
  (#$Kegg.sysname.s :allocation :instance :initform nil)
  (#$Kegg.comment.s :allocation :instance :initform nil)
  ))

(def-frame-class 
 #$Kegg.Reaction (#$Kegg.KeggNode)
 ((#$Kegg.lhs.s :allocation :instance :initform nil)
  (#$Kegg.equation.s :allocation :instance :initform nil)
  (#$Kegg.definition.s :allocation :instance :initform nil)
  (#$Kegg.rhs.s :allocation :instance :initform nil)
  (#$Kegg.enzyme.s :allocation :instance :initform nil)
  (#$Kegg.name.s :allocation :instance :initform nil)
  (#$Kegg.pathway.s :allocation :instance :initform nil)
  ))

#|
 
(defun analyze-kegg-group (group)
  (let ((group (remove-if-not (lambda (x) (eq group (#^Kegg.Source.s x)))
          *kegg-frames*))
      (hash (make-hash-table :test 'eq)))
    (loop for g in group do
      (for-each-frame-slot (slot value) g
        (incf (gethash slot hash 0))
        value
        ))
    (sort (hash-table-contents hash) '> :key 'second)
    ))


 (analyze-kegg-group :compound)
((#$KEGG.name.s 10785) (#$KEGG.accession.s 10785) (#$fName 10785)
 (#$isA 10785) (#$Source 10785) (#$Kegg.image.s 10785)
 (#$Kegg.Source.s 10785) (#$KEGG.formula.s 10071)
 (#$KEGG.dblinks.s 6433) (#$KEGG.reaction.s 4669)
 (#$KEGG.enzyme.s 4132) (#$KEGG.GOframe.s 3383) (#$KEGG.pathway.s 2650)
 (#$KEGG.glycan.s 193))

 (analyze-kegg-group :glycan)
((#$KEGG.accession.s 10222) (#$fName 10222) (#$isA 10222)
 (#$Source 10222) (#$Kegg.image.s 10222) (#$Kegg.Source.s 10222)
 (#$KEGG.composition.s 10221) (#$KEGG.dblinks.s 9313)
 (#$KEGG.mass.s 6181) (#$KEGG.class.s 6162) (#$KEGG.name.s 1103)
 (#$KEGG.binding.s 742) (#$KEGG.enzyme.s 491) (#$KEGG.reaction.s 258)
 (#$KEGG.pathway.s 204) (#$KEGG.compound.s 194) (#$KEGG.ortholog.s 65)
 (#$KEGG.comment.s 43))

(analyze-kegg-group :enzyme)
((#$KEGG.product.s 4309) (#$KEGG.name.s 4309) (#$KEGG.accession.s 4309)
 (#$Kegg.link.s 4309) (#$fName 4309) (#$KEGG.class.s 4309) (#$isA 4309)
 (#$Source 4309) (#$KEGG.dblinks.s 4309) (#$Kegg.Source.s 4309)
 (#$KEGG.substrate.s 4309) (#$KEGG.reactstring.s 3806)
 (#$KEGG.reference.s 3806) (#$KEGG.sysname.s 3435)
 (#$KEGG.comment.s 3300) (#$KEGG.pathway.s 1947) (#$KEGG.genes.s 1728)
 (#$KEGG.ortholog.s 1264) (#$KEGG.motif.s 1044) (#$KEGG.disease.s 662)
 (#$KEGG.structures.s 606))

(analyze-kegg-group :reaction)
((#$KEGG.accession.s 5904) (#$fName 5904) (#$KEGG.lhs.s 5904)
 (#$isA 5904) (#$Source 5904) (#$Kegg.image.s 5904)
 (#$KEGG.equation.s 5904) (#$KEGG.definition.s 5904)
 (#$KEGG.rhs.s 5904) (#$Kegg.Source.s 5904) (#$KEGG.enzyme.s 5319)
 (#$KEGG.name.s 4484) (#$KEGG.pathway.s 3991) (#$KEGG.comment.s 786)
 (#$KEGG.compound.s 332))

|#

