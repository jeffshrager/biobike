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

#|


From organims (gene) to Ocelot reaction models:

Gene: #$S6803.slr1096
via the #^oc.polypeptide slot points to the active enzyme:
#$OC.Slr1096-Monomer ; Active enzyme (monomer)
which points to its enzymatic reaction:
#^OC.Catalyzes 	#$OC.Enzrxn-25 ; Enzymatic reaction internode
which points to the actual reaction:
#^OC.Reaction 	#$OC.Dihydlipoxn-Rxn
#$OC.Dihydlipoxn-Rxn ; The actual reaction
which points everywhere....

From ocelot models to organism (gene):

#$OC.Pyruvdehyd-Pwy
#^OC.Reaction-List ... #$OC.Dihydlipoxn-Rxn
#^OC.Enzymatic-Reaction 	#$OC.Enzrxn-25
#^OC.Enzyme 	#$OC.Slr1096-Monomer
#^oc.gene 	#$S6803.slr1096

If you wanted to from a pathway, like:
  #$OC.Pyruvdehyd-Pwy
to All the genes that implement it:

|#


(defun threading-test1 ()
  (ocelot-pathway-to-genes #$OC.Pyruvdehyd-Pwy))

(defun threading-test2 ()
   (ocelot-pathway-to-genes #$OC.Calvin-Pwy))

(defun threading-test3 ()
  (ocelot-pathway-to-genes #$OC.Pentose-P-Pwy))

(defun ocelot-basic-reaction-to-genes (reaction)
  (let ((genes nil))
    (loop for rxn in (ensure-list (#^OC.Enzymatic-Reaction reaction))
	  do (loop for monomer in (ensure-list (#^OC.Enzyme rxn))
		   do (loop for gene in (ensure-list (#^oc.gene monomer))
			    do (push gene genes))))
    genes))

(defun ocelot-pathway-to-genes (pathway)
  (remove nil
	  (flatten
	   (loop with sub-pathways = (#^OC.Sub-Pathways pathway)
		for reaction in (#^oc.reaction-list pathway)
		if (not (member reaction sub-pathways))
		collect (ocelot-basic-reaction-to-genes reaction)
		else collect (ocelot-pathway-to-genes reaction)))))
