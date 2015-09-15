;; -*- mode: Lisp; Syntax: Common-Lisp; Package: biolisp; -*-

(in-package :bio)

;;; +=========================================================================+
;;; | Copyright (c) 2002-2006 by The BioBike Team                             |
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

;;; Useful prefix translation tables (Maybe move these to someplace better? FFF)

;;; These are prefixes in two different senses. There's the organism
;;; prefix (s6803), and the gene prefixes (slr, sll, etc). Kind of
;;; confusing that they're the same name!

(defvar *gene-prefix->organism* (make-hash-table :test #'equalp)) ; from ssl slr, etc. -> org
(defvar *organism->prefix* (make-hash-table :test #'equalp)) ; what to add (prefix) for an given organism

;;; Need to know that slr, sll etc. are from s6803, etc. in order to
;;; get back to the organism and its gene from the blast match. UUU

(defun setup-prefix-tables ()
  (clrhash *gene-prefix->organism*)
  (loop for org in *loaded-organisms*
        do (loop for gene in (#^genes org)
                 as fname = (#^fname gene)
                 as dotpos = (position #\. fname)
                 as gname = (subseq fname (1+ dotpos))
                 as prefix = (subseq gname 0 3)
                 do (setf (gethash prefix *gene-prefix->organism*)
                      org)))
  (clrhash *organism->prefix*)
  (loop for org in *loaded-organisms*
        as gname = (#^fname (first (#^genes org)))
        as prefix = (subseq gname 0 (position #\. gname))
        do (setf (gethash org *organism->prefix*)
             prefix)))

;;; Pwy initialization:

;;; Setup pathways models (pwy.) in the knowledge based. These are
;;; linking concepts that unify GO and Kegg (and eventually biocyc)
;;; objects. Pathways are initially defined by a KEGG pathway map, and
;;; takes its name from there, but unifies a great deal of random
;;; information from other sources, each of which has (I hope)
;;; pointers into the pwy objects as well.

(defun initpwys ()
  (setq *pwys* nil)
  (loop for entry in  
	(mapframes 
	 #'(lambda (frame)
	     (loop for kps in (#^KEGG.pathway.s frame)
		   when (search "PATH:" kps)
		   collect 
		   (let* ((name (second (string-split kps #\space)))
			  (pwy (frame-fnamed (format nil "pwy.~a" name) t)))
		     (setf (#^description pwy) (subseq kps (+ 8 (length name)))) ; remove "PATH:_XXXXXX_"
		     (setf (#^kegg.pwy-id pwy) name)
		     (setf (#^pwy-no pwy) (parse-integer (subseq name (- (length name) 5))))
		     (setf (#^pwy-type pwy) (keywordize (subseq name 0 (- (length name) 5))))
		     (pushnew frame (#^keggobjs pwy))
		     (pushnew pwy (#^pwys frame))
		     pwy))))
	do (loop for p in entry
		 do (pushnew p *pwys*)))
  (thread-kegg-ecs)
  (backthread-ec-to-go)
  )

;;; Thread KEGG EC numbers into #$EC. frames.

(defun thread-kegg-ecs ()
  (mapframes 
   #'(lambda (kegg.frame)
       (let ((KEGG.accession.s (#^KEGG.accession.s kegg.frame)))
	 (when (and (stringp KEGG.accession.s) (equal 0 (search "EC" KEGG.accession.s)))
	   (let ((ec.frame (frame-fnamed (format nil "EC.~a" (subseq KEGG.accession.s 2)) t)))
	     (setf (#^ec.frame kegg.frame) ec.frame)
	     (setf (#^kegg.ec.frame ec.frame) kegg.frame)))))))

;;; Reverse thread the EC frames to their GO frames cognates:

(defun backthread-ec-to-go ()
  (mapframes 
   #'(lambda (go.frame)
       (let ((ec.frames (#^GO.ECRef go.frame)))
	 (when ec.frames
	   (loop for ec.frame in ec.frames
		 do (pushnew go.frame (#^go.s ec.frame))))))))


;;; Pwy tools:

(defun go->pwys (go.frame)
  "Given a GO. frame, return a list of unique pathway (pwy. frames) referred to by that GO, via its EC reference."
  (remove-duplicates 
   (let ((GO.ECRefs (#^GO.ECRef go.frame)))
     (when GO.ECRefs
       (loop for GO.ECRef in GO.ECRefs
	     as kegg.ec.frame = (#^kegg.ec.frame GO.ECRef)
	     when kegg.ec.frame
	     append (#^pwys kegg.ec.frame))))))
 
(defun mol->pwys (mol)
  "Return a list of the unique pathways (pwy. frames) that a MOL. object belongs to by tracking to the #^pwys slot of the kegg C---- object that the MOL. refers to."
  (remove-duplicates 
   (let ((GO.KEGGFrame.s (#^GO.KEGGFrame.s mol)))
     (if GO.KEGGFrame.s
	 (loop for GO.KEGGFrame in GO.KEGGFrame.s
	       append (#^pwys GO.KEGGFrame))))))
