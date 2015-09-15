;;; -*- Package: biolisp; mode: lisp; base: 10; Syntax: Common-Lisp; -*-

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

;;; Read KGML (Kegg pathway files) into Frames. 
;;; ex: (read-kegg-file "~/sourceforge/BioLisp/data/kegg/map00010.xml")
;;; where the file is downloaded from 
;;; http://www.genome.ad.jp/kegg/KGML/KGML_v0.3/map/map00010.xml
;;; Mike Travers 3/2004

#| 

For documentation of the XML format, see 
http://www.genome.ad.jp/kegg/xml/DraftSpecification.html

Unfortunately the available files only describe the pathways and not
their constituents! That is, they are described in terms of things
like C00469, but the fact that this is ethanol is not available in the
XMl files. You can get HTML descriptions of any term (ie:
  http://www.genome.ad.jp/dbget-bin/www_bget?compound+C00469 so it
would be possible to screen-scrape the knowledge base, if we are truly
desperate.

So that's what I did, see kegg-jit.lisp. Note that because it turns
out the pathway XML files are not very useful, these two methods are
not yet really integrated. Use the keggg-jit stuff for now.

|#

;;; Read a single KGML file that describes a single "pathway".
(defun read-kegg-file (file)
  (let ((lxml (with-open-file (p file)
		(parse-xml p)))
;	(id->frame-ht (make-hash-table :test #'eql))
	pathframe)
    ;;; pick out the pathway element, the only interesting one.
    (setf lxml (lxml-subnode lxml '|pathway|))
    (setf pathframe
      (def-frame (make-fname (format nil "~APath.~A" *kegg-prefix* (lxml-node-att lxml '|title|)))
	  #$isA '(#$KEGGPathway)))
    ;; +++ could pull out other path attributes
    (dolist (node (cdr lxml))
      (case (lxml-node-type node)
	((nil)				;a string
	 )
	(|entry|
	 (let* (;(kid (lxml-node-att node '|id|))
		(kname (lxml-node-att node '|name|))
		(ktype (lxml-node-att node '|type|))
		(klink (lxml-node-att node '|link|))
		(frame (kegg-name->frame kname)))
	   (setf (slotv frame #$KEGG.link.s) klink)
;	   (setf (gethash (read-from-string kid) id->frame-ht) frame)
	   (case (intern ktype)
	     (|enzyme| 
	      (vwhen (att (lxml-node-att node '|reaction|))
		     (setf (slotv frame #$kreaction) att))
	      (push frame (slotv pathframe #$enzymes)))
	     (|compound|
	      (push frame (slotv pathframe #$compounds))
	      )
	     (|map|
		 (push frame (slotv pathframe #$maps))
		 )
	     (t (error "Unknown entry type ~A" ktype)))))
	(|reaction|
	 (let* ((kname (lxml-node-att node '|name|))
		(frame (kegg-name->frame kname)))
	   (dolist (subnode (lxml-subnodes node :elements))
	     (let ((compound-frame (kegg-name->frame (lxml-node-att subnode '|name|))))
	       (case (lxml-node-type subnode)
		 ;; using the same slots as GO, which has the side benefit of generating inverses...
		 (|substrate|
		  (push compound-frame (slotv frame #$reactants)))
		 (|product|
		  (push compound-frame (slotv frame #$products))))))
	   (push frame (slotv pathframe #$reactions))))
	(|relation|
	 )
	(t (error "Unknown node type ~A" (lxml-node-type node)))))
    pathframe))

(defun kegg-name->frame (name)
  (let ((frame (frame-fnamed (format nil "kegg:~a" name) t)))
    (unless (slotv frame #$isA) 
      ;; new frame, give it an identity
      (let ((prefix (car (string-split name #\:))))
	(setf (slotv frame #$isA)
	  (list 
	   (case (intern prefix)
	     (|ec| #$KEGGEnzyme)
	     (|cpd| #$KEGGCompound)
	     (|path| #$KEGGMap)
	     (|rn| #$KEGGReaction)
	     (t (error "Unclassifiable KEGG name ~A" name)))))))
    frame))
	  

;;; LXML utilities revived and updated from long ago

;;; Utilities for navigating an LXML structure
(defun lxml-node-type (lxml)
  (if (listp lxml)
      (if (listp (car lxml))
	  (car (car lxml))
	(car lxml))
    nil))

;;; +++ this probably doesn't work, see next fn
(defun lxml-subnode (lxml subnode-type)
  (find subnode-type lxml :key #'lxml-node-type))

;;; subnode-type can be an element symbol, or :elements for subelements only (no strings), or nil for everything
(defun lxml-subnodes (lxml &optional subnode-type)
  (cond ((null subnode-type)
	 (cdr lxml))
	((eq subnode-type :elements)
	 (remove-if-not #'listp (cdr lxml)))
	(t (remove-if-not #'(lambda (subnode)
			      (eq subnode-type (lxml-node-type subnode)))
			  (cdr lxml)))))

;; Go down several levels
(defun lxml-descend (lxml &rest subnode-types)
  (if (null subnode-types)
      lxml
    (apply #'lxml-descend (lxml-subnode lxml (car subnode-types)) (cdr
    subnode-types))))
			    
(defun lxml-node-att (node att-name)
  (cadr (member att-name (car node))))

;;; Translates from Kegg's table-based pathway models, directly downloaded 
;;; from ftp://ftp.genome.ad.jp/pub/kegg/ into a lisp-like format, which
;;; can then be turned into frames.

;;; Top-Directory should be given as without a final /, as: "c:/foo/bar"

(defun xlate-maps (&optional (topdir "c:/jshrager/biolispwork/map"))
  (labels ((lclfile (part1 part2) (print (format nil (format nil "~a/~a.~a" topdir part1 part2)))))
    (with-open-file (o (lclfile "maps" "lisp") :direction :output :if-exists :supersede)
      (format o "(setq *maps* '(~%")
      (loop for pathway-name in (loop for file in (directory (lclfile "map*" "cpd"))
				      collect (pathname-name file))
	    do 
	    (format o "(~s~%" pathway-name)
	    (loop for (ext . fn) in '(("cpd" . xlate-cpd)
				      ("enz" . xlate-enz)
				      ("rn" . xlate-rn))
		  as infile = (lclfile pathway-name ext)
		  do 
		  (format o "  (~s~%" ext)
		  (when (probe-file infile)
		    (with-open-file (i (lclfile pathway-name ext))
		      (funcall fn i o)))
		  (format o "  ~%)~%")
		  )
	    (format o "~%)~%")
	    )
      (format o "~%))~%")
      )))

(defun xlate-cpd (instream outstream)
  (loop for line = (read-line instream nil nil)
	until (null line)
	do (let ((parts (parse-kegg-line line)))
	     (print parts outstream))))
(defun xlate-enz (instream outstream)
  (loop for line = (read-line instream nil nil)
	until (null line)
	do (let ((parts (parse-kegg-line line)))
	     (print parts outstream))))
(defun xlate-rn (instream outstream)
  (loop for line = (read-line instream nil nil)
	until (null line)
	do (let ((parts (parse-kegg-line line)))
	     ;; If there's an enzyme, it's listed first.  So any reaction
	     ;; with no enzyme is only one long.
	     (print (list (first (first parts))
			  (when (rest (second parts))
			    (first (second parts)))
			  (parse-kegg-reaction 
			   (cond ((rest (second parts)) (second (second parts)))
				 (t (first (second parts)))))
			  )
		    outstream))))

(defun parse-kegg-line (line)
  (mapcar #'(lambda (part) 
	      (mapcar #'(lambda (string) (string-trim " " string)) 
		      (string-split part #\;)))
	  (string-split line #\tab)))

(defun parse-kegg-reaction (reaction)
    (let ((<=>pos (search "<=>" reaction)))
      (when (and <=>pos (> (length reaction) 5)) ; Protect against degenerate reactions.
	(list 
	 (+split-kegg-reaction-half (subseq reaction 0 (1- <=>pos)))
	 (+split-kegg-reaction-half (subseq reaction (+ <=>pos 4)))))))

(defun +split-kegg-reaction-half (part)
  (loop with start-pos = 0
	with mols = nil
	as +pos = (search " + " part :start2 start-pos)
	do (cond ((null +pos) 
		  (return (cons (subseq part start-pos) mols)))
		 (t (push (subseq part start-pos +pos) mols)
		    (setq start-pos (+ 3 +pos))))))
