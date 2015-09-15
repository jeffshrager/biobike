;;; -*- mode: Lisp; Syntax: Common-Lisp; Package: biolisp; -*-

(in-package :bio)

;;; +=========================================================================+
;;; | Copyright (c) 2002-2005 JP Massar, Jeff Shrager, Mike Travers           |
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

;;; Author:  Jeff Shrager.

;;; Code to translate NCBI XML-format genomes into the necessary fasta files
;;; and tables for organism upload.  Note that at the moment this is limited
;;; to organisms that have exactly ONE plasmid!  You need to create the file. 
;;; This is NOT loaded
;;; into the biolingua system automatically; You have to manually load it when
;;; you want to do this particular operation.  
;;; See the HOW TO Doc for instructions.

(require :pxml)

;;; This uses a million globals for no good reason at all execpt that
;;; it was useful in debugging and saves having to pass them all over
;;; the world. 

(defvar *positiontbl* (make-hash-table :test #'equal))

(defvar *org-name*)
(defvar *org-dir*)

;;; JP's going to kill me for this:
(defparameter *org-top-dir* 
  (format nil "~a" (cl-user:translate-simple-lp "bioetc:data;")))

(defvar *ncbixmlfile-name*)
(defvar *ncbilispfile-name*)
(defvar *genome-dir*)
(defvar *genes-dir*)
(defvar *proteins-dir*)

;;; Usage, once you have the directoies in place should be just:
;;; e.g.: (translate-ncbi-organism-xml "pro1375") 

(defvar *ncbi-xml*)

(defun translate-ncbi-organism-xml (oname) 
  (flet ((find-or-create-dir 
	  (name)
	  (if (probe-file name) 
	      (format t "Used previously existing ~a directory.~%" name)
	    (progn
	      (format t "Creating ~a directory.~%" name)
	      (EXCL:MAKE-DIRECTORY name)))))
    (setq *org-name* oname)
    ;; This one must exist:
    (setq *org-dir* (format nil "~a/~a" *org-top-dir* *org-name*))
    ;; The rest of these are create if they don't exist:
    (find-or-create-dir (setq *genome-dir* (format nil "~a/genome" *org-dir*)))
    (find-or-create-dir (setq *genes-dir* (format nil "~a/genes" *org-dir*)))
    (find-or-create-dir 
     (setq *proteins-dir* (format nil "~a/proteins" *org-dir*)))
    )
  (import-xml-from-ncbi)
  (let ((doctype (cadr (assoc :doctype *ncbi-xml*))))
    (case doctype
      (:|Bioseq-set|
	(build-xml-position-table)
	(create-chromosome)
	(create-protein-and-gene-files-from-xml)
	)
      (t (error "Sorry, XML DOCTYPE ~a isn't supported yet." doctype)))
    ))

(defun import-xml-from-ncbi ()
  (setq *NCBIXMLfile-name* (format nil "~a/~a.xml" *ORG-dir* *ORG-name*))
  (setq *NCBILISPfile-name* (format nil "~a/~a.lisp" *ORG-dir* *ORG-name*))
  (cond 
   ((probe-file *NCBILISPfile-name*)
    (format t "Loading NCBI XML from previously-created ~a.  To refresh it you must delete it first.~%" *NCBILISPfile-name*)
    (load *NCBILISPfile-name*))
   (t (format t "Creating ~a from ~a~%" *NCBIXMLfile-name* *NCBILISPfile-name*)
      (with-open-file (o *NCBILISPfile-name* :direction :output :if-exists :supersede)
        (with-open-file (i *NCBIXMLfile-name*)
          (format o "(setq *ncbi-xml* '~%")
          (print (setq *ncbi-xml* (dclean-xml (NET.XML.PARSER:PARSE-XML i))) o)
          (format o "~%)~%")
          )))))

;;; Destructively removes from the XML anything that has only spaces, 
;;; tabs, and newlines, and/or CRs.
;;; replacing them with :X to minimize storage space.

(defun dclean-xml (xml)
  (cond 
   ((or (atom xml) (null xml) (stringp xml) (numberp xml)) xml)
   ((listp xml)
    (mapcar 
     (lambda (item)
       (cond 
        ((and (stringp item)
              (loop for char across item
                    unless (member char '(#\tab #\space #\newline #\return) 
                                   :test #'char-equal)
                    do (return nil)
                    finally (return t)))
         :x)
        (t (dclean-xml item))))
            xml))
   (t xml)))
			   
;;; Find the sequence objects of type "dna" or "aa" in the XML jumble.

(defun ncbi-objects-of-type (type)
  (loop for entry in (bio::extract-headed-lists '|Bioseq| *ncbi-xml*)
	when (string-equal type (car (xmlget '|Seq-inst_mol| entry)))
	collect entry))

;;; This will always return a list of values, even if there's only one
;;; member, because there COULD be multiple instances in the
;;; structure.  WWW Caller has to process the list as desired (often
;;; just taking the CAR).

(defun xmlget (label xml)
  (mapcar #'third (bio::extract-headed-lists label xml)))

;;; This is hacked to handle a SINGLE circular chromosome, called "CHROMOSOME"
;;; Some day we'll make it handle multiple ones.  The seq is in a bunch
;;; of pieces that we have to piece together.

(defun create-chromosome ()
 (with-open-file (fa (format nil "~a/genome.fasta" *genome-dir*) 
		    :direction :output :if-exists :supersede)
   (with-open-file (gtbl (format nil "~a/genome.tbl" *genome-dir*) 
			:direction :output :if-exists :supersede)
     (format gtbl "NAME	CIRCULAR~%")
     (format gtbl "CHROMOSOME	T~%")
     (format fa ">chromosome~%") ; FFF Won't work for multiple plasmids
     (loop for gpart in (bio::extract-headed-lists '|IUPACna| (ncbi-objects-of-type "dna"))
	   as k from 1 by 1
	   as seq = (second gpart)
	   do (format fa "~a" seq)
	   (format t "Wrote ~a bytes to the genome fasta~%" (length seq))
	   )
       (format fa "~%")
       )))

(defun create-protein-and-gene-files-from-xml ()
 (with-open-file (fa (format nil "~a/proteins.fasta" *proteins-dir*) 
		    :direction :output :if-exists :supersede)
   (with-open-file (ptbl (format nil "~a/proteins.tbl" *proteins-dir*) 
			:direction :output :if-exists :supersede)
     (format ptbl "NAME	GENE~%")
     (with-open-file (gtbl (format nil "~a/genes.tbl" *genes-dir*) 
			   :direction :output :if-exists :supersede)
       (format gtbl "NAME	GENOME-COMPONENT	ENCODES-PROTEIN	DIRECTION	START-UNKNOWN	END-UNKNOWN	FROM	TO	ARCHITECTURE	BEST-HIT-ACCESSION	BEST-HIT-EVALUE	GENE-NAME	ANNOTATION	BEST-HIT-ID-PCT	BEST-HIT-DESCR	EC-ID	GO-ID	COG-ID	TRANSMEMBRANE-REGIONSNAME	GENE~%")
       (flet ((pget (tag prot) (cadar (bio::extract-headed-lists tag prot))))
	 (loop for protein in (ncbi-objects-of-type "aa")
	     as name = (or (pget '|Object-id_str| (bio::extract-headed-lists '|Dbtag_tag| protein))
			   (cadar (bio::extract-headed-lists '|Textseq-id_accession| protein)))
	     as pname = (format nil "p-~a" name)
	     as sequence = (or (pget '|IUPACaa| protein)
			       (pget '|NCBIeaa| protein))
	     as full-name = (pget '|Prot-ref_name_E| protein)
	     as description = (pget '|Seqdesc_title| protein)
	     as strand = (xmlget '|Na-strand| protein)
	     as ec = (pget '|Prot-ref_ec_E| protein)
	     as (from . to) = (gethash name *positiontbl*)
	     do 
	     (format fa ">~a~%~A~%" pname sequence)
	     (format gtbl 
		     "~a	~a	~a	~a	~a	~a	~a	~a	~a	~a	~a	~a	~a	~a	~a	~a	~a	~a	~a~%" 
		     name 
		     "chromosome" ; FFF eventually fix!
		     t ; ENCODES-PROTEIN
		     (if (string-equal "plus" (car strand)) 'f 'b) 
		     'f ; START-UNKNOWN
		     'f ; END-UNKNOWN
		     from
		     to
		     '- ; ARCHITECTURE
		     '- ; BEST-HIT-ACCESSION 
		     '- ; BEST-HIT-EVALUE 
		     full-name 
		     description 
		     '- ; BEST-HIT-ID-PCT 
		     description 
		     ec 
		     '- ; GO-ID 
		     '- ; COG-ID 
		     '- ;TRANSMEMBRANE-REGIONSNAME
		     )
	     (format ptbl "~a	~A~%" pname name)
	     ))))))
	
(defun build-xml-position-table ()
  (clrhash *positiontbl*)
  ;; The NCBI seems to be zero-origin, so we have to correct for that.
  (flet ((xiget (key xml) 
           (1+ (parse-integer (cadar (bio::extract-headed-lists key xml))))))
    (loop for feat in (bio::extract-headed-lists '|Seq-feat| *ncbi-xml*)
	  as name = (cadar (bio::extract-headed-lists '|Gene-ref_locus-tag| feat))
	  when name
	  do (let ((from (xiget '|Seq-interval_from| feat))
		   (to (xiget '|Seq-interval_to| feat))
		   )
	       (setf  (gethash name *positiontbl*)  (cons from to))))
    ))

;;; There's a whole other world of NCBI .gbk files, which are
;;; intensely annoying to deal with, but such is life; the XML seems
;;; to be incomplete; Ugh!

(defun translate-ncbi-organism-gbk (oname) 
  (flet ((find-or-create-dir 
	  (name)
	  (if (probe-file name) 
	      (format t "Used previously existing ~a directory.~%" name)
	    (progn
	      (format t "Creating ~a directory.~%" name)
	      (EXCL:MAKE-DIRECTORY name)))))
    (setq *org-name* oname)
    ;; This one must exist:
    (setq *org-dir* (format nil "~a/~a" *org-top-dir* *org-name*))
    ;; The rest of these are create if they don't exist:
    (find-or-create-dir (setq *genome-dir* (format nil "~a/genome" *org-dir*)))
    (find-or-create-dir (setq *genes-dir* (format nil "~a/genes" *org-dir*)))
    (find-or-create-dir 
     (setq *proteins-dir* (format nil "~a/proteins" *org-dir*)))
    )
  (import-gbk-from-ncbi)
  (build-gbk-locus-tag-table)
  (create-chromosome)
  (create-protein-and-gene-files-from-gbk)
  )

(defvar *ncbi-gbk*)
(defvar *NCBIGBKFILE-NAME*)

(defun import-gbk-from-ncbi ()
  (setq *NCBIGBKfile-name* (format nil "~a/~a.gbk" *ORG-dir* *ORG-name*))
  (setq *NCBILISPfile-name* (format nil "~a/~a.lisp" *ORG-dir* *ORG-name*))
  (cond 
   ((probe-file *NCBILISPfile-name*)
    (format t "Loading NCBI GBK from previously-created ~a.  To refresh it you must delete it first.~%" *NCBILISPfile-name*)
    (load *NCBILISPfile-name*))
   (t (format t "Creating ~a from ~a~%" *NCBIGBKfile-name* *NCBILISPfile-name*)
      (with-open-file (o *NCBILISPfile-name* :direction :output :if-exists :supersede)
        (with-open-file (i *NCBIGBKfile-name*)
          (format o "(setq *ncbi-gbk* '~%")
          (print (setq *ncbi-gbk* (import-gbk i)) o)
          (format o "~%)~%")
          )))))

(defun import-gbk (i)
  ;; Skip header junk:
  (loop for line = (read-line i nil nil)
	if (null line) 
	do (error "GBK file ended prematurely!")
	else if (and (> (length line) 8)
		       (string-equal "FEATURES" (subseq line 0 8)))
	do (return t))
  ;; Read all the feature lines:
  (let ((lines
	 (loop for line = (read-line i nil nil)
	       until (or (null line)
			 (and (> (length line) 6)
			      (string-equal "ORIGIN" (subseq line 0 6))))  
	       collect line)))
    ;; Now parse the features into lisp structures:
    (prog (features current-feature line)
	  :next-line
	  (setq line (pop lines))
	  (when (or (null line)
		    (and (> (length line) 5)
			 (not (char-equal #\space (elt line 5)))))
	    (go :store-feature))
	  (push (subseq line 21) current-feature)
	  (go :next-line)
	  :store-feature
	  (push (post-process-feature (reverse current-feature)) features)
	  (when (null line) (return features))
	  (setq current-feature
		(list (parse-feature-location (subseq line 21))
		      (keywordize (string-trim " " (subseq line 0 20)))))
	  (go :next-line)
	  )))

(defun post-process-feature (feature)
  (compress-extended-translations (translate-features-=items feature)))

#| 

Need to compress this stupidity:

                     /translation="MKLDYEHVTDLGELLDSDTIIQINKQFFDSENVDYEETPDSPQD
                     IERVRDARLERLTTEFPRFEPLANQLAHIVRTFCGHHLFPDANHRTGTHIADMLAKKQ
                     GYDLFSLIQQDTDGIRRAVELSKILRGLCSNVRNSIDYLWMKDELFYHWNRYFRDLLY
                     DLSPQKRVHPDTGECQYENLTSNERISLIYKFAILETAEMRDALSDYRFE"

Which we get like this:

 (:CDS (:LOCATION (:START 2007840) (:END 2009699))
  (:LOCUS_TAG "VNG2677H") (:NOTE "hypothetical protein")
  (:CODON_START "1") (:TRANSL_TABLE "11") (:PRODUCT "Vng2677h")
  (:PROTEIN_ID "AAG20700.1") (:DB_XREF "GI:10582053")
  (:TRANSLATION "MTRTPSLTTTLRATAVSILVVAVVCTPAAVTVAAATDHAPTAAA")
  "GAPPESPSSSTDDAGWQHTEHTRPAGDIVSLSLTLPDRIPNDHPVYIHVGGADAGFLD"
  "VVRAVDTDADGTITLGINTRTLGTSTSLSPTNTENVYYSGGDTVTSGVHGRLGGDPGP"
  "RFVGPDGTQLDGFADYLAAVGLIRSADRERPTAQLTRPLQPGTYTVTATTNRTISAQA"
  "RTDGATTALTSSPADGGFDTATVSLTTPGVHNITVNTAPAGAADAAPNATALAASMTD"
  "QRPPTTTDRVVIAANATGIYGHLAAIAGGPDALASGVAPSTLSTLEARTGEGVQFAVE"
  "ATNSPVVKGPDSPPGIESPLEILDLSGADPAAASVYANASRGRLYVVVDPQAVADFQP"
  "SSGTDFQNLSASLTYETDPADAFQFDRGDNNIQTYYRSLLGGAGGDITIPAFPYLAPG"
  "TNETASDDFQVVTPRTRFDLRADSGDGRLLTRAQPTHVSGVTNLAPGTEATVRVSFAE"
  "SNGSFGYIQRSTATVAANGTFTGTVDLPVPTVGETVTLTMFVADDTVAHTTATVTDPT"
  "RTTTAATTTPETQSETTTTSRETGGPTPGFTAVGALVAVVIVFAGVGLRRRRE\"")
                                                        ^^ Note extra quote! 

Into this:

  (:TRANSLATION "MTRTPSLTTTLRATAVSILVVAVVCTPAAVTVAAATDHAPTAAAGAPPESPSSSTDDAGWQHTEHTRPAGDIVSLSLTLPDRIPNDHPVYIHVGGADAGFLD...")

|#

(defun compress-extended-translations (feature)
  (loop as item+ on feature
	as head = (car item+)
	if (stringp head)
	do (null head) ; This ignores these -- they'll have been up-taken already (I hope)
	else if (and (listp head) (eq :translation (car head)))
	collect (list (car head)
		      (apply #'s+ 
			     (cons (second head)
				   (loop for next-string in (cdr item+)
					 until (not (stringp next-string))
					 collect (string-trim "\"" next-string)))))
	else collect head))

(defun parse-feature-location (string)
  "Understands start..stop and complement(start..stop); Anything else warns out and returns the string."
  (cond ((and (> (length string) 10)
	      (string-equal "complement" (subseq string 0 10)))
	 (let ((s/e (parse-feature-start-stop (subseq string 11 (1- (length string))))))
	   (cond ((listp s/e)
		  `(:location (:start ,(cdr s/e))
			      (:end ,(car s/e))))
		 (t s/e))))
	(t 
	 (let ((s/e (parse-feature-start-stop string)))
	   (cond ((listp s/e)
		  `(:location (:start ,(car s/e))
			      (:end ,(cdr s/e))))
		 (t s/e))))))

(defun parse-feature-start-stop (string)
  "Understands only: start..stop; Anything else warns out and returns the string"
  (or (ignore-errors 
	(let ((dots (search ".." string)))
	  (cons (parse-integer (subseq string 0 dots))
		(parse-integer (subseq string (+ 2 dots))))))
      (progn (warn "The string ~s couldn't be parsed as start..stop!" string)
	     string)))

(defun translate-features-=items (feature)
  (loop for item in feature
	collect (cond ((not (stringp item))
		       item)
		      ((char-equal #\/ (elt item 0))
		       (let ((=pos (position #\= item)))
			 (list (keywordize (subseq item 1 =pos))
			       (string-trim "\"" (subseq item (1+ =pos))))))
		      (t item))))

(defvar *locus-tag->features* (make-hash-table :test #'equal))

(defun build-gbk-locus-tag-table ()
  (clrhash *locus-tag->features*)
  (loop for entry in *ncbi-gbk*
	as (nil . data) = entry
	as tag = (assocadr :locus_tag data)
	do (push entry (gethash tag *locus-tag->features*))))
	
(defun create-protein-and-gene-files-from-gbk ()
  (with-open-file 
   (fa (format nil "~a/proteins.fasta" *proteins-dir*) :direction :output :if-exists :supersede)
   (with-open-file 
    (ptbl (format nil "~a/proteins.tbl" *proteins-dir*) :direction :output :if-exists :supersede)
    (format ptbl "NAME	GENE~%")
    (with-open-file 
     (gtbl (format nil "~a/genes.tbl" *genes-dir*) :direction :output :if-exists :supersede)
     (format gtbl "NAME	GENOME-COMPONENT	ENCODES-PROTEIN	DIRECTION	START-UNKNOWN	END-UNKNOWN	FROM	TO	ARCHITECTURE	BEST-HIT-ACCESSION	BEST-HIT-EVALUE	GENE-NAME	ANNOTATION	BEST-HIT-ID-PCT	BEST-HIT-DESCR	EC-ID	GO-ID	COG-ID	TRANSMEMBRANE-REGIONSNAME	GENE~%")
     (loop for locus-tag being the hash-keys of *locus-tag->features*
	   using (hash-value features)
	   as gene-data = (assocdr :gene features)
	   as cds-data = (assocdr :cds features)
	   as data = (append gene-data cds-data)
	   as name = (or (assocadr :gene data) 
			 (assocadr :locus_tag data))
	   as sequence = (assocadr :translation data)
	   as full-name = (assocadr :PROTEIN_ID data)
	   as pname = (format nil "p-~a" full-name)
	   as description = (assocadr :PRODUCT data)
	   as ec = "?"
	   as s/e = (assocdr :location data)
	   as from = (assocadr :start s/e)
	   as to = (assocadr :end s/e)
	   as strand = (if (< from to) 
			   "plus"
			 (let ((temp from))
			   ;; The location has to be re-reversed, and the strand tagged instead.... or not?
			   (setq from to to temp)
			   "minus"))
	   do 
	   (format fa ">~a~%~A~%" pname sequence)
	   (format gtbl 
		   "~a	~a	~a	~a	~a	~a	~a	~a	~a	~a	~a	~a	~a	~a	~a	~a	~a	~a	~a~%" 
		   name 
		   "chromosome"		; FFF eventually fix!
		   t			; ENCODES-PROTEIN
		   (if (string-equal "plus" strand) 'f 'b) 
		   'f			; START-UNKNOWN
		   'f			; END-UNKNOWN
		   from
		   to
		   '-			; ARCHITECTURE
		   '-			; BEST-HIT-ACCESSION 
		   '-			; BEST-HIT-EVALUE 
		   full-name 
		   description 
		   '-			; BEST-HIT-ID-PCT 
		   description 
		   ec 
		   '-			; GO-ID 
		   '-			; COG-ID 
		   '-			;TRANSMEMBRANE-REGIONSNAME
		   )
	   (format ptbl "~a	~A~%" pname name)
	   )))))

; (translate-ncbi-organism-gbk "halonrc1")
