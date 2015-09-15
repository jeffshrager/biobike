(in-package :pb)

;;; This has to be loaded into the running Trouble server and
;;; excecuted.  It does a major reorganization of the knowledge 
;;; base.

(defvar *pub-id->frame* (make-hash-table :test #'equal))
(defvar *author-id->frame* (make-hash-table :test #'equal))

(defvar *temp-table* (make-hash-table :test #'equal))

(defvar *pdb* nil) ; Server connection

(defvar *year-frames* nil)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(*year-frames*)))

(defun reorg-and-thread-trbl (&key (minyear 1991) (reload? nil) (authors? t))
  (when reload?				; Get the descriptions??
    (load-organism :arabidopsis_thaliana :descriptions? t :reload? t))
  (setq *pdb* (make-db-server-connection 
	       :mysql :dbname "isipubtest" 
	       :cl-user "trbl" :file "/var/lib/mysql/mysql.sock"))
  (cformatt "Doing base load")
  (rearrange-base-load)
  (cformatt "Doing publications.")
  (arrange-publications minyear)
  (when authors? 
    (cformatt "Doing authors")
    (arrange-authors))
  (misc-final-setup)
  (clrhash *temp-table*)
  (cformatt "Reorg and thread complete.")
  )

(defun rearrange-base-load ()
  ;; The #^genes are actually gene models; For each we create a
  ;; locus object which holds onto the positions, and the genes point
  ;; to the loci.  The loci get stored in a hash table, and then
  ;; stuck into the chromosomes.
  (loop for cs in (#^Contiguous-Sequences #$arabidopsis_thaliana)
	do 
	(clrhash *temp-table*)
	(loop for gene-model across (#^Genes-Sorted-by-Position cs)
	      as from = (#^from gene-model)
	      as to = (#^to gene-model)
	      as locusid = (#^locus-id gene-model)
	      as locus = (gethash locusid *temp-table*)
	      do (when (null locus)
		   (setq locus (frame-fnamed (format nil "Gene.~a" locusid) t))
		   (setf (gethash locusid *temp-table*) locus)
		   (setf (#^from locus) from)
		   (setf (#^to locus) to)
		   (setf (#^Contiguous-Sequence locus) cs)
		   (push locus (#^loci #$arabidopsis_thaliana))
		   )
	      (push gene-model (#^gene-models locus))
	      (setf (#^locus gene-model) locus))
	;; Now jam the loci back into the chromosome
	(setf (#^loci cs) 
	      (lmaphash 
	       #'(lambda (k v) (declare (ignore k)) v) 
	       *temp-table*))
	)
  ;; Reset the gene and gene-models for the organism, and whack the loci!
  (setf (#^gene-models #$arabidopsis_thaliana) (#^genes #$arabidopsis_thaliana))
  (setf (#^genes #$arabidopsis_thaliana) (#^loci #$arabidopsis_thaliana))
  ;; Now we can thread the Aracyc model into the loci.  More could be done here.
  ;; (This should go into the postload for aracyc!)
  (mapframes #'(lambda (frame)
		 (when (eq :ocelot (#^source frame))
		   (let* ((gene (#^oc.gene frame))
			  (cname (and gene (isframe? gene) (#^oc.common-name gene)))
			  (locus (frame-fnamed (format nil "Gene.~a" cname))))
		     (when locus 
		       (setf (#^gene gene) locus)
		       (setf (#^oc.gene locus) gene)
		       (setf (#^oc.dblinks gene)
			     (cons locus (remove gene (#^oc.dblinks gene)))))
		     ))))
  ;; Remove remnant nils from the GO-ID slots which were left from 
  ;; postload go-id conversion where no go concept was found. These nils
  ;; screw everyone downsteam.
  (loop for gene-model in (#^gene-models #$arabidopsis_thaliana)
	do (setf (#^go-id gene-model) 
		 (remove-duplicates (remove nil (#^go-id gene-model)))))
  )

(defun arrange-publications (minyear)
  (clrhash *pub-id->frame*)     ; This will be used to facilitate author binding
  (loop for (string-year title gene pub-gene-id pub-id nil)
	in (xsql *pdb* 
                 (one-string 
                  "select all publication.year, publication.title, "
                  "genes.gene, genes.pub_gene_id, gene_match.pub_id, "
                  "gene_match.gene_id from publication, genes, gene_match "
                  "where genes.gene_id = gene_match.gene_id and "
                  "publication.pub_id = gene_match.pub_id;"))
	as gene-frame = (frame-fnamed (format nil "At.~a" gene)) ; Could be nil if the gene isn't in TAIR
	as year = (parse-integer string-year)
	when (>= year minyear)
	do 
	(let ((pub-frame (frame-fnamed (format nil "Pub.~a" pub-id) t)))
	  (when gene-frame (push pub-frame (#^pubs gene-frame)))
	  ;; Set up all the info on the publication frame (which may already exist).
	  (push gene (#^gene-names pub-frame))
	  (if gene-frame 
	      (push gene-frame (#^genes pub-frame))
	    (push gene (#^genes-not-found pub-frame)))
	  (setf (#^year pub-frame) year)
	  (setf (#^pub-id pub-frame) pub-id)
	  (setf (#^pub-gene-id pub-frame) pub-gene-id)
	  (setf (#^title pub-frame) title)
	  (setf (gethash pub-id *pub-id->frame*) pub-frame)
	  ))
  ;; Create year frames containing pubs for that year.
  (setq *year-frames* nil)
  (for-all-frames 
   (f) 
   (when (initial-subsequence-of? (#^fname f) "Pub.")
     (let* ((year (#^year f))
	    (yf (frame-fnamed (formatn "Year.~a" year) t)))
       (setf (#^year yf) year)
       (pushnew yf *year-frames* :test #'eq)
       (push f (#^pubs yf)))))
  ;; Sort them into chron order:
  (setq *year-frames* (sort *year-frames* #'< :key #'#^year))
  )

(defun arrange-authors ()
  (clrhash *author-id->frame*)
  (loop for (author-id pub-id author-name)
	in (xsql *pdb*
                 (one-string 
                  "select author.author_id, publication.pub_id, "
                  "author.author_name from author, map_auth, "
                  "publication where publication.pub_id = map_auth.pub_id "
                  "and map_auth.author_id = author.author_id"))
	as auth-frame = (frame-fnamed (format nil "Author.~a" author-id) t)
	as pub-frame = (gethash pub-id *pub-id->frame*)
	do 
	(setf (gethash author-id *author-id->frame*)
	      auth-frame)
	(setf (#^author-id auth-frame) author-id)
	(setf (#^name auth-frame) author-name)
	(when pub-frame
	  (push pub-frame (#^pubs auth-frame))
	  (push auth-frame (#^authors pub-frame)))
	)
  ;; Assign PI author frames to each publication and v.v.
  (let ((aids/piids  
	 (xsql pb::*pdb* "select author.author_id, auth2api.pi_id from author, auth2api where author.author_id = auth2api.author_id "))
	(pubids/pidds (xsql pb::*pdb* "select arpiTpubs.pub_id, arpiTpubs.pi_id from arpiTpubs"))
	)
    (clrhash *temp-table*)
    (loop for (aiid piid) in aids/piids  
	  do (setf (gethash piid *temp-table*)
		   (gethash aiid *author-id->frame*)))
    (loop for (pubid piid) in pubids/pidds
	  as auth-frame = (gethash piid *temp-table*)
	  as pub-frame = (gethash pubid *pub-id->frame*)
	  when (and auth-frame pub-frame)
	  do 
	  (setf (#^pi pub-frame) auth-frame)
	  (push pub-frame (#^pid-pubs auth-frame)))
    )
  )

(defun misc-final-setup ()
  ;; Create the parent path in each GO frame.
  (loop for frame in *go-frames*
	do (setf (#^parents frame)
		 (frame->related-and-containing-frames frame #$isa))))
