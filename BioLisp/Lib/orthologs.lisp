;;; -*- mode: Lisp; Syntax: Common-Lisp; Package: biolisp; -*-

(in-package :bio)

;;; +=========================================================================+
;;; | Copyright (c) 2002-2006 The BioBike Team                                |
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

;;; Authors:  JP Massar, Jeff Elhai, Jeff Shrager.

(defun loaded-gene-or-protein-to-protein-frame 
       (gene-or-protein 
	&key (if-non-coding-gene :error)
	&aux protein-frame gene-or-protein-frame entity-type)
  (setq gene-or-protein-frame 
        (canonicalize-frame-designator gene-or-protein t))
  (setq entity-type 
        (ecase cl-user:*frame-system-version*
          (:old (#^Organism-Entity-Type gene-or-protein-frame))
          (:new 
           (cond
            ((typep gene-or-protein-frame 'frames::bio.gene) #$Gene)
            ((typep gene-or-protein-frame 'frames::bio.protein) #$Protein)
            (t nil)
            ))))
  (cond
   ((eq entity-type #$Gene)
    (let* ((gene gene-or-protein-frame)
           (proteins (#^Proteins gene-or-protein-frame))
           (len (length proteins)))
      (cond
       ((zerop len)
	(case if-non-coding-gene
	  (:error (error "The gene, ~s, does not encode any proteins!" gene))
	  ((:nil nil :ignore :ok) 
           (return-from loaded-gene-or-protein-to-protein-frame nil))
	  (t 
           (error 
            #.(one-string-nl
               "The gene, ~A, does not encode any proteins, but you gave"
               "~s for IF-NON-CODING-GENE, which isn't a valid option!"
               "(Must be: nil, :nil, :ignore, or :ok")
            gene if-non-coding-gene))))
       ((> len 1)
        (error "This gene, ~A, encodes more than one protein: ~A"
               gene proteins))
       (t (setq protein-frame (first proteins)))
       )))
   ((eq entity-type #$Protein) (setq protein-frame gene-or-protein-frame))
   (t
    (error 
     #.(one-string-nl
      "The object ~A is not recognizable as either a gene or protein"
      "frame of a loaded organism")
     gene-or-protein-frame
     )))
  protein-frame
  )

(defun two-way-ortholog-of 
  (gene-or-protein other-organism blast-threshold
		   &key (if-non-coding-gene :error) using-bit
		   &aux protein-frame other-orgf)
  #.(one-string-nl
     "First converts GENE-OR-PROTEIN into a protein frame. If GENE-OR-PROTEIN"
     "is a gene, the gene frame's unique protein frame is used, or an error"
     "is signalled if no unique protein exists."
     "Then return a protein frame of OTHER-ORGANISM if and only if that "
     "protein frame results from calling:"
     "(best-blast-ortholog-of protein-frame other-organism blast-threshold)"
     "*** and *** "
     "the original PROTEIN results from calling best-blast-ortholog-of"
     "again using the result of the first call, the original PROTEIN's"
     "organism, and the same BLAST-THRESHOLD."
     "As a special case, if PROTEIN is actually a protein of OTHER-ORGANISM"
     "then the original PROTEIN frame is returned."
     "If no protein satisfies this criterion NIL is returned."
     "The keywords argument :IF-NON-CODING-GENE (default :Error) can be set to"
     ":ok, :nil, :ignore, in which case if a non-coding gene is given, nil will be returned.")
  (setq protein-frame 
        (loaded-gene-or-protein-to-protein-frame 
	 gene-or-protein :if-non-coding-gene if-non-coding-gene))
  ;; Above may return nil if if-non-coding-gene is nil and key is ok, etc.
  ;; In that case, skip all this and just return a nil.
  (when protein-frame 
    (setq other-orgf (canonicalize-organism-designator other-organism))
    (if (eq (#^Organism protein-frame) other-orgf)
	protein-frame
      (let ((match 
	     (best-blast-ortholog-of 
	      protein-frame other-orgf blast-threshold :USING-BIT using-bit)))
	(when match
	  (let* ((match-frame (canonicalize-frame-designator match t))
		 (r-match
		  (best-blast-ortholog-of 
		   match-frame (#^Organism protein-frame) blast-threshold :USING-BIT using-bit)))
	    (when r-match
	      (let ((r-match-frame (canonicalize-frame-designator r-match t)))
		(and (eq protein-frame r-match-frame) match-frame)
		))))))))

(defun best-blast-ortholog-of (protein-frame other-orgf blast-threshold
         &KEY using-bit)
  #.(one-string-nl
     "Returns a protein frame of OTHER-ORGANISM which best matches PROTEIN"
     "wrt a BLAST comparison metric. The metric must produce a match whose"
     "metric value is less than BLAST-THRESHOLD or NIL is returned."
     "The 2nd value returned is the metric value of the best match."
     "The third value is a list of all the matches satisfying BLAST-THRESHOLD."
     )
  (let* ((matches 
	  (ordered-blast-orthologs-of 
	   protein-frame
	   blast-threshold 
	   :other-organism other-orgf :USING-BIT using-bit))
	 )
    (values 
     (caar matches)
     (cadar matches)
     (purge-duplicates matches :key 'first :test 'eq)
     )))

(defun ordered-blast-orthologs-of (protein blast-threshold &key other-organism
         using-bit)
  #.(one-string-nl
     "Returns an ordered list of protein frames where each is a blast"
     "ortholog of the given protein, at or below the indicated blast threshold."
     "This is done both forwards and backwards, and the result is sorted by"
     "e-value. If :OTHER-ORGANISM is given, the resulting proteins will only"
     "come from that organism.")
  (let* ((protein-frame (canonicalize-frame-designator protein))
         (*db-verbose* nil))
    (when protein-frame
      (unless (ecase cl-user::*frame-system-version*
                (:old (eq #$protein (#^organism-entity-type protein-frame)))
                (:new (typep protein-frame 'frames::bio.protein))
                (:sframes (typep protein-frame 'bio::protein)))
        (error "Argument ~S is not a protein designator or protein frame."
               protein))
      (->frames
       (IF using-bit
          (sort 
	        (two-way-crossblast-query 
	           protein-frame blast-threshold other-organism :USING-BIT T)
            '> :key 'THIRD)
          (sort 
	        (two-way-crossblast-query 
	           protein-frame blast-threshold other-organism)
            '< :key 'second))))))

(defun two-way-crossblast-query (protein blast-threshold other-organism
         &KEY using-bit)
  (let ((other-organism 
	 (when other-organism
	   (mysql-escape 
            (fname (canonicalize-organism-designator other-organism)))))
	(sql-protein-name (mysql-escape (fname protein)))
	)
     (append 
      (dosql 
       (forward-crossblast-query
	sql-protein-name blast-threshold other-organism :USING-BIT using-bit))
      (dosql
       (reverse-crossblast-query
	sql-protein-name blast-threshold other-organism :USING-BIT using-bit)))))

#|

Here's the raw mysql query we're trying to do here:

select id_to, orgname from crossblast 
  left join biobike_orgobjs on id_to = fname 
  left join biobike_orgids on biobike_orgobjs.orgid = biobike_orgids.orgid 
  where id_from = 'A29413.p-Av?0001' and evalue < 0.001 limit 10;

|#

(defun forward-crossblast-query (protein-name blast-threshold other-org-name
         &KEY using-bit)
   (formatn
    (one-string
     (IF using-bit
       "select id_to, evalue, BitScore from crossblast "
       "select id_to, evalue from crossblast ")
     "left join biobike_orgobjs on id_to = fname "
     "left join biobike_orgids on biobike_orgobjs.orgid = biobike_orgids.orgid "
     "where id_from = ~s "
     "and evalue < ~a "
     (if other-org-name (formatn "and orgname = ~s" other-org-name) ""))
    protein-name
    (float-to-mysql-float-string blast-threshold)
    ))

(defun reverse-crossblast-query (protein-name blast-threshold other-org-name
         &KEY using-bit)
  (formatn
   (one-string
    (IF using-bit
       "select id_from, evalue, BitScore from crossblast "
       "select id_from, evalue from crossblast ")
    "left join biobike_orgobjs on id_from = fname "
    "left join biobike_orgids on biobike_orgobjs.orgid = biobike_orgids.orgid "
    "where id_to = ~s "
    "and evalue < ~a "
    (if other-org-name (formatn "and orgname = ~s" other-org-name) ""))
   protein-name
   (float-to-mysql-float-string blast-threshold)
   ))

#+symmetric-query
;;; Tentative code assuming the crossblast table is symmetric.
;;; THIS IS HIGHLY BIT-ROTTED! (JS20060305)
(defun best-blast-ortholog-of 
       (protein other-organism blast-threshold 
                &key (max-best 10) (verbose? nil))
  #.(one-string-nl
     "Returns a protein frame of OTHER-ORGANISM which best matches PROTEIN"
     "wrt a BLAST comparison metric. The metric must produce a match whose"
     "metric value is less than BLAST-THRESHOLD or NIL is returned."
     "The 2nd value returned is the metric value of the best match."
     "The third value is a list of the at most MAX-BEST (protein metric-value)"
     "pairs with the highest scores that satisfied BLAST-THRESHOLD as well,"
     "ordered by score.")
  (let* ((protein (canonicalize-frame-designator protein))
         (other-organism 
          (canonicalize-organism-designator 
           other-organism :must-be-loaded? nil :load? nil))
         (protein-name (fname protein))
         (sql-protein-name (mysql-escape protein-name))
         (organism-protein-table-name
          (mysql-escape
           (one-string 
            (string-downcase (fname other-organism)) 
            "_proteins_information")))
         (query 
          (forward-crossblast-query
           organism-protein-table-name sql-protein-name 
           blast-threshold max-best))
         (*db-verbose* verbose?)
         (query-result (dosql query))
         (ordered-result (sort query-result '< :key 'second))
         (len (length ordered-result))
         )
    (when ordered-result
      (values 
       (->frames (first (first ordered-result)) t)
       (second (first ordered-result))
       (subseq ordered-result 0 (min len max-best))
       ))))

;;; Returns genes in a different organism that are related to a gene
;;; via various GO nodes.

;;; A list of GO nodes and related genes is returned, in the form of
;;; ((GO-node-1 (gene1 gene2 ...)) (GO-node-2 (gene1 gene2...)) ...)

(defun go-orthologs-of (gene other-organism)
  #.(one-string-nl
     "Find the genes of OTHER-ORGANISM that are related to GENE by virtue "
     "of their potentially having the same function -- i.e., they have a "
     "GO node in common.  Returns a list of GO nodes they have in common, "
     "and for each GO node, the genes of OTHER-ORGANISM associated with "
     "that GO node.  The return form is: "
     "((GO-node-1 (gene1 gene2 ...)) (GO-node-2 (gene1 gene2...)) ...)")
  (let ((gene-frame (canonicalize-gene-designator gene))
        (other-org-frame (canonicalize-organism-designator other-organism)))
    (loop for go-frame in (#^Go-Id  gene-frame) nconc
	  (let* ((related
		  (remove-if-not
		   #'(lambda (x) (eq (#^Organism x) other-org-frame))
		   (#^Go.Related-Genes go-frame)
		   )))
	    (when related (list (list go-frame related)))
	    ))))

;;; These are standard programs that come from NCBI.  You can download them at
;;;
;;;    http://genome.nhgri.nih.gov/blastall/blast_install/

;;; For running crossblast.  These are only meant to be manually run.
#-:SBCL
(defun start-crossblast (&key (organisms *loaded-organisms*) (e-limit "1e-3"))
  #.(one-string-nl
     "Given an optional list of loaded organisms (default=*loaded-organisms*) "
     "creates a /tmp file containing the proteins for each organism, and then"
     "sets up an all x all crossblast between them, leaving the results in"
     "/tmp/allproteins.crossblast, which can be uploaded using"
     "upload-crossblast-table.  Optionally the key :e-limit can be a STRING "
     "representing the e-value limit to give to blastp (default = \"1e-3\"")
  (with-open-file 
      (o "/tmp/allproteins.fa" :direction :output :if-exists :supersede)
    (loop for organism in organisms
          do 
          (format t "Dumped ~a~%" organism)
          (loop for p in (#^proteins organism)
                do (format o ">~a~%" (#^fname p))
                (format o "~a~%" (extract-protein-sequence p))
                )))
  ;; Not clear why I have to put the complete path on the DB....
  (let* ((formatdb-path 
          (formatn "~Aformatdb " cl-user::*blast-executable-toplevel-dir*))
         (formatdb-args "-p T -i /tmp/allproteins.fa")
         #+:sbcl
         (sbcl-formatdb-args "-p -T -i /tmp/allproteins.fs")
         (blastall-path 
          (formatn "~Ablastall " cl-user::*blast-executable-toplevel-dir*))
         (blastall-args 
          (formatn 
           (one-string
            "-FF -p blastp -d /tmp/allproteins.fa -i /tmp/allproteins.fa "
            "-o /tmp/allproteins.crossblast -e ~a -m 8") e-limit))
         #+:sbcl
         (sbcl-blastall-args 
          (formatn
           (one-string
            "-FF -p blastp -d /tmp/allproteins.fa -i /tmp/allproteins.fa "
            "-o /tmp/allproteins.crossblast -e ~a -m 8") e-limit))
         #-:sbcl
         (formatdb-cmd (one-string formatdb-path formatdb-args))
         #+:sbcl
         (formatdb-cmd (one-string formatdb-path sbcl-formatdb-args))
         #-:sbcl
         (blastall-cmd (one-string blastall-path blastall-args))
         #+:sbcl
         (blastall-cmd (one-string blastall-path sbcl-blastall-args))
         )
    (protected-shell-command formatdb-cmd)
    (protected-shell-command blastall-cmd)
    ))

#|


    #-:SBCL
  (EXCL::PROTECTED-SHELL-COMMAND "/usr/bin/formatdb -p T -i /tmp/allproteins.fa")
  #+:SBCL
  (PROTECTED-SHELL-COMMAND "/usr/bin/formatdb -p -T -i /tmp/allproteins.fs")
  #-:SBCL
  (EXCL::PROTECTED-SHELL-COMMAND
   (format nil "/usr/bin/blastall -FF -p blastp -d /tmp/allproteins.fa -i /tmp/allproteins.fa -o /tmp/allproteins.crossblast -e ~a -m 8" e-limit)
   :wait nil)
  #+:SBCL
  (PROTECTED-SHELL-COMMAND
   (format nil "/usr/bin/blastall -FF -p blastp -d /tmp/allproteins.fa -i /tmp/allproteins.fa -o /tmp/allproteins.crossblast -e ~a -m 8" e-limit)
   :wait nil)
  )

|#


#| Blast output (from http://www.indiana.edu/~rac/bioinformatics/spblast.html)

    Identity of query sequence 
    Identity of subject sequence (matching sequence in database) 
    Percent identity 
    Alignment length 
    Number of mismatches 
    Number of gaps 
    Start of query sequence 
    End of query sequence 
    Start of subject sequence 
    End of subject sequence 
    E-value 
    Bit-score 
    |#

(defun replace-crossblast-table 
       (file &key (include-symmetries? nil) (really-delete-the-old-table?? nil))
  (unless really-delete-the-old-table?? 
    (error 
     (one-string
      "DANGER WILL ROBINSON! This function deletes the crossblast table! "
      "If you REALLY want to do it, use the magic keyword!!  If not, you "
      "might want to use ADD-TO-CROSSBLAST-TABLE instead.")))
  (with-organisms-db 
      (db)
    (let ((existing-tables (mapcar #'car (bio::esql "show tables"))))
      (when (member "crossblast" existing-tables :test #'string=)
        (bio::esql (print (format nil "drop table crossblast"))))
      ;; This is separate so that if the user has cleverly moved the table, 
      ;; it will recreate it regardless of whether it was there to be removed.
      (bio::esql 
       (print 
        (formatn
         (one-string
          "create table crossblast ( "
          " id_from varchar(50) not null null null,"
          " id_to varchar(50) not null null null,"
          " pID float, "
          " QueryStart  smallint(5) unsigned, "
          " QueryEnd  smallint(5) unsigned, "
          " SubjectStart  smallint(5) unsigned, "
          " SubjectEnd  smallint(5) unsigned, "
          " evalue double not null null null, "
          " index id (id_from, id_to), "
          " index idf (id_from), "
          " index idt (id_to)"
          ")"
          ))))))
  (add-to-crossblast-table file :include-symmetries? include-symmetries?)
  )

(defparameter +cb-entry-present-template-sql+
  "select * from crossblast where id_from = ~s and id_to = ~s")

(defparameter +cb-insert-template-sql+
  (one-string
   "insert into crossblast "
   "(id_from, id_to, pID, QueryStart, QueryEnd, "
   "SubjectStart, SubjectEnd, evalue) "
   "values (~s,~s,~a,~a,~a,~a,~a,~a)"
   ))
  
(defun add-to-crossblast-table (file &key (include-symmetries? nil))
  (with-organisms-db 
      (db)
    (let ((*db-verbose* nil)
          (*generic-db-verbose* nil))
      (with-open-file (i file)
        (loop for line = (read-line i nil nil)
              until (null line)
              with count = 0
              as (from to pid nil nil nil qs qe ss se evalue nil) = 
              (cl-ppcre::split "\\s+" line)
              ;; Include a=a blasts -- usually a very small number -- 
              ;; comment in to exclude these
              when (and ; (not (string-equal from to)) 
                    (or include-symmetries? ; Aways include if asked to...
                        (not (bio::esql 
                              (formatn 
                               +cb-entry-present-template-sql+
                               to from)))))
              do 
              (bio::esql 
               (formatn
                +cb-insert-template-sql+
                from to pid qs qe ss se evalue
                ))
              (incf count)
              (when (zerop (mod count 10000)) (print count))
              finally 
              (cformatt "~D records entered into crossblast table." count)))
      )))

#||

;;; If we ever need this code, we can always uncomment it. 
;;; I don't think anybody is ever going to use it.  

;;; The organismobjects and organismids tables contain the fname of
;;; every object and every organism in the system. These are often
;;; joined with complex queries in order to restrict them to specific
;;; organisms. The entries are all fnames, but for efficiency's sake,
;;; the organisms are each given a numerical id which crosses with
;;; another, smaller table -- thus saving eleventy-zillion bytes!

(defun recreate-orgobj-tables (&key (really-delete-the-old-tables?? nil))
  ;; I don't know what-all these are, but they should -all be off!
  (let ((*KDB-VERBOSE* nil)
	(*DB-VERBOSE* nil)
	(*generic-DB-VERBOSE* nil))
    (unless really-delete-the-old-tables?? 
      (error 
       (one-string-nl
	"WARNING! recreate-organism-objects-tables deletes the"
        "organismobjects and organismids tables!"
        "If you REALLY want to do this, use the magic keyword!")))
    (flet ((replace-table (table create-form)
             (ignore-errors (dosql (print (format nil "drop table ~a" table))))
             (dosql create-form)))
      (with-organisms-db 
       (db)
       (replace-table 
	"biobike_orgobjs"
	"create table biobike_orgobjs (orgid tinyint, objtypeid tinyint, fname varchar(50) UNIQUE NOT NULL, PRIMARY KEY (fname))")
       (replace-table 
	"biobike_orgids"
	"create table biobike_orgids (orgid tinyint, orgname varchar(100))")
       (replace-table 
	"biobike_objtypeids"
	"create table biobike_objtypeids (objtypeid tinyint, objtypename varchar(10))")))
    (install-orgobj-tables)
    ))

||#

(defvar *orgobj-types* 
  (list (cons "gene" #^genes)
	(cons "protein" #^proteins)))

(defun install-orgobj-tables 
       (&aux insert-into-objtypeids replace-into-orgids insert-into-orgobjs)
  (setq 
   insert-into-objtypeids
   "insert into biobike_objtypeids (objtypeid, objtypename) values (~a,~s)"
   replace-into-orgids   
   "replace into biobike_orgids (orgid , orgname) values (~a,~s)"
   insert-into-orgobjs   
   "insert into biobike_orgobjs (orgid, objtypeid, fname) values (~a,~a,~s)"
   )
  (loop for (objtypename . nil) in *orgobj-types*
        as objtypeid from 1 by 1 do
        (dosql (formatn insert-into-objtypeids objtypeid objtypename)))
  (loop for organism in *loaded-organisms*
	as orgid from 1 by 1 do
        (cformatt "Processing table for ~A" organism)
	(dosql
	 (formatn replace-into-orgids orgid (mysql-escape (#^fname organism))))
	(loop for (nil . objaccessor) in *orgobj-types*
	      as objtypeid from 1 by 1 do
	      (loop for object in (funcall objaccessor organism) do
		    (dosql
		     (formatn 
                      insert-into-orgobjs orgid objtypeid (#^fname object)
                      ))))))

;;; Database operations for single organisms:

(defmacro reload-single-organism-crossblast (organism)
  #.(one-string-nl
     "Used to reload a single organism into the database, this sets up"
     "/tmp files containing the proteins for each loaded organism, and for"
     "the target organism, and then does the crossblast between them,"
     "leaving the results in /tmp/allproteins.crossblast. This, then, is"
     "uploaded. Note that the key :e-limit is a STRING  representing the"
     "e-value limit to give to blastp (default = \"1e-3\"")
  `(format nil "~a" 
    (one-string
     "In order to reload reload the crossblast of " ',organism " into the database," #\newline
     "You need to issue the following commands:" #\newline
     "From a listener loaded with the new organism data, do:" #\newline
     "  (bio::cbrso-dump " ',organism ")" #\newline
     "" #\newline
     "Next log in to to the server and issue:" #\newline
     (formatn "~Aformatdb -p T -i /tmp/targetproteins.fa" cl-user::*blast-executable-toplevel-dir*) #\newline
     "Don't worry about WARNINGs about zero length sequences or illegal characters (unless there are hundreds of these!!" #\newline
     "Followed by:" #\newline
     (formatn "~Ablastall -FF -p blastp -d /tmp/targetproteins.fa -i /tmp/newproteins.fa -o /tmp/new.crossblast -e ELIMIT -m 8 >& /tmp/cblog & " 
	      cl-user::*blast-executable-toplevel-dir*) 
     #\newline
     "Where ELIMIT is replaced with the maximum e-value that you want to record blasts at, for example: 1e-5" #\newline
     "This starts a batch job to Go to lunch (This will take a long time!)" #\newline
     "" #\newline
     "When the blast is done, return to the BioBike listener and issue:" #\newline
     "  (bio::cbrso-replace " ',organism " :the-magic-keyword t)" #\newline
     "" #\newline
     "If you don't know what the magic keyword is, then you shouldn't be doing this!"
     )))

(defun bio::cbrso-dump (organism)
  ;; Dump targets
  (with-open-file 
   (o "/tmp/targetproteins.fa" :direction :output :if-exists :supersede)
   (loop for this-organism in *loaded-organisms*
	 ;; unless (eq this-organism organism) ; Skip ourself! -- actually do NOT skip ourself!!
	 do 
	 (format t "Dumping ~a~%" this-organism)
	 (loop for p in (#^proteins this-organism)
	       do (format o ">~a~%" (#^fname p))
	       (format o "~a~%" (extract-protein-sequence p))
	       )))
  ;; Dump my prots
  (with-open-file 
   (o "/tmp/newproteins.fa" :direction :output :if-exists :supersede)
   (loop for p in (#^proteins organism)
	 do (format o ">~a~%" (#^fname p))
	 (format o "~a~%" (extract-protein-sequence p))
	 ))
  "Ready for the manual server-side blast steps!")

(defun bio::cbrso-replace (organism 
			   &key (really-delete-the-tables?? nil) (include-symmetries? t)
			   &aux (orgname (#^fname organism)))
  (if (not really-delete-the-tables??)
      (error 
       (one-string-nl
	"WARNING! bio::cbrso-replace unrecoverably destroys data!"
	"If you REALLY want to do this, use the magic keyword!")))
  ;; Destroy the old data:
  (let ((orgid (xsql "select orgid from biobike_orgids where orgname = ~s" orgname)))
    (unless (numberp orgid)
      (error "In destroy-base-organism-data: No organism seems to be called ~s" orgname))
    (xsql "delete from biobike_orgids where orgid = ~a" orgid)
    (xsql "delete from crossblast where id_from = ~a" orgid)
    (xsql "delete from crossblast where id_to = ~a" orgid)
    (xsql "delete from biobike_orgobjs where orgid = ~a" orgid)
    (warn "Destroy-base-organism-data has removed all the infomation for ~s from the database tables."))
  ;; Now reload the tables:
  (with-organisms-db 
   (db)
   (let ((*db-verbose* nil)
	 (*generic-db-verbose* nil))
     (with-open-file 
      (i "/tmp/new.crossblast")
      (loop for line = (read-line i nil nil)
	    until (null line)
	    with count = 0
	    as (from to pid nil nil nil qs qe ss se evalue nil) = 
	    (cl-ppcre::split "\\s+" line)
	    ;; Include a=a blasts -- usually a very small number -- 
	    ;; comment in to exclude these
	    when (and		       ; (not (string-equal from to)) 
		  (or include-symmetries? ; Aways include if asked to...
		      (not (bio::esql 
			    (formatn 
			     +cb-entry-present-template-sql+
			     to from)))))
	    do 
	    (bio::esql 
	     (formatn
	      +cb-insert-template-sql+
	      from to pid qs qe ss se evalue
	      ))
	    (incf count)
	    (when (zerop (mod count 10000)) (print count))
	    finally 
	    (cformatt "~D records entered into crossblast table." count))
      ))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Functions to construct revised flat-ortholog-list representation
;;; 07/28/07 JP

;;; Returns two values: 
;;; 1 - a list that looks like 
;;; ((org1 ((protein1 bitmask1) (protein2 bitmask2) ...)) 
;;;  (org2 ...))
;;; 2 - the mapping from organisms to ids which looks like
;;; ((org1 id1) (org2 id2) ...)
;;; 
;;; In the above all the organism and proteins are frames.  

(defun create-flat-ortholog-list (evalue)
  (let ((organisms (available-organisms)))
    (loop for orgf in organisms 
          do
          (unless (#^organism-loaded? orgf) (load-organism orgf)))
    (let ((mapping (create-organism->id-mapping organisms)))
      (values 
       (loop for orgf in organisms 
             collect 
             (list 
              orgf
              (proteins->flat-ortholog-mapping (#^proteins orgf) mapping evalue)
              ))
       mapping
       ))))

;;; Given a number representing a protein's organism bitmask,
;;; returns a list of the organisms represented by that bitmask
;;; with respect to the provided mapping

(defun decode-organisms-bitmask (bitmask mapping)
  (loop for (organism id) in mapping
        when (plusp (logand bitmask (ash 1 id)))
        collect organism
        ))

;;; Auxiliary functions 
  
(defun create-organism->id-mapping (organisms)
  (loop for org in organisms
        for j from 0
        collect (list org j)
        ))

(defun organism->id (orgf mapping) (second (assoc orgf mapping)))
  
(defun id->organism (id mapping) (first (find id mapping 'key 'second)))

;;; This does the query both ways, looking for matches for the protein in 
;;; both the id_to and id_from fields, then removing any duplicates.
;;; If the table is actually stored both ways, this is unnecessary.  

;;; The function then turns the resulting strings into frames and returns
;;; a list of protein frames. 

(defun crossblast-query-for-particular-protein (protein evalue)
  (let ((protein-strings 
         (purge-duplicates 
          (nconc 
           (mapcar 
            'car
            (dosql
             (formatn 
              (one-string
               "select id_from from crossblast "
               "where id_to = '~A' and evalue < ~A;")
              (fname protein) (float-to-mysql-float-string evalue)
              )))
           (mapcar 
            'car
            (dosql
             (formatn 
              (one-string
               "select id_to from crossblast "
               "where id_from = '~A' and evalue < ~A;")
              (fname protein) (float-to-mysql-float-string evalue)
              ))))
          :test 'equal :hash-threshold 30
          )))
    (loop for s in protein-strings 
          collect 
          (let ((p (frame-fnamed s)))
            (unless p (error "No frame found for protein string ~S" s))
            p
            ))))

(defun proteins->flat-ortholog-mapping 
       (protein-list mapping evalue)
  (loop for p in protein-list 
        as orthologs = (crossblast-query-for-particular-protein p evalue)
        collect 
        (list 
         p
         (encode-organisms-bitmask 
          (mapcar (lambda (protein) (#^organism protein)) orthologs)
          mapping
          ))))

(defun encode-organisms-bitmask (organisms mapping)
  (create-organisms-bitmask 
   (mapcar 
    (lambda (organism) 
      (let ((id (organism->id organism mapping)))
        (unless (integerp id) 
          (error "Organism ~A has no ID!" organism))
        id
        ))
    organisms
    )))
       
(defun create-organisms-bitmask (list-of-integers)
  (let ((mask 0))
    (loop for i in list-of-integers do
          (setq mask (logior mask (ash 1 i)))
          finally (return mask)
          )))


; ************************************************************************************
; **** functions required by ORTHOLOG-OF* and HOMOLOG-OF* , Arnaud - June 6 2008. ****

(defun forward-crossblast-query-tmp1 (protein-name blast-threshold similarity other-org-name)
   (formatn
    (one-string
     "select id_to, pID, QueryStart, QueryEnd, SubjectStart, SubjectEnd, evalue, BitScore from crossblast "
     "left join biobike_orgobjs on id_to = fname "
     "left join biobike_orgids on biobike_orgobjs.orgid = biobike_orgids.orgid "
     "where id_from = ~s "
     "and evalue < ~a "
     (if other-org-name (formatn "and orgname = ~s" other-org-name) "")
     (if similarity (formatn "and pID > ~s" similarity) ""))
    protein-name
    (float-to-mysql-float-string blast-threshold)
    ))

(defun forward-crossblast-query-tmp2 (protein-name blast-threshold similarity other-org-name)
   (formatn
    (one-string
     "select id_to, pID, QueryStart, QueryEnd, SubjectStart, SubjectEnd, evalue, BitScore from crossblast_ncg "
     "left join biobike_orgobjs on id_to = fname "
     "left join biobike_orgids on biobike_orgobjs.orgid = biobike_orgids.orgid "
     "where id_from = ~s "
     "and evalue < ~a "
     (if other-org-name (formatn "and orgname = ~s" other-org-name) "")
     (if similarity (formatn "and pID > ~s" similarity) ""))
    protein-name
    (float-to-mysql-float-string blast-threshold)
    ))


(defun reverse-crossblast-query-tmp1 (protein-name blast-threshold similarity other-org-name)
  (formatn
   (one-string
    "select id_from, pID, QueryStart, QueryEnd, SubjectStart, SubjectEnd, evalue, BitScore from crossblast "
    "left join biobike_orgobjs on id_from = fname "
    "left join biobike_orgids on biobike_orgobjs.orgid = biobike_orgids.orgid "
    "where id_to = ~s "
    "and evalue < ~a "
    (if other-org-name (format nil "and orgname = ~s" other-org-name) "")
    (if similarity (formatn "and pID > ~s" similarity) ""))
   protein-name
   (float-to-mysql-float-string blast-threshold)
   ))

(defun reverse-crossblast-query-tmp2 (protein-name blast-threshold similarity other-org-name)
  (formatn
   (one-string
    "select id_from, pID, QueryStart, QueryEnd, SubjectStart, SubjectEnd, evalue, BitScore from crossblast_ncg "
    "left join biobike_orgobjs on id_from = fname "
    "left join biobike_orgids on biobike_orgobjs.orgid = biobike_orgids.orgid "
    "where id_to = ~s "
    "and evalue < ~a "
    (if other-org-name (format nil "and orgname = ~s" other-org-name) "")
    (if similarity (formatn "and pID > ~s" similarity) ""))
   protein-name
   (float-to-mysql-float-string blast-threshold)
   ))


(defun forward-way-crossblast-query-tmp (protein blast-threshold blast-ID-threshold other-organism)
  (let* ((other-organism 
                     (when other-organism
                       (mysql-escape 
            (fname (canonicalize-organism-designator other-organism)))))
                    (sql-protein-name (mysql-escape (fname protein)))
         (orthologs
     (append 
      (dosql 
       (forward-crossblast-query-tmp1
                    sql-protein-name blast-threshold blast-ID-threshold other-organism))
      (dosql 
       (forward-crossblast-query-tmp2
                    sql-protein-name blast-threshold blast-ID-threshold other-organism)))))
(sort (sort orthologs '> :key 'second) '< :key 'seventh)

))

(defun reverse-way-crossblast-query-tmp (protein blast-threshold blast-ID-threshold other-organism)
  (let* ((other-organism 
                     (when other-organism
                       (mysql-escape 
            (fname (canonicalize-organism-designator other-organism)))))
                    (sql-protein-name (mysql-escape (fname protein)))
         (orthologs
     (append 
      (dosql 
       (reverse-crossblast-query-tmp1
                    sql-protein-name blast-threshold blast-ID-threshold other-organism))
      (dosql 
       (reverse-crossblast-query-tmp2
                    sql-protein-name blast-threshold blast-ID-threshold other-organism)))))
(sort (sort orthologs '> :key 'second) '< :key 'seventh)

))


(defun forward-blast-orthologs-of-tmp (protein blast-threshold blast-ID-threshold &key other-organism)
  #.(one-string-nl
     "Returns an ordered list of protein frames where each is a blast"
     "ortholog of the given protein, at or below the indicated blast threshold."
     "This is done both forwards and backwards, and the result is sorted by"
     "e-value. If :OTHER-ORGANISM is given, the resulting proteins will only"
     "come from that organism.")
  (let* ((protein-frame (canonicalize-frame-designator protein))
         (*db-verbose* nil))
    (when protein-frame
      (unless (ecase cl-user::*frame-system-version*
                (:old (or (eq #$protein (#^organism-entity-type protein-frame)) (eq #$gene (#^organism-entity-type protein-frame))))
                (:new (or (typep protein-frame 'frames::bio.protein) (typep protein-frame 'frames::bio.gene))))
        (error "Argument ~S is not a protein/gene designator or protein/gene frame."
               protein))
      (->frames
                    (forward-way-crossblast-query-tmp
                     protein-frame blast-threshold blast-ID-threshold other-organism)
))))
                    


(defun reverse-blast-orthologs-of-tmp (protein blast-threshold blast-ID-threshold &key other-organism)
  #.(one-string-nl
     "Returns an ordered list of protein frames where each is a blast"
     "ortholog of the given protein, at or below the indicated blast threshold."
     "This is done both forwards and backwards, and the result is sorted by"
     "e-value. If :OTHER-ORGANISM is given, the resulting proteins will only"
     "come from that organism.")
  (let* ((protein-frame (canonicalize-frame-designator protein))
         (*db-verbose* nil))
    (when protein-frame
      (unless (ecase cl-user::*frame-system-version*
                (:old (or (eq #$protein (#^organism-entity-type protein-frame)) (eq #$gene (#^organism-entity-type protein-frame))))
                (:new (or (typep protein-frame 'frames::bio.protein) (typep protein-frame 'frames::bio.gene))))
        (error "Argument ~S is not a protein/gene designator or protein/gene frame."
               protein))
      (->frames
                    (reverse-way-crossblast-query-tmp 
                     protein-frame blast-threshold blast-ID-threshold other-organism)
))))


(defun forward-best-blast-ortholog-of-tmp (protein-frame other-orgf blast-threshold blast-ID-threshold) 
  #.(one-string-nl
     "Returns a protein frame of OTHER-ORGANISM which best matches PROTEIN"
     "wrt a BLAST comparison metric. The metric must produce a match whose"
     "metric value is less than BLAST-THRESHOLD or NIL is returned."
     "The 2nd value returned is the metric value of the best match."
     "The third value is a list of all the matches satisfying BLAST-THRESHOLD."
     )
  (let* ((matches 
                      (forward-blast-orthologs-of-tmp 
                       protein-frame
                       blast-threshold
                       blast-ID-threshold
                       :other-organism other-orgf))
                     )
    (values 
     (first (purge-duplicates matches :key 'first :test 'eq))
     )))

(defun reverse-best-blast-ortholog-of-tmp (protein-frame other-orgf blast-threshold blast-ID-threshold) 
  #.(one-string-nl
     "Returns a protein frame of OTHER-ORGANISM which best matches PROTEIN"
     "wrt a BLAST comparison metric. The metric must produce a match whose"
     "metric value is less than BLAST-THRESHOLD or NIL is returned."
     "The 2nd value returned is the metric value of the best match."
     "The third value is a list of all the matches satisfying BLAST-THRESHOLD."
     )
  (let* ((matches 
                      (reverse-blast-orthologs-of-tmp 
                       protein-frame
                       blast-threshold
                       blast-ID-threshold
                       :other-organism other-orgf))
                     )
    (values 
     (first (purge-duplicates matches :key 'first :test 'eq))
     )))


(defun two-way-best-blast-ortholog-of-tmp 
  (gene-or-protein other-organism blast-threshold  blast-id-threshold)
  #.(one-string-nl
      " .... ")
      (let ((f-match 
               (forward-best-blast-ortholog-of-tmp 
                   gene-or-protein other-organism blast-threshold blast-ID-threshold)))
	(when f-match
	  (let* ((f-match-frame (canonicalize-frame-designator (FIRST f-match) t))
	        
                        (r-match (forward-best-blast-ortholog-of-tmp 
		   f-match-frame (#^Organism gene-or-protein) blast-threshold blast-id-threshold)))
	    (when r-match
	      (let ((r-match-frame (canonicalize-frame-designator (FIRST r-match) t)))
	    (and (eq gene-or-protein r-match-frame) f-match)
		))))))










