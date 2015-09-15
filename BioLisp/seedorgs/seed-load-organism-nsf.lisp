;;; -*- Package: bio; mode: lisp; base: 10; Syntax: Common-Lisp; -*-

(in-package :bio)

;;; +=========================================================================+
;;; | Copyright (c) 2009 JP Massar
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

;;; Author:  JP Massar

(defun application-instance-initializations-nsf ()
  (setq genericdb::*generic-db-verbose* nil)
  (cformatt "Creating frames for each SEED organism.")
  ;; Creates list of all the organism frames in *seed-genome-frames*
  (multiple-value-bind (data slotnames)
      (bio::all-useful-seed-genome-info)
    (cformatt "~D organisms found in SEED database." (length data))
    (setq slotnames (mapcar (lambda (x) (frames::frame-fnamed x t)) slotnames))
    (loop for record in data 
          as gid = (first record)
          as orgframename = (bio::seed-gid-to-biobike-orgname gid)
          as orgframe = (frames::frame-fnamed orgframename t)
          do 
          (push orgframe bio::*seed-genome-frames*)
          (setf (#^Seed-ID orgframe) gid)
          (setf (#^organism-data-directory orgframe)  
                (bio::organism-data-directory orgframename))
          (setf (#^isa orgframe) (list #$organism))
          (loop for datum in (cdr record) 
                for slotname in (cdr slotnames)
                do
                (setf (slotv orgframe slotname) datum)
                )))
  (cformatt "Creating lists of organism types found in the SEED.")
  (set-up-seed-genome-types)
  (let ((orgs user::*organisms-to-load-list*))
    (cformatt "Organisms to be loaded: ~A" orgs)
    (loop for organism in orgs 
          as orgf = (frame-fnamed (string organism))
          as gid = (#^seed-id orgf)
          do 
          (forward-package-funcall :bio :load-seed-organism gid)
          ))
  (cformatt "Creating VPL Seed organism selection operators")
  (create-seed-organism-selection-operators)
  (wb::load-instance-modules)
  )



(defun load-seed-organism-nsf 
       (gid &key (verbose? *seed-load-verbose*) (commit? t) (reload? nil))
  (declare (ignorable reload? commit?))
  (let ((*seed-load-verbose* verbose?)
        (*disable-seed-organism-autoload* t)  
        (*seed-access-mode* :mysql)
        (*bike-genome-name* (seed-gid-to-biobike-orgname gid)))
    (let ((*pegs-for-gid* nil)
          (*genome-info* nil)
          (*seed-genome-sequence-file* nil)
          (*genes-info* nil)
          (*proteins-info* nil)
          (*seed-proteins-sequence-file* nil)
          (*seed-gene-annotations-file* nil)
          (*annotations-hash* nil)
          (*plist-info* nil))
      (download-all-organism-data-from-seed gid)
      (let ((orgf (create-seed-organism-frames *bike-genome-name*)))
        (add-annotations-to-seed-gene-frames gid)
        (let* ((dir (#^organism-data-directory orgf))
               (file (non-acache-signal-file dir)))
          (with-open-file (p file :direction :output :if-exists :supersede)
            nil
            ))
        orgf
        ))))

(defun download-all-organism-data-from-seed (gid)
  (let ((orgname (seed-gid-to-biobike-orgname gid)))
    (download-seed-data-for-gid-into-vars gid orgname)
    ))

(defun download-seed-data-for-gid-into-vars (gid bgid)
  (vformatt 
   "You have asked to get download data for Seed organism ~A, (~A).~%" 
   gid bgid)
  (ensure-directories-exist (utils::s+ *data-directory* bgid "/"))
  (ensure-directories-exist (utils::s+ *data-directory* bgid "/genome/"))
  (ensure-directories-exist (utils::s+ *data-directory* bgid "/proteins/"))
  (vformatt "Now getting info for organism ~A genome.tbl." gid)
  (setq *genome-info* (get-seed-info-for-genome-tbl gid bgid))
  (vformatt "Done.")
  (vformatt "Now getting info for organism ~A genome.fasta." gid)
  (get-seed-info-for-genome-fasta gid bgid)
  (vformatt "Done.")
  (vformatt "Now getting info for organism ~A genes.tbl." gid)
  (setq *genes-info* (get-seed-info-for-genes-tbl gid bgid))
  (vformatt "Done.")
  (vformatt "Now getting info for organism ~A proteins.tbl." gid)
  (setq *proteins-info* (get-seed-info-for-protein-tbl))
  (vformatt "Done.")
  (vformatt "Now getting info for organism ~A proteins.fasta" gid)
  (get-seed-info-for-protein-fasta gid bgid)
  (vformatt "Done.")
  (vformatt "Now getting annotation info for organism ~A" gid)
  (setq *annotations-hash* (get-seed-info-for-annotations gid))
  (vformatt "Done.")
  (vformatt "Now getting plist information for ~A" gid)
  (setq *plist-info* (get-seed-info-for-plist gid bgid))
  (vformatt "Done.")
  (vformatt "All information downloaded.")
  nil
  )
      
(defun get-seed-info-for-genome-fasta (gid bgid)
  (let* ((dir (utils::s+ *data-directory* bgid "/"))
         (genomedir (utils::s+ dir "genome/"))
         (fasta-file (utils::s+ genomedir "genome.fasta")))
    (with-open-file
        (p fasta-file
           :direction :output
           :if-does-not-exist :create
           :if-exists :supersede)
      ;; Find the seed data file that is the fasta file that contains
      ;; all the sequence information for all the contigs of a gid.  
      ;; Then copy the data in that file to our own directory.  
      (let* ((contig-file-info 
              (seed-query
               "select contig,fileno from contig_seeks where genome = ~S"
               gid
               )))
        (let ((first-fileno (second (first contig-file-info))))
          (unless (every (lambda (x) (= (second x) first-fileno)) 
                         contig-file-info)
            (error "All the contig sequences aren't in the same file!")
            )
          (let* ((absolute-filepath 
                  (absolute-seed-path-from-fileno first-fileno))
                 (data 
                  (utils::file-to-string
                   absolute-filepath :max 50000000)))
            ;; Save to store in organism frame eventually
            (setq *seed-genome-sequence-file* absolute-filepath)
            (loop for ch across data do (write-char ch p))
            ))))))

;; returns list of 
;; peg/rna contig-name encodes-protein direction from to description
(defun get-seed-info-for-genes-tbl (gid orgname)
  (declare (ignore orgname))
  (let ((info (seed-gene-info-for-gid-nsf gid)))
    (setq *pegs-for-gid* (mapcar 'first info))
    (mapcar 
     (lambda (x desc) 
       (let ((gene-name (first x))
             (contig (second x))
             (location (third x))
             (type (fourth x)))
         (when (find #\, location)
           (cformatt "Gene ~A has multiple segments: ~A" gene-name location))
         (destructuring-bind (xcontig from to direction)
             (parse-seed-location-info location)
           (declare (ignore xcontig))
           (list
            gene-name
            contig
            ;; encodes-protein test
            (string-equal "peg" type)
            direction 
            from
            to
            desc
            ))))
     info
     (mysql-get-descriptions-for-pegs-of-genome *pegs-for-gid*)
     )))

;; just return a list of proteins names and associated peg names, not 
;; any information about the protein (for now)
(defun get-seed-info-for-protein-tbl ()
  (loop for gene-info in *genes-info*
        as gene-name = (first gene-info)
        when (search "peg" gene-name :test 'string-equal)
        collect
        (list (utils::s+ "p-" gene-name) gene-name)
        ))

(defun get-seed-info-for-protein-fasta (gid orgname)
  (let* ((dir (utils::s+ *data-directory* orgname "/"))
         (proteins-fasta-file (utils::s+ dir "proteins/proteins.fasta")))
  
    ;; for proteins there are master files which contain
    ;; the sequences of all
    ;; seed proteins (including the one we're looking for), and there is
    ;; a file containing the sequences for all the proteins
    ;; of the gid that 
    ;; we are interested in.  We want to snarf this latter file. 
    ;; The first query below potentially returns both 
    ;; a pointer to the master file and to the specific file.
    ;; How do we distinguish the specific file (which we want)
    ;; from the master file (which we don't want).  
    ;; It turns out that the master files don't contain in their path 
    ;; the gid, so by searching for the gid string in the path,
    ;; we can discriminate between the two.  
    (let* ((peg 
            (find-if
             (lambda (x) (search "peg" x :test 'string-equal))
             *pegs-for-gid*
             ))
           (possible-protein-filenos
            (mapcar
             'first
             (seed-query
              "select fileno from protein_sequence_seeks where id = ~S"
              (or peg (error "No peg found for gid ~A" gid))
              )))
           (absolute-filepath 
            (utils::s+ 
             *seed-mysql-data-root*
             (loop for fileno in possible-protein-filenos
                   as possible-path = 
                   (caar
                    (seed-query 
                     "select file from file_table where fileno = ~D"
                     fileno
                     ))
                   do
                   (when (search gid possible-path :test 'string-equal)
                     (return possible-path))
                   finally
                   (error "No non-master file found for gid ~A" gid)
                   )))
           ;; read the seed fasta file in
           (data (utils::file-to-string absolute-filepath :max 10000000)))
      ;; Store for later saving in organism frame
      (setq *seed-proteins-sequence-file* absolute-filepath)
      ;; and write a copy back out
      (with-open-file 
          (p proteins-fasta-file
             :direction :output
             :if-does-not-exist :create
             :if-exists :supersede)
        (loop for ch across data do (write-char ch p))
        ))
    ;; post-processing of protein fasta file, changing the 
    ;; name of the peg in each fasta entry to the biobike name
    ;; of the associated protein
    (seed-protein-fasta-to-biobike-fasta proteins-fasta-file gid)
    nil
    ))

(defun get-seed-info-for-plist (gid orgname)
  (let ((plist nil))
    (flet ((add (key value) (push (list key value) plist)))
      (add :submitter '("Rob Edwards" "Seed"))
      (add :seed-id gid)
      (add :links 
           '("http://ws.nmpdr.org/" "http://www.theseed.org/wiki/Glossary#PEG"))
      (add :organism-prefix (utils::s+ orgname "."))
      (add :genome-char-predicate "ALLOWABLE-GENOME-CHAR-UNFINISHED?")
      (add :protein-char-predicate "ALLOWABLE-SEED-PROTEIN-CHAR?")
      (destructuring-bind (gname maindomain complete restrictions taxonomy)
          (first 
           (seed-query 
            (utils::s+
             "select gname,maindomain,complete,restrictions,taxonomy "
             "from genome where genome = ~S")
            gid
            ))
        (add :official-seed-name gname)
        (add :maindomain maindomain)
        (let ((bool 
               (cond
                ;; Needs to be :t because of unkeywordize-property-list
                ((string= "1" complete) :t)
                (t nil)
                )))
          (add :seed-complete complete)
          (add :completed bool)
          )
        (add :restrictions restrictions)
        (add :taxonomy taxonomy)
        ))
    plist
    ))

(defun create-seed-organism-frames 
       (organism 
        &key 
        (verbose? *seed-load-verbose*)
        (postload? t)
        (data-structures? t)
        (pprint nil)
        (pprint-limit 10)
        (test? t)
        &aux
        (orgn (fstring organism))
        (orgf nil)
        (*seed-load-verbose* verbose?)
        )
  #.(one-string-nl
     "Load information from the organisms directory about an organism. "
     "Returns the organism frame."
     "Creates genome frames for each contig of the organism, and"
     "gene and protein frames if these sudbirectories exist."  
     "Modifies *loaded-organisms* appropriately."
     "The POSTLOAD? keyword controls whether a postload file (located "
     "in the directory where data for this organism is kept) is loaded "
     "(using LISP:LOAD) or not (the default is T, load the file if it exists)."
     "The DATA-STRUCTURES? keyword controls whether the various useful "
     "data structures, such as fragment information and sorted (by position"
     "on contig) vectors of genes are created.  The default is T, create"
     "these data structures."
     "If PPRINT is T a pretty print summary of this organism's data is"
     "printed to standard output."
     "If TEST? is true a simple test defined by a test file in the same"
     "directory as the postload file is run as a sanity check."
     )
 
  (vformatt "Now starting to load ~A" organism)

  ;; find the organism associated with ORGANISM which can either be 
  ;; an organism frame, an organism name, or an organism nickname.
  (setq orgf (canonicalize-organism-designator organism))

  ;; check to see if this organism has been preloaded
  (unless orgf 
    (error (one-string 
            "The organism denoted by '~A' either hasn't yet been preloaded "
            "or is not a known organism name or nickname.") organism))
  (setq orgn (fname orgf))

  ;; make sure this organism's prefix doesn't match 
  ;; that of any already-loaded organisms
  (vwhen (prefix (slotv orgf #$Organism-Prefix))
    (loop for existing-organism in *loaded-organisms* 
          as existing-prefix = 
          (slotv existing-organism #$organism-prefix)
          when 
          (and (not (eq existing-organism orgf)) 
               existing-prefix
               (string-equal prefix existing-prefix)) 
          do 
          (error
           (one-string-nl
            "Organism being loaded, ~A, has the same prefix, ~S"
            "as an existing organism ~A (prefix ~S)")
           orgf prefix existing-organism existing-prefix
           )))

  (vformatt "Now repopulating organism slots from plist data.")
  (populate-organism-frame-slots-from-plist-data orgf)
    
  (let ((directory-invalid? nil))

    (flet ((flag-n-warn (warn-string &rest warn-args)
             (apply 'warn warn-string warn-args)
             (setq directory-invalid? t)))

      ;; genome/genome.fasta
      (unless (probe-file (organism-fasta-file orgf "genome"))
        (flag-n-warn
         "There is no FASTA file in the GENOME directory for organism ~A"
         orgn))
          
      (unless (or (#^no-genes-or-proteins? orgf) (#^no-proteins? orgf))

        ;;proteins/proteins.fasta
        (unless (probe-file (organism-fasta-file orgf "proteins"))
          (flag-n-warn
           "There is no FASTA file in the PROTEINS directory for organism ~A"
           orgn))
        )
      
      (when directory-invalid? 
        (error "Fatal error: Invalid directory structure for organism ~A" orgn)
        )))
        
  (let ((count (load-the-seed-genome-into-frames orgf)))
    (vformatt "Genome Done, loaded ~D contigs" count))

  (unless (#^no-genes-or-proteins? orgf) 
    (let ((count (load-the-seed-genes-into-frames orgf)))
      (vformatt "Genes Done, loaded ~D genes" count)))
  
  (unless (or (#^no-genes-or-proteins? orgf) (#^no-proteins? orgf))
    (let ((count (load-the-seed-proteins-into-frames orgf)))
      (vformatt "Proteins Done, loaded ~D proteins" count)))

  (vformatt "Organism ~A loaded successfully." orgf)
  
  (when postload? 
    (do-organism-postload-processing orgf verbose?))

  (unless (#^no-genes-or-proteins? orgf)
    (when data-structures? 
      (when verbose? (cformatt "Creating organism data structures"))
      (create-organism-data-structures 
       :organisms (list orgf) :verbose? verbose?
       )))

  (setf (slotv orgf #$organism-loaded?) T)

  (when pprint (pprint-organism orgf pprint-limit))
  
  (when test? (test-seed-organism orgf))

  (pushnew orgf *loaded-organisms*)

  orgf
  )

(defun load-the-seed-genome-into-frames 
       (orgf
        &aux 
        (dirname "genome") (contigs nil) (org-prefix (#^organism-prefix orgf)))
  
  (setf (slotv orgf #$Contiguous-Sequences) nil)

  ;; Create a frame for each contig, fill in the appropriate slots

  (loop for (seed-contig-name contig-length) in *genome-info*
        as bike-contig-name = (s+ org-prefix seed-contig-name)
        as contig-frame =
        (frame-fnamed bike-contig-name t)
        do
        (setf (#^sequence-length contig-frame) contig-length)
        (setf (#^organism-entity-type contig-frame) #$contiguous-sequence)
        (setf (#^organism contig-frame) orgf)
        (setf (#^seed-id contig-frame) seed-contig-name)
        (push contig-frame contigs)
        )
  (setq contigs (nreverse contigs))
  (setf (slotv orgf #$contiguous-sequences) contigs)
  
  ;; create the sequence and seqidx files if necessary
  (let ((fasta-file (organism-fasta-file orgf dirname)))
    (seed-organism-fastas-to-index-and-seq-files
     orgf (#^fname orgf) dirname (list fasta-file) :rebuild? t
     :data-char-predicate 
     (let ((pred (#^genome-char-predicate orgf))) 
       (or (and pred (intern (string pred) :bio))
           'allowable-genome-char-finished?
           ))))
        
  (let* ((index-data (read-organism-seqidx-file orgf dirname))
         (sequenced-contig-names (mapcar 'first index-data))
         (mismatches nil))
          
    ;; Make sure the contig names which came from the Fasta file
    ;; match the ones that came from the mysql database
    (unless (= (length *genome-info*) (length index-data))
      (error 
       (one-string
        "The number of contiguous sequences in the genome.fasta file"
        " is not the same as the number downloaded from SEED.")))
    (loop for (seed-contig-name nil) in *genome-info* 
          as biobike-contig-name = (s+ org-prefix seed-contig-name)
          do
          (unless (find biobike-contig-name sequenced-contig-names 
                        :test 'string-equal)
            (push seed-contig-name mismatches)
            ))
    (limited-errors-reported 
     (find-duplicates contigs)
     10 "And more duplicates..." 
     (lambda (x) (cformatt "Duplicate contig ~A found" x)))
    (limited-errors-reported 
     mismatches
     10 "And more mismatches..." 
     (lambda (x) 
       (cformatt
        "Contig named ~A found in mysql database but not in .fasta file" x)))
    (when mismatches 
      ;; CHANGED ERROR MESSAGE 
      (error "Fatal error processing genome ~A.  Cannot continue." orgf))
        
    (store-internal-sequence-info-and-seqlen index-data)
          
    (let ((seqfile (seqinfo-seq-file (#^fname orgf) dirname)))
      (set-genome-sequence-file orgf seqfile)
      (set-genome-sequence-stream orgf nil)
      ))

  (length contigs)
  
  )

(defun load-the-seed-genes-into-frames (orgf &aux (dirname "genes"))
  (declare (ignore dirname))
  (setf (slotv orgf #$Genes) nil)
  (let ((contigs (slotv orgf #$Contiguous-Sequences))
        (genes nil)
        (org-prefix (slotv orgf #$organism-prefix))
        (gid (#^seed-id orgf)))
    (let ((dir-errors nil)
          (from-errors nil)
          (to-errors nil))
      ;; peg/rna contig-name encodes-protein direction from to description      
      (loop for (gene-name contig-name encodes-protein direction from to desc)
            in *genes-info*
            for count from 0
            do
            ;; make sure contig associated with gene exists and belongs
            ;; to the organism.  sanity check.  
            (let ((contig-frame 
                   (frame-fnamed (one-string org-prefix contig-name))))
              (unless contig-frame
                (error 
                 (one-string
                  "Gene ~A in organism ~A " 
                  "is said to belong to a contiguous sequence "
                  "named ~A which in fact does not exist.")
                 gene-name orgf contig-name))
              (unless (find contig-frame contigs)
                (error 
                 (one-string
                  "Ruh roh. Gene ~A belongs to contiguous segment named "
                  "~A which is not present in the organism.")
                 gene-name contig-name
                 ))
              ;; error checks on direction, from, and to
              (unless (or (eq direction :b) (eq direction :f))
                (push (list gene-name direction) dir-errors))
              (let ((contig-length (slotv contig-frame #$sequence-length)))
                (if (integerp from) 
                    (if (and (>= from 1) (<= from contig-length))
                        nil
                      (push (list gene-name from "FROM is not within range") 
                            from-errors
                            ))
                  (push (list gene-name from "FROM is not an integer")
                        from-errors)
                  )
                (if (integerp to) 
                    (if (and (>= to 1) (<= to contig-length))
                        nil
                      (push (list gene-name to "TO is not within range") 
                            to-errors
                            ))
                  (push (list gene-name from "TO is not an integer") to-errors)
                  ))
              (let* ((bike-gene-name 
                      (seed-peg-name-to-biobike-name gene-name gid))
                     (bike-gene-name-with-prefix 
                      (one-string org-prefix bike-gene-name))
                     (gene-frame 
                      #-:sframes
                      (fff bike-gene-name-with-prefix)
                      #+:sframes
                      (frame-fnamed bike-gene-name-with-prefix t 'seed-gene)
                      ))
                (setf (slotv gene-frame #$Organism-Entity-Type) #$Gene)
                (setf (slotv gene-frame #$Organism) orgf)
                (setf (slotv gene-frame #$Contiguous-Sequence) contig-frame)
                (setf (slotv gene-frame #$Direction) direction)
                (setf (slotv gene-frame #$From) from)
                (setf (slotv gene-frame #$To) to)
                (setf (slotv gene-frame #$seed-id) gene-name)
                (when desc (setf (slotv gene-frame #$Description) desc))
                (when encodes-protein 
                  (setf (slotv gene-frame #$encodes-protein) t))
                (push gene-frame genes)
                (when (and *seed-load-verbose* (zerop (mod count 1000)))
                  (format t ".")
                  )))
              
            (limited-errors-reported 
             (find-duplicates genes)
             10 "And more duplicates..." 
             (lambda (x) 
               (cformatt "*** Duplicate gene ~A found in mysql database!" x)))
        
            (limited-errors-reported 
             dir-errors
             10 "And more direction errors..."
             (lambda (x)
               (cformatt "Invalid direction ~A in gene ~A" 
                         (second x) (first x))))

            (limited-errors-reported
             from-errors
             10 "And more from-errors..."
             (lambda (x)
               (cformatt "Invalid FROM-entry ~A in gene ~A: ~A" 
                         (second x) (first x) (third x))))

            (limited-errors-reported
             to-errors
             10 "And more to-errors..."
             (lambda (x)
               (cformatt "Invalid TO-entry ~A in gene ~A: ~A"  
                         (second x) (first x) (third x))))

            (when (or dir-errors from-errors to-errors)
              (error "Fatal error(s) in processing genes of organism ~A." orgf))

            ))

    (length (setf (slotv orgf #$Genes) (nreverse genes)))

    ))


(defun load-the-seed-proteins-into-frames (orgf &aux (dirname "proteins"))

  (setf (slotv orgf #$Proteins) nil)
  ;; Get all the relevant data out of the sequence and information
  ;; tables for the proteins for this organism.
  (let* ((orgn (fname orgf))
         (fasta-file (organism-fasta-file orgf dirname))
         ;; Create index files for protein fasta file if necessary
         (dummy
          (seed-organism-fastas-to-index-and-seq-files 
           orgf orgn dirname (list fasta-file) 
           :rebuild? t
           :data-char-predicate 
           (let ((pred (#^protein-char-predicate orgf))) 
             (or (and pred (intern (string pred) :bio))
                 'allowable-protein-char-finished?))))
         ;; read the index files 
         (index-data (read-organism-seqidx-file orgf dirname))
         ;; these are the biobike names of the proteins, 
         ;; having the organism prefix and a 'p-' prepended.  
         ;; These names are conjured up when the seqidx file is created.
         (sequenced-protein-names (mapcar 'first index-data))
         ;; Initialize various.
         (missing-genes-list nil)
         (proteins-from-fasta-file nil)
         )
    (declare (ignore dummy))

    ;; Process each protein in the protein sequences table,
    ;; creating a frame for it and putting these protein frames in a 
    ;; special hash table.
    (loop for name in sequenced-protein-names
          for count fixnum from 0
          do
          (let ((protein-frame (fff name)))
            (push protein-frame proteins-from-fasta-file)
            (setf (slotv protein-frame #$Organism-Entity-Type) #$Protein)
            (setf (slotv protein-frame #$Organism) orgf)
            (when (and *seed-load-verbose* (zerop (mod count 1000)))
              (format t "."))
            ))
    (when *seed-load-verbose* (terpri))

    ;; make sure the gene frame for every protein exists
    ;; and associate the two
    (loop for name in sequenced-protein-names
          as protein-frame = (frame-fnamed name)
          as parse = (string-split name #\.)
          as org-prefix = (first parse)
          as gene = (subseq (second parse) 2)
          as gene-frame-name =
          (string-join (list* org-prefix gene (cddr parse)) #\.)
          do
          (let ((gene-frame (frame-fnamed gene-frame-name)))
            (if (and gene-frame (eq #$gene (#^organism-entity-type gene-frame)))
                (progn
                  (setf (slotv gene-frame #$proteins)
                        (pushnew protein-frame (slotv gene-frame #$proteins)))
                  (setf (slotv protein-frame #$gene) gene-frame))
              (push (list name gene-frame-name) missing-genes-list)
              )))
  
    (limited-errors-reported 
     (find-duplicates proteins-from-fasta-file)
     10 "And more duplicates..." 
     (lambda (x) 
       (cformatt "*** Duplicate protein ~A found in .fasta file" x))) 

    (limited-errors-reported
     missing-genes-list
     10 "And more unknown genes..."
     (lambda (x) 
       (let ((protein (car x))
             (gene (cadr x)))
         (cformatt 
          (one-string
           "There is no gene ~A corresponding to the "
           "protein ~A found in the biobike fasta file ~A")
          gene protein fasta-file
          ))))

    (when missing-genes-list 
      (error 
       "Fatal error in processing proteins of organism ~A. Can't continue."
       orgf))
        
    (store-internal-sequence-info-and-seqlen index-data)

    (let ((seqfile (seqinfo-seq-file orgn dirname)))
      (set-protein-sequence-file orgf seqfile))
               
    (setf (slotv orgf #$proteins) (nreverse proteins-from-fasta-file))
    ;; rearrange the proteins so that they appear in the same order as the
    ;; order the genes they correspond to appear.  
    (cformatt "Rearranging proteins in order of their corresponding genes...")
    (setf (slotv orgf #$proteins) (put-proteins-in-genes-order orgf))
    
    (length (slotv orgf #$proteins))

    ))

(defun add-annotations-to-seed-gene-frames (gid)
  (case *seed-access-mode*
    (:soap nil)
    (:mysql
     (utils::formatt "Now storing annotation info for organism ~A~%" gid)
     (let* ((orgf (frames::frame-fnamed (seed-gid-to-biobike-orgname gid)))
            (orgprefix (#^organism-prefix orgf))
            (oopslist nil)
            (annotation-file (seed-annotation-file gid)))
       (when (probe-file annotation-file)
         (setf (#^annotations-file orgf) annotation-file))
       (maphash 
        (lambda (pegid annotations)
          (let* ((biobike-peg-name (seed-peg-name-to-biobike-name pegid gid))
                 (biobike-peg-frame-name 
                  (utils::s+ orgprefix biobike-peg-name)))
            (utils::vif
             (frame (frames::frame-fnamed biobike-peg-frame-name))
             (setf (frames::slotv frame #$seed-annotation-history) annotations)
             (push (list biobike-peg-frame-name pegid) oopslist)
             )))
        *annotations-hash*
        )
       (when oopslist 
         (loop for (missing-frame-name pegid) in oopslist
               for k from 1
               do
               (if (<= k 10)
                   (cformatt 
                    "No gene frame '~A' (for ~A), but an annotation exists!"
                    missing-frame-name pegid
                    )
                 (progn
                   (cformatt 
                    "... And ~D more annotations for non-existent genes!"
                    (- (length oopslist) 10))
                   (return nil)
                   )))
         (setf (#^annotated-nonexistent-pegs orgf) (mapcar 'second oopslist))
         )))))


(defun seed-protein-fasta-to-biobike-fasta (file gid)
  (let ((lines (utils::file-to-string-list file))
        (keys nil))
    (with-open-file 
        (p file :direction :output
           :if-exists :supersede :if-does-not-exist :error)
      (loop for line in lines 
            do
            (if (and (plusp (length line)) (char= #\> (char line 0)))
                (let* ((header 
                        (string-trim utils::*whitespace* (subseq line 1)))
                       (tokens (string-split header #\Space))
                       (peg-name (first tokens))
                       (rest-of-header-string
                        (string-join (cdr tokens) #\Space))
                       (biobike-peg-name  
                        (seed-peg-name-to-biobike-name peg-name gid)))
                  (push biobike-peg-name keys)
                  (write-line 
                   (utils::s+ ">p-" biobike-peg-name " " rest-of-header-string)
                   p
                   ))
              (write-line line p)
              )))
    (let ((dups (check-for-duplicates keys :test 'equalp)))
      (loop for dup in dups do
            (warn "Duplicate peg ~A found in seed fasta file ~A" dup file)
            ))))

(defun all-useful-seed-genome-info ()
  (let* ((genome-fields
          (one-string
           "genome,gname,szdna,maindomain,pegs,rnas,"
           "complete,restrictions,taxonomy"
           ))
         (slot-names (string-split genome-fields #\,))
         (data (seed-query (s+ "select " genome-fields " from genome"))))
    (labels ((toint (x) 
               (let ((result (ignore-errors (parse-integer x))))
                 (if (null result) -1 result)
                 ))
             (toboolean (x)
               (let ((result (toint x)))
                 (case result
                   (-1 nil)
                   (0 nil)
                   (otherwise t)
                   ))))
      (values 
       (mapcar 
        (lambda (x) 
          (list 
           (first x) (second x) (third x)
           (fourth x) (fifth x) (sixth x)
           (toboolean (seventh x)) (toboolean (eighth x))
           (ninth x)
           ))
        data
        )
       slot-names
       ))))