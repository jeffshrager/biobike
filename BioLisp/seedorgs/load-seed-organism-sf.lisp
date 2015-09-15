;;; -*- Package: bio; mode: lisp; base: 10; Syntax: Common-Lisp; -*-

(in-package :bio)

;;; +=========================================================================+
;;; | Copyright (c) 2009 JP Massar                                            |
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

(defun load-seed-organism-sf
       (gid &key (verbose? *seed-load-verbose*) (commit? t) (reload? nil)
            &aux orgf)
  (block exit
    (setq orgf (canonicalize-seed-org-designator gid))
    (when (and (null reload?) (#^organism-loaded? orgf)) 
      (cformatt "Organism ~A already loaded..." orgf)
      (return-from exit nil))
    (cformatt "Loading organism ~A designated by ~A" orgf gid)
    (let ((*seed-load-verbose* verbose?)
          (*disable-seed-organism-autoload* t)  
          (*seed-access-mode* :mysql)
          (*acache-organism-dir* (#^organism-data-directory orgf))
          (*bike-genome-name* (fname orgf)))
      (let ((*pegs-for-gid* nil)
            (*genome-info* nil)
            (*seed-genome-sequence-file* nil)
            (*genes-info* nil)
            (*genes-descriptions* nil)
            (*proteins-info* nil)
            (*seed-proteins-sequence-file* nil)
            (*seed-gene-annotations-file* nil)
            (*annotations-hash* nil)
            (*plist-info* nil)
            (*acache-organism-dir* (#^organism-data-directory orgf))
            (*acache-genome-dir* 
             (namestring (append-subdir *acache-organism-dir* "genome")))
            (*acache-proteins-dir* 
             (namestring (append-subdir *acache-organism-dir* "proteins")))
            (*acache-seqinfo-dir*
             (namestring (append-subdir *acache-organism-dir* "seqinfo"))))
        (ensure-directories-exist *acache-organism-dir*)
        (ensure-directories-exist *acache-genome-dir*)
        (ensure-directories-exist *acache-proteins-dir*)
        (ensure-directories-exist *acache-seqinfo-dir*)
        (new-download-seed-data-for-gid-into-vars orgf)
        (new-create-seed-organism-frames orgf)
        (progn
          (remove-organism-directory orgf)
          (when commit?
            (vformatt "Committing organism to acachedb...~%")
            (db.ac::commit)
            ))
        orgf
        ))))

(defun new-download-seed-data-for-gid-into-vars (orgf)
  (let* ((orgn (fname orgf))
         (seed-id (#^seed-id orgf)))
    (vformatt 
     "You have asked to get download data for Seed organism ~A, (GID: ~A)." 
     orgf (#^seed-id orgf))
    (vformatt "Now getting plist information for ~A" orgf)
    (setq *plist-info* (new-get-seed-info-for-plist seed-id))
    (vformatt "  Official seed name: ~A, main domain: ~A"
              (second (assoc :official-seed-name *plist-info*))
              (second (assoc :maindomain *plist-info*))
              )
    (vformatt "Done.")
    (vformatt "Now getting genome.tbl info for organism ~A." orgf)
    (setq *genome-info* (get-seed-info-for-genome-tbl seed-id orgn))
    (vformatt "Done.")
    (vformatt "Now getting genome.fasta info for organism ~A. " orgf)
    (new-get-seed-info-for-genome-fasta seed-id)
    (vformatt "Done.")
    (vformatt "Now getting genes info for organism ~A. " orgf)
    (multiple-value-setq (*genes-info* *genes-descriptions* *annotations-hash*)
        (new-get-seed-info-for-genes-tbl seed-id))
    (vformatt "Done.")
    (vformatt "Now getting proteins.fasta info for organism ~A. " orgf)
    (new-get-seed-info-for-protein-fasta seed-id)
    (vformatt "Done.")
    (vformatt "All information downloaded.")
    nil
    ))

(defun new-get-seed-info-for-plist (gid)
  (let ((plist nil))
    (flet ((add (key value) (push (list key value) plist)))
      (add :submitter '("Rob Edwards" "Seed"))
      (add :links 
           '("http://ws.nmpdr.org/" "http://www.theseed.org/wiki/Glossary#PEG"))
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

(defun new-get-seed-info-for-genome-fasta (gid)
  (let ((fasta-file (merge-pathnames "genome.fasta" *acache-genome-dir*)))
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
                  (absolute-seed-path-from-fileno first-fileno)))
            (vformatt "Retrieving genome fasta info from ~A" absolute-filepath)
            (vformatt "Copying genome fasta info to ~A" fasta-file)
            (let ((data 
                   (utils::file-to-string
                    absolute-filepath :max 50000000)))
              (unless data 
                (error "Genome fasta file ~A too big!  Cannot continue!"
                       absolute-filepath))
              ;; Save to store in organism frame eventually
              (setq *seed-genome-sequence-file* absolute-filepath)
              (loop for ch across data do (write-char ch p))
              )))))))

(defun new-get-seed-info-for-genes-tbl (gid)
  (let* ((info 
          (progn
            (vformatt "Extracting info for genes from features table")
            (seed-gene-info-for-gid-sf gid)
            ))
         (descriptions 
          (progn
            (vformatt 
             "Extracting descriptions for genes using assigned_functions table")
            (mysql-get-descriptions-for-pegs-of-genome 
             (setq *pegs-for-gid* (mapcar 'first info))
             )))
         (annotations-hash 
          (progn
            (vformatt
             "Extracting annotations for genes from annotation disk files.")
            (get-seed-info-for-annotations gid)
            )))
    (unless *pegs-for-gid* 
      (error "No pegs exist for organism with gid = ~A!  Cannot continue." gid))
    (values info descriptions annotations-hash)
    ))

(defun process-seed-gene-info (orgf org-prefix gene-prefix pos gene-info)
  (destructuring-bind (id contig location type aliases) gene-info 
    gene-info
    (let* ((numeric-ordering-name (s+ gene-prefix "-" (formatn "~4,'0d" pos)))
           (gene-name 
            (s+ 
             org-prefix
             (or (determine-best-alias orgf aliases) numeric-ordering-name)
             )))
      (when (find #\, location)
        (cformatt "Gene ~A has multiple segments: ~A" gene-name location))
      (destructuring-bind (xcontig from to direction segments wraps?)
          (new-parse-seed-location-info location)
        (declare (ignore xcontig))
        (list
         gene-name
         (s+ org-prefix numeric-ordering-name)
         contig
         ;; encodes-protein test
         (string-equal "peg" type)
         direction 
         from
         to
         id
         (and aliases (string-split aliases #\,))
         location
         segments
         wraps?
         )))))

(defvar *enable-gene-pattern* nil)

(defun determine-best-alias (orgf seed-alias-string)
  (block exit
    (unless (zerop (length seed-alias-string))
      (let ((aliases (string-split seed-alias-string #\,)))
        ;; If the seed-organism-table provides us with a pattern to match
        ;; against the aliases, do that, otherwise see if the first alias
        ;; is appropriate and if so use that, otherwise give up for now.  
        (vif (gp (and *enable-gene-pattern* (#^gene-pattern orgf)))
             (let ((compiled-pattern (ppcre::create-scanner gp)))
               (loop for alias in aliases 
                     do
                     (multiple-value-bind (start end)
                         (ppcre::scan compiled-pattern alias)
                       (when (and start end 
                                  ;; Jeff says any partial match is OK
                                  ;; (= start 0)
                                  ;; (= end (length alias))
                                  )
                         (return-from exit alias)
                         )))
               nil
               )
             (if (seed-peg-alias-ok? (first aliases))
                 (first aliases)
               nil
               ))))))

(defun new-create-seed-organism-frames 
       (orgf
        &key 
        (verbose? *seed-load-verbose*)
        (postload? t)
        (data-structures? t)
        (test? t)
        &aux
        (orgn (fname orgf))
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
 
  (vformatt "Now starting to load ~A" orgf)

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
        
  (vformatt "Loading seed genome into frames")
  (let ((count (new-load-the-seed-genome-into-frames orgf)))
    (vformatt "Genome Done, loaded ~D contigs" count))
  (setf (#^seed-genome-sequence-file orgf) *seed-genome-sequence-file*)

  (vformatt "Loading seed genes into frames")
  (unless (#^no-genes-or-proteins? orgf) 
    (let ((count (new-load-the-seed-genes-into-frames orgf)))
      (vformatt "Genes Done, loaded ~D genes" count)))
  (setf (#^seed-gene-annotations-file orgf) *seed-gene-annotations-file*)
  
  (vformatt "Loading seed proteins into frames")
  (unless (or (#^no-genes-or-proteins? orgf) (#^no-proteins? orgf))
    (let ((count (new-load-the-seed-proteins-into-frames orgf)))
      (vformatt "Proteins Done, loaded ~D proteins" count)))
  (setf (#^seed-proteins-sequence-file orgf) *seed-proteins-sequence-file*)

  (vformatt "Organism ~A loaded successfully." orgf)
  
  (when postload? 
    (do-organism-postload-processing orgf verbose?))

  (unless (#^no-genes-or-proteins? orgf)
    (when data-structures? 
      (vformatt "Creating organism data structures")
      (create-organism-data-structures 
       :organisms (list orgf) :verbose? verbose?
       )))

  (setf (slotv orgf #$organism-loaded?) T)

  (when test? (test-seed-organism orgf))

  (pushnew orgf *loaded-organisms*)

  orgf
  )

(defun new-load-the-seed-genome-into-frames 
       (orgf
        &aux 
        (dirname "genome")
        (contigs nil)
        (org-prefix (#^organism-prefix orgf)))
  
  ;; Create a frame for each contig, fill in the appropriate slots

  (loop for (seed-contig-name contig-length) in *genome-info*
        as bike-contig-name = (s+ org-prefix seed-contig-name)
        as contig-frame =
        (frame-fnamed bike-contig-name t 'seed-contiguous-sequence)
        do
        (setf (#^sequence-length contig-frame) contig-length)
        (setf (#^organism-entity-type contig-frame) #$contiguous-sequence)
        (setf (#^genes-sorted-by-position contig-frame) nil)
        (setf (#^fragments-sorted-by-position contig-frame) nil)
        (setf (#^organism contig-frame) orgf)
        (setf (#^seed-id contig-frame) seed-contig-name)
        (push contig-frame contigs)
        )
  (setq contigs (nreverse contigs))

  ;; create the sequence and seqidx files if necessary
  (let ((fasta-file (merge-pathnames "genome.fasta" *acache-genome-dir*)))
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
      (error "Fatal error processing genome ~A.  Cannot continue." orgf))
        
    (store-internal-sequence-info-and-seqlen index-data)
          
    (let ((seqfile (seqinfo-seq-file (#^fname orgf) dirname)))
      (setf (slotv orgf #$Genome-sequence-file) (namestring seqfile))
      ;; Probably not needed.
      (setf (slotv orgf #$Genome-sequence-stream) nil)
      ))

  (let* ((block-data 
          (progn
            (vformatt "Creating contiguous sequence block frames...")
            (loop
             for contig in contigs 
             with *extract-sequence-info-bootstrap* = t
             collect
             (list 
              contig
              (let ((sequence-blocks 
                     (break-contig-sequence-into-blocks 
                      contig
                      ;; Extract the entire sequence
                      ;; make all seed sequences uppercase 
                      ;; using acahe as per elhai request
                      (string-upcase 
                       (extract-contig-sequence
                        contig 1 (#^sequence-length contig) :f
                        )))))
                (setf (#^n-sequence-blocks contig) (length sequence-blocks))
                sequence-blocks
                )))))
         (final-contigs 
          (progn
            (vformatt "Creating final (possibly renamed) contig frames")
            (loop 
             for contig in contigs 
             for contig-block-data in block-data 
             collect
             (maybe-create-new-frame-names-for-contig 
              orgf contig contig-block-data
              )))))
    (setf (#^contiguous-sequences orgf) final-contigs)
    (length final-contigs)
    ))

;; (gene-name contig-name encodes-protein direction from to)

(defun new-load-the-seed-genes-into-frames (orgf)
  (let* ((contigs (slotv orgf #$Contiguous-Sequences))
         (genes nil)
         (org-prefix (slotv orgf #$organism-prefix))
         (gene-prefix 
          (or 
           ;; if a gene prefix has been provided, strip the trailing '-'
           ;; if present, because it gets put back in process-seed-gene-info.
           ;; otherwise use the organism prefix without the trailing '.'
           (vwhen (x (#^gene-prefix orgf))
             (if (char-equal #\- (lastelem x))
                 (subseq x 0 (1- (length x)))
               x
               ))
           (subseq org-prefix 0 (1- (length org-prefix)))))
         (gene-name-hash (make-string-equal-hash-table)))
    (let ((dir-errors nil) (from-errors nil) (to-errors nil)
          (gene-info-errors nil))
      (loop 
       for gene-info in *genes-info*
       for desc in *genes-descriptions*
       for count from 1
       do
       (block exit
         (destructuring-bind
             (gene-name numeric-ordering-name contig-name encodes-protein 
                        direction from to id aliases location segments wraps?)
             (handler-case
                 (process-seed-gene-info
                  orgf org-prefix gene-prefix count gene-info)
               (error 
                (c)
                (push (list c gene-info) gene-info-errors)
                (return-from exit nil)
                ))
           ;; make sure contig associated with gene exists and belongs
           ;; to the organism.  sanity check.  
           (let ((contig-frame 
                  (find 
                   contig-name contigs :key #^seed-id :test 'string-equal
                   )))
             (unless contig-frame
               (error 
                (one-string
                 "Gene ~A with seed id ~A in organism ~A " 
                 "is said to belong to a contiguous sequence "
                 "with a seed id of ~A but in fact no contig of "
                 "organism ~A has that seed id.")
                gene-name id orgf contig-name orgf
                ))
             ;; make sure a gene of the same name doesn't already exist
             (vwhen (dup (gethash gene-name gene-name-hash))
               (cformatt 
                (one-string
                "Duplicate gene name: ~A, with gene info~%"
                "  ~S~%"
                "  same as ~A (~A),~%"
                "  Using the name ~A (derived from numeric ordering) instead.")
                gene-name gene-info dup (#^seed-id dup) numeric-ordering-name)
               (setq gene-name numeric-ordering-name))
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
                 (push 
                  (list gene-name from "FROM is not an integer")
                  from-errors)
                 )
               (if (integerp to) 
                   (if (and (>= to 1) (<= to contig-length))
                       nil
                     (push 
                      (list gene-name to "TO is not within range") to-errors)
                     )
                 (push (list gene-name from "TO is not an integer") to-errors)
                 ))
             (let* ((bike-gene-name-with-prefix gene-name)
                    (gene-frame 
                     (frame-fnamed 
                      bike-gene-name-with-prefix t 'seed-gene
                      ))
                    (gene-annotation (gethash id *annotations-hash*)))
               (setf (gethash gene-name gene-name-hash) gene-frame)
               (setf (slotv gene-frame #$Organism-Entity-Type) #$Gene)
               (setf (slotv gene-frame #$Organism) orgf)
               (setf (slotv gene-frame #$Contiguous-Sequence) contig-frame)
               (setf (slotv gene-frame #$Direction) direction)
               (setf (slotv gene-frame #$From) from)
               (setf (slotv gene-frame #$To) to)
               (setf (slotv gene-frame #$seed-id) id)
               (when desc (setf (slotv gene-frame #$Description) desc))
               (when gene-annotation 
                 (setf 
                  (slotv gene-frame #$seed-annotation-history)
                  gene-annotation)
                 )
               (when encodes-protein 
                 (setf (slotv gene-frame #$encodes-protein) t))
               (when aliases (setf (slotv gene-frame #$aliases) aliases))
               (when location (setf (slotv gene-frame #$location) location))
               (when segments (setf (slotv gene-frame #$segments) segments))
               ;; if any gene wraps, then its contig must be circular
               (when wraps? (setf (slotv contig-frame #$circular) wraps?))
               (push gene-frame genes)
               (when (and *seed-load-verbose* (zerop (mod count 1000)))
                 (format t ".")
                 ))))))

      (limited-errors-reported 
       gene-info-errors 
       10 "And more bad gene data..."
       (lambda (x)
         (cformatt "Error processing gene info: ~S" (second x))
         (cformatt "Actual error: ~A" (first x))
         ))
              
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

      )

    (length (setf (slotv orgf #$Genes) (nreverse genes)))

    ))

(defun new-get-seed-info-for-protein-fasta (gid)
  (let ((proteins-fasta-file 
         (merge-pathnames "proteins.fasta" *acache-proteins-dir*)))
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
              (or peg (error "No pegs found for gid ~A" gid))
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
                   ))))
      (vformatt "Obtaining protein fasta info from ~A" absolute-filepath)
      (vformatt "Copying protein fasta info to ~A" proteins-fasta-file)
      ;; read the seed fasta file in
      (let ((data (utils::file-to-string absolute-filepath :max 17000000)))
        (unless data 
          (error "Proteins fasta file ~A too big!  Cannot continue!"
                 absolute-filepath))
        ;; Store for later saving in organism frame
        (setq *seed-proteins-sequence-file* absolute-filepath)
        ;; and write a copy back out
        (with-open-file 
            (p proteins-fasta-file
               :direction :output
               :if-does-not-exist :create
               :if-exists :supersede)
          (loop for ch across data do (write-char ch p))
          )))
    nil
    ))

(defun new-load-the-seed-proteins-into-frames (orgf &aux (dirname "proteins"))

  (let* ((orgn (fname orgf))
         (fasta-file (merge-pathnames "proteins.fasta" *acache-proteins-dir*))
         (proteins-without-peg-genes nil)
         (peg-genes-without-proteins nil)
         (duplicate-fasta-pegs nil)
         (proteins-to-create nil)
         (initial-protein-frames nil)
         (final-protein-frames nil)
         ;; Create index files for the protein fasta file
         (dummy
          (seed-organism-fastas-to-index-and-seq-files 
           orgf orgn dirname (list fasta-file) 
           :rebuild? t
           :data-char-predicate 
           (let ((pred (#^protein-char-predicate orgf))) 
             (or (and pred (intern (string pred) :bio))
                 'allowable-protein-char-finished?))))
         ;; read the index file, separate data into protein names and 
         ;; index information
         (index-data (read-organism-seqidx-file orgf dirname))
        (sequenced-protein-names (mapcar 'first index-data))
         (index-info-list (mapcar 'cdr index-data))
         ;; strip off organism prefix from the names in the index file
         (peg-names
          (mapcar
           (lambda (x) (string-after-first x #\.))
           sequenced-protein-names
           ))
         ;; find any duplicate entries in the fasta/index file 
         (peg-proteins-hash 
          (loop for peg-name in peg-names 
                with hash = (make-string-equal-hash-table)
                do
                (if (gethash peg-name hash) 
                    (push peg-name duplicate-fasta-pegs)
                  (setf (gethash peg-name hash) t))
                finally (return hash)
                ))
         (peg-genes-hash
          (loop for g in (#^genes orgf)
                with hash = (make-string-equal-hash-table)
                do
                (when (#^encodes-protein g) 
                  (setf (gethash (#^seed-id g) hash) g))
                finally (return hash)
                ))
         )
    (declare (ignore dummy))
    
    ;; find any proteins (pegs) in the proteins fasta file 
    ;; that there are no corresponding genes for
    (loop 
     for peg-name in peg-names 
     for index-info in index-info-list 
     do 
     (let ((peg-gene (gethash peg-name peg-genes-hash)))
       (if peg-gene
           (push (list peg-name peg-gene index-info) proteins-to-create)
         (push peg-name proteins-without-peg-genes)
         )))

    ;; find any protein-encoding genes for which there is no
    ;; entry in the proteins fasta file 
    (maphash 
     (lambda (peg-name gene-frame) 
       (unless (gethash peg-name peg-proteins-hash)
         (push gene-frame peg-genes-without-proteins)
         ))
     peg-genes-hash
     )

    (limited-errors-reported 
     duplicate-fasta-pegs
     10 "And more duplicates..." 
     (lambda (x) 
       (cformatt "*** Duplicate protein ~A found in .fasta file" x)))

    (limited-errors-reported
     proteins-without-peg-genes
     10 "And more proteins without corresponding genes..."
     (lambda (protein)
       (cformatt 
        (one-string
         "There is no gene corresponding to the "
         "protein ~A found in the proteins fasta file ~A")
        protein fasta-file
        )))

    (limited-errors-reported
     peg-genes-without-proteins
     10 "And more protein-encoding genes without protein fasta file entries..."
     (lambda (peg-frame)
       (cformatt 
        (one-string
         "There is no record in the proteins fasta file "
         "corresponding to the protein-encoding gene ~A (seed-id: ~A)")
        peg-frame (#^seed-id peg-frame)
        )))
    
    (when *seed-load-verbose* 
      (cformatt 
       "Creating ~D initial protein frames" (length proteins-to-create)))
    (loop for (peg-name peg-gene index-info) in proteins-to-create
          for count from 0
          as protein-frame = (frame-fnamed peg-name t 'seed-protein)
          do
          (setf (slotv protein-frame #$Organism-Entity-Type) #$Protein)
          (setf (slotv protein-frame #$Organism) orgf)
          (setf (slotv protein-frame #$gene) peg-gene)
          ;; Taken from store-internal-sequence-info-and-seqlen 
          ;; in .../organisms/organism-load.lisp
          (destructuring-bind (start-header start-data seqlen) index-info
            (setf (slotv protein-frame #$Internal-sequence-info) 
                  (list start-header (- start-data start-header) start-data))
            (setf (slotv protein-frame #$Sequence-Length) seqlen)
            )
          (push protein-frame initial-protein-frames)
          (when (and *seed-load-verbose* (zerop (mod count 200)))
            (format t ".")
            ))
    (when *seed-load-verbose* (terpri))

    ;; Sets up #$protein-sequence-file for extraction
    (let ((seqfile (seqinfo-seq-file orgn "proteins")))
      (set-protein-sequence-file orgf seqfile))

    (when *seed-load-verbose* (cformatt "Extracting protein sequences..."))
    (loop for protein in initial-protein-frames
          with *extract-sequence-info-bootstrap* = t
          do
          (setf (slotv protein #$sequence) (extract-protein-sequence protein))
          )

    (when *seed-load-verbose* (cformatt "Creating final protein frames ..."))
    (loop 
     for p in initial-protein-frames 
     as gene = (#^gene p)
     as gene-name = (fname gene)
     as org-prefix = (string-before-first gene-name #\.)
     as gene-suffix = (string-after-first gene-name #\.)
     as final-name = (s+ org-prefix "." "p-" gene-suffix)
     as final-protein-frame = (frame-fnamed final-name t 'seed-protein)
     do
     (frames::for-each-frame-slot (slot value) p
       (cond
        ((eq slot #$fname) nil)
        ((eq slot #$Internal-sequence-info) nil)
        (t (setf (slotv final-protein-frame slot) value))
        ))
     (let* ((peg-gene (#^gene final-protein-frame))
            (proteins-for-gene (#^proteins peg-gene)))
       (pushnew final-protein-frame proteins-for-gene)
       (setf (#^proteins peg-gene) proteins-for-gene))
     (frames::delete-frame p)
     (push final-protein-frame final-protein-frames))
    
    (setf (slotv orgf #$proteins) final-protein-frames)

    ;; rearrange the proteins so that they appear in the same order as the
    ;; order the genes they correspond to appear.  
    (cformatt "Rearranging proteins in order of their corresponding genes...")
    (setf (slotv orgf #$proteins) (put-proteins-in-genes-order orgf))
    
    (when *seed-load-verbose* (cformatt "All protein frames created..."))

    (length final-protein-frames)

    ))


