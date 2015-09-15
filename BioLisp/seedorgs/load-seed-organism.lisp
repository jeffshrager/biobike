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

;;; Author: JP Massar.

;;; assigned_functions

#-:lispworks
(defun load-all-unloaded-seed-organisms (&key (in-batches-of 50))
  (flet ((list-loaded-organisms (loadlist)
           (loop for org in loadlist
                 for j from 1
                 do
                 (formatt "~A, " org)
                 (when (zerop (mod j 4)) (terpri))
                 )))
    (block exit
      (let ((continue? t)
            (loadlist nil)
            (n 0)
            (*seed-load-verbose* nil))
        (db.ac::doclass*
         (orgf 'bio::seed-organism)
         (when (null (#^organism-loaded? orgf))
           (push orgf loadlist)
           (incf n)
           )
         (when (= n in-batches-of)
           (load-seed-organisms (reverse loadlist))
           (list-loaded-organisms loadlist)
           (setq loadlist nil)
           (setq n 0)
           (setq continue? (yes-or-no-p "Next batch? "))
           )
         (unless continue? (return-from exit nil)))
        (when loadlist 
          (load-seed-organisms (reverse loadlist))
          (list-loaded-organisms loadlist)
          )))))

(defun load-the-next-n-organisms (n)
  (let ((orgs (remove-if #^organism-loaded? (available-organisms))))
    (loop for org in orgs
          for j from 1 to n 
          collect
          (load-organism org)
          )))

      
(defun load-seed-organism-msf
       (gid &key (verbose? *seed-load-verbose*) (commit? t) (reload? nil)
            (private? nil) (dir nil) (remove-temp-acache-dir? nil)
            &aux orgf)
  (declare (ignore dir private?))
  (block exit
    (setq orgf (canonicalize-seed-org-designator gid))
    (when (and (null reload?) (#^organism-loaded? orgf)) 
      (cformatt "Organism ~A already loaded..." orgf)
      (setq *organism-already-loaded?* t)
      (return-from exit orgf))
    (terpri) (terpri)
    (cformatt "****>> Loading organism ~A designated by ~A" orgf gid)
    (let ((*seed-load-verbose* verbose?)
          (*disable-seed-organism-autoload* t)  
          (*seed-access-mode* :mysql)
          (*acache-organism-dir* (#^organism-data-directory orgf))
          (*bike-genome-name* (fname orgf)))
      (let ((*fids-for-gid* nil)
            (*features-info-hash* nil)
            (*genome-info* nil)
            (*seed-genome-sequence-file* nil)
            (*proteins-info* nil)
            (*seed-proteins-sequence-file* nil)
            (*seed-gene-annotations-file* nil)
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
        (cformatt "Downloading data from the seed")
        (download-seed-data-for-gid-into-vars-msf orgf)
        ;; make sure slot methods don't try to access description map
        ;; because it isn't set up yet!  
        (let ((*enable-descriptions-map* nil))
          (cformatt "Creating seed organism frames")
          (create-seed-organism-frames-msf orgf)
          )
        (cformatt "Adding organism data to descriptions map")
        (store-organism-descriptions-into-dmap orgf)
        (cformatt "Organism is loaded")
        (setf (slotv orgf #$organism-loaded?) T)
        (when remove-temp-acache-dir? (remove-organism-directory orgf))
        (when commit?
          (cformatt "Committing organism to acachedb...")
          #-:lispworks
          (db.ac::commit)
          ))
      orgf
      )))

(defun download-seed-data-for-gid-into-vars-msf (orgf)
  (let* ((seed-id (#^seed-id orgf)))
    (cformatt 
     "You have asked to get download data for Seed organism ~A, (GID: ~A)." 
     orgf (#^seed-id orgf))
    (cformatt "  Official seed name: ~A, main domain: ~A"
              (#^gname orgf) (#^maindomain orgf))
    (cformatt "Now getting contig information for organism ~A." orgf)
    ;; This sets *genome-info*
    (get-seed-info-for-genome-tbl-msf seed-id orgf)
    (cformatt "Done.")
    (cformatt "Now getting genome.fasta info for organism ~A. " orgf)
    (get-seed-info-for-genome-fasta-msf seed-id orgf)
    (cformatt "Done.")
    (cformatt "Now getting features info for organism ~A. " orgf)
    ;; This sets *figs-for-gid* and *features-info-hash*
    (get-seed-info-for-genes-tbl-msf seed-id orgf)
    (cformatt "Done.")
    (cformatt "Now getting proteins.fasta info for organism ~A. " orgf)
    (get-seed-info-for-protein-fasta-msf seed-id orgf)
    (cformatt "Done.")
    (cformatt "All information downloaded.")
    nil
    ))

;; Returns a list of lists of the form 
;; (contig-seed-id contig-length &optional contig-ncbi-id 
;;   complete? plasmid? circular? elhai-contig-name)

(defun get-seed-info-for-genome-tbl-msf (org-seed-id orgf)
  (declare (ignore orgf))
  (cformatt "In new get-seed-info-for-genome-tbl-msf")
  (setq
   *genome-info*
   (let ((contig-length-info 
          (get-seed-contig-length-info-for-genome org-seed-id))
         (ncbi-contig-info 
          (get-seed-ncbi-info-for-contigs-of-org org-seed-id))
         (contig-hash (make-string-equal-hash-table)))
     ;; put the info we retrieved from the contig_lengths table
     ;; into the hash table
     (loop 
      for (contig-seed-id contig-length) in contig-length-info
      do
      (setf (gethash contig-seed-id contig-hash) 
            (list contig-seed-id contig-length)))
     ;; add the info we retrieved from the ncbi_contig_info table
     (loop 
      ;; the ncbi info has the organism seed-id as the first
      ;; element, and the contig seed-id as the second element
      for (nil contig-seed-id . ncbi-data) in ncbi-contig-info
      as contig-length-data = (gethash contig-seed-id contig-hash)
      do
      (if contig-length-data 
          (setf (gethash contig-seed-id contig-hash) 
                (append contig-length-data ncbi-data))
        (cformatt 
         (one-string-nl
          "*** This should not happen!"
          "*** The contig id ~A, associated with the organism seed-id ~A"
          "*** in the ncbi_contig_info table, has no corresponding contig id"
          "*** associated with that organism's seed-id in"
          "*** the contig_lengths table!"
          )
         contig-seed-id org-seed-id 
         )))
     (lmaphash 
      (lambda (key contig-data) (declare (ignore key)) contig-data)
      contig-hash
      ))))

(defun get-seed-info-for-genome-fasta-msf (gid orgf)
  ;; Find the seed data file that is the fasta file that contains
  ;; all the sequence information for all the contigs of a gid.  
  ;; Then copy the data in that file to our own directory.  
  (let* ((fasta-file (merge-pathnames "genome.fasta" *acache-genome-dir*))
         (contig-file-info 
          (seed-query
           "select contig,fileno from contig_seeks where genome = ~S"
           gid
           ))
         (first-fileno (second (first contig-file-info))))
    (unless (every (lambda (x) (= (second x) first-fileno)) 
                   contig-file-info)
      (error "All the contig sequences aren't in the same file!")
      )
    (let* ((absolute-filepath 
            (absolute-seed-path-from-fileno first-fileno)))
      (cformatt "Retrieving genome fasta info from ~A" absolute-filepath)
      (cformatt "Copying genome fasta info to ~A" fasta-file)
      (let ((data (utils::file-to-string absolute-filepath :max 50000000)))
        (unless data 
          (error "Genome fasta file ~A too big!  Cannot continue!"
                 absolute-filepath))
        ;; Save to store in organism frame eventually
        (setq *seed-genome-sequence-file* absolute-filepath)
        (with-open-file
            (p fasta-file
               :direction :output
               :if-does-not-exist :create
               :if-exists :supersede)
          (loop for ch across data do (write-char ch p))
          )
        (setf (#^seed-genome-sequence-file orgf) *seed-genome-sequence-file*)
        ))))

(defun get-seed-info-for-genes-tbl-msf (gid orgf)
  (setq *features-info-hash* (make-string-equal-hash-table))
  (multiple-value-bind (features-data column-names)
      (progn
        (cformatt "Extracting info for genes from features table")
        (seed-gene-info-for-gid-msf gid)
        )
    ;; put all the information from the features table into our
    ;; features hash table and create ordered list of feature ids
    (cformatt "Creating features hash table")
    (setq *fids-for-gid* (process-features-info-msf features-data column-names))
    ;; put description information into our features hash table
    (cformatt 
     "Extracting descriptions for genes using assigned_functions table")
    (let ((assigned-functions 
           (descriptions-for-pegs-of-genome-msf *fids-for-gid*)))
      (loop for (fid assigned-function) in assigned-functions
            do
            (set-feature-item fid :subsystem-role assigned-function)
            ))

    ;; parse information from features table and determine 
    ;; an appropriate gene name for each fid 
    (cformatt "Parsing features table information.")
    (process-seed-gene-info-msf orgf)
    ))

(defun process-features-info-msf (features-data column-names)
  (loop with ckeys = (mapcar 'keywordize column-names)
        with id-index = (position :id ckeys)
        for features-record in features-data
        for findex from 0
        as fid = (nth id-index features-record)
        collect fid
        do
        (set-feature-item fid :order findex)
        (loop for ckey in ckeys 
              for item in features-record
              do
              (unless (or (eq ckey :id) (eq ckey :genome))
                (set-feature-item fid ckey item)
                ))))

(defun descriptions-for-pegs-of-genome-msf (fids)
  (mysql-get-descriptions-for-pegs-of-genome-msf fids))

(defun get-seed-info-for-protein-fasta-msf (gid orgf)
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
    (cformatt "Determining seed protein sequence file")
    (let* ((peg 
            (block exit 
              (maphash 
               (lambda (fid plist)
                 (when (string-equal "peg" (get-feature-prop plist :type))
                   (return-from exit fid)
                   ))
               *features-info-hash*
               ))))
      (setq *seed-proteins-sequence-file* nil)
      (setf (#^no-proteins? orgf) t)
      (when peg 
        (let* ((possible-protein-filenos (filenos-for-a-peg peg))
               (absolute-filepath 
                (utils::s+ 
                 user::*seed-mysql-data-root*
                 (loop for fileno in possible-protein-filenos
                       as possible-path = (file-from-fileno fileno)
                       do
                       (when (search gid possible-path :test 'string-equal)
                         (return possible-path))
                       finally
                       (error "No non-master file found for gid ~A" gid)
                       ))))
          (cformatt "Obtaining protein fasta info from ~A" absolute-filepath)
          (cformatt "Copying protein fasta info without duplicates to ~A"
                    proteins-fasta-file)

          (let ((ndups
                 (copy-fasta-file-without-duplicates 
                  absolute-filepath proteins-fasta-file)))
            (when (plusp ndups)
              (cformatt 
               "*** ~D duplicated fasta records found in ~A and removed"
               ndups absolute-filepath
               )))


#||          
          ;; read the seed fasta file in
          (let ((data (utils::file-to-string absolute-filepath :max 17000000)))
            (unless data 
              (error "Proteins fasta file ~A too big!  Cannot continue!"
                     absolute-filepath))
            ;; and write a copy back out
            (with-open-file 
                (p proteins-fasta-file
                   :direction :output
                   :if-does-not-exist :create
                   :if-exists :supersede)
              (loop for ch across data do (write-char ch p))
              )

||#
          (setf (#^no-proteins? orgf) nil)
          ;; Store for later saving in organism frame
          (setq *seed-proteins-sequence-file* absolute-filepath)
          )))
    nil
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun create-seed-organism-frames-msf
       (orgf
        &key 
        (verbose? *seed-load-verbose*)
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
 
  (cformatt "Now starting to load ~A" orgf)

  (let ((genome-directory-invalid? nil)
        (no-proteins-file? nil))
    
    ;; genome/genome.fasta
    (unless (probe-file (organism-fasta-file orgf "genome"))
      (setq genome-directory-invalid? t)
      (cformatt
       "*** There is no FASTA file in the GENOME directory for organism ~A"
       orgn
       ))
          
    ;;proteins/proteins.fasta
    (unless (#^no-proteins? orgf)
      (unless (probe-file 
               (merge-pathnames "proteins.fasta" *acache-proteins-dir*))
        (setq no-proteins-file? t)
        (cformatt
         "*** There is no FASTA file in the PROTEINS directory for organism ~A"
         orgn
         )))
      
    (when (or genome-directory-invalid? no-proteins-file?)
      (error "Fatal error: Invalid directory structure for organism ~A" orgn)
      ))
        
  (cformatt "Loading seed genome into frames")
  (let ((count (load-the-seed-genome-into-frames-msf orgf)))
    (cformatt "Genome Done, loaded ~D contigs" count))
  (setf (#^seed-genome-sequence-file orgf) *seed-genome-sequence-file*)

  (cformatt "Loading seed genes into frames")
  (let ((count (load-the-seed-genes-into-frames-msf orgf)))
    (cformatt "Genes Done, loaded ~D genes" count))
  (setf (#^seed-gene-annotations-file orgf) *seed-gene-annotations-file*)
  
  (unless (#^no-proteins? orgf)
    (cformatt "Loading seed proteins into frames")
    (let ((count (load-the-seed-proteins-into-frames-msf orgf)))
      (cformatt "Proteins Done, loaded ~D proteins" count))
    (setf (#^seed-proteins-sequence-file orgf) *seed-proteins-sequence-file*)
    )

  (cformatt "Running patch code for boundary crossing genes.")
  (patch-genes-that-probably-cross-origin orgf)

  (cformatt "Organism ~A loaded successfully." orgf)
  
  (when data-structures? 
    (cformatt "Creating organism data structures")
    (create-organism-data-structures 
     :organisms (list orgf) :verbose? verbose?
     ))

  ;; (setf (slotv orgf #$organism-loaded?) T)

  (when test? (test-seed-organism orgf))

  (pushnew orgf *loaded-organisms*)

  orgf
  )

(defun load-the-seed-genome-into-frames-msf 
       (orgf
        &aux 
        (dirname "genome")
        (contigs nil)
        (org-prefix (#^organism-prefix orgf))
        (contig-name-map (make-string-equal-hash-table)))
  
  ;; Create a frame for each contig, fill in the appropriate slots

  (cformatt "Creating contig frames for the organism")
  
  ;; (contig-seed-id contig-length &optional contig-ncbi-id 
  ;;   plasmid? complete? circular? elhai-contig-name)
  (loop for (seed-contig-name contig-length . ncbi-data) in *genome-info*
        as ncbi-name = (fifth ncbi-data)
        as fasta-contig-name = (s+ org-prefix seed-contig-name)
        ;; even if there is ncbi data there might not be a name to use 
        as bike-contig-name = 
        (s+ org-prefix 
            (if (and ncbi-name (not (nullstring? ncbi-name)))
                ncbi-name 
              seed-contig-name
              ))
        as contig-frame =
        (frame-fnamed bike-contig-name t 'seed-contiguous-sequence)
        for contig-index from 1 
        do
        ;; create a mapping from the names of the contigs that should be 
        ;; in the fasta files to the names we want
        (setf (gethash fasta-contig-name contig-name-map) bike-contig-name)
        (setf (#^sequence-length contig-frame) contig-length)
        (setf (#^organism-entity-type contig-frame) #$contiguous-sequence)
        (setf (#^genes-sorted-by-position contig-frame) nil)
        (setf (#^fragments-sorted-by-position contig-frame) nil)
        (setf (#^organism contig-frame) orgf)
        (setf (#^seed-id contig-frame) seed-contig-name)
        (if ncbi-data 
            (progn
              (cformatt "Filling in ncbi data for contig ~A nee ~A"
                        bike-contig-name seed-contig-name)
              (flet ((binary->bool (x) (if (zerop x) nil t)))
                (setf (#^ncbi-id contig-frame) (first ncbi-data))
                (setf (#^plasmid? contig-frame)
                      (binary->bool (second ncbi-data)))
                (setf (#^complete? contig-frame)
                      (binary->bool (third ncbi-data)))
                ;; no question mark for circular because cyano instances use
                ;; this slot without having a question mark
                (setf (#^circular contig-frame) 
                      (binary->bool (fourth ncbi-data)))
                ))
          ;; if there is no ncbi data about the contig, 
          ;; then we are going to set the #$circular frame of
          ;; the contig using the value of the #$circular slot of the organism.
          ;; The #$circular slot of the organism is either T, NIL, or
          ;; a list of circular contig indices.  
          (let ((orgc (#^circular orgf)))
            (when orgc
              (setf (#^circular contig-frame) 
                    (cond
                     ((not (listp orgc)) t)
                     (t (not (null (member contig-index orgc))))
                     )))))
        (push contig-frame contigs)
        )
  
  (setq contigs (nreverse contigs))

  ;; create the sequence and seqidx files if necessary
  (cformatt "Creating sequence and index files for the genome")
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
        
    (store-internal-sequence-info-and-seqlen-msf 
     orgf index-data contig-name-map)
          
    (let ((seqfile (seqinfo-seq-file (#^fname orgf) dirname)))
      (setf (slotv orgf #$Genome-sequence-file) (namestring seqfile))
      ;; Probably not needed.
      (setf (slotv orgf #$Genome-sequence-stream) nil)
      ))

  (let* ((block-data 
          (progn
            (cformatt "Creating contiguous sequence block frames...")
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
            (cformatt "Creating final (possibly renamed) contig frames")
            (loop 
             for contig in contigs 
             for contig-block-data in block-data 
             collect
             (maybe-create-new-frame-names-for-contig 
              orgf contig contig-block-data
              )))))

    (setf (#^contiguous-sequences orgf) final-contigs)

    (let ((chromosome (determine-seed-chromosome final-contigs)))
      (when chromosome 
        (setf (#^chromosome orgf) chromosome)
        (setf (#^contiguous-sequences orgf) 
              (cons 
               chromosome (remove chromosome (#^contiguous-sequences orgf))
               ))))

    ;; according to our new understanding, an organism is #$completed 
    ;; if and only if every contig is #$complete? 

    (setf (#^completed orgf) 
          (every 
           (lambda (contig) (#^complete? contig))
           (#^contiguous-sequences orgf)
           ))

    (length final-contigs)
    ))

(defun store-internal-sequence-info-and-seqlen-msf 
       (orgf index-table contig-name-map)
  (loop for (fasta-name start-header start-data seqlen) in index-table do
        (let ((biobike-contig-name (gethash fasta-name contig-name-map)))
          (if (null biobike-contig-name)
              (cformatt 
               (one-string-nl
                "*** The contig named ~A found in the genome sequence"
                "*** index file for the organism ~A, has no matching"
                "*** biobike contig name in the contig name mapping table!")
               fasta-name orgf
               )
            (let ((frame (frame-fnamed biobike-contig-name)))
              (unless frame
                (error "Frame named ~S does not exist!" biobike-contig-name))
              (setf (slotv frame #$Internal-sequence-info)
                    (list start-header (- start-data start-header) start-data))
              (unless (= (#^sequence-length frame) seqlen)
                (cformatt
                 (one-string-nl
                  "*** The sequence length (~A) obtained from the"
                  "*** contig frame ~A  for the organism ~A,"
                  "*** is not consistent with the length"
                  "*** (~A) found in the genome sequence index file!")
                 (#^sequence-length frame) frame orgf seqlen
                 ))
              )))))

(defun determine-seed-chromosome (contigs)
  (loop for contig in contigs
        do
        (when (search "chromosome" (#^fname contig) :test 'string-equal)
          (return contig)
          )))

(defparameter *seed-non-coding-types* '("rna" "srna"))
(defparameter *seed-coding-types* '("peg"))

(defun load-the-seed-genes-into-frames-msf (orgf)
  (let* ((contigs (slotv orgf #$Contiguous-Sequences))
         (genes nil)
         (coding-list nil)
         (non-coding-list nil)
         (other-features-list nil))
    (let ((dir-errors nil) (from-errors nil) (to-errors nil))
      (cformatt "Creating gene frames for the organism")
      
      (loop 
       for fid in *fids-for-gid*
       for count from 1
       do
       (multiple-value-setq 
           (genes dir-errors from-errors to-errors)
           (insert-seed-gene-data-into-frame-msf
            fid orgf contigs genes dir-errors from-errors to-errors
            ))
       (when (and *seed-load-verbose* (zerop (mod count 1000)))
         (format t ".")
         ))

      (limited-errors-reported 
       dir-errors
       10 "And more direction errors..."
       (lambda (x)
         (cformatt "Invalid direction ~A in gene ~A, id ~A on contig ~A" 
                   (second x) (first x) (third x) (fourth x))))

      (limited-errors-reported
       from-errors
       10 "And more from-errors..."
       (lambda (x)
         (cformatt "Invalid FROM-entry ~A in gene ~A, id ~A on contig ~A: ~A" 
                   (second x) (first x) (third x) (fourth x) (fifth x))))

      (limited-errors-reported
       to-errors
       10 "And more to-errors..."
       (lambda (x)
         (cformatt "Invalid TO-entry ~A in gene ~A, id ~A on contig ~A: ~A"  
                   (second x) (first x) (third x) (fourth x) (fifth x))))

      (when (or dir-errors from-errors to-errors)
        (error "Fatal error(s) in processing genes of organism ~A." orgf))

      )

    (if *enable-revised-gene-ordering* 
       
        ;; separate out the 'other features' from the 'real genes.'
        ;; Put both lists in the order that we determined back when
        ;; we first processed them, but independently.  Then put
        ;; the 'real genes' into the #$genes slot and the other stuff
        ;; into the #$other-features slot.

        ;; This is different than the algorithm when 
        ;; *enable-revised-gene-ordering* is not true. There, the
        ;; coding genes and the non-coding genes don't get mushed
        ;; together as they are here.  The non-coding genes appear
        ;; as the last part of the #$genes slot.

        (let ((genes-sans-other-features nil))
          (loop for gene in genes 
              as type = (#^seed-type gene)
              do
              (cond
               ((member type *seed-coding-types* :test 'string-equal)
                (push gene coding-list))
               ((member type *seed-non-coding-types* :test 'string-equal)
                (push gene non-coding-list))
               (t (push gene other-features-list))
               ))
          (setq genes-sans-other-features
                (sort (nconc coding-list non-coding-list) 
                      '< :key (lambda (x) (#^order x))
                      ))
          (setq other-features-list 
                (sort other-features-list '< :key (lambda (x) (#^order x))))
                      
          (setf (slotv orgf #$Genes) genes-sans-other-features)
          (setf (slotv orgf #$other-features) other-features-list)
          )

      (progn

        ;; put the genes back into the order they came from the seed
        (setq genes (nreverse genes))

        (cformatt "Separating out coding genes from other features")

        (loop for gene in genes 
              as type = (#^seed-type gene)
              do
              (cond
               ((member type *seed-coding-types* :test 'string-equal)
                (push gene coding-list))
               ((member type *seed-non-coding-types* :test 'string-equal)
                (push gene non-coding-list))
               (t (push gene other-features-list))
               ))
    
        ;; put the non-coding genes at the end instead of keeping them all
        ;; in the order we got them from the seed
        (setf (slotv orgf #$Genes) 
              (append (nreverse coding-list) (nreverse non-coding-list)))
        (when other-features-list
          (setf (slotv orgf #$other-features) (nreverse other-features-list)))

        ))

    ;; assign a #$genetic-name to each gene according to elhai algorithm
    (cformatt "Assigning #$genetic-name to each gene.")
    (assign-genetic-names orgf)

    (cformatt "Syncing gene info with annotation table info.")
    (sync-the-genes-with-the-annotation-tables orgf)
    

    (+ (length (slotv orgf #$Genes)) (length (slotv orgf #$other-features)))

    ))

(defun sync-the-genes-with-the-annotation-tables (orgf)
  (let ((fid->frame-hash (make-string-equal-hash-table)))
    (loop for g in (#^genes orgf)
          do (setf (gethash (#^seed-id g) fid->frame-hash) g))
    (let* ((annotated-fids 
            (mysql-get-pegs-of-genome-in-categories-table *fids-for-gid*))
           (in-data (fids-to-mysql-in-format annotated-fids)))
      (cformatt "There are ~D annotated fids for this genome."
                (length annotated-fids))
      (flet ((sync-category (category-type slot)
               (let ((records
                      (mysql-get-categories-info-by-type
                       in-data
                       category-type
                       )))
                 ;; create lists of annotation records for each fid that has 
                 ;; an annotation record
                 (setq records 
                       (mapcar 
                        'third 
                        (group-by-type records :key 'second :test 'string-equal)
                        ))
                 ;; get only the most recent annotation record for each fid 
                 (setq records
                       (mapcar 
                        (lambda (x) 
                          (lastelem
                           (sort x 'string-lessp :key 'categories-timestamp)))
                        records 
                        ))
                 (loop for record in records
                       as fid = (second record)
                       as annotation = (categories-annotation record)
                       as fid-frame = (gethash fid fid->frame-hash)
                       do
                       (setf (slotv fid-frame slot) annotation)
                       ))))
        (if annotated-fids 
            (progn
              (cformatt "Syncing main annotation -> #$description")
              (sync-category "ANN" #$description)
              (cformatt "Syncing genetic name")
              (sync-category "GEN" #$genetic-name)
              )
          (cformatt "No annotated FIDs to synchronize!")
          )))))
            
(defun assign-genetic-names (orgf)
  (loop for gene in (#^genes orgf)
          as aliases = (#^aliases gene)
          as short-name = (string-after-first (#^fname gene) #\.)
          do
          (loop for alias in aliases 
                do
                (when (and (<= (length alias) 6) 
                           (not (string-equal alias short-name)))
                  (setf (#^genetic-name gene) alias)
                  (return)
                  ))))
                  

(defun insert-seed-gene-data-into-frame-msf
       (fid orgf contigs genes dir-errors from-errors to-errors)
  (let* ((plist (gethash fid *features-info-hash*))
         (contig-name (get-feature-prop plist :contig))
         (gene-name (get-feature-prop plist :gene-name))
         (from (get-feature-prop plist :from))
         (to (get-feature-prop plist :to))
         (direction (get-feature-prop plist :direction))
         (subsystem-role (get-feature-prop plist :subsystem-role))
         (encodes-protein (get-feature-prop plist :encodes-protein))
         (aliases (get-feature-prop plist :aliases))
         (location (get-feature-prop plist :location))
         (segments (get-feature-prop plist :segments))
         (wraps? (get-feature-prop plist :wraps?))
         (type (get-feature-prop plist :type))
         (order (get-feature-prop plist :order)))
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
         gene-name fid orgf contig-name orgf
         ))
      ;; error checks on direction, from, and to
      (unless (or (eq direction :b) (eq direction :f))
        (push (list gene-name direction fid) dir-errors))
      (let ((contig-length (slotv contig-frame #$sequence-length)))
        (if (integerp from) 
            (if (and (>= from 1) (<= from contig-length))
                nil
              (push 
               (list gene-name from fid contig-name "FROM is not within range") 
               from-errors
               ))
          (push 
           (list gene-name from fid contig-name "FROM is not an integer")
           from-errors)
          )
        (if (integerp to) 
            (if (and (>= to 1) (<= to contig-length))
                nil
              (push 
               (list gene-name to fid contig-name "TO is not within range")
               to-errors))
          (push 
           (list gene-name to fid contig-name "TO is not an integer")
           to-errors
           )))
      (let* ((gene-frame (frame-fnamed gene-name t 'seed-gene)))
        (setf (slotv gene-frame #$Organism-Entity-Type) #$Gene)
        (setf (slotv gene-frame #$Organism) orgf)
        (setf (slotv gene-frame #$Contiguous-Sequence) contig-frame)
        (setf (slotv gene-frame #$Direction) direction)
        (setf (slotv gene-frame #$From) from)
        (setf (slotv gene-frame #$To) to)
        (setf (slotv gene-frame #$seed-id) fid)
        (setf (slotv gene-frame #$seed-type) type)
        (setf (slotv gene-frame #$order) order)
        ;; get rid of :null problem and potentially any other 
        ;; weird symbols that come back from the allegro mysql interface.
        ;; Make sure if description has been removed in the sql database
        ;; that the old description gets overwritten too.  
        (if (frames::frame-has-slot? gene-frame #$subsystem-role)
            (setf (slotv gene-frame #$subsystem-role) 
                  (if (and subsystem-role (stringp subsystem-role))
                      subsystem-role
                    nil
                    ))
          (when (and subsystem-role (stringp subsystem-role))
            (setf (slotv gene-frame #$subsystem-role) subsystem-role)
            ))
        ;; Unless the categories table has a main annotation entry, 
        ;; the #$description slot is to be the same value as the
        ;; #$subsystem-role slot, according to Jeff E.  Only later
        ;; in the load process we will get the information from the categories
        ;; table and update the #$descriptions slot appropriately.  
        (setf (slotv gene-frame #$description) 
              (slotv gene-frame #$subsystem-role))
        ;; as per discussion with Jeff E 5/6/11 
        #+obsolete
        (if (frames::frame-has-slot? gene-frame #$seed-annotation-history)
            (setf (slotv gene-frame #$seed-annotation-history) annotation)
          (when annotation
            (setf (slotv gene-frame #$seed-annotation-history) annotation)
            ))
        ;; don't check for these frames already existing.  if you want
        ;; to make sure you get new information about this stuff, then
        ;; purge the organism first.  
        (when encodes-protein 
          (setf (slotv gene-frame #$encodes-protein) t))
        (when aliases (setf (slotv gene-frame #$aliases) aliases))
        (when location (setf (slotv gene-frame #$location) location))
        (when segments (setf (slotv gene-frame #$segments) segments))
        ;; if any gene wraps, then its contig must be circular
        (when wraps? (setf (slotv contig-frame #$circular) wraps?))
        (push gene-frame genes)
        (values genes dir-errors from-errors to-errors)
        ))))

(defun load-the-seed-proteins-into-frames-msf (orgf &aux (dirname "proteins"))

  (let* ((orgn (fname orgf))
         (fasta-file (merge-pathnames "proteins.fasta" *acache-proteins-dir*))
         (proteins-without-peg-genes nil)
         (peg-genes-without-proteins nil)
         (duplicate-fasta-pegs nil)
         (proteins-to-create nil)
         (final-protein-frames nil)
         ;; Create index files for the protein fasta file
         (dummy
          (progn
            (cformatt "Creating sequence and index files from proteins fasta")
            (seed-organism-fastas-to-index-and-seq-files 
             orgf orgn dirname (list fasta-file) 
             :rebuild? t
             :data-char-predicate 
             (let ((pred (#^protein-char-predicate orgf))) 
               (or (and pred (intern (string pred) :bio))
                   'allowable-protein-char-finished?)))))
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
    (cformatt 
     "Checking proteins for duplicates and for one to one match with pegs")
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
    
    (cformatt 
     "Creating ~D protein frames" (length proteins-to-create))
    (loop for (nil peg-gene index-info) in proteins-to-create
          for count from 0
          as protein-name = (protein-name-from-gene-msf peg-gene)
          as protein-frame = (frame-fnamed protein-name t 'seed-protein)
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
          ;; make the protein point to the gene and vice versa
          (let* ((proteins-for-gene (#^proteins peg-gene)))
            (pushnew protein-frame proteins-for-gene)
            (setf (#^proteins peg-gene) proteins-for-gene))
          (push protein-frame final-protein-frames)
          (when (and *seed-load-verbose* (zerop (mod count 200)))
            (format t ".")
            ))
    (when *seed-load-verbose* (terpri))

    ;; Sets up #$protein-sequence-file for extraction
    (let ((seqfile (seqinfo-seq-file orgn "proteins")))
      (set-protein-sequence-file orgf seqfile))

    (cformatt "Extracting protein sequences...")
    (loop for protein in final-protein-frames
          with *extract-sequence-info-bootstrap* = t
          do
          (setf (slotv protein #$sequence) (extract-protein-sequence protein))
          (frames::delete-slot protein #$Internal-sequence-info)
          )
    
    (setf (slotv orgf #$proteins) final-protein-frames)

    ;; rearrange the proteins so that they appear in the same order as the
    ;; order the genes they correspond to appear.  
    (cformatt "Rearranging proteins in order of their corresponding genes...")
    (setf (slotv orgf #$proteins) (put-proteins-in-genes-order orgf))
    
    (when *seed-load-verbose* (cformatt "All protein frames created..."))

    (length final-protein-frames)

    ))

(defun protein-name-from-gene-msf (peg-gene)
  (let* ((gene-name (fname peg-gene))
         (org-prefix (string-before-first gene-name #\.))
         (gene-suffix (string-after-first gene-name #\.))
         (final-name (s+ org-prefix "." "p-" gene-suffix)))
    final-name
    ))

(defun copy-fasta-file-without-duplicates (from to)
  (let ((keyhash (make-string-equal-hash-table))
        (write? t)
        (dup-count 0))
    (with-open-file (p from :direction :input)
      (with-open-file (q to :direction :output :if-exists :supersede)
        (block exit
          (loop 
           while t
           do
           (let ((line (read-line p nil nil nil)))
             (when (null line) 
               (return-from exit nil))
             (when (and (plusp (length line)) (char= (char line 0) #\>))
               (setq write? t)
               (let ((key (string-trim
                           *whitespace*
                           (first (string-split line)))))
                 (if (gethash key keyhash)
                     (progn (setq write? nil) (incf dup-count))
                   (setf (gethash key keyhash) t)
                   )))
             (when write? (write-line line q))
             )))))
    dup-count
    ))
                
              
;;; This code has to get run after the proteins are created!
;;; And it needs to get run before the ordered vectors of genes 
;;; for the contigs of the org are created.

;;; When we find a gene that needs patching we want to figure out its
;;; direction.  So, after fixing FROM and TO, first set the direction to F, 
;;; then extract the gene sequence and translate it.  If the first N
;;; amino acids match the protein sequence then F is almost certainly
;;; correct, and if not, it likely (but not certainly) means that the direction
;;; is B.  We could do the same test but then what if neither match?  We
;;; just have to pick one and go with it so we might as well just make it B
;;; if the F direction is not a match.

(defun patch-genes-that-probably-cross-origin (orgf)
  (loop 
   for g in (#^genes orgf)
   as from = (#^from g)
   as to = (#^to g)
   as contig = (#^contiguous-sequence g)
   do
   ;; Only need to check genes on circular contigs.  
   ;; Don't even try if the gene has segments or it doesn't encode a protein.
   ;; If it already crosses the boundary, no sense checking...
   (when (and
          (#^circular contig)
          (null (#^segments g))
          (null (> from to))
          (#^encodes-protein g))
     (let* ((gseqlen (1+ (- to from)))
            (clen (#^sequence-length contig))
            (p (first (#^proteins g))))
       ;; Some genes claim to encode proteins but the protein
       ;; sequence is missing from the SEED data file.  
       (when p
         (let* ((pseq (#^sequence p))
                (expected-pseq-length (1- (/ gseqlen 3)))
                (actual-pseq-length (length pseq))
                (diff 
                 (abs 
                  (- actual-pseq-length (ceiling expected-pseq-length)))))
           (when (/= expected-pseq-length actual-pseq-length)
             ;; These values (4 and 0.7) are somewhat arbitrary
             (when (and (> diff 4) (> (/ gseqlen clen) 0.7))
               (cformatt "Found probable spanning gene ~A" g)
               (cformatt "Reversing FROM (~D) and TO (~D)" from to)
               (setf (#^from g) to)
               (setf (#^to g) from)
               (setf (#^direction g) :f)
               (let ((gseq (extract-sequence g)))
                 ;; 30 is completely arbitrary, we want some 
                 ;; initial subsequence.  
                 (when (> (length gseq) 30)
                   (let* ((gseq-translation 
                           (subseq 
                            (translate-d/rna-to-aa
                             gseq :if-partial-codon nil)
                            0 10
                            )))
                     (cond
                      ;; If the protein sequence matches the gene sequence,
                      ;; then we're good to go.  
                      ((initial-subsequence-of? pseq gseq-translation) 
                       (cformatt "Forward match, direction set to :f"))
                      ;; Otherwise, we just assume it would match
                      ;; the backward direction, because if it doesn't
                      ;; match either one, we have to pick
                      ;; a direction anyway.  
                      (t 
                       (setf (#^direction g) :b)
                       (cformatt "Default, direction set to :b")
                       )))))))))))))