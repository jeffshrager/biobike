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

;;; Author:  JP Massar.

(defun application-instance-initializations-msf
       (&aux (*disable-seed-organism-autoload* t))
  (setq genericdb::*generic-db-verbose* nil)
  (setq *seed-genome-frames* nil)
  (setq *new-seed-genome-frames* nil)
  (setq *stable-seed-genome-frames* nil)
  (setq *updated-seed-genome-frames* nil)
  (setq *gid->frames-map* nil)
  (clear-seed-genome-lists)
  (let ((*external-seed-master-table* nil)
        (*external-seed-master-hash* nil)
        (*seed-genome-table-info* nil)
        (*seed-genome-table-hash* nil)
        (*existing-acache-genome-frames* nil)
        )
    (cformatt "Extracting information about all seed genomes...")
    (initialize-seed-acache-organisms-msf)
    (cformatt "~D existing downloaded organisms found in acachedb" 
              (length *loaded-organisms*))
    (cformatt "Populating BBL frame mapping hash with organism aliases...")
    (set-up-bbl-organism-names-and-nicknames)
    (cformatt "Creating lists/menus/opcodes for organism types found in SEED.")
    (set-up-seed-genome-types)
    (cformatt "Creating unique IDs for each master list organism")
    (create-organism-unique-ids)
    (cformatt "Creating seed subsystem frames")
    (download-subsystem-info-and-create-frames)
    ;; (cformatt "Assigning IDs to NCBI taxonomy nodes")
    ;; (forward-package-funcall :vpl :assign-ids-to-ncbi-taxonomy)
    (cformatt "Preloading metagenome frames")
    (preload-metagenomes)
    (cformatt "~D metagenomes found and available" 
              (length (available-metagenomes)))
    (wb::load-instance-modules)
    (cformatt "Committing ~D new organisms to the database!" 
              (length *new-seed-genome-frames*))
    #-:lispworks
    (db.ac:commit)
    (cformatt "Determining KEGG ids for seed organisms...")
    (determine-seed-organism-kegg-ids)
    ))

(defun initialize-seed-acache-organisms-msf ()
  (setq genericdb::*generic-db-verbose* nil)
  (cformatt "Reading and verifying seed external table ~A" 
            *seed-gid-table-path*)
  ;; this sets or resets *external-seed-master-hash* and 
  ;; *external-seed-master-table* 
  (process-master-seed-organism-list :force nil)
  (cformatt 
   "~D valid seed genome entries found in master table"
   (hash-table-count *external-seed-master-hash*))
  (cformatt "Downloading genome information from the SEED.")
  ;; downloads genome information from the SEED. 
  ;; This sets or resets *seed-genome-table-info* and 
  ;; *seed-genome-table-hash*
  (process-seed-genome-data)
  (cformatt "~D seed ids found in the SEED genome table and downloaded..." 
            (hash-table-count *seed-genome-table-hash*))
  ;; creates gid -> org frames hash and defines *loaded-organisms*
  ;; This sets or resets *existing-acache-genome-frames* and 
  ;; *gid->frames-map*
  (setq *gid->frames-map* (process-existing-organism-frames-msf))
  (cformatt "~D organisms found in Acache database." 
            (hash-table-count *gid->frames-map*))
  ;; Now we have all the data: the master table data, the genome data from
  ;; the seed, and all the organisms that we previously created frames for.  
  ;; Verify the master list against the seed genome data, 
  ;; and verify the master list against the existing organism info, 
  ;; and then create any new organism frames that need to be created,
  ;; and update any organism frames that have new data available from the
  ;; master list or the seed, and finally deal with organism frames whose gids
  ;; are no longer found in the master list.  
  (cformatt "Resolving master list against existing acache database")
  (resolve-master-seed-and-acache-data)
  ;; Create organism frames for entries in the master list that
  ;; do not yet have acache frames.  Make sure that any new or
  ;; revised information found in the master list and in the SEED 
  ;; is entered into existing seed organism frames.  
  (cformatt "Entering new organisms and updating old organisms")
  (enter-new-orgs-and-update-old-orgs)
  ;; Set up *loaded-organisms* and *available-organisms*
  (cformatt "Creating loaded and available organisms lists")
  (create-loaded-and-available-organisms-list)
  ;; Set up all the aliases for all the organisms 
  (cformatt "Setting up organism nicknames")
  (do-seed-nicknames-msf)
  (cformatt "Initializing descriptions map")
  (maybe-create-descriptions-map)
  )

(defun available-organisms-msf (&key (as :frames) (dir nil))
  (declare (ignore dir))
  (flet ((symbols-in-package (strings package)
           (mapcar 
            (lambda (x) (intern (string-upcase x) package))
            strings                   
            )))
    (case as 
      ((:frame :frames) *available-organisms*)
      (otherwise 
       (let ((names (mapcar 'fname *available-organisms*)))
         (case as 
           ((:string :strings) names)
           ((:symbol :symbols) (symbols-in-package names :bio))
           ((:keyword :keywords) (symbols-in-package names :keyword))
           ))))))

(defun report-added-gids (added-gids)
  (loop for gid in added-gids 
        for j from 0 
        do
        (cond
         ((< j 10) (cformatt "  new gid: ~A" gid))
         ((= j 10) (cformatt "  and more gids..."))
         (t nil)
         )))

(defun report-deleted-gids (deleted-gids)
  (loop for gid in deleted-gids
        for j from 0
        do
        (cond
         ((< j 10) (cformatt "  deleted gid: ~A" gid))
         ((= j 10) (cformatt "  and more gids..."))
         (t nil)
         )))

(defun process-existing-organism-frames-msf ()
  (cformatt "Creating hash of all existing Biobike Acache organisms...")
  (let ((list nil)
        (seed-id-hash (make-string-equal-hash-table)))
    #-:lispworks
    (db.ac::doclass* (orgf 'bio::seed-organism) (push orgf list))
    (setq *loaded-organisms* nil)
    (setq *existing-acache-genome-frames* list)
    (loop for orgf in list do
          (when (#^organism-loaded? orgf) (push orgf *loaded-organisms*))
          (let ((prefix (#^organism-prefix orgf))
                (gid (#^seed-id orgf)))
            (unless prefix 
              (error "Every organism (~A) must have an organism prefix!" orgf))
            (unless gid 
              (error "Every seed organism (~A) must have a seed id!" orgf))
            (setf (gethash gid seed-id-hash) orgf)
            ))
    (setq *gid->frames-map* seed-id-hash)
    ))

(defun resolve-master-seed-and-acache-data ()
  (let* ((master-but-not-in-seed 
          (set-difference-hash 
           *external-seed-master-hash* *seed-genome-table-hash*
           ))
         (acache-but-not-in-master 
          (set-difference-hash
           *gid->frames-map* *external-seed-master-hash*
           ))
         (acache-but-not-in-seed
          (set-difference-hash
           *gid->frames-map* *seed-genome-table-hash*
           ))
         (acache-but-not-in-master-or-seed
          (union acache-but-not-in-master acache-but-not-in-seed)
          ))
    (when master-but-not-in-seed 
      (loop for gid in master-but-not-in-seed 
            do 
            (formatt 
             (one-string-nl
              "*** ~A : "
              "*** Master list contains this gid, but the SEED either does not"
              "***   or no longer contains that gid!  This gid will be ignored."
              )
             gid
             )
            (setq *external-seed-master-table* 
                  (delete gid *external-seed-master-table*
                          :test 'string-equal :key 'first))
            (remhash gid *external-seed-master-hash*)
            ))
    (when acache-but-not-in-master-or-seed
      (loop for gid in acache-but-not-in-master-or-seed 
            do
            (cond
             ((and (member gid acache-but-not-in-seed :test 'string-equal)
                   (member gid acache-but-not-in-master :test 'string-equal))
              (formatt
               (one-string-nl
                "*** ~A / ~A : "
                "*** The acache database contains a seed organism frame"
                "***  with this gid (~A),"
                "***  but neither the master list nor the"
                "***  SEED database now contains such a gid.  This"
                "***  organism frame (and all its component frames)"
                "***  is being deleted from the acache database.")
               gid (gethash gid *gid->frames-map*) gid
               )
              (purge-acache-seed-organism (gethash gid *gid->frames-map*))
              )
             ((member gid acache-but-not-in-master :test 'string-equal)
              (formatt 
               (one-string-nl
                "*** ~A / ~A : "
                "*** The acache database contains a seed organism frame"
                "***  with this gid (~A),"
                "***  but the master list either does not or no"
                "***  longer contains that gid.  The organism frame will remain"
                "***  but will not be included in the set of"
                "***  available organisms.")
               gid (gethash gid *gid->frames-map*) gid
               ))
             ((member gid acache-but-not-in-seed :test 'string-equal)
              (formatt
               (one-string-nl
                "*** ~A / ~A : "
                "*** The acache database contains a seed organism frame"
                "***  with this gid (~A),"
                "***  but the SEED database either does not"
                "***  or no longer contains such a gid.  This"
                "***  organism frame (and all its component frames)"
                "***  is being deleted from the acache database.")
               gid (gethash gid *gid->frames-map*) gid
               )
              (purge-acache-seed-organism (gethash gid *gid->frames-map*))
              ))))))

(defun enter-new-orgs-and-update-old-orgs ()
  (maphash 
   (lambda (gid value)
     (declare (ignore value))
     (if (gethash gid *gid->frames-map*)
         (multiple-value-bind (orgf updated?)
             (maybe-update-organism gid)
           (if updated? 
               (push orgf *updated-seed-genome-frames*)
             (push orgf *stable-seed-genome-frames*)
             ))
       (let ((orgf (create-new-seed-organism-frame gid)))
         (if orgf 
             (progn
               (push orgf *new-seed-genome-frames*)
               ;; add newly created organism frame to set of organism frames
               (setf (gethash gid *gid->frames-map*) orgf)
               )
           (remhash gid *external-seed-master-hash*)
           ))))
   *external-seed-master-hash*
   ))

(defun create-new-seed-organism-frame (gid)
  (block exit
    (let* ((mli (gethash gid *external-seed-master-hash*))
           (sgi (gethash gid *seed-genome-table-hash*))
           (frame-name (m-full-name-slot mli)))
      (vwhen (orgf (frame-fnamed frame-name))
        (cformatt
         (one-string-nl
          "*** The gid ~A belongs to the organism named ~A in the master list."
          "*** Furthermore, there is no organism frame currently in the acache"
          "*** database with this gid."
          "*** But the organism named ~A exists in the acache database, and"
          "*** has the gid ~A."
          "*** Either the organism is being assigned a new gid, or some kind"
          "*** of error is being corrected.  Regardless, the existing"
          "*** acache organism is being purged.  The data in the master list"
          "*** about this organism will not be processed until the next time"
          "*** the system starts up.")
         gid orgf orgf (#^seed-id orgf))
        (purge-acache-seed-organism orgf :commit? nil)
        (return-from exit nil)
        )
      (let ((orgf (frame-fnamed frame-name t 'bio::seed-organism)))
        ;; Put information from master list into organism frame 
        (master-list-info-into-organism-frame mli orgf)
        ;; Put information from the SEED into organism frames
        (seed-genome-info-into-organism-frame sgi orgf)
        ;; Store standard information about seed organism frames
        (setf (#^organism-data-directory orgf)  
              (namestring 
               (append-subdir 
                (organism-data-directory (fname orgf))
                "acache"
                )))
        (setf (#^isa orgf) (list #$organism))
        (setf (#^organism-loaded? orgf) nil)
        orgf
        ))))

(defun master-list-info-into-organism-frame (mli orgf)
  (setf (#^Seed-ID orgf) (m-gid-slot mli))
  (let ((loaded? (#^organism-loaded? orgf)))
    (let* ((opf (#^organism-prefix orgf))
           (opm (maybe-add-dot-to-org-prefix (m-organism-prefix-slot mli)))
           (opchanged? (not (equalp opf opm))))
      (setf (#^organism-prefix orgf) opm)
      (when (and loaded? opchanged?)
        (cformatt
         (one-string-nl
          "*** The organism prefix of a loaded organism ~A (gid: ~A)"
          "***  is different in the master list (~S) than in the stored"
          "***  organism data (~S)!  This change will not be reflected in the"
          "***  names of the genes, contigs, and proteins  until and unless"
          "***  the organism is purged and re-downloaded from the SEED.")
         orgf (#^seed-id orgf) opm opf
         )))
    (labels ((do-merge (slot mli-accessor)
               (let ((fv (slotv orgf slot))
                     (mv (funcall mli-accessor mli))
                     (modified? nil))
                 (if fv
                     (progn 
                       (setf (slotv orgf slot) mv)
                       (setq modified? (not (equalp fv mv))))
                   (when mv (setf (slotv orgf slot) mv)))
                 modified?
                 ))
             (merge-and-warn (slot mli-accessor english-slot-name)
               (let ((slot-modified? (do-merge slot mli-accessor)))
                 (when (and loaded? slot-modified?)
                   (cformatt 
                    (one-string-nl
                     "*** The ~A of a loaded organism, ~A (gid: ~A),"
                     "***  is different in the master list than in the stored"
                     "***  organism data!  This change will not be"
                     "***  reflected in the names of the genes until"
                     "***  and unless the organism is purged"
                     "***  and re-downloaded from the SEED.")
                    english-slot-name orgf (#^seed-id orgf)
                    )))))
      (do-merge #$nicknames 'm-org-nicknames-slot)
      (do-merge #$domain 'm-domain-slot)
      (do-merge #$real-domain 'm-real-domain-slot)
      (merge-and-warn #$gene-pattern 'm-gene-pattern-slot "gene pattern")
      (merge-and-warn 
       #$alt-gene-pattern 'm-alt-gene-pattern-slot "alt gene pattern")
      (merge-and-warn #$gene-prefix 'm-gene-prefix-slot "gene prefix")
      (do-merge #$provisional 'm-provisional-slot)
      (do-merge #$circular 'm-circular-slot)
      #+add-complete
      (do-merge #$complete 'm-complete-slot)
      (do-merge #$host 'm-host-slot)
      (do-merge #$systematic-name 'm-systematic-name-slot)
      (do-merge #$lifestyle 'm-lifestyle-slot)
      (do-merge #$phylogeny 'm-phylogeny-slot)
      (do-merge #$taxonomy-id 'm-taxonomy-id-slot)
      ;; (do-merge #$contigs 'm-contigs-slot)
      ;; (do-merge #$number-of-contigs 'm-number-of-contigs-slot)
      (do-merge #$total-length 'm-total-length-slot)
      (do-merge #$gc% 'm-gc%-slot)
      (do-merge #$seed? 'm-seed?-slot)

      )))

;; Returns T if there was a significant change to the organism's info
;; in the SEED database.
(defun seed-genome-info-into-organism-frame (sgi orgf)
  (setf (#^gname orgf) (gname-slot sgi))
  (setf (#^szdna orgf) (szdna-slot sgi))
  (setf (#^maindomain orgf) (maindomain-slot sgi))

  ;; #$completed slot for organism is now to be determined by
  ;; whether all the contigs are #$complete? 
  ;; This means that before an organism is loaded there is no way
  ;; to tell whether it is completed or not (however according to
  ;; Jeff Elhai, the #$seed-complete information is inaccurate and useless.
  ;; That's why we're not even having that slot exist anymore.  

#||

  (let ((cvalue (complete-slot sgi)))
    (setf (#^seed-complete orgf) cvalue)
    ;; need to turn the value from the seed which is a string
    ;; into a boolean for various biobike functions
    (setf (#^completed orgf) (if (string= "1" cvalue) t nil)))

||#

  (setf (#^restrictions orgf) (restrictions-slot sgi))
  (setf (#^taxonomy orgf) (taxonomy-slot sgi))
  (let* ((loaded? (#^organism-loaded? orgf))
         (pegs-from-frame (#^pegs orgf))
         (rnas-from-frame (#^rnas orgf))
         (pegs-from-seed (pegs-slot sgi))
         (rnas-from-seed (rnas-slot sgi))
         (pegs-changed? 
          (and pegs-from-frame (/= pegs-from-frame pegs-from-seed)))
         (rnas-changed? 
          (and rnas-from-frame (/= rnas-from-frame rnas-from-seed))))
    (setf (#^pegs orgf) (pegs-slot sgi))
    (setf (#^rnas orgf) (rnas-slot sgi))
    (when (and loaded? (or pegs-changed? rnas-changed?))
      (cformatt 
       (one-string-nl
        "The number of pegs and/or the number of rnas of the loaded"
        "organism ~A (gid: ~A), is now different in the SEED database"
        "(~D and ~D) than it is in the acache database (~D and ~D)."
        "This would suggest that pegs or rnas were added to or"
        "removed from the SEED database since this organism was loaded"
        "into Biobike!  These new pegs and/or rnas will not be"
        "available until and unless the organism is purged and"
        "re-downloaded from the SEED.")
       orgf (#^seed-id orgf) pegs-from-seed rnas-from-seed 
       pegs-from-frame rnas-from-frame
       ))
    ;; various random static information 
    (setf (#^submitter orgf) '("Rob Edwards" "Seed"))
    (setf (#^links orgf) 
          '("http://www.theseed.org/wiki/index.php/Main_Page"
            "http://ws.nmpdr.org/"))
    (setf (#^genome-char-predicate orgf) "ALLOWABLE-GENOME-CHAR-UNFINISHED?")
    (setf (#^protein-char-predicate orgf) "ALLOWABLE-SEED-PROTEIN-CHAR?")
    (or pegs-changed? rnas-changed?)
    ))

(defun maybe-update-organism (gid)
  (let* ((mli (gethash gid *external-seed-master-hash*))
         (sgi (gethash gid *seed-genome-table-hash*))
         (orgf (gethash gid *gid->frames-map*)))
    (master-list-info-into-organism-frame mli orgf)
    (seed-genome-info-into-organism-frame sgi orgf)
    ))

(defun do-seed-nicknames-msf ()
  (loop 
   for orgf in (available-organisms)
   do
   ;; This sets up the #$organism-symbols slot of the organism 
   ;; as well as exporting the aliases 
   (create-and-export-organism-nickname-symbols orgf)
   ;; For each seed organism have its gid (with any '.'s replaced with '-'s)
   ;; as an alias for the organism
   (let* ((seed-id (#^seed-id orgf))
          (biobike-seed-id (seed-gid-to-biobike-orgname seed-id)))
     (unless (string-equal biobike-seed-id (fname orgf))
       (create-and-export-organism-nickname-symbol orgf biobike-seed-id)
       ))))

(defun create-loaded-and-available-organisms-list ()
  (setq *loaded-organisms* nil)
  (setq *available-organisms* nil)
  (maphash 
   (lambda (key value) 
     (declare (ignore value))
     (let ((frame (gethash key *gid->frames-map*)))
       (unless frame 
         (error "Fatal error!  No frame for gid ~A in master list!" key))
       (when (#^organism-loaded? frame)
         (push frame *loaded-organisms*))
       (push frame *available-organisms*)
       ))
   *external-seed-master-hash*
   ))

(defun set-up-bbl-organism-names-and-nicknames ()
  (loop
   for orgf in (available-organisms)
   as fname = (fname orgf)
   do
   (loop 
    with bbl-frame-mapping-hash = 
    (symbol-value-in-package "*BBL-FRAME-MAPPING*" :bbi)
    for nickname-symbol in (#^organism-symbols orgf)
    as nsname = (string nickname-symbol) 
    do
    (unless (string-equal nsname fname)
      (setf (gethash (string nickname-symbol) bbl-frame-mapping-hash) fname)
      ))))

(defun create-organism-unique-ids ()
  (loop for orgf in (available-organisms)
        do
        ;; need to do the forward-package-funcall because the vpl
        ;; package isn't defined yet when these files are compile/loaded.  
        (setf (#^organism-id orgf) 
              (forward-package-funcall 
               :vpl :new-unique-id :organism-id
               ))))

(defun delete-organisms-with-bad-prefixes ()
  #-:lispworks
  (db.ac::doclass* 
   (orgf 'bio::seed-organism)
   (let ((prefix (#^organism-prefix orgf)))
     (when (some 'invalid-master-list-prefix-char (subseq (reverse prefix) 1))
       (purge-acache-seed-organism orgf)
       ))))
                   