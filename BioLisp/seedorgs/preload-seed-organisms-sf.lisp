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

#+:lispworks
(defpackage :db.ac (:use :lisp))

(defun application-instance-initializations-sf
       (&aux (bio::*disable-seed-organism-autoload* t))
  (setq genericdb::*generic-db-verbose* nil)
  (setq bio::*seed-genome-frames* nil)
  (setq bio::*new-seed-genome-frames* nil)
  (bio::clear-seed-genome-lists)
  (cformatt "Extracting information about all seed genomes...")
  (bio::initialize-seed-acache-organisms)
  (cformatt "~D existing downloaded organisms found in acachedb" 
            (length bio::*loaded-organisms*))
  (cformatt "Creating lists of organism types found in the SEED.")
  (set-up-seed-genome-types)
  (cformatt "Populating BBL frame mapping hash with organism aliases...")
  (loop
   for orgf in bio::*seed-genome-frames* 
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
      )))
  (cformatt "Creating VPL Seed organism selection operators")
  (bio::create-seed-organism-selection-operators)
  (wb::load-instance-modules)
  (cformatt "Committing ~D new organisms to the database!" 
            (length bio::*new-seed-genome-frames*))
  (db.ac:commit)
  )

(defun available-organisms-sf (&key (as :frames) (dir nil))
  (declare (ignore dir))
  (flet ((symbols-in-package (strings package)
           (mapcar 
            (lambda (x) (intern (string-upcase x) package))
            strings                   
            )))
    (case as 
      ((:frame :frames) *seed-genome-frames*)
      (otherwise 
       (let ((names (mapcar 'fname *seed-genome-frames*)))
         (case as 
           ((:string :strings) names)
           ((:symbol :symbols) (symbols-in-package names :bio))
           ((:keyword :keywords) (symbols-in-package names :keyword))
           ))))))

(defun initialize-seed-acache-organisms ()
  (setq genericdb::*generic-db-verbose* nil)
  (cformatt "Reading and verifying seed external table ~A" 
            *seed-gid-table-path*)
  ;; this binds *gid->gid-table-info* and *seed-gid-table*
  (with-seed-gid-table
    (cformatt 
     "~D non-duplicate entries found in GID name,prefix and aliases table"
     (hash-table-count *gid->gid-table-info*))
    (cformatt "Downloading genome information from the SEED.")
    (let* (
         ;; downloads genome information from the SEED
           (*seed-genome-table-info* (useful-seed-genome-info))
           (*existing-prefix-hash* (make-string-equal-hash-table))
           ;; creates gid -> org frames hash and defines *loaded-organisms*
           (*gid->frames-map* (process-existing-organism-frames))
           )
      (cformatt "~D organisms found in SEED database." 
                (length *seed-genome-table-info*))
      (process-downloaded-genome-info)
      (check-for-non-existent-organisms)
      )))

(defun useful-seed-genome-info ()
  (unless (= (length *seed-genome-table-slot-names*) 9)
    (error "Internal inconsistency in USEFUL-SEED-GENOME-INFO"))
  (let* ((genome-fields *seed-genome-table-rows*)
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
      )))

(defun process-existing-organism-frames ()
  (cformatt "Creating hash of all existing Biobike Acache organisms...")
  (let ((list nil)
        (seed-id-hash (make-string-equal-hash-table)))
    (db.ac::doclass* (orgf 'bio::seed-organism) (push orgf list))
    (setq *seed-genome-frames* list)
    (loop for orgf in list do
          (when (#^organism-loaded? orgf) (push orgf *loaded-organisms*))
          (let ((prefix (#^organism-prefix orgf))
                (gid (#^seed-id orgf)))
            (unless prefix 
              (error "Every organism (~A) must have an organism prefix!" orgf))
            (unless gid 
              (error "Every organism (~A) must have a seed id!" orgf))
            (vwhen (dup-org (gethash prefix *existing-prefix-hash*))
              (error 
               (one-string
                "Internal error:  Existing organism ~A already is"
                " using prefix ~A, while existing organism ~A uses it also.")
               dup-org prefix orgf
               ))
            (setf (gethash prefix *existing-prefix-hash*) orgf)
            (setf (gethash gid seed-id-hash) orgf)
            ))
    (values seed-id-hash *loaded-organisms*)
    ))

(defun process-downloaded-genome-info 
       (&aux (new-count 0) (new-count-ex 0) (old-no-ex 0) (old-ex 0)
             (old-ex-ok 0) (old-ex-conflicts 0)
             (old-ex-org-names nil))
  (cformatt "Processing all GID info downloaded from the SEED...")
  (cformatt "  And incorporating information from external GID table...")
  (loop 
   for genome-info in *seed-genome-table-info*
   as gid = (first genome-info)
   as gname = (second genome-info)
   as orgf = (gethash gid *gid->frames-map*)
   as gid-info = (gethash gid *gid->gid-table-info*)
   as slots = 
   (mapcar (lambda (x) (frame-fnamed x t)) *seed-genome-table-slot-names*)
   do
   (cond
    ;; organism associated with GID does not exist in Acache
    ((null orgf)
     (cond
      ((null gid-info)
       ;; No info about GID in external table either.  Must be completely 
       ;; new GID recently entered into the SEED.
       (incf new-count)
       (enter-new-organism-using-seed-info gid gname genome-info slots))
      (t
       ;; We have info about this GID from the SEED (in GENOME-INFO)
       ;; And info exists about this GID in the external table (in GID-INFO)
       ;; but GID does not now exist in ACACHE (via *GID->FRAMES-MAP*)
       ;; Use external info to decide on name and prefix, nicknames,
       ;; and create the new organism.
       (incf new-count-ex)
       (enter-new-organism-using-external-table-info 
        gid gname genome-info gid-info slots)
       )))
    (t
     ;; The organism frame associated with the GID exists in acache
     (cond
      ;; No info exists about it in external table
      ((null gid-info)
       (incf old-no-ex)
       ;; if using GID, verify name is a duplicate, otherwise
       ;; issue advisory to redownload this organism to name it
       ;; and purge it -- purge needs to undo loaded? slot
       ;; and remove from *loaded-organisms* (which it does)
       (let* ((name (gname->biobike-organism-name gname))
             (existing-frame (frame-fnamed name)))
         (cond
          ((and existing-frame
                (not (string-equal gid (#^seed-id existing-frame))))
           (warn
            (one-string-nl
            "This should be impossible but it seems 2 GIDs have the same gname!"
            "A frame, ~A, with seed id ~A exists in the Acache database."
            "But the GID ~A has associated with it from the SEED a gname,"
            "~A, which when converted to a biobike organism name ~A,"
            "is the same.  But GID ~A is associated with a different"
            "organism frame, ~A.")
            existing-frame (#^seed-id existing-frame)
            gid gname name gid orgf
            ))
          ((null existing-frame)
           (cformatt 
            (one-string-nl
             "The organism identified by gid ~A should be using the name"
             "~A instead of its gid.  The organism frame ~A is going"
             "to be purged from the biobike database and replaced by a frame"
             "named ~A.  Contig, gene, and protein information will be lost."
             "You can download the organism to get this information back"
             "if you wish.")
            gid name orgf name)
           ;; Completely eradicate the old frame.
           ;; Are the hashes being cleared?
           (remhash (#^organism-prefix orgf) *existing-prefix-hash*)
           (purge-acache-seed-organism orgf)
           (remhash gid *gid->frames-map*)
           ;; and create a new frame using coverted gname and a prefix
           ;; based on the GID
           (initialize-new-seed-org-frame 
            name genome-info slots 
            (maybe-add-dot-to-org-prefix (seed-gid-to-biobike-orgname gid)) 
            nil
            ))
          ;; Nothing to do.  Frame with name obtained from the SEED
          ;; and with the correct GID already exists in Acache.
          ;; Without a way to timestamp to determine whether there is
          ;; any new information, we just assume there is not and keep
          ;; whatever
          (t nil)
          )))
      (t
       ;; Info exists in the external table
       (incf old-ex)
       (cond
        ;; The frame info and the table info are consistent
        ((info-does-not-conflict? orgf gid-info)
         (incf old-ex-ok)
         ;; fiddle with nicknames, but nothing else to do
         (setf (#^nicknames orgf) (nicknames-slot gid-info))
         )
        (t
         (incf old-ex-conflicts)
         ;; If the existing acache name and the proffered name from the
         ;; external table are in conflict, we have to completely wipe out
         ;; the frame and create another one.  If not, we could use the
         ;; old frame.  But we might as well wipe out the existing frame
         ;; and start anew anyway.  We do want to warn the user if organism
         ;; data is going to be lost
         (push (fname orgf) old-ex-org-names)
         (when (#^organism-loaded? orgf)
           (cformatt
            (one-string-nl
             "Organism data for ~A is being deleted, because this organism's"
             "prefix (~A) and/or name is being changed due to new information"
             "from the external GID table. It will be reloaded with possibly"
             "new contig, gene and protein names on demand.")
            orgf (#^organism-prefix orgf)
            ))
         ;; completely purge this frame (commit), create a new frame
         ;; with appropriate name and prefix, and fiddle with nicknames
         (remhash (#^organism-prefix orgf) *existing-prefix-hash*)
         (purge-acache-seed-organism orgf)
         (remhash gid *gid->frames-map*)
         (enter-new-organism-using-external-table-info 
          gid gname genome-info gid-info slots)
         )))))))
  
  (cformatt "new-count: ~D, new-count-ex: ~D, old-no-ex: ~D, old-ex: ~D
             old-ex-ok: ~D, old-ex-conflicts: ~D"
            new-count new-count-ex old-no-ex old-ex old-ex-ok old-ex-conflicts
            )
  (loop for org in old-ex-org-names 
        do
        (vformatt 
         (one-string-nl
          "*** ~A purged because gid table inconsistent with existing"
          "     organism data.")
         org
         ))
  (cformatt "Instantiating all nicknames as constants...")
  (do-seed-nicknames)
  
  )

    
(defun check-for-non-existent-organisms ()
  ;; deal with organisms that exist in Acache but no longer
  ;; exist in the SEED.
  (let ((hash (make-string-equal-hash-table)))
    (loop for data in *seed-genome-table-info*
          as gid = (first data)
          do
          (setf (gethash gid hash) t)
          )
        (maphash
         (lambda (gid frame)
           (when (null (gethash gid hash))
             (cformatt 
              (one-string-nl
               "An organism with GID ~A, exists in the Biobike database"
               "but that GID no longer exists in the SEED!"
               "The organism is being removed from the Biobike database!")
              gid)
             (purge-acache-seed-organism frame)
             ))
         *gid->frames-map*
         )))

(defun do-seed-nicknames ()
  (loop 
   for orgf in *seed-genome-frames*
   do
   (create-and-export-organism-nickname-symbols orgf)
   (let* ((seed-id (#^seed-id orgf))
          (biobike-seed-id (seed-gid-to-biobike-orgname seed-id)))
     (unless (string-equal biobike-seed-id (fname orgf))
       (create-and-export-organism-nickname-symbol orgf biobike-seed-id)
       ))))

(defun info-does-not-conflict? (orgf gid-info)
  (let ((et-gname (genome-name-slot gid-info))
        (et-prefix (prefix-slot gid-info)))
    (not
     (or (not (string-equal (#^fname orgf) et-gname))
         (not (string-equal
               (#^organism-prefix orgf)
               (maybe-add-dot-to-org-prefix et-prefix)))
         ))))

(defun initialize-new-seed-org-frame 
       (frame-name 
        org-info org-slots prefix nicknames 
                   &optional 
                   (gene-pattern nil) (alt-gene-pattern nil) (gene-prefix nil))
  (let ((seed-id (first org-info))
        (orgf (frame-fnamed frame-name t 'bio::seed-organism)))
    (setf (#^organism-prefix orgf) prefix)
    (setf (#^nicknames orgf) nicknames)
    (setf (#^organism-data-directory orgf)  
          (namestring 
           (append-subdir 
            (organism-data-directory (fname orgf))
            "acache"
            )))
    (setf (#^Seed-ID orgf) seed-id)
    (setf (#^isa orgf) (list #$organism))
    (setf (#^organism-loaded? orgf) nil)
    (when gene-pattern (setf (#^gene-pattern orgf) gene-pattern))
    (when alt-gene-pattern (setf (#^alt-gene-pattern orgf) alt-gene-pattern))
    (when gene-prefix (setf (#^gene-prefix orgf) gene-prefix))
    (loop for datum in (cdr org-info) 
          for slotname in (cdr org-slots)
          do
          (setf (slotv orgf slotname) datum)
          )
    (push orgf *new-seed-genome-frames*)
    (pushnew orgf *seed-genome-frames*)
    (setf (gethash prefix *existing-prefix-hash*) orgf)
    (setf (gethash seed-id *gid->frames-map*) orgf)
    orgf
    ))

(defun enter-new-organism-using-seed-info (gid gname genome-info slots)
  ;; convert name, if not already in use, use it, otherwise use
  ;; converted GID.  Use converted GID as prefix.  No nicknames.
  ;; The GID must be new because higher-up code has already checked
  ;; it against all current organism frames.
  (let* ((new-organism-name (biobike-gname-or-biobike-gid gname gid))
         (prefix 
          (maybe-add-dot-to-org-prefix (seed-gid-to-biobike-orgname gid))))
    (vwhen (dup-org (gethash prefix *existing-prefix-hash*))
      (error 
       (one-string
        "New organism with gid ~A, is supposed to use prefix ~A, "
        "but existing organism ~A already uses that prefix!")
       gid prefix dup-org
       ))
    ;; call initialize-new-seed-org-frame to create new frame and
    ;; populate its slots with data
    (initialize-new-seed-org-frame 
     new-organism-name genome-info slots prefix nil)
    ))

(defun enter-new-organism-using-external-table-info 
       (gid gname genome-info gid-info slots)

  ;; Info about GID from the external table is in GID-INFO
  ;; Info about GID from the SEED is in GENOME-INFO
  (let ((biobike-gid (seed-gid-to-biobike-orgname gid))
        (new-org-name nil))

    ;; figure out an appropriate name for the organism, which 
    ;; is not already in use.  
    ;; To do this, first see if we can use the name in the 
    ;; external table as the organism name
    (let ((table-name (genome-name-slot gid-info)))
      (cond
       ((null table-name) nil)
       ((null (every 'frames::valid-frame-char? table-name))
        (cformatt 
         (one-string-nl
          "The organism name '~A' associated with gid ~A"
          "from the external genome table is not a valid frame name!"
          "The converted SEED gname or gid will be used instead.")
         table-name gid 
         ))
       ((frame-fnamed table-name) 
        (cformatt 
         (one-string-nl
          "Organism name '~A' associated with gid ~A"
          "from the seed genome table already names"
          "another organism with gid ~A !")
         gname gid (#^seed-id (frame-fnamed table-name))
         ))
       ;; Okay, the name in the external table is fine
       (t (setq new-org-name table-name))
       ))

    ;; if we can't use it, see if we can use the gname from the SEED.  
    ;; If not, we resort to the gid
    (unless new-org-name 
      (setq new-org-name (biobike-gname-or-biobike-gid gname gid)))
    
    ;; Now figure out a prefix we can use.
    (let* ((actual-prefix 
            (let ((table-prefix (prefix-slot gid-info)))
              ;; If no prefix was provided in the external table,
              ;; just use the converted GID.
              (if (null table-prefix) 
                  (maybe-add-dot-to-org-prefix biobike-gid)
                ;; If one was provided, make sure it does not conflict
                ;; with an already existing one.  If it does, revert
                ;; back to using the converted GOD.
                (progn
                  (setq table-prefix (maybe-add-dot-to-org-prefix table-prefix))
                  (vif (dup-org (gethash table-prefix *existing-prefix-hash*))
                       (progn
                         (cformatt 
                          (one-string-nl
                           "Prefix name '~A' associated with GID ~A"
                           "in external genome info table is already"
                           "in use by organism frame ~A !")
                          table-prefix gid dup-org)
                         (maybe-add-dot-to-org-prefix biobike-gid))
                       table-prefix
                       )))))
           (nicknames (nicknames-slot gid-info)))

      ;; Just for yucks(?), make sure prefix is unique again
      (vwhen (dup-org (gethash actual-prefix *existing-prefix-hash*))
        (error
         (one-string
          "New organism with gid ~A, is supposed to use prefix ~A, "
          "but existing organism ~A already uses that prefix!")
         gid actual-prefix dup-org
         ))

      ;; Now that we have a valid name and a valid prefix,
      ;; create the new organism with that name
      (let ((gene-pattern (gene-pattern-slot gid-info))
            (alt-gene-pattern (alt-gene-pattern-slot gid-info))
            (gene-prefix (gene-prefix-slot gid-info)))
        (initialize-new-seed-org-frame
         new-org-name genome-info slots actual-prefix nicknames
         gene-pattern alt-gene-pattern gene-prefix
         ))

      )))


