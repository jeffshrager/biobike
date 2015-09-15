;;;; -*- mode: Lisp; Syntax: Common-Lisp; Package: bbi; -*-

(in-package :bbi)


#||

BLAST-AUX LOGIC

  Initialize random stuff.

  Determine:

   IS THERE AN EXISTING SYSTEM DB FOR THE TARGET SET?
   IS A TARGET SET DB TO BE CREATED?
   IS REMAKE-DATABASE BEING REQUESTED?
   IS USE-EXISTING SPECIFIED?
   IS THERE AN EXISTING USER-SAVED DB BY THE USE NAME PROVIDED?
   IS THERE AN EXISTING SYSTEM-SAVED DB BY THE USER NAME PROVIDED?
   IS USER A GURU?

   STATE VARIABLES:

    GURU?
    REMAKE-DB?
    TARGET-SET-STATUS = { :CREATE :EXISTING NIL }
    USE-DB-STATUS = 
     { NIL :USE-SYSTEM :USE-MY :USE-OTHER-USER 
       :STORE-SYSTEM :STORE-OTHER :STORE-ME }
    DB-SPECIFIED

    Case TARGET-SET-STATUS

     :CREATE

       case USE-DB-STATUS
        NIL : DB-TO-STORE-INTO = TARGET-DB; DB-TO-USE = NIL
        :USE-SYSTEM
           if REMAKE-DB?
              if GURU?
                 DB-TO-STORE-INTO = DB-SPECIFIED; DB-TO-USE = nil;
              else oops;
           else 
             DB-TO-STORE-INTO = NIL; DB-TO-USE = DB-SPECIFIED;
        :USE-OTHER
           if REMAKE-DB?          
              if GURU?
                 DB-TO-STORE-INTO = DB-SPECIFIED; DB-TO-USE = nil;
              else oops;
           else DB-TO-STORE-INTO = nil; DB-TO-USE = DB-SPECIFIED;
        :USE-MY
           ALSO-CREATE-TARGET-DB-NAMED = TARGET-DB;
           if REMAKE-DB?
              DB-TO-STORE-INTO = DB-SPECIFIED; DB-TO-USE = nil;
           else DB-TO-STORE-INTO = nil; DB-TO-USE = DB-SPECIFIED;
        :STORE-SYSTEM
            if GURU?
               ALSO-CREATE-TARGET-DB-NAMED = TARGET-DB;
               issue notification that TARGET-DB will also be created;
               DB-TO-STORE-INTO = DB-SPECIFIED; DB-TO-USE = nil;
            else oops;
        :STORE-OTHER
           if GURU?
              ALSO-CREATE-TARGET-DB-NAMED = TARGET-DB;
              issue notification that TARGET-DB will also be created;
              DB-TO-STORE-INTO = DB-SPECIFIED; DB-TO-USE = nil;
           else oops;        
        :STORE-ME
           ALSO-CREATE-TARGET-DB-NAMED = TARGET-DB;
           issue notification that TARGET-DB will also be created;
           DB-TO-STORE-INTO = DB-SPECIFIED; DB-TO-USE = nil;  
 
     :EXISTING

       case USE-DB-STATUS
        NIL 
          if REMAKE-DB?
             if GURU?
                DB-TO-STORE-INTO = TARGET-DB;v DB-TO-USE = nil;
             else oops;
          else DB-TO-STORE-INTO = nil; DB-TO-USE = TARGET-DB;
        :USE-SYSTEM
           issue note about existing TARGET-DB;
           if REMAKE-DB?
              if GURU?
                 DB-TO-STORE-INTO = DB-SPECIFIED; DB-TO-USE = nil;
              else oops;
           else DB-TO-STORE-INTO = nil; DB-TO-USE = DB-SPECIFIED;
        :USE-OTHER
           issue note about existing TARGET-DB;
           if REMAKE-DB?
              if GURU?
                 DB-TO-STORE-INTO = DB-SPECIFIED; DB-TO-USE = nil;
              else oops;
           else DB-TO-STORE-INTO = nil; DB-TO-USE = DB-SPECIFIED;
        :USE-MY
           issue note about existing TARGET-DB;
           if REMAKE-DB?
              DB-TO-STORE-INTO = DB-SPECIFIED; DB-TO-USE = nil;
           else DB-TO-STORE-INTO = nil; DB-TO-USE = DB-SPECIFIED;
        :STORE-SYSTEM, :STORE-OTHER
           issue note about existing TARGET-DB;
           if GURU?
              if REMAKE-DB?
                 issue note that DB-SPECIFIED does not exist, cannot be remade.
                 DB-TO-STORE-INTO = DB-SPECIFIED; DB-TO-USE = nil;
              else
                 DB-TO-STORE-INTO = DB-SPECIFIED; DB-TO-USE = nil;
           else oops;
        :STORE-ME
           if REMAKE-DB?
              issue note that DB-SPECIFIED does not exist, cannot be remade.
              issue note about existing TARGET-DB;
              DB-TO-STORE-INTO = DB-SPECIFIED; DB-TO-USE = nil;
           else 
              issue note that DB-SPECIFIED does not exist, cannot be remade.
              DB-TO-STORE-INTO = DB-SPECIFIED; DB-TO-USE = nil;

     NIL

       case USE-DB-STATUS
        NIL
          if REMAKE-DB?
             issue note about no DB existing to be remade
          DB-TO-STORE-INTO = nil; DB-TO-USE = nil;
        :USE-SYSTEM, :USE-OTHER
          if REMAKE-DB?
             if GURU?
                DB-TO-STORE-INTO = DB-SPECIFIED; DB-TO-USE = nil;
             else oops
          else
            DB-TO-STORE-INTO = nil; DB-TO-USE = DB-SPECIFIED;
        :USE-MY
           if REMAKE-DB?
              DB-TO-STORE-INTO = DB-SPECIFIED; DB-TO-USE = nil;
           else DB-TO-STORE-INTO = nil; DB-TO-USE = DB-SPECIFIED;    
        :STORE-ME
           if REMAKE-DB?
              issue note that DB-SPECIFIED does not exist, cannot be remade.
           DB-TO-STORE-INTO = DB-SPECIFIED; DB-TO-USE = nil;
        :STORE-SYSTEM, :STORE-OTHER
           if REMAKE-DB?
              issue note that DB-SPECIFIED does not exist, cannot be remade. 
           if GURU?
              DB-TO-STORE-INTO = DB-SPECIFIED; DB-TO-USE = nil;           
           else oops

   We end up with
      DB-TO-USE = nil | directory
      DB-TO-STORE-INTO = nil | directory
      ALSO-CREATE-TARGET-DB-NAMED = nil | system directory

   If (null DB-TO-USE)
      run formatdb into temp blast directory
      create the .txt file
      DB-TO-USE becomes temp blast directory
   run the actual blast using DB-TO-USE, get results
   If DB-TO-STORE-INTO
      copy formatdb produced files and .txt from temp blst directory into it
   If ALSO-CREATE-TARGET-DB-NAMED
      copy formatdb produced files and .txt from temp blst directory into it 
   delete files produced by formatdb in temp blast directory
  
||#

(defparameter *stored-blast-directory-name* "blast-database")

(defun blast-db-extensions (t-type)
  (ecase t-type 
    (:dna (list "nhr" "nin" "nsq"))
    (:aa (list "phr" "pin" "psq"))
    ))

(defun all-blast-db-extensions ()
  (append (blast-db-extensions :aa) (blast-db-extensions :dna)))
       

;; Move a set of blast files which exist in the directory 
;; that the file OLDPATH lives in to a new directory, NEWDIR.
;; OLDPATH must have the same name component as the blast files 
;; to be moved.  The moved files are given a new component NEWNAME.
;; E.g., 
;; OLDPATH = "/home/biobike/tmp/foo.fa"
;; NEWDIR = "/home/biobike/users/massar/blastdir/"
;; NEWNAME = "fred"
;; One of the new files is "/home/biobike/users/massar/blastdir/fred.phr"
;; And similarly for the rest of the blast files 


(defun new-move-blast-files (oldpath newdir newname)
  (ensure-directories-exist (merge-pathnames "foo.txt" newdir))
  (loop with oldfile-name = (file-namestring oldpath)
        with newfile-name = newname
        for ext in (all-blast-db-extensions)
        as oldext = (s+ oldfile-name "." ext)
        as newext = (s+ newfile-name "." ext)
        as oldext-path = (merge-pathnames oldext oldpath)
        as newext-path = (merge-pathnames newext newdir)
        do 
        (when (probe-file oldext-path) 
          (when *debug-blast-aux* 
            (formatt "Moving ~A to ~A~%" oldext-path newext-path))
          (rename-file oldext-path newext-path))
        ))

(defun delete-blast-files (oldpath)
  (loop with oldfile-name = (file-namestring oldpath)
        for ext in (all-blast-db-extensions)
        as delname = (merge-pathnames (s+ oldfile-name "." ext) oldpath)
        do 
        (when (probe-file delname) (delete-file delname))
        ))

(defun determine-db-status (use-db)
  (cond
   ((find #\: use-db) 
    (let* ((parse (string-split use-db #\:))
           (user (first parse))
           (system? (string-equal user "system"))
           (other-user? (not (string-equal user (string wb::*username*))))
           (fastadb-name (second parse))
           (userdir (wb::user-directory user))
           (blastdir (append-subdir userdir *stored-blast-directory-name*))
           (storagedir (append-subdir blastdir fastadb-name))
           )
      (values 
       storagedir
       (cond
        ((probe-file storagedir) 
         (cond
          (system? :use-system)
          (other-user? :use-other)
          (t :use-me)
          ))
        (t 
         (cond
          (system? :store-system)
          (other-user? :store-other)
          (t :store-me)
          ))))))
   (t 
    (let* ((fastadb-name use-db)
           (userdir (wb::user-directory wb::*username*))
           (sysdir (wb::user-directory "system"))
           (blastdir (append-subdir userdir *stored-blast-directory-name*))
           (storagedir (append-subdir blastdir fastadb-name))
           (sysblastdir (append-subdir sysdir *stored-blast-directory-name*))
           (sysstoragedir (append-subdir sysblastdir fastadb-name)))
      (cond
       ((probe-file storagedir) (values storagedir :use-me))
       ((probe-file sysstoragedir) (values sysstoragedir :use-system))
       (t (values storagedir :store-me))
       )))))

(defun determine-blast-aux-directories (targets t-type use-db remake-db?)
  (let* ((sysdir (wb::user-directory "system"))
         (sysblastdir (append-subdir sysdir *stored-blast-directory-name*))
         (possible-target-db nil)
         (target-db nil)
         (target-set-status nil)
         (use-db-status nil)
         (db-specified nil)
         ;; (also-create-target-db-named nil)
         (db-to-store-into nil)
         (db-to-use nil)
         (guru? (wb::weblistener-guru-p))
         )
    
    ;; IS THERE AN EXISTING SYSTEM DB FOR THE TARGET SET?

    (setq 
     possible-target-db 
     (cond
      ((and (= (length targets) 1) (typep (first targets) 'organism))
       (string-downcase (join (slotv (first targets) #$fname) "-" t-type)))
      ((typep targets 'organism-list)
       (loop for subset-name in *organism-subsets*
             ;; must be eval, not symbol-value because symbols are really
             ;; symbol macros
             as subset = (eval subset-name)
             do 
             (when (set-equal subset targets)
               (return 
                (remove #\* (string-downcase (join subset-name "-" t-type)))
                ))
             finally (return nil)
             ))
      (t nil)
      ))
    (if possible-target-db
        (let ((dir (append-subdir sysblastdir possible-target-db)))
          (if (probe-file dir) 
              (progn
                (setq target-set-status :existing)
                (setq target-db dir))
            (progn
              (setq target-set-status :create)
              (setq target-db dir)
              )))
      (progn
        (setq target-set-status nil)
        (setq target-db nil)
        ))
    
    ;; IS THERE AN EXISTING USER-SAVED DB BY THE USE NAME PROVIDED?
    ;; IS THERE AN EXISTING SYSTEM-SAVED DB BY THE USER NAME PROVIDED?

    (if (null use-db)
        (progn
          (setq db-specified nil)
          (setq use-db-status nil))
      (multiple-value-setq (db-specified use-db-status)
          (determine-db-status use-db)
        ))

    ;; Determine values for 
    ;; db-to-use, db-to-store-into, also-create-target-db-named
    
    (flet ((oops-write-not-allowed ()
             (error 
              (one-string-nl
               "You cannot cause a blast database to be stored"
               "to ~A blast directory!"
               )
              (ecase use-db-status
                ((:store-system :use-system) "the system")
                ((:store-other :use-other) "another user's")
                )))
           (oops-remake-not-allowed ()
             (error "You cannot cause a system blast database to be remade!"))
           (notify-target-db ()
             (when guru? 
               (formatt ";; Note: A system database, ~A, will be created."
                        target-db 
                        )))
           (notify-existing-target-db ()
             (when guru?
               (formatt 
                (one-string-nl
                 ";; Note: A system database, ~A, already exists"
                 "for your target entities!"
                 )
                target-db
                )))
           (notify-db-not-there ()
             (formatt 
              ";; Note: Cannot remake ~A, that database does not exist!"
              use-db))
           (set-store-and-use (store use)
             (setq db-to-store-into store)
             (setq db-to-use use)
             ))

      (ecase target-set-status 

        (:create 
         ;; (setq also-create-target-db-named target-db)
         ;; (notify-target-db)
         (ecase use-db-status 
           ((nil) 
            (notify-target-db)
            (set-store-and-use target-db nil)
            ;; (setq also-create-target-db-named nil)
            )
           ((:use-system :use-other) 
            (if remake-db?
                (if guru?
                    (set-store-and-use db-specified nil)
                  (oops-write-not-allowed)
                  )
              (progn
                (set-store-and-use nil db-specified)
                ;; (setq also-create-target-db-named nil)
                )))
           (:use-me
            (if remake-db?
                (set-store-and-use db-specified nil)
              (progn
                (set-store-and-use nil db-specified)
                ;; (setq also-create-target-db-named nil)
                )))
           ((:store-system :store-other)
            (if guru?
                (set-store-and-use db-specified nil)
              (oops-write-not-allowed)
              ))
           (:store-me
            (set-store-and-use db-specified nil)
            )))
      
        (:existing
         (ecase use-db-status
           ((nil) 
            (if remake-db?
                (if guru?
                    (set-store-and-use target-db nil)
                  (oops-remake-not-allowed))
              (set-store-and-use nil target-db)
              ))
           ((:use-system :use-other)
            (notify-existing-target-db)
            (if remake-db?
                (if guru?
                    (set-store-and-use db-specified nil)
                  (oops-write-not-allowed))
              (set-store-and-use nil db-specified)
              ))
           (:use-me
            (notify-existing-target-db)
            (if remake-db?
                (set-store-and-use db-specified nil)
              (set-store-and-use nil db-specified)
              ))
           ((:store-system :store-other)
            (notify-existing-target-db)
            (if guru?
                (if remake-db?
                    (progn 
                      (notify-db-not-there)
                      (set-store-and-use db-specified nil))
                  (set-store-and-use nil db-specified))
              (oops-write-not-allowed)
              ))
           (:store-me
            (notify-existing-target-db)
            (if remake-db? 
                (progn
                  (notify-db-not-there)
                  (set-store-and-use db-specified nil))
              (set-store-and-use db-specified nil)
              ))))

        ((nil)
         (ecase use-db-status
           ((nil)
            (when remake-db? (notify-db-not-there))
            (set-store-and-use nil nil))
           ((:use-system :use-other)
            (if remake-db?
                (if guru?
                    (set-store-and-use db-specified nil)
                  (oops-write-not-allowed))
              (set-store-and-use nil db-specified)
              ))
           (:use-me
            (if remake-db?
                (set-store-and-use db-specified nil)
              (set-store-and-use nil db-specified)
              ))
           (:store-me
            (when remake-db? (notify-db-not-there))
            (set-store-and-use db-specified nil)
            )
           ((:store-system :store-other)
            (when remake-db? (notify-db-not-there))
            (if guru? 
                (set-store-and-use db-specified nil)
              (oops-write-not-allowed)
              )))))

      (values db-to-use db-to-store-into)

      )))

(defun new-blast-aux 
       (queries
        targets given-program threshold given-word-size
        translate remake-database
        &key use-database
        &aux word-size program
          q-type t-type targets-are-proteins? targets-are-organisms?
          (DNA-word-size-default 11)
          (AA-word-size-default 3)
          db-dir-to-use db-dir-to-store-into q-seqs
        )
        
  (when *debug-blast-aux*
    (formatt "Queries = ~S~%" queries)
    (formatt "targets = ~S~%" targets)
    (formatt "given-program = ~S~%" given-program)
    (formatt "threshold = ~S~%" threshold)
    (formatt "given-word-size = ~S~%" given-word-size)
    (formatt "translate = ~S~%" translate)
    (formatt "remake-database  = ~S~%" remake-database)
    (formatt "use-database = ~S~%" use-database))
  
  (setq q-type (list-type-for-blast given-program queries "Q"))
  (setq t-type (list-type-for-blast given-program targets "T"))
  (setq targets-are-proteins? (if (equal t-type 'aa) "T" "F"))
  (setq targets-are-organisms? (typep targets 'organism-list))
  (setq program 
        (string-downcase 
           (unless-provided given-program 
		      (inferred-program q-type t-type translate))))
  (setq word-size 
        (unless-provided 
         given-word-size
         (if (equal program "blastn") 
             dna-word-size-default
           aa-word-size-default
           )))

  (when *debug-blast-aux*
    (formatt "targets-are-organisms? = ~S~%" targets-are-organisms?)
    (formatt "targets-are-proteins? = ~S~%" targets-are-proteins?)
    (formatt "program = ~S~%" program)
    (formatt "word-size = ~S~%" word-size))

  (unless targets-are-organisms?
    (multiple-value-setq (db-dir-to-use db-dir-to-store-into)
        (determine-blast-aux-directories 
         targets t-type use-database remake-database)))

  (when *debug-blast-aux*
    (formatt "db-dir-to-use = ~S~%" db-dir-to-use)
    (formatt "db-dir-to-store-into = ~S~%" db-dir-to-store-into))

  (setq q-seqs (sequence-list-of queries :seq-type q-type))

  (IF (ALL-TRUE (MAPCAR (LAMBDA (x) (< word-size (LENGTH (SECOND x))))
                             q-seqs))
      (NEW-BLAST-aux-part2 q-seqs targets q-type t-type program 
	     threshold word-size targets-are-proteins? 
		 targets-are-organisms?	db-dir-to-use db-dir-to-store-into)
      (PROGN
        (WARN 
          (S+ "~&The length of at least one query is less than the"
              "~&~Aword-size (~A). Only exact matches will be reported."
              *NEWLINE* "It would be better to use the MATCH/ES-OF function.")
              (IF (PROVIDED given-word-size) "" "default ") word-size)
        (COND
           ((AND (> (LENGTH queries) 1) (> (LENGTH targets) 1))
                 (MATCHES-OF-ITEM EACH queries IN-EACH targets))
           ((> (LENGTH queries) 1)
                 (MATCHES-OF-ITEM EACH queries IN (FIRST targets)))
           ((> (LENGTH targets) 1)
                 (MATCHES-OF-ITEM (FIRST queries) IN-EACH targets))
           (T (MATCHES-OF-ITEM (FIRST queries) IN (FIRST targets)))))
	  )
)


(DEFUN New-blast-aux-part2
  (q-seqs targets q-type t-type program threshold word-size
    targets-are-proteins? targets-are-organisms?
	db-dir-to-use db-dir-to-store-into)
  (LET* ((t-seqs 
          (when (and (null db-dir-to-use) 
		             (not targets-are-organisms?))
             (sequence-list-of targets :seq-type t-type)))
         (safe? T)
		 (query-info)(blast-list)(e-sort-list)
        )

  (when *debug-blast-aux*
    (formatt "q-type = ~S~%" q-type)
    (formatt "t-type = ~S~%" t-type))
    
  ;; We always create a fasta file (.fa) for use by BLAST for
  ;; the query sequences.
  ;; This .fa file is created in the biobike system's temp directory 
  ;; and has a random name.  
  
  (biolisp::with-temporary-fasta-file 
      ;; attempt to use :user-label t but doesn't work!
      (prefix1 fapath1 fafile1 master-list1 :safe? safe?) q-seqs
    (setf query-info (LIST prefix1 fapath1 master-list1))

    (when *debug-blast-aux*
      (formatt "prefix1 = ~S~%" prefix1)
      (formatt "fapath1 = ~S~%" fapath1)
      (formatt "fafile1 = ~S~%" fafile1)
      (formatt "master-list1 = ~S~%" master-list1))

    (cond

     ;; if all the targets are organisms, we use the
     ;; blast metaindex feature to take advantage of
     ;; existing organism blast databases. 
     
     (targets-are-organisms?

      (let ((t-type (canonicalize-t-type t-type)))

        (case t-type
          (:aa (setq targets 
                  (LOOP FOR target IN targets
                        AS genes? = (#^genes target)
                        DO (IF (NOT genes?)
                               (WARN "Removing ~A because it lacks genes/proteins"
                                        target))
                        WHEN genes? COLLECT target))
                (IF (NOT targets)
                    (ERROR "No targets with genes/proteins")))
                #| (remove-if-not (lambda (x) (#^genes x)) targets))) |#
          (otherwise nil))
     
        (let* (
               ;; Get the organism database directories.
               ;; Determine how many contain valid databases.
               (target-db-directories 
                (blast-organism-database-directories targets t-type))
               (target-db-names 
                (loop for orgf in targets collect
                      (name-of-blast-database-dir-for-organism orgf t-type)
                      ))
               (blastdb-full-paths 
                (mapcar 
                 (lambda (dir db-name) (merge-pathnames (pathname dir) db-name))
                 target-db-directories
                 target-db-names 
                 ))
               (dbs-valid? 
                (mapcar 
                 (lambda (db-dir db-name)
                   (verify-blast-organism-database-directory 
                    db-dir db-name t-type
                    ))
                 target-db-directories
                 target-db-names
                 ))
               (n-targets (length targets))
               (n-valid (count-if-not 'null dbs-valid?))
               (n-invalid (count-if 'null dbs-valid?)))
        
          (when *debug-blast-aux*
            (formatt "T-type = ~S~%" t-type)
            (formatt "Attempting to search ~D organism databases~%" n-targets)
            (formatt "Existing organism databases: ~D~%" n-valid)
            (formatt "Databases that need to be created: ~D~%" n-invalid)
            )
        
          ;; If too many do not contain valid databases give it up,
          ;; tell user to contact sysadmin (if guru, say to run make-b-d-f-os)
          (when (> n-invalid 5)
            (if (not (wb::weblistener-guru-p))
                (error 
                 (one-string-nl
                  "There are too many organism databases that need to be"
                  " generated! (~D databases do not yet exist.)"
                  "Please contact the system administrator to have"
                  "these organism databases generated!"
                  )
                 n-invalid
                 )
              (error
               (one-string-nl
                "~D organism databases need to be generated!"
                "Use the function"
                "(BBI::MAKE-BLAST-DATABASE-FOR-ORGANISM org t-type)"
                "(where t-type is one of :aa, :dna, or :all)"
                "to generate them before trying to"
                "run Blast over all ~D organisms you requested.")
               n-invalid n-targets
               )))

          ;; Make all the necessary blast databases.

          (loop for orgf in targets 
                for valid? in dbs-valid? 
                unless valid?
                do
                (make-blast-database-for-organism orgf t-type :force? t)
                )

          ;; Create the metaindex file amalgamating all the organism databases
          ;; and then run the blast using this metaindex file
          (with-temp-file-in 
              (metaindex-filepath 
               cl-user::*tmp-directory* 
               :prefix "blast"
               :name "metaindex"
               :type (ecase t-type (:aa "pal") (:dna "nal"))
               )
            (when *debug-blast-aux*
              (loop for bfp in blastdb-full-paths 
                    do
                    (formatt "Organism target for metaindex: ~A~%" bfp))
              (formatt "Metaindex file: ~A~%" metaindex-filepath))
            (create-blast-metaindex-file metaindex-filepath blastdb-full-paths)
            (let* ((p (pathname metaindex-filepath))
                   (q (pathname-of-new-type p nil)))
              (when *debug-blast-aux*
                (formatt "metaindex target for blast-with2: ~A~%" 
                         (namestring q)))
              (setq 
               blast-list 
               (blast-with2 
                query-info (namestring q)
                program threshold word-size
                )))))))

     ;; If a database (a set of files) called USE-DATABASE 
     ;; (created by formatdb) exists already, we will use it.
     ;; We will not have to run formatdb (MAKE-BLAST-DATABASE).

     (db-dir-to-use 
        
      (let* ((db-name (lastelem (pathname-directory db-dir-to-use))))
        (when db-dir-to-store-into (error "This should be impossible!"))
        (formatt "~%;; Using blast database from ~A~%~%" db-dir-to-use)
        (let ((fapath2 (merge-pathnames db-name db-dir-to-use)))
          (setq blast-list 
                (blast-with2 query-info fapath2
                             program threshold word-size
                             )))))

     (t

      ;; if no database exists, we must create one
      ;; using formatdb (MAKE-BLAST-DATABASE) to be used by
      ;; the actual blast command (BLAST-WITH).  If we want
      ;; it to be saved, then after we've run the BLAST, we'll
      ;; copy the files (and add the .txt file) into the directory
      ;; where we want it to be saved to (db-dir-to-store-into)

      (biolisp::with-temporary-fasta-file 
          (prefix2 fapath2 fafile2 master-list2 :safe? safe? :user-label t)
          t-seqs

        (when *debug-blast-aux*
          (formatt "prefix2 = ~S~%" prefix2)
          (formatt "fapath2 = ~S~%" fapath2)
          (formatt "fafile2 = ~S~%" fafile2)
          (formatt "master-list2 = ~S~%" 
                   (mapcar 'second (initial-subseq-or-all master-list2 10))
                   ))

        (when *debug-blast-aux*
          (formatt "calling make-blast-database..~%"))
        (make-blast-database fapath2 targets-are-proteins?)
        (when *debug-blast-aux*
          (formatt "calling blast-with...~%"))
        (setf blast-list 
              (blast-with2 query-info fapath2 program threshold word-size))
        (if db-dir-to-store-into 
            (let* ((db-name 
                    (lastelem (pathname-directory db-dir-to-store-into))))
              (when *debug-blast-aux*
                (formatt 
                 "copying blast files from ~A to ~A, database name ~A~%"
                 fapath2 db-dir-to-store-into db-name))
              (when (wb::weblistener-guru-p)
                (formatt "~%;; Saving new blast database as ~A in ~A~%~%"
                         db-name db-dir-to-store-into))
              (new-move-blast-files fapath2 db-dir-to-store-into db-name)
              )
          (delete-blast-files fapath2)
          )))))

  (when *debug-blast-aux*
    (formatt "creating hit table from blast data...~%")
    (formatt "blast-list = ~S~%" blast-list)
    )
  
  (setf e-sort-list
        (stable-sort 
         blast-list 
         #'(lambda (x y) (< (ref x "E-VALUE") (ref y "E-VALUE")))))

  (for-each 
   hit-info in e-sort-list
   initialize hit-table = 
   (if e-sort-list (new-table (list (length e-sort-list) $)))
   for-each hit from 1
   do (for-each label in (labels-of hit-info dimension 1)
                as item = (ref hit-info label)
                do (if (member label (list "QUERY" "TARGET" "T-ORGANISM")
                               :test 'equal)
                       (setf item (or (frame-fnamed item) item)))
                (assign (ref hit-table hit label) = item))
   finally (return hit-table)
   )
))


(defun canonicalize-t-type (t-type)
  (cond
   ((symbol= t-type :aa) :aa)
   ((symbol= t-type :dna) :dna)
   (t (error "Unknown target-type: ~S" t-type))
   ))


(defun name-of-blast-database-dir-for-organism (orgf t-type)
  (s+
   (simplified-organism-name-for-blast-dir orgf)
   (case t-type
     (:aa "-aa")
     (:dna "-dna")
     (otherwise (error "t-type must be either :aa or :dna")))
   ))

(defun simplified-organism-name-for-blast-dir (orgf)
  (let ((orgn (#^fname orgf)))
    (substitute-if #\- (lambda (x) (not (alphanumericp x))) orgn)
    ))

(defun blast-database-dir-path-for-organism (orgf t-type)
  (let* ((sysdir (wb::user-directory "system"))
         (sysblastdir
          (append-subdir sysdir *stored-blast-directory-name*))
         (orgblastdir 
          (append-subdir 
           sysblastdir
           (name-of-blast-database-dir-for-organism orgf t-type)
           )))
    orgblastdir
    ))

(defun blast-organism-database-directories (orgfs t-type)
  (mapcar 
   (lambda (orgf) (blast-database-dir-path-for-organism orgf t-type))
   orgfs
   ))

(defun verify-blast-organism-database-directory (dir db-name t-type)
  (and (probe-file dir) 
       (loop for ext in (blast-db-extensions t-type)
             as required-file = (s+ db-name "." ext)
             as required-path = (merge-pathnames required-file dir)
             do
             (unless (probe-file required-path) (return nil))
             finally (return t)
             )))
         
(defun make-blast-databases-for-organisms (orgfs t-type &key (force? t))
  (loop for orgf in orgfs
        do
        (make-blast-database-for-organism orgf t-type :force? force?)
        ))

(defun make-blast-database-for-organism (orgf t-type &key (force? t))
  (flet  
      ((doit (t-type) 
         (let* ((t-seqs 
                 (sequence-list-of
                  (list orgf) :seq-type (intern t-type :bbi)))
                (targets-are-proteins? 
                 (ecase t-type (:aa "T") (:dna "F")))
                (orgblastdir 
                 (blast-database-dir-path-for-organism orgf t-type))
                (db-name 
                 (name-of-blast-database-dir-for-organism orgf t-type))
                )
           ;; only create the database if it appears as if 
           ;; it doesn't exist 
           
           (if (or force?
                   (null 
                    (verify-blast-organism-database-directory 
                     orgblastdir db-name t-type
                     )))
               (progn
                 ;; create the system blast database directory for the
                 ;; organism for the appropriate t-type
                 (handler-case 
                     (ensure-directories-exist 
                      (merge-pathnames "foo.txt" orgblastdir))
                   (error 
                    (c)
                    (error "Could not create or access ~A, actual error: ~A"
                           orgblastdir c
                           )))
                 (biolisp::with-temporary-fasta-file 
                     (prefix fasta-file-path fasta-file-name
                             master-list :safe? t :user-label t)
                     t-seqs
                   (when *debug-blast-aux*
                     (formatt "prefix = ~S~%" prefix)
                     (formatt "fasta-file-path = ~S~%" fasta-file-path)
                     (formatt "fasta-file-name = ~S~%" fasta-file-name)
                     )
                   (when *debug-blast-aux*
                     (formatt "calling make-blast-database on ~A~%" orgf))
                   ;; call formatdb to create the fasta database files 
                   ;; in the temporary directory, using our relocated
                   ;; fasta file (database files are created in the same 
                   ;; place as the location of the provided fasta file)
                   (make-blast-database fasta-file-path targets-are-proteins?)
                   ;; move the fasta database files from the temporary 
                   ;; directory to permanent storage
                   (new-move-blast-files fasta-file-path orgblastdir db-name)
                   ;; delete the temporary blast database files and the
                   ;; temporary fasta file 
                   (delete-blast-files fasta-file-path)
                   :created
                   ))
             :existing
             ))))
    (cond
     ((symbol= t-type :aa) (doit :aa))
     ((symbol= t-type :dna) (doit :dna))
     ((or (eq t t-type) (symbol= t-type :all)) 
      (doit :aa) (doit :dna)
      ))))

(defun create-blast-metaindex-file
       (full-metaindex-pathname blast-db-pathnames &key (title nil))
  (with-open-file (p full-metaindex-pathname
                     :if-exists :supersede :direction :output)
    (format p "TITLE ~A~%" (or title "Biobike Metaindex file"))
    (format p "#~%")
    (format p "DBLIST ")
    (loop for path in blast-db-pathnames
          do
          (format p "~A " path)
          finally (terpri p)
          )))
      
;; This is an alternative version of BLAST-WITH found in 
;; .../bike/5level.bike which does not use the target master list
(defun blast-with2
       (query-info target-fapath program threshold word-size 
                   &key (delete-outfile t) (user-label nil))
  "Runs a blast with a given database"
  (let* ((prefix1 (first query-info))
         (query-fapath (second query-info))
         (master-list1 (third query-info))
         (outfile (format nil "~a.out" prefix1))
         (outpath (merge-pathnames  user:*tmp-directory* outfile))
         (blastall-command
          (format 
           nil 
           (one-string
            "~Ablastall -p ~a -d ~a -i ~a -o ~a -e ~a -m 8 -W ~A -FF "
            ">& /dev/null")
           cl-user::*blast-executable-toplevel-dir* program
           target-fapath query-fapath outpath threshold word-size)))
    
    (when *debug-blast-aux*
      (formatt "blastall-command = ~S~%" blastall-command))

    (case (protected-shell-command blastall-command
                                   :action-on-invalid-return :error)
      (:timeout (error "***** Blast failed due to timeout! *****"))
      (otherwise nil))

    (let ((results 
           (with-open-file (i outpath)
             (loop 
              for line = (read-line i nil nil) 
              until (null line)
              as line-data = (cl-ppcre::split "\\s+" line)
              as hit = (utils::make-garray '($))
              do 
              (destructuring-bind 
                  (from to pid all nm ng sq eq ss es eval bits) 
                  line-data
                (let ((query 
                       ;; this user-label stuff doesn't work and
                       ;; we don't know why! 
                       (if user-label 
                           (second 
                            (find 
                             (read-from-string from) master-list1 
                             :test 'equal :key 'second 
                             ))
                         (second 
                          (assoc (read-from-string from) master-list1
                                 :test 'equal
                                 )))))
                  (setf (gref hit "QUERY") query)
                  (setf (gref hit "TARGET") to)
                  (setf (gref hit "%ID") (read-from-string pid))
                  (setf (gref hit "ALIGN-LENGTH") (read-from-string all))
                  (setf (gref hit "N-MISMATCHES") (read-from-string nm))
                  (setf (gref hit "GAPS") (read-from-string ng))
                  (setf (gref hit "Q-START") (read-from-string sq))
                  (setf (gref hit "Q-END") (read-from-string eq))
                  (setf (gref hit "T-START") (read-from-string ss))
                  (setf (gref hit "T-END") (read-from-string es))
                  (setf (gref hit "E-VALUE")  
                        (read-from-string (substitute #\d #\e eval)))
                  (setf (gref hit "BIT-SCORE") (read-from-string bits))
                  ))
              collect hit
              ))))
      (when delete-outfile (when (probe-file outpath) (delete-file outpath)))
      results
      )))