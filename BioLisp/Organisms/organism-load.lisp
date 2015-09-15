;; -*- mode: Lisp; Syntax: Common-Lisp; Package: bio; -*-

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

;;; Author:  JP Massar, Mark Slupesky.

;;;; AVAILABLE-ORGANISMS
;;;; PRELOAD-ORGANISMS
;;;; LOAD-ORGANISMS 
;;;; LOAD-ORGANISM
;;;; PPRINT-ORGANISM
;;;; ADD-ORGANISM-ALIAS
;;;; ORGANISM

;;; We assume a file called available-organisms.lisp which contains a single
;;; list whose elements are the names (without any quote marks) of all the
;;; organisms available for loading.  

(defparameter *warn-about-blank-lines* nil)

(defun available-organisms (&key (as :frames) (dir nil))
  #.(one-string-nl
     "Returns a list of organisms potentially available for analysis."
     "If AS is :frames or :frame (the default) a list of frames is returned."
     "If AS is :strings or :string a list of strings is returned."
     "If AS is :symbols or :symbol symbols in the BIOLISP package are returned."
     "See *LOADED-ORGANISMS* for a list of organisms which are fully loaded."
     "To make organisms accessible see (LOAD-ORGANISMS ....)")
  (ecase user::*frame-system-version*
    (:old 
     (let ((ofile 
            (namestring
             (merge-pathnames 
              (or dir (organism-toplevel-directory))
              "available-organisms.lisp"))))
       (with-open-file (s ofile :direction :input :if-does-not-exist nil)
         (if (null s)
             (progn 
               (cformatt "There is no organisms file ~A" ofile)
               nil)
           (let ((organism-strings (read s nil nil)))
             (if (null organism-strings) 
                 (cformatt "There are no organisms in ~A" ofile)
               (progn
                 (unless (every 'stringp organism-strings)
                   (error "Organism names in  ~A are not all strings" ofile))
                 (flet ((symbols-in-package (strings package)
                          (mapcar 
                           (lambda (x) (intern (string-upcase x) package))
                           strings                   
                           )))
                   (case as
                     ((:string :strings) organism-strings)
                     ((:symbol :symbols) 
                      (symbols-in-package organism-strings :bio))
                     ((:keyword :keywords)
                      (symbols-in-package organism-strings :keyword))
                     (otherwise
                      (mapcar (lambda (x) (frame-fnamed x t)) organism-strings))
                     )))))))))
    (:sframes 
     (forward-funcall 
      (if user::*master-list-enabled* 
          'available-organisms-msf
        'available-organisms-sf)
      :as as :dir dir
      ))))

(defun crossblast-organisms (&key (as :frames) (dir nil) (force? nil))
  #.(one-string-nl
     "Returns a list of organisms whose genes are available in"
     "the cross blast table."
     "If AS is :frames or :frame (the default) a list of frames is returned."
     "If AS is :strings or :string a list of strings is returned."
     "If AS is :symbols or :symbol symbols in the BIOLISP package are returned."
     )
  (labels ((symbols-in-package (strings package)
             (mapcar 
              (lambda (x) (intern (string-upcase x) package))
              strings                   
              ))
           (convert (organism-strings as)
             (case as
               ((:string :strings) organism-strings)
               ((:symbol :symbols) 
                (symbols-in-package organism-strings :bio))
               ((:keyword :keywords)
                (symbols-in-package organism-strings :keyword))
               (otherwise
                (mapcar (lambda (x) (frame-fnamed x t)) organism-strings))
               )))
    (when (and (not force?) *crossblast-organisms*)
      (return-from crossblast-organisms
        (convert (mapcar 'fname *crossblast-organisms*) as)))
    (let ((ofile 
           (namestring
            (merge-pathnames 
             (or dir (organism-toplevel-directory)) 
             "crossblast-organisms.lisp"))))
      (with-open-file (s ofile :direction :input :if-does-not-exist nil)
        (if (null s)
            (progn 
              (cformatt "There is no crossblast organisms file ~A" ofile)
              nil)
          (let ((organism-strings (read s nil nil)))
            (if (null organism-strings) 
                (cformatt "There are no organisms in ~A" ofile)
              (progn
                (unless (every 'stringp organism-strings)
                  (error "Organism names in  ~A are not all strings" ofile))
                (convert organism-strings as)
                ))))))))


(defun common-ortholog-organisms (&key (as :frames) (dir nil) (force? nil))
  #.(one-string-nl
     "Returns a list of organisms whose genes are available in"
     "the list of common orthologs."
     "If AS is :frames or :frame (the default) a list of frames is returned."
     "If AS is :strings or :string a list of strings is returned."
     "If AS is :symbols or :symbol symbols in the BIOLISP package are returned."
     )
  (labels ((symbols-in-package (strings package)
             (mapcar 
              (lambda (x) (intern (string-upcase x) package))
              strings                   
              ))
           (convert (organism-strings as)
             (case as
               ((:string :strings) organism-strings)
               ((:symbol :symbols) 
                (symbols-in-package organism-strings :bio))
               ((:keyword :keywords)
                (symbols-in-package organism-strings :keyword))
               (otherwise
                (mapcar (lambda (x) (frame-fnamed x t)) organism-strings))
               )))
    (when (and (not force?) *common-ortholog-organisms*)
      (return-from common-ortholog-organisms
        (convert (mapcar 'fname *common-ortholog-organisms*) as)))
    (let ((ofile 
           (namestring
            (merge-pathnames 
             (or dir (organism-toplevel-directory)) 
             "common-ortholog-organisms.lisp"))))
      (with-open-file (s ofile :direction :input :if-does-not-exist nil)
        (if (null s)
            (progn 
              (cformatt "There is no common ortholog organisms file ~A" ofile)
              nil)
          (let ((organism-strings (read s nil nil)))
            (if (null organism-strings) 
                (cformatt "There are no organisms in ~A" ofile)
              (progn
                (unless (every 'stringp organism-strings)
                  (error "Organism names in  ~A are not all strings" ofile))
                (convert organism-strings as)
                ))))))))

;;; This should get called by the BioLingua instance initialization method.

(defun preload-organisms (&key (dir nil) (only nil))

  #.(one-string-nl 
     "Loads auxiliary information for all the organisms available to"
     "this instance of Biolingua (via the AVAILABLE-ORGANISMS function)."
     "This information is found in the plist.lisp file in the organism's"
     "directory."
     "To preload only certain organisms, use the ONLY keyword.  It takes"
     "a list of organism frames."
     "Does NOT load the genome, gene, and protein information.  A frame"
     "is created for each organism, along with exported symbols bound to"
     "that frame whose names are the nicknames of the organism."
     "Finally a list of the organisms preloaded is returned.")

  ;; Read the file containing the names of all organisms in the 
  ;; cross blast table and create frames 
  (let ((crossblast-names 
         (mapcar 
          'string-downcase
          (crossblast-organisms :as :strings :dir dir))))
    (setq *crossblast-organisms* 
          (loop for name in crossblast-names 
                collect (frame-fnamed name t)
                )))
  (cformatt "~D organisms found in crossblast file" 
            (length *crossblast-organisms*))

  ;; Preload the organisms specified 

  (setq only (mapcar 'string-downcase (mapcar 'fstring (ensure-list only)))) 
  
  (let ((organism-names 
         (or (and only (ensure-list only)) 
             (mapcar 
              'string-downcase (available-organisms :as :strings :dir dir)))))

    ;; For each organism
    (loop for orgn in organism-names
          ;; Create a frame for it if one does not already exist
          ;; as orgf = (frame-fnamed orgn t)
          as orgf = (make-appropriate-organism-frame orgn t)
          as orgdir = (organism-data-directory orgn :dir dir)
          do
          (cformatt "Preloading organism ~A" orgf)          
          (populate-organism-frame-slots-from-plist-file orgf orgn :dir dir)
          (pushnew #$Organism (slotv orgf #$Isa))
          (setf (slotv orgf #$Organism-Data-Directory) (namestring orgdir))
          (setf (slotv orgf #$Organism-Info-Loaded) t)
          ;; if it's already loaded, preserve that information,
          ;; otherwise the slotv returns NIL and the slot is created.  
          ;; Presumably only relevant for private organisms being reloaded
          ;; inadvertently.  
          (setf (slotv orgf #$Organism-loaded?) (slotv orgf #$Organism-loaded?))
          (create-and-export-organism-nickname-symbols orgf)
          (cformatt "Organism ~A preloaded." orgf)
          (cformatt "  Nicknames: ~A" (#^Nicknames orgf))
          collect orgf
          ))
  
  )
  
                

(defun organism (org)
  #.(one-string-nl
     "Convert an organism designator to an organism frame.  "
     "Search all preloaded organisms, including all their nicknames,"
     "for possible matches.")
  (canonicalize-organism-designator org))


(defun organism-plist-file (orgn &key (dir nil))
   (make-pathname
    :name "plist"
    :type "lisp"
    :defaults (organism-data-directory orgn :dir dir)
    ))

(defun read-organism-plist-file (plist-file)
  (let ((*package* (find-package :keyword)))
    (with-open-file (p plist-file :direction :input) 
      (unkeywordize-property-list (read p))
      )))

(defun populate-organism-frame-slots-from-plist-file (orgf orgn &key (dir nil))
  (let ((opf (organism-plist-file orgn :dir dir)))
    (populate-frame-slots-from-plist-file orgf orgn opf)
    ))

(defun populate-frame-slots-from-plist-file (frame frame-name file)
  (unless (probe-file file)
    (error "PLIST file '~A' for ~A does not exist!" file frame-name))
  (let ((plist-data (read-organism-plist-file file)))
    (loop for (key data) in plist-data do
          (setf (slotv frame (frame-fnamed (string key) t)) data)
          finally (return plist-data)
          )))


(defun create-and-export-organism-nickname-symbols (orgf)
  (setf (#^organism-symbols orgf) nil)
  (loop for nickname in (cons (#^fname orgf) (#^Nicknames orgf)) do
        (unless (stringp nickname)
          (ierror 
           "Ruh roh. Organism nickname ~S for ~A isn't a string!!"
           nickname orgf
           ))
        (create-and-export-organism-nickname-symbol orgf nickname)
        )
  (setf (slotv orgf #$Organism-symbols)
        (nreverse (slotv orgf #$Organism-symbols)))
  orgf)

(defun create-and-export-organism-nickname-symbol (orgf nickname)
  (block exit
    (let ((packages-used-by-user 
           (cons
            :lisp
            (remove 
             *organism-nickname-package*
             (wb::application-packages-to-use cl-user::*ai*)
             )))
          (upnick (string-upcase nickname)))
      (loop for pkg in packages-used-by-user
            do
            #+debug
            (when (string-equal nickname "LAMBDA") 
              (print packages-used-by-user)
              (print (find-symbol nickname pkg))
              (multiple-value-bind (s status)
                  (find-symbol nickname pkg)
                (print (list s status))
                )
              (break "LAMBDA"))
            (multiple-value-bind (s status) 
                (find-symbol upnick pkg)
              (when (and s (eq status :external))
                (formatt 
                 (one-string-nl
                  ";; *** Could not use nickname '~A' for organism ~A"
                  ";; ***   because there already exists an external symbol by"
                  ";; ***   that name in the ~A package."
                  )
                 nickname orgf pkg
                 )
                (return-from exit nil)
                )))
      (let ((symbol (intern upnick *organism-nickname-package*)))
        (handler-case 
            (export symbol *organism-nickname-package*)
          (error 
           (c)
           (formatt 
            (one-string-nl
             ";; *** Could not use nickname ~A for organism ~A"
             ";; ***   Exporting symbol from ~A caused error."
             ";; ***   Actual error: ~A"
             )
            symbol orgf *organism-nickname-package* c
            )))
        (handler-case 
            (when (find-package :bbl)
              (import symbol :bbl)
              (export (list symbol) :bbl))
          (error 
           (c)
           (formatt
            (one-string-nl
             ";; *** Could not import or export symbol ~S"
             ";; *** in package ~S, into the BBL package!"
             ";; *** Actual error: ~A"
             )
            symbol (package-name (symbol-package symbol)) c
            )))
        (handler-bind
            ((warning #'(lambda (c) (declare (ignore c)) (muffle-warning))))
          (eval
           ;; avoid problem with redefinition check by using lisp:defconstant
           ;; instead of our overloaded redefinition
           `(lisp:defconstant ,symbol ,orgf
              ,(formatn "A nickname for the organism ~A" (#^Fname orgf))
              )))
        (pushnew symbol (slotv orgf #$Organism-symbols))
        ))))

(defun add-organism-alias (alias orgf)
  #.(one-string-nl 
     "Adds (string ALIAS) as a nickname of the organism ORGF, and"
     "creates an exported symbol in the biolisp package of the same"
     "name (all upper case) bound to the organism frame.")  
  (cond 
   ((symbolp alias) (add-organism-alias (string alias) orgf))
   ((stringp alias) 
    (pushnew alias (#^nicknames orgf))
    (create-and-export-organism-nickname-symbols orgf))
   (t (error "An organism alias must be a string or a symbol."))
   ))

(defun organism-dir (orgf) (#^organism-data-directory orgf))

(defun organism-genome-dir (orgf)
  (append-subdir (organism-dir orgf) "genome"))

(defun organism-genes-dir (orgf)
  (append-subdir (organism-dir orgf) "genes"))

(defun organism-proteins-dir (orgf)
  (append-subdir (organism-dir orgf) "proteins"))

(defun organism-seqinfo-dir (orgf)
  (append-subdir (organism-dir orgf) "seqinfo"))

(defun organism-documentation-dir (orgf)
  (append-subdir (organism-dir orgf) "documentation"))

(defun organism-test-dir (orgf) (organism-documentation-dir orgf))

(defun organism-tbl-file (orgf dirname)
  "The name of the tbl file is the same as the directory name."
  (merge-pathnames 
   (make-pathname :name dirname :type "tbl") 
   (append-subdir (organism-dir orgf) dirname)
   ))

(defun organism-fasta-file (orgf dirname)
  "The name of the fasta file is the same as the directory name."
  (merge-pathnames 
   (make-pathname :name dirname :type "fasta") 
   (append-subdir (organism-dir orgf) dirname)
   ))

(defun organism-testfile (orgf)
  (let ((file
         (merge-pathnames 
          *organisms-seq-testfile* 
          (organism-test-dir orgf)
          )))
    (when (probe-file file) file)))

(defun load-organisms (&rest key-args
                             &key 
                             (organisms (available-organisms))
                             (verbose? t) 
                             (reload? nil) 
                             (rebuild? nil)
                             (plist? nil)
                             (postload? t)
                             (data-structures? t)
                             (pprint nil)
                             (pprint-limit 10)
                             (test? t)
                             (test-all-genes? nil)
                             (descriptions? nil)
                             (private? nil)
                             (dir nil)
                             (if-seed-more-than-error 50)
                             )
  #.(one-string-nl
     "Loads all the organisms in ORGANISMS (default all available organisms)"
     "as if by calling LOAD-ORGANISM on each organism.")

  (declare (ignorable organisms verbose? reload? rebuild? plist? 
                      postload? data-structures? pprint pprint-limit
                      test? test-all-genes? descriptions? private?
                      dir if-seed-more-than-error))
  
  (block exit
    
    (ecase user::*frame-system-version* 
      (:old nil)
      (:sframes 
       (if (every (lambda (x) (typep x 'bio::seed-organism)) organisms)
           (if (> (length organisms) if-seed-more-than-error)
               (error "Cannot load more than ~D organisms at one time!"
                      if-seed-more-than-error)
             (loop for organism in organisms
                   do
                   (apply 
                    'load-organism
                    organism (utils::remove-key-and-value :organisms key-args))
                   finally (return-from exit)
                   ))
         (error "Attempting to load both seed and non-seed organisms!")
         )))

    (loop for organism in organisms
          do
          (when (and (isframe? organism)
                     (null (#^Organism-Info-Loaded organism)))
            (preload-organisms :dir dir :only (list organism)))
          (apply 
           'load-organism 
           organism (utils::remove-key-and-value :organisms key-args)
           )))

  (when verbose? (cformatt "All organisms loaded."))

  )

(defun create-organism-load-broadcast-stream 
       (string-stream &optional (verbose? t))
  (if (and verbose? (or (null wb::*username*) (wb::weblistener-guru-p)))
      (make-broadcast-stream string-stream *standard-output*)
    (make-broadcast-stream string-stream (make-broadcast-stream))
    ))

(defvar *organism-already-loaded?* nil)

(defun load-organism (organism 
                      &rest key-args
                      &key 
                      (verbose? t) 
                      (reload? nil) 
                      (rebuild? nil)
                      (plist? nil)
                      (postload? t)
                      (data-structures? t)
                      (pprint nil)
                      (pprint-limit 10)
                      (test? t)
                      (test-all-genes? nil)
                      (descriptions? nil)
                      (private? nil)
                      (dir nil)
                      ;; seed/acache only 
                      (commit? nil)
                      &aux
                      (orgf nil)
                      (string-list-slots nil)
                      )
  #.(one-string-nl
     "Load information from the organisms directory about an organism. "
     "Creates genome frames for each contig of the organism, and"
     "gene, transcript and protein frames if these sudbirectories exist."  
     "Modifies *loaded-organisms* appropriately."
     "If RELOAD? is NIL (the default), only information which has not"
     "already been loaded will be loaded.  If RELOAD? is non-NIL, all"
     "specified components of the organism will be reloaded."
     "If REBUILD? is T (the default is NIL), then the sequence files"
     "built from the fasta files are always regenerated, otherwise"
     "they are only regenerated if they do not exist or they are out of date."
     "If PRIVATE? is T, then the caller must also specify DIR, and"
     "the organism data is expected to be found in DIR."
     "The organism is loaded from this directory but is not included"
     "in the list of loaded or available organisms; in a sense, it is"
     "private to the individual loading the organism (but accessible to anyone"
     "if they know its name.)  When PRIVATE? is used, the ORGANISM"
     "parameter MUST designate the exact name of the organism, not"
     "a nickname."
     "The DESCRIPTIONS? keyword controls whether textual data describing"
     "individual genes, proteins and transcripts gets loaded. The default"
     "is NIL, do not load this data."
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
     "If TEST-ALL-GENES? is T (assuming TEST? is T) then as well as"
     "the simple test, every gene's sequence is extracted as a more"
     "robust test of the organism."
     )
  (declare 
   (ignorable 
    verbose? reload? rebuild? plist? postload? data-structures? pprint
    pprint-limit test? test-all-genes? descriptions? private? dir commit?))
  ;; capture all the printout from executing a load-organism 
  ;; and store it in the #$load-log slot of the organism frame
  (let ((*organism-already-loaded?* nil)
        (organism-load-log 
         (with-output-to-string (s) 
           (let ((*standard-output* 
                  (create-organism-load-broadcast-stream s verbose?)
                  ))
             ;; call either load-seed-organism or load-non-seed-organism
             ;; as the case may be
             (block exit 
               (ecase user::*frame-system-version* 
                 (:old nil)
                 (:sframes 
                  (when (typep organism 'bio::seed-organism)
                    (setq
                     orgf
                     (apply
                      (symbol-of-package :load-seed-organism :bio)
                      organism key-args
                      ))
                    (setf string-list-slots (list #$annotation-history))
                    (return-from exit nil)
                    )))
               (setq orgf (apply 'load-non-seed-organism organism key-args))
               )
             (when (not *organism-already-loaded?*)
               (check-organism-for-non-ascii-strings 
                orgf verbose? :string-list-slots string-list-slots)
               )))))
    (when (and orgf organism-load-log)
      (setf (#^load-log orgf) organism-load-log))
    orgf
    ))
        
(defun load-non-seed-organism 
       (organism 
        &rest key-args
        &key 
        (verbose? t) 
        (reload? nil) 
        (rebuild? nil)
        (plist? nil)
        (postload? t)
        (data-structures? t)
        (pprint nil)
        (pprint-limit 10)
        (test? t)
        (test-all-genes? nil)
        (descriptions? nil)
        (private? nil)
        (dir nil)
        ;; seed/acache only 
        (commit? nil)
        &aux
        (orgn (fstring organism))
        (orgf nil)
        )
  
  (declare (ignore commit?))
  (declare (ignorable key-args))
              
  (if private? 
      (unless dir 
        (error "If you specify PRIVATE?, you must specify a DIR (directory)."))
    (when dir
      (error "You can only specify a DIR (directory) if you specify PRIVATE?")))

  (when private? (preload-organisms :dir dir :only organism))

  (cformatt "Now starting to load ~A" organism)
  ;; find the organism associated with ORGANISM which can either be 
  ;; an organism frame, an organism name, or an organism nickname.

  (setq orgf (canonicalize-organism-designator organism))

  ;; check to see if this organism has been preloaded
  (unless orgf 
    (error (one-string 
            "The organism denoted by '~A' either hasn't yet been preloaded "
            "or is not a known organism name or nickname.") organism))
  (setq orgn (fname orgf))
  
  (when (and (not rebuild?) (not reload?)
             (slotv orgf #$organism-loaded?))
    (when verbose? (cformatt "Organism ~A already loaded." organism))
    (setq *organism-already-loaded?* t)
    (return-from load-non-seed-organism orgf))

  ;; make sure no two prefixes are identical
  (vwhen (prefix (and (not reload?) (not rebuild?) 
                      (slotv orgf #$Organism-Prefix)))
    (loop for existing-organism in (appropriate-extant-organism-set)
          ;; (loop for existing-organism in *loaded-organisms* 
          as existing-prefix = 
          (slotv existing-organism #$organism-prefix)
          when 
          (and existing-prefix (string-equal prefix existing-prefix)) 
          do 
          (error
           (one-string-nl
            "Organism being loaded, ~A, has the same prefix, ~S"
            "as an existing organism ~A (prefix ~S)")
           orgf prefix existing-organism existing-prefix)))

  (when (or reload? rebuild? plist?)
    (cformatt "Now repopulating slots from plist file.")
    (populate-organism-frame-slots-from-plist-file orgf orgn))
    
  (let ((directory-invalid? nil))

    (flet ((flag-n-warn (warn-string &rest warn-args)
             (apply 'warn warn-string warn-args)
             (setq directory-invalid? t)))
    
      
      ;; genome/genome.tbl
      (unless (probe-file (organism-tbl-file orgf "genome"))
        (warn
         "There is no TBL file in the GENOME directory for organism ~A"
         orgn))

      ;; genome/genome.fasta
      (unless (probe-file (organism-fasta-file orgf "genome"))
        (flag-n-warn
         "There is no FASTA file in the GENOME directory for organism ~A"
         orgn))
          
      (when verbose? (formatt ";; Checked Genome, "))

      ;; genes/genes.tbl
      (unless (#^no-genes-or-proteins? orgf)
        (unless (probe-file (organism-tbl-file orgf  "genes"))
          (flag-n-warn
           "There is no TBL file in the GENES directory for organism ~A"
           orgn))
        (when verbose? (formatt "Genes, ")))        
        
      ;; proteins/proteins.tbl
      
      (unless (or (#^no-genes-or-proteins? orgf) (#^no-proteins? orgf))
      
        (unless (probe-file  (organism-tbl-file orgf "proteins"))
          (flag-n-warn
           "There is no TBL file in the PROTEINS directory for organism ~A"
           orgn))

        ;;proteins/proteins.fasta
        (unless (probe-file (organism-fasta-file orgf  "proteins"))
          (flag-n-warn
           "There is no FASTA file in the PROTEINS directory for organism ~A"
           orgn))

        (when verbose? (formatt "and Proteins directories.~%")))

      (when directory-invalid? 
        (error "Fatal error: Invalid directory structure for organism ~A"
               orgn))

      ))
        
        
  (let ((count 
         (load-the-genome-into-frames 
          orgf descriptions? :rebuild? rebuild?)))
    (when verbose? (cformatt "Genome Done, loaded ~D contigs" count)))

  (unless (#^no-genes-or-proteins? orgf) 
    (let ((count (load-the-genes-into-frames orgf descriptions?)))
      (when verbose? (cformatt "Genes Done, loaded ~D genes" count))))

  (unless (or (#^no-genes-or-proteins? orgf) (#^no-proteins? orgf))
    (let ((count 
           (load-the-proteins-into-frames 
            orgf descriptions? :rebuild? rebuild?)))
      (when verbose? (cformatt "Proteins Done, loaded ~D proteins" count))))

  (canonicalize-circular-contig-slot orgf)
  (when verbose? (cformatt "#$circular slot values canonicalized."))

  (when verbose? (cformatt "Organism ~A loaded successfully." orgf))
  
  (when postload? 
    (do-organism-postload-processing orgf verbose?))

  (unless (#^no-genes-or-proteins? orgf)
    (when data-structures? 
      (when verbose? (cformatt "Creating organism data structures"))
      (create-organism-data-structures 
       :organisms (list orgf) :verbose? verbose?
       )))

  (unless private? (pushnew orgf *loaded-organisms*))

  (setf (slotv orgf #$organism-loaded?) T)

  (when pprint (pprint-organism orgf pprint-limit))
  
  (when test?
    (let ((testfile (organism-testfile orgf)))
      (cond
       ((and (null testfile) (null test-all-genes?))
        (cformatt "No test file found.  No tests being run."))
       (t
        (test-sequence-extraction 
         orgf
         :test-file testfile
         :other-genes? test-all-genes?
         :verbose? t
         )))))
  
  orgf
  
  )

(defun canonicalize-circular-contig-slot (orgf)
  (loop for c in (#^contiguous-sequences orgf)
        as cvalue = (#^circular c)
        do
        (cond
         ((or (null cvalue) (eq t cvalue)) nil)
         ((and (stringp cvalue) 
               (or (string-equal cvalue "nil") 
                   (string-equal cvalue "false")
                   (string-equal cvalue "f")
                   (string-equal cvalue "0")
                   ))
          (setq cvalue nil))
         ((and (stringp cvalue)
               (or (string-equal cvalue "t")
                   (string-equal cvalue "true")
                   (string-equal cvalue "1")
                   ))
          (setq cvalue t))
         ((and (numberp cvalue) (zerop cvalue)) (setq cvalue nil))
         ((numberp cvalue) (setq cvalue t))
         (t 
          (error
           (one-string-nl
            "Unrecognizable value '~A' for #$circular slot of"
            "contig ~A of organism ~A.")
           cvalue c orgf
           )))
        (setf (#^circular c) cvalue)
        ))
           
(defun check-organism-for-non-ascii-strings 
       (orgf verbose? &key (string-list-slots nil))
  (when verbose? (cformatt "Checking organism for non-ascii strings..."))
  (let ((changed-frames 
         (check-and-replace-org-frame-strings-with-bad-chars 
          orgf :string-list-slots string-list-slots)))
    (when verbose?
      (loop for (frame slot) in changed-frames
            do
            (cformatt "Changed slot ~A of ~A to ~S" 
                      slot frame (slotv frame slot)
                      )))))

(defun unload-organism (organism &aux orgf)
  #.(one-string-nl
     "Deletes (uninterns the frames of) the contiguous sequences,"
     "genes, proteins, and fragments, if any, of the organism frame"
     "denoted by ORGANISM.  Other information about the organism"
     "is preserved."
     "You are only allowed to delete private organisms unless you are a guru") 
  (setq orgf (canonicalize-organism-designator organism))
  (when (member orgf *loaded-organisms*) 
    (unless (wb::weblistener-guru-p) 
      (error "You are not allowed to unload universally available organisms.")))
  (cformatt "Unloading organism ~A" orgf)
  (flet ((delete-slot-n-frames (slot)
           (loop for frame in (slotv orgf slot) do (unintern-frame frame))
           (setf (slotv orgf slot) nil)))
    (delete-slot-n-frames #$contiguous-sequences)
    (delete-slot-n-frames #$genes)
    (setf (slotv orgf #$noncoding-genes) nil) 
    (delete-slot-n-frames #$proteins)
    (delete-slot-n-frames #$fragments)
    (setf (slotv orgf #$organism-loaded?) nil)
    (when (open-stream-p (#^genome-sequence-stream orgf))
      (close (#^genome-sequence-stream orgf))
      (setf (slotv orgf #$genome-sequence-stream) nil))
    (setq *loaded-organisms* (delete orgf *loaded-organisms*))
    (cformatt "Unloaded organism ~A" orgf)
    ))

(defun do-organism-postload-processing (orgf verbose? &aux orgn)
  (setq orgn (string-downcase (fname orgf)))
  (let* ((postload-file-name (organism-postload-file-name orgn))
         (postload-directory (organism-dir orgf))
         (postload-file
          (merge-pathnames
           postload-file-name
           postload-directory
           )))
    (unless (probe-file postload-file)
      (generic-postload-function 
       cl-user::*organisms-descriptor* orgf 
       :postload-directory postload-directory 
       :verbose? nil
       )
      (return-from do-organism-postload-processing nil))
    (when verbose? (cformatt "Doing postload processing."))
    (when verbose? 
      (cformatt 
       "Loading postload file ~A" (namestring (truename postload-file))))
    (handler-case
        (progn
          (load postload-file)
          (when verbose? (cformatt "Running postload function."))
          (funcall (compile (organism-postload-function-name orgn))))
      (error 
       (c)
       (cformatt "***** Problem compiling, loading or running postload file.")
       (cformatt "  Filepath: ~A" (namestring (truename postload-file)))
       (cformatt "  Error actually signalled: ~A" c)
       (cformatt "***** This likely means the organism is not fully loaded!!")
       (cformatt "***** This probably should not be happening.")
       (cformatt "***** Please let the system administrators know.")
       ))))


(defun load-the-genome-into-frames 
       (orgf load-descriptions? 
             &key (rebuild? nil) &aux (dirname "genome"))
  
  (setf (slotv orgf #$Contiguous-Sequences) nil)
  ;; what we want to end up with is a three element list...
  ;; the elements being NAME, PROPERTIES (a list), and a DESCRIPTION
  ;; the problem is tables can have lots of rows and whatnot.  
  ;; so our strategy is to just move everything that
  ;; isn't a name or a description into the properties list.  

  ;; the idea is to make a vector which has the headers from the table.  
  ;; we will then find the
  ;; name, description, and properties headers using this vector. 
  ;; we remove them, make an assoc list of sorts,
  ;; and stick them in the properties list.  
  (let* ((orgn (fname orgf))
         (tbl-file (organism-tbl-file orgf dirname))
         (fasta-file (organism-fasta-file orgf dirname))
         (tbl-data (retrieve-entire-table tbl-file))
         (headers (pop tbl-data))
         (nameref (position "name" headers :test 'string-equal))
         (propref (position "properties" headers :test 'string-equal))
         (descref (position "description" headers :test 'string-equal))
         (refs (remove nil (list nameref propref descref)))
         (newheaderlist (mapcar 'keywordize (index-remover refs headers)))
         (contigs nil))

    (unless nameref 
      (error "No NAME column in organism ~A genome.tbl file" orgf))

    (flet ((all-else-to-properties (lst)
             (let* ((newlst (index-remover refs lst))
                    (assoclist (mapcar 'list newheaderlist newlst)))
               (list (nth nameref lst)
                     (append 
                      (when propref 
                        (let ((val (nth propref lst)))
                          (when (not (zerop (length val)))
                            (organism-property-string-to-key-value-list val))))
                      assoclist)
                     (when descref (nth descref lst))))))
      (let ((new-tbl-data (mapcar #'all-else-to-properties tbl-data)))
        ;; so now new-tbl-data is a three element list, 
        ;; (NAME (PROPERTY LIST) DESCRIPTION)
        (loop for (name properties description) in new-tbl-data 
              for count fixnum from 0
              do
              (if (zerop (length name))
                  (when *warn-about-blank-lines* 
                    (warn "Blank line in genome tbl file. Line ~S" (+ count 2)))
                ;; Create a frame by that name
                (let ((contig-frame 
                       (make-appropriate-contig-frame 
                        (one-string (slotv orgf #$organism-prefix) name) t)
                       ;; (fff (one-string (slotv orgf #$organism-prefix) name))
                       ))
                      
                  (setf (slotv contig-frame #$Organism-Entity-Type) 
                        #$Contiguous-Sequence)
                  ;; Add to list of contiguous sequence frames for the organism
                  (push contig-frame contigs)
                  ;; Make the contig frame point back to the organism
                  (setf (slotv contig-frame #$Organism) orgf)
                  (property-list-to-frame-slots contig-frame properties)
                  (when load-descriptions? 
                    (setf (slotv contig-frame #$Description) description))))
              )
              

        (setf contigs (nreverse contigs))
        (organism-fastas-to-index-and-seq-files
         orgf orgn dirname (list fasta-file) tbl-file :rebuild? rebuild?
         :data-char-predicate 
         (let ((pred (#^genome-char-predicate orgf))) 
           (or (and pred (intern (string pred) :bio))
               'allowable-genome-char-finished?)))
        (let* ((index-data (read-organism-seqidx-file orgf dirname))
               (sequenced-contig-names (mapcar 'first index-data))
               (mismatches nil))
          
          ;; Make sure the contig names which came from the Fasta file
          ;; match the ones that are defined in the .tbl file.  
          (unless (= (length contigs) (length index-data))
            (error 
             (one-string
              "The number of contiguous sequences in the genome.fasta file"
              " is not the same as the number named in the genome.tbl file.")))
          (loop for contig in contigs do
                (unless (find (fname contig) sequenced-contig-names 
                              :test 'string-equal)
                  (push contig mismatches))
                )
          (limited-errors-reported 
           (find-duplicates contigs)
           10 "And more duplicates..." 
           (lambda (x) 
             (cformatt "Duplicate contig ~A found in genome .tbl file"  x)))
          (limited-errors-reported 
           mismatches
           10 "And more mismatches..." 
           (lambda (x) 
             (cformatt
              "Contig named ~A found in .tbl file but not in .fasta file" x)))
          (when mismatches 
            ;; CHANGED ERROR MESSAGE 
            (error "Fatal error processing genome ~A.  Cannot continue." orgf))
        
          (store-internal-sequence-info-and-seqlen index-data)
          
          (let ((seqfile (seqinfo-seq-file orgn dirname)))
            (set-genome-sequence-file orgf seqfile)
            (set-genome-sequence-stream 
             orgf
             (if *keep-sequence-streams-open*
                 (open seqfile :direction :input :if-does-not-exist :error)
               nil
               )))
    
          (length (setf (slotv orgf #$contiguous-sequences) contigs))
          
          )))))

(defun property-list-to-frame-slots (frame property-list)
  (loop for (property value) in property-list do
        (unless (empty-value? value)
          (setf (slotv frame (fff (symbol-name property))) value))))

(defun empty-value? (value)
  (and (stringp value) 
       (or (string= value "")
           (string= value "-")
           )))

(defun load-the-genes-into-frames
       (orgf load-descriptions? &aux (dirname "genes"))
  (setf (slotv orgf #$Genes) nil)
  (let* ((tbl-file (organism-tbl-file orgf dirname))
         (tbl-data (retrieve-entire-table tbl-file))
         (headers (pop tbl-data)) 
         (contigs (slotv orgf #$Contiguous-Sequences))
         (genes nil)
         (nameref (position "name" headers :test 'string-equal))
         (propref (position "properties" headers :test 'string-equal))
         (descref (position "description" headers :test 'string-equal))
         (contigref (position "genome-component" headers :test 'string-equal))
         (dirref (position "direction" headers :test 'string-equal))
         (fromref (position "from" headers :test 'string-equal))
         (toref (position "to" headers :test 'string-equal))
         (refs (remove nil (list nameref propref descref contigref
                                 dirref fromref toref)))
         (newheaderlist (mapcar 'keywordize (index-remover refs headers)))
         (org-prefix (slotv orgf #$organism-prefix)))

    (unless nameref (error "No NAME column in organism ~A genes.tbl file" orgf))
    (unless contigref 
      (error "No GENOME-COMPONENT column in organism ~A genes.tbl file" orgf))
    (unless dirref 
      (error "No DIRECTION column in organism ~A genes.tbl file" orgf))    
    (unless toref (error "No TO column in organism ~A genes.tbl file" orgf))
    (unless fromref (error "No FROM column in organism ~A genes.tbl file" orgf))

    (flet ((all-else-to-properties (lst)
             (let* ((newlst (index-remover refs lst))
                    (assoclist (mapcar 'list newheaderlist newlst)))
               (list (nth nameref lst)
                     (nth contigref lst)
                     (nth dirref lst)
                     (nth fromref lst)
                     (nth toref lst)
                     (append 
                      (when propref 
                        (let ((val (nth propref lst)))
                          (when (not (zerop (length val)))
                            (organism-property-string-to-key-value-list val))))
                      assoclist)
                     (when descref (nth descref lst)))))
           )

      (let ((new-tbl-data (mapcar #'all-else-to-properties tbl-data))
            (dir-errors nil)
            (from-errors nil)
            (to-errors nil))
        (setq 
         new-tbl-data 
         (loop for data-record in new-tbl-data
               for count fixnum from 0 
               as blank? = nil 
               do
               (destructuring-bind 
                   (name contig direction from to properties description)
                   data-record
                 (declare (ignore properties description))
                 (if (zerop (length name))
                     (progn 
                       (when *warn-about-blank-lines*
                         (warn "Blank line in genes tbl file.  Line ~S"
                               (+ 2 count)))
                       (setq blank? t))
                   (let ((contig-frame 
                          (frame-fnamed (one-string org-prefix contig))))
                     (unless contig-frame
                       (error 
                        (one-string
                         ;; CHANGED ERROR MESSAGE
                         "Gene ~A in organism ~A " 
                         "is said to belong to a contiguous sequence "
                         "named ~A which in fact does not exist.")
                        name orgf contig))
                     (unless (find contig-frame contigs)
                       (error 
                        (one-string
                         "Ruh roh. Gene ~A belongs to contiguous segment named "
                         "~A which is not present in the organism.")
                        name contig
                        ))
                     (setf (second data-record) contig-frame)
                     (progn
                       (let ((dirr (string-upcase direction))
                             (contig-length 
                              (slotv contig-frame #$sequence-length)))
                         (handler-case 
                             (let ((num (parse-integer from)))
                               (if  (or (and (>= num 1) (<= num contig-length))
                                        (= num -1))
                                   (setf (fourth data-record)
                                         num)
                                 (push (list name from "Not within range")
                                       from-errors)))
                           (error () (push (list name from "Not an integer") 
                                           from-errors)))

                         (if (or (stringp (fourth data-record))
                                 (= (fourth data-record) -1))
                             (progn
                               (setf (fifth data-record) -1)
                               (setf (fourth data-record) -1)
                               (setf (third data-record) nil))
                           (progn 
                             (if (or (string= dirr "F") (string= dirr "B"))
                                 (setf (third data-record) (keywordize dirr))
                               (push (list name direction) dir-errors))
                             (handler-case 
                                 (let ((num (parse-integer to)))
                                   (if  (and (>= num 1) (<= num contig-length))
                                       (setf (fifth data-record)
                                             (parse-integer to))
                                     (push (list name to "Not within range")
                                           to-errors)))
                               (error () (push (list name to "Not an integer") 
                                               to-errors)))))
                         )))))
               when (not blank?) collect data-record
               ))

        (loop for (name contig-frame direction from to properties description) 
              in new-tbl-data
              for count fixnum from 0 do
              (let ((gene-frame 
                     (make-appropriate-gene-frame 
                      (one-string org-prefix name) t)
                     ;; (fff (one-string org-prefix name))
                     )) 
                (setf (slotv gene-frame #$Organism-Entity-Type) #$Gene)
                (setf (slotv gene-frame #$Organism) orgf)
                (setf (slotv gene-frame #$Contiguous-Sequence) contig-frame)
                (property-list-to-frame-slots gene-frame properties)
                (setf (slotv gene-frame #$Direction) direction)
                (setf (slotv gene-frame #$From) from)
                (setf (slotv gene-frame #$To) to)
                (when load-descriptions?
                  (setf (slotv gene-frame #$Description) description))
                (push gene-frame genes)
                (when (zerop (mod count 1000)) (format t "."))))
              
        (limited-errors-reported 
         (find-duplicates genes)
         10 "And more duplicates..." 
         (lambda (x) 
           (cformatt "*** Duplicate gene ~A found in .tbl file" x)))
        
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
          ;; CHANGED ERROR MESSAGE
          (error "Fatal error(s) in genes.tbl of organism ~A. Cannot continue."
                 orgf))


        (length (setf (slotv orgf #$Genes) (nreverse genes)))

        ))))


(defun load-the-proteins-into-frames 
       (orgf load-descriptions? 
             &key (rebuild? nil) &aux (dirname "proteins"))
     
  (setf (slotv orgf #$Proteins) nil)
  ;; Get all the relevant data out of the sequence and information
  ;; tables for the proteins for this organism.
  (let* ((orgn (fname orgf))
         (tbl-file (organism-tbl-file orgf dirname))
         (fasta-file (organism-fasta-file orgf dirname))
         (tbl-data (retrieve-entire-table tbl-file))
         (headers (pop tbl-data)) 
         (nameref (position "name" headers :test 'string-equal))
         (generef (position "gene" headers :test 'string-equal))
         (propref (position "properties" headers :test 'string-equal))
         (descref (position "description" headers :test 'string-equal))
         (refs (remove nil (list nameref generef propref descref)))
         (newheaderlist (mapcar 'keywordize (index-remover refs headers)))
                  
         ;; *** We need to do the verification of the names in the index file
         ;; that was previously done by the call below ***

         ;; Goes through the proteins and creates corresponding index
         ;; and seq files if necessary
         (dummy (organism-fastas-to-index-and-seq-files 
                 orgf orgn dirname (list fasta-file) tbl-file 
                 :rebuild? rebuild? 
                 :data-char-predicate 
                 (let ((pred (#^protein-char-predicate orgf))) 
                   (or (and pred (intern (string pred) :bio))
                       'allowable-protein-char-finished?))))
         (index-data (read-organism-seqidx-file orgf dirname))
         (sequenced-protein-names (mapcar 'first index-data))
         ;; Initialize various.
         (mismatches nil)
         (missing-gene-list nil)
         (genes (slotv orgf #$Genes)) 
         (proteins-from-fasta-file nil)
         (tbl-proteins-in-order nil)
         (genes-ht (make-hash-table :test #'eq))
         (fasta-proteins-ht (make-hash-table :test #'eq))
         )
    (declare (ignore dummy))

    (unless nameref 
      (error "No NAME column in organism ~A proteins.tbl file" orgf))
    (unless generef 
      (error "No GENE column in organism ~A proteins.tbl file" orgf))
    
    (flet ((all-else-to-properties (lst)
             (let* ((newlst (index-remover refs lst))
                    (assoclist (mapcar 'list newheaderlist newlst)))
               (list (nth nameref lst)
                     (nth generef lst)
                     (append 
                      (when propref 
                        (let ((val (nth propref lst)))
                          (when (not (zerop (length val)))
                            (organism-property-string-to-key-value-list val))))
                      assoclist)
                     (when descref (nth descref lst))))))
      (let ((new-tbl-data (mapcar #'all-else-to-properties tbl-data)))

        ;; Put all the genes of the organism in a hash table.
        (loop for gene in genes do (setf (gethash gene genes-ht) gene))
      
        ;; Process each protein in the protein sequences table,
        ;; creating a frame for it and putting these protein frames in a 
        ;; special hash table.
        (loop for name in sequenced-protein-names
              for count fixnum from 0
              do
              (let ((protein-frame 
                     (make-appropriate-protein-frame name t)
                     ;; (fff name)
                     ))
                (push protein-frame proteins-from-fasta-file)
                (setf (gethash protein-frame fasta-proteins-ht) t)
                (setf (slotv protein-frame #$Organism-Entity-Type) #$Protein)
                (setf (slotv protein-frame #$Organism) orgf)
                (when (zerop (mod count 1000)) (format t "."))
                ))
        (terpri)
        
        ;; Loop for each protein in the protein information table.
        ;; There may be proteins here which don't have sequence entries.

        (loop for (name gene properties description) in new-tbl-data 
              for count fixnum from 0 
              as problem? = nil 
              do
              (if (zerop (length name))
                  (when *warn-about-blank-lines*
                    (warn "Blank line in proteins tbl file.  Line ~S" 
                          (+ count 2)))
                (let* ((tbl-protein-frame 
                        (fff (one-string (slotv orgf #$organism-prefix) name))) 
                       (gene-frame 
                        (fff (one-string (slotv orgf #$organism-prefix) gene))))
                  ;; Each protein must point back to a gene in the organism.
                  (unless (gethash gene-frame genes-ht)
                    (setq problem? t)
                    (push (list tbl-protein-frame gene-frame) 
                          missing-gene-list))
                  (unless (gethash tbl-protein-frame fasta-proteins-ht)
                    (setq problem? t)
                    (push tbl-protein-frame mismatches))
                  (unless problem? 
                    (setf (slotv tbl-protein-frame #$Organism-Entity-Type) 
                          #$Protein)
                    (setf (slotv tbl-protein-frame #$Gene) gene-frame)
                    ;; Make the gene pointed back to by this protein point at it
                    (pushnew tbl-protein-frame (slotv gene-frame #$Proteins))
                    ;; Deal with properties of the protein from the info table.
                    (property-list-to-frame-slots tbl-protein-frame properties)
                    (when (and load-descriptions? description)
                      (setf (slotv tbl-protein-frame #$Description) 
                            description))
                    ;; Make the protein point back to its organism.
                    (setf (slotv tbl-protein-frame #$Organism) orgf)
                    (push tbl-protein-frame tbl-proteins-in-order)
                    (when (zerop (mod count 1000)) (format t "."))
                    ))))
        (terpri)

        (limited-errors-reported 
         (find-duplicates proteins-from-fasta-file)
         10 "And more duplicates..." 
         (lambda (x) 
           (cformatt "*** Duplicate protein ~A found in .fasta file" x))) 

        (limited-errors-reported
         (find-duplicates tbl-proteins-in-order)
         10 "And more duplicates..."
         (lambda (x) 
           (cformatt "*** Duplicate protein ~A found in .tbl file" x)))
  
        (limited-errors-reported
         mismatches 10 "And more mismatches..."
         (lambda (x) 
           (cformatt 
            "*** Mismatch: Protein ~A in .tbl file but not in .fasta file"
            x)))

        (limited-errors-reported
         missing-gene-list
         10 "And more unknown genes..."
         (lambda (x) 
           (let ((protein (car x))
                 (gene (cadr x)))
             (cformatt 
              (one-string
               "Gene named ~S is associated with protein ~A in the .tbl file "
               "but that gene does not exist in the organism.")
              (fstring gene) protein))))
        (when missing-gene-list 
          ;; CHANGED ERROR MESSAGE
          (error 
           "Fatal error in processing proteins of organism ~A. Can't continue."
           orgf))
        
        (store-internal-sequence-info-and-seqlen index-data)

        (let ((seqfile (seqinfo-seq-file orgn dirname)))
          (set-protein-sequence-file orgf seqfile))
               
        (length (setf (slotv orgf #$proteins) (nreverse tbl-proteins-in-order)))
      
        ))))
             


(defun limited-errors-reported (list limit msg body-function)
  (loop for element in list 
        for count from 0 below limit
        do
        (funcall body-function element)
        finally
        (when (> (length list) limit)
          (cformatt (formatn "~A. (~D total)"  msg (length list)))
          )))

(defun pprint-organism (orgf &optional (pprint-limit 10))

  #.(one-string-nl
     "Pretty prints information about an organism.  The information "
     "is obtained from the organism's frame.  PPRINT-LIMIT genes, transcripts "
     "and proteins are shown (if they exist).")

  (flet ((n-of (slot) (length (slotv orgf slot)))
         (set-of (slot) (slotv orgf slot))
         (augment (slot-list) 
           (append 
            (list #$Fname #$Description #$Organism)
            slot-list)))

    (let ((contigs (set-of #$Contiguous-Sequences))
          (genes (set-of #$Genes))
          (transcripts (set-of #$Transcripts))
          (proteins (set-of #$Proteins))
          (ncontigs (n-of #$Contiguous-Sequences))
          (ngenes (n-of #$Genes))
          (ntranscripts (n-of #$Transcripts))
          (nproteins (n-of #$Proteins))
          (known-organism-slots 
           (list #$Contiguous-Sequences #$Genes 
                 #$Transcripts #$Proteins #$Date #$Version))
          (known-contig-slots (list #$Circular))
          (known-gene-slots 
           (list #$Contiguous-Sequence #$From #$To #$Direction))
          (known-transcript-slots
           (list #$Contiguous-Sequence #$From #$To #$Direction))
          (known-protein-slots nil)
          )

      (dotimes (j 4) (terpri))

      (cformatt "DATA FOR ORGANISM ~A" (slotv orgf #$Fname))
      (terpri)
      (cformatt "  DATE UPLOADED: ~A" (slotv orgf #$Date))
      (cformatt "  VERSION TAG: ~S" (slotv orgf #$Version))
      (cformatt "  DESCRIPTIONS LOADED? ~A" 
                (slotv orgf #$Organism-Descriptions-Loaded))
      (cformatt "  NUMBER OF CONTIGUOUS SEQUENCES: ~D" ncontigs)
      (cformatt "  NUMBER OF GENES: ~D" ngenes)
      (cformatt "  NUMBER OF TRANSCRIPTS: ~D" ntranscripts)
      (cformatt "  NUMBER OF PROTEINS: ~D" nproteins)
      (terpri)

      (cformatt "  CONTIGUOUS SEQUENCES:")
      (loop for contig in contigs for j below pprint-limit do
            (cformatt 
             "    ~A, CIRCULAR: ~A, SEQLEN: ~D" contig
             (slotv contig #$Circular)
             (slotv contig #$sequence-Length)))
      (let ((other-contig-slots
             (set-difference 
              (all-slots-of contigs) (augment known-contig-slots))))
        (when other-contig-slots
          (cformatt "  OTHER FRAME SLOT NAMES FOUND IN THESE CONTIGS:")
          (loop for slot in other-contig-slots do (cformatt "    ~A" slot))
          ))
      (terpri)

      (when genes
        (cformatt "  GENES:")
        (loop for gene in genes for j below pprint-limit do
              (cformatt "    ~A, CONTIG: ~A, FROM: ~D, TO: ~D, DIRECTION: ~A"
                        gene 
                        (slotv gene #$Contiguous-Sequence)
                        (slotv gene #$From)
                        (slotv gene #$To)
                        (slotv gene #$Direction)
                        ))
        (let ((other-gene-slots 
               (set-difference 
                (all-slots-of genes) (augment known-gene-slots))))
          (when other-gene-slots
            (cformatt "  OTHER FRAME SLOT NAMES FOUND IN THESE GENES: ")
            (loop for slot in other-gene-slots do (cformatt "    ~A" slot))))
        (terpri))
      
      (when transcripts
        (cformatt "  TRANSCRIPTS:")
        (loop for transcript in transcripts for j below pprint-limit do
              (cformatt "    ~A, CONTIG: ~A, FROM: ~D, TO: ~D, DIRECTION: ~A"
                        transcript 
                        (slotv transcript #$Contiguous-Sequence)
                        (slotv transcript #$From)
                        (slotv transcript #$To)
                        (slotv transcript #$Direction)
                        ))
        (let ((other-transcript-slots 
               (set-difference 
                (all-slots-of transcripts) (augment known-transcript-slots))))
          (when other-transcript-slots
            (cformatt "  OTHER FRAME SLOT NAMES FOUND IN THESE TRANSCRIPTS: ")
            (loop for slot in other-transcript-slots do 
                  (cformatt "    ~A" slot))))
        (terpri))

      (when proteins
        (cformatt "  PROTEINS:")
        (loop for protein in proteins for j below pprint-limit do
              (cformatt 
               "    ~A, GENE: ~A, SEQLEN: ~D" protein
               (slotv protein #$Gene)
               (slotv protein #$Sequence-Length)))
        (let ((other-protein-slots 
               (set-difference 
                (all-slots-of proteins) 
                (cons #$Gene (augment known-protein-slots)))))
          (when other-protein-slots
            (cformatt "  OTHER FRAME SLOT NAMES FOUND IN THESE PROTEINS: ")
            (loop for slot in other-protein-slots do (cformatt "    ~A" slot))
            ))
        (terpri))

      (let ((other-organism-properties
             (set-difference 
              (all-slots-of (list orgf)) (augment known-organism-slots))))
        (when other-organism-properties
          (cformatt "  OTHER PROPERTIES OF THE ORGANISM:")
          (loop for prop in other-organism-properties do
                (let ((value (slotv orgf prop)))
                  (cformatt 
                   "    ~A: ~S" (slotv prop #$Fname) (limited-string value))
                  ))
          (terpri)
          ))

      (let ((od (slotv orgf #$Description)))
        (when od 
          (cformatt "  ORGANISM DESCRIPTION: ~A" (limited-string od 200))
          (terpri)
          ))

      (terpri)

      )))

(defun organism-seqidx-info (orgn prefix)
  (let ((file (seqinfo-index-file orgn prefix))
        (index-info nil))
    (unless (probe-file file)
      (error 
       "No .SEQIDX file exists for organism ~A!!  It should exist as ~A" 
       orgn file))
    (cformatt "Accessing seqidx file ~A" file)
    (with-open-file (p file) 
      (with-standard-io-syntax (setq index-info (read p nil nil))))
    (when (null index-info)
      (error
       "No index information for organism ~A found in file ~A!!" 
       orgn file))
    index-info
    ))

(defun read-organism-seqidx-file (orgf prefix)
  (let* ((orgn (string-downcase (slotv orgf #$Fname)))
         (index-table (organism-seqidx-info orgn prefix)))
    index-table
    ))

(defun store-internal-sequence-info-and-seqlen (index-table)
  (loop for (name start-header start-data seqlen) in index-table do
        (let ((frame (frame-fnamed name)))
          (unless frame (error "Frame named ~S does not exist!" name))
          (setf (slotv frame #$Internal-sequence-info)
                (list start-header (- start-data start-header) start-data))
          (setf (slotv frame #$Sequence-Length) seqlen)
          )))

(defun retrieve-entire-table (file-name)
 ;(let* ((stringed-file (file-to-string file-name :max  10000000))
 ;(let* ((stringed-file (file-to-string file-name :max  20000000))
  (let* ((stringed-file (file-to-string file-name :max 100000000))
         (line-list (simple-string-split stringed-file #\Newline)))
    (mapcar (lambda (line) (simple-string-split line #\Tab)) line-list)))

(defun index-remover (index-list lst)
  (loop for x in lst
        for j fixnum from 0
        when (not (member j index-list))
        collect
        x))
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun organism-fastas-to-index-and-seq-files 
       (orgf orgn prefix fasta-files tblfilepath 
             &key (rebuild? nil) (data-char-predicate 'identity))
  ;; Turn the fasta files for the contig or proteins of an organisms
  ;; into a single long sequence file with an associated index file.
  (unless (#^organism-prefix orgf) (error "No organism prefix!!"))
  (let ((seqdir (organism-seqinfo-dir orgf))
        (seqfile-name (one-string prefix "-" *seqinfo-name*)))
    (handler-case
        (ensure-directories-exist seqdir :verbose t)
      (error
       ()
       (formatn
        (one-string
         "Cannot access or cannot create the directory to store "
         "sequence information for organism ~A. "
         "The inaccessible directory is ~A. ")
        orgn seqdir
        )))
    (let ((seqpath 
           (merge-pathnames 
            (make-pathname :name seqfile-name :type *seqinfo-ext*)
            seqdir)))
      (when (or rebuild?
                (or (not (probe-file seqpath))
                    (> (file-write-date tblfilepath) 
                       (file-write-date seqpath))))
        (cformatt "Creating seq files in directory ~A from .fasta file" seqdir)
        ;; Create sequence.seqidx and sequence.seq files
        (fasta-files-to-index-and-seq-files 
         fasta-files seqdir seqfile-name 
         (#^organism-prefix orgf) :verbose? t 
         :data-char-predicate data-char-predicate)
        (cformatt "FASTA data stored into SEQ files in directory ~A" seqdir)
        ))))


(defun fasta-files-to-index-and-seq-files 

       (fasta-files result-dir result-file-name prefix 
                    &key (verbose? t) (data-char-predicate 'identity))

  (ensure-directories-exist result-dir :verbose t)

  (let*  ((fasta-paths (mapcar #'merge-pathnames fasta-files))
          (index-file 
           (merge-pathnames 
            (make-pathname :name result-file-name :type *seqinfo-index-ext*)
            result-dir))
          (seq-file-path
           (merge-pathnames 
            (make-pathname :name result-file-name :type *seqinfo-ext*)
            result-dir))
          (index-list nil)
          (start-header 0)
          (start-data 0)
          (found-non-blank-line? nil)
          (reading-data? nil)
          (file-header-count 0)
          (linecount 0)
          (charcount 0)
          (key nil)
          )

    (when verbose?
      (cformatt "Creating sequence file ~A" (namestring seq-file-path))
      (cformatt "Creating index file ~A" (namestring index-file))
      (cformatt "Using fasta files: ")
      (dolist (f fasta-paths) (cformatt "  ~A" (namestring f))))
                
    (flet ((add-entry ()
             (push 
              (list (one-string prefix key)
                    start-header start-data (- charcount start-data))
              index-list)))

      ;; Open the output sequence file

      (with-open-file 
          (seqp seq-file-path :direction :output :if-exists :supersede)

        ;; For each input file...

        (dolist (fasta-file fasta-paths)

          ;; Get rid of any %$#^&** RETURN/NEWLINE sequences

          (strip-file-of-returns-preceding-newlines fasta-file)

          ;; Open it

          (when verbose? 
            (cformatt "Processing fasta file ~A" (namestring fasta-file)))

          (with-open-file (fastap fasta-file :direction :input)

            ;; Set up state for this file.

            (setq reading-data? nil)
            (setq found-non-blank-line? nil)
            (setq file-header-count 0)
            (setq linecount 0)

            ;; Read successive lines.

            (do ((line (read-line fastap nil nil) (read-line fastap nil nil)))
                ((null line))

              (incf linecount)

              ;; Skip over blank lines.  Warn if blank lines found
              ;; in file.

              (if (every 
                   #'(lambda (ch) (or (eql ch #\Space) (eql ch #\Tab))) 
                   line)

                  (when found-non-blank-line?
                    (warn 
                     (one-string
                      "File ~A.  Line ~D~% "
                      "Blank line found in fasta file.")
                     (namestring fasta-file) linecount))

                ;; Process a non-blank line.

                (progn
                  (setq found-non-blank-line? t)
                  (write-sequence line seqp)
                  (cond
                   ((eql (char line 0) #\>)
                    (when (and (plusp file-header-count) (not reading-data?))
                      (warn
                       "File ~A.  Line ~D~%No data found after header: ~S"
                       (namestring fasta-file) linecount key))
                    (when (plusp file-header-count) (add-entry))
                    (incf file-header-count)
                    (setq reading-data? nil)
                    (setq key (get-contig-fasta-key line))
                    (setq start-header (1+ charcount))
                    (setq start-data (+ charcount (length line))))
                   (t 
                    (setq reading-data? t)
                    (unless (every data-char-predicate line)
                      (let ((*print-length* 300))
                        (error 
                         (one-string
                          "File ~A, Line ~D, ~S~%"
                          "Character ~S at position ~D does not satisfy ~A")
                         (namestring fasta-file) linecount 
                         (map 'list 'identity line) 
                         (aref line (position-if-not data-char-predicate line))
                         (position-if-not data-char-predicate line)
                         data-char-predicate
                         )))))
                  (incf charcount (length line))
                  )))

        ;; Add the last record.

        (unless reading-data? (error "No data found after last header."))
        (add-entry)

        )

      ;; End processing of this file
      ;; Process next file.

      (cformatt "~D records processed" file-header-count)

      )

  ;; All done.  Output sequence file has been closed.

  (when verbose? 
    (cformatt "All fasta files processed.  Checking for duplicate keys."))

  (setq index-list (nreverse index-list))
  (let ((duplicates
         (check-for-duplicates 
          index-list :key 'first :test 'string-equal)))
    (when duplicates
      (error "*** Ruh roh.  Duplicate keys found: ~S" duplicates)))
        
  ;; Open the output index file and write out the index info.

  (when verbose?
    (cformatt 
     "Writing index data (~D entries) to index file ~A" 
     (length index-list) (namestring index-file)))

  (with-open-file (p index-file :direction :output :if-exists :supersede)
    (with-standard-io-syntax (format p "~S" index-list)))

  (when verbose? (cformatt "Processing complete."))

  index-list

  ))))

(defun seqinfo-seq-file (orgn prefix)
  (merge-pathnames 
   (make-pathname :name (one-string prefix "-" *seqinfo-name*) 
                  :type *seqinfo-ext*)
   (organism-seqinfo-dir (frame-fnamed orgn))))

(defun seqinfo-index-file (orgn prefix)
  (merge-pathnames 
   (make-pathname :name (one-string prefix "-" *seqinfo-name*)
                  :type *seqinfo-index-ext*)
   (organism-seqinfo-dir (frame-fnamed orgn))))

(defun get-contig-fasta-key (line)
  (let ((pos (position-if
              #'(lambda (x) (or (eql x #\Space) (eql x #\Tab) (eql x #\,)))
              line)))
    (if pos (subseq line 1 pos) (subseq line 1))))

(defun allowable-genome-char-strict? (ch)
  #.(one-string-nl
     "Allowable alphabetic genome chars are ACGTN")
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (let ((uch (char-upcase ch)))
        (or (eql uch #\A)
            (eql uch #\C)
            (eql uch #\G)
            (eql uch #\T)
            )))

(defun allowable-genome-char-finished? (ch)
  #.(one-string-nl
     "Allowable alphabetic genome chars are ACGTN")
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (let ((uch (char-upcase ch)))
        (or (eql uch #\A)
            (eql uch #\C)
            (eql uch #\G)
            (eql uch #\T)
            (eql uch #\N))))

(defun allowable-genome-char-unfinished? (ch)
  #.(one-string-nl
     "Allowable alphabetic genome chars are ABCDGHKMNRSTVWY")
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (let ((uch (char-upcase ch)))
    (or (eql uch #\A) ;; Adenine
        (eql uch #\C) ;; Cytosine
        (eql uch #\G) ;; Guanine
        (eql uch #\T) ;; Thymine
        (eql uch #\N) ;; aNything
        (eql uch #\B) ;; C or G or T
        (eql uch #\D) ;; A or G or T
        (eql uch #\H) ;; A or C or T
        (eql uch #\K) ;; G or T  (Keto)
        (eql uch #\M) ;; A or C  (aMino)
        (eql uch #\R) ;; A or G  (pUrine)
        (eql uch #\S) ;; G or C  (Strong)
        (eql uch #\V) ;; A or C or G
        (eql uch #\W) ;; A or T  (Weak)
        (eql uch #\Y) ;; C or T  (pYrimidine)
        )))

(defun allowable-genome-char-brucei? (ch)
  #.(one-string-nl
     "Allowable alphabetic genome chars are ABCDGHKMNRSTVWY")
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (let ((uch (char-upcase ch)))
    (or (eql uch #\A) ;; Adenine
        (eql uch #\C) ;; Cytosine
        (eql uch #\G) ;; Guanine
        (eql uch #\T) ;; Thymine
        (eql uch #\N) ;; aNything
        (eql uch #\B) ;; C or G or T
        (eql uch #\D) ;; A or G or T
        (eql uch #\H) ;; A or C or T
        (eql uch #\K) ;; G or T  (Keto)
        (eql uch #\M) ;; A or C  (aMino)
        (eql uch #\R) ;; A or G  (pUrine)
        (eql uch #\S) ;; G or C  (Strong)
        (eql uch #\V) ;; A or C or G
        (eql uch #\W) ;; A or T  (Weak)
        (eql uch #\Y) ;; C or T  (pYrimidine)
        (eql uch #\X) ;; presumably synonymous with N
        )))



(defun allowable-protein-char-finished? (ch)
  #.(one-string-nl 
     "Allowable alphabetic protein sequence chars are"
     "ACDEFGHIKLMNPQRSTVWY*-") 
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (or (and (alpha-char-p ch)
           (let ((uch (char-upcase ch)))
             (and (not (eql uch #\B))
                  (not (eql uch #\J))
                  (not (eql uch #\O))
                  (not (eql uch #\U))
                  (not (eql uch #\X))
                  (not (eql uch #\Z)))))
      (eql ch #\*)
      (eql ch #\-)))

(defun allowable-protein-char-arab? (ch)
   (allowable-protein-char-unfinished? ch))

(defun allowable-protein-char-unfinished? (ch)
  #.(one-string-nl 
     "Allowable alphabetic protein sequence chars are"
     "ACDEFGHIKLMNPQRSTVWXY*- (adds an X)") 
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (or (and (alpha-char-p ch)
           (let ((uch (char-upcase ch)))
             (and (not (eql uch #\B))
                  (not (eql uch #\J))
                  (not (eql uch #\O))
                  (not (eql uch #\U))
                  ;; (not (eql uch #\X))
                  (not (eql uch #\Z)))))
      (eql ch #\*)
      (eql ch #\-)))

(defun allowable-protein-char-all-letters? (ch)
  #.(one-string-nl 
     "Allowable alphabetic protein sequence chars are"
     "ACDEFGHIKLMNPQRSTVWXY*- (adds an X)") 
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (or (alpha-char-p ch)
      (eql ch #\*)
      (eql ch #\-)))

(defun allowable-seed-protein-char? (ch)
  #.(one-string-nl 
     "Allowable alphabetic protein sequence chars are"
     "ACDEFGHIKLMNPQRSTVWXY*- (adds an X)") 
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (or (and (alpha-char-p ch)
           (let ((uch (char-upcase ch)))
             (and 
              ;; (not (eql uch #\B))
              (not (eql uch #\J))
              (not (eql uch #\O))
              ;; (not (eql uch #\Z))
              )))
      (eql ch #\*)
      (eql ch #\-)))

(defun allowable-protein-char-strict? (ch)
  #.(one-string-nl 
     "Allowable alphabetic protein sequence chars are"
     "ACDEFGHIKLMNPQRSTVWY* (no - allowed)") 
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (or (and (alpha-char-p ch)
           (let ((uch (char-upcase ch)))
             (and (not (eql uch #\B))
                  (not (eql uch #\J))
                  (not (eql uch #\O))
                  (not (eql uch #\U))
                  (not (eql uch #\X))
                  (not (eql uch #\Z)))))
      (eql ch #\*)
      ))



;;; These functions are defined also in the old load-organism.lisp
;;; The extract-* routines have been modified to call these functions
;;; instead of accessing slots directly so that the extract-* routines
;;; will now work using either the load-organism.lisp code or the 
;;; ms-load-organism.lisp code

(defun internal-sequence-data-start (frame)
  (third (slotv frame #$Internal-sequence-info)))

(defun genome-sequence-stream (orgf)
  (slotv orgf #$Genome-sequence-stream))

;; make sure any old stream is closed
(defun set-genome-sequence-stream (orgf stream)
  (let ((s (#^genome-sequence-stream orgf)))
    (when (streamp s)
      (when (open-stream-p s)
        (close s)))
    (setf (slotv orgf #$Genome-sequence-stream) stream)))

(defun genome-sequence-file (orgf)
  (slotv orgf #$Genome-sequence-file))

(defun set-genome-sequence-file (orgf file)
  (setf (slotv orgf #$Genome-sequence-file) (namestring file)))

(defun protein-sequence-file (orgf)
  (slotv orgf #$protein-sequence-file))

(defun set-protein-sequence-file (orgf file)
  (setf (slotv orgf #$protein-sequence-file) (namestring file)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod purge-organism 
           ((org t) &rest args 
            &key (delete-frame? nil) (verbose? t)
            &allow-other-keys)
  (unless (isframe? org)
    (error "Cannot purge ~A.  It is not an organism frame." org))
  (unless (#^isa org) 
    (error "Cannot purge ~A. it is not an organism frame." org))
  (when verbose? 
    (cformatt "Deleting organism ~A's contigs, genes, and proteins." org))
  (loop for c in (#^contiguous-sequences org) do (unintern-frame c))
  (setf (#^contiguous-sequences org) nil)
  (loop for g in (#^genes org) do (unintern-frame g))
  (setf (#^genes org) nil)
  (loop for p in (#^proteins org) do (unintern-frame p))
  (setf (#^proteins org) nil)
  (setf (#^non-coding-genes org) nil)
  (setq *loaded-organisms* (delete org *loaded-organisms*))
  (setf (#^organism-loaded? org) nil)
  (let ((name (#^fname org)))
    (when delete-frame? 
      (when verbose? (cformatt "Deleting organism frame!"))
      (unintern-frame org)
      )
    (when verbose? (cformatt "Organism ~A purged!" name))
    )
  (if delete-frame? t org)
  )
  








        
