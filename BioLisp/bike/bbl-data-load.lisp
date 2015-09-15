;;;; -*- mode: Lisp; Syntax: Common-Lisp; Package: bbi; -*-
(IN-PACKAGE :BBI)

; ******* GLOBAL DYNAMIC LISTS *******

(DEFPARAMETER *microarray-tables* NIL)
(DEFPARAMETER *digesters* NIL)
(DEFPARAMETER *hydrophobicity-hash* (MAKE-HASH-TABLE :TEST 'EQUAL))
(DEFPARAMETER *active-microarray-tables* NIL)
(DEFPARAMETER *bacterial-codons* NIL)
(DEFPARAMETER *viral-dinucleotides* NIL)

(DEFVAR *ortholog-table* NIL)
(DEFVAR *rna-ortholog-table* NIL)
(defvar *flat-ortholog-index-hash* nil)

(defvar *flat-ortholog-list-file* "flat-ortholog-list.lisp")
(defvar *flat-ortholog-index-file* "flat-ortholog-index.lisp")
(defvar *flat-ortholog-data-file* "flat-ortholog-data.lisp")

; ************** SERVICE FUNCTIONS **************

(defun bll-data-file (filename) 
  (merge-pathnames filename (cl-user::translate-simple-lp "biol:data;")))

(DEFUN Load-bll-data-file (filename)
  (LET ((filepath (bll-data-file filename)))
    (COND 
     ((PROBE-FILE filepath) (LOAD filepath))
     (T (ERROR "Could not find file ~A!!" filename))
     )))

; *****************************

(defun instantiate-flat-ortholog-table (force?)
  (create-autoloaded-flat-ortholog-table)
  (maybe-generate-new-flat-ortholog-files force?)
  (create-flat-ortholog-index-hash)
  )

(defun create-autoloaded-flat-ortholog-table ()
  (setq *ortholog-table* (make-garray '($ $)))
  (set-garray-name *ortholog-table* "flat-ortholog-table")
  (utils::set-garray-autoload-function
   *ortholog-table* 'flat-ortholog-autoload-function
   ))

(defun maybe-generate-new-flat-ortholog-files (force?)
  (declare (special flat-ortholog-list))
  (let ((original-flat-ortholog-file (bll-data-file *flat-ortholog-list-file*))
        (index-file (bll-data-file *flat-ortholog-index-file*))
        (data-file (bll-data-file *flat-ortholog-data-file*))
        )
    (when (or force? 
              (cl-user::generate-derived-file?
               original-flat-ortholog-file index-file
               ))
      (formatt ";; Reading in ~A ~%" original-flat-ortholog-file)
      (load-bll-data-file *flat-ortholog-list-file*)
      (formatt ";; Generating index and data files for flat ortholog list...")
      (create-index-and-data-file 
       (loop for x in flat-ortholog-list append x)
       index-file 
       data-file
       :key-extractors '(first second)
       :data-extractor 'third
       )
      (setq flat-ortholog-list nil)
      )))

(defun create-flat-ortholog-index-hash ()
  (let ((path (bll-data-file *flat-ortholog-index-file*)))
    (formatt ";; Reading flat ortholog index file ~A~%" path)
    (with-open-file (p path :direction :input)
      (setq *flat-ortholog-index-hash* (make-hash-table :test 'equal))
      (loop as datum = (read p nil nil)
            until (null datum)
            do 
            (setf (gethash 
                   (list (first datum) (second datum))
                   *flat-ortholog-index-hash*)
                  (third datum))
            ))))

(defun flat-ortholog-autoload-function (g &rest keys)
  (let ((filepos (gethash keys *flat-ortholog-index-hash*)))
    (when (null filepos)
      (error "Cannot find entry for ~A in flat-ortholog-hash!" keys))
    (with-open-file (p (bll-data-file *flat-ortholog-data-file*)
                       :direction :input)
      (unless (file-position p filepos)
        (error "Could not go to position ~D in ~A !" 
               filepos *flat-ortholog-data-file*
               ))
      (setf (gref g (first keys) (second keys)) (read p))
      )))

; *****************************

(DEFUN Make-RNA-ortholog-table ()
  (DECLARE (SPECIAL *RNA-ortholog-list*))
  ;; Puts data in file into *rna-ortholog-list*
  (LOAD-BLL-DATA-FILE "RNA-ortholog-list.lisp")
  
  ;; Create a garray with two hash axes whose keys are frames
  ;; (so the test can be EQ)
  (LET ((garray (MAKE-GARRAY '((:HASH EQUAL) (:HASH EQUAL)))))
    (LOOP FOR (organism1 organism2 gene-pairs) IN *RNA-ortholog-list* DO
	  (LOOP FOR (gene1 gene2) IN gene-pairs do
		(SETF (GREF garray gene1 organism2) gene2)
		(SETF (GREF garray gene2 organism1) gene1)
		))
    garray
    ))

; *****************************

(DEFUN Load-ortholog-tables (&key (force? nil))
  (declare (special flat-ortholog-list *rna-ortholog-list*))
  (WHEN (OR force? (NULL *ortholog-table*))
    (LET ((*package* (FIND-PACKAGE :bbi)))
      (LOAD-BLL-DATA-FILE "digester-info.lisp")
      ;; transfer the data from flat-ortholog-list into an autoloaded garray
      (instantiate-flat-ortholog-table force?)
      ;; the list is no longer needed so gc it
      (setq flat-ortholog-list nil)
      ;; transfer the data from *rna-ortholog-list* into a garray
      (ecase user::*frame-system-version*
        (:old (SETQ *rna-ortholog-table* (MAKE-RNA-ORTHOLOG-TABLE)))
        (:sframes nil))
      ;; the list is no longer needed so gc it
      (setq *rna-ortholog-list* nil)
      )))

(load-ortholog-tables)

; *****************************

#|
(DEFUN Read-codon-frequencies (&KEY (genes-at-least 3)(codons-at-least 1000))
  (LET ((raw-counts (LOAD-BBL-DATA-FILE "codon-freqs.txt"))
        (table (MAKE-GARRAY '($ $)))
        (codon-list (ALL-DNA-SEQUENCES OF-LENGTH 3)))
   BODY
     (IF-TRUE all
         THEN (ASSIGN genes-at-least = 0)
              (ASSIGN codons-at-least = 0))

     (FOR-EACH (organism genes-string codons-string freqs-string) IN raw-counts
          AS gene-count = (CONVERT genes-string TO Number)
          AS codon-count = (CONVERT codons-string TO Number)
          AS freqs = (bbi::STRING-TO-LIST freqs-string)
          (IF-TRUE (AND (>= gene-count genes-at-least)
                        (>= codon-count codons-at-least))
              THEN (DEFINE table[organism 'genes] = gene-count)
                   (DEFINE table[organism 'codons] = codon-count)
                   (FOR-EACH codon IN codon-list
                    FOR-EACH freq IN freqs
                        (DEFINE table[organism codon] = freq))))
     table))
|#

; *****************************
#|
(DEFUN Read-dinucleotide-biases ()
   (LET* ((filename "viral-dinucleotide-biases.txt")
          (path (MERGE-PATHNAMES filename 
                   (cl-user::TRANSLATE-SIMPLE-LP "biol:data;")))
          )
    (STRING-TO-LIST (JOIN (BBL::READ FROM path)))))
|#
; ***************** INITIALIZE LISTS ************

(LET ((hydrophobicity-list 
  '((I 4.5)(V 4.2)(L 3.8)(F 2.8)(C 2.5)(M 1.9)(A 1.8)(G -0.4)(T -0.7)
    (W -0.9)(S -0.8)(Y -1.3)(P -1.6)(H -3.2)(E -3.5)(Q -3.5)(D -3.5)
    (N -3.5)(K -3.9)(R -4.5))))
 (LOOP FOR element IN hydrophobicity-list
       AS aa = (SYMBOL-NAME (FIRST element))
       AS KD-score = (SECOND element)
          DO (SETF (GETHASH aa *hydrophobicity-hash*) KD-score)))

;; Load file containing restriction enzymes and proteases
;;   and make hash *digesters*

(SETF *microarray-tables*    ; Temporary fix
      '(("Hihara2001" 
         "BIOL:/data/hihara.txt"
         #$synechocystis_pcc6803
         "Acclimation from low to high light intensity" "link-to-reference" 
         "link-to-raw-data")
        ("Ehira2006"
         "BIOL:/data/ehira2006.txt"
         #$anabaena_pcc7120
         "Response to removal of nitrate" "link-to-reference"
         "link-to-raw-data")))

(SETF *bacterial-codons* 
  NIL) ;  (READ-CODON-FREQUENCIES :genes-at-least 3 :codons-at-least 1000))
(SETF *viral-dinucleotides* NIL)
 ;  (READ-DINUCLEOTIDE-BIASES))
