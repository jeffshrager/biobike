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

(defun seed-query (query &rest args)
  (genericdb::xsql (apply 'utils::format nil query args))
  )

(defun all-gids-in-domain-mysql (domain &key (complete? nil) (restrictions nil))
  (declare (ignore complete? restrictions))
  (seed-query 
   "select genome,gname from genome where maindomain = ~S"
   domain
   ))

(defun get-seed-annotation-info-mysql (gid)
  (let ((annotation-file
         (utils::s+ 
          user::*seed-mysql-data-root* 
          *seed-relative-organisms-path*
          gid 
          "/"
          "annotations"
          )))
    (unless (probe-file annotation-file) 
      (error "Annotation file for organism ~A not found!" gid))
    (let ((lines (utils::file-to-string-list annotation-file)))
      (setq *annotations-hash* (make-hash-table :test 'equalp))
      (let ((records 
             (loop for line in lines 
                   with reclist = nil
                   with reclines = nil
                   do
                   (if (and (>= (length line) 2)
                            (string= "//" (subseq line 0 2)))
                       (progn
                         (push (reverse reclines) reclist)
                         (setq reclines nil)
                         )
                     (push line reclines)
                     )
                   finally
                   (return reclist)
                   )))
        (loop for record in records
              as pegid = (first record)
              do
              (push (cdr record) (gethash pegid *annotations-hash*))
              )))))
           
(defun contig-info-mysql (gid)
  (mapcar 
   'cdr
   (seed-query "select * from contig_lengths where genome = ~S" gid)
   ))

(defun ncbi-contig-info-mysql (gid)
  (seed-query 
   (formatn "select * from ncbi_contig_info where genome = '~A'" gid)
   ))

(defun all-pegs-of-a-genome-mysql (gid)
  (coerce 
   (mapcar 
    'first
    (seed-query 
     "select id from features where genome = ~S && type = \"peg\"" gid)
    )
   'vector
   ))

(defun mysql-get-descriptions-for-pegs-of-genome (pegs)
  (mapcar 
   (lambda (peg) 
     (caar
      (seed-query
       "select assigned_function from assigned_functions where prot = ~S" peg
       )))
   pegs
   ))

;; Get all the assigned functions for all the pegs in one query.
;; If there is a limitation on the size of the query, we may have to
;; rewrite this to retrieve the information in blocks.  
(defun mysql-get-descriptions-for-pegs-of-genome-msf (fids)
  (let* ((in-data (fids-to-mysql-in-format fids))
         (results 
          (seed-query 
           (one-string
            "select prot,assigned_function from "
            "assigned_functions where prot in ~A")
           in-data
           )))
    results
    ))

(defun mysql-get-pegs-of-genome-in-categories-table (fids)
  (let* ((in-data (fids-to-mysql-in-format fids))
         (results 
          (seed-query
           "select distinct gene_id from categories where gene_id in ~A"
           in-data 
           )))
    (mapcar 'car results)
    ))

(defun mysql-get-categories-info-by-type (in-data category-type)
  (let ((query 
         (formatn 
          (one-string
           "select * from categories where gene_id in ~A and "
           "type_ID = '~A'")
          in-data category-type
          )))
    (seed-query query)
    ))

(defun mysql-get-description-for-peg (fid)
  (second 
   (first
    (seed-query 
     (one-string
      "select prot,assigned_function from "
      "assigned_functions where prot=\"~A\"")
     fid
     ))))


(defun fids-to-mysql-in-format (fids) (format nil "(~{~s~^,~})" fids))
   
(defun absolute-seed-path-from-fileno (fileno)
  (let ((relative-filepath 
         (caar
          (seed-query 
           "select file from file_table where fileno = ~D"
           fileno
           ))))
    (utils::s+ 
     *seed-mysql-data-root*
     relative-filepath
     )))

(defun seed-gene-info-for-gid-nsf (gid)
  (seed-query
   "select id,contig,location,type from features where genome = ~S" 
   gid))

(defun seed-gene-info-for-gid-sf (gid)
  (seed-query
   (one-string
    "select id,contig,location,type,aliases"
    " from features where genome = ~S")
   gid
   ))

(defun seed-gene-info-for-gid-msf (gid)
  ;; don't download any features which are marked as deleted,
  ;; as per rob edwards email 9/7/10
  (seed-query 
   (formatn
    (one-string
     "select * from features left join deleted_fids on (features.id = "
     "deleted_fids.fid) where features.genome = ~S and "
     "deleted_fids.fid IS NULL;"
     )
    gid
    )))

(defun seed-gene-info-for-seed-id (seed-id)
  (seed-query "select * from features where id = \"~A\"" seed-id))
                                   

(defun seed-gene-gid-and-contig-for-pegid (pegid)
  (seed-query "select id,genome,contig from features where id = ~S" pegid))

(defun genome-of-contig (contig-id)
  (seed-query "select genome from contig_lengths where contig = ~S" contig-id))

(defun get-all-seed-types ()
  (let ((domains (list "Virus" "Bacteria" "Plasmid" "Eukaryota" "Archaea")))
    (loop for domain in domains 
      collect
      (list domain (get-all-types-in-domain domain))
      )))
  
(defun get-all-types-in-domain (domain)
  (let* ((thelist nil)
         (orglisting (bio::all-gids-in-domain-mysql domain))
         (orgs
          (loop for org in orglisting collect (first org))))
    (loop for org in orgs
          as figtypes = 
          (bio::seed-query "select type from features where genome = ~S" org)
          do
          (loop for figtype in figtypes
                do 
                (pushnew (car figtype) thelist :test 'string-equal)
                ))
    thelist
    ))

#||

;; Result of get-all-seed-types on pipeline 7002

(("Virus" ("opr" "prm" "rna" "peg"))
 ("Bacteria"
  ("atn" "pi" "sRNA" "trm" "prm" "rsw" "pp" "opr" "rna" "pbs" "peg"))
 ("Plasmid" ("rna" "pbs" "opr" "peg"))
 ("Eukaryota" ("peg" "rna"))
 ("Archaea" ("rsw" "pbs" "prm" "opr" "rna" "trm" "peg")))


||#

(defun all-features-of-a-genome (gid &optional (type t))
  (mapcar 
   (lambda (feature-data) 
     (let ((location-info (fifth feature-data)))
       (list (parse-seed-location-info location-info) feature-data)
       ))
   (if (eq type t)
       (seed-query "select * from features where genome = ~S" gid)
     (seed-query 
      "select * from features where genome = ~S && type = ~S" gid type)
     )))

(defun all-features-of-a-domain (domain &optional (type t))
  (loop for (gid gname) in (all-gids-in-domain-mysql domain)
        nconc
        (let ((data (all-features-of-a-genome gid type)))
          (if data (list (list gid gname data)) nil)
          )))

(defun all-genomes-of-the-seed () (seed-query "select * from genome"))


(defun filenos-for-a-peg (peg)
  (mapcar
   'first
   (seed-query
    "select fileno from protein_sequence_seeks where id = ~S"
    peg
    )))

(defun file-from-fileno (fileno &key (if-does-not-exist :error))
  (let ((result 
         (seed-query 
          "select file from file_table where fileno = ~D"
          fileno
          )))
    (cond
     ((null result) 
      (case if-does-not-exist
        (:error 
         (error "No file corresponding to fileno ~D in file_table!" fileno))
        (otherwise if-does-not-exist)
        ))
     ((= 1 (length result)) (caar result))
     (t (error
         "file_table non-unique!  Query returned ~D results: ~S"
         (length result) result
         )))))
       

(defun md5-info-from-peg-id (peg-id)
  (seed-query "select * from protein_sequence_MD5 where id = ~S" peg-id))

(defun sim-seeks-info-from-prefixed-md5-key (sim-seeks-key)
  (seed-query "select * from sim_seeks where id = ~S" sim-seeks-key))

(defun md5-info-from-md5-key (key)
  (seed-query "select * from protein_sequence_MD5 where md5 = ~S" key))

(defun peg-synonyms-info-from-key (key) 
  (seed-query 
   "select * from peg_synonyms where maps_to = ~S" key))