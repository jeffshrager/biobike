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

;; Specification of interface (possibly not up to date)
;; can be found at http://ws.nmpdr.org/

;; Phage list http://bioseed.mcs.anl.gov/~redwards/FIG/Phage.cgi

;; seed glossary: http://www.theseed.org/wiki/Glossary#PEG

;; Biobike on seed machine: http://seed.sdsu.edu:7001/biologin

;; new wsdl: http://seed.sdsu.edu/FIG/wsdl_seed_complex.cgi

;; good test gid is 217.1
;; good virus test gid is 12021.1

#||

;; returns list of 
;; peg contig-name encodes-protein direction from to cogdesc
(defun soap-parsed-peg-info (gid)
  (let* ((all-pegs (setq *pegs-for-gid* (all-pegs-of-a-genome gid)))
         (parsed-peg-info 
          (loop for peg across all-pegs 
                for j from 1
                collect
                (parsed-peg-location-info peg)
                do
                (when (= 0 (mod j 100)) (print "."))
                )))
    (mapcar 
     (lambda (info) 
       (destructuring-bind 
           (peg contig-name encodes-protein direction from to cogdesc)
           info
         (list peg contig-name encodes-protein direction from to cogdesc)
         ))
     parsed-peg-info
     )))

(defun parsed-peg-location-info (peg)
  (destructuring-bind (contig-name from to direction)
      (peg-contig-from-and-to peg)
    (let ((encodes-protein t)
          (cogdesc (gene-annotation peg)))
      (list 
       peg contig-name encodes-protein
       direction from to cogdesc)
      )))

||#

#||

;; returns list of 
;; peg/rna contig-name encodes-protein direction from to description
(defun mysql-parsed-peg-info (gid)
  (let ((info 
         (seed-query
          "select id,contig,location from features where genome = ~S" 
          gid)))
    (setq *pegs-for-gid* (mapcar 'first info))
    (mapcar 
     (lambda (x desc) 
       (destructuring-bind (xcontig from to direction)
           (parse-seed-location-info (third x))
         (declare (ignore xcontig))
         (list
          (first x)
          (second x)
          t
          direction 
          from
          to
          desc
          )))
     info
     (mysql-get-descriptions-for-pegs-of-genome *pegs-for-gid*)
     )))

(defun add-seed-id-to-parsed-genes (gene-data gid)
  (let* ((seed-name (first gene-data))
         (bike-name (seed-peg-name-to-biobike-name seed-name gid)))
    ;; replace the seed name by the bike name and use the seed name
    ;; as the value of the new seed-id column
    (append (list bike-name) (cdr gene-data) (list seed-name))
    ))

(defun add-orgname-to-available-organisms (orgname)
  (let ((old-org-list nil)
        (new-org-list nil)
        (available-organisms-file
         (utils::s+ *data-directory* "available-organisms.lisp")))
    (with-open-file (s available-organisms-file :direction :input)
      (setq old-org-list (read s nil)))
    ;; shut compiler up
    old-org-list
    (setq new-org-list (pushnew orgname old-org-list :test 'string-equal))
    (with-open-file 
        (p available-organisms-file
           :direction :output
           :if-exists :supersede
           )
      (write new-org-list :stream p)
      )))

||#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;;; Tests

#+test
(progn

  (defvar *some-genome* nil)

  (defvar *some-peg* nil)

  (defun some-genome-id ()
    (let* ((bacterial-genomes 
            (sdsd
             (funcall 
              'seed::genomes
              :complete "complete" :restrictions "" :domain "Bacteria"
              )))
           (genome 
            (aref bacterial-genomes (random (length bacterial-genomes))))
           (genomeid (first (utils::string-split genome #\Tab))))
      (setq *some-genome* genomeid)
      ))

  (defun some-peg ()
    (let* ((pegs (sdsd (funcall 'seed::pegs-of :genomeid *some-genome*)))
           (peg (aref pegs (random (length pegs)))))
      (setq *some-peg* peg)
      ))

  (defun seed-test ()
    (some-genome-id)
    (some-peg)
    (funcall 'seed::abstract-coupled-to :peg *some-peg*)
    ;; (funcall 'seed::client-39 :peg "fig|83333.1.peg.1234")
    (funcall 'seed::get-translation :peg "fig|83333.1.peg.1234")
    ;; (funcall 'seed::client-55 :peg "fig|83334.1.peg.121")
    (funcall 'seed::abstract-coupled-to :peg "fig|83334.1.peg.121")
    ;; (funcall 'seed::client-3 
    ;; :pegs "fig|83334.1.peg.121,fig|83334.1.peg.123,fig|83334.1.peg.120")
    (funcall 'seed::adjacent
             :pegs 
             "fig|83334.1.peg.121,fig|83334.1.peg.123,fig|83334.1.peg.120")
    ;; (funcall 'seed::client-43 :peg "fig|83334.1.peg.121")
    (funcall 'seed::aliases-of :peg "fig|83334.1.peg.121")
    )

  (defun check-virus-gids ()
    (let ((bad-info nil))
      (flet ((show ()
               (loop for (gid contig info) in bad-info
                     do
                     (format t "~A : ~A : ~A~%" gid contig (reverse info))
                     )))
        (handler-case 
            (progn
              (loop for gid in *virus-gids* 
                    do
                    (progn
                      (utils::formatt "Checking ~A~%" gid)
                      (let ((gid-results (check-for-bad-chars gid)))
                        (when gid-results 
                          (setq bad-info (nconc bad-info gid-results))
                          ))))
              (show))
          (error () (show))
          ))))

  )

#||

(defun verify-start-and-stop-codons (org)
  (loop for g in (#^genes org)
        as seq = (bbi::with-bbl-form (bbi::sequence-of g))
        do
        (cond
         ((< (length seq) 6) 
          (print (list g :short (#^from g) (#^to g) (#^direction g))))
         ((search "rna" (#^fname g)) nil)
         (t 
          (let ((start (subseq seq 0 3))
                (end (subseq seq (- (length seq) 3))))
            (loop for start-codon in '("atg" "gtg" "ttg")
                  do
                  (when (string-equal start-codon start) (return nil))
                  finally (print (list g :start start))
                  )
            (loop for stop-codon in '("tag" "taa" "tga")
                  do
                  (when (string-equal stop-codon end) (return nil))
                  finally (print (list g :end end))
                  ))))))

||#
                  

#||

(progn

  (setq *seed-xsql-connection*
        (genericdb::make-db-server-connection
         :mysql
         :file "/var/lib/mysql/mysql.sock"
         :user "seed"
         :password "theseed"
         :dbname "seed"
         ))
  (setq genericdb::*generic-db-verbose* nil)
  (setq *seed-access-mode* :mysql)
  (genericdb::xsql *seed-xsql-connection* "show tables")
  )
--> 

(defun mysql-columns (c tables)
  (loop for (table) in tables
        do 
        (multiple-value-bind (row column-names)
            (genericdb::xsql
             c (utils::s+ "select * from " table " limit 1"))
          (declare (ignore row))
          (utils::formatt "~%Table ~A~%" table)
          (loop for col in column-names do 
                (utils::formatt "  ~A~%" col)
                ))))

(genericdb::xsql y "select * from contig_lengths where genome = \"217.1\"")

;; all the pegs of a genome
(genericdb::xsql y "select * from features where genome = \"217.1\" limit 10")

;; file and file-number for contig sequence information
;; pathname currently preceded by /mnt/u03/robs_data/FIGdisk/
(genericdb::xsql y "select * from file_table limit 5")

||#

#||


<22>> (mysql-columns y tables)
:: 

Table annotation_seeks
  fid
  dateof
  who
  ma
  fileno
  seek
  len

Table assigned_functions
  prot
  made_by
  assigned_function
  quality
  org

Table attribute
  genome
  ftype
  id
  tag
  val
  url

Table attribute_metadata
  attrkey
  metakey
  metaval

Table aux_roles
  subsystem
  role

Table bbh
  peg1
  peg2
  psc
  nsc

Table chromosomal_clusters
  cluster_id
  id

Table comp_cas
  cid
  cas

Table comp_name
  cid
  pos
  name

Table contig_lengths
  genome
  contig
  len

Table contig_md5sums
  genome
  contig
  md5

Table contig_seeks
  genome
  contig
  startN
  indexpt
  fileno
  seek

Table deleted_fids
  genome
  fid

Table distances
  genome1
  genome2
  dist

Table ec_map
  ec
  map

Table ec_names
  ec
  name

Table ext_alias
  id
  alias
  genome

Table external_orgs
  prot
  org

Table fc_pegs
  peg1
  peg2
  score

Table features
  id
  idN
  type
  genome
  location
  contig
  minloc
  maxloc
  aliases

Table fid_links
  fid
  link

Table fid_locks
  fid

Table file_table
  file
  fileno

Table fr2go
  role
  go_id

xxx
Table genome
  genome
  gname
  szdna
  cksum
  maindomain
  pegs
  rnas
  complete
  restrictions
  taxonomy

Table genome_md5sum
  genome
  md5sum

Table go_terms
  go_id
  go_desc
  go_type
  obsolete

Table id_correspondence
  file_num
  set_id
  protein_id
  type

Table id_correspondence_type
  id
  name
  searchable

Table literature_titles
  gi
  pmid
  title

Table localfam_cid
  family
  cid

Table localfam_function
  family
  function

Table localid_cid
  localid
  cid

Table localid_map
  family
  localid

Table map_name
  map
  mapname

Table neigh_seeks
  role
  seek
  len

Table pch_pins
  pin
  id

Table pchs
  peg1
  peg2
  peg3
  peg4
  inden13
  inden24
  para3
  para4
  rep

Table peg_synonyms
  maps_to
  maps_to_ln
  syn_id
  syn_ln

Table protein_sequence_MD5
  id
  gid
  md5

Table protein_sequence_seeks
  id
  fileno
  seek
  len
  slen
  cksum
  sufcksum

Table reaction_direction
  rid
  mapid
  direction

Table reaction_to_compound
  rid
  setn
  cid
  stoich
  main
  path

Table reaction_to_enzyme
  rid
  role

Table replaced_fids
  genome
  from_fid
  to_fid

Table reversible
  rid
  reversible

Table roles
  prot
  role
  made_by
  org

Table sim_seeks
  id
  fileN
  seek
  len

Table subsystem_index
  protein
  subsystem
  role
  variant

Table subsystem_metadata
  subsystem
  classification
  class_1
  class_2
  curator
  creation_date
  last_update
  version
  exchangable

||#

