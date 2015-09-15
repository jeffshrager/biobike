;;; -*- mode: Lisp; Syntax: Common-Lisp; Package: nvpl; -*-

(in-package :bio)

;;; +=========================================================================+
;;; | Copyright (c) 2011 JP Massar.                                           |
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

;; Author: JP Massar.

#||

Here's how you bring up any NCBI taxonomy page:

http://www.ncbi.nlm.nih.gov/Taxonomy/Browser/wwwtax.cgi?mode=Info&id=103690&lvl=3&p=mapview&p=has_linkout&p=blast_url&p=genome_blast&lin=f&keep=1&srchmode=1&unlock

||#

(defparameter *ncbi-names-file* (translate-simple-lp "biol:ncbi;names.dmp"))
(defparameter *ncbi-nodes-file* (translate-simple-lp "biol:ncbi;nodes.dmp"))

(defparameter *ncbi-taxonomy-browser-url-template* 
  (one-string
   "http://www.ncbi.nlm.nih.gov/Taxonomy/Browser/wwwtax.cgi?"
   "mode=Info&id=~A&lvl=3&p=mapview&p=has_linkout&p=blast_url"
   "&p=genome_blast&lin=f&keep=1&srchmode=2&unlock"
   ))

(defparameter *sql-taxonomy-base-date* "2001-01-01")

(defstruct ncbi-id-info sname synonyms parent marked?)

(defstruct ncbi-sname-info id marked?)

#-:lispworks
(def-aframe-type ncbi-taxonomy-node (aframe))

#||



0 - The NCBI taxonomy files are on the following ftp site:

ftp://ftp.ncbi.nih.gov/pub/taxonomy/ 

You need to retrieve taxdump.tar.gz and gunzip and untar 
the files.  The ones we use are nodes.dmp and names.dmp.  The
taxdump_readme.txt file is also useful.  

1 - Make sure the retrieved ncbi taxonomy files are in the directory.
Then load this code into the lisp.  
(cl "biol:ncbi;taxonomy-parse.lisp")
2 - Run parse-ncbi-id-names-and-nodes-files, creating a hash.
(setq hash (parse-ncbi-id-names-and-nodes-files))
3 - Run compute-ncbi-taxonomy-ids-for-seed-orgs using the hash table.
(compute-ncbi-taxonomy-ids-for-seed-orgs hash)
This stores taxonomy ids in organisms' #$ncbi-taxonomy-id slots and returns
a list of organisms that have no ncbi ids.  
4 - Run associate-ncbi-synonyms-with-organism-frames using the hash.
(associate-ncbi-synonyms-with-organism-frames hash)
5 - Run create-minimal-ncbi-hash-table to remove all unused hash table entries 
This creates a new, much smaller hash table.
(setq mhash (create-minimal-ncbi-hash-table hash))
You want to lose the pointer to the big one created previously if you
aren't going to kill the lisp. (setq hash nil)
6 - Run create-acache-taxonomy-nodes, which creates frames storing all the 
parent/child/leaf ncbi taxonomy information starting at 
*ncbi-taxonomy-root-frame*.
(create-acache-taxonomy-nodes mhash)
7 - Test by calling ncbi-taxonomy-strings on an organism.
  (defined in seedorgs/ seed-organism-types.lisp)  
8 - Commit.  
9 - If necessary, use destroy-acache-taxonomy-nodes to start over.

||#

(defun parse-ncbi-id-names-and-nodes-files ()
  (let ((taxonomy-hash (make-hash-table :test 'equalp)))
    (cformatt "Processing names file...")
    (with-open-file (p *ncbi-names-file* :direction :input)
      (loop 
       as line = (read-line p nil nil)
       for count from 0
       until (null line)
       do
       (when (zerop (mod count 20000)) (formatt "."))
       (flet ((parse (line)
                (let* ((elements (string-split line #\|))
                       (id (string-trim *whitespace* (first elements)))
                       (sname (string-trim *whitespace* (second elements))))
                  (values sname id)
                  )))
         (cond 
          ((search "scientific name" line :test 'string-equal)
           (multiple-value-bind (sname id) (parse line)
             (setf (gethash id taxonomy-hash)
                   (vif (data (gethash id taxonomy-hash))
                        (progn
                          (setf (ncbi-id-info-sname data) sname)
                          data)
                        (make-ncbi-id-info :sname sname)
                        ))))
          ((search "synonym" line :test 'string-equal)
           (multiple-value-bind (syn id) (parse line)
             (setf (gethash id taxonomy-hash)
                   (vif (data (gethash id taxonomy-hash))
                        (progn
                          (push syn (ncbi-id-info-synonyms data))
                          data)
                        (make-ncbi-id-info :synonyms (list syn))
                        ))))))))
    (cformatt "Processing nodes file...")
    (with-open-file (p *ncbi-nodes-file* :direction :input)
      (loop 
       as line = (read-line p nil nil)
       for count from 0
       until (null line)
       do
       (when (zerop (mod count 20000)) (formatt "."))
       (let* ((elements (string-split line #\|))
              (id (string-trim *whitespace* (first elements)))
              (parent-id (string-trim *whitespace* (second elements))))
         (vif (data (gethash id taxonomy-hash))
              (progn
                (setf (ncbi-id-info-parent data) parent-id)
                (setf (gethash (ncbi-id-info-sname data) taxonomy-hash) 
                      (make-ncbi-sname-info :id id)
                      ))
              (cformatt "No scientific name found for node id ~A" id)
              ))))
    taxonomy-hash
    ))
      
(defun ncbi-id-from-organism-name (orgf hash)
  (let* ((orgn (#^fname orgf))
         (assumed-ncbi-name 
          (substitute 
           #\Space #\_
           (substitute #\Space #\- orgn))))
    (let ((data (gethash assumed-ncbi-name hash)))
      (when data 
        (ncbi-sname-info-id data)
        ))))

(defun ncbi-id-from-seed-gname (gname hash)
  (let ((data (gethash gname hash)))
    (when data 
      (ncbi-sname-info-id data)
      )))

(defun compute-ncbi-taxonomy-ids-for-seed-orgs (hash)
  (loop for org in (available-organisms)
        with no-ncbi-ids = nil
        with human-checklist = nil
        as seed-id = (#^seed-id org)
        as prefix = (first (string-split seed-id #\.))
        as name = (#^fname org)
        do
        (cond
         ((string-equal prefix "6666666")
          (let ((ncbi-id (ncbi-id-from-organism-name org hash)))
            (if ncbi-id 
                (setf (#^ncbi-taxonomy-id org) (parse-integer ncbi-id))
              (push org no-ncbi-ids)
              )))
         (t 
          (let ((data (gethash prefix hash)))
            (if data 
                (let ((dname (substitute #\- #\Space (ncbi-id-info-sname data))))
                  (if (> (compute-edit-distance name dname) 10)
                      (push (list name dname) human-checklist)
                    (setf (#^ncbi-taxonomy-id org) (parse-integer prefix))
                    ))
              (push org no-ncbi-ids)
              ))))
        finally (return (list no-ncbi-ids human-checklist))
        ))

(defun associate-ncbi-synonyms-with-organism-frames (hash)
  (loop for org in (available-organisms) 
        as ncbi-id = (ncbi-id-from-organism-name org hash)
        do
        (when ncbi-id 
          (let* ((data (gethash ncbi-id hash))
                 (synonyms (ncbi-id-info-synonyms data)))
            (when synonyms 
              (setf (#^ncbi-synonyms org) synonyms)
              )))))

(defun create-minimal-ncbi-hash-table (ncbi-hash) 
  (let ((root-id *ncbi-taxonomy-id-root*)
        (minimal-hash-table (make-hash-table :test 'equalp)))
    (setf (ncbi-id-info-marked? (gethash root-id ncbi-hash)) t)
    (labels ((mark (parent-id)
               (if (string-equal parent-id root-id)
                   nil
                 (let ((parent-node (gethash parent-id ncbi-hash)))
                   (if parent-node 
                       (progn
                         (setf (ncbi-id-info-marked? parent-node) t)
                         (mark (ncbi-id-info-parent parent-node))
                         )
                     (error "Taxonomy id ~A does not have a parent!" parent-id)
                     )))))
      (loop for org in (available-organisms)
            as ncbi-id = (#^ncbi-taxonomy-id org)
            do
            (when ncbi-id (mark (formatn "~S" ncbi-id)))
            ))
    (maphash 
     (lambda (key value)
       (typecase value 
         (ncbi-id-info 
          (if (not (ncbi-id-info-marked? value))
              (remhash key ncbi-hash)
            (setf (gethash key minimal-hash-table) value)
            ))
         (ncbi-sname-info (remhash key ncbi-hash))
         ))
     ncbi-hash
     )
    minimal-hash-table
    ))

(defun create-acache-taxonomy-nodes (hash)
  (let ((root-id *ncbi-taxonomy-id-root*)
        (root-frame 
         (frame-fnamed *ncbi-taxonomy-root-name* t 'ncbi-taxonomy-node)))
    (setq *ncbi-taxonomy-root-frame* root-frame)
    (labels ((maybe-create-node (node-info child leaf)
               (let* ((node-name (ncbi-id-info-sname node-info))
                      (fname (create-valid-frame-name 
                              node-name :space-char-action #\-))
                      (frame (frame-fnamed fname t 'ncbi-taxonomy-node))
                      (parent-id (ncbi-id-info-parent node-info))
                      (parent-node (gethash parent-id hash)))
                 (unless (#^ncbi-taxonomy-string frame)
                   (setf (#^ncbi-taxonomy-string frame) node-name))
                 (when child 
                   (pushnew child (#^children frame))
                   (setf (#^parent child) frame))
                 (when leaf 
                   (push leaf (#^leaves frame))
                   (setf (#^ncbi-taxonomy-parent leaf) frame))
                 (if (string-equal parent-id root-id)
                     (progn
                       (setf (#^parent frame) root-frame)
                       (pushnew frame (#^children root-frame)))
                   (progn
                     (maybe-create-node parent-node frame nil)
                     )))))
      (loop 
       for org in (available-organisms)
       as ncbi-id = (#^ncbi-taxonomy-id org)
       do
       (when ncbi-id 
         (setf (#^ncbi-taxonomy org) nil)
         (when (numberp ncbi-id) (setq ncbi-id (formatn "~D" ncbi-id)))
         (let ((org-hash-data (gethash ncbi-id hash)))
           (unless org-hash-data 
             (error "No hash entry for org ~A with taxonomy-id ~A" org ncbi-id))
           (let* ((parent-id (ncbi-id-info-parent org-hash-data))
                  (parent-hash-data (gethash parent-id hash)))
             (unless parent-hash-data 
               (error "No hash entry for parent of org ~A with tax id ~A"
                      org ncbi-id 
                      ))
             (maybe-create-node parent-hash-data nil org)
             )))))))

(defun destroy-acache-taxonomy-nodes ()
  (labels ((destroy (node) 
             (let ((children (#^children node))
                   (leaves (#^leaves node)))
               (frames::delete-frame node)
               (loop for child in children do (destroy child))
               (loop for leaf in leaves do 
                     (frames::delete-slot leaf #$ncbi-taxonomy-parent)
                     (frames::delete-slot leaf #$ncbi-taxonomy)
                     ))))
    (destroy *ncbi-taxonomy-root-frame*)
    (setq *ncbi-taxonomy-root-frame* nil)
    ))

(defun taxonomy-path-from-ncbi-id (ncbi-id hash &key (verbose? t))
  (let ((path nil)
        (root *ncbi-taxonomy-id-root*))
    (if (null (gethash ncbi-id hash))
        (progn
          (when verbose? 
            (cformatt "No entry for ncbi id '~A' in taxonomy hash!" ncbi-id))
          nil
          )
      (loop 
       until (string-equal parent-id root)
       as data = (gethash ncbi-id hash)
       as name = (ncbi-id-info-sname data)
       as parent-id = (ncbi-id-info-parent data)
       do
       (push name path)
       (setq ncbi-id parent-id)
       ))
    path
    ))
       
(defun check-seed-genomes-against-ncbi-taxonomy (complete-ncbi-hash)
  (cformatt "Downloading all genome info...")
  (let ((genome-info 
         (bio::seed-query "select genome,gname,taxonomy from genome"))
        (not-found nil))
    (cformatt "Checking genomes...")
    (loop for (gid gname taxonomy) in genome-info
          do
          (let ((ncbi-id (first (string-split gid #\.))))
            (unless (gethash ncbi-id complete-ncbi-hash)
              (unless (gethash gname complete-ncbi-hash)
                (push (list gid gname taxonomy) not-found)
                ))))
    not-found
    ))
    
(defun new-ncbi-taxonomy-id-from-old-ncbi-taxonomy-id (old-id)
  (let* ((url (formatn *ncbi-taxonomy-browser-url-template* old-id))
         (page-source (wb::web-page-contents url))
         (before-id-string "<em>Taxonomy ID: </em>")
         (pos (search before-id-string page-source)))
    (when pos 
      (let ((remaining-string 
             (subseq page-source (+ pos (length before-id-string)))))
        (let* ((pos-after-id (position-if-not 'digit-char-p remaining-string))
               (id-string (subseq remaining-string 0 pos-after-id)))
          id-string
          )))))

(defvar *seed-ids-with-new-ncbi-taxonomy-ids* nil)

(defun all-available-seed-genomes-ncbi-taxonomies (complete-ncbi-hash)
  (setq *seed-ids-with-new-ncbi-taxonomy-ids* nil)
  (cformatt "Downloading all genome info...")
  (let ((genome-info 
         (bio::seed-query "select genome,gname,taxonomy from genome"))
        (seed-taxonomy-hash (make-hash-table :test 'equalp)))
    (cformatt "Checking ~D genomes..." (length genome-info))
    (loop 
     with new-ncbi-tax-count = 0
     with six-count = 0
     for (gid gname seed-tax) in genome-info
     as ncbi-id = (first (string-split gid #\.))
     for count from 0
     do
     (when (zerop (mod count 25)) (formatt "."))
     (flet ((store (ncbi-tax) 
              (setf (gethash gid seed-taxonomy-hash)
                    (if ncbi-tax (list ncbi-tax :ncbi) (list seed-tax :seed))
                    ))
            (taxpath (ncbi-id) 
              (taxonomy-path-from-ncbi-id 
               ncbi-id complete-ncbi-hash :verbose? nil)))
       (cond
        ;; xxxxx
        ((string-equal ncbi-id "6666666")
         (let ((orgf (seed-id->frame gid)))
           (when orgf 
             (let ((ncbi-id 
                    (ncbi-id-from-organism-name orgf complete-ncbi-hash)))
               (when ncbi-id
                 (incf six-count)
                 (formatt "*** Match: ~A, ~A, ~A~%" gid orgf 
                          (ncbi-id-info-sname 
                           (gethash ncbi-id complete-ncbi-hash)
                           ))))))
         (store nil)
         )
        (t 
         (if (gethash ncbi-id complete-ncbi-hash)
             (let ((ncbi-tax (taxpath ncbi-id)))
               (store ncbi-tax)
               )
           (vif (data (gethash gname complete-ncbi-hash))
                (let* ((ncbi-id (ncbi-sname-info-id data))
                       (ncbi-tax (taxpath ncbi-id)))
                  (store ncbi-tax))
                (progn
                  (formatt "~% Using NCBI url for ~A~%" gid)
                  (vif (new-ncbi-id 
                        (new-ncbi-taxonomy-id-from-old-ncbi-taxonomy-id ncbi-id))
                       (let ((ncbi-tax (taxpath new-ncbi-id)))
                         (when ncbi-tax 
                           (incf new-ncbi-tax-count)
                           (push (list gid new-ncbi-id ncbi-id)
                                 *seed-ids-with-new-ncbi-taxonomy-ids*)
                           (store ncbi-tax))
                         (store nil)
                         ))))))))
     finally
     (progn
       (cformatt "~D entries in seed taxonomy hash" 
                 (hash-table-count seed-taxonomy-hash))
       (cformatt "~D organisms with new ncbi ids found" new-ncbi-tax-count)
       (cformatt "~D 6666666 matches" six-count)
       ))
    seed-taxonomy-hash
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#||

Algorithm to modify seed genome table's taxonomy field 
with the NCBI taxonomy data where available.  

1 - If there's new NCBI data download it.  (See above)

2 - Create the giant hash of all the ncbi taxonomy data using
(setq complete-hash (parse-ncbi-id-names-and-nodes-files)).

3 - If the genome_taxonomy_source table does not exist, create it using
(create-genome-taxonomy-source-table)

Then call
(initialize-genome-taxonomy-source)

This copies every seed gid and taxonomy into the new table from the
genome table.  The source field is set to "seed" and the updated field
is set to "2001-01-01".
 
4 - If the genome_taxonomy_source does exist, there may be
additional organisms in the genome table since it was updated.  
So for any genome that isn't in the genome_taxonomy_source table, 
but is in the genome table, create a row for that genome as in step 3.
Do this by calling:
(update-genome-taxonomy-source)

5 - Create the hash table mapping seed ids to taxonomies and their origin.
(setq seed-id->ncbi-hash
      (all-available-seed-genomes-ncbi-taxonomies complete-hash)).

6 - A list of any seed ids that have new ncbi taxonomy ids is stored
in *seed-ids-with-new-ncbi-taxonomy-ids*.  We'll use this later.  
 
7 - Do the actual update!  Change the data in the taxonomy field of the 
genome table when we have ncbi taxonomy info for the seed-id/organism.  
(update-all-genome-taxonomies seed-id->ncbi-hash)

8 - Store the new ncbi taxonomy ids, if any, into the frames corresponding
to those organisms.  (update-acache-with-new-ncbi-taxonomy-ids)

9 - If you are satisfied with step 8, then commit the changes.  
(db.ac::commit)

10 - (setq complete-hash nil), allowing the huge hash table to be gc'ed.

11 - If you need to undo this operation, you can call 
(restore-seed-taxonomies)

which puts back the seed taxonomies instead of the ncbi taxonomies
into the seed genome table and resets the info in the genome_taxonomy_source
table.  

||#

(defun create-genome-taxonomy-source-table ()
  (bio::seed-query 
   (one-string
    "CREATE TABLE genome_taxonomy_source "
    "("
    "Genome varchar(16), "
    "Source varchar(16), "
    "Seedtax text, "
    "Updated date, "
    "PRIMARY KEY (Genome)"
    ")"
    )))

(defun initialize-genome-taxonomy-source ()
  (let ((data (bio::seed-query "select genome,taxonomy from genome")))
    (loop for (gid seedtax) in data 
          for count from 0
          do
          (when (zerop (mod count 500)) (formatt "."))
          (add-genome-taxonomy-source-row gid seedtax)
          finally (terpri)
          )))

(defun update-genome-taxonomy-source ()
  (let ((data (bio::seed-query "select genome,taxonomy from genome")))
    (loop for (gid seedtax) in data 
          for count from 0
          with update-count = 0
          do
          (when (zerop (mod count 500)) (formatt "."))
          (let ((exists? 
                 (bio::seed-query
                  (formatn
                   (one-string
                    "select genome from genome_taxonomy_source "
                    "where genome = '~A'")
                   gid
                   ))))
            (unless exists? 
              (incf update-count)
              (add-genome-taxonomy-source-row gid seedtax))
            )
          finally (progn (terpri) (return update-count))
          )))

(defun add-genome-taxonomy-source-row (gid seedtax)
  (bio::seed-query 
   (formatn
    (one-string
     "INSERT INTO genome_taxonomy_source (Genome, Source, Seedtax, Updated) "
     "VALUES (\"~A\", \"~A\", \"~A\", \"~A\") ")
    gid "seed" seedtax *sql-taxonomy-base-date*
    )))

(defun populate-genome-taxonomy-source-table (seed-taxonomy-hash)
  (maphash 
   (lambda (key value)
     (add-genome-taxonomy-source-row key (second value)))
   seed-taxonomy-hash
   ))

(defun update-all-genome-taxonomies (seed-taxonomy-hash)
  (let ((date (make-timestamp-string :mode :sql-datestamp))
        (count 0))
    (maphash 
     (lambda (key value)
       (let ((taxlist (first value))
             (source (second value)))
         (when (not (string-equal source "seed"))
           (incf count)
           (when (zerop (mod count 100)) (formatt "."))
           (update-one-genome-taxonomy 
            key source (taxlist->seed-format taxlist) date)
           )))
     seed-taxonomy-hash
     )
    (cformatt "~D ncbi taxonomies stored in genome table." count)
    ))

(defun taxlist->seed-format (taxlist)
  (string-join taxlist "; "))

(defun update-one-genome-taxonomy (gid source new-taxonomy date)
  (update-genome-taxonomy-field gid new-taxonomy)
  (update-gts-source-and-date gid source date)
  )

(defun update-genome-taxonomy-field (gid new-taxonomy)
   (bio::seed-query
    (formatn
     (one-string
      "UPDATE genome "
      "SET taxonomy = \"~A\" "
      "WHERE genome = \"~A\"; "
      )
     new-taxonomy gid
     )))

(defun update-gts-source-and-date (gid source date)
  (bio::seed-query 
   (formatn
    (one-string
     "UPDATE genome_taxonomy_source "
     "SET "
     "source = \"~A\", "
     "updated = \"~A\" "
     "WHERE genome = \"~A\"; "
     )
    source date gid
    )))
     
(defun update-acache-with-new-ncbi-taxonomy-ids ()
  (loop 
   for (seed-id new-tax-id nil) in *seed-ids-with-new-ncbi-taxonomy-ids*
   with count = 0
   with orgs = nil
   do
   (let ((orgf (seed-id->frame seed-id)))
     (when orgf
       (let ((nti (#^ncbi-taxonomy-id orgf)))
         (cond
          ((null nti)
           (incf count)
           (push orgf orgs)
           (setf (#^ncbi-taxonomy-id orgf) (parse-integer new-tax-id)))
          ((not (= nti (parse-integer new-tax-id)))
           (incf count)
           (push orgf orgs)
           (setf (#^old-ncbi-taxonomy-id orgf) (#^ncbi-taxonomy-id orgf))
           (setf (#^ncbi-taxonomy-id orgf) (parse-integer new-tax-id))
           )
          (t nil)
          ))))
   finally (progn (cformatt "~D new taxonomy ids set" count) (return orgs))
   ))
   
(defun restore-seed-taxonomies ()
  (let ((data
         (bio::seed-query "select genome,seedtax from genome_taxonomy_source")))
    (loop for (genome seedtax) in data
          do
          (update-genome-taxonomy-field genome seedtax)
          (update-gts-source-and-date genome "seed" *sql-taxonomy-base-date*)
          )))
             
          
