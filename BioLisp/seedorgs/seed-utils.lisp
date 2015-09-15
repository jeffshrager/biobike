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

;; Returns (contig from to) in biobike terms
(defun parse-seed-location-info (location-info)
  (let* ((first-segment (first (utils::string-split location-info #\,)))
         (fields (utils::string-split (reverse first-segment) #\_))
         (to (read-from-string (reverse (first fields))))
         (from (read-from-string (reverse (second fields))))
         (contig-name (reverse (utils::string-join (cddr fields) #\_)))
         (direction (if (> from to) :b :f)))
    ;; Doesn't handle circular contigs yet
    (when (eq direction :b)
      (let ((temp from)) (setq from to) (setq to temp)))
    (list contig-name from to direction)
    ))

;; location info = "NC_007517_2674816_2674748,NC_007517_2674746_2673697"
;; Returns (contig-name from to direction segment-from-tos wraps?)
;; wrapping is not determined yet
(defun new-parse-seed-location-info (location-info)
  (if (null (position #\, location-info))
      (parse-single-seed-location location-info)
    (let ((segments (utils::string-split location-info #\,)))
      (parse-multiple-seed-locations segments location-info)
      )))

;; first-segment = "NC_007517_2674816_2674748"
;; fields = ("8474762" "6184762" "715700" "CN")
;; to = 2674748
;; from = 2674816
;; contig-name = "NC_007517"
(defun parse-single-seed-location (location-info)
  (let* ((fields (utils::string-split (reverse location-info) #\_))
         (to (read-from-string (reverse (first fields))))
         (from (read-from-string (reverse (second fields))))
         (contig-name (reverse (utils::string-join (cddr fields) #\_)))
         (direction (if (> from to) :b :f)))
    ;; Doesn't handle circular contigs yet
    (when (eq direction :b)
      (let ((temp from)) (setq from to) (setq to temp)))
    (list contig-name from to direction nil nil)
    ))

(defun parse-multiple-seed-locations (segments location-info)
  (let* ((parses (mapcar 'parse-single-seed-location segments))
         (first-parse (first parses))
         (first-contig (first first-parse))
         (contig-info 
          (find first-contig *genome-info* :key 'first :test 'string-equal)))
    (unless contig-info
      (error "No contig info for gene's seed id ~A!  This should be impossible!"
             first-contig))
    (unless (every (lambda (x) (string-equal (first x) first-contig)) parses)
      (error 
       "All the segments of ~A do not have the same contig!" location-info))
    (let ((contig-length (second contig-info))
          (circular? (sixth contig-info)))
      (multiple-value-bind (spans-origin? from to direction)
          (gene-spans-origin? parses contig-length circular?)
        (if spans-origin?
            (progn 
              (cformatt "Found a spanning gene, location ~A" location-info)
              (list 
               first-contig
               from 
               to
               direction 
               ;; If the gene is actually a spanning gene, then it 
               ;; doesn't really have two segments.  That's just the way
               ;; the seed chose to represent the fact that it spanned
               ;; the origin.  
               #+wrong
               (mapcar 
                (lambda (parse)
                  (list (second parse) (third parse) (fourth parse)))
                parses
                )
               nil
               t
               ))
          (list 
           ;; contig-name 
           first-contig
           ;; from
           (reduce 'min (mapcar 'second parses))
           ;; to
           (reduce 'max (mapcar 'third parses))
           ;; direction
           (if (>= (count-if (lambda (x) (eq (fourth x) :f)) parses)
                   (count-if (lambda (x) (eq (fourth x) :b)) parses))
               :f :b)
           ;; segment from tos
           (mapcar 
            (lambda (parse)
              (list (second parse) (third parse) (fourth parse)))
            parses
            )
           ;; wraps?
           nil
           ))))))

;; examples of forward and backward origin spanning seed gene location info.
;; we believe that all such origin spanning genes have exactly two 
;; comma-separated components, each component having one coordinate 
;; on the edge of the origin.  
;; NC_001365_7492_8273,NC_001365_1_232
;; NC_005231_1914_1,NC_005231_44343_43288
;; NC_013085_194454_194263,NC_013085_498_1

;; The DIRECTION of the gene is the direction of both segments.  
;; If the segments have contradictory directions, we assume something
;; is wrong, issue a warning, and say the gene is not really spanning.  
;; The FROM of the gene is the other coordinate paired with the 
;; contig length, while the TO of the gene is the other coordinate
;; paired with the '1'. 


(defun gene-spans-origin? (parses contig-length circular?)
  (block exit
    (and circular?
         (= 2 (length parses))
         (let* ((p1 (first parses)) 
                (p1s (second p1))
                (p1e (third p1))
                (p1d (fourth p1))
                (p2 (second parses))
                (p2s (second p2))
                (p2e (third p2))
                (p2d (fourth p2))
                (cl contig-length))
           (flet ((coord-paired-with (x) 
                    (cond
                     ((= x p1s) p1e)
                     ((= x p1e) p1s)
                     ((= x p2s) p2e)
                     ((= x p2e) p2s)
                     )))
             (when 
                 ;; Check that one segment has a '1' for one of its 
                 ;; components and the other segment has the contig
                 ;; length as one of its components, otherwise
                 ;; it's not circular.  
                 (or 
                  (and (or (= p1s 1) (= p1e 1))
                       (or (= p2s cl) (= p2e cl)))
                  (and (or (= p1s cl) (= p1e cl))
                       (or (= p2s 1) (= p2e 1))))
               (let ((dir 
                      (cond
                       ((and (eq p1d :f) (eq p2d :f)) :f)
                       ((and (eq p1d :b) (eq p2d :b)) :b)
                       (t 
                        (warn "Segment directions inconsistent!")
                        (return-from exit nil)
                        )))
                     (from (coord-paired-with cl))
                     (to (coord-paired-with 1))
                     )
                 (values t from to dir)
                 )))))))

(defun vformatt (s &rest args)
  (when *seed-load-verbose* (apply 'cformatt s args)))

(defun seed-annotation-file (gid)
  (utils::s+ 
   *seed-mysql-data-root* 
   *seed-relative-organisms-path*
   gid 
   "/"
   "annotations"
   ))


;;; This should all be moved to some utils file or similar
;;; and the file removed.

(defun seed-gid-to-biobike-orgname (gid) (substitute #\- #\. gid))

(defun seed-contig-name-to-biobike-contig-name (gid contig)
  (utils::s+
   (seed-gid-to-biobike-orgname gid)
   "."
   contig
   ))

(defun biobike-orgname-to-seed-gid (orgname) (substitute #\. #\- orgname))
(defun biobike-gid-to-seed-gid (orgname) (substitute #\. #\- orgname))

(defun seed-peg-name-to-biobike-name (seed-name gid)
  (let ((glen (length gid))
        (bike-name seed-name))
    ;; strip out organism gid and subsequent dot 
    (utils::vwhen (pos (search gid seed-name :test 'char-equal))
      (let ((dotpos (+ pos glen)))
        (when (char= #\. (char seed-name dotpos))
          (setq bike-name 
                (utils::s+ (subseq seed-name 0 pos) 
                           (subseq seed-name (1+ dotpos))
                           )))))
    (setq bike-name (substitute #\- #\| bike-name))
    (setq bike-name (substitute #\- #\. bike-name))
    bike-name
    ))

(defun gname->biobike-organism-name (gname)
  (substitute 
   #\@ #\)
   (substitute 
    #\@ #\( 
    (substitute #\- #\Space (string-capitalize gname))
    )))

(defun maybe-add-dot-to-org-prefix (s)
  (if (char-equal (char s (1- (length s))) #\.)
      s
    (s+ s ".")
    ))

(defun create-seed-organism-selection-operators ()
  (#+:allegro
   excl::without-redefinition-warnings 
   #-:allegro
   progn
   (eval 
    (forward-package-funcall 
     :vpl "GENERATE-SEED-ORGANISM-SELECTION-OPERATORS"
     ))))

(defun seed-organism-frame-operator (frame) 
  (intern (s+ (#^Fname frame) "-" "OPERATOR") :vpl))
                    
(defun clean-up-gid-table-records (filedata)
  (mapcar 
   (lambda (x) 
     (mapcar 
      (lambda (y) (string-trim '(#\Space) y))
      (string-split x #\Tab)
      ))
   filedata
   ))

(defun gid-string->organism-frame (gid)
  (loop for orgf in 
        (if user::*master-list-enabled* 
            (available-organisms)
          *seed-genome-frames*
          )
        as id = (#^seed-id orgf)
        when (or (string-equal gid id)
                 (string-equal (biobike-gid-to-seed-gid gid) id))
        do
        (return orgf)
        finally (return nil)
        ))

(defun biobike-gname-or-biobike-gid (gname gid)
  (let* ((biobike-orgname (gname->biobike-organism-name gname))
         (biobike-gid (seed-gid-to-biobike-orgname gid))
         (existing-frame (frame-fnamed biobike-orgname)))
    (if existing-frame
        (progn 
          (cformatt 
           (one-string-nl
            "Organism name '~A' associated with gid ~A"
            "from the seed genome table already names"
            "another organism with gid ~A")
           gname gid (#^seed-id existing-frame))
          biobike-gid
          )
      biobike-orgname
      )))

(defun get-feature-item (fid item)
  (get-feature-prop (gethash fid *features-info-hash*) item))

(defun get-feature-prop (plist item) (getf plist item))

(defmacro set-feature-prop (plist item value)
  `(setf (getf ,plist ,item) ,value))

(defun set-feature-item (fid item value)
  (setf (getf (gethash fid *features-info-hash*) item) value))

(defun seed-peg-alias-ok? (alias) 
  (let ((len (length alias)))
    (and 
     (not (> len 10))
     (not (< len 4))
     (not (find #\Space alias))
     (not (find #\( alias))
     (not (find #\) alias))
     )))

(defun maybe-add-newline (s)
  (if (char= #\Newline (lastelem s)) s (s+ s #\Newline)))

(defun crossblast-info-for-peg (peg)
  (let* ((seed-id (#^seed-id peg))
         (md5info (md5-info-from-peg-id seed-id)))
    (unless (= 1 (length md5info))
      (error 
       "I don't understand what the protein_sequence_MD5 table is about!"
       ))
    (let* ((key (third (first md5info)))
           (sim-seeks-key (s+ "gnl|md5|" key))
           (sim-seeks-info (sim-seeks-info-from-prefixed-md5-key sim-seeks-key))
           (len (length sim-seeks-info)))
      ;; (setq *sim-seeks-info* sim-seeks-info)
      (cond 
       ((zerop len) nil)
       (t 
        ;; the hit information for the sequence represented by the 
        ;; md5 key may be found in multiple files.  
        ;; first, get the filepaths for each file number.  
        (let ((massaged-data 
               (mapcar 
                (lambda (record) 
                  (destructuring-bind (key filen seek len)
                      record
                    (let* ((local-file-path (file-from-fileno filen))
                           (absolute-file-path 
                            (s+ user::*seed-mysql-data-root* local-file-path)))
                      (list key absolute-file-path seek len)
                      )))
                sim-seeks-info
                )))
          ;; (setq *massaged-data* massaged-data)
          ;; then get the data from each file and concatenate it all together
          (apply 
           's+ 
           (mapcar 
            (lambda (massaged-record)
              (destructuring-bind (key absolute-file-path seek len)
                  massaged-record
                (declare (ignore key))
                (with-open-file (p absolute-file-path :direction :input)
                  (file-position p seek)
                  (let ((buffer (make-string len)))
                    (loop for j from 0 below len do
                          (setf (schar buffer j) (read-char p)))
                    (maybe-add-newline buffer)
                    ))))
            massaged-data
            ))))))))



;; If there is no corresponding featured id in the protein_sequence_MD5 table, 
;; this means that the md5 key we have represents a protein sequence that does
;; not exist in the seed database but in fact exists in some other databases.
;; One can determine these other databases by using the peg_synonyms table.

(defun decode-seed-crossblast-info (records-string peg)

  (flet ((md5-match (file-record) (second file-record))
         (ps-md5 (ps-table-record) (third ps-table-record))
         (add-gnl-md5-goo (md5key) (s+ "gnl|md5|" md5key))
         (strip-gnl-md5-goo (md5key) (subseq md5key 8))
         (has-gnl-md5-prefix? (md5key) 
           (string-equal "gnl|md5|" (subseq md5key 0 8))))
    
    (let* ((file-records 
            (mapcar 
             (lambda (x) (string-split x #\Tab))
             (remove-if 
              (lambda (s) (zerop (length (string-trim *whitespace* s))))
              (string-split records-string #\Newline)
              )))
           (file-records-hash 
            ;; create a hash of all the md5 key matches to the data record
            ;; but with the prefix goop stripped off
            (let ((hash (make-hash-table :test 'equal)))
              (loop for file-record in file-records 
                    as md5-match = (md5-match file-record)
                    do
                    #| Don't error until database is fixed
                    (unless (has-gnl-md5-prefix? md5-match)
                      (error "Unknown prefix for key (~S for ~S)!" 
                             (subseq md5-match 0 8) md5-match))
                    (setf (gethash (strip-gnl-md5-goo md5-match) hash)
                          file-record)
     |#
                    (IF (has-gnl-md5-prefix? md5-match)
                        (setf (gethash (strip-gnl-md5-goo md5-match) hash)
                              file-record)
                      (WARN "Unknown prefix for key (~S for ~S)!" 
                            (subseq md5-match 0 8) md5-match))

                    finally (return hash)
                    )))
           (returned-data nil))
      
      (when (plusp (hash-table-count file-records-hash))
        (let* 
            ;; take all the md5 keys and put commas in between them and
            ;; parentheses around them -- a form suitable to feed to an
            ;; 'in' query to mysql
            ((md5keys-in-mysql-in-format
              (fids-to-mysql-in-format (hash-table-keys file-records-hash)))
             ;; ps-table-records are of the form (peg-id gid md5)
             ;; where the md5 key doesn't have the prefix goo
             (ps-table-records
              (seed-query "select * from protein_sequence_MD5 where md5 in ~A" 
                          md5keys-in-mysql-in-format
                          ))
             ;; md5 keys may map to multiple peg-ids 
             (ps-records-hash 
              (let ((hash (make-hash-table :test 'equal)))
                (loop for ps-record in ps-table-records 
                      as md5key = (ps-md5 ps-record)
                      do
                      (push ps-record (gethash md5key hash))
                      finally (return hash)
                      ))))
          ;; (setq *file-records* file-records)
          ;; (setq *md5keys-in-mysql-in-format* md5keys-in-mysql-in-format)
          ;; (setq *ps-table-records* ps-table-records)
                  
          ;; now loop over every match key
          (maphash 
           (lambda (file-md5-key file-record) 
             (let ((ps-records (gethash file-md5-key ps-records-hash)))
               ;; if the match key matches seed proteins...
               (if ps-records
                   (push 
                    (list 
                     (loop 
                      for ps-record in ps-records 
                      as peg-id = (first ps-record)
                      as gid = (second ps-record)
                      ;; decode-seed-crossblast-info is only called
                      ;; by the msf stuff but is compile/loaded by all the seed 
                      ;; code, so we need the forward-funcall to suppress 
                      ;; a compiler warning
                      as orgf = (forward-funcall 'gid->orgf gid)
                      collect
                      (cond
                       ((and orgf (#^organism-loaded? orgf))
                        (let ((peg-frame 
                               (find peg-id (#^genes orgf) 
                                     :key (lambda (x) (#^seed-id x))
                                     :test 'string-equal
                                     )))
                          (list :loaded peg-id gid peg-frame orgf)
                          ))
                       (orgf (list :master-list peg-id gid nil orgf))
                       (t (list :seed-organism peg-id gid nil nil))
                       ))
                     file-record
                     )
                    returned-data
                    )
                 ;; if the match key does not match any seed proteins 
                 ;; then it is some kind of external protein reference.
                 ;; We can obtain this external information from the
                 ;; peg synonyms table.  Each record we return is of the form
                 ;; (maps-to maps-to-length synonym-id syn-length)
                 ;; where synonym-id is some kind of external identifier
                 (let* ((peg-synonyms-info 
                         (peg-synonyms-info-from-key 
                          (add-gnl-md5-goo file-md5-key)))
                        (external-records
                         (loop for (nil nil external-id nil)
                               in peg-synonyms-info
                               collect
                               (list :external-organism external-id nil nil nil)
                               )))
                   (push (list external-records file-record) returned-data)
                   ))))
           file-records-hash
           )))
      (cons peg returned-data)
      )))

;; Returns a list in the format 
;; (peg 
;;  ((matching-protein-info-a matching-protein-info-b ...) match-info-1))
;;  ((matching-protein-info-a matching-protein-info-b ...) match-info-2))
;;  ...
;;  )
;; 
;; where matching-protein-info is of the form
;; (type x y z q)
;; type is one of :loaded, :master-list, :seed-organism, or :external-organism
;; 
;; If type is :loaded
;;   x is the peg-id of the matching protein
;;   y is the gid of the genome of the matching protein, 
;;   z is the gene frame of the peg-id
;;   q is the organism frame
;;   
;; If type is :master-list, 
;;   x is the peg-id of the matching protein, 
;;   y is the gid of the genome of the matching protein, 
;;   z is nil
;;   q is the organism frame of said genome.  
;; 
;; If type is :seed-organism, 
;;   x is the peg-id of the matching protein
;;   y is the gid of the genome of the matching protein
;;   z is nil
;;   q is nil (because no frame exists because the organism is not in the 
;;     master list).
;; 
;; If type is :external-organism, 
;;   x is an identifier returned by the peg_synonyms table, which presumably
;;     identifies the entity in some external database.  
;;   y is nil
;;   z is nil
;;   q is nil

(defun crossblast-matches (peg)
  (let ((info (crossblast-info-for-peg peg)))
    (if info 
        (decode-seed-crossblast-info info peg)
      (list peg nil)
      )))
              
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; accessors into the categories table

(defun categories-timestamp (x) (eighth x))

(defun categories-type (x) (third x))

(defun categories-annotation (x) (fourth x))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#||

(defun find-all-spanning-genes (seed-id)
  (let ((gene-data 
         (seed-query
          "select id,location,contig from features where genome = ~S" seed-id
          ))
        (contig-data 
         (seed-query
          "select contig,len from contig_lengths where genome = ~S" seed-id
          ))
        (spanning-info nil))
    (loop for (id location contig) in gene-data
          do
          (unless (null (position #\, location))
            (let* ((segments (utils::string-split location #\,))
                   (parses (mapcar 'parse-single-seed-location segments))
                   (contig-length 
                    (second 
                     (find contig contig-data :test 'string-equal :key 'first)
                     )))
              (when (gene-spans-origin? parses contig-length t)
                (push (list contig id) spanning-info)
                ))))
    (when spanning-info (cons seed-id spanning-info))
    ))

(defun find-every-spanning-gene ()
  (let ((org-id-list (mapcar 'first (seed-query "select genome from genome"))))
    (remove-if 
     'null
     (loop for seed-id in org-id-list 
           for j from 1 to 2000
           collect
           (find-all-spanning-genes seed-id)
           ))))
          

(defun are-any-spanning-genes-on-non-circular-contigs (data)
  (loop for (org-id . spanning-gene-info) in data
        as orgf = (seed-id->frame org-id)
        do
        (when orgf 
          (loop for (contig-id feature-id) in spanning-gene-info
                as contig-frame = 
                (find contig-id (#^contiguous-sequences orgf) 
                      :key #^seed-id :test 'string-equal)
                do
                (when (not (#^circular contig-frame))
                  (formatt "Org: ~A, contig: ~A, spanning gene: ~A~%"
                           orgf contig-frame feature-id
                           ))))))
            
(defparameter *spanning-info* 
  '(("103690.1" ("NC_003273" "fig|103690.1.peg.5744")
                ("NC_003240" "fig|103690.1.peg.1"))
    ("10633.1" ("NC_001669" "fig|10633.1.peg.1"))
    ("10651.1" ("NC_001739" "fig|10651.1.peg.9"))
    ("10724.1" ("NC_004166" "fig|10724.1.peg.106")
               ("NC_004166" "fig|10724.1.peg.1"))
    ("10815.1" ("NC_001928" "fig|10815.1.peg.1"))
    ("10821.1" ("NC_001346" "fig|10821.1.peg.5"))
    ("10825.1" ("NC_003379" "fig|10825.1.peg.5")
               ("NC_003379" "fig|10825.1.peg.4"))
    ("10827.1" ("NC_001934" "fig|10827.1.peg.1"))
    ("10831.1" ("NC_001507" "fig|10831.1.peg.1"))
    ("10835.1" ("NC_001938" "fig|10835.1.peg.1"))
    ("10838.1" ("NC_001931" "fig|10838.1.peg.3"))
    ("10839.1" ("NC_004042" "fig|10839.1.peg.1"))
    ("10840.1" ("NC_001412" "fig|10840.1.peg.1"))
    ("10847.1" ("NC_001422" "fig|10847.1.peg.9")
               ("NC_001422" "fig|10847.1.peg.10")
               ("NC_001422" "fig|10847.1.peg.11"))
    ("10854.1" ("NC_001365" "fig|10854.1.peg.12"))
    ("10855.1" ("NC_003438" "fig|10855.1.peg.8"))
    ("10857.2" ("NC_001741" "fig|10857.2.peg.1"))
    ("10863.2" ("AB334720" "fig|10863.2.peg.1"))
    ("10864.2" ("PFDCG" "fig|10864.2.peg.1"))
    ("10869.1" ("NC_001332" "fig|10869.1.peg.9"))
    ("10870.1" ("NC_003287" "fig|10870.1.peg.10"))
    ("10872.1" ("NC_001418" "fig|10872.1.peg.9"))
    ("10876.1" ("NC_001341" "fig|10876.1.peg.4"))
    ("110944.1" ("NC_003325" "fig|110944.1.peg.3"))
    ("113194.1" ("NC_002168" "fig|113194.1.peg.3")
                ("NC_002168" "fig|113194.1.peg.4"))
    ("114777.1" ("NC_004084" "fig|114777.1.peg.98"))
    ("1148.1" ("NC_005232" "fig|1148.1.peg.3566")
              ("NC_005231" "fig|1148.1.peg.3408")
              ("NC_005230" "fig|1148.1.peg.3302")
              ("NC_005229" "fig|1148.1.peg.3170")
              ("NC_000911" "fig|1148.1.peg.3167"))
    ("117575.1" ("NC_002180" "fig|117575.1.peg.7"))
    ("12348.2" ("NC_009554" "fig|12348.2.peg.1"))
    ("12367.1" ("NC_001741" "fig|12367.1.peg.12"))
    ("12639.1" ("NC_001344" "fig|12639.1.peg.6")
               ("NC_001344" "fig|12639.1.peg.7"))
    ("127507.1" ("NC_002362" "fig|127507.1.peg.10"))
    ("127508.1" ("NC_002363" "fig|127508.1.peg.8"))
    ("1311.1" ("NC_002136" "fig|1311.1.peg.11"))
    ("134599.1" ("NC_002555" "fig|134599.1.peg.1"))
    ("1351.1" ("NC_005013" "fig|1351.1.peg.59"))
    ("1358.1" ("NC_002799" "fig|1358.1.peg.94"))
    ("1360.1" ("NC_002502" "fig|1360.1.peg.8"))
    ("139.1" ("NC_004971" "fig|139.1.peg.31"))
    ("1428.1" ("NC_004334" "fig|1428.1.peg.6")
              ("NC_005567" "fig|1428.1.peg.17"))
    ("1432.1" ("NC_004335" "fig|1432.1.peg.1"))
    ("145579.1" ("NC_002643" "fig|145579.1.peg.10"))
    ("146032.1" ("NC_003054" "fig|146032.1.peg.4"))
    ("1587.1" ("NC_002102" "fig|1587.1.peg.13"))
    ("164037.1" ("NC_004462" "fig|164037.1.peg.35"))
    ("1661.1" ("NC_005206" "fig|1661.1.peg.4"))
    ("1718.1" ("NC_003227" "fig|1718.1.peg.60")
              ("NC_001791" "fig|1718.1.peg.28"))
    ("1744.1" ("NC_002580" "fig|1744.1.peg.2"))
    ("176652.1" ("NC_003038" "fig|176652.1.peg.1"))
    ("180503.1" ("NC_003311" "fig|180503.1.peg.6"))
    ("180816.1" ("NC_003525" "fig|180816.1.peg.166"))
    ("181604.1" ("NC_003327" "fig|181604.1.peg.1"))
    ("1856.1" ("NC_002699" "fig|1856.1.peg.1"))
    ("185959.1" ("NC_003793" "fig|185959.1.peg.13"))
    ("187410.1" ("NC_004838" "fig|187410.1.peg.4381"))
    ("190304.1" ("NC_003454" "fig|190304.1.peg.1"))
    ("194445.1" ("NC_004036" "fig|194445.1.peg.7"))
    ("194948.1" ("NC_004913" "fig|194948.1.peg.167"))
    ("194949.1" ("NC_004914" "fig|194949.1.peg.170"))
    ("200913.2" ("AF527608" "fig|200913.2.peg.1"))
    ("210.1" ("NC_001756" "fig|210.1.peg.7"))
    ("216816.1" ("NC_004769" "fig|216816.1.peg.26")
                ("NC_004768" "fig|216816.1.peg.25")
                ("NC_004253" "fig|216816.1.peg.16")
                ("NC_004252" "fig|216816.1.peg.13"))
    ("220340.1" ("NC_001439" "fig|220340.1.peg.3"))
    ("220341.1" ("NC_003385" "fig|220341.1.peg.4631")
                ("NC_003384" "fig|220341.1.peg.4396"))
    ("222523.1" ("NC_005707" "fig|222523.1.peg.5844"))
    ("223314.1" ("NC_004636" "fig|223314.1.peg.1"))
    ("223316.1" ("NC_004659" "fig|223316.1.peg.1"))
    ("223317.1" ("NC_004635" "fig|223317.1.peg.1"))
    ("223321.1" ("NC_004661" "fig|223321.1.peg.1"))
    ("2242.1" ("NC_002121" "fig|2242.1.peg.1"))
    ("2248.1" ("NC_004531" "fig|2248.1.peg.7"))
    ("2371.1" ("NC_002092" "fig|2371.1.peg.1"))
    ("243232.1" ("NC_001733" "fig|243232.1.peg.1774"))
    ("2443.1" ("NC_001371" "fig|2443.1.peg.21"))
    ("244590.1" ("NC_005265" "fig|244590.1.peg.34"))
    ("244590.2" ("NC_005265" "fig|244590.2.peg.1"))
    ("2527.1" ("NC_001705" "fig|2527.1.peg.1"))
    ("2562.1" ("NC_001375" "fig|2562.1.peg.1"))
    ("2577.1" ("NC_001377" "fig|2577.1.peg.1"))
    ("2613.1" ("NC_001391" "fig|2613.1.peg.5"))
    ("266835.1" ("NC_002679" "fig|266835.1.peg.7066")
                ("NC_002679" "fig|266835.1.peg.7065"))
    ("272561.1" ("NC_000117" "fig|272561.1.peg.895"))
    ("272844.1" ("NC_000868" "fig|272844.1.peg.1"))
    ("273057.1" ("NC_002754" "fig|273057.1.peg.2977"))
    ("273075.1" ("NC_002578" "fig|273075.1.peg.1482"))
    ))

||#