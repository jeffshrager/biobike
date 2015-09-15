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

;; "http://www.phantome.org/~jmyers/test15.php?page=Annotations&feature=fig|103690.1.peg.146"
  
;; "fig|103690.1.peg.1462"
;; #$A-7120.alr1152

;; old page
;; http://www.phantome.org/PhageSeed/seedviewer.cgi?page=Annotation&feature=fig|103690.1.peg.1462&user=


;; proposed test gene: "fig|543939.2.peg.1", aka #$Sputnik.Sputnik_Gp01
	
;; Also Sputnik_Gp21

;; In the VPL, bring down other-commands --> annotate-seed-feature
;; and use the above seed id

(defparameter *annotation-test-page* "/~jmyers/test48.php")
#+notyet
(defparameter *annotation-test-page* "/PhageSeed/annotations.php")

(defparameter *change-subsystem-role-page* "/~jmyers/csrp.php")

(defparameter *seed-annotation-domain* "http://www.phantome.org")

(defun create-seed-annotation-url (seed-gene-frame)
  (create-seed-annotation-url-aux 
   (#^seed-id seed-gene-frame) :gf seed-gene-frame))

(defun create-seed-annotation-url-aux (fid-string &key (gf nil))
  (let ((domain *seed-annotation-domain*)
        (page *annotation-test-page*))
    (formatn "~A~A?~A" domain page 
             (seed-annotation-url-args fid-string :gf gf)
             )))

(defun account-exists?-no-output (username)
  (let ((result nil))
    (with-output-to-string (p) 
      (let ((*standard-output* p)
            (*error-output* p))
        (setq result (wb::account-exists? username :by :login))
        ))
    result
    ))
       
  
(defun seed-annotation-url-args 
       (fid-string &key (orgn "testorg") (contign "testcontig") (gf nil))
  (let* ((username (string wb::*username*))
         (sessionid (string wb::*sessionid*))
         (orgname (if gf (fname (#^organism gf)) orgn))
         (contigname (if gf (fname (#^contiguous-sequence gf)) contign))
         (split-gene-name 
          (if gf (string-split (#^fname gf) #\.) (list "testorg" "testgene")))
         (org-prefix (first split-gene-name))
         (abbreviated-gene-name (second split-gene-name))
         (genetic-name (if gf (#^genetic-name gf) nil))
         (port wb::*weblistener-port*)
         (account (account-exists?-no-output username))
         (email (wb::user-email account))
         (fullname (wb::user-fullname account))
         (affiliation (getf (wb::user-other-stuff account) :affiliation))
         (protein (if gf (first (#^proteins gf)) nil))
         (protein-sequence (if protein (extract-sequence protein) nil))
         (protein-length (if protein-sequence (length protein-sequence) 0))
         (mw 
          (if gf 
              (handler-case 
                  (if protein 
                      (molecular-weight-of protein-sequence) 
                    (molecular-weight-of 
                     (translate-d/rna-to-aa (extract-sequence gf))
                     ))
                (error () 0)
                )
            0
            )))
    (string-join 
     (list 
      (formatn "feature=~A" fid-string)
      (formatn "username=~A" username)
      (formatn "sessionid=~A" sessionid)
      (formatn "orgn=~A" orgname)
      (formatn "contig=~A" contigname)
      (formatn "orgprefix=~A" org-prefix)
      (formatn "gname=~A" abbreviated-gene-name)
      (formatn "genetic-name=~A" (or genetic-name "unknown"))
      (formatn "port=~A" port)
      (formatn "email=~A" (or email "unknown"))
      (formatn "fullname=~A" (or fullname "unknown"))
      (formatn "affiliation=~A" (or affiliation "unknown"))
      (formatn "proteinlength=~A" protein-length)
      (formatn "mw=~A" mw)
      )
     "&"
     )))

(defun vpl-execution-check ()
  (unless (forward-package-funcall :vpl :vpl-executing?)
    (error "You can only execute this from the VPL!")
    ))

(defun annotate-seed-feature-aux (seed-feature-frame &key (test nil))
  (vpl-execution-check)
  (let ((account-record (account-exists?-no-output (string wb::*username*))))
    (unless account-record
      (error "No user account for ~A!" wb::*username*)))
  (flet ((annotation-window (url) 
           (forward-package-funcall 
            :vpl "SHOW-VPL-POPUP-URL-WINDOW"
            url
            :relative-p 0
            :width "800px" :height "800px" :menubar "yes"
            )))
    (if (and test (stringp seed-feature-frame))
        (annotation-window (create-seed-annotation-url-aux seed-feature-frame))
      (let ((gene-frame 
             (cond
              ((stringp seed-feature-frame) (seed-id->frame seed-feature-frame))
              ((frames::isframe? seed-feature-frame) seed-feature-frame)
              (t 
               (error
                "Must provide a string naming a seed gene or a seed gene frame"
                )))))
        (cond
         ((null gene-frame)
          (error 
           (one-string-nl
            "The name you provided, '~A', is not recognizable as"
            "a seed gene name."
            )
           seed-feature-frame
           ))
         ((not (typep gene-frame 'bio::seed-gene))
          (error  
           (one-string-nl
            "The object ~A, is not a seed gene!  You must provide the name"
            "of a seed gene or a frame representing that gene."
            )
           gene-frame
           )))
        (annotation-window (create-seed-annotation-url gene-frame))
        ))))

(defun change-subsystem-role-aux (to genes)
  (vpl-execution-check)
  (let ((account-record (account-exists?-no-output (string wb::*username*))))
    (unless account-record
      (error "No user account for ~A!" wb::*username*))
    (when (wb::pseudo-account? account-record)
      (error 
       "You are not a registered user, you cannot use this functionality!"
       )))
  (let ((seed-id-string
         (cond
          ((typep genes 'gene) (#^seed-id genes))
          ((typep genes 'frame) 
           (error "The frame you provided, ~A, is not a gene frame!" genes))
          ((listp genes) 
           (cond
            ((every (lambda (x) (typep x 'gene)) genes)
             (string-join (mapcar #^seed-id genes) #\,))
            ((every (lambda (x) (typep x 'frame)) genes)
             (error "All the frames must be seed gene frames!"))
            ((every 'stringp genes) (string-join genes #\,))
            (t (error "You must provide a list of seed gene frames!"))
            ))
          ((stringp genes) genes)
          (t (error "The object, ~A, is not a gene!" genes))
          )))
    (flet ((change-subsystem-report-window (url) 
             (forward-package-funcall 
              :vpl "SHOW-VPL-POPUP-URL-WINDOW"
              url
              :relative-p 0
              :width "800px" :height "800px" :menubar "yes"
              )))
      (change-subsystem-report-window 
       (create-change-subsytem-role-url to seed-id-string)
       ))))

(defun create-change-subsytem-role-url (to seed-id-string)
  (let* ((username (string wb::*username*))
         (sessionid (string wb::*sessionid*))
         (port wb::*weblistener-port*)
         (account (account-exists?-no-output username))
         (email (wb::user-email account))
         (fullname (wb::user-fullname account))
         (affiliation (getf (wb::user-other-stuff account) :affiliation))
         (domain *seed-annotation-domain*)
         (page *change-subsystem-role-page*))
    (formatn
     "~A~A?~A"
     domain page 
     (string-join 
      (list 
       (formatn "features=~A" (url-safe-string seed-id-string))
       (formatn "newrole=~A" (url-safe-string to))
       (formatn "username=~A" username)
       (formatn "sessionid=~A" sessionid)
       (formatn "port=~A" port)
       (formatn "email=~A" (or email "unknown"))
       (formatn "fullname=~A" (or fullname "unknown"))
       (formatn "affiliation=~A" (or affiliation "unknown"))
       )
      "&"
      ))))
  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(publish 
 :path wb::*annotation-modified-request-url* 
 :content-type cl-user::*html-publish-content-type*
 :function
 (lambda (req ent) 
   (let* ((input (request-query req))
          (package-name (url-parameter-value :pkg input))
          (package-symbol (keywordize package-name))
          (seed-id (url-parameter-value :seed-id input))
          (title "Annotation URL response"))
     (wb::log-system-event 
      "Annotation update request.  Session ID = ~A, seed id = ~A"
      package-symbol seed-id)
     (wb::with-http-response-and-body 
         (req ent)
       (handler-case 
           (wb::with-protected-globals-bound 
               package-symbol
             (multiple-value-bind (gene-frame error-string)
                 (sync-seed-gene seed-id)
               (html
                (:princ utils::*transitional-html-doctype-header*)
                (:html
                 (:head (:title title))
                 (:body 
                  (if gene-frame 
                      (progn
                        (html
                         (:princ-safe
                          (formatn 
                           "OK.  Updated ~A (seed-id: ~A)"
                           gene-frame seed-id)))
                        (wb::log-system-event 
                         (one-string
                          "Seed update message from annotation page. "
                          "~A: Updated ~A (seed-id: ~A)")
                         wb::*username* gene-frame seed-id))
                    (progn
                      (html
                       (:princ-safe
                        (formatn 
                         "OOPS.  Could not update ~A.  Actual error: ~A"
                         seed-id error-string
                         )))
                      (wb::log-system-event
                       (one-string
                        "Seed update from annotation page. "
                        "~A: Could not update ~A.  Actual error: ~A")
                       wb::*username* seed-id error-string
                       ))))))))
         (error 
          (c)
          (html 
           (:head (:title title))
           (:body
            (:princ-safe 
             (formatn 
              (one-string
               "OOPS.  Unable to execute annotation update with "
               "user session = ~A.  Actual error: ~A")
              package-symbol c
              ))))))))))

(defparameter *categories-codes*
  '(
    "REG" ;; A/Regulation
    "MEM" ;; Membrane spans
    "SIG" ;; Signal sequence
    "ANN" ;; Main annotation
    "GEN" ;; Genetic name
    "ALI" ;; Aliases
    "FUN" ;; A/Biochemical Function
    "MUT" ;; A/Mutants
    "SBR" ;; Subsystem role
    "TOC" ;; To coordinate
    "FRM" ;; From coordinate
    "ROL" ;; A/Physiological Role
    "STR" ;; A/Operon Structure
    "OTH" ;; A/Other
    ))

;; Mode can be either :current (just the most recent update to the field)
;; or :all (the entire history).  Types can be either :all (all info)
;; or a particular type as a keyword or a string (e.g., "MEM" or :mem), 
;; or a list of types

;; If the :MODE is :ALL then the function returns a list of lists of lists.
;; Each outer sublist is information for a single type.  Each inner sublist is
;; a list of all the records for that type.  Even if there is only
;; one type or one record for a type, you still get a list.  

;; If the :MODE is :CURRENT then the function returns a list of lists.  
;; Each sublist is a record for a particular type; one record per type.  

(defun retrieve-annotation-information (gid &key (mode :current) (types :all))
  (let ((gene-annotation-info 
         ;; ("id" "gene_id" "type_ID" "annotation" "source_id" "justification"
         ;; "just_link" "time" "entry_num" "jfrom")
         (seed-query "select * from categories where gene_id = ~S" gid)))
    (labels ((remove-types-except (types)
               (setq 
                types 
                (loop for type in types 
                      collect 
                      (if (or (stringp type) (keywordp type)) 
                          (string type)
                        (error "Illegal value ~S for :types keyword!" type)
                        )))
               (setq gene-annotation-info
                     (remove-if-not
                      (lambda (x) 
                        (member 
                         (categories-type x) types :test 'string-equal))
                      gene-annotation-info
                      ))))
      (when gene-annotation-info 
        (cond
         ((eq types :all) nil)
         ((or (stringp types) (keywordp types)) 
          (remove-types-except (list types)))
         ((listp types) (remove-types-except types))
         (t (error "Illegal value ~S for :types keyword!" types))
         )
        ;; sort by the timestamp 
        (setq 
         gene-annotation-info 
         (stable-sort 
          gene-annotation-info 'string-lessp :key 'categories-timestamp))
        ;; sort by the type
        (setq gene-annotation-info 
              (stable-sort 
               gene-annotation-info 'string-lessp :key 'categories-type))
        (let* ((extracted-types 
                (purge-duplicates 
                 (mapcar 'categories-type gene-annotation-info)
                 :test 'string-equal
                 ))
               (records 
                (loop for type in extracted-types 
                      collect
                      (remove-if-not 
                       (lambda (x) (string-equal type (categories-type x)))
                       gene-annotation-info
                       ))))
          (cond
           ((eq mode :all) 
            (values 
             records
             (mapcar (lambda (x) (categories-type (first x))) records)
             ))
           ((eq mode :current)
            (values 
             (mapcar 'lastelem records)
             (mapcar (lambda (x) (categories-type (first x))) records)
             ))
           (t (error "Illegal value ~S for :mode keyword!" mode))
           ))))))

(defun all-annotated-figs-with-evidence (&KEY any-evidence no-duplicates)
 ;; ANY-EVIDENCE: If NIL, then evidence must be experimental ("Lab")
 ;;               Otherwise, any human annotation will do
 ;; NO-DUPLICATES: If T, then duplicate genes removed
 (LET ((result
  (mapcar 
   'bio::seed-id->frame
   (mapcar 
    'second
	(IF any-evidence
	    (APPEND
		  (bio::seed-query 
             "select * from categories where type_ID = 'ANN' and jfrom = 'Lab'")
		  (bio::seed-query 
             "select * from categories where type_ID = 'ANN' and jfrom = 'Com'")
		  (bio::seed-query 
             "select * from categories where type_ID = 'ANN' and jfrom = 'Seq'"))
        (bio::seed-query 
          "select * from categories where type_ID = 'ANN' and jfrom = 'Lab'"))
    ))))
  (IF no-duplicates
      (DELETE-DUPLICATES result :TEST 'EQUAL)
	  result)
 ))

(defun update-biobike-using-annotation-table (&key (since nil))
  (flet ((update-them (gids) 
           (loop for gid in gids do (sync-seed-gene gid :commit? nil))
           #-:lispworks
           (db.ac::commit)
           gids
           ))
    (cond
     ;; update every gene ever annotated 
     ((null since) 
      (let ((gids 
             (mapcar 
              'first 
              (seed-query "select distinct gene_id from categories")
              )))
        (update-them gids)
        ))
     ;; list needs to be of the form 
     ;; (year month day &optional hour minute second)
     ((listp since) 
      (destructuring-bind 
          (year month day &optional (hour 0) (minute 0) (second 0))
          since
        (let ((universal-time 
               (encode-universal-time second minute hour day month year)))
          (update-biobike-using-annotation-table :since universal-time)
          )))
     ;; if it's a number it is assumed to be a lisp universal time.
     ;; Convert to unix timestamp and then find every gene that's been annotated
     ;; since that date
     ((numberp since) 
      (let ((unix-timestamp-string 
             (make-timestamp-string 
              :universal-time since :mode :unix-timestamp)))
        (let ((gids 
               (mapcar
                'first
                (seed-query
                 (one-string
                  "select distinct gene_id from categories where "
                  "strcmp(time,~S) = 1"
                  )
                 unix-timestamp-string
                 ))))
          (update-them gids)
          ))))))
        
              
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#||

(defun find-all-bad-seed-aliases (&key (org-seed-ids nil))
  (when (null org-seed-ids)
    (setq org-seed-ids (mapcar 'first (seed-query "select genome from genome")))
    )
  (with-open-file 
      (p "/home/jpmassar/bad-aliases.txt" 
         :direction :output :if-exists :supersede)
    (loop 
     for org in org-seed-ids 
     with bad-features = nil
     as any? = nil
     do
     (let ((data 
            (seed-query
             "select id,aliases from features where genome = ~S"
             org
             )))
       (loop for (id aliases) in data
             do
             (when (and (not (find #\, aliases)) (find #\; aliases))
               (when (null any?) 
                 (format p "~%Organism: ~A~%" org))
               (setq any? t)
               (format p "  feature: ~A, aliases: ~A~%" id aliases)
               (push (list id aliases) bad-features)
               )))
     finally
     (return bad-features)
     )))
                  
||#    
          
           
   