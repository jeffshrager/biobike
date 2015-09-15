;;; -*- mode: Lisp; Syntax: Common-Lisp; Package: bio; -*-

(in-package :bio)

;; Author: Mark Slupesky

;; Testing Biotools

;; CROSS-BLAST uberprims.lisp
;; still need factual checks using http://www.ncbi.nlm.nih.gov/blast/bl2seq/wblast2.cgi?0
;; CLUSTAL uberprims.lisp 
;; MEME uberprims.lisp
;; GOOGLEAPI uberprims.lisp
;; RNAZ rnaz.lisp
;; PHYLIP phylip.lisp
;; PUBMED pubmed.lisp -- not sure "representing search terms" means

;; GNUPLOT gnuplot.lisp -- how should we test gnuplot, it outputs a picture
;; SEEGRAPH uberprims.lisp  -- same deal 
;; R rtable.lisp -- writes out stuff to a file


;; checks that cross-blast-long outputs a list of blast frames when given lists of gene frames

;; CROSS-BLAST-LONG 
;; still need factual tests, and we need to make sure we test 
;; all the blast options as defined in the 'program' link at 
;; http://www.ncbi.nlm.nih.gov/blast/bl2seq/wblast2.cgi?0

;; checks that cross-blast-long outputs a list of blast frames 
;; when given lists of gene frames

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Confirming the existence of directories

(defbiotooltest 
 confirm-blast-dir
 (ecase (os?)
   (:windows t)
   (:unix 
    (not (null (pathnamep (probe-file user::*blast-executable-toplevel-dir*))))
    ))
 t
 )

(defbiotooltest 
 confirm-clustal-dir
 (ecase (os?)
   (:windows t)
   (:unix 
    (not (null (pathnamep (probe-file user::*clustal-executable-dir*))))
    ))
 t
 )

(defbiotooltest 
 confirm-r-dir
 (ecase (os?)
   (:windows t)
   (:unix 
    (not (null (pathnamep (probe-file user::*R-executable-dir*))))
    ))
 t
 )

(defbiotooltest 
 confirm-pyhlip-dir
 (ecase (os?)
   (:windows t)
   (:unix 
    (not (null (pathnamep (probe-file user::*phylip-executable-dir*))))
    ))
 t
 )

(defbiotooltest 
 confirm-meme-dir
 (ecase (os?)
   (:windows t)
   (:unix 
    (not (null (pathnamep (probe-file user::*meme-executable-dir*))))
    ))
 t
 )

(defbiotooltest 
 confirm-rnaz-dir
 (ecase (os?)
   (:windows t)
   (:unix 
    (not (null (pathnamep (probe-file (external-executable-path "RNAz")))))
    ))
 t
 )

(defbiotooltest 
 confirm-gnuplot-path
 (ecase (os?)
   (:windows t)
   (:unix 
    (not (null (pathnamep (probe-file user::*gnuplot-path*))))
    ))
 t
 )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                

(defbiotooltest crossblast-long-1 
  (let ((result (cross-blast-long (list (g1)) (list (g2)))))
    (loop for frame in result 
          when (not (eq (generic-instanceof frame) #$BlastOutput))
          return frame))
  nil)

(defbiotooltest crossblast-long-2
  (let ((result (cross-blast-long (list (g1) (g3)) (list (g2) (g4)))))
    (loop for frame in result 
          when (not (eq (generic-instanceof frame) #$BlastOutput))
          return frame))
  nil)

;; checks that cross-blast-long outputs a list of blast frames 
;; when given strings of gene sequences
(defbiotooltest crossblast-long-3
  (let ((result (cross-blast-long (list human-cftr) (list orangutan-cftr) 
                                  :force-type #$gene)))
    (loop for frame in result 
          when (not (eq (generic-instanceof frame) #$BlastOutput))
          return frame))
  nil)

(defbiotooltest crossblast-long-4
  (let ((result (cross-blast-long 
                 (list human-cftr chimpanzee-cftr) 
                 (list orangutan-cftr gorilla-cftr) 
                 :force-type #$gene)))
    (loop for frame in result 
          when (not (eq (generic-instanceof frame) #$BlastOutput))
          return frame))
  nil)


;; CLUSTALW
;; http://www.ebi.ac.uk/clustalw/

;; checks that clustalw outputs a list of clustal frames when given gene frames
(defbiotooltest clustalw1
  (let ((result (align (list (g1) (g2) (g3)))))
    (eq (generic-instanceof result) #$ClustalWResult))
  t)

;; checks that clustalw outputs a list of clustal frames when 
;; given strings of gene sequences
(defbiotooltest clustalw2
  (let ((result (align (list human-cftr orangutan-cftr chimpanzee-cftr))))
    (eq (generic-instanceof result) #$ClustalWResult))
  t)


;; run some factual checks on clustal

;; compare consensus of 4 genes against EBI
(defbiotooltest clustalw3
  (let ((consensus 
         (#^consensus (align (list human-bmp7 pig-bmp7 rabbit-bmp7 sheep-bmp7)))))
    (string= consensus EBI-human-pig-rabbit-sheep-bmp-consensus))
  t)

(defbiotooltest clustalw4
  (let ((consensus 
         (#^consensus (align (list chimpanzee-cftr human-cftr orangutan-cftr gorilla-cftr)))))
    (string= consensus EBI-chimpanzee-human-orangutan-gorilla-consensus))
  t)


;; meme
;; http://meme.sdsc.edu/meme/website/meme.html

;; MEME


;; we can create a lot more meme "factual" tests using the examples 
;; and MEME program at
;; http://meme.sdsc.edu/meme/website/meme.html 

;; checks that MEME outputs a list of lists of motif frames when 
;; given gene frames

(defbiotooltest 
 meme0
 (let ((org (first bio::*loaded-organisms*)))
   (if (< (length (#^genes org)) 18)
       t
     (let* ((g2 (subseq (#^genes org) 6 8))
            (g3 (subseq (#^genes org) 10 12))
            (g4 (subseq (#^genes org) 16 18))
            (output (bio::run-meme (list g2 g3 g4) :html? nil :labels? nil)))
       (and output
            (loop for lst in output always (listp lst))
            (loop for frame in (flatten output) 
              always (eq (generic-instanceof frame) #$motif)
              )))))
 t
 )
       
;; checks that MEME outputs a list of lists of motif frames when 
;; given strings of gene sequences
(defbiotooltest meme2
  (let ((output 
         (bio::run-meme
          (list orangutan-cftr chimpanzee-cftr human-cftr)
          :html? nil :labels? nil)))
    (and output
         (loop for lst in output always (listp lst))
         (loop for frame in (flatten output) 
               always (eq (generic-instanceof frame) #$motif))))
  t)

;; takes two protein sequences (strings) and makes sure the
;; motifs returned are in fact the right ones.  
(defbiotooltest meme3
  (let* ((motif-frame 
          (caar
           (bio::run-meme 
            (list kinase-prot1 kinase-prot2) 
            :labels? nil :target-type :protein :return 1 :html? nil)))
         (match-frame1 (first (#^matches motif-frame)))
         (match-frame2 (second (#^matches motif-frame))))
    (and (string= (#^sequence match-frame1) kinase-match2)
         (string= (#^sequence match-frame2) kinase-match1)))  t)


;; google-search

#+hangs-randomly
(defbiotooltest google1 
  (let ((result (google-search "biolingua")))
    (loop for frame in result 
          always (eq (generic-instanceof frame) #$google-result)))
  t)

#+hangs-randomly
(defbiotooltest google2 
  (let ((result (google-search (list "biolingua" "biobike"))))
    (loop for frame in result 
          always (eq (generic-instanceof frame) #$google-result)))
  t)


(defbiotooltest rnaz1
  (flet ((shorten (s n) (subseq s 0 n)))
    (let* ((human (shorten human-cftr 200))
           (orangutan (shorten orangutan-cftr 200))
           (chimpanzee (shorten chimpanzee-cftr 200))
           (temp-frame
            (run-rnaz (list (list "human" human)
                            (list "orangutan" orangutan)
                            (list "chimpanzee" chimpanzee))))
           (params (slotv temp-frame #$parameters))
           (results (slotv temp-frame #$results)))
      (list
       (if (not params) 
           :error 
         ;; the #$parameters value is an assoc list of names and values
         ;; both of which are strings
         (loop for string in (flatten params)
               when (not (stringp string))
               return string))
       ;; The #$results value is a list of lists, 
       ;; each sublist being a list of four strings
       (if (not results) :error
         (loop for l in results 
               when (or (not (= (length l) 4))
                        (loop for s in l when (not (stringp s)) return s))
               return l)))))
  '(nil nil)
  :comparison 'equal
  )

;; make sure when the size of the sequence is greater than 400 we get
;; an error
(defbiotooltest rnaz1a
  (handler-case
      (progn
        (run-rnaz (list (list "human" human-cftr)
                        (list "orangutan" orangutan-cftr)
                        (list "chimpanzee" chimpanzee-cftr)))
        nil)
    (error () t))
  t
  )
     
(defbiotooltest rnaz2
  (let* ((temp-frame (run-rnaz *rnaz-test-data*))
         (params (slotv temp-frame #$parameters))
         (results (slotv temp-frame #$results)))
    (list
     ;; the #$parameters value is an assoc list of names and values
     ;; both of which are strings
     (if (not params) 
         :error
       (loop for string in (flatten params)
             when (not (stringp string))
             return string))
     
     ;; The #$results value is a list of lists, 
     ;; each sublist being a list of four strings
     (if (not results)
         :error
       (loop for l in results 
             when (or (not (= (length l) 4))
                      (loop for s in l when (not (stringp s)) return s))
             return l))))
  '(nil nil)
  :comparison 'equal)
           
#+maybe-later          
(defbiotooltest rtable-1 
  (with-temp-file-in (rtable-file "/usr/local/bioetc/" :on-delete-error :error)
                     (table-data-to-simple-r-format 
                      "/usr/local/bioetc/data/a29413/genome/genome.tbl"
                      rtable-file))
  t)
    

(defbiotooltest pubmed-query-1
  (let ((result (pubmed-query (list "biolingua"))))
    (loop for frame in result 
          always (eq (generic-instanceof frame) #$pubmed-result)))
  t)


;;phylip


(defbiotooltest phylip-1 
  (let ((s1 '(a "acgtacgtacgt"))
        (s2 '(b "cagtcagtcagt"))
        (s3 '(c "catggacttgac"))
        (s4 '(d "gactgatcgatc"))
        (s5 '(e "acgtacgttttt")))
    (run-phylip (list s1 s2 s3 s4 s5) :seqfn 'second :labelfn 'first)
    t)
  t)
  
