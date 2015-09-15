;;; -*- Package: biolisp; mode: lisp; base: 10; Syntax: Common-Lisp; -*-

;;; Copyright (c) 2002-2003 by Jeff Shrager, Mike Travers, JP Massar.
;;; All rights reserved.

(in-package :bio)

;;; This software is made avilable for EDUCATIONAL PURPOSES
;;; ONLY, and WITHOUT ANY WARRANTY, express or implied, of
;;; its merchantability or fitness for any particular purpose. 


(defvar blast1)
(defparameter first-query
    (one-string
     "http://www.ncbi.nlm.nih.gov/blast/Blast.cgi?"
     "QUERY=555"
     "&DATABASE=nr"
     "&HITLIST_SIZE=10"
     "&FORMAT_TYPE=HTML"
     "&FILTER=L"
     "&EXPECT=0.00001"
     "&PROGRAM=blastn"
     "&CLIENT=web"
     "&SERVICE=plain"
     "&NCBI_GI=on"
     "&PAGE=Nucleotides"
     "&CMD=Put"))

(defun do-blast1-query ()
  (prog1
      (lsetq blast1 (web-page-contents first-query))
    (print (subseq blast1 (search "QBlastInfoBegin" blast1)))))
   
;;; You'll need to cut and paste the RID that gets printed out
;;; when you run the above function, and pass it into the
;;; DO-RESULT0-QUERY function like so:

;;; (do-result0-query '1044584572-020243-29658)
;;; (i.e, put the quote mark at the beginning, and use the RID you
;;; got instead of the one in this text).

;;; Once you've done this, the symbol variable 'rid' will be set to 
;;; the RID.  So now you will be able to just type

;;; (do-result1-query rid)

(defvar rid)
(defvar result0 nil)
(defvar result1 nil)
(defun do-result0-query (rid-symbol)
  (setq rid rid-symbol)
  (lsetq result0
    (web-page-contents 
     (one-string
      "http://www.ncbi.nlm.nih.gov/blast/Blast.cgi?"
      "CMD=Get"
      "&RID=" (string rid-symbol)
      "&ALIGNMENTS=0"))))
(defun do-result1-query (rid-symbol)
  (setq rid rid-symbol)
  (lsetq result1
    (web-page-contents 
     (one-string
      "http://www.ncbi.nlm.nih.gov/blast/Blast.cgi?"
      "CMD=Get"
      "&RID=" (string rid-symbol)
      "&ALIGNMENTS=0"))))
 
(defun extract-blast-info (web-page-contents)
  
  ;; Find the position of the surrounding markers (QBlastInfoBegin, etc.)
  
  (let ((Begin-position (search "QBlastInfoBegin" web-page-contents))
        (End-position (search "QBlastInfoEnd" web-page-contents)))
    
    ;; If either one is missing, skip it and return nothing!
    
    (when (and Begin-position End-position)
      
      ;; Extract the part between the markers, bind it to a stream
      ;; and read each line out of it, until you hit the end.
      
      (loop with stream = (make-string-input-stream 
                           (subseq web-page-contents 
                             Begin-position 
                             End-position))
          as line = (read-line stream nil nil)
          with result = nil ; This will collect the resulting parsed values.
          until (null line)
            
            ;; This turns this: "&<TAB>LABEL=VALUE" to this: (LABEL "VALUE")
            
          do (let ((=pos (position #\= line)))
               (when =pos 
                 (push (list (read-from-string (subseq line 1 =pos))
                         (string-trim " " (subseq line (1+ =pos))))
                   result)))
            
          finally 
            (progn (close stream) (return result))
            ))))

(defun do-matching-sequence-query ()
  (web-page-contents 
   (one-string
    "http://www.ncbi.nlm.nih.gov/entrez/query.fcgi?"
    "cmd=Retrieve"
    "&db=Nucleotide"
    "&list_uids=00000555"
    "&dopt=GenBank")))


;;; TEST-STRING is equivalent to the variable 's' in the tutorial.  
;;; (Here we give it a more mnemonic name.)
;;; So after the discussion about brokets,
;;; where the following examples are shown:

;;; (broket-positions s)
;;; (setq x (get-e-value s))
;;; (parse-blast-match s)

;;; you can do

;;; (broket-positions test-string)
;;; (setq x (get-e-value test-string))
;;; (parse-blast-match test-string)

;;; and get the same result.

(defparameter test-string
  (one-string
   "<a href=\"http://www.ncbi.nlm.nih.gov/entrez/query.fcgi?"
   "cmd=Retrieve"
   "&db=Nucleotide"
   "&list_uids=18390198"
   "&dopt=GenBank\" >gb|AC092496.3|</a>  "
   "Bos taurus clone RP42-394P20, "
   "complete sequence   <a href = #18390198>289</a>   3e-75"))


(defun broket-positions (string)
  (loop for current-position from 0 by 1
      with <-pos = nil
      as char across string
      when (char-equal #\< char)
      do (setq <-pos current-position)
      when (char-equal #\> char)
      collect (list <-pos current-position)))

;;; This is a version of broket-positions which catches syntax
;;; errors.  It is not part of the tutorial.
;;; It is also written in a different style, using a DO*
;;; looping construct which has a more 'lispy' syntax than the
;;; original LOOP looping form above, which has a much more
;;; 'English-reading-style' syntax.  The nice thing about Lisp is
;;; that you can use one or the other, or even roll your own (with macros!).

(defun safe-brocket-positions (string)
  (let ((<-pos nil) (len (length string)) (result nil) char)
    (do* ((curpos 0 (1+ curpos))) 
         ((>= curpos len))
      (setq char (char string curpos))
      (cond
       ((char= #\< char)
        (unless (null <-pos) (error "Found two consecutive <'s"))
        (setq <-pos curpos))
       ((char= #\> char)
        (unless <-pos (error "Found > without preceding <"))
        (push (list <-pos curpos) result)
        (setq <-pos nil))))
    (when <-pos (error "Found < without matching >"))
    (nreverse result)))


(defun read-e-value (s)
  (let ((e-pos (position #\e s))) ; look for a exponential (scientific) form.
    (cond 
     ((null e-pos) ; no e... just read the number straight!
      (list (read-from-string s) 0.0)) ; Return the "weird" form
     (t ; otherwise we have to produce the fancy parse...
      (list (read-from-string (subseq s 0 e-pos))
        (read-from-string (subseq s (1+ e-pos))))))))

(defun get-e-value (s)
  (read-e-value (subseq s (1+ (second (first (last (broket-positions s)))))))
  )

(defun parse-blast-match (s)
  (let* ((brokets (broket-positions s))
	 (e-value 
	  (read-e-value (subseq s (1+ (second (first (last brokets)))))))
	 (accession-start (+ 1 (second (first brokets))))
	 (accession-end (first (second brokets)))
	 (accession (subseq s accession-start accession-end))
	 (match-start (+ 1 (second (second brokets))))
	 (match-end (first (third brokets)))
	 (match (subseq s match-start match-end))
	 )
    (list 
     (list 'accession accession)
     (list 'match match)
     (list 'e-value e-value)
     )))


#|

;;; This is the original version of EXTRACT-BLAST-HITS as defined
;;; and used in the tutorial.  It is commented out because a subsequent
;;; version, with the same name (but with somewhat different semantics),
;;; is also defined (see below).

;;; To make the first tutorial example using EXTRACT-BLAST-HITS work
;;; you need to cut and paste either this definition or the definition
;;; in the tutorial into your lisp, because the other one is the current
;;; definition.  

;;; Then, to make the second example work, you need to cut and paste
;;; the 2nd definition below or the 2nd definition from the tutorial
;;; into Lisp.

(defun extract-blast-hits (web-page-contents)
  (let ((stream (make-string-input-stream web-page-contents)))
    ;; Skip until we find the target line: 
    ;; "Sequences producing significant alignments:"
    (loop as line = (read-line stream nil nil)
        until (or (null line)
                (and (> (length line) 43)
                  (string-equal 
                   (subseq line 0 43) 
                    "Sequences producing significant alignments:"))))
    ;; Okay, now skip one (the blank line!) 
    ;; -- stop here if we hit the end of the file!
    (when (read-line stream nil nil)
      ;; And start parsing until we hit the </pre> line:
      (loop as line = (read-line stream nil nil)
          until (string-equal "</PRE>" line)
          collect (parse-blast-match line)
		  ))))

|#

(defun extract-blast-hits (url)
  (with-open-web-page (stream url)
    ;; Skip until we find the target line: 
    ;; "Sequences producing significant alignments:"
    (loop as line = (read-line stream nil nil)
        until (or (null line)
		  (and (> (length line) 43)
		       (string-equal 
			(subseq line 0 43) 
			"Sequences producing significant alignments:")))
	      )		 
    ;; Okay, now skip one (the blank line!)
    ;; -- stop here if we hit the end of the file!
    (when (read-line stream nil nil)
      ;; And start parsing until we hit the </pre> line:
      (loop as line = (read-line stream nil nil)
          until (string-equal "</PRE>" line)
          collect 
	    (parse-blast-match line)
	    ))))


(defun do-extract-blast-hits (&optional (rid-symbol rid))
  (extract-blast-hits 
   (one-string 
    "http://www.ncbi.nlm.nih.gov/blast/Blast.cgi?"
    "CMD=Get"
    "&RID=" (string rid-symbol))))
  


(defun blast-query (query)
  (format nil "http://www.ncbi.nlm.nih.gov/blast/Blast.cgi?~a" query))


(defun submit-blast-query (query)
  ;; Submit the query and parse-out the RID from the reply.
  (let ((rid (assocadr 'rid (extract-blast-info (web-page-contents (blast-query query))))))
    (format t "The query has been assigned RID ~a... Waiting for it to become ready!" rid)
    ;; Wait for ready, sleeping a little each time so as not to overload NCBI!
    (loop until 
          (not (string-equal "WAITING" 
                 (assocadr 
                  'status 
                  (extract-blast-info 
                   (web-page-contents 
                    (blast-query 
		     (format nil "CMD=Get&RID=~a&ALIGNMENTS=0" rid)))))))
        do (format t "#") (force-output) (sleep 5))
    ;; Okay, should be ready; Let's get the results.
    (extract-blast-hits 
     (blast-query (format nil "CMD=Get&RID=~a&ALIGNMENTS=0" rid)))))

(defun do-submit-blast-query ()
  (submit-blast-query 
   (one-string
    "QUERY=555"
    "&DATABASE=nr"
    "&HITLIST_SIZE=10"
    "&FORMAT_TYPE=HTML"
    "&FILTER=L"
    "&EXPECT=0.00001"
    "&PROGRAM=blastn"
    "&CLIENT=web"
    "&SERVICE=plain"
    "&NCBI_GI=on"
    "&PAGE=Nucleotides"
    "&CMD=Put")))


(defun blast-sequence 
    (sequence &key (db "nr") (expect 0.00001) (program "blastn"))
  (submit-blast-query 
   (format nil
       (one-string
        "QUERY=~a"
        "&DATABASE=~a"
        "&HITLIST_SIZE=10"
        "&FORMAT_TYPE=HTML"
        "&FILTER=L"
        "&EXPECT=~a"
        "&PROGRAM=~a"
        "&CLIENT=web"
        "&SERVICE=plain"
        "&NCBI_GI=on"
        "&PAGE=Nucleotides"
        "&CMD=Put")
     sequence db expect program
     )))

(defparameter test-sequence
  (one-string
   "AGACGCCGCCGCCACCACCGCCACCGCCGCAGCAGAAGCAGCGCACCGC"
   "AGGAGGGAAGATGCCGGCGGGGCACGGGCTGCGGGCGCGGACGGCGACC"
   "TCTTCGCGCGGCCGTTCCGCAAGAAGGGTTACATCCCGCTCACCACCTA"
   "CCTGAGGACGTACAAGATCGGCGATTACGTNGACGTCAAGGTGAACGG"))

(defun do-blast-sequence () (blast-sequence test-sequence))
