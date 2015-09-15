;;; -------------------------------------------------- BLAST --------------------------------------------------

(in-package biolisp)

(defstruct result id log content temp alist)
(defstruct blast query rid hits domains)
(defstruct hit id percent-identity alignment-length mismatches gap-openings q-start q-end s-start s-end expected-value bit-score)
(defstruct domain id name expected-value alignment)
(defstruct description raw genbank-info)

;;; This is a process lock to prevent spamming requests to NCBI
(defvar *blast-request-lock* (mp:make-process-lock))

;;; This is the maximum number of seconds to wait before checking for results initially
(defvar *max-blast-wait-seconds-first-round* 60)

;;; This is the maximum number of seconds to wait after having already checked once
(defvar *secondary-blast-wait-seconds* 20)

;;; This is the maximum number of times to check of blast results before giving up
(defvar *max-wait-time* 10)

;;; These are globals for results for each individual blast
(defvar *blast-results*)
(setq *blast-results* (make-hash-table))
(defvar *blast-result-num*)
(setq *blast-result-num* 0)

(defun blast-mp (sequence &key (expect "1E-10") (hitlist-size "3"))
  (incf *blast-result-num*)
  (let ((id (format nil "blast ~a" *blast-result-num*)))
    (setf (gethash *blast-result-num* *blast-results*) (make-result :id id))
    (mp:process-run-function id #'blast-process 
			     sequence 
			     expect 
			     hitlist-size
			     *blast-result-num* 
			     *blast-results*))
  *blast-result-num*)

(defun blast-process (sequence expect hitlist-size result-num query-results)
  (declare (special result-num))
  (declare (special query-results))
  (push (blast sequence :expect expect :hitlist-size hitlist-size) (result-content (gethash result-num *blast-results*))))

;;; Top level function to query NCBI for either a blastn or a blastp depending on the input sequence

(defun blast (sequence &key (expect "1E-10") (hitlist-size "3"))
  (get-conserved-domains
   (convert-blast-to-struct
    (wait-for-blast-result 
     (parse-meta-info-from-blast-result 
      (create-blast-query 
       (cleanup-sequence sequence) 
       :expect expect
       :hitlist-size hitlist-size))
     :hitlist-size hitlist-size)) 
   sequence))

;;; This will check if the sequence is a protein, then searches for conserved domains from NCBI 

(defun get-conserved-domains (blast sequence)
  (if (protein-p sequence) 
      (progn
	(biolingua::appendlog (format nil "Searching for conserved domains...~%"))
	(let* ((page (CG:WEB-PAGE-CONTENTS 
		      (concatenate 'string
				   "http://www.ncbi.nlm.nih.gov/Structure/cdd/wrpsb.cgi?"
				   "datalib=oasis_sap"
				   "&input_type=fasta"
				   "&sequence=" (cleanup-sequence sequence))))
	       (domains (cdr (make-list-from-inbetween-token page "<a name"))))
	  (loop for domain in (remove nil domains)
		do
		(let* ((id (subseq-token domain "uid=" "&"))
		       (name (string-right-trim "." (subseq-token domain "</a>, " "</TD>")))
		       (expected-value (subseq-token domain "Expect = " (string #\newline)))
		       (alignment (get-inbetween-tag domain "pre" :from-end 1)))
		  (push (make-domain :id id 
				     :name name 
				     :expected-value expected-value 
				     :alignment alignment) (blast-domains blast)))
		finally (setf (blast-domains blast) (reverse (blast-domains blast)))))
	(biolingua::appendlog (format nil "Found conserved domains.~%"))))
  blast)


;;; This displays the information from genbank for each hit from a blast struct

(defun show-genbank-info (blast)
  (remove nil 
	  (loop for hit in (remove nil (blast-hits blast)) 
		collect (description-genbank-info (hit-description hit)))))
  
;;; Removes anything that isn't a letter and makes everything else upper case

(defun cleanup-sequence (sequence &aux clean-sequence)
  (do ((i 0 (+ i 1)))
      ((= i (length sequence)) (string-upcase clean-sequence))
      (if (alpha-char-p (char sequence i))
	  (setq clean-sequence (concatenate 'string clean-sequence (string (char sequence i)))))))

;;; Retrieves the RID (request ID), and RTOE (the estimated time before real blast results are available)
;;; from the web page outputed after issueing a query.

(defun parse-meta-info-from-blast-result (blast-result)
  (let* ((rid (subseq-token blast-result "RID = " (string #\newline)))
	 (rtoe (subseq-token blast-result "RTOE = " (string #\newline))))
    (biolingua::appendlog (format nil "RID: ~a~%" rid))
    (cons rid (min *max-blast-wait-seconds-first-round* (parse-integer rtoe)))
    ))

;;; This issues the blastn or blastp query for a given already cleaned sequence

(defun create-blast-query (sequence &key expect hitlist-size)
  (let ((program (if (protein-p sequence) "blastp" "blastn")))
    (biolingua::appendlog (format nil "Submitting ~a request for a ~a letter long sequence...~%" program (length sequence)))
    (mp:with-process-lock (*blast-request-lock*)
			  (CG:WEB-PAGE-CONTENTS 
			   (concatenate 'string 
					"http://www.ncbi.nlm.nih.gov/blast/Blast.cgi?"
					"CMD=PUT"
					"&QUERY=" sequence
					"&PROGRAM=" program
					"&ALIGNMENTS=" hitlist-size
					"&ALIGNMENT_VIEW=Tabular"
					"&AUTO_FORMAT=Semiauto"
					"&CLIENT=web"
					"&FILTER=L"
					"&DATABASE=nr"
					"&DESCRIPTIONS=" hitlist-size
					"&EXPECT=" expect
					"&FORMAT_BLOCK_ON_RESPAGE=None"
					"&FORMAT_OBJECT=Alignment"
					"&FORMAT_TYPE=Text"
					"&LAYOUT=TwoWindows"
					"&SERVICE=plain"
					"&HITLIST_SIZE=" hitlist-size)))))

;;; This returns 't if there are no G, C, A, or T's in the input sequence, 'nil if there are. NEEDS TO BE IMPROVED!

(defun protein-p (sequence)
  (loop for char across sequence
	do
	(if (case char
		  (#\M 't)
		  (#\m 't))
	    (return 't))
	finally (return 'nil)))

;;; This retrieves the blast result for a specific rid

(defun get-blast-answer (rid &key hitlist-size)
  (cg::with-open-web-page 
   (stream
    (concatenate 'string 
		 "http://www.ncbi.nlm.nih.gov/blast/Blast.cgi?"
		 "CMD=GET"
		 "&ALIGNMENT_VIEW=Tabular"
		 "&ALIGNMENTS=" hitlist-size
		 "&FORMAT_TYPE=Text"
		 "&RID=" rid))
   (loop for line = (read-line stream nil nil)
	 until (null line)
	 do (if (equal "<HTML>" line) (return 'nil))
	 when (equal "1" (subseq line 0 1))
	 collect (make-list-from-inbetween-token line #\tab))))    

;;; This will sleep for the recommended about of time from the blast query result 
;;; before checking if the results are in yet, and will wait *secondary-blast-wait-seconds*
;;; after the initial recommended sleep.

(defun wait-for-blast-result (blast-meta-info &key hitlist-size)
  (let* ((num 0)
	 (rid (car blast-meta-info))
	 (rtoe (cdr blast-meta-info)))
    (loop as page = (get-blast-answer rid :hitlist-size hitlist-size)
	  until page
	  do 
	  (incf num)
	  (if (= num *max-wait-time*) (progn (biolingua::appendlog (format nil "Blast results could not be found.~%"))
					     (return 'nil)))
	  (biolingua::appendlog (format nil "Checking for blast results in ~a...~%" (format-time rtoe)))
	  (sleep rtoe)
	  (setq rtoe *secondary-blast-wait-seconds*)
	  finally 
	  (biolingua::appendlog (format nil "Blast results found.~%"))
	  (return (cons rid (remove nil (list-remove-second-duplicates-equal page)))))))

;;; This will take a list of blast hits and return a blast struct containing them
;;; Fields: Query id, Subject id, % identity, alignment length, mismatches, gap openings, q. start, q. end, s. start, s. end, e-value, bit score

(defun convert-blast-to-struct (list &aux hits)
  (loop for hit in (cdr list)
	do (push (make-hit 
		  :id (second hit)
		  :percent-identity (third hit)
		  :alignment-length (parse-integer (fourth hit))
		  :mismatches (parse-integer (fifth hit))
		  :gap-openings (parse-integer (sixth hit))
		  :q-start (parse-integer (seventh hit))
		  :q-end (parse-integer (eighth hit))
		  :s-start (parse-integer (ninth hit))
		  :s-end (parse-integer (tenth hit))
		  :expected-value (convert-scientific-number-to-real (nth 10 hit))
		  :bit-score (nth 11 hit))
		 hits)
	finally (return (make-blast :hits (reverse hits) :rid (car list)))))
	
(defun convert-scientific-number-to-real (string)
  (let ((.-pos (position #\. string))
	(E-pos (or (position #\E string) (position #\e string))))
    (cond
     ((and .-pos E-pos) (scientific-notation string))
     (.-pos (scientific-notation (concatenate 'string string "E0")))
     (E-pos (if (= E-pos 0) 
		(scientific-notation (concatenate 'string (subseq string 0 E-pos) "1.0" (subseq string E-pos)))
	      (scientific-notation (concatenate 'string (subseq string 0 E-pos) ".0" (subseq string E-pos))))))))

(defun scientific-notation (string)
  (let* ((.-pos (position #\. string))
	 (E-pos (or (position #\E string) (position #\e string)))
	 (before-decimal (subseq string 0 .-pos))
	 (after-decimal (subseq string (+ .-pos 1) E-pos))
	 (after-e (subseq string (+ E-pos 1))))
    (float
     (* 
      (+ (parse-integer before-decimal)
	 (* (parse-integer after-decimal)
	    (expt 10 (* -1 (length after-decimal)))))
      (expt 10 (parse-integer after-e))))))



;;; This function will take a list like '(("a" . "1") ("a" . "2") ("b" . "1")) and remove all items that have
;;; a common car. The result for the above list would be: '(("a" "1") ("b" "1"))

(defun list-remove-second-duplicates-equal (list &aux results)
  (loop for item in list
	do (if (not (second-member item results)) (push item results))
	finally (return (reverse results))))

;;; This is a helper function for list-remove-car-duplicates. It will return 't when the car of member
;;; is equal to the car of any item in list.

(defun second-member (member list)
  (loop for item in list
	do (if (equal (second member) (second item))
	       (return member))))
			  
;;; This will take a blast struct and fill in the description-genbank-info for each hit
;;; by querying genbank with the hit id

(defun parse-descriptions (blast &key (counter 2))
  (loop for hit in (blast-hits blast) 
	until (or (null hit) (= counter 0))
	do (setf (description-genbank-info (hit-description hit)) (get-genbank-info (hit-id hit)))
	(setq counter (- counter 1))
	finally (return blast)))

;;; This takes a list of blast-ids and 

(defun get-genbank-infos (blast-ids)
  (loop for hit in (blast-hits blast-ids)
	as id = (hit-id hit)
	as genbank-info = (get-genbank-info id)
	do (biolingua::appendlog (format nil "Retrieved information for id: ~a~%" id))
	collect (cons (hit-expected-value hit) genbank-info)))

;;; This clarifies the "/product" fields from the genbank web page

(defun get-genbank-info (id)
 (let* ((genbank-id (second (make-list-from-inbetween-token id "|")))
	(products (get-genbank-products genbank-id)))
   (if products
       (loop for product in products
	     collect (remove-string "  " 
				    (remove #\linefeed 
					    (subseq-token product "/product=\"" "\"")))))))

;;; This returns the "/product" lines from the genbank web page

(defun get-genbank-products (genbank-id)
  (remove nil 
	  (cg::with-open-web-page 
	   (stream 
	    (format nil "http://www.ncbi.nlm.nih.gov/entrez/query.fcgi?cmd=Retrieve&db=Nucleotide&list_uids=~a&dopt=GenBank"
		    genbank-id))
	   (loop for line = (read-line stream nil nil) until (null line) collect
		 (if (search "/product" line)
		     (concatenate 'string line (read-line stream nil nil)))))))

(export '(
	  blast domain hit description *max-wait-time* *max-blast-wait-seconds-first-round*
	  *secondary-blast-wait-seconds* *cat* *kb* *kb-location* *stop-terms*
	  *trimchars* cleanup-sequence create-blast-query
	  parse-meta-info-from-blast-result get-blast-answer
	  wait-for-blast-result blast-results-ready?  convert-blast-to-struct
	  get-hits-from-xml parse-xml-hit parse-descriptions
	  categorize-description import-kb trim-and-parse-string
	  add-category-entry find-organism-category
	  remove-organism-category gene-descriptions get-genbank-infos 
	  get-genbank-info show-genbank-info get-blast-ids protein-p
	  car-member list-remove-car-duplicates-equal get-conserved-domains
	  convert-scientific-number-to-real blast-mp blast-process *blast-results* 
	  *blast-result-num*))
