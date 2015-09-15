(in-package biolingua)

(defstruct result id log content temp)
(defstruct gene name sequence go-hits ec-hits domains)
(defstruct go-hit accuracy e-value go-num ec-num product-description go-tree)
(defstruct ec-hit accuracy e-value ec-num product-description ec-info)
(defstruct ec id description alternate-names catalytic-activity cofactors comments diseases prosite swiss-prot)
(defstruct whinfo c lsht lsk tl)

;;; This is the GO hash table where each GO# is a key, and it's name and parents are the values
(defvar *kb*)

;;; This is the EC hash table where each EC# is a key, and it's properties are the values
(defvar *ec*)

;;; This is a whinfo struct containing all the GO names as word-homology keys and the #'s and values
(defvar *gowh*)

;;; This is a whinfo struct containing all the EC names as word-homology keys and the #'s and values
(defvar *ecwh*)

;;; This hash table has all the description and alternate names from *ec* as keys and their corresponding EC # as values
(defvar *word2ec*)

;;; This hash table converts EC #'s to multiple GO #'s
(defvar *ec2go*)

;;; This hash table converts each GO # to their corresponding EC #
(defvar *go2ec*)

;;; This is location of ec2go.txt from http://www.geneontology.org/ec2go
(defvar *ec2go-file* (merge-pathnames  "ec2go.txt" *load-truename*))

;;; This is location of the ec2go.lsp generated from update-kb
(defvar *ec2go-ht-file* (merge-pathnames  "ec2go.lsp" *load-truename*))

;;; This is location of go.xml from http://www.fruitfly.org/annot/go/go.xml
(defvar *go-file* (merge-pathnames  "go.xml" *load-truename*))

;;; This is location of the go.lisp generated from update-kb
(defvar *kb-file* (merge-pathnames  "go.lsp" *load-truename*))

;;; This is location of enzyme.lsp generated from update-ec
(defvar *ec-file* (merge-pathnames "enzyme.lsp" *load-truename*))

;;; This is the location of enzyme.dat from ftp://ca.expasy.org/databases/enzyme/enzyme.dat
(defvar *enzyme-file* (merge-pathnames "enzyme.dat" *load-truename*))

;;; This is a settable option to determine if appendlog should log progress.
;;; Turn it off if you want to run functions in biolisp without worrying about biolingua.
(defvar *web-log* 'nil) 

;;; These are globals for results found when blasting several sequences at once
(defvar *batch-results*)
(setq *batch-results* (make-hash-table))
(defvar *batch-result-num*)
(setq *batch-result-num* 0)

;;; These are globals for results for each individual blast
(defvar *gene-results*)
(setq *gene-results* (make-hash-table))
(defvar *gene-result-num*)
(setq *gene-result-num* 0)

;;; This tells (biostart) to reload the knowledge base
(setq *kb* 'nil)

;;; This is used to log progress from individual processes into the global results log

(defun appendlog (string &key (no-p))
  (if *web-log*
      (setf (result-log (gethash result-num query-results)) (concatenate 'string (result-log (gethash result-num query-results))
									 (if no-p "" "<P>") 
									 string))
    (princ string)))

;;; These are two seperate methods to get the GO # from an EC #, where the EC # can be either a list or a string

(defmethod get-go-from-ec ((ec list))
  (let (ec-string)
    (loop for i from 0 to (- (length ec) 2)
	  do (setq ec-string (concatenate 'string ec-string (string (digit-char (nth i ec))) "."))
	  finally (setq ec-string (concatenate 'string ec-string (string (digit-char (nth i ec))))))
    (get-go-from-ec ec-string)))

(defmethod get-go-from-ec ((ec string))
  (let* ((go (caar (gethash ec *ec2go*))))
    (get-go-trees go)))

;;; This function creates three lisp files to be loaded by import-kb from go.xml, ec2go.txt, and enzyme.dat

(defun update-kb ()
  (write-hashtable *kb-file*
		   (create-initial-go-ht 
		    (file-contents *go-file*)))
  (write-hashtable *ec2go-ht-file*
		   (create-ec2go-ht *ec2go-file*))
  (write-hashtable *ec-file*
		   (create-enzyme-ht
		    (file-contents *enzyme-file*))))

;;; This imports the knowledge base from several pre-made lisp files if it hasn't been done already

(defun import-kb ()
  (if (not *kb*)  
      (progn 
	(format t "Importing knowledge base... ")
	(setq *ec* (read-hashtable *ec-file*))
	(setq *word2ec* (create-word2ec-ht *ec*))
	(setq *kb* (read-hashtable *kb-file*))
	(setq *ec2go* (read-hashtable *ec2go-ht-file*))
	(setq *go2ec* (create-go2ec-ht *ec2go*))
	(setq *gowh* (make-whinfo))
	(maphash #'(lambda (key value) (push (cons (caar value) key) (whinfo-c *gowh*))) *kb*)
	(init *gowh*)
	(setq *ecwh* (make-whinfo))
	(maphash #'(lambda (key value) (push (cons key (car value)) (whinfo-c *ecwh*))) *word2ec*)
	(init *ecwh*)
	(format t "Done~%"))))

;;; This creates a hash table for *word2ec*

(defun create-word2ec-ht (ec)
  (let ((word2ec-ht (make-hash-table :test #'equal)))
    (maphash #'(lambda (key value)
		 (loop for name in (append (ec-description (caar value)) (ec-alternate-names (caar value)))
		       when (> (length name) 1)
		       do (push key (gethash name word2ec-ht))))
	     ec)
    word2ec-ht))

;;; This creates a hash table for *ec*

(defun create-enzyme-ht (enzyme-file)
  (let ((ht (make-hash-table :test #'equal)))
    (loop for ec in (make-list-from-inbetween-token enzyme-file "//")
	  as enzyme = (make-ec)
	  do
	  (loop for line in (reverse (make-list-from-inbetween-token (subseq ec 2) (string #\newline)))
		when (> (length line) 4)
		do
		(let ((type (subseq line 0 2))
		      (content (string-trim " ." (subseq line 5))))
		  (cond
		   ((equal type "ID") (push content (ec-id enzyme)))
		   ((equal type "DE") (push content (ec-description enzyme)))
		   ((equal type "AN") (push content (ec-alternate-names enzyme)))
		   ((equal type "CA") (push content (ec-catalytic-activity enzyme)))
		   ((equal type "CF") (push content (ec-cofactors enzyme)))
		   ((equal type "CC") (push content (ec-comments enzyme)))
		   ((equal type "DI") (push content (ec-diseases enzyme)))
		   ((equal type "PR") (push content (ec-prosite enzyme)))
		   ((equal type "DR") (push content (ec-swiss-prot enzyme))))))
	  (push enzyme (gethash (car (ec-id enzyme)) ht))
	  finally (return ht))))

;;; This creates a hash table for *ec2go*

(defun create-ec2go-ht (ec2go)
  (let* ((ht (make-hash-table :test #'equal)))
    (with-open-file (stream ec2go :direction :input)
		    (loop for line = (read-line stream nil nil)
			  until (null line)
			  do
			  (if (eql (char line 0) #\E)
			      (let* ((ec-go (get-ec-go line))
				   (ec (car ec-go))
				   (go (cdr ec-go)))
				(setf (gethash ec ht) (append (gethash ec ht) go))))))
    ht))

;;; This creates a hash table for *go2ec*

(defun create-go2ec-ht (ec2go-ht)
  (let ((go2ec-ht (make-hash-table :test #'equal)))
    (maphash #'(lambda (key value)
		 (loop for num in (car value)
		       do (push key (gethash num go2ec-ht))))
		 ec2go-ht)
    go2ec-ht))

;;; This creates a hash table for *kb*

(defun create-initial-go-ht (go)
  (let* ((go-ht (make-hash-table :test #'equal)))
    (loop as start-pos = (search "<go:term" go :start2 (if start-pos (+ start-pos 1) 0))
	  until (null start-pos)
	  as end-pos = (+ (search "</go:term" go :start2 start-pos) 10)
	  as entry = (if start-pos (parse-term 
				    (remove-inbetween-tokens #\& #\; 
							     (subseq go start-pos end-pos))))
	  do
	  (setf (gethash (car entry) go-ht) (cdr entry)))
    go-ht))

;;; This is a helper function for create-initial-go-ht that parses each term in the GO

(defun parse-term (term)
  (let* ((parsed-term (car (net.xml.parser:parse-xml term :content-only term)))
	 (id (subseq (nth 1 (nth 2 parsed-term)) 3))
	 (name (nth 1 (nth 4 parsed-term)))
	 (parents (get-parents-from-isa parsed-term)))
    (list id name parents)))

;;; This is a helper function for create-initial-go-ht that is called by parse-term and returns all the
;;; "isa" fields for a go-num in the GO

(defun get-parents-from-isa (parsed-term)
  (remove nil (loop for property in parsed-term 
		    collect (if (and (listp property) 
				     (listp (car property)))
				(subseq (nth 2 (car property)) 34)))))

;;; This is a helper function for *ec2go*

(defun get-ec-go (line)
  (let* ((list (make-list-from-inbetween-token (remove-string "GO:" line) ";"))
	 (ec (string-trim " " (subseq-token (car list) ":" ">")))
	 (go (make-list-from-inbetween-token (remove #\space (nth 1 list)) ",")))
    (remove nil (cons ec go))))


(defun get-info-from-sequences (sequences &key (expect "1E-10"))
  (let* ((batch-num (what-are-these-genes-mp? sequences :expect expect))
	 (batch-nums (result-log (gethash batch-num *batch-results*))))
    (appendlog (format nil "Determining the EC numbers for ~a sequences" 
	    (length batch-nums)))
    (loop for time-elapsed = 0 then (+ time-elapsed 5)
	  as genes = (loop for (fasta-header . gene-num) in batch-nums
			   as gene = (result-content (gethash gene-num *gene-results*))
			   when gene
			   collect (cons 
				    (cons gene-num fasta-header)
				    (append
				     (loop for ec-hit in (gene-ec-hits gene) 
					   collect (list (ec-hit-e-value ec-hit) 
							 (ec-hit-accuracy ec-hit)
							 (ec-hit-ec-num ec-hit)))
				     (loop for go-hit in (gene-go-hits gene) 
					   collect (list (go-hit-e-value go-hit) 
							 (go-hit-accuracy go-hit)
							 (go-hit-ec-num go-hit))))))
	  until (= (length batch-nums) (length genes))
	  do 
	  (appendlog "." :no-p 't)
	  (sleep 5)
	  finally (return genes))))

;;; This takes as input a fasta string of sequences and immediately returns the batch result number
;;;  as a key for *batch-results*

(defun what-are-these-genes-mp? (sequences &key (expect "1E-10"))
  (incf *batch-result-num*)
  (let ((id (format nil "batch ~a" *batch-result-num*)))
    (setf (gethash *batch-result-num* *batch-results*) 
	  (make-result :id id
		       :log
		       (loop for sequence in (remove nil (make-list-from-inbetween-token sequences ">"))
			     when (not (equal sequence ""))
			     collect 
			     (let* ((pos (search (string #\newline) sequence))
				    (name (subseq sequence 0 pos))
				    (query (subseq sequence pos)))
			       (cons name (what-is-this-gene-mp? query :expect expect)))))))
  *batch-result-num*)

;;; This takes as input a sequence and immediately returns the result num as a key for *gene-results*

(defun what-is-this-gene-mp? (sequence &key (expect "1E-10"))
  (incf *gene-result-num*)
  (let ((id (format nil "sequence ~a" *gene-result-num*)))
    (setf (gethash *gene-result-num* *gene-results*) (make-result :id id))
    (mp:process-run-function id #'what-is-this-gene? sequence expect *gene-result-num* *gene-results*))
  *gene-result-num*)


;;; This is the function used for each process in order to figure out what a gene is
;;; This declares result-num and query-results special so that they are considered global, but only
;;; within this individual process, and no others
				   
(defun what-is-this-gene? (sequence expect result-num query-results &key (n-returned 3))
  (declare (special result-num))
  (declare (special query-results))
  (let* ((blast (blast sequence :expect expect))
	 (keywords (prioritize-keywords (get-blast-id-keywords blast)))
	 (domains (biolisp::blast-domains blast)))
    (appendlog (format nil "Searching knowledge base for ~a keywords. It should take about ~a...~%" 
		       (length keywords)
		       (format-time (* .05 (sum (loop for keyword in keywords 
						      collect (length (cdr keyword))))))))
    (setf (result-content (gethash result-num *gene-results*))
	  (make-gene 
	   :sequence (cleanup-sequence sequence)
	   :domains domains
	   :go-hits
	   (loop for (e-value accuracy go-num product-description) in (get-hits keywords *gowh*)
		 as ec = (get-best-ec-from-go go-num)
		 as n from 0 to n-returned
		 when ec
		 collect (make-go-hit 
			  :accuracy accuracy 
			  :e-value e-value 
			  :go-num go-num 
			  :ec-num ec
			  :product-description product-description
			  :go-tree (get-go-trees go-num)))
	   :ec-hits
	   (loop for (e-value accuracy ec-num product-description) in (get-hits keywords *ecwh*)
		 as n from 1 to n-returned
		 as ec-info = (caar (gethash ec-num *ec*))
		 collect (make-ec-hit
			  :accuracy accuracy
			  :e-value e-value
			  :ec-num ec-num
			  :product-description product-description
			  :ec-info ec-info))))
    (appendlog (format nil "Completed!~%"))))

;;; This functions get an EC # for an input GO #

(defun get-best-ec-from-go (go-num)
  (car (gethash go-num *go2ec*)))

;;; This averages the accuracy from word-homology for identical go and ec hits and removes duplicates

(defun average-same-num (go-hits &aux unique-hits)
  (let ((old-hits go-hits))
    (loop for old-hit in old-hits
	  when (not (loop for new-hit in unique-hits
			  do (if (equal (third old-hit) (third new-hit)) (return 't))))
	  do (setf (second old-hit) (mean (loop for old-hit2 in old-hits
							  when (equal (third old-hit)
								      (third old-hit2))
							  collect (second old-hit2))))
	  (push old-hit unique-hits)
	  finally (return (reverse unique-hits)))))

;;; This calls word-homology for a given string and whinfo struct

(defun get-hits (keywords wh)
  (sort
   (sort
    (average-same-num
     (sort 
      (loop for (evalue . keyword) in keywords
	    append (prepend-list evalue (word-homology keyword wh)))
      #'(lambda (a b) (< (car a) (car b)))))
    #'(lambda (a b) (< (car a) (car b))))
   #'(lambda (a b) (and (= (car a) (car b)) (> (second a) (second b))))))

;;; This function is used to determine the best keywords to use with word-homology

(defun prioritize-keywords (keywords)
  (let (value-list)
    (loop for keyword in keywords
	  as value = (assoc keyword value-list :test #'equal)
	  do (if value (incf (cdr value))
	       (push (cons keyword 1) value-list)))
    (loop for keyword in (sort (reverse value-list) #'(lambda (a b) (> (cdr a) (cdr b))))
	  for i from 0 to 4
	  collect (car keyword))))

;;; This function returns a list of expected value and keywords for each blast id given as input

(defun get-blast-id-keywords (blast-ids)
  (loop for (evalue . keywords) in (get-genbank-infos blast-ids)
	append (prepend-list evalue keywords)))

#| old one
(defun get-blast-id-keywords (blast-ids)
  (let* ((genbank-infos (get-genbank-infos blast-ids))
	 (first-two-in-common (cons (car (first genbank-infos))
				    (intersection-equal (cdr (first genbank-infos)) (cdr (second genbank-infos)))))
	 (first-three-in-common (cons (car (first genbank-infos))
				      (intersection-equal first-two-in-common (cdr (third genbank-infos)))))
	 (keywords (cond ((cdr first-three-in-common) first-three-in-common)
			 ((cdr first-two-in-common) first-two-in-common)
			 ((append (cdr (first genbank-infos))
				  (cdr (second genbank-infos))
				  (cdr (third genbank-infos)))))))
    keywords))
|#

;;; This recursively creates a hash table containing each GO # and it's corresponding parent tree

(defun organize-entire-go (go-ht)
  (let ((organized-go-ht (make-hash-table :test #'equal)))
    (maphash #'(lambda (key name-and-parents)
		 (let* ((name (car (car name-and-parents))))
		   (push (cons name 
			       (get-parents-recursively key go-ht)) 
			 (gethash key organized-go-ht)))) 
	     go-ht)
    organized-go-ht))

;;; This individually creates the GO parent tree for a given GO #

(defun get-go-trees (go-num &key (go-ht *kb*))
  (let* ((name (car (car (gethash go-num go-ht)))))
    (cons name (get-parents-recursively go-num go-ht))))

;;; This is the recursive function used to create the parent tree for a GO #
  
(defun get-parents-recursively (key go-ht)
  (loop for parent in (nth 1 (car (gethash key go-ht)))
  	collect (cons 
		 (cons (car (car (gethash parent go-ht)))
			    parent) 
		 (get-parents-recursively parent go-ht))))

;;; Word homology, originally for converting cyanobase into EcoCyc

;;; lsht is the Letter sequence Hash Table
;;; lsk is the Letter sequence counter
;;; c is the list of (word . result) where what is returned when a word is found, is result 
;;; tl is the compiled target list

;;; This function initilizes the word-homology database for a given whinfo struct

(defun init (whinfo)
  (setf (whinfo-lsht whinfo) (make-hash-table :test #'equal))
  (setf (whinfo-lsk whinfo) 0)
  (setf (whinfo-tl whinfo) (compile-target-list (whinfo-c whinfo) whinfo))
  )

;;; This converts a word to a series of numbers where each unique pair of letters is a new number and
;;; adds it to the existing word-homology database

(defun compile-word (word whinfo)
  (loop for p from 0 to (- (length word) 2)
	as l1 = (aref word p)
	as l2 = (aref word (1+ p))
	as l1.l2 = (cons l1 l2)
	as k = (gethash l1.l2 (whinfo-lsht whinfo))
	collect (or k
		    (let ((k (incf (whinfo-lsk whinfo))))
		      (setf (gethash l1.l2 (whinfo-lsht whinfo)) k)
		      k))
	))

;;; This function determines how similar the two input lists are, where the lists are the numbers generated
;;; by compile-word

(defun score-homology (w1 w2)
  (let* ((o1 (loop for l1 in w1 if (member l1 w2) sum 1))
	 (o2 (loop for l2 in w2 if (member l2 w1) sum 1))
	 (l1 (length w1))
	 (l2 (length w2))
	 )
    (/ (+ (/ o1 l1) (/ o2 l2)) 2.0)))

;;; This compiles a list of words with compile-word

(defun compile-target-list (names whinfo)
  (loop for name in names
	collect (cons (compile-word (car name) whinfo) (cdr name))))

;;; Get the top N scoring of a set of words w/o having to sort, which is too slow.

(defun word-homology (w whinfo &optional (n 3))
  (let* ((wc (compile-word w whinfo))
	 (topn (loop for i from 1 to n collect (list 0 'foo 'bar)))
	 )
    (if (not (whinfo-tl whinfo)) (import-kb))
    ;; If the new word is more than the lowest of the ones in the set, replace that one with the new one.
    (loop for (tc . tw) in (whinfo-tl whinfo)
	  as score = (score-homology wc tc)
	  as lowest = (loop with least-entry = (car topn)
			    with least-score = (car least-entry)
			    for test-entry in (cdr topn)
			    as ts = (car test-entry)
			    if (< ts least-score)
			    do 
			    (setq least-score ts least-entry test-entry)
			    finally (return least-entry))
	  if (> score (car lowest))
	  do 
	  (setf (first lowest) score)
	  (setf (second lowest) tw)
	  (setf (third lowest) w)
	  finally (return topn))))