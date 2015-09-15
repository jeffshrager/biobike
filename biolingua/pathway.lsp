;;; -------------------------------------------------- KEGG --------------------------------------------------

(in-package biolingua)

(defstruct enzyme ec name reaction substrate product class comment)
(defstruct compound id name)

(defvar *web-client-connections* 0)
(defvar *max-web-client-connections* 20)
(defvar *total-web-client-connections* 0)

;;; These are globals for results found when trying to determine the pathways between two EC numbers

(defvar *pathway-results*)
(setq *pathway-results* (make-hash-table))
(defvar *pathway-result-num*)
(setq *pathway-result-num* 0)

(defvar *ec-pathways*)

(defvar *ec-pathways-file* (merge-pathnames "ec-pathways.lsp" *load-truename*))

(defun import-pathway ()
  (setq *ec-pathways* (read-hashtable *ec-pathways-file*)))

(defvar *general-compound-list*)
(setq *general-compound-list* (append
			       (loop for num from 1 to 30
				     collect (format nil "C~a" (prepend-with-zeros (format nil "~a" num) 5)))
			       '("C00035"
				 "C00044")))

;;; This creates a process to determine the pathways between two EC numbers and returns the the
;;; pathway result num as a key for *pathway-results* where max-links is the maximum number of 
;;; queries to make to KEGG

(defun find-pathway-between-ec-mp (initial-ec final-ec &key cutoff max-links)
  (incf *pathway-result-num*)
  (let ((id (format nil "pathway ~a" *pathway-result-num*)))
    (setf (gethash *pathway-result-num* *pathway-results*) (make-result :id id))
    (mp:process-run-function id #'find-pathway-between-ec 
			     initial-ec 
			     final-ec 
			     cutoff
			     max-links 
			     *pathway-result-num* 
			     *pathway-results*))
  *pathway-result-num*)

;;; This is the function called to create the process to determine the pathways between two EC
;;; numbers where max-links is the maximum number of queries to make to KEGG

(defun find-pathway-between-ec (initial-ec final-ec cutoff max-links result-num query-results)
  (declare (special result-num))
  (declare (special query-results))
  (appendlog (format nil "Determining all the pathways from ~a to ~a~%" initial-ec final-ec))
  (let* ((initial-info (get-enzyme-info initial-ec))
	 (initial-substrates (get-compound-ids (enzyme-substrate initial-info)))
	 (initial-products (get-compound-ids (enzyme-product initial-info)))
	 (final-info (get-enzyme-info final-ec))
	 (final-substrates (get-compound-ids (enzyme-substrate final-info)))
	 (final-products (get-compound-ids (enzyme-product final-info)))
	 (substrates (append initial-substrates initial-products))
	 (products (append final-substrates final-products))
	 (pathways (if max-links max-links
		     (* (length substrates) (length products)))))
    (appendlog (format nil "It will take about ~A to make ~A queries...~%" (format-time (* 5 pathways)) pathways))
    (setf (result-content (gethash result-num *pathway-results*))
	  (let ((content (excl::list-remove-duplicates-equal	  
			  (filter-pathways
			   (loop with num = 0
				 for time = 0 then (+ time 1)
				 initially (loop for substrate in substrates
						 do
						 (loop for product in products
						       do 
						       (incf num)
						       collect (find-kegg-pathway-mp substrate product :cutoff cutoff)
						       until (= num pathways))
						 until (= num pathways))
				 do
				 (sleep 1)
				 until ;(or 
				 (= pathways (length (result-temp (gethash result-num *pathway-results*))))
					;(= time 60))
				 finally (return (loop for item in (result-temp (gethash result-num *pathway-results*))
						       append item)))
			   initial-ec 
			   final-ec))))
	    (if content content "none"))))
  (setf (result-temp (gethash result-num *pathway-results*)) 'nil))

;;; This returns the compound-id for a list of compounds

(defun get-compound-ids (compounds)
  (loop for compound in compounds
	collect (compound-id compound)))

;;; This filters the pathways so that only the ones with the exact initial-ec and final-ec are returned

(defun filter-pathways (pathways initial-ec final-ec &aux results)
  (loop for pathway in pathways
	do (if (or (and (equal (enzyme-ec (second pathway)) initial-ec)
			(equal (enzyme-ec (nth (- (length pathway) 2) pathway)) final-ec))
		   (and (equal (enzyme-ec (second pathway)) final-ec)
			(equal (enzyme-ec (nth (- (length pathway) 2) pathway)) initial-ec)))
	       (push (loop for item in pathway
			   collect (if (compound-p item)
				       (cons (compound-id item) (compound-name item))
				     (cons (enzyme-ec item) (enzyme-name item))))
		     results))
	finally (return results)))

;;; This creates a new process to determine the pathways between the initial EC and the final EC

(defun find-kegg-pathway-mp (initial final &key cutoff)
  (let ((id (format nil "pathway ~a: ~a -> ~a" result-num initial final)))
    (loop until (< *web-client-connections* *max-web-client-connections*)
	  do (sleep 1))
    (mp:process-run-function id #'find-kegg-pathway-process initial final cutoff result-num query-results)))

;;; This declares result-num and query-results special so that they are considered global, but only
;;; within this individual process, and no others
  
(defun find-kegg-pathway-process (initial final cutoff result-num query-results)
  (declare (special result-num))
  (declare (special query-results))
  (push (find-kegg-pathway initial final :cutoff cutoff) (result-temp (gethash result-num *pathway-results*))))

;;; Given an initial substrate and a final product, this will return all the pathways in the KEGG database
;;; to get from one to the other. Each step on the pathway is given in the form of 
;;; (id . description), for example ("C00293" . "Glucose")

(defun find-kegg-pathway (initial final &key cutoff)
  (if (and (eq #\C (char initial 0)) (eq #\C (char final 0)))
      (parse-clarified-results
       (get-clarified-results (cons initial final) :cutoff cutoff))
    (parse-clarified-results
     (get-clarified-results    
      (clarify-query initial final)))))

;;; This parses the KEGG web page output to determine the pathways

(defun parse-clarified-results (clarified-results)
  (if (> (length clarified-results) 0)
      (remove nil (loop for line in (make-list-from-inbetween-token clarified-results (string #\newline))
			collect (loop for link in (get-links-from-line line)
				      collect (get-kegg-info-from-link link))))))

;;; This returns all the link tags from a string

(defun get-links-from-line (line)
  (remove nil 
	  (loop for link in (make-list-from-inbetween-token line " <")
		collect (get-inbetween-tag link "A"))))

#| ------ This parse-clarified-results retrieves information for each enzyme in the pathway -----
  
;;; Given a small piece of HTML that contains the pathways wanted, this function will return a list of lists for
;;; each pathway possible.  

(defun parse-clarified-results (clarified-results &aux pathway pathways)
  (dolist (link (loop with i = 0
			    as more? = (search "</A" clarified-results :start2 i)
			    until (not more?)
			    collect (get-inbetween-tag clarified-results "A" :start2 i) 
			    do
			    (setq i (+ 1 more?))) 
		(reverse pathways))
	  (if (search "dbget" link) 
	      (push (get-kegg-info-from-link link) pathway) 
	    (and (if pathway (push (reverse pathway) pathways))
		 (setq pathway NIL)))))

|#

;;; This function returns (id . description) given a link that contains both the id and description of the enzyme
;;; or protein.

(defun get-kegg-info-from-link (link &key (retrieve-enzyme 'nil))
  (let* ((name (subseq link (+ 1 (search "'" link)) (search "'" link :from-end 1)))
	 (id (subseq link (+ 1 (search ">" link)) (search "<" link :from-end 1))))
    (if (search "C" id) 
	(make-compound :id id :name name) 
      (if retrieve-enzyme 
	  (get-enzyme-info id)
	(make-enzyme :ec id :name name)))))

;;; This retrieves all the information known for a particular EC #

(defun get-enzyme-info (ec)
  (let* ((page (get-inbetween-tag 
		(CG:WEB-PAGE-CONTENTS 
		 (format nil "http://www.genome.ad.jp/dbget-bin/www_bget?enzyme+~a" ec)) "PRE"))
	 (content (get-content-inbetween-keywords (subseq page 6 (- (length page) 11))))
	 (name (get-keyword-from-list content "NAME"))
	 (class (get-keyword-from-list content "CLASS"))
	 (reaction (remove nil
			   (make-list-from-inbetween-token
			    (remove-string "           "
					   (remove #\linefeed
						   (get-keyword-from-list content "REACTION"))) ";")))
	 (substrate (clean-compound
		     (get-keyword-from-list content "SUBSTRATE")))
	 (product (clean-compound
		   (get-keyword-from-list content "PRODUCT")))
	 (comment (get-keyword-from-list content "COMMENT")))
    (make-enzyme :ec ec :name name :reaction reaction :substrate substrate :product product :class class :comment comment)))

;;; This clarifies each compound into a compound struct

(defun clean-compound (compound)
  (let* ((links (make-list-from-inbetween-token
		 (string-right-trim (string #\linefeed) compound) #\linefeed)))
    (remove nil (loop for link in links
		      until (null link)
		      as id = (subseq-token link "+" ">")
		      as name = (remove-tags link)
		      collect (if id (make-compound :id id :name name))))))

;;; This returns the cdr of a list of cons where the car matches key

(defun get-keyword-from-list (list key)
  (cdr (assoc key list :test #'equal)))

;;; This is a helper function for get-enzyme-info

(defun get-content-inbetween-keywords (page) 
  (let* ((keyword-positions (cons 0 (get-keyword-positions page))))
    (loop for i from 0 to (- (length keyword-positions) 1)
	  as content = (string-trim (string #\linefeed) 
				    (subseq page (nth i keyword-positions) (nth (+ i 1) keyword-positions)))
	  as keyword = (string-trim (string #\space) (subseq content 0 12))
	  collect (cons keyword (subseq content 12)))))

;;; This returns a list of positions on a page to help get-content-inbetween-keywords

(defun get-keyword-positions (page)
   (let* ((pos 0))
     (remove nil 
	     (loop do					     
		   (setq pos (position #\linefeed page :start (+ 1 pos)))
		   until (null pos) 
		   collect (if (not (equal (char page (+ 1 pos)) #\space)) pos)))))

;;; This function takes a query that has already been clarified (i.e. Glucose is now C00293@Glucose), and returns a
;;; small piece of HTML that contains the pathways requested.

(defun get-clarified-results (clarified-query &key cutoff)
  (incf *web-client-connections*)
  (incf *total-web-client-connections*)  
  (let* ((clarified-initial (car clarified-query))
	 (clarified-final (cdr clarified-query))
	 (query (list (cons "org_name" "all") 
		       (cons "start_select" clarified-initial) 
		       (cons "end_select" clarified-final)
		       (cons "cutoff" cutoff)
		       (cons "hierarchy" "none")
		       (cons "level" 2)
		       (cons "sort" "len")))
	 (page (net.aserve.client:do-http-request
					   "http://www.genome.ad.jp/kegg-bin/check_cpd" :method :post
					   :query query))
	 (clarified-results (if page (get-inbetween-tag page "PRE"))))
    (decf *web-client-connections*)
    (appendlog (format nil "  Retrieved pathways from ~a to ~a~%" clarified-initial clarified-final))
    (if page (subseq clarified-results 6 (- (length clarified-results) 6)))))

;;; This piece of code takes as input the names of two proteins and clarifies which particular two proteins you
;;; desire specifically from the KEGG database.

(defun clarify-query (initial final)
  (let* ((query (list (cons "org_name" "all") 
		      (cons "start_input" initial) 
		      (cons "end_input" final)))
	 (clarified-query-page (net.aserve.client:do-http-request
			      "http://www.genome.ad.jp/kegg-bin/check_cpd" :method :post
			      :query query))
	 (clarified-initial (determine-selected clarified-query-page "initial"))
	 (clarified-final (determine-selected clarified-query-page "final")))
	 (cons clarified-initial clarified-final)))

;;; This function helps clarify the query by returning which particular protein is pre-selected by KEGG.

(defun determine-selected (page query)
  (let* ((options (get-inbetween-tag (subseq page (search query page)) "SELECT"))
	 (rt-pos (- (search "SELECTED" options) 2)) 
	 (lf-pos (+ (search "VALUE" (subseq options 0 rt-pos) :from-end 1) 7))) 
    (subseq options lf-pos rt-pos)))

(defun local-find-shortest-pathway-between-ec (initial-ec final-ec &key (cutoff-max 5))
  (loop for cutoff from 1 to cutoff-max
	as pathways = (local-find-pathway-between-ec initial-ec final-ec :cutoff cutoff)
	until pathways
	finally (return pathways)))

(defun local-find-pathway-between-ec (initial-ec final-ec &key (cutoff 5))
  (let (results)
    (declare (special results))
    (traverse-pathway-recursively (list initial-ec) final-ec (- cutoff 1))
    (traverse-pathway-recursively (list (concatenate 'string initial-ec "*")) final-ec (- cutoff 1))
    results))

(defun traverse-pathway-recursively (pathway final-ec cutoff)
  (if (equal (car pathway) final-ec)
      (push (reverse pathway) results))
  (if (not (zerop cutoff))
      (loop for ec in (car (gethash (car pathway) *ec-pathways*))
	    when (not (or (member ec pathway :test #'equal) 
			  (member (concatenate 'string ec "*") pathway :test #'equal)
			  (member (string-right-trim "*" ec) pathway :test #'equal)))		      
	    do (traverse-pathway-recursively (cons ec pathway) final-ec (- cutoff 1)))))

(defun update-ligand ()
  (let* ((compound-file (file-contents "c:\\biolingua\\ligand\\compound"))
	 (compound-ht (make-hash-table :test #'equal))
	 (enzyme-file (file-contents "c:\\biolingua\\ligand\\enzyme"))
	 (ec-products-ht (make-hash-table :test #'equal))
	 (substrates-ec-ht (make-hash-table :test #'equal))
    	 (ec-pathways-ht (make-hash-table :test #'equal)))
    (loop for entry in (make-list-from-inbetween-token compound-file "///" :start-with-length 't)
	  as items = (get-items-from-entry entry)
	  as compound-id = (second (assoc "ENTRY" items :test #'equal))
	  as names = (cdr (assoc "NAME" items :test #'equal))
	  do
	  (loop for name in names
		do (setf (gethash name compound-ht) compound-id)))
    (loop for entry in (make-list-from-inbetween-token enzyme-file "///" :start-with-length 't)
	  as items = (get-items-from-entry entry)
	  as ec-num = (subseq (second (assoc "ENTRY" items :test #'equal)) 3)
	  as backword-ec-num = (concatenate 'string ec-num "*")
	  as substrates = (replace-with-compound-ids (cdr (assoc "SUBSTRATE" items :test #'equal)) compound-ht)
	  as products = (replace-with-compound-ids (cdr (assoc "PRODUCT" items :test #'equal)) compound-ht)
	  do
	  (setf (gethash ec-num ec-products-ht) products)
	  (setf (gethash backword-ec-num ec-products-ht) substrates)
	  (loop for substrate in substrates
		do (setf (gethash substrate substrates-ec-ht)
			 (excl::list-remove-duplicates-equal
			  (cons ec-num (gethash substrate substrates-ec-ht)))))
	  (loop for substrate in products
		do (setf (gethash substrate substrates-ec-ht)
			 (excl::list-remove-duplicates-equal
			  (cons backword-ec-num (gethash substrate substrates-ec-ht))))))
    (maphash #'(lambda (ec products) 
		 (setf (gethash ec ec-pathways-ht)
		       (excl::remove-equal ec
					   (excl::list-remove-duplicates-equal
					    (loop for product in products
						  append (gethash product substrates-ec-ht))))))
	     ec-products-ht)
    (write-hashtable *ec-pathways-file* ec-pathways-ht)))

(defun replace-with-compound-ids (name-list compound-ht)
  (loop for name in name-list
	as compound-id = (gethash name compound-ht)
	when (not (member compound-id *general-compound-list* :test #'equal))
	collect compound-id))  

(defun get-items-from-entry (entry)
  (let* ((entry (string-left-trim (string #\newline) entry))
	 (label-pos (cons -1
			  (loop with pos = 0
				as pos = (position #\newline entry :start (if pos (+ pos 1) 0))
				until (not pos) 
				when (not (eq #\space (char entry (min (- (length entry) 1) (+ pos 1)))))
				collect pos)))
	 (items (loop for i from 0 to (- (length label-pos) 2)
		      as start-pos = (+ (nth i label-pos) 1)
		      as end-pos = (nth (+ i 1) label-pos)
		      as label = (string-trim " " (subseq entry start-pos (+ start-pos 12)))
		      collect (cons label (make-list-from-inbetween-token 
					   (subseq entry (+ start-pos 12) (min (length entry) end-pos)) #\newline)))))
    items))

#| This is to create a EC pathway database for the ligand map files
(defun update-ligand ()
  (let* ((ec-products-ht (make-hash-table :test #'equal))
	 (substrates-ec-ht (make-hash-table :test #'equal))
    	 (ec-pathway-ht (make-hash-table :test #'equal))
	 (files (list-map-files "c:\\biolingua\\ligand\\reaction.main")))
    (loop for file in files
	  do (with-open-file (stream file :direction :input)
			     (loop for line = (read-line stream nil nil)
				   until (null line)
				   as ec = (subseq-token line ":" ":")
				   as reaction = (parse-reaction (subseq-token line " " (string #\newline)))
				   as substrates = (first reaction)
				   as products = (second reaction)
				   when (not (equal ec "0.0.0.0"))
				   do
				   (setf (gethash ec ec-products-ht) (excl::list-remove-duplicates-equal
								      (append (gethash ec ec-products-ht) products)))
				   (loop for substrate in substrates
					 do (setf (gethash substrate substrates-ec-ht)
						  (excl::list-remove-duplicates-equal
						   (cons ec (gethash substrate substrates-ec-ht))))))))
    (maphash #'(lambda (ec products) 
		 (setf (gethash ec ec-pathway-ht)
		       (excl::remove-equal ec
					   (excl::list-remove-duplicates-equal
					    (loop for product in products
						  append (gethash product substrates-ec-ht))))))
	     ec-products-ht)
    ec-pathway-ht))

(defun parse-reaction (reaction)
  (let* ((reaction-list (make-list-from-inbetween-token reaction "="))
	 (left (string-right-trim " <" (first reaction-list)))
	 (goes-left (equal "<" (subseq (first reaction-list) (- (length (first reaction-list)) 1))))
	 (left-compounds (make-list-from-inbetween-token left " + " :start-with-length 't))
	 (right (string-left-trim " >" (second reaction-list)))
	 (goes-right (equal ">" (subseq (second reaction-list) 0 1)))
	 (right-compounds (make-list-from-inbetween-token right " + " :start-with-length 't))
	 (substrates (append (if goes-right
				 left-compounds)
			     (if goes-left
				 right-compounds)))
	 (products (append (if goes-right
			       right-compounds)
			   (if goes-left
			       left-compounds))))
    (list substrates products)))

(defun list-map-files (directory)
  (loop for num from 0 to 999
	as num-string = (format nil "~a" num)
	as file = (probe-file (format nil "~a\\map~a.rea" directory (prepend-with-zeros num-string 5)))
	when file
	collect file))


|#