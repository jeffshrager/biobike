(in-package biolingua)

;;; These are globals for results found when determining the pathways for a set of sequences
(defvar *genes2pathways-results*)
(setq *genes2pathways-results* (make-hash-table))
(defvar *genes2pathways-result-num*)
(setq *genes2pathways-result-num* 0)

(defun test-bio ()
  (determine-metabolic-pathways-mp (file-contents "c:\\biolingua\\sequences\\glycolysis.txt")))

(defun determine-metabolic-pathways-mp (sequences &key (expect "1E-10") cutoff)
  (incf *genes2pathways-result-num*)
  (let ((id (format nil "biotelligence ~a" *genes2pathways-result-num*)))
    (setf (gethash *genes2pathways-result-num* *genes2pathways-results*) (make-result :id id))
    (mp:process-run-function id 
			     #'determine-metabolic-pathways 
			     sequences
			     expect
			     cutoff
			     *genes2pathways-result-num* 
			     *genes2pathways-results*))
  *genes2pathways-result-num*)

(defun determine-metabolic-pathways (sequences expect cutoff result-num query-results)
  (declare (special result-num))
  (declare (special query-results))
  (let* ((genes (get-info-from-sequences sequences :expect expect))
	 (ec-list (remove nil (list-top-ec-num-from-genes genes)))
	 (ec-and-gene-num (list-ec-and-gene-num genes))
	 (pathways-in-common (find-pathways-in-common ec-list :cutoff cutoff))
	 (tagged-pathways 
	  (loop for pathway in pathways-in-common
		collect (loop for ec in pathway
			      collect (cons (if (member ec ec-list :test #'equal) 
						(cdr (assoc ec ec-and-gene-num :test #'equal))
					      'nil) ec)))))
    (setf (result-content (gethash result-num *genes2pathways-results*))
	  (sort
	   (loop for pathway in tagged-pathways
		 collect (cons 
			  (loop for item in pathway
				when (car item)
				sum 1)
			  pathway))
	   #'(lambda (a b) (> (car a) (car b)))))))

(defun list-ec-and-gene-num (genes)
  (loop for gene in genes
	as gene-num = (caar gene)
	as gene-fasta-header = (cadr gene)
	as ec = (third (second gene))
	collect (cons ec gene-num)))
  
(defun find-pathways-in-common (ec-list &key cutoff)
  (let* ((shrinking-ec-list ec-list)
	 (pathways-already-tried-ht (make-hash-table :test #'equal))
	 (good-pathways))
    (appendlog (format nil "EC #'s found from sequences: ~a~%" ec-list))
    (appendlog (format nil "Maximum # of pairs to check: ~a~%" (sumation (length ec-list))))
    (loop as to-remove-list = 'nil
	  as pair = (next-pair shrinking-ec-list pathways-already-tried-ht)
	  until (or (not pair) (< (length shrinking-ec-list) 2))
	  as pathways = (remove-stars (local-find-shortest-pathway-between-ec 
				       (car pair) 
				       (cdr pair) 
				       :cutoff-max (parse-integer cutoff)))
	               ;(get-ecs-between-two-ec (car pair) (cdr pair) :cutoff cutoff)
	  do
	  (loop for pathway in pathways
		as in-common = (intersection-equal pathway ec-list)
		when in-common
		do
		(setf to-remove-list (append to-remove-list in-common))
		(push pathway good-pathways))
	  (loop for to-remove in (excl::remove-equal (cdr pair)
						     (excl::remove-equal (car pair) to-remove-list))
		do
		(setf shrinking-ec-list (excl::list-remove-equal to-remove shrinking-ec-list)))
	  (setf to-remove-list 'nil)
	  (appendlog (format nil "# of pathways found from ~a to ~a: ~a~%"
			     (car pair)
			     (cdr pair)
			     (length pathways)))
	  (appendlog (format nil "# of overall good pathways: ~a~%"
			     (length good-pathways)))
	  (appendlog (format nil "EC #'s still to check: ~a~%"
			     shrinking-ec-list))
	  (appendlog (format nil "Maximum # of pairs still to check: ~a~%"
			     (sumation (length shrinking-ec-list))))
  	  (appendlog "<br>" :no-p 't)
	  finally (return good-pathways))))

(defun next-pair (shrinking-ec-list pathways-already-tried-ht)
  (let ((pair (recurse-ec-list shrinking-ec-list pathways-already-tried-ht)))
    (setf (gethash pair pathways-already-tried-ht) 't)
    (if pair (appendlog (format nil "Checking pathways between ~a and ~a" (car pair) (cdr pair))))
    pair))
  
(defun recurse-ec-list (shrinking-ec-list pathways-already-tried-ht)
  (if shrinking-ec-list
      (let ((pair (loop for ec in (cdr shrinking-ec-list)
		       as pair = (cons (car shrinking-ec-list) ec)
		       do (if (not (gethash pair pathways-already-tried-ht))
			      (return pair))
		       finally (return 'nil))))
	(if pair pair
	  (recurse-ec-list (cdr shrinking-ec-list) pathways-already-tried-ht)))))
 
(defun get-ecs-between-two-ec (initial-ec final-ec &key cutoff max-links)
  (let ((pathway-num (find-pathway-between-ec-mp initial-ec final-ec :cutoff cutoff :max-links max-links)))
    (loop as pathways = (result-content (gethash pathway-num *pathway-results*))
	  until pathways
	  do 
	  (sleep 1)
	  (appendlog "." :no-p 't)
	  finally (return (if (equal pathways "none") 'nil
			    (list-ecs-in-pathway pathways))))))

(defun list-top-ec-num-from-genes (genes)
  (excl::list-remove-duplicates-equal
   (loop for gene in genes
	 collect (third (second gene)))))

(defun list-ecs-in-pathway (pathways)
  (excl::list-remove-duplicates-equal
   (loop for pathway in pathways
	 collect
	 (loop for item in pathway
	       when (position #\. (car item))
	       collect (car item)))))	      

(defun sumation (num)
  (loop for x from 0 to num
	sum x))