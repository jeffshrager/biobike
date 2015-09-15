(defvar *weblog-location* "c:\\biolingua\\weblog.txt")
(defvar log-visitor-lock (mp:make-process-lock))
 
(setq *read-request-timeout* 1000
      NET.ASERVE::*READ-REQUEST-BODY-TIMEOUT* 1000
      NET.ASERVE::*READ-REQUEST-TIMEOUT* 1000
      NET.ASERVE:*HTTP-RESPONSE-TIMEOUT* 1000
      )

;;; Do this for testing:
;(setq *standard-output* *html-stream*) 

(defun biostart ()
  (import-kb)
  (import-pathway)
  (start))

(publish-file :path "/" :file (merge-pathnames "biolingua.html" *load-truename*))

;;; -------------------------------------------------- GENE ---------------------------------------------------

(publish-file :path "/gene" :file (merge-pathnames "gene\\index.html" *load-truename*))
(publish-file :path "/gene/gene-query.html" :file (merge-pathnames "gene\\gene-query.html" *load-truename*))

(defun publish-gene-query ()
  (publish :path "/cgi/gene/query"
	   :content-type "text/html"
	   :function
	   #'(lambda (req ent)
	       (let* ((input (get-post-query-input req))
		      (file (cdr (assoc "file" input :test #'equal)))
		      (sequence (if (equal file "") 
				    (cdr (assoc "sequence" input :test #'equal))
				  file))
		      (refresh (cdr (assoc "refresh" input :test #'equal)))
		      (expect (cdr (assoc "expect" input :test #'equal)))
		      (cutoff (cdr (assoc "cutoff" input :test #'equal)))
		      (pathways (cdr (assoc "pathways" input :test #'equal))))
		 (with-http-response (req ent)
				     (with-http-body (req ent)
						     (setq *web-log* 't)
						     (if* sequence
							  then
							  (log-visitor req sequence)
							  (if* (position #\> sequence)
							       then
							       (if* (equal pathways "yes")
								    then
								    (format *html-stream* 
									    "<META HTTP-EQUIV=\"refresh\" content=\"0; url=/cgi/gene/pathway-status?refresh=~a&id=~a\">"
									    refresh 
									    (determine-metabolic-pathways-mp sequence :expect expect :cutoff cutoff))
								    else
								    (format *html-stream* 
									    "<META HTTP-EQUIV=\"refresh\" content=\"0; url=/cgi/gene/batch-status?refresh=~a&id=~a\">"
									    refresh 
									    (what-are-these-genes-mp? sequence :expect expect)))
							       else
							       (format *html-stream* 
								       "<META HTTP-EQUIV=\"refresh\" content=\"0; url=/cgi/gene/status?refresh=~a&id=~a\">"
								       refresh 
								       (what-is-this-gene-mp? sequence :expect expect)))
							  else
							  (html
							   (:html
							    (:body "No sequence entered."))))))))))


(publish-gene-query)

(defun log-visitor (request query)
  (let ((time (get-universal-time))
	(ip-address (socket:ipaddr-to-dotted (socket:remote-host (request-socket request)))))
    (mp:with-process-lock (log-visitor-lock)
			  (with-open-file (log *weblog-location*
					       :direction :output 
					       :if-exists :append
					       :if-does-not-exist :create)
					  (format log "(~s~%  ~s~%  ~s)~%" ip-address time query)))))
  
(defun get-post-query-input (req)
  (loop
   until (null (setq h (get-multipart-header req)))
   collect (cons 
	    (cdadr (cdadr (assoc ':content-disposition h)))
	    (get-output-stream-string
	     (let* ((str (make-string-output-stream))
		    (buffer (make-array 4096
					:element-type 'character)))
	       (loop (let ((count (get-multipart-sequence 
				   req 
				   buffer)))
		       (if* (null count) then (return))
		       (write-sequence buffer str :end count)))
	       str)))))



(defun publish-gene-status ()
  (publish :path "/cgi/gene/status"
	   :content-type "text/html"
	   :function
	   #'(lambda (req ent)
	       (let* ((id (parse-integer (cdr (assoc "id" (request-query req)
						     :test #'equal))))
		      (refresh (cdr (assoc "refresh" (request-query req) :test #'equal)))
		      (result (gethash id *gene-results*)))
		 (with-http-response (req ent)
				     (with-http-body (req ent)
						     (if* result
							  then
							  (if* (result-content result)
							       then
							       (print-gene-result (result-content result) id)
							       else
							       (if (not (equal refresh "-1"))
								   (format *html-stream* 
									   "<META HTTP-EQUIV=\"refresh\" content=\"~a; url=/cgi/gene/status?refresh=~a&id=~a\">"
									   refresh refresh id))
							       (html
								(:html
								 (:body 
								  (:pre (format *html-stream* "~a" 
										(result-log result)))))))
							  else
							  (html
							   (:html
							    (:body
							     (format *html-stream* "There is no query associated with id ~a" 
								     id)))))))))))

(publish-gene-status)

(defun publish-gene-pathway-status ()
  (publish :path "/cgi/gene/pathway-status"
	   :content-type "text/html"
	   :function
	   #'(lambda (req ent)
	       (let* ((id (parse-integer (cdr (assoc "id" (request-query req)
						     :test #'equal))))
		      (refresh (cdr (assoc "refresh" (request-query req) :test #'equal)))
		      (result (gethash id *genes2pathways-results*)))
		 (with-http-response (req ent)
				     (with-http-body (req ent)
						     (if* result
							  then
							  (if* (result-content result)
							       then
							       (print-gene-pathway-result (result-content result) id)
							       else
							       (if (not (equal refresh "-1"))
								   (format *html-stream* 
									   "<META HTTP-EQUIV=\"refresh\" content=\"~a; url=/cgi/gene/pathway-status?refresh=~a&id=~a\">"
									   refresh refresh id))
							       (html
								(:html
								 (:body 
								  (:pre (format *html-stream* "~a" 
										(result-log result)))))))
							  else
							  (html
							   (:html
							    (:body
							     (format *html-stream* "There is no query associated with id ~a" 
								     id)))))))))))

(publish-gene-pathway-status)


(defun publish-gene-batch-status ()
  (publish :path "/cgi/gene/batch-status"
	   :content-type "text/html"
	   :function
	   #'(lambda (req ent)
	       (let* ((id (parse-integer (cdr (assoc "id" (request-query req)
						     :test #'equal))))
		      (refresh (cdr (assoc "refresh" (request-query req) :test #'equal)))
		      (result (gethash id *batch-results*)))
		 (with-http-response (req ent)
				     (with-http-body (req ent)
						     (if* result
							  then
							  (if* (not (loop for (name . gene-id) in (result-log result)
									  do (if (not (result-content 
										       (gethash gene-id *gene-results*)))
										 (return 'nil))
									  finally (return 't)))
							       then
							       (if (not (equal refresh "-1"))
								   (format *html-stream* 
									   "<META HTTP-EQUIV=\"refresh\" content=\"~a; url=/cgi/gene/batch-status?refresh=~a&id=~a\">"
									   refresh refresh id))
							       else
							       (html
								((:a :href (format nil "/cgi/gene/batch.lsp?id=~a" id))
								 "Download all results as a lisp data structure")
								(:p)))
							  (print-batch-status result)
							  else
							  (html
							   (:html
							    (:body
							     (format *html-stream* "There is no batch associated with id ~a" 
								     id)))))))))))

(publish-gene-batch-status)



(defun publish-gene-download ()
  (publish :path "/cgi/gene/gene.lsp"
	   :content-type "text/lsp"
	   :function
	   #'(lambda (req ent)
	       (let* (;(*print-pretty* 'nil)
		      (*print-level* 'nil)
		      (id (parse-integer (cdr (assoc "id" (request-query req)
						     :test #'equal)))))
		 (with-http-response (req ent)
				     (with-http-body (req ent)
						     (if id
							 (prin1 
							  (result-content (gethash id *gene-results*)) *html-stream*))))))))
(publish-gene-download)
		 

(defun publish-batch-download ()
    (publish :path "/cgi/gene/batch.lsp"
	   :content-type "text/lsp"
	   :function
	   #'(lambda (req ent)
	       (let* (;(*print-pretty* 'nil)
		      (*print-level* 'nil)
		      (id (parse-integer (cdr (assoc "id" (request-query req)
						     :test #'equal))))
		      (result (gethash id *batch-results*)))
		 (with-http-response (req ent)
				   (with-http-body (req ent)
						   (if id
						       (html
							(:prin1 (loop for (name . gene-id) in (result-log result)
								      collect 
								      (cons name
									    (result-content 
									     (gethash gene-id *gene-results*)))))))))))))
(publish-batch-download)


(defun print-batch-status (batch-result &key (extended))
  (html
   (:html
    ((:table :border "0" :cellpadding "0" :cellspacing "0")
     (:tr
      (:td (:b "#"))
      (:td (:b "fasta id")))
     (loop with num = 0
	   for (name . id) in (result-log batch-result)
	   as result = (gethash id *gene-results*)
	   do
	   (if* extended
		then
		(html (:b (:princ name))
		     (if (result-content result)
			 (print-gene-result (result-content result) id)
		       (html (:pre (:princ (result-log result)))))
		     (:br)
		     (:br)
		     (:br))
		else
		(incf num)
		(html
		 (:tr
		  ((:td :width "20") (format *html-stream* "~a." num))
		  (:td (format *html-stream* "~a" name))
		  (if (result-content result)
		      (html ((:td :width "100") ((:p :align "center") 
						 ((:a :href (format nil "/cgi/gene/status?id=~a" id)) "HTML")))
			    ((:td :width "100") ((:p :align "center")
						 ((:a :href (format nil "/cgi/gene/gene.lsp?id=~a" id)) "Lisp"))))
		    (html
		     ((:td :width "100") ((:p :align "right") 
					  ((:a :href (format nil "/cgi/gene/status?id=~a" id))
					   (format *html-stream* "~a% done...~%<br>" 
						   (min 100 
							(round (* 
								(/ (length (result-log result)) 700) 
								100)))))))))))))))))

(defun print-gene-result (gene id)
  (html
   (:html
    (:body
     (:pre (:b "Sequence: ") (:princ (word-wrap (gene-sequence gene) 70)))
     (:p)
     ((:table :border "1" :cellpadding "3" :cellspacing "3")
      (:tr 
       (:td (:b "GO #"))
       (:td (:b "EC #"))
       (:td (:b "e-value"))
       (:td (:b "accuracy"))
       ((:td :width "200") (:b "blast description"))
       (:td (:b "ontology tree")))
      (loop for go-hit in (gene-go-hits gene)
	    do
	    (html 
	     (:tr
	      (:td ((:a :href (format nil "http://toy.lbl.gov:8888/cgi-bin/go.cgi?query=GO:~a&view=details" 
				      (go-hit-go-num go-hit)))
		    (:princ (go-hit-go-num go-hit))))
	      (:td ((:a :href (format nil "http://www.expasy.ch/cgi-bin/nicezyme.pl?~a"
				      (go-hit-ec-num go-hit)))
		    (:princ (go-hit-ec-num go-hit))))
	      (:td (:princ (go-hit-e-value go-hit)))
	      (:td (:princ (go-hit-accuracy go-hit)))
	      ((:td :width "200") (:princ (go-hit-product-description go-hit)))
	      (:td (print-go-tree "" (go-hit-go-tree go-hit)))))))
     (:br)
     (:br)
     (:br)
     ((:table :border "1" :cellpadding "3" :cellspacing "3")
      (:tr 
       (:td (:b "EC #"))
       (:td (:b "e-value"))
       (:td (:b "accuracy"))
       ((:td :width "200") (:b "blast description"))
       (:td (:b "EC additional information")))
      (loop for ec-hit in (gene-ec-hits gene)
	    do
	    (html 
	     (:tr
	      (:td ((:a :href (format nil "http://www.expasy.ch/cgi-bin/nicezyme.pl?~a"
					      (ec-hit-ec-num ec-hit)))
		    (:princ (ec-hit-ec-num ec-hit))))
	      (:td (:princ (ec-hit-e-value ec-hit)))
	      (:td (:princ (ec-hit-accuracy ec-hit)))
	      ((:td :width "200") (:princ (ec-hit-product-description ec-hit)))
	      (:td (print-ec-info (ec-hit-ec-info ec-hit)))))))
     (if (gene-domains gene)
	 (html
	  (:br)
	  (:br)
	  (:br)
	  ((:table :border "1" :cellpadding "3" :cellspacing "3")
	   (:tr
	    (:td (:b "domain id"))
	    (:td (:b "name"))
	    (:td (:b "e-value"))
	    (:td (:b "alignment")))
	   (loop for domain in (gene-domains gene)
		 do
		 (html
		  (:tr
		   (:td ((:a :href 
			     (format nil 
				     "http://www.ncbi.nlm.nih.gov/Structure/cdd/cddsrv.cgi?uid=~a" 
				     (biolisp::domain-id domain))) (:princ (biolisp::domain-id domain))))
		   (:td (:princ (biolisp::domain-name domain)))
		   (:td (:princ (biolisp::domain-expected-value domain)))
		   (:td (:princ (biolisp::domain-alignment domain)))))))))
     (:br)
     (:br)
     (:br)
     ((:a :href (format nil "/cgi/gene/gene.lsp?id=~a" id)) (:princ "Download as a lisp data structure"))))))

(defun print-ec-info (ec-info)
  (html
   (:u "Description:") (:princ (make-list-into-string (ec-description ec-info)))
   (:br (:u "Alternate names:") (:princ (make-list-into-string (ec-alternate-names ec-info))))
   (:br (:u "Catalytic activity:") (:princ (make-list-into-string (ec-catalytic-activity ec-info))))
   (:br (:u "Cofactors:") (:princ (make-list-into-string (ec-cofactors ec-info))))
   (:br (:u "Diseases:") (:princ (make-list-into-string (ec-diseases ec-info))))
   (:br (:u "Comments:")(:princ (make-list-into-string (ec-comments ec-info))))))

(defun print-go-tree (pre go-tree)
  (if (listp (car go-tree))
      (loop for parent in go-tree
	    do
	    (format *html-stream* "<BR>~a~a~%" pre (caar parent))
	    (print-go-tree (concatenate 'string pre "&nbsp;&nbsp;") (cdr parent)))
    (progn (format *html-stream* "~a~%" (car go-tree))
	   (print-go-tree (concatenate 'string pre "&nbsp;&nbsp;") (cdr go-tree)))))
  

(defun make-list-into-string (list &aux string)
  (if list
      (loop for item in list
	    as string = (concatenate 'string string " " item)
	    finally (return string))
    ""))

(defun print-gene-pathway-result (pathways id)
  (html
   (:html
    (:body
     (format *html-stream* "<b>~a pathways found</b>" (length pathways))
     (:p)
     (:pre
      (loop for pathway in pathways
	    do
	    (loop for item in (cdr pathway)
		  as gene-num = (car item)
		  do		  
		  (if gene-num
		      (html ((:a :href (format nil "/cgi/gene/status?id=~a" gene-num)) (format *html-stream*
											       "[~a]" (cdr item))))
		    (html ((:a :href (format nil "http://www.expasy.ch/cgi-bin/nicezyme.pl?~a" (cdr item))) 
			   (:princ (cdr item)))))
		  (html (:princ " ")))
	    (html (:br))))))))


;;; -------------------------------------------------- KEGG --------------------------------------------------

(publish-file :path "/pathway" :file (merge-pathnames "pathway\\index.html" *load-truename*))
(publish-file :path "/pathway/pathway-query.html" :file (merge-pathnames  "pathway\\pathway-query.html" *load-truename*))

(defun publish-pathway-query ()
  (publish :path "/cgi/pathway/query"
	   :content-type "text/html"
	   :function
	   #'(lambda (req ent)
	       (let* ((initial (cdr (assoc "initial" (request-query req) :test #'equal)))
		      (final (cdr (assoc "final" (request-query req) :test #'equal)))
		      (refresh (cdr (assoc "refresh" (request-query req) :test #'equal)))
		      (max-links (parse-integer (cdr (assoc "links" (request-query req) :test #'equal)))))
		 (with-http-response (req ent)
				     (with-http-body (req ent)
						     (setq *web-log* 't)
						     (if* (and initial final)
							  then
							  (format *html-stream* 
								  "<META HTTP-EQUIV=\"refresh\" content=\"0; url=/cgi/pathway/status?refresh=~a&id=~a\">"
								  refresh 
								  (find-pathway-between-ec-mp initial final 
											      :max-links 
											      (if (= max-links -1) 
												  'nil 
												max-links)))
							  else
							  (html
							   (:html
							    (:body "Missing initial or final EC number."))))))))))
(publish-pathway-query)

(defun publish-pathway-status ()
  (publish :path "/cgi/pathway/status"
	   :content-type "text/html"
	   :function
	   #'(lambda (req ent)
	       (let* ((id (parse-integer (cdr (assoc "id" (request-query req)
						     :test #'equal))))
		      (refresh (cdr (assoc "refresh" (request-query req) :test #'equal)))
		      (result (gethash id *pathway-results*)))
		 (with-http-response (req ent)
				     (with-http-body (req ent)
						     (if* result
							  then
							  (if* (result-content result)
							       then
							       (print-pathway-result (result-content result) id)
							       else
							       (if (not (equal refresh "-1"))
								   (format *html-stream* 
									   "<META HTTP-EQUIV=\"refresh\" content=\"~a; url=/cgi/pathway/status?refresh=~a&id=~a\">"
									   refresh refresh id))
							       (html
								(:html
								 (:body 
								  (:pre (format *html-stream* "~a" 
										(result-log result)))))))
							  else
							  (html
							   (:html
							    (:body
							     (format *html-stream* "There is no query associated with id ~a" 
								     id)))))))))))
						       

(publish-pathway-status)


(defun publish-pathway-download ()
  (publish :path "/cgi/pathway/pathway.lsp"
	   :content-type "text/lsp"
	   :function
	   #'(lambda (req ent)
	       (let* (;(*print-pretty* 'nil)
		      (*print-level* 'nil)
		      (id (parse-integer (cdr (assoc "id" (request-query req)
						     :test #'equal)))))
		 (with-http-response (req ent)
				     (with-http-body (req ent)
						     (if id
							 (prin1 
							  (result-content (gethash id *pathway-results*)) *html-stream*))))))))

(publish-pathway-download)

(defun html-space (length)
  (when (> length 1) (html-space (- length 1)) (princ "&nbsp;" *html-stream*)))
  

(defun print-pathway-result (result id)
  (if* (equal result "none")
       then
       (html (:princ "There are no pathways that match your query. Try increasing the max number of queries."))
       else
       (loop with num = 0
	     for pathway in result 
	     do 
	     (html 
	      (:p)
	      ((:font :size "2" :face "Courier New") 
;;;	  (format *html-stream* "~a. " (incf num)) 
	       (loop for (c-ec-id . name) in pathway 
		     with item-num = 0
		     do 
		     (incf item-num)
		     (html
		      ((:a :href (format nil 
					 (if (position #\C c-ec-id)
					     "http://www.genome.ad.jp/dbget-bin/www_bget?cpd:~A"
					   "http://www.expasy.ch/cgi-bin/nicezyme.pl?~a")
					 c-ec-id)) (:princ c-ec-id))
		      (format *html-stream* ": ~a" name))
		     (if (< item-num (length pathway)) (html (:br (html-space item-num))))))))
       (html
	(:br)
	(:br)
	(:br)
	((:a :href (format nil "/cgi/pathway/pathway.lsp?id=~a" id)) (:princ "Download as a lisp data structure")))))

;;; -------------------------------------------------- DISCO --------------------------------------------------

(publish-file :path "/disco" :file (merge-pathnames "disco\\index.html" *load-truename*))
(publish-file :path "/disco/disco-query.html" :file (merge-pathnames "disco\\disco-query.html" *load-truename*))

(defun publish-disco ()
  (publish :path "/cgi/disco"
	   :content-type "text/html"
	   :function
	   #'(lambda (req ent)
	       (let* ((model (cdr (assoc "Model" (request-query req) :test #'equal))))
		 (print (request-query req))
		 (with-http-response (req ent)
				     (with-http-body (req ent)
						     (if* model
							  then 
							  (html (:html
								 (:body (:pre (:prin1-safe (disco-for-web model))))))
							  else 
							  (html (:html
								 (:body "No model!"))))))))))

(publish-disco)
