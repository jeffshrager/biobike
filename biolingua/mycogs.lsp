(defvar *mycogs-results* (make-hash-table)) 
(clrhash *mycogs-results*)
(defvar *mycogs-result-num*)
(setq *mycogs-result-num* 0)

(defun get-url-of-similar-sequences-mp (sequence left-pos right-pos)
  (setq *web-log* 't)
  (incf *mycogs-result-num*)
  (let ((id (format nil "mycogs ~a" *mycogs-result-num*)))
    (setf (gethash *mycogs-result-num* *mycogs-results*) (make-result :id id))
    (mp:process-run-function id #'get-url-of-similar-sequences sequence left-pos right-pos *mycogs-result-num* *mycogs-results*))
  *mycogs-result-num*)

(defun get-url-of-similar-sequences (sequence left-pos right-pos result-num query-results)
  (declare (special result-num))
  (declare (special query-results))
  (let* ((blast (blast sequence :hitlist-size "100"))
	 (hits-within-target (list-hits-within-target (biolisp::blast-hits blast) left-pos right-pos))
	 (query (concatenate 'string 
			     "http://www.ncbi.nlm.nih.gov/entrez/query.fcgi?cmd=Text&db=Nucleotide&dopt=fasta"
			     (concatenate-string-list (loop for hit in hits-within-target
							    as id = (second (make-list-from-inbetween-token 
									     (biolisp::hit-id hit) "|"))
							    collect (concatenate 'string "&uid=" id)))))
	 (result (gethash result-num *mycogs-results*))
	 )
    (push (cons 'blast blast) (result-temp result))
    (setf (result-content result) (format nil "<a href=\"~a\">Download fasta file of similar sequences</a>" query))
    ))
  
(defun list-hits-within-target (hits left-pos right-pos)
  (loop for hit in hits
	when (and (<= left-pos (biolisp::hit-q-start hit))
		  (>= right-pos (biolisp::hit-q-end hit))
		  (< (biolisp::hit-s-start hit) 3000)
		  )
	collect hit))

(publish-file :path "/mycogs" :file (merge-pathnames "mycogs\\index.html" *load-truename*))
(publish-file :path "/mycogs/mycogs-query.html" :file (merge-pathnames "mycogs\\mycogs-query.html" *load-truename*))

(defun publish-mycogs-query ()
  (publish :path "/cgi/mycogs/query"
	   :content-type "text/html"
	   :function
	   #'(lambda (req ent)
	       (let* ((sequence (cdr (assoc "sequence" (request-query req) :test #'equal)))
		      (left-pos (parse-integer (cdr (assoc "leftpos" (request-query req) :test #'equal))))
		      (right-pos (parse-integer (cdr (assoc "rightpos" (request-query req) :test #'equal))))
		      (refresh (cdr (assoc "refresh" (request-query req) :test #'equal))))
		 (with-http-response (req ent)
				     (with-http-body (req ent)
						     (setq *web-log* 't)
						     (if* (and sequence left-pos right-pos)
							  then
							  (format *html-stream* 
								  "<META HTTP-EQUIV=\"refresh\" content=\"0; url=/cgi/mycogs/status?refresh=~a&id=~a\">"
								  refresh 
								  (get-url-of-similar-sequences-mp 
								   sequence 
								   left-pos 
								   right-pos))
							  else
							  (html
							   (:html
							    (:body "Missing sequence, or left, or right positions.")
							    (:princ sequence)
							    (:princ left-pos)
							    (:princ right-pos))))))))))
(publish-mycogs-query)


(defun publish-mycogs-status ()
  (publish :path "/cgi/mycogs/status"
	   :content-type "text/html"
	   :function
	   #'(lambda (req ent)
	       (let* ((id (parse-integer (cdr (assoc "id" (request-query req)
						     :test #'equal))))
		      (refresh (cdr (assoc "refresh" (request-query req) :test #'equal)))
		      (result (gethash id *mycogs-results*)))
		 (with-http-response (req ent)
				     (with-http-body (req ent)
						     (if* result
							  then
							  (if* (result-content result)
							       then
							       (html (:html (:body (:princ (result-content result)))))
							       else
							       (if (not (equal refresh "-1"))
								   (format *html-stream* 
									   "<META HTTP-EQUIV=\"refresh\" content=\"~a; url=/cgi/mycogs/status?refresh=~a&id=~a\">"
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

(publish-mycogs-status)