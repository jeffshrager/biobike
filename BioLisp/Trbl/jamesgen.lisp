(in-package :pb)

;;; Create the output that James can use to run stats.  


;;; This is needed in order that the report can be done efficiently.
;;; (Doing this multiple times won't hurt bcs it pushnew's.)
(defun bubble-pubs-to-go-frames ()
  (loop for year in *year-frames*
	do (loop for pub in (#^pubs year)
		 do (loop for gene in (#^genes pub)
			  do (loop for go in (#^Go-Id gene)
				   do (pushnew pub (#^pubs go)))))))

(defun jamesgen1 (&key (filename "james1.dat") &aux (tab #\tab))
  (format t "Creating ~a...~%" filename)
  ;; 
  (with-open-file 
   (o filename :direction :output :if-exists :supersede)
   (format o "go-prettyname~cgo-id~cgene-name~cpub-year~cpub-id~call-go-f3-levels~cmean-go-f3-levels~%" 
	   tab tab tab tab tab tab)
   (loop for go in *go-frames*
	 as pubs = (#^pubs go)
	 when pubs
	 do (loop for pub in pubs
		  do (loop for gene in (#^genes pub)
			   do (format o "~a~c~a~c~a~c~a~c~a~c~a~c~a~%"
				      (#^GO.prettyname go) tab
				      (#^GO.goid go) tab
				      (#^fname gene) tab
				      (#^year pub) tab
				      (#^pub-id pub) tab
				      (#^f3-levels go) tab
				      (if (#^f3-levels go) 
					  (mean (#^f3-levels go))
					"UNDEFINED")
				      ))))))

				       
