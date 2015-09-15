;; -*- mode: Lisp; Syntax: Common-Lisp; Package: biolisp; -*-

(in-package :bio)

;;; +=========================================================================+
;;; | Copyright (c) 2002-2006 by The BioBike Team                             |
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

;;; Authors: Jeff Shrager, JP Massar.

; (setq g (first-n 5 (#^proteins (setq s (load-organism "syn6803")))))
; (bio::cross-blast-short g g)
; (setq g (mapcar #'extract-sequence g))
; (bio::cross-blast-short g g) ; Should give an error
; (bio::cross-blast-short g g :force-type #$protein) ; Should work
; (bio::cross-blast-long g g) ; Should give an error
; (bio::cross-blast-long g g :force-type #$protein) ; Should work

(defun generic-organism-entity-type (frame)
  (ecase user:*frame-system-version*
    (:old (#^Organism-Entity-Type frame))
    (:new
     (cond
      ((typep frame 'frames::Bio.Gene) #$Gene)
      ((typep frame 'frames::Bio.Protein) #$Protein)
      ((typep frame 'frames::Bio.Contiguous-Sequence) #$Contiguous-Sequence)
      (t nil)
      ))
    (:sframes 
     (typecase frame 
       (bio::gene #$gene)
       (bio::protein #$protein)
       (bio::contiguous-sequence #$contiguous-sequence)
       (t nil)
       ))
    ))

(defun cross-blast-short 
       (objset1 objset2 &key (safe? t) force-type (e-limit 0.001))
  #.(one-string-nl 
     "Given two lists of genes, proteins, etc. (all must be of the same"
     "type!)  [i.e., things that can be passed to EXTRACT-SEQUENCE] or"
     "sequences as strings, this function passes them to the appropriate"
     "tablular blast program and returns a list of frames representing their"
     "cross-blast.  That is, the blast of all the elements in the first"
     "list, against all those in the second list.  The keyword argument"
     ":safe (default = t) skips anything passed in that one can't get a"
     "sequence from. Dropped items are flagged with an error report to"
     "standard output, but the alignment proceeds anyway."
     ""
     "If safe = nil,"
     "objects that don't have sequence information will probably cause an"
     "error! (Strings are always considered sequences.)  (You might want to"
     "use safe?=nil if, for example, you are assuming that the result"
     "matches some list you sent in, and you desire to have an error if the"
     "object has no sensible sequence interpretation.)"
     ""
     "The resulting list"
     "of frames (which will be of length n1 x n2 where nx is the length of"
     "the list of objects in the x'th list) has slots for all the results of"
     "the tabular form of blast. (i.e., percent-id, etc.)"
     ""
     "You may also name"
     "the input objects by using this list syntax for the arguments:"
     "((name1 object1) (name2 object2) ...) The names may be anything and they"
     "will appear in the FROM and TO slots instead of the objects"
     "themselves."
     ""
     "The order of crossblast results is indeterminate."
     "Moreover, note that there can be multiple submatches within the same"
     "sequence, so that you can get multiple results between the same"
     "entities!  The user should search through these and organize them as"
     "desired.  You can use ALIGN or BLAST-LONG on selected pairs in order"
     "to get the alignments.  If you pass strings to this function, no"
     "checking is done to ensure that the string is of the appropriate type"
     "to match the other entities.  Moreover, if you pass ALL strings, you"
     "MUST supply the :FORCE-TYPE keyword argument, as either #$gene or"
     "#$protein to tell the program what blast parameters to use.  Indeed,"
     "if you supply :FORCE-TYPE, no checking is done at all to ensure that"
     "the objects are of the correct type! The value of e-limit (default 0.001)"
     "constrains the blast to report only matches at or better"
     "than this e-value."
     )
  safe?
  (block exit
    (with-temporary-fasta-file 
        (prefix1 fapath1 fafile1 master-list1 :safe? safe?) objset1
      (with-temporary-fasta-file 
          (prefix2 fapath2 fafile2 master-list2 :safe? safe?) objset2
        ;; Get the type info and ensure that all are the same type of thing
        (let* ((type 
                (or force-type
                    (when (stringp (third (car master-list1)))
                      (error 
                       #.(one-string-nl
                          "For CROSS-BLAST-SHORT: If you supply strings, you"
                          "must provide :FORCE-TYPE (#$gene or #$protein).")))
                    (generic-organism-entity-type (third (car master-list1)))
                    )))
          ;; Check type consistency unless the user forced the type.
          (unless force-type
            (flet ((test (o) 
                     (let ((p (third o)))
                       (unless (or (stringp p) 
                                   (eq type (generic-organism-entity-type p)))
                         (error 
                          #.(one-string-nl
                             "All of the objects passed to CROSS-BLAST-SHORT"
                             "are not of the same organism-entity-type"
                             "(i.e., not all genes or all proteins)."
                             "You cannot mix types to this function!"))))))
              (mapcar #'test master-list1)
              (mapcar #'test master-list2)
              ))
          ;; Okay, so go for it!
          (let ((formatdb-command 
                 (formatn "~Aformatdb -p ~a -i ~a >& /dev/null" 
                          cl-user:*blast-executable-toplevel-dir*
                          (case type (#$gene "F") (#$protein "T")) fapath1)))
            (case (protected-shell-command 
                   formatdb-command :action-on-invalid-return :error)
              (:timeout (return-from exit nil))
              (otherwise nil)))
          (let* ((outfile (format nil "~a.out" prefix1))
                 (outpath (merge-pathnames  cl-user:*tmp-directory* outfile)))
            (let ((blastall-command 
                   (formatn
                    (one-string
                     "~Ablastall -FF -p ~a -d ~a -i ~a -o ~a -m 8 -e ~a"
                     " >& /dev/null")
                    cl-user:*blast-executable-toplevel-dir*
                    (case type (#$gene "blastn") (#$protein "blastp"))
                    fapath1 fapath2 outpath e-limit)))
              (case (protected-shell-command 
                     blastall-command :action-on-invalid-return :error)
                (:timeout 
                 (cformatt "***** Blast failed due to timeout! *****")
                 (return-from exit nil))
                (otherwise nil)))              
            (read-cross-blast-short-output outpath master-list1 master-list2)
            ))))))

(defun read-cross-blast-short-output (outpath master-list1 master-list2)
  (with-open-file (i outpath)
    (loop for line = (read-line i nil nil)
          until (null line)
          as (from to pid all nm ng sq eq ss es eval bits) = 
          (cl-ppcre::split "\\s+" line)
          collect 
          (let ((frame (make-temp-frame)))
            (setf (slotv frame #$subject) 
                  (second (assoc (read-from-string from) master-list2)))
            (setf (slotv frame #$query) 
                  (second (assoc (read-from-string to) master-list1)))
            (setf (slotv frame #$percent-identity) 
                  (read-from-string pid))
            (setf (slotv frame #$alignment-length) 
                  (read-from-string all))
            (setf (slotv frame #$n-mismatches) (read-from-string nm))
            (setf (slotv frame #$n-gaps) (read-from-string ng))
            (setf (slotv frame #$query-start) (read-from-string sq))
            (setf (slotv frame #$query-end) (read-from-string eq))
            (setf (slotv frame #$subject-start) (read-from-string ss))
            (setf (slotv frame #$subject-end) (read-from-string es))
            (setf (slotv frame #$e-value) 
                  (read-from-string (substitute #\d #\e eval)))
            (setf (slotv frame #$bit-score) (read-from-string bits))
            frame
            ))))


;;; Due to what appears to be a BUG in blastall, we have to do this 
;;; slightly weird thing of running each blast separately.  
;;; The problem is that blastall creates multiple XML
;;; objects in the file, which is technically illegal in the XML spec 
;;; (I'm told by Franz).
;;; We could either parse up the result file, or run separate queries.   
;;; I've chosen to to the latter.  

;;; !!! This refactoring made it difficult to do the same-entity-type
;;; test, so that's been removed from this for the time being; This
;;; just assumes that they're the same type!!!

(defun cross-blast-long
       (objset1 objset2 &key (safe? t) force-type (e-limit 0.001))
  #.(one-string-nl 
     "See the documentation for (cross-blast-short ...) for a complete"
     "description of the arguments."
     "This function passes OBJSET1 and OBJSET2 to the appropriate blast program"
     "and returns a list of the blasts of each element in OBJSET1 against"
     "all the elements of OBJSET2."
     "Each element in this list will be a blast result temporary frame,"
     "and the resulting list will have one result for each element in the"
     "first argument.  That is, if you pass in a list of three genes and"
     "another of ten genes, you'll get a list of three blast results"
     "back.")
  ;; Protect this from Unearthing
  (push :do-not-permit-unearthing-descent 
        (slotv #$BlastOutput_query-def #$properties))
  (push :do-not-permit-unearthing-descent (slotv #$hit_def #$properties))
  safe?
  (block exit
    (with-temporary-fasta-file 
        (prefix1 fapath2 fafile2 master-list2 :safe? safe?) objset2
      (let* ((type 
              (or force-type 
                  (when (stringp (third (car master-list2)))
                    (error 
                     #.(one-string-nl
                        "For CROSS-BLAST-LONG: If you supply strings, you"
                        "must provide :FORCE-TYPE (#$gene or #$protein).")))
		  (generic-organism-entity-type (third (car master-list2)))
		  ))
             (formatdb-command 
              (formatn "~Aformatdb -p ~a -i ~a >& /dev/null" 
                       cl-user:*blast-executable-toplevel-dir* 
                       (case type (#$gene "F") (#$protein "T")) fapath2)))
        (case (protected-shell-command formatdb-command)
          (:timeout (return-from exit nil))
          (otherwise nil))
        (loop 
         for obj1 in objset1
         collect
         (with-temporary-fasta-file 
             (prefix1 fapath1 fafile1 master-list1 :safe? safe?) (list obj1)
           (let* ((outfile (format nil "~a.out" prefix1))
                  (outpath 
                   (merge-pathnames cl-user:*tmp-directory* outfile))
                  (blastall-command 
                   (formatn 
                    (one-string
                     "~Ablastall -FF -p  ~a -i ~a -d ~a -o ~a -m 7 -e ~a >& "
                     "/dev/null")
                    cl-user:*blast-executable-toplevel-dir*
                    (case type (#$gene "blastn") (#$protein "blastp"))
                    fapath1 fapath2 outpath e-limit)))
             (case (protected-shell-command blastall-command)
               (:timeout (return-from exit nil))
               (otherwise nil))
             (let ((result 
                    (with-open-file (i outpath) 
                      (xml->temp-frame (third (wb::parse-xml i))))))
               (setf (#^BlastOutput_query-def result)
                     (second (assoc (#^BlastOutput_query-def result) 
                                    master-list1)))
               (loop 
                for blastoutput-iteration 
                in (#^BlastOutput_iterations result) do
                (loop for iteration 
                      in (#^Iteration blastoutput-iteration)
                      do 
                      (loop for Iteration_hits 
                            in (#^Iteration_hits iteration) do
                            (loop for hit in (#^Hit Iteration_hits) do
                                  (Setf (#^hit_def hit)
                                        (second (assoc (#^hit_def hit) 
                                                       master-list2)))))))
               result
               ))))))))

;;; Get the NCBI standard info out of a web page's contents, represented
;;; by a single long string.  Note that if there is more than one occurrance
;;; of "QBlastInfoBegin", etc., this will only get the first one.  
;;; Call it as:
;;;   (extract-blast-info the-web-page-contents)
;;; It returns this sort of thing:
;;;   ((:STATUS "WAITING") (:RID "1044584572-020243-29658"))

(defun extract-blast-info (web-page-contents)
  ;; Find the position of the surrounding markers (QBlastInfoBegin, etc.)
  (let* ((QBlastInfoBegin-position (search "QBlastInfoBegin" web-page-contents))
	 (QBlastInfoEnd-position (search "QBlastInfoEnd" web-page-contents)))
    ;; If either one is missing, skip it and retrun nothing!
    (when (and QBlastInfoBegin-position QBlastInfoEnd-position)
      ;; Extract the part between the markers, bind it to a stream
      ;; and read each line out of it, until you hit the end.
      (loop with stream = (make-string-input-stream (subseq web-page-contents QBlastInfoBegin-position
                                                            QBlastInfoEnd-position))
            as line = (read-line stream nil nil)
            with result = nil ; This will collect the resulting parsed values.
            until (null line)
            ;; This turns this: "<TAB>LABEL=VALUE" to this: (LABEL "VALUE")
            do (let ((=pos (position #\= line)))
                 (when =pos (push (list (lxml-keywordize (subseq line 1 =pos))
                                        (string-trim " " (subseq line (1+ =pos))))
                                  result)))
            finally (return result)
            ))))

;;; These names come out really weirdly, like with spaces and shit
;;; so you have to both read and keyworize it!

(defun lxml-keywordize (thing)
  (cond ((stringp thing)
	 (keywordize (read-from-string thing)))
	((symbolp thing)
	 (keywordize thing))
	(t thing)))

(defun broket-positions (string)
  (loop for current-position from 0 by 1
	with <-pos = nil
	as char across string
	when (char-equal #\< char)
	do (setq <-pos current-position)
	when (char-equal #\> char)
	collect (list <-pos current-position)))

;;; The very small e-values get converted to 0.0 by read-from-string, so
;;; we have our own (mantissa exponent) notation.

(defun read-e-value (s)
  (let ((e-pos (position #\e s))) ; look for a exponential (scientific) form.
    (cond ((null e-pos) ; no e... just read the number straight!
	   (list (read-from-string s) 0.0)) ; Return the "weird" form
	  (t ; otherwise we have to produce the fancy parse...
	   (list (read-from-string (subseq s 0 e-pos))
		 (read-from-string (subseq s (1+ e-pos))))))))

(defun get-e-value (s)
  (read-e-value (subseq s (1+ (second (first (last (broket-positions s)))))))
  )

(defun parse-blast-match (s)
  (let* ((brokets (broket-positions s))
	 (e-value (read-e-value (subseq s (1+ (second (first (last brokets)))))))
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

(defun extract-blast-hits (url)
  (with-open-web-page (stream url)
    ;; Skip until we find the target line: "Sequences producing significant alignments:"
    (loop as line = (read-line stream nil nil)
	  until (or (null line)
		  (and (> (length line) 43)
		         (string-equal (subseq line 0 43) "Sequences producing significant alignments:")))
	)		 
  ;; Okay, now skip one (the blank line!) -- stop here if we hit the end of the file!
  (when (read-line stream nil nil)
  ;; And start parsing until we hit the </pre> line:
	(loop as line = (read-line stream nil nil)
	      until (string-equal "</PRE>" line)
	      collect (parse-blast-match line)
	      ))))

;;; First a couple of little helping functions.

(defparameter *ncbi-blast-url* "http://www.ncbi.nlm.nih.gov/blast/Blast.cgi")

;;; This just makes a string with the url on the front for convenience.
(defun blast-query (query)
  (format nil (s+ *ncbi-blast-url* "?~a") query))


(defun extract-blast-hits-from-xml (xml)
  ;; Find all the hits, each of which is identified in the XML structure
  ;; by a specific sequene of tages: '|BlastOutput| '|BlastOutput_iterations|, ... etc.
  (let ((hits (lxml-subnodes
	       (lxml-descend xml :BlastOutput :BlastOutput_iterations
			     :Iteration :Iteration_hits) :Hit)))
    ;; For each hit return...
    (mapcar #'(lambda (hit)
		(let ((frame (make-temp-frame)))
		  ;; The hit id and definition (the description)...
		  (setf (#^hit_id frame) (second (lxml-subnode hit :Hit_id)))
		  (setf (#^hit_def frame) (second (lxml-subnode hit :Hit_def)))
                  ;; And then all the information about that hit; at least all
		  ;; the info we care to get at the moment. This should make new
		  ;; temp frames for each of these eventually. FFF
		  (setf (#^hit_hsps frame) 
			(let ((hsps (lxml-subnodes 
				     (lxml-subnode hit :Hit_hsps)
				     :Hsp)))
			  (mapcar #'(lambda (hsp)
				      (list
				       (lxml-subnode hsp :Hsp_evalue)
				       (lxml-subnode hsp :Hsp_hit-from)
				       (lxml-subnode hsp :Hsp_hit-to) ))
				  hsps)))
		  frame))
	    hits)))

;;; This recursively digs down through the XML structure according to the given
;;; tag sequence and hands back all the leaves of the tree that are at this path point.

(DEFUN Convert-to-number-maybe (entity)
  (IF (AND (TYPEP entity 'String) (> (LENGTH entity) 0))
      (LET ((item (STRING-TO-LISP-OBJECT 
                     (SUBSTITUTE #\d #\e entity))))
          ;; needed because blast returns things like 1e-166
        (IF (OR (TYPEP item 'Number) (TYPEP item 'List))
            item
            entity))
      entity))

;;; Function below is like unexpanded original except that it
;;; reads many more blast fields.

(DEFUN Extract-blast-hits-from-xml-expanded (xml)
  ;; Find all the hits, each of which is identified in the XML structure
  ;; by a specific sequene of tags: '|BlastOutput| 
  ;;    '|BlastOutput_iterations|, ... etc.

 (LET* ((hits (lxml-subnodes
               (lxml-descend xml :BlastOutput :BlastOutput_iterations
                   :Iteration :Iteration_hits) :Hit))
        (hit-categories
           (LIST '("Q-START" :Hsp_query-from)
                 '("Q-END" :Hsp_query-to)
                 '("T-START" :Hsp_hit-from)
                 '("T-END" :Hsp_hit-to)
                 '("Q-FRAME" :Hsp_query-frame)
                 '("T-FRAME" :Hsp_hit-frame)
                 '("%ID" :Hsp_identity)
                 '("POSITIVE" :Hsp_positive)
                 '("GAPS" :Hsp_gaps)
                 '("ALIGN-LENGTH" :Hsp_align-len)
                 '("BIT-SCORE" :Hsp_bit-score)
                 '("E-VALUE" :Hsp_evalue)
                 '("Q-SEQ" :Hsp_qseq) 
                 '("T-SEQ" :Hsp_hseq))) 
       )

   (LOOP FOR hit IN hits
         AS target = (SECOND (LXML-SUBNODE hit :Hit_accession))
         AS tdef = (SECOND (LXML-SUBNODE hit :Hit_def))
         AS hsps = (LXML-SUBNODES (LXML-SUBNODE hit :Hit_hsps) :Hsp)
         AS result-start = (LIST (LIST "TARGET" target) 
                                 (LIST "TDEF" tdef))
         AS results 
           = (LOOP FOR hsp IN hsps
                   AS hit-cat-values
                     = (LOOP FOR (label cat) IN hit-categories
                             AS value = (SECOND (LXML-SUBNODE hsp cat))
                             AS converted-value
                               = (CONVERT-TO-NUMBER-MAYBE value)
                             COLLECT (LIST label converted-value)) 
                   AS result = (APPEND result-start hit-cat-values)
                   COLLECT result)
         APPEND results)))


(defun lxml-descend (lxml &rest subnode-types)
  (if (null subnode-types) ; If you hit the end...
      lxml ; You're where you want to be; Return this structure.
      ;; Here's the recursion step; It's a little tricky because it strips the
      ;; tag sequence as it goes!
      (apply #'lxml-descend (lxml-subnode lxml (car subnode-types))
        (cdr subnode-types))))


;;; And here are the rest of the the relevant help functions:

;;; Once you have the leaf you want in hand, these
;;; strip out the specific tags that you're interested in.

;;; This one gets the first hit matching the specified type.

(defun lxml-subnode (lxml subnode-type)
  (dolist (sub lxml)
    (if (and (listp sub)
          (eq (lxml-keywordize (car sub)) subnode-type))
        (return sub))))

;;; This one is slightly fancier, getting all of the hits that
;;; match the type.
(defun lxml-subnodes (lxml subnode-type)
  (let ((result nil))
    (dolist (sub lxml (nreverse result))
      (if (and (listp sub)
            (eq (lxml-keywordize (car sub)) subnode-type))
          (push sub result)))))

(defun ncbi-blast-sequence 
       (sequence &key (db "nr") (expect 0.00001) (program "blastn"))
  #.(one-string-nl
     "Blasts the given sequence against an NCBI database,"
     "and returns the results as"
     "a set of temporary frames containing the relevant parts of the results."
     "Keyword args:"
     "  :db [default \"nr\"] - Which NCBI databast to use"
     "  :expect [default 0.00001] - e-value limit"
     "  :program [default \"blastn\"] - which program to use"
     )
  (submit-blast-query-xml 
   (format nil "QUERY=~a&DATABASE=~a&HITLIST_SIZE=10&FORMAT_TYPE=HTML&FILTER=L&EXPECT=~a&PROGRAM=~a&CLIENT=web&SERVICE=plain&NCBI_GI=on&PAGE=Nucleotides&CMD=Put"
           sequence
           db
           expect
           program
           )))

;;; The sequence specified in a query can be very large.  Because of this
;;; if it exceeds a certain threshold NCBI rejects an HTTP GET query.  
;;; The solution is to use the HTTP POST method which requires that we 
;;; keep separate the URL and the arguments.  


(DEFUN submit-blast-query-xml (query &KEY full)
  ;; Submit the query and parse-out the RID from the reply.
  (LET* ((extraction-program
          (IF full 'EXTRACT-BLAST-HITS-FROM-XML-EXPANDED
            'EXTRACT-BLAST-HITS-FROM-XML))
         (query-alist (net.aserve::form-urlencoded-to-query query))
         (rid 
          (let* ((wpc (WEB-PAGE-CONTENTS
                       *ncbi-blast-url* :method :post :query query-alist))
                 (ebi (extract-blast-info wpc))
                 (rid (assocadr :rid ebi)))
            (unless rid
              (error 
               (one-string-nl
                ""
                "**** Ruh roh!  NCBI request did not return RID! ****"
                "The web page contents returned by NCBI are:"
                ""
                "~A"
                ""
                "(The reason for the problem may be explained within"
                "the above text.)"
                )
               wpc 
               ))
            rid
            ))
         (blast-page 
          (BLAST-QUERY 
           (FORMAT NIL "CMD=Get&RID=~a&ALIGNMENTS=0" rid)))
         )

    (UNLESS full
      (FORMAT T (S+ "The query has been assigned RID ~a... "
                    "Waiting for it to become ready!")
              rid))
    ;; Wait for ready, sleeping a little each time 
    ;; so as not to overload NCBI!
    (LOOP AS blast-info 
          = (EXTRACT-BLAST-INFO (WEB-PAGE-CONTENTS blast-page))
          AS status = (ASSOCADR :status blast-info)
          UNTIL (NOT (STRING-EQUAL "WAITING" status))
          DO (UNLESS full (FORMAT T "#"))
          (SLEEP 5))
    ;; Okay, should be ready; Let's get the results.

    (FUNCALL 
     extraction-program
     (PARSE-XML 
      (let ((wpc (WEB-PAGE-CONTENTS (S+ blast-page "&FORMAT_TYPE=XML"))))
        wpc
        )))))

#+debug-only
(DEFUN submit-blast-query-xml (query &KEY full)
  (print (list 'query query))
  ;; Submit the query and parse-out the RID from the reply.
  (LET* ((extraction-program
          (IF full 'EXTRACT-BLAST-HITS-FROM-XML-EXPANDED
            'EXTRACT-BLAST-HITS-FROM-XML))
         (query-alist (net.aserve::form-urlencoded-to-query query))
         t1 t2 t3 rid)
    ;; (print 't1)
    ;; (setq t1 (BLAST-QUERY query))
    (print 't2)
    (setq t2 (web-page-contents
              *ncbi-blast-url* :method :post :query query-alist))
    (setq *t2* t2)
    (print 't3)
    (setq t3 (extract-blast-info t2))
    (print 'rid)
    (setq rid (ASSOCADR :rid t3))
    (print (list 'rid rid))
    (when (null rid) 
      (error 
       (one-string-nl
        ""
        "**** Ruh roh!  NCBI request did not return RID! ****"
        "The web page contents returned by NCBI are:"
        ""
        "~A"
        ""
        "(The reason for the problem may be explained within"
        "the above text.)"
        )
       t2
       ))
    (print 'blast-page)
    (let ((blast-page 
           (BLAST-QUERY 
            (FORMAT NIL "CMD=Get&RID=~a&ALIGNMENTS=0" rid)))
          )

      (print 'print)
      (print (list 'full full))
      
      (UNLESS full
        (FORMAT T (S+ "The query has been assigned RID ~a... "
                      "Waiting for it to become ready!")
          rid))

      (print 'wait)
      ;; Wait for ready, sleeping a little each time 
      ;; so as not to overload NCBI!
      (loop with blast-info = nil
            with status = nil
            with wpc = nil
            do 
            (print 'wpc)
            (setq *wpc* (setq wpc (web-page-contents blast-page)))
            (print 'blast-info)
            (setq blast-info (extract-blast-info wpc))
            (setq status (assocadr :status blast-info))
            (print (list 'status status))
            (unless (string-equal "WAITING" status)
              (print 'exit-wait)
              (return))
            (sleep 5)
            )
      
      ;; Okay, should be ready; Let's get the results.
      
      (print 'parse-xml)
      
      (let (t1 t2 t3)
        (print 't1)
        (setq t1 (WEB-PAGE-CONTENTS (S+ blast-page "&FORMAT_TYPE=XML")))
        (print 't2)
        (setq t2 (parse-xml (setq *t1* t1)))
        (print 't3)
        (setq t3 (funcall extraction-program t2))
        t3
        ))))
      


#| Above function differs from original (below) only in the
   introduction and use of the keyword FULL. Specifying :FULL T
   redirects xml extraction from EXTRACT-BLAST-HITS-FROM-XML to
   EXTRACT-BLAST-HITS-FROM-XML-EXPANDED. If :FULL is NIL (default),
   then the new and old functions are functionally identical.

(defun submit-blast-query-xml (query)
  ;; Submit the query and parse-out the RID from the reply.
  (let ((rid (assocadr :rid (extract-blast-info (web-page-contents (blast-query query))))))
    (format t "The query has been assigned RID ~a... Waiting for it to become ready!" rid)
    ;; Wait for ready, sleeping a little eeach time so as not to overload NCBI!
    (loop until (not (string-equal "WAITING"
				   (assocadr :status (extract-blast-info (web-page-contents (blast-query (format nil "CMD=Get&RID=~a&ALIGNMENTS=0" rid)))))))
          do (format t "#")
          (sleep 5))
    ;; Okay, should be ready; Let's get the results.
    (extract-blast-hits-from-xml
     (parse-xml (web-page-contents (blast-query (format nil "CMD=Get&RID=~a&ALIGNMENTS=0&FORMAT_TYPE=XML" rid)))))))
|#

#|
Test:

(blast-sequence-xml "tccagataatatagtgaaagacacagatgtactagcactattccgtgtatctcctcaaccaggagtaga")

The query has been assigned RID 1044855959-011332-31420... Waiting for it to become ready!#######
(((|Hit_id| "gi|4868454|gb|AF148520.1|")
  (|Hit_def|
    "Mazzaella linearis ribulose-1,5-bisphosphate carboxylase/oxygenase large subunit (rbcL) gene, partial cds; chloroplast gene for chloroplast product")
  (((|Hsp_evalue| "1.86659e-30") (|Hsp_hit-from| "9") (|Hsp_hit-to| "77"))))
 ((|Hit_id| "gi|23393060|gb|AF294814.1|")
  (|Hit_def|
    "Mazzaella japonica ribulose-1,5-bisphosphate carboxylase/oxygenase large subunit (rbcL) gene, partial cds; chloroplast gene for chloroplast product")
  (((|Hsp_evalue| "1.10928e-25") (|Hsp_hit-from| "5") (|Hsp_hit-to| "73"))))
 ((|Hit_id| "gi|4733924|gb|U03385.2|MSU03385")
  (|Hit_def|
    "Mazzaella splendens ribulose-1,5-bisphosphate carboxylase/oxygenase large subunit (rbcL) gene, complete cds; chloroplast gene for chloroplast product")
  (((|Hsp_evalue| "1.10928e-25") (|Hsp_hit-from| "43") (|Hsp_hit-to| "111"))))
 ((|Hit_id| "gi|4733920|gb|U03378.2|MFU03378")
  (|Hit_def|
    "Mazzaella flaccida ribulose-1,5-bisphosphate carboxylase/oxygenase large subunit (rbcL) gene, complete cds; chloroplast gene for chloroplast product")
  (((|Hsp_evalue| "1.10928e-25") (|Hsp_hit-from| "65") (|Hsp_hit-to| "133"))))
 ((|Hit_id| "gi|467944|gb|U03384.1|MSU03384")
  (|Hit_def|
    "Mazzaella sp. Bodega Head, Sonoma Co., CA, USA, chloroplast ribulose-1,5-bisphosphate carboxylase/oxygenase large subunit (rbcL) gene, partial cds")
  (((|Hsp_evalue| "1.10928e-25") (|Hsp_hit-from| "44") (|Hsp_hit-to| "112"))))
 ((|Hit_id| "gi|23393058|gb|AF294813.1|")
  (|Hit_def|
    "Chondrus nipponicus ribulose-1,5-bisphosphate carboxylase/oxygenase large subunit (rbcL) gene, partial cds; chloroplast gene for chloroplast product")
  (((|Hsp_evalue| "2.7042e-23") (|Hsp_hit-from| "8") (|Hsp_hit-to| "76"))))
 ((|Hit_id| "gi|23393056|gb|AF294812.1|")
  (|Hit_def|
    "Chondrus armatus ribulose-1,5-bisphosphate carboxylase/oxygenase large subunit (rbcL) gene, partial cds; chloroplast gene for chloroplast product")
  (((|Hsp_evalue| "2.7042e-23") (|Hsp_hit-from| "7") (|Hsp_hit-to| "75"))))
 ((|Hit_id| "gi|5525038|gb|U03379.2|MHU03379")
  (|Hit_def|
    "Mazzaella heterocarpa Seal Rock, Lincoln Co., OR, USA, chloroplast ribulose-1,5-bisphosphate carboxylase/oxygenase large subunit (rbcL) gene, partial cds")
  (((|Hsp_evalue| "2.7042e-23") (|Hsp_hit-from| "41") (|Hsp_hit-to| "109"))))
 ((|Hit_id| "gi|5020055|gb|AF146213.1|AF146213")
  (|Hit_def|
    "Mazzaella japonica ribulose-1,5-bisphosphate carboxylase/oxygenase large subunit (rbcL) gene, partial cds; chloroplast gene for chloroplast product")
  (((|Hsp_evalue| "2.7042e-23") (|Hsp_hit-from| "7") (|Hsp_hit-to| "75"))))
 ((|Hit_id| "gi|5020053|gb|AF146212.1|AF146212")
  (|Hit_def|
    "Mazzaella cornucopiae ribulose-1,5-bisphosphate carboxylase/oxygenase large subunit (rbcL) gene, partial cds; chloroplast gene for chloroplast product")
  (((|Hsp_evalue| "2.7042e-23") (|Hsp_hit-from| "54") (|Hsp_hit-to| "122")))))

|#
