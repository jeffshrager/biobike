;; -*- mode: Lisp; Syntax: Common-Lisp; Package: biolisp; -*-

(in-package :bio)

;;; +=========================================================================+
;;; | Copyright (c) 2002-2006 The BioBike Team                                |
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

;;; Interfaces to many NCBI tools (blast interface is in blast.lisp, not here!)

(defparameter *extract-pubmed-slots* 
  '(#$ArticleTitle #$Author #$MedlineTA #$AbstractText
		   #$MedlinePgn #$NlmUniqueID #$PMID #$PubDate #$Volume #$Year))

(defun pubmed-query (search-terms &key (return-n 3) (simplify t))
  #.(one-string-nl 
     "Given one or a list of strings representing search terms"
     "(which are implicitly AND'ed together) returns up to"
     ":return-n (default = 3)"
     "temp frames representing PubMed entries that match these terms."
     "Each such frame is a simplified form of the query, which inculdes"
     "much more information.  The #$raw-pubmed-result slot in each frame"
     "contains the original contents in a complex structure that"
     "mirrors the format of the XML returned from PubMed."
     "You can also just get those raw frames by using the key :simplify nil"
     )
  (let ((basic-result
	 (mapcar (lambda (e) (xml->temp-frame (third e)))
		 (raw-pubmed-query search-terms :return-n return-n))))
    (if simplify
	(mapcar 
         (lambda (result-frame)
           (simplify-complex-temp-frame-tree
            result-frame *extract-pubmed-slots*))
         basic-result)
      basic-result)))

(defun simplify-complex-temp-frame-tree (top-frame slots-to-extract)
  (let ((temp-frame (make-temp-frame #$pubmed-result)))
    (loop for (slot values) in (unearth-slots top-frame slots-to-extract)
	  do (setf (slotv temp-frame slot) values))
    (setf (slotv temp-frame #$unsimplified-frame) top-frame)
    temp-frame))

(defvar *unearthing-alist*)
(defvar *unearthing-master-table*)
(defvar *force-descent-through-protected-slots*)

(defun unearth-slots (frame-structure slots
		      &key (depth-limit 20) (*force-descent-through-protected-slots* nil))
  #.(one-string-nl 
     "Given a structure of frames (a single frame or a tree of frames and other types), and a slot or list of slots"
     "this function will return a list of the values of all the slots in all the frames in the first arg structure"
     "that have slot values in the slots indicated by the second arg. The result will have the same structure as the"
     "first argument, although in place of each frame will be an alist of the slots in the order given in the second arg."
     "In order to keep from walking the whole database, this has a :DEPTH-LIMIT keyword argument with default limit 10."
     "WARNING: This can easily try to walk through every frame in the system if some frame points into a part of the gene model!!"
     "(Thus the depth limit, but even 10 can be too deep in some cases!)"
     "Thes search is also constrained not to descend through slots that have the :do-not-permit-unearthing-descent property."
     "The :force-descent-through-protected-slots t keyword will force it through these, but you probably don't want to do this"
     "for the same reason as above. "
     )
  (let (*unearthing-alist* 
	(*unearthing-master-table* (make-hash-table :test 'equal)))
    (setq *unearthing-alist* (mapcar 'list (if (listp slots) slots (list slots))))
    (unearth-slots2 frame-structure depth-limit)
    (values *unearthing-alist* 
	    (loop for slot being the hash-keys of *unearthing-master-table* collect slot))))
  
(defun unearth-slots2 (frame-structure depth-limit)
  (cond ((isframe? frame-structure)
	 (unearth-slots3 frame-structure depth-limit))
	((listp frame-structure)
	 (mapcar (lambda (frame-structure) (unearth-slots2 frame-structure depth-limit)) frame-structure))
	(t frame-structure)))

(defun unearth-slots3 (frame depth-limit)
  (cond ((< depth-limit 1))
	(t 
	 ;; Get any slots here
	 (loop for entry in *unearthing-alist*
	       as value = (slotv frame (car entry))
	       when value 
	       do 
	       (push value (cdr entry)))
	 ;; Now walk through every subframe
	 (mapslots (lambda (frame value) 
		       (setf (gethash frame *unearthing-master-table*) t)
		       (unless (or *force-descent-through-protected-slots*
				   (member :do-not-permit-unearthing-descent (slotv frame #$properties)))
			 (unearth-slots4 value (1- depth-limit)))) frame)
	)))

(defun unearth-slots4 (frame-structure depth-limit)
  (cond ((< depth-limit 1) nil)
	((isframe? frame-structure)
	 (unearth-slots3 frame-structure (1- depth-limit)))
	((listp frame-structure)
	 (mapcar (lambda (frame-structure) (unearth-slots4 frame-structure (1- depth-limit))) frame-structure))
	(t frame-structure)))

(defun raw-pubmed-query (search-terms &key extract-fields (return-n 3))
  (unless (listp search-terms) (setq search-terms (list search-terms)))
  #.(one-string-nl 
     "Given one or a list of strings, representing search terms which are"
     "implicitly AND'ed together, this returns up to :RETURN-N (default = 3)"
     "PubMed entries that match these terms.  The results are returned in LISP/XML" 
     "format.  If you provide the :extract-fields key, it must be a list, and the specific given field are"
     "extracted from the results.  Each field name must exactly match the XML terms.  So, for example"
     "you must specify '(|AbstractText|), not '|AbstractText| or '(abstracttext), etc."
     "As a convenience, if you don't give :extract-fields, the result is two values, the second of which is the result of"
     "calling headed-list-headers on the pubmed search result, but if you have a result from pubmed-query that you"
     "don't know the fields of, use (headed-list-headers ...) on it to get the field names, and then"
     "you can use (extract-headed-lists ...) with each field in order to avoid repeating the query if you care."
     )
  (let* ((page 
          (wb::web-page-contents 
           (format
            nil
            "http://www.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi?db=pubmed&retmax=1&usehistory=y&term=~a" 
            (cond ((listp search-terms)
                   (loop for term in (cdr search-terms)
                         with result = (car search-terms)
                         do (setq result (format nil "~a+AND+~a" result term))
                         finally (return result)))
                  (t search-terms)))))
	 (count (read-from-string (extract-xml-token "Count" page)))
	 (webenv (extract-xml-token "WebEnv" page))
	 (querykey (read-from-string (extract-xml-token "QueryKey" page)))
	 (results 
          (loop for i from 0 below (min count return-n)
                collect
                (wb::parse-xml 
                 (wb::web-page-contents 
                  (format nil "http://www.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi?rettype=full&retmode=xml&retstart=~a&retmax=1&db=pubmed&query_key=~a&WebEnv=~a" 
                          i querykey webenv))))
          )
	 )
    (if extract-fields
	(loop for result in results 
	      collect (loop for field in extract-fields 
			    collect (extract-headed-lists field result)))
      (values results (headed-list-headers results)))))

(defun extract-xml-token (key xml)
  #.(one-string-nl 
     "Given a string key, return the first occurance of 'stuff' in the XML string: \"...<key>stuff</key>...\""
     "This is a rather inefficient way to deal with XML unless you're just looking for one specific thing."
     "One should consider, instead, parsing the xml to lisp and then processing it in list form."
     )
  (let ((p1 (search (format nil "<~a>" key) xml)))
    (when p1
      (let ((p2 (search (format nil "</~a>" key) xml :start2 p1)))
	(when p2
	  (subseq xml (+ p1 (length key) 2) p2))))))

(defun extract-headed-lists (target-symbol list &aux result)
  #.(one-string-nl 
     "Find all sublists in a list that begin with the indicated target symbol."
     "Useful mainly in extracting content from lisp-parsed XML results.")
    (labels ((ehl2 (target list)
		 (cond ((null list) nil)
		       ((not (listp list)) nil)
		       ((equal (car list) target)
			(push list result))
		       (t (ehl2 target (car list))
			  (ehl2 target (cdr list))))))
      (ehl2 target-symbol list)
      (nreverse result)))

(defun headed-list-headers (list &aux result)
  #.(one-string-nl
     "Find all the symbols that head up any sublist in the given list."
     "This is mainly useful in finding the names of XML field in order"
     "to interpret XML results.")
  (labels ((ehl2 (list)
             (cond ((null list) nil)
                   ((not (listp list)) nil)
                   ((symbolp (car list))
                    (pushnew (car list) result)
                    (ehl2 (cdr list)))
                   (t (mapcar #'ehl2 list)))))
    (ehl2 list)
    result))

(defun try-to-hook-text-to-frames (text &key (target-frames *go-frames*) (match-threshold 0.8))
  #.(one-string-nl
     "Given a single string, parse it at word breaks, and then find, for each word"
     "the frame that matches it (by fname through word homology).  The best matching"
     "frame is returned.  The keyword :match-threshold (defafult 0.8) sets the limit"
     "below with a match does not count, so if there are no matches above 0.8 (in the"
     "default case, no match will be found.  By default, this operates over the Gene"
     "ontology (*go-frames*) list, but you can set this to any list of frames using the"
     ":target-frames keyword, which must be a list of frames."
     )
  (let ((words 
         (mapcar 'cleanup-word-for-frame-hooking (string-split text #\space))))
    (loop for word in words
	  as match = (first (sort (frame-fnamed-by-word-homology word :targets target-frames :limit match-threshold) '> :key 'car))
	  when match
	  collect (cons word match))))

(defun cleanup-word-for-frame-hooking (inword)
  (let* ((word (string-trim ". ,:;!" inword))
	 (b1 (search "<b>" word :test 'char-equal))
	 (b2 (search "</b>" word :test 'char-equal)))
    (if (and b1 b2 (= 0 b1) (= (- (length word) 4) b2))
	(subseq word 3 (- (length word) 4))
      word)))

;;; This is very preliminary; We need to figure out how to splify the result to something
;;; useful.

(defun do-one-ncbi-query (db uid)
  (let ((result (NET.ASERVE.CLIENT::do-http-request (format nil "http://www.ncbi.nlm.nih.gov/entrez/viewer.fcgi?db=~a&qty=1&c_start=1&list_uids=~a&dopt=xml&send=Send&sendto=t&from=begin&to=end" db uid))))
    (when (stringp result)
      (xml->temp-frame (third (wb::parse-xml result))))))
      
