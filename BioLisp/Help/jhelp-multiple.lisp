;;; -*- Package: help; mode: lisp; base: 10; Syntax: Common-Lisp; -*-

(in-package :help)

;;; +=========================================================================+
;;; | Copyright (c) 2002-2006 JP Massar, Jeff Shrager, Peter Seibel           |
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

;;; J.Myers:  Loosely adapted from help-multiple, by Authors:  JP Massar, Peter Seibel.
;;  Format is mostly for historical reasons.  Why is there so much structure in setting up a call???





(defun build-clean-wordlist-with-tiny-words-and-stems (words)
;; Takes rough words coming in; cleans punctuation; expands tiny-words into tiny words word; returns expansion for search.
;; This could be used to send to the search algorithm, except the search algorithm needs 
;; a clean list so it can deal with bigrams properly, it seems.
;; Whereas this list is needed later on, for use against lines, to grab context.
;;
  (let* ((probe-list words))
 ;; words = probe-list is "list" "of" "words," "some-having" "puctuation?" .

    (when (>= *jhelp-debug* 5)  (print "BUILD-CLEAN-WORDLIST of: "))

	;;First, we fix the probelist so it actually takes out the punctuation it should have already.
	;; Put it all back together again, then tear it apart, correctly this time.
	(setq probe-list (split-at-puctuationwhitespace 
   	 (apply 'concatenate 'string
  		 (loop for x in probe-list 
               		collecting (format nil "~a " x)))))

	(loop for big-word in probe-list 
  
      	 for wordz = (split-at-specialpunctuationspace big-word)
	 for tiny-word-stems = (loop for tword in wordz with stem 
				when (and (setq stem (find-stem tword)) (>= (length stem) 3))
				collecting stem)
		
	   do
	   (when (> (length wordz) 1)
	     (push big-word wordz))

          appending

	(loop for word in (append wordz tiny-word-stems) 

	when    (or (<= (length wordz) 1)
		    (not (isa-jhelp-stop-wordp word))  )
	  ;;actually this should not be necessary, as there should be no barrels for stop words in the first place.
	  ;; For some reason we are getting some leakage so far, though.


         collecting    word into mywords                  ;this resulting list gets returned.
 ;        and do                   (when (>= *jhelp-debug* 3)  (format T "?~s " word))
         finally (return mywords)
       ))
    )  ; let*
)






;;; "Do the single word help algorithm on each word, gathering up the matches.
;;; Then combine the results, sorting the hits by figuring out how many
;;; times each hit occurs: This takes priority over the score, but within
;;; each group of equal numbers of hits, they are still sorted by score."



(defun multiple-word-jhelp (words &rest keys &key &allow-other-keys)
  ;; Ensure we really have strings.  NullOp if already a string.
  (setq words (mapcar 'string words))  
  ;; (setq words (remove-if (lambda (x) (gethash x *stop-table*)) words))
  (setq words (remove-if (lambda (x) 
                           (and (stringp x) 
                                (or (zerop (length x))
                                    (every 'whitespacep x))))
                         words))

  (when (>= *jhelp-debug* 1)
    (print "Multiple-word-jhelp ")
    (prin1 words)
    (princ " ...keys...:  ")
    (prin1 keys)
    (terpri))

  


  (let* ((*current-search-string* (string-join words #\Space))
         (JDocs (JHelp-Run-Engine-Get-JDocuments  words))
         (allwords (build-clean-wordlist-with-tiny-words-and-stems words))  ;Now done after the matching, since only needed for context.
         )

    ;;(loop for JDoc in JDocs do
    ;;	(setq foo 'bar)
    ;;)
    ;; Views now done inside jhelp-html.  
    ;; Just pass the JDocs list into the return object.
#|               
    ;; OLD CODE, NOT USED!!
         (merged-match-lists 
          ;; (print (list 'len (length multiple-results)))
          (loop
           for match-accessor in '(single-word-help-results-exact-matches
                                   single-word-help-results-near-matches
                                   single-word-help-results-keyword-matches)
           as docobj-hash = (make-hash-table :test 'eq)
           collect
           (loop for match-record in multiple-results 
                 as matches = (funcall match-accessor match-record)
                 do 
                 (loop for match in matches 
                       as docobj = (help-match-ref match)
                       do 
                       (if (gethash docobj docobj-hash)
                           (incf (first (gethash docobj docobj-hash)))
                         (setf (gethash docobj docobj-hash) 
                               (list 1 match)
                               )))
                 finally 
                 (return 
                  (mapcar 
                   'second
                   (sort 
                    (hash-table-values docobj-hash) 
                    (lambda (x y) 
                      (cond
                       ((> (first x) (first y)) t)
                       ((< (first x) (first y)) nil)
                       (t (> (help-match-score (second x)) 
                             (help-match-score (second y))))))
                    ))))

	)) ;merged
         ) ;let varlist, if necessary
 |#
         
    
    ;;  "return NIL if there are no matches instead of a null help object"  
    ;; not for me, we return empty object.

    ;;This gets quite baroque, but it appears necessary for historical reasons.
    ;; Instead of spitting out html directly,
    ;; it appears necessary to create and then return a stuffed 
    ;; "single-word-help-results" object.
    ;; Then this is sometime later automagically invoked
    ;; inside a display-web-page context  (see jhelp-html, I believe)
    ;; so that all the html commands it creates, get displayed.
    ;; For some reason, displaying web pages directly is discouraged.
    ;; Currently easier to hack a shim onto this existing interface
    ;; than to find out why so much gingerbread, and whether it's required 
    ;; or optional.
    ;; John Myers, 2011.

    (let ((results 
           (make-single-word-help-results 
            :exact-matches  nil ;(first merged-match-lists)    ;;nil
            :near-matches   nil ; (second merged-match-lists)  ;;nil
            :keyword-matches nil ; (third merged-match-lists)  ;;nil
            :jdocs	     JDocs  
            :search-words    allwords
            )))
      (setf (get wb::*sessionid* :help-matches) results)
      (setf (get wb::*sessionid* :current-search-string) *current-search-string*)

      (when (>= *jhelp-debug* 2)
        (print "SEARCH ON: ") (print (single-word-help-results-search-words results))
        (princ "Results: ") (print results)
        (princ "Exact: ")
        (print (single-word-help-results-exact-matches results))
        (princ "Near:  ")
        (print (single-word-help-results-near-matches results))
        (princ "Keywd: ")
        (print (single-word-help-results-keyword-matches results))
        (princ "J-Docs: ")
        (print (single-word-help-results-jdocs results))
        (princ "Search-words: ")
        (print (single-word-help-results-search-words results))
        )

      results
      )
    ))
      
    
