;;; -*- Package: help; mode: lisp; base: 10; Syntax: Common-Lisp; -*-
(in-package :help)

#|   User Interface:
(Enter-document-Into-JHelp   raw-document  mydoctype  url-functor
	       2.0 * full-title-string  
                       1.0 * full-subtitle-string  
	       1.5 * full-comma-sep-keywords-string  
                       1.0 * full-summary-string  
                       1.0 * full-discussion-string  
	)


Jan 29 '12  Finished fixes to indexing system to handle foo-bar-baz and foo::bar.
	Modified scoring so single title words get extra votes.
|#

;; At this point,  _ is officially punctuation non-characters, but not - and : .  I could change later.
 (defparameter *punctuationwhitespace* 
    '(#\Space #\Tab #\Return #\Newline 
              #\Page #\Null #\Linefeed
              #+:MCL #\312
#\~   #\`   #\!   #\@   #\#   #\$   #\%   #\^   #\&   #\*   #\(   #\)      #\_  #\+  #\=   
#\{   #\[   #\}   #\]   #\|   #\\      #\;   #\"   #\'   #\<   #\,   #\>   #\.  #\?   #\/
              ))

;;
(defparameter *special-punctuation* 
	'( #\-   #\:))

(defun split-at-puctuationwhitespace (s)
  ;;  This is WAAAAAY ugly.  Consider fixing it when there's time.
  ;;  Not significant at run-time for short probe strings, 
  ;;  but still chews up way too many cycles for load time.   But it works.
  (loop for ch in *punctuationwhitespace* do (setq s (substitute #\Space ch s)))
  (remove-if 
   (lambda (x) (zerop (length x))) 
   (string-split s)))

;; JP: How about?
#+not-yet
(defun split-at-puctuationwhitespace (s)
  (remove-if
   (lambda (x) (zerop (length x))) 
   (string-split
    (substitute-if #\space (lambda (ch) (member ch *punctuationwhitespace*)) s)
    )))

;; Or even
#+not-yet
(defun split-at-puctuationwhitespace (s)
  (remove-if
   (lambda (x) (zerop (length x))) 
   (string-split
    (substitute-if 
     #\space 
     (lambda (ch) 
       (and (not (alphanumericp ch)) (char/= ch #\-) (char/= ch #\:)))
     s
     ))))
   
(defun split-at-specialpunctuationspace (s)
  ;;  Input is single string s.
  ;;  Output is (list of strings).
  ;;  This is WAAAAAY ugly.  Consider fixing it when there's time.
  ;;  Not significant at run-time for short probe strings, 
  ;;  but still chews up way too many cycles for load time.   But it works.
  (loop for ch in *special-punctuation*  do (setq s (substitute #\Space ch s)))
  (remove-if 
   (lambda (x) (zerop (length x))) 
   (string-split s)))

#+not-yet
(defun split-at-specialpunctuationspace (s)
  (remove-if
   (lambda (x) (zerop (length x))) 
   (string-split
    (substitute-if #\space (lambda (ch) (member ch *special-punctuation*)) s)
    )))

(defun Record-Document-Contents-Into-Engine-Barrels  
       (JDocID  full-title-string  full-subtitle-string  
                full-comma-sep-keywords-string  full-summary-string  
                full-discussion-string )  
;;
;; User Interface:  [example function call]:
;; (Record-Document-Contents-Into-Engine-Barrels
;;   "My Full Title"  "My Subtitle" "important phrases, other keywords"  "The summarization."  "Full discussion.  Many stuff." )
;;   note:  Here full-strings are real strings, and not lists of strings.  
;;    We will do separation inside here.
;;
;;  assumes JDocID has already been assigned, and *JDocument-count* 
;;   has already been incremented by this point.
;;
;; This important workhorse function takes a bunch of full-string arguments,
;; then slices them up into words,
;; and digests the words into barrels   for that JDocID.
;; Call this once for each document that you want to record.
;; ...Because these are digested words, close enough is good enough.  Colons get lost.  (Or maybe put in twice, in round 2).
;;   Round 3:  Yes, soon foo:bar does indeed get filed as foo:bar, foo, and bar.
;;
;;  Full-strings can have spaces, CR's, [commas, periods], etc. in them.
;;  Lisp is agnostic as to case, so no smallification going on here.
;;  Colons need to get replaced as spaces...they are.


    (let ((wordtallyhash  (make-hash-table :test #'equalp)  )
	(bigramtallyhash  (make-hash-table :test #'equalp)  )
	(oldtally 0)
	)

;; So. This is a simply tallying algorithm that does not normalize for size 
;; [but the helps are small enough that any mention is probably enough, 
;; so this is not such a problem];
;; it does not go for salience or specificity.  
;; Leave these for improvements for Round 3.


;  TO DO:  *Keyword phrases need to be processed separately, 
;; as some of their naive bigrams are bogus.*

     (loop  	for full-string in (list full-title-string  full-subtitle-string full-comma-sep-keywords-string  full-summary-string  full-discussion-string )
	for strength in    '(2.0  1.0  1.5  1.0  1.0)
	for which in '(title subtitle keywords summary text)
	do

;;Round 1:  Assume there's only white-space in the strings, and split on it.   OK
;;Round 2:  **Handle commas, punctuation, quotes, question marks.**   OK
;;Round 3:  Handle - and : and :: for symbols.    OK
;;Round 4:  <html> commands.     TBD

	(let ((words  (split-at-puctuationwhitespace full-string)))

	;;First, we vote on the WORDS.
	  (ecase which
	    ( (text summary)
	      (loop for big-word in words
		for count from 1
		do
		;; (and (equal full-title-string "AND")  (formatt "Text: Big: ~s " big-word))

      	 (let* ((tiny-words (split-at-specialpunctuationspace big-word)))	;; Jan 29 '12 JKM Hack to handle f-b and f:b. Cut them in twice.

	   (when (> (length tiny-words) 1)
	     (push big-word tiny-words))

	   (loop for word in tiny-words do
		;;(formatt " ~s " word)
		(when (not (isa-jhelp-stop-wordp word))
		(if  (setq oldtally  (gethash word wordtallyhash))	;;Anything there already?
		;;Yes--increment new score and put into place.  One line.
		  (setf (gethash word wordtallyhash)  (+  oldtally  ( / strength count)))  ;Words close to first parag weigh more. Any whisper counts.
		;;Else, No--Simply put new score into place.  One line.
		  (setf (gethash word wordtallyhash)  strength  )
		))
	        ) ; loop
			  
	;; File the stems as well as the words themselves, so we can search on the stems.  
	;; But we give a different score weight for the stems.
	;; The stemmer is quite rough and aggressive in version 1, so dial its include scores down--/ 1.5 = 66% of regular score.
	;; To kill the aggression, we only count stems that are 3 letters or longer.
	   (loop for word in tiny-words with stem do
		(when (and (setq stem (find-stem word))   (>= (length stem) 3)  )
			;;(formatt " ~s " word)
			(when (not (isa-jhelp-stop-wordp word))
			(if  (setq oldtally  (gethash word wordtallyhash))	;;Anything there already?
			;;Yes--increment new score and put into place.  One line.
		 	 (setf (gethash word wordtallyhash)  (+  oldtally  ( / strength count 1.5)))  ;Words close to first parag weigh more. Any whisper counts.
			;;Else, No--Simply put new score into place.  One line.
			  (setf (gethash word wordtallyhash)  (/ strength 1.5)  )
			))
		)
	    )
		) ; let*	
	      ) ;loop over big-words
	    )  ;  ecase of text or summary



	    ( (title)
	      (loop for big-word in words do
		;;(formatt "Titles: Big: ~s ~%" big-word)
    	 (let* ((tiny-words (split-at-specialpunctuationspace big-word))
	         (tiny-word-stems (loop for tword in tiny-words with stem 
				when (and (setq stem (find-stem tword)) (>= (length stem) 3))
				collecting stem)
		)
		)	;; Jan 29 '12 JKM Hack to handle f-b and f:b. Cut them in twice.
	   (when (> (length tiny-words) 1)
	     (push big-word tiny-words))
	   (loop for word in (append tiny-words tiny-word-stems)  do
		;;(formatt "Word: ~s " word)
		(when (or   (<=  (length words) 2)   (not (isa-jhelp-stop-wordp word)) )   ;Dec 27 '12 JKM
		(let ((more-strength (+ strength			
			(if (and (equalp (length words) 1) (equalp word (car words)))      8		;;Add 8 if word is single title.
			    (if (and (equalp  (length words) 2)(equalp word (car words)))     4		;;Add 4  if word is first in double title.
			     ;;    (if (equalp (length words) 3)  1		;;Add 1 if word is in triple title.
				0)))))  ;)
		;;(formatt "more-strength: ~s " more-strength)
		(if  (setq oldtally  (gethash word wordtallyhash))	;;Anything there already?
		;;Yes--increment new score and put into place.  One line.
		  (setf (gethash word wordtallyhash)  (+  oldtally  more-strength))
		;;Else, No--Simply put new score into place.  One line.
		  (setf (gethash word wordtallyhash)  more-strength  )
		)))
	        ) ;loop
		) ; let*			
	      ) ;loop over big-words
	    )  ; ecase of Title



	    ( (subtitle keywords)
	      (loop for big-word in words do
		;;(formatt "Titles: Big: ~s ~%" big-word)
    	 (let* ((tiny-words (split-at-specialpunctuationspace big-word))
	         (tiny-word-stems (loop for tword in tiny-words with stem 
				when (and (setq stem (find-stem tword)) (>=  (length stem) 3))
				collecting stem)
		)
		)	;; Jan 29 '12 JKM Hack to handle f-b and f:b. Cut them in twice.
	   (when (> (length tiny-words) 1)
	     (push big-word tiny-words))
	   (loop for word in  (append tiny-words tiny-word-stems) do
		;;(formatt "Word: ~s " word)
		(when (not (isa-jhelp-stop-wordp word))

		(if  (setq oldtally  (gethash word wordtallyhash))	;;Anything there already?
		;;Yes--increment new score and put into place.  One line.
		  (setf (gethash word wordtallyhash)  (+  oldtally  strength))
		;;Else, No--Simply put new score into place.  One line.
		  (setf (gethash word wordtallyhash)  strength  )
		))
	        )) ;loop, let			
	      ) ;loop
	    )
	  ); ecase




	;  Bi-grams do not get stems, I think.

	  ; Next, we vote on the BI-GRAMS.
	  ; ...Since we have already interned foo::bar  and foo-bar-baz as one symbol each,
	  ; this should give decent coverage and we don't take them apart to record bigrams here.
	  ; Instead, we take these as atomic words, and will record bigrams "search foo:bar" and "find foo-bar-baz" etc.
	  ; Reasonable. 
	(when words   ;;Don't process a nil list. Well, cdr of nil doesn't break here, so maybe OK, but...
	    (loop for word in words
	            for nextword in (cdr words)
	            for bigram = (concatenate 'string word " " nextword) 
		do
		(when (not (or (isa-jhelp-stop-wordp word) (isa-jhelp-stop-wordp nextword)))
		(if  (setq oldtally  (gethash bigram bigramtallyhash))	;;Anything there already?
		;;Yes--increment new score and put into place.  One line.
		  (setf (gethash bigram bigramtallyhash)  (+  oldtally  (* strength 1.5) ))  ;;Bi-grams are 1.5 * normal strength.
		;;Else, No--Simply put new score into place.  One line.
		  (setf (gethash bigram bigramtallyhash)  (* strength 1.5)  )
		))
		
	    )) ;loop

	  ;At this point we would vote on the CATEGORIES

	  ; and the LEARNEDs

	)  ; end of let for words

     )  ; end of loop on full-strings.  Now digest the hashtable tally summarizations into the barrels.



	;;Digest the WORDS
    (loop for key being the hash-keys of wordtallyhash    ;;Loop through the vocab words in the tallyhash...
        using (hash-value value)
        with oldBarrel
        do
	;; Do we have a Barrel for this word in the Engine already (from other documents)?
	(if (setq oldBarrel (gethash key (JHelp-Engine-wordhash *JHelp-Engine*) ))
	  ;;  Yes.  Cut a new barrel-entry into it; sort it; and replace it for good luck.
	  (progn
		;;In a parallel / live case, we'd have to sort/merge this in, bubbling up from the back.
		;;Since we are processing OUR JDocs in increasing order, from a dead standstill,
		;;we can simply APPEND the new barrelentry dotted cons onto the end of the Barrel entries.
		(setf   (JBarrel-entries oldBarrel)      (nconc (JBarrel-entries oldBarrel) (list (cons JDocID value))))  ;extra reset for good luck.
		(incf (JBarrel-count oldBarrel))    ;;Goose the count up by one.
		(setf    (gethash key (JHelp-Engine-wordhash *JHelp-Engine*))   oldBarrel )  ;this line also should not be necessary, but put in anyway.
	  )
	  ;;  No.  Create a new Barrel, and then stick a new barrel-entry into it.  Put it into the Engine.
	  (progn
		(setf    (gethash key (JHelp-Engine-wordhash *JHelp-Engine*))    
			(make-JBarrel :entries (list (cons JDocID value)) :count 1) )
	  )
	)  ;end if Barrel already exists
     )  ; end wordtallyhash loop.

	;;Digest the BI-GRAMS
    (loop for key being the hash-keys of bigramtallyhash
        using (hash-value value)
        with oldBarrel
        do
	;; Do we have a Barrel for this word in the Engine already (from other documents)?
	(if (setq oldBarrel (gethash key (JHelp-Engine-bigramhash *JHelp-Engine*) ))
	  ;;  Yes.  Cut a new barrel-entry into it; sort it; and replace it for good luck.
	  (progn
		;;In a parallel / live case, we'd have to sort/merge this in, bubbling up from the back.
		;;Since we are processing OUR JDocs in increasing order, from a dead standstill,
		;;we can simply APPEND the new barrelentry dotted cons onto the end of the Barrel entries.
		(setf   (JBarrel-entries oldBarrel)      (nconc (JBarrel-entries oldBarrel) (list (cons JDocID value))))  ;extra reset for good luck.
		(incf (JBarrel-count oldBarrel))    ;;Goose the count up by one.
		(setf    (gethash key (JHelp-Engine-bigramhash *JHelp-Engine*))   oldBarrel )  ;this line also should not be necessary, but put in anyway.
	  )
	  ;;  No.  Create a new Barrel, and then stick a new barrel-entry into it.  Put it into the Engine.
	  (progn
		(setf    (gethash key (JHelp-Engine-bigramhash *JHelp-Engine*))    
			(make-JBarrel :entries (list (cons JDocID value)) :count 1) )
	  )
	)  ;end if Barrel already exists
     )  ; end bigramtallyhash loop.


	;; PUT CATEGORIESHASH  AND LEARNEDHASH IN HERE.

    )  ; end of let allocating the temporary hashes for tallies.
)  





;;  Note:  functor-url is not some kind of RESTful command to get some box
;;  but is rather the URL of the documentation itself,
;;  HOWEVER,
;;  it has to be entered in a '(eval-me) form
;;  that gets eval'd at RUNTIME
;;  as the system gets too mean to let it work well at load time.
;;  Normally this will be
;;  '(docobj->url doc)

(defun Enter-document-Into-JHelp 
       (doc  mydoctype  functor-url
             full-title-string  full-subtitle-string  
             full-comma-sep-keywords-string  full-summary-string  
             full-discussion-string  
             )
  ;; Takes in a textual document.  Digests it.
  ;; At this point, "full-strings" are sorta vaguely close.  Stringify everything.

  ;       (when T ;(>= *jhelp-debug* 3)
  ;	    (format T "| ~s " full-title-string))
  (when (>= *jhelp-debug* 9)
    (format T "| ~s " full-title-string))
   (when (>= *jhelp-debug* 11)
    (format T "~&KeyWds: ~s ~%" full-comma-sep-keywords-string)) 

  (let* (
	
         ;; Create the JDoc wrapper, and save it in our master array.  
         ;; Get its ID number.
         (JDoc-ID 	*JDocument-count* ) 
         (JDoc 	(make-JDocument   
                 :ID JDoc-ID   :doctype mydoctype   
                 :thedoc doc  :functor-url functor-url
                 :text (stringify full-discussion-string)      ;;This is coming in as alarming Lisp html goop.  Not good.
                 ))
         )
#|
(when (equal mydoctype 'function-documentation)
  (print "FUNDOC:>>")
  (print full-discussion-string)
  (print "<<")
 
)
(if (not (stringp (JDocument-text JDoc)))
  (progn (print "Danger:  Should be text string:")
  (print (JDocument-text JDoc)))
;else
;;  (print "Happy we got a STRING.")
)
|#

    (add-JDocument JDoc) ;;currently a vector-push extend.
    (incf *JDocument-count*)
    (when (not (equal  JDoc-ID  (- (JDocuments-count) 1)))
      (format T "Error--New JDoc ID ~a different from JDocuments length ~a minus one.~%"  JDoc-ID (JDocuments-count))
      )
	
    ;; Now that you have the ID, take the document itself apart.
    ;; Cut the words in the title, the keywords, the description, 
    ;; the paragraph   in to the barrels,
    ;; using the appropriate weighing function.	

        ;;(print "About to Record Document.")
    (Record-Document-Contents-Into-Engine-Barrels   
     JDoc-ID 
     (stringify  full-title-string)
     (stringify  full-subtitle-string)  
     (stringify  full-comma-sep-keywords-string)
     (stringify  full-summary-string)
     (stringify  full-discussion-string)
     )  


    ))



