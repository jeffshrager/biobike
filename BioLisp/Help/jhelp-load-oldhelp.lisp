;;; -*- Package: help; mode: lisp; base: 10; Syntax: Common-Lisp; -*-
#|
Challenges here.
Jul 15 '12  J.Myers  Now that Jeff has come in with a clean REMOVE-HTML-INTELLIGENTLY,
   I am removing the debug statements from the Function and putting the regular code back in.
   help::html-string-for-function-documentation still seems to be leaving out a key section,
   but at least it runs, so it's good enough for now.
Mar 1 '13  J.Myers. Syntax entries were blank.  Functions to massage and use parameter keywords, descriptions.
|#

(in-package :help)


;; Support stuff to tell if functions / symbols will be part of the JHelp Box system, and so can't be counted twice:

(defun lose-BBL-INTERNALS (string)
  (if (and (> (length string) 14)   
           ;;subseq craps out if you feed it too short a string.  Thpppp.
           (equalp (subseq  string 0 14)  "BBL-INTERNALS:"))
      (subseq string 14)
    string
    ))

(defvar *jhelp-VPL-names-dict* (make-hash-table :test #'equalp))

(defun load-jhelp-VPL-names-dict ()
  (loop for sym being the hash-keys in 
        (utils:symbol-value-in-package :*template-symbol->tid* 'NVPL)
        ;;too nice, should use ~s next time.
	for full-name-string = (stringify sym)    
	for nice-name-string = (lose-BBL-INTERNALS full-name-string)
	do
	;;Taste the function-documentation hash.  
        ;; If there, use it.  Set summary at same time.
	(setf (gethash nice-name-string *jhelp-VPL-names-dict*) T )
	(setf (gethash full-name-string *jhelp-VPL-names-dict*) T )
	(setf (gethash sym *jhelp-VPL-names-dict*) T )
        ))

(defun will-go-into-jhelpVPLbox-p (name)
  ;;  (format t "Checking: ~a ...~a. ~%" name   (not (null (or (gethash name *jhelp-VPL-names-dict*) (gethash (stringify name) *jhelp-VPL-names-dict*)))) )
  (or (gethash name *jhelp-VPL-names-dict*) 
      (gethash (stringify name) *jhelp-VPL-names-dict*))
  )

(defun cleanlist-function-parameter-names ( funcobj )
     (let ((syntactic-tokens 
             (get (name funcobj) :syntactic-tokens)))
    (loop
      for p in (parameters funcobj)
      as parameter-name = (or (keyword-name p) (name p))
     for temp = 
      (unless 
          (and (symbol= :token (parameter-type p))
                  (member parameter-name syntactic-tokens :test 'symbol=))
        (intern (name p))    ;;get rid of the nasty random PACKAGE::
        )

    when temp
    collecting temp
     )
   )
)

(defun cleanlist-function-parameter-descriptions ( funcobj )
     (let ((syntactic-tokens 
    ; Every single function has a trap-door plist that allows you to hang info off the symbol name itself.  Go get its :syntactic-tokens.
             (get (name funcobj) :syntactic-tokens)))
    (loop
      for p in (parameters funcobj)
      as parameter-name = (or (keyword-name p) (name p))
     for temp = 
      (unless 
          (and (symbol= :token (parameter-type p))
                  (member parameter-name syntactic-tokens :test 'symbol=))
        (docstring p)
        )

    when temp
    collecting temp
     )
   )
)

#|
Enter-document-Into-JHelp stores the URL of the item that it wants to jump to.
However, apparently this uses and is dependent on wb:user-session-id,
which is only found in the context when you're playing with the web turned on.
It barfs upon being called even at the very end of "load".
So we've turned it into a functor, which needs to be evaluated in order to get the URL at runtime.
This is messy and will have to be fixed.
|#

(defun jhelp-load-oldhelp ()
  (let ( 	; These are all hashes, themselves.
	(documentation-file  (gethash 'documentation-file *documentation*))
	(function-documentation  
         (gethash 'function-documentation *documentation*))
	(glossary-entry  (gethash 'glossary-entry *documentation*))
	(module  (gethash 'module *documentation*))
	(symbol-doc  (gethash 'symbol-doc *documentation*))
	(topic  (gethash 'topic *documentation*))
	(tutorial  (gethash 'tutorial *documentation*))

	(df-count 0)
	(fd-count 0)
	(gloss-count 0)
	(mod-count 0)
	(sym-count 0)
	(topic-count 0)
	(tut-count 0)
	)

    (format t "~%===============================================~%")
    (format t "============ Loading Old Help =================~%")
	

    (load-jhelp-VPL-names-dict)  ; Reset the duplicate names dictionary.

    #|     
    ;; Set these up to do the best for each flavor, to cover the following slots:
    (loop for doc being the hash-values of DOCUMENT-HASH do
	(Enter-document-Into-JHelp   doc  'DOC-FLAVOR  (URL doc)
	        (TITLE doc)   (SUBTITLE  doc)
	        (KEYWORDS doc)   (SUMMARY doc)   (DISCUSSION/TEXT doc)  
	)
     )
|#

    ;; DOCUMENTATION-FILES
    (format   t "~&docfiles: ") (finish-output nil)  ;;Flushes standard output.
    (loop for doc being the hash-values of documentation-file
          for file = (associated-text-file doc)  
          for text = (and file (utils::file-to-string file))  
          do

          (Enter-document-Into-JHelp   
           doc  'documentation-file  
           '(docobj->url doc)
           ;;'(make-help-documentation-file-url :name (name doc))
           (name doc)   (author  doc)
           (keywords doc)   
           (summary doc)   
           text  
           )
          (incf df-count)
          )
	
    (format 
     t "~a of ~a " df-count (hash-table-count documentation-file))



    ;; FUNCTION-DOCUMENTATIONs, if you want them
    ;;
    (when  *jhelp-load-funcdocs*
      (format 
       t "~&funcdocs: ")
      (finish-output nil)  ;;Flushes standard output.
      (loop for doc being the hash-values of function-documentation 

	when (will-go-into-jhelpVPLbox-p (name doc))
		collecting (name doc) into notusefuncs
	unless  (will-go-into-jhelpVPLbox-p (name doc))
		collecting (name doc) into usefuncs
            unless (will-go-into-jhelpVPLbox-p (name doc))
            do
            (Enter-document-Into-JHelp   
             doc  'function-documentation 
             ' (docobj->url doc)
             ;; '(make-help-function-documentation-url :name (name doc) :package (package doc))
             (name doc)   
             (docstring  doc)
             (list (keywords doc)   (cleanlist-function-parameter-names doc ))  ; Keywords are strong, at 1.5.  ;commented out for testing Mar 25 '13
             (list (summary doc)  (syntax doc)  (cleanlist-function-parameter-descriptions doc ))    
	   ; Summary is normal, at 1.0.  Text is also normal, but weighted internally with a declining tail for incrementals.   
             ;;  FULL TEXT...
             ;;  So.  This was causing problems the first time it was compiled.
             ;;  Here's some voodoo to fix it.

#|             
;;Replace the below lines with this, if you want to debug...
            (progn
(print "Help html string:")
              (let ((clean-html) (html-string  (help::html-string-for-function-documentation (name doc))))
(print "Resulting HTML string:")
(print html-string)
(setq clean-html     (forward-package-funcall :bbi :REMOVE-HTML-INTELLIGENTLY html-string))
(print "Cleaned results:")
(print clean-html)
(print "DONE!")

clean-html
))
|#

             (forward-package-funcall 
              :bbi 
              :REMOVE-HTML-INTELLIGENTLY
              (help::html-string-for-function-documentation (name doc))
              :DOCUMENTATION T
              )  ; aux5level.lisp. Loading order. Sigh.
             ;;;;;;          (text doc)  ;;Replaced by above   
             )  ; End of EnterDoc
            (incf fd-count)

	finally
	(terpri)
	(print "Box functions, to be filed later:")
	(print notusefuncs)
	(terpri)
	(print "Simple functions, filed right away:")
	(print usefuncs)
	(terpri)
            )  ; end of Loop over funcs
      
      (format 
       t "~a of ~a " fd-count 
       (hash-table-count function-documentation))
      )




    ;; GLOSSARY-ENTRYs
      (format  t "~&gloss: ")   (finish-output nil)  ;;Flushes standard output.
    (loop for doc being the hash-values of glossary-entry do
          (Enter-document-Into-JHelp   
           doc  'glossary-entry 
           '(docobj->url doc)
           ;; '(make-help-glossary-entry-url :name (name doc))
           (name doc)   (docstring  doc)
           (keywords doc)   
           (summary doc)   
           (text doc)    
           )
          (incf gloss-count)
          )
    (format t "~a " gloss-count)

    ;;  MODULEs
      (format  t "~&modules: ")  (finish-output nil)  ;;Flushes standard output.
    (loop for doc being the hash-values of module do
          (Enter-document-Into-JHelp   
           doc  'module  
           '(docobj->url doc)
           ;;'(make-help-module-url :name (name doc))
           (name doc)   (docstring  doc)
           (keywords doc)   
           (summary doc)   
           (text doc)  
           )
          (incf mod-count)
          )
    (format t "~a " mod-count)


    ;;  SYMBOL-DOCs, if you want to load Symbols (from jhelp-control-panel)
    ;;
    (when *jhelp-load-symbols*
      (format t "~&symbols: ")  (finish-output nil)  ;;Flushes standard output.
      (loop for consdoc being the hash-values of symbol-doc 
       
            do
            ;;Just to be inconsistant, symbol-doc stores a single list of the doc object. 
            (let (( doc
                    (if (consp consdoc) (car consdoc) consdoc)))
              (when (and (consp consdoc) (> (length consdoc) 1))
	;  (format T "~&Warning: JHelp encountered unusual dual (unary-Symbol) in Symbol-Doc hashtable: ~a   but should be OK.~%" consdoc)
                ;;(format T "Only using the first symbol in this list, crossing fingers.  Should be OK.~%")
                )

              (unless (will-go-into-jhelpVPLbox-p (name doc))
                (Enter-document-Into-JHelp   
                 doc  'symbol-doc  
                 '(docobj->url doc)
                 ;; '(make-help-symbol-doc-url :name (name doc)  :package (package doc) :type (dtype doc))
                 (name doc)   (docstring  doc)
                 (keywords doc)   (stype doc)   (text doc)  
                 )
                (incf sym-count)
                )
              )
            )
      (format t "~a " sym-count)
      )


    ;; TOPICs
      (format t "~&topics: ")  (finish-output nil)  ;;Flushes standard output.

    (loop for doc being the hash-values of topic do
          (Enter-document-Into-JHelp   
           doc  'topic 
           '(docobj->url doc) 
           ;;'(make-help-topic-url :name (name doc))
           (name doc)   (docstring  doc)
           (keywords doc)   (summary doc)   (text doc)  
           )
          (incf topic-count)
          )
    (format t "~a " topic-count)


    ;;  TUTORIALs
      (format t "~&tutorials: ")  (finish-output nil)  ;;Flushes standard output.
    (loop for doc being the hash-values of tutorial do
          (Enter-document-Into-JHelp   
           doc  'tutorial 
           '(docobj->url doc)
           ;; '(make-help-tutorial-url :name (name doc))
           (name doc)   (docstring  doc)
           (keywords doc)   (description doc)   (text doc)  
           )
          (incf tut-count) 
          )
    (format t "~a " tut-count)

    (format t "~%===============================================~%")

    (+ 	df-count 
        fd-count 
        gloss-count 
        mod-count 
        sym-count 
        topic-count 
        tut-count )

    )  ; let
  )

