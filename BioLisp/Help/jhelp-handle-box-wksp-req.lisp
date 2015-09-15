;;; -*- Package: help; mode: lisp; base: 10; Syntax: Common-Lisp; -*-

(in-package :help)
#|
User Interface:  
  (jhelp-load-vpl-symbols)  is filed here for historical reasons, to ensure way late after nvpl:.

  Magic URL  /box-wksp-req?PKG=userid789&jdocid=1234    , the handler is defined/published here.
  This is "a request for a nested documentation box to be entered into the workspace,
               pulled from/corresponding with the id of the wrapped general jdoc #1234,
               and sent to the VPL of the terminal used by logged-in session-ID userID789."
   Note that the request is quite flat, only pulling a single index;
   it is up to the JHelp system to pluck out that wrapped document, then do the right thing.
   Currently only jpage-symbols are supported with live, non-zombie boxes as of Feb '12.

Feb 1 '12  J.Myers.  Stomped out nasty echo problem with box being displayed 6 times,
   because the voodoo chicken had been taken out.  Put it back in.  
   Publish needs to be consummated or else the incoming message gets quite frustrated.
   Make sure you complete it.
Feb 6 '12  J.Myers.  
Feb 7 '12  J.Myers.  Key switched to first of subtitle; second of subtitle still holds documentation.  
May 29 '12  J.Myers.  URL for jpage-symbol.  This MUST be eval'd at RUN-TIME,
  as it MUST contain the USER LOGIN of the invoking user. CANNOT eval at LOAD TIME.  Thus functor-url.

Mar 6 '13 J.Myers.  Version 2.  (jhelp-load-vpl-functions) instead of jhelp-load-vpl-symbols. 
Mar 22 '13 J.Myers.  Tuned the new one.
Mar 25 '13 J.Myers.  Nasty heisenbug with page results crashing, not showing up in html output formatting:
 ABS:  Attempt to take CAR of non-list:  "Returns ABS of number".
 PDF:  Attempt to take length of non-sequence :REGULAR
 INTERLEAVE:  Attempt to take length of non-sequence :JOIN
  but then it worked, unable to replicate error.
  **apparently changes may need to be run through the system twice in order to take effect ** due to newer ultra-late run-time compilation?
  ...I think it had to do with the backquote-comma being commented out.  Seems more stable today.
Mar 25 '13  Old functions, major hedge cruft needs to be hedge-trimmed
   after this source file stabilizes.

forward-reference magic:
SYMBOLS:     (utils:symbol-value-in-package :*template-symbol->tid* 'NVPL)
FUNCTIONS:  (forward-package-funcall  :its-pkg  :the-function    my-args...)
|#


;; Jan 29 '12  JMyers from JP.
;;
(defun add-new-single-level-VPLsymbol-to-workspace  ( VPLsymbol  &optional (sessionID wb::*sessionid*) )
;;  Output glue routine.
;;  Actually sends a first-level box (this technology can't handle anything more, sorry) display 
;;  from the server out to the workspace.
;;
;; Looks up the magic ID for a particular high-level symbol.
;; Creates a message pretending to be a pallet-menu Creation click message.
;; Sends it to the VPL interpreter,
;; causing it to think a new high-level box has been asked for
;; and thus cleanly creating it in the workspace.
  (let* (	( channel (get sessionID :vpl-channel))
                ( theVPLsymbol (or (and (not (stringp VPLsymbol)) VPLsymbol)     ;Mar 25 '13.
                            (intern VPLsymbol)))                          ; This is a Hail Mary because it won't be there.  But we tried.
		( message  (vpl::construct-pseudo-message-from-client 
				"palettemenumouseclick"
				(vpl::symbol->template-id    theVPLsymbol )
				:sessionid  sessionID
			))
		( function-handler (function vpl::VPL-JSON-DECODE-MESSAGE-AND-EXECUTE-IT))		
	        )
	  (vpl::vpl-handler channel message function-handler)
	); let
)



;; This pretty much has to be >here<, because nvpl is defined so late in the game.
;; Now that we have the forward-delayed symbols, it is perhaps possible that this loading function
;; should be broken out into its own documentation file, to keep things clean.
;; However, this could very well break the loading order again, so save for middle term.
;; Let's not break stuff that's actually working right now.


#|  ;; now much earlier.
(defun lose-BBL-INTERNALS (string)
	(if (and (> (length string) 14)   ;;subseq craps out if you feed it too short a string.  Thpppp.
	           (equalp (subseq  string 0 14)  "BBL-INTERNALS:"))
	  (subseq string 14)
	string
))
|#

;;:IN-PACKAGE :BBI
;; From Jeff Elhai. 
(DEFUN Collect-leaves (complex-list)
   (loop for element in complex-list
        APPEND
          (COND
             ((BBI::IS-SIMPLE-LIST? element)
                (LIST (FIRST element)))
             ((LISTP element)
              (FORWARD-FUNCALL 'Collect-leaves element))
              )
        )
)



;Mar 25 '13.  This should be reworked at some point.
;; It is only used for functions / symbols coming in from Collect-Leaves in the form of "strings"
;; which were NOT found inside the function documentation hash
;; and are therefore lost-and-found undetermined.
;; Maybe about 100 of them.
;;
(defun nodocobj->url (name)
    (if (eq (symbol-package name) (find-package :cl))
        (wb::common-lisp-external-symbol-url name)
      (make-help-symbol-doc-url 
       :name (url-safe-string name) 
       :package (url-safe-string "Unspecified-Package")
       :type (url-safe-string (string :function))
       )))



(defun jhelp-load-vpl-functions ()
  (format t "~%===============================================~%")
  (format t "============= Loading VPL Functions==============~%")
;  (format t "~&jfunction-count:... ") (finish-output nil)  ;;Flushes standard output.
  
  ;  So.  This turns out to be a little bit of a bitch
  ; because the collect-leaves flattened tree comes out with "STRINGS" that have no packages in them,
  ; whereas the function doc hash files things under SYMBOLS, which normally are clean but occasionally have PKG:SYMB on them.
  ; Typically UTILS: or BBI::  .
  ;  So...
  ;  We file the "STRINGS" into a gigantic hash table, and stick a count behind each of them:  0.
  ;  Then we rip through the functions in the function doc hash.   We convert their names into strings.
  ;  If anything string-matches the hash, not worrying about case,
  ;  we go ahead and cut an entry for it;
  ;  otherwise, we barf it out as a duplicate function reject, not on our menus.
  ;  We increment any hash that we do find.
  ;  Afterwards, we rip through the name hash table,
  ;  and print out a separate list of orphans that didn't get picked up.
  ;  We should probably duplicate this, run through the existing symbol list
  ;  and print out a list of existing symbols that didn't get picked up.
  ;  Fun.
  
  (let* (  (func-names   (remove-duplicates (COLLECT-LEAVES nvpl::*vpl-module-tree*) :test 'equal)  )
         (nonfunc-symbol-count 0)
;         (nonfunc-oldsym-count 0)
;         (redundant-symbol-count 0)
         (length-of-tree (length func-names))
         (tree-hash  (make-hash-table :test #'equalp))
;         (symb-hash  (make-hash-table :test #'equalp))
         (funcprint 0 )
         )



    (when (>= *jhelp-debug* 3)
      (print "Flattened Tree's Function Names:")
      (print func-names)
      (print "Function Hashes:")
     (loop for doc being the hash-keys of   (gethash 'function-documentation *documentation*)
	do
	(format t "~S " doc))
    )

  ; Stick "names" into tree-hash.
    (loop for symname   in  func-names 
	when symname   ;sometimes this is NIL!  Don't file it then!
	do 
	(setf (gethash symname tree-hash) 0)   )

;   ;Stick old symbol assortment into symb-hash.
;    (loop for sym being the hash-keys  in  (utils:symbol-value-in-package :*template-symbol->tid* 'NVPL)
;	for full-name-string = (stringify sym)    ;;too nice, should use ~s next time.
;	for nice-name-string = (lose-BBL-INTERNALS full-name-string)
;	do
;	    (setf (gethash nice-name-string symb-hash) 0)   )





;(print " ")
;     (format t "~&Redundant functions NOT put into JHelp:~%" )
    (when (>= *jhelp-debug* 5)
     (format t "~&Using the following functions put into JHelp:~%" )
     )

     ;Now do the real work.
     ;
     ;John says:  I believe it is necessary to loop through the function documentation,
     ;which can then be simplified and turned into strings,
     ;as it is probably not possible at this time to loop through the simple strings
     ;and use them to access the complex function documentation.
     ;But I haven't looked at it recently, and I could be wrong.
     ;
      (loop for funcdoc being the hash-values of   (gethash 'function-documentation *documentation*)
		        using (hash-key sym)   ;;pulls out symbol too, simultaneously.  Note hash-key is singular, and requires parens.
	for full-name-string = (stringify (name funcdoc)) ;;  see next line
	;;;;;???why is stringify necessary here? ;; was (stringify sym)    ;;too nice, should use ~s next time.
	for nice-name-string = (lose-BBL-INTERNALS full-name-string)
	with doc
	for qURL = '""			;; May 29 '12
	for summary = ""
	
     ;;when sym  ;spurious NIL in tree list, bolluxes things up
	do
	;;Taste the function-documentation hash.  If there, use it.  Set summary at same time.
;(print "Working on")
;(print full-name-string)
;(print nice-name-string)

	(if  (gethash nice-name-string tree-hash)   ;Has it been blessed, to be here?  ..note this comes out 0, which is True in Lisp.
	  ;;Yes; use the summary.
	(progn
		(setq 
		qURL	`(docobj->url ,funcdoc)
		summary (summary funcdoc))
		(when (>= *jhelp-debug* 8)
		(format t "~&Found function doc for ~s~%" nice-name-string ))
		(when (>= *jhelp-debug* 5)
		(format t "~s ~%"  nice-name-string))

		; Tally the tree-hash, just to be nice.
		(incf  (gethash nice-name-string tree-hash)) 

;		; hey let's look in the symb-hash and tally it, just for grins.  Can delete this if needed.
;		(when (gethash nice-name-string symb-hash)
;			(incf (gethash nice-name-string symb-hash)))


    (setq doc  (make-JPage
		;;  :name                      full-name-string  ;;NOT SUPPORTED YET, DON'T USE THIS.
		 :title        	full-name-string	;; This one's important.
			;; Note oldsymdoc is guaranteed to be of type funcdoc or symboldoc, so this next is OK.
		 :subtitle     (list sym (and funcdoc (docstring  funcdoc)))        ;;Now using first as key to box, see far below. 
		 :keywords     	(or (and funcdoc (keywords funcdoc))
					full-name-string)
		 :logical-form   `     (427 :JBML-DELETE :JBML-OUTDENT
 (5013 :JBML-MAIN-BOX-MENU "" (549 :JBML-MENU-ENTRY "Help")
  (122 :JBML-MENU-ENTRY "Collapse") (136 :JBML-MENU-ENTRY "Execute")
  (143 :JBML-MENU-ENTRY "Copy") (150 :JBML-MENU-ENTRY "Cut")
  (164 :JBML-MENU-ENTRY "Insert")
  (535 :JBML-MENU-ENTRY "Surround with")
  (185 :JBML-MENU-ENTRY "Show code") (290 :JBML-MENU-ENTRY "Results")
  (171 :JBML-MENU-ENTRY "Cut/Insert") (269 :JBML-MENU-ENTRY "Monitor"))
 ,nice-name-string
 :JBML-B
)  ; end of Logical Form
		 :summary      	summary
		 :text         	(or (and funcdoc (text funcdoc))
					full-name-string)
		 ))


	(Enter-document-Into-JHelp

	   doc  'jpage-symbol

	     ;;(format nil "/jpage/~a" (JPage-title  doc))  ;;URL.  This is being deprecated.  ...Whoops, now it needs to be a form to get eval'd.
	       qURL        ;; (quote url_contents_string), what we need.  Actually '(function-to-find-URL '#<docobj>) .
;;'(make-help-documentation-file-url :name (name doc))

	        (JPage-title doc)   

	      (second  (JPage-subtitle  doc))

	; Here we put in the parameters.
	        (list  (JPage-keywords doc)    (cleanlist-function-parameter-names funcdoc )  ) ; Keywords are strong, at 1.5.

	        (list  (JPage-summary doc) (syntax funcdoc)   (cleanlist-function-parameter-descriptions funcdoc ))  

	        (JPage-text doc)  
	) ; Enter-doc
	(incf *jsymbol-count*)


	) ; progn we got two matches.

	  ;else
	; this following clause now deprecated, commented out.


	  ;Here we have something that's in the function documentation, but not blessed by being in the tree name table.
	;; WE DO NOT WANT THESE.
	(progn
	(when (>= *jhelp-debug* 5)
	(format t "~&Rejected ~s ~%"  nice-name-string))

;	; Simply print it out.
;	(format t "~s "  nice-name-string)
;	(when (> (incf funcprint) 4)
;	      (setq funcprint 0)
;	    (format t "~%"))
;	(incf redundant-symbol-count)
;
;		; hey let's look in the symb-hash and tally it, just for grins.  Can delete this if needed.
;		(when (gethash nice-name-string symb-hash)
;			(incf (gethash nice-name-string symb-hash)))
       ) ;progn else.
      ); if
     )  ; loop
;	(format t "~&~s total deprecated functions.~%" redundant-symbol-count)


#| |#
         ;; WE WANT THESE

      (when (>= *jhelp-debug* 0)
	(format t "~&Palette functions that do not have documentation pages written yet:~%" )  (setq funcprint 0)
	)
      	(loop for fcount being the hash-values of   tree-hash
		        using (hash-key symname)   ;;pulls out symbol too, simultaneously.  Note hash-key is singular, and requires parens.
	for full-name-string = (stringify symname) ;should be a string already.
	for nice-name-string = (lose-BBL-INTERNALS full-name-string)  ;;should have none but never know
	for summary = ""
	    with doc
	;    for qURL = '""			;; Mar 25 '13
	    when symname  ;spurious NIL in tree list, bolluxes things up
	    do
	    (when (= fcount 0)   ; we didn't take care of it already...


	      ; INSERT SYMNAME, A STRING.
		(setq 
     ;		qURL	`(nodocobj->url ,nice-name-string) ;;FIX THIS!!!
		summary "This function has no summary.  Ask Jeff to write documentation for it."  
		)

               (when (>= *jhelp-debug* 4)
                     (format t "~s... ~%"  nice-name-string))

    (setq doc  (make-JPage
		 :title        	full-name-string	;; "This one's important". 
			;; "Note oldsymdoc is guaranteed to be of type funcdoc or symboldoc, so this next is OK." not any more here.
		 :subtitle     (list symname symname)        ;;Now using first as key to box, see far below. MUST BE A LIST. SECOND is doc!
		 :keywords     	(or full-name-string)
		 :logical-form   `     (427 :JBML-DELETE :JBML-OUTDENT
 (5013 :JBML-MAIN-BOX-MENU "" (549 :JBML-MENU-ENTRY "Help")
  (122 :JBML-MENU-ENTRY "Collapse") (136 :JBML-MENU-ENTRY "Execute")
  (143 :JBML-MENU-ENTRY "Copy") (150 :JBML-MENU-ENTRY "Cut")
  (164 :JBML-MENU-ENTRY "Insert")
  (535 :JBML-MENU-ENTRY "Surround with")
  (185 :JBML-MENU-ENTRY "Show code") (290 :JBML-MENU-ENTRY "Results")
  (171 :JBML-MENU-ENTRY "Cut/Insert") (269 :JBML-MENU-ENTRY "Monitor"))
 ,nice-name-string
 :JBML-B
)  ; end of Logical Form
		 :summary      	summary
		 :text         	(or full-name-string)
		 )) ;make-JPage setq doc



	(Enter-document-Into-JHelp

	   doc  'jpage-symbol    ;this is a type, don't change it.

	     ;;(format nil "/jpage/~a" (JPage-title  doc))  ;;URL.  This is being deprecated.  ...Whoops, now it needs to be a form to get eval'd.
;	       qURL        ;; (quote url_contents_string), what we need.  Actually '(function-to-find-URL '#<docobj>) .
; help-documentation-file-url is an object that stores generic information.
;;'(make-help-documentation-file-url :name (name doc))
`(make-help-documentation-file-url :name ,symname )
;;****  WAS USING qURL.  and is again. No, insufficient information.  Up for grabs. Use this one instead.

	        (JPage-title doc)   

	      (second  (JPage-subtitle  doc))

	; Here we put in the parameters.
	        (list  (JPage-keywords doc)  
		;;     (cleanlist-function-parameter-names funcdoc )  
		       ) ; Keywords are strong, at 1.5.

	        (list  (JPage-summary doc) 
		 ;;    (syntax funcdoc)   
		 ;;    (cleanlist-function-parameter-descriptions funcdoc )
		       )  

	        (JPage-text doc)  
	) ; Enter-doc




		; Print it out.
              (when (>= *jhelp-debug* 0)
		(format t "~s "  symname)
		(when (> (incf funcprint) 4)
		      (setq funcprint 0)
	   		 (format t "~%"))
	       )
		(incf nonfunc-symbol-count)
	    ))
	(format t "~&~s total important functions inserted into JHelp, lacking documentation.~%"  nonfunc-symbol-count)
#| |#




        ;; WE DON'T WANT THESE ...Symbols from the previous version of 2012.  Extra Lisp stuff etc., unwanted.
;;	(format t "~&Other important symbols/functions NOT put into JHelp:~%" )  (setq funcprint 0)
;;      	(loop for fcount being the hash-values of   symb-hash
;;		        using (hash-key symname)   ;;pulls out symbol too, simultaneously.  Note hash-key is singular, and requires parens.
;;	    do
;;	    (when (= fcount 0)
;;		; Simply barf it out.
;;		(format t "~s "  symname)
;;		(when (> (incf funcprint) 4)
;;	   		 (format t "~%"))
;;		(incf nonfunc-oldsym-count)
;;	    ))
;;	(format t "~&~s total other important symbol/functions left out.~%"  nonfunc-oldsym-count)
	




	(format t "functions: ~a   undocumented symbols: ~a  extras/duplicates (e.g, nil): ~a   out of ~a total" 
	            *jsymbol-count*  nonfunc-symbol-count  
	            (- length-of-tree *jsymbol-count*  nonfunc-symbol-count)
   	            length-of-tree
	)

;	(format t "functions: ~a   passed-over symbols: ~a  neither: ~a   other (nil): ~a   out of ~a total" 
;	            *jsymbol-count*  nonfunc-symbol-count  nonfuncnonsym-count 
;	            (- length-of-tree *jsymbol-count*  nonfunc-symbol-count  nonfuncnonsym-count )
;   	            length-of-tree
;	)



	(format t "~&===============================================~%")

))  ; let* defun





;; Round Three version.
;; Gets results, BUT:
;; -All functions are "Struck Out" as "names" can't look up symb documentation in function hash
;; -As a result, no yellow boxes work; and all red Q-marks simply pop up the page again.
;; -barely useable, not really.
;;
(defun jhelp-load-vpl-functions-ugly ()
	(format t "~%===============================================~%")
	(format t "============= Loading VPL Symbols==============~%")
	(format t "~&*jsymbol-count*: ") (finish-output nil)  ;;Flushes standard output.

  (let* (  (func-names   (remove-duplicates (COLLECT-LEAVES nvpl::*vpl-module-tree*) :test 'equal)  )
          )
;    (loop for sym being the hash-keys  in  (utils:symbol-value-in-package :*template-symbol->tid* 'NVPL)
    (loop for sym in func-names
	for full-name-string = (stringify sym)    ;;too nice, should use ~s next time.
	for nice-name-string = (lose-BBL-INTERNALS full-name-string)
	with doc 
	with oldsymdoc
	for qURL = '""			;; May 29 '12
	for summary = ""
      when sym  ;;Must be after all FORs. necessary as too many NILs in list
	do
	;;Taste the function-documentation hash.  If there, use it.  Set summary at same time.
	(if (setq oldsymdoc (gethash sym (gethash 'function-documentation *documentation*)))
	  ;;Yes; use the summary.
	(progn
	  (format T "~&Found ~s in function documentation.~%" sym)
	(setq 
		qURL	`(docobj->url ,oldsymdoc)
		summary (summary oldsymdoc)
	)
	)
	  ;else
	  ; No; try the old symbol table...
	(if (setq oldsymdoc  (car (gethash sym (gethash 'symbol-doc *documentation*))))
	(progn
	  (format T "~&Found ~s in symbol documentation.~%" sym)
	  (setq 
		qURL	`(docobj->url ,oldsymdoc)
		summary (stype oldsymdoc))
	  )
	   ;else, struck out
	   ; oldsymdoc is already nil at this point. 
	(progn
	  (format T "~&Struck out, unable to find ~s in function documentation.~%" sym)
	  (setq summary full-name-string)
	)
	))
    (setq doc  (make-JPage
		 :title        	full-name-string	;; This one's important.
			;; Note oldsymdoc is guaranteed to be of type funcdoc or symboldoc, so this next is OK.
		 :subtitle     (list sym (and oldsymdoc (docstring  oldsymdoc)))        ;;Now using first as key to box, see far below. 
		 :keywords     	(or (and oldsymdoc (keywords oldsymdoc))
					full-name-string)
		 :logical-form   `     (427 :JBML-DELETE :JBML-OUTDENT
 (5013 :JBML-MAIN-BOX-MENU "" (549 :JBML-MENU-ENTRY "Help")
  (122 :JBML-MENU-ENTRY "Collapse") (136 :JBML-MENU-ENTRY "Execute")
  (143 :JBML-MENU-ENTRY "Copy") (150 :JBML-MENU-ENTRY "Cut")
  (164 :JBML-MENU-ENTRY "Insert")
  (535 :JBML-MENU-ENTRY "Surround with")
  (185 :JBML-MENU-ENTRY "Show code") (290 :JBML-MENU-ENTRY "Results")
  (171 :JBML-MENU-ENTRY "Cut/Insert") (269 :JBML-MENU-ENTRY "Monitor"))
 ,nice-name-string
 :JBML-B
#|
 (441 :JBML-HOLE :JBML-OUTDENT :JBML-BACKGROUND-COLOR "#FFFFFF"
  (5014 :JBML-MAIN-BOX-MENU "" 
   (549 :JBML-MENU-ENTRY "Help")
   (157 :JBML-MENU-ENTRY "Paste")
   (535 :JBML-MENU-ENTRY "Surround with")
   (206 :JBML-MENU-ENTRY "Multiline input")
   (542 :JBML-MENU-ENTRY "Unsurround"))
  "number" :JBML-COLOR "#B7410E" :JBML-I)
|#
)  ; end of Logical Form
		 :summary      	summary
		 :text         	(or (and oldsymdoc (text oldsymdoc))
					full-name-string)
		 ))

	(Enter-document-Into-JHelp   doc  'jpage-symbol
	     ;;(format nil "/jpage/~a" (JPage-title  doc))  ;;URL.  This is being deprecated.  ...Whoops, now it needs to be a form to get eval'd.
	       qURL        ;; (quote url_contents_string), what we need.  Actually '(function-to-find-URL '#<docobj>) .
;;'(make-help-documentation-file-url :name (name doc))
	        (JPage-title doc)   
	      (second  (JPage-subtitle  doc))
	        (JPage-keywords doc)   
	        (JPage-summary doc)   
	        (JPage-text doc)  
	)
	(incf *jsymbol-count*)
     )
	
	(format t " ~a" *jsymbol-count* )

	(format t "~&===============================================~%")

))
;; round three

















;; Old 2010-2012 version.   Currently unused.
;
(defun jhelp-load-vpl-symbols ()
	(format t "~%===============================================~%")
	(format t "============= Loading VPL Symbols==============~%")
	(format t "~&*jsymbol-count*: ") (finish-output nil)  ;;Flushes standard output.

    (loop for sym being the hash-keys  in  (utils:symbol-value-in-package :*template-symbol->tid* 'NVPL)
	for full-name-string = (stringify sym)    ;;too nice, should use ~s next time.
	for nice-name-string = (lose-BBL-INTERNALS full-name-string)
	with doc 
	with oldsymdoc
	for qURL = '""			;; May 29 '12
	for summary = ""
	do
	;;Taste the function-documentation hash.  If there, use it.  Set summary at same time.
	(if (setq oldsymdoc (gethash sym (gethash 'function-documentation *documentation*)))
	  ;;Yes; use the summary.
	(setq 
		qURL	`(docobj->url ,oldsymdoc)
		summary (summary oldsymdoc))
	  ;else
	  ; No; try the old symbol table...
	(if (setq oldsymdoc  (car (gethash sym (gethash 'symbol-doc *documentation*))))
	  (setq 
		qURL	`(docobj->url ,oldsymdoc)
		summary (stype oldsymdoc))
	   ;else, struck out
	   ; oldsymdoc is already nil at this point. 
	  (setq summary full-name-string)
	))
    (setq doc  (make-JPage
		 :title        	full-name-string	;; This one's important.
			;; Note oldsymdoc is guaranteed to be of type funcdoc or symboldoc, so this next is OK.
		 :subtitle     (list sym (and oldsymdoc (docstring  oldsymdoc)))        ;;Now using first as key to box, see far below. 
		 :keywords     	(or (and oldsymdoc (keywords oldsymdoc))
					full-name-string)
		 :logical-form   `     (427 :JBML-DELETE :JBML-OUTDENT
 (5013 :JBML-MAIN-BOX-MENU "" (549 :JBML-MENU-ENTRY "Help")
  (122 :JBML-MENU-ENTRY "Collapse") (136 :JBML-MENU-ENTRY "Execute")
  (143 :JBML-MENU-ENTRY "Copy") (150 :JBML-MENU-ENTRY "Cut")
  (164 :JBML-MENU-ENTRY "Insert")
  (535 :JBML-MENU-ENTRY "Surround with")
  (185 :JBML-MENU-ENTRY "Show code") (290 :JBML-MENU-ENTRY "Results")
  (171 :JBML-MENU-ENTRY "Cut/Insert") (269 :JBML-MENU-ENTRY "Monitor"))
 ,nice-name-string
 :JBML-B
#|
 (441 :JBML-HOLE :JBML-OUTDENT :JBML-BACKGROUND-COLOR "#FFFFFF"
  (5014 :JBML-MAIN-BOX-MENU "" 
   (549 :JBML-MENU-ENTRY "Help")
   (157 :JBML-MENU-ENTRY "Paste")
   (535 :JBML-MENU-ENTRY "Surround with")
   (206 :JBML-MENU-ENTRY "Multiline input")
   (542 :JBML-MENU-ENTRY "Unsurround"))
  "number" :JBML-COLOR "#B7410E" :JBML-I)
|#
)  ; end of Logical Form
		 :summary      	summary
		 :text         	(or (and oldsymdoc (text oldsymdoc))
					full-name-string)
		 ))

	(Enter-document-Into-JHelp   doc  'jpage-symbol
	     ;;(format nil "/jpage/~a" (JPage-title  doc))  ;;URL.  This is being deprecated.  ...Whoops, now it needs to be a form to get eval'd.
	       qURL        ;; (quote url_contents_string), what we need.  Actually '(function-to-find-URL '#<docobj>) .
;;'(make-help-documentation-file-url :name (name doc))
	        (JPage-title doc)   
	      (second  (JPage-subtitle  doc))
	        (JPage-keywords doc)   
	        (JPage-summary doc)   
	        (JPage-text doc)  
	)
	(incf *jsymbol-count*)
     )
	
	(format t " ~a" *jsymbol-count* )

	(format t "~&===============================================~%")

)
;; old version 2012.







(wb::define-url&pkg&args
      box-wksp-req "/box-wksp-req" :jdocid)
;; This is a magic meta-definition that creates *box-wksp-req* = path?PKG=user&jdocid

(publish
  :path  *box-wksp-req*
  :content-type cl-user::*html-publish-content-type*  ;;this line should go away, but...
  :function
    (lambda (req ent)
       (setq ent ent)  ; shut the compiler up. Can we use nil as arg? fix later.
        (let* ((input (request-query req))
	 (package-name (url-parameter-value :pkg input))
	 (package-symbol (keywordize package-name))
                 (JDocID (read-from-string (url-parameter-value :jdocid input)))  ;; not "1234", it's a string, and we're not JS...

	  ;  context for ajax pipe output, shadowing the dynamic variables.
	  (wb:*sessionid*	package-symbol)
	  (nvpl::*channel*    (get package-symbol :vpl-channel)                  )
	  (jdoc  (get-JDocument JDocID))   ;; TO DO:  WHAT TO DO IF NOT FOUND? ...please return NIL.
	  (lform      (and jdoc (JPage-logical-form
		       (JDocument-thedoc    ;; And THIS is now a JPage.
			jdoc ))))     
	  )
           

;;        (format T  "Request for JDoc#~a using package ~a, channel ~a.~%" JDocID package-symbol nvpl::*channel*)
;;        (format T  "Sending LForm add-workspace ~s.~%" lform)

	(if (eq (JDocument-doctype jdoc) 'jdoc)   ;;otherwise it's a jdoc-symbol.

	(progn
  	  (nvpl::add-workspace  
		;   '(1 MyWorkspace :jbml-b foo :jbml-b (2 Cleared))   ; test input
	     lform
	    )
	) ;progn
;else
	(add-new-single-level-VPLsymbol-to-workspace (first (JPage-subtitle  (JDocument-thedoc jdoc  ))))  ;;Use the subtitle.
	);if


	(execute-with-standard-weblistener-environment   ;;voodoo chicken.
	req ent package-symbol
	 (lambda ()
     		(html "Thanks, got your message." )   ;; needed to shut up the resend echo if nothing ack'd back.
	 ) ;lambda
	 ) ; execute

)))



