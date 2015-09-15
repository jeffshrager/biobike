;;; -*- Package: help; mode: lisp; base: 10; Syntax: Common-Lisp; -*-
(in-package :help)
(defvar jh-boxstamp 0)  ;;private local variable.  Arbitrary integer.

;;; +=========================================================================+
;;; | Copyright (c) 2011 John Myers, J.P. Massar, Jeff Shrager, Peter Seibel  |
;;; |                                                                         |
;;; |                                                                         |
;;; | THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,         |
;;; | EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF      |
;;; | MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  |
;;; | IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY    |
;;; | CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,    |
;;; | TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE       |
;;; | SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.                  |
;;; +=========================================================================+
#| May 29 '12 Printed title of output result now deprecated when box exists.
;;	We turn the Help Page into a Link.
;; Jun 25 '12  Context printouts to show where word(s) come up in results page.
;;	Takes *multiple* search words as inputs, including stemmed words,
;;	but it searches for the real ones first
;;	and first hit overrides for any one line, so stemmed results are secondary to primary results.
;;	Not sure if it puts hypen-ated and foo:bar tuples in front or not, this would be good.
Jun 25 '12  Changed title so it reflects search string.  Put in search-words as extra parameter in answer form.
Jun 25 '12  html-for-elhai routines now take  search-words, no, match as an arg, so can display context.  
	Woa.  Backing this out, as we don't have a match object on John's side.
Jul 11 '12  Top-aligned all <tr valign="top">s.  Lots of cleanup.
Jul 14 '12  Splitting Function out separately from the other help prints.
Jul 17 '12  <nobr>, bigger (?); right padding on <td> for Box; 500 now 60 for context
Jul 19 '12  ellipses at beginning for functions in addition to files.
            Put JDoc-text in as alternate for Boxes.
Mar 25 '13  More detailed debugging statements, not shown during normal operation.



Displaying of the results is done magically using inductance coils through routine wb::out-record-to-html 
in help-single.lisp, note not jhelp.  This invokes (display-single-word-jhelp-results form), in middle of this file.
|#
;;;;;;;;;;;;;;;;;;;Format routines for comment string after a search result.;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;Note these are already inside a :td context; table would not be consistent otherwise;;;
;;... if more fancy stuff is required, the next step would be to assign the search string
;; into the single-word search results, then pull it out and use it for fancy formatting / search matches.
;;  ...No, it's returned as a global in (get wb::*sessionid* :current-search-string) .   One single long string, I believe.

(defun jhtml-for-elhai-JPage  (docobj search-words JDoc-text)
;; SECOND LEVEL.
;;  Used for futuristic hierarchical-box JPages, which have been deprecated for now on an indefinite basis
;;  until the multi-level boxes stop giving zombie results in the workspace.
;;
;;    (declare (ignore search-words))
;;    (html (:princ-safe ">JPage>"))

	(html (:princ-safe
     (jemit-limited-match-with-search-term-bolded     ;;power version, but only one line.
       (jstring-for-text-match-line (or  (JPage-subtitle docobj) JDoc-text  "") search-words) 
      search-words :color "red" :limit 60
      )
        ))

;;    (html (:princ-safe "<<"))
)

(defun jhtml-for-elhai-jpageVPLsymbol ( docobj search-words JDoc-text )
;; SECOND LEVEL.
;;  Used for the really nice JHelp Symbol Boxes, the single-level yellow ones, that handle both functions and symbols.
;;
;; Only called when result is of type Jpage Symbol, which puts up the magic single-level box and "(?)".
;; Massage the Explanation.

     ;   (declare (ignore search-words))

;    (html (:princ-safe ">symb>"))
     (jemit-limited-match-with-search-term-bolded     ;;power version, but only one line.
       (jstring-for-text-match-line (or (second (JPage-subtitle docobj)) JDoc-text "") search-words)    ;; ... at beginning.         
      search-words :color "red" :limit 60
      )

;    (html (:princ-safe "<<"))

  ;;first is the object itself, not a string.
;;	(html (:princ-safe (or (second (JPage-subtitle docobj))  "") ))
)

(defun jhtml-for-elhai-documentation-file (docobj search-words)
;; SECOND LEVEL.
;;  Used for file objects.
;;
;  Master Description Output For the long file objects.
;  Pulls the search text out of the docobj, so you don't need to pass it in.
	;;Here you would take this, take the search probe sentence,
	;;search the object, format it into half a line, and return the results.
	;;  The subtitle field is using (author docobj), not so good.
	(html 
            ;;(:princ-safe ">file>") 
            (:princ-safe (or (summary docobj)	""))  
            ;;(:princ-safe "<<")
	    (html-for-jhelp-formatted-search-result-docobj docobj search-words )   ; autoresolves to method for docfiles.
	)
)

(defun jhtml-for-elhai-function-doc ( docobj search-words JDoc-text)
;; SECOND LEVEL.
;;  Used for Function Documentations, the old-style text objects.
;;
;  Master Description Output For old Help that is the Function 
;;(print "Formatting>>")
;; (print JDoc-text)
	(html 
;            (:princ-safe ">else>") 
  ;          (:princ-safe (or  (docstring docobj) "") )

  ;          (:princ-safe JDoc-text)  ;; delete this later.

     (jemit-limited-match-with-search-term-bolded       ;;power version, but only one line.
       (jstring-for-text-match-line (or JDoc-text (docstring docobj)  "") search-words)    ;; ... at beginning.              
      search-words :color "red" :limit 60
      )  
;            (:princ-safe "<<")
;;	    (html-for-jhelp-formatted-search-result-docobj docobj  search-words)     ; autoresolves to method for others.
	)
;;(print "<<")
)

(defun jhtml-for-elhai-everything-else ( docobj search-words JDoc-text)
;;  SECOND LEVEL.
;;  Used for Symbol-Docs, Topics, 
;;
;  Master Description Output For old Help that is not the long file objects nor function documentations.
;;(print "Formatting>>")
;;(print JDoc-text)
; (declare (ignore JDoc-text))
	(html 
;            (:princ-safe ">else>") 
  ;          (:princ-safe (or  (docstring docobj) JDoc-text "") )
     (jemit-limited-match-with-search-term-bolded      ;;power version, but only one line.
        (jstring-for-text-match-line (or (docstring docobj) JDoc-text "") search-words)    ;; ... at beginning.
      search-words :color "red" :limit 60
      )  
;            (:princ-safe "<<")
;;	    (html-for-jhelp-formatted-search-result-docobj docobj  search-words) ;;match)    ; autoresolves to method for others.
	)
;;(print "<<")
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;end of formatting.;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun display-single-word-jhelp-results (results)
;; FIRST LEVEL.
  (when (>= *jhelp-debug* 2)
	(print "JHelp:  display-single-word-jhelp-results called."))

  (let* (
;;         (exact-matches (single-word-help-results-exact-matches results))
;;         (near-matches (single-word-help-results-near-matches results))
;;         (keyword-matches (single-word-help-results-keyword-matches results))
;;         (all-matches (append exact-matches near-matches keyword-matches))
;;         (remaining-matches all-matches)
;;         (n-exact (length exact-matches))
;;         (first-match (first exact-matches))
;;         (second-match (second exact-matches))
;;         (n-displayed 0)
	(search-string (get wb::*sessionid* :current-search-string) )
	(search-words (single-word-help-results-search-words results)) 
	(local-box-stamp jh-boxstamp)

	(JDocs (single-word-help-results-jdocs results))   ;Simply pulls the jdocs slot from the object. 
	   ;; was list of IDs, currently list of (score.ID) pairs.
	(n-matches  (length JDocs))
        (title 
       	   (formatn "BioBIKE Help Search for '~A' (~D top results)" search-string n-matches))
	 )

    ;; Display algorithm. All matches come with a link to a page which will
    ;; display the documentation for entire item (or the HTML page, etc),
    ;; the type of documentation item 
    ;; and a 1/2 line summary 
    ;; (maybe with a tooltip to show the entire summary string?).
    ;; Jun '12  Now we are also displaying gratuitous context hits inside document
    ;;              in succeeding lines, up to 5 first hits, with "more etc." if more.

    ;; If there are one or two exact matches, we display the exact matches
    ;; along with up to four total examples and a link to a page that will
    ;; show all other matches, if there are any.

    ;; Then we display more matches up to five total, and if there are
    ;; more than five, we say how many more exact and partial matches
    ;; there are, along with a link that will display a page showing
    ;; all the matches.

; JKM:  Are last two paragraphs accurate?

  (when (>= *jhelp-debug* 4)
	(print "JHelp:  (html) output being invoked:")
	(print title)
        (print search-words)
        (print "<OK" )
	)
    (html 
     (:html
       (:head (:title  (:princ-safe (formatn "Help: ~a" (get wb::*sessionid* :current-search-string))) )
  	((:link  rel "shortcut icon"  href "/ajax/favicon.ico"))
 	((:link  rel "stylesheet"      type "text/css"   href "/ajax/vpl.css" ))
  	((:script type "text/javascript"  src "/ajax/jquery-1.3.2.min.js"))
 	((:script type "text/javascript"  src "/ajax/json2.js"))
;;  (:script type='text/javascript' src='queue.js')
 ;; (:script type='text/javascript' src='ajax.js')
;;  (:script type='text/javascript' src='http.js')
;;  (:script type='text/javascript' src='vpl.js')
;;  (:script type='text/javascript' src='vpl-version.js')
;;  (:script type='text/javascript' src='sexp.js')
;;  (:script type='text/javascript' src='patches.js')
;;  !!! Note importantly that Help page comes up in very weird temp directory under user's name, 
;;      and so requires absolute path to .js files etc.
	((:script type "text/javascript"  src "/ajax/jhelp_window_Boxes.js"))
       )  ;; end of head
       ((:body :bgcolor "\#A0ffff")

              ((:div align "center")
                    ((:font size "5")(:b (:princ-safe title)))
		    "<br>Click a function to bring it into your workspace<br>Click a link to learn more<br><br>"
	      )

              ((:div style "position:absolute; right:0px; top:0px; z-index:10")
                    ((:img src "/ajax/images/lifesaver.png"   alt "" height "80" width "92" border "0"))
              )

           (when (>= *jhelp-debug* 8)
             (format T "~&~s results being formatted for HTML output, entering Table loop.~%" (length JDocs))
             )


        (if JDocs   ;;Did we get ANY outputs?


     (html (:table 


     (loop for tuplet in JDocs 
       for score = (car tuplet)
       for JDocID = (cdr tuplet)
       for JDoc  = (get-JDocument JDocID)           ;;put industrial safeties in here, in case we don't find it...
       for doc-type = (JDocument-doctype JDoc)
       for Document = (JDocument-thedoc JDoc )
       for JDoc-text = (JDocument-text JDoc) 
       with boxcount = local-box-stamp
       do

           (when (>= *jhelp-debug* 10)
             (format T "score: ~s; JDocID: ~s; doc-type: ~s~% " score JDocID doc-type)
             )


	;;  JPage and JPage-VPLSymbols
       (if (or (equal doc-type 'jpage) (equal doc-type 'jpage-symbol))


           ;; JPAGE, JPAGE-SYMBOX (Magic Box) VERSIONS:
	 (let* ( (url  (eval (JDocument-functor-url JDoc)))  )  ;;needs to be eval'd at run-time, apparently.
	 (html
	   ((:tr :valign "top")  


	       (when (>= *jhelp-debug* 1) (html (:td "[" (:princ-safe (formatn "~,2f" score)) "] &nbsp;&nbsp;&nbsp;" )       ))    ;;SCORE.  Only in Debugging.


              (unless (and *jhelp-dont-output-first-type-column* (< *jhelp-debug* 3))  (html  ;;putting in logic makes it forget the html context.
	       (:td   ((:a :href  url :target "_blank")    (:princ-safe (s+ "("   "HelpPage"  ")" )))   ;; May 29 '12 Added link.
		 "&nbsp;&nbsp;" 
	       ) ))

	       ; :target _blank is the html magic for invoking a separate page, not reloading current.
	       ((:td  :style "padding-right:35px" )  ;;((:a :href  url :target "_blank") 
		     ;; (:princ-safe (JPage-title Document) )  (:br)	;; May 29 '12 Printed title now deprecated when box exists.
                      ;)
		    (:nobr
		    
                    ((:div :style "float:left" 
                           id (s+ "help" (formatn "~a" boxcount))))           ; Glue for Box.  This will be cleverly replaced live at runtime by JS.        
		    (incf boxcount) (incf jh-boxstamp)

   ;;                 "&nbsp;"
                    ((:a :href  url :target "_blank")  ((:img src "/ajax/images/question-mark-24.gif"   alt "" height "22" width "22" border "0")))      ;; Question-mark Link coming after Box.   
                   )
                    "&nbsp;&nbsp;&nbsp;" 
                  
               )
	      
	       (:td	(if (equal doc-type 'jpage)
			(jhtml-for-elhai-JPage Document search-words JDoc-text)
		;else
			(jhtml-for-elhai-jpageVPLsymbol Document search-words JDoc-text) )
	       )

	  )  ; tr


	 ))  ;  html, let





	 ;else,   Old Help
         ;;  OLD FORMAT, 7 FLAVORS VERSIONS:
	 (let* ( (url  (docobj->url Document)) (label (name Document)) )
        (html
	 ((:tr :valign "top")  
	         (when (>= *jhelp-debug* 1) (html (:td "[" (:princ-safe (formatn "~,2f" score)) "] &nbsp;&nbsp;&nbsp;" )       ))

              (unless (and *jhelp-dont-output-first-type-column* (< *jhelp-debug* 3))  (html  ;;putting in logic makes it forget the html context.
		(:td (:princ-safe (s+ "(" 
                                       (if (or (equal doc-type 'jpage) (equal doc-type 'jpage-symbol)) 
                                           "HelpPage" (abbr-for-doc-item Document)) ")" )) ;HP not used here
 		  "&nbsp;&nbsp;" 
		)))

	       ; :target _blank is the html magic for invoking a separate page, not reloading current.
	       (:td ((:a :href  url :target "_blank") (:princ-safe label ))     ;;forgets what :princ-safe is inside (:a).  Die.
	    	  "&nbsp;&nbsp;&nbsp;" 
	       )

	      (:td     (if (equal doc-type 'documentation-file)
                    (jhtml-for-elhai-documentation-file Document search-words) ;;match)          ;; DOCUMENTATION-FILE Version
		;else
                  (if (equal doc-type 'function-documentation)
                      ( jhtml-for-elhai-function-doc  Document search-words JDoc-text )
                    ;else
		 (jhtml-for-elhai-everything-else Document search-words JDoc-text ) ))           ;;  EVERYTHING-ELSE  Version
	      )
	  )


	 ))  ;   html, let

	) ;if




       ) ; loop


#|
    (cond
       ((zerop n-exact) nil)
       ((= 1 n-exact)
        (display-match-with-examples first-match 2)
        (setq n-displayed 1)
        (pop remaining-matches))
       ((= 2 n-exact)
        (let ((max (if (and (match-examples first-match)
                            (match-examples second-match)) 
                       2 2)))
          (display-match-with-examples first-match max)
          (display-match-with-examples second-match max)
          (pop remaining-matches) (pop remaining-matches))
        (setq n-displayed 2))
       ((> 2 n-exact) 
        (loop for j from 1 to 5 
              for match in exact-matches 
              do
              (display-match-with-examples match 0)
              (incf n-displayed)
              (pop remaining-matches)
              )))
    
      (when (< n-displayed 5) 
        (loop for j from (1+ n-displayed) to 5
              for match in remaining-matches
              do 
              (display-match-with-examples match 0)
              (incf n-displayed)
              (pop remaining-matches)
              ))
    
      (when remaining-matches 
        (link-for-rest-of-matches 
         (nthcdr 5 exact-matches) 
         remaining-matches
         (length all-matches)))
|#



      ))  ;table

          ;ELSE, no JDocs whatsoever!
          (progn
            (html (:princ-safe (formatn "No results found."))
            ))
          );  if any JDocs

      )  ;body


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  This is the end of the HTML proper.
;;  Now we create and include
;;  the magic JavaScript
;;  for operating the injector, to put the boxes up on the screen of the search results.
;;  At present this only runs on the logical forms for jpages and jpage-symbols.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


      ((:script type "text/javascript")

"var jbml;
var parsed;
"
#|
"jbml = \"{\\\"id\\\":427,\\\"type\\\":\\\"jbml-delete\\\",\\\"modifiers\\\":{\\\"jbml-outdent\\\":true},\\\"children\\\":[{\\\"id\\\":5013,\\\"type\\\":\\\"jbml-main-box-menu\\\",\\\"title\\\":\\\"\\\",\\\"entries\\\":[{\\\"id\\\":549,\\\"type\\\":\\\"jbml-menu-entry\\\",\\\"title\\\":\\\"Help\\\"},{\\\"id\\\":122,\\\"type\\\":\\\"jbml-menu-entry\\\",\\\"title\\\":\\\"Collapse\\\"},{\\\"id\\\":136,\\\"type\\\":\\\"jbml-menu-entry\\\",\\\"title\\\":\\\"Execute\\\"},{\\\"id\\\":143,\\\"type\\\":\\\"jbml-menu-entry\\\",\\\"title\\\":\\\"Copy\\\"},{\\\"id\\\":150,\\\"type\\\":\\\"jbml-menu-entry\\\",\\\"title\\\":\\\"Cut\\\"},{\\\"id\\\":164,\\\"type\\\":\\\"jbml-menu-entry\\\",\\\"title\\\":\\\"Insert\\\"},{\\\"id\\\":535,\\\"type\\\":\\\"jbml-menu-entry\\\",\\\"title\\\":\\\"Surround with\\\"},{\\\"id\\\":185,\\\"type\\\":\\\"jbml-menu-entry\\\",\\\"title\\\":\\\"Show code\\\"},{\\\"id\\\":290,\\\"type\\\":\\\"jbml-menu-entry\\\",\\\"title\\\":\\\"Results\\\"},{\\\"id\\\":171,\\\"type\\\":\\\"jbml-menu-entry\\\",\\\"title\\\":\\\"Cut\\\\\\/Insert\\\"},{\\\"id\\\":269,\\\"type\\\":\\\"jbml-menu-entry\\\",\\\"title\\\":\\\"Monitor\\\"}]},{\\\"type\\\":\\\"text\\\",\\\"value\\\":\\\"ABS\\\",\\\"modifiers\\\":{\\\"jbml-b\\\":true}},{\\\"id\\\":441,\\\"type\\\":\\\"jbml-hole\\\",\\\"modifiers\\\":{\\\"jbml-outdent\\\":true,\\\"jbml-background-color\\\":\\\"#FFFFFF\\\"},\\\"children\\\":[{\\\"id\\\":5014,\\\"type\\\":\\\"jbml-main-box-menu\\\",\\\"title\\\":\\\"\\\",\\\"entries\\\":[{\\\"id\\\":549,\\\"type\\\":\\\"jbml-menu-entry\\\",\\\"title\\\":\\\"Help\\\"},{\\\"id\\\":157,\\\"type\\\":\\\"jbml-menu-entry\\\",\\\"title\\\":\\\"Paste\\\"},{\\\"id\\\":535,\\\"type\\\":\\\"jbml-menu-entry\\\",\\\"title\\\":\\\"Surround with\\\"},{\\\"id\\\":206,\\\"type\\\":\\\"jbml-menu-entry\\\",\\\"title\\\":\\\"Multiline input\\\"},{\\\"id\\\":542,\\\"type\\\":\\\"jbml-menu-entry\\\",\\\"title\\\":\\\"Unsurround\\\"}]},{\\\"type\\\":\\\"text\\\",\\\"value\\\":\\\"number\\\",\\\"modifiers\\\":{\\\"jbml-color\\\":\\\"#B7410E\\\",\\\"jbml-i\\\":true}}]}]}\";
"

"
parsed  = JSON.parse( jbml );
if( ! parsed ) alert(\"Unable to PARSE! Bad jbml: \" + jbml );
attachJbmlBoxes (parsed, document.getElementById(\"help1\"), \"help1\", 3921, \""  
	(:princ-safe (formatn  "~a" wb::*sessionid*))  "\");
"
|#
#|
"
jbml = \"{\\\"id\\\":833,\\\"type\\\":\\\"jbml-clear\\\",\\\"modifiers\\\":{\\\"jbml-background-color\\\":\\\"#ffc000\\\",\\\"jbml-outdent\\\":true},\\\"children\\\":[{\\\"id\\\":5036,\\\"type\\\":\\\"jbml-main-box-menu\\\",\\\"title\\\":\\\"\\\",\\\"entries\\\":[{\\\"id\\\":549,\\\"type\\\":\\\"jbml-menu-entry\\\",\\\"title\\\":\\\"Help\\\"},{\\\"id\\\":122,\\\"type\\\":\\\"jbml-menu-entry\\\",\\\"title\\\":\\\"Collapse\\\"},{\\\"id\\\":136,\\\"type\\\":\\\"jbml-menu-entry\\\",\\\"title\\\":\\\"Execute\\\"},{\\\"id\\\":143,\\\"type\\\":\\\"jbml-menu-entry\\\",\\\"title\\\":\\\"Copy\\\"},{\\\"id\\\":150,\\\"type\\\":\\\"jbml-menu-entry\\\",\\\"title\\\":\\\"Cut\\\"},{\\\"id\\\":164,\\\"type\\\":\\\"jbml-menu-entry\\\",\\\"title\\\":\\\"Insert\\\"},{\\\"id\\\":535,\\\"type\\\":\\\"jbml-menu-entry\\\",\\\"title\\\":\\\"Surround with\\\"},{\\\"id\\\":185,\\\"type\\\":\\\"jbml-menu-entry\\\",\\\"title\\\":\\\"Show code\\\"},{\\\"id\\\":290,\\\"type\\\":\\\"jbml-menu-entry\\\",\\\"title\\\":\\\"Results\\\"},{\\\"id\\\":171,\\\"type\\\":\\\"jbml-menu-entry\\\",\\\"title\\\":\\\"Cut\\\\\\/Insert\\\"},{\\\"id\\\":542,\\\"type\\\":\\\"jbml-menu-entry\\\",\\\"title\\\":\\\"Unsurround\\\"},{\\\"id\\\":269,\\\"type\\\":\\\"jbml-menu-entry\\\",\\\"title\\\":\\\"Monitor\\\"}]},{\\\"type\\\":\\\"text\\\",\\\"value\\\":\\\"ABS\\\",\\\"modifiers\\\":{\\\"jbml-b\\\":true}},{\\\"id\\\":1134,\\\"type\\\":\\\"jbml-clear\\\",\\\"modifiers\\\":{\\\"jbml-outdent\\\":true,\\\"jbml-background-color\\\":\\\"#eeeeee\\\"},\\\"children\\\":[{\\\"id\\\":5037,\\\"type\\\":\\\"jbml-main-box-menu\\\",\\\"title\\\":\\\"\\\",\\\"entries\\\":[{\\\"id\\\":549,\\\"type\\\":\\\"jbml-menu-entry\\\",\\\"title\\\":\\\"Help\\\"},{\\\"id\\\":136,\\\"type\\\":\\\"jbml-menu-entry\\\",\\\"title\\\":\\\"Execute\\\"},{\\\"id\\\":143,\\\"type\\\":\\\"jbml-menu-entry\\\",\\\"title\\\":\\\"Copy\\\"},{\\\"id\\\":150,\\\"type\\\":\\\"jbml-menu-entry\\\",\\\"title\\\":\\\"Cut\\\"},{\\\"id\\\":164,\\\"type\\\":\\\"jbml-menu-entry\\\",\\\"title\\\":\\\"Insert\\\"},{\\\"id\\\":535,\\\"type\\\":\\\"jbml-menu-entry\\\",\\\"title\\\":\\\"Surround with\\\"},{\\\"id\\\":171,\\\"type\\\":\\\"jbml-menu-entry\\\",\\\"title\\\":\\\"Cut\\\\\\/Insert\\\"},{\\\"id\\\":101,\\\"type\\\":\\\"jbml-menu-entry\\\",\\\"title\\\":\\\"Edit\\\"},{\\\"id\\\":542,\\\"type\\\":\\\"jbml-menu-entry\\\",\\\"title\\\":\\\"Unsurround\\\"}]},{\\\"type\\\":\\\"text\\\",\\\"value\\\":\\\"42\\\",\\\"modifiers\\\":{}}]}]}\";
"
|#

     (loop for tuplet in JDocs 
;      for score = (car tuplet)
       for JDocID = (cdr tuplet)
       for JDoc  = (get-JDocument JDocID)           ;;put industrial safeties in here, in case we don't find it...
       for doc-type = (JDocument-doctype JDoc)
       for Document = (JDocument-thedoc JDoc )
       with boxcount = local-box-stamp
       do

;;Note:  (:princ-safe carefully transforms " into &quot;, thus breaking JS.  Don't use it inside script.
       (when (or (equal doc-type 'jpage) (equal doc-type 'jpage-symbol))

	 (let* (( lform  (JPage-logical-form Document))
		)
	 (html
	  "

          "
	  "jbml = "   
	  ;; (html macro refuses to acknowledge this generates a string, unless rub nose in it.  
	  ;; Unclear why (:princ is required but it is.  (format nil might be optional.
	  ;; unclear why ~s works but it does, so we'll take it.  Must need to double- quote the slashes, I guess.
	  ;; ...Compiler is barfing that both gigamonkeys and nvpl are undefined packages.  Gyahhh.  Here's how to fix it.
	  (:princ (format nil "~s" 
			  (forward-package-funcall :com.gigamonkeys.json :json 
						   (forward-package-funcall :nvpl :boxes->json lform))))
	  ";" 
;;(format T "~%INCLUDING |~s|" (com.gigamonkeys.json::json (nvpl::boxes->json lform)))
	  (:princ (s+ 
"

parsed  = JSON.parse( jbml );
if( ! parsed ) alert(\"Unable to PARSE! Bad jbml: \" + jbml );
attachJbmlBoxes (parsed, document.getElementById(\"help"
  (formatn "~a" boxcount)
"\"), \"help"
  (formatn "~a" boxcount)
"\", "
  (formatn "~a" JDocID)
", \""   ))
	(:princ-safe (formatn  "~a" wb::*sessionid*))  (:princ  "\");
"  ) ;CR inside string, end of mess


;;;;;		    ((:div id (s+ "help" (formatn "~a" boxcount))))
		    (incf boxcount)

	 );html
       ) ; let
     ) ; when


    ) ; loop


       ) ; end :script javascript


))))




;;;;;;;;;;;;;;;;;;;;master helper

(defun find-first-search-term (matchline search-words)
;; Finds first matching word in matchline, given a set of search-words.
;; Returns a LIST of ( #,  best-term-lenth,  matching-search-term )
;;
          (loop for search-term in search-words 
                with min = 1000000
                with best-term-length = 0
                with first-match-search-term = ""
              for foundit = 
                (search search-term matchline :test 'string-equal)
              when (and foundit (< foundit min)) do
                        (setq min foundit)
                        (setq best-term-length (length search-term))
                        (setq first-match-search-term search-term)
          finally (if (< min 1000000)
                      (return (list min best-term-length first-match-search-term))
                    ;else
                      (return (list nil 0 ""))  ;; didn't find it.
                    ))
)






#|  ;; Old version. Replaced with next one down.
(defun jstring-for-text-match-line (matchline search-words &optional (context 20))
  (let* ((startpos (first (find-first-search-term matchline search-words)))
         (endpos nil)
         (linelen nil))
    (cond
     ((<= startpos context) nil)
     (t (setq matchline (s+ "..." (subseq matchline (- startpos context)))))
    )
    (setq startpos (search search-term matchline :test 'string-equal))
    (setq endpos (+ startpos (length search-term)))
    (setq linelen (length matchline))
    (cond
     ((>= (+ endpos context) linelen) nil)
     (t (setq matchline (s+ (subseq matchline 0 (+ endpos context)) "...")))
    )
    matchline
    ;; (s+ "'" matchline "'")    ;; ??? ...and what if it had single quotes inside it anyway??
 )
)
|#

(defun jstring-for-text-match-line (matchline search-words &optional (context 20))  ;;Context never supplied, always defaults.
  (let* ((startpos (first (find-first-search-term matchline search-words)))
         )
    (when (and startpos (> startpos context))
     (setq matchline (s+ "..." (subseq matchline (- startpos context))))
    )
    matchline
 )
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Helper Functions;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Used to have a "match" arg, but now it's deprecated throughout the stack.
;; USAGE DEPRECATED
(defun jemit-descriptor-and-docobj-link-tds (docobj)
  (html
;;   (:td (:tt (:princ-safe (string-capitalize (abbr-for-help-item match)))))
;;   :newline
   (:td (:tt 
;;"&nbsp;&nbsp;" 
         (html-for-elhai-doc-object-reference docobj)))
   :newline
   ))

;; Used to have a "match" arg, but now it's deprecated throughout the stack.
;; USAGE DEPRECATED
(defun jemit-descriptor-and-docobj-link-tds-for-docfile (docobj)
  (let ((source-url (source-url-for-filedoc docobj)))
    (multiple-value-bind (url label)      ; Stick into vars "url" and "label"
        (docobj->url&label docobj nil)    ; the results of evaluating this multiple-value-returning function
      (declare (ignore url))              ; and then use the results to evaluate this paragraph series of statements.
      (html
;;;       (:td (:tt (:princ-safe (string-capitalize (abbr-for-help-item match)))))   ;; Don't.  No Match now.
;;;       :newline
       (:td (:tt 
;;"&nbsp;&nbsp;" 
             ((:a :href source-url :target "_blank") (:princ-safe label))
             ))
       :newline
       ))))





;; Deals with a list of possible search words.
#| Jul 07'12 JKM:  Apparently this originally was supposed to only take a "single matchline"
as input, then give out a trimmed version of the line around it.
But it is mostly calling jstring-for-text-match-line, which duplicates its trimming function,
but is currently more primitive and has not been refactored to handle multiple search-words yet.
Trying experiment of just simply handing in the entire paragraph to this beast.
I believe it's strong enough to handle it.  Let's not reduplicate efforts again redundantly. 

Does not search multiple lines, returns first line only.
|#
(defun jemit-limited-match-with-search-term-bolded 
;; THIRD LEVEL  SINGLE PARAGRAPH,  also FIFTH LEVEL under File Objects, and more??
;; 
       (matchline search-words &key (color "green") (limit 30))
  (let* ((pos-termlength
          (find-first-search-term matchline search-words)
	  )
	(pos        (first pos-termlength))
	(termlength (second pos-termlength))
  ;     (first-match-search-term (third pos-termlength))
       )
    (if (null pos)
        (html (:princ-safe (ellipsis-string matchline limit)))  ;Cuts off END, glues on "...", but only if shorter than limit.
      ;else
      (let* ((before (subseq matchline 0 pos))
             (szbefore (length before))
             (endpos (+ pos termlength ))
             (term (subseq matchline pos endpos))
             (szterm (length term))
             (after (subseq matchline endpos))
             (szafter (length after))
             (szbefore&term (+ szbefore szterm)))
        (cond
         ((>= szbefore limit) 
          (html (:princ-safe (ellipsis-string before limit))))
         ((> limit (+ szbefore&term 3))
          (html 
           (:princ-safe before)
           ((:font :color color) (:princ-safe term)))
          (when (plusp szafter) 
            (jemit-limited-match-with-search-term-bolded   ;; RECURSIVE CALL.
             after search-words :color color :limit (- limit szbefore szterm)))
          )
         (t 
          (let* ((amount-of-term-to-show (- limit szbefore 3)))
            (if (not (plusp amount-of-term-to-show))
                (html (:princ-safe (ellipsis-string matchline limit)))
              (let ((tterm (subseq term 0 amount-of-term-to-show)))
                (html 
                 (:princ-safe before)
                 (:b ((:font :color color) (:princ-safe tterm)))
                 (:princ-safe "...")
                 )))))))))
)





;; Deals with a list of possible search words. Only one line is output.
(defun jemit-matchline-td (matchline search-words)
;;  Puts in a <td> that will be used as an entry in a row for multi-row matches.
;;  Handles ellipses at the beginning.
;;
  (html
   (:td
;;    (:tt   ;; ugly!
;;     "&nbsp;&nbsp;"
     (jemit-limited-match-with-search-term-bolded  ;;???Why the double call?? ...needed to put ellipses at beginning.
        (jstring-for-text-match-line  matchline search-words)    ;; ... at beginning.
      ;;            matchline                        ;;no redundance.
      search-words :color "red" :limit 65
      ))))
;;)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Extra stuff:  Context;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod html-for-jhelp-formatted-search-result-docobj ((docobj t) search-words )
;;  Deprecated method for old help objects.  Not used at present.
;;

      (html
       (:table
       ((:tr :valign "top") 
 ;;       (jemit-descriptor-and-docobj-link-tds docobj)   ;; name/hotlink.  We've done our own already.
        (:td
         (:tt
;;          "&nbsp;&nbsp;"
         (if  (help:docstring docobj) 

               (jemit-limited-match-with-search-term-bolded      ;;Only one line, I think
                     (help:docstring docobj)   search-words  :color "red" :limit 65)                 
	;; else
            ;;(if (help:symbol-doc docobj)   ;;This part might only work for symbols, but let's try it.
             (let ((name (help:name docobj)))  ;; if this crashes modify it.  Not sure this will work all the time.
               (html
                (:princ-safe 
                 (if (and (symbolp   name )                  ;; name still uncertain.  Consider trimming clause out.
                          (eq (symbol-package  name ) 
                              (find-package :cl)))
                     "Common Lisp symbol" 
                   ""
                   )))))))
        ))))



(defun search-multi (search-words matchline)
;; Multi-word search.. This version only effectively returns a boolean.
          (loop for search-term in search-words 
              for foundit = 
                (search search-term matchline :test 'string-equal)
              when foundit do
                (return foundit )
          finally (return nil)  ;; didn't find it.
	 )
)

(defmethod html-for-jhelp-formatted-search-result-docobj 
           ((docobj documentation-file)   search-words ) 
;;
;;  THIRD-LEVEL MAIN PRINTOUT FOR FILE OBJECTS, MULTI-LINE
;;
;; This one is medium-low-level for output, when docobj is a long documentation file contents.
;; It chops up the lines, highlights matches, and puts in ellipses, coming back with multiple hits.
;;
;;  This one is hard-wired to take input from the file's text file.
;;  Would need another one to do general-purpose multi-line listing.
;;

(html (:table             ;; HERE'S THE TABLE CALL.  LOTS OF ROWS TO BE PUT IN.

      (if (text-file-match-exists? docobj)
          (with-open-file 
              (p (or (associated-text-file docobj) (source-file docobj))
                 :direction :input)
            (loop 
             while t 
             with match-count = 0
             do
             (let ((line (read-line p nil nil)))
               (when (null line) 

                 (when (>= match-count 6)
                   (html 
                    ((:tr :valign "top") 
 ;                    (:td)
 ;                    (:td)
                     (:td 
                      (:i "&nbsp;&nbsp;&nbsp;"
                       (:princ-safe
                        (formatn "(and ~D further lines with matches)" 
                                 (- match-count 5)
                                 )))))))

                 (return nil))
               (when 
		(search-multi search-words line) ; :test 'string-equal)                 

                 (incf match-count)
                 (cond 

                  ((= 1 match-count)
                   (html
                    ((:tr :valign "top") 
 ;;;;                   (jemit-descriptor-and-docobj-link-tds-for-docfile docobj)     ; Name / hotlink.  We've done ours already.

                     (:td
       (:b               (:tt
 ;;                      "&nbsp;&nbsp;"
                       (jemit-limited-match-with-search-term-bolded       ;;Only one line, I think.
                        ( jstring-for-text-match-line            line search-words)              ;; Why the dual call?  ...puts in ellipses at beginning.
                        ;;      line                      ;; no redundancy.
                        search-words :color "red" :limit 65
                        ))))))    )

                  ((> match-count 5) nil)

                  (t 
                   (html 
                    ((:tr :valign "top") 
;;                     (:td) (:td) 
                     (jemit-matchline-td line search-words))
                    ))
                  )))))

           ;ELSE NO Open File

        (html 
         ((:tr :valign "top") 
;         (jemit-descriptor-and-docobj-link-tds-for-docfile  docobj)       ; Name / hotlink.  We've done ours already.
          ;; Only emit a matchline if in fact the doc matches.  
          (vif (doc (help:docstring docobj))
               (if   (search-multi search-words doc) ; :test 'string-equal)         

                   (jemit-matchline-td doc search-words)
                 (html (:td))
                 )
               (html (:td))
               )))  ; html
        ))
        ))
                     
     
