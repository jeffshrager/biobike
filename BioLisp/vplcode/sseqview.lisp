;;; -*- mode: Lisp; Syntax: Common-Lisp; Package: wb; -*-

(in-package :wb)

;;; +=========================================================================+
;;; | Copyright (c) 2006 JP Massar                                            |
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

;; Author: JP Massar.  

(defun clss ()
  (bio::cl "biol:vpl;sseqview-defs.lisp" t)
  (bio::cl "biol:vpl;sseqview.lisp" t)
  )

(bbi::define-function bbi::sequence-viewer
  keyword (organism contig from to rows columns search)
  body 
  (bbi::sequence-viewer-aux 
   :organism organism
   :contig contig
   :from from
   :to to
   :rows rows
   :columns columns
   :search search
   ))

(defun sequence-viewer (&key organism contig from to rows columns search)
  (bbi::sequence-viewer-aux 
   :organism organism
   :contig contig
   :from from
   :to to
   :rows rows
   :columns columns
   :search search
   ))

(DEFUN bbi::sequence-viewer-aux
  (&key organism contig from to rows columns search)
  (make-svs 
   :org organism
   :contig contig
   :from from
   :to to
   :rows rows
   :columns columns
   :search search
   ))

(publish 
 :path wb::*sseqview-url*
 :content-type cl-user::*html-publish-content-type*
 :function
 (lambda (req ent)
   (let* ((input (request-query req))
          (package-name (url-parameter-value :pkg input))
          (package-symbol (keywordize package-name))
          )
     (setq *input* input)
     (wb::execute-with-standard-weblistener-environment
      req ent package-symbol
      (lambda () 
        (let* ((*current-svs* nil)
               (*current-linemap* nil))
          (sseqview-main input)
          ))))))

(defun test-sseqview (input sessionid)
  (with-protected-globals-bound sessionid 
    (with-output-to-string (s)
      (let ((*html-stream* s))
        (let ((*current-svs* nil))
          (sseqview-main input)
          )))))

(defun entire-search-mechanism? (my-svs)
  (or (svs-search-entire-next/prev my-svs)
      (and (has-content? (svs-search my-svs)) 
           (null (svs-search-display? my-svs))
           )))

(defun sseqview-main (input)
  (parse-sseqview-request-and-save input)
  (let ((my-svs *current-svs*))
    (cond 
     ((svs-errors my-svs) (display-sseqview-results))
     ((entire-search-mechanism? my-svs) 
      (sseqview-entire-search)
      (display-sseqview-results))
     (t
      (when (svs-contig *current-svs*) (compute-sseqview-results))
      (display-sseqview-results)
      ))))

(defun default-default-svs ()
  (make-svs 
   :from 1 :to *default-nucleotides-to-display*
   :extent *default-nucleotides-to-display*
   :rows *default-sseqview-rows-to-display* 
   :columns *default-sseqview-columns-to-display*
   :gene-names t
   ))

(defun parse-sseqview-request-and-save (input)

  (unless (get wb::*sessionid* :svs)
    (setf (get wb::*sessionid* :svs) (default-default-svs)))

  (setq *current-svs* (get wb::*sessionid* :svs))

  (let ((org (url-parameter-value :org input))
        (contig (url-parameter-value :contig input))
        (goto (url-parameter-value :goto input))
        (search (url-parameter-value :search input))
        (next (url-parameter-value :next input))
        (prev (url-parameter-value :prev input))
        (start (url-parameter-value :start input))
        (end (url-parameter-value :end input))
        (match-prev (url-parameter-value :matchprev input))
        (match-next (url-parameter-value :matchnext input))
        (vpl (url-parameter-value :vpl input))
        (search-display-only? (url-parameter-value :searchdisplay input))
		(inverted (url-parameter-value :invert input)) ; ************ modified from invert?
        (translate? (url-parameter-value :translate input)) ; **********
		(double-stranded? (url-parameter-value :double-stranded input)) ; ************
        ;; (rows (url-parameter-value :rows input))
        ;; (columns (url-parameter-value :columns input))
        ;; (gene-names (url-parameter-value :gene-names input))
        ;; (font-size (url-parameter-value :font-size input))
        (contig-changed? nil)
        (old-contig nil)
        (search-string-changed? nil)
        (old-search-string nil)
        (my-svs *current-svs*))
    (when vpl 
      (setf (get wb::*sessionid* :svs) (default-default-svs))
      (setq *current-svs* (get wb::*sessionid* :svs))
      (setq my-svs *current-svs*))

    ;; save these so we can tell whether they changed
    (setq old-contig (svs-contig my-svs))
    (setq old-search-string (svs-search my-svs))
    
    (setf (svs-errors my-svs) nil)
    (setf (svs-notes my-svs) nil)
    (setf (svs-search my-svs) nil)
    (setf (svs-search-note my-svs) nil)
    (setf (svs-goto my-svs) goto)
    (setf (svs-search-display? my-svs) search-display-only?)
	(setf (svs-invert? my-svs) inverted) ;t) ; ***************
	(setf (svs-translate? my-svs) translate?) ; ************
    (setf (svs-double-stranded? my-svs) double-stranded?) ; ***************
    (setf (svs-search-entire-next/prev my-svs) nil)

    (block exit
      (labels 
          ((oops (s &rest args)
             (push (apply 'format nil s args) (svs-errors my-svs))
             (return-from exit my-svs)
             )
           (verify-org-string (s)
             (let ((org (org-string-to-org-frame org 'organism-predicate)))
               (unless org 
                 (setf (svs-org my-svs) nil)
                 (if (null (bio::loaded-organisms))
                     (oops "There are no organisms loaded!")
                   (progn
                     (setf (svs-contig my-svs) nil)
                     (oops "No organism named or nicknamed ~S found!" s)
                     )))
               (unless (#^organism-loaded? org)
                 (setf (svs-org my-svs) nil)
                 (oops "The organism ~A is not loaded!" (#^fname org))
                 )
               org
               ))
           (verify-contig-string (s org? org)
             (let ((contig (contig-string-to-contig-frame s org? org)))
               (unless contig 
                 (if org? 
                     (progn
                       (setq contig (first (#^contiguous-sequences org)))
                       (if (null contig) 
                           (oops "Organism ~A has no contiguous sequences!"
                                 org)
                         (push
                          (formatn "Contig ~A not found, showing ~A instead"
                                   s (#^fname contig))
                          (svs-notes my-svs)
                          )))
                   (progn
                     (setf (svs-contig my-svs) nil)
                     (if (null (bio::loaded-organisms))
                         (oops "There are no organisms loaded!")
                       (oops 
                        "Could not find contiguous sequence named ~S" s
                        )))))
               contig
               )))

        ;; handle when the user clicks the Start, Next, End or Prev links

        (when (new-process-start-prev-next-end start prev next end)
          (return-from exit my-svs))

        ;; handle when the user clicks the Prev match or Next match links

        (when (or match-prev match-next)
          (cond
           (match-prev (setf (svs-search-entire-next/prev my-svs) :prev))
           (match-next (setf (svs-search-entire-next/prev my-svs) :next))
           )
          (return-from exit my-svs)
          )

        (let ((hcorg (has-content? org))
              (hccontig (has-content? contig))
              (old-org (svs-org my-svs))
              )
          (cond
           ((and hcorg hccontig) 
            (setf (svs-org my-svs) (verify-org-string org))
            (setf (svs-contig my-svs) 
                  (verify-contig-string contig t (svs-org my-svs))
                  ))
           (hcorg 
            (setf (svs-org my-svs) (verify-org-string org))
            (setf (svs-contig my-svs)
                  (first (#^contiguous-sequences (svs-org my-svs)))
                  ))
           (hccontig
            (setf (svs-contig my-svs) (verify-contig-string contig nil nil))
            (setf (svs-org my-svs) (#^organism (svs-contig my-svs)))
            )
           (t 
            (setf (svs-org my-svs) nil)
            (setf (svs-contig my-svs) nil)
            (if (or (has-content? goto) (has-content? search))
                (oops "You did not provide either an organism or a contig!")
              (oops "")
              )))
          
          ;; Reset 'entire contig' state if user changed contig
          (setq contig-changed? (not (eq old-contig (svs-contig my-svs))))
          (when contig-changed?
            (setf (svs-match-locations my-svs) nil)
            (setf (svs-match-counter my-svs) nil))

          ;; When the user has changed the organism or contig, 
          ;; and has not provided new goto values, set the
          ;; from, to, and extent to the defaults.  
          (when (or (not (eq old-org (svs-org my-svs))) contig-changed?)
            (when (not (has-content? goto))
              (set-from-to-extent-defaults my-svs))
            ))

        (when (has-content? goto)
          
          (let ((from-to-and-extent 
                 (decoded-pattern->from-and-to 
                  (parse-goto-string goto) (svs-contig my-svs) my-svs
                  )))
            
            (when from-to-and-extent
              
              (setf (svs-from my-svs) (first from-to-and-extent))
              (setf (svs-to my-svs) (second from-to-and-extent))
              (setf (svs-extent my-svs) (third from-to-and-extent))
              

              ;; make sure FROM, TO, and EXTENT are limited by the number
              ;; of base pairs to display and the size of the contig
              (let ((clen (#^sequence-length (svs-contig my-svs)))
                    (max-bp *max-basepairs-to-display*))
                (when (> (svs-extent my-svs) max-bp)
                  (push 
                   (format 
                    nil "System will only display ~A base pairs at one time"
                    max-bp)
                   (svs-notes my-svs)
                   )
                  (setf (svs-extent my-svs) max-bp)
                  (setf (svs-to my-svs) (circ+ (svs-from my-svs) max-bp clen))
                  )
                (when (> (svs-extent my-svs) clen)
                  (setf (svs-extent my-svs) clen)
                  (setf (svs-to my-svs)
                        (circ+ (svs-from my-svs) (1- clen) clen)
                        )))
              
              )))

        (when (and (svs-contig my-svs) (has-content? search))
          ;; remove any surrounding whitespace and ignore quotes
          (setf (svs-search my-svs) 
                (remove-if 
                 (lambda (ch) (find ch "'\""))
                 (string-trim *whitespace* search)
                 )))

        ;; Reset 'entire contig' state if user changed search string 
        (setq 
         search-string-changed? 
         (let ((new-search-string (svs-search my-svs)))
           (or (and (stringp new-search-string) (null old-search-string))
               (and (stringp new-search-string) 
                    (stringp old-search-string)
                    (not (string-equal new-search-string old-search-string))
                    ))))
        (when search-string-changed?
           (setf (svs-match-locations my-svs) nil)
           (setf (svs-match-counter my-svs) nil)
           )

        (if search-display-only? 
            (when (null (svs-search my-svs))
              (oops "No search pattern specified!"))
          (when (svs-search my-svs)
            (setf (svs-match-counter my-svs) 0)
            (setf (svs-match-string my-svs) (svs-search my-svs))
            ))

        #||
        (when (has-content? rows)
          (setf (svs-rows my-svs) (read-from-string rows)))
        (when (has-content? columns)
          (setf (svs-columns my-svs) (read-from-string columns)))
        ;; need font size
        (when gene-names 
          (when (not (zerop (length gene-names)))
            (setf (svs-gene-names my-svs) 
                  (case (char gene-names 0)
                    ((#\t #\T) t)
                    ((#\n #\N #\f #\F) nil)
                    ))))

||#
        
        my-svs

        ))))

(defun set-from-to-extent-defaults (my-svs)
  (let* ((clen (#^sequence-length (svs-contig my-svs)))
         (maxlen (min clen *default-nucleotides-to-display*)))
    (setf (svs-from my-svs) 1)
    (setf (svs-to my-svs) maxlen)
    (setf (svs-extent my-svs) maxlen)
    ))

(defun display-sseqview-results ()
  (let* ((my-svs *current-svs*))
    (html
     (:princ *transitional-html-doctype-header*)
     :newline
     (webpage-style-html cl-user:*ai*)
     :newline
     (:head 
      (:title 
       (:princ-safe
        (s+ (machine-name-letter-for-title) " Biobike sequence viewer")))
      (:princ (create-sseqview-css-goo))
      :newline
      (:princ *sseqview-js-goo*)
      :newline
      )
     (:body
      ((:div 
        :style "position:absolute; top:0px; bottom:0px; left:5px; right:2px;")
       :newline
       :p
       (html-for-sseqview-request my-svs)
       )))))

(defun html-for-sseqview-request (my-svs)
  (html
   ((:div :id "colossal" :style colossal-style)
    ((:div :id "topstuff" :style topstuff-style)
     (html-for-sseqview-top-panel my-svs)
     )
    :newline
    ((:div :id "bottom1" :style bottom1-style)
     ((:div :id "bottom2" :style bottom2-style)
      ((:div :id "bottom3" :style bottom3-style)
       ((:div :id "bottommain" :style bottommain-style)
        (if (svs-errors my-svs)
            (loop for estring in (svs-errors my-svs)
                  do
                  (html 
                   :br
                   (:h3 ((:font :color "red") (:princ-safe estring) :br))
                   ))
          (when (svs-contig my-svs)
            (new-display-sseqview-data my-svs)
            ))
        ))))
    :newline
    )))

(defun html-for-sseqview-top-panel (my-svs)
  (flet ((nsfn (x) 
           (cond
            ((null x) "")
            ((frames::isframe? x) (#^fname x))
            (t x))))
    (html
     ((:div :style 
       #.(one-string
          "padding: 10px 0px 0px 0px; "
          "margin: 0px; "
          "width: 100%; "
          "background-image: url(/weblistenerdocs/skybg.jpg);"
          ))
      (html-for-title-with-org-and-contig my-svs)
      ;; (webpage-toplinks cl-user:*ai* :listener? t)
      ((:form :name "sseqview-params" :method "get"
        :action "sseqview-url")
       :newline
       (:table
        (:tr
          ((:a :href (make-sseqview-orgs-and-contigs-url) ; *********
         ((:td :align "right" :vertical-align "bottom")   ; *********
            :style "text-decoration:none")                
           "Organism:")
          )
         (:td
          ((:input :type "text" :id "org" :name "org"
            :value (nsfn (svs-org my-svs)))))
         :newline
         ;; xxx

         ((:td :vertical-align "bottom")
          (html-for-current-organisms-contig-menu (svs-org my-svs))
          )

         (:td
          ((:input :type "text" 
            :name "contig" :id "contig" :value (nsfn (svs-contig my-svs)))))
         :newline

         (:td "&nbsp;&nbsp;" 
          ((:a :title 
            "Enter a position or FROM,TO or POS+EXT or POS-EXT or GENE")
           ((:img :src "/weblistenerdocs/question-mark3.jpg"
             :height "13px" :width "13px")))
          "&nbsp;"
          ((:a :title 
            "POS or FROM,TO or POS+EXT or POS-EXT or GENE"
            ) "Go to: "
           )
          ((:input :type "text" 
            :name "goto" :id "goto" :value (nsfn (svs-goto my-svs)))))
         :newline
         (:td
          ((:input :type "submit" :name "submit" :value "Go")))  ; *********??????
         )
       
        (:tr

         ((:td :align "right") 
          ((:a :title 
            "Enter a nucleotide sequence or regular expression pattern")
           ((:img :src "weblistenerdocs/question-mark3.jpg"
             :height "13px" :width "13px"))
           "&nbsp;"
           "Search: "))
         ((:td :colspan "3" :align "right")
          ((:input :type "text" :size "58" 
            :name "search" :value (nsfn (svs-search my-svs)))))
         (:td 
          ((:input :type "checkbox" :name "searchdisplay"
            :value "on" 
            )
           "Display only?&nbsp;" 
           ))
           ;; ************ VVVVVVVVVVVVVV **************
		   ;;- checkbox to invert sequences and display double strands
		   (:td 
          ((:input :type "checkbox" :name "invert"
            :value "on" 
            )
           "Invert?&nbsp;" 
           ))
           #|
		   (:td 
          ((:input :type "checkbox" :name "translate"
            :value "on" 
            )
           "translate?&nbsp;" 
           ))  |#
		   ;;
		   (:td 
          ((:input :type "checkbox" :name "double-stranded"
            :value "on" 
            )
           "Double-stranded?&nbsp;" 
           ))
		   ;;end checkboxes
           ;; ************ ^^^^^^^^^^^^^^ **************
         :newline 

         ))

       (flet ((goto-url (command)
                (utils::url-with-parameters
                 wb::*sseqview-url*
                 (format nil "pkg=~A" wb::*sessionid*)
                 (format nil "~A=y" command)
                 ))
              (space-and-newline () (html (:princ "&nbsp;") :newline)))

         (html
          (:princ "&nbsp;&nbsp;")
          ((:a :href (make-sseqview-orgs-and-contigs-url)) 
           "Select org/contig")
          (space-and-newline))

         (unless (svs-errors my-svs)

           (let* ((contig (svs-contig my-svs))
                  (clen (#^sequence-length contig))
                  (from (svs-from my-svs))
                  (to (svs-to my-svs)))

             ;; start link
             (when (/= 1 from)
               (html 
                ;; (:princ "&nbsp;")  (:princ "&nbsp;")  (:princ "&nbsp;")
                ((:a :href (goto-url "start")) "Start")
                (space-and-newline)
                ))
             ;; prev link
             (when (or (/= 1 from) (/= to clen))
               (html
                ((:a :href (goto-url "prev")) "Prev")
                (space-and-newline)
                ))
             ;; next link
             (when (or (/= 1 from) (/= to clen))
               (html 
                ((:a :href (goto-url "next")) "Next")
                (space-and-newline)
                ))
				
			 ;; end link
             (when (/= to clen)
               (html
                ((:a :href (goto-url "end")) "End") 
                (space-and-newline)
                ))
             (when (svs-match-locations my-svs)
               (html
                ((:a :href (goto-url "matchprev")) "Prev Match")
                (space-and-newline)
                ((:a :href (goto-url "matchnext")) "Next Match")
                (space-and-newline)
                ))
             (when (svs-search-note my-svs) 
               (let ((color
                      (if (string-equal
                           (svs-search-note my-svs) "No match found")
                          :red
                        :green
                        )))
                 (html
                  ((:font :color color) 
                   (:princ "&nbsp;&nbsp;")
                   (:princ-safe (svs-search-note my-svs))
                   :newline
                   )))
               ))))
       :newline
       ((:input :type "HIDDEN" 
         :name "PKG" :value (string (user-session-id))))
       :hr

       )))))

(defun tooltip (g)
  (let* ((from (#^from g))
         (to (#^to g))
         (nuclen
          (if (>= to from) 
              (1+ (- to from))
            (+ 
             (1+ (- (#^sequence-length (#^contiguous-sequence g)) from))
             to))))
    (if (#^encodes-protein g)
        (let ((seq (get-seqview-basepairs (#^contiguous-sequence g) from to)))
          (formatn
           "Nucleotide length: ~D, Amino Acid length: ~D, Molecular Weight: ~A"
           nuclen (ceiling nuclen 3) 
           (let ((mw (ignore-errors 
                       (bio::molecular-weight-of 
                        (bio::translate-d/rna-to-aa seq)))))
             (or mw "??")
             )))
      (formatn 
       "Nucleotide length: ~D, * Does not encode protein *"
       nuclen
       ))))

(defun new-process-start-prev-next-end (start prev next end)
  (when (or start prev next end)
    (let* ((my-svs *current-svs*)
           (from (svs-from my-svs))
           (to (svs-to my-svs))
           (cols (svs-columns my-svs))
           (contig (svs-contig my-svs))
           (seqlen (#^sequence-length contig))
           (basepairs-being-shown (circseg-length from to seqlen))
           (rows-being-displayed (ceiling basepairs-being-shown cols)))
      (cond 
       (start 
        (setf (svs-from my-svs) 1)
        (setf (svs-to my-svs) (min basepairs-being-shown seqlen)))
       (end 
        (setf (svs-to my-svs) seqlen)
        (setf (svs-from my-svs) (max 1 (1+ (- seqlen basepairs-being-shown)))))
       (prev 
        (let ((newstart 
               (if (< rows-being-displayed 10)
                   (circ- from basepairs-being-shown seqlen)
                 (circ+ 
                  (circ- from basepairs-being-shown seqlen)
                  (* (svs-columns my-svs) 2)
                  seqlen
                  ))))
          (setf (svs-from my-svs) newstart)
          (setf (svs-to my-svs) 
                (circ+ newstart (1- basepairs-being-shown) seqlen))
          ))
       (next 
        (let ((newend
               (if (< rows-being-displayed 10)
                   (circ+ to basepairs-being-shown seqlen)
                 (circ-
                  (circ+ to basepairs-being-shown seqlen)
                  (* (svs-columns my-svs) 2)
                  seqlen
                  ))))
          (setf (svs-to my-svs) newend)
          (setf (svs-from my-svs)
                (circ- newend (1- basepairs-being-shown) seqlen))
          ))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Utility routines

(defun organism-predicate (x) (stringp (#^organism-prefix x)))
(defun contig-predicate (x) 
  (eq (#^organism-entity-type x) #$contiguous-sequence))

(defun org-string-to-org-frame (x frame-predicate)
  (let ((f (frame-fnamed x)))
    (if (and f (funcall frame-predicate f))
        f
      (let ((s (find-symbol (string-upcase x) :oa)))
        (when (and s (boundp s))
          (let ((sv (symbol-value s)))
            (when (and (frames::isframe? sv) (funcall frame-predicate sv))
              sv 
              )))))))
                
(defun contig-string-to-contig-frame (cs org-provided? org)
  
  ;; if the user typed in an organism, 
  ;;   then if no contig was provided, 
  ;;       then the chromosome of the organism is used
  ;;       else find the contig within the organism 
  ;; if the string names a contig directly, use it
  ;; else try to locate a contig with that name 

  (block exit

    (if org-provided?
        (if (null cs) 
            (if (#^completed org)
                (first (#^contiguous-sequences org))
              nil)
          (loop for c in (#^contiguous-sequences org) do
                (when (search cs (fname c) :test 'char-equal)
                  (return-from exit c)
                  )))
      (let ((f (frame-fnamed cs)))
        (if (and f (contig-predicate f))
            f
          (loop for orgf in (bio::loaded-organisms)
                do
                (loop for contig in (#^contiguous-sequences orgf)
                      do
                      (let ((cname (second (string-split (fname contig) #\.))))
                        (when (and cname (string-equal cname cs))
                          (return-from exit contig)
                          )))))))))

(defun null-or-blank? (x)
  (or (null x) (and (stringp x) (whitespacep x))))

(defun has-content? (x) 
  (not (null-or-blank? x)))

(defun org-prefix-for-display (org)
  (reverse (subseq (reverse (#^Organism-prefix org)) 1)))

(defun contig-name-for-display (contig)
  (string-capitalize
   (let ((x (string-split (#^fname contig) #\.)))
     (if (= (length x) 1)
         (first x)
       (string-join (cdr x) #\.)
       ))))

(defun html-for-title-with-org-and-contig (svs)
  (html
   (:h2
    (:center
     (if (or (null (svs-org svs)) (null (svs-contig svs)))
         (html 
          ((:font :color "brown") (:princ-safe "Biobike Sequence Viewer")))
       (let ((org-name-for-display 
              (substitute #\Space #\_ (#^fname (svs-org svs))))
             (contig-name-for-display 
              (contig-name-for-display (svs-contig svs))))
         ;; try to prevent header text from wrapping and causing
         ;; problems with the sequence display window
         (when (> (+ (length org-name-for-display) 
                     (length contig-name-for-display))
                  45)
           (setq org-name-for-display 
                 (limited-string org-name-for-display 15))
           (setq contig-name-for-display 
                 (limited-string contig-name-for-display 25)))
         (html
          ((:a :href 
            (s+ 
             "/" 
             (forward-package-funcall 
              :de :simple-frame-editor-url (svs-org svs)))
            :style "text-decoration:none"
            :onmouseover 
            "this.style.textDecoration='underline'; this.style.color='blue';"
            :onmouseout "this.style.textDecoration='none';"
            :target "_blank"
            )
           ((:font :color "blue")
            (:princ-safe org-name-for-display)))
          " - "
          ((:a 
            :href 
            (s+ 
             "/" 
             (forward-package-funcall 
              :de :simple-frame-editor-url (svs-contig svs)))
            :style "text-decoration:none"
            :onmouseover 
            "this.style.textDecoration='underline'; this.style.color='blue';"
            :onmouseout "this.style.textDecoration='none';"
            :target "_blank"
            )
           ((:font :color "blue")
            (:princ-safe contig-name-for-display)))
          (:princ-safe 
           (formatn " (~D -> ~D of ~D)" 
                    (svs-from svs) (svs-to svs) 
                    (#^sequence-length (svs-contig svs))))
          )))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun compute-sseqview-results ()
  (let* ((my-svs *current-svs*)
         (contig (svs-contig my-svs))
         (from (svs-from my-svs))
         (to (svs-to my-svs))
         (columns (svs-columns my-svs))
         (basepairs (get-seqview-basepairs contig from to))
         )

    ;; Construct the data structures for the display, ultimately
    ;; resulting in a LINEMAP

    (multiple-value-bind (csg color-table)
        (create-gene-map-and-color-map contig from to)
      (setq *csg* csg)
      (setq *genemap* (utils::circseg-array csg))
      (setq *color-table* color-table)
      
      (let* ((initial-segment-map 
              (setq *initial-segment-map* 
                    (new-create-segment-map csg color-table)))
             (search-modified-segment-map
              (setq *sm-segment-map* 
                    (if (svs-search my-svs)
                        (new-create-search-modified-segment-map 
                         initial-segment-map my-svs csg basepairs)
                      initial-segment-map 
                      )))
             (final-segment-map 
              (setq *final-segment-map*
                    (new-split-segments-that-span-multiple-lines
                     search-modified-segment-map csg columns
                     )))
             (linemap 
              (setq 
               *linemap* 
               (new-create-line-map final-segment-map csg columns contig (svs-invert? my-svs)) ; ***********
               )))
        (setq *current-linemap* linemap)
        ))))

(defun create-gene-map-and-color-map (contig from to) 
  (let ((cs (create-circseg from to (#^sequence-length contig)))
        (color-hash (make-hash-table :test 'eq))
        (forward-color-index 0)
        (backward-color-index 0)
        (n-forward-colors (length *seqview-forward-gene-colors*))
        (n-backward-colors (length *seqview-backward-gene-colors*)))
    (loop for g across (#^genes-sorted-by-position contig)
          as fromg = (#^from g)
          as tog =  (#^to g)
          as dir = (#^direction g)
          as anywhere-within? = nil
          do
          (cs-loop-from-to (i cs fromg tog)
            (when (in-circseg? i cs) 
              (setq anywhere-within? t)
              (setf-cs-aref cs i (cons g (cs-aref cs i)))
              ))
          (case dir
            (:f 
             (when anywhere-within? 
               (setf (gethash g color-hash) 
                     (nth forward-color-index *seqview-forward-gene-colors*)
                     ))
             (incf forward-color-index)
             (when (= forward-color-index n-forward-colors)
               (setq forward-color-index 0)
               ))
            (:b
             (when anywhere-within?
               (setf (gethash g color-hash)
                     (nth backward-color-index *seqview-backward-gene-colors*)
                     ))
             (incf backward-color-index)
             (when (= backward-color-index n-backward-colors)
               (setq backward-color-index 0)
               ))))
    (cs-loop-from-to (pos cs from to)
      (setf-cs-aref cs pos (reverse (cs-aref cs pos))))
    (values cs color-hash)
    ))

(defun new-create-segment-map (csg color-table)
  (let* ((from (utils::circseg-from csg))
         (to (utils::circseg-to csg))
         (current-segment (make-svsegment :start from))
         (genes-at-pos (cs-aref csg from))
         (segments nil))
    (flet ((get-gene-color (genes-list) 
             (let ((ngenes (length genes-list)))
               (cond
                ((zerop ngenes) no-genes-color)
                ;; use the color we assigned previously to the gene,
                ;; which we stored in our hash table
                ((= 1 ngenes) 
                 (let* ((gene (first genes-list))
                        (color (gethash gene color-table)))
                   (unless color 
                     (error "No color assigned to gene ~A" gene))
                   color
                   ))
                ;; use a color based on the directions of the 2 genes 
                ;; that overlapped as per Elhai's color choices
                ((= 2 ngenes) 
                 (let ((dirg1 (#^direction (first genes-list)))
                       (dirg2 (#^direction (second genes-list))))
                   (cond
                    ((and (eq dirg1 :f) (eq dirg2 :f)) 
                     *seqview-overlapping-forward-gene-color*)
                    ((and (eq dirg1 :b) (eq dirg2 :b))
                     *seqview-overlapping-backward-gene-color*)
                    (t *seqview-overlapping-forward-backward-gene-color*)
                    )))
                ;; shouldn't happen but just in case 
                (t multiple-overlap-color)
                ))))
      (setf (svsegment-genes current-segment) genes-at-pos)
      (setf (svsegment-color current-segment) (get-gene-color genes-at-pos))
      ;; every time the set of genes changes from one position to another,
      ;; that ends a (and usually starts a) new segment 
      (cs-loop-from-to (pos csg from to)
        (if (/= pos to)
            (let ((current-genes (cs-aref csg pos))
                  (next-genes 
                   (cs-aref csg (circseg+ csg pos 1))))
              ;; finish old segment and start new one
              (when (not (equal current-genes next-genes))
                (setf (svsegment-end current-segment) pos)
                (push current-segment segments)
                (setq current-segment 
                      (make-svsegment
                       :start (circseg+ csg pos 1) 
                       :genes next-genes))
                (setf (svsegment-color current-segment) 
                      (get-gene-color next-genes)
                      )))
          ;; if we're at the end, always finish current segment 
          (progn
            (setf (svsegment-end current-segment) to)
            (push current-segment segments)
            ))))
    (setq segments (reverse segments))
    
    segments

    ))

(defun all-sseqview-matches (basepairs my-svs)
  (let ((search-string (svs-search my-svs)))
    (ppcre::all-matches 
     (ppcre::create-scanner search-string :case-insensitive-mode t)
     basepairs
     )
    ))

(defun all-basepairs? (s)
  (every (lambda (x) (member x '(#\A #\C #\G #\T) :test 'char-equal)) s))

(defun match-coordinates->circseg-coordinates (matches csg)
  (loop for m on matches by #'cddr 
        collect
        (list 
         (zbc->circseg-coord (first m) csg)
         (zbc->circseg-coord (1- (second m)) csg)
         )))

(defun new-create-search-modified-segment-map (segment-map my-svs csg basepairs)
  (let ((matches (all-sseqview-matches basepairs my-svs)))
    (if (null matches) 
        (progn
          (setf (svs-search-note my-svs) "No match found")
          segment-map 
          )
      ;; translate zero-based pattern match coordinates (start end) to
      ;; circular segment coordinates (from to)
      (let* ((match-segments 
              (match-coordinates->circseg-coordinates matches csg))
             (search-modified-segment-map (copy-list segment-map)))
        (setq *match-segments* match-segments)
        (loop for match-segment in match-segments do
              (setq search-modified-segment-map 
                    (new-merge-match-segment-into-segment-map 
                     search-modified-segment-map match-segment csg
                     )))
        (let ((len (length match-segments))
              (elen (length (svs-match-locations my-svs))))
          (setf (svs-search-note my-svs)
                (if (plusp elen)
                    (formatn "Occurence ~D (of ~D total, ~D in this range)"
                             (1+ (svs-match-counter my-svs)) elen len)
                  (formatn "~D match~A found." len (if (> len 1) "es" ""))
                  )))
        search-modified-segment-map
        ))))

(defun new-merge-match-segment-into-segment-map (segment-map match-segment csg)
  (let* ((start-segment nil)
         (end-segment nil)
         (match-from (first match-segment))
         (match-to (second match-segment))
         (pre-segments nil)
         (mid-segments nil)
         (post-segments nil)
         (new-match-start-segments nil)
         (new-match-end-segments nil))

    ;; Find which existing segment contains the coordinate for the start
    ;; of the match segment, and similarly for the end of the match segment
    (loop for segment in segment-map 
          for count from 1
          as segment-from = (svsegment-start segment)
          as segment-to = (svsegment-end segment)
          do
          (when (csc-within? match-from segment-from segment-to)
            (unless start-segment (setq start-segment segment))
            (when end-segment 
              (error "new-merge: This should be impossible!")))
          (when (csc-within? match-to segment-from segment-to)
            (setq end-segment segment) 
            (unless start-segment 
              (error "new-merge: This too should be impossible!")))
          (when (and start-segment end-segment) (return nil))
          )
    
    (unless (and start-segment end-segment)
      (error "Did not match segment ~A in segment map!" match-segment)
      )

    (setq pre-segments 
          (loop for seg in segment-map 
                until (eq seg start-segment) 
                collect seg
                ))

    (setq post-segments 
          (reverse 
           (loop for seg in (reverse segment-map) 
                 until (eq seg end-segment)
                 collect seg
                 )))
    
    (let ((ss-start (svsegment-start start-segment))
          (ss-end (svsegment-end start-segment))
          (es-start (svsegment-start end-segment))
          (es-end (svsegment-end end-segment)))

      (flet ((make-pm-segment (new-start new-end existing-segment)
               (make-svsegment 
                :start new-start :end new-end 
                :color (svsegment-color existing-segment)
                :genes (svsegment-genes existing-segment)
                :pattern-match? t
                )))

        (cond

         ;; match-segment is within a single segment
         ((eq start-segment end-segment)
          (cond
           ;; match-segment is equivalent to segment
           ((and (= match-from ss-start) (= match-to ss-end)) 
            (setf (svsegment-pattern-match? start-segment) t))
           ;; match-segment starts at the same place the existing segment
           ;; starts but doesn't end in the same place
           ((= match-from ss-start)
            (let ((pm-segment 
                   (make-pm-segment match-from match-to start-segment)))
              ;; smash existing segment to start after end of match segment
              (setf (svsegment-start start-segment) (circseg1+ csg match-to))
              (setq new-match-start-segments (list pm-segment start-segment))
              ))
           ;; match-segment ends at the same place the existing segment ends
           ;; but doesn't start in the same place
           ((= match-to ss-end)
            (let ((pm-segment 
                   (make-pm-segment match-from match-to start-segment)))
              ;; smash existing segment to end before start of match segment
              (setf (svsegment-end start-segment) (circseg1- csg match-from))
              (setq new-match-start-segments (list start-segment pm-segment))
              ))
           ;; match-segment is in the middle of the segment
           ;; create a total of 3 segments 
           (t 
            (let ((pm-segment 
                   (make-pm-segment match-from match-to start-segment))
                  (ss1 (copy-svsegment start-segment))
                  (ss2 (copy-svsegment start-segment)))
              ;; smash first copied segment to end before start of match
              (setf (svsegment-end ss1) (circseg1- csg match-from))
              ;; smash second copied segment to start after end of match
              (setf (svsegment-start ss2) (circseg1+ csg match-to))
              (setq new-match-start-segments (list ss1 pm-segment ss2))
              ))))

         ;; match-segment starts in one segment and ends in another
         (t 
          (cond
           ;; match-segment spans entire start segment
           ((= match-from ss-start)
            (setf (svsegment-pattern-match? start-segment) t)
            (setq new-match-start-segments (list start-segment)))
           ;; match-segment starts in middle of start-segment 
           (t 
            (let ((pm-segment 
                   (make-pm-segment match-from ss-end start-segment)))
              ;; smash existing segment to end before start of match segment
              (setf (svsegment-end start-segment) (circseg1- csg match-from))
              (setq new-match-start-segments (list start-segment pm-segment))
              )))
          (cond
           ;; match-segment spans entire end segment 
           ((= match-to es-end)
            (setf (svsegment-pattern-match? end-segment) t)
            (setq new-match-end-segments (list end-segment)))
           ;; match-segment ends in middle of end segment
           (t
            (let ((pm-segment 
                   (make-pm-segment es-start match-to end-segment)))
              ;; smash existing segment to start after end of match segment
              (setf (svsegment-start end-segment) (circseg1+ csg match-to))
              (setq new-match-end-segments (list pm-segment end-segment))
              )))
          ;; when there are segments between the start-segment and the 
          ;; end-segment, these become pattern match segments
          (when (/= ss-end (circseg1- csg es-start))
            (loop for seg in segment-map 
                  as within? = nil
                  do
                  (cond
                   ((eq seg start-segment) (setq within? t))
                   ((eq seg end-segment) (return nil))
                   (within?
                    (push seg mid-segments)
                    (setf (svsegment-pattern-match? seg) t))
                   (t nil)
                   )
                  finally (setq mid-segments (reverse mid-segments))
                  ))
          ))))

    (append 
     pre-segments new-match-start-segments mid-segments 
     new-match-end-segments post-segments
     )))

(defun new-split-segments-that-span-multiple-lines (segments csg cols)
  (let ((from (utils::circseg-from csg)))
    (labels ((maybe-break-one-segment (segment) 
               (let* ((start (svsegment-start segment))
                      (end (svsegment-end segment))
                      (zstart (circseg-coord->zbc start csg))
                      (zend (circseg-coord->zbc end csg))
                      (startline (floor zstart cols))
                      (endline (floor zend cols))
                      (genes (svsegment-genes segment))
                      (color (svsegment-color segment))
                      (pattern-match? (svsegment-pattern-match? segment)))
                 (if (= startline endline)
                     ;; segment is contained on a single line
                     (list segment)
                   ;; find the coordinate of the end of the first line
                   ;; the segment is on
                   (let* ((end-of-line 
                           (circseg+ 
                            csg from (+ (* startline cols) (1- cols))
                            ))
                          ;; create a partial segment all of which is on
                          ;; the first line
                          (segment-a 
                           (make-svsegment 
                            :start start
                            :genes genes
                            :color color
                            :end end-of-line
                            :pattern-match? pattern-match?
                            ))
                          ;; create second segment that spans the remainder
                          ;; of the original segment
                          (segment-b
                           (make-svsegment
                            :start (circseg+ csg end-of-line 1)
                            :genes genes
                            :color color
                            :end end
                            :pattern-match? pattern-match?
                            )))
                     ;; and recurse on the second segment
                     (cons segment-a (maybe-break-one-segment segment-b))
                     )))))
      (mapcan #'maybe-break-one-segment segments)
      )))

;; *********** VVVVVVVVVVVVVVVV **************
(defun new-create-line-map (segments csg cols contig invert?);; &aux lines)

  (let 
  (
  (lines)
  (number-of-lines-required 
         (ceiling (length (utils::circseg-array csg)) cols))
        (from (utils::circseg-from csg)))

    (setq 
     lines 
     (if invert? 
	 ;;*run function in reverse* 
	 (loop 
      with genes-present = nil
	  
	  ;;reverse of next code section
	  ;;loop for row from number-of-lines-required by -1
	  ;;until row <= 0
;; *********** ^^^^^^^^^^^^^^ **************

      for row from 0
      as row-start-pos = (circseg+ csg from (* row cols))
      as row-end-pos = (circseg+ csg row-start-pos (1- cols))
      until (>= row number-of-lines-required)
      collect
      (let ((line (make-svline :num row-start-pos)))
        (setf (svline-segments line) 
              (mapcan 
               (lambda (segment)
                 (let ((start (svsegment-start segment))
                       (end (svsegment-end segment)))
                   (when (and (csc-within? start row-start-pos row-end-pos)
                              (csc-within? end row-start-pos row-end-pos))
                     (list segment)
                     )))
               segments
               ))
        ;; probably no reason to sort them...
        ;; maybe no reason to have this info at all...
        (setf (svline-genes line) 
              (sort 
               (remove-duplicates 
                (mapcan 
                 (lambda (x) (copy-list (svsegment-genes x)))
                 (svline-segments line)))
               '<
               :key (lambda (gene) (slotv gene #$from))
               ))
        (setf (svline-links line) (svline-genes line))
        ;; make a list of all the genes that begin on this line
        (loop for gene in (svline-genes line) do 
              (when (not (member gene genes-present))
                (push gene genes-present)
                (push gene (svline-new-genes line))
                ))
        line
;; *********** VVVVVVVVVVVVVVVV **************
        ))
	 
	 ;;*run function forward*
	 (loop 
      with genes-present = nil
	  
      for row from 0
      as row-start-pos = (circseg+ csg from (* row cols))
      as row-end-pos = (circseg+ csg row-start-pos (1- cols))
      until (>= row number-of-lines-required)
      collect
      (let ((line (make-svline :num row-start-pos)))
        (setf (svline-segments line) 
              (mapcan 
               (lambda (segment)
                 (let ((start (svsegment-start segment))
                       (end (svsegment-end segment)))
                   (when (and (csc-within? start row-start-pos row-end-pos)
                              (csc-within? end row-start-pos row-end-pos))
                     (list segment)
                     )))
               segments
               ))
        ;; probably no reason to sort them...
        ;; maybe no reason to have this info at all...
        (setf (svline-genes line) 
              (sort 
               (remove-duplicates 
                (mapcan 
                 (lambda (x) (copy-list (svsegment-genes x)))
                 (svline-segments line)))
               '<
               :key (lambda (gene) (slotv gene #$from))
               ))
        (setf (svline-links line) (svline-genes line))
        ;; make a list of all the genes that begin on this line
        (loop for gene in (svline-genes line) do 
              (when (not (member gene genes-present))
                (push gene genes-present)
                (push gene (svline-new-genes line))
                ))
        line
        ))
		);;end if
		)
;; *********** ^^^^^^^^^^^^^^^^^ **************

    
    ;; split up a segment that happens to cross the 
    ;; contig boundary if the contig is not circular 
    (when (not (#^circular contig))
      (loop named outer-loop
       with spanning-segment = nil
       for remaining-lines on lines
       as line = (first remaining-lines)
       for line-count from 1
       do
       (loop named inner-loop
        for segment in (svline-segments line)
        do 
        (when (> (svsegment-start segment) (svsegment-end segment))
          (setq spanning-segment segment)
          (return-from inner-loop nil)
          ))
       (when spanning-segment
         (unless (null (svsegment-genes spanning-segment))
           (error "A gene ~A spans the boundary in a non-circular contig ~A!"
                  (first (svsegment-genes spanning-segment))
                  contig))
         (let ((next-line-segment nil)
               (dummy-line nil)
               (new-line nil)
               (further-lines? (not (null (cdr remaining-lines)))))
           ;; create a new segment that will go onto the next line,
           ;; starting at the beginning of the contig 
           (setf next-line-segment 
                 (make-svsegment 
                  :start 1
                  :end (svsegment-end spanning-segment)
                  :color (svsegment-color spanning-segment)
                  :genes nil
                  :pattern-match? nil
                  ))
           ;; smash the old segment so that it ends at the end of the contig
           (setf (svsegment-end spanning-segment) (#^sequence-length contig))
           (setf (svsegment-pattern-match? spanning-segment) nil)
           ;; create a dummy line to go between the end of the contig and the
           ;; beginning of the contig
           (setq dummy-line (make-svline :num "---"))
           ;; create a new line, and give it the new segment
           (setq new-line
                 (make-svline 
                  :num 1
                  :segments (list next-line-segment)
                  ))
           (if (null further-lines?)
               ;; add the dummy line and the new line to our list of lines
               (setq lines (append lines (list dummy-line new-line)))
             ;; insert the dummy line and the next line between the current line
             ;; and the next line
             (setq lines 
                   (append 
                    (subseq lines 0 line-count)
                    (list dummy-line new-line)
                    (subseq lines line-count)
                    ))))
         (return-from outer-loop nil)
         )))
                  
    (loop for remaining-lines on lines 
          as line = (first remaining-lines)
          do 
          (when (= (length (svline-new-genes line)) 1)
            (let* ((gene (first (svline-new-genes line)))
                   (gene-description (bbi-description-of gene))
                   (gene-description-lines 
                    (and gene-description 
                         (string-to-lines gene-description 40))))
              (loop for rrl on (cdr remaining-lines) 
                    for gdl in gene-description-lines
                    as possible-text-line = (first rrl)
                    until (svline-new-genes possible-text-line)
                    do
                    (setf (svline-text possible-text-line) gdl)
                    ))))

    lines
    ))

(defun bbi-description-of (gene)
  (bbi::with-bbl-form 
    (bbi::description-of gene :length 1000)))

(defun new-display-sseqview-data (my-svs)
  
  (let* ((linemap *current-linemap*)
         (contig (svs-contig my-svs)) 
         (from (svs-from my-svs))
         (to (svs-to my-svs))
;; *********** VVVVVVVVVVVVVVVV **************
		 (tempCounter 0);;this is to clear up which line is being printed for double stranded genes
		 (savedColor (make-array 40));;this is to store the color of the previos line for use in double stranded genes
		 (savedBPS (make-array 40));; this is to account for the mulpuple segments per line when search quires are made
		 (lengthAggregate 0);; this will be used to test when there have been enough segments printed on a single line
		 (inverted  (svs-invert? my-svs)) ; ************** modified from invert?
		 (translate? (svs-translate? my-svs))
         (double-stranded? (svs-double-stranded? my-svs)) ; ************
;; *********** ^^^^^^^^^^^^^^ **************
         (basepairs (get-seqview-basepairs contig from to))
         (basepairs-cg 
          (create-circseg 
           from to (#^sequence-length contig) :data-array basepairs
                      )))
        (when (svs-notes my-svs)
          (html 
           ((:font :color "green")
            (loop for note in (svs-notes my-svs) do
                  (if (listp note) 
                      (html :br (:princ-safe (car note)))
                    (html :br "Note: " (:princ-safe note)))
                  finally (html :br :br)
                  ))))

        ;; Display each line
	(FLET ((MAYBE-REVERSE (list)   ; ********** 2
               (IF inverted          ; ********** 2 modified from invert?
                   (REVERSE list)   ; ********** 2
                   list))           ; ********** 2
           )                        ; ********** 2
        (html 
         (:table 
          (:tr 
           ((:td :align "right") 
            (:tt
             (loop for line in (MAYBE-REVERSE linemap) ; ********** 2 
                   as start = (svline-num line)
                   do 
                   (html 
;; *********** VVVVVVVVVVVVVVVV **************
					(:princ-safe (format nil "~D" (if inverted (+ start 59) start) )) ;;start is the identifier for the numbers that appear in the leftmost colunm designating the first nucleotide of that line
                    :br
					(when double-stranded? (html :br ))
					(when translate? (html :br ));;test with extra break for if(translate) Not complete
;; *********** ^^^^^^^^^^^^^^^ **************
                    :newline
                    ))))
           :newline
           ((:td :align "left")
            (:tt
             (loop 
              for line in (MAYBE-REVERSE linemap) ; ********** 2
              as segments = (svline-segments line)
              do
              (loop
               for segment in (MAYBE-REVERSE segments) ; ********** 2
               as start = (svsegment-start segment)
               as end = (svsegment-end segment)
               as color = (svsegment-color segment)
;; *********** VVVVVVVVVVVVVVVV **************
               as bps-pre = (cs-subseq basepairs-cg start end) ;***************
         ;      as bps = (cs-subseq basepairs-cg start end)   ; ******* original commented out
               as bps = (if inverted         ;************************ modified from invert?
							(bioutils::ncomplement-base-pairs bps-pre) ; **************
                            bps-pre) ; ***********
							
			   do
			   (setq lengthAggregate 0)
			   (loop
			   for i from 0 to  tempCounter 
			   do
			   (setq lengthAggregate (+ (bbi::LENGTH-OF (aref savedBPS i)) lengthAggregate))
			   )
			   (setq lengthAggregate (+ (bbi::LENGTH-OF  bps) lengthAggregate)) 
               (html  ((:font :color (color-to-html-color color))
                          (if double-stranded? 
							(progn
								(when (and (= lengthAggregate 60) (/= (bbi::LENGTH-OF bps) 60)) 
									(progn (html (:princ-safe  bps):br)
									(loop
									for i from 0 to (- tempCounter 1)
									do
									(html ((:font :color (color-to-html-color (aref savedColor i)))(:princ-safe  (aref savedBPS i))) )
									)
									(html((:font :color (color-to-html-color color))(:princ-safe (bioutils::ncomplement-base-pairs bps :reverse? nil))));;needs color to be saved?
									(setq tempCounter 0)
									(setq savedBPS (make-array 40))))
							(if (= (bbi::LENGTH-OF bps) 60)
								(if (svsegment-pattern-match? segment) (html ((:font :style (formatn "background-color:~A" background-hex-color) :color color)	(:princ-safe bps) :br (:princ-safe (bioutils::ncomplement-base-pairs bps :reverse? nil))))
								(html  (:princ-safe bps) :br (:princ-safe (bioutils::ncomplement-base-pairs bps :reverse? nil))   ))
								 
								(when (<= lengthAggregate 59)
									(html  (:princ-safe  bps) (setf (svref savedBPS tempCounter) (bioutils::ncomplement-base-pairs bps :reverse? nil)) (setf (svref savedColor tempCounter) color) (setq tempCounter (+ 1 tempCounter)));;testing
								)       
							))
							(if (svsegment-pattern-match? segment)
							(html ((:font :style 
								(formatn "background-color:~A" background-hex-color)
								:color color)
								(:princ-safe bps)
							))
							(html 
							((:font :color (color-to-html-color color))
							(:princ-safe bps)
							)))
				   )
                          )
				   
				   ))
;; *********** ^^^^^^^^^^^^^ **************
              (html :br :newline)
              )))
           :newline
           ((:td :align "left" :style "color: brown;")
            (:tt
             (loop
              for line in (MAYBE-REVERSE linemap) ; ********** 2
              as new-genes = (svline-new-genes line)
              as text = (svline-text line)
              do
              (loop 
               for g in new-genes 
               do
               (html
                (:princ "&nbsp;")
                ((:a :href 
                  (s+
                   "/"
                   (forward-package-funcall :de :simple-frame-editor-url g))
                  :title (tooltip g)
                  :target "_blank"
                  )
                 (:princ-safe (frames::fname g))
                 )
                (:princ "&nbsp;")
                (:princ-safe 
                 (formatn "(~D ~A ~D)"
                          (#^from g) 
                          (if (eq (#^direction g) :F) "->" "<-")
                          (#^to g)
                          ))))
              (when text (html "&nbsp;" (:princ-safe text)))
;; *********** VVVVVVVVVVVVVVVV **************
              (if double-stranded? (html (when translate? (html :br )) :br :br :newline) (html (when translate? (html :br )) :br :newline));;test with extra break for if(double-stranded) *translate not implemented
;; *********** ^^^^^^^^^^^^^^^^ **************
              )))
           :newline
           ))
         ((:div :style "float: right; height: 100%; background-color: green;"))
         ) ; ********** 2
		 )))

(defun get-seqview-basepairs (contig from to)
  
	(if (and (> from to) (not (#^circular contig)))
      (s+ 
       (bio::extract-contig-sequence contig from (#^sequence-length contig) :f)
       (bio::extract-contig-sequence contig 1 to :f)
       )
    (bio::extract-contig-sequence contig from to :f)
    ))

(defun sseqview-entire-search ()
  (let* ((my-svs *current-svs*)
         (contig (svs-contig my-svs))
         (circular? (#^circular contig))
         (clen (#^sequence-length contig))
         (cs (create-circseg 1 clen clen))
         (search-string (svs-match-string my-svs))
         (next/prev (svs-search-entire-next/prev my-svs))
         (cols (svs-columns my-svs))
         )
    ;; reset search string to 'entire contig' search string
    (setf (svs-search my-svs) search-string)
    (labels 
        ((doit (from to) 
           (let* ((extent (min clen *default-nucleotides-to-display*))
                  (range 
                   (circ-range 
                    from to (* 2 cols) extent clen nil nil circular?
                    ))
                  (range-from (first range))
                  (range-to (second range)))
             (setf (svs-from my-svs) range-from)
             (setf (svs-to my-svs) range-to)
             (setf (svs-extent my-svs) 
                   (circseg-length range-from range-to clen)
                   ))
           (compute-sseqview-results)
           )
         (set-up-next-prev-and-compute (dir)
           ;; the match counter is zero-based
           (let* ((matches (svs-match-locations my-svs))
                  (nmatches (length matches))
                  (match-counter (svs-match-counter my-svs))
                  (match-index 
                   (case dir
                     (:prev (1- match-counter))
                     (:next (1+ match-counter))
                     )))
             (case dir
               (:prev 
                (when (minusp match-index) (setq match-index (1- nmatches))))
               (:next (when (= match-index nmatches) (setq match-index 0)))
               )
             (setf (svs-match-counter my-svs) match-index)
             (let* ((contig-match-coords (nth match-index matches))
                    (from (first contig-match-coords))
                    (to (second contig-match-coords)))
               (doit from to)
               ))))
      
      (if next/prev 

          (set-up-next-prev-and-compute next/prev)

        ;; Doing the intial search over the entire contig.
        ;; Do the search and get all the match locations.  
        (let* ((basepairs (bio::extract-contig-sequence contig 1 clen :f))
               (matches (all-sseqview-matches basepairs my-svs))
               (contig-match-coords 
                (match-coordinates->circseg-coordinates matches cs))
               (first-coords (first contig-match-coords))
               (from (first first-coords))
               (to (second first-coords))
               )
          (if (null matches) 
              ;; If there aren't any matches just display the
              ;; start of the contig and note that no matches were found.
              ;; No need to set up the 'next match' mechanism.  
              (progn
                (set-from-to-extent-defaults my-svs)
                (setf (svs-search-note my-svs) "No match found")
                (setf (svs-search my-svs) nil)
                (compute-sseqview-results)
                (setf (svs-search my-svs) search-string)
                )
            ;; Matches were found.  Display contig starting at first match. 
            ;; Set up 'next match' mechanism
            (progn
              (setf (svs-match-locations my-svs) contig-match-coords)
              (setf (svs-match-counter my-svs) 0)
              (setf (svs-search-entire-next/prev my-svs) nil)
              (doit from to)
              )))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun parse-goto-string (gs)
  (let ((s (string-trim *whitespace* gs))
        (number-comma-number-pattern 
         (ppcre::create-scanner "(\\d+)\\s*,\\s*(\\d+)"))
        (number-minus-number-pattern
         (ppcre::create-scanner "(\\d+)\\s*\\-\\s*(\\d+)"))
        (number-plus-number-pattern
         (ppcre::create-scanner "(\\d+)\\s*\\+\\s*(\\d+)"))
        )
    (flet ((match (pattern type) 
             (when (ppcre::all-matches pattern s)
               (multiple-value-bind (ignore match-vector)
                   (ppcre::scan-to-strings pattern s)
                 (declare (ignore ignore))
                 (cons type (map 'list 'read-from-string match-vector))
                 ))))
      (let ((decoded-pattern
             (cond
              ((match number-comma-number-pattern :comma))
              ((match number-minus-number-pattern :minus))
              ((match number-plus-number-pattern :plus))
              ((every #'digit-char-p s) 
               (list :number (read-from-string s) nil))
              (t 
               (vif (g (actually-a-gene? s))
                    (list :gene g)
                    (list :unrecognized s)
                    )))))
        decoded-pattern
        ))))

(defun decoded-pattern->from-and-to (decoded-pattern contig my-svs)
  (block exit 
    (flet ((oops (s &rest args)
             (push (apply 'format nil s args) (svs-errors my-svs))
             (return-from exit nil)
             ))
      (let ((clen (#^sequence-length contig)))
        (case (first decoded-pattern)
          (:comma 
           (let ((from (second decoded-pattern))
                 (to (third decoded-pattern)))
             (when (> from clen) 
               (oops "FROM, ~D, bigger than ~A's size ~D." from contig clen))
             (when (> to clen)
               (oops "TO, ~D, bigger than ~A's size ~D." to contig clen))
             (list from to (circseg-length from to clen))
             ))
          (:plus 
           (let ((from (second decoded-pattern))
                 (extent (min clen (third decoded-pattern))))
             (when (> from clen) 
               (oops "POSITION ~D bigger than ~A's size ~D." from contig clen))
             (list from (circ+ from (1- extent) clen) extent)
             ))
          (:minus 
           (let ((to (second decoded-pattern))
                 (extent (min clen (third decoded-pattern))))
             (when (> to clen) 
               (oops "POSITION ~D bigger than ~A's size ~D." to contig clen))
             (list (circ- to (1- extent) clen) to extent)
             ))
          (:number 
           (let ((from (second decoded-pattern))
                 (extent (svs-extent my-svs))
                 (default-extent (min clen *default-nucleotides-to-display*)))
             (if extent 
                 (setq extent (min clen extent))
               (setq extent default-extent))
             (when (> from clen) 
               (oops "POSITION ~D bigger than ~A's size ~D." from contig clen))
             (list from (circ+ from (1- extent) clen) extent)
             ))
          (:gene 
           (let ((g (second decoded-pattern)))
             (unless (eq (#^contiguous-sequence g) contig)
               (oops "Gene ~A is not found on contig ~A" g contig))
             (let* ((from (#^from g))
                    (to (#^to g))
                    (cols (svs-columns my-svs))
                    (pad (min clen (* 2 cols)))
                    ;; make sure extent is no bigger than current contig
                    (extent 
                     (min 
                      clen 
                      (or (svs-extent my-svs) *default-nucleotides-to-display*)
                      ))
                    ;; Add two lines of padding above gene
                    (pfrom (circ- from pad clen))
                    ;; Adjust for degenerate cases
                    (padded-from 
                     ;; If gene does not wrap...
                     (if (< from to) 
                         ;; if padding location is on other side of 1,
                         ;; make padding location start of contig (1),
                         ;; regardless of whether contig is circular.
                         (if (>= pfrom from) 1 pfrom)
                       ;; If gene does wrap make sure padding has not
                       ;; wrapped all the way around to back inside the
                       ;; gene.  If so, make padding start be beyond
                       ;; end of gene
                       (if (or (> pfrom from) (< pfrom to)) (1+ to) pfrom)
                       ))
                    ;; now add the extend amount to our padded-from location.
                    (pto (circ+ padded-from (1- extent) clen))
                    ;; Adjust for degenerate cases
                    (padded-to 
                     ;; If the extent is less than even the padding,
                     ;; make the extent the end of the gene (TO)
                     (if (<= extent pad)
                         to
                       ;; If gene does not wrap...
                       (if (< from to) 
                           ;; if the padded extent lies somewhere in 
                           ;; the gene, make it extend
                           ;; to the end of the gene (TO)
                           ;; if the padded extent wrapped then clamp
                           ;; the extent at the end of the contig
                           ;; otherwise we are fine
                           (cond
                            ((and (>= pto from) (< pto to)) to)
                            ((< pto from) clen)
                            (t pto)
                            )
                         ;; If gene does wrap, if the padded extent lies
                         ;; somewhere in the gene, extend it to TO
                         (if (or (> pto from) (< pto to)) to pto)
                         ))))
               (list padded-from padded-to 
                     (circseg-length padded-from padded-to clen))
               )))
          (:unrecognized 
           (oops "Cannot understand '~A' !" (second decoded-pattern)))
          (otherwise
           (error "Internal Biobike error in decoded-pattern->from-and-to")
           ))))))
         
       

(defun actually-a-gene? (s)
  (flet ((string-names-gene-frame? (x) 
           (let ((f (frames::frame-fnamed x)))
             (and f (eq (#^organism-entity-type f) #$gene) f)
             )))
           
    (cond
     ((position #\. s) (string-names-gene-frame? s))
     (t
      (loop for org in (bio::loaded-organisms) 
            as prefix = (#^organism-prefix org)
            as name = (s+ prefix s)
            do 
            (vwhen (f (string-names-gene-frame? name))
              (return f)
              ))))))
          
(defun org-to-menu-item (org)
  ;; make sure the organism prefix is a valid nickname.
  ;; If not, use the first nickname of the organism.  
  (let* ((prefix (#^organism-prefix org))
         (item (subseq prefix 0 (1- (length prefix))))
         (nicknames (#^nicknames org)))
    (if (org-string-to-org-frame item 'organism-predicate)
        item
      (if (null nicknames)
          (fname org)
        (let* ((lengths (mapcar (lambda (x) (length (string x))) nicknames))
               (minlen (reduce 'min lengths))
               (minpos (position minlen lengths)))
          (string (nth minpos nicknames))
          )))))

;; Leave around as example of inserting a menu with links
#+obsolete
(defun new-html-for-organism-and-contig-menu ()
  (html
    ((:ul :class "visible")
     :newline
     ((:li :class "visible-item")
      :newline
      ;; we can insert a style background-color here if we wish
      (:a ((:font :color "brown") "Organism:")) 
      :newline
      ((:ul :class "level1")
       (loop 
        for org in (sort (bio::loaded-organisms) 
                         (lambda (x y) (string-lessp (fname x) (fname y))))
        as org-menu-item = (org-to-menu-item org)
        for j from 1 to 40
        do
        (html 
         :newline
         ((:li :class "level1-item")
          :newline
          ((:a :href "#"  :style "text-decoration:none"
            :onclick 
            (formatn "{setOrgBox('~A'); clearContigBox(); clearGotoBox()}"
                     org-menu-item)
            ) 
           (:princ-safe org-menu-item) 
           )
          :newline
          ((:ul :class "level2")
           (loop
            for contig in (#^contiguous-sequences org)
            as contig-menu-item = (#^fname contig)
            for i from 1 to 40
            do 
            (html 
             :newline
             ((:li :class "level2-item")
              ((:a :href "#" :style "text-decoration:none"
                :onclick 
                (formatn 
                 "{ setOrgBox('~A'); setContigBox('~A'); clearGotoBox()}"
                 org-menu-item contig-menu-item))
               "&nbsp;" (:princ-safe contig-menu-item) "&nbsp;"
               ))))))))))))
    )

;; yyy
(defun html-for-current-organisms-contig-menu (orgf)
  (if (null orgf) 
      (html (:princ "&nbsp;&nbsp;Contig: "))
    (html 
     ((:ul :class "visible")
      :newline
      ((:li :class "visible-item")
       :newline
       ;; we can insert a style background-color here if we wish
       (:a ((:font :color "brown") "Contig:"))
       :newline
       ((:ul :class "level1")
        (loop
         for contig in (#^contiguous-sequences orgf)
         as contig-menu-item = (#^fname contig)
         do 
         (html 
          :newline
          ((:li :class "level1-item")
           :newline
           ((:a :href "#"  :style "text-decoration:none"
             :onclick 
             (formatn "{setContigBox('~A'); clearGotoBox()}"
                      contig-menu-item)
             ) 
            (:princ-safe contig-menu-item) 
            ))))))))))

#||

A check box, "[[Search]] Entire Contig".

If selected, the code will perform a search of the pattern over the
entire contig and store the matches (converted to contig coordinates).
The search will then display starting at the first match, going for at
least the entire match but otherwise the default display size.

At this point links would appear, next to START, PREV, NEXT, END,
labelled NEXT MATCH and PREV MATCH.

These would remain until the user a) entered a different search
pattern and unchecked the 'Entire Contig' box, or b) changed the
Contig and unchecked the 'Entire Contig' box.

Clicking on NEXT MATCH displays the match after the one that had been
previously displayed at the top of the viewing area, and PREV MATCH
the match before the one that had been previously displayed.  State is
kept as to which match was the one previously displayed, and a list of
the locations of all the matches in the contig is also kept as state.
The search string is also preserved.

The logic is tricky.

We display NEXT MATCH and PREV MATCH iff we have entire contig
search state (being the list of match boundaries and the current
pointer).

The 'Search Entire Contig' box never stays checked.

If NEXT MATCH or PREV MATCH is clicked, then
   go to the appropriate match and display the matches in the display
   range (we must preserve the search string!), and update the state.

If the CONTIG changes, then the search state gets wiped out.
  If the 'Search Entire Contig' box is still checked, AND 
     the search box has content, then do the entire contig search
     and re-populate the entire contig search state.
  Else do the normal algorithm.

IF the SEARCH string contents is different than the saved entire-contig
   search string (but the CONTIG has not changed), AND
   the cntent is non-null, then
   the search state gets wiped out.
   If the 'Search Entire Contig' box is still checked, then
      do the entire contig search
   Else do the normal algorithm

Otherwise, do the normal algorithm, but do NOT wipe out he entire contig
search state.

    

There is a problem with a match across the contig boundary if the
contig is circular.  This can be hacked by taking a 'sufficently
long' substring on either side of the boundary and doing the search
on it, and seeing any of the match coordinates fall across the
original boundary.  If so, add them to the original list.
Probably ignore this for now.

||#

(publish 
 :path wb::*sseqview-orgs-and-contigs-url*
 :content-type cl-user::*html-publish-content-type*
 :function
 (lambda (req ent)
   (let* ((input (request-query req))
          (package-name (url-parameter-value :pkg input))
          (package-symbol (keywordize package-name))
          )
     (wb::execute-with-standard-weblistener-environment
      req ent package-symbol
      (lambda () 
        (sseqview-orgs-and-contigs input)
        )))))

(defun sseqview-orgs-and-contigs (input)
  (let ((letter (url-parameter-value :letter input))
        ;; list of lists each sublist of the form 
        ;; (letter number-of-orgs number-of-contigs first-org last-org orgs)
        (letter-info (alphabet-organism-info bio::*loaded-organisms*)))
    (unless letter (setq letter (string (caar letter-info))))
    (html
     (:head
      (:title
       (:princ-safe 
        (s+ (machine-name-letter-for-title) " Org/contig selector"))))
     ((:body :bgcolor "#abcdef")
      ((:div :id "colossal" :style colossal-style)
       (:div
        ((:img :style "float:right; padding-right:14px;" 
          :src "/weblistenerdocs/double-helix.jpg"
          :width "125px" :height "125px"))
        (:center (:h1 "Sequence Viewer Contiguous Sequence Selector"))
        ((:font :color "green" :size "4")
         #.(one-string
            "&nbsp;&nbsp;To view a sequence, click on an organism "
            "or a contiguous sequence below."))
        :br :br
        (generate-orgs-and-contigs-selector letter-info)
        :br :br :hr 
        )
       :newline :newline
       ((:div :style bottom1-style)
        :newline
        ((:div :style bottom2-style)
         :newline
         ((:div :style bottom3-style)
          :newline
          ((:div :style bottommain-style2)
           :newline
           ((:ul :style "list-style:none;") 
            :newline
            (let ((letter-list 
                   (sort 
                    (sixth 
                     (find-if 
                      (lambda (letter-list) 
                        (char-equal (first letter-list) (char letter 0)))
                      letter-info
                      ))
                    'string-lessp
                    :key #^fname
                    )))
              (loop 
               for org in letter-list
               as prefix = (#^organism-prefix org)
               do 
               (labels 
                   ((strip-prefix (cname) (subseq cname (length prefix)))
                    (html-for-org-and-contig () 
                      (html 
                       ((:a :style "text-decoration:none; color:blue; "
                         :href 
                         (formatn "sseqview-url?pkg=~A&org=~A"
                                  wb::*sessionid* (#^fname org)))
                        (:b (:princ-safe (#^fname org))))
                       :br :newline
                       (loop 
                        for c in (#^contiguous-sequences org) do
                        (html 
                         "&nbsp;&nbsp;"
                         ((:a :style "text-decoration:none; color:brown; " 
                           :title 
                           (formatn "Size: ~A, Number of genes: ~A"
                                    (#^sequence-length c)
                                    (length (#^genes-sorted-by-position c)))
                           :href 
                           (formatn "sseqview-url?pkg=~A&contig=~A"
                                    wb::*sessionid* (#^fname c)))
                          (:princ-safe 
                           (strip-prefix (#^fname c))))
                         "&nbsp;"
                         :newline
                         )))))
                 (html (:li (html-for-org-and-contig)))
                 )
               (html :br :newline)
               ))))))))))))

(defun alphabet-organism-info (orgs)
  (flet ((first-char-of-org (org) (char (#^fname org) 0)))
    (let ((letter-lists
           (sort 
            (mapcar 
             'third
             (group-by-type 
              orgs 
              :key #'first-char-of-org
              :test 'equalp
              ))
            'char-lessp 
            :key (lambda (orglist) (first-char-of-org (first orglist)))
            )))
      (mapcar 
       (lambda (org-list-for-letter)
         ;; (letter number-of-orgs number-of-contigs first-org last-org orgs)
         (setq 
          org-list-for-letter
          (sort org-list-for-letter 'string-lessp :key #^fname))
         (list 
          (first-char-of-org (first org-list-for-letter))
          (length org-list-for-letter)
          (reduce 
           '+ org-list-for-letter 
           :key (lambda (org) (length (#^contiguous-sequences org)))
           )
          (first org-list-for-letter)
          (lastelem org-list-for-letter)
          org-list-for-letter
          ))
       letter-lists
       ))))

(defun generate-orgs-and-contigs-selector (letter-info)
  (html
   (:b "&nbsp;&nbsp;Index: ")
   :newline
   (loop 
    for (letter number-of-orgs nil first-org last-org nil)
    in letter-info
    do
    (flet ((title () 
             (if (> number-of-orgs 1)
                 (formatn "~D organisms, from ~A to ~A."
                          number-of-orgs (fname first-org) (fname last-org))
               (formatn "1 organism, ~A" (fname first-org))
               )))
      (html 
       ((:a 
         :title (title)
         :href
         (utils::url-with-parameters
          wb::*sseqview-orgs-and-contigs-url*
          (format nil "pkg=~A" wb::*sessionid*)
          (format nil "letter=~A" letter)
          ))
        (:princ (string-upcase letter)))
       "&nbsp;"
       :newline
       )))))

(defun alphabet-for-organisms (orgs)
  (sort  
   (remove-duplicates 
    (mapcar (lambda (x) (char (#^fname x) 0)) orgs)
    :test 'char-equal)
   'char-lessp
   ))
   
    