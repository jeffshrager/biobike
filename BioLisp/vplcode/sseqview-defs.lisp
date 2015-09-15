BioBIKE Lisp File Listing

Listener BioDocs FindFrames BioFiles LispDocs

/home/biovcu/BioLisp/vplcode/sseqview-defs.lisp  	Download source  	Edit source

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

(defstruct svs 
  org 
  contig
  gene
  from
  to
  extent
  goto
  rows
  columns
  font-size 
  gene-names
  search
  errors 
  notes
  search-note
  search-display?
  invert?  ; **************
  double-stranded? ; ****************
  translate? ; ****************
  invert-sequence? ;;Michael Tse
  search-entire-next/prev
  ;; these next two constitute the 'entire contig' match state 
  match-locations
  match-counter
  match-string
  )
  
;;setf(svs-invert-sequence? t) ;;Michael Tse

(defstruct svline
  num 
  segments
  links
  genes 
  new-genes 
  text
  )

(defstruct svsegment
  start
  end
  color 
  genes
  pattern-match?
  )

(defmethod out-record-to-html
           ((obj wb::svs) (string string) &key (pkg nil))
  (declare (ignore pkg))
  (html 
   :br
   ((:a :href (create-url-from-svs obj) :target "_blank")
    (:princ-safe "Sequence viewer link"))))

(defun create-url-from-svs (value)
  (let ((args (list (format nil "pkg=~a" wb::*sessionid*)))
        (from (wb::svs-from value))
        (to (wb::svs-to value)))
    (when (wb::svs-org value)
      (push (format nil "org=~a" (#^fname (wb::svs-org value))) args))
    (when (wb::svs-contig value)
      (push (format nil "contig=~a" (#^fname (wb::svs-contig value))) args))
    (cond 
     ((and from to) (push (formatn "goto=~d,~d" from to) args))
     (from (push (formatn "goto=~d" from) args))
     (to (push (formatn "goto=~d,~d" 1 to) args))
     )
    (when (wb::svs-rows value)
      (push (format nil "rows=~a" (wb::svs-rows value)) args))
    (when (wb::svs-columns value)
      (push (format nil "columns=~a" (wb::svs-columns value)) args))
    (when (wb::svs-search value)
      (push (format nil "search=~a" (wb::svs-search value)) args))
    (push (format nil "vpl=~a" "t") args)
    (apply 'utils::url-with-parameters 
           wb::*sseqview-url*
           (reverse args)
           )))

(defvar *current-svs* nil)
(defvar *current-linemap* nil)

(defparameter no-genes-color :black)
(defparameter overlap-color :orange)
(defparameter multiple-overlap-color :cyan)

(defparameter *default-nucleotides-to-display* 6000)
(defparameter *max-basepairs-to-display* 120000)
(defparameter *default-sseqview-rows-to-display* 50)
(defparameter *default-sseqview-columns-to-display* 60)

(defparameter background-hex-color "#dddddd")

(defparameter *seqview-forward-gene-colors* '(:red :magenta))
(defparameter *seqview-backward-gene-colors* '(:blue :green))
(defparameter *seqview-overlapping-forward-gene-color* :orange)
(defparameter *seqview-overlapping-backward-gene-color* :violet)
(defparameter *seqview-overlapping-forward-backward-gene-color* :purple)

(defvar *csg*)
(defvar *genemap*)
(defvar *color-table*)
(defvar *initial-segment-map*)
(defvar *match-segments*)
(defvar *sm-segment-map*)
(defvar *final-segment-map*)
(defvar *linemap*)
(defvar *input*)

  

(defparameter colossal-style 
  (one-string
   "bottom: 0px; left: 0px; position: absolute; right: 0px; top: 0px; "
   "margin: 0px -5px 0px -5px; padding: 0px; overflow:hidden;"
   ))

;;; Changed from 107 -> 140
(defparameter topstuff-style 
  (one-string
   "bottom: 0px; left: 0px; margin: 0px; padding: 0px; "
   "position: absolute; right: 0px; top: 0px; "
   "height: 140px; text-align: left; z-index: 100;"
   ))

;;; Changed from 107 -> 165
(defparameter bottom1-style 
  (one-string
   "bottom: 0px; left: 0px; position: absolute; right: 0px; top: 0px; "
   "margin-top: 6pt; top: 165px;"
   ))
(defparameter bottom2-style 
  (one-string
   "bottom: 0px; left: 0px; position: absolute; right: 0px; top: 0px; "
   "bottom: 0%; max-height: 100%"
   ))
(defparameter bottom3-style 
  (one-string
   "bottom: 0px; left: 0px; margin: 3pt; padding: 12pt; "
   "position: absolute; right: 0px; top: 0px;"
   ))
(defparameter bottommain-style 
  (one-string
   "bottom: 0px; left: 0px; position: absolute; right: 0px; "
   "top: 0px; overflow: auto; text-align: center; vertical-align: middle;"
   ))
(defparameter bottommain-style2
  (one-string
   "bottom: 0px; left: 0px; position: absolute; right: 0px; "
   "top: 0px; overflow: auto;"
   ))

(defparameter *sseqview-css-goo* 
  "
<style type=\"text/css\">

/* body {font-family: Verdana, Geneva, Arial; font-size: 12pt;} */

#nav, #nav ul { /* all lists */
	padding: 0;
	margin: 0;
	list-style: none;
	line-height: 1;
}

#nav a {
	display: block;
	width: 10em;
}

#nav li { /* all list items */
	float: right;
	width: 10em; /* width needed or else Opera goes nuts */
}

#nav li ul { /* second-level lists */
	position: absolute;
	/* background: transparent; */
        background-color: #FF7F00;
        text-align: left;
	width: 10em;
	left: -999em; /* using left instead of display to hide menus because display: none isn't read by screen readers */
}

#nav li ul ul { /* third-and-above-level lists */
        margin: -1em 0 0 10em; 
        background-color: orange;
        border: solid 1px #ccc;
        /* width: 200px; */
        min-width: 10em;
        max-width: 30em;
        text-align: left; 
        /*width: 10em;*/

}

#nav li:hover ul ul, #nav li.sfhover ul ul {
	left: -999em;
}

#nav li:hover ul, #nav li li:hover ul, #nav li.sfhover ul, #nav li li.sfhover ul { /* lists nested under hovered list items */
	left: auto;
}

</style>
"
  )

(defparameter *sseqview-js-goo*
"

<script type=\"text/javascript\"><!--//--><![CDATA[//><!--

sfHover = function() {
	var sfEls = document.getElementById(\"nav\").getElementsByTagName(\"LI\");
	for (var i=0; i<sfEls.length; i++) {
		sfEls[i].onmouseover=function() {
			this.className+=\" sfhover\";
		}
		sfEls[i].onmouseout=function() {
			this.className=this.className.replace(new RegExp(\" sfhover\\b\"), \"\");
		}
	}
}

setOrgBox = function(value) {
   setBox(\"org\",value);
}

setContigBox = function(value) {
   setBox(\"contig\",value);
}

setBox = function(boxId,value) {
  var foo = document.getElementById(boxId);
  foo.value = value;
}

clearContigBox = function() {
  var foo = document.getElementById('contig');
  foo.value = '';
}

clearGotoBox = function() {
  var foo = document.getElementById('goto');
  foo.value = '';
}

//--><!]]></script>

if (window.attachEvent) window.attachEvent(\"onload\", sfHover);

")

(defun create-sseqview-css-goo 
       (&key 
        (anchor-hover-color "#995534")
        (level1-background-color "lime")
        (level1-hover-background-color "green")
        (level2-background-color "orange")
        (level2-hover-background-color "#FF7F50")
        )
  (formatn 
   "

 <style type=\"text/css\">
      
    .visible
    { 
    list-style: none; 
    }

    .visible-item 
    {
    font-size: 10pt; 
    }

    a:hover
    {
    color: ~A;
    }

    .level1
    {
        margin-top: 0em;
        padding: 0;
        list-style: none;
	display: none;
	width: 10em; 
        line-height: 0.6;
	text-align: left;
	position: absolute;
	font-size: 11pt;
	font-family: monospace;

    }

    .visible-item:hover .level1
    {
	display: block; 
    }

    .level1-item span
    {
        padding-left: 0em;
    }

    .level1-item
    {
        padding: .4em;
        background: ~A;
    }

    .level1-item:hover { background: ~A; }

    .level2
    {
        margin-top: -20px;
        display: none;
        position: absolute; 
        top: auto; 
	left: 10em;
        padding: 0;
        line-height: 1.2;
        list-style: none;
	background: ~A;
	text-align: left;
	min-width: 5em;
    }

    .level2-item
    {
        padding: .1em .5em; 
    }

    .level2-item a
    {
        display: block;
    }

    /* all immediate li children of objects of class .level2 */
    /* are given a hover action */
    .level2 > li:hover { background: ~A; } 

    .level1-item:hover .level2
    {
        display: block;
    }

    
    </style>

"
   anchor-hover-color 
   level1-background-color 
   level1-hover-background-color 
   level2-background-color 
   level2-hover-background-color 
   ))
   

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun color-to-html-color (color)
  (case color
    (:black "#000000")
    (:red "#FF0000")
    (:magenta "#FF00FF")
    (:blue "#0000FF")
    (:cyan "#00FFFF")
    (:green "#44944A")
    (:lime "#32CD32")
    (:orange "#CC5500")
    (:violet "#EE82EE")
    (:purple "#A020F0")
    (:brown "#A52A2A")
    (otherwise color)
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#||

(defparameter *nsv-search-types*
  '(
    ;; case-insensitive search on all full gene names in the organism
    :gene-name
    ;; simple SEARCH of every gene annotation 
    :annotation-string
    ;; regular expression search of every gene annotation
    :annotation-pattern 
    ;; regular expression search of both strands 
    ;; (forward, reverse and complemenet)
    :dna-sequence-2
    :dna-sequence-forward
    :dna-sequence-backward
    ;; search only within gene boundaries 
    :gene-sequence-2
    :gene-sequence-forward
    :gene-sequence-backward
    ;; search only region between genes
    :intergenic-sequence-2
    :intergenic-sequence-forward
    :intergenic-sequence-backward
    :protein
    :restriction-sequence-2
    :restriction-sequence-forward
    :restriction-sequence-backward
    ))
    
(defstruct nsv-search-spec 
  organism
  ;; if T, search all contigs, otherwise a list of the contigs to search 
  contigs
  ;; proteins, forward vs backward
  type
  ;; what you're looking for
  pattern
  ;; a vector of vectors, #(#(m1 m2) #(m1 m2))
  contig-match-vector
  ;; (contig-index match-index)
  match-pointer
  )

(defun nsv-search-gene-name (nsv)
  (let ((org (nsv-search-spec-org nsv))
        (pattern (nsv-search-spec-pattern nsv))
        (results nil))
    (loop for gene in (#^genes org)
      do
      (let ((search-results (search pattern (#^fname gene))))
        (when search-results (push gene results))
        ))))
          
(defun nsv-search-gene-annotation (nsv)
  (let ((org (nsv-search-spec-org nsv))
        (pattern (nsv-search-spec-pattern nsv))
        (results nil))
    (loop for gene in (#^genes org)
          as annotation = (#^annotation gene)
          do
          (let ((search-results (search pattern annotation)))
            (when search-results (push gene results))
            ))))
          

||#        
      

  