;;; -*- mode: Lisp; Syntax: Common-Lisp; Package: user; -*-

(in-package :cl-user)

;;; +=========================================================================+
;;; | Copyright (c) 2002, 2003, 2004 JP Massar, Jeff Shrager, Mike Travers    |
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

;;; Author:  JP Massar.


(defmacro defbiocfgvar (name doc value)
  `(defcfgvar ,name ,value 
             :belongs-to :biolingua
             :parameter-type :standard
             :doc ,doc
             ))

(defmacro defbiocfgdir (name doc value)
  `(defcfgvar ,name ,value 
             :belongs-to :biolingua
             :parameter-type :directory
             :doc ,doc           
             ))

(defbiocfgdir
 *biotools-directory*
 "Directory where various executables (tools) BioLingua might wish to use are."
 (ecase (os?)
   (:unix "/usr/local/biotools/")
   (:windows "C:\\Biomisc\\Biotools\\")
   ))

(defbiocfgdir 
 *bioetc-directory*
 "Directory where miscellaneous non-source files for BioLingua live.
  Knowledge base data files live here, in subdirs of the 'data' subdirectory."
 (ecase (os?)
   (:unix "/usr/local/bioetc/")
   (:windows "C:\\Biomisc\\Bioetc\\")
   ))

(defbiocfgdir
 *instance-bioetc-directory*
 "Directory where miscellaneous files particular to a BioLingua instance
  are.  E.g., Organism data files should be on a per-instance basis, and
  would live here, in subdirs of the 'data' subdirectory."
 *bioetc-directory*)

(defbiocfgvar
 *biolingua-email-address*
 "A From: address to use when user requests mail be sent to him."
 "biolingua@nostoc.stanford.edu")

(defbiocfgvar
 *biowebserver*
 "Determines whether knowledge frames and organism data gets loaded into
  the system at startup.  If this is NIL, the value of *ORGANISMS-TO-LOAD-LIST*
  is irrelevant.  Generally, this would be set to T for user-intended systems
  and to NIL for developers."
  t)

(defbiocfgvar
 *default-bio-database-name*
 "The name of the default MYSQL database opened when querying MYSQL."
 "cyanobacteria")

(defbiocfgvar
 *organisms-descriptor* 
 "A keyword describing the kind of organisms this instance deals with.
  E.g., :cyanobacteria, :pond-scum-viruses, etc."
 :cyanobacteria
 )

(defbiocfgvar
 *organisms-to-load-list* 
 "Organisms that should be pre-loaded when the system starts up.
  If the value is :all all available organisms are loaded."
 '(:syn6803 :nos29133 :ana7120 :ana29413b :ter101))

(defbiocfgvar
 *keep-sequence-streams-open* 
 "If T (the default), loaded organisms keep in their frames an open stream
  to the organism's genome file.  This would fail if there were too many
  organisms (like more than 100?).  If you have that many organisms you should
  set this configuration variable to NIL."  
 t)

(defbiocfgdir
 *biotutorials-directory*
 "Directory where live tutorials for biobike live"
 (ecase (os?)
   (:unix "/usr/local/BioLisp/Doc/livetutorials/") 
   (:windows "C:\\lispcode\\biolisp\\doc\\livetutorials\\")
   ))

;;; You may wish to indicate these by hard-coded paths, or by merger with 
;;; *biotools-directory*, for example:
;;;     (merge-pathnames *biotools-directory* "mytool/bin")

(defbiocfgvar 
 *blast-executable-toplevel-dir* 
 "The directory under which the FORMATDB and BLASTALL executables live."
 (ecase (os?)
   (:unix 
    (concatenate 'string *biotools-directory* "/blast/current_blast/bin/"))
   (:windows nil)
   ))

(defbiocfgvar 
 *clustal-executable-dir*
 "Where the clustal executables live."
 (ecase (os?)
   (:unix (concatenate 'string *biotools-directory* "clustalw/clustalw1.83/"))
   ;; (:unix "/usr/local/biotools/clustalw/clustalw1.83/")
   (:windows nil)
   ))

(defbiocfgvar 
 *R-executable-dir*
 "Where the R executables live."
 (ecase (os?)
   (:unix (concatenate 'string *biotools-directory* "R/R-2.1.1/bin/"))
   ;; (:unix "/usr/local/biotools/R/R-2.1.1/bin/")
   (:windows nil)
   ))

(defbiocfgvar 
  *phylip-executable-dir*
  "Where the phylip executables live."
 (ecase (os?)
   (:unix (concatenate 'string *biotools-directory* "phylip/current/exe/"))
   ;; (:unix "/usr/local/biotools/phylip/current/exe/")
   (:windows nil)
   ))

(defbiocfgvar 
 *meme-executable-dir*
 "Where the meme executables live."
 (ecase (os?)
   (:unix (concatenate 'string *biotools-directory* "meme/meme_3.5.7/bin/"))
   #+notyet
   ;; This is for version 4.8.1.  For some reason the default install
   ;; makes a bin directory that doesn't distinguish by named version.  
   (:unix (concatenate 'string *biotools-directory* "meme/bin/"))
   (:windows nil)
   ))

(defbiocfgvar 
 *meme-version* 
 "The version of meme we are using"
 (ecase (os?)
   (:unix "3.5.7")
   #+notyet 
   (:unix "4.8.1")
   (:windows nil)
   ))

;;; *** Note for RNAz

;; The RNAz executable directory is computed using 
;; (external-executable-path "RNAz")
;; which uses the *biotools-directory* path 
;; so the RNAz executable or a link to it must be placed in
;; .../RNAz/current/bin/
;; where 'current' is generally a symbolic link to the current RNAz version

(defbiocfgvar 
 *gnuplot-path*
 "Where the GNUPlot executable(s) live."
 (ecase (os?)
   (:unix 
    (concatenate
     'string *biotools-directory* "gnuplot/gnuplot-4.0.0/src/gnuplot"))
   (:windows nil)
   ))

(defbiocfgvar
 *pdftotext-path*
 "Where the pdftotext executable lives."
 (ecase (os?)
   (:unix 
    (concatenate
     'string *biotools-directory* "poppler/poppler-0.16.7/utils/pdftotext"))
   (:windows nil)
   ))

(defbiocfgvar
 *ppthtml-path*
 "Where the ppthtml executable lives."
 (ecase (os?)
   (:unix "/usr/bin/ppthtml")
   (:windows nil)
   ))

(defbiocfgvar 
  *default-dotty-font*
  "The font that dotty-based graph visualization tools (e.g., seegraph) use."
 (ecase (os?)
   (:unix "courier")
   (:windows nil)
   ))

(defbiocfgvar
 *repl-mode* 
 "The evaluation mode the REPL uses: currently either :BBL or :Biolisp"
 :biolisp)

(defbiocfgvar 
 *include-bbl*
 "Whether BBL and related goo gets loaded."
 t)

(defbiocfgvar 
 *code-creation-mode*
 "Whether the user gets a Weblistener or a VPL window initially by default."
 :weblistener
 )

(defbiocfgvar 
 *vpl-version*
 "The version of the VPL to use"
 2)

(defbiocfgvar 
 *blast-lookup-database*
 "What protocol, if any, to use to lookup protein-protein blast queries"
 NIL)


;;;;  *****> IF YOU ADD A CONFIGURATION VARIABLE, ADD AN EXAMPLE INTO
;;;;  *****> .../biolisp/sample-config.lisp TOO.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun create-biolingua-logical-pathname-translations ()
  
  (create-simple-logical-pathname-host 
   "biol" '*application-directory*
   :nil-ok? nil :directory-must-be-writeable? nil)
  (create-simple-logical-pathname-host 
   "bioetc" '*bioetc-directory*
   :nil-ok? nil :directory-must-be-writeable? nil)
  (create-simple-logical-pathname-host 
   "biotools" '*biotools-directory*
   :nil-ok? nil :directory-must-be-writeable? :never)
  
  (terpri)
  )

(defun configure-biolingua ()
  (create-biolingua-logical-pathname-translations)
  (when cl-user:*biowebserver* (push :biowebserver *features*)))

(configure-biolingua)

(display-configuration-variables :biolingua)

(display-simple-lp-translations)

(terpri)
(force-output)


