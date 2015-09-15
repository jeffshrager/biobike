;;; -*- Package: cl-user; mode: lisp; base: 10; Syntax: Common-Lisp; -*-

#+:allegro
(LISP:UNLESS (LISP:EQ EXCL:*CURRENT-CASE-MODE* :CASE-INSENSITIVE-UPPER)
  (ERROR "*CURRENT-CASE-MODE must be :CASE-INSENSITIVE-UPPER !!"))

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

(push :new-tests *features*)

;;; Temporary to find GC problem
#-:allegro-v8.2
(proclaim '(optimize (safety 3) (debug 3) (speed 0)))

;;; Used later to create a list of all packages created by BioBike
(defparameter *startup-packages* (list-all-packages))
(defparameter *biobike-packages* nil)
(defparameter *all-non-user-packages* nil)

(defvar *recompile-biolisp* nil)
(defvar *recompile-bbl* nil)

;;; These two should already be defined by the config file,
;;; but if one is loading for testing sometimes there is no config.

(defvar *application* :biolingua)
(defvar *application-instance* :biolingua)

;;; This is not a configuration variable but needs to be defined somewhere...

(defvar *ai* nil)

(eval-when  (:compile-toplevel :load-toplevel :execute)
  (export '(*application* *application-instance* *ai*) :cl-user)
  )


;;; Load all the Weblistener stuff.

(load (merge-pathnames "webload.lisp" *load-truename*) :verbose t)


;;;; Load BioLingua.

(maybe-force-all *recompile-biolisp*)

;; Load Biolingua definitions.

(requires :Biodefs "appsrc:Biodefs;load.lisp")


;;; Create and load SOAP interface to KEGG and Seed.
;;; The interface files are created in the Biolisp directory.

(requires :soap-tools "appsrc:soap;load.lisp")
(keggapi)

;; Make a class/object for the application/instance being run.

(wb::make-application-instance-class-and-object)

;; Load standard methods for BioLingua.

(requires :Biomethods "biol:Biomethods;load.lisp")

;; Load everything else related to BioLingua.

;; (requires :Datadefs "biol:data;load.lisp")
(requires :Bioutils "biol:Bioutils;load.lisp")

(ecase *frame-system-version*
  (:old
   (requires :Kdbs "biol:Kdbs;load.lisp")
   (requires :Go "biol:Gocode;load.lisp")
   (requires :KEGG "biol:KEGG;load.lisp")
   (requires :ocelot "biol:Ocelot;load.lisp"))
  (:sframes 
   (requires :KEGG "biol:KEGG;load.lisp")
   ;; shut compiler up
   (defvar bio::*go-frames* nil)
   )

  #||  
  (:new
   (requires :Kdbs "biol:Kdbs;load.lisp")
   (requires :Go "biol:Gocode;load.lisp")
   (requires :ocelot "biol:Ocelot;load.lisp")
   (requires :KEGG "biol:KEGG;load.lisp")
   )
||#

  )

(ecase *frame-system-version* 
  ((:old :sframes) 
   (requires :Organisms "biol:Organisms;load.lisp"))

#||
  (:new
   (requires :Organisms "biol:Organisms;af-load.lisp"))
||#

  )


(case *organisms-descriptor*
  (:seed-organisms (requires :seedorgs "biol:seedorgs;load.lisp"))
  (:otherwise nil)
  )


(utils:ld "biol:Lib;load.lisp")  

(maybe-unforce-all *recompile-biolisp*)

;; BBL stuff 
(when cl-user::*include-bbl* 

  (maybe-force-all *recompile-bbl*)

  (requires :bbl-implementation "biol:BBL;load.lisp")
  (requires :bbl "biol:bike;load.lisp")

  (ecase *frame-system-version* 
    ((:old :mt :sframes)
     (requires :MICROARRAYS "biol:MICROARRAYS;load.lisp"))
    (:new
     (requires :MICROARRAYS "biol:MICROARRAYS;load.lisp")))

  (ecase cl-user::*vpl-version* 
    (2 (requires :server-vpl "biol:vplcode;load.lisp")))
  
  (maybe-unforce-all *recompile-bbl*)

  )

(requires :data-editor "biol:data-editor;load")
   

(maybe-force-all *recompile-biolisp*)

(requires :Documentation-definitions "biol:Doc;load")

;; load documentation forms specific to biolisp

(requires :biodf (translate-simple-lp "biol:Doc;biodf;load.lisp"))
(requires :externaldf (translate-simple-lp "biol:Doc;externaldf;load.lisp"))

(when cl-user::*include-bbl*

  ;; load documentation forms specific to bbl

  (requires :bbldf (translate-simple-lp "biol:Doc;bbldf;load.lisp"))

  )

(help::help-init)

;; Load specific tests for biolingua

(requires :biotests "biol:Biotests;load.lisp")
(when cl-user::*include-bbl*
  (requires :bbltests "biol:Bbltests;load.lisp"))

(maybe-unforce-all *recompile-biolisp*)

;; Run initializations for BioLingua instance.

(format t ";; Running application initializations based on ~S~%" cl-user::*ai*)
(wb::application-initializations cl-user:*ai*)


#-:sframes
(when cl-user::*include-bbl*
  (utils::C/L (MERGE-PATHNAMES "kegg-load-function.lisp" *load-pathname*))
  (LOAD (MERGE-PATHNAMES "run-load-kegg-ids.lisp" *load-pathname*))
  )

;;; Run biolingua tests

(progn
  (let ((test-symbol (find-symbol "*RUN-BIOWEBLISTENER-TESTS*" :cl-user)))
    (when (or (null test-symbol) (symbol-value test-symbol))
      (wb::run-weblistener-tests :verbose nil)
      ;; to run more tests (fasta tests), call (run-biolisp-tests)
      (bio::run-startup-biolisp-tests :verbose nil)
      (when cl-user::*include-bbl*
        (utils::forward-package-funcall :bbi :run-bbl-tests :verbose nil)
        ))))

;;; Change allegro parameter for GC'ing as per Franz advice 05/20/05
#+:allegro
(setf (sys:gsgc-switch :gc-old-before-expand) t)

(setq *biobike-packages* 
  (set-difference (list-all-packages) *startup-packages*))
(setq *all-non-user-packages* (list-all-packages))

