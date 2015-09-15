;;; -*- mode: Lisp; Syntax: Common-Lisp; Package: biolisp; -*-

;;; Author:  JP Massar. 

(in-package :biolisp)

(tests:define-chapter 
 :td
 :package :bioutils
 :directory #.(cl-user::translate-simple-lp "biol:Biotests;")
 :files ("table-data-tests.lisp")
 )

(tests:define-chapter 
 :loop
 :package :bio
 :directory #.(cl-user::translate-simple-lp "biol:Biotests;")
 :files ("loop-tests.lisp")
 )

(tests:define-chapter 
 :fasta
 :package :bioutils
 :directory #.(cl-user::translate-simple-lp "biol:Biotests;")
 :files ("fasta-tests.lisp")
 )

(tests:define-chapter 
 :bioutils
 :package :bio
 :directory #.(cl-user::translate-simple-lp "biol:Biotests;")
 :files ("bioutils-tests.lisp")
 )

(tests:define-chapter 
 :organisms
 :package :bio
 :directory #.(cl-user::translate-simple-lp "biol:Biotests;")
 :files ("organism-tests.lisp")
 )

(tests:define-chapter 
 :biotools
 :package :bio
 :directory #.(cl-user::translate-simple-lp "biol:Biotests;")
 :files ("biotools-tests.lisp")
 )



(defvar *auxiliary-code-loaded* nil)

(defun load-auxiliary-biolisp-code ()
  (unless *auxiliary-code-loaded*
    (format t "~&~%;; Loading auxiliary biolisp code~%") (force-output t)
    (cl-user::requires :Matching "biol:Matching;load.lisp")
    (c/l "biol:Testing;match-tests.lisp")
    (setq *auxiliary-code-loaded* t)
    ))

(defun run-biolisp-tests (&key (verbose t)) 
  (format t "~&~%;; Running all Biolisp tests~%") (force-output t)
  (load-auxiliary-biolisp-code)
  (tests:run-chapter-tests :td :verbose verbose)
  (tests:run-chapter-tests :fasta :verbose verbose)
  (tests:run-chapter-tests :loop :verbose verbose)
  )

(defun run-startup-biolisp-tests (&key (verbose t))
  (format t "~&~%;; Running all Startup Biolisp tests~%") (force-output t)
  (tests:run-chapter-tests :td :verbose verbose)
  (tests:run-chapter-tests :loop :verbose verbose)
  (tests:run-chapter-tests :bioutils :verbose verbose)
  (when (and user::*biowebserver* 
             (equal "cyanobacteria" cl-user:*default-mysql-database-name*)
             (>= (length *loaded-organisms*) 5))
    (tests:run-chapter-tests :organisms :verbose verbose)
    (if (zerop (mod (get-universal-time) 4))
        (tests:run-chapter-tests :biotools :verbose verbose)
      (cformatt "Biotools tests not being run..."))
    ))

(defpackage :sseqview-test-user 
  (:nicknames :ssvu)
  (:use :bbl :tests)
  (:import-from :utils :cl :c/l)
  )

(defmacro ssvu::defssvtest= (name &body body)
  `(tests:deftest ,name ,(lisp:first body) ,@(cdr body) 
                  :chapter :sseqview :comparison 'equal))

(tests::define-chapter 
 :sseqview
 :package :ssvu
 :directory #.(cl-user:translate-simple-lp "biol:Biotests;")
 :files ("sseqview-tests.lisp")
 :before-code
 ;; make sure we don't inadvertently create #$markvirus !
 (let ((mv (frames::frame-fnamed "markvirus")))
   (if mv
       (cformatt "Markvirus found in system, proceeding...")
     (progn
       (cformatt "Markvirus not loaded, loading organism...")
       (setq 
        mv
        (load-organism 
         "markvirus" :private? t 
         :dir (cl-user:translate-simple-lp "biol:Biotests;markvirus;")))))
   (if (#^organism-loaded? mv) 
       (cformatt "Markvirus loaded, proceeding...")
     (error "Error loading Markvirus.  Organism frame exists but not loaded.")
     )
   (if (not (member mv bio::*loaded-organisms*))
       (progn
         (cformatt "Markvirus not a member of *loaded-organisms*...adding...")
         (push mv bio::*loaded-organisms*)
         (cformatt "Added, ready to run seqview tests")
         )
     (cformatt "Ready to run sseqview tests...")
     )
   (cformatt "Beginning seqview tests...")
   (cformatt "Making sure weblistener is running...")
   (unless wb::*current-weblistener-port* (wb::sw))
   (wb::system-dummy-users)
   )
 :after-code
 (let ((mv (frames::frame-fnamed "markvirus")))
   (when (member mv bio::*loaded-organisms*)
     (cformatt "Removing Markvirus from *loaded-organisms*")
     (setq bio::*loaded-organisms* (remove mv bio::*loaded-organisms*))
     (cformatt "Done")
     (bio::unload-organism mv)
     (frames::unintern-frame mv)
     ))
   
 )
     
   