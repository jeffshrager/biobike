;;; -*- mode: Lisp; Syntax: Common-Lisp; Package: weblistener; -*-

;;; Author:  JP Massar. 

(in-package :weblistener)

(tests:define-chapter 
 :utils 
 :package :weblistener
 :directory #.(cl-user::translate-simple-lp "websrc:Webtests;")
 :files ("utils-tests.lisp")
 )

(ecase cl-user:*frame-system-version*
  ((:old :mt :sframes)
   (tests:define-chapter 
    :frames
    :package :weblistener
    :directory #.(cl-user::translate-simple-lp "websrc:Webtests;")
    :files ("frames-tests.lisp")
    ))
  (:new
   (tests:define-chapter 
    :frames
    :package :weblistener
    :directory #.(cl-user::translate-simple-lp "websrc:Webtests;")
    :files ("af-frames-tests.lisp")
    )))   

(tests:define-chapter
 :arrayops
 :package :utils
 :directory #.(cl-user::translate-simple-lp "websrc:Webtests;")
 :files ("array-tests.lisp")
 )

(tests:define-chapter
 :garrays
 :package :utils
 :directory #.(cl-user::translate-simple-lp "websrc:Webtests;")
 :files ("garray-tests.lisp")
 )

(defun run-weblistener-tests (&key (verbose t))
  (tests:run-chapter-tests :utils :verbose verbose)
  (tests:run-chapter-tests :frames :verbose verbose)
  (tests:run-chapter-tests :arrayops :verbose verbose)
  (tests:run-chapter-tests :garrays :verbose verbose)
  )
