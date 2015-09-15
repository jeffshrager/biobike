;;; -*- Package: cl-user; mode: lisp; base: 10; Syntax: Common-Lisp; -*-

(in-package :cl-user)


(defparameter *ppcre-subdir* "cl-ppcre-0.5.9/")

;;; Bootstrap.lisp loads in the microloader.

(load (merge-pathnames "bootstrap.lisp" *load-truename*) :verbose t)

;;; Load libraries of utility functions we will need.

(load 
 (merge-pathnames
  (concatenate 'string "ppcre/" *ppcre-subdir* "load.lisp") 
  *load-truename*)
 :verbose t)

(load (merge-pathnames "Utils/load.lisp" *load-truename*) :verbose t)
(load (merge-pathnames "Frames/jp-load.lisp" *load-truename*) :verbose t)
(load (merge-pathnames "Dbaccess/load.lisp" *load-truename*) :verbose t)

