;;; -*- Package: cl-user; mode: lisp; base: 10; Syntax: Common-Lisp; -*-

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

;;; Bootstrap.lisp loads in the microloader.

(load (merge-pathnames "bootstrap.lisp" *load-truename*) :verbose t)

;;; Load the Weblistener system

(defvar *recompile-weblistener* nil)

(maybe-force-all *recompile-weblistener*)

;;; Load configuration parameters and the logical pathname translations
;;; from the Webconfig subdirectory.

(load (merge-pathnames
       (make-pathname
        :host nil 
        :device nil 
        :directory '(:relative "Webconfig")
        :name "load"
        :type "lisp"
        :version nil)
       *load-truename*
       ))

;;; Load libraries of utility functions we will need.

;;; PPCRE is included in the 1.2.35 version of Portableaserve
;;; that we are using with lispworks4.4

#-:lispworks4.4
(requires :Ppcre "ppcre:load" :warn nil :autoprovide? t)

#+:lispworks4.4
(progn
  (defparameter *portableaserve-path* 
    (ecase (os?)
      (:windows "C:/Lispcode/portableaserve-1.2.35/portableaserve/")
      (:unix (error "Need to have PortableAserve for Lispworks"))
      ))
  (load (concatenate 'string *portableaserve-path* "install.lisp"))
  )

;; Prepare third party libs to be loaded. Loads asdf.lisp if necessary
;; and registers the directories containing the .asd files of
;; third-party libs in asdf:*central-registry*. It seems like this
;; ought to go in a more general load file such as blload but it can't
;; because the logical pathname translations aren't defined until this
;; file. Anyway, for the moment all the code that uses these libs is
;; loaded below here so this is fine.

(requires :third-party "appsrc:ThirdParty;load.lisp")

(requires :Wlisp "websrc:Wlisp;load")

(requires :Utils "websrc:Utils;load")

(ecase *frame-system-version*
  (:old 
   (requires :Frames "websrc:Frames;load")
   (pushnew :weblistener-frames *features*))
  (:new 
   (requires :Nframes "websrc:Nframes;load")
   (pushnew :weblistener-aframes *features*))
  (:mt 
   (load "C:/jpm/mtfs/3rdwheel/jp-load.lisp")
   (pushnew :weblistener-frames *features*)
   )
  (:sframes
   (requires :Sframes "websrc:Sframes;load")
   (pushnew :Sframes *features*)
   ))


(requires :Dbaccess "websrc:Dbaccess;load")

;;; Load package definition stuff, portability interfaces, etc.

(requires :Webdefs "websrc:Webdefs;load")

(requires :Help "websrc:Help;load")

(requires :Portability "websrc:Portability;load")
(requires :Scripts "websrc:Scripts;load")

;; Load the basic Weblistener

(requires :Weblistener "websrc:Weblisten;load")
(requires :Webuser "websrc:Webuser;load")

;; load documentation for weblistener stuff

(requires :webdf "websrc:Doc;webdf;load")

;; Load code for Ajax server

(requires :Ajax "websrc:Ajax;load") 

;; Load Slime code (for SWANK server)

;; Suppress warnings about swank interfaces not being implemented.  
(with-output-to-string (s)
  (let ((*error-output* s))
    (requires :Slime "websrc:Slime;load")
    ))

;; Load test mechanism.
(requires :Test-mechanism "websrc:Testing;load.lisp")

;; Load basic weblistener tests 

(requires :Webtests "websrc:Webtests;load.lisp")

(maybe-unforce-all *recompile-weblistener*)
