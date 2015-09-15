;; -*- Package: cl-user; mode: lisp; base: 10; Syntax: Common-Lisp; -*-

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

;;; Author:  JP Massar

(defvar *df-autoload-mode* nil)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *bootstrap-symbols*
    '(
      os?
      *force-requires-reload*
      *force-load-system*-recompile*
      *requires-modules*
      requires
      provides
      unprovide
      force-all
      dirpath-from-path
      file-in-loading-file-dir
      in
      add-simple-lp-host
      delete-simple-lp-host
      translate-simple-lp
      load-system*
      compile/load
      ))
  (defparameter *bootstrap-symbols-to-be-compiled* 
    (append *bootstrap-symbols* nil))
  (export *bootstrap-symbols* (find-package :cl-user)))

;;; The (block nil ... (return ...)) funny business makes it
;;; possible to use #+ as a CASE form with an error condition at
;;; the end, so that we don't just fall through and up with
;;; strange errors later on in the load process.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun os? ()
    (block nil
      #+(AND :ALLEGRO :MSWINDOWS) 
      (return :windows)
      #+(AND :ALLEGRO :UNIX) 
      (return :unix)
      #+(AND :LISPWORKS :WIN32)
      (return :windows)
      ;; XXX Why shouldn't we just check for OS, why implementation as well?
      #+(AND :SBCL :UNIX)
      (return :unix)
      #+MCL 
      (return :mac)
      #+CORMANLISP
      (return :windows)
      ;; error condition
      (error
       "*** OS? UNSUPPORTED COMBINATION OF OS AND LISP VERSION ***")
      )))

(unless (fboundp 'in)
  (defmacro in (package-designator) `(in-package ,package-designator)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun one-string-nl (&rest strings)
    (apply 'concatenate 'string 
           (mapcar 
            (lambda (x) (concatenate 'string x (string #\Newline)))
            strings
            ))))

;;;   -- Provide a portable way to get a path containing directory
;;;      information, but not file, type or version information.  
;;;      DIRPATH-FROM-PATH

(defun dirpath-from-path (path)
  (make-pathname
   :host (pathname-host path)
   :device (pathname-device path)
   :directory (pathname-directory path)
   :name nil
   :type nil
   :version nil))

(defun file-in-loading-file-dir (file)
  (merge-pathnames file *load-truename*))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;; SIMPLE LOGICAL PATHNAME FACILITY

(defvar *simple-lp-hosts* nil)

(defun display-simple-lp-translations ()
  (if (null *simple-lp-hosts*)
      (format t "~%;; NO SIMPLE LP TRANSLATIONS DEFINED.~%")
    (progn
      (terpri)
      (format t ";; SIMPLE LP HOST    TRANSLATION~%")
      (terpri)
      (loop for (host trans) in *simple-lp-hosts* do
            (format t ";; ~13A    ~S~%" host trans))
      (terpri)
      )))

(defun canonicalize-simple-lp-translation (translation)
  (substitute #\/ #\\ translation))

(defun add-simple-lp-host (host translation &key (if-exists :replace))
  #.(one-string-nl
     "Define a 'simple' logical pathname host and its 'simple' translation."
     "The translation must be an absolute pathname.  The host should be"
     "a string with only alphanumeric characters.")
  (setq host (string host))
  (when (simple-lp-translation host)
    (ecase if-exists
      (:error (error "Host ~S already exists!" host))
      (:warn (warn "Host ~S already exists!" host))
      ((:replace nil) nil)
      ))
  (delete-simple-lp-host host)
  (push (list host (canonicalize-simple-lp-translation translation))
        *simple-lp-hosts*)) 

(defun delete-simple-lp-host (host)
  #.(one-string-nl
     "Delete a 'simple' logical pathname host from the 'simple' logical"
     "pathname host table.")
  (setq *simple-lp-hosts* 
        (delete host *simple-lp-hosts* :test 'string-equal :key 'first)))

(defun simple-lp-translation (host)
  (second (find (string host) *simple-lp-hosts*
          :test 'string-equal :key 'first)))


(defun translate-simple-lp (slp)
  #.(one-string-nl
     "Translate a 'simple' logical pathname into a standard pathname."
     "SLP must be a string or a pathname. (If a pathname, nothing is done)."
     "If SLP has no ':' in it, then no attempt at translation is made,"
     "and SLP is returned directly (converted to a pathname)."
     "If SLP does have a ':' everything before the ':' is interpreted"
     "as the host.  If no such host has been defined"
     "(using ADD-SIMPLE-LP-HOST) then an error is signalled."
     "If the host is defined, then it is translated, while the path"
     "after the host in SLP has any ';' characters translated to '/'"
     "characters.  The translated host and the converted path are appended;"
     "finally, PATHNAME is called on the result."
     "Example: "
     "(add-simple-lp-host \"foo\" \"C:/lispcode/\")"
     "(translate-simple-lp \"foo:biolisp;x.lisp\")"
     "->"
     "\"c:/lispcode/biolisp/x.lisp\"")
  (cond
   ((pathnamep slp) slp)
   (t
    (let ((pos (position #\: slp)))
      (if pos 
          (let* ((host (subseq slp 0 pos))
                 (path (if (= pos (1- (length slp))) "" (subseq slp (1+ pos))))
                 (translation (simple-lp-translation host)))
            (cond 
             (translation 
              (pathname 
               (concatenate 'string translation (substitute #\/ #\; path))))
             ((eq (os?) :windows) 
              (if (and (= (length host) 1) (alpha-char-p (char host 0)))
                  (pathname slp)
                (error "No simple lp host defined for ~S" host)))
             (t 
              (error "No simple lp host defined for ~S" host)
              )))
        (pathname slp))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;   -- Provide a REQUIRE/PROVIDE mechanism via REQUIRES/PROVIDES
;;;      that demands a file argument (instead of it being an option,
;;;      as with REQUIRE) and a way to force that file to be reloaded
;;;      via *FORCE-REQUIRES-RELOAD*.


(defvar *force-requires-reload* nil)

(defvar *requires-modules* nil)

(defun requires 
       (module-name filepath 
                    &key 
                    (verbose nil)
                    (auto-provide? nil)
                    (warn t)
                    (compile-load? nil)
                    &allow-other-keys)
  (setq filepath (path->lisp-physical-pathname filepath))
  (cond
   ((member module-name *requires-modules*)
    (when *force-requires-reload* 
      (when verbose (format t "~&;; Reloading module ~A~%" module-name))
      (if compile-load? (compile/load filepath) (load filepath))))
   (t 
    (when verbose (format t "~&;; Loading module ~A~%" module-name))
    (if compile-load? (compile/load filepath) (load filepath))))
  (when auto-provide? (provides module-name))
  (if (not (member module-name *requires-modules*))
      (progn
        (when warn
          (warn "REQUIRES caused ~A to be loaded for module ~A, ~A"
                filepath module-name 
                "but that module is still not recorded as having been provided"
                ))
        nil)
    module-name
    ))

(defun provides (module-name) 
  (pushnew module-name *requires-modules* :test #'eql)
  module-name)

(defun unprovide (module-name)
  (cond
   ((eq module-name :all)
    (setq *requires-modules* nil) t) 
   ((null (member module-name *requires-modules*)) nil)
   (t (setq *requires-modules* 
            (delete module-name *requires-modules* :test #'eql))
      module-name
      )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Micro-loader for Lisp systems

;; Copyright 1996 Michael Travers

;; Author: Michael Travers (mt@media.mit.edu)
;; Modified by JP Massar for BioLingua 2003-2004.

;; The smallest possible system loader.  Example of use:

;; (load-system 
;;  "c:/Lispcode/Mysource/"
;;  '("file-1"
;;    "file-2"
;;    ...
;;   ))

(defvar *force-load-system*-recompile* nil)

(defun path->lisp-physical-pathname (path)
  "Converts any legitimate path (including a slp) to a physical pathname."
  (handler-case
      (cl-user:translate-simple-lp path)
    (error () (translate-logical-pathname path))))

;;; load-system is taken in ACL, and causes problems
(defun load-system* (base-path file-list)
  (setq base-path (namestring (path->lisp-physical-pathname base-path)))
  (with-compilation-unit ()
    (mapc (lambda (file)
            (let ((full-file (concatenate 'string base-path file)))
              (compile/load full-file)))
          file-list)))


;;; Different conventions (also sometimes CL in Allegro-land, sigh)
;;; This will go to simply "lisp" for all implementations once we standardize.

(defparameter *lisp-extension* 
    (block nil
      #+(OR MCL :ALLEGRO :LISPWORKS :SBCL :CORMANLISP) (return "lisp")
      (error "Unknown Lisp.  Need to specify a source extension.")))

;;; Corman Lisp does not work correctly when source files are
;;; turned into binary files (sometimes).  (But Corman compiles all the
;;; forms that it loads, so it is not necessary to COMPILE-FILE source
;;; files.)

(defun generate-derived-file? (source-file derived-file)
  (or (not (probe-file derived-file))
      (> (file-write-date source-file) 
         (or (file-write-date derived-file) 0)
         )))

(defun compile/load 
       (file &optional (recompile? *force-load-system*-recompile*))
  #+:CORMANLISP
  (load (source-file* file) :verbose t)
  #-CORMANLISP
  (let* ((source (source-file* file))
         (fasl (fasl-file file))
         (filepath (pathname file))
         (type (pathname-type filepath)))
    (when (and type (not (string-equal *lisp-extension* type)))
      (error "Cannot compile file with type ~S" type))
    (when (or recompile? (generate-derived-file? source fasl))
      (compile-file source :output-file fasl #+:SBCL :verbose #+:SBCL nil))
    (load fasl)))

(defun change-file-type (file type)
  #+:CORMANLISP
  ;; We are working around a bug in Corman's MAKE-PATHNAME 
  ;; function here.  MAKE-PATHNAME insists on a real pathname as 
  ;; the argument to :defaults, and it cannot be a logical pathname.
  (let ((p (pathname file)))
    (if (lp::logical-pathnamep p)
	(change-file-type (translate-logical-pathname file) type)
      (make-pathname :defaults p :type type)
      ))
  #-:CORMANLISP
  (make-pathname :defaults (pathname file) :type type))

(defun source-file* (file)
  (change-file-type file *lisp-extension*))

(defun fasl-file (file)
  (let ((fasl-extension
	 (block nil
	   #+(AND MCL POWERPC) (return "pfsl")
	   #+(AND MCL (NOT POWERPC)) (return "fasl")
	   #+:ALLEGRO (return "fasl")
	   #+:SBCL (return "fasl")
	   #+:LISPWORKS (return "fsl")
	   #+:CORMANLISP (return "fasl")
	   (error "Unknown Lisp.  Need to specify a binary extension.")
	   )))
    (change-file-type file fasl-extension)
    ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Executing (FORCE-ALL) causes all files processed with COMPILE/LOAD
;;; to be (re)compiled and all system load files being processed by REQUIRES
;;; to be reloaded.  

(when (not (fboundp 'force-all))
  (defun force-all (&key (undo nil) (verbose? t))
    (when verbose?                         
      (format t "~&;; Control variables: ~A,~A~%" 
              'cl-user::*force-requires-reload*
              'cl-user::*force-load-system*-recompile*
              ))
    (if (not undo)
        (progn
          (set 'cl-user::*force-requires-reload* t)
          (set 'cl-user::*force-load-system*-recompile* t)
          (when verbose? 
            (format t ";; REQUIRES will always reload module file now.~%")
            (format t ";; LOAD-SYSTEM* will recompile all component files.~%")))
      (progn
        (set 'cl-user::*force-requires-reload* nil)
        (set 'cl-user::*force-load-system*-recompile* nil)
        (when verbose?
          (format t ";; REQUIRES won't reload module file if already loaded.~%")
          (format t ";; LOAD-SYSTEM* won't recompile files unless necessary.~%")
          )))))

;;; Mechanism to save state of recompilation variables so that
;;; user can call (maybe-force-all ...) and (maybe-unforce-all ...)
;;; in a pair, and after the second call the recompilation variables
;;; are reset to what they were before the first call.  

;;; This allows us to force a recompilation of a 
;;; subset of the system; it does not allow us to NOT
;;; recompile a set if the recompilation variables are globally
;;; set to force compilation

(let ((frr nil) (flsr nil))      
  (defun maybe-force-all (flag)
    (when flag 
      (setq frr cl-user::*force-requires-reload*)
      (setq flsr cl-user::*force-load-system*-recompile*)
      (setq cl-user::*force-requires-reload* t
            cl-user::*force-load-system*-recompile* t)))
  (defun maybe-unforce-all (flag)
    (when flag 
      (setq cl-user::*force-requires-reload* frr)
      (setq cl-user::*force-load-system*-recompile* flsr))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(mapcar 
 (lambda (x) 
   (when (and (fboundp x) (not (compiled-function-p (symbol-function x))))
     (compile x)
     ))
 *bootstrap-symbols-to-be-compiled*)
 

       
  
