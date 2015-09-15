;;;; -*- mode: Lisp; Syntax: Common-Lisp; Package: bbi; -*-

(in-package :bbi) 

;;; +=========================================================================+
;;; | Copyright (c) 2005 JP Massar, Jeff Elhai, Mark Slupesky, Peter Seibel   |
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

;;; Author: JP Massar. 

(defun bbload (filespec &key 
                        (verbose *load-verbose*) 
                        (print *load-print*) 
                        (if-does-not-exist t)
                        &aux
                        file-pathname
                        (unique (cons nil nil)))
                        
  #.(one-string-nl
     "BBLOAD is the BBL equivalent to the lisp LOAD function."
     "In BBL there is no concept of a binary (.fasl) file; files of"
     "bbl code are never compiled using COMPILE-FILE.  BBL files"
     "are, in effect, incrementally compiled by bbload; it reads in forms"
     "from FILESPEC one by one, processes them for bbl syntax (such as"
     "[]), calls COMPILE on each processed form, and then evaluates the"
     "resulting closure, (much as the weblistener does for each form"
     "a user types in.)")

  (when (streamp filespec) 
    (error "bbload does not support using streams to load from."))
  (setq file-pathname (utils::webpath->lisp-physical-pathname filespec))
  (when (null (probe-file file-pathname))
    (if (null if-does-not-exist)
        (return-from bbload nil)
      (error 
       (one-string-nl
        "The file argument, ~S," 
        "given to BBLOAD and (possibly) translated as"
        "~S,"
        "either does not exist or is not accessible to the biobike system."
        "Perhaps you misspelled the name or the syntax you used for the"
        "file path is not correct; please consult a system administrator or"
        "teaching assistant if you cannot resolve this yourself.")
       filespec (namestring file-pathname)
       )))

  (when verbose (cformatt "BBL load of ~S beginning..." filespec))

  (progn

    (macrolet 
        ((timeit (form)
           `(progn 
              (cformatt "")
              (cformatt 
               "Timing ~A ..." (stack-trace-form-start-string ',form 50))
              (prog1 (time ,form) (cformatt ""))
              )))

      (with-standard-toplevel-bbl-bindings

        (with-open-file (p file-pathname :direction :input)
          (loop 
           with *readtable* = *bbl-readtable* 
           with *package* = *package* 
           with *load-pathname* = file-pathname
           with *load-truename* = (truename file-pathname)
           with source-file-namestring = (namestring file-pathname)
           with *bbload-in-progress?* = t
           for form = (read p nil unique)
           until (eq form unique)
           do 
           (let ((excl::*source-pathname* source-file-namestring))
             (declare (special excl::*source-pathname*))
             (when print 
               (cformatt "< ~A" (stack-trace-form-start-string form 60))))
           (let* ((form-without-brackets (progn (bracket-hacking form)))
                  (codewalked-form 
                   (progn (bbl-form-processor form-without-brackets)))
                  (compiled-lambda-form 
                   (compile-codewalked-form codewalked-form)
                   )
                  (result-form 
                   (funcall-codewalked-form-function compiled-lambda-form)
                   ))
             (when print 
               (cformatt "> ~A" (stack-trace-form-start-string result-form 60))
               )))))))

  (when verbose (cformatt "BBL load of ~S ended successfully." filespec))

  t)




(defun compile-codewalked-form (form) (compile nil `(lambda () ,form)))

(defun funcall-codewalked-form-function (function)
  (let ((saved-warnings nil))
    (flet ((compiler-warning-handler 
                   (c)
             (let ((r (find-restart 'muffle-warning c)))
               (unless r 
                 (error 
                  "Internal error. Should be a restart for muffle warning! ~A"
                  c
                  ))
               (when r (push c saved-warnings) (invoke-restart r))
               )))
      (handler-bind ((warning #'compiler-warning-handler))
        (prog1 
            (funcall function)
          (loop for w in (reverse saved-warnings)
            as warning-text = (format nil "~A" w)
            do
            (case (bbl-compiler-warning-action w warning-text)
              ((:excise :delete :remove) nil)
              ((t nil :print) (format t "Warning: ~A~%" warning-text))
              (otherwise nil)
              ))
          )))))

(defun bbl-compiler-warning-action (w wt)
  (declare (ignore w))
  (setq wt (substitute #\Space #\Newline wt)) 
  (cond 
   ((search "is now being defined at the top level" wt :test 'string-equal)
    :delete)
   (t :print)
   ))
              

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun bbcl (filespec &rest goo) (apply 'bbcompileload filespec goo))

(defun bbcompileload
       (filespec &key 
                 (verbose *load-verbose*) 
                 (print *load-print*) 
                 (if-does-not-exist t)
                 (force cl-user::*force-load-system*-recompile*)
                 &aux
                 file-pathname
                 lisp-file-pathname 
                 (unique (cons nil nil)))

  (block exit 
  
    (setq file-pathname (utils::webpath->lisp-physical-pathname filespec))
    (when (null (probe-file file-pathname))
      (if (null if-does-not-exist)
          (progn
            (when verbose
              (cformatt 
               ";; BBCL: File ~A does not exist... No compilation performed."
               file-pathname
               ))
          (return-from exit nil))
        (error 
         (one-string-nl
          "The file argument, ~S," 
          "given to BBCOMPILELOAD and (possibly) translated as"
          "~S,"
          "either does not exist or is not accessible to the biobike system."
          "Perhaps you misspelled the name or the syntax you used for the"
          "file path is not correct; please consult a system administrator or"
          "teaching assistant if you cannot resolve this yourself.")
         filespec (namestring file-pathname)
         )))

    (when verbose 
      (cformatt "BBCL: compile/load of ~A beginning..." filespec))
  
    (setq lisp-file-pathname 
          (merge-pathnames 
           (make-pathname :type cl-user::*lisp-extension*)
           file-pathname 
           ))

    ;; If a corresponding lisp file exists and it was created after the 
    ;; bbl file, then just compile/load the lisp file.  Otherwise proceed
    ;; to compiling the BBL file into a lisp file.  

    (when (not force)
      (when (probe-file lisp-file-pathname)
        (let ((bbl-fwd (file-write-date file-pathname))
              (lisp-fwd (file-write-date lisp-file-pathname)))
          (when (> lisp-fwd bbl-fwd) 
            (when verbose 
              (cformatt "BBCL: Compile/Loading existing lisp file..."))
            (return-from exit 
              (let ((*bbload-in-progress?* t)) (cl lisp-file-pathname)))
            ))))

    (when verbose 
      (cformatt "BBCL: Translating ~A to .lisp file..." filespec))

    (with-standard-toplevel-bbl-bindings

      (with-open-file (p file-pathname :direction :input)
       
        (handler-case 
           
            (with-open-file 
                (q lisp-file-pathname 
                   :direction :output :if-exists :supersede
                   :if-does-not-exist :create)
              (loop 
               with *readtable* = *bbl-readtable* 
               with *package* = *package*
               with *print-length* = nil
               with *print-level* = nil
               with *print-lines* = nil
               with *load-pathname* = file-pathname
               with *load-truename* = (truename file-pathname)
               with *bbload-in-progress?* = t
               with first? = t
               for form = (read p nil unique)
               until (eq form unique)
               do 
       
               (when print 
                 (cformatt "< ~A" (stack-trace-form-start-string form 60)))
       
               (if first? 
           
                   ;; We require the first form to be 
                   ;; an in-package form so that 
                   ;; we can determine what package 
                   ;; to put ourselves in to compile
                   ;; the rest of the file.  Any subsequent
                   ;; in-package forms are
                   ;; ignored for the moment.  
                   ;; Perhaps we should catch and trap them.
                   (progn
                     (unless (and (listp form) 
                                  (symbol= :in-package (first form)))
                       (error 
                        "To compile BBL, the 1st form must be IN-PACKAGE!"))
                     (let ((package-name (second form)))
                       (unless (and (symbolp package-name)
                                    (find-package package-name))
                         (error 
                          "Invalid package: ~A, cannot compile!" package-name))
                       (setq *package* (find-package package-name))
                       (format q "~S~%" form)
                       (setq first? nil)
                       ))
         
                 (let* ((form-without-brackets
                         (progn (bracket-hacking form)))
                        (codewalked-form 
                         (bbcl-maybe-codewalk-toplevel-form 
                          form-without-brackets))
                        (gensym-hacked-form 
                         (progn (replace-gensyms-in-code codewalked-form)))
                        )
                   ;; Turn off (ref x 3) being printed as x[3]
                   ;; if in BBL mode.  See readtable.lisp.
                   (let ((*print-pprint-dispatch* 
                          *biolisp-print-pprint-dispatch*))
                     (terpri q)
                     (pprint gensym-hacked-form q)
                     )))))
         
          (error 
           (c)
           (when (probe-file lisp-file-pathname)
             (delete-file lisp-file-pathname))
           (error c)
           ))))
    
    (let ((*bbload-in-progress?* t)) 
      (when verbose
        (cformatt 
         "BBCL: Compile/loading generated lisp file ~A"
         lisp-file-pathname
         ))
      (cl lisp-file-pathname))

    (when verbose 
      (cformatt "BBCL: Compile/load of ~S ended successfully." filespec))

    t
  
    ))

(defun bbcl-maybe-codewalk-toplevel-form (form)
  (cond
   ((null (listp form)) form)
   ((< (length form) 2) form)
   ((eq 'bbi::define-function (first form)) form)
 ; ((eq 'bbi::define-function-mv (first form)) form)
   ((eq 'in-package (first form)) form)
   ((member (first form) '(define-df-syntactic-tokens declaim proclaim)) form)
   ((member (first form) '(define-macro defmacro)) form)
   ((eq 'defun (first form)) (bbcl-form-processor form))
   ((member (first form) '(defconstant defvar defparameter))
    (if (null (third form)) 
        form
      `(,(first form) 
        ,(second form) 
        ,(bbl-form-processor (third form))
        ,@(cdddr form)
        )))
   ((member (first form) '(bbi::define)) 
    `(,(first form) 
      ,(second form)
      ,(third form)
      ,(bbl-form-processor (fourth form))
      ))
   ((eq 'progn (first form)) 
    `(progn (mapcar 'bbcl-maybe-codewalk-toplevel-form (rest form))))
   (t 
    (warn "Non-toplevel form found in .bike file: ~A" form)
    (bbl-form-processor form)
    )))

(defun gensym? (obj) 
  (and (symbolp obj) (null (symbol-package obj))))

(defun replace-gensyms-in-code (code)
  (let ((ghash (make-hash-table :test 'eq))
        (needs-gensyms-replaced? nil))
    (labels ((code-requires-gensym-replace? (form) 
               (unless (eq (first form) 'quote)
                 (multiple-value-bind (length type)
                     (length-circular-or-dotted? form)
                   (declare (ignore length))
                   (ecase type 
                     ((:dotted :circular) 
                      (error "~S list found in file! (~A ...)"
                             type (first form)))
                     (:proper 
                      (loop for sublist on form 
                            as obj = (first sublist)
                            do
                            (cond 
                             ((gensym? obj) (setq needs-gensyms-replaced? t))
                             ((consp obj) (code-requires-gensym-replace? obj))
                             (t nil)
                             )))))))
             (find-and-replace-in (form) 
               (unless (eq (first form) 'quote)
                 (loop for sublist on form 
                       as obj = (first sublist)
                       do
                       (cond 
                        ((gensym? obj)
                         (let ((replacement (gethash obj ghash)))
                           (unless replacement 
                             (setq 
                              replacement (intern (symbol-name obj) :$$))
                             (setf (gethash obj ghash) replacement)
                             )
                           (setf (first sublist) replacement)
                           ))
                        ((consp obj) (find-and-replace-in obj))
                        (t nil)
                        )))))
      (if (consp code)
          (progn
            (code-requires-gensym-replace? code)
            (if (null needs-gensyms-replaced?)
                code
              (let ((code-copy (copy-tree code)))
                (find-and-replace-in code-copy)
                code-copy
                )))
        code
        ))))

