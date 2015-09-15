;;; -*- Package: help; mode: lisp; base: 10; Syntax: Common-Lisp; -*-

(in-package :help)

;;; +=========================================================================+
;;; | Copyright (c) 2002-2006 JP Massar, Jeff Shrager, Peter Seibel           |
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

;;; Authors:  JP Massar.

#||

Every file in the "externaldf" directory is either
  
  1) of a type listed in *extensions-to-extract-text-from*
  2) of a text file type
  3) a subdirectory named "textfiles"  
  3) otherwise it is ignored

If it's a text file type then it should have a corresponding file of
a type listed in *extensions-to-extract-text-from*; it there is no
corresponding file a warning should be issued.

On startup the directory is processed:

  1. any extractable type file that needs to have its corresponding
text file created or updated is processed to do so.
  2. FILE-DOCUMENTATION docobj's are updated or created for every 
extractable file and for all text files in the textfiles subdir.  
There should be no files other than .txt files in the textfiles subdir.

When searching for single word matches, a new search on FILE-DOCUMENTATION
objects is to be added.  

If there is content in the text slot of the object, it is either a
string or a list of strings.  The string or each string is searched
for a match. If there is no content in the text slot, then

  1) if the object is located in the externaldf directory the
corresponding text file is searched (read it in using file-to-string-list
and search each line)

  2) if the object is located in the textfiles subdirectory it itself
is searched in the same manner.

  3) otherwise it is ignored.

A match is to be reported with context -- a few words on either side
of the match on the line the match occurs.  Possibly the number of
matches in the document as well.

||#


(defvar *externaldf-dir* (translate-simple-lp "websrc:Doc;externaldf;"))

(defparameter *extensions-to-extract-text-from* 
  '(("pdf" extract-text-from-pdf)
    ("ppt" extract-text-from-ppt)
    ("pps" extract-text-from-ppt)
    ))

(defparameter *doc-directories-to-extract-text-from*
  (list 
   (list "externaldf" (translate-simple-lp "websrc:Doc;externaldf;"))
   (list "vpl-tutorial" (translate-simple-lp "websrc:vpl;tutorial;"))
   ))

(defparameter *new-extensions-to-extract-text-from* 
  '(("pdf" extract-text-from-pdf)
    ("ppt" extract-text-from-ppt)
    ("pps" extract-text-from-ppt)
    ("txt" identity)
    ("htm" extract-text-from-html)
    ("html" extract-text-from-html)
    ))

(defun existing-file-documentation-object? (file)
  (find-documentation (namestring file) 'documentation-file))

(defun process-externaldf-directories ()
  (cformatt "Processing other docobj directories...")
  (loop for (subdir-unix subdir-windows) in 
        *doc-directories-to-extract-text-from*
        do
        (new-process-externaldf-directory 
         (ecase (user::os?) (:unix subdir-unix) (:windows subdir-windows)) 
         )))

(defun process-externaldf-directory ()
  (let* ((extensions (mapcar 'first *extensions-to-extract-text-from*))
         (listing (directory-with-subdirs-in-directory-form *externaldf-dir*))
         (files (remove-if 'pathname-names-directory? listing))
         (dirs (remove-if-not 'pathname-names-directory? listing))
         (dirnames (mapcar (lambda (x) (lastelem (pathname-directory x))) dirs))
         (textfiles-found? nil)
         )
    ;; Check that the 'textfiles' subdirectory exists or create it if
    ;; it doesn't.  No other subdirectories should exist.  
    (loop for dirname in dirnames do
          (if (string-equal "textfiles" dirname) 
              (setq textfiles-found? t)
            (unless (string-equal "CVS" dirname)
              (cformatt "Unknown directory named ~A found in ~A!"
                        dirname *externaldf-dir*))))
    (unless textfiles-found? 
      (cformatt "No 'textfiles' directory in ~A.  One will be created." 
                *externaldf-dir*)
      (ensure-directories-exist 
       (merge-pathnames "textfiles/foo.lisp" *externaldf-dir*)))
    ;; Find all the files that are supposed 
    ;; to have text extracted from them in the directory.  Any other files
    ;; are ignored.  
    (multiple-value-bind (text-extraction-files ignored-files)
        (separate-into-lists 
         files 
         (lambda (x) (member (pathname-type x) extensions :test 'string-equal))
         )
      (declare (ignore ignored-files))
      ;; (print (list 'te-files text-extraction-files))
      ;; (print (list 'ignored ignored-files))
      ;; For each file that is supposed to have text extracted from it, 
      ;; see if the corresponding text file already exists and is current. 
      ;; If not, create or recreate it.  Also create a file-documentation
      ;; docobj for this file, or if it already exists, merge the information.  
      (loop for file in text-extraction-files
            as name = (file-namestring file)
            as type = (pathname-type file)
            as data = 
            (find type *extensions-to-extract-text-from* 
                  :test 'string-equal :key 'first)
            as conversion-function = (second data)
            as associated-text-file = (pathname-of-new-type file "txt")
            do
            (cond
             ((null (probe-file associated-text-file))
              (generate-txt-file file conversion-function))
             (t 
              (let ((source-write-date (file-write-date file))
                    (txt-write-date (file-write-date associated-text-file)))
                (when (> source-write-date txt-write-date)
                  (generate-txt-file file conversion-function)
                  ))))
            (let* ((filepath (namestring file))
                   (docobj 
                    (existing-file-documentation-object? name)))
              (if docobj 
                  (merge-externaldf-file-with-docobj 
                   docobj filepath associated-text-file)
                (create-new-associated-text-file-documentation-object 
                 name filepath associated-text-file
                 ))))
      ;; For any standalone text file in the textfiles subdirectory 
      ;; that doesn't already have a docobj, create one. 
      (let* ((listing (directory-with-subdirs-in-directory-form 
                       (merge-pathnames "textfiles/" *externaldf-dir*)))
             (text-files 
              (remove-if 
               (lambda (x) 
                 (or (pathname-names-directory? x) 
                     (and 
                      (not (string-equal "txt" (pathname-type x)))
                      (not (string-equal "html" (pathname-type x))))))
               listing
               )))
        ;; (print (list 'listing listing))
        ;; (print (list 'text-files text-files))
        (loop for file in text-files 
              as name = (file-namestring file)
              as filepath = (namestring file)
              do
              (unless (existing-file-documentation-object? name)
                (cformatt "Creating docobj for ~A" filepath)
                (create-new-raw-text-file-documentation-object name filepath)
                )))))) 

(defun new-process-externaldf-directory (dir)

  (cformatt "Processing external docobj directory ~A" dir)

  (let* ((extensions (mapcar 'first *extensions-to-extract-text-from*))
         (docdir (translate-simple-lp "websrc:Doc;"))
         (dirpath 
          (ecase (user::os?)
            (:linux (append-subdir docdir dir))
            (:windows dir)
            ))
         (textfiles-dirpath (append-subdir dirpath "textfiles"))
         (listing (directory-with-subdirs-in-directory-form dirpath))
         (files (remove-if 'pathname-names-directory? listing))
         )

    ;; Create the textfiles subdirectory if it does not exist.
    (ensure-directories-exist (merge-pathnames "foo.txt" textfiles-dirpath))

    ;; Find all the files that are supposed 
    ;; to have text extracted from them in the directory.  
    ;; Any other files are ignored.  
    ;; Extract the text and create the associated text file
    ;; in a subdirectory of this directory called textfiles
    (multiple-value-bind (text-extraction-files ignored-files)
        (separate-into-lists 
         files 
         (lambda (x) 
           (let ((type (pathname-type x)))
             (member type extensions :test 'string-equal)
             )))
      (declare (ignore ignored-files))
      ;; For each file that is supposed to have text extracted from it, 
      ;; see if the corresponding text file already exists and is current. 
      ;; If not, create or recreate it.  Also create a file-documentation
      ;; docobj for this file, or if it already exists, merge the information.  
      ;; If the file is a text file, don't bother creating a corresponding file.
      ;; The file itself will be its own associated text file.
      (loop for file in text-extraction-files
            as name = (file-namestring file)
            as type = (pathname-type file)
            as text? = (string-equal type "txt")
            as data = 
            (find type *extensions-to-extract-text-from* 
                  :test 'string-equal :key 'first)
            as associated-text-file = nil
            as conversion-function = nil
            do
            (if text?
                (setq associated-text-file file)
              (progn
                (setq conversion-function (second data))
                (setq associated-text-file
                      (merge-pathnames
                       (file-namestring (pathname-of-new-type file "txt"))
                       textfiles-dirpath
                       ))
                (cond
                 ((null (probe-file associated-text-file))
                  (generate-txt-file file conversion-function))
                 (t 
                  (let ((source-write-date (file-write-date file))
                        (txt-write-date (file-write-date associated-text-file)))
                    (when (> source-write-date txt-write-date)
                      (generate-txt-file file conversion-function)
                      ))))))
            (let* ((filepath (namestring file))
                   (docobj 
                    (existing-file-documentation-object? name)))
              (if docobj 
                  (merge-externaldf-file-with-docobj 
                   docobj filepath associated-text-file)
                (progn
                  (cformatt "Creating docobj for ~A" file)
                  (create-new-associated-text-file-documentation-object 
                   name filepath associated-text-file
                   ))))))

    ))




(defun create-default-txt-file (txt-file)
  (with-open-file (p txt-file :direction :output :if-exists :supersede)
    (format p "abracadabra xyzzy~%plugh plover~%")
    ))

(defun generate-txt-file (source-file conversion-function)
  (let ((txt-file (pathname-of-new-type source-file "txt")))
    (if (or (null conversion-function) 
            (and (eq (wb::os?) :windows)
                 (not (eq conversion-function 'extract-text-from-html))
                 ))
        (create-default-txt-file txt-file)
      (funcall conversion-function source-file txt-file)
      ))
  nil
  )

(defun extract-text-from-pdf (source-file text-file)
  (cformatt "Extracting text from ~A into ~A" source-file text-file)
  (let ((pdftotext-path 
         (symbol-value-in-package 
          "*PDFTOTEXT-PATH*" :cl-user 
          :if-does-not-exist :create
          :if-unbound :return 
          :if-unbound-value nil
          )))
    (cond
     ((stringp pdftotext-path)
      (protected-shell-command 
       (s+ 
        pdftotext-path
        " -raw "
        source-file " " 
        text-file
        )))
     ((null pdftotext-path) (create-default-txt-file text-file))
     (t (error "Internal error.  user::*pdftotext-path* must be defined!"))
     )))

(defun extract-text-from-ppt (source-file text-file)
  (cformatt "Extracting text from ~A into ~A" source-file text-file)
  (let ((ppthtml-path 
         (symbol-value-in-package 
          "*PPTHTML-PATH*" :cl-user 
          :if-does-not-exist :create
          :if-unbound :return 
          :if-unbound-value nil
          )))
    (cond
     ((stringp ppthtml-path)
      (protected-shell-command 
       (s+ 
        ppthtml-path " "
        source-file " > " 
        text-file
        )))
     ((null ppthtml-path) (create-default-txt-file text-file))
     (t (error "Internal error.  user::*ppthtml-path* must be defined!"))
     )))
            
(defun extract-text-from-html (source-file text-file)
  (cformatt "Extracting text from ~A into ~A" source-file text-file)
  (copy-text-file source-file text-file)
  )

(defun merge-externaldf-file-with-docobj (docobj filepath associated-text-file)
  ;; (print (list 'merge filepath))
  (when (text docobj)
    (cformatt 
     (one-string
      "Warn: Docobj associated with ~A has text but is now "
      "being given an associated text file, ~A.  Text is being removed.")
     filepath associated-text-file
     ))
  (setf (text docobj) nil)
  (setf (source-file docobj) filepath)
  (setf (associated-text-file docobj) associated-text-file)
  (unless (display-modes docobj) (setf (display-modes docobj) '(:all)))
  )

(defun create-new-raw-text-file-documentation-object (name filepath)
  (let ((docobj (intern-documentation name 'documentation-file)))
    (setf (source-file docobj) filepath)
    (setf (display-modes docobj) '(:all))
    docobj
    ))

(defun create-new-associated-text-file-documentation-object 
       (name filepath associated-text-file)
  (let ((docobj (intern-documentation name 'documentation-file)))
    (setf (source-file docobj) filepath)
    (setf (associated-text-file docobj) associated-text-file)
    (setf (display-modes docobj) '(:all))
    docobj
    ))

              
                      

