;;; -*- Package: bio; mode: lisp; base: 10; Syntax: Common-Lisp; -*-

(in-package :bio)

;;; +=========================================================================+
;;; | Copyright (c) 2009 JP Massar
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

(defvar *seed-load-broadcast-stream* nil)


;;; pass through list elements to load-seed-organism
;;; new reload? keyword
;;; should return frames of orgs loaded, list elements not loaded
(defun load-seed-organisms 
       (list &key (verbose? *seed-load-verbose*) (commit? t) (reload? nil))
  (let ((*seed-load-verbose* verbose?))
    (let ((gids 
           (mapcar 
            (lambda (x) 
              (cond
               ((stringp x) x)
               ((typep x 'seed-organism) (#^seed-id x))
               (t (error "Don't know how to load ~A" x))
               ))
            list
            )))
      (flet ((doit (x) 
               (load-seed-organism x :commit? commit? :reload? reload?)))
        (loop for id in gids 
              with gids-loaded = nil 
              with gids-not-loaded = nil
              do
              (handler-case 
                  (progn
                    (if verbose? 
                        (doit id)
                      (with-output-to-string (s)
                        (let ((*standard-output* s))
                          (doit id)
                          )))
                    (push id gids-loaded)
                    )
                (error 
                 (c)
                 (push (list id c) gids-not-loaded)
                 ))
              finally
              (flet ((showthem (ids)
                       (loop for id in ids do
                             (cformatt 
                              "Seed organism with ID ~A loaded~A"
                              id (if commit? "/committed" "")
                              )))
                     (bad (ids)
                       (loop for (id c) in ids do
                             (cformatt "Seed organism with ID ~A not loaded" id)
                             (cformatt "  Actual error: ~A" c)
                             )))
                (let ((nl (length gids-loaded))
                      (nnl (length gids-not-loaded)))
                  (if (<= nl 10)
                      (when (plusp nl) (showthem gids-loaded))
                    (progn
                      (showthem (subseq gids-loaded 0 10))
                      (cformatt "  And ~D others also loaded..." (- nl 10))
                      ))
                  (when (plusp nl)
                    (cformatt "Total of ~D organisms loaded successfully." nl))
                  (if (<= nnl 10)
                      (when (plusp nnl) (bad gids-not-loaded))
                    (progn
                      (bad (subseq gids-not-loaded 0 10))
                      (cformatt 
                       "  And ~D others also failed to load..." 
                       (- nnl 10)
                       )))
                  (when (plusp nnl)
                    (cformatt "A total of ~D organisms failed to load..." nnl)
                    ))
                (return (values gids-loaded gids-not-loaded))
                ))))))

;;; canonicalize arg into orgf frame, pass frame around
;;; move annotations into create function
(defun load-seed-organism 
       (gid 
        &key 
        (verbose? *seed-load-verbose*) (commit? t) (reload? nil)
        (private? nil) (dir nil)
        &allow-other-keys 
        )
  (let ((*seed-load-verbose* verbose?))
    (case user::*frame-system-version*
      (:old
       (forward-funcall 
        'load-seed-organism-nsf gid :commit? commit? :reload? reload?))
      (:sframes
       (if user::*master-list-enabled*
           (forward-funcall
            'load-seed-organism-msf gid
            :commit? commit? :reload? reload?
            :private? private? :dir dir
            )
         (forward-funcall 
          'load-seed-organism-sf gid :commit? commit? :reload? reload?
          ))))))

;;;  Build or rebuild the genome or protein sequence files as appropriate

(defun seed-organism-fastas-to-index-and-seq-files 
       (orgf orgn prefix fasta-files 
             &key (rebuild? nil) (data-char-predicate 'identity))
  ;; Turn the fasta files for the contig or proteins of an organisms
  ;; into a single long sequence file with an associated index file.
  (let ((seqdir (organism-seqinfo-dir orgf))
        (seqfile-name (s+ prefix "-" *seqinfo-name*)))
    (handler-case
        (ensure-directories-exist seqdir :verbose t)
      (error
       ()
       (formatn
        (one-string
         "Cannot access or cannot create the directory to store "
         "sequence information for organism ~A. "
         "The inaccessible directory is ~A. ")
        orgn seqdir
        )))
    (let ((seqpath 
           (merge-pathnames 
            (make-pathname :name seqfile-name :type *seqinfo-ext*)
            seqdir)))
      (when (or rebuild? (not (probe-file seqpath)))
        (vformatt "Creating seq files in directory ~A from .fasta file" seqdir)
        ;; Create sequence.seqidx and sequence.seq files
        (fasta-files-to-index-and-seq-files 
         fasta-files seqdir seqfile-name 
         (#^organism-prefix orgf) :verbose? *seed-load-verbose*
         :data-char-predicate data-char-predicate)
        (vformatt "FASTA data stored into SEQ files in directory ~A" seqdir)
        ))))

(defun put-proteins-in-genes-order (orgf)
  (let ((genehash (make-hash-table)))
    (loop for gene in (#^genes orgf) 
          for count from 0 
          do
          (setf (gethash gene genehash) count)
          )
    (let ((protein-sort-list 
           (loop for protein in (#^proteins orgf)
                 as gene = (#^gene protein)
                 as index = (gethash gene genehash)
                 do
                 (unless index 
                   (error "Internal error!  No gene for protein ~A" protein))
                 collect
                 (list protein index)
                 )))
      (mapcar 'first (sort protein-sort-list '< :key 'second))
      )))

(defun test-seed-organism (orgf)
  (declare (ignore orgf))
  nil
  )

(defun populate-organism-frame-slots-from-plist-data (orgf)
  (loop for (key data) in *plist-info* do
        (setf (slotv orgf (frame-fnamed (string key) t)) data)
        finally (return *plist-info*)
        ))

;;; returns a hash table mapping pegs to annotation information
;;; There is a single file of annotations for each GID (organism).
;;; Each annotation record is separated by a line beginning with //
;;; The first line of the annotation record is the peg id.
;;; All the lines between the peg id and the separator line are
;;; gathered up and stored as a list of strings.  There can be more than
;;; one annotation record per peg.  The information is returned 
;;; as a hash table keyed on the peg id, as a list of lists of strings.

(defun get-seed-info-for-annotations (gid)
  (block exit
    (let ((annotation-file (seed-annotation-file gid))
          (annohash (make-hash-table :test 'equalp)))
      (unless (probe-file annotation-file) 
        (cformatt "*** Annotation file for organism ~A not found! ***" gid)
        (return-from exit annohash))
      (with-open-file (p annotation-file :direction :input)
        (let ((len (file-length p)))
          (when (> len 50000000) 
            (cformatt 
             (one-string-nl
              "*** Annotation file ~A"
              "*** for organism ~A is ~D chars! Too big!")
             annotation-file gid len)
            (return-from exit annohash)
            )))
      (vformatt "Retrieving/parsing annotation information from ~A" 
                annotation-file)
      (let ((lines (utils::file-to-string-list annotation-file)))
        (let ((records 
               (loop for line in lines 
                     with reclist = nil
                     with reclines = nil
                     do
                     (if (and (>= (length line) 2)
                              (string= "//" (subseq line 0 2)))
                         (progn
                           (push (reverse reclines) reclist)
                           (setq reclines nil)
                           )
                       (push line reclines)
                       )
                     finally
                     (return reclist)
                     )))
          (loop for record in records
                as pegid = (first record)
                do
                (push (cdr record) (gethash pegid annohash))
                ))
        annohash
        ))))

(defun remove-organism-directory (orgf)
  (ecase (os?)
    (:windows nil)
    (:unix 
     (let* ((dir (#^organism-data-directory orgf))
            (file (non-acache-signal-file dir)))
       (unless (probe-file file)
         (vformatt "Removing organism data from ~A~%" dir)
         (protected-shell-command (formatn "rm -r ~A" dir))
         ))))
  nil)

(defun non-acache-signal-file (dir)
  (merge-pathnames "do-not-delete-me.txt" dir))

