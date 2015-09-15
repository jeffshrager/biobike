;; -*- mode: Lisp; Syntax: Common-Lisp; Package: biolisp; -*-

(in-package :bio)

;;; +=========================================================================+
;;; | Copyright (c) 2002-2005 JP Massar, Jeff Shrager, Mike Travers           |
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

;;; Authors:  Jeff Shrager & JP Massar

(defun align (objects &key (safe? t) (sequence-length-limit 25000)
                      (gapopen 10) (gapext 0.2) (max-clustalw-output 10000))
  #.(one-string-nl 
     "Given a list of genes, proteins, etc [i.e., things that can be passed"
     "to EXTRACT-SEQUENCE] or sequences as strings, this function passes"
     "them to ClustalW and returns a frame representing the alignment. The"
     "keyword argument :safe (default = t) skips anything passed in that one"
     "can't get a sequence from. Dropped items are flagged with an error"
     "report to standard output, but the alignment proceeds anyway.  If safe"
     "= nil, objects that don't have sequence information will probably"
     "cause an error! (Strings are always considered sequences.) (You might"
     "want to use safe?=nil if, for example, you are assuming that the"
     "result matches some list you sent in, and you desire to have an error"
     "if the object has no sensible sequence interpretation.)"
     ""
     "The resulting"
     "frame has an #$ALIGNMENTS slot and a #$CONSENSUS slot. You may also"
     "name the OBJECTS to ALIGN by using this list syntax for the arguments:"
     "((name1 object1) (name2 object2) ...).  The names may be anything and"
     "they will appear in the #^ALIGNMENTS slot instead of the objects. The"
     "order of objects is dictated by Clustal's result order, which relates"
     "to the phylogenetic distance of the sequences. (You'll have to read"
     "the ClustalW documentation to figure out what this means.))")
  (declare (ignorable safe?))
  (with-temp-file-in (clustal-output-file cl-user:*tmp-directory*)
    (with-temporary-fasta-file 
        (prefix fapath fafile master-list 
                :safe? safe?
                :translate '("*" "Z")
                :sequence-length-limit sequence-length-limit
                ) objects
      (let* ((alnfile (format nil "~a.aln" prefix))
             (alnpath (merge-pathnames  cl-user:*tmp-directory* alnfile))
             (clustal-command 
              (formatn 
               "~aclustalw -infile=~a -GAPOPEN=~a -GAPEXT=~a > ~A" 
               cl-user::*clustal-executable-dir* fapath
               gapopen gapext (namestring clustal-output-file))))
        (case 
            (progn
              (when wb::*username* 
                (wb::log-system-event 
                 "Clustalw invoked by user ~A." wb::*username*))
              (prog1
                  (protected-shell-command clustal-command)
                (when wb::*username*
                  (wb::log-system-event
                   "Clustalw, invoked by user ~A, returned." wb::*username*
                   ))))
          (:timeout
           (signal
            (make-condition
             'wb::timelimit-exceeded :limit wb::*execution-timelimit*)))
          ;; (:timeout nil)
          (otherwise 
           ;; Load the result, if any.  
           ;; The order prob. won't be the same as the user-given order, so 
           ;; we have to assign the results appropriately.
           (when (probe-file alnpath)
             (let ((tempframe (make-temp-frame #$ClustalWResult))
                   ;; Preset the temp frame's per-object alignments to "" 
                   ;; and the name temporarily to !
                   ;; This will get replaced the first time through!
                   (alignments 
                    (loop for n from 1 to (length master-list) collect 
                          (LIST '! ""))))
               
               ;; get all the clustalw output directed to standard output
               ;; and store it in the frame
               ;; unless its too large, in which case just get the first
               ;; max-clustalw-output characters.  
               (when (probe-file clustal-output-file)
                 (let ((too-big? nil))
                   (with-open-file (p clustal-output-file :direction :input)
                     (when (> (file-length p) max-clustalw-output)
                       (setq too-big? t)
                       (let ((seq (make-string max-clustalw-output)))
                         (read-sequence seq p)
                         (setf (#^clustalw-output tempframe) (s+ seq "..."))
                         )))
                   (unless too-big?
                     (setf (#^clustalw-output tempframe) 
                           (file-to-string clustal-output-file)
                           ))))

               ;; process the actual clustalw output file 
               (setf (slotv tempframe #$consensus) "")
               (block eof 
                 (with-open-file (i alnpath)
                   ;; Skip the first three header lines, then we have to 
                   ;; annoyingly thread together the clustering result, 
                   ;; and the consensus. 
                   (dotimes (k 3) (read-line i nil nil))
                   ;; This loop is blown out of by a return-from 
                   ;; when we hit the end of the file
                   (loop 
                    (loop
                     as key/alignment in alignments
                     as line = (read-line i nil nil)
                     do 
                     ;; If we slam into the end, just break all the way out!
                     (when (null line) (return-from eof nil))
                     (let ((incoming-key (read-from-string (subseq line 0 3)))
                           (current-key (car key/alignment)))
                       ;; This is just a safety test
                       ;; In theory, this should never happen!!
                       (cond ((eq '! current-key)) 
                             ;; skip test -- nothing set yet!
                             ((/= current-key incoming-key)
                              (error
                               (one-string 
                                "Something's wrong in ALIGN; The incoming key "
                                "is ~a while the current key is ~s!!")
                               incoming-key current-key)))
                       ;; This is really only function the first time through.
                       (setf (car key/alignment) incoming-key)
                       ;; Here's where the real work of appending the next
                       ;; part of the result happens!
                       (setf (cdr key/alignment) 
                             (LIST
                              (format nil "~a~a" (SECOND key/alignment) 
                                      (subseq line 16)))
                             )
                       ))
                    ;; And update the consensus, too:
                    (setf (slotv tempframe #$consensus)
                          (format nil "~a~a" (slotv tempframe #$consensus)
                                  (subseq (read-line i nil nil) 16)))
                    ;; And skip one (and possibly break here as well):
                    (when (null (read-line i nil nil)) (return-from eof nil))
                    )))
               ;; Now we have to properly tag the alignments based upon 
               ;; the user's object list, and finally shove them into the 
               ;; #^alignments slot and return the temp frame.  While we're
               ;; doing this, we xlate the Z's back to *s.
               (setf 
                (slotv tempframe #$alignments)
                (loop for (key alignment) in alignments
                      as (nil user-given-label nil) = (assoc key master-list)
                      as fixed-alignment = (substitute #\* #\Z alignment)
                      collect (list user-given-label fixed-alignment)))
               tempframe 
               ))))))))
