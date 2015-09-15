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

;;; Author:  JP Massar


(defun canonicalize-seed-org-designator (od) 
  (cond
   ((typep od 'seed-organism) od)
   ((typep od 'frames::frame) (error "~A is not an organism frame!" od))
   ((stringp od) 
    (let ((f (frame-fnamed od)))
      (cond
       ((null f) 
        (let ((s (find-symbol (string-upcase od) :bio)))
          (cond
           ((and (boundp s) (symbol-value s))
            (let ((f (symbol-value s)))
              (cond
               ((typep f 'seed-organism) (symbol-value s))
               ((typep f 'frames::frame) 
                (error 
                 (one-string-nl
                  "~S is a string which names a symbol, ~S, whose value is"
                  "the frame ~A, but that frame is not an organism frame!")
                 od s f
                 ))
               (t 
                (or (gid-string->organism-frame od)
                    (error "The string ~S does not designate a frame!" od)
                    )))))
           (t 
            (or (gid-string->organism-frame od)
                (error "The string ~S does not designate a frame!" od)
                )))))
       ((typep f 'seed-organism) f)
       ((typep f 'frames::frame)
        (error "~A names a frame, but that frame is not an organism frame!" od))
       (t (error "Internal error in canonicalize-seed-org-designator"))
       )))
   ((symbolp od) 
    (cond
     ((and (boundp od) (symbol-value od))
      (let ((f (symbol-value od)))
        (cond
         ((typep f 'seed-organism) (symbol-value od))
         ((typep f 'frames::frame) 
          (error 
           (one-string-nl
            "~A is a symbol whose value is the frame ~A,"
            "but that frame is not an organism frame!")
           od f
           ))
         (t (error "The symbol ~A does not designate a frame!" od))
         )))
     (t (error "The symbol ~A does not designate a frame!" od))
     ))
   (t 
    (error 
     "~A, an object of type ~A, cannot designate a frame!" od (type-of od)
     ))))

;; This function returns 3 values:
;; 1- The frame the seed id represents, or NIL.
;; 2- The organism frame the frame the seed-id represents is associated with,
;;    or in the case of a contig id, a list of organism frames (because
;;    contig ids are not necessarily unique; in this case the value
;;    returned as the first value is NIL.  The value can also be NIL
;;    if the seed id cannot be recognized, or the seed id represents
;;    an organism, in which case the first value is the organism frame.  
;; 3- An error message or NIL.
       
(defun seed-id->frame (seed-id)
  (block exit
    (when user::*master-list-enabled*
      (return-from exit (seed-id->frame-msf seed-id)))
    ;; Does the ID name a seed frame directly? 
    (vwhen (f (frame-fnamed seed-id))
      (when (or (typep f 'seed-organism) 
                (typep f 'seed-contiguous-sequence)
                (typep f 'seed-protein)
                (typep f 'seed-gene))
        (return-from exit f)))
    ;; Does the ID when converted to a biobike organism name designate an
    ;; organism frame directly or a symbol whose value is an organism frame?
    (when (or (= 1 (count #\- seed-id)) (= 1 (count #\. seed-id)))
      (let ((orgf (gid->orgf seed-id)))
        (when orgf (return-from exit orgf))
        ))
    ;; is it a feature id?
    (multiple-value-bind (feature-frame orgf msg)
        (seed-id-is-feature-id? seed-id)
      (when (or orgf msg)
        (return-from exit (values feature-frame orgf msg))))
    ;; It's not an organism or a gene.  Could it be a contig?
    (multiple-value-bind (contig-frame orgf msg)
        (seed-id-is-contig-id? seed-id)
      (when (or orgf msg) (return-from exit (values contig-frame orgf msg))))
    ;; Doesn't seem to be anything recognizable!
    (return-from exit 
      (values nil nil "Not recognizable as organism, contig, or gene!")
      )))

(defun seed-id->frame-msf (seed-id)
  (block exit
    (unless (stringp seed-id)
      (return-from exit 
        (values
         nil nil 
         (formatn "Seed id argument, ~A,  must be a string!" seed-id)
         )))
    ;; is it a genome id?
    (vwhen (orgf (gid->orgf-msf seed-id))
      (return-from exit orgf))
    ;; is it a feature id?
    (multiple-value-bind (feature-frame orgf msg)
        (seed-id-is-feature-id? seed-id)
      (when (or orgf msg) 
        (return-from exit (values feature-frame orgf msg))))
    ;; It's not an organism or a gene.  Could it be a contig?
    (multiple-value-bind (contig-frame orgf msg)
        (seed-id-is-contig-id? seed-id)
      (when (or orgf msg) (return-from exit (values contig-frame orgf msg))))
    ;; Doesn't seem to be anything recognizable!
    (return-from exit 
      (values nil nil "Not recognizable as organism, contig, or gene!")
      )))


(defun seed-id-is-feature-id? (seed-id)
  ;; Is the ID the ID of a peg or similar in the seed database? 
  ;; If so, is the peg's genome in the allegrocache database?  
  ;; If so, is the genome loaded?  
  ;; If so, can we find the gene in that genome?  
  ;; If so, return it!  
  (block exit
    (let ((gene-info (first (seed-gene-gid-and-contig-for-pegid seed-id))))
      (when gene-info 
        (let* ((gid (second gene-info))
               (contig (third gene-info))
               (orgf (gid->orgf gid)))
          (if orgf 
              (progn
                (unless (#^organism-loaded? orgf)
                  (return-from exit (values nil orgf "Not loaded!")))
                (loop
                 for c in (#^contiguous-sequences orgf)
                 do
                 (when (string-equal contig (#^seed-id c))
                   (let ((v (#^genes-sorted-by-position c)))
                     (loop for gene across v do 
                           (when (string-equal seed-id (#^seed-id gene))
                             (return-from exit (values gene orgf))
                             ))
                     (return-from exit 
                       (values 
                        nil
                        orgf
                        (formatn
                         (one-string-nl
                          "The seed associates PEGID ~A with organism ~A (~A)"
                          "and its contig ~A (~A) but no gene"
                          "with that ID exists on that contig!")
                         seed-id gid orgf contig c
                         ))))))
                (return-from exit 
                  (values
                   nil
                   orgf 
                   (formatn
                    (one-string-nl
                     "The seed associates PEGID ~A with organism ~A (~A)"
                     "and the seed contig ~A, but no contiguous sequence frame"
                     "belonging to the organism has that seed id!")
                    seed-id gid orgf contig
                    ))))
            (return-from exit
              (values 
               nil 
               nil
               (formatn 
                "The GID ~A in the seed has no corresponding frame yet!"
                gid
                )))))))))

(defun seed-id-is-contig-id? (seed-id)
  (block exit
    (let ((info (genome-of-contig seed-id)))
      (cond
       ((null info) (return-from exit (values nil nil nil)))
       ((> (length info) 1) 
        (let ((orgframes nil) (gids-without-frames nil))
          (loop for (gid) in info do
                (vif (orgf (seed-id->frame gid))
                     (push orgf orgframes)
                     (push gid gids-without-frames)
                     ))
          (return-from exit
            (let ((msg 
                   (formatn
                    "seed-id ~A names multiple contigs in the seed!" seed-id)))
              (values 
               nil orgframes 
               (if (null gids-without-frames) 
                   msg
                 (s+ msg " GIDs of contigs without frames: " 
                     (string-join gids-without-frames #\,) "."
                     )))))))
       (t 
        (let* ((gid (caar info))
               (orgf (gid->orgf gid)))
          (when orgf
            (unless user::*master-list-enabled*
              (unless (#^organism-loaded? orgf)
                (return-from exit (values nil orgf "Not loaded!"))))
            (loop for c in (#^contiguous-sequences orgf)
                  do
                  (when (string-equal seed-id (#^seed-id c))
                    (return-from exit (values c orgf))
                    ))
            (return-from exit 
              (values 
               nil
               orgf
               (formatn
                (one-string-nl
                 "The ID ~A names a contig in the seed associated with gid"
                 "~A (~A), but that organism does not contain a contig with"
                 "the seed id ~A")
                seed-id gid orgf seed-id
                ))))))))))

(defun gid->orgf (gid)
  (block exit
    (when user::*master-list-enabled*
      (return-from exit (gid->orgf-msf gid)))
    (let ((biobike-orgname (seed-gid-to-biobike-orgname gid)))
      (vwhen (orgf (frame-fnamed biobike-orgname))
        (when (typep orgf 'seed-organism) (return-from exit orgf)))
      (let ((org-symbol (find-symbol biobike-orgname :bio)))
        (when org-symbol 
          (let ((orgf (and (boundp org-symbol) (symbol-value org-symbol))))
            (when (and orgf (typep orgf 'seed-organism))
              (return-from exit orgf)
              )))))
    nil
    ))

(defun gid->orgf-msf (seed-id) (gethash seed-id *gid->frames-map*))
                 
(defun fix-organism-symbols-slot ()
  (loop 
   for org in (available-organisms) 
   as alias-symbols = (#^organism-symbols org)
   do
   (let ((new-organism-symbols
          (loop 
           for symbol in alias-symbols
           as name = (symbol-name symbol)
           do
           (unintern symbol :bio)
           collect
           (let ((new-symbol (intern name :oa)))
             (export new-symbol :oa)
             (import new-symbol :bbl)
             (export new-symbol :bbl)
             ))))
     (setf (slotv #$organism-symbols org) new-organism-symbols)
     ))
  #-:lispworks
  (db.ac::commit)
  )
                
(defun existing-subsystem-frames-sf ()
  (let ((list nil))
    #-:lispworks
    (db.ac::doclass (x 'subsystem) (push x list))
    list
    ))              
               
                      
                       
