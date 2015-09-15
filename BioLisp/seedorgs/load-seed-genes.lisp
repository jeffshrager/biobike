;;; -*- Package: bio; mode: lisp; base: 10; Syntax: Common-Lisp; -*-

(in-package :bio)

;;; +=========================================================================+
;;; | Copyright (c) 2009 JP Massar                                            |
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

(defun process-seed-gene-info-msf (orgf)
  (cformatt "Computing gene prefix and compiled gene pattern")
  (let* ((org-prefix (#^organism-prefix orgf))
         (gene-prefix (determine-appropriate-gene-prefix-msf orgf org-prefix))
         ;; ppcre-compile the gene pattern if it was provided 
         (gene-pattern (#^gene-pattern orgf))
         (alt-gene-pattern (#^alt-gene-pattern orgf))
         (*compiled-gene-pattern* 
          (handler-case 
              (and *enable-gene-pattern-msf* 
                   gene-pattern 
                   (ppcre::create-scanner 
                    gene-pattern :case-insensitive-mode t))
            (error 
             ()
             (cformatt 
              (one-string-nl
               "*** Invalid gene pattern: ~S.  Master list for"
               "organism ~A has a gene pattern which will not"
               "PPCRE compile!")
              gene-pattern orgf)
             nil
             )))
         (*compiled-alt-gene-pattern*
          (handler-case 
              (and *enable-gene-pattern-msf* 
                   alt-gene-pattern 
                   (ppcre::create-scanner
                    alt-gene-pattern :case-insensitive-mode t))
            (error 
             ()
             (cformatt 
              (one-string-nl
               "*** Invalid alt gene pattern: ~S.  Master list for"
               "organism ~A has an alt gene pattern which will not"
               "PPCRE compile!")
              gene-pattern orgf)
             nil
             ))))

    ;; extract out detailed information from each fid  
    (cformatt "Parsing location information for each gene")
    (extract-fid-info-msf orgf)

    ;; figure out a gene name for each fid 
    ;; point out duplicated aliases that we would otherwise use
    (cformatt "Determining the gene name for each gene")
    (determine-gene-names-msf orgf org-prefix gene-prefix)
    ))
           
(defun determine-appropriate-gene-prefix-msf (orgf org-prefix)
  (or 
   ;; if a gene prefix has been provided, strip the trailing '-'
   ;; if present, because we ensure its presence below, 
   ;; otherwise use the organism prefix without the trailing '.'
   (vwhen (x (#^gene-prefix orgf))
     (if (char-equal #\- (lastelem x))
         (subseq x 0 (1- (length x)))
       x
       ))
   (subseq org-prefix 0 (1- (length org-prefix)))
   ))

(defun extract-fid-info-msf (orgf)
  (let ((removed-fids nil))
    (maphash 
     (lambda (fid plist)
       (flet ((oops (fid)
                (remhash fid *features-info-hash*)
                (setq *fids-for-gid* (delete fid *fids-for-gid*))
                (push (list fid "No location info") removed-fids)
                ))
         (let* ((location (get-feature-prop plist :location))
                (aliases (get-feature-prop plist :aliases))
                (type (get-feature-prop plist :type))
                )
           (setq location (string-trim *whitespace* location))
           (if (zerop (length location))
               (progn
                 (cformatt 
                  (one-string-nl
                   "*** The gene (feature) ~A has no location information!"
                   "*** This gene is being removed from the genes list for this"
                   "*** organism.")
                  fid
                  )
                 (oops fid)
                 )
             (destructuring-bind (contig from to direction segments wraps?)
                 (new-parse-seed-location-info location)
               (declare (ignore contig))
               (when aliases 
                 ;; Some alias fields seem to be semicolon-separated
                 ;; instead of comma-separated
                 (let* ((commas? (find #\, aliases))
                        (semis? (find #\; aliases))
                        (separator 
                         (cond
                          ((and (null commas?) (null semis?)) #\,)
                          ((and commas? semis?) nil)
                          (commas? #\,)
                          (semis? #\;)
                          )))
                   (if (null separator)
                       (progn
                         (cformatt 
                          (one-string-nl
                           "*** The gene (feature ~A) has strange aliases"
                           "*** info!  There are both commas and semicolons"
                           "*** in the aliases string!  This gene is being"
                           "*** removed from the genes list for this organism.")
                          fid
                          )
                         (oops fid)
                         )
                     ;; Put everything we've decoded into the property list
                     ;; for the gene in the hash table.  
                     (progn
                       (set-feature-prop 
                        plist 
                        :aliases 
                        ;; make sure there are no spaces anywhere!
                        (flet ((trim (x) (string-trim *whitespace* x)))
                          (mapcar 
                           (lambda (x) (substitute #\- #\Space (trim x)))
                           (let ((x (trim aliases)))
                             (when (plusp (length x)) 
                               (string-split x separator)
                               )))))
                       (set-feature-prop 
                        plist :encodes-protein (string-equal "peg" type))
                       (set-feature-prop plist :from from)
                       (set-feature-prop plist :to to)
                       (set-feature-prop plist :direction direction)
                       (set-feature-prop plist :segments segments)
                       (set-feature-prop plist :wraps? wraps?)
                       (setf (gethash fid *features-info-hash*) plist)
                       )))))))))
     *features-info-hash*
     )
    (when removed-fids (setf (#^removed-fids orgf) removed-fids))
    nil
    ))
      
(defparameter *enable-revised-gene-ordering* t)

(defun determine-gene-names-msf (orgf org-prefix gene-prefix)
  (when *enable-revised-gene-ordering* (adjust-gene-ordering))
  (let* ((alias-dup-table (make-string-equal-hash-table))
         (dupslist nil)
         (nfeatures (hash-table-count *features-info-hash*))
         (any-pattern? (or *compiled-gene-pattern* *compiled-alt-gene-pattern*))
         ;; insure no name conflicts when potentially using patterns
         ;; Jeff wants gene name numbers to start at 1.
         (base (if any-pattern? (* 1000 (ceiling nfeatures 1000)) 1))
         (any-matches? nil)
         )
    (maphash
     (lambda (fid plist)
       (let* ((pos (get-feature-prop plist :order))
              (aliases (get-feature-prop plist :aliases)))
         (multiple-value-bind (alias-to-use dups)
             (if any-pattern?
                 (determine-best-alias-msf fid aliases alias-dup-table)
               (values nil nil))
           (when (and any-pattern? alias-to-use)
             (setq any-matches? t))
           (let ((gene-name 
                  (s+ 
                   org-prefix
                   (if alias-to-use
                       (cond
                        (*compiled-gene-pattern* alias-to-use)
                        (*compiled-alt-gene-pattern* 
                         (s+ gene-prefix "-" alias-to-use))
                        (t (error "This should be impossible!"))
                        )
                     ;; numeric-ordering-name
                     (s+ gene-prefix "-" (formatn "~4,'0d" (+ base pos)))
                     ))))
             (set-feature-prop plist :gene-name gene-name)
             (setf (gethash fid *features-info-hash*) plist)
             (when dups (push dups dupslist))
             ))))
     *features-info-hash*
     )
    (when (and any-pattern? (null any-matches?))
      (cformatt 
       (one-string-nl
        "*** This organism (~A) has either a gene pattern or an"
        "*** alt gene pattern specified in the master list."
        "*** But no seed gene (feature) of this organism had any aliases"
        "*** that matched the pattern!")
       orgf
       ))
    (limited-errors-reported 
     dupslist
     10 "And more duplicates..." 
     (lambda (dup-info) 
       (loop for dup-record in dup-info
             do
             (destructuring-bind (alias fid dup-fid) dup-record
               (cformatt 
                (one-string-nl
                 "The fid ~A, has an alias, ~A, which would be used" 
                 "as its gene name, except that the fid"
                 "~A, (~A)"
                 "is already using that alias!")
                fid alias dup-fid (get-feature-item dup-fid :gene-name)
                )))))))

(defun adjust-gene-ordering ()
  (let ((contigs-hash (make-string-equal-hash-table))
        (ordering-index -1))
    ;; put each gene's info into a list associated with its contig
    ;; make sure the gene's seed-id is not lost!  
    (maphash 
     (lambda (fid-key plist) 
       (let ((contig (get-feature-prop plist :contig)))
         (push
          (cons :id (cons fid-key plist))
          (gethash contig contigs-hash nil)
          )))
     *features-info-hash* 
     )
    ;; for each contig, sort the gene data we've associated with it by
    ;; the FROM coordinate, assign a new ordering index to each gene
    ;; and put the gene info back into *features-info-hash* 
    (maphash 
     (lambda (contig genes-data) 
       (declare (ignore contig))
       (let ((ordered-genes-data 
              (sort  
               genes-data
               '<
               :key 
               (lambda (plist) (get-feature-prop plist :from))
               )))
         ;; change the :order property to reflect the sorted order,
         ;; making sure that the :order property is unique for the entire
         ;; organism, not just for each contig.  
         (loop for plist in ordered-genes-data 
               do 
               (set-feature-prop plist :order (incf ordering-index))
               )
         ;; make sure this change gets stuffed back into the 
         ;; *features-info-hash* table.  Remember to remove the
         ;; id info that we just stuck on!
         (loop for revised-plist in ordered-genes-data 
               as fid-key = (get-feature-prop revised-plist :id)
               do
               (setf (gethash fid-key *features-info-hash*)
                     (cddr revised-plist))
               )))
     contigs-hash 
     )))
               
         
                 

;; Returns as the first value one of the aliases of FID or 
;; nil if none are acceptable.  The second value is a list 
;; of aliases for this FID which are already in use by other 
;; genes of this organism 
(defun determine-best-alias-msf (fid aliases alias-dup-table)
  (let ((duplist nil)
        (alias-to-use nil))
    (block exit
      (flet ((dupcheck (alias)
               (vif (dup-fid (gethash alias alias-dup-table))
                    (push (list alias fid dup-fid) duplist)
                    (progn
                      (setf (gethash alias alias-dup-table) fid)
                      (setq alias-to-use alias)
                      (return-from exit nil)
                      ))))
        ;; for each alias, if it matches the pattern (if one exists)
        ;; and that alias isn't already being used by another gene of this
        ;; organism, then we use this alias as the gene name.  
        (loop for alias in aliases 
              do
              (cond
               (*compiled-gene-pattern* 
                (multiple-value-bind (start end)
                    (ppcre::scan *compiled-gene-pattern* alias)
                  (when (and start end)
                    (dupcheck (subseq alias start end))
                    )))
               (*compiled-alt-gene-pattern*
                (multiple-value-bind (start end register-starts register-ends)
                    (ppcre::scan *compiled-alt-gene-pattern* alias)
                  (when (and start end)
                    (let ((alias-match 
                           (cond 
                            ((zerop (length register-starts))
                             (subseq alias start end))
                            (t 
                             (subseq 
                              alias 
                              (elt register-starts 0) 
                              (elt register-ends 0)
                              )))))
                      (dupcheck alias-match)
                      ))))))))
    (values alias-to-use duplist)
    ))

