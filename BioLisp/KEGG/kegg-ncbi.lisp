;;; -*- Package: biolisp; mode: lisp; base: 10; Syntax: Common-Lisp; -*-

(in-package :bio)

;;; +=========================================================================+
;;; | Copyright (c) 2002-2006 by the BioBike team                        s    |
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

(defparameter *kegg-taxonomy-page-url*
  "http://www.genome.jp/kegg-bin/get_htext?htext=Organisms&option=-e&open=A2&oneclick=on#A2"
  )

(defparameter *kegg-ncbi-mapping-data-file* 
  (translate-simple-lp "biol:KEGG;kegg-ncbi-map.lisp"))

;;; Extract the kegg id to ncbi id mapping from the kegg if possible
;;; and store it away.  If we can't get the data from the kegg, use
;;; the stored mapping.  

(defun read-ncbi-mapping-data-file (verbose?)
  (when verbose?
    (cformatt "Extracting KEGG taxonomy table from stored file..."))
  (with-open-file 
      (p *kegg-ncbi-mapping-data-file* :direction :input)
    (read p)
    ))

(defun create-kegg-id-ncbi-id-map 
       (&key
        (data nil) (get-if-older-than (* 3600 24 7)) (force? nil) (verbose? t)
        &aux (map-file-exists? nil) (current-time (get-universal-time))
        )
  (setq map-file-exists? (probe-file *kegg-ncbi-mapping-data-file*))
  (block exit
    (when (null data) 
      (setq 
       data 
       ;; if the map file exists and is less than a week old (by default)
       ;; take the data from the map file 
       (if (and (not force?) 
                map-file-exists?
                (let ((date (file-write-date *kegg-ncbi-mapping-data-file*)))
                  (< (- current-time date) get-if-older-than)
                  ))
           (return-from exit (read-ncbi-mapping-data-file verbose?))
         ;; otherwise try to get the data from kegg itself.  
         ;; if we can't, and the map file exists, use that.  
         #+:allegro
         (mp::with-timeout 
          (15 (return-from exit 
                (and map-file-exists? (read-ncbi-mapping-data-file verbose?))))
          (when verbose?
            (cformatt "Extracting KEGG taxonomy table from KEGG website..."))
          (wb::web-page-contents *kegg-taxonomy-page-url*))
         #-:allegro
         (error "Need to provide data for non-allegro use!")
         ))
      (setq data (string-split data #\Newline))
      (setq data 
            (remove-if-not
             (lambda (x) (search "[TAX:" x :test 'string-equal))
             data
             )))
    (flet ((extract (s) 
             (let* ((gtpos (search ">" s)))
               (when gtpos 
                 (let ((ltpos (search "<" s :start2 gtpos)))
                   (when ltpos
                     (subseq s (1+ gtpos) ltpos)
                     ))))))
      (let ((map nil))
        (loop 
         for line in data 
         as taxpos = (search "[TAX:" line :test 'string-equal)
         as gnpos = (search "[GN:" line :test 'string-equal)
         do
         (if (and taxpos gnpos)
             (let* ((tax-string (subseq line taxpos gnpos))
                    (gn-string (subseq line gnpos))
                    (tdata (extract tax-string))
                    (gdata (extract gn-string)))
               (if (and tdata gdata)
                   (push (list tdata gdata) map)
                 (formatt "Invalid data on line: ~A~%" line)
                 ))
           (formatt "*** Invalid data line: ~A~%" line)
           ))
        (when verbose?
          (cformatt "Saving KEGG ID <-> NCBI ID map..."))
        (with-open-file 
            (p *kegg-ncbi-mapping-data-file*
               :direction :output :if-exists :supersede)
          (format p "~S~%" map)
          )
        map
        ))))
        
(defun maybe-set-kegg-id (ncbi-id map org)
  (vwhen (datum (find ncbi-id map :key 'first :test 'string-equal))
    (setf (#^kegg-id org) (second datum))
    ))

(defun determine-seed-organism-kegg-ids (&key (map nil))
  (unless map 
    (setq map (create-kegg-id-ncbi-id-map :verbose? t)))
  (loop for org in (symbol-value '*loaded-organisms*)
        with count = 0
        as seed-id = (#^seed-id org)
        as ncbi-id = (first (string-split seed-id #\.))
        do
        (when (maybe-set-kegg-id ncbi-id map org) (incf count))
        finally (return count)
        ))

(defun determine-cyano-organism-kegg-ids (&key (map nil))
  (unless map 
    (setq map (create-kegg-id-ncbi-id-map :verbose? t)))
  (loop for org in (forward-funcall 'available-organisms)
        with count = 0
        as ncbi-id = (#^ncbi-taxonomy-id org)
        do
        (when ncbi-id 
          (when (numberp ncbi-id) (setq ncbi-id (formatn "~D" ncbi-id)))
          (when (maybe-set-kegg-id ncbi-id map org) (incf count)))
        finally (return count)
        ))
            
            
