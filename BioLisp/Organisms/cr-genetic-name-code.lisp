(in-package :bio)

(defun remove-crs-from-orgs ()
  (loop for org in (available-organisms)
        as dir = (#^organism-data-directory org)
        as genes-dir = (append-subdir dir "genes")
        as genes-file = (merge-pathnames "genes.tbl" genes-dir)
        as oldfile = (merge-pathnames "old-genes.tbl" genes-dir)
        do
        (formatt ";; Copying ~A to ~A~%" genes-file oldfile)
        (copy-text-file genes-file oldfile)
        (strip-file-of-returns-preceding-newlines genes-file)
        ))


(defun check ()
  (loop for org in *loaded-organisms*
        with bad-genetic = nil
        with bad-gene = nil
        with count = 0
        do
        (loop for g in (#^genes org)
              do
              (when (frames::frame-has-slot? g #$genetic-name)
                (incf count)
                (when (null (#^genetic-name g))
                  (push g bad-genetic)
                  ))
              (when (frames::frame-has-slot? g #$gene-name)
                (when (null (#^gene-name g))
                  (push g bad-gene)
                  )))
        finally
        (return (list bad-genetic bad-gene count))
        ))

(defun change-gene-name-column-to-genetic-name ()
  (loop 
   for org in
   (remove-if-not
    (lambda (x) (#^organism-data-directory x))
    (available-organisms)
    )
   with orgs-changed = nil
   as dir = (#^organism-data-directory org)
   as genes-dir = (append-subdir dir "genes")
   as genes-file = (merge-pathnames "genes.tbl" genes-dir)
   as oldfile = (merge-pathnames "old-genes.tbl" genes-dir)
   do
   (when (probe-file genes-file)
     (let* ((lines (file-to-string-list genes-file))
            (firstline (first lines)))
       (vwhen (pos (search "gene-name" firstline :test 'char-equal))
         (formatt ";; Copying ~A to ~A~%" genes-file oldfile)
         (copy-text-file genes-file oldfile)
         (setq 
          firstline
          (s+ (subseq firstline 0 pos) 
              "GENETIC-NAME"
              (subseq firstline (+ pos (length "gene-name")))
              ))
         (setf (first lines) firstline)
         (with-open-file 
             (p genes-file :direction :output :if-exists :supersede)
           (loop for line in lines do (write-line line p))
           )
         (push org orgs-changed)
         )))
   finally (return orgs-changed)
   )) 
