;;; -*- Package: bio; mode: lisp; base: 10; Syntax: Common-Lisp; -*-

(in-package :bio)

;;; +=========================================================================+
;;; | Copyright (c) 2011 JP Massar
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

(defun is-acache-map-empty? (map)
  (block exit
    (progn
      (db.ac::map-map 
       (lambda (key value)
         (declare (ignore key value))
         (return-from exit nil)
         )
       map
       )
      (return-from exit t)
      )))
         

(defun maybe-create-descriptions-map (&key (delete-old-one? nil))
  (block exit
    (let ((existing-map 
           (db.ac::retrieve-from-index 
            'db.ac::ac-map-range
            'db.ac::ac-map-name
            *features-descriptions-map-name*
            )))
      (when existing-map 
        (unless delete-old-one? 
          (setq *features-descriptions-map* existing-map)
          (when (is-acache-map-empty? *features-descriptions-map*)
            (cformatt "*** *features-descriptions-map* is empty!!!!")
            (cformatt "*** Disabling features description map access!")
            (setq *enable-descriptions-map* nil)
            )
          (return-from exit *features-descriptions-map*)
          ))
      (when existing-map (db.ac::delete-instance existing-map))
      (setq *features-descriptions-map* 
            (make-instance 
             'db.ac::ac-map-range 
             :ac-map-name *features-descriptions-map-name*
             ))
      (db.ac::commit)
      (cformatt "*** *features-descriptions-map* needs to be initialized!")
      (setq *enable-descriptions-map* nil)
      *features-descriptions-map*
      )))

(defun store-descriptions-into-dmap ()
  (loop for orgf in *loaded-organisms*
        for count from 1
        do
        (when (zerop (mod count 25)) (print orgf))
        (store-organism-descriptions-into-dmap orgf)
        )
  (db.ac::commit)
  )

(defun store-organism-descriptions-into-dmap (orgf)
  (when *enable-descriptions-map*
    (let ((dmap *features-descriptions-map*))
      (store-gene-descriptions-into-dmap orgf dmap)
      ;; Find the alphabetically first and last gene names
      ;; which are the first and last elements of the description map
      ;; for this organism.
      (let ((gene-names 
             (sort (mapcar #^fname (#^genes orgf)) 'string<)))
        (setf (#^description-keys orgf) 
              (list (first gene-names) (lastelem gene-names))
              ))
      ;; remove the slots whose info has been stored in the descriptions map
      (loop for g in (#^genes orgf) do
            (frames::delete-slot g #$description)
            (frames::delete-slot g #$genetic-name)
            (frames::delete-slot g #$subsystem-role)
            ))))

(defun recompute-description-keys ()
  (loop for orgf in *loaded-organisms*
        for j from 0
        do
        (when (zerop (mod j 100)) (print orgf))
        (let ((gene-names (sort (mapcar #^fname (#^genes orgf)) 'string<)))
          (setf (#^description-keys orgf) 
                (list (first gene-names) (lastelem gene-names))
                ))))
         

(defun descriptions-map-key (gene) (#^fname gene))

(defun map-key->gene (mapkey) (frames::frame-fnamed mapkey))
    
(defun store-gene-descriptions-into-dmap (orgf dmap)     
  (loop for g in (#^genes orgf)
        as mapkey = (descriptions-map-key g)
        as value = (create-gene-description-map-value g)
        do 
        (setf (db.ac::map-value dmap mapkey) value)
        ))

(defun create-gene-description-map-value (gene)
  (let ((*enable-descriptions-map* nil))
    (let ((description (#^description gene))
          (genetic-name (#^genetic-name gene))
          (ssrole (#^subsystem-role gene)))
      (pack-gene-description-info description genetic-name ssrole)
      )))

(defun pack-gene-description-info (description genetic-name ssrole)
  (let ((ns (string #\Newline)))
    (format nil "~A~A~A~A~A~A" 
            (or description "")
            ns
            (or genetic-name "")
            ns
            ;; If there's no description, and there is a subsystem-role
            ;; store the subsystem-role.  
            ;; If there is a description and there is a subsystem-role string
            ;; and the subsystem role string is not identical to the 
            ;; description, store the subsystem role.  
            ;; Otherwise, do not store the subsystem role. 
            (if (or (and (null description) ssrole)
                    (and description ssrole (not (string= ssrole description))))
                ssrole 
              ""
              )
            ns
            )))

(defun description-from-map-value (value)
  (let ((pos (position #\Newline value)))
    (if (zerop pos)
        nil
      (subseq value 0 pos)
      )))

(defun genetic-name-from-map-value (value)
  (let* ((dpos (position #\Newline value))
         (gpos (position #\Newline value :start (1+ dpos))))
    (if (= (1+ dpos) gpos)
        nil
      (subseq value (1+ dpos) gpos)
      )))

(defun ssrole-from-map-value (value)
  (let* ((dpos (position #\Newline value))
         (gpos (position #\Newline value :start (1+ dpos)))
         (spos (position #\Newline value :start (1+ gpos))))
    (if (= (1+ gpos) spos)
        (description-from-map-value value)
      (subseq value (1+ gpos) spos)
      )))
      
         

(defun copymap (oldmap newmap)
  (db.ac::map-map 
   (lambda (key value) (setf (db.ac::map-value newmap key) value))
   oldmap
   ))

(defun show-descriptions-map 
       (&key (dm *features-descriptions-map*) (n 10))
  (block exit
    (let ((count 0))
      (db.ac::map-map 
       (lambda (key value)
         (formatt "~A: ~A~%" key value)
         (when (>= (incf count) n)
           (return-from exit nil)
           ))
       dm
       ))))

(defun description-matches->genes (matches)
  (remove-if 
   'null 
   (mapcar (lambda (match) (map-key->gene (first match))) matches)
   ))

(defun every-gene-described-by-in-organism (string orgf)
  (let ((keys (#^description-keys orgf)))
    (every-gene-described-by-in-range string (first keys) (second keys))
    ))

(defun every-gene-described-by-in-descriptions-map (string)
  (let ((matches nil)
        (count 0)
        )
    (db.ac::map-map
     (lambda (key value)
       (when (zerop (mod (incf count) 1000)) nil)
       (when (search string value :test 'string-equal) 
         (push (list key value) matches))
       )
     *features-descriptions-map*
     )
    (description-matches->genes matches)
    ))

(defun every-gene-described-by-in-genes (string genes)
  (description-matches->genes 
   (loop for g in genes
         as key = (descriptions-map-key g)
         as value = (db.ac::map-value *features-descriptions-map* key)
         when (and value (search string value :test 'string-equal))
         collect (list key value)
         )))

(defun every-gene-described-by-in-range (string start-key end-key)
  (let ((matches nil)
        (count 0)
        (map-cursor 
         (db.ac::create-map-cursor 
          *features-descriptions-map* 
          :initial-value start-key 
          ))
        (done? nil))
    (loop while (not done?)
          do
          (multiple-value-bind (key value)
              (db.ac::next-map-cursor map-cursor)
            (when (null key) 
              (error 
               "map key is nil!  this should be impossible! start key = ~A"
               start-key
               ))
            (when (zerop (mod (incf count) 1000)) nil)
            (when (search string value :test 'string-equal)
              (push (list key value) matches))
            (when (string= end-key key) 
              (db.ac::free-map-cursor map-cursor)
              (setq done? t)
              )))
    (description-matches->genes matches)
    ))

(defun description-keys-consistency-check ()
  (loop for orgf in *loaded-organisms* 
        for count from 0
        as ngenes = (length (#^genes orgf))
        as mapkeys = (#^description-keys orgf)
        as startkey = (first mapkeys)
        as endkey = (second mapkeys)
        as map-cursor = 
        (db.ac::create-map-cursor 
         *features-descriptions-map* 
         :initial-value startkey 
         )
        do
        (when (zerop (mod count 25)) (formatt "~A~%" orgf))
        (loop for j from 1 to ngenes
              do
              (multiple-value-bind (key value)
                  (db.ac::next-map-cursor map-cursor)
                (declare (ignore value))
                (if (null key)
                    (progn
                      (formatt "~A: No next key at ~D~%" orgf j)
                      (return nil))
                  (progn
                    (when (= j 1)
                      (unless (string= key startkey)
                        (format t "~A: startkey ~S, firstkey, ~S~%"
                                orgf startkey key)))
                    (when (= j ngenes)
                      (unless (string= key endkey)
                        (format t "~A: endkey ~S, lastkey, ~S~%"
                                orgf endkey key
                                )))))))
        (db.ac::free-map-cursor map-cursor)
        ))
                

;; "PTS system"
;; "Phage repressor protein CII"
                
(defmethod frame-slots-to-show ((frame seed-gene))
  (sort 
   (append 
    '(#$description #$genetic-name #$subsystem-role)
    (call-next-method))
   'string-lessp :key #^fname
   ))
              
              
    