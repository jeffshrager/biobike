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

(defun run-meme 
       (objects 
        &key (html? t) (both-strands nil) (palindromes nil) (threshold nil)
        (width nil) (min-width nil) (max-width nil) (start-with nil)
        (return 3) (mode "zoops") (target-type :dna) (labels? t)
        (labels-are-integers? nil))
  
  (when (not labels?) 
    (when labels-are-integers? 
      (error "If there are no labels, how can they be integers?"))
    (setq labels-are-integers? t)
    )
        
  (block exit 
    (let ((meme-type 
           (cond
            ((string-equal target-type "dna") "-dna")
            ((string-equal target-type "protein") "-protein")
            (t "-protein")
            ))
          (web-path nil)
          (result nil))
      (flet ((doit (prefix fapath fafile master-list)
               (declare (ignore fafile))
               (let* ((outpath 
                       (if html?
                           (merge-pathnames cl-user::*webtmp-directory* 
                                            (format nil "~a.html" prefix))
                         (merge-pathnames cl-user:*tmp-directory*
                                          (format nil "~a.out" prefix))))
                      (meme-command 
                       (formatn 
                        (one-string
                         "~ameme -nostatus "
                         (if html? "" "-text ")
                         (if both-strands "-revcomp " "")
                         (if palindromes "-pal " "")
                         (if threshold (s+ "-evt " threshold " ") "")
                         (if width (s+ "-w " width " ") "")
                         (if min-width (s+ "-minw " min-width " ") "")
                         (if max-width (s+ "-maxnw " max-width " ") "")
                         (if start-with (s+ "-cons " start-with " ") "")
                         "-nmotifs "
                         "~a -mod ~a ~a ~a > ~a")
                        cl-user::*meme-executable-dir* 
                        return mode meme-type fapath outpath)))
          
                 #+debug
                 (print 
                  (list 'prefix prefix 'fapath fapath 'fafile fafile 
                        'outpath outpath 'meme-command meme-command 
                        'return return 'width width 'min-width min-width
                        'max-width max-width 'threshold threshold 
                        'start-with start-with 
                        'both-strands both-strands 'palindromes palindromes
                        'meme-type meme-type 'objects objects
                        ))

                 (case (protected-shell-command meme-command)
                   (:timeout (return-from exit (values nil nil :timeout)))
                   (otherwise nil))
                 (if html?
                     (progn
                       (setf 
                        web-path
                        (wb::webtmp-url (format nil "~a.html" prefix)))
                       (setq result 
                             (wb::make-url
                              :path web-path
                              :display-string "meme results in html")))
                   (with-open-file  (i outpath)
                     (setq 
                      result 
                      (loop
                       for motif =
                       (bio::parse-next-meme-motif i master-list)
                       until (null motif)
                       collect motif
                       )))))))
        (cond
         ((null labels?) 
          (with-temporary-fasta-file 
              (prefix fapath fafile master-list :safe? t :user-label nil) 
              objects
            (doit prefix fapath fafile master-list)
            ))
         (t 
          (with-temporary-fasta-file 
              (prefix fapath fafile master-list :safe? t :user-label t) 
              objects
            (doit prefix fapath fafile master-list)
            )))
        (values result web-path t)
        ))))

(defun parse-next-meme-motif (i master-list &key (labels-are-integers? t))
  (loop with result = nil
	as line = (read-line i nil nil)
	do 
	(cond ((null line) (return result))
	      ((and (> (length line) 5) (equal "MOTIF" (subseq line 0 5)))
	       (push 
                (parse-meme-motif 
                 line master-list i :labels-are-integers? labels-are-integers?)
                result
                )))))

(defun parse-meme-motif (header master-list i &key (labels-are-integers? t))
  (flet ((parse-results-until-break ()
	   (loop for line = (read-line i nil nil)
		 until (and (= (length line) 80) 
                            (char-equal #\- (aref line 0)))
		 collect (cl-ppcre::split "\\s+" line)))
	 (skip-to-search (for)
	    (loop for line = (read-line i nil nil)
		 until (search for line)))
	 )
    (let* ((frame (make-temp-frame #$motif)))
      (destructuring-bind 
          (x1 number x2 x3 width x4 x5 sites x6 x7 llr x8 x9 e-value)
	  (cl-ppcre::split "\\s+" header)
        (declare (ignore x1 x2 x3 x4 x5 x6 x7 x8 x9))
	(setf (slotv frame #$number) (parse-integer number))
	(setf (slotv frame #$width) (parse-integer width))
	(setf (slotv frame #$sites) (parse-integer sites))
	(setf (slotv frame #$llr) (parse-integer llr))
	(setf (slotv frame #$e-value) (read-from-string e-value))
	)
      (skip-to-search "sites sorted by position p-value")
      (dotimes (n 3) (read-line i))
      (loop for (label start p-value . seqs) in (parse-results-until-break)
	    as mframe = (make-temp-frame #$motif-match)
	    do 
	    (setf (slotv mframe #$object) 
                  (if labels-are-integers? 
                      (second (assoc (parse-integer label) master-list))
                    (second (assoc label master-list :test 'string-equal))
                    ))
	    (setf (slotv mframe #$start) (parse-integer start))
	    (setf (slotv mframe #$p-value) 
                  (read-from-string (substitute #\d #\e p-value)))
	    (setf (slotv mframe #$sequence) (apply #'concatenate 'string seqs))
	    (push mframe (slotv frame #$matches))
	    )
      frame)))
  