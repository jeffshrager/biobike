;;; -*- Package: biolisp; mode: lisp; base: 10; Syntax: Common-Lisp; -*-

(in-package :bio)

;;; +=========================================================================+
;;; | Copyright (c) 2002, 2003, 2004 JP Massar, Jeff Shrager, Mike Travers    |
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

;;; I don't think anything from this file is used at all.
;;; JP.  09/15/04


;;; Try to relate OCELOT frames with GO frames and vice versa.  This
;;; takes FOREVER!! (See faster implementaion using EC numbers later
;;; on!)  Therefore we will need to store the results in a file to be
;;; read in a lot quicker than they can be computed.

(defun relate-ocelot-to-go-frames (&optional (topn 3))
  ;; Get the names of all the GO frames into a word homology form.
  (let ((go-pretty-names-and-frames
         (mapcar 
          (lambda (gf) (list (compile-word (#^PrettyName gf)) gf))
          *go-frames*))
        (n 0))
    (format t "~&;; Compiled all go frame names~%")
    (dolist (oframe *ocelot-frames*)
      (when (zerop (mod (incf n) 200)) (format t "."))
      ;;(when (= 1000 (incf n)) (return))
      (let ((common-name (#^Common-Name oframe)))
        (when common-name
          (let ((go-matches
                 (word-homology-fast 
                  common-name go-pretty-names-and-frames topn #'car)))
            (setf (slotv oframe #$Go-Frames) 
                  (mapcar #'(lambda (x) (list (cadadr x) (car x)))
                          go-matches))))))))

(defun store-ocelot-to-go-mapping 
       (&key (file "biol:data;ocelot-to-go.lisp")
             (threshold 0.75))
  (with-open-file (p file :direction :output :if-exists :supersede)
    (let ((*package* (find-package :biolisp)))
      (dolist (oframe *ocelot-frames*)
        (let ((go-mapping (#^Go-Frames oframe)))
          (when go-mapping
            (let ((restricted-go-mapping
                   (remove-if-not
                    (lambda (x) (>= (second x) threshold))
                    go-mapping
                    )))
              (when restricted-go-mapping
                (pprint (list oframe restricted-go-mapping) p)
                ))))))))

(defun retrieve-ocelot-to-go-mapping 
       (&key (file "biol:data;ocelot-to-go.lisp"))
  (with-open-file (p file :direction :input :if-does-not-exist :error)
    (let ((*package* (find-package :biolisp)))
      (do ((form (read p nil :eof) (read p  nil :eof)))
          ((eq form :eof))
        (let ((frame (first form)) (mapping (second form)))
          (setf (slotv frame #$Go-Frames) mapping)
          )))))
    
;;; These need to be stated in order to have accessors so that they
;;; can be used before anyone actually mentions them.  I think...anyhow;
;;; I'm not sure what's really going on here.

(defvar *ocelot-slots-for-preparation*
  (list #$EcRef #$ec-number))

;;; Much faster implementation of connecting GO and Ocelot frames,
;;; using EC numbers.  The ECRef field in GO frames binds to the Ec-Number
;;; slot in Ocelot frames.

(defun integrate-go-to-ocelot-by-ec-number ()
  (let ((ogotbl (make-hash-table :test #'equal)))
    (loop for frame in *go-frames*
	  as ecrefs = (slotv frame #$EcRef)
	  when ecrefs 
	  do (loop for ecref in ecrefs
		   do (pushnew frame (gethash ecref ogotbl))))
    (loop for ocelot-frame in *ocelot-frames*
	  as ecref = (slotv ocelot-frame #$ec-number)
	  as go-frames = (gethash ecref ogotbl)
	  do 
	  (loop for go-frame in go-frames 
		do 
		(pushnew ocelot-frame (slotv go-frame #$ocelot-frames))
		(pushnew go-frame (slotv ocelot-frame #$go-frames))))
    ))
