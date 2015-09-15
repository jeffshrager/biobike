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

;;; Authors: JP Massar, Mike Travers, Jeff Shrager.

;;; A framed version of the original code. 
;;; Eval (add-react-info) to set up data

#-:jpmtf
(def-inverse-slot #$GO.reactantIn #$GO.reactants)
#-:jpmtf
(def-inverse-slot #$GO.productIn #$GO.products)

;;; Call this to add the reaction info
(defun add-go-reaction-info ()
  (let ((verbose? *go-verbose*) (reactions nil))
    (loop for f in *go-frames* for count fixnum from 0 do
          (when (parse-reaction f) (push f reactions))
          (when (and verbose? (zerop (mod count 1000))) (format t ".")))
    (when verbose? (terpri))
    (dolist (r reactions)
      ;; if the reaction is not a subclass of another reaction
      (when (null (intersection (slotv r #$isA) reactions))
	;; mark that it isA reaction
	(pushnew #$GO.Reaction (slotv r #$isA))))))


;;; Extract reaction info from slot description
;;; We are conflating enzyme and reaction, which will probably bite us later.
(defun parse-reaction (reaction)
  (let ((reaction-string 
         (extract-reaction-from-defn (slotv reaction #$GO.definition))))
    (when reaction-string
      (let* ((parts (simple-string-split reaction-string #\=)) ; Split into 2
	     (reactants (+split-reaction-half (first parts)))
	     (products (+split-reaction-half (second parts))))
	;; Note: this seems order dependent. Something smarter is needed.
        (setf (slotv reaction #$GO.reactants) (mapcar 'find-molecule reactants))
        (setf (slotv reaction #$GO.products) (mapcar 'find-molecule products))
        )
      reaction
      )))

(defun extract-reaction-from-defn (definition)
  (let* ((reaction-prefix "Catalysis of the reaction:")
         (lenrp (length reaction-prefix)))
    (when definition
      (let ((catpos (search reaction-prefix definition :test #'char-equal))
	    dotpos react)
        (when catpos 
	  (setf dotpos (position #\. definition :start (1+ catpos)))
	  (when dotpos
	    (setf react (subseq definition (+ lenrp catpos) dotpos))
            ;; make sure there is an = in the string
	    (when (position #\= react)	
	      react
              )))))))

(defun +split-reaction-half (part)
  ;; This both parses a half reaction, and fixes the H+ problem by looking
  ;; for cases where a term is followed by a null string (""), so anywhere we
  ;; find something like: "H" "", it goes to "H+", etc.  At the same time this
  ;; will skip over null strings.
  ;; Gather the terms in the +-split of the half reaction.                
  (loop for molecule+ on 
        (loop for term in (simple-string-split part #\+)
              ;; Downcase for case folding and remove spaces.
              collect (string-downcase (string-trim " " term)))
        when (string-equal "" (cadr molecule+))
        collect (format nil "~a+" (car molecule+))
        else unless (zerop (length (car molecule+)))
        collect (car molecule+)))



;;; memoized for now because find-frame isn't?!
(def-memoized-function find-molecule (mol-name)
  (let ((mol (frame-fnamed (make-kdb-fname mol-name "MOL.") t)))
    (unless (slotv mol #$isA) (push #$GO.Molecule (slotv mol #$isA)))
    mol))


;;; +++ this should be a "method" of the frame...
(defun display-reaction (reaction &optional (stream t))
   (format stream "~a --[~a]--> ~a~%" 
	   (slotv reaction #$GO.reactants)
	   (slotv reaction #$fName)
	   (slotv reaction #$GO.products)))

(defun display-reactions-involving (molecule)
  (mapc #'display-reaction
	(union
	 (slotv molecule #$GO.reactantIn)
	 (slotv molecule #$GO.productIn)))
  nil)



;;; Experimental

;;; Molecules that are one-step precursors of this one

#-:jpmtf
(def-computed-slot  (#$GO.directPrecursors frame slot)
  (let ((result nil))
    (dolist (producer (slotv frame #$GO.productIn))
      (setf result (union result (slotv producer #$GO.reactants))))
    result))
      

