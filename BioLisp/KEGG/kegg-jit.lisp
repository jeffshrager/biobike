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

;;; Hack to download kegg info on demand.
;;; Mike Travers 3/2004

;;; NOTE, this code is more or less obsolete,
;;; you should load KEGG from the flat files
;;; see kegg-file-load.lisp

(defslot #$KEGG.link.s :base #$URLvalue)

(defparameter *kegg-prefix* "KEGG.")

(defslot #$KEGG.link.s :base #$URLvalue)
(defslot #$KEGG.image.s :base #$imageValue)


;;; returns a list of (field value) pairs.  
;;; There may be multiple entries for a given field.
(defun parse-kegg-html (url)
  (let* ((html (net.aserve.client:do-http-request url))
	 (lines (cl-user::string-split-on-char html #\Newline))
	 (state nil) (result nil)
	 prefix rest
	 )
    (dolist (line lines)
      (if (string-equal "///" line)
	  (return result))
      (when (> (length line) 12)
	(setq prefix (string-trim '(#\Space) (subseq line 0 12))
	      rest (subseq line 12))
	(if (or 
	     (and (eq state nil)
		  (string-equal prefix "NAME"))
	     (and (not (eq state nil))
		  (not (equal prefix ""))))
	    (setq state (intern prefix)))
	(if state
	    (push (list state rest) result))
	))
    ))

(defun fill-kegg-frame (frame)
  (let ((url (slotv frame #$KEGG.link.s)))
    (if url
	(dolist (entry (parse-kegg-html url))
	  (unless (char= #\< (char (symbol-name (car entry)) 0)) ;+++ special case crock, ignore some  html
	    (let* ((field (car entry))
		   (value (cadr entry)))
	      (case field
		;; special cases go here
		;; +++
;		(pathway
;		 )
		(t (dolist (elt (process-field-value value))
		     (pushnew elt (slotv frame (kegg-sym->slot field)))))))))
      (format t "No URL for ~A" frame))))

;;; +++ already something like this, see kegg-name->frame
(def-memoized-function kegg-sym->slot (sym)
  (let* ((fname (format nil "~a~a.s" *kegg-prefix* (string-downcase (symbol-name sym))))
	 (frame (frame-fnamed fname t)))
    (unless (frame-is-a frame #$slot)
      (defslot frame))			;+++ add some type info?
    frame))

;;; Return a list of values...
(defun process-field-value (string)
  (let ((html 
         #+:allegro
         (net.html.parser:parse-html string)
         #-:allegro
         (error "Need to have net.html.parser:parse-html equivalent" string)
         )
	(result nil))
    (dolist (item html)
      (cond ((stringp item)
	     (push item result))
	    ((and (listp item)
		  (eq (caar item) :a))
	     (push (process-link item) result))
	    (t (error "Unknown html element ~A" item))))
    ;; this is reversed but it gets re-reversed when it gets pushed onto the slot
    result))

;;; Turns a link into a frame, if possible
(defun process-link (link-item)
  (let* ((url (cadr (member :href (car link-item))))
	 (pos (search "www_bget?" url)))
    (if pos
	(let* ((namepos (+ pos (length "www_bget?")))
	       (name (subseq url namepos))
	       (pluspos (position #\+ name))
	       (type (subseq name 0 pluspos)))
	  (def-frame (format nil "~A~A" *kegg-prefix* name)
	      #$KEGG.link.s (kegg-delocalize-url url)
	      #$isA (list (kegg-type-frame type))))
      (progn
	(print `(unknown url ,url))
	link-item))))

(defun kegg-delocalize-url (url)
  (format nil "http://www.genome.ad.jp/~A" url)) ;+++ more principled way of doing this?)
  
(defun kegg-type-frame (type)
  (def-frame (format nil "~A~A" *kegg-prefix* type)
      #$isA '(#$KEGG.Thing)))
	     
;;; Keep function objects out of slots so we can dump them.  JP.

(defun kegg-thing-fillmethod (frame)
  (if (null (slotv frame #$KEGG.name.s)) (fill-kegg-frame frame)))

(def-frame #$KEGG.Thing
    #$fillMethod 'kegg-thing-fillmethod)

#|

#'(lambda (frame)
    (if (null (slotv frame #$KEGG.name.s))
        (fill-kegg-frame frame))))
|#


;;; bootstrap!
(def-frame #$KEGG.compound+C00031
    #$isA '(#$KEGG.Thing)		;not quite right
    #$KEGG.link.s "http://www.genome.ad.jp//dbget-bin/www_bget?compound+C00031")







		 

		 
       
