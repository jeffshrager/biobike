;;
;; Copyright (c) 2005, Gigamonkeys Consulting All rights reserved.
;;

(in-package :com.gigamonkeys.markup)

(defparameter *page-width-inches* 8.5)
(defparameter *page-height-inches* 11)
(defparameter *default-font-size* 11)

(defvar *no-mdash* nil)
(defvar *significant-whitespace* nil)

(defconstant +mdash+ (code-char 151))
(defconstant +copyright+ (code-char 169))
(defconstant +bullet+ (code-char 127))

(defparameter *my-leading-ratio* 1.2)

(defmethod render-as ((type (eql :pdf)) sexp file)
  (multiple-value-bind (body notes) (extract-notes sexp)
    (with-document ()
      (let ((content
             (compile-text ()
               (let ((*my-leading-ratio* 1.2))
                 (dolist (p body) (emit-pdf p)))
               (vspace 24)
               (hrule :dy .1)
               (vspace 6)
               (let ((*default-font-size* 10))
                 (loop for n in notes
                       for counter from 1
                       do (emit-note counter n)))
               (mark-ref-point :the-end))))
        (loop
           while (typeset::boxes content)
           do (draw-page
               content
               :width (* *page-width-inches* 72)
               :height (* *page-height-inches* 72)
               :margins '(72 36 72 36)
               :header-height 36
               :footer-height 36)))
      (pdf:write-document file))))

(defun draw-page (content &key width height margins (header-height 12) (footer-height 12))
  (destructuring-bind (left &optional (top left) (right left) (bottom top)) margins
    (let* ((x left)
           (y (- height top))
           (content-width (- width left right))
           (content-height (- height top bottom))
           (main-height (- content-height header-height footer-height)))
      (pdf:with-page (:bounds (vector 0 0 width height))
        (pdf:with-saved-state
          (typeset::stroke
           (make-filled-vbox content  content-width main-height :top) x (- y header-height))
          (typeset::stroke (compile-header content-width header-height) x y)
          (typeset::stroke (compile-footer content-width footer-height) x (+ bottom footer-height)))))))

(defmacro compute-natural-width (&body body)
  `(typeset::compute-boxes-natural-size
    (typeset::boxes 
     (compile-text ()
       ,@body))
    'typeset::dx))

(defun compile-header (content-width header-height)
  (declare (ignore header-height))
  (let* ((second-col-width (compute-natural-width (page-number-contents 999 999)))
         (first-col-width (- content-width second-col-width)))
    (table 
      (:col-widths (list first-col-width second-col-width)
                   :border 0
                   :padding 0
                   :cell-padding 0)
      (row ()
        (cell () 
          (paragraph
              (:h-align :left :font "Times-Italic" :font-size 10)
            (put-string 
             (get-contextual-variable 'chapter-name))))
        (cell ()
          (page-number-contents 
           pdf:*page-number*
           (find-ref-point-page-number :the-end)))))))

(defun page-number-contents (page max)
  (paragraph
      (:h-align :right :font "Times-Italic" :font-size 10)
    (put-string (format nil "Page ~d of ~d" page max))))



(defun compile-footer (content-width footer-height)
  (let ((cols 2))
    (table 
      (:col-widths (loop repeat cols collect (/ content-width cols)) :border 0
                   :padding 0 :cell-padding 0)
      (row (:height footer-height)
        (cell (:v-align :bottom) 
          (paragraph
              (:h-align :left :font "Times-Italic" :font-size 10)
            (put-string
	     (format nil "Copyright ~c ~a" +copyright+ *copyright*))))
        
        (cell (:v-align :bottom)
          (paragraph
              (:h-align :right :font "Times-Italic" :font-size 10)
            (put-string (date-string))))))))

(defun font-size (type)
  "Determine the font size for a given paragraph type, based on the current default."
  (case type
    (:blockquote (1- *default-font-size*))
    (:example (- *default-font-size* 4))
    (t *default-font-size*)))


(defgeneric emit-pdf (thing)
  (:documentation "Emit part of a sexp-marked document as PDF."))

(defmethod emit-pdf ((thing string))
  (if (or *significant-whitespace* *no-mdash*)
    (verbatim thing)
    (fix-character-typography thing)))

(defmethod emit-pdf ((thing character))
  (emit-pdf (string thing)))

(defun fix-character-typography (string)
  (loop for start = 0 then (+ end 2)
     for end = (search "--" string :start2 start)
     do (put-string (subseq string start end))
     when end do (put-string (format nil "~c" +mdash+))
     while end))

(defmethod emit-pdf ((thing cons))
  (emit-pdf-by-type (car thing) (cdr thing)))

(defgeneric emit-pdf-by-type (type children)
  (:documentation "Called by emit-pdf for tagged elements."))

;;; Paragraph styles

#+(or)(defmethod emit-pdf-by-type ((type t) children)
  "Catchall emitter--uses red text so we notice if we missed any thing."
  (paragraph
   (:h-align :left :font "Helvetica" :font-size 12 :color '(1.0 0.0 0.0))
   (dolist (c children) (emit-pdf c))))

(defmethod emit-pdf-by-type ((type t) children)
  (dolist (c children) (emit-pdf c)))

(defmethod emit-pdf-by-type ((type (eql :h1)) children)
  ;;(typeset::fresh-page)
  (set-contextual-variable 'chapter-name (first children))
  (paragraph
   (:h-align :left :font "Helvetica" :font-size 18)
   (vspace 24)
   (dolist (c children) (emit-pdf c))))
  
(defmethod emit-pdf-by-type ((type (eql :h2)) children)
  (paragraph
      (:h-align :left :font "Helvetica" :font-size 16)
    (vspace 18)
    (dolist (c children) (emit-pdf c))))

(defmethod emit-pdf-by-type ((type (eql :h3)) children)
  (paragraph
   (:h-align :left :font "Helvetica" :font-size 12)
   (vspace 12)
   (dolist (c children) (emit-pdf c))))

(defmethod emit-pdf-by-type ((type (eql :p)) children)
  (let ((typeset::*leading-ratio* *my-leading-ratio*))
    (paragraph
     (:h-align :left :font "Times-Roman" :font-size (font-size type))
     (vspace 12)
     (dolist (c children) (emit-pdf c)))))

(defmethod emit-pdf-by-type ((type (eql :blockquote)) children)
  (paragraph
   (:h-align :left :font "Times-Roman" :font-size (font-size type) :left-margin 18)
   (vspace 12)
   (dolist (c children) (emit-pdf c))))

(defmethod emit-pdf-by-type ((type (eql ':example)) children)
  (paragraph
   (:h-align :left :font "Courier" :font-size (font-size type) :left-margin 18)
   (vspace 12)
   (let ((*significant-whitespace* t))
     (dolist (c children) (emit-pdf c)))))

(defvar *list-type* :bullets)
(defvar *list-number*)

(defmethod emit-pdf-by-type ((type (eql :bullets)) children)
  (vspace 12)
  (loop for (c next) on children
        do (emit-pdf c)
        (when next (vspace 6))))

(defmethod emit-pdf-by-type ((type (eql :numbered)) children)
  (let ((*list-type* :numbered)
        (*list-number* 0))
    (vspace 12)
    (loop for (c next) on children
       for number from 1
       do (emit-pdf c)
         (when next (vspace 6)))))

(defmethod emit-pdf-by-type ((type (eql :sidebar)) children)
  (paragraph
      (:h-align :left :font "Helvetica" :font-size (1- (font-size type)) :left-margin 18)
    (vspace 12)
    (dolist (c children) (emit-pdf c))))

(defmethod emit-pdf-by-type ((type (eql :sidebarhead)) children)
  (paragraph
      (:h-align :left :font "Helvetica-Oblique" :font-size 10 :left-margin 18)
    (dolist (c children) (emit-pdf c))))


;;   (:h-align :left :font "Helvetica" :font-size 12 :color '(1.0 0.0 0.0) :left-margin 24)

(defmethod emit-pdf-by-type ((type (eql :item)) children)
  (paragraph
      (:h-align :left :font "Times-Roman" :font-size (font-size type) :left-margin 24)
    (case *list-type*
      (:bullets
       (put-string (format nil "~c " +bullet+)))
      (:numbered
       (put-string (format nil "~d. " (incf *list-number*)))))
    (dolist (c children) (emit-pdf c))))

#+(or)(defmethod emit-pdf-by-type ((type (eql :figure)) children)
  (book::draw-figure (book::get-figure (first children))))

(defmethod emit-pdf-by-type ((type (eql :table)) children)
  (declare (ignorable children))
  (let* ((num-columns (length (first children)))
	 (column-widths (make-list num-columns :initial-element (floor (/ (* *page-width-inches* 72) num-columns)))))
    (vspace 12)
    (dolist (c children)
      (table
	  (:col-widths column-widths :background-color '(1 1 1) :cell-padding 3 :padding 0)
	(emit-pdf c)))))

(defmethod emit-pdf-by-type ((type (eql :tablecaption)) children)
  (paragraph
      (:h-align :left :font "Helvetica-Oblique" :font-size 9)
    (vspace 3)
    (dolist (c children) (emit-pdf c))))

(defmethod emit-pdf-by-type ((type (eql :figurecaption)) children)
  (paragraph
      (:h-align :left :font "Helvetica-Oblique" :font-size 9)
    (vspace 3)
    (dolist (c children) (emit-pdf c))))


(defmethod emit-pdf-by-type ((type (eql :tr)) children)
  (row () (dolist (c children) (emit-pdf c))))

(defmethod emit-pdf-by-type ((type (eql :td)) children)
  (cell
      (:background-color '(1.0 1.0 1.0))
    (paragraph
	(:h-align :left :font "Times-Roman" :font-size 10)
      (dolist (c children) (emit-pdf c)))))

;;; Character styles

#+(or)(defmethod emit-pdf-by-type ((type (eql :review)) children)
  (with-style (:color '(1.0 0.0 0.0))
    (dolist (c children) (emit-pdf c))))

(defmethod emit-pdf-by-type ((type (eql :i)) children)
  (with-style (:font "Times-Italic")
    (dolist (c children) (emit-pdf c))))

(defmethod emit-pdf-by-type ((type (eql :b)) children)
  (let ((newfont (cond
                   ((string-equal (pdf:name typeset::*font*) "Times-Roman")
                    "Times-Bold")
                   (t (format nil "~a-Bold" (pdf:name typeset::*font*))))))
  (with-style (:font newfont)
    (dolist (c children) (emit-pdf c)))))

(defmethod emit-pdf-by-type ((type (eql :title)) children)
  (with-style (:font "Times-Italic")
    (dolist (c children) (emit-pdf c))))

(defmethod emit-pdf-by-type ((type (eql :code)) children)
  (let ((*no-mdash* t))
    (with-style (:font "Courier")
      (dolist (c children) (emit-pdf c)))))

(defmethod emit-pdf-by-type ((type (eql :url)) children)
  (emit-pdf-by-type :code children))

(defmethod emit-pdf-by-type ((type (eql :link)) children)
  (emit-pdf (second children)))

(defmethod emit-pdf-by-type ((type (eql :cl)) children)
  (with-style (:font "Courier-Bold")
    (dolist (c children) (emit-pdf c))))

(defmethod emit-pdf-by-type ((type (eql :note-ref)) children)
  (with-superscript ()
    (emit-pdf (princ-to-string (first children)))))

(defun emit-note (counter note)
  (destructuring-bind ((first-type &rest first-body) &rest rest) (rest note)
    (dolist (c `((,first-type ,(format nil "~d. " counter) ,@first-body) ,@rest))
      (emit-pdf c))))

(defun date-string (&optional (utc (get-universal-time)))
  (multiple-value-bind (sec min hour date month year day daylight-p zone)
      (decode-universal-time utc)
    (declare (ignorable sec min hour date month year day daylight-p zone))
    (format nil "~d ~[~;~
                      January~;~
                      February~;~
                      March~;~
                      April~;~
                      May~;~
                      June~;~
                      July~;~
                      August~;~
                      September~;~
                      October~;~
                      November~;~
                      December~;~] ~4d, ~d:~2,'0d ~:[am~;pm~]" date month year (1+ (mod (1- hour) 12)) min (>= hour 12))))