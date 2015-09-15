;;
;; Copyright (c) 2005, Gigamonkeys Consulting All rights reserved.
;;

(in-package :com.gigamonkeys.markup)

(defvar *indentation*)

(defclass input ()
  ((in              :initarg :in  :accessor in)
   (eof-character   :initform nil :accessor eof-character)
   (read-eof        :initform nil :accessor read-eof)
   (line            :initform 0   :accessor line)
   (column          :initform 0   :accessor column)
   (buffered-spaces :initform 0   :accessor buffered-spaces)
   (eof-start :initform nil :accessor eof-start)))

(defmacro with-eof ((char input) &body body)
  (once-only (char input)
    (with-gensyms (original-eof original-eof-start)
      `(let ((,original-eof (eof-character ,input))
	     (,original-eof-start (eof-start ,input)))
	 (unwind-protect 
	      (progn 
		(setf (eof-character ,input) ,char)
		(setf (eof-start ,input) (list (line ,input) (column ,input)))
		,@body)
	   (setf (eof-character ,input) ,original-eof
		 (eof-start ,input) ,original-eof-start))))))

(defmethod (setf eof-character) :after (value (input input))
  (declare (ignore value))
  (setf (read-eof input) nil))

(defun read-character (input)
  (cond
    ((plusp (buffered-spaces input))
     (decf (buffered-spaces input))
     (incf (column input))
     #\Space)
    (t
     (let ((char (peek-character input)))
       (cond
	 (char
	  (read-char (in input) nil nil)
	  (incf (column input))
	  (when (char= char #\Newline)
	    (incf (line input))
	    (setf (column input) 0)
	    (buffer-spaces input)))
	 ((not (read-eof input))
	  (read-char (in input) nil nil)
	  (setf (read-eof input) t)))
       char))))

(defun buffer-spaces (input)
  (loop for char = (peek-char nil (in input) nil nil)
     when (eql char #\Space) do 
       (read-char (in input) nil nil)
       (incf (buffered-spaces input))
     else when (and *tab-width* (eql char #\Tab)) do
       (read-char (in input) nil nil)
       (incf (buffered-spaces input) *tab-width*)
     else return nil)
  (when (eql (peek-char nil (in input) nil nil) #\Newline)
    (setf (buffered-spaces input) 0)))

(defun peek-character (input)
  (cond
    ((plusp (buffered-spaces input)) #\Space)
    ((read-eof input) nil)
    (t
     (let ((char (peek-char nil (in input) nil nil)))
       (cond
	 ((eql char (eof-character input)) nil)
	 ((null char)
	  (error "Unexpected EOF at ~d:~d on stream when eof-character is ~c started at ~a" (line input) (column input) (eof-character input) (eof-start input)))
	 (t char))))))
  
(defun read-document (in)
  (loop with current-list = nil
     for paragraph = (read-paragraph in)

     when (list-item-p paragraph)
     do (push paragraph current-list)
       
     when (and current-list (not (list-item-p paragraph)))
     collect (make-list-paragraph (nreverse current-list)) and
     do (setf current-list nil)

     when (and paragraph (not (list-item-p paragraph)))
     collect paragraph

     while paragraph))

(defun make-list-paragraph (items)
  (let ((list-type (ecase (caar items)
		     (:numbered-list-item :numbered)
		     (:bulleted-list-item :bullets))))
    `(,list-type 
      ,@(loop for item in items collect `(:item ,@(rest item))))))
  

(defun read-paragraph (in)
  (let ((*indentation* 0))
    (let ((start (read-paragraph-start in)))
      (nconc
       (cond
	 ((null start) (return-from read-paragraph nil))
	 ((outline-marker-p start) (list (outline-tag start)))
	 ((indentation-p start) 
	  (multiple-value-bind (tag to-strip spaces) (indented-paragraph-tag start)
	    (setf *indentation* to-strip)
	    (list* tag (if (plusp spaces) (list (make-string spaces :initial-element #\Space))))))
	 ((numbered-list-p start)
	  (list :numbered-list-item))
	 ((bulleted-list-p start)
	  (list :bulleted-list-item))
	 ((or (paragraph-p start) (subdocument-p start)) 
	  (return-from read-paragraph start))
	 (t (list :p start)))
       (loop for thing = (read-thing in)
	  until (or (not thing) (eql thing 'blank-line))
	  collect thing)))))

#+(or)(defun read-paragraph (in)
  (let ((*indentation* 0))
    (let ((start (read-paragraph-start in)))
      (flet ((read-rest ()
	       (loop for thing = (read-thing in)
		  until (or (not thing) (eql thing 'blank-line))
		  collect thing)))
	(cond
	  ((null start) nil)
	  ((outline-marker-p start) (list* (outline-tag start) (read-rest)))
	  ((indentation-p start) 
	   (multiple-value-bind (tag to-strip spaces) (indented-paragraph-tag start)
	     (setf *indentation* to-strip)
	     (if (plusp spaces)
		 (list* tag (make-string spaces :initial-element #\Space) (read-rest))
		 (list* tag (read-rest)))))
	  ((numbered-list-p start) (list* :numbered-list-item (read-rest)))
	  ((bulleted-list-p start) (list* :bulleted-list-item (read-rest)))
	  ((or (paragraph-p start) (subdocument-p start))  start)
	  (t (list* :p start (read-rest))))))))

;;; Should read lists like a kind of sub-document: 

(defun read-paragraph-start (in)
  (loop
     (let ((char (peek-character in)))
       (if char
	 (case char
	   (#\Space   (return (read-indentation in)))
	   (#\*       (return (read-outline-marker in)))
	   (#\Newline (read-character in))
	   (t         (return (read-thing in))))
	 (return (read-character in))))))

(defun read-indentation (in)
  (let ((spaces (read-and-count in #\Space))
	(next (peek-character in)))
    (cond
      ((eql next #\#)
       (read-character in)
       (read-and-count in #\Space)
       (list 'numbered-list))
      ((eql next #\@)
       (read-character in)
       (read-and-count in #\Space)
       (list 'bulleted-list))
      ((not (eql next #\Newline))
       (list 'indentation spaces))
      (t nil))))

(defun read-outline-marker (in)
  (let ((level (read-and-count in #\*)))
    (read-and-count in #\Space)
    (list 'outline level)))

(defun read-and-count (in char)
  (loop while (eql (peek-character in) char)
       count t
       do (read-character in)))

(defun read-thing (in)
  (let ((char (peek-character in)))
    (if char
      (case char
	(#\\ (read-slash in))
	(#\Newline (read-newline in))
	(t (read-text in)))
      (read-character in))))

(defun read-slash (in)
  (read-character in)
  (cond
    ((let ((eof (eof-character in)))
       (and eof (with-eof (nil in) (eql (peek-character in) eof))))
     (string (with-eof (nil in) (read-character in))))
    ((member (peek-character in) '(#\\ #\# #\@))
     (string (read-character in)))
    (t (read-tag in))))

(defun read-tag (in)
  (let ((name (intern (string-upcase (read-name in)) :keyword)))
    (list* 
     name
     (with-eof (#\} in)
       (if (subdocument-tag-p name)
	 (read-document in)
	 (loop for thing = (read-thing in)
	    while thing collect thing))))))

;;; Should probably put some sanity checking here, such as that the
;;; name is only legal name characters. otherwise an unescaped #\\
;;; silently eats a big chunk of the document (i.e. up to the next {)
(defun read-name (in)
  (with-output-to-string (s)
    (with-eof (#\{ in)
      (loop for char = (read-character in)
	 while char do (write-char char s)))))

(defun read-newline (in)
  (read-character in)
  (cond
    ((eql (peek-character in) #\Newline)
     (read-character in)
     'blank-line)
    (t
     (eat-indentation in)
     #\Newline)))

(defun eat-indentation (in)
  (loop repeat *indentation*
       while (eql (peek-character in) #\Space)
       do (read-character in)))

(defun read-text (in)
  (with-output-to-string (s)
    (loop for char = (peek-character in)
       while (and char (not (member char '(#\\ #\Newline))))
       do (write-char (read-character in) s))))

(defun indentation-p (thing)
  (and (consp thing) (eql (car thing) 'indentation)))

(defun numbered-list-p (thing)
  (and (consp thing) (eql (car thing) 'numbered-list)))

(defun bulleted-list-p (thing)
  (and (consp thing) (eql (car thing) 'bulleted-list)))

(defun outline-marker-p (thing)
  (and (consp thing) (eql (car thing) 'outline)))

(defun paragraph-tag-p (thing)
  (and (symbolp thing) (member thing *paragraph-tags*)))

(defun paragraph-p (thing)
  (and (consp thing) (paragraph-tag-p (car thing))))

(defun subdocument-tag-p (thing)
  (and (symbolp thing) (member thing *subdocument-tags*)))

(defun subdocument-p (thing)
  (and (consp thing) (subdocument-tag-p (car thing))))

(defun list-item-tag-p (thing)
  (and (symbolp thing) (member thing *list-items-tags*)))

(defun list-item-p (thing)
  (and (consp thing) (list-item-tag-p (car thing))))

(defun outline-tag (level)
  (intern (format nil "~a~d" *outline-tag-base* (second level)) :keyword))

(defun indented-paragraph-tag (indentation)
  (let ((level (second indentation))
	(to-strip nil)
	(best-tag nil))
    (loop for (tag-level . tag) in *indented-paragraph-tags*
       when (>= level tag-level) do
	 (setf to-strip tag-level)
	 (setf best-tag tag))
    (if best-tag
	(values best-tag to-strip (- level to-strip))
	(error "No tag specified for ~r space~:p of indentation" level))))

(defun parse-file (file)
  (with-open-file (in file)
    (handler-bind ((error #'(lambda (e) (break "~a at ~d" e (file-position in)))))
      (read-document (make-instance 'input :in in)))))