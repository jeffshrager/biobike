(in-package :com.gigamonkeys.markup)

(defparameter *tab-width* 8 
  "The width of a tab for purposes of computing indentation.")

;; Bound by parse-stream
(defvar *nodes*)
(defvar *handlers*)
(defvar *filters*)

;; Not bound -- to be bound by callers of parse-stream
(defvar *paragraph-tags* ())
(defvar *subdocument-tags* ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Node classes -- used to represent the document being built.

(defclass node ()
  ((children
    :initform (make-array 10 :adjustable t :fill-pointer 0)
    :accessor children-of)))

(defclass document (node)
  ((current-paragraph 
    :initform nil
    :accessor current-paragraph-of)))

(defclass paragraph (node)
  ((name
    :initarg :name
    :initform "p"
    :accessor name-of)))

(defclass span (node)
  ((name :initarg :name :accessor name-of)))

(defclass text-node ()
  ((text
    :initarg :text
    :initform (make-array 10 :adjustable t :fill-pointer 0 :element-type 'character)
    :accessor text-of)))

(defclass name-node (text-node) ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Parsing functions.

(defun parse-stream (in)
  (let ((*nodes* ())
	(*handlers* ())
	(*filters* ()))
    (let ((document (push-node 'document)))
      (push-handler #'document-handler)
      (loop for char = (read-char in nil nil) while char do (handle char))
      (to-sexp (pop-to-node document)))))

(defun parse-file (file) (with-open-file (in file) (parse-stream in)))

(defun parse-text (text) (with-input-from-string (in text) (parse-stream in)))

(defun handle (char)
  (if *filters*
      (destructuring-bind (f &rest rest) (reverse *filters*)
	(funcall f char rest))
      (handle/no-filter char)))

(defun handle/no-filter (char)
  (funcall (peek-handler) char))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Build document out of nodes put on *nodes* stack

(defgeneric add-child (node child))

(defmethod add-child ((node node) child)
  (vector-push-extend child (children-of node)))

(defmethod add-child ((node text-node) child)
  (declare (ignore child))
  (error "Text nodes can't have children."))

(defun list-paragraph-name (marker)
  (ecase marker
    (#\@ "bullets")
    (#\# "numbered")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Handler functions. These functions are also kept on a stack that
;;; loosely mirrors the stack of nodes being built.

(defun document-handler (char)
  (case char
    (#\Space (push-handler (make-indentation-measurer)))
    (#\* 
     (push-handler (make-outline-name-handler))
     (handle char))
    (#\\ (push-handler #'slash-handler))
    (#\Newline )
    (:blank-line)
    (t
     (push-handler #'paragraph-handler)
     (push-node 'paragraph)
     (handle char))))

(defun paragraph-handler (char)
  (case char
    (#\\ (push-handler #'slash-handler))
    (#\Newline
     (save-character char)
     (push-handler (make-possible-blank-line-handler)))
    (:blank-line 
     (pop-to-type 'paragraph)
     (pop-handler)
     (handle char))
    (t (save-character char))))

(defun make-possible-blank-line-handler ()
  (let ((spaces 0))
    #'(lambda (char)
	(case char
	  (#\Space (incf spaces 1))
	  (#\Tab (incf spaces *tab-width*))
	  (#\Newline
	   (pop-handler)
	   (handle :blank-line))
	  (t 
	   (pop-handler)
	   (dotimes (i spaces) (handle #\Space))
	   (handle char))))))

(defun make-indentation-measurer ()
  (let ((spaces 1))
    #'(lambda (char)
	(case char
	  (#\Space (incf spaces))
	  (#\Tab (incf spaces *tab-width*))
	  (#\Newline
	   (pop-handler)
	   (handle char))
	  ((#\@ #\#)
	   (pop-handler)
	   (push (make-indented-section-filter spaces) *filters*)
	   (push-node 'paragraph :name (list-paragraph-name char))
	   (push-handler (make-list-handler char))
	   (handle char))
	  (t
	   (pop-handler)
	   (push (make-indented-section-filter spaces) *filters*)
	   (ecase spaces
	     (2 (push-handler #'blockquote-handler))
	     (4 (push-handler #'code-handler)))
	   (handle char))))))

(defun blockquote-handler (char)
  (case char
    (#\Space (error "Space at beginning of blockquote."))
    (#\\ (push-handler #'slash-handler))
    (#\Newline )
    (:blank-line)
    (t
     (push-handler #'paragraph-handler)
     (push-node 'paragraph :name "blockquote")
     (handle char))))

(defun code-handler (char)
  (case char
    (#\Space (error "Space at beginning of code."))
    (#\\ (push-handler #'slash-handler))
    (#\Newline )
    (:blank-line)
    (t
     (push-handler #'code-paragraph-handler)
     (push-node 'paragraph :name "example")
     (handle char))))

(defun code-paragraph-handler (char)
  (case char
    (#\\ (push-handler #'slash-handler))
    (t (save-character char))))

(defun make-list-handler (marker)
  #'(lambda (char)
      (cond
	((eql char marker)
	 (push-handler (make-list-item-handler marker))
	 (handle char))
	((eql char #\Space)
	 (push-handler (make-indentation-measurer))))))

(defun make-list-item-handler (marker)
  #'(lambda (char)
      (cond
	((eql char marker)
	 (when (list-item-p (peek-node)) (pop-to-node (peek-node)))
	 (push-node 'paragraph :name "item")
	 (push-handler #'list-item-paragraph-handler))
	((eql char :blank-line))
	((eql char #\Space)
	 (push-handler (make-indentation-measurer)))
	(t 
	 (push-node 'paragraph)
	 (push-handler #'paragraph-handler)
	 (handle char)))))

(defun list-item-p (thing)
  (and (typep thing 'paragraph)
       (string= (name-of thing) "item")))

(defun list-item-paragraph-handler (char)
  (ecase char
    (#\Space
     (pop-handler)
     (push-node 'paragraph)
     (push-handler #'paragraph-handler))))

(defun slash-handler (char)
  (case char
    ((#\\ #\{ #\} #\* #\@ #\#)
     (save-character char)
     (pop-handler))
    (t
     (when (typep (peek-node) 'text-node)
       (pop-to-node (peek-node)))
     (pop-handler)
     (push-handler #'tag-name-handler)
     (push-node 'name-node)
     (save-character char))))

(defun tag-name-handler (char)
  (case char
    (#\{
     (pop-handler)
     (let ((node (pop-node)))
       (assert (typep node 'name-node))
       (let* ((name (text-of node))
	      (node 
	       (cond
		 ((typep (peek-node) 'document)
		  (cond
		    ((paragraph-tag-p name)
		     (push-node 'paragraph :name name))
		    (t
		     (push-node 'paragraph)
		     (push-handler #'paragraph-handler)
		     (push-node 'span :name name))))
		 (t
		  (push-node 'span :name name)))))
	 (if (subdocument-tag-p name)
	     (push-handler (make-brace-document-handler node))
	     (push-handler (make-brace-handler node))))))
    (t
     (save-character char))))

(defun make-brace-handler (node)
  #'(lambda (char)
      (case char
	(#\\ (push-handler #'slash-handler))
	(#\}
	 (pop-handler)
	 (pop-to-node node))
	(t
	 (save-character char)))))

(defun make-brace-document-handler (node)
  #'(lambda (char)
      (case char
	(#\Space (push-handler (make-indentation-measurer)))
	(#\\ (push-handler #'slash-handler))
	(#\Newline )
	(:blank-line)
	(#\}
	 (pop-handler)
	 (pop-to-node node))
	(t
	 (push-handler #'in-brace-paragraph-handler)
	 (push-node 'paragraph)
	 (handle char)))))

(defun in-brace-paragraph-handler (char)
  (case char
    (#\\ (push-handler #'slash-handler))
    (#\Newline
     (save-character char)
     (push-handler (make-possible-blank-line-handler)))
    ((:blank-line #\})
     (pop-to-type 'paragraph)
     (pop-handler)
     (handle char))
    (t (save-character char))))

(defun make-outline-name-handler ()
  (let ((outline-level 0))
    #'(lambda (char)
	(case char
	  (#\* (incf outline-level))
	  (#\Space
	   (push-node 'paragraph :name (format nil "h~d" outline-level))
	   (pop-handler)
	   (push-handler #'paragraph-handler))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Filters

(defun make-indented-section-filter (indentation)
  (let ((seen indentation)
	(current-node-at-start (peek-node))
	(current-handler-at-start (peek-handler)))
    #'(lambda (char next-filters)
	(flet ((pass-on (char)
		 (if next-filters
		     (funcall (first next-filters) char (rest next-filters))
		     (handle/no-filter char))))
	  (case char
	    (#\Space 
	     (if (< seen indentation) (incf seen) (pass-on char)))
	    (#\Newline
	     (setf seen 0)
	     (pass-on char))
	    (:blank-line
	     (setf seen 0)
	     (pass-on char))
	    (t
	     (when (< seen indentation)
	       (pop *filters*)
	       (pop-below-node current-node-at-start)
	       (pop-below-handler current-handler-at-start)
	       (when next-filters
		 (dotimes (i indentation) 
		   (pass-on #\Space))))
	     (pass-on char)))))))

(defun paragraph-tag-p (name)
  (member name *paragraph-tags* :test #'string=))

(defun subdocument-tag-p (name)
  (member name *subdocument-tags* :test #'string=))

(defun save-character (char)
  (unless (typep (peek-node) 'text-node)
    (push-node 'text-node))
  (vector-push-extend char (text-of (peek-node))))


;;; Convert nodes to sexps for further manipulation

(defgeneric to-sexp (thing))

(defmethod to-sexp ((thing document))
  (map 'list #'to-sexp (children-of thing)))

(defmethod to-sexp ((thing paragraph))
  (let* ((tag (intern (string-upcase (name-of thing)) :keyword))
	 (children 
	  (map 'list #'to-sexp
	       (cond
		 ((and (eql tag :li) (= (length (children-of thing)) 1))
		  (children-of (aref (children-of thing) 0)))
		 (t (children-of thing))))))
    (if (stringp (first (last children)))
	(setf (first (last children)) (string-right-trim '(#\Newline) (first (last children)))))
    `(,tag ,@children)))

(defmethod to-sexp ((thing text-node))
  (text-of thing))

(defmethod to-sexp ((thing span))
  `(,(intern (string-upcase (name-of thing)) :keyword) ,@(map 'list #'to-sexp (children-of thing))))

(defun push-handler (handler) (push handler *handlers*))
(defun peek-handler () (first *handlers*))
(defun pop-handler () (pop *handlers*))

(defun push-node (type &rest args) (first (push (apply #'make-instance type args) *nodes*)))
(defun peek-node () (first *nodes*))
(defun pop-node () (pop *nodes*))

(defun pop-to-type (type)
  (loop for node = (pop-node)
     do (add-child (peek-node) node)
       when (typep node type) return node))

(defun find-type (type)
  (find-if #'(lambda (x) (typep x type)) *nodes*))

(defun pop-to-node (target)
  (loop for node = (pop-node)
     when (peek-node) do (add-child (peek-node) node)
     when (eql node target) return node))

(defun pop-below-node (target)
  (loop for node = (peek-node)
     until (eql node target)
     do 
       (let ((node (pop-node)))
	 (when (peek-node)
	   (add-child (peek-node) node)))))

(defun pop-below-handler (target)
  (loop for handler = (peek-handler)
     until (eql handler target)
     do  (pop-handler)))


