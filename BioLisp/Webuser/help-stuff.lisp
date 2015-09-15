;;; -*- Package: weblistener; mode: lisp; base: 10; Syntax: Common-Lisp; -*-

(in-package :weblistener)

;;; +=========================================================================+
;;; | Copyright (c) 2002-2006 JP Massar, Jeff Shrager, Mike Travers           |
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

;;; Authors:  JP Massar, Jeff Shrager.

(defun pp-doc-string (ds)
  (let ((len (length ds)))
    (if (< len 50) (format t "~A~%" ds) (format t "~A ...~%" (subseq ds 0 50)))
    ))

(defun pp-symbol-and-doc-string (s score maxwidth)
  (let* ((ds (or (documentation s 'function) (documentation s 'variable)))
         (len (length ds))
         (slen (length (symbol-name s))))
    (list
     s
     score
     (cond
      ((null ds) (formatn "<no documentation>"))
      ((or (null maxwidth) (<= len (- maxwidth slen))) ds)
      (t (formatn "~A ..." (subseq ds 0 (- maxwidth slen))))
      ))))

(defun current-user-package ()
  (let ((user-package-symbol 'wb:*username*))
    (cond
     ((boundp user-package-symbol) 
      (let ((val (symbol-value user-package-symbol)))
        (and val (symbolp val) (find-package val))
        ))
     (t (ierror "WB:*USERNAME* should be bound!"))
     )))

(defstruct editable-fname-list names)

(defun my-stuff ()
  #.(one-string-nl
     "List the names and types of user-defined variables and functions. "
     "This just prints its output to the screen.  To get a list of your "
     "stuff for further processing use: (my-stuff-as-a-list).  Functions"
     "are live links that when clicked put the function definition into"
     "the eval buffer for editing.")
  (let ((user-package (current-user-package))
        (variable-list nil)
        (function-list nil))
    (when (packagep user-package)
      (loop for s in (my-stuff-as-a-list)
            when (eq user-package (symbol-package s)) 
            do
            (when (boundp s) (push s variable-list))
            (when (fboundp s) (push s function-list)))
      (setq variable-list (sort variable-list 'string-lessp :key 'symbol-name))
      (setq function-list (sort function-list 'string-lessp :key 'symbol-name))
      (format t "~%Your Variables:~%~%")
      (loop for v in variable-list
            as bound? = (boundp v)
            as val = (if bound? (symbol-value v) '<<UNBOUND>>)
            as type = (if bound? (type-of val) "")
            do
            (formatt 
             "~A~%"
             (limited-string 
              (formatn "  ~A   Value: ~A   Type: ~A" v val type))
             ))
      (if (null function-list)
          (cformatt "(No functions defined.)")
        (progn
          (format t "~%Your Functions (click to edit):")
          (make-editable-fname-list :names function-list)
          )))))


(defmethod wb::out-record-to-html 
           ((obj editable-fname-list) (string string) &key (pkg nil))
  (declare (ignore pkg))
  (html
   :br
   (:table
    (:tr
     (:th "Name (Edit in Multibox)&nbsp;") 
     (:th "&nbsp;Source File&nbsp;")
     (:th "&nbsp;Scratch File&nbsp;&nbsp;")
     )
    ;; don't try to list any uninterned symbols...
    (loop for name in (remove-if-not 
                       'symbol-package (editable-fname-list-names obj)) 
          as source-file = (system-specific-source name :function)
          as package-name = (package-name (symbol-package name))
          do
          (html 
           (:tr
            (:td
             ((:a :href (make-weblistener-evalstring-url 
                         :evalstring 
                         (url-safe-string (formatn "(wb::editfun ~a)" name))))
              (:princ-safe (maybe-clip-string (string name) 30))
              ))
            ((:td :align "center")
             (if source-file
                 (html 
                  ((:a :href (function-source-file-url name)) "view")
                  "&nbsp;&nbsp;"
                  ((:a :href 
                    (safe-make-editanyfile-url
                     (namestring source-file) nil nil nil))
                   "edit"))
               (html "none")
               ))
            ((:td :align "center")
             (if (or source-file (get name :procedure-definition))
                 (html 
                  ((:a :href 
                    (make-editfunction-url 
                     :function name :package package-name)) "edit"))
               (html "no source")))
            ))))))

;;; This enables users to edit function definitions by re-entering
;;; the definition into the eval box.

(defmacro editfun (name)
  (cond
   ((null name) nil)
   ((symbolp name) `(put-function-definition-in-multiline-box ',name))
   ((stringp name) 
    (let ((symbol (or (find-symbol name) (find-symbol (string-upcase name)))))
      `(editfun ,symbol)))
   ((and (listp name) (eq 'quote (first name)) (symbolp (second name)))
    `(editfun ,(second name)))
   (t (error "Don't know how to edit ~S" name))
   ))
   
(defun put-function-definition-in-multiline-box (name)
  (vwhen (form (get name :procedure-definition))
    (setq *multiline-form-data* (pretty-print-to-string form :downcase))
    name))

(defun pretty-print-to-string (form &optional (print-case *print-case*))
  (with-output-to-string (z)
    (let ((*standard-output* z)
          (*print-level* nil)
          (*print-length* nil)
          (*print-case* print-case))
      (pprint (re^-slotv-lambdas form) z)
      )))

;;; In order to invert the reader macro for #^, we convert anything in
;;; the form that look like this:
;;;   (lambda (frames::x) (slotv frames::x #$bar)) 
;;; to this:
;;;   #^bar

(defstruct temp^printer name)

(defmethod print-object ((object temp^printer) stream) 
  (format stream "#^~a" (temp^printer-name object))) 

(defun re^-slotv-lambdas (form)
  (cond ((null form) nil)
	((and (listp form)
	      (eq 'lambda (car form))
	      (listp (third form))
	      (eq 'slotv (first (third form)))
	      (framep (third (third form))))
	 (make-temp^printer 
          :name (slotv (third (third form)) #$fname)))
	((listp form)
	 (mapcar #'re^-slotv-lambdas form))
	(t form)))

(defun my-stuff-as-a-list (&aux (result nil))
  "A list of symbols which are variables and functions the user has defined."
  (let ((user-package (current-user-package)))
    (when (packagep user-package)
      (do-symbols (s user-package) 
        (when (and (eq user-package (symbol-package s))
                   (or (boundp s) (fboundp s)))
          (push s result)
          )))
    result
    ))

(defun cl-external? (thing)
  (cond 
   ((stringp thing) (vwhen (s (find-symbol thing :cl)) (cl-external? s)))
   ((symbolp thing)
    (and
     (eq (find-package :common-lisp) (symbol-package thing))
     (eq :external 
         (second
            (multiple-value-list (find-symbol (string thing) :common-lisp))
            ))))
   (t nil)
   ))

(defmacro lisp-help (string)
  "Prints out list of Common Lisp symbols containing STRING in their names"
  (cond
   ((symbolp string) `(lisp-help ,(string string)))
   ((quoted-symbol-p string) `(lisp-help ,(string (second string))))
   (t `(lisp-help-aux ,string))
   ))

(defun lisp-help-aux (s)
  (setq s (string-upcase s))
  (terpri)
  (let ((symlist (external-present-apropos-list (string s) :common-lisp)))
    (if symlist
        (loop for sym in (sort symlist 'string-lessp :key 'symbol-name) do
              (formatt "~35A " (symbol-name sym))
              (when (fboundp sym)
                (multiple-value-bind (arglist known?)
                    (system-specific-arglist-of sym)
                  (if (or arglist known?)
                      (formatt "~A" (arglist-to-help-display-string arglist))
                    (formatt "?")
                    )))
              (terpri))
      (formatt "Nothing appropriate~%")
      )))

(defun arglist-to-help-display-string (arglist &optional (limit 40))
  (labels ((all-keywords (x)
             (cond 
              ((null x) nil)
              ((symbolp x) (keywordize x))
              ((listp x) (mapcar #'all-keywords x))
              (t x)
              )))
    (if (null arglist)
        "()"
      (let ((s (limited-string (formatn "~A" (all-keywords arglist)) limit)))
        (if (eql (lastelem s) #\)) s (one-string s ")"))
        ))))

(defun external-present-apropos-list (string package)
  (let ((pkg (find-package package)))
    (remove-if-not
     (lambda (x) 
       (let ((xpkg (symbol-package x)))
         (multiple-value-bind (symbol type)
             (find-symbol (string x) xpkg)
           (declare (ignore symbol))
           (and (eq type :external) (eq pkg xpkg))
           )))
     (apropos-list string package)
     )))
                   





