;;;; -*- mode: Lisp; Syntax: Common-Lisp; Package: nvpl; -*-

(in-package :nvpl) 

;;; +=========================================================================+
;;; | Copyright (c) 2005 JP Massar, Jeff Elhai, Mark Slupesky, Peter Seibel   |
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

;;; Author: JP Massar. 

;;; The documentation for the client representation (ie, box language) 
;;; is in .../Doc/JBML.rtf

(defvar *box-flags* nil)
(defvar *main-menu* nil)
(defvar *collapsed?* nil)

(defmacro with-id-box-flags-and-main-menu 
          (snippet &body goo)
  ``(,(snippet-id ,snippet)
     ,@*box-flags*
     ,@(when *main-menu* (list *main-menu*))
     ,@(splice-lists ,@goo)
     ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; These two functions are the interface.  They get called in 
;;; the commands file in the functions that redraw the client. 

(defun toplevel-snippet->client-representation (snippet)
  (when (hidden? snippet) 
    (vpl-internal-error "Toplevel snippets cannot be hidden!"))
  (when (newline-before? snippet)
    (vpl-internal-error 
     "Toplevel snippets should not have newline indicators!"))
  (provide-background-color-for-snippet-and-children snippet)
  ;; could implement a check here for valid toplevel snippets
  (autonomous-snippet->client-representation snippet)
  )

(defun modified-snippet->client-representation (snippet)
  (if (toplevel-snippet? snippet)
      (toplevel-snippet->client-representation snippet)
    (progn 
      (when (hidden? snippet) 
        (vpl-internal-error "Modified snippets cannot be hidden!"))
      (autonomous-snippet->client-representation snippet)
      )))

(defun autonomous-snippet->client-representation (snippet)
  (splice-text-descriptors 
   (snippet->client-representation snippet)))

;; a snippet can have a :newline flag which indicates that after the snippet's
;; visual representation has been displayed, the client should proceed to the
;; next line.  But we don't want this to happen for hidden nodes which don't
;; actually appear.  

;; So first we get rid of hidden nodes, and then we insert :jbml-cr into the
;; list of representations after every node that indicated a :newline flag. 
;; This would fail if a :newline flag were in a snippet at toplevel, but this
;; should never be the case. 
 
(defun internal-snippets->client-representation (snippets) 
  (let* ((client-representations 
          (mapcar 'snippet->client-representation snippets))
         (hidden-representations? 
          (mapcar (lambda (s) (eq s :hidden-box-node)) client-representations))
         (newlines? (mapcar 'newline-before? snippets)))
    (let ((non-hidden-representations 
           (loop for c in client-representations 
                 for h in hidden-representations?
                 unless h collect c
                 ))
          (non-hidden-newlines? 
           (loop for n in newlines? 
                 for h in hidden-representations?
                 unless h collect n
                 )))
      (labels ((add-newlines (reps newlines?)
                 (let ((rep (first reps))
                       (newline? (first newlines?)))
                   (cond
                    ((null reps) nil)
                    (newline?
                     (list* 
                      :jbml-cr rep (add-newlines (cdr reps) (cdr newlines?))))
                    (t (cons rep (add-newlines (cdr reps) (cdr newlines?))))
                    ))))
        (let ((client-representation
               ;; inline text and associated flags 
               (splice-text-descriptors 
                (add-newlines non-hidden-representations non-hidden-newlines?)
                )))
          client-representation
          ;; hack to make sure newline token is not first box element
          #+oops
          (if (eq (first client-representation) :jbml-cr)
              (cdr client-representation)
            client-representation
            ))))))

(defun snippet->client-representation (snippet)
  (vdbg "Converting snippet type ~A to box...~%" (type-of snippet))
  (flet ((doit () 
           (cond
            ((visualized-as-box? snippet) (snippet->box snippet))
            ((visualized-as-icon? snippet) (snippet->icon snippet))
            ((visualized-as-text? snippet) (snippet->text-info snippet))
            (t 
             (vpl-internal-error 
              "Don't know how to visualize snippet ~A" snippet
              )))))
  (cond
   ((hidden? snippet) :hidden-box-node)
   ((collapsed? snippet) (let ((*collapsed?* t)) (doit)))
   (t (doit))
   )))

(defun visualized-as-icon? (snippet)
  (typecase snippet
    ((or choice-snippet aggregate-snippet) t)
    (otherwise nil)
    ))

(defun visualized-as-text? (snippet)
  (typecase snippet
    (literal-snippet t)
    (otherwise nil)
    ))

(defun visualized-as-box? (snippet)
  (and (not (visualized-as-icon? snippet))
       (not (visualized-as-text? snippet))
       ))

(defmethod snippet->text-info ((snippet snippet))
  (vpl-internal-error "Only literals are currently represented as text!"))

(defmethod snippet->text-info ((snippet literal-snippet))
  (when *collapsed?* 
    (vpl-internal-error "Literals should not be collapsible!"))
  (multiple-value-bind (literal-text literal-flags)
      (literal-snippet-text-and-text-flags snippet)
    (list* :text literal-text literal-flags)
    ))

(defun snippet->icon (snippet)
  (let* ((icon-menu (create-snippet-menu snippet))
         (client-icon (get-client-icon snippet))
         (box-flags (box-flags-and-background snippet))
         (child-representations 
          (unless *collapsed?* 
            (internal-snippets->client-representation
             (snippet-children snippet)
             )))
         (description 
          (vwhen (d (get-snippet-property snippet :description))
            (when (and (or (null child-representations) 
                           (and (= (length child-representations) 1)
                                (snippet-is-hole? 
                                 (first (snippet-children snippet)))))
                       (not *collapsed?*))
              (list d :jbml-i)))))
    ;; (declare (ignore client-icon))
    (setq icon-menu (maybe-non-default-icon-menu icon-menu client-icon))
    (when *collapsed?*
      (designate-menu-title-as-collapsed icon-menu))
    `(,(snippet-id snippet) 
      ,@box-flags 
      ,@child-representations
      ,icon-menu
      ,@description
      )))

(defun maybe-non-default-icon-menu (icon-menu client-icon)
  (if (null client-icon)
      icon-menu 
    ;; Hack the standard menu syntax into the correct syntax for 
    ;; :jbml-options-menu2
    `(
    ;; Menu ID
      ,(first icon-menu)
      :jbml-options-menu2
      ;; Menu title
      ,(third icon-menu)
      ;; all the menu selections
      ,(cdddr icon-menu)
      ;; menu options, currently only ICON exists as an option
      ("ICON" ,client-icon)
      )))

(defun designate-menu-title-as-collapsed (menu)
  (setf (third menu) (s+ (third menu) ""))
  )

(defmethod get-client-icon ((snippet t)) nil)

(defmethod get-client-icon ((snippet aggregate-snippet))
  "images/whitearrowgreen_16x16.gif")


(defun box-flags-and-background (snippet) 
  (let* ((bf (box-flags snippet))
         (bc (get-snippet-property snippet :background-color))
         (bc-hex
          (ecase bc
            (:elhai-yellow nil)
            (:elhai-orange *elhai-orange-background-color*)
            ((nil) nil)
            )))
    (cond
     ((member :jbml-background-color bf) bf)
     (bc-hex (list* :jbml-background-color bc-hex bf))
     (t bf)
     )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *snippet-debug-counter* 0)

(defun save-boxes-for-debugging (snippet boxes)
  (let ((start (get-universal-time)))
    (with-open-file (out (format nil "/tmp/snippet-to-box-~a-~a.sexp" start (incf *snippet-debug-counter*)) :direction :output :if-exists :supersede)
      (with-standard-io-syntax
	(let ((*print-case* :downcase)
	      (*package* #.*package*))
	  (format out "~&;; Snippet type: ~a~%" (type-of snippet))
	  (print boxes out))))))

(defun snippet->box (snippet)
  (vdbg "Creating box representation for snippet of type ~A~%" 
        (type-of snippet))
  (let* ((*main-menu* (create-snippet-menu snippet))
         (box-flags (box-flags-and-background snippet))
         (clear-delete-flag (delete-or-clear-box-flag-for-snippet snippet))
         (*box-flags* 
          (if clear-delete-flag (cons clear-delete-flag box-flags) box-flags)))
    ;; replaces line below
    (snippet->box-method snippet)
    #+very-bad
    (save-boxes-for-debugging snippet
			      (snippet->box-method snippet))
    ))

(defparameter *string-print-limit* 20)
(defparameter *other-print-limit* 30)

;;; SYMBOLS 

(defmethod snippet->box-method ((snippet symbol-snippet))
  (when *collapsed?* 
    (vpl-internal-error "Symbols should not be collapsible!"))
  (let ((text (string-downcase (snippet-value-to-formatted-string snippet))))
    (if (toplevel-ws-snippet? snippet)
        (display-symbol-with-value snippet text)
      (with-id-box-flags-and-main-menu snippet text (text-flags snippet))
      )))

;; Make the values of variables appear in data boxes along with the name
;; of the variable.  
(defun display-symbol-with-value (snippet text)
  (let* ((snippet-value (snippet-value snippet))
         (augmented-text 
          (if (boundp snippet-value)
              (s+ 
               text 
               " = "
               (let ((v (symbol-value snippet-value)))
                 (cond 
                  ((stringp v) 
                   (formatn "~S" (limited-string v *string-print-limit*)))
                  ((frames::isframe? v)
                   (limited-string
                    (string-downcase (frames::fname v)) *string-print-limit*))
                  ((or (vectorp v) (consp v))
                   (let ((*print-case* :downcase))
                     (limited-string (formatn "~S" v) *other-print-limit*)
                     ))
                  (t 
                   (limited-string 
                    (string-downcase (formatn "~S" v)) *other-print-limit*
                    )))))
            text
            )))
    (with-id-box-flags-and-main-menu 
        snippet augmented-text (text-flags snippet)
      )))

;;; CONSTANTS

(defmethod snippet->box-method ((snippet constant-snippet) &aux text)
  (block exit
    (when *collapsed?* 
      (vpl-internal-error "Constants should not be collapsible!"))
    (let ((v (snippet-value snippet)))
      (cond 
       ((stringp v) 
        (setq text (formatn "~S" (limited-string v *string-print-limit*))))
       ((frames::isframe? v)
        (setq 
         text 
         (limited-string
          (string-downcase (frames::fname v)) *string-print-limit*)
         ))
       ((or (vectorp v) (consp v))
        (let ((*print-case* :downcase))
          (setq text (snippet-value-to-formatted-string snippet))
          (setq text (limited-string text *other-print-limit*))))
       ;; make constant symbols use the name = value notation at toplevel
       ((and (toplevel-ws-snippet? snippet) (symbolp (snippet-value snippet)))
        (let ((text (snippet-value-to-formatted-string snippet)))
          (return-from exit 
            (display-symbol-with-value snippet text)
            )))
       (t 
        (setq text (snippet-value-to-formatted-string snippet))
        (setq text (limited-string (string-downcase text) *other-print-limit*))
        )))
    (with-id-box-flags-and-main-menu snippet text (text-flags snippet))
    ))
     
;;; LITERALS

(defmethod snippet->box-method ((snippet literal-snippet))
  (vpl-internal-error "Literals are processed through snippet->text-info!"))

;;; FORMS

(defmethod snippet->box-method ((snippet form-snippet))
  (when *collapsed?* 
    (vpl-internal-error "Holes should not be collapsible!"))
  (cond 
   ((get-snippet-property snippet :hole-open) 
    (open-hole-box snippet))
   ((get-snippet-property snippet :hole-open-multiline)
    (open-multiline-hole-box snippet))
   (t (closed-hole-box snippet))
   ))

(defun open-hole-box (snippet)
  (push :jbml-hole-opened *box-flags*) 
  (let* ((sv (get-snippet-property snippet :contents)) 
         (format (get-snippet-property snippet :format))
         (text (if format (formatn format sv) sv)))
    (with-id-box-flags-and-main-menu snippet text (text-flags snippet))
    ))

(defun open-multiline-hole-box (snippet)
  (vdbg "In open-multiline-hole-box...~%")
  (let ((sv (get-snippet-property snippet :contents)))
    (vdbg "Snippet contents: ~S~%" sv)
    (let ((multiline-data (massage-contents-for-multiline sv)))
      (vdbg "multiline data: ~S~%" multiline-data)
      (push multiline-data *box-flags*)
      (push :jbml-multiline-hole-opened *box-flags*) 
      (with-id-box-flags-and-main-menu snippet (text-flags snippet))
      )))

(defmethod massage-contents-for-multiline ((sv null)) "")

(defmethod massage-contents-for-multiline ((sv string)) 
  sv)

(defmethod massage-contents-for-multiline ((sv t))
  (with-output-to-string (p) 
    (let ((*print-pretty* t)
	  (*print-right-margin* 40)
	  (*print-lines* nil)
	  (*print-length* nil)
	  (*print-level* nil)
	  (*print-circle* t)
	  (*print-readably* t)
	  ;; this doesn't seem to be portable, and furthermore 
	  ;; we don't really know exactly what it's doing...
	  (*print-miser-width* 10)
	  )
      (write sv :stream p))))

(defun closed-hole-box (snippet)
  (push :jbml-hole *box-flags*)
  (let ((text (string-downcase (snippet-label snippet))))
    (with-id-box-flags-and-main-menu snippet text (text-flags snippet))
    ))

;;; CALLS

(defparameter *special-box-methods* 
  (create-hash-table
   '(("{" curly-list-snippet->box)
     )
   :test 'equal
   ))

(defmethod snippet->box-method ((snippet call-snippet))
  (destructuring-bind (name-snippet &rest arg-snippets)
      (snippet-children snippet)
    (let ((text (string (snippet-value name-snippet)))
          (collapsed-text (when *collapsed?* "")))
      (when *collapsed?* 
        (vwhen (label (get-snippet-property snippet :collapsed-name))
          (setq text (string label))
          ))
      (vif (special-handler (gethash text *special-box-methods*))
           (funcall special-handler snippet)
           (cond
            (*collapsed?* 
             (with-id-box-flags-and-main-menu snippet 
               text 
               (text-flags snippet)
               collapsed-text
               ))
            ((ref-snippet? snippet)
             (ref-snippet->box snippet))
            (t
             (with-id-box-flags-and-main-menu snippet 
               text 
               (text-flags snippet)
               (internal-snippets->client-representation arg-snippets)
               )))))))

;;; FLAGS

(defmethod snippet->box-method ((snippet flag-snippet))
  (when *collapsed?* 
    (vpl-internal-error "Flags should not be collapsible!"))
  (setq *main-menu* nil)
  (let ((text (snippet-value-to-formatted-string snippet)))
    (with-id-box-flags-and-main-menu snippet
      text 
      (text-flags snippet)
      )))

;;; KEYWORDS

(defmethod snippet->box-method ((snippet keyword-snippet))
  (when *collapsed?* 
    (vpl-internal-error "Keywords should not be collapsible!"))
  (setq *main-menu* nil)
  (with-id-box-flags-and-main-menu snippet
    (internal-snippets->client-representation (snippet-children snippet))
    ))

;;; PROGN

(defmethod snippet->box-method ((snippet progn-snippet))
  (when *collapsed?* 
    (vpl-internal-error "Progn nodes should not be collapsible!"))
  (unless (get-snippet-property snippet :main-menu)
    (setq *main-menu* nil))
  (let ((d (get-snippet-property snippet :description)))
    (with-id-box-flags-and-main-menu snippet
      (when d (list d :jbml-i))
      (internal-snippets->client-representation (snippet-children snippet))
      )))

;;; TOPLEVEL-OUTPUT

(defmethod snippet->box-method ((snippet toplevel-output-snippet))
  (when *collapsed?* 
    (vpl-internal-error "Toplevel outputs should not be collapsible!"))
  (let ((nvalues (get-snippet-property snippet :nvalues))
        (children (snippet-children snippet)))
    (with-id-box-flags-and-main-menu snippet
      (snippet-label snippet)
      (text-flags snippet)
      (when (= nvalues 1) (list (snippet-value (first children))))
      (when (= nvalues 1) (single-output-value-text-flags))
      (when (/= nvalues 1) (internal-snippets->client-representation children))
      )))

;;; OUTPUT-VALUE

(defmethod snippet->box-method ((snippet output-value-snippet))
  (when *collapsed?* 
    (vpl-internal-error "Output values should not be collapsible!"))
  (with-id-box-flags-and-main-menu snippet
    (snippet-value snippet)
    (text-flags snippet)
    ))
  

(defun snippet-value-to-formatted-string (snippet)
  (let ((sv (snippet-value snippet))
        (format (get-snippet-property snippet :format))
        (default-format "~S"))
    (cond
     ((stringp sv) sv)
     (format (formatn format sv))
     ((not (keywordp sv)) (formatn default-format sv))
     (t 
      (typecase (snippet-parent snippet)
        (keyword-snippet 
         (if (non-bbl-function-node? 
              (snippet-parent (snippet-parent (snippet-parent snippet))))
             (formatn "~S" sv)
           (formatn "~A" sv)
           ))
        (otherwise (formatn "~A" sv))
        )))))

(defun non-bbl-function-node? (snippet)
  (declare (ignore snippet)) 
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun curly-list-snippet->box (snippet)
  (vdbg "In curly...~%")
  (let ((contents (second (snippet-children snippet))))
    (with-id-box-flags-and-main-menu snippet
      "{" :jbml-color curly-bracket-color
      (if (collapsed? snippet)
          "(collapsed)"
        (list (snippet->client-representation contents)))
      "}" :jbml-color curly-bracket-color
      )))

(defun ref-snippet->box (snippet)
  (vdbg "ref-snippet->box...~%")
  (with-id-box-flags-and-main-menu snippet
    (internal-snippets->client-representation (cdr (snippet-children snippet)))
    ))

(defun collapsed? (snippet) (get-snippet-property snippet :collapsed?))

(defun hidden? (snippet) (get-snippet-property snippet :hidden-node))

(defun newline-before? (snippet)  (get-snippet-property snippet :newline))

(defun splice-lists (&rest goo)
  (loop for item in goo append (ensure-list item)))

(defun splice-text-descriptors (client-representation)
  (mapcan
   (lambda (vr)
     (if (and (listp vr) (eq :text (first vr))) 
         (cdr vr)
       (list vr)
       ))
   client-representation
   ))
