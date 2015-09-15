;;; -*- mode: Lisp; Syntax: Common-Lisp; Package: wb; -*-

(in-package :wb)

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

;;; Author:  JP Massar

(defmacro defprotocol

          (&whole dp name sds &body body
                  &aux 
                  arg-forms substitution-lines directives
                  steps-lines
                  hop-body-references hop-history-references
                  *-history-references
                  substituted-body
                  bad-reference
                  )

  (unless (symbolp name) (error "Invalid name for DEFPROTOCOL: ~A" name))
  
  (let* ((*in-history-forms* 
          (map 'vector #'copy-tree 
               (mapcar #'second (reverse *in-history*))))
         (unsubstituted-step-forms nil))

    ;; Parse and canonicalize all the substitution directives
    ;; Also get the arglist and all line numbers to be substituted into.

    (multiple-value-setq 
        (arg-forms substitution-lines directives)
        (parse-substitution-directives sds))

    ;; Find all the line numbers referenced by STEP or STEPS directives.

    (setq steps-lines (all-unique-steps-lines `(progn ,@body)))

    ;; Warn if a line we were told to substitute into is not included
    ;; in the DEFPROTOCOL via a STEPS form.

    (dolist (sub-line substitution-lines)
      (unless (member sub-line steps-lines)
        (warn "A substitution directive uses line ~D, but~%~A"
              sub-line
              "no such line is used in any STEPS form in the body")))

    ;; Make a copy of the unsubstituted history forms referenced,
    ;; so we can display them later and/or write them out to disk.
    ;; (We will do substitutions in place on the forms in *in-history-forms*)

    (setq unsubstituted-step-forms
          (mapcar 
           #'copy-tree
           (mapcar 
            #'(lambda (n) (list n (history-form-from-linenum n)))
            steps-lines
            )))

    ;; Find all references to history lines made
    ;; -- in the DEFPROTOCOL body by HOP forms
    ;; -- in the history lines referred to, by HOP forms or indirectly
    ;;    via use of *, ** or ***.

    (setq hop-body-references
          (all-hop-linenums-and-associated-body-forms body))
    (setq hop-history-references
          (all-hop-linenums-and-associated-history-forms steps-lines))
    (setq *-history-references
          (all-*-linenums-and-associated-history-forms steps-lines))

    (setq bad-reference nil)

    ;; Warn if the result of a line is referenced, but that
    ;; line itself is not included via a STEPS form.

    (flet ((msg (format-string &rest format-args)
             (apply #'format t format-string format-args) (terpri)))
      (loop for (hopln body-form) in hop-body-references do
            (unless (member hopln steps-lines)
            (msg "~&;; ** An HOP form in your DEFPROTOCOL body references")
            (msg ";;   history line ~D, but no such line number" hopln)
            (msg ";;   is used within a STEPS form in your DEFPROTOCOL body.")
            (msg ";; Body form containing invalid reference: ~S" body-form)
            (setq bad-reference t)))
      (loop for (hopln history-form) in hop-history-references do
            (unless (member hopln steps-lines)
              (msg "~&;; ** An HOP form references history line ~D," hopln)
              (msg ";;   but no such line number is used within a STEPS form")
              (msg ";;   in your DEFPROTOCOL body.  (The invalid HOP form is")
              (msg ";;   within a history form referenced by a STEPS")
              (msg ";;   statement in your DEFPROTOCOL body.)")
              (msg ";; Form containing invalid HOP form: ~S" history-form)
              (setq bad-reference t)))
      (loop for (*-linenum history-form) in *-history-references do
            (unless (member *-linenum steps-lines)
              (msg "~&;; An invalid use of '*', '**' or '***' was found.")
              (msg ";;   This use refers to history line number ~D," *-linenum)
              (msg ";;   but no such line number is used with a STEPS form")
              (msg ";;   in your DEFPROTOCOL body.  (The invalid reference")
              (msg ";;   is within a history form reference by a STEPS")
              (msg ";;   statement in your DEFPROTOCOL body.)")
              (msg ";; Form containing invalid reference: ~S" history-form)
              (setq bad-reference t)))
      (when bad-reference
        (error "** One or more references to unused history forms detected"))
      )

    ;; Do all the substitutions the user directed.
    
    (dolist (directive directives)
      (do-defprotocol-substitution directive))
        
    ;; Substitute variables for all the references to history line numbers

    ;; First in the body

    (setq substituted-body
          (mapcar #'replace-hop-references-with-step-variables body))

    ;; Then in all the history lines that are to be included in the program
    
    (dolist (linenum steps-lines)
      (let ((form (history-form-from-linenum linenum)))
        (set-history-form-at-linenum
         linenum
         (replace-history-references-with-step-variables form linenum)
         )))

    ;; Finally replace all the STEPS forms with the substituted in for
    ;; history forms.

    (setq substituted-body (mapcar #'replace-steps-forms substituted-body))

    ;; Create the DEFPROTOCOL function DEFUN form.

    (let* ((argument-list `(&key ,@arg-forms))
           (step-variables-needed (mapcar #'step-variable steps-lines))
           (defun-form
            `(defun ,name ,argument-list
               (let (,@step-variables-needed) 
                 ,@(when step-variables-needed
                     `((declare (ignorable ,@step-variables-needed))))
                 ,@substituted-body
                 )))
           (step-forms unsubstituted-step-forms))

      ;; Save away lots of info about the DEFPROTOCOL function.

      `(progn
         ,defun-form
         (compile ',name)
         (setf (get ',name :defprotocol) ',defun-form)
         (setf (get ',name :defprotocol-form) ',dp)
         (setf (get ',name :defprotocol-steps) ',step-forms)
         (pushnew ',name *protocol-names*)
         ',name
         ))

    ))


(defun isd (sd &optional (s1 "") (s2 ""))
  (error "Invalid substitution directive: ~S.~%  ~A ~A" sd s1 s2))

(defun all-history-linenums ()
  (loop for j from 1 to (length *in-history-forms*) collect j))

(defun history-form-from-linenum (linenum)
  (aref *in-history-forms* (1- linenum)))

(defun set-history-form-at-linenum (linenum new-form)
  (setf (aref *in-history-forms* (1- linenum)) new-form))

(defun verify-line-in-history (ln sd)
  (unless (and (integerp ln) (plusp ln))
    (isd sd "Invalid line number: " ln))
  (unless (<= ln (length *in-history-forms*))
    (error "Out of range line number, ~A, in substitution directive~%~S~%~A"
           ln sd "No such history line number currently exists."))
  ln)


(defvar *substitution-lines-referenced*)

;;; Canonicalize each substitution directive into a list of the form
;;; ((<symbol> <lines> (<method> &rest <method-args>))
;;; Also create a list of (<symbol> <initial-value>) pairs to be
;;; used as the DEFPROTOCOL &key arglist.

(defun parse-substitution-directives (sds)

  (let ((arg-forms nil)
        (directives nil)
        (*substitution-lines-referenced* nil))

    ;; Deal with creating the argument list, allowing the
    ;; same argument to be mentioned in more than one directive.

    (labels ((arg-key (arg) (if (symbolp arg) arg (first arg)))
             (arg-exists? (sf)
             (cond 
              ((symbolp sf) (member sf arg-forms :key #'arg-key))
              (t (member (first sf) arg-forms :key #'arg-key))))
             (add-arg (sf)
               (cond
                ((symbolp sf) (unless (arg-exists? sf) (push sf arg-forms)))
                (t
                 (let ((arg (arg-exists? sf)))
                   (if (consp arg)
                       (warn "Argument ~A defined more than once ~A"
                             "with an initial value.  ")
                     (progn
                       (setq arg-forms (delete sf arg-forms))
                       (push sf arg-forms)
                       )))))))

      ;; For each substitution directive, parse it, build a canonical
      ;; representation, keep track of all the line references made,
      ;; and keep track of the argument symbols and their initial values

      (dolist (sd sds)
        (cond
         ((null sd) (isd sd))
         ((and sd (symbolp sd)) (add-arg sd))
         ((not (listp sd)) (isd sd))
         ((and (symbolp (car sd)) (null (cdr sd))) (add-arg (car sd)))
         ;; We have something of the form (<arg-descriptor> &rest stuff)
         ;; <arg-descriptor> can either be a symbol or 
         ;; (<symbol> <initial-value>)
         (t
          (let ((sf (car sd)) (keys (cdr sd)) (symbol nil))
            ;; Extract the symbol and add the symbol to the arglist
            ;; with its initial value if any, if not already present
            (cond
             ((symbolp sf) (setq symbol sf) (add-arg sf))
             ((not (and (listp sf) (eql (length sf) 2) (symbolp (first sf))))
              (isd sd))
             (t (setq symbol (first sf)) (add-arg sf)))
            ;; Build one or more canonical descriptors from the rest 
            ;; of the list.  The rest is either a list of directive
            ;; keywords, or multiple lists of these lists.  E.g. either
            ;; (:line 1 :position 2) or 
            ;; ((:line 1 :position 2) (:line 2 :value 5) ...)
            (cond 
             ((null keys) nil)
             ((not (listp keys)) (isd sd))
             ;; A single list.  Parse it and create a canonical descriptor.
             ((not (listp (first keys)))
              (push (cons symbol (parse-substitution-keys sd keys)) 
                    directives))
             ;; Multiple lists.  Parse each one.
             (t
              (dolist (skey keys)
                (push (cons symbol (parse-substitution-keys sd skey)) 
                      directives)))))))))

    (values 
     ;; If no initial value give it NIL.
     (mapcar #'(lambda (x) (if (consp x) x (list x nil))) arg-forms)
     *substitution-lines-referenced* 
     directives
     )))
                 
          
(defun parse-substitution-keys 
       (sd keys &aux (lines nil) (how nil) (how-arg nil))
  (unless (evenp (length keys)) (isd sd "Odd number of key-value pairs"))
  (do ((skeys keys (cddr skeys))) ((null skeys))
    (let ((key (first skeys)) (val (second skeys)))
      (case key
        (:line 
         (when lines (isd sd "Duplicate :line or :lines directive"))
         (verify-line-in-history val sd)
         (pushnew val *substitution-lines-referenced*)
         (setq lines (list val)))
        (:lines
         (when lines (isd sd "Duplicate :line or :lines directive"))
         (if (eq val :all) 
             (setq val (all-history-linenums))
           (progn
             (unless (listp val) (isd sd "Value for :lines must be a list"))
             (dolist (n val) (verify-line-in-history n sd))
             (dolist (n val) (pushnew n *substitution-lines-referenced*))))
         (setq lines val))
        (:position 
         (when how (isd sd "Duplicate substitution methods provided"))
         (unless (and (numberp val) (not (minusp val)))
           (isd sd "Invalid :position value"))
         (setq how :position)
         (setq how-arg val))
        (:value
         (when how (isd sd "Duplicate substitution methods provided"))
         (setq how :value)
         (setq how-arg val))
        (:keyval
         (when how (isd sd "Duplicate substitution methods provided"))
         (unless (keywordp val) (isd sd ":keyval must be followed by keyword"))
         (setq how :keyval)
         (setq how-arg val))
        (otherwise 
         (if (keywordp key) 
             (isd sd "Unknown keyword in substitution directive")
           (isd sd "Directive does not begin with a keyword"))))))
  (when (not how) (isd sd "No substitution method specified"))
  (when (not lines)
    (ecase how 
     (:keyval (setq lines (all-history-linenums)))
      (:value (setq lines (all-history-linenums)))
      (:position (isd sd "Must specify :line or :lines when using :position"))
      ))
  (list lines (list how how-arg))
  )
       

;;; Executed for side effect on *in-history-forms* contents.

(defun do-defprotocol-substitution (cs)
  (let* ((var (first cs))
        (line-numbers (second cs))
        (method (third cs))
        (how (first method))
        (how-arg (second method)))
    (dolist (line-number line-numbers)
      (let ((form (history-form-from-linenum line-number)))
        (set-history-form-at-linenum
         line-number
         (ecase how
           (:position 
            (unless (< how-arg (length form))
              (error "Position ~D does not exist in history form ~A.~%  ~A"
                     how-arg form
                     (format nil "(Attempting to substitute ~A into line ~D)"
                             var line-number)))
            (progn (setf (nth how-arg form) var) form))
           (:value 
            (let ((test (cond
                         ((symbolp how-arg) 'eq)
                         ((characterp how-arg) 'eql)
                         ((numberp how-arg) 'eql)
                         ((stringp how-arg) 'string=)
                         ((listp how-arg) 'equal)
                         (t 'equalp)
                         )))
              (subst var how-arg form :test test)))
           (:keyval (do-keyval-substitution var how-arg form))
           ))))))

(defun do-keyval-substitution (var keyword form)
  (cond
   ((not (consp form)) form)
   (t (cons 
       (do-keyval-substitution var keyword (car form))
       (do-keyval-subform-substitution var keyword (cdr form))))
   ))

(defun do-keyval-subform-substitution (var keyword form)
  (cond
   ((not (consp form)) form)
   ((eq keyword (first form))
    (if (cdr form)
        `(,(car form) 
          ,var 
          ,@(do-keyval-subform-substitution var keyword (cddr form)))
      (error "Attempted substitution of ~A for value of keyword ~S ~A"
             var keyword                 
             (format nil "in form ~A failed: no value to substitute ~A"
                     form "for after keyword itself!"))))
   (t
    (cons 
     (do-keyval-substitution var keyword (car form))
     (do-keyval-subform-substitution var keyword (cdr form))))
   ))

;;; Analysis code to find all history references.

;;; All the unique integer arguments to all the STEP and STEPS forms
;;; found within FORM.

(defun all-unique-steps-lines (form)
  (labels 
      ((do-it (form)
         (cond
          ((not (consp form)) nil)
          ((or (eq (first form) 'step) (eq (first form) 'steps))
           (mapcar #'(lambda (n) (verify-linenum n (first form))) (cdr form)))
          ((eq 'quote (first form)) nil)
          (t (mapcan #'do-it form))
          )))
    (sort (remove-duplicates (do-it form)) #'<)
    ))

;;; All the unique integer arguments to all HOP forms found within FORM.

(defun all-unique-history-line-references (form)
  (labels 
      ((do-it (form)
         (cond
          ((not (consp form)) nil)
          ((eq (first form) 'hop) 
           (unless (= 2 (length form))
             (error "Invalid HOP reference: ~A" form))
           (list (verify-linenum (second form) (first form))))
          ((eq 'quote (first form)) nil)
          (t (mapcan #'do-it form))
          )))
    (sort (remove-duplicates (do-it form)) #'<)
    ))

(defun all-hop-linenums-and-associated-history-forms (history-linenums)
  (mapcan
   #'(lambda (linenum)
       (let ((form (history-form-from-linenum linenum)))
         (mapcar 
          #'(lambda (hop-line) (list hop-line form))
          (all-unique-history-line-references form)
          )))
   history-linenums
   ))

(defun all-hop-linenums-and-associated-body-forms (body-forms)
  (mapcan
   #'(lambda (form)
       (mapcar
        #'(lambda (hop-line) (list hop-line form))
        (all-unique-history-line-references form)
        ))
   body-forms
   ))


;; Careful to not pick up '* reference in first (functional) position.

(defun all-unique-*-line-references (form current-linenum)
  (labels 
      ((do-it (form)
         (cond
          ((eq form '*) (list (- current-linenum 1)))
          ((eq form '**) (list (- current-linenum 2)))
          ((eq form '***) (list (- current-linenum 3)))
          ((not (consp form)) nil)
          ((eq 'quote (first form)) nil)
          ((not (consp (first form))) (mapcan #'do-it (cdr form)))
          (t (append (do-it (first form)) (mapcan #'do-it (cdr form))))
          )))
    (sort (remove-duplicates (do-it form)) #'<)
    ))
  
(defun all-*-linenums-and-associated-history-forms (history-linenums)
  (mapcan
   #'(lambda (linenum)
       (let ((form (history-form-from-linenum linenum)))
         (mapcar
          #'(lambda (*-line) (list *-line form))
          (all-unique-*-line-references form linenum)
          )))
   history-linenums
   ))


;;; Code generation.  These routines substitute symbols for
;;; HOP and '*' references.

(defun replace-history-references-with-step-variables (form linenum)
  (replace-*-references-with-step-variables
   (replace-hop-references-with-step-variables form)
   linenum
   ))

(defun replace-hop-references-with-step-variables (form)
  (cond
   ((not (consp form)) form)
   ((eq (first form) 'hop) (step-variable (second form)))
   ((eq 'quote (first form)) form)
   (t (mapcar #'replace-hop-references-with-step-variables form))
   ))

(defun replace-*-references-with-step-variables (form current-linenum)
  (flet ((maprepl (forms)
           (mapcar
            #'(lambda (f)
                (replace-*-references-with-step-variables f current-linenum))
            forms)))
    (cond
     ((eq form '*) (step-variable (- current-linenum 1)))
     ((eq form '**) (step-variable (- current-linenum 2)))  
     ((eq form '***) (step-variable (- current-linenum 3)))
     ((not (consp form)) form)
     ((eq 'quote (first form)) form)
     ;; Don't substitute for '* in functional position
     ((not (consp (first form))) (cons (first form) (maprepl (cdr form))))
     (t (maprepl form))
     )))

;;; Replaces STEP and STEPS forms with the (substituted in) 
;;; history line the form references

(defun replace-steps-forms (form)
  (cond
   ((not (consp form)) form)
   ((or (eq (first form) 'step) (eq (first form) 'steps))
    `(progn 
       ,@(mapcar 
          #'(lambda (n) 
              `(setq ,(step-variable n) ,(history-form-from-linenum n)))
          (cdr form))))
   (t (mapcar #'replace-steps-forms form))
   ))

#+test
(progn

(cl-user::deftest 
 protocol1
 (with-test-history 
  ((+ 4 5) (sqrt *) (oops) (+ (hop 2) (hop 1)) (print "Hello, world!"))
  (eval '(defprotocol protocol1 
             ((x :line 1 :value 4) (y (:line 1 :position 2)))
        (steps 5 1 2 4)))
  (eval '(progn (protocol1 :x 4 :y 5))))
 12.0
 :chapter :defprotocol
 :comparison #'=
 )

(cl-user::deftest 
 protocol2
 (with-test-history 
  ((+ 4 5) (+ 4 6) (+ (hop 2) (hop 1)) (- * 10))
  (eval '(defprotocol protocol2 
             ((x (:line 1 :value 4) (:line 2 :value 4))
              (z :line 4 :value 10))
        (steps 1 2 3 4)))
  (eval '(protocol2 :x 3 :z 5)))
 12
 :chapter :defprotocol 
 :comparison #'=
 )

(cl-user::deftest 
 protocol3
 (with-test-history 
  ((+ 4 5) (+ 4 6) (+ * (hop 1)) (- * 10))
  (eval '(defprotocol protocol3 
             ((x :line 1 :value 4)
              (y :lines (2 3) :value 4)
              (z :lines (4) :position 2))
           (steps 1 2) (step 3 4)))
  (eval '(protocol3 :x 3 :y 2 :z 12)))
 4
 :chapter :defprotocol
 :comparison #'=
 )

(cl-user::deftest 
 protocol4
 (with-test-history 
  ((+ 4 5) (+ 6 7) (+ * **) (+ (hop 3) ** ***) (sqrt (+ (* * 2) (hop 1))))
  (eval '(defprotocol protocol4
             ((x :line 1 :value 4)
              (y :lines (2 3) :value 6))
           (steps 1 2 3) (let ((fred 0)) (+ fred (steps 4 5)))))
  (eval '(protocol4 :x 3 :y 2)))
 (sqrt 76)
 :chapter :defprotocol
 :comparison #'=
 )

(cl-user::deftest 
 protocol5
 (with-test-history 
  ((+ 4 5) (+ 6 7) (+ * **) (+ (hop 3) ** ***) (sqrt (+ (* * 2) (hop 1))))
  (eval '(defprotocol protocol5
             ((x :line 1 :value 4)
              (y :lines (2 3) :value 6))
           (steps 1 2 3) 
           (format t "~&Step 1 result: ~A~%" (hop 1))
           (let ((fred 0)) (+ fred (steps 4 5)))))
  (eval '(protocol5 :x 3 :y 2)))
 (sqrt 76)
 :chapter :defprotocol
 :comparison #'=
 )

)
