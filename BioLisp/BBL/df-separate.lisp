;;;; -*- mode: Lisp; Syntax: Common-Lisp; Package: bbi; -*-

(in-package :bbi) 

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

(defstruct df-clause 
  name
  (aliases nil) 
  (special-parsing? nil)
  (special-combining? nil))

(defparameter *define-function-long-clauses*
  (list 
   (make-df-clause :name :required :aliases nil :special-parsing? nil)
   (make-df-clause :name :keyword  :aliases '(:keywords) :special-parsing? t)
   (make-df-clause :name :flag  :aliases '(:flags) :special-parsing? nil)
   (make-df-clause 
    :name :return  
    :aliases '(:returns) :special-parsing? t :special-combining? t)
   (make-df-clause 
    :name :type :aliases '(:types) :special-parsing? t)
   (make-df-clause :name :convert  :aliases '(:converts) :special-parsing? t)
   (make-df-clause 
    :name :mapcar  :aliases '(:map :mapover :map-over) :special-parsing? nil)
   (make-df-clause
    :name :mapcarnn  :aliases '(:mapnn :mapovernn :map-over-nn) 
    :special-parsing? nil)
   (make-df-clause 
    :name :maptree  :aliases '(:mapinto :map-into) :special-parsing? nil)
   (make-df-clause 
    :name :crossmap 
    :aliases '(:cross-map :cross-product :crossproduct) 
    :special-parsing? t)
   (make-df-clause :name :init  :aliases '(:initialize) :special-parsing? t)
   (make-df-clause 
    :name :public :aliases nil :special-parsing? t :special-combining? t)
   (make-df-clause :name :export-from  :aliases nil :special-parsing? nil)
   (make-df-clause
    :name :body :aliases nil :special-parsing? t :special-combining? t)
   (make-df-clause :name :summary :aliases nil :special-parsing? t)
   (make-df-clause :name :description :aliases '(:documentation) 
                   :special-parsing? t)
   (make-df-clause :name :see-also :aliases nil :special-parsing? nil)
   (make-df-clause :name :author :aliases '(:authors) :special-parsing? nil)
   (make-df-clause :name :example :aliases '(:examples) :special-parsing? t)
   ))


;;; Returns a list of clause specifiers.
;;; A clause specifier is a list whose first element is
;;; the keyword denoting the clause, whose second element
;;; is a list of subclauses for that clause, and whose third element
;;; is the original clause.    
;;; A subclause of a clause specifier is of a format particular 
;;; to the clause.

(defun separate-df-clauses (clauses)
  ;; recursion termination condition
  (when clauses
    (block exit
      (let* ((clause-keyword (first clauses))
             (*current-clause-keyword* clause-keyword))
        ;; all clauses begin with symbols
        (unless (symbolp clause-keyword)
          (restart-case 
              (df-parse-error 
               (one-string-nl
                "Unexpected form found: ~S."
                "Expected a clause designator such as REQUIRED, TYPE, etc."
                "(You may have misplaced parenthesis or quotation marks).")
               clause-keyword)
            (continue-df-parse 
             () 
             (return-from exit (separate-df-clauses (rest clauses))))
            ))
        (setq clause-keyword (keywordize clause-keyword))
        ;; find information, if any, about this particular clause
        ;; keyword (we do allow unknown clause keywords for now)
        (let ((clause-data 
               (find clause-keyword *define-function-long-clauses* 
                     :test
                     (lambda (keyword df-clause) 
                       (or (eq keyword (df-clause-name df-clause))
                           (member keyword (df-clause-aliases df-clause))))))
              (parse nil)
              (remaining-clauses (cdr clauses)))
          (restart-case
              (flet ((subsection (list tail)
                       (loop for remaining-list on list 
                             until (eq remaining-list tail)
                             collect (first remaining-list)
                             )))
                ;; given the clause type, parse out the entire clause,
                ;; then recurse to parse the remaining clauses
                (multiple-value-setq (parse remaining-clauses)
                    (funcall 
                     (cond 
                      ((null clause-data) 'separate-unknown-df-clause)
                      ((df-clause-special-parsing? clause-data)
                       'separate-special-df-clause)
                      (t 'separate-standard-df-clause))
                     (if (null clause-data) 
                         clause-keyword 
                       (df-clause-name clause-data))
                     (cdr clauses)))
                (cons (list
                       (if (null clause-data) 
                           clause-keyword
                         (df-clause-name clause-data))
                       parse
                       (subsection clauses remaining-clauses))
                      (separate-df-clauses remaining-clauses)
                      ))
            ;; If an error occurs trying to parse a clause, search
            ;; the form until we find a known clause keyword (or hit the end),
            ;; then continue the parse.
            (continue-df-parse 
             ()
             (progn 
               (pop remaining-clauses) 
               (loop until (null remaining-clauses) do 
                     (if (known-df-clause-keyword? (first remaining-clauses))
                         (return)
                       (pop remaining-clauses)))
               (separate-df-clauses remaining-clauses)
               ))))))))

(defun known-df-clause-keyword? (x) 
  (and (symbolp x) 
       (let ((y (keywordize x)))
         (loop for dflc in *define-function-long-clauses* 
               when (or (eq y (df-clause-name dflc))
                        (member y (df-clause-aliases dflc)))
               do
               (return t)))))
         

(defun oops-premature? (clause-keyword form)
  (when (null form)
    (error "(~A ~S ...) terminated prematurely after ~A ..." 
           *df-type* *df-name* clause-keyword)))

(defun separate-standard-df-clause (clause-keyword rest-of-form)
  (oops-premature? clause-keyword rest-of-form)
  (values (ensure-list (first rest-of-form)) (cdr rest-of-form))
  )

(defun separate-unknown-df-clause (clause-keyword rest-of-form)
  (warn "Unknown define-function clause keyword: ~S" clause-keyword)
  (separate-standard-df-clause clause-keyword rest-of-form))

(defgeneric separate-special-df-clause (clause-keyword rest-of-form) 
  (:documentation
   #.(one-string-nl
      "Grabs forms after CLAUSE-KEYWORD that are associated with"
      "that keyword.  Returns a list of these forms.  If a single"
      "form is associated with the clause keyword, the list of that"
      "form is returned.  The second value returned are the remaining"
      "clauses to be parsed.")))
   

(defmethod separate-special-df-clause
           ((clause-keyword (eql :return)) rest-of-form)
  (oops-premature? clause-keyword rest-of-form)
  (values (list (first rest-of-form)) (cdr rest-of-form)))

;; xxxxxxxxxxxxxx

(defmethod separate-special-df-clause
           ((clause-keyword (eql :type)) rest-of-form)
  (separate-special-df-equals-clause+ clause-keyword rest-of-form))

(defmethod separate-special-df-clause
           ((clause-keyword (eql :init)) rest-of-form)
  (separate-special-df-equals-clause+ clause-keyword rest-of-form))

(defmethod separate-special-df-clause
           ((clause-keyword (eql :convert)) rest-of-form)
  (separate-df-convert-clause+ clause-keyword rest-of-form))

(defmethod separate-special-df-clause
           ((clause-keyword (eql :public)) rest-of-form)
  (values nil rest-of-form))

(defmethod separate-special-df-clause
           ((clause-keyword (eql :body)) rest-of-form)
  (oops-premature? clause-keyword rest-of-form)
  (values rest-of-form nil))

(defmethod separate-special-df-clause
           ((clause-keyword (eql :keyword)) rest-of-form)
  (oops-premature? clause-keyword rest-of-form)
  (let ((next (first rest-of-form)))
    (if (listp next)
        (if (multiple-keyword-specs? rest-of-form)
            (let ((*multiple-keyword-specs* next))
              (values 
               (separate-multiple-keyword-specs next)
               (cdr rest-of-form)
               ))
          (separate-single-keyword-spec rest-of-form)
          )
      (separate-non-aliased-single-keyword-spec rest-of-form)
      )))

(defmethod separate-special-df-clause
           ((clause-keyword (eql :description)) rest-of-form)
  (oops-premature? clause-keyword rest-of-form)
  (separate-special-df-strings-clause clause-keyword rest-of-form))

(defmethod separate-special-df-clause
           ((clause-keyword (eql :summary)) rest-of-form)
  (oops-premature? clause-keyword rest-of-form)
  (separate-special-df-strings-clause clause-keyword rest-of-form))

(defmethod separate-special-df-clause
           ((clause-keyword (eql :example)) rest-of-form)
  (oops-premature? clause-keyword rest-of-form)
  (separate-special-df-strings-clause clause-keyword rest-of-form))

(defmethod separate-special-df-clause
           ((clause-keyword (eql :crossmap)) rest-of-form)
  (oops-premature? clause-keyword rest-of-form)
  (let ((next (first rest-of-form)))
    (unless (listp next) 
      (df-parse-error
       (one-string-nl
        "In a ~S clause, "
        "the form immediately after ~S, ~S, must be a list of two "
        "required arguments, but it is not a list at all, it is"
        "of type ~S.")
       *current-clause-keyword* *current-clause-keyword*
       next (printed-type-of next)
       ))
    (unless (= 2 (length next))
      (df-parse-error
       (one-string-nl
        "In a ~S clause, "
        "the form immediately after ~S, ~S, must be a list of two "
        "required arguments, but the list contains ~D elements.")
       *current-clause-keyword* *current-clause-keyword* next (length next)
       ))
    (values next (cdr rest-of-form))
    ))


(defun separate-special-df-strings-clause (clause-keyword rest-of-form)
  (declare (ignore clause-keyword))
  (let ((next (first rest-of-form)))
    (if (listp next) 
        (values next (cdr rest-of-form))
      (progn
        (unless (stringp next)
          (df-parse-error 
           (one-string-nl
            "In the ~S clause beginning with '~A',"
            "the form, ~S, immediately after ~S is neither a list nor"
            "a string.  ~S expects either one or more strings or"
            "a list of strings, but the next form is of type ~S.")
           *current-clause-keyword* 
           (limited-form-string (cons *current-clause-keyword* rest-of-form) 30)
           next *current-clause-keyword* 
           *current-clause-keyword* (printed-type-of next)
           ))
        (let ((strings (list (pop rest-of-form))))
          (loop until (or (null rest-of-form) 
                          (not (stringp (first rest-of-form)))) do
                (push (pop rest-of-form) strings))
          (values (reverse strings) rest-of-form)
          )))))
            
            

(defun multiple-keyword-specs? (form)
  (let ((next (second form)))
    (not (or (symbol= := next) (and (symbolp next) (=symbol? next))))))
  
(defun separate-single-keyword-spec (form)
  (let ((next (first form)))
    (if (symbolp next)
        (separate-non-aliased-single-keyword-spec form)
      (separate-aliased-single-keyword-spec form)
      )))


(defun separate-non-aliased-single-keyword-spec (form)
  (let ((var (first form)))
    (unless (symbolp var)
      (df-parse-error 
       (one-string-nl
        "In a KEYWORD clause, the next token, ~S,"
        "is a ~S but it must be a symbol.")
       var (printed-type-of var)))
    (if (let* ((name (string var))
               (pos (position #\= name)))
          (cond
           ((null pos) 
            (or (symbol= := (second form))
                (and (symbolp (second form)) (=symbol? (second form)))))
           ((zerop pos) 
            (df-parse-error 
             (one-string-nl
              "In the KEYWORD clause beginning '~S ~S ...'"
              "the keyword variable contains an '=' at the beginning."
              "In Biobike no keyword variables are allowed to contain '='"
              "characters in their names.")
             *current-clause-keyword* var))
           (t t)
           ))
        (multiple-value-bind (equals-parse rest-of-form)
            (separate-one-df-keyword-equals-clause :keyword form)
          (values 
           (list (list (first equals-parse) nil (second equals-parse)))
           rest-of-form))
      (values (list (list var nil nil)) (cdr form))
      )))


          
(defun separate-aliased-single-keyword-spec (form &aux initial-value)
  (let ((alias-list (first form)))
    (pop form)
    (if (symbol= := (first form)) 
        (progn
          (pop form)
          (setq initial-value (first form))
          (pop form))
      (let* ((symbol (first form))
             (name (string symbol)))
        (unless (and (symbolp symbol) (=symbol? symbol))
          (error "Internal error.  Should never get here."))
        (setq initial-value (read-from-string (subseq name 1)))
        (pop form)
        ))
    (loop for alias in alias-list do 
          (restart-case 
              (progn 
                (unless (symbolp alias) 
                  (df-parse-error 
                   (one-string-nl
                    "In the KEYWORD clause beginning '~S ~S ...'"
                    "One of the aliases, ~S, is not a symbol!")
                   *current-clause-keyword* alias-list alias))
                (unless (null (position #\= (string alias)))
                  (df-parse-error
                   (one-string-nl
                    "In the KEYWORD clause beginning '~S ~S ...'"
                    "One of the aliases, ~S, contains an '='."
                    "In Biobike neither keywords nor their aliases"
                    "may contain '='.")
                   *current-clause-keyword* alias-list alias)))
            (continue-df-parse () nil)
            ))
    (values 
     (list (list (first alias-list) (rest alias-list) initial-value))
     form)))
            
    
(defun separate-multiple-keyword-specs (keyword-specs)
  (let ((next (first keyword-specs))
        (keyword-clause 
         (list *current-clause-keyword* *multiple-keyword-specs*)))
    (cond 
     ((listp next)
      (when (multiple-keyword-specs? keyword-specs)
        (df-parse-error 
         (one-string-nl
          "In the KEYWORD clause ~S,"
          "in the subclause ~S naming a keyword and its aliases,"
          "there is no initial value specification immediately"
          "after this name and alias list, but there must be"
          "because you cannot define a keyword with aliases"
          "without providing an initial value."
          "(e,g, use '~S = nil' instead.)")
         keyword-clause next next))
      (multiple-value-bind (parsed-keyspec more-keyspecs) 
          (separate-aliased-single-keyword-spec keyword-specs)
        (if (null more-keyspecs) 
            parsed-keyspec
          (cons (first parsed-keyspec) 
                (separate-multiple-keyword-specs more-keyspecs)))))
     ((symbolp next)      
      (if (let* ((name (string next))
                 (pos (position #\= name))
                 (form keyword-specs))
            (cond
             ((null pos) 
              (or (symbol= := (second form))
                  (and (symbolp (second form)) (=symbol? (second form)))))
             ((zerop pos) 
              (df-parse-error 
               (one-string-nl
                "In the KEYWORD clause ~S,"
                "the keyword variable ~S contains an '=' at the beginning."
                "In Biobike no keyword variables are allowed to contain '='"
                "characters in their names.")
               keyword-clause next))
             (t t)
             ))
          (multiple-value-bind (parsed-keyspec more-keyspecs) 
              (separate-one-df-keyword-equals-clause :keyword keyword-specs)
            (setq parsed-keyspec 
                  (list (first parsed-keyspec) nil (second parsed-keyspec)))
            (if (null more-keyspecs) 
                (list parsed-keyspec)
              (cons parsed-keyspec 
                    (separate-multiple-keyword-specs more-keyspecs))))
        (cons
         (list next nil nil)
         (and (rest keyword-specs) 
              (separate-multiple-keyword-specs (rest keyword-specs))))
        ))
     (t 
      (df-parse-error 
       (one-string-nl
        "In the KEYWORD clause ~S,"
        "one of the keyword specifiers begins with ~S,"
        "but a keyword specifier must begin with a symbol,"
        "not a ~S")
       keyword-clause next (printed-type-of next)))
     )))
        

(defun =symbol? (symbol)
  "Is a symbol's first character an equal sign?"
  (char= #\= (char (string symbol) 0)))

(defun symbol=? (symbol)
  "Is a symbol's last character an equal sign?"
  (let ((name (string symbol)))
    (char= #\= (char name (1- (length name))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Code to combine equivalent clauses 

(defun add-clause-form-to-each-datum (data-and-clause-form)
  (let ((data (first data-and-clause-form))
        (clause-form (second data-and-clause-form)))
    (mapcar (lambda (datum) (list datum clause-form)) data)
    ))

(defun combine-df-clauses (cclauses)
  (let* ((mapcars? (find :mapcar cclauses :key 'first))
         (mapcarnns? (find :mapcarnn cclauses :key 'first))
         (maptrees? (find :maptree cclauses :key 'first))
         (crossmaps? (find :crossmap cclauses :key 'first))
         (maps (list mapcars? mapcarnns? maptrees? crossmaps?))
         (mapcount (count-if 'identity maps)))
    (unless (or (zerop mapcount) (= mapcount 1))
      (df-parse-error
       (one-string-nl
        "At most one argument mapping clause is allowed!"
        "You provided ~D different mapping clauses: ~S.")
       (count-if 'identity maps)
       (loop for present? in maps 
             for clause-keyword in '(:mapcar :mapcarnn :maptree :crossmap)
             when present? collect clause-keyword))))
  (let ((chash (make-hash-table)))
    (loop for cclause in cclauses 
          as clause-name = (separated-clause-keyword cclause)
          do
          (restart-case
              (case clause-name
                (:return 
                 (when (gethash clause-name chash)
                   (df-parse-error 
                    (one-string-nl
                     "There is more than one RETURN (or RETURNS) clause!"
                     "Define-function insists that at most one"
                     "of these clauses be present.")
                    )))
                (:public 
                 (when (gethash clause-name chash)
                   (df-parse-error 
                    (one-string-nl
                     "There is more than one PUBLIC clause!"
                     "Define-function insists that at most one"
                     "of these clauses be present.")
                    )))
                (:crossmap
                 (when (gethash clause-name chash)
                   (df-parse-error 
                    (one-string-nl
                     "There is more than one CROSSMAP (or CROSSPRODUCT)"
                     "clause! Define-function insists that at most one"
                     "of these clauses be present.")
                    )))
                (otherwise nil))
            (continue-df-parse () nil))
          (if (gethash clause-name chash) 
              (setf (gethash clause-name chash)
                    (combine-two-equivalent-df-clauses 
                     clause-name
                     (gethash clause-name chash) 
                     (cdr cclause)))
            (setf (gethash clause-name chash)
                  (add-clause-form-to-each-datum (cdr cclause)))
            ))
    (hash-table-contents chash)
    ))

(defun combine-two-equivalent-df-clauses 
       (clause-name existing-hash-info new-hash-info)
  (let ((clause-info (find clause-name *define-function-long-clauses* 
                           :key 'df-clause-name)))
    (flet ((default-combining () 
             (append 
              existing-hash-info 
              (add-clause-form-to-each-datum new-hash-info))))
      (cond
       ((null clause-info) (default-combining))
       ((null (df-clause-special-combining? clause-info)) (default-combining))
       (t 
        (ecase clause-name
          (:return nil)
          (:public nil)
          (:body (error "Internal error!  This should be impossible."))
          ))))))
        
        
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  



(defun separate-special-df-equals-clause+ (clause-keyword rest-of-form)
  (oops-premature? clause-keyword rest-of-form)
  (if (not (another-equals-clause?+ rest-of-form))
      (let ((next (first rest-of-form)))
        (if (and next (listp next))
            (multiple-value-bind (equals-items rest)
                (separate-special-df-equals-clause+ clause-keyword next)
              (when rest 
                (df-parse-error 
                 (one-string-nl
                  "Illegal ~S specifier ~S."
                  "The last part of the specifier, ~S,"
                  "is not a legal subclause for a '~S' clause.")
                 *current-clause-keyword* next rest *current-clause-keyword* 
                 ))
              (values equals-items (rest rest-of-form))
              )
          (df-parse-error 
           (one-string-nl
            "Unrecognized syntax after ~S clause-specifier."
            "~S is not a valid subclause for a '~S' clause.")
           *current-clause-keyword* next *current-clause-keyword*
           )))
    (multiple-value-bind (equals-items rest-of-form)
        (separate-one-df-equals-clause+ clause-keyword rest-of-form)
      (if (another-equals-clause?+ rest-of-form)
          (multiple-value-bind (more-equals-items rest-of-form)
              (separate-special-df-equals-clause+ 
               clause-keyword rest-of-form) 
            (values 
             (append equals-items more-equals-items)
             rest-of-form
             ))
        (values equals-items rest-of-form)
        ))))

(defun check-for-symbol-or-list-of-symbols (symbol-or-list)
  (unless (or (listp symbol-or-list) (symbolp symbol-or-list))
    (df-parse-error 
     (one-string-nl
      "The next token after the clause designator ~S"
      "must be a symbol or a list of symbols,"
      "but instead you provided ~S,"
      "an object of type ~S.")
     *current-clause-keyword* symbol-or-list (printed-type-of symbol-or-list)))
  (when (listp symbol-or-list) 
    (when (null symbol-or-list) 
      (df-parse-error 
       (one-string-nl
        "The next token after the clause designator ~S"
        "must be a non-NIL symbol or a list of symbols."
        "In fact it is NIL, which is not legal here.")
       *current-clause-keyword*))
    (loop for s in symbol-or-list do 
          (unless (and s (symbolp s))
            (df-parse-error 
             (one-string-nl
              "The next token after the clause designator ~S"
              "must be a non-NIL symbol or a list of non-NIL symbols."
              "In fact, one of the elements of the list, ~S,"
              "is not a non-NIL symbol.")
             *current-clause-keyword* s
             )))))

(defun separate-one-df-equals-clause+
       (clause-keyword rest-of-form 
                       &aux symbol symbol-or-list equal-sign? equal-value)
  (declare (ignore clause-keyword))
  
  (setq symbol-or-list (pop rest-of-form))
  
  (check-for-symbol-or-list-of-symbols symbol-or-list)
  
  ;; check for foo= bar, or foo=bar 

  (setq symbol symbol-or-list)
  (setq equal-sign? nil)
  
  (when (symbolp symbol) 
    (let ((name (symbol-name symbol))
          (package (symbol-package symbol)))
      (cond 
       ((symbol=? symbol)
        (setq symbol-or-list
              (intern (subseq name 0 (1- (length name))) package))
        (push := rest-of-form) 
        (setq equal-sign? t))
       ((position #\= name) 
        (let ((pos (position #\= name)))
          (when (zerop pos) 
            (df-parse-error 
             (one-string-nl 
              "In a clause beginning with ~S,"
              "a symbol begins with an '=' (~S).  In Biobike, symbols"
              "denoting DEFINE-FUNCTION arguments are not allowed"
              "to contain '='.")
             *current-clause-keyword* symbol))
          (setq symbol-or-list (intern (subseq name 0 pos) package))
          (push (read-from-string (subseq name (1+ pos))) rest-of-form)
          (push := rest-of-form)
          (setq equal-sign? t)
          ))
       (t (setq equal-sign? nil)))))

  ;; if the next form is not an equal sign, then if it is a symbol
  ;; check if it's of the form =foo 

  (if equal-sign? 
      (pop rest-of-form)
    (let ((next (pop rest-of-form)))
      (cond 
       ((symbol= := next) nil)
       ((symbolp next) 
        (let ((name (string next)))
          (if (=symbol? next)
              (progn 
                (setq equal-sign? t)
                (push (read-from-string (subseq name 1)) rest-of-form))
            (df-parse-error 
             (one-string-nl
              "In a clause beginning with ~S,"
              "~S expects subclauses of the form symbol = thing"
              "but there is no '=' after the symbol or list of symbols"
              "~S.")
             *current-clause-keyword* *current-clause-keyword* symbol
             ))))
       (t 
        (df-parse-error
         (one-string-nl
          "In a clause beginning with ~S,"
          "~S expects subclauses of the form symbol-or-list = thing"
          "but the form after ~S is ~S.")
         *current-clause-keyword* *current-clause-keyword* symbol next))
       )))
  
  ;; get the next value which must be the thing to the right
  ;; of the equal sign

  (let ((next (pop rest-of-form)))
    (cond
     ((symbolp next) 
      (let ((name (string next)))
        (when (position #\= name) 
          (df-parse-error 
           (one-string-nl 
            "In a clause beginning with ~S,"
            "a symbol contains an '=' (~S).  In Biobike, symbols"
            "denoting DEFINE-FUNCTION arguments are not allowed"
            "to contain '='.")
           *current-clause-keyword* next))
        (setq equal-value next)
        ))
     (t (setq equal-value next))
     ))

  (if (symbolp symbol-or-list) 
      (values (list (list symbol-or-list equal-value)) rest-of-form)
    (values 
     (mapcar (lambda (s) (list s equal-value)) symbol-or-list)
     rest-of-form
     ))
  
  )

(defun another-equals-clause?+ (form)
  (let ((next (first form))
        (second (second form)))
    (cond
     ((null form) nil)
     ;; look for (a b) = integer, (a b c) =integer 
     ((and next (listp next))
      (or (symbol= := second)
          (and (symbolp second) (=symbol? second))))
     (t 
      ;; look for things of the form foo = bar, foo= bar, foo=bar, foo =bar
      (let* ((symbol next)
             (name (string symbol))
             (pos (position #\= name)))
        (or (and (null pos) (symbol= := second))
            (and pos (plusp pos))
            (and (null pos) (symbolp second) (=symbol? second))
            ))))))


(defun separate-df-convert-clause+ (clause-keyword rest-of-form)
  (oops-premature? clause-keyword rest-of-form)
  (if (not (another-convert-clause?+ rest-of-form))
      (let ((next (first rest-of-form)))
        (if (and next (listp next))
            (multiple-value-bind (convert-items rest)
                (separate-df-convert-clause+ clause-keyword next)
              (when rest 
                (df-parse-error 
                 (one-string-nl
                  "Illegal ~S specifier ~S."
                  "The last part of the specifier, ~S,"
                  "is not a legal subclause for a '~S' clause.")
                 *current-clause-keyword* next rest *current-clause-keyword* 
                 ))
              (values convert-items (rest rest-of-form)))
          (df-parse-error 
           (one-string-nl
            "Unrecognized syntax after ~S clause-specifier."
            "~S is not a valid subclause for a '~S' clause.")
           *current-clause-keyword* next *current-clause-keyword*
           )))
    (multiple-value-bind (convert-items rest-of-form)
        (separate-one-convert-clause+ clause-keyword rest-of-form)
      (if (another-convert-clause?+ rest-of-form)
          (multiple-value-bind (more-convert-items rest-of-form)
              (separate-df-convert-clause+ clause-keyword rest-of-form)
            (values 
             (append convert-items more-convert-items)
             rest-of-form
             ))
        (values convert-items rest-of-form)
        ))))

(defun separate-one-convert-clause+
       (clause-keyword 
        rest-of-form
        &aux 
        symbol-or-list from-word from-value to-value to-word
        using-symbol using-word)
  (declare (ignore clause-keyword))
  
  ;; grab the first token and make sure it's a symbol

  (setq symbol-or-list (pop rest-of-form))
  
  (check-for-symbol-or-list-of-symbols symbol-or-list)

  ;; the next symbol must be 'from'

  (setq from-word (pop rest-of-form))
  (unless (symbol= :from from-word)
    (df-parse-error
     (one-string-nl
      "In a ~A clause beginning with ~S,"
      "after the symbol or list of symbols being converted,"
      "~S, the word 'from' must appear but it does not."
      "Instead '~S' is the next form.")
     *df-type* *current-clause-keyword* symbol-or-list from-word))

  ;; the next object must be a valid type descriptor, 
  ;; which, for our purposes here, simply means that it must
  ;; be a symbol or a list.

  (setq from-value (pop rest-of-form))
  (unless (or (symbolp from-value) (listp from-value))
    (df-parse-error
     (one-string-nl
      "In a ~A clause beginning with ~S,"
      "the type object that ~S is being converted from,"
      "~S,"
      "is not a valid type specifier (it is neither a symbol nor"
      "a list, it is a ~S.)")
     *df-type* *current-clause-keyword* symbol-or-list from-value
     (printed-type-of from-value)
     ))

  ;; The next symbol must be 'to' 

  (setq to-word (pop rest-of-form))
  (unless (symbol= :to to-word)
    (df-parse-error
     (one-string-nl
      "In a ~A clause beginning with ~S,"
      "after the symbol or list of symbols being converted,"
      "~S, the word 'to' must appear but it does not."
      "Instead '~S' is the next form.")
     *df-type* *current-clause-keyword* symbol-or-list to-word))

  ;; the next object must be a valid type descriptor, 
  ;; which, for our purposes here, simply means that it must
  ;; be a symbol or a list.

  (setq to-value (pop rest-of-form))
  (unless (or (symbolp to-value) (listp to-value))
    (df-parse-error
     (one-string-nl
      "In a ~A clause beginning with ~S,"
      "the type object that ~S is being converted to,"
      "~S,"
      "is not a valid type specifier (it is neither a symbol nor"
      "a list, it is a ~S.)")
     *df-type* *current-clause-keyword* 
     symbol-or-list to-value (printed-type-of to-value)
     ))

  ;; Optionally there can be a 'using <defconverion-name>' subclause here

  (flet ((return-values (using)
           (values 
            (if (symbolp symbol-or-list) 
                (list (list symbol-or-list to-value from-value using))
              (mapcar 
               (lambda (s) (list s to-value from-value using))
               symbol-or-list))
            rest-of-form
            )))
    (setq using-word (first rest-of-form))
    (if (not (symbol= using-word :using))
        (return-values nil)
      (progn
        (pop rest-of-form)
        (when (null rest-of-form) 
          (df-parse-error 
           (one-string-nl
            "In a ~A clause beginning with ~S,"
            "you specify the word 'using' after the conversion type ~S,"
            "but the DEFINE-FUNCTION form terminates immediately after"
            "the word 'using'.  There must be a conversion method named"
            "after 'using'.")
           *df-type* *current-clause-keyword* to-value))
        (setq using-symbol (pop rest-of-form))
        (unless (symbolp using-symbol) 
          (df-parse-error
           (one-string-nl
            "In a ~A clause beginning with ~S,"
            "you specify the word 'using' after the conversion type ~S,"
            "but there is no symbol indicating what conversion method"
            "to use after the word 'using'.  The next form is in fact"
            "~S, a ~S, which is not legal syntax.")
           *df-type* *current-clause-keyword* to-value
           using-symbol (printed-type-of using-symbol)
           ))
        (return-values using-symbol)
        )))
  )

(defun another-convert-clause?+ (form)
  (let ((next (first form))
        (second (second form)))
    (cond
     ((null form) nil)
     ((and next (or (symbolp next) (listp next))) (symbol= :from second))
     (t nil))))


(defun separate-one-df-keyword-equals-clause
       (clause-keyword rest-of-form &aux symbol equal-sign? equal-value)
  (declare (ignore clause-keyword))
  
  ;; grab the first token and make sure it's a symbol

  (setq symbol (pop rest-of-form))
  (unless (symbolp symbol)
    (df-parse-error 
     (one-string-nl
      "The next token after the clause designator ~S"
      "must be a symbol, but instead you provided ~S,"
      "an object of type ~S.")
     *current-clause-keyword* symbol (printed-type-of symbol)))
  
  ;; check for foo= bar, or foo=bar 

  (let ((name (symbol-name symbol))
        (package (symbol-package symbol)))
    (cond 
     ((symbol=? symbol)
      (setq symbol (intern (subseq name 0 (1- (length name))) package))
      (push := rest-of-form) 
      (setq equal-sign? t))
     ((position #\= name) 
      (let ((pos (position #\= name)))
      (when (zerop pos) 
        (df-parse-error 
         (one-string-nl 
          "In a clause beginning with ~S,"
          "a symbol begins with an '=' (~S).  In Biobike, symbols"
          "denoting DEFINE-FUNCTION arguments are not allowed"
          "to contain '='.")
         *current-clause-keyword* symbol))
      (setq symbol (intern (subseq name 0 pos) package))
      (push (read-from-string (subseq name (1+ pos))) rest-of-form)
      (push := rest-of-form)
      (setq equal-sign? t)
      ))
     (t (setq equal-sign? nil))))

  ;; if the next form is not an equal sign, then if it is a symbol
  ;; check if it's of the form =foo 

  (if equal-sign? 
      (pop rest-of-form)
    (let ((next (pop rest-of-form)))
      (cond 
       ((symbol= := next) nil)
       ((symbolp next) 
        (let ((name (string next)))
          (if (=symbol? next)
              (progn 
                (setq equal-sign? t)
                (push (read-from-string (subseq name 1)) rest-of-form))
            (df-parse-error 
             (one-string-nl
              "In a clause beginning with ~S,"
              "~S expects subclauses of the form symbol = thing"
              "but there is no '=' after the symbol ~S.")
             *current-clause-keyword* *current-clause-keyword* symbol
             ))))
       (t 
        (df-parse-error
         (one-string-nl
          "In a clause beginning with ~S,"
          "~S expects subclauses of the form symbol = thing"
          "but the form after ~S is ~S.")
         *current-clause-keyword* *current-clause-keyword* symbol next))
       )))
  
  ;; get the next value which must be the thing to the right
  ;; of the equal sign

  (let ((next (pop rest-of-form)))
    (cond
     ((symbolp next) 
      (let ((name (string next)))
        (when (position #\= name) 
          (df-parse-error 
           (one-string-nl 
            "In a clause beginning with ~S,"
            "a symbol contains an '=' (~S).  In Biobike, symbols"
            "denoting DEFINE-FUNCTION arguments are not allowed"
            "to contain '='.")
           *current-clause-keyword* next))
        (setq equal-value next)
        ))
     (t (setq equal-value next))
     ))
  
  (values (list symbol equal-value) rest-of-form)
  
  )