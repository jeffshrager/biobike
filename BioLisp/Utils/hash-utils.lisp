;;; -*- Package: utils; mode: lisp; base: 10; Syntax: Common-Lisp; -*-

(in-package :utils)

;;; +=========================================================================+
;;; | Copyright (c) 2002-2005 JP Massar, Jeff Shrager, Mike Travers           |
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

;;; Author:  JP Massar.

(eval-when (:compile-toplevel :load-toplevel :execute)

  (defparameter *utility-hash-user-symbols*
    '(
      hash-table-contents
      hash-table-keys
      hash-table-values
      create-hash-table
      lmaphash
      lmaphashnn
      hash-tables-have-identical-keys?
      showhash
      check-for-duplicates
      purge-duplicates
      find-duplicates
      set-equal
      binsearch
      group-by-type
      freqsort
      set-difference-hash
      ))

  (defparameter *utility-hash-api-symbols*
    (append *utility-hash-user-symbols*
            '(
              simple-hash-table
              make-string-equal-hash-table
              ht-contents
              def-hash-table
              create-hash-table
              a-hash-element
              mapfunc-over-hashtable
              dump-hash-to-file
              parse-doc-decls-body
              def-memoized-function
              defmemoize
              canonicalize-hash-test
              intersection-size
              union-size
              check-if-any-a-is-not-in-b
              )))

  (export *utility-hash-api-symbols* (find-package :utils)))

(defun simple-hash-table ()
  #.(one-string-nl
     "This is a simpler form of the most common version of make-hash-table,"
     "with :test #'equal")
  (make-hash-table :test #'equal))

(defun make-string-equal-hash-table (&rest keys)
  #.(one-string-nl
     "Returns a hash table capable of keying on case-sensitive strings")
  #+:allegro
  (apply 'make-hash-table :test 'string-equal keys)
  #+:lispworks 
  (apply 'make-hash-table :test 'equalp keys)
  #+:SBCL
  (apply 'make-hash-table :test 'string-equal keys)
  #-(or :allegro :lispworks :SBCL)
  (error "Fix me!")
  )


(defun ht-contents (ht)
  "Returns as a list of (KEY VALUE) pairs all the entries in the hash table"
  (let ((result nil))
    (maphash (lambda (key value) (push (list key value) result)) ht)
    result))

(defun hash-table-contents (ht)
  "Returns as a list of (KEY VALUE) pairs all the entries in the hash table"  
  (ht-contents ht))

(defun hash-table-keys (ht)
  "Returns as a list all the keys in the hash table"
  (let ((result nil))
    (maphash (lambda (key value) (declare (ignore value)) (push key result)) ht)
    result
    ))

(defun hash-table-values (ht)
  "Returns as a list the values of all the keys in the hash table"
  (let ((result nil))
    (maphash (lambda (key value) (declare (ignore key)) (push value result)) ht)
    result
    ))

;;;;;;;;;;;;;;;;


;;; Auxiliary functions for DEF-HASH-TABLE and CREATE-HASH-TABLE.


(defun init-hash-table-key (hash-table key default-value mode)
  (flet ((key-and-value (mode)
           (ecase mode
             (:dotted-pair (values (car key) (cdr key)))
             ((:list :two-element-list) (values (car key) (cadr key)))
             (:singleton 
              (values 
               key
               (if (functionp default-value) 
                   (funcall default-value key) 
                 default-value
                 ))))))
    (multiple-value-bind (real-key real-value)
        (case mode
          (:default
           (key-and-value
            (cond
             ((atom key) :singleton)
             ((and (consp key) (not (consp (cdr key)))) :dotted-pair)
             (t :two-element-list)
             )))
          (otherwise (key-and-value mode)))
      (setf (gethash real-key hash-table) real-value)
      )))

(defun remove-keyargs-key-and-value (key key-args)
  (let ((len (length key-args)))
    (vif (pos (position key key-args))
         (append 
          (subseq key-args 0 pos)
          (unless (= len (+ pos 2)) (subseq key-args (+ pos 2))))
         key-args
         )))


(defmacro def-hash-table 
          (var 
           initial-contents
           &rest keyargs
           &key 
           (defvar? nil) 
           (default-value #'identity)
           (mode :default)
           (documentation nil)
           &allow-other-keys
           )
  #.(one-string-nl
     "Defines a global variable VAR which is initialized to be a hash table."
     "This hash table is initialized to have keys and values as specified by"
     "INITIAL-CONTENTS, DEFAULT-VALUE, and MODE."
     "The hash table is created using MAKE-HASH-TABLE and any other keywords"
     "provided by the caller other than those specifically in the argument"
     "list are passed through to MAKE-HASH-TABLE (e.g. the hash table TEST"
     "is specified is the usual way, using, say, :test 'equal)."
     "If DEFVAR? is non-nil, then VAR is defined as a DEFVAR, otherwise as"
     "a DEFPARAMETER."
     "MODE defines how INITIAL-CONTENTS is interpreted."
     "  If MODE is :DOTTED-PAIR then the key of each hash table entry is the"
     "CAR of each element, and the value is the CDR."
     "  If MODE is :LIST or :TWO-ELEMENT-LIST then the key is the CAR, and the"
     "value is the CADR."
     "  If MODE is :SINGLETON, then the key is always the element itself."
     "If DEFAULT-VALUE is #'IDENTITY (the default) the value is also the"
     "element itself.  Otherwise, if DEFAULT-VALUE is a function, it is"
     "applied to the element to compute the value, otherwise DEFAULT-VALUE"
     "itself is taken to be the value (so by specifying :default-value t)"
     "all the entries in the hash table will have a value of T)."
     "  If MODE is DEFAULT, then each ELEMENT is examined individually to"
     "determine what mode to use -- if ELEMENT is a dotted pair, :DOTTED-PAIR"
     "is used for that element, otherwise :SINGLETON or :LIST is used as"
     "appropriate.")
  (declare (ignore mode default-value))
  (setq keyargs (remove-keyargs-key-and-value :defvar? keyargs))
  (setq keyargs (remove-keyargs-key-and-value :documentation keyargs))
  `(,(if defvar? 'defvar 'defparameter)
    ,var (funcall 'create-hash-table ,initial-contents ,@keyargs)
    ,@(when documentation (list documentation))
    ))

(defun create-hash-table 
       (initial-contents 
        &rest keyargs 
        &key
        (default-value #'identity)
        (mode :default)
        &allow-other-keys
        )
  #.(one-string-nl
     "Creates a hash table and initializes it using INITIAL-CONTENTS."
     "See (DEF-HASH-TABLE ...) for specifics on how INITIAL-CONTENTS is"
     "interpreted, how DEFAULT-VALUE and MODE affect this interpetation,"
     "and how other keywords are passed through to LISP:MAKE-HASH-TABLE.")
  (setq keyargs (remove-keyargs-key-and-value :default-value keyargs))
  (setq keyargs (remove-keyargs-key-and-value :mode keyargs))
  (let ((ht (apply 'make-hash-table keyargs)))
    (loop for element in initial-contents do
          (init-hash-table-key ht element default-value mode))
    ht
    ))
  
(defun a-hash-element (hash-table &optional (maphash-element 1))
  #.(one-string-nl
     "Returns an element of HASH-TABLE in the form (KEY VALUE)."
     "The element returned is the nth element of the hash table iterated over"
     "by MAPHASH (by default, the first element), where N = MAPHASH-ELEMENT."
     "If the hash table is empty or contains less than MAPHASH-ELEMENT items"
     "then NIL is returned.")
  (let ((count 1))
    (block exit
      (maphash
       (lambda (key value) 
         (when (= count maphash-element) 
           (return-from exit (list key value))
           ))
       hash-table
       ))))
       

(defun lmaphash (f hash-table) 
  #.(one-string-nl
     "Map F, a function of two arguments (key and value) over the entries"
     "of HASH-TABLE, returning a list of the results.")
  (mapfunc-over-hashtable f hash-table nil))

(defun lmaphashnn (f hash-table) 
  #.(one-string-nl
     "Map F, a function of two arguments (key and value) over the entries"
     "of HASH-TABLE, returning a list of the non-nil results.")
  (mapfunc-over-hashtable f hash-table t))

(defun mapfunc-over-hashtable 
       (f hash-table &optional (remove-null-results nil))
  #.(one-string-nl
     "Returns a list of the results of executing (F key value), "
     "on all the key-value pairs in HASH-TABLE.  If REMOVE-NULL-RESULTS is"
     "non-nil, null return values are not included in the list returned.")
  (let ((results nil))
    (maphash 
     (lambda (key val) 
       (let ((x (funcall f key val)))
         (when (or (not remove-null-results) x) (push x results))
         ))
     hash-table)
    (setq results (nreverse results))
    results
    ))

(defun hash-tables-have-identical-keys? (h1 h2)
  #.(one-string-nl
     "Returns T if and only if both hash tables have the same"
     "number of elements, have the same test functions, and"
     "have the same keys (using the test function for comparison).")
  (block exit
    (unless (= (hash-table-count h1) (hash-table-count h2))
      (return-from exit nil))
    (unless (eq (hash-table-test h1) (hash-table-test h2))
      (return-from exit nil))
    (maphash 
     (lambda (key value) 
       (declare (ignore value))
       (multiple-value-bind (value2 present?) (gethash key h2)
         (declare (ignore value2))
         (unless present? (return-from exit nil))
         ))
     h1
     )
    t
    ))

(defun showhash (hash-table &optional (n 10))
  "Prints out up to N (default 10) hash table keys and values"
  (format t "~&")
  (cformatt "Hash table ~A, displaying ~D entries:" 
            hash-table (min n (hash-table-count hash-table)))
  (block exit
    (let ((count 0))
      (maphash 
       #'(lambda (key val) 
           (when (> (incf count) n) (return-from exit nil))
           (format t "~S ~S~%" key val))
       hash-table)))
  (terpri)
  nil)
       

(defmacro hash-table-list-generation-code (ht-symbol)
  `(let ((list nil))
     (maphash
      #'(lambda (key value) (push (list key value) list))
      ,ht-symbol)
     (reverse list)))


;;; Write out forms to a file that will define and repopulate a
;;; hash table from the data within the hash table HT.

;;; FILE can be a file, a stream (in which case the forms are simply
;;; output to the stream), or :forms, in which case a list of the
;;; forms created is returned.

;;; Options include specifying the name of the hash table, the predicate
;;; the hash table will use, exactly how the hash table will be recreated,
;;; name of the package the file written will have as the argument to its
;;; IN-PACKAGE form, and whether to compile the file after writing it out.

;;; The hash table can be recreated either by defining a function to do
;;; it (in which case that function can later be called by a user) and
;;; generating a form to call the function, or by storing the input 
;;; hash table data in a DEFPARAMETER in list format, and generating
;;; a form to loop over the list, or by writing out individual SETF
;;; forms, one for each KEY-VALUE pair of the hash table.
;;; The value of RECREATE-VIA can thus be :function, :defparameter or
;;; :load-forms.

(defun dump-hash-to-file 
    
    (ht file &key 
      (hash-table-name '*hash-table*) 
      (hash-table-predicate 'equal)
      (recreate-via :defparameter)
      (recreation-function-name nil)
      (inverse-function nil)
      (package :biolisp)
      (compile-file? nil)
      (keep-data-in-list? nil)
      &aux output
      )
  
  #.(one-string-nl
     "Writes out a hash table to a file or stream, in a way that the hash "
     "table can be recreated by loading the file to which it was written.")
  (labels 
      ((terpprint (form) 
         (cond 
          ((streamp output) (pprint form output) (terpri output))
          ((listp output) (push form output))
          ))
       (hash-data-setf-loop (data-symbol)
         `(loop for (key value) in ,data-symbol do
                (setf (gethash key ,hash-table-name) value)))
       (generate-hash-table-as-list (hash-table)
         (hash-table-list-generation-code hash-table))
       (package-gensym-symbol (string)
         (intern (symbol-name (gensym string)) (find-package package)))
       (body ()
         (when package
           (unless (keywordp package)
             (error "Package name, ~A, must be a keyword" package))
           (terpprint `(in-package ,package)))
         (terpprint
          `(defparameter ,hash-table-name 
             (make-hash-table :test #',hash-table-predicate)))
         (ecase recreate-via
           ((:defparameter :setq :defvar)
            (let ((symbol (package-gensym-symbol "HASH-TABLE-LIST-"))
                  (assign-symbol
                   (intern (string recreate-via) :common-lisp)))
              (terpprint
               `(,assign-symbol ,symbol ',(generate-hash-table-as-list ht)))
              (terpprint (hash-data-setf-loop symbol))
              (unless keep-data-in-list? (terpprint `(setq ,symbol nil)))))
           (:function
            (let ((symbol (or recreation-function-name
                            (package-gensym-symbol "HASH-TABLE-MAKE-"))))
              (terpprint
               `(defun ,symbol ()
                  (clrhash ,hash-table-name)
                  (let ((hash-data ',(generate-hash-table-as-list ht)))
                    ,(hash-data-setf-loop 'hash-data))))
              (terpprint `(,symbol))))
           (:load-forms
            (maphash
             (lambda (key value) 
               (terpprint 
                `(setf (gethash ',key ,hash-table-name) ',value)))
             ht)))
         (when inverse-function
           (terpprint
            `(defun ,inverse-function () 
               ,(macroexpand 
                 `(hash-table-list-generation-code ,hash-table-name)))))))
    
    
    (cond
     ((or (eq file :forms) (eq file :lists)) 
      (setq output nil) (body) (reverse output))
     ((streamp file) (setq output file) (body))
     (t
      (with-open-file (p file :direction :output :if-exists :supersede)
        (setq output p)
        (body))
      (when compile-file? (compile-file file))
      ))

    (namestring (merge-pathnames file))

    ))
       


;;; This works only if the function it's in is compiled. 
;;; If it's interpreted, the hashtable gets
;;; recreated each time (in ACL, at least).
;;; This only works when BODY always returns the same result. 
(defmacro memoize (arglist &body body)
  (let ((ht (make-hash-table :test #'equal)))
    `(let ((args (list ,@arglist)))
       (multiple-value-bind (val found)
	   (gethash args ,ht)
	 (if found 
	     val
	   (setf (gethash args ,ht)
	     (progn ,@body)))))))

;;; alternative: have a memoize object that lets you flush the table, override
;;; a value, etc.  Apparently PAIP has a memoization chapter.


;;; Kludge - an alternate version that won't store null values.
(defmacro memoize-if (arglist &body body)
  (let ((ht (make-hash-table :test #'equal)))
    `(let ((args (list ,@arglist)))
       (multiple-value-bind (val found)
	   (gethash args ,ht)
	 (if found 
	     val
	   (let ((new-val (progn ,@body)))
	     (if new-val (setf (gethash args ,ht) new-val))
	     new-val))))))


(defmacro def-memoized-function (name arglist &body body)
  `(defun ,name ,arglist
     (memoize ,arglist ,@body)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Returns four values
;; 1.  Doc string or NIL
;; 2.  List of decls (or NIL)
;; 3.  List of body forms (Not including bad decls from 4 below)
;; 4.  Declaration forms which are not in legal position

(defun parse-doc-decls-body (forms)
  #.(one-string-nl
     "Used to separate the documentation, declarations and real body "
     "from each other, given a list of forms usually constituting the body "
     "of a DEFUN, DEFMACRO, or similar construct.  Returns as multiple values "
     "the doc string, a list of declaration forms, a list of valid body forms "
     "and a list of invalid body forms (i.e., badly placed declarations).")
  (let ((doc-string nil)
        (declarations nil))
    (flet ((decl-form? (x) (and (consp x) (eq (car x) 'declare))))
      ;; look for declarations and doc string
      (do* ((f forms (cdr f)))
           ((null f) (setq forms f))
        (if (and (typep (car f) 'string) (null doc-string) (cdr f))
            (setq doc-string (car f))
          (if (decl-form? (car f))
              (push (car f) declarations)
            (progn (setq forms f) (return)))))
      (values
       doc-string
       declarations
       (remove-if #'decl-form? forms)
       (remove-if-not #'decl-form? forms)
       ))))

;;; Restrictions:  Nothing but required arguments allowed.
;;; Function must return a single value.
;;; If the DEFMEMOIZE form is recompiled or reevaluated, a
;;; new hash table will result.

(defmacro defmemoize (name arglist &body body)
  #.(one-string-nl
     "Define a function which caches previous results in a hash table "
     "keyed by ARGLIST instead of executing BODY again for the same set "
     "of input values.")
  (when (null arglist) (error "Having no arguments makes no sense."))
  (multiple-value-bind (doc decls body bad-decls)
      (parse-doc-decls-body body)
    (declare (ignore bad-decls))
    ;; Make sure there are no name conflicts between variables
    ;; in the argument list and variables used in the expansion.
    (let ((arglist-symbol (gensym "ARGLIST-"))
	  (argptr-symbol (gensym "ARGPTR-"))
	  (hash-table-symbol (gensym "HASH-TABLE-"))
	  (hash-value-symbol (gensym "HASH-VALUE-"))
	  (hash-key-found-symbol (gensym "HASH-KEY-FOUND-")))
      ;; Create a closure.  The function closes over the memoization 
      ;; hash table and also closes over a list where we will store
      ;; the arguments to the function (this list is then used as the
      ;; key for the hash table).  In this way we don't have to CONS
      ;; a new list of arguments every time the function is called.
      `(let ((,hash-table-symbol (make-hash-table :test #'equal))
	     ;; %%% The arglist has to be reconsed everytime
	     (,arglist-symbol nil))
	 (defun ,name ,arglist
	   ,@(and doc (list doc))
	   ,@decls
	   ;; %%% The arglist has to be reconsed everytime
           ,(if (= 1 (length arglist)) 
                `(setq ,arglist-symbol ,(first arglist))
              `(setq ,arglist-symbol (make-list ,(length arglist))))
           ,@(when (/= 1 (length arglist)) 
               `((let ((,argptr-symbol ,arglist-symbol))
                  ,@(butlast
                     (mapcan
                      #'(lambda (argname) 
                          `((setf (first ,argptr-symbol) ,argname)
                            (pop ,argptr-symbol)
                            ))
                      arglist)))))
	   (multiple-value-bind (,hash-value-symbol ,hash-key-found-symbol)
	       (gethash ,arglist-symbol ,hash-table-symbol)
	     (if ,hash-key-found-symbol
		 ,hash-value-symbol
	       (setf (gethash ,arglist-symbol ,hash-table-symbol)
                     (progn ,@body)
                     ))))))))



#+test
(defmemoize fred (x y) 
  "A simple memoization example"
  #.(optimization-declaration)
  (declare (fixnum x y))
  (sqrt (the fixnum (+ (the fixnum (* x x)) (the fixnum (* y y))))))



(defun maybe-make-valid-hash-test (test)
  (cond
   ((or (eq test 'string=) (eq test #'string=)) 'equal)
   ((or (eq test 'string-equal) (eq test #'string-equal)) 'equalp)
   (t test)
   ))

(defun valid-hash-test (x)
  (or (member x '(eq eql equal equalp))
      (eq x #'eq)
      (eq x #'eql)
      (eq x #'equal)
      (eq x #'equalp)
      ))

(defun canonicalize-hash-test (x)
  #.(one-string-nl
     "Returns a symbol which is one of EQ, EQL, EQUAL or EQUALP."
     "Converts the function objects #'EQ, #'EQL, #'EQUAL and #'EQUALP"
     "to their symbolic equivalents."
     "Converts CHAR= to EQL"
     "Converts STRING= and #'STRING= to EQUAL."
     "Converts CHAR-EQUAL, STRING-EQUAL and #'STRING-EQUAL to EQUALP."
     "Returns NIL if X cannot be converted as above and is not already"
     "in canonical (symbolic) form.")
  (cond
   ((member x '(eq eql equal equalp)) x)
   ((eq x #'eq) 'eq)
   ((eq x #'eql) 'eql)
   ((eq x #'equal) 'equal)
   ((eq x #'equalp) 'equalp)
   ((or (eq x 'string=) (eq x #'string=)) 'equal)
   ((or (eq x 'string-equal) (eq x #'string-equal)) 'equalp)
   ((or (eq x 'char=) (eq x 'char=)) 'eql)
   ((or (eq x 'char-equal) (eq x 'char-equal)) 'equalp)
   (t nil)
   ))

(defun check-for-duplicates 
       (list &key (key 'identity) (test 'eql) (hash-threshold 100))
  #.(one-string-nl
     "Returns a list of duplicated KEYS of or computed from LIST."
     "(The :KEY argument operates as with any sequence function.)"
     "If the list exceeds HASH-THRESHOLD and the TEST is compatible "
     "with using a hash table (EQ, EQL, EQUAL, EQUALP, STRING=, STRING-EQUAL) "
     "then a hash table algorithm is used instead of an O(n**2) double loop.")
  (setq test (maybe-make-valid-hash-test test))
  (let ((len (length list)) 
        (duplicates nil)
        (keys (if (eq key 'identity) list (mapcar key list))))
    (cond
     ((or (< len hash-threshold) (not (valid-hash-test test)))
      (loop for keylist on keys do
            (let ((key (first keylist)))
              (when (position key (cdr keylist) :test test) 
                (push key duplicates))
              ))
      duplicates
      )
     (t
      (let ((ht (make-hash-table :test test)))
        (loop for keylist on keys do
              (let ((key (first keylist)))
                (if (gethash key ht)
                    (push key duplicates)
                  (setf (gethash key ht) t)
                  )))
        duplicates
        )))))

(defun purge-duplicates 
       (list &key (key 'identity) (test 'eql) (hash-threshold 50))
  #.(one-string-nl
     "Returns a list of non-duplicate elements from LIST, such that"
     "the first element of each duplicate set is returned in the original"
     "list order.  So"
     ""
     "(PURGE-DUPLICATES '(\"a\" \"A\" \"B\" \"b\") :test 'string-equal)"
     ""
     "returns (\"a\" \"B\"), not (\"A\" \"b\"), (\"a\" \"b\") "
     "or any other variation."
     "KEY and TEST are as in (CHECK-FOR-DUPLICATES ..."
     "The algorithm will use an O(n) hash-table algorithm if LIST is as"
     "long or longer than HASH-THRESHOLD and TEST is valid for hashes,"
     "otherwise it will use the obvious O(N**2) double-iterative algorithm.")
  (setq test (maybe-make-valid-hash-test test))
  (let ((unique-items nil))
    (cond
     ((or (< (length list) hash-threshold) (not (valid-hash-test test)))
      (loop for items on list do
            (let* ((item (first items)) (keyval (funcall key item)))
              (unless (position keyval unique-items :test test :key key) 
                (push item unique-items)
                ))))
     (t
      (let ((ht (make-hash-table :test test)))
        (loop for item in list do
              (let ((keyval (funcall key item)))
                (unless (gethash keyval ht)
                  (push item unique-items)
                  (setf (gethash keyval ht) t)
                  ))))))
    (nreverse unique-items)
    ))
  
(defun find-duplicates 
       (list &key 
             (key 'identity) 
             (test 'eql) 
             (return-exactly-one-duplicate? nil)
             (return-all-duplicates? nil)
             (hash-threshold 50))

  #.(one-string-nl
     "Returns a list of duplicated elements from LIST, such that"
     "the second and subsequent occurences of an element are returned in"
     "the original list order.  So"
     ""
     "(FIND-DUPLICATES '(\"a\" \"A\" \"B\" \"b\" \"c\") :test 'string-equal)"
     ""
     "returns (\"A\" \"b\"), not (\"a\" \"b\"), (\"a\" \"B\") "
     "or any other variation."
     "KEY and TEST are as in (CHECK-FOR-DUPLICATES ..."
     "If RETURN-EXACTLY-ONE-DUPLICATE? is T, then"
     ""
     "(FIND-DUPLICATES '(1 2 1 3 1 2))"
     ""
     "returns (1 2), wherease if it were NIL, (1 1 2) would be returned."
     ""
     "If RETURN-ALL-DUPLICATES? is T, then all entries which are duplicates"
     "of each other are returned, including the 'original'.  So"
     ""
     "(FIND-DUPLICATES '(1 2 1 3 1 2) :return-all-duplicates? t)"
     "--> (1 1 1 2 2)"
     "(This is most useful when used with the :key parameter)"
     ""
     "The algorithm will use an O(n) hash-table algorithm if LIST is as"
     "long or longer than HASH-THRESHOLD and TEST is valid for hashes,"
     "or RETURN-ALL-DUPLICATES? is T,"
     "otherwise it will use the obvious O(N**2) double-iterative algorithm.")

  (setq test (maybe-make-valid-hash-test test))

  (let ((dups nil))

    (cond
     (return-all-duplicates?
      (let ((ht (make-hash-table :test test)))
        (loop for item in list do
              (let* ((keyval (funcall key item))
                     (count-and-data (gethash keyval ht))
                     (count (first count-and-data))
                     (data (second count-and-data)))
                (cond
                 ((null count) (setf (gethash keyval ht) (list 1 item)))
                 ((= count 1)
                  (push data dups)
                  (push item dups)
                  (incf (first (gethash keyval ht))))
                 (t 
                  (push item dups)
                  (incf (first (gethash keyval ht)))
                  ))))))
     ((or (< (length list) hash-threshold) (not (valid-hash-test test)))
      (loop for item in list
            for index fixnum from 0 do
            (let* ((keyval (funcall key item)))
              (vwhen (pos (position keyval list :test test :key key)) 
                (when (< pos index)
                  (when (or (null return-exactly-one-duplicate?)
                            (null (position keyval dups :test test :key key)))
                    (push item dups)
                    ))))))
     (t
      (let ((ht (make-hash-table :test test)))
        (loop for item in list do
              (let* ((keyval (funcall key item))
                     (count (gethash keyval ht)))
                (if (null count)
                    (setf (gethash keyval ht) 1)
                  (progn
                    (when (or (= count 1) (null return-exactly-one-duplicate?))
                      (push item dups))
                    (incf (gethash keyval ht))
                    )))))))

    (nreverse dups)

    ))

     
(defun intersection-size
       (list1 list2 &key (key 'identity) (test 'eql) (hash-threshold 400))
  #.(one-string-nl
     "Returns number of elements in intersection of LIST1 and LIST2"
     "without actually consing up the intersection list.  If the product of the"
     "lengths of the lists exceeds HASH-THRESHOLD an O(n) algorithm involving"
     "hash table lookup is used, otherwise the obvious O(n**2) method is used.")
  (combined-set-size list1 list2 key test hash-threshold :intersection)
  )

(defun union-size
       (list1 list2 &key (key 'identity) (test 'eql) (hash-threshold 400))
  #.(one-string-nl
     "Returns number of elements in union of LIST1 and LIST2 without"
     "actually consing up the union list.  If the product of the lengths"
     "of the lists exceeds HASH-THRESHOLD an O(n) algorithm involving hash"
     "table lookup is used, otherwise the obvious O(n**2) method is used.")
  (combined-set-size list1 list2 key test hash-threshold :union)
  )

(defun combined-set-size (list1 list2 key test hash-threshold type)
  #.(optimization-declaration)
  (setq test (maybe-make-valid-hash-test test))
  (let* ((l1 (length list1)) 
         (l2 (length list2)) 
         (product (the fixnum (* l1 l2)))
         (invert? (ecase type (:intersection nil) (:union t)))
         )
    (declare (fixnum l1 l2 product))
    ;; Canonicalize so LIST1 is always the longer list.
    (when (< l1 l2) (psetq list1 list2 list2 list1))
    (cond
     ((zerop product) (ecase type (:intersection 0) (:union (max l1 l2))))
     ((or (< product hash-threshold) (not (valid-hash-test test)))  
      (let ((count (ecase type (:intersection 0) (:union (max l1 l2)))))
        (declare (fixnum count))
        ;; Iterate through smaller list.
        (dolist (item list2 count)
          (let ((increment? 
                 (position (funcall key item) list1 :test test :key key)))
            (when invert? (setq increment? (not increment?)))
            (when increment? (incf count))
            ))))
     (t
      (let* ((size (min l1 l2)) 
             (ht (make-hash-table :test test :size size))
             (count (ecase type (:intersection 0) (:union size))))
        ;; Insert smaller number of elements into hash table
        (dolist (item list2) (setf (gethash (funcall key item) ht) t))
        (dolist (item list1 count)
          (let ((increment? (gethash (funcall key item) ht)))
            (when invert? (setq increment? (not increment?)))
            (when increment? (incf count))
            )))))))



(defun check-if-any-a-is-not-in-b (a b &key (test 'eql) (hash-threshold 1000))
  #.(one-string-nl
     "Returns any element of A that is not in B."
     "If the product of the lengths of A and B exceeds HASH-THRESHOLD "
     "and the TEST is compatible with using a hash table"
     "(EQ, EQL, EQUAL, EQUALP, STRING=, STRING-EQUAL) "
     "then a hash table algorithm is used instead of an O(n**2) double loop.")
  (let ((test (maybe-make-valid-hash-test test))
        (lena (length a))
        (lenb (length b))
        (bad-as nil))
    (cond
     ((or (< (* lena lenb) hash-threshold) (not (valid-hash-test test)))
      (dolist (elem a) (unless (member elem b :test test) (push elem bad-as))))
     (t
      (let ((ht (make-hash-table :test test)))
        (dolist (elem b) (setf (gethash elem ht) t))
        (dolist (elem a) (unless (gethash elem ht) (push elem bad-as)))
        )))
    bad-as
    ))


(defun set-equal (seq1 seq2 &key (test 'eql) (hash-threshold 256))
  (one-string-nl
   "Returns T if SEQ1 and SEQ2 contain the same elements, independent of"
   "order.  Returns NIL if SEQ1 and SEQ2 are different lengths.  The"
   "sequences are assumed to be sets -- i.e., no duplicate elements. If"
   "either sequence contains duplicates the results are undefined."
   "TEST is used to decide if two elements, one from each sequence,"
   "are equal."
   "If the product of the lengths of the two sequences is >= HASH-THRESHOLD"
   "a hash table lookup algorithm is used instead of an O(N**2) algorithm."
   "In this case TEST must be an equality predicate suitable for use"
   "with MAKE-HASH-TABLE, or STRING= or STRING-EQUAL.")
  (block exit
    (flet ((set-equal-via-hash (size)
             (let ((test (maybe-make-valid-hash-test test)))
               (unless (valid-hash-test test)
                 (error "Test must be valid HASH predicate!"))
               (let ((ht (make-hash-table :test test :size size)))
                 (map nil (lambda (elem) (setf (gethash elem ht) t)) seq1)
                 (map nil (lambda (elem) 
                            (unless (gethash elem ht) (return-from exit nil)))
                      seq2)
                 t))))
      (let ((len1 (length seq1)) (len2 (length seq2)))
        (declare (fixnum len1 len2))
        (cond
         ((zerop len1) (zerop len2))
         ((/= len1 len2) nil)
         ((and (listp seq1) (listp seq2))
          (cond
           ((every test seq1 seq2) t)
           ((>= (* len1 len2) hash-threshold)
            (set-equal-via-hash len1))
           (t 
            (null (set-difference seq1 seq2 :test test))
            )))
         ((< (* len1 len2) hash-threshold)
          (set-equal 
           (coerce seq1 'list) (coerce seq2 'list)
           :test test :hash-threshold hash-threshold
           ))
         (t (set-equal-via-hash len1))
         )))))


(defun binsearch 
       (item vector 
             &key
             (test #'<)
             (test-type :two-way)
	     (key #'identity) 
	     (start 0) 
             (end nil)
             (if-not-found nil))
  #.(one-string-nl
     "General binary search on a vector."
     "Returns the index of the element equivalent to ITEM via TEST and KEY."
     "END is inclusive, so END is never >= the length of VECTOR."
     "The test function can be either a predicate (:two-way), returning"
     "true if for (test x y) x comes before y, or a function returning"
     "either -1, 0 or 1 (:three-way), returning -1 if for (test x y)"
     "x comes before y, or 0 if x is equivalent to y, or 1 if y comes"
     "before x.  If the ITEM is not found, then NIL is returned.  If"
     "IF-NOT-FOUND is the value :RANGE or :START-END then the 2nd and 3rd"
     "values returned if the ITEM is not found are the indices of the"
     "elements between which ITEM would go were it inserted into VECTOR."
     "If ITEM would go before the first element then the values NIL NIL 0"
     "are returned, while if ITEM would go after the last element then the"
     "values NIL <LASTINDEX> NIL are returned.")
  (unless (and (integerp start) (not (minusp start)))
    (error "Invalid START argument to BINSEARCH: ~D" start))
  (unless (or (and (integerp end) (plusp end)) (null end))
    (error "Invalid END argument to BINSEARCH: ~D" end))
  (let ((real-end (if (null end) (1- (length vector)) end)))
    (unless (<= start real-end) 
      (error "START, ~D, should be less than END, ~D" start real-end))
    (case if-not-found
      ((:range :start-end)
       (binsearch2-aux item vector test test-type key start real-end))
      (t
       (binsearch-aux item vector test test-type key start real-end))
      )))

(defun binsearch-aux (item vector test test-type key start end)
  #.(optimization-declaration)
  (labels 
      ((bs-eq (element)
	 (ecase test-type
	   (:three-way (zerop (the fixnum (funcall test item element))))
	   (:two-way
	    (and (not (funcall test item element)) 
		 (not (funcall test element item))
		 ))))
       (bs-lt (element)
	 (ecase test-type
	   (:three-way (= -1 (the fixnum (funcall test item element))))
	   (:two-way (funcall test item element))
	   ))
       (bs-aref (index)
	 (declare (fixnum index))
	 (if (eq key #'identity) 
	     (aref vector index) 
	   (funcall key (aref vector index))))
       (bs (start end)
	 (declare (fixnum start end))
	 (cond
	  ((< end start) nil)
	  ((= end start) 
	   (and (bs-eq (bs-aref start)) start))
	  ((= end (the fixnum (1+ start)))
	   (cond
	    ((bs-eq (bs-aref start)) start)
	    ((bs-eq (bs-aref end)) end)
	    (t nil)
	    ))
	  (t (let* ((mid (the fixnum (ash (the fixnum (+ start end)) -1))))
	       (declare (fixnum mid))
	       (cond
		((bs-lt (bs-aref mid)) (bs start (the fixnum (1- mid))))
		(t (bs mid end))
		))))))
    (bs start end)
    ))

(defun binsearch2-aux (item vector test test-type key start end)
  (COND
   ;; fixes problem with lists or vectors of length 1
   ((= end 0)
    (cond
     ((< item (funcall key (aref vector 0))) (values nil nil 0))
     ((> item (funcall key (aref vector 0))) (values nil 0 nil))
     (t 0)))
   ((>= start end) (error "start must be less than than end!"))
   (t 
    (labels 
        ((bs-eq (element)
           (ecase test-type
             (:three-way (zerop (the fixnum (funcall test item element))))
             (:two-way
              (and (not (funcall test item element)) 
                   (not (funcall test element item))
                   ))))
         (bs-lt (element)
           (ecase test-type
             (:three-way (= -1 (the fixnum (funcall test item element))))
             (:two-way (funcall test item element))
             ))
         (bs-aref (index)
           (declare (fixnum index))
           (if (eq key #'identity) 
               (aref vector index) 
             (funcall key (aref vector index))))
         (bs (start end)
           (declare (fixnum start end))
           (cond
            ((= end (the fixnum (1+ start)))
             (cond
              ((bs-eq (bs-aref start)) start)
              ((bs-eq (bs-aref end)) end)
              ((bs-lt (bs-aref start)) (values nil nil 0))
              ((bs-lt (bs-aref end)) (values nil start end))
              (t (values nil end nil))
              ))
            (t 
             (let ((mid (the fixnum (ash (the fixnum (+ start end)) -1))))
               (declare (fixnum mid))
               (let ((mid-elem (bs-aref mid)))
                 (cond
                  ((bs-lt mid-elem) (bs start mid))
                  ((bs-eq mid-elem) mid)
                  (t (bs mid end))
                  )))))))
      (bs start end)
      ))))

#||

(utils::binsearch-file 
 "xyzz" "C:/Lispcode/Biolisp/utils/foo.txt"
 :key-length 4
 :value-length 6
 :record-size 11
 )

(defun binsearch-file
       (key-to-find file-or-stream 
             &key
             (test-function #'string-lessp)
             (test-type :two-way)
             (key-start-position 0)
             (key-length 1)
	     (key-transform #'identity) 
             (value-start-position (+ key-start-position key-length))
             (value-length 1)
             (record-size (+ key-length value-length))
             (if-not-found-value nil)
             (first-record-pos 0)
             (number-of-records nil)
             )
  #.(one-string-nl
     "General binary search on a file."
     "Returns the VALUE associated with KEY-TO-FIND if KEY-TO-FIND is"
     "found in FILE-OR-STREAM."
     "The test function can be either a predicate (:two-way), returning"
     "true if for (test x y) x comes before y, or a function returning"
     "either -1, 0 or 1 (:three-way), returning -1 if for (test x y)"
     "x comes before y, or 0 if x is equivalent to y, or 1 if y comes"
     "before x."
     "If KEY-TO-FIND is not found, then IF-NOT-FOUND-VALUE is returned as"
     "the first value, and the 2nd and 3rd values are the positions of the"
     "records between which KEY-TO-FIND would go if it were inserted."
     "(If KEY-TO-FIND would go before the first element then the values"
     "NIL and 0 are returned as the 2nd and 3rd values, while if KEY-TO-FIND"
     "would go after the last element then the values <LASTINDEX> and NIL"
     "are returned as the 2nd and third values."
     "KEY-START-POSITION and KEY-LENGTH define where the keys live within"
     "a record."
     "VALUE-START-POSITION and VALUE-LENGTH define where the values live"
     "within a record."
     "RECORD-SIZE defines the (fixed) size of each record in the file."
     "KEY-TRANSFORM is applied to the extracted key before TEST-FUNCTION"
     "is used to compare it with KEY-TO-FIND."
     "FIRST-RECORD-POS is where the first key record is located."
     "NUMBER-OF-RECORDS is the number of key records in the file.  This"
     "allows for a file to contain data beyond the key records.  If the"
     "default (NIL) is used, the entire file starting at FIRST-RECORD-POS"
     "is assumed to be key records."
     )
  (flet ((ip (x) (and (integerp x) (plusp x)))
         (inn (x) (and (integerp x) (not (minusp x))))
         (do-it (stream)
           (binsearch-file-aux
            key-to-find stream
            test-function test-type
            key-start-position key-length key-transform
            value-start-position value-length
            record-size
            if-not-found-value
            first-record-pos
            number-of-records
            )))
    (unless (ip record-size)
      (error "Invalid RECORD-SIZE argument to BINSEARCH-FILE: ~D" record-size))
  (unless (and (inn key-start-position) (< key-start-position record-size))
    (error "Invalid KEY-START-POSITION argument to BINSEARCH-FILE: ~D" 
           key-start-position
           ))
  (unless (and (ip key-length) 
               (<= key-length (- record-size key-start-position)))
    (error "Invalid KEY-LENGTH argument to BINSEARCH-FILE: ~D" key-length))
  (unless (and (inn value-start-position) (< value-start-position record-size))
    (error "Invalid VALUE-START-POSITION argument to BINSEARCH-FILE: ~D" 
           value-start-position
           ))
  (unless (and (inn value-length)
               (<= value-length (- record-size value-start-position)))
    (error "Invalid VALUE-LENGTH argument to BINSEARCH-FILE: ~D" 
           value-length
           ))
  (typecase file-or-stream
    ((or string pathname)
     (with-open-file (p file-or-stream :direction :input) (do-it p)))
    (stream (do-it file-or-stream))
    )))



(defmacro bsf-record-pos (frp ri rs)
  `(the fixnum (+ (the fixnum ,frp) 
                  (the fixnum (* (the fixnum ,ri) (the fixnum ,rs)))
                  )))

(defun bsf-number-of-key-records (stream first-record-pos record-size)
  (let* ((file-size (file-length stream))
         (number-of-key-bytes (- file-size first-record-pos)))
    (multiple-value-bind (q r) (floor number-of-key-bytes record-size)
      (unless (zerop r)
        (error 
         (formatn
          (one-string-nl
           "Invalid file format.  Last record doesn't have enough"
           "bytes:  Key record length: ~D, last record size: ~D,"
           "first record position: ~D, Total file size: ~D")
          record-size r first-record-pos file-size
          )))
      q
      )))

(defun binsearch-file-aux
       (
        key-to-find
        stream
        test-function test-type
        key-start-position key-length key-transform
        value-start-position value-length
        record-size
        if-not-found-value
        first-record-pos
        number-of-records
        )
  (declare (fixnum key-start-position key-length))
  (declare (fixnum value-start-position value-length))
  (declare (fixnum record-size first-record-pos))
  #.(optimization-declaration)
  (macrolet ((tfn (x) `(the fixnum ,x)))
    (let* ((key-buffer (make-string key-length))
           (value-buffer (make-string value-length))
           (frp first-record-pos)
           (rs record-size)
           (nr number-of-records)
           (tf test-function)
           (fri 0)
           (lri 
            (if (null nr)
                (tfn (bsf-number-of-key-records stream frp rs))
              (tfn (1- nr))
              )))
      (declare (fixnum frp rs fri lri))
      (labels 
          ((bs-eq (key-from-file)
             (ecase test-type
               (:three-way 
                (zerop (tfn (funcall tf key-to-find key-from-file))))
               (:two-way
                (and (not (funcall tf key-to-find key-from-file)) 
                     (not (funcall tf key-from-file key-to-find))
                     ))))
           (bs-lt (key-from-file)
             (ecase test-type
               (:three-way (= -1 (tfn (funcall tf key-to-find key-from-file))))
               (:two-way (funcall tf key-to-find key-from-file))
               ))
           (key-at-record-position (p)
             (file-position stream (tfn (+ p key-start-position)))
             (read-sequence key-buffer stream)
             (funcall key-transform key-buffer))
           (value-at-record-position (p)
             (file-position stream (tfn (+ p value-start-position)))
             (read-sequence value-buffer stream)
             value-buffer)
           (bs (sri eri)
             (declare (fixnum sri eri))
             (print (list sri eri))
             (cond
              ((= sri eri)
               (let* ((srp (bsf-record-pos frp sri rs))
                      (skey (key-at-record-position srp)))
                 (cond
                  ((bs-eq skey)
                   (values (value-at-record-position srp) nil nil))
                  ((bs-lt skey)
                   (if (zerop sri)
                       (values if-not-found-value nil sri)
                     (values if-not-found-value (tfn (1- sri)) sri)
                     ))
                  (t 
                   (if (= sri lri)
                       (values if-not-found-value sri nil)
                     (values if-not-found-value sri (tfn (1+ sri)))
                     )))))
              ((= eri (tfn (1+ sri)))
               (let ((srp (bsf-record-pos frp sri rs))
                     (erp (bsf-record-pos frp eri rs)))
                 (declare (fixnum srp erp))
                 (cond
                  ((bs-eq (key-at-record-position srp))
                   (values (value-at-record-position srp) nil nil))
                  ((bs-eq (key-at-record-position erp)) 
                   (values (value-at-record-position erp) nil nil))
                  ((bs-lt (key-at-record-position srp))
                   (values if-not-found-value nil sri))
                  ((bs-lt (key-at-record-position erp))
                   (values if-not-found-value sri eri))
                  (t (values if-not-found-value eri nil))
                  )))
              (t (let* ((midi (the fixnum (ash (the fixnum (+ sri eri)) -1)))
                        (midp (bsf-record-pos frp midi rs))
                        (mid-key (key-at-record-position midp)))
                   (declare (fixnum midi midp))
                   (cond
                    ((bs-eq mid-key) 
                     (values (value-at-record-position midp) nil nil))
                    ((bs-lt mid-key) 
                     (bs sri (tfn (1- midi))))
                    (t (bs (tfn (1+ midi)) eri))
                    ))))))

        (bs fri lri)

        ))))

(defstruct pframe id goo)



||#

(defun bs-3test (x y) (cond ((< x y) -1) ((> x y) 1) (t 0)))



(defun group-by-type 
       (sequence &key (key 'identity) (test 'equal) (sort? t) (sublists? t))
  #.(one-string-nl
     "Group a list of objects by type, and return the types, the counts and"
     "the groupings."
     "Two objects are of the same type if TEST is true when called on the"
     "results of calling KEY on the two objects."
     "KEY must be a one-argument function that extracts the relevant datum"
     "from each element of SEQUENCE.  It defaults to the IDENTITY function."
     "TEST is the function used to compare the returned values from KEY,"
     "which must be suitable for use as a hash table equality predicate."
     "For example, if you wanted to organize a bunch of lists"
     "by their second element, you would use :KEY 'SECOND."
     "GROUP-BY-TYPE returns a list of lists, one for each unique key."
     "Each sublist is of the form (<count> <key> <list-of-elements>)."
     "By using a predicate as the key function,"
     "GROUP-BY-TYPE can be used to divide a set into two (or more) subsets."
     "For example:"
     ";; Divide a set of pairs into two groups based upon whether the"
     "second term is even or odd."
     ">> (setq myset '((a 1) (b 2) (c 3) (d 4)))"
     ">> (group-by-type myset :key (lambda (item) (evenp (second item))))"
     ":: ((2 NIL ((C 3) (A 1))) (2 T ((D 4) (B 2))))"
     "There are two odds (evenp => NIL) and two evens (evenp => T). "
     "The THIRD element of each result is the list of objects in that group."
     "The result is returned in sorted (largest count first) order by <count>."
     "The order of the elements in each group sublist is the reverse of their"
     "order of appearance in the input sequence)."
     "If SORT? is NIL, no sorting is done."
     "If SUBLISTS? is NIL, only <count> and <key> are meaningful."
     "(<list-of-elements> will always be NIL if SUBLISTS? is NIL.)"
     )
  (let ((hash-predicate (maybe-make-valid-hash-test test)))
    (unless (valid-hash-test hash-predicate)
      (error "Test, ~A, is not a valid hash predicate" test))
    (let ((table (make-hash-table :test hash-predicate)))
      (map 
       nil
       (lambda (e) 
         (let ((hashkey (funcall key e)))
           (if sublists?
               (push e (gethash hashkey table))
             (incf (gethash hashkey table 0))
             )))
       sequence)
      (let ((results
             (lmaphash 
              (lambda (key value)
                (if sublists? 
                    (list (length value) key value)
                  (list value key nil)
                  ))
              table
              )))
        (if sort? (sort results '> :key 'first) results)
        ))))


(defun freqsort (list &key (test #'equal) (sort-direction :descending))
  #.(one-string-nl
     "LIST is a list with (possibly) duplicate elements. FREQSORT returns"
     "a list of sublists, each sublist of the form (count element), where"
     "COUNT is the number of times ELEMENT appears in LIST. The returned"
     "list is sorted by COUNT, so that, by default, the most common"
     "element appears first, etc.  This is useful for making histograms.")
  (let ((hash-predicate (maybe-make-valid-hash-test test)))
    (unless (valid-hash-test hash-predicate)
      (error "Test, ~A, is not a valid hash predicate" test))
    (let ((table (make-hash-table :test hash-predicate)))
      (loop for item in list do (incf (gethash item table 0)))
      (sort (lmaphash (lambda (concept count) (list count concept)) table)
            (ecase sort-direction
              ((:descending :down) '>)
              ((:ascending :up) '<)
              )
            :key 'first
            ))))

(defun set-difference-hash (h1 h2)
  "Returns a list of keys found in h1 that are not keys in h2"
  (let ((set nil))
    (maphash 
     (lambda (key value) (declare (ignore value)) 
       (unless (gethash key h2) (push key set))) 
     h1
     )
    set
    ))