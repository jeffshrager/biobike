(in-package :snark)

(print "Loading biosnark add-ons (v22)...")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export 
  '(
    use-snark
    do-snark-shadowing-imports

    f2s ; macro

    replace-hashdollar
    prefix-hashdollar
    make-frame
    remove-hashdollar
    is-hashdollar
    s2f
    declare-f2s
    ANSWER-TO-LISP
    FIRST-ANSWER-TO-LISP
    EVALUATE-ANSWER
    EVALUATE-ANSWER-W-ERRORS
    FIND-ONE
    ANOTHER
    FIND-ALL
    FIND-REST

    remove-nth
    function-satisfies-relation
    relation-satisfier
    function-falsifies-relation
    relation-falsifier
    function-rewrites-operator
    function-rewriter
    ashdollar
    prefix-hashdollar
    make-frame
    remove-hashdollar
    is-hashdollar
    s2f
    declare-f2s
    ANSWER-TO-LISP
    FIRST-ANSWER-TO-LISP
    EVALUATE-ANSWER
    EVALUATE-ANSWER-W-ERRORS
    FIND-ONE
    ANOTHER
    FIND-ALL
    FIND-REST

    )))


(defun use-snark ()
  (utils::initialize-saved-variables 
   (intern "SNARK" wb::*username*)
   snark::*snark-globals*))

(defun do-snark-shadowing-imports ()
  (format t "Shadowing SNARK symbols in ~s~%" wb::*username*)
  (shadowing-import 'snark::true wb::*username*)
  (shadowing-import 'snark::implies wb::*username*)
  (shadowing-import 'snark::forall wb::*username*)
  (shadowing-import 'snark::exists wb::*username*)
  (shadowing-import 'snark::iff wb::*username*)
  (shadowing-import 'snark::implied-by wb::*username*)
  (shadowing-import 'snark::function-sort wb::*username*)
  (shadowing-import 'snark::new-row-context wb::*username*)
  (shadowing-import 'snark::prove wb::*username*)
  (shadowing-import 'snark::answer wb::*username*)
  (shadowing-import 'snark::proof wb::*username*) 
  (shadowing-import 'snark::true wb::*username*)
  (shadowing-import 'snark::assert wb::*username*)
  (shadowing-import 'snark::implies wb::*username*)
  (shadowing-import 'snark::forall wb::*username*)
  (shadowing-import 'snark::exists wb::*username*)
  (shadowing-import 'snark::iff wb::*username*)
  (shadowing-import 'snark::implied-by wb::*username*)
  (shadowing-import 'snark::function-sort wb::*username*)
  (shadowing-import 'snark::new-row-context wb::*username*)
  (shadowing-import 'snark::prove wb::*username*)
  (shadowing-import 'snark::answer wb::*username*)
  (shadowing-import 'snark::proof wb::*username*)
  (shadowing-import 'snark::prog-> wb::*username*)
  (shadowing-import 'snark::function-symbol-p wb::*username*)
  (shadowing-import 'snark::function-sort-declarations wb::*username*)
  (shadowing-import 'snark::fsd-argument-sort-alist wb::*username*)
  ;; For reasons I can't figure out, these need to be evaluated by the user from his/her package directly.
;  (eval (read-from-string "(shadowing-import 'snark::assert wb::*username*)"))
;  (eval (read-from-string "(shadowing-import 'snark::? wb::*username*)"))
  )

(defmacro f2s (frm) `(replace-hashdollar ,frm))

(defvar *evaluables*
  '(list genes orthologs intersection set-difference t1 project
	 select selectneg select-pair select-pair-rel 
	 map-compose neg-compose 
	 mapcan function funcall of 
	 evenp < hihara-exceeds-2))

;; replaces #$ with hashdollar-

(defun replace-hashdollar (sym)
  (cond ((frames::isframe? sym) (bio::slotv sym #$fname))
	((symbolp sym) sym)
	((listp sym) (mapcar #'replace-hashdollar sym))
	(t (error "replace-hashdollar given: ~a, unknown type ~a" sym (type-of sym)))))

(defun prefix-hashdollar (sym)
  ;; adds hashdollar to symbol
  (intern (format nil "hashdollar-~A" sym)))

(defun make-frame (sym)
  ;; produces frame named name, 
  ;; given hashdollar-name.
  (or (bio::frame-fnamed (remove-hashdollar sym) nil)
      (remove-hashdollar sym)))

(defun remove-hashdollar (sym)
  ;; produces sym0 from hashdollar-sym0 returns argument unchanged if
  ;; it doesn't begin with hashdollar
  (or (is-hashdollar sym) sym))

(defun is-hashdollar (sym)
  ;; tests if sym begins with the prefix "hashdollar-"; if so, removes
  ;; the prefix.
  (and (atom sym)
       (let* ((str (format nil "~A" sym)))
	 (and (<= 11 (length str)) 
	      (string-equal (subseq str 0 11) "hashdollar-")
	      (intern (format nil (subseq str 11)))))))

(defun s2f (exp)
  ;; Converts expression with hashdollar symbols into frames.
  (if (atom exp)
      (let* ((suffix (is-hashdollar exp)))
	(if suffix
	    (bio::frame-fnamed suffix nil)
	  exp))
    (mapcar #'s2f exp)))

(defun declare-f2s (frm srt)
  ;; Replaces #$ of frm with hashdollar and declares the result to be
  ;; of sort srt.
  (if (atom frm) 
      (declare-constant (replace-hashdollar frm) :sort srt)
    (replace-hashdollar frm)))

;;; Procedures for Displaying Answer Found by Proof: 

;;;; Takes the answer discovered by the proof and converts it from
;;;; SNARK form to LISP form for display.

(defun ANSWER-TO-LISP (ansatom)
  (if (proof)
      (if (eq (answer) 'FALSE)    ;;;contradiction in axioms
	  (s2f ansatom)
	(evaluate-answer
	      (s2f (second (term-to-lisp (answer))))
	    *evaluables*
	      ))
    :none))

;;; Convert the first component of the multi-component answer into LISP.

(defun FIRST-ANSWER-TO-LISP (ansatom)
  (if (proof)
      (if (eq (answer) 'FALSE)   ;;;contradiction in axioms
	  (second ansatom)
	  (second (term-to-lisp (answer)));;(second (answer))
	  )
    :none))

(defun EVALUATE-ANSWER (ans *evaluables*)
  (multiple-value-bind (val err)
      (ignore-errors
	(eval ans)	    ;(evaluate-answer-w-errors ans *evaluables*)
	)    
    (if err
	ans
      val)))

(defun EVALUATE-ANSWER-W-ERRORS (ans *evaluables*)
  (if (atom ans)
      (s2f ans)
    (apply (car ans) 
	   (s2f
	    (mapcar 
	     #'(lambda (ans1) 
		 (evaluate-answer-w-errors ans1 *evaluables*))
	     (cdr ans))))))


(defun FIND-ONE (&optional (p 'true) &key documentation answer name sequential 
			   (time-limit 10))
  (new-row-context)
  (run-time-limit time-limit)
  ;; Prove == asserting the negative of p and putting it into the set of support 
  ;; and then doing deductive closure on everything. -- the results are any 
  ;; contradiction. This is the whole refutational theorem proving theory.
  (prove p :answer `(answer-- ,answer)
	 :sequential sequential
	 :name name)
  (list `(question-- ,p)
	documentation
	(print
	 (answer-to-lisp (answer))))
  )

;;; Another seeks another answer to the previous question, by seeking
;;; other proofs of the same theorem.

(defun ANOTHER (&optional  &key (time-limit 10))
  (run-time-limit time-limit)
  (closure)
  (answer-to-lisp (answer)))

(defun FIND-ALL (&optional (p 'true) &key documentation answer name sequential
			   (time-limit 10))
  ;; Find a list of all the answers up to a time limit. 
  (cons
   (find-one p 
	     :documentation documentation
	     :answer answer
	     :sequential sequential
	     :name name
	     :time-limit time-limit)
   (find-rest nil :time-limit time-limit)
   ))

(defun FIND-REST  (answer-list  &key (time-limit 10))
  ;; Find a list of the remaining answers up to a time limit.
  (run-time-limit time-limit)
  (closure)
  (if (proof)
      (find-rest (cons (answer-to-lisp (answer)) answer-list))
    answer-list))

;;; ================================================================
;;; Functions necessary to establish procedural attachments between
;;; biodeducta relations and biolingua functions:

;; ;;  Here are functions necessary to establish procedural attachments
;; ;;  between biodeducta relations and bioBike functions.

(defun remove-nth (n l)
  ;; removes  the n-th element from l;  1-based.
  (if (equal n 0) l
    (if (null l) nil
      (if (equal n 1) (rest l)
	  (cons (first l) (remove-nth (- n 1) (rest l)))))))
  
(defun function-satisfies-relation (fun n)
  ;; generates satisfy-code that lets the nth argument of the relation
  ;; be satisfied by elements of the list yielded when fun is applied
  ;; to the other arguments, in order.

    #'(lambda (cc atom subst)
       (relation-satisfier 
			 fun cc atom subst n)))

(defun relation-satisfier (fun cc atom subst n)
  ;; Assumes atom is of form (rel const... var ...const) where only
  ;; the n-th argument is a variable.
  ;; The procedural attachment for rel will invoke fun in this case.
  (let* ((rel (head atom)) 
	 (args (args atom))
	 (var (nth (- n 1) args))
	 (const-list
	  (mapcar 
	   #'(lambda (const)
	       (dereference const subst
			    :if-variable 
			    (return-from relation-satisfier :none)
			    :if-constant
			    (make-frame const)))
	   (remove-nth n args))))
    ;; convert constants to frames.
    ;; ensure none in const-list is variable.
    (mapc
     (lambda (item) 
       (let* ((item 
	       (declare-f2s
		item
		(cdr (nth (- n 1)
			  (fsd-argument-sort-alist
			   (first (function-sort-declarations rel))))))))
	 (unify cc var item subst)))
     (apply fun const-list))
    ;; unify var with each of the items in fun(const-list)
    ))

(defun function-falsifies-relation (fun n)
  ;; Generates falsify-code that allows the relation to be falsified if
  ;; the result of applying fun to all but the nth argument of the
  ;; relation is nil.

  #'(lambda (cc atom subst)
	   (relation-falsifier
	    fun cc atom subst n)))

(defun relation-falsifier (fun cc atom subst n)
  ;; Assumes that atom is of form (rel c1, c2, ...,var, ...,) where
  ;; all but the nth argument are constant.  The procedural attachment
  ;; to rel will falsify the atom if (f c1, c2,..,) is nil.
  (let* ((rel (head atom)) 
	 (args (args atom))
	 
	 (const-list
	  (mapcar 
	   #'(lambda (const)
	     (dereference const subst
			  :if-variable 
			  (return-from relation-falsifier :none)
			  :if-constant
			  (make-frame const)))
	   (remove-nth n args)))
	 ;; convert all constants to frames;
	 ;; ensure that none is a variable
	 )
    (if (apply fun const-list) :none
      (funcall cc subst)))
)

(defun function-rewrites-operator (fun)
  ;; generates a procedural attachment that allows a term headed by
  ;; operator to be rewritten by a lisp function.
 #'(lambda (term subst) (function-rewriter fun term subst)))

(defun function-rewriter (fun term subst)
  ;; term is of form (op c1 ... cn, where all arguments are constant.
  ;; Makes a procedural attachment to op that replaces term with the
  ;; value of the Lisp expression (fun c1 ... cn).

  (let* ((op (head term))
	 (args (args term))
	 (const-list 
	  (mapcar 
	   #'(lambda (const)
	       (dereference const subst
			    :if-variable
			    (return-from function-rewriter :none)
			    :if-compound
			    (return-from function-rewriter :none)
			    :if-constant
			    (make-frame const)
			    ))
	   args))
	 (value-sort (first (first (function-sort (head term)))))
	 (valu (apply fun const-list)))
    (if valu
	(dereference valu subst
		     :if-constant 
		     (declare-constant valu :sort value-sort)
		     :if-compound valu
		     :if-variable valu)
      :false)))

;;; JS20051215: I can't figure out what's wrong with the following,
;;; all that I did, i think, is to add a (remove nil ...) but it
;;; causes certain calls to blow up, apparently trying to change the
;;; var (e.g., ?GENE123) to a string in the make-frame call????
;;; I think that someone's calling this in reverse, or something?
#+bugjs20051215
(defun relation-satisfier (fun srt cc const var subst) 
  ;; Procedural attachment for a binary relation with one constant and
  ;; one variable argument.  Unifies the variable with the value of
  ;; fun applied to the constant.

  ;; Declares those values to be of sort srt.
  (dereference 
   const subst
   :if-constant
   ;; We expect a constant second argument
   ;; (otherwise fail)
   (mapc 
    (lambda (item)
      (let* ((item 
	      (declare-f2s item srt))) 
	(unify cc var item subst)))
    (remove nil	      ; KKK This should be done in some better way KKK
	    (funcall fun (make-frame const))))
   ;; unify var with const
   ;; all the items on 
   ;; (fun #$const))
   ))

(print "... biosnark add-ons (v22) loaded okay!")
