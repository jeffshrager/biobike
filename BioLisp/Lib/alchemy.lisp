;; -*- mode: Lisp; Syntax: Common-Lisp; Package: biolisp; -*-

(in-package :bio)

;;; +=========================================================================+
;;; | Copyright (c) 2006 The BioBike Team                                     |
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

(defun run-alchemy (mln db cmd)
  (with-temp-file-in (mln-path *tmp-directory* :type "mln")
    (with-temp-file-in (db-path *tmp-directory* :type "db")
      (with-temp-file-in (res-path *tmp-directory* :type "res")
	(with-temp-file-in (out-path *tmp-directory* :type "out")
	  (with-open-file (o mln-path :direction :output)
	    (loop for line in mln do (format o "~a~%" (lisp2alchemy line))))
	  (with-open-file (o db-path :direction :output)
	    (loop for line in db do (format o "~a~%" (lisp2alchemy line))))
          (let ((alchemy-command 
                 (formatn 
                  (one-string
                   "/usr/local/biotools/alchemy/current/bin/~a "
                   "-i ~a -e ~a -r ~a > ~a")
                  cmd mln-path db-path res-path out-path)))                  
            (case (protected-shell-command alchemy-command)
              (:timeout nil)
              (otherwise             
               `((:result
                  ,(mapcar #'alchemy2lisp (file-to-string-list res-path)))
                 (:output ,(file-to-string-list out-path)))))))))))

(defun lisp2alchemy (expr)
  #.(one-string-nl
     "Convert exprs in lispy prefix syntax to alchemical infix syntax."
     "Examples: "
     "  (1.0 (:implies (:and (grass_wet d) (sprinklers_off d)) (raining d)))"
     "  Becomes: 1.0 (grass_wet(d) ^ sprinklers_off(d)) => raining(d)"
     "  (1.0 (:implies (:and (grass_wet d) (sprinklers_off d)) (raining d)))"
     "  Becomes: 1.0 (grass_wet(d) ^ sprinklers_off(d)) => raining(d)"
     "Note that you'll have to use quoted strings in order to create literals, which must be"
     "capitalized in alchemy -- it's your responsibility to do that. So, for example:"
     "  (:set day \"Monday\" \"Tuesday\" \"Wednesday\")"
     "  Becomes: day = {Monday, Tuesday, Wednesday}" 
     )
  (cond ((stringp expr) expr)
	((numberp expr) (format nil "~a" expr))
	((atom expr) (string-downcase (string expr)))
	((not (listp expr))
	 (error "LISP2ALCHEMY got ~s which it can't understand." expr))
	((numberp (car expr))
	 (format nil "~a ~a" 
		 (lisp2alchemy (car expr))
		 (lisp2alchemy (cadr expr))))
	(t 
	 (case (car expr)
	   (:not
	    (format nil "!~a"
		    (lisp2alchemy (second expr))))
	   (:set 
	    (format nil "~a = {~a}"
		    (lisp2alchemy (second expr))
		    (loop with res = (string (caddr expr))
			as next in (cdddr expr)
			do (setq res (format nil "~a,~a" res next))
			finally (return res))))
	   ((:and :or)
	    (format nil "(~a)"
		    (loop with res = (lisp2alchemy (second expr))
			as clause in (cddr expr)
			do (setf res (format nil "~a ~a ~a"
					     res
					     (cadr (assoc (car expr) '((:and "^") (:or "||"))))
					     (lisp2alchemy clause)))
			finally (return res))))
	   (:implies
	    (format nil "~a => ~a"
		    (lisp2alchemy (second expr))
		    (lisp2alchemy (third expr))))
	   (t 
	    (format nil "~a(~a)"
		    (lisp2alchemy (first expr))
		    (loop with res = (lisp2alchemy (cadr expr))
			as next in (cddr expr)
			do (setq res (format nil "~a,~a" res (lisp2alchemy next)))
			finally (return res))))
	   ))))

(defun alchemy2lisp (expr)
  #.(one-string-nl
     "Convert expressions in Alchemy (infix) syntax into Lispy exprs."
     "This only handles expressions of the form: function(Value,Value,Value...) weight"
     "As: \"raining(Tuesday) 0\" => (0.0 (raining Tuesday))"
     "Exprs beginning in / are just returned as-is (comments begin with //)"
     )
  (cond ((char-equal #\/ (elt expr 0)) expr)
	(t (let* ((open-paren-pos (position #\( expr))
		  (close-paren-pos (position #\) expr))
		  (function (read-from-string (subseq expr 0 open-paren-pos)))
		  (args (string-split (subseq expr (1+ open-paren-pos) close-paren-pos) #\,))
		  (n (read-from-string (subseq expr (1+ close-paren-pos)))))
	     `(,n (,function ,@args))))))

(defun altest1 ()
  (run-alchemy 
   '(
     (:set day "Monday" "Tuesday" "Wednesday")
     (raining day)
     (grass_wet day)
     (sprinklers_off day)
     (temp_65 day)
     (humidity_100 day)
     (1.0 (:implies (:and (grass_wet d) (sprinklers_off d))
	   (raining d)))
     (1.0 (:implies (:and (temp_65 d) (humidity_100 d))
	   (raining d)))
     (0.5 (:implies (raining d) (:and (grass_wet d) (sprinklers_off d))))
     (0.5 (:implies (raining d) (:and (temp_65 d) (humidity_100 d))))
     (0.5 (:not (raining d)))
     (0.5 (:not (grass_wet d)))
     (0.5 (:not (temp_65 d)))
     (0.5 (:not (humidity_100 d)))
     (0.5 (:not (sprinklers_off d)))
     )
   '(
     (temp_65 "Monday")
     (raining "Monday"))
   "infer -p -mcmcMaxSteps 2000 -q 'raining,grass_wet,temp_65'"))

(defun altest2 (&optional (n 10))
  (flet ((randp (expr)
	   (list (/ (random 100) 100.0)
		 expr)))
    (loop for i from 1 to n
	as model = (list 
		    '(:set day "Monday" "Tuesday" "Wednesday")
		    '(raining day)
		    '(grass_wet day)
		    '(sprinklers_off day)
		    '(temp_65 day)
		    '(humidity_100 day)
		    (randp '(:implies (:and (grass_wet d) (sprinklers_off d))
				     (raining d)))
		    (randp '(:implies (:and (temp_65 d) (humidity_100 d))
				     (raining d)))
		    (randp '(:implies (raining d) (:and (grass_wet d) (sprinklers_off d))))
		    (randp '(:implies (raining d) (:and (temp_65 d) (humidity_100 d))))
		    (randp '(:not (raining d)))
		    (randp '(:not (grass_wet d)))
		    (randp '(:not (temp_65 d)))
		    (randp '(:not (humidity_100 d)))
		    (randp '(:not (sprinklers_off d)))
		    )
	collect `((:model ,model)
		  ,(assoc :result
			  (run-alchemy 
			   model 
			   '(
			     (temp_65 "Monday")
			     (raining "Monday"))
			   "infer -p -mcmcMaxSteps 200 -q 'raining,grass_wet,temp_65'"))))))

#+test 
(defun store-facts (&key fact pid)
  (with-open-file (....)
    ...
    ))

#+test
(defun alchemasp1 ()
  (create-aserve-form-pages
   'altest3
   :page-title "RacFacts"
   :arguments
   '((:text :fact :title "Fact:" :data-type :string)
     (:text :pid :title "PID:" :data-type :string)
     )))
