;;; To make this work c/l it into the web listener first.

(in-package :com.biobike.ajax)

(eval-when (:compile-toplevel :load-toplevel :execute)
  #+(or)(setf *block-elements* '(:list :cons :hole))
  #+(or)(setf *paragraph-elements* '(:symbol :number :nil :t :object :car :cdr))
  (setf *block-elements* '())
  (setf *paragraph-elements* '()))

(defparameter *workarea-start* 
  '((:div :id "workarea")
    (:p "Click a hole in an expression to select it or double click to edit it directly. "
     "Click on an expression to move it to a selected hole. Double click an expression to evaluate it.")))

(defvar *workarea*)

(defun ss-vpl (channel message)
  (declare (ignorable form))
  (vlog "Got VPL mesage: ~s~%" message)
  (destructuring-bind (vpl-tag verb &body body) message
    (declare (ignore vpl-tag body))
    (cond
      ((string-equal verb "hello")
       (setf *workarea* *workarea-start*)
       (send channel `(:ss-vpl (:palette 
				((:div :id "palette")
				 (:p "Click a box to add an expression to the work area below.")
				 (:span :class "palette-item" "T") " "
				 (:span :class "palette-item" "NIL") " "
				 (:span :class "palette-item" "IF") " "
				 (:span :class "palette-item" "EQL") " "
				 (:span :class "palette-item" "EQUAL") " "
				 (:span :class "palette-item" "<") " "
				 (:span :class "palette-item" "<=") " "
				 (:span :class "palette-item" ">") " "
				 (:span :class "palette-item" ">=") " "
				 (:span :class "palette-item" "=") " "
				 (:span :class "palette-item" "+") " "
				 (:span :class "palette-item" "-") " "
				 (:span :class "palette-item" "*") " "
				 (:span :class "palette-item" "/") " "
				 (:span :class "palette-item" "NOT") " "
				 (:span :class "palette-item" "RANDOM") " "
				 (:span :class "palette-item" "WHEN") " "
				 (:span :class "palette-item" "UNLESS") " "
				 (:span :class "palette-item" "AND") " "
				 (:span :class "palette-item" "OR") " "
				 (:span :class "palette-item" "COND") " "
				 (:span :class "palette-item" "LET") " "
				 (:span :class "palette-item" "LET*") " "
				 (:span :class "palette-item" "ABS") " "
				 (:span :class "palette-item" "VOID") " "))))
       (send channel `(:ss-vpl (:workarea ,*workarea*))))
      ((string-equal verb "click")
       (frob-workarea)
       (send channel `(:ss-vpl (:workarea ,*workarea*)))))))

(defun frob-workarea ()
  (if (third *workarea*)
      (setf (third *workarea*) (frob-sexp (third *workarea*)))
      (setf *workarea* (append *workarea* (list (frob-sexp nil))))))

(defun frob-sexp (sexp)
  (if (not sexp)
      '((:div :class "inserted-item")
	((:div :class "list") (:span :class "symbol" "+")))
      (destructuring-bind (div1 (div2 &rest elements)) sexp
	`(,div1 (,div2 ,@elements (:span :class "value" "10"))))))
		 
	  


(register-message-handler :ss-vpl 'ss-vpl)

