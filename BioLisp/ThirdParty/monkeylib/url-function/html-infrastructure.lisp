;;
;; Copyright (c) 2005, Gigamonkeys Consulting All rights reserved.
;;

(in-package :com.gigamonkeys.url-function)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; API

(defmacro define-url-function (name (request &rest params) &body body)
  `(progn
     (define-url-function/no-publish ,name (,request ,@params) ,@body)
     (publish :path ,(format nil "/~(~a~)" name) :function ',name)))

(defmacro define-redirect (name (request &rest params) &body body)
  "Define and publish a function that issues a redirect. The body
  of the function should is evaluated in an implicit PROGN and
  its return value should be the url to redirect to."
  `(progn
     (define-redirect/no-publish ,name (,request ,@params) ,@body)
     (publish :path ,(format nil "/~(~a~)" name) :function ',name)))

(defmacro define-url-function/no-publish (name (request &rest params) &body body)
  (multiple-value-bind (options body) (parse-options body)
    (let ((content-type (or (cadr (assoc :content-type options)) "text/html")))
      (with-gensyms (entity)
	(let ((params (mapcar #'normalize-param params)))
	  `(defun ,name (,request ,entity)
	     (with-http-response (,request ,entity :content-type ,content-type)
	       (let* (,@(param-bindings name request params))
		 ,@(set-cookies-code name request params)
		 (with-http-body (,request ,entity)
		   (with-html-output ((request-reply-stream ,request))
		     (html ,@body)))))))))))

(defmacro define-redirect/no-publish (name (request &rest params) &body body)
  (with-gensyms (entity)
    (let ((params (mapcar #'normalize-param params)))
      `(defun ,name (,request ,entity)
	 (with-http-response (,request ,entity :response *response-temporary-redirect*)
	   (let* (,@(param-bindings name request params))
	     ,@(set-cookies-code name request params)
	     (setf (reply-header-slot-value ,request :location) (progn ,@body))
	     (with-http-body (,request ,entity))))))))

(defun parse-options (body)
  (loop for (item . rest) on body
     when (and (consp item) (keywordp (car item)))
     collect item into options
     else return (values options (cons item rest))))
  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Compiler code

(defun normalize-param (param)
  (etypecase param
    (list param)
    (symbol `(,param string nil nil))))

(defun param-bindings (function-name request params)
  (loop for param in params
     collect (param-binding function-name request param)))

(defun param-binding (function-name request param)
  (destructuring-bind (name type &optional default sticky) param
    (let ((query-name (symbol->query-name name))
          (cookie-name (symbol->cookie-name function-name name sticky)))
      `(,name (or 
               (string->type ',type (request-query-value ,query-name ,request))
               ,@(if cookie-name
                     (list `(string->type ',type (get-cookie-value ,request ,cookie-name))))
               ,default)))))

(defun symbol->query-name (sym)
  (string-downcase sym))

(defun symbol->cookie-name (function-name sym sticky)
  (let ((package-name (package-name (symbol-package function-name))))
    (when sticky
      (ecase sticky
        (:global
         (string-downcase sym))
        (:package
         (format nil "~(~a:~a~)" package-name sym))
        (:local 
         (format nil "~(~a:~a:~a~)" package-name function-name sym))))))

(defun set-cookies-code (function-name request params)
  (loop for param in params
       when (set-cookie-code function-name request param) collect it))

(defun set-cookie-code (function-name request param)
  (destructuring-bind (name type &optional default sticky) param
    (declare (ignore type default))
    (if sticky
      `(when ,name 
         (set-cookie-header 
          ,request
          :name ,(symbol->cookie-name function-name name sticky)
          :value (princ-to-string ,name))))))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Runtime

(defgeneric string->type (type value))

(defmethod string->type ((type (eql 'string)) value)
  (and (plusp (length value)) value))

(defun get-cookie-value (request name)
  (cdr (assoc name (get-cookie-values request) :test #'string=)))



