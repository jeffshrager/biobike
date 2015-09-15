;;; -*- mode: Lisp; Syntax: Common-Lisp; Package: wb; -*-

(in-package :wb)

;;; +=========================================================================+
;;; | Copyright (c) 2002-2009 JP Massar, Jeff Shrager, Mike Travers           |
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
  (export '(sum-two-numbers add-two-numbers)))
               
(defvar *web-widget-refs* nil)

(defvar *ww-page-title* nil)
(defvar *ww-results-label* nil )
(defvar *ww-results-display-mode* nil)

(defvar *ww-funcallable-function* nil)
(defvar *ww-function-arguments* nil)

(defparameter *default-cfg-time-limit* 600)

(defstruct web-widget-ref path exec-path display-string documentation)

(defmethod out-record-to-html
           ((obj web-widget-ref) (string string) &key (pkg nil))
  (declare (ignore ignore))
  (princ-with-frame-links string :pkg pkg)
  (html 
   :br
   ((:a :href (web-widget-ref-path obj)) 
    (:princ-safe (web-widget-ref-display-string obj)))))

(defmacro create-vpl-web-service 
          ((&rest keyword-argument-names) 
           function-name-or-body 
           &rest keys-and-values
           )
  (let* ((body? (not (symbolp function-name-or-body)))
         (fname 
          (when body? 
            (intern (string (gensym "VPL-WS-FUNCTION-")) wb::*username*)
            ))
         (body-code 
          (when body? 
            `(lisp:defun ,fname (&key ,@keyword-argument-names)
               ,function-name-or-body
               )))
         (identifier-pos 
          (position :service-name keys-and-values :test 'symbol=))
         (identifier-value
          (when identifier-pos (nth (1+ identifier-pos) keys-and-values)))
         (service-class-pos 
          (position :service-class keys-and-values :test 'symbol=))
         (service-class
          (when service-class-pos (nth (1+ service-class-pos) keys-and-values))
          ))
    `(progn
       ,@(when body? (list body-code))
       (create-aserve-form-pages 
        ',fname
        :arguments 
        ',(loop for name in keyword-argument-names 
                for n from 1
                collect
                (list :text (keywordize name) :title (formatn "Argument ~D" n))
                )
        :result-display-mode :raw
        :verbose? nil
        :for-vpl? t
        ,@(when identifier-value
            `(:page-title (s+ wb::*username* "-" ',identifier-value)))
        ,@(when service-class 
            `(:service ,(keywordize service-class)))
        ))))

;; (create-vpl-web-service (a b) (+ a b) :service-name test :service-class collabrx)



(defgeneric service-webwidget-html-function (service output-object)
  (:documentation
   #.(one-string-nl
      "Creates HTML to send back to the client once the FUNCTION"
      "specified in CREATE-ASERVE-FORM-PAGES has been called."
      )))

(defgeneric service-webwidget-error-function (error service)
  (:documentation
   #.(one-string-nl
      "Creates HTML to send back to the client once the FUNCTION"
      "specified in CREATE-ASERVE-FORM-PAGES has been called."
      )))

(defmethod service-webwidget-html-function ((service t) (output-object t))
  (form-function-evaluation-results-message output-object))

(defmethod service-webwidget-html-function
           ((service (eql :test)) (output-object t))
  (html
   (:h3 (:princ-safe "This is a test!"))
   (:h3 (:princ-safe "Results:"))
   (:princ-safe (formatn "~S" output-object))
   ))

(defmethod service-webwidget-html-function
           ((service (eql :collabrx)) (output-object wb::jpg))
  (html 
   (:princ
    (format 
     nil
     "<html> <a href=\"~A\"> <img src=\"~A\"> </a> </html>"
     (wb::jpg-click-path output-object) (wb::jpg-path output-object)
     )))
  #+doesnotwork
  (html
   ((:a :href (wb::jpg-click-path output-object))
    (:img :src (wb::jpg-path output-object))
    )))
    


(defmethod service-webwidget-error-function ((error t) (service t))
  (form-function-evaluation-error-message
   *ww-page-title* error *ww-funcallable-function* *ww-function-arguments*
   ))

(defmethod service-webwidget-error-function ((error t) (service (eql :test)))
  (html
   (:h3 (:princ-safe "This is a test of the error mechanism!"))
   (:h3 (:princ-safe "Actual error:"))
   (:princ-safe (formatn "~A" error))
   ))


;; (create-vpl-web-service (+ a b) a b)

(defun create-aserve-form-pages

       (function
        &rest
        args 
        &key
        (page-title nil)
        (name nil)
        (documentation "")
        (page-model nil)
        (time-limit *default-cfg-time-limit*)
        (arguments '((:text-field :arg1 :title "Argument 1")))
        ;; A string or NIL
        (execute-button-title "Go")
        ;; A string, T or NIL
        (reset-button-title nil)
        (results-label "Result: ")
        (dry-run? nil)
        (verbose? t)
	(result-display-mode :safe)
        (for-vpl? nil)
        (service t)
        &allow-other-keys
        &aux 
        (page-name page-title)
        form-page-name exec-page-name
        form-function-name exec-function-name
        widget-name form-name funcallable-form 
        form-page-function
        exec-page-function
        )
  (declare (ignore args))

  (unless (eq service t)
    (setq service (keywordize service)))
  (unless page-title 
    (setq page-title (string (gensym "Biolingua-Tool-"))))
  (setq page-name (substitute #\- #\Space page-name))
  (setq form-page-name (one-string "/" page-name "-form.html"))
  (setq exec-page-name (one-string "/" page-name "-exec.html"))
  (setq widget-name (or name page-name))
  (setq form-function-name (gensym "BIOLINGUA-FORM-FUNCTION-"))
  (setq exec-function-name (gensym "BIOLINGUA-EXEC-FUNCTION-"))
  (setq form-name (string (gensym "Biolingua-form-")))
  (setq funcallable-form
        (cond
         ((symbolp function) (list 'quote function))
         ((or (functionp function)
              (and (listp function) 
                   (or (eq (first function) 'lambda)
                       (eq (first function) 'function))))
          function)
         (t (error "Unknown function form: ~A" function))
         ))

  (unless page-model
    (setq 
     form-page-function
     `(lambda (req ent)
        (with-http-response-and-body (req ent)
          (html
           (:h2 (:princ-safe ,page-title))
           :br
           ((:form 
             :name ,form-name 
             :method "GET"
             :action ,exec-page-name)
            (:table
             ,@(loop for argd in arguments for argnum from 1 collect
                     (generate-html-for-form-argument-input argd argnum)
                     ))
            ,@(when execute-button-title
                `(((:input :type "SUBMIT" 
                    :value ,execute-button-title
                    ))))
            ,@(when reset-button-title
                `(((:input :type "RESET"
                    ,@(when (not (eq t reset-button-title))
                        `(:value ,reset-button-title)
                        )))))
            ))))))

  (setq
   exec-page-function
   `(lambda (req ent)
      (with-http-response-and-body (req ent)
        (block exit
          ;; Pull the arguments out of the URL
          (let* ((input (request-query req)) 
                 (*ww-page-title* ,page-title)
                 (*ww-results-label* ,results-label)
                 (*ww-results-display-mode* ,result-display-mode)
                 (type nil)
                 (name nil)
                 (arguments 
                  ;; Trap problems with the argument values.
                  (handler-case
                      (nconc
                       ,@(loop for argd in arguments collect
                               (extract-form-argument-code 
                                argd 'input 'type 'name)))
                    (error
                     (c)
                     (form-argument-extraction-error-message 
                      *ww-page-title* c type name)
                     (return-from exit nil)
                     )))
                 (*ww-funcallable-function* ,funcallable-form)
                 (*ww-function-arguments* arguments))
            ;; Call the function associated with the form, trapping problems
            ;; with the execution.
            (handler-case
                (let ((results 
                       (with-timeout-limit 
                           (,time-limit 
                            (form-function-evaluation-timeout-message
                             *ww-page-title* ,funcallable-form arguments)
                            (return-from exit nil))
                         ,(if for-vpl? 
                              `(,(intern :with-bbl-form :bbi)
                                (apply ,funcallable-form arguments))
                            `(apply ,funcallable-form arguments)
                            ))))
                  ;; Call a method potentially discriminating on
                  ;; service type and the result type to generate
                  ;; HTML 
                  (service-webwidget-html-function ,service results)
                  )
              (error
               (c)
               (service-webwidget-error-function c ,service)
               (return-from exit nil)
               )))))))

  (when (or dry-run? verbose?)
    (terpri)
    (cformatt "Form page function:")
    (if page-model
        (cformatt "*** Using HTML from file ~A~%" page-model)
      (pprint form-page-function))
    (terpri)
    (cformatt "Exec page function:")
    (pprint exec-page-function)
    (terpri)
    (terpri)
    )
    
  (when (not dry-run?)

    (cond
     (page-model
      (when verbose?
        (cformatt 
         "Publishing ~A (as alias for ~A)" 
         form-page-name (translate-logical-pathname page-model)))
      (publish-file
       :path form-page-name
       :file (translate-logical-pathname page-model)
       :content-type cl-user::*html-publish-content-type*
       ))
     (t
      (when verbose? (cformatt "Publishing ~A" form-page-name))
      (publish
       :path form-page-name
       :content-type cl-user::*html-publish-content-type*
       :function 
       (symbol-function 
        (compile form-function-name (eval form-page-function))
        ))))

    (when verbose? (cformatt "Publishing ~A" exec-page-name))
    (publish
     :path exec-page-name
     :content-type cl-user::*html-publish-content-type*
     :function 
     (symbol-function
      (compile exec-function-name (eval exec-page-function))
      )))

  (let ((widget
         (make-web-widget-ref
          :path form-page-name 
          :exec-path exec-page-name
          :display-string widget-name
          :documentation documentation
          )))
    (setq *web-widget-refs*
          (delete (web-widget-ref-display-string widget) *web-widget-refs* 
                  :key 'web-widget-ref-display-string :test 'string-equal
                  ))
    (push widget *web-widget-refs*)
    (when for-vpl? 
      (formatt "URL template for execution of the new ~A service:~%~%" 
               page-name)
      (let ((url-template 
             (formatn "http://~A:~A/~A?" 
                      (ecase (cl-user::os?)
                        (:unix (cl-user::environment-variable-value "HOSTNAME"))
                        (:windows "localhost"))
                      cl-user::*weblistener-port*
                      (subseq (web-widget-ref-exec-path widget) 1)
                      )))
        (loop for arg in arguments 
              for count from 0
              as argname = (second arg)
              do
              (setq 
               url-template
               (s+ 
                url-template
                (formatn "~A~A=[[value]]" (if (zerop count) "" "&") argname)
                )))
        (formatt "~A~%~%" url-template)
        (formatt 
         "To create a complete URL and cause execution, you must substitute ~%")
        (formatt 
         "in values for the [[args]] near the end of the above URL~%~%")
        ))
    widget
    )
           
  )

(defun generate-html-for-form-argument-input (argd argnum)
  (destructuring-bind (type name . keys) argd
    (flet ((optional (key &optional default)
             (let ((val (keyval key keys)))
               (cond
                (val (list key val))
                (default (list key default))
                (t nil)
                ))))
      (let ((namestring (string name)))
        `(:tr
          (:td (:princ-safe
                ,(or (keyval :title keys) (formatn "Arg ~D:" argnum))))
          (:td
           ,@(case type
               (:text
                `(((:input :type "text" 
                    :name ,namestring
                    ,@(optional :size)
                    ,@(optional :value "")
                    ,@(optional :maxlength)
                    ))))
               (:password
                `(((:input :type "password" 
                    :name ,namestring
                    ,@(optional :size)
                    ,@(optional :value "")
                    ,@(optional :maxlength)
                    ))))
               ((:checkbox :radio)
                (form-elements-for-checkbox-or-radio
                 type name namestring (keyval :values keys)
                 ))
               (:hidden
                (let ((value (keyval :value keys)))
                  (unless value
                    (error "HIDDEN input must have a :value specified"))
                  `(((:input :type "hidden"
                      :name ,namestring :value ,value
                      )))))
               (:image 
                (error "INPUT type IMAGE not implemented"))
               (:submit 
                (error "Use the :EXECUTE-BUTTON-TITLE keyword for SUBMIT"))
               (:reset 
                (error "Use the :RESET-BUTTON-TITLE keyword for RESET"))
               (:textarea 
                (let ((rows (keyval :rows keys)) (cols (keyval :cols keys)))
                  (unless rows (error "Must provide :ROWS to TEXTAREA"))
                  (unless cols (error "Must provide :COLS to TEXTAREA"))
                  `(((:textarea :name ,namestring :rows ,rows :cols ,cols)))
                  ))
               (:select (error "SELECT not implemented"))
               (otherwise (error "Unknown form component label: ~A" type))
               )))))))

(defun form-elements-for-checkbox-or-radio (type name namestring keys)
  (let ((checkcount 0))
    (loop for value in keys 
          as triple =
          (cond
           ((atom value) 
            (setq value (list value nil value)))
           ((= 1 (length value)) 
            (setq value (list (first value) nil (first value))))
           ((= 2 (length value)) 
            (setq value (append value (list (first value)))))
           ((> (length value) 3)
            (error "Invalid VALUES triple, ~A, in CHECKBOX ~A"
                   value name)))
          collect
          (progn
            (ecase type
              (:checkbox nil)
              (:radio
               (when (second triple) (incf checkcount))
               (when (> checkcount 1)
                 (error 
                  "At most one RADIO box may be checked!"))))
            (destructuring-bind 
                (value-string checked? label-string) 
                triple
              `((:input :type ,(string-downcase (string type))
                 :name ,namestring
                 :value ,(string value-string)
                 ,@(when checked? (list :checked "checked"))
                 )
                (:princ-safe ,(string label-string)))
              )))))

(defun form-string-to-lisp-type (string &optional (type :string))
  (with-standard-io-syntax
    (let ((*read-eval* nil) (*readtable* (frames-readtable)))
      (flet ((maybe-oops (string test type-string)
               (cond
                ((zerop (length string))
                 (error "Empty string instead of a valid ~A" type-string))
                (t
                 (let ((result (read-from-string string)))
                   (unless (funcall test result)
                     (error "The input ~S does not represent a valid ~A" 
                            string type-string
                            ))
                   result
                   )))))
        (ecase type
          (:string string)
          ((:fixnum :integer) 
           (maybe-oops string 'integerp "integer"))
          ((:float :single-float :double-float)
           (float 
            (maybe-oops string 'realp "float")
            (ecase type
              ((:float :single-float) 0.0)
              ((:double :double-float) 0.0d0)
              )))
          ((:t t :lisp-object) (maybe-oops string 'identity "lisp object"))
          (:symbol (maybe-oops string 'symbolp "symbol"))
          (:frame (maybe-oops string 'isframe? "Frame object"))
          (:list (maybe-oops string 'listp "list"))
          )))))

(defun extract-form-argument-code 
       (argd input-symbol type-symbol name-symbol)
  (destructuring-bind (type name . keys) argd
    (let* ((keyname (keywordize name))
           (extraction-code
            (block exit
              `(progn
                 (setq ,type-symbol ,type ,name-symbol ,keyname)
                 ,(case type
                    ;; Any kind of value; gets converted from string
                    (:text
                     `(form-string-to-lisp-type 
                       (url-parameter-value ,(string keyname) ,input-symbol)
                       ,(keyval :data-type keys :string)))
                    ;; List of values of items checked
                    (:checkbox
                     `(url-parameter-values ,(string keyname) ,input-symbol))
                    ;; Always string values
                    ((:password :radio :hidden :textarea :select)
                     `(url-parameter-value ,(string keyname) ,input-symbol))
                    (t (return-from exit nil))
                    )))))
      (and extraction-code `(list ,keyname ,extraction-code))
      )))

(defun form-argument-extraction-error-message (title c type name)
  (html
   (:h2 (:princ-safe title))
   (:b (:big "RUH ROH!") )
   (:pre
    (:princ-safe "Error obtaining argument value.")
    :newline
    (:princ-safe (formatn "  Input element type: ~A" type))
    :newline
    (:princ-safe (formatn "  Input element name: ~A" name))
    :newline
    :newline
    (:b (:princ-safe (formatn "Specific problem is: ~A" c)))
    )))

(defun form-function-evaluation-message (type page-title c function arguments)
  (html
   (:h2 (:princ-safe page-title))
   (:b (:big "RUH ROH!"))
   (:pre
    (:princ-safe
     (formatn 
      (one-string-nl
       "~A executing form-designated function ~A"
       "  Called with arguments: ")
      (ecase type (:error "Error") (:timeout "Timeout"))
      function))
    (loop for (key value) on arguments by #'cddr do 
          (html (:princ-safe "    " (formatn "~A: ~S~%" key value))))
   :newline
   (ecase type
     (:error (html (:princ-safe (formatn "Actual error condition: ~A" c))))
     (:timeout nil)
     ))))

(defun form-function-evaluation-error-message (page-title c function arguments)
  (form-function-evaluation-message 
   :error page-title c function arguments))
(defun form-function-evaluation-timeout-message (page-title function arguments)
  (form-function-evaluation-message 
   :timeout page-title nil function arguments))

(defun form-function-evaluation-results-message (results)
  (if (eq *ww-results-display-mode* :raw)
      (html (:princ results))
    (html
;     (:h3 (:princ-safe *ww-page-title*))
;     (:h3 (:princ-safe "Results:"))
     (html (:princ results))
     #+dyked
     (case *ww-results-display-mode*
       ((:safe :princ-safe :simple)
        (html (:princ-safe (formatn "~A ~S" *ww-results-label* results))))
       ;; Trust the user to format their own results:
       ((:unsafe :html :do-not-translate :as-is)
        (html (:princ "foo" )))
       ))))

(defun keyval (key key-value-pair-list &optional (default nil))
  (let ((pos (position key key-value-pair-list)))
    (if pos (nth (1+ pos) key-value-pair-list) default)
    ))


(defun url-parameter-values (key url-alist)
  (let ((keystr (string key)))
    (mapcar 'cdr (remove-if-not (lambda (x) (equal keystr (car x))) url-alist))
    ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(publish
 :path "/web-widget-index"
 :content-type cl-user::*html-publish-content-type*
 :function 
 (lambda (req ent)
   (let* ((input (request-query req))
          (package-name (url-parameter-value :pkg input))
          (package-symbol (keywordize package-name))
          )
     (execute-with-standard-weblistener-environment
      req ent package-symbol
      (lambda () (html-for-web-widget-index))
      ))))

(defun html-for-web-widget-index ()
  (with-standard-weblistener-page-header
      ("Web Widget Directory" :listener? nil)
    (let ((widgets 
           (sort 
            (copy-list *web-widget-refs*)
            'string-lessp
            :key 'web-widget-ref-display-string
            )))
      (dolist (ww widgets)
        (html
         ((:a :href (web-widget-ref-path ww)) 
          (:princ-safe (web-widget-ref-display-string ww)))
         :br
         (:princ-safe (one-string " -- " (web-widget-ref-documentation ww)))
         :p
         )))))

;;; Some examples:

(create-aserve-form-pages
 'form-two-way-ortholog-of
 :title "Two way Ortholog with respect to an Organism"
 :page-name "two-way-ortholog"
 :time-limit 10
 :documentation 
 (one-string-nl
  "Bi-directional ortholog of gene with respect to a related but distinct"
  "organism. See documentation for TWO-WAY-ORTHOLOG-OF function.")
 :arguments
 '((:text :gene-or-protein :title "Gene or Protein" :data-type t)
   (:text :organism :title "Organism" :data-type t)
   (:text :threshold :title "Blast threshold" 
    :data-type :double-float :value "1.0e-10"))
 :execute-button-title "Find Ortholog"
 :reset-button-title t
 :results-label "Orthologous Protein: "
 :verbose? nil
 :dry-run? nil
 )

(defun form-two-way-ortholog-of
       (&key gene-or-protein organism threshold)
  (forward-package-funcall
   :biolisp 'two-way-ortholog-of gene-or-protein organism threshold))

(create-aserve-form-pages
  'add-two-numbers
  :page-title "The Amazing Addition Utility"
  :time-limit 10
  :documentation
  (one-string-nl 
   "Takes two real numbers that a user inputs, and, amazingly, "
   "adds them together to produce their sum.  Displays the sum "
   "to the user in standard arithmetic notation.  Also demonstrates"
   "other input options/features of the web widget interface.")
  :arguments 
  '((:text :arg1 :title "Number of frobs:" :data-type :float)
    (:text :arg2 :title "Number of widgets:" :data-type :integer)
    (:checkbox :arg3 :title "Result tag:" :values ((foo t) (bar nil)))
    (:radio :arg4 :title "Favorite flavor?" 
     :values ((asparagus nil) (steak t) (mango nil)))
    (:textarea :arg5 :title "Comments:" :rows 4 :cols 50)
    )
  :execute-button-title "Do it!"
  :reset-button-title t
  :results-label "Number of frobs and widgets:"
  :verbose? nil
  :dry-run? nil
  )

(create-aserve-form-pages
  'add-two-test-numbers
  :service :test 
  :page-title "The Amazing Test Addition Utility"
  :time-limit 10
  :documentation
  (one-string-nl 
   "Takes two real numbers that a user inputs, and, amazingly, "
   "adds them together to produce their sum.  Displays the sum "
   "to the user in standard arithmetic notation.  Also demonstrates"
   "other input options/features of the web widget interface.")
  :arguments 
  '((:text :arg1 :title "Number of frobs:" :data-type :float)
    (:text :arg2 :title "Number of widgets:" :data-type :integer)
    (:checkbox :arg3 :title "Result tag:" :values ((foo t) (bar nil)))
    (:radio :arg4 :title "Favorite flavor?" 
     :values ((asparagus nil) (steak t) (mango nil)))
    (:textarea :arg5 :title "Comments:" :rows 4 :cols 50)
    )
  :execute-button-title "Do it!"
  :reset-button-title t
  :results-label "Number of frobs and widgets:"
  :verbose? nil
  :dry-run? nil
  )

(create-aserve-form-pages
  'divide-two-test-numbers
  :service :test 
  :page-title "The Amazing Test Division Utility"
  :time-limit 10
  :documentation
  (one-string-nl 
   "Takes two real numbers that a user inputs, and, amazingly, "
   "divides them.  Displays the quotient "
   "to the user in standard arithmetic notation.  Also demonstrates"
   "other input options/features of the web widget interface.")
  :arguments 
  '((:text :arg1 :title "Number of frobs:" :data-type :float)
    (:text :arg2 :title "Number of widgets:" :data-type :integer)
    )
  :execute-button-title "Do it!"
  :reset-button-title t
  :results-label "Number of frobs and widgets:"
  :verbose? nil
  :dry-run? nil
  )

(defun add-two-numbers (&key (arg1 0) (arg2 0) (arg3 0) (arg4 0) (arg5 0))
  (list (list arg4 (+ arg1 arg2) arg3) arg5))

(defun add-two-test-numbers (&key (arg1 0) (arg2 0) (arg3 0) (arg4 0) (arg5 0))
  (list (list arg4 (+ arg1 arg2) arg3) arg5))

(defun divide-two-test-numbers (&key (arg1 0) (arg2 0))
  (/ arg1 arg2))

;; ============================================================

(defmacro help-me-program-this (description &optional code-block)
  `(help-me-program-this-message ',(list description code-block)))

(defun help-me-program-this-message (data)
  (let* ((description (first data))
         (code-block (second data))
         (user wb::*username*)
         (to 
          (list "massar@alum.mit.edu" "jshrager@collabrx.com" "elhaij@vcu.edu"))
         (message 
          (with-output-to-string (s)
            (format s "From system user ~A~%~%" user)
            (format s "Description: ~%~%~A~%~%" description)
            (if code-block
                (let ((*print-length* nil)
                      (*print-level* nil))
                  (format s "User provided code: ~%~%")
                  (pprint code-block s))
              (format s "No user-provided code...")
              ))))
    (cformatt "Email sent to ~A.~%~%" to)
    (print message)
    (send-file-string-as-email 
     nil message to :subject "Help me program this!"
     )))


#|

(defparameter *form-statement-types*'(:input :select :textarea))

(defparameter *form-input-field-types*
  '(
    ;; Needs additional specification as to type of value
    (:text :name &optional :maxlength :size :value)
    ;; Value is always a string
    (:password :name &optional :maxlength :size :value)
    ;; Actually a set of (value checked? label) triples with a single name
    (:checkbox :name :values &optional :checked)
    ;; Actually a set of (value checked? label) triples with at most 
    ;; one checked, with a single name
    (:radio :name :value &optional :checked)
    ;; Returns two values, name.x and name.y
    (:image :name :src)
    (:hidden :name :value)
    (:submit &optional :name :value)
    (:reset &optional :value)
    ))

;; SELECT Statement
    ;; If multiple, how are multiple values returned?
    '(:select :name &optional :multiple :size &rest options)
    ;; Value defaults to title
    '(:option :title &optional :value :selected)

;; TEXTAREA Statement
    ;; Foo
    '(:textarea :name :cols :rows &optional :value)

|#
