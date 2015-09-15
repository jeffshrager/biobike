(in-package :com.gigamonkeys.ajax)

;; TODO: deal with fact that server sends the response headers (with a
;; 200 status) first but can subsequentry time out an send an HTTP 500
;; body which the client sees as just some xml. Maybe just make the
;; timeout very long (infinite).

(defvar *channel* nil)
(defvar *channels-lock* (mp:make-process-lock))
(defvar *newest-channel* nil)
(defvar *channels* (make-hash-table :test #'equal))
(defvar *timer* (make-timer))
(defvar *generators* (make-hash-table :test #'equal))
(defvar *debug-channel-ops* nil)

(defparameter *channel-ttl-seconds* 300)

;; if this is changed, URL needs to be changed in http.ls too.
(defparameter *ajax-url-prefix* "/ajax/") 
(defparameter *ajax-source-directory*
  #.(make-pathname
     :directory
     (pathname-directory (or *compile-file-truename* *load-truename*))))

(defvar *ajax-directories* (list *ajax-source-directory*))

;; if you recompile this file you get screwed if the hash table doesn't get
;; filled in again because you don't restart the ajax server...
;; this is an attempt to make everything continue to work if you recompile/
;; reload this file without bringing down the ajax server and bringing
;; it back up again. -- JP
;; (defparameter *can-be-generated-from* (make-hash-table :test #'equal))
(defvar *can-be-generated-from* (make-hash-table :test #'equal))
(defparameter *check-source-timestamps* t)
(defparameter *regenerate-if-newer* t)

(defun start-ajax-server 
       (&optional
        (url-prefix *ajax-url-prefix*) 
        (ajax-directories *ajax-directories*))
  (unless (wserver-socket *wserver*) (start :port 2002))
  (publish-ajax-urls url-prefix ajax-directories)
  (schedule-event 
   *timer* 'discard-stale-channels :in *channel-ttl-seconds* :repeating t))

(defun stop-ajax-server ()
  (when *wserver* (shutdown))
  (shutdown-timer *timer*))

;;; Messaging

(defun ajax-message-handler (request entity)
  (maintain-thread-headroom 2)
  (with-http-response (request entity :content-type "text/xml")
    (let ((channel (setf *channel* (intern-channel (request-query-value "id" request)))))
      ;;(format t "Got request in ajax-message-handler from channel: ~a~%" (id channel))
      (touch channel)
      (give-messages-to-channel (first (net.xml.sax:parse-to-lxml (get-request-body request) :normalize t)) channel)
      (let ((messages (wait-for-messages-or-next-request (transport channel))))
	(with-http-body (request entity)
	  (with-html-output ((request-reply-stream request))
	    (xml (:messages (dolist (m messages) (emit-xml m))))))))))

(defun intern-channel (id)
  (mp:with-process-lock (*channels-lock*)
    (multiple-value-bind (channel found-p) (gethash id *channels*)
      (unless found-p
	(setf *newest-channel*
	      (setf (gethash id *channels*) 
		    (setf channel 
			  (make-instance 'channel
			    :id id
			    :on-message 'message-dispatcher #+(or)'ping #+(or)'shared-repl-on-message
			    :transport (make-instance 'queue-transport)
			    :timer *timer*)))))
      channel)))

(defun discard-channel (channel)
  (format t "Discarding channel ~a~%" channel)
  (setf (dead-p channel) t)
  (mp:with-process-lock (*channels-lock*)
    (remhash (id channel) *channels*)))

(defun discard-stale-channels ()
  (when *debug-channel-ops* (format t "Reaping old channels.~%"))
  (mp:with-process-lock (*channels-lock*)
    (loop for channel being the hash-values of *channels*
       when (stale-p channel) do (discard-channel channel))))

(defun discard-all-channels ()
  (mp:with-process-lock (*channels-lock*)
    (loop for channel being the hash-values of *channels* do
	 (discard-channel channel))))

(defun stale-p (channel)
  (and (> (age channel) *channel-ttl-seconds*)
       (not (pending-request-p channel))))

(defun list-all-channels ()
  (mp:with-process-lock (*channels-lock*)
    (loop for channel being the hash-values of *channels*
       do (format t "channel ~a: age: ~a; stale-p: ~a; dead-p: ~a (~a)~%"
		  (id channel)
		  (age channel)
		  (stale-p channel)
		  (dead-p channel)
		  (pending-request-p channel)))))

(defun test-all-channels ()
  (mp:with-process-lock (*channels-lock*)
    (loop for channel being the hash-values of *channels*
       do (test-channel channel))))

(defun wait-for-messages-or-next-request (transport)
  (with-slots (messages gates) transport
    (flet ((wait-turn ()
	     (let ((gate (mp:make-gate nil)))
	       (enqueue gates gate)
	       (mp:process-wait "Waiting on gate in transport" #'mp:gate-open-p gate))))
      (cond
	((not (empty-p gates))
	 (mp:open-gate (dequeue gates))
	 (wait-turn))
	((empty-p messages)
	 (wait-turn)))
      (dequeue-all messages))))

(defun give-messages-to-channel (messages channel)
  (destructuring-bind (messages-tag &rest messages) messages
    (unless (eql messages-tag :messages)
      (warn "Expected :messages; got ~s" messages-tag))
    (dolist (m messages) 
      (receive channel m))))

;;; Lispscript and FML

(defun default-handler (request entity)
  (loop for directory in *ajax-directories* do
        (let* ((path (path-info request))
               (content-type (mime-type-for-path path))
               (file (merge-pathnames path directory))
               (file-exists? (file-exists-p file))
               (source-extensions
                (gethash (pathname-type path) *can-be-generated-from*))
               (source-files
                (mapcar
                 #'(lambda (ext) (make-pathname :type ext :defaults file))
                 source-extensions))
               (generate-from 
                (find-if (exists-and-newer-than file) source-files)))
          (when (and generate-from 
                     (or (not file-exists?) *regenerate-if-newer*))
            ;; JP hacking
            (handler-case 
                (progn
                  (format t "Generating ~A ~%" file)
                  (generate file generate-from))
              (error 
               (c)
               (default-handler-file-generator-error 
                file generate-from c request entity)
               )))
          #+obsolete
          (when generate-from (generate file generate-from))
          (when (file-exists-p file)
            (with-http-response 
             (request entity :content-type content-type)
             (with-http-body 
              (request entity)
              (let ((out (request-reply-stream request)))
                (with-open-file (in file)
                  (let ((buffer
                         (make-array 4096 :element-type '(unsigned-byte 8))))
                    (loop for read = (read-sequence buffer in)
                          while (plusp read) do 
                          (write-sequence buffer out :end read)))))))
            (return-from default-handler))))
  (failed-request request))

;; JP hacking
(defun default-handler-file-generator-error
       (file generate-from c request entity)
  (with-http-response 
   (request entity)
   (with-http-body 
    (request entity)
    (net.aserve::html 
     (:big
      (:b
       :br
       (:princ-safe (format nil "Error trying to create ~A" file))
       :br
       (:princ-safe (format nil " from source file ~A" generate-from))
       :br :br
       (:princ-safe (format nil "Actual error: ~A" c))
       ))))))

(defun register-generator (in-ext out-ext generator)
  (push in-ext (gethash out-ext *can-be-generated-from*))
  (setf (gethash (cons in-ext out-ext) *generators*) generator))

(defun exists-and-newer-than (file)
  (if *check-source-timestamps*
      #'(lambda (f) 
	  (and (file-exists-p f) (file-newer-p f file)))
      #'(lambda (f) (file-exists-p f))))

(defun generate (file from)
  (funcall (gethash (cons (pathname-type from) (pathname-type file)) *generators*) from file))

(defun compile-lispscript (lispscript javascript)
  (com.gigamonkeys.foo::generate-from-file 
   com.gigamonkeys.foo.lispscript::*lispscript*
   lispscript
   javascript))

(defun compile-foo-html (fml html)
  (com.gigamonkeys.foo::generate-from-file
   com.gigamonkeys.foo.html::*html*
   fml
   html))

(defun compile-foo-css (fss css)
  (com.gigamonkeys.foo::generate-from-file
   com.gigamonkeys.foo.css::*css*
   fss
   css))

(defun compile-foo-xml (fxml xml)
  (let ((com.gigamonkeys.foo.html::*xhtml* t))
    (com.gigamonkeys.foo::generate-from-file
     com.gigamonkeys.foo.html::*html*
     fxml
     xml)))


(defun publish-ajax-urls (url-prefix ajax-directories)
  (setf *ajax-directories* ajax-directories)
  (publish 
   :path (format nil "~amessaging" url-prefix)
   :content-type "text/html"
   :function 'ajax-message-handler)
  (publish-prefix :prefix url-prefix :function 'default-handler)
  (register-generator "ls" "js" 'compile-lispscript)
  (register-generator "fml" "html" 'compile-foo-html)
  (register-generator "fss" "css" 'compile-foo-css)
  (register-generator "fxml" "xml" 'compile-foo-xml))
