(in-package :cl-user)

(defparameter *this-directory*
  (make-pathname :name nil :type nil :defaults *load-truename*))

(push (merge-pathnames #p"Ajax/" *this-directory*) 
      com.gigamonkeys.ajax::*ajax-directories*)

(let ((vpl-version-dir 
       (merge-pathnames 
        (ecase cl-user::*vpl-version*
          (2 #p"vpl/")
          )
        *this-directory*
        )))
  (wb::log-system-event "VPL directory being used: ~A" vpl-version-dir)
  (push vpl-version-dir com.gigamonkeys.ajax::*ajax-directories*)
  )

(wb::log-system-event "Ajax server starting...")
(com.gigamonkeys.ajax::start-ajax-server)
(wb::log-system-event "Ajax server started.")



