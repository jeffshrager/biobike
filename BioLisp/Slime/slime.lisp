(in-package :wb)

(defvar *slime-server-port* nil)

(defun start-biobike-slime-server (&optional (port 0))
  "Create a long-lived SWANK server on PORT. Defaults to a random available port."
  (when *slime-server-port*
    (warn "*slime-server-port* already set to ~d" *slime-server-port*))
  (setf *slime-server-port* (swank:create-server :port port :dont-close t)))
