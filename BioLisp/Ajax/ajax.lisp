(in-package :wb)

(defun start-biobike-ajax-server (&optional (directory cl-user::*ajax-directory*))
  (pushnew directory com.gigamonkeys.ajax::*ajax-directories* :test #'equal)
  (com.gigamonkeys.ajax::start-ajax-server))
