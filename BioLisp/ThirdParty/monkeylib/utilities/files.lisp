(in-package :com.gigamonkeys.utilities)

(defun file-text (pathname)
  (with-open-file (in pathname)
    (with-output-to-string (out)
      (loop for line = (read-line in nil nil)
	   while line do (write-line line out)))))