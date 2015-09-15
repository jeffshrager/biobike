(in-package :com.gigamonkeys.asdf-extensions)

(defun load-directory ()
  (when *load-truename* 
    (make-pathname :directory (pathname-directory *load-truename*))))

(defun register (dir)
  "Register a directory that contains one or more .asd files."
  (let ((*default-pathname-defaults* (or (load-directory) *default-pathname-defaults*)))
    (pushnew (merge-pathnames dir) *central-registry* :test #'equalp)))

(defun include (dir)
  "Include an .asdf file in the named directory."
  (let ((*default-pathname-defaults* (load-directory)))
    (load (merge-pathnames (merge-pathnames (parse-namestring ".asdf") dir)))))

(defun register-and-include (dir)
  "REGISTER and INCLUDE the given directory."
  (register dir)
  (include dir))

