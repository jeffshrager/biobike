;;; -*- mode: Lisp; Syntax: Common-Lisp; Package: nvpl; -*-

(in-package :vpl)

;;; Author: Mark Slupesky

;;; Finds error messages in all the log files in a given directory
;;; and writes them out to a file in the same directory called vpl-errors.txt.

(defun extract-all-vpl-errors (usersdir &optional (which-year t))
      (loop for thing in (directory usersdir)
        do
        (handler-case
            (let* ((p (pathname thing))
                   (pd (pathname-directory (utils::s+ p "/")))
                   (namestring (utils::lastelem pd)))
              (print namestring)
              (vpl::extract-vpl-log-errors-from-user 
               usersdir namestring which-year))
          (error () nil)
          )))
              
(defvar *current-log-file* nil)

(defun extract-vpl-log-errors-from-user (usersdir user &optional (which-year t))
  (extract-vpl-log-errors-from-directory 
   usersdir user (wb::user-logs-directory user) which-year))

(defun extract-vpl-log-errors-from-directory 
       (usersdir user dir &optional (which-year t) (outfile "vpl-errors.txt"))
  (let ((filepath (merge-pathnames outfile dir)))
    (with-open-file (out filepath :direction :output :if-exists :supersede)
      (formatt "~%Processing log files in directory ~A~%" (namestring dir))
      (formatt "~%Writing errors to error file ~A~%" (namestring filepath))
      (let ((files (directory-with-subdirs-in-directory-form dir)))
        (format out ";;; Errors found from logs in ~A~%~%" (namestring dir))
        ;; Get rid of anything that isn't a log file 
        (setq files 
              (remove-if
               (lambda (file) 
                 (or (pathname-names-directory? file) 
                     (null (is-log-file? file))))
               files
               ))
        ;; Sort the files by write date most recent first
        (setq files (sort files '> :key 'file-write-date))
        (loop for file in files
              do
              (multiple-value-bind (a b c d e year g h i)
                  (decode-universal-time (file-write-date file))
                (declare (ignore a b c d e g h i))
                (when (or (eq t which-year) (= which-year year))
                  (formatt "  ~A...~%" (file-namestring file))
                  (let ((errors (extract-vpl-errors-from-log file)))
                    (when errors 
                      (let* ((destination-dir usersdir)
                             (destination-file 
                              (utils::s+ destination-dir "stupidusers.txt"))
                             (stupidusername user))
                        (with-open-file 
                            (usernamefile
                             destination-file
                             :direction :output
                             :if-does-not-exist :create
                             :if-exists :append)
                          (format usernamefile stupidusername)
                          (format usernamefile "~%")
                          ))
                      (format out "~%;; Errors found in log file ~A~%~%"
                              (file-namestring file)
                              ))
                    (loop for error in errors do 
                          (loop for line in error do 
                                (format out "~A~%" line))
                          (format out ";;; ----------~%")
                          )))))))))

(defun extract-vpl-errors-from-log (log-file)
  (let* ((lines (coerce (file-to-string-list log-file) 'vector))
         (nlines (length lines))
         (line-count 0)
         (errors nil)
         (*current-log-file* log-file))
    (loop until (>= line-count nlines)
          do
          (vif (error-type (line-contains-vpl-error? (aref lines line-count)))
               (multiple-value-bind (error-lines new-line-count)
                   (funcall 
                    (ecase error-type
                      (:vpl-interface-error 'extract-vpl-interface-error)
                      (:evaluation-error 'extract-evaluation-error)
                      )
                    lines line-count)
                 (push error-lines errors)
                 (setq line-count new-line-count)
                 )
               (incf line-count)
               ))
    (reverse errors)
    ))

(defun extract-vpl-interface-error (lines line-count)
  (block exit
    (loop for j from (1+ line-count) below (length lines) do
          (let ((line (aref lines j)))
            (when (log-line? line)
              (return-from exit 
                (values
                 (coerce (subseq lines line-count j) 'list)
                 j
                 )))))
    (return-from exit 
      (values 
       (coerce (subseq lines line-count) 'list)
       (length lines)
       ))))

(defun extract-evaluation-error (lines line-count)
  (let ((uplines 
         (block exit
           (loop for j from (1- line-count) downto 0 do
                 (when (search ": Form: " (aref lines j) :test 'string-equal)
                   (return-from exit 
                     (coerce (subseq lines j line-count) 'list)
                     )))
           (error "No form found above evaluation error in ~A!!"
                  *current-log-file*
                  ))))
    (multiple-value-bind (downlines next-line-count)
        (extract-vpl-interface-error lines line-count)
      (values (append uplines downlines) next-line-count)
      )))
            
(defun line-contains-vpl-error? (line)
  (cond
   ((search "Error message: " line :test 'string-equal) :evaluation-error)
   ((search "Vpl interface error:" line :test 'string-equal)
    :vpl-interface-error)
   (t nil)
   ))

(defun log-line? (line)
  (and (> (length line) 8)
       (digit-char-p (aref line 0))
       (digit-char-p (aref line 1))
       (char= (aref line 2) #\/)
       ))

(defun is-log-file? (pathname) (string-equal "log" (pathname-type pathname)))
