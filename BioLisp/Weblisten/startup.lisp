;;; -*- mode: Lisp; Syntax: Common-Lisp; Package: weblistener; -*-

(in-package :weblistener)

;;; +=========================================================================+
;;; | Copyright (c) 2002, 2003, 2004 JP Massar, Jeff Shrager, Mike Travers    |
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

;;; Authors:  JP Massar, Jeff Shrager.

(defvar *current-wserver* nil)
(defvar *overseer-thread* nil)

;;; See documentation in .../doc/process-priorities.txt 

(defparameter *highest-priority* 20
  "Priority at which toplevel aserve process and the overseer process run at")
(defparameter *initial-worker-quantum* 1.0
  "Quantum that worker threads that evaluate user forms are initially set to") 
(defparameter *continuing-worker-quantum* 0.5
  "Quantum that worker threads get set to after exceeding initial quantum")
(defparameter *overseer-wakeup-interval* 5
  "Seconds between overseer process running")

(defparameter *help-requests-filename* "vpl-help-requests.log")

  


;;;; START THE ALLEGROSERVE WEBSERVER

(defun start-weblistener (&key (port cl-user:*weblistener-port*))
  (terpri)
  (cformatt "Biolisp Weblistener starting, listening on port ~D" port)
  #+does-not-work
  (setq *current-wserver*
        (net.aserve:start 
         :port port :external-format (excl:find-external-format :default)))
  (setq *current-wserver* (net.aserve:start :port port))
  ;; This doesn't work...
  (setq net.aserve::*http-response-timeout* 3600)
  (clear-weblistener-state)
  (setq *current-weblistener-port* port)
  #+:allegro
  (setq *weblistener-pid* (excl::getpid))
  (setup-system-log-files)
  (let ((msg (formatn "Biolisp Weblistener started on port ~D~%" port)))
    (cformatt "~A" msg)
    (when cl-user:*logs-directory*
      (cformatt "Log file ~A" *system-logfile*)
      (cformatt "Alogfile ~A" *system-alogfile*)
      (log-system-event msg)
      (cformatt "Purging logs directory of files more than ~A days old."
                cl-user:*days-until-system-logs-purged*)
      (let ((n (purge-log-directory 
                :system :files-not-to-delete (list *help-requests-filename*))))
        (cformatt "~D log files purged." n))
      (cformatt "Creating system logs index file.")
      (create-log-files-index-for-user :system)
      ))
  #+:allegro
  (progn
    (diddle-aserve-process-priorities)
    (start-overseer-thread))
  (after-allegroserve-start-action cl-user:*ai*)
  (cformatt "Purging temporary weblistener files...")
  (handler-case 
      (progn
        (remove-temporary-weblistener-files)
        (cformatt "Purged."))
    (error 
     (c)
     (cformatt "Problem removing temporary files.  Actual error: ~A" c)))
  t)

(defun setup-system-log-files ()
  (when cl-user:*logs-directory*
    (setq *system-logfile* (make-timestamp-logfile :system))
    (setq *system-alogfile* (make-timestamp-logfile :system :type "alog"))
    (setq *system-login-file* 
          (let ((path (make-pathname :type "login")))
            (namestring (merge-pathnames path *system-logfile*))
            ))
    (setq *system-vpl-help-request-log-file* 
          (namestring 
           (merge-pathnames "vpl-help-requests.log" *system-logfile*)))
    (ensure-directories-exist *system-logfile*)
    (hack-allegroserve-logging)
    ))

;; Allegroserve doesn't provide any external documented interface to
;; control its logging messages.  We experimented and found the slot
;; deep in the bowels of the toplevel allegroserve data structure that 
;; contains the port that gets logged to.  So this code just replaces
;; the value in that slot with a port open to our own allegroserve log file.  
(defun hack-allegroserve-logging ()
  (case (os?)
    (:windows nil)
    ((:unix)
     (handler-case 
         (let ((vhost (net.aserve::wserver-default-vhost *current-wserver*)))
           (setf (net.aserve:vhost-log-stream vhost) 
                 (open
                  *system-alogfile* 
                  :direction :output :if-exists :supersede
                  )))
       (error
        () 
        (cformatt "Could not open ~A for aserve logging!" *system-alogfile*)
        )))
    (otherwise (error "Fix me"))
    ))


(defun diddle-aserve-process-priorities ()
  (let ((main-thread (slot-value *current-wserver* 'net.aserve::accept-thread))
        (worker-threads 
         (slot-value *current-wserver* 'net.aserve::worker-threads)))
    (set-process-priority *highest-priority* main-thread)
    (cformatt
     "Process priority for ~A set to ~A" 
     main-thread *highest-priority*)
    (loop for wt in worker-threads do
          (set-process-priority (1- *highest-priority*) wt)
          (set-process-quantum *initial-worker-quantum* wt))
    (cformatt "Process quantums for ~D worker threads set to ~A" 
              (length worker-threads) *initial-worker-quantum*)))


;;; Process will write a note to the log every
;;; (* *overseer-dump-every* *overseer-wakeup-interval*) seconds

(defparameter *overseer-dump-every* 200)


;;; When the overseer process wakes up, it checks up on every
;;; process that has a :pdl property, which indicates it is an
;;; allegroserve worker thread running a weblistener user's form.
;;; Each such process has its priority decremented.   Thus the
;;; longer a user's form is running the lower the priority, down
;;; to a minimum.  Brand new user forms run at highest priority,
;;; because the aserve worker process running that form gets
;;; reset to highest priority when the previous user form is
;;; finished executing.

(defun start-overseer-thread ()
  (let ((overseer-process 
         (run-function-as-process
          "overseer" 
          ;; For each worker thread, see if it has been 'instantiated'
          ;; as a weblistener thread (that is, it has a :pdl object
          ;; on its property list).  If so, decrement it's priority
          ;; unless it's already at minimum priority, and set its
          ;; quantum to a small interval.
          #'overseer-process-function
          )))
    (set-process-priority *highest-priority* overseer-process)
    (setq *overseer-thread* overseer-process)
    ))

(defun overseer-process-function ()
  (handler-case 
      (loop for j from 0 do (execute-overseer-function j))
    (error
     (c) 
     (let ((msg 
            (formatn "Overseer process error at ~A, actual error ~A"
                     (make-timestamp-string) c)))
       (cformatt "~A" msg)
       (log-system-event "~A" msg)
       ))))

(defun execute-overseer-function (j)
  (when (zerop (mod j *overseer-dump-every*))
    (log-system-event "Overseer process activated, ~D~%" j))
  (sleep *overseer-wakeup-interval*)
  (let ((worker-threads
         (slot-value 
          *current-wserver* 'net.aserve::worker-threads)))
    (loop for wt in worker-threads 
          as current-priority = (get-process-priority wt)
          do
          ;; Every weblistener user request gets a :pdl
          ;; property associated with it before it starts
          ;; via WITH-PROCESS-INFORMATION-BOUND in
          ;; WEBLISTENER-STANDARD-FORM-RESPONSE
          (when (getf (my-process-property-list wt) :pdl)
            (set-process-priority 
             (max 0 (1- current-priority)) wt)
            (set-process-quantum 
             *continuing-worker-quantum* wt)
            (when t ;; dump?
              (log-system-event 
               "  Process ~A, old priority: ~D, new priority: ~D."
               wt current-priority (get-process-priority wt)
               ))))))

(defun stop-weblistener ()
  (terpri)
  (cond
   ((or (not (boundp '*current-weblistener-port*)) 
        (null *current-weblistener-port*))
    (cformatt "??? *current-weblistener-port* is not bound or is NIL.")
    (cformatt "No Weblistener should be running in this Lisp!")
    (cformatt "Calling SHUTDOWN anyway...Good luck!"))
   (t
    (cformatt "Weblistener on port ~D shutting down." 
              *current-weblistener-port*)))
  (net.aserve:shutdown)
  (setq *current-weblistener-port* nil)
  (let ((msg "Weblistener stopped."))
    (format t ";; ~A~%" msg)
    (log-system-event (one-string msg #\Newline)))
  (kill-process *overseer-thread*)
  (cformatt "Overseer process killed")
  (after-allegroserve-stop-action cl-user:*ai*)
  (setq *overseer-thread* nil)
  (setq *current-wserver* nil)
  )

;;; Convenient shorthands because I don't feel like either typing, nor having
;;; to remember those, and package hair prevently me from creating shortcuts
;;; in my clinint.cl very easily.  UUU

(defun startweb 
       (&rest args &key (port cl-user:*weblistener-port*) &allow-other-keys)
  (declare (ignore port))
  (apply 'start-weblistener args))

(defun stopweb () (stop-weblistener))

(defmacro sw (&rest args) `(start-weblistener ,@args))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *memory-usage-history* nil)

(defun capture-current-memory-usage ()
  (let ((room-info 
         (with-output-to-string (s)
           (let ((*standard-output* s)) (room t))
           )))
    (declare (ignorable room-info))
    #+:allegro 
    (let ((pos (search "total bytes" room-info)))
      (when pos 
        (with-input-from-string (s (subseq room-info pos))
          ;; 'total'
          (read s nil nil)
          ;; 'bytes'
          (read s nil nil)
          ;; '='
          (read s nil nil)
          ;; memory consumption value returned
          (read s nil nil)
          )))))

(defun add-latest-memory-stats (&optional memory-used)
  (push (list (get-universal-time) 
              (or memory-used (capture-current-memory-usage)))
        *memory-usage-history*
        ))
    
(defun memory-usage (&key (last 60))
  (let ((usage *memory-usage-history*))
    (unless (eq last :all)
      (when (> (length usage) last)
        (setq usage (subseq usage 0 last))
        ))
    (setq usage (reverse usage))
    (formatt "~%Memory usage statistics: ~%~%")
    (loop for (time space) in usage 
          do 
          (formatt 
           "  ~A ~15D bytes~%"
           (make-timestamp-string :universal-time time)
           space
           ))
    (terpri)
    ))
   
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defvar *temp-file-types-not-to-delete* '("jar"))
(defvar *temp-file-names-not-to-delete* nil)

;;; This function marches through a set of directories and removes 
;;; files if they satisfy a criterion.  INFO-LIST is of the form 
;;; (directory criterion-function)

(defun delete-directory-files (info-list &key (recursive? t))
  (loop for (directory criterion) in info-list 
        as dirname = (namestring directory)
        do
        (cond
         ((not (pathname-names-directory? directory))
          (formatt 
           ";; *** Pathname not a directory: ~A~%;; *** Cannot purge!"
           dirname))
         (t 
          (handler-case 
              (file-operate 
               (list directory)
               criterion
               (lambda (x) 
                 (handler-case 
                     (delete-file x)
                   (error
                    ()
                    (formatt
                     ";; *** Could not delete ~A from ~A..." x dirname)
                    )))
               :recursive? recursive?
               )
            (error 
             (c)
             (formatt "Problem accessing directory ~A, actual error: ~A"
                      dirname c
                      )))))))

(defun file-has-existed-for? (file duration-in-seconds)
  (let ((current-time (get-universal-time))
        (file-write-time (file-write-date file)))
    (>= (- current-time file-write-time) duration-in-seconds)
    ))

(defun remove-temporary-weblistener-files ()
  (let* ((webtmp-info 
          (list wb::*webtmp-directory* 
                (lambda (x)
                  (let ((save-because-of-type?
                         (member 
                          (pathname-type x) *temp-file-types-not-to-delete*
                          :test 'equalp))
                        (save-because-of-name?
                         (member
                          (pathname-name x) *temp-file-names-not-to-delete*
                          :test 'equalp))
                        (file-is-old-enough-to-delete? 
                         (file-has-existed-for? x (* 60 60 48))))
                    (and file-is-old-enough-to-delete?
                         (not save-because-of-type?)
                         (not save-because-of-name?)
                         )))))
         (tmp-info 
          (list wb::*tmp-directory* 
                (lambda (x) (file-has-existed-for? x (* 60 60 48)))
                ))
         (info-list 
          (if (equalp wb::*webtmp-directory* wb::*tmp-directory*)
              (list webtmp-info)
            (list webtmp-info tmp-info)
            )))
    (loop for data in info-list do
          (cformatt "Cleaning up directory ~A..." (namestring (first data))))
    (delete-directory-files info-list :recursive? t)
    ))

#+test
(defun remove-test-files ()
  (let ((webtmp-info 
         (list (pathname "C:/biomisc/test/")
               (lambda (x) (file-has-existed-for? x (* 30 1)))
               )))
    (delete-directory-files (list webtmp-info) :recursive? t)
    ))
                 
                 
                      
            