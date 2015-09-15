;;; -*- mode: Lisp; Syntax: Common-Lisp; Package: wb; -*-

(in-package :wb)

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

;;; Author:  JP Massar.

;;; When the user executes a form, the Allegroserve worker process
;;; which is the thread which is doing the evaluation is marked with
;;; a PDL structure by the BioWebListener REPL, using the
;;; WITH-PROCESS-INFORMATION-BOUND macro.

(defstruct pdl process owner timestamp timestamp-string pid input-string)

(defun all-my-processes (&key (how :pprint))
  #.(one-string-nl
     "If HOW is :pprint (the default), prints out a table of all of your"
     "currently executing weblistener processes and returns the number"
     "of such processes."
     "If HOW is :list returns a list of process-descriptor structures, one"
     "descriptor for each of your executing processes."
     "If HOW is :processes returns a list of the actual implementation's"
     "process objects, one for each of your executing processes."
     "This function does NOT show you process threads you've created"
     "via RUNJOB or directly through allegro's MP::PROCESS-RUN-FUNCTION."
     )
  (let ((process-descriptor-list
         (all-process-descriptors-if
          (lambda (p) 
            (let ((pdl (getf (my-process-property-list p) :pdl)))
              (and pdl (eq *username* (pdl-owner pdl)))
              ))
          :sort-test '<
          :sort-key 'pdl-timestamp
          )))
    (ecase how
      ((:processes :process) (mapcar 'pdl-process process-descriptor-list))
      ((:list) process-descriptor-list)
      ((:pprint :pretty-print :print)
       (terpri)
       (cformatt "")
       (cformatt "Processes owned by ~A" *username*)
       (loop for pd in process-descriptor-list do
             (cformatt 
              "  PID: ~6A ~18A ~A" 
              (pdl-pid pd) (pdl-timestamp-string pd) 
              (limited-string (pdl-input-string pd) 40)
              ))
       (length process-descriptor-list)
       ))))
    

(defun users (&key (active-in-last 60 ail-p?) (sort-by :time))
  #.(one-string-nl
     "Shows users who have been active in the last ACTIVE-IN-LAST minutes,"
     "or all users by default."
     "Prints out each user's login name and time since last evaluation."
     "By default, order the display by the user's last execution time"
     "(a SORT-BY valye of :TIME).  If SORT-BY is :NAME, the listing is"
     "alphabetical according to user name.")
  (terpri) (terpri)
  (formatt "USER NAME        LAST ACTIVITY~%~%")
  (multiple-value-bind (sort-comparison sort-key)
      (ecase sort-by
        ((:time :last-execute-time) 
         (values 
          '> 
          (lambda (user) (last-execution-time user))
          ))
        ((:user :name :username) 
         (values 'string-lessp (lambda (user) (string (keywordize user))))
         ))
    (let ((count 0) 
          (seconds (* active-in-last 60))
          (now (get-universal-time))
          (logins (sort (copy-list *logins*) sort-comparison :key sort-key))
          )
      (loop for user in logins 
            unless (eq user :evaluser) 
            do
            (let ((last (last-execution-time user)))
              (when (or (not ail-p?) (< now (+ last seconds)))
                (incf count)
                (let ((how-long-ago (- now last)))
                  (cond
                   ((> how-long-ago (* 3600 72)) 
                    (formatt 
                     "~15a ~d days ago~%" 
                     user (round how-long-ago (* 3600 24))))
                   ((> how-long-ago (* 3600 24))
                    (formatt 
                     "~15a ~d hours ago~%" user (round how-long-ago 3600)))
                   ((> how-long-ago 3600)
                    (formatt
                     "~15a ~d hours, ~d minutes ago~%"
                     user 
                     (floor how-long-ago 3600) 
                     (round (mod how-long-ago 3600) 60)))
                   (t
                    (formatt 
                     "~15a  ~d minutes ago~%" user (round how-long-ago 60)))
                   )))))
      (when (< count (length *logins*))
        (let ((real-users (remove :evaluser *logins*)))
          (terpri)
          (formatt 
           (one-string
            "A total of ~D distinct ~A logged on since the Weblistener "
            "was activated.~%")
           (length real-users) 
           (if (= 1 (length real-users)) "user has" "users have")
           )))
      (formatt "System startup at ~A~%" 
               (make-timestamp-string 
                :universal-time  *system-startup-time* :mode :mmddyyhhmm
                ))
      )))

(defun last-execution-time (user)
  (let ((user (keywordize user)))
    (let ((user-session-ids (gethash user *user->sessionids-ht*)))
      (if (null user-session-ids)
          (get user :login-time)
        (reduce 'max user-session-ids 
                :key (lambda (x) (get x :last-execution-time))
                )))))

(defun accounts ()
  (terpri)
  (cformatt "All registered user accounts:")
  (terpri)
  (let ((sorted-accounts
         (sort (copy-tree *xyzzy*) 
               'string-lessp
               :key  (lambda (x) (string (user-login x)))
               )))
    (loop for entry in sorted-accounts
          as login = (user-login entry)
          as fullname = (user-fullname entry)
          as email = (user-email entry)
          do
          (unless (eq (keywordize login) :evaluser)
            (formatt "~12A ~30A ~A~%" login fullname email)
            )))
  (terpri)
  (length *xyzzy*)
  )


(defun all-webuser-processes (&key (how :pprint))
  #.(one-string-nl
     "If HOW is :pprint (the default), prints out a table of all currently"
     "executing webuser processes and returns the number of such processes."
     "If HOW is :list returns a list of process-descriptor structures, one"
     "descriptor for each executing webuser process."
     "If HOW is :processes returns a list of the actual implementation's"
     "process objects, one for each executing webuser process.")
  (let ((process-descriptor-list
         (all-process-descriptors-if
          (lambda (p) (getf (my-process-property-list p) :pdl))
          :sort-test 
          (lambda (x1 x2)
            (let ((owner1 (string (pdl-owner x1)))
                  (owner2 (string (pdl-owner x2)))
                  (time1 (pdl-timestamp x1)) 
                  (time2 (pdl-timestamp x2)))
              (cond 
               ((string-lessp owner1 owner2) t)
               ((string-greaterp owner1 owner2) nil)
               (t (< time1 time2))
               )))
          :sort-key 'identity
          )))
    (ecase how
      ((:processes :process) (mapcar 'pdl-process process-descriptor-list))
      ((:list) process-descriptor-list)
      ((:pprint :pretty-print :print)
       (terpri)
       (cformatt "")
       (cformatt "Running webuser processes:")
       (loop for pd in process-descriptor-list do
             (cformatt 
              "~12A PID: ~6A ~18A ~A" 
              (pdl-owner pd) (pdl-pid pd) (pdl-timestamp-string pd)
              (limited-string (pdl-input-string pd) 30)
              ))
       (cformatt "")
       (length process-descriptor-list)
       ))))


(defun sort-ppl-by-owner-then-time-predicate (x1 x2)
  ;; X1 and X2 are property lists from two different processes
  (let ((owner1 (string (pdl-owner x1)))
        (owner2 (string (pdl-owner x2)))
        (time1 (pdl-timestamp x1)) 
        (time2 (pdl-timestamp x2)))
    (cond 
     ((string-lessp owner1 owner2) t)
     ((string-greaterp owner1 owner2) nil)
     (t (< time1 time2))
     )))



(defun show-system-processes (&key (how :pprint) (webuser? t) (system? t))
  #.(one-string-nl
     "If HOW is :processes returns two values: the first a list of webuser"
     "processes if requested or NIL, the 2nd a list of system processes or NIL."
     "If HOW is :pprint (the default), prints out a table of all currently"
     "executing processes and returns the number of such processes."
     "If WEBUSER? is true (the default) the listing includes user processes."
     "If SYSTEM? is true (the default) the listing includes all other processes"
     "besides WEBUSER processes."
     "WEBUSER processes contain more information than system processes, since"
     "the Weblistener stores information about the process on the process'"
     "property list (in Allegro).  Therefore the system and webuser process"
     "listings look different and are displayed in two different sections.")
  (let ((webuser-processes
         (when webuser?
           (sort
            (remove-if-not 
             (lambda (p) (getf (my-process-property-list p) :pdl)) 
             (all-the-processes))
            'sort-ppl-by-owner-then-time-predicate
            :key (lambda (p) (getf (my-process-property-list p) :pdl))
            )))
        (system-processes
         (when system?
           (sort
            (remove-if
             (lambda (p) (getf (my-process-property-list p) :pdl)) 
             (all-the-processes))
            'string-lessp
            :key 'get-process-name
            ))))
    (ecase how
      ((:processes :process) (values webuser-processes system-processes))
      ((:pprint :pretty-print :print)
       (when webuser?
         (terpri)
         (if (null webuser-processes)
             (cformatt "No webuser processes exist...")
           (progn
             (cformatt "Existing webuser processes:")
             (terpri)
             (formatt 
              "  OWNER    ID  PRIORITY QUANTUM CPU       START         FORM~%~%")
             (loop for p in webuser-processes
                   as pd = (getf (my-process-property-list p) :pdl)
                   do
                   (formatt "~10A ~2D   ~3D     ~$ ~6D  ~17A  ~A~%"
                            (pdl-owner pd) (pdl-pid pd)
                            (get-process-priority p) (get-process-quantum p)
                            (get-process-cpu p)
                            (pdl-timestamp-string pd) 
                            (limited-string (pdl-input-string pd) 30)
                            )))))
       (when system?
         (terpri)
         (if (null system-processes)
             (cformatt "No system processes exist ???")
           (progn
             (cformatt "Existing system processes:")
             (terpri)
             (formatt "PROCESS NAME       PRIORITY  QUANTUM   CPU~%~%")
             (loop for p in system-processes do
                   (formatt "~20A ~3D      ~$     ~S~%"
                            (limited-string (get-process-name p) 17)
                            (get-process-priority p)
                            (get-process-quantum p)
                            (get-process-cpu p)
                            ))))))
      (values (length webuser-processes) (length system-processes))
      )))


(defparameter *super-secret-kill-flag* nil
  "Allows KILL-MY-PROCESS to kill any webuser process if T")

(defun kill-my-process (&rest pids)
  #.(one-string-nl
     "Kills one or more of your processes.  Each PID can either be a "
     "'PID' value (as shown by ALL-MY-PROCESSES) or an actual implementation"
     "process object."
     "Note: The following seems to be no longer true:"
     "****> WHEN YOU KILL A PROCESS, YOU FIRST NEED TO CLOSE"
     "THE BROWSER WINDOW THE PROCESS WAS STARTED FROM, RELOG IN, AND THEN"
     "KILL.  OTHERWISE IT WILL START RIGHT BACK UP AGAIN!! <*****")
  (let ((process-function
         (if *super-secret-kill-flag* 'all-webuser-processes 'all-my-processes))
        (processes-killed nil))
    (loop for pd in (funcall process-function :how :list) do
      (let ((pid (pdl-pid pd)) 
            (process (pdl-process pd)) 
            (owner (pdl-owner pd)))
        (when (or (member pid pids) (member process pids))
          (if (or *super-secret-kill-flag* (eq *username* owner))
              (handler-case
                  (progn
                    (cformatt "Killing process with pid ~A..." pid)
                    #-:SBCL
                    (mp:process-reset process)
                    (push pid processes-killed)
                    (push process processes-killed)
                    (cformatt "Process with pid ~A killed." pid))
                (error
                 (c)
                 (cformatt 
                  "**** Problem attempting to kill process with pid ~A" 
                  pid)
                 (cformatt "  Actual error signalled: ~A" c)
                 ))
            (ierror 
             "Attempt to kill process ~A (~A) not owned by executor (~A)."
             pid process owner
             )))))
    (loop for pid in pids do 
      (unless (find pid processes-killed)
        (cformatt "Process ~A not killed!" pid)
        ))))

(defun kill-my-processes (&rest pids) 
  "Alias for KILL-MY-PROCESS"
  (apply 'kill-my-process pids))

#+allegro
(defvar *username-pid-lock* (mp:make-process-lock :name "Username PID lock")
  "Lock used to protect the region where we set the :pid property on the username.")

(defun set-webuser-process-pdl (input-string)
  (let ((process-plist (my-process-property-list)))
    (when process-plist
      (add-process-property
       :pdl
       (make-pdl 
        :process (my-current-process) 
        :owner *username*
        :timestamp (get-universal-time)
        :timestamp-string (make-timestamp-string)
        :input-string input-string
        :pid
        (#+:allegro mp:with-process-lock #+allegro (*username-pid-lock*)
         #-:allegro
         progn
         (let ((pid (get *username* :pid)))
           (when (null pid) (setq pid (setf (get *username* :pid) 1)))
           (incf (get *username* :pid))
           pid
           )))))))

(defun clear-webuser-process-pdl () (clear-process-property :pdl))

(defun all-process-descriptors-if (test &key (sort-test '<) (sort-key 'second))
  (sort
   (loop for p in (all-the-processes) nconc
         (when (funcall test p)
           (let ((plist (my-process-property-list p)))
             (list (getf plist :pdl))
             )))
   sort-test
   :key sort-key
   ))

