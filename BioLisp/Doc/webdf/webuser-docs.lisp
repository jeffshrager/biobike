;; -*- mode: Lisp; Syntax: Common-Lisp; Package: weblistener; -*-

(in-package :webuser)

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

(help:document-module
    commands
  "Functions dealing with user-accessible system facilities"
  (:keywords :operating-system :system :system-utilities :process)
  (:display-modes :all)
  #.`(:functions ,@ cl-user::*webuser-user-symbols*))

(help:document-function my-stuff
  (:summary "Lists a user's functions and variables.")
  (:returns "Used for side effect, not for return value." :type t)
  (:examples 
   ((my-stuff) :nil "Prints table of user's functions and variables."))
  (:text
   (:p 
    #.(one-string-sp
       "List the names of user-defined variables and functions. "
       "MY-STUFF prints its output to the screen.  (To get a list of your "
       "stuff for further processing use MY-STUFF-AS-A-LIST.)"  
       ))
   (:p 
    #.(one-string-sp
       "Any user variables are dislayed first, along with their current values"
       "and type.  Functions are displayed next with live links"
       "that allow the user to edit the function definition."
       ))
   )
  (:parameters)
  (:see-also my-stuff-as-a-list lisp:apropos)
  )

(help:document-function my-stuff-as-a-list
  (:summary "A list of a user's functions and variables.")
  (:returns "A list of a user's functions and variables." :type list)
  (:examples
   ((my-stuff-as-a-list) :nil "A list of symbols."))
  (:text
   (:p 
    #.(one-string-sp
       "Returns all the symbols in a user's pacakage which are either"
       "BOUNDP or FBOUNDP.  Does not include imported symbols or symbols"
       "inherited from a used package.  The symbols are returned in no"
       "particular order."
       )))
  (:parameters)
  (:see-also my-stuff lisp:apropos)
  )

      
(help:document-function editfun
  (:summary "[Macro] Puts a function definition into the multiline box.")
  (:returns  "Used for side effect, not for return value." :type t)
  (:examples
   ((editfun my-function) :nil "Code for MY-FUNCTION -> multiline box"))
  (:text
   (:p 
    #.(one-string-sp
       "The NAME is converted to a symbol and if that symbol has an available"
       "function definition that definition is pretty-printed to the"
       "multiline box in the Weblistener."
       ))
   (:p 
    #.(one-string-sp
       "Only certain functions, specifically those defined by the user,"
       "have available function definitions.  It is generally not possible"
       "to edit the function definition of a common lisp function or a"
       "Weblistener system function."
       ))
   (:p 
    #.(one-string-sp
       "Since EDITFUN is a macro the argument is not evaluated, but"
       "EDITFUN is clever enough to interpret 'foo as foo."
       )))
  (:parameters 
   (name :docstring 
         "The symbol or string representing the function to be edited."))
  (:see-also my-stuff))
      
(help:document-function lisp-help
  (:summary 
   "[Macro] A list of Common Lisp symbols containing STRING in their names")
  (:returns  "Used for side effect, not for return value." :type t)
  (:examples
   ((lisp-help "bind") :nil 
    "Prints a table of Common Lisp symbols containing 'bind'."))
  (:text
   (:p 
    #.(one-string-sp
       "Prints out a table of all the exported Common Lisp symbols that"
       "contain (string-upcase STRING) in their name.  For symbols that have"
       "function bindings the argument list is also printed out."
       "LISP-HELP will also accept a symbol or a quoted symbol."
       )))
  (:parameters 
   (string :docstring "The symbol or string which is searched for."
           :value-type (or symbol string)))
  (:see-also lisp:apropos lisp:apropos-list lisp:search frames:search-frames))

(help:document-function users
  (:summary "Displays a list of users logged in.")
  (:returns  "Used for side effect, not for return value." :type t)
  (:examples
   ((users) :nil 
    "All users currently logged in, those most recently active first.")
   ((users :active-in-last 60) :nil 
    "Users who have typed something in the last 60 minutes.")
   ((users :sort-by :name) :nil
    "All users currently logged in sorted alphabetically.")
   )
  (:text
   (:p 
    #.(one-string-sp
       "Shows users who have been active in the last ACTIVE-IN-LAST minutes,"
       "or all users by default."
       "Prints out each user's login name and time since last evaluation."
       "By default, the display is ordered by the user's last execution time"
       "(a SORT-BY valye of :TIME).  If SORT-BY is :NAME, the listing is in"
       "alphabetical according to user name."
       ))
   (:p 
    #.(one-string-sp
       "Note: The Weblistener is brought down and restarted from time to time."
       "No information about users logged in to previous Weblistener sessions"
       "is printed by USERS.  Users may be logged in to more than one session,"
       "but this information is not presented; simply the last time the user"
       "typed at any of his sessions."))
   (:p 
    #.(one-string-sp
       "Note: Information about previous sessions can be had by using the"
       "'Session Logs' link.  System administrators have access to all users'"
       "session logs, while regular users only have access to their own."
       )))
  (:parameters 
   (active-in-last 
    :docstring 
    #.(one-string-sp
       "A time T in minutes such that if a user has not been active within the"
       "last T minutes no information is printed.  If this argument is not"
       "provided T is taken to be infinite.")
    :parameter-type &key :default-value 60 :value-type integer)
   (sort-by
    :docstring 
    #.(one-string-sp
       "Specifies the order in which the user names are displayed."
       "Legal values are either :time or :name.")
    :parameter-type &key :default-value :time :value-type keyword))
  (:see-also sessions all-my-processes lisp:room))

(help:document-function all-my-processes
  (:summary "Displays or returns user's active processes.")
  (:returns "The number of processes or a list of process descriptors.")
  (:examples
   ((all-my-processes) :nil "Lists all the user's active processes.")
   )
  (:text
   (:p 
    #.(one-string-sp
       "By default prints out a table of all a user's currently executing"
       "processes and identifies them with a process ID."
       ))
   (:p 
    #.(one-string-sp
       "This command can be used in conjunction with KILL-MY-PROCESS to"
       "kill a process running in another session."
       ))
   (:p 
    #.(one-string-sp
       "Note: Whenever the user executes a command in a session, a"
       "new process is created.  When a process is killed, the session"
       "itself remains, and the user may reconnect to it.  To permanently"
       "exit from a session, use the LOGOUT command."
       ))
   (:p 
    #.(one-string-sp
       "Note: This command can also be used to get a list of process"
       "descriptors or a list of process objects; these features would"
       "only be of interest to system administrators.  Use :list or :processes"
       "as the value of the HOW keyword."
       ))
   )
  (:parameters 
   (how
    :docstring "Determines what is printed and what is returned."
    :parameter-type &key :default-value :pprint :value-type keyword)
   )
  (:see-also kill-my-process runjob sessions logout))

(help:document-function kill-my-process
  (:summary "Kills one or more of your processes.")
  (:returns  "Used for side effect, not for return value." :type t)
  (:examples
   ((kill-my-process 46) :nil
    #.(one-string-sp
       "Kills the process whose process ID (as returned by ALL-MY-PROCESSES)"
       "is 46.")))   
  (:text
   (:p 
    #.(one-string-sp
       "Kills one or more of your processes.  Each PID can either be a "
       "'PID' value (as shown by ALL-MY-PROCESSES) or an actual implementation"
       "process object."
       ))
   (:p 
    #.(one-string-sp
       "Note: To kill a runaway process, log in again to the same Weblistener"
       "and choose 'New Login'.  (Use a new browser window to do this.)"
       "Type (all-my-processes) to find the PID"
       "of the process you want to kill, and call KILL-MY-PROCESS with that"
       "PID as the argument."
       ))
   (:p 
    #.(one-string-sp
       "Note: Whenever the user executes a command in a session, a"
       "new process is created.  When a process is killed, the session"
       "itself remains, and the user may reconnect to it.  To permanently"
       "exit from a session, use the LOGOUT command."
       ))
   (:p 
    #.(one-string-sp
       "Note: This function and KILL-MY-PROCESSES are identical."
       ))
   )
  (:parameters 
   (pids
    :docstring "The process IDs of the processes you want terminated."
    :parameter-type &rest :value-type (list-of integer)
    ))
  (:see-also all-my-processes runjob sessions logout))

(help:document-function kill-my-processes
  (:summary "Kills one or more of your processes.")
  (:returns  "Used for side effect, not for return value." :type t)
  (:examples
   ((kill-my-process 46) :nil
    #.(one-string-sp
       "Kills the process whose process ID (as returned by ALL-MY-PROCESSES)"
       "is 46.")))   
  (:text
   (:p 
    #.(one-string-sp
       "Kills one or more of your processes.  Each PID can either be a "
       "'PID' value (as shown by ALL-MY-PROCESSES) or an actual implementation"
       "process object."
       ))
   (:p 
    #.(one-string-sp
       "Note: To kill a runaway process, log in again to the same Weblistener"
       "and choose 'New Login'.  (Use a new browser window to do this.)"
       "Type (all-my-processes) to find the PID"
       "of the process you want to kill, and call KILL-MY-PROCESSES with that"
       "PID as the argument."
       ))
   (:p 
    #.(one-string-sp
       "Note: Whenever the user executes a command in a session, a"
       "new process is created.  When a process is killed, the session"
       "itself remains, and the user may reconnect to it.  To permanently"
       "exit from a session, use the LOGOUT command."
       ))
   (:p 
    #.(one-string-sp
       "Note: This function and KILL-MY-PROCESS are identical."
       ))
   )
  (:parameters 
   (pids
    :docstring "The process IDs of the processes you want terminated."
    :parameter-type &rest :value-type (list-of integer)
    ))
  (:see-also all-my-processes runjob sessions logout))


(help:document-function logout
  (:summary "Logs you out of your current session.")
  (:returns "Used for side effect, not for return value." :type t)
  (:examples
   ((logout) :nil "Your current session is eliminated.")
   ((logout :all-others) :nil 
    #.(one-string-sp
       "All your sessions other than the one you typed the logout command"
       "to are eliminated.")
    ))
  (:text
   (:p 
    #.(one-string-sp
       "By default logs you out of your current session."
       "If SESSIONID is a number, it will log you out of the session identified"
       "by that number as with the output of (SESSIONS)."
       "If SESSIONID is an actual sessionid (eg something like :massar83521)"
       "it will log you out of that session."
       "If SESSIONID is :all it will log you out of all of your sessions."
       "If SESSINOID is :all-others it will log you out of every existing session"
       "except the one you are typing at. (Use this to clean up sessions you"
       "may have lying around that you don't need anymore.)"
       ))
   (:p 
    #.(one-string-sp
       "Note: If you log out of your current session and hit ENTER, you will"
       "be directed to a new login page."
       ))
   )
  (:parameters 
   (sessionid
    :docstring "Indicates which session is to be eliminated."
    :parameter-type &key :default-value *sessionid* :value-type (or integer keyword)
    )
   (verbose?
    :docstring "If T, prints out information about sessions being logged out of."
    :parameter-type &key :default-value t :value-type boolean)
   )
  (:see-also all-my-processes runjob kill-my-processes my-sessions sessions))

(help:document-function my-sessions
  (:summary "Prints out information about the user's sessions.")
  (:returns "Used for side effect, not for return value." :type t)
  (:examples
   ((my-sessions) :nil "Information about each login session."))
  (:text
   (:p 
    #.(one-string-sp
       "A user may be logged in multiple times to a single Weblistener."
       "SESSIONS prints out a table containing, for each such login session,"
       "the Session ID, the last time the user executed a command,"
       "the time the session was initiated, the location of the log file,"
       "and the latest form evaluated."
       ))
   )
  (:parameters)
  (:see-also all-my-processes runjob kill-my-processes sessions logout))

(help:document-function runjob
  (:summary "Executes code in a separate Lisp thread.")
  (:returns "The Lisp structure representing the spawned thread." :type t)
  (:examples
   ((runjob () (loop for j from 1 to 10 do (format t "~D~%" j) (sleep 1)))
    "#<MULTIPROCESSING:PROCESS RUNJOB-26193(15) @ #x23875192>"
    #.(one-string-nl
       "Takes 10 seconds to create an output file called runjob.output"
       "in your Biobike home directory with the first 10 integers as contents."))
   )
  (:text
   (:p 
    #.(one-string-sp
       "Executes BODY in a separate thread.  Normal output from BODY "
       "(i.e., written to *standard-output* or *standard-error*)"
       "is diverted to OUTPUT-FILE.  The thread runs for a maximum of"
       "TIMELIMIT seconds before exiting.  Prior to running the thread"
       "sleeps for SLEEP-FOR seconds (or 0 if SLEEP-FOR is nil)."
       "RUNJOB returns the process data structure."))
   (:p
    #.(one-string-sp
       "If the thread gets an error, it is aborted, and the symbol"
       "*RUNJOB-ERROR* (or the symbol which is the value of ERROR-FLAG)"
       "is set to T (initially it is set to NIL)."
       "If the thread completes, the symbol *RUNJOB-DONE* (or the symbol which"
       "is the value of DONE-FLAG) is set to T (initially it is set to NIL)."
       "The symbol *RUNJOB-OUTPUT-FILE* (or the symbol which is the value of"
       "OUTPUT-FILE-SYMBOL) is set to the full pathname of the output file"
       "before execution of the background thread starts."))
   (:p
    #.(one-string-sp
       "RUNJOB sets the processes' priority and quantum to PRIORITY"
       "and QUANTUM.  Normally, a process has priority 0 and quantum 1.0,"
       "but RUNJOB by default sets the priority to -100 (very low priority)"
       "and 0.1 (interruptable every tenth of a second)."
       "If VERBOSE? is T (the default), information about the thread is"
       "printed out before it begins execution."))
   (:p
    #.(one-string-sp
       "Threads started with RUNJOB cannot be killed using KILL-MY-PROCESSES."
       "You must use MP:PROCESS-KILL on the object returned by RUNJOB"
       "to terminate the spawned thread."))
   (:p
    #.(one-string-sp
       "Note: Use (LIST-FILE-CONTENTS *RUNJOB-OUTPUT-FILE*) to check on the"
       "status of the thread (assuming you cause it to do any output!)"
       ))
   (:p
    #.(one-string-sp
       "Note: By default, RUNJOB assumes it is executing from the Weblistener."
       "If not, the caller (usually a system administrator) must provide"
       ":FROM-Weblistener? NIL."   
       ))
   )
  (:parameters 
   (output-file
    :docstring "A file to which output from the spawned thread is written."
    :parameter-type &key :default-value "runjob.output" 
    :value-type (or string pathname))
   (timelimit
    :docstring
    "A maximum amount of time (in seconds) the spawned thread is allowed to execute."
    :parameter-type &key :default-value 1800
    :value-type integer)
   (priority
    :docstring
    "The Lisp system priority the thread will be given."
    :parameter-type &key :default-value -100
    :value-type integer)
   (quantum
    :docstring
    "The Lisp system quantum the thread will be given (in seconds)."
    :parameter-type &key :default-value 0.1
    :value-type number)
   (error-flag
    :docstring
    "The name of a global variable that will be set if the thread errors."
    :parameter-type &key :default-value *runjob-error*
    :value-type symbol)
   (done-flag
    :docstring
    "The name of a global variable that will get set if the thread finishes."
    :parameter-type &key :default-value *runjob-done*
    :value-type symbol)
   (output-file-symbol
    :docstring
    "A global variable which is set to the full pathname of the output file."
    :parameter-type &key :default-value *runjob-output-file*
    :value-type symbol)
   (sleep-for 
    :docstring
    #.(one-string-nl
       "If non-nil, specifies that the job should sleep for some number"
       "of seconds BEFORE executing.  TIMELIMIT does not take effect"
       "until the job begins execution, so this parameter makes it possible"
       "to begin a job at some point in the future without timing out.")
    :parameter-type &key :default-value nil :value-type (or boolean number))
   (verbose?
    :docstring 
    #.(one-string-sp
       "If T, information about the spawned thread and the names of"
       "the global variables RUNJOB may set, is displayed before the thread"
       "is started.")
    :parameter-type &key :default-value t 
    :value-type boolean)
   )
  (:see-also all-my-processes sessions kill-my-processes my-sessions logout)
  (:examples-package :webuser)
  )

(help:document-function sessions
  (:summary "Prints out information about a user's sessions.")
  (:returns "Used for side effect, not for return value." :type t)
  (:examples
   ((sessions) :nil "Information about each login session.")
   ((sessions :user :massar) :nil "Information about massar's login sessions."))
  (:text
   (:p 
    #.(one-string-sp
       "A user may be logged in multiple times to a single Weblistener."
       "SESSIONS prints out a table containing, for each such login session,"
       "the Session ID, the last time the user executed a command,"
       "the time the session was initiated, the location of the log file,"
       "and the latest form evaluated.  The keyword argument :USER to SESSIONS"
       "allows one to view any other currently logged-on user's sessions."
       ))
   )
  (:parameters 
   (user
    :docstring "The user whose sessions are to be described."
    :parameter-type &key :default-value *username* :value-type keyword)
   )
  (:see-also all-my-processes runjob kill-my-processes my-sessions logout))

(help:document-function user-mode
  (:summary "Returns which evaluation mode the Weblistener is in.")
  (:returns "Either :bbl or :biolisp" :type t)
  (:examples
   ((user-mode) :biolisp "The default mode on the Stanford BioBike server.")
   ((user-mode) :bbl "The default mode on the VCU Biobike server.")
   )
  (:text
   (:p 
    #.(one-string-sp
       "Currently, the Weblistener evaluate forms in one of two modes:"
       "BIOLISP-MODE, which is standard Common Lisp syntax and semantics,"
       "with additional functionality,"
       "BBL-MODE, which is a biologist-friendly variant on Common Lisp,"
       "both syntactically and semantically."))
   (:p 
    "Note: To learn more about BBL, use the Help menu and select 'BBL documentation'"
    ))
  (:parameters)
  (:see-also bbl bbl-mode bbl-mode? lisp biolisp-mode biolisp-mode? ensure-bbl-mode))

(help:document-function bbl
  (:summary "Puts the user into BBL mode.")
  (:returns "Used for side effect, not for return value." :type t)
  (:examples
   ((bbl) :nil "Enters (or remains in) BBL mode.")
   ((bbl :permanent? t) :nil "Enters BBL mode and makes it his default.")
   ((bbl :destroy-my-symbols? t) :nil 
    "Enters BBL mode even if package conflicts exist.")
   )
  (:text
   (:p 
    #.(one-string-sp
       "If the user is not already in BBL mode, attempts to put the user into"
       "that mode.  If the attempt is not successful, it is generally due to"
       "symbol conflicts.  For example, the user may have typed"
       "(defun split (x) (1+ x))" 
       "before entering BBL mode.   Since BBL contains a SPLIT function,"
       "the symbol SPLIT is now conflicted between the user and BBL."
       "The user may use the :destroy-my-symbols? parameter if he wishes to have"
       "BBL destroy his SPLIT symbol (and its function definition along with it)."
       "Or the user may choose to resolve the situation himself by uninterning"
       "the SPLIT symbol: (unintern 'split), then reattempting to enter BBL mode."
       ))
   (:p
    #.(one-string-sp
       "If the user specifies PERMANENT? as T, then his initialization file"
       "will be modified so that it contains a call to (bbl), which has"
       "the effect of putting the user immediately into BBL mode when he logs in."))
   (:p 
    #.(one-string-sp
       "Note: To learn more about BBL, use the Help menu and select 'BBL documentation'"
       "and/or run through the introductory live tutorials available via the help menu"
       "when in BBL mode.")))
  (:parameters
   (permanent? 
    :docstring "Whether to make BBL mode the default."
    :parameter-type &key :default-value nil
    :value-type boolean)
   (destroy-my-symbols?
    :docstring "Whether to enter BBL even if conflicting definitions exist."
    :parameter-type &key :default-value nil
    :value-type boolean))
  (:see-also user-mode bbl-mode bbl-mode? lisp biolisp-mode
   biolisp-mode? ensure-bbl-mode))

(help:document-function bbl
  (:summary "Puts the user into BBL mode.")
  (:returns "Used for side effect, not for return value." :type t)
  (:examples
   ((bbl) :nil "Enters (or remains in) BBL mode.")
   ((bbl :permanent? t) :nil "Enters BBL mode and makes it the default.")
   ((bbl :destroy-my-symbols? t) :nil 
    "Enters BBL mode even if package conflicts exist.")
   )
  (:text
   (:p 
    #.(one-string-sp
       "If the user is not already in BBL mode, attempts to put the user into"
       "that mode.  If the attempt is not successful, it is generally due to"
       "symbol conflicts.  For example, the user may have typed"
       "(defun split (x) (1+ x))" 
       "before entering BBL mode.   Since BBL contains a SPLIT function,"
       "the symbol SPLIT is now conflicted between the user and BBL."
       "The user may use the :destroy-my-symbols? parameter if he wishes to have"
       "BBL destroy his SPLIT symbol (and its function definition along with it)."
       "Or the user may choose to resolve the situation himself by uninterning"
       "the SPLIT symbol: (unintern 'split), then reattempting to enter BBL mode."
       ))
   (:p
    #.(one-string-sp
       "If the user specifies PERMANENT? as T, then his initialization file"
       "will be modified so that it contains a call to (bbl), which has"
       "the effect of putting the user immediately into BBL mode when he logs in."))
   (:p 
    #.(one-string-sp
    "Note: To learn more about BBL, use the Help menu and select 'BBL documentation'"
    "and/or run through the introductory live tutorials available via the help menu"
    "when in BBL mode.")))
  (:parameters
   (permanent? 
    :docstring "Whether to make BBL mode the default."
    :parameter-type &key :default-value nil
    :value-type boolean)
   (destroy-my-symbols?
    :docstring "Whether to enter BBL even if conflicting definitions exist."
    :parameter-type &key :default-value nil
    :value-type boolean))
  (:see-also user-mode bbl-mode bbl-mode? lisp biolisp-mode
   biolisp-mode? ensure-bbl-mode))

(help:document-function bbl-mode
  (:summary "Puts the user into BBL mode.")
  (:returns "Used for side effect, not for return value." :type t)
  (:examples
   ((bbl-mode) :nil "Enters (or remains in) BBL mode.")
   ((bbl-mode :permanent? t) :nil "Enters BBL mode and makes it the default.")
   ((bbl-mode :destroy-my-symbols? t) :nil 
    "Enters BBL mode even if package conflicts exist.")
   )
  (:text
   (:p 
    #.(one-string-sp
       "If the user is not already in BBL mode, attempts to put the user into"
       "that mode.  If the attempt is not successful, it is generally due to"
       "symbol conflicts.  For example, the user may have typed"
       "(defun split (x) (1+ x))" 
       "before entering BBL mode.   Since BBL contains a SPLIT function,"
       "the symbol SPLIT is now conflicted between the user and BBL."
       "The user may use the :destroy-my-symbols? parameter if he wishes to have"
       "BBL destroy his SPLIT symbol (and its function definition along with it)."
       "Or the user may choose to resolve the situation himself by uninterning"
       "the SPLIT symbol: (unintern 'split), then reattempting to enter BBL mode."
       ))
   (:p
    #.(one-string-sp
       "If the user specifies PERMANENT? as T, then his initialization file"
       "will be modified so that it contains a call to (bbl), which has"
       "the effect of putting the user immediately into BBL mode when he logs in."))
   (:p 
    #.(one-string-sp
    "Note: To learn more about BBL, use the Help menu and select 'BBL documentation'"
    "and/or run through the introductory live tutorials available via the help menu"
    "when in BBL mode."))
   )
  (:parameters
   (permanent? 
    :docstring "Whether to make BBL mode the default."
    :parameter-type &key :default-value nil
    :value-type boolean)
   (destroy-my-symbols?
    :docstring "Whether to enter BBL even if conflicting definitions exist."
    :parameter-type &key :default-value nil
    :value-type boolean))
  (:see-also user-mode bbl bbl-mode? lisp biolisp-mode
   biolisp-mode? ensure-bbl-mode))

(help:document-function bbl-mode?
  (:summary "Returns T if user is in the Weblistener's BBL evaluation mode.")
  (:returns "T or NIL." :type boolean)
  (:examples
   ((bbl-mode?) T)
   )
  (:text)
  (:parameters)
  (:see-also user-mode bbl bbl-mode lisp biolisp-mode
   biolisp-mode? ensure-bbl-mode))

(help:document-function lisp
  (:summary "Puts the user into Biolisp mode.")
  (:returns "Used for side effect, not for return value." :type t)
  (:examples
   ((lisp) :nil "Enters (or remains in) Biolisp mode.")
   ((lisp :permanent? t) :nil "Enters Biolisp mode and makes it the default.")
   )
  (:text
   (:p 
    #.(one-string-sp
       "If the user is not already in Biolisp mode, puts the user into"
       "that mode."
       ))
   (:p
    #.(one-string-sp
       "If the user specifies PERMANENT? as T, then his initialization file"
       "will be modified so that it contains a call to (lisp), which has"
       "the effect of putting the user immediately into Biolisp mode when he logs in."))
   (:p 
    #.(one-string-sp
       "Note: To learn more about Biolisp, use the Help menu and select 'Documentation'"
       "and/or run through the introductory live tutorials available via the help menu"
       "when in Biolisp mode."
       )))
  (:parameters
   (permanent? 
    :docstring "Whether to make Biolisp mode the default."
    :parameter-type &key :default-value nil
    :value-type boolean)
   )
  (:see-also user-mode bbl bbl-mode? bbl-mode biolisp-mode
   biolisp-mode? ensure-bbl-mode))

(help:document-function biolisp-mode
  (:summary "Puts the user into Biolisp mode.")
  (:returns "Used for side effect, not for return value." :type t)
  (:examples
   ((biolisp-mode) :nil "Enters (or remains in) Biolisp mode.")
   ((biolisp-mode :permanent? t) :nil "Enters Biolisp mode and makes it the default.")
   )
  (:text
   (:p 
    #.(one-string-sp
       "If the user is not already in Biolisp mode, puts the user into"
       "that mode."
       ))
   (:p
    #.(one-string-sp
       "If the user specifies PERMANENT? as T, then his initialization file"
       "will be modified so that it contains a call to (lisp), which has"
       "the effect of putting the user immediately into Biolisp mode when he logs in."))
   (:p 
    #.(one-string-sp
       "Note: To learn more about Biolisp, use the Help menu and select 'Documentation'"
       "and/or run through the introductory live tutorials available via the help menu"
       "when in Biolisp mode."
       )))
  (:parameters
   (permanent? 
    :docstring "Whether to make Biolisp mode the default."
    :parameter-type &key :default-value nil
    :value-type boolean)
   )
  (:see-also user-mode bbl bbl-mode? bbl-mode lisp biolisp-mode? ensure-bbl-mode))

(help:document-function biolisp-mode?
  (:summary "Returns T if user is in the Weblistener's Biolisp evaluation mode.")
  (:returns "T or NIL." :type boolean)
  (:examples
   ((biolisp-mode?) T)
   )
  (:text)
  (:parameters)
  (:see-also user-mode bbl bbl-mode lisp biolisp-mode
   bbl-mode? ensure-bbl-mode))

(help:document-function ensure-bbl-mode
  (:summary "Puts the user into BBL mode and makes it his default.")
  (:returns "Used for side effect, not for return value." :type t)
  (:examples
   ((ensure-bbl-mode) :nil "Enters (or remains in) BBL mode and makes it the default.")
   )
  (:text
   (:p 
    #.(one-string-sp
       "This is equivalent to (bbl-mode :permanent? t)."
       "The user is put into BBL mode if possible, and if the attempt succeeds"
       "the user's initialization file is modified to make BBL mode the default.")))
  (:parameters)
  (:see-also user-mode bbl bbl-mode? bbl-mode lisp biolisp-mode? biolisp-mode))


(help:document-function hop
  (:summary "HOP (History OutPut): Used to reference a previous result.")
  (:returns
   "One of the values (default first) of the result indicated by N."  :type t)
  (:examples
   ((+ (hop 2) 1) :nil 
    "If the result form for the input form labeled <2>> is 10, returns 11")
   ((+ (hop 2 3) 1) :nil
    "If the input form for <2>> was (values 3 10 8), then 9 is returned.")
   )
  (:text
   (:p 
    #.(one-string-sp
       "Each form that is evaluated and its subsequent results is indexed"
       "by a small integer, shown in the weblistener before each input form."
       "The index looks like '<1>>'."
       ))
   (:p 
    #.(one-string-sp
       "Each evaluated input form has one or more results associated with it."
       "(Usually one, but if a form returns multiple values, any number)"
       "If a single result is returned, the result is shown immediately after"
       "'::'.  If more than one result is returned, each result is preceded"
       "by a '>'."
       ))
   (:p 
    #.(one-string-sp
       "HOP allows the user to retrieve a previous result via its index"
       "and, if necessary, a specifics return value.  The first argument,"
       "N, specifies the index, while the optional argument VALUE-INDEX"
       "specifies which multiple value to return."
       ))
   )
  (:parameters
   (n
    :docstring "The history index from which to retrieve the result."
    :value-type integer)
   (value-index 
    :docstring "Which multiple value to return"
    :parameter-type &optional :default-value 1
    :value-type integer)
   )   
  (:see-also result redo)
  )


(help:document-function load-user-file
  (:summary 
   "Load a lisp file from your, or another user's Weblistener home directory.")
  (:returns "Used for side effect, not for return value." :type t)
  (:examples
   ((load-user-file "my-file.lisp") :nil 
    #.(one-string-sp
       "The file my-file.lisp in the user's weblistener home directory"
       "is loaded (using LISP:LOAD) if it exists."))
   ((load-user-file "my-file.lisp" :userid "jelhai") :nil 
    #.(one-string-sp
       "The file my-file.lisp in JELHAI's weblistener home directory"
       "is loaded (using LISP:LOAD) if it exists."))
   ((load-user-file "/my-subdir/my-file.lisp") :nil 
    #.(one-string-nl
       "The file my-file.lisp in the my-subdir subdirectory of the user's " 
       ";; Weblistener home directory is loaded (using LISP:LOAD) if it exists."
       )))
  (:text
   "Load a lisp file from your, or another user's Weblistener home directory."
   )
  (:parameters
   (name 
    :docstring "The name of the file to be loaded."
    :value-type string)
   (userid
    :docstring "The name of a user"
    :parameter-type &key :default-value *username*
    :value-type string)
   )   
  (:see-also lisp:load lisp:compile-file list-file-contents utils:c/l)
  )

(help:document-function redo
  (:summary "Re-evaluates one or more previous input expressions")
  (:returns "Whatever the expression to be re-evaluated returns." :type t)
  (:examples
   ((redo +) :nil 
    #.(one-string-sp
       "If the most recent form beginning with '(+ ...)' is (+ 3 4)"
       "then 7 is returned."))
   ((redo 5) :nil 
    "If the form indexed by '<5>>' is (abs -3) then 3 is returned.")
   ((redo 3 5) :nil 
    #.(one-string-nl
       "If the forms indexed by <3>>, <4>> and <5>> are (+ 3 4) "
       ";; (print 'foo) (- 10 4) respectively then 'foo' is printed out "
       ";; and 6 is returned."))
   )
  (:text
   (:p
    #.(one-string-sp
       "Re-executes a previous input form.  The form can be identified either"
       "by its first element or by the history index.  (A form which is"
       "not a list, such as a string, cannot be re-evaluated using this method."
       ))
   (:p
    #.(one-string-sp
       "Multiple forms identified via a range of history indices may also be"
       "re-evaluated.  Each form is re-evaluated in turn, but only the result"
       "of the last form is returned."
       ))
   (:p
    #.(one-string-sp
       "REDO is a macro; its arguments are not evaluated.  Therefore"
       "you cannot pass an argument to REDO, it only takes literal symbols"
       "or integers."
       ))
   )
  (:parameters
   (symbol 
    :docstring 
    "A symbol or an integer, used to find the history form to be re-evaluated."
    :value-type (or symbol integer))
   (limit 
    :docstring 
    #.(one-string-sp
       "If SYMBOL is an integer, LIMIT denotes the upper bound on the history"
       "indices whose forms will be re-evaluated.")
    :parameter-type &optional :value-type integer))
  (:see-also hop result)
  )

(help:document-function result
  (:summary "Returns the Nth result as in HOP.")
  (:returns "One of the values (default first) of the result indicated by N."
   :type t)
  (:text
   (:p "This is an alias for HOP.  Refer to that documentation."))
  (:parameters
   (n
    :docstring "The history index from which to retrieve the result."
    :value-type integer)
   (value-index 
    :docstring "Which multiple value to return"
    :parameter-type &optional :default-value 1
    :value-type integer)
   )
  (:see-also hop redo)
  )

(help:document-function set-email-address
  (:summary "Sets a user's email")
  (:returns "Used for side effect, not for return value."
   :type t)
  (:text
   (:p
    #.(one-string-sp
       "Set a users' email address, overriding the one stored in the system,"
       "and/or entered at login time.  The email address is only overridden"
       "while the weblistener is running; it is not saved permanently."
       "At present, you must ask a system administrator to change the"
       "email address recorded for you in the BioBike system if you"
       "wish the change to be permanent."))
   (:p
    #.(one-string-sp
       "The user's email address is used by the EMAIL-ME command and"
       "is also used by the feedback page as a return address."
       ))
   )
  (:examples
   ((set-email-address "massar@new-address.com") :nil 
    "Changes the system's notion of your email address."))
  (:parameters
   (email-string
    :docstring "The new email address."
    :value-type string)
   )
  (:see-also email-me)
  )


(help:document-function email-me
  (:summary "E-mails a file, a definition, or any lisp object." )
  (:returns "Used for side effect, not for return value." :type t)
  (:examples
   ((email-me "myfile.tbl") :nil 
    #.(one-string-sp
       "Mails the file myfile.tbl in the user's weblistener"
       "home directory to the user."))
   ((email-me 'my-function :to "massar@wherever.com") :nil
    "Mails the definition of MY-FUNCTION to the address given.")
   ((email-me (iota 2000) :subject "The first 2000 integers") :nil
    "Mails (0 1 2 3 ... 1998 1999) to the user with a provided Subject: line.")
   ((email-me "/home/visitors/jelhai/foo.lisp") :nil
    "Mails the file foo.lisp in JELHAI's weblistener home directory.")
   )
  (:text
   (:p 
    #.(one-string-sp
       "Emails the contents of a file, or the source code (definition)"
       "of a function, or the printed representation (if any) of any"
       "other lisp object.  The information is mailed to the current user"
       "by default (if the system knows his email address), or to the"
       "email address specified by :TO if provided.  The subject line"
       "of the email is, by default, 'Requested BioBike object',"
       "or the :SUBJECT argument, if provided."))
   (:p 
    #.(one-string-sp
       "If a Lisp object is being mailed, EMAIL-ME uses PPRINT to create a"
       "string representation of OBJECT, which is then sent."))
   (:p 
    #.(one-string-sp
       "If OBJECT is a string, if it is a short string (< 256 characters)"
       "EMAIL-ME tries to interpret the string as a filename and"
       "if the file exists, mails the contents of the file (if no such"
       "file exists, an error is signalled).  If the string is longer than"
       "256 characters, the string itself is mailed.  To mail"
       "a short string that is not a file name, enclose the string"
       "in a list, e.g., (email-me (list \"your-short-string-here\")),"
       "and then remove the parentheses when you receive the email."
       ))
   )
  (:parameters
   (object :docstring "The thing or name of the object to be e-mailed"
           :value-type t)
   (to :docstring "The recipient of the e-mail"
       :value-type string :parameter-type &key :default-value nil)
   (subject :docstring "The subject of the e-mail"
            :value-type string :parameter-type &key :default-value nil))
  (:see-also set-email-address "Tools -> Upload File")
  )
  
(help:document-function clear-history
  (:summary "Delete part or all of the user's recorded history.")
  (:returns "Used for side effect, not for return value." :type t)
  (:examples
   ((clear-history) :nil "Clears the entire history save the last entry.")
   ((clear-history :all) :nil "Clears the entire history.")
   ((clear-history 20) :nil "Clears the oldest 20 history entries.")
   ((clear-history -20) :nil "Clears the most recent 20 history entries.")
   ((clear-history :from 10) :nil 
    "Clears all history items from index 10 to the most recent item.")
   ((clear-history :to 15) :nil 
    "Clears all history items from the beginning of the history to index 15.")
   )
  (:text
   (:p 
    #.(one-string-sp
       "Delete part or all of the recorded history of the user's input/output"
       "(and hence what gets shown to the user).  By default the entire"
       "history is cleared except for the most recent entry.  If there is"
       "only one entry, it is cleared no matter what."
       ))
   (:p 
    #.(one-string-sp
       "History can be cleared relative to the start or end of"
       "the current display, or in a specified range from the beginning"
       "forward or from the end backward.  See examples."
       ))
   (:p 
    #.(one-string-sp
       "Note: It is useful to clear your history if you have no need to"
       "refer to it, because the screen will refresh faster and system"
       "memory will be freed up.  A transcript of your output is kept"
       "in a session log, so that something cleared is always recoverable."
       ))
   )
  (:parameters
   (how-much 
    :docstring 
    #.(one-string-sp
       "Determines the method used for clearing, and sometimes"
       "the number of history items to clear.")
    :parameter-type &optional :default-value :all-but-latest 
    :value-type (or keyword integer))
   (limit :docstring "An index of a history item."
          :value-type integer :parameter-type &optional :default-value nil)
   )
  (:see-also "Session Logs link on Weblistener page.")
  )
  


(help:document-function explain
  (:summary "Prints out a stack trace from the most recently occuring error.")
  (:returns "Used for side effect, not for return value." :type t)
  (:examples
   ((explain) :nil "Shows the Allegro stack trace.")
   )
  (:text
   (:p 
    #.(one-string-sp
       "Prints out a stack trace from the most recently occurring error."
       "The stack trace is edited to remove some frames that are deemed"
       "useless before being printed.  You can view the entire stack trace"
       "by executing (wb::explain! :verbose? t), but you probably don't want"
       "this!"
       ))
   (:p 
    #.(one-string-sp
       "Note: These stack traces can be very difficult to read unless"
       "you are familiar with them.  Print statements in your code are"
       "another way of figuring out what is going on when you run into"
       "problems.  If you need help decoding the stack trace information,"
       "please ask the system administrators."
       ))
   )
  (:see-also (url #.(s+ wb::*doc-directory-url* "progdev.html") 
                  "Program Development and Debugging"))
  )
  
(help:document-function explain-timeout
  (:summary "Prints out a long discussion of options if you timeout.")
  (:returns "Used for side effect, not for return value." :type t)
  (:text
   (:p
    #.(one-string-sp
       "The system will abort your program if it takes longer than"
       "a certain number of seconds (user settable up to a limit)."
       "This function prints out a discussion of what you should do"
       "if this occurs.")))
  (:see-also runjob explain set-timelimit set-output-limits 
   show-output-limits "Tools -> Prefs" *execution-timelimit*
   cl-user:*maximum-execution-timelimit* cl-user:*default-execution-timelimit*))
  

(help:document-function reload
  (:summary "Reloads a user's initialization file.")
  (:returns 
   "T if the init file was successfully reloaded, NIL if an error occured."
   :type boolean)
  (:examples
   ((reload) t "Causes the user's biolisp.ini file to be (re)loaded.")
   )
  (:text
   (:p 
    #.(one-string-sp
       "Each user has an initialization file in his weblistener home directory"
       "called 'biolisp.ini'.  This file is created automatically the first"
       "time a user logs in.  RELOAD causes the system to call LISP:LOAD"
       "on this file."
       ))
   (:p 
    #.(one-string-sp
       "Note: This function may be useful if you are editing your"
       "initialization file.  To edit this file, choose the 'Browse Files'"
       "link, choose the link to your home directory, and then click on the"
       "EDIT link for the biolisp.ini file."
       ))
   )
  (:see-also "Browse Files (link)" lisp:load)
  )

(help:document-function set-timelimit
  (:summary "Changes the user's timeout limit.")
  (:returns "The new timelimit." :type integer)
  (:examples
   ((set-timelimit 100) 100 
    "An executed form will now time out after 100 seconds.")
   ((set-timelimit 1000000) :nil 
    "The timelimit is too large, an error is signalled.")
   ((set-timelimit -5) :nil "The timelimit is invalid, an error is signalled.")
   )
  (:text
   (:p 
    #.(one-string-sp
       "Changes the timeout threshold for a single computation executed by"
       "the Weblistener for the current user.  The argument is given"
       "in seconds.  A value greater than *maximum-execution-timelimit*"
       "is not allowed."
       ))
   (:p 
    #.(one-string-sp
       "Note: The value of the variable *execution-timelimit* is"
       "the user's current timelimit."
       ))
   )
  (:parameters
   (seconds :docstring "The new timeout limit" :value-type integer)
   )
  (:see-also set-output-limits show-output-limits runjob "Tools -> Prefs"
   *execution-timelimit* cl-user:*maximum-execution-timelimit* 
   cl-user:*default-execution-timelimit*)
  )

(help:document-function set-output-limits
  (:summary 
   "Sets parameters having to do with how the weblistener prints output.")
  (:returns "Used for side effect, not for return value." :type t)
  (:examples
   ((set-output-limits :lines 100) :nil 
    "No more than 100 lines of output will be displayed for a single evaluation")
   ((set-output-limits :chars-per-line 90) :nil
    "No more than 90 characters per line will be printed.")
   ((set-output-limits :truncation-message-mode :none) :nil
    "Warning messages about truncated output are suppressed.")
   ((set-output-limits :lines 100 :chars-per-line 90 
                       :truncation-message-mode :none) :nil
    "As above, all at once.")
   )
  (:text
   (:p
    #.(one-string-sp
       "Sets a limit on the number of lines printed out"
       "and the width of each line, and determines whether warning"
       "messages about exceeding these limits will be printed out."))
   (:p 
    #.(one-string-sp
       "The number of lines limit applies individually to each component"
       "of your output -- all the output messages that appear as the result"
       "of PRINT (and similar) statements in your code are one component,"
       "and each value returned by executing your code is another component."
       ))
   (:p
    #.(one-string-sp
       "The initial limit for the number of lines to be shown is 500."
       "The initial limit for the number of characters shown per line is 200."
       ))
   (:p 
    #.(one-string-sp
       "The TRUNCATION-MESSAGE-MODE keyword is used to designate"
       "whether a message is to be printed when either width or"
       "number of lines truncation takes place."
       "A value of :all (the initial value and default) will cause"
       "both width and number of lines truncation messages to be printed,"
       "as appropriate."
       "A value of :width causes only a width truncation message to be printed,"
       "should any line be truncated. A message will be printed immediately"
       "below only the first line to be truncated, not subsequent ones."
       "A value of :number-of-lines causes only a number of lines truncation"
       "message to be printed should the number of lines shown be truncated."
       "The message is printed at the point of truncation."
       "A value of :none causes no truncation message to appear. (If number"
       "of lines truncation takes place, three vertical '.'s are printed at"
       "the point of truncation.)"
       ))
   )
  (:parameters
   (lines :docstring "The new limit on number of lines output."
          :parameter-type &key :default-value *output-lines-limit*
          :value-type integer)
   (chars-per-line :docstring "The new limit on line width."
                   :parameter-type &key
                   :default-value *output-chars-per-line-limit*
                   :value-type integer)
   (truncation-message-mode :docstring "The new mode for truncation messages."
                            :parameter-type &key :default-value :all 
                            :value-type keyword))
  (:see-also show-output-limits "Tools -> Prefs" set-timelimit 
   cl:*print-lines* cl:*print-pretty* cl:*print-length* cl:*print-level*)
  )

(help:document-function show-output-limits
  (:summary 
   "Shows parameters having to do with how the weblistener prints output.")
  (:returns "Used for side effect, not for return value." :type t)
  (:examples
   ((show-output-limits) :nil "Displays parameters and their values.")
   )
  (:text
   (:p
    #.(one-string-sp
       "Shows the current limits on number of lines output and"
       "number of characters per line before truncation, and displays"
       "the current truncation message mode (see SET-OUTPUT-LIMITS for a"
       "discussion of this mode)."
       ))
   )
  (:see-also set-output-limits "Tools -> Prefs" set-timelimit 
   cl:*print-lines* cl:*print-pretty* cl:*print-length* cl:*print-level*)
  )


  

(help:document-function create-workspace
  (:summary "Creates a new workspace (a package) for a user.")
  (:returns "The newly created package for the workspace." :type package
   :display-type nil)
  (:examples
   ((create-workspace :mygenes) :nil 
    "If the current user is FRED, creates the package FRED$MYGENES")
   ((create-workspace :mygenes :subpackage-of :wilma :separator "-") :nil
    "Creates the package WILMA-MYGENES"))
  (:text
   (:p 
    #.(one-string-sp
       "Workspaces are a mechanism for compartmentalizing, saving, and"
       "restoring your work. For a tutorial on using workspaces,"
       "see the 'workspaces.txt' link in the SEE-ALSO list below."))    
   (:p 
    #.(one-string-sp
       "CREATE-WORKSPACE"
       "creates a new workspace. The workspace will have a name formed"
       "by concatenating SUBPACKAGE-OF, SEPARATOR and NAME.  NAME is always"
       "uppercased. If SUBPACKAGE-OF is NIL, the workspace name is simply NAME."
       "The current package, *package*, is set to the new workspace's package"
       "which has the same name as the workspace."))
   (:p 
    #.(one-string-sp
       "The new workspace package is similar to the user's original package."
       "When the user logs in, his initial package (named for his login)"
       "is created, and uses a number of system utility packages."
       "When a workspace package is created, that package uses the same"
       "set of system utility packages, but does not inherit any"
       "functions, variable values, or other information from the user's"
       "initial package."
       ))
   )
  (:parameters
   (name 
    :docstring "A string designator used to uniquely identify the new workspace."
    :value-type (or symbol string))
   (subpackage-of 
    :docstring 
    "A name or a package used to derive the new package name"
    :parameter-type &key :default-value *package*
    :value-type (or package symbol string))
   (separator 
    :docstring "A string used to separate NAME and SUBPACKAGE-OF"
    :parameter-type &key
    :default-value "$"
    :value-type string)
   )
  (:see-also (docfile "workspaces.txt") 
   save-workspace restore-workspace
   purge-workspace-versions show-workspace show-workspace-versions
   show-available-workspaces)
  )

(help:document-function save-workspace
  (:summary "Saves the information in a workspace to disk.")
  (:returns "Used for side effect, not for return value." :type t)
  (:examples
   ((save-workspace) :nil 
    #.(one-string-sp
       "Saves workspace information to a disk file in the user's"
       "weblistener home directory whose name is based on the workspace name."))
   ((save-workspace :directory-path "workdir/") :nil
    #.(one-string-sp
       "Saves workspace information to a disk file in the 'workdir'"
       "subdirectory of the user's weblistener home directory."))
   )
  (:text
   (:p 
    #.(one-string-sp
       "Workspaces are a mechanism for compartmentalizing, saving, and"
       "restoring your work. For a tutorial on using workspaces,"
       "see the 'workspaces.txt' link in the SEE-ALSO list below."))    
   (:p 
    #.(one-string-sp
       "SAVE-WORKSPACE"
       "saves function definitions, variable values and other information"
       "found in PACKAGE (default the current package) to a disk file."
       "The information is written out in such a way that it can be"
       "restored using the LISP:LOAD function."
       ))
   (:p 
    #.(one-string-sp
       "The file the information is saved to is normally one in the"
       "user's weblistener home directory, but may be anywhere the user"
       "has write permission; specify DIRECTORY-PATH.  To save the"
       "workspace information to a subdirectory of your home directory,"
       "use a relative pathname as in the second example above."
       ))
   (:p 
    #.(one-string-sp
       "If a saved workspace already exists by the same name, the"
       "old file is 'versioned' (see the tutorial)."
       ))
   (:p 
    #.(one-string-sp
       "Note: Only certain defining forms are currently saved, along with the"
       "values of all variables.  If a form to be written out contains "
       "data that cannot be read back in, it is flagged and not written out."
       ))
   )
  (:parameters
   (package 
    :docstring "The package whose information is to be saved."
    :parameter-type &key :default-value *package*
    :value-type (or package symbol string))
   (user
    :docstring 
    #.(one-string-sp
       "The login name of the user to whose home directory the workspace is to"
       "be saved.  Normally this is your own home directory.")    
    :parameter-type &key :default-value *username*
    :value-type keyword)
   (directory-path
    :docstring 
    "A full or relative pathname specifying where the file is to be saved to."
    :parameter-type &key
    :default-value "The user's home directory"
    :value-type (or string pathname))
   (verbose?
    :docstring 
    "If T, status information about the save operation is printed out."
    :parameter-type &key
    :default-value t
    :value-type boolean)
   (version?
    :docstring 
    #.(one-string-sp
       "If T (the default), if the workspace file already exists, a new version"
       "is created.  If NIL, the old version is overwritten if it exists.")
    :parameter-type &key
    :default-value t
    :value-type boolean)
   )
  (:see-also (docfile "workspaces.txt") 
   create-workspace restore-workspace
   purge-workspace-versions show-workspace show-workspace-versions
   show-available-workspaces)
  )

(help:document-function restore-workspace
  (:summary "Loads a previously saved workspace file.")
  (:returns "Used for side effect, not for return value." :type t)
  (:examples
   ((restore-workspace) :nil "Loads a workspace file for the current package.")
   ((restore-workspace :package :massar$project1) :nil
    "Loads a workspace file for the :massar$project1 package.")
   ((restore-workspace :directory-path "workdir/") :nil 
    #.(one-string-sp
       "Loads the workspace file from the workdir/ subdirectory of the user's"
       "weblistener home directory."))
   ((restore-workspace :version 2) :nil
    "Loads the workspace version saved out two iterations previous.")
   )
  (:text
   (:p 
    #.(one-string-sp
       "Workspaces are a mechanism for compartmentalizing, saving, and"
       "restoring your work. For a tutorial on using workspaces,"
       "see the 'workspaces.txt' link in the SEE-ALSO list below."))    
   (:p 
    #.(one-string-sp
       "RESTORE-WORKSPACE"
       "loads a workspace file designated by PACKAGE and VERSION and located"
       "in USER's directory (by default the current user) or in DIRECTORY-PATH"
       "(by default the current user's home directory)."
       "If VERSION is NIL (the default) the standard workspace file will be"
       "used, otherwise VERSION should be a small positive integer, indicating"
       "the backup version to be loaded. (Use (SHOW-WORKSPACE-VERSIONS ...)"
       "to see the backup veersions available.)"))
   (:p
    #.(one-string-sp
       "If COMPILE? is T (the default) an attempt will first be made to"
       "compile the file, and if successful, load the binary."
       "The COMPILE-VERBOSE and COMPILE-PRINT options are given to COMPILE-FILE."
       "If compilation is attempted and fails, an attempt is made to load the"
       "source file.  If the workspace file (binary or source) gets loaded"
       "successfully, and if ENTER-WORKSPACE? is T, then the current package, "
       "*package*, gets set to the loaded workspace's package."
       ))
   (:p 
    #.(one-string-sp
       "The file the information is normally restored from a file in the"
       "user's weblistener home directory, but may be anywhere the user"
       "has read permission; specify DIRECTORY-PATH.  To restore the"
       "workspace information from a subdirectory of your home directory,"
       "use a relative pathname as in the third example above."
       ))
   )
  (:parameters
   (package 
    :docstring "A package name used to construct the name of the workspace file."
    :parameter-type &key :default-value *package*
    :value-type (or package symbol string))
   (version
    :docstring 
    "The particular version of the workspace to be restored."
    :parameter-type &key :default-value nil
    :value-type (or null integer))
   (user
    :docstring 
    #.(one-string-sp
       "The login name of the user from whose home directory the workspace is to"
       "be restored.  Normally this is your own home directory.")    
    :parameter-type &key :default-value *username*
    :value-type keyword)
   (directory-path
    :docstring 
    #.(one-string-sp
    "A full or relative pathname specifying where the file"
    "is to be restored from.")
    :parameter-type &key
    :default-value "the user's home directory"
    :value-type (or string pathname))
   (compile?
    :docstring "Whether to compile the file before loading it."
    :parameter-type &key
    :default-value t
    :value-type boolean)
   (enter-workspace?
    :docstring 
    "If T, the user's current package is set to the workspace package."
    :parameter-type &key
    :default-value t
    :value-type boolean)
   (verbose?
    :docstring 
    "If T, status information about the restore operation is printed out."
    :parameter-type &key
    :default-value t
    :value-type boolean)
   )
  (:see-also (docfile "workspaces.txt") 
   create-workspace save-workspace
   purge-workspace-versions show-workspace show-workspace-versions
   show-available-workspaces)
  )


(help:document-function purge-workspace-versions
  (:summary "Remove 'surplus' backup workspace versions")
  (:returns "Used for side effect, not for return value." :type t)
  (:examples
   ((purge-workspace-versions) :nil 
    "Deletes all but one backup of the current workspace")
   ((purge-workspace-versions :delete-everything? t) :nil
    "Deletes all backups and the current workspace file.")
   ((purge-workspace-versions :save-n-versions 3) :nil
    #.(one-string-sp
    "Deletes all backup versions of the current workspace except"
    "for the most recent three")))
  (:text
   (:p 
    #.(one-string-sp
       "Workspaces are a mechanism for compartmentalizing, saving, and"
       "restoring your work. For a tutorial on using workspaces,"
       "see the 'workspaces.txt' link in the SEE-ALSO list below."))    
   (:p 
    #.(one-string-sp
       "PURGE-WORKSPACE-VERSIONS"
       "removes 'surplus' backup workspace versions for workspace PACKAGE"
       "in either USER's home directory or in DIRECTORY-PATH.  By default"
       "the most recent backup version is saved, and renamed to be backup"
       "version 1.  If DELETE-EVERYTHING? is T (default NIL), then all the"
       "backup versions AND THE WORKSPACE FILE ITSELF are deleted."
       "Otherwise, SAVE-N-VERSIONS (default 1) backup versions are saved"
       "and renumbered appropriately; the rest are deleted."
       ))
   )
  (:parameters
   (package 
    :docstring "A package name used to construct the name of the workspace file."
    :parameter-type &key :default-value *package*
    :value-type (or package symbol string))
   (user
    :docstring 
    #.(one-string-sp
       "The login name of the user from whose home directory the workspace is to"
       "be restored.  Normally this is your own home directory.")    
    :parameter-type &key :default-value *username*
    :value-type keyword)
   (directory-path
    :docstring 
    #.(one-string-sp
    "A full or relative pathname specifying where the file"
    "is to be restored from.")
    :parameter-type &key
    :default-value "the user's home directory"
    :value-type (or string pathname))
   (delete-everything?
    :docstring "Whether to delete all files for the designated workspace."
    :parameter-type &key
    :default-value nil
    :value-type boolean)
   (save-n-versions
    :docstring 
    "How many backup versions to save."
    :parameter-type &key
    :default-value 1
    :value-type integer)
   (verbose?
    :docstring 
    "If T, status information about the purge operation is printed out."
    :parameter-type &key
    :default-value t
    :value-type boolean)
   )
  (:see-also (docfile "workspaces.txt") 
   create-workspace save-workspace
   restore-workspace show-workspace show-workspace-versions
   show-available-workspaces)
  )

(help:document-function show-workspace
  (:summary "Displays the contents of a workspace.")
  (:returns "Used for side effect, not for return value." :type t)
  (:examples
   ((show-workspace) :nil 
    "Prints defined functions and variables in a workspace.")
   )
  (:text
   (:p 
    #.(one-string-sp
       "Workspaces are a mechanism for compartmentalizing, saving, and"
       "restoring your work. For a tutorial on using workspaces,"
       "see the 'workspaces.txt' link in the SEE-ALSO list below."))    
   (:p 
    #.(one-string-sp
       "SHOW-WORKSPACE"
       "pretty prints the contents of workspace PACKAGE to STREAM."
       "By default, only symbols with a definition attached to them are"
       "listed, but if ALL is T, all symbols in PACKAGE are listed."
       "The listing consists of the defining form for each symbol."
       "The listing is arranged by definition type, and within definition"
       "type by SORT-BY which can be :TIME (the default), or :NAME, which"
       "arranges them alphabetically."
       "If REPORT-PROBLEMS? is T (the default), definition forms which cannot"
       "be written out in a readable form are flagged."))
   )
  (:parameters
   (package 
    :docstring "The name of the package representing the workspace."
    :parameter-type &key :default-value *package*
    :value-type (or package symbol string))
   (stream
    :docstring "Where the printout goes."
    :parameter-type &key :default-value *standard-output*
    :value-type keyword)
   (all
    :docstring 
    "If T, display all symbols, not just defined ones."
    :parameter-type &key
    :default-value nil
    :value-type boolean)
   (sort-by
    :docstring 
    "Either :time (most recently defined last) or :name (alphabetically)"
    :parameter-type &key
    :default-value :time
    :value-type keyword)
   (report-problems
    :docstring 
    "Notify about definition forms which are unwritable."
    :parameter-type &key
    :default-value t
    :value-type boolean)
   )
  (:see-also (docfile "workspaces.txt") 
   create-workspace save-workspace
   restore-workspace purge-workspace-versions show-workspace-versions
   show-available-workspaces)
  )

(help:document-function show-workspace-versions
  (:summary "Lists all the files storing versions of a workspace.")
  (:returns "Used for side effect, not for return value." :type t)
  (:examples
   ((show-workspace-versions) :nil 
    "Lists all file versions of the current workspace.")
   )
  (:text
   (:p 
    #.(one-string-sp
       "Workspaces are a mechanism for compartmentalizing, saving, and"
       "restoring your work. For a tutorial on using workspaces,"
       "see the 'workspaces.txt' link in the SEE-ALSO list below."))    
   (:p 
    #.(one-string-sp
       "SHOW-WORKSPACE-VERSIONS"
       "lists all the files storing versions of a workspace designated by"
       "PACKAGE, for a particular USER (defaulting to the current user) or"
       "in a particular DIRECTORY-PATH (defaulting to the current user's home"
       "directory).  Also shows the date/time each file was created."))
   )
  (:parameters
   (package 
    :docstring "The name of the package representing the workspace."
    :parameter-type &key :default-value *package*
    :value-type (or package symbol string))
   (user
    :docstring 
    #.(one-string-sp
       "The login name of the user from whose home directory the workspace"
       "files are to be listed from."
       "Normally this is your own home directory.")    
    :parameter-type &key :default-value *username*
    :value-type keyword)
   (directory-path
    :docstring 
    #.(one-string-sp
       "A full or relative pathname specifying where the files"
       "are to be listed from.")
    :parameter-type &key
    :default-value "the user's home directory"
    :value-type (or string pathname))
   )
  (:see-also (docfile "workspaces.txt") 
   create-workspace save-workspace
   restore-workspace purge-workspace-versions show-workspace
   show-available-workspaces)
  )

(help:document-function show-available-workspaces
  (:summary "Lists all available workspace files.")
  (:returns "Used for side effect, not for return value." :type t)
  (:examples
   ((show-available-workspaces) :nil 
    "Lists the different (non-versioned) workspace files available.")
   )
  (:text
   (:p 
    #.(one-string-sp
       "Workspaces are a mechanism for compartmentalizing, saving, and"
       "restoring your work. For a tutorial on using workspaces,"
       "see the 'workspaces.txt' link in the SEE-ALSO list below."))    
   (:p 
    #.(one-string-sp
       "SHOW-AVAILABLE-WORKSPACES"
       "shows all the workspace files in USER's toplevel directory or in"
       "DIRECTORY-PATH, along with the date each workspace file was created."
       "Only shows the non-versioned workspace files."
       "Use (SHOW-WORKSPACE-VERSIONS ...)"
       "to see the versions of a given workspace."))
   )
  (:parameters
   (user
    :docstring 
    #.(one-string-sp
       "The login name of the user from whose home directory the workspace"
       "files are to be listed from."
       "Normally this is your own home directory.")    
    :parameter-type &key :default-value *username*
    :value-type keyword)
   (directory-path
    :docstring 
    #.(one-string-sp
       "A full or relative pathname specifying where the files"
       "are to be listed from.")
    :parameter-type &key
    :default-value "the user's home directory"
    :value-type (or string pathname))
   )
  (:see-also (docfile "workspaces.txt") 
   create-workspace save-workspace
   restore-workspace purge-workspace-versions show-workspace
   show-workspace-versions)
  )













      




