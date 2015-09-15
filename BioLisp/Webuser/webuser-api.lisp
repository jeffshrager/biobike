;;; -*- mode: Lisp; Syntax: Common-Lisp; Package: webuser; -*-

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

;;; Author:  JP Massar.  

(eval-when (:compile-toplevel :load-toplevel :execute)

  (defparameter cl-user::*webuser-user-symbols*
    '(
      ;; Help functions
      help:help
      help:apropos+
      my-stuff
      my-stuff-as-a-list
      editfun
      lisp-help

      ;; Info functions
      memory-usage
      users

      ;; Process functions
      all-my-processes
      kill-my-process
      kill-my-processes
      logout
      my-sessions
      runjob
      sessions
      
      ;; Commands
      bbl
      bbl-mode
      bbl-mode?
      biolisp-mode
      biolisp-mode? 
      change-password
      ensure-bbl-mode
      hop
      lisp
      load-user-file
      redo
      result
      user-mode

      ;; Email
      set-email-address
      email-me

      ;; Miscellaneous (not in this directory, in Weblistener)
      ;; Definitions should be moved into this directory.
      clear-history 
      explain
      explain-timeout
      reload
      set-timelimit
      set-output-limits
      show-output-limits
      vpl-result

      ;; Workspace stuff
      ;; functions
      create-workspace
      ;; enable-workspace
      save-workspace
      restore-workspace
      purge-workspace-versions
      show-workspace
      show-workspace-versions
      show-available-workspaces
      
      ))

  (defparameter cl-user::*webuser-commands-api-symbols*
    
    (append 
     cl-user::*webuser-user-symbols*

     '(

      ;; Info functions
       accounts
       show-per-user-variables

       ;; Commands
       steps
       vpl
       
       ;; Process functions
       all-webuser-processes

       ;; Protocol operations
       defprotocol 
       delete-protocol
       load-protocols
       list-protocols
       publish-protocol
       save-protocols
       show-protocol

       ;; Forms
       create-aserve-form-pages

       ;; Announce facility
       announce
       announce-reboot
       clear-announcements
       delete-announcement
       minutes-until
       hours-from-now
       minutes-from-now
       seconds-from-now

       ;; Miscellaneous (not in this directory, in Weblistener)
       ;; Definitions should be moved into this directory.
       *execution-timelimit*
       *output-chars-per-line-limit*
       *output-lines-limit*
       *username*
       *sessionid*

       frame-grid
       frame-grid-filter-out
       frame-grid-add-column
       frame-grid-delete-column
       frame-grid-export-column
       frame-grid-sort

       show-color-grid

       ;; Shared module functions
       load-module
       use-module
       create-loadable-module
       find-module 

       )))

  (import cl-user::*webuser-commands-api-symbols* (find-package :webuser))
  (export cl-user::*webuser-commands-api-symbols* (find-package :webuser))

  )


