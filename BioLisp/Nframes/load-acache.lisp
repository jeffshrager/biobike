;;; -*- mode: Lisp; Syntax: Common-Lisp; Package: user; -*-

(in-package :cl-user)

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

;;; Author:  JP Massar

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *allegrocache-server* nil)
  (defvar *allegrocache-port* nil)
  )

(eval-when (:compile-toplevel :load-toplevel :execute)
  (when *acache-loaded* 
    (if (ecase cl-user::*acache-connection-mode* 
          (:single db.ac:*allegrocache*)
          (:multi *allegrocache-server*))
        (format t "Allegrocache DB already open; not opening...")
      (let ((dbpath (reverse (subseq (reverse *acache-database-dir*) 1))))
        (format t "Opening Allegrocache db ~A~%" *acache-database-dir*)
        ;; When the database is opened in either mode, allegrocache sets
        ;; the value of db.ac:*allegrocache* to the open database 
        ;; connection object
        (ecase cl-user::*acache-connection-mode* 
          (:single 
           (utils:forward-funcall 
            'db.ac:open-file-database 
            dbpath
            :if-exists :open
            :if-does-not-exist :create)
           (unless db.ac:*allegrocache* 
             (error "Internal error.  Acache not opened!")))
          (:multi 
           (utils:cformatt "Opening Allegrocache server...")
           (let ((server 
                  (utils:forward-funcall 
                   'db.ac:start-server 
                   dbpath
                   nil ;; port, allegrocache chooses a free one
                   :if-exists :open 
                   :if-does-not-exist :create
                   :authenticate nil
                   )))
             (setq *allegrocache-server* server)
             (setq *allegrocache-port* 
                   (utils:forward-funcall 'db.ac::netdb-port server))
             (utils:cformatt "Opening system allegrocache connection...")
             (utils:forward-funcall 
              'db.ac:open-network-database "localhost" *allegrocache-port*)
             (unless db.ac:*allegrocache* 
               (error 
                (one-string-nl
                 "Internal error.  Opened acache server but could not"
                 "open a connection for the system to the server running on"
                 "acache port ~S")
                *allegrocache-port*
                )))))))
    (setq *acache-running* t)
    ))
        
    

