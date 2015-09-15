;;; -*- Package: utils; mode: lisp; base: 10; Syntax: Common-Lisp; -*-

(in-package :utils)

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

  (defparameter *utility-shell-api-symbols*
    '(defshell protected-shell-command grep-source))
     
  (export *utility-shell-api-symbols* (find-package :utils)))

#|
 
The DEFSHELL utility is used to define an interface to a Unix command. The
utility provides a mechanism to define which option flags and default values
will be given to the Unix command when it executes.  

DEFSHELL defines a Lisp function (Generally the name of the Unix command) which
can then be called to actually execute the Unix command. The caller can override
the default values of the option flags, and if permitted by the DEFSHELL
interface writer, include additional option flags and values. The caller of the
Lisp function can also provide additional keyword arguments which specify the 
execution directory, where the temporary output file is to be written, a
VERBOSE? flag and what to do if the Unix command returns a nonzero error code.  

DEFSHELL handles option flags in four distinct formats: 

  1) An option which takes no value, e.g. -i in the command "grep -i foo *.lisp"
  2) An option which takes a value with no intervening space, e.g. 
     "grep -Zfoo ..."
  3) An option which takes a value with an intervening space, e.g. 
     "grep -x xvalue foo *.lisp"
  4) An option which takes an arbitrary number of values, e.g. -f in the command
     "mycommand -f foo.lisp bar.lisp -x mumble"

DEFSHELL allows the interface specifier to deal with uppercase flags as well.
To specify an uppercase flag, the notation :^Z or :^z is used
(instead of :Z or :z).

Example DEFSHELL specifications:

(defshell grep ((:i :noarg)) 
          :allow-other-options? t 
          :command-arguments? t
          )

This specification says that a function called GREP will be defined.  
It will take a keyword argument, I, which will get translated into the string
"-i",  and it will allow the caller to specify other keywords and values which
will get translated into Unix flag specifiers in a similar manner.   

The specification also says that the caller may provide COMMAND-ARGUMENTS which
will be inserted after all the option flags.  

For example, 

(grep :b "100" :command-arguments '("foo" "*.lisp"))

will generate and execute the Unix command

% grep -i -b 100 foo *.lisp > grep.output

By default, standard output is redirected to a file called <name>.output 
(in the directory specified by OUTPUT-FILE-DIRECTORY, which defaults to the
user's BioLingua home directory).  

After the Unix command successfully executes, the Lisp command GREP will call a
function whose intent is to read the information contained in the output file
back into Lisp and/or the frame system.  The interface specifier can specify a 
function to do this by providing a value to the keyword argument 
READER-FUNCTION-NAME.  A default function is provided which simply reads the 
output file line by line and returns a list of all the lines in the output file.

The allowable option specification formats and their meanings are:

  specifier format    translation

      :i                  "-i"
      "i"                 "-i"
      :^I                 "-I"
      (:i "x")            "-i x"
      (:i "x" :nospace)   "-ix"
      (:i ("a" "b"))      "-i a b"
      
When the function is called, if a specified option value is not provided as a 
keyword argument then the default value provided or implied by the option 
specification is used. If a value is provided, it is used instead of the default
value.  

For an option specification which does not take a value
(such as the first entry above) if the keyword is provided, and its value 
is NIL, then the option will not be inserted into the command.  

e.g. (grep ((:i nil)) ...) will generate "grep ..." 

|#

(defparameter *defshell-command-immediate-keywords* 
  '(:execute? :verbose? :action-on-nonzero-return :execution-directory 
    :output-file-directory))

(defun canonicalize-defshell-arg-keyword (k)
  (cond
   ((keywordp k) k)
   ((symbolp k) (keywordize k))
   ((stringp k) 
    (cond 
     ((zerop (length k)) (error "Illegal keyword (null string)"))
     ((or (and (some 'lower-case-p k) 
               (some 'upper-case-p k))
          (not (every 'alpha-char-p k)))
      (error "Invalid keyword (not all same case or all alphabetic)"))
     ((every 'upper-case-p k) 
      (keywordize (one-string "^" (string k))))
     (t (keywordize k))
     ))
   (t (error "Invalid keyword (not a string or symbol)"))))


(defun canonicalize-defshell-arg-descriptor (d) 
  (cond 
   ((or (symbolp d) (stringp d)) 
    (canonicalize-defshell-arg-descriptor (list d)))
   ((not (listp d)) (error "Invalid format for argument descriptor: ~S" d))
   ((not (keywordp (first d))) 
    (canonicalize-defshell-arg-descriptor
     (cons (canonicalize-defshell-arg-keyword (first d)) (rest d))))
   ((= 1 (length d)) 
    (canonicalize-defshell-arg-descriptor (list (first d) t :noarg)))
   ((= 2 (length d)) 
    (let ((option (second d)))
      (cond
       ((stringp option)
        (unless (plusp (length option)) 
          (error "Invalid format for option value for descriptor: ~S" option))
        (list (first d) (second d) :standard))
       ((listp option) 
        (unless (every 'stringp option) 
          (error "Invalid format for option value for descriptor: ~S" option))
        (list (first d) (second d) :list))
       ((eq option :noarg) (list (first d) t :noarg))
       (t 
        (error "Invalid format for option value for descriptor: ~S" option)))))
   ((= 3 (length d))
    (verify-defshell-arg-value (second d) d 'defshell)
    d)))


(defun defshell-uppercase-keyword? (k) 
  (let ((s (string k)))
    (and (> (length s) 1) (eql (char s 0) #\^))))

(defun verify-defshell-arg-value (v arg-descriptor command)
  (unless 
      (ecase (third arg-descriptor)
        (:list (and (listp v) (every 'stringp v)))
        (:nospace (stringp v))
        (:noarg (or (eq v t) (null v)))
        (:standard (stringp v))
        )
    (error "~A: Invalid value ~S for option with argument descriptor ~S"
           command v arg-descriptor))
  t)



(defun split-defshell-command-args 
       (args option-descriptors command-args-allowed? unknown-allowed? command)
  (let ((known nil) (unknown nil) (command-args-value nil) (rejected nil))
    (loop for arguments on args by #'cddr 
          as arg = (first arguments) 
          as value = (second arguments) do
          (cond
           ((member arg option-descriptors :key 'first) 
            (if value (push (list arg value) known) (push arg rejected)))
           ((eq arg :command-arguments) 
            (if command-args-allowed?
                (setq command-args-value value)
              (error "~A: :COMMAND-ARGUMENTS specified but not allowed.")))
           ((member arg *defshell-command-immediate-keywords*) nil)
           (unknown-allowed? (push (list arg value) unknown))
           (t 
            (error "~A: Unknown option ~S. Additional options are not allowed."
                   command arg))
           ))
    (let ((default-args nil))
      (loop for (key value nil) in option-descriptors do
            (unless (or (member key rejected) (find key known :key 'first))
              (push (list key value) default-args)
              ))
      (values (append known default-args) unknown command-args-value) 
      )))

(defun create-defshell-command
       (name 
        arg-descriptors 
        known-arg-values
        unknown-arg-values
        command-arg-values
        output-specifications
        output-file-directory
        execution-directory
        prefix
        suffix)
  (let* ((username 
          (and (find-package :wb) (symbol-package-value :*username* :wb)))
         (vdir
          (and username (forward-package-funcall :visitor-directory username)))
         (edir (or execution-directory vdir))
         (odir 
          (or output-file-directory vdir 
              (and (boundp 'user::*tmp-directory*) user::*tmp-directory*))))
    (one-string 
     (if edir (one-string "cd " (namestring execution-directory) " ; ") "")
     (if prefix (one-string prefix " ; ") "")
     (string-downcase name) " "
     (known-args-to-string arg-descriptors known-arg-values)
     (unknown-args-to-string unknown-arg-values)
     (if command-arg-values 
         (one-string (string-join command-arg-values #\Space) " ")
       "")
     " > " 
     (if odir 
         (namestring (merge-pathnames (first output-specifications) odir))
       (first output-specifications))
     " " 
     (if suffix (one-string " ; " suffix) "") 
     )))
   
(defun key-to-option-string (key)
  (one-string 
   "-" 
   (if (defshell-uppercase-keyword? key)
       (subseq (symbol-name key) 1)
     (string-downcase (symbol-name key)))))

(defun known-arg-to-string (descriptor value)
  (let ((option-string (key-to-option-string (first descriptor))))
    (one-string 
     (ecase (third descriptor) 
       (:standard (one-string option-string " " value))
       (:noarg (one-string option-string " "))
       (:nospace (one-string option-string value))
       (:list (one-string option-string " " (string-join value #\Space))))
     " ")))
      
(defun known-args-to-string (descriptors values)
  (let ((s ""))
    (loop for (key value) in values do 
          (let ((descriptor (find key descriptors :key 'first)))
            (setq s (one-string s (known-arg-to-string descriptor value)))
            ))
    s))

(defun unknown-args-to-string (values)
  (let ((s ""))
    (loop for (key value) in values do 
          (setq s (one-string
                   s 
                   (key-to-option-string key)
                   (cond 
                    ((null value) " ")
                    ((stringp value) (one-string " " value " "))
                    ((listp value) 
                     (unless (every 'stringp value) 
                       (error "List of options contains non-string(s)"))
                     (one-string (string-join value #\Space) " "))
                    (t (error "Illegal value for option ~S" key))
                    ))))
                     
    s))

(defun canonicalize-defshell-output-specifications (s)
  (ensure-list s))

(defun option-descriptors-to-declare-ignores (d)
  (loop for (key nil nil) in d 
        as keyarg = (intern (string key) :utils)
        collect 
        `(declare (ignorable ,keyarg))
        ))
 
(defun canonicalize-defshell-name (name)
  (cond
   ((symbolp name) (values name (string-downcase name)))
   ((listp name) (values (first name) (second name)))
   (t (error "Invalid name argument to DEFSHELL: ~S" name))))


(defmacro defshell 
          (name 
           option-descriptions 
           &key
           (prefix-command-generator nil)
           (suffix-command-generator nil)
           (allow-other-options? nil)
           (command-arguments? nil)
           (output-specifications nil)
           (reader-function-name 'default-defshell-output-reader)
           &aux lisp-function-name shell-command-name
           )
  (multiple-value-setq (lisp-function-name shell-command-name)
      (canonicalize-defshell-name name))
  (setq option-descriptions 
        (mapcar 
         'canonicalize-defshell-arg-descriptor 
         option-descriptions))
  (when (null output-specifications)
    (setq output-specifications 
          (one-string (string-downcase shell-command-name) ".output")))
  (setq output-specifications 
        (canonicalize-defshell-output-specifications output-specifications))
  `(defun ,lisp-function-name  
          (&rest 
           args
           &key
           (execute? nil)
           (verbose? t)
           (valid-return-codes (list 0))
           (action-on-invalid-return :error)
           (execution-directory nil)
           (output-file-directory nil)
           ,@(option-descriptors-to-keyword-args option-descriptions)
           ,@(when allow-other-options? (list '&allow-other-keys))
           )
     ,@(option-descriptors-to-declare-ignores option-descriptions)
     (let ((known-descriptors ',option-descriptions) 
           (output-descriptors ',output-specifications))
       (multiple-value-bind (known-args unknown-args command-args)
           (split-defshell-command-args 
            args known-descriptors
            ,command-arguments? ,allow-other-options?
            ',lisp-function-name)
         (loop for (key value) in known-args 
               as descriptor = (find key known-descriptors :key 'first) do
               (verify-defshell-arg-value value descriptor ',lisp-function-name)
               )
         (let ((shell-command 
                (create-defshell-command 
                 ,shell-command-name known-descriptors 
                 known-args unknown-args command-args
                 output-descriptors
                 output-file-directory
                 execution-directory
                 ,(when prefix-command-generator `(,prefix-command-generator))
                 ,(when suffix-command-generator `(,suffix-command-generator))
                 )))
           (if execute? 
               (unwind-protect 
                   (progn 
                     (when verbose? (cformatt "Command: ~S" shell-command))
                     (protected-shell-command 
                      shell-command
                      :valid-return-codes valid-return-codes
                      :action-on-invalid-return action-on-invalid-return)
                     (when verbose? 
                       (cformatt 
                        "Calling output reader ~S" ',reader-function-name))
                     (apply ',reader-function-name 
                            :output output-descriptors args))
                 (ignore-errors 
                   (delete-file (first output-descriptors))))
             (cformatt "Command: ~S" shell-command))
           )))))
         
             

(defun option-descriptors-to-keyword-args (descriptors)
  (loop for (key value type) in descriptors
        as keyarg = (intern (string key) :utils)
        collect 
        (ecase type  
          (:standard `(,keyarg ,value))
          (:nospace `(,keyarg ,value))
          (:noarg `(,keyarg ,value))
          (:list `(,keyarg (list ,@value)))
          )))

(defun default-defshell-output-reader (&key output &allow-other-keys)
  (with-open-file (s (first output) :direction :input :if-does-not-exist :error)
    (loop for line = (read-line s nil nil) 
          until (null line) 
          collect line
          )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      
               
(defparameter *use-sc-with-timeout* t)

(defun protected-shell-command
       (command-string 
        &key 
        (valid-return-codes (list 0))
        (action-on-invalid-return :print)
        (directory nil)
        (exec? t)
        (keys-for-subcall '(:wait t) kfs-provided?))
  #.(one-string-nl
     "Run a shell command and take an action upon an invalid return code."
     "Invalid return codes are any codes other than those provided by"
     "VALID-RETURN-CODES (default 0)."
     "The :ACTION-ON-INVALID-RETURN keyword value (default :print) can be "
     ":PRINT = just display a message, don't error out; or"
     ":ERROR = display message an error out, or nil = ignore the error."
     "Note that keyword args to EXCL::RUN-SHELL-COMMAND must be provided"
     "as a list using the keyword :KEYS-FOR-SUBCALL (default '(:wait t)). "
     )
  (if *use-sc-with-timeout* 
      (progn 
        (when kfs-provided? 
          (error "Cannot use keys-for-subcall when timeout enabled."))
        (forward-package-funcall 
         :bio :shell-command-with-timeout command-string 
         :valid-return-codes valid-return-codes 
         :action-on-invalid-return action-on-invalid-return
         :directory directory
         :exec? exec?))
    (let ((result (apply #'cl-user::run-shell-command 
                         (cons command-string 
                               (append `(:directory ,directory) 
                                       keys-for-subcall)))))
      (if (member result valid-return-codes)
          result
        (case action-on-invalid-return
          (:print 
           (cformatt "The command ~s returned ~d!" command-string result)
           result)
          (:error (error "The command ~s returned ~d!" command-string result))
          (nil result))))))

