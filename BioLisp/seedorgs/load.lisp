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

(defparameter *biolisp-seedorgs-files*
  '(
    "interface-info"

    "seed-defs"
    "seed-utils"
    "seed-queries"
    #+obsolete
    "seed-soap-queries"
    "seed-mysql-queries"
    "seed-common-load"
    "seed-vpl-organism-lists"

    #-:sframes
    "seed-load-organism-nsf"

    #+:sframes
    "seed-variables"

    #+:sframes
    "acache-tools"
    #+:sframes
    "seed-organism-types"
    #+:sframes
    "purge-acache-organism"
    
    #+:sframes
    "preload-seed-organisms"

    ;; #+:sframes
    ;; "process-gid-tables"
    #+:sframes
    "process-seed-organism-table"
    #+:sframes
    "seed-subsystems"
    #+:sframes
    "seed-genome-processing"
    #+:sframes
    "load-seed-genes"
    #+:sframes
    "load-seed-organism"
    #+:sframes
    "reload-gene"
    #+:sframes
    "seed-annotations"
    #+:sframes
    "seed-descriptions"

    #+:sframes
    "metagenomes"
  
    ))

(load-system* "biol:seedorgs;" *biolisp-seedorgs-files*)

(when (fboundp 'provides) (funcall 'provides :seedorgs))


