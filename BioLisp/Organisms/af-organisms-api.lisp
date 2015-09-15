;;; -*- mode: Lisp; Syntax: Common-Lisp; Package: bio; -*-

(in-package :bio)

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

(eval-when (:compile-toplevel :load-toplevel :execute)

  (defparameter *organisms-api-symbols*
    '(
      ;; load-organism.lisp
      available-organisms
      *loaded-organisms*
      loaded-organisms
      preload-organisms
      load-organism
      unload-organism
      load-organisms
      pprint-organism
      add-organism-alias 
      organism
      ;; extract.lisp
      extract-gene-sequence
      extract-contig-sequence
      extract-protein-sequence
      extract-sequence
      test-sequence-extraction
      ;; utils.lisp
      contigs-of-organism
      genes-of-organism
      proteins-of-organism
      canonicalize-contig-designator
      canonicalize-gene-designator
      canonicalize-protein-designator
      canonicalize-organism-designator
      canonicalize-loaded-organism-designator
      ;; postload.lisp
      string-to-lisp-object
      string-to-double-float
      string-to-double-float-or-nil
      string-to-single-float
      string-to-single-float-or-nil
      string-to-integer
      string-to-integer-or-nil
      string-to-list
      string-to-list-or-nil
      string-to-direction
      boolean-string-to-boolean
      boolean-string-to-boolean-or-nil
      circular-string-to-t-or-nil
      ;; architecture.lisp
      fragment-of-gene
      gene-length
      overlap-between2
      ;; gene-locators.lisp
      sequence-upstream-from
      sequence-downstream-from
      gene-upstream-from
      gene-downstream-from
      overlap-between
      positional-relationship-between-two-genes
      overlapping-genes-of
      embedding-genes-of
      ))

  (export *organisms-api-symbols* (find-package :biolisp)))

