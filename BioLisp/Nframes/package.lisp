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

;;; Authors:  JP Massar, Mike Travers.


(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :aserve))

(defpackage :frames
  (:nicknames :aframe :aframes :frame)
  (:use :wlisp :utils :net.html.generator :net.aserve)
  )

(in-package :aframes)

(eval-when (:compile-toplevel :load-toplevel :execute)

  (defparameter *frames-api-symbols*
    '(
      ;; defs 
      ;; necessary to be able to write a method on a frame object type
      %aframe 
      *after-frame-creation-hook*
      *after-frame-modification-hook*
      
      ;; reader
      valid-frame-char?

      ;; iterators
      with-new-frame-table
      with-alternative-frame-table
      with-frames-iterated
      for-all-frames		
      mapframes
      nmapframes
      make-temp-frame
      for-each-frame-slot
      mapslots
      slots-and-values

      ;; primitives
      frame-fnamed
      intern-frame
      unintern-frame
      purge-frame
      genframe
      framep
      isframe?
      resize-frame-table
      copy-system-frame-table
      frame-has-slot?
      slot-of-frame?
      frame-slots-of		
      fff
      frames-equalp
      make-frame-instance

      ;; reader
      read-fname       ; used by PRINC-WITH-FRAME-LINKS
      saved-standard-readtable
      frames-readtable
      framedcl			

      ;; accessors
      *set-valued-non-list-action*
      fname
      fpname
      fstring
      slotv
      delete-frame
      delete-slot
      frame-slot-value
      slot-value-of-frame
      describe-frame
      df
      def-frame			
      rename-frame
      add-element
      delete-element
      has-element?
      delete-frame-contents

      ;; special-slots
      defslot
      def-computed-slot
      def-always-computed-slot
      make-computed-slot
      def-inverse-slot
      def-inherited-slot
      compute-transitive-slot
      def-transitive-slot
      make-temp-frame
      top-level-frame?
      make-top
      
      ;; def-frame-class 
      def-frame-class
      frame-class-frame

      ;; indexed-slots
      def-indexed-slot
      slot-lookup

      ;; utils
      canonicalize-frame-designator
      create-valid-frame-name
      ->frames
      all-similarly-named-frames
      similarly-named-frames
      frame->all-similarly-named-frames
      frame->related-and-containing-frames
      find-frames
      google-frames
      googleplex-frames
      search-frames
      with-temp-frames
      frameloop
      frame-loop
      with-temporary-slot

      ;; dumper
      dump-user-frames
      retrieve-user-frames
      frame-dump-exists?
      purge-user-frames

      ))

  (export *frames-api-symbols* (find-package :aframes)))


