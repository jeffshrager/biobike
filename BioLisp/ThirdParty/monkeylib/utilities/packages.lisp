;;
;; Copyright (c) 2005, Gigamonkeys Consulting All rights reserved.
;;

(in-package :cl-user)

(defpackage :com.gigamonkeys.utilities
  (:use :cl :com.gigamonkeys.macro-utilities)
  (:export
   :make-heap
   :heap>
   :heap-pop
   :heap-peek
   :heap-push
   :hh-mm-ss
   :empty-heap
   :file-text
   :with-time
   :with-current-time
   :lisp-time-zone
   :iso-8601-time-zone
   :format-iso-8601-time
   :make-time
   :merge-time
   :now
   :&zone
   :date/time->utc
   :time->utc
   :make-timer
   :shutdown-timer
   :schedule-event
   :cancel-event
   :shuffle-vector
   :nshuffle-vector))