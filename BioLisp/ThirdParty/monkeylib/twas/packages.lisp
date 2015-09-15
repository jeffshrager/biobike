(in-package :cl-user)

#+allegro
(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :aserve)
  (require :process))

(defpackage :com.gigamonkeys.twas 
  (:use :cl #+allegro :net.aserve)
  (:export
   :*default-mime-type*
   :file-newer-p
   :maintain-thread-headroom
   :mime-type-for-path)
  #+allegro
  (:import-from :net.aserve :file-entity))


