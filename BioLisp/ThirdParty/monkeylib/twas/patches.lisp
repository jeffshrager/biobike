(in-package :net.aserve)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export 'path-info))

(defvar *request-path* nil)

(defun slot-value-if-bound (object slot-name)
  (when (slot-boundp object slot-name)
    (slot-value object slot-name)))

(defmethod process-entity ((req http-request) (entity computed-entity))
  (let ((*request-path* (or (prefix entity) (slot-value-if-bound entity 'path))))
    (let ((fcn (entity-function entity)))
      (funcall fcn req entity)
      t	; processed
      )))

(defun path-info (request)
  "Return the path part of the requet URI minus the prefix used to publish the entity."
  (let ((full-path (uri-path (request-uri request))))
    (assert *request-path*)
    (when (starts-with full-path *request-path*)
      (subseq full-path (length *request-path*)))))

(defun starts-with (string prefix)
  (and
   (<= (length prefix) (length string))
   (string= string prefix :end1 (length prefix))))




