(in-package :com.gigamonkeys.ajax)

(register-message-handler :json 'json-on-message)

(defun send-json-test (object)
  (send *newest-channel* `(:json ,(com.gigamonkeys.ajax.json:json object))))

(defun json-on-message (channel message)
  (declare (ignore channel))
  (destructuring-bind (jsontag &rest payload) message
    (assert (eql jsontag :json))
    (let ((expr (format nil "~{~a~}" payload)))
      (format t "Text-in: ~s; object: ~s~%" expr (nth-value 1 (com.gigamonkeys.ajax.json-parser::parse-json expr))))))
