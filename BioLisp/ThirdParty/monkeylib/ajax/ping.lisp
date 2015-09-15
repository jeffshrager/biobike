(in-package :com.gigamonkeys.ajax)

(register-message-handler :ping 'ping)

(defun ping (channel message)
  (declare (ignore message))
  (send channel '(:ping ".")))
