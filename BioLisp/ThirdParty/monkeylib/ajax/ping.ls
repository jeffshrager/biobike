(defvar pings 0)
(defvar pongs 0)
(defvar birthdate (new Number (new Date)))

(defun send-to-ping ()
  (let ((text (@ expr value)))
    (when (< 0 (@ text length))
      (log-ping text)
      (.send channel (make-ping-message "PING" text))
      (set (@ expr value) ""))))

(defun make-ping-message (text)
  (make-simple-message-for-app "PING" text))

(defun start-pings ()
  (set birthdate (new Number (new Date)))
  (do-ping))

(defun do-ping ()
  (log-ping ".")
  (.send channel (make-ping-message ".")))

(defun log-ping (text)
  (set (@ (.get-element-by-id document "ping") innerHTML) (+ "" pings))
  (++ pings)
  #+(or)(.append-child ping (.create-text-node document text)))

(defun log-pong (text)
  (let ((pps (* 1000 (/ pongs (- (new Number (new Date)) birthdate)))))
    (set (@ (.get-element-by-id document "pong") innerHTML) (+ "" pongs))
    (set (@ (.get-element-by-id document "persecond") innerHTML) (+ "" (/ (.round Math (* pps 100)) 100)))
    (++ pongs))
  #+(or)(.append-child pong (.create-text-node document text)))

(defun send-on-key-up (event)
  (send-to-ping))

(defun ping-on-message (message)
  (log-pong (@ message textContent))
  (do-ping))

(.register-message-handler channel "ping" ping-on-message)


