
(defun send-to-json (object)
  (.send channel (make-json-message object)))

(defun make-json-message (object)
  (make-simple-message-for-app "JSON" (.toJSONString object)))

(defun json-on-message (message)
  (let* ((text-in (@ message text-content))
	 (object (.parseJSON text-in))
	 (text-out (.toJSONString object)))
    (debug "text-in: " text-in "; object: " object "; text-out: " text-out)
    (send-to-json object)))

(defun showID ()
  (.append-child (.getElementById document "id") (.create-text-node document (+ "Channel ID: " (@ channel id)))))

(.register-message-handler channel "json" json-on-message)



