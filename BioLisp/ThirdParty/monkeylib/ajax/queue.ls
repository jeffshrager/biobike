;; Lispscript

(defun Queue ()
  (set (@ this items) (new Array))
  this)

(defmethod (Queue enqueue) (msg) (.push (@ this items) msg))

(defmethod (Queue dequeue) () (.shift (@ this items)))

(defmethod (Queue depth) () (@ this items length))

(defmethod (Queue empty-p) () (= 0 (.depth this)))

(defmethod (Queue peek) () (ref (@ this items) 0))

