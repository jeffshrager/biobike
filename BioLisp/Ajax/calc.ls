;;; -*-lisp-*-

(defun handleOne ()
  (alert "Got one!")
  )

(defun handleTwo ()
  (alert "Got two!")
  )

(defun handleThree ()
  (.send channel (make-calc-message "test"))
  )

(defun handleFour ()
  (alert "Got four!")
  )

(defun handleFive ()
  (alert "Got five!")
  )

(defun handleSix ()
  (alert "Got Six!")
  )

(defun handleSeven ()
  (alert "Got Seven!")
  )

(defun handleEight ()
  (alert "Got eight!")
  )

(defun handleNine ()
  (alert "Got nine!")
  )

(defun handleZero ()
  (alert "Got zero!")
  )

(defun handleClear ()
  (alert "Got clear!")
  )

(defun handleEquals ()
  (alert "Got equals!")
  )

(defun handleAdd ()
  (alert "Got add!")
  )

(defun handleSub ()
  (alert "Got sub!")
  )

(defun handleMul ()
  (alert "Got mul!")
  )

(defun handleDiv ()
  (alert "Got div!")
  )

(defun setup ()
  (.send channel (make-calc-message "hello")))

(defun add-one ()
  (.send channel (make-calc-message "add")))

(defun sub-one ()
  (.send channel (make-calc-message "sub")))

(defun mul-one ()
  (.send channel (make-calc-message "mul")))

(defun div-one ()
  (.send channel (make-calc-message "div")))

(defun make-calc-message (text)
  (make-simple-message-for-app "CALC" text))

(defun update-result (text)
  (set (@ (.get-element-by-id document "result") value) text))

(defun calc-on-message (msg)
  (update-result (@ msg textContent)))

(.register-message-handler channel "calc" calc-on-message)
