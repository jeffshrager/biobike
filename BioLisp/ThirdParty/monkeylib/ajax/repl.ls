;;; BUGS: if you hit return twice, quickly enough, the same expression
;;; may be sent to be evaluated twice. (This is because
;;; collect-expression doesn't do anything to mark the expression as
;;; having already beeen collected once. Perhaps should invalidate
;;; last-prompt.))

(defvar package "CL-USER")
(defvar last-prompt false)
(defvar open-parens 0)
(defvar flashed-paren false)

(defvar repl-history (new Array))
(defvar repl-history-cursor 0)

(.unshift repl-history "")

(defun print-prompt ()
  (cond
    (last-prompt
     (.append-child repl (make-prompt))
     (.remove-child repl cursor)
     (.append-child repl cursor))
    (true
     (.insert-before repl (make-prompt) cursor))))

(defun collect-text (e)
  (let* ((code (@ e which))
	 (text (.from-char-code String code))
	 (to-insert (.create-text-node document text)))

    ;; Let through all meta keys for browser. May want to steal some
    ;; of these later.
    (when (@ e meta-key) (return-from collect-text true))

    (when flashed-paren (unflash))
    (when (string= text "(") (++ open-parens))
    (when (string= text ")") 
      (-- open-parens)
      (let ((open-paren (find-open-paren)))
	(when open-paren (flash open-paren))))
    (when (or (= code 8) (and (string= text "h") (@ e ctrl-key)))
      (delete-one-character)
      (return-from collect-text false))
    (cond
      ((or (= code 10) (= code 13))
       (cond
	 ((= open-parens 0)
	  (send-to-repl (collect-expression)))
	 (true 
	  (.append-child repl (.create-text-node document " "))
	  (.append-child repl (.create-element document "BR"))
	  (.remove-child repl cursor)
	  (.append-child repl cursor)))))
    (.insert-before repl to-insert cursor))
  false)

(defun keyup-handler (e)
  (let ((code (@ e which)))
    ;; This is a bit extreme--sending a new request on essentially
    ;; every keystroke. Could probably be smarter than that.
    (let ((name (new-open-function-name (collect-expression))))
      (if (and name (not (or (= code 10) (= code 13))))
	  (send-arglist-request name)
	  (clear-arglist-display)))
    (cond
      ((= code 38) ;; up arrow
       (when (< 0 (@ repl-history length))
	 (set repl-history-cursor (% (1+ repl-history-cursor) (@ repl-history length)))
	 (replace-current-expression (ref repl-history repl-history-cursor))))
      ((= code 40) ;; down arrow
       (when (< 0 (@ repl-history length))
	 (if (= 0 repl-history-cursor)
	     (set repl-history-cursor (1- (@ repl-history length)))
	     (set repl-history-cursor (1- repl-history-cursor)))
	 (replace-current-expression (ref repl-history repl-history-cursor)))))
    false))

(defun delete-one-character ()
  (let ((to-remove (@ cursor previous-sibling)))
    (cond 
      ((= (@ to-remove node-type) 3)
       (cond
	 ((= (@ to-remove length) 1)
	  (cond
	    ((string= (@ to-remove text-content) "(")
	     (-- open-parens))
	    ((string= (@ to-remove text-content) ")")
	     (++ open-parens)))
	  (.remove-child repl to-remove))
	 (true
	  (cond
	    ((string= (.substring-data to-remove (1- (@ to-remove length)) 1) "(")
	     (-- open-parens))
	    ((string= (.substring-data to-remove (1- (@ to-remove length)) 1) ")")
	     (++ open-parens)))
	  (.delete-data to-remove (1- (@ to-remove length)) 1))))
      ((and (= (@ to-remove node-type) 1)
	    (string= (@ to-remove tag-name) "BR"))
       (.remove-child repl to-remove))
      (true ;; ignore
       ))))


(defun flash (text)
  (let ((before (@ text next-sibling)))
    (.remove-child repl text)
    (let ((span (.create-element document "SPAN")))
      (.set-attribute span "class" "flash")
      (.append-child span text)
      (.insert-before repl span before)
      (set flashed-paren span))))

(defun unflash ()
  (let ((before (@ flashed-paren next-sibling))
	(text (@ flashed-paren firstChild)))
    (.remove-child repl flashed-paren)
    (.insert-before repl text before)
    (set flashed-paren false)))

(defun replace-current-expression (text)
  (unless (eq (@ last-prompt next-sibling) cursor)
    (do* ((node (@ last-prompt next-sibling) next-sib)
	  (next-sib (@ node next-sibling) (@ node next-sibling)))
	((eq node cursor))
      (.remove-child repl node)))
  (set open-parens 0)
  (dotimes (i (@ text length))
    (let ((char (.substr text i 1)))
      (when (string= char "(") (++ open-parens))
      (when (string= char ")") (-- open-parens))
      (.insert-before repl (.create-text-node document char) cursor))))

(defun collect-expression ()
  (let ((expression ""))
    (do ((node (@ last-prompt next-sibling) (@ node next-sibling)))
	((eq node cursor) expression)
      (+= expression (@ node text-content)))))

(defun find-open-paren ()
  (let ((count 1))
    (do ((node (@ cursor previous-sibling) (@ node previous-sibling)))
	((eq node last-prompt) false)
      (when (= (@ node node-type) 3)
	(cond
	  ((= (@ node length) 1)
	   (cond
	     ((string= (@ node text-content) "(")
	      (-- count)
	      (if (= 0 count)
		(return-from find-open-paren node)))
	     ((string= (@ node text-content) ")")
	      (++ count))))
	  (true
	   (cond
	     ((string= (.substring-data node (1- (@ node length)) 1) "(")
	      (-- count)
	      (if (= 0 count)
		(debug "In multi-character text.")))
	     ((string= (.substring-data node (1- (@ node length)) 1) ")")
	      (++ count)))))))))



(defun send-to-repl (text)
  (when (< 0 (@ text length))
    (set (ref repl-history 0) text)
    (.unshift repl-history "")
    (set repl-history-cursor 0)
    (.send channel (make-eval-message text))
    (clear-arglist-display)))

(defun send-arglist-request (name)
  (.send channel (make-arglist-request-message name)))

(defun make-eval-message (to-eval)
  (let* ((message (.create-document (@ document implementation) "" "REPL" null))
	 (payload (.create-element message "EVAL")))
    (.append-child payload (.create-text-node message to-eval))
    (.append-child (@ message document-element) payload)
    message))

(defun make-arglist-request-message (name)
  (let* ((message (.create-document (@ document implementation) "" "REPL" null))
	 (payload (.create-element message "ARGLIST")))
    (.append-child payload (.create-text-node message name))
    (.append-child (@ message document-element) payload)
    message))

(defun clear-arglist-display ()
  (set (@ arglists innerHTML) "&nbsp;"))

(defun log-repl-input (text)
  (.append-child last-prompt-paragraph (.create-text-node document text)))

(defun log-repl-output (text)
  (.append-child repl (make-output text)))

(defun log-repl-result-or-error (element)
  (.append-child repl element)
  (print-prompt)
  (+= (@ repl scrollTop) 4000000))

(defun log-repl-result (text)
  ;;(debug "got result: " (@ text length))
  (log-repl-result-or-error (make-result text "result")))

(defun log-repl-error (text)
  (log-repl-result-or-error (make-result (+ "Error: " text) "error")))

(defun log-read-error (text)
  (log-repl-result-or-error (make-result (+ "Read Error: " text) "error")))

(defun make-prompt ()
  (let ((prompt (.create-element document "span")))
    (.set-attribute prompt "class" "prompt")
    (.append-child prompt (.create-text-node document (+ package "> ")))
    (set last-prompt prompt)
    prompt))

(defun make-cursor ()
  (set cursor (.create-element document "span"))
  (.set-attribute cursor "class" "cursor")
  (.append-child cursor (.create-text-node document "_"))
  cursor)

(defun open-function-name ()
  (let* ((text (@ expr value))
	 (length (@ text length)))
    (let ((open 1)
	  (lparen-position -1))
      (do* ((j (1- (@ expr selection-start)) (- j 1))
	    (ch (.charAt text j) (.charAt text j)))
	   ((< j 0))
	(when (equal ch #\)) (++ open))
	(when (equal ch #\() 
	  (-- open)
	  (when (= open 0)
	    (set lparen-position j)
	    (return undefined))))
      (when (= lparen-position -1)
	(return-from open-function-name false))
      (do* ((j lparen-position (+ j 1))
	    (ch (.charAt text j) (.charAt text j)))
	   ((= j (@ text length)) "")
	(when (equal ch #\Space)
	  (return-from open-function-name
	    (.substring text (+ 1 lparen-position) j)))))))

(defun new-open-function-name (text)
  (let* ((length (@ text length)))
    (let ((open 1)
	  (lparen-position -1))
      (do* ((j (1- length) (- j 1))
	    (ch (.charAt text j) (.charAt text j)))
	   ((< j 0))
	(when (equal ch #\)) (++ open))
	(when (equal ch #\() 
	  (-- open)
	  (when (= open 0)
	    (set lparen-position j)
	    (return undefined))))
      (when (= lparen-position -1)
	(return-from new-open-function-name false))
      (do* ((j lparen-position (+ j 1))
	    (ch (.charAt text j) (.charAt text j)))
	   ((= j (@ text length)) "")
	(when (equal ch #\Space)
	  (return-from new-open-function-name
	    (.substring text (+ 1 lparen-position) j)))))))

(defun make-output (text)
  (let ((p (.create-element document "p"))
	(pre (.create-element document "pre")))
    (.set-attribute p "class" "output")
    (.append-child pre (.create-text-node document text))
    (.append-child p pre)
    p))

;;; There's a limit, apparently on how big a text node you can create,
;;; so we break the result up into chunks.
(defun make-result (text class)
  (let ((p (.create-element document "p")))
    (.set-attribute p "class" class)
    (do ((start 0 end)
	 (end (.min Math 256 (@ text length)) (.min Math (+ end 256) (@ text length))))
	(false)
      (let ((substr (.substring text start end)))
	(.append-child p (.create-text-node document substr)))
      (when (>= end (@ text length))
	(return undefined)))
    p))

(defun repl-on-message (message)
  ;; skip empty text foo
  (while (= (@ message node-type) 3)
    (set message (@ message next-sibling)))
  (let ((message-type (@ message nodeName)))
    (cond
      ((string= message-type "package-change")
       (set package (@ message textContent)))
      ((string= message-type "input")
       #+(or)(log-repl-input (@ message text-content)))
      ((string= message-type "result")
       (log-repl-result (@ message text-content)))
      ((string= message-type "output")
       (log-repl-output (@ message textContent)))
      ((string= message-type "error")
       (log-repl-error (@ message textContent)))
      ((string= message-type "read-error")
       (log-read-error (@ message textContent)))
      ((string= message-type "arglist")
       (set (@ arglists innerHTML) (@ message textContent)))
      (true
       (debug "Unrecognized message type: " message-type " with conent: " (@ message textContent))))))


(.register-message-handler channel "repl" repl-on-message)
(set (@ document onkeypress) collect-text)
(set (@ document onkeyup) keyup-handler)
