(in-package :com.gigamonkeys.ajax)

(deftest queue ()
  (let ((q (make-instance 'threaded-queue))
	(item1 (gensym))
	(item2 (gensym))
	(item3 (gensym)))
    (check (empty-p q))
    (enqueue q item1)
    (check
      (not (empty-p q))
      (eql (peek q) item1)
      (eql (dequeue q) item1)
      (empty-p q))
    (dolist (x (list item1 item2 item3))
      (enqueue q x))
    (dolist (x (list item1 item2 item3))
      (check
	(not (empty-p q))
	(eql (peek q) x)
	(eql (dequeue q) x)))
    (check
      (empty-p q)
      (null (peek q)))
    (let ((result nil)
	  (was-blocked nil))
      (flet ((async-enqueue (delay-seconds item)
	       (mp:process-run-function "async enqueue" #'(lambda (item)
							    (mp:process-sleep delay-seconds)
							    (enqueue q item))
					item))
	     (check-blocked (delay-seconds)
	       (mp:process-run-function "check blocked" 
					#'(lambda ()
					    (mp:process-sleep delay-seconds)
					    (if (not result) (setf was-blocked t))))))
	;; fire off a thread to enqueue an item in a little bit
	(async-enqueue 1/4 item1)
	;; fire off a thread to check even sooner that the next expreession hasn't completed.
	(check-blocked 1/8)
	;; This should block until the async-enqueue does its work.
	(setf result (dequeue q)))
      (check 
	was-blocked
	(eql result item1)))))
	  

      

    


