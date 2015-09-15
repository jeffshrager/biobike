(defvar pass 0)
(defvar fail 0)

(defun dotest (ls js result expected note)
  (let* ((results (.get-element-by-id document "results"))
	 (row (.insert-row results (@ results rows length))))
    (set (@ (.insert-cell row (@ row cells length)) innerHTML) ls)
    (set (@ (.insert-cell row (@ row cells length)) innerHTML) js)
    (set (@ (.insert-cell row (@ row cells length)) innerHTML) (display-string expected))
    (set (@ (.insert-cell row (@ row cells length)) innerHTML) (display-string result))

    (cond
      ((eq result expected)
       (set (@ (.insert-cell row (@ row cells length)) innerHTML) "Pass")
       (set (@ row style background) "#aaffaa")
       (++ pass)
       (set (@ (.get-element-by-id document "passes") innerHTML) pass))
      (true
       (set (@ (.insert-cell row (@ row cells length)) innerHTML) "Fail")
       (set (@ row style background) "#ffaaaa")
       (++ fail)
       (set (@ (.get-element-by-id document "failures") innerHTML) fail)))))

(defun all-tests-done ()
  (set (@ (.get-element-by-id document "done") innerHTML) "All tests completed."))

(defun display-string (obj)
  (if (and obj (equal (@ obj constructor) String))
      (+ "\"" obj "\"")
      (+ "" obj)))