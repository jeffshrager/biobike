(in-package :com.gigamonkeys.test-tests)

(deftest always-pass () (check t))
(deftest always-fail () (check nil))
(deftest should-pass-n (n) (loop repeat n do (check t)))
(deftest should-fail-n (n) (loop repeat n do (check nil)))
(deftest should-pass-n-fail-m (n m)
  (test should-pass-n n)
  (test should-fail-n m))

