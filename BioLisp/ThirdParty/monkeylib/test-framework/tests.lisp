(in-package :com.gigamonkeys.test)

(deftest pass ()
  (multiple-value-bind (ok passes failures)
      ;; ignore-results keeps results of (test pass) being recorded as
      ;; part of this test function. But TEST still returns the
      ;; results accumulated by running the test function.
      (ignore-results (test com.gigamonkeys.test-tests::always-pass))
    (check ok (= passes 1) (= failures 0))))

(deftest fail ()
  (multiple-value-bind (ok passes failures)
      (ignore-results (test com.gigamonkeys.test-tests::always-fail))
    (check (not ok) (= passes 0) (= failures 1))))

(deftest multiple-passes ()
  (multiple-value-bind (ok passes failures)
      (ignore-results (test com.gigamonkeys.test-tests::should-pass-n 10))
    (check ok (= passes 10) (= failures 0))))

(deftest multiple-failures ()
  (multiple-value-bind (ok passes failures)
      (ignore-results (test com.gigamonkeys.test-tests::should-fail-n 10))
    (check (not ok) (= passes 0) (= failures 10))))

(deftest mixed-results ()
  (multiple-value-bind (ok passes failures)
      (ignore-results
	(test com.gigamonkeys.test-tests::should-pass-n-fail-m 10 13))
    (check (not ok) (= passes 10) (= failures 13))))

(deftest test-package ()
  (multiple-value-bind (ok passes failures)
      (ignore-results (test-package :print nil :summary nil :package :com.gigamonkeys.test-tests))
    (check (not ok) (= passes 1) (= failures 1))))

