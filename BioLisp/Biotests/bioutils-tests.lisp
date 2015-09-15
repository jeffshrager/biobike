;;; -*- Package: bio; mode: lisp; base: 10; Syntax: Common-Lisp; -*-

;;; Author: JP Massar.

(in-package :bio)

(tests:deftest prbti1 (prbti nil 20 1 3 5 7) '(:standard :order-forward)
               :comparison 'equal :chapter :bioutils)

(tests:deftest prbti2 (prbti t 20 1 3 5 7) '(:standard :order-forward)
               :comparison 'equal :chapter :bioutils)

(tests:deftest prbti3 (prbti t 20 1 3 2 7) '(:overlapped :order-forward)
               :comparison 'equal :chapter :bioutils)

(tests:deftest prbti4 (prbti t 20 2 3 2 7) '(:within :order-forward)
               :comparison 'equal :chapter :bioutils)

(tests:deftest prbti5 (prbti t 20 18 5 7 9) '(:standard :order-backward)
               :comparison 'equal :chapter :bioutils)

(tests:deftest prbti6 (prbti t 20 18 5 3 7) '(:overlapped :order-backward)
               :comparison 'equal :chapter :bioutils)

(tests:deftest prbti7 (prbti t 20 18 5 15 20) '(:overlapped :order-backward)
               :comparison 'equal :chapter :bioutils)

(tests:deftest prbti8 (prbti t 20 5 10 5 6) '(:within :order-forward)
               :comparison 'equal :chapter :bioutils)

(tests:deftest prbti9 (prbti t 20 5 10 6 8) '(:within :order-forward)
               :comparison 'equal :chapter :bioutils)

(tests:deftest prbti10 (prbti t 20 15 5 18 2) '(:within :order-forward)
               :comparison 'equal :chapter :bioutils)

(tests:deftest prbti11 (prbti t 20 15 5 18 10) '(:overlapped :order-forward)
               :comparison 'equal :chapter :bioutils)

(tests:deftest prbti12 (prbti t 20 15 5 15 5) '(:identical :order-forward)
               :comparison 'equal :chapter :bioutils)

(tests:deftest prbti13 (prbti t 20 15 14 2 3) '(:within :order-backward)
               :comparison 'equal :chapter :bioutils)

(tests:deftest prbti14 (prbti t 20 15 14 19 18) '(:within :order-forward)
               :comparison 'equal :chapter :bioutils)

(tests:deftest prbti15 (prbti t 20 15 5 6 9) '(:adjacent :order-backward)
               :comparison 'equal :chapter :bioutils)

(tests:deftest prbti16 (prbti t 20 15 5 10 14) '(:adjacent :order-backward)
               :comparison 'equal :chapter :bioutils)

(tests:deftest prbti17 (prbti t 20 10 14 15 5) '(:adjacent :order-forward)
               :comparison 'equal :chapter :bioutils)

(tests:deftest prbti18 (prbti t 20 15 5 6 14) '(:adjacent :order-backward)
               :comparison 'equal :chapter :bioutils)

(tests:deftest prbti19 (prbti nil 20 2 5 1 2) '(:overlapped :order-backward)
               :comparison 'equal :chapter :bioutils)

(tests:deftest prbti20 (prbti nil 20 5 10 3 4) '(:adjacent :order-backward)
               :comparison 'equal :chapter :bioutils)

(tests:deftest prbti21 (prbti nil 20 3 4 5 10) '(:adjacent :order-forward)
               :comparison 'equal :chapter :bioutils)

(tests:deftest prbti22 (prbti nil 20 1 20 1 20) '(:identical :order-forward)
               :comparison 'equal :chapter :bioutils)

(tests:deftest prbti23 (prbti nil 20 3 5 3 5) '(:identical :order-forward)
               :comparison 'equal :chapter :bioutils)

(tests:deftest prbti24 (prbti nil 20 5 15 7 10) '(:within :order-forward)
               :comparison 'equal :chapter :bioutils)

(tests:deftest prbti25 (prbti nil 20 7 10 5 15) '(:within :order-backward)
               :comparison 'equal :chapter :bioutils)

(tests:deftest prbti26 (prbti nil 20 7 10 5 15) '(:within :order-backward)
               :comparison 'equal :chapter :bioutils)

(tests:deftest prbti27 (prbti nil 20 7 10 9 10) '(:within :order-forward)
               :comparison 'equal :chapter :bioutils)

(tests:deftest prbti28 (prbti nil 20 1 20 1 3) '(:within :order-forward)
               :comparison 'equal :chapter :bioutils)




