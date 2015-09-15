;;; Jeff Shrager's General Lisp Numerical Utilities
;;; Copyright (c) Jeff Shrager 1999 2000 2001 
;;; Contact: jshrager@andrew2.stanford.edu; 650.325.1521.287

;(compile-file "i:\\everyone\\jshrager\\lib\\stats.lsp")


;;; --- Various math and stats routines.

;;; Returns a string that is rounded to the appropriate number of
;;; digits, but the only thing you can do with it is print it.  It's
;;; just a convenience hack for rounding recursive lists.

(defun pround (n v)
  (if (listp v)
      (mapcar #'(lambda (v) (pround n v)) v)
    (if (numberp v)
	(format nil (format nil "~~,~af" n) v)
      v)))

;;; And some conveniences:

(defun p2 (v) (pround 2 v))
(defun p1 (v) (pround 2 v))
(defun p3 (v) (pround 3 v))
(defun p2* (l) (mapcar #'p2 l))

;;; One way t-test to see if a group differs from a numerical mean
;;; target value.  From Misanin & Hinderliter p. 248.

(defun t1-test (values target &optional (warn? t))
  (t-p-value (abs (t1-value values target)) (1- (length values)) warn?))

(defun t1-value (values target)
  (/ (- (mean values) target)
     (/ (sqrt (standard-deviation values))
	(sqrt (length values)))))

;;; Select n random sublists from a list, without replacement.  This
;;; copies the list and then destroys the copy.  N better be less than
;;; or equal to (length l).

(defun n-random (n l &aux r)
  (setq l (copy-list l))
  (dotimes (k n)
    (if (null (cdr l)) ; this has to be the last one
        (progn (push (car l) r)
	       (return nil))
      ;; If we're taking the first, it's easy, otherwise 
      ;; the first item get shoved into the middle.
      (let ((take (random (length l))))
        (if (zerop take)
	    (push (car l) r)
	  (let ((nc (nthcdr take l)))
	    (push (car nc) r)
	    (rplaca nc (car l))
	    )
	  )
	(pop l)
	) ; let
      ) ; if 
    ) ; dotimes
  r)

;;; From Misanin & Hinderliter p. 268.   The t-cdf part is inherent in 
;;; xlispstat, and I'm not entirely sure that it's really the right
;;; computation since it doens't agree entirely with Table 5 of M&H, but
;;; it's close, so I assume that M&H have round-off error.

(defun t2-test (l1 l2)
  (if (equal l1 l2) ; protect against 0 result from sdiff!
    1.0
    (t-p-value (abs (t2-value l1 l2)) (- (+ (length l1) (length l2)) 2))
    ))

(defun t2-value (l1 l2)
  (let* ((n1 (float (length l1)))
	 (n2 (float (length l2)))
	 (s21 (s2 l1 n1))
	 (s22 (s2 l2 n2))
	 (m1 (mean l1))
	 (m2 (mean l2))
 	 (s2p (/ (+ (* s21 (1- n1))
		    (* s22 (1- n2)))
		 (+ (1- n1) (1- n2))))
	 )
    (/ (- m1 m2)
       (sqrt (+ (/ s2p n1) (/ s2p n2))))
    ))

(defun s2 (l n)
  (/ (- (sum (mapcar #'(lambda (a) (* a a)) l))
	(/ (let ((r (sum l))) (* r r)) n))
     (1- n)))

;;; For non-xlispstat, we compute t-cdf the hard way.  Sometime I
;;; ought to put in a real computation, but for the moment, it's a
;;; total cheat, just returning the symbol '>.05 or '<.05.  Each
;;; Entry is a df followed by the critical point at that df.

(defvar *t-cdf-critical-points-table-for-.05*
  '((1 . 6.31)
    (2 . 2.92)
    (3 . 2.35)
    (4 . 2.13)
    (5 . 2.02)
    (6 . 1.94)
    (7 . 1.89)
    (8 . 1.86)
    (9 . 1.83)
   (10 . 1.81)
   (15 . 1.75)
   (20 . 1.72)
   (25 . 1.71)
   (30 . 1.70)
   (40 . 1.68)
   (60 . 1.67)
  (120 . 1.66)))

(defun t-p-value (x df &optional (warn? t))
  (if (> df 120)
      (progn 
	(if warn? 
	    (format t "Warning df (~a) is off the scale.  Using df=120~%" df))
	(setq df 120)))
  (dolist (tcp *t-cdf-critical-points-table-for-.05*)
    (if (or (= df (car tcp))
	    (< df (car tcp)))
	(return (if (> x (cdr tcp)) '*/p<.05! 'ns/p>.05))))
  )

;;; --- This emulates some of the xlispstat functions, and other stats
;;; utilities. 

;;; Some math ops that take many args.

(defun sum (l &aux (sum 0))
  (dolist (n l) (incf sum n)) sum)

(defun sqr (a)
  (if (numberp a) (expt a 2)
      (mapcar #'* a a)))

;;; This version of min and max operate on a list if there's only one arg.

(defun max* (l &rest ll &aux m)
  (if ll (setq l (cons l ll)))
  (setq m (pop l))
  (dolist (i l)
    (if (> i m) (setq m i)))
  m)

(defun min* (l &rest ll &aux m)
  (if ll (setq l (cons l ll)))
  (setq m (pop l))
  (dolist (i l)
    (if (< i m) (setq m i)))
  m)

;;; Warning, you can't use apply on most of the math operations
;;; because the arglist limits a too small for any large list.

(defun mean (l)
  (/ (sum l) (float (length l))))

;;; Computes a mean protected where there will be a divide by zero, and gives us n/a in that case.

(defun protected-mean (l)
  (loop with sum = 0
	with n = 0
	as value in l
	when (numberp value)
	do (incf sum value) (incf n)
	finally (return (cond ((not (zerop n)) (/ sum (float n)))
			      (t 'n/a)))))

;;; This is specially protected for zero divisors.

(defun standard-deviation (l)
  (if (null (cdr l))
      0.0
    (sqrt
     (let ((m (mean l)))
       (* (/ 1.0 (1- (length l)))
	  (sum (mapcar #'(lambda (x) (expt (- x m) 2)) l))
	  )))))

(defun standard-error (l)
  (/ (standard-deviation l) (sqrt (length l))))

;;; Lmean takes the mean of entries in a list of lists vertically.  So:
;;;  (lmean '((1 2) (5 6))) -> (3 4)  The args have to be the same length.

(defun lmean (ll)
  (let* ((n (length ll))
	 (sums (copy-list (pop ll))) ; copy so that incf doesn't wreck us!
	 (len (length sums)))
    (dolist (l ll)
      (dotimes (k len)
	(incf (nth k sums) (nth k l))
	))
    (setq n (float n))
    (mapcar #'(lambda (v) (/ v n)) sums)
    )
  )

;;; --- Two-Way Anova.  (From Misanin & Hinderliter, 1991, p. 367-) This
;;; is specialized for four groups of equal n, called by their plot
;;; location names: left1 left2 right1 right2.

(defun anova2 (a1b1 a1b2 a2b1 a2b2)
  (let* ((n (length a1b1)) ; All are the same, so any will do here.
         (npq (* 4 n)) ; This is specialized for a 2x2 design; always 4 levels.
	 (a1 (append a1b1 a1b2))
	 (suma1 (sum a1))
	 (a2 (append a2b1 a2b2))
	 (suma2 (sum a2))
	 (b1 (append a1b1 a2b1))
	 (sumb1 (sum b1))
	 (b2 (append a1b2 a2b2))
	 (sumb2 (sum b2))
	 (allscores (append a1 a2))
	 (sym1 (float (/ (sqr (sum allscores)) npq)))
	 (sym2 (float (sum (sqr allscores))))
	 (np (* n 2))
	 (sym3 (float (/ (+ (sqr suma1) (sqr suma2)) np)))
	 (nq np)
	 (sym4 (float (/ (+ (sqr sumb1) (sqr sumb2)) np)))
	 (sym5 (float (/ (+ (sqr (sum a1b1)) (sqr (sum a2b1)) 
			    (sqr (sum a1b2)) (sqr (sum a2b2)))
			 n)
		      ))
	 (ssbg (- sym5 sym1))
	 (ssa (- sym3 sym1))
	 (ssb (- sym4 sym1))
	 (ssab (+ sym1 (- (- sym5 sym4) sym3)))
	 (sswg (- sym2 sym5))
	 (sstot (- sym2 sym1))
	 (df1 3)
	 (df2 1)
	 (df3 1)
	 (df4 1)
	 (df5 (* 4 (1- n)))
	 (dftot (1- npq))
	 (msbg (float (/ ssbg df1)))
	 (msa (float (/ ssa df2)))
	 (msb (float (/ ssb df3)))
	 (msab (float (/ ssab df4)))
	 (mswg (float (/ sswg df5)))
	 )
    (list :ssbg ssbg :dfbg df1 :msbg msbg :fbg (/ msbg mswg)
	  :ssa ssa :dfa df2 :msa msa :fa (/ msa mswg)
	  :ssb ssb :dfb df3 :msb msb :fb (/ msb mswg)
	  :ssab ssab :dfab df4 :msab msab :fab (/ msab mswg)
	  :sswg sswg :dfwg df5 :mswg mswg
	  :sstot sstot :dftot dftot
	  :a1b1 a1b1 :a1b2 a1b2 :a2b1 a2b1 :a2b2 a2b2)
    ))

(defun all-squares (as bs &aux squares)
  (dolist (a as)
    (dolist (b bs)
      (push (sqr (* a b)) squares)
      )))

;;; --- Two way ANOVA with repeated measures on one dimension.  From
;;; Ferguson & Takane, 1989, p. 359.  Data is organized differently
;;; for this test.  Each group (g1 g2) contains list of all subjects'
;;; repeated measures, and same for B.  So, A: ((t1s1g1 t2s1g1 ...)
;;; (t1s2g2 t2s2g2 ...) ...)  Have to have the same number of test
;;; repeats for each subject, and this assumes the same number of
;;; subject in each group.

(defun anova2r (g1 g2)
  (let* ((c (length (car g1)))
	 (r 2) ; only two groups in this special case
	 (tsr1 (mapcar #'sum g1))
	 (t1 (sum tsr1))
	 (t1c (let (temp)
		(dotimes (n c temp)
		 (push (sum (mapcar #'(lambda (s) (nth n s)) g1)) temp))))
	 (tsr2 (mapcar #'sum g2))
	 (t2 (sum tsr2))
	 (t2c (let (temp)
		(dotimes (n c temp)
		 (push (sum (mapcar #'(lambda (s) (nth n s)) g2)) temp))))
	 (tc (mapcar #'+ t1c t2c))
	 (total (+ t1 t2))
	 (n (length g1))
	 (q1 (* (/ 1.0 c) (sum (append (sqr tsr1) (sqr tsr2)))))
	 (q2 (* (/ 1.0 (* c n)) (+ (sqr t1) (sqr t2))))
	 (q3 (* (/ 1.0 (* 2 n)) (sum (sqr tc))))
	 (q4 (* (/ 1.0 n) (sum (append (sqr t1c) (sqr t2c)))))
	 (q5 (sum (append (mapcar #'sum (mapcar #'sqr g1))
			  (mapcar #'sum (mapcar #'sqr g2)))))
	 (q6 (/ (sqr total) (* n 2.0 c)))
	 (ssbs (- q1 q6))
	 (ssr (- q2 q6))
	 (sssr (- q1 q2))
	 (ssws (- q5 q1))
	 (ssc (- q3 q6))
	 (ssrc (+ q6 (- (- q4 q2) q3)))
	 (sscsr (+ q2 (- (- q5 q1) q4)))
	 (sstotal (- q5 q6))
	 (dfr (1- r))
	 (dfc (1- c))
	 (dfrc (* (1- r) (1- c)))
	 (dfsr (* r (1- n)))
	 (dfcsr (* r (1- n) (1- c)))
	 (msr (/ ssr dfr))
	 (mssr (/ sssr dfsr))
	 (msc (/ ssc dfc))
	 (msrc (/ ssrc dfrc))
	 (mscsr (/ sscsr dfcsr))
	 (dftotal (+ dfr dfc dfrc dfsr dfcsr))
	 (fr (/ msr mssr))
	 (fc (/ msc mscsr))
	 (frc (/ ssrc mscsr))
	 )
    (if (not (= (length g1) (length g2)))
	(format t "Warning, ANOVA2R design has unbalanced cells!~%"))
    (list :ssbs ssbs :ssr ssr :sssr sssr :ssws ssws :ssc ssc 
	  :ssrc ssrc :sscsr sscsr :sstotal sstotal
	  :dfr dfr :dfsr dfsr :dfc dfc :dfrc dfrc :dfcsr dfcsr :dftotal dftotal
	  :msr msr :mssr mssr :msc msc :msrc msrc :mscsr mscsr
	  :fr fr :fc fc :frc frc)
    ) ; close let*
  )

(setq an2rd1 '( (2 7 6 7 9)
      (4 3 7 12 14)
      (7 6 4 12 10)
      (1 3 3 6 6)))
(setq an2rd2 '( (4 4 7 9 1)
      (10 12 12 12 16)
      (8 7 8 12 10)
      (5 7 6 7 8)))

;;; One way simple ANOVA, from Neter, et al. p677+.
;;; Data is give as a list of lists, each one representing a treatment, and each containing
;;; the observations.

;;; Example from Neter p.677

(setq neter677data 
      '((11 17 16 14 15) (12 10 15 19 11) (23 20 18 17) (27 33 22 26 28)))

(defun anova1 (d) ; Note that dots are replaced by dashes, so: Y.. = Y-- here
  (let* ((meanYi- (mapcar #'mean d))
	 (serrYi- (mapcar #'standard-error d))
	 (r (length d))
	 (Yi- (mapcar #'sum d))
	 (ni (mapcar #'length d))
	 (Y-- (sum Yi-))
	 (meanY-- (mean meanYi-))
	 (ntotal (sum ni))
	 (SSTO (sum (mapcar #'(lambda (treatment) 
				(sum (mapcar #'(lambda (Oij) 
						 (expt (- Oij meanY--) 2)) treatment))) d)))
	 (SSE (sum (mapcar #'(lambda (treatment meanYi-) 
			       (sum (mapcar #'(lambda (Oij) 
						(expt (- Oij meanYi-) 2)) treatment))) d meanYi-)))
	 (SSTR (sum (mapcar #'(lambda (meanYi- ni) (* ni (expt (- meanYi- meanY--) 2))) meanYi- ni)))
	 (SSTRdf (- r 1))
	 (SSEdf (- ntotal r))
	 (SSTOdf (- ntotal 1))
	 (MSTR (/ SSTR SSTRdf))
	 (MSE (/ SSE SSedf))
	 (F* (/ MSTR MSE))
	 )
	 (list :SUMMARY (format nil "F(.95,~a,~a) = ~a" SSTRdf SSEdf F*) 
	       :r r
	       :meanYi- meanYi-
	       :serrYi- serrYi-
	       :Yi- Yi- 
	       :ni ni
	       :Y-- Y--
	       :meanY-- meanY--
	       :ntotal ntotal
	       :SSTO SSTO
	       :SSE SSE
	       :SSTR SSTR 
	       :SSTRdf SSTRdf
	       :SSEdf SSEdf
	       :SSTOdf SSTOdf
	       :MSTR MSTR
	       :MSE MSE
	       :F* F*
	       )))

;;; Tukey's HSD post hoc, using info from http://vassun.vassar.edu/~lowry/ch14pt2.html
;;; Since the number of observations is always the same, we don't have to worry about
;;; using harminoc means, or any such gunk.  This does all the pairwise comparisons,
;;; between the means, telling us which comparisons are valid.  You give it the actual
;;; values and the within group MS Error, and it does the rest.  (Note that the number
;;; of treatments is (length data), which is called K in the HSD calculation, and 
;;; = the between df+1.) What you get back is each pairwise mean, and its significance
;;; level, as n/s * or **

(defun Anova1+TukeyHSD (data)
  (let* ((result (anova1 data))
	(k (getf result :r))
	(dfwg (getf result :SSEdf))
	(partial (sqrt (/ (getf result :mse) (length (car data))))) ; cheat -- assumes same n for all -- should use harmonic mean
	(q2 (loop for (dflimit . p2) in (reverse (cdr (assoc k q-table))) ; UUU wants to use the next SMALLER value...
		 until (<= dflimit dfwg)
		 finally (return p2)))
	(q05 (first q2))
	(q01 (second q2))
	)
    (setf (getf result :post-hocs-by-HSD-05) (TukeyHSD (getf result :meanYi-) (* q05 partial)))
    (setf (getf result :post-hocs-by-HSD-01) (TukeyHSD (getf result :meanYi-) (* q01 partial)))
    result))

(defun TukeyHSD (means q)
  (cond ((null means) ())
	(t (append (mapcar #'(lambda (e) (list (car means) e 
					       (if (> (abs (- (car means) e)) q) '+ '-))) 
			   (cdr means))
		   (TukeyHSD (cdr means) q)))))

;;; This is a convenience that lets you summarize the TukeyHSD results in a slightly more
;;; legible form.   It takes the plist that results from Anova1+TukeyHSD and gives you
;;; a printable form for the post hocs.

(defun pretty-TukeyHSD (result)
  (let* ((p05 (getf result :post-hocs-by-HSD-05))
	 (p01 (getf result :post-hocs-by-HSD-01))
	 )
    (loop for (m105 m205 +-05) in p05
	  as  (m101 m201 +-01) in p01
	  when (or (eq '+ +-05) (eq '+ +-01))
	  collect (list m105 m205 
			(if (eq '+ +-01) '** '*)))))

(setq q-table 
  '(
    (3 ; k=3
     (9     3.95   5.43)
     (10     3.88   5.27)
     (11     3.82   5.15)
     (12     3.77   5.05)
     (13     3.73   4.96)
     (14     3.70   4.89)
     (15     3.67   4.84)
     (16     3.65   4.79)
     (17     3.63   4.74)
     (18     3.61   4.70)
     (19     3.59   4.67)
     (20     3.58   4.64)
     (22     3.55   4.59)
     (24     3.53   4.55)
     (27     3.50   4.49)
     (30     3.49   4.45)
     (35     3.46   4.39)
     (40     3.44   4.37)
     (50     3.41   4.32)
     (60     3.40   4.28)
     (90     3.37   4.23)
     )
    (4 ; K=4
     (9     4.41   5.96)
     (10     4.33   5.77)
     (11     4.26   5.62)
     (12     4.20   5.50)
     (13     4.15   5.40)
     (14     4.11   5.32)
     (15     4.08   5.25)
     (16     4.05   5.19)
     (17     4.02   5.14)
     (18     4.00   5.09)
     (19     3.98   5.05)
     (20     3.96   5.02)
     (22     3.92   4.96)
     (24     3.90   4.91)
     (27     3.87   4.85)
     (30     3.85   4.80)
     (35     3.81   4.74)
     (40     3.79   4.70)
     (50     3.76   4.63)
     (60     3.74   4.59)
     (90     3.70   4.54)
     )
    (5
     (9     4.76   6.35)
     (10     4.65   6.14)
     (11     4.57   5.97)
     (12     4.51   5.84)
     (13     4.45   5.73)
     (14     4.41   5.63)
     (15     4.37   5.56)
     (16     4.33   5.49)
     (17     4.30   5.43)
     (18     4.28   5.38)
     (19     4.25   5.33)
     (20     4.23   5.29)
     (22     4.19   5.22)
     (24     4.17   5.17)
     (27     4.13   5.10)
     (30     4.10   5.05)
     (35     4.06   4.98)
     (40     4.04   4.93)
     (50     4.00   4.87)
     (60     3.98   4.82)
     (90     3.94   4.76)
     )
    ))

;;; --- Simple linear regression.

(defun regress (x y)
  (let* ((n (float (length x)))
	 (sumx (sum x))
	 (sumy (sum y))
	 (sumxy (sum (mapcar #'* x y)))
	 (sumx2 (sum (mapcar #'* x x)))
	 (m (/ (- (* n sumxy) (* sumx sumy))
	       (- (* n sumx2) (expt sumx 2))))
	 (b (+ (/ (* (- m) sumx) n)
	       (/ sumy n)))
	 (sumy2 (sum (mapcar #'* y y)))
	 (resids (mapcar #'(lambda (x y) (- y (+ (* x m) b))) x y))
	 (r (/ (- (* n sumxy) (* sumx sumy))
	       (sqrt (* 
		      (- (* n sumx2) (expt (sum x) 2))
		      (- (* n sumy2) (expt (sum y) 2))
		      ))))
	 (r2 (expt r 2))
	 )
    (list :m m :b b :resids resids :r r :r2 r2)))

;;; --- Correlation of two sequences, as in Ferguson & Takane, 1989,
;;; p. 125.  Assumes NO MISSING VALUES!

(defun correlate (x y)
  (if (not (= (length x) (length y)))
      (break "Can only correlate equal-sized sets."))
  (let* ((mx (mean x))
         (my (mean y))
         (devx (mapcar #'(lambda (v) (- v mx)) x))
         (devy (mapcar #'(lambda (v) (- v my)) y))
	 (sumdevxy (sum (mapcar #'* devx devy)))
	 (sumsqdevx (sum (sqr devx)))
	 (sumsqdevy (sum (sqr devy)))
	 (r (/ sumdevxy (sqrt (* sumsqdevx sumsqdevy))))
	 )
    (list :r r :r2 (sqr r))
    ))

;;; 

(defun even-power-of-two? (n)
  (zerop (mod (/ (log n) (log 2)) 1)))

;;; Normalize a vector by dividing it through by subtracting its min
;;; and then dividing through by its range (max-min).  If the numbers
;;; are all the same, this would screw up, so we check that first and
;;; just return a long list of 0.5 if so!

(defun normalize (v)
  (let* ((mx (mymax v))
	 (mn (mymin v))
	 (range (float (- mx mn)))
	 )
    (mapcar #'(lambda (i) (if (zerop range) 0.5
			      (/ (- i mn) range))) v)
    ))

;;; A dumb terminal way of plotting data.

(defun dumplot (v &optional show-values)
  (let* ((d1 (normalize v))
	 (d2 (mapcar #'(lambda (i) (* 50.0 i)) d1))
	 (min (mymin v))
	 (max (mymax v))
	 )
    (format t "~a~50t~a~%" min max)
    (dolist (i d2)
      (dotimes (k (round i))
        (format t " ")
	)
      (if show-values
	  (format t "* = ~a~%" (p2 (car v)))
	  (format t "*~%")
	  )
      (pop v) ; follow along for showing values
      )
    ))

;;; Cross mean takes a list of lists, as ((1 2 3) (4 3 2 1) ...) and
;;; produces a list with mean and standard error for each VERTICLE
;;; entry, so, as: ((2.5 . 1) ...) where the first pair is computed
;;; from the nth 1 of all the sublists in the input set, etc.  This is
;;; useful in some cases of data cruching.  Note that missing data is
;;; assumed to be always at the END of lists.  If it isn't, you've
;;; got to do something previously to interpolate.

(defun cross-mean (l &aux k r)
  (let* ((nmax (mymax (mapcar #'length l)))
	 (vs (make-array nmax)))
    (dolist (i l)
      (setq k 0)
      (dolist (v i)
	(push v (aref vs k))
	(incf k)))
    (dotimes (i nmax)
      (push (cons (mean (aref vs i))
		  (standard-error (aref vs i)))
	    r))
    (reverse r)))

;;; Macro to protect from division by zero.

(defmacro z/protect (expr testvar)
  `(if (zerop ,testvar) "[/0!]" ,expr))

;;; Take a set of values and produce a histogram binned into n groups, so that 
;;; you can get a report of the distribution of values.  There's a large 
;;; chance for off-by-one errores here!

(defun histovalues (v* &key (nbins 10))
  (let* ((min (min* v*))
	 (max (max* v*))
	 (inc (round (/ (abs (- min max)) nbins)))
	 (bins (loop with i = min
		     for n from 1 to nbins
		     collect (list i (incf i inc) 0)
		     ))
	 )
    (loop for v in v*
	  as bin = (loop for bin in bins
			 if (and (>= v (first bin))
				 (< v (second bin)))
			 do (incf (third bin))))
    bins))
  
(export '(
	  display pround p2 p1 p3 p2* t1-test t1-value n-random t2-test t2-value
	  s2 *t-cdf-critical-points-table-for-.05* t-p-value sum sqr max* min*
	  mean protected-mean standard-deviation standard-error lmean anova2
	  all-squares anova2r anova1 Anova1+TukeyHSD TukeyHSD pretty-TukeyHSD
	  regress correlate even-power-of-two?  normalize dumplot cross-mean
	  z/protect histovalues))
