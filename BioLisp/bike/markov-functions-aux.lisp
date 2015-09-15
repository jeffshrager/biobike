;;;; -*- mode: Lisp; Syntax: Common-Lisp; Package: bbi; -*-  

(IN-PACKAGE :BBI)


(DEFUN Markov-score 
       (results lbuf0 lbuf1 order seq start end model both-strands?)
  (DECLARE (IGNORABLE both-strands? lbuf1))
  (DECLARE (Simple-string seq))
  (DECLARE (FIXNUM start end))
  (DECLARE ((Simple-array Single-float (2)) results))
  (DECLARE (OPTIMIZE (Speed 3) (Safety 0) (Debug 0)))
  (DECLARE (Simple-string lbuf0 lbuf1))
  (SETF (AREF results 0) 1.0)
  (SETF (AREF results 1) 1.0)
  (LET ((key0 "") (key1 ""))
    (DECLARE (SIMPLE-STRING key0 key1))
    (DECLARE (IGNORABLE key1))

    (LOOP FOR forward FIXNUM FROM (1- start) BELOW (- end order)
;        FOR backward FIXNUM FROM (1- (LENGTH-SEQ)) DOWNTO (1- start)
          DO 
          (SETF key0 (SUBSEQ seq forward (+ forward order)))
          (SETF (SCHAR lbuf0 0) (SCHAR seq (+ forward order)))
          (IF (NOT (TYPEP (gethash lbuf0 (GETHASH key0 model)) 'SINGLE-FLOAT))
              (PROGN 
                (DISPLAY-DATA seq key0 lbuf0 forward order
                              (TYPE-OF (gethash lbuf0 (GETHASH key0 model))) 
                              (gethash lbuf0 (GETHASH key0 model)))
                (error "Internal error.  Markov value not single-float!")
                ))
;          (WHEN DNA?
;          (setf (schar lbuf1 0) 
;                (ecase (schar seq backward)
;                  (#\a #\t) (#\A #\T)
;                  (#\c #\g) (#\C #\G)
;                  (#\g #\c) (#\G #\C)
;                  (#\t #\a) (#\T #\A)
;                  )))

          (SETF (AREF results 0) 
                (* (AREF results 0) 
                   (TSF (GETHASH lbuf0 (GETHASH key0 model)))))
              
;       (when dna?
;          (setf (aref results 1) 
;                (* (aref results 1) 
;                   (tsf (gethash lbuf1 (aref main-table pos)))
;                    )))))
          )))

(DEFUN Markov-body 
       (sequence-set order optimized-model motif-size threshold dna?)
  (LET ((window-scores (MAKE-ARRAY 2 :ELEMENT-TYPE 'Single-float))
        (lbuf0 (MAKE-STRING 1))
        (lbuf1 (MAKE-STRING 1))
        (fthresh (FLOAT threshold 0.0))
        (variable-length (NOT motif-size))
        (results NIL))
    (DECLARE (BOOLEAN variable-length))
    (DECLARE (SINGLE-FLOAT fthresh))
    (UNLESS (TYPEP window-scores '(Simple-array Single-float (2)))
      (ERROR "Internal error: MAKE-ARRAY not working as we want!"))
    (UNLESS (TYPEP lbuf0 'Simple-string)
      (ERROR "Internal error: MAKE-STRING not returning simple string!"))
    (LOOP
     FOR item IN sequence-set
     AS label = (IF (LISTP item) 
                    (biolisp::FIRST item) 
                  item)
     AS sequence = 
     (COND
      ((LISTP item) (biolisp::SECOND item))
      ((IsFrame? item) (EXTRACT-SEQUENCE item))
      (T (ERROR "~A must be a frame or labeled sequence" item)))
     AS slen FIXNUM = (TFIX (LENGTH sequence))
     AS window-size FIXNUM = (IF variable-length slen motif-size)
     AS nwindows FIXNUM = (TFIX (1+ (TFIX (- slen window-size))))
     AS extent FIXNUM = (TFIX (1- window-size))
     DO 
     (LOOP FOR start FIXNUM FROM 1 TO nwindows
           AS end FIXNUM = (TFIX (+ start extent))
           DO (MARKOV-SCORE window-scores lbuf0 lbuf1 order sequence 
                            start end optimized-model DNA?)
           (WHEN (> (AREF window-scores 0) fthresh)
             (PUSH (LIST label start :Forward (AREF window-scores 0)) results))
           ;; (WHEN (> (AREF window-scores 1) fthresh)
           ;; (PUSH (LIST label start :Backward (AREF window-scores 1)) results))
           ))
    (SORT results '> :KEY 'FOURTH)))


(DEFMACRO LABELS-OF-SLICE (table keys)
  (IF (NOT (LISTP keys))
      (ERROR 
       "Error in LABEL-OF-SLICE!~&  Syntax is (LABEL-OF-SLICE table(indices))"
       ))
  `(APPLY 'GARRAY-COMPONENT-INDICES ,table (LIST ,@keys)))

