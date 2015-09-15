;;; -*- Package: utils; mode: lisp; base: 10; Syntax: Common-Lisp; -*-

(in-package :utils)

;;; +=========================================================================+
;;; | Copyright (c) 2009 and beyond JP Massar, Jeff Shrager, Mike Travers     |
;;; |                                                                         |
;;; | Permission is hereby granted, free of charge, to any person obtaining   |
;;; | a copy of this software and associated documentation files (the         |
;;; | "Software"), to deal in the Software without restriction, including     |
;;; | without limitation the rights to use, copy, modify, merge, publish,     |
;;; | distribute, sublicense, and/or sell copies of the Software, and to      |
;;; | permit persons to whom the Software is furnished to do so, subject to   |
;;; | the following conditions:                                               |
;;; |                                                                         |
;;; | The above copyright notice and this permission notice shall be included |
;;; | in all copies or substantial portions of the Software.                  |
;;; |                                                                         |
;;; | THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,         |
;;; | EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF      |
;;; | MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  |
;;; | IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY    |
;;; | CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,    |
;;; | TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE       |
;;; | SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.                  |
;;; +=========================================================================+

;;; Authors:  JP Massar

(eval-when (:compile-toplevel :load-toplevel :execute)

  (defparameter *utility-circseg-user-symbols*
    '(
      ;; 1-based circular coordinate utilities
      circseg-length
      create-circseg
      in-circseg?
      csc-within?
      coordinate-within-region?
      csc->zbc
      zbc->csc
      circ+ 
      circ-
      circ-range
      circ-loop-from-to
      ;; circseg data structure utilities
      circseg-coord->zbc
      zbc->circseg-coord
      cs-aref 
      setf-cs-aref
      cs-subseq
      circseg1+
      circseg+
      circseg1-
      circseg-
      cs-loop-from-to
      ))

    
  (defparameter *utility-circseg-api-symbols*
    (append *utility-circseg-user-symbols*
            '( )))

  (export *utility-circseg-api-symbols* (find-package :utils)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Circular arithmetic etc.

(defun circseg-length (from to circle-size)
  "Returns length of (FROM TO) range on a circle of CIRCLE-SIZE units"
  (if (< from to) 
      (1+ (- to from))
    (+ (1+ (- circle-size from)) to)
    ))

(defun csc-within? (csc from to)
  "Is coordinate CSC with the circular range (FROM TO)?"
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  #+:allegro
  (declare (fixnum csc from to))
  (csc-maybe-oops csc)
  (if (<= from to)
      (and (>= csc from) (<= csc to))
    (or (>= csc from) (<= csc to))
    ))

(defun coordinate-within-region? 
       (c from to clen &key (from-ok? t) (to-ok? t) (too-big? :error))
  #.(one-string-nl
     "Is coordinate C in the range (FROM TO) in a circular coordinate space"
     "  of size CLEN?"
     "If FROM-OK? is NIL, NIL is returned if C = FROM"
     "If TO-OK? is NIL, NIL is returned if C = TO"
     "TOO-BIG? determines what happens when C is not in the range (1 CLEN)."
     "  By default, an error is signaled."
     "  If the value is anything but :error, the value is returned in lieu"
     "    of an answer."
     )
  (cond
   ((or (< c 1) (> c clen)) 
    (when (eq too-big? :error)
      (error "Coordinate ~A not between 1 and size of circle (~D)" c clen))
    too-big?
    )
   ((= c from) from-ok?)
   ((= c to) to-ok?)
   ((<= from to) (and (>= c from) (<= c to)))
   ((> from to) (or (>= c from) (<= c to)))
   (t (error "This is impossible!"))
   ))

(defun csc-maybe-oops (x)
  (when (not (plusp x))
    (error "~D is not a valid coordinate in circle space!" x)
    ))

   
(defun csc->zbc (x from to circle-size)
  #.(one-string-nl
     "Translates a circular coordinate to a zero based coordinate"
     "such that X=FROM -> 0, taking into account that (FROM TO) may wrap."
     )
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  #+:allegro
  (declare (fixnum x from to circle-size))
  (csc-maybe-oops x)
  (if (<= from to)
      (if (or (< x from) (> x to))
          (error "The coordinate ~D is not within the segment range ~D - ~D"
                 x from to)
        (- x from))
    (if (and (< x from) (> x to))
        (error "The coordinate ~D is not within the segment range ~D - ~D"
               x from to)
      (if (>= x from)
          (- x from)
        (+ x (- circle-size from))
        ))))

(defun zbc->csc (x from to circle-size)
  #.(one-string-nl
     "Translates a zero based coordinate to a circular coordinate"
     "such that X=0 -> FROM, taking into account that (FROM TO) may wrap."
     )
  (when (or (minusp x) (>= x circle-size))
    (error "Invalid zero-based coordinate ~D for circle-size ~D" x circle-size))
  (flet ((oops (x) 
           (error "Zero based coordinate ~D beyond range (~D to ~D)" x from to)
           ))
    (let ((csc (+ from x)))
      (if (<= from to)
          (if (> csc to) (oops x) csc)
        (cond
         ((<= csc circle-size) csc)
         (t 
          (let ((wrapped-csc (- csc circle-size)))
            (if (> wrapped-csc to) (oops x) wrapped-csc)
            )))))))

(defun circ+ (coord offset circle-size)
  #.(one-string-nl
     "Modular arithmetic in circular coordinate (1-based) space of size CLEN"
     "Adds OFFSET to COORD in circular space."
     "COORD must be a valid coordinate -- in the range (1 CLEN)."
     )
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  #+:allegro
  (declare (fixnum coord offset circle-size))
  (let ((x (+ coord offset)))
    (cond
     ((and (plusp x) (<= x circle-size)) x)
     ((> x circle-size) (mod x circle-size))
     ((zerop x) circle-size)
     ((minusp x) (mod x circle-size))
     (t (error "Impossible!"))
     )))
        
(defun circ- (coord offset circle-size) 
  #.(one-string-nl
     "Modular arithmetic in circular coordinate (1-based) space of size CLEN"
     "Subtracts OFFSET from COORD in circular space."
     "COORD must be a valid coordinate -- in the range (1 CLEN)."
     )
  (circ+ coord (- offset) circle-size))

(defun circ-range (from min-to less extent clen clamp-start clamp-end wrap)

  #.(one-string-nl
     "Returns an list of (START END) coordinates on a circle of CLEN units."
     "The coordinate system is 1-based, and END is the last coordinate, not"
     "(as with SUBSEQ and its ilk) one past the end."
     "FROM is used as a base point."
     "LESS is an offset pushing START towards 1 from FROM."
     "EXTENT is the size of the range returned"
     "  (unless it is bigger than CLEN, or is usurped by MIN-TO)"
     "MIN-TO, if provided, specifies a coordinate which the range returned"
     "  must go to, regardless of EXTENT."
     "CLAMP-START demands that if LESS would cause START to be beyond"
     "  the beginning of the circle (coordinate 1), i.e., wrap, that START"
     "  be forced to be at coordinate 1 (i.e., 'clamped')."
     "CLAMP-END demands that if EXTENT would cause END to be beyond the end"
     "  of the circle (coordinate CLEN), i.e., wrap, that END be forced to be"
     "  at coordinate CLEN (i.e., 'clamped')."
     "WRAP specifies whether wrapping is allowed at all.  If not, clamping"
     "  of the returned range at both ends of the segment (1 CLEN) is"
     "  always enforced."
     )
  
  ;; make sure nothing is bigger than clen
  (when (> extent clen) (setq extent clen))
  (when (> less clen) (setq less clen))

  (when (and min-to (or (not (plusp min-to)) (> min-to clen)))
    (error "Invalid MIN-TO (~D).  Cannot be outside of (1,~D)" min-to clen)) 
  
  (cond
   
   ((or (null wrap) (and clamp-start clamp-end))

    (unless (or (null min-to) (>= min-to from))
      (error "Inconsistency: MIN-TO (~D) < FROM (~D) but WRAP disabled" 
             min-to from))
    (let ((range-from (- from less)))
      (when (not (plusp range-from)) (setq range-from 1))
      (let ((range-to (+ range-from (1- extent))))
        (when min-to 
          (when (< range-to min-to) (setq range-to min-to)))
        (when (> range-to clen) (setq range-to clen))
        (list range-from range-to)
        )))

   ((null min-to) 
    (let ((range-from (circ- from less clen)))
      (when clamp-start 
        (when (> range-from from) (setq range-from 1)))
      (let ((range-to (circ+ range-from (1- extent) clen)))
        (when clamp-end 
          (when (< range-to range-from) (setq range-to clen)))
        (list range-from range-to)
        )))

   (wrap 

    (cond
     
     ;; the (FROM MIN-TO) segment does not overlap the end 
     ((<= from min-to) 
      (let ((range-from (circ- from less clen))) 

        (if clamp-start 
            ;; we went back beyond the end so clamp at 1
            (when (coordinate-within-region? range-from from clen clen)
              (setq range-from 1))
          ;; We wrapped all the way around into the (FROM MIN-TO) region
          ;; so push RANGE-FROM beyond MIN-TO
          (when (coordinate-within-region? range-from from min-to clen)
            (setq range-from (circ+ min-to 1 clen))
            ))
        
        (cond
         
         ;; going LESS back from FROM doesn't push us over the end
         ((<= range-from from)
          (let ((range-to (circ+ range-from (1- extent) clen)))
            (if clamp-end 
                ;; we went past the end so clamp at the end (CLEN)
                (when (coordinate-within-region? range-to 1 min-to clen)
                  (setq range-to clen)
                  )
              (cond
               ;; didn't even make it to FROM...
               ((coordinate-within-region? range-to range-from from clen)
                (setq range-to min-to))
               ;; didn't make it to MIN-TO...
               ((coordinate-within-region? range-to from min-to clen)
                (setq range-to min-to))
               ;; made it all the way around past the end 
               ((coordinate-within-region? range-to 1 range-from clen)
                nil)
               ;; made it past min-to but not past the end
               ((coordinate-within-region? range-to min-to clen clen)
                nil)
               (t (error "circ-range: This should be impossible! 1"))
               ))
            (list range-from range-to)
            ))

         ;; going LESS back from FROM pushes us over the end
         ((> range-from from)
          (let ((range-to (circ+ range-from (1- extent) clen)))
            (cond
             ;; didn't even make it back to the end
             ((coordinate-within-region? range-to range-from clen clen)
              (setq range-to min-to))
             ;; made it back past the end but not even to FROM
             ((coordinate-within-region? range-to 1 from clen)
              (setq range-to min-to))
             ;; made it back past the end but not to MIN-TO
             ((coordinate-within-region? range-to from min-to clen)
              (setq range-to min-to))
             ;; made it past MIN-TO
             ((coordinate-within-region? range-to min-to range-to clen)
              nil)
             (t (error "circ-range: This should be impossible! 2"))
             )
            (list range-from range-to)
            )))))

     ;;  the (FROM MIN-TO) segment does overlap the end 
     ((> from min-to)
      (let ((range-from (circ- from less clen)))
        ;; We wrapped all the way around into the (FROM MIN-TO) region
        ;; so push RANGE-FROM beyond MIN-TO
        (when (or (coordinate-within-region? range-from from clen clen)
                  (coordinate-within-region? range-from 1 min-to clen))
          (setq range-from (circ+ min-to 1 clen)))
        (let ((range-to (circ+ range-from (1- extent) clen)))
          (cond
           ((coordinate-within-region? range-to range-from from clen)
            (setq range-to min-to))
           ((coordinate-within-region? range-to from min-to clen)
            (setq range-to min-to))
           ((coordinate-within-region? range-to min-to range-from clen)
            nil)
           (t (error "circ-range: This should be impossible! 3"))
           )
          (list range-from range-to)
          )))
     
     ))

   (t (error "circ-range: This should be impossible! 4"))
   
   ))


(defmacro circ-loop-from-to 
          ((pos-symbol from to clen &optional (return-form nil)) &body body)
  (let ((clen-symbol (gensym "CLEN-"))
        (from-symbol (gensym "FROM-"))
        (to-symbol (gensym "TO-")))
    `(let ((,from-symbol ,from)
           (,to-symbol ,to)
           (,clen-symbol ,clen))
       (when (or (not (plusp ,from-symbol)) 
                 (> ,from-symbol ,clen-symbol)
                 (not (plusp ,to-symbol))
                 (> ,to-symbol ,clen-symbol))
         (error "Invalid FROM,TO,CLEN specification (~D, ~D, ~D)"
                ,from-symbol ,to-symbol ,clen-symbol
                ))
       (loop 
        for ,pos-symbol = ,from-symbol then (circ+ ,pos-symbol 1 ,clen-symbol)
        do
        ,@body
        (when (= ,pos-symbol ,to-symbol) (return ,return-form))
        ))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct circseg array from to circle-size)

(defun create-circseg (from to circle-size &key (data-array nil))
  (make-circseg 
   :array 
   (or data-array (make-array (list (circseg-length from to circle-size))))
   :from from
   :to to
   :circle-size circle-size 
   ))

(defun in-circseg? (coord cs)
  (csc-maybe-oops coord)
  (let ((from (circseg-from cs))
        (to (circseg-to cs)))
    (csc-within? coord from to)
    ))

(defun circseg-coord->zbc (x cs)
  (csc->zbc x (circseg-from cs) (circseg-to cs) (circseg-circle-size cs)))

(defun zbc->circseg-coord (x cs)
  (zbc->csc x (circseg-from cs) (circseg-to cs) (circseg-circle-size cs)))
       
(defun cs-aref (cs i)
  (csc-maybe-oops i)
  (aref (circseg-array cs) (circseg-coord->zbc i cs)))

(defun setf-cs-aref (cs i value)
  (csc-maybe-oops i)
  (setf (aref (circseg-array cs) (circseg-coord->zbc i cs)) value))

(defun cs-subseq (cs from to)
  (let ((cfrom (circseg-from cs))
        (cto (circseg-to cs)))
    (when (not (in-circseg? from cs))
      (error "Start of subsequence (~D) is not in circ. sequence range ~D - ~D"
             from cfrom cto))
    (when (not (in-circseg? to cs))
      (error "End of subsequence (~D) is not in circ. sequence range ~D - ~D"
             to cfrom cto))
    (let ((zfrom (circseg-coord->zbc from cs))
          (zto (circseg-coord->zbc to cs))
          (data-array (circseg-array cs)))
      (if (<= zfrom zto)
          (subseq data-array zfrom (1+ zto))
        ;; This should be impossible!  In the zero-based coordinates
        ;; the cirseg coordinates FROM and TO are translated to, ZFROM
        ;; should always be less than or equal to ZTO
        (error "This really should be impossible!")
        ))))


(defun circseg1+ (cs coord) (circseg+ cs coord 1))

(defun circseg+ (cs coord offset)
  (circ+ coord offset (circseg-circle-size cs)))

(defun circseg1- (cs coord) (circseg- cs coord 1))

(defun circseg- (cs coord offset)
  (circ- coord offset (circseg-circle-size cs)))

(defmacro cs-loop-from-to ((pos-symbol cs from to) &body body)
  (let ((cs-symbol (gensym "CS-"))
        (from-symbol (gensym "FROM-"))
        (to-symbol (gensym "TO-")))
    `(let ((,cs-symbol ,cs)
           (,from-symbol ,from)
           (,to-symbol ,to))
       (loop 
        for ,pos-symbol = ,from-symbol then (circseg1+ ,cs-symbol ,pos-symbol)
        do
        ,@body
        (when (= ,pos-symbol ,to-symbol) (return nil))
        ))))



      
         

   
        
        
      
        