;;; -*- Package: help; mode: lisp; base: 10; Syntax: Common-Lisp; -*-
(in-package :help)


;; MASTER SWITCH.

; Are you using jhelp ( t ) or the old help ( nil ) ?
; Factory setting:  T
(defvar *use-jhelp* T)

; Debug level.  Set to 0 for no debug.   1 prints the scores when you get the listing back.
; Factory setting:  0
(defvar *jhelp-debug* 0)

; Are you using the newer salience math, that discounts boring words?
; Factory setting:   T  (use salience)
(defvar *jhelp-use-salience*  T)

;;============================================
;;  Jumpers for loading specific parts of the old help system, upon initialization.

(defvar *jhelp-load-symbols* nil)
(defvar *jhelp-load-funcdocs* nil) ;; nil:Mar 6 '13;; T: Jul 15 '12

;;  Jumper for loading the advanced jpages portion of the new jhelp system.
;;  This requires file jhelp-pages.lisp to be fleshed out past pre-alpha, 
;;   and non-zombie complex boxes to be implemented correctly.
;;   Note this is a load-time switch, as are the previous ones.
(defvar *jhelp-use-jpages* nil)

;;  Simple first-level version of jhelp, the VPL symbols, is separate
;;  and is always loaded anyway.  No jumper.
;;============================================
;; EMERGENCY DISCONNECT.

;; We hate JHelp so much, we don't even want to run its initialization.
;; This should almost always be NIL.  Disables (jhelp-init),
;; which calls both (jhelp-load-oldhelp) and (jhelp-load-jhelp).
;;  Factory setting:  NIL
(defvar *dont-even-init-jhelp* NIL)



;; OUTPUT LISTING CUT-OFF THRESHOLDS.

;; These numbers determine when returned results get too boring
;; and need to be truncated.  We only return at most the first 100 listings in any case.
;; Scores are an arbitrary sum of interesting features, 
;; and (so far) they are always positive (above 0).
;; Any single word that hits seriously gets a score around 1.

;; Recommend mostly kill off any scores below this number, as long as you have something better than this.
;; Set this number to 0 if you want to help display ALL scores.
;; Set this number to something like 10 if you're really picky, and only want around the top 5 scores.
;; Factory setting is around 0.2 or 0.1.
(defvar *jhelp-threshold*  0)  ;; was 0.20 ;; was 1.0.  

;; If we get a strong hit for the top score, 
;; we want to stop returning anything with a score that's really boring,
;; which is pretty much  less than (top score / *jhelp-div-threshhold*).
;; For instance, if the top item scores 8, and *jhelp-div-threshold* is 4,
;; then there is an additional recommended cutoff threshold of (8/4) => 2.
;; Note this is dependent upon the top score, which is search-dependent.
;;
;; Set this number to 10,000 if you want to help display ALL scores.
;; Set this number to 1 if you're really picky, and only want to display around the top 5 scores.
;; Factory setting is around 4.
(defvar *jhelp-div-threshold* 10000)  ;; was 4 ;; also kill anything with score less than top / div.


;;============================================
;; HELP OUTPUT CLEANUP

;; Don't list the type of the output. 
;; (Overridden by *jhelp-debug* >= 3, which always shows the type, anyway.) 
;;  Factory setting:  T
(defvar *jhelp-dont-output-first-type-column* T)
