;; FUNCTIONS FOR What-is-the-genetic-code TOUR
;   These functions are available to students but the code is not

(LISP:IN-PACKAGE :CL-USER)
(bbi::DEFPACKAGE :Alien-world (:use :BBL))
(bbi::IN-PACKAGE :Alien-world)

(LET ((wb::*guru-allow-arbitrary-redefinitions* T)
      (file-name (bbi::CONCATENATE 'STRING "BIOL:bike/Tutorials/" 
                           "alien-world-aux.lisp"))
     )
  (bbi::c/l file-name)
  )
                   
(lisp:eval-when (:compile-toplevel :load-toplevel :execute)
  (bbi::EXPORT 
   '(Make-random-RNA
     In-vitro-translate 
     Analyze-peptide-content 
     Analyze-amino-acid-content))
  )