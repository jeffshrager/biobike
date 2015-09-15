;;; -*- mode: Lisp; Syntax: Common-Lisp; Package: bbi; -*-

(in-package :bbi)

;---------------------------------------------------------------------------
(DEFUN Exp-files-in-directory (dir-contents)
  "Counts how many files in directory end -n.txt"
  (LOOP FOR file IN dir-contents
        WHEN (MATCH-OF PATTERN "-\\d+\\.txt" IN (STRING-OF file))
        COUNT file))

;---------------------------------------------------------------------------

(DEFUN Load-Microarray-Condition-frames
  (path organism expt-frame n detail-list)
  "Loads condition frames for microarray data"
  (SETF detail-list (SUBSEQ detail-list 2))
  (LET*
    ((replicate-structure (NEW-TABLE '($ $)))
     (condition-frame-list
       (LOOP WITH expt-frame-name = (SLOTV expt-frame #$FName)
             WITH cond-number = 0
             FOR i FROM 1 TO n
             FOR raw-descr IN detail-list
             AS id-descr = (FIRST (SPLIT raw-descr AT " Download data")) 
             AS repl-file-name = (FORMAT NIL "~a~a~a~a~a" path expt-frame-name "-" i ".txt" )
             AS cond-frame = NIL
             AS new-frame = T
             AS id = NIL
             AS descr = NIL
             AS group = NIL ; optional group of technical replicates
             ;; This variable is never accessed -- JP
             ;; AS dye-type = NIL
             AS dye-flip = NIL
             DO 
             (ASSIGN (id descr group) ;; dye-type) -- JP (see above)
                     =
                     (OR
                      (MATCH-OF-PATTERN-aux NIL
                              "(\\S+) +([^|]+[^ |]) *\\|\\| *(\\S+) *(\\S+)"
                              id-descr '(NIL NIL NIL T))
                      (MATCH-OF-PATTERN-aux NIL "(\\S+) +(0|1) +(.*)"
                              id-descr '(NIL NIL NIL T))
                      (MATCH-OF-PATTERN-aux NIL "(\\S+) +(.*)"
                              id-descr '(NIL NIL NIL T))))
             (IF-TRUE (AND (OR (EQUAL descr "0")(EQUAL descr "1"))
                        group)
               THEN (SETF dye-flip (LIST (EQUAL descr "1")))
               (SETF descr group)
               (SETF group NIL))
             ;   (IF (EQUAL expt-frame #$Campbell_2007)
                     ;      (DISPLAY-DATA id descr group dye-flip))
             (SETF cond-frame
               (LOOP FOR j FROM 1 TO cond-number
                     AS frame-name-x = (FORMAT NIL "~a~a~a" expt-frame-name "-" j) 
                     AS frame-x = (FRAME-FNAMED frame-name-x)
                     AS descr-x = (AND frame-x (SLOTV frame-x #$description))
                     DO (IF-TRUE (EQUAL descr-x descr)
                          THEN (SETF new-frame NIL)
                          (RETURN frame-x))
                     FINALLY
                     (PROGN
                       (INCF cond-number)
                       (SETF frame-name-x
                         (FORMAT NIL "~a~a~a" expt-frame-name "-" cond-number)) 
                       (RETURN (FRAME-FNAMED frame-name-x T)))))
             (IF-TRUE group
               THEN (SETF (REF replicate-structure cond-frame group)
                      (APPEND (REF replicate-structure cond-frame group)
                        (LIST i))))
             (IF-TRUE new-frame
               THEN (SETF (SLOTV cond-frame #$organism) organism)
               (SETF (SLOTV cond-frame #$filename) repl-file-name)
               (SETF (SLOTV cond-frame #$experiment) expt-frame)
               (SETF (SLOTV cond-frame #$description) descr)
               (SETF (SLOTV cond-frame #$dye-flip) dye-flip)
               (SETF (SLOTV cond-frame #$ids) (LIST id))
               (SETF (SLOTV cond-frame #$replicates)
                 (LIST repl-file-name))
               ELSE (SETF (SLOTV cond-frame #$ids)
                      (APPEND (SLOTV cond-frame #$ids) (LIST id)))
               (SETF (SLOTV cond-frame #$dye-flip)
                 (APPEND (SLOTV cond-frame #$dye-flip) dye-flip))
               (SETF (SLOTV cond-frame #$replicates)
                 (APPEND (SLOTV cond-frame #$replicates)
                   (LIST repl-file-name))))
             WHEN new-frame
             COLLECT (LIST cond-frame descr))))
    (IF (GARRAY-COMPONENT-INDICES replicate-structure)
        (LOOP FOR (cond-frame structure)
              IN (INTERLEAVE
                   (GARRAY-COMPONENT-INDICES replicate-structure)
                   (GMAP 'IDENTITY replicate-structure))
              DO (SETF (SLOTV cond-frame #$replicate-structure) structure)))
    
    condition-frame-list))

;-----------------------------------------------------------------------

(DEFUN Load-experiment-frames (path &KEY organism org-frame)
  "Makes a frame for an experiment and all conditions within it"
  (UNLESS (STRINGP path)
    (ERROR (S+ "Path to LOAD-EXPERIMENT-FRAME must be "
               "a string, not ~A")
       (TYPE-OF path)))
  (LET* ((path (IF (NOT (EQUAL (BBL::LAST path) "/"))
                   (FORMAT NIL "~a~a" path "/")
                   path))
         (expt-name (BBL::LAST (SPLIT path EVERY "/")))
 	 (expt-frame (FRAME-FNAMED expt-name T))
         (expt-directory-contents (DIRECTORY path))
         (detail-file (FORMAT NIL "~a~a~a" path expt-name "_detail.txt"))
         (detail-list (FILE-TO-STRING-LIST  detail-file))   ; BBL::READ FROM
         (description-string (FIRST detail-list))
         (refer)
         (descr)
         (PMID)
         (pmid-ref-pattern "(.*\\(\\d\\d\\d\\d\\)), (.*) \\[PMID:(.*)\\]" )
         (ref-pattern "(.*\\(\\d\\d\\d\\d\\)), (.*)")
         (pmid-pattern "(.*) \\[PMID:(.*)\\]" )
         (cond-count (EXP-FILES-IN-DIRECTORY expt-directory-contents))
         (cond-list (LOAD-MICROARRAY-CONDITION-FRAMES path organism
                      expt-frame cond-count detail-list))
         )


    (SETF (SLOTV expt-frame (BBI::FRAME-FNAMED "Conditions" T))
      cond-list)
    (SETF (SLOTV expt-frame #$Organism) organism)
    (SETF (SLOTV expt-frame #$Org-expts) org-frame)

    (ASSIGN (refer descr PMID)
      = (MATCH-OF-PATTERN-aux NIL pmid-ref-pattern
                 description-string '(NIL NIL NIL T)))
    (UNLESS refer
      (ASSIGN (refer descr)
        = (MATCH-OF-PATTERN-aux NIL ref-pattern
                   description-string '(NIL NIL NIL T))))
    (UNLESS refer
      (ASSIGN (descr pmid)
        = (MATCH-OF-PATTERN-aux NIL pmid-pattern
                  description-string '(NIL NIL NIL T))))
    (UNLESS descr (SETF descr description-string))
    (SETF (SLOTV expt-frame #$Description) descr)
    (SETF (SLOTV expt-frame #$Ref)
      (IF pmid
          (FORMAT NIL "~a~a~a" refer " http://www.ncbi.nlm.nih.gov/pubmed/" pmid)
          refer))
    expt-frame
    ))

;-----------------------------------------------------------------------

(DEFUN Load-microarray-experiments ()
  "Creates frame of microarray experiments for each organism"
  (DECLARE (SPECIAL *microarray-directory*))
  (LET* ((main-directory *microarray-directory*)
         (organism-directories (DIRECTORY main-directory))
         (number-of-organisms (LENGTH-OF organism-directories))
         )
    
    (LOOP FOR org-number FROM 1 TO number-of-organisms
          FOR organism-directory IN organism-directories
          AS raw-org-name = (BB-STRING-OF organism-directory)
          AS org-name = (SECOND (SPLIT raw-org-name AT "microarray/"))
          AS organism = (FRAME-FNAMED org-name)
          AS org-name2 = (S+ org-name "-mA")
          AS org-frame-name = (S+ "#$" org-name2)
          AS org-frame-id = (READ-FROM-STRING org-frame-name)
          AS org-directory = (S+ raw-org-name "/")
          AS org-frame = (BBI::FRAME-FNAMED org-name2 T)
          AS subdirectory-contents = (DIRECTORY org-directory)
          AS exp-count = (LENGTH-OF subdirectory-contents)
          AS exp-list =
          (LOOP FOR experiment FROM 1 to exp-count
                FOR subdir IN subdirectory-contents
                AS subdir-name = (BB-STRING-OF subdir)
                AS exp-frame-id
                = (LOAD-EXPERIMENT-FRAMES subdir-name
                    :ORGANISM organism :ORG-FRAME org-frame)
                COLLECT exp-frame-id) ;end for-each experiment
          DO (SETF (SLOTV org-frame #$Experiments) exp-list)
          COLLECT org-frame-id)
    ))

