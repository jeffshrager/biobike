;;; -*- mode: Lisp; Syntax: Common-Lisp; Package: biolisp; -*-

(in-package :bio)

;;; +=========================================================================+
;;; | Copyright (c) 2002, 2003, 2004 JP Massar, Jeff Shrager, Mike Travers    |
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

;;; Author: JP Massar.

;;;; Code that translates from one form of amino acid representation
;;;; to another (e.g., from the name to the three-letter code).

;;;; Code to compute the molecular weight of a protein.

;;;; Code to translate a DNA sequence into a sequence of amino acids.



(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *amino-acid-conversion-api-symbols*
    '(
      dna-to-rna-sequence
      rna-to-dna-sequence
      amino-acid-designator?
      aa-to-long-name
      aa-to-1-letter-code
      aa-to-3-letter-code
      aa-to-atoms
      aa-to-mw
      aa-to-codons
      codon-to-aa
      molecular-weight-of
      translate-d/rna-to-aa
      translate-to-aa
      ))
  (export *amino-acid-conversion-api-symbols* (find-package :biolisp))
  )


(defparameter *aa-info-hash-table* (make-string-equal-hash-table))

(defstruct aa
  full-name
  full-namestring
  full-namestring-lc
  full-namestring-cap
  letter-symbol
  letter-char
  letter-char-lc
  letter-string
  letter-string-lc
  code-symbol
  code-string
  code-string-lc
  code-string-cap
  atoms
  molecular-weight
  codon-symbols
  codon-strings
  codon-strings-lc
  rna-codon-symbols
  rna-codon-strings
  rna-codon-strings-lc
  )

(defparameter *aa-info-list*
  ;; name 1-letter-code 3-letter-code n-atoms mol-weight codons
  '(
    (alanine          A        Ala         5      89  GCT GCC GCA GCG)
    (arginine         R        Arg        11     174  CGT CGC CGA CGG AGA AGG)
    (asparagine       N        Asn         8     132  AAT AAC)
    (aspartic-acid    D        Asp         8     133  GAT GAC )
    (cysteine         C        Cys         6     121  TGT TGC)
    (glutamic-acid    E        Glu         9     146  GAA GAG)
    (glutamine        Q        Gln         9     147  CAA CAG)
    (glycine          G        Gly         4      75  GGT GGC GGA GGG )
    (histidine        H        His        10     155  CAT CAC)
    (isoleucine       I        Ile         8     131  ATT ATC ATA)
    (leucine          L        Leu         8     131  TTA TTG CTT CTC CTA CTG)
    (lysine           K        Lys         9     146  AAA AAG)
    (methionine       M        Met         8     149  ATG)
    (phenylalanine    F        Phe        11     165  TTT TTC)
    (proline          P        Pro         7     115  CCT CCC CCA CCG)
    (serine           S        Ser         6     105  TCT TCC TCA TCG AGT AGC)
    (threonine        T        Thr         7     119  ACT ACC ACA ACG)
    (tryptophan       W        Trp        13     204  TGG)
    (tyrosine         Y        Tyr        11     181  TAT TAC)
    (valine           V        Val         7     117  GTT GTC GTA GTG)
    (stop             *        ***         0       0  TAA TAG TGA)
    ))

(defun dna-to-rna-sequence (s &key (in-place? nil))
  "Converts Tt to Uu. The substitution is done in place iff IN-PLACE? is T"
  (funcall 
   (if in-place? 'ntranslate-string 'translate-string)
   s "Tt" "Uu"
   ))

(defun rna-to-dna-sequence (s &key (in-place? nil))
  "Converts Uu to Tt. The substitution is done in place iff IN-PLACE? is T"
  (funcall 
   (if in-place? 'ntranslate-string 'translate-string)
   s "Uu" "Tt"
   ))

(defun aa-info-to-aa-record (aa-info)
  (destructuring-bind
      (name letter code atoms molecular-weight &rest codons)
      aa-info
    (let ((ns (copy-seq (string name)))
          (lc (char (string letter) 0))
          (ls (copy-seq (string letter)))
          (cs (copy-seq (string code))))
      (make-aa
       :full-name name
       :full-namestring ns
       :full-namestring-lc (string-downcase ns)
       :full-namestring-cap (string-capitalize ns)
       :letter-symbol letter
       :letter-char lc
       :letter-char-lc (char-downcase lc)
       :letter-string ls
       :letter-string-lc (string-downcase ls)
       :code-symbol code
       :code-string cs
       :code-string-lc (string-downcase cs)
       :code-string-cap (string-capitalize cs)
       :atoms atoms
       :molecular-weight molecular-weight
       :codon-symbols codons
       :codon-strings 
       (mapcar (lambda (c) (copy-seq (string c))) codons)
       :codon-strings-lc 
       (mapcar (lambda (c) (string-downcase (string c))) codons)
       :rna-codon-symbols
       (mapcar (lambda (c) (intern (dna-to-rna-sequence (string c)))) codons)
       :rna-codon-strings
       (mapcar (lambda (c) (dna-to-rna-sequence (string c))) codons)
       :rna-codon-strings-lc
       (mapcar 
        (lambda (c) (string-downcase (dna-to-rna-sequence (string c))))
        codons
        )))))

(defun create-aa-info-hash-table (aa-info-list)
  (loop for aa-info in aa-info-list do
        (let ((aar (aa-info-to-aa-record aa-info))
              (aaht *aa-info-hash-table*))
          (setf (gethash (aa-full-name aar) aaht) aar)
          (setf (gethash (keywordize (aa-full-name aar)) aaht) aar)
          (setf (gethash (aa-full-namestring aar) aaht) aar)
          (setf (gethash (aa-letter-symbol aar) aaht) aar)
          (setf (gethash (keywordize (aa-letter-symbol aar)) aaht) aar)
          (setf (gethash (aa-letter-char aar) aaht) aar)
          (setf (gethash (aa-letter-string aar) aaht) aar)
          (setf (gethash (aa-code-symbol aar) aaht) aar)
          (setf (gethash (keywordize (aa-code-symbol aar)) aaht) aar)
          (setf (gethash (aa-code-string aar) aaht) aar)
          (loop for codon-symbol in (aa-codon-symbols aar) 
                for rna-codon-symbol in (aa-rna-codon-symbols aar)
                for codon-string in (aa-codon-strings aar) 
                for rna-codon-string in (aa-rna-codon-strings aar) do
                (setf (gethash codon-symbol aaht) aar)
                (setf (gethash (keywordize codon-symbol) aaht) aar)
                (setf (gethash rna-codon-symbol aaht) aar)
                (setf (gethash (keywordize rna-codon-symbol) aaht) aar)
                (setf (gethash codon-string aaht) aar)
                (setf (gethash rna-codon-string aaht) aar)
                )))
  nil)

(create-aa-info-hash-table *aa-info-list*)

(defun amino-acid-designator? (x) 
  "T if X is a symbol, string or char that denotes an amino acid"
  (not (null (gethash x *aa-info-hash-table*))))

(DEFUN aa-to-1-letter-code (aa &optional (as :string))
  #.(one-string-nl
     "(AA-TO-1-LETTER-CODE amino-acid &optional as)"
     "Takes amino acid"
     "  (1-letter -- character, string or symbol), "
     "  (3-letter -- string or symbol),"
     "  (full name -- string or symbol)"
     "Returns 1-letter code AS "
     ":string :lc-string :symbol :keyword :char or :lcchar")
  (let ((aar (gethash aa *aa-info-hash-table*)))
    (ecase as
      (:string (aa-letter-string aar))
      ((:lcstring :lc-string) (aa-letter-string-lc aar))
      (:symbol (aa-letter-symbol aar))
      (:keyword (keywordize (aa-letter-symbol aar)))
      ((:char :character) (aa-letter-char aar))
      ((:lcchar :lc-char :lccharacter :lc-character) (aa-letter-char-lc aar))
      )))

(DEFUN aa-to-3-letter-code (aa &optional (as :string))
  #.(one-string-nl
     "(AA-TO-3-LETTER-CODE amino-acid &optional as)"
     "Takes amino acid"
     "  (1-letter -- character, string or symbol), "
     "  (3-letter -- string or symbol),"
     "  (full name -- string or symbol)"
     "Returns 3-letter code AS "
     ":string :lcstring :capstring :symbol or :keyword")
  (let ((aar (gethash aa *aa-info-hash-table*)))
    (ecase as
      (:string (aa-code-string aar))
      ((:lcstring :lc-string) (aa-code-string-lc aar))
      ((:capstring :cap-string) (aa-code-string-cap aar))
      (:symbol (aa-code-symbol aar))
      (:keyword (keywordize (aa-code-symbol aar)))
      )))

(DEFUN aa-to-codons (aa &optional (as :strings))
  #.(one-string-nl
     "(AA-TO-CODONS amino-acid &optional as)"
     "Takes amino acid"
     "  (1-letter -- character, string or symbol), "
     "  (3-letter -- string or symbol),"
     "  (full name -- string or symbol)"
     "Returns list of codons that encode amino acid AS"
     ":strings :lcstrings :symbols :keywords "
     ":rna-strings :rna-lcstrings :rna-symbols")
  (let ((aar (gethash aa *aa-info-hash-table*)))
    (ecase as
      ((:strings :string) (aa-codon-strings aar))
      ((:lcstrings :lc-strings :lcstring :lc-string) (aa-codon-strings-lc aar))
      ((:symbol :symbols) (aa-codon-symbols aar))
      ((:keywords :keyword) (mapcar 'keywordize (aa-codon-symbols aar)))
      ((:rna-strings :rna-string) (aa-rna-codon-strings aar))
      ((:rna-lcstrings :rna-lcstrings :rna-lc-string :rna-lc-string)
       (aa-rna-codon-strings-lc aar))
      ((:rna-symbols :rna-symbol) (aa-rna-codon-symbols aar))
      )))

(DEFUN aa-to-mw (aa &optional (as :integer))
  #.(one-string-nl
     "(AA-TO-MW amino-acid &optional as)"
     "Takes amino acid"
     "  (1-letter -- character, string or symbol), "
     "  (3-letter -- string or symbol),"
     "  (full name -- string or symbol)"
     "Returns molecular weight of amino acid AS"
     ":integer :fixnum :single-float :double-float")
  (let* ((aar (gethash aa *aa-info-hash-table*))
         (mw (aa-molecular-weight aar)))
    (declare (fixnum mw))
    (ecase as
      ((:integer :fixnum) mw)
      ;; These will cons.
      ((:single-float :sfloat :float) (float mw 0.0))
      ((:double-float :dfloat :double) (float mw 0.0d0))
      )))
       
(DEFUN aa-to-atoms (aa)
  #.(one-string-nl
     "(AA-TO-ATOMS amino-acid &optional as)"
     "Takes amino acid"
     "  (1-letter -- character, string or symbol), "
     "  (3-letter -- string or symbol),"
     "  (full name -- string or symbol)"
     "Returns number of atoms in amino acid")
  (aa-atoms (gethash aa *aa-info-hash-table*)))


(DEFUN aa-to-long-name (aa &optional (as :string))
  #.(one-string-nl
     "(AA-TO-LONG-NAME amino-acid &optional as)"
     "Takes amino acid"
     "  (1-letter -- character, string or symbol), "
     "  (3-letter -- string or symbol),"
     "  (full name -- string or symbol)"
     "Returns full name of amino acid AS"
     ":string :lcstring :capstring :symbol :keyword")
  (let ((aar (gethash aa *aa-info-hash-table*)))
    (ecase as
      (:string (aa-full-namestring aar))
      ((:lcstring :lc-string) (aa-full-namestring-lc aar))
      ((:capstring :cap-string) (aa-full-namestring-cap aar))
      (:symbol (aa-full-name aar))
      (:keyword (keywordize (aa-full-name aar)))
      )))

(DEFUN codon-to-aa (codon &optional (as :string))
  #.(one-string-nl
     "(CODON-TO-AA codon &optional as)"
     "Takes 3-letter codon (symbol or string)"
     "Returns 1-letter code of amino acid encoded by the codon, or '-' AS"
     ":character :string :lcstring :symbol :keyword")
  (let ((aar (gethash codon *aa-info-hash-table*)))
    (if aar
        (ecase as
          ((:char :character) (aa-letter-char aar))
          (:string (aa-letter-string aar))
          ((:lcstring :lc-string) (aa-letter-string-lc aar))
          (:symbol (aa-letter-symbol aar))
          (:keyword (keywordize (aa-letter-symbol aar))))
      (ecase as
        ((:char :character) #\-)
        ((:string :lcstring :lc-string) "-")
        (:symbol '-)
        (:keyword :-)
        ))))


(defun molecular-weight-of (aa-sequence)
  #.(one-string-nl
     "(MOLECULAR-WEIGHT-OF aa-sequence)"
     "returns the integer molecular weight of the given amino acid sequence")
  #.(optimization-declaration)
  (let ((total-mw 0))
    (declare (fixnum total-mw))
    (cond
     ((typep aa-sequence 'simple-string)
      (loop for pos fixnum from 0 below (length aa-sequence)
            as aa = (schar aa-sequence pos)
            as mw fixnum = (aa-to-mw aa)
            do (incf total-mw mw)))
     (t
      (loop for pos fixnum from 0 below (length aa-sequence)
            as aa = (char aa-sequence pos)
            as mw fixnum = (aa-to-mw aa)
            do (incf total-mw mw))))
    total-mw))


(defun translate-d/rna-to-aa 

       (d/rna-sequence 
        &KEY (as :1codes) (separator "" separator-provided?) 
        (if-partial-codon :error) (if-unknown-codon :error)
        (insist-on nil))

  #.(one-string-nl
     "(TRANSLATE-D/RNA-TO-AA"
     "  d/rna-sequence &key as separator if-partial-codon if-unknown-codon)"
     "Translates DNA or RNA sequence to amino acid sequence."
     "Returns string of amino acids, either as 1-letter codes "
     "(:as :1codes), the default, or as 3-letter codes (:as :3codes) "
     "Each amino acid is separated by nothing (default for :1codes) or a"
     "single space (default for :3codes) or the provided separator string"
     "SEPARATOR."
     "If the sequence is not a multiple of 3 characters long, IF-PARTIAL-CODON"
     "specifies whether to signal an error (:error), a warning (:warn)"
     "or ignore the excess terminating characters (:ignore)."
     "IF-UNKNOWN-CODON specifies what to do if an unknown codon is detected"
     "in the sequence.  Possibilities are to signal an error (:error), a"
     "warning (:warn), or, if IF-UNKNOWN-CODON is a character or string to"
     "use that value as the translation, or if it is anything else to use"
     "the default (#\- or '---') as the translation."
     "If INSIST-ON is :DNA or :RNA, each codon is checked to insure that it"
     "does not contain 'U' or 'T' respectively. The default, NIL, allows both")

  (block exit

    ;; First figure out how big the translation is going to be
    ;; and allocate a string of that size.

    (let* ((seqlen (length d/rna-sequence))
           (separator (coerce separator 'simple-string))
           (codon #.(make-string 3))
           (n-translations (/ seqlen 3))
           (tunit-size
            (ecase as
              ((:char :chars :character :characters :code1 :1code :1codes) 1)
              ((:code3 :code3-strings :3code :3codes :3code-strings) 
               (unless separator-provided? (setq separator " "))
               3)))
           (seplen (length separator))
           (tdelta (+ tunit-size seplen)))
      (declare (fixnum seqlen seplen tunit-size))
      (declare (fixnum tdelta))
      (declare (simple-string separator codon))
      (unless (integerp n-translations)
        (ecase if-partial-codon
          (:error (error "D/RNA sequence is not a multiple of 3 long!!"))
          (:warn (warn "D/RNA sequence is not a multiple of 3"))
          ((:ignore :truncate nil) nil))
        (setq n-translations (floor n-translations)))
      (let ((total-translation-size
             (- (* n-translations (+ tunit-size seplen)) seplen)))
        (when (not (plusp total-translation-size))
          (return-from exit ""))
        (let ((translated-string (make-string total-translation-size))
              (n-translations n-translations))
          (declare (fixnum n-translations))

          ;; Now loop for every three characters in D/RNA-SEQUENCE
          ;; translating the 3-character codon into an amino acid.
          ;; Ignore excess characters, if any, at the end.

          (LOOP FOR coord fixnum FROM 0 BELOW (- seqlen 2) BY 3
                for tcoord fixnum from 0 by tdelta 
                for n fixnum from 1 do
                (loop for j fixnum from coord 
                      for i fixnum from 0 below 3 do
                      (setf (schar codon i) (char d/rna-sequence j)))
                (when insist-on
                  (ecase insist-on
                    (:dna 
                     (when (find #\U codon :test 'char-equal)
                       (error 
                        "RNA char ~S in codon ~S in DNA sequence at pos ~D"
                        #\U codon coord)))
                    (:rna
                     (when (find #\T codon :test 'char-equal)
                       (error 
                        "DNA char ~S in codon ~S in RNA sequence at pos ~D"
                        #\T codon coord)))))
                (cond
                 ((= tunit-size 1)
                  (let ((code-char (codon-to-aa codon :character)))
                    (when (eql code-char #\-)
                      (case if-unknown-codon
                        (:error 
                         (error "Unknown codon ~S, position ~D" codon coord))
                        (:warn 
                         (warn "Unknown codon ~S, position ~D" codon coord))
                        (otherwise
                         (cond
                          ((characterp if-unknown-codon)
                           (setq code-char if-unknown-codon))
                          ((stringp if-unknown-codon)
                           (setq code-char (char if-unknown-codon 0)))
                          (t nil)
                          ))))
                    (setf (schar translated-string tcoord) code-char)))
                 ((= tunit-size 3)
                  (let ((code-symbol (codon-to-aa codon :symbol)) 
                        code-string)
                    (if (eq code-symbol '-)
                        (case if-unknown-codon
                          (:error 
                           (error "Unknown codon ~S, position ~D" codon coord))
                          (:warn 
                           (warn "Unknown codon ~S, position ~D" codon coord)
                           (setq code-string "---"))
                          (otherwise
                           (setq code-string
                                 (if (stringp if-unknown-codon)
                                     if-unknown-codon "---"))))
                      (setq code-string 
                            (aa-to-3-letter-code (codon-to-aa codon :symbol))))
                    (loop for j fixnum from tcoord
                          for i fixnum from 0 below 3 do
                          (setf (schar translated-string j) 
                                (schar code-string i)
                                ))))
                 (t (error "Internal error. Invalid translation unit size.")))

                ;; Add the separation string if it exists and
                ;; this isn't the very last iteration.

                (when (and (plusp seplen) (/= n n-translations))
                  (loop for j from (the fixnum (+ tcoord tunit-size))
                        for i from 0 below seplen do
                        (setf (schar translated-string j) (schar separator i))
                        )))

          translated-string

          )))))

(defun translate-to-aa (&rest args)
  "Alias for (TRANSLATE-D/RNA-TO-AA ...  See documentation for that function."
  (apply 'translate-d/rna-to-aa args))
