;;;; -*- mode: Lisp; Syntax: Common-Lisp; Package: bbi; -*-

(IN-PACKAGE :bbi)

(defstruct nonprovided (type :generic))

(defparameter *unprovided* (make-nonprovided))

; ******* GLOBAL CONSTANTS *******
(DEFCONSTANT *bbl-time-stamp* "2015-04-20 09:02")

(DEFCONSTANT $ '$)
(DEFCONSTANT *unnamed* "Unnamed sequence")
(DEFCONSTANT TRUE T)
(DEFCONSTANT FALSE NIL)
(DEFCONSTANT *genbank* "genbank")
(DEFCONSTANT *big-number* 3.2e38)
(DEFCONSTANT *small-number* 1.0e-45)
(DEFCONSTANT *tab* #\tab)
(DEFCONSTANT *newline* #\newline)
(DEFCONSTANT *CR* #\return)
(DEFCONSTANT *unused* 'do-not-use1357908642)
(DEFCONSTANT *unprovided-* most-negative-fixnum)
(DEFCONSTANT *unprovided+* most-positive-fixnum)
(DEFCONSTANT *unprovided-string* 
   (LET ((BBL (LOOP FOR char ACROSS "BBL"
                    WITH char-list = NIL
                    AS code = (CHAR-CODE char)
                    AS newchar = (CODE-CHAR (- code 64))
                    DO (PUSH (STRING newchar) char-list)
                    FINALLY (RETURN (REVERSE (APPLY 'S+ char-list))))))
      (S+ BBL "-unprovided-string-" BBL)))
(DEF-FRAME '*unprovided-frame*  
           #$Isa  (LIST #$organism) #$organism-prefix :none)
(DEFCONSTANT *unprovided-frame* #$*unprovided-frame*)
(DEFCONSTANT *unprovided-list* (cons nil nil)) 
(DEFCONSTANT *unprovideds*
   (LIST *unprovided-* *unprovided+* *unprovided-string*
         *unprovided-frame* *unprovided-list*))
(DEFCONSTANT *end* 
  (- *unprovided+*
     #+allegro 10000000
     #+lispworks 1000000
     #-(or allegro lispworks) (error "Fix me!")
     ))
(DEFCONSTANT @ (+ *end* 1))

(defvar *bbl-current-case-mode* nil)

(defparameter *kegg-org-of-mapping* nil)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant *legal-protein-chars* "ACDEFGHIKLMNPQRSTVWY")
  (defconstant *legal-dna-chars* "ACGT")
  (defconstant *legal-rna-chars* "ACGU")
  )
(defconstant *legal-extended-protein-chars* 
  #.(s+ *legal-protein-chars* "BOUXZ.*-"))
(defconstant *legal-extended-dna-chars*
  #.(s+ *legal-dna-chars* "BDHKMRSVWYN.-"))
(defconstant *legal-extended-rna-chars*
  #.(s+ *legal-rna-chars* "BDHKMRSVWYN.-"))


; ******* GLOBAL CONSTANT LISTS *******
(DEFCONSTANT *nucleotides* (LIST "A" "C" "G" "T"))
(DEFCONSTANT *amino-acids* (LIST "A" "C" "D" "E" "F" "G" "H" "I" "K" "L"
                                  "M" "N" "P" "Q" "R" "S" "T" "V" "W" "Y"))
(DEFCONSTANT *amino-acids-string* "ACDEFGHIKLMNPQRSTVWY")


; ******* GLOBAL DYNAMIC LISTS *******

(defparameter *legal-protein-regexp* 
  (ppcre:create-scanner (s+ "[^" *legal-protein-chars* "]")))

(defparameter *legal-dna-regexp* 
  (ppcre:create-scanner (s+ "[^" *legal-dna-chars* "]")))

(defparameter *legal-extended-protein-regexp* 
  (ppcre:create-scanner (s+ "[^" *legal-extended-protein-chars* "]")))

(defparameter *legal-extended-dna-regexp* 
  (ppcre:create-scanner (s+ "[^" *legal-extended-dna-chars* "]")))

; ******* TYPES & CONVERSIONS *******

(DEFUN Is-frame? (frame?) (ISFRAME? frame?))
(DEFUN Is-nonnegative? (x)
  "Checks to see if the argument is a number, and is not below 0"
 (AND (REALP x) (or (plusp x) (zerop x))))
(DEFUN Is-organism? (x) (BIOBIKE-ORGANISM? x))
(DEFUN Is-organism-list? (x)
    (AND (LISTP x) (EVERY 'IS-ORGANISM? x)))
(DEFUN Is-positive-integer? (x)
  "Checks to see whether the argument in a positive integer (not necessarily type integer)"
  (AND (REALP x) (PLUSP x) (= x (ROUND x))))
(DEFUN Is-integer? (x)
    "Checks to see whether the argument is an integer (not necessarily type integer)"
    (AND (REALP x) (= x (ROUND x))))
(DEFUN Is-positive-number? (x) 
  "Checks to see whether the argument is a positive number (not necessarily an integer"
    (AND (REALP x) (PLUSP x)))
(DEFUN Is-negative-number? (x)
  (AND (REALP x) (MINUSP x)))	
(DEFUN Is-nonnegative-integer? (x)
  "Checks to see whether the argument is an integer >= 0 (not necessarily type integer)"
  (AND (REALP x) (NOT (MINUSP x)) (= x (ROUND x)))) 
(DEFUN Is-even? (x)
  (AND (IS-INTEGER? x) (EVENP (ROUND x))))
(DEFUN Is-odd? (x)
  (AND (IS-INTEGER? x) (ODDP (ROUND x))))
(DEFUN Is-non-NIL-list? (x) (AND x (LISTP x)))
(DEFUN Is-simple-list? (x) 
  "Checks to see if something is a list which has no sublists"
  (AND (LISTP x) (notany 'consp x))
  ;; (EVERY 'NULL (MAPCAR 'Is-NON-NIL-LIST? x))))
  )
(DEFUN Is-number-string? (x)
   (AND (STRINGP x) (> (LENGTH x) 0)
        (NUMBERP (READ-FROM-STRING x))))

(DEFUN Is-number-list? (x)
   (AND (LISTP x) (EVERY 'NUMBERP x)))

(DEFUN Unprovided? (x)
   (NULL (NULL (MEMBER x *unprovideds* :TEST 'EQUAL))))

(DEFUN Is-function? (f)
    (OR (AND (SYMBOLP f) (FBOUNDP f) T)
        (TYPEP f 'Lisp::Function)))
(DEFUN Is-coding-gene? (entity)
  (AND (TYPEP entity 'GENE)
       (SLOTV entity #$encodes-protein)))

(DEFUN elements-are-of-type (seq type)
  (IF (LISTP type)
      (EVERY
         #'(LAMBDA (x)
             (FORWARD-FUNCALL 'ANY-TRUE-aux
                (MAPCAR (LAMBDA (y)
                    (TYPEP x y)) 
                    type)))
            seq)
      (EVERY #'(LAMBDA (x) (TYPEP x type)) seq)))

;;; Avoiding package problems with different frame systems.

(DEFTYPE Organism-list () `(SATISFIES Is-organism-list?))
(DEFTYPE Positive-number () `(SATISFIES Is-positive-number?))
(DEFTYPE Positive-integer () `(SATISFIES Is-positive-integer?))
(DEFTYPE Negative-number () `(SATISFIES Is-negative-number?))
; (DEFTYPE Negative-integer () `(SATISFIES Is-negative-integer?))
(DEFTYPE Nonnegative-number () `(SATISFIES Is-nonnegative?))
(DEFTYPE Nonnegative-integer () `(SATISFIES Is-nonnegative-integer?))
(DEFTYPE Table () `utilities::garray)
(DEFTYPE Simple-list () `(SATISFIES Is-simple-list?))
(DEFTYPE Unprovided () `(SATISFIES Unprovided?))
(DEFTYPE Number-string () `(SATISFIES Is-number-string?))
(DEFTYPE Number-list () `(SATISFIES Is-number-list?))
(DEFTYPE coding-gene () `(SATISFIES Is-coding-gene?))

(DEFCONVERSION (Number TO Integer) (n) (ROUND n))
(DEFCONVERSION (Number to number) (n) n)
(DEFCONVERSION (Number TO String) (x)
   (FORWARD-FUNCALL 'BB-STRING-OF x))
(DEFCONVERSION (Positive-number TO Integer) (n) (MAX 1 (ROUND n)))
(DEFCONVERSION (Symbol TO String) (s) (SYMBOL-NAME s))
(DEFCONVERSION (Character TO String) (s) (STRING s))

(DEFCONVERSION (protein TO gene) (p) (SLOTV p #$gene))
(DEFCONVERSION (gene TO protein) (g) (FIRST (SLOTV g #$proteins)))
(defconversion (gene to string) (g) (#^fname g))
(defconversion (gene to organism) (g) (#^organism g))
(defconversion (gene to contiguous-sequence) (g) (#^contiguous-sequence g))
(defconversion (protein to string) (p) (#^fname p))
(defconversion (protein to gene) (p) (#^gene p))
(defconversion (protein to organism) (p) (#^organism p))
(defconversion (protein to contiguous-sequence) (p) (#^contiguous-sequence p))
(defconversion (contiguous-sequence to string) (c) (#^fname c))
(defconversion (contiguous-sequence to organism) (c) (#^organism c))
(DEFCONVERSION (frame to string) (f) (#^FNAME f))
(defconversion (organism to string) (o) (#^fname o))

(DEFCONVERSION (string TO protein) (s)
  (LET* ((conversion (forward-funcall 'bio::seed-id->frame s))
         (type (#^Organism-entity-type conversion))
        )
    (IF (EQUAL type #$Gene)
        (PROTEIN-OF conversion)
        (ERROR "~s cannot be converted to a protein" s))
  ))

(DEFCONVERSION (string TO gene) (s)
  (LET* ((conversion (forward-funcall 'bio::seed-id->frame s))
         (type (#^Organism-entity-type conversion))
        )
    (IF (EQUAL type #$Gene)
        conversion
        (ERROR "~s cannot be converted to a gene" s))
  ))

(DEFCONVERSION (Labeled-sequence TO String) (s) (LABELED-SEQUENCE-SEQUENCE s))

(Defconversion (String TO Number) (s) 
   (LET* ((n (IF (FIND #\e s)
             (FORWARD-FUNCALL 'CONVERT-TO-NUMBER-MAYBE s)
             (READ-FROM-STRING s))))
     (IF (NOT (NUMBERP n))
         (ERROR "'~A' cannot be converted to a number" s))
     n))

(defconversion 
 (string to string) (s)
 (lisp:string s))

(defconversion (string to bbl::frame named string-to-biological-entity) (s) 	 
  (block exit 	 
    (let ((f (frames::frame-fnamed s))) 	 
      (cond 	 
       ((typep f '(or organism contiguous-sequence gene protein)) 	 
        (return-from exit f)) 	 
       (f (error "No biological entity named ~A is known to the system!" s)) 	 
       (t 	 
        (let ((sym (find-symbol (string-upcase s) :bio))) 	 
          (if (and sym (symbol-value sym) (typep (symbol-value sym) 'organism)) 
              (symbol-value sym) 	 
            (error "No biological entity named ~A is known to the system!" s) 	 
            )))))))
               
               

(DEFUN String-number-conversion-function ()
  (LET* ((possible-conversions (GETHASH 'number *defconversion-hash-table*)))
     (CADR (find-if 
             (lambda (x) 
                 (equal (caar x) 'string))
              possible-conversions))))

#|(DEFCONVERSION (Table TO List) (table)
   (GMAP 'IDENTITY table)) |#

(DEFCONVERSION (Table TO List) (table)
   (FORWARD-FUNCALL 'TABLE-TO-LIST-aux table :WITH-LABELS T))

(DEFCONVERSION (Table TO Simple-list) (table)
   (FLATTEN (GMAP 'IDENTITY table)))

(eval
 (ecase cl-user:*frame-system-version*
   (:old 
    (read-from-string 
     "(DEFCONVERSION (Frames:%Frame TO String) (x) (FRAMES:FNAME x))"))
   (:new 
    (read-from-string 
     "(DEFCONVERSION (Frames:%aFrame TO String) (x) (FRAMES:FNAME x))"))
   (:mt
    (read-from-string 
     "(DEFCONVERSION (swframes:frame TO String) (x) (FRAMES::FNAME x))"))
   (:sframes 
    (read-from-string
     "(DEFCONVERSION (frames:frame TO String) (x) (FRAMES::FNAME x))"))
   ))
     

; ******* BIOLOGICAL STRUCTURES *******

(defstruct labeled-sequence
  (label "" :type string)
  (sequence "" :type (or string list))
  )

(DEFSTRUCT domain
   (gene "" :TYPE String)
   (domain "" :TYPE String)
   (start 0 :TYPE Number)
   (stop 0 :TYPE Number)
   (score 0 :TYPE Number)
   (evalue 0 :TYPE Number)
   (definition "" :TYPE String))

(defmethod wb::memory-object-crude-size-method 
           ((obj bbi::labeled-sequence) &optional (error? t))        
  (declare (ignore error?))
  (+ (length (bbi::labeled-sequence-label obj))
     (length (bbi::labeled-sequence-sequence obj))))

; Utility / optimization macros

(defmacro tfix (x) `(the fixnum ,x))
(defmacro tsf (x) `(the single-float ,x))

(defmacro bblme (form)
  `(let ((bbi::*in-bbl-form-processor* t))
     (lisp:macroexpand ',form)))

(defmacro bblme1 (form)
  `(let ((bbi::*in-bbl-form-processor* t))
     (lisp:macroexpand-1 ',form)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *common-vpl-data-menu-variables* 
     '(bbl::true bbl::false bbl::*genbank*
                 bbl::*big-number* #|| bbl::*small-number* ||# 
                 bbl::*tab* bbl::*newline* 
                 bbl::*nucleotides* bbl::*amino-acids* bbl::*end*
      ))

(defgeneric vpl-data-menu-variables (instance organism-descriptor)
  (:documentation
   #.(one-string-nl
      "Returns a list of variables which are to be placed on the DATA"
      "palette menu in the VPL.")))

(defmethod vpl-data-menu-variables ((instance t) (organism-descriptor t))
  nil)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-symbol-macro *all-organisms* (biolisp::loaded-organisms))

(DEFINE-SYMBOL-MACRO *organisms-with-orthologs* 
       (INTERSECTION (biolisp::CROSSBLAST-ORGANISMS) *all-organisms*))

;; Does not return NIL for symbols without loaded organisms 
(defun organism-symbol-values (symbols)
  (loop for org-symbol in symbols
        as value = (organism-symbol-value org-symbol)
        when value collect value
        ))

(defun organism-symbol-value (symbol)
  (vwhen (s (find-symbol (string symbol) :oa))
    (when (boundp s) (symbol-value s))
    ))

;;; Stuff specific to organisms 

(defgeneric organism-subset-variables (organisms-descriptor)
  (:documentation
   #.(one-string-nl
      "Returns a list of variables whose values are lists of organisms."
      "These subsets are supposed to be mnemonic and should define useful"
      "subsets of the organism set this instance is dealing with."
      )))

;;; Load a methods and data file specific to the type of organisms
;;; this instance is dealing with.  Specifically the organism-subset-variables
;;; method.  
(when *load-pathname*
  (let* ((od-source-file-name
          (s+ (string-downcase cl-user::*organisms-descriptor*) "-data.lisp"))
         (od-source-path (merge-pathnames od-source-file-name *load-pathname*)))
    (if (probe-file od-source-path) 
        (handler-case 
            (cl-user::compile/load od-source-path)
          (error () (load od-source-path)))
      (formatt ";; No BBL data file for *organisms-descriptor* ~S" 
               cl-user::*organisms-descriptor*
               ))))

(defparameter *organism-subsets* 
  (organism-subset-variables cl-user::*organisms-descriptor*))

;; Very strange lispworks bug.  Shadowing-import causes NIL to no longer
;; be an exported symbol of BBL!! (But import works fine)
(defun export-organism-variables ()
  (export *organism-subsets* :bbi)
  #-:lispworks
  (shadowing-import *organism-subsets* :bbl)
  #+:lispworks
  (import *organism-subsets* :bbl)
  (export *organism-subsets* :bbl)
  )

(export-organism-variables)

;;; Stuff specific to the :cyanobacteria *organisms-descriptor* value


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(DEFCONSTANT *Carbohydrate-Metabolism*
    '(("00010" "Glycolysis / Gluconeogenesis")
    ("00020" "Citrate cycle (TCA cycle)")
    ("00030" "Pentose phosphate pathway")
    ("00040" "Pentose and glucuronate interconversions")
    ("00051" "Fructose and mannose metabolism")
    ("00052" "Galactose metabolism")
    ("00053" "Ascorbate and aldarate metabolism")
    ("00500" "Starch and sucrose metabolism")
    ("00530" "Aminosugars metabolism")
    ("00520" "Nucleotide sugars metabolism")
    ("00620" "Pyruvate metabolism")
    ("00630" "Glyoxylate and dicarboxylate metabolism")
    ("00640" "Propanoate metabolism")
    ("00650" "Butanoate metabolism")
    ("00660" "C5-Branched dibasic acid metabolism")
    ("00031" "Inositol metabolism")
    ("00562" "Inositol phosphate metabolism")))

(DEFCONSTANT *Energy-Metabolism*
    '(("00190" "Oxidative phosphorylation")
    ("00195" "Photosynthesis")
    ("00196" "Photosynthesis - antenna proteins")
    ("00710" "Carbon fixation")
    ("00720" "Reductive carboxylate cycle (CO2 fixation)")
    ("00680" "Methane metabolism")
    ("00910" "Nitrogen metabolism")
    ("00920" "Sulfur metabolism")
    ("00191" "Pyruvate/Oxoglutarate oxidoreductases")
    ("00192" "ATPases")))

(DEFCONSTANT *Lipid-Metabolism*
    '(("00061" "Fatty acid biosynthesis")
    ("00062" "Fatty acid elongation in mitochondria")
    ("00071" "Fatty acid metabolism")
    ("00072" "Synthesis and degradation of ketone bodies")
    ("00100" "Biosynthesis of steroids")
    ("00120" "Bile acid biosynthesis")
    ("00140" "C21-Steroid hormone metabolism")
    ("00150" "Androgen and estrogen metabolism")
    ("00561" "Glycerolipid metabolism")
    ("00564" "Glycerophospholipid metabolism")
    ("00565" "Ether lipid metabolism")
    ("00600" "Sphingolipid metabolism")
    ("00590" "Arachidonic acid metabolism")
    ("00591" "Linoleic acid metabolism")))

(DEFCONSTANT *Nucleotide-Metabolism*
    '(("00230" "Purine metabolism")
    ("00240" "Pyrimidine metabolism")))

(DEFCONSTANT *Amino-Acid-Metabolism*
    '(("00251" "Glutamate metabolism")
    ("00252" "Alanine and aspartate metabolism")
    ("00260" "Glycine, serine and threonine metabolism")
    ("00271" "Methionine metabolism")
    ("00272" "Cysteine metabolism")
    ("00280" "Valine, leucine and isoleucine degradation")
    ("00290" "Valine, leucine and isoleucine biosynthesis")
    ("00300" "Lysine biosynthesis")
    ("00310" "Lysine degradation")
    ("00330" "Arginine and proline metabolism")
    ("00340" "Histidine metabolism")
    ("00350" "Tyrosine metabolism")
    ("00360" "Phenylalanine metabolism")
    ("00380" "Tryptophan metabolism")
    ("00400" "Phenylalanine, tyrosine and tryptophan biosynthesis")
    ("00220" "Urea cycle and metabolism of amino groups")))

(DEFCONSTANT *Metabolism-of-Other-Amino-Acids*
    '(("00410" "beta-Alanine metabolism")
    ("00430" "Taurine and hypotaurine metabolism")
    ("00440" "Aminophosphonate metabolism")
    ("00450" "Selenoamino acid metabolism")
    ("00460" "Cyanoamino acid metabolism")
    ("00471" "D-Glutamine and D-glutamate metabolism")
    ("00472" "D-Arginine and D-ornithine metabolism")
    ("00473" "D-Alanine metabolism")
    ("00480" "Glutathione metabolism")))

(DEFCONSTANT *Glycan-Biosynthesis-and-Metabolism*
    '(("00510" "N-Glycan biosynthesis")
    ("00513" "High-mannose type N-glycan biosynthesis")
    ("00511" "N-Glycan degradation")
    ("00512" "O-Glycan biosynthesis")
    ("00532" "Chondroitin sulfate biosynthesis")
    ("00534" "Heparan sulfate biosynthesis")
    ("00533" "Keratan sulfate biosynthesis")
    ("00531" "Glycosaminoglycan degradation")
    ("00540" "Lipopolysaccharide biosynthesis")
    ("00550" "Peptidoglycan biosynthesis")
    ("00563" "Glycosylphosphatidylinositol(GPI)-anchor biosynthesis")
    ("00601" "Glycosphingolipid biosynthesis - lactoseries")
    ("00602" "Glycosphingolipid biosynthesis - neo-lactoseries")
    ("00603" "Glycosphingolipid biosynthesis - globoseries")
    ("00604" "Glycosphingolipid biosynthesis - ganglioseries")))

(DEFCONSTANT *Biosynthesis-of-Polyketides-and-Nonribosomal-Peptides*
    '(("01052" "Type I polyketide structures")
    ("00522" "Biosynthesis of 12-, 14- and 16-membered macrolides")
    ("01051" "Biosynthesis of ansamycins")
    ("01056" "Biosynthesis of type II polyketide backbone")
    ("01057" "Biosynthesis of type II polyketide products")
    ("00523" "Polyketide sugar unit biosynthesis")
    ("01054" "Nonribosomal peptide structures")
    ("01053" "Biosynthesis of siderophore group nonribosomal peptides")
    ("01055" "Biosynthesis of vancomycin group antibiotics")))

(DEFCONSTANT *Metabolism-of-Cofactors-and-Vitamins*
    '(("00730" "Thiamine metabolism")
    ("00740" "Riboflavin metabolism")
    ("00750" "Vitamin B6 metabolism")
    ("00760" "Nicotinate and nicotinamide metabolism")
    ("00770" "Pantothenate and CoA biosynthesis")
    ("00780" "Biotin metabolism")
    ("00790" "Folate biosynthesis")
    ("00670" "One carbon pool by folate")
    ("00830" "Retinol metabolism")
    ("00860" "Porphyrin and chlorophyll metabolism")
    ("00130" "Ubiquinone biosynthesis")))

(DEFCONSTANT *Biosynthesis-of-Secondary-Metabolites*
    '(("00900" "Terpenoid biosynthesis")
    ("00902" "Monoterpenoid biosynthesis")
    ("00903" "Limonene and pinene degradation")
    ("00904" "Diterpenoid biosynthesis")
    ("00905" "Brassinosteroid biosynthesis")
    ("00901" "Indole and ipecac alkaloid biosynthesis")
    ("00940" "Stilbene, coumarine and lignin biosynthesis")
    ("00941" "Flavonoid biosynthesis")
    ("00950" "Alkaloid biosynthesis I")
    ("00960" "Alkaloid biosynthesis II")
    ("01058" "Acridone alkaloid biosynthesis")
    ("00311" "Penicillin and cephalosporin biosynthesis")
    ("00312" "beta-Lactam resistance")
    ("00521" "Streptomycin biosynthesis")
    ("00253" "Tetracycline biosynthesis")
    ("00331" "Clavulanic acid biosynthesis")
    ("00231" "Puromycin biosynthesis")
    ("00401" "Novobiocin biosynthesis")
    ("00402" "Benzoxazinone biosynthesis")))

(DEFCONSTANT *Xenobiotics-Biodegradation-and-Metabolism*
    '(("00930" "Caprolactam degradation")
    ("00621" "Biphenyl degradation")
    ("00622" "Toluene and xylene degradation")
    ("00361" "gamma-Hexachlorocyclohexane degradation")
    ("00641" "3-Chloroacrylic acid degradation")
    ("00351" "1,1,1-Trichloro-2,2-bis(4-chlorophenyl)ethane (DDT) degradation")
    ("00623" "2,4-Dichlorobenzoate degradation")
    ("00631" "1,2-Dichloroethane degradation")
    ("00625" "Tetrachloroethene degradation")
    ("00643" "Styrene degradation")
    ("00627" "1,4-Dichlorobenzene degradation")
    ("00626" "Nitrobenzene degradation")
    ("00642" "Ethylbenzene degradation")
    ("00628" "Fluorene degradation")
    ("00629" "Carbazole degradation")
    ("00632" "Benzoate degradation via CoA ligation")
    ("00362" "Benzoate degradation via hydroxylation")
    ("00791" "Atrazine degradation")
    ("00363" "Bisphenol A degradation")
    ("00624" "1- and 2-Methylnaphthalene degradation")
    ("00980" "Metabolism of xenobiotics by cytochrome P450")))

(DEFCONSTANT *Transcription*
    '(("03020" "RNA polymerase")
    ("03022" "Basal transcription factors")
    ("03028" "Other transcription related proteins")
    ("03040" "Spliceosome")))

(DEFCONSTANT *Translation*
    '(("03010" "Ribosome")
    ("00970" "Aminoacyl-tRNA biosynthesis")
    ("03014" "Other translation proteins")))

(DEFCONSTANT *Folding-Sorting-and-Degradation*
    '(("03100" "Protein folding and associated processing")
    ("03060" "Protein export")
    ("03090" "Type II secretion system")
    ("03070" "Type III secretion system")
    ("03080" "Type IV secretion system")
    ("04130" "SNARE interactions in vesicular transport")
    ("04140" "Regulation of autophagy")
    ("04120" "Ubiquitin mediated proteolysis")
    ("03050" "Proteasome")
    ("03018" "RNA degradosome")))

(DEFCONSTANT *Replication-and-Repair*
    '(("03030" "DNA polymerase")
    ("03032" "Replication complex")
    ("03034" "Other replication, recombination and repair proteins")))

(DEFCONSTANT *Membrane-Transport*
    '(("02010" "ABC transporters")
    ("02052" "Other ion-coupled transporters")
    ("02070" "Pores ion channels")
    ("02080" "Electron transfer carriers")
    ("02082" "Other transporters")
    ("02060" "Phosphotransferase system (PTS)")))

(DEFCONSTANT *Signal-Transduction*
    '(("02020" "Two-component system")
    ("04010" "MAPK signaling pathway")
    ("04310" "Wnt signaling pathway")
    ("04330" "Notch signaling pathway")
    ("04340" "Hedgehog signaling pathway")
    ("04350" "TGF-beta signaling pathway")
    ("04370" "VEGF signaling pathway")
    ("04630" "Jak-STAT signaling pathway")
    ("04020" "Calcium signaling pathway")
    ("04070" "Phosphatidylinositol signaling system")
    ("04150" "mTOR signaling pathway")))

(DEFCONSTANT *Signaling-Molecules-and-Interaction*
    '(("04080" "Neuroactive ligand-receptor interaction")
    ("04060" "Cytokine-cytokine receptor interaction")
    ("04512" "ECM-receptor interaction")
    ("04514" "Cell adhesion molecules (CAMs)")))

(DEFCONSTANT *Cell-Motility*
    '(("02030" "Bacterial chemotaxis")
    ("02040" "Flagellar assembly")
    ("04810" "Regulation of actin cytoskeleton")))

(DEFCONSTANT *Cell-Growth-and-Death*
    '(("04410" "Cell division")
    ("04420" "Sporulation")
    ("04430" "Germination")
    ("04110" "Cell cycle")
    ("04111" "Cell cycle - yeast")
    ("04210" "Apoptosis")))

(DEFCONSTANT *Cell-Communication*
    '(("04510" "Focal adhesion")
    ("04520" "Adherens junction")
    ("04530" "Tight junction")
    ("04540" "Gap junction")))

(DEFCONSTANT *Endocrine-System*
    '(("04910" "Insulin signaling pathway")
    ("04920" "Adipocytokine signaling pathway")
    ("03320" "PPAR signaling pathway")
    ("04912" "GnRH signaling pathway")
    ("04914" "Progesterone-mediated oocyte maturation")
    ("04916" "Melanogenesis")))

(DEFCONSTANT *Immune-System*
    '(("04640" "Hematopoietic cell lineage")
    ("04610" "Complement and coagulation cascades")
    ("04620" "Toll-like receptor signaling pathway")
    ("04650" "Natural killer cell mediated cytotoxicity")
    ("04612" "Antigen processing and presentation")
    ("04660" "T cell receptor signaling pathway")
    ("04662" "B cell receptor signaling pathway")
    ("04664" "Fc epsilon RI signaling pathway")
    ("04670" "Leukocyte transendothelial migration")))

(DEFCONSTANT *Nervous-System*
    '(("04720" "Long-term potentiation")
    ("04730" "Long-term depression")))

(DEFCONSTANT *Sensory-System*
    '(("04740" "Olfactory transduction")
    ("04742" "Taste transduction")))

(DEFCONSTANT *Development*
    '(("04320" "Dorso-ventral axis formation")
    ("04360" "Axon guidance")))

(DEFCONSTANT *Behavior*
    '(("04710" "Circadian rhythm")
    ("01500" "Human Diseases")))

(DEFCONSTANT *Neurodegenerative-Disorders*
    '(("05010" "Alzheimer s disease")
    ("05020" "Parkinson s disease")
    ("05030" "Amyotrophic lateral sclerosis (ALS)")
    ("05040" "Huntington s disease")
    ("05050" "Dentatorubropallidoluysian atrophy (DRPLA)")
    ("05060" "Prion disease")))

(DEFCONSTANT *Infectious-Diseases*
    '(("05110" "Cholera")
    ("05120" "Epithelial cell signaling in Helicobacter pylori infection")
    ("05130" "Pathogenic Escherichia coli infection")))

(DEFCONSTANT *Metabolic-Disorders*
    '(("04940" "Type I diabetes mellitus")
    ("04930" "Type II diabetes mellitus")
    ("04950" "Maturity onset diabetes of the young")))

(DEFCONSTANT *Cancers*
    '(("05210" "Colorectal cancer")
    ("05212" "Pancreatic cancer")
    ("05214" "Glioma")
    ("05216" "Thyroid Cancer")
    ("05220" "Chronic myeloid leukemia")
    ("05217" "Basal cell carcinoma")
    ("05218" "Melanoma")
    ("05211" "Renal cell carcinoma")))

