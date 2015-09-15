;;;; -*- mode: Lisp; Syntax: Common-Lisp; Package: bbi; -*-

(in-package :bbi)

(defconstant gene-record-length 230)
(defconstant file-header-length 5)
(defconstant end-of-record-marker #x45)
(defconstant orftext-float-header #\N)

(defvar *orftext-stream* nil)
(defvar *orftext-direction* :little-endian)

(defun make-orftext-files 
       (organism
        orffile-prefix
        &key
        (directory "C:/lispcode/biolisp/")
        (convert-lf-to-crlf nil))
  (format t "~%Creating Co.txt file in directory ~A...~%" directory)
  (make-orftext-co-file 
   organism orffile-prefix :directory directory 
   :convert-lf-to-crlf convert-lf-to-crlf)
  (format t "~%Creating DBLU.DAT file in directory ~A...~%" directory )
  (make-orftext-dblu-file 
   organism orffile-prefix :directory directory)
  (format t "~%Creating DB.DAT file in directory ~A...~%" directory)
  (make-orftext-db-file 
   organism orffile-prefix :directory directory)
  )

(defun make-orftext-dblu-file 
       (org 
        file-prefix
        &key
        (direction *orftext-direction*)
        (directory "C:/lispcode/biolisp/")
        &aux 
        (file (s+ directory file-prefix "DBLU.DAT")))
         
  (with-open-file 
      (p file
         :element-type '(unsigned-byte 8)
         :direction :output
         :if-does-not-exist :create
         :if-exists :supersede)
    (let* ((*orftext-stream* p)
           (*orftext-direction* direction)
           (contigs 
; ******
                (FOR-EACH gene IN (#^Genes org)
                     INITIALIZE contigs = NIL
                     AS contig = (#^Contiguous-sequence gene)
                     DO (IF (NOT (MEMBER contig contigs :TEST 'EQUAL))
                            (PUSH contig contigs))
                     FINALLY (RETURN (REVERSE contigs))))
; ******
           (ncontigs (length contigs))
           (record-number 1))
        
      (write-dblu-file-header)
      
      (write-float-header)
      (write-double-precision-float (float ncontigs 1.0d0))
      
      (write-float-header)
      ;; first record of first contig
      (write-double-precision-float (float record-number 1.0d0))
      
      ;; record number of the first gene of each subsequent contig
      ;; and record number which is 1 more than last gene of last contig
      (loop for c in contigs
            as gene-count = (length (genes-of c))
            do
            (incf record-number gene-count)
            (write-float-header)
            (write-double-precision-float (float record-number 1.0d0))
            ))))
      
(defun make-orftext-db-file 
       (org 
        file-prefix
        &key
        (directory "C:/lispcode/biolisp/")
        (other-org1 org) 
        (other-org2 org)
        (direction *orftext-direction*)
        &aux 
        (file (s+ directory file-prefix "DB.DAT")))
  
  (let* ((genes (bbi::genes-of org)))

    (with-open-file 
        (p file
           :element-type '(unsigned-byte 8)
           :direction :output
           :if-does-not-exist :create
           :if-exists :supersede)
      (let ((*orftext-stream* p)
            (*orftext-direction* direction))
        
        ;; FILE HEADER
        (write-db-file-header)
      
        ;; three characters that contain the length of each record
        (write-three-byte-integer gene-record-length p)

        (loop for gene in genes
              as gene-name = (bbi::name-of gene short)
              as contig = (slotv gene #$contiguous-sequence)
              as contig-orfid = (slotv contig #$orfid)
              for j from 0 
              do
              
              (unless contig-orfid
                (error "You must have an orfid for each contig (via co-file)."))
      
              (when (zerop (mod j 100)) (format t "."))

              ;; STRING HEADER
              ;; S character
              
              ;; Get gene name and massage it to be exactly 11 characters
              (setq gene-name 
                    (make-left-justified-string-of-exact-length gene-name 11))
              (write-string-header 11)
              ;; Orfname -- must be exactly 11 characters
              (write-string-as-bytes gene-name)

              ;; string header for contig
              (write-string-header 4)
              ;; contig id as defined by the code that creates the 
              ;; co file
              (write-string-as-bytes 
               (make-left-justified-string-of-exact-length 
                contig-orfid 4
                ))

              ;; Float header
              (write-float-header)
              ;; left-boundary
              (write-double-precision-float (float (#^from gene) 1.0d0))

              ;; Float header
              (write-float-header)
              ;; right-boundary
              (write-double-precision-float (float (#^to gene) 1.0d0))
              
              ;; string header
              (write-string-header 1)
              ;; direction
              (let ((direction (#^direction gene)))
                (if (equal direction :F) 
                    (write-byte (char-code #\d) p)
                  (write-byte (char-code #\c) p)
                  ))
              
              ;; string header
              (write-string-header 14)
              ;; accession number -- must be 14 characters
              (write-string-as-bytes
               (make-left-justified-string-of-exact-length 
                (or (slotv gene #$best-hit-accession) "") 14))
        
              ;; string header
              (write-string-header 5)
              ;; pct
              (write-string-as-bytes
               (make-left-justified-string-of-exact-length 
                (or (slotv gene #$best-hit-id-pct) "-1") 5))

              ;; float header
              (write-float-header)
              ;; expvsnr
              (let ((bhe (#^best-hit-evalue gene)))
                (setq bhe
                      (cond
                       ((stringp bhe) 
                        (setq bhe (substitute #\d #\e bhe :test 'char-equal))
                        (setq bhe (read-from-string bhe nil nil)))
                       ((floatp bhe) bhe)
                       ((null bhe) -1.0d0)
                       (t (error "Best hit evalue of weird type: ~A" bhe))))
                (write-double-precision-float bhe)
                )

              ;; string header
              (write-string-header 50)
              ;; description -- must be 50 characters
              (write-string-as-bytes
               (make-left-justified-string-of-exact-length
                (or (description-of gene length 50) "") 50))
              
              ;; string header
              (write-string-header 5)
              ;; types
              (write-byte 
               (if (slotv gene #$encodes-protein) 
                   (char-code #\P)
                 (char-code #\R)
                 )
               *orftext-stream*
               )
              (loop for j from 1 to 4 do (write-byte 255 *orftext-stream*))

              ;; string header
  ; ***       (write-string-header 15)
              (write-string-header 11)
              ;; hit name 1
              (let* ((hit1 
                      (when (and other-org1 (not (eq other-org1 org)))
                        (ortholog-of gene in other-org1)
                        ))
                     (hit-name-1 
                      (if hit1 (name-of hit1 short) "")
                      ))
                (write-string-as-bytes 
                 (make-left-justified-string-of-exact-length
  ; ***           hit-name-1 15
                  hit-name-1 11
                  )))
        
              ;; float header
              (write-float-header)
              ;; num field 1a
              (write-eight-byte-dummy)

              ;; float header
              (write-float-header)
              ;; num field 1b
              (write-eight-byte-dummy)
              
              ;; float header
              (write-float-header)
              ;; num field 1c
              (write-eight-byte-dummy)
              
              ;; string header
  ; ***       (write-string-header 15)
              (write-string-header 11)
              ;; hitname2
              (let* ((hit2
                      (when (and other-org2 (not (eq other-org2 org)))
                        (ortholog-of gene in other-org2)
                        ))
                     (hit-name-2
                      (if hit2 (name-of hit2 short) "")
                      ))
                (write-string-as-bytes 
                 (make-left-justified-string-of-exact-length
  ; ***           hit-name-2 15
                  hit-name-2 11
                  )))
              
              ;; float header
              (write-float-header)
              ;; num-field-2a
              (write-eight-byte-dummy)
              
              ;; float header
              (write-float-header)
              ;; num-field-2b
              (write-eight-byte-dummy)
              
              ;; float header
              (write-float-header)
              ;; numfield2c
              (write-eight-byte-dummy)
              
              ;; end of record marker
              (write-byte end-of-record-marker *orftext-stream*)

              )))))


(defun make-orftext-co-file 
       (organism
        file-prefix
        &key
        (directory "C:/lispcode/biolisp/")
        (convert-lf-to-crlf nil)
        &aux
        (co-file (s+ directory file-prefix "Co.txt")))
  
  (let* ((genome-file (join (slotv organism #$organism-data-directory)
                            "genome/genome.fasta"))
         (line-length (length (second (bbl::read genome-file))))
         (sequences (bbl::read genome-file fasta))
         (number-of-replicons (length sequences))
         (topology (if (slotv (first (replicons-of organism)) #$circular)
                       "C" "L"))
         (abbreviated (IF (EQUAL topology "C") "A" "N"))
         (lines 
          (join (fit (list number-of-replicons line-length topology abbreviated)
                  into 15 flush-right)))
         (replicon-names (names-of sequences))
         (replicons
          (for-each name in replicon-names
                    initialize replicon-frames = (replicons-of organism)
                    initialize frame-names = (names-of replicon-frames short)
                    as position = (position-of name in frame-names)
                    as replicon = (ref replicon-frames position)
                    collect replicon
                    )))

    (setf 
     lines
     (join 
      (list lines)
      (for-each 
       replicon in replicons
       for-each name in replicon-names
       for-each i FROM 1
       initialize byte = 0
       assign len = (length-of replicon)
       assign name-len = (length-of name)
       assign number-of-crs = (bbl::round (/ len line-length) down)
       assign number-of-bytes = (+ len (* number-of-crs 2))
       assign first-byte 
; *****
         = (IF (= i 1)
               (incf byte (+ name-len 4))
               (incf byte (+ name-len 6)))
; *****
       assign last-byte = (incf byte (+ number-of-bytes -1))
       assign abbrev 
       = (if (same name "chromosome")
             "C"
           (setf abbrev (BB-STRING-OF i)))
       do
       (setf (ref replicon #$orfid) abbrev)
       collect 
       (join (fit (IF (EQUAL abbreviated "A")
                      (list name abbrev first-byte last-byte len)
                    (list name first-byte last-byte len))
               into 15 flush-right)))
       as-list))
    (with-open-file (p co-file :direction :output :if-exists :supersede)
      (loop for line in lines do (format p "~A~%" line))
      )
    (when convert-lf-to-crlf
      (utils::convert-linefeeds-to-crlfs co-file))
    ))

(defun write-dblu-file-header (&optional (stream *orftext-stream*))
  ;; code char for that funky O thing
  (write-byte #xd4 stream)
  (write-byte #x4a stream)
  (loop for j from 1 to 3 do (write-byte #xff stream)))

(defun write-db-file-header (&optional (stream *orftext-stream*))
  ;; code char for that funky O thing
  (write-byte #xd4 stream)
  (write-byte #x58 stream)
  )

(defun orftext-file (directory org suffix)
  (let ((path (s+ directory (name-of org short) suffix)))
    (when (> (length (pathname-name path)) 8)
      (error "Orftext file names must be 8 or less character (DOS)."))
    path
    ))
      

(defun make-left-justified-string-of-exact-length (s n)
  (if (> (length s) n)
      (subseq s 0 n)
    (let ((format (format nil "~~~D" n)))
      (format nil (utils::s+ format "a") s)
      )))

(defun integer-byte (x n)
  (ldb (byte 8 (* n 8)) x))

(defun write-float-header (&optional (stream *orftext-stream*))
  (write-byte (char-code orftext-float-header) stream))

(defun write-string-header 
       (length &optional (stream *orftext-stream*) 
               &key (direction *orftext-direction*))
  (write-byte (char-code #\S) stream)
  (write-three-byte-integer 
   length stream :direction direction))

(defun write-string-as-bytes (s &optional (stream *orftext-stream*))
  (loop for j from 0 below (length s)
        do
        (write-byte (char-code (char s j)) stream)
        ))

(defun write-three-byte-integer 
       (number &optional (stream *orftext-stream*)
               &key (direction *orftext-direction*))
  (if (equal direction :big-endian)
      (progn
        (write-byte (integer-byte number 0) stream)
        (write-byte (integer-byte number 1) stream)
        (write-byte (integer-byte number 2) stream))
    (progn
      (write-byte (integer-byte number 2) stream)
      (write-byte (integer-byte number 1) stream)
      (write-byte (integer-byte number 0) stream)
      )))

(defun write-four-byte-integer
       (number &optional (stream *orftext-stream*)
               &key (direction *orftext-direction*))
  (if (equal direction :big-endian)
      (progn
        (write-byte (integer-byte number 0) stream)
        (write-byte (integer-byte number 1) stream)
        (write-byte (integer-byte number 2) stream)
        (write-byte (integer-byte number 3) stream))
    (progn
      (write-byte (integer-byte number 3) stream)
      (write-byte (integer-byte number 2) stream)
      (write-byte (integer-byte number 1) stream)
      (write-byte (integer-byte number 0) stream)
      )))

(defun write-eight-byte-integer 
       (i8 &optional (stream *orftext-stream*)
           &key (direction *orftext-direction*))
  (if (equal direction :big-endian)
      (loop for x from 0 to 7
            as b = (integer-byte i8 x)
            do
            (write-byte b stream)
            )
    (loop for x from 7 downto 0
          do
          (write-byte (integer-byte i8 x) stream)
          )))

(defun write-eight-byte-dummy 
       (&optional (stream *orftext-stream*)
                  &key (direction *orftext-direction*))
  (write-double-precision-float -1.0d0 stream :direction direction))

(defun write-double-precision-float 
       (f &optional (stream *orftext-stream*)
          &key (direction *orftext-direction*))
  (let ((eight-byte-integer (ieee-floats::encode-float64 f)))
    (write-eight-byte-integer eight-byte-integer stream :direction direction)
    ))

(defun read-three-byte-integer 
       (&optional (stream *orftext-stream*) 
                  &key (direction *orftext-direction*))
  (let ((b1 (read-byte stream nil nil))
        (b2 (read-byte stream nil nil))
        (b3 (read-byte stream nil nil)))
    (if (eq direction :big-endian)
        (+ b1 (* 256 b2) (* 256 256 b3))
      (+ b3 (* 256 b2) (* 256 256 b1))
      )))

(defun read-four-byte-integer 
       (&optional (stream *orftext-stream*) 
                  &key (direction *orftext-direction*))
  (let ((b1 (read-byte stream nil nil))
        (b2 (read-byte stream nil nil))
        (b3 (read-byte stream nil nil))
        (b4 (read-byte stream nil nil)))
    (if (eq direction :big-endian)
        (+ b1 (* 256 b2) (* 256 256 b3) (* 256 256 256 b4))
      (+ b4 (* 256 b3) (* 256 256 b2) (* 256 256 256 b1))
      )))

(defun read-eight-byte-integer 
       (&optional (stream *orftext-stream*) 
                  &key (direction *orftext-direction*))
  (let ((b1 (read-byte stream nil nil))
        (b2 (read-byte stream nil nil))
        (b3 (read-byte stream nil nil))
        (b4 (read-byte stream nil nil))
        (b5 (read-byte stream nil nil))
        (b6 (read-byte stream nil nil))
        (b7 (read-byte stream nil nil))
        (b8 (read-byte stream nil nil)))
    (if (eq direction :big-endian)
        (+ b1 (* 256 b2) (* 256 256 b3) (* 256 256 256 b4)
           (* 256 256 256 256 b5) (* 256 256 256 256 256 b6) 
           (* 256 256 256 256 256 256 b7) (* 256 256 256 256 256 256 256 b8))
      (+ b8 (* 256 b7) (* 256 256 b6) (* 256 256 256 b5)
         (* 256 256 256 256 b4) (* 256 256 256 256 256 b3)
         (* 256 256 256 256 256 256 b2) (* 256 256 256 256 256 256 256 b1)
         ))))

(defun show-orfdblu-file (file)
  (with-open-file 
      (p file :direction :input :element-type '(unsigned-byte 8))
    (let ((file-header-prefix-1 (read-byte p))
          (file-header-prefix-2 (read-byte p))
          (dummy1 (read-byte p))
          (dummy2 (read-byte p))
          (dummy3 (read-byte p))
          )
      (declare (ignore dummy1 dummy2 dummy3))
      (unless (= file-header-prefix-1 #xd4)
        (error "DBLU first prefix byte invalid: ~A, should be ~A"
               file-header-prefix-1 #xd4))
      (unless (= file-header-prefix-2 #x4a)
        (error "DBLU first prefix byte invalid: ~A, should be ~A"
               file-header-prefix-2 #x4a))
      (let ((ncontigs 
             (round (read-float-header-then-double-precision-float p)))
            (first-index 
             (round (read-float-header-then-double-precision-float p))))
        (format t "Number of contigs: ~D~%" ncontigs)
        (format t "First index: ~D~%" first-index)
        (loop for j from 1 below ncontigs 
              as index = 
              (round (read-float-header-then-double-precision-float p))
              do 
              (format t "Index of contig ~D: ~D~%" (1+ j) index)
              )))))

(defvar *byte-count* 0)

(defun show-orfdb-file (file &key (n 2))
  (with-open-file (p file :direction :input)
    (let ((file-header-prefix-1 (read-byte p))
          (file-header-prefix-2 (read-byte p))
          (*byte-count* 0)
          (one-record-size (read-three-byte-integer p)))
      (unless (= file-header-prefix-1 #xd4)
        (error "DB first prefix byte invalid: ~A, should be ~A"
               file-header-prefix-1 #xd4))
      (unless (= file-header-prefix-2 #x58)
        (error "DB first prefix byte invalid: ~A, should be ~A"
               file-header-prefix-2 #x58))
      (format t "One record size: ~D~%" one-record-size)
      
      (loop for j from 0 below n
            do
            (let ((gene-name (read-string-header-then-string p))
                  (contig-name (read-string-header-then-string p)))
              (format t "Gene name: ~A~%" gene-name)
              (format t "Contig name: ~A~%" contig-name)
              )
            (let ((from (round 
                         (read-float-header-then-double-precision-float p)))
                  (to (round 
                       (read-float-header-then-double-precision-float p)))
                  (dir (read-string-header-then-string p))
                  (accession (read-string-header-then-string p))
                  (pct (read-string-header-then-string p))
                  (expvsnr (read-float-header-then-double-precision-float p))
                  (description (read-string-header-then-string p))
                  (types (read-string-header-then-string p))
                  (hitname1 (read-string-header-then-string p))
                  (field1a (read-float-header-then-double-precision-float p))
                  (field1b (read-float-header-then-double-precision-float p))
                  (field1c (read-float-header-then-double-precision-float p))
                  (hitname2 (read-string-header-then-string p))
                  (field2a (read-float-header-then-double-precision-float p))
                  (field2b (read-float-header-then-double-precision-float p))
                  (field2c (read-float-header-then-double-precision-float p))
                  (end-of-record (read-byte p))
                  )
              (declare (ignore end-of-record))
              (incf *byte-count*)
              (format t "From: ~D~%" from)
              (format t "To: ~D~%" to)
              (format t "Direction: ~A~%" 
                      (cond
                       ((equalp dir "D") :F)
                       ((equalp dir "C") :B)
                       (t (error "Direction malformed!"))
                       ))
              (format t "Accession: ~A~%" accession)
              (format t "PCT: ~A~%" pct)
              (format t "EXPvsNR: ~F~%" expvsnr)
              (format t "description: ~A~%" description)
              (format t "types: ~A~%" types)
              (format t "hitname1: ~A~%" hitname1)
              (format t "field1a: ~F~%" field1a)
              (format t "field1b: ~F~%" field1b)
              (format t "field1c: ~F~%" field1c)
              (format t "hitname2: ~A~%" hitname2)
              (format t "field2a: ~F~%" field2a)
              (format t "field2b: ~F~%" field2b)
              (format t "field2c: ~F~%" field2c)
              ;; (format t "*byte-count* = ~D~%" *byte-count*)
              )))))
              

(defun read-float-header-then-double-precision-float 
       (&optional (stream *orftext-stream*)) 
  (let ((header (code-char (read-byte stream nil nil))))
    (unless (char-equal header orftext-float-header)
      (error "Float header is not #\n!")))
  (incf *byte-count*)
  (let ((i8 (read-eight-byte-integer stream)))
    (incf *byte-count* 8)
    (ieee-floats::decode-float64 i8)
    ))

(defun read-string-header-then-string 
       (&optional (stream *orftext-stream*)) 
  (let ((header (code-char (read-byte stream nil nil))))
    (unless (char-equal header #\s)
      (error "Oops!  String-header is not #\\S!"))
    (incf *byte-count*)
    (let* ((string-length (read-three-byte-integer stream))
           (s (make-string string-length)))
      ;; (print (list 'string-length string-length))
      (incf *byte-count* 3)
      (loop for j from 0 below string-length do 
            (setf (char s j) (code-char (read-byte stream nil nil))))
      (incf *byte-count* string-length)
      s
      )))
            


