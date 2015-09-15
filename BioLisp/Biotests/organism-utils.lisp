(in-package :bio)

(defun 2wayo (x y) (two-way-ortholog-of x y 1.0e-10))

(defun g1 () (extract-test-gene-subsequence 18 2 4))
(defun g2 () (extract-test-gene-subsequence 18 6 8))
(defun g3 () (extract-test-gene-subsequence 18 10 12))
(defun g4 () (extract-test-gene-subsequence 18 16 18))

(defun extract-test-gene-subsequence (n start end)
  (declare (ignorable n))
  #-:sframes
  (subseq (#^genes (symbol-value 'oa::npun)) start end)
  #+:sframes 
  (subseq (#^genes (find-org-with-enough-genes n)) start end)
  )

(defun find-org-with-enough-genes (n)
  (block exit
    (loop for org in *loaded-organisms*
          do
          (when (>= (length (#^genes org)) n)
            (return-from exit org)
            ))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun rnl (x) (remove #\Newline x)))

;; the strings passed to the cftr and bmp defparameters come  from 
;; http://www.sciencebuddies.org/mentoring/project_ideas/Genom_p004.shtml?from=Home
;; there are two other genes on the site, leptin and ospin1

(defparameter human-cftr 
  (rnl "ATATTTGAAAGCTGTGTCTGTAAACTGATGGCTAACAAAACTAGGATTTTGGTCACTTC
TAAAATGGAACATTTAAAGAAAGCTGACAAAATATTAATTTTGAATGAAGGTAGCAGCT
ATTTTTATGGGACATTTTCAGAACTCCAAAATCTACAGCCAGACTTTAGCTCAAAACTC
ATGGGATGTGATTCTTTCGACCAATTTAGTGCAGAAAGAAGAAATTCAATCCTAACTGA
GACCTTACACCGTTTCTCATTAGAAGGAGATGCTCCTGTCTCCTGGACAGAAACCAATC
TTTTAAACAGACTGGAGAGTTTGGGGAAAAAAGGAAGAATTCTATTCTCAATCCAATCA
ACTCTATACGAAAATTTTCCATTGTGCAAAAGACTCCCTTACAAATGAATGGCATCGAA
GAGGATTCTGATGAGCCTTTAGAGAGAAGGCTGTCCTTAGTACCAGATTCTGAGCAGGG
AGAGGCGATACTGCCTCGCATCAGCGTGATCAGCACTGGCCCCACGCTTCAGGCACGAA
GGAGGCAGTCTGTCCTGAACCTGATGACACACTCAGTTAACCAAGGTCAGAACATTCAC
CGAAAGACAACAGCATCCACACGAAAAGTGTCACTGGCCCCTCAGGCAAACTTGACTGA
ACTGGATATATATTCAAGAAGGTTATCTCAAGAAACTGGCTTGGAAATAAGTGAAGAAA
TTAACGAAGAAGACTTAAAGG"))

(defparameter orangutan-cftr 
  (rnl "ATATCTTAAAGCTGTGTCTGTAAACTGATGGCTAACAAAACTAGGATTTTGGTCACTTC
TAAAATGGAACATTTAAAGAAAGCTGACAAAATTTTAATTTTACATGAAGGTAGCAGCT
ATTTTTATGGGACATTTTCAGAACTCCAAAATCTACGGCCAGACTTTAGCTCAAAACTC
ATGGGATGTGATTCTTTCGACCAATTTAGTGCAGAAAGAAGAAATTCAATCCTAACTGA
GACTTTACGCCGTTTCTCATTAGAAGGAGATGCTCCTGTCTCCTGGACAGAAACCAACC
TTTTAAACAGACTGGAGAGTTTGGGGAAAAAAGGAAGAATTCTATTCTCAATCCAATCA
ACTCTATACGAAAATTTTCCATTGTACAAAAGACTCCCTTACAAATGAATGGCATCGAA
GAGGATTCTGATGAGCCTTTCGAGAGAAGGGTGTCCTTAGTTCCAGATTCTGAGCAGGG
AGAGGCGATACTGCCTCGCATCAGCGTGATCAGCACTGGCCCCATGCTTCAGGCACGAA
GGAGGCAGTCTGTTCTGAACCTGATGACACAGTCAGTTAACCAAGGTCAGAACATTCAC
CGAAAGACAACAGCATCCACACGAAAAGTGTCACTGGCCCCTCAGGCAAACTTGACTGA
ATTGGATATATATTCAAGAAGGTTATCTCAAGAAACTGGCTTGGAAATAAGTGAAGAAA
TTAATGAAGAAGACTTAAAGG"))

(defparameter chimpanzee-cftr 
  (rnl "ATATTTGAAAGCTGTGTCTGTAAACTGATGGCTAACAAAACTAGGATTTTGGTCACTTC
TAAAATGGAACATTTAAAGAAAGCTGACAAAATATTAATTTTGCATGAAGGTAGCAGCT
ATTTTTATGGGACATTTTCAGAACTCCAAAATCTACGGCCAGACTTTAGCTCAAAACTC
ATGGGATGTGATTCTTTCGACCAATTTAGTGCAGAAAGAAGAAATTCAATCCTAACTGA
GACCTTACGCCGTTTCTCATTAGAAGGAGATGCTCCTGTCTCCTGGACAGAAACCAATC
TTTTAAACAGACTGGAGAGTTTGGGGAAAAAAGGAAGAATTCTATTCTCAATCCAATCA
ACTCTATACGAAAATTTTCCATTGTGCAAAAGACTCCCTTACAAATGAATGGCATCGAA
GAGGATTCTGATGAGCCTTTAGAGAGAAGGCTGTCCTTAGTACCAGATTCTGAGCAGGG
AGAGGCGATACTGCCTCGCATCAGCGTGATCAGCACTGGCCCCACGCTTCAGGCACGAA
GGAGGCAGTCTGTTCTGAACCTGATGACACACTCAGTTAACCAAGGTCAGAACATTCAC
CGAAAGACAACAGCATCCACACGAAAAGTGTCACTGGCCCCTCAGGCAAACTTGACTGA
ACTGGATATATATTCAAGAAGGTTATCTCAAGAAACTGGCTTGGAAATAAGTGAAGAAA
TTAACGAAGAAGACTTAAAGG"))

(defparameter gorilla-cftr 
  (rnl "ATATCTTAAAGCTGTGTCTGTAAACTGATGGCTAACAAAACTAGGATTTTGGTCACTTC
TAAAATGGAACATTTAAAGAAAGCTGACAAAATATTAATTTTGCATGAAGGTAGCAGCT
ATTTTTATGGGACATTTTCAGAACTCCAAAATCTACGGCCAGACTTTAGCTCAAAACTC
ATGGGATGTGATTCTTTCGACCAATTTAGTGCAGAAAGAAGAAATTCAATCCTAACTGA
GACCTTACGCCGTTTCTCATTAGAAGGAGATGCTCCTGTCTCCTGGACAGAAACCAATC
TTTTAAACAGACTGGAGAGTTTGGGGAAAAAAGGAAGAATTCTATTCTCAATCCAATCA
ACTCTATACGAAAATTTTCCATTGTACAAAAGACTCCCTTACAAATGAATGGCATCGAA
GAGGATTCTGATGAGCCTTTAGAGAGAAGGCTGTCCTTAGTACCAGATTCTGAGCAGGG
AGAGGCGATACTGCCTCGCATCAGCGTGATCAGCACTGGCCCCACGCTTCAGGCACGAA
GGAGGCAGTCTGTTCTGAACCTGATGACACACTCAGTTAACCAAGGTCAGAACATTCAC
CGAAAGACAACAGCATCCACACGAAAAGTGTCACTGGCCCCTCAGGCAAACTTGACTGA
ACTGGATATATATTCAAGAAGGTTATCTCAAGAAACTGGCTTGGAAATAAGTGAAGAAA
TTAACGAAGAAGACTTAAAGG"))

(defparameter human-bmp7 
  (rnl "AGAACCGCTCCAAGACGCCCAAGAACCAGGAAGCCCTGCGGATGGCCAACGTGGCAGAG
AACAGCAGCAGCGACCAGAGGCAGGCCTGTAAGAAGCACGAGCTGTATGTCAGCTTCCG
AGACCTGGGCTGGCAGGACTGGATCATCGCGCCTGAAGGCTACGCCGCCTACTACTGTG
AGGGGGAGTGTGCCTTCCC"))

(defparameter pig-bmp7 
  (rnl "AGAACCGCTCCAAGACGCCCAAGAACCAGGAAGCCCTGCGGGTGGCCAACGTCGCAGAG
AACAGCAGCAGTGACCAGCGGCAGGCCTGTAAGAAGCATGAGCTCTACGTCAGCTTCCG
GGACCTGGGCTGGCAAGACTGGATCATCGCGCCCGAAGGCTATGCCGCCTACTACTGCG
AGGGGGAGTGCGCCTTCCC"))
(defparameter rabbit-bmp7 
  (rnl "AGAACCGCTCCAAGGCACCCAAGAACCAAGAGGCGCTGCGAGTGGCCAACGTGGCAGAA
AACAGCAGCAGTGACCAGCGGCAGGCGTGCAAGAAACACGAACTGTACGTCAGCTTCCG
CGACCTGGGCTGGCAGGATTGGATCATTGCCCCGGAAGGCTACGCCGCCTACTACTGCG
AGGGAGAGTGCGCCTTCCC"))
(defparameter sheep-bmp7 
  (rnl "AGAATCGCTCCAAGGCGCCCAAGAACCAAGAAGCCCTGCGGGTGGCCAACGTCGCAGAA
AACAGCAGCAGTGACCAGAGGCAGGCATGTAAGAAGCACGAGCTATACGTCAGCTTCCG
GGACCTGGGCTGGCAGGATTGGATCATCGCACCCGAAGGCTATGCCGCCTACTACTGCG
agggggagtgcgccttccc"))

;; the kinase protein strings are from the 
;; sample input file at http://meme.sdsc.edu/meme/website/meme.html 

(defparameter kinase-prot1 
  (rnl "MSGSRRKATPASRTRVGNYEMGRTLGEGSFAKVKYAKNTVTGDQAAIKILDREKVFRHKM
VEQLKREISTMKLIKHPNVVEIIEVMASKTKIYIVLELVNGGELFDKIAQQGRLKEDEAR
RYFQQLINAVDYCHSRGVYHRDLKPENLILDANGVLKVSDFGLSAFSRQVREDGLLHTAC
GTPNYVAPEVLSDKGYDGAAADVWSCGVILFVLMAGYLPFDEPNLMTLYKRICKAEFSCP
PWFSQGAKRVIKRILEPNPITRISIAELLEDEWFKKGYKPPSFDQDDEDITIDDVDAAFS
NSKECLVTEKKEKPVSMNAFELISSSSEFSLENLFEKQAQLVKKETRFTSQRSASEIMSK
MEETAKPLGFNVRKDNYKIKMKGDKSGRKGQLSVATEVFEVAPSLHVVELRKTGGDTLEF
HKFYKNFSSGLKDVVWNTDAAAEEQKQ"))

(defparameter kinase-prot2 
  (rnl "MSGSRRKATPASRTRVGNYEMGRTLGEGSFAKVKYAKNTVTGDQAAIKILDREKVFRHKM
VEQLKREISTMKLIKHPNVVEIIEVMASKTKIYIVLELVNGGELFDKIAQQGRLKEDEAR
RYFQQLINAVDYCHSRGVYHRDLKPENLILDANGVLKVSDFGLSAFSRQVREDGLLHTAC
GTPNYVAPEVLSDKGYDGAAADVWSCGVILFVLMAGYLPFDEPNLMTLYKRVRICKAEFS
CPPWFSQGAKRVIKRILEPNPITRISIAELLEDEWFKKGYKPPSFDQDDEDITIDDVDAA
FSNSKECLVTEKKEKPVSMNAFELISSSSEFSLENLFEKQAQLVKKETRFTSQRSASEIM
SKMEETAKPLGFNVRKDNYKIKMKGDKSGRKGQLSVATEVFEVAPSLHVVELRKTGGDTL
EFHKFYKNFSSGLKDVVWNTDAAAEEQKQ"))

(defparameter kinase-match1 
  (rnl "SRQVREDGLLHTACGTPNYVAPEVLSDKGYDGAAADVWSCGVILFVLMAGYLPFDEPNLMTLYKRVRICK"))
(defparameter kinase-match2 
  (rnl "SRQVREDGLLHTACGTPNYVAPEVLSDKGYDGAAADVWSCGVILFVLMAGYLPFDEPNLMTLYKRICKAE"))

#|
EBI 

http://www.ebi.ac.uk/clustalw/
leave all defaults
type in sequences in the following format
(theres a >, then the seqname, then a newline, then the sequence begins, no quotes)

> seq-name
LETTERS
>seq-name
LETTERS
etc

|#


(defparameter ALIGN-human-pig-bmp-consensus 
  "***************************************** ********** ***************** ****** ******************* ***** ** *********** ************** ***************** ******** ************** *********** ********")

(defparameter EBI-human-pig-bmp-consensus
  "***************************************** ********** ***************** ****** ******************* ***** ** *********** ************** ***************** ******** ************** *********** ********")

(defparameter ALIGN-human-pig-rabbit-sheep-bmp-consensus
  "**** ********* * *********** ** ** *****  ********** ***** *********** ****** ******* ** ***** ** ** ** ** *********** ************** ** ******** ** ** ******** ************** ***** ***** ********")
  
(defparameter EBI-human-pig-rabbit-sheep-bmp-consensus
  "**** ********* * *********** ** ** *****  ********** ***** *********** ****** ******* ** ***** ** ** ** ** *********** ************** ** ******** ** ** ******** ************** ***** ***** ********")

(defparameter ALIGN-chimpanzee-human-orangutan-gorilla-consensus
"**** * ************************************************************************************* ********  *************************************************** ************************************************************************************ **** ************************************************ ************************************************************************************* ***************************************************** ********* ********** ************************************************************* *************************** ***************** *************************************************************************************** ************************************************************* ****************")


(defparameter EBI-chimpanzee-human-orangutan-gorilla-consensus
  (one-string
                "**** * *****************************************************"
                "******************************** ********  *****************"
                "********************************** *************************"
                "*********************************************************** " 
                "**** ************************************************ ******"
                "************************************************************"
                "******************* ****************************************"
                "************* ********* ********** *************************"
                "************************************ ***********************"
                "**** ***************** *************************************"
                "************************************************** *********"
                "**************************************************** *******"
                "*********"
                ))

(defparameter *rnaz-test-data* 
  '(
    ("AF041468.1-40566_40494"      
     "GGGGGTATAGCTCAGT-TGGTAGAGCGCTGCCTTTGCACGGCAGATGTCAGGGGTTCGAGTCCCCTTACCTCCA")
    ("X54300.1-105_177"            
     "GGGGGTATAGCTTAGT-TGGTAGAGCGCTGCTTTTGCAAGGCAGATGTCAGCGGTTCGAATCCGCTTACCTCCA")
    ("L00194.1-685_756"           
     "GGGGCCATAGCTCAGT-TGGTAGAGCGCCTGCTTTGCAAG-CAGGTGTCGTCGGTTCGAATCCGTCTGGCTCCA")
    ("AY017179.1-1528_1601"        
     "GGGCCGGTAGCTCAGCCTGGGAGAGCGTCGGCTTTGCAAGCCGAAGGCCCCGGGTTCGAATCCCGGCCGGTCCA")
    ))



;; this is for the bioutils tests
(defmacro defbiotooltest (name &body body)
  `(tests:deftest ,name ,@body :chapter :biotools))

(defun generic-instanceof (frame)
  (ecase user:*frame-system-version*
    (:old (#^instanceof frame))
    (:new (#^sys.instanceof frame))
    (:sframes (#^instanceof frame))
    ))
