;;; -*- Package: help; mode: lisp; base: 10; Syntax: Common-Lisp; -*-

(in-package :help)

;;; +=========================================================================+
;;; | Copyright (c) 2002-2006 JP Massar, Jeff Shrager, Peter Seibel           |
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

;;; Authors:   Emily Niman, Bogdan Mihai, Arnaud Taton, Jeff Elhai, JP Massar, Peter Seibel.     


(def-topic "BioBIKE Listener"
(:text
(:table :width "80%" :align "middle" 
(:tr 
(:td (:small
(:ul "The BioBIKE Listener is a web-based interactive environment that enables you answer questions immediately or to develop more complicated programs piece by piece.")
(:ul "This tutorial won't make a lot of sense unless you're looking at the Listener.")
(:ul "So go to the main page and log in to one of the public instances of BioBIKE")
(:p (:b "Table of Contents"))
(:ul "A. "((:a :href (:print (make-help-topic-url :name "The three windows of the Listener"))) "The three windows of the Listener"))
  (:ul (:ul "1. "((:a :href (:print (make-help-topic-url :name "Entering code in the command and program windows"))) "Entering code in the command and program windows")))
  (:ul (:ul "2. "((:a :href (:print (make-help-topic-url :name "Working with the history window"))) "Working with the history window")))
  (:ul "B. "((:a :href (:print (make-help-topic-url :name "Evaluation Process"))) "Evaluation Process (How BioBIKE thinks)"))
  (:ul "C. "((:a :href (:print (make-help-topic-url :name "Uploading/downloading files"))) "Uploading/downloading files"))
  (:ul "D. "((:a :href (:print (make-help-topic-url :name "When things go wrong"))) "When things go wrong"))
  (:ul (:ul "1. "((:a :href (:print (make-help-topic-url :name "Running long programs"))) "Running long programs")))
  (:ul (:ul "2. "((:a :href (:print (make-help-topic-url :name "Killing a program"))) "Killing a program")))
  (:ul (:ul "3. "((:a :href (:print (make-help-topic-url :name "Reporting problems"))) "Reporting problems")))
))))

(:table :align "middle" :width "80%" 
(:b (:tr  
(:td  :width "26.7%" :align "left" ((:a :style "text-decoration:none" :href (:print (make-help-topic-url :name "BioBIKE Examples")))(:button " Previous Page ")))
(:td :width "26.7%" :align "middle"((:a :style "text-decoration:none" :href (:print (make-help-topic-url :name "Help for BBL Users")))(:button "Help for BBL Users"))) 
(:td :align "right" :width "26.7%"((:a :style "text-decoration:none" :href (:print (make-help-topic-url :name "The three windows of the listener")))(:button " Next Page "))) 
)))

))




;;============ The three windows of the listner =============
(def-topic "The three windows of the listener"
(:text
(:table :width "80%" :align "middle" 

(:tr 
(:td (:small
(:p (:b "A. " (:u "The three windows of the Listener")))
(:ol :type "1"
(:li ((:a :style "color:#0000EE" :href (:print (make-help-topic-url :name "Entering code in the command and program windows"))) "Entering code in the command and program windows"))
(:li ((:a :style "color:#0000EE" :href (:print (make-help-topic-url :name "Working with the history window"))) "Working with the history window")) )

(:p "The space you see when logged onto BioBIKE is partioned into three windows:")
(:ul (:li (:b "The Program Window") " (or Expression Entry Box) - The large box at the bottom of the screen"))
(:ul (:li (:b "The Command Window") " (or Command Entry Box) - The long thin box in the middle of the screen"))
(:ul (:li (:b "The History window") " (or Output Area) - The large space above the command window"))
(:p "The program and command windows are used to enter BioBIKE statements, while the history window shows you what you have done and enables you to retrieve earlier statements.")
))))

(:table :align "middle" :width "80%" 
(:b (:tr  
(:td :width "26.7%" :align "left" ((:a :style "text-decoration:none" :href (:print (make-help-topic-url :name "BioBIKE Listener")))(:button " Previous Page ")))
(:td :width "26.7%" :align "middle" ((:a :style "text-decoration:none" :href (:print (make-help-topic-url :name "BioBIKE Listener")))(:button "Up to Table of Contents"))) 
(:td :width "26.7%" :align "right" ((:a :style "text-decoration:none" :href (:print (make-help-topic-url :name "Entering code in the command and program windows")))(:button " Next Page "))) ) 

(:tr (:td :width "26.7%")
(:td :align "middle"  :width "26.7%"((:a :style "text-decoration:none" :href (:print (make-help-topic-url :name "HELP for BBL users"))) (:button "Help for BBL Users")))
(:td :width "26.7%") ) )) 
)) 




(def-topic "Entering code in the command and program windows"
(:text
(:table :width "80%" :align "middle" 
(:tr 
(:td (:small
(:p (:b "A. " (:u "The three windows of the Listener")))
(:ol :type "1"
(:li ((:a :style "color:#0000EE" :href (:print (make-help-topic-url :name "Entering code in the command and program windows"))) (:font :color "red" "Entering code in the command and program windows")))
(:li ((:a :style "color:#0000EE" :href (:print (make-help-topic-url :name "Working with the history window"))) "Working with the history window")) )

(:p (:b "A1. " (:u "Entering code in the command and program windows ")))
(:p "These two windows are almost functionally equivalent. The small differences serve to make it easier to enter multiline statements in the program window. Try entering this into the Command Window (including parentheses!):"))

	(:p (:pre "          (DISPLAY-SEQUENCE-OF ssr1600)"))

(:small (:p "Then press the Enter key on your keyboard or click on the Enter button on the screen. You should see in the History Window the sequence of the gene ssr1600. (" (:i "By the way\, don't be concerned about upper case/lower case distinctions; they're there only for clarity to humans -- BioBIKE doesn't care") ").")

(:p "That was nice\, but you decide that what you really want is the " (:b (:i "protein")) " sequence. So type (or copy/paste) the following statement\, this time for variety into the " (:b (:i "Program")) " Window:")) 

	(:p (:pre "          (DISPLAY-SEQUENCE-OF p-ssr1600)"))

(:small (:p "Press the Enter key on your keyboard... Nothing happened\, except the cursor moved. OK\, try clicking the Enter button on the screen. Now you get another copy of the " (:b (:i "gene")) " sequence... and the line you put into the Program Window has disappeared!")

(:p "Reenter the line into the Program Window and this time click on the " (:b "Eval") " button (Eval for \"Evaluate\"). This should get you the sequence of the " (:i "protein") " p-Ssr1600.")

(:p "To summarize:") 
  (:ul (:li "Both windows allow you to enter commands"))
  (:ul (:li "The Command Window is activated by pressing Enter or clicking the Enter button"))
  (:ul (:li "The Program Window is activated by pressing the Eval button"))
  (:ul (:li "In the Program Window\, pressing the Enter key does " (:i (:b "not")) " activate the window but instead moves you to the next line. This facilitates the entry of multiline programs"))
  (:ul (:li "BioBIKE knows nothing about text you enter until the specific window is activated\,
thus text you just entered is lost if you activate the " (:i "wrong") " window"))

(:p "Mark my words\, the day will come when you will enter into the Program Window the product of much thought and great effort\, and you will blithely press the Enter key\, and you will lose forever what you have done\, and you will be very unhappy. . .")

(:p ". . . " (:i (:b "unless")) " you refrain from using " (:i (:b "either")) " Enter or Eval! Instead (regardless of whether you are in the Command Window or the Program Window) activate your code by pressing Tab and Enter on your keyboard. Tab has the effect of moving the cursor to the next button (the Enter button in the case of the Command Window\, the Eval button in the case of the Program Window)\, and pressing Enter on your keyboad has the effect of activating that button. Don't try to understand it. Just do it. You will thank me\, and you will name your first-born child in my memory.")
(:p "Copy and paste these multiple lines into the Program Window:"))

(:p (:pre "          ("((:a :href (:print (make-help-function-documentation-url :name "LENGTH-OF" :package "bbi" ))) "LENGTH-OF") " ssr1600)
          ("((:a :href (:print (make-help-function-documentation-url :name "MW-OF" :package "bbi" ))) "MW-OF") " ssr1600)
          (ALL-ORTHOLOGS-OF ssr1600)")) 

(:small (:p "Clicking on Eval now gives you a number that is plausibly the length of this small gene, but what about the other statements?")
(:ul (:b (:i "You can execute only a single statement at one time from either the Command or the Program Window"))) 

(:p "Let's be clear: a statement is everything between \"(\" and its matching \")\", so long as it corresponds to proper BioBIKE language syntax. However, there will certainly be cases where you will want to execute many statements at once (see the many "((:a :href (:print (make-help-topic-url :name "Function Examples"))) "examples") " provided). How do we overcome the one-statement limitation?") 
 
  (:ul (:li "Copy/paste sets of statements and after executing the first one, delete it and reevaluate"))
  (:ul (:li "Encase the statements within a larger single unit.  For example, try evaluating:")) )

(:p (:pre "          { ("((:a :href (:print (make-help-function-documentation-url :name "LENGTH-OF" :package "bbi" ))) "LENGTH-OF") " ssr1600)
            ("((:a :href (:print (make-help-function-documentation-url :name "MW-OF" :package "bbi" ))) "MW-OF") " ssr1600)
            (ALL-ORTHOLOGS-OF ssr1600) }")) 
  (:small (:ul "Use " (:b "{") " and " (:b "}") " at the beginning and end of the complete program to display the results as a list.  Use " (:b "(") " and " (:b ")") " when combining statements to generate a single result."))

  (:small (:ul (:li "Combine the statements into a function.  Example:")))

(:p (:pre "           ("((:a :href (:print (make-help-function-documentation-url :name "DEFINE" :package "bbi" ))) "DEFINE") " gene-length-sum ("((:a :href (:print (make-help-function-documentation-url :name "SUM-OF" :package "bbi" ))) "SUM-OF") " ("((:a :href (:print (make-help-function-documentation-url :name "LENGTHS-OF" :package "bbi" ))) "LENGTHS-OF") " ("((:a :href (:print (make-help-function-documentation-url :name "GENES-OF" :package "bbi" ))) "GENES-OF") " S6803)))"))
(:p)
(:p)
)))

(:table :align "middle" :width "80%" 
(:b (:tr  
(:td :width "26.7%" :align "left" ((:a :style "text-decoration:none" :href (:print (make-help-topic-url :name "The three windows of the Listener")))(:button " Previous Page ")))
(:td :width "26.7%" :align "middle" ((:a :style "text-decoration:none" :href (:print (make-help-topic-url :name "BioBIKE Listener")))(:button "Up to Table of Contents"))) 
(:td :width "26.7%" :align "right" ((:a :style "text-decoration:none" :href (:print (make-help-topic-url :name "Working with the history window")))(:button " Next Page "))) ) 

(:tr (:td :width "26.7%")
(:td :align "middle"  :width "26.7%"((:a :style "text-decoration:none" :href (:print (make-help-topic-url :name "HELP for BBL users"))) (:button "Help for BBL Users")))
(:td :width "26.7%") ) )) 
)) 




(def-topic "Working with the history window"
(:text
(:table :width "80%" :align "middle" 
(:tr 
(:td (:small
(:p (:b "A. " (:u "The three windows of the Listener")))
(:ol :type "1"
(:li ((:a :style "color:#0000EE" :href (:print (make-help-topic-url :name "Entering code in the command and program windows"))) "Entering code in the command and program windows"))
(:li ((:a :style "color:#0000EE" :href (:print (make-help-topic-url :name "Working with the history window"))) (:font :color "red" "Working with the history window"))) )

(:p (:b "A2. " (:u "Working with the history window")))
(:p "Every statement you enter is copied by BioBIKE into the History Window, appearing as something like:") )

	(:pre "          <4>> ("((:a :href (:print (make-help-function-documentation-url :name "LENGTH-OF" :package "bbi" ))) "LENGTH-OF") " ssr1600)
          :: 351")

(:small (:p "Each statement is numbered sequentially from the top of the window and is followed by :: and the result of the statement. You can exploit these features in several useful ways:")

(:ul (:li (:u "Copy a command")))
(:ul "Clicking on the number associated with a statement causes that statement to reappear in either the Command or Program Window (BioBIKE chooses according to the size of the statement). This is quite useful when you want to repeat execution of a command or to edit it.")

(:ul (:li (:u "Copy a result")))
(:ul "Clicking on the double-colon associated with a result causes that result to reappear in either the Command or Program Window.")

(:ul (:li (:u "Refer to a result")))
(:ul "You can use a previous result within a new statement with the RESULT function. For example:") )

(:pre "          (DISPLAY-LINE \"The length of the gene is \" (RESULT 4))")

(:small (:ul "Especially if you generate long lists (like a list of all genes of " (:i "Synechocystis") "), your history window will grow to a size that can become a burden. The larger the window, the longer it will take for your screen to load when you contact BioBIKE. At some point, you'll probably like to junk all or some of the window. You can do this with the CLEAR-HISTORY command.")

(:ul "Your History Window is automatically saved and retained for a period of time (currently 90 days). You can click on Session Logs at the bottom of the screen and bring up the History Window of any session within that period. You can download your History Window to your own computer as a permanent record of what you've done simply by saving it through your web browser.") )
)))

(:table :align "middle" :width "80%" 
(:b (:tr  
(:td :width "26.7%" :align "left" ((:a :style "text-decoration:none" :href (:print (make-help-topic-url :name "Entering code in the command and program windows")))(:button " Previous Page ")))
(:td :width "26.7%" :align "middle" ((:a :style "text-decoration:none" :href (:print (make-help-topic-url :name "BioBIKE Listener")))(:button "Up to Table of Contents"))) 
(:td :width "26.7%" :align "right" ((:a :style "text-decoration:none" :href (:print (make-help-topic-url :name "Evaluation Process")))(:button " Next Page "))) ) 

(:tr (:td :width "26.7%")
(:td :align "middle"  :width "26.7%"((:a :style "text-decoration:none" :href (:print (make-help-topic-url :name "HELP for BBL users"))) (:button "Help for BBL Users")))
(:td :width "26.7%") ) )) 
)) 



(def-topic "Evaluation Process"
(:text
(:table :width "80%" :align "middle" 
(:tr 
(:td (:small
(:p (:b "B. " (:u "Evaluation Process (How BioBIKE thinks")))
(:p "When the BBL web-listener is handed over code, it goes through the following thought
processes:")
(:ul "1. " (:u "Overall syntax check:"))
  (:ul (:ul "a. Do all parentheses occur as matching pairs?")) )
	(:pre (:ul (:ul (:ul (:i "Example (valid)") ": ("((:a :href (:print (make-help-function-documentation-url :name "SEQUENCE-OF" :package "bbi"))) "SEQUENCE-OF")" all4312 FROM x TO (+ x 100))
" (:i "Example (invalid)") ": ("((:a :href (:print (make-help-function-documentation-url :name "SEQUENCE-OF" :package "bbi"))) "SEQUENCE-OF")" all4312"))))

  (:ul (:ul (:small "b. Does each open [ have a corresponding close ]?")))
	(:pre (:ul (:ul (:ul (:i "Example (valid)") ": *all-organisms*[1 -> 3]
"(:i "Example (invalid)") ": *all-organisms*[1"))))

  (:ul (:ul (:small "c. Does each open { have a corresponding close }")))
	(:pre (:ul (:ul (:ul (:i "Example (valid)") ": {{A7120 all4312} {Npun npf0001}}
" (:i"Example (invalid)") ": {{A7120 all4312} {Npun npf0001}"))))

(:small (:p "*If the line ends in at least one close parenthesis, the BBL-reader will add or subtract as many parentheses necessary to make parentheses match. This is not done with square and curly brackets."))

(:small (:ul "2. " (:u "Extract the first form and ignore the rest"))
  (:ul (:ul "a. If the first non-space symbol is an open parenthesis\, read until the corresponding close parenthesis and ignore the rest.")) )
	(:pre (:ul (:ul (:ul (:i "Example") ": ("((:a :href (:print (make-help-function-documentation-url :name "DEFINE" :package "bbi"))) "DEFINE")" x AS 7) (DISPLAY (+ x 1))
         |<-extracted->| |<---ignored--->|"))))

  (:ul (:ul (:small "b. Otherwise read until the end of the symbol")))
	(:pre (:ul (:ul (:ul (:i "Example") ": 1 + 1
           |<-ignored..."))))

(:small (:ul "3. " (:u "Evaluate all symbols"))
  (:ul (:ul "a. Numbers, literal strings and symbols are evaluated to themselves")))
	(:pre (:ul (:ul (:ul (:i "Example") ": \"x\" evaluates to \"x\"
"(:i "Example") ": 47 evaluates to 47"))))
  (:small (:ul (:ul "b. Other symbols are evaluated as variables or as functions, depending on their position")))

	(:pre (:ul (:ul (:ul (:i "Example") ": (+ 1 x): x is interpreted as the " (:b (:i "variable")) " called x
"(:i "Example") ": (x + 1): x is interpreted as the " (:b (:i "function")) " called x"))))

  (:small (:ul (:ul "c. If a symbol is deemed to be a variable or a function and that variable or function has not been defined, an error is raised")))	
	(:pre (:ul (:ul (:ul (:i "Example") ": (+ 1 x): ERROR, unless x previously defined as a " (:b (:i "variable")) "
" (:i "Example") ": (x + 1): ERROR, unless x previously defined as a " (:b (:i "function")) ))))

(:small (:ul "4. " (:u "Evaluate functions"))
  (:ul (:ul "a. Start with logically most inner functions and work outwards")) )
	(:pre (:ul (:ul (:ul (:i "Example") ": (+ 1 ("((:a :href (:print (make-help-function-documentation-url :name "COUNT-OF" :package "bbi"))) "COUNT-OF")" \"A\" IN ("((:a :href (:print (make-help-function-documentation-url :name "CHROMOSOME-OF" :package "bbi"))) "CHROMOSOME-OF")" A7120)))
                               |<- evaluate 1st -->|"))))

  (:small (:ul (:ul "b. Check syntax of function, raising error if syntax faulty")))
	(:pre (:ul (:ul (:ul (:i "Example") ": ("((:a :href (:print (make-help-function-documentation-url :name "CHROMOSOME-OF" :package "bbi"))) "CHROMOSOME-OF")" 47): ERROR: argument can't be a number"))))

  (:small (:ul (:ul "c. Replace function with object returned by function")))
	(:pre (:ul (:ul (:ul "Example: (+ 1 ("((:a :href (:print (make-help-function-documentation-url :name "COUNT-OF" :package "bbi"))) "COUNT-OF")" \"A\" IN ("((:a :href (:print (make-help-function-documentation-url :name "CHROMOSOME-OF" :package "bbi"))) "CHROMOSOME-OF")" A7120)))
" (:i "The step-by-step process within the computer:") "
--> (+ 1 ("((:a :href (:print (make-help-function-documentation-url :name "COUNT-OF" :package "bbi"))) "COUNT-OF")" \"A\" IN A7120.chromosome ))
--> (+ 1 1878490 )
--> 1878491"))))

(:small (:p "It is important to see from this sequence of events that:")
(:ul (:b (:i "BioBIKE examines only one form at a time")))

(:p "That one form may be a single number or dozens of lines of code within matching parentheses, but learn this lesson well. A common error amongst new users is to expect things like this to work:"))
(:pre (:ul "("((:a :href (:print (make-help-function-documentation-url :name "DEFINE" :package "bbi"))) "DEFINE")" x AS 47)
(- x 18)"))

(:p (:small "Users wonder why they never see 29 nor any error message or warning. But you now know why.  For further explanation of how to create a multi-line program go to "((:a :href (:print (make-help-topic-url :name "Entering code in the command and program windows"))) "Entering code in the command and program windows") "." ))
(:p)
)))

(:table :align "middle" :width "80%" 
(:b (:tr  
(:td :width "26.7%" :align "left" ((:a :style "text-decoration:none" :href (:print (make-help-topic-url :name "Working with the history window")))(:button " Previous Page ")))
(:td :width "26.7%" :align "middle" ((:a :style "text-decoration:none" :href (:print (make-help-topic-url :name "BioBIKE Listener")))(:button "Up to Table of Contents"))) 
(:td :width "26.7%" :align "right" ((:a :style "text-decoration:none" :href (:print (make-help-topic-url :name "Uploading/downloading files")))(:button " Next Page "))) ) 

(:tr (:td :width "26.7%")
(:td :align "middle"  :width "26.7%"((:a :style "text-decoration:none" :href (:print (make-help-topic-url :name "HELP for BBL users"))) (:button "Help for BBL Users")))
(:td :width "26.7%") ) )) 
)) 


;;============ Uploading/downloading files =============
(def-topic "Uploading/downloading files"
(:text
(:table :width "80%" :align "middle" 
(:tr 
(:td (:small
(:p (:b "C. " (:u "Uploading/downloading files")))
(:p (:u "Directories"))
(:p "When you logged into BioBIKE for the first time, you were given a directory of the same name as your login ID in which to store files. You can see what's in this space clicking the " (:b "Browse Files") " button at the bottom of the listener. Your directory is born with two files:") 

  (:ul (:u "arglist-data.js") " - Internal use only -- DON'T TOUCH IT!")
  (:ul (:u "biolisp.ini") " - Put here any statement you want to execute every time you log in. ")

(:p "You also have access to a directory called "(:b "shared-files") ". You have access to your own directory. Everyone has access to the "(:b "shared-files") " directory. BioBIKE provides many functions to get information in and out of these two directories.")

(:p (:u "Uploading files from your computer"))
(:p "BioBIKE statements can access only files in its directories, so if you want to use your own sequence or a large data set, you'll need first to upload the file to your directory. To do this, click on " (:b "Upload file") " and navigate to the location of your file on your own computer. If you are uploading a text file from a PC, then click the box labeled \" "(:i "Convert Return\Newline -> Newline?") "\", which tells BioBIKE to translate from the method used by PCs to store text files to the Unix method.")

(:p (:u "Downloading files to your computer"))
(:p "You may well generate results that you wish to transfer to your own computer. BioBIKE provides functions to help generate files containing your results. All these files are kept in your subdirectory.")

(:p "To download a file, click the "(:b "Browse Files") " button at the bottom of the listener. Look through the available files and find the file you want to download and click its name. You can then use your browser buttons to download the file to your own computer.")
))))

(:table :align "middle" :width "80%" 
(:b (:tr  
(:td :width "26.7%" :align "left" ((:a :style "text-decoration:none" :href (:print (make-help-topic-url :name "Evaluation Process")))(:button " Previous Page ")))
(:td :width "26.7%" :align "middle" ((:a :style "text-decoration:none" :href (:print (make-help-topic-url :name "BioBIKE Listener")))(:button "Up to Table of Contents"))) 
(:td :width "26.7%" :align "right" ((:a :style "text-decoration:none" :href (:print (make-help-topic-url :name "When things go wrong")))(:button " Next Page "))) ) 

(:tr (:td :width "26.7%")
(:td :align "middle"  :width "26.7%"((:a :style "text-decoration:none" :href (:print (make-help-topic-url :name "HELP for BBL users"))) (:button "Help for BBL Users")))
(:td :width "26.7%") ) )) 
)) 




;;============ When things go wrong =============
(def-topic "When things go wrong"
(:text
(:table :width "80%" :align "middle" 
(:tr 
(:td (:small
(:p (:b "D. " (:u "When things go wrong")))
(:ol :type "1"
(:li ((:a :style "color:#0000EE" :href (:print (make-help-topic-url :name "Running long programs"))) "Running long programs"))
(:li ((:a :style "color:#0000EE" :href (:print (make-help-topic-url :name "Killing a program"))) "Killing a program"))
(:li ((:a :style "color:#0000EE" :href (:print (make-help-topic-url :name "Reporting problems"))) "Reporting problems")) )

(:p "Most often, when things go wrong its because of some silly error (e.g. a missing parentheses or mispelled word). Sometimes it is because of a silly error in making this language (if so, please report the problem to BioBIKE Central!). However, there are times that things go wrong because they've gone too right.")
))))

(:table :align "middle" :width "80%" 
(:b (:tr  
(:td :width "26.7%" :align "left" ((:a :style "text-decoration:none" :href (:print (make-help-topic-url :name "Uploading/downloading files")))(:button " Previous Page ")))
(:td :width "26.7%" :align "middle" ((:a :style "text-decoration:none" :href (:print (make-help-topic-url :name "BioBIKE Listener")))(:button "Up to Table of Contents"))) 
(:td :width "26.7%" :align "right" ((:a :style "text-decoration:none" :href (:print (make-help-topic-url :name "Running long programs")))(:button " Next Page "))) ) 

(:tr (:td :width "26.7%")
(:td :align "middle"  :width "26.7%"((:a :style "text-decoration:none" :href (:print (make-help-topic-url :name "HELP for BBL users"))) (:button "Help for BBL Users")))
(:td :width "26.7%") ) )) 
)) 




(def-topic "Running long programs"
(:text
(:table :width "80%" :align "middle" 
(:tr 
(:td (:small
(:p (:b "D. " (:u "When things go wrong")))
(:ol :type "1"
(:li ((:a :style "color:#0000EE" :href (:print (make-help-topic-url :name "Running long programs"))) (:font :color "red" "Running long programs")))
(:li ((:a :style "color:#0000EE" :href (:print (make-help-topic-url :name "Killing a program"))) "Killing a program"))
(:li ((:a :style "color:#0000EE" :href (:print (make-help-topic-url :name "Reporting problems"))) "Reporting problems")) )

(:p (:b "D1. " (:u "Running long programs")))
(:p "You might enter a program into the Program Window, click EVAL, and... nothing happens! Maybe the computer is thinking. If so, it has a limited time (default currently 40 seconds) to sort things out, and then you will get the following message:") )

	(:pre "          ;; <<< *** TIMEOUT TIMEOUT TIMEOUT *** >>>
          ;; <<< *** COMPUTATION ABORTED ***>>>
          ;; <<< *** ADJUST *EXECUTION-TIMELIMIT* TO RUN LONGER *** >>>
          ;; <<< *** CURRENT VALUE IS 40 SECONDS ***>>>")

(:small (:p "Possible responses:")
  (:ul (:li (:u "Try again later") " or go to another BioBIKE site (someone else may be running a computationally intensive program temporarily clogging things up)."))

  (:ul (:li (:u "Increase the time limit.") " First, however, it is a good idea to run a shorter version of the code to get an estimate of how long the full code will take. The TIME function is useful in this regard. Suppose you run across this problem::")) )

	(:pre "          (FOR-EACH electron IN universe
          ...        ; something really important
          FINALLY (DISPLAY answer))
          :: 
          ;; <<< *** TIMEOUT TIMEOUT TIMEOUT *** >>>")

  (:small (:ul "Here's how you can find out how long the program will take:"))

	(:pre "          (TIME
          (FOR-EACH electron IN universe
          FOR count FROM 1 TO 100
          ...       ; something really important
          FINALLY (DISPLAY answer))
          )
          :: 
          42
          ; cpu time (non-gc) 380 msec user, 0 msec system
          ; cpu time (gc)     200 msec user, 0 msec system
          ; cpu time (total)  580 msec user, 0 msec system
          ; real time  583 msec")

  (:small (:ul " Going through just the first 100 iterations took 380 msec of computer thought, 200 msec of garbage collection (retrieving memory no longer in use), for a total of 580 msec. Now you can calculate the amount of time it would take to run the full program (= 580 msec * number of electrons in universe / 100).")

  (:ul "If the calculation convinces you that you really need a reasonable amount of execution time, then click on the down arrow of the " (:b "Tools") " menu at the bottom of the screen, click on " (:b "Prefs") ", and set the execution time limit to the desired number of seconds.")
       
  (:ul (:li (:u "Run a long program in the middle of the night") " so as to not drain resources from your colleagues (see DELAY-EXECUTION)"))

  (:ul (:li (:u "Step back and reconsider") "... 40 seconds is a LONG TIME for a computer. There's a good chance you are doing something wrong or at least inefficient. Try contacting us to see if we might be able to speed up the program.")) )

))) 

(:table :align "middle" :width "80%" 
(:b (:tr  
(:td :width "26.7%" :align "left" ((:a :style "text-decoration:none" :href (:print (make-help-topic-url :name "When things go wrong")))(:button " Previous Page ")))
(:td :width "26.7%" :align "middle" ((:a :style "text-decoration:none" :href (:print (make-help-topic-url :name "BioBIKE Listener")))(:button "Up to Table of Contents"))) 
(:td :width "26.7%" :align "right" ((:a :style "text-decoration:none" :href (:print (make-help-topic-url :name "Killing a program")))(:button " Next Page "))) ) 

(:tr (:td :width "26.7%")
(:td :align "middle"  :width "26.7%"((:a :style "text-decoration:none" :href (:print (make-help-topic-url :name "HELP for BBL users"))) (:button "Help for BBL Users")))
(:td :width "26.7%") ) )) 
)) 




(def-topic "Killing a program"
(:text
(:table :width "80%" :align "middle" 
(:tr 
(:td (:small
(:p (:b "D. " (:u "When things go wrong")))
(:ol :type "1"
(:li ((:a :style "color:#0000EE" :href (:print (make-help-topic-url :name "Running long programs"))) "Running long programs"))
(:li ((:a :style "color:#0000EE" :href (:print (make-help-topic-url :name "Killing a program"))) (:font :color "red" "Killing a program")))
(:li ((:a :style "color:#0000EE" :href (:print (make-help-topic-url :name "Reporting problems"))) "Reporting problems")) )

(:p (:b "D2. " (:u "Killing a program")))
(:p "If your program is taking too long and you don't want to wait for it to timeout, you can kill it yourself through the following steps:")

  (:ul "1. Close your BioBIKE screen (this will not cause the program to stop running)")
  (:ul "2. Login again to BioBIKE")
  (:ul "3. Type the following into the command window and press ENTER:") )

	(:pre "          (ALL-MY-PROCESSES)")
 
(:small (:ul "You will see something like:"))

	(:pre "          ::
          ;;
          ;; Processes owned by JOEUSER
          ;; PID: 1127 05/24/04 23:40:47 (LOOP FOR x FROM here TO infinity)
          ;; PID: 1135 05/24/04 23:44:51 (all-my-processes)")

(:small (:ul "The number is called the PID (Process Identifier), through which you can refer to specific programs that are running.")

  (:ul "4. Type the following into the command window and press ENTER:") )

	(:pre "          (KILL-MY-PROCESS 1127)        
          " (:i "(or whatever the PID number is of the offending process)"))

(:small (:ul "You will be deposited to a screen that was present just before running the program you just killed."))
)))

(:table :align "middle" :width "80%" 
(:b (:tr  
(:td :width "26.7%" :align "left" ((:a :style "text-decoration:none" :href (:print (make-help-topic-url :name "Running long programs")))(:button " Previous Page ")))
(:td :width "26.7%" :align "middle" ((:a :style "text-decoration:none" :href (:print (make-help-topic-url :name "BioBIKE Listener")))(:button "Up to Table of Contents"))) 
(:td :width "26.7%" :align "right" ((:a :style "text-decoration:none" :href (:print (make-help-topic-url :name "Reporting problems")))(:button " Next Page "))) ) 

(:tr (:td :width "26.7%")
(:td :align "middle"  :width "26.7%"((:a :style "text-decoration:none" :href (:print (make-help-topic-url :name "HELP for BBL users"))) (:button "Help for BBL Users")))
(:td :width "26.7%") ) )) 
)) 




(def-topic "Reporting problems"
(:text
(:table :width "80%" :align "middle" 
(:tr 
(:td (:small
(:p (:b "D. " (:u "When things go wrong")))
(:ol :type "1"
(:li ((:a :style "color:#0000EE" :href (:print (make-help-topic-url :name "Running long programs"))) "Running long programs"))
(:li ((:a :style "color:#0000EE" :href (:print (make-help-topic-url :name "Killing a program"))) "Killing a program"))
(:li ((:a :style "color:#0000EE" :href (:print (make-help-topic-url :name "Reporting problems"))) (:font :color "red" "Reporting problems"))) )

(:p (:b "D3. " (:u "Reporting problems")))
(:p "You are amongst the first to use BioBIKE. Things will go wrong, but each time a bug is reported, things will go wrong less frequently. Please, when something doesn't seem right, copy the relevant portion of your History Window and send us a report. One easy way to do this is to click the down arrow on the " (:b "Help") " box at the bottom of the screen, then click " (:b "Send Feedback") ".")

(:p "We need LOTS of feedback. So don't worry if it's just you (it isn't) or if you're bothering us (you aren't).") )
)))

(:table :align "middle" :width "80%" 
(:b (:tr  
(:td :width "26.7%" :align "left" ((:a :style "text-decoration:none" :href (:print (make-help-topic-url :name "Killing a program")))(:button " Previous Page ")))
(:td :width "26.7%" :align "middle" ((:a :style "text-decoration:none" :href (:print (make-help-topic-url :name "BioBIKE Listener")))(:button "Up to Table of Contents"))) 
(:td :width "26.7%" :align "right" ((:a :style "text-decoration:none" :href (:print (make-help-topic-url :name "Basic Syntax")))(:button "Next Page"))) ) 

(:tr (:td :width "26.7%")
(:td :align "middle"  :width "26.7%"((:a :style "text-decoration:none" :href (:print (make-help-topic-url :name "HELP for BBL users"))) (:button "Help for BBL Users")))
(:td :width "26.7%") ) )) 
)) 
