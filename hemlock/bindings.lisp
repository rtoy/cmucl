;;; -*- Log: hemlock.log; Package: Hemlock -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the Spice Lisp project at
;;; Carnegie-Mellon University, and has been placed in the public domain.
;;; Spice Lisp is currently incomplete and under active development.
;;; If you want to use this code or any part of Spice Lisp, please contact
;;; Scott Fahlman (FAHLMAN@CMUC). 
;;; **********************************************************************
;;;
;;; Some bindings:
;;;
(in-package 'hemlock)



;;;; Default key translations:

;;; This page first maps all uppercase characters with all modifier bit
;;; combinations to the equivalent lowercase letter.  This saves many
;;; duplicated BIND-KEY forms and tends to support what users expect out of a
;;; BIND-KEY call.
;;;
;;; Secondly, this page defines prefix characters that set specified modifier
;;; bits on the next character typed.
;;;

;;; Case insensitivize:
;;;
(defun case-insensitivize (bits)
  "Make key translations from all the lowercase characters with specified bits
  to the corresponding uppercase characters."
  (do-alpha-chars (lower :lower)
    (setf (key-translation (make-char (char-upcase lower) bits))
	  (make-char lower bits))))

(case-insensitivize 0)
(case-insensitivize 1)
(case-insensitivize 2)
(case-insensitivize 3)

(case-insensitivize 8)
(case-insensitivize 9)
(case-insensitivize 10)
(case-insensitivize 11)


;;; Prefix characters:
;;;
(setf (key-translation #\escape) '(:bits :meta))
(setf (key-translation #\control-z) '(:bits :control :meta))
(setf (key-translation #\control-\z) '(:bits :control :meta))
(setf (key-translation #\control-^) '(:bits :control))
(setf (key-translation #\control-c) '(:bits :hyper))
(setf (key-translation #\control-\c) '(:bits :hyper))



;;;; Void some key translations.

;;; This page kills some key translations so we can make use of case sensitive
;;; bindings.  These translations must be voided for each modifier bit
;;; combination we are interested in.  Characters with nil translations, used
;;; in bindings for which the translation is desired, must be bound to both the
;;; lower and upper case characters separately.  For examples, see "Delete Next
;;; Character" and "Previous Undeleted Message".
;;;

(setf (key-translation #\control-meta-s) nil)
(setf (key-translation #\control-meta-a) nil)
(setf (key-translation #\control-meta-v) nil)
(setf (key-translation #\control-meta-f) nil)
(setf (key-translation #\control-meta-l) nil)
(setf (key-translation #\control-meta-c) nil)

(setf (key-translation #\control-d) nil)
(setf (key-translation #\control-m) nil)

(setf (key-translation #\meta-p) nil)

(setf (key-translation #\D) nil)
(setf (key-translation #\U) nil)
(setf (key-translation #\R) nil)
(setf (key-translation #\C) nil)



;;;; Most every binding.

;;; Self insert letters:
;;;
(do-alpha-chars (upper :upper)
  (bind-key "Self Insert" upper))

;;; Add these lowercase letters since we killed the uppercase translations
;;; on the previous page.
;;;
(bind-key "Self Insert" #\d)
(bind-key "Self Insert" #\u)
(bind-key "Self Insert" #\r)
(bind-key "Self Insert" #\c)


(bind-key "Beginning of Line" #\control-a)
(bind-key "Delete Next Character" #\control-\d)
(bind-key "Delete Next Character" #\control-d)
(bind-key "End of Line" #\control-e)
(bind-key "Forward Character" #\control-f)
(bind-key "Forward Character" #\rightarrow)
(bind-key "Backward Character" #\control-b)
(bind-key "Backward Character" #\leftarrow)
(bind-key "Kill Line" #\control-k)
(bind-key "Refresh Screen" #\control-l)
(bind-key "Next Line" #\control-n)
(bind-key "Next Line" #\downarrow)
(bind-key "Previous Line" #\control-p)
(bind-key "Previous Line" #\uparrow)
(bind-key "Query Replace" #\meta-%)
(bind-key "Reverse Incremental Search" #\control-r)
(bind-key "Incremental Search" #\control-s)
(bind-key "Forward Search" #\meta-s)
(bind-key "Reverse Search" #\meta-r)
(bind-key "Transpose Characters" #\control-t)
(bind-key "Universal Argument" #\control-u)
(bind-key "Scroll Window Down" #\control-v)
(bind-key "Scroll Window Up" #\meta-v)
(bind-key "Scroll Next Window Down" #\control-meta-\v)

(bind-key "Scroll Next Window Up" #\control-meta-V)

(bind-key "Help" #\home)
(bind-key "Help" #\control-_)
(bind-key "Describe Key" #\meta-?)


(bind-key "Here to Top of Window" #\leftdown)
(bind-key "Do Nothing" #\leftup)
(bind-key "Top Line to Here" #\rightdown)
(bind-key "Do Nothing" #\rightup)
(bind-key "Point to Here" #\middledown)
(bind-key "Point to Here" #\super-leftdown)
(bind-key "Generic Pointer Up" #\middleup)
(bind-key "Generic Pointer Up" #\super-leftup)
(bind-key "Do Nothing" #\super-rightup)
(bind-key "Insert Kill Buffer" #\super-rightdown)


(bind-key "Insert File" '#(#\control-x #\control-r))
(bind-key "Save File" '#(#\control-x #\control-s))
(bind-key "Visit File" '#(#\control-x #\control-v))
(bind-key "Write File" '#(#\control-x #\control-w))
(bind-key "Find File" '#(#\control-x #\control-f))
(bind-key "Backup File" '#(#\control-x #\meta-b))
(bind-key "Save All Files" '#(#\control-x #\control-\m))
(bind-key "Save All Files" '#(#\control-x #\control-m))
(bind-key "Save All Files" '#(#\control-x #\return))
(bind-key "Save All Files and Exit" '#(#\control-x #\meta-z))

(bind-key "List Buffers" '#(#\control-x #\control-b))
(bind-key "Buffer Not Modified" #\meta-~)
(bind-key "Check Buffer Modified" '#(#\control-x #\~))
(bind-key "Select Buffer" '#(#\control-x #\b))
(bind-key "Select Previous Buffer" #\control-meta-\l)
(bind-key "Circulate Buffers" #\control-meta-l)
(bind-key "Create Buffer" '#(#\control-x #\meta-b))
(bind-key "Kill Buffer" '#(#\control-x #\k))
(bind-key "Select Random Typeout Buffer" #\hyper-t)

(bind-key "Next Window" '#(#\control-x #\n))
(bind-key "Next Window" '#(#\control-x #\o))
(bind-key "Previous Window" '#(#\control-x #\p))
(bind-key "Split Window" '#(#\control-x #\2))
(bind-key "New Window" '#(#\control-x #\control-n))
(bind-key "Delete Window" '#(#\control-x #\d))
(bind-key "Delete Window" '#(#\control-x #\D))
(bind-key "Delete Next Window" '#(#\control-x #\1))
(bind-key "Line to Top of Window" #\meta-!)
(bind-key "Line to Center of Window" #\meta-\#)
(bind-key "Top of Window" #\meta-\,)
(bind-key "Bottom of Window" #\meta-\.)

(bind-key "Exit Hemlock" '#(#\control-x #\control-z))
(bind-key "Exit Hemlock" '#(#\control-x #\control-\z))
(bind-key "Exit Recursive Edit" #\control-meta-z)
(bind-key "Abort Recursive Edit" #\control-])

(bind-key "Delete Previous Character" #\Delete)
(bind-key "Delete Previous Character" #\Backspace)
(bind-key "Kill Next Word" #\meta-d)
(bind-key "Kill Previous Word" #\meta-delete)
(bind-key "Kill Previous Word" #\meta-backspace)
(bind-key "Exchange Point and Mark" '#(#\control-x #\control-x))
(bind-key "Mark Whole Buffer" '#(#\control-x #\h))
(bind-key "Set/Pop Mark" #\control-@)
(bind-key "Set/Pop Mark" #\control-space)
(bind-key "Pop and Goto Mark" #\meta-space)
(bind-key "Pop and Goto Mark" #\meta-@)
(bind-key "Pop Mark" #\control-meta-space)  ;#\c-m-@ is "Mark Form".
(bind-key "Kill Region" #\control-w)
(bind-key "Save Region" #\meta-w)
(bind-key "Un-Kill" #\control-y)
(bind-key "Rotate Kill Ring" #\meta-y)

(bind-key "Forward Word" #\meta-f)
(bind-key "Backward Word" #\meta-b)

(bind-key "Forward Paragraph" #\meta-])
(bind-key "Forward Sentence" #\meta-e)
(bind-key "Backward Paragraph" #\meta-[)
(bind-key "Backward Sentence" #\meta-a)

(bind-key "Mark Paragraph" #\meta-h)

(bind-key "Forward Kill Sentence" #\meta-k)
(bind-key "Backward Kill Sentence" '#(#\control-x #\delete))
(bind-key "Backward Kill Sentence" '#(#\control-x #\backspace))

(bind-key "Beginning of Buffer" #\meta-<)
(bind-key "End of Buffer" #\meta->)
(bind-key "Mark to Beginning of Buffer" #\control-<)
(bind-key "Mark to End of Buffer" #\control->)

(bind-key "Extended Command" #\meta-x)

(bind-key "Uppercase Word" '#\meta-u)
(bind-key "Lowercase Word" '#\meta-l)
(bind-key "Capitalize Word" '#\meta-c)

(bind-key "Previous Page" '#(#\control-x #\[))
(bind-key "Next Page" '#(#\control-x #\]))
(bind-key "Mark Page" '#(#\control-x #\control-p))
(bind-key "Count Lines Page" '#(#\control-x #\l))



;;;; Argument Digit and Negative Argument.

(bind-key "Negative Argument" #\meta--)
(bind-key "Argument Digit" #\meta-0)
(bind-key "Argument Digit" #\meta-1)
(bind-key "Argument Digit" #\meta-2)
(bind-key "Argument Digit" #\meta-3)
(bind-key "Argument Digit" #\meta-4)
(bind-key "Argument Digit" #\meta-5)
(bind-key "Argument Digit" #\meta-6)
(bind-key "Argument Digit" #\meta-7)
(bind-key "Argument Digit" #\meta-8)
(bind-key "Argument Digit" #\meta-9)
(bind-key "Negative Argument" #\control--)
(bind-key "Argument Digit" #\control-0)
(bind-key "Argument Digit" #\control-1)
(bind-key "Argument Digit" #\control-2)
(bind-key "Argument Digit" #\control-3)
(bind-key "Argument Digit" #\control-4)
(bind-key "Argument Digit" #\control-5)
(bind-key "Argument Digit" #\control-6)
(bind-key "Argument Digit" #\control-7)
(bind-key "Argument Digit" #\control-8)
(bind-key "Argument Digit" #\control-9)
(bind-key "Negative Argument" #\control-meta--)
(bind-key "Argument Digit" #\control-meta-0)
(bind-key "Argument Digit" #\control-meta-1)
(bind-key "Argument Digit" #\control-meta-2)
(bind-key "Argument Digit" #\control-meta-3)
(bind-key "Argument Digit" #\control-meta-4)
(bind-key "Argument Digit" #\control-meta-5)
(bind-key "Argument Digit" #\control-meta-6)
(bind-key "Argument Digit" #\control-meta-7)
(bind-key "Argument Digit" #\control-meta-8)
(bind-key "Argument Digit" #\control-meta-9)


;;;; Self Insert and Quoted Insert.

(bind-key "Quoted Insert" #\control-q)

(bind-key "Self Insert" " ")
(bind-key "Self Insert" "!")
(bind-key "Self Insert" "@")
(bind-key "Self Insert" "#")
(bind-key "Self Insert" "$")
(bind-key "Self Insert" "%")
(bind-key "Self Insert" "^")
(bind-key "Self Insert" "&")
(bind-key "Self Insert" "*")
(bind-key "Self Insert" "(")
(bind-key "Self Insert" ")")
(bind-key "Self Insert" "_")
(bind-key "Self Insert" "+")
(bind-key "Self Insert" "~")
(bind-key "Self Insert" "1")
(bind-key "Self Insert" "2")
(bind-key "Self Insert" "3")
(bind-key "Self Insert" "4")
(bind-key "Self Insert" "5")
(bind-key "Self Insert" "6")
(bind-key "Self Insert" "7")
(bind-key "Self Insert" "8")
(bind-key "Self Insert" "9")
(bind-key "Self Insert" "0")
(bind-key "Self Insert" "[")
(bind-key "Self Insert" "]")
(bind-key "Self Insert" "\\")
(bind-key "Self Insert" "|")
(bind-key "Self Insert" ":")
(bind-key "Self Insert" ";")
(bind-key "Self Insert" "\"")
(bind-key "Self Insert" "'")
(bind-key "Self Insert" "-")
(bind-key "Self Insert" "=")
(bind-key "Self Insert" "`")
(bind-key "Self Insert" "<")
(bind-key "Self Insert" ">")
(bind-key "Self Insert" ",")
(bind-key "Self Insert" ".")
(bind-key "Self Insert" "?")
(bind-key "Self Insert" "/")
(bind-key "Self Insert" "{")
(bind-key "Self Insert" "}")



;;;; Echo Area.

;;; Basic echo-area commands.
;;; 
(bind-key "Help on Parse" #\home :mode "Echo Area")
(bind-key "Help on Parse" #\control-_ :mode "Echo Area")

(bind-key "Complete Keyword" #\escape :mode "Echo Area")
(bind-key "Complete Field" #\space :mode "Echo Area")
(bind-key "Confirm Parse" #\return :mode "Echo Area")

;;; Rebind some standard commands to behave better.
;;; 
(bind-key "Kill Parse" #\control-u :mode "Echo Area")
(bind-key "Insert Parse Default" #\control-i :mode "Echo Area")
(bind-key "Insert Parse Default" #\tab :mode "Echo Area")
(bind-key "Echo Area Delete Previous Character" #\delete :mode "Echo Area")
(bind-key "Echo Area Delete Previous Character" #\backspace :mode "Echo Area")
(bind-key "Echo Area Kill Previous Word" #\meta-h :mode "Echo Area")
(bind-key "Echo Area Kill Previous Word" #\meta-delete :mode "Echo Area")
(bind-key "Echo Area Kill Previous Word" #\meta-backspace :mode "Echo Area")
(bind-key "Echo Area Kill Previous Word" #\control-w :mode "Echo Area")
(bind-key "Beginning of Parse" #\control-a :mode "Echo Area")
(bind-key "Beginning of Parse" #\meta-< :mode "Echo Area")
(bind-key "Echo Area Backward Character" #\control-b :mode "Echo Area")
(bind-key "Echo Area Backward Word" #\meta-b :mode "Echo Area")
(bind-key "Next Parse" #\control-n :mode "Echo Area")
(bind-key "Previous Parse" #\control-p :mode "Echo Area")

;;; Nuke some dangerous standard bindings.
;;; 
(bind-key "Illegal" #\control-x :mode "Echo Area")
(bind-key "Illegal" #\control-meta-c :mode "Echo Area")
(bind-key "Illegal" #\control-meta-\c :mode "Echo Area")
(bind-key "Illegal" #\control-meta-l :mode "Echo Area")
(bind-key "Illegal" #\control-meta-\l :mode "Echo Area")
(bind-key "Illegal" #\meta-x :mode "Echo Area")
(bind-key "Illegal" #\control-s :mode "Echo Area")
(bind-key "Illegal" #\control-r :mode "Echo Area")
(bind-key "Illegal" #\middledown :mode "Echo Area")
(bind-key "Do Nothing" #\middleup :mode "Echo Area")
(bind-key "Illegal" #\super-leftdown :mode "Echo Area")
(bind-key "Do Nothing" #\super-leftup :mode "Echo Area")
(bind-key "Illegal" #\super-rightdown :mode "Echo Area")
(bind-key "Do Nothing" #\super-rightup :mode "Echo Area")



;;;; Eval and Editor Modes.

(bind-key "Confirm Eval Input" #\return :mode "Eval")
(bind-key "Previous Interactive Input" #\meta-\p :mode "Eval")
(bind-key "Search Previous Interactive Input" #\meta-p :mode "Eval")
(bind-key "Next Interactive Input" #\meta-n :mode "Eval")
(bind-key "Kill Interactive Input" #\meta-i :mode "Eval")
(bind-key "Abort Eval Input" #\control-meta-i :mode "Eval")
(bind-key "Interactive Beginning of Line" #\control-a :mode "Eval")
(bind-key "Reenter Interactive Input" #\control-return :mode "Eval")

(bind-key "Editor Evaluate Expression" #\control-meta-escape)
(bind-key "Editor Evaluate Expression" #\meta-escape  :mode "Editor")
(bind-key "Editor Evaluate Defun" '#(#\control-x #\control-e) :mode "Editor")
(bind-key "Editor Compile Defun" '#(#\control-x #\control-c) :mode "Editor")
(bind-key "Editor Compile Defun" '#(#\control-x #\control-\c) :mode "Editor")
(bind-key "Editor Macroexpand Expression" #\control-M :mode "Editor")
(bind-key "Editor Describe Function Call" #\control-meta-A :mode "Editor")
(bind-key "Editor Describe Symbol" #\control-meta-S :mode "Editor")



;;;; Typescript.

(bind-key "Confirm Typescript Input" #\return :mode "Typescript")
(bind-key "Interactive Beginning of Line" #\control-a :mode "Typescript")
(bind-key "Kill Interactive Input" #\meta-i :mode "Typescript")
;(bind-key "Abort Typescript Input" #\control-meta-i :mode "Typescript")
(bind-key "Previous Interactive Input" #\meta-\p :mode "Typescript")
(bind-key "Search Previous Interactive Input" #\meta-p :mode "Typescript")
(bind-key "Next Interactive Input" #\meta-n :mode "Typescript")
(bind-key "Reenter Interactive Input" #\control-return :mode "Typescript")
(bind-key "Typescript Slave Break" #\hyper-b :mode "Typescript")
(bind-key "Typescript Slave to Top Level" #\hyper-g :mode "Typescript")
(bind-key "Select Slave" #\control-meta-\c)
(bind-key "Select Background" #\control-meta-C)

(bind-key "Abort Operations" #\hyper-a)
(bind-key "List Operations" #\hyper-l)

(bind-key "Next Compiler Error" #\hyper-n)
(bind-key "Previous Compiler Error" #\hyper-p)



;;;; Lisp (some).

(bind-key "Indent Form" #\control-meta-q)
(bind-key "Defindent" #\control-meta-\#)
(bind-key "Beginning of Defun" #\control-meta-[)
(bind-key "End of Defun" #\control-meta-])
(bind-key "Beginning of Defun" #\control-meta-\a)
(bind-key "End of Defun" #\control-meta-e)
(bind-key "Forward Form" #\control-meta-\f)
(bind-key "Backward Form" #\control-meta-b)
(bind-key "Forward List" #\control-meta-n)
(bind-key "Backward List" #\control-meta-p)
(bind-key "Transpose Forms" #\control-meta-t)
(bind-key "Forward Kill Form" #\control-meta-k)
(bind-key "Backward Kill Form" #\control-meta-backspace)
(bind-key "Backward Kill Form" #\control-meta-delete)
(bind-key "Mark Form" #\control-meta-@)
(bind-key "Mark Defun" #\control-meta-h)
(bind-key "Insert ()" #\meta-\()
(bind-key "Move over )" #\meta-\))
(bind-key "Backward Up List" #\control-meta-\()
(bind-key "Backward Up List" #\control-meta-u)
(bind-key "Forward Up List" #\control-meta-\))
(bind-key "Down List" #\control-meta-d)
(bind-key "Extract List" #\control-meta-x)
(bind-key "Lisp Insert )" #\) :mode "Lisp")
(bind-key "Delete Previous Character Expanding Tabs" #\backspace :mode "Lisp")
(bind-key "Delete Previous Character Expanding Tabs" #\delete :mode "Lisp")

(bind-key "Evaluate Expression" #\meta-escape)
(bind-key "Evaluate Defun" '#(#\control-x #\control-e))
(bind-key "Compile Defun" '#(#\control-x #\control-c))
(bind-key "Compile Defun" '#(#\control-x #\control-\c))
(bind-key "Compile Buffer File" '#(#\control-x #\c))
(bind-key "Compile Buffer File" '#(#\control-x #\C))
(bind-key "Macroexpand Expression" #\control-M)

(bind-key "Describe Function Call" #\control-meta-A)
(bind-key "Describe Symbol" #\control-meta-S)

(bind-key "Goto Definition" #\control-meta-F)



;;;; More Miscellaneous bindings.

(bind-key "Open Line" #\Control-O)
(bind-key "New Line" #\return)

(bind-key "Transpose Words" #\meta-t)
(bind-key "Transpose Lines" '#(#\control-x #\control-t))
(bind-key "Transpose Regions" '#(#\control-x #\t))

(bind-key "Uppercase Region" '#(#\control-x #\control-u))
(bind-key "Lowercase Region" '#(#\control-x #\control-l))

(bind-key "Delete Indentation" #\meta-^)
(bind-key "Delete Indentation" #\control-meta-^)
(bind-key "Delete Horizontal Space" #\meta-\\)
(bind-key "Delete Blank Lines" '#(#\control-x #\control-o) :global)
(bind-key "Just One Space" #\meta-\|)
(bind-key "Back to Indentation" #\meta-m)
(bind-key "Back to Indentation" #\control-meta-m)
(bind-key "Indent Rigidly" '#(#\control-x #\tab))
(bind-key "Indent Rigidly" '#(#\control-x #\control-i))

(bind-key "Indent New Line" #\linefeed)
(bind-key "Indent" #\tab)
(bind-key "Indent" #\control-i)
(bind-key "Indent Region" #\control-meta-\\)
(bind-key "Quote Tab" #\meta-tab)

(bind-key "Directory" '#(#\control-x #\control-\d))
(bind-key "Verbose Directory" '#(#\control-x #\control-D))

(bind-key "Activate Region" '#(#\control-x #\control-@))
(bind-key "Activate Region" '#(#\control-x #\control-space))

(bind-key "Save Position" '#(#\control-x #\s))
(bind-key "Jump to Saved Position" '#(#\control-x #\j))
(bind-key "Put Register" '#(#\control-x #\x))
(bind-key "Get Register" '#(#\control-x #\g))

(bind-key "Delete Previous Character Expanding Tabs" #\backspace :mode "Pascal")
(bind-key "Delete Previous Character Expanding Tabs" #\delete :mode "Pascal")
(bind-key "Scribe Insert Bracket" #\) :mode "Pascal")
(bind-key "Scribe Insert Bracket" #\] :mode "Pascal")
(bind-key "Scribe Insert Bracket" #\} :mode "Pascal")


;;;; Auto Fill Mode.

(bind-key "Fill Paragraph" #\meta-q)
(bind-key "Fill Region" #\meta-g)
(bind-key "Set Fill Prefix" '#(#\control-x #\.))
(bind-key "Set Fill Column" '#(#\control-x #\f))
(bind-key "Auto Fill Return" #\return :mode "Fill")
(bind-key "Auto Fill Space" #\space :mode "Fill")
(bind-key "Auto Fill Linefeed" #\linefeed :mode "Fill")



;;;; Keyboard macro bindings.

(bind-key "Define Keyboard Macro" '#(#\control-x #\())
(bind-key "Define Keyboard Macro Key" '#(#\control-x #\meta-\())
(bind-key "End Keyboard Macro" '#(#\control-x #\)))
(bind-key "End Keyboard Macro" '#(#\control-x #\hyper-\)))
(bind-key "Last Keyboard Macro" '#(#\control-x #\e))
(bind-key "Keyboard Macro Query" '#(#\control-x #\q))



;;;; Spell bindings.

(bind-key "Check Word Spelling" #\meta-$)
(bind-key "Add Word to Spelling Dictionary" '#(#\control-x #\$))

(dolist (c (command-bindings (getstring "Self Insert" *command-names*)))
  (let ((ch (svref (car c) 0)))
    (unless (or (alpha-char-p ch) (char= ch #\'))
      (bind-key "Auto Check Word Spelling" (car c) :mode "Spell"))))
(bind-key "Auto Check Word Spelling" #\return :mode "Spell")
(bind-key "Auto Check Word Spelling" #\tab :mode "Spell")
(bind-key "Auto Check Word Spelling" #\linefeed :mode "Spell")
(bind-key "Correct Last Misspelled Word" #\meta-\:)
(bind-key "Undo Last Spelling Correction" '#(#\control-x #\a))



;;;; Overwrite Mode.

(bind-key "Overwrite Delete Previous Character" #\delete :mode "Overwrite")
(bind-key "Overwrite Delete Previous Character" #\backspace :mode "Overwrite")

;;; do up the printing characters ...
(do* ((i 33 (1+ i))
      (char (code-char i) (code-char i)))
     ((= i 126))
  (bind-key "Self Overwrite" char :mode "Overwrite"))

(bind-key "Self Overwrite" #\space :mode "Overwrite")



;;;; Comment bindings.

(bind-key "Indent for Comment" #\meta-\;)
(bind-key "Set Comment Column" '#(#\control-x #\;))
(bind-key "Kill Comment" #\control-meta-\;)
(bind-key "Down Comment Line" #\meta-n)
(bind-key "Up Comment Line" #\meta-p)
(bind-key "Up Comment Line" #\meta-\p)
(bind-key "Indent New Comment Line" #\meta-j)
(bind-key "Indent New Comment Line" #\meta-linefeed)


;;;; Word Abbrev Mode.

(bind-key "Add Mode Word Abbrev" '#(#\Control-X #\Control-A))
(bind-key "Add Global Word Abbrev" '#(#\Control-X #\+))
(bind-key "Inverse Add Mode Word Abbrev" '#(#\Control-X #\Control-H))
(bind-key "Inverse Add Global Word Abbrev" '#(#\Control-X #\-))
;; Removed in lieu of "Pop and Goto Mark".
;;(bind-key "Abbrev Expand Only" #\meta-space)
(bind-key "Word Abbrev Prefix Mark" #\meta-\")
(bind-key "Unexpand Last Word" '#(#\Control-X #\u))
(bind-key "Unexpand Last Word" '#(#\Control-X #\U))

(dolist (x '(#\! #\~ #\@ #\# #\; #\$ #\% #\^ #\& #\* #\- #\_ #\= #\+ #\[ #\]
	     #\( #\) #\/ #\| #\: #\' #\" #\{ #\} #\, #\< #\. #\> #\` #\\ #\?
	     #\return #\newline #\tab #\space))
  (bind-key "Abbrev Expand Only" x :mode "Abbrev"))



;;;; Scribe Mode.

(dolist (ch '(#\] #\) #\} #\>))
  (bind-key "Scribe Insert Bracket" ch :mode "Scribe"))

(bind-key "Scribe Buffer File" '#(#\control-x #\c) :mode "Scribe")
(bind-key "Scribe Buffer File" '#(#\control-x #\C) :mode "Scribe")
(bind-key "Select Scribe Warnings" #\control-meta-C :mode "Scribe")

(bind-key "Insert Scribe Directive" #\hyper-i :mode "Scribe")



;;;; X commands:

(bind-key "Insert Cut Buffer" #\insert)
(bind-key "Region to Cut Buffer" #\meta-insert)



;;;; Mailer commands.

(do-alpha-chars (c :both)
  (bind-key "Illegal" c :mode "Headers")
  (bind-key "Illegal" c :mode "Message"))


;;; Global.

(bind-key "Incorporate and Read New Mail" '#(#\control-x #\i))
(bind-key "Send Message" '#(#\control-x #\m))
(bind-key "Message Headers" '#(#\control-x #\r))
(bind-key "Message Headers" '#(#\control-x #\R))


;;; Both Headers and Message modes.
;;;
;;; The bindings in these two blocks should be the same, one for "Message" mode
;;; and one for "Headers" mode.
;;;

(bind-key "Next Message" #\meta-n :mode "Message")
(bind-key "Previous Message" #\meta-p :mode "Message")
(bind-key "Previous Message" #\meta-\p :mode "Message")
(bind-key "Next Undeleted Message" #\n :mode "Message")
(bind-key "Previous Undeleted Message" #\p :mode "Message")
(bind-key "Send Message" #\s :mode "Message")
(bind-key "Send Message" #\m :mode "Message")
(bind-key "Forward Message" #\f :mode "Message")
(bind-key "Headers Delete Message" #\k :mode "Message")
(bind-key "Headers Undelete Message" #\u :mode "Message")
(bind-key "Headers Undelete Message" #\U :mode "Message")
(bind-key "Headers Refile Message" #\o :mode "Message")
(bind-key "List Mail Buffers" #\l :mode "Message")
(bind-key "Quit Headers" #\q :mode "Message")
(bind-key "Incorporate and Read New Mail" #\i :mode "Message")
(bind-key "Beginning of Buffer" #\< :mode "Message")
(bind-key "End of Buffer" #\> :mode "Message")

(bind-key "Next Message" #\meta-n :mode "Headers")
(bind-key "Previous Message" #\meta-p :mode "Headers")
(bind-key "Previous Message" #\meta-\p :mode "Headers")
(bind-key "Next Undeleted Message" #\n :mode "Headers")
(bind-key "Previous Undeleted Message" #\p :mode "Headers")
(bind-key "Send Message" #\s :mode "Headers")
(bind-key "Send Message" #\m :mode "Headers")
(bind-key "Forward Message" #\f :mode "Headers")
(bind-key "Headers Delete Message" #\k :mode "Headers")
(bind-key "Headers Undelete Message" #\u :mode "Headers")
(bind-key "Headers Undelete Message" #\U :mode "Headers")
(bind-key "Headers Refile Message" #\o :mode "Headers")
(bind-key "List Mail Buffers" #\l :mode "Headers")
(bind-key "Quit Headers" #\q :mode "Headers")
(bind-key "Incorporate and Read New Mail" #\i :mode "Headers")
(bind-key "Beginning of Buffer" #\< :mode "Headers")
(bind-key "End of Buffer" #\> :mode "Headers")


;;; Headers mode.

(bind-key "Delete Message and Down Line" #\d :mode "Headers")
(bind-key "Delete Message and Down Line" #\D :mode "Headers")
(bind-key "Pick Headers" #\h :mode "Headers")
(bind-key "Show Message" #\space :mode "Headers")
(bind-key "Show Message" #\. :mode "Headers")
(bind-key "Reply to Message" #\r :mode "Headers")
(bind-key "Reply to Message" #\R :mode "Headers")
(bind-key "Expunge Messages" #\! :mode "Headers")
(bind-key "Headers Help" #\? :mode "Headers")


;;; Message mode.

(bind-key "Delete Message and Show Next" #\d :mode "Message")
(bind-key "Delete Message and Show Next" #\D :mode "Message")
(bind-key "Goto Headers Buffer" #\^ :mode "Message")
(bind-key "Scroll Message" #\space :mode "Message")
(bind-key "Scroll Message" #\control-v :mode "Message")
(bind-key "Scroll Window Up" #\backspace :mode "Message")
(bind-key "Scroll Window Up" #\delete :mode "Message")
(bind-key "Reply to Message in Other Window" #\r :mode "Message")
(bind-key "Reply to Message in Other Window" #\R :mode "Message")
(bind-key "Edit Message Buffer" #\e :mode "Message")
(bind-key "Insert Message Region" #\hyper-y :mode "Message")
(bind-key "Message Help" #\? :mode "Message")


;;; Draft mode.

(bind-key "Goto Headers Buffer" #\hyper-^ :mode "Draft")
(bind-key "Goto Message Buffer" #\hyper-m :mode "Draft")
(bind-key "Deliver Message" #\hyper-s :mode "Draft")
(bind-key "Deliver Message" #\hyper-c :mode "Draft")
(bind-key "Insert Message Buffer" #\hyper-y :mode "Draft")
(bind-key "Delete Draft and Buffer" #\hyper-q :mode "Draft")
(bind-key "List Mail Buffers" #\hyper-l :mode "Draft")
(bind-key "Draft Help" #\hyper-? :mode "Draft")



;;;; Process (Shell).

(bind-key "Confirm Process Input" #\return :mode "Process")
(bind-key "Shell" #\control-meta-\s)
(bind-key "Interrupt Buffer Subprocess" #\hyper-c :mode "Process")
(bind-key "Stop Buffer Subprocess" #\hyper-z :mode "Process")
(bind-key "Quit Buffer Subprocess" #\hyper-\\)
(bind-key "Send EOF to Process" #\hyper-d)

(bind-key "Previous Interactive Input" #\meta-\p :mode "Process")
(bind-key "Search Previous Interactive Input" #\meta-P :mode "Process")
(bind-key "Interactive Beginning of Line" #\control-a :mode "Process")
(bind-key "Kill Interactive Input" #\meta-i :mode "Process")
(bind-key "Next Interactive Input" #\meta-n :mode "Process")
(bind-key "Reenter Interactive Input" #\control-return :mode "Process")



;;;; Bufed.

(bind-key "Bufed" '#(#\control-x #\control-meta-b))
(bind-key "Bufed Delete" #\d :mode "Bufed")
(bind-key "Bufed Delete" #\D :mode "Bufed")
(bind-key "Bufed Delete" #\control-d :mode "Bufed")
(bind-key "Bufed Delete" #\control-\d :mode "Bufed")
(bind-key "Bufed Undelete" #\u :mode "Bufed")
(bind-key "Bufed Undelete" #\U :mode "Bufed")
(bind-key "Bufed Expunge" #\! :mode "Bufed")
(bind-key "Bufed Quit" #\q :mode "Bufed")
(bind-key "Bufed Goto" #\space :mode "Bufed")
(bind-key "Bufed Goto and Quit" #\super-leftdown :mode "Bufed")
(bind-key "Bufed Save File" #\s :mode "Bufed")
(bind-key "Next Line" #\n :mode "Bufed")
(bind-key "Previous Line" #\p :mode "Bufed")


(bind-key "Bufed Help" #\? :mode "Bufed")



;;;; Dired.

(bind-key "Dired" '#(#\control-x #\control-meta-\d))

(bind-key "Dired Delete File and Down Line" #\d :Mode "Dired")
(bind-key "Dired Delete File with Pattern" #\D :Mode "Dired")
(bind-key "Dired Delete File" #\control-\d :Mode "Dired")
(bind-key "Dired Delete File" #\control-d :Mode "Dired")
(bind-key "Dired Delete File" #\k :Mode "Dired")

(bind-key "Dired Undelete File and Down Line" #\u :Mode "Dired")
(bind-key "Dired Undelete File with Pattern" #\U :Mode "Dired")
(bind-key "Dired Undelete File" #\control-u :Mode "Dired")

(bind-key "Dired Expunge Files" #\! :Mode "Dired")
(bind-key "Dired Update Buffer" #\hyper-u :Mode "Dired")
(bind-key "Dired View File" #\space :Mode "Dired")
(bind-key "Dired Edit File" #\e :Mode "Dired")
(bind-key "Dired Quit" #\q :Mode "Dired")
(bind-key "Dired Help" #\? :Mode "Dired")

(bind-key "Dired Copy File" #\c :Mode "Dired")
(bind-key "Dired Copy with Wildcard" #\C :Mode "Dired")
(bind-key "Dired Rename File" #\r :Mode "Dired")
(bind-key "Dired Rename with Wildcard" #\R :Mode "Dired")

(bind-key "Next Line" #\n :mode "Dired")
(bind-key "Previous Line" #\p :mode "Dired")



;;;; View Mode.

(bind-key "View Scroll Down" #\space :mode "View")
(bind-key "Scroll Window Up" #\b :mode "View")
(bind-key "Scroll Window Up" #\backspace :Mode "View")
(bind-key "Scroll Window Up" #\delete :Mode "View")
(bind-key "View Return" #\^ :mode "View")
(bind-key "View Quit" #\q :mode "View")
(bind-key "View Edit File" #\e :mode "View")
(bind-key "View Help" #\? :mode "View")
(bind-key "Beginning of Buffer" #\< :Mode "View")
(bind-key "End of Buffer" #\> :Mode "View")



;;;; Lisp Library.


(bind-key "Describe Pointer Library Entry" #\leftdown :Mode "Lisp-Lib")
(bind-key "Load Pointer Library Entry" #\rightdown :Mode "Lisp-Lib")
(bind-key "Describe Library Entry" #\space :Mode "Lisp-Lib")
(bind-key "Load Library Entry" #\l :Mode "Lisp-Lib")
(bind-key "Exit Lisp Library" #\q :Mode "Lisp-Lib")
(bind-key "Lisp Library Help" #\? :Mode "Lisp-Lib")



;;;; Completion mode.

(dolist (c (command-bindings (getstring "Self Insert" *command-names*)))
  (bind-key "Completion Self Insert" (car c) :mode "Completion"))

(bind-key "Completion Self Insert" #\Space :mode "Completion")
(bind-key "Completion Self Insert" #\Tab :mode "Completion")
(bind-key "Completion Self Insert" #\Return :mode "Completion")
(bind-key "Completion Self Insert" #\Linefeed :mode "Completion")

(bind-key "Completion Complete Word" #\End)
(bind-key "Completion Rotate Completions" #\Meta-End)



;;;; Logical characters.

(setf (logical-char= #\control-s :forward-search) t)
(setf (logical-char= #\control-r :backward-search) t)
(setf (logical-char= #\control-r :recursive-edit) t)
(setf (logical-char= #\delete :cancel) t)
(setf (logical-char= #\backspace :cancel) t)
(setf (logical-char= #\control-g :abort) t)
(setf (logical-char= #\escape :exit) t)
(setf (logical-char= #\Y :yes) t)
(setf (logical-char= #\space :yes) t)
(setf (logical-char= #\N :no) t)
(setf (logical-char= #\backspace :no) t)
(setf (logical-char= #\delete :no) t)
(setf (logical-char= #\! :do-all) t)
(setf (logical-char= #\. :do-once) t)
(setf (logical-char= #\home :help) t)
(setf (logical-char= #\H :help) t)
(setf (logical-char= #\? :help) t)
(setf (logical-char= #\control-_ :help) t)
(setf (logical-char= #\return :confirm) t)
(setf (logical-char= #\control-q :quote) t)
(setf (logical-char= #\K :keep) t)


