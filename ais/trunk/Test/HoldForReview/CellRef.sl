;;/**********************************************************************************
;;    Copyright (C) 2008 Investment Science Corp.
;;
;;    This program is free software: you can redistribute it and/or modify
;;    it under the terms of the GNU General Public License as published by
;;    the Free Software Foundation, either version 3 of the License, or
;;    any later version.
;;
;;    This program is distributed in the hope that it will be useful,
;;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;    GNU General Public License for more details.
;;
;;    You should have received a copy of the GNU General Public License
;;    along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;***********************************************************************************/
;;
;;  Title:    SmartLisp Cell reference Test
;;
;;  Author:   Michael F. Korns
;;
;;  Project:  SmartBase Automated test suite 
;;
;;  Notes:    The SmartBase cell references and functions for 
;;            various objects
;;            tested in this test suite script.
;;
;;  Files:    No dependencies. 
;;

;;************************************************************************
;; Test script global variables
;;************************************************************************
(setq scriptName "Cell References Test")

;;************************************************************************
;;  Test Script diagnostic and utility functions
;;************************************************************************
(setq diagnostic writeln)

(defun testit(evalTxt result) 
   (setq lt evalTxt)
   (setq lr result)
   (if (not (isEqual (eval evalTxt) result ))
       (begin
         (diagnostic scriptName " *FAILURE* " evalTxt)
         (error "Cellref"))
       true))


;;************************************************************************
;;  Start the Test Script
;;************************************************************************
(writeln   scriptName " started")
(deleteView aST:)
(setq aST #void)

(setq aST (makeSmarttable normal: #{Id: key: Synopsis: formula: Description: key:} aST: ))

;; now populate the rows.
(loop for i from 0 until 5 do
	(setq aST[Id: i] i)
	(setq aST[Synopsis: i] (append "Synopsis" i))
	(setq aST[Description: i] (append "Description" i)))


;; now we should have 5 rows in our table.
;; Test the contents of random cells using absolute and relative cell references

( testit "aST [0 0] " 0 )
( testit "aST [1 0] " "Synopsis0" )
( testit "aST [2 0] " "Description0" )


( testit "(ref aST 0 0)" 0 )
( testit "(ref aST 1 0)" "Synopsis0" )
( testit "(ref aST 2 0)" "Description0" )


( testit "(ref aST '#$a$1)" 0 )
( testit "(ref aST '#$b$1)" "Synopsis0" )
( testit "(ref aST '#$c$1)" "Description0" )


( testit "(ref aST #B2)"     (ref aST 1 1 ))
( testit "(ref aST '#B2)"    (ref aST 1 1 ))
( testit "(ref aST  #[1,1])" (ref aST 1 1))



;;************************************************************************
;;  Test enterCellText and getCellValue
;;************************************************************************

(enterCellText aST 1 1 "Hello")
( testit "(getCellValue aST 1 1 )" "Hello" )

;;************************************************************************
;;  Test makeCell and makeRange
;;************************************************************************

;; create a cell reference

(testit "(ref (makeCell aST 1 1))" (ref aST #B2))

(setq a (ref (makeCell aST 1 1)))
( testit "a" "Hello" )

(setq b (ref (makeRange aST 1 1 2 2)))
(objectToRange #(1 2 3 4) b)
( testit "aST[1 1]" 1 )
( testit "aST[1 2]" 2 )
( testit "aST[2 1]" 3 )
( testit "aST[2 2]" 4 )

(setq v (makeRange aST 1 1 2 2))
( testit "(recalc v)" true )

;;************************************************************************
;;  Test foreign cell references
;;************************************************************************

(setq ss (new Spreadsheet:))

(enterCellText ss 0 0 "Hello")
( testit "ss[0 0]" "Hello" )
( testit "(ref ss:#A1)" "Hello")
( testit "(ref ss:#[0,0])" "Hello")


(setq ss.name Mike:)
(addView ss)
( testit "(ref @Mike:#$A$1)" "Hello")
( testit "@Mike:#A1" "Hello")
( testit "@Mike:#[0,0]" "Hello")



;;************************************************************************
;;  Test Directory References
;;************************************************************************

(setq a (new Directory: A: "a" B: "b" C: "c"))
(testit "(length a)" 3)
(testit "a.A" "a")
(testit "a.B" "b")
(testit "a.C" "c")

(setq a.A #void)           ;; Delete an item
(testit "(length a)" 2)    ;; Check the item count
(testit "a.A" #void)
(testit "a.B" "b")
(testit "a.C" "c")


(setq a.B #void)              ;; Delete an item
(testit "(length a)" 1)       ;; Check the item count
(testit "a.A" #void)
(testit "a.B" #void)
(testit "a.C" "c")

(setq a.C #void)              ;; Delete an item
(testit "(length a)" 0)       ;; Check the item count
(testit "a.A" #void)
(testit "a.B" #void)
(testit "a.C" #void)


(setq b (new Directory: 1: "a" 2: "b" 3: "c"))
(testit "(length b)" 3)
(testit "b[1]" "a")
(testit "b[2]" "b")
(testit "b[3]" "c")

(setq b[1] #void)           ;; Delete an item
(testit "(length b)" 2)    ;; Check the item count
(testit "b[1]" #void)
(testit "b[2]" "b")
(testit "b[3]" "c")


(setq b[2] #void)              ;; Delete an item
(testit "(length b)" 1)       ;; Check the item count
(testit "b[1]" #void)
(testit "b[2]" #void)
(testit "b[3]" "c")

(setq b[3] #void)              ;; Delete an item
(testit "(length b)" 0)       ;; Check the item count
(testit "b[1]" #void)
(testit "b[2]" #void)
(testit "b[3]" #void)


;;************************************************************************
;;  Test Dictionary References
;;************************************************************************

(setq aDic (new Dictionary: A: "a" B: "b" C: "c"))
(testit "(length aDic)" 3)
(testit "aDic.A" "a")
(testit "aDic.B" "b")
(testit "aDic.C" "c")

(setq aDic.A #void)           ;; Delete an item
(testit "(length aDic)" 2)    ;; Check the item count
(testit "aDic.A" #void)
(testit "aDic.B" "b")
(testit "aDic.C" "c")


(setq aDic.B #void)              ;; Delete an item
(testit "(length aDic)" 1)       ;; Check the item count
(testit "aDic.A" #void)
(testit "aDic.B" #void)
(testit "aDic.C" "c")

(setq aDic.C #void)              ;; Delete an item
(testit "(length aDic)" 0)       ;; Check the item count
(testit "aDic.A" #void)
(testit "aDic.B" #void)
(testit "aDic.C" #void)


(setq aDic (new Dictionary: A: "a" B: "b" C: "c"))
(testit "(length aDic)" 3)
(testit "aDic[0]" "a")
(testit "aDic[1]" "b")
(testit "aDic[2]" "c")

(setq aDic[0] #void)           ;; Delete an item
(testit "(length aDic)" 2)     ;; Check the item count
(testit "aDic[0]" "b")
(testit "aDic[1]" "c")

(setq aDic[1] #void)           ;; Delete an item
(testit "(length aDic)" 1)     ;; Check the item count
(testit "aDic[0]" "b")

(setq aDic[0] #void)           ;; Delete an item
(testit "(length aDic)" 0)     ;; Check the item count



;;************************************************************************
;;  Miscellaneous Bug Fixes
;;************************************************************************

;;************************************************************************
;; Test the renameCol function
;;************************************************************************

(deleteView Defect:)
(setq Defect #void)

(setq Defect  (makeSmarttable normal: #{Id: key: Synopsis: key: Description: key:} Defect: ))

;; now populate the rows.
(loop for i from 0 until 5 do
	(setq Defect[Id: i] i)
	(setq Defect[Synopsis: i] (append "Synopsis" i))
	(setq Defect[Description: i] (append "Description" i)))

(renameCol Defect Synopsis: NewSynopsis: )
(testit "(getColHeader Defect 0 1)" "NewSynopsis")


(defun cmpeq (x y) (= x y))
(cmpeq 'x ''x)
(cmpeq 'x ''x)

;;************************************************************************
;; Test comparisons of symbols and quoted symbols
;;************************************************************************

(defun cmpeq (x y) (= x y))
(defun cmpne (x y) (<> x y))
(defun cmpgt (x y) (> x y))
(defun cmplt (x y) (< x y))
(defun cmpge (x y) (>= x y))
(defun cmple (x y) (<= x y))

(testit "(cmpeq 'x ''x)" true)
(testit "(cmpeq 'x ''x)" true)

(testit "(cmpne 'x ''y)" true)
(testit "(cmpne 'x ''x)" false)

(testit "(cmpgt 'x ''y)" false)
(testit "(cmpgt 'y ''x)" true)

(testit "(cmplt 'x ''y)" true)
(testit "(cmplt 'y ''x)" false)

(testit "(cmpge 'x ''y)" false)
(testit "(cmpge 'y ''x)" true)
(testit "(cmpge 'y ''y)" true)

(testit "(cmple 'x ''y)" true)
(testit "(cmple 'y ''x)" false)
(testit "(cmple 'y ''y)" true)
(testit "(cmple ''y 'y)" true)

;;************************************************************************
;; Test comparisons of pairs and quoted pairs
;;************************************************************************
(testit "(compare '(1 2 3) ''( 1 2 3))" 0)
(testit "(compare '(1 2 3) ''(4 5 6))"  -1)
(testit "(compare ''(4 5 6) '(1 2 3) )"  1)

(testit "(compareEQ '(1 2 3) ''( 1 2 3))" true)
(testit "(compareLT '(1 2 3) ''(4 5 6))"  true)
(testit "(compareLE '(1 2 3) ''(4 5 6))"  true)
(testit "(compareNE '(1 2 3) ''(4 5 6))"  true)
(testit "(compareGT ''(4 5 6) '(1 2 3) )"  true)
(testit "(compareGE ''(4 5 6) '(1 2 3) )"  true)

;;************************************************************************
;; Test the isMember and member functions using quoted pairs
;;************************************************************************

(testit "(isMember 'x #(x y))" true)
(testit "(isMember ''(1 2 3) #((1 2 3) (4 5 6)))" true)
(testit "(isMember ''(1 2 3) #((4 5 6) (7 8 9)))" false)
(testit "(member ''(1 2 3) #((1 2 3) (4 5 6)))" 0)
(testit "(member ''(1 2 3) #((4 5 6) (7 8 9)))" false)

(setq aDic (new Dictionary: A: "a" B: "b" C: "c"))
(testit "(isMember 'A aDic)" true)
(testit "(isMember ''A aDic)" true)
(testit "(isMember ''X aDic)" false)
(testit "(isMember ''A aDic 0)" true)
(testit "(isMember ''a aDic 1)" true)
(testit "(member 'A aDic)" 0)
(testit "(member ''A aDic)" 0)
(testit "(member 'X aDic)" false)
(testit "(member 'A aDic  0)" 0)
(testit "(member 'a aDic  1)" 0)

(setq aDir (new Directory: A: "a" B: "b" C: "c"))
(testit "(isMember 'A aDir)" true)
(testit "(isMember ''A aDir)" true)
(testit "(isMember ''X aDir)" false)
(testit "(isMember ''A aDir 0)" true)
(testit "(isMember ''a aDir 1)" true)
(testit "(member 'A aDir)" 0)
(testit "(member ''A aDir)" 0)
(testit "(member 'X aDir)" false)
(testit "(member 'A aDir 0)" 0)
(testit "(member 'a aDir 1)" 0)

(setq aStruc (new Structure: A: "a" B: "b" C: "c"))
(testit "(isMember 'A aStruc )" true)
(testit "(isMember ''A aStruc )" true)
(testit "(isMember 'X aStruc )" false)
(testit "(isMember ''A aStruc 0)" true)
(testit "(isMember ''a aStruc 1)" true)

(testit "(member 'A aStruc)" 0)
(testit "(member ''A aStruc)" 0)
(testit "(member 'X aStruc)" false)
(testit "(member 'A aStruc 0)" 0)
(testit "(member 'a aStruc 1)" 0)

(testit "(isMember ''(1 2 3) '((1 2 3) (4 5 6)))" true)
(testit "(isMember '(1 2 3) ''((1 2 3) (4 5 6)))" true)
(testit "(isMember ''(1 2 3) '((4 5 6) (7 8 9)))" false)
(testit "(member '(1 2 3) ''((1 2 3) (4 5 6)))" 0)
(testit "(member ''(1 2 3) '((1 2 3) (4 5 6)))" 0)
(testit "(member '(1 2 3) ''((7 8 9) (4 5 6)))" false)

;;************************************************************************
;; Test the objectToStructure Function
;;************************************************************************

;; (objectToStructure {object})

(setq aDir (new Directory: A: 1 B: 2 C: 3))
(setq aStruct  #{A: 1 B: 2 C: 3})
(testit "(objectToStructure aDir)" aStruct)

(setq aDic (new Dictionary: A: 1 B: 2 C: 3))
(testit "(objectToStructure aDic)" aStruct)


;; (objectToStructure {structure} {vector})
;; (objectToStructure {structure} {small vector})
;; (objectToStructure {structure} {int vector})
;; (objectToStructure {structure} {num vector})

(setq aVec    (makeVector   3  1 2 3))               ; define a small vector
(setq aVecSml (makeVector small:   3  1 2 3))        ; define a small vector(setq aVecFlt (makeVector float:   3  7.9 7.8 7.7))  ; define a float vector
(setq aVecInt (makeVector integer: 3  1 2 3 ))       ; define an integer vector
(setq aVecNum (makeVector number: 3 1 2 3 ))         ; define a number vector

       
(setq aStruc   (new Structure: A: "a" B: "b" C: "c"))
(setq newStruc #{A: 1 B: 2 C: 3 })
(testit "(objectToStructure  aStruc aVec)"     newStruc)
(setq aStruc   (new Structure: A: "a" B: "b" C: "c"))
(setq newStruc #{A: 1 B: 2 C: 3 })
(testit "(objectToStructure  aStruc aVecSml )" newStruc)
(setq aStruc   (new Structure: A: "a" B: "b" C: "c"))
(setq newStruc #{A: 1 B: 2 C: 3 })
(testit "(objectToStructure  aStruc aVecInt )" newStruc)
(setq aStruc   (new Structure: A: "a" B: "b" C: "c"))
(setq newStruc #{A: 1 B: 2 C: 3 })
(testit "(objectToStructure  aStruc aVecNum )" newStruc)

;; (objectToStructure {vector} {vector})

(setq xVec (objectToStructure  #(A B C D) #(1 2 3)))
(testit "xVec.A" 1)
(testit "xVec.B" 2)
(testit "xVec.C" 3)
(testit "xVec.D" #void)


;; (objectToStructure {structure} {structure})
;; (objectToStructure {structure} {dictionary})
;; (objectToStructure {structure} {directory})

(setq aStruc1 (new Structure: A: "a" B: "b" C: "c"))
(setq aStruc2 (new Structure: A:  1  B:  2  C: 3 D: "d"))
(setq newStruc #{A: 1 B: 2 C: 3})
(testit "(objectToStructure  aStruc1 aStruc2)" newStruc)

(setq aStruc1 (new Structure: A: "a" B: "b" C: "c"))
(setq aDir    (new Directory: A: 1   B:  2  C: 3  D: 4))
(testit "(objectToStructure  aStruc1 aDir)" newStruc)

(setq aStruc1 (new Structure: A: "a" B: "b" C: "c"))
(setq aDic (new Dictionary: A: 1 B: 2 C: 3 D: 6))
(testit "(objectToStructure  aStruc1 aDic)" newStruc)

;; (objectToStructure {directory} {structure})
;; (objectToStructure {directory} {dictionary})
;; (objectToStructure {directory} {directory})
;; (objectToStructure {dictionary} {structure})
;; (objectToStructure {dictionary} {dictionary})
;; (objectToStructure {dictionary} {directory})

(setq aDic1 (new Dictionary: A: "a"  B: "b"  C: "c" ))
(setq aDic2 (new Dictionary: A: 1 B: 2 C: 3 D: 6))
(setq newStruc #{A: 1 B: 2 C: 3 D: 6})
(testit "(objectToStructure  aDic1 aDic2)" newStruc)

(setq aDir1 (new Directory: A: "a"  B: "b"  C: "c" ))
(setq aDir2 (new Directory: A:  1   B:  2   C: 3   D: 6))
(testit "(objectToStructure  aDir1 aDir2)" newStruc)

(setq aDic1 (new Dictionary: A: "a"  B: "b"  C: "c" ))
(setq aDir2 (new Directory: A:  1   B:  2   C: 3   D: 6))
(testit "(objectToStructure  aDic1 aDir2)" newStruc)

(setq aDir1 (new Directory: A: "a"  B: "b"  C: "c" ))
(setq aDic2 (new Dictionary: A: 1 B: 2 C: 3 D: 6))
(testit "(objectToStructure  aDir1 aDic2)" newStruc)

(setq aDic1 (new Dictionary: A: "a"  B: "b"  C: "c" ))
(setq aStruc (new Structure: A: 1 B: 2 C: 3 D: 6 ))
(testit "(objectToStructure  aDic1 aStruc)" newStruc)

(setq aDir1 (new Dictionary: A: "a"  B: "b"  C: "c" ))
(setq aStruc (new Structure: A: 1 B: 2 C: 3 D: 6 ))
(testit "(objectToStructure  aDir1 aStruc)" newStruc)

;;************************************************************************
;; Test the objectToDictionary Function
;;************************************************************************

;; (objectToDictionary {object})

(setq aDic (new Dictionary: A: 1 B: 2 C: 3))
(setq aDir (new Directory: A: 1 B: 2 C: 3))
(setq aStruc (new Structure: A: 1 B: 2 C: 3))

(testit "(objectToDictionary aDir)"    aDic )
(testit "(objectToDictionary aStruct)" aDic )

(setq xVec (objectToDictionary  #(A B C) #(1 2 3)))
(testit "(objectToDictionary  #(A B C) #(1 2 3))" aDic )

(setq xVec (objectToDictionary  #(A B C D) #(1 2 3)))
(testit "xVec.A" 1)
(testit "xVec.B" 2)
(testit "xVec.C" 3)
(testit "xVec.D" #void)

;; (objectToDictionary {structure} {vector})
;; (objectToDictionary {structure} {small vector})
;; (objectToDictionary {structure} {int vector})
;; (objectToDictionary {structure} {num vector})

(setq aVec    (makeVector   3  1 2 3))               ; define a small vector
(setq aVecSml (makeVector small:   3  1 2 3))        ; define a small vector
(setq aVecFlt (makeVector float:   3  7.9 7.8 7.7))  ; define a float vector
(setq aVecInt (makeVector integer: 3  1 2 3 ))       ; define an integer vector
(setq aVecNum (makeVector number: 3 1 2 3 ))         ; define a number vector

(setq aStruc  (new Structure: A: "a" B: "b" C: "c"))
(setq newDic  (new Dictionary: A: 1 B: 2 C: 3 ))

(testit "(objectToDictionary  aStruc aVec)"   newDic)
(testit "(objectToDictionary  aStruc aVecSml )" newDic)
(testit "(objectToDictionary  aStruc aVecInt )" newDic)
(testit "(objectToDictionary  aStruc aVecNum )" newDic)

;; (objectToDictionary {structure} {structure})
;; (objectToDictionary {structure} {dictionary})
;; (objectToDictionary {structure} {directory})

(setq aDir (new Directory: A: 1   B:  2  C: 3  D: 4))
(testit  "(objectToDictionary  aStruc aDir)" newDic)

(setq aDic (new Dictionary: A: 1 B: 2 C: 3 D: 6))
(testit "(objectToDictionary  aStruc aDic)" newDic)

(setq aStruc2 (new Structure: A: 1 B: 2 C: 3 D: 6))
(testit "(objectToDictionary  aStruc aStruc2)" newDic)

;; (objectToDictionary {directory} {structure})
;; (objectToDictionary {directory} {dictionary})
;; (objectToDictionary {directory} {directory})
;; (objectToDictionary {dictionary} {structure})
;; (objectToDictionary {dictionary} {dictionary})
;; (objectToDictionary {dictionary} {directory})

(setq aDic1 (new Dictionary: A: "a"  B: "b"  C: "c" ))
(setq aDic2 (new Dictionary: A: 1 B: 2 C: 3 D: 6))
(setq newDic  (new Dictionary: A: 1 B: 2 C: 3 D: 6))
(testit "(objectToDictionary  aDic1 aDic2)" newDic)

(setq aDir1 (new Directory: A: "a"  B: "b"  C: "c" ))
(setq aDir2 (new Directory: A:  1   B:  2   C: 3   D: 6))
(testit "(objectToDictionary  aDir1 aDir2)" newDic)

(setq aDic1 (new Dictionary: A: "a"  B: "b"  C: "c" ))
(setq aDir2 (new Directory: A:  1   B:  2   C: 3   D: 6))
(testit "(objectToDictionary  aDic1 aDir2)" newDic)

(setq aDir1 (new Directory: A: "a"  B: "b"  C: "c" ))
(setq aDic2 (new Dictionary: A: 1 B: 2 C: 3 D: 6))
(testit "(objectToDictionary  aDir1 aDic2)" newDic)

(setq aDic1 (new Dictionary: A: "a"  B: "b"  C: "c" ))
(setq aStruc (new Structure: A: 1 B: 2 C: 3 D: 6 ))
(testit "(objectToDictionary  aDic1 aStruc)" newDic)

(setq aDir1 (new Directory: A: "a"  B: "b"  C: "c" ))
(setq aStruc (new Structure: A: 1 B: 2 C: 3 D: 6 ))
(testit "(objectToDictionary  aDir1 aStruc)" newDic)

;;************************************************************************
;; Test the objectToDirectory Function
;;************************************************************************

;; objectToDirectory {object})

(setq aDic (new Dictionary: A: 1 B: 2 C: 3))
(setq aDir (new Directory: A: 1 B: 2 C: 3))
(setq aStruc (new Structure: A: 1 B: 2 C: 3))

(testit "(objectToDirectory aDic)"    aDir )
(testit "(objectToDirectory aStruct)" aDir )

(setq xVec (objectToDirectory  #(A B C) #(1 2 3)))
(testit "(objectToDirectory  #(A B C) #(1 2 3))" aDir )

(setq xVec (objectToDirectory  #(A B C D) #(1 2 3)))
(testit "xVec.A" 1)
(testit "xVec.B" 2)
(testit "xVec.C" 3)
(testit "xVec.D" #void)

;; (objectToDirectory {structure} {vector})
;; (objectToDirectory {structure} {small vector})
;; (objectToDirectory {structure} {int vector})
;; (objectToDirectory {structure} {num vector})

(setq aVec    (makeVector   3  1 2 3))               ; define a small vector
(setq aVecSml (makeVector small:   3  1 2 3))        ; define a small vector
(setq aVecFlt (makeVector float:   3  7.9 7.8 7.7))  ; define a float vector
(setq aVecInt (makeVector integer: 3  1 2 3 ))       ; define an integer vector
(setq aVecNum (makeVector number: 3 1 2 3 ))         ; define a number vector

(setq aStruc  (new Structure: A: "a" B: "b" C: "c"))
(setq newDir  (new Directory: A: 1 B: 2 C: 3 ))

(testit "(objectToDirectory  aStruc aVec)"   newDir)
(testit "(objectToDirectory  aStruc aVecSml )" newDir)
(testit "(objectToDirectory  aStruc aVecInt )" newDir)
(testit "(objectToDirectory  aStruc aVecNum )" newDir)

;; (objectToDirectory {structure} {structure})
;; (objectToDirectory {structure} {dictionary})
;; (objectToDirectory {structure} {directory})

(setq aStruc  (new Structure: A: "a" B: "b" C: "c"))
(setq aDir (new Directory: A: 1   B:  2  C: 3  D: 4))
(setq newDir  (new Directory: A: 1 B: 2 C: 3 ))
(testit  "(objectToDirectory  aStruc aDir)" newDir)

(setq aDic (new Dictionary: A: 1 B: 2 C: 3 D: 6))
(testit "(objectToDirectory  aStruc aDic)" newDir)

(setq aStruc2 (new Structure: A: 1 B: 2 C: 3 D: 6))
(testit "(objectToDirectory  aStruc aStruc2)" newDir)



;; (objectToDirectory {directory} {structure})
;; (objectToDirectory {directory} {dictionary})
;; (objectToDirectory {directory} {directory})
;; (objectToDirectory {dictionary} {structure})
;; (objectToDirectory {dictionary} {dictionary})
;; (objectToDirectory {dictionary} {directory})

(setq aDir1 (new Directory: A: "a"  B: "b"  C: "c" ))
(setq aDir2 (new Directory: A: 1 B: 2 C: 3 D: 6))
(setq newDir  (new Directory: A: 1 B: 2 C: 3 D: 6))
(testit "(objectToDirectory  aDir1 aDir2)" newDir)

(setq aDic1 (new Dictionary: A: "a"  B: "b"  C: "c" ))
(setq aDic2 (new Dictionary: A:  1   B:  2   C: 3   D: 6))
(testit "(objectToDirectory  aDic1 aDic2)" newDir)

(setq aDic1 (new Dictionary: A: "a"  B: "b"  C: "c" ))
(setq aDir2 (new Directory: A:  1   B:  2   C: 3   D: 6))
(testit "(objectToDirectory  aDic1 aDir2)" newDir)

(setq aDir1 (new Directory: A: "a"  B: "b"  C: "c" ))
(setq aDic2 (new Dictionary: A: 1 B: 2 C: 3 D: 6))
(testit "(objectToDirectory  aDir1 aDic2)" newDir)

(setq aDic1 (new Dictionary: A: "a"  B: "b"  C: "c" ))
(setq aStruc (new Structure: A: 1 B: 2 C: 3 D: 6 ))
(testit "(objectToDirectory  aDic1 aStruc)" newDir)

(setq aDir1 (new Directory: A: "a"  B: "b"  C: "c" ))
(setq aStruc (new Structure: A: 1 B: 2 C: 3 D: 6 ))
(testit "(objectToDirectory  aDir1 aStruc)" newDir)


(writeln   scriptName " Completed")






























