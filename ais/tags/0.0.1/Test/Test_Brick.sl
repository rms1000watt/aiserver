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
;;  
;;  Title:    Lisp Brick Procedures Test
;;
;;  Author:   Michael F. Korns
;			  Tim May
;;
;;  Project:  AIS Regression Suite
;;
;;  Notes:    The SmartBase features in the Brick Procedures
;;            chapter are tested in this test suite script.
;;
;;  Files:    RegTest.sl
;;

;#memory=500
;#memoryObjectHeaders=100
(runScript "RegTest.sl")
(testStart "Test_Brick.sl")

;;************************************************************************
;; Additional tests for the Brick Procedure
;;************************************************************************

(defun foo()
    pvars:(a b) 
    vars:((X #(brk| w:Word:1 x:Object:1 y:Number:1 z:Integer:1))) 
    regs:(CharPointer:ptr Integer:n) 
    (setq ptr X) 
    (setq X.z 22)
    (setq n ptr[X(z)])
    n)
(testit {(foo)} 22)

(defun foo()
    pvars:(a b) 
    vars:((X #(brk| w:Word:1 x:Object:1 y:Number:1 z:Integer:1))) 
    regs:(CharPointer:ptr Integer:n) 
    (setq ptr X) 
    (setq X.w "Hello")
    (setq a ptr[X(w)])
    a)
(testit {(foo)} "Hello")

(defun foo()
    pvars:(a b) 
    vars:((X #(brk| w:Word:1 x:Object:1 y:Number:1 z:Integer:1))) 
    regs:(CharPointer:ptr Number:n) 
    (setq ptr X) 
    (setq ptr[X(y)] 34.56)
    (setq n ptr[X(y)])
    n)
(testit {(foo)} 34.56)

(defun foo()
    pvars:(a b) 
    vars:((X #(brk| w:Integer:1 x:Object:1 y:Number:1 z:Integer:1))) 
    regs:(CharPointer:ptr Integer:n) 
    (setq ptr X) 
    (vmnatSaveInteger 34 ptr[:0:])
    (setq n X.w)
    n)
(testit {(foo)} 34)

;;************************************************************************
;; Additional tests for the Brick Procedure
;;************************************************************************

(define X #(brk| x:Word:2 y:Object:2 z:Integer:5))

(testit {X["RowCount"]} 1)

(testit {X["RowByteCount"]} (if (= _ais.bits 64) 88 60))

(testit {X["FieldCount"]} 3)

(testit {X["ByteCount"]} (if (= _ais.bits 64) 88 60))

(testit "(setq X.z 22)[0][2][0]" 22)

(testit "X.z[0]" 22)

;;************************************************************************
;; Additional tests for the Brick Procedure
;;************************************************************************

(testit "(type (setq Y (new Brick: x:Word:2 y:Object:2 z:Integer:5)))" Brick:)

(testit "(type (setq X (new Brick: Y)))" Brick:)

(testit "(type (setq R (new ObjectRepository: {TestMike.db} clear:)))" ObjectRepository:)

(testit "(setq X.z 22)[0][2][0]" 22)

(testit "X.z[0]" 22)

(testit "(type (setq R.test X))" ObjectRepository:)

(testit "X.z[0]" 22)

(testit "(type (setq X R.test))" Brick:)

(testit "X.z[0]" 22)

;;************************************************************************
;; Additional tests for the Brick Procedure
;;************************************************************************

(writeln scriptName "step [001] started")

(testit "(type (setq X (new Brick: 1 x:Word:2 y:Object:2 z:Integer:5)))" Brick:)

(testit {X["RowCount"]} 1)

(testit {X["RowByteCount"]} (if (= _ais.bits 64) 88 60))

(testit {X["FieldCount"]} 3)

(testit {X["ByteCount"]} (if (= _ais.bits 64) 88 60))

(testit "(setq X.z 22)[0][2][0]" 22)

(testit "X.z[0]" 22)

;;************************************************************************
;; Additional tests for the Brick Procedure
;;************************************************************************

(testit "(setq X[0 0] {Hello})[x:][0]" "Hello")

(testit "(setq X[0 1] Goodbye:)[0 1]" Goodbye:)

(testit "(setq X[1 0] #(1 2 3))[y: 0]" #(1 2 3))

(testit "(setq X[1 1] Mike:)[1 1]" Mike:)

(testit "(setq X[1 1] Mike:)[1 1 0]" Mike:)

(testit "(setq X[1 1 0] Mike:)[1 1 0]" Mike:)

(testit "(setq X[z: 3 0] 16)[z: 3 0]" 16)

;;************************************************************************
;; Additional tests for the Brick Procedure
;;************************************************************************

(testit "(length X)" 1)

(testit "(cdr (setCdr X 22))" 22)

(testit "(= X (setq Y (copy X)))" true)

(testit "(= X[...].x Word:)" true)

(testit "X[z: repeats:]" 5)

(testit "X[2 repeats:]" 5)

(testit {(insertRows X 0 1)["RowCount"]} 2)

(testit "X[1 1 1]" Mike:)

(testit {(deleteRows X 0 1)["RowCount"]} 1)

(testit "X[1 1 0]" Mike:)

;;************************************************************************
;; Fix this bug MFK
;;************************************************************************

(testit "(type (setq R (new ObjectRepository: {TestMike.db} clear:)))" ObjectRepository:)

(testit "(type (setq R.test X))" ObjectRepository:)

(testit "X.z[0]" 22)

(testit "(setq X R.test)[0][0][0]" "Hello")

(testit "X.z[0]" 22)

;;************************************************************************
;; Additional tests for the Brick Procedure
;;************************************************************************

(testit "(gc)" #void)

(testit "(type (setq R (new ObjectRepository: {TestMike.db} clear:)))" ObjectRepository:)
 
(testit "(type (setq R.test X))" ObjectRepository:)

(testit "X.z[0]" 22)

(testit "(setq X #void)" #void)

(testit "(gc)" #void)

(testit "X[1]" #void)

(testit "(setq X R.test)[0][0][0]" "Hello")

(testit "X.z[0]" 22)

(testit "X[0][1][0]" #(1 2 3))

;;************************************************************************
;; Additional tests for the Brick Procedure
;;************************************************************************

(writeln scriptName "step [002] started")

(testit "(type (setq X (new Brick: 10 x:Word:2 y:Object:2 z:Integer:5)))" Brick:)

(testit "(setq X[z: 2 1] 99)[z: 2 1]" 99)

(testit {X["RowCount"]} 10)

(testit {X["RowByteCount"]} (if (= _ais.bits 64) 88 60))

(testit {X["FieldCount"]} 3)

(testit {X["ByteCount"]} (if (= _ais.bits 64) 880 600))

(testit "(setq X.z 22)[0][2][0]" 22)

(testit "X.z[0]" 22)

;;************************************************************************
;; Additional tests for the Brick Procedure
;;************************************************************************

(testit "(setq X[0 1 9] {Hello})[x: 1 9]" "Hello")

(testit "(setq X[0 1 2] Goodbye:)[0 1 2]" Goodbye:)

(testit "(setq X[1 0 3] #(1 2 3))[y: 0 3]" #(1 2 3))

(testit "(setq X[1 1 1] Mike:)[1 1 1]" Mike:)

(testit "(setq X[1 1] Mike:)[1 1 0]" Mike:)

(testit "(setq X[1 1 0] Mike:)[1 1 0]" Mike:)

(testit "(setq X[z: 3 0] 16)[z: 3 0]" 16)

;;************************************************************************
;; Additional tests for the Brick Procedure
;;************************************************************************

(testit "(length X)" 10)

(testit "(cdr (setCdr X 22))" 22)

(testit "(= X (setq Y (copy X)))" true)

(testit "(= X[...].x Word:)" true)

(testit "X[z: repeats:]" 5)

(testit "X[2 repeats:]" 5)

(testit "X[y: offset:]" 32)

(testit "X[y: offset: 1]" (if (= _ais.bits 64) 120 92))

;;************************************************************************
;; Additional tests for the Brick Procedure
;;************************************************************************

(writeln scriptName "step [003] started")

(testit "(gc)" #void)

(testit "(type (setq R (new ObjectRepository: {TestMike.db} clear:)))" ObjectRepository:)
 
(testit "(setq X[z: 2 1] 99)[z: 2 1]" 99)

(testit "(setq X[1 0 3] #(4 5 6))[y: 0 3]" #(4 5 6))

(testit "(type (setq R.test X))" ObjectRepository:)

(testit "(setq X #void)" #void)

(testit "(gc)" #void)

(testit "X[1]" #void)

(testit "(setq X R.test)[z: 2 1]" 99)

(testit "X[y: 0 3]" #(4 5 6)) 

(testEnd "Test_Brick.sl")
