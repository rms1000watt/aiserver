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
;; *******************************************************************
;;  Title:    AIS Benchmark Test
;;
;;  Author:   Michael F. Korns, Tim May
;;
;;  Project:  AIS Regression Suite
;;	Notes:	  The speed of simple AIS Lisp Lambda execution
;;            loops, as well as the efficiency of the Lisp
;;            compiler is acceptance tested in this test script.
;;
;;
;; *******************************************************************

;#memory=500
;#memoryObjectHeaders=100
(runScript "RegTest.sl")
(testStart "Test_BenchMark.sl")

(define _DBPathName "Test.db")

;; *******************************************************************
;; name:     diagnostic
;; 
;; summary:  Displays the diagnostic error message.
;; *******************************************************************
(defun diagnostic(msg) ((ringBell) (error msg)))

;; *******************************************************************
;; name:     timingTest
;; 
;; summary:  Define a simple VmScript timing test procedure.
;; args:
;;           proc:      the procedure to be tested
;;           count:     the number of iterations to test
;; *******************************************************************
(define (timingTest proc count) vars:(startTime endTime)
    (setq startTime (getTickCount 0))
    (proc count) 
    (setq endTime (getTickCount startTime))
    endTime)

;; *******************************************************************
;; name:     timingRatio
;; 
;; summary:  Define a simple VmScript ratio timing test procedure.
;; note:     We use the minimum of four ratio tests because, on 
;;           multitasking machines, timings vary depending upon the
;;           number of demons executing. 
;; args:
;;           proc1:     the first procedure to be tested
;;           proc2:     the second procedure to be tested
;;           count:     the number of iterations to test
;; *******************************************************************
(define (timingRatio proc1 proc2 count) 
    vars:(startTime endTime i (n 0) (m 0))
    (loop for i from 0 to 3 do
       (setq startTime (getTickCount 0))
       (proc1 count) 
       (setq endTime (getTickCount startTime))
       (setq m (+ m endTime)))
    (loop for i from 0 to 3 do
       (setq startTime (getTickCount 0))
       (proc2 count) 
       (setq endTime (getTickCount startTime))
       (setq n (+ n endTime)))
    (if (= n 0) (writeln error "timing" "timingRatio got a divide by zero error."))
    (/ m n))
    
;; *******************************************************************
;; name:     readSourceFile
;; 
;; summary:  Perform a complete read of the specified source file.
;; *******************************************************************
(defun readSourceFile(name) 
    vars:(fileID self (type 0))
    (setq fileID (fileOpen name 0 type))
    (setq self (fileRead fileID))
    (fileClose fileID 1)
    self)

;; *******************************************************************
;; name:     writeSourceFile
;; 
;; summary:  Perform a complete write of the specified source file.
;; *******************************************************************
(defun writeSourceFile(name data) 
    vars:(fileID self (type 0))
    (setq fileID (fileOpen name 1 type))
    (setq self (fileWrite fileID data))
    (fileClose fileID 1)
    self)

;; *******************************************************************
;; name:     lispNilpotent
;; 
;; summary:  Define a simple Lisp nilpotent loop procedure.
;; *******************************************************************
(define (lispNilpotent n)
    vars:(i) 
    (loop for i from 1 to n do ))

;; *******************************************************************
;; name:     memoryInteger
;; 
;; summary:  Define a Lisp integer register loop procedure.
;; Notes:	 Uses strongly typed memory instructions (no registers)
;; *******************************************************************
(defun memoryInteger(i)
   vars:(m M n N x y)  
   vars:(v1 v2 I)  
   (setq N i)
   (setq M 1000)
   (setq v1 (new Vector: integer: M 0))
   (setq v2 (new Vector: integer: M 0))
   (setq y 10)
   (loop for n from 0 until N do
	   (loop for m from 0 until M do 
	     (setq x (refIntVector v1 m))
	     (setq x (iadd x 10))
	     (setq x (imul x x))
	     (setq x (idiv x 10))
	     (setIntVector v2 m  x)
	     ) ; end m loop
	   ) ; end n loop
    v2) ; end memoryInteger

;; *******************************************************************
;; name:     lispInteger
;; 
;; summary:  Define a Lisp integer register loop procedure.
;; *******************************************************************
(defun lispInteger(i)
   regs:(IntPointer:p1 IntPointer:p2 m M n N x y)  
   vars:(v1 v2 I)  
   (setq N i)
   (setq M 1000)
   (setq v1 (new Vector: integer: M 0))
   (setq v2 (new Vector: integer: M 0))
   (setq y 10)
   (loop for n from 0 until N do
       (setq p1 v1) 
       (setq p2 v2) 
	   (loop for m from 0 until M do
	     ;(writeln "lisInteger: " m) 
	     (setq x p1[m])
	     (+= x 10)
	     (*= x x)
	     (/= x 10)
	     (setq p2[m] x)
	     ) ; end m loop
	   ) ; end n loop
    v2) ; end lispInteger

;; *******************************************************************
;; name:     persistInteger
;; 
;; summary:  Define a Lisp integer register loop procedure.
;; Notes:	 Uses strongly typed memory instructions (no registers)
;; *******************************************************************
(defun persistInteger(i)
   pvars:(x y)  
   regs:(m M n N)  
   vars:(v1 v2 I)  
   (setq N i)
   (setq M 1000)
   (setq v1 (new Vector: M 0))
   (setq v2 (new Vector: M 0))
   (setq y 10)
   (loop for n from 0 until N do
	   (loop for m from 0 until M do 
	     (setq x (refVector v1 m))
	     (setq x (iadd x y))
	     (setq x (imul x x))
	     (setq x (idiv x 10))
	     (setVector v2 m x)
	     ) ; end m loop
	   ) ; end n loop
    v2) ; end persistInteger

;; *******************************************************************
;; name:     genericInteger
;; 
;; summary:  Define a Lisp integer generic loop procedure.
;; *******************************************************************
(defun genericInteger(i)
   vars:(m M n N)  
   vars:(x y)  
   vars:(v1 v2 I)  
   (setq N i)
   (setq M 1000)
   (setq v1 (new Vector: integer: M 10))
   (setq v2 (new Vector: integer: M 10))
   (setq y 10)
   (loop for n from 0 until N do
	   (loop for m from 0 until M do 
	     (setq x v1[m])
	     (+= x y)
	     (*= x x)
	     (/= x 10)
	     (setq v2[m] x)
	     ) ; end m loop
	   ) ; end n loop
    v2) ; end genericInteger

;; *******************************************************************
;; name:     assemblerInteger
;; 
;; summary:  Define an Assembler integer register loop procedure.
;; *******************************************************************
(defun assemblerInteger(i) 
   regs:(IntPointer:p1 IntPointer:p2 m M n N x y)  
   vars:(v1 v2 I)  
   (setq I 1000)
   (vmregLoadInteger I M)
   (setq v1 (new Vector: integer: I 0))
   (setq v2 (new Vector: integer: I 0))
   (vmregMoveImmediate 10 y)

   (vmregMoveImmediate 0 n)
   (vmregLoadInteger i N)

   LoopOutter::

	   (vmregObjPointer v1 p1)
	   (vmregObjPointer v2 p2)
	   (vmregMoveImmediate 0 m)
	
	   LoopInner::
	   (vmregRefXInteger p1 m x)
	   (vmregAddInteger y x)
	   (vmregMulInteger x x)
	   (vmregDivInteger y x)
	   (vmregSetXInteger x m p2)
	   (vmregIncrement 1 m)
	   (vmregJmpLTInteger m M LoopInner) 

   (vmregIncrement 1 n)
   (vmregJmpLTInteger n N LoopOutter) 

   v2) ; end assemblerInteger

;; *******************************************************************
;; name:     memoryReal
;; 
;; summary:  Define a Lisp real register loop procedure.
;; Notes:	 Uses strongly typed memory instructions (no registers)
;; *******************************************************************
(defun memoryReal(i)
   vars:(m M n N x y)  
   vars:(v1 v2 I)  
   (setq N i)
   (setq M 1000)
   (setq v1 (new Vector: number: M 0))
   (setq v2 (new Vector: number: M 0))
   (setq y 10.0)
   (loop for n from 0 until N do
	   (loop for m from 0 until M do 
	     (setq x (refNumVector v1 m))
	     (setq x (nadd x 10.0))
	     (setq x (nmul x x))
	     (setq x (ndiv x 10.0))
	     (setNumVector v2 m  x)
	     ) ; end m loop
	   ) ; end n loop
    v2) ; end memoryReal

;; *******************************************************************
;; name:     persistReal
;; 
;; summary:  Define a Lisp real persistent loop procedure.
;; Notes:	 Uses strongly typed memory instructions (no registers)
;; *******************************************************************
(defun persistReal(i)
   pvars:(x y)  
   regs:(m M n N)  
   vars:(v1 v2 I)  
   (setq N i)
   (setq M 1000)
   (setq v1 (new Vector: number: M))
   (setq v2 (new Vector: number: M))
   (setq y 10)
   (loop for n from 0 until N do
	   (loop for m from 0 until M do 
	     (setq x (refNumVector v1 m))
	     (setq x (nadd x y))
	     (setq x (nmul x x))
	     (setq x (ndiv x 10))
	     (setNumVector v2 m x)
	     ) ; end m loop
	   ) ; end n loop
    v2) ; end persistReal

;; *******************************************************************
;; name:     lispReal
;; 
;; summary:  Define a Lisp real register loop procedure.
;; *******************************************************************
(defun lispReal(i)
   regs:(NumPointer:p1 NumPointer:p2 m M n N Number:x Number:y)  
   vars:(v1 v2 I)  
   (setq N i)
   (setq M 1000)
   (setq v1 (new Vector: number: M))
   (setq v2 (new Vector: number: M))
   (setq y 10.0)
   (loop for n from 0 until N do
       (setq p1 v1) 
       (setq p2 v2) 
	   (loop for m from 0 until M do 
	     (setq x p1[m])
	     (+= x y)
	     (*= x x)
	     (/= x y)
	     (setq p2[m] x)
	     ) ; end m loop
	   ) ; end n loop
    v2) ; end lispReal

;; *******************************************************************
;; name:     genericReal
;; 
;; summary:  Define a Lisp real generic loop procedure.
;; *******************************************************************
(defun genericReal(i)
   vars:(m M n N)  
   vars:(x y)  
   vars:(v1 v2 I)  
   (setq N i)
   (setq M 1000)
   (setq v1 (new Vector: number: M 10.0))
   (setq v2 (new Vector: number: M 10.0))
   (setq y 10)
   (loop for n from 0 until N do
	   (loop for m from 0 until M do 
	     (setq x v1[m])
	     (+= x y)
	     (*= x x)
	     (/= x 10)
	     (setq v2[m] x)
	     ) ; end m loop
	   ) ; end n loop
    v2) ; end genericReal

;; *******************************************************************
;; name:     assemblerReal
;; 
;; summary:  Define an Assembler real register loop procedure.
;; *******************************************************************
(defun assemblerReal(i) 
   regs:(m M n N NumPointer:p1 NumPointer:p2 m M n N Number:x Number:y)
   vars:(v1 v2 I)  
   (setq I 1000)
   (vmregLoadInteger I M)
   (setq v1 (new Vector: number: I))
   (setq v2 (new Vector: number: I))
   (vmregLoadNumber 10.0 y)

   (vmregMoveImmediate 0 n)
   (vmregLoadInteger i N)

   LoopOutter::

	   (vmregObjPointer v1 p1)
	   (vmregObjPointer v2 p2)
	   (vmregMoveImmediate 0 m)
	
	   LoopInner::
	   (vmregRefXNumber p1 m x)
	   (vmregAddNumber y x)
	   (vmregMulNumber x x)
	   (vmregDivNumber y x)
	   (vmregSetXNumber x m p2)
	   (vmregIncrement 1 m)
	   (vmregJmpLTInteger m M LoopInner) 

   (vmregIncrement 1 n)
   (vmregJmpLTInteger n N LoopOutter) 

   v2) ; end assemblerReal

;; *******************************************************************
;; name:     lispString
;; 
;; summary:  Define a simple Lisp string loop procedure.
;; *******************************************************************
(define (lispString i)
    regs:(n N k z)
    vars:(sleft sright)
    (setq N i)
    (setq sleft (string |Hello there you all|:))
    (setq sright (string |Hello buddy|:))
    (setq z 0)
    (loop for n from 0 to N do
      (vmregStringCompare sleft sright k) 
      )
    i)

;; *******************************************************************
;; name:     genericString
;; 
;; summary:  Define a simple Lisp string loop procedure.
;; *******************************************************************
(define (genericString i)
    vars:(n N k z)
    vars:(sleft sright)
    (setq N i)
    (setq sleft (string |Hello there you all|:))
    (setq sright (string |Hello buddy|:))
    (loop for n from 0 to N do
      (if (= sleft sright) (setq k z) (setq k z))
      )
    i)

;; *******************************************************************
;; name:     lispCall
;; 
;; summary:  Define a simple vm call loop procedure.
;; *******************************************************************
(define (lispCall n)
    regs:(i N)
    vars:(x)
    (setq N n)
    (setq x +)
    (loop for i from 0 to N do (x 0.1 1.1)))

;; *******************************************************************
;; name:     lispLoop
;; 
;; summary:  Define a simple Lisp timing loop procedure.
;;  Note:    This loop includes an integer add, a real add,
;;           a string compare, and a function call.
;; *******************************************************************
(define (lispLoop n)
    regs:(i N m Number:y k)
    vars:(x sleft sright)
    (setq N n)
    (setq m 0)
    (setq x +)
    (setq sleft (string |Hello there you all|:))
    (setq sright (string |Hello buddy|:))
    (loop for i from 0 to N do
       (begin
           (+= m 1)                         ; Integer add
           (+= y 1.0)                       ; real add
           (if (= sleft sright) (setq k 1)) ; string compare
           (x 0.1 1.1))))                   ; procedure call

;; *******************************************************************
;; name:     lispDotProduct
;; 
;; summary:  Define a simple Lisp timing loop procedure.
;;  Note:    This loop uses the vitual machine vector instructions.
;; *******************************************************************
(define (lispDotProduct X Y)
    regs:(NumPointer:ap NumPointer:sp m M n N Number:y)
    vars:(x sleft sright)
    (setq ap X)
    (setq sp Y)
    (setq m (offset X 1))
    (vmregObjLength X N)
    (vmregObjLength Y M)
    (if (<> N M) (error "lispDotProduct: both vectors must have the same length"))
    (vmvecSetPointers ap sp sp)
    (vmvecSetIncrements m m m)
    (vmvecNumScalar dot: N y)
    y)

;; *******************************************************************
;; name:     lispDotLoop
;; 
;; summary:  Define a simple Lisp timing loop procedure.
;;  Note:    This loop uses the vitual machine vector instructions.
;; *******************************************************************
(define (lispDotLoop K)
    regs:(NumPointer:ap NumPointer:sp m M i K n N Number:y)
    vars:(X Y)
    (setq k K)
	(setq X (new Vector: number: 50))
	(setq Y (new Vector: number: 50))
	(loop for n from 1 until 50 do: (setq X[n] (/ 1.0 n)) (setq Y[n] (/ 2.0 n)))
    (vmregObjLength X N)
    (vmregObjLength Y M)
    (setq ap X)
    (setq sp Y)
    (setq m (offset X 1))
    (vmvecSetPointers ap sp sp)
    (vmvecSetIncrements m m m)
    (loop for i from 0 until k do (vmvecNumScalar dot: N y))
    y)

;; *******************************************************************
;; name:     cDotLoop
;; 
;; summary:  Define a simple Lisp timing loop procedure.
;;  Note:    This loop uses the vitual machine vector instructions.
;; *******************************************************************
(define (cDotLoop K)
    regs:(i k n N m M)
    vars:(X Y y)
    (setq k K)
	(setq X (new Vector: number: 1000))
	(setq Y (new Vector: number: 1000))
	(loop for n from 1 until 1000 do: (setq X[n] (/ 1.0 n)) (setq Y[n] (/ 2.0 n)))
    (loop for n from 0 until k do (setq y (vectorInnerProduct X Y)))
    y)

;; *******************************************************************
;; name:     lispSourceFile
;; 
;; summary:  Define a simple Lisp timing loop procedure.
;;  Note:    This loop uses the virtual machine vector instructions.
;; *******************************************************************
(define (lispSourceFile K)
    regs:(i k)
    vars:(X)
    (setq k K)
    (loop for i from 0 until k do 
       (setq X (readSourceFile "Test_BenchMark.sl"))
       (writeSourceFile "Test_BenchMark.txt" X)
       )
    k)

;; *******************************************************************
;; name:     lispRepository
;; 
;; summary:  Define a simple Lisp timing loop procedure.
;;  Note:    This loop uses the virtual machine vector instructions.
;; *******************************************************************
(define (lispRepository K)
    regs:(i k)
    vars:(X)
    (clear db)
    (setq k K)
    (loop for i from 0 until k do
       (setq db[i] lispInteger)
       (setq X db[i])
       )
    (X 1))

;; *****************************
;; Initialize Lambda Repository::
;; ***************************** 
(define db (new ObjectRepository: _DBPathName clear:))

;; *******************************************************************
;; Start timing tests
;; *******************************************************************
(gc)
(writeln "*******************************************")
(writeln "******Start AIS Benchmark Test Suite*******")
(writeln "*******************************************")
(writeln "")
(setq startTime (getTickCount 0))

(display "******Start timing test for Integer Register Lisp*******")
(lispInteger 1) ;; Make sure we run the JIT before we try timing.
(writeln " = [" (timingTest lispInteger 200000) " seconds]") 

(display "******Start timing test for Integer Generic Lisp*******")
(genericInteger 1) ;; Make sure we run the JIT before we try timing.
(writeln " = [" (timingTest genericInteger 20000) " seconds]") 

(display "******Start timing test for Real Register Lisp*******")
(lispInteger 1) ;; Make sure we run the JIT before we try timing.
(writeln " = [" (timingTest lispReal 100000) " seconds]") 

(display "******Start timing test for Real Generic Lisp*******")
(genericInteger 1) ;; Make sure we run the JIT before we try timing.
(writeln " = [" (timingTest genericReal 20000) " seconds]") 

(display "******Start timing test for Real Dot Product Lisp*******")
(lispDotLoop 1) ;; Make sure we run the JIT before we try timing.
(writeln " = [" (timingTest lispDotLoop 40000000) " seconds]") 

(display "******Start timing test for String Register Compare*******")
(lispString 1) ;; Make sure we run the JIT before we try timing.
(writeln " = [" (timingTest lispString 400000000) " seconds]") 

(display "******Start timing test for String Generic Compare*******")
(genericString 1) ;; Make sure we run the JIT before we try timing.
(writeln " = [" (timingTest genericString 70000000) " seconds]") 

(display "******Start timing test for Lisp Call*******")
(lispCall 1) ;; Make sure we run the JIT before we try timing.
(writeln " = [" (timingTest lispCall 20000000) " seconds]") 

(display "******Start timing test for Lisp Heterogenious Loop*******")
(lispLoop 1) ;; Make sure we run the JIT before we try timing.
(writeln " = [" (timingTest lispLoop 17000000) " seconds]") 

(display "******Start timing test for Lisp Source File Loop*******")
(lispSourceFile 1) ;; Make sure we run the JIT before we try timing.
(writeln " = [" (timingTest lispSourceFile 9000) " seconds]") 

(display "******Start timing test for Lisp Repository Loop*******")
(lispRepository 1) ;; Make sure we run the JIT before we try timing.
(writeln " = [" (timingTest lispRepository 3000) " seconds]")

(system "del Test.db") 
(system "del Test_BenchMark.txt") 

;;  Notify user of completion
(writeln "")
(writeln "AIS Benchmark completed in [" (setq endTime (getTickCount startTime)) "] seconds.")


(testEnd "Test_BenchMark.sl")









