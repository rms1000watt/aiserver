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
;;  Title:    Lisp Timing Test
;;
;;  Author:   Michael F. Korns, Tim May
;;
;;  Project:  AIS Regression Suite
;;  Notes:	  The speed of simple native Lisp execution
;;            loops, as well as the efficiency of the Lisp
;;            compiler is acceptance tested in this test script.
;;
;;  Dependencies: RegTest.sl
;; *******************************************************************

;#memory=500
;#memoryObjectHeaders=100
(runScript "RegTest.sl")
(testStart "Test_LispTime.sl")

;; *******************************************************************
;; name:     timingTest
;; 
;; summary:  Define a simple VmScript timing test procedure.
;; args:
;;           proc:      the procedure to be tested
;;           count:     the number of iterations to test
;; *******************************************************************
(define (timingTest proc count) 
    vars:(startTime endTime)
    (setq startTime (getTickCount 0))
    (proc count) 
    (setq endTime (getTickCount startTime))
    (writeln "Elapsed time is " endTime " Seconds" ) endTime)

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
    (text (/ m n) "####.0000"))
    
;; *******************************************************************
;; name:     lispGenericVector
;; 
;; summary:  Timing utility to test the speed of a simple loop 
;;           transfering data between three Integer Vectors 
;;           passed as arguments.
;;
;; args:	vA		Argument Vector.
;;			v1		Source Vector.
;;			v2		Destination Vector.
;;
;; return:	v2		Returns the destination vector 
;;
;; example:	(lispGenericVector (new Vector: Integer: 10000 1 -2 3 -4 5 -3 10 -20 3 8) 
;;			                   (new Vector: Integer: 10000 1 2 3 4 5 6 7 8 9 10) 
;;							   (new Vector: Integer: 10000 1 2 3 4 5 6 7 8 9 10))
;;
;; example:	(lispGenericVector (new Vector: Number: 10000 1.1 -2.2 3.7 -4.6 5.3 -3.8 10.3 -20.2 3.2 8.1) 
;;			                   (new Vector: Number: 10000 1.1 2.4 3.8 4.3 5.6 6.9 7.1 8.7 9.1 10.3) 
;;							   (new Vector: Number: 10000 1.1 2.4 3.8 4.3 5.6 6.9 7.1 8.7 9.1 10.3))
;;
;; Notes:	[1]This timing function always destructively returns the destination vector with
;;			   the alterations which have been made.
;;			[2]The calling Lisp Lambda should create large vectors in order to have the timings 
;;			   be as representative as possible.
;;
;; authors:         Michael F. Korns
;;
;; *******************************************************************
(defun lispGenericVector(vA v1 v2)
   regs:(m M1 M2 n N)  
   vars:(x A)  
   (setq N (length vA))
   (setq M1 (length v1))
   (setq M2 (length v2))
   (if (<> M1 M2) (error "lispGenericVector: invalid vector lengths"))
   (loop for n from 0 until N do
	   (setq A vA[n])
	   (loop for m from 0 until M1 do
	     (setq x v1[m])
	     (+= x A)
	     (*= x x)
	     (/= x A)
	     (setq v2[m] x)
	     ) ; end m loop
	   ) ; end n loop
    v2) ; end lispGenericVector

;; *******************************************************************
;; name:     lispIntegerVector
;; 
;; summary:  Timing utility to test the speed of a simple loop 
;;           transfering data between three Integer Vectors 
;;           passed as arguments.
;;
;; args:	vA		Argument Integer Vector.
;;			v1		Source Integer Vector.
;;			v2		Destination Integer Vector.
;;
;; return:	v2		Returns the destination vector 
;;
;; example:	(lispIntegerVector (new Vector: Integer: 10000 1 -2 3 -4 5 -3 10 -20 3 8) 
;;			                   (new Vector: Integer: 10000 1 2 3 4 5 6 7 8 9 10) 
;;							   (new Vector: Integer: 10000 1 2 3 4 5 6 7 8 9 10))
;;
;; Notes:	[1]This timing function always destructively returns the destination vector with
;;			   the alterations which have been made.
;;			[2]The calling Lisp Lambda should create large vectors in order to have the timings 
;;			   be as representative as possible.
;;
;; authors:         Michael F. Korns
;;
;; *******************************************************************
(defun lispIntegerVector(IntVector:vA IntVector:v1 IntVector:v2)
   regs:((IntPointer:pvA) (IntPointer:pv1) (IntPointer:pv2) m M1 M2 n N x A)  
   (setq pvA vA)
   (setq pv1 v1)
   (setq pv2 v2)
   (setq N (length vA))
   (setq M1 (length v1))
   (setq M2 (length v2))
   (if (<> M1 M2) (error "lispIntegerVector: invalid vector lengths"))
   (loop for n from 0 until N do
	   (setq A pvA[n])
	   (loop for m from 0 until M1 do
	     (setq x pv1[m])
	     (+= x A)
	     (*= x x)
	     (/= x A)
	     (setq pv2[m] x)
	     ) ; end m loop
	   ) ; end n loop
    v2) ; end lispIntegerVector

;; *******************************************************************
;; name:     lispNumberVector
;; 
;; summary:  Timing utility to test the speed of a simple loop 
;;           transfering data between three Integer Vectors 
;;           passed as arguments.
;;
;; args:	vA		Argument Integer Vector.
;;			v1		Source Integer Vector.
;;			v2		Destination Integer Vector.
;;
;; return:	v2		Returns the destination vector 
;;
;; example:	(lispNumberVector  (new Vector: Number: 10000 1.1 -2.2 3.7 -4.6 5.3 -3.8 10.3 -20.2 3.2 8.1) 
;;			                   (new Vector: Number: 10000 1.1 2.4 3.8 4.3 5.6 6.9 7.1 8.7 9.1 10.3) 
;;							   (new Vector: Number: 10000 1.1 2.4 3.8 4.3 5.6 6.9 7.1 8.7 9.1 10.3))
;;
;; Notes:	[1]This timing function always destructively returns the destination vector with
;;			   the alterations which have been made.
;;			[2]The calling Lisp Lambda should create large vectors in order to have the timings 
;;			   be as representative as possible.
;;
;; authors:         Michael F. Korns
;;
;; *******************************************************************
(defun lispNumberVector(NumVector:vA NumVector:v1 NumVector:v2)
   regs:((NumPointer:pvA) (NumPointer:pv1) (NumPointer:pv2) m M1 M2 n N Number:x Number:A)  
   (setq pvA vA)
   (setq pv1 v1)
   (setq pv2 v2)
   (setq N (length vA))
   (setq M1 (length v1))
   (setq M2 (length v2))
   (if (<> M1 M2) (error "lispNumberVector: invalid vector lengths"))
   (loop for n from 0 until N do
	   (setq A pvA[n])
	   (loop for m from 0 until M1 do
	     (setq x pv1[m])
	     (+= x A)
	     (*= x x)
	     (/= x A)
	     (setq pv2[m] x)
	     ) ; end m loop
	   ) ; end n loop
    v2) ; end lispNumberVector

;; *******************************************************************
;; name:     lispStringVector
;; 
;; summary:  Timing utility to test the speed of a simple loop 
;;           transfering result codes from String comparisons 
;;           between two Vectors to a third Integer Vector.
;;
;; args:	v1		Argument Vector.
;;			v2		Source Vector.
;;			v3		Destination Integer Vector.
;;
;; return:	v3		Returns the destination vector 
;;
;; example:	(lispIntegerVector (new Vector: 10000 "Hello" "Test" "xyz") 
;;			                   (new Vector: 10000 "Hello1" "Test1" "xyz1") 
;;							   (new Vector: Integer: 10000 0 1 0 ))
;;
;; Notes:	[1]This timing function always destructively returns the destination vector with
;;			   the alterations which have been made.
;;			[2]The calling Lisp Lambda should create large vectors in order to have the timings 
;;			   be as representative as possible.
;;
;; authors:         Michael F. Korns
;;
;; *******************************************************************
(defun lispStringVector(Vector:v1 Vector:v2 IntVector:v3)
   regs:(m M1 M2 n N result)  
   regs:((WordPointer:pv1) (WordPointer:pv2) (IntPointer:pv3))  
   vars:(left right)  
   (setq pv1 v1)
   (setq pv2 v2)
   (setq pv3 v3)
   (setq N (length v1))
   (setq M1 (length v2))
   (setq M2 (length v3))
   (if (or (<> M1 M2) (<> N M1)) (error "lispStringVector: invalid vector lengths"))
   (loop for n from 0 until N do
	   (setq pv1 v1)(vmregAddPointer WordPointer: n pv1)
	   (setq pv2 v2)(vmregAddPointer WordPointer: n pv2)
       (vmregStringCompare pv1[:0:] pv2[:0:] result)
       (setq pv3[n] result)
	   ) ; end n loop
    v3) ; end lispStringVector

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
(defun memoryInteger(Integer:i)
   vars:(Integer:m Integer:M Integer:n Integer:N Integer:x Integer:y)  
   vars:(IntVector:v1 IntVector:v2)  
   (setq N i)
   (setq M 1000)
   (setq v1 (new Vector: Integer: M 0))
   (setq v2 (new Vector: Integer: M 0))
   (setq y 10)
   (loop for n from 0 until N do
	   (loop for m from 0 until M do 
	     (setq x v1[m])
	     (+= x y)
	     (*= x x)
	     (/= x y)
	     (setq v2[m] x)
	     ) ; end m loop
	   ) ; end n lop
    v2) ; end memoryInteger

;; *******************************************************************
;; name:     normalInteger
;; 
;; summary:  Define a Lisp integer register loop procedure.
;; Notes:	 Uses strongly typed instructions (normal coding practice)
;; *******************************************************************
(defun normalInteger(Integer:i)
   regs:(m M n N x y)  
   vars:(IntVector:v1 IntVector:v2)  
   (setq N i)
   (setq M 1000)
   (setq v1 (new Vector: Integer: M 0))
   (setq v2 (new Vector: Integer: M 0))
   (setq y 10)
   (loop for n from 0 until N do
	   (loop for m from 0 until M do 
	     (setq x v1[m])
	     (+= x y)
	     (*= x x)
	     (/= x y)
	     (setq v2[m] x)
	     ) ; end m loop
	   ) ; end n loop
    v2) ; end normalInteger

;; *******************************************************************
;; name:     registerInteger
;; 
;; summary:  Define a Lisp integer register loop procedure.
;; *******************************************************************
(defun registerInteger(i)
   regs:((IntPointer:p1) (IntPointer:p2) m M n N x y)  
   vars:(IntVector:v1 IntVector:v2 (slots 1000))  
   (setq v1 (new Vector: Integer: slots 0))
   (setq v2 (new Vector: Integer: slots 0))
   (setq N i)
   (setq M slots)
   (setq y 10)
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
    v2) ; end registerInteger

;; *******************************************************************
;; name:     persistInteger
;; 
;; summary:  Define a Lisp integer register loop procedure.
;; Notes:	 Uses strongly typed memory instructions (no registers)
;; *******************************************************************
(defun persistInteger(Integer:i)
   pvars:(Integer:x Integer:y)  
   vars:(Integer:m Integer:M Integer:n Integer:N)  
   vars:(IntVector:v1 IntVector:v2 Integer:I)  
   (setq N i)
   (setq M 1000)
   (setq v1 (new Vector: M 0))
   (setq v2 (new Vector: M 0))
   (setq y 10)
   (loop for n from 0 until N do
	   (loop for m from 0 until M do 
	     (setq x v1[m])
	     (+= x y)
	     (*= x x)
	     (/= x y)
	     (setq v2[m] x)
	     ) ; end m loop
	   ) ; end n loop
    v2) ; end persistInteger

;; *******************************************************************
;; name:     lispIntegerVector
;; 
;; summary:  Timing utility to test the speed of a simple loop 
;;           transfering data between three Integer Vectors 
;;           passed as arguments.
;;
;; args:	vA		Argument Integer Vector.
;;			v1		Source Integer Vector.
;;			v2		Destination Integer Vector.
;;
;; return:	v2		Returns the destination vector 
;;
;; example:	(cMemoryInteger (new Vector: Integer: 10 1 -2 3 -4 5 -3 10 -20 3 8) 
;;			                (new Vector: Integer: 10 1 2 3 4 5 6 7 8 9 10) 
;;							(new Vector: Integer: 10 1 2 3 4 5 6 7 8 9 10))
;;
;; Notes:	[1]This timing function always destructively returns the destination vector with
;;			   the alterations which have been made.
;;			[2]The calling Lisp Lambda should create large vectors in order to have the timings 
;;			   be as representative as possible.
;;
;; authors:         Michael F. Korns
;;
;; *******************************************************************
(defun lispIntegerVector(IntVector:vA IntVector:v1 IntVector:v2)
   regs:((IntPointer:pvA) (IntPointer:pv1) (IntPointer:pv2) m M1 M2 n N x A)  
   (setq pvA vA)
   (setq pv1 v1)
   (setq pv2 v2)
   (setq N (length vA))
   (setq M1 (length v1))
   (setq M2 (length v2))
   (if (<> M1 M2) (error "registerIntegerNew: invalid vector lengths"))
   (loop for n from 0 until N do
	   (setq A pvA[n])
	   (loop for m from 0 until M1 do
	     (setq x pv1[m])
	     (+= x A)
	     (*= x x)
	     (/= x A)
	     (setq pv2[m] x)
	     ) ; end m loop
	   ) ; end n loop
    v2) ; end lispIntegerVector

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
   (setq v1 (new Vector: Integer: M 10))
   (setq v2 (new Vector: Integer: M 10))
   (setq y 10)
   (loop for n from 0 until N do
	   (loop for m from 0 until M do 
	     (setq x v1[m])
	     (+= x y)
	     (*= x x)
	     (/= x y)
	     (setq v2[m] x)
	     ) ; end m loop
	   ) ; end n loop
    v2) ; end genericInteger

;; *******************************************************************
;; name:     assemblerInteger
;; 
;; summary:  Define an Assembler integer register loop procedure.
;; *******************************************************************
(defun assemblerInteger(Integer:i) 
   regs:(IntPointer:p1 IntPointer:p2 m M n N x y)  
   vars:(IntVector:v1 IntVector:v2 I)  
   (setq I 1000)
   (vmregLoadInteger I M)
   (setq v1 (new Vector: Integer: I 0))
   (setq v2 (new Vector: Integer: I 0))
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
(defun memoryReal(Integer:i)
   vars:(Integer:m Integer:M Integer:n Integer:N Number:x Number:y)  
   vars:(NumVector:v1 NumVector:v2 Integer:I)  
   (setq N i)
   (setq M 1000)
   (setq v1 (new Vector: Number: M 0))
   (setq v2 (new Vector: Number: M 0))
   (setq y 10.0)
   (loop for n from 0 until N do
	   (loop for m from 0 until M do 
	     (setq x v1[m])
	     (+= x y)
	     (*= x x)
	     (/= x y)
	     (setq v2[m] x)
	     ) ; end m loop
	   ) ; end n loop
    v2) ; end memoryReal

;; *******************************************************************
;; name:     normalReal
;; 
;; summary:  Define a Lisp real register loop procedure.
;; Notes:	 Uses strongly typed memory instructions (normal coding practice)
;; *******************************************************************
(defun normalReal(Integer:i)
   regs:(m M n N Number:x Number:y)  
   vars:(NumVector:v1 NumVector:v2)  
   (setq N i)
   (setq M 1000)
   (setq v1 (new Vector: Number: M 0))
   (setq v2 (new Vector: Number: M 0))
   (setq y 10.0)
   (loop for n from 0 until N do
	   (loop for m from 0 until M do 
	     (setq x v1[m])
	     (+= x y)
	     (*= x x)
	     (/= x y)
	     (setq v2[m] x)
	     ) ; end m loop
	   ) ; end n loop
    v2) ; end normalReal

;; *******************************************************************
;; name:     persistReal
;; 
;; summary:  Define a Lisp real persistent loop procedure.
;; Notes:	 Uses strongly typed memory instructions (no registers)
;; *******************************************************************
(defun persistReal(Integer:i)
   pvars:(Number:x Number:y)  
   vars:(Integer:m Integer:M Integer:n Integer:N Number:x Number:y)  
   vars:(NumVector:v1 NumVector:v2 Integer:I)  
   (setq N i)
   (setq M 1000)
   (setq v1 (new Vector: Number: M 0))
   (setq v2 (new Vector: Number: M 0))
   (setq y 10.0)
   (loop for n from 0 until N do
	   (loop for m from 0 until M do 
	     (setq x v1[m])
	     (+= x y)
	     (*= x x)
	     (/= x y)
	     (setq v2[m] x)
	     ) ; end m loop
	   ) ; end n loop
    v2) ; end persistReal

;; *******************************************************************
;; name:     registerReal
;; 
;; summary:  Define a Lisp real register loop procedure.
;; *******************************************************************
(defun registerReal(Integer:i)
   regs:(Number:x Number:y)  
   regs:((NumPointer:p1) (NumPointer:p2) m M n N)  
   vars:(NumVector:v1 NumVector:v2 I (slots 1000))  
   (setq v1 (new Vector: Number: slots))
   (setq v2 (new Vector: Number: slots))
   (setq N i)
   (setq M slots)
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
    v2) ; end registerReal

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
   (setq v1 (new Vector: Number: M 10.0))
   (setq v2 (new Vector: Number: M 10.0))
   (setq y 10.0)
   (loop for n from 0 until N do
	   (loop for m from 0 until M do 
	     (setq x v1[m])
	     (+= x y)
	     (*= x x)
	     (/= x y)
	     (setq v2[m] x)
	     ) ; end m loop
	   ) ; end n loop
    v2) ; end genericReal

;; *******************************************************************
;; name:     assemblerReal
;; 
;; summary:  Define an Assembler real register loop procedure.
;; *******************************************************************
(defun assemblerReal(Integer:i) 
   regs:(m M n N Number:x Number:y NumPointer:p1 NumPointer:p2)  
   vars:(NumVector:v1 NumVector:v2)  
   (setq M 1000)
   (setq v1 (new Vector: Number: M))
   (setq v2 (new Vector: Number: M))
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
;; name:     lispGenericString
;; 
;; summary:  Define a simple Lisp string loop procedure.
;; *******************************************************************
(define (lispGenericString i)
    regs:(n N k z)
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
;; name:     assemblerDotLoop
;; 
;; summary:  Define an Assembler dot product register loop procedure.
;;  Note:    This loop uses the vitual machine vector instructions.
;; *******************************************************************
(defun assemblerDotLoop(i) 
   regs:(NumPointer:p1 NumPointer:p2 
         m M n N 
         Number:x Number:y (Number:sum 0.0) 
         )  
   vars:(v1 v2 w)  
   (setq M 50)
   (setq v1 (new Vector: Number: M))
   (setq v2 (new Vector: Number: M))
   (loop for n from 1 until M do: (setq v1[n] (/ 1.0 n)) (setq v2[n] (/ 2.0 n)))

   (vmvecSetPointers p1 p2 p2)
   (vmvecSetIncrements m m m)

   (setq p1 v1)
   (setq p2 v2)
   (setq m (offset v1 1))
   (setq n 0)
   (setq N i)
    
   (loop for n from 0 until N do   
	   (vmadd w w w) 
	   (vmvecPush Number: zero:)
	   (vmvecInitialize source: M)
	   (vmvecPush Number: argument:)
	   (vmvecPush Number: source:)
	   (vmvecBinary mul:)
	   (vmvecBinary add:)
	   (vmvecLoop)
	   (vmvecPopNumber sum)
	   ) ; end test loop

   sum) ; end assemblerDotLoop

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
	(setq X (new Vector: Number: 50))
	(setq Y (new Vector: Number: 50))
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
	(setq X (new Vector: Number: 50))
	(setq Y (new Vector: Number: 50))
	(loop for n from 1 until 50 do: (setq X[n] (/ 1.0 n)) (setq Y[n] (/ 2.0 n)))
    (loop for n from 0 until k do (setq y (vectorInnerProduct X Y)))
    y)

;; *******************************************************************
;; name:     lispDistance
;; 
;; summary:  Define a simple Lisp timing loop procedure.
;;  Note:    This loop uses the vitual machine vector instructions.
;; *******************************************************************
(define (lispDistance X Y)
    regs:(NumPointer:ap NumPointer:sp m M n N Number:y)
    vars:(x sleft sright)
    (setq ap X)
    (setq sp Y)
    (setq m (offset X 1))
    (vmregObjLength X N)
    (vmregObjLength Y M)
    (if (<> N M) (error "lispDistance: both vectors must have the same length"))
    (vmvecSetPointers ap sp sp)
    (vmvecSetIncrements m m m)
    (vmvecNumScalar dis: N y)
    y)

;; *******************************************************************
;; name:     lispDisLoop
;; 
;; summary:  Define a simple Lisp timing loop procedure.
;;  Note:    This loop uses the vitual machine vector instructions.
;; *******************************************************************
(define (lispDisLoop K)
    regs:(NumPointer:ap NumPointer:sp m M i K n N Number:y)
    vars:(X Y)
    (setq k K)
	(setq X (new Vector: Number: 50))
	(setq Y (new Vector: Number: 50))
	(loop for n from 1 until 50 do: (setq X[n] (/ 1.0 n)) (setq Y[n] (/ 2.0 n)))
    (vmregObjLength X N)
    (vmregObjLength Y M)
    (setq ap X)
    (setq sp Y)
    (setq m (offset X 1))
    (vmvecSetPointers ap sp sp)
    (vmvecSetIncrements m m m)
    (loop for i from 0 until k do (vmvecNumScalar dis: N y))
    y)

;; *******************************************************************
;; name:     cDisLoop
;; 
;; summary:  Define a simple Lisp timing loop procedure.
;;  Note:    This loop uses the vitual machine vector instructions.
;; *******************************************************************
(define (cDisLoop K)
    regs:(i k n N m M)
    vars:(X Y y)
    (setq k K)
	(setq X (new Vector: Number: 50))
	(setq Y (new Vector: Number: 50))
	(loop for n from 1 until 50 do: (setq X[n] (/ 1.0 n)) (setq Y[n] (/ 2.0 n)))
    (loop for n from 0 until k do (setq y (vectorDistance X Y)))
    y)

;; *******************************************************************
;; name:     cSort
;; 
;; summary:  Define a simple Lisp timing loop procedure.
;;  Note:    This loop uses the vitual machine vector instructions.
;; *******************************************************************
(define (cSort K)
    regs:(NumPointer:ap m M i K n N Number:y)
    vars:(X Y)
    (setq k K)
    (setq M 10000)
	(setq X (new Vector: Number: M))
	(loop for n from 0 until M do: (setq X[n] (- M n)))
    (loop for i from 0 until k do (sort X <))
    X)

;; *******************************************************************
;; name:     lispSort
;; 
;; summary:  Define a simple Lisp timing loop procedure.
;;  Note:    This loop uses the vitual machine vector instructions.
;; *******************************************************************
(define (lispSort K)
    regs:(NumPointer:ap m M i K n N Number:y)
    vars:(X Y)
    (setq k K)
    (setq M 10000)
	(setq X (new Vector: Number: M))
	(loop for n from 0 until M do: (setq X[n] (- M n)))
    (loop for i from 0 until k do (bubbleSort X))
    X)

;; *******************************************************************
;; name:     bubbleSort
;; 
;; summary:  Define a simple Lisp timing loop procedure.
;;  Note:    This loop uses the vitual machine vector instructions.
;; *******************************************************************
(defun bubbleSort(NumVector:V)

   regs:(NumPointer:pv Number:v i n N) ;; Declare register variables for speed.

   (vmregObjPointer V pv) ;; Set a pointer to the vector in pv.

   (vmregObjLength V N) ;; Set the length of the vector in N.

   (setq i (offset V 1)) ;; Set the byte size of an Integer in i.

   (vmvecSetPointers pv pv pv) ;; Declare the Vector Pointer Registers.

   (vmvecSetIncrements i i i) ;; Declare the Vector Increment Registers.

   (loop for n from N until 0 by -1 do ;; Loop through each Integer in the vector.

      (vmregRefNumber pv v) ;; Load the first integer in the bubble into a temporary register.

      (vmvecPushNumber v) ;; Load the first integer in the bubble.

      (vmvecInitialize argument: n) ;; Start looping through each integer in the bubble.

      (vmvecPush Number: argument:) ;; Load the next integer in the bubble.

      (vmvecSwapCC lt:) ;; Leave the larger number on top of the stack.

      (vmvecPop Number: argument:) ;; Save the larger number.

      (vmvecLoop) ;; End the Vector data loop.

      (vmvecPopNumber v) ;; Save the minimum number in a temporary register.

      (vmregSetNumber v pv) ;; Save the minimum number in the first position of this bubble.

      (vmregAddInteger i pv) ;; Promote the vector pointer to the next bubble.

      ) ;; end n loop.

   V) ;; Return the sorted vector.

;; *************************************
;; Virtual Machine Vector Instructions::
;; ************************************* 


;; *******************************************************************
;; Start timing tests
;; *******************************************************************
(setq totalStartTime (getTickCount 0))
(gc)      

(writeln "****** Numeric, Vector, and Pointer Processing: ")

(display "****** For Integer Instructions ... ")
(setq outterLoop 5000)
(setq innerLoop 10000)
(setq startTime (getTickCount 0))
(setq v1 (lispIntegerVector (new Vector: Integer: outterLoop 1 -2 3 -4 5 -3 10 -20 3 8) (new Vector: Integer: innerLoop 1 2 3 4 5 6 7 8 9 10) (new Vector: Integer: innerLoop 1 2 3 4 5 6 7 8 9 10)))
(setq endLTime (getTickCount startTime))
(setq startTime (getTickCount 0))
(setq v2 (cIntegerVector (new Vector: Integer: outterLoop 1 -2 3 -4 5 -3 10 -20 3 8) (new Vector: Integer: innerLoop 1 2 3 4 5 6 7 8 9 10) (new Vector: Integer: innerLoop 1 2 3 4 5 6 7 8 9 10)))
(setq endCTime (getTickCount startTime))
(if (<> v1 v2) (error "!ERROR: Lisp result vector does not match C result vector!"))
(writeln "Lisp(Register) is " (text (/ endCTime endLTime) "###.0000") " times as fast as C(Optimized) Lisp[" endLTime "], C[" endCTime "] ******* ") 

(display "****** For Number Instructions ... ")
(setq outterLoop 5000)
(setq innerLoop 10000)
(setq startTime (getTickCount 0))
(setq v1 (lispNumberVector  (new Vector: Number: outterLoop 1.1 -2.2 3.7 -4.6 5.3 -3.8 10.3 -20.2 3.2 8.1) (new Vector: Number: innerLoop 1.1 2.4 3.8 4.3 5.6 6.9 7.1 8.7 9.1 10.3) (new Vector: Number: innerLoop 1.1 2.4 3.8 4.3 5.6 6.9 7.1 8.7 9.1 10.3)))
(setq endLTime (getTickCount startTime))
(setq startTime (getTickCount 0))
(setq v2 (cNumberVector  (new Vector: Number: outterLoop 1.1 -2.2 3.7 -4.6 5.3 -3.8 10.3 -20.2 3.2 8.1) (new Vector: Number: innerLoop 1.1 2.4 3.8 4.3 5.6 6.9 7.1 8.7 9.1 10.3) (new Vector: Number: innerLoop 1.1 2.4 3.8 4.3 5.6 6.9 7.1 8.7 9.1 10.3)))
(setq endCTime (getTickCount startTime))
(if (<> v1 v2) (error "!ERROR: Lisp result vector does not match C result vector!"))
(writeln "Lisp(Register) is " (text (/ endCTime endLTime) "###.0000") " times as fast as C(Optimized) Lisp[" endLTime "], C[" endCTime "] ******* ") 

(display "****** For Number Dot Product Instructions ... ")
(lispDotLoop 1) ;; Make sure we run the JIT before we try timing.
(writeln "Lisp(Register) is " (timingRatio cDotLoop lispDotLoop 400000) " times as fast as C(Optimized) ******* ") 

(display "****** For Real Vector Distance Instructions ... ")
(lispDisLoop 1) ;; Make sure we run the JIT before we try timing.
(writeln "Lisp(Register) is " (timingRatio cDisLoop lispDisLoop 400000) " times as fast as C(Optimized) ******* ") 

(writeln "****** General Processing: ")

(display "****** For String Instructions ... ")
(setq outterLoop 100)
(setq N 1000000)
(setq v1 (new Vector: N))
(setq v2(new Vector: N))
(setq vL3 (new Vector: Integer: N))
(setq vC3 (new Vector: Integer: N))
(loop for n from 0 until N do (setq v1[n] "Testing my system") (setq v2[n] "Testing my systex"))
(setq startTime (getTickCount 0))
(loop for n from 0 until outterLoop do (setq vL3 (lispStringVector v1 v2 vL3)))
(setq endLTime (getTickCount startTime))
(setq startTime (getTickCount 0))
(loop for n from 0 until outterLoop do (setq vC3 (cStringVector v1 v2 vL3)))
(setq endCTime (getTickCount startTime))
(if (<> vL3 vC3) (error "!ERROR: Lisp result vector does not match C result vector!"))
(writeln "Lisp(Register) is " (text (/ endCTime endLTime) "###.0000") " times as fast as C(Optimized) Lisp[" endLTime "], C[" endCTime "] ******* ") 

(display "****** For Function Call Instructions ... ")
(lispCall 1) ;; Make sure we run the JIT before we try timing.
(writeln "Lisp(Register) is " (timingRatio cCall lispCall 20000000) " times as fast as C(Optimized) ******* ") 

(writeln "****** Generic Prototyping: ")

(display "****** For Integer Instructions ... ")
(genericInteger 1) ;; Make sure we run the JIT before we try timing.
(writeln "Lisp(Generic) is " (timingRatio cGenericInteger genericInteger 1000) " times as fast as C(Generic) ******* ") 

(display "****** For Number Instructions ... ")
(genericReal 1) ;; Make sure we run the JIT before we try timing.
(writeln "Lisp(Generic) is " (timingRatio cGenericReal genericReal 1000) " times as fast as C(Generic) ******* ") 

(display "****** For Integer Instructions ... ")
(setq outterLoop 5000)
(setq innerLoop 10000)
(setq startTime (getTickCount 0))
(setq v1 (lispGenericVector (new Vector: Integer: outterLoop 1 -2 3 -4 5 -3 10 -20 3 8) (new Vector: Integer: innerLoop 1 2 3 4 5 6 7 8 9 10) (new Vector: Integer: innerLoop 1 2 3 4 5 6 7 8 9 10)))
(setq endGTime (getTickCount startTime))
(setq startTime (getTickCount 0))
(setq v2 (lispIntegerVector (new Vector: Integer: outterLoop 1 -2 3 -4 5 -3 10 -20 3 8) (new Vector: Integer: innerLoop 1 2 3 4 5 6 7 8 9 10) (new Vector: Integer: innerLoop 1 2 3 4 5 6 7 8 9 10)))
(setq endLTime (getTickCount startTime))
(if (<> v1 v2) (error "!ERROR: Lisp generic result vector does not match Lisp integer result vector!"))
(writeln "Lisp(Register) is " (text (/ endGTime endLTime) "###.0000") " times as fast as Lisp(Generic) Lisp[" endLTime "], Generic[" endGTime "] ******* ") 

(display "****** For Number Instructions ... ")
(setq outterLoop 5000)
(setq innerLoop 10000)
(setq startTime (getTickCount 0))
(setq v1 (lispGenericVector  (new Vector: Number: outterLoop 1.1 -2.2 3.7 -4.6 5.3 -3.8 10.3 -20.2 3.2 8.1) (new Vector: Number: innerLoop 1.1 2.4 3.8 4.3 5.6 6.9 7.1 8.7 9.1 10.3) (new Vector: Number: innerLoop 1.1 2.4 3.8 4.3 5.6 6.9 7.1 8.7 9.1 10.3)))
(setq endGTime (getTickCount startTime))
(setq startTime (getTickCount 0))
(setq v2 (lispNumberVector  (new Vector: Number: outterLoop 1.1 -2.2 3.7 -4.6 5.3 -3.8 10.3 -20.2 3.2 8.1) (new Vector: Number: innerLoop 1.1 2.4 3.8 4.3 5.6 6.9 7.1 8.7 9.1 10.3) (new Vector: Number: innerLoop 1.1 2.4 3.8 4.3 5.6 6.9 7.1 8.7 9.1 10.3)))
(setq endLTime (getTickCount startTime))
(if (<> v1 v2) (error "!ERROR: Lisp generic result vector does not match Lisp integer result vector!"))
(writeln "Lisp(Register) is " (text (/ endGTime endLTime) "###.0000") " times as fast as Lisp(Generic) Lisp[" endLTime "], Generic[" endGTime "] ******* ") 

;;  Notify user of completion
(setq totalEndTime (getTickCount totalStartTime))
(writeln "Lisp Timing Test completed in [" totalEndTime "] Seconds" )

(testEnd "Test_LispTime")








