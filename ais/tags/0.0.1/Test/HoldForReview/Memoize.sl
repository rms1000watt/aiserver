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
;;  Title:    SmartLisp Timing Test
;;
;;  Author:   Michael F. Korns
;;
;;  Project:  The speed of simple native SmartLisp execution
;;            loops, as well as the efficiency of the SmartLisp
;;            compiler is acceptance tested in this test script.
;;
;;  Notes:    cinteger    
;;            cReal
;;            cString
;;            cCall
;;            cLoop
;;            No other dependencies.  
;;

(writeln "SmartLisp Timing Test started")

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
    (/ m n))
    
;; *******************************************************************
;; name:     lispNilpotent
;; 
;; summary:  Define a simple SmartLisp nilpotent loop procedure.
;; *******************************************************************
(define (lispNilpotent n)
    vars:(i) 
    (loop for i from 1 to n do ))

;; *******************************************************************
;; name:     lispInteger
;; 
;; summary:  Define a simple SmartLisp integer loop procedure.
;; *******************************************************************
(define (lispInteger n)
    vars:(i (x 0))
    (loop for i from 1 to n do (setq x (addi x 1))))

;; *******************************************************************
;; name:     lispReal
;; 
;; summary:  Define a simple SmartLisp real loop procedure.
;; *******************************************************************
(define (lispReal n)
    vars:(i (x 0))
    (loop for i from 1 to n do (setq x (+ x 1))))

;; *******************************************************************
;; name:     lispString
;; 
;; summary:  Define a simple SmartLisp string loop procedure.
;; Note:     (loop for i from 0 to n do (setq s "Hello there you all"))
;; *******************************************************************
(define (lispString n)
    vars:(i s)
    (loop for i from 0 to n do (setq s "Hello there you all")))

;; *******************************************************************
;; name:     lispCall
;; 
;; summary:  Define a simple vm call loop procedure.
;; Note:     vars:((x +)) (loop for i from 0 to n do (x 0.1 1.1))
;; *******************************************************************
(define (lispCall n)
    vars:(i x)
    (setq x +)
    (loop for i from 0 to n do (x 0.1 1.1)))

;; *******************************************************************
;; name:     lispLoop
;; 
;; summary:  Define a simple SmartLisp timing loop procedure.
;;  Note:    This loop includes an integer add, a real add,
;;           a string move, and a function call.
;; *******************************************************************
(define (lispLoop n)
    vars:(i (m 0) (y 1.1) x s)
    (setq x +)
    (loop for i from 0 to n do
       (begin
           (addi m 1)                       ; Integer add
           (+ y 1)                          ; real add
           (setq s "Hello there you all")   ; string move
           (x 0.1 1.1))))                   ; procedure call

;; *******************************************************************
;; name:     lispStructure
;; 
;; summary:  Define a simple SmartLisp timing loop procedure.
;;  Note:    This loop compares two Structures.
;; *******************************************************************
(define (lispStructure n)
    vars:(i (m 0) y x s)
    (setq x #{a: 1 b: 2})
    (setq y #{a: 2 b: 3})
    (loop for i from 0 to n do (if (< x.a y.a) true false)))

;; *******************************************************************
;; name:     lispFriend
;; 
;; summary:  Define a simple SmartLisp timing loop procedure.
;;  Note:    This loop compares two Structures (as simulated friends).
;; *******************************************************************
(define (lispFriend n)
    vars:(i (m 0) y x x_a y_a)
    (setq x #{a: 1 b: 2})
    (setq y #{a: 2 b: 3})
    (setq x_a 1)
    (setq y_a 2)
    (loop for i from 0 to n do (if (< x_a y_a) true false)))

;; *******************************************************************
;; Start timing tests
;; *******************************************************************
(gc)
(writeln "******Start timing ration for lispStructure*******")
(writeln "Timing ratio for lispStructure/lispFriend = " (timingRatio lispStructure lispFriend 100000)) 

(writeln "******Start timing for lispInteger*******")
(writeln "Timing for lispInteger = " (timingTest lispInteger 2000000)) 





(writeln "******Start timing ration for lispInteger*******")
(writeln "Timing ratio for lispInteger/cInteger = " (timingRatio lispInteger cInteger 2000000)) 
(writeln "******Start timing ration for lispReal*******")
(writeln "Timing ratio for lispReal/cReal = " (timingRatio lispReal cReal 400000)) 
(writeln "******Start timing ration for lispString*******")
(writeln "Timing ratio for lispString/cString = " (timingRatio lispString cString 400000)) 
(writeln "******Start timing ration for lispCall*******")
(writeln "Timing ratio for lispCall/cCall = " (timingRatio lispCall cCall 400000)) 
(writeln "******Start timing ration for lispLoop*******")
(writeln "Timing ratio for lispLoop/cLoop = " (timingRatio lispLoop cLoop 400000)) 

;;  Notify user of completion
(writeln "SmartLisp Timing Test completed")










