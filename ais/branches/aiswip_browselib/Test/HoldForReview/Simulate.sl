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
;;  Title:    SmartLisp Simulation Programming Test
;;
;;  Author:   Michael F. Korns
;;
;;  Project:  SmartBase Automated test suite 
;;
;;  Notes:    The SmartBase features in the Simulation
;;            chapter are tested in this test suite script.
;;
;;  Files:    No dependencies. 
;;

;;************************************************************************
;; Test script global variables
;;************************************************************************
(setq scriptName "Simulation Programming Test")

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
         (error "funcprog"))
       true))

;;************************************************************************
;;  Start the Test Script
;;************************************************************************
(writeln   scriptName " started")

;;************************************************************************
;; Complete declarative description of the foo2.v example problem
;;************************************************************************
#CLEARCONSTRAINTS#
(constraintEvaluation false)
(setq _initial 0)

(setf %R  {(cond 
             ((and (= &S2 0) (= (- _clock _S2) 1)) 0)
             ((and (= &S2 1) (= (- _clock _S2) 12)) 1)
             (else %R))})
(setf %A  {(cond 
             ((and (= &S1 0) (= (- _clock _S1) 1)) 0)
             ((and (= &S1 2) (= (- _clock _S1) 1)) 1)
             (else %A))})
(setf %BV  {(cond 
             ((and (= &S2 1) (= (- _clock _S2) 11) (>= %BV 99)) 0)
             ((and (= &S2 1) (= (- _clock _S2) 11) (< %BV 99)) (add1 %BV))
             (else %BV))})
(setf %L  {(cond 
             ((and (= &S1 1) (= (- _clock _S1) 5)) %RV)
             ((and (= &S1 1) (>= (- _clock _S1) 5) (> %L 0)) (sub1 %L))
             (else %L))})
(setf %RV {(cond 
             ((and (= &S2 1) (= (- _clock _S2) 11)) %BV)
             (else %RV))})
(setf &S1 {(cond 
             ((and (= &S1 0) (>= (- _clock _S1) 1) (= %R 1)) (setq _S1 _clock) 1)
             ((and (= &S1 1) (>= (- _clock _S1) 5) (<= %L 0)) (setq _S1 _clock) 2)
             ((and (= &S1 2) (>= (- _clock _S1) 1) (= %R 0)) (setq _S1 _clock)  0)
             (else &S1))})
(setf &S2 {(cond 
             ((and (= &S2 0) (>= (- _clock _S2) 1) (= %A 0)) (setq _S2 _clock) 1)
             ((and (= &S2 1) (>= (- _clock _S2) 12) (= %A 1)) (setq _S2 _clock) 0)
             (else &S2))})
(setf &monitor {(if (= _clock _finish)
                    (writeln "Tm=" _clock ",A=" %A 
                         ",L=" %L ",R=" %R ",RV=" %RV ",BV=" %BV 
                         ",S1=" &S1 ",_S1=" _S1 ",S2=" &S2 ",_S2=" _S2))})
(setf &initial {(cond 
                  ((= _clock 0) (setq _S1 0) (setq _S2 0)))})

;;************************************************************************
;; Test the foo2.v interpreter against the compiled C version for accuracy
;;************************************************************************
(simulate 500) 
(simulate 500 false) 

(writeln   scriptName " completed")

