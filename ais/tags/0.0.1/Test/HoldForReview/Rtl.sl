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
;;  Title:    Simulation Test for rtl problem
;;
;;  Author:   Michael F. Korns
;;
;;  Project:  SmartBase Verilog Simulator Project
;;
;;  Notes:    The SmartBase features in the Simulation
;;            chapter are tested in this script.
;;
;;  Files:    No dependencies. 
;;

;;************************************************************************
;; Complete declarative description of the rtl.v example problem
;;************************************************************************
#CLEARCONSTRAINTS#
(constraintEvaluation false)
(setq _initial 0)

(setf %R  {(cond 
             ((timestampEQ &S2 0 1) 0)
             ((timestampEQ &S2 1 12) 1)
             (else %R))})
(setf %A  {(cond 
             ((timestampEQ &S1 0 1) 0)
             ((timestampEQ &S1 2 1) 1)
             (else %A))})
(setf %BV  {(cond 
             ((and (timestampEQ &S2 1 11) (>= %BV 99)) 0)
             ((and (timestampEQ &S2 1 11) (< %BV 99)) (add1 %BV))
             (else %BV))})
(setf %L  {(cond 
             ((timestampEQ &S1 1 5) %RV)
             ((and (timestampGE &S1 1 5) (> %L 0)) (sub1 %L))
             (else %L))})
(setf %RV {(cond 
             ((timestampEQ &S2 1 11) %BV)
             (else %RV))})
(setf &S1 {(cond 
             ((and (timestampGE &S1 0 1) (= %R 1)) (timestamp 1))
             ((and (timestampGE &S1 1 5) (<= %L 0)) (timestamp 2))
             ((and (timestampGE &S1 2 1) (= %R 0)) (timestamp 0))
             (else &S1))})
(setf &S2 {(cond 
             ((and (timestampGE &S2 0 1) (= %A 0)) (timestamp 1))
             ((and (timestampGE &S2 1 12) (= %A 1)) (timestamp 0))
             (else &S2))})
(setf &monitor {(if (= _clock _finish)
                    (writeln "Tm=" _clock ",A=" %A 
                         ",L=" %L ",R=" %R ",RV=" %RV ",BV=" %BV 
                         ",S1=" (getState &S1) ",t_S1=" (getTime &S1) ",S2=" (getState &S2) ",t_S2=" (getTime &S2)))})

