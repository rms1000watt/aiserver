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
;;  Title:    Simulation Test for the Gate.sl problem
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
;; Complete declarative description of the gate.v example problem
;;************************************************************************
#CLEARCONSTRAINTS#
(constraintEvaluation false)
(setq _initial 0)
(setf %count   {(cond 
                  ((= (- &clock %clock) 1)
                     (setq %count (setq %count[0] (binaryNot &count[0])))
                     (setq %count (setq %count[1] (bitwiseXor &count[1] &count[0])))
                     (setq %count (setq %count[2] (bitwiseXor &count[2] (reduceAnd &count 2))))
                     (setq %count (setq %count[3] (bitwiseXor &count[3] (reduceAnd &count 3)))))
                  (else %count))})
(setf %clock   {(cond 
                  ((= _clock 0) 0)
                  ((= (modi _clock 2) 0) (binaryNot %clock))
                  (else %clock))})
(setf %f       {(reduceAnd %count 4)})
(setf %af      {(reduceAnd %count 4)})
(setf &count   {%count})
(setf &clock   {%clock})
(setf &monitor {(if (= _clock _finish)
                    (writeln _clock ",,,count=" %count " ,f=" %f ",af=" %af))})
