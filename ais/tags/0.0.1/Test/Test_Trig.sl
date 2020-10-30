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
;;  Title:    SmartLisp Trigonometric Procedures Test
;;
;;  Author:   Michael F. Korns, Tim May
;;
;;  Project:  AIS Regression Suite
;;
;;  Notes:    The SmartBase features in the Trig Procedures
;;            chapter are tested in this test suite script.
;;
;;  Files:    RegTest.sl
;;

;#memory=500
;#memoryObjectHeaders=100
(runScript "RegTest.sl")
(testStart "Test_Trig.sl")

;;************************************************************************
;;  Start the Test Script
;;************************************************************************
(writeln   scriptName " started")

;;************************************************************************
;; Tests for the acos Trig Procedure
;;************************************************************************

( testit "(round (* (acos -1) 10000000))" 31415927 )

( testit "(round (* (acos 0) 10000000))" 15707963 )

( testit "(round (* (acos .5) 10000000))" 10471976 )

( testit "(round (* (acos 1) 10000000))" 0 )

;;************************************************************************
;; Tests for the asin Trig Procedure
;;************************************************************************

( testit "(round (* (asin -1) 10000000))" -15707963 )

( testit "(round (* (asin 0) 10000000))" 0 )

( testit "(round (* (asin .5) 10000000))" 5235988 )

( testit "(round (* (asin 1) 10000000))" 15707963 )

;;************************************************************************
;; Tests for the atan Trig Procedure
;;************************************************************************

( testit "(round (* (atan 10) 10000000))" 14711277 )

( testit "(round (* (atan -1) 10000000))" -7853982 )

( testit "(round (* (atan 0) 10000000))" 0 )

( testit "(round (* (atan 1) 10000000))" 7853982 )

;;************************************************************************
;; Tests for the cos Trig Procedure
;;************************************************************************

( testit "(round (* (cos .5) 10000000))" 8775826 )

( testit "(round (* (cos -1) 10000000))" 5403023 )

( testit "(round (* (cos 0) 10000000))" 10000000 )

( testit "(round (* (cos 1) 10000000))" 5403023 )

;;************************************************************************
;; Tests for the cosh Trig Procedure
;;************************************************************************

( testit "(round (* (cosh 10) 10000000))" 110132329201 )

( testit "(round (* (cosh -1) 10000000))" 15430806 )

( testit "(round (* (cosh 0) 10000000))" 10000000 )

( testit "(round (* (cosh 1) 10000000))" 15430806 )

;;************************************************************************
;; Tests for the deg Trig Procedure
;;************************************************************************

( testit "(round (* (deg 10) 10000000))" 5729577951 )

( testit "(round (* (deg -1) 10000000))" -572957795 )

( testit "(round (* (deg 0) 10000000))" 0 )

( testit "(round (* (deg 1) 10000000))" 572957795 )

;;************************************************************************
;; Tests for the exp Trig Procedure
;;************************************************************************

( testit "(round (* (exp 10) 10000000))" 220264657948 )

( testit "(round (* (exp -1) 10000000))" 3678794 )

( testit "(round (* (exp 0) 10000000))" 10000000 )

( testit "(round (* (exp 1) 10000000))" 27182818 )

;;************************************************************************
;; Tests for the log Trig Procedure
;;************************************************************************

( testit "(round (* (log 10) 10000000))" 23025851 )

( testit "(round (* (log (exp 1)) 10000000))" 10000000 )

( testit "(round (* (log .5) 10000000))" -6931472 )

( testit "(round (* (log 1) 10000000))" 0 )

;;************************************************************************
;; Tests for the logbase Trig Procedure
;;************************************************************************

( testit "(round (* (logbase 10) 10000000))" 10000000 )

( testit "(round (* (logbase 4 2) 10000000))" 20000000 )

( testit "(round (* (logbase .5 3) 10000000))" -6309298 )

( testit "(round (* (logbase 1) 10000000))" 0 )

;;************************************************************************
;; Tests for the log10 Trig Procedure
;;************************************************************************

( testit "(round (* (log10 10) 10000000))" 10000000 )

( testit "(round (* (log10 4) 10000000))" 6020600 )

( testit "(round (* (log10 .5) 10000000))" -3010300 )

( testit "(round (* (log10 1) 10000000))" 0 )

;;************************************************************************
;; Tests for the log2 Trig Procedure
;;************************************************************************

( testit "(round (* (log2 10) 10000000))" 33219281 )

( testit "(round (* (log2 4) 10000000))" 20000000 )

( testit "(round (* (log2 .5) 10000000))" -10000000 )

( testit "(round (* (log2 1) 10000000))" 0 )

;;************************************************************************
;; Tests for the pi Trig Procedure
;;************************************************************************

( testit "(round (* (pi) 10000000))" 31415927 )

;;************************************************************************
;; Tests for the rad Trig Procedure
;;************************************************************************

( testit "(round (* (rad 90) 10000000))" 15707963 )

( testit "(round (* (rad 45) 10000000))" 7853982 )

( testit "(round (* (rad 1) 10000000))" 174533 )

( testit "(round (* (rad 0) 10000000))" 0 )

;;************************************************************************
;; Tests for the sign Trig Procedure
;;************************************************************************

( testit "(sign -45.67)" -1 )

( testit "(sign 0)" 0 )

( testit "(sign 145.67)" 1 )

;;************************************************************************
;; Tests for the sin Trig Procedure
;;************************************************************************

( testit "(round (* (sin -1) 10000000))" -8414710 )

( testit "(round (* (sin .5) 10000000))" 4794255 )

( testit "(round (* (sin 1) 10000000))" 8414710 )

( testit "(round (* (sin 0) 10000000))" 0 )

;;************************************************************************
;; Tests for the sinh Trig Procedure
;;************************************************************************

( testit "(round (* (sinh -1) 10000000))" -11752012 )

( testit "(round (* (sinh .5) 10000000))" 5210953 )

( testit "(round (* (sinh 1) 10000000))" 11752012 )

( testit "(round (* (sinh 0) 10000000))" 0 )

;;************************************************************************
;; Tests for the tan Trig Procedure
;;************************************************************************

( testit "(round (* (tan -1) 10000000))" -15574077 )

( testit "(round (* (tan .5) 10000000))" 5463025 )

( testit "(round (* (tan 1) 10000000))" 15574077 )

( testit "(round (* (tan 0) 10000000))" 0 )

;;************************************************************************
;; Tests for the tanh Trig Procedure
;;************************************************************************

( testit "(round (* (tanh -1) 10000000))" -7615942 )

( testit "(round (* (tanh .5) 10000000))" 4621172 )

( testit "(round (* (tanh 1) 10000000))" 7615942 )

( testit "(round (* (tanh 0) 10000000))" 0 )

(testEnd "Test_Trig.sl")

