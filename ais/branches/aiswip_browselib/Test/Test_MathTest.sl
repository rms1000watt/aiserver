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
;;  Title:    SmartLisp Math Procedures Test
;;
;;  Author:   Michael F. Korns, Tim May
;;
;;  Project:  AIS Regression Suite
;;
;;  Notes:    The SmartBase features in the Math Procedures
;;            chapter are tested in this test suite script.
;;
;;  Files:    RegTest.sl
;;
;#memory=500
;#memoryObjectHeaders=100
(runScript "RegTest.sl")
(testStart "Test_BadRef.sl")

;;************************************************************************
;; Tests for the abs Math Procedure
;;************************************************************************

(testit "(abs -13 )" 13)

(testit "(abs -13.45 )" 13.45)

;;************************************************************************
;; Tests for the avg Math Procedure
;;************************************************************************

(testit "(avg  #(1 2 3 4))" 2.5)

(testit "(avg  1 2 3 4 5)" 3)

(testit "(define  Y  (begin  (+  1  2)  (+  3  4)))" 7)

;;************************************************************************
;; Tests for the ceiling Math Procedure
;;************************************************************************

(testit "(ceiling  -2.3)" -2)

(testit "(ceiling  2.3)" 3)

(testit "(ceiling  5)" 5)

;;************************************************************************
;; Tests for the count Math Procedure
;;************************************************************************

(testit "(count  '(1  2  3  4))" 4)

(testit "(count  #{'A 1  'B 2  'C 5  'D 6})" 4)

;;************************************************************************
;; Tests for the isEven Math Procedure
;;************************************************************************

(testit "(isEven  1)"   false  )

(testit "(isEven  .22)"   false  )

(testit "(isEven  0)"   true  )

(testit "(isEven  -4)"   true  )

;;************************************************************************
;; Tests for the isExact Math Procedure
;;************************************************************************

(testit "(isExact  1)"   true  )

(testit "(isExact  .22)"   false  )

(testit "(isExact  0)"   true  )

(testit "(isExact  -4)"   true  )

;;************************************************************************
;; Tests for the expt Math Procedure
;;************************************************************************

(testit "(expt  4 .5)"   2)

(testit "(expt  4 1.5)"   8)

(testit "(expt  4 -1)"   .25)

(testit "(round (expt  2 4))"   16  )

(testit "(round (* (expt  2 -4) 10000))"   625  )

;;************************************************************************
;; Tests for the fact Math Procedure
;;************************************************************************

( testit "(fact  4)" 24)

( testit "(fact  10)" 3628800)

;;************************************************************************
;; Tests for the floor Math Procedure
;;************************************************************************

( testit "(truncate  -4.2)"  -5    )

(testit "(floor  2.3)"   2  )

(testit "(floor  -2.3)"   -3  )

(testit "(floor  2)"   2  )

;;************************************************************************
;; Tests for the fraction Math Procedure
;;************************************************************************

( testit "(+ (fraction -4.2345) (ceiling -4.2345))"  -4.2345)

( testit "(+ (fraction 344.2345) (floor 344.2345))"  344.2345)

;;************************************************************************
;; Tests for the gcd Math Procedure
;;************************************************************************

( testit "(gcd  49  28)" 7 )

( testit "(gcd  206  40)" 2 )

;;************************************************************************
;; Tests for the inExact Math Procedure
;;************************************************************************

( testit "(isInexact  -4.1)" true )

( testit "(isInexact  0)" false )

;;************************************************************************
;; Tests for the kurtosis Math Procedure
;;************************************************************************

( testit "(round (* (kurtosis  10 20 40 45) 10000000))" -38690053 )

( testit "(round (* (kurtosis  20 50 55 60) 10000000))" 30143600 )

;;************************************************************************
;; Tests for the lcm Math Procedure
;;************************************************************************

( testit "(lcm  32  -36)" 288 )

( testit "(lcm  4   5)" 20 )

;;************************************************************************
;; Tests for the max Math Procedure
;;************************************************************************

( testit "(max  (+ 1  5)  (+ 3  4)) " 7 )

;;************************************************************************
;; Tests for the median Math Procedure
;;************************************************************************

( testit "(median 13500 15200 16820 17800 19350)" 16820 )

( testit "(median 10 20 30 40)" 25 )

;;************************************************************************
;; Tests for the min Math Procedure
;;************************************************************************

( testit "(min  1  2  3  4  5  6) " 1 )

;;************************************************************************
;; Tests for the isNegative Math Procedure
;;************************************************************************

( testit "(isNegative  -1.23) " true )

( testit "(isNegative  0) " false )

( testit "(isNegative  3.4) " false )

;;************************************************************************
;; Tests for the isOdd Math Procedure
;;************************************************************************

( testit "(isOdd  -1.23) " false )

( testit "(isOdd  0) " false )

( testit "(isOdd  3) " true )

( testit "(isOdd  -5) " true )

;;************************************************************************
;; Tests for the isPositive Math Procedure
;;************************************************************************

( testit "(isPositive  -5) " false )

( testit "(isPositive  0) " false )

( testit "(isPositive  45) " true )

;;************************************************************************
;; Tests for the product Math Procedure
;;************************************************************************

( testit "(product  #(1 2 3 4)) " 24 )

( testit "(product  #(1 2 3 4)  5) " 120 )

( testit "(product  #(1 2 3 4)  #(1 2 3 4)) " 576 )

;;************************************************************************
;; Tests for the range Math Procedure
;;************************************************************************

( testit "(range  #(1 2 3 4 5)) " 4 )

( testit "(range  -5 -2 0 1 5) " 10 )

( testit "(range  #(1 2 3 4)  #(1 2 3 4)) " 3 )

;;************************************************************************
;; Tests for the round Math Procedure
;;************************************************************************

( testit "(round  -4.2)" -4 )

( testit "(round  44.2)" 44 )

( testit "(round  44)" 44 )

;;************************************************************************
;; Tests for the skew Math Procedure
;;************************************************************************

( testit "(round (* (skew 10 20 40 45) 10000000))" -2287278 )

( testit "(round (* (skew 20 50 55 60) 10000000))" -16963868 )

;;************************************************************************
;; Tests for the sqrt Math Procedure
;;************************************************************************

( testit "(sqrt  25)" 5 )

( testit "(sqrt  16)" 4 )

( testit "(sqrt  9)" 3 )

;;************************************************************************
;; Tests for the stdev Math Procedure
;;************************************************************************

( testit "(round (* (stdev 10 20 40 45) 10000000))" 165201897 )

( testit "(round (* (stdev 20 50 55 60) 10000000))" 179698822 )

;;************************************************************************
;; Tests for the stdevp Math Procedure
;;************************************************************************

( testit "(round (* (stdevp 10 20 40 45) 10000000))" 143069039 )

( testit "(round (* (stdevp 20 50 55 60) 10000000))" 155623745 )

;;************************************************************************
;; Tests for the sum Math Procedure
;;************************************************************************

( testit "(sum  #(1 2 3 4))"  10    )

( testit "(sum  #(1 2 3 4) 55)"  65    )

( testit "(sum  1 2 3 4)"  10    )

;;************************************************************************
;; Tests for the sumsqr Math Procedure
;;************************************************************************

( testit "(sumsqr  #(1 2 3 4))"  30    )

( testit "(sumsqr  #(1 2 3 4) 55)"  3055    )

( testit "(sumsqr  1 2 3 4)"  30    )

;;************************************************************************
;; Tests for the var Math Procedure
;;************************************************************************

( testit "(round (* (var 10 20 40 45) 10000000))" 2729166667 )

( testit "(round (* (var 20 50 55 60) 10000000))" 3229166667 )

;;************************************************************************
;; Tests for the varp Math Procedure
;;************************************************************************

( testit "(round (* (varp 10 20 40 45) 10000000))" 2046875000 )

( testit "(round (* (varp 20 50 55 60) 10000000))" 2421875000 )

;;************************************************************************
;; Tests for the isZero Math Procedure
;;************************************************************************

( testit "(isZero  (+  -1  1))" true )

( testit "(isZero  1)" false )

(testEnd "Test_MathTest.sl")

