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
;;  Title:    SmartLisp Financial Procedures Test
;;
;;  Author:   Michael F. Korns, Tim May
;;
;;  Project:  AIS Regression Suite 
;;
;;  Notes:    The SmartBase features in the Financial Procedures
;;            chapter are tested in this test suite script.
;;
;;  Files:    RegTest.sl
;;

;#memory=500
;#memoryObjectHeaders=100
(runScript "RegTest.sl")
(testStart "Test_Finance.sl")

;;************************************************************************
;; Tests for the depreciation Financial Procedures
;;************************************************************************

(testit "(depreciation 5000 500 5 1 6)" 922.5)

(testit "(depreciation 5000 500 5 2 6)" 1504.5975)

(testit "(round (depreciation 5000 500 5 3 6))" 949)

(testit "(round (depreciation 5000 500 5 4  6))" 599)

(testit "(round (depreciation 5000 500 5 5 6))" 378)

;;************************************************************************
;; Tests for the doubleDepreciation Financial Procedures
;;************************************************************************

(testit "(doubleDepreciation 5000 700 5 1 2)" 2000)

(testit "(doubleDepreciation 5000 700 5 2 2)" 1200)

(testit "(round (doubleDepreciation 5000 700 5 3 2))" 720)

(testit "(round (doubleDepreciation 15000 1200 60 17 2))" 291)

;;************************************************************************
;; Tests for the futureValue Financial Procedures
;;************************************************************************

(testit "(round (futureValue .12 25 -2000 -6000))" 368668)

(testit "(round (futureValue .12 25 -3000 -6000))" 502002)

(testit "(round (futureValue .01 300 -166 -6000))" 430619)

(testit "(round (futureValue (/ .09 365) (* 5 365) 0 -50000))" 78411)

;;************************************************************************
;; Tests for the interestPayment Financial Procedures
;;************************************************************************

(testit "(round (interestPayment (/ 0.085 12) 1 360 202300))" -1433)

(testit "(round (interestPayment (/ 0.085 12) 360 360 202300))" -11)

;;************************************************************************
;; Tests for the payment Financial Procedures
;;************************************************************************

(testit "(round (payment (/ 0.085 12) 360 202300))" -1556)

(testit "(round (payment (/ 0.06 12) 480 245000))" -1348)

(testit "(round (payment .1 360 100000))" -10000)

;;************************************************************************
;; Tests for the principalPayment Financial Procedures
;;************************************************************************

(testit "(round (principalPayment (/ .085 12) 1 360 202300))" -123)

(testit "(round (principalPayment (/ .085 12) 360 360 202300))" -1545)

;;************************************************************************
;; Tests for the presentValue Financial Procedures
;;************************************************************************

(testit "(round (presentValue .0075 60 1000))" -48173)

(testit "(round (presentValue .09 5 0 80000))" -51995)

;;************************************************************************
;; Tests for the straightDepreciation Financial Procedures
;;************************************************************************

(testit "(round (straightDepreciation 5000 700 5))" 860)

(testit "(round (straightDepreciation 510000 200 5))" 101960)

;;************************************************************************
;; Tests for the sumDepreciation Financial Procedures
;;************************************************************************

(testit "(round (sumDepreciation 5000 700 5 1))" 1433)

(testit "(round (sumDepreciation 5000 700 5 2))" 1147)

(testit "(round (sumDepreciation 5000 700 5 3))" 860)

(testit "(round (sumDepreciation 5000 700 5 4))" 573)

;;************************************************************************
;; Tests for the variableDepreciation Financial Procedures
;;************************************************************************

(testit "(round (variableDepreciation 8000 1500 1825 0 1))" 9)

(testit "(round (variableDepreciation 8000 1500 260 0 1))" 62)

(testit "(round (variableDepreciation 8000 1500 60 13 31 1.25))" 2026)

(testit "(round (variableDepreciation 8000 1500 260 53 131  1.25))" 2037)


(testEnd "Test_Finance.sl")



