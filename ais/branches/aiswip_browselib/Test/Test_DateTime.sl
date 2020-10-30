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
;;  Title:    SmartLisp Date Time Procedures Test
;;
;;  Author:   Michael F. Korns, Tim May
;;
;;  Project:  AIS Regression Suite
;;
;;  Notes:    The SmartBase features in the Date Time Procedures
;;            chapter are tested in this test suite script.
;;
;;  Files:    RegTest.sl
;;

;#memory=500
;#memoryObjectHeaders=100
(runScript "RegTest.sl")
(testStart "Test_DateTime.sl")


;;************************************************************************
;; Tests for the basic date conversdion Procedures
;;************************************************************************

(loop for i from -1 until -2000 by -1 do 
   (if (<> (date (string (date i))) (date i) )
       (begin
         (diagnostic scriptName " *FAILURE* " "(date i)")
         (error "datetimeprocs")))
   )

(loop for i from 0 until 2000 do 
   (if (<> (date (string (date i))) (date i) )
       (begin
         (diagnostic scriptName " *FAILURE* " "(date i)")
         (error "datetimeprocs")))
   )

;;************************************************************************
;; Tests for the date Date Time Procedure
;;************************************************************************

(testit "(date  1994  2  3)" #Feb,3,1994)

(testit "(date  1904 1  3)" #Jan,3,1904)

(testit "(date  -349  7  4)" #Jul,4,349BC)

;;************************************************************************
;; Tests for the day Date Time Procedure
;;************************************************************************

(testit "(day #Jan,3,1954)" 3)

(testit "(day \"7/4/1992\")" 4)

(testit "(day 713685)" 3)

;;************************************************************************
;; Tests for the days360 Date Time Procedure
;;************************************************************************

(testit "(days360 \"1/1/1992\" \"1/1/1993\")" 360)

(testit "(days360 \"2/15/1992\" \"3/15/1992\")" 30)

(testit "(days360 \"2/29/1992\" \"2/1/1992\")" -28)

;;************************************************************************
;; Tests for the hour Date Time Procedure
;;************************************************************************

(testit "(hour \"1:30:00 PM\")" 13)

(testit "(hour \"23:30\")" 23)

(testit "(hour #Jan,1,0:23:30)" 23)

(testit "(hour .5)" 12)

;;************************************************************************
;; Tests for the julian Date Time Procedure
;;************************************************************************

(testit "(julian \"7/4/1992\")" 727747)

(testit "(julian \"1/2/-1000\")" -365241)

(testit "(julian \"12/7/1941\")" 709275)

(testit "(julian #Dec,7,1941)" 709275)

(setq i (julian #Jan,1,4BC))
(setq x (julian (date (string (date i)))))
(if (<> x i)
	(begin
	(diagnostic scriptName " *FAILURE* " "(julian (date (string (date i))))")
	(error "datetimeprocs"))
)
(setq i (julian #Jan,1,4))
(setq x (julian (date (string (date i)))))
(if (<> x i)
	(begin
	(diagnostic scriptName " *FAILURE* " "(julian (date (string (date i))))")
	(error "datetimeprocs"))
)

(setq i (julian #Jan,1,1996))
(setq x (julian (date (string (date i)))))
(if (<> x i)
	(begin
	(diagnostic scriptName " *FAILURE* " "(julian (date (string (date i))))")
	(error "datetimeprocs"))
)
(setq i (julian #Jan,1,2004))
(setq x (julian (date (string (date i)))))
(if (<> x i)
	(begin
	(diagnostic scriptName " *FAILURE* " "(julian (date (string (date i))))")
	(error "datetimeprocs"))
)

;;************************************************************************
;; Tests for the minute Date Time Procedure
;;************************************************************************

(testit "(minute \"1:30:23 PM\")" 30)

(testit "(minute \"23:01\")" 1)

(testit "(minute .5)" 0)

;;************************************************************************
;; Tests for the month Date Time Procedure
;;************************************************************************

(testit "(month #Jan,3,1954)" 1)

(testit "(month \"7/4/1992\")" 7)

(testit "(month 713882)" 7)

;;************************************************************************
;; Tests for the second Date Time Procedure
;;************************************************************************

(testit "(second #Jan,1,1:12:34:17)" 17)

(testit "(second \"3:23:45PM\")" 45)

(testit "(second 713882.56839)" 28)

;;************************************************************************
;; Tests for the time Date Time Procedure
;;************************************************************************

(testit "(second (time 0 10 30))" 30)

(testit "(minute (time 12 15 45))" 15)

(testit "(hour (time 12 15 45))" 12)

(testit "(hour (time #Jan,1,0:2:30))" 2)

(testit "(hour (time \"2:30pm\"))" 14)

;;************************************************************************
;; Tests for the year Date Time Procedure
;;************************************************************************

(testit "(year #Jan,4,1987)" 1987)

(testit "(year \"3/5/1967\")" 1967)

(testit "(year -1029)" -3)

(testEnd "Test_DateTime.sl")



