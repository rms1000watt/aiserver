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
;;  Title:    Lisp String Procedures Test
;;
;;  Author:   Michael F. Korns, Tim May
;;
;;  Project:  AIS Regression Suite
;;
;;  Notes:    The SmartBase features in the String Procedures
;;            chapter are tested in this test suite script.
;;
;;  Files:    RegTest.sl
;;


;#memory=500
;#memoryObjectHeaders=100
(runScript "RegTest.sl")
(testStart "Test_String.sl")

;;************************************************************************
;; Tests for the char String Procedure
;;************************************************************************

( testit "(char 32)" #\space )

( testit "(char 13)" #\return )

( testit "(char 48)" #\0 )

;;************************************************************************
;; Tests for the clean String Procedure
;;************************************************************************

( testit "(clean  (append  (char 7)  \"TEXT\"   (char 233)))" "TEXT" )

( testit "(clean  (append  (char 7)  \"TE XT\"   (char 233)))" "TE XT" )

;;************************************************************************
;; Tests for the code String Procedure
;;************************************************************************

( testit "(code (char 32))" 32 )

( testit "(code (char 13))" 13 )

( testit "(code (char 48))" 48 )

;;************************************************************************
;; Tests for the dollar String Procedure
;;************************************************************************

( testit "(dollar 8763.369 2)" "$8763.36" )

( testit "(dollar -1.358 2)" "($1.35)" )

;;************************************************************************
;; Tests for the downcase String Procedure
;;************************************************************************

(testit "(downcase  #\\A)" #\a)

(testit "(downcase  \"Hello\")" "hello")

(testit "(charDowncase  \"HeLLo\")" "hello")

;;************************************************************************
;; Tests for the find String Procedure
;;************************************************************************

(setq x (makeVector byte: 4 1 0 2 3))

(setq y (makeVector byte: 2 0 2))

(testit "(find \"She\" \"Sheldon sells sea shells\")" 0)

(testit "(find \"Pipe\" \"Piper Piper\"  3)" 6)

(testit "(search \"Pipe\" \"peter piper\")" false)

(testit "(find y x)" 1)

;;************************************************************************
;; Tests for the fixed String Procedure
;;************************************************************************

(testit "(fixed 3.141592)" "3.14")

(testit "(fixed -3.141592 3)" "-3.141")

(testit "(fixed .141592 4)" "0.1415")

;;************************************************************************
;; Tests for the left String Procedure
;;************************************************************************

(testit "(left \"ABCDEF\" 3)" "ABC")

(testit "(left \"ABCDEF\" 4)" "ABCD")

;;************************************************************************
;; Tests for the mid String Procedure
;;************************************************************************

(testit "(mid \"Small World\" 0 5 )" "Small")

(testit "(mid \"Small World\" 6 5 )" "World")

(testit "(mid \"Small World\" 6 1000)" "World")

(testit "(mid \"Small World\" 34 1000)" "")

;;************************************************************************
;; Tests for the parse String Procedure
;;************************************************************************

(testit "(parse \"3.141592\")" 3.141592)

(testit "(parse \"#Jan,19,1993\")" #Jan,19,1993)

(testit "(parse \"23Hello\")" "23Hello")

;;************************************************************************
;; Tests for the replace String Procedure
;;************************************************************************

(testit "(replace \"Small World\"  0  5 \"Old\")" "Old World")

(testit "(replace \"Small World\"  6  5 \"Planet\")" "Small Planet")

;;************************************************************************
;; Tests for the rept String Procedure
;;************************************************************************

(testit "(rept \"S\"  5)" "SSSSS")

(testit "(rept \"S\"  0)" "")

;;************************************************************************
;; Tests for the right String Procedure
;;************************************************************************

(testit "(right \"Small World\" 34)" "Small World")

(testit "(right \"Small World\" 1)" "d")

(testit "(right \"Small World\" 5)" "World")

;;************************************************************************
;; Tests for the stringCiLT String Procedure
;;************************************************************************

(testit "(stringCiLT \"Small World\" \"small world\")" false)

(testit "(stringCiLT \"Small World\" \"small World No?\")" true)

(testit "(stringCiLT \"Small World\" \"small Planet\")" false)

;;************************************************************************
;; Tests for the stringCiLE String Procedure
;;************************************************************************

(testit "(stringCiLE \"Small World\" \"small world\")" true)

(testit "(stringCiLE \"Small World\" \"small World No?\")" true)

(testit "(stringCiLE \"Small World\" \"small Planet\")" false)

;;************************************************************************
;; Tests for the stringCiNE String Procedure
;;************************************************************************

(testit "(stringCiNE \"Small World\" \"small world\")" false)

(testit "(stringCiNE \"Small World\" \"small World No?\")" true)

(testit "(stringCiNE \"Small World\" \"small Planet\")" true)

;;************************************************************************
;; Tests for the stringCiGE String Procedure
;;************************************************************************

(testit "(stringCiGE \"Small World\" \"small world\")" true)

(testit "(stringCiGE \"Small World\" \"small World No?\")" false)

(testit "(stringCiGE \"Small World\" \"small Planet\")" true)

;;************************************************************************
;; Tests for the stringCiGT String Procedure
;;************************************************************************

(testit "(stringCiGT \"Small World\" \"small world\")" false)

(testit "(stringCiGT \"Small World\" \"small World No?\")" false)

(testit "(stringCiGT \"Small World\" \"small Planet\")" true)

;;************************************************************************
;; Tests for the stringFill String Procedure
;;************************************************************************

(define x "My Love")
(testit "(stringFill x \"x\")" "xxxxxxx")

(testit "(stringFill x \"xy\")" "xxxxxxx")

;;************************************************************************
;; Tests for the stringToVector String Procedure
;;************************************************************************

(define x "My Love for you")
(testit "(stringToVector x \" \")" #("My" "Love" "for" "you"))

(testit "(stringToVector x \"for\")" #("My Love " " you"))

;;************************************************************************
;; Tests for the substitute String Procedure
;;************************************************************************

(testit "(substitute \"January 11, 1991\" \"9\" \"8\")" "January 11, 1881")

(testit "(substitute \"January 11, 1991\" \"1\" \"2\" 2)" "January 22, 1991")

;;************************************************************************
;; Tests for the substring String Procedure
;;************************************************************************

(define  x  "My love")
(testit "(substring x 3 6)" "love")

(testit "(substring x 0 1)" "My")

(testit "(substring x 0 34)" "My love")

(testit "(substring x 3)" "love")

;;************************************************************************
;; Tests for the substring String Procedure
;;************************************************************************

(define  x  "My love")
(testit "(substring x 3 6)" "love")

(testit "(substring x 0 1)" "My")

(testit "(substring x 0 34)" "My love")

(testit "(substring x 3)" "love")

;;************************************************************************
;; Tests for the substringLT String Procedure
;;************************************************************************

(define  y  "My love")
(define  x  "Love My Mom")
(testit "(substringLT   y  0  1  x  5  6)" false)

(testit "(substringLT   x  0  4  y  3  6)" true)

;;************************************************************************
;; Tests for the substringLE String Procedure
;;************************************************************************

(define  y  "My love")
(define  x  "Love My Mom")
(testit "(substringLE  y  0  1  x  5  6)" true)

(testit "(substringLE  x  0  4  y  3  6)" true)

;;************************************************************************
;; Tests for the substringEQ String Procedure
;;************************************************************************

(define  y  "My love")
(define  x  "Love My Mom")
(testit "(substringEQ  y  0  1  x  5  6)" true)

(testit "(substringEQ  x  0  4  y  3  6)" false)

;;************************************************************************
;; Tests for the substringNE String Procedure
;;************************************************************************

(define  y  "My love")
(define  x  "Love My Mom")
(testit "(substringNE  y  0  1  x  5  6)" false)

(testit "(substringNE  x  0  4  y  3  6)" true)

;;************************************************************************
;; Tests for the substringGE String Procedure
;;************************************************************************

(define  y  "My love")
(define  x  "Love My Mom")
(testit "(substringGE  y  0  1  x  5  6)" true)

(testit "(substringGE  x  0  4  y  3  6)" false)

;;************************************************************************
;; Tests for the substringGT String Procedure
;;************************************************************************

(define  y  "My love")
(define  x  "Love My Mom")
(testit "(substringGT  y  0  1  x  5  6)" false)

(testit "(substringGT  x  0  4  y  3  6)" false)

;;************************************************************************
;; Tests for the substringCiLT String Procedure
;;************************************************************************

(define  y  "My love")
(define  x  "Love my Mom")
(testit "(substringCiLT  y  0  1  x  5  6)" false)

(testit "(substringCiLT  x  0  4  y  3  6)" false)

;;************************************************************************
;; Tests for the substringCiLE String Procedure
;;************************************************************************

(define  y  "My love")
(define  x  "Love my Mom")
(testit "(substringCiLE  y  0  1  x  5  6)" true)

(testit "(substringCiLE  x  0  3  y  3  6)" true)

;;************************************************************************
;; Tests for the substringCiEQ String Procedure
;;************************************************************************

(define  y  "My love")
(define  x  "Love my Mom")
(testit "(substringCiEQ  y  0  1  x  5  6)" true)

(testit "(substringCiEQ  x  0  4  y  3  6)" false)

;;************************************************************************
;; Tests for the substringCiNE String Procedure
;;************************************************************************

(define  y  "My love")
(define  x  "Love my Mom")
(testit "(substringCiNE  y  0  1  x  5  6)" false)

(testit "(substringCiNE  x  0  4  y  3  6)" true)

;;************************************************************************
;; Tests for the substringCiGE String Procedure
;;************************************************************************

(define  y  "My love")
(define  x  "Love my Mom")
(testit "(substringCiGE  y  0  1  x  5  6)" true)

(testit "(substringCiGE  x  0  4  y  3  6)" true)

;;************************************************************************
;; Tests for the substringCiGT String Procedure
;;************************************************************************

(define  y  "My love")
(define  x  "Love my Mom")
(testit "(substringCiGT  y  0  1  x  5  6)" false)

(testit "(substringCiGT  x  0  4  y  3  6)" true)

;;************************************************************************
;; Tests for the substringFill String Procedure
;;************************************************************************

(define  y  "My love")
(testit "(substringFill  y  0  1  #\\x)" "xx love")

(testit "(substringFill  y  3  7  #\\x)" "My xxxx")

;;************************************************************************
;; Tests for the text String Procedure
;;************************************************************************

(testit "(text  5732.279  \"#,##0\")" "5,732")

(testit "(text  -5732.279  \"#,##0.00000\")" "-5,732.27900")

(testit "(text  -5732.279  \"(#,##0.00000)\")" "(5,732.27900)")

(testit "(text  -5732.279  \"($#,##0.00000)\")" "($5,732.27900)")

(testit "(text  -.27  \"(00%)\")" "(27%)")

(testit "(text  -12.27  \"(%)\")" "(1227%)")

;;************************************************************************
;; Tests for the trim String Procedure
;;************************************************************************

(testit "(trim \"  this  is a     test\")" "this is a test")

;;************************************************************************
;; Tests for the upcase String Procedure
;;************************************************************************

(testit "(upcase \"this is a test\")" "THIS IS A TEST")

(testit "(charUpcase \"test\")" "TEST")

(testit "(stringUpcase #\\a)" #\A)


(testEnd "Test_String.sl")

