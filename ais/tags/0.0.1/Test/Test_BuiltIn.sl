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
;;  Title:    SmartLisp Builtin Procedures Test
;;
;;  Author:   Michael F. Korns, Tim May
;;
;;  Project:  AIS Regression Suite
;;
;;  Notes:    The SmartBase features in the Builtin Procedures
;;            chapter are tested in this test suite script.
;;
;;  Files:    RegTest.sl
;;

;#memory=500
;#memoryObjectHeaders=100
(runScript "RegTest.sl")
(testStart "Test_Builtin.sl")

;;************************************************************************
;; Tests for the 1+ builtin procedure
;;************************************************************************

(testit "(1+ 12)" 13)

(testit "(add1 -4)" -3)

(testit "(1+ 0)" 1)

(define x 1+)
(testit "(x 0)" 1)

;;************************************************************************
;; Tests for the -1+ builtin procedure
;;************************************************************************

(testit "(-1+ 12)" 11)

(testit "(sub1 -4)" -5)

(testit "(-1+ 0)" -1)

(define x -1+ )
(testit "(x 0)" -1)

;;************************************************************************
;; Tests for the * builtin procedure
;;************************************************************************

(testit "(* 2 3 4 )" 24)

(testit "(*)" 1)

(testit "(* 2)" 2)

(define x *)
(testit "(x 2)" 2)

;;************************************************************************
;; Tests for the + builtin procedure
;;************************************************************************

(testit "(+ 2 3 4 )" 9)

(testit "(+)" 0)

(testit "(+ 2)" 2)

(define x +)
(testit "(x 2)" 2)

;;************************************************************************
;; Tests for the - builtin procedure
;;************************************************************************

(testit "(- 2 3 4 )" -5)

(testit "(- 1 2)" -1)

(testit "(- 2)" -2)

(define x -)
(testit "(x 2)" -2)

;;************************************************************************
;; Tests for the / builtin procedure
;;************************************************************************

(testit "(/ 8 4 2 )" 1)

(testit "(/ 1 2)" .5)

(testit "(/ 2)" .5)

(define x /)
(testit "(x 2)" .5)

;;************************************************************************
;; Tests for the < builtin procedure
;;************************************************************************

(testit "(< 8 4)" false)

(testit "(< 1 2)" true)

(testit "(charLT 8 4)" false)

(testit "(stringLT 1 2)" true)

(define x <)
(testit "(x 1 2)" true)

;;************************************************************************
;; Tests for the <= builtin procedure
;;************************************************************************

(testit "(<= 8 4)" false)

(testit "(<= 1 1)" true)

(testit "(charLE 8 4)" false)

(testit "(stringLE 1 2)" true)

(define x <=)
(testit "(x 1 2)" true)

;;************************************************************************
;; Tests for the >= builtin procedure
;;************************************************************************

(testit "(>= 8 4)" true)

(testit "(>= 1 1)" true)

(testit "(charGE 8 4)" true)

(testit "(stringGE 1 2)" false)

(define x >=)
(testit "(x 1 2)" false)

;;************************************************************************
;; Tests for the <> builtin procedure
;;************************************************************************

(testit "(<> 8 4)" true)

(testit "(<> 1 1)" false)

(testit "(charNE 8 4)" true)

(testit "(stringNE 1 2)" true)

(define x <>)
(testit "(x 1 2)" true)

;;************************************************************************
;; Tests for the <> builtin procedure
;;************************************************************************

(testit "(<> 8 4)" true)

(testit "(<> 1 1)" false)

(testit "(charNE 8 4)" true)

(testit "(stringNE 1 2)" true)

(define x <>)
(testit "(x 1 2)" true)

;;************************************************************************
;; Tests for the = builtin procedure
;;************************************************************************

(testit "(= 8 4)" false)

(testit "(= 1 1)" true)

(testit "(charEQ 8 4)" false)

(testit "(stringEQ 1 1.0)" true)

(testit "(isEq 8 4)" false)

(testit "(isEqv 1 1.0)" true)

(define x =)
(testit "(x 1.1 1.1)" true)

;;************************************************************************
;; Tests for the addi builtin procedure
;;************************************************************************

(testit "(addi 8.1 4)" 12)

(testit "(addi -1 1)" 0)

(define x addi)
(testit "(x 1.1 1.1)" 2)

;;************************************************************************
;; Tests for the bitwiseAnd builtin procedure
;;************************************************************************

(testit "(bitwiseAnd (=  12  12)  (>  2  1))" 1)

(testit "(bitwiseAnd (=  24  23)  (<  1  2))" 0)

(define x bitwiseAnd)
(testit "(x 8 8 8)" 8)

;;************************************************************************
;; Tests for the binaryNand builtin procedure
;;************************************************************************

(testit "(binaryNand (=  12  12)  (>  2  1))" 0)

(testit "(binaryNand (=  24  23)  (<  1  2))" 1)

(testit "(binaryNand 8  8)" 0)

(define x binaryNand)
(testit "(x 8 8)" 0)

;;************************************************************************
;; Tests for the bitwiseNand builtin procedure
;;************************************************************************

(testit "(bitwiseNand (=  12  12)  (>  2  1))" -2)

(testit "(bitwiseNand (=  24  23)  (<  1  2))" -1)

(testit "(bitwiseNand 8  8)" -9)

(define x bitwiseNand)
(testit "(x 8 8)" -9)

;;************************************************************************
;; Tests for the binaryNor builtin procedure
;;************************************************************************

(testit "(binaryNor (=  12  12)  (>  2  1))" 0)

(testit "(binaryNor (=  24  23)  (<  2  2))" 1)

(testit "(binaryNor 0  0)" 1)

(define x binaryNor)
(testit "(x false false)" 1)

;;************************************************************************
;; Tests for the bitwiseNor builtin procedure
;;************************************************************************

(testit "(bitwiseNor (=  12  12)  (>  2  1))" -2)

(testit "(bitwiseNor (=  24  23)  (<  2  2))" -1)

(testit "(bitwiseNor 0  0)" -1)

(define x bitwiseNor)
(testit "(x false false)" -1)

;;************************************************************************
;; Tests for the binaryNxor builtin procedure
;;************************************************************************

(testit "(binaryNxor (=  12  12)  (>  2  1))" 1)

(testit "(binaryNxor (=  24  23)  (<  1  2))" 0)

(testit "(binaryNxor 0  0)" 1)

(define x binaryNxor)
(testit "(x false false)" 1)

;;************************************************************************
;; Tests for the bitwiseNxor builtin procedure
;;************************************************************************

(testit "(bitwiseNxor (=  12  12)  (>  2  1))" -1)

(testit "(bitwiseNxor (=  24  23)  (<  1  2))" -2)

(testit "(bitwiseNxor 0  0)" -1)

(define x bitwiseNxor)
(testit "(x false false)" -1)

;;************************************************************************
;; Tests for the bitwiseOr builtin procedure
;;************************************************************************

(testit "(bitwiseOr (=  12  12)  (>  2  1))" 1)

(testit "(bitwiseOr (=  24  23)  (<  1  2))" 1)

(testit "(bitwiseOr 0  0)" 0)

(define x bitwiseOr)
(testit "(x false true)" 1)

;;************************************************************************
;; Tests for the bitwiseShiftLeft builtin procedure
;;************************************************************************

(testit "(bitwiseShiftLeft 4  2)" 16)

(testit "(bitwiseShiftLeft 5  1)" 10)

(testit "(bitwiseShiftLeft 0  1)" 0)

(define x bitwiseShiftLeft)
(testit "(x true 2)" 4)

;;************************************************************************
;; Tests for the bitwiseShiftRight builtin procedure
;;************************************************************************

(testit "(bitwiseShiftRight 4  2)" 1)

(testit "(bitwiseShiftRight 5  1)" 2)

(testit "(bitwiseShiftRight 0  1)" 0)

(define x bitwiseShiftRight)
(testit "(x true 1)" 0)

;;************************************************************************
;; Tests for the bitwiseXor builtin procedure
;;************************************************************************

(testit "(bitwiseXor 4  2)" 6)

(testit "(bitwiseXor 5  1)" 4)

(testit "(bitwiseXor 1  1)" 0)

(define x bitwiseXor)
(testit "(x true false)" 1)

;;************************************************************************
;; Tests for the divi builtin procedure
;;************************************************************************

(testit "(divi 4  2)" 2)

(testit "(divi 5.1  2.1)" 2)

(testit "(quotient 2.2  1.1)" 2)

(define x divi)
(testit "(x 2.3 1.1)" 2)

;;************************************************************************
;; Tests for the mod builtin procedure
;;************************************************************************

(testit "(mod 4  2)" 0)

(testit "(modulo 5  2)" 1)

(testit "(remainder 3  2)" 1)

(define x mod)
(testit "(x 4 2)" 0)

;;************************************************************************
;; Tests for the modi builtin procedure
;;************************************************************************

(testit "(modi 12  5.3)" 2)

(testit "(modi  8.4  -4)" 0)

(testit "(modi  21  4.4)" 1)

(define x modi)
(testit "(x 21  4.4)" 1)

;;************************************************************************
;; Tests for the muli builtin procedure
;;************************************************************************

(testit "(muli  2  3.6  4)" 24)

(testit "(muli  2  -4.1)" -8)

(testit "(muli  4  5)" 20)

(define x muli)
(testit "(x 4  5)" 20)

;;************************************************************************
;; Tests for the nil builtin variable
;;************************************************************************

(testit "nil" #void)

;;************************************************************************
;; Tests for the pi builtin procedure
;;************************************************************************

(testit "(cos (pi))" -1)

;;************************************************************************
;; Tests for the ref builtin procedure
;;************************************************************************

(testit "(ref #(1 2 3) 1)" 2)

(testit "(listRef #(1 2 3) 2)" 3)

(testit "(stringRef \"Hello\" 2)" #\l)

(testit "(nth \"Hello\" 0)" #\H)

(testit "(vectorRef #(1 2 4) 1)" 2)

(define x ref)
(testit "(x #{a: 1 b: 2 c: 3} 1 1)" 2)

;;************************************************************************
;; Tests for the set builtin procedure
;;************************************************************************

(testit "(set  x:  3.3)" 3.3)

(define x #(1 2 3))
(testit "(ref (set x  0  3)  0)" 3)

(testit "(sum (setNth  x  1  -4))" 2)

(testit "(setCellArray  x:  6)" 6)

(testit "(stringSet (makeString \"Hello\") 0 #\\h)" "hello")

(testit "(ref (vectorSet #(1 2 3) 2 5) 2)" 5)

(define x set)
(testit "(ref (x #(1 2 3) 2 5) 2)" 5)

(testit "(set  Y:  (*  3  4))" 12)

(testit "(set  #(1  2  3)  2  (*  3  4))" #(1 2 12))

(testit "(set  '(1  2  3)  2  (*  3  4))" '(1  2  12))

(testit "(set  (makeString \"Hello\")  0  #\\h)" "hello")

(testit "(set  #{A: 1  B: 2  C: 3}  A:  (*  3  4))" #{A: 12  B: 2  C: 3})

(testit "(set (if (= x 10) d: c:) 12)" 12)

(testit "(set  #{A:  1   B:  2   C:  8}  0 0  'D)" #{D:  1   B:  2  C:  8})

(testit "(set  #{A:  1   B:  2   C:  8}  2  1  5)" #{A:  1   B:  2  C:  5})

;;************************************************************************
;; Tests for the subi builtin procedure
;;************************************************************************

(testit "(subi  12  3.3  4)" 5)

(testit "(subi  2  -4)" 6)

(testit "(subi  -42.4  6)" -48)

(define x subi)
(testit "(x 4  5)" -1)

;;************************************************************************
;; Tests for the Vectors with assigned attributes
;;************************************************************************

(setq v #(1 2 3 4 5))
(setq av (new Vector: object: 5 a: b: c: d: e: f:))
(setAttributes v av)

(testit "v.c" 3)

(setq v.d 10)
(testit "v.d" 10)

(testit "(refAttributes v)" av)

(setAttributes v #void)
(testit "(refAttributes v)" #void)

(testEnd "Test_Builtin.sl")



