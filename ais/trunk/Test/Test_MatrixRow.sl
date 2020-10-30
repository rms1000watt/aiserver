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
;;  Title:    Lisp MatrixRow Procedures Test
;;
;;  Author:   Michael F. Korns
;;			  Franklin Chua
;;			  Tim May
;;
;;  Project:  AIS Regression Suite
;;
;;  Notes:    The SmartBase features in the MatrixRow Procedures
;;            chapter are tested in this test suite script.
;;
;;  Files:    RegTest.sl
;;

;#memory=500
;#memoryObjectHeaders=100
(runScript "RegTest.sl")
(testStart "Test_MatrixRow.sl")

;;************************************************************************
;;  1-Dimension Matrix
;;************************************************************************
(testit "(type (setq matrix1 (new Matrix: 1 1)))" Matrix:)
(testit "(type (setq matrix2 (new Matrix: 1 5)))" Matrix:)

(testit "(dimension matrix1)" 1)
(testit "(dimension matrix2)" 1)

(testit "(rank matrix1)[0]" 1)
(testit "(rank matrix2)[0]" 5)

(testit "(length matrix1)" 1)
(testit "(length matrix2)" 5)

(testit "(type (setq matrix1[0] \"0\"))" Matrix:)
(testit "(type (setq matrix2[0] \"0\"))" Matrix:)
(testit "(type (setq matrix2[1] \"1\"))" Matrix:)
(testit "(type (setq matrix2[2] \"2\"))" Matrix:)
(testit "(type (setq matrix2[3] \"3\"))" Matrix:)
(testit "(type (setq matrix2[4] \"4\"))" Matrix:)

(testit "(type matrix1[0])" Text:)
(testit "(type matrix2[0])" Text:)
(testit "(type matrix2[1])" Text:)
(testit "(type matrix2[2])" Text:)
(testit "(type matrix2[3])" Text:)
(testit "(type matrix2[4])" Text:)

(setq matrix1 #void)
(setq matrix2 #void)
(testit "(gc)" #void)

;;************************************************************************
;;  2-Dimension Matrix
;;************************************************************************
(testit "(type (setq matrix1 (new Matrix: 2 2 2)))" Matrix:)
(testit "(type (setq matrix2 (new Matrix: 2 4 4)))" Matrix:)

(testit "(dimension matrix1)" 2)
(testit "(dimension matrix2)" 2)

(testit "(rank matrix1)[0]" 2)
(testit "(rank matrix1)[1]" 2)
(testit "(rank matrix2)[0]" 4)
(testit "(rank matrix2)[1]" 4)

(testit "(type (setq mat1row1 matrix1[0]))" MatrixRow:)
(testit "(type (setq mat1row2 matrix1[1]))" MatrixRow:)
(testit "(type (setq mat2row1 matrix2[0]))" MatrixRow:)
(testit "(type (setq mat2row2 matrix2[1]))" MatrixRow:)
(testit "(type (setq mat2row3 matrix2[2]))" MatrixRow:)
(testit "(type (setq mat2row4 matrix2[3]))" MatrixRow:)

(testit "(dimension mat1row1)" 1)
(testit "(dimension mat1row2)" 1)
(testit "(dimension mat2row1)" 1)
(testit "(dimension mat2row2)" 1)
(testit "(dimension mat2row3)" 1)
(testit "(dimension mat2row4)" 1)

(testit "(length mat1row1)" 2)
(testit "(length mat1row2)" 2)
(testit "(length mat2row1)" 4)
(testit "(length mat2row2)" 4)
(testit "(length mat2row3)" 4)
(testit "(length mat2row4)" 4)

(testit "(string mat1row1 true)" "{MatrixRow[2]| #void #void}")
(testit "(string mat1row2 true)" "{MatrixRow[2]| #void #void}")
(testit "(string mat2row1 true)" "{MatrixRow[4]| #void #void #void #void}")
(testit "(string mat2row2 true)" "{MatrixRow[4]| #void #void #void #void}")
(testit "(string mat2row3 true)" "{MatrixRow[4]| #void #void #void #void}")
(testit "(string mat2row4 true)" "{MatrixRow[4]| #void #void #void #void}")

;; Set the value using Matrix, then retrieve using MatrixRow

(testit "(type (setq matrix1[0] \"0:0\"))" Matrix:)
(testit "(type (setq matrix1[1] \"0:1\"))" Matrix:)
(testit "(type (setq matrix1[2] \"1:0\"))" Matrix:)
(testit "(type (setq matrix1[3] \"1:1\"))" Matrix:)

(testit "(type (setq matrix2[0] \"0:0\"))" Matrix:)
(testit "(type (setq matrix2[1] \"0:1\"))" Matrix:)
(testit "(type (setq matrix2[2] \"0:2\"))" Matrix:)
(testit "(type (setq matrix2[3] \"0:3\"))" Matrix:)

(testit "(type (setq matrix2[4] \"1:0\"))" Matrix:)
(testit "(type (setq matrix2[5] \"1:1\"))" Matrix:)
(testit "(type (setq matrix2[6] \"1:2\"))" Matrix:)
(testit "(type (setq matrix2[7] \"1:3\"))" Matrix:)

(testit "(type (setq matrix2[8] \"2:0\"))" Matrix:)
(testit "(type (setq matrix2[9] \"2:1\"))" Matrix:)
(testit "(type (setq matrix2[10] \"2:2\"))" Matrix:)
(testit "(type (setq matrix2[11] \"2:3\"))" Matrix:)

(testit "(type (setq matrix2[12] \"3:0\"))" Matrix:)
(testit "(type (setq matrix2[13] \"3:1\"))" Matrix:)
(testit "(type (setq matrix2[14] \"3:2\"))" Matrix:)
(testit "(type (setq matrix2[15] \"3:3\"))" Matrix:)

(testit "mat1row1[0]" "0:0")
(testit "mat1row1[1]" "0:1")

(testit "mat1row2[0]" "1:0")
(testit "mat1row2[1]" "1:1")

(testit "mat2row1[0]" "0:0")
(testit "mat2row1[1]" "0:1")
(testit "mat2row1[2]" "0:2")
(testit "mat2row1[3]" "0:3")

(testit "mat2row2[0]" "1:0")
(testit "mat2row2[1]" "1:1")
(testit "mat2row2[2]" "1:2")
(testit "mat2row2[3]" "1:3")

(testit "mat2row3[0]" "2:0")
(testit "mat2row3[1]" "2:1")
(testit "mat2row3[2]" "2:2")
(testit "mat2row3[3]" "2:3")

(testit "mat2row4[0]" "3:0")
(testit "mat2row4[1]" "3:1")
(testit "mat2row4[2]" "3:2")
(testit "mat2row4[3]" "3:3")

(testit "(string mat1row1 true)" "{MatrixRow[2]| \"0:0\" \"0:1\"}")
(testit "(string mat1row2 true)" "{MatrixRow[2]| \"1:0\" \"1:1\"}")
(testit "(string mat2row1 true)" "{MatrixRow[4]| \"0:0\" \"0:1\" \"0:2\" \"0:3\"}")
(testit "(string mat2row2 true)" "{MatrixRow[4]| \"1:0\" \"1:1\" \"1:2\" \"1:3\"}")
(testit "(string mat2row3 true)" "{MatrixRow[4]| \"2:0\" \"2:1\" \"2:2\" \"2:3\"}")
(testit "(string mat2row4 true)" "{MatrixRow[4]| \"3:0\" \"3:1\" \"3:2\" \"3:3\"}")

;; Set the value using MatrixRow, then retrieve using Matrix
(testit "(type (setq mat1row1[0] \"A:0\"))" MatrixRow:)
(testit "(type (setq mat1row1[1] \"A:1\"))" MatrixRow:)
(testit "(type (setq mat1row2[0] \"B:0\"))" MatrixRow:)
(testit "(type (setq mat1row2[1] \"B:1\"))" MatrixRow:)

(testit "(type (setq mat2row1[0] \"A:0\"))" MatrixRow:)
(testit "(type (setq mat2row1[1] \"A:1\"))" MatrixRow:)
(testit "(type (setq mat2row1[2] \"A:2\"))" MatrixRow:)
(testit "(type (setq mat2row1[3] \"A:3\"))" MatrixRow:)

(testit "(type (setq mat2row2[0] \"B:0\"))" MatrixRow:)
(testit "(type (setq mat2row2[1] \"B:1\"))" MatrixRow:)
(testit "(type (setq mat2row2[2] \"B:2\"))" MatrixRow:)
(testit "(type (setq mat2row2[3] \"B:3\"))" MatrixRow:)

(testit "(type (setq mat2row3[0] \"C:0\"))" MatrixRow:)
(testit "(type (setq mat2row3[1] \"C:1\"))" MatrixRow:)
(testit "(type (setq mat2row3[2] \"C:2\"))" MatrixRow:)
(testit "(type (setq mat2row3[3] \"C:3\"))" MatrixRow:)

(testit "(type (setq mat2row4[0] \"D:0\"))" MatrixRow:)
(testit "(type (setq mat2row4[1] \"D:1\"))" MatrixRow:)
(testit "(type (setq mat2row4[2] \"D:2\"))" MatrixRow:)
(testit "(type (setq mat2row4[3] \"D:3\"))" MatrixRow:)

(testit "(string matrix1 true)" "#(mat[2 2]| \"A:0\" \"A:1\" \"B:0\" \"B:1\" )")
(testit "(string matrix2 true)" "#(mat[4 4]| \"A:0\" \"A:1\" \"A:2\" \"A:3\" \"B:0\" \"B:1\" \"B:2\" \"B:3\" \"C:0\" \"C:1\" \"C:2\" \"C:3\" \"D:0\" \"D:1\" \"D:2\" \"D:3\" )")

(setq matrix1 #void)
(setq matrix2 #void)

(setq mat1row1 #void)
(setq mat1row2 #void)
(setq mat2row1 #void)
(setq mat2row2 #void)
(setq mat2row3 #void)
(setq mat2row4 #void)

(testit "(gc)" #void)

;;************************************************************************
;;  3-Dimension Matrix
;;************************************************************************
(testit "(type (setq matrix1 (new Matrix: 3 2 2 2)))" Matrix:)
(testit "(type (setq matrix2 (new Matrix: 3 2 3 4)))" Matrix:)

(testit "(dimension matrix1)" 3)
(testit "(dimension matrix2)" 3)

(testit "(rank matrix1)[0]" 2)
(testit "(rank matrix1)[1]" 2)
(testit "(rank matrix1)[2]" 2)

(testit "(rank matrix2)[0]" 2)
(testit "(rank matrix2)[1]" 3)
(testit "(rank matrix2)[2]" 4)

;; 1st dimension MatrixRow
(testit "(type (setq mat1row1 matrix1[0]))" MatrixRow:)
(testit "(type (setq mat1row2 matrix1[1]))" MatrixRow:)

;; 2nd dimension MatrixRow
(testit "(type (setq mat1row1fld1 mat1row1[0]))" MatrixRow:)
(testit "(type (setq mat1row1fld2 mat1row1[1]))" MatrixRow:)
(testit "(type (setq mat1row2fld1 mat1row2[0]))" MatrixRow:)
(testit "(type (setq mat1row2fld2 mat1row2[1]))" MatrixRow:)

;; 1st dimension MatrixRow
(testit "(type (setq mat2row1 matrix2[0]))" MatrixRow:)
(testit "(type (setq mat2row2 matrix2[1]))" MatrixRow:)

;; 2nd dimension MatrixRow
(testit "(type (setq mat2row1fld1 mat2row1[0]))" MatrixRow:)
(testit "(type (setq mat2row1fld2 mat2row1[1]))" MatrixRow:)
(testit "(type (setq mat2row1fld3 mat2row1[2]))" MatrixRow:)

(testit "(type (setq mat2row2fld1 mat2row2[0]))" MatrixRow:)
(testit "(type (setq mat2row2fld2 mat2row2[1]))" MatrixRow:)
(testit "(type (setq mat2row2fld3 mat2row2[2]))" MatrixRow:)

;; Dimension tests
(testit "(dimension mat1row1)" 2)
(testit "(dimension mat1row2)" 2)

(testit "(dimension mat2row1)" 2)
(testit "(dimension mat2row2)" 2)

(testit "(dimension mat1row1fld1)" 1)
(testit "(dimension mat1row1fld2)" 1)

(testit "(dimension mat1row2fld1)" 1)
(testit "(dimension mat1row2fld2)" 1)

(testit "(dimension mat2row1fld1)" 1)
(testit "(dimension mat2row1fld2)" 1)
(testit "(dimension mat2row1fld3)" 1)

(testit "(dimension mat2row2fld1)" 1)
(testit "(dimension mat2row2fld2)" 1)
(testit "(dimension mat2row2fld3)" 1)

;; Length tests
(testit "(length mat1row1)" 4)
(testit "(length mat1row2)" 4)

(testit "(length mat2row1)" 12)
(testit "(length mat2row2)" 12)

(testit "(length mat1row1fld1)" 2)
(testit "(length mat1row1fld2)" 2)

(testit "(length mat1row2fld1)" 2)
(testit "(length mat1row2fld2)" 2)

(testit "(length mat2row1fld1)" 4)
(testit "(length mat2row1fld2)" 4)
(testit "(length mat2row1fld3)" 4)

(testit "(length mat2row2fld1)" 4)
(testit "(length mat2row2fld2)" 4)
(testit "(length mat2row2fld3)" 4)

;; String conversion tests
(testit "(string mat1row1 true)" "{MatrixRow[2 2]| #void #void #void #void}")
(testit "(string mat1row2 true)" "{MatrixRow[2 2]| #void #void #void #void}")

(testit "(string mat2row1 true)" "{MatrixRow[3 4]| #void #void #void #void #void #void #void #void #void #void #void #void}")
(testit "(string mat2row2 true)" "{MatrixRow[3 4]| #void #void #void #void #void #void #void #void #void #void #void #void}")

(testit "(string mat1row1fld1 true)" "{MatrixRow[2]| #void #void}")
(testit "(string mat1row1fld2 true)" "{MatrixRow[2]| #void #void}")

(testit "(string mat1row2fld1 true)" "{MatrixRow[2]| #void #void}")
(testit "(string mat1row2fld2 true)" "{MatrixRow[2]| #void #void}")

(testit "(string mat2row1fld1 true)" "{MatrixRow[4]| #void #void #void #void}")
(testit "(string mat2row1fld2 true)" "{MatrixRow[4]| #void #void #void #void}")
(testit "(string mat2row1fld3 true)" "{MatrixRow[4]| #void #void #void #void}")

(testit "(string mat2row2fld1 true)" "{MatrixRow[4]| #void #void #void #void}")
(testit "(string mat2row2fld2 true)" "{MatrixRow[4]| #void #void #void #void}")
(testit "(string mat2row2fld3 true)" "{MatrixRow[4]| #void #void #void #void}")

;; Set the value using Matrix, then retrieve using MatrixRow

;; 1st Row of 1st Matrix
(testit "(type (setq matrix1[0] \"0:0:0\"))" Matrix:)
(testit "(type (setq matrix1[1] \"0:0:1\"))" Matrix:)
(testit "(type (setq matrix1[2] \"0:1:0\"))" Matrix:)
(testit "(type (setq matrix1[3] \"0:1:1\"))" Matrix:)

;; 2nd Row of 2nd Matrix
(testit "(type (setq matrix1[4] \"1:0:0\"))" Matrix:)
(testit "(type (setq matrix1[5] \"1:0:1\"))" Matrix:)
(testit "(type (setq matrix1[6] \"1:1:0\"))" Matrix:)
(testit "(type (setq matrix1[7] \"1:1:1\"))" Matrix:)

;; 1st Row of 2nd Matrix
(testit "(type (setq matrix2[0] \"0:0:0\"))" Matrix:)
(testit "(type (setq matrix2[1] \"0:0:1\"))" Matrix:)
(testit "(type (setq matrix2[2] \"0:0:2\"))" Matrix:)
(testit "(type (setq matrix2[3] \"0:0:3\"))" Matrix:)
(testit "(type (setq matrix2[4] \"0:1:0\"))" Matrix:)
(testit "(type (setq matrix2[5] \"0:1:1\"))" Matrix:)
(testit "(type (setq matrix2[6] \"0:1:2\"))" Matrix:)
(testit "(type (setq matrix2[7] \"0:1:3\"))" Matrix:)
(testit "(type (setq matrix2[8]  \"0:2:0\"))" Matrix:)
(testit "(type (setq matrix2[9]  \"0:2:1\"))" Matrix:)
(testit "(type (setq matrix2[10] \"0:2:2\"))" Matrix:)
(testit "(type (setq matrix2[11] \"0:2:3\"))" Matrix:)

;; 2nd Row of 2nd Matrix
(testit "(type (setq matrix2[12] \"1:0:0\"))" Matrix:)
(testit "(type (setq matrix2[13] \"1:0:1\"))" Matrix:)
(testit "(type (setq matrix2[14] \"1:0:2\"))" Matrix:)
(testit "(type (setq matrix2[15] \"1:0:3\"))" Matrix:)
(testit "(type (setq matrix2[16] \"1:1:0\"))" Matrix:)
(testit "(type (setq matrix2[17] \"1:1:1\"))" Matrix:)
(testit "(type (setq matrix2[18] \"1:1:2\"))" Matrix:)
(testit "(type (setq matrix2[19] \"1:1:3\"))" Matrix:)
(testit "(type (setq matrix2[20] \"1:2:0\"))" Matrix:)
(testit "(type (setq matrix2[21] \"1:2:1\"))" Matrix:)
(testit "(type (setq matrix2[22] \"1:2:2\"))" Matrix:)
(testit "(type (setq matrix2[23] \"1:2:3\"))" Matrix:)

;; 1st Row of 1st Matrix
(testit "mat1row1fld1[0]" "0:0:0")
(testit "mat1row1fld1[1]" "0:0:1")
(testit "mat1row1fld2[0]" "0:1:0")
(testit "mat1row1fld2[1]" "0:1:1")

;; 2nd Row of 1st Matrix
(testit "mat1row2fld1[0]" "1:0:0")
(testit "mat1row2fld1[1]" "1:0:1")
(testit "mat1row2fld2[0]" "1:1:0")
(testit "mat1row2fld2[1]" "1:1:1")

;; 1st Row of 2nd Matrix 
(testit "mat2row1fld1[0]" "0:0:0")
(testit "mat2row1fld1[1]" "0:0:1")
(testit "mat2row1fld1[2]" "0:0:2")
(testit "mat2row1fld1[3]" "0:0:3")

(testit "mat2row1fld2[0]" "0:1:0")
(testit "mat2row1fld2[1]" "0:1:1")
(testit "mat2row1fld2[2]" "0:1:2")
(testit "mat2row1fld2[3]" "0:1:3")

(testit "mat2row1fld3[0]" "0:2:0")
(testit "mat2row1fld3[1]" "0:2:1")
(testit "mat2row1fld3[2]" "0:2:2")
(testit "mat2row1fld3[3]" "0:2:3")

;; 2nd Row of 2nd Matrix
(testit "mat2row2fld1[0]" "1:0:0")
(testit "mat2row2fld1[1]" "1:0:1")
(testit "mat2row2fld1[2]" "1:0:2")
(testit "mat2row2fld1[3]" "1:0:3")

(testit "mat2row2fld2[0]" "1:1:0")
(testit "mat2row2fld2[1]" "1:1:1")
(testit "mat2row2fld2[2]" "1:1:2")
(testit "mat2row2fld2[3]" "1:1:3")

(testit "mat2row2fld3[0]" "1:2:0")
(testit "mat2row2fld3[1]" "1:2:1")
(testit "mat2row2fld3[2]" "1:2:2")
(testit "mat2row2fld3[3]" "1:2:3")

(testit "(string mat1row1 true)" "{MatrixRow[2 2]| \"0:0:0\" \"0:0:1\" \"0:1:0\" \"0:1:1\"}")
(testit "(string mat1row2 true)" "{MatrixRow[2 2]| \"1:0:0\" \"1:0:1\" \"1:1:0\" \"1:1:1\"}")

(testit "(string mat2row1 true)" "{MatrixRow[3 4]| \"0:0:0\" \"0:0:1\" \"0:0:2\" \"0:0:3\" \"0:1:0\" \"0:1:1\" \"0:1:2\" \"0:1:3\" \"0:2:0\" \"0:2:1\" \"0:2:2\" \"0:2:3\"}")
(testit "(string mat2row2 true)" "{MatrixRow[3 4]| \"1:0:0\" \"1:0:1\" \"1:0:2\" \"1:0:3\" \"1:1:0\" \"1:1:1\" \"1:1:2\" \"1:1:3\" \"1:2:0\" \"1:2:1\" \"1:2:2\" \"1:2:3\"}")

;; Set the value using MatrixRow
(testit "(type (setq mat1row1fld1[0] \"A:A:A\"))" MatrixRow:)
(testit "(type (setq mat1row1fld1[1] \"A:A:B\"))" MatrixRow:)
(testit "(type (setq mat1row1fld2[0] \"A:B:A\"))" MatrixRow:)
(testit "(type (setq mat1row1fld2[1] \"A:B:B\"))" MatrixRow:)

(testit "(type (setq mat1row2fld1[0] \"B:A:A\"))" MatrixRow:)
(testit "(type (setq mat1row2fld1[1] \"B:A:B\"))" MatrixRow:)
(testit "(type (setq mat1row2fld2[0] \"B:B:A\"))" MatrixRow:)
(testit "(type (setq mat1row2fld2[1] \"B:B:B\"))" MatrixRow:)


(testit "(type (setq mat2row1fld1[0] \"A:A:A\"))" MatrixRow:)
(testit "(type (setq mat2row1fld1[1] \"A:A:B\"))" MatrixRow:)
(testit "(type (setq mat2row1fld1[2] \"A:A:C\"))" MatrixRow:)
(testit "(type (setq mat2row1fld1[3] \"A:A:D\"))" MatrixRow:)

(testit "(type (setq mat2row1fld2[0] \"A:B:A\"))" MatrixRow:)
(testit "(type (setq mat2row1fld2[1] \"A:B:B\"))" MatrixRow:)
(testit "(type (setq mat2row1fld2[2] \"A:B:C\"))" MatrixRow:)
(testit "(type (setq mat2row1fld2[3] \"A:B:D\"))" MatrixRow:)

(testit "(type (setq mat2row1fld3[0] \"A:C:A\"))" MatrixRow:)
(testit "(type (setq mat2row1fld3[1] \"A:C:B\"))" MatrixRow:)
(testit "(type (setq mat2row1fld3[2] \"A:C:C\"))" MatrixRow:)
(testit "(type (setq mat2row1fld3[3] \"A:C:D\"))" MatrixRow:)

(testit "(type (setq mat2row2fld1[0] \"B:A:A\"))" MatrixRow:)
(testit "(type (setq mat2row2fld1[1] \"B:A:B\"))" MatrixRow:)
(testit "(type (setq mat2row2fld1[2] \"B:A:C\"))" MatrixRow:)
(testit "(type (setq mat2row2fld1[3] \"B:A:D\"))" MatrixRow:)

(testit "(type (setq mat2row2fld2[0] \"B:B:A\"))" MatrixRow:)
(testit "(type (setq mat2row2fld2[1] \"B:B:B\"))" MatrixRow:)
(testit "(type (setq mat2row2fld2[2] \"B:B:C\"))" MatrixRow:)
(testit "(type (setq mat2row2fld2[3] \"B:B:D\"))" MatrixRow:)

(testit "(type (setq mat2row2fld3[0] \"B:C:A\"))" MatrixRow:)
(testit "(type (setq mat2row2fld3[1] \"B:C:B\"))" MatrixRow:)
(testit "(type (setq mat2row2fld3[2] \"B:C:C\"))" MatrixRow:)
(testit "(type (setq mat2row2fld3[3] \"B:C:D\"))" MatrixRow:)

(testit "(string matrix1 true)" {#(mat[2 2 2]| "A:A:A" "A:A:B" "A:B:A" "A:B:B" "B:A:A" "B:A:B" "B:B:A" "B:B:B" )})
(testit "(string matrix2 true)" {#(mat[2 3 4]| "A:A:A" "A:A:B" "A:A:C" "A:A:D" "A:B:A" "A:B:B" "A:B:C" "A:B:D" "A:C:A" "A:C:B" "A:C:C" "A:C:D" "B:A:A" "B:A:B" "B:A:C" "B:A:D" "B:B:A" "B:B:B" "B:B:C" "B:B:D" "B:C:A" "B:C:B" "B:C:C" "B:C:D" )})

(testit "(string mat1row1 true)" "{MatrixRow[2 2]| \"A:A:A\" \"A:A:B\" \"A:B:A\" \"A:B:B\"}")
(testit "(string mat1row2 true)" "{MatrixRow[2 2]| \"B:A:A\" \"B:A:B\" \"B:B:A\" \"B:B:B\"}")

(testit "(string mat2row1 true)" "{MatrixRow[3 4]| \"A:A:A\" \"A:A:B\" \"A:A:C\" \"A:A:D\" \"A:B:A\" \"A:B:B\" \"A:B:C\" \"A:B:D\" \"A:C:A\" \"A:C:B\" \"A:C:C\" \"A:C:D\"}")
(testit "(string mat2row2 true)" "{MatrixRow[3 4]| \"B:A:A\" \"B:A:B\" \"B:A:C\" \"B:A:D\" \"B:B:A\" \"B:B:B\" \"B:B:C\" \"B:B:D\" \"B:C:A\" \"B:C:B\" \"B:C:C\" \"B:C:D\"}")

(testit "(string mat1row1fld1 true)" "{MatrixRow[2]| \"A:A:A\" \"A:A:B\"}")
(testit "(string mat1row1fld2 true)" "{MatrixRow[2]| \"A:B:A\" \"A:B:B\"}")

(testit "(string mat1row2fld1 true)" "{MatrixRow[2]| \"B:A:A\" \"B:A:B\"}")
(testit "(string mat1row2fld2 true)" "{MatrixRow[2]| \"B:B:A\" \"B:B:B\"}")

(testit "(string mat2row1fld1 true)" "{MatrixRow[4]| \"A:A:A\" \"A:A:B\" \"A:A:C\" \"A:A:D\"}")
(testit "(string mat2row1fld2 true)" "{MatrixRow[4]| \"A:B:A\" \"A:B:B\" \"A:B:C\" \"A:B:D\"}")
(testit "(string mat2row1fld3 true)" "{MatrixRow[4]| \"A:C:A\" \"A:C:B\" \"A:C:C\" \"A:C:D\"}")

(testit "(string mat2row2fld1 true)" "{MatrixRow[4]| \"B:A:A\" \"B:A:B\" \"B:A:C\" \"B:A:D\"}")
(testit "(string mat2row2fld2 true)" "{MatrixRow[4]| \"B:B:A\" \"B:B:B\" \"B:B:C\" \"B:B:D\"}")
(testit "(string mat2row2fld3 true)" "{MatrixRow[4]| \"B:C:A\" \"B:C:B\" \"B:C:C\" \"B:C:D\"}")

;; Save/Load Tests #1
(setq repo (new ObjectRepository: "MatrixRow.db" clear:))
(testit "(type (setq repo.mat1row1 mat1row1))" ObjectRepository:)
(testit "(type (setq repo.mat1row2 mat1row2))" ObjectRepository:)

(testit "(type (setq repo.mat1row1fld1 mat1row1fld1))" ObjectRepository:)
(testit "(type (setq repo.mat1row1fld2 mat1row1fld2))" ObjectRepository:)

(testit "(type (setq repo.mat1row2fld1 mat1row2fld1))" ObjectRepository:)
(testit "(type (setq repo.mat1row2fld2 mat1row2fld2))" ObjectRepository:)

(testit "(type (setq repo.mat2row1fld1 mat2row1fld1))" ObjectRepository:)
(testit "(type (setq repo.mat2row1fld2 mat2row1fld2))" ObjectRepository:)
(testit "(type (setq repo.mat2row1fld3 mat2row1fld3))" ObjectRepository:)

(testit "(type (setq repo.mat2row2fld1 mat2row2fld1))" ObjectRepository:)
(testit "(type (setq repo.mat2row2fld2 mat2row2fld2))" ObjectRepository:)
(testit "(type (setq repo.mat2row2fld3 mat2row2fld3))" ObjectRepository:)

(testit "(setq matrix1 #void)" #void)
(testit "(setq matrix2 #void)" #void)
(testit "(setq mat1row1 #void)" #void)
(testit "(setq mat1row2 #void)" #void)

(testit "(setq mat1row1fld1 #void)" #void)
(testit "(setq mat1row1fld1 #void)" #void)
(testit "(setq mat1row2fld1 #void)" #void)
(testit "(setq mat1row2fld1 #void)" #void)

(testit "(setq mat2row1fld1 #void)" #void)
(testit "(setq mat2row1fld2 #void)" #void)
(testit "(setq mat2row1fld3 #void)" #void)
(testit "(setq mat2row2fld1 #void)" #void)
(testit "(setq mat2row2fld2 #void)" #void)
(testit "(setq mat2row2fld3 #void)" #void)

(testit "(setq repo #void)" #void)

(testit "(gc compact:)" #void)

(setq repo (new ObjectRepository: "MatrixRow.db"))
(testit "(type (setq mat1row1 repo.mat1row1))" MatrixRow:)
(testit "(type (setq mat1row2 repo.mat1row2))" MatrixRow:)

(testit "(type (setq mat1row1fld1 repo.mat1row1fld1))" MatrixRow:)
(testit "(type (setq mat1row1fld2 repo.mat1row1fld2))" MatrixRow:)
(testit "(type (setq mat1row2fld1 repo.mat1row2fld1))" MatrixRow:)
(testit "(type (setq mat1row2fld2 repo.mat1row2fld2))" MatrixRow:)

(testit "(type (setq mat2row1fld1 repo.mat2row1fld1))" MatrixRow:)
(testit "(type (setq mat2row1fld2 repo.mat2row1fld2))" MatrixRow:)
(testit "(type (setq mat2row1fld3 repo.mat2row1fld3))" MatrixRow:)
(testit "(type (setq mat2row2fld1 repo.mat2row2fld1))" MatrixRow:)
(testit "(type (setq mat2row2fld2 repo.mat2row2fld2))" MatrixRow:)
(testit "(type (setq mat2row2fld3 repo.mat2row2fld3))" MatrixRow:)

(testit "(string mat1row1 true)" "{MatrixRow[2 2]| \"A:A:A\" \"A:A:B\" \"A:B:A\" \"A:B:B\"}")
(testit "(string mat1row2 true)" "{MatrixRow[2 2]| \"B:A:A\" \"B:A:B\" \"B:B:A\" \"B:B:B\"}")

(testit "(string mat1row1fld1 true)" "{MatrixRow[2]| \"A:A:A\" \"A:A:B\"}")
(testit "(string mat1row1fld2 true)" "{MatrixRow[2]| \"A:B:A\" \"A:B:B\"}")

(testit "(string mat1row2fld1 true)" "{MatrixRow[2]| \"B:A:A\" \"B:A:B\"}")
(testit "(string mat1row2fld2 true)" "{MatrixRow[2]| \"B:B:A\" \"B:B:B\"}")

(testit "(string mat2row1fld1 true)" "{MatrixRow[4]| \"A:A:A\" \"A:A:B\" \"A:A:C\" \"A:A:D\"}")
(testit "(string mat2row1fld2 true)" "{MatrixRow[4]| \"A:B:A\" \"A:B:B\" \"A:B:C\" \"A:B:D\"}")
(testit "(string mat2row1fld3 true)" "{MatrixRow[4]| \"A:C:A\" \"A:C:B\" \"A:C:C\" \"A:C:D\"}")

(testit "(string mat2row2fld1 true)" "{MatrixRow[4]| \"B:A:A\" \"B:A:B\" \"B:A:C\" \"B:A:D\"}")
(testit "(string mat2row2fld2 true)" "{MatrixRow[4]| \"B:B:A\" \"B:B:B\" \"B:B:C\" \"B:B:D\"}")
(testit "(string mat2row2fld3 true)" "{MatrixRow[4]| \"B:C:A\" \"B:C:B\" \"B:C:C\" \"B:C:D\"}")

(setq matrix1 #void)
(setq matrix2 #void)

(setq mat1row1 #void)
(setq mat1row2 #void)
(setq mat2row1 #void)
(setq mat2row2 #void)

(setq mat1row1fld1 #void)
(setq mat1row1fld2 #void)

(setq mat1row2fld1 #void)
(setq mat1row2fld2 #void)

(setq mat2row1fld1 #void)
(setq mat2row1fld2 #void)
(setq mat2row1fld3 #void)

(setq mat2row2fld1 #void)
(setq mat2row2fld2 #void)
(setq mat2row2fld3 #void)

(setq repo #void)

(testit "(gc compact:)" #void)

;; Save/Load Tests #2
(setq repo (new ObjectRepository: "MatrixRow.db" clear:))

(setq matrix1 (new Matrix: 3 2 2 2))
(setq matrix1[0][0][0] 1)
(setq matrix1[0][0][1] 2)
(setq matrix1[0][1][0] 3)
(setq matrix1[0][1][1] 4)
(setq matrix1[1][0][0] 5)
(setq matrix1[1][0][1] 6)
(setq matrix1[1][1][0] 7)
(setq matrix1[1][1][1] 8)

(setq vector1 (new Vector:))
(setq vector1[0] matrix1[0])
(setq vector1[1] matrix1[1])
(setq vector1[2] matrix1[0][0])
(setq vector1[3] matrix1[0][1])
(setq vector1[4] matrix1[1][0])
(setq vector1[5] matrix1[1][1])

(testit "(type (setq repo.vector1 vector1))" ObjectRepository:)

(setq repo #void)
(setq matrix1 #void)
(setq vector1 #void)

(testit "(gc compact:)" #void)

(setq repo (new ObjectRepository: "MatrixRow.db"))
(testit "(type (setq vector1 repo.vector1))" Vector:)

(testit "(type vector1[0])" MatrixRow:)
(testit "(type vector1[1])" MatrixRow:)
(testit "(type vector1[2])" MatrixRow:)
(testit "(type vector1[3])" MatrixRow:)
(testit "(type vector1[4])" MatrixRow:)
(testit "(type vector1[5])" MatrixRow:)

(testit "vector1[0][0][0]" 1)
(testit "vector1[0][0][1]" 2)
(testit "vector1[0][1][0]" 3)
(testit "vector1[0][1][1]" 4)
(testit "vector1[1][0][0]" 5)
(testit "vector1[1][0][1]" 6)
(testit "vector1[1][1][0]" 7)
(testit "vector1[1][1][1]" 8)
(testit "vector1[2][0]" 1)
(testit "vector1[2][1]" 2)
(testit "vector1[3][0]" 3)
(testit "vector1[3][1]" 4)
(testit "vector1[4][0]" 5)
(testit "vector1[4][1]" 6)
(testit "vector1[5][0]" 7)
(testit "vector1[5][1]" 8)

(setq repo #void)
(setq matrix1 #void)
(setq vector1 #void)

(testit "(gc compact:)" #void)

;; Save/Load Tests #3
(setq matrix1 (new Matrix: 3 2 2 2))
(setq matrix1[0][0][0] 1)
(setq matrix1[0][0][1] 2)
(setq matrix1[0][1][0] 3)
(setq matrix1[0][1][1] 4)
(setq matrix1[1][0][0] 5)
(setq matrix1[1][0][1] 6)
(setq matrix1[1][1][0] 7)
(setq matrix1[1][1][1] 8)

(setq matrix1row1 matrix1[0])
(setq matrix1row2 matrix1[1])

(testit "(type (setq byteVector1 (saveObject matrix1row1)))" ByteVector:)
(testit "(type (setq byteVector2 (saveObject matrix1row2)))" ByteVector:)

(setq matrix1 #void)
(setq matrix1row1 #void)
(setq matrix1row2 #void)
(testit "(gc compact:)" #void)

(testit "(type (setq matrix1row1 (loadObject byteVector1)))" MatrixRow:)
(testit "(type (setq matrix1row2 (loadObject byteVector2)))" MatrixRow:)

(testit "matrix1row1[0][0]" 1)
(testit "matrix1row1[0][1]" 2)
(testit "matrix1row1[1][0]" 3)
(testit "matrix1row1[1][1]" 4)
(testit "matrix1row2[0][0]" 5)
(testit "matrix1row2[0][1]" 6)
(testit "matrix1row2[1][0]" 7)
(testit "matrix1row2[1][1]" 8)

(setq matrix1row1 #void)
(setq matrix1row2 #void)
(setq byteVector1 #void)
(setq byteVector2 #void)

(testit "(gc compact:)" #void)

;; Save/Load Tests #4
(setq matrix1 (new Matrix: 3 2 2 2))
(setq matrix1[0][0][0] 1)
(setq matrix1[0][0][1] 2)
(setq matrix1[0][1][0] 3)
(setq matrix1[0][1][1] 4)
(setq matrix1[1][0][0] 5)
(setq matrix1[1][0][1] 6)
(setq matrix1[1][1][0] 7)
(setq matrix1[1][1][1] 8)

(setq matrix1row1 matrix1[0])
(setq matrix1row2 matrix1[1])

(setq bufferSize1 (saveObject 0 matrix1row1 false))
(setq bufferSize2 (saveObject 0 matrix1row2 false))

(setq bufferPtr1 (createBuffer bufferSize1))
(setq bufferPtr2 (createBuffer bufferSize2))

(testit "(type (saveObject matrix1row1 bufferSize1 bufferPtr1))" Integer:)
(testit "(type (saveObject matrix1row2 bufferSize2 bufferPtr2))" Integer:)

(setq matrix1 #void)
(setq matrix1row1 #void)
(setq matrix1row2 #void)
(testit "(gc compact:)" #void)

(testit "(type (setq matrix1row1 (loadObject bufferPtr1)))" MatrixRow:)
(testit "(type (setq matrix1row2 (loadObject bufferPtr2)))" MatrixRow:)

(testit "matrix1row1[0][0]" 1)
(testit "matrix1row1[0][1]" 2)
(testit "matrix1row1[1][0]" 3)
(testit "matrix1row1[1][1]" 4)
(testit "matrix1row2[0][0]" 5)
(testit "matrix1row2[0][1]" 6)
(testit "matrix1row2[1][0]" 7)
(testit "matrix1row2[1][1]" 8)

(setq matrix1row1 #void)
(setq matrix1row2 #void)
(setq bufferPtr1 #void)
(setq bufferPtr2 #void)

(testit "(gc compact:)" #void)

;; Save/Load Tests #5
(setq fd1 (fileOpen "MatrixRow1.db" 1 4))
(setq fd2 (fileOpen "MatrixRow2.db" 1 4))
(setq matrix1 (new Matrix: 3 2 2 2))
(setq matrix1[0][0][0] 1)
(setq matrix1[0][0][1] 2)
(setq matrix1[0][1][0] 3)
(setq matrix1[0][1][1] 4)
(setq matrix1[1][0][0] 5)
(setq matrix1[1][0][1] 6)
(setq matrix1[1][1][0] 7)
(setq matrix1[1][1][1] 8)

(setq matrix1row1 matrix1[0])
(setq matrix1row2 matrix1[1])

(testit "(type (saveObject fd1 matrix1row1))" Integer:)
(testit "(type (saveObject fd2 matrix1row2))" Integer:)

(fileClose fd1 1)
(fileClose fd2 1)

(setq fd1 #void)
(setq fd2 #void)
(setq matrix1 #void)
(setq matrix1row1 #void)
(setq matrix1row2 #void)
(testit "(gc compact:)" #void)

(setq fd1 (fileOpen "MatrixRow1.db" 0 4))
(setq fd2 (fileOpen "MatrixRow2.db" 0 4))

(testit "(type (setq matrix1row1 (loadObject fd1)))" MatrixRow:)
(testit "(type (setq matrix1row2 (loadObject fd2)))" MatrixRow:)

(testit "matrix1row1[0][0]" 1)
(testit "matrix1row1[0][1]" 2)
(testit "matrix1row1[1][0]" 3)
(testit "matrix1row1[1][1]" 4)
(testit "matrix1row2[0][0]" 5)
(testit "matrix1row2[0][1]" 6)
(testit "matrix1row2[1][0]" 7)
(testit "matrix1row2[1][1]" 8)

(fileClose fd1 0)
(fileClose fd2 0)
(setq matrix1row1 #void)
(setq matrix1row2 #void)

(testit "(gc compact:)" #void)

;;************************************************************************
;; MatrixRow Compare Procedure
;;************************************************************************
(setq matrix1 (new Matrix: 2 2 2))
(setq matrix2 (new Matrix: 3 2 2 2))
(setq matrix3 (new Matrix: 2 2 4))
(setq matrix5 (new Matrix: 2 2 2))

(setq matrix1[0][0] "ABC")
(setq matrix1[0][1] "DEF")
(setq matrix1[1][0] "GHI")
(setq matrix1[1][1] "JKL")

(setq matrix3[0][0] "ABC")
(setq matrix3[0][1] "DEF")
(setq matrix3[0][2] "GHI")
(setq matrix3[0][3] "JKL")
(setq matrix3[1][0] "MNO")
(setq matrix3[1][1] "PQR")
(setq matrix3[1][2] "STU")
(setq matrix3[1][3] "VWX")

(setq matrix5[0][0] "ABC")
(setq matrix5[0][1] "DEF")
(setq matrix5[1][0] "GHI")
(setq matrix5[1][1] "JKL")

(setq matrix2[0][0][0] "ABC")
(setq matrix2[0][0][1] "DEF")
(setq matrix2[0][1][0] "GHI")
(setq matrix2[0][1][1] "JKL")
(setq matrix2[1][0][0] "MNO")
(setq matrix2[1][0][1] "PQR")
(setq matrix2[1][1][0] "STU")
(setq matrix2[1][1][1] "VWX")

;; Compare two MatrixRow's of the same dimension and same length
(testit "(type (setq mat1row1 matrix1[0]))" MatrixRow:)
(testit "(type (setq mat1row2 matrix1[1]))" MatrixRow:)
(testit "(type (setq mat5row1 matrix5[0]))" MatrixRow:)
(testit "(type (setq mat5row2 matrix5[1]))" MatrixRow:)

(testit "(length mat1row1)" 2)
(testit "(length mat1row2)" 2)
(testit "(length mat5row1)" 2)
(testit "(length mat5row2)" 2)

(testit "(compare mat1row1 mat1row2)" -1)
(testit "(compare mat1row2 mat1row1)" 1)
(testit "(compare mat1row1 mat5row1)" 0)

(testit "(compare mat5row1 mat1row1)" 0)
(testit "(compare mat1row1 mat5row2)" -1)
(testit "(compare mat5row2 mat1row1)" 1)

;; Compare two MatrixRow's of the same dimension but different length
(testit "(type (setq mat3row1 matrix3[0]))" MatrixRow:)
(testit "(type (setq mat3row2 matrix3[1]))" MatrixRow:)

(testit "(length mat1row1)" 2)
(testit "(length mat1row2)" 2)
(testit "(length mat3row1)" 4)
(testit "(length mat3row2)" 4)

(testit "(compare mat1row1 mat3row1)" -1)
(testit "(compare mat1row1 mat3row2)" -1)
(testit "(compare mat3row1 mat1row1)" 1)
(testit "(compare mat3row2 mat1row1)" 1)

(testit "(compare mat1row2 mat3row1)" 1)
(testit "(compare mat1row2 mat3row2)" -1)
(testit "(compare mat3row1 mat1row2)" -1)
(testit "(compare mat3row2 mat1row2)" 1)

;; Compare two MatrixRow's of different dimension but same length
(testit "(type (setq mat2row1fld1 matrix2[0][0]))" MatrixRow:)
(testit "(type (setq mat2row1fld2 matrix2[0][1]))" MatrixRow:)
(testit "(type (setq mat2row2fld1 matrix2[1][0]))" MatrixRow:)
(testit "(type (setq mat2row2fld2 matrix2[1][1]))" MatrixRow:)

(testit "(length mat2row1fld1)" 2)
(testit "(length mat2row1fld2)" 2)
(testit "(length mat2row2fld1)" 2)
(testit "(length mat2row2fld2)" 2)

(testit "(compare mat1row1 mat2row1fld1)" 0)
(testit "(compare mat1row1 mat2row1fld2)" -1)
(testit "(compare mat1row1 mat2row2fld1)" -1)
(testit "(compare mat1row1 mat2row2fld2)" -1)
(testit "(compare mat1row2 mat2row1fld1)" 1)
(testit "(compare mat1row2 mat2row1fld2)" 0)
(testit "(compare mat1row2 mat2row2fld1)" -1)
(testit "(compare mat1row2 mat2row2fld2)" -1)

(testit "(compare mat2row1fld1 mat1row1)" 0)
(testit "(compare mat2row1fld2 mat1row1)" 1)
(testit "(compare mat2row2fld1 mat1row1)" 1)
(testit "(compare mat2row2fld2 mat1row1)" 1)
(testit "(compare mat2row1fld1 mat1row2)" -1)
(testit "(compare mat2row1fld2 mat1row2)" 0)
(testit "(compare mat2row2fld1 mat1row2)" 1)
(testit "(compare mat2row2fld2 mat1row2)" 1)

(testit "(gc compact:)" #void)

(testEnd "Test_MatrixRow.sl")