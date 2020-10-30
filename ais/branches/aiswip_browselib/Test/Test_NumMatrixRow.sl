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
;;  Title:    NumMatrixRow Procedures Test
;;
;;  Author:   Michael F. Korns
;;			  Franklin Chua
;;		      Tim May
;;
;;  Project:  AIS Regression Suite
;;
;;  Notes:    The SmartBase features in the NumMatrixRow Procedures
;;            chapter are tested in this test suite script.
;;
;;  Files:    RegTest.sl
;;

;#memory=500
;#memoryObjectHeaders=100
(runScript "RegTest.sl")
(testStart "Test_NumMatrixRow.sl")

;;************************************************************************
;;  1-Dimension NumMatrix
;;************************************************************************
(testit "(type (setq matrix1 (new Matrix: Number: 1 1)))" NumMatrix:)
(testit "(type (setq matrix2 (new Matrix: Number: 1 5)))" NumMatrix:)

(testit "(dimension matrix1)" 1)
(testit "(dimension matrix2)" 1)

(testit "(rank matrix1)[0]" 1)
(testit "(rank matrix2)[0]" 5)

(testit "(length matrix1)" 1)
(testit "(length matrix2)" 5)

(testit "(type (setq matrix1[0] 0))" NumMatrix:)
(testit "(type (setq matrix2[0] 0))" NumMatrix:)
(testit "(type (setq matrix2[1] 1))" NumMatrix:)
(testit "(type (setq matrix2[2] 2))" NumMatrix:)
(testit "(type (setq matrix2[3] 3))" NumMatrix:)
(testit "(type (setq matrix2[4] 4))" NumMatrix:)

(testit "(type matrix1[0])" Number:)
(testit "(type matrix2[0])" Number:)
(testit "(type matrix2[1])" Number:)
(testit "(type matrix2[2])" Number:)
(testit "(type matrix2[3])" Number:)
(testit "(type matrix2[4])" Number:)

(setq matrix1 #void)
(setq matrix2 #void)
(testit "(gc)" #void)

;;************************************************************************
;;  2-Dimension NumMatrix
;;************************************************************************
(testit "(type (setq matrix1 (new Matrix: Number: 2 2 2)))" NumMatrix:)
(testit "(type (setq matrix2 (new Matrix: Number: 2 4 4)))" NumMatrix:)

(testit "(dimension matrix1)" 2)
(testit "(dimension matrix2)" 2)

(testit "(rank matrix1)[0]" 2)
(testit "(rank matrix1)[1]" 2)
(testit "(rank matrix2)[0]" 4)
(testit "(rank matrix2)[1]" 4)

(testit "(type (setq mat1row1 matrix1[0]))" NumMatrixRow:)
(testit "(type (setq mat1row2 matrix1[1]))" NumMatrixRow:)
(testit "(type (setq mat2row1 matrix2[0]))" NumMatrixRow:)
(testit "(type (setq mat2row2 matrix2[1]))" NumMatrixRow:)
(testit "(type (setq mat2row3 matrix2[2]))" NumMatrixRow:)
(testit "(type (setq mat2row4 matrix2[3]))" NumMatrixRow:)

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

(testit "(string mat1row1 true)" "{NumMatrixRow| [2]| 0.0 0.0}")
(testit "(string mat1row2 true)" "{NumMatrixRow| [2]| 0.0 0.0}")
(testit "(string mat2row1 true)" "{NumMatrixRow| [4]| 0.0 0.0 0.0 0.0}")
(testit "(string mat2row2 true)" "{NumMatrixRow| [4]| 0.0 0.0 0.0 0.0}")
(testit "(string mat2row3 true)" "{NumMatrixRow| [4]| 0.0 0.0 0.0 0.0}")
(testit "(string mat2row4 true)" "{NumMatrixRow| [4]| 0.0 0.0 0.0 0.0}")

;; Set the value using NumMatrix, then retrieve using NumMatrixRow

(testit "(type (setq matrix1[0] 11))" NumMatrix:)
(testit "(type (setq matrix1[1] 12))" NumMatrix:)
(testit "(type (setq matrix1[2] 21))" NumMatrix:)
(testit "(type (setq matrix1[3] 22))" NumMatrix:)

(testit "(type (setq matrix2[0] 11))" NumMatrix:)
(testit "(type (setq matrix2[1] 12))" NumMatrix:)
(testit "(type (setq matrix2[2] 13))" NumMatrix:)
(testit "(type (setq matrix2[3] 14))" NumMatrix:)

(testit "(type (setq matrix2[4] 21))" NumMatrix:)
(testit "(type (setq matrix2[5] 22))" NumMatrix:)
(testit "(type (setq matrix2[6] 23))" NumMatrix:)
(testit "(type (setq matrix2[7] 24))" NumMatrix:)

(testit "(type (setq matrix2[8] 31))" NumMatrix:)
(testit "(type (setq matrix2[9] 32))" NumMatrix:)
(testit "(type (setq matrix2[10] 33))" NumMatrix:)
(testit "(type (setq matrix2[11] 34))" NumMatrix:)

(testit "(type (setq matrix2[12] 41))" NumMatrix:)
(testit "(type (setq matrix2[13] 42))" NumMatrix:)
(testit "(type (setq matrix2[14] 43))" NumMatrix:)
(testit "(type (setq matrix2[15] 44))" NumMatrix:)

(testit "mat1row1[0]" 11)
(testit "mat1row1[1]" 12)

(testit "mat1row2[0]" 21)
(testit "mat1row2[1]" 22)

(testit "mat2row1[0]" 11)
(testit "mat2row1[1]" 12)
(testit "mat2row1[2]" 13)
(testit "mat2row1[3]" 14)

(testit "mat2row2[0]" 21)
(testit "mat2row2[1]" 22)
(testit "mat2row2[2]" 23)
(testit "mat2row2[3]" 24)

(testit "mat2row3[0]" 31)
(testit "mat2row3[1]" 32)
(testit "mat2row3[2]" 33)
(testit "mat2row3[3]" 34)

(testit "mat2row4[0]" 41)
(testit "mat2row4[1]" 42)
(testit "mat2row4[2]" 43)
(testit "mat2row4[3]" 44)
{NumMatrixRow| [2]| 11.0 12.0}
(testit "(string mat1row1 true)" "{NumMatrixRow| [2]| 11.0 12.0}")
(testit "(string mat1row2 true)" "{NumMatrixRow| [2]| 21.0 22.0}")
(testit "(string mat2row1 true)" "{NumMatrixRow| [4]| 11.0 12.0 13.0 14.0}")
(testit "(string mat2row2 true)" "{NumMatrixRow| [4]| 21.0 22.0 23.0 24.0}")
(testit "(string mat2row3 true)" "{NumMatrixRow| [4]| 31.0 32.0 33.0 34.0}")
(testit "(string mat2row4 true)" "{NumMatrixRow| [4]| 41.0 42.0 43.0 44.0}")

;; Set the value using NumMatrixRow, then retrieve using NumMatrix
(testit "(type (setq mat1row1[0] 1001))" NumMatrixRow:)
(testit "(type (setq mat1row1[1] 1002))" NumMatrixRow:)
(testit "(type (setq mat1row2[0] 2001))" NumMatrixRow:)
(testit "(type (setq mat1row2[1] 2002))" NumMatrixRow:)

(testit "(type (setq mat2row1[0] 1001))" NumMatrixRow:)
(testit "(type (setq mat2row1[1] 1002))" NumMatrixRow:)
(testit "(type (setq mat2row1[2] 1003))" NumMatrixRow:)
(testit "(type (setq mat2row1[3] 1004))" NumMatrixRow:)

(testit "(type (setq mat2row2[0] 2001))" NumMatrixRow:)
(testit "(type (setq mat2row2[1] 2002))" NumMatrixRow:)
(testit "(type (setq mat2row2[2] 2003))" NumMatrixRow:)
(testit "(type (setq mat2row2[3] 2004))" NumMatrixRow:)

(testit "(type (setq mat2row3[0] 3001))" NumMatrixRow:)
(testit "(type (setq mat2row3[1] 3002))" NumMatrixRow:)
(testit "(type (setq mat2row3[2] 3003))" NumMatrixRow:)
(testit "(type (setq mat2row3[3] 3004))" NumMatrixRow:)

(testit "(type (setq mat2row4[0] 4001))" NumMatrixRow:)
(testit "(type (setq mat2row4[1] 4002))" NumMatrixRow:)
(testit "(type (setq mat2row4[2] 4003))" NumMatrixRow:)
(testit "(type (setq mat2row4[3] 4004))" NumMatrixRow:)

(testit "(string matrix1 true)" "#(nummat[2 2]| 1001.0 1002.0 2001.0 2002.0 )")
(testit "(string matrix2 true)" "#(nummat[4 4]| 1001.0 1002.0 1003.0 1004.0 2001.0 2002.0 2003.0 2004.0 3001.0 3002.0 3003.0 3004.0 4001.0 4002.0 4003.0 4004.0 )")

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
;;  3-Dimension NumMatrix
;;************************************************************************
(testit "(type (setq matrix1 (new Matrix: Number: 3 2 2 2)))" NumMatrix:)
(testit "(type (setq matrix2 (new Matrix: Number: 3 2 3 4)))" NumMatrix:)

(testit "(dimension matrix1)" 3)
(testit "(dimension matrix2)" 3)

(testit "(rank matrix1)[0]" 2)
(testit "(rank matrix1)[1]" 2)
(testit "(rank matrix1)[2]" 2)

(testit "(rank matrix2)[0]" 2)
(testit "(rank matrix2)[1]" 3)
(testit "(rank matrix2)[2]" 4)

;; 1st dimension NumMatrixRow
(testit "(type (setq mat1row1 matrix1[0]))" NumMatrixRow:)
(testit "(type (setq mat1row2 matrix1[1]))" NumMatrixRow:)

;; 2nd dimension NumMatrixRow
(testit "(type (setq mat1row1fld1 mat1row1[0]))" NumMatrixRow:)
(testit "(type (setq mat1row1fld2 mat1row1[1]))" NumMatrixRow:)
(testit "(type (setq mat1row2fld1 mat1row2[0]))" NumMatrixRow:)
(testit "(type (setq mat1row2fld2 mat1row2[1]))" NumMatrixRow:)

;; 1st dimension NumMatrixRow
(testit "(type (setq mat2row1 matrix2[0]))" NumMatrixRow:)
(testit "(type (setq mat2row2 matrix2[1]))" NumMatrixRow:)

;; 2nd dimension NumMatrixRow
(testit "(type (setq mat2row1fld1 mat2row1[0]))" NumMatrixRow:)
(testit "(type (setq mat2row1fld2 mat2row1[1]))" NumMatrixRow:)
(testit "(type (setq mat2row1fld3 mat2row1[2]))" NumMatrixRow:)

(testit "(type (setq mat2row2fld1 mat2row2[0]))" NumMatrixRow:)
(testit "(type (setq mat2row2fld2 mat2row2[1]))" NumMatrixRow:)
(testit "(type (setq mat2row2fld3 mat2row2[2]))" NumMatrixRow:)

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
(testit "(string mat1row1 true)" "{NumMatrixRow| [2 2]| 0.0 0.0 0.0 0.0}")
(testit "(string mat1row2 true)" "{NumMatrixRow| [2 2]| 0.0 0.0 0.0 0.0}")

(testit "(string mat2row1 true)" "{NumMatrixRow| [3 4]| 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0}")
(testit "(string mat2row2 true)" "{NumMatrixRow| [3 4]| 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0}")

(testit "(string mat1row1fld1 true)" "{NumMatrixRow| [2]| 0.0 0.0}")
(testit "(string mat1row1fld2 true)" "{NumMatrixRow| [2]| 0.0 0.0}")

(testit "(string mat1row2fld1 true)" "{NumMatrixRow| [2]| 0.0 0.0}")
(testit "(string mat1row2fld2 true)" "{NumMatrixRow| [2]| 0.0 0.0}")

(testit "(string mat2row1fld1 true)" "{NumMatrixRow| [4]| 0.0 0.0 0.0 0.0}")
(testit "(string mat2row1fld2 true)" "{NumMatrixRow| [4]| 0.0 0.0 0.0 0.0}")
(testit "(string mat2row1fld3 true)" "{NumMatrixRow| [4]| 0.0 0.0 0.0 0.0}")

(testit "(string mat2row2fld1 true)" "{NumMatrixRow| [4]| 0.0 0.0 0.0 0.0}")
(testit "(string mat2row2fld2 true)" "{NumMatrixRow| [4]| 0.0 0.0 0.0 0.0}")
(testit "(string mat2row2fld3 true)" "{NumMatrixRow| [4]| 0.0 0.0 0.0 0.0}")

;; Set the value using NumMatrix, then retrieve using NumMatrixRow

;; 1st Row of 1st NumMatrix
(testit "(type (setq matrix1[0] 111))" NumMatrix:)
(testit "(type (setq matrix1[1] 112))" NumMatrix:)
(testit "(type (setq matrix1[2] 121))" NumMatrix:)
(testit "(type (setq matrix1[3] 122))" NumMatrix:)

;; 2nd Row of 2nd NumMatrix
(testit "(type (setq matrix1[4] 211))" NumMatrix:)
(testit "(type (setq matrix1[5] 212))" NumMatrix:)
(testit "(type (setq matrix1[6] 221))" NumMatrix:)
(testit "(type (setq matrix1[7] 222))" NumMatrix:)

;; 1st Row of 2nd NumMatrix
(testit "(type (setq matrix2[0] 111))" NumMatrix:)
(testit "(type (setq matrix2[1] 112))" NumMatrix:)
(testit "(type (setq matrix2[2] 113))" NumMatrix:)
(testit "(type (setq matrix2[3] 114))" NumMatrix:)

(testit "(type (setq matrix2[4] 121))" NumMatrix:)
(testit "(type (setq matrix2[5] 122))" NumMatrix:)
(testit "(type (setq matrix2[6] 123))" NumMatrix:)
(testit "(type (setq matrix2[7] 124))" NumMatrix:)

(testit "(type (setq matrix2[8]  131))" NumMatrix:)
(testit "(type (setq matrix2[9]  132))" NumMatrix:)
(testit "(type (setq matrix2[10] 133))" NumMatrix:)
(testit "(type (setq matrix2[11] 134))" NumMatrix:)

;; 2nd Row of 2nd NumMatrix
(testit "(type (setq matrix2[12] 211))" NumMatrix:)
(testit "(type (setq matrix2[13] 212))" NumMatrix:)
(testit "(type (setq matrix2[14] 213))" NumMatrix:)
(testit "(type (setq matrix2[15] 214))" NumMatrix:)

(testit "(type (setq matrix2[16] 221))" NumMatrix:)
(testit "(type (setq matrix2[17] 222))" NumMatrix:)
(testit "(type (setq matrix2[18] 223))" NumMatrix:)
(testit "(type (setq matrix2[19] 224))" NumMatrix:)

(testit "(type (setq matrix2[20] 231))" NumMatrix:)
(testit "(type (setq matrix2[21] 232))" NumMatrix:)
(testit "(type (setq matrix2[22] 233))" NumMatrix:)
(testit "(type (setq matrix2[23] 234))" NumMatrix:)

;; 1st Row of 1st NumMatrix
(testit "mat1row1fld1[0]" 111)
(testit "mat1row1fld1[1]" 112)
(testit "mat1row1fld2[0]" 121)
(testit "mat1row1fld2[1]" 122)

;; 2nd Row of 1st NumMatrix
(testit "mat1row2fld1[0]" 211)
(testit "mat1row2fld1[1]" 212)
(testit "mat1row2fld2[0]" 221)
(testit "mat1row2fld2[1]" 222)

;; 1st Row of 2nd NumMatrix 
(testit "mat2row1fld1[0]" 111)
(testit "mat2row1fld1[1]" 112)
(testit "mat2row1fld1[2]" 113)
(testit "mat2row1fld1[3]" 114)

(testit "mat2row1fld2[0]" 121)
(testit "mat2row1fld2[1]" 122)
(testit "mat2row1fld2[2]" 123)
(testit "mat2row1fld2[3]" 124)

(testit "mat2row1fld3[0]" 131)
(testit "mat2row1fld3[1]" 132)
(testit "mat2row1fld3[2]" 133)
(testit "mat2row1fld3[3]" 134)

;; 2nd Row of 2nd NumMatrix
(testit "mat2row2fld1[0]" 211)
(testit "mat2row2fld1[1]" 212)
(testit "mat2row2fld1[2]" 213)
(testit "mat2row2fld1[3]" 214)

(testit "mat2row2fld2[0]" 221)
(testit "mat2row2fld2[1]" 222)
(testit "mat2row2fld2[2]" 223)
(testit "mat2row2fld2[3]" 224)

(testit "mat2row2fld3[0]" 231)
(testit "mat2row2fld3[1]" 232)
(testit "mat2row2fld3[2]" 233)
(testit "mat2row2fld3[3]" 234)

(testit "(string mat1row1 true)" "{NumMatrixRow| [2 2]| 111.0 112.0 121.0 122.0}")
(testit "(string mat1row2 true)" "{NumMatrixRow| [2 2]| 211.0 212.0 221.0 222.0}")

(testit "(string mat2row1 true)" "{NumMatrixRow| [3 4]| 111.0 112.0 113.0 114.0 121.0 122.0 123.0 124.0 131.0 132.0 133.0 134.0}")
(testit "(string mat2row2 true)" "{NumMatrixRow| [3 4]| 211.0 212.0 213.0 214.0 221.0 222.0 223.0 224.0 231.0 232.0 233.0 234.0}")

;; Set the value using NumMatrixRow
(testit "(type (setq mat1row1fld1[0] 10101))" NumMatrixRow:)
(testit "(type (setq mat1row1fld1[1] 10102))" NumMatrixRow:)
(testit "(type (setq mat1row1fld2[0] 10201))" NumMatrixRow:)
(testit "(type (setq mat1row1fld2[1] 10202))" NumMatrixRow:)

(testit "(type (setq mat1row2fld1[0] 20101))" NumMatrixRow:)
(testit "(type (setq mat1row2fld1[1] 20102))" NumMatrixRow:)
(testit "(type (setq mat1row2fld2[0] 20201))" NumMatrixRow:)
(testit "(type (setq mat1row2fld2[1] 20202))" NumMatrixRow:)


(testit "(type (setq mat2row1fld1[0] 10101))" NumMatrixRow:)
(testit "(type (setq mat2row1fld1[1] 10102))" NumMatrixRow:)
(testit "(type (setq mat2row1fld1[2] 10103))" NumMatrixRow:)
(testit "(type (setq mat2row1fld1[3] 10104))" NumMatrixRow:)

(testit "(type (setq mat2row1fld2[0] 10201))" NumMatrixRow:)
(testit "(type (setq mat2row1fld2[1] 10202))" NumMatrixRow:)
(testit "(type (setq mat2row1fld2[2] 10203))" NumMatrixRow:)
(testit "(type (setq mat2row1fld2[3] 10204))" NumMatrixRow:)

(testit "(type (setq mat2row1fld3[0] 10301))" NumMatrixRow:)
(testit "(type (setq mat2row1fld3[1] 10302))" NumMatrixRow:)
(testit "(type (setq mat2row1fld3[2] 10303))" NumMatrixRow:)
(testit "(type (setq mat2row1fld3[3] 10304))" NumMatrixRow:)

(testit "(type (setq mat2row2fld1[0] 20101))" NumMatrixRow:)
(testit "(type (setq mat2row2fld1[1] 20102))" NumMatrixRow:)
(testit "(type (setq mat2row2fld1[2] 20103))" NumMatrixRow:)
(testit "(type (setq mat2row2fld1[3] 20104))" NumMatrixRow:)

(testit "(type (setq mat2row2fld2[0] 20201))" NumMatrixRow:)
(testit "(type (setq mat2row2fld2[1] 20202))" NumMatrixRow:)
(testit "(type (setq mat2row2fld2[2] 20203))" NumMatrixRow:)
(testit "(type (setq mat2row2fld2[3] 20204))" NumMatrixRow:)

(testit "(type (setq mat2row2fld3[0] 20301))" NumMatrixRow:)
(testit "(type (setq mat2row2fld3[1] 20302))" NumMatrixRow:)
(testit "(type (setq mat2row2fld3[2] 20303))" NumMatrixRow:)
(testit "(type (setq mat2row2fld3[3] 20304))" NumMatrixRow:)

(testit "(string matrix1 true)" "#(nummat[2 2 2]| 10101.0 10102.0 10201.0 10202.0 20101.0 20102.0 20201.0 20202.0 )")
(testit "(string matrix2 true)" "#(nummat[2 3 4]| 10101.0 10102.0 10103.0 10104.0 10201.0 10202.0 10203.0 10204.0 10301.0 10302.0 10303.0 10304.0 20101.0 20102.0 20103.0 20104.0 20201.0 20202.0 20203.0 20204.0 20301.0 20302.0 20303.0 20304.0 )")

(testit "(string mat1row1 true)" "{NumMatrixRow| [2 2]| 10101.0 10102.0 10201.0 10202.0}")
(testit "(string mat1row2 true)" "{NumMatrixRow| [2 2]| 20101.0 20102.0 20201.0 20202.0}")

(testit "(string mat2row1 true)" "{NumMatrixRow| [3 4]| 10101.0 10102.0 10103.0 10104.0 10201.0 10202.0 10203.0 10204.0 10301.0 10302.0 10303.0 10304.0}")
(testit "(string mat2row2 true)" "{NumMatrixRow| [3 4]| 20101.0 20102.0 20103.0 20104.0 20201.0 20202.0 20203.0 20204.0 20301.0 20302.0 20303.0 20304.0}")

(testit "(string mat1row1fld1 true)" "{NumMatrixRow| [2]| 10101.0 10102.0}")
(testit "(string mat1row1fld2 true)" "{NumMatrixRow| [2]| 10201.0 10202.0}")

(testit "(string mat1row2fld1 true)" "{NumMatrixRow| [2]| 20101.0 20102.0}")
(testit "(string mat1row2fld2 true)" "{NumMatrixRow| [2]| 20201.0 20202.0}")

(testit "(string mat2row1fld1 true)" "{NumMatrixRow| [4]| 10101.0 10102.0 10103.0 10104.0}")
(testit "(string mat2row1fld2 true)" "{NumMatrixRow| [4]| 10201.0 10202.0 10203.0 10204.0}")
(testit "(string mat2row1fld3 true)" "{NumMatrixRow| [4]| 10301.0 10302.0 10303.0 10304.0}")

(testit "(string mat2row2fld1 true)" "{NumMatrixRow| [4]| 20101.0 20102.0 20103.0 20104.0}")
(testit "(string mat2row2fld2 true)" "{NumMatrixRow| [4]| 20201.0 20202.0 20203.0 20204.0}")
(testit "(string mat2row2fld3 true)" "{NumMatrixRow| [4]| 20301.0 20302.0 20303.0 20304.0}")

;; Save/Load Tests #1
(setq repo (new ObjectRepository: "NumMatrixRow.db" clear:))
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

(setq repo (new ObjectRepository: "NumMatrixRow.db"))
(testit "(type (setq mat1row1 repo.mat1row1))" NumMatrixRow:)
(testit "(type (setq mat1row2 repo.mat1row2))" NumMatrixRow:)

(testit "(type (setq mat1row1fld1 repo.mat1row1fld1))" NumMatrixRow:)
(testit "(type (setq mat1row1fld2 repo.mat1row1fld2))" NumMatrixRow:)
(testit "(type (setq mat1row2fld1 repo.mat1row2fld1))" NumMatrixRow:)
(testit "(type (setq mat1row2fld2 repo.mat1row2fld2))" NumMatrixRow:)

(testit "(type (setq mat2row1fld1 repo.mat2row1fld1))" NumMatrixRow:)
(testit "(type (setq mat2row1fld2 repo.mat2row1fld2))" NumMatrixRow:)
(testit "(type (setq mat2row1fld3 repo.mat2row1fld3))" NumMatrixRow:)
(testit "(type (setq mat2row2fld1 repo.mat2row2fld1))" NumMatrixRow:)
(testit "(type (setq mat2row2fld2 repo.mat2row2fld2))" NumMatrixRow:)
(testit "(type (setq mat2row2fld3 repo.mat2row2fld3))" NumMatrixRow:)

(testit "(string mat1row1 true)" "{NumMatrixRow| [2 2]| 10101.0 10102.0 10201.0 10202.0}")
(testit "(string mat1row2 true)" "{NumMatrixRow| [2 2]| 20101.0 20102.0 20201.0 20202.0}")

(testit "(string mat2row1 true)" "{NumMatrixRow| [3 4]| 10101.0 10102.0 10103.0 10104.0 10201.0 10202.0 10203.0 10204.0 10301.0 10302.0 10303.0 10304.0}")
(testit "(string mat2row2 true)" "{NumMatrixRow| [3 4]| 20101.0 20102.0 20103.0 20104.0 20201.0 20202.0 20203.0 20204.0 20301.0 20302.0 20303.0 20304.0}")

(testit "(string mat1row1fld1 true)" "{NumMatrixRow| [2]| 10101.0 10102.0}")
(testit "(string mat1row1fld2 true)" "{NumMatrixRow| [2]| 10201.0 10202.0}")

(testit "(string mat1row2fld1 true)" "{NumMatrixRow| [2]| 20101.0 20102.0}")
(testit "(string mat1row2fld2 true)" "{NumMatrixRow| [2]| 20201.0 20202.0}")

(testit "(string mat2row1fld1 true)" "{NumMatrixRow| [4]| 10101.0 10102.0 10103.0 10104.0}")
(testit "(string mat2row1fld2 true)" "{NumMatrixRow| [4]| 10201.0 10202.0 10203.0 10204.0}")
(testit "(string mat2row1fld3 true)" "{NumMatrixRow| [4]| 10301.0 10302.0 10303.0 10304.0}")

(testit "(string mat2row2fld1 true)" "{NumMatrixRow| [4]| 20101.0 20102.0 20103.0 20104.0}")
(testit "(string mat2row2fld2 true)" "{NumMatrixRow| [4]| 20201.0 20202.0 20203.0 20204.0}")
(testit "(string mat2row2fld3 true)" "{NumMatrixRow| [4]| 20301.0 20302.0 20303.0 20304.0}")

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
(setq repo (new ObjectRepository: "NumMatrixRow.db" clear:))

(setq matrix1 (new Matrix: Number: 3 2 2 2))
(setq matrix1[0][0][0] 1.0)
(setq matrix1[0][0][1] 2.0)
(setq matrix1[0][1][0] 3.0)
(setq matrix1[0][1][1] 4.0)
(setq matrix1[1][0][0] 5.0)
(setq matrix1[1][0][1] 6.0)
(setq matrix1[1][1][0] 7.0)
(setq matrix1[1][1][1] 8.0)

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

(setq repo (new ObjectRepository: "NumMatrixRow.db"))
(testit "(type (setq vector1 repo.vector1))" Vector:)

(testit "(type vector1[0])" NumMatrixRow:)
(testit "(type vector1[1])" NumMatrixRow:)
(testit "(type vector1[2])" NumMatrixRow:)
(testit "(type vector1[3])" NumMatrixRow:)
(testit "(type vector1[4])" NumMatrixRow:)
(testit "(type vector1[5])" NumMatrixRow:)

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
(setq matrix1 (new Matrix: Number: 3 2 2 2))
(setq matrix1[0][0][0] 1.0)
(setq matrix1[0][0][1] 2.0)
(setq matrix1[0][1][0] 3.0)
(setq matrix1[0][1][1] 4.0)
(setq matrix1[1][0][0] 5.0)
(setq matrix1[1][0][1] 6.0)
(setq matrix1[1][1][0] 7.0)
(setq matrix1[1][1][1] 8.0)

(setq matrix1row1 matrix1[0])
(setq matrix1row2 matrix1[1])

(testit "(type (setq byteVector1 (saveObject matrix1row1)))" ByteVector:)
(testit "(type (setq byteVector2 (saveObject matrix1row2)))" ByteVector:)

(setq matrix1 #void)
(setq matrix1row1 #void)
(setq matrix1row2 #void)
(testit "(gc compact:)" #void)

(testit "(type (setq matrix1row1 (loadObject byteVector1)))" NumMatrixRow:)
(testit "(type (setq matrix1row2 (loadObject byteVector2)))" NumMatrixRow:)

(testit "matrix1row1[0][0]" 1.0)
(testit "matrix1row1[0][1]" 2.0)
(testit "matrix1row1[1][0]" 3.0)
(testit "matrix1row1[1][1]" 4.0)
(testit "matrix1row2[0][0]" 5.0)
(testit "matrix1row2[0][1]" 6.0)
(testit "matrix1row2[1][0]" 7.0)
(testit "matrix1row2[1][1]" 8.0)

(setq matrix1row1 #void)
(setq matrix1row2 #void)
(setq byteVector1 #void)
(setq byteVector2 #void)

(testit "(gc compact:)" #void)

;; Save/Load Tests #4
(setq matrix1 (new Matrix: Number: 3 2 2 2))
(setq matrix1[0][0][0] 1.0)
(setq matrix1[0][0][1] 2.0)
(setq matrix1[0][1][0] 3.0)
(setq matrix1[0][1][1] 4.0)
(setq matrix1[1][0][0] 5.0)
(setq matrix1[1][0][1] 6.0)
(setq matrix1[1][1][0] 7.0)
(setq matrix1[1][1][1] 8.0)

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

(testit "(type (setq matrix1row1 (loadObject bufferPtr1)))" NumMatrixRow:)
(testit "(type (setq matrix1row2 (loadObject bufferPtr2)))" NumMatrixRow:)

(testit "matrix1row1[0][0]" 1.0)
(testit "matrix1row1[0][1]" 2.0)
(testit "matrix1row1[1][0]" 3.0)
(testit "matrix1row1[1][1]" 4.0)
(testit "matrix1row2[0][0]" 5.0)
(testit "matrix1row2[0][1]" 6.0)
(testit "matrix1row2[1][0]" 7.0)
(testit "matrix1row2[1][1]" 8.0)

(setq matrix1row1 #void)
(setq matrix1row2 #void)
(setq bufferPtr1 #void)
(setq bufferPtr2 #void)

(testit "(gc compact:)" #void)

;; Save/Load Tests #5
(setq fd1 (fileOpen "NumMatrixRow1.db" 1 4))
(setq fd2 (fileOpen "NumMatrixRow2.db" 1 4))
(setq matrix1 (new Matrix: Number: 3 2 2 2))
(setq matrix1[0][0][0] 1.0)
(setq matrix1[0][0][1] 2.0)
(setq matrix1[0][1][0] 3.0)
(setq matrix1[0][1][1] 4.0)
(setq matrix1[1][0][0] 5.0)
(setq matrix1[1][0][1] 6.0)
(setq matrix1[1][1][0] 7.0)
(setq matrix1[1][1][1] 8.0)

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

(setq fd1 (fileOpen "NumMatrixRow1.db" 0 4))
(setq fd2 (fileOpen "NumMatrixRow2.db" 0 4))

(testit "(type (setq matrix1row1 (loadObject fd1)))" NumMatrixRow:)
(testit "(type (setq matrix1row2 (loadObject fd2)))" NumMatrixRow:)

(testit "matrix1row1[0][0]" 1.0)
(testit "matrix1row1[0][1]" 2.0)
(testit "matrix1row1[1][0]" 3.0)
(testit "matrix1row1[1][1]" 4.0)
(testit "matrix1row2[0][0]" 5.0)
(testit "matrix1row2[0][1]" 6.0)
(testit "matrix1row2[1][0]" 7.0)
(testit "matrix1row2[1][1]" 8.0)

(fileClose fd1 0)
(fileClose fd2 0)
(setq matrix1row1 #void)
(setq matrix1row2 #void)

(testit "(gc compact:)" #void)

;;************************************************************************
;; NumMatrixRow Compare Procedure
;;************************************************************************
(setq matrix1 (new Matrix: Number: 2 2 2))
(setq matrix2 (new Matrix: Number: 3 2 2 2))
(setq matrix3 (new Matrix: Number: 2 2 4))
(setq matrix5 (new Matrix: Number: 2 2 2))

(setq matrix1[0][0] 1234)
(setq matrix1[0][1] 2345)
(setq matrix1[1][0] 3456)
(setq matrix1[1][1] 4567)

(setq matrix3[0][0] 1234)
(setq matrix3[0][1] 2345)
(setq matrix3[0][2] 3456)
(setq matrix3[0][3] 4567)
(setq matrix3[1][0] 5678)
(setq matrix3[1][1] 6789)
(setq matrix3[1][2] 7890)
(setq matrix3[1][3] 8901)

(setq matrix5[0][0] 1234)
(setq matrix5[0][1] 2345)
(setq matrix5[1][0] 3456)
(setq matrix5[1][1] 4567)

(setq matrix2[0][0][0] 1234)
(setq matrix2[0][0][1] 2345)
(setq matrix2[0][1][0] 3456)
(setq matrix2[0][1][1] 4567)
(setq matrix2[1][0][0] 5678)
(setq matrix2[1][0][1] 6789)
(setq matrix2[1][1][0] 7890)
(setq matrix2[1][1][1] 8901)

;; Compare two NumMatrixRow's of the same dimension and same length
(testit "(type (setq mat1row1 matrix1[0]))" NumMatrixRow:)
(testit "(type (setq mat1row2 matrix1[1]))" NumMatrixRow:)
(testit "(type (setq mat5row1 matrix5[0]))" NumMatrixRow:)
(testit "(type (setq mat5row2 matrix5[1]))" NumMatrixRow:)

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

;; Compare two NumMatrixRow's of the same dimension but different length
(testit "(type (setq mat3row1 matrix3[0]))" NumMatrixRow:)
(testit "(type (setq mat3row2 matrix3[1]))" NumMatrixRow:)

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

;; Compare two NumMatrixRow's of different dimension but same length
(testit "(type (setq mat2row1fld1 matrix2[0][0]))" NumMatrixRow:)
(testit "(type (setq mat2row1fld2 matrix2[0][1]))" NumMatrixRow:)
(testit "(type (setq mat2row2fld1 matrix2[1][0]))" NumMatrixRow:)
(testit "(type (setq mat2row2fld2 matrix2[1][1]))" NumMatrixRow:)

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

(testit "(gc)" #void)


(testEnd "Test_NumMatrixRow.sl")