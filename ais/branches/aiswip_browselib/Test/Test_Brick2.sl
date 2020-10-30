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
;;  Title:    Lisp Brick2 Procedures Test
;;
;;  Author:   Michael F. Korns
;;			  Franklin Chua
;;			  Tim May
;;
;;  Project:  AIS Regression Suite
;;
;;  Notes:    The SmartBase features in the Brick Procedures
;;            chapter are tested in this test suite script.
;;
;;  Files:    RegTest.sl
;;

;#memory=500
;#memoryObjectHeaders=100
(runScript "RegTest.sl")
(testStart "Test_Brick2.sl")

;;************************************************************************
;; Additional tests for the Brick Procedure
;; New Brick Data Types: Date and Money
;; New Brick Sub Types: BrickRow, BrickField, Substring
;;************************************************************************

(writeln scriptName "step [004] started")

;; Single Brick Behavior
(testit "(type (setq myBrick (new Brick: 1 Name:Character:20 Code:Integer:2 Salary:Money:1 Title:Character:20 Gender:Character:1)))" Brick:)

(testit {myBrick["RowCount"]} 1)
(testit {myBrick["RowByteCount"]} (if (= _ais.bits 64) 65 57))
(testit {myBrick["FieldCount"]} 5)
(testit {myBrick["ByteCount"]} (if (= _ais.bits 64) 65 57))

;; Single index access (set) to a single-row Brick
(testit "(type (setq myBrick[0] \"John Doe\"))" Brick:)
(testit "myBrick[0][0]" "John Doe")
(testit "myBrick[0].Name" "John Doe")

;; Single index access (get) to a single-row Brick
;; Multi-repeat character fields will return a Substring type
(testit "(type myBrick[0])" BrickRow:)
(testit "(type myBrick.Name)" Substring:)

;; Single index access (set) to a single-row Brick
;; Multi-repeat Integer field, the first repeat item will be set
(testit "(type (setq myBrick[0][1][0] 1001))" BrickField:)
(testit "myBrick[0][1][0]" 1001)

;; Single index access (get) to a single-row Brick
;; Multi-repeat Integer fields will return a BrickField type
(testit "(type myBrick[0][1])" BrickField:)

(testit "(type myBrick[0][1][0])" Integer:)

(testit "(type (setq myBrick[0][1][1] 2002))" BrickField:)
(testit "myBrick[0][1][1]" 2002)

(testit "(type (setq myBrick[0][2] 195.75))" BrickRow:)
(testit "(type myBrick[0].Salary)" Money:)
(testit "(type myBrick[0][2])" Money:)

(testit "(type (setq myBrick[0].Title \"Victim\"))" BrickRow:)
(testit "(setq myTitle myBrick[0].Title)" "Victim")

;; Single-repeat character fields will return a Character type
(testit "(type (setq myBrick[0].Gender #\\M))" BrickRow:)
(testit "(type myBrick[0].Gender)" Character:)

;; Brick to Ascii conversion for single-row Brick
(testit "(string myBrick true)" "#(brk(Name:Character:20 Code:Integer:2 Salary:Money:1 Title:Character:20 Gender:Character:1)| Name:\"John Doe\" Code[0]:1001 Code[1]:2002 Salary:$195.75 Title:\"Victim\" Gender:#\\M)")

(setq myBrick #void)
(setq myTitle #void)
(testit "(gc)" #void)
;; End of Single Brick Test

(writeln scriptName "step [005] started")

;; Multiple Brick Behavior
(testit "(type (setq myBrick (new Brick: 5 Name:Character:20 Code:Integer:2 Salary:Money:1 Title:Character:20 Gender:Character:1)))" Brick:)

(testit {myBrick["RowCount"]} 5)
(testit {myBrick["RowByteCount"]} (if (= _ais.bits 64) 65 57))
(testit {myBrick["FieldCount"]} 5)
(testit {myBrick["ByteCount"]} (if (= _ais.bits 64) 325 285) )

;; Single index access will return a BrickRow type
(testit "(type (setq row1 myBrick[0]))" BrickRow:)
(testit "(type (setq row2 myBrick[1]))" BrickRow:)
(testit "(type (setq row4 myBrick[3]))" BrickRow:)
(testit "(type (setq row5 myBrick[4]))" BrickRow:)

;; Rows are initially empty
(testit "(string row1 true)" "{BrickRow| Name:\"\" Code[0]:0 Code[1]:0 Salary:$0.0 Title:\"\" Gender:#\\}")
(testit "(string row2 true)" "{BrickRow| Name:\"\" Code[0]:0 Code[1]:0 Salary:$0.0 Title:\"\" Gender:#\\}")
(testit "(string row4 true)" "{BrickRow| Name:\"\" Code[0]:0 Code[1]:0 Salary:$0.0 Title:\"\" Gender:#\\}")
(testit "(string row5 true)" "{BrickRow| Name:\"\" Code[0]:0 Code[1]:0 Salary:$0.0 Title:\"\" Gender:#\\}")

;; String index access for BrickRow objects
(testit "row1[\"FieldCount\"]" 5)
(testit "row1[\"ByteCount\"]" (if (= _ais.bits 64) 65 57))
(testit "(type row1[\"FieldList\"])" Structure:)

(testit "row5[\"FieldCount\"]" 5)
(testit "row5[\"ByteCount\"]" (if (= _ais.bits 64) 65 57))
(testit "(type row5[\"FieldList\"])" Structure:)

;; Combination of numeric and symbolic indexes
(testit "row1[0 type:]" Character:)
(testit "row1[1 type:]" Integer:)
(testit "row1[2 type:]" Money:)

;; For Repeats
(testit "row1[0 repeats:]" 20)
(testit "row1[1 repeats:]" 2)
(testit "row1[2 repeats:]" 1)

;; For offsets
(testit "row1[0 offset:]" 0)
(testit "row1[1 offset:]" 20)
(testit "row1[2 offset:]" (if (= _ais.bits 64) 36 28))

;; Row offsets are relative to the beginning of the Row data
(testit "row5[0 offset:]" 0)
(testit "row5[1 offset:]" 20)
(testit "row5[2 offset:]" (if (= _ais.bits 64) 36 28))

;; Field value can be set using the BrickRow objects
(testit "(type (setq row1[0] \"Mike Korns\"))" BrickRow:)
(testit "(type (setq row2[0] \"Gilda Cabral\"))" BrickRow:)
(testit "(type (setq row4[0] \"Ted Williams\"))" BrickRow:)
(testit "(type (setq row5[0] \"Tim May\"))" BrickRow:)

;; Aside from the Field Index, the Field Name can be used to reference a Field
(testit "(type (setq row1.Gender #\\M))" BrickRow:)
(testit "(type (setq row2.Gender #\\F))" BrickRow:)
(testit "(type (setq row4.Gender #\\M))" BrickRow:)
(testit "(type (setq row5.Gender #\\M))" BrickRow:)

;; Multi-index access to a BrickObject
(testit "(type (setq row1[1 0] 1))" BrickRow:)
(testit "(type (setq row1[1 1] 2))" BrickRow:)
(testit "row1[1 0]" 1)
(testit "row1[1 1]" 2)

;; Multi-repeat Character fields will return Substring type
(testit "(type row1[0])" Substring:)
(testit "(type row2[0])" Substring:)
(testit "(type row4[0])" Substring:)
(testit "(type row5[0])" Substring:)

(testit "(type row1.Name)" Substring:)
(testit "(type row1.Code)" BrickField:)
(testit "(type row1.Salary)" Money:)
(testit "(type row1.Gender)" Character:)

;; Rows should contain some data
(testit "(string row1 true)" "{BrickRow| Name:\"Mike Korns\" Code[0]:1 Code[1]:2 Salary:$0.0 Title:\"\" Gender:#\\M}")
(testit "(string row2 true)" "{BrickRow| Name:\"Gilda Cabral\" Code[0]:0 Code[1]:0 Salary:$0.0 Title:\"\" Gender:#\\F}")

;; Accessing a multi-repeat field will return a BrickField
(testit "(type (setq row1field2 row1[1]))" BrickField:)
(testit "(string row1field2 true)" "{BrickField| [0]:1 [1]:2}")

;; Symbolic indexes for BrickField objects
(testit "row1field2[type:]" Integer:)
(testit "row1field2[repeats:]" 2)
(testit "row1field2[offset:]" 20)

;; Setting a value in the BrickField should reflect in the BrickRow object
(testit "(type (setq row1field2[0] 10001))" BrickField:)
(testit "row1field2[0]" 10001)
(testit "row1[1][0]" 10001)
(testit "myBrick[0][1][0]" 10001)
(testit "myBrick[0].Code[0]" 10001)
(testit "(string row1field2 true)" "{BrickField| [0]:10001 [1]:2}")
(testit "(string row1 true)" "{BrickRow| Name:\"Mike Korns\" Code[0]:10001 Code[1]:2 Salary:$0.0 Title:\"\" Gender:#\\M}")

;; Saving Brick, BrickRow, and BrickField objects to the Object Repository
(testit "(type (setq R (new ObjectRepository: {TestBrickObjects.db} clear:)))" ObjectRepository:)
(testit "(type (setq R.myBrick myBrick))" ObjectRepository:)
(testit "(type (setq R.row1 row1))" ObjectRepository:)
(testit "(type (setq R.row1field2 row1field2))" ObjectRepository:)

(testit "(setq myBrick #void)" #void)
(testit "(setq row1 #void)" #void)
(testit "(setq row2 #void)" #void)
(testit "(setq row4 #void)" #void)
(testit "(setq row5 #void)" #void)
(testit "(setq row1field2 #void)" #void)

(testit "(gc compact:)" #void)

(testit "myBrick[1]" #void)
(testit "row1[1]" #void)
(testit "row1field2[1]" #void)

;; Loading Brick, BrickRow, and BrickField objects from the Object Repository
(testit "(type (setq R (new ObjectRepository: {TestBrickObjects.db})))" ObjectRepository:)
(testit "(type (setq myBrick R.myBrick))" Brick:)
(testit "(type (setq row1 R.row1))" BrickRow:)
(testit "(type (setq row1field2 R.row1field2))" BrickField:)

(testit "myBrick[0][0]" "Mike Korns")
(testit "row1[0]" "Mike Korns")

(testit "myBrick[0][1][0]" 10001)
(testit "row1field2[0]" 10001)
(testit "row1[1][0]" 10001)

(testit "(type (setq row1field2[0] 20002))" BrickField:)
(testit "row1field2[0]" 20002)

(setq R #void)
(setq myBrick #void)
(setq row1 #void)
(setq row1field2 #void)
(testit "(gc compact:)" #void)

;; Saving/Loading BrickRow/BrickField to a ByteVector
(setq myBrick (new Brick: 2 Name:Character:20 Age:Integer:1 Status:Boolean:2))
(setq myBrick[0][0] "John Doe")
(setq myBrick[0][1] 45)
(setq myBrick[0][2][0] true)
(setq myBrick[0][2][1] false)
(setq myBrick[1][0] "Jane Doe")
(setq myBrick[1][1] 34)
(setq myBrick[1][2][0] false)
(setq myBrick[1][2][1] true)

(testit "(type (setq byteVector1 (saveObject myBrick[0])))" ByteVector:)
(testit "(type (setq byteVector2 (saveObject myBrick[1])))" ByteVector:)
(testit "(type (setq byteVector3 (saveObject myBrick[0][2])))" ByteVector:)
(testit "(type (setq byteVector4 (saveObject myBrick[1][2])))" ByteVector:)

(setq myBrick #void)
(testit "(gc compact:)" #void)

(testit "(type (setq myBrickRow1 (loadObject byteVector1)))" BrickRow:)
(testit "(type (setq myBrickRow2 (loadObject byteVector2)))" BrickRow:)
(testit "(type (setq myBrickFld1 (loadObject byteVector3)))" BrickField:)
(testit "(type (setq myBrickFld2 (loadObject byteVector4)))" BrickField:)

(testit "myBrickRow1[0]" "John Doe")
(testit "myBrickRow1[1]" 45)
(testit "myBrickRow1[2][0]" true)
(testit "myBrickRow1[2][1]" false)

(testit "myBrickRow2[0]" "Jane Doe")
(testit "myBrickRow2[1]" 34)
(testit "myBrickRow2[2][0]" false)
(testit "myBrickRow2[2][1]" true)

(testit "myBrickFld1[0]" true)
(testit "myBrickFld1[1]" false)

(testit "myBrickFld2[0]" false)
(testit "myBrickFld2[1]" true)

(setq myBrickRow1 #void)
(setq myBrickRow2 #void)
(setq myBrickFld1 #void)
(setq myBrickFld2 #void)
(setq byteVector1 #void)
(setq byteVector2 #void)
(setq byteVector3 #void)
(setq byteVector4 #void)

(testit "(gc compact:)" #void)

;; Saving/Loading BrickRow/BrickField to a Buffer
(setq myBrick (new Brick: 2 Name:Character:20 Age:Integer:1 Status:Boolean:2))
(setq myBrick[0][0] "John Doe")
(setq myBrick[0][1] 45)
(setq myBrick[0][2][0] true)
(setq myBrick[0][2][1] false)
(setq myBrick[1][0] "Jane Doe")
(setq myBrick[1][1] 34)
(setq myBrick[1][2][0] false)
(setq myBrick[1][2][1] true)

(setq bufferSize1 (saveObject 0 myBrick[0] false))
(setq bufferSize2 (saveObject 0 myBrick[1] false))
(setq bufferSize3 (saveObject 0 myBrick[0][2] false))
(setq bufferSize4 (saveObject 0 myBrick[1][2] false))

(setq bufferPtr1 (createBuffer bufferSize1))
(setq bufferPtr2 (createBuffer bufferSize2))
(setq bufferPtr3 (createBuffer bufferSize3))
(setq bufferPtr4 (createBuffer bufferSize4))

(testit "(type (saveObject myBrick[0] bufferSize1 bufferPtr1))" Integer:)
(testit "(type (saveObject myBrick[1] bufferSize2 bufferPtr2))" Integer:)
(testit "(type (saveObject myBrick[0][2] bufferSize3 bufferPtr3))" Integer:)
(testit "(type (saveObject myBrick[1][2] bufferSize3 bufferPtr4))" Integer:)

(setq myBrick #void)
(testit "(gc compact:)" #void)

(testit "(type (setq myBrickRow1 (loadObject bufferPtr1)))" BrickRow:)
(testit "(type (setq myBrickRow2 (loadObject bufferPtr2)))" BrickRow:)
(testit "(type (setq myBrickFld1 (loadObject bufferPtr3)))" BrickField:)
(testit "(type (setq myBrickFld2 (loadObject bufferPtr4)))" BrickField:)

(testit "myBrickRow1[0]" "John Doe")
(testit "myBrickRow1[1]" 45)
(testit "myBrickRow1[2][0]" true)
(testit "myBrickRow1[2][1]" false)

(testit "myBrickRow2[0]" "Jane Doe")
(testit "myBrickRow2[1]" 34)
(testit "myBrickRow2[2][0]" false)
(testit "myBrickRow2[2][1]" true)

(testit "myBrickFld1[0]" true)
(testit "myBrickFld1[1]" false)

(testit "myBrickFld2[0]" false)
(testit "myBrickFld2[1]" true)

(setq myBrickRow1 #void)
(setq myBrickRow2 #void)
(setq myBrickFld1 #void)
(setq myBrickFld2 #void)
(setq bufferPtr1 #void)
(setq bufferPtr2 #void)
(setq bufferPtr3 #void)
(setq bufferPtr4 #void)

(testit "(gc compact:)" #void)

;; Saving/Loading BrickRow/BrickField to a File
(setq fd1 (fileOpen "BrickRow1.db" 1 4))
(setq fd2 (fileOpen "BrickRow2.db" 1 4))
(setq fd3 (fileOpen "BrickFld1.db" 1 4))
(setq fd4 (fileOpen "BrickFld2.db" 1 4))

(setq myBrick (new Brick: 2 Name:Character:20 Age:Integer:1 Status:Boolean:2))
(setq myBrick[0][0] "John Doe")
(setq myBrick[0][1] 45)
(setq myBrick[0][2][0] true)
(setq myBrick[0][2][1] false)
(setq myBrick[1][0] "Jane Doe")
(setq myBrick[1][1] 34)
(setq myBrick[1][2][0] false)
(setq myBrick[1][2][1] true)

(testit "(type (saveObject fd1 myBrick[0]))" Integer:)
(testit "(type (saveObject fd2 myBrick[1]))" Integer:)
(testit "(type (saveObject fd3 myBrick[0][2]))" Integer:)
(testit "(type (saveObject fd4 myBrick[1][2]))" Integer:)

(fileClose fd1 1)
(fileClose fd2 1)
(fileClose fd3 1)
(fileClose fd4 1)

(setq myBrick #void)
(testit "(gc compact:)" #void)

(setq fd1 (fileOpen "BrickRow1.db" 0 4))
(setq fd2 (fileOpen "BrickRow2.db" 0 4))
(setq fd3 (fileOpen "BrickFld1.db" 0 4))
(setq fd4 (fileOpen "BrickFld2.db" 0 4))

(testit "(type (setq myBrickRow1 (loadObject fd1)))" BrickRow:)
(testit "(type (setq myBrickRow2 (loadObject fd2)))" BrickRow:)
(testit "(type (setq myBrickFld1 (loadObject fd3)))" BrickField:)
(testit "(type (setq myBrickFld2 (loadObject fd4)))" BrickField:)

(testit "myBrickRow1[0]" "John Doe")
(testit "myBrickRow1[1]" 45)
(testit "myBrickRow1[2][0]" true)
(testit "myBrickRow1[2][1]" false)

(testit "myBrickRow2[0]" "Jane Doe")
(testit "myBrickRow2[1]" 34)
(testit "myBrickRow2[2][0]" false)
(testit "myBrickRow2[2][1]" true)

(testit "myBrickFld1[0]" true)
(testit "myBrickFld1[1]" false)

(testit "myBrickFld2[0]" false)
(testit "myBrickFld2[1]" true)

(setq myBrickRow1 #void)
(setq myBrickRow2 #void)
(setq myBrickFld1 #void)
(setq myBrickFld2 #void)
(fileClose fd1 0)
(fileClose fd2 0)
(fileClose fd3 0)
(fileClose fd4 0)

(testit "(gc compact:)" #void)

(testEnd "Test_Brick2.sl")