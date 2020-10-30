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
;;  Title:    Smarttable Test Suite 
;;
;;  Author:   Michael F. Korns
;;
;;  Project:  Smarttable cell access and assignment study.
;;            The speed and efficiency of cell access and
;;            math, is acceptance tested in this test script.
;;
;;  Notes:    No other dependencies.  
;;

;;  The diagnostic report function for this test script


(defun diagnostic(msg) (ringBell) (error "smarttable" msg))
(writeln "Smarttable test suite initialization started")

;;  Define a simple VmScript timing test procedure.
;;  Note:  This procedure accepts two arguments
;;         proc:      the procedure to be tested
;;         count:     the number of iterations to test
(define (timingTest proc count) vars:(startTime endTime)
    (setq startTime (getTickCount 0))
    (proc count) 
    (setq endTime (getTickCount startTime))
    (writeln "Elapsed time is " endTime " Seconds" ) endTime)

;;  Define a simple VmScript test procedure.
;;  Note:  This procedure accepts one argument
;;         n:         The number of times to delete rows
(defun delTest(n) 
   vars:(StockST rowCount rowNum i j)
   (define _DBPathName "Test.db")
   (define db (new ObjectRepository: _DBPathName))
   (setq StockST (new Smarttable:))
   (setq StockST.name Stocks:)  

   (setq fileID (fileOpen "stocks.sbf" 0 0))
   (importTab fileID StockST )
   (fileClose fileID 1)
   (setq rowCount StockST.rowCount)
   ;;(writeln "row Count = " rowCount)
   (setq db[1] StockST)

   (loop for j from 0 until n do
       (setq StocksST db[1])

       (loop for i from 0 until 100 do
           (setq rowNum (integer (random rowCount)))
           (deleteRow StockST 1 rowNum)
           ) ;; end i loop

       (setq db[2] StockST)
       (setq StockST db[2])
       (systemCheck false)
       ) ;; end j loop
   ) ;; end delTest

;;  Create the required smarttable.
(define AddressList "Address List")
(define AddressListST 
    (new Smarttable: 
         (makeEnvironment name: key: city: key: salary: formula:)))
(addView AddressListST)
(setq AddressListST.autoRecalc false)
(if (<> AddressListST.autoRecalc false) (diagnostic "*FAILURE*: autoRecalc is wrong"))
(if (<> AddressListST.colCount 3) (diagnostic "*FAILURE*: colCount is wrong"))
(if (<> AddressListST.colVector[0].name name:) (diagnostic "*FAILURE*: name: column is wrong"))
(if (<> AddressListST.colVector[0].colFormat key:) (diagnostic "*FAILURE*: name: column is wrong"))
(if (<> AddressListST.colVector[0].cellIndex 0) (diagnostic "*FAILURE*: name: column is wrong"))
(if (<> AddressListST.colVector[1].name city:) (diagnostic "*FAILURE*: city: column is wrong"))
(if (<> AddressListST.colVector[1].colFormat key:) (diagnostic "*FAILURE*: city: column is wrong"))
(if (<> AddressListST.colVector[1].cellIndex 1) (diagnostic "*FAILURE*: city: column is wrong"))
(if (<> AddressListST.colVector[2].name salary:) (diagnostic "*FAILURE*: salary: column is wrong"))
(if (<> AddressListST.colVector[2].colFormat formula:) (diagnostic "*FAILURE*: salary: column is wrong"))
(if (<> AddressListST.colVector[2].cellIndex 2) (diagnostic "*FAILURE*: salary: column is wrong"))

;;  Assign selected Smarttable cells.
;;  Note: This tests that the Smarttable can add rows one cell at a time
;;        and still keep all its indexing intact. This should be true even
;;        when the rows are added in random order.
(setq AddressListST[0 2 value:] "Mike Korns")
(setq AddressListST[name: 1 value:] "Sam Spade")
(setq AddressListST[0 0 value:] "John Doe")
(setq AddressListST[0 3 value:] "Mike Korns")
(if (<> AddressListST[0 0 value:] "John Doe") (diagnostic "*FAILURE*: cell [0 0] is wrong"))
(if (<> AddressListST[name: 1 value:] "Sam Spade") (diagnostic "*FAILURE*: cell [name: 1] is wrong"))
(if (<> AddressListST[0 2 value:] "Mike Korns") (diagnostic "*FAILURE*: cell [0 2] is wrong"))
(if (<> AddressListST[0 3 value:] "Mike Korns") (diagnostic "*FAILURE*: cell [0 3] is wrong"))
(if (<> AddressListST.colVector[0].valueIndex.valueCount 3) 
    (diagnostic "*FAILURE*: name: column value count is wrong"))
(if (<> AddressListST.colVector[0].valueIndex[0] "John Doe") 
    (diagnostic "*FAILURE*: name: column valueIndex[0] is wrong"))
(if (<> AddressListST.colVector[0].valueIndex[1] "Mike Korns") 
    (diagnostic "*FAILURE*: name: column valueIndex[1] is wrong"))
(if (<> AddressListST.colVector[0].valueIndex[2] "Sam Spade") 
    (diagnostic "*FAILURE*: name: column valueIndex[2] is wrong"))
(if (<> AddressListST.colVector[1].valueIndex.valueCount 0) (diagnostic "*FAILURE*: city: column value count is wrong"))
(if (<> AddressListST.colVector[2].valueIndex #void) (diagnostic "*FAILURE*: salary: column valueIndex is wrong"))
(setq AddressListST[1 2 value:] "San Francisco")
(setq AddressListST[city: 0 value:] "Kansas city")
(setq AddressListST[1 1 value:] "Boston")
(setq AddressListST[1 3 value:] "San Jose")
(if (<> AddressListST[1 0 value:] "Kansas city") (diagnostic "*FAILURE*: cell [1 0] is wrong"))
(if (<> AddressListST[city: 1 value:] "Boston") (diagnostic "*FAILURE*: cell [city: 1] is wrong"))
(if (<> AddressListST[1 2 value:] "San Francisco") (diagnostic "*FAILURE*: cell [1 2] is wrong"))
(if (<> AddressListST[1 3 value:] "San Jose") (diagnostic "*FAILURE*: cell [1 3] is wrong"))
(if (<> AddressListST.colVector[1].valueIndex.valueCount 4) 
    (diagnostic "*FAILURE*: city: column value count is wrong"))
(if (<> AddressListST.colVector[1].valueIndex[0] "Boston") 
    (diagnostic "*FAILURE*: city: column valueIndex[0] is wrong"))
(if (<> AddressListST.colVector[1].valueIndex[1] "Kansas city") 
    (diagnostic "*FAILURE*: city: column valueIndex[1] is wrong"))
(if (<> AddressListST.colVector[1].valueIndex[2] "San Francisco") 
    (diagnostic "*FAILURE*: city: column valueIndex[2] is wrong"))
(if (<> AddressListST.colVector[1].valueIndex[3] "San Jose") 
    (diagnostic "*FAILURE*: city: column valueIndex[3] is wrong"))
(setq AddressListST[2 1 value:] $23098)
(setq AddressListST[2 0 value:] $40456)
(setq AddressListST[2 2 value:] $53459)
(setq AddressListST[2 3 value:] $33900)
(if (<> AddressListST[2 0 value:] $40456) (diagnostic "*FAILURE*: cell [2 0] is wrong"))
(if (<> AddressListST[salary: 1 value:] $23098) (diagnostic "*FAILURE*: cell [salary: 1] is wrong"))
(if (<> AddressListST[2 2 value:] $53459) (diagnostic "*FAILURE*: cell [2 2] is wrong"))
(if (<> AddressListST.rowCount 4) (diagnostic "*FAILURE*: rowCount is wrong"))
(setq AddressListST.colVector[0].colFormat formula:)
(if (<> AddressListST.colVector[0].valueIndex #void) (diagnostic "*FAILURE*: name: column value count is wrong"))
(setq AddressListST.colVector[0].colFormat key:)
(if (<> AddressListST.colVector[0].valueIndex.valueCount 3) 
    (diagnostic "*FAILURE*: name: column value count is wrong"))
(if (<> AddressListST.colVector[0].valueIndex[0] "John Doe") 
    (diagnostic "*FAILURE*: name: column valueIndex[0] is wrong"))
(if (<> AddressListST.colVector[0].valueIndex[1] "Mike Korns") 
    (diagnostic "*FAILURE*: name: column valueIndex[1] is wrong"))
(if (<> AddressListST.colVector[0].valueIndex[2] "Sam Spade") 
    (diagnostic "*FAILURE*: name: column valueIndex[2] is wrong"))
(if (<> AddressListST.colVector[0].valueIndex[1 rowIndex:].rowCount 2) 
    (diagnostic "*FAILURE*: name: column valueIndex[1 rowIndex:].rowCount is wrong"))
(if (<> AddressListST.colVector[0].valueIndex[1 rowIndex:].value "Mike Korns") 
    (diagnostic "*FAILURE*: name: column valueIndex[1 rowIndex:].value is wrong"))
(if (<> AddressListST.colVector[0].valueIndex[1 rowIndex:][0].name "Mike Korns") 
    (diagnostic "*FAILURE*: name: column valueIndex[1 rowIndex:][1].name is wrong"))
(if (<> AddressListST.colVector[0].valueIndex[1 rowIndex:][1].name "Mike Korns") 
    (diagnostic "*FAILURE*: name: column valueIndex[1 rowIndex:][1].name is wrong"))
(setq x (rowIntersect AddressListST.colVector[0].valueIndex[1 rowIndex:]
                       AddressListST.colVector[1].valueIndex[2 rowIndex:]))
(if (<> x.name "Mike Korns") 
    (diagnostic "*FAILURE*: name: rowIntersect is wrong"))
(setq y (makeVector 2 AddressListST.colVector[0].valueIndex[1 rowIndex:]
                       AddressListST.colVector[1].valueIndex[2 rowIndex:]))
(setq x (rowIntersect y))
(if (<> x.name "Mike Korns") 
    (diagnostic "*FAILURE*: name: rowIntersect is wrong"))
(writeln "**Listing of Table**")
(loop for i from 0 to 3 do (writeln AddressListST[0 i] " " AddressListST[1 i] " " AddressListST[2 i]))
(writeln "**Listing of name: valueIndex**")
(loop for i from 0 to 2 do (writeln AddressListST.colVector[0].valueIndex[i]))
(writeln "**Listing of city: valueIndex**")
(loop for i from 0 to 3 do (writeln AddressListST.colVector[1].valueIndex[i]))

;;  Create a simple Outline view of the Smarttable.
;;  Note: This tests that the Outline view is operating correctly.
(setq myOutline (makeOutline AddressListST AddressListST.colVector[0] AddressListST.colVector[1]))
(define spaces #("" "   "))
(writeln "**Listing of name: city: Outline**")
(loop for i from 0 to 6 do (writeln spaces[myOutline[i level:]] myOutline[i]))
(if (<> myOutline.lineCount 7) 
    (diagnostic "*FAILURE*: Outline line count is wrong"))
(if (<> myOutline[0] "John Doe") 
    (diagnostic "*FAILURE*: Outline line [0] is wrong"))
(if (<> myOutline[1] "Kansas city") 
    (diagnostic "*FAILURE*: Outline line [1] is wrong"))
(if (<> myOutline[2] "Mike Korns") 
    (diagnostic "*FAILURE*: Outline line [2] is wrong"))
(if (<> myOutline[3] "San Francisco") 
    (diagnostic "*FAILURE*: Outline line [3] is wrong"))
(if (<> myOutline[4] "San Jose") 
    (diagnostic "*FAILURE*: Outline line [4] is wrong"))
(if (<> myOutline[5] "Sam Spade") 
    (diagnostic "*FAILURE*: Outline line [5] is wrong"))
(if (<> myOutline[6] "Boston") 
    (diagnostic "*FAILURE*: Outline line [6] is wrong"))

;;  Create a simple Matrix view of the Smarttable.
;;  Note: This tests that the Matrix view is operating correctly.

(writeln "**Starting Matrix View**")
(setq myMatrix (makeView AddressListST matrix: "myMatrix"))


(addView myMatrix)


(if (<> AddressListST.viewCount 1) 
    (diagnostic "*FAILURE*: myMatrix view count failed"))

;; Setup a Matrix View
(setupMatrixView myMatrix 
 row: AddressListST.colVector[0] AddressListST.colVector[1]
 cell: AddressListST.colVector[2])


(writeln "**Listing of name: city: Matrixview**")
(loop for i from 0 to 6 do 
 (writeln  spaces[myMatrix.row[i level:]] 
    myMatrix.row[i] "  " 
    myMatrix[0 i]))
(if (<> myMatrix.row.lineCount 7) 
    (diagnostic "*FAILURE*: Matrix line count is wrong"))
(if (<> myMatrix[0 0] $40456) 
    (diagnostic "*FAILURE*: Matrix cell [0 0] is wrong"))
(if (<> myMatrix[0 1] $40456) 
    (diagnostic "*FAILURE*: Matrix cell [0 1] is wrong"))
(if (<> myMatrix[0 2] $87359) 
    (diagnostic "*FAILURE*: Matrix cell [0 2] is wrong"))
(if (<> myMatrix[0 3] $53459) 
    (diagnostic "*FAILURE*: Matrix cell [0 3] is wrong"))
(if (<> myMatrix[0 4] $33900) 
    (diagnostic "*FAILURE*: Matrix cell [0 4] is wrong"))
(if (<> myMatrix[0 5] $23098) 
    (diagnostic "*FAILURE*: Matrix cell [0 5] is wrong"))
(if (<> myMatrix[0 6] $23098) 
    (diagnostic "*FAILURE*: Matrix cell [0 6] is wrong"))

;;  Create a simple Hyper view of the Smarttable.
;;  Note: This tests that the Hyper view is operating correctly.
(writeln "**Starting Hyper View**")
(setq myHyper (makeView AddressListST hyper: "myHyper"))
(addView myHyper)
(if (<> AddressListST.viewCount 2) 
    (diagnostic "*FAILURE*: myHyper view count failed"))
(setupHyperView myHyper 
 row: AddressListST.colVector[0] AddressListST.colVector[1]
 cell: AddressListST.colVector[2])
(writeln "**Listing of name: city: Hyperview**")
(loop for i from 0 to 14 do 
 (writeln  spaces[myHyper.row[i level:]] 
    myHyper.row[i] "  " 
    myHyper[0 i]))
(if (<> myHyper.row.lineCount 15) 
    (diagnostic "*FAILURE*: Hyperview line count is wrong"))
(if (<> myHyper[0 0] $40456) 
    (diagnostic "*FAILURE*: Hyperview cell [0 0] is wrong"))
(if (<> myHyper[0 2] $40456) 
    (diagnostic "*FAILURE*: Hyperview cell [0 2] is wrong"))
(if (<> myHyper[0 5] $87359) 
    (diagnostic "*FAILURE*: Hyperview cell [0 5] is wrong"))
(if (<> myHyper[0 8] $53459) 
    (diagnostic "*FAILURE*: Hyperview cell [0 8] is wrong"))
(if (<> myHyper[0 9] $33900) 
    (diagnostic "*FAILURE*: Hyperview cell [0 9] is wrong"))
(if (<> myHyper[0 10] $23098) 
    (diagnostic "*FAILURE*: Hyperview cell [0 10] is wrong"))
(if (<> myHyper[0 11] $23098) 
    (diagnostic "*FAILURE*: Hyperview cell [0 11] is wrong"))
(deleteSmarttool myHyper)
(setq myHyper #void)

;;  Define a simple import routine for the Smarttable cells.
;;  Note: This tests that the Smarttable can add cells quickly.
(defun importAddressList(endRow withKeys) vars:(rowIndex memFree startTime endTime name city salary)
   ;; Reset the Smarttable for volume record import
   (setq AddressListST.autoRecalc false)
   (if (= withKeys false)
       (begin 
           (setq AddressListST.colVector[0].colFormat formula:)
           (setq AddressListST.colVector[1].colFormat formula:)))
   ;; Create a large number of random record data
   (writeln "starting creation of " (* endRow 3) " collated data cells for import" )
   (setq startTime (getTickCount 0))
   (setq name (makeVector endRow))
   (setq city (makeVector endRow))
   (setq salary (makeVector endRow))
   (loop for rowIndex from 0 to endRow do
    (begin
     (setq name[rowIndex] (append "name" (+ 100000 rowIndex)))
     (setq city[rowIndex] (append "city" (mod rowIndex 4)))
     (setq salary[rowIndex] (money (truncate (random 100000))))))
   (setq endTime (getTickCount startTime))
   (writeln "starting import of " (* endRow 3) " collated data cells" )
   ;; Import a large number of records into the Smarttable
   (setq startTime (getTickCount 0))
   (setq memFree (inspect))
   (loop for rowIndex from 0 to endRow do
    (begin
     (setq AddressListST[0 rowIndex value:] name[rowIndex])
     (setq AddressListST[1 rowIndex value:] city[rowIndex])
     (setq AddressListST[2 rowIndex value:] salary[rowIndex])))
   (setq endTime (getTickCount startTime))
   (writeln "time required for import of " endRow " records is " endTime " Seconds" )
   (writeln "space required for import of " endRow " records is " (- memFree (inspect)) " bytes")
   ;; Perform validity checks on the Smarttable
   (setq AddressListST.autoRecalc true)
   (if (<> AddressListST.rowCount (+ endRow 1)) (diagnostic "*FAILURE*: import is wrong"))
   ;; Perform key column index checks on the Smarttable
   (if (= withKeys true) (indexAddressList)))

;;  Define a create key column routine for the Smarttable cells.
;;  Note: This tests the time and space required to index Smarttable key columns.
(defun indexAddressList() vars:(memFree startTime endTime)
   ;; Reset the Smarttable for volume record import
   (setq AddressListST.autoRecalc false)
   (setq AddressListST.colVector[0].colFormat formula:)
   (setq AddressListST.colVector[1].colFormat formula:)
   ;; Index the name column of the Smarttable
   (setq startTime (getTickCount 0))
   (setq memFree (inspect))
   (setq AddressListST.colVector[0].colFormat key:)
   (setq endTime (getTickCount startTime))
   (writeln "time required for index of name column is " endTime " Seconds" )
   (writeln "space required for index of name column is " (- memFree (inspect)) " bytes")
   ;; Index the city column of the Smarttable
   (setq startTime (getTickCount 0))
   (setq memFree (inspect))
   (setq AddressListST.colVector[1].colFormat key:)
   (setq endTime (getTickCount startTime))
   (writeln "time required for index of city column is " endTime " Seconds" )
   (writeln "space required for index of city column is " (- memFree (inspect)) " bytes"))

;;  Import data into the smarttable.
(setq memFree (inspect))
(setq startTime (getTickCount 0))
(importAddressList 1000 true)
(setq endTime (getTickCount startTime))
(writeln "time required for creating a Smarttable is " endTime " Seconds" )
(writeln "space required for a Smarttable is " (- memFree (inspect)) " bytes")

;;  Create a Matrixview of the smarttable.
(setq memFree (inspect))
(setq startTime (getTickCount 0))
(setupMatrixView myMatrix 
 row: AddressListST.colVector[0] 
 col: AddressListST.colVector[1]
 cell: AddressListST.colVector[2])
(setq endTime (getTickCount startTime))
(writeln "time required for creating a Matrixview is " endTime " Seconds" )
(writeln "space required for a Matrixview is " (- memFree (inspect)) " bytes")

;;  Create an Outline of the smarttable.
(setq memFree (inspect))
(setq startTime (getTickCount 0))
(setq myOutline (makeOutline AddressListST AddressListST.colVector[0] AddressListST.colVector[1]))
(setq endTime (getTickCount startTime))
(writeln "time required for creating an Outline is " endTime " Seconds" )
(writeln "space required for an Outline is " (- memFree (inspect)) " bytes")

;;  Create a Tableview of the smarttable.
(setq memFree (inspect))
(setq startTime (getTickCount 0))
(setq MyTableView (addView (makeView AddressListST table: MyTableView:)))
(if (<> AddressListST.viewCount 2) 
    (diagnostic "*FAILURE*: MyTableView view count failed"))
(if (<> AddressListST.viewVector[1] MyTableView) 
    (diagnostic "*FAILURE*: MyTableView creation failed"))
(if (<> AddressListST[0 456] MyTableView[0 456]) 
    (diagnostic "*FAILURE*: MyTableView cell access failed"))
(setq endTime (getTickCount startTime))
(writeln "time required for creating a Tableview is " endTime " Seconds" )
(writeln "space required for a Tableview is " (- memFree (inspect)) " bytes")

;;  Add a trailing summary row to the Tableview.
(setq myTable MyTableView)
(setq AddressListST.autoRecalc true)
(setq myTable.autoRecalc true)
(setq summaryRow myTable.rowCount)
(insertSummary myTable summaryRow)
(setq myTable[2 summaryRow formula:] "(money (sum #$C$1:$C$1))") 
(if (<> AddressListST[2 0] myTable[2 summaryRow]) 
    (diagnostic "*FAILURE*: MyTableView summary cell access failed"))
(deleteSummary myTable summaryRow)
(setq startTime (getTickCount 0))
(find myTable (lambda (x) (= x.city "Kansas city")))
(setq endTime (getTickCount startTime))
(writeln "time required for find on city column is " endTime " Seconds" )
(findAll myTable)

;;  Repeatedly delete rows from a Smarttable.
(delTest 100)

;;  Delete all references to the smarttable to give back memory.
(setq AddressListST #void)
(setq myTable #void)

;;  The test script has completed
(writeln "Smarttable test suite completed")
(clear)





