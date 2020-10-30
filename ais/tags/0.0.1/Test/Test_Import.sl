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
;;  Title:    Import Export Test Suite 
;;
;;  Author:   Michael F. Korns, Tim May
;;
;;  Project:  AIS Regression Suite
;;
;;  Notes:    RegTest.sl  
;;


;#memory=500
;#memoryObjectHeaders=100
(runScript "RegTest.sl")
(testStart "Test_Import.sl")

(defun tableLambda()
;; ********************************************************************
;; summary:  This Lambda creates, maintains, and operates as an in 
;;           memory relational table. The relational table is a series
;;           of attributed rows. The table can be sorted, we can have 
;;           multiple views, subsets of records can be found, etc.
;; Parms:    none  
;; return:   true
;; ********************************************************************
   pvars:(colCount 	           ;; The number of columns
          colVector                ;; Structure of attribute names (columns)
          exportSW                 ;; True if an exportTab is in progress
          fileID                   ;; Contains a fileID if an importTab is in progress
          importSW                 ;; True if an importTab is in progress
          myParent                 ;; The parent table Lambda
          rowCount      	   ;; The number of rows
          rowVector                ;; Vector of attributed relation rows
          saveVector               ;; Vector of saved relational rows
          showLimit                ;; The maximum number of records to show
          sparseSW                 ;; True if tableLambda manages sparse records
          ;; Methods list 
          addColumn 	           ;; Adds a new column name to the relation
          addRow 	           ;; Adds a new row to the relation
          clearRows	           ;; Clears all the rows in the relation
          defColumns 	           ;; Defines a new column structure for the relation
          delimitedCells           ;; Return a tab delimited string of cell values
          doAddRow	           ;; Inserts an empty row to the relation
          doCellText	           ;; Enters text data into the cell row and column specified
          doClear	           ;; Clears all the rows and columns in the relation
          doDeleteRow	           ;; Deletes a row from the relation
          doFind	           ;; Find a matching row value in the relational table
          doSort                   ;; Sort the relation on the specified column
          errorStop                ;; Clean up after any errors which may occur during operations
          exportTab                ;; Export a tab delimited .sbf format file into the relation
          find                     ;; Select matching rows in the relational table
          findAll                  ;; Restore the original rows in the relational table
          getColumnHeaders         ;; Return a tab delimited string of column headers
          getRowBlock              ;; Return a tab delimited string of row values for a specified column
          importTab                ;; Import a tab delimited .sbf format file into the relation
          len                      ;; Return the number of rows in the relation
          new                      ;; Create a new relational table
          ref1                     ;; Return a single row from the relational table
          refExport                ;; Called by exportTab to request a filled record to be exported.
          refImport                ;; Called by importTab to request an empty record for the import data
          refFormattedRow          ;; Return a single display formatted row from the relational table
          refColCount              ;; Return the number of columns in the relational table
          refRowCount              ;; Return the number of rows in the relational table
          setImport                ;; Install a single row in the relation table (only done by importTab)
          setSparse                ;; Set the tableLambda for sparse records
          show                     ;; Display the rows in the relation
          sort                     ;; Sort the rows in the relation
          truncate                 ;; Truncate rows in the relational table
         ) ;; end of persistent variables
   ;; Never initialize this Lambda more than once, because the
   ;; inline child Lambdas will overlay the cloned child Lambdas
   ;; in any clone copies of the Lambda and this causes serious
   ;; confusion when the reinitialized clone begins to affect
   ;; the persistant storage of the original Lambda.
   (if (<> new #void) (goto Continue:))
   ;; Initialize the inline child Lambdas.
   (defun addColumn(...)
      vars:(colName colIndex)
      (if (= colVector #void) (setq colVector (^new Structure:)))
      (loop for colIndex from 0 until (argCount) do
          (setq colName (symbol (argFetch colIndex)))
          (if (not (isMember colName colVector)) 
              (setq colVector (append colVector (makeStructure colName #void))))
          ) ;; end loop
      (setq colCount (length colVector))
      colCount) ;; end addColumn
   (defun addRow(newRow)
      vars:(newRecord)
      ;; Make a new record template and copy into it.
      (if (= sparseSW true)
          (cond 
            ((isDictionary newRow)
             (setq newRecord newRow))
            ((isStructure newRow)
             (setq newRecord (objectToDictionary newRow)))
            (else
             (setq newRecord (objectToDictionary (copy colVector) newRow)))
            ) ;; end sparse record coercion cond
          (cond 
            ((isStructure newRow)
             (setq newRecord newRow))
            (else
             (setq newRecord (objectToDictionary (copy colVector) newRow)))
            ) ;; end dense record coercion cond
          ) ;; end sparse if
      ;; Make sure we are adding to the main row vector
      (if (<> saveVector #void) 
          (begin
             (setq rowVector saveVector)
             (setq saveVector #void)
          )) ;; end if
      ;; Add new record to the end of the main row vector
      (setq rowVector[rowCount] newRecord)
      (setq rowCount (length rowVector))
      rowCount) ;; end addRow
   (defun clearRows()
      (setq rowVector (^new Vector: object: 0))
      (setq saveVector #void)
      (setq rowCount 0)
      true) ;; end clearRows
   (defun defColumns(newColumns)
      (doClear)
      (if (isVector newColumns)
          (setq colVector (objectToStructure newColumns #(#void)))
          (setq colVector (objectToStructure newColumns))
          ) ;; end new column coercion if
      (setq colCount (length colVector))
      colCount) ;; end defColumns
   (defun delimitedCells(startRow endRow startCol endCol)
      vars:(rowIndex colIndex dls cellValue record) 
      ;; Return a tab delimited string of cell values for
      ;; all cells between the following rows and columns.
      (setq dls "")
      (loop for rowIndex from startRow to endRow do
         (setq dls (append dls rowIndex (char 09)))
         (setq record (refFormattedRow rowIndex))
         (loop for colIndex from startCol until endCol do
             (setq cellValue record[colIndex])
             (if (= cellValue #void) then (setq cellValue ""))
             (setq dls (append dls cellValue (char 09)))
             ) ;;end column loop
         (setq dls (append dls (char 10) (char 13)))
         ) ;;end row loop
      (append dls "")) ;; end of delimitedCells
   (defun doAddRow(rowIndex colIndex) (error "NoAccess"))
   (defun doCellText(rowIndex colIndex newValue) (error "NoAccess"))
   (defun doClear()
      (setq colVector #void)
      (setq rowVector (^new Vector: object: 0))
      (setq saveVector #void)
      (setq colCount 0)
      (setq rowCount 0)
      true) ;; end doClear
   (defun doDeleteRow(rowIndex colIndex) (error "NoAccess"))
   (defun doFind(colName startRow value)
      vars:(rowIndex colIndex)
      (setq colIndex (symbol colName))
      ;; Return the first row with the chosen value in the column.
      (loop for rowIndex from 0 until rowCount do
          (if (= value (string rowVector[rowIndex][colIndex]))
              (return rowIndex))
          ) ;; end of loop
      ;; If we get to here, we did not find the specified value anywhere.
      startRow) ;; end doFind
   (defun doSort(columnName sortOrder) 
      vars: (source Lambda)
      (setq source (append "(lambda(x y) (" sortOrder "  x.|") )
      (setq source (append source columnName "| y.|"))
      (setq source (append source columnName "|))"))
      (setq Lambda (eval source))
      (sort Lambda)) ;; end doSort
   (defun errorStop(errMsg)
      (setq importSW false)
      (setq exportSW false)
      (if (isNumber fileID) 
          (fileClose fileID 1))
      (setq fileID #void)
      (error (mid errMsg 1 (subi (length errMsg) 2)))) ;; end errorStop
  
   (defun exportTab(fileName)
      vars:(rowIndex newIndex n vec)
      ;; Make sure we catch all errors which might occur.
      (onError errorStop)
      ;; Make sure we are properly initialized.
      (if (= myParent #void) (error "tableInit"))
      (if (<> myParent.Pv (myself)[Pv:]) (error "tableInit"))
      (setq exportSW true)
      ;; Open the specified .SBF file and export.
      (setq fileID (fileOpen fileName 1 0))
      (^exportTab fileID myParent recordVectors:)
      (fileClose fileID 1)
      (setq fileID #void)
      (setq exportSW false)
      rowCount) ;; end exportTab
 
  (defun find(findLambda)
      vars:(rowIndex newIndex n vec)
      (if (= saveVector #void) (setq saveVector (copy rowVector)))
      ;; Select only those rows for which the find Lambda returns true.
      (setq n (length rowVector))
      (setq vec (^new Vector: object: 0))
      (setq newIndex -1)
      (loop for rowIndex from 0 until n do
          (if (findLambda rowVector[rowIndex]) (setq vec[(++ newIndex)] rowVector[rowIndex]))
          ) ;; end loop
      (setq rowVector vec)
      (setq rowCount (length rowVector))
      rowCount) ;; end find
   (defun findAll()
      (if (= saveVector #void) (setq saveVector (copy rowVector)))
      (setq rowVector (copy saveVector))
      (setq rowCount (length rowVector))
      rowCount) ;; end findAll
   (defun getColumnHeaders(startCol endCol) 
      vars:(colIndex dls colName)  
      (setq dls "")
      (loop for colIndex from startCol until endCol do
          (setq colName (string colVector[colIndex 0]))
          (if (= (left colName 1) "|") (setq colName (mid colName 1 (subi (length colName) 2))))
          (setq dls (append dls colName (char 09)))
          ) ;;end column loop
      (append dls "")) ;; end getColumnHeaders
   (defun getRowBlock(startRow endRow colName) 
      vars:(rowIndex dls cellValue)  
      (setq dls "")
      (setq startRow (max startRow 0))
      (setq endRow (min endRow rowCount))
      (loop for rowIndex from startRow until endRow do
          (setq dls (append dls rowVector[rowIndex][colName] (char 09)))
          ) ;;end row loop
      (append dls "")) ;; end getRowBlock
   (defun importTab(fileName)
      vars:(rowIndex newIndex n vec)
      ;; Make sure we catch all errors which might occur.
      (onError errorStop)
      ;; Make sure we are properly initialized.
      (if (= myParent #void) (error "tableInit"))
      (if (<> myParent.Pv (myself)[Pv:]) (error "tableInit"))
      (setq importSW true)
      ;; Open the specified .SBF file and import.
      (setq fileID (fileOpen fileName 0 0))
      (^importTab fileID myParent recordVectors:)
      (fileClose fileID 1)
      (setq fileID #void)
      (setq importSW false)
      (findAll)
      rowCount) ;; end importTab
   (defun len() 
      (setq rowCount (length rowVector))
      rowCount) ;; end len
   (defun new()
      (setq colVector #void)
      (setq rowVector (^new Vector: object: 0))
      (setq saveVector #void)
      (setq colCount 0)
      (setq rowCount 0)
      (setq showLimit 10)
      (setq importSW false)
      (setq exportSW false)
      (setq fileID #void)
      true) ;; end of new

   (defun ref1(rowOrMember)
      (if (isSymbol rowOrMember) (return (myself)[Pv:][rowOrMember])) ;; Allows Lambda polymorphism.
      (if (not (isNumber rowOrMember)) (error "invalidKey"))
      ;; We are now retrieving a specific indexed row of the table.
      rowVector[rowOrMember]) ;; end of ref1
   
   (defun refImport(row)
      (if (not (isNumber row)) (error "invalidKey"))
      ;; Return a an empty vector (if there are no columns defined),
      ;; or return an empty record Structure (if there are columns defined).
      (if (= colVector #void)
          (return (^new Vector: 0))
          (return (copy colVector)))
      ) ;; end of refImport

   (defun refExport(rowIndex)
     vars:(record colIndex colName) 
      ;; Construct the .SBF column name header record for tab delimited export.
      (if (= rowIndex 0)
          (begin
             (setq record (^new Vector: colCount))
             (loop for colIndex from 0 until colCount do
                 (setq colName (string colVector[colIndex 0]))
                 (if (= (left colName 1) "|") (setq colName (mid colName 1 (subi (length colName) 2))))
                 (setq record[colIndex] colName)
                 ) ;; end loop
             (return record)
          )) ;; end if
      ;; Construct the .SBF column type header record for tab delimited export.
      (if (= rowIndex 1) (return (^new Vector: colCount "G")))
      ;; Construct the .SBF column keyness header record for tab delimited export.
      (if (= rowIndex 2) (return (^new Vector: colCount "n")))
      ;; We are now exporting a specific indexed row of the table.
      (setq rowIndex (subi rowIndex 3))
      (if (>= rowIndex rowCount) (return false))
      (if (= sparseSW true)
          (setq record (objectToStructure (copy colVector) rowVector[rowIndex]))
          (setq record rowVector[rowIndex])
          ) ;; end spare if
      record) ;; end of refExport 

   (defun refFormattedRow(rowIndex)
      vars:(record)
      (if (not (isNumber rowIndex)) (error "invalidKey"))
      ;; We are now retrieving a specific indexed row of the table.
      (if (= sparseSW true)
          (setq record (objectToStructure (copy colVector) rowVector[rowIndex]))
          (setq record rowVector[rowIndex])
          ) ;; end spare if
      record) ;; end of refFormattedRow
   (defun refColCount() colCount)
   (defun refRowCount() (if (= exportSW true) (addi rowCount 3) rowCount))

   (defun setImport(rowIndex recordVector)
      vars:(newRecord)
      ;; Only valid during the importTab function.
      (if (<> importSW true) (error "NoAccess")) 
      ;; Allow numeric record keys only!
      (if (not (isNumber rowIndex)) (error "invalidKey")) 
      ;; Import the .SBF column header record.
      (if (= rowIndex 0)
          (begin 
             (setq colVector (objectToStructure recordVector #(#void)))
             (setq colCount (length colVector))
             (return true)
             )) ;; end if
      ;; Ignore all other .SBF header records.
      (if (< rowIndex 3) (return true))
      ;; Import all other .SBF records as data rows.
      (if (= sparseSW true)
          (setq newRecord (objectToDictionary recordVector))
          (setq newRecord recordVector)
          ) ;; end sparse if
      (setq rowVector[rowCount] newRecord)
      (setq rowCount (length rowVector))
      rowCount) ;; end of setImport


   (defun setSparse(SW) (clearRows) (setq sparseSW SW))
   (defun show(startIndex) 
      vars:(i n) 
      (setq n (min (length rowVector) (addi startIndex showLimit)))
      (loop for i from startIndex until n do
          (writeln "[" i "] " (refFormattedRow i))
          ) ;; end loop
      true) ;; end show
   (defun sort(sortLambda)
      (if (= saveVector #void) (setq saveVector (copy rowVector)))
      (^sort rowVector sortLambda)) ;; end sort
   (defun truncate(rowLimit)
      vars:(rowIndex newIndex n vec)
      ;; Truncate the relational table to the specified number of rows.
      (setq rowLimit (min rowLimit (length rowVector)))
      (resize rowVector rowLimit)
      (setq rowCount (length rowVector))
      rowCount) ;; end truncate
   ;; Initialize the Lambda and clear the relational table.
   Continue::
   (setq myParent (myself))
   (new)) ;; end of tableLambda


;; *****************************************************
;;  Test the importTab method of tableLambda for a Reading an .SBF file
;;******************************************************

(writeln "Import/export test suite initialization started")


;;  Create the required tableLambda.
(tableLambda)
(define Stocks (new tableLambda))

(setq memFree (inspect))
(setq startTime (getTickCount 0))
(Stocks.importTab "Test_Import_stocks.sbf")
(setq endTime (getTickCount startTime))
(writeln "Rows imported during importTab of Stocks.sbf is " Stocks.rowCount " rows" )
(writeln "time required for importTab of Stocks.SBF is " endTime " Seconds" )
(writeln "space required for a tableLambda is " (- memFree (inspect)) " bytes")

;;  Test the save procedures for the Stocks tableLambda.
(setq startTime (getTickCount 0))
(setq fileID (fileOpen "stocks.tbl" 1 2))
(saveObject fileID Stocks)
(fileClose fileID 1)
(setq endTime (getTickCount startTime))
(writeln "time required for save of Stocks is " endTime " Seconds" )

;;  Test the load procedures for the Stocks tableLambda.
(setq Stocks #void)
(setq startTime (getTickCount 0))
(setq fileID (fileOpen "stocks.tbl" 0 2))
(setq Stocks (loadObject fileID))
(fileClose fileID 1)
(setq endTime (getTickCount startTime))
(writeln "time required for load of Stocks is " endTime " Seconds" )

(setq fileID (fileOpen "stocks.tbl" 1 2)) 
(fileClose fileID 0) 

;; *********************************************
;;  Test the ref methods for the tableLambda 
;; *********************************************

;;  Import Tough.sbf into a tableLambda.
(setq Tough (new tableLambda))
(setq memFree (inspect))
(setq startTime (getTickCount 0))
(Tough.importTab "Test_Import_tough.sbf")
(setq endTime (getTickCount startTime))
(writeln "Rows imported during importTab of tough.sbf is " Tough.rowCount " rows" )
(writeln "time required for importTab of tough.sbf is " endTime " Seconds" )
(writeln "space required for a tableLambda is " (- memFree (inspect)) " bytes")
(if (<> Tough.rowCount 243) 
    (writeln "Tough.sbf row count is wrong"))

;; Test the contents of the last cell 
(setq col 23)
(setq row 242)
(if (<> Tough[row][col] "DATA24") 
	(testError (append "*FAILURE*: Tough [" row "] [" col "] is wrong")))

;; Test the contents of the first cell 
(setq col 0)
(setq row 0)
(if (<> Tough[row][col] "AAAAA1") 
	(testError (append "*FAILURE*: Tough [" row "] [" col "] is wrong")))

;; Test if data was written beyond the last column
(setq col 24)
(setq row 241)
(if (<> Tough[row][col] #void) 
	(testError (append "*FAILURE*: Tough [" row "] [" col "] is wrong")))

;; Test a whole row with a void cells 
(setq col 0)
(setq row 234)
(if (<> Tough[row][col] #void) 
	(testError (append "*FAILURE*: Tough [" row "] [" col "] is wrong")))

;; Test a whole row of null strings
(setq col 0)
(setq row 236)
(if (<> Tough[row][col] #void) 
	(testError (append "*FAILURE*: Tough [" row "] [" col "] is wrong")))

;; Test a row containing a void column in the middle of it 
(setq col 5)
(setq row 238)
(if (<> Tough[row][col] #void) 
	(testError (append "*FAILURE*: Tough [" row "] [" col "] is wrong")))

;; Test a row containing a null string in the middle of it
(setq col 9)
(setq row 239)
(if (<> Tough[row][col] #void) 
	(testError (append "*FAILURE*: Tough [" row "] [" col "] is wrong")))


;; Test a row containing fewer columns than the rest of the rest of the rows
(setq col 23)
(setq row 240)
(if (<> Tough[row][col] #void) 
	(testError (append "*FAILURE*: Tough [" row "] [" col "] is wrong")))


;; *********************************************
;;  Test the exportTab method for the tableLambda 
;; *********************************************


;;  Use the exportTab method to create the temp.sbf file.
(setq memFree (inspect))
(setq startTime (getTickCount 0))
(setq rows (Tough.exportTab "temp.sbf"))
(setq endTime (getTickCount startTime))

(writeln "Rows exported during exportTab of temp.sbf is " rows " rows" )
(writeln "time required for exportTab of temp.sbf is " endTime " Seconds" )

;;  Re-import temp.sbf into the tableLambda and re-check the data

(setq memFree (inspect))
(setq startTime (getTickCount 0))
(setq rows (Tough.importTab "temp.sbf"))
(setq endTime (getTickCount startTime))

(writeln "Rows imported during importTab of temp.sbf is " Tough.rowCount " rows" )
(writeln "Time required for importTab of temp.sbf is " endTime " Seconds" )
(writeln "Space required by tableLambda is " (- memFree (inspect)) " bytes")

(if (<> Tough.rowCount 243) 
    (writeln "Tough.sbf row count is wrong"))

;; Test the contents of the last cell 
(setq col 23)
(setq row 242)
(if (<> Tough[row][col] "DATA24") 
	(testError (append "*FAILURE*: Tough [" row "] [" col "] is wrong")))

;; Test the contents of the first cell 
(setq col 0)
(setq row 0)
(if (<> Tough[row][col] "AAAAA1") 
	(testError (append "*FAILURE*: Tough [" row "] [" col "] is wrong")))

;; Test if data was written beyond the last column
(setq col 24)
(setq row 241)
(if (<> Tough[row][col] #void) 
	(testError (append "*FAILURE*: Tough [" row "] [" col "] is wrong")))

;; Test a whole row with a void cells 
(setq col 0)
(setq row 234)
(if (<> Tough[row][col] #void) 
	(testError (append "*FAILURE*: Tough [" row "] [" col "] is wrong")))

;; Test a whole row of null strings
(setq col 0)
(setq row 236)
(if (<> Tough[row][col] #void) 
	(testError (append "*FAILURE*: Tough [" row "] [" col "] is wrong")))

;; Test a row containing a void column in the middle of it 
(setq col 5)
(setq row 238)
(if (<> Tough[row][col] #void) 
	(testError (append "*FAILURE*: Tough [" row "] [" col "] is wrong")))

;; Test a row containing a null string in the middle of it
(setq col 9)
(setq row 239)
(if (<> Tough[row][col] #void) 
	(testError (append "*FAILURE*: Tough [" row "] [" col "] is wrong")))


;; Test a row containing fewer columns than the rest of the rest of the rows
(setq col 23)
(setq row 240)
(if (<> Tough[row][col] #void) 
	(testError (append "*FAILURE*: temp.sbf cell " col " " row " is wrong")))


(testEnd "Test_Import.sl")
















