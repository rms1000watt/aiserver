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
;;*************************************
;;*************************************
;; Exported Lambda File Cabinet Document
;;*************************************
;;*************************************



;;**EXPORTKEY**:show
(defun show(thing)
;; *******************************************************************
;; summary:  This procedure shows the interior of a procedure, or
;;           the system statistics.
;; args:     thing        (Procedure) => The procedure to be shown.   
;;                        (Workspace) => The workspace Lambdas.   
;;                        (nn)        => Shows *error*[nn].   
;;                        (true)      => The workspace statistics.   
;; *******************************************************************
    vars:(s (title "") (instruction ""))
    ;; Do we show an error tree element?
    (if (isInteger thing)
        (setq thing *error*[thing]))
    ;; Do we show an error tree SPair?
    (if (isSpair thing)
        (begin
          (setq instruction (append "[" thing[1] "]"))
          (setq thing thing[0])))
    ;; Do we show an error tree Procedure Symbol?
    (if (or (isSymbol thing) (isString thing))
        (setq thing (eval thing)))
    ;; Do we disassemble a Procedure?
    (if (isType Lambda: thing)
        (begin
           (setq title (globalBinding thing))
           (if (= title #void)
               (setq title (string thing)))
     (setq title (append "" title instruction))
           (setq s (append "Variable declarations:" #\newline))
           (setq s (append s " Av = " (string thing.Av true) #\newline))
           (setq s (append s " Tv = " (string thing.Tv true) #\newline))
           (setq s (append s " Pv = " (string thing.Pv true) #\newline))
           (setq s (append s "Instructions:" #\newline))
           ;(setq s (append s (disassemble thing srcOnly: short:)))
           (setq s (append s (disassemble thing short:)))
           (setq s (stringToVector s (append "" #\return #\newline) true))))
    ;; Do we display Workspace Lambdas?
    (if (isType Workspace: thing)
        (begin
           (setq title "Active Workspace Views")
           (setq s (inspect _currentViews))))
    ;; Do we display Workspace statistics?
    (if (= true thing)
        (begin
           (setq title "Current Workspace Statistics")
           (setq s (inspect true))))
    ;; Do we have an improper argument?
    (if (= s #void)
        (error "showArgument"))
    (browse title "Hit OK when done browsing." s 0))






























;;**EXPORTKEY**:statisticsLambda
(defun statisticsLambda(legacyTable)
;; *******************************************************************
;; summary:  Statistics Lambda which collects and presents statistics
;;           on the current legacy database.
;; Args:     none   
;; Return:   true
;; *******************************************************************
  pvars:(myParent                ;; Parent Lambda object reference
         myIndex                 ;; Field statistics Dictionary (indexed by field name) 
         myFieldName             ;; Current field name
         myFieldStats            ;; Statistics records for the current field
         myLegacyTable           ;; Statistics legacy table
         ;; Constants
         (minValue -900000000000000000000)
         (maxValue  900000000000000000000)
         ;; Methods
         clearDataBase           ;; Clears the statistics database
         clearFieldStatistics    ;; Clears the statistics for the current field
         createFieldStructure    ;; Creates the structure used to store field statistics
         deleteField             ;; Deletes the field statistics record
         doClear                 ;; Clears the field statistics
         doFieldStatistics       ;; Compute the field statistics for the specified field
         doFieldWrapup           ;; Compute the final field statistics for the specified field
         doStatistics            ;; Compute the field statistics for the specified record
         getFieldNames           ;; Return a tab delimited string of all registered field names
         getFieldStatistics      ;; Return a tab delimited string of field statistics
         getFieldDelimitedString ;; Return a tab delimited string of field name and values
         getFieldValue           ;; Return the value of the registered field
         loadIndex               ;; Load the field statistics index
         parseFieldName          ;; Parse and save the field name
         saveIndex               ;; Saves the field statistics index
         setCurrent              ;; Sets myFieldStats
         updateField             ;; Update the field statistics record
         )   ;;end of persistant variables
    vars:(fieldIndex fieldCount)  
    ;; Never initialize this Lambda more than once, because the
    ;; inline child Lambdas will overlay the cloned child Lambdas
    ;; in any clone copies of the Lambda and this causes serious
    ;; confusion when the reinitialized clone begins to affect
    ;; the persistant storage of the original Lambda.
    (if (<> clearDataBase #void) (goto Continue:))
    ;; Initialize the inline child Lambdas.
    (defun clearDataBase()
       (setq myIndex (^new Dictionary:))
       (setq _Blackboard._Statistics myIndex)
       0) ;; end clearDataBase
    (defun clearFieldStatistics() 
       ;; Clear the current field statistics
       vars:(valueIndex valueList valueCount)  
       ;; We assume myFieldStats is loaded at this point.
       (if (= myFieldStats #void) (error "unknownField"))
       (setq myFieldStats.count #void)
       (setq myFieldStats.mean #void)
       (setq myFieldStats.min #void)
       (setq myFieldStats.max #void)
       (setq myFieldStats.std #void)
       (if (<> (setq valueList myFieldStats.valueCounts) #void)
           (begin
              (setq valueCount (length valueList))
              (loop for valueIndex from 0 until valueCount do
                  (setq dls valueList[valueIndex 0] #void)
                  ) ;;end value loop
              )) ;; end if
       true)
    (defun createFieldStructure()
       (setq myFieldStats (makeStructure name:        #void 
                                         firstName:   #void 
                                         middleName:  #void 
                                         lastName:    #void 
                                         type:        #void 
                                         count:       #void 
                                         mean:        #void 
                                         min:         #void 
                                         max:         #void 
                                         std:         #void 
                                         valueCounts: #void))
       ) ;; end of createFieldStructure
    (defun deleteField(fieldName) 
       (loadIndex)  
       (setq myIndex[fieldName] #void)
       (saveIndex)) ;; end of deleteField
    (defun doClear() 
       ;; Clear all the field statistics
       vars:(fieldIndex fieldCount)  
       (loadIndex)
       (setq fieldCount (length myIndex))
       (loop for fieldIndex from 0 until fieldCount do
           (setq myFieldStats myIndex[fieldIndex 1])
           (clearFieldStatistics)
           (setq myIndex[myIndex[fieldIndex 0]] myFieldStats)
           ) ;;end value loop
       (saveIndex)
       true) ;; end doClear
    (defun doFieldStatistics(theRecord) 
       ;; Compute the current field statistics
       vars:(valueIndex valueList valueCount key fieldValue)  
       ;; We assume myFieldStats is loaded at this point.
       (if (= myFieldStats #void) (error "unknownField"))
       (setq fieldValue (getFieldValue theRecord))
       (if (= fieldValue #void) (return true))
       (if (= myFieldStats.type Number:)
           (begin
              (if (not (isNumber fieldValue)) (return true))
              (+= myFieldStats.mean (getFieldValue theRecord))
              (if (= myFieldStats.min #void)
                  (setq myFieldStats.min (getFieldValue theRecord))
                  (setq myFieldStats.min (min myFieldStats.min (getFieldValue theRecord))))
              (if (= myFieldStats.max #void)
                  (setq myFieldStats.max (getFieldValue theRecord))
                  (setq myFieldStats.max (max myFieldStats.max (getFieldValue theRecord))))
              (+= myFieldStats.std (* (getFieldValue theRecord) (getFieldValue theRecord)))
              (++ myFieldStats.count)
              (if (<> (setq valueList myFieldStats.valueCounts) #void)
                  (begin
                     (setq valueCount (length valueList))
                     (loop for valueIndex from 0 until valueCount do
                         (if (<= (getFieldValue theRecord) (setq key valueList[valueIndex 0]))
                             (begin
                                (++ valueList[key])
                                (return true)
                                )) ;; end if
                         ) ;;end value loop
                     )) ;; end inner if
              )) ;; end number if
       (if (= myFieldStats.type Coded:)
           (begin
              (if (= fieldValue #void) (return true))
              (if (<> (setq valueList myFieldStats.valueCounts) #void)
                  (begin
                     (++ valueList[(getFieldValue theRecord)])
                     )) ;; end if
              (++ myFieldStats.count)
              )) ;; end coded if
       true)
    (defun doFieldWrapUp() 
       ;; Compute the final field statistics
       vars:(valueIndex valueList valueCount key)  
       ;; We assume myFieldStats is loaded at this point.
       (if (= myFieldStats #void) (error "unknownField"))
       (if (and (= myFieldStats.type Number:) (<> myFieldStats.count #void))
           (begin
              (/= myFieldStats.mean myFieldStats.count)
              (/= myFieldStats.std myFieldStats.count)
              (setq myFieldStats.std (- myFieldStats.std (* myFieldStats.mean myFieldStats.mean)))
              (setq myFieldStats.std (sqrt myFieldStats.std))
              )) ;; end number if
       true)
    (defun doStatistics(theRecord) 
       ;; Compute the field statistics for the specified record
       vars:(fieldIndex fieldCount)  
       (setq fieldCount (length myIndex))
       (loop for fieldIndex from 0 until fieldCount do
           (setq myFieldStats myIndex[fieldIndex 1])
           (doFieldStatistics theRecord)
           (setq myIndex[myIndex[fieldIndex 0]] myFieldStats)
           ) ;;end value loop
       true) ;; end doStatistics
    (defun getFieldDelimitedString(fieldName) 
       ;; Return a tab delimited string of field name and discrete values
       vars:(dls valueIndex valueList valueCount)  
       (loadIndex)
       (setq dls "")
       (setq myFieldStats myIndex[fieldName])
       (if (= myFieldStats #void) (error "unknownField"))
       (setq dls (append dls myFieldStats.name #\tab))
       (setq dls (append dls myFieldStats.type))
       (if (<> (setq valueList myFieldStats.valueCounts) #void)
           (begin
              (setq valueCount (length valueList))
              (loop for valueIndex from 0 until valueCount do
                  (setq dls (append dls #\tab valueList[valueIndex 0]))
                  ) ;;end value loop
              )) ;; end if
       dls) ;; end getFieldDelimitedString
    (defun getFieldNames() 
       ;; Return the User Names
       vars:(fieldIndex indexCount dls)  
       (loadIndex)
       (setq dls "")
       (setq indexCount (length myIndex))
       (loop for fieldIndex from 0 until indexCount do
           (setq myFieldStats myIndex[fieldIndex 1])
           (setq dls (append dls myFieldStats.name (char 09)))
           ) ;;end field loop
       (append dls "")) ;; end getFieldNames
    (defun getFieldStatistics(fieldName) 
       ;; Return the field statistics as a tab delimited string
       vars:(dls valueIndex valueList valueCount)  
       (loadIndex)
       (setq dls "")
       (setq myFieldStats myIndex[fieldName])
       (if (= myFieldStats #void) (error "unknownField"))
       (setq dls (append "count = " myFieldStats.count #\tab))
       (setq dls (append dls "type = " myFieldStats.type #\tab))
       (if (= myFieldStats.type Number:)
           (begin
              (setq dls (append dls #\tab "mean = " myFieldStats.mean))
              (setq dls (append dls #\tab "min = " myFieldStats.min))
              (setq dls (append dls #\tab "max = " myFieldStats.max))
              (setq dls (append dls #\tab "std = " myFieldStats.std))
              )) ;; end if
       (if (<> (setq valueList myFieldStats.valueCounts) #void)
           (begin
              (setq valueCount (length valueList))
              (setq dls (append dls #\tab "Value Counts"))
              (loop for valueIndex from 0 until valueCount do
                  (setq dls (append dls #\tab valueList[valueIndex 0] " = " valueList[valueIndex 1]))
                  ) ;;end value loop
              )) ;; end if
       dls)
    (defun getFieldValue(theRecord) 
       ;; Return the registered field value
       (if (= myFieldStats.lastName #void) (return theRecord[myFieldStats.name]))
       theRecord[myFieldStats.firstName][myFieldStats.middleName][myFieldStats.lastName] 
       ) ;; end getFieldValue
    (defun loadIndex()
       (setq myIndex _Blackboard._Statistics)
       (if (= myIndex #void) (clearDataBase))
       true) ;; end loadIndex
    (defun parseFieldName(fieldName)
       vars:(vec ftype)
       (loadIndex)
       (setq myFieldStats myIndex[fieldName])
       (if (= myFieldStats #void) (createFieldStructure))
       (setq myFieldStats.name fieldName)
       (setq vec (stringToVector fieldName "["))
       (setq myFieldStats.firstName (symbol vec[0]))
       (if (> (length vec) 1)
           (begin
              (setq myFieldStats.middleName (symbol (stringToVector vec[1] "].")[0]))
              (setq myFieldStats.lastName (symbol (stringToVector vec[1] "].")[1]))
              )
           (begin
              (setq myFieldStats.middleName #void)
              (setq myFieldStats.lastName #void)
              )) ;; end if
       (if (isMember myFieldStats.firstName _validFields)
           (setq ftype _validFields[myFieldStats.firstName])
           (error "unknownField"))
       (if (and (= ftype |isDictionary|:) (<> _parseFieldName #void))
           (setq ftype (_parseFieldName myFieldStats)))
       (cond
           ((= ftype |isNumber|:) (setq myFieldStats.type Number:))
           ((= ftype |isSymbol|:) (setq myFieldStats.type Coded:))
           (else (setq myFieldStats.type Other:))
           ) ;; end cond
       true) ;; end parseFieldName
    (defun saveIndex() 
       (if (= myIndex #void) (clearDataBase))
       (setq _Blackboard._Statistics myIndex)
       true) ;; end saveIndex
    (defun setCurrent(fieldName) 
       ;; Load myFieldStats
       (loadIndex)
       (setq myFieldStats myIndex[fieldName])
       (if (= myFieldStats #void) (error "noStatOnField"))
       true) ;; end setCurrent
    (defun updateField(fieldName valueList)
       (parseFieldName fieldName)
       (if (isVector valueList) (setq valueList (sort valueList <)))
       (setq myFieldStats.count #void)
       (setq myFieldStats.mean #void)
       (setq myFieldStats.min #void)
       (setq myFieldStats.max #void)
       (setq myFieldStats.std #void)
       (cond
           ((= myFieldStats.type Number:) (setq myFieldStats.valueCounts valueList))
           ((= myFieldStats.type Coded:) (setq myFieldStats.valueCounts (^new Dictionary:)))
           (else (setq myFieldStats.valueCounts #void))
           ) ;; end cond
       (if (= (type myFieldStats.valueCounts) Vector:)
           (begin
              (setq myFieldStats.valueCounts[(length myFieldStats.valueCounts)] maxValue)
              (setq myFieldStats.valueCounts (objectToDirectory myFieldStats.valueCounts #(#void)))
           )) ;; end if
       (setq myIndex[fieldName] myFieldStats)
       (saveIndex)
       true) ;; end updateField
    ;; We compute the field statistics for the legacy database.
    ;; We call the legacy librarian to perform the statistics run.
    Continue::
    (setq myParent (myself))
    (setq myLegacyTable legacyTable)
    (myLegacyTable.runStatistics doStatistics)
    ;; Compute final wrapup statistics
    (setq fieldCount (length myIndex))
    (loop for fieldIndex from 0 until fieldCount do
        (setq myFieldStats myIndex[fieldIndex 1])
        (doFieldWrapUp)
        (setq myIndex[myIndex[fieldIndex 0]] myFieldStats)
        ) ;;end field loop
    (saveIndex)
    true) ;; end of statisticsLambda







































;;**EXPORTKEY**:stockGetMatch
(defun stockGetMatch(name ST)
;; *******************************************************************
;;  summary: Returns the Quarterly History Smarttable index for the
;;           specified stock symbol.
;;  args:    The symbol of the stock.
;;           ST      The Quarterly History tableLambda.
;; *******************************************************************
   vars:((low 0) (high 0) (mean 0) ST STlen i index)
   ;;  Prepare the specified Stock symbol.
   (setq name (upcase (trim name)))
   ;; Perform binary search on the Smarttable symbol column
   (begin
      (setq STlen ST.rowCount)
      (setq high (-1+ STlen))
      (setq mean (divi (1+ (- high low)) 2))
      (while (<= low high) do
             (if (= ST[Symbol: mean] name)
             (begin
                 (setq index mean)
                 (setq low (1+ high)))
                 (if (< ST[Symbol: mean] name)
                     (begin
                         (setq low (1+ mean))
                         (setq mean (+ low (divi (1+ (- high low)) 2))))
                     (begin
                         (setq high (-1+ mean))
                         (setq mean (+ low (divi (1+ (- high low)) 2))))))))
   ;;  Return the specified matching Smarttable Index (if any).
   index) ;; end stockGetMatch


























;;**EXPORTKEY**:tableLambda
(defun tableLambda()
;; ********************************************************************
;; summary:  This Lambda creates, maintains, and operates as an in 
;;           memory relational table. The relational table is a series
;;           of attributed rows. The table can be sorted, we can have 
;;           multiple views, subsets of records can be found, etc.
;; Parms:    none  
;; return:   true
;; ********************************************************************
   pvars:(activeIndex              ;; Currently-active index -- used to get rows by key
          colCount 	           ;; The number of columns
          colVector                ;; Structure of attribute names (columns)
          exportSW                 ;; True if an exportTab is in progress
          fileID                   ;; Contains a fileID if an importTab is in progress
          indexSW                  ;; True if indexes are to be automatically updated
          importSW                 ;; True if an importTab is in progress
          myIndexes                ;; Dictionary of indexes
          myParent                 ;; The parent table Lambda
          rowCount      	     ;; The number of rows
          rowVector                ;; Vector of attributed relation rows
          saveVector               ;; Vector of saved relational rows
          showLimit                ;; The maximum number of records to show
          sparseSW                 ;; True if tableLambda manages sparse records
          uniqueIndexSW            ;; True if tableLambda enforces unique indexes
          ;; Methods list =====================================================
          addColumn 	           ;; Adds a new column name to the relation
          addRow 	                 ;; Adds a new row to the relation
          clearRows	           ;; Clears all the rows in the relation
          defColumns 	           ;; Defines a new column structure for the relation
          delimitedCells           ;; Return a tab delimited string of cell values
          doAddRow	           ;; Inserts an empty row to the relation
          doCellText	           ;; Enters text data into the cell row and column specified
          doClear	                 ;; Clears all the rows and columns in the relation
          doDeleteRow	           ;; Deletes a row from the relation
          doFind	                 ;; Find a matching row value in the relational table
          doIndex                  ;; Create and an index on the specified column
          doReindex                ;; Recreate all existing indexes
          doSort                   ;; Sort the relation on the specified column
          errorStop                ;; Clean up after any errors which may occur during operations
          exportTab                ;; Export a tab delimited .sbf format file into the relation
          find                     ;; Select matching rows in the relational table
          findAll                  ;; Restore the original rows in the relational table
          findRow                  ;; Return the first row with the specified value
          forEach                  ;; Operate on and possibly alter each row in a table Lambda
          getColumnHeaders         ;; Return a tab delimited string of column headers
          getRowBlock              ;; Return a tab delimited string of row values for a specified column
          getRowByKey              ;; Return a vector of rows for a given key value
          importTab                ;; Import a tab delimited .sbf format file into the relation
          len                      ;; Return the number of rows in the relation
          new                      ;; Create a new relational table
          ref1                     ;; Return a single row from the relational table
          ref2                     ;; Return a single cell from the relational table
          refExport                ;; Return a single export formatted row from the relational table
          refFormattedRow          ;; Return a single display formatted row from the relational table
          refColCount              ;; Return the number of columns in the relational table
          refRowCount              ;; Return the number of rows in the relational table
          saveState                ;; Save the current row Vector state as the permanent state
          saveSwap                 ;; Swap the row Vector during save-load
          set1                     ;; Import a record during the importTab operation
          set2                     ;; Set a single cell of the relational table
          setIndexTo               ;; Set the activeIndex 
          setIndexUpdate           ;; Set the automatic update of indexes on/off
          setImport                ;; Called by ImportTab to installed a filled vector into the relational Table
          setSparse                ;; Set the tableLambda for sparse records
          setUniqueIDX             ;; Set the tableLambda for unique indexes
          show                     ;; Display the rows in the relation
          smarttableToTableLambda   ;; Save the specified Smarttable into the table Lambda.
          sort                     ;; Sort the rows in the relation
          truncate                 ;; Truncate rows in the relational table
          truncateEnds             ;; Truncate rows in the relational table
          updateIndex              ;; Update index for specified column with new value
          updateRowByKey           ;; Make sure a row for the specified key exists
          ) ;; end of persistent variables
   ;; Never initialize this Lambda more than once, because the
   ;; inline child Lambdas will overlay the cloned child Lambdas
   ;; in any clone copies of the Lambda and this causes serious
   ;; confusion when the reinitialized clone begins to affect
   ;; the persistant storage of the original Lambda.
   (if (<> addColumn #void) (goto Continue:))
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
   (defun clearRows()
      (setq rowVector (^new Vector: object: 0))
      (setq saveVector #void)
      (setq rowCount 0)
      (if (> (length myIndexes) 0)
         (setq myIndexes (objectToDictionary myIndexes (^new Vector: 0))))
      true) ;; end clearRows
   (defun defColumns(newColumns)
      (doClear)
      (if (isVector newColumns)
          (setq colVector (objectToStructure newColumns #(#void)))
          (setq colVector (objectToStructure newColumns))
          ) ;; end new column coercion if
      (setq colCount (length colVector))
      colCount) ;; end defColumns
   (defun delimitedCells(startRow endRow startCol endCol ...)
      ;; WJH - added optional arg to trigger 1-based row numbers
      ;; WJH - added optional arg to suppress row numbers
      vars:(rowIndex colIndex dls tab cellValue record) 
      ;; Return a tab delimited string of cell values for
      ;; all cells between the following rows and columns.
      (setq dls "")
      (setq tab (append "" (char 09)))
      (if (> startRow endRow) (return dls))
      (if (<= rowCount startRow) (return dls))
      (loop for rowIndex from startRow to endRow do
         ;; WJH - optioal row number suppression
         (if (= (argCount) 6)
            () ;; do nothing
         else
            ;; WJH - optional 1-based row numbers
            (setq dls (append dls (if (= (argCount) 5) (+ rowIndex 1) rowIndex) (char 09)))
         )
         (setq record (refFormattedRow rowIndex))
         (loop for colIndex from startCol until endCol do
             (setq cellValue record[colIndex])
             (if (isString cellValue) (setq cellValue (substitute cellValue tab " ")))
             (if (= cellValue #void) (setq cellValue ""))
             (if (isVector cellValue) (setq cellValue (string cellValue true)))
             ;(setq cellValue (append "" cellValue)) 
             ;(setq cellValue (substitute cellValue (char 09)  " "))
             ;(setq cellValue (substitute cellValue (char 13)  " "))
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
      (setq myIndexes (^new Dictionary:))
      (setq activeIndex #void)
      true) ;; end doClear
   (defun doDeleteRow(rowIndex colIndex) (error "NoAccess")) ;; WJH - be sure to update indexes
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
   (defun doReindex() 
      vars:(i) 
      (loop for i from 0 until (length myIndexes) do 
         (doIndex myIndexes[i 0]))) ;; end doReindex
   (defun doSort(columnName sortOrder) 
      vars: (source Lambda)
      (setq source (append "(lambda(x y) (" sortOrder "  x.|") )
      (setq source (append source columnName "| y.|"))
      (setq source (append source columnName "|))"))
      (setq Lambda (eval source))
      (sort Lambda)
      ;; Update indexes if switch is set true
      (if (= true indexSW) (doReindex))
      rowCount) ;; end doSort
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
      ;(onError errorStop)
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
      (if (<= rowCount 0) (return rowCount))
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
   (defun findRow(col value)
      vars:(rowIndex colIndex)
      (setq colIndex (makeSymbol col))
      ;; Return the first row with the chosen value in the column.
      (loop for rowIndex from 0 until rowCount do
          (if (= value (string rowVector[rowIndex][colIndex]))
              (return rowIndex))
          ) ;; end of loop
      ;; If we get to here, we did not find the specified value anywhere.
      false) ;; end of findRow
   (defun forEach(rowLambda)
      vars:(rowIndex newIndex n vec)
      (if (<= rowCount 0) (return rowCount))
      ;; Operate on and possibly alter each row in the table.
      (setq n (length rowVector))
      (loop for rowIndex from 0 until n do
          (setq rowVector[rowIndex] (rowLambda rowVector[rowIndex]))
          ) ;; end loop
      rowCount) ;; end find
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
      ;; (onError errorStop)
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
      (setq myIndexes (^new Dictionary:))
      (setq saveVector #void)
      (setq colCount 0)
      (setq rowCount 0)
      (setq showLimit 10)
      (setq importSW false)
      (setq exportSW false)
      (setq indexSW false)
      (setq fileID #void)
      true) ;; end of new
   (defun ref1(rowOrMember)
      (if (isSymbol rowOrMember) (return (myself)[Pv:][rowOrMember])) ;; Allows Lambda polymorphism.
      (if (not (isNumber rowOrMember)) (error "invalidKey"))
      ;;;;;;;;;;;;;;;; WHJ - this section commented-out to match MFK's test Lambda
      ;; If we are importing, we will return a vector (if there are no columns defined),
      ;; or we will return an empty record Structure (if there are columns defined).
      ;(if (= importSW true)
      ;    (if (= colVector #void)
      ;        (return (^new Vector: 0))
      ;        (return (copy colVector)))
      ;    ) ;; end importing if
      ;; If we are exporting, we will return records formatted for export.
      ;(if (= exportSW true) (return (refExport rowOrMember)))
      ;;;;;;;;;;;;;
      ;; We are now retrieving a specific indexed row of the table.
      rowVector[rowOrMember]) ;; end of ref1
   (defun ref2(colName rowIndex) rowVector[rowIndex][colName])
   (defun refColCount() colCount)
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
   (defun refImport(row)
      (if (not (isNumber row)) (error "invalidKey"))
      ;; Return a an empty vector (if there are no columns defined),
      ;; or return an empty record Structure (if there are columns defined).
      (if (= colVector #void)
          (return (^new Vector: 0))
          (return (copy colVector)))
      ) ;; end of refImport

   (defun refRowCount() (if (= exportSW true) (addi rowCount 3) rowCount))
   (defun saveState() (setq saveVector rowVector))
   (defun saveSwap(theVector)
      vars:(holdArea)
      (if (= saveVector #void)
          (setq holdArea rowVector)
          (setq holdArea saveVector))
      (setq rowVector theVector) 
      (setq saveVector theVector) 
      holdArea) ;; end of saveSwap
   (defun set1(rowIndex recordVector)
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
      rowCount) ;; end of set1
   (defun set2(colName rowIndex newValue) 
      vars:(oldValue)
      (if (= indexSW false)
         (return (setq rowVector[rowIndex][colName] newValue))
      else
         (begin 
            (setq oldValue rowVector[rowIndex][colName])
            (setq rowVector[rowIndex][colName] newValue)
            (if (<> myIndexes[colName] #void)
               (updateIndex colName newValue oldValue rowIndex)
            )
         )
      )
      rowVector[rowIndex]) ;; end of set2
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
   (defun setIndexTo(idxName)
      (if (<> #void myIndexes[idxName])
         (begin 
            (setq activeIndex idxName)
            (return true)
         )
      else
         (begin 
            (setq activeIndex #void)
            (return false)
         )
      )) ;; end of setIndexTo
   (defun setIndexUpdate(flag)
      (setq indexSW flag) true) ;; end of setIndexUpdate
   (defun setSparse(SW) (clearRows) (setq sparseSW SW))
   (defun setUniqueIDX(flag) 
      (setq uniqueIndexSW flag)) ;; end of setUniqueIDX
   (defun show(startIndex) 
      vars:(i n) 
      (setq n (min (length rowVector) (addi startIndex showLimit)))
      (loop for i from startIndex until n do
          (writeln "[" i "] " (refFormattedRow i))
          ) ;; end loop
      true) ;; end show
   (defun sort(sortLambda)
      (if (= saveVector #void) (setq saveVector (copy rowVector)))
      (if (<= rowCount 0) (return rowCount))
      (^sort rowVector sortLambda)
      rowCount) ;; end sort
   (defun truncate(rowLimit)
      vars:(rowIndex newIndex n vec)
      ;; Truncate the relational table to the specified number of rows.
      (setq rowLimit (max (min rowLimit (length rowVector)) 0))
      (if (<= rowCount rowLimit) (return rowCount))
      (resize rowVector rowLimit)
      (setq rowCount (length rowVector))
      ;; Reindex if flag is set
      (if (= true indexSW) (doReindex))
      rowCount) ;; end truncate
   (defun truncateEnds(begRowLimit endRowLimit)
      vars:(rowIndex newIndex n vec endRowIndex)
      (if (= saveVector #void) (setq saveVector (copy rowVector)))
      (if (<= rowCount 0) (return rowCount))
      ;; Select only those rows in between the begin and end row limits.
      (setq n (length rowVector))
      (setq endRowIndex (subi n endRowLimit))
      (setq vec (^new Vector: object: 0))
      (setq newIndex -1)
      (loop for rowIndex from begRowLimit until endRowIndex do
          (setq vec[(++ newIndex)] rowVector[rowIndex])
          ) ;; end loop
      (setq rowVector vec)
      (setq rowCount (length rowVector))
      ;; Reindex if flag is set
      (if (= true indexSW) (doReindex))
      rowCount) ;; end truncateEnds
   ;; Initialize the Lambda and clear the relational table.
   Continue::
   (setq myParent (myself))
   (new)
   myParent) ;; end of tableLambda
    








;;**EXPORTKEY**:tableLambda:addRow
(defChildLambda tableLambda:addRow(newRow)
   vars:(newRecord idxObj idxNum idxColumn idxNode)
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
   ;; Update indexes if switch is set true
   (if (= indexSW true)
      (if (> (length myIndexes) 0) 
         (loop for idxNum from 0 until (length myIndexes) do
            ;; Get key name of index
            (setq idxColumn myIndexes[idxNum 0])
            ;; Retrieve index object
            (setq idxObj myIndexes[idxNum])
            ;; Retrieve value node
            (setq idxNode idxObj[newRecord[idxColumn]])
            ;; Test to see if node found
            (if (<> #void idxNode) 
               (begin 
                  ;; Found - add new row to existing idx value node
                  (setq idxNode[(length idxNode)] rowCount)   
                  (setq idxObj[newRecord[idxColumn]] idxNode)
               )
            else
               ;; Not found - create new idx value node,
               ;; but don't index #voids 
               (if (<> #void newRecord[idxColumn])
                  (begin
                     (setq idxNode (^new Vector: 0))
                     (setq idxNode[0] rowCount)
                     (setq idxObj[newRecord[idxColumn]] idxNode)
                  )
               )
            )
            ;; Save the updated index
            (setq myIndexes[idxNum] idxObj)
         )  
      )
   )
   ;; Advance row count 
   (setq rowCount (length rowVector))
   rowCount) ;; end addRow








;;**EXPORTKEY**:tableLambda:doIndex
(defChildLambda tableLambda:doIndex(columnName)
   ;;
   vars: (row            ;; Row index into rowVector
          value          ;; Current index value
          limit          ;; rowVector loop limit
          idxVector      ;; Vector of row numbers for this index value
          thisIndex      ;; Index dictionary being built
         )
   ;;
   ;;
   ;; Initialize local variables
   (setq columnName (symbol columnName))
   (setq limit (length rowVector))
   (setq thisIndex (^new Dictionary:))
   ;;
   ;; Scan the rowVector, building the index
   (loop for row from 0 until limit do
      ;; Retrieve value
      (setq value rowVector[row][columnName])
      ;; Test to see if value is indexed
      (if (<> #void (setq idxVector thisIndex[value]))
         ;; Add this row to value node
         (setq idxVector[(length idxVector)] row)
      else
         (begin
            ;; Create new value node
            (setq idxVector (^new Vector: 0))
            ;; Index this row 
            (setq idxVector[0] row)
         )
      )
      ;; Save altered value node for this value
      (setq thisIndex[value] idxVector)
   )
   ;; Save newly-created index, overwriting any existing version 
   (setq myIndexes[columnName] thisIndex)
   true) ;; end of doIndex








;;**EXPORTKEY**:tableLambda:getRowByKey
(defChildLambda tableLambda:getRowByKey(keyValue)
;; *************************************************************************************
;; summary:  This procedure gets a vector of rows from the table Lambda. The key value must
;;           be a value for the tableLambda's current index 
;;                               
;; args:     keyValue:           The key object whose attributes are used to find the row.
;;           
;; return:   resultVector        A vector of row objects who match the key.
;;                               Length of return is zero if no rows match the key.
;;                               
;; *************************************************************************************
   vars:(
         resultVect
         idxObj
         idxNode         
         rowIndex
        )
   ;; Initialize result
   (setq resultVect (^new Vector: 0))
   ;; If there are no rows, we can't find ours.
   (if (= rowCount 0) (return resultVect))
   ;; Try to retrieve the index node for this value
   (if (<> #void (setq idxObj myIndexes[activeIndex]))
      (setq idxNode idxObj[keyValue])
   )
   ;; If value not in index, no rows can match
   (if (= #void idxNode) (return resultVect))
   ;; Build vector of matching rows
   (loop for rowIndex from 0 until (length idxNode) do
      (setq resultVect[rowIndex] rowVector[idxNode[rowIndex]])
   )
   ;; return the resultVect
   resultVect) ;; end getRowByKey
   




























;;**EXPORTKEY**:tableLambda:updateIndex
(defChildLambda tableLambda:updateIndex(colName newValue oldValue rowIndex)
;; *************************************************************************************
;; summary:  This procedure updates the specified index. Row references are removed from
;;           the previous value's node.
;;           
;; args:     colName:           The name of the column and index key
;;           newValue:          The key object for the new value's index node
;;           oldValue:          The key object for the old value's index node
;;           rowIndex:          The row number being indexed
;;           
;; return:   true        
;;                               
;; *************************************************************************************
   pvars:(idxObj idxNode rowVect oldVect)
   ;; 
   ;; Update index if it exists
   (if (= #void (setq idxObj myIndexes[colName]))
      ;; unknown index error
      (error "unknownIndex")
   else
      (begin 
         ;; Remove row from old value's node, if present
         (if (<> #void (setq oldVect idxObj[oldValue]))
            (setq idxObj[oldValue] (delete rowIndex oldVect))
         )
         ;; Retrieve index node for new value
         (setq idxNode idxObj[newValue])
         (if (<> #void idxNode) 
            ;; Do nothing if row is already indexed
            (if (= (member rowIndex idxNode) false)
               (begin 
                  ;; Add row to existing idx value node
                  (setq idxNode[(length idxNode)] rowCount)   
                  (setq idxObj[newValue] idxNode)
               )
            )
         else
            ;; Create new idx value node
            (begin
               (setq idxNode (^new Vector: 0))
               (setq idxNode[0] rowIndex)
               (setq idxObj[newValue] idxNode)
            )
         )
         ;; Save the updated index
         (setq myIndexes[colName] idxObj)
      )
   )  
   true) ;; end updateIndex








;;**EXPORTKEY**:tableLambda:updateRowByKey
(defChildLambda tableLambda:updateRowByKey(keyValue)
;; *************************************************************************************
;; summary:  This procedure gets a Row from the specified Table
;;           using the values in the key attributes of the key object. The key object
;;           may be any object which can be indexed by the active index.
;;           If there is no matching row, a new row is added with
;;           the specified key values.
;; args:     key:                The key object whose attributes are used to find the row.
;;
;; return:   row:                The Row whose key attributes match the key object,
;;                               a new Row whose key attributes are set to match 
;;                               the key object, or an error if there is more than one
;;                               Row whose key attributes match the key object.
;; *************************************************************************************
   vars:(
         row 
         resultVect
        )
   ;; If there are matching rows, then return them.
   (setq resultVect (getRowByKey keyValue))
   (if (> (length resultVect) 0) (return resultVect))
   ;;
   ;; Get the next available row index.
   (setq row (objectToDictionary colVector))
   (setq row[activeIndex] keyValue)
   (addRow row)
   (setq resultVect[0] row)
   resultVect) ;; end updateRowByKey



























;;**EXPORTKEY**:tableDbms
(defun tableDbms(objRepository)
;; *******************************************************************
;; Summary:  Database Librarian Lambda which manages a simple table 
;;           database and controls the table database schema. The 
;;           table schema is similar to the relational model except 
;;           there is only one table in the database.
;; Keys:     _colVector	        The table's vector of column names.
;;           _blockCount	The table's current block count.
;;           _blockSize 	The number of records per table block.
;;           _exportLayout   	The table's export record fields layouts.
;;           _importFixup   	The table's import record fields fixup Lambda.
;;           _name    		The table's current record count.
;;           _recordCount	The table's current record count.
;;           _recordStructure	The table's record structure.
;;           _tableDate		The table's date (assigned at creation time).
;;           _validFields   	The table's valid fields and types (necessary for query building).
;;           {Positive Number}	The record blocks are assigned sequential numbers.
;;           {Negative Number}	All temporary record blocks are assigned negative numbers.
;; Depends:  excludCriteriaLambda
;;           rulesLib
;;           prodCriteriaLambda
;;           selectCriteriaLambda
;; Args:     objRepository  The table database repository object.
;; Return:   self           The table database managers.
;; *******************************************************************
    ;; Persistent Variables
    pvars:(myOR                ;; The table object repository
           myIndex             ;; The index of the table object repository
           (myResident false)  ;; The Lambda librarian residence switch
           self                ;; The parent Lambda
           backupPathName      ;; The path and file name of the table backup database
           blockKEY            ;; The retrieval key for the current block
           (blockSize 500)     ;; The table database blocking factor
           checkPointPathName  ;; The path and file name of the table checkpoint database
           colVector           ;; The table database column vector
           currentBlock        ;; The current block of table database records
           currentExportLayout ;; The current export layout record
           exportFieldData     ;; The current export field metadata dictionary 96.06.26 WJH
           exportLayouts       ;; The current export layout dictionary
           importDate          ;; The date associated with the imported table data
           importFileID        ;; The file identifier of the table tab delimited file
           importFixup         ;; The Lambda which fixes up each imported record       
           importFixupSW       ;; True if each imported record is also to be fixed up       
           importingSW         ;; True if we are importing now switch
           importNilRows       ;; Count of nil rows which have been ignored by import
           maxRecordLimit      ;; The maximum number of table records to import
           name                ;; The name of the database table
           recordCount         ;; The count of records in the table database
           recordNUM           ;; The table database current record number
           recordStructure     ;; Blank record structure for table database records
           sbfFormatSW         ;; True if we are importing an .SBF file
           tablePathName       ;; The path and file name of the table database
           updatingDirty       ;; Update has occurred switch
           updatingSW          ;; Update transaction in progress switch
           validFields         ;; The table's valid field names and types (necessary for query building).
           ;; Private Child Lambdas
           _makeBlockList      ;; Return a list of contiguous block keys ready to be merged
           _mergeBlockList     ;; Merge a list of blocks into a sequential run of blocks
           _readBlockList      ;; Return the next record from a list of blocks
           _renameBlocks       ;; Rename the keys for the specified list of blocks
           ;; Public Child Lambdas
           abortTrans          ;; Manage repository abort transaction
           addRow              ;; Add a row to the table database
           backup              ;; Backup or restore the table database repository
           beginTrans          ;; Manage repository begin transaction
           Checkin             ;; Manage repository check in
           checkPoint          ;; CheckPoint or restore the current rows in the table database
           cnvColHeaders       ;; Convert the valid fields into a column header vector
           commitTrans         ;; Manage repository commit transaction
           copyDatabase        ;; Copy one table database to another table database (with defragmentation)
           copyFile	       ;; Copy one table database file to another
           delimitedCells      ;; Return block of tab delimited field values for display
           doAddRow            ;; Add a row to the table database
           doCellText          ;; Add text to a cell in the table database
           doClear             ;; Clear the table database
           doDeleteRow         ;; Delete a row from the table database
           doFind              ;; Find a row in the table database
           doSort              ;; Sort the table database
           exportCells         ;; Return block of tab delimited field values for export
           fieldKeys           ;; Valid keys (for specified Dictionary field) as tab delimited string
           fieldSubfields      ;; Valid subfield names (for specified Dictionary field) as tab delimited string
           fieldSubtypes       ;; Valid subfield types (for specified Dictionary field) as tab delimited string
           fieldTypes          ;; Field names and types as tab delimited string
           findAll             ;; Acts as a noop for backward compatibility (see restore)
           findRow             ;; Find a row in the table database
           fixupColumnHeaders  ;; Fix column headers if they are broken
           flush               ;; Complete any pending block write operations
           getColumnHeaders    ;; Return the column headers as a tab delimited string
           getColVector        ;; Return the table data column vector
           importFixup         ;; Fixup the imported table data record
           importLegacyData    ;; Import manager
           importTab           ;; Import manager for tab delimited data
           len                 ;; Return the number of records in the table
           queryBuilder        ;; Converts query specification text into a table query Lambda
           refColCount         ;; Return the number of columns in the table
           refColName          ;; Return the specified column name
           refColValues        ;; Return a column values vector
           refRow              ;; Return a row at random
           refRowCount         ;; Return the number of rows in the table
           refRowSeq           ;; Return a row sequentially
           ref1                ;; Return the specified row
           reset               ;; Reset the current block to #void
           restart             ;; Reset the record and block count to zero
           restore             ;; Restore all original rows in the table database
           runCriteria         ;; Run the specified selection criteria
           runProdSelection    ;; Run the specified Production selection strategy
           runSelection        ;; Run the specified selection strategy
           runStatistics       ;; Run statistics for the current table database
           select              ;; Select only records which match a lambda expression
           setRowSeq           ;; Save a row sequentially
           setup               ;; Set the database backup, checkpoint, block size, and valid fields.
           set2                ;; Set a field (not used)
           set1                ;; Add a row during import processing
           smarttableToTableDbms;; Save Smarttable into table database
           sort                ;; Sort the table database
           switchUser          ;; Restore the table database for a new user
           tableLambdaToTableDbms;; Save tableLambda into table database
           tableToHistoryLambda ;; Return table database as a history time slice tableLambda
           tableToSmarttable   ;; Return table database as a Smarttable
           tableToTableLambda   ;; Return table database as a tableLambda
           truncate            ;; Truncate rows in the table database
           updateBegin         ;; Begin an update transaction on the table database 
           updateEnd           ;; Terminate an update transaction on the table database
           updateRead          ;; Read a table database row for later update
           updateReadStructure ;; Read a table database row (converted to a Structure) for later update
           updateWrite         ;; Write a table database row previously read for update
           ) ;; end of pvars
    ;; Never initialize this Lambda more than once, because the
    ;; inline child Lambdas will overlay the cloned child Lambdas
    ;; in any clone copies of the Lambda and this causes serious
    ;; confusion when the reinitialized clone begins to affect
    ;; the persistant storage of the original Lambda.
    (if (<> ref1 #void) (goto Continue:))
    ;; Initialize the inline child Lambdas.
    (defun abortTrans() (abortTransaction myOR))
    (defun addRow(record) (setRowSeq recordCount record))
    (defun backup(backupSW)
	(if (or (= backupPathName "") (= backupPathName #void))
	    (error "tMgr:NoPath"))
        (abortTransaction myOR)
        (if backupSW
            (begin
               ;(copyFile tablePathName backupPathName)
               (copyDatabase backupPathName)
               ) ;; end user backup from table database
            (begin
               (copyFile backupPathName tablePathName)
               (beginTransaction myOR)
               (setq recordCount myIndex._recordCount)
               (setq recordStructure myIndex._recordStructure)
               (commitTransaction myOR)
               ) ;; end restore from table backup database
            ) ;; end if
        ) ;; end backup
    (defun beginTrans() (beginTransaction myOR))
    (defun Checkin(title) (error "NoAccess"))
    (defun checkPoint(checkptSW)
        (if (or (= checkPointPathName "") (= checkPointPathName #void))
            (error "tMgr:NoPath"))
        (abortTransaction myOR)
        (if checkptSW
            (begin
               ;(copyFile tablePathName checkPointPathName)
               (copyDatabase checkPointPathName)
               ) ;; end user check point from table database
            (begin
               (copyFile checkPointPathName tablePathName)
               (beginTransaction myOR)
               (setq recordCount myIndex._recordCount)
               (setq recordStructure myIndex._recordStructure)
               (commitTransaction myOR)
               ) ;; end restore from user check point
            ) ;; end if
        ) ;; end checkPoint
    (defun cnvColHeaders()
        vars:(fieldIndex fieldCount colHeaders)
	    (if (<> (type validFields) Structure:)
	        (return (^new Vector: 0)))
	    (setq fieldCount (length validFields))
        (setq colHeaders (^new Vector: fieldCount))
        (loop for fieldIndex from 0 until fieldCount do
            (setq colHeaders[fieldIndex] validFields[fieldIndex 0])
            ) ;; end loop
        colHeaders) ;; end cnvColHeaders
    (defun commitTrans() (commitTransaction myOR))
    (defun doAddRow(count row) (error "NoAccess"))
    (defun doCellText(col row value) (error "NoAccess")) 
    (defun doClear() true) ;; Only import can clear.
    (defun doDeleteRow(count row) (error "NoAccess"))
    (defun doFind(colName startRow value) (error "NoAccess"))
    (defun doSort(columnName sortOrder) (error "NoAccess"))
    (defun exportLayoutCreate()                                         ;; 96.06.26 WJH
        (setq currentExportLayout (makeStructure name:        #void 
                                                 type:        #void 
                                                 typeData:    #void
                                                 fieldList:   #void 
                                                 fieldData:   #void))
        true) ;; end of exportLayoutCreate
    (defun findAll() true)
    (defun findRow(col value) (error "NoAccess"))
    (defun fixupColumnHeaders() 
    ;; Fix column headers if they are broken.
        vars:(i j n)
        (setq colVector (cnvColHeaders))
        (setq recordStructure (objectToStructure colVector #(#void)))
        (if (<> myIndex #void) 
            (setq myIndex._colVector colVector)
            (begin
                (beginTransaction myOR) 
                (setq myIndex._colVector colVector)
                (setq myIndex._recordStructure recordStructure)
                (commitTransaction myOR)))
        colVector) ;; end fixupColumnHeaders
    (defun flush()
        ;; Do we need to write the current block?
        (if (<> currentBlock #void)
            (begin
               (setq myOR[blockKEY] currentBlock)
               (setq myIndex._recordCount recordCount)
               (setq myIndex._blockCount (add1 blockKEY))))
        true) ;; end of flush
    (defun getColumnHeaders(startCol endCol) 
        vars:(i getRow getCol dls cellValue)  
        (if (<> myIndex #void) 
            (setq colVector myIndex._colVector)
            (begin
                (beginTransaction myOR) 
                (setq colVector myIndex._colVector)
                (abortTransaction myOR)))
        ;; Return a tab delimited string for the client viewer.
        (setq dls "")
        (loop for i from startCol until endCol do
            (setq dls (append dls colVector[i] (char 09)))
            ) ;;end for loop
        (append dls "")) ;; end of getColumnHeaders
    (defun getColVector() 
        (if (<> myIndex #void) 
            (setq colVector myIndex._colVector)
            (begin
                (beginTransaction myOR) 
                (setq colVector myIndex._colVector)
                (abortTransaction myOR)))
        colVector) ;; end of getColVector
    (defun len()
        (if (<> myIndex #void) 
            (setq recordCount myIndex._recordCount)
            (begin
                (beginTransaction myOR) 
                (setq recordCount myIndex._recordCount)
                (abortTransaction myOR)))
        recordCount) ;; end of len
    (defun refColCount()
        vars:(count)
        (if (<> myIndex #void) 
            (setq count (length myIndex._colVector))
            (begin
                (beginTransaction myOR) 
                (setq count (length myIndex._colVector))
                (abortTransaction myOR)))
        count) ;; end of len
    (defun refColKeyness(col) formula:)
    (defun refColName(col)
        vars:(name)
        (if (<> myIndex #void) 
            (setq name myIndex._colVector[col])
            (begin
                (beginTransaction myOR) 
                (setq name myIndex._colVector[col])
                (abortTransaction myOR)))
        name) ;; end of refColName
    (defun refColType(col) "G")
    (defun refRow(row)
        vars:(blockKEY recordNUM block record)
        ;; Compute block and record numbers.
        (setq blockKEY (divi row blockSize))
        (setq recordNUM (modi row blockSize))
        ;; Retrieve block
        (setq block myOR[blockKEY])
        ;; Retrieve record
        (if (= block #void)
            (setq record #void)
            (setq record (objectToStructure (copy recordStructure) block[recordNUM])))
        record) ;; end of refRow
    (defun refRowCount() (len))
    (defun refRowSeq(row)
        vars:(record newBlockKEY)
        ;; Compute new block and record numbers.
        (setq newBlockKEY (divi row blockSize))
        (setq recordNUM (modi row blockSize))
        ;; Load new block of records?
        (if (or (= row 0)
                (<> newBlockKEY blockKEY))
            (begin
               (setq blockKEY (divi row blockSize))
               (setq currentBlock myOR[blockKEY])
               )) ;; end if
        ;; Retrieve record
        (if (= currentBlock #void)
            (setq record #void)
            (setq record (objectToStructure (copy recordStructure) currentBlock[recordNUM])))
        record) ;; end of refRowSeq
    (defun ref1(rowOrMember)
       ;; If we are importing, we will return a vector (if there are no columns defined),
       ;; or we will return an empty record Structure (if there are columns defined).
       (if (= importingSW true)
           (begin
              ;; Never import more than the maximum number of records
              (if (>= rowOrMember maxRecordLimit) (error "TooManyRecs"))
              (if (= colVector #void)
                  (return (new Vector: 0))
                  (return (copy recordStructure))))
           ) ;; end importing if
       ;; If we are not importing, we will return a record (for numeric indices), or
       ;; we will return a member of this Lambdas own persistent variables (for symbolic indices).
       (if (isNumber rowOrMember) (refRow rowOrMember) (myself)[Pv:][rowOrMember])) ;; Allows Lambda polymorphism.
    (defun ref2(col row)
        vars:(record value)
        ;; Retrieve block
        (setq record (refRow row))
        (if (= record #void)
            (setq value #void)
            (setq value record[col]))
        value) ;; end of ref2
    (defun reset()
        ;; Reset the current block.
        (setq blockKEY #void)
        (setq currentBlock #void)
        (setq recordNUM 0)
        recordCount) ;; end of reset
    (defun restart()
        ;; Reset the record and block count to zero.
        (setq blockKEY #void)
        (setq currentBlock #void)
        (setq recordNUM 0)
        (setq recordCount 0)
        (setq myIndex._recordCount recordCount)
        (setq myIndex._blockCount 0)
        recordCount) ;; end of reset
    (defun restore()
        (abortTransaction myOR) 
        (copyFile backupPathName tablePathName)
        (beginTransaction myOR)
        (setq recordCount myIndex._recordCount)
        (setq recordStructure myIndex._recordStructure)
	    (if (<> myIndex._blockSize #void) (setq blockSize myIndex._blockSize))
        (commitTransaction myOR)
        recordCount) ;; end restore
    (defun runCriteria(selectCriteria)
        ;; Run the specified selection criteria.
        vars:(algorithm)
        (setq algorithm (queryBuilder selectCriteria))
        (algorithm self)
        recordCount) ;; end runCriteria
    (defun runProdSelection(selectName)
        vars:(selectSpec algorithm)
        ;; Get the specified selection criteria specification.
        (prodCriteriaLambda)
        (setq selectSpec (prodCriteriaLambda.Pv.getCriteria selectName))
        (setq algorithm (queryBuilder selectSpec))
        (algorithm self)
        recordCount) ;; end runProdSelection
    (defun runSelection(selectName)
        vars:(selectSpec algorithm)
        ;; Get the specified selection criteria specification.
        (selectCriteriaLambda)
        (setq selectSpec (selectCriteriaLambda.Pv.getCriteria selectName))
        (setq algorithm (queryBuilder selectSpec))
        (algorithm self)
        recordCount) ;; end runSelection
    (defun setColKeyness(col keyness) (error "NoAccess"))
    (defun setColName(col name)
        (if (<> myIndex #void) 
            (setq myIndex._colVector[col] name)
            (begin
                (beginTransaction myOR) 
                (setq myIndex._colVector[col] name)
                (abortTransaction myOR)))
        name) ;; end of setColName
    (defun setColType(col type) (error "NoAccess"))
    (defun setRowSeq(row record)
        vars:(newBlockKEY)
        ;; Update the record count.
        (setq recordCount (max (add1 row) recordCount))
        ;; Compute new block and record numbers.
        (setq newBlockKEY (divi row blockSize))
        (setq recordNUM (modi row blockSize))
        ;; Load new block of records?
        (if (or (= row 0)
                (<> newBlockKEY blockKEY)
                (= currentBlock #void))
            (begin
               (setq blockKEY (divi row blockSize))
               (setq currentBlock myOR[blockKEY])
               )) ;; end if
        (if (= currentBlock #void)
            (setq currentBlock (new Vector: object: 1)))
        ;; Save record into current block.
        (setq currentBlock[recordNUM] (objectToDictionary record))
        ;; Save current block?
        (if (<> blockKEY (divi (addi row 1) blockSize))
            (begin
               (commitTransaction myOR)
               (setq myOR[blockKEY] currentBlock)
               (beginTransaction myOR)
               (setq myIndex._recordCount recordCount)
               (setq myIndex._blockCount (add1 blockKEY))))
        true) ;; end of setRowSeq
    (defun setup(aName chkName backName vldFields fixUp)
       (if (<> myIndex #void)
           (begin
              (setq name aName)
              (setq checkPointPathName chkName)
              (setq backupPathName backName)
              (setq validFields vldFields)
              (setq myIndex._validFields validFields)
              (setq importFixup fixUp)
              (setq myIndex._importFixup importFixup)
              (setq myIndex._name name)
              ) ;; end then
           (begin
              (beginTransaction myOR)
              (setq name aName)
              (setq checkPointPathName chkName)
              (setq backupPathName backName)
              (setq validFields vldFields)
              (setq myIndex._validFields validFields)
              (setq importFixup fixUp)
              (setq myIndex._importFixup importFixup)
              (setq myIndex._name name)
              (commitTransaction myOR)
              )) ;; end if
           ) ;; end setup
    (defun set2(col row newValue) (error "NoAccess"))
    (defun switchUser()
        (abortTransaction myOR) 
        (beginTransaction myOR)
        (setq recordCount myIndex._recordCount)
        (setq recordStructure myIndex._recordStructure)
	(if (<> myIndex._blockSize #void) (setq blockSize myIndex._blockSize))
        (setq myIndex._blockSize blockSize)
	(setq tablePathName myOR[member: name:])
        (abortTransaction myOR)
        ) ;; end restore
    (defun truncate(rowLimit)
        vars:(newBlockKEY blockCount blockIndex lastBlock)
        ;; Are we already truncated?
        (if (>= rowLimit recordCount) (return recordCount))
        ;; Compute new block and record numbers.
        (setq newBlockKEY (divi (sub1 rowLimit) blockSize))
        (setq recordNUM (modi (sub1 rowLimit) blockSize))
        ;; Erase unnecessary blocks from the table repository.
        (beginTransaction myOR)
        (setq blockCount myIndex._blockCount)
        (loop for blockIndex from (add1 newBlockKEY) until blockCount  do
            (setq myOR[blockIndex] #void)
            ) ;; end loop
        (setq lastBlock myOR[newBlockKEY])
        (resize lastBlock (add1 recordNUM))
        (setq myOR[newBlockKEY] lastBlock)
        (setq recordCount rowLimit)
        (setq myIndex._recordCount rowLimit)
        (setq myIndex._blockCount (add1 newBlockKEY))
        (commitTransaction myOR)
        recordCount) ;; end of truncate
    ;; Initialize the Lambda when called by new ObjectRepository: or attachLibrarian. 
    ;; This function always reattaches itself to the Repository (if necessary),
    ;; and always reestablishes the table block count, record count, and other statistics.
    Continue::
    (setq self (myself))
    (setq myResident false)
    ;; Ressign the internal object repository properly.
    (cond ((= (type objRepository) Lambda:)
           (setq myOR objRepository.myOR))
          ((<> (type objRepository) ObjectRepository:)
           (error "tMgr:myOR"))
	  ((<> myOR objRepository)
           (setq myOR objRepository))
	   ) ;; end cond
    (if (<> (type myOR) ObjectRepository:) (error "tMgr:myOR"))
    ;; Reattach self as the repository librarian properly.
    (if (<> (refLibrarian myOR) self)
	(begin
        (attachLibrarian myOR self)
        (return self)
	)) ;; end if
	;; Reestablish the table block count, record count, and other statistics.
    (if (<> myIndex #void)
        (begin
           (setq recordStructure myIndex._recordStructure)
           (setq recordCount myIndex._recordCount)
           (setq validFields myIndex._validFields)
           (setq importFixup myIndex._importFixup)
           (setq name myIndex._name)
           (if (<> myIndex._blockSize #void) (setq blockSize myIndex._blockSize))
           (setq myIndex._blockSize blockSize)
           (setq tablePathName myOR[member: name:])
           ) ;; end then
        (begin
           (beginTransaction myOR)
           (setq recordStructure myIndex._recordStructure)
           (setq recordCount myIndex._recordCount)
           (setq validFields myIndex._validFields)
           (setq importFixup myIndex._importFixup)
           (setq name myIndex._name)
           (if (<> myIndex._blockSize #void) (setq blockSize myIndex._blockSize))
           (setq myIndex._blockSize blockSize)
           (setq tablePathName myOR[member: name:])
           (commitTransaction myOR)
           )) ;; end if
    self) ;; end of tableDbms





















;;**EXPORTKEY**:tableDbms:_makeBlockList
(defChildLambda tableDbms:_makeBlockList(runStart runLength runEnd)
;; *******************************************************************
;; summary:  Return a vector containing the run sequence starting
;;           with the specified runStart and running for the
;;           specified runLength, but not to exceed the specified
;;           runEnd.
;; parent:   Librarian Lambda which manages the table database and
;;           controls the table database schema.
;; Args:     runStart   The starting integer of the run.
;;           runCount   The number of integers in the run.
;;           runEnd     The maximum integer in the run.
;; Return:   runVector  The run vector or #void.        
;; *******************************************************************
    vars:(runIndex             ;; Index of run integers
          runVector            ;; Result run vector
          ) ;; end temporary variables
    ;; Compute the number of integers in this run.
    ;; Note: We cannot exceed the specified runEnd.
    (if (>= (+ runStart runLength) runEnd)
        (setq runLength (- runEnd runStart)))
    (if (<= runLength 0)
        (return runVector))
    ;; Create the run vector and fill it with sequential integers.
    (setq runVector (makeVector runLength))
    (setq runStart (integer runStart))
    (loop for runIndex from 0 until runLength do
        (setq runVector[runIndex] runStart)
        (setq runStart (addi runStart 1))
        ) ;; end fill loop
    runVector) ;; end of _makeBlockList





















;;**EXPORTKEY**:tableDbms:_mergeBlockList
(defChildLambda tableDbms:_mergeBlockList(blockListOne blockListTwo sortLambda)
;; *******************************************************************
;; summary:  Merge two lists of blocks into a sequential run of blocks. 
;;           Each blockList is a vector of block keys. Each blockList
;;           represents a sequential run of block keys, and the keys
;;           in blockListTwo are immediately contiguous to blockListOne.
;;           The records in blockListOne are presorted. The records in 
;;           blockListTwo are presorted. When the merge is complete,
;;           the records will be sorted across both blockLists.
;; parent:   Librarian Lambda which manages the table database and
;;           controls the table database schema.
;; Args:     blockListOne   A vector of block keys to be merged.
;;           blockListTwo   A vector of block keys to be merged.
;;           sortLambda      Sort lambda value for record pairs.
;; Return:   true        
;; *******************************************************************
    vars:(blockListOut         ;; Block list for merged output
          outBlock             ;; Block for merged output
          outKey               ;; Block key for merged output
          recordOne            ;; Record one for merge
          recordTwo            ;; Record two for merge
          ) ;; end temporary variables
    ;; Return if either block list is empty.
    (if (or (= blockListOne #void) 
            (<= (length blockListOne) 0)
            (= blockListTwo #void) 
            (<= (length blockListTwo) 0)) 
        (return #void))
    ;; Begin the merge by reading the first two records.
    ;; Note: We create a list of output block keys,
    ;;       then we rename the input block keys so that
    ;;       we will not overwrite them with our output.
    (setq blockListOut (append blockListOne blockListTwo))
    (setq outBlock (^new Vector: object: 0))
    (beginTransaction myOR)
    (_renameBlocks blockListOne myIndex)
    (_renameBlocks blockListTwo myIndex)
    (setq recordOne (_readBlockList blockListOne true))
    (setq recordTwo (_readBlockList blockListTwo true))
    (commitTransaction myOR)
    ;; Merge the records in both block lists together
    ;; into a single contiguous presorted run of blocks.
    (while (or (<> recordOne #void) (<> recordTwo #void)) do
        (cond
           ;; Manage the case where the second list is empty.
           ((= recordTwo #void)
            (begin
               (setq outBlock[(length outBlock)] recordOne)
               (setq recordOne (_readBlockList blockListOne true))
               )) ;; end second list is empty.
           ;; Manage the case where the first list is empty.
           ((= recordOne #void)
            (begin
               (setq outBlock[(length outBlock)] recordTwo)
               (setq recordTwo (_readBlockList blockListTwo true))
               )) ;; end first list is empty
           ;; Manage the case where record one should be output.
           ((sortLambda recordOne recordTwo)
            (begin
               (setq outBlock[(length outBlock)] recordOne)
               (setq recordOne (_readBlockList blockListOne true))
               )) ;; end record one comes first
           ;; Manage the case where record two should be output.
           (else
            (begin
               (setq outBlock[(length outBlock)] recordTwo)
               (setq recordTwo (_readBlockList blockListTwo true))
               )) ;; end record two comes first
            ) ;; end cond
        ;; Write the output block (if it is full).
        ;; Note: The output key is taken off the front
        ;;       of the list of output block keys. This
        ;;       means that the merged blocks are written
        ;;       back to the database using original keys.
        (if (>= (length outBlock) blockSize)
            (begin
               (setq outKey blockListOut[0])
               (delete blockListOut 0) 
               (setq myOR[outKey] outBlock)
               (setq outBlock (^new Vector: object: 0))
               )) ;; end if
        ) ;; end while
    ;; Write the output block (if it is not empty).
    ;; Note: The output key is taken off the front
    ;;       of the list of output block keys. This
    ;;       means that the merged blocks are written
    ;;       back to the database using original keys.
    (if (> (length outBlock) 0)
        (begin
           (setq outKey blockListOut[0])
           (delete blockListOut 0) 
           (setq myOR[outKey] outBlock)
           (setq outBlock #void)
           )) ;; end if
    ;; Always return true.
    true) ;; end of _mergeBlockList






















;;**EXPORTKEY**:tableDbms:_readBlockList
(defChildLambda tableDbms:_readBlockList(blockList eraseSW)
;; *******************************************************************
;; summary:  Read the next record from the specified list of blocks. 
;;           The blockList is a vector of block keys. As a new block
;;           is read, it is stored in the cdr of the blockList. As
;;           a block is exhausted, it is erased from the repository,
;;           and the blockList is altered to reflect the erased block.
;; parent:   Librarian Lambda which manages the table database and
;;           controls the table database schema.
;; Args:     blockList   A vector of block keys to be read.
;;           eraseSW     True if blocks are to be erased after reading.
;; Return:   theRecord   The next database record (or #void).       
;; *******************************************************************
    vars:(blockKey theBlock theRecord) 
    ;; Return #void if there are no more records.
    (if (and (= (cdr blockList) #void) (<= (length blockList) 0)) (return #void))
    ;; Make sure the current block is loaded from the repository.
    ;; Note: If we must load a new block, then we
    ;;       erase the block from the repository
    ;;       and from the blockList after we load.
    (if (= (cdr blockList) #void)
        (begin
           (setq blockKey blockList[0])
           (setCdr blockList myOR[blockKey])
           (if (= eraseSW true)
               (setq myOR[blockKey] #void))
           (delete blockList 0)
           )) ;; end if
    ;; Read the next record from the current block.
    ;; Note: If we exhaust the current block, then we
    ;;       set the cdr of the blockList to #void.
    (setq theBlock (cdr blockList))
    (if (or (= theBlock #void) (<= (length theBlock) 0)) (return #void))
    (setq theRecord theBlock[0])
    (delete theBlock 0)
    (if (<= (length theBlock) 0) (setCdr blockList #void))
    ;; Return the next record.
    theRecord) ;; end of _readBlockList





















;;**EXPORTKEY**:tableDbms:_renameBlocks
(defChildLambda tableDbms:_renameBlocks(blockList anIndex)
;; *******************************************************************
;; summary:  Rename the specified list of blocks. The block keys are
;;           positive integers. Each key is renamed to its negation
;;           and restored in the specified repository index. The
;;           blockList is altered to reflect the new keys.
;; parent:   Librarian Lambda which manages the table database and
;;           controls the table database schema.
;; Args:     blockList   A vector of block keys to be renamed.
;;           keyIndex    A Directory of block keys.
;; Return:   true        
;; *******************************************************************
    vars:(blockCount blockIndex oldKey newKey oldValue) 
    ;; Rename each block to its negation.
    ;; Note: This does not reposition the blocks  
    ;;       in the table database. The blocks
    ;;       are only renamed in the index.
    (setq blockCount (length blockList))
    (loop for blockIndex from 0 until blockCount do
        (setq oldKey blockList[blockIndex])
        (setq newKey (- oldKey))
        (setq blockList[blockIndex] newKey)
        (setq oldValue anIndex[oldKey])
        (setq anIndex[oldKey] #void)
        (setq anIndex[newKey] oldValue)
        ) ;; end of loop
    true) ;; end of _renameBlocks





















;;**EXPORTKEY**:tableDbms:copyDatabase
(defChildLambda tableDbms:copyDatabase(fileTwo) 
;; *******************************************************************
;; summary:  Copy the entire contents of this table database to table
;;           database two (with file defragmentation).
;; Args :    fileTwo    The name of the table database file to receive the copy. 
;; Return:   true       If no error occurs.
;; *******************************************************************
   vars:(fileTwo             ;; The file id for file two.
         databaseTwo         ;; The Object Repository for file two.
         dataBlock           ;; The block of data to be copied.
         blockKey            ;; The key for the block of data to be copied.
         blockIndex          ;; The number of bytes already copied.
         indexLength         ;; The number of records to be copied.
         ) ;; end of temporary variables
   ;; Open and clear database file two for receiving the copied records.
   (setq databaseTwo (^new ObjectRepository: fileTwo clear:))
   (setq databaseTwo (dummyDbms databaseTwo))
   (beginTransaction myOR)
   (beginTransaction databaseTwo)
   (setq indexLength (length myIndex))
   ;(writeln "") 
   ;(writeln "Starting copy of " indexLength " blocks to " fileTwo) 
   ;; Copy each record from this table database to table database two.
   (loop for blockIndex from 0 until indexLength do
      (setq blockKey myIndex[blockIndex 0])
      ;(writeln "Copying block [" blockIndex "] key = {" blockKey "}") 
      (if (isSymbol blockKey)
          (setq databaseTwo.myIndex[blockKey] myIndex[blockIndex 1])
          (setq databaseTwo.myOR[blockKey] myOR[blockKey])) 
      ) ;; end of loop
   ;; Close both database files.
   ;(writeln "Ending copy of " indexLength " blocks to " fileTwo) 
   (abortTransaction myOR)
   (commitTransaction databaseTwo)
   true) ;; end of copyDatabase
























;;**EXPORTKEY**:tableDbms:copyFile
(defChildLambda tableDbms:copyFile(fileOne fileTwo) 
;; *******************************************************************
;; summary:  Copy the entire contents of file one to file two.
;; Args :    fileOne    The name of the file to be copied. 
;;           fileTwo    The name of the file to receive the copy. 
;; Return:   The number of bytes copied.
;; *******************************************************************
   vars:(fileOneID           ;; The file id for file one.
         fileTwoID           ;; The file id for file two.
         dataBlock           ;; The block of data to be copied.
         (fileType 0)        ;; The file type for standard files.
         (byteCount 0)       ;; The number of bytes to be copied.
         byteIndex           ;; The number of bytes already copied.
         (byteLength 100000) ;; The number of bytes to be read.
         ) ;; end of temporary variables
   ;; Open file one for reading and file two for writing.
   (setq fileOneID (fileOpen fileOne 0 fileType))
   (setq fileTwoID (fileOpen fileTwo 1 fileType))
   ;; Find the length of file one and reset file pointer.
   (setq byteCount (fileSeek fileOneID 0 2))
   (fileSeek fileOneID 0 1)
   ;; Read and write blocks until there is no more data
   (loop for byteIndex from 0 until byteCount by 100000 do
      ;; Make sure we read only to the end of the file.
      (if (> (+ byteIndex byteLength) byteCount)
          (setq byteLength (- byteCount byteIndex))
          ) ;; end if
      ;; Copy a block from file one to file two.
      (setq dataBlock (fileRead fileOneID byteLength))
      (fileWrite fileTwoID dataBlock)
      ) ;; end of loop
   ;; Close both files.
   (fileClose fileOneID 1)
   (fileClose fileTwoID 1)
   byteCount) ;; end of copyFile





















;;**EXPORTKEY**:tableDbms:delimitedCells
(defChildLambda tableDbms:delimitedCells(startRow endRow startCol endCol)
;; *******************************************************************
;; summary:  Return a set of values from the table database as a
;;           tab delimited string.
;; parent:   Librarian Lambda which manages the table database and
;;           controls the table database schema.
;; Args:     startRow    Starting row for values
;;           endRow      Ending row for values
;;           startCol    Starting column for values
;;           endCol      Ending column for values    
;; Return:   String      A string with field values separated by tabs
;;                       and rows separated by the contents of _eol.
;; *******************************************************************
    vars:(i getRow getCol dls cellValue realRow 
          recordBLOCK startBlockKey endBlockKey
          startRecord endRecord record tab) 
    ;; Retrieve the start and end record blocks.
    ;; Note: We assume that no more than two 
    ;;       blocks will ever be necessary.
    (if (<> myIndex #void)
        (begin 
            (setq startBlockKey (divi startRow blockSize))
            (setq recordBLOCK myOR[startBlockKey])
            (setq endBlockKey (divi endRow blockSize))
            (if (<> startBlockKey endBlockKey)
                (setq recordBLOCK (append recordBLOCK myOR[endBlockKey])))
                ) ;; end of then
        (begin
            (beginTransaction myOR) 
            (setq startBlockKey (divi startRow blockSize))
            (setq recordBLOCK myOR[startBlockKey])
            (setq endBlockKey (divi endRow blockSize))
            (if (<> startBlockKey endBlockKey)
                (setq recordBLOCK (append recordBLOCK myOR[endBlockKey])))
            (abortTransaction myOR))
            ) ;; end of else
    ;; Compute blocked record numbers.
    (setq startRecord (modi startRow blockSize))
    (setq endRecord (addi startRecord (subi endRow startRow)))
    ;; Return a tab delimited string of cell values for
    ;; all cells between the following rows and columns.
    (setq dls "")
    (setq tab (append "" (char 09)))
    (setq realRow startRow)
    (loop for getRow from startRecord to endRecord do
       (setq dls (append dls realRow (char 09)))
       (++ realRow)
       (setq record (objectToStructure (copy recordStructure) recordBLOCK[getRow]))
       (loop for getCol from startCol until endCol do
            (setq cellValue record[getCol])
            (if (= cellValue #void) (setq cellValue ""))
            (setq cellValue (append "" cellValue)) 
            (setq cellValue (substitute cellValue (char 09)  " "))
            (setq cellValue (substitute cellValue (char 13)  " "))
            (setq dls (append dls cellValue (char 09)))
            ) ;;end getCol loop
        (setq dls (append dls (char 10) (char 13)))
        ) ;;end getRow loop
    (append dls "")) ;; end of delimitedCells





















;;**EXPORTKEY**:tableDbms:exportCells
(defChildLambda tableDbms:exportCells(formatName exportAs exportDelim fileName startRow endRow)
;; *******************************************************************
;; summary:  Return a set of values from the table database as a
;;           tab delimited string.
;; parent:   Librarian Lambda which manages the table database and
;;           controls the table database schema.
;; Args:     formatName  type of format to export
;;           exportAs    export as "delimited","fixed",etc. 
;;           startRow    Starting row for values
;;           endRow      Ending row for values
;; Return:   String      A string with field values separated by tabs
;;                       and rows separated by the contents of _eol.
;; *******************************************************************
    vars:(
          cellValue               ;; Data field value from database
          dls                     ;; Row output buffer
          exportFormatRecord      ;; Export record layout format data
          fieldName               ;; Name of output field
          fieldLen                ;; Field output length
          fileId                  ;; File handle
          getRow                  ;; Row index
          layoutLength            ;; Length of layout data
          myPos                   ;; Delimiter position terminating field in layout data
          outputField             ;; Buffer for building fixed-length output field
          record                  ;; Record data
         ) 

    ;; Format exportAs parameter to avoid confusion
    (setq exportAs (upcase (left exportAs 3)))

    ;; Retrieve the export layout from the exportLayoutLambda.
    (if (= (setq exportFormatRecord (exportLayoutLambda.getLayoutRecord formatName)) #void)
        (error "unknownFormat")
    )

    ;; Return a tab delimited string of cell values for
    ;; all cells between the following rows and columns.

    ;; Initialize row buffer
    (setq dls "")
     
    ;; For all rows by index, assuming user gives us 1-based record parameters
    ;; and further assuming that refRowseq is zero-based
    (if (<= startRow 0) (setq startRow 1))
    (if (> startRow endRow) (setq endRow startRow))
    (if (>= endRow recordCount) (setq endRow recordCount))

    ;; Open file, getting handle
    (setq fileId (fileOpen  fileName 1 0))     
    (updateBegin)

    (loop for getRow from (sub1 startRow) until endRow do
         
       ;; Read database row
       (setq record (updateRead getRow))
                  
       ;; Initialize position variables
       (setq myPos 0)
       (setq layoutLength (length exportFormatRecord.fieldList))
                 
       ;; Output fields for this row
       (while (< myPos layoutLength) do
          ;; Parse out the export field name
          (setq fieldName exportFormatRecord.fieldList[myPos])
                                        
          ;; Store the field's value
          (setq cellValue record[(symbol fieldName)])
                 
          ;; Set #void values to an empty string -- 0 bytes will be output
          (if (= cellValue #void) then (setq cellValue ""))
                  
          ;; Format numeric field values
          (if (= (isString cellValue) false)
              (setq cellValue (string cellValue)))
                
          ;; Branch to fixed-length format -------------------------------
          (if (= exportAs "FIX") 
              (begin
                
                 ;; Store the length of the output field
                 (setq fieldLen exportFormatRecord.fieldData[exportFormatRecord.fieldList[myPos]])
                       
                 ;; Initialize the outputField to spaces
                 (setq outputField (append cellValue (rept " " fieldLen)))
                        
                 ;; Truncate any excess bytes per the specified field len
                 (setq outputField (left outputField fieldLen))
              
                 ;; Add the output field to the row buffer
                 (setq dls (append dls outputField))
              )
          )
          
          ;; Branch to Delimited ------------------------------------
          (if (= exportAs "DEL")
              (begin
                (setq dls (append dls cellValue))
                (if (< (add1 myPos) layoutLength)
                    (setq dls (append dls (char (number exportDelim)))))
              )
          )

          ;; Increment position variable
          (++ myPos)

       ) ;; end while (< layoutLength myPos)
       
       ;; Output row to file               
       (fileWrite  fileId  (append dls _eol))
       ;; Clear row buffer
       (setq dls "") 
       ;; process next row
    ) ;;end getRow loop

    ;; Close the file
    (fileClose  fileId  1)
    (updateEnd)

    true) ;; end of exportCells



















;;**EXPORTKEY**:tableDbms:fieldKeys
(defChildLambda tableDbms:fieldKeys(fieldName)
;; *******************************************************************
;; summary:  Return a set of field key values from the table database
;;           as a tab delimited string.
;; parent:   Librarian Lambda which manages the table database and
;;           controls the table database schema.
;; Args:     fieldName   Name of the Dictionary field whose valid keys 
;;                       are to be returned.  
;; Return:   String      A string with field key values separated by 
;;                       tabs and rows separated by the contents of _eol.
;; *******************************************************************
    vars:(i dls typeValue record) 
    ;; Return a tab delimited string of field key values.
    (setq dls "")
    ;; Set the field key values
    (cond
        ((= fieldName "DepProducts")
         (setq record #(CHKC CHKB INTC INTB SAVC SAVB CDC CDB IRA)))
        ((= fieldName "CrdProducts")
         (setq record #(CCVC CCMS CCMG CCVP CCVB CCMB CCVR)))
        ((= fieldName "LinProducts")
         (setq record #(CLE CLC PLI HLIV EMXC EMXB ACLB DCLB PDLB OCLB)))
        ((= fieldName "LonProducts")
         (setq record #(ADIR ADLR RV MFH PLO CBLO HLOF HLOV FRLC FRLB OLON REFC REVC REFB REVB)))
        (else
         (setq record #void))
        ) ;; end cond
    (setq endCol (length record))
    (loop for i from 0 until endCol do
        (setq typeValue record[i])
        (if (= typeValue #void) then (setq typeValue ""))
        (setq dls (append dls typeValue (char 09)))
        ) ;;end field type loop
    (setq dls (append dls (char 10) (char 13)))
    (append dls "")) ;; end of fieldKeys





















;;**EXPORTKEY**:tableDbms:fieldSubfields
(defChildLambda tableDbms:fieldSubfields(fieldName)
;; *******************************************************************
;; summary:  Return a set of field subfield name values from the table 
;;           database as a tab delimited string.
;; parent:   Librarian Lambda which manages the table database and
;;           controls the table database schema.
;; Args:     fieldName   Name of the Dictionary field whose valid subfield 
;;                       names are to be returned.  
;; Return:   String      A string with field subfield name values separated by 
;;                       tabs and rows separated by the contents of _eol.
;; *******************************************************************
    vars:(i dls typeValue record) 
    ;; Return a tab delimited string of field key values.
    (setq dls "")
    ;; Set the field key values
    (cond
        ((= fieldName "DepProducts") (setq record _depRollupFields))
        ((= fieldName "DepAccounts") (setq record _depDetailFields))
        ((= fieldName "CrdProducts") (setq record _cardRollupFields))
        ((= fieldName "CrdAccounts") (setq record _cardDetailFields))
        ((= fieldName "LinProducts") (setq record _lineRollupFields))
        ((= fieldName "LinAccounts") (setq record _lineDetailFields))
        ((= fieldName "LonProducts") (setq record _loanRollupFields))
        ((= fieldName "LonAccounts") (setq record _loanDetailFields))
        (else
         (setq record #void))
        ) ;; end cond
    (setq endCol (length record))
    (loop for i from 0 until endCol do
        (setq typeValue record[i 0])
        (if (= typeValue #void) then (setq typeValue ""))
        (setq dls (append dls typeValue (char 09)))
        ) ;;end field type loop
    (setq dls (append dls (char 10) (char 13)))
    (append dls "")) ;; end of fieldSubfields





















;;**EXPORTKEY**:tableDbms:fieldSubtypes
(defChildLambda tableDbms:fieldSubtypes(fieldName)
;; *******************************************************************
;; summary:  Return a set of field subfield type values from the table 
;;           database as a tab delimited string.
;; parent:   Librarian Lambda which manages the table database and
;;           controls the table database schema.
;; Args:     fieldName   Name of the Dictionary field whose valid subfield 
;;                       types are to be returned.  
;; Return:   String      A string with field subfield type values separated by 
;;                       tabs and rows separated by the contents of _eol.
;; *******************************************************************
    vars:(i dls typeValue record) 
    ;; Return a tab delimited string of field key values.
    (setq dls "")
    ;; Set the field key values
    (cond
        ((= fieldName "DepProducts") (setq record _depRollupFields))
        ((= fieldName "DepAccounts") (setq record _depDetailFields))
        ((= fieldName "CrdProducts") (setq record _cardRollupFields))
        ((= fieldName "CrdAccounts") (setq record _cardDetailFields))
        ((= fieldName "LinProducts") (setq record _lineRollupFields))
        ((= fieldName "LinAccounts") (setq record _lineDetailFields))
        ((= fieldName "LonProducts") (setq record _loanRollupFields))
        ((= fieldName "LonAccounts") (setq record _loanDetailFields))
        (else
         (setq record #void))
        ) ;; end cond
    (setq endCol (length record))
    (loop for i from 0 until endCol do
        (setq typeValue record[i 1])
        (if (= typeValue #void) then (setq typeValue ""))
        (setq dls (append dls typeValue (char 09)))
        ) ;;end field type loop
    (setq dls (append dls (char 10) (char 13)))
    (append dls "")) ;; end of fieldSubtypes





















;;**EXPORTKEY**:tableDbms:fieldTypes
(defChildLambda tableDbms:fieldTypes()
;; *******************************************************************
;; summary:  Return a set of field type values from the table database
;;           as a tab delimited string.
;; parent:   Librarian Lambda which manages the table database and
;;           controls the table database schema.
;; Args:     fieldName   
;; Return:   String      A string with field type values separated by 
;;                       tabs and rows separated by the contents of _eol.
;; *******************************************************************
    vars:(i dls typeValue record) 
    ;; Return a tab delimited string of field type values.
    (setq dls "")
    (setq record validFields)
    (setq endCol (length record))
    (loop for i from 0 until endCol do
        (setq typeValue record[i])
        (if (= typeValue #void) then (setq typeValue ""))
        (setq dls (append dls typeValue (char 09)))
        ) ;;end field type loop
    (setq dls (append dls (char 10) (char 13)))
    (append dls "")) ;; end of fieldTypes





















;;**EXPORTKEY**:tableDbms:importLegacyData
(defChildLambda tableDbms:importLegacyData(...)
;; *******************************************************************
;; summary:  Import a time slice into the table database.
;; parent:   Librarian Lambda which manages the table database and
;;           controls the table database schema.
;; Args:     tableDate      (optional) The date of this extract    
;;           sbfFormatSW    (optional) The boolean format switch 
;;                                     (true if NO separate column 
;;                                      headers file)   
;;           colFile        (optional) The path and file name of the 
;;                                     separate column headers file   
;;           tableFile      (optional) The path and file name of the 
;;                                     tab delimited table data file
;;           maxRecordLimit (optional) The maximum number of records 
;;                                     to import.
;; Note:     If only one argument is passded, it is treated as the 
;;           error return passed by onError.
;; Return:   true
;; *******************************************************************
    vars:(i j n commands colHeaders 
          tableDate colFile tableFile
          recordLimit startTime endTime
          ) ;; end of temporary variables
    (writeln "Starting record import." )
    (setq importNilRows 0)
    ;; Is this an error during table extract import?
    (if (= (argCount) 1) (goto ErrorContinue:))
    ;; Request the date for this table extract import.
    (if (>= (argCount) 1)
        (setq tableDate (argFetch 0))
        (begin
           (setq tableDate #void)
           (setq tableDate (input "Enter table extract date." (string (date (integer (now))))))
           (if (isBoolean tableDate) (return true))
           (setq tableDate (date tableDate))
           (setq importDate tableDate)
           )) ;; end if
    ;; Determine the column header location and file name.
    (if (>= (argCount) 3)
        (begin
           (setq sbfFormatSW (argFetch 1))
           (setq colFile (argFetch 2)))
        (begin
           ;; Request column header options.
           (setq colFile #void)
           (setq commands #("Header with data" "Header apart from data"))
           (setq n (select  "tableDataLambda"  "Where are the column headers?" commands 0))
           (if (isBoolean n) (return false))
           (if (= n 0)
               (setq sbFormatSW true)
               (setq sbfFormatSW false))
           )) ;; end outter if
    ;; Request the path and file for this table extract import.
    (if (>= (argCount) 4)
        (setq tableFile (argFetch 3))
        (begin
           (setq tableFile (input "Enter table path and file name." ""))
           (if (isBoolean tableFile) (return true))
           )) ;; end if
    ;; Request the import record limit for this table extract import.
    (if (>= (argCount) 5)
        (setq maxRecordLimit (argFetch 4))
        (setq maxRecordLimit 1000000000)
        ) ;; end if
    ;; Do we need to open a separate column header file?
    (setq importFixupSW false)
    (if (= sbfFormatSW false)
        (begin
           (if (= colFile "default")
               (begin
                 (setq colHeaders (cnvColHeaders))
                 (setq importFixupSW true))
               (begin 
                 (setq commands (readTextFile colFile))
                 (if (= commands false) (return false))
                 (setq colHeaders (stringToVector commands _eol))
                 (setq commands #void) ;; conserve space
                 (setq n (length colHeaders))
                 (loop for i from 0 until n do
                     (setq j (find #\tab colHeaders[i]))
                     (if (isNumber j) 
                         (setq colHeaders[i] (left colHeaders[i] j)))         
                     ))) ;; end of for loop
           (setq colVector colHeaders)
           (setq colHeaders #void)
           ) ;; end then
         (begin
           (setq colVector #void)
           (setq colHeaders #void)
           ) ;; end else
         ) ;; end of inner if
    ;; Make sure the record template is constructed correctly.
    (if (<> colVector #void)
        (setq recordStructure (objectToStructure colVector #(#void)))
        ) ;; end build record template
    ;; Make sure the user opens the file correctly.
    (setq importFileID (fileOpen tableFile 0 0))
    (if (= importFileID false) (return false))
    ;; Destroy all the records, statistics, and 
    ;; indices in the existing database.
    (setq importingSW false)
    (clear myOR)
    ;; Load myIndex and reset all database statistics.
    (beginTransaction myOR)
    (setq myIndex._colVector #void)
    (setq myIndex._recordCount 0)
    (setq recordCount 0)
    (setq myIndex._blockCount 0)
    (setq myIndex._tableDate tableDate)
    ;; Reset all import related persistent variables.
    (setq importingSW true)
    (setq blockKEY 0)
    (setq currentBlock #void)
    ;; Set up import timing statistics
    (setq startTime (getTickCount 0))
    ;; Import tab delimited file and close file when done.
    ;; Note: This will recursively call setColName, set1, etc.
    (onError (myself))
    (^importTab importFileID self recordVectors:)
    ;; Return here if an import error occurs
    ErrorContinue::
    (if (isNumber importFileID)
        (fileClose importFileID 1))
    (setq importFileID #void)
    ;; Make sure we write out the last block when done.
    (if (<> currentBlock #void)
        (begin
            (setq myOR[blockKEY] currentBlock)
            (setq myIndex._blockCount (addi blockKEY 1))
            (setq blockKEY 0)
            (setq currentBlock #void)))
    ;; Complete the import process.
    (setq endTime (integer (getTickCount startTime)))
    (writeln "Records imported = " recordCount " in " endTime " Seconds" )
    (commitTransaction myOR)
    (setq importingSW false)
    ;; Copy the freshly imported table database file
    ;; to the backup file for use by the findAll method.
    (copyFile tablePathName backupPathName)
    true) ;; end tableDbms:ImportTableDbmsData





















;;**EXPORTKEY**:tableDbms:importTab
(defChildLambda tableDbms:importTab(...)
;; *******************************************************************
;; summary:  Import a time slice into the table database.
;; parent:   Librarian Lambda which manages the table database and
;;           controls the table database schema.
;; Args:     tableDate      (optional) The date of this extract    
;;           sbfFormatSW    (optional) The boolean format switch 
;;                                     (true if NO separate column 
;;                                      headers file)   
;;           colFile        (optional) The path and file name of the 
;;                                     separate column headers file   
;;           tableFile      (optional) The path and file name of the 
;;                                     tab delimited table data file
;;           maxRecordLimit (optional) The maximum number of records 
;;                                     to import.
;;           fixupSW        (optional) True if imported records are 
;;                                     to use default fixup function.
;; Note:     If only one argument is passded, it is treated as the 
;;           error return passed by onError.
;; Return:   true
;; *******************************************************************
    vars:(i j n commands colHeaders 
          tableDate colFile tableFile
          recordLimit startTime endTime
          fixupSW
          ) ;; end of temporary variables
    (setq importNilRows 0)
    ;; Is this an error during table extract import?
    (if (= (argCount) 1) (goto ErrorContinue:))
    ;; Request the date for this table extract import.
    (if (>= (argCount) 1)
        (setq tableDate (argFetch 0))
        (begin
           (setq tableDate #void)
           (setq tableDate (input "Enter table extract date." (string (date (integer (now))))))
           (if (isBoolean tableDate) (return true))
           (setq tableDate (date tableDate))
           (setq importDate tableDate)
           )) ;; end if
    ;; Determine the column header location and file name.
    (if (>= (argCount) 3)
        (begin
           (setq sbfFormatSW (argFetch 1))
           (setq colFile (argFetch 2)))
        (begin
           ;; Request column header options.
           (setq colFile #void)
           (setq commands #("Header with data" "Header apart from data"))
           (setq n (select  "tableDataLambda"  "Where are the column headers?" commands 0))
           (if (isBoolean n) (return false))
           (if (= n 0)
               (setq sbFormatSW true)
               (setq sbfFormatSW false))
           )) ;; end outter if
    ;; Request the path and file for this table extract import.
    (if (>= (argCount) 4)
        (setq tableFile (argFetch 3))
        (begin
           (setq tableFile (input "Enter table path and file name." ""))
           (if (isBoolean tableFile) (return true))
           )) ;; end if
    ;; Request the import record limit for this table extract import.
    (if (>= (argCount) 5)
        (setq maxRecordLimit (argFetch 4))
        (setq maxRecordLimit 1000000000)
        ) ;; end if
    ;; Request the import fixup switch for this table extract import.
    (if (>= (argCount) 6)
        (setq fixupSW (argFetch 5))
        (setq fixupSW true)
        ) ;; end if
    ;; Do we need to open a separate column header file?
    (setq importFixupSW false)
    (if (= sbfFormatSW false)
        (begin
           (if (= colFile "default")
               (begin
                 (setq colHeaders (cnvColHeaders))
                 (setq importFixupSW fixupSW))
               (begin 
                 (setq commands (readTextFile colFile))
                 (if (= commands false) (return false))
                 (setq colHeaders (stringToVector commands _eol))
                 (setq commands #void) ;; conserve space
                 (setq n (length colHeaders))
                 (loop for i from 0 until n do
                     (setq j (find #\tab colHeaders[i]))
                     (if (isNumber j) 
                         (setq colHeaders[i] (left colHeaders[i] j)))         
                     ))) ;; end of for loop
           (setq colVector colHeaders)
           (setq colHeaders #void)
           ) ;; end then
         (begin
           (setq colVector #void)
           (setq colHeaders #void)
           ) ;; end else
         ) ;; end of inner if
    ;; Make sure the record template is constructed correctly.
    (if (<> colVector #void)
        (setq recordStructure (objectToStructure colVector #(#void)))
        ) ;; end build record template
    ;; Make sure the user opens the file correctly.
    (setq importFileID (fileOpen tableFile 0 0))
    (if (= importFileID false) (return false))
    ;; Destroy all the records, statistics, and 
    ;; indices in the existing database.
    (setq importingSW false)
    (clear myOR)
    ;; Load myIndex and reset all database statistics.
    (beginTransaction myOR)
    (setq myIndex._colVector #void)
    (setq myIndex._recordCount 0)
    (setq recordCount 0)
    (setq myIndex._blockCount 0)
    (setq myIndex._tableDate tableDate)
    ;; Reset all import related persistent variables.
    (setq importingSW true)
    (setq blockKEY 0)
    (setq currentBlock #void)
    ;; Set up import timing statistics
    (setq startTime (getTickCount 0))
    ;; Import tab delimited file and close file when done.
    ;; Note: This will recursively call setColName, set1, etc.
    (onError (myself))
    (^importTab importFileID self recordVectors:)
    ;; Return here if an import error occurs
    ErrorContinue::
    (if (isNumber importFileID)
        (fileClose importFileID 1))
    (setq importFileID #void)
    ;; Make sure we write out the last block when done.
    (if (<> currentBlock #void)
        (begin
            (setq myOR[blockKEY] currentBlock)
            (setq myIndex._blockCount (addi blockKEY 1))
            (setq blockKEY 0)
            (setq currentBlock #void)))
    ;; Complete the import process.
    (setq endTime (integer (getTickCount startTime)))
    (writeln "Records imported = " recordCount " in " endTime " Seconds" )
    (commitTransaction myOR)
    (setq importingSW false)
    ;; Copy the freshly imported table database file
    ;; to the backup file for use by the findAll method.
    (copyFile tablePathName backupPathName)
    true) ;; end tableDbms:ImportTab





















;;**EXPORTKEY**:tableDbms:makeBlockList
(defChildLambda tableDbms:makeBlockList(runStart runLength runEnd)
;; *******************************************************************
;; summary:  Return a vector containing the run sequence starting
;;           with the specified runStart and running for the
;;           specified runLength, but not to exceed the specified
;;           runEnd.
;; parent:   Librarian Lambda which manages the table database and
;;           controls the table database schema.
;; Args:     runStart   The starting integer of the run.
;;           runCount   The number of integers in the run.
;;           runEnd     The maximum integer in the run.
;; Return:   runVector  The run vector or #void.        
;; *******************************************************************
    vars:(runIndex             ;; Index of run integers
          runVector            ;; Result run vector
          ) ;; end temporary variables
    ;; Compute the number of integers in this run.
    ;; Note: We cannot exceed the specified runEnd.
    (if (>= (+ runStart runLength) runEnd)
        (setq runLength (- runEnd runStart)))
    (if (<= runLength 0)
        (return runVector))
    ;; Create the run vector and fill it with sequential integers.
    (setq runVector (makeVector runLength))
    (setq runStart (integer runStart))
    (loop for runIndex from 0 until runLength do
        (setq runVector[runIndex] runStart)
        (setq runStart (addi runStart 1))
        ) ;; end fill loop
    runVector) ;; end of makeBlockList




















;;**EXPORTKEY**:tableDbms:mergeBlockList
(defChildLambda tableDbms:mergeBlockList(blockListOne blockListTwo sortLambda)
;; *******************************************************************
;; summary:  Merge two lists of blocks into a sequential run of blocks. 
;;           Each blockList is a vector of block keys. Each blockList
;;           represents a sequential run of block keys, and the keys
;;           in blockListTwo are immediately contiguous to blockListOne.
;;           The records in blockListOne are presorted. The records in 
;;           blockListTwo are presorted. When the merge is complete,
;;           the records will be sorted across both blockLists.
;; parent:   Librarian Lambda which manages the table database and
;;           controls the table database schema.
;; Args:     blockListOne   A vector of block keys to be merged.
;;           blockListTwo   A vector of block keys to be merged.
;;           sortLambda      Sort lambda value for record pairs.
;; Return:   true        
;; *******************************************************************
    vars:(blockListOut         ;; Block list for merged output
          outBlock             ;; Block for merged output
          outKey               ;; Block key for merged output
          recordOne            ;; Record one for merge
          recordTwo            ;; Record two for merge
          ) ;; end temporary variables
    ;; Return if either block list is empty.
    (if (or (= blockListOne #void) 
            (<= (length blockListOne) 0)
            (= blockListTwo #void) 
            (<= (length blockListTwo) 0)) 
        (return #void))
    ;; Begin the merge by reading the first two records.
    ;; Note: We create a list of output block keys,
    ;;       then we rename the input block keys so that
    ;;       we will not overwrite them with our output.
    (setq blockListOut (append blockListOne blockListTwo))
    (setq outBlock (^new Vector: object: 0))
    (beginTransaction myOR)
    (renameBlocks blockListOne myIndex)
    (renameBlocks blockListTwo myIndex)
    (setq recordOne (readBlockList blockListOne true))
    (setq recordTwo (readBlockList blockListTwo true))
    (commitTransaction myOR)
    ;; Merge the records in both block lists together
    ;; into a single contiguous presorted run of blocks.
    (while (or (<> recordOne #void) (<> recordTwo #void)) do
        (cond
           ;; Manage the case where the second list is empty.
           ((= recordTwo #void)
            (begin
               (setq outBlock[(length outBlock)] recordOne)
               (setq recordOne (readBlockList blockListOne true))
               )) ;; end second list is empty.
           ;; Manage the case where the first list is empty.
           ((= recordOne #void)
            (begin
               (setq outBlock[(length outBlock)] recordTwo)
               (setq recordTwo (readBlockList blockListTwo true))
               )) ;; end first list is empty
           ;; Manage the case where record one should be output.
           ((sortLambda recordOne recordTwo)
            (begin
               (setq outBlock[(length outBlock)] recordOne)
               (setq recordOne (readBlockList blockListOne true))
               )) ;; end record one comes first
           ;; Manage the case where record two should be output.
           (else
            (begin
               (setq outBlock[(length outBlock)] recordTwo)
               (setq recordTwo (readBlockList blockListTwo true))
               )) ;; end record two comes first
            ) ;; end cond
        ;; Write the output block (if it is full).
        ;; Note: The output key is taken off the front
        ;;       of the list of output block keys. This
        ;;       means that the merged blocks are written
        ;;       back to the database using original keys.
        (if (>= (length outBlock) blockSize)
            (begin
               (setq outKey blockListOut[0])
               (delete blockListOut 0) 
               (setq myOR[outKey] outBlock)
               (setq outBlock (^new Vector: object: 0))
               )) ;; end if
        ) ;; end while
    ;; Write the output block (if it is not empty).
    ;; Note: The output key is taken off the front
    ;;       of the list of output block keys. This
    ;;       means that the merged blocks are written
    ;;       back to the database using original keys.
    (if (> (length outBlock) 0)
        (begin
           (setq outKey blockListOut[0])
           (delete blockListOut 0) 
           (setq myOR[outKey] outBlock)
           (setq outBlock #void)
           )) ;; end if
    ;; Always return true.
    true) ;; end of mergeBlockList





















;;**EXPORTKEY**:tableDbms:queryBuilder
(defChildLambda tableDbms:queryBuilder(aSpec)
;; *******************************************************************
;;  Summary: Builds a table screening function from the text specification   
;;           passed as an argument. The text specification is designed
;;           to allow ease of specification, recognition of similar 
;;           table screening strategies, and generation of table
;;           database screening strategies by genetic algorithms.
;; Note:     queryBuilder refers to building table screening Lambdas,
;;           not to building graphic user interface screens.
;; Depends:	 excludCriteriaLambda
;;           rulesLib
;;           selectCriteriaLambda
;; Args:     aSpec      Screen specification text.
;; Return:   proc       a table screening Lambda.   
;; *******************************************************************
   pvars:((debugOn false)         ;; Display generated Lambda on switch
          filterFields            ;; Temporary Dictionary of valid field names
          fixupLambda              ;; Lambda for expression transformation
          syntaxLambda             ;; Lambda for expression syntax validation
          ;; Methods list 
          assertRules             ;; Method for defining fix up rules
          buildBody               ;; Method to build the SmartLisp expression body
          buildName               ;; Method for recognizing a Legacy field name
          cmdAll                  ;; Method for parsing the "all" command
          cmdOmit                 ;; Method for parsing the "omit" command
          cmdSort                 ;; Method for parsing the "bottom" and "top" commands
          errorStop               ;; Method for error recovery
          mergeRules              ;; Method for merging "all" criteria together
          pairToVector            ;; Method for converting a list into a vector
          replaceRules            ;; Method for recursive criteria replacement
          ) ;; end of persistent variables
   vars:(proc aCmd i j m n cmds scmd theSpec cmdVector)
   ;; Never initialize this Lambda more than once, because the
   ;; inline child Lambdas will overlay the cloned child Lambdas
   ;; in any clone copies of the Lambda and this causes serious
   ;; confusion when the reinitialized clone begins to affect
   ;; the persistant storage of the original Lambda.
   (if (<> cmdAll #void) (goto Continue:))
   ;; Initialize the inline child Lambdas.
   (defun assertRules()
       ;; We only need to run this once.
       (if (isLambda fixupLambda) (return true))
       ;; Define the expression fix up rules.
       (rulesLib)
       (setq fixupLambda (new rulesLib))
       ;; Make sure there are no duplication errors
       (if (= fixupLambda.rulesDic rulesLib.rulesDic) (error "dupRules"))
       ;; Operator name, function name, and number recognition rules
       (fixupLambda.assert $OP:(lambda(x) 
                                 vars:(s (d #{and and or or min min max max + + - - * * / / ** expt < < <= <= = = <> <> >= >= > >})) 
                                 (if (isString x) (setq s (symbol x)) (setq s x)) 
                                 (if (isMember s d) d[s]))
                                 ) ;; end assert
       (fixupLambda.assert $REL:(lambda(x) vars:((d #{< < <= <= = = <> <> >= >= > >})) (if (isMember x d) d[x])))
       (fixupLambda.assert $FN1:(lambda(x) 
                                 vars:(s (d #{not not sin sin cos cos tan tan log log exp exp sqrt sqrt})) 
                                 (if (isString x) (setq s (symbol x)) (setq s x)) 
                                 (if (isMember s d) d[s]))
                                 ) ;; end assert
       (fixupLambda.assert $FN2:(lambda(x) 
                                 vars:(s (d #{min min max max}))
                                 (if (isString x) (setq s (symbol x)) (setq s x)) 
                                 (if (isMember s d) d[s]))
                                 ) ;; end assert
       (fixupLambda.assert $NAM:(lambda(x) 
                                 vars:((d #(today))) 
                                 (if (not (isMember x d)) x))
                                 ) ;; end assert
       (fixupLambda.assert $NUM:(lambda(x) (if (isNumber x) x)))
       ;; Infix to Prefix notation production rules
       (fixupLambda.assert $PFN:(lambda(a fn x y b) 
                                   vars:(p)
                                   (setq p (list fn x y)) 
                                   (if (<> b #void) (setq p (append (list p) b))) 
                                   (if (<> a #void) (setq p (append a p))) 
                                   p))
       (fixupLambda.assert '($a* $X <$FN=$OP> $Y $b*) '(<$PFN> $a $FN $X $Y $b))
       ;; Constant folding production rules
       (fixupLambda.assert $DIV:(lambda(x) (error "queryBuildExp" (append "queryBuilder divide by zero on: " (string x true)))))
       (fixupLambda.assert $FOLD1:(lambda(op x) vars:(f) (setq f (getGlobalValue (symbol op))) (f x)))
       (fixupLambda.assert $FOLD2:(lambda(op x y) vars:(f) (setq f (getGlobalValue (symbol op))) (f x y)))
       (fixupLambda.assert '(<$Y=$OP> <$X=$NUM> <$Z=$NUM>) '(<$FOLD2> $Y $X $Z))
       (fixupLambda.assert '(<$Y=$FN1> <$X=$NUM>) '(<$FOLD1> $Y $X))
       (fixupLambda.assert '(<$Y=$FN2> <$X=$NUM> <$Z=$NUM>) '(<$FOLD2> $Y $X $Z))
       (fixupLambda.assert '(** $X $Y) '(expt $X $Y))
       ;; Algebraic expression reduction rules
       (fixupLambda.assert '(/ $X $X) 1)
       (fixupLambda.assert '(+ $X 0) '$X)
       (fixupLambda.assert '(- $X 0) '$X)
       (fixupLambda.assert '(* $X 0) 0)
       (fixupLambda.assert '(/ 0 0) 0)
       (fixupLambda.assert '(/ $X 0) '(<$DIV> (/ $X 0)))
       (fixupLambda.assert '(expt $X 0) 1)
       (fixupLambda.assert '(+ 0 $X) '$X)
       (fixupLambda.assert '(* 0 $X) 0)
       (fixupLambda.assert '(/ 0 $X) 0)
       (fixupLambda.assert '(expt 0 $X) 0)
       (fixupLambda.assert '(* $X 1) '$X)
       (fixupLambda.assert '(/ $X 1) '$X)
       (fixupLambda.assert '(expt $X 1) '$X)
       (fixupLambda.assert '(* 1 $X) '$X)
       (fixupLambda.assert '(expt 1 $X) 1)
       ;; _ANY_ empty indices reduction rules
       (fixupLambda.assert '(<$O=$REL> (ref (ref (ref x $X)) $Y) $Z) 
                          '(_ANY_ ($O (ref x $Y) $Z) (ref x $X)))
       ;; One based indices reduction rules
       (fixupLambda.assert '(ref (ref x $X) <$Y=$NUM>) '(ref (ref x $X) (sub1 $Y)))
       ;; Excess parentheses reduction rules
       (fixupLambda.assert '(($X*)) '$X)
       (fixupLambda.assert '(<$X=$NAM>) '$X)
       ;; Define the expression syntax validation rules.
       (setq syntaxLambda (new rulesLib))
       (syntaxLambda.setFailure false)
       ;; Make sure there are no duplication errors
       (if (= syntaxLambda.rulesDic fixupLambda.rulesDic) (error "dupRules"))
       ;; Prefix notation recognition rules
       (syntaxLambda.assert '(ref $X $Z) false)
       (syntaxLambda.assert '(<$Y=$FN1> <$X=$NUM>) '(<$FOLD1> $Y $X))
       (syntaxLambda.assert '(<$Y=$OP> $X $Z) false)
       (syntaxLambda.assert '(<$Y=$FN1> $X) false)
       (syntaxLambda.assert '(<$Y=$FN2> $X $Z) false)
       (syntaxLambda.assert '(today) false)
       ;; _ANY_ macro substitution rules
       (syntaxLambda.assert '(_ANY_ $A $B) 
                           '(mapc (lambda(x) (let ((r r)) (if $A (setq r true)) r)) $B))
       ;; Unrecognized syntax error rules
       (syntaxLambda.assert $ERR:(lambda(x) (error "queryBuildExp" (append "queryBuilder syntax error on: " (string x true)))))
       (syntaxLambda.assert '$ALL$ '(<$ERR> $ALL$))
       ;; Operator and function name recognition lambda rules
       (syntaxLambda.assert $OP:(lambda(x) 
                                 vars:(s (d #{and and or or min min max max + + - - * * / / expt expt < < <= <= = = <> <> >= >= > >}))
                                 (if (isString x) (setq s (symbol x)) (setq s x)) 
                                 (if (isMember s d) d[s]))
                                 ) ;; end assert
       (syntaxLambda.assert $FN1:(lambda(x) 
                                 vars:(s (d #{isBoolean isBoolean isNumber isNumber isDate isDate isSymbol isSymbol isString isString isDictionary isDictionary not not sin sin sub1 sub1 cos cos tan tan log log exp exp sqrt sqrt}))
                                 (if (isString x) (setq s (symbol x)) (setq s x)) 
                                 (if (isMember s d) d[s]))
                                 ) ;; end assert
       (syntaxLambda.assert $FN2:(lambda(x)
                                 vars:(s (d #{min min max max mapc mapc}))
                                 (if (isString x) (setq s (symbol x)) (setq s x)) 
                                 (if (isMember s d) d[s]))
                                 ) ;; end assert
       (syntaxLambda.assert $FOLD1:(lambda(op x) vars:(f) (setq f (getGlobalValue (symbol op))) (f x)))
       (syntaxLambda.assert $NUM:(lambda(x) (if (isNumber x) x)))
       true) ;; end assertRules
   (defun buildBody(v p begIndex endIndex)
   ;; Define the build expression body function. This function builds SmartLisp
   ;;  expression bodies.
   ;;  such as (1 {A1} true etc).
       vars:(fieldName s i n)
       (setq s "")
       (loop for i from begIndex until endIndex do
           (setq s (append s " " (buildName v[i] p)))
           ) ;; end loop
       ;; Apply expression transformation rules.
       (if (= p "x")
           (fixupLambda.setVerbose false)
           (fixupLambda.setVerbose false))
       (setq s (lisp s arithmetic:))
       (setq fixupLambda.singlePass false)
       (setq s (fixupLambda.apply s))
       (setq s (syntaxLambda.apply s))
       (setq s (string s true))
       s) ;; end buildBody
   (defun buildName(s p)
   ;; Define the build name function. This function recognizes Legacy
   ;;  field names inclosed in vertical bar | symbols, or SmarLisp constants
   ;;  such as (1 {A1} true etc).
       vars:(fieldName)
       (cond ;; Recognize a Legacy field name inclosed in vertical bar | symbols.
             ;;  Return the construct: p.|Fieldname|
             ((and (isType Symbol: s) (= s[0] #\|))
              (begin
                 (setq fieldName s)
                 (if (= (member fieldName _validFields) false)
                     (error "buildName"))
                 (setq filterFields[fieldName] _validFields[fieldName])
                 (append p "." fieldName)))
             ;; Recognize a Legacy field name not inclosed in vertical bar | symbols.
             ;;  Return the construct: p.|Fieldname|
             ((and (isType Symbol: s) (<> (member s _validFields) false))
              (begin
                 (setq fieldName s)
                 (setq filterFields[fieldName] _validFields[fieldName])
                 (append p "." fieldName)))
             ;; Recognize the today operator symbols.
             ;;  Convert the symbol to a List contant.
             ((= s today:)
              (append "(" s ")"))
             ;; Recognize all other non-arithmetic operator symbols.
             ;;  Convert the symbol to a string contant.
             ((and (isType Symbol: s) (= (member s #(isBoolean isNumber isDate isSymbol isString isDictionary and or not + - * / < > >= <= = <> ref |(| |)| |[| |]|)) false))
              (append "{" s "}"))
             ;; Repackage string constants within braces.
             ;;  Convert the string to a string contant.
             ((and (isString s) (= (member s #(isBoolean isNumber isDate isSymbol isString isDictionary and or not + - * / < > >= <= = <> ref |(| |)| |[| |]|)) false))
              (append "{" s "}"))
             ;; Recognize all quoted symbols.
             ;;  Convert the quoted symbol to a string contant.
             ((isType QuotedSymbol: s)
              (append "{" s "}"))
             ;; Assume we have a SmartLisp constant.
             ;;  Return the constant unaltered.
             (else s)
             ) ;; end cond
        ) ;; end buildName
   (defun cmdAll(parms s)
   ;; Define the "all" command parser.
   ;;   for example: "all = |Timeliness| 1"
       vars:(parmCnt outS)
       ;; Break the command up into its parameters.
       (setq parmCnt (length parms))
       ;; Check command for valid parameters.
       (if (= parmCnt 1)
           (return "")) 
       (if (< parmCnt 4)
           (error "queryBuilder" s)) 
       (if (<> parms[0] "all")
           (error "queryBuilder" s)) 
       (if (and (= parmCnt 4)
                (isBoolean (member parms[2] #("=" "<>" "<=" ">=" "<" ">"))))
           (error "queryBuilder" s))
       ;; Build the resulting find command.
       (setq outS (append "(select (lambda(x) (onError (lambda(s) false)) "
                          (buildBody parms "x" 1 parmCnt) "))"))
       outS) ;; end cmdAll
   (defun cmdOmit(parms s)
   ;; Define the "all" command parser.
   ;;   for example: "omit |Timeliness| = 5"
       vars:(parmCnt outS)
       ;; Break the command up into its parameters.
       (setq parmCnt (length parms))
       ;; Check command for valid parameters.
       (if (= parmCnt 1)
           (return "")) 
       (if (< parmCnt 4)
           (error "queryBuilder" s)) 
       (if (<> parms[0] "omit")
           (error "queryBuilder" s)) 
       (if (and (= parmCnt 4)
                (isBoolean (member parms[2] #("=" "<>" "<=" ">=" "<" ">"))))
           (error "queryBuilder" s))
       ;; Build the resulting find command.
       (setq outS (append "(select (lambda(x) (onError (lambda(s) false)) (not "
                          (buildBody parms "x" 1 parmCnt) ")))"))
       outS) ;; end cmdOmit
   (defun cmdSort(parms s)
   ;; Define the "top", "bottom", & "sort" command parsers.
   ;;   for example: "top |Price| 5"
       vars:(parmCnt lastParm outS n ratio sortOp)
       ;; Break the command up into its parameters.
       (setq parmCnt (length parms))
       (setq lastParm (subi parmCnt 1))
       ;; Check command for number of arguments.
       (cond 
           ;; Manage simple truncate: "top cutoff"
           ((and (= parmCnt 2) (isMember parms[0] #(top: bottom:)))
            (begin 
               (if (= parms[0] "top")
                   (error "topInvalid")
                   (setq sortOp "<="))
               (cond ((isInteger parms[1])
                      (setq ratio (append "" parms[1])))
                     ((and (isNumber parms[1]) (< parms[1] 1))
                      (setq ratio (append "(integer (* LDB.recordCount " parms[1] "))")))
                     ((= parms[1] "all")
                      (setq ratio "LDB.recordCount"))
                     (else
                      (error "queryBuilder" parms))
                   ) ;; end cond
               ;; Build the resulting find command.
               (setq outS (append "(truncate " ratio ")"))))
           ;; Manage simple field sort: "top,value1,cutoff"
           ((and (= parmCnt 3) (isMember parms[0] #(top: bottom:)))
            (begin 
               (if (= parms[0] "top")
                   (setq sortOp ">=")
                   (setq sortOp "<="))
               (cond ((isInteger parms[2])
                      (setq ratio (append "" parms[2])))
                     ((and (isNumber parms[2]) (< parms[2] 1))
                      (setq ratio (append "(integer (* LDB.recordCount " parms[2] "))")))
                     ((= parms[2] "all")
                      (setq ratio "LDB.recordCount"))
                     (else
                      (error "queryBuilder" parms))
                   ) ;; end cond
               ;; Build the resulting sort find commands.
               (setq outS (append "(sort (lambda(x y) (onError (lambda(s) false)) (" sortOp
                                  " " (buildName parms[1] "x")
                                  " " (buildName parms[1] "y") ")))" _eol "   "))
               (setq outS (append outS "(truncate " ratio ")"))))
           ;; Manage simple expression sort: "top,value1,/,value2,cutoff" 
           ;; Note: (for backward compatibility).
           ((and (= parmCnt 5) 
                 (isMember parms[0] #(top: bottom:)) 
                 (isNumber (member parms[2] #("+" "-" "*" "/"))))
            (begin 
               (if (= parms[0] "top")
                   (setq sortOp ">=")
                   (setq sortOp "<="))
               (cond ((isInteger parms[4])
                      (setq ratio (append "" parms[4])))
                     ((and (isNumber parms[4]) (< parms[4] 1))
                      (setq ratio (append "(integer (* LDB.recordCount " parms[4] "))")))
                     ((= parms[4] "all")
                      (setq ratio "LDB.recordCount"))
                     (else
                      (error "queryBuilder" parms))
                   ) ;; end cond
               ;; Build the resulting sort find commands.
               (setq outS (append "(sort (lambda(x y) (onError (lambda(s) false)) (" sortOp
                                   " (" parms[2] " " (buildName parms[1] "x") " " (buildName parms[3] "x") ")"
                                   " (" parms[2] " " (buildName parms[1] "y") " " (buildName parms[3] "y") ")"
                                   ")))" _eol "   "))
               (setq outS (append outS "(truncate " ratio ")"))))
           ;; Manage complex expression sort: "top,/,value1,value1,cutoff" 
           (else
            (begin 
               (if (= parms[0] "top")
                   (setq sortOp ">=")
                   (setq sortOp "<="))
               (cond ((isInteger parms[lastParm])
                      (setq ratio (append "" parms[lastParm])))
                     ((and (isNumber parms[lastParm]) (< parms[lastParm] 1))
                      (setq ratio (append "(integer (* LDB.recordCount " parms[lastParm] "))")))
                     ((= parms[lastParm] "all")
                      (setq ratio "LDB.recordCount"))
                     (else
                      (error "queryBuilder" parms))
                   ) ;; end cond
               ;; Build the resulting sort find commands.
               (setq outS (append "(sort (lambda(x y) (onError (lambda(s) false)) (" sortOp " "
                                   (buildBody parms "x" 1 lastParm) " "
                                   (buildBody parms "y" 1 lastParm)
                                  ")))" _eol "   "))
               (setq outS (append outS "(truncate " ratio ")"))))
           ) ;end cond
       outS) ;; end cmdSort
   (defun errorStop(err) (writeln "queryBuilder: " err) false)
   (defun mergeRules(cmds)      
   ;; Merges consecutive "all" rules together with an "and" conjunction.
   ;; Looks for "all" rules and eliminates them.
       vars:(cmdIndex args newCmds ruleLambda i)
       (loop for cmdIndex from 0 until (length cmds) do
           (cond
              ;; Replace "all" command with nothing,
              ;; because and "all" command is a noop.
              ((= cmds[cmdIndex] "all")
               (begin
                  (setq cmds (delete cmds cmdIndex))
                  (setq cmdIndex (max (- cmdIndex 2) -1))
                  )) ;; end begin
              ;; Replace "remarks" command with nothing,
              ;; because and "remarks" command is a comment.
              ((= (left cmds[cmdIndex] 7) "remarks")
               (begin
                  (setq cmds (delete cmds cmdIndex))
                  (setq cmdIndex (max (- cmdIndex 2) -1))
                  )) ;; end begin
              ;; Replace "omit" command with "all (not )",
              ((= (left cmds[cmdIndex] 5) "omit ")
               (begin
                  (setq newCmds (append "all (not ("
                                        (mid cmds[cmdIndex] 5 100000)
                                        "))"))
                  (setq cmds[cmdIndex] newCmds)
                  (setq cmdIndex (max (- cmdIndex 2) -1))
                  )) ;; end begin
              ;; Merge consecutive "all" commands together,
              ;; with a conjunctive "and" operator.
              ((and (= (left cmds[cmdIndex] 4) "all ")
                    (< (add1 cmdIndex) (length cmds))
                    (= (left cmds[(add1 cmdIndex)] 4) "all "))
               (begin
                  (setq newCmds (append "all ("
                                        (mid cmds[cmdIndex] 4 100000)
                                        ") and ("
                                        (mid cmds[(add1 cmdIndex)] 4 100000)
                                        ")"))
                  (setq cmds (delete cmds cmdIndex))
                  (setq cmds[cmdIndex] newCmds)
                  (setq cmdIndex (max (sub1 cmdIndex) -1))
                  )) ;; end begin
               ) ;; end cond
           ) ;; end of loop
       cmds) ;; end of mergeRules
   (defun pairToVector(v p)
       vars:(i n result)
       (if (= v #void) (setq v (new Vector: 0)))
       (setq n (length p))
       (loop for i from 0 until n do
          (if (isAtom p[i])
              (setq v[(length v)] p[i])
              (begin
                 (setq v[(length v)] |(|:)
                 (setq v (pairToVector v p[i]))
                 (setq v[(length v)] |)|:)
                 )) ;; end if
          ) ;; end loop
       v) ;; end pairToVector
   (defun replaceRules(cmds)
   ;; Recursively replaces rules with their command strings.
   ;; Looks for "selection" rules in the selectCriteriaLambda.
   ;; Looks for "exclusion" rules in the excludCriteriaLambda.
       vars:(cmdIndex args newCmds ruleLambda i)
       (loop for cmdIndex from 0 until (length cmds) do
           ;; Check for rule command
           (if (= (left cmds[cmdIndex] 8) "criteria")
               (begin
                  (setq args (objectToVector (lisp cmds[cmdIndex] arithmetic:)))
                  ;; Assign the type of rule (selection or exclusion).
                  (cond ((= (length args) 2)
                         (setq ruleLambda selectCriteriaLambda))
                        ((and (= (length args) 3) (= args[2] selection:))
                         (setq ruleLambda selectCriteriaLambda))
                        ((and (= (length args) 3) (= args[2] exclusion:))
                         (setq ruleLambda excludCriteriaLambda))
                        (else 
                         (error "ruleType"))
                        ) ;; end cond
                  ;; Retrieve the rule string and 
                  ;; covert to a vector of commands
                  (if (and (= (left args[1] 1) "{") (= (right args[1] 1) "}")) 
                      (setq args[1] (mid args[1] 1 (subi (length args[1]) 2))))
                  (setq newCmds (ruleLambda.Pv.getCriteria args[1]))
                  (setq newCmds (stringToVector newCmds ";"))
                  ;; Replace substitute rules within the original 
                  ;; vector of command strings.
                  (setq cmds[cmdIndex] newCmds[0])
                  (loop for i from 1 until (length newCmds) do
                      (setq cmds (vectorInsert cmds (addi cmdIndex i) newCmds[i]))
                      ) ;; end loop
                  ;; Make sure we retest the first substituted rule.
                  (-- cmdIndex)
                  )) ;; end if
           ) ;; end of loop
       cmds) ;; end of replaceRules
   ;; Initialize the Lambda.
   Continue::
   (onError errorStop)
   (setq filterFields (new Dictionary:))
   (assertRules)
   ;; Break the command up into its sub-commands.
   (setq cmds (stringToVector aSpec ";"))
   ;; Perform recursive rule substitution.
   (setq cmds (replaceRules cmds)) 
   ;; Establish the filter names array.
   (loop for i from 0 until (length cmds) do
       ;; Establish the filter names for each command.
       (setq cmdVector (pairToVector #void (lisp cmds[i] arithmetic:)))
       (loop for j from 0 until (length cmdVector) do
           (buildName cmdVector[j] "x")
           ) ;; end j loop
       ) ;end i loop
   ;; Add the filter names array as additional restrictions (if "nocheck" command not present).
   (if (and (> (length cmds) 0) (<> (trim cmds[0]) "nocheck"))
       (begin
          (loop for i from 0 until (length filterFields) do
             (if (and (<> filterFields[i 1] #void) (<> filterFields[i 1] isDictionary:))
                 (begin
                    (setq aCmd (append "all (" filterFields[i 1] " |" filterFields[i 0] "|)"))
                    (setq cmds (vectorInsert cmds 0 aCmd))  
                    ))) ;; end loop
          )) ; end if
   ;; Perform rule merging.
   (setq cmds (mergeRules cmds))
   ;; Parse each sub command and append to the specification.
   (setq theSpec "")
   (loop for i from 0 until (length cmds) do
       ;; Call the proper command parser (based on the command word).
       (setq cmdVector (pairToVector #void (lisp cmds[i] arithmetic:)))
       (cond 
           ((= cmdVector[0] "all") (setq scmd (cmdAll cmdVector cmds[i]))) 
           ((= cmdVector[0] "check") (setq scmd ""))
           ((= cmdVector[0] "nocheck") (setq scmd ""))
           ((= cmdVector[0] "bottom") (setq scmd (cmdSort cmdVector cmds[i])))
           ((= cmdVector[0] "omit") (setq scmd (cmdOmit cmdVector cmds[i]))) 
           ((= cmdVector[0] "top") (setq scmd (cmdSort cmdVector cmds[i])))
           (else (error "queryBuilder" cmds[i]))
           ) ; end cond
       (setq theSpec (append theSpec scmd _eol "   "))
       ) ;end loop
   ;; Convert the screen specification into a procedure.
   (setq proc (append "(lambda(LDB)" _eol "   "))
   (setq proc (append proc "vars:(restore select sort truncate)" _eol "   "))
   (setq proc (append proc "(setq truncate LDB.truncate)" _eol "   "))
   (setq proc (append proc "(setq restore LDB.restore)" _eol "   "))
   (setq proc (append proc "(setq select LDB.select)" _eol "   "))
   (setq proc (append proc "(setq sort LDB.sort)" _eol "   "))
   (if (<> theSpec "")
       (setq proc (append proc theSpec)))
   (setq proc (append proc "true)"))
   (if (= debugOn true) (writeln proc)) ;; Use for testing only
   (setq proc (eval proc))
   (if (not (isLambda proc)) (error "queryBuilder"))
   proc) ;; end queryBuilder

;;  ***NOTES***:
;; 
;; see the screenBuilder notes, this Lambda implements the same query language.





















;;**EXPORTKEY**:tableDbms:readBlockList
(defChildLambda tableDbms:readBlockList(blockList eraseSW)
;; *******************************************************************
;; summary:  Read the next record from the specified list of blocks. 
;;           The blockList is a vector of block keys. As a new block
;;           is read, it is stored in the cdr of the blockList. As
;;           a block is exhausted, it is erased from the repository,
;;           and the blockList is altered to reflect the erased block.
;; parent:   Librarian Lambda which manages the table database and
;;           controls the table database schema.
;; Args:     blockList   A vector of block keys to be read.
;;           eraseSW     True if blocks are to be erased after reading.
;; Return:   theRecord   The next database record (or #void).       
;; *******************************************************************
    vars:(blockKey theBlock theRecord) 
    ;; Return #void if there are no more records.
    (if (and (= (cdr blockList) #void) (<= (length blockList) 0)) (return #void))
    ;; Make sure the current block is loaded from the repository.
    ;; Note: If we must load a new block, then we
    ;;       erase the block from the repository
    ;;       and from the blockList after we load.
    (if (= (cdr blockList) #void)
        (begin
           (setq blockKey blockList[0])
           (setCdr blockList myOR[blockKey])
           (if (= eraseSW true)
               (setq myOR[blockKey] #void))
           (delete blockList 0)
           )) ;; end if
    ;; Read the next record from the current block.
    ;; Note: If we exhaust the current block, then we
    ;;       set the cdr of the blockList to #void.
    (setq theBlock (cdr blockList))
    (if (or (= theBlock #void) (<= (length theBlock) 0)) (return #void))
    (setq theRecord theBlock[0])
    (delete theBlock 0)
    (if (<= (length theBlock) 0) (setCdr blockList #void))
    ;; Return the next record.
    theRecord) ;; end of readBlockList




















;;**EXPORTKEY**:tableDbms:refColValues
(defChildLambda tableDbms:refColValues(col)
;; *******************************************************************
;; summary:  Return the column values vector for the specified column.
;; parent:   Librarian Lambda which manages the table database and
;;           controls the table database schema.
;; Args:     col      Either the integer index of the column
;;                    of the name of the column as a string or
;;                    symbol.   
;; Return:   true
;; *******************************************************************
    vars:(colIndex currentColumn colKey)
    ;; Set table database to transaction mode
    (beginTransaction myOR)
    ;; Create the column key.
    (if (isNumber col)
        (begin
           (if (= colVector #void) (setq colVector myIndex._colVector))
           (setq colIndex (integer col))
           (setq colKey (makeSymbol (append "_column:" colVector[colIndex])))
           ) ;; end then
        (setq colKey (makeSymbol (append "_column:" col)))
        ); end if
    ;; Return the specified column values vector.
    (setq currentColumn myOR[colKey])
    ;; Set table database to privileged access
    (abortTransaction myOR)
    currentColumn) ;; end refColValues





















;;**EXPORTKEY**:tableDbms:renameBlocks
(defChildLambda tableDbms:renameBlocks(blockList anIndex)
;; *******************************************************************
;; summary:  Rename the specified list of blocks. The block keys are
;;           positive integers. Each key is renamed to its negation
;;           and restored in the specified repository index. The
;;           blockList is altered to reflect the new keys.
;; parent:   Librarian Lambda which manages the table database and
;;           controls the table database schema.
;; Args:     blockList   A vector of block keys to be renamed.
;;           keyIndex    A Directory of block keys.
;; Return:   true        
;; *******************************************************************
    vars:(blockCount blockIndex oldKey newKey oldValue) 
    ;; Rename each block to its negation.
    ;; Note: This does not reposition the blocks  
    ;;       in the table database. The blocks
    ;;       are only renamed in the index.
    (setq blockCount (length blockList))
    (loop for blockIndex from 0 until blockCount do
        (setq oldKey blockList[blockIndex])
        (setq newKey (- oldKey))
        (setq blockList[blockIndex] newKey)
        (setq oldValue anIndex[oldKey])
        (setq anIndex[oldKey] #void)
        (setq anIndex[newKey] oldValue)
        ) ;; end of loop
    true) ;; end of renameBlocks




















;;**EXPORTKEY**:tableDbms:runStatistics
(defChildLambda tableDbms:runStatistics(statsLambda)
;; *******************************************************************
;; summary:  Run statistics for the current TableDbms database records.
;; parent:   Librarian Lambda which manages the table database and
;;           controls the table database schema.
;; Args:     statsLambda    Statistics Lambda to gather record statistics.
;; Return:   recordCount   The number of records selected.     
;; *******************************************************************
    vars:(blockCount           ;; Count of record blocks in table database
          blockList            ;; Block list for 
          record               ;; Record to test against the selectLambda
          ) ;; end temporary variables
    ;; Select only the records which match the select Lambda.
    ;; Note: The old blocks are renamed and deleted
    ;;       as they are read. Only the matched 
    ;;       records are kept.
    (beginTransaction myOR)
    (setq blockCount myIndex._blockCount)
    (setq blockList (_makeBlockList 0 blockCount blockCount))
    (reset)
    (clear statsLambda)
    ;; Read each of the old records and keep the ones which
    ;; match the selectLambda. The kept records are written
    ;; back to the database, and the others are disgarded.
    (setq record (_readBlockList blockList true))
    (while (<> record #void)
        (statsLambda record)
        (setq record (_readBlockList blockList true))
        ) ;; end while
    (reset)
    (abortTransaction myOR)
    recordCount) ;; end of runStatistics





















;;**EXPORTKEY**:tableDbms:select
(defChildLambda tableDbms:select(selectLambda)
;; *******************************************************************
;; summary:  Select only those records which match the selectLambda.
;; parent:   Librarian Lambda which manages the table database and
;;           controls the table database schema.
;; Args:     selectLambda   Select lambda value for matching records.
;; Return:   recordCount   The number of records selected.     
;; *******************************************************************
    vars:(blockCount           ;; Count of record blocks in table database
          blockList            ;; Block list for 
          record               ;; Record to test against the selectLambda
          ) ;; end temporary variables
    ;; Select only the records which match the select Lambda.
    ;; Note: The old blocks are renamed and deleted
    ;;       as they are read. Only the matched 
    ;;       records are kept.
    (beginTransaction myOR)
    (setq blockCount myIndex._blockCount)
    (setq blockList (_makeBlockList 0 blockCount blockCount))
    (_renameBlocks blockList myIndex)
    (restart)
    ;; Read each of the old records and keep the ones which
    ;; match the selectLambda. The kept records are written
    ;; back to the database, and the others are disgarded.
    (setq record (_readBlockList blockList true))
    (while (<> record #void)
        (if (selectLambda record) (addRow record))
        (setq record (_readBlockList blockList true))
        ) ;; end while
    (flush)
    (commitTransaction myOR)
    recordCount) ;; end of select





















;;**EXPORTKEY**:tableDbms:set1
(defChildLambda tableDbms:set1(row recordVector)
;; *******************************************************************
;; summary:  Add a row to the table database during Import processing. 
;;           Note: The table database is nonvolatile after import.
;; parent:   Librarian Lambda which manages the table database and
;;           controls the table database schema.
;; Args:     row           Row index (0 to n) of row to be added.
;;           recordVector  Row to be added to the table database.
;; Return:   true
;; Note:     This function is called from the importTab function whose
;;           setup processing is done in the importTableDbmsData Lambda.
;;           (see the tableDbms:importTableDbmsData Lambda).         
;; *******************************************************************
    ;; Only valid during the importTab function.
    (if (<> importingSW true) (error "NoAccess")) 
    ;; Allow numeric record keys only!
    (if (not (isNumber row)) (error "invalidKey")) 
    ;; Handle .SBF column header records.
    (if (and sbfFormatSW (= row 0))
        (begin 
            (setq myIndex._colVector recordVector) 
            (setq colVector recordVector)
            (setq myIndex._recordStructure (objectToStructure colVector #(#void)))
            (setq recordStructure myIndex._recordStructure)
            (if (compareNE (cnvColHeaders) colVector) (setq importFixupSW false))
            (return true)
            )) ;; end if
    ;; Handle separate column header records.
    (if (and (not sbfFormatSW) (= row 0))
        (begin
            (if (compareNE (cnvColHeaders) colVector) (setq importFixupSW false))
            (setq myIndex._colVector colVector)
            (setq myIndex._recordStructure (objectToStructure colVector #(#void)))
            (setq recordStructure myIndex._recordStructure)
            )) ;; end if
    ;; Ignore all other .SBF header records.
    (if (and sbfFormatSW (< row 3)) (return true))
    ;; Ignore all data records where the first field is #void.
    (if (= recordVector[0] #void)
        (begin
           (++ importNilRows)
           (return false)
           )) ;; end if
    ;; Reset record number to omit .SBF header records.
    (if sbfFormatSW (setq row (subi row 3)))
    (setq row (subi row importNilRows))
    ;; We get here only for valid import data records.
    ;; Fix up the newly imported data record.
    (resize recordVector (length myIndex._colVector))
    ;(if (and (= importFixupSW true) (isType Lambda: importFixup))
    (if (isType Lambda: importFixup)
        (importFixup recordVector myIndex._tableDate))
    ;; Compute block and record numbers.
    (setq blockKEY (divi row blockSize))
    (setq recordNUM (modi row blockSize))
    ;; Save the record in the current block
    (if (= currentBlock #void)
        (setq currentBlock (new Vector: object: 1)))
    (setq currentBlock[recordNUM] (objectToDictionary recordVector))
    ;; Save the block (if necessary)
    (if (>= (length currentBlock) blockSize)
        (begin
            (writeln "Importing record #" row)
            (setq myOR[blockKEY] currentBlock)
            (setq myIndex._blockCount (addi blockKEY 1))           
            (setq currentBlock #void)))
    (setq myIndex._recordCount (addi row 1))
    (setq recordCount myIndex._recordCount)
    true) ;; end of set1





















;;**EXPORTKEY**:tableDbms:smarttableToTableDbms
(defChildLambda tableDbms:smarttableToTableDbms(ST)
;; ********************************************************************
;; summary:  Save the specified Smarttable into the table database.
;;           after running production fixup.
;; Parms:    ST      The Smarttable of fixed up table data.
;; Return    true
;; ********************************************************************
    vars:(i j m colCount recCount record rowIndex colIndex)
    ;; Set table database to transaction mode
    (beginTransaction myOR)
    ;; Compute the column count and record count.
    (if (= colVector #void) (setq colVector myIndex._colVector))
    (setq colCount (length colVector))
    (setq recCount recordCount)
    ;; Save each row of the Smarttable into
    ;; the appropriate table database record.
    (loop for rowIndex from 0 until recCount do
        (setq record (copy recordStructure))
        (loop for colIndex from 0 until colCount do
            (setq record[colIndex] ST[colIndex rowIndex])
            ) ;; end of col loop
        (setRowSeq rowIndex record)
        ) ;; end of row loop
    ;; Set table database out of transaction mode.
    (commitTransaction myOR)
    true) ;; end tableToSmarttable





















;;**EXPORTKEY**:tableDbms:sort
(defChildLambda tableDbms:sort(sortLambda)
;; *******************************************************************
;; summary:  Sort the entire table database.
;; parent:   Librarian Lambda which manages the table database and
;;           controls the table database schema.
;; Notes:    This algorithm will increase the sort cluster factor
;;           during the critical initial sort phase (to match the
;;           available RAM memory). 
;; Args:     sortLambda   Sort lambda value for record pairs.
;; Return:   true        
;; *******************************************************************
    pvars:(;; Methods List
           _sortReadBigBlock   ;; Read many smaller blocks into the currentBlock, which becomes a giant block for sorting.
           _sortWriteBigBlock  ;; Break the currentBlock up into blocks of the correct blocking factor, then write each smaller block to the database.
           ) ;; end of persistent variables
    vars:(blockCount           ;; Count of record blocks in table database
          blockListOne         ;; Block list one for merge
          blockListTwo         ;; Block list two for merge
          clusterIndex         ;; Index for scanning clustered blocks
          clusterSize          ;; Size of clustered blocks (always a log of 2)
          endTime              ;; Ending time for sort
          freeSpace            ;; Number of bytes of free space
          mergeIndex           ;; Index of the current block to merge
          mergePassIndex       ;; Index of the current merge pass
          mergePassCount       ;; Number of merge passes required for this sort
          mergePassStart       ;; Starting value of the merge pass index
          mergeStepSize        ;; Number of blocks in each merge pass block list pair
          mergeRunCount        ;; Number of blocks in each merge pass block list
          recordOne            ;; Record one for merge
          recordTwo            ;; Record two for merge
          startTime            ;; Starting time for sort
          ) ;; end temporary variables
    ;; Never initialize this Lambda more than once, because the
    ;; inline child Lambdas will overlay the cloned child Lambdas
    ;; in any clone copies of the Lambda and this causes serious
    ;; confusion when the reinitialized clone begins to affect
    ;; the persistant storage of the original Lambda.
    (if (<> _sortReadBigBlock #void) (goto Continue:))
    (defun _sortReadBigBlock(blockIndex clusterSize blockCount)
        ;; Read many smaller blocks into the currentBlock,
        ;; which becomes a giant block for sorting.
        ;; Note: We have already read the first block 
        ;;       into currentBlock.
        vars:(newBlock clusterIndex stopIndex)
        (setq stopIndex (min clusterSize (subi blockCount blockIndex)))
        (beginTransaction myOR)
        (loop for clusterIndex from 1 until stopIndex do
            (setq newBlock myOR[(addi blockIndex clusterIndex)])
            ;(gc)(writeln "Read block [" (addi blockIndex clusterIndex) "] size = [" (sizeof newBlock) "] free = [" (inspect))  
            (setq currentBlock (append currentBlock newBlock))
            ) ;; end loop
        (abortTransaction myOR)
        true) ;; end of _sortReadBigBlock
    (defun _sortWriteBigBlock(blockIndex)
        ;; Break the currentBlock up into blocks of the correct blocking
        ;; factor, then write each smaller block to the database.
        vars:(newBlock clusterIndex clusterSize recordIndex blockStart blockLen)
        (commitTransaction myOR)
        (setq clusterSize (ceiling (/ (length currentBlock) blockSize)))
        (if (= clusterSize 1) (return (setq myOR[blockIndex] currentBlock)))
        (loop for clusterIndex from 0 until clusterSize do
            (setq blockStart (muli clusterIndex blockSize))
            (setq newBlock (^new Vector: object: (min blockSize (subi (length currentBlock) blockStart))))
            (setq blockLen (length newBlock))
            (loop for recordIndex from 0 until blockLen do
                (setq newBlock[recordIndex] currentBlock[(addi recordIndex blockStart)])
                ) ;; end recordIndex loop
            (setq myOR[(addi blockIndex clusterIndex)] newBlock)
            ) ;; end clusterIndex loop
        true) ;; end of _sortWriteBigBlock
    ;; Initialize the Lambda when called by new ObjectRepository: or attachLibrarian. 
    ;; This function always reattaches itself to the Repository (if necessary),
    ;; and always reestablishes the table block count, record count, and other statistics.
    Continue::
    ;; Sort the records in each block.
    ;; Note: This is the first part of a merge  
    ;;       sort of the entire table database.
    (beginTransaction myOR)
    (setq startTime (getTickCount 0))
    (setq blockCount myIndex._blockCount)
    (gc)
    (setq freeSpace (- (inspect) 10000000))
    (setq clusterSize 1)
    (commitTransaction myOR)
    (loop for blockKEY from 0 until blockCount do
        (setq currentBlock myOR[blockKEY])
        ;; Compute the cluster size (if this is the first block)
        (if (and (= blockKEY 0) (isPositive freeSpace))
            (begin
               (setq clusterSize (divi freeSpace (sizeof currentBlock) 4))
               (setq clusterSize (expt 2 (integer (log2 clusterSize))))
               (setq clusterSize (min clusterSize blockCount 16))
               ;(writeln "Cluster size = [" clusterSize "]")
               )) ;; end if
        ;; Read and sort blocks in clusters according to cluster size
        (_sortReadBigBlock blockKEY clusterSize blockCount)
        (^sort currentBlock sortLambda)
        (_sortWriteBigBlock blockKEY)
        ;; Adjust block index for size of cluster
        (setq blockKEY (sub1 (addi blockKEY clusterSize)))
        ) ;; end of loop
    (commitTransaction myOR)
    (setq currentBlock #void)
    (setq blockKEY #void)
    ;(writeln "Completed first pass sort.")
    ;; Perform the merge phase of the sort.
    ;; Note:  The number of merge passes is always equal
    ;;        to the log 2 of the number of blocks.
    ;; For Example:
    ;;        The merge phase of the sort for a database
    ;;        of seven blocks would appear as follows.
    ;;        (_mergeBlockList (makeVector 1 0) (makeVector 1 1) sortLambda)
    ;;        (_mergeBlockList (makeVector 1 2) (makeVector 1 3) sortLambda)
    ;;        (_mergeBlockList (makeVector 1 4) (makeVector 1 5) sortLambda)
    ;;        (_mergeBlockList (makeVector 1 6) (makeVector 0 0) sortLambda)
    ;;        (_mergeBlockList (makeVector 2 0 1) (makeVector 2 2 3) sortLambda)
    ;;        (_mergeBlockList (makeVector 2 4 5) (makeVector 1 6) sortLambda)
    ;;        (_mergeBlockList (makeVector 4 0 1 2 3) (makeVector 3 4 5 6) sortLambda)
    (if (< clusterSize blockCount)
        (begin
           (setq mergePassCount (integer (log2 blockCount)))
           (setq mergePassStart (integer (log2 clusterSize)))
           (if (<> mergePassCount (log2 blockCount)) (++ mergePassCount))
           (loop for mergePassIndex from mergePassStart until mergePassCount do
               ;(writeln "Starting merge pass [" mergePassIndex "] of [" mergePassCount "]")
               (setq mergeStepSize (expt 2 (add1 mergePassIndex))) 
               (setq mergeRunCount (divi mergeStepSize 2)) 
               (setq mergeIndex 0)
               (while (< mergeIndex blockCount)
                   (setq blockListOne (_makeBlockList mergeIndex mergeRunCount blockCount))
                   (setq blockListTwo (_makeBlockList (+ mergeIndex mergeRunCount) mergeRunCount blockCount))
                   (_mergeBlockList blockListOne blockListTwo sortLambda)
                   (+= mergeIndex mergeStepSize) 
                   ) ;; end merge stepping while
               ) ;; end merge passes loop
           )) ;; end merge if
    (setq endTime (integer (getTickCount startTime)))
    ;(writeln "Records sorted = " recordCount " in " endTime " Seconds" )
    true) ;; end of sort





















;;**EXPORTKEY**:tableDbms:tableLambdaToTableDbms
(defChildLambda tableDbms:tableLambdaToTableDbms(ST)
;; ********************************************************************
;; summary:  Save the specified tableLambda into the table database.
;; Parms:    ST      The tableLambda of table data.
;; Return    true
;; ********************************************************************
    vars:(i j m colCount recCount record rowIndex colIndex)
    ;; Set table database to transaction mode
    (beginTransaction myOR)
    ;; Compute the column count and record count.
    (if (= colVector #void) (setq colVector myIndex._colVector))
    (setq colCount (length colVector))
    (setq recCount recordCount)
    ;; Save each row of the tableLambda into
    ;; the appropriate table database record.
    (loop for rowIndex from 0 until recCount do
        (setRowSeq rowIndex ST[rowIndex])
        ) ;; end of row loop
    ;; Set table database out of transaction mode.
    (commitTransaction myOR)
    true) ;; end tableLambdaToTableDbms





















;;**EXPORTKEY**:tableDbms:tableToHistoryLambda
(defChildLambda tableDbms:tableToHistoryLambda(tableSize startRow rowIncr)
;; ********************************************************************
;; summary:  Convert the current table database into a tableLambda
;;           for running production selection criteria and for 
;;           production fixup.
;; Parms:    tableSize  The size of the history tableLambda.    
;; Return    ST         The tableLambda of table data.
;; ********************************************************************
    vars:(ST cols i j m colCount recCount 
          tableIncrement tableRow tableDate
          record rowIndex colIndex
          ) ;; end temporary variables
    ;; Set table database to transaction mode
    (beginTransaction myOR)
    ;; Compute the startRow, endRow and rowIncrement values.
    (setq recCount recordCount)
    (setq tableIncrement rowIncr)
    (setq tableRow startRow)
    ;; Retrieve the table database date.
    (setq tableDate myIndex._tableDate)
    (if (= tableDate #void)
        (begin
           (setq tableDate (now))
           (setq myIndex._tableDate tableDate)
           )) ;; end if
    ;; Retrieve the column names and make the tableLambda.
    (if (= colVector #void) (setq colVector myIndex._colVector))
    (setq colCount (length colVector))
    (tableLambda)
    (setq ST (new tableLambda))
    (ST.setSparse true)
    (ST.defColumns colVector)
    ;; Read each table database record
    ;; and add a new row to the tableLambda.
    (reset)
    (updateBegin)
    (loop for rowIndex from 0 until tableSize do
        (if (= (modi rowIndex 100) 0) 
            (writeln "Archiving record #" rowIndex " [Mfree = " (inspect) "] {TSize = " (sizeof ST) "}"))
        (setq record (updateRead tableRow))
        (ST.addRow record)
        (setq tableRow (integer (+ tableRow tableIncrement)))
        (if (>= tableRow recordCount) (goto OutOfRecords:))
        ) ;; end of row loop
	OutOfRecords::
    (updateEnd)
    ;; Set table database out of transaction mode.
    (abortTransaction myOR)
    ST) ;; end tableToHistoryLambda





















;;**EXPORTKEY**:tableDbms:tableToSmarttable
(defChildLambda tableDbms:tableToSmarttable(...)
;; ********************************************************************
;; summary:  Convert the current table database into a Smarttable
;;           for running production selection criteria and for 
;;           production fixup.
;; Parms:    startRow   The starting row (optional).    
;;           endingRow  The ending row (optional).    
;; Return    ST         The Smarttable of table data.
;; ********************************************************************
    vars:(ST cols i j m colCount recCount 
          record rowIndex colIndex 
          tableDate startRow endRow
          ) ;; end temporary variables
    ;; Set table database to transaction mode
    (beginTransaction myOR)
    ;; Retrieve the optional startRow and endRow arguments.
    (setq recCount recordCount)
    (if (= (argCount) 2)
        (begin
           (setq startRow (argFetch 0))
           (setq endRow (argFetch 1))
           ) ;; end then
        (begin
           (setq startRow 0)
           (setq endRow recCount)
           ) ;; end then
        ) ;; end if
    (if (< startRow 0) (setq startRow 0))
    (if (> endRow recCount) (setq endRow recCount))
    (if (>= startRow endRow)
        (begin
           (setq startRow 0)
           (setq endRow recCount)
           )) ;; end then
    ;; Retrieve the table database date.
    (setq tableDate myIndex._tableDate)
    (if (= tableDate #void)
        (begin
           (setq tableDate (now))
           (setq myIndex._tableDate tableDate)
           )) ;; end if
    ;; Create the column names and type structure.
    (if (= colVector #void) (setq colVector myIndex._colVector))
    (setq colCount (length colVector))
    (setq cols (new Vector: (muli colCount 2)))  
    (loop for i from 0 until colCount do
        (setq j (muli i 2))
        (setq cols[j] (symbol colVector[i]))
        (setq cols[(addi j 1)] formula:)
        ) ;; end of loop
    (setq cols (objectToStructure cols))
    ;; Make the Smarttable shell
    (setq ST (new Smarttable: cols))
    ;; Read each table database record
    ;; and create a new row of the Smarttable.
    (reset)
    (loop for rowIndex from startRow until endRow do
        (setq record (refRowSeq rowIndex))
        (loop for colIndex from 0 until colCount do
            (setq ST[colIndex (subi rowIndex startRow)] record[colIndex])
            ) ;; end of col loop
        ) ;; end of row loop
    ;; Set table database out of transaction mode.
    (abortTransaction myOR)
    ST) ;; end tableToSmarttable





















;;**EXPORTKEY**:tableDbms:tableToTableLambda
(defChildLambda tableDbms:tableToTableLambda(...)
;; ********************************************************************
;; summary:  Convert the current table database into a tableLambda
;;           for running production selection criteria and for 
;;           production fixup.
;; Parms:    startRow   The starting row (optional).    
;;           endingRow  The ending row (optional).    
;; Return    ST         The tableLambda of table data.
;; ********************************************************************
    vars:(ST cols i j m colCount recCount 
          record rowIndex colIndex 
          tableDate startRow endRow
          ) ;; end temporary variables
    ;; Set table database to transaction mode
    (beginTransaction myOR)
    ;; Retrieve the optional startRow and endRow arguments.
    (setq recCount recordCount)
    (if (= (argCount) 2)
        (begin
           (setq startRow (argFetch 0))
           (setq endRow (argFetch 1))
           ) ;; end then
        (begin
           (setq startRow 0)
           (setq endRow recCount)
           ) ;; end then
        ) ;; end if
    (if (< startRow 0) (setq startRow 0))
    (if (> endRow recCount) (setq endRow recCount))
    (if (>= startRow endRow)
        (begin
           (setq startRow 0)
           (setq endRow recCount)
           )) ;; end then
    ;; Retrieve the table database date.
    (setq tableDate myIndex._tableDate)
    (if (= tableDate #void)
        (begin
           (setq tableDate (now))
           (setq myIndex._tableDate tableDate)
           )) ;; end if
    ;; Retrieve the column names and make the tableLambda.
    (if (= colVector #void) (setq colVector myIndex._colVector))
    (setq colCount (length colVector))
    (tableLambda)
    (setq ST (new tableLambda))
    (ST.setSparse true)
    (ST.defColumns colVector)
    ;; Read each table database record
    ;; and add a new row to the tableLambda.
    (reset)
    (loop for rowIndex from startRow until endRow do
        (setq record (refRowSeq rowIndex))
        (ST.addRow record)
        ) ;; end of row loop
    ;; Set table database out of transaction mode.
    (abortTransaction myOR)
    ST) ;; end tableToTableLambda





















;;**EXPORTKEY**:tableDbms:updateBegin
(defChildLambda tableDbms:updateBegin()
;; *******************************************************************
;; Summary:  Prepares the table database Lambda for an extended series 
;;           of reads for update followed by writes of the rows
;;           previously read. 
;; Note:     Reads do not have to be sequential.
;;           Writes do not have to be sequential.
;;           Each update session must be terminated with an updateEnd
;; Args:     none
;; Return:   true
;; *******************************************************************
    (if updateSW (updateEnd))
    (setq updateSW true)
    (setq updateDirty false)
    (setq blockKEY #void)
    (setq currentBlock #void)
    (setq recordNUM 0)
    true) ;; end of updateBegin



















;;**EXPORTKEY**:tableDbms:updateEnd
(defChildLambda tableDbms:updateEnd()
;; *******************************************************************
;; Summary:  Terminates an extended series of reads for update followed
;;           by writes of the rows previously read. 
;; Note:     Reads do not have to be sequential.
;;           Writes do not have to be sequential.
;;           Each update session must be started with an updateBegin
;; Args:     none
;; Return:   true
;; *******************************************************************
    (if (not updateSW) (return false))
    ;; Save current block?
    (if updateDirty (setq myOR[blockKEY] currentBlock))
    (setq updateSW true)
    (setq updateDirty false)
    (setq blockKEY #void)
    (setq currentBlock #void)
    (setq recordNUM 0)
    true) ;; end of updateEnd



















;;**EXPORTKEY**:tableDbms:updateRead
(defChildLambda tableDbms:updateRead(row)
;; *******************************************************************
;; Summary:  Reads a record from the table database for later update.
;; Note:     Reads do not have to be sequential.
;;           Writes do not have to be sequential.
;;           Each update session must be started with an updateBegin
;; Args:     row      Row number of row to be read for later update.
;; Return:   record   The row just read. 
;; *******************************************************************
    vars:(newBlockKEY)
    (if (not updateSW) (error "accessDenied"))
    (if (or (< row 0) (>= row recordCount)) (error "badRowIndex"))
    ;; Compute new block and record numbers.
    (setq newBlockKEY (divi row blockSize))
    (setq recordNUM (modi row blockSize))
    ;; Need different block of records?
    (if (<> newBlockKEY blockKEY)
        (begin
           ;; Save current block?
           (if updateDirty (setq myOR[blockKEY] currentBlock))
           (setq updateDirty false)
           (setq blockKEY newBlockKEY)
           (setq currentBlock myOR[blockKEY])
           )) ;; end if
    ;; Return the requested record for later update.
    currentBlock[recordNUM]) ;; end of updateRead



















;;**EXPORTKEY**:tableDbms:updateReadStructure
(defChildLambda tableDbms:updateReadStructure(row)
;; *******************************************************************
;; Summary:  Reads a record (converted to a Structure) from the table
;;           database for later update.
;; Note:     Reads do not have to be sequential.
;;           Writes do not have to be sequential.
;;           Each update session must be started with an updateBegin
;; Args:     row      Row number of row to be read for later update.
;; Return:   record   The row just read (converted to a Structure). 
;; *******************************************************************
    vars:(record)
	(setq record (updateRead row))
    ;; Return the requested record (converted to a Structure) for later update.
    (setq record (objectToStructure (copy recordStructure) record))
    record) ;; end of updateReadStructure



















;;**EXPORTKEY**:tableDbms:updateWrite
(defChildLambda tableDbms:updateWrite(row record)
;; *******************************************************************
;; Summary:  Write a record from the table database during an extended
;;           update transaction.
;; Note:     Reads do not have to be sequential.
;;           Writes do not have to be sequential.
;;           Each update session must be started with an updateBegin
;; Args:     row      Row number of row to be read for later update.
;;           record   Row to be written to the table database.
;; Return:   record   The row just written. 
;; *******************************************************************
    vars:(newBlockKEY)
    (updateRead row)
    (setq updateDirty true)
    ;; Return the record just written.
    (setq currentBlock[recordNUM] record)
    record) ;; end of updateWrite



















;;**EXPORTKEY**:updateRowByKey
(defun updateRowByKey(SS key)
;; *************************************************************************************
;; summary:  This procedure gets a SmartRow from the specified Smarttable
;;           using the values in the key attributes of the key object. The key object
;;           may be any object which can be indexed by the key column names of the
;;           specified Smarttable. If there is no matching row, a new row is added with
;;           the specified key values.
;; args:     SS:                 The Smarttable to be searched for the row.
;;           key:                The key object whose attributes are used to find the row.
;; return:   row:                The SmartRow whose key attributes match the key object,
;;                               a new SmartRow whose key attributes are set to match 
;;                               the key object, or an error if there is more than one
;;                               SmartRow whose key attributes match the key object.
;; *************************************************************************************
   vars:(i r n colCount row col valueIndex value rowIndex colVector)
   ;; If there is a matching row, then return it.
   (setq row (getRowByKey SS key))
   (if (<> row #void) (return row))
   ;; Get the next available row index.
   (setq row (length SS))
   ;; Search the Smarttable columns looking for key columns.
   (setq colCount SS.colCount)
   (setq colVector SS.colVector)
   (loop for i from 0 until colCount do
      (if (= colVector[i].colFormat key:)
          (begin
             ;; Use the key column's name to get the index value from the key object.
             (setq value key[colVector[i].name])
             (setq col colVector[i].cellIndex)
             ;; Set the index value from the key object into the new row.
             (set SS col row value))))
   ;; Return the newly added row.
   SS[row])




























;;**EXPORTKEY**:validateColHeaders
(defun validateColHeaders(colHeaders)
;; ********************************************************************
;; summary:  Validate the legacy database column headers.
;; Parms:    colHeaders   The legacy data column headers.
;; Return    true
;; ********************************************************************
    vars:(i j m colCount colIndex)
    ;; There must be exactly the right number of column headers
    (setq colCount (length _validFields))
    (if (<> (length colHeaders) colCount) (error "badColHeaders"))
    ;; Each column header must match the valid column headers
    (loop for colIndex from 0 until colCount do
        (if (<> _validFields[colIndex 0] colHeaders[colIndex]) (error "badColHeaders"))
        ) ;; end of loop
    true) ;; end validateColHeaders





























;;**EXPORTKEY**:vectorAdd
(defun vectorAdd(x y)
;; *******************************************************************
;; summary:  Returns the sum of two vectors or of a vector and a constant. 
;;           If x and y are vectors of equal length,
;;           then x+y is: #(x[0]+y[0]  x[1]+y[1] .... x[N]+y[N]).
;;           If x is a vector and y is a constant,
;;           then x+y is: #(x[0]+y  x[1]+y .... x[N]+y).
;; Parms:    x:       A vector of length N.
;;           y:       A vector of length N or a constant.
;; Return:   v:       The vector sum of x+y
;; *******************************************************************
    vars:(i m n v)
    (setq n (length x))
    (setq v (new Vector: n))
    (if (isNumber y)
        (loop for i from 0 until n do
            (setq v[i] (+ x[i] y)))
        (begin
            (setq m (length y))
            (if (<> n m)
                (begin 
                   (ringBell)
                   (writeln "vectorAdd: vectors not the same length")
                   (writeln "(length x)=" n)
                   (writeln "(length y)=" m)
                   (writeln "x=" x)
                   (writeln "y=" y)
                   (setq n (min m n))))
            (loop for i from 0 until n do
                (setq v[i] (+ x[i] y[i])))))
    v)





























;;**EXPORTKEY**:vectorDotProduct
(defun vectorDotProduct(x y)
;; *******************************************************************
;; summary:  Returns the dot product of two vectors. If x and y are 
;;           vectors of equal length, 
;;           then xùy is: x[0]y[0] + x[1]y[1] + .... x[N]y[N].
;; Parms:    x:       A vector of length N.
;;           y:       A vector of length N.
;; Return:   v:       The product of xùy
;; *******************************************************************
    vars:(i m n sum)
    (setq n (length x))
    (setq m (length y))
    (if (<> n m)
        (begin 
            (ringBell)
            (writeln "vectorDotProduct: vectors not the same length")
            (writeln "(length x)=" n)
            (writeln "(length y)=" m)
            (writeln "x=" x)
            (writeln "y=" y)
            (setq n (min m n))))
    (loop for i from 0 until n do
        (+= sum (* x[i] y[i])))
    sum)





























;;**EXPORTKEY**:vectorProduct
(defun vectorProduct(x y)
;; *******************************************************************
;; summary:  Returns the product of two vectors or of a vector and a constant. 
;;           If x and y are vectors of equal length,
;;           then xøy is: #(x[0]y[0]  x[1]y[1] .... x[N]y[N]).
;;           If x is a vector and y is a constant,
;;           then xøy is: #(x[0]y  x[1]y .... x[N]y).
;; Parms:    x:       A vector of length N.
;;           y:       A vector of length N or a constant.
;; Return:   v:       The vector product of xøy
;; *******************************************************************
    vars:(i m n v)
    (setq n (length x))
    (setq v (new Vector: n))
    (if (isNumber y)
        (loop for i from 0 until n do
            (setq v[i] (* x[i] y)))
        (begin
            (setq m (length y))
            (if (<> n m)
                (begin 
                   (ringBell)
                   (writeln "vectorProduct: vectors not the same length")
                   (writeln "(length x)=" n)
                   (writeln "(length y)=" m)
                   (writeln "x=" x)
                   (writeln "y=" y)
                   (setq n (min m n))))
             (loop for i from 0 until n do
                 (setq v[i] (* x[i] y[i])))))
    v)





























;;**EXPORTKEY**:vectorSub
(defun vectorSub(x y)
;; *******************************************************************
;; summary:  Returns the difference of two vectors or of a vector and
;;           a constant. 
;;           If x and y are vectors of equal length,
;;           then x-y is: #(x[0]-y[0]  x[1]-y[1] .... x[N]-y[N]).
;;           If x is a vector and y is a constant,
;;           then x+y is: #(x[0]-y  x[1]-y .... x[N]-y).
;; Parms:    x:       A vector of length N.
;;           y:       A vector of length N or a constant.
;; Return:   v:       The vector difference of x+y
;; *******************************************************************
    vars:(i m n v)
    (setq n (length x))
    (setq v (new Vector: n))
    (if (isNumber y)
        (loop for i from 0 until n do
            (setq v[i] (- x[i] y)))
        (begin
            (setq m (length y))
            (if (<> n m)
                (begin 
                   (ringBell)
                   (writeln "vectorSub: vectors not the same length")
                   (writeln "(length x)=" n)
                   (writeln "(length y)=" m)
                   (writeln "x=" x)
                   (writeln "y=" y)
                   (setq n (min m n))))
            (loop for i from 0 until n do
                (setq v[i] (- x[i] y[i])))))
    v)






























;;**EXPORTKEY**:vectorToDelimitedString
(defun vectorToDelimitedString(vec delimiter)
;; ********************************************************************
;; name:     vectorToDelimitedString
;; 
;; summary:  Convert the specified vector into a Spreadsheet
;;                  Lambda for easy viewing.
;; Parms:      vec:          The vector to be converted.
;;                  delimiter:   The delimiter for the result string.
;; Return:     ss:             The resulting delimited string
;; ********************************************************************
    vars:(ss i m)
    (setq m (subi (length vec) 1))
    (setq ss "")
    (loop for i from 0 until m do
         (setq ss (append ss vec[i] delimiter))
         ) ;; end of loop
    (setq ss (append ss vec[m]))
    ss)





























;;**EXPORTKEY**:vectorToSpreadsheet
(defun vectorToSpreadsheet(v)
;; ********************************************************************
;; summary:  Convert the specified vector into a Spreadsheet
;;           Lambda for easy viewing.
;; Parms:    v:       The vector to be converted.
;;           ss:      The Spreadsheet version of the vector.
;; ********************************************************************
    vars:(ss i m)
    (setq m (length v))
    (setq ss (new Spreadsheet:))
    (loop for i from 0 until m do
            (setq ss[i 0] v[i]))
    ss)





























;;**EXPORTKEY**:wholeWordReplace
(defun wholeWordReplace(findme replaceme sourceVec)
;; *******************************************************************
;; summary:  This procedure replaces whole words with other whole words.
;; args:     findme        The target word to be replaced.   
;;           replaceme     The new replacement word.   
;;           sourceVec     The source file data to be altered.   
;; return:   sourceVec     The source file data after alteration.   
;; *******************************************************************
    vars:(sourceLen i j m n startPos findLEN replaceLEN)
    ;; Define alpha numeric check.
    (defun isAlphanum(cc) (or (isCharAlphabetic cc) (isCharNumeric cc)))
    ;; Find the length of the source data.
    (setq sourceLen (length sourceVec))
    (setq findLEN (length findme))
    (setq replaceLEN (length replaceme))
    ;; Replace target whole word with the replacement whole word.
    (loop for j from 0 until sourceLen do
       (if (isBoolean (setq startPos (find findme sourceVec j)))
           (setq j sourceLen)
           (begin
               (cond ((and (> startPos 0) (isAlphanum sourceVec[(sub1 startPos)])) (setq j (add1 startPos)))
                     ((and (< startPos (- sourceLen findLEN)) (isAlphanum sourceVec[(+ startPos findLEN)])) (setq j (add1 startPos)))
                     (else (setq sourceLen (length (setq sourceVec (replace sourceVec startPos findLEN replaceme)))))))))
    ;; Return the altered source data.
    sourceVec)































;;**EXPORTKEY**:writeTextFile
(defun writeTextFile(name self) 
;; *******************************************************************
;; summary:  Perform a complete write of the specified text file.
;; Parms:    This procedure accepts two arguments.
;;           name:      The name of the file to create.
;;           self:      The object to be saved in the file.
;; *******************************************************************
   vars:(fileID (type 0))
   (setq fileID (fileOpen name 1 type))
   (fileWrite fileID self)
   (fileClose fileID 1)
   self)






