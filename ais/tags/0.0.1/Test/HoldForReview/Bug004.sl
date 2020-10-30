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
