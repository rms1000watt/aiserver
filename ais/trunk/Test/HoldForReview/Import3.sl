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
;;  Title:    Import Export Test Suite Three
;;
;;  Author:   Michael F. Korns
;;
;;  Project:  Import Export test script.
;;
;;  Notes:    No other dependencies.  
;;

(define (readTextFile name) 
;; *******************************************************************
;; name:     readTextFile
;; 
;; summary:  Perform a complete read of the specified text file.
;; Parms:    This procedure accepts one argument.
;;           name:      The name of the file to open.
;; Return:   The byte vector containing the complete text file contents.
;; *******************************************************************
   vars:(fileID self (type 0))
   (setq fileID (fileOpen name 0 type))
   (setq self (fileRead fileID))
   (fileClose fileID 1)
   self)


;;  The diagnostic report function for this test script

(defun diagnostic(msg) (ringBell) (error "import" msg))
(writeln "Import/export Three test suite initialization started")

;;  Define a simple VmScript timing test procedure.
;;  Note:  This procedure accepts two arguments
;;         proc:      the procedure to be tested
;;         count:     the number of iterations to test
(define (timingTest proc count) vars:(startTime endTime)
    (setq startTime (getTickCount 0))
    (proc count) 
    (setq endTime (getTickCount startTime))
    (writeln "Elapsed time is " endTime " Seconds" ) endTime)

;;  Create an Lambda managing vectors  .
(defun VG()
    pvars:(aVc colVector self)
    (defun doClear() 
    	(setq aVc (new Vector: 0))
    	(setq colVector (new Vector: 0))
	) ;; end doClear
    (defun refRowCount() (length aVc))
    (defun refColCount() (length colVector))
    (defun Checkin(title) (writeln "Checked in " title))
    (defun ref1(ix) 
        (if (= (type ix) Symbol:)
            self.Pv[ix]
	    aVc[ix])
        ) ;; Allows Lambda polymorphism.
    (defun ref2(col row) aVc[row][col])
    (defun refColKeyness(col) formula:)
    (defun refColName(col) colVector[col])
    (defun refColType(col) "G")
    (defun set1(row recordVector) (set aVc row recordVector))
    (defun set2(col row newValue)
	(if (>= row (length aVc))
            (set aVc row (new Vector: (length colVector))))
	(setq aVc[row][col] newValue)
	) ;; end set2
    (defun doCellText(col row value) 
	(if (>= row (length aVc))
            (set aVc row (new Vector: (length colVector))))
	(setq aVc[row][col] (parse newValue))
	) ;; end doCellText
    (defun setColKeyness(col keyness) true)
    (defun setColName(col name) (setq colVector[col] name))
    (defun setColType(col type) true)
    (defun findRow(col value) false)
    (defun doSort(pred) false)
    (defun doDeleteRow(count row) (delete aVc row) (delete colVector row))
    (defun doAddRow(count row) (insert aVc row #void) (insert colVector row noname:))
    ;; Initialize the Lambda
    (setq aVc (new Vector: 0))
    (setq colVector (new Vector: 0))
    (setq self (myself))
    ) ;; end of VG

;; ***********************************************************  
;;  Test the importTab function for an Lambda managing records  
;; ***********************************************************

;;  Test the importTab function for the VG:Tough.sbf Lambda.
(writeln "Starting import:recordsOnly to Lambda managing records")
(VG)
(setq memFree (inspect))
(setq startTime (getTickCount 0))
(setq fileID (fileOpen "Tough2.sbf" 0 0))
(setq rowsIn (importTab fileID VG recordsOnly:))
(fileClose fileID 1)
(setq endTime (getTickCount startTime))
(writeln "Rows imported during importTab of VG:recordVectors:Tough.sbf is " (VG.refRowCount) " rows" )
(writeln "time required for importTab of VG:recordVectors:Tough.sbf is " endTime " Seconds" )
(writeln "space required for the Lambda is " (- memFree (inspect)) " bytes")
(if (<> (VG.refRowCount) rowsIn) 
    (writeln "Tough2.sbf row count is wrong"))

;; ***********************************************************
;;  Test the exportTab function for an Lambda managing records 
;; ***********************************************************

(writeln "Starting export:recordsOnly to Lambda managing records")

(setq fileID (fileOpen "Toughexp.sbf" 1 0))
(setq rowsOut (exportTab fileID VG recordsOnly:))
(fileClose fileID 1)
(writeln "Rows exported during exportTab of VG:recordVectors:Tough2.sbf is " (VG.refRowCount) " rows" )

;;  Make sure the number of rows imported = number of rows exported

(if (<> rowsOut (VG.refRowCount)) 
	(diagnostic (append "*FAILURE*: row counts for Tough2.sbf and Toughexp.sbf are not equal")))

(if (<> rowsOut rowsIn) 
	(diagnostic (append "*FAILURE*: row counts for Tough2.sbf and Toughexp.sbf are not equal")))


;; ******************************************** 
;; Re-import the result file and export again.  
;; Check the result files for equality
;; ********************************************


(setq fileID (fileOpen "Toughexp.sbf" 0 0))
(setq rowsIn (importTab fileID VG recordsOnly:))
(fileClose fileID 1)

(setq fileID (fileOpen "Toughexp2.sbf" 1 0))
(setq rowsOut (exportTab fileID VG recordsOnly:))
(fileClose fileID 1)


;; Check if the contents of the exported file is the same as the contents of the imported file

(setq fileOne (readTextFile "Toughexp.sbf"))
(setq fileTwo (readTextFile "Toughexp2.sbf"))

(if (<> fileOne fileTwo) 
	(diagnostic (append "*FAILURE*: files Tough2.sbf and Toughexp.sbf are not equal")))


;; *********************************************************************
;;  Test the importTab function for an Lambda managing Spreadsheet Fields
;; *********************************************************************


;; Define an Lambda to manage the spreadsheet

(defun AG()
    pvars:(aSS self)
    (defun doClear() (setq aSS (new Spreadsheet:)))
    (defun refRowCount() aSS.rowCount)
    (defun refColCount() (getColCount aSS))
    (defun Checkin(title) (writeln "Checked in " title))
    (defun ref1(name) self.Pv[name])
    (defun ref2(col row) aSS[col row])
    (defun set2(col row newValue) (set aSS col row newValue))
    (defun doCellText(col row value) (enterCellText aSS col row value))
    (defun doSort(pred) false)
    (defun doDeleteRow(count row) (deleteRow aSS count row))
    (defun doAddRow(count row) (addRow aSS count row))
    ;; Initialize the Lambda
    (setq aSS (new Spreadsheet:))
    (setq self (myself))
    ) ;; end of AG

;; *******************************************************************************
;;  Test the importTab function for an Lambda managing Spreadsheet using fieldsOnly:
;; *******************************************************************************

;;  Test the importTab function for the AG:Tough.sbf Lambda.
(AG)
(writeln "Starting fieldsOnly: import to Lambda managing a spreadsheet")
(setq memFree (inspect))
(setq startTime (getTickCount 0))
(setq fileID (fileOpen "Tough.sbf" 0 0))
(importTab fileID AG fieldsOnly:)
(fileClose fileID 1)
(setq endTime (getTickCount startTime))
(writeln "Rows imported during importTab of AG:fieldsOnly:Tough.sbf is " (AG.refRowCount) " rows" )
(writeln "time required for importTab of AG:fieldsOnly:Tough.sbf is " endTime " Seconds" )
(writeln "space required for the Lambda is " (- memFree (inspect)) " bytes")
(if (<> (AG.refRowCount) 246) 
    (writeln "Tough.sbf row count is wrong"))

;; ************************************** 
;; Check Random cells in the spreadsheet
;; ************************************** 

;; Test the contents of the first cell 
(setq col 0)
(setq row 3)
(if (<> AG[col row] "AAAAA1") 
	(diagnostic (append "*FAILURE*: AG cell " col " " row " is wrong")))


;; Test the contents of the last row first column
(setq col 0)
(setq row 245)
(if (<> AG[col row] "DATA1") 
	(diagnostic (append "*FAILURE*: AG cell " col " " row " is wrong")))


;; Test the contents of the last row, last column
(setq row 245)
(setq col 24)
(if (<> AG[col row] "DATA25") 
	(diagnostic (append "*FAILURE*: AG cell " col " " row " is wrong")))

;; **************************************************************************
;;  Test the exportTab:fieldsOnly function for an Lambda managing Spreadsheet 
;; **************************************************************************

(AG)
(writeln "Starting import:fieldsOnly to Lambda managing a Spreadsheet")
(setq fileID (fileOpen "Tough2.sbf" 0 0))
(setq rowsIn (importTab fileID AG fieldsOnly:))
(fileClose fileID 1)
(writeln "Rows exported during exportTab of AG:fieldsOnly:Toughexp.sbf is " (AG.refRowCount) " rows" )


(writeln "Starting export:fieldsOnly to Lambda managing a Spreadsheet")
(setq fileID (fileOpen "Toughexp.sbf" 1 0))
(setq rowsOut (exportTab fileID AG fieldsOnly:))
(fileClose fileID 1)
(writeln "Rows exported during exportTab of VG:fieldsOnly:Toughexp.sbf is " (AG.refRowCount) " rows" )

;;  Make sure the number of rows imported = number of rows exported

(if (<> rowsIn rowsOut) 
	(diagnostic (append "*FAILURE*: files Tough2.sbf and Toughexp.sbf are not equal")))



;; ******************************************** 
;; Re-import the result file and export again.  
;; Check the result files for equality
;; ********************************************

;(AG)
;setq fileID (fileOpen "Toughexp.sbf" 0 0))
;(setq rowsIn (importTab fileID AG recordsOnly:))
;(fileClose fileID 1)
;
;(setq fileID (fileOpen "Toughexp2.sbf" 1 0))
;(setq rowsIn (exportTab fileID AG recordsOnly:))
;(fileClose fileID 1)


;(setq fileOne (readTextFile "Toughexp.sbf"))
;(setq fileTwo (readTextFile "Toughexp2.sbf"))

;; Check if the contents of the exported file is the same as the contents of the imported file

;(if (<> fileOne fileTwo) 
	;(diagnostic (append "*FAILURE*: files Toughexp.sbf and Toughexp2.sbf are not equal")))

;; ***************************************************************************
;;  Define an Lambda managing importing of Smalltable cells using "Fields Only"
;; ***************************************************************************


;; Define an Lambda to manage the Smalltable

(defun AGSM(aTable)
    pvars:(aSM self)
    (defun doClear() true)
    (defun refRowCount() aSM.rowCount)
    (defun refColCount() aSM.colCount)
    (defun refColKeyness(col) aSM.colVector[col].colFormat)
    (defun refColName(col) aSM.colVector[col].name)
    (defun refColType(col) "G")
    (defun Checkin(title) (writeln "Checked in " title))
    (defun ref1(name) self.Pv[name])
    (defun ref2(col row) aSM[col row])
    (defun set2(col row newValue) (set aSM col row newValue))

    (defun setColKeyness(col keyness) (setq aSM.colVector[col].colFormat keyness))
    (defun setColName(col name)(setq aSM.colVector[col].name name))
    (defun setColType(col type) true)

    (defun doCellText(col row value) (enterCellText aSM col row value))
    (defun doSort(pred) false)
    (defun doDeleteRow(count row) (deleteRow aSM count row))
    (defun doAddRow(count row) (addRow aSM count row))
    ;; Initialize the Lambda
    (setq aSM aTable)
    (setq self (myself))
    ) ;; end of AGSM

;; *******************************************************************************
;;  Test the importTab function for an Lambda managing a Smalltable using fieldsOnly:
;; *******************************************************************************

;;  Test the importTab function for the AGSM:Lambda for manage a smalltable)

(setq SM (new Smarttable: small: 
               #{Column1: formula:
                 Column2: formula:
                 Column3: formula:
                 Column4: formula:
                 Column5: formula:  
                 Column6: formula:  
                 Column7: formula:  
                 Column8: formula:  
                 Column9: formula:  
                 Column10: formula:
                 Column11: formula:
                 Column12: formula:
                 Column13: formula:
                 Column14: formula:
                 Column15: formula:
                 Column16: formula:
                 Column17: formula:
                 Column18: formula:
                 Column19: formula:
                 Column20: formula:
                 Column21: formula:
                 Column22: formula:
                 Column23: formula:
                 Column24: formula:
				}))


(AGSM SM)
(writeln "Starting fieldsOnly: import to Lambda Managing a Smalltable")
(setq memFree (inspect))
(setq startTime (getTickCount 0))
(setq fileID (fileOpen "Tough2.sbf" 0 0))
(setq rowsIn (importTab fileID AGSM fieldsOnly:))
(fileClose fileID 1)
(setq endTime (getTickCount startTime))
(writeln "Rows imported during importTab of AGSM:fieldsOnly:Tough2.sbf is " (AGSM.refRowCount) " rows" )
(writeln "time required for importTab of AGSM:fieldsOnly:Tough2.sbf is " endTime " Seconds" )
(writeln "space required for the Lambda is " (- memFree (inspect)) " bytes")


(if (<> (AGSM.refRowCount) rowsIn) 
    (writeln "Tough2.sbf row count is wrong"))

;; ************************************** 
;; Check Random cells in the smarttable
;; ************************************** 

(setq col 0)
(setq row 0)
(if (<> AGSM[col row] "Column1") 
	(diagnostic (append "*FAILURE*: AGSM cell " col " " row " is wrong")))

(setq col 0)
(setq row 3)
(if (<> AGSM[col row] "AAAAA1") 
	(diagnostic (append "*FAILURE*: AGSM cell " col " " row " is wrong")))

(setq col 0)
(setq row 236)
(if (<> AGSM[col row] "ZZZZZ1") 
	(diagnostic (append "*FAILURE*: AGSM cell " col " " row " is wrong")))

(setq col 23)
(setq row 236)
(if (<> AGSM[col row] "ZZZZZ24") 
	(diagnostic (append "*FAILURE*: AGSM cell " col " " row " is wrong")))


;; ***************************************************************************
;;  Define an Lambda managing export:fieldsOnly for a smalltable
;; ***************************************************************************

(writeln "Starting export:fieldsOnly to Lambda managing a smalltable")

(setq fileID (fileOpen "Toughexp.sbf" 1 0))
(setq rowsOut (exportTab fileID AGSM fieldsOnly:))
(fileClose fileID 1)
(writeln "Rows exported during exportTab of AGSM:fieldsOnly:Toughexp.sbf is " (AGSM.refRowCount) " rows" )

;;  Make sure the number of rows imported = number of rows exported

(if (<> rowsIn rowsOut) 
	(diagnostic (append "*FAILURE*: files Tough2.sbf and Toughexp.sbf are not equal")))

;; *******************************************************************************
;;  Test the importTab function for an Lambda managing a Smarttable using fieldsOnly:
;; *******************************************************************************

;; Define a smarttable with 24 columns
(setq ST (new Smarttable: 
               #{Column1: formula:
                 Column2: formula:
                 Column3: formula:
                 Column4: formula:
                 Column5: formula:  
                 Column6: formula:  
                 Column7: formula:  
                 Column8: formula:  
                 Column9: formula:  
                 Column10: formula:
                 Column11: formula:
                 Column12: formula:
                 Column13: formula:
                 Column14: formula:
                 Column15: formula:
                 Column16: formula:
                 Column17: formula:
                 Column18: formula:
                 Column19: formula:
                 Column20: formula:
                 Column21: formula:
                 Column22: formula:
                 Column23: formula:
                 Column24: formula:
				}))


;;  Test the importTab function for the VG:Tough.sbf Lambda.
(AGSM ST)

(writeln "Starting fieldsOnly: import to Lambda Managing a Smarttable")
(setq memFree (inspect))
(setq startTime (getTickCount 0))
(setq fileID (fileOpen "Tough2.sbf" 0 0))
(setq rowsIn (importTab fileID AGSM fieldsOnly:))
(fileClose fileID 1)
(setq endTime (getTickCount startTime))
(writeln "Rows imported during importTab of AGSM:fieldsOnly:Tough2.sbf is " (AGSM.refRowCount) " rows" )
(writeln "time required for importTab of AGSM:fieldsOnly:Tough2.sbf is " endTime " Seconds" )
(writeln "space required for the Lambda is " (- memFree (inspect)) " bytes")


(if (<> (AGSM.refRowCount) rowsIn) 
    (writeln "Tough2.sbf row count is wrong"))

;; ************************************** 
;; Check Random cells in the smarttable
;; ************************************** 


(setq col 0)
(setq row 0)
(if (<> AGSM[col row] "Column1") 
	(diagnostic (append "*FAILURE*: AGSM cell " col " " row " is wrong")))

(setq col 0)
(setq row 3)
(if (<> AGSM[col row] "AAAAA1") 
	(diagnostic (append "*FAILURE*: AGSM cell " col " " row " is wrong")))

(setq col 0)
(setq row 236)
(if (<> AGSM[col row] "ZZZZZ1") 
	(diagnostic (append "*FAILURE*: AGSM cell " col " " row " is wrong")))

(setq col 23)
(setq row 236)
(if (<> AGSM[col row] "ZZZZZ24") 
	(diagnostic (append "*FAILURE*: AGSM cell " col " " row " is wrong")))


;; ***************************************************************************
;;  Define an Lambda managing export:fieldsOnly for a smarttable
;; ***************************************************************************

(writeln "Starting export:fieldsOnly to Lambda managing a smarttable")

(setq fileID (fileOpen "Toughexp.sbf" 1 0))
(setq rowsOut (exportTab fileID AGSM fieldsOnly:))
(fileClose fileID 1)
(writeln "Rows exported during exportTab of AGSM:fieldsOnly:Toughexp.sbf is " (AGSM.refRowCount) " rows" )

;;  Make sure the number of rows imported = number of rows exported

(if (<> rowsIn rowsOut) 
	(diagnostic (append "*FAILURE*: files Tough.sbf and Toughexp.sbf are not equal")))


;
;;  The test script has completed
(writeln "Import/export test suite three completed")











































