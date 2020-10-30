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

;;**EXPORTKEY**:egm
(defun egm(...)
;; *******************************************************************
;; summary: Creates a new evolutionary grid machine Lambda instance.
;;          Form 1: create a new egm instance with initial training information
;;              (setq egmInstance (math.egm cursor modelStructure trainedToDate previousPeriods))
;;          Form 2: create a new egm instance with previously saved egm state information
;;              (setq egmInstance (math.egm s)) 
;;          s is structure containing saved pvars from former instance of egm
;; 
;;			Call Form 1 to ready machine for training on a summary table cursor.
;; 		    See egm.setMyPvars for logic behind Form 2.
;;			The cursor is reduced to a dense XY sigmod vector array (grid) using the 
;;			supplied baseExpressions, fieldExpressions and trainExpressions.
;;
;;          Evolutionary grid machines (egm) are quasi regression engines which
;;          learn and make regression estimates on XY vector arrays such as:
;;
;;               XY:  An NumRows by NumX + NumY array representing the original observations
;;                    in the form of:    x x x ...  y y ...
;;                                       x x x ...  y y ...
;;                                           ... 
;;                                       x x x ...  y y ...
;;
;;					NumRows is the number of rows in the observation set
;;					NumX is the number of independent columns (x values)
;;					NumY is the number of dependent columns (y values) for which regressions are to be peformed
;;
;;			The XY vector array (grid) is constructued by applying the baseExpressions against
;;			the cursor to reduce the cursor to NumRows. Then the fieldExpressions 
;;			are applied to the cursor to produce NumX independent columns (x values). Then
;;			the trainExpressions are applied to the cursor to produce NumY dependent
;;			columns (y values). Then the XY grid independent columns are crunched to dense 
;;			sigmod values.
;;
;;			The resulting XY grid is stored in the egm pvars. Each egm instances is 
;;			stored persistently by the calling application. In egm.selfTest, each egm
;;			instance is stored in memory during the test. In deepGreen applications each
;;			egm instance is stored in a repository to provide persistence across machine
;;			training and runs - see egm.trainMachine and egm.runMachine.
;;
;;			egmInstance.trainMachine
;;			  This Lambda trains (performs the quasi regression) on the XY grid owned by the instance 		
;;
;;			egmInstance.runMachine
;;		  	  This Lambda applies one or more trained instances to a table of data.
;;
;;
;; Form 1 Parms:    
;;(setq estimator (math.egm cursor modelStructure previousPeriods))
;;	Args:
;;		cursor			- dataMineLib table containing estimator Lambda data. The table must contain
;;							a column called CurrentDate and this date must be the date of the
;;							independent values associated with the table. All rows in the table must
;;							have the same date. Note that other values are extracted from the table 
;;							for populating the estimator Lambda's XY matrix using the BaseFilters, 
;;							CutFilters and TrainingExression passed as members of the modelStructure 
;;							argument described next. 
;;			
;;		modelStructure	- Structure containing:
;;			.BaseFilters		- BaseFilters reduce the input table to a set suitable for use.
;;			.Cabinet			- AnalystName
;;			.CutFilters		    - CutFilters are the field expressions (independent variables)
;;			.CoreExpCount		- Number of filter expressions belonging to core genome population
;;			.MaxDepth		    - Maximum depth of field expression cross correlation to perform. Can not exceed 10.
;;			.Morph			    - Boolean flag indicating weather to use morphing or not
;;			.Bucketsize		    - Bucketsize (usual and default is 5)
;;			.NumTopGridSelCore  - Number of top Grid selectors to keep for core genome population
;;			.NumTopGridSelSec   - Number of top Grid selectors to keep for secondary genome population
;;			.TrainExpressions	- Training expressions (dependent variables)
;;			.TrainingBias		- minimum number of days separating tables - this value is related to the
;;								     training expressions. For instance, a training expression of
;;								     Next3MonthProfit would imply a training bias of 91 days.
;;			TrainingQtrs:	1	- The number of 
;;		    trainedTo	 	    - the first date that the estimator Lambda can be used in training
;;							         or testing. 
;;		previousPeriods 	    - a vector of dates associated witht the regressMemories
;;	                                 that will be passed in the trainMachine call documented next.
;; Return:   Rf:                - A new evolutionary grid machine Lambda instance.
;;
;; *******************************************************************
  pvars:(;; Public Variables
		;; All of these will be copied by getMyPvars and
		;; set by setMyPvars to facilitate exteral storage of the data
		;; portion of an egm instance.
		AllExp				;; Concatenation of FieldExp and TrainExp
		AllStocks			;; Vector of Directories for each y. Each directory is list of stocks picked with count.
		AnalystName			;; AnalystName -- documents creator of instance
		BaseExp				;; Saved so it can be applied to cursor passed with runMachine
		BucketSize			;; The size of individual buckets in the grid
		(CoreDone false)	;; Flag indicating if the core genome population has been trained
		CoreExpCount		;; Number of field expressions included in core genome population
		Evolve				;; Flag indicating if the estimator Lambda has and should evolve a secondary population
		FieldExp			;; Saved so it can be applied to cursor passed with runMachine
		Genome				;; This short vector has the following schema:
							;; index		value description
							;;	0		index into morph vector
							;; 	1..n		indices into field expressions
		LowRanksCore	    ;; Vector of current lowest rank for each dependent variable from core genome population
		LowRanksSec			;; Vector of current lowest rank for each dependent variable from secondary genome population
		LowRanksIndexCore	;; Vector of index into TopGridSelCore[t] to lowest rank entry for dependent variable from core genome population
		LowRanksIndexSec	;; Vector of index into TopGridSelSec[t] to lowest rank entry for dependent varaible from secondary genome population
		MaxDepth			;; maximum number for column cross-correlation to perform
		Morph				;; Use morph vectors? true/false
		NumAllExp			;; Length of all expressions .. equal to NumCols
		NumBaseExp			;; Length of base expressions vector
		NumCols				;; The number of cols in XY
		NumFieldExp			;; Length of field expressions vector .. equal to NumX
		NumGenome			;; number of elements in Genome
		NumRows				;; The number of rows in XY
		NumTrainExp			;; Length of training expressions vector .. equal to NumY
		(NumTopGridSelCore 100)	;; Number of top grid selectors to keep for each dependent variable from core genome population
		(NumTopGridSelSec 100)	;; Number of top grid selectors to keep for each dependent variable from secondary genome population
		NumX				;; Number of independent variables
		NumY				;; Number of dependent variables
		PreviousPeriods		;; Vector of TVAL dates of previous periods to train on in trainMachine
		RankByWinPct		;; boolean value indicating the use of winPct rather than score to rank grid selectors in topGridSelectors vector
		(SecondaryDone false) ;; Flag indicating if secondary genome population has been trained
		StockIDs			;; Vector of stock IDs - matching indicies with XY.
		(Summerized false)	;; Flag variable for public child Lambda summerize.
		TableDate			;; Date associated with independent data values
		TopGridSelCore		;; Vector of Vectors of top scoring genome grid selector structures for each dependent variable from
							;; core genome population
		TopGridSelSec		;; Vector of Vectors of top scroring genome grid selector structures for each dependent variable from 
							;; secondary genome population
		TrainingBias		;; Number of days of training bias. This number is associated with the training expressions used
							;; in the table. For instance, if Next3MonthProfit is used as a training field then the training
							;; bias is 91 days. This number is added to the TableDate to get the TrainedToDate.
		TrainedToDate		;; The first date the estimator Lambda can be used. This date the TableDate plus the TrainingBias. 
		TrainExp			;; Saved so it can be applied to cursor passed with runMachine.
		XY					;; Dense sigmod training vector. Created from cursor passed on Lambda creation.


		;; Private Child Lambdas
         _inputCursor    	;; Crunch cursor input into a dense sigmoid XY training vector array - uses baseExpressions and fieldExpressions
		_setMyPvars			;; Assign values from structure argument to pvars of Lambda - used to initlize egm Lambda from previously stored Lambda data
		_init
		_prettyPrintMemory

     	;; Public Child Lambdas
        exportEmbeddedOntology ;; Export all embedded ontology objects from the specified Lambda.
        runMachine			;; Run this percentile grid machine on the XY sample vector array 
        trainMachine		;; Train this percentile grid machine on the XY training vector array.
		clearSecondary		;; Clear the training information for the secondary genome population
		getTotalY			;; Get the toal value of the specified dependent variable for the XY stored in this estimator instance
		getMyPvars			;; Extract data portion of Lambda for external storage
		selfTest			;; Perform a training and return trained instance of the Lambda
		summerize			;; Perform summary calculations on an estimator Lambdas pvars. This is where we do the calculations that 
							;; we do not want to do each time we call trainMachine. Remember that trainMachine may be call many many times
							;; with small time slices. It is better to call summerize later when we want to "use" a previously trained
							;; estimator Lambda result. Note that the flag variable Summerized is set by the summerize routine and cleared
							;; by subsequent calls to trainMachine.

         ) ; end persistant variables

   ;; *******************************************************************************
   ;; Define Private Child Lambdas 
   ;; *******************************************************************************
   ;; Export all embedded ontology objects from this Lambda.
   ;; Note: (egm.exportEmbeddedOntology "WorkingDocs\\")
   (defun exportEmbeddedOntology(exportFolder)
      regs:(k K m M n N)
      vars:(embeddedOntologyNames embeddedOntologyPrefix tempNames currentFocus)
      vars:(ontologyObject (cabinetPrefix ";#text#") (cabinetPrefixLength 7))
      vars:((targetCabinetName "Egm") (targetLambdaName "egm"))

	  ;; Convert the argument and initialize.
	  (setq exportFolder (string exportFolder))
	  (setq embeddedOntologyPrefix (append targetLambdaName ":%%"))
	
      ;; Load the names of all embedded Ontology objects.
      (setq currentFocus (browseLib.getFocus))
      (browseLib.setFocus targetCabinetName)
      (setq tempNames (browseLib.getChildNames))
      (browseLib.setFocus currentFocus)
      (setq K (length embeddedOntologyPrefix))
      (setq N (length tempNames))
      (setq m -1)(setq embeddedOntologyNames (new Vector:))
      (loop for n from 0 until N do (if (stringCiEQ (left tempNames[n] K) embeddedOntologyPrefix) (setq embeddedOntologyNames[(++ m)] (mid tempNames[n] K 100000))))  
      (setq N (length embeddedOntologyNames))

      ;; Export all embedded Ontology objects (only export no HTML generation).
      (setq N (length embeddedOntologyNames))
      (loop for n from 0 until N do
         (setq ontologyObject (browseLib.checkout targetCabinetName (append embeddedOntologyPrefix embeddedOntologyNames[n])))
         (setq ontologyObject (mid ontologyObject cabinetPrefixLength 10000000))
         (browseLib.writeSourceFile (append exportFolder embeddedOntologyNames[n] ".html") ontologyObject)
         ) ; end loop
      true) ; end exportEmbeddedOntology
   ;; Create a pretty print string of the specified training memory for self-descriptive storage.
   (defun _prettyPrintMemory(buffer memory margin)
      vars:(n N key newMargin sourceTemplate
           (blanks "                                                                                                                                                              ")
           (tailSW false)
           ) ; end temporary variables
      ;; Initialize the display buffer (if necessary).
      (if (= buffer #void)
          (begin 
             (setq buffer (new Vector: byte: 20000000))
             (setq sourceTemplate (mid (browseLib.checkout Analyst: "analystWizard:pgmRegress:%MEMORY_HDR") (+ 7 (length _eol)) 100000))
             (appendWriteln buffer sourceTemplate _eol (left blanks margin))
             (setq tailSW true)
          )) ; end if
      ;; Validate the display buffer (if necessary).
      (cond 
         ;; Manage pretty printing of a Structure.
         ((= (type memory) Structure:)
          (begin
             (setq N (length memory))
             (setq newMargin (+ margin 2))
             (appendWriteln buffer "#{")
             (loop for n from 0 until N do  
                (setq key memory[n 0])
                (appendWriteln buffer key ": ")
                (_prettyPrintMemory buffer memory[n 1] (+ newMargin (length key) 2))
                (appendWriteln buffer _eol (left blanks newMargin))
                ) ; end loop
             (appendWriteln buffer "} ; end Structure")
          )) ; end Structure case
         ;; Manage pretty printing of a Dictionary.
         ((= (type memory) Dictionary:)
          (begin
             (setq N (length memory))
             (setq newMargin (+ margin 2))
             (appendWriteln buffer "#{dic|| " _eol  (left blanks newMargin))
             (loop for n from 0 until N do  
                (setq key memory[n 0])
                (appendWriteln buffer key ": ")
                (appendWriteln buffer (string memory[n 1] true))
                (appendWriteln buffer _eol (left blanks newMargin))
                ) ; end loop
             (appendWriteln buffer "} ; end Dictionary")
          )) ; end Dictionary case
         ;; Manage pretty printing of a Directory.
         ((= (type memory) Directory:)
          (begin
             (setq N (length memory))
             (setq newMargin (+ margin 2))
             (appendWriteln buffer "#{dir|| " _eol  (left blanks newMargin))
             (loop for n from 0 until N do  
                (setq key memory[n 0])
                (appendWriteln buffer (string key true) " ")
                (appendWriteln buffer (string memory[n 1] true))
                (appendWriteln buffer _eol (left blanks newMargin))
                ) ; end loop
             (appendWriteln buffer "} ; end Directory")
          )) ; end Directory case
         ;; Manage pretty printing of an Object Vector.
         ((= (type memory) ObjVector:)
          (begin
             (setq N (length memory))
             (setq newMargin (+ margin 6))
             (appendWriteln buffer "#(obj|" _eol (left blanks newMargin))
             (loop for n from 0 until N do  
                (_prettyPrintMemory buffer memory[n] newMargin)
                (appendWriteln buffer " ;; [" n "]" _eol (left blanks newMargin))
                ) ; end loop
             (appendWriteln buffer ") ; end ObjVector")
          )) ; end ObjVector case
         ;; Manage pretty printing of a String.
         ((= (type memory) String:)
          (begin
             (appendWriteln buffer "{" (substitute (substitute (substitute (substitute memory _eol #\tab) #\tab " ") #\return " ") #\newline " ") "}")
          )) ; end ObjVector case
         ;; Manage pretty printing of default value.
         (else
             (appendWriteln buffer (string memory true))
         ) ; end other case
         ) ; end cond
      ;; Terminate the display buffer (if necessary).
      (if (= tailSW true)
          (begin 
             (setq sourceTemplate (mid (browseLib.checkout Analyst: "analystWizard:pgmRegress:%MEMORY_TAIL") 7 100000))
             (appendWriteln buffer sourceTemplate)
             (setq buffer (string buffer))
          )) ; end if
      buffer) ; end prettyPrintMemory

   ;; *******************************************************************************
   ;; Define Public Child Lambdas 
   ;; *******************************************************************************

;selfTest - full process test of the math.egm Lambda. The only dependencies on 
;DeepGreen are the FASummary tables.
;
;selfTest processes tables of data managed by dataMineLib. These tables
;are specified by the selfTest.tableList structure initialized in selfTest. Each 
;table is associated with a date. These tables are 91 and 98 days apart.
;
;These tables contain both the independent and dependent values. The independent
;values are "as of" the table's date. The dependent values vary in their date
;association based on their content. A common example would be Next3MonthProfit
;in the stock summary tables. Next3MonthProfit contains the profit for a stock
;for the three month period subsequent to the tables date. Since we are using
;the quarterly tables, no dependent expressions can have more than a 98 day bias.
;
;Estimator Lambdas contain both the dependent and independent values from the
;tables. We identify estimator Lambdas by the date associated with the 
;DEPENDENT values in the estimator Lambda. Note that some other math Lambdas,
;like the PGM, identify estimator Lambdas by the date associated with the 
;independent values used to train the estimator Lambda. Don't let this ball
;you up!
;
;EGM estimator Lambdas are created using calls like:
;	(setq estimator (math.egm cursor modelStructure trainedToDate previousPeriods))
;
;EGM estimator Lambdas are trained using calls like:
;	(estimator.trainMachine timeToTrain regressMemories)
;
;EGM estimator Lambdas are applied to a cursor by calls like:
;	(esitmator.runMachine cursor backCountPeriods)
;
;selfTest's training is controlled by the following parameters assigned in the pvars:
;		tableList		; Structure of table specification vectors of the form:
;							#{tablekey: #(XlsFilename tableName tabledate) ...}
;		(backCount 2) 	; Number of estimator Lambdas passed to egn.runMachine
;		(backPeriods 4) 	; Number of tables processed.
;		
; Pay particular attention to the processing of the tables and their associated dates
; in this routine.
;
(defun selfTest() 
	vars:( b j k p n N r R y i 
		(backCount 2) ; Number of estimator Lambdas passed to egm.runMachine
		backCountPeriods	; Vector of estimator Lambdas specified by backCount
		backPeriodEstimators
		currrentDate
		cursor 
		end
		estimator
		estimators
		first
		memory
		modelStructure
		notes
		numFound
		(numPreviousPeriods 2) ; number of estimator Lambdas passed to egm.trainMachine
		previousEstimator
		previousPeriods
		previousPeriodEstimators
		record
		regressMemories
		Rf
		start
		tableDate
		tableList
		theDate
		(timeToTrain 100000)
		trainedToDate
		)

	(setq modelStructure (new Structure:
		; BaseFilters are the base expressions
		BaseFilters: #("top (Volume*Price) 400;" ) ; BaseExp
		Cabinet:		"test"	; AnalystName
		; CutFilters are the field expressions
		CutFilters:	#( 	"((Volume/SharesOutstanding)*(1/PEGLastQtrEPSPctChg))" 
						"((Volume/SharesOutstanding)*(1/PEGPrj5YearGrowth))" 
						"RelativePriceStrength" 
						"((Volume/SharesOutstanding)*(1/PEG))"
						"(max(High52Week,Price)/min(Low52Week,Price))" 
						"((Price-min(Low52Week,Price))/(max(High52Week,Price)-min(Low52Week,Price)))" 
					)	; FieldExp
		CoreExpCount: 4
		MaxDepth: 3
		Morph: true
		NumTopGridSelCore: 50
		NumTopGridSelSec: 50
		RankByWinPct: false
		TrainExpressions:	#("Next3MonthProfit") ; TrainExp
		TrainingBias: 91
		TrainingQtrs:	1		;
	))
	;; Setup MorphVectors
	;; TBD		

;	(setq tableList #{FASummary20010302: #("SummaryTablesFA\FASummary20010302.xls" FASummary20010302 #Mar,2,2001 ) FASummary20010601: #("SummaryTablesFA\FASummary20010601.xls" FASummary20010601 #Jun,1,2001 ) FASummary20010831: #("SummaryTablesFA\FASummary20010831.xls" FASummary20010831 #Aug,31,2001 ) FASummary20011130: #("SummaryTablesFA\FASummary20011130.xls" FASummary20011130 #Nov,30,2001 ) FASummary20020301: #("SummaryTablesFA\FASummary20020301.xls" FASummary20020301 #Mar,1,2002 ) FASummary20020531: #("SummaryTablesFA\FASummary20020531.xls" FASummary20020531 #May,31,2002 ) FASummary20020830: #("SummaryTablesFA\FASummary20020830.xls" FASummary20020830 #Aug,30,2002 ) FASummary20021129: #("SummaryTablesFA\FASummary20021129.xls" FASummary20021129 #Nov,29,2002 ) FASummary20030228: #("SummaryTablesFA\FASummary20030228.xls" FASummary20030228 #Feb,28,2003 )})
	(setq tableList #{FASummary20010302: #("SummaryTablesFA\FASummary20010302.xls" FASummary20010302 #Mar,2,2001 ) FASummary20010601: #("SummaryTablesFA\FASummary20010601.xls" FASummary20010601 #Jun,1,2001 ) FASummary20010831: #("SummaryTablesFA\FASummary20010831.xls" FASummary20010831 #Aug,31,2001 ) FASummary20011130: #("SummaryTablesFA\FASummary20011130.xls" FASummary20011130 #Nov,30,2001 ) FASummary20020301: #("SummaryTablesFA\FASummary20020301.xls" FASummary20020301 #Mar,1,2002 )})
	;(setq tableList ^deepGreen._summaryQuarterlyTables) ;; Note Dependency on deepGreen!!!!
	; tableList[n 0] -- contains name of dataMineLib table
	; tableList[n][0] -- contains name of .xls file
	; tableList[n][1] -- contains name of dataMineLib table
	; tableList[n][2] -- contains date of table

	(setq N (length tableList))
	(setq end (- N 1)) ; last period to process 
	(setq estimators (new Directory:))
	; Create and perform training of each estimator Lambda
	(loop for n from 0 until end do ; don't include the last entry in tableList
		; Get tableDate from tableList
		(setq tableDate tableList[n][2]); this is the date assoicated with the independent data values!

		; Calculate the date of the next quarter table entry
		; Note that we can not just add 91 days as the spread is sometimes 98 days. This means we always have to have
		; a table like the quarterly tables, create by deepGreen for instance, to get this future date.
		(setq trainedToDate tableList[(+ n 1)][2])

		; Check that we have at least 91 days spread between tables
		(if (< (- trainedToDate tableDate) 91) (begin
			(writeln "egm.selfTest error! less than 91 days between trainedToDate and tableDate")
			(return false)
			end))

		; Create estimator Lambda 
		(if (not (isMember trainedToDate estimators)) (begin ;; create Lambda, will always happen in this simple selfTest
	 		; Generate list of previous periods to pass as an argument when creating the egmInstance for this period.
			; These previous period dates are the "trainedToDates" used to index the estimator Lambdas.
			(setq j (max 1 (- n numPreviousPeriods))) 
			(setq k 0)
			(setq previousPeriods (new Vector:))
			(loop for i from j until n do 
				(setq previousPeriods[k] tableList[i][2])
				(setq k (+ k 1))
			);j
			(setq cursor (dataMineLib.open tableList[n 0] memory:)); Open the cursor for the tableDate
			(setq estimator (math.egm cursor modelStructure trainedToDate previousPeriods)); create the esitmator Lambda
			(setq cursor (dataMineLib.close cursor))
			(setq estimators[trainedToDate] estimator); save the estimator keyed by the trainedToDate
			(writeln "created estimators[" trainedToDate"]  trainedToDate: " estimator.TrainedToDate " tableDate: " estimator.TableDate " previous periods:" estimator.PreviousPeriods );
			end))

		(setq estimator estimators[trainedToDate])

		(writeln "training estimator Lambda, trainedToDate: " estimator.TrainedToDate " previousPeriods:" estimator.PreviousPeriods)
		;generate vector of estimator Lambda memories to pass to trainMachine using
		; the previousPeriods list stored in the current estimator Lambda's pvars
		(setq previousPeriodEstimators (new Vector: object:))
		(setq previousPeriods estimator.PreviousPeriods)
		(loop for i from 0 until (length previousPeriods) do
			(setq previousPeriodEstimators[i] (estimators[previousPeriods[i]].getMyPvars))
		);i

(writeln "estimator.CoreExpCount = " estimator.CoreExpCount)
;		(estimator.trainMachine timeToTrain Jitter: previousPeriodEstimators) ; Jitter
;		(estimator.trainMachine timeToTrain All: previousPeriodEstimators) ; All
;		(estimator.trainMachine timeToTrain Core: previousPeriodEstimators) ; Core
;		(estimator.trainMachine timeToTrain Secondary: previousPeriodEstimators) ; Secondary
		(estimator.trainMachine timeToTrain Evolve: previousPeriodEstimators) ; Secondary		
;		Disable the preceeding statements and enable the following loop to test the incremental training feature
;		vars:(t)
;		(loop for t from 0 until 5 do
;			(estimator.trainMachine .20 Jitter: previousPeriodEstimators)
;		)		
	);n

	; print out  gridSelectors
	(loop for n from 0 until (length estimators) do
		(setq estimator estimators[n 1])
		(writeln "Estimator for " estimators[n 0])
;		(writeln (_prettyPrintMemory #void (estimator.getMyPvars) 0))
		(loop for  y from 0 until estimator.NumY do 
			(loop for i from 0 until estimator.NumTopGridSelCore do
				(writeln "topGridSelCore["y"]["i"] " estimator.TopGridSelCore[y][i])
			);i
		);y
		(loop for  y from 0 until estimator.NumY do 
			(loop for i from 0 until estimator.NumTopGridSelSec do
				(writeln "topGridSelSec["y"]["i"] " estimator.TopGridSelSec[y][i])
			);i
		);y
	);n

	; perform ranking for each table in tableList from start to end
	(loop for n from 1 to end do ;don't include first entry, do include last entry in tableList!
		(writeln "Calling runMachine on table " tableList[n][1])

		(setq trainedToDate tableList[n][2])

		(setq cursor (dataMineLib.open tableList[n 0] memory:)); Open the cursor
		; Get the estimator Lambda to run against the current table
		(setq estimator estimators[trainedToDate])
		; generate a directory of additional trained estimator Lambdas to run against
		; the current cursor. The rankings of each estimator Lambda is 
		; averaged for a final ranking.
		(setq first (max 1 (- n backCount)))
		(setq backPeriodEstimators (new Directory:))
		(loop for j from first until n do
			(setq backPeriodEstimators[estimators[j 0]] (estimators[j 1].getMyPvars))
		);j
		(estimator.runMachine cursor backPeriodEstimators)

		(setq numFound 0)
		(setq R (length cursor.rowVector))
		(loop for r from 0 until R do
			(setq record (refObjVector cursor.rowVector r))
			(setq notes record.SpecialSituationNotes)
			(if (<> notes #void) (begin
				(writeln r " " notes)
				(setq numFound (+ numFound 1))
				))
		);n
		(writeln "Number of stocks ranked=" numFound)
		(setq cursor (dataMineLib.close cursor))
	);n
	
  	true)

(defun getTotalY(y)
	vars:(n total yOffset)
	(if (> y NumY) (begin
		(writeln "Error: math.egm.getTotalY - argument larger than number of Y in dataset")
		(return 0)
		))
	(setq yOffset (+ NumX y))
	(loop for n from 0 until NumRows do
		(setq total (+ total XY[n][yOffset]))
	);n
	total);getAllPerformance


;; Assign seleted pvars to the structure argument s. This routine allows the "data" portion
;; of the egm instance to be extracted for external store. Note that no Lambdas
;; are included in this copy.
(defun getMyPvars(...)
	vars:(p n N s)
	(setq p (myself))
	(setq p p.Pv)
	(setq N (length p))
	(if (= (argCount) 1)
		(setq s (argFetch 0))
		(setq s (new Structure:)))
	(loop for n from 0 until N do
		(if (not (isLambda p[n]))
			(setq s[p[n 0]] (copy p[n])))
	); n
	s) ;; end of getMyPvars

;; Assign selected items from the structure s into this Lambda's pvars. This routine allows the "data"
;; portion of an egm instance to be initialized from some external store. Note that this
;; copy is governed by the existance of the target Pv element in  this Lambda's pvars. Other values
;; stored in the structure argument s will not be copied into the  this Lambda's pvars structure.
;; Note: Lambdas will never be assigned into this Lambda's pvars by this routine.
(defun _setMyPvars(s)
	vars: (i n N p)
	(setq p (myself))
	(setq p p.Pv)
	(setq N (length p))
	(loop for n from 0 until N do
		(if (not (isLambda p[n])) (begin
			(setq i (member p[n 0] s))
			(if (isNumber i)
				(setq p[n 1] (copy s[i 1])))
			))
	)
	true)

;; Form 1 init of the egm. Note that this process is only called once to establish
;; a starting internal state for the egm.
(defun _init(cursor modelStructure trainedToDate previousPeriods)
	vars:(i cursorData)

	(setq Evolve 			modelStructure.Evolve)
(writeln "modelStructure.Evolve=" modelStructure.Evolve)
	(setq TrainingQtrs 	modelStructure.TrainingQtrs)
	(setq AnalystName 	modelStructure.Cabinet)
	(setq FieldExp 		modelStructure.CutFilters)
	(setq BaseExp 		modelStructure.BaseFilters)
	(setq CoreExpCount	modelStructure.CoreExpCount)
	(setq TrainExp 		modelStructure.TrainExpressions)
	(setq MaxDepth 		(integer modelStructure.MaxDepth))
	(setq Morph			modelStructure.Morph)
	(setq NumTopGridSelCore 	(integer modelStructure.NumTopGridSelCore))
	(setq NumTopGridSelSec (integer modelStructure.NumTopGridSelSec))
	(setq TrainingBias 	(integer modelStructure.TrainingBias))
	(setq RankByWinPct	modelStructure.RankByWinPct)
	(setq TrainedToDate	trainedToDate)
	(setq PreviousPeriods previousPeriods)
	
     (if (not (isVector BaseExp)) (setq BaseExp (new Vector: 1 BaseExp)))
	(setq NumBaseExp (integer (length BaseExp)))
	(setq NumFieldExp (integer (length FieldExp)))
	(setq NumTrainExp (integer (length TrainExp)))
	(setq AllExp (append FieldExp TrainExp))
	(setq NumAllExp (integer (length AllExp)))

	(if (not (isVector PreviousPeriods)) (setq PreviousPeriods (new Vector:)))

	(setq Genome (new Vector: short: 1 0)) ; First number is morphVector index, Second number is first number of column selection genome
	(setq NumGenome (integer (length Genome)))

	(setq cursorData (_inputCursor cursor)) ; crunch the table into a dense sigmod vector of vectors
	(setq TableDate cursor.bckVector[0].CurrentDate)	

	(setq XY cursorData.xy)
	(setq StockIDs cursorData.StockIDs)

	(setq NumRows (integer (length XY)))
	(setq NumX (integer NumFieldExp))
	(setq NumY (integer NumTrainExp))
	(setq NumCols (integer (+ NumX NumY)))
	(setq TopGridSelCore (new Vector: object: NumY))
	(setq TopGridSelSec (new Vector: object: NumY))

	(loop for i from 0 until NumY do (setq TopGridSelCore[i] (new Vector: object: NumTopGridSelCore)))
	(setq LowRanksIndexCore (new Vector: short: NumY 0))
	(setq LowRanksCore (new Vector: number: NumY BIGNEGNUM))

	(loop for i from 0 until NumY do (setq TopGridSelSec[i] (new Vector: object: NumTopGridSelSec)))
	(setq LowRanksIndexSec (new Vector: short: NumY 0))
	(setq LowRanksSec (new Vector: number: NumY BIGNEGNUM))

	(setq BucketSize 5) ; Fixed value

 	true); end init

 ;; Crunch cursor input into a dense sigmoid XY training vector array.
 ;; Note: The SpecialSituationNotes field of the cursor is used as a scratch pad. 
 ;; ** inputCursor uses many parent Lambdas pvars. These must be initialized
 ;;    before calling _inputCursor! See the _init Lambda. 
 (defun _inputCursor(cursor)
    vars:(k m n numRecords xy command record minX maxX rngX stockIDs
         temp rowXY)

	 ; Run base expressions
    (loop for k from 0 until NumBaseExp do (begin 
		(cursor.run BaseExp[k])
	))
    (setq numRecords cursor.recordCount)
    (setq xy (new Vector: object: numRecords)) ;; xy is local here, not the XY in the pvars
	(setq stockIDs (new Vector: short: numRecords))

	;; Construct the filter command that will update the SpecialSituationNotes field in the table with
	;; the calculated dependent and independent values in the xy
	(setq command (append "setnr SpecialSituationNotes new('Vector','number'," NumAllExp)) 
	(loop for m from 0 until NumAllExp do (setq command (append command "," AllExp[m])))
	(setq command (append command ");"))
	;; Extract raw data from cursor into XY vector array.
	(cursor.run command) ;; Fills the special situation notes with results of all expressions

	; extract special situation notes into XY array vector and find min, max and range in independent columns
    (setq minX (new Vector: number: numRecords BIGPOSNUM))  
    (setq maxX (new Vector: number: numRecords BIGNEGNUM))  
    (setq rngX (new Vector: number: numRecords))
    (loop for n from 0 until numRecords do
       (setq record (refObjVector cursor.rowVector n)) 
    (setq stockIDs[n] record.ID)
       (setq rowXY record.SpecialSituationNotes) ; assign local for speed
       (loop for m from 0 until NumFieldExp do ; for each field expression column, don't examine train expression columns
          (if (< (setq temp (refNumVector rowXY m)) (refNumVector minX m)) (setNumVector minX m temp))
          (if (> temp (refNumVector maxX m)) (setNumVector maxX m temp))
          (setNumVector rngX m (- (refNumVector maxX m) (refNumVector minX m)))
          ) ; end MX loop 
       (setObjVector xy n rowXY)
       (setq record.SpecialSituationNotes #void)
       ) ;n
    ; make a pass through XY making values sigmod for field expression columns (ie: independent variables)
	(loop for n from 0 until numRecords do
       (setq rowXY (refObjVector xy n)) ; assign local for speed
       (loop for m from 0 until NumFieldExp do
          (if (> (setq temp (refNumVector rngX m)) 0)
              (setNumVector rowXY m (/ (- (refNumVector rowXY m) (refNumVector minX m)) (refNumVector rngX m)))
              (setNumVector rowXY m 0.0)
              ) ; end if
          ); m
       );n
    (new Structure: xy: xy StockIDs: stockIDs)) ; end inputCursor

;; Clear the secondary genome population training results and
;; reset the SecondaryDone flag. Note that myTrainingMemory is passed
;; so that the latest field expressions are updated and that we 
;; recrunch the table data into the xy matrix.
(defun clearSecondary(cursor myTrainingMemory)
	vars:(i 
		cursorData
		modelStructure
		)
	(setq SecondaryDone false)
	(setq modelStructure myTrainingMemory.Models[0])
	(setq FieldExp 		modelStructure.CutFilters)
	(setq CoreExpCount	modelStructure.CoreExpCount)
	(setq NumTopGridSelSec (integer modelStructure.NumTopGridSelSec))

	(loop for i from 0 until NumY do (setq TopGridSelSec[i] (new Vector: object: NumTopGridSelSec)))
	(setq LowRanksIndexSec (new Vector: short: NumY 0))
	(setq LowRanksSec (new Vector: number: NumY BIGNEGNUM))	

	(setq NumBaseExp (integer (length BaseExp)))
	(setq NumFieldExp (integer (length FieldExp)))
	(setq NumTrainExp (integer (length TrainExp)))
	(setq AllExp (append FieldExp TrainExp))
	(setq NumAllExp (integer (length AllExp)))

;(writeln TrainExp)
	(setq cursorData (_inputCursor cursor)) ; crunch the table into a dense sigmod vector of vectors
										; This could be made more efficient by crunching only the columns
										; affected by the secondary genome population. 

	(setq TableDate cursor.bckVector[0].CurrentDate)	

	(setq XY cursorData.xy)
;(writeln "XY[0]" XY[0])
	(setq StockIDs cursorData.StockIDs)

	(setq NumRows (integer (length XY)))
	(setq NumX (integer NumFieldExp))
	(setq NumCols (integer (+ NumX NumY)))
	(setq Genome (new Vector: short: 1 0)) ; First number is morphVector index, Second number is first number of column selection genome
	(setq NumGenome (integer (length Genome)))


true)

; Run the machine on cursor
;	Args:
;		cusor			- table containing data to be ranked by the estimator Lambda. 
;							This is not the same as the estimator Lambda data. Instead,
;							it is a table of data having a table date greater than or
;							the same as the date of the estimator Lambda. Remember that
;							the estimator Lambda is identified by the date associated with
;							the dependent values the estimator Lambda trained on.
;		backCountPeriods - directory of zero or more previously trained estimator Lambda memories
; see the docmentation in the egm.cpp file for the runMachine C function
;
(defun runMachine(cursor ...)
    vars:(n N me estVector)
    ;; Collect arguments.
    (if (= (argCount) 2) (setq estVector (argFetch 1)) (setq estVector (new Directory:)))
	(setq N (length estVector))
	(display "runMachine estVector=")
	(loop for n from 0 until N do
		(display  estVector[n].TableDate " ")
	)
	(display _eol)
	(writeln "math.egm.runMachine tableDate:" (date cursor.bckVector[0].CurrentDate))
	(setq me (myself))
	(math_egm_RunMachine me cursor estVector)
	true) ; end runMachine


;; Train this grid machine for the time allowed by timeToRun
;	(egmInstance.trainMachine timeToTrain regressMemories)
;	Args:
;		timeToTrain		- the number of seconds the estimator should train
;		regressMemories	- a vector of zero or more previously trained estimator
;							Lambda memories. The dates of these previous periods
;							were specified in the previousPeriods argument to
;							math.egm. These dates can be retreived by the statement
;							(setq previousPeriods estimator.previousPeriods)
;
;trainMachine performs quasi-regression on the XY grid in the egmInstance and
;on the XY grids in egmInstances passed in the regressMemories vector.
; see the documentation in the egm.cpp file for the trainMachine C function.
(defun trainMachine(...)
     vars:(i n N me estMemories timeToRun procType)

	;; Get arguments
	(if (< (argCount) 2) (begin
		(writeln "Error! math.egm.trainMachine - too few arguments")
		(return false)))



	(setq timeToRun (argFetch 0))
	(setq procType (argFetch 1))
(writeln "procType = " procType)
	(cond 
		((= procType Jitter:) (setq procType 0))
		((= procType All:) (setq procType 1))
		((= procType Core:) (setq procType 2))
		((= procType Secondary:) (setq procType 3))
		((= procType Evolve:) (setq procType 4)) ;; Do all but skip Core if Core already done
	)

	; 3rd argument is optional vector of estimator Lambda memories used for time series based training
	(if (= (argCount) 3) 
		(setq estMemories (argFetch 2))
		(setq estMemories (new Vector: object: 0)))

	; Check to make sure none of the estimator Lambda memories passed have TrainedToDates less than
	; the TrainedToDate of this Lambda
	(loop for i from 0 until (length estMemories) do
		(if (not (< estMemories[i].TrainedToDate TrainedToDate)) (begin
			(writeln "egm.trainMachine Error! estimator Lambdas TrainedToDates out of range")
			(return false)
			))
	);i
	(display "math.egm.trainMachine estMemories:")
	(setq N (length estMemories))
	(loop for n from 0 until N do
		(writeln estMemories[n].TableDate)
	)
	(display _eol)
	(writeln "math.egm.trainMachine")
	(setq me (myself))
	(math_egm_TrainMachine me timeToRun procType (copy estMemories))
     true) ; end trainMachine

;; summerize
;; This Lambda will perform summary calculations on the pvars of an math.egm Lambda.
;; You can pass in the pvars structure or allow it to work on the current Lambdas
;; pvars by not passing an argument.
(defun summerize(...) 
	vars:(y i m j
		allStocks
		len
		NumTopGridSelCore
		NumTopGridSelSec
		numY
		result 
		stocks
		topGridSelCore
		topGridSelSec
		)
;(writeln "Sumerizing")

	(if (= (argCount) 1) ; argument is expected to be pvars of an egm Lambda
		(setq result (argFetch 0))
	else (begin ;; use the current Lambdas pvars
		(setq result (myself))
		(setq result result.Pv)
		))

	; Don't repeat summary if it has already been done and no calls to 
	; trainMachine have been performed since the last summarize call.
	(if (= result.Summerized true) (return result))

	; Assign local copies of pvars used in summary analysis
	(setq NumTopGridSelCore result.NumTopGridSelCore)
	(setq NumTopGridSelSec result.NumTopGridSelSec)
	(setq numY result.NumY)
	(setq topGridSelCore result.TopGridSelCore)
	(setq topGridSelSec result.TopGridSelSec)
		
	; Perform stock selection diversity analysis for each y
	(setq allStocks (new Vector: numY))
	(loop for y from 0 until numY do
		(setq allStocks[y] (new Directory:))
		(loop for i from 0 until NumTopGridSelCore do 
			;(writeln topGridSelCore[y][i])
			(setq stocks topGridSelCore[y][i].Stocks)
			(setq len (length stocks))
			(loop for j from 0 until len do
				(setq m (member stocks[j] allStocks[y]))
				(if (= m false) 
					(setq allStocks[y][stocks[j]] 1)
					(setq allStocks[y][m 1] (+ allStocks[y][m 1] 1)))
			);j 
		);i
		(loop for i from 0 until NumTopGridSelSec do 
			;(writeln topGridSelSec[y][i])
			(setq stocks topGridSelSec[y][i].Stocks)
			(setq len (length stocks))
			(loop for j from 0 until len do
				(setq m (member stocks[j] allStocks[y]))
				(if (= m false) 
					(setq allStocks[y][stocks[j]] 1)
					(setq allStocks[y][m 1] (+ allStocks[y][m 1] 1)))
			);j 
		);i
;		(writeln "For y=" y " StocksPicked=" (length allStocks[y]) " AllStocks=" allStocks[y])
	);y 
	(setq result.Summerized true)
	(setq result.AllStocks allStocks)
	result); end summerize

  	;; *******************************************************************************
  	;; Begin Main Logic 
  	;; *******************************************************************************
  	vars:(Egm memory)
  	(setq Egm (new (myself)))
	; Args
	;; Form 1: create a new egm instance with initial training information contained in modelStructure
	;; (setq estimator (math.egm cursor modelStructure trainedToDate previousPeriods))
	;; Form 2: create a new egm instance with previously saved egm state information
	;; (setq estimator (math.egm s)) ;; s is structure containing saved pvars from former instance of egm
	(cond
		((= (argCount) 1) (begin ; Form 2
			(setq memory (argFetch 0))
			(if (= memory #void) (return #void))
			(Egm._setMyPvars memory)
			))
		; Form 1 - (egm cursor modelStructure trainedToDate previousPeriods)
		((= (argCount) 4) (Egm._init (argFetch 0) (argFetch 1) (argFetch 2) (argFetch 3)))
		(else 
			(begin
			(writeln "Error: egm received wrong number of arguments")
			(return #void)
		))
	); end cond


  Egm) ; end egm







;;**EXPORTKEY**:egm:%%Document_Egm_Ref_Guide
;#text#
<html><head></head><body>


<?xml version="1.0" encoding="UTF-8"?>
<Document>
	<KnowledgeBase>
	    <Title>EGM Reference Guide</Title>
		<Topic>EGM</Topic>
		<SubTopic>Reference Guide</SubTopic>
		<HumanKeywords>EGM Lambdas Machine-Learning  Genetic-Programming Artificial-Intelligence</HumanKeywords>
	</KnowledgeBase>
	<Essay>Essay_EGM_Introduction</Essay>
 
</Document>
 
</body></html>


;;**EXPORTKEY**:egm:%%Essay_EGM_Introduction
;#text#
<html><head></head><body>


<?xml version="1.0" encoding="UTF-8"?>
<Essay>
	<KnowledgeBase>
	    <Title>EGM Introduction</Title>
		<Topic>EGM</Topic>
		<SubTopic>Overview</SubTopic>
		<HumanKeywords>EGM Machine-Learning Genetic-Programming Artificial-Intelligence</HumanKeywords>
	</KnowledgeBase>
	<Section>
	    <Heading>Overview</Heading>
		<Description>	
             <p>The Evolutionary Grid Machine Lambda (EGM) is a learning machine which learns to
             select and score the best individuals from a universe of individuals over time. Over  
             a series of discrete time steps, a universe of individuals is collected for each time
             step. The individuals are things such as Stocks, People, Cities, etc. The discrete time
             steps are weeks, days, seconds, years, microseconds, etc.</p>
         
             <p>Each individual followed by the system is given a unique identifier which remains
             unique across all time periods studied (no two individuals ever have the same identifier).
             Furthermore, each time period studied is given a unique ascending integer index (i.e. week 1,
             week 2, etc.). So, for a series of time periods, historical information about groups of
             individuals is collected for each time period. The historical information collected for each
             individual for each time period is stored in a Number Vector and includes: the time period index;
             the unique identifier of the individual; and other numeric information about the individual
             pertinent to the current investigation. Finally, each individual in each time period is given
             a numeric "score" which determines the value of the individual in that time period. The "best"
             individuals have the highest "score" values.</p>

             <p>During training, the EGM is given historical information for time periods 0 through T for
             all individuals. The EGM is also given the "score" values for each individual in each training
             time period from 0 through T. During training the EGM attempts to "learn" any patterns in 
             the available historical data. The machine (EGM) is free to discover static as well as time
             varying patterns.</p>

             <p>During forward prediction, the EGM is given new information for time period T+1 for
             all individuals. The EGM is <b>NOT</b> given the "score" values for each individual in the new
             time period T+1. During prediction the EGM attempts to use any patterns it has learned to 
             select and score the best individuals from the universe of individuals seen in time period T+1. The 
             machine (EGM) is free to select no individuals in time period T+1 (an "I am uncertain" response). 
             Once the machine selects and scores a set of individuals, the accuracy of the machine is determined by
             least squares error on the selected individuals, averaging the actual "score" values for the selected
             individuals (<i>which the machine has never seen</i>) with the average "score" for all individuals 
             in time period T+1, or by other appropriate methods.</p>
	    </Description>
	</Section>
	<Section>
	    <Heading>Summary</Heading>
		<Description>	
             <p>A time series set of vectors, X, together with a set, Y, of scores for the vectors
             in X are used to train a learning machine. There is also a testing set, TX and TY, of 
             of vectors similar to those in X and Y but for the testing time period (a time period
             not present in X or Y). After training, the machine is presented with the testing set,
             TX and attempts to estimate TY. The learning machine returns a Vector EY, of estimates
             for TY. Each element in EY is an estimate for the corresponding value in TY which is 
             either void (an "I am uncertain" response) or contains a numeric estimate for the
             corresponding value in TY. All non-void elements in TY are said to be "selected". The 
             learning machine attempts to select the "best" individuals (with above average "scores" 
             in the testing time period).</p>

             <p>The <i>selection</i> mission of the Evolutionary Grid Machine is an important
             distinguishing feature between these learning machines and regressions learning
             machines. The EGM is <b>NOT</b> trying to fit a function to the testing data.
             Instead the EGM is trying to <i>select</i> out the "best" individuals from the 
             testing data, scoring only within the selected "best" individuals.</p>

             <p>In many instances the Evolutionary Grid Machine may not have an exact estimate
             for the scores of the <i>best</i> individuals in the testing data. However, if the
             learning machine is able to select the <i>best</i> individuals out of the testing
             data, then the machine has been successful even if its estimated score is incorrect.
             Furthermore, if the machine's estimated score is incorrect; but, preserves the ordering
             of the corresponding actual scores, then the machine has achieved an added layer of success.</p>
	    </Description>
	</Section>
	<Section>
	    <Heading>Description</Heading>
		<Description>	
             <p>Let X be a set of vectors such as the vector x = {e[0] e[1] e[2] ... e[M]},
             where each e[m] is an independent element and let Y be a vector of dependent
             "score" values. Let both X and Y be of length N.</p>

             <p>Furthermore, let the zeroth element, e[0], of every vector, in X, contain a 
             non-negative integer value indicating some time span of integral length, for
             example, if the time span were weeks, a value of 1 would indicate week one, 
             and a value of 10 would indicate week ten, etc. (i.e. the vectors contained
             in X are time sequenced).</p>

             <p>Furthermore, let the first element, e[1], of every vector, in X, contain an 
             integer entity identifier value indicating some unique entity. Thus two vectors
             x[i] and x[j] having the same entity identifier (x[i].e[1] = x[j].e[1]), are
             said to refer to the same abstract entity in some unspecified manner.</p>

             <p>Furthermore, for any two distinct vectors, in X, the following condition
                ((x[i].e[0] == x[j].e[0]) && (x[i].e[1] == x[j].e[1])) 
             is an error may never be true (i.e. two vectors cannot exist for the same entity
             and the same time span).</p>

             <p>Finally, any unique entity value may be missing entirely from all vectors in X,
             or present for some integral time values but missing for other integral time values.</p>
	    </Description>
	</Section>
	<Section>
	    <Heading>Example</Heading>
		<Description>	
             <p>An example, but by no means the only example, of X and Y would be a set of vectors
             of stock market data taken from the Open High Low Close and Volume numbers for all
             NASDQ traded stocks over a 52 week period. In this example we have the following:</p>

		    <table border="3" cellpadding="2" width="100%" bgcolor="#99CCCC">
		      <tr align="top"><th>e[0]</th> <td>The sequential index of the current week (from 1 through 52).</td><tr>
		      <tr align="top"><th>e[1]</th> <td>The unique integer identifier of the stock (stocks not traded would not appear).</td></tr>
		      <tr align="top"><th>e[2]</th> <td>The current week opening price.</td></tr>
		      <tr align="top"><th>e[3]</th> <td>The current week high price.</td></tr>
		      <tr align="top"><th>e[4]</th> <td>The current week low price.</td></tr>
		      <tr align="top"><th>e[5]</th> <td>The current week closing price.</td></tr>
		      <tr align="top"><th>e[6]</th> <td>The current week share volume traded.</td></tr>
		      <tr align="top"><th>Y</th> <td>The "score" vector of next week profits (next_week_closing_price - the_current_week_closing_price).</td></tr>
            </table>

            <p>Similar examples can be constructed for oil exploration data over time, for
            the height and weight of individuals over time, etc. However, continuing with our
            securities example, we train our machine on the market data for four stocks
            over a four week period as follows:</p>

            <p><b>Training Data</b></p>

		    <table border="3" cellpadding="2" width="100%" bgcolor="#99CCCC">
		      <tr align="top"><th><i>Time</i></th><th><i>ID</i></th><th><i>Open</i></th><th><i>High</i></th><th><i>Low</i></th><th><i>Close</i></th><th><i>Vol</i></th><th><i>Score</i></th><tr>
		      <tr align="top"><th>x.e[0]</th><th>x.e[1]</th><th>x.e[2]</th><th>x.e[3]</th><th>x.e[4]</th><th>x.e[5]</th><th>x.e[6]</th><th>y</th><tr>
		      <tr align="top"><th></th><th></th><th></th><th><i>(first week)</i></th><th></th><th></th><th></th><th>Note: (<i>next week's profit</i>)</th><tr>
		      <tr align="top"><td align="center">0</td><td align="center">1 <i>(Apple)</i></td><td align="center">$23.45</td><td align="center">$25.67</td><td align="center">$23.35</td><td align="center">$24.56</td><td align="center">19367</td><td align="center">3.4%</td><tr>
		      <tr align="top"><td align="center">0</td><td align="center">2 <i>(IBM)</i></td><td align="center">$143.45</td><td align="center">$145.27</td><td align="center">$143.15</td><td align="center">$144.96</td><td align="center">894676</td><td align="center">-1.2%</td><tr>
		      <tr align="top"><td align="center">0</td><td align="center">3 <i>(Xerox)</i></td><td align="center">$13.95</td><td align="center">$15.27</td><td align="center">$13.35</td><td align="center">$14.72</td><td align="center">56832</td><td align="center">4.8%</td><tr>
		      <tr align="top"><td align="center">0</td><td align="center">4 <i>(GM)</i></td><td align="center">$57.15</td><td align="center">$62.17</td><td align="center">$53.65</td><td align="center">$62.05</td><td align="center">3419647</td><td align="center">9.1%</td><tr>
		      <tr align="top"><th></th><th></th><th></th><th><i>(second week)</i></th><th></th><th></th><th></th><th>Note: (<i>next week's profit</i>)</th><tr>
		      <tr align="top"><td align="center">1</td><td align="center">1 <i>(Apple)</i></td><td align="center">$24.56</td><td align="center">$25.38</td><td align="center">$22.75</td><td align="center">$25.40</td><td align="center">12046</td><td align="center">1.2%</td><tr>
		      <tr align="top"><td align="center">1</td><td align="center">2 <i>(IBM)</i></td><td align="center">$144.96</td><td align="center">$144.96</td><td align="center">$143.15</td><td align="center">$143.23</td><td align="center">864023</td><td align="center">-3.2%</td><tr>
		      <tr align="top"><td align="center">1</td><td align="center">3 <i>(Xerox)</i></td><td align="center">$14.72</td><td align="center">$16.12</td><td align="center">$14.39</td><td align="center">$15.43</td><td align="center">59204</td><td align="center">3.4%</td><tr>
		      <tr align="top"><td align="center">1</td><td align="center">4 <i>(GM)</i></td><td align="center">$62.05</td><td align="center">$62.05</td><td align="center">$68.00</td><td align="center">$67.70</td><td align="center">3219382</td><td align="center">6.5%</td><tr>
		      <tr align="top"><th></th><th></th><th></th><th><i>(third week)</i></th><th></th><th></th><th></th><th>Note: (<i>next week's profit</i>)</th><tr>
		      <tr align="top"><td align="center">2</td><td align="center">1 <i>(Apple)</i></td><td align="center">$25.40</td><td align="center">$26.98</td><td align="center">$24.75</td><td align="center">$25.71</td><td align="center">22056</td><td align="center">0.8%</td><tr>
		      <tr align="top"><td align="center">2</td><td align="center">2 <i>(IBM)</i></td><td align="center">$143.23</td><td align="center">$143.23</td><td align="center">$136.75</td><td align="center">$138.64</td><td align="center">824093</td><td align="center">-4.3%</td><tr>
		      <tr align="top"><td align="center">2</td><td align="center">3 <i>(Xerox)</i></td><td align="center">$15.43</td><td align="center">$16.45</td><td align="center">$15.09</td><td align="center">$15.96</td><td align="center">61205</td><td align="center">-1.4%</td><tr>
		      <tr align="top"><td align="center">2</td><td align="center">4 <i>(GM)</i></td><td align="center">$67.70</td><td align="center">$75.35</td><td align="center">$66.39</td><td align="center">$72.10</td><td align="center">3619582</td><td align="center">7.8%</td><tr>
            </table>

            <p>We train the EGM on the training data shown above. After training, we
            show the machine the following testing data, TX, and ask it to return an estimate
            of the next week's profit, TY, for each of the four individuals. <u>We do not show the machine the scores, TY.</u></p>

            <p><b>Testing Data</b></p>

		    <table border="3" cellpadding="2" width="100%" bgcolor="#99CCCC">
		      <tr align="top"><th><i>Time</i></th><th><i>ID</i></th><th><i>Open</i></th><th><i>High</i></th><th><i>Low</i></th><th><i>Close</i></th><th><i>Vol</i></th><th><i>Score</i></th><tr>
		      <tr align="top"><th>tx.e[0]</th><th>tx.e[1]</th><th>tx.e[2]</th><th>tx.e[3]</th><th>tx.e[4]</th><th>tx.e[5]</th><th>tx.e[6]</th><th>ty</th><tr>
		      <tr align="top"><th></th><th></th><th></th><th><i>(fourth week)</i></th><th></th><th></th><th></th><th>Note: (<i>next week's profit</i>)</th><tr>
		      <tr align="top"><td align="center">3</td><td align="center">1 <i>(Apple)</i></td><td align="center">$25.71</td><td align="center">$26.18</td><td align="center">$25.55</td><td align="center">$25.92</td><td align="center">25046</td><td align="center">-1.2%</td><tr>
		      <tr align="top"><td align="center">3</td><td align="center">2 <i>(IBM)</i></td><td align="center">$138.64</td><td align="center">$139.23</td><td align="center">$131.15</td><td align="center">$132.67</td><td align="center">774593</td><td align="center">-6.1%</td><tr>
		      <tr align="top"><td align="center">3</td><td align="center">3 <i>(Xerox)</i></td><td align="center">$15.96</td><td align="center">$16.13</td><td align="center">$15.00</td><td align="center">$15.73</td><td align="center">59205</td><td align="center">2.4%</td><tr>
		      <tr align="top"><td align="center">3</td><td align="center">4 <i>(GM)</i></td><td align="center">$72.10</td><td align="center">$77.87</td><td align="center">$71.39</td><td align="center">$77.73</td><td align="center">3710582</td><td align="center">5.8%</td><tr>
            </table>

            <p><b>Resulting Estimates</b></p>

            <p>After testing, the learning machine returns the following estimated scores, EY.</u></p>

		    <table border="3" cellpadding="2" width="100%" bgcolor="#99CCCC">
		      <tr align="top"><th><i>Estimate</i></th><th><i>Score</i></th><tr>
		      <tr align="top"><th>ey</th><th>ty</th><tr>
		      <tr align="top"><td align="center">void</td><td align="center">-1.2%</td><tr>
		      <tr align="top"><td align="center">-7.6%</td><td align="center">-6.1%</td><tr>
		      <tr align="top"><td align="center">1.9%</td><td align="center">2.4%</td><tr>
		      <tr align="top"><td align="center">4.9%</td><td align="center">5.8%</td><tr>
            </table>

            <p>Ignoring the <b>void</b> element, we calculate the least squares error on the three "selected" individuals as 1.10% 
           and we can also score these estimates in an alternate manner. We can sort these estimates, EY, and show that a sorting
           of the EY estimates also preserves the sort order of their corresponding TY values.</p>

	    </Description>
	</Section>
	<Section>
	    <Heading>FAQ</Heading>
		<Description>	
             <p><font color=blue><b>Question 1:</b></font> Why must we train the learning machine on collections of individuals in each time period? 
             Why not simply train the machine on each individual separately and perform a normal regression estimate for each individual?</p>

             <p><font color=blue><b>Answer:</b></font> Many <i>real world</i> estimates cannot be made unless one is aware of the competitive land scape.</p>
             <p>For instance, suppose one is estimating the <i>social popularity</i> of students in the senior high school class. We can perform any
             number of individual regressions correlating high scores for <i>intelligence</i>, <i>appearance</i>, and <i>social skills</i>
             with <i>social popularity</i>. However, all of these individual regression models are greatly skewed in the case where all
             students in the senior high school class are male except one student who is female.</p>

             <p>Also, suppose one is estimating the <i>financial popularity</i> of our Bank's Certificates of Deposit. We perform any
             number of individual regressions correlating our Bank's previous Certificates of Deposit with their <i>financial popularity</i>.
             However, all of these individual regression models are greatly skewed in the case where one of our competitors is advertising
             an aggressive interest rate two percentage points higher than ours.</p>

             <p><font color=blue><b>Question 2:</b></font> Why must we allow the learning machine to select individuals in the testing time period? 
             Why not simply have the machine provide normal regression estimates for each individual in the testing time period?</p>

             <p><font color=blue><b>Answer:</b></font> Many <i>real world</i> estimates cannot be made for all individuals; but, only for a few individuals.</p>
             <p>For instance, suppose one is estimating currency <i>conversion rates</i>. Normally these rates have very small random daily changes.
             However, every so often, a central bank will pre-announce its intention to buy or sell its own currency. In those special cases the 
             learning machine will want to have "no oppinion" on most currencies; yet, make an estimate for the currency whose central bank has pre-announced.</p>

	    </Description>
	</Section>
</Essay>
 
</body></html>


;;**EXPORTKEY**:egm:%%Essay_EGM_IntroductionOld
;#text#
<html><head></head><body>


<?xml version="1.0" encoding="UTF-8"?>
<Essay>
	<KnowledgeBase>
	    <Title>EGM Introduction</Title>
		<Topic>EGM</Topic>
		<SubTopic>Overview</SubTopic>
		<HumanKeywords>EGM Machine-Learning Genetic-Programming Artificial-Intelligence</HumanKeywords>
	</KnowledgeBase>
	<Section>
	    <Heading>Introduction</Heading>
		<Description>	
             <div class="h2section" id="Introduction">	
		    <p>JavaScript was developed by Netscape as a means to bring the simplicity
		    of scripting languages to a wider audience. The main difference between Java
		    and JavaScript, is that Java code is compiled into &quot;applets&quot; that
		    are distinct from the HTML page and whose code cannot be viewed directly,
		    whereas JavaScript code is included directly in the HTML document and can be
		    easily modified.</p>
		    <p>JavaScript is a scripting language designed to extend the functionality
		    of web pages. Typically written as part of an HTML document, a JavaScript
		    controls the elements of the page and reacts to user actions.</p>
		    <p>The generic JavaScript language is distinguished from the <b>javaScript</b>
		    language by an uppercase letter &quot;J&quot;. The<b> javaScript </b>language<b>
		    </b>is<b> </b>a proprietary javaScript compiler generated by <b>ParseLib</b>.
		    The <b>javaScript</b> compiler Lambda converts javaScript source code into an
		    Analytic Information Server parse tree which is the input to the <b>morph</b>
		    and <b>compile</b> functions. The <b>compile</b> function finally produces
		    an Lambda ready execute.</p>
		    <p>Thus, the <b>javaScript </b>language<b> </b>is an easy to use alternative
		    tool that web programmers may find more easy to use than Lisp. The
		    javaScript language extends the power of Lambda Oriented programming to a
		    wider audience.</p>
		    <p>javaScript includes a number of language commands for creating datamine
		    tables, filtering tables, and scoring tables. It was developed, for the
		    dataMineLib, as a means to bring the full power and simplicity of
		    scripting languages to the data mining process.</p>
		    <p>Thus, the <b>Filter Extensions</b> is an easy to use alternative tool
		    that dataMineLib programmers may find more easy to use than the data mine
		    Lambda?s API. These language extensions bring the power of scripting
		    languages to the data mining process.</p>
             </div>
	    </Description>
	</Section>
	<Section>
	    <Heading>Invoking javaScript from Lisp</Heading>
		<Description>
             <div class="h2section" id="Invoking javaScript from Lisp">
		    <p>The <b>javaScript</b> compiler Lambda can be called from the Lisp parser, <b>lisp</b>,
		    by using the <b>#javaScript#</b> compiler directive, or it can be called
		    directly. If called directly, it should be followed by morph and compile
		    (see the lisp parser - javaScript is a replacement for the lisp parser). The
		    <b>javaScript</b> Lambda was generated by the ParseLib Lambda.</p>
		    <b>Note</b>: The javaScript Lambda must be loaded into the workspace prior to
		    using the javaScript language. (see the javaScript library and the Dynamic
		    HTML example).
            </div>
	    </Description>
	</Section>
	<Section>
	    <Heading>Invoking javaScript from the Console</Heading>
		<Description>
            <div class="h2section" id="Invoking javaScript from the Console">
			<p>The LambdaClient IDE contains a Console tab. Clicking the right mouse
		    button, and selecting the &quot;Set Prefix&quot; option displays a modal
		    dialog containing an edit box for the user to enter a Console prefix. The
		    Console prefix controls which parser is invoked when the console receives
		    any text to be evaluated. The valid prefixes are: &quot;&quot; (a null
		    string), &quot;<b>#lisp#&quot;,</b> and <b>&quot;#javaScript#&quot;</b>. If
		    the Console prefix is a null string or <b>#lisp#,</b> then any expressions
		    submitted for evaluation will be parsed with the Lisp parser. If the Console
		    prefix is <b>&quot;#javaScript#&quot;</b>, the JavaScript parser will be
		    invoked.</p>
            </div>
	    </Description>
	</Section>
	<Section>
	    <Heading>Lambda Objects</Heading>
		<Description>
           <div class="h2section" id="Lambda Objects;">
			<p>The LambdaClient development uses an Lambda-oriented metaphor. <b>javaScript</b>
		    &quot;Lambdas&quot; are a unique class of objects designed to act as the <b><u>building
		    blocks</u></b><u></u> for intelligent, adaptive, systems. Lambdas contain
		    more than just binary machine code (Analytic Information Server supports many
		    built-in functions, which are primarily binary machine code, but these are
		    not Lambdas). Lambdas are something more than just functions. Lambdas are
		    building block objects, which contain the necessary structure to provide
		    some rudimentary autonomy. Lambdas can contain other child Lambdas and can
		    give birth to other child Lambdas. Lambdas can publish their preferred style
		    of interface. Lambdas have an abstract threshold (like a cell membrane) which
		    makes the Lambda aware of any mutative or referential access attempt from the
		    outside. Lambdas may run on native machine code or they may be emulated by a
		    virtual machine. There may be a different virtual machine for each Lambda.
		    Lambdas contain their persistent and temporary knowledge variables. Lambdas
		    contain the original source code used to compile them. Lambdas can be
		    generated from multiple languages. Analytic Information Server comes with a
		    built-in Lisp compiler, which produces Lambdas.</p>
           </div>
	    </Description>
	</Section>
	<Section>
	    <Heading>Lambda Properties</Heading>
		<Description>
            <div class="h2section" id="Lambda Properties">
			<p>The Lambda object stores Lambda behavior and knowledge in a standard
		    building block format (regardless of the original source language). The
		    Analytic Information Server Lambda object contains the following eight
		    properties:</p>
			
		    <table class=members_table border="3" cellpadding="2" >
			<colgroup><col class="member"><col class="description"></colgroup>
		      <tr>
		        <td>Av:</td>
		        <td>
		          <p>The arguments Structure object containing the Lambda's arguments.<br><br></p>
		        </td>
		      </tr>
		      <tr">
		        <td>In:</td>
		        <td>
		          <p>The <b>faces</b>: Structure object containing the Lambda's published
		          interface styles.</p>
		        </td>
		      </tr>
		      <tr>
		        <td>Pc:</td>
		        <td>
		          <p>The Pcode Vector object containing the Lambda's virtual machine
		          codes.</p>
		        </td>
		      </tr>
		      <tr>
		        <td>Pv:</td>
		        <td>
		          <p>The <b>pvars</b>: Structure object containing the Lambda's
		          persistent variables.</p>
		        </td>
		      </tr>
		      <tr>
		        <td>Cv:</td>
		        <td>
		          <p>The <b>cvars</b>: Structure object containing the Lambda's
		          persistent class variables</p>
		        </td>
		      </tr>
		      <tr>
		        <td>Nc:</td>
		        <td>
		          <p>The Native Code Vector object containing the Lambda's native machine
		          code.</p>
		        </td>
		      </tr>
		      <tr>
		        <td>Sc:</td>
		        <td>
		          <p>The Source Code Vector containing the original language source for
		          debugger display.</p>
		        </td>
		      </tr>
		      <tr>
		        <td>Tv:</td>
		        <td>
		          <p>The <b>vars</b>: Structure object containing the Lambda's temporary
		          frame variables.</p>
		        </td>
		      </tr>
		      <tr>
		        <td>Vm:</td>
		        <td>
		          <p>The Virtual Machine emulator function (each Lambda may run on a
		          separate virtual machine).</p>
		        </td>
		      </tr>
		    </table>
		    <p>&nbsp;</p>
		    <p>An Lambda is <i>First Class Object</i>. A First Class object in Lambda
		    Information Server is any object that is fully exposed, i.e., all of the
		    Structures are visible and modifiable by the programmer. All Lambdas have the
		    following data structures: source code tokens (<b>Sc</b>), pcode tokens (<b>Pc</b>),
		    argument variables (<b>Av</b>), persistent variables (<b>Pv</b>)<b>, </b>persistent
		    class variables (<b>Cv</b>)<b>, </b>temporary variables (<b>Tv</b>),
		    interfaces (<b>In</b>), native code (<b>Nc</b>), and the virtual machine
		    emulator (<b>Vm</b>). All Lambda structures can viewed and modified by the
		    programmer:</p>
             </div>
	    </Description>
	</Section>
	<Section>
	    <Heading>javaScript Filter Lambdas</Heading>
		<Description>
            <div class="h2section" id="JavaScript Filter Lambdas">
			<p>The principal activity of the datamine is to reduce tables to smaller
		    tables which have a higher <i>score</i> than the original table. This
		    process is called filtering. For instance, in the stock market, we start
		    with a table of all possible stocks and we wish to select a few stocks to
		    purchase. If the few stocks we purchase have a higher <i>score</i> (percent
		    profit) than the average of all stocks, then we are happy. The act of
		    selecting a few stocks to purchase reduces the original table of all stocks
		    down to the table of those we wish to purchase. This process is called
		    filtering.</p>
		    <p>Other examples of filtering include reviewing a table of all United
		    States households to select only those households which are to receive this
		    month's promotional mailing. Reviewing a table of possible oil deposit sites
		    to select only those sites where we wish to drill. There are many other
		    examples of filtering.</p>
		    <p>The <i>score</i> of a table is determined by the Score Lambda (see the <b>Score
		    Lambdas</b> section next).</p>
		    <p>All datamine table filtering is performed by Filter Lambdas, which are
		    entered by the user in the simple, easy to learn datamine javaFilter
		    language. An example might be:</p>
		    <blockquote>
		      <pre>filter top LastMonthProfit 10%;</pre>
		    </blockquote>
		    <p>or</p>
		    <blockquote>
		      <pre>filter all Timeliness = 1;</pre>
		    </blockquote>
           </div>
	    </Description>
	</Section>
	<Section>
	    <Heading>javaScript Score Lambdas</Heading>
		<Description>
             <div class="h2section" id="javaScript Score Lambdas">
			<p>If the principal activity of the datamine is to reduce tables to smaller
		    tables which have a higher <i>score</i> than the original table, then there
		    must be some means of determining a table?s score. All datamine table
		    scoring is performed by Score Lambdas, which are entered by the user in the
		    simple, easy to learn datamine javaFilter language. An example might be:</p>
		    <blockquote>
		      <pre>score avg NextMonthProfit;</pre>
		    </blockquote>
		    <p>or</p>
		    <blockquote>
		      <pre>score sum ItemPrice * TotalItems;</pre>
		    </blockquote>
           </div>
	    </Description>
	</Section>
	<Section>
	    <Heading>JavaScript Data Types</Heading>
		<Description>
            <div class="h2section" id="JavaScript Data Types">
		   <p>The <b>javaScript</b> Programmer has access to all of the types in the
		    Analytic Information Server environment. These are the same as the Lisp data
		    types. The Lisp data types are divided into three categories: Native Data
		    Types (also known as Immediate types), Objects (heap objects) and
		    Repositories. The Native (immediate) types can be entirely contained within
		    the immediate data of a single Virtual Machine Container. The Objects (heap
		    objects) types are too large to be contained within a single Virtual Machine
		    Container and require extra memory must be managed by the heap manager.
		    Without exception, all of the Object types are identified by an object id.
		    The object id identifies a block of memory, managed by the Analytic Information
		    Server memory manager, in which the Object's data is stored. (see Object
		    Identifier Notation).</p>
		    <p>Virtual Machine Containers are of fixed length and come in different
		    sizes. Small data items are stored in immediate mode, and may be moved to
		    the heap if the data becomes too large to store immediately.</p>
		    <p>The Heap contains memory resident data, which is of variable length or is
		    too large to fit in small fixed containers. The Analytic Information Server
		    object Heap manager supports automated object resizing, garbage collection,
		    and anti-fragmentation algorithms so that the user may concentrate on the
		    analysis and modeling of data rather than on memory management.</p>
		    <p>Repositories (databases) contain persistent data of all sorts. Lambda
		    Information Server supports repositories with multiple database volumes and
		    multiple database schema's including General Object Repositories, Text
		    Repositories, and Lambda Repositories.</p>
		    <p>The generic Analytic Information Server data type is known to javaScript as <b>obj</b>.
		    No type identification, such as <i>var n;</i>, will cause javaScript to
		    treat the variable, <i>n</i>, as being of type <b>obj</b>, that is to say
		    any possible Analytic Information Server data type.</p>
		    <p>The javaScript compiler also supports strong typing of declared variables
		    <b>obj</b>. Providing a type identification, such as <i>var int n;</i>, will
		    cause javaScript to treat the variable, <i>n</i>, as being of type <b>int</b>,
		    that is to say it will be managed as an Analytic Information Server type <b>Integer</b>.</p>
		    <p>The following is a list of javaScript strong data types together with the
		    Analytic Information Server types which they represent.</p>
		    <table border="3" cellpadding="2" width="100%" bgcolor="#99CCCC">
			<colgroup><col><col class="italic"><col><col class="italic"></colgroup>
		      <tr align="top">
		        <th>obj</th>
		        <td>Object</td>
		        <th>bool</th>
		        <td>Boolean</td>
		        <th>char</th>
		        <td>Character</td>
		        <th>int</th>
		        <td>Integer</td>
		        <th>float</th>
		        <td>Number</td>
		      </tr>
		      <tr align="top">
		        <th>text</th>
		        <td>Text</td>
		        <th>string</th>
		        <td>String</td>
		        <th>symbol</th>
		        <td>Symbol</td>
		        <th>bytvec</th>
		        <td>ByteVector</td>
		        <th>fltvec</th>
		        <td>FloatVector</td>
		      </tr>
		      <tr align="top">
		        <th>stc</th>
		        <td>Structure</td>
		        <th>dir</th>
		        <td>Directory</td>
		        <th>dic</th>
		        <td>Dictionary</td>
		        <th>matrix</th>
		        <td>Matrix</td>
		        <th>nummat</th>
		        <td>NumMatrix</td>
		      </tr>
		      <tr align="top">
		        <th>vec</th>
		        <td>Vector</td>
		        <th>bitvec</th>
		        <td>BitVector</td>
		        <th>numvec</th>
		        <td>NumVector</td>
		        <th>intvec</th>
		        <td>IntVector</td>
		        <th>objvec</th>
		        <td>ObjVector</td>
		      </tr>
		    </table>
         </div>
	    </Description>
	</Section>
	<Section>
	    <Heading>Strong Typing</Heading>
		<Description>
            <div class="h2section" id="Strong Typing">
			<p>The <b>javaScript</b> Programmer has access to compile time strong typing
		    variable declarations. Strongly typed variables are compiled with Lambda
		    Information Server's strong typed virtual machine instructions. Strongly
		    typed variables operate faster, at run time; but, are more prone to
		    programmer error as there is little or no run time type checking performed.</p>
		    <p>The programmer can even cast an arbitrary javaScript expression to a
		    valid type. The casting will alert the javaScript compiler to treat the
		    result of the cast expression as specified. This will direct the javaScript
		    compiler to use Analytic Information Server's strong typed virtual machine
		    instructions with the cast expression. Warning: casting does not introduce
		    any run time type checking.</p>
		    <p>The following javaScript code sample illustrates the actions of the
		    javaScript compiler when strong typing variable declarations and type casts
		    are encounteres.</p>
		    <p><b>The javaScript source code for foo</b></p>
		    <blockquote>
			  <pre>
		      // A test of strong typing, including expression type casting, in javaScript.
		      function foo(int i) {
		      var char c1, string name=new('String',&quot;Hello There&quot;);
		      c1 +=name[((int)length(name))-i];
		      }</pre>
		    </blockquote>
		    <p><b>The compiled code for foo</b></p>
		    <table border="3" cellpadding="2">
		      <tr>
		        <td><u>Virtual Machine Instructions for</u>: <b>foo</b></td>
		      </tr>
		      <tr>
		        <td>0000: push &quot;String,&quot;Hello There&quot;</td>
		      </tr>
		      <tr>
		        <td>0007: call 2,new,vars:(name)</td>
		      </tr>
		      <tr>
		        <td>0011: push vars:(name)</td>
		      </tr>
		      <tr>
		        <td>0013: call 1,length,vars:(__T4)</td>
		      </tr>
		      <tr>
		        <td>0017: isub args:(i),vars:(__T4),vars:(__T3)</td>
		      </tr>
		      <tr>
		        <td>0021: refstring vars:(__T3),vars:(name),vars:(__T2)</td>
		      </tr>
		      <tr>
		        <td>0025: cadd vars:(__T2),vars:(c1),vars:(c1)</td>
		      </tr>
		      <tr>
		        <td>0029: return vars:(c1)</td>
		      </tr>
		    </table>
            </div>
	    </Description>
	</Section>
	<Section>
	    <Heading>White Space</Heading>
		<Description>
             <div class="h2section" id="White Space">
			<p>The javaScript compiler uses white space to separate each of its symbols
		    and operators. The javaScript white space characters include all the
		    standard 8-bit ASCII control characters (less than 32 decimal), and the
		    blank character (32 decimal).</p>
		    <b>LF, CR, TAB</b> ..control chars..
		    <p>space</p>
		    <p>The javaScript compiler ignores whitespace</p>
		    <blockquote>
		      <pre>a = 1 + 2;  // This is a valid statement
		       b=1+2;  // This is also a valid statement</pre>
		    </blockquote>
            </div>
	    </Description>
	</Section>
	<Section>
	    <Heading>Special Characters</Heading>
		<Description>
           <div class="h2section" id="Special Characters">
			<p>javaScript uses the standard 8-bit ASCII character set. Some of the
		    javaScript special characters serve to group a set of characters as a single
		    unit (e.g. double quotes group characters to form a string constant). The
		    remainder of the special characters serve to separate tokens (e.g. comma or
		    blank) or prefix a constant (e.g. $ # ).</p>
		    <p>The following are the javaScript special characters.</p>
		    <table border="3" cellpadding="2" width="50%" style="font-weight: bold" >
		      <tr align="top">
		        <td>\</td>
		        <td>|</td>
		        <td>(</td>
		        <td>)</td>
		        <td>[</td>
		        <td>]</td>
		        <td>{</td>
		        <td>}</td>
		        <td>#</td>
		        <td>@</td>
		      </tr>
		      <tr align="top">
		        <td>'</td>
		        <td>'</td>
		        <td>,</td>
		        <td>&quot;</td>
		        <td>:</td>
		        <td>;</td>
		        <td>$</td>
		        <td>%</td>
		        <td>.</td>
		        <td>&nbsp;</td>
		      </tr>
            </div>
	    </Description>
	</Section>
	<Section>
	    <Heading>Naming Conventions for Variables</Heading>
		<Description>
             <div class="h2section" id="Naming Conventions for Variables">
			<p>javaScript variable names are composed of case-sensitive alphanumeric
		    characters. No spaces are allowed in a variable name but the underscore (_)
		    character may be embedded to separate multi-word names . Another convention
		    to make multiple word names more readable its to use start the first word
		    with a lowercase letter and begin the first letter of each succeeding word
		    with an uppercase letter.</p>
		    <p>For example</p>
		    <blockquote>
		      <pre>myVariable
		
		sum
		
		namesOfStudents</pre>
		     </blockquote>
		  </div>
	    </Description>
	</Section>
	<Section>
	    <Heading>Constants</Heading>
		<Description>
		     <div class="h2section" id="Constants">
			<p>javaScript is a dynamically typed language. The type of a variable is
		    unknown until runtime when data is stored into it. The follow table contains
		    the constant forms recognized by the javaScript compiler. For more detail on
		    the data types listed below, see <b>Analytic Information Server Programmer's
		    Guide.</b></p>
		    <table border="3" cellpadding="2" width="50%">
		      <tr align="top">
		        <td><b>Type</b></td>
		        <td><b>Constant Form</b></td>
		      </tr>
		      <tr align="top">
		        <td><b>Void</b></td>
		        <td><b>void</b> or <b>nil</b></td>
		      </tr>
		      <tr align="top">
		        <td><b>Boolean</b></td>
		        <td><b>true</b> or <b>false</b></td>
		      </tr>
		      <tr align="top">
		        <td><b>Date</b></td>
		        <td><b>#Mar,2,1987</b> or <b>#Jun,1,200BC</b></td>
		      </tr>
		      <tr align="top">
		        <td><b>Integer</b></td>
		        <td><b>12 </b>or<b> -2345</b></td>
		      </tr>
		      <tr align="top">
		        <td><b>Number</b></td>
		        <td><b>12.9 </b>or<b> 0.123456</b></td>
		      </tr>
		      <tr align="top">
		        <td><b>Object</b></td>
		        <td><b>#&lt;Vector 1273&gt;</b></td>
		      </tr>
		      <tr align="top">
		        <td><b>String</b></td>
		        <td><b>&quot;Hello World&quot;</b></td>
		      </tr>
		      <tr align="top">
		        <td><b>Symbol</b></td>
		        <td><b>'Hello'</b></td>
		      </tr>
		    </table>
		  </div>
	    </Description>
	</Section>
	<Section>
	    <Heading>Comments</Heading>
		<Description>
             <div class="h2section" id="Comments">
			<p>Because the javaScript compiler tries to evaluate all of the words in a
		    script, it is useful to have text, which is to be ignored by the compiler.
		    This ignored text, called a comment, allows you to include information,
		    which may be useful to understanding the javaScript statements. There are
		    two types of comments: single line and multi-line.</p>
		    <p>A single line comment tells the compiler to ignore all the characters up
		    to the end-of-line (eol). A single line comment must begin with the
		    characters: //</p>
		    <p>For Example:</p>
		    <blockquote>
		      <pre>// This is a comment</pre>
		    </blockquote>
		    <p>A multi-line comment tells the compiler to ignore all the characters
		    embedded in between the delimiters: <b>/*</b> and */</p>
		    <p>For Example:</p>
		    <blockquote>
		      <pre>/*  Humpty Dumpty 
		
		            sat on a wall  */</pre>
		    </blockquote>
            </div>
	    </Description>
	</Section>
	<Section>
	    <Heading>Global Variables</Heading>
		<Description>
             <div class="h2section" id="Global Variables">
			<p>javaScript variables have automatic global declaration. Referencing a
		    symbol, which has not already been declared, automatically causes it to be
		    declared as a global variable. This feature has been added to make
		    javaScript user-friendlier and to make javaScript consistent with other
		    Analytic Information Server languages.</p>
		    <p>The following javaScript expressions are equivalent (The assumption is
		    made that X has not already been referenced).</p>
		    <blockquote>
		      <pre>X = 23</pre>
		    </blockquote>
		    <p>is equivalent to:</p>
		    <blockquote>
		      <pre>var X = 23</pre>
		    </blockquote>
		    <p>javaScript global variables are valid during the whole life of the
		    current workspace (see the <b>_globals</b> global symbol table variable).
		    javaScript global variables are referenced by specifying the symbol. In
		    addition to user defined globals, javaScript global variables include all of
		    the built-in functions such as <b>+ - * upperCase, sin, cos, date, etc.</b></p>
		    <p>The Analytic Information Server javaScript dialect is specified as
		    case-sensitive (most dialects of javaScript are case-insensitive). Therefore</p>
		    <blockquote>
		      <pre>Var</pre>
		    </blockquote>
		    <p>is NOT equivalent to:</p>
		    <blockquote>
		      <pre>var</pre>
		    </blockquote>
            </div>
	    </Description>
	</Section>
	<Section>
	    <Heading>Function Calls</Heading>
		<Description>
           <div class="h2section" id="Function Calls">
			<p>Any user-defined javaScript Lambda, and Lisp Lambda, and any Lambda
		    Information Server function may be called from javaScript. The syntax is
		    simple the function name followed by parenthesis, (). If the function
		    requires arguments, they must be supplied in between the parenthesis and
		    multiple arguments should be separated by a comma. The parenthesis are
		    mandatory even if no arguments are supplied. All javaScript functions
		    receive arguments by value. After function invocation, one and only one
		    result value is returned.</p>
		    <p>For example</p>
		    <blockquote>
		      <pre>mod(10, 2); &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;//Returns 0
		
		            today();            //Returns 729855</pre>
		    </blockquote>
            </div>
	    </Description>
	</Section>
</Essay>
 
</body></html>

