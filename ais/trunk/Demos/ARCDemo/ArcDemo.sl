;;*************************************
;;*************************************
;; Exported Lambda File Cabinet Document
;;*************************************
;;*************************************
;;version=5.0012-32bit
;;location=ArcDemo.sl
;;storageScope=file
;;importSync=auto
;;exportSync=auto
;;autoCompile=true
;;searchSW=false
;;dependencies=
;;objRepoName=ArcDemo.db
;;lastImport=#Mar,11,2011:19:08:21
;;END __DATA

;;**EXPORTKEY**:arcDemo
;;arcDemo 
;;This program provides an interface for regression using the ARC library.
;; See function headers below or AStartup.sl for details.
;; arcDemo.setModel - Returns a model given one of several standard model types. For use with generateData.
;; arcDemo.generateData - Creates training data in training.txt, test data in test.txt given a model,
;;	number of rows, number of columns.
;; arcDemo.SetUniversalGoal - Sets a universal goal given Height of parse tree, NCols, TermSW
;;		Set TermSW to true to allow constants in goal. For use with arcDemo.
;; arcDemo - The arcDemo main program starts a regression given a goal, training data, test data.
;;
;; Author:	Dr. Ted Williams
;;
;; Dependencies:
;;  browseLib
;;	arc
;;	javaScript
;;	math
;;	parseLib
;;	rulesLib
;;	svm
;; Usage: (arcDemo iName iGoal iMaxGens iHaltScore)
;;		iName - User assigned test name assigned to the data
;;		iGoal - iGoal holds abstract regression expression, op statement, where clauses
;;		iMaxGens - iMaxGens holds limit on the number of generations performed
;;		iHaltScore - Halting score to terminate regression
;;		iVerboseSW - Show progress iff true (default false)
(defun arcDemo(iName iGoal iMaxGens iHaltScore ...)
	pvars:(		;; Persistent variables
		;; List of supported model types
		(cModelTypes #(
			"cubic1" "cubic2" "cubic3" "cubic4" "hypertangent1" "linear1" "log1" "sine1" "sqrt1"
			"square1" "trig1"
		))
		;; Corresponding list of formulas
		(cModelFormulas #(
			;; cubic1
			"model(-9.16 + cos(-9.16*x0*x0*x0));"
			;; cubic2
			"model(9.16 + (-9.16*x0*x0*x0) + (-19.56*x0*x1*x1) + (21.87*x0*x1*x2) + (-17.48*x1*x2*x3) + (38.81*x2*x3*x4));"
			;; cubic3
			"model(1.57 + (1.57*x0*x0*x0) + (-39.34*x1*x1*x1) + (2.13*x2*x2*x2) + (46.59*x3*x3*x3) + (11.54*x4*x4*x4));"
			;; cubic4
			"model(9.16 + cos(-9.16*x0*x0*x0) + sin(22.19*x0*x1*x1) + cos(1.07*x0*x1*x2) + sin(-17.48*x1*x2*x3) + cos(18.81*x2*x3*x4));"
			;; hypertangent1
			"model(1.57 + (1.57*tanh(x0*x0*x0)) + (-39.34*tanh(x1*x1*x1)) + (2.13*tanh(x2*x2*x2)) + (46.59*tanh(x3*x3*x3)) + (11.54*tanh(x4*x4*x4)));"
			;; linear1
			"model(1.57 + (1.57*x0) + (-39.34*x1) + (2.13*x2) + (46.59*x3) + (11.54*x4));"
			;; log1
			"model(1.57 + (1.57*log(.000001+abs(x0))) + (-39.34*log(.000001+abs(x1))) + (2.13*log(.000001+abs(x2))) + (46.59*log(.000001+abs(x3))) + (11.54*log(.000001+abs(x4))));"
			;; sine1
			"model(1.57 + (2.13*sin(x2)));"
			;; sqrt1
			"model(1.23 + (1.23*sqrt(x0*x0*x0)) + (-9.16*sqrt(x0*x1*x1)) + (11.27*sqrt(x0*x1*x2)) + (7.42*sqrt(x1*x2*x3)) + (8.21*sqrt(x2*x3*x4)));"
			;; square1
			"model((1.0*x1*x1) + (2.0*x2*x2) + (3.0*x3*x3) + (4.0*x4*x4));"
			;; trig1
			"model(14.65 + (14.65*sin(x0)) + (-6.73*cos(x1)) + (-18.35*tan(x2)) + (-40.32*sin(x3)) + (-4.43*cos(x4)));"
		))
		cTrainX				; Training data - independent variables
		cTrainY				; Training data - dependent variable
		cTestX
		cTestY
		cResult
	) ;; pvars
	vars:( ;;Automatic variables for local use
		aChampions			; Vector of champions returned from ARC
		aDir				; Application directory
		aError				; Error message
		aEstimate			; An X value for one test
		aEstimatesFilename
		aEstimatesCur		; Table of Y estimates on testing data. Columns:
		(aEstimatesCols	#(ID:))
							;ID -- row id (same as aTesting Cursor)
							;Y00 -- Champion 0 Independent variable estimate column ...
		aNChamp				; Champions index
		aNChamp1			; One more than aNChamp
		aNCol				; Column index in X data
		aNCol1				; One more than the column index
		aNRow				; Row index in X and Y data
		aNRows				; Number of rows of data
		aNXCols				; Number of dependent X variables found in training cursor
		aNChampions			; Number of champions in result
		(aNMaxChampions 5)	; Max number of champions to be shown
		aOptionsCur			; Run options cursor
		aOptionsFilename	; Name of file to hold run options
		aOptionsVector		; Vector of data options. A vector of string vectors
		aResultsFilename	; Name of results file in application directory
		aResultsCur			; Table of overall results
		(aResultsCols #("Generations" "Time"))
		aRecord				; Local reference to a record in data table
		aRegressLambda		; Trained regression lambda returned by ARC run
		aEndTime			; Run time in seconds
		aSampleSize			; Number of rows in training data set
		aStartTime			; Starting tick count
		aStatisticsCur		; Table of statistics and estimator formulas (as JavaScript text). columns:
		(aStatisticsCols #(Description: DataSource: NLSE: TCE: RSQ: WFF:))
							; Desc 	-- Champion01 Champion02 ...
							; Data 	-- Taining, Testing
							; NLSE	-- Normalized Least Squred Error
							; TCE	-- Classification Error
							; RSQ	-- R Squared
							; WFF	-- Well Formed Formula
		aStatisticsFilename	; Name of file in application dir. with statistics for last regression.
		aTestEstimate		; Vector of future Y estimates 
		aTestFilename		; Name of file holding testing data
		aTestCur			; Table of data for testing. Columns:
							; ID -- row id
							; X0..n -- Independent variable columns
							; Y -- dependent variable column
		aTestX				; Current (out of sample) data vector rows
		aTestXrow			; Vector of X values in test data for one row
		aTestY				; Current Y dependent variable values
		aTrainCur			; Table of regression training data. Columns:
							; ID	-- row id
							; X0..n -- Independent variable columns
							; Y -- dependent variable column
		aTrainFilename		; Name of file containing training data
		aTrainX				; Training X values
		aTrainXrow			; Vector of X values in test data for one row
		aTrainY				; Training Y dependent variable values
		aTranslationTable	; ARC translation table for selected Training Columns
		aVerboseSW			; true iff show progress
	)
	;; Check for optional arguments
	(if (> (argCount) 4) (setq aVerboseSW (argFetch 4)) (setq aVerboseSW false))

	;;Make sure data files exist
	(setq aDir (new browseLib.dir)) ;; references the application folder (where AStartup.sl resides)
	(setq aTrainFilename (if (> (length iName) 0) (append iName ".Training.txt") "Training.txt"))
	(if aVerboseSW (writeln "Open training file data " aTrainFilename))
	(if (not (aDir.exists aTrainFilename)) (begin
		(writeln (append aTrainFilename " does not exist"))
		(goto EXIT:))
	)
	(setq aTestFilename (if (> (length iName) 0) (append iName ".Testing.txt") "Testing.txt"))
	(if aVerboseSW (writeln "Open testing file data " aTestFilename))
	(if (not (aDir.exists aTestFilename)) (begin
		(writeln (append aTestFilename " does not exist."))
		(goto EXIT:))
	)
	;;Load Training Data
	(if aVerboseSW (writeln "Load Training data."))
	(setq aTrainCur (browseLib.memoryCursor "Training" #("")))
	(aTrainCur.importTab aTrainFilename)

	;;Load Testing Data
	(if aVerboseSW (writeln "Load Testing data."))
	(setq aTestCur (browseLib.memoryCursor "Testing" #("")))
	(aTestCur.importTab aTestFilename)

	;;Make sure Training Data and Testing Data tables have the same column headers
	(setq aNXCols aTrainCur.colCount)
	(loop for aNCol from 0 until aNXCols do
		(if (<> aTrainCur.colVector[aNCol] aTestCur.colVector[aNCol])
			(error (append "TrainingColumn " aTrainCur.colVector[aNCol] " does not match TestingColumn "
			 aTestCur.colVector[aNCol]))
		)
	)
	;; Save these options in the RunOptions file
	(if (> (length iName) 0)
		(setq aOptionsFilename (append iName ".RunOptions.txt"))
		(setq aOptionsFilename "RunOptions.txt"j)
	)
	(setq aOptionsCur (browseLib.memoryCursor aOptionsFilename #("Option   " "Value")))
	(setq aOptionsVector (new Vector:))
	(setq aOptionsVector[0] #("User Name"))
	(setq aOptionsVector[0][1] iName)
	(setq aOptionsVector[1] #("Goal     "))
	(setq aOptionsVector[1][1] iGoal)
	(setq aOptionsVector[2] #("Max  Gens"))
	(setq aOptionsVector[2][1] iMaxGens)
	(setq aOptionsVector[3] #("HaltScore"))
	(setq aOptionsVector[3][1] iHaltScore)
	(setq aNRows (length aOptionsVector))
	(loop for aNRow from 0 until aNRows do
		(setq aRecord (aOptionsCur.getNewRecord))
		(setq aRecord aOptionsVector[aNRow])
		(aOptionsCur.writeLast aRecord)
	)
	(aOptionsCur.exportTab aOptionsFilename)

	;; Note the search space size
	(writeln (arcDemo.calculateSearchSpace iGoal))

	;;Generate translation table (X0 X1 ...)
	(if aVerboseSW (writeln "Generate translation table."))
	(setq aNXCols (- aNXCols 2)) ;;Don't include ID column or dependent (Y) column
	(setq aTranslationTable (new Vector: object: aNXCols)) ;;don't include ID and Y
	(loop for aNCol from 0 until aNXCols do
		(setq aNCol1 (+ aNCol 1))
		(setq aTranslationTable[aNCol] (string aTrainCur.colVector[aNCol1]))
	)
	;;Create Statistics Cursor and remove any previous output file
	(setq aStatisticsFilename (if (> (length iName) 0) (append iName ".Statistics.txt") "Statistics.txt"))
	(if (aDir.exists aStatisticsFilename) (aDir.remove aStatisticsFilename))
	(setq aStatisticsCur (browseLib.memoryCursor aStatisticsFilename aStatisticsCols))

	;;Create Results Cursor and remove any previous output file
	(if aVerboseSW (writeln "Create Results cursor."))
	(setq aResultsFilename (if (> (length iName) 0) (append iName ".Results.txt") "Results.txt"))
	(if (aDir.exists aResultsFilename) (aDir.remove aResultsFilename))
	(setq aResultsCur (browseLib.memoryCursor aResultsFilename aResultsCols))

	;;Load current regression matrix from testing cursor
	;;The independent variables (aTestX)
	;;The dependent variables (aTestY)
	(if aVerboseSW (writeln "Create testing matrix X and testing vector Y."))
	(setq aTestX (new Vector: Object: aTestCur.recordCount))
	(setq aTestY (new Vector: Number: aTestCur.recordCount))
	(loop for aNRow from 0 until aTestCur.recordCount do
		(setq aRecord aTestCur.rowVector[aNRow])
		(setq aTestXrow (new Vector: Number: aNXCols))
		(loop for aNCol from 0 until aNXCols do
			(setq aNCol1 (+ aNCol 1))
			(setq aTestXrow[aNCol] aRecord[aNCol1])
		)
		(setq aTestX[aNRow] aTestXrow)
		(setq aNCol1 (+ aNCol 1))
		(setq aTestY[aNRow] aRecord[aNCol1])
	)
	;;Prepare Training regression matrix from training cursor
	(if aVerboseSW (writeln "Create training matrix X and training vector Y."))
	(setq aTrainX (new Vector: Object: aTrainCur.recordCount))
	(setq aTrainY (new Vector: Number: aTrainCur.recordCount))
	(loop for aNRow from 0 until aTrainCur.recordCount do
		(setq aRecord aTrainCur.rowVector[aNRow])
		(setq aTrainXrow (new Vector: Number: aNXCols))
		(loop for aNCol from 0 until aNXCols do
			(setq aNCol1 (+ aNCol 1))
			(setq aTrainXrow[aNCol] aRecord[aNCol1])
		)
		(setq aTrainX[aNRow] aTrainXrow)
		(setq aNCol1 (+ aNCol 1))
		(setq aTrainY[aNRow] aRecord[aNCol1])
	)
	(setq aTrainCur #void) ;;Release training cursor resources
	(gc) ;;force garbage collection

	;; Set the user Goal
	(if aVerboseSW (writeln "Set user goal"))
	(arc.setUserGoalWFF iGoal)

	;; Run the ARC regression on training data
	;; (arc aTrainX aTrainY MaxGenerations HaltingScore [RandomNumberSeed])
	(setq aStartTime (getTickCount 0))
	(setq aSampleSize (length aTrainX))
	(if aVerboseSW
		(writeln "Start Abstract Regression Classification on [" aSampleSize "] training examples."))
	(arc.clear)
	(arc aTrainX aTrainY (integer iMaxGens) (number iHaltScore))

	;; Get list of Champions -- arc returns up to 25 champions.
	(setq aChampions (arc.getEstimatorChampions false aTranslationTable))
	(setq aNChampions (length aChampions))
	(if (> aNMaxChampions aNChampions) (setq aNMaxChampions aNChampions))
	(if (<= aNMaxChampions 0) (begin
		(setq aError "No champions for training data found.")
		(goto EXIT:)
	))
	(if aVerboseSW (writeln "ARC found at least " aNMaxChampions " champions"))
	;;Create a list of columns for each available champion estimate. Y01 .. Y25
	(loop for aNChamp from 0 until aNMaxChampions do
		(setq aNChamp1 (+ aNChamp 1))	;; Skip over ID
		(setq aEstimatesCols[aNChamp1] (append "Y" (right (append "00" aNChamp) 2)))
	)
	(if aVerboseSW (writeln "Created " (length aEstimatesCols) " Estimates Columns"))

	;;Create Estimates Cursor and remove any previous output file
	(setq aEstimatesFilename (if (> (length iName) 0) (append iName ".Estimates.txt") "Estimates.txt"))
	(if (aDir.exists aEstimatesFilename) (aDir.remove aEstimatesFilename))
	(setq aEstimatesCur (browseLib.memoryCursor aEstimatesFilename aEstimatesCols))
	(if aVerboseSW (writeln "Created aEstimatesCur"))

	;; Generate records for each testing data row
	(if aVerboseSW (writeln "Add results to " aResultsFilename))
	(loop for aNRow from 0 until aTestCur.recordCount do
		(setq aRecord (aEstimatesCur.getNewRecord))
		(setq aRecord[0] aTestCur.rowVector[aNRow][0])
		(aEstimatesCur.writeLast aRecord)	;;Append record to table cursor
	)
	;; Generate Estimates, Statistics for each champion
	(if aVerboseSW (writeln "Generate records for each testing data row"))
	(if aVerboseSW (writeln "Found " aNMaxChampions " champions"))
	(loop for aNChamp from 0 until aNMaxChampions do
		(setq aRegressLambda arc.myPopulation[aNChamp])

		;; Score results against training set
		(arc.scoreTCEandNLSE aRegressLambda)
		(setq aRecord (aStatisticsCur.getNewRecord))
		(setq aRecord.Description (append "Champion" (right (append "00" (+ aNChamp 1)) 2)))
		(setq aRecord.DataSource "Training")
		(setq aRecord.NLSE (text aRegressLambda.NLSE "#.###"))
		(setq aRecord.TCE (text aRegressLambda.TCE "#.####"))
		(setq aRecord.RSQ (text aRegressLambda.RSQ "#.###"))
		(setq aRecord.WFF aChampions[aNChamp])
		(aStatisticsCur.writeLast aRecord)

		;; Score results with testing set
		(arc.scoreTCEandNLSE aRegressLambda aTestX aTestY)
		(setq aRecord (aStatisticsCur.getNewRecord))
		(setq aRecord.Description (append "Champion" (right (append "00" (+ aNChamp 1)) 2)))
		(setq aRecord.DataSource "Testing")
		(setq aRecord.NLSE (text aRegressLambda.NLSE "#.###"))
		(setq aRecord.TCE (text aRegressLambda.TCE "#.####"))
		(setq aRecord.RSQ (text aRegressLambda.RSQ "#.###"))
		(setq aRecord.WFF aChampions[aNChamp])
		(aStatisticsCur.writeLast aRecord)

		;;Insert testing estimates into aEstimatesCur
		(setq aTestEstimate (aRegressLambda.run aTestX)) ;; generate estimates
		(setq aNRows (length aTestEstimate))
		(writeln "Found " aNRows " rows for champion " aNChamp)
		(loop for aNRow from 0 until aNRows do
			;;Offset column by 1 for ID
			(setq aNChamp1 (+ aNChamp 1))
			(setq aEstimate (text aTestEstimate[aNRow] "######.###"))
			(setq aEstimatesCur.rowVector[aNRow][aNChamp1] aEstimate)
			;? (writeln "Set rowVector[" aNRow "][" aNChamp1 "] to " aTestEstimate[aNRow])
		)
	)
	;;Insert Results data
	(if aVerboseSW (writeln "Insert Results data into results file"))
	(setq aRunTime (getTickCount aStartTime))
	(setq aRecord (aResultsCur.getNewRecord))
	(setq aRecord.Generations arc.myGc)
	(setq aRecord.Time (/ aRunTime 3600))	; Convert RunTime to hours
	(aResultsCur.writeLast aRecord)

	;;Export all output tables
	(if aVerboseSW (writeln "Export output tables"))
	(aEstimatesCur.exportTab aEstimatesFilename)
	(aStatisticsCur.exportTab aStatisticsFilename)
	(aResultsCur.exportTab aResultsFilename)

;; SOME DIAGNOSTICS HERE...
;;(setq aChampions (arc.getEstimatorChampions))
;;	(writeln "Summary for diagnostics : in [" myGc "]generations, on random X with Y built from this model source = " iModel)       
;;	(writeln "Search space size=[10e" (integer (round mySearchSpaceExp)) "], goal template WFF = " myAbstractTemplate.JSRC) 
;;	(writeln "Actual computed error on testing data is NLSE=[" (text Lambda.NLSE "#0.00") "], TCE=[" (text Lambda.TCE "#0.00") "], RSQ=[" (text Lambda.RSQ "#0.00") "], while average Y is AvgY=[" (avg Y) "]")
;;	(writeln "WFFs=[" (text (/ myNextID 1000) "#0.00") "K]" 
;;		", WFFEvaluations=[" (text (/ (integer myTotalEvaluations) 1000000) "#0.00") "M]"
;;		", MvlRegressions=[" (text (/ (integer myTotalRegressions) 1000) "#0.00") "K]"
;;		", Time=[" (text (/ endTimeT 3600.0) "#0.00") "Hrs]" 
;;		", ParseTime=[" (text (/ myTotalParseTime 3600.0) "#0.00")  "Hrs]" 
;;		", CompileTime=[" (text (/ myTotalCompileTime 3600.0) "#0.00") "Hrs]"  
;;		", TestingNLSE=[" (text Lambda.NLSE "#0.00") "]"  
;;		", TestingTCE=[" (text Lambda.TCE "#0.00") "]" 
;;		", TestingRSQ=[" (text Lambda.RSQ "#0.00")  "]"  
;;	) ; end writeln      
;;	(writeln "Champion = " aChampions[0])
;; END DIAGNOSTICS

EXIT::  ;;Jump target of various error conditions.
	(writeln "Generations = " arc.myGc)
	(if (< aRunTime 60)
		(writeln "Time = " aRunTime " secs.")
		(if (< aRunTime 3600)
			(writeln "Time = " (/ aRunTime 60) " mins.")
			(writeln "Time = " (/ aRunTime 3600) " hours")
		)
	)
	(writeln "Regression finished. The following files were generated:")
	(writeln aEstimatesFilename)
	(writeln aStatisticsFilename)
	(writeln aResultsFilename)
	(return true)
) ; End main program

;;**EXPORTKEY**:arcDemo.calculateSearchSpace
;; Shows number of all possible combinations of functions, variables, constants
;; Usage: (arcDemo.calculateSearchSpace iGoal)
;;		iGoal - Regression expression plus op expression plus 0-25 where clauses.
;;  Returns: String noting the search space size as a power of 10.
(deforphan arcDemo:calculateSearchSpace(iGoal)

	(return (append "Search Space Size=10**" (arc.computeSizeOfSearchSpace iGoal) " possible combinations"))
)
;;**EXPORTKEY**:arcDemo.generateData
;; Generates training or testing data
;; Usage: (arcDemo.generateData iModel iRandomErr iNRows iNCols iRange iSkew)
;;		iModel - String containing formula for y given x.
;;		iRandomErr - Percentage of random error to be added to Y
;;		iNRows - Number of rows of data
;;		iNCols - Number of columns of X values for each row
;;		iRange - The range of values for each X value generated (defaults to 100).
;;		iSkew - The negative offset of each X value from 0 (defaults to 50).
;;		iSeed - The random number generator seed value.
;;		iVerboseSW - Show exact value of Y iff true (defaults to false).
(deforphan arcDemo:generateData(iModel iRandomErr iNRows iNCols ...)
	vars:(
		aModelLambda	; Function to compute value of Y given the model
		aXMtx			; X matrix with iNRows of aXRow vectors
		aXRow			; X vector with one row of iNCols
		aYCol			; Y column vector of iNRows
		aNRow			; Row index
		aNCol			; Column index
		aX				; One random X value
		aY				; One evaluated Y value
		aRange			; The range of values for each  generated X value (defaults to 100).
		aSkew			; The negative offset of each X value from 0 (defaults to 50).
		aSeed			; The random number generator seed value.
		aVerboseSW		; Verbose flag.
		aText           ; Buffer for generated data.
		)
	(if (> (argCount) 4) (setq aRange (argFetch 4)) (setq aRange 100))
	(if (> (argCount) 5) (setq aSkew (argFetch 5)) (setq aSkew 50))
	(if (> (argCount) 6) (setq aSeed (argFetch 6)) (setq aSeed false))
	(if (> (argCount) 7) (setq aVerboseSW (argFetch 7)) (setq aVerboseSW false))

	;; Make sure that the random function is set
	(arc.setOptions default: 0.0 false)

	;; Use generateXY to prepare Training data. Set the random seed to generate the same Xs as before.
	(if (not (= aSeed false)) (arc.setRandomSeed (number aSeed)))
	;;(if aVerboseSW (writeln "Rows=" iNRows ", Cols=" iNCols ", Range=" aRange ", Skew=" aSkew ", RandomErr=" iRandomErr ", Seed=" arc.mySeedDefault))

	(setq aModelLambda (arc.compileEstimator iModel))
	(if (not (isLambda aModelLambda)) (begin
		(writeln "arc.compileEstimator returned #void")
		(return false)))

	(setq aXMtx (new Vector: Object: iNRows))
	(setq aYCol (new Vector: Number: iNRows))
    (setq aText (new Vector: Byte: (* iNRows iNCols 24)))

	(loop for aNRow from 0 until iNRows do
		(setq aXMtx[aNRow] (setq aXRow (new Vector: Number: iNCols)))
		(loop for aNCol from 0 until iNCols do
			(setq aX (- (arc.myRandomFunction aRange) aSkew))
			(setq aXRow[aNCol] aX)
			(appendWriteln aText (text aX "######.######") #\tab)
		)
		(setq aY (aModelLambda aXRow))
		(if (or (>= aY BIGPOSNUM) (<= aY BIGNEGNUM))
			(writeln "Model generated a big Y " aY "=Model(" (string aX) ")")
		)
		(if (> iRandomErr 0.0)
			(setq aY (+ (* aY (- 1.0 (* .50 iRandomErr))) (* aY (arc.myRandomFunction iRandomErr))))
		)
		(appendWriteln aText (text aY "######.######") #\newline)
		(setq aYCol[aNRow] aY)
		;;(setCdr aXRow aY)
	)

	;(setq arcDemo.cTrainX aXMtx)
	;(setq arcDemo.cTrainY aYCol)
aText)	;; End generateData

;;**EXPORTKEY**:arcDemo.generateData2
;; Generates training and testing data and saves them in tab-delimited files
;; Training.txt and Testing.txt
;; Usage: (arcDemo.generateData iName iModel iRandomErr iNTrainRows iNCols iNTestRows iRange iSkew)
;;		iName - String containing the user assigned name for this regression (may be empty).
;;		iModel - String containing formula for y given x.
;;		iRandomErr - Percentage of random error to be added to Y
;;		iNTrainRows - Number of rows of training data
;;		iNCols - Number of columns of X values for each row of training data and each row of testing data.
;;		iNTestRows - Number of rows of test data (defaults to iNTrainRows).
;;		iRange - The range of values for each X value generated (defaults to 100).
;;		iSkew - The negative offset of each X value from 0 (defaults to 50).
;;		iVerboseSW - Show exact value of Y iff true (defaults to false).
(deforphan arcDemo:generateData2(iName iModel iRandomErr iNTrainRows iNCols ...)
	vars:(
		aData			; Structure containing X matrix and Y vector from generateXY
		aNCol			; Column index
		aNCol1			; One more than the number of columns
		aNOptions		; Number of options
		aNRow			; Row index
		aNRow1			; One more than the number of rows
		aNTestRows		; Number of rows of testing data
		aOptionsCur		; Data options cursor
		aOptionsFilename ; Name of file to hold data options
		aOptionsVector	; Vector of data options. A vector of string vectors
		aRange			; The range of values for each  generated X value (defaults to 100).
		aRecord			; Holds one row of training data or testing data
		aSkew			; The negative offset of each X value from 0 (defaults to 50).
		aTestCols		; Vector holding one one column of testing data
		aTestCursor		; Holds testing data as it is generated
		aTestFilename	; Name of testing data file in application directory
		aTrainCols		; Vector holding one one column of training data
		aTrainCursor	; Holds training data as it is generated
		aTrainFilename	; Name of training data file in application directory
		aVerboseSW		; Verbose flag.
		)
	(if (> (argCount) 5) (setq aNTestRows (argFetch 5)) (setq aNTestRows iNTrainRows))
	(if (> (argCount) 6) (setq aRange (argFetch 6)) (setq aRange 100))
	(if (> (argCount) 7) (setq aSkew (argFetch 7)) (setq aSkew 50))
	(if (> (argCount) 8) (setq aVerboseSW (argFetch 8)) (setq aVerboseSW false))

	;; Save these options in DataOptions file
	(if (> (length iName) 0)
		(setq aOptionsFilename (append iName ".DataOptions.txt"))
		(setq aOptionsFilename "DataOptions.txt"j)
	)
	(setq aOptionsCur (browseLib.memoryCursor aOptionsFilename #("Option  " "Value")))
	(setq aOptionsVector (new Vector:))
	(setq aOptionsVector[0] #("UserName"))
	(setq aOptionsVector[0][1] iName)
	(setq aOptionsVector[1] #("Model   "))
	(setq aOptionsVector[1][1] iModel)
	(setq aOptionsVector[2] #("RandomErr"))
	(setq aOptionsVector[2][1] iRandomErr)
	(setq aOptionsVector[3] #("TrainRows"))
	(setq aOptionsVector[3][1] iNTrainRows)
	(setq aOptionsVector[4] #("XColumns"))
	(setq aOptionsVector[4][1] iNCols)
	(setq aOptionsVector[5] #("TestRows"))
	(setq aOptionsVector[5][1] aNTestRows)
	(setq aOptionsVector[6] #("XRange  "))
	(setq aOptionsVector[6][1] aRange)
	(setq aOptionsVector[7] #("XSkew   "))
	(setq aOptionsVector[7][1] aSkew)
	(setq aNOptions (length aOptionsVector))
	(loop for aNRow from 0 until aNOptions do
		(setq aRecord (aOptionsCur.getNewRecord))
		(setq aRecord aOptionsVector[aNRow])
		(aOptionsCur.writeLast aRecord)
	)
	(aOptionsCur.exportTab aOptionsFilename)

	;; Set file names to hold tab-delimited rows of training data and testing data
	;; Training and Testing Data Format
	;; X1 X2 ... XN Y
	(setq aTrainFilename "Training.txt")
	(setq aTestFilename "Testing.txt")
	(if (> (length iName) 0) (begin
		(setq aTrainFilename (append iName "." aTrainFilename))
		(setq aTestFilename  (append iName "." aTestFilename))
	))

	;; Use generateXY to prepare Training data. Set the random seed to generate the same Xs as before.
	(arc.setRandomSeed arc.mySeedDefault)
	(if aVerboseSW (writeln "Range=" aRange ", Skew=" aSkew ", RandomErr=" iRandomErr ", Seed=" arc.mySeedDefault))
	(setq aData (arcDemo.generateXY iModel iRandomErr iNTrainRows iNCols aRange aSkew aVerboseSW))

	;; Put a header in first row
	;; ID  X0  X1 ...  Y
	(setq aTrainCols (new Vector: (+ iNCols 2)))
	(setq aTrainCols[0] "ID")
	(loop for aNCol from 0 until iNCols do
		(setq aNCol1 (+ aNCol 1))
		(setq aTrainCols[aNCol1] (append "X" aNCol))
	)
	(setq aNCol1 (+ iNCols 1))
	(setq aTrainCols[aNCol1] "Y")

	; Transfer data in aData to training file
	(setq aTrainCursor (browseLib.memoryCursor aTrainFilename aTrainCols))
	(loop for aNRow from 0 until iNTrainRows do
		(setq aNRow1 (+ aNRow 1))
		(setq aRecord (aTrainCursor.getNewRecord))
		(setq aRecord[0] aNRow1)
		(loop for aNCol from 0 until iNCols do
			(setq aNCol1 (+ aNCol 1))
			(setq aRecord[aNCol1] aData.XMtx[aNRow][aNCol])
		)
		(setq aNCol1 (+ iNCols 1))
		(setq aRecord[aNCol1] aData.YCol[aNRow])
		(aTrainCursor.writeLast aRecord)
	)
	;; Use generateXY to prepare Testing data
	(setq aData (arcDemo.generateXY iModel iRandomErr aNTestRows iNCols aRange aSkew aVerboseSW))

	;; Put a header in first row of testing data
	;; ID  X0  X1 ...  Y
	(setq aTestCols (new Vector: (+ iNCols 2)))
	(setq aTestCols[0] "ID")
	(loop for aNCol from 0 until iNCols do
		(setq aNCol1 (+ aNCol 1))
		(setq aTestCols[aNCol1] (append "X" aNCol))
	)
	(setq aNCol1 (+ iNCols 1))
	(setq aTestCols[aNCol1] "Y")

	; Transfer data in aData to testing file
	(setq aTestCursor (browseLib.memoryCursor aTestFilename aTestCols))
	(loop for aNRow from 0 until aNTestRows do
		(setq aNRow1 (+ aNRow 1))
		(setq aRecord (aTestCursor.getNewRecord))
		(setq aRecord[0] aNRow1)
		(loop for aNCol from 0 until iNCols do
			(setq aNCol1 (+ aNCol 1))
			(setq aRecord[aNCol1] aData.XMtx[aNRow][aNCol])
		)
		(setq aNCol1 (+ iNCols 1))
		(setq aRecord[aNCol1] aData.YCol[aNRow])
		(aTestCursor.writeLast aRecord)
	)
	;; Export to tab-delimited files
	(aTrainCursor.exportTab aTrainFilename)
	(aTestCursor.exportTab aTestFilename)
	(return true)
)	;; End generateData

;;**EXPORTKEY**:arcDemo.generateXY
;; Generates XMtx containing NRows of XRow vectors and YCol vector  where YCol[aNRow] = model(XMtx[aNRow]).
;; Random Err. The calculated value for Y for each row may include a small random variation as follows: Let
;;  RE% = 1 - .5*iRandomErr and RE = random value between 0 and iRandomErr.  Then Y = aY * RE% + aY * RE
;; For example, if RandomErr = .05 and Re = .025, Y = aY * .975 + aY * .025 = aY
;; Usage: (arcDemo.generateXY iModelSource iRandomErr iNRows iNCols iRange iSkew)
;;		iModel - String containing formula for y given x. model( C0 + C0
;;		iRandomErr - Fraction of random error introduced into calculated value of Y.
;;		iNRows - Number of rows of data
;;		iNCols - Number of columns of X values for each row of data.
;;		iRange - The range of values for each X value generated.
;;		iSkew - The offset of each X value from 0.  -iSkew <= X[aNCol] <= iRange-iSkew
;;		iVerboseSW - Show exact value of Y
;; Returns: aData structure containing XMtx and YCol
(deforphan arcDemo:generateXY(iModel iRandomErr iNRows iNCols iRange iSkew iVerboseSW)
	vars:(
		aData	;; Structure holding X and Y values generated here.
		aModelLambda ;; Function to compute value of Y given the model
		aNCol	;; column index
		aNRow	;; Row index
		aText	;; One row of generated data
		aX		;; One random X value
		aXRow   ;; X vector with one row of iNCols
		aXMtx	;; X matrix with iNRows of aXRow vectors.
		aYCol	;; Y column vector of iNRows
		x0 x1 x2 x3 x4 ;; Just for testing
		aY		;; value in aYCol[aNRow]
		)
	;; Create function to compute aY given aXRow according to the model provided.
	(setq aModelLambda (arc.compileEstimator iModel))
	(setq aData (new Structure: XMtx: #void YCol: #void)) 

	;; Build Y[aNRow] = model(aXRow) and build aXMtx[aNrow] = aXrow for iNRows
	(setq aXMtx (new Vector: Object: iNRows))
	(setq aYCol (new Vector: Number: iNRows))
	(if iVerboseSW	(begin		;; Print model, header
		(writeln _eol #\tab #\tab #\tab #\tab "GENERATED DATA")
		(writeln iModel _eol)
		(setq aText (append "Row" #\tab))
		(loop for aNCol from 0 until iNCols do
			(setq aText (append aText "X" aNCol #\tab #\tab))
		)
		(writeln aText "Yexact")
	))
	;; Fill aXRow with random values and use the model to compute Y[aNRow].
	(loop for aNRow from 0 until iNRows do
		(setq aXMtx[aNRow] (setq aXRow (new Vector: Number: iNCols)))
		(if iVerboseSW (setq aText (append (+ aNRow 1) #\tab)))
		(loop for aNCol from 0 until iNCols do
			(setq aX (- (arc.myRandomFunction iRange) iSkew))
			(setq aXRow[aNCol] aX)
			(if iVerboseSW (setq aText (append aText (text aX "#####.##") #\tab)))
		)
		(setq aY (aModelLambda aXRow))
		(if iVerboseSW (writeln aText (text aY "#####.##")))
		(if (or (>= aY BIGPOSNUM) (<= aY BIGNEGNUM))
			(writeln "Model generated a big Y " aY "=Model(" (string aX) ")")
		)
		(if (> iRandomErr 0.0)
			(setq aY (+ (* aY (- 1.0 (* .50 iRandomErr))) (* aY (arc.myRandomFunction iRandomErr))))
		)
		(setq aYCol[aNRow] aY)
		(setCdr aXRow aY)
	)
	(setq aData.XMtx aXMtx)
	(setq aData.YCol aYCol)
	(return aData)
)

;;**EXPORTKEY**:arcDemo.getAbstractModel
(deforphan arcDemo:getAbstractModel(Integer:iDepth Integer:iBases Boolean:termSW)
	(arc.openingBook iDepth iBases termSW))

;;**EXPORTKEY**:arcDemo.getCartLtAbstractModel
(deforphan arcDemo:getCartLtAbstractModel(Integer:iTreeDepth Integer:iLeafNodeDepth Boolean:termSW)
	vars:(aSource)
	(setq aSource (append "cartLT(" iTreeDepth "," iLeafNodeDepth "," (if termSW "t" "v") ")"))
	(arc.cartLTJavaScriptModel aSource)
	)

;;**EXPORTKEY**:arcDemo.getDefaultGoal
(deforphan arcDemo:getDefaultGoal(Integer:iDepth Integer:iBases Boolean:termSW)
	(arc.openingBook iDepth iBases termSW))
;;**EXPORTKEY**:arcDemo.getEstimates
;; Returns the estimates using the given model
;; 1st line - "estimates"
;; 2nd line - index of formula
;; 3rd line - tab delimited values
(deforphan arcDemo:getEstimates(iModel iIndex ...)
	vars:(
		aText
		aLambda
		aEstimates
		aLen
		aIndex
	)

	(setq aLambda (arc.compileEstimator iModel))
	;; Check if lambda is valid
	(if (not (isLambda aLambda))
		(begin
			(writeln "arc.compileEstimator returned #void")
			(return false)
		)
	)

	(if (= arcDemo.cTestX #void)
		(begin
			(writeln "arcDemo.cTestX is #void")
			(return false)
		)
	)

	(setq aEstimates (aLambda.run arcDemo.cTestX))
	(setq aLen (length aEstimates))
	(setq aText (new Vector: Byte: (* (+ aLen 1) 24)))
	(appendWriteln aText "estimates" #\newline iIndex #\newline)

	(loop for aIndex from 0 until aLen do
		(appendWriteln aText (text aEstimates[aIndex] "######.######") #\tab)
	)

aText)

;;**EXPORTKEY**:arcDemo.getResults
;; Returns the results as tab delimited data
;; 1st line - "solutions"
;; 2nd line - tab delimted data
(deforphan arcDemo:getResults(...)
	vars:(
		aNumChampions
		aIndex
		aChampions
		aRegressLambda
		aText
	)

	(setq aChampions (arc.getEstimatorChampions))
	(setq aNumChampions (length aChampions))

	(setq aText (new Vector: Byte: (* aNumChampions 512)))
	(appendWriteln aText "solutions" #\newline)

	(loop for aIndex from 0 until aNumChampions do
		(setq aRegressLambda (arc.compileEstimator aChampions[aIndex]))
		(arc.scoreTCEandNLSE aRegressLambda)

		(appendWriteln aText aRegressLambda.WFF #\tab)
		(appendWriteln aText (text aRegressLambda.NMAE "#.######") #\tab)
		(appendWriteln aText (text aRegressLambda.NLSE "#.######") #\tab)
		(appendWriteln aText (text aRegressLambda.TCE "#.######") #\tab)
		(appendWriteln aText (text aRegressLambda.RSQ "#.######") #\newline)
	)

aText)

;;**EXPORTKEY**:arcDemo.getScore
;; Returns the score using the given model
;; We need to have a way to specify the fitness function
(deforphan arcDemo:getScore(iModel iIndex ...)
	vars:(
		aText
		aLambda
	)

	(setq aLambda (arc.compileEstimator iModel))
	;; Check if lambda is valid
	(if (not (isLambda aLambda))
		(begin
			(writeln "arc.compileEstimator returned #void")
			(return false)
		)
	)

	(if (or (= arcDemo.cTestX #void) (= arcDemo.cTestY #void))
		(begin
			(writeln "arcDemo.cTestX and/or arcDemo.cTestY is #void")
			(return false)
		)
	)

	(arc.scoreTCEandNLSE aLambda arcDemo.cTestX arcDemo.cTestY)
	(setq aText (append "score" #\newline iIndex #\tab (text aLambda.SCORE "#.######")))

aText)

;;**EXPORTKEY**:arcDemo.prettyPrintMemory
(defchild arcDemo:prettyPrintMemory(ioBuffer iMemory iMargin)
	vars:(n N key newMargin
		(blanks "                                                                                                                                                              ")
		(tailSW false)
		) ; end temporary variables
		;; Initialize the display buffer (if necessary).
		(if (= ioBuffer #void) (begin
			(setq ioBuffer (new Vector: byte: 2000000))
			(setq tailSW true)
		))
	;; Validate the display buffer (if necessary).
	(cond 
		;; Manage pretty printing of a Structure.
		((= (type iMemory) Structure:)
		(begin
			(setq N (length iMemory))
			(setq newMargin (+ iMargin 2))
			(appendWriteln ioBuffer "#{")
			(loop for n from 0 until N do  
				(setq key iMemory[n 0])
				(appendWriteln ioBuffer key ": ")
			(prettyPrintMemory ioBuffer iMemory[n 1] (+ newMargin (length key) 2))
				(appendWriteln ioBuffer _eol (left blanks newMargin))
				) ; end loop
			(appendWriteln ioBuffer "} ; end Structure")
		)) ; end Structure case
		;; Manage pretty printing of a Dictionary.
		((= (type iMemory) Dictionary:)
		(begin
			(setq N (length iMemory))
			(setq newMargin (+ iMargin 2))
			(appendWriteln ioBuffer "#{dic|| " _eol  (left blanks newMargin))
			(loop for n from 0 until N do  
				(setq key iMemory[n 0])
				(appendWriteln ioBuffer key ": ")
				(appendWriteln ioBuffer (string iMemory[n 1] true))
				(appendWriteln ioBuffer _eol (left blanks newMargin))
				) ; end loop
			(appendWriteln ioBuffer "} ; end Dictionary")
		)) ; end Dictionary case
		;; Manage pretty printing of a Directory.
		((= (type iMemory) Directory:)
		(begin
			(setq N (length iMemory))
			(setq newMargin (+ iMargin 2))
			(appendWriteln ioBuffer "#{dir|| " _eol  (left blanks newMargin))
			(loop for n from 0 until N do  
				(setq key iMemory[n 0])
				(appendWriteln ioBuffer (string key true) " ")
				(appendWriteln ioBuffer (string iMemory[n 1] true))
				(appendWriteln ioBuffer _eol (left blanks newMargin))
				) ; end loop
			(appendWriteln ioBuffer "} ; end Directory")
		)) ; end Directory case
		;; Manage pretty printing of an Object Vector.
		((= (type iMemory) ObjVector:)
		(begin
			(setq N (length iMemory))
			(setq newMargin (+ iMargin 6))
			(appendWriteln ioBuffer "#(obj|" _eol (left blanks newMargin))
			(loop for n from 0 until N do  
				(prettyPrintMemory ioBuffer iMemory[n] newMargin)
				(appendWriteln ioBuffer " ;; [" n "]" _eol (left blanks newMargin))
				) ; end loop
			(appendWriteln ioBuffer ") ; end ObjVector")
		)) ; end ObjVector case
		;; Manage pretty printing of a String.
		((= (type iMemory) String:)
		(begin
			(appendWriteln ioBuffer "{" (substitute (substitute (substitute (substitute iMemory _eol #\tab) #\tab " ") #\return " ") #\newline " ") "}")
		)) ; end ObjVector case
		;; Manage pretty printing of default value.
		(else
			(appendWriteln ioBuffer (string iMemory true))
		) ; end other case
	) ; end cond
	;; Terminate the display buffer (if necessary).
	(if (= tailSW true)
		(begin 
			(setq ioBuffer (string ioBuffer))
	))	; end if
ioBuffer) ;; end prettyPrintMemory

;;**EXPORTKEY**:arcDemo.setModel
;; Usage: (arcDemo.setModel iModelType iVerboseSW)
;; setModel creates one of several standard models supported by arcDemo.  Only used by generateData.
;;    iModelType - String containing one of the types of models provided by arcDemo (see code for list)
;;    iVerboseSW - If true, prints model type and the associated formulas (default false)
;; Returns: String containing the formula to be estimated by the regression.
(deforphan arcDemo:setModel(iModelType ...)
	vars:(	;; automatic variables for local use
		(aModel "")		;; Formula for specified model type
		aNMod			;; ModelType index
		aNMods			;; Number of model types
		aVerboseSW		;; Verbose switch
	)
	;; Check for optional arguments
	(if (> (argCount) 1) (setq aVerboseSW (argFetch 1)) (setq aVerboseSW false))

	;; If no Model Type provided, show the selection
	(setq aNMods (length arcDemo.cModelTypes))
	(if (< (length iModelType) 2) (begin
		(arcDemo.showModels)
		(goto EXIT:)
	)) 
	;; Select model for specified model type
	(loop for aNMod from 0 until aNMods do
		(if (= (downcase iModelType) arcDemo.cModelTypes[aNMod]) (begin
			(setq aModel arcDemo.cModelFormulas[aNMod])
			(goto EXIT:)
		))
	)
	(setq aModel "Unknown")
	(writeln "Unknown Model Type: " iModelType)
EXIT::
	(if aVerboseSW (writeln (append iModelType ": " aModel ")")))
	(return aModel)
)	 ;; end setModel

;;**EXPORTKEY**:arcDemo.setUniversalGoal
;; Usage: (arcDemo.setUniversalGoal iClauses iHeight iNArgs iTermSW iVerboseSW)
;; setUniversalGoal returns a universal goal for use by arcDemo.
;;	  iClauses - optional op clause followed by 0-25 where clauses tacked on to end of returned goal 
;;    iHeight - The maximum depth of any node in the parse tree (the number of nested terms in f0).
;;    iNArgs - The number of arguments in the resulting regress expression (number of columns in X matrix)
;;    iTermSW - true iff constants are allowed.
;;    iVerboseSW - If true, prints each argument of regress expression as it proceeds (default false)
;; Returns: The regression expression that includes all possible concrete instances of the allowed grammars.
;; Uses: (arc.universalJavaScriptTerm iHeight iTermSW aDepth aNArg) to generated each argument of regress.
(deforphan arcDemo:setUniversalGoal(iClauses iHeight iNArgs iTermSW ...)
	vars:(		;; automatic variables for local use
		aArg		; One regress argument
		(aGoal "")	; Entire regress expression
		aNArg		; Number of arguments so far
		(aNDepth 0)	; Number of functions used so far
		aNLast		; Last argument required
		aVerboseSW	; Verbose flag
		)
	;; Check for optional arguments
	(if (> (argCount) 4) (setq aVerboseSW (argFetch 4)) (setq aVerboseSW false))

	;; Validate input
	(if (or (< iHeight 1) (> iHeight 10))
		(error (append "Height must be from 1 - 10, Height =" iHeight))
	)
	(if (or (< iNArgs 1) (> iNArgs 10))
		(error (append "NArgs must be from 1 - 10, NArgs =" iNArgs))
	)
	;; Reset myUniversalFNUM and myUniversalTNUM to -1
	(arc.clear)

	;; Build regress expression one argument at a time.
	(setq aGoal "regress(")
	(if aVerboseSW (writeln aGoal))
	(setq aNLast (- iNArgs 1))
	(loop for aNArg from 0 until iNArgs do
		(setq aArg (arc.universalJavaScriptTerm iHeight iTermSW aDepth aNArg))
		(if aVerboseSW
			(if (< aNArg aNLast)
				(writeln aArg ",")
				(writeln aArg)
			)
		)
		(if (< aNArg aNLast)
			(setq aGoal (append aGoal aArg ", "))
			(setq aGoal (append aGoal aArg))
		)
	)
	(if aVerboseSW (writeln ")" iClauses ";"))
	(return (append aGoal ")" iClauses ";"))
) ;; end setUniversalGoal

;;**EXPORTKEY**:arcDemo.showAgents
;; Displays a description of each ArcDemo agent, the usage, the arguments and the return value.
;; Usage: (arcDemo.showAgents)
;; Returns: true
(deforphan arcDemo:showAgents()
	(writeln "arcDemo.setModel - Returns a model, y = f(X), for any one of several standard model types.")
	(writeln "A model is only needed if you are going to generate data.  The following model types are supported:")
	(writeln "cubic1, cubic2, cubic3, cubic4, hypertangent1, linear1, log1, sine1, sqrt1, square1, trig1")
	(writeln "(arcDemo.setModel iModelType iVerboseSW)")
	(writeln "   iModelType - String containing one of the above model types")
	(writeln "   iVerboseSW - Display model equation iff true (default false).")
	(writeln " Returns: String holding model, f(X)")
	(writeln "")
	(writeln "arcDemo.generateData - Creates training data in training.txt, test data in testing.txt given a model,")
	(writeln "number of rows of training data, number of X columns of training data, etc.  The calculated X values")
	(writeln "are random values between -iSkew and iRange - iSkew.  A header row plus iNTrainRows (or iNTestRows)")
	(writeln "are created in each file.  The header of the form ID  X0 X1 ... Xn Y where n is one less than iNCols.")
	(writeln "The Y value is calculated for each row of Xs from the model formula y = f(X0 X1 ...) where")
	(writeln "iRandomErr may be used to introduce a small random variation in the calculated Y value.")
	(writeln "iName specifies an arbitrary user-defined name that is prepended to the data file names.")
	(writeln "See arcDemo.sl for details.  If the last 3 arguments are missing, the default values are shown below.")
	(writeln "generateData is only needed if you do not have your own data.")
	(writeln "(arcDemo.generateData iName iModel iRandomErr iNTrainRows iNCols iNTestRows iRange iSkew iVerboseSW)")
	(writeln "    iName - User-assigned name for this regression (may be empty).")
	(writeln "    iModel - String containing the model (expression for Y given the Xs)")
	(writeln "    iRandomErr - Percent error added to the Y variable")
	(writeln "    iNTrainRows - Number of rows of training data to be generated.")
	(writeln "    iNCols - Number of X columns (X0, X1, ...) to be generated.")
	(writeln "    iNTestRows - Number of rows of testing data to be generated (default iNTrainRows).")
	(writeln "    iRange - Range of each calculated X value (default 100).")
	(writeln "    iSkew - Negative offset of minimum value of X from 0 (default 50).")
	(writeln "    iVerboseSW - Show exact value of Y iff true (default false).")
	(writeln "")
	(writeln "arcDemo.setUniversalGoal - Sets a universal goal given height of parse tree, NArgs, TermSW.")
	(writeln "Set TermSW to true to allow constants in goal. SetUniversalGoal is only needed if you wish to ")
	(writeln "automatically create a goal that encompases all possible combinations of the 28 concrete functions.")
	(writeln "Otherwise, you can enter a goal of your choice when calling arcDemo to run a regression.")
	(writeln "(arcDemo.setUniversalGoal iClauses iHeight iNArgs iTermSW iVerboseSW)")
	(writeln "     iClause - string holding optional op clause followed by 0-25 where clauses.")
	(writeln "     iHeight - the maximum depth of the parse tree any argument of the regession expression.")
	(writeln "     iNArgs - the number of arguments (independent variables) in the regression expression.")
	(writeln "     iTermSW - allow constants in the regression expression iff true.")
	(writeln "     iVerboseSw - display arguments of the regression expression iff true (default false).")
	(writeln " Returns: Goal - A regression expression plus op expression plus 1-25 where clauses.")
	(writeln "")
	(writeln "arcDemo.calculateSearchSpace - Returns the search space as a power of ten for a given goal.")
	(writeln "(arcDemo.calculateSearchSpace iGoal)")
	(writeln "      iGoal - Regression expression plus op expression plus 1-25 where clauses.")
	(writeln "Returns: String noting the search space size as 10 ** size.")
	(writeln "")
	(writeln "arcDemo - The arcDemo main program starts a regression given a goal, training data, test data. This")
	(writeln "is the only function that is required to run a regression.  arcDemo notes the search space size")
	(writeln "at the beginning of the regression.")
	(writeln "(arcDemo iName iGoal iMaxGens iHaltScore iVerboseSW)")
	(writeln "     iName - User-assigned name for this regression (same name as the one assigned to data files.")
	(writeln "     iGoal - Regression expression plus op expression plus 1-25 where clauses.")
	(writeln "     iMaxGens - Maximum number of generations")
	(writeln "     iHaltScore - Halting score")
	(writeln "     iVerboseSw - display progress iff true (default false).")
	(writeln "")
	(return true)
)

;;**EXPORTKEY**:arcDemo.showHypertangentExample1
;; Displays the calls to ArcDemo to do a simple linear example
;; Usage: (arcDemo.showHypertangentExample1)
;; Returns: true
(deforphan arcDemo:showHypertangentExample1()
	(writeln "                               HYPERTANGENT EXAMPLE1")
	(writeln "First, create a hypertangent model for use as an argument to generateData, with verbose")
	(writeln "set to show the model type and the model generated:")
	(writeln "(arcDemo.setModel \"hypertangent1\" true)")
	(writeln "Then, create 200 rows of sample training data with columns ID X0 X1 X2 X3 X4 Y with .05 random ")
	(writeln "noise, 100 rows of sample testing data, 5 independent Xs, range of 1.0, skew of 0.5 (-.5 to +.5)")
	(writeln "and verbose set to show each row generated where Y is determined by the sample model:")
	(writeln "(arcDemo.generateData \"hypertangent1\" (arcDemo.setModel \"hypertangent1\") .05 200 5 100 1.0 .5 true)")
	(writeln "")
	(writeln "Next, create a Universal Goal for this model with parse tree 2 levels deep, 5 arguments, allow")
	(writeln "constants, verbose to display the goal:")
	(writeln "(arcDemo.setUniversalGoal \" op(noop,+,-,*,tanh) where{}\" 2 5 true true)")
	(writeln "To show the search space size for this goal, use:")
	(writeln "(arcDemo.calculateSearchSpace (arcDemo.setUniversalGoal \" op(noop,+,-,*,tanh) where{}\" 2 5 true))")
	(writeln "")
	(writeln "Finally, run a regression on this model to see how close ARC can come to finding the model")
	(writeln "using the training.txt and testing.txt generated above.  Select goal using the above universal")
	(writeln "goal, 5 generations max, .05 halting score, verbose to show each step")
	(writeln "(arcDemo \"hypertangent1\" (arcDemo.setUniversalGoal \" op(noop,+,-,*,tanh) where{}\" 2 5 true) 5 .05 true)")
	(writeln "")
	(return true)
)

;;**EXPORTKEY**:arcDemo.showLinearExample1
;; Displays the calls to ArcDemo to do a simple linear example
;; Usage: (arcDemo.showLinearExample1)
;; Returns: true
(deforphan arcDemo:showLinearExample1()
	(writeln "                               LINEAR EXAMPLE1")
	(writeln "First, create the linear model(1.57 + 1.57*x0 + -39.34*x1 + 2.13*x2 + 46.59*x3 + 11.54*x4); for use")
	(writeln "as an argument to generateData, with verbose set to show the model type and the model generated:")
	(writeln "(arcDemo.setModel \"linear1\" true)")
	(writeln "Then, create 200 rows of sample training data with columns ID X0 X1 X2 X3 X4 Y")
	(writeln "with 5 independent Xs, 100 rows of sample testing data, range of 10.0, skew of 5.0 (-5 to +5)")
	(writeln "and verbose set to show each row generated where Y is determined by the sample linear model:")
	(writeln "(arcDemo.generateData \"linear1\" (arcDemo.setModel \"linear1\") .05 200 5 100 10.0 5.0 true)")
	(writeln "")
	(writeln "Next, create a Universal Goal for this model with parse tree 2 levels deep, 5 arguments, allow")
	(writeln "constants, verbose to display the goal:")
	(writeln "(arcDemo.setUniversalGoal \"\" 2 5 true true)")
	(writeln "To show the search space size for this goal, use:")
	(writeln "(arcDemo.calculateSearchSpace (arcDemo.setUniversalGoal \"\" 2 5 true))")
	(writeln "")
	(writeln "Finally, run a regression on this linear model to see how close ARC can come to finding the model")
	(writeln "using the training.txt and testing.txt generated above.  Select goal using the above universal")
	(writeln "goal 5 generations max, .05 halting score, verbose to show each step")
	(writeln "(arcDemo \"linear1\" (arcDemo.setUniversalGoal \"\" 2 5 true) 5 .05 true)")
	(writeln "or run a quick linear regression using:")
	(writeln "(arcDemo \"linear1\" \"regress(x0,x1,x2,x3,x4);\" 5 .05 true)")
	(writeln "")
	(return true)
)

;;**EXPORTKEY**:arcDemo.showModels
;; Displays the sample models supported by arcDemo
;; Usage: (arcDemo.showModels)
;; Returns: true
(deforphan arcDemo:showModels()
	vars:(		;; automatic variables for local use
		aLine	;; One line of output
		aNMod	;; Model type index
		aNMods	;; Number of model types
		aNTab	;; Tab index
		aNTabs	;; Number of tabs to second column of display
	)
	;;(debug traceon:)
	;; Print the list of model types and the associated formula
	(setq aNMods (length arcDemo.cModelTypes))
	(writeln "                               EXAMPLE MODELS")
	(loop for aNMod from 0 until aNMods do
		(setq aLine arcDemo.cModelTypes[aNMod])
		(setq aNTabs (idiv (- 19 (length aLine)) 4))
		(loop for aNTab from 0 until aNTabs do
			(setq aLine (append aLine #\tab))
		)
		(writeln aLine arcDemo.cModelFormulas[aNMod])
	)
)
;; end arcDemo.sl


;;**EXPORTKEY**:arcDemo.start
(deforphan arcDemo:start(iGoal iMaxGens iHaltFitness iType)
	(writeln "Maximum generations: " iMaxGens)
	(writeln "Halting score: " iHaltFitness)
	(writeln "Goal: " iGoal)
	(writeln "Type: " iType)
	(if (= (length iGoal) 0) (return false))
	(arc.setOptions iType 00% true)
	(arc.setUserGoalWFF iGoal)
	;;(setq arc.myClientReportingLambda (lambda(msg) (writeln msg)))
	(setq arc.myClientReportingLambda sendStatusUpdateEvent)
	(arc.clear)
	(writeln "Starting training...")
	(setq arcDemo.cResult (arc arcDemo.cTrainX arcDemo.cTrainY (integer iMaxGens) (number iHaltFitness)))
	(writeln "Training complete.")
true) ;; end of start

