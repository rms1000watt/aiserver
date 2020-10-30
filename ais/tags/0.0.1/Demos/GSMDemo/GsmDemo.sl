;;*************************************
;;*************************************
;; Exported Lambda File Cabinet Document
;;*************************************
;;*************************************
;;version=5.0011-64bit
;;location=GsmDemo.sl
;;storageScope=file
;;importSync=auto
;;exportSync=auto
;;autoCompile=true
;;objRepoName=GsmDemo.db
;;lastImport=#Jul,28,2009:08:53:39
;;END __DATA

;;**EXPORTKEY**:gsmDemo
;;gsmDemo 
;;This program provides a simple example harness for regression 
;;using the GSM library. 
;;This program requires the following input files:
;; 	testname.parameters.txt	-- tab delimited parameters table for GSM settings.
;; 	testname.traindata.txt	-- tab delimited data table used for training. id, independant variables, dependant variable
;;	testname.testdata.txt	-- tab delimited data table used for testing. id, independant variables.
;;
;;The program produces the following output files:
;; 	testname.output.txt		-- tab delimited data table. id, Calculated dependant variable.
;;	testname.report.txt		-- textual report containing regression formulas etc.
;;
;; Author:	Timothy May
;;		
;; Dependencies: browseAgent 
;;	gsm 
;;	math
;;	parseAgent 
;;	rulesAgent 
;;	javaScript
;;
(defun gsmDemo(aTestName)

	vars:( ;;Automatic (temporary) Variables
		N r R c i n 			;; various counter args
		aStartTime				; start time tick count
		aEndTime				; end time in tick counts
		aError					; error string
		aText					; 

		aParametersFilename
		aParametersCursor		;table of regression parameters, Columns:
									;Generations -- maximum number of generations
									;MaxTime -- maximum amount of time to process in hours
									;ModelName -- name of GSM Model to use
									;HaltingScore -- Percentage error to halt on
									;SVMKernelID --Which SVM Kernel to use
									;NumChampions -- Number of Champions to return
		(aModelNames #(regressGSOALPS: regressSWARM:))			;;Allowed list of ModelNames supported by GSM
		(aSVMKernelIDs #(binary: bipolar: composite: cosine: cube: exp: linear: log: poly: quart: sigmoid: sine: square: tan: tanh:))	;;Allowed list of SVMKernelIDs supported by GSM
		
		aTrainingFilename
		aTrainingCursor			;table of regression data. Columns:
									;ID	-- row id
									;X1..n -- Independant variable columns
									;Y -- Dependant variable column


   		aDependantColumnCount	;number of dependant variables found in traning cursor
		aSampleSize				;Number of rows in training sample set

		aTestingFilename
		aTestingCursor			;table of data for testing. Columns:
									;ID -- row id
									;X1..n -- Independant variable columns
									;Y -- Dependant variable column


		aEstimatesFilename
		aEstimatesCursor		;table of Y estimates on testing data. Columns:
		(aEstimatesColumns	#(ID: Y00:))
									;ID -- row id (same as aTesting Cursor)
									;Y00 -- Linear Independant variable estimate
									;Y01 -- Non-Linear Candidate 1 Independant variable estimate column
									;...
									;Y99 -- Non-Linear Candidate 99 Independant variable estimate column


		aStatisticsFilename
		aStatisticsCursor		;table of statistics and estimator formulas (as JavaScript text). columns:
		(aStatisticsColumns #(Desc: Data: NLSE: TCE: RSQ: WFF:))
									;Desc 	-- Linear, NonLinear01 NonLinear02 .. NonLinear05
									;Data 	-- Taining, Testing
									;NLSE	-- Normalized Least Squred Error
									;TCE	-- Classification Error
									;RSQ	-- R Squared
									;WFF	-- Well Formed Formula

		aResultsFilename
		aResultsCursor			;table of overall results
		(aResultsColumns #("TestName" "Generations" "Time"))
									

		(aGenerations 100)		;Maximum number of generations -- default 100
		(aMaxTime 1)			;MaxTime -- default 1 hour
		(aModelName regressMVLALPS:)	;ModelName -- default 
		(aHaltingScore 0.0)		;HaltingScore -- default 0.0
		(aSVMKernelID composite:)		;SVMKernelID -- default
		(aNumChampions 5)		;aNumWFF -- number of champions to return

		aChampions				;vector of champions returned from gsm

		aRegressLambda			;trained regression lambda returned by gsm run
		aTranslationTable		;GSM translation table for selected Training Columns
		aDependantColumnCount	;Number of Independant variable columns.

		aTestX						;Current (out of sample) data vector rows
		aTestXrow
		aTestY						;Current (out of sample estimate) dependant variable values

		aTrainX						;Training (in sample) data  vector
		aTrainXrow
		aTrainY						;Training (out of sample) dependant variable values
		
		aTestingEST				;Vector of future value estimates 
		
		aDir					;directory lambda
		aRecord					;Local reference (for speed) to a record in data table
	)


	;;Make sure data files exist
	(setq aDir (new browseLib.dir)) ;; referecne to application folder
	(setq aParametersFilename (append aTestName ".Parameters.txt"))
	(if (not (aDir.exists aParametersFilename))(begin
		(setq aError (append aParametersFilename " does not exist"))
		(goto EXIT:)))

	(setq aTrainingFilename (append aTestName ".Training.txt"))
	(if (not (aDir.exists aTrainingFilename)) (begin
		(setq aError (append aTrainingFilename " does not exist"))
		(goto EXIT:)))

	(setq aTestingFilename (append aTestName ".Testing.txt"))
	(if (not (aDir.exists aTestingFilename)) (begin
		(setq aError (append aTestingFilename " does not exist"))
		(goto EXIT:)))

	;;Load Parameters
	(setq aParametersCursor (browseLib.memoryCursor "Parameters" #("")))
	(aParametersCursor.importTab aParametersFilename)
		
	;;Load Traning Data
	(setq aTrainingCursor (browseLib.memoryCursor "Training" #("")))
	(aTrainingCursor.importTab aTrainingFilename)

	;;Load Testing Data
	(setq aTestingCursor (browseLib.memoryCursor "Testing" #("")))
	(aTestingCursor.importTab aTestingFilename)

	;;Make sure Training Data and Testing Data tables have the same column headers
	(loop for c from 0 until aTrainingCursor.colCount do
		(if (<> aTrainingCursor.colVector[c] aTestingCursor.colVector[c])
			(error (append "TrainingColumn " aTrainingCursor.colVector[c] " does not match TestingColumn " aTestingCursor.colVector[c])))
	)
	(setq aDependantColumnCount (- aTrainingCursor.colCount 2)) ;;Don't count ID column or Dependant (Y) column

	;;Generate translation table
	(setq aTranslationTable (new Vector: object: (- aTrainingCursor.colCount 2))) ;;don't include ID column and Y column
	(loop for i from 0 until aDependantColumnCount do 
		(setq c (+ i 1))
		(setq aTranslationTable[i] (string aTrainingCursor.colVector[c]))
	)

   		;;Load and Check arguments
	(loop for r from 0 until aParametersCursor.recordCount do
		(setq aRecord aParametersCursor.rowVector[r])
		(cond
			((= aRecord[0] "Generations")
				(setq aGenerations aRecord[1])				
				(if (or (not (isNumber aGenerations)) (< aGenerations 1)) (begin
					(setq aError (append aTestName "Generations must be greater than 0 -- (" aGenerations ") specified"))
					(goto EXIT:))))

			((= aRecord[0] "MaxTime")
				(setq aMaxTime aRecord[1])
				(if (or (not (isNumber aMaxTime)) (<= aMaxTime 0.0)) (begin
					(setq aError (append aTestName "MaxTime must be greater than 0.0 -- (" aMaxTime ") specified"))
					(goto EXIT:))))

			((= aRecord[0] "ModelName")
				(setq aModelName (symbol aRecord[1]))
				(if (not (member aModelName aModelNames))
					(setq aError (append aTestName "ModelName must be one of " aModelNames))
					(goto EXIT:)))

			((= aRecord[0] "HaltingScore")
				(setq aHaltingScore aRecord[1])
				(if (or (not (isNumber aHaltingScore)) (< aHaltingScore 0.0)) (begin
					(setq aError (append aTestName "HaltingScore must be greater than or equal to 0.0 -- (" aMaxTime ") specified"))
					(goto EXIT:))))

			((= aRecord[0] "SVMKernelID")
				(setq aSVMKernelID (symbol aRecord[1]))
				(if (not (member aSVMKernelID aSVMKernelIDs))
					(setq aError (append aTestName "SVMKernelID " aModelName " specified not of " aSVMKernelIDs))
					(goto EXIT:)))

			((= aRecord[0] "NumChampions")
				(setq aNumChampions aRecord[1])
				(if (or (not (isNumber aNumChampions)) (< aNumChampions 1) (< aNumChampions 25)) (begin
					(setq aError (append aTestName "NumChampions must be greater than 0 and less than 25 -- (" aNumChampions ") specified"))
					(goto EXIT:))))
		
		);cond
	);r

	;;Create Statistics Cursor and remove any previous output file
	(setq aStatisticsFilename (append aTestName ".Statistics.txt"))
	(if (aDir.exists aStatisticsFilename) (aDir.remove aStatisticsFilename))
	(setq aStatisticsCursor (browseLib.memoryCursor aStatisticsFilename aStatisticsColumns))

	;;Create Results Cursor and remove any previous output file
	(setq aResultsFilename (append aTestName ".Results.txt"))
	(if (aDir.exists aResultsFilename) (aDir.remove aResultsFilename))
	(setq aResultsCursor (browseLib.memoryCursor aResultsFilename aResultsColumns))

	;;Load current regression matrix from testing cursor
	;;The independant variables (aTestX) 
	;;The dependant variables (aTestY) 
	(setq aTestX (new Vector: Object: aTestingCursor.recordCount))
	(setq aTestY (new Vector: Number: aTestingCursor.recordCount))
	(loop for r from 0 until aTestingCursor.recordCount do
		(setq aRecord aTestingCursor.rowVector[r])
		(setq aTestXrow (new Vector: Number: aDependantColumnCount))
		(loop for i from 0 until aDependantColumnCount do
			(setq aTestXrow[i] aRecord[(+ i 1)])
		)
		(setq aTestX[r] aTestXrow)
		(setq aTestY[r] aRecord[(+ i 1)])
	);r


	;;Prepare Training regression matrix from training cursor
	;;The independant variables (aTrainX) 
	;;The dependant variables (aTrainY) 
	(setq aTrainX (new Vector: Object: aTrainingCursor.recordCount))
	(setq aTrainY (new Vector: Number: aTrainingCursor.recordCount))
	(loop for r from 0 until aTrainingCursor.recordCount do
		(setq aRecord aTrainingCursor.rowVector[r])
		(setq aTrainXrow (new Vector: Number: aDependantColumnCount))
		(loop for i from 0 until aDependantColumnCount do
			(setq aTrainXrow[i] aRecord[(+ i 1)])
		)
		(setq aTrainX[r] aTrainXrow)
		(setq aTrainY[r] aRecord[(+ i 1)])
	);r

	(setq aTrainingCursor #void) ;;Release training cursor resources
	(gc) ;;force garbage collection


	(setq aStartTime (getTickCount 0))
	

	(setq aSampleSize (length aTrainX))
	(writeln "GSM Regression is starting training on [" aSampleSize "] training examples.")

	;; Set the Regression User Defined Learning Options
	;; (gsm.setOptions ModelName randomError verboseSW [Maxtime] [SVMKernelID])  [] indicates optional parameters
	(gsm.setOptions aModelName 0% false aMaxTime aSVMKernelID) ;; use this script now  

	;; Run the gsm regression
	;; (gsm aTrainX aTrainY MaxGenerations HaltingScore [RandomNumberSeed])  [] indicate optional values
	(gsm aTrainX aTrainY (integer aGenerations) (number aHaltingScore))


	;; Get list of Champions -- gsm returns up to 25 champions.
	(setq aChampions (gsm.getEstimatorChampions false aTranslationTable))
	(if (> aNumChampions (length aChampions)) (setq aNumChampions (length aChampions)))

	;;Create a list of columns for each available champion estimate. Y01 .. Y25
	(loop for c from 0 until aNumChampions do
		(setq aEstimatesColumns[(length aEstimatesColumns)] (append "Y" (right (append "00" (+ c 1)) 2)))
	)

	;;Create Estimates Cursor and remove any previous output file
	(setq aEstimatesFilename (append aTestName ".Estimates.txt"))
	(if (aDir.exists aEstimatesFilename) (aDir.remove aEstimatesFilename))
	(setq aEstimatesCursor (browseLib.memoryCursor aEstimatesFilename aEstimatesColumns))


	;Generate records for each TESTING data row
	(loop for r from 0 until aTestingCursor.recordCount do
		(setq aRecord (aEstimatesCursor.getNewRecord))
		(setq aRecord[0] aTestingCursor.rowVector[r][0])
		(aEstimatesCursor.writeLast aRecord);;Append record to table cursor
	)

	;; Generate Estimates, Statistics entries for Linear regression
	(setq aRegressLambda gsm.myBestLinear) 
	(gsm.scoreTCEandNLSE aRegressLambda)  ;;score updates aRegressLambda with results against TRAINING set

	(setq N (length aRegressLambda.Mvl.myW)) ;;coefficient vector
	;;Construct LinearWWF
	(setq aText (append "y = " aRegressLambda.A))
	(loop for n from 0 until N do
		(setq aText (append aText " + (" (* aRegressLambda.Mvl.myW[n] aRegressLambda.B) " * " aTranslationTable[n] ")"))
	)

    (setq aRecord (aStatisticsCursor.getNewRecord))
	(setq aRecord.Desc "Linear")
	(setq aRecord.Data "Training")
	(setq aRecord.NLSE aRegressLambda.NLSE)
	(setq aRecord.TCE aRegressLambda.TCE)
	(setq aRecord.RSQ aRegressLambda.RSQ)
	(setq aRecord.WFF aText)
	(aStatisticsCursor.writeLast aRecord)
	
	(gsm.scoreTCEandNLSE aRegressLambda aTestX aTestY) ;;score updates aRegressLambda with results against TESTING set

    (setq aRecord (aStatisticsCursor.getNewRecord))
	(setq aRecord.Desc "Linear")
	(setq aRecord.Data "Testing")
	(setq aRecord.NLSE aRegressLambda.NLSE)
	(setq aRecord.TCE aRegressLambda.TCE)
	(setq aRecord.RSQ aRegressLambda.RSQ)
	(setq aRecord.WFF aText)
	(aStatisticsCursor.writeLast aRecord)

	;;Insert linear testing estimates into aEstimatesCursor 
	(setq aTestingEST (aRegressLambda.run aTestX)) ;;generate estimates
	(setq R (length aTestingEST))
	(loop for r from 0 until R do
		(setq aEstimatesCursor.rowVector[r][1] aTestingEST[r])
	)

	;; Generate Estimates, Statistics entries for each champion
	;; Get list of Champions -- gsm returns up to 25 champions.
	(setq aChampions (gsm.getEstimatorChampions false aTranslationTable))
	(if (> aNumChampions (length aChampions)) (setq aNumChampions (length aChampions)))
	(loop for c from 0 until aNumChampions do
		(setq aRegressLambda gsm.myPopulation[c]) ;;get regression lambda for champion
		(gsm.scoreTCEandNLSE aRegressLambda); score updates aRegressLambda with results against TRAINING set
	    (setq aRecord (aStatisticsCursor.getNewRecord))
		(setq aRecord.Desc (append "NonLinear" (right (append "00" (+ c 1)) 2)))
		(setq aRecord.Data "Training")
		(setq aRecord.NLSE aRegressLambda.NLSE)
		(setq aRecord.TCE aRegressLambda.TCE)
		(setq aRecord.RSQ aRegressLambda.RSQ)
		(setq aRecord.WFF aChampions[c])
		(aStatisticsCursor.writeLast aRecord)


		(gsm.scoreTCEandNLSE aRegressLambda aTestX aTestY); score updates aRegressLambda with results against TESTING set
	    (setq aRecord (aStatisticsCursor.getNewRecord))
		(setq aRecord.Desc (append "NonLinear" (right (append "00" (+ c 1)) 2)))
		(setq aRecord.Data "Testing")
		(setq aRecord.NLSE aRegressLambda.NLSE)
		(setq aRecord.TCE aRegressLambda.TCE)
		(setq aRecord.RSQ aRegressLambda.RSQ)
		(setq aRecord.WFF aChampions[c])
		(aStatisticsCursor.writeLast aRecord)

		;;Insert non-linear testing estimates into aEstimatesCursor 
		(setq aTestingEST (aRegressLambda.run aTestX)) ;;generate estimates
		(setq R (length aTestingEST))
		(loop for r from 0 until R do
			(setq aEstimatesCursor.rowVector[r][(+ c 2)] aTestingEST[r]) ;;Offset column by 2 for ID and Y00 (linear estimate)
		)

	);c

	(setq aEndTime (getTickCount aStartTime))

	;;Insert Results data
	(setq aRecord (aResultsCursor.getNewRecord))
	(setq aRecord.TestName aTestName)
	(setq aRecord.Generations gsm.myGc)
	(setq aRecord.Time (/ aEndTime 3600))
	(aResultsCursor.writeLast aRecord)

	;;Export all output tables
	(aEstimatesCursor.exportTab aEstimatesFilename)
	(aStatisticsCursor.exportTab aStatisticsFilename)
	(aResultsCursor.exportTab aResultsFilename)


	EXIT::  ;;Jump target of various error conditions.

	(writeln "Generations = " gsm.myGc)
	(writeln "Time = " (/ aEndTime 3600))
	(writeln "Regression finished. The following files were generated:")
	(writeln aEstimatesFilename)
	(writeln aStatisticsFilename)
	(writeln aResultsFilename)

true)




;;**EXPORTKEY**:gsmDemo.%DynamicCrossCorrelationRegressionData
;#text#
(lambda(...)
;; ******************************************************************************
;; summary:  This unnamed lambda is an example of a dynamic data generation
;;           program which will create a new dynamic regression matrix every
;;           time it is run. 
;;
;; Args:     none		
;;
;; Return:   theData		A Structure containing TX, TY, X, and Y regression data.
;;
;; Notes:    TX				The training independent variable matrix.
;;           TY             The training dependent variable vector.
;;           X				The testing independent variable matrix.
;;           Y              The testing dependent variable vector.
;;
;; Sample Usage:
;;    (setq aTemplate (mid (browseLib.checkout GSMDemo: "gsmDemo:%DynamicCrossCorrelationRegressionData") 7 1000000))    
;;    (setq aDataLambda (substitute aTemplate "$NOISE$" ".10"))
;;    (setq aDataLambda (substitute aDataLambda "$COLS$" "5"))
;;    (setq aDataLambda (substitute aDataLambda "$ROWS$" "1000"))
;;    (setq aDataLambda (eval (compile (lisp aDataLambda))))
;;    (setq aJobData (aDataLambda))
;; ******************************************************************************
    regs:(m mm M n nn N)
	vars:(theData (noise $NOISE$) (MM $COLS$) (NN $ROWS$) (seed 29183465.0))
    vars:(modelSource modelString modelLambda)
    vars:(x X y Y C)

    ;; Initialize and set up the final data structure to be returned.
    (gsm.setOptions default: 0% false)    
    (gsm.setRandomSeed seed)
    (setq N NN)(if (< N 1000) (setq N 1000))
    (setq M MM)(if (< M 3)(setq M 3))
    (if (< noise 0.0) (setq noise 0.0))
    (if (> noise .40) (setq noise .40))
    (setq theData (new Structure: TX: #void TY: #void X: #void Y: #void N: N M: M Noise: noise Seed: seed Model:#void)) 

    ;; ****************************************************************************************************************************
    ;; Create the training data
    ;; Note: Should match Cross Correlation test case published in "Genetic Programming Theory and Practive V" Springer Verlag 2008
    ;; ****************************************************************************************************************************
    (setq X (new Vector: Object: N))
    (setq Y (new Vector: Number: N))
    (setq C (new Vector: Number: M))

    ;; Create the constants to be used in model construction
    (loop for m from 0 until M do
       (cond 
	     ((= m 0) (setq C[m] -9.16))
	     ((= m 1) (setq C[m] -19.56))
	     ((= m 2) (setq C[m] 21.87))
	     ((= m 3) (setq C[m] -17.48))
	     ((= m 4) (setq C[m] 38.81))
	     (else (setq C[m] (- (gsm.myRandomFunction 100.0) 50.0)))
         ) ; end cond
	    ) ; end C loop

    ;; Compile the model which will be used to generate the dependent variables.
    (setq modelSource (append "model(" (string C[0])))
    (loop for m from 0 until M do
       (cond 
	     ((= m 0) (setq modelSource (append modelSource " + (" C[m] "*" (gsm.eGrammar.ruleName m) "*" (gsm.eGrammar.ruleName m) "*" (gsm.eGrammar.ruleName m) ")")))
	     ((= m 1) (setq modelSource (append modelSource " + (" C[m] "*" (gsm.eGrammar.ruleName (- m 1)) "*" (gsm.eGrammar.ruleName m) "*" (gsm.eGrammar.ruleName m) ")")))
	     (else (setq modelSource (append modelSource " + (" C[m] "*" (gsm.eGrammar.ruleName (- m 2)) "*" (gsm.eGrammar.ruleName (- m 1)) "*" (gsm.eGrammar.ruleName m) ")")))
        ) ; end cond
	   ) ; end C loop
    (setq modelSource (append modelSource ");"))
    (setq modelLambda (gsm.compileEstimator modelSource))
    (setq modelString modelSource)
    (setq modelString (mid modelString 7 10000000)) 
	(setq modelString (append "y = " modelString))
    (setq theData.Model modelString)

	;; Build training independendant data randomly and use the training model to compute the dependent data.
    (setq nn 0)
	(loop for n from 0 until N do
       (setq X[nn] (setq x (new Vector: Number: M)))
	   (loop for m from 0 until M do
		  (setq x[m] (- (gsm.myRandomFunction 100.0) 50.0))
		  ) ; end M loop
       (setq y (modelLambda x))
	   (setq Y[nn] (setq y (+ (* y (- 1.0 (* .50 noise))) (* y (gsm.myRandomFunction noise)))))
       (setCdr x y)
       (++ nn)
	   ) ; end N loop
    (setq theData.TX X)
    (setq theData.TY Y)

    (setq X (new Vector: Object: N))
    (setq Y (new Vector: Number: N))

	;; Build testing independendant data randomly and use the training model to compute the dependent data.
    (setq nn 0)
	(loop for n from 0 until N do
       (setq X[nn] (setq x (new Vector: Number: M)))
	   (loop for m from 0 until M do
		  (setq x[m] (- (gsm.myRandomFunction 100.0) 50.0))
		  ) ; end M loop
       (setq y (modelLambda x))
	   (setq Y[nn] (setq y (+ (* y (- 1.0 (* .50 noise))) (* y (gsm.myRandomFunction noise)))))
       (setCdr x y)
       (++ nn)
	   ) ; end N loop
    (setq theData.X X)
    (setq theData.Y Y)

    ;; Return the dynamic data generated herein.
    theData) ; end Dynamic Regress Data unnamed lambda






;;**EXPORTKEY**:gsmDemo.%DynamicCubicRegressionData
;#text#
(lambda(...)
;; ******************************************************************************
;; summary:  This unnamed lambda is an example of a dynamic data generation
;;           program which will create a new dynamic regression matrix every
;;           time it is run. 
;;
;; Args:     none		
;;
;; Return:   theData		A Structure containing TX, TY, X, and Y regression data.
;;
;; Notes:    TX				The training independent variable matrix.
;;           TY             The training dependent variable vector.
;;           X				The testing independent variable matrix.
;;           Y              The testing dependent variable vector.
;;
;; Sample Usage:
;;    (setq aTemplate (mid (browseLib.checkout GSMDemo: "gsmDemo:%DynamicCubicRegressionData") 7 1000000))    
;;    (setq aDataLambda (substitute aTemplate "$NOISE$" ".10"))
;;    (setq aDataLambda (substitute aDataLambda "$COLS$" "5"))
;;    (setq aDataLambda (substitute aDataLambda "$ROWS$" "1000"))
;;    (setq aDataLambda (eval (compile (lisp aDataLambda))))
;;    (setq aJobData (aDataLambda))
;; ******************************************************************************
    regs:(m mm M n nn N)
	vars:(theData (noise $NOISE$) (MM $COLS$) (NN $ROWS$) (seed 29183465.0))
    vars:(modelSource modelString modelLambda)
    vars:(x X y Y C)

    ;; Initialize and set up the final data structure to be returned.
    (gsm.setOptions default: 0% false)    
    (gsm.setRandomSeed seed)
    (setq N NN)(if (< N 1000) (setq N 1000))
    (setq M MM)(if (< M 3)(setq M 3))
    (if (< noise 0.0) (setq noise 0.0))
    (if (> noise .40) (setq noise .40))
    (setq theData (new Structure: TX: #void TY: #void X: #void Y: #void N: N M: M Noise: noise Seed: seed Model:#void)) 

    ;; *****************************************************************************************************************
    ;; Create the training data
    ;; Note: Should match test case published in "Genetic Programming Theory and Practive V" Springer Verlag 2008
    ;; *****************************************************************************************************************
    (setq X (new Vector: Object: N))
    (setq Y (new Vector: Number: N))
    (setq C (new Vector: Number: M))

    ;; Create the constants to be used in model construction
    (loop for m from 0 until M do
       (cond 
	     ((= m 0) (setq C[m] 1.57))
	     ((= m 1) (setq C[m] -39.34))
	     ((= m 2) (setq C[m] 2.13))
	     ((= m 3) (setq C[m] 46.59))
	     ((= m 4) (setq C[m] 11.54))
	     (else (setq C[m] (- (gsm.myRandomFunction 100.0) 50.0)))
         ) ; end cond
	    ) ; end C loop

    ;; Compile the model which will be used to generate the dependent variables.
    (setq modelSource (append "model(" (string C[0])))
    (loop for m from 0 until M do
       (setq modelSource (append modelSource " + (" C[m] "*" (gsm.eGrammar.ruleName m) "*" (gsm.eGrammar.ruleName m) "*" (gsm.eGrammar.ruleName m) ")"))
	   ) ; end C loop
    (setq modelSource (append modelSource ");"))
    (setq modelLambda (gsm.compileEstimator modelSource))
    (setq modelString modelSource)
    (setq modelString (mid modelString 7 10000000)) 
	(setq modelString (append "y = " modelString))
    (setq theData.Model modelString)

	;; Build training independendant data randomly and use the training model to compute the dependent data.
    (setq nn 0)
	(loop for n from 0 until N do
       (setq X[nn] (setq x (new Vector: Number: M)))
	   (loop for m from 0 until M do
		  (setq x[m] (- (gsm.myRandomFunction 100.0) 50.0))
		  ) ; end M loop
       (setq y (modelLambda x))
	   (setq Y[nn] (setq y (+ (* y (- 1.0 (* .50 noise))) (* y (gsm.myRandomFunction noise)))))
       (setCdr x y)
       (++ nn)
	   ) ; end N loop
    (setq theData.TX X)
    (setq theData.TY Y)

    (setq X (new Vector: Object: N))
    (setq Y (new Vector: Number: N))
	;; Build testing independendant data randomly and use the training model to compute the dependent data.
    (setq nn 0)
	(loop for n from 0 until N do
       (setq X[nn] (setq x (new Vector: Number: M)))
	   (loop for m from 0 until M do
		  (setq x[m] (- (gsm.myRandomFunction 100.0) 50.0))
		  ) ; end M loop
       (setq y (modelLambda x))
	   (setq Y[nn] (setq y (+ (* y (- 1.0 (* .50 noise))) (* y (gsm.myRandomFunction noise)))))
       (setCdr x y)
       (++ nn)
	   ) ; end N loop
    (setq theData.X X)
    (setq theData.Y Y)

    ;; Return the dynamic data generated herein.
    theData) ; end Dynamic Regress Data unnamed lambda



;;**EXPORTKEY**:gsmDemo.%DynamicCyclicSeriesRegressionData
;#text#
(lambda(...)
;; ******************************************************************************
;; summary:  This unnamed lambda is an example of a dynamic data generation
;;           program which will create a new dynamic regression matrix every
;;           time it is run. 
;;
;; Args:     none		
;;
;; Return:   theData		A Structure containing TX, TY, X, and Y regression data.
;;
;; Notes:    TX				The training independent variable matrix.
;;           TY             The training dependent variable vector.
;;           X				The testing independent variable matrix.
;;           Y              The testing dependent variable vector.
;;
;; Sample Usage:
;;    (setq aTemplate (mid (browseLib.checkout GSMDemo: "gsmDemo:%DynamicCyclicSeriesRegressionData") 7 1000000))    
;;    (setq aDataLambda (substitute aTemplate "$NOISE$" ".10"))
;;    (setq aDataLambda (substitute aDataLambda "$COLS$" "5"))
;;    (setq aDataLambda (substitute aDataLambda "$ROWS$" "1000"))
;;    (setq aDataLambda (eval (compile (lisp aDataLambda))))
;;    (setq aJobData (aDataLambda))
;; ******************************************************************************
    regs:(m mm M n nn N)
	vars:(theData (noise $NOISE$) (MM $COLS$) (NN $ROWS$) (seed 29183465.0))
    vars:(modelSource modelString modelLambda)
    vars:(x X y Y C)

    ;; Initialize and set up the final data structure to be returned.
    (gsm.setOptions default: 0% false)    
    (gsm.setRandomSeed seed)
    (setq N NN)(if (< N 1000) (setq N 1000))
    (setq M MM)(if (< M 3)(setq M 3))
    (if (< noise 0.0) (setq noise 0.0))
    (if (> noise .40) (setq noise .40))
    (setq theData (new Structure: TX: #void TY: #void X: #void Y: #void N: N M: M Noise: noise Seed: seed Model:#void)) 

    ;; ****************************************************************************************************************************
    ;; Create the training data
    ;; Note: Should match Cross Correlation test case published in "Genetic Programming Theory and Practive V" Springer Verlag 2008
    ;; ****************************************************************************************************************************
    (setq X (new Vector: Object: N))
    (setq Y (new Vector: Number: N))
    (setq C (new Vector: Number: M))

    ;; Create the constants to be used in model construction
    (loop for m from 0 until M do
       (cond 
	     ((= m 0) (setq C[m] 14.65))
	     ((= m 1) (setq C[m] -6.73))
	     ((= m 2) (setq C[m] -18.35))
	     ((= m 3) (setq C[m] -40.32))
	     ((= m 4) (setq C[m] -4.43))
	     (else (setq C[m] (- (gsm.myRandomFunction 100.0) 50.0)))
         ) ; end cond
	    ) ; end C loop

    ;; Compile the model which will be used to generate the dependent variables.
    (setq modelSource (append "model(" (string C[0])))
    (loop for m from 0 until Mp1 do
       (cond
          ((= (modi m 3) 0) (setq modelSource (append modelSource " + (" C[m] "*sin(" (gsm.eGrammar.ruleName m) "))")))
          ((= (modi m 3) 1) (setq modelSource (append modelSource " + (" C[m] "*cos(" (gsm.eGrammar.ruleName m) "))")))
          ((= (modi m 3) 2) (setq modelSource (append modelSource " + (" C[m] "*tan(" (gsm.eGrammar.ruleName m) "))")))
          (else (error "gsm.selfTest: invalid cyclicSeries condition during model building"))
       ) ; end cond
    ) ; end C loop
    (setq modelSource (append modelSource ");"))
    (setq modelLambda (gsm.compileEstimator modelSource))
    (setq modelString modelSource)
    (setq modelString (mid modelString 7 10000000)) 
	(setq modelString (append "y = " modelString))
    (setq theData.Model modelString)

	;; Build training independendant data randomly and use the training model to compute the dependent data.
    (setq nn 0)
	(loop for n from 0 until N do
       (setq X[nn] (setq x (new Vector: Number: M)))
	   (loop for m from 0 until M do
		  (setq x[m] (- (gsm.myRandomFunction 100.0) 50.0))
		  ) ; end M loop
       (setq y (modelLambda x))
	   (setq Y[nn] (setq y (+ (* y (- 1.0 (* .50 noise))) (* y (gsm.myRandomFunction noise)))))
       (setCdr x y)
       (++ nn)
	   ) ; end N loop
    (setq theData.TX X)
    (setq theData.TY Y)

    (setq X (new Vector: Object: N))
    (setq Y (new Vector: Number: N))
	;; Build testing independendant data randomly and use the training model to compute the dependent data.
    (setq nn 0)
	(loop for n from 0 until N do
       (setq X[nn] (setq x (new Vector: Number: M)))
	   (loop for m from 0 until M do
		  (setq x[m] (- (gsm.myRandomFunction 100.0) 50.0))
		  ) ; end M loop
       (setq y (modelLambda x))
	   (setq Y[nn] (setq y (+ (* y (- 1.0 (* .50 noise))) (* y (gsm.myRandomFunction noise)))))
       (setCdr x y)
       (++ nn)
	   ) ; end N loop
    (setq theData.X X)
    (setq theData.Y Y)

    ;; Return the dynamic data generated herein.
    theData) ; end Dynamic Regress Data unnamed lambda


;;**EXPORTKEY**:gsmDemo.%DynamicElipsoidRegressionData
;#text#
(lambda(...)
;; ******************************************************************************
;; summary:  This unnamed lambda is an example of a dynamic data generation
;;           program which will create a new dynamic regression matrix every
;;           time it is run. 
;;
;; Args:     none		
;;
;; Return:   theData		A Structure containing TX, TY, X, and Y regression data.
;;
;; Notes:    TX				The training independent variable matrix.
;;           TY             The training dependent variable vector.
;;           X				The testing independent variable matrix.
;;           Y              The testing dependent variable vector.
;;
;; Sample Usage:
;;    (setq aTemplate (mid (browseLib.checkout GSMDemo: "gsmDemo:%DynamicElipsoidRegressionData") 7 1000000))    
;;    (setq aDataLambda (substitute aTemplate "$NOISE$" ".10"))
;;    (setq aDataLambda (substitute aDataLambda "$COLS$" "5"))
;;    (setq aDataLambda (substitute aDataLambda "$ROWS$" "1000"))
;;    (setq aDataLambda (eval (compile (lisp aDataLambda))))
;;    (setq aJobData (aDataLambda))
;; ******************************************************************************
    regs:(m mm M n nn N)
	vars:(theData (noise $NOISE$) (MM $COLS$) (NN $ROWS$) (seed 29183465.0))
    vars:(modelSource modelString modelLambda)
    vars:(x X y Y C)

    ;; Initialize and set up the final data structure to be returned.
    (gsm.setOptions default: 0% false)    
    (gsm.setRandomSeed seed)
    (setq N NN)(if (< N 1000) (setq N 1000))
    (setq M MM)(if (< M 3)(setq M 3))
    (if (< noise 0.0) (setq noise 0.0))
    (if (> noise .40) (setq noise .40))
    (setq theData (new Structure: TX: #void TY: #void X: #void Y: #void N: N M: M Noise: noise Seed: seed Model:#void)) 

    ;; *****************************************************************************************************************
    ;; Create the training data
    ;; Note: Should match test case published in "Genetic Programming Theory and Practive V" Springer Verlag 2008
    ;; *****************************************************************************************************************
    (setq X (new Vector: Object: N))
    (setq Y (new Vector: Number: N))
    (setq C (new Vector: Number: M))

    ;; Create the constants to be used in model construction
    (loop for m from 0 until M do
       (cond 
         ((= m 0) (setq C[m] 0.0))
         ((= m 1) (setq C[m] 1.0))
         ((= m 2) (setq C[m] 2.0))
         ((= m 3) (setq C[m] 3.0))
         ((= m 4) (setq C[m] 4.0))
	     (else (setq C[m] (- (gsm.myRandomFunction 100.0) 50.0)))
         ) ; end cond
	    ) ; end C loop

    ;; Compile the model which will be used to generate the dependent variables.
    (setq modelSource (append "model(" (string C[0])))
    (loop for m from 0 until M do
       (setq modelSource (append modelSource " + (" C[m]  "*" (gsm.eGrammar.ruleName m) "*" (gsm.eGrammar.ruleName m) ")"))
	   ) ; end C loop
    (setq modelSource (append modelSource ");"))
    (setq modelLambda (gsm.compileEstimator modelSource))
    (setq modelString modelSource)
    (setq modelString (mid modelString 7 10000000)) 
	(setq modelString (append "y = " modelString))
    (setq theData.Model modelString)

	;; Build training independendant data randomly and use the training model to compute the dependent data.
    (setq nn 0)
	(loop for n from 0 until N do
       (setq X[nn] (setq x (new Vector: Number: M)))
	   (loop for m from 0 until M do
		  (setq x[m] (- (gsm.myRandomFunction 100.0) 50.0))
		  ) ; end M loop
       (setq y (modelLambda x))
	   (setq Y[nn] (setq y (+ (* y (- 1.0 (* .50 noise))) (* y (gsm.myRandomFunction noise)))))
       (setCdr x y)
       (++ nn)
	   ) ; end N loop
    (setq theData.TX X)
    (setq theData.TY Y)

    (setq X (new Vector: Object: N))
    (setq Y (new Vector: Number: N))
	;; Build testing independendant data randomly and use the training model to compute the dependent data.
    (setq nn 0)
	(loop for n from 0 until N do
       (setq X[nn] (setq x (new Vector: Number: M)))
	   (loop for m from 0 until M do
		  (setq x[m] (- (gsm.myRandomFunction 100.0) 50.0))
		  ) ; end M loop
       (setq y (modelLambda x))
	   (setq Y[nn] (setq y (+ (* y (- 1.0 (* .50 noise))) (* y (gsm.myRandomFunction noise)))))
       (setCdr x y)
       (++ nn)
	   ) ; end N loop
    (setq theData.X X)
    (setq theData.Y Y)

    ;; Return the dynamic data generated herein.
    theData) ; end Dynamic Regress Data unnamed lambda


;;**EXPORTKEY**:gsmDemo.%DynamicHiddenModelRegressionData
;#text#
(lambda(...)
;; ******************************************************************************
;; summary:  This unnamed lambda is an example of a dynamic data generation
;;           program which will create a new dynamic regression matrix every
;;           time it is run. 
;;
;; Args:     none		
;;
;; Return:   theData		A Structure containing TX, TY, X, and Y regression data.
;;
;; Notes:    TX				The training independent variable matrix.
;;           TY             The training dependent variable vector.
;;           X				The testing independent variable matrix.
;;           Y              The testing dependent variable vector.
;;
;; Sample Usage:
;;    (setq aTemplate (mid (browseLib.checkout GSMDemo: "gsmDemo:%DynamicHiddenModelRegressionData") 7 1000000))    
;;    (setq aDataLambda (substitute aTemplate "$NOISE$" ".10"))
;;    (setq aDataLambda (substitute aDataLambda "$COLS$" "5"))
;;    (setq aDataLambda (substitute aDataLambda "$ROWS$" "1000"))
;;    (setq aDataLambda (eval (compile (lisp aDataLambda))))
;;    (setq aJobData (aDataLambda))
;; ******************************************************************************
    regs:(m mm M n nn N)
	vars:(theData (noise $NOISE$) (MM $COLS$) (NN $ROWS$) (seed 29183465.0))
    vars:(modelSource modelString modelLambda)
    vars:(x X y Y C)

    ;; Initialize and set up the final data structure to be returned.
    (gsm.setOptions default: 0% false)    
    (gsm.setRandomSeed seed)
    (setq N NN)(if (< N 1000) (setq N 1000))
    (setq M MM)(if (< M 3)(setq M 3))
    (if (< noise 0.0) (setq noise 0.0))
    (if (> noise .40) (setq noise .40))
    (setq theData (new Structure: TX: #void TY: #void X: #void Y: #void N: N M: M Noise: noise Seed: seed Model:#void)) 

    ;; *****************************************************************************************************************
    ;; Create the training data
    ;; Note: Should match test case published in "Genetic Programming Theory and Practive V" Springer Verlag 2008
    ;; *****************************************************************************************************************
    (setq X (new Vector: Object: N))
    (setq Y (new Vector: Number: N))
    (setq C (new Vector: Number: M))

    ;; Create the constants to be used in model construction
    (loop for m from 0 until M do
       (cond 
         ((= m 0) (setq C[m] 1.57))
         ((= m 1) (setq C[m] -39.34))
         ((= m 2) (setq C[m] 2.13))
         ((= m 3) (setq C[m] 46.59))
         ((= m 4) (setq C[m] 11.54))
	     (else (setq C[m] (- (gsm.myRandomFunction 100.0) 50.0)))
         ) ; end cond
	    ) ; end C loop

    ;; Compile the model which will be used to generate the dependent variables.
    (setq modelSource (append "model(" (string C[0])))
    (loop for m from 0 until M do
       (if (= (divi Mp1 2) m) (setq modelSource (append modelSource " + (" C[m] "*sin(" (gsm.eGrammar.ruleName m) "))")))
	   ) ; end C loop
    (setq modelSource (append modelSource ");"))
    (setq modelLambda (gsm.compileEstimator modelSource))
    (setq modelString modelSource)
    (setq modelString (mid modelString 7 10000000)) 
	(setq modelString (append "y = " modelString))
    (setq theData.Model modelString)

	;; Build training independendant data randomly and use the training model to compute the dependent data.
    (setq nn 0)
	(loop for n from 0 until N do
       (setq X[nn] (setq x (new Vector: Number: M)))
	   (loop for m from 0 until M do
		  (setq x[m] (- (gsm.myRandomFunction 100.0) 50.0))
		  ) ; end M loop
       (setq y (modelLambda x))
	   (setq Y[nn] (setq y (+ (* y (- 1.0 (* .50 noise))) (* y (gsm.myRandomFunction noise)))))
       (setCdr x y)
       (++ nn)
	   ) ; end N loop
    (setq theData.TX X)
    (setq theData.TY Y)

    (setq X (new Vector: Object: N))
    (setq Y (new Vector: Number: N))
	;; Build testing independendant data randomly and use the training model to compute the dependent data.
    (setq nn 0)
	(loop for n from 0 until N do
       (setq X[nn] (setq x (new Vector: Number: M)))
	   (loop for m from 0 until M do
		  (setq x[m] (- (gsm.myRandomFunction 100.0) 50.0))
		  ) ; end M loop
       (setq y (modelLambda x))
	   (setq Y[nn] (setq y (+ (* y (- 1.0 (* .50 noise))) (* y (gsm.myRandomFunction noise)))))
       (setCdr x y)
       (++ nn)
	   ) ; end N loop
    (setq theData.X X)
    (setq theData.Y Y)

    ;; Return the dynamic data generated herein.
    theData) ; end Dynamic Regress Data unnamed lambda


;;**EXPORTKEY**:gsmDemo.%DynamicHyperTangentRegressionData
;#text#
(lambda(...)
;; ******************************************************************************
;; summary:  This unnamed lambda is an example of a dynamic data generation
;;           program which will create a new dynamic regression matrix every
;;           time it is run. 
;;
;; Args:     none		
;;
;; Return:   theData		A Structure containing TX, TY, X, and Y regression data.
;;
;; Notes:    TX				The training independent variable matrix.
;;           TY             The training dependent variable vector.
;;           X				The testing independent variable matrix.
;;           Y              The testing dependent variable vector.
;;
;; Sample Usage:
;;    (setq aTemplate (mid (browseLib.checkout GSMDemo: "gsmDemo:%DynamicHyperTangentRegressionData") 7 1000000))    
;;    (setq aDataLambda (substitute aTemplate "$NOISE$" ".10"))
;;    (setq aDataLambda (substitute aDataLambda "$COLS$" "5"))
;;    (setq aDataLambda (substitute aDataLambda "$ROWS$" "1000"))
;;    (setq aDataLambda (eval (compile (lisp aDataLambda))))
;;    (setq aJobData (aDataLambda))
;; ******************************************************************************
    regs:(m mm M n nn N)
	vars:(theData (noise $NOISE$) (MM $COLS$) (NN $ROWS$) (seed 29183465.0))
    vars:(modelSource modelString modelLambda)
    vars:(x X y Y C)

    ;; Initialize and set up the final data structure to be returned.
    (gsm.setOptions default: 0% false)    
    (gsm.setRandomSeed seed)
    (setq N NN)(if (< N 1000) (setq N 1000))
    (setq M MM)(if (< M 3)(setq M 3))
    (if (< noise 0.0) (setq noise 0.0))
    (if (> noise .40) (setq noise .40))
    (setq theData (new Structure: TX: #void TY: #void X: #void Y: #void N: N M: M Noise: noise Seed: seed Model:#void)) 

    ;; *****************************************************************************************************************
    ;; Create the training data
    ;; Note: Should match test case published in "Genetic Programming Theory and Practive V" Springer Verlag 2008
    ;; *****************************************************************************************************************
    (setq X (new Vector: Object: N))
    (setq Y (new Vector: Number: N))
    (setq C (new Vector: Number: M))

    ;; Create the constants to be used in model construction
    (loop for m from 0 until M do
       (cond 
         ((= m 0) (setq C[m] 1.57))
         ((= m 1) (setq C[m] -39.34))
         ((= m 2) (setq C[m] 2.13))
         ((= m 3) (setq C[m] 46.59))
         ((= m 4) (setq C[m] 11.54))
	     (else (setq C[m] (- (gsm.myRandomFunction 100.0) 50.0)))
         ) ; end cond
	    ) ; end C loop

    ;; Compile the model which will be used to generate the dependent variables.
    (setq modelSource (append "model(" (string C[0])))
    (loop for m from 0 until M do
       (setq modelSource (append modelSource " + (" C[m]  "*tanh(" (gsm.eGrammar.ruleName m) "*" (gsm.eGrammar.ruleName m) "*" (gsm.eGrammar.ruleName m) "))"))
	   ) ; end C loop
    (setq modelSource (append modelSource ");"))
    (setq modelLambda (gsm.compileEstimator modelSource))
    (setq modelString modelSource)
    (setq modelString (mid modelString 7 10000000)) 
	(setq modelString (append "y = " modelString))
    (setq theData.Model modelString)

	;; Build training independendant data randomly and use the training model to compute the dependent data.
    (setq nn 0)
	(loop for n from 0 until N do
       (setq X[nn] (setq x (new Vector: Number: M)))
	   (loop for m from 0 until M do
		  (setq x[m] (- (gsm.myRandomFunction 100.0) 50.0))
		  ) ; end M loop
       (setq y (modelLambda x))
	   (setq Y[nn] (setq y (+ (* y (- 1.0 (* .50 noise))) (* y (gsm.myRandomFunction noise)))))
       (setCdr x y)
       (++ nn)
	   ) ; end N loop
    (setq theData.TX X)
    (setq theData.TY Y)

    (setq X (new Vector: Object: N))
    (setq Y (new Vector: Number: N))
	;; Build testing independendant data randomly and use the training model to compute the dependent data.
    (setq nn 0)
	(loop for n from 0 until N do
       (setq X[nn] (setq x (new Vector: Number: M)))
	   (loop for m from 0 until M do
		  (setq x[m] (- (gsm.myRandomFunction 100.0) 50.0))
		  ) ; end M loop
       (setq y (modelLambda x))
	   (setq Y[nn] (setq y (+ (* y (- 1.0 (* .50 noise))) (* y (gsm.myRandomFunction noise)))))
       (setCdr x y)
       (++ nn)
	   ) ; end N loop
    (setq theData.X X)
    (setq theData.Y Y)

    ;; Return the dynamic data generated herein.
    theData) ; end Dynamic Regress Data unnamed lambda


;;**EXPORTKEY**:gsmDemo.%DynamicLinearRegressionData
;#text#
(lambda(...)
;; ******************************************************************************
;; summary:  This unnamed lambda is an example of a dynamic data generation
;;           program which will create a new dynamic regression matrix every
;;           time it is run. 
;;
;; Args:     none		
;;
;; Return:   theData		A Structure containing TX, TY, X, and Y regression data.
;;
;; Notes:    TX				The training independent variable matrix.
;;           TY             The training dependent variable vector.
;;           X				The testing independent variable matrix.
;;           Y              The testing dependent variable vector.
;;
;; Sample Usage:
;;    (setq aTemplate (mid (browseLib.checkout GSMDemo: "gsmDemo:%DynamicLinearRegressionData") 7 1000000))    
;;    (setq aDataLambda (substitute aTemplate "$NOISE$" ".10"))
;;    (setq aDataLambda (substitute aDataLambda "$COLS$" "5"))
;;    (setq aDataLambda (substitute aDataLambda "$ROWS$" "1000"))
;;    (setq aDataLambda (eval (compile (lisp aDataLambda))))
;;    (setq aJobData (aDataLambda))
;; ******************************************************************************
    regs:(m mm M n nn N)
	vars:(theData (noise $NOISE$) (MM $COLS$) (NN $ROWS$) (seed 29183465.0))
    vars:(modelSource modelString modelLambda)
    vars:(x X y Y C)

    ;; Initialize and set up the final data structure to be returned.
    (gsm.setOptions default: 0% false)    
    (gsm.setRandomSeed seed)
    (setq N NN)(if (< N 1000) (setq N 1000))
    (setq M MM)(if (< M 3)(setq M 3))
    (if (< noise 0.0) (setq noise 0.0))
    (if (> noise .40) (setq noise .40))
    (setq theData (new Structure: TX: #void TY: #void X: #void Y: #void N: N M: M Noise: noise Seed: seed Model:#void)) 

    ;; ****************************************************************************************************************************
    ;; Create the training data
    ;; Note: Should match test case published in "Genetic Programming Theory and Practive V" Springer Verlag 2008
    ;; ****************************************************************************************************************************
    (setq X (new Vector: Object: N))
    (setq Y (new Vector: Number: N))
    (setq C (new Vector: Number: M))

    ;; Create the constants to be used in model construction
    (loop for m from 0 until M do
       (cond 
	     ((= m 0) (setq C[m] 1.57))
         ((= m 1) (setq C[m] -39.34))
         ((= m 2) (setq C[m] 2.13))
         ((= m 3) (setq C[m] 46.59))
         ((= m 4) (setq C[m] 11.54))
	     (else (setq C[m] (- (gsm.myRandomFunction 100.0) 50.0)))
         ) ; end cond
	    ) ; end C loop

    ;; Compile the model which will be used to generate the dependent variables.
    (setq modelSource (append "model(" (string C[0])))
    (loop for m from 0 until M do
       (setq modelSource (append modelSource " + (" C[m] "*" (gsm.eGrammar.ruleName m) ")"))
	   ) ; end C loop
    (setq modelSource (append modelSource ");"))
    (setq modelLambda (gsm.compileEstimator modelSource))
    (setq modelString modelSource)
    (setq modelString (mid modelString 7 10000000)) 
	(setq modelString (append "y = " modelString))
    (setq theData.Model modelString)

	;; Build training independendant data randomly and use the training model to compute the dependent data.
    (setq nn 0)
	(loop for n from 0 until N do
       (setq X[nn] (setq x (new Vector: Number: M)))
	   (loop for m from 0 until M do
		  (setq x[m] (- (gsm.myRandomFunction 100.0) 50.0))
		  ) ; end M loop
       (setq y (modelLambda x))
	   (setq Y[nn] (setq y (+ (* y (- 1.0 (* .50 noise))) (* y (gsm.myRandomFunction noise)))))
       (setCdr x y)
       (++ nn)
	   ) ; end N loop
    (setq theData.TX X)
    (setq theData.TY Y)

    (setq X (new Vector: Object: N))
    (setq Y (new Vector: Number: N))
	;; Build testing independendant data randomly and use the training model to compute the dependent data.
    (setq nn 0)
	(loop for n from 0 until N do
       (setq X[nn] (setq x (new Vector: Number: M)))
	   (loop for m from 0 until M do
		  (setq x[m] (- (gsm.myRandomFunction 100.0) 50.0))
		  ) ; end M loop
       (setq y (modelLambda x))
	   (setq Y[nn] (setq y (+ (* y (- 1.0 (* .50 noise))) (* y (gsm.myRandomFunction noise)))))
       (setCdr x y)
       (++ nn)
	   ) ; end N loop
    (setq theData.X X)
    (setq theData.Y Y)

    ;; Return the dynamic data generated herein.
    theData) ; end Dynamic Regress Data unnamed lambda


;;**EXPORTKEY**:gsmDemo.%DynamicMixedModelRegressionData
;#text#
(lambda(...)
;; ******************************************************************************
;; summary:  This unnamed lambda is an example of a dynamic data generation
;;           program which will create a new dynamic regression matrix every
;;           time it is run. 
;;
;; Args:     none		
;;
;; Return:   theData		A Structure containing TX, TY, X, and Y regression data.
;;
;; Notes:    TX				The training independent variable matrix.
;;           TY             The training dependent variable vector.
;;           X				The testing independent variable matrix.
;;           Y              The testing dependent variable vector.
;;
;; Sample Usage:
;;    (setq aTemplate (mid (browseLib.checkout GSMDemo: "gsmDemo:%DynamicMixedModelRegressionData") 7 1000000))    
;;    (setq aDataLambda (substitute aTemplate "$NOISE$" ".10"))
;;    (setq aDataLambda (substitute aDataLambda "$COLS$" "5"))
;;    (setq aDataLambda (substitute aDataLambda "$ROWS$" "1000"))
;;    (setq aDataLambda (eval (compile (lisp aDataLambda))))
;;    (setq aJobData (aDataLambda))
;; ******************************************************************************
    regs:(m mm M n nn N)
	vars:(theData (noise $NOISE$) (MM $COLS$) (NN $ROWS$) (seed 29183465.0))
    vars:(modelSource modelString modelLambda)
    vars:(x X y Y C)

    ;; Initialize and set up the final data structure to be returned.
    (gsm.setOptions default: 0% false)    
    (gsm.setRandomSeed seed)
    (setq N NN)(if (< N 1000) (setq N 1000))
    (setq M MM)(if (< M 3)(setq M 3))
    (if (< noise 0.0) (setq noise 0.0))
    (if (> noise .40) (setq noise .40))
    (setq theData (new Structure: TX: #void TY: #void X: #void Y: #void N: N M: M Noise: noise Seed: seed Model:#void)) 

    ;; ****************************************************************************************************************************
    ;; Create the training data
    ;; Note: Should match Cross Correlation test case published in "Genetic Programming Theory and Practive V" Springer Verlag 2008
    ;; ****************************************************************************************************************************
    (setq X (new Vector: Object: N))
    (setq Y (new Vector: Number: N))
    (setq C (new Vector: Number: M))

    ;; Create the constants to be used in model construction
    (loop for m from 0 until M do
       (cond 
	     ((= m 0) (setq C[m] 1.57))
         ((= m 1) (setq C[m] -39.34))
         ((= m 2) (setq C[m] 2.13))
         ((= m 3) (setq C[m] 46.59))
         ((= m 4) (setq C[m] 11.54))
	     (else (setq C[m] (- (gsm.myRandomFunction 100.0) 50.0)))
         ) ; end cond
	    ) ; end C loop

    ;; Compile the model which will be used to generate the dependent variables.
    (setq modelSource "model(")
    (setq modelSource (append modelSource " if (abs(ninteger(" (gsm.eGrammar.ruleName 0) " % 4.0)) == 0.0) {" (string C[0])))
    (loop for m from 0 until Mp1 do
       ;; log model
       (setq modelSource (append modelSource " + (" C[m] "*log(.000001+abs(" (gsm.eGrammar.ruleName m) ")))"))
    ) ; end C loop
    (setq modelSource (append modelSource "} else "))
    (setq modelSource (append modelSource " if (abs(ninteger(" (gsm.eGrammar.ruleName 0) " % 4.0)) == 1.0) {" (string C[0])))
    (loop for m from 0 until Mp1 do
       ;; square model
       (setq modelSource (append modelSource " + (" C[m] "*" (gsm.eGrammar.ruleName m) "*" (gsm.eGrammar.ruleName m)")"))
    ) ; end C loop
    (setq modelSource (append modelSource "} else "))
    (setq modelSource (append modelSource " if (abs(ninteger(" (gsm.eGrammar.ruleName 0) " % 4.0)) == 2.0) {" (string C[0])))
    (loop for m from 0 until Mp1 do
       ;; sine model
       (setq modelSource (append modelSource " + (" C[m] "*sin(" (gsm.eGrammar.ruleName m) "))"))
    ) ; end C loop
    (setq modelSource (append modelSource "} else "))
    (setq modelSource (append modelSource " if (abs(ninteger(" (gsm.eGrammar.ruleName 0) " % 4.0)) == 3.0) {" (string C[0])))
    (loop for m from 0 until Mp1 do
       ;; linear model
       (setq modelSource (append modelSource " + (" C[m] "*" (gsm.eGrammar.ruleName m) ")"))
    ) ; end C loop
    (setq modelSource (append modelSource "});"))
    (setq modelLambda (gsm.compileEstimator modelSource))
    (setq modelString modelSource)
    (setq modelString (mid modelString 7 10000000)) 
	(setq modelString (append "y = " modelString))
    (setq theData.Model modelString)

	;; Build training independendant data randomly and use the training model to compute the dependent data.
    (setq nn 0)
	(loop for n from 0 until N do
       (setq X[nn] (setq x (new Vector: Number: M)))
	   (loop for m from 0 until M do
		  (setq x[m] (- (gsm.myRandomFunction 100.0) 50.0))
		  ) ; end M loop
       (setq y (modelLambda x))
	   (setq Y[nn] (setq y (+ (* y (- 1.0 (* .50 noise))) (* y (gsm.myRandomFunction noise)))))
       (setCdr x y)
       (++ nn)
	   ) ; end N loop
    (setq theData.TX X)
    (setq theData.TY Y)

    (setq X (new Vector: Object: N))
    (setq Y (new Vector: Number: N))
	;; Build testing independendant data randomly and use the training model to compute the dependent data.
    (setq nn 0)
	(loop for n from 0 until N do
       (setq X[nn] (setq x (new Vector: Number: M)))
	   (loop for m from 0 until M do
		  (setq x[m] (- (gsm.myRandomFunction 100.0) 50.0))
		  ) ; end M loop
       (setq y (modelLambda x))
	   (setq Y[nn] (setq y (+ (* y (- 1.0 (* .50 noise))) (* y (gsm.myRandomFunction noise)))))
       (setCdr x y)
       (++ nn)
	   ) ; end N loop
    (setq theData.X X)
    (setq theData.Y Y)

    ;; Return the dynamic data generated herein.
    theData) ; end Dynamic Regress Data unnamed lambda


;;**EXPORTKEY**:gsmDemo.%DynamicRandomModalRegressionData
;#text#
(lambda(...)
;; ******************************************************************************
;; summary:  This unnamed lambda is an example of a dynamic data generation
;;           program which will create a new dynamic regression matrix every
;;           time it is run. 
;;
;; Args:     none		
;;
;; Return:   theData		A Structure containing TX, TY, X, and Y regression data.
;;
;; Notes:    TX				The training independent variable matrix.
;;           TY             The training dependent variable vector.
;;           X				The testing independent variable matrix.
;;           Y              The testing dependent variable vector.
;;
;; Sample Usage:
;;    (setq aTemplate (mid (browseLib.checkout GSMDemo: "gsmDemo:%DynamicRandomModalRegressionData") 7 1000000))    
;;    (setq aDataLambda (substitute aTemplate "$NOISE$" ".10"))
;;    (setq aDataLambda (substitute aDataLambda "$COLS$" "5"))
;;    (setq aDataLambda (substitute aDataLambda "$ROWS$" "1000"))
;;    (setq aDataLambda (eval (compile (lisp aDataLambda))))
;;    (setq aJobData (aDataLambda))
;; ******************************************************************************
    regs:(m mm M n nn N)
	vars:(theData (noise $NOISE$) (MM $COLS$) (NN $ROWS$) (seed 29183465.0))
    vars:(modelString modelLambda)
    vars:(x X y Y C)

    ;; Initialize and set up the final data structure to be returned.
    (gsm.setOptions default: 0% false)    
    (gsm.setRandomSeed seed)
    (setq N NN)(if (< N 1000) (setq N 1000))
    (setq M MM)(if (< M 3)(setq M 3))
    (if (< noise 0.0) (setq noise 0.0))
    (if (> noise .40) (setq noise .40))
    (setq theData (new Structure: TX: #void TY: #void X: #void Y: #void N: N M: M Noise: noise Seed: seed Model:#void)) 

    ;; ****************************************************************************************************************************
    ;; Create the training data
    ;; Note: Should match Cross Correlation test case published in "Genetic Programming Theory and Practive V" Springer Verlag 2008
    ;; ****************************************************************************************************************************
    (setq X (new Vector: Object: N))
    (setq Y (new Vector: Number: N))
    (setq C (new Vector: Number: M))

    (setq modelLambda (gsm.buildTrainingModel M true))
    (setq modelString modelLambda.WFF)
    (setq modelString (mid modelString 7 10000000))
	(setq modelString (append "y = " modelString))
    (setq theData.Model modelString)

	;; Build training independendant data randomly and use the training model to compute the dependent data.
    (setq nn 0)
	(loop for n from 0 until N do
       (setq X[nn] (setq x (new Vector: Number: M)))
	   (loop for m from 0 until M do
		  (setq x[m] (- (gsm.myRandomFunction 100.0) 50.0))
		  ) ; end M loop
       (setq y (modelLambda x))
       (if (>= y BIGPOSNUM) (goto Last:))
       (if (<= y BIGNEGUM) (goto Last:))
	   (setq Y[nn] (setq y (+ (* y (- 1.0 (* .50 noise))) (* y (gsm.myRandomFunction noise)))))
       (setCdr x y)
       (++ nn)
	   ) ; end N loop
    (setq theData.TX X)
    (setq theData.TY Y)

    (setq X (new Vector: Object: N))
    (setq Y (new Vector: Number: N))
	;; Build testing independendant data randomly and use the training model to compute the dependent data.
    (setq nn 0)
	(loop for n from 0 until N do
       (setq X[nn] (setq x (new Vector: Number: M)))
	   (loop for m from 0 until M do
		  (setq x[m] (- (gsm.myRandomFunction 100.0) 50.0))
		  ) ; end M loop
       (setq y (modelLambda x))
       (if (>= y BIGPOSNUM) (goto Last:))
       (if (<= y BIGNEGUM) (goto Last:))
	   (setq Y[nn] (setq y (+ (* y (- 1.0 (* .50 noise))) (* y (gsm.myRandomFunction noise)))))
       (setCdr x y)
       (++ nn)
	   ) ; end N loop
    (setq theData.X X)
    (setq theData.Y Y)

	Last::

    ;; Return the dynamic data generated herein.
    theData) ; end Dynamic Regress Data unnamed lambda


;;**EXPORTKEY**:gsmDemo.%DynamicRandomRootRegressionData
;#text#
(lambda(...)
;; ******************************************************************************
;; summary:  This unnamed lambda is an example of a dynamic data generation
;;           program which will create a new dynamic regression matrix every
;;           time it is run. 
;;
;; Args:     none		
;;
;; Return:   theData		A Structure containing TX, TY, X, and Y regression data.
;;
;; Notes:    TX				The training independent variable matrix.
;;           TY             The training dependent variable vector.
;;           X				The testing independent variable matrix.
;;           Y              The testing dependent variable vector.
;;
;; Sample Usage:
;;    (setq aTemplate (mid (browseLib.checkout GSMDemo: "gsmDemo:%DynamicRandomRootRegressionData") 7 1000000))    
;;    (setq aDataLambda (substitute aTemplate "$NOISE$" ".10"))
;;    (setq aDataLambda (substitute aDataLambda "$COLS$" "5"))
;;    (setq aDataLambda (substitute aDataLambda "$ROWS$" "1000"))
;;    (setq aDataLambda (eval (compile (lisp aDataLambda))))
;;    (setq aJobData (aDataLambda))
;; ******************************************************************************
    regs:(m mm M n nn N)
	vars:(theData (noise $NOISE$) (MM $COLS$) (NN $ROWS$) (seed 29183465.0))
    vars:(modelString modelLambda)
    vars:(x X y Y C)

    ;; Initialize and set up the final data structure to be returned.
    (gsm.setOptions default: 0% false)    
    (gsm.setRandomSeed seed)
    (setq N NN)(if (< N 1000) (setq N 1000))
    (setq M MM)(if (< M 3)(setq M 3))
    (if (< noise 0.0) (setq noise 0.0))
    (if (> noise .40) (setq noise .40))
    (setq theData (new Structure: TX: #void TY: #void X: #void Y: #void N: N M: M Noise: noise Seed: seed Model:#void)) 

    ;; ****************************************************************************************************************************
    ;; Create the training data
    ;; Note: Should match Cross Correlation test case published in "Genetic Programming Theory and Practive V" Springer Verlag 2008
    ;; ****************************************************************************************************************************
    (setq X (new Vector: Object: N))
    (setq Y (new Vector: Number: N))
    (setq C (new Vector: Number: M))

    (setq modelLambda (gsm.buildTrainingModel M false))
    (setq modelString modelLambda.WFF)
    (setq modelString (mid modelString 7 10000000))
	(setq modelString (append "y = " modelString))
    (setq theData.Model modelString)

	;; Build training independendant data randomly and use the training model to compute the dependent data.
    (setq nn 0)
	(loop for n from 0 until N do
       (setq X[nn] (setq x (new Vector: Number: M)))
	   (loop for m from 0 until M do
		  (setq x[m] (- (gsm.myRandomFunction 100.0) 50.0))
		  ) ; end M loop
       (setq y (modelLambda x))
       (if (>= y BIGPOSNUM) (goto Last:))
       (if (<= y BIGNEGUM) (goto Last:))
	   (setq Y[nn] (setq y (+ (* y (- 1.0 (* .50 noise))) (* y (gsm.myRandomFunction noise)))))
       (setCdr x y)
       (++ nn)
	   ) ; end N loop
    (setq theData.TX X)
    (setq theData.TY Y)

    (setq X (new Vector: Object: N))
    (setq Y (new Vector: Number: N))
	;; Build testing independendant data randomly and use the training model to compute the dependent data.
    (setq nn 0)
	(loop for n from 0 until N do
       (setq X[nn] (setq x (new Vector: Number: M)))
	   (loop for m from 0 until M do
		  (setq x[m] (- (gsm.myRandomFunction 100.0) 50.0))
		  ) ; end M loop
       (setq y (modelLambda x))
       (if (>= y BIGPOSNUM) (goto Last:))
       (if (<= y BIGNEGUM) (goto Last:))
	   (setq Y[nn] (setq y (+ (* y (- 1.0 (* .50 noise))) (* y (gsm.myRandomFunction noise)))))
       (setCdr x y)
       (++ nn)
	   ) ; end N loop
    (setq theData.X X)
    (setq theData.Y Y)

	Last::

    ;; Return the dynamic data generated herein.
    theData) ; end Dynamic Regress Data unnamed lambda


;;**EXPORTKEY**:gsmDemo.%DynamicRatioRegressionData
;#text#
(lambda(...)
;; ******************************************************************************
;; summary:  This unnamed lambda is an example of a dynamic data generation
;;           program which will create a new dynamic regression matrix every
;;           time it is run. 
;;
;; Args:     none		
;;
;; Return:   theData		A Structure containing TX, TY, X, and Y regression data.
;;
;; Notes:    TX				The training independent variable matrix.
;;           TY             The training dependent variable vector.
;;           X				The testing independent variable matrix.
;;           Y              The testing dependent variable vector.
;;
;; Sample Usage:
;;    (setq aTemplate (mid (browseLib.checkout GSMDemo: "gsmDemo:%DynamicRatioRegressionData") 7 1000000))    
;;    (setq aDataLambda (substitute aTemplate "$NOISE$" ".10"))
;;    (setq aDataLambda (substitute aDataLambda "$COLS$" "5"))
;;    (setq aDataLambda (substitute aDataLambda "$ROWS$" "1000"))
;;    (setq aDataLambda (eval (compile (lisp aDataLambda))))
;;    (setq aJobData (aDataLambda))
;; ******************************************************************************
    regs:(m mm M n nn N)
	vars:(theData (noise $NOISE$) (MM $COLS$) (NN $ROWS$) (seed 29183465.0))
    vars:(modelSource modelString modelLambda)
    vars:(x X y Y C)

    ;; Initialize and set up the final data structure to be returned.
    (gsm.setOptions default: 0% false)    
    (gsm.setRandomSeed seed)
    (setq N NN)(if (< N 1000) (setq N 1000))
    (setq M MM)(if (< M 3)(setq M 3))
    (if (< noise 0.0) (setq noise 0.0))
    (if (> noise .40) (setq noise .40))
    (setq theData (new Structure: TX: #void TY: #void X: #void Y: #void N: N M: M Noise: noise Seed: seed Model:#void)) 

    ;; ****************************************************************************************************************************
    ;; Create the training data
    ;; Note: Should match Cross Correlation test case published in "Genetic Programming Theory and Practive V" Springer Verlag 2008
    ;; ****************************************************************************************************************************
    (setq X (new Vector: Object: N))
    (setq Y (new Vector: Number: N))
    (setq C (new Vector: Number: M))

    ;; Create the constants to be used in model construction
    (loop for m from 0 until M do
       (cond 
	     ((= m 0) (setq C[m] 1.57))
         ((= m 1) (setq C[m] -39.34))
         ((= m 2) (setq C[m] 2.13))
         ((= m 3) (setq C[m] 46.59))
         ((= m 4) (setq C[m] 11.54))
	     (else (setq C[m] (- (gsm.myRandomFunction 100.0) 50.0)))
         ) ; end cond
	    ) ; end C loop

    ;; Compile the model which will be used to generate the dependent variables.
    (setq modelSource "model(")
    (setq modelSource (append modelSource " if (abs(ninteger(" (gsm.eGrammar.ruleName 0) " % 4.0)) == 0.0) {" (string C[0])))
    (loop for m from 0 until Mp1 do
       ;; linear model
       (setq modelSource (append modelSource " + (" C[m] "*" (gsm.eGrammar.ruleName m) ")"))
    ) ; end C loop
    (setq modelSource (append modelSource "} else "))
    (setq modelSource (append modelSource " if (abs(ninteger(" (gsm.eGrammar.ruleName 0) " % 4.0)) == 1.0) {" (string C[0])))
    (loop for m from 0 until Mp1 do
       ;; square model
       (setq modelSource (append modelSource " + (" C[m] "*" (gsm.eGrammar.ruleName m) "*" (gsm.eGrammar.ruleName m)")"))
    ) ; end C loop
    (setq modelSource (append modelSource "} else "))
    (setq modelSource (append modelSource " if (abs(ninteger(" (gsm.eGrammar.ruleName 0) " % 4.0)) == 2.0) {" (string C[0])))
    (loop for m from 0 until Mp1 do
       ;; sine model
       (setq modelSource (append modelSource " + (" C[m] "*sin(" (gsm.eGrammar.ruleName m) "))"))
    ) ; end C loop
    (setq modelSource (append modelSource "} else "))
    (setq modelSource (append modelSource " if (abs(ninteger(" (gsm.eGrammar.ruleName 0) " % 4.0)) == 3.0) {" (string C[0])))
    (loop for m from 0 until Mp1 do
       ;; log model
       (setq modelSource (append modelSource " + (" C[m] "*log(.000001+abs(" (gsm.eGrammar.ruleName m) ")))"))
    ) ; end C loop
	(setq modelSource (append modelSource "});"))
    (setq modelLambda (gsm.compileEstimator modelSource))
    (setq modelString modelSource)
    (setq modelString (mid modelString 7 10000000)) 
	(setq modelString (append "y = " modelString))
    (setq theData.Model modelString)

	;; Build training independendant data randomly and use the training model to compute the dependent data.
    (setq nn 0)
	(loop for n from 0 until N do
       (setq X[nn] (setq x (new Vector: Number: M)))
	   (loop for m from 0 until M do
		  (setq x[m] (- (gsm.myRandomFunction 100.0) 50.0))
		  ) ; end M loop
       (setq y (modelLambda x))
	   (setq Y[nn] (setq y (+ (* y (- 1.0 (* .50 noise))) (* y (gsm.myRandomFunction noise)))))
       (setCdr x y)
       (++ nn)
	   ) ; end N loop
    (setq theData.TX X)
    (setq theData.TY Y)

    (setq X (new Vector: Object: N))
    (setq Y (new Vector: Number: N))
	;; Build testing independendant data randomly and use the training model to compute the dependent data.
    (setq nn 0)
	(loop for n from 0 until N do
       (setq X[nn] (setq x (new Vector: Number: M)))
	   (loop for m from 0 until M do
		  (setq x[m] (- (gsm.myRandomFunction 100.0) 50.0))
		  ) ; end M loop
       (setq y (modelLambda x))
	   (setq Y[nn] (setq y (+ (* y (- 1.0 (* .50 noise))) (* y (gsm.myRandomFunction noise)))))
       (setCdr x y)
       (++ nn)
	   ) ; end N loop
    (setq theData.X X)
    (setq theData.Y Y)

    ;; Return the dynamic data generated herein.
    theData) ; end Dynamic Regress Data unnamed lambda


;;**EXPORTKEY**:gsmDemo.%DynamicSquareRootRegressionData
;#text#
(lambda(...)
;; ******************************************************************************
;; summary:  This unnamed lambda is an example of a dynamic data generation
;;           program which will create a new dynamic regression matrix every
;;           time it is run. 
;;
;; Args:     none		
;;
;; Return:   theData		A Structure containing TX, TY, X, and Y regression data.
;;
;; Notes:    TX				The training independent variable matrix.
;;           TY             The training dependent variable vector.
;;           X				The testing independent variable matrix.
;;           Y              The testing dependent variable vector.
;;
;; Sample Usage:
;;    (setq aTemplate (mid (browseLib.checkout GSMDemo: "gsmDemo:%DynamicSquareRootRegressionData") 7 1000000))    
;;    (setq aDataLambda (substitute aTemplate "$NOISE$" ".10"))
;;    (setq aDataLambda (substitute aDataLambda "$COLS$" "5"))
;;    (setq aDataLambda (substitute aDataLambda "$ROWS$" "1000"))
;;    (setq aDataLambda (eval (compile (lisp aDataLambda))))
;;    (setq aJobData (aDataLambda))
;; ******************************************************************************
    regs:(m mm M n nn N)
	vars:(theData (noise $NOISE$) (MM $COLS$) (NN $ROWS$) (seed 29183465.0))
    vars:(modelSource modelString modelLambda)
    vars:(x X y Y C)

    ;; Initialize and set up the final data structure to be returned.
    (gsm.setOptions default: 0% false)    
    (gsm.setRandomSeed seed)
    (setq N NN)(if (< N 1000) (setq N 1000))
    (setq M MM)(if (< M 3)(setq M 3))
    (if (< noise 0.0) (setq noise 0.0))
    (if (> noise .40) (setq noise .40))
    (setq theData (new Structure: TX: #void TY: #void X: #void Y: #void N: N M: M Noise: noise Seed: seed Model:#void)) 

    ;; ****************************************************************************************************************************************
    ;; Create the training data
    ;; Note: Should match Square Root test case published in "Mutation and Crossover using Abstract Expression Grammars" GEECO Proceedings 2009
    ;; ****************************************************************************************************************************************
    (setq X (new Vector: Object: N))
    (setq Y (new Vector: Number: N))
    (setq C (new Vector: Number: M))

    ;; Create the constants to be used in model construction
    (loop for m from 0 until M do
       (cond 
	 	 ((= m 0) (setq C[m] 1.23))
		 ((= m 1) (setq C[m] -9.16))
		 ((= m 2) (setq C[m] 11.27))
		 ((= m 3) (setq C[m] 7.42))
		 ((= m 4) (setq C[m] 8.21))
	     (else (setq C[m] (- (gsm.myRandomFunction 100.0) 50.0)))
         ) ; end cond
	    ) ; end C loop

    ;; Compile the model which will be used to generate the dependent variables.
    (setq modelSource (append "model(" (string C[0])))
    (loop for m from 0 until M do
       (cond 
	     ((= m 0) (setq modelSource (append modelSource " + (" C[m] "*(sqrt" (gsm.eGrammar.ruleName m) "*" (gsm.eGrammar.ruleName m) "*" (gsm.eGrammar.ruleName m) "))")))
	     ((= m 1) (setq modelSource (append modelSource " + (" C[m] "*(sqrt" (gsm.eGrammar.ruleName (- m 1)) "*" (gsm.eGrammar.ruleName m) "*" (gsm.eGrammar.ruleName m) "))")))
	     (else (setq modelSource (append modelSource " + (" C[m] "*(sqrt" (gsm.eGrammar.ruleName (- m 2)) "*" (gsm.eGrammar.ruleName (- m 1)) "*" (gsm.eGrammar.ruleName m) "))")))
        ) ; end cond
	   ) ; end C loop
    (setq modelSource (append modelSource ");"))
    (setq modelLambda (gsm.compileEstimator modelSource))
    (setq modelString modelSource)
    (setq modelString (mid modelString 7 10000000)) 
	(setq modelString (append "y = " modelString))
    (setq theData.Model modelString)

	;; Build training independendant data randomly and use the training model to compute the dependent data.
    (setq nn 0)
	(loop for n from 0 until N do
       (setq X[nn] (setq x (new Vector: Number: M)))
	   (loop for m from 0 until M do
		  (setq x[m] (- (gsm.myRandomFunction 100.0) 50.0))
		  ) ; end M loop
       (setq y (modelLambda x))
	   (setq Y[nn] (setq y (+ (* y (- 1.0 (* .50 noise))) (* y (gsm.myRandomFunction noise)))))
       (setCdr x y)
       (++ nn)
	   ) ; end N loop
    (setq theData.TX X)
    (setq theData.TY Y)

    (setq X (new Vector: Object: N))
    (setq Y (new Vector: Number: N))
	;; Build testing independendant data randomly and use the training model to compute the dependent data.
    (setq nn 0)
	(loop for n from 0 until N do
       (setq X[nn] (setq x (new Vector: Number: M)))
	   (loop for m from 0 until M do
		  (setq x[m] (- (gsm.myRandomFunction 100.0) 50.0))
		  ) ; end M loop
       (setq y (modelLambda x))
	   (setq Y[nn] (setq y (+ (* y (- 1.0 (* .50 noise))) (* y (gsm.myRandomFunction noise)))))
       (setCdr x y)
       (++ nn)
	   ) ; end N loop
    (setq theData.X X)
    (setq theData.Y Y)

    ;; Return the dynamic data generated herein.
    theData) ; end Dynamic Regress Data unnamed lambda


;;**EXPORTKEY**:gsmDemo.%DynamicTrigonometricRegressionData
;#text#
(lambda(...)
;; ******************************************************************************
;; summary:  This unnamed lambda is an example of a dynamic data generation
;;           program which will create a new dynamic regression matrix every
;;           time it is run. 
;;
;; Args:     none		
;;
;; Return:   theData		A Structure containing TX, TY, X, and Y regression data.
;;
;; Notes:    TX				The training independent variable matrix.
;;           TY             The training dependent variable vector.
;;           X				The testing independent variable matrix.
;;           Y              The testing dependent variable vector.
;;
;; Sample Usage:
;;    (setq aTemplate (mid (browseLib.checkout GSMDemo: "gsmDemo:%DynamicTrigonometricRegressionData") 7 1000000))    
;;    (setq aDataLambda (substitute aTemplate "$NOISE$" ".10"))
;;    (setq aDataLambda (substitute aDataLambda "$COLS$" "5"))
;;    (setq aDataLambda (substitute aDataLambda "$ROWS$" "1000"))
;;    (setq aDataLambda (eval (compile (lisp aDataLambda))))
;;    (setq aJobData (aDataLambda))
;; ******************************************************************************
    regs:(m mm M n nn N)
	vars:(theData (noise $NOISE$) (MM $COLS$) (NN $ROWS$) (seed 29183465.0))
    vars:(modelSource modelString modelLambda)
    vars:(x X y Y C)

    ;; Initialize and set up the final data structure to be returned.
    (gsm.setOptions default: 0% false)    
    (gsm.setRandomSeed seed)
    (setq N NN)(if (< N 1000) (setq N 1000))
    (setq M MM)(if (< M 3)(setq M 3))
    (if (< noise 0.0) (setq noise 0.0))
    (if (> noise .40) (setq noise .40))
    (setq theData (new Structure: TX: #void TY: #void X: #void Y: #void N: N M: M Noise: noise Seed: seed Model:#void)) 

    ;; *****************************************************************************************************************
    ;; Create the training data
    ;; Note: Should match test case published in "Genetic Programming Theory and Practive V" Springer Verlag 2008
    ;; *****************************************************************************************************************
    (setq X (new Vector: Object: N))
    (setq Y (new Vector: Number: N))
    (setq C (new Vector: Number: M))

    ;; Create the constants to be used in model construction
    (loop for m from 0 until M do
       (cond 
	     ((= m 0) (setq C[m] -9.16))
	     ((= m 1) (setq C[m] 22.19))
	     ((= m 2) (setq C[m] 1.07))
	     ((= m 3) (setq C[m] -17.48))
	     ((= m 4) (setq C[m] 18.81))
	     (else (setq C[m] (- (gsm.myRandomFunction 100.0) 50.0)))
         ) ; end cond
	    ) ; end C loop

    ;; Compile the model which will be used to generate the dependent variables.
    (setq modelSource (append "model(" (string C[0])))
    (loop for m from 0 until M do
       (cond 
	     ((= m 0) (setq modelSource (append modelSource " + cos(" C[m] "*" (gsm.eGrammar.ruleName m) "*" (gsm.eGrammar.ruleName m) "*" (gsm.eGrammar.ruleName m) ")")))
	     ((= m 1) (setq modelSource (append modelSource " + sin(" C[m] "*" (gsm.eGrammar.ruleName (- m 1)) "*" (gsm.eGrammar.ruleName m) "*" (gsm.eGrammar.ruleName m) ")")))
	     ((= (modi m 2) 0) (setq modelSource (append modelSource " + cos(" C[m] "*" (gsm.eGrammar.ruleName (- m 2)) "*" (gsm.eGrammar.ruleName (- m 1)) "*" (gsm.eGrammar.ruleName m) ")")))
	     (else (setq modelSource (append modelSource " + sin(" C[m] "*" (gsm.eGrammar.ruleName (- m 2)) "*" (gsm.eGrammar.ruleName (- m 1)) "*" (gsm.eGrammar.ruleName m) ")")))
        ) ; end cond
	   ) ; end C loop
    (setq modelSource (append modelSource ");"))
    (setq modelLambda (gsm.compileEstimator modelSource))
    (setq modelString modelSource)
    (setq modelString (mid modelString 7 10000000)) 
	(setq modelString (append "y = " modelString))
    (setq theData.Model modelString)

	;; Build training independendant data randomly and use the training model to compute the dependent data.
    (setq nn 0)
	(loop for n from 0 until N do
       (setq X[nn] (setq x (new Vector: Number: M)))
	   (loop for m from 0 until M do
		  (setq x[m] (- (gsm.myRandomFunction 100.0) 50.0))
		  ) ; end M loop
       (setq y (modelLambda x))
	   (setq Y[nn] (setq y (+ (* y (- 1.0 (* .50 noise))) (* y (gsm.myRandomFunction noise)))))
       (setCdr x y)
       (++ nn)
	   ) ; end N loop
    (setq theData.TX X)
    (setq theData.TY Y)

    (setq X (new Vector: Object: N))
    (setq Y (new Vector: Number: N))
	;; Build testing independendant data randomly and use the training model to compute the dependent data.
    (setq nn 0)
	(loop for n from 0 until N do
       (setq X[nn] (setq x (new Vector: Number: M)))
	   (loop for m from 0 until M do
		  (setq x[m] (- (gsm.myRandomFunction 100.0) 50.0))
		  ) ; end M loop
       (setq y (modelLambda x))
	   (setq Y[nn] (setq y (+ (* y (- 1.0 (* .50 noise))) (* y (gsm.myRandomFunction noise)))))
       (setCdr x y)
       (++ nn)
	   ) ; end N loop
    (setq theData.X X)
    (setq theData.Y Y)

    ;; Return the dynamic data generated herein.
    theData) ; end Dynamic Regress Data unnamed lambda


;;**EXPORTKEY**:gsmDemo.%DynamicUserModelRegressionData
;#text#
(lambda(...)
;; ******************************************************************************
;; summary:  This unnamed lambda is an example of a dynamic data generation
;;           program which will create a new dynamic regression matrix every
;;           time it is run. 
;;
;; Args:     none		
;;
;; Return:   theData		A Structure containing TX, TY, X, and Y regression data.
;;
;; Notes:    TX				The training independent variable matrix.
;;           TY             The training dependent variable vector.
;;           X				The testing independent variable matrix.
;;           Y              The testing dependent variable vector.
;;
;; Sample Usage:
;;    (setq aTemplate (mid (browseLib.checkout GSMDemo: "gsmDemo:%DynamicUserModelRegressionData") 7 1000000))    
;;    (setq aDataLambda (substitute aTemplate "$NOISE$" ".10"))
;;    (setq aDataLambda (substitute aDataLambda "$COLS$" "5"))
;;    (setq aDataLambda (substitute aDataLambda "$ROWS$" "1000"))
;;    (setq aDataLambda (substitute aDataLambda "$MODEL_STRING$" "regress(x2*x3*x4);"))
;;    (setq aDataLambda (eval (compile (lisp aDataLambda))))
;;    (setq aJobData (aDataLambda))
;; ******************************************************************************
    regs:(m mm M n nn N)
	vars:(theData (noise $NOISE$) (MM $COLS$) (NN $ROWS$) (seed 29183465.0))
    vars:((modelSource "$MODEL_STRING$") modelString modelLambda)
    vars:(x X y Y)

    ;; Initialize and set up the final data structure to be returned.
    (gsm.setOptions default: 0% false)    
    (gsm.setRandomSeed seed)
    (setq N NN)(if (< N 1000) (setq N 1000))
    (setq M MM)(if (< M 3)(setq M 3))
    (if (< noise 0.0) (setq noise 0.0))
    (if (> noise .40) (setq noise .40))
    (setq theData (new Structure: TX: #void TY: #void X: #void Y: #void N: N M: M Noise: noise Seed: seed Model:#void)) 

    ;; ****************************************************************************************************************************
    ;; Create the training data
    ;; Note: Should match Cross Correlation test case published in "Genetic Programming Theory and Practive V" Springer Verlag 2008
    ;; ****************************************************************************************************************************
    (setq X (new Vector: Object: N))
    (setq Y (new Vector: Number: N))

    (setq modelLambda (gsm.compileEstimator modelSource))
	(setq modelLambda.WFF modelSource)
	(setq modelLambda.A 0.0)
	(setq modelLambda.B 1.0)
    (setq modelString modelSource)
    (setq modelString (mid modelString 7 10000000))
	(setq modelString (append "y = " modelString))
    (setq theData.Model modelString)

	;; Build training independendant data randomly and use the training model to compute the dependent data.
    (setq nn 0)
	(loop for n from 0 until N do
       (setq X[nn] (setq x (new Vector: Number: M)))
	   (loop for m from 0 until M do
		  (setq x[m] (- (gsm.myRandomFunction 100.0) 50.0))
		  ) ; end M loop
       (setq y (modelLambda x))
       (if (>= y BIGPOSNUM) (goto Last:))
       (if (<= y BIGNEGUM) (goto Last:))
	   (setq Y[nn] (setq y (+ (* y (- 1.0 (* .50 noise))) (* y (gsm.myRandomFunction noise)))))
       (setCdr x y)
       (++ nn)
	   ) ; end N loop
    (setq theData.TX X)
    (setq theData.TY Y)

    (setq X (new Vector: Object: N))
    (setq Y (new Vector: Number: N))
	;; Build testing independendant data randomly and use the training model to compute the dependent data.
    (setq nn 0)
	(loop for n from 0 until N do
       (setq X[nn] (setq x (new Vector: Number: M)))
	   (loop for m from 0 until M do
		  (setq x[m] (- (gsm.myRandomFunction 100.0) 50.0))
		  ) ; end M loop
       (setq y (modelLambda x))
       (if (>= y BIGPOSNUM) (goto Last:))
       (if (<= y BIGNEGUM) (goto Last:))
	   (setq Y[nn] (setq y (+ (* y (- 1.0 (* .50 noise))) (* y (gsm.myRandomFunction noise)))))
       (setCdr x y)
       (++ nn)
	   ) ; end N loop
    (setq theData.X X)
    (setq theData.Y Y)

	Last::

    ;; Return the dynamic data generated herein.
    theData) ; end Dynamic Regress Data unnamed lambda


;;**EXPORTKEY**:gsmDemo.generateData
;; generates training and testing data and saves them in tab-delimited files
;; aTestName.Training.txt
;; aTestName.Testing.txt
;; aTestName.Parameters.txt
;; Usage: (gsmDemo.generateData aTestName aTestCase aRows aColumns aNoise aGenerations aMaxTime aModelName aHaltingScore aSVMKernelID aNumChampions)
(deforphan gsmDemo:generateData(aTestName aTestCase aRows aColumns ...)
	vars:(aTestFile
		  aTemplate
		  aRecord
		  aDataLambda
		  aData
		  (aNoise 0.10)
		  (aGenerations 5)
		  (aMaxTime 0.5)
		  (aModelName "regressGSOALPS")
		  (aHaltingScore 0)
		  (aSVMKernelID "composite")
		  (aNumChampions 25)
		  aTrainingFileName
		  aTestingFileName
		  aParametersFileName
		  aTrainingCursor
		  aTestingCursor
		  aParametersCursor
		  aParametersVector
		  aTrainingDataColumns
		  aTestingDataColumns
		  (aValidTestCases #(CrossCorrelation:
							 Cubic:
						  	 CyclicSeries:
					 		 Elipsoid:
				 			 HiddenModel:
			 				 HyperTangent:
							 Linear:
						 	 MixedModel:
							 RandomModal:
							 RandomRoot:
							 Ratio:
							 SquareRoot:
							 Trigonometric:
							 UserModel:))
		)
	vars:(c r n C)
	
	(if (> (argCount) 4) (setq aNoise (argFetch 4)))
	(if (> (argCount) 5) (setq aGenerations (argFetch 5)))
	(if (> (argCount) 6) (setq aMaxTime (argFetch 6)))
	(if (> (argCount) 7) (setq aModelName (argFetch 7)))
	(if (> (argCount) 8) (setq aHaltingScore (argFetch 8)))
	(if (> (argCount) 9) (setq aSVMKernelID (argFetch 9)))
	(if (> (argCount) 10) (setq aNumChampions (argFetch 10)))

	(if (not (isMember aTestCase aValidTestCases)) (error (append "Invalid test case [" aTestCase "].")))
	(setq aTestFile (append "gsmDemo:%Dynamic" aTestCase "RegressionData"))
	(setq aTemplate (browseLib.checkout GsmDemo: aTestFile))

	(if (= aTemplate #void) (error (append "Invalid template [" aTestFile "].")))
	(setq aTemplate (mid aTemplate 7 1000000))
	(setq aTemplate (substitute aTemplate "$NOISE$" (string aNoise)))
	(setq aTemplate (substitute aTemplate "$COLS$" (string aColumns)))
	(setq aTemplate (substitute aTemplate "$ROWS$" (string aRows)))

	(setq aDataLambda (eval (compile (lisp aTemplate))))

	(if (not (isLambda aDataLambda)) (error (append "Invalid lambda")))
	(setq aData (aDataLambda))

	(setq aTrainingFileName (append aTestName ".Training.txt"))
	(setq aTestingFileName (append aTestName ".Testing.txt"))
	(setq aParametersFileName (append aTestName ".Parameters.txt"))

	;(writeln "aGenerations = " aGenerations)
	;(writeln "aMaxTime = " aMaxTime)
	;(writeln "aModelName = " aModelName)
	;(writeln "aHaltingScore = " aHaltingScore)
	;(writeln "aSVMKernelID = " aSVMKernelID)
	;(writeln "aNumChampions = " aNumChampions)

	(setq aParametersCursor (browseLib.memoryCursor aParametersFileName #("Parameter" "Value")))
	(setq aParametersVector (new Vector:))
	(setq aParametersVector[0] #("Generations"))
	(setq aParametersVector[0][1] aGenerations)
	(setq aParametersVector[1] #("MaxTime"))
	(setq aParametersVector[1][1] aMaxTime)
	(setq aParametersVector[2] #("ModelName"))
	(setq aParametersVector[2][1] aModelName)
	(setq aParametersVector[3] #("HaltingScore"))
	(setq aParametersVector[3][1] aHaltingScore)
	(setq aParametersVector[4] #("SVMKernelID"))
	(setq aParametersVector[4][1] aSVMKernelID)
	(setq aParametersVector[5] #("NumChampions"))
	(setq aParametersVector[5][1] aNumChampions)

	(setq C (length aParametersVector))
	(loop for c from 0 until C do
		(setq aRecord (aParametersCursor.getNewRecord))
		(setq aRecord aParametersVector[c])
		(aParametersCursor.writeLast aRecord)
	) ; loop c

	;; Training and Testing Data Format 
	;; ID X1 X2 ... XN Y

	;; Prepare Training Data
	(setq aTrainingDataColumns (new Vector: (+ aColumns 2)))
	(setq aTrainingDataColumns[0] "ID")
	(loop for c from 0 until aColumns do
		(setq n (+ c 1))
		(setq aTrainingDataColumns[n] (append "X"n))
	) ; loop c

	(setq aTrainingDataColumns[(+ aColumns 1)] "Y")
	(setq aTrainingCursor (browseLib.memoryCursor aTrainingFileName aTrainingDataColumns))
	(loop for r from 0 until aRows do
		(setq aRecord (aTrainingCursor.getNewRecord))
		(setq aRecord[0] (+ r 1))
		(loop for c from 0 until aColumns do
			(setq n (+ c 1))
			(setq aRecord[n] aData.TX[r][c])
		) ; loop c
		(setq aRecord[(+ aColumns 1)] aData.TY[r])
		(aTrainingCursor.writeLast aRecord)
	) ; loop r

	;; Prepare Testing Data
	(setq aTestingDataColumns (new Vector: (+ aColumns 2)))
	(setq aTestingDataColumns[0] "ID")
	(loop for c from 0 until aColumns do
		(setq n (+ c 1))
		(setq aTestingDataColumns[n] (append "X"n))
	) ; loop c

	(setq aTestingDataColumns[(+ aColumns 1)] "Y")
	(setq aTestingCursor (browseLib.memoryCursor aTestingFileName aTestingDataColumns))
	(loop for r from 0 until aRows do
		(setq aRecord (aTestingCursor.getNewRecord))
		(setq aRecord[0] (+ r 1))
		(loop for c from 0 until aColumns do
			(setq n (+ c 1))
			(setq aRecord[n] aData.X[r][c])
		) ; loop c
		(setq aRecord[(+ aColumns 1)] aData.Y[r])
		(aTestingCursor.writeLast aRecord)
	) ; loop r

	;; Export to tab-delimited files
	(aParametersCursor.exportTab aParametersFileName)
	(aTrainingCursor.exportTab aTrainingFileName)
	(aTestingCursor.exportTab aTestingFileName)

true)


;;**EXPORTKEY**:gsmDemo.prettyPrintMemory
	(defchild gsmDemo:prettyPrintMemory(buffer memory margin)
		vars:(n N key newMargin
			(blanks "                                                                                                                                                              ")
			(tailSW false)
			) ; end temporary variables
			;; Initialize the display buffer (if necessary).
			(if (= buffer #void) (begin
				(setq buffer (new Vector: byte: 2000000))
				(setq tailSW true)
				))
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
					(prettyPrintMemory buffer memory[n 1] (+ newMargin (length key) 2))
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
					(prettyPrintMemory buffer memory[n] newMargin)
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
				(setq buffer (string buffer))
			)) ; end if
		buffer)

