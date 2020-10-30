;;*************************************
;;*************************************
;; Exported Lambda File Cabinet Document
;;*************************************
;;*************************************
;;version=5.0011-64bit
;;location=/home/franklin/Sources/ais/Libraries/RunQueue/RunQueue.sl
;;storageScope=file
;;importSync=auto
;;exportSync=auto
;;autoCompile=true
;;objRepoName=/home/franklin/Sources/ais/Libraries/RunQueue.db
;;lastImport=#Oct,2,2009:09:15:36
;;END __DATA

;;**EXPORTKEY**:runQueue
(defun runQueue(command ...)
;; ******************************************************************************
;; summary:  The RunQueue lambda implements a peer to peer architecture 
;;           which can be used to submit jobs from anywhere and which 
;;           can be used to accept and run jobs from anywhere. At an extreme, 
;;           to make a point, this would even include submitting a job 
;;           to runQueue and accepting the same job or another job, 
;;           via runQueue, from the same running context. Contexts 
;;           which submit jobs to runQueue for execution are called 
;;           suppliers, while contexts which accept jobs from runQueue 
;;           for execution are called subscribers.
;;
;;           This architecture is currently implemented on a local area  
;;           network of computers and may be implemented with slight variations
;;           in a cloud computing environment.
;;
;;           The runQueue Lambda, which is designed to be open source, 
;;           provides added value through its job management API, through 
;;           its console management API, via the thin job execution services 
;;           it provides, and via the function hiding design of its API (ideally, 
;;           if possible, the API design should not change regardless of a Network 
;;           File Sharing implementation, a TCP/IP implementation, or a Cloud 
;;           implementation). By far the most extensive value added will be provided
;;           by several function/class libraries providing remote execution services 
;;           for specific applications. The regressionHarness function library will 
;;           be extended to provide services for automatic data-matrix extraction, 
;;           multiple regression distribution and re-accumulation, multiple regression 
;;           reporting, trader Lambda memoization (defaulting/learning/promotion) 
;;           providing support of portfolio and factor research. The Gsm GP function 
;;           library will be enhanced to provide services, under regressMVLGSOALPS, 
;;           for multiple parallel population evolution. The Gsm Abstract Grammar 
;;           function library will be investigated to provide services, under 
;;           regressSWARM, for multiple parallel abstract feature, function, 
;;           and constant evolution.
;;
;;           It is possible for the runQueue Lambda to function without requiring 
;;           the user to study any of the available function/class libraries. 
;;           All jobFiles continue actual source code for the distributed work task 
;;           (unnamed lambda values to avoid unwanted side affects). These work tasks 
;;           may be completely stand alone or they may extend existing function/class 
;;           libraries. Thus runQueue will provide much of its added value in a non 
;;           restrictive manner.
;;
;;           Job Configuration
;;
;;           All runQueue suppliers must provide Job configuration information 
;;           containing only the basic information necessary to allow any subscriber 
;;           to determine whether or not to accept the job request. This information 
;;           should include the supplier's identification, the minimum free context 
;;           memory required, the minimum free disk memory required, any SQL databases 
;;           required, any function/class libraries required, etc. This job configuration 
;;           information is accessed by a prospective subscriber and the job is either 
;;           accepted or rejected based entirely upon the information in the job 
;;           configuration.
;;
;;           Job Data
;;
;;           All runQueue suppliers must provide thick Task data structures containing 
;;           all of the data and information necessary to allow a subscriber, which has 
;;           accepted the job request, to perform the work required by the job request. 
;;
;;           Job Lambda
;;
;;           All runQueue suppliers must provide a Job Lambda (an unnamed Lambda) 
;;           that knows how to process the job data.
;;
;; Args:     command			Initialization command (Values: local)
;;           queuePathName      (Optional) JobQueue folder path name		
;;           sqlAccess          (Optional) SQL access information and passwords		
;;
;; Return:   true
;;
;; Depend:   browseLib
;;           RunQueueDataConfig.ini  (see dataConfig method)
;; ******************************************************************************
pvars:(;; Public variables
       (jobQueuePathName "JobQueue")				;; The JobQueue folder path name
       (verboseSW true)		    					;; Verbose mode displays progress and debug information during execution.
       (pSleep 10000)								;; Time interval to check JobQueue (10 seconds)
       ;; Private methods
       checkoutDynamicDataLambda					;; Checks out and returns the self test dynamic data lambda.
       checkoutRegressionLambda		    			;; Checks out and returns the self test regression lambda.
       dataConfig									;; Implements the data connection, parsing, and construction logic generally necessary for the runQueue processing.
       selfTest 									;; The self testing utility for runQueue.
       _prettyPrintMemory                           ;; Returns a structures display of the specified object.
       _errHandler
       ;; Public methods for suppliers
       getResult    								;; Returns the final results of a completed job in the JobQueue 
       getStatus    								;; Returns the current status of a job in the JobQueue 
       registerSupplier	    						;; Adds a supplier to the registry in the JobQueue 
       remove	    								;; Removes a job from the JobQueue 
       submit										;; Submits a job to the JobQueue 
       unregisterSupplier	    					;; Removes a supplier from the registry in the JobQueue 
       ;; Public methods for subscribers
       registerSubscriber	    					;; Adds a subscriber to the registry in the JobQueue 
       run					    					;; Runs a job in the JobQueue 
       setDone					    				;; Post final results for a job in the JobQueue 
       setError					    				;; Post a run error to a job in the JobQueue 
       setJobQueuePathName		    				;; Set the JobQueue folder path name (default is "JobQueue")
       unregisterSubscriber	    					;; Removes a subscriber from the registry in the JobQueue 
       ;; Public methods for queue management      
       freeze	    								;; Sets job status to unavailable in the JobQueue 
       getActiveSubscriberList	    				;; Returns the list of active subscribers from the registry in the JobQueue 
       getActiveSupplierList					    ;; Returns the list of active suppliers from the registry in the JobQueue 
       isContextAlive					    		;; Returns true if the specified context is running 
       launchContext					    		;; Launches the specified context 
       launchMachine	    						;; Launches the specified machine 
       setPriority					    			;; Sets job priority in the JobQueue 
       stopContext					    			;; Shuts down the specified context 
       unfreeze					    				;; Sets job status to available in the JobQueue
      ) ; end persistent variables

pvars:(;; Variables visible to the error handler
       aCurrentJob aCurrentFile aJobName)

	;; ******************************************************************************
	;; Public Lambda Definitions
	;; ******************************************************************************
    ;; Checks out and returns the self test dynamic data lambda.
	(defun checkoutDynamicDataLambda(testcase rows cols noise ...)
		vars:(result ddLambda (generateSW false) (modelString "regress(x2*x3*x4);"))
 
        ;; Check out the dynamic data lambda source and substitude the arguments.
        (if (> (argCount) 4) (setq generateSW (argFetch 4)))
		(if (> (argCount) 5) (setq modelString (argFetch 5)))

		(setq ddLambda (append "runQueue:%Dynamic" testcase "RegressionData"))
        (setq ddLambda (mid (browseLib.checkout RunQueue: ddLambda) 7 1000000))
        (setq ddLambda (substitute ddLambda "$NOISE$" (string noise)))
        (setq ddLambda (substitute ddLambda "$COLS$" (string cols)))
        (setq ddLambda (substitute ddLambda "$ROWS$" (string rows)))
		(if (= testcase "UserModel") (setq ddLambda (substitute ddLambda "$MODEL_STRING$" modelString)))

        ;; Are we to return the source for the lambda or generate the data?
        (if (= generateSW true) 
            (setq result (eval (eval (compile (lisp ddLambda)))))
            (setq result ddLambda)
            ) ; end generate if

        result) ; end checkoutDynamicDataLambda

    ;; Checks out and returns the self test regression lambda.
	(defun checkoutRegressionLambda(gens maxTime ...)
		vars:(result ddLambda (generateSW false))
 
        ;; Check out the dynamic data lambda source and substitude the arguments.
        (if (> (argCount) 2) (setq generateSW (argFetch 2)))
        (setq ddLambda (mid (browseLib.checkout RunQueue: "runQueue:%DynamicRegressionLambda") 7 1000000))
        (setq ddLambda (substitute ddLambda "$GENS$" (string gens)))
        (setq ddLambda (substitute ddLambda "$TIME$" (string maxTime)))

        ;; Are we to return the source for the lambda or generate the compiled lambda?
        (if (= generateSW true) 
            (setq result (eval (compile (lisp ddLambda))))
            (setq result ddLambda)
            ) ; end generate if

        result) ; end checkoutRegressionLambda
    ;; Set the JobQueue folder path name (default is "JobQueue")
    (defun setJobQueuePathName(pathName) (setq jobQueuePathName pathName))
    
    ;; Returns a structures display of the specified object.
	(defun _prettyPrintMemory(buffer memory margin)
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
			)) ; end String case
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
		buffer) ; end _prettyPrintMemory

    ;; Handles the error coming from the job lambda
    ;; The following pvars should be set for this function to work:
    ;; aCurrentJob - Filename of the current job (full filename)
    ;; aCurrentFile - Partial filename of the current job (including .qrun)
    ;; aJobName - Partial filename of the current job (name only)
	(defun _errHandler(msg)
        vars:(aFile aBuffer aJobQueueDir)
		;; Check if Job Queue exists
		(setq aJobQueueDir (new browseLib.dir jobQueuePathName))
		(if (not (aJobQueueDir.exists)) (error: "RunQueue._errHandler: jobQueue directory does not exist"))

		(aJobQueueDir.rename aCurrentJob (append aCurrentFile ".errored." _ais.contextName))

		(writeln "Subscriber: Writing error.")
		(setq aFile (fileOpen (append jobQueuePathName "/" aJobName "error") 1 0))
		(setq aBuffer (append "" msg))
		(fileWrite aFile aBuffer)
		(setq aFile (fileClose aFile 1))

		(writeln "error: " msg)
	) ; end of ErrHandler

    ;; Submits a job to the JobQueue
	;; aJobConfig - a structure containing the job configuration
	;;	.JobName	-- job name
	;;	.MajorName  -- major name
    ;;  .MinorName  -- minor name
    ;;  .SeriesName -- series name
    ;;  .LambdaSourceFileName -- filename of lambda source
    ;;  .DataSourceFileName -- filename of data source
    ;;  .MinMemoryFree -- minimum free memory required to execute job
    ;;  .RequiredLibraries -- list of lambdas required to execute job
	;; aJobData   - a string which can be compiled and evaluated which produces the argument for the aJobLambda
	;; aJobLambda - a string which can be compiled and evaluated which produces the lambda to be executed
	;; exclusiveSW - optional, when set to "exclusive", will cause an error if job/s with a similar name exists
	(defun submit(aJobConfig ...)
		vars:(
			aJobData
			aJobLambda
			aJobQueueFileInfo
			aJobQueueDir
			aJobFileName
			aJobFile
			aBuffer
            aExclusiveSW)

		;; Check optional arguments
		(if (> (argCount) 1) (setq aJobData (argFetch 1)))
		(if (> (argCount) 2) (setq aJobLambda (argFetch 2)))
        (if (> (argCount) 3) (setq aExclusiveSW (argFetch 3)))

		;; Check Job Configuration
		;; JobName is required
		(if (not (isStructure aJobConfig)) (error "RunQueue.submit: invalid aJobConfig argument"))
		(if (= aJobConfig.JobName #void) (error "RunQueue.submit: missing aJobConfig.JobName argument"))

		;; Check JobLamda argument
		(if (not (= aJobLambda #void)) then
			(begin
			;; JobLambda should be a string (which can be compiled and evaluated)
			(if (and (not (isString aJobLambda)) (not (isByteVector aJobLambda))) (error: "RunQueue.submit: expecting aJobLambda argument to be void or a string"))
			)
		else
			(begin
			;; If JobLambda is not specified, check aJobConfig.LambdaSourceFileName
			;; This should point to the path of the lambda source
			(if (= aJobConfig.LambdaSourceFileName #void) (error "RunQueue.submit: missing aJobConfig.LambdaSourceFileName argument"))
			(if (not (isString aJobConfig.LambdaSourceFileName)) (error: "RunQueue.submit: invalid aJobConfig.LambdaSourceFileName argument"))

			;; Check if LambdaSourceFile exists
			(setq aJobQueueFileInfo (new browseLib.fileInfo aJobConfig.LambdaSourceFileName))
			(if (not (and (aJobQueueFileInfo.exists) (aJobQueueFileInfo.isFile))) (error: "RunQueue.submit: Lambda source file does not exist"))

			;; Read LambdaSourceFile contents
			(setq aJobFile (fileOpen aJobConfig.LambdaSourceFileName 0 0))
			(setq aJobLambda (fileRead aJobFile))
			(setq aJobFile (fileClose aJobFile 1))
			))

		;; Check JobData argument
		(if (not (= aJobData #void)) then
			(begin
			;; JobData should be a string (which can be compiled and evaluated)
			(if (and (not (isString aJobData)) (not (isByteVector aJobData)) (not (isStructure aJobData))) 
				(error: "RunQueue.submit: invalid aJobData argument"))
			(if (isStructure aJobData) (begin ;; Add jobConfig.binaryJobData: true to indicate we are doing a serialization to disk
				(setq aJobConfig.binaryJobData true); This will inform runQueue.run the data is in binary form
				))
			)
		else
			(begin
			;; If JobData is not specified, check aJobConfig.DataSourceFileName
			;; This should point to the path of the data source
			(if (= aJobConfig.DataSourceFileName #void) (error "RunQueue.submit: missing aJobConfig.DataSourceFileName argument"))
			(if (not (isString aJobConfig.DataSourceFileName)) (error: "RunQueue.submit: invalid aJobConfig.DataSourceFileName argument"))

			;; Check if DataSourceFile exists
			(setq aJobQueueFileInfo (new browseLib.fileInfo aJobConfig.DataSourceFileName))
			(if (not (and (aJobQueueFileInfo.exists) (aJobQueueFileInfo.isFile))) (error: "RunQueue.submit: Data source file does not exist"))

			;; Read LambdaSourceFile contents
			(setq aJobFile (fileOpen aJobConfig.DataSourceFileName 0 0))
			(setq aJobData (fileRead aJobFile))
			(setq aJobFile (fileClose aJobFile 1))
			))

		;; Check Job Queue Path
		(if (= jobQueuePathName #void) (error: "RunQueue.submit: jobQueuePathName not defined"))
		(setq aJobQueueFileInfo (new browseLib.fileInfo jobQueuePathName))

		;; Check if Job Queue Path points to an existing directory
		(if (not (and (aJobQueueFileInfo.exists) (aJobQueueFileInfo.isDir))) (error: "RunQueue: jobQueue directory does not exist"))
		(setq aJobQueueDir (new browseLib.dir jobQueuePathName))

		;; Compose the Job Filename
		;; JobFileName = JobName + MajorName + MinorName + SeriesName
		(setq aJobFileName aJobConfig.JobName)
		(if (not (= aJobConfig.MajorName #void)) (setq aJobFileName (append aJobFileName "_" aJobConfig.MajorName)))
		(if (not (= aJobConfig.MinorName #void)) (setq aJobFileName (append aJobFileName "_" aJobConfig.MinorName)))
		(if (not (= aJobConfig.SeriesName #void)) (setq aJobFileName (append aJobFileName "_" aJobConfig.SeriesName)))

        ;; Check to make sure that there is no other job with a conflicting name.
        (if (= aExclusiveSW "exclusive")
            (if (> (length (aJobQueueDir.entryList (append aJobConfig.JobName "_*"))) 0) (error: (append "RunQueue: jobQueue already contains a job named [" aJobConfig.JobName "]")))
            (if (> (length (aJobQueueDir.entryList (append aJobFileName ".*"))) 0) (error: (append "RunQueue: jobQueue already contains a job named [" aJobFileName "]")))
            ) ; end conflicting names if

		;; Write Lambda
		(setq aJobFile (fileOpen (append jobQueuePathName "/" aJobFileName ".lambda") 1 0))
		(fileWrite aJobFile aJobLambda)
		(setq aJobFile (fileClose aJobFile 1))

		;; Write Data
		(setq aJobFile (fileOpen (append jobQueuePathName "/" aJobFileName ".data") 1 0))
			(if aJobConfig.binaryJobData
				(saveObject aJobFile aJobData true)
				(fileWrite aJobFile aJobData))
		(setq aJobFile (fileClose aJobFile 1))

		;; Write Job Config
		(setq aJobFile (fileOpen (append jobQueuePathName "/" aJobFileName ".qrun.writing") 1 0))
		(setq aBuffer (_prettyPrintMemory #void aJobConfig 0))
		(fileWrite aJobFile aBuffer)
		(setq aJobFile (fileClose aJobFile 1))

		(writeln "Supplier:   Job Name: " aJobFileName)
		(writeln "Supplier:   Job Config written to: " (append jobQueuePathName "/" aJobFileName ".qrun"))
		(writeln "Supplier:   Lambda Source written to: " (append jobQueuePathName "/" aJobFileName ".lambda"))
		(writeln "Supplier:   Data Source written to: " (append jobQueuePathName "/" aJobFileName ".data"))

		;; Make sure we don't have duplicates
		(aJobQueueDir.remove (append aJobFileName ".qrun"))
		;; Rename Job Config
		(aJobQueueDir.rename (append aJobFileName ".qrun.writing") (append aJobFileName ".qrun"))
	true) ; end of submit

    ;; Runs a job in the JobQueue
	;; aRunOnce - optional, when set to true, the run function will return after a single pass on the JobQueue
	(defun run(...)
		vars:(
			aJobQueueDir
			aFileList
			aFileCount
			aIndex
			aJobConfig
			aJobData
			aFile
			aBuffer
			aLambda
			aData
			aResults
			aLoop
			aRunOnce
			aFreeMemory
			aRequiredLibs
			aLibCount
			aLibIndex
			aStartTime
			aEndTime)

		;; Check Job Queue Path
		(if (= jobQueuePathName #void) (error: "RunQueue.run: jobQueuePathName not defined"))

		;; Check if Job Queue exists
		(setq aJobQueueDir (new browseLib.dir jobQueuePathName))
		(if (not (aJobQueueDir.exists)) (error: "RunQueue.run: jobQueue directory does not exist"))

		(if (> (argCount) 0) (setq aRunOnce (argFetch 0)))

		(setq aLoop true)
		(while aLoop ;; Loop never terminates (except in selfTest)
			;; Get a list of available jobs
			(setq aFileList (aJobQueueDir.entryList "*.qrun"  Files: #(Name:)))
			(setq aFileCount (length aFileList))
	
			(if (> aFileCount 0)
				(writeln "Subscriber: Found " aFileCount " job/s in " jobQueuePathName))

			(loop for aIndex from 0 until aFileCount do
				(setq aCurrentFile aFileList[aIndex])
				(setq aCurrentJob (append aCurrentFile ".running." _ais.contextName))
				(setq aJobName (substring aCurrentFile 0 (- (length aCurrentFile) 5)))
				(if (aJobQueueDir.rename aCurrentFile aCurrentJob)
					(begin
					;; Load Job Configuration
					(writeln "Subscriber: Current job: " aCurrentFile)

					(writeln "Subscriber: Loading configuration: " (append aCurrentJob))
					(setq aFile (fileOpen (append jobQueuePathName "/" aCurrentJob) 0 0))
					(setq aBuffer (fileRead aFile))
					(setq aFile (fileClose aFile 1))
					(setq aJobConfig (eval (compile (lisp aBuffer))))

					;; Check if the subscriber can run the job
					;; Check if we have the minimum amount of free memory (specified by the supplier)
					(if (isMember MinMemoryFree: aJobConfig)
						(begin
							(gc compact:)
							(setq aFreeMemory (inspect))
							(if (> (* aJobConfig.MinMemoryFree 1024 1024) aFreeMemory)
								(begin
								(writeln "Subscriber: Not enough memory to run the job.")
								; put the job back
								(aJobQueueDir.rename aCurrentJob aCurrentFile)
								(goto SKIP:)
								)
						) ; if memory is not enough
						) ; begin
					) ; if MinMemoryFree is defined

					;; Check if we have the required libraries (specified by the supplier)
					(if (isMember RequiredLibraries: aJobConfig)
						(begin
							(setq aRequiredLibs (stringToVector aJobConfig.RequiredLibraries " "))
							(setq aLibCount (length aRequiredLibs))
							(loop for aLibIndex from 0 until aLibCount do
								(if (not (isLambda (eval aRequiredLibs[aLibIndex])))
									(begin
									(writeln "Subscriber: One of the required libraries [" aRequiredLibs[aLibIndex] "] is missing.")
									; put the job back
									(aJobQueueDir.rename aCurrentJob aCurrentFile)
									(goto SKIP:)
									); begin
								) ; required lambda not present
							) ; loop
						) ; begin
					) ; if RequiredLibraries is defined

					;; Prepare the Results Structure
					(setq aResults (new Structure:
						JobStartTime: #void
						JobEndTime: #void
						LambdaExecutionTime: #void
						LambdaReturn: #void
					))

					;; Record Job Start Time
					(setq aResults.JobStartTime (date (now)))
					(onError _errHandler)
	
					;; Load Job Data
					;; Note1: The data file has the suffix .data instead of .qrun
                    ;; Note2: If the data file, after compilation, is a Lambda object,
                    ;;        then we evaluate it once more. This technique allows us
                    ;;        to have dynamic data files which construct themselves
                    ;;        at run time.
					(writeln "Subscriber: Loading data: " (append aJobName "data"))
					(setq aFile (fileOpen (append jobQueuePathName "/" aJobName "data") 0 0))
					(if (and (isMember binaryJobData: aJobConfig) (= aJobConfig.binaryJobData true)) (begin
						(setq aData (loadObject aFile))
						(setq aFile (fileClose aFile 1))
						)
					else (begin
						(setq aBuffer (fileRead aFile))
						(setq aFile (fileClose aFile 1))
						(setq aData (eval (compile (lisp aBuffer))))
						))
			
                    (if (isLambda aData) 
                        (begin
                          (clear)
                          (gc compact:)
                          (setq aData (eval aData))
                        )) ; end dynamic data if

					;; Load Job Lambda	
					(writeln "Subscriber: Loading lambda source: " (append aJobName "lambda"))
					;; Replace .qrun with .lambda
					(setq aFile (fileOpen (append jobQueuePathName "/" aJobName "lambda") 0 0))
					(setq aBuffer (fileRead aFile))
					(setq aFile (fileClose aFile 1))
                    ;;(debug compileon:)
					(setq aLambda (eval (compile (lisp aBuffer))))
                    ;;(debug compileoff:)

					;; Execute the Lambda
					(writeln "Subscriber: Executing lambda.")
                    (clear)
                    (gc compact:)

					;; Record Subscriber Context Name
					(setq aResults.SubscriberName _ais.contextName)

					(setq aStartTime (getTickCount 0))
					(setq aResults.LambdaReturn (aLambda aData))
					(setq aEndTime (getTickCount aStartTime))

					;; Record Lambda Execution Time
					(setq aResults.LambdaExecutionTime aEndTime)

					;; Record Job End Time
					(setq aResults.JobEndTime (date (now)))

					(onError #void)

					;; Write Lambda execution results
					(writeln "Subscriber: Writing result.")
					(setq aFile (fileOpen (append jobQueuePathName "/" aJobName "result") 1 0))
					(setq aBuffer (_prettyPrintMemory #void aResults 0))
					(fileWrite aFile aBuffer)
					(setq aFile (fileClose aFile 1))

					;; Done
					(aJobQueueDir.rename aCurrentJob (append aCurrentFile ".done." _ais.contextName))
					) ; begin
				) ; if
				SKIP::
			) ; loop
			(if aRunOnce (setq aLoop false) ; terminate loop if aRunOnce is true
			else
				(sleep pSleep)
			) ; if
		) ; while
	true) ; end of run

    ;; Returns the final results of a completed job in the JobQueue
	;; jobName - a string which identifies the job
	;; majorName - optional, major name or number of the job
	;; minorName - optional, minor name or number of the job
	;; seriesName - optional, series name or number of the job
	(defun getResult(jobName ...)
		vars: (aResults
			   aJobName
			   aResultsFile
			   aResultsFileName
			   aJobQueueDir
			   aResultsFileInfo
			   aBuffer
			   aMajorName
			   aMinorName
			   aSeriesName)

		;; Get optional arguments
		(if (> (argCount) 1) (setq aMajorName (argFetch 1)))
		(if (> (argCount) 2) (setq aMinorName (argFetch 2)))
		(if (> (argCount) 3) (setq aSeriesName (argFetch 3)))

		;; jobName is required
		(if (= jobName #void) (error: "RunQueue.getResult: missing jobName argument"))
		(if (not (isString jobName)) (error: "RunQueue.getResult: invalid jobName argument"))

		;; Check Job Queue Path
		(if (= jobQueuePathName #void) (error: "RunQueue.getResult: jobQueuePathName not defined"))

		;; Check if Job Queue exists
		(setq aJobQueueDir (new browseLib.dir jobQueuePathName))
		(if (not (aJobQueueDir.exists)) (error: "RunQueue.getResult: jobQueue directory does not exist"))

		(setq aJobName jobName)
		(if (not (= aMajorName #void)) (setq aJobName (append aJobName "_" aMajorName)))
		(if (not (= aMinorName #void)) (setq aJobName (append aJobName "_" aMinorName)))
		(if (not (= aSeriesName #void)) (setq aJobName (append aJobName "_" aSeriesName)))

		;; Check if result file exists
		(setq aResultsFileName (append aJobName ".result"))
		(setq aResultsFileInfo (new browseLib.fileInfo (append jobQueuePathName "/" aResultsFileName)))
		(if (not (and (aResultsFileInfo.exists) (aResultsFileInfo.isFile))) (error: "RunQueue.getResult: Results file does not exist"))

		;; Load result file
		(setq aResultsFile (fileOpen (append jobQueuePathName "/" aResultsFileName) 0 0))
		(setq aBuffer (fileRead aResultsFile))
		(setq aResultsFile (fileClose aResultsFile 1))

		;; Get result
		(setq aResults (eval (compile (lisp aBuffer))))
	aResults) ; end of getResult

	;; Returns the status of the job
	;; jobName - a string which identifies the job
	;; majorName - optional, major name or number of the job
	;; minorName - optional, minor name or number of the job
	;; seriesName - optional, series name or number of the job
	;; Returns
	;;	"missing"
	;;	"duplicate"
	;;	"ready"
	;;	"running"
	;;	"errored"
	;;	"done"
	(defun getStatus(jobName ...)
		vars: (aStatus
			   aJobQueueDir
			   aJobName
			   aJobFileName
			   aFileList
		 	   aFileCount
			   aJobFileInfo
			   aIndex1
			   aIndex2
			   aMajorName
			   aMinorName
			   aSeriesName)

		;; Get optional arguments
		(if (> (argCount) 1) (setq aMajorName (argFetch 1)))
		(if (> (argCount) 2) (setq aMinorName (argFetch 2)))
		(if (> (argCount) 3) (setq aSeriesName (argFetch 3)))

		;; jobName is required
		(if (= jobName #void) (error: "RunQueue.getStatus: missing jobName argument"))
		(if (not (isString jobName)) (error: "RunQueue.getStatus: invalid jobName argument"))

		;; Check Job Queue Path
		(if (= jobQueuePathName #void) (error: "RunQueue.getStatus: jobQueuePathName not defined"))

		;; Check if Job Queue exists
		(setq aJobQueueDir (new browseLib.dir jobQueuePathName))
		(if (not (aJobQueueDir.exists)) (error: "RunQueue.getStatus: jobQueue directory does not exist"))

		(setq aJobName jobName)
		(if (not (= aMajorName #void)) (setq aJobName (append aJobName "_" aMajorName)))
		(if (not (= aMinorName #void)) (setq aJobName (append aJobName "_" aMinorName)))
		(if (not (= aSeriesName #void)) (setq aJobName (append aJobName "_" aSeriesName)))

		(setq aJobFileName (append aJobName ".qrun"))

		;; Check if Job File exists (for ready status)
		(setq aJobFileInfo (new browseLib.fileInfo (append jobQueuePathName "/" aJobFileName)))

		(if (and (aJobFileInfo.exists) (aJobFileInfo.isFile))
			(begin
				(setq aStatus "ready")
				(goto END:)
			)
		)

		;; Check (for other status)
		(setq aJobFileName (append aJobName ".qrun.*.*"))
		(setq aFileList (aJobQueueDir.entryList (append aJobFileName) Files: #(Name:)))
		(setq aFileCount (length aFileList))

		;; We're expecting a single Job File
		;(if (= aFileCount 0) (error: "RunQueue.getStatus: Job file does not exist"))
		;(if (> aFileCount 1) (error: "RunQueue.getStatus: Duplicate job files exist"))
		(if (= aFileCount 0) (return "missing"))
		(if (> aFileCount 1) (return "duplicate"))
		

		(setq aStatus aFileList[0])

		(setq aJobFileName (append aJobName ".qrun."))
		(setq aIndex1 (length aJobFileName)) ;; End of the Job Name
		(setq aIndex2 (length aStatus)) ;; End of of the Job FileName

		;; Omit the Job Name
		;; status.workerContext
		(setq aStatus (substring aStatus aIndex1 aIndex2))

		;; Get the status
		(setq aStatus (substring aStatus 0 (- (find "." aStatus) 1)))
		END::
	aStatus) ; end of getStatus

	;; Removes a job from the JobQueue
	;; jobName - a string which identifies the job
	;; majorName - optional, major name or number of the job
	;; minorName - optional, minor name or number of the job
	;; seriesName - optional, series name or number of the job
	(defun remove(jobName ...)
		vars: (aStatus
			   aJobName
			   aJobQueueDir
			   aJobFileName
			   aFileList
			   aFileCount
			   aIndex
			   aMajorName
			   aMinorName
			   aSeriesName)

		;; Get optional arguments
		(if (> (argCount) 1) (setq aMajorName (argFetch 1)))
		(if (> (argCount) 2) (setq aMinorName (argFetch 2)))
		(if (> (argCount) 3) (setq aSeriesName (argFetch 3)))

		;; jobName is required
		(if (= jobName #void) (error: "RunQueue.remove: missing jobName argument"))
		(if (not (isString jobName)) (error: "RunQueue.remove: invalid jobName argument"))

		;; Check Job Queue Path
		(if (= jobQueuePathName #void) (error: "RunQueue.remove: jobQueuePathName not defined"))

		;; Check if Job Queue exists
		(setq aJobQueueDir (new browseLib.dir jobQueuePathName))
		(if (not (aJobQueueDir.exists)) (error: "RunQueue.remove: jobQueue directory does not exist"))

		(setq aJobName (append jobName))
		(if (not (= aMajorName #void)) (setq aJobName (append aJobName "_" aMajorName)))
		(if (not (= aMinorName #void)) (setq aJobName (append aJobName "_" aMinorName)))
		(if (not (= aSeriesName #void)) (setq aJobName (append aJobName "_" aSeriesName ".")))

		;; Matches jobname_*, jobname_major_*, jobname_major_minor_*
		(if (<> (right aJobName 1) ".")
			(begin
				(setq aJobFileName (append aJobName "_*"))
				(setq aFileList (aJobQueueDir.entryList (append aJobFileName) Files: #(Name:)))
				;; Matches jobname.*, jobname_major.*, jobname_major_minor.*
				(setq aJobName (append aJobName "."))
			)
		)

		;; Matches jobname_major_minor_series.*
		(setq aJobFileName (append aJobName "*"))
		(setq aFileList (append aFileList (aJobQueueDir.entryList (append aJobFileName) Files: #(Name:))))
		(setq aFileCount (length aFileList))

		;; We're expecting one or more files (ignore if already removed)
		;; (if (= aFileCount 0) (error: "RunQueue.remove: Job files do not exist"))
		(loop for aIndex from 0 until aFileCount do
			(aJobQueueDir.remove aFileList[aIndex])
		) ; loop
	true) ; end of remove

	;; ******************************************************************************
	;; Private Lambda Definitions
	;; ******************************************************************************
	;; The self testing utility for runQueue.
	(defun selfTest(testName)
		vars:(aBuffer aFile aJobConfig aData aJobId aStatus aResult aLambda)
        
        ;; Perform the various self tests by name
        (cond 
          ;; Perform a test where runQueue is both supplier and subscriber.
          ((= testName both:)
          	(begin
				;; Content of lambda
                ;; Notes:(MFK) Let's make this job more meaty - say a linear regression which takes very little time.
                ;;             We should add the worker lambda to RunQueue (see Study Harness).
				(setq aLambda (checkoutRegressionLambda 1 .10))
				;; Content of data source
                (setq aData (checkoutDynamicDataLambda "CrossCorrelation" 1000 5 .10))
			
				;; TEST #1: Lambda and Data Arguments

				;; Construct Job configuration
				(setq aJobConfig (new Structure:
						JobName: "selfTest"
						MajorName: 0
						MinorName: 0
						SeriesName: 1
						MinDiskFree: 1024			;; 1024 MB
						MinMemoryFree: 150			;; 150 MB
						RequiredLibraries: "gsm math javaScript rulesLib"
						))

				;; Submit Job
				(runQueue.submit aJobConfig aData aLambda)

				;; Check Initial Job Status
				(setq aStatus (runQueue.getStatus "selfTest" 0 0 1))
				(if (not (= aStatus "ready")) (error: "RunQueue.selfTest: ready status expected"))
				(writeln "SelfTest:   Job Status: " aStatus " [OK]")

				;; Run Job
                ;; Notes:(MFK) Let's add calls to make sure we run only jobs we're capable of running.
                ;;             We should check for lambda library dependencies, memory requirements, etc.
                ;; Notes:(FCC) Should we enhance the dir class exposure to allow us to check available disk space?
				(if (not (runQueue.run true)) (error: "RunQueue.selfTest: run failed"))

				;; Check Post-run Job Status
				(setq aStatus (runQueue.getStatus "selfTest" 0 0 1))
				(if (not (= aStatus "done")) (error: "RunQueue.selfTest: done status expected"))
				(writeln "SelfTest:   Job Status: " aStatus " [OK]")

				;; Check Results
                ;; Notes:(MFK) Let's really beef up the results returned.
                ;;             We should know how long the job took, when it started, when it ended, which subscriber ran it, etc. (see Study Harness).
				(setq aResult (runQueue.getResult "selfTest" 0 0 1))
				(if (not (isStructure aResult)) (error: "RunQueue.selfTest: unexpected result: " aResult))
				(writeln "selfTest:   Job Subscriber:     " aResult.SubscriberName)
				(writeln "SelfTest:   Job Start Time:     " aResult.JobStartTime)
				(writeln "SelfTest:   Job End Time:       " aResult.JobEndTime)
				(writeln "SelfTest:   Job Execution Time: " aResult.LambdaExecutionTime)
				(writeln "SelfTest:   Job Return Value:   " aResult.LambdaReturn)

				;; Remove Job Files
				(runQueue.remove "selfTest" 0 0 1)

				;; TEST #2: Lambda and Data Files

				;; Export unnamed lambda to source file
				(setq aFile (fileOpen "MySource.sl" 1 0))
				(fileWrite aFile aLambda)
				(setq aFile (fileClose aFile 1))

				;; Export data source to data file
				(setq aFile (fileOpen "MyData.sl" 1 0))
				(fileWrite aFile aData)
				(setq aFile (fileClose aFile 1))

				;; Construct Job configuration
				(setq aJobConfig (new Structure:
						JobName: "selfTest"
						MajorName: 0
						MinorName: 0
						SeriesName: 2
						MinDiskFree: 1024			;; 1024 MB
						MinMemoryFree: 150			;; 150 MB
						RequiredLibraries: "gsm math javaScript rulesLib"
						LambdaSourceFileName: "MySource.sl"
						DataSourceFileName: "MyData.sl"
						))

				;; Submit Job
				(runQueue.submit aJobConfig #void #void)

				;; Check Initial Job Status
				(setq aStatus (runQueue.getStatus "selfTest" 0 0 2))
				(if (not (= aStatus "ready")) (error: "RunQueue.selfTest: ready status expected"))
				(writeln "SelfTest:   Job Status: " aStatus " [OK]")

				;; Run Job
				(if (not (runQueue.run true)) (error: "RunQueue.selfTest: run failed"))

				;; Check Post-run Job Status
				(setq aStatus (runQueue.getStatus "selfTest" 0 0 2))
				(if (not (= aStatus "done")) (error: "RunQueue.selfTest: done status expected"))
				(writeln "SelfTest:   Job Status: " aStatus " [OK]")

				;; Check Results
				(setq aResult (runQueue.getResult "selfTest" 0 0 2))
				(if (not (isStructure aResult)) (error: "RunQueue.selfTest: unexpected result: " aResult))
				(writeln "selfTest:   Job Subscriber:     " aResult.SubscriberName)
				(writeln "SelfTest:   Job Start Time:     " aResult.JobStartTime)
				(writeln "SelfTest:   Job End Time:       " aResult.JobEndTime)
				(writeln "SelfTest:   Job Execution Time: " aResult.LambdaExecutionTime)
				(writeln "SelfTest:   Job Return Value:   " aResult.LambdaReturn)

				;; Remove Job Files
				(runQueue.remove "selfTest" 0 0 2)

				;; Delete extra files
				(setq aFile (fileOpen "MySource.sl" 1 0))
				(setq aFile (fileClose aFile 0))
				(setq aFile (fileOpen "MyData.sl" 1 0))
				(setq aFile (fileClose aFile 0))

           	)) ; end both case
          	;; Perform a test where runQueue is both supplier and subscriber.
          	(else (error (append "runQueue.selfTest: unknown self test name [" testName "]")))    
		) ; end cond
      
		true) ; end selftest

	;; ******************************************************************************
	;; Start Main Logic (System Initialization)
	;; ******************************************************************************
	vars:(queuePathName sqlAccess)

   	;; Validate and retrieve initialization arguments
	(if (<> command local:) (error "RunQueue: invalid initialization command"))
	(if (and (>= (argCount) 2) (not (isString (setq queuePathName (argFetch 1))))) (error "RunQueue: invalid or missing queuePathName argument"))
	(if (and (>= (argCount) 3) (not (isString (setq sqlAccess (argFetch 2))))) (error "RunQueue: invalid or missing sqlAccess argument"))

   	;; Initialize system configuration settings
    (setq jobQueuePathName (dataConfig queuePathName sqlAccess))  

	true) ; end runQueue

;;**EXPORTKEY**:runQueue.%DynamicCrossCorrelationRegressionData
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
;;    (setq aTemplate (mid (browseLib.checkout RunQueue: "runQueue:%DynamicCrossCorrelationRegressionData") 7 1000000))    
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

;;**EXPORTKEY**:runQueue.%DynamicCubicRegressionData
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
;;    (setq aTemplate (mid (browseLib.checkout RunQueue: "runQueue:%DynamicCubicRegressionData") 7 1000000))    
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

;;**EXPORTKEY**:runQueue.%DynamicCyclicSeriesRegressionData
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
;;    (setq aTemplate (mid (browseLib.checkout RunQueue: "runQueue:%DynamicCyclicSeriesRegressionData") 7 1000000))    
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

;;**EXPORTKEY**:runQueue.%DynamicElipsoidRegressionData
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
;;    (setq aTemplate (mid (browseLib.checkout RunQueue: "runQueue:%DynamicElipsoidRegressionData") 7 1000000))    
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

;;**EXPORTKEY**:runQueue.%DynamicHiddenModelRegressionData
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
;;    (setq aTemplate (mid (browseLib.checkout RunQueue: "runQueue:%DynamicHiddenModelRegressionData") 7 1000000))    
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

;;**EXPORTKEY**:runQueue.%DynamicHyperTangentRegressionData
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
;;    (setq aTemplate (mid (browseLib.checkout RunQueue: "runQueue:%DynamicHyperTangentRegressionData") 7 1000000))    
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

;;**EXPORTKEY**:runQueue.%DynamicLinearRegressionData
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
;;    (setq aTemplate (mid (browseLib.checkout RunQueue: "runQueue:%DynamicLinearRegressionData") 7 1000000))    
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

;;**EXPORTKEY**:runQueue.%DynamicMixedModelRegressionData
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
;;    (setq aTemplate (mid (browseLib.checkout RunQueue: "runQueue:%DynamicMixedModelRegressionData") 7 1000000))    
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

;;**EXPORTKEY**:runQueue.%DynamicRandomModalRegressionData
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
;;    (setq aTemplate (mid (browseLib.checkout RunQueue: "runQueue:%DynamicRandomModalRegressionData") 7 1000000))    
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

;;**EXPORTKEY**:runQueue.%DynamicRandomRootRegressionData
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
;;    (setq aTemplate (mid (browseLib.checkout RunQueue: "runQueue:%DynamicRandomRootRegressionData") 7 1000000))    
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

;;**EXPORTKEY**:runQueue.%DynamicRatioRegressionData
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
;;    (setq aTemplate (mid (browseLib.checkout RunQueue: "runQueue:%DynamicRatioRegressionData") 7 1000000))    
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

;;**EXPORTKEY**:runQueue.%DynamicRegressionData
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
;;    (setq aTemplate (mid (browseLib.checkout RunQueue: "runQueue:%DynamicRegressionData") 7 1000000))    
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

;;**EXPORTKEY**:runQueue.%DynamicRegressionLambda
;#text#
(lambda(theData)
;; ******************************************************************************
;; summary:  This unnamed lambda is an example of a dynamic data generation
;;           program which will create a new dynamic regression matrix every
;;           time it is run. 
;;
;; Args:     theData       The Structure with the regression problem data
;;                          TX			The training independent variable matrix.
;;           				TY          The training dependent variable vector.
;;           				X		    The testing independent variable matrix.
;;           				Y           The testing dependent variable vector.
;;                          N           The number of rows in the regression
;;                          M           The number of cols in the regression
;;                          Noise       The noise in the regression
;;                          Model       The training model used in the regression
;;
;; Return:   results	   A Structure containing the regression results.
;;                          N           		The number of rows in the regression 
;;                          M           		The number of cols in the regression
;;                          LinearTrainNLSE     The training linear normalized least squared error 
;;                          LinearTrainTCE      The training linear tail classification error  
;;                          BestTrainNLSE       The training nonlinear normalized least squared error 
;;                          BestTrainTCE        The training nonlinear tail classification error
;;                          TrainTime           The training time in elapsed seconds
;;                          LinearTestNLSE      The testing linear normalized least squared error 
;;                          LinearTestTCE       The testing linear tail classification error 
;;                          BestTestNLSE        The testing nonlinear normalized least squared error 
;;                          BestTestTCE         The testing nonlinear tail classification error
;;                          LinearAxis          The linear trained model axis
;;                          LinearCoefficients  The linear trained model coefficients
;;                          BestChampion        The best trained model
;;
;; ******************************************************************************
    regs:(m M n N)
	vars:((Gs $GENS$) (maxTime $TIME$) (Seed 21857348.0))
	vars:(results startTime endTime Lambda champions)

    ;; Initialize and set up the final data structure to be returned.
    (gsm.setOptions default: 0% false maxTime)    
    (setq results (new Structure: N: theData.N 
                                  M: theData.M 
                                  LinearTrainNLSE: #void 
                                  LinearTrainTCE: #void 
                                  BestTrainNLSE: #void 
                                  BestTrainTCE: #void
                                  TrainTime: #void
                                  LinearTestNLSE: #void 
                                  LinearTestTCE: #void 
                                  BestTestNLSE: #void 
                                  BestTestTCE: #void
                                  LinearAxis: #void
                                  LinearCoefficients: #void
                                  BestChampion: #void
                                  )) 

    ;; ****************************************************************************************************************************
    ;; Train on the training data
    ;; ****************************************************************************************************************************
    (setq startTime (getTickCount 0))
    (setq Lambda (gsm theData.TX theData.TY Gs .00 Seed))
    (setq endTime (getTickCount startTime))
    (if (not (isLambda Lambda)) (error "gsm training failed"))
    (setq champions (gsm.getEstimatorChampions false))
    (setq results.LinearTrainNLSE gsm.myBestLinear.NLSE) 
    (setq results.LinearTrainTCE gsm.myBestLinear.TCE) 
    (setq results.BestTrainNLSE Lambda.NLSE) 
    (setq results.BestTrainTCE Lambda.TCE)
    (setq results.TrainTime endTime)
    (setq results.LinearAxis gsm.myBestLinearAxis)
    (setq results.LinearCoefficients gsm.myBestLinearCoefficients)
    (setq results.BestChampion champions[0]) 

    ;; ****************************************************************************************************************************
    ;; Score on the testing data
    ;; ****************************************************************************************************************************
    (gsm.scoreTCEandNLSE gsm.myBestLinear theData.X theData.Y)
    (setq results.LinearTestNLSE gsm.myBestLinear.NLSE) 
    (setq results.LinearTestTCE gsm.myBestLinear.TCE) 
    (gsm.scoreTCEandNLSE Lambda theData.X theData.Y)
    (setq results.BestTestNLSE Lambda.NLSE) 
    (setq results.BestTestTCE Lambda.TCE) 

    ;; Return the regression results herein.
    results) ; end Dynamic Regression unnamed lambda

;;**EXPORTKEY**:runQueue.%DynamicSquareRootRegressionData
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
;;    (setq aTemplate (mid (browseLib.checkout RunQueue: "runQueue:%DynamicSquareRootRegressionData") 7 1000000))    
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

;;**EXPORTKEY**:runQueue.%DynamicTrigonometricRegressionData
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
;;    (setq aTemplate (mid (browseLib.checkout RunQueue: "runQueue:%DynamicTrigonometricRegressionData") 7 1000000))    
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

;;**EXPORTKEY**:runQueue.%DynamicUserModelRegressionData
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
;;    (setq aTemplate (mid (browseLib.checkout RunQueue: "runQueue:%DynamicUserModelRegressionData") 7 1000000))    
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

;;**EXPORTKEY**:runQueue.%RegressionDataDemo
;#text#
(lambda(...)
;; ******************************************************************************
;; summary:  This unnamed lambda is an example of a dynamic data generation
;;           program which will create a new dynamic regression matrix from
;;           tab delimited training and testing data files.
;;
;; Args:     none		
;;
;; Return:   theData		A Structure containing TX, TY, X, and Y regression data.
;;
;; Notes:    TrainX           The training independent variable matrix.
;;           TrainY           The training dependent variable vector.
;;           TestX            The testing independent variable matrix.
;;           TestY            The testing dependent variable vector.
;;           TestID           The testing ID vector.
;;           TranslationTable Variable translation table.
;;
;; ******************************************************************************
	vars:((aTrainingFileName "$TRAINING_FILE$") (aTestingFileName "$TESTING_FILE$"))
	vars:(aTrainingLoader				; loader for training data
		  aTestingLoader				; loader for testing data
		  aDependantColumnCount 		; no. of independent variable columns
		  aTranslationTable				; GSM translation table for selected training columns

		  aRecord						; temporary record variable
		  aTestXRow						; temporary record variable for Test X
		  aTrainXRow					; temporary record variable for Train X
		 )

    vars:(c i r)

	(defun basicLoader(...)
		pvars: (myParent ColVector ColCount RowVector RowCount)
		vars: (myCopy)
	
		;; receives a single record from importTab function
		(defun setImport(rowIndex rowRecord)
			vars: (aDelim aIndex aRecord aLength aValidFields)
			
			; extract record columns
			(setq aDelim #\tab)
			(setq aRecord (stringToVector rowRecord aDelim true true))
			(setq aLength (length aRecord))
			
			; parse record column data
			(loop for aIndex from 0 until aLength
				(setq aRecord[aIndex] (parse aRecord[aIndex]))
			) ; end of loop
	
			; the first row represents our column headers
			(if (= rowIndex 0)
				(begin
					(setq aValidFields (objectToStructure aRecord #(#void)))
					(setq ColVector (refAttributes aValidFields))
					(setq ColCount (length ColVector))
					(return true)
				)) ; end if
	
			; succeeding rows need to be attributed
			(setAttributes aRecord ColVector)
			(setq RowVector[RowCount] aRecord)
			(setq RowCount (length RowVector))
		RowCount) ; end of setImport
	
		;; imports a tab-delimited file
		(defun import(aFilename)
			vars: (aFileID)
			;; open data file
			(setq aFileID (fileOpen aFilename 0 0))
	
			(setq RowVector (new Vector:))
			(setq RowCount (length RowVector))
	
			;; import tab data file
			(importTab aFileID myParent recordsOnly:)
	
			;; close data file
			(setq aFileID (fileClose aFileID 1))
		RowCount) ; end of import
	
		;; main logic
		(setq myCopy (copy (myself)))
		(setq myCopy.myParent myCopy)
		myCopy) ; end of basicLoader

	;; load tab-delimited training data file
	(setq aTrainingLoader (basicLoader))
	(aTrainingLoader.import aTrainingFileName)

	;; load tab-delimited testing data file
	(setq aTestingLoader (basicLoader))
	(aTestingLoader.import aTestingFileName)

	;; make sure the training and testing data tables have the same column headers
	(loop for c from 0 until aTrainingLoader.ColCount do
		(if (<> aTrainingLoader.ColVector[c] aTestingLoader.ColVector[c])
			(error (append "Training Column " aTrainingLoader.ColVector[c] "does not match Testing Column " aTestingLoader.ColVector[c]))
		) ; end of if
	) ; end of loop

	;; we assume the columns follow this format:
	;; ID a b c d e ... Y
	;; Where:
	;; ID    - row identifier (first column)
	;; a..z  - independent variable
	;; Y     - dependent variable (last column)
	(setq aDependantColumnCount (- aTrainingLoader.ColCount 2))

	;; build translation table
	(setq aTranslationTable (new Vector: Object: aDependantColumnCount))
	(loop for c from 0 until aDependantColumnCount do
		(setq i (+ c 1))
		(setq aTranslationTable[c] (string aTrainingLoader.ColVector[i]))
	) ; end of loop
	
    ;; Initialize and set up the final data structure to be returned.
    (setq theData (new Structure: 
		TrainX: #void					; Training independent variable
		TrainY: #void					; Training dependent variable
		TestX: #void					; Testing independent variable
		TestY: #void					; Testing dependent variable
        TestID: #void					; List of Test ID's 
		TranslationTable: #void			; GSM translation table
		))

	;; load regression matrix from training data
	(setq theData.TrainX (new Vector: Object: aTrainingLoader.RowCount))
	(setq theData.TrainY (new Vector: Number: aTrainingLoader.RowCount))
	(loop for r from 0 until aTrainingLoader.RowCount do
		(setq aRecord aTrainingLoader.RowVector[r])
		(setq aTrainXRow (new Vector: Number: aDependantColumnCount))
		(loop for c from 0 until aDependantColumnCount do
			(setq aTrainXRow[c] aRecord[(+ c 1)])
		) ; end of loop: c 
		(setq theData.TrainX[r] aTrainXRow)
		(setq theData.TrainY[r] aRecord[(+ c 1)])
	) ; end of loop: r

	;; load regression matrix from testing data
	(setq theData.TestX (new Vector: Object: aTestingLoader.RowCount))
	(setq theData.TestY (new Vector: Number: aTestingLoader.RowCount))
	(setq theData.TestID (new Vector: Object: aTestingLoader.RowCount))
	(loop for r from 0 until aTestingLoader.RowCount do
		(setq aRecord aTestingLoader.RowVector[r])
		(setq aTestXRow (new Vector: Number: aDependantColumnCount))
		(loop for c from 0 until aDependantColumnCount do
			(setq aTestXRow[c] aRecord[(+ c 1)])
		) ; end of loop: c 
		(setq theData.TestX[r] aTestXRow)
		(setq theData.TestY[r] aRecord[(+ c 1)])
		(setq theData.TestID[r] (string aRecord[0]))
	) ; end of loop: r

	;; save the translation table
	(setq theData.TranslationTable aTranslationTable)

	(setq aTrainingLoader #void)
	(setq aTestingLoader #void)

    ;; Return the dynamic data generated herein.
    theData) ; end Dynamic Regress Data unnamed lambda

;;**EXPORTKEY**:runQueue.%RegressionLambdaDemo
;#text#
(lambda(theData)
;; ******************************************************************************
;; summary:  This unnamed lambda is an example of a dynamic data generation
;;           program which will create a new dynamic regression matrix every
;;           time it is run. 
;;
;; Args:     theData       The Structure with the regression problem data
;;                          TrainX              The training independent variable matrix.
;;           				TrainY              The training dependent variable vector.
;;           				TestX               The testing independent variable matrix.
;;           				TestY               The testing dependent variable vector.
;;
;; Return:   aResults	   A Structure containing the regression results.
;;
;; ******************************************************************************
    regs:(n N)
	vars:((aParametersFileName "$PARAMS_FILE$")
		  (aResultsFileName "$RESULTS_FILE$")
		  (aEstimatesFileName "$ESTIMATES_FILE$")
		  (aStatisticsFileName "$STATISTICS_FILE$")
		  aResults
		  aParametersLoader
		  aRecord
		  aGenerations
		  aMaxTime
		  aHaltingScore
		  aSVMKernelID
		  aNumChampions
		  aModelName
		  aChampions
		  aSampleSize
		  aRegressLambda
		  aText
		  aNumRows
		  aTestingEST
		  (aModelNames #(regressGSOALPS: regressSWARM: regressMVLALPS:))			;;Allowed list of ModelNames supported by GSM
		  (aSVMKernelIDs #(binary: bipolar: composite: cosine: cube: exp: linear: log: poly: quart: sigmoid: sine: square: tan: tanh:))	;;Allowed list of SVMKernelIDs supported by GSM
		 )
	vars:(r R)

	;; loads a tab-delimited file
	(defun basicLoader(...)
		pvars: (myParent ColVector ColCount RowVector RowCount)
		vars: (myCopy)
	
		;; receives a single record from importTab function
		(defun setImport(rowIndex rowRecord)
			vars: (aDelim aIndex aRecord aLength aValidFields)
			
			; extract record columns
			(setq aDelim #\tab)
			(setq aRecord (stringToVector rowRecord aDelim true true))
			(setq aLength (length aRecord))
			
			; parse record column data
			(loop for aIndex from 0 until aLength
				(setq aRecord[aIndex] (parse aRecord[aIndex]))
			) ; end of loop
	
			; the first row represents our column headers
			(if (= rowIndex 0)
				(begin
					(setq aValidFields (objectToStructure aRecord #(#void)))
					(setq ColVector (refAttributes aValidFields))
					(setq ColCount (length ColVector))
					(return true)
				)) ; end if
	
			; succeeding rows need to be attributed
			(setAttributes aRecord ColVector)
			(setq RowVector[RowCount] aRecord)
			(setq RowCount (length RowVector))
		RowCount) ; end of setImport
	
		;; imports a tab-delimited file
		(defun import(aFilename)
			vars: (aFileID)
			;; open data file
			(setq aFileID (fileOpen aFilename 0 0))
	
			(setq RowVector (new Vector:))
			(setq RowCount (length RowVector))
	
			;; import tab data file
			(importTab aFileID myParent recordsOnly:)
	
			;; close data file
			(setq aFileID (fileClose aFileID 1))
		RowCount) ; end of import
	
		;; main logic
		(setq myCopy (copy (myself)))
		(setq myCopy.myParent myCopy)
		myCopy) ; end of basicLoader

	;; load regression parameters
	(setq aParametersLoader (basicLoader))
    (aParametersLoader.import aParametersFileName)

	(loop for r from 0 until aParametersLoader.RowCount
		(setq aRecord aParametersLoader.RowVector[r])
		(cond
			((= aRecord[0] "Generations")
				(setq aGenerations aRecord[1])
				(if (or (not (isNumber aGenerations)) (< aGenerations 1))
					(error (append "Generations must be greater than 0 -- (" aGenerations ") specified"))))

			((= aRecord[0] "MaxTime")
				(setq aMaxTime aRecord[1])
				(if (or (not (isNumber aMaxTime)) (<= aMaxTime 0.0))
					(error (append "MaxTime must be greater than 0.0 -- (" aMaxTime ") specified"))))

			((= aRecord[0] "ModelName")
				(setq aModelName (symbol aRecord[1]))
				(if (not (isMember aModelName aModelNames))
					(error (append "ModelName must be one of " aModelNames))))

			((= aRecord[0] "HaltingScore")
				(setq aHaltingScore aRecord[1])
				(if (or (not (isNumber aHaltingScore)) (< aHaltingScore 0.0))
					(error (append "HaltingScore must be greater than or equal to 0.0 -- (" aMaxTime ") specified"))))

			((= aRecord[0] "SVMKernelID")
				(setq aSVMKernelID (symbol aRecord[1]))
				(if (not (isMember aSVMKernelID aSVMKernelIDs))
					(error (append "SVMKernelID " aSVMKernelID " specified not of " aSVMKernelIDs))))

			((= aRecord[0] "NumChampions")
				(setq aNumChampions aRecord[1])
				(if (or (not (isNumber aNumChampions)) (< aNumChampions 1) (< aNumChampions 25))
					(error (append "NumChampions must be greater than 0 and less than 25 -- (" aNumChampions ") specified"))))
		);cond
	);r

	(setq aSampleSize (length theData.TrainX))
	(writeln "GSM Regression is starting training on [" aSampleSize "] training examples.")

	;; set the regression user-defined learning options
    ;; (gsm.setOptions ModelName randomError verboseSW [Maxtime] [SVMKernelID])  [] indicates optional parameters
	(gsm.setOptions aModelName 0% false aMaxTime aSVMKernelID) 

	(writeln "begin training")

	;; run the gsm regression
	;; (gsm aTrainX aTrainY MaxGenerations HaltingScore [RandomNumberSeed])  [] indicate optional values
	(gsm theData.TrainX theData.TrainY (integer aGenerations) (number aHaltingScore))

	;; get list of champions
	(setq aChampions (gsm.getEstimatorChampions false theData.TranslationTable))
	(if (> aNumChampions (length aChampions)) (setq aNumChampions (length aChampions)))

    ;; intialize and set up the final data structure to be returned.
    (setq aResults (new Structure: EstimatesVector: #void
								   StatisticsVector: #void
								   EstimatesColumns: #void
								   StatisticsColumns: #void
								   Generations: #void))

	;; 1st column: ID
	;; 2nd column: Linear
	;; 3rd to last: Non-Linear
	(setq aResults.EstimatesColumns (new Vector:))
	(setq aResults.EstimatesColumns[0] "ID")
	(setq aResults.EstimatesColumns[1] "Y00")
	(loop for c from 0 until aNumChampions
		(setq aResults.EstimatesColumns[(length aResults.EstimatesColumns)] (append "Y" (right (append "00" (+ c 1)) 2)))
	) ; end of loop

	(setq aResults.EstimatesVector (new Vector:))
	(setq aNumRows (length theData.TestY))
	(loop for c from 0 until aNumRows do
		(setq aRecord (new Vector: (+ aNumChampions 2)))
		(setq aRecord[0] theData.TestID[c])
		(setq aResults.EstimatesVector[(length aResults.EstimatesVector)] aRecord)
	) ; end of loop

	;; Desc, Data, NLSE, TCE, RSQ, WFF
	(setq aResults.StatisticsColumns #("Desc" "Data" "NLSE" "TCE" "RSQ" "WFF"))
	(setq aResults.StatisticsVector (new Vector:))

	(setq aRegressLambda gsm.myBestLinear)
	;; score updates aRegressLambda with results against TRAINING set
	(gsm.scoreTCEandNLSE aRegressLambda)

	(setq N (length aRegressLambda.Mvl.myW)) ;;coefficient vector
	;; construct LinearWWF
	(setq aText (append "y = " aRegressLambda.A))
	(loop for n from 0 until N do
		(setq aText (append aText " + (" (* aRegressLambda.Mvl.myW[n] aRegressLambda.B) " * " theData.TranslationTable[n] ")"))
	)

	;; for the statistics, we'll have 2 entries for each champion (including the best linear)
	(setq aResults.StatisticsVector (new Vector:))

	(setq aRecord (new Vector:))
	(setq aRecord[0] "Linear")
	(setq aRecord[1] "Training")
	(setq aRecord[2] aRegressLambda.NLSE)
	(setq aRecord[3] aRegressLambda.TCE)
	(setq aRecord[4] aRegressLambda.RSQ)
	(setq aRecord[5] aText)
	(setq aResults.StatisticsVector[(length aResults.StatisticsVector)] aRecord)

	;; score updates aRegressLambda with results against TESTING set
	(gsm.scoreTCEandNLSE aRegressLambda theData.TestX theData.TestY)

	(setq aRecord (new Vector:))
	(setq aRecord[0] "Linear")
	(setq aRecord[1] "Testing")
	(setq aRecord[2] aRegressLambda.NLSE)
	(setq aRecord[3] aRegressLambda.TCE)
	(setq aRecord[4] aRegressLambda.RSQ)
	(setq aRecord[5] aText)
	(setq aResults.StatisticsVector[(length aResults.StatisticsVector)] aRecord)

	;; insert linear testing estimates into aEstimatesVector
	;; generate estimates
	(setq aTestingEST (aRegressLambda.run theData.TestX))
	(setq R (length aTestingEST))
	(loop for r from 0 until R do
		(setq aResults.EstimatesVector[r][1] aTestingEST[r])
	)

	;; generate estimates, statistics entries for each champion
	;; Get list of Champions -- gsm returns up to 25 champions.
	;;(setq aChampions (gsm.getEstimatorChampions false theData.TranslationTable))
	;;(if (> aNumChampions (length aChampions)) (setq aNumChampions (length aChampions)))
	(loop for c from 0 until aNumChampions do
		;; get regression lambda for champion
		(setq aRegressLambda gsm.myPopulation[c])

		;; score updates aRegressLambda with results against TRAINING set
		(gsm.scoreTCEandNLSE aRegressLambda)
	    (setq aRecord (new Vector:))
		(setq aRecord[0] (append "Champion" (right (append "00" (+ c 1)) 2)))
		(setq aRecord[1] "Training")
		(setq aRecord[2] aRegressLambda.NLSE)
		(setq aRecord[3] aRegressLambda.TCE)
		(setq aRecord[4] aRegressLambda.RSQ)
		(setq aRecord[5] aChampions[c])
		(setq aResults.StatisticsVector[(length aResults.StatisticsVector)] aRecord)

		;; score updates aRegressLambda with results against TESTING set
		(gsm.scoreTCEandNLSE aRegressLambda theData.TestX theData.TestY)
	    (setq aRecord (new Vector:))
		(setq aRecord[0] (append "Champion" (right (append "00" (+ c 1)) 2)))
		(setq aRecord[1] "Testing")
		(setq aRecord[2] aRegressLambda.NLSE)
		(setq aRecord[3] aRegressLambda.TCE)
		(setq aRecord[4] aRegressLambda.RSQ)
		(setq aRecord[5] aChampions[c])
		(setq aResults.StatisticsVector[(length aResults.StatisticsVector)] aRecord)

		;; insert non-linear testing estimates into EstimatesVector
		;; generate estimates
		(setq aTestingEST (aRegressLambda.run theData.TestX)) 
		(setq R (length aTestingEST))
		(loop for r from 0 until R do
			(setq aResults.EstimatesVector[r][(+ c 2)] aTestingEST[r]) ;; Offset column by 2 for ID and Y00 (linear estimate)
		)
	) ; end of loop

	(setq aResults.Generations gsm.myGc)

    ;; return the regression results herein.
    aResults) ; end Dynamic Regression unnamed lambda

;;**EXPORTKEY**:runQueue.%StudyHarnessData
;#text#
(lambda(...)
;; ******************************************************************************
;; summary:  This unnamed lambda is an example of a dynamic data generation
;;           program which will create a new dynamic regression matrix from
;;           a tab delimited file.
;;
;; Args:     none		
;;
;; Return:   theData		A Structure containing TX, TY, X, and Y regression data.
;;
;; Notes:    TrainX           The training independent variable matrix.
;;           TrainY           The training dependent variable vector.
;;           TestX            The testing independent variable matrix.
;;           TestY            The testing dependent variable vector.
;;           TestID           The testing ID vector.
;;           TranslationTable Variable translation table.
;;
;; Sample Usage:
;;    (setq aTemplate (mid (browseLib.checkout RunQueue: "runQueue:%StudyHarnessData") 7 1000000))
;;    (setq aDataLambda (substitute aTemplate "$PERIOD$" "4"))
;;    (setq aDataLambda (substitute aDataLambda "$WINDOW_SIZE$" "3"))
;;    (setq aDataLambda (substitute aDataLambda "$FORWARD_PERIODS$" "1"))
;;    (setq aDataLambda (substitute aDataLambda "$TESTING_PERIODS$" "1"))
;;    (setq aDataLambda (substitute aDataLambda "$ID_COLUMN$" "1"))
;;    (setq aDataLambda (substitute aDataLambda "$TARGET_COLUMN$" "3"))
;;    (setq aDataLambda (substitute aDataLambda "$TRAINING_COLUMNS$" "#(4 5 6 7 8 9 10 11 12)"))
;;    (setq aDataLambda (substitute aDataLambda "$SOURCE_DATA_FILE$" "Studies/FFEarnStudy01/ff_quarterly_fix.xls"))
;;    (setq aDataLambda (eval (compile (lisp aDataLambda))))
;;    (setq aJobData (aDataLambda))
;; ******************************************************************************
	vars:((aSourceDataFileName "$SOURCE_DATA_FILE$")	; filename of source data file
		  (aPeriod $PERIOD$)							; current period
		  (aWindowSize $WINDOW_SIZE$)					; training window size (in-sample training)
		  (aForwardPeriods $FORWARD_PERIODS$)			; number of periods to look ahead in training
		  (aTestingPeriods $TESTING_PERIODS$)			; number of out-of-sample test periods
		  (aIdColumn $ID_COLUMN$)						; index of id column
		  (aTargetColumn $TARGET_COLUMN$)				; index of target column
		  (aTrainingColumns $TRAINING_COLUMNS$)			; vector of indexes for independent variables
		 )
	vars:(aDataLoader					; loader for training and testing data
		  aTranslationTable				; GSM translation table for selected training columns

		  aCalcTestingPeriods
		  aNumPeriods					; total number of periods available
		  aTrainingColumnCount			; no. of indepedent variable columns
		  aPeriodsIndex					; index for available periods
		  aTestingPeriod				; temporary period variable
		  aIndexValue					; temporary variable for index value
		  aRecord						; temporary record variable
		  aTestXRow						; temporary record variable for Test X
		  aTrainXRow					; temporary record variable for Train X
		 )

    vars:(c i r R p P)

	(defun basicLoader(...)
		pvars: (myParent ColVector ColCount RowVector RowCount)
		vars: (myCopy)
	
		;; receives a single record from importTab function
		(defun setImport(rowIndex rowRecord)
			vars: (aDelim aIndex aRecord aLength aValidFields)
			
			; extract record columns
			(setq aDelim #\tab)
			(setq aRecord (stringToVector rowRecord aDelim true true))
			(setq aLength (length aRecord))
			
			; parse record column data
			(loop for aIndex from 0 until aLength
				(setq aRecord[aIndex] (parse aRecord[aIndex]))
			) ; end of loop
	
			; the first row represents our column headers
			(if (= rowIndex 0)
				(begin
					(setq aValidFields (objectToStructure aRecord #(#void)))
					(setq ColVector (refAttributes aValidFields))
					(setq ColCount (length ColVector))
					(return true)
				)) ; end if
	
			; succeeding rows need to be attributed
			(setAttributes aRecord ColVector)
			(setq RowVector[RowCount] aRecord)
			(setq RowCount (length RowVector))
		RowCount) ; end of setImport
	
		;; imports a tab-delimited file
		(defun import(aFilename)
			vars: (aFileID)
			;; open data file
			(setq aFileID (fileOpen aFilename 0 0))
	
			(setq RowVector (new Vector:))
			(setq RowCount (length RowVector))
	
			;; import tab data file
			(importTab aFileID myParent recordsOnly:)
	
			;; close data file
			(setq aFileID (fileClose aFileID 1))
		RowCount) ; end of import
	
		;; main logic
		(setq myCopy (copy (myself)))
		(setq myCopy.myParent myCopy)
		myCopy) ; end of basicLoader

	;; load tab-delimited data file
	(setq aDataLoader (basicLoader))
	(aDataLoader.import aSourceDataFileName)

	;; build index from collected data
	(setq aIndexValue #void)
	(setq aPeriodsIndex (new Vector:))
	(setq i -1)
	(setq R aDataLoader.RowCount)
	(loop for r from 0 until R do
		(setq aRecord aDataLoader.RowVector[r])
		(if (<> aRecord[0] aIndexValue)
			(begin
			(if (<> aIndexValue #void)
				(begin
				(++ i)
				(setq aPeriodsIndex[i] aIndexValue)
				) ; end of begin
			) ; end of if
			(setq aIndexValue aRecord[0])
			) ; end of begin
		) ; end of if
	) ; end of loop
	(if (= i -1) (error (append "Could not build periods index")))
	(setq aNumPeriods (length aPeriodsIndex))

	; use all columns by default (includes the ID column as well)
	(if (= aTrainingColumns #void)
		(begin
		(setq aTrainingColumnCount (aDataLoader.ColCount))
		(setq aTrainingColumns (new Vector: aTrainingColumnCount))
		(loop for c from 0 until aTrainingColumnCount do
			(setq aTrainingColumns[c] c)
		) ; end of loop
		)
	) ; end of if
	(setq aTrainingColumnCount (length aTrainingColumns))

	;; build translation table
	(setq aTranslationTable (new Vector: Object: aTrainingColumnCount))
	(loop for c from 0 until aTrainingColumnCount do
		(setq aTranslationTable[c] (string aDataLoader.ColVector[aTrainingColumns[c]]))
	) ; end of loop

	;; check parameters
	(if (< aForwardPeriods 0)
		(error (append "ForwardPeriods must be greater than 0")))

	(if (< aPeriod (+ aWindowSize aForwardPeriods))
		(error (append "Not enough preceeding data to do regression on period (" aPeriod ") -- Check window size (" WindowSize ")")))
		
	(if (>= aPeriod (- aNumPeriods aForwardPeriods))
		(error (append "Not enough future data to do regression on period (" aPeriod ") -- total periods (" aNumPeriods ")")))

	(if (< aWindowSize 3)
		(error (append "WindowSize must be greater than 3 -- (" aWindowSize ") specified")))

	(if (< aTestingPeriods 1)
		(error (append "TestingPeriods must be 1 or more -- (" aTestingPeriods ") specified")))

	;; build regression matrix
	
	(setq p (- aPeriod aForwardPeriods)) ;; we have to step back into history by aForwardPeriods to avoid forward bias
	(setq P (- aPeriod aWindowSize aForwardPeriods)) ;; P is the earliest period we will pull data from

	(if (< P 0)
		(error "Combination period values result in first period value less than 0"))

    ;; Initialize and set up the final data structure to be returned.
    (setq theData (new Structure: 
		TrainX: #void					; Training independent variable
		TrainY: #void					; Training dependent variable
		TestX: #void					; Testing independent variable
		TestY: #void					; Testing dependent variable
        TestID: #void					; List of Test ID's 
		TranslationTable: #void			; GSM translation table
		))

	(setq theData.TrainX (new Vector: Object: 0))
	(setq theData.TrainY (new Vector: Number: 0))
	(setq theData.ID (new Vector: 0))
	(setq i 0)
	(setq R aDataLoader.RowCount)
	(while (> p P) do ; stepping backwards through each period in training window
		(setq aIndexValue aPeriodsIndex[p])
		(loop for r from 0 until R do ; Collect all the data records that share the current period's period index value
			(setq aRecord aDataLoader.RowVector[r])
			(if (= aRecord[0] aIndexValue)
				(begin
				(setq aTrainXRow (new Vector: Number: aTrainingColumnCount))
				(loop for c from 0 until aTrainingColumnCount do ;for each training column
					(setq aTrainXRow[c] aRecord[aTrainingColumns[c]])
				);c
				(setq theData.TrainX[i] aTrainXRow)
				(setq theData.TrainY[i] aDataLoader.RowVector[r][aTargetColumn])
				(setq theData.ID[i] aDataLoader.RowVector[r][aIdColumn])
				(++ i)
				))
			);j
			(-- p)
	);p

	;;Prepare an out of sample test data.
	;;This test data will use the currently regressed formula to estimate target column value for the number
	;;of periods specified in the aForwardPeriods argument.
	;;Later, after training, the gsm.scoreTCEandNLSE routine will be used to calculate the efficacy of the estimates.
	;;NOTE: also see the tests peformed on the aggregate regression output -- where the output
	;;from multiple regressions in different periods are collected and tested.
	
	(setq aCalcTestingPeriods (min aTestingPeriods (+ 1 (- aNumPeriods aPeriod aForwardPeriods))))
	(if (= aCalcTestingPeriods 0)
		(error "Not enough testing periods"))

	;; Generate TestX -- Future inputs to champion formula
	(setq theData.TestX (new Vector: Object: 0))
	(setq theData.TestY (new Vector: Number: 0))
	(setq i 0)
	(setq R aDataLoader.RowCount)
	(loop for p from 0 until aCalcTestingPeriods do ;;For each testing period
		(setq aTestingPeriod (+ aPeriod p))
		(setq aIndexValue aPeriodsIndex[aTestingPeriod])
		
		(loop for r from 0 until R do ;;For each row in table  -- look for period index match
			(setq aRecord aDataLoader.RowVector[r])
			(if (= aRecord[0] aIndexValue) (begin ;; Found period match so collect independant variables for period
				(setq aTestXRow (new Vector: Number: aTrainingColumnCount))
				(loop for c from 0 until aTrainingColumnCount do ; for each factor
					(setq aTestXRow[c] aRecord[aTrainingColumns[c]])
				);c 
				(setq theData.TestX[i] aTestXRow)
				(setq theData.TestY[i] aRecord[aTargetColumn])
				(++ i)
				))
		);r
	);p

	;; save the translation table
	(setq theData.TranslationTable aTranslationTable)

    ;; Return the dynamic data generated herein.
    theData) ; end Dynamic Regress Data unnamed lambda

;;**EXPORTKEY**:runQueue.%StudyHarnessLambda
;#text#
(lambda(theData)
;; ******************************************************************************
;; summary:  This unnamed lambda is an example of a dynamic data generation
;;           program which will create a new dynamic regression matrix every
;;           time it is run. 
;;
;; Args:     theData       The Structure with the regression problem data
;;                          TrainX              The training independent variable matrix.
;;           				TrainY              The training dependent variable vector.
;;           				TestX               The testing independent variable matrix.
;;           				TestY               The testing dependent variable vector.
;;
;; Return:   aResults	   A Structure containing the regression results.
;;
;; Sample Usage:
;;    (setq aRegressTemplate (mid (browseLib.checkout RunQueue: "runQueue:%StudyHarnessLambda") 7 1000000))
;;    (setq aRegressLambda (substitute aRegressTemplate "$MODEL_NAME$" "regressMVLALPS"))
;;    (setq aRegressLambda (substitute aRegressLambda "$SVMKERNEL_ID$" "composite"))
;;    (setq aRegressLambda (substitute aRegressLambda "$MAX_TIME$" "3"))
;;    (setq aRegressLambda (substitute aRegressLambda "$GENERATIONS$" "500"))
;;    (setq aRegressLambda (substitute aRegressLambda "$HALTING_SCORE$" "0"))
;;    (setq aJobLambda (eval (compile (lisp aRegressLambda))))
;;    (setq aJobResult (aJobLambda aJobData))
;; ******************************************************************************
    regs:(n N)
	vars:((aModelName "$MODEL_NAME$")
		  (aSVMKernelID "$SVMKERNEL_ID$")
		  (aMaxTime $MAX_TIME$)
		  (aGenerations $GENERATIONS$)
		  (aHaltingScore $HALTING_SCORE$)
		  aNumVec
		  aText
		  aStartTime
		  aEndTime
		  aChampions
		  aBestLinear
		  aBestNonLinear
		  aResult)

	(setq aResult (new Structure:
		;; results
		SampleSize:				#void				; Number of samples passed to GSM
		LinearTrainingNLSE:		#void				;
		LinearTrainingTCE:		#void
		LinearTrainingRSQ:		#void
		LinearTestingNLSE:		#void
		LinearTestingTCE:		#void
		LinearTestingRSQ:		#void
		LinearWWF:				#void				;
		LinearYIntercept:		#void
		LinearCoefficients:		#void
		TrainingNLSE: 			#void				;..
		TrainingTCE: 			#void				;
		TrainingRSQ:			#void				;
		TrainingGenerations:	#void				; Training Generations
		TrainingTime:			#void				; Training Time
		TestingNLSE:			#void				; Normalized Leased Square Error
		TestingTCE:				#void				; T.. Classification Error
		TestingRSQ:				#void
		ID:						theData.ID			; Id values for each estimate value
		TestingEST:				#void				; TestingPeriods estimate values
		TestingActuals:			the.TestY			; TestingPeriods actual values
		LinearTestingEST:		#void
		WWF:					#void				; added after training
	))

	(setq aStartTime (getTickCount 0))

	(setq TrainX theData.TrainX)
	(setq TrainY theData.TrainY)
	(setq TestX theData.TestX)
	(setq TestY theData.TestY)

	(setq theData (remove TrainX: theData))
	(setq theData (remove TrainY: theData))
	(setq theData (remove TestX: theData))
	(setq theData (remove TestY: theData))

	(writeln "GSM Regression is starting training on [" (length TrainX) "] training examples.")
	(setq aResult.SampleSize (length TrainX))
		;; Set the Regression User Defined Learning Options
		;; (gsm.setOptions ModelName randomError verboseSW [Maxtime] [SVMKernelID])  [] indicates optional parameters
		(gsm.setOptions aModelName 0% false aMaxTime aSVMKernelID) ;; use this script now  
		;; (gsm TX TY MaxGenerations HaltingScore [RandomNumberSeed])  [] indicate optional values
		(gsm TrainX TrainY aGenerations aHaltingScore)
	
		(setq aBestNonLinear gsm.myBest)
		(setq aBestLinear gsm.myBestLinear)

		(setq aEndTime (getTickCount aStartTime))
	
		; Process results from "aModelName" gsm run
		(if (= aBestNonLinear #void)  (writeln "gsm run on " aModelName " are #void")
			(begin
			(setq aChampions (gsm.getEstimatorChampions false theData.TranslationTable))
			(gsm.scoreTCEandNLSE aBestNonLinear)

			(writeln "In Sample Regression Results, NLSE=[" aBestNonLinear.NLSE "], TCE=[" aBestNonLinear.TCE "], G=[" gsm.myGc "], RSQ=[" aBestNonLinear.RSQ "], Hours=[" (/ aEndTime 3600) "]")

			(setq N (length aChampions))
			(loop for n from 0 until N do  (writeln "Regression champion [" n "] = " aChampions[n]))
	
			(setq aResult.WWF: aChampions)
			(setq aResult.TrainingNLSE aBestNonLinear.NLSE)
			(setq aResult.TrainingTCE aBestNonLinear.TCE)
			(setq aResult.TrainingRSQ aBestNonLinear.RSQ)
			(setq aResult.TrainingGenerations gsm.myGc)
			(setq aResult.TrainingTime (/ aEndTime 3600))
	
			; Perform out of sample test using gsm.scoreTCEandNLSE
			(setq aBestNonLinear.NLSE 0.0)
			(setq aBestNonLinear.TCE 0.0)
			(gsm.scoreTCEandNLSE aBestNonLinear TestX TestY)

			; Add results to results structure
			(setq aResult.TestingNLSE aBestNonLinear.NLSE)
			(setq aResult.TestingTCE aBestNonLinear.TCE)
			(setq aResult.TestingRSQ aBestNonLinear.RSQ)

			(writeln "Out of Sample Regression Results: NLSE=[" aBestNonLinear.NLSE "], TCE=[" aBestNonLinear.TCE "], RSQ=[" aBestNonLinear.RSQ "]")
            (setq aResult.TestingEST (aBestNonLinear.run TestX))
			))
	
		(if (= aBestLinear #void)
			(writeln "Linear Regression Results #void")
			(begin
			(gsm.scoreTCEandNLSE aBestLinear)
			(writeln "In Sample Linear Regression Results, NLSE=[" aBestLinear.NLSE "], TCE=[" aBestLinear.TCE "], Hours=[" (/ aEndTime 3600) "]")

			(setq N (length aBestLinear.Mvl.myW)) ;;coefficient vector
			(writeln "Linear Regression coefficients:" aBestLinear.Mvl.myW)
			(writeln "Linear Regression A = " aBestLinear.A)
			(writeln "Linear Regression B = " aBestLinear.B)

			;;Construct LinearWWF
			(setq aNumVec (new Vector: N 0))
			(setq aText (append "y = " aBestLinear.A))
			(loop for n from 0 until N do
				(setq aNumVec[n] (* aBestLinear.Mvl.myW[n] aBestLinear.B)) ;;Composite coefficient
				(setq aText (append aText " + (" aNumVec[n] " * " theData.TranslationTable[n] ")"))
			)

			(writeln "Linear Regression Formula " aText)
			(setq aResult.LinearWWF: aText)
			(setq aResult.LinearTrainingNLSE aBestLinear.NLSE)
			(setq aResult.LinearTrainingTCE aBestLinear.TCE)
			(setq aResult.LinearTrainingRSQ aBestLinear.RSQ)
			(setq aResult.LinearCoefficients aNumVec)
			(setq aResult.LinearYIntercept aBestLinear.A)
	
			;;Perform out of sample test using gsm.scoreTCEandNLSE
			(setq aBestLinear.NLSE 0.0)
			(setq aBestLinear.TCE 0.0)
			(gsm.scoreTCEandNLSE aBestLinear TestX TestY)
			;; Add results to results structure
			(setq aResult.LinearTestingNLSE aBestLinear.NLSE)
			(setq aResult.LinearTestingTCE aBestLinear.TCE)
			(setq aResult.LinearTestingRSQ aBestLinear.RSQ)
			(writeln "Out of Sample Linear Regression Results: NLSE=[" aBestLinear.NLSE "], TCE=[" aBestLinear.TCE "] RSQ=[" aBestLinear.RSQ "]")
            (setq aResult.LinearTestingEST (aBestLinear.run TestX))
			)) ;;end of regression and out-of-sample test section

    ;; return the regression results herein.
    aResult) ; end Dynamic Regression unnamed lambda

;;**EXPORTKEY**:runQueue.dataConfig
(defriend runQueue:dataConfig(...)
;; ******************************************************************************
;; summary:  The dataConfig lambda implements the data connection, parsing, and
;;           construction logic generally necessary for the runQueue processing. 
;;
;; Args:     queuePathName      (Optional) JobQueue folder path name		
;;           sqlAccess          (Optional) SQL access information and passwords		
;;
;; Return:   queuePathName		JobQueue folder path name
;;
;; Depend:   browseLambda
;;           Gsm
;; ******************************************************************************
	pvars:(
		;; Persistent variables
		(pConfig #void)									;; Structure holding data connection information
		(pDataConfigFile "RunQueueDataConfig.ini") 		;; Name of default data connection information
		;; Child Lambdas
		parseLocation									;; Parse a location string and return a structure containing access information
		SQLConnect										;; connect to an sql db using dataConfig structure
		)
	;; ******************************************************************************
	;; Public Method Definitions
	;; ******************************************************************************
    ;; -------------------------------------------------------------------------------------
    ;; Parse a location string and return a structure containing access information	
    ;; parseLocation returns a structure with all of the location information necessary to 
	;; access a file or database connection. The string argument can be a file and/or folder 
	;; path. It may also start with the special token "MySql:" to indicate database access. 
	;; Database access string may have the following formats ([] indicating optional parts):
	;; "MySql:[server:][database.]table[,username,password]"
	;; Examples:
	;;	"MySql:mytable"
	;;	"MySql:192.168.121:mytable"
	;;	"MySql:mydatabase.mytable,goofy,bigshoes"
	;; Optional parameters are expected to be provided by the application in the pConfig structure
	;; and are returned for convienence in the structure returned by pareseLocation. See the
	;; loadConfig Lambda.
	;; Notes: ":" and "," characters have special meaning and may not be used in configuration strings like passwords and usernames
	;; dataConfig is extensible. It is possible to add additional data location types.
	(defun parseLocation(aString)
		vars:(aParts aSubString aNumParts aResult (aDb #void))
		(setq aResult (new Structure: IsFile: false IsDb: false Path: #void DBType: #void Server: #void Database: #void Table: #void User: #void Password:#void))
		(dataConfig) ;; load defaults if they have not already been loaded
		(if (and (<> (type aString) String:) (<> (type aString) Text:))
			(error "runQueue.dataConfig.parseLocation: argument must be a string."))

		;;Determine if a Database has been specified
		;; We do it this wall to allow for additional database connection types to be specified.
		;; For instance, in the future we may want to specify a connection to another Ais instance this way.
		(if (= (left (upcase aString) 6) "MYSQL:") (begin
			(setq aDb pConfig.MySql) 	;; get database defaults 
			(setq aResult.DBType MySql:)
			(setq aResult.IsDb true)
			))

		(if (<> aDb #void) (begin ;;Assume all DBs have the same elements in their parameter list for now
			(setq aParts (stringToVector aString ":"))
			(setq aNumParts (length aParts))
			(cond 
				((= aNumParts 2) ;ie "MySql:..."
					(setq aResult.Server aDb.Server) ;; Use global default database location url
					(setq aSubString aParts[1])
					)
				((= aNumParts 3) ;ie: "MySql:server:...") url found
					(setq aResult.Server aParts[1])
					(setq aSubString aParts[2])
					)
				(true (error "runQueue.dataConfig.parseLocation: Invalid location string passed. (" aString ")"))
				);end cond

			(setq aParts (stringToVector aSubString ","))
			(setq aNumParts (length aParts))
			(if (= aNumParts 0)
				(error "runQueue.dataConfig.parseLocation: Invalid location string passed. (" aString ")"))
			(if (> aNumParts 1)
				(setq aResult.User aParts[1])
				(setq aResult.User aDb.User))
			(if (> aNumParts 2)
				(setq aResult.Password aParts[2])
				(setq aResult.Password aDb.Password))

			(setq aSubString aParts[0])
			(setq aParts (stringToVector aSubString "."))
			(setq aNumParts (length aParts))
			(cond ; get datbase.table specification
				((= aNumParts 1)
					(setq aResult.Database aDb.Database)
					(setq aResult.Table aParts[0]))
				((= aNumParts 2)
					(setq aResult.Database aParts[0])
					(setq aResult.Table aParts[1]))
				(true (error "runQueue.dataConfig.parseLocation: Invalid location string passed (" aString ")"))
				);end cond

			;;Check that we enough information to actually connect to the database
;			(if (= aResult.Database #void) (begin
;				(writeln aResult)
;				(error "runQueue.dataConfi.parseLocation: Missing Default Database in " pDataConfigFile " file")))
;
;			(if (= aResult.Server #void) (begin
;				(writeln aResult)
;				(error "runQueue.dataConfi.parseLocation: Missing Default Server in "  pDataConfigFile " file")))
;
			(return aResult)
			)
		else (begin
			(setq aResult.IsFile true)
			(setq aResult.Path aString)
			(return aResult)
			))
	(return (new Structure: IsFile: false IsMySql: false ))
	) ; end parseLocation
	;; ******************************************************************************
	;; Private Method Definitions
	;; ******************************************************************************
    ;; connect to an sql db using dataConfig structure
	(defun SQLConnect(aSourceLoc)
		(if (not aSourceLoc.IsDb)
			(error "runQueue.dataConfig.SQLConnect: called on non SQL source " aSourceLoc))
		(cond
			((or (= aSourceLoc.Server #void) ;; Local embedded server connection
				 (= aSourceLoc.Server embedded:))
				(return (sql connect:))
			)
		) 
	) ; end SQLConnect
	;; ******************************************************************************
	;; Start Main Logic (Initialize the runQueue system)
	;; ******************************************************************************
	regs:(n N)
    vars:(queuePathName sqlAccess)		
	vars:(aConfig iniFileInfo iniFileID iniFileSource aConfigFile)

	;; Retrieve the optional arguments.
	(if (> (argCount) 0) (setq queuePathName (argFetch 0)))
	(if (> (argCount) 1) (setq sqlAccess (argFetch 1)))
    
	;; Load the settings in the RunQueue .ini file.
    ;; Note: the .ini file settings are saved as a Lisp Structure constant in the .ini file.
    (setq pConfig #void)
    (setq iniFileInfo (new browseLambda.fileInfo pDataConfigFile))
	(if (iniFileInfo.exists)
        then 
        (begin
		  (setq iniFileID (fileOpen pDataConfigFile 0 0))
	      (setq iniFileSource (fileRead iniFileID))
	      (setq iniFileID (fileClose iniFileID 1))
		  (setq pConfig (eval (compile (lisp iniFileSource))))
		  (if verboseSW (writeln "Using configuration specified in " pDataConfigFile))
		  ) 
        else
		(if verboseSW (writeln "Warning: Using default configuration as " pDataConfigFile " not found"))
		) ; end if

	;; Load the default settings in the RunQueue .ini file (if necessary).
    ;; Note: any optional arguments override the .ini file settings.
	(if (not (isStructure pConfig)) (setq pConfig (new Structure:)))
	(if (not (isMember JobQueue: pConfig)) (setq pConfig.JobQueue "JobQueue/"))
	(if (not (isMember MySql: pConfig))  
        (begin
		  (setq pConfig.MySql (new Structure:
		       	;;Configuration file settings defaluts -- configuration file entries override these values
                Embedded: true          ;; True iff embedded SQL is the server		
			  	Server: #void			;; IP address of MySql server
				Database: #void			;; MySql database to be accessed
				User: #void				;; MySql database user login
				Password: #void			;; MySql database user passwrod
				))
		)) ; end if

    ;; Any optional arguments, which are present, override the .ini file settings.
    (if (<> queuePathName #void)  (setq pConfig.JobQueue queuePathName))
    (if (<> sqlAccess #void)  (setq pConfig.MySql sqlAccess))

    ;; Return the JobQueue path and folder name.
    pConfig.JobQueue) ; end dataConfig


