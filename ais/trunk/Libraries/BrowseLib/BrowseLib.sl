;;**EXPORTKEY**:browseLib
(defun browseLib(...)
;; *******************************************************************
;; summary:  Manages all Lambda source code stored in the file cabinet.
;;           Includes checking source in and out of the file cabinet,
;;           importing, exporting, and deleting source from the file
;;           cabinet.
;; Args:     extentName:          Name of the new file cabinet repository extent.
;;           extentFileName:      Path and file name of new file cabinet repository extent, or
;;                                 the extent manager Lambda for managing the foreign extent.
;; Globals set by browseLib:
;;           _ais.browseLibExtents  The current browseLib extent structure.
;; Return:   true
;; *******************************************************************
	vars:(name s i n commands keys mainExtent altExtent)
	vars:(extentName extentFileName extentLocation extentStorageScope importSync exportSync autoCompile autoCompileList)
	pvars:(;; Persistent Variables
			LambdaCabinetList        ;; List of all Lambda cabinets added using addExtent
			dString                 ;; Delimited string result
			_compileName            ;; Name of Lambda being compiled
			htmlPageRegistry        ;; HTML page registry (see htmlPageServer)
			(maxLine 140)           ;; Maximum delimited string line length
			myImportFileID          ;; File ID during file cabinet import from exported source
			myImportBuffer          ;; File buffer during file cabinet import from exported source
			myImportName            ;; Next Lambda name during file cabinet import from exported source
			mySQLHandle	           ;; The SQL handle used for communicating with the MySQL database engine.
			mySQLDatabase	       ;; The SQL database used when communicating with the MySQL database engine.
			(myForceSettingsFlag false)	   ;; State flag used to force addExtent settings to be applied
			(myDelayAutoCompile false) ;;Flat cuases autoCompile flag on initial addExtent to set myCompileRequired to be set instead of immediate compile.
			(myVerboseCompile false)	;;Show progress of compileAllAutoCompileCabinets command
			myAutoCompileList
			;; Public Child Lambdas
			abortAll                ;; Close, with abort, all extents in File Cabinet
			addDataExtent           ;; Register a data file cabinet extent to the browse Lambda
			addExtent               ;; Register an Lambda file cabinet extent to the browse Lambda
			addHostTextExtent       ;; Register a host text file folder extent to the browse Lambda
			addMemoryExtent         ;; Register the memory manager extent to the browse Lambda
			checkin                 ;; Check an Lambda script into the File Cabinet
			checkout                ;; Check an Lambda script out of the File Cabinet
			checkoutLambda	       ;; Check an Lambda binary out of the File Cabinet
			checkoutParent          ;; Check a whole parent Lambda script including all its children
			clearCabinet            ;; Clear the specified File Cabinet of all data    
			closeImportSourceFile   ;; Close import source file.
			commitAll               ;; Close, with commit, all extents in File Cabinet
			compileAll              ;; Compile all Lambdas in File Cabinet
			compileAllAutoCompileCabinets
			markAutoCompileCabinet
			compileCabinet
			compileDependents       ;; Compile all Lambda cabinets dependent on the current file cabinet.
			compileSource           ;; Manage source compilation
			copyCabinetSource       ;; Copy the specified source File Cabinet contents to the specified destination File Cabinet   
			delimitedGlobals        ;; Convert globals to a delimited string
			delimitedString         ;; Convert object to a delimited string
			dir						;; dir Lambda surfaces the QDir object
			dropExtent              ;; Unregister a file cabinet extent from the browse Lambda
			eraseChildren           ;; Manage source erasing including all related child Lambdas
			eraseSource             ;; Manage source erasing
			errorHandler            ;; Handle errors gracefully
			exportSource            ;; Manage source exporting 
			fileInfo					;; fileInfo Lambda surfaces the QFileInfo object
			getChildNames           ;; Return a vector of all source names 
			getExtentCount          ;; Return the count of file cabinet extents 
			getExtentNames          ;; Return a vector of extent names 
			getExtentStatus         ;; Return a vector of extent names and their current status
			getFocus                ;; Return the name of the extent currently in focus 
			getFocusIndex           ;; Return the index of the extent currently in focus 
			getKeys                 ;; Return a vector of source names
			getNextLevel            ;; Return an XPath directory string list for the next repository directory level.
			getParentNames          ;; Return a vector of parent Lambda names 
			getTypes                ;; Return an XPath directory type list string for all possible types in a hierarchical descendents list 
			htmlPageServer          ;; Return the name of an html page for downloading
			htmlParmToStructure     ;; Convert the HTML parameter string to a structure
			importSmallSource       ;; Manage source importing for small files 
			importSource            ;; Manage source importing for files of all sizes
			inspect                 ;; Return a Structure of symbols and their closure size
			loadApplication		   ;; Loads the application from the specified cabinet
			memoryCursor  		   ;; Factory template for creating generic memory cursor Lambdas.
			openImportSourceFile    ;; Open source file for import 
			profiler                ;; Profiler for adding timing/profiling/statistical directives to active Lambdas 
			precompiler             ;; Precompiler for adding C-like precompiler directives to source files 
			readImportSourceRecord  ;; Read next import source file record 
			readSourceFile          ;; Read source file 
			removeDelimiters        ;; Remove all delimiters from a string 
			saveApplication		   ;; Saves the current application in the specified cabinet
			scanFile				   ;; Scans the source code in the specified file
			scanSource			   ;; Scans the specified source code string
			setFocus                ;; Set the specified extent in focus  
			showGlobals			   ;; Finds all global references in the specified Lambda
			sqlb                    ;; Issue the specified SQL command and return the results as a Brick.
			sqlc                    ;; Issue the specified SQL command and return the results as a memory cursor.
			sqld                    ;; Issue the specified SQL command and display the results on the console.
			stripAll                ;; Remove the source from every Lambda in the current file cabinet
			systemCheck             ;; Perform a complete inspection of each registered repository extent
			tabbedNameList          ;; Return a tab delimted string of Lambda script names
			takeout                 ;; Take an Lambda out of the File Cabinet
			writeSourceFile         ;; Write source file 
			;; Private Child Lambdas
			_dataManagerTemplate    ;; Factory for creating standard data manager Lambdas.
			_exportSource           ;; Manage source exporting 
			_extentManagerTemplate  ;; Factory for creating standard extent manager Lambdas.
			_hostTextManagerTemplate;; Factory for creating host text file folder extent manager Lambdas.
			_memoryManagerTemplate  ;; Factory for creating standard memory manager Lambdas.
			_initialize             ;; Initialize the browseLib (if necessary) 
			_sysErrorStop           ;; Recover of all system errors.

			;; New functions for database synchronization
			checkStatus             ;; Check Synchronization status for a cabinet
			displayData             ;; Display __DATA information of focused cabinet
			exportDirectory         ;; Manage source importing with Storage Scope Folder
			fileExists              ;; Checks if a file exists
			fileTimeStamp           ;; Returns the timestamp the file was last modified
			getMetaData             ;; Return a cabinet's metadata structure as a delimited string
			getMetaDataFromFile     ;; Create a metadata structure from a .sl file or directory, returns false if metadata information is invalid
			importDirectory         ;; Manage source exporting with Storage Scope Folder
			importSourceByDirectory ;; Manage individual files in a directory for import, this is used by importDirectory
			registerDirectory       ;; Registers all .dbs and .sls in a folder as a cabinet
			showAllCabinets         ;; Show list of all Lambda cabinets
			showLambdaCabinets       ;; Show list of all cabinets
			updateMetaData          ;; checks if the metadata is valid, if it is, update the metadata information for the focused cabinet
			validateMetaData        ;; checks if the metadata information is valid or not, returns true or false

			;; File System Watcher functions
			watchFile               ;; Add a file to the watch list 
			setWatchedFileFlag      ;; Set flag for the watched file
			displayWatchList        ;; Display the list of files being watched

			;; new data used for database synchronization / file system watcher functionality
			mySourceDirectory       ;; Open source file for import using folder as storage scope
			extentStatusList        ;; List conataining the status for each cabinet
			watchedFileList         ;; List of files being watched
			) ;; end of pvars
	;; Never initialize this Lambda more than once, because the
	;; inline child Lambdas will overlay the cloned child Lambdas
	;; in any clone copies of the Lambda and this causes serious
	;; confusion when the reinitialized clone begins to affect
	;; the persistant storage of the original Lambda.
	;; *******************************************************************
	;; Define New Public Child Lambdas
	;; *******************************************************************


	;;makeDefaultMetaData is called in many places to get a default
	;;meta data structure. To change global default settings, make the
	;;change here. Override a specific values in calling routines only
	;;when necessary.
	(defun makeDefaultMetaData()
		(new Structure:
			objRepoName: ""
			location: ""
			storageScope: file:
			importSync: none:
			exportSync: none:
			autoCompile: false
			searchSW: false
			dependencies: ""
			cabinetHash: (new Dictionary:)
		))

	;; Update Metadata Information on a cabinet.  Create a new structure if the information does not exist.
	(defun updateMetaData(iKey ... )
		vars:(n N A)
		vars:(aExtentInfo aExtentInfoArgument aOldFocus aCabinetName aLocation aStorageScope aImportSync aExportSync aAutoCompile aSearchSW)
		vars:(aExtMgr)
		(setq aOldFocus (getFocus))
		(setFocus iKey)
		(setq aExtentInfo (loadMetaData _ais.browseLibExtents.Focus.myRepository))
		(if (not (isStructure aExtentInfo))
			(setq aExtentInfo (makeDefaultMetaData)))

		;; Create extent info from the passed argument that will be merged to the current extent info
		(setq A (argCount))
		(if (> A 1) (begin
			(setq aExtentInfoArgument (new Structure:
				objRepoName: 	(if (> A 1) (argFetch 1) "")
				location: 		(if (> A 2) (argFetch 2) "")
				storageScope: 	(if (> A 3) (argFetch 3) file:)
				importSync: 	(if (> A 4) (argFetch 4) none:)
				exportSync: 	(if (> A 5) (argFetch 5) none:)
				autoCompile: 	(if (> A 6) (argFetch 6) false)
				searchSW: 		(if (> A 7) (argFetch 7) false)
				dependencies: 	""
				))
			))

		;; create a single string based on the argument dependencies
        (if (> A 8) (begin
        (loop for n from 8 until (argCount) do
            (setq aDependencies (append (argFetch n) " " ))
        )
        (setq aExtentInfo.dependencies (trim aDependencies))
        ))

		;;Merge aExtentInfoArgument into aExtentInfo
		;;Do not include the dependencies passed as arguments
		(setq N (length aExtentInfoArgument))
		(if (> 7 N) (setq N 8))
		(loop for n from 0 until N do
			(setq aExtentInfo[aExtentInfoArgument[n 0]] aExtentInfoArgument[n 1])
		)

		(if (validateMetaData aExtentInfo) (begin
			(saveMetaData _ais.browseLibExtents.Focus.myRepository aExtentInfo)
			;;TM? Rajah, We were not updating the aExtMgr values and this was probably an error?
			;;Update selected values into extent manager
			(setq aExtMgr _ais.browseLibExtents.Focus)
			;(setq aExtMgr.myExtentName iKey) ;TM? Do we need to updat this?
			(setq aExtMgr.mySearchSW aExtentInfo.searchSW)
			(if (and (not aExtentMgr.myAutoCompile) aExtentInfo.autoCompile)
				(setq aExtMgr.myCompileRequired true)
				(setq aExtMgr.myCompileRequired false))
			(setq aExtMgr.myAutoCompile aExtentInfo.autoCompile)
			;;Add saving of dependants information!
			)
		else
			(setFocus aOldFocus))
		(setq extentStatusList[(getFocus)] (checkStatus (getFocus)))
		(setFocus aOldFocus)
	true)
	;; Return the cabinet's metadata information, if it is not found return #void
	;; Format: <Cabinet Name> <Object Repository> <Source File Location> <Storage Scope> <ImportSync> <ExportSync> <Auto Compile Flag> <Search Switch> <Dependencies>
	;; Parameters are separated by #\tab
	;; The dependencies value is a #void or a string with the cabinet names separated by a comma
	(defun getMetaData(iCabinetName)
		vars:(aResult aExtentInfo aObjRepo aOldFocus)
		(if (= false (member iCabinetName _ais.browseLibExtents.Extents))
			(begin
			(setq aResult #void)
			)
			else
			(begin
			(setq aOldFocus (getFocus))
			(setFocus iCabinetName)
			(setq aExtentInfo (loadMetaData _ais.browseLibExtents.Focus.myRepository))
			(setq aResult (append iCabinetName #\tab aExtentInfo.objRepoName #\tab aExtentInfo.location #\tab aExtentInfo.storageScope 
					#\tab aExtentInfo.importSync #\tab aExtentInfo.exportSync #\tab aExtentInfo.autoCompile #\tab aExtentInfo.searchSW #\tab aExtentInfo.dependencies))
			(setFocus aOldFocus)
			)
		)
	aResult)

	;; Return a vector of all file cabinet extent names.
	(defun getExtentStatus()
		vars:(aExtentList aStatus n N aResult)
		;; Return false if there are no cabinets in the status list
		(if (= _ais.browseLibExtents.Extents #void)
			(return false))
		(setq aExtentList (refAttributes _ais.browseLibExtents.Extents))
		(setq N (length aExtentList))
		(loop for n from 0 until N do
			;; (if (<> ".Memory" aExtentList[n]))
			(if (and (<> #void _ais.browseLibExtents.Extents[n].Pv)
				(= true (isMember _extentManagerTemplate: _ais.browseLibExtents.Extents[n].Pv)))
				(begin
				;; The next line checks the current status of the cabinet, this takes time
				;(setq aResult (append aResult (checkStatus aExtentList[n]) _eol))
				;; The next line just uses the current state of the cabinet in the extentStatusList
				(setq aResult (append aResult extentStatusList[ aExtentList[n] ] _eol))
				)
			else
				(begin
				(setq aResult (append aResult "#void" #\tab "#void" #\tab "#void" #\tab "#void" #\tab aExtentList[n] #\tab "#void" _eol))
				)
			)
		)
	aResult)

	;; validateMetaData -- fixs medata types and returns true/false if metadata information is valid/invalid
	;; Arguments
	;;	aExtentInfo			structure containing metadata settings
	;; Updates structure in place if type correction possible.
	;; Call this routine often in lambdas that manage changes to cabinet meta data to catch problems 
	;; in user input and code. Note that this routine only checks existing elements -- it does not 
	;; create them for you. Use makeDefaultMetaData to generate a meta data structure with all 
	;; required elements.
	(defun validateMetaData(aExtentInfo)
		(if (not (isStructure aExtentInfo)) (begin
			(writeln "browseLib.addExtent --- no structure passed")
			(return false)))

		;;storageScope Check
		(if (isMember storageScope: aExtentInfo) (begin
			(if (not (isSymbol aExtentInfo.storageScope))
				(setq aExtentInfo.storageScope (symbol (downcase (string aExtentInfo.storageScope)))))
			(setq aExtentInfo.storageScope (symbol (downcase (string aExtentInfo.storageScope))))
			(if (not (isMember aExtentInfo.storageScope #(file: folder:))) (begin
				(writeln "browseLib.addExtent -- bad storageScope loaded. " aExtentInfo.storageScope " found")
				(return false)))
			))
	
		;;importSync Check
		(if (isMember importSync: aExtentInfo) (begin
			(if (not (isSymbol aExtentInfo.importSync)) 
				(setq aExtentInfo.importSync (symbol (downcase aExtentInfo.importSync))))
			(setq aExtentInfo.importSync (symbol (downcase (string aExtentInfo.importSync))))
			(if (not (isMember aExtentInfo.importSync #(none: ask: auto: notify:))) (begin
				(writeln "browseLib.addExtent -- bad importSync loaded. " aExtentInfo.importSync " found")
				(return false)))
			))
	
		;;exportSync Check
		(if (isMember exportSync: aExtentInfo) (begin
			(if (not (isSymbol aExtentInfo.exportSync)) 
				(setq aExtentInfo.exportSync (symbol (downcase aExtentInfo.exportSync))))
			(setq aExtentInfo.exportSync (symbol (downcase (string aExtentInfo.exportSync))))
			(if (not (isMember aExtentInfo.exportSync #(none: ask: auto: notify:))) (begin
				(writeln "browseLib.addExtent -- bad exportSync loaded. " aExtentInfo.exportSync " found")
				(return false)))
			))
	
		;;autoCompile Check
		(if (isMember autoCompile: aExtentInfo) (begin
			(if (not (isBoolean aExtentInfo.autoCompile))
				(begin 
				(if (not (isString aExtentInfo.autoCompile))
					(setq aExtentInfo.autoCompile (string aExtentInfo.autoCompile)))
				(setq aExtentInfo.autoCompile (downcase aExtentInfo.autoCompile))
				(cond 
					((= aExtentInfo.autoCompile "true") (setq aExtentInfo.autoCompile true))
					((= aExtentInfo.autoCompile "false")(setq aExtentInfo.autoCompile false))
					(else
					(writeln "browseLib.addExtent -- bad autoCompile loaded. " aExtentInfo.autoCompile " found")
					(return false))
				)
				))
			))
	
		;;searchSW Check
		(if (isMember searchSW: aExtentInfo) (begin
			(if (not (isBoolean aExtentInfo.searchSW))
				(begin
				(if (not (isString aExtentInfo.searchSW))
					(setq aExtentInfo.searchSW (string aExtentInfo.searchSW)))
				(setq aExtentInfo.searchSW (downcase aExtentInfo.searchSW))
				(cond 
					((= aExtentInfo.searchSW "true") (setq aExtentInfo.searchSW true))
					((= aExtentInfo.searchSW "false")(setq aExtentInfo.searchSW false))
					(else
					(writeln "browseLib.addExtent -- bad searchSW loaded. " aExtentInfo.searchSW " found")
					(return false))
				)
				))
			))
	true)

	;; getMetaDataFromFile
	;; Read the metadata from source file or directory, return false if specified file is not found
	;; Arguments
	;;	iLocation			path and or path and file
	;;	iScope				file: or folder:
	;; getMetaDataFromFile will look for a _METADATA file if the iScope is folder:.
	;; Pass only a path if with an iScope == folder:
	;; Pass a a full path with filename if an iScope == file: is provided.
	;; This routine should be called anytime you want to read meta data from a text source. 
	;; Do NOT write one off versions of this routine. Modify this one to
	;; do all necessary processing for reading meta data.
	(defun getMetaDataFromFile(iLocation iScope)
		vars:(	aExtentInfo aExtentFileName aFileId aFileInfo aSourceFile aNextLine 
				aExtentImportSync aExtentExportSync aExtentAutoCompile aExtentSearchSW aExtentDependencies
				aExtentLocation aExtentStorageScope aSourceFile aLineCount aExtentInfoDefaults
				(aFoundSettings false))

		(setq aExtentInfo (new Structure:))
		(if (= iScope folder:)
			(setq aExtentFileName (append iLocation "/_METADATA"))
			(setq aExtentFileName iLocation)
		)
		(setq aFileInfo (^new browseLib.fileInfo aExtentFileName))
		(if (= false (aFileInfo.exists))
			(return false))
		(setq aLineCount 0)
		(setq aFileId  (fileOpen aExtentFileName 0 0))
		(setq aSourceFile (fileReadRecord aFileId))
		(setq aNextLine (fileReadRecord aFileId aSourceFile))
		;;Note: use line count to avoid reading long files that have no meta data
		;;Meta Data is always at the top of the file.
		(while (and (< aLineCount 50) (<> aNextLine #void) (<> aNextLine ";;END __DATA")) do
			(setq aNextLine (trim aNextLine))
			(cond
				((= (find ";;location=" aNextLine) 0) 
					(setq aFoundSettings true)
					(setq aExtentLocation (substring aNextLine (length ";;location=")))
				)
				((= (find ";;storageScope=" aNextLine) 0)
					(setq aFoundSettings true)
					(setq aExtentStorageScope (trim (substring aNextLine (length ";;storageScope="))))
				)
				((= (find ";;importSync=" aNextLine) 0)
					(setq aFoundSettings true)
					(setq aExtentImportSync (trim (substring aNextLine (length ";;importSync="))))
				)
				((= (find ";;exportSync=" aNextLine) 0)
					(setq aFoundSettings true)
					(setq aExtentExportSync (trim (substring aNextLine (length ";;exportSync="))))
				)
				((= (find ";;autoCompile=" aNextLine) 0)
					(setq aFoundSettings true)
					(setq aExtentAutoCompile (trim (substring aNextLine (length ";;autoCompile="))))
				)
				((= (find ";;searchSW=" aNextLine) 0)
					(setq aFoundSettings true)
					(setq aExtentSearchSW (trim (substring aNextLine (length ";;searchSW="))))
				)
				((and (= (find ";;dependencies=" aNextLine) 0) (<> aNextLine ";;dependencies="))
					(setq aFoundSettings true)
					(setq aExtentDependencies (trim (substring aNextLine (length ";;dependencies="))))
				)
			)
			(++ aLineCount)
			(setq aNextLine (fileReadRecord aFileId aSourceFile))
		)
		(fileClose aFileId 1)

		(if (not aFoundSettings) (return false))

		;;Provide defaults on missing settings!
		;;It is up to the calling routine to check if the defaults are ok in
		;;the context in which the getMetaDataFromFile was called.
		;;Allowing defaults makes it possible to load up older versions of settings as new
		;;settings are added to set of allowed meta data.
		(setq aExtentInfoDefaults (makeDefaultMetaData))
		(setq aExtentInfo.location iLocation) ;; Override the location
		(setq aExtentInfo.storageScope iScope) ;; Override the scope
		(setq aExtentInfo.importSync (if (= aExtentImportSync #void) aExtentInfoDefaults.importSync aExtentImportSync))
		(setq aExtentInfo.exportSync (if (= aExtentExportSync #void) aExtentInfoDefaults.exportSync aExtentExportSync))
		(setq aExtentInfo.autoCompile (if (= aExtentAutoCompile #void) aExtentInfoDefaults.autoCompile aExtentAutoCompile))
		(setq aExtentInfo.searchSW (if (= aExtentSearchSW #void) aExtentInfoDefaults.searchSW aExtentSearchSW))
		(setq aExtentInfo.dependencies (if (= aExtentDependencies #void) aExtentInfoDefaults.dependencies aExtentDependencies))

	aExtentInfo)

	;; Add filename to list of files being watched by QFileSystemWatcher
	(defun watchFile(iFilename iCabName)
		vars:(aCabList)
		(if (= watchedFileList #void)
			(setq watchedFileList (new Dictionary:))
		)
		(if (= watchedFileList[iFilename] #void)
			(begin
			(setq aCabList (new Vector:))
			(setq aCabList[(length aCabList)] iCabName)
			(setq watchedFileList[iFilename] (new Structure: changedFlag: false cabinetList: aCabList))
			(addFileWatcher iFilename)
			)
		else
			(begin
			(setq aCabList watchedFileList[iFilename].cabinetList)
			(if (= false (member iCabName aCabList))
				(setq watchedFileList[iFilename].cabinetList[(length watchedFileList[iFilename].cabinetList)] iCabName)
			)
			(setq watchedFileList[iFilename].changedFlag false)
			)
		)
	)

	;; Flag the file being watched.  This function is called by AFileWatcherClass
	(defun setWatchedFileFlag(iFilename iFlag)
		vars:(aFlag aCabList aCabCount aCtr)
		(setq aFlag watchedFileList[iFilename].changedFlag)
		(if (<> aFlag iFlag)
			(begin
			(setq watchedFileList[iFilename].changedFlag iFlag)
			(setq aCabList watchedFileList[iFilename].cabinetList)
			(setq aCabCount (length aCabList))
			(loop for aCtr from 0 until aCabCount do
				;; Convert the symbol to a string, still need to fix glue/asbglue.cpp
				;; sendFileChangedEvent function to accept a symbol ;; RCA
				(sendFileChangedEvent iFilename (string aCabList[aCtr]))
			)
			)
		)
	true)

	;; Display the the files being watched by browse Lambda
	(defun displayWatchList()
		(writeln watchedFileList)
	)

	;; Display meta data information for current extent
	(defun displayData()
		vars:(aExtentInfo)
		(setq aExtentInfo (loadMetaData _ais.browseLibExtents.Focus.myRepository))
		(writeln aExtentInfo)
	)

	;; Imports a source file of multiple Lambdas into the file
	;;  cabinet. The source file must be in the export format
	;;  output from the export function.
	(defun importSourceByDirectory(name)
		vars:(n Lambda LambdaName fileSize (aType 0))
		vars:(self aBuffer aFileID aExtentInfo (aSetLocation "false") oldName record aTmpName aNextLine)
		(setq oldName name)
		(setq aExtentInfo (loadMetaData _ais.browseLibExtents.Focus.myRepository))
		;; Open the import source file containing all 
		;; the Lambda scripts for the Lambda file cabinet.
		(setq aFileID (fileOpen name 0 aType))
		(setq aBuffer (fileReadRecord aFileID))
		;; Use the name of the file as the import name
		(setq aTmpName (substring name (length mySourceDirectory)))
		(setq aTmpName (substring aTmpName 0 (- (length aTmpName) 4)))
		(setq LambdaName (substitute aTmpName "." ":"))
		;; process buffered source file
		(setq record (new Vector: byte: 32000000))
		(setq aNextLine (fileReadRecord aFileID aBuffer))
		(while (<> aNextLine #void) do
			(appendWriteln record aNextLine _eol)
			(setq aNextLine (fileReadRecord aFileID aBuffer))
		)

		(_ais.browseLibExtents.Focus.beginTransaction)
		;; Import the script for an Lambda
		(writeln "Importing source for " LambdaName)
		(_ais.browseLibExtents.Focus.setSourceExByKey LambdaName (string record))
		;(writeln "Completed importing Lambdas.")
		(_ais.browseLibExtents.Focus.commitTransaction) 
		(fileClose aFileID 1)
		;; RAJAH Is this causing the problem?
		;(if (= aExtentInfo.autoCompile "true")
			;(begin
			;(compileAll true)
			;)
		;)
		true)

	;; Import all valid source files in a directory
	;; If there is a 2nd argument and its value is "true" or true information is written to the meta data repository
	(defun importDirectory(iNewDirectory ...)
		vars:(n N aBuffer aDir aExtentName aFileID aFileName aKey aTmpKey aExtentInfo aFilter aFileList (aSetLocation "false"))
	(setq mySourceDirectory iNewDirectory)
	(setq aFilter #(RWEMask Files))

	(setq aExtentInfo (loadMetaData _ais.browseLibExtents.Focus.myRepository))
	(if (= (argCount) 2) (begin
		(setq aSetLocation (argFetch 1))
		(if (or (= aSetLocation "true") (= aSetLocation true)) true false)
	))

	(if (= mySourceDirectory "..Lambda location..")
		(begin
		(setq mySourceDirectory aExtentInfo.location)
		(setq aSetLocation true)
		)
	)

	(setq aDir (^new browseLib.dir mySourceDirectory))
	(aDir.setFilter aFilter)
	(setq aFileList (aDir.entryList "*.sl"))
	(setq N (length aFileList))

	(if (<> (string mySourceDirectory[(- (length mySourceDirectory) 1)]) "/")
		(setq mySourceDirectory (append mySourceDirectory "/"))
	)

	(loop for n from 0 until N do
		(importSourceByDirectory (append mySourceDirectory aFileList[n]))
	)

	(if (or (= aSetLocation true) (= aSetLocation "true"))
		(begin
		(setq aExtentInfo.location mySourceDirectory)
		(setq aExtentInfo.lastImport (fileTimeStamp aExtentInfo.location)) ;;TM? Is this used anymore?
		(setq aExtentInfo.storageScope folder:)
		(setq aExtentInfo.fileHash (new Dictionary:)) ;;Note that fileHash is Dictionary when the metadata is for directory!
		(setq aExtentInfo.cabinetHash (new Dictionary:)) ;;TM? Added.
		(setq N (_ais.browseLibExtents.Focus.refLength))
		(loop for n from 0 until N do 
			(setq aTmpKey (_ais.browseLibExtents.Focus.refKeyByIndex n))
			(setq aFileName (append aExtentInfo.location (substitute aTmpKey ":" ".") ".sl"))
			(setq aFileID (fileOpen aFileName 0 0))
			(setq aBuffer (fileReadRecord aFileID))
			(fileClose aFileID 1)
			(setq aExtentInfo.fileHash[aTmpKey] (md5hash (string aBuffer)))
			(setq aExtentInfo.cabinetHash[aTmpKey] (md5hash (string  (_ais.browseLibExtents.Focus.refSourceByKey aTmpKey))))
		)
		(saveMetaData _ais.browseLibExtents.Focus.myRepository aExtentInfo)
		(watchFile aExtentInfo.location (getFocus))
		)
	) ;; end if
	(setq extentStatusList[(getFocus)] (checkStatus (getFocus)))
	true)

	;; export source with folder as storage scope
	;; Currently LambdaName is not used since we are assuming we want to export all nodes in the cabinet
	(defun exportDirectory(iExportDir LambdaName ...)
	vars:(n N m M aBuffer aDir aFileID aFileName aEndIndex aTempName aStartIndex aKey
		aExtentInfo aLambdaSource aIndex aSourceDirectory (aSetLocation false) aTmpKey aStorageScope
		)
	(setq aSourceDirectory iExportDir)

	(setq aExtentInfo (loadMetaData _ais.browseLibExtents.Focus.myRepository))
	(validateMetaData aExtentInfo)
	(setq aStorageScope aExtentInfo.storageScope)

	(if (>= (argCount) 3) (begin
		(setq aSetLocation (argFetch 2))
		(if (or (= aSetLocation "true") (= aSetLocation true)) true false)
	))

	(if (= aSourceDirectory "..Lambda location..")
		(begin
		(setq aSourceDirectory aExtentInfo.location)
		(setq aSetLocation true) 
		)
	)

	(setq aStartIndex 0)
	(setq aEndIndex (_ais.browseLibExtents.Focus.refLength))
	(if (= aEndIndex false)
		(setq aEndIndex 0)
	)

	(if (<> (string aSourceDirectory[(- (length aSourceDirectory) 1)]) "/")
		(setq aSourceDirectory (append aSourceDirectory "/"))
	)

	(setq aDir (new browseLib.dir.current))
	(if (= (aDir.exists aSourceDirectory) false)
		(aDir.mkdir aSourceDirectory)
	)

	;; Write the export source file header records.
	(setq aFileName (append aSourceDirectory "_METADATA"))
	(setq aFileID (fileOpen aFileName 1 0))
	(fwriteln aFileID ";;*************************************")
	(fwriteln aFileID ";;*************************************")
	(fwriteln aFileID ";; Exported Lambda File Cabinet Document")
	(fwriteln aFileID ";;*************************************")
	(fwriteln aFileID ";;*************************************")
	(setq N (length aExtentInfo))
	(fwriteln aFileID ";;version="(isVersion))
	(loop for n from 0 until N do
		(if (and (<> fileHash: aExtentInfo[n 0]) (<> cabinetHash: aExtentInfo[n 0]))
			(begin
			;; Change the storage scope/location values when exporting
			(cond 
				((= aExtentInfo[n 0] location:)
					(fwriteln aFileID ";;"aExtentInfo[n 0]"="aSourceDirectory)
				)
				((= aExtentInfo[n 0] storageScope:)
					(fwriteln aFileID ";;"aExtentInfo[n 0]"=folder")
				)
				(else
					;; Write the data as is
					(fwriteln aFileID ";;"aExtentInfo[n 0]"="aExtentInfo[n 1])
				)
			)
			)
		)
	)
	(fwriteln aFileID ";;END __DATA" _eol)
	(fileClose aFileID 1)

	(loop for n from aStartIndex until aEndIndex do
		(setq aTempName (_ais.browseLibExtents.Focus.refKeyByIndex n))
		;; Create the Lambda's name and convert the colons to .
		(setq aFileName (append aSourceDirectory (substitute aTempName ":" ".") ".sl"))
		(setq aFileID (fileOpen aFileName 1 0))

		(setq aLambdaSource (_ais.browseLibExtents.Focus.refSourceExByIndex n))
		(fwriteln aFileID aLambdaSource)
		(fileClose aFileID 1)
	)
	;; Check if we need to save new cabinet information
	(if (or (= aSetLocation true) (= aSetLocation "true"))
		(begin
		(setq aExtentInfo.location aSourceDirectory)
		(setq aExtentInfo.lastImport (fileTimeStamp aExtentInfo.location))
		(setq aExtentInfo.storageScope folder:)
		(setq aExtentInfo.fileHash (new Dictionary:)) 
		(setq N (_ais.browseLibExtents.Focus.refLength))
		(loop for n from 0 until N do
			(setq aTmpKey (_ais.browseLibExtents.Focus.refKeyByIndex n))
			(setq aFileName (append aExtentInfo.location (substitute aTmpKey ":" ".") ".sl"))
			(setq aFileID (fileOpen aFileName 0 0))
			(setq aBuffer (fileReadRecord aFileID))
			(fileClose aFileID 1)
			(setq aExtentInfo.fileHash[aTmpKey] (md5hash (string aBuffer))) 
			(setq aExtentInfo.cabinetHash[aTmpKey] (md5hash (string  (_ais.browseLibExtents.Focus.refSourceByKey aTmpKey)))) 
		)

		(saveMetaData _ais.browseLibExtents.Focus.myRepository aExtentInfo)
		(watchFile aExtentInfo.location (getFocus))
		)
	) ;; end if

	(setq extentStatusList[(getFocus)] (checkStatus (getFocus)))
	true)  ;; end exportDirectory
	
	;; Check import/export synchronization status for a cabinet key
	;; The result should be <action> #\tab <message box display> #\tab <importSync> #\tab <exportSync>
	;; <iPath> - directory to import cabinets from
	;; <source directory> - optional argument, separate location for .sl files
	(defun registerDirectory(iPath ...)
		vars:(n N m M _SavePath fileId nextLine sourceFile aExtentInfo)
		vars:(aDatabase aDir aDirList aDBList aExtentInfo aSLList (aFilter (new Vector:)) aDatabaseDirectory aSourceDirectory)
		vars:(aKey aExtentFilename aExtentLocation aExtentStorageScope aImportSync aExportSync aAutoCompile aSearchSW aSearchSW aForceSetting)
		(setq aDatabaseDirectory iPath)
		(if (>= (argCount) 2)
			(setq aSourceDirectory (argFetch 1))
			(setq aSourceDirectory iPath)
		)

		(if (<> (right (string aDatabaseDirectory) 1) "/")
			(setq aDatabaseDirectory (append aDatabaseDirectory "/")))

		(if (<> (right (string aSourceDirectory) 1) "/")
			(setq aSourceDirectory (append aSourceDirectory "/")))

		;; Process all .db files
		(setq aFilter #(RWEMask Files))
		(setq aDir (^new browseLib.dir aDatabaseDirectory))
		(aDir.setFilter aFilter)
		(setq aDBList (aDir.entryList "*.db"))

		(setq N (length aDBList))
		(loop for n from 0 until N do
			(setq aExtentFilename (append aDatabaseDirectory aDBList[n]))
			(setq aDatabase (new ObjectRepository: aExtentFilename))
			(setq aExtentInfo (loadMetaData aDatabase))
			(if (isStructure aExtentInfo) (begin
				(if (= (validateMetaData aExtentInfo) false) (begin
					(writeln "browseLib.registerDirectory skiiping " aExtentFilename " because validateMetaData failed")
					)
				else (begin
					(setq aKey (left aDBList[n] (find "." aDBList[n])))
					;;(setq aExtentLocation (append aSourceDirectory aKey ".sl"))
					;;TM? Why are we forcing the parameters here? Does this result in a force export to 
					;;move the meta data into the source file(s)?
					(if (= _ais.browseLibExtents.Extents[aKey] #void)
						(addExtentForce aKey aExtentInfo))
					))
			))
		)

		;; Process Source Files
		(setq aFilter #(RWEMask Files))
		(setq aDir (^new browseLib.dir aSourceDirectory))
		(aDir.setFilter aFilter)
		(setq aSLList (aDir.entryList "*.sl"))
		(setq N (length aSLList))
		(setq aKey #void)
		(loop for n from 0 until N do
			(setq aExtentFilename (append aSourceDirectory aSLList[n]))
			(if (= (setq aExtentInfo (getMetaDataFromFile aExtentFilename file:)) false) (begin
				(writeln "browseLib.registerDirectory skipping file:" aExtentFilename " because getMetaDataFromFile failed")
				)
			else (begin
				(setq aKey (substring aSLList[n] 0 (- (find ".sl" aSLList[n]) 1)))
				(setq aExtentInfo.objRepoName (append aDatabaseDirectory aKey ".db"))
				;;TM? Is it correct to call addExtentForce?
				(if (= _ais.browseLibExtents.Extents[aKey] #void) 
					(addExtentForce aKey aExtentInfo)) 
			))
		)

		;;TM? Should we think about using recursion to eat a folder hierarchy?
		;; Process Directories
		(setq aFilter #(RWEMask Files))
		(setq aDir (^new browseLib.dir aSourceDirectory))
		(aDir.setFilter aFilter)
		(setq aDirList (aDir.entryList "*" #(Dirs:)))
		(setq N (length aDirList))
		(setq aExtentInfo (new Structure:))
		(loop for n from 0 until N do
			(setq aKey aDirList[n])
			;; Skip unnecessary directories listed
			(if (or (= ".." aKey) (= "." aKey))
				(goto SKIP:)
			)
			(setq aExtentFilename (append aSourceDirectory aKey "/_METADATA"))
			(if (= (getMetaDataFromFile aSourceDirectory folder:) false)
				(goto SKIP:) ;; Skip the directory if no metadata file is found
			)
			(setq aExtentInfo.objRepoName (append aDatabaseDirectory aKey ".db"))
			;;(setq aExtentInfo.location (append aSourceDirectory aKey)) ;;TM? This doesn't look right? Why is aKey appended for a storageScope of folder: ?
			(setq aExtentInfo.storageScope folder:)
			;;TM? Should we be calling addExtentForce?
			(if (= _ais.browseLibExtents.Extents[aKey] #void)
				(addExtent aKey aExtentInfo)) ;;Note use of new argument signature
			SKIP::
		)
	true)

	;; Check import/export synchronization status for a cabinet key
	;; The result should be <action> #\tab <message box display> #\tab <importSync> #\tab <exportSync>
	;; <action>		nothing - means source and db is up-to-date.
	;;				import - source needs to be imported.
	;;				export - db needs to be exported to the source location.
	;;				unknown - Unknown status.
	;; <message box display> - Information to be displayed in webide regarding the status of a cabinet
	(defun checkStatus(key)
		vars: (n N aDir aExtentInfo aFile aItemCount aLastMod aLocation aSLList aTmpKey (result "") (searchedItemCount 0) tempName)
		vars: ((aNeedExport false) (aNeedImport false))
		vars: (nextLine aFileID aBuffer aTmpDir aTmpHash (type 0))
		(setq aExtentInfo (loadMetaData _ais.browseLibExtents.Extents[key].myRepository))

		;; If cabinet is not copied from _extentManagerTemplate, return void values for the resulting string
		(if (= false (isMember _extentManagerTemplate: _ais.browseLibExtents.Extents[key].Pv))
			(begin
			(setq result (append "#void" #\tab "#void" #\tab "#void" #\tab "#void" #\tab key #\tab "#void"))
			(return result))
		)

		;; If cabinet's import and export synchronization are both "none", return #void values.
		;; This cabinet shouldn't have any status displayed on the cabinet page.
		(if (and (= aExtentInfo.importSync none:) (= aExtentInfo.exportSync none:))
			(begin
			(setq result (append "#void" #\tab "#void" #\tab "#void" #\tab "#void" #\tab key #\tab "#void"))
			(return result))
		)

		(if (= aExtentInfo #void)
			(begin
			(setq result (append "unknown" #\tab "__DATA in cabinet "key" is #void." #\tab aExtentInfo.importSync #\tab aExtentInfo.exportSync #\tab key #\tab aExtentInfo.storageScope))
			)
		else
		(begin
			(if (= aExtentInfo.storageScope file:)
				(begin
					(setq aItemCount (_ais.browseLibExtents.Extents[key].refLength))
					(if (> aItemCount 0)
						(begin
						(setq aFile (^new browseLib.fileInfo aExtentInfo.location))
						(if (= true (aFile.exists))
							(begin
							(setq aFileID (fileOpen aExtentInfo.location 0 type))
							(setq aBuffer (fileReadRecord aFileID))
							(setq aTmpHash (md5hash (string aBuffer)))
							(fileClose aFileID 1)
							(if (or (= aExtentInfo.fileHash #void) (<> aTmpHash aExtentInfo.fileHash))
								(setq aNeedImport true))

							;; Read until we encounter the start of the first exported Lambda header.
							(loop for n from 0 until aItemCount do
								(setq aTmpKey (_ais.browseLibExtents.Extents[key].refKeyByIndex n))
								(setq aTmpSource (_ais.browseLibExtents.Extents[key].refSourceByKey aTmpKey))
								(setq aTmpHash aExtentInfo.cabinetHash[aTmpKey])
								(if (<> aTmpHash (md5hash (string aTmpSource)))
									(setq aNeedExport true))
							) ; end loop
							)
							else
								;; Source file does not exist
								(setq aNeedExport true))
						)
						else
						(begin
							;; There are no Lambdas yet in the cabinet
							(setq aNeedImport true)))
					(if (not (isMember aExtentInfo.importSync #(ask: auto: notify:)))
						(setq aNeedImport false))
					(if (not (isMember aExtentInfo.exportSync #(ask: auto: notify:)))
						(setq aNeedExport false))
					(cond
						((and (= aNeedImport true) (= aNeedExport true))
							(setq result (append "merge" #\tab "Needs merge." #\tab aExtentInfo.importSync #\tab aExtentInfo.exportSync #\tab key #\tab aExtentInfo.storageScope)))
						((= aNeedImport true)
							(setq result (append "import" #\tab "Need import." #\tab aExtentInfo.importSync #\tab aExtentInfo.exportSync #\tab key #\tab aExtentInfo.storageScope)))
						((= aNeedExport true)
							(setq result (append "export" #\tab "Need export." #\tab aExtentInfo.importSync #\tab aExtentInfo.exportSync #\tab key #\tab aExtentInfo.storageScope)))
					)
				)
			)
			(if (= aExtentInfo.storageScope "folder")
				(begin
					;(cond 
					;((= aExtentInfo.lastImport #void)
					;(setq result (append "import" #\tab "No source file imported: "key" needs to import source from "aExtentInfo.location"." #\tab aExtentInfo.importSync #\tab aExtentInfo.exportSync #\tab key #\tab aExtentInfo.storageScope)))
					;(else
						(setq aTmpDir aExtentInfo.location)
						(if (<> (string aTmpDir[(- (length aTmpDir) 1)]) "/")
							(setq aTmpDir (append aTmpDir "/"))
						)
						(setq aDir (^new browseLib.dir aExtentInfo.location))
						(setq aSLList (aDir.entryList "*.sl"))
						(setq endIndex (_ais.browseLibExtents.Extents[key].refLength))
						(loop for n from 0 until endIndex do
							(setq aTmpKey (_ais.browseLibExtents.Extents[key].refKeyByIndex n))
							(setq tempName (append (substitute aTmpKey ":" ".") ".sl"))
							(if (= false (isMember tempName aSLList))
								(begin
								(setq aNeedExport true)
								(goto Skip:))
								else
								(begin
								;; Open file and check against previously generated md5hash value
								(setq aFileID (fileOpen (append aTmpDir tempName) 0 type))
								(setq aBuffer (fileReadRecord aFileID))
								(setq aTmpHash (md5hash (string aBuffer)))
								(fileClose aFileID 1)
								(if (or (= aExtentInfo.fileHash[aTmpKey] #void) (<> aTmpHash aExtentInfo.fileHash[aTmpKey]))
									(setq aNeedImport true))

								(setq aTmpSource (_ais.browseLibExtents.Extents[key].refSourceByKey aTmpKey))
								(setq aTmpHash aExtentInfo.cabinetHash[aTmpKey])
								(if (<> aTmpHash (md5hash (string aTmpSource)))
									(setq aNeedExport true))
								)
							)
							(++ searchedItemCount)
							Skip::
						)
						(if (<> searchedItemCount (length aSLList))
							(setq aNeedImport true))

						;; Check if the cabinet wants to synchronize import/export
						(if (not (isMember aExtentInfo.importSync #(ask: auto: notify:)))
							(setq aNeedImport false))
						(if (not (isMember aExtentInfo.exportSync #(ask: auto: notify:)))
							(setq aNeedExport false))
						(cond
							((and (= aNeedImport true) (= aNeedExport true))
								(setq result (append "merge" #\tab "Needs merge." #\tab aExtentInfo.importSync #\tab aExtentInfo.exportSync #\tab key #\tab aExtentInfo.storageScope)))
							((= aNeedImport true)
								(setq result (append "import" #\tab "Need import." #\tab aExtentInfo.importSync #\tab aExtentInfo.exportSync #\tab key #\tab aExtentInfo.storageScope)))
							((= aNeedExport true)
								(setq result (append "export" #\tab "Need export." #\tab aExtentInfo.importSync #\tab aExtentInfo.exportSync #\tab key #\tab aExtentInfo.storageScope)))
						)
					;))
				)
			)
		)
		) ;; end if
		(if (= result "")
			(setq result (append "nothing" #\tab "Cabinet "key" is up-to-date." #\tab aExtentInfo.importSync #\tab aExtentInfo.exportSync #\tab key #\tab aExtentInfo.storageScope)))
		;; The next lines would store the resulting status string to browseLib's extentStatusList
		;; This is used by the GUI to display the current status for each cabinet
		(if (<> extentStatusList[key] #void)
			(setq extentStatusList[key] result))

	result)
	
	;; Checks if a file exists
	(defun fileExists (path)
		vars: (f)
		(setq f (^new browseLib.fileInfo path))
		(f.exists) ;; Return true if file exists
	)

	;; Checks if a file exists
	(defun fileTimeStamp (path)
		vars: (f)
		(setq f (^new browseLib.fileInfo path))
		(f.lastModified) ;; Return true if file exists
	)

	;; Show list of all Lambda cabinets
	(defun showLambdaCabinets()
		vars:(n N)
		(setq N (length _ais.browseLibExtents.LambdaCabinetList))
		(writeln "================= Lambda Cabinet List =================" )
		(loop for n from 0 until N do
			(writeln "["n"] "_ais.browseLibExtents.LambdaCabinetList[n])
		)
		(writeln "======================================================" )
	)
	;; Show list of all cabinets
	(defun showAllCabinets()
		vars:(n N)
		(setq N (length _ais.browseLibExtents.Extents))
		(writeln "==================== Cabinet List ====================" )
		(loop for n from 0 until N do
			(writeln "["n"] "_ais.browseLibExtents.Extents[n 0])
		)
		(writeln "======================================================" )
	)

	;; *******************************************************************
	;; Define Private Child Lambdas
	;; *******************************************************************
	;;  scripts in the Lambda file cabinet to a source file.
	(defun _exportSource(fileID command)
		vars:(name LambdaName LambdaSource s i n
				startIndex endIndex
				parentName parentLen
				childName childLen
				file brk focus keys
				aTempName
				) ;; end of temporary variables
		;; Check for a request to export "all" Lambdas.
		(if (= command "") (setq command "..all Lambdas.."))
		(if (= command "..all Lambdas..") (setq LambdaName "") (setq LambdaName (string command)))        
		;; Compute the parent Lambda name and length.
		;; If the name contains an imbedded colon (:) symbol,
		;; then we keep only the parent Lambda part of the name
		;; preceeding the colon (:) symbol.
		(setq n (find ":" LambdaName))
		(if (isNumber n)
			(setq parentName (left LambdaName n))
			(setq parentName LambdaName))
		(setq parentLen (length parentName))
		;; Compute the parent Lambda name and length.
		(if (= parentName "")
			(setq childName "")         
			(setq childName (append "" parentName ":")))
		(setq childLen (length childName))
		;; Position the start and end index at the Lambda to be exported.
		(if (= command "..all Lambdas..")
			;; We are exporting all Lambdas. 
			(begin
				(setq startIndex 0)
				(setq endIndex (_ais.browseLibExtents.Focus.refLength))
				) ;; end of then
			;; We are exporting one parent and its child Lambdas. 
			(begin
				(setq endIndex (_ais.browseLibExtents.Focus.refLength))
				(loop for startIndex from 0 until endIndex do
					;; Quit if this Lambda is not to be exported.
					(setq name (_ais.browseLibExtents.Focus.refKeyByIndex startIndex))
					(if (or (= parentName name) (= childName (left name childLen))) (goto FoundStartIndex:))
					) ; end loop
				FoundStartIndex::
				) ;; end of else
			) ;; end of if
		;; Write each Lambda script to the export file.
		(_ais.browseLibExtents.Focus.beginTransaction) 
		(loop for i from startIndex until endIndex do
			;; Quit if this Lambda is not to be exported.
			(setq name (_ais.browseLibExtents.Focus.refKeyByIndex i))
			(if (and (<> command "..all Lambdas..")
						(<> parentName name)
						(<> childName (left name childLen)))
				(goto CloseFileCabinet:)
				) ;; end if
			;; Export this Lambda.
			(setq aTempName (substitute name ":" "."))
			(fwriteln fileID ";;**EXPORTKEY**:" aTempName)
			(setq LambdaSource (_ais.browseLibExtents.Focus.refSourceExByIndex i))
			(fwriteln fileID LambdaSource)
			Skip::
			) ; end loop
		CloseFileCabinet::
		(_ais.browseLibExtents.Focus.commitTransaction) 
		true) ;; end _exportSource
	;; Initialize the browseLib (if necessary)
	(defun _initialize()
		vars:(n N extMgr extentName)
		(if (= _ais.browseLibExtents #void)
			(begin
				(setq _ais.browseLibExtents (new Structure: Focus: #void FocusIndex: -1 Precompiler: precompiler Extents: (new Dictionary:) LambdaCabinetList: (new Vector: Object:) ))
				(addMemoryExtent)
				)) ; end if       
		(if (and (<> _ais.browseLibExtents.FocusIndex -1) (<> _ais.browseLibExtents.FocusIndex false)) 
			(setq _ais.browseLibExtents.Focus _ais.browseLibExtents.Extents[_ais.browseLibExtents.FocusIndex]))
		;; Initialize the extentStatusList if needed
		(if (<> (type extentStatusList) Dictionary:)
			(begin
			(setq extentStatusList (new Dictionary:))
			)
		)
		true) ; end of _initialize
	;; Recover from any system errors which occur.
	(defun _sysErrorStop(errCode) (append dString " !sysErr#" errCode))
	;; *******************************************************************
	;; Define Public Child Lambdas
	;; *******************************************************************
	;; Close, with abort, all extents in File Cabinet
	(defun abortAll()
		vars:(n N)
		(setq N (length _ais.browseLibExtents.Extents))
		(loop for n from 0 until N do
			(_ais.browseLibExtents.Extents[n 1].abortTransaction)
			) ; end loop
		true) ;; end of abortAll
	;; Register a data file cabinet extent to the browse Lambda
	(defun addDataExtent(extentName extentFileName)
		vars:(objRepo extMgr)
		(_initialize)
		(setq extentName (string extentName))
		(if (not (isLambda extentFileName))
			(begin
				(setq objRepo (new ObjectRepository: extentFileName))
				(setq extMgr (copy _dataManagerTemplate))
				(setq extMgr.myRepository objRepo)
				(setq extMgr.myExtentName extentName)
				) ; end then
			else
			(setq extMgr extentFileName)
			) ; end if
		(setq _ais.browseLibExtents.Extents[extentName] extMgr)
		(setq _ais.browseLibExtents.Focus extMgr)
		(setq _ais.browseLibExtents.FocusIndex (member extentName _ais.browseLibExtents.Extents))
		true) ; end of addDataExtent

	;; Call addExtent with the browseLib.myForceSettingsFlag set to True.
	;; See 
	(defun addExtentForce(...)
		regs:(a A)
		vars:(aArgs aResult aSavedForceSettingsFlag)
		(setq A (argCount))
		(setq aArgs (new Vector: A))
		(loop for a from 0 until A do
			(setq aArgs[a] (argFetch a))
		)
		(setq aSavedForceSettingsFlag myForceSettingsFlag)
		(setq myForceSettingsFlag true)
		(setq aResult (apply addExtent aArgs))
		(setq myForceSettingsFlag aSavedForceSettingsFlag)
		aResult)

	;;addExtent -- Register an Lambda file cabinet extent to the browse Lambda
	;; Arguments form 1
	;;			location				directory. -- Load everything found in directory.
	;; Arguments form 2
	;; 		 	extentName   			The name of the extent to be registered or Lambda cabinet manager.
	;;       	extentFileName   	    The path and file name for the repository file for this extent (or may be an Lambda object).
	;;        	extentLocation  		(Optional) The path and file name for the source file for this extent (.sl or other).
	;;        	extentStorageScope    	(Optional) Either file: for a single source file or folder: for a folder of source files.
	;;        	importSync    			(Optional) Import synchronization either: none:, ask:, or auto:.
	;;        	exportSync 		    	(Optional) Export synchronization either: none:, ask:, or auto:.
	;;        	autoCompileSW   		(Optional) TRUE iff this extent is to be automatically compiled.
	;;        	searchSW   		    	(Optional) TRUE iff this extent is to be automatically searched.
	;;        	dependName(s)      		(Optional) None or more cabinet names upon whom this new cabinet dependends.
	;; Arguments form 3
	;;			extentName				Name of the extent to be registered
	;;			metaDataStructure		A structure containing meta data information 
	;;
	(defun addExtent(...)
		regs:(n N a A)
		vars:(	aDir 
				aObjRepo 
				aMgr 
				aExtMgr
				aExtentInfo 
				(aNeedImport false) 
				aDependName 
				aExtentFileName 
				aExtentName 
				aExtentLocation 
				aExtentName 
				aExtentInfo
				(aExtentInfoCopy #void) 
				(aCurrentExtentInfo #void)
				(aExtentInfoUserSettings #void)
			)
		(_initialize)

		(setq A (argCount))

		;;Generate Default MetaData structure
		;;Handle Argument Form 3. ExtentName and MetaData Structure
		(if (and (= A 2) (isStructure (argFetch 1))) (begin
			(setq aExtentName (argFetch 0))
			(setq aExtentInfo (argFetch 1))
			)
		else (begin ;;Handle Argument Form 2 amd default aExtentInfo for Argument form 1. 
			(setq aExtentName (argFetch 0))
			(setq aExtentInfo (makeDefaultMetaData))
			))

		(if (= (validateMetaData aExtentInfo) false) ;;validateMetaData will write error messages
			(return false))

		;;Handle Argument Form 1 -- Directory passed
		(if (= A 1)
			(begin
			;; Check if the passed argument is a directory, if it is not, return false
			(setq aExtentLocation (argFetch 0))
			(if (not (isString aExtentLocation)) (begin
				(writeln "browseLib.addExtent -- Directory name passed must be a string")
				(return false)
				))
			(setq aDir (^new browseLib.fileInfo aExtentLocation))
			(if (= (aDir.isDir) false)
				(return false))
			(return (registerDirectory aExtentLocation))
			)
		)

		;;Fetch arguments up to but not including depends list and
		;;test each argument for validitiy where possible.
		(setq aExtentName (string (argFetch 0)))
		(setq aExtentFileName (argFetch 1))
		(if (isLambda aExtentFileName)
			(setq aExtentInfo.objRepoName (if (isMember objRepoName: aExtentFileName.Pv) aExtentFileName.objRepoName ""))
			(setq aExtentInfo.objRepoName aExtentFileName))
		(if (> A 1) (setq aExtentInfo.objRepoName (argFetch 1)))
		(if (> A 2) (setq aExtentLocation (argFetch 2)))
		(if (> A 2) (setq aExtentInfo.location (argFetch 2)))
		(if (> A 3) (setq aExtentInfo.storageScope (argFetch 3)))
		(if (> A 4) (setq aExtentInfo.importSync (argFetch 4)))
		(if (> A 5) (setq aExtentInfo.exportSync (argFetch 5)))
		(if (> A 6) (setq aExtentInfo.autoCompile (argFetch 6)))
		(if (> A 7) (setq aExtentInfo.searchSW (argFetch 7)))
		;; Construct depends list and store to aExtentInfo
		(setq aDependencies "")
		(if (> A 8) (begin
		(loop for n from 8 until (argCount) do
			(setq aDependencies (append aDependencies " " (argFetch n)))
		)
		(setq aExtentInfo.dependencies (trim aDependencies))
		))
		(if (= (validateMetaData aExtentInfo) false) ;;validateMetaData will write error messages
			(return false))

		;; Handle Argument form 2 where first argument is an extent manager lambda
		;; See the stockMonthlyFundWizard.repositoryManager.addExtent for an example of where this is called.
		(if (isLambda aExtentFileName)
			(begin
			(setq aExtMgr aExtentFileName)
			(setq aObjRepo aExtMgr.myRepository)
			(setq aExtMgr.myExtentName aExtentName)
			;; Try to load meta data from the Lambda's object repository
			(if (= (type aObjRepo) ObjectRepository:)
				(begin
				(setq aExtentInfoUserSettings (copy aExtentInfo))
				(setq aExtentInfo (loadMetaData aObjRepo))
				(if (or (<> (type aExtentInfo) Structure:)(not (validateMetaData aExtentInfo)))
					(begin
					(setq aExtentInfo aExtentInfoUserSettings)
					(if (= aExtentInfo.cabinetHash #void)
						(setq aExtentInfo.cabinetHash (new Dictionary:))
					)
					)
				)
				)
			)
			)
		else ;; Handle Argument form 2 where first argument is a cabinet name
			(begin
			;;Open/Create Repository and load existing settings into aExtentInfo structure
			(setq aObjRepo (new ObjectRepository: aExtentFileName))
			(if myForceSettingsFlag (begin
				(setq aExtentInfoCopy (loadMetaData aObjRepo))
				;; Copy the generated hashes in the object repository if it exists
				(if (isStructure aExtentInfoCopy) (begin
					(setq aExtentInfo.fileHash aExtentInfoCopy.fileHash)
					(setq aExtentInfo.cabinetHash aExtentInfoCopy.cabinetHash)
				))
				(setq aExtentInfoCopy #void) ;;Cause write of settings later after jump to SKIPLOAD
				(goto SKIPLOAD:)))
				
			(setq aExtentInfoUserSettings (copy aExtentInfo)) ;;Grab a copy of any user supplied parameters
			;;Try and load meta data settings from repository
			(setq aExtentInfo (loadMetaData aObjRepo)) ;;loadMetaData returns false if load failed
			(setq aExtentInfoCopy (copy aExtentInfo)) ;;Save the initial load state for later determination of change

			(if (not (isStructure aExtentInfo)) ;;No meta data found
				(begin
				(setq aNeedImport true) ;; Force reload if we could not load meta data from repository

				;;No ExtentInfo was in repository cabinet so we try and find existing meta data in file system
				(cond 
					((< A 3) ;;No storageScope argument was passed so try and determine storage scope by inspection of specified source
						;;Note: We try file first as the default storageScope is file:
						(setq aExtentInfo (getMetaDataFromFile aExtentLocation file:)) ;;Look for a file
						(if (not (isStructure aExtentInfo)) ;;Look for directory _METADATA file
							(setq aExtentInfo (getMetaDataFromFile aExtentLocation folder:)))
					)
					;; If storageScope argument is file: we try and load meta data from the .sl file
					((= aExtentInfoUserSettings.storageScope file:)
						(setq aExtentInfo (getMetaDataFromFile aExtentLocation file:))
						)
					;; If storageScope argument is folder: we try and load meta data from the _METADATA file
					((= aExtentInfoUserSettings.storageScope folder:)
						(setq aExtentInfo (getMetaDataFromFile aExtentLocation folder:))
					)
				)

				;; If we could find no meta data then use user supplied arguments and defaults where possible
				(if (not (isStructure aExtentInfo))
					(begin
					(setq aExtentInfo (copy aExtentInfoUserSettings))
;					(setq aExtentInfo.location aExtentLocation)
					))
	
				;;Make sure all required elements are in aExtentInfo -- this catches new elements not stored in older versions
				(if (not (isMember location: aExtentInfo)) 
					(setq aExtentInfo.location aExtentInfoUserSettings.location))

				(if (not (isMember storageScope: aExtentInfo)) 
					(setq aExtentInfo.storageScope aExtentInfoUserSettings.extentStorageScope))

				(if (not (isMember importSync: aExtentInfo))
					(setq aExtentInfo.importSync aExtentInfoUserSettings.importSync))

				(if (not (isMember exportSync: aExtentInfo))
					(setq aExtentInfo.exportSync aExtentInfoUserSettings.exportSync))

				(if (not (isMember autoCompile: aExtentInfo))
					(setq aExtentInfo.autoCompile aExtentInfoUserSettings.autoCompile))

				(if (not (isMember searchSW: aExtentInfo))
					(setq aExtentInfo.searchSW aExtentInfoUserSettings.searchSW))

				(if (not (isMember cabinetHash: aExtentInfo))
					(setq aExtentInfo.cabinetHash aExtentInfoUserSettings.cabinetHash))

				(if (not (isMember dependencies: aExtentInfo))
					(setq aExtentInfo.dependencies aExtentInfoUserSettings.dependencies))
				);end begin				
			);end if
			); end begin
		);end if
			
		SKIPLOAD:: ;;We jump here if we are forcing the use of user parameters
		(setq aExtentInfo.objRepoName aExtentFileName)

		;;Do final validation check
		(if (= (validateMetaData aExtentInfo) false) (begin ;;validation writes error messages
			(writeln "Could not add extent " aExtentName)
			(return false)
			))

		;; Save the metadata information to the object repository
		(if (<> aExtentInfo aExtentInfoCopy)
			(saveMetaData aObjRepo aExtentInfo))

		;; Add this file to the file watcher class
		(if (<> aExtentInfo.location "") ;;It is valid for a cabinet to have no sourcefile!
			(watchFile aExtentInfo.location aExtentName))

		;;Add selected information from the meta data into the extent manager
		(setq aExtMgr (copy _extentManagerTemplate))
		(setq aExtMgr.myRepository aObjRepo)
		(setq aExtMgr.myExtentName aExtentName)
		(setq aExtMgr.mySearchSW aExtentInfo.searchSW)
		(setq aExtMgr.myAutoCompile aExtentInfo.autoCompile)
		;;(setq aExtMgr.myCompileRequired false) ;;Assign default see Import Source loop below for conditional assignment on import
		(setq aExtMgr.myCompileRequired aExtMgr.myAutoCompile) ;;Removed assignment from Import Source, extent needs compilation even if the cabinet is up-to-date

		(setq _ais.browseLibExtents.Extents[aExtentName] aExtMgr)
		(setq _ais.browseLibExtents.Focus aExtMgr)
		(setq _ais.browseLibExtents.FocusIndex (member aExtentName _ais.browseLibExtents.Extents))
		;; Add this Lambda's key to LambdaCabinetList
		(setq _ais.browseLibExtents.LambdaCabinetList[(length _ais.browseLibExtents.LambdaCabinetList)] aExtentName)

		;; Check if source needs to be synced if cabinet's importSync is set to true
		(if (and (or (= aExtentInfo.importSync auto:) myForceSettingsFlag) (= (find "import" (checkStatus aExtentName)) 0 ))(begin
			(setq aNeedImport true))
		)

		;; Import source
		(if (= aNeedImport true)
			(cond
			((= aExtentInfo.storageScope file:)
				(if (and (<> aExtentInfo.location "") (fileExists aExtentInfo.location))
					(importSource aExtentInfo.location true) ;; import the source code and set the location
				else
					(writeln "Warning: Cannot import source " aExtentInfo.location)
				)
			)
			((= aExtentInfo.storageScope folder:)
				(importDirectory aExtentInfo.location true) ;; Import the whole folder.
			)
			);cond
		)

		;; Register any dependencies between this new cabinet and other cabinets.
		;;TM? How are dependencies stored in metadata and assigned into aExtMgr.myDependencies?
		;;TM? Is it ok that dependant cabinets have to be loaded before being referenced?
		;;Use the data in ExtentInfo's to register the cabinet dependencies
		(if (isMember dependencies: aExtentInfo) (begin
			(setq aDependencies (stringToVector aExtentInfo.dependencies " "))
			(setq N (length aDependencies))
			(loop for n from 0 until N do
				(setq aDependName (symbol aDependencies[n]))
				(setq aMgr _ais.browseLibExtents.Extents[aDependName])
				(if (not (isLambda aMgr))
					(error (append "browseLib.addExtent: dependent cabinet must be an Lambda cabinet [" aDependName "]")))
				(if (not (isStructure aExtMgr.myDependencies))
					(setq aExtMgr.myDependencies (new Structure:)))
				(if (not (isStructure aMgr.myDependents)) 
					(setq aMgr.myDependents (new Structure:)))
				(setq aExtMgr.myDependencies[aDependName] true)
				(setq aMgr.myDependents[aExtentName] true)
			) ; end register dependencies
			))

		;; Save the current status for this cabinet to the extentStatusList
		(setq extentStatusList[aExtentName] (checkStatus aExtentName))

		true) ; end of addExtent


	;; Register a host text file folder extent to the browse Lambda
	(defun addHostTextExtent(extentName folderName fileSuffix searchSW compileSW)
		vars:(extMgr)
		(_initialize)
		(setq extentName (string extentName))
		(setq extMgr (copy _hostTextManagerTemplate))
		(extMgr extentName folderName fileSuffix)
		(setq extMgr.Pv.mySearchSW searchSW)
		(setq _ais.browseLibExtents.Extents[extentName] extMgr)
		(setq _ais.browseLibExtents.Focus extMgr)
		(setq _ais.browseLibExtents.FocusIndex (member extentName _ais.browseLibExtents.Extents))
		(if (= compileSW true) (compileAll true))
		true) ; end of addHostTextExtent
	;; Register the memory manager extent to the browse Lambda
	(defun addMemoryExtent()
		vars:(objRepo extMgr extentName)
		(_initialize)
		(setq extentName ".Memory")
		(setq extMgr (copy _memoryManagerTemplate))
		(setq extMgr.myRepository (getSymbolTable 1 1 1))
		(setq extMgr.myExtentName extentName)
		(setq _ais.browseLibExtents.Extents[extentName] extMgr)
		(setq _ais.browseLibExtents.Focus extMgr)
		(setq _ais.browseLibExtents.FocusIndex (member extentName _ais.browseLibExtents.Extents))
		true) ; end of addMemoryExtent
	;; Bind all unbound global Lambdas based upon their global bindings
	;; Note: Also binds the child Lambdas of any unbound global Lambdas
	(defun bind(...)
		regs:(n N)
		vars:(symbolList theLambda childLambda childName LambdaName)
		;; Are we starting out with no arguments?
		(if (= (argCount) 0)
			(begin
				(setq symbolList (getSymbolTable 0 0 1))
				(setq N (length symbolList))
				(loop for n from 0 until N do
					(setq theLambda (getGlobalValue (setq LambdaName symbolList[n])))
					(if (and (isLambda theLambda) (<> LambdaName _currentResult:) (= theLambda.In.Binding #void))
						(bind theLambda LambdaName)
						) ; end if
					) ; end loop
				(goto Last:)
			)) ; end if 
		;; Are we starting out with two arguments?
		(if (= (argCount) 2)
			(begin
				(setq theLambda (argFetch 0))
				(setq LambdaName (argFetch 1))
				(if (not (isStructure theLambda.In)) (setq theLambda.In (new Structure:)))
				(setq theLambda.In.Binding LambdaName)
				(setq N (length theLambda.Pv))
				(loop for n from 0 until N do
					(setq childLambda theLambda.Pv[n 1])
					(setq childName theLambda.Pv[n 0])
					(if (and (isLambda childLambda) (= childLambda.In.Binding #void))
						(bind childLambda (append LambdaName ":" childName))
						) ; end if
					) ; end Pv loop
				(loop for n from 0 until (length theLambda.Cv) do
					(setq childLambda theLambda.Cv[n 1])
					(setq childName theLambda.Cv[n 0])
					(if (and (isLambda childLambda) (= childLambda.In.Binding #void))
						(bind childLambda (append LambdaName ":" childName))
						) ; end if
					) ; end Cv loop
				(goto Last:)
			)) ; end if 
		;; Anything else is an error
		(error "browseLib.bind: invalid arglist")
		;; Return after binding all unbound Lambdas
		Last::
		true) ; end of bind
	;; Check an Lambda script into the File Cabinet.
	(defun checkin(...)
		vars:(LambdaName newLambda cabinetName cabinetIndex resultStatus resultVector)
		(_initialize)
		(if (= (argCount) 2) (begin (setq LambdaName (string (argFetch 0))) (setq newLambda (argFetch 1))))
		(if (= (argCount) 3) (begin (setq cabinetName (string (argFetch 0))) (setq LambdaName (string (argFetch 1))) (setq newLambda (argFetch 2))))
		(if (<> cabinetName #void) (setq cabinetIndex (member cabinetName _ais.browseLibExtents.Extents)))
		;; Do not check in anything but a script or an Lambda.
		;(if (not (or (isString newLambda) (isLambda newLambda))) (error "browseLib.checkin requires either a script or an Lambda"))
		;; If no cabinet name specified,  check in to the file cabinet in focus.
		(if (isNumber cabinetIndex) 
			(_ais.browseLibExtents.Extents[cabinetIndex].setLambdaByKey LambdaName newLambda)
			(_ais.browseLibExtents.Focus.setLambdaByKey LambdaName newLambda)
			) ; end if
		;; Force a checkStatus call for this Lambda if it is from an extentManagerTemplate and if it needs export
		(if (isNumber cabinetIndex) 
			(begin
			(if (= true (isMember _extentManagerTemplate: _ais.browseLibExtents.Extents[cabinetIndex].Pv))
				(begin
				(setq resultStatus (checkStatus (string _ais.browseLibExtents.Extents[cabinetIndex].myExtentName)))
				;; we checked in code, we need to export the code.
				;; export only if the exportSync for the cabinet is auto: or ask:
				(if (= (find "export" resultStatus) 0)
				(begin
				(setq resultVector (stringToVector resultStatus #\tab))
				(if (or (= resultVector[3] ask:) (= resultVector[3] auto:))
					(begin
					(sendFileChangedEvent "" (string _ais.browseLibExtents.Extents[cabinetIndex].myExtentName))
					)
				))
				)
				)
			)  ;; end if
			)
			else
			(begin
				(setq resultStatus (checkStatus LambdaName))
				;; we checked in code, we need to export the code.
				;; export only if the exportSync for the cabinet is auto: or ask:
				(if (= (find "export" resultStatus) 0)
				(begin
				(setq resultVector (stringToVector resultStatus #\tab))
				(if (or (= resultVector[3] ask:) (= resultVector[3] auto:))
					(begin
					(sendFileChangedEvent "" (string LambdaName))
					)
				))
				)
			)
		)  ;; end if
		true) ; end of checkin
	;; Check an Lambda script out of the File Cabinet.
	(defun checkout(...) 
		vars:(result i LambdaName cabinetName cabinetIndex)
		(_initialize)
		(if (= (argCount) 1) (setq LambdaName (string (argFetch 0))))
		(if (= (argCount) 2) (begin (setq cabinetName (string (argFetch 0))) (setq LambdaName (string (argFetch 1)))))
		(if (<> cabinetName #void) (setq cabinetIndex (member cabinetName _ais.browseLibExtents.Extents)))
		(if (isNumber cabinetIndex) (return (_ais.browseLibExtents.Extents[cabinetIndex].refSourceByKey LambdaName)))
		;; Perform general search of all file cabinet extents.
		(setq result (_ais.browseLibExtents.Focus.refSourceByKey LambdaName))
		(if (<> result #void) (return result))
		(loop for i from 0 until (length _ais.browseLibExtents.Extents) do
			;; Search only those extents which have their auto-search switch turned on.
			(if (= _ais.browseLibExtents.Extents[i 1].Pv.mySearchSW true)
				(setq result (_ais.browseLibExtents.Extents[i 1].refSourceByKey LambdaName))
				) ; end if
			(if (<> result #void) (return result))
			) ; end loop
		result) ;; end of checkout
	;; Check an Lambda binary out of the File Cabinet.
	(defun checkoutLambda(...) 
		vars:(result i LambdaName cabinetName cabinetIndex)
		(_initialize)
		(if (= (argCount) 1) (setq LambdaName (string (argFetch 0))))
		(if (= (argCount) 2) (begin (setq cabinetName (string (argFetch 0))) (setq LambdaName (string (argFetch 1)))))
		(if (<> cabinetName #void) (setq cabinetIndex (member cabinetName _ais.browseLibExtents.Extents)))
		(if (isNumber cabinetIndex) (return (_ais.browseLibExtents.Extents[cabinetIndex].refLambdaByKey LambdaName)))
		;; Perform general search of all file cabinet extents.
		(setq result (_ais.browseLibExtents.Focus.refLambdaByKey LambdaName))
		(if (<> result #void) (return result))
		(loop for i from 0 until (length _ais.browseLibExtents.Extents) do
			;; Search only those extents which have their auto-search switch turned on.
			(if (= _ais.browseLibExtents.Extents[i 1].Pv.mySearchSW true)
				(setq result (_ais.browseLibExtents.Extents[i 1].refLambdaByKey LambdaName))
				) ; end if
			(if (<> result #void) (return result))
			) ; end loop
		result) ;; end of checkoutLambda
	;; Compile the specified Lambda script.
	(defun checkoutParent(...)
	;; *******************************************************************
	;; summary:  Checks out the specified (LambdaName) parent Lambda script along
	;;	         with all its children as a single appended string object. 
	;; Args:     LambdaName	Name of the parent Lambda to be checked out from 
	;;           the Lambda file cabinet.
	;; Return:   result The parent Lambda and all its children as a single string.
	;; *******************************************************************
		vars:(childName n startIndex result ix i LambdaName cabinetName cabinetIndex)
		(defun strip(s) (if (= (left s 7) ";#text#") (mid s 7 100000000) s))
		(if (= (argCount) 1) (setq LambdaName (string (argFetch 0))))
		(if (= (argCount) 2) (begin (setq cabinetName (string (argFetch 0))) (setq LambdaName (string (argFetch 1)))))
		(if (<> cabinetName #void) (setq cabinetIndex (member cabinetName _ais.browseLibExtents.Extents)))
		;; We treat the LambdaName as the parent Lambda part of the LambdaName
		;; appending the colon (:) symbol, so we can check out any 
		;; child Lambdas (if necessary).
		(setq childName (append "" LambdaName ":"))
		(setq n (length childName))
		(setq result "")
		(if (isNumber cabinetIndex) 
			(begin 
				(setq result (_ais.browseLibExtents.Extents[cabinetIndex].refSourceByKey LambdaName))
				(if (<> result #void) (goto Continue:))
				(return #void)
			)) ; end if
		;; Perform general search of all file cabinet extents.
		(loop for cabinetIndex from 0 until (length _ais.browseLibExtents.Extents) do
			;; Search only those extents which have their auto-search switch turned on.
			(if (= _ais.browseLibExtents.Extents[i 1].Pv.mySearchSW true)
				(setq result (_ais.browseLibExtents.Extents[cabinetIndex].refSourceByKey LambdaName))
				) ; end if
			(if (<> result #void) (goto Continue:))
			) ; end loop
		(if (= result #void) (return result))
		Continue::
		(setq startIndex (_ais.browseLibExtents.Extents[cabinetIndex].memberIndex LambdaName))
		(if (isNumber startIndex)
			(begin
				(setq result "")
				(_ais.browseLibExtents.Extents[cabinetIndex].beginTransaction)
				(setq ix startIndex)
				(setq result (append result (strip (_ais.browseLibExtents.Extents[cabinetIndex].refSourceByIndex ix))))
				(setq ix (addi ix 1))
				(while (and (< ix (_ais.browseLibExtents.Extents[cabinetIndex].refLength))
							(= (left (_ais.browseLibExtents.Extents[cabinetIndex].refKeyByIndex ix) n) childName))
					(setq result (append result (strip (_ais.browseLibExtents.Extents[cabinetIndex].refSourceByIndex ix))))
					(setq ix (addi ix 1))
					) ; end while
				(_ais.browseLibExtents.Extents[cabinetIndex].commitTransaction)
			)) ;; end of if
		result) ;; end of checkoutParent
	;; Clear the specified File Cabinet of all data.
	(defun clearCabinet(cabinetName) 
		vars:(cabinetIndex)
		(_initialize)
		(if (<> cabinetName #void) (setq cabinetIndex (member cabinetName _ais.browseLibExtents.Extents)))
		(if (isNumber cabinetIndex) 
			(_ais.browseLibExtents.Extents[cabinetIndex].clearRepository)
			(error (append "browseLib.clearCabinet: unknown file cabinet [" cabinetName "]"))
			) ; end if
		true) ;; end of clearCabinet
	;; Close import source file.
	(defun closeImportSourceFile() 
		(fileClose myImportFileID 1)
		(setq myImportBuffer #void)
		true)
	;; Close, with commit, all extents in File Cabinet
	(defun commitAll()
		vars:(n N)
		(setq N (length _ais.browseLibExtents.Extents))
		(loop for n from 0 until N do
			(_ais.browseLibExtents.Extents[n 1].commitTransaction)
			) ; end loop
		true) ;; end of commitAll
	;; Compile all Lambdas in the current file cabinet.
	;;TM: alwaysCompile is now ignored! Because of the addition of 
	;;defClone, defClass etc. The source must always be compiled to restablish
	;;the environment. As a future project we will remove this vestigle parameter.
	;; Moved implementation of compileAll into extentManager 
	(defun compileAll(alwaysCompile)
		vars:(holdMe result errSwitch ix n name anItem aExtent)
		(if (<> (type _ais.browseLibExtents.Focus) Lambda:) 
			(error (append "browseLib: Current focus is not an extent")))
		(_ais.browseLibExtents.Focus.compileAll)
		true) ;; end of compileAll

	;; Iterate through all cabinets compiling each one that is marked autoCompile
	;; and the cabinets myCompileRequired flag is true.
	(defun compileAllAutoCompileCabinets()
		regs:(n N p P)
		vars:(aExtMgr aExtentName aOldExtent aDependsExist)
		(_initialize)
		(setq aOldExtent (getFocus))
		(setq N (length _ais.browseLibExtents.Extents))
		(setq myAutoCompileList (new Vector:))
		(loop for n from 0 until N do
			(setq aExtMgr _ais.browseLibExtents.Extents[n 1])
			(setFocus (setq aExtentName _ais.browseLibExtents.Extents[n 0]))
			(if (isMember _extentManagerTemplate: _ais.browseLibExtents.Extents[n 1].Pv)
				(markAutoCompileCabinet aExtentName))
		); n
		(setq N (length myAutoCompileList))
		(loop for n from 0 until N
			(setq aExtMgr _ais.browseLibExtents.Extents[myAutoCompileList[n]])
			(setq P (aExtMgr.refLength))
			(if (= true aExtMgr.myCompileRequired)
				(loop for p from 0 until P do
					(if (not (aExtMgr.compileByIndex p true))
						(writeln "Failed in compiling: " myAutoCompileList[n] "[" n "]"))
				);n
			)
		)
		(setFocus aOldExtent)
	true)

	(defun markAutoCompileCabinet(aCabinetName)
		vars:(n N aExtMgr)
		(setq aExtMgr _ais.browseLibExtents.Extents[aCabinetName])
		(if (not (isMember aCabinetName myAutoCompileList))
			(setq myAutoCompileList[(length myAutoCompileList)] aCabinetName))
		(if (and (isMember myDependents: aExtMgr.Pv) (> (length aExtMgr.myDependents) 0))
			(begin
			(setq N (length aExtMgr.myDependents))
			(loop for n from 0 until N
				(markAutoCompileCabinet aExtMgr.myDependents[n 0])
			))
		)
	)

	(defun compileCabinet(aExtentName)
		regs:(n N p P)
		vars:(aExtMgr aDependName aOldExtent aDependsExist aMgr)
		(_initialize)
		(setq aOldExtent (getFocus))
		(setFocus aExtentName)

		(setq aExtMgr _ais.browseLibExtents.Extents[aExtentName])
		;; Note: Compiles for lambdas that have dependencies will be delayed
		;; until the dependate lambda is compiled. This behavior is implemented
		;; in the _extentManagerTemplate.compileAll routine.
		(setq aDependsExist (and (isMember myDependents: aExtMgr.Pv) (> (length aExtMgr.myDependents) 0)))
		(if (= aDependsExist true)(begin
			(aExtMgr.compileAll))
		else
			(compileAll true)
		)
		(setFocus aOldExtent)
	true)

	;; Compile all Lambda cabinets dependent on the current file cabinet.
;This code is moved into _extentManagerTemplate
;	(defun compileDependents(extentName)
;		regs:(m M n N)
;		vars:(result errSwitch)
;		vars:(extMgr depMgr depName dependents)
;		(cond
;			((isSymbol extentName)
;				(if (<> (type (setq extMgr _ais.browseLibExtents[(setq extentName (symbol extentName))])) Lambda:)  
;					(error (append "browseLib.compileDependents: specified parent is not an extent")))
;				(setq M (length (setq dependents extMgr.myDependents)))
;			) ; end Symbol case
;			((isStructure extentName)
;				(setq dependents extentName)
;				(setq M (length dependents))
;			) ; end Structure case
;		) ; end cond
;		(loop for m from 0 until M do
;			(setq depName dependents[m 0])
;			(if (<> (type (setq depMgr _ais.browseLibExtents.Extents[depName])) Lambda:)
;				(error (append "browseLib.compileDependents: specified dependent is not an extent")))
;			(depMgr.beginTransaction)
;			(setq N (depMgr.refLength))
;			(loop for n from 0 until N do
;				(setq result (depMgr.compileByIndex n true))
;				(if (<> result true) 
;					(setq errSwitch true))
;				) ; end loop
;			(depMgr.commitTransaction)
;			(if (= errSwitch true) 
;				(return "!browseLib.compileDependents: Errors occured during compilation. (see console log)!"))
;		) ; end dependents loop
;		true) ;; end of compileDependents
	;; Compile the specified Lambda script.
	(defun compileSource(name)
	;; *******************************************************************
	;; summary:  Loads, compiles, and evaluates the specified (name) Lambda 
	;;	         script in the Lambda file cabinet. If an error occurs, 
	;;	         the error message is displayed. 
	;; Args:     name	Name of the Lambda to be compiled from the Lambda file cabinet.
	;; Return:   false	Always returns false or an error.
	;; *******************************************************************
		vars:(aMyself aExtents myDependents parentName keys childName s n N startIndex 
				file brk fileID (typ 0) err focus ix
				) ;; end of temporary variables
		;; If the name contains an imbedded colon (:) symbol,
		;; then we keep only the parent Lambda part of the name
		;; preceeding the colon (:) symbol. We also compile any 
		;; child Lambdas (if necessary).
		(setq parentName name)
		(setq n (find ":" name))
		(if (isNumber n)
			(setq parentName (left name n)))
		;; Search for any child Lambdas
		(setq childName (append "" parentName ":"))
		(setq n (length childName))
		(setq startIndex (_ais.browseLibExtents.Focus.memberIndex parentName))
		(if (isNumber startIndex)
			then
			(begin
				;; We have found a parent by this name in the file cabinet.
				(_ais.browseLibExtents.Focus.beginTransaction)
				(setq ix startIndex)
				(setq err (_ais.browseLibExtents.Focus.compileByIndex ix true))
				(if (isString err)
					(begin
						(_ais.browseLibExtents.Focus.commitTransaction)
						(error err)
					)) ; end if
				(setq ix (addi ix 1))
				(while (and (< ix (_ais.browseLibExtents.Focus.refLength))
							(= (left (_ais.browseLibExtents.Focus.refKeyByIndex ix) n) childName))
					(setq err (_ais.browseLibExtents.Focus.compileByIndex ix true))
					(if (isString err)
						(begin
							(_ais.browseLibExtents.Focus.commitTransaction)
							(error err)
						)) ; end if
					(setq ix (addi ix 1))
					) ; end while
				(_ais.browseLibExtents.Focus.commitTransaction)
			)
			else
			(begin
				;; We have not found a parent by this name in the file cabinet,
				;; so we compile only the original name as specified and do not
				;; attempt to compile all of its siblings.
				(_ais.browseLibExtents.Focus.beginTransaction)
				(setq err (_ais.browseLibExtents.Focus.compileByKey name true))
				(if (isString err)
					(begin
						(_ais.browseLibExtents.Focus.commitTransaction)
						(error err)
					)) ; end if
				(_ais.browseLibExtents.Focus.commitTransaction)
			)) ;; end of if
		;; Compile all dependents -- now part of _extentManagerTemplate.compileAll routine.
		;;(compileDependents _ais.browseLibExtents.Focus.myDependents)
		(setq aMyself _ais.browseLibExtents.Focus)
		;; Compile all lambdas on which the current cabinet depends
		(if (and (isMember myDependents: aMyself.Pv) (isStructure aMyself.Pv.myDependents)) (begin
			(setq myDependents aMyself.Pv.myDependents)
			(setq aExtents _ais.browseLibExtents.Extents)
			(setq N (length myDependents))
			(loop for n from 0 until N do
				(if (isMember myDependents[n 0] aExtents) (begin
					(if browseLib.myVerboseCompile
						(writeln "    Compiling Dependency " myDependents[n 0]))
					(aExtents[myDependents[n 0]].compileAll)
				))
			);n
			))
		true) ;; end of compileSource
	;; Copy the specified source File Cabinet contents to the specified destination File Cabinet    
	(defun copyCabinetSource(sourceFileCabinetName destinationFileCabinetName)
		regs:(n N)
		vars:(childSource sourceIndex destinationIndex sourceChildNames)
		vars:(sourceFileCabinet destinationFileCabinet)
		(_initialize)
		(if (<> sourceFileCabinetName #void) (setq sourceIndex (member sourceFileCabinetName _ais.browseLibExtents.Extents)))
		(if (isNumber sourceIndex) 
			(setq sourceFileCabinet _ais.browseLibExtents.Extents[sourceIndex])
			(error (append "browseLib.copyCabinet: unknown cabinet name [" sourceFileCabinetName "]"))
			) ; end if
		(if (<> destinationFileCabinetName #void) (setq destinationIndex (member destinationFileCabinetName _ais.browseLibExtents.Extents)))
		(if (isNumber destinationIndex) 
			(setq destinationFileCabinet _ais.browseLibExtents.Extents[destinationIndex])
			(error (append "browseLib.copyCabinet: unknown cabinet name [" destinationFileCabinetName "]"))
			) ; end if
		;; Retrieve list of all child names in the source file cabinet.
		(sourceFileCabinet.beginTransaction)
		(setq N (sourceFileCabinet.refLength))
		(setq sourceChildNames (new Vector: N))
		(loop for n from 0 until N do
			(setq sourceChildNames[n] (sourceFileCabinet.refKeyByIndex n))
			) ;; end of loop
		;; Copy list of all child names from the source file cabinet to the destination file cabinet.
		(loop for n from 0 until N do
			(writeln "[" n "] copying " sourceChildNames[n])
			(setq childSource (sourceFileCabinet.refSourceByKey sourceChildNames[n]))
			(destinationFileCabinet.setSourceByKey sourceChildNames[n] childSource)
			) ;; end of loop
		true) ;; end copyCabinetSource
	;; Convert the global variables to a delimited string.
	(defun delimitedGlobals(showCFuncs showLocked)
		vars:(nameVec symbolVec valueVec i n (showSorted 1) result)
		(setq _sysError _sysErrorStop) ;; Trap all system errors here.
		(setq _currentResult #void)   ;; Drop all previous errors here.
		(setq _errorMsg #void)        ;; Drop all previous errors here.
		(setq symbolVec (getSymbolTable showSorted showCFuncs showLocked))
		(setq valueVec (new Vector: 0))
		(setq nameVec (new Vector: 0))
		(setq n (length symbolVec))
		(loop for i from 0 until n do
			(setq nameVec[i] symbolVec[i])
			(setq valueVec[i] (getGlobalValue nameVec[i]))
			) ;; end loop
		(setq result (objectToStructure nameVec valueVec))
		(setq result (delimitedString result " = " #\tab))
		(setq _sysError #void)
		result) ;; end delimitedGlobals
	;; Convert the specified object to a delimited string.
	(defun delimitedString(anObj midDel endDel ...) 
	;; *******************************************************************
	;; summary:  Convert the specified object to a delimited string. Each
	;;           delimiter, and two item elements have each item separated
	;;           by the midDel delimiter.
	;; args:     anObj       The object to be converted into a string.
	;;           midDel      The delimiter between elements items.
	;;           endDel      The delimiter separating different elements.
	;;           lineNum     (Optional) Include line numbers before Vector elements (must be a numeric offset, starting at 0, if line numbers are to be included).
	;;           fieldName   (Optional) Treat object as a vector of Field elements.
	;; return:   dString     The converted delimited string   
	;; *******************************************************************
		vars:(title i n lineNum cols fieldName tempVector)
		;; Retrieve any optional arguments
		(if (>= (argCount) 4) (setq lineNum (argFetch 3)) (setq lineNum false))
		(if (>= (argCount) 5) (setq fieldName (argFetch 4)) (setq fieldName #void))
		(if (= lineNum true) (setq lineNum 0))
		(setq dString "")
		;; If a field name is specified, convert the object to a Vector of field elements.
		(if (and (<> fieldName #void) (or (= (type anObj) Brick:) (isVector anObj))) 
			(begin
				(setq n (length anObj))
				(setq tempVector (|Gv:new| Vector: n))
				(loop for i from 0 until n do (setq tempVector[i] anObj[i][fieldName]))
				(setq anObj tempVector)
			)) ; end if
		;; Use the object type to determine the string format.
		(if (isType Lambda: anObj) (setq n 0) (setq n (length anObj)))
		(cond  
			;; Disassemble a Lambda into a string.
			((isType Lambda: anObj)
			(begin
				(setq title (globalBinding anObj))
				(if (= title #void)
					(setq title (string anObj)))
				(setq dString (append "Binding" midDel title endDel))
				(setq dString (append dString "args:" midDel (removeDelimiters (string anObj.Av)) endDel))
				(setq dString (append dString "svars:" midDel (removeDelimiters (string anObj.Sv)) endDel))
				(setq dString (append dString "vars:" midDel (removeDelimiters (string anObj.Tv)) endDel))
				(setq dString (append dString "pvars:" midDel (removeDelimiters (string anObj.Pv)) endDel))
				(setq dString (append dString "cvars:" midDel (removeDelimiters (string anObj.Cv)) endDel))
				(setq dString (append dString "regs:" midDel (removeDelimiters (string anObj.Rv)) endDel))
				(setq dString (append dString "faces:" midDel (removeDelimiters (string anObj.In)) endDel))
				(setq dString (append dString "vm:" midDel (removeDelimiters (string anObj.Vm)) endDel))
				(setq dString (append dString "Instructions:" endDel))
				;(setq dString (append dString (disassemble anObj srcOnly: short:)))
				(setq dString (append dString (disassemble anObj)))
				))
			;; Format a string for an Object Repository.
			((= (type anObj) ObjectRepository:)
			(begin
				(beginTransaction anObj) 
				(loop for i from 0 until n do
					(if (<> dString "") (setq dString (append dString endDel)))
					(setq dString (append dString "Key[" i "]" midDel (removeDelimiters anObj[i 0])))
					) ;; end loop
				(commitTransaction anObj) 
				))
			;; Format an object of rank two.
			((or (isDictionary anObj) (= (type anObj) Directory:) (isStructure anObj))
			(loop for i from 0 until n do
				(if (<> dString "") (setq dString (append dString endDel)))
				(setq dString (append dString (removeDelimiters anObj[i 0]) midDel))
				(setq dString (append dString (removeDelimiters anObj[i 1])))
				))
			;; Format a Brick with multiple rows. 
			((and (= (type anObj) Brick:) (> (setq n anObj["RowCount"]) 0))
			(loop for i from 0 until n do
				(if (<> dString "") (setq dString (append dString endDel)))
				;;(setq dString (append dString (removeDelimiters anObj[i 0]) midDel))
				(setq dString (append dString "[" i "]" midDel))
				(setq dString (append dString (removeDelimiters (string anObj[i]))))
				;;(setq dString (append dString (removeDelimiters (string anObj[i] true))))
				))
			;; Format a Brick with a single row and multiple fields. 
			((and (= (type anObj) Brick:) (> (setq n (length (setq cols (refAttributes anObj["FieldList"])))) 0))
			(loop for i from 0 until n do
				(if (<> dString "") (setq dString (append dString endDel)))
				;;(setq dString (append dString (removeDelimiters anObj[i 0]) midDel))
				(setq dString (append dString cols[i] midDel))
				(setq dString (append dString (removeDelimiters string anObj[i])))
				;;(setq dString (append dString (removeDelimiters (string anObj[i] true))))
				))
			;; Format a BrickRow with multiple fields.
			((and (= (type anObj) BrickRow:) (> (setq n (length (setq cols (refAttributes anObj["FieldList"])))) 0))
			(loop for i from 0 until n do
				(if (<> dString "") (setq dString (append dString endDel)))
				(setq dString (append dString (removeDelimiters cols[i]) midDel))
				(setq dString (append dString (removeDelimiters anObj[i])))
				))
			;; Format a BrickField with multiple repeats.
			((and (= (type anObj) BrickField:) (> (setq n anObj[repeats:]) 0))
			(loop for i from 0 until n do
				(if (<> dString "") (setq dString (append dString endDel)))
				(setq dString (append dString "[" i "]" midDel))
				(setq dString (append dString (removeDelimiters anObj[i])))
				))
			;; Format a Vector with attributes.
			((and (= (type anObj) Vector:) (= (type (setq cols (refAttributes anObj))) ObjVector:))
			(loop for i from 0 until n do
				(if (<> dString "") (setq dString (append dString endDel)))
				(setq dString (append dString (removeDelimiters cols[i]) midDel))
				(setq dString (append dString (removeDelimiters anObj[i])))
				))
			;; Format a string.
			((isString anObj)
			(setq dString (removeDelimiters anObj)))
			;; Format a single element object.
			((= n 0)
			(if (isVector anObj)
				(setq dString "")
				(setq dString (removeDelimiters anObj))
				))
			;; Format a string of rank one.
			(else
			(loop for i from 0 until n do
				(if (<> dString "") (setq dString (append dString endDel)))
				(if (<> lineNum false)
					(setq dString (append dString "[" (+ lineNum i) "] " (removeDelimiters anObj[i])))
					(setq dString (append dString (removeDelimiters anObj[i])))
					) ; end if
				))) ;; end cond
		dString) ;; delimitedString
	;; Unregister a file cabinet extent from the browse Lambda
	(defun dropExtent(extentName)
		(setq extentName (string extentName))
		(setq _ais.browseLibExtents.Focus #void)
		(setq _ais.browseLibExtents.FocusIndex -1)
		(setq _ais.browseLibExtents.Extents[extentName] #void)
		(if (> (length _ais.browseLibExtents.Extents) 0)
			(setFocus _ais.browseLibExtents.Extents[0 0]))
		true) ; end of dropExtent
	;; Manage source erasing including all related child Lambdas.
	(defun eraseChildren(command)
		vars:(name LambdaName LambdaSource s i n
				startIndex endIndex
				parentName parentLen
				childName childLen
				file brk focus keys
				) ;; end of temporary variables
		;; Check for a request to erase "all" Lambdas.
		(if (= command "") (setq command "..all Lambdas.."))
		(if (= command "..all Lambdas..") (setq LambdaName "") (setq LambdaName (string command)))        
		;; Compute the parent Lambda name and length.
		;; If the name contains an imbedded colon (:) symbol,
		;; then we keep only the parent Lambda part of the name
		;; preceeding the colon (:) symbol.
		(setq n (find ":" LambdaName))
		(if (isNumber n)
			(setq parentName (left LambdaName n))
			(setq parentName LambdaName))
		(setq parentLen (length parentName))
		;; Compute the parent Lambda name and length.
		(if (= parentName "")
			(setq childName "")         
			(setq childName (append "" parentName ":")))
		(setq childLen (length childName))
		;; Position the start and end index at the Lambda to be erased.
		(if (= command "..all Lambdas..")
			;; We are erasing all Lambdas. 
			(begin
				(setq startIndex 0)
				(setq endIndex (_ais.browseLibExtents.Focus.refLength))
				) ;; end of then
			;; We are erasing one parent and its child Lambdas. 
			(begin
				(setq endIndex (_ais.browseLibExtents.Focus.refLength))
				(loop for startIndex from 0 until endIndex do
					;; Quit if this Lambda is not to be erased.
					(setq name (_ais.browseLibExtents.Focus.refKeyByIndex startIndex))
					(if (or (= parentName name) (= childName (left name childLen))) (goto FoundStartIndex:))
					) ; end loop
				FoundStartIndex::
				) ;; end of else
			) ;; end of if
		;; Erase each Lambda script from the file cabinet.
		(_ais.browseLibExtents.Focus.beginTransaction) 
		(loop for i from startIndex until endIndex do
			;; Quit if this Lambda is not to be erased.
			(setq name (_ais.browseLibExtents.Focus.refKeyByIndex i))
			(if (and (<> command "..all Lambdas..")
						(<> parentName name)
						(<> childName (left name childLen)))
				(goto CloseFileCabinet:)
				) ;; end if
			;; Erase this Lambda.
			(setq name (string name))
			(_ais.browseLibExtents.Focus.deleteLambdaByKey name)
			(setq i (subi i 1))
			(setq endIndex (subi endIndex 1))
			) ; end loop
		CloseFileCabinet::
		(_ais.browseLibExtents.Focus.commitTransaction) 
		true) ;; end eraseChildren
	;; Erases an Lambda (including all its source) from the file cabinet. 
	(defun eraseSource(...)
		vars:(LambdaName cabinetName cabinetIndex)
		(_initialize)
		(if (= (argCount) 1) (begin (setq LambdaName (string (argFetch 0)))))
		(if (= (argCount) 2) (begin (setq cabinetName (string (argFetch 0))) (setq LambdaName (string (argFetch 1)))))
		(if (<> cabinetName #void) (setq cabinetIndex (member cabinetName _ais.browseLibExtents.Extents)))
		;; If no cabinet name specified,  check in to the file cabinet in focus.
		(if (isNumber cabinetIndex) 
			(_ais.browseLibExtents.Extents[cabinetIndex].deleteLambdaByKey LambdaName)
			(_ais.browseLibExtents.Focus.deleteLambdaByKey LambdaName)
			) ; end if
		true)
	;; Handle errors gracefully
	(defun errorHandler(errmsg) (msgbox (append "browseLib got this error: " errmsg)))
	;; Uses the Workbench IO functions to export Lambda
	;;  scripts in the Lambda file cabinet to a source file.
	;; If fileName is "..Lambda location..", use the Lambda's location in meta data
	(defun exportSource(fileName LambdaName ...)
		vars:(oldPath aBuffer aDir aFileID (typ 0) aFileName aExtentInfo (aSetLocation false))
		;; Create the output source file containing all 
		;; the Lambda scripts in the Lambda file cabinet.
		(if (= (argCount) 3)
			(setq aSetLocation (argFetch 2)))
		(setq aSetLocation (boolean aSetLocation))

		(setq oldPath _path)
		(setq aDir (^new browseLib.dir))
		(setq _path "")
		(setq aExtentInfo (loadMetaData _ais.browseLibExtents.Focus.myRepository))
		(if (and (<> fileName #void)
				(<> aExtentInfo.location #void)
				(or (= "..Lambda location.." fileName) (and (<> aExtentInfo #void) (= (aDir.absFilePath aExtentInfo.location) (aDir.absFilePath fileName)))))
			(begin
			(setq aFileName aExtentInfo.location)
			(setq aSetLocation true)
			)
			else
			(setq aFileName fileName)
		) ;; end if
		(setq aFileID  (fileOpen aFileName 1 typ))
		(setq _path oldPath)
		;; Write the export source file header records.
		(fwriteln aFileID ";;*************************************")
		(fwriteln aFileID ";;*************************************")
		(fwriteln aFileID ";; Exported Lambda File Cabinet Document")
		(fwriteln aFileID ";;*************************************")
		(fwriteln aFileID ";;*************************************")
		(setq N (length aExtentInfo))
		(fwriteln aFileID ";;version="(isVersion))
		(loop for n from 0 until N do
			(if (and (<> "fileHash" aExtentInfo[n 0]) (<> "cabinetHash" aExtentInfo[n 0]))
				(fwriteln aFileID ";;"aExtentInfo[n 0]"="aExtentInfo[n 1]) 
			)
		)
		(fwriteln aFileID ";;END __DATA" _eol)
		;; Write the export source file records.
		(_exportSource aFileID LambdaName)
		;; Close the export source file.
		(fileClose aFileID 1)

		(if aSetLocation
			(begin
			(setq aExtentInfo.location aFileName)
;;			(setq aExtentInfo.lastImport (fileTimeStamp aExtentInfo.location)) ;;TM? No longer used.
			(setq aExtentInfo.storageScope file:)
			(setq aFileID (fileOpen aExtentInfo.location 0 0))
			(setq aBuffer (fileReadRecord aFileID))
			(fileClose aFileID 1)
			(setq aExtentInfo.fileHash (md5hash (string aBuffer))) ;; fileHash for the single export file
			;;Iterate through all of the individual items in the cabinet and generate a fresh hash value
			;;for each item. This hash is used in the checkStatus routine to identify when chanes have
			;;occured to an item.
			(setq N (_ais.browseLibExtents.Focus.refLength))
			(loop for n from 0 until N do
				(setq aTmpKey (_ais.browseLibExtents.Focus.refKeyByIndex n))
				(setq aExtentInfo.cabinetHash[aTmpKey] (md5hash (string (_ais.browseLibExtents.Focus.refSourceByKey aTmpKey))))
			)
			(saveMetaData _ais.browseLibExtents.Focus.myRepository aExtentInfo)
			(watchFile aExtentInfo.location (getFocus))
			)
		) ;; end if
		(if (<> extentStatusList #void) (setq extentStatusList[(getFocus)] (checkStatus (getFocus))))
		true) ;; end exportSource
	;; Uses the Workbench IO functions to export Lambda
	;; Return a vector of all source file names in the file cabinet.
	(defun getChildNames()
		vars:(i v)
		(_ais.browseLibExtents.Focus.beginTransaction)
		(setq v (new Vector: (_ais.browseLibExtents.Focus.refLength)))
		(loop for i from 0 until (_ais.browseLibExtents.Focus.refLength) do
			(setq v[i] (_ais.browseLibExtents.Focus.refKeyByIndex i))
			) ;; end of loop
		(_ais.browseLibExtents.Focus.commitTransaction)
		v)
	;; Return the count of all file cabinet extents.
	(defun getExtentCount() (length _ais.browseLibExtents.Extents))
	;; Return a vector of all file cabinet extent names.
	(defun getExtentNames() (if (= _ais.browseLibExtents.Extents #void)(return false))(refAttributes _ais.browseLibExtents.Extents))
	;; Return the name of the file cabinet extent currently in focus.
	(defun getFocus() 
		vars:(aBrowseLibExtents)
		(setq aBrowseLibExtents _ais.browseLibExtents) ;;debug helper
		_ais.browseLibExtents.Extents[_ais.browseLibExtents.FocusIndex 0])
	;; Return the index of the file cabinet extent currently in focus.
	(defun getFocusIndex() _ais.browseLibExtents.FocusIndex)
	;; Return a vector of all source file names in the file cabinet.
	(defun getKeys() (getChildNames))
	;; Return an XPath directory string list for the next repository directory level.
	(defun getNextLevel(cabinetName LambdaName startLine lineCount ...)
		vars:(i I cabinetIndex s option1 options)
		;; Note1: Each XPath directory line has the following tab delimited fields:
		;;         type    		{..data type..}
		;;         value   		{LambdaName for each node in the repository.}
		;;         size			none
		;;         date			none
		;;         time			none
		;;         version			{current AIS version}
		;;         symbolicKey		{LambdaName for each node in the repository}
		;;         uniqueKey		{LambdaIndex for each node in the repository}
		(if (= (setq cabinetIndex (member cabinetName _ais.browseLibExtents.Extents)) false) 
			(error (append "browseLib: cannot focus on " cabinetName)))
		(if (>= (argCount) 5) (setq options (argFetch 4)))
		(setq s (_ais.browseLibExtents.Extents[cabinetIndex 1].getNextLevel LambdaName  startLine lineCount options))
		s)2
	;; Return a vector of all source file names in the file cabinet.
	(defun getParentNames()
		vars:(i parentCount name v)
		(_ais.browseLibExtents.Focus.beginTransaction)
		(setq parentCount 0)
		(setq v (new Vector: (addi (_ais.browseLibExtents.Focus.refLength) 1)))
		(setq v[0] "..all Lambdas..")
		(loop for i from 0 until (_ais.browseLibExtents.Focus.refLength) do
			(setq name (_ais.browseLibExtents.Focus.refKeyByIndex i))
			;; Only save the name if it is a parent Lambda.              
			(if (= (find ":" name) false)                
				(setq v[(setq parentCount (addi parentCount 1))] name))
			) ;; end of loop
		(_ais.browseLibExtents.Focus.commitTransaction)
		(resize v (addi parentCount 1))
		v)
	;; Return an XPath directory type list string for all possible types in a hierarchical descendents list 
	(defun getTypes(cabinetName)
		vars:(cabinetIndex result)
		(if (= (setq cabinetIndex (member cabinetName _ais.browseLibExtents.Extents)) false) 
			(error (append "browseLib: cannot focus on " cabinetName)))
		(setq result (_ais.browseLibExtents.Extents[cabinetIndex 1].getTypes))
		result)
	;; Return the name of a default html page.
	(defun htmlPageServer(pageName pageParm)
		vars:(page action)
		(setq pageName (string pageName))
		(if (= (setq action htmlPageRegistry[pageName]) #void)
			then
			(begin
				(setq page "<HTML><BODY><H1 ALIGN=center>SmartBase/LambdaServer Blank Home Page</H1></BODY></HTML>")
				(if (= _path #void) (setq _path ""))
				(if (= _path "")
					then
					(setq pageName "c:\\default.html")
					else
					(setq pageName "default.html")
					) ; end if
				(writeSourceFile pageName page)
				) ;; end begin
			else
			(setq pageName (action pageName (htmlParmToStructure pageParm)))
			) ;; end if
		(setq pageName (append _path pageName))
		pageName)
	;; Convert the HTML parameter string to a structure.
	(defun htmlParmToStructure(pageParm)
		vars:(vtemp stemp parmStructure parmCount parmIndex equalLoc)
		(if (= pageParm "") (return #void))
		(setq stemp (substitute pageParm "%20" " "))
		(setq vtemp (stringToVector stemp "[$$$]"))
		(setq parmCount (length vtemp))
		(setq parmStructure (new Structure:))
		(loop for parmIndex from 0 until parmCount do
			(setq equalLoc (find "=" vtemp[parmIndex]))
			(setq parmStructure[(symbol (left vtemp[parmIndex] equalLoc))] 
					(mid vtemp[parmIndex] (add1 equalLoc) 100000))
			) ;; end loop
		parmStructure)
	;; Imports a source file of multiple Lambdas into the file
	;;  cabinet. The source file must be in the export format
	;;  output from the export function.
	(defun importSmallSource(name)
		vars:(s v i n LambdaName focus Lambda childSize matchStr)
		(_initialize)
		;; Set up child Lambda focus vector.
		(setq focus (^new Vector: 0)) 
		;; Read the import source file containing all 
		;; the Lambda scripts for the Lambda file cabinet.
		(setq s (string (readSourceFile name)))
		;; Break the source into a vector of components.
		(setq v (stringToVector s (append _eol ";;**EXPORTKEY**:")))
		(if (and (= (length v) 1) (= v[0] s)) (setq v (stringToVector s (append #\return ";;**EXPORTKEY**:"))))
		(if (and (= (length v) 1) (= v[0] s)) (setq v (stringToVector s (append #\newline ";;**EXPORTKEY**:"))))
		(if (and (= (length v) 1) (= v[0] s)) (setq v (stringToVector s (append #\newline #\return ";;**EXPORTKEY**:"))))
		;; Write each Lambda script to the file cabinet.
		(_ais.browseLibExtents.Focus.beginTransaction)
		(loop for i from 1 until (length v) do
			;; Isolate the Lambda script from the ;;**EXPORTKEY**: header.
			(setq n (find (setq matchStr (append "" #\return #\newline)) v[i]))
			(if (= n false) (setq n (find (setq matchStr (append "" #\newline #\return)) v[i])))
			(if (= n false) (setq n (find (setq matchStr (append "" #\return)) v[i])))
			(if (= n false) (setq n (find (setq matchStr (append "" #\newline)) v[i])))
			(setq LambdaName (left v[i] n))
			(setq LambdaName (string LambdaName))
			(setq Lambda (mid v[i] (+ n (length matchStr)) 100000000))
			;; Import the script for an Lambda
			(writeln "[" i "] Importing source for " LambdaName)
			(_ais.browseLibExtents.Focus.setSourceExByKey LambdaName Lambda)
			) ; end loop
		(writeln "Completed importing [" (subi i 1) "] Lambdas.")
		(_ais.browseLibExtents.Focus.commitTransaction) 
		true)
	;; Imports a source file of multiple Lambdas into the file
	;;  cabinet. The source file must be in the export format
	;;  output from the export function.
	;;  Adding a second argument with true as value will set the imported file as the cabinet's source file location.
	(defun importSource(name ...)
		vars:(n N Lambda LambdaName fileSize (aType 0) aDir)
		vars:(aBuffer aFileID self aExtentInfo (aSetLocation "true") oldName)
		(setq oldName name)
		(setq aDir (^new browseLib.dir))
		(_initialize)
		(if (= (argCount) 2)
			(setq aSetLocation (argFetch 1))
		)

		(if (<> _ais.browseLibExtents.Focus.myRepository #void)
			(begin
			(setq aExtentInfo (loadMetaData _ais.browseLibExtents.Focus.myRepository))
			;; If the location in the metadata is the same as file being imported,
			;; mark this import operation to update the metadata information.
			(if (and (<> name #void) (<> aExtentInfo.location #void)
					(or (= "..Lambda location.." name) (= (aDir.absFilePath aExtentInfo.location) (aDir.absFilePath name))))
				(begin
				(setq name aExtentInfo.location)
				(setq oldName aExtentInfo.location)
				(setq aSetLocation true)
				)
			)
			)
		)
		;; If the import source file is small, then use the faster import Lambda.
		(setq aFileID (fileOpen name 0 aType))
		(setq fileSize (fileSeek aFileID 0 2)) 
		(fileClose aFileID 1)
		;(if (<= fileSize 30000000) (return (importSmallSource name)))        
		;; Open the import source file containing all 
		;; the Lambda scripts for the Lambda file cabinet.
		(openImportSourceFile name)
		(_ais.browseLibExtents.Focus.beginTransaction)
		;; Write each Lambda script to the file cabinet.
		(while (<> myImportName #void) do
			(setq LambdaName myImportName)
			;; Import the script for an Lambda
			(writeln "[" (setq n (addi n 1)) "] Importing source for " LambdaName)
			(setq Lambda (readImportSourceRecord))
			(_ais.browseLibExtents.Focus.setSourceExByKey LambdaName Lambda)
			) ; end while   
		(writeln "Completed importing [" (subi n 1) "] Lambdas.")
		(_ais.browseLibExtents.Focus.commitTransaction) 
		(closeImportSourceFile)
		(if (and (<> aExtentInfo #void) (<> _ais.browseLibExtents.Focus.myRepository #void))
			(begin
				(if (or (= aSetLocation true) (= aSetLocation "true"))
				(begin
				(setq aExtentInfo.location oldName)
				(setq aExtentInfo.lastImport (fileTimeStamp oldName))
				(setq aExtentInfo.storageScope "file")
				(setq aFileID (fileOpen aExtentInfo.location 0 0))
				(setq aBuffer (fileReadRecord aFileID))
				(fileClose aFileID 1)
				(setq aExtentInfo.fileHash (md5hash (string aBuffer)))
				(setq N (_ais.browseLibExtents.Focus.refLength))
				(loop for n from 0 until N do
					(setq aTmpKey (_ais.browseLibExtents.Focus.refKeyByIndex n))
					(setq aExtentInfo.cabinetHash[aTmpKey] (md5hash (string (_ais.browseLibExtents.Focus.refSourceByKey aTmpKey))))
				)
				(saveMetaData _ais.browseLibExtents.Focus.myRepository aExtentInfo)
				(watchFile aExtentInfo.location (getFocus))
				(if (= aExtentInfo.autoCompile "true")
					(begin
					(compileAll true)
					)
				)
				)
				)
			)
		)
		(setq extentStatusList[(getFocus)] (checkStatus (getFocus)))
		true)
	;; Return a Structure of symbols and their closure size.
	(defun inspect(typeSW)
		vars:(i n result valueVector)
		(setq result (new Structure:))
		(cond
			((= typeSW Lambdas:) (setq valueVector (getParentNames)))
			((= typeSW globals:) (setq valueVector (getSymbolTable 0 0 1)))
			((= typeSW stats:) (return (^inspect true)))
			) ;; end of cond
		;; Create result structure of objects and closure sizes. 
		(setq n (length valueVector))
		(loop for i from 0 until n do
			(set result (symbol (string valueVector[i])) (sizeof (getGlobalValue (symbol valueVector[i]))))
			) ;; end of loop              
		(sort result > byValue:)
		result)
	;; Loads the application from the specified cabinet
	(defun loadApplication(cabinetName LambdaName saveSW)
		regs:(n N)
		vars:(newLambda cabinetIndex globals sym app newLambda)
		(_initialize)
		(setq newLambda (checkoutLambda cabinetName LambdaName))
		(if (= newLambda #void)
			(if (<> saveSW true)
				(error "browseLib.loadApplication: no application found under the specified cabinet name and Lambda name")
				(return (saveApplication cabinetName LambdaName))
				) ; end if
			) ; end if
		;; Load the specified application.
		(newLambda)
		(^gc compact:)
		newLambda) ; end of loadApplication
	;; Open source file for import.
	(defun openImportSourceFile(name) 
		vars:(nextLine (aType 0))
		(setq myImportFileID (fileOpen name 0 aType))
		(setq myImportBuffer (fileReadRecord myImportFileID))
		(setq nextLine (fileReadRecord myImportFileID myImportBuffer))
		;; Read until we encounter the start of the first exported Lambda header.
		(while (and (<> nextLine #void) (<> (left nextLine 16) ";;**EXPORTKEY**:")) do
			(setq nextLine (fileReadRecord myImportFileID myImportBuffer))
			) ; end while
		(if (= nextLine #void) (setq myImportName #void) (setq myImportName (substitute (mid nextLine 16 1000000) "." ":")))
		true)
	;; Read next import source file record 
	(defun readImportSourceRecord() 
		vars:(record nextLine)
		(if (= myImportName #void) (return #void))
		(setq record (new Vector: byte: 32000000))
		(setq nextLine (fileReadRecord myImportFileID myImportBuffer))
		;; Read until we encounter the start of the next exported Lambda header.
		(while (and (<> nextLine #void) (<> (left nextLine 16) ";;**EXPORTKEY**:")) do
			(appendWriteln record nextLine _eol)
			(setq nextLine (fileReadRecord myImportFileID myImportBuffer))
			) ; end while
        (if (= nextLine #void) 
           (setq myImportName #void)
           (begin
           (setq myImportName (substitute (mid nextLine 16 1000000) "." ":"))
           ;; Remove extra _eol
           (setq record[(- record.AppendLength 1)] 0)
           ) ; not #void
        ) ; end if
		(string record))
	;; Perform a complete read of the specified source file.
	(defun readSourceFile(name) 
		vars:(fileID self (aType 0))
		(setq fileID (fileOpen name 0 aType))
		(setq self (fileRead fileID))
		(fileClose fileID 1)
		self)
	;; Remove all delimiters from a string
	(defun removeDelimiters(anObj) 
		vars:(result)
		(if (isString anObj) 
			(setq result anObj)
			(setq result (string anObj))
			) ; end if
		(setq result (substitute result (char 9) " "))
		(setq result (substitute result (char 10) " "))
		(setq result (substitute result (char 13) " "))
		(if (> (length result) maxLine) (setq result (append (left result maxLine) "...")))
		result)
	;; Saves the current application in the specified cabinet
	(defun saveApplication(cabinetName LambdaName)
		regs:(n N)
		vars:(newLambda cabinetIndex globals sym app index newLambda)
		(_initialize)
		;; Create a current application Lambda containing the values of all currently defined global variables.
		(setq newLambda (eval (compile (lisp {(lambda() regs:(n N) pvars:(myGlobals) (setq N (length myGlobals)) (loop for n from 0 until N do (set myGlobals[n 0] myGlobals[n 1])) true)}))))
		(setq app (new Structure:))
		(setq globals (getSymbolTable 1 0 0))
		(if (isNumber (setq index (member _ais.browseLibExtents: globals))) (delete globals index))
		(if (isNumber (setq index (member browseLib: globals))) (delete globals index))
		(setq N (length globals))
		(loop for n from 0 until N do (setq sym globals[n]) (setq app[sym] (getGlobalValue sym)))
		(setq newLambda.Pv.myGlobals app)
		;; Save the current application in the cabinet name and Lambda name specified.
		(browseLib.checkin cabinetName LambdaName newLambda)
		(^gc compact:)
		newLambda) ; end of saveApplication
	;; Set the file cabinet extent focus to the specified extent.
	(defun setFocus(extentName)
		vars:(focusIndex)
		(if (<> _ais.browseLibExtents.Focus #void) (_ais.browseLibExtents.Focus.abortFocus))
		(setq extentName (string extentName))
		(if (= (setq focusIndex (member extentName _ais.browseLibExtents.Extents)) false) 
			(error (append "browseLib: cannot focus on " extentName)))
		(setq _ais.browseLibExtents.FocusIndex focusIndex)
		(setq _ais.browseLibExtents.Focus _ais.browseLibExtents.Extents[focusIndex 1])
		(_ais.browseLibExtents.Focus.setFocus)
		true) ; end of setFocus
	;; Issue the specified SQL command and return the results as a Brick.
	(defun sqlb(command ...)
		vars:(result)
		;; Bring the proper database into focus.
		(if (>= (argCount) 2) (setq mySQLDatabase (argFetch 1)))
		(if (not (isNumber mySqlHandle)) (setq mySqlHandle (sql connect:)))
		(if (<> mySQLDatabase #void) (sql mySqlHandle (append "create database if not exists " mySQLDatabase)))
		(if (<> mySQLDatabase #void) (sql mySqlHandle (append "use " mySQLDatabase)))
		(setq result (sql mySqlHandle command))
		(if (isNumber mySqlHandle) (setq mySqlHandle (sql disconnect: mySqlHandle)))
		result) ; end sqlb
	;; Issue the specified SQL command and return the results as a memory cursor.
	(defun sqlc(command ...)
		vars:(result cursor)
		;; Bring the proper database into focus.
		(if (>= (argCount) 2) (setq mySQLDatabase (argFetch 1)))
		(if (not (isNumber mySqlHandle)) (setq mySqlHandle (sql connect:)))
		(if (<> mySQLDatabase #void) (sql mySqlHandle (append "create database if not exists " mySQLDatabase)))
		(if (<> mySQLDatabase #void) (sql mySqlHandle (append "use " mySQLDatabase)))
		(setq result (sql mySqlHandle command))
		(if (isNumber mySqlHandle) (setq mySqlHandle (sql disconnect: mySqlHandle)))
		(setq cursor (browseLib.memoryCursor SQLQuery: #(obj|)))
		(cursor.attachBrick result)
		cursor) ; end sqlc
	;; Issue the specified SQL command and display the results on the console.
	(defun sqld(command ...)
		regs:(n N)        
		vars:(result startTime endTime)
		;; Bring the proper database into focus.
		(setq startTime (getTickCount 0))
		(if (>= (argCount) 2) (setq mySQLDatabase (argFetch 1)))
		(if (not (isNumber mySqlHandle)) (setq mySqlHandle (sql connect:)))
		(if (<> mySQLDatabase #void) (sql mySqlHandle (append "create database if not exists " mySQLDatabase)))
		(if (<> mySQLDatabase #void) (sql mySqlHandle (append "use " mySQLDatabase)))
		(setq result (sql mySqlHandle command))
		(if (isNumber mySqlHandle) (setq mySqlHandle (sql disconnect: mySqlHandle)))
		(writeln "SQL command completed in [" (setq endTime (getTickCount startTime)) "] seconds.")
		;; Display the results on the console
		(setq N (length result))
		(loop for n from 0 until N do (writeln "[" n "]" result[n]))(return result)
		result) ; end sqld
	;; Remove the source from every Lambda in the current file cabinet.
	(defun stripAll(...)
		regs:(n N)
		vars:(holdMe result errSwitch name anItem cabinetName cabinetIndex mgr)
		(setq holdMe browseLib)
		(if (>= (argCount) 1) (setq cabinetName (argFetch 0)))
		(if (= (setq cabinetIndex (member cabinetName _ais.browseLibExtents.Extents)) false) (setq cabinetIndex _ais.browseLibExtents.FocusIndex))
		(setq mgr _ais.browseLibExtents.Extents[cabinetIndex 1])
		(mgr.beginTransaction)
		(setq N (mgr.refLength))
		(loop for n from 0 until N do
			(mgr.deleteSourceByIndex n)
			) ; end loop
		(mgr.commitTransaction)
		(setq browseLib holdMe)
		(if (= errSwitch true) (return "!Errors occured during strip source. (see console log)!"))
		true) ;; end of stripAll
	;; Perform a complete inspection of each registered repository extent.
	(defun systemCheck()
		vars:(n N)
		(writeln _eol "Starting a complete system check of all repository extents.")
		(setq N (length _ais.browseLibExtents.Extents))
		(loop for n from 0 until N do
			(writeln "Starting inspection of " _ais.browseLibExtents.Extents[n 0] " repository extent.")
			(_ais.browseLibExtents.Extents[n 1].beginTransaction)
			(_ais.browseLibExtents.Extents[n 1].inspectRepository)
			(_ais.browseLibExtents.Extents[n 1].abortTransaction)
			) ; end loop
		true) ;; end of systemCheck
	;; Return a tab delimited string of Lambda script names.
	(defun tabbedNameList() 
		vars:(vec i n tabString)
		(setq vec (getKeys))
		(setq n (length vec))
		(setq tabString "")
		(loop for i from 0 until n do
			(if (<> tabString "") (setq tabString (append tabString #\tab)))
			(setq tabString (append tabString vec[i]))
			) ;; end loop
		tabString)
	;; Take an Lambda out of the File Cabinet.
	(defun takeout(...) 
		vars:(result i LambdaName cabinetName cabinetIndex)
		(_initialize)
		(if (= (argCount) 1) (setq LambdaName (string (argFetch 0))))
		(if (= (argCount) 2) (begin (setq cabinetName (string (argFetch 0))) (setq LambdaName (string (argFetch 1)))))
		(if (<> cabinetName #void) (setq cabinetIndex (member cabinetName _ais.browseLibExtents.Extents)))
		(if (isNumber cabinetIndex) (return (_ais.browseLibExtents.Extents[cabinetIndex].refLambdaByKey LambdaName)))
		;; Perform general search of all file cabinet extents.
		(setq result (_ais.browseLibExtents.Focus.refLambdaByKey LambdaName))
		(if (<> result #void) (return result))
		(loop for i from 0 until (length _ais.browseLibExtents.Extents) do
			;; Search only those extents which have their auto-search switch turned on.
			(if (= _ais.browseLibExtents.Extents[i 1].Pv.mySearchSW true)
				(setq result (_ais.browseLibExtents.Extents[i 1].refLambdaByKey LambdaName))
				) ; end if
			(if (<> result #void) (return result))
			) ; end loop
		result) ;; end of takeout
	;; Perform a complete write of the specified source file.
	(defun writeSourceFile(name data) 
		vars:(fileID self (aType 0))
		(setq fileID (fileOpen name 1 aType))
		(setq self (fileWrite fileID data))
		(fileClose fileID 1)
		self)
	;; *******************************************************************
	;; Main logic of browseLib
	;; *******************************************************************
	(onError errorHandler)
	regs:(a A)
	vars:(aArgs)
	(setq A (argCount))
	(setq aArgs (new Vector: A))
	(loop for a from 0 until A do
		(setq aArgs[a] (argFetch a))
	)
	(apply addExtent aArgs)
	
	(compileAll true)) ;; end of browseLib





;;**EXPORTKEY**:browseLib.%MISCELLANEOUS
;; *******************************************************************
;;  summary:  This browseLib file loads miscellaneous utility Lambdas
;;            useful in many projects. 
;;
;;            These utility Lambdas are used so often, that they are
;;            included in the main AIS online documentation.
;;
;;  Lambdas:   loadWorkspace
;;            saveWorkspace
;;            random
;;            srandom
;; *******************************************************************

;; *******************************************************************
;; Loads a previously saved workspace (see AIS online documentation)
;; *******************************************************************
(defun loadWorkspace (filename) 
	vars:(fileID)  
	(if (isNumber filename) 
		(setq fileID filename) 
		(setq fileID (fileOpen filename 0 3))
		) ; end if 
	(if (<> 0 fileID) 
		(begin 
		(setCdr _currentViews true) 
		(setq _saveTypes true) 
		(setq _currentViews (loadObject fileID)) 
		(setq _saveTypes false) 
		(setCdr _currentViews #void) 
		(fileClose fileID 1))
		) ; end if
	) ; end loadWorkspace

;; *******************************************************************
;; Saves the current workspace (see AIS online documentation)
;; *******************************************************************
(defun saveWorkspace(filename) 
	vars:(fileID) 
	(if (isNumber filename) 
		(setq fileID filename) 
		(setq fileID (fileOpen filename 1 3))
		) ; end if 
	(if (<> 0 fileID) 
		(begin 
		(setCdr _currentViews (getSymbolTable 0 1 1)) 
		(setq _saveTypes true) 
		(saveObject fileID _currentViews) 
		(setq _saveTypes false) 
		(setCdr _currentViews #void) 
		(fileClose fileID 1))
		) ; end if
	) ; end saveWorkspace


;; *******************************************************************
;; Generates a random number (see AIS online documentation)
;; *******************************************************************
(defun random(n) 
	pvars:((seed 0.0) (prime 2796203.0) (primeNew 2147483647.0) (adjust 0.610699392797)) 
	(defun test(m) 
	vars:(rtop rbottom i rmean rsq rn) 
	(loop for i from 0 until m do 
		(setq rn (random 1.0))
		(+= rmean rn) 
		(+= rsq (* rn rn)) 
		(if (< rn .5) (++ rbottom) (++ rtop)) 
		) ; end loop
	(writeln _eol {Avg = } (/ rmean m)  
					{,Std = } (sqrt (- (/ rsq m) (* (/ rmean m) (/ rmean m)))) 
					{,Ratio = } (/ rtop rbottom) ))
	;; Main logic 
	(if (<= seed 0.0000000001) 
		(setq seed (fraction (* (now) random.prime))) 
		(setq seed (abs (fraction (* (- seed (/ (+ adjust seed) primeNew)) prime)))) 
		) ; end if
	(* (fraction seed) n)) ;; end of random 


;; *******************************************************************
;; Generates a pseudo random number (see AIS online documentation)
;; *******************************************************************
(defun srandom(n) 
	pvars:((seed 0.0) (prime 2796203.0) (primeNew 2147483647.0) (adjust 0.610699392797)) 
	(defun test(m) 
	vars:(rtop rbottom stop sbottom i rmean smean rsq ssq rn sn) 
	(loop for i from 0 until m do 
		(setq sn (srandom 1)) 
		(setq rn (random 1)) 
		(+= rmean rn) 
		(+= smean sn) 
		(+= rsq (* rn rn)) 
		(+= ssq (* sn sn)) 
		(if (< rn .5) (++ rbottom) (++ rtop)) 
		(if (< sn .5) (++ sbottom) (++ stop)) 
		) ; end loop
	(writeln _eol {Avg = } (/ rmean m) {[} (/ rmean m) {]} 
					{,Std = } (sqrt (- (/ rsq m) (* (/ rmean m) (/ rmean m)))) {[} (sqrt (- (/ ssq m) (* (/ smean m) (/ smean m)))) {]} 
					{,Ratio = } (/ rtop rbottom) {[} (/ stop sbottom) {]} ))
	;; Main logic 
	(setq seed (abs (fraction (* (- seed (/ (+ adjust seed) primeNew)) prime)))) 
	(* (fraction seed) n)) ;; end of srandom 






























;;**EXPORTKEY**:browseLib._dataManagerTemplate
(deforphan browseLib:_dataManagerTemplate(extentName extentRepository)  
;; *******************************************************************
;; summary:  Factory for creating standard data manager Lambdas.
;;           Manages all Lambda source/binary code stored in each file
;;           cabinet extent. Includes checking Lambda source/binary 
;;           in and out of the file cabinet, importing, exporting, 
;;           and deleting source from the file cabinet.
;;
;; Args:     extentName:          Name of the new file cabinet repository extent.
;;           extentRepository:    Path and file name of new file cabinet repository extent.
;;
;; Globals set by browseLib:
;;           _ais.browseLibExtents  The current browseLib extent structure.
;; Return:   true
;; *******************************************************************
	pvars:(;; Public Child Lambdas
			abortFocus                 ;; Remove the specified extent from focus 
			abortTransaction           ;; Abort a transaction on the extent repository
			beginTransaction           ;; Begin a transaction on the extent repository
			clearRepository            ;; Clear the extent repository
			commitTransaction          ;; Commit a transaction on the extent repository
			compileAll				   ;; Compile all lambdas in cabinet
			compileByIndex             ;; Compile the specified Lambda source using an index
			compileByKey               ;; Compile the specified Lambda source using a key
			deleteLambdaByIndex         ;; Delete an Lambda from the extent using an index
			deleteLambdaByKey           ;; Delete an Lambda from the extent using a key
			deleteBinaryByIndex        ;; Delete an Lambda binary value using an index
			deleteBinaryByKey          ;; Delete an Lambda binary value using a key
			deleteSourceByIndex        ;; Delete an Lambda source value using an index
			deleteSourceByKey          ;; Delete an Lambda source value using a key
			getNextLevel               ;; Return an XPath directory string list for the next repository directory level.
			getTypes                   ;; Return an XPath directory type list string for all possible types in a hierarchical descendents list
			inspectRepository          ;; Perform a com plete inspection of my extent repository.
			memberIndex                ;; Return the index of a key in an extent
			refLambdaByIndex            ;; Retrieve an Lambda from an extent using an index
			refLambdaByKey              ;; Retrieve an Lambda from an extent using a key
			refBinaryByIndex           ;; Retrieve an Lambda binary value using an index
			refBinaryByKey             ;; Retrieve an Lambda binary value using a key
			refKeyByIndex              ;; Retrieve an extent key using an index
			refLength                  ;; Retrieve the number of items in an extent
			refSourceByIndex           ;; Retrieve an Lambda source value using an index
			refSourceByKey             ;; Retrieve an extent source value using a key
			refSourceExByIndex         ;; Retrieve an Lambda source (for export) using an index
			refSourceExByKey           ;; Retrieve an extent source (for export) using a key
			setLambdaByKey              ;; Store an extent value using a key
			setBinaryByKey             ;; Store an extent Lambda binary value using a key
			setFocus                   ;; Set the specified extent in focus 
			setSourceByKey             ;; Store an extent Lambda source value using a key
			setSourceExByKey           ;; Store an extent Lambda source (from export) using a key
			;; Private Variables
			myDependencies             ;; The cabinet names upon which I depend (I may need recompilation if any of these cabinets change). 
			myDependents               ;; The cabinet names who depend upon me (Each of these cabinets may need recompilation if I change). 
			myRepository               ;; The object repository for this extent 
			myExtentName               ;; The name for this extent
			myLambdaName                ;; The name for the current Lambda being managed
			;; Private Child Lambdas
			_lambdaTemplate		       ;; Orphan Lambda template for use in storing training memory in dataOnly file cabinet Lambdas (does not share pvars or cvars).
			_prettyPrintMemory         ;; Create a pretty print source string of myTrainingMemory for dataOnly file cabinet Lambdas.
			) ; end persistant variables
	;;****************************************************************
	;; Define Public Child Lambdas
	;;****************************************************************
	;; Remove the specified extent from focus 
	(defun abortFocus() true)
	;; Abort a transaction on the extent repository 
	(defun abortTransaction() (^abortTransaction myRepository))
	;; Begin a transaction on the extent repository 
	(defun beginTransaction() (^beginTransaction myRepository))
	;; Clear the extent repository
	(defun clearRepository() (^abortTransaction myRepository) (^clear myRepository))
	;; Commit a transaction on the extent repository 
	(defun commitTransaction() (^commitTransaction myRepository))
	;;Compile contents of cabinet and all dependents of cabinet
	(defun compileAll()
		regs:(n N)
		vars:(
		aExtent
		(aErrSwitch false)
		aExtents
		aMyself
		)
		(setq aMyself (myself))
			
		(beginTransaction)
		(setq N (refLength))
		(loop for n from 0 until N do
			(if (not (compileByIndex n true))
				(setq aErrSwitch true))
		);n
		(commitTransaction)

		;; Compile all lambdas on which the current cabinet depends
		(if (and (isMember myDependents aMyself.Pv) (isStructure myDependents)) (begin
			(setq aExtents _ais.browseLibExtents.Extents)
			(setq N (length myDependents))
			(loop for n from 0 until N do
				(if (isMember myDependents[n 0] aExtents)
					(aExtents[myDependents[n 0]].compileAll))
			);n
			))

		(if aErrSwitch 
			(return (append "!browseLib: Errors occured during compilation of " myExtentName )))

		true)
	;; Compile the specified Lambda source using an index 
	(defun compileByIndex(index alwaysCompile) (compileByKey myRepository[index 0] alwaysCompile))
	;; Compile the specified Lambda source using a key 
	(defun compileByKey(key alwaysCompile)
		vars:(theSource theLambda anItem key n N childLambda childName)
		;; Capture and return any compile errors as strings
		(defun _compileItErr(err)
			vars:(msg) 
			(setq msg (append "browseLib: Compiling " myExtentName "[" myLambdaName "] returned this error: " err))
			(writeln msg) 
			msg)
		;; Prepare to compile the named Lambda and then evaluate the result.
		;; Note: the Lambda may already be compiled, if so, it will only be
		;;       recompiled if the always compile switch is true.
		(onError _compileItErr)
		(setq myLambdaName key)
		(setq anItem myRepository[key])
		;; If the Lambda is already compiled, do not compile it again.
;		(if (isLambda anItem)
;			(if alwaysCompile
;				(setq anItem anItem.Sc)
;				(setq theLambda anItem)
;				) ; end if
;			) ; end if
		(if (isLambda anItem)
			(setq anItem anItem.Sc))

		;; If the item is not already compiled, then compile it, and
		;; save then compiled Lambda back into the file cabinet so
		;; that it will not be compiled again.
		(if (isString anItem)
			(begin
				(setq theSource anItem)
				;; Do not compile any script with a text directive.
				(if (= (left theSource 7) ";#text#") (return true))
				;; Compile any script without a text directive.
				(setq theLambda (|Gv:compile| (lisp (_ais.browseLibExtents.Precompiler theSource))))
				(setq theLambda.Sc theSource)
				(setq myRepository[key] theLambda)
			)) ; end if
		;; Evaluate the compiled result so that any defines will
		;; be assigned to the global variable names specified in
		;; the source definition of the Lambda 
		(if (isLambda theLambda) (begin
			(setq theLambda (eval theLambda))
			(if (= (type theLambda) Error:)
				(writeln (append "eval returned an Error compiling " key)))
			))

		;; Set the Interfaces bindings of the Lambda and all its
		;; child Lambdas to match the names taken from the file cabinet.
		(if (isLambda theLambda)
			(begin 
				(if (not (isStructure theLambda.In)) (setq theLambda.In (new Structure:)))
				(setq theLambda.In.Binding myLambdaName)
				(loop for n from 0 until (length theLambda.Pv) do
					(setq childLambda theLambda.Pv[n 1])
					(setq childName theLambda.Pv[n 0])
					(if (and (isLambda childLambda) (not (isStructure childLambda.In)))
						(setq childLambda.In (new Structure: Binding: (append myLambdaName ":" childName)))
						) ; end if 
					) ; end Pv loop
				(loop for n from 0 until (length theLambda.Cv) do
					(setq childLambda theLambda.Cv[n 1])
					(setq childName theLambda.Cv[n 0])
					(if (and (isLambda childLambda) (not (isStructure childLambda.In)))
						(setq childLambda.In (new Structure: Binding: (append myLambdaName ":" childName)))
						) ; end if 
					) ; end Cv loop
			)) ; end if
		true) ; end compileByKey
	;; Delete an Lambda from the extent using an index 
	(defun deleteLambdaByIndex(index) (deleteLambdaByKey myRepository[index 0]))
	;; Delete an Lambda from the extent using a key 
	(defun deleteLambdaByKey(key) (setq myRepository[key] #void))
	;; Delete an Lambda binary value using an index 
	(defun deleteBinaryByIndex(index) (deleteBinaryByKey myRepository[index 0]))
	;; Delete an Lambda binary value using a key 
	(defun deleteBinaryByKey(key)
		vars:(anItem) 
		(setq anItem myRepository[key])
		;; Save only the source from any binary Lambda found
		(if (isLambda anItem) (setq anItem anItem.Sc)) 
		(setq myRepository[key] anItem)) ; end deleteBinaryByKey
	;; Delete an Lambda source value using an index 
	(defun deleteSourceByIndex(index) (deleteSourceByKey myRepository[index 0]))
	;; Delete an Lambda source value using a key 
	(defun deleteSourceByKey(key)
		vars:(anItem) 
		(setq anItem myRepository[key])
		;; Remove the source from any binary Lambda found
		(if (isLambda anItem) (setq anItem.Sc #void) (setq anItem #void)) 
		(setq myRepository[key] anItem)) ; end deleteSourceByKey
	;; Return an XPath directory string list for the next repository directory level.
	(defun getNextLevel(LambdaName startLine lineCount ...)
		vars:(i I s stockName index)
		;; Note1: Each XPath directory line has the following tab delimited fields:
		;;         type    		{Lambda}
		;;         value   		{LambdaName for each node in the repository.}
		;;         size			none
		;;         date			none
		;;         time			none
		;;         version			{current AIS version}
		;;         symbolicKey		{LambdaName for each node in the repository}
		;;         uniqueKey		{LambdaName for each node in the repository}
		;; Note2: We support only one hierarchy level of repository directory.
		;;        Stock History names are the only keys allowed.
		(setq index (inspect myRepository directory:))
		(setq I (length index))
		(setq I (min I (+ startLine lineCount)))
		(setq s (append "" startLine #\tab lineCount #\tab I )) 
		(setq s (append s #\newline "Current" #\tab LambdaName #\tab I #\tab "" #\tab "" #\tab (isVersion) #\tab LambdaName #\tab LambdaName)) 
		(loop for i from 0 until I do
			(setq s (append s #\newline "Lambda" #\tab (setq stockName index[i 0]) #\tab "" #\tab "" #\tab "" #\tab (isVersion) #\tab stockName #\tab stockName))
			) ;; end of loop
		s)
	;; Return an XPath directory type list string for all possible types in a hierarchical descendents list 
	(defun getTypes()
		vars:(result)
		(setq result (append ".default." #\tab "edit" #\newline
								"Lambda" #\tab "edit,erase,export,import,compile,checkin" #\newline
						))
		result)
	;; Perform a complete inspection of my extent repository.
	(defun inspectRepository() 
		vars:(i ss)
		(inspect myRepository check:)
		(loop for i from 0 until (length myRepository) do
			(inspect myRepository check: myRepository[i 0])
		) ; end repo if
		true) ; end inspectRepository
	;; Return the index of a key in an extent 
	(defun memberIndex(key) (member key myRepository))
	;; Retrieve an extent value using an index 
	(defun refLambdaByIndex(index) myRepository[index 1])
	;; Retrieve an extent value using a key 
	(defun refLambdaByKey(key) myRepository[key])
	;; Retrieve an Lambda binary value using an index 
	(defun refBinaryByIndex(index) (refBinaryByKey myRepository[index 0])) 
	;; Retrieve an Lambda binary value using a key 
	(defun refBinaryByKey(key)
		vars:(result)       
		(setq result myRepository[key])
		(if (not (isLambda result)) (setq result #void))
		result) ; end refBinaryByKey
	;; Retrieve an extent key using an index 
	(defun refKeyByIndex(index) myRepository[index 0])
	;; Retrieve the number of items in an extent 
	(defun refLength() (length myRepository))
	;; Retrieve an extent scource value using an index 
	(defun refSourceByIndex(index) (refSourceByKey myRepository[index 0])) 
	;; Retrieve an Lambda source value using a key 
	(defun refSourceByKey(key)
		vars:(result)       
		(setq result myRepository[key])
		(if (isLambda result) 
			(begin
				(if (and (= result.Sc #void) (= result.In.dataOnly true)) 
					(setq result (_prettyPrintMemory #void result.Pv.myTrainingMemory 12))
					(setq result result.Sc)
					) ; end dataOnly if 
			)) ; end isLambda if
		result) ; end refSourceByKey
	;; Retrieve an extent scource (for export) using an index 
	(defun refSourceExByIndex(index) (refSourceExByKey myRepository[index 0])) 
	;; Retrieve an Lambda source (for export) using a key 
	(defun refSourceExByKey(key) (refSourceByKey key))
	;; Store an extent value using a key 
	(defun setLambdaByKey(key newLambda) (setq myRepository[key] newLambda))
	;; Store an extent Lambda binary value using a key 
	(defun setBinaryByKey(key newBinary) (setq myRepository[key] newBinary))
	;; Set the specified extent in focus 
	(defun setFocus() true)
	;; Store an extent Lambda source value using a key 
	(defun setSourceByKey(key newSource) (setq myRepository[key] newSource))
	;; Store an extent Lambda source (from export) using a key 
	(defun setSourceExByKey(key newSource) (setq myRepository[key] newSource))
	;;****************************************************************
	;; Define Private Child Lambdas
	;;****************************************************************
	;; Create a pretty print source string of myTrainingMemory for dataOnly file cabinet Lambdas.
	(defun _prettyPrintMemory(buffer memory margin)
		vars:(n N key newMargin sourceTemplate
			(blanks "                                                                                                                                                              ")
			(frontMargin "  ")
			(backMargin "          ")
			(lastMargin "    ")
			(tailSW false)
			) ; end temporary variables
		;; Initialize the display buffer (if necessary).
		(if (= buffer #void)
			(begin 
				(setq buffer (new Vector: byte: 20000000))
				(appendWriteln buffer "(lambda()" _eol)
				(setq sourceTemplate (browseLib.checkout BrowseLib: "browseLib:_dataManagerTemplate:%MEMORY_HDR"))
				(appendWriteln buffer sourceTemplate _eol frontMargin)
				(appendWriteln buffer ";; The current historical and training memory for this dataOnly file cabinet Lambda." _eol frontMargin)
				(appendWriteln buffer "faces:((dataOnly true))" _eol frontMargin)
				(appendWriteln buffer "pvars:((myTrainingMemory" _eol _eol (left blanks margin))
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
				(appendWriteln buffer _eol _eol backMargin)
				(appendWriteln buffer ")) ; end myTrainingMemory" _eol lastMargin)
				(appendWriteln buffer "(myself)) ; end lambda" _eol)
			)) ; end if
		buffer) ; end _prettyPrintMemory
	;;****************************************************************
	;; MAIN initialization section
	;;****************************************************************
	(setq myExtentName extentName)
	(setq myRepository extentRepository)
	myRepository) ; end _dataManagerTemplate



































;;**EXPORTKEY**:browseLib._dataManagerTemplate.%MEMORY_HDR
;; *******************************************************************
;;  summary:  This browseLib my Training Memory Template serves
;;            as a storage module for the training history of this 
;;            dataOnly file cabinet Lambda.
;;
;;            All dataOnly file cabinet Lambdas contain a pvars with
;;            only one variable: "myTrainingMemory", and a faces with
;;            the variable "dataOnly" equal to true.
;;            
;;            Only historical training data, of the format manageable
;;            by the _prettyPrintMemory function may be place the dataOnly
;;            file cabinet Lambda's "myTrainingMemory" persistant variable.
;;            Currently the _prettyPrintMemory function expects the
;;            myTrainingMemory variable to be a Structure, whose elements
;;            contain only Vectors, Dictionaries, Directories, and native
;;            types.
;;            
;;            The dataOnly Lambda may be checked into the file cabinet
;;            without attached source in its Sc attribute. Upon checkout,
;;            the browseLib data extent manager will create source
;;            for the dataOnly Lambda, using the _prettyPrintMemory
;;            function and this blank Lambda template. 
;;
;;  Args:     none
;;  Return:   myself
;; *******************************************************************



































;;**EXPORTKEY**:browseLib._extentManagerTemplate
(deforphan browseLib:_extentManagerTemplate(extentName extentRepository)  
;; *******************************************************************
;; summary:  Factory for creating standard extent manager Lambdas.
;;           Manages all Lambda source/binary code stored in each file
;;           cabinet extent. Includes checking Lambda source/binary 
;;           in and out of the file cabinet, importing, exporting, 
;;           and deleting source from the file cabinet.
;;
;; Args:     extentName:          Name of the new file cabinet repository extent.
;;           extentRepository:    Path and file name of new file cabinet repository extent.
;;
;; Globals set by browseLib:
;;           _ais.browseLibExtents  The current browseLib extent structure.
;; Return:   true
;; *******************************************************************
	pvars:(;; Public Child Lambdas
			abortFocus                 ;; Remove the specified extent from focus 
			abortTransaction           ;; Abort a transaction on the extent repository
			beginTransaction           ;; Begin a transaction on the extent repository
			clearRepository            ;; Clear the extent repository
			compileAll				   ;; Compile all lambdas and dependents in repository
			commitTransaction          ;; Commit a transaction on the extent repository
			compileByIndex             ;; Compile the specified Lambda source using an index
			compileByKey               ;; Compile the specified Lambda source using a key
			deleteLambdaByIndex         ;; Delete an Lambda from the extent using an index
			deleteLambdaByKey           ;; Delete an Lambda from the extent using a key
			deleteBinaryByIndex        ;; Delete an Lambda binary value using an index
			deleteBinaryByKey          ;; Delete an Lambda binary value using a key
			deleteSourceByIndex        ;; Delete an Lambda source value using an index
			deleteSourceByKey          ;; Delete an Lambda source value using a key
			getNextLevel               ;; Return an XPath directory string list for the next repository directory level.
			getTypes                   ;; Return an XPath directory type list string for all possible types in a hierarchical descendents list
			inspectRepository          ;; Perform a com plete inspection of my extent repository.
			memberIndex                ;; Return the index of a key in an extent
			refLambdaByIndex            ;; Retrieve an Lambda from an extent using an index
			refLambdaByKey              ;; Retrieve an Lambda from an extent using a key
			refBinaryByIndex           ;; Retrieve an Lambda binary value using an index
			refBinaryByKey             ;; Retrieve an Lambda binary value using a key
			refKeyByIndex              ;; Retrieve an extent key using an index
			refLength                  ;; Retrieve the number of items in an extent
			refSourceByIndex           ;; Retrieve an Lambda source value using an index
			refSourceByKey             ;; Retrieve an extent source value using a key
			refSourceExByIndex         ;; Retrieve an Lambda source (for export) using an index
			refSourceExByKey           ;; Retrieve an extent source (for export) using a key
			setLambdaByKey              ;; Store an extent value using a key
			setBinaryByKey             ;; Store an extent Lambda binary value using a key
			setFocus                   ;; Set the specified extent in focus 
			setSourceByKey             ;; Store an extent Lambda source value using a key
			setSourceExByKey           ;; Store an extent Lambda source (from export) using a key
			;; Public Variables
			myDependencies             ;; The cabinet names upon which I depend (I may need recompilation if any of these cabinets change). 
			myDependents               ;; The cabinet names who depend upon me (Each of these cabinets may need recompilation if I change). 
			mySearchSW                 ;; The cabinet search switch (true iff cabinet is to be auto-searched). 
			myAutoCompile			   ;; The cabinet autoCompile switch governs non-manual (automatic) compilation
			myCompileRequired		   ;; Flag indicating the cabinet contents should be compiled. Usually set after an initial import
			;; Private Variables
			myRepository               ;; The object repository for this extent 
			myExtentName               ;; The name for this extent
			myLambdaName                ;; The name for the current Lambda being managed
			) ; end persistant variables

	;;****************************************************************
	;; Define Public Child Lambdas
	;;****************************************************************
	;; Remove the specified extent from focus 
	(defun abortFocus() true)
	;; Abort a transaction on the extent repository 
	(defun abortTransaction() (^abortTransaction myRepository))
	;; Begin a transaction on the extent repository 
	(defun beginTransaction() (^beginTransaction myRepository))
	;; Clear the extent repository
	(defun clearRepository()
		vars:(aExtentInfo)
		(setq aExtentInfo (loadMetaData myRepository))
		(^abortTransaction myRepository) (^clear myRepository)
		(saveMetaData myRepository aExtentInfo)
	)

	;;Compile contents of cabinet and all dependent cabinets
	(defun compileAll()
		regs:(n N)
		vars:(
		aExtent
		(aErrSwitch false)
		aExtents
		aMyself
		aResult
		)
		(setq aMyself (myself))

		(beginTransaction)
		(setq N (refLength))
		(loop for n from 0 until N do
			(setq aResult (compileByIndex n true))
			(if (not aResult)
				(setq aErrSwitch true))
		);n
		(commitTransaction)

		;; Compile all lambdas on which the current cabinet depends
		(if (and (isMember myDependents: aMyself.Pv) (isStructure myDependents)) (begin
			(setq aExtents _ais.browseLibExtents.Extents)
			(setq N (length myDependents))
			(loop for n from 0 until N do
				(if (isMember myDependents[n 0] aExtents) (begin
					(if browseLib.myVerboseCompile
						(writeln "    Compiling Dependency " myDependents[n 0]))
					(aExtents[myDependents[n 0]].compileAll)
				))
			);n
			))

		(if aErrSwitch 
			(return (append "!browseLib: Errors occured during compilation of " myExtentName )))
		true)
	;; Commit a transaction on the extent repository 
	(defun commitTransaction() (^commitTransaction myRepository))
	;; Compile the specified Lambda source using an index 
	;;TM: alwaysCompile is now ignored! Because of the addition of 
	;;defClone, defClass etc. The source must always be compiled to restablish
	;;the environment. As a future project we will remove this vestigle parameter.
	(defun compileByIndex(index alwaysCompile) (compileByKey myRepository[index 0] alwaysCompile))
	;; Compile the specified Lambda source using a key 
	;;TM: alwaysCompile is now ignored! Because of the addition of 
	;;defClone, defClass etc. The source must always be compiled to restablish
	;;the environment. As a future project we will remove this vestigle parameter.
	(defun compileByKey(key alwaysCompile)
		vars:(theSource theLambda anItem n N childLambda childName)
		;; Capture and return any compile errors as strings
		(defun _compileItErr(err)
			vars:(msg) 
			(setq msg (append "browseLib: Compiling " myExtentName "[" myLambdaName "] returned this error: " err))
			(writeln msg) 
			msg)

		;; Prepare to compile the named Lambda and then evaluate the result.
		;; Note: the Lambda may already be compiled, if so, it will only be
		;;       recompiled if the always compile switch is true.
		(onError _compileItErr)
		(setq myLambdaName key)
		(setq anItem myRepository[key])
		;; If the Lambda is already compiled, do not compile it again.
;		(if (isLambda anItem)
;			(if alwaysCompile
;				(setq anItem anItem.Sc)
;				(setq theLambda anItem)
;				) ; end if
;			) ; end if

		(if (isLambda anItem)
			(setq anItem anItem.Sc))

		;; If the item is not already compiled, then compile it, and
		;; save then compiled Lambda back into the file cabinet so
		;; that it will not be compiled again.
		(if (isString anItem)
			(begin
				(setq theSource anItem)
				;; Do not compile any script with a text directive.
				(if (= (left theSource 7) ";#text#") (return true))
				;; Compile any script without a text directive.
				(setq theLambda (|Gv:compile| (lisp (_ais.browseLibExtents.Precompiler theSource))))
				(setq theLambda.Sc theSource)
				(setq myRepository[key] theLambda)
			)) ; end if

		;; Evaluate the compiled result so that any defines will
		;; be assigned to the global variable names specified in
		;; the source definition of the Lambda 
		(if (isLambda theLambda) (begin
			(setq theLambda (eval theLambda))
			(if (= (type theLambda) Error:)
				(writeln (append "eval returned an Error compiling " key)))
			))
		;; Set the Interfaces bindings of the Lambda and all its
		;; child Lambdas to match the names taken from the file cabinet.
		(if (isLambda theLambda)
			(begin 
				(if (not (isStructure theLambda.In)) (setq theLambda.In (new Structure:)))
				(setq theLambda.In.Binding myLambdaName)
				(loop for n from 0 until (length theLambda.Pv) do
					(setq childLambda theLambda.Pv[n 1])
					(setq childName theLambda.Pv[n 0])
					(if (and (isLambda childLambda) (not (isStructure childLambda.In)))
						(setq childLambda.In (new Structure: Binding: (append myLambdaName ":" childName)))
						) ; end if 
					) ; end Pv loop
				(loop for n from 0 until (length theLambda.Cv) do
					(setq childLambda theLambda.Cv[n 1])
					(setq childName theLambda.Cv[n 0])
					(if (and (isLambda childLambda) (not (isStructure childLambda.In)))
						(setq childLambda.In (new Structure: Binding: (append myLambdaName ":" childName)))
						) ; end if 
					) ; end Cv loop
			)) ; end if
		true) ; end compileByIndex
	;; Delete an Lambda from the extent using an index 
	(defun deleteLambdaByIndex(index) (deleteLambdaByKey myRepository[index 0]))
	;; Delete an Lambda from the extent using a key 
	(defun deleteLambdaByKey(key) (setq myRepository[key] #void))
	;; Delete an Lambda binary value using an index 
	(defun deleteBinaryByIndex(index) (deleteBinaryByKey myRepository[index 0]))
	;; Delete an Lambda binary value using a key 
	(defun deleteBinaryByKey(key)
		vars:(anItem) 
		(setq anItem myRepository[key])
		;; Save only the source from any binary Lambda found
		(if (isLambda anItem) (setq anItem anItem.Sc)) 
		(setq myRepository[key] anItem)) ; end deleteBinaryByKey
	;; Delete an Lambda source value using an index 
	(defun deleteSourceByIndex(index) (deleteSourceByKey myRepository[index 0]))
	;; Delete an Lambda source value using a key 
	(defun deleteSourceByKey(key)
		vars:(anItem) 
		(setq anItem myRepository[key])
		(cond 
			;; Remove the source from any Lambda found
			;; Note: Do NOT remove any source which begins with the ";#text#" directive!
			((isLambda anItem)
			(begin        
			(if (and (isString anItem.Sc) (<> (left anItem.Sc 7) ";#text#"))
				(begin 
					(setq anItem.Sc #void)
					(setq myRepository[key] anItem)
				)) ; end if
			)) ; end case Lambda
			;; Remove the source from any text found
			;; Note: Do NOT remove any source which begins with the ";#text#" directive!
			((isString anItem)
			(begin        
			(if (<> (left anItem 7) ";#text#")
				(begin 
					(setq anItem "")
					(setq myRepository[key] anItem)
				)) ; end if
			)) ; end case Lambda
		) ; end cond 
		true) ; end deleteSourceByKey
	;; Return an XPath directory string list for the next repository directory level.
	(defun getNextLevel(LambdaName startLine lineCount ...)
		vars:(i I s stockName index)
		;; Note1: Each XPath directory line has the following tab delimited fields:
		;;         type    		{Lambda}
		;;         value   		{LambdaName for each node in the repository.}
		;;         size			none
		;;         date			none
		;;         time			none
		;;         version			{current AIS version}
		;;         symbolicKey		{LambdaName for each node in the repository}
		;;         uniqueKey		{LambdaName for each node in the repository}
		;; Note2: We support only one hierarchy level of repository directory.
		;;        Stock History names are the only keys allowed.
		(setq index (inspect myRepository directory:))
		(setq I (length index))
		(setq I (min I (+ startLine lineCount)))
		(setq s (append "" startLine #\tab lineCount #\tab I )) 
		(setq s (append s #\newline "Current" #\tab LambdaName #\tab I #\tab "" #\tab "" #\tab (isVersion) #\tab LambdaName #\tab LambdaName)) 
		(loop for i from 0 until I do
			(setq s (append s #\newline "Lambda" #\tab (setq stockName index[i 0]) #\tab "" #\tab "" #\tab "" #\tab (isVersion) #\tab stockName #\tab stockName))
			) ;; end of loop
		s)
	;; Return an XPath directory type list string for all possible types in a hierarchical descendents list 
	(defun getTypes()
		vars:(result)
		(setq result (append ".default." #\tab "edit" #\newline
								"Lambda" #\tab "edit,erase,export,import,compile,checkin" #\newline
						))
		result)
	;; Perform a complete inspection of my extent repository.
	(defun inspectRepository() 
		vars:(i ss)
		(inspect myRepository check:)
		(loop for i from 0 until (length myRepository) do
			(inspect myRepository check: myRepository[i 0])
		) ; end repo if
		true) ; end inspectRepository
	;; Return the index of a key in an extent 
	(defun memberIndex(key) (member key myRepository))
	;; Retrieve an extent value using an index 
	(defun refLambdaByIndex(index) myRepository[index 1])
	;; Retrieve an extent value using a key 
	(defun refLambdaByKey(key) myRepository[key])
	;; Retrieve an Lambda binary value using an index 
	(defun refBinaryByIndex(index) (refBinaryByKey myRepository[index 0])) 
	;; Retrieve an Lambda binary value using a key 
	(defun refBinaryByKey(key)
		vars:(result)       
		(setq result myRepository[key])
		(if (not (isLambda result)) (setq result #void))
		result) ; end refBinaryByKey
	;; Retrieve an extent key using an index 
	(defun refKeyByIndex(index) myRepository[index 0])
	;; Retrieve the number of items in an extent 
	(defun refLength() (length myRepository))
	;; Retrieve an extent scource value using an index 
	(defun refSourceByIndex(index) (refSourceByKey myRepository[index 0])) 
	;; Retrieve an Lambda source value using a key 
	(defun refSourceByKey(key)
		vars:(result)       
		(setq result myRepository[key])
		(if (isLambda result) (setq result result.Sc))
		result) ; end refSourceByKey
	;; Retrieve an extent scource (for export) using an index 
	(defun refSourceExByIndex(index) (refSourceExByKey myRepository[index 0])) 
	;; Retrieve an Lambda source (for export) using a key 
	(defun refSourceExByKey(key)
		vars:(result)       
		(setq result myRepository[key])
		(if (isLambda result) (setq result result.Sc))
		result) ; end refSourceExByKey
	;; Store an extent value using a key 
	(defun setLambdaByKey(key newLambda) (setq myRepository[key] newLambda))
	;; Store an extent Lambda binary value using a key 
	(defun setBinaryByKey(key newBinary) (setq myRepository[key] newBinary))
	;; Set the specified extent in focus 
	(defun setFocus() true)
	;; Store an extent Lambda source value using a key 
	(defun setSourceByKey(key newSource) (setq myRepository[key] newSource))
	;; Store an extent Lambda source (from export) using a key 
	(defun setSourceExByKey(key newSource) (setq myRepository[key] newSource))
	;;****************************************************************
	;; MAIN initialization section
	;;****************************************************************
	(setq myExtentName extentName)
	(setq myRepository extentRepository)
	myRepository) ; end _extentManagerTemplate



































;;**EXPORTKEY**:browseLib._hostTextManagerTemplate
(deforphan browseLib:_hostTextManagerTemplate(extentName folderName fileSuffix)  
;; *******************************************************************
;; summary:  Factory for creating standard host text manager Lambdas.
;;           Manages all Host source/text code stored in each host
;;           file folder. Includes checking Lambda source/text 
;;           in and out of the host file folder, importing, exporting, 
;;           and deleting source from the host file folder.
;;
;; Args:     extentName:          The name of the new extent to be managed.
;;           folderName:          Dir path name of the new host file folder.
;;           fileSuffix:          Case insensitive file suffix for new text files.
;;
;; Globals set by browseLib:
;;           _ais.browseLibExtents  The current browseLib extent structure.
;; Return:   true
;; *******************************************************************
	pvars:(;; Public Child Lambdas
			abortFocus                 ;; Remove the specified extent from focus 
			abortTransaction           ;; Abort a transaction on the extent repository
			beginTransaction           ;; Begin a transaction on the extent repository
			clearRepository            ;; Clear the extent repository
			commitTransaction          ;; Commit a transaction on the extent repository
			compileAll				   ;; Compile all Lambda source
			compileByIndex             ;; Compile the specified Lambda source using an index
			compileByKey               ;; Compile the specified Lambda source using a key
			deleteLambdaByIndex         ;; Delete an Lambda from the extent using an index
			deleteLambdaByKey           ;; Delete an Lambda from the extent using a key
			deleteBinaryByIndex        ;; Delete an Lambda binary value using an index
			deleteBinaryByKey          ;; Delete an Lambda binary value using a key
			deleteSourceByIndex        ;; Delete an Lambda source value using an index
			deleteSourceByKey          ;; Delete an Lambda source value using a key
			getNextLevel               ;; Return an XPath directory string list for the next repository directory level.
			getTypes                   ;; Return an XPath directory type list string for all possible types in a hierarchical descendents list
			inspectRepository          ;; Perform a com plete inspection of my extent repository.
			memberIndex                ;; Return the index of a key in an extent
			refLambdaByIndex            ;; Retrieve an Lambda from an extent using an index
			refLambdaByKey              ;; Retrieve an Lambda from an extent using a key
			refBinaryByIndex           ;; Retrieve an Lambda binary value using an index
			refBinaryByKey             ;; Retrieve an Lambda binary value using a key
			refKeyByIndex              ;; Retrieve an extent key using an index
			refLength                  ;; Retrieve the number of items in an extent
			refSourceByIndex           ;; Retrieve an Lambda source value using an index
			refSourceByKey             ;; Retrieve an extent source value using a key
			refSourceExByIndex         ;; Retrieve an Lambda source (for export) using an index
			refSourceExByKey           ;; Retrieve an extent source (for export) using a key
			setLambdaByKey              ;; Store an extent value using a key
			setBinaryByKey             ;; Store an extent Lambda binary value using a key
			setFocus                   ;; Set the specified extent in focus 
			setSourceByKey             ;; Store an extent Lambda source value using a key
			setSourceExByKey           ;; Store an extent Lambda source (from export) using a key
			;; Public Variables
			mySearchSW                 ;; The cabinet search switch (true iff cabinet is to be auto-searched). 
			;; Private Variables
			myLambdaName                ;; The name for the current Lambda being managed
			myExtentName               ;; The name for this extent
			myFileNames                ;; The current file names in the host file folder being managed
			myFolderName               ;; The path name for the current host file folder being managed
			myHostDirObject            ;; The host dir object for this host file folder
			mySuffix                   ;; The case insensitive suffix for any new text files
			;; Private Child Lambdas
			_checkin        	          ;; Write the host text file into the host file folder being managed.
			_checkout        	      ;; Read the host text file from the host file folder being managed.
			_compileFile                ;; Compile the specified host source file
			_getFileNames        	  ;; Fills the myFilesNames vector with the latest list of file names in the host file folder being managed.
			_lambdaTemplate		      ;; Orphan Lambda template for use in storing training memory in dataOnly file cabinet Lambdas (does not share pvars or cvars).
			) ; end persistant variables
	;;****************************************************************
	;; Define Public Child Lambdas
	;;****************************************************************
	;; Remove the specified extent from focus 
	(defun abortFocus() true)
	;; Abort a transaction on the extent repository 
	(defun abortTransaction() true)
	;; Begin a transaction on the extent repository 
	(defun beginTransaction() (_getFileNames))
	;; Clear the extent repository
	(defun clearRepository() (error "hostTextExtentManager: clear not yet implemented"))
	;; Commit a transaction on the extent repository 
	(defun commitTransaction() true)
	;;Compile contents of cabinet and all dependent cabinets
	(defun compileAll()
		regs:(n N)
		vars:(
		aExtent
		(aErrSwitch false)
		aExtents
		aMyself
		)
		(setq aMyself (myself))
			
		(beginTransaction)
		(setq N (refLength))
		(loop for n from 0 until N do
			(if (not (compileByIndex n true))
				(setq aErrSwitch true))
		);n
		(commitTransaction)

		;; Compile all lambdas on which the current cabinet depends
		(if (and (isMember myDependents aMyself.Pv) (isStructure myDependents)) (begin
			(setq aExtents _ais.browseLibExtents.Extents)
			(setq N (length myDependents))
			(loop for n from 0 until N do
				(if (isMember myDependents[n 0] aExtents)
					(aExtents[myDependents[n 0]].compileAll))
			);n
			))

		(if aErrSwitch 
			(return (append "!browseLib: Errors occured during compilation of " myExtentName )))

		true)
	;; Compile the specified Lambda source using an index 
	(defun compileByIndex(index alwaysCompile) (compileByKey myFileNames[index] alwaysCompile))
	;; Compile the specified Lambda source using a key 
	(defun compileByKey(key alwaysCompile)
		vars:(theSource theLambda anItem n N childLambda childName src)
		;; Capture and return any compile errors as strings
		(defun _compileItErr(err)
			vars:(msg) 
			(setq msg (append "browseLib: Compiling " myExtentName "[" myLambdaName "] returned this error: " err))
			(writeln msg) 
			msg)
		;; Prepare to compile the named Lambda and then evaluate the result.
		(onError _compileItErr)
		(setq myLambdaName key)
		(setq theSource (_checkout key))
		;; Do not compile any script with a text directive.
		(if (= (left theSource 7) ";#text#") (return true))
		;; Compile any script without a text directive.
		(setq theLambda (|Gv:compile| (lisp (_ais.browseLibExtents.Precompiler theSource))))
		(setq theLambda (eval theLambda))
		;; Evaluate the compiled result so that any defines will
		;; be assigned to the global variable names specified in
		;; the source definition of the Lambda 
		(if (isLambda theLambda) (begin
			(setq theLambda (eval theLambda))
			(if (= (type theLambda) Error:)
				(writeln (append "eval returned an Error compiling " key)))
			))

		;; Set the Interfaces bindings of the Lambda and all its
		;; child Lambdas to match the names taken from the file cabinet.
		(if (isLambda theLambda)
			(begin 
				(if (not (isStructure theLambda.In)) (setq theLambda.In (new Structure:)))
				(setq theLambda.In.Binding myLambdaName)
				(loop for n from 0 until (length theLambda.Pv) do
					(setq childLambda theLambda.Pv[n 1])
					(setq childName theLambda.Pv[n 0])
					(if (and (isLambda childLambda) (not (isStructure childLambda.In)))
						(setq childLambda.In (new Structure: Binding: (append myLambdaName ":" childName)))
						) ; end if 
					) ; end Pv loop
				(loop for n from 0 until (length theLambda.Cv) do
					(setq childLambda theLambda.Cv[n 1])
					(setq childName theLambda.Cv[n 0])
					(if (and (isLambda childLambda) (not (isStructure childLambda.In)))
						(setq childLambda.In (new Structure: Binding: (append myLambdaName ":" childName)))
						) ; end if 
					) ; end Cv loop
			)) ; end if
		true) ; end compileByIndex
	;; Delete an Lambda from the extent using an index 
	(defun deleteLambdaByIndex(index) (deleteLambdaByKey myFileNames[index]))
	;; Delete an Lambda from the extent using a key 
	(defun deleteLambdaByKey(key) (myHostDirObject.remove (append key "." mySuffix)))
	;; Delete an Lambda binary value using an index 
	(defun deleteBinaryByIndex(index) (deleteBinaryByKey myFileNames[index]))
	;; Delete an Lambda binary value using a key 
	(defun deleteBinaryByKey(key) (myHostDirObject.remove key))
	;; Delete an Lambda source value using an index 
	(defun deleteSourceByIndex(index) (deleteSourceByKey myFileNames[index]))
	;; Delete an Lambda source value using a key 
	(defun deleteSourceByKey(key) (myHostDirObject.remove key))
	;; Return an XPath directory string list for the next repository directory level.
	(defun getNextLevel(LambdaName startLine lineCount ...)
		vars:(i I s stockName index)
		;; Note1: Each XPath directory line has the following tab delimited fields:
		;;         type    		{Lambda}
		;;         value   		{LambdaName for each node in the repository.}
		;;         size			none
		;;         date			none
		;;         time			none
		;;         version			{current AIS version}
		;;         symbolicKey		{LambdaName for each node in the repository}
		;;         uniqueKey		{LambdaName for each node in the repository}
		;; Note2: We support only one hierarchy level of repository directory.
		;;        Stock History names are the only keys allowed.
		(setq index (_getFileNames))
		(setq I (length index))
		(setq I (min I (+ startLine lineCount)))
		(setq s (append "" startLine #\tab lineCount #\tab I ))
		(setq s (append s #\newline "Current" #\tab LambdaName #\tab I #\tab "" #\tab "" #\tab (version) #\tab LambdaName #\tab LambdaName))
		(loop for i from 0 until I do
			(setq s (append s #\newline "Lambda" #\tab (setq stockName index[i]) #\tab "" #\tab "" #\tab "" #\tab (isVersion) #\tab stockName #\tab stockName))
			) ;; end of loop
		s)
	;; Return an XPath directory type list string for all possible types in a hierarchical descendents list 
	(defun getTypes()
		vars:(result)
		(setq result (append ".default." #\tab "edit" #\newline
								"Lambda" #\tab "edit,erase,compile,checkin" #\newline
						))
		result)
	;; Perform a complete inspection of my extent repository.
	(defun inspectRepository() true)
	;; Return the index of a key in an extent
	(defun memberIndex(key) (member key myFileNames))
	;; Retrieve an extent value using an index
	(defun refLambdaByIndex(index) (refLambdaByKey myFileNames[index]))
	;; Retrieve an extent value using a key
	(defun refLambdaByKey(key) (refBinaryByKey key))
	;; Retrieve an Lambda binary value using an index
	(defun refBinaryByIndex(index) (refBinaryByKey myFileNames[index]))
	;; Retrieve an Lambda binary value using a key
	(defun refBinaryByKey(key)
		vars:(result)
		(setq myLambdaName key)
		(setq result (_checkout key))
		(setq result (_compileFile result))
		(if (not (isLambda result)) (setq result #void))
		result) ; end refBinaryByKey
	;; Retrieve an extent key using an index
	(defun refKeyByIndex(index) myFileNames[index])
	;; Retrieve the number of items in an extent
	(defun refLength() (length myFileNames))
	;; Retrieve an extent scource value using an index
	(defun refSourceByIndex(index) (refSourceByKey myFileNames[index]))
	;; Retrieve an Lambda source value using a key
	(defun refSourceByKey(key) (_checkout key))
	;; Retrieve an extent scource (for export) using an index 
	(defun refSourceExByIndex(index) (refSourceExByKey myFileNames[index])) 
	;; Retrieve an Lambda source (for export) using a key 
	(defun refSourceExByKey(key) (_checkout key))
	;; Store an extent value using a key 
	(defun setLambdaByKey(key newLambda) (_checkin key (if (isLambda newLambda) newLambda.Sc newLambda)))
	;; Store an extent Lambda binary value using a key 
	(defun setBinaryByKey(key newBinary) (setLambdaByKey key newBinary))
	;; Set the specified extent in focus 
	(defun setFocus() true)
	;; Store an extent Lambda source value using a key 
	(defun setSourceByKey(key newSource) (_checkin key newSource))
	;; Store an extent Lambda source (from export) using a key 
	(defun setSourceExByKey(key newSource) (_checkin key newSource))
	;;****************************************************************
	;; Define Private Child Lambdas
	;;****************************************************************
	;; Write the host text file into the host file folder being managed.
	(defun _checkin(fileName fileText)
		(browseLib.writeSourceFile (append myFolderName "/" fileName "." mySuffix) fileText)
		true) ; end _checkin
	;; Write the host text file into the host file folder being managed.
	(defun _checkout(fileName)
		(browseLib.readSourceFile (append myFolderName "/" fileName "." mySuffix))
		) ; end _checkout
	;; Compile the specified host source file
	(defun _compileFile(theSource)
		vars:(theLambda anItem key n N childLambda childName src)
		;; Prepare to compile the named Lambda and then evaluate the result.
		(onError _compileItErr)
		;; Do not compile any script with a text directive.
		(if (= (left theSource 7) ";#text#") (return true))
		;; Compile any script without a text directive.
		(setq theLambda (|Gv:compile| (lisp (_ais.browseLibExtents.Precompiler theSource))))
		(setq theLambda (eval theLambda))
		;; Evaluate the compiled result so that any defines will
		;; be assigned to the global variable names specified in
		;; the source definition of the Lambda 
		(if (isLambda theLambda) (setq theLambda (eval theLambda)))
		theLambda) ; end _compileFile
	;; Fills the myFilesNames vector with the latest list of file names in the host file folder being managed.
	(defun _getFileNames()
		vars:(n N L temp)
		;; Initialize the host dir object (if necessary).
		(if (= myHostDirObject #void)
			(begin 
			(setq myHostDirObject (^new browseLib.dir myFolderName))
			(myHostDirObjectX.setNameFilter (append "*." mySuffix))
			)) ; end if
		;; Get the latest list of file names (matching the suffix) in the host file folder.
		(setq temp (myHostDirObject.entryList))
		;; Drop the suffix names from the list of file names (if necessary).
		(if (and (<> mySuffix #void) (<> mySuffix ""))
			then
			(begin
				(setq myFileNames (^new Vector: Object: (setq N (length temp))))
				(setq L (addi (length mySuffix) 1))
				(loop for n from 0 until N do
				(setq myFileNames[n] (left temp[n] (- (length temp[n]) L)))
				) ; end loop
			)
			else
			(setq myFileNames temp)
			) ; end if
		myFileNames) ; end _getFileNames
	;;****************************************************************
	;; MAIN initialization section
	;;****************************************************************
	(setq myExtentName extentName)
	(setq myFolderName folderName)
	(setq mySuffix (downcase fileSuffix))
	(setq myHostDirObject (^new browseLib.dir myFolderName))
	(myHostDirObject.setNameFilter (append "*." mySuffix))
	(_getFileNames)
	myHostDirObject) ; end _hostTextManagerTemplate































;;**EXPORTKEY**:browseLib._memoryManagerTemplate
(deforphan browseLib:_memoryManagerTemplate(extentName extentRepository)  
;; *******************************************************************
;; summary:  Factory for creating standard memory manager Lambdas.
;;           Manages all Lambda source/binary code stored in each file
;;           cabinet extent. Includes checking Lambda source/binary 
;;           in and out of the file cabinet, importing, exporting, 
;;           and deleting source from the file cabinet.
;;
;; Args:     extentName:          Name of the new file cabinet repository extent.
;;           extentRepository:    Path and file name of new file cabinet repository extent.
;;
;; Globals set by browseLib:
;;           _ais.browseLibExtents  The current browseLib extent structure.
;; Return:   true
;; *******************************************************************
	pvars:(;; Public Child Lambdas
			abortFocus                 ;; Remove the specified extent from focus 
			abortTransaction           ;; Abort a transaction on the extent repository
			beginTransaction           ;; Begin a transaction on the extent repository
			clearRepository            ;; Clear the extent repository
			commitTransaction          ;; Commit a transaction on the extent repository
			compileAll                 ;; Compile All Lambdas
			compileByIndex             ;; Compile the specified Lambda source using an index
			compileByKey               ;; Compile the specified Lambda source using a key
			deleteLambdaByIndex         ;; Delete an Lambda from the extent using an index
			deleteLambdaByKey           ;; Delete an Lambda from the extent using a key
			deleteBinaryByIndex        ;; Delete an Lambda binary value using an index
			deleteBinaryByKey          ;; Delete an Lambda binary value using a key
			deleteSourceByIndex        ;; Delete an Lambda source value using an index
			deleteSourceByKey          ;; Delete an Lambda source value using a key
			getNextLevel               ;; Return an XPath directory string list for the next repository directory level.
			getTypes                   ;; Return an XPath directory type list string for all possible types in a hierarchical descendents list
			inspectRepository          ;; Perform a com plete inspection of my extent repository.
			memberIndex                ;; Return the index of a key in an extent
			refLambdaByIndex            ;; Retrieve an Lambda from an extent using an index
			refLambdaByKey              ;; Retrieve an Lambda from an extent using a key
			refBinaryByIndex           ;; Retrieve an Lambda binary value using an index
			refBinaryByKey             ;; Retrieve an Lambda binary value using a key
			refKeyByIndex              ;; Retrieve an extent key using an index
			refLength                  ;; Retrieve the number of items in an extent
			refSourceByIndex           ;; Retrieve an Lambda source value using an index
			refSourceByKey             ;; Retrieve an extent source value using a key
			refSourceExByIndex         ;; Retrieve an Lambda source (for export) using an index
			refSourceExByKey           ;; Retrieve an extent source (for export) using a key
			setLambdaByKey              ;; Store an extent value using a key
			setBinaryByKey             ;; Store an extent Lambda binary value using a key
			setFocus                   ;; Set the specified extent in focus 
			setSourceByKey             ;; Store an extent Lambda source value using a key
			setSourceExByKey           ;; Store an extent Lambda source (from export) using a key
			;; Private Variables
			myDependencies             ;; The cabinet names upon which I depend (I may need recompilation if any of these cabinets change). 
			myDependents               ;; The cabinet names who depend upon me (Each of these cabinets may need recompilation if I change). 
			myCurrentTreeLocation      ;; The current memory tree location for this file cabinet 
			myRepository               ;; The object repository for this extent 
			myExtentName               ;; The name for this extent
			myLambdaName                ;; The name for the current Lambda being managed
			;; Private Child Lambdas
			_length                    ;; Perform the length function (but not on Lambdas).
			_isName                    ;; Return true IFF the argument is a valid name.
			) ; end persistant variables
	;;****************************************************************
	;; Define Private Child Lambdas
	;;****************************************************************
	(defun _length(x) (if (not (or (isMacro x) (isLambda x))) (length x) 1))
	(defun _isName(name) (setq name (symbol (string name))) (or (isCharAlphabetic name[0]) (= name[0] #\_)))
	;;****************************************************************
	;; Define Public Child Lambdas
	;;****************************************************************
	;; Remove the specified extent from focus 
	(defun abortFocus() true)
	;; Abort a transaction on the extent repository 
	(defun abortTransaction() (setq myRepository #void) true)
	;; Begin a transaction on the extent repository 
	(defun beginTransaction() (setq myRepository (getSymbolTable 1 1 1)) true)
	;; Clear the extent repository
	(defun clearRepository() (writeln "browseLib._memoryManagerTemplate: not implemented"))
	;; Commit a transaction on the extent repository 
	(defun commitTransaction() (setq myRepository #void) true)
	;; Compile all Lambdas in the current file cabinet. This is not implemented for _memoryManagerTemplate
	(defun compileAll() (writeln "browseLib._memoryManagerTemplate: not implemented") true)
	;; Compile the specified Lambda source using an index 
	(defun compileByIndex(index alwaysCompile) (writeln "browseLib._memoryManagerTemplate: not implemented"))
	;; Compile the specified Lambda source using a key 
	(defun compileByKey(key alwaysCompile)(writeln "browseLib._memoryManagerTemplate: not implemented"))
	;; Delete an Lambda from the extent using an index 
	(defun deleteLambdaByIndex(index) (writeln "browseLib._memoryManagerTemplate: not implemented"))
	;; Delete an Lambda from the extent using a key 
	(defun deleteLambdaByKey(key) (writeln "browseLib._memoryManagerTemplate: not implemented"))
	;; Delete an Lambda binary value using an index 
	(defun deleteBinaryByIndex(index) (writeln "browseLib._memoryManagerTemplate: not implemented"))
	;; Delete an Lambda binary value using a key 
	(defun deleteBinaryByKey(key)(writeln "browseLib._memoryManagerTemplate: not implemented"))
	;; Delete an Lambda source value using an index 
	(defun deleteSourceByIndex(index) (writeln "browseLib._memoryManagerTemplate: not implemented"))
	;; Delete an Lambda source value using a key 
	(defun deleteSourceByKey(key) (writeln "browseLib._memoryManagerTemplate: not implemented"))
	;; Return an XPath directory string list for the next repository directory level.
	(defun getNextLevel(memoryItemName startLine lineCount ...)
		vars:(i I s exp v name value item itemType itemDisplay index cols (options #(1 1 1)))
		;; Note1: Each XPath directory line has the following tab delimited fields:
		;;         type    		{..data type..}
		;;         value   		{memoryItemName for each node in the repository.}
		;;         size			none
		;;         date			none
		;;         time			none
		;;         version			{current AIS version}
		;;         symbolicKey		{memoryItemName for each node in the repository}
		;;         uniqueKey		{memoryItemIndex for each node in the repository}
		;; Note2: As we descend into the memory tree, we keep track of our
		;;        current location. We use the current location for our directory
		;;        length and directory indexing operations.
		;; We return an XPath directory line for each element which is a descendent 
		(onError (lambda(msg) (setq msg (append "Error" #\tab "Cannot navigate memory path=[" msg "]"))))
		;; of the specified memory tree location. 
		(cond
			;; root case
			((or (= memoryItemName #void) (= memoryItemName ""))
			(begin
				(if (>= (argCount) 4) (setq options (argFetch 3)))
				(setq v (getSymbolTable options[0] options[1] options[2])) 
				(setq I (length v))
				(setq I (min I (+ startLine lineCount)))
				(setq s (append "" startLine #\tab lineCount #\tab I )) 
				(setq s (append s #\newline "Current" #\tab memoryItemName #\tab I #\tab "" #\tab "" #\tab (isVersion) #\tab memoryItemName #\tab memoryItemName)) 
				(loop for i from startLine until I do
					(setq name v[i])
					(setq value (left (substitute (substitute (substitute (string (setq item (getGlobalValue name))) #\tab " ") #\newline " ") #\return " ") 40))
					(setq itemType (type item))
					(setq s (append s #\newline itemType #\tab (append name " = " value) #\tab (_length item) #\tab "" #\tab "" #\tab (isVersion) #\tab name #\tab name))
					) ;; end of loop
				)) ; end root case
			;; all other cases
			(else
			(begin
				(setq v (stringToVector memoryItemName "/")) 
				(setq I (length v))
				(setq exp (append "|" (string v[0]) "|"))
				(loop for i from 1 until I do
					(cond ((= (left (string v[i]) 1) "[") (setq exp (append exp (string v[i])))) (else (setq exp (append exp ".|" (string v[i]) "|")))) 
					) ;; end of loop
				(setq item (eval exp))
				;; Produce directory list of memory elements depending on data type.
				(cond
				;; String Item case
				((or (isString item) (isSymbol item) (isByteVector item))
				(begin
					(setq I (_length item))
					(setq s (append "" startLine #\tab lineCount #\tab 1 )) 
					(setq s (append s #\newline "Current" #\tab memoryItemName #\tab 1 #\tab "" #\tab "" #\tab (isVersion) #\tab memoryItemName #\tab memoryItemName)) 
					(setq value (left (substitute (substitute (substitute (string item) #\tab " ") #\newline " ") #\return " ") 40)) 
					(setq s (append s #\newline (type item) #\tab (append LambdaName " = " value)  #\tab I #\tab "" #\tab "" #\tab "" #\tab (isVersion) #\tab memoryItemName #\tab memoryItemName))
					)) ; end case
				;; Lambda Item case
				((or (isLambda item) (isMacro item))
				(begin 
					(setq itemDisplay (stringToVector (disassemble item src: short:) _eol))
					(setq I (_length itemDisplay))
					(setq I (min I (+ startLine lineCount)))
					(setq s (append "" startLine #\tab lineCount #\tab I )) 
					(setq s (append s #\newline "Current" #\tab memoryItemName #\tab I #\tab "" #\tab "" #\tab (isVersion) #\tab memoryItemName #\tab memoryItemName)) 
					(setq name "Av")
					(setq value item.Av)
					(setq itemType (type value))
					(setq value (left (substitute (substitute (substitute (string value) #\tab " ") #\newline " ") #\return " ") 40))
					(setq s (append s #\newline itemType #\tab (append name " = " value) #\tab (_length item.Av) #\tab "" #\tab "" #\tab (isVersion) #\tab "Av" #\tab "Av"))
					(setq name "Sv")
					(setq value item.Sv)
					(setq itemType (type value))
					(setq value (left (substitute (substitute (substitute (string value) #\tab " ") #\newline " ") #\return " ") 40))
					(setq s (append s #\newline itemType #\tab (append name " = " value) #\tab (_length item.Sv) #\tab "" #\tab "" #\tab (isVersion) #\tab "Sv" #\tab "Sv"))
					(setq name "Tv")
					(setq value item.Tv)
					(setq itemType (type value))
					(setq value (left (substitute (substitute (substitute (string value) #\tab " ") #\newline " ") #\return " ") 40))
					(setq s (append s #\newline itemType #\tab (append name " = " value) #\tab (_length item.Tv) #\tab "" #\tab "" #\tab (isVersion) #\tab "Tv" #\tab "Tv"))
					(setq name "Pv")
					(setq value item.Pv)
					(setq itemType (type value))
					(setq value (left (substitute (substitute (substitute (string value) #\tab " ") #\newline " ") #\return " ") 40))
					(setq s (append s #\newline itemType #\tab (append name " = " value) #\tab (_length item.Pv) #\tab "" #\tab "" #\tab (isVersion) #\tab "Pv" #\tab "Pv"))
					(setq name "Cv")
					(setq value item.Cv)
					(setq itemType (type value))
					(setq value (left (substitute (substitute (substitute (string value) #\tab " ") #\newline " ") #\return " ") 40))
					(setq s (append s #\newline itemType #\tab (append name " = " value) #\tab (_length item.Cv) #\tab "" #\tab "" #\tab (isVersion) #\tab "Cv" #\tab "Cv"))
					(setq name "Rv")
					(setq value item.Rv)
					(setq itemType (type value))
					(setq value (left (substitute (substitute (substitute (string value) #\tab " ") #\newline " ") #\return " ") 40))
					(setq s (append s #\newline itemType #\tab (append name " = " value) #\tab (_length item.Rv) #\tab "" #\tab "" #\tab (isVersion) #\tab "Rv" #\tab "Rv"))
					(setq name "In")
					(setq value item.In)
					(setq itemType (type value))
					(setq value (left (substitute (substitute (substitute (string value) #\tab " ") #\newline " ") #\return " ") 40))
					(setq s (append s #\newline itemType #\tab (append name " = " value) #\tab (_length item.In) #\tab "" #\tab "" #\tab (isVersion) #\tab "In" #\tab "In"))
					(setq name "Vm")
					(setq value item.Vm)
					(setq itemType (type value))
					(setq value (left (substitute (substitute (substitute (string value) #\tab " ") #\newline " ") #\return " ") 40))
					(setq s (append s #\newline itemType #\tab (append name " = " value) #\tab (_length item.Vm) #\tab "" #\tab "" #\tab (isVersion) #\tab "Vm" #\tab "Vm"))
					(setq v itemDisplay)
					(setq I (_length v))
					(loop for i from startLine until I do
						(setq value (left (substitute (substitute (substitute (string v[i]) #\tab " ") #\newline " ") #\return " ") 120))
						(setq s (append s #\newline "Pcode" #\tab value #\tab 1 #\tab "" #\tab "" #\tab (isVersion) #\tab (append "Pc[" i "]") #\tab (append "Pc[" i "]")))
						) ; end loop
					)) ; end case
				;; Single Item case
				((isPair item)
				(begin 
					(setq I 2)
					(setq I (min I (+ startLine lineCount)))
					(setq s (append "" startLine #\tab lineCount #\tab I )) 
					(setq s (append s #\newline "Current" #\tab memoryItemName #\tab I #\tab "" #\tab "" #\tab (isVersion) #\tab memoryItemName #\tab memoryItemName)) 
					(setq value (car item))
					(setq itemType (type value))
					(setq value (left (substitute (substitute (substitute (string value) #\tab " ") #\newline " ") #\return " ") 40)) 
					(setq s (append s #\newline itemType #\tab (append name " = " value) #\tab (_length value) #\tab "" #\tab "" #\tab (isVersion) #\tab "[0]" #\tab "[0]"))
					(setq value (cdr item))
					(setq itemType (type value))
					(setq value (left (substitute (substitute (substitute (string value) #\tab " ") #\newline " ") #\return " ") 40)) 
					(setq s (append s #\newline itemType #\tab (append name " = " value) #\tab (_length value) #\tab "" #\tab "" #\tab (isVersion) #\tab "[1]" #\tab "[1]"))
					)) ; end case
				;; Structure Item case
				((or (isStructure item) (isDictionary item) (isDirectory item))
				(begin 
					(setq I (_length item))
					(setq I (min I (+ startLine lineCount)))
					(setq s (append "" startLine #\tab lineCount #\tab I )) 
					(setq s (append s #\newline "Current" #\tab memoryItemName #\tab I #\tab "" #\tab "" #\tab (isVersion) #\tab memoryItemName #\tab memoryItemName)) 
					(loop for i from 0 until I do
					(setq name item[i 0])
					(setq value item[i 1])
					(cond ((isNumber (parse name)) (setq name (append "[" (string name) "]"))) ((not (_isName name)) (setq name (append "[" i  " 1]")))) 
					(setq itemType (type value))
					(setq value (left (substitute (substitute (substitute (string value) #\tab " ") #\newline " ") #\return " ") 40)) 
					(setq s (append s #\newline itemType #\tab (append name " = " value) #\tab (_length value) #\tab "" #\tab "" #\tab (isVersion) #\tab name #\tab (append "[" i " 1]")))
					) ;; end of loop
					)) ; end case
				;; ObjectRepository Item case
				((= (type item) ObjectRepository:)
				(begin 
					(setq index (inspect item directory:))
					(setq I (_length index))
					(setq I (min I (+ startLine lineCount)))
					(setq s (append "" startLine #\tab lineCount #\tab I )) 
					(setq s (append s #\newline "Current" #\tab memoryItemName #\tab I #\tab "" #\tab "" #\tab (isVersion) #\tab memoryItemName #\tab memoryItemName)) 
					(loop for i from 0 until I do
					(setq name index[i 0])
					(setq value index[i 1])
					(cond ((isNumber (parse name)) (setq name (append "[" (string name) "]"))) ((not (_isName name)) (setq name (append "[" i  " 1]")))) 
					(setq itemType (type value))
					(setq value (left (substitute (substitute (substitute (string value) #\tab " ") #\newline " ") #\return " ") 40))
					(setq s (append s #\newline itemType #\tab (append name " = " value) #\tab (_length value) #\tab "" #\tab "" #\tab (isVersion) #\tab name #\tab (append "[" i " 1]")))
					) ;; end of loop
					)) ; end case
				;;Record Item case
				((= (type item) Brick:)
				(begin
					(setq I (length (setq cols item["FieldList"])))
					(setq I (min I (+ startLine lineCount)))
					(setq s (append "" startLine #\tab lineCount #\tab I )) 
					(setq s (append s #\newline "Current" #\tab memoryItemName #\tab I #\tab "" #\tab "" #\tab (isVersion) #\tab memoryItemName #\tab memoryItemName)) 
					(loop for i from 0 until I do
					(setq name cols[i 0])
					(setq value item[i])
					(setq itemType (type value))
					(setq value (left (substitute (substitute (substitute (string value) #\tab " ") #\newline " ") #\return " ") 40))
					(setq s (append s #\newline itemType #\tab (append name " = " value) #\tab (_length value) #\tab "" #\tab "" #\tab (isVersion) #\tab name #\tab (append "[" i " 1]")))
					) ;; end of loop
					)) ;; end case
				;; Vector Item case
				((> (_length item) 0)
				(begin 
					(setq I (_length item))
					(setq I (min I (+ startLine lineCount)))
					(setq s (append "" startLine #\tab lineCount #\tab I )) 
					(setq s (append s #\newline "Current" #\tab memoryItemName #\tab I #\tab "" #\tab "" #\tab (isVersion) #\tab memoryItemName #\tab memoryItemName)) 
					(loop for i from 0 until I do
					(setq name (append "[" i "]"))
					(setq value item[i])
					(setq itemType (type value))
					(setq value (left (substitute (substitute (substitute (string value) #\tab " ") #\newline " ") #\return " ") 40))
					(setq s (append s #\newline itemType #\tab (append name " = " value) #\tab (_length value) #\tab "" #\tab "" #\tab (isVersion) #\tab name #\tab name))
					) ;; end of loop
					)) ; end case
				;; Single Item case
				(else
				(begin 
					(setq s (append "" startLine #\tab lineCount #\tab 1 )) 
					(setq s (append s #\newline "Current" #\tab memoryItemName #\tab 1 #\tab "" #\tab "" #\tab (isVersion) #\tab memoryItemName #\tab memoryItemName)) 
					(setq value (left (substitute (substitute (substitute (string item) #\tab " ") #\newline " ") #\return " ") 40)) 
					(setq s (append s #\newline (type item) #\tab (append memoryItemName " = " value) #\tab (_length item) #\tab "" #\tab "" #\tab (isVersion) #\tab memoryItemName #\tab memoryItemName))
					)) ; end case
				) ; end cond
				)) ; end else case
			) ; end cond
		s)
	;; Return an XPath directory type list string for all possible types in a hierarchical descendents list 
	(defun getTypes()
		vars:(result)
		(setq result (append ".default." #\tab "descend" #\newline
								"Void" #\tab "popup" #\newline
								"Error" #\tab "popup" #\newline
								"Boolean" #\tab "popup" #\newline
								"Integer" #\tab "popup" #\newline
								"Number" #\tab "popup" #\newline
								"Short" #\tab "popup" #\newline
								"Character" #\tab "popup" #\newline
								"Compare" #\tab "popup" #\newline
								"Type" #\tab "popup" #\newline
								"Pointer" #\tab "popup" #\newline
								"Date" #\tab "popup" #\newline
								"Money" #\tab "popup" #\newline
								"CFunction" #\tab "popup" #\newline
								"CProcedure" #\tab "popup" #\newline
								"CMacro" #\tab "popup" #\newline
								"SpecialForm" #\tab "popup" #\newline
								"Text" #\tab "popup,edit" #\newline
								"Source" #\tab "edit,popup" #\newline
								"String" #\tab "popup,edit" #\newline
								"Symbol" #\tab "popup,edit" #\newline
								"QuotedSymbol" #\tab "popup,edit" #\newline
								"LabelSymbol" #\tab "popup,edit" #\newline
								"ByteVector" #\tab "popup,edit" #\newline
								"Complex" #\tab "popup" #\newline
						))
		result)
	;; Perform a complete inspection of my extent repository.
	(defun inspectRepository() (gc))
	;; Return the index of a key in an extent 
	(defun memberIndex(key) (writeln "browseLib._memoryManagerTemplate: not implemented"))
	;; Retrieve an extent value using an index 
	(defun refLambdaByIndex(index) (getGlobalValue myRepository[index]))
	;; Retrieve an extent value using a key 
	(defun refLambdaByKey(key)
		vars:(i I v exp item)
		(setq v (stringToVector key "/")) 
		(setq I (length v))
		(setq exp (append "|" (string v[0]) "|"))
		(loop for i from 1 until I do
			(cond ((= (left (string v[i]) 1) "[") (setq exp (append exp (string v[i])))) (else (setq exp (append exp "." (string v[i]))))) 
			) ;; end of loop
		(setq item (eval exp))
		item) ; end refLambdaByKey
	;; Retrieve an Lambda binary value using an index 
	(defun refBinaryByIndex(index) (getGlobalValue myRepository[index])) 
	;; Retrieve an Lambda binary value using a key 
	(defun refBinaryByKey(key) (refLambdaByKey key))
	;; Retrieve an extent key using an index 
	(defun refKeyByIndex(index) myRepository[index])
	;; Retrieve the number of items in an extent 
	(defun refLength() (length myRepository))
	;; Retrieve an extent scource value using an index 
	(defun refSourceByIndex(index) (refSourceByKey (refKeyByIndex index))) 
	;; Retrieve an Lambda source value using a key 
	(defun refSourceByKey(key)
		vars:(i I s exp v name value item itemType)
		(setq item (refLambdaByKey key))
		;; Produce directory list of memory elements depending on data type.
		(cond
			;; String Item case
			((or (isString item) (isSymbol item) (isByteVector item))
			(begin
				(setq s (string item true))
			)) ; end case
			;; Lambda Item case
			((or (isLambda item) (isMacro item))
			(begin 
				(setq s (string item))
				(setq name "Av")
				(setq value item.Av)
				(setq value (left (substitute (substitute (substitute (string value) #\tab " ") #\newline " ") #\return " ") 40))
				(setq s (append s #\newline name " = " value))
				(setq name "Tv")
				(setq value item.Tv)
				(setq value (left (substitute (substitute (substitute (string value) #\tab " ") #\newline " ") #\return " ") 40))
				(setq s (append s #\newline name " = " value))
				(setq name "Pv")
				(setq value item.Pv)
				(setq value (left (substitute (substitute (substitute (string value) #\tab " ") #\newline " ") #\return " ") 40))
				(setq s (append s #\newline name " = " value))
				(setq name "Cv")
				(setq value item.Cv)
				(setq value (left (substitute (substitute (substitute (string value) #\tab " ") #\newline " ") #\return " ") 40))
				(setq s (append s #\newline name " = " value))
				(setq name "Rv")
				(setq value item.Rv)
				(setq value (left (substitute (substitute (substitute (string value) #\tab " ") #\newline " ") #\return " ") 40))
				(setq s (append s #\newline name " = " value))
				(setq name "In")
				(setq value item.In)
				(setq value (left (substitute (substitute (substitute (string value) #\tab " ") #\newline " ") #\return " ") 40))
				(setq s (append s #\newline name " = " value))
				(setq name "Vm")
				(setq value item.Vm)
				(setq value (left (substitute (substitute (substitute (string value) #\tab " ") #\newline " ") #\return " ") 40))
				(setq s (append s #\newline name " = " value))
				(setq v (stringToVector (disassemble item src: short:) _eol))
				(setq I (length v))
				(loop for i from 0 until I do
				(setq value (left (substitute (substitute (substitute (string v[i]) #\tab " ") #\newline " ") #\return " ") 120))
				(setq s (append s #\newline value))
				) ; end loop
				)) ; end case
			;; Single Item case
			((isPair item)
			(begin 
				(setq s (string item))
				(setq value (car item))
				(setq value (left (substitute (substitute (substitute (string value) #\tab " ") #\newline " ") #\return " ") 40)) 
				(setq s (append s #\newline "[0] = " value))
				(setq value (cdr item))
				(setq value (left (substitute (substitute (substitute (string value) #\tab " ") #\newline " ") #\return " ") 40)) 
				(setq s (append s #\newline "[1] = " value))
				)) ; end case
			;; Structure Item case
			((or (isStructure item) (isDictionary item) (isDirectory item))
			(begin 
				(setq s (append (string item) "[length=" (length item) "]"))
				(setq I (length item))
				(loop for i from 0 until I do
				(setq name item[i 0])
				(setq value item[i 1])
				(cond ((isNumber (parse name)) (setq name (append "[" (string name) "]"))) ((not (_isName name)) (setq name (append "[" i  " 0]")))) 
				(setq value (left (substitute (substitute (substitute (string value) #\tab " ") #\newline " ") #\return " ") 40))
				(setq s (append s #\newline name " = " value))
				) ;; end of loop
			)) ; end case
			;; ObjectRepository Item case
			((= (type item) ObjectRepository:)
			(begin 
				(setq s (append (string item) "[length=" (length item) "]"))
				(setq item (inspect item directory:))
				(setq I (length item))
				(loop for i from 0 until I do
				(setq name item[i 0])
				(setq value item[i 1])
				(cond ((isNumber (parse name)) (setq name (append "[" (string name) "]"))) ((not (_isName name)) (setq name (append "[" i  " 0]")))) 
				(setq value (left (substitute (substitute (substitute (string value) #\tab " ") #\newline " ") #\return " ") 40))
				(setq s (append s #\newline name " = " value))
				) ;; end of loop
			)) ; end case
			;; Vector Item case
			((> (length item) 0)
			(begin 
				(setq s (append (string item) "[length=" (length item) "]"))
				(setq I (length item))
				(loop for i from 0 until I do
				(setq name (append "[" i "]"))
				(setq value item[i])
				(setq value (left (substitute (substitute (substitute (string value) #\tab " ") #\newline " ") #\return " ") 40))
				(setq s (append s #\newline name " = " value))
				) ;; end of loop
			)) ; end case
			;; Single Item case
			(else
			(begin 
				(setq s (string item true))
				)) ; end case
			) ; end cond
		(setq result s)
		result) ; end refSourceByKey
	;; Retrieve an extent scource (for export) using an index 
	(defun refSourceExByIndex(index) (refSourceByIndex index)) 
	;; Retrieve an Lambda source (for export) using a key 
	(defun refSourceExByKey(key) (refSourceByKey key))
	;; Store an extent value using a key 
	(defun setLambdaByKey(key newLambda) (writeln "browseLib._memoryManagerTemplate: not implemented"))
	;; Store an extent Lambda binary value using a key 
	(defun setBinaryByKey(key newBinary) (writeln "browseLib._memoryManagerTemplate: not implemented"))
	;; Set the specified extent in focus 
	(defun setFocus() true)
	;; Store an extent Lambda source value using a key 
	(defun setSourceByKey(key newSource) (writeln "browseLib._memoryManagerTemplate: not implemented"))
	;; Store an extent Lambda source (from export) using a key 
	(defun setSourceExByKey(key newSource) (writeln "browseLib._memoryManagerTemplate: not implemented"))
	;;****************************************************************
	;; MAIN initialization section
	;;****************************************************************
	(setq myExtentName extentName)
	(setq myRepository #void)
	myRepository) ; end _memoryManagerTemplate



























;;**EXPORTKEY**:browseLib.dir
;; dir Lambda
; The dir Lambda provides access to directory structures and 
; their contents in a platform-independent way. 
; A dir instance is used to manipulate path names, access 
; information regarding paths and files, and manipulate 
; the underlying file system.
; 
; A dir instance can point to a file using either a relative 
; or an absolute path. Absolute paths begin with the directory 
; separator "/" (optionally preceded by a drive specification 
; under Windows). If you always use "/" as a directory 
; separator, the dir Lambda will translate your paths to conform 
; to the underlying operating system. Relative file names 
; begin with a directory name or a file name and 
; specify a path relative to the current directory. 
;
; The "current" path refers to the application's working 
; directory. A dir's own path is set and retrieved with 
; setPath and path. An example of an absolute path is the 
; string "/tmp/quartz", a relative path might look like "src/fatlib". 
; You can use the child Lambda isRelative to check if a dir is 
; using a relative or an absolute file path. Call convertToAbs 
; to convert a relative dir to an absolute one. For a simplified 
; path use cleanDirPath. To obtain a path which has no symbolic 
; links or redundant ".." elements use canonicalPath. The path 
; can be set with setPath, and changed with cd and cdUp. 
;
; dir provides several child Lambdas, for example, setCurrent to 
; set the application's working directory and currentDirPath to 
; retrieve the application's working directory. Access to some 
; common paths is provided with the child Lambdas, current, home 
; and root which return the dir instance or currentDirPath, 
; homeDirPath and rootDirPath which return the path as a string.
;
; The number of entries in a directory is returned by count. 
; Obtain a string list of the names of all the files and directories 
; in a directory with entryList. If you prefer a list of fileInfo 
; Lambdas use entryInfoList. Both these functions can apply a name 
; filter, an attributes filter (e.g. read-only, files not 
; directories, etc.), and a sort order. The filters and sort may 
; be set with calls to setNameFilter, setFilter and 
; setSorting. They may also be specified in the entryList and 
; entryInfoList's arguments. 
;
; Create a new directory with mkdir, rename a directory with rename 
; and remove an existing directory with rmdir. Remove a file with 
; remove. You can interrogate a directory with exists, isReadable 
; and isRoot. 
;
; To get a path with a filename use filePath, and to get a directory 
; name use dirName, neither of these functions checks for the 
; existence of the file or directory. 
;
; The list of root directories is provided by drives, on Unix systems 
; this returns a list containing one root directory, "/", on Windows 
; the list will usually contain "C:/", and possibly "D:/", etc. 
; It is easiest to work with "/" separators in lisp code. If you need 
; to present a path to the user or need a path in a form suitable for a 
; function in the underlying operating system use convertSeparators. 
;
; A NOTE ON STATIC FUNCTIONS 
; The underlying C++ object has static functions - i.e. functions that
; can be called directly on the class without an instance of an object
; of that class. This is reflected in the dir Lambda. Some child Lambdas are
; listed as "STATIC". You may call these Lambdas directly without creating
; an Lambda instance of dir. The other child Lambdas work only on an
; instance of the dir Lambda.
;
; Notes
; This Lambda surfaces the full functionality of the QDir object. Most
; of the functionality of this Lambda resides in the glueLayer and is
; accessed by calls to the _QDir function. When you create a new 
; dir instance, a corrosponding QDir C++ object is created. Calls
; to child Lambdas of the dir Lambda result in corrosponding calls to the
; member functions of the C++ QDir object. All of these calls are
; made through the _AISQDir function.
; 
; A child Lambda called "free" is provided to force the early destruction
; of the underlying C++ QDir object prior to garbage collection of a
; dir Lambda. The C++ object always be destroyed when the Lambda that created
; it is garbage collected.
;
;Extensions to underlying QDir object (features not in the underlying QDir object)
; See fileList - this child Lambda returns a vector of file information structures.
(deforphan browseLib:dir()
	faces:((myType dir:))
	pvars:(
		; Public variables
		; Public Child Lambdas
		absFilePath		;Returns the absolute path name of a file in the directory. 
		absPath			;Returns the absolute path (a path that starts with "/" or with a drive specification)
		canonicalPath	;Returns the canonical path, i.e. a path without symbolic links or redundant "." or ".." elements.
		cd				;Changes the dir's directory to dirName. 
		cdUp			;Changes directory by moving one directory up from the dir's current directory. 
		cleanDirPath	;Removes all multiple directory separators "/" and resolves any "."s or ".."s found in the path, filePath. 
		count			;Returns the total number of directories and files that were found.
		currentDirPath	;Returns the absolute path of the application's current directory. 
		convertToAbs	;Converts the directory path to an absolute path. If it is already absolute nothing is done.
		convertSeparators ;Returns pathName with the '/' separators converted to separators that are appropriate for the underlying operating system.
		current			;Returns the application's current directory. 
		dirName			;Returns the name of the directory.
		drives			;Returns a list of the root directories on this system.
		entryInfoList	;
		entryList		;Returns a vector of the names of all the files and directories in the directory.
		exists			;Checks for the existence of the file name. 
		filePath		;Returns the path name of a file in the directory.
		filter			;Returns the value set by setFilter 
		fileList		;Returns a vector of file information structures for all the files and directories in the directory.
		free			;Force the early destruction of the C++ resources allocated by a dir Lambda instance
		home			;Returns the home directory. 
		homeDirPath		;Returns the absolute path of the user's home directory. 
		isReadable		;Returns TRUE if the directory is readable and we can open files by name; otherwise returns FALSE. 
		isRelative		;Returns TRUE if the directory path is relative to the current directory and returns FALSE if the path is absolute.
		isRelativePath	;Returns TRUE if path is relative; returns FALSE if it is absolute. 
		isRoot			;Returns TRUE if the directory is the root directory; otherwise returns FALSE. 
		nameFilter		;Returns the string set by setNameFilter 
		new				;Construct a new dir instance 
		match			;Returns TRUE if the fileName matches the wildcard (glob) pattern filter; otherwise returns FALSE.
		matchAllDirs	;Returns the value set by setMatchAllDirs() 
		mkdir			;Creates a directory. 
		path			;Returns the path, this may contain symbolic links, but never contains redundant ".", ".." or multiple separators. 
		refresh			;Refreshes the directory information. 
		remove			;Removes the file, fileName. 
		rename			;Renames a file or directory. 
		rmdir			;Removes a directory.
		root			;Returns the root directory. 
		rootDirPath		;Returns the absolute path for the root directory.
		separator		;Returns the native directory separator; "/" under UNIX (including Mac OS X) and "\" under Windows.
		setCurrent		;Sets the application's current working directory to path.
		setFilter		;Sets the filter used by entryList and entryInfoList to filterSpec.
		setMatchAllDirs	;If enable is TRUE then all directories are included.
		setNameFilter	;Returns the name filter used by entryList and entryInfoList to nameFilter. 
		setPath			;Sets the path of the directory to path.
		setSorting		;Sets the sort order used by entryList and entryInfoList. 
		sorting			;Returns the value set by setSorting 

		;; Utility Lambdas
		selfTest		;Perform full self test on dir Lambda. ex: (browseLib.dir.selfTest)

		;; private variables and child Lambdas
		_AISQDirHandle	;Pointer to C++ QtDir object
		_CleanUp		;Child Lambda responsible for cleaning up on garbage collection of a dir instance
		_CreateFilterSpec
		_CreateSortSpec
		_CheckForBadArgs
		_CheckIfInitialized
		)
(defun selfTest()
	vars:(d h r
		newdir
		result
		)
	(writeln "Checking static Lambdas")
	(writeln {(browseLib.dir.cleanDirPath "C:/somepath") ->} (browseLib.dir.cleanDirPath "C:/somepath"))
	(writeln {(browseLib.dir.currentDirPath) ->} (browseLib.dir.currentDirPath))
	(writeln {(browseLib.dir.convertSeparators "C:/somepath") ->} (browseLib.dir.convertSeparators "C:/somepath"))
	(writeln {(browseLib.dir.convertSeparators "C:\\somepath") ->} (browseLib.dir.convertSeparators "C:\\somepath"))
	;(writeln {(browseLib.dir.drives) ->} (browseLib.dir.drives))
	(writeln {(setq h (browseLib.dir.home)) (h.path) ->} (setq h (browseLib.dir.home)) " " (h.path))
	(writeln {(browseLib.dir.homeDirPath) ->} (browseLib.dir.homeDirPath))
	(writeln {(browseLib.dir.isRelativePath "C:/somepath") (browseLib.dir.isRelativePath "somepath") ->} (browseLib.dir.isRelativePath "C:/somepath") " " (browseLib.dir.isRelativePath "somepath"))
	(writeln {(browseLib.dir.match "*.txt" "myfile.txt") ->} (browseLib.dir.match "*.txt" "myfile.txt"))
	(writeln {(browseLib.dir.match "*.xml" "myfile.txt") ->} (browseLib.dir.match "*.xml" "myfile.txt"))

	(writeln {(setq r (browseLib.dir.root)) (r.path) ->} (setq r (browseLib.dir.root)) " " (r.path))
	
	(writeln {(browseLib.dir.rootDirPath) ->} (browseLib.dir.rootDirPath))
	(writeln {(browseLib.dir.separator) ->} (browseLib.dir.separator))
	(writeln {(setq InfoItemList (browseLib.dir.drives)) ->} (setq InfoItemList (browseLib.dir.drives)))
	(writeln {(loop for i from 0 until (length InfoItemList) do (writeln (InfoItemList[i].fileName))) ->})
	(loop for i from 0 until (length InfoItemList) do (writeln (InfoItemList[i].dirPath)))

	; setCurrent?
	
	(writeln "****Check instance Lambdas***")
	(writeln {(setq d (^new browseLib.dir)) " " (d.path) ->} (setq d (^new browseLib.dir)) " " (d.path))
	(writeln {(d.entryList) ->} (d.entryList))
	(writeln {(d.entryList #void  Files: Name:) ->} (d.entryList #void Files: Name:))	
	(writeln {(d.entryList "*"  Files: Name:) ->} (d.entryList "*" Files: Name:))	
	(writeln {(d.entryList #void  #(Dirs: Files:) #(Name: Reversed:)) ->} (d.entryList #void  #(Dirs: Files:) #(Name: Reversed:)))
	(writeln {(d.entryList "*"  #(Dirs: Files:) #(Name: Reversed:)) ->} (d.entryList "*"  #(Dirs: Files:) #(Name: Reversed:)))
	(writeln {(d.entryList "*.sl"  Files: #(Name: Reversed:)) ->} (d.entryList "*.sl"  Files: #(Name: Reversed:)))
	(writeln {(d.entryList "*.sl" ) ->} (d.entryList "*.sl" ))
	(writeln {(d.absFilePath "anewfile.sl") ->} (d.absFilePath "anewfile.sl"))
	(writeln {(d.exists "astartup.sl") ->} (d.exists "astartup.sl"))
	(writeln {(d.filePath "somefile.txt") ->} (d.filePath "somefile.txt"))
	(writeln {(d.filePath "c:/somefile.txt" true) ->} (d.filePath "c:/somefile.txt" true))
	(writeln {(d.filePath "c:/somefile.txt" false) ->} (d.filePath "c:/somefile.txt" false))
	(writeln {(d.isReadable) ->} (d.isReadable))
	(writeln {(d.isRelative) ->} (d.isRelative))
	(writeln {(d.isRoot) ->} (d.isRoot))
	(writeln {(r.isRoot) ->} (r.isRoot))

	(writeln "****Test seting and unsetting matchAllDirs****")
	(writeln {(d.matchAllDirs) -> } (d.matchAllDirs))
	(writeln {(d.setMatchAllDirs true) ->} (d.setMatchAllDirs true))
	(writeln {(d.matchAllDirs) -> } (d.matchAllDirs))
	(writeln {(d.setMatchAllDirs false) ->} (d.setMatchAllDirs false))
	(writeln {(d.matchAllDirs) -> } (d.matchAllDirs))
	(writeln "****Test creating and removing directories****")
	(writeln "Check if the last run left a 'newdirtwo' directory lying around and remove it if it is there.")
	(writeln {(setq newdir (^new browseLib.dir "newdirtwo")) ->} (setq newdir (^new browseLib.dir "newdirtwo")))
	(writeln {(if (newdir.exists) (newdir.rmdir (newdir.absPath))) ->} (if (newdir.exists) (newdir.rmdir (newdir.absPath))))
	(writeln "Create a dir instance for an OS directory called 'newdir'")
	(writeln {(setq newdir (^new browseLib.dir "newdir")) ->} (setq newdir (^new browseLib.dir "newdir")))
	(writeln {(newdir.path) ->} (newdir.path))
	(writeln {(newdir.absPath) -> } (newdir.absPath))
	(writeln "Remove OS directory if it exists")
	(writeln {(if (newdir.exists) (newdir.rmdir (newdir.absPath))) ->} (if (newdir.exists) (newdir.rmdir (newdir.absPath))))
	(writeln "Create the OS directory using parent directory. Note use of relative path!")
	(writeln {(d.mkdir (newdir.path)) -> } (d.mkdir (newdir.path)))
	(writeln {(newdir.exists) ->} (newdir.exists))
	(writeln {(if (newdir.exists) (newdir.rmdir (newdir.absPath))) ->} (if (newdir.exists) (newdir.rmdir (newdir.absPath))))
	(writeln "Create the OS directory using new dir instance. Note use of absolute path.")
	(writeln "Relative does not work on dir object representing OS directory being created.")
	(writeln {(newdir.mkdir (newdir.absPath)) ->} (newdir.mkdir (newdir.absPath)))
	(writeln {(d.refresh) (d.entryList) ->} (d.refresh)(d.entryList))
	(writeln {(newdir.exists) ->} (newdir.exists))
	(if (not (newdir.exists)) (goto ERRORFOUND:))
	(writeln {(newdir.path) ->} (newdir.path))
	(writeln {(newdir.absPath) "  " (d.filePath "newdirtwo") ->} (newdir.absPath) " " (d.absFilePath "newdirtwo"))
	(writeln {(newdir.rename (newdir.absPath) (d.absFilePath "newdirtwo")) -> }(newdir.rename (newdir.absPath) (d.absFilePath "newdirtwo")))
	(writeln {(d.refresh) (d.entryList) ->} (d.refresh)(d.entryList))
	(writeln {(newdir.setPath (d.absFilePath "newdirtwo")) ->} (newdir.setPath (d.absFilePath "newdirtwo")) )
	(writeln {(newdir.refresh) ->} (newdir.refresh))
	(writeln {(newdir.absPath) ->} (newdir.absPath))
	(writeln {(newdir.exists) ->} (newdir.exists))
	(writeln {(newdir.rmdir (newdir.absPath) true) -> } (newdir.rmdir (newdir.absPath) true))
	(writeln {(d.refresh) (d.entryList) ->} (d.refresh)(d.entryList))
	(writeln "Test filtering and sorting")
	(writeln {(d.nameFilter) -> } (d.nameFilter))
	(writeln {(d.entryList) -> } (d.entryList))
	(writeln {(d.setNameFilter "*.sl") -> } (d.setNameFilter "*.sl"))
	(writeln {(d.entryList) -> } (d.entryList))	
	(writeln {(setq InfoItemList (d.entryInfoList)) ->} (setq InfoItemList (d.entryInfoList)))
	(writeln {(loop for i from 0 until (length InfoItemList) do (writeln (InfoItemList[i].fileName))) ->})
	(loop for i from 0 until (length InfoItemList) do (writeln (InfoItemList[i].fileName)))
	(writeln {(setq InfoItemList (d.fileList) ->} (setq InfoItemList (d.fileList)))
	(loop for i from 0 until (length InfoItemList) do (writeln InfoItemList[i]))
	(setq result true)
	(goto SUCCESS:)
	ERRORFOUND::
	(setq result false)
	SUCCESS::
	;; Clean up - this is imporatant because you can not recompile the 
	;; dir Lambda with instances lying around in memory - errors will occur.
	(setq newdir #void)
	(setq h #void)
	(setq r #void)
	(setq d #void)
	(gc)
result)

;_checkIfInitialized
;This internal function checks to make sure the directory instance is an
;initialized dir instance.
(defun _checkIfInitialized(functionName)
	(if (or (= _AISQDirHandle #void) (= _AISQDirHandle 0)) (begin
		(writeln (append "!Error: dir." functionName ", can not be called on dir directly. Create an instance of dir first."))
		(return false)))
	true)

;_checkForBadArgs
;Arguments
;	functionName
;	argSpec			A vector of vectors holding the allowed types for each argument
;	args			A vector of arguments
;	checkForInit	A boolean indicating whether to check if object is initialized
;Returns false if arguments in the args vector conform to the argument specifications in the argSpec vector. 
;Returns true if a violation is found and error messages are printed specifying what specifications are violated.
;This internal function makes sure the args passed in the args vector match the argument specifications in the argSpec 
;vector. This centralized type checking greatly reduces the size of this Lambda at the cost of an additional function 
;call and one new vector object per child Lambda.
(defun _checkForBadArgs(functionName, argSpec, args, checkForInit)
	vars:(n N j J k K
		arg
		argType
		spec
		types
		)
;(debug traceon:)
	(if (and checkForInit (or (= _AISQDirHandle #void) (= _AISQDirHandle 0))) (begin
		(writeln (append "!Error: dir." functionName ", can not be called on dir directly. Create an instance of dir first."))
		(return true)))

	(if (<> (type argSpec) Vector:) (begin
		(writeln "!Error: dir." functionName ", argSpec error")
		(return true)
		))
	(setq N (length argSpec))
	(setq J (length args))
	(loop for n from 0 until N do ; for each possible argument
		(setq spec argSpec[n])
		(if (= J n) (begin ; we ran out of arguments
			(if (= spec[0] Optional:) ; found a valid end condition
				(return false) ; we ran out of arguments on an optional argument
			else
			(begin ; we were expecting another argument
				(writeln "!Error: dir." functionName ", Too few arguments passed.")
				(return true))
			)))

		(setq arg args[n])
		(setq argType (type arg))
		(setq K (length spec))
		(if (not (isMember argType spec)) (begin
			(setq types "")
			(loop for k from 0 until K do (setq types (append types ", " (string spec[k]))))
			(writeln "!Error: dir." functionName ", argument " n "(" arg ") has type " argType ", it should be one of " types)
			(return true)
			))
	);n

	false)


;_CleanUp
;Child Lambda responsible for cleaning up on garbage collection of a dir instance
; Arguments
;	None
;Returns #void
; Note: The _clearUp Lambda is called when the dir instance is garbage collected and the _cleanUp Lambda's
; evalWhenDoomed property is set to true. 
;See new and free.
(defun _CleanUp() 
	vars:((functionHandle 0))
	(if (and (<> _AISQDirHandle 0) (<> _AISQDirHandle #void)); free resources held by C++ object
		(_AISQDir _AISQDirHandle functionHandle)) 
	(setq _AISQDirHandle #void)
	#void)

;_CreateSortSpec
; Arguments
;	sortSpecArg
; Returns sortSpec argument as an integer, #void, or an error string
;This function takes a vector of sortSpec symbols and generates a sortSpec argument number
;from the local _SortSpec directory variable.
(defun _CreateSortSpec(sortSpecArg)
	vars:(n N m
		sortSpecArgInt
		(_SortSpec #{dir| 
			Name: 0 		;Sort by name
			Time: 1 		;Sort by time
			Size: 2 		;Sort by file size
			Unsorted: 3 	;Do not sort
			SortByMask: 3 	;A mask for Name, Time and Size
			DirsFirst: 4 	;Put directories first, then the files
			Reversed: 8 	;Reverse the sort order
			IgnoreCase: 16 	;Sort case-insensitively
			DefaultSort: -1})

		)
		(if (= sortSpecArg #void) (return sortSpecArg))
		(if (not (isVector sortSpecArg)) (begin
			(return "!Error: dir._CreateSortSpec, sortSpec argument must be a vector or #void")))
		;Create sortSpec integer argument for C++ constructor
		(setq N (length sortSpecArg))
		(loop for n from 0 until N do
			(setq m (member (symbol sortSpecArg[n]) _SortSpec))
			(if (= m false) (begin
				; Return a string to be used in an error message constructed by the calling Lambda
				(return (append " sortSpec vector contains an unknown sort specification:" (string sortSpecArg[n])))
				))
			(setq sortSpecArgInt (bitwiseOr sortSpecArgInt _SortSpec[m 1]))
		)
	sortSpecArgInt)

;_CreateFilterSpec
; Arguments
;	filterSpecArg
; Returns filterSpec argument as an integer, #void, or an error string
;This function takes a vector of filterSpec symbols and generates a filterSpec argument number
;from the local _FilterSpec directory variable.
(defun _CreateFilterSpec(filterSpecArg)
	vars:(n N m
		filterSpecArgInt
		(_FilterSpec #{dir| 
			Dirs: 1 		;List directories only
			Files: 2 		;List files only
			Drives: 4 		;List disk drives only (ignored under Unix)
			NoSymLinks: 8 	;Do not list symbolic links (ignored by operating systems that don't support symbolic links).
			All: 7 			;List directories, files, drives and symlinks (this does not list broken symlinks unless you specify System)
			TypeMask: 15 	;A mask for the the Dirs, Files, Drives and NoSymLinks flags.
			Readable: 16 	;List files for which the application has read access. 
			Writable: 32	;List files for which the application has write access. 	
			Executable: 64 	;List files for which the application has execute access. Executables needs to be combined with Dirs or Files. 
			RWEMask: 112 	;A mask for the Readable, Writable and Executable flags.
			Modified: 128 	;Only list files that have been modified (ignored under Unix)
			Hidden: 256 	;List hidden files (on Unix, files starting with a .)
			System:512  	; List system files (on Unix, FIFOs, sockets and device files) 
			AccessMask: 1008 ; A mask for the Readable, Writable, Executable Modified, Hidden and System flags
			DefaultFilter: -1 })
		)
		(if (= filterSpecArg #void) (return filterSpecArg))
		(if (not (isVector filterSpecArg)) (begin
			(return "!Error: dir._CreateFilterSpec, filterSpec argument must be a vector or #void")))
		;Create filterSpec integer argument for C++ constructor
		(setq N (length filterSpecArg))
		(loop for n from 0 until N do
			(setq m (member (symbol filterSpecArg[n]) _FilterSpec))
			(if (= m false) (begin
				; Return a string to be used in an error message constructed by the calling Lambda
				(return (append " filterSpec vector contains an unknown sort specification:" (string filterSpecArg[n])))
				))
			(setq filterSpecArgInt (bitwiseOr filterSpecArgInt _FilterSpec[m 1]))
		)
filterSpecArgInt)

	
;absFilePath
; Arguments
; 	fileName
;	acceptAbsPath 	optional default=TRUE
;Returns the absolute path name of a file in the directory. 
;Does not check if the file actually exists in the directory. 
;Redundant multiple separators or "." and ".." directories in fileName will not be removed 
;If acceptAbsPath is TRUE a fileName starting with a separator "/" will be returned without change. 
;If acceptAbsPath is FALSE an absolute path will be prepended to the fileName and the resultant string returned. 
;See also filePath.
(defun	absFilePath(fileName ...) 
	vars:(a A
		aCount
		args
		(functionHandle 1)
		)
	(setq A (argCount))
	(setq args (^new Vector: A))
	(loop for a from 0 until A do (setq args[a] (argFetch a)))
	(if (_checkForBadArgs "absFilePath" #(#(String: Text:) #(Optional: Boolean:)) args true) (return #void))
	(_AISQDir _AISQDirHandle functionHandle args))

;absPath
; Arguments
;	none
;Returns the absolute path (a path that starts with "/" or with a drive specification), which 
;may contain symbolic links, but never contains redundant ".", ".." or multiple separators. 
;See also setPath, canonicalPath, exists, cleanDirPath, dirName, and absFilePath.  
(defun absPath() 
	vars:((functionHandle 2))
	(if (not (_checkIfInitialized "absPath")) (return false))
	(_AISQDir _AISQDirHandle functionHandle))

;canonicalPath
; Arguments
;	none
;Returns the canonical path, i.e. a path without symbolic links or redundant "." or ".." elements.
;On systems that do not have symbolic links this function will always return the same string that 
;absPath returns. If the canonical path does not exist (normally due to dangling symbolic links) 
;canonicalPath returns #void. 
(defun canonicalPath() 
	vars:((functionHandle 3))
	(if (not (_checkIfInitialized "cannonicalPath")) (return false))
	(_AISQDir _AISQDirHandle functionHandle))

;cd
; Arguments
;	dirName
;	acceptAbsPath 	optional default=TRUE
;Changes the dir's directory to dirName. 
;If acceptAbsPath is TRUE a path starting with separator "/" will cause the function to change 
;to the absolute directory. If acceptAbsPath is FALSE any number of separators at the beginning 
;of dirName will be removed and the function will descend into dirName. 
;Returns TRUE if the new directory exists and is readable; otherwise returns FALSE. 
;Note that the logical cd operation is not performed if the new directory does not exist. 
;Calling (cd ".." ) is equivalent to calling (cdUp). 
;See also cdUp, isReadable, exists, and path. 
(defun cd(dirName ...) 
	vars:(a A args
		(functionHandle 4)
		)
	(setq A (argCount))
	(setq args (^new Vector: A))
	(loop for a from 0 until A do (setq args[a] (argFetch a)))
	(if (_checkForBadArgs "cd" #(#(String: Text:) #(Optional: Boolean:)) args true) (return #void))
	(_AISQDir _AISQDirHandle functionHandle args))


;cdUp
;Changes directory by moving one directory up from the dir's current directory. 
;Returns TRUE if the new directory exists and is readable; otherwise returns FALSE.
;Note that the logical cdUp operation is not performed if the new directory does not exist. 
;See also cd, isReadable, exists, and path.
(defun cdUp() 
	vars:((functionHandle 5))
	(if (not (_checkIfInitialized "cdUp")) (return false))
	(return (_AISQDir _AISQDirHandle functionHandle)))

;cleanDirPath - STATIC
; Arguments
;	filePath
;Removes all multiple directory separators "/" and resolves any "."s or ".."s found in the path, filePath. 
;Symbolic links are kept. This function does not return the canonical path, but rather the simplest
;version of the input. For example, "./local" becomes "local", "local/../bin" becomes "bin" and
; "/local/usr/../bin" becomes "/local/bin". 
;See also absPath and canonicalPath.
;NOTE: This is a static function and may be called on the base Lambda. eg:
; (setq path (browseLib.dir.cleanDirPath path))
(defun cleanDirPath(filePath) 
	vars:(a A
		args
		(functionHandle 6)
		)
	(setq A (argCount))
	(setq args (^new Vector: A))
	(loop for a from 0 until A do (setq args[a] (argFetch a)))
	(if (_checkForBadArgs "cleanDirPath" #(#(String: Text:)) args false) (return #void))
	(_AISQDir #void functionHandle args))

;currentDirPath - STATIC
; Arguments
;	none
;Returns the absolute path of the application's current directory.
;See also current
;NOTE: This is a static function and may be called on the base Lambda. eg
; (setq path (browseLib.dir.currentDirPath)) 
(defun currentDirPath() 
	vars:((functionHandle 7))
	(return (_AISQDir #void functionHandle)))

;convetToAbs
; Arguments
;	none
;Converts the directory path to an absolute path. If it is already absolute nothing is done.
;See also isRelative
(defun convertToAbs() 
	vars:((functionHandle 8))
	(if (not (_checkIfInitialized "convertToAbs")) (return false))
	(return (_AISQDir _AISQDirHandle functionHandle)))

;convertSeparators - STATIC
; Arguments
;	pathName
;Returns pathName with the '/' separators converted to separators that are appropriate for 
;the underlying operating system.
;On Windows, convertSeparators("c:/winnt/system32") returns "c:\winnt\system32". 
;The returned string may be the same as the argument on some operating systems, 
;for example on Unix.
;NOTE: This is a static function and may be called on the base Lambda. eg:
; (browseLib.dir.convertSeparaters "some path") 
(defun convertSeparators(pathName) 
	vars:(a A
		args
		(functionHandle 9)
		)
	(setq A (argCount))
	(setq args (^new Vector: A))
	(loop for a from 0 until A do (setq args[a] (argFetch a)))
	(if (_checkForBadArgs "convertSeparators" #(#(String: Text:)) args false) (return #void))
	(_AISQDir #void functionHandle args))


;count
; Arguments
;	none
;Returns the total number of directories and files that were found.
(defun count()
	vars:((functionHandle 10))
	(if (not (_checkIfInitialized "count")) (return false))
	(return (_AISQDir _AISQDirHandle functionHandle)))

;current - STATIC
; Arguments
;	none
;Returns the application's current directory.
;Use path to access a dir instance's path. 
;NOTE: This is a static function and may be called on the base Lambda. eg
; (browseLib.dir.current)
(defun current() 
	vars:(d
		(functionHandle 11)
		)
	(setq d (^new browseLib.dir NOOBJECT:)) ; create a dir Lambda without a corrosponding C++ QtDir object
	(setq d._AISQDirHandle (_AISQDir #void functionHandle)); This call creates the C++ QtDir object
	(return d))

;dirName
; Arguments
;	none
;Returns the name of the directory; this is not the same as the path, e.g. a 
;directory with the name "mail", might have the path "/var/spool/mail". If the 
;directory has no name (e.g. it is the root directory) #void is returned.
;No check is made to ensure that a directory with this name actually exists. 
;See also path, absPath, absFilePath and exists.
(defun dirName() 
	vars:((functionHandle 12))
	(if (not (_checkIfInitialized "dirName")) (return false))
	(_AISQDir _AISQDirHandle functionHandle))

;drives - STATIC
; Arguments
;	none
;Returns a vector of the root directories on this system. On Windows this returns a number 
;of fileInfo objects containing "C:/", "D:/" etc. On other operating systems, it returns 
;just one fileInfo object for the root directory (e.g. "/").
;The vector returned is a copy of the information maintained internally by the dir Lambda.
;NOTE: This is a static function and may be called on the base Lambda. eg:
; (setq drives (browseLib.dir.drives)
(defun drives() 
	vars:(a A
		(functionHandle 13)
		InfoItemList
		)
	(setq InfoItemList (_AISQDir #void functionHandle))
	(setq A (length InfoItemList))
	(loop for a from 0 until A do
		;; Create infoItem Lambdas for each pointer (to a QInfoItem) in the vector
		(setq InfoItemList[a] (^new browseLib.fileInfo NOOBJECT: InfoItemList[a]))
	);a
	InfoItemList)

;entryInfoList
; Arguments
;	nameFilter 	optional, use #void for default or string
;	filterSpec 	optional, use #void for default or vector of filterSpec symbols
;	sortSpec	optional, use #void for default or vector of sortSpec symbols
;Returns a vector of fileInfo objects for all the files and directories in the directory, 
;ordered in accordance with setSorting and filtered in accordance with setFilter and setNameFilter. 
; The vector returned is a COPY of the information maintained internally by the dir Lambda.
;The filter and sorting specifications can be overridden using the nameFilter, 
;filterSpec and sortSpec arguments. 
;Returns #void if the directory is unreadable or does not exist or an argument error was encountered. 
;See also entryList, setNameFilter, setSorting, and setFilter. 
(defun	entryInfoList( ... ) 
	vars:(a A
		args
		argType
		(functionHandle 14)
		(nameFilter #void)
		(sortSpec #void)
		InfoItemList
		)
	(setq A (argCount))
	(setq args (^new Vector: 3 (^new String) #void #void))
	(loop for a from 0 until A do (setq args[a] (argFetch a)))
	(if (_checkForBadArgs "entryInfoList" #(#(Optional: String: Text: Void:) #(Optional: Vector: Void: Symbol:) #(Optional: Vector: Void: Symbol:)) args true) (return #void))

	(if (= args[0] #void) (setq args[0] ""))

	;; Proces filterSpec
	(setq filterSpec args[1])
	(setq argType (type filterSpec))
	(if (= argType Symbol:) (begin 
		(setq filterSpec (^new Vector: 1 filterSpec))
		(setq argType Vector:)))
	(if (= argType Vector:) (begin
		(setq filterSpec (_CreateFilterSpec filterSpec)) ; convert filterSpec to integer required by C function
		(if (isString filterSpec) (begin
			(writeln "!Error: dir.entryInfoList, " filterSpec)
			(return #void)
			))
		(setq args[1] filterSpec)
		))
	;; Process sortSpec
	(setq sortSpec args[2])
	(setq argType (type sortSpec))
	(if (= argType Symbol:) (begin
		(setq sortSpec (^new Vector: 1 sortSpec))
		(setq argType Vector:)))
	
	(if (= argType Vector:) (begin
		(setq sortSpec (_CreateSortSpec sortSpec)) ; convert sortSpec to integer required by C function
		(if (isString sortSpec) (begin
			(writeln "!Error: dir.entryInfoList, " sortSpec)
			(return #void)
			))
		(setq args[2] sortSpec)
		))

	(setq InfoItemList (_AISQDir _AISQDirHandle functionHandle args))
	(setq A (length InfoItemList))
	(loop for a from 0 until A do
		;; Create infoItem Lambdas for each pointer (to a QInfoItem) in the vector
		(setq InfoItemList[a] (^new browseLib.fileInfo NOOBJECT: InfoItemList[a]))
	);a
	InfoItemList)

;entryList
; Arguments
;	nameFilter 	optional, use #void for default or string
;	filterSpec 	optional, use #void for default or filterSpec Symbol or vector of filterSpec symbols
;	sortSpec	optional, use #void for default or sortSpec Symbol or vector of sortSpec symbols
;Returns a vector of the names of all the files and directories in the directory, 
;ordered in accordance with setSorting and filtered in accordance with setFilter and setNameFilter. 
(defun entryList(...) 
	vars:(a A
		args
		argType
		(functionHandle 15)
		(nameFilter #void)
		(sortSpec #void)
		)
	(setq A (argCount))
	(setq args (^new Vector: 3 (^new String) #void #void))
	(loop for a from 0 until A do (setq args[a] (argFetch a)))
	(if (_checkForBadArgs "entryList" #(#(Optional: String: Text: Void:) #(Optional: Vector: Void: Symbol:) #(Optional: Vector: Void: Symbol:)) args true) (return #void))

	(if (= args[0] #void) (setq args[0] ""))

	;; Proces filterSpec
	(setq filterSpec args[1])
	(setq argType (type filterSpec))
	(if (= argType Symbol:) (begin 
		(setq filterSpec (^new Vector: 1 filterSpec))
		(setq argType Vector:)))
	(if (= argType Vector:) (begin
		(setq filterSpec (_CreateFilterSpec filterSpec)) ; convert filterSpec to integer required by C function
		(if (isString filterSpec) (begin
			(writeln "!Error: dir.entryList, " filterSpec)
			(return #void)
			))
		(setq args[1] filterSpec)
		))
	;; Process sortSpec
	(setq sortSpec args[2])
	(setq argType (type sortSpec))
	(if (= argType Symbol:) (begin
		(setq sortSpec (^new Vector: 1 sortSpec))
		(setq argType Vector:)))
	
	(if (= argType Vector:) (begin
		(setq sortSpec (_CreateSortSpec sortSpec)) ; convert sortSpec to integer required by C function
		(if (isString sortSpec) (begin
			(writeln "!Error: dir.entryList, " sortSpec)
			(return #void)
			))
		(setq args[2] sortSpec)
		))

	(_AISQDir _AISQDirHandle functionHandle args))

;exists
; Arguments
;	fileName	- Optional
;Checks for the existence of the file name or existence of directory if
;no argument is passed.
(defun exists(...) 
	vars:(a A
		args
		(functionHandle 16)
		)
	(setq A (argCount))
	(setq args (^new Vector: A))
	(loop for a from 0 until A do (setq args[a] (argFetch a)))
	(if (_checkForBadArgs "exists" #(#(Optional: String: Text:)) args true) (return #void))
	(_AISQDir _AISQDirHandle functionHandle args))

;filePath
; Arguments
;	fileName
;	acceptAbsPath 	optional default=TRUE
;Returns the path name of a file in the directory. Does not check if the file 
;actually exists in the directory. If the dir is relative the returned path 
;name will also be relative. Redundant multiple separators or "." and ".." 
;directories in fileName will not be removed (see cleanDirPath). 
(defun filePath(fileName ...) 
	vars:(a A
		args
		(functionHandle 17)
		)
	(setq A (argCount))
	(setq args (^new Vector: A))
	(loop for a from 0 until A do (setq args[a] (argFetch a)))
	(if (_checkForBadArgs "filePath" #(#(String: Text:) #(Optional: Boolean:)) args true) (return #void))
	(_AISQDir _AISQDirHandle functionHandle args))

;filter
;Returns the value set by setFilter 
	(defun filter() 
	vars:((functionHandle 18))
	(if (not (_checkIfInitialized "filter")) (return false))
	(_AISQDir _AISQDirHandle functionHandle))

;fileList
; Arguments
;	nameFilter 	optional, use #void for default or string
;	filterSpec 	optional, use #void for default or filterSpec Symbol or vector of filterSpec symbols
;	sortSpec	optional, use #void for default or sortSpec Symbol or vector of sortSpec symbols
;Returns a vector of file information structures for all the files and directories in the 
;directory, ordered in accordance with setSorting and filtered in accordance 
;with setFilter and setNameFilter. 
; NOTE: This child Lambda has no corrosponding QFileInfo counterpart. We are taking
; the results of an entryInfoList call and creating the file information structures.
;The filter and sorting specifications can be overridden using the 
;filterSpec and sortSpec arguments. 
;Returns an empty vector if the directory is unreadable or does not exist. 
(defun fileList(...) 
	vars:(a A
		FileInfoList
		Result
	)
	(setq Result (^new Vector:))
	(if (not (_checkIfInitialized "fileInfoList")) (return Result))
	(setq A (argCount))
	(cond
		((= A 0) (setq FileInfoList (entryInfoList)))
		((= A 1) (setq FileInfoList (entryInfoList (argFetch 0))))
		((= A 2) (setq FileInfoList (entryInfoList (argFetch 0) (argFetch 1))))
		((= A 3) (setq FileInfoList (entryInfoList (argFetch 0) (argFetch 1) (argFetch 2))))
		(true (writeln "!Error:dir.fileList - too many arguments") (return Result))
	);cond
	(if (= FileInfoList #void) (begin
		(writeln "!Error:dir.fileList - error calling dir.entryInfoList")
		(return Result)
		))
	(setq A (length FileInfoList))
	(setq Result (^new Vector: object: A))
	(loop for a from 0 until A do
		(setq Result[a] (FileInfoList[a].getFileInfoStructure))
	);a
	Result)

;home - STATIC
; Arguments
;	none
;Returns the home directory.
;Under Windows the HOME environment variable is used. If this does not exist the 
;USERPROFILE environment variable is used. If that does not exist the path is 
;formed by concatenating the HOMEDRIVE and HOMEPATH environment variables. 
;If they don't exist the rootDirPath is used (this uses the SystemDrive 
;environment variable). If none of these exist "C:\" is used. 
;Under non-Windows operating systems the HOME environment variable is 
;used if it exists, otherwise rootDirPath is used. 
;See also homeDirPath. 
;NOTE: This is a static function and may be called on the base Lambda. eg
; (setq d (browseLib.dir.home))
(defun home() 
	vars:(d
		(functionHandle 20))
	(setq d (^new browseLib.dir NOOBJECT:)) ; create a dir Lambda without a corrosponding C++ QtDir object
	(setq d._AISQDirHandle (_AISQDir #void functionHandle)); This call creates the C++ QtDir object
	(return d))

;homeDirPath - STATIC
; Arguments
;	none
;Returns the absolute path of the user's home directory. 
;NOTE: This is a static function and may be called on the base Lambda. eg
; (setq path (browseLib.dir.homeDirPath))
(defun homeDirPath() 
	vars:((functionHandle 21))
	(_AISQDir #void functionHandle))

;isReadable
; Arguments
;	none
;Returns TRUE if the directory is readable and we can open files by 
;name; otherwise returns FALSE.
;See also fileInfo:isReadable
(defun isReadable() 
	vars:((functionHandle 22))
	(if (not (_checkIfInitialized "isReadable")) (return false))
	(_AISQDir _AISQDirHandle functionHandle))

;isRelative
; Arguments
;	none
;Returns TRUE if the directory path is relative to the current directory and 
;returns FALSE if the path is absolute (e.g. under UNIX a path is relative 
;if it does not start with a "/
;See also convertToAbs 
(defun isRelative() 
	vars:((functionHandle 23))
	(if (not (_checkIfInitialized "isRelative")) (return false))
	(_AISQDir _AISQDirHandle functionHandle))

;isRelativePath - STATIC
; Arguments
;	pathName
;Returns TRUE if path is relative; returns FALSE if it is absolute.
;See also isRelative 
;NOTE: This is a static function and may be called on the base Lambda. eg:
; (setq testResult (browseLib.dir.isRelativePath somepath))
(defun isRelativePath() 
	vars:(a A
		args
		(functionHandle 24)
		)
	(setq A (argCount))
	(setq args (^new Vector: A))
	(loop for a from 0 until A do (setq args[a] (argFetch a)))
	(if (_checkForBadArgs "isRelativePath" #(#(String: Text:)) args false) (return #void))
	(_AISQDir #void functionHandle args))

;isRoot
; Arguments
;	none
;Returns TRUE if the directory is the root directory; otherwise returns FALSE. 
;Note: If the directory is a symbolic link to the root directory 
;this function returns FALSE. If you want to test for this use canonicalPath
;
;   (setq d (^new browseLib.dir "/tmp/root_link" ))
;   (setq d d.canonicalPath)
;   (if (d.isRoot)
;       (writeln "It is a root link"))
;
;See also root and rootDirPath. 
(defun isRoot() 
	vars:((functionHandle 25))
	(if (not (_checkIfInitialized "isRoot")) (return false))
	(_AISQDir _AISQDirHandle functionHandle))

;match - STATIC
; Arguments
;	filter
;	fileName
;Returns TRUE if the fileName matches the wildcard (glob) pattern filter; otherwise 
;returns FALSE. The filter may contain multiple patterns separated by spaces or semicolons. 
;(See QRegExp wildcard matching.) 
;See also QRegExp::match(). 
;NOTE: This is a static function and may be called on the base Lambda. eg
; (setq m (browseLib.dir.match filter fileName))
(defun match(filter fileName) 
	vars:(a A
		args
		(functionHandle 26)
		)
	(setq A (argCount))
	(setq args (^new Vector: A))
	(loop for a from 0 until A do (setq args[a] (argFetch a)))
	(if (_checkForBadArgs "match" #(#(String: Text:) #(String: Text:)) args false) (return #void))
	(_AISQDir #void functionHandle args))

;matchAllDirs
;Returns the value set by setMatchAllDirs() 
(defun matchAllDirs() 
	vars:((functionHandle 27))
	(if (not (_checkIfInitialized "matchAllDirs")) (return false))
	(_AISQDir _AISQDirHandle functionHandle))

;mkDir
; Arguments
;	dirName
;	acceptAbsPath 	optional default=TRUE
;Creates a directory. 
;If acceptAbsPath is TRUE a path starting with a separator ('/') will 
;create the absolute directory; if acceptAbsPath is FALSE any number 
;of separators at the beginning of dirName will be removed. 
;Returns TRUE if successful; otherwise returns FALSE. 
;See also rmdir. 
(defun mkdir(dirName ...) 
	vars:(a A
		args
		(functionHandle 28)
		)
	(setq A (argCount))
	(setq args (^new Vector: A))
	(loop for a from 0 until A do (setq args[a] (argFetch a)))
	(if (_checkForBadArgs "mkdir" #(#(String: Text:) #(Optional: Boolean:)) args true) (return #void))
	(_AISQDir _AISQDirHandle functionHandle args))

;nameFilter
; Arguments
;	none
;Returns the string set by setNameFilter 
(defun nameFilter() 
	vars:((functionHandle 29))
	(if (not (_checkIfInitialized "nameFilter")) (return false))
	(_AISQDir _AISQDirHandle functionHandle))

;new
; Argument
;	path			optional
;	nameFilter		optional	
;	sortSpec		optional - vector of sort specifications or single sortSpec symbol
;	filterSpec		optional - vector of filter specifications or single filterSpec symbol
;Construct a new dir Lambda instance 
;Constructs a QDir with path path, that filters its entries by name using 
;nameFilter and by attributes using filterSpec. It also sorts the names 
;using sortSpec. 
;The default nameFilter is an empty string, which excludes nothing; the 
;default filterSpec is All, which also means exclude nothing. The 
;default sortSpec is Name|IgnoreCase, i.e. sort by name case-insensitively. 
;Example that lists all the files in "/tmp": 
;
;	(setq d (new browseLib.dir "/tmp" ));	
;   (loop for i from 0 until (d.count) do
;		(writeln d[i])
;	)	
;    
;If path is "" or QString::null, QDir uses "." (the current directory). 
;If nameFilter is "" or QString::null, QDir uses the name filter "*" (all files). 
;Note that path need not exist. 
;See also exists, setPath, setNameFilter, setFilter, and setSorting. 
(defun new(...) 
	vars:(n N s
		args
		aCount 
		result
		pathArg
		nameFilterArg
		newDirLambda
		sortSpecArg
		(sortSpecArgInt 0)
		filterSpecArg
		(filterSpecArgInt 0)
		(functionHandle 30)
		)

	(setq aCount (argCount)) ;; new receives a default first argument

;	(writeln "browseLib.dir.new called")
;	(loop for n from 0 until aCount (writeln "argument " n " " (argFetch n)))

	(setq args (^new Vector: aCount))
	; Check for valid arguments. We check at runtime what C++ checks during
	; compilation. This is important as there are usually no runtime checks
	; in the underlying C++ object for these conditions.
	; We also convert the sortSpec and filterSpec arguments to the
	; form expected by the C++ constructor.
	(if (> aCount 0) (begin ; Check path argument. Must be string or text
		(setq pathArg (argFetch 0))

		;; The following special case code is used when we want to construct a dir Lambda
		;; without a corrosponding C++ QtDir object instance. See browseLib.dir.current for an
		;; example of this useage.
		(if (= pathArg NOOBJECT:) (begin 
			(setq newDirLambda (myself))
			(setq newDirLambda._CleanUp.EvalWhenDoomed true)
			(return newDirLambda)
			));
		
		;; Back to the normal flow...
		(if (not (or (= #void pathArg) (isString pathArg) (isText pathArg))) (begin
			(writeln "!Error: dir.new, path argument must be a string or #void")
			(return #void)))
		(setq args[0] pathArg)
		))
	(if (> aCount 1) (begin ; Check nameFilter argument
		(setq nameFilterArg (argFetch 1))
		(if (not (or (= #void nameFilterArg) (isString nameFilterArg) (isText nameFilterArg))) (begin
			(writeln "!Error: dir.new, nameFilter argument must be a string or #void")
			(return #void)))
		(setq args[1] nameFilterArg)
		))
	(if (> aCount 2) (begin ; Check sortSpec argument
		(setq arg (argFetch 2))
		(if (= (argType arg) Symbol:) (setq arg (new Vector: 1 arg)))
		(setq sortSpecArg (_CreateSortSpec arg))
		(if (isString sortSpecArg) (begin
			(writeln "!Error: dir.new, " sortSpecArg) 
			(return #void)
			))
		(setq args[2] sortSpecArg)
		))
	(if (> aCount 3) (begin ; Check filterSpec argument
		(setq arg (argFetch 3))
		(if (= (argType arg) Symbol:) (setq arg (new Vector: 1 arg)))
		(setq filterSpecArg (_CreateFilterSpec arg))
		(if (isString filterSpecArg) (begin
			(writeln "!Error: dir.new, " filterSpecArg) 
			(return #void)
			))
		(setq args[3] filterSpecArg)
		))

	(setq newDirLambda (myself))

	;Create the QtDir object and associate it with the dir Lambdainstance
	(setq newDirLambda._AISQDirHandle (_AISQDir #void functionHandle args))
	;Setting the EvalWhenDoomed flag on the _CleanUp child Lambda allows the dir Lambda instance to 
	;clean up resources when it is garbage collected.
	(setq newDirLambda._CleanUp.EvalWhenDoomed true)
	newDirLambda)

;path
; Arguments
;	none
;Returns the path, this may contain symbolic links, but never contains 
;redundant ".", ".." or multiple separators.
;The returned path can be either absolute or relative, see setPath. 
;See also setPath, absPath, exists, cleanDirPath, dirName, 
;absFilePath, and convertSeparators.  
(defun path() 
	vars:((functionHandle 31))
	(if (not (_checkIfInitialized "path")) (return false))
	(_AISQDir _AISQDirHandle functionHandle))

;refresh
; Arguments
;	none
;Refreshes the directory information. 
(defun refresh() 
	vars:((functionHandle 32))
	(if (not (_checkIfInitialized "refresh")) (return false))
	(_AISQDir _AISQDirHandle functionHandle))

;remove
; Arguments
;	fileName
;	acceptAbsPath	optional default=TRUE
;Removes the file, fileName.
;Removes the file, fileName. 
;If acceptAbsPath is TRUE a path starting with separator "/" will 
;remove the file with the absolute path. If acceptAbsPath is FALSE 
;any number of separators at the beginning of fileName will be removed 
;and the resultant file name will be removed. 
;Returns TRUE if the file is removed successfully; otherwise returns FALSE.  
(defun remove(fileName ...) 
	vars:(a A
		args
		(functionHandle 33)
		)
	(setq A (argCount))
	(setq args (^new Vector: A))
	(loop for a from 0 until A do (setq args[a] (argFetch a)))
	(if (_checkForBadArgs "remove" #(#(String: Text:) #(Optional: Boolean:)) args true) (return #void))
	(_AISQDir _AISQDirHandle functionHandle args))

;rename
; Arguments
;	oldName
;	newName
;	acceptAbsPath 	optional default=TRUE
;Renames a file or directory. 
;If acceptAbsPaths is TRUE a path starting with a separator ('/') will 
;rename the file with the absolute path; if acceptAbsPaths is FALSE any 
;number of separators at the beginning of the names will be removed. 
;Returns TRUE if successful; otherwise returns FALSE. 
;On most file systems, rename fails only if oldName does not exist or 
;if newName and oldName are not on the same partition. On Windows, 
;rename will fail if newName already exists. However, there are also 
;other reasons why rename can fail. For example, on at least one 
;file system rename fails if newName points to an open file. 
(defun rename(oldName newName ...)
	vars:(a A
		args
		(functionHandle 34)
		)
	(setq A (argCount))
	(setq args (^new Vector: A))
	(loop for a from 0 until A do (setq args[a] (argFetch a)))
	(if (_checkForBadArgs "rename" #(#(String: Text:) #(String: Text:) #(Optional: Boolean:)) args true) (return #void))
	(_AISQDir _AISQDirHandle functionHandle args))

;rmdir
;Removes a directory.
; Arguments
;	dirName
;	acceptAbsPath 	optional default=TRUE
;If acceptAbsPath is TRUE a path starting with a separator ('/') will 
;remove the absolute directory; if acceptAbsPath is FALSE any number of 
;separators at the beginning of dirName will be removed. 
;The directory must be empty for rmdir() to succeed. 
;Returns TRUE if successful; otherwise returns FALSE. 
(defun rmdir(dirName ...) 
	vars:(a A
		args
		(functionHandle 35)
		)
	(setq A (argCount))
	(setq args (^new Vector: A))
	(loop for a from 0 until A do (setq args[a] (argFetch a)))
	(if (_checkForBadArgs "rmdir" #(#(String: Text:) #(Optional: Boolean:)) args true) (return #void))
	(_AISQDir _AISQDirHandle functionHandle args))

;root - STATIC
; Arguments
;	none
;Returns the root directory.
;See also rootDirPath and drives. 
;NOTE: This is a static Lambda and may be called on the base Lambda. eg
; (setq d (browseLib.dir.root)) 
(defun root() 
	vars:(d
		(functionHandle 36)
		)
	(setq d (^new browseLib.dir NOOBJECT:)) ; create a dir Lambda without a corrosponding C++ QtDir object
	(setq d._AISQDirHandle (_AISQDir #void functionHandle)); This call creates the C++ QtDir object
	(return d))

;rootDirPath - STATIC
; Arguments
;	none
;Returns the absolute path for the root directory.
;For UNIX operating systems this returns "/". For Windows file systems 
;this normally returns "c:/". 
;See also root and drives.
;NOTE: This is a static function and may be called on the base Lambda. eg
;(setq path (browseLib.dir.rootDirPath)) 
(defun rootDirPath() 
	vars:((functionHandle 37))
	(_AISQDir #void functionHandle))

;separator - STATIC
; Arguments
;	none
;Returns the native directory separator; "/" under UNIX (including 
; Mac OS X) and "\" under Windows.
;You do not need to use this function to build file paths. 
;If you always use "/", Qt will translate your paths to conform to 
;the underlying operating system. 
;NOTE: This is a static function and may be called on the base Lambda. eg:
; (setq s (browseLib.dir.separator))
(defun separator() 
	vars:((functionHandle 38))
	(_AISQDir #void functionHandle))

;setCurrent - STATIC
;Sets the application's current working directory
; Arguments
;	path
;Returns TRUE if the directory was successfully changed; otherwise 
;returns FALSE.
;NOTE: This is a static function and may be called on the base Lambda. eg:
; (browseLib.dir.setCurrent "somepath") 
(defun setCurrent(path) 
	vars:(a A
		args
		(functionHandle 39)
		)
	(setq A (argCount))
	(setq args (^new Vector: A))
	(loop for a from 0 until A do (setq args[a] (argFetch a)))
	(if (_checkForBadArgs "setCurrent" #(#(String: Text:)) args false) (return #void))
	(_AISQDir #void functionHandle args))

;setFiilter
; Arguments
;	filterSpec	- vector of filter specifications or single filterSpec symbol
;Returns TRUE
;Sets the filter used by entryList and entryInfoList to filterSpec. 
;The filter is used to specify the kind of files that should be 
;returned by entryList and entryInfoList. See FilterSpec. 
(defun setFilter(filterSpec) 
	vars:(a A
		args
		argType
		filterSpec
		(functionHandle 40)
		)
	(setq A (argCount))
	(setq args (^new Vector: A))
	(loop for a from 0 until A do (setq args[a] (argFetch a)))
	(if (_checkForBadArgs "setFilter" #(#(Vector: Symbol:)) args true) (return #void))
	(setq filterSpec args[0])
	(setq argType (type filterSpec))
	(if (= argType Symbol:) (begin 
		(setq filterSpec (^new Vector: 1 filterSpec))
		(setq argType Vector:)))
	(if (= argType Vector:) (begin
		(setq filterSpec (_CreateFilterSpec filterSpec)) ; convert filterSpec to integer required by C function
		(if (isString filterSpec) (begin
			(writeln "!Error: dir.setFilter, " filterSpec)
			(return #void)
			))
		(setq args[0] filterSpec)
		))
	(_AISQDir _AISQDirHandle functionHandle args))

;setMatchAllDirs
; Arguments
;	enable
;Returns TRUE
;If enable is TRUE then all directories are included (e.g. in entryList), 
;and the nameFilter is only applied to the files. If enable is FALSE then the 
;nameFilter is applied to both directories and files.
(defun setMatchAllDirs(enable) 
	vars:(a A
		args
		argType
		filterSpec
		(functionHandle 41)
		)
	(setq A (argCount))
	(setq args (^new Vector: A))
	(loop for a from 0 until A do (setq args[a] (argFetch a)))
	(if (_checkForBadArgs "setMatchAllDirs" #(#(Boolean:)) args true) (return #void))
	(_AISQDir _AISQDirHandle functionHandle args))

;setNameFilter
; Arguments
;	nameFilter
;Returns TRUE
;Sets the name filter used by entryList and entryInfoList to nameFilter. 
(defun setNameFilter(nameFilter) 
	vars:(a A
		args
		argType
		filterSpec
		(functionHandle 42)
		)
	(setq A (argCount))
	(setq args (^new Vector: A))
	(loop for a from 0 until A do (setq args[a] (argFetch a)))
	(if (_checkForBadArgs "setNameFilter" #(#(String: Text:)) args true) (return #void))
	(_AISQDir _AISQDirHandle functionHandle args))

;setPath
; Arguments
;	path
;Returns TRUE
;Sets the path of the directory to path. The path is cleaned of redundant ".", ".." 
;and of multiple separators. No check is made to ensure that a directory with this path exists. 
(defun setPath(path) 
	vars:(a A
		args
		(functionHandle 43)
		)
	(setq A (argCount))
	(setq args (^new Vector: A))
	(loop for a from 0 until A do (setq args[a] (argFetch a)))
	(if (_checkForBadArgs "setPath" #(#(String: Text:)) args true) (return #void))
	(_AISQDir _AISQDirHandle functionHandle args))

;setSorting
; Arguments
;	sortSpec	- vector of sort specifications or single sortSpec symbol
;Returns TRUE
;Sets the sort order used by entryList and entryInfoList.
;The sortSpec is a vector of sort specification symbols.
;See also sorting and SortSpec.  
(defun setSorting(sortSpec) 
	vars:(a A
		args
		argType
		sortSpec
		(functionHandle 44)
		)
	(setq A (argCount))
	(setq args (^new Vector: A))
	(loop for a from 0 until A do (setq args[a] (argFetch a)))
	(if (_checkForBadArgs "setSorting" #(#(Vector: Symbol:)) args true) (return #void))
	(setq sortSpec args[0])
	(setq argType (type filterSpec))
	(if (= argType Symbol:) (begin 
		(setq sortSpec (^new Vector: 1 sortSpec))
		(setq argType Vector:)))
	(if (= argType Vector:) (begin
		(setq sortSpec (_CreateSortSpec sortSpec)) ; convert sortSpec to integer required by C function
		(if (isString sortSpec) (begin
			(writeln "!Error: dir.setSorting, " sortSpec)
			(return #void)
			))
		(setq args[0] sortSpec)
		))
	(_AISQDir _AISQDirHandle functionHandle args))

;sorting
; Arguments
;	none
;Returns the value set by setSorting 
;See also setSorting and SortSpec.
(defun sorting() 
	vars:((functionHandle 45))
	(if (not (_checkIfInitialized "sorting")) (return false))
	(_AISQDir _AISQDirHandle functionHandle))

;*** Main Logic ***
(writeln "!Error: dir Lambda can not be called directly - please create a dir instance. ex: (setq d (new browseLib.dir))")
true)


































;;**EXPORTKEY**:browseLib.fileInfo
;; fileInfo
;
;The fileInfo Lambda provides system-independent file information. 
;A fileInfo Lambda provides information about a file's name and position (path) 
;in the file system, its access rights and whether it is a directory or symbolic 
;link, etc. The file's size and last modified/read times are also available. 
;A fileInfo can point to a file with either a relative or an absolute file path. 
;Absolute file paths begin with the directory separator "/" (or with a drive 
;specification on Windows). Relative file names begin with a directory name or 
;a file name and specify a path relative to the current working directory. An 
;example of an absolute path is the string "/tmp/quartz". A relative path might 
;look like "src/fatlib". You can use fileInfo.isRelative to check whether 
;a fileInfo is using a relative or an absolute file path. Use fileInfo.convertToAbs 
;to convert a relative fileInfo's path to an absolute path. 
;The file that the fileInfo works on is set in the constructor or later with 
;fileInfo.setFile. Use fileInfo.exists to see if the file exists and 
;fileInfo.size to get its size. 
;
;To speed up performance, fileInfo caches information about the file. Because 
;files can be changed by other users or programs, or even by other parts of the
;same program, use fileInfo.refresh to refresh file information. 
;If you want to switch off a fileInfo's caching and force it to access the file 
;system every time you request information from it call (fileInfo.setCaching FALSE). 
;The file's type is obtained with fileInfo.isFile, isDir and isSymLink. The 
;fileInfo.readLink function provides the name of the file the symlink points to. 
;Elements of the file's name can be extracted with fileInfo.dirPath and 
;fileInfo.fileName. The fileName's parts can be extracted with fileInfo.baseName 
;and fileInfo.extension. The file's dates are returned by fileInfo.created, 
;fileInfo.lastModified and fileInfo.lastRead. 
;Information about the file's access permissions is obtained with fileInfo.isReadable, 
;fileInfo.isWritable and fileInfo.isExecutable. The file's ownership is available 
;from fileInfo.owner, fileInfo.ownerId, fileInfo.group and fileInfo.groupId. You 
;can examine a file's permissions and ownership in a single statement using 
;fileInfo.permission. 
;
;If you need to read and traverse directories, see the dir Lambda. 
;
; Notes
; This Lambda surfaces the full functionality of the QFileInfo object. Most
; of the functionality of this Lambda resides in the glueLayer and is
; accessed by calls to the _AISQFileInfo function. When you create a new 
; fileInfo instance, a corrosponding QFileInfo C++ object is created. Calls
; to child Lambdas of the dir Lambda result in corrosponding calls to the
; member functions of the C++ QFileInfo object. All of these calls are
; made through the _AISQFileInfo function.
; 
; A child Lambda called "free" is provided to force the early destruction
; of the underlying C++ QDir object prior to garbage collection of a
; dir Lambda. The C++ object will always be destroyed when the Lambda that 
; created it is garbage collected.
;
;Extensions to underlying QDir object (features not in the underlying QDir object)
; See getFileInfoStructure - this child Lambda returns file information structure.
;
(deforphan browseLib:fileInfo()
	faces:((myType fileInfo:))
	pvars:(
		;Public child Lambdas
		absFilePath
		baseName
		caching
		convertToAbs
		created
		dir
		dirPath
		exists
		extension
		fileName
		filePath
		getFileInfoStructure
		group
		groupId
		isDir
		isExecutable
		isFile
		isHidden
		isReadable
		isRelative
		isSymLink
		isWriteable
		lastModified
		lastRead
		owner
		ownerId
		permission
		readLink
		refresh
		setCaching
		setFile
		size
		
		;Private pvars
		_AISQFileInfoHandle

		;Privage child Lambdas
		_checkForBadArgs
		_checkIfInitialized
		_CleanUp
		)
;selfTest
(defun selfTest()
	vars:(f)
	(writeln "****Check instance Lambdas***")
	(writeln {(setq f (^new browseLib.fileInfo "C:/AisDev/libraries/dir/astartup.sl"))  ->} (setq f (^new browseLib.fileInfo "C:/AisDev/libraries/dir/astartup.sl")))
	(writeln {(f.absFilePath) ->} (f.absFilePath))
	(writeln {(f.baseName) ->} (f.baseName))
	(writeln {(f.caching) ->} (f.caching))
	(writeln {(f.setCaching false) ->} (f.setCaching false))
	(writeln {(f.caching) ->} (f.caching))
	(writeln {(f.setCaching true) ->} (f.setCaching true))
	(writeln {(f.caching) ->} (f.caching))
	(writeln {(f.filePath) ->}(f.filePath))
	(writeln {(f.convertToAbs) ->} (f.convertToAbs))
	(writeln {(f.filePath) ->}(f.filePath))
	(writeln {(f.created) ->}(f.created))
	(writeln {(f.dir) ->} (f.dir))
	(writeln {(f.dirPath) ->} (f.dirPath))
	(writeln {(f.exists) ->} (f.exists))
	(writeln {(f.extension) ->}(f.extension))
	(writeln {(f.fileName) ->}(f.fileName))
	(writeln {(f.filePath) ->}(f.filePath))
	(writeln {(f.group) ->}(f.group))
	(writeln {(f.groupId) ->}(f.groupId))
	(writeln {(f.isDir) ->} (f.isDir))
	(writeln {(f.isExecutable) ->}(f.isExecutable))
	(writeln {(f.isFile) ->}(f.isFile))
	(writeln {(f.isHidden) ->}(f.isHidden))
	(writeln {(f.isReadable) ->}(f.isReadable))
	(writeln {(f.isRelative) ->}(f.isRelative))
	(writeln {(f.isSymLink) ->}(f.isSymLink))
	(writeln {(f.isWriteable) ->}(f.isWriteable))
	(writeln {(f.lastModified) ->}(f.lastModified))
	(writeln {(f.lastRead) ->}(f.lastRead))
	(writeln {(f.owner) ->}(f.owner))
	(writeln {(f.ownerId) ->}(f.ownerId))
;;	(writeln {(f.permission) ->}(f.permission))
	(writeln {(f.readLink) ->}(f.readLink))
	(writeln {(f.refresh) ->}(f.refresh))
	(writeln {(f.size) ->}(f.size))
	(writeln {(f.getFileInfoStructure) ->}(f.getFileInfoStructure))

	(setq f #void)
	(gc)
true)


;_checkIfInitialized
;This internal function checks to make sure the fileInfo instance is an
;initialized fileInfo instance.
(defun _checkIfInitialized(functionName)
	(if (or (= _AISQFileInfoHandle #void) (= _AISQFileInfoHandle 0)) (begin
		(writeln (append "!Error: fileInfo." functionName ", can not be called on fileInfo directly. Create an instance of fileInfo first."))
		(return false)))
	true)

;_checkForBadArgs
;Arguments
;	functionName
;	argSpec			A vector of vectors holding the allowed types for each argument
;	args			A vector of arguments
;	checkForInit	A boolean indicating whether to check if object is initialized
;Returns false if arguments in the args vector conform to the argument specifications in the argSpec vector. 
;Returns true if a violation is found and error messages are printed specifying what specifications are violated.
;This internal function makes sure the args passed in the args vector match the argument specifications in the argSpec 
;vector. This centralized type checking greatly reduces the size of this Lambda at the cost of an additional function 
;call and one new vector object per child Lambda.
(defun _checkForBadArgs(functionName, argSpec, args, checkForInit)
	faces:((myType fileInfo:))
	vars:(n N j J k K
		arg
		argType
		spec
		types
		)
;(debug traceon:)
	(if (and checkForInit (or (= _AISQFileInfoHandle #void) (= _AISQFileInfoHandle 0))) (begin
		(writeln (append "!Error: fileInfo." functionName ", can not be called on fileInfo directly. Create an instance of fileInfo first."))
		(return true)))

	(if (<> (type argSpec) Vector:) (begin
		(writeln "!Error: fileinfo." functionName ", argSpec error")
		(return true)
		))
	(setq N (length argSpec))
	(setq J (length args))
	(loop for n from 0 until N do ; for each possible argument
		(setq spec argSpec[n])
		(if (= J n) (begin ; we ran out of arguments
			(if (= spec[0] Optional:) ; found a valid end condition
				(return false) ; we ran out of arguments on an optional argument
			else
			(begin ; we were expecting another argument
				(writeln "!Error: fileInfo." functionName ", Too few arguments passed.")
				(return true))
			)))

		(setq arg args[n])
		(setq argType (type arg))
		(if (not (isMember argType spec)) (begin
			(setq types "")
			(loop for k from 0 until K do (append types ", " (string spec[k])))
			(writeln "!Error: fileInfo." functionName ", argument " n " has type " argType ", it should be one of " types)
			(return true)
			))
	);n

	false)

;_CreatePermissionSpec
; Arguments
;	permissionSpecArg
;Returns permissionSpec argument as an integer, #void, or an error string
;This function takes a vector of permissionSpec symbols and generates a 
;permissionSpec argument number from the local _PermissionSpec directory variable.
;
;Warning: The semantics of ReadUser, WriteUser and ExeUser 
;are unfortunately not platform independent: on Unix, the rights 
;of the owner of the file are returned and on Windows the rights 
;of the current user are returned. This behavior might change 
;in a future Qt version. If you want to find the rights of the 
;owner of the file, you should use the flags ReadOwner, WriteOwner 
;and ExeOwner. If you want to find out the rights of the current user, 
;you should use fileInfo.isReadable, fileInfo.isWritable and 
;fileInfo.isExecutable().
(defun _CreatePermissionSpec(specArg)
	vars:(n N m
		specArgInt
		(_PermissionSpec #{dir| 
			ReadOwner: 2048	
			ReadUser: 256
			ReadGroup: 32
			ReadOther: 4
			WriteOwner: 1024
			WriteUser: 128
			WriteGroup: 16
			WriteOther: 2
			ExeOwner: 512
			ExeUser: 64
			ExeGroup: 8
			ExeOther: 1
			})

		)
		(if (= specArg #void) (return specArg))
		(if (not (isVector specArg)) (begin
			(return "!Error: fileInfo._CreatePermissionSpec, permissionSpec argument must be a vector or #void")))
		;Create permissionSpec integer argument for C++ constructor
		(setq N (length specArg))
		(loop for n from 0 until N do
			(setq m (member (symbol specArg[n]) _PermissionSpec))
			(if (= m false) (begin
				; Return a string to be used in an error message constructed by the calling Lambda
				(return (append " permissionSpec vector contains an unknown permission specification:" (string specArg[n])))
				))
			(setq specArgInt (bitwiseOr specArgInt _PermissionSpec[m 1]))
		)
	specArgInt)

;_CleanUp
;Child Lambda responsible for cleaning up on garbage collection of a fileInfo instance
; Arguments
;	None
;Returns #void
; Note: The _clearUp Lambda is called when the fileInfo instance is garbage collected and the _cleanUp Lambda's
; evalWhenDoomed property is set to true. 
;See new and free.
(defun _CleanUp() 
	vars:((functionHandle 0))
	(if (and (<> _AISQFileInfoHandle 0) (<> _AISQFileInfoHandle #void)); free resources held by C++ object
		(_AISQFileInfo _AISQFileInfoHandle functionHandle)) 
	(setq _AISQFileInfoHandle #void)
	#void)

;absFilePath 
; Arguments
;	none
;Returns the absolute path including the file name. 
;The absolute path name consists of the full path and the file name. 
;On Unix this will always begin with the root, '/', directory. On 
;Windows this will always begin 'D:/' where D is a drive letter, except 
;for network shares that are not mapped to a drive letter, in which case 
;the path will begin '//sharename/'. 
;This Lambda returns the same as fileInfo.filePath, unless fileInfo.isRelative
;is TRUE. If the fileInfo is empty it returns dir.currentDirPath. 
; This function can be time consuming under Unix (in the order of milliseconds). 
;See also fileInfo.isRelative and fileInfo.filePath. 
(defun absFilePath()
	vars:((functionHandle 1))
	(if (not (_checkIfInitialized "absFilePath")) (return false))
	(_AISQFileInfo _AISQFileInfoHandle functionHandle))


;baseName
; Arguments
;	complete	- optional boolean value (defaults to false)
;Returns the base name of the file. 
;If complete is FALSE (the default) the base name consists of all 
;characters in the file name up to (but not including) the first '.' character. 
;If complete is TRUE the base name consists of all characters in the file name 
;up to (but not including) the last '.' character. 
;The path is not included in either case. 
;See also fileInfo.fileName and fileInfo.extension. 
(defun baseName(...)
	vars:(a A args (functionHandle 2))
	(setq A (argCount))
	(setq args (^new Vector: A))
	(loop for a from 0 until A do (setq args[a] (argFetch a)))
	(if (_checkForBadArgs "baseName" #(#(Optional: Boolean:)) args true) (return #void))
	(_AISQFileInfo _AISQFileInfoHandle functionHandle args))

;caching
;Arguments
;	none
;Returns true if caching is enabled; otherwise returns false.
;See also fileInfo.setCaching and fileInfo.refresh.
(defun caching()
	vars:((functionHandle 3))
	(if (not (_checkIfInitialized "caching")) (return false))
	(_AISQFileInfo _AISQFileInfoHandle functionHandle))

;convertToAbs
;Arguments
;	none
;Returns true. This Lambda converts the file's path to an absolute path.
;If it is already absolute, nothing is done.
;See also fileInfo.filePath and fileInfo.isRelative.
(defun convertToAbs()
	vars:((functionHandle 4))
	(if (not (_checkIfInitialized "convertToAbs")) (return false))
	(_AISQFileInfo _AISQFileInfoHandle functionHandle))

;created
;Arguments
;	none
;Returns the date and time when the file was created.
;On platforms where this information is not avaialble, returns the same
;as fileInfo.lastModified.
;See also fileInfo.lastModified and fileInfo.lastRead.
(defun created()
	vars:((functionHandle 5))
	(if (not (_checkIfInitialized "created")) (return false))
	(_AISQFileInfo _AISQFileInfoHandle functionHandle))

;dir
;Arguments
;	absPath		- optional boolean (default false)
;Returns a dir Lambda instance for the file's path.
;If the fileInfo Lambda is relative and the absPath is false, the dir will be
;relative; otherwise it will be absolute.
;See also fileInfo.dirPath, fileInfo.fileName and fileInfo.isRelative.
(defun dir(...)
	vars:(a A 
		args 
		d
		(functionHandle 6))
	(setq A (argCount))
	(setq args (^new Vector: A))
	(loop for a from 0 until A do (setq args[a] (argFetch a)))
	(if (_checkForBadArgs "dir" #(#(Optional: Boolean:)) args true) (return #void))
	(setq d (^new browseLib.dir NOOBJECT:)) ; create a dir Lambda without a corrosponding C++ QtDir object
	(setq d._AISQDirHandle (_AISQFileInfo _AISQFileInfoHandle functionHandle args)); This call creates the C++ QtDir object
	)

;dirPath
;Arguments
;	absPath		-optional boolean (default false)
;Returns the file's path.
;If the absPath is true an absolute path is returned.
;See also fileInfo.dir, fileInfo.filePath, fileInfo.fileName and 
;fileInfo.isRelative.
(defun dirPath()
	vars:(a A args (functionHandle 7))
	(setq A (argCount))
	(setq args (^new Vector: A))
	(loop for a from 0 until A do (setq args[a] (argFetch a)))
	(if (_checkForBadArgs "dirPath" #(#(Optional: Boolean:)) args true) (return #void))
	(_AISQFileInfo _AISQFileInfoHandle functionHandle args))

;exists
;Arguments
;	none
;Returns true if the file exists; otherwise returns false.
(defun exists()
	vars:((functionHandle 8))
	(if (not (_checkIfInitialized "exists")) (return false))
	(_AISQFileInfo _AISQFileInfoHandle functionHandle))

;extension
;Arguments
;	complete	- optional boolean (default true)
;Returns the file's extension.
;If complete is true (the default), extension returns the string of all
;characters in the file name after (but not including) the first '.' character.
;If complete is false, extension returns the string of all characters in the 
;file name after (but not including) the last '.' character.
;See also fileInfo.fileName and fileInfo.baseName.
(defun extension()
	vars:(a A args (functionHandle 9))
	(setq A (argCount))
	(setq args (^new Vector: A))
	(loop for a from 0 until A do (setq args[a] (argFetch a)))
	(if (_checkForBadArgs "extension" #(#(Optional: Boolean:)) args true) (return #void))
	(_AISQFileInfo _AISQFileInfoHandle functionHandle args))

;fileName
;Arguments
;	none
;Returns the name of the file, excluding the path.
;See also fileInfo.isRelative, fileInfo.filePath, fileInfo.basename and fileInfo.extension.
(defun fileName()
	vars:((functionHandle 10))
	(if (not (_checkIfInitialized "fileName")) (return false))
	(_AISQFileInfo _AISQFileInfoHandle functionHandle))

;filePath
;Arguments
;	none
;Returns the file anme, including the path (which may be absolute or relative).
;See also fileInfo.isRelative and fileInfo.absFilePath.
(defun filePath()
	vars:((functionHandle 11))
	(if (not (_checkIfInitialized "filePath")) (return false))
	(_AISQFileInfo _AISQFileInfoHandle functionHandle))

;getFileInfoStructure
;Arguments
;	none
;Returns a structure containing information on the fileInfo object
;NOTE: This function has no counterpart in the underlying QFileInfo object.
;
(defun getFileInfoStructure()
	vars:(n N
		Result
		)
	(if (not (_checkIfInitialized "getFileInfoStructure")) (return false))
	(setq Result (^new Structure:
		Type: (cond ((isDir) Dir:) ((isFile) File:))
		FileName: (fileName)
		Size: (size)		
		Extension: (extension)
		FilePath: (filePath)
		Created: (created)
		Exists: (exists)
		Group: (group)
		GroupId: (groupId)
		Executable: (isExecutable)
		Hidden: (isHidden)
		Readable: (isReadable)
		Relative: (isRelative)
		SymLink: (isSymLink)
		Writable: (isWriteable)
		Modified: (lastModified)
		Read:	(lastRead)
		Owner: (owner)
		OwnerId: (ownerId)
		Link: (readLink)
		))
	Result)

;group
;Arguments
;	none
;Returns the group of the file. On Windows, on systems where files do not have
;groups, or if an error occurs, #void is returned.
;This Function can be time consuming under Unix (in the order of milliseconds).
;See also fileInfo.groupId, fileInfo.owner and fileInfo.ownerId.
(defun group()
	vars:((functionHandle 12))
	(if (not (_checkIfInitialized "group")) (return false))
	(_AISQFileInfo _AISQFileInfoHandle functionHandle))

;groupId
;Arguments
;	none
;Returns the id of the group the file belongs to. On Windows and on systems
;where files do not have groups this function always returns -2.
;See also fileInfo.group, fileInfo.owner and fileInfo.ownerId.
(defun groupId()
	vars:((functionHandle 13))
	(if (not (_checkIfInitialized "groupId")) (return false))
	(_AISQFileInfo _AISQFileInfoHandle functionHandle))

;isDir
;Arguments
;	none
;Returns true if this object points to a directory or to a symbolic link
;to a directory; otherwise returns false.
(defun isDir()
	vars:((functionHandle 14))
	(if (not (_checkIfInitialized "isDir")) (return false))
	(_AISQFileInfo _AISQFileInfoHandle functionHandle))

;isExecutable
;Arguments
;	none
;Returns true if this file is executable; otherwise false.
(defun isExecutable()
	vars:((functionHandle 15))
	(if (not (_checkIfInitialized "isExecutable")) (return false))
	(_AISQFileInfo _AISQFileInfoHandle functionHandle))

;isFile
;Arguments
;	none
;Returns true if this object points to a file; otherwise false.
(defun isFile()
	vars:((functionHandle 16))
	(if (not (_checkIfInitialized "isFile")) (return false))
	(_AISQFileInfo _AISQFileInfoHandle functionHandle))

;isHidden
;Arguments
;	none
;Returns true if this file is hidden; otherwise false.
(defun isHidden()
	vars:((functionHandle 17))
	(if (not (_checkIfInitialized "isHidden")) (return false))
	(_AISQFileInfo _AISQFileInfoHandle functionHandle))

;isReadable
;Arguments
;	none
;Returns true if the user can read the file; otherwise false.
(defun isReadable()
	vars:((functionHandle 18))
	(if (not (_checkIfInitialized "isReadable")) (return false))
	(_AISQFileInfo _AISQFileInfoHandle functionHandle))

;isRelative
;Arguments
;	none
;Returns true if the path name is relative; otherwise false.
(defun isRelative()
	vars:((functionHandle 19))
	(if (not (_checkIfInitialized "isRelative")) (return false))
	(_AISQFileInfo _AISQFileInfoHandle functionHandle))

;isSymLink
;Arguments
;	none
;Returns true if t his object points to a symbolik link (or to a shortcut
;on Windows); otherwise false.
(defun isSymLink()
	vars:((functionHandle 20))
	(if (not (_checkIfInitialized "isSymLink")) (return false))
	(_AISQFileInfo _AISQFileInfoHandle functionHandle))

;isWriteable()
;Arguments
;	none
;Returns true if the user can write to the file; otherwise false.
(defun isWriteable()
	vars:((functionHandle 21))
	(if (not (_checkIfInitialized "isWriteable")) (return false))
	(_AISQFileInfo _AISQFileInfoHandle functionHandle))

;lastModified
;Arguments
;	none
;Returns the date and time the file was last modified.
(defun lastModified()
	vars:((functionHandle 22))
	(if (not (_checkIfInitialized "lastModified")) (return false))
	(_AISQFileInfo _AISQFileInfoHandle functionHandle))

;lastRead
;Arguments
;	none
;Returns the data and time when th file was last read(accessed). On
;platforms where this information is not available, returns the same
;as lastModified.
(defun lastRead()
	vars:((functionHandle 23))
	(if (not (_checkIfInitialized "lastRead")) (return false))
	(_AISQFileInfo _AISQFileInfoHandle functionHandle))

;new
;Arguments
;	dirInstance		- optional
;	fileName		- optional
;	fileInfoInstance - optional
;Allowed Signatures
; 	(new fileInfo)
;	(new fileInfo NOOBJECT: objectHandle) -- QFileInfo created by some other process
;	(new fileInfo fileName)
;	(new fileInfo dirInstance fileName)
;	(new fileInfo fileInfoInstance)
(defun new(...)
	vars:(a A
		args
		argsType 
		result
		fileName
		(functionHandle 24)
		QtFileInfoHandle
		newLambda
		)
	
	(setq A (argCount))


	(setq args (^new Vector: A))
	(setq argsType (^new Vector: A))
	(loop for a from 0 until A do 
		(setq args[a] (argFetch a))
		(setq argsType[a] (type args[a]))
;	(writeln "args[" a "]=" args[a] " , argsType[]=" argsType[a])
	)

	;Use the following conds to make the appropriate call to _AISQFileInfo which takes
	;arguments in the following signatures:
	; (_AISQFileInfo #void functionHandle fileName dirInstance fileInfoInstance). 
	; Use #void for arguments not supplied by call to (new fileInfo ...)
	(cond
		;(new fileInfo)
		((= A 0)
			(setq QtFileInfoHandle (_AISQFileInfo #void functionHandle #void #void #void))
		)
		;(new fileInfo NOOBJECT: objectHandle) -- see dir.entryInfoList for example usage
		((and (= A 2) (= argsType[0] Symbol:) (= args[0] NOOBJECT:))
			(setq QtFileInfoHandle args[1])
		)
		;(new fileInfo fileName)
		((and (= A 1) (or (= argsType[0] String:) (= argsType[0] Text:))) 
			(setq QtFileInfoHandle (_AISQFileInfo #void functionHandle args[0] #void #void))
		);
		;(new fileInfo fileInfoInstance)
		((and (= A 1) (= argsType[0] Lambda:) (= args[0].In.myType fileInfo:))
			(setq QtFileInfoHandle (_AISQFileInfo #void functionHandle #void  #void args[0]._AISQFileInfoHandle))
		);
		;(new fileInfo dirInstance fileName)
		((and (= A 2) (= argsType[0] Lambda:) (= args[0].In.myType dir:) (or (= argsType[1] String:) (= argsType[1] Text:)))
			(setq QtFileInfoHandle (_AISQFileInfo #void functionHandle args[1] args[0]._AISQDirHandle #void))
		);
		;BAD Signature
		(true 
			(writeln "!Error:fileInfo.new - bad arguments")
			(return #void)
		)
	)

	(setq newLambda (myself))
	(setq newLambda._AISQFileInfoHandle QtFileInfoHandle)
	;Setting the EvalWhenDoomed flag on the _CleanUp child Lambda allows the dir Lambda instance to 
	;clean up resources when it is garbage collected.
	(setq newLambda._CleanUp.EvalWhenDoomed true)
	newLambda)

;owner
;Arguments
;	none
;Returns the owner of the file. On systems where files do not have owners, or
;if an error occurs, #void is returned.
(defun owner()
	vars:((functionHandle 25))
	(if (not (_checkIfInitialized "functionHandle")) (return false))
	(_AISQFileInfo _AISQFileInfoHandle functionHandle))

;ownerId
;Arguments
;	none
;Returns the id of the owner of the file. On windows and on systems where files
;do not have owners this function returns -2.
;See also fileInfo.owner, fileInfo.group and fileInfo.groupId.
(defun ownerId()
	vars:((functionHandle 26))
	(if (not (_checkIfInitialized "ownerId")) (return false))
	(_AISQFileInfo _AISQFileInfoHandle functionHandle))

;permission
;Arguments
;	permissionSpec	- permission spec symbol or vector of permission spec symbols
;Returns true if user has specified permissions on file.
(defun permission(permissionSpec)
	vars:(a A args argType (functionHandle 27))
	(setq A (argCount))
	(setq args (^new Vector: A))
	(loop for a from 0 until A do (setq args[a] (argFetch a)))
	(if (_checkForBadArgs "permission" #(#(Vector: Symbol:)) args true) (return #void))
	(setq argType (type permissionSpec))
	(if (= argType Symbol:) (begin 
		(setq permissionSpec (^new Vector: 1 permissionSpec))
		(setq argType Vector:)))
	(if (= argType Vector:) (begin
		(setq permissionSpec (_CreatePermissionSpec permissionSpec)) ; convert permissionSpec to integer required by C function
		(if (isString permissionSpec) (begin
			(writeln "!Error: fileInfo.permission, " permissionSpec)
			(return #void)
			))
		(setq args[0] permissionSpec)
		))
	(_AISQFileInfo _AISQFileInfoHandle functionHandle args))

;readLink
;Arguments
;	none
;Returns the name a symlink (or shortcut on Windows) points to, or #void if the
;object is a symbolic link. The name may not represent an existing file; it is
;only a string. fileInfo.exists returns true if the symlink points to
;an existing file.
;See also fileInfo.exists, fileInfo.isSymLink, fileInfo.isDir and fileInfo.isFile.
(defun readLink()
	vars:((functionHandle 28))
	(if (not (_checkIfInitialized "readLink")) (return false))
	(_AISQFileInfo _AISQFileInfoHandle functionHandle))

;refresh
;Arguments
;	none
;Refreshes the information about the file. i.e. reads in information from the file
;system the next time a cached property is fetched.
;See also fileInfo.setCaching.
(defun refresh()
	vars:((functionHandle 29))
	(if (not (_checkIfInitialized "refresh")) (return false))
	(_AISQFileInfo _AISQFileInfoHandle functionHandle))

;setCaching
;Arguments
;	enable		- bool
;If enable is true, enables caching of file information. If enable is false
;caching is disabled.
;When caching is enabled, fileInfo reads the file information from the file
;system the first time it's needed, but generally not later. Caching is
;enabled by default.
;See also fileInfo.refresh and fileInfo.caching.
(defun setCaching(enable)
	vars:(a A args (functionHandle 30))
	(setq A (argCount))
	(setq args (^new Vector: A))
	(loop for a from 0 until A do (setq args[a] (argFetch a)))
	(if (_checkForBadArgs "setCaching" #(#(Boolean:)) args true) (return #void))
	(_AISQFileInfo _AISQFileInfoHandle functionHandle args))

;setFile
;Arguments
;	fileName	- string 
;Returns true.
;Sets the file that the fileInfo provides information about to fileName.
;The fileName can be an absolute or relative path.
(defun setFile(fileName)
	vars:(a A args (functionHandle 31))
	(setq A (argCount))
	(setq args (^new Vector: A))
	(loop for a from 0 until A do (setq args[a] (argFetch a)))
	(if (_checkForBadArgs "setFile" #(#(String: Text)) args true) (return #void))
	(_AISQFileInfo _AISQFileInfoHandle functionHandle args))

;size
;Arguments
;	none
;Returns the file size in bytes, or 0 if the file does nto exist or the
;size is 0 or if th size cannot be fetched.
(defun size()
	vars:((functionHandle 32))
	(if (not (_checkIfInitialized "size")) (return false))
	(_AISQFileInfo _AISQFileInfoHandle functionHandle))

;*** Main Logic ***
(writeln "!Error: fileInfo Lambda can not be called directly - please create a fileInfo instance. ex: (setq fi (new fileInfo))")

true)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; support PROFILING macros 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro STARTTIME(...)
;;	Summary 		Setup and initialize the variables required for profiling in the faces: structure
;;	Args:		
;;		posttime:	Symbol argument specifying non-persistent Lambda (ie: call POSTTIMES will be made)
	vars:(result args numArgs)
	(if (= (argCount) 1) 
		(begin
		(setq args (argFetch 0))
		(setq numArgs (length args))
		end)
	else
		(setq numArgs 0))		
	
	(if (= profiling true)
		(setq result (append
		(list (makeSymbol "begin"))

		(if (and (> numArgs 0) (= args[0] posttime:))
			(list (list setq: (list ref: (list ref: (list myself:) (makeQuotedSymbol "In")) (makeQuotedSymbol "postTimeSW")) true))
		else
			(list (list setq: (list ref: (list ref: (list myself:) (makeQuotedSymbol "In")) (makeQuotedSymbol "postTimeSW")) false)))

		;intialize the time variable
		(list (list setq: (list ref: (list ref: (list myself:) (makeQuotedSymbol "In")) (makeQuotedSymbol "time")) (list getTickCount: 0)))
	
		;set "step" vector index incrementor 
		(list (list setq: (list ref: (list ref: (list myself:) (makeQuotedSymbol "In")) (makeQuotedSymbol "step")) 0))
		
		;Increment the invocation counter by one (tc is created if it does not exist)
		(list (list |+=|: (list ref: (list ref: (list myself:) (makeQuotedSymbol "In")) (makeQuotedSymbol "tc")) 1))
	
		;create new t vector if one does not already exist
		(list
			(list if: (list |=|: (list ref: (list ref: (list myself:) (makeQuotedSymbol "In")) (makeQuotedSymbol "t")) #void)
				(list setq: (list ref: (list ref: (list myself:) (makeQuotedSymbol "In")) (makeQuotedSymbol "t")) (list new: (makeQuotedSymbol "Vector") (makeQuotedSymbol "number")))))
		))
	else
		(setq result (list (makeSymbol "begin")))
	);if
	result); STARTTIME





(defmacro SETTIME(...)
;; 	Summary
;;	Args:	None
	vars:(result)
	(if (= profiling true)
		(setq result (append
		(list (makeSymbol "begin"))
		;	Insert the time since the last settime at the position indicated by the step variable
	
		;	(setq (myself)[In:].t[(myself)[In:].step] (getTickCount (myself)[In:].time))
		;	(setq (ref (ref (ref (myself) In:) t:)                                                                         (ref (ref (myself) In:) step:)                                                            (getTickCount (ref (ref (myself) In:) time:)) ))
		(list (list setq: (list ref: (list ref: (list ref: (list myself:) (makeQuotedSymbol "In")) (makeQuotedSymbol "t")) (list ref: (list ref: (list myself:) (makeQuotedSymbol "In")) (makeQuotedSymbol "step"))) (list getTickCount: (list ref: (list ref: (list myself:) (makeQuotedSymbol "In")) (makeQuotedSymbol "time"))) ))
	
		;increment the step variable
		(list (list |+=|: (list ref: (list ref: (list myself:) (makeQuotedSymbol "In")) (makeQuotedSymbol "step")) 1))
	
		; reset the time variable
		(list (list setq: (list ref: (list ref: (list myself:) (makeQuotedSymbol "In")) (makeQuotedSymbol "time")) (list getTickCount: 0)))
		))
	else
		(setq result (list (makeSymbol "begin")))
	);if
	result); SETTIME


(defmacro POSTTIMES(sourceLambda LambdaKeyPrefix LambdaKeySuffix)
;;	Summary		The POSTTIMES macro calls the browseLib.profiler.postTimes Lambda.
;;	Args:       See the postTimes Lambda for specification of these arguments
;;		sourceLambda
;;		LambdaKeyPrefix
;;		LambdaKeySuffix
	vars:(args)
	(if (= profiling true)
		(list (list ref: (list ref: (list ref: browseLib:) (makeQuotedSymbol "profiler")) (makeQuotedSymbol "postTimes")) sourceLambda LambdaKeyPrefix LambdaKeySuffix)
	else
		(list (makeSymbol "begin"))
	);if
	); POSTTIMES





























;;**EXPORTKEY**:browseLib.memoryCursor
(deforphan browseLib:memoryCursor(tableName colNames)
;; *******************************************************************
;; Summary:  BrowseLib generic table cursor which manages a 
;;           generic table in memory. This table cursor is built 
;;           on principles similar to relational cursors, but
;;           loads a complete copy of the table in memory.
;;
;; Notes:    Each element in the table is called a record.
;;           The index of a table record is called its row (rows begin with zero).
;;
;; Depends:  browseLib
;;
;; args:     tableName      The name for this new in memory table.
;;           colNames       The column name vector for this new in memory table OR.
;;                           if colNames is type Brick, then the table is attached
;;                           to the brick using its rows as records and Field Names
;;                           as column names.
;;                          
;; Return:   self           This newly initialized table cursor.
;; *******************************************************************
	;; Persistent Variables
	pvars:(;; Public variables
			fileID              ;; The file id used during import and export.
			myCursorName        ;; The unique name of this investor colony table cursor.
			myCursorNotes       ;; The notes Dictionary of this investor colony table cursor.
			myCursorScore       ;; The score of this investor colony table cursor.
			myCursorType        ;; The type of this investor colony table cursor.
			myCursorTable       ;; The investor colony table underlying this cursor.
			myDataMine          ;; The investor colony owner of this cursor.
			myIndex             ;; The schema repository for investor colony tables.
			myParent            ;; This parent cursor Lambda.
			mySQLHandle	       ;; The SQL handle used for communicating with the MySQL database engine.
			myTableCursor       ;; The cursor for the table of this view (views only).
			showLimit           ;; The maximum number of records to display with the show function.
			bckVectorKey        ;; The repository key of the backup record view vector (bckVector).
			colCount            ;; The number of columns in the table.
			colVector           ;; The table's vector of column names.
			myMemoPad           ;; The table's memo pad (always a Dictionary).
			myObjectName        ;; The name of the table object (#void if cursor is inactive).
			myPathInFileName    ;; The input path and file name of the table object (#void if cursor is inactive).
			myPathOutFileName   ;; The output path and file name of the table object (#void if cursor is inactive).
			qtrVector           ;; The quarterly record view vector.
			recordCount         ;; The table's current total record count.
			recordStructure     ;; The table's record structure.
			validFields         ;; The table's valid fields and types (necessary for query compilation).
			rowVector           ;; The current record view vector.
			bckVector           ;; The backup record view vector.
			viewDirectory       ;; The the Directory of saved table record view vectors.
			UPDIamDirty         ;; Update has occurred switch.
			;; Public Child Lambdas
			attachBrick         ;; Attaches the memory cursor table to the specified Brick argument.
			average             ;; Averages a lambda value on each record of a table.
			averageForAll       ;; Averages a lambda value on ALL records of a table.
			checkin             ;; Checks in an ascii tab delimited table file.
			close               ;; Terminate an update transaction on the table object.
			delete              ;; Delete the specified row from the table object.
			delimitedCells      ;; Returns a block of cells as a tab delimited string.
			deviation           ;; Returns the standard deviation of a lambda value on each record of a table.
			drop                ;; Drop all records from a table object.
			dropView            ;; Drops the specified record view.
			exportTab           ;; Exports ascii tab delimited records from the table.
			fillFromDirectory   ;; Fills a table cursor with records from a unique directory index.
			getColumnHeaders    ;; Returns the table headers as a tab delimited string.
			getNewRecord        ;; Returns a blank new record.
			importTab           ;; Imports ascii tab delimited records into the table.
			importTabIFF        ;; Imports ascii tab delimited records into the table (if and only if they exit).
			insert              ;; Insert a table row.
			insertColumn        ;; Insert a table column.
			isView              ;; Returns true iff the specified key is a saved view of this cursor.
			maximum             ;; Returns the max of a lambda value on each record of a table.
			minimum             ;; Returns the min of a lambda value on each record of a table.
			newIndex            ;; Creates unique index of each record in a table.
			newMemoPadIndex     ;; Creates unique memoPad index of each record in a table.
			omit                ;; Deletes those records from a table for which a lambda predicate is true.
			read                ;; Read a table row for later update.
			readByKey           ;; Read a table row for later update (using the specified index key).
			readLast            ;; Read the last table row for later update.
			refExport           ;; Returns a record, for export, to the exportTab function.
			refImport           ;; Returns an empty record to the importTab function.
			reset               ;; Resets the backup copy and views of the investor colony table.
			restore             ;; Restores the backup copy of the investor colony table.
			restoreView         ;; Restore the specified record view.
			run                 ;; Run the specified javaFilter Lambda against this cursor.
			save                ;; Save the contents of the table cursor back to disk.
			saveView            ;; Save the current record view for later retrieval.
			search              ;; Returns the row index for the first record for which a lambda predicate is true.
			setImport           ;; Receives a single record from the importTab function.
			sharpe              ;; Returns the sharpe ratio of a lambda value on each record of a table.
			show                ;; Shows a group of records starting from the specified row.
			sort                ;; Sort the table rows using a lambda sort function.
			total               ;; Totals a lambda value on each record of a table.
			totalForAll         ;; Totals a lambda value on ALL records of a table.
			truncate            ;; Truncates a table to those records for which a lambda predicate is true.
			updateView          ;; Updates each record in the current view and restores the altered backup view.
			viewMath            ;; Combines two saved views using a mathematical operator.
			write               ;; Write a table row, perhaps previously read, for update.
			writeByKey          ;; Write a table row (using the specified index key).
			writeLast           ;; Write a new table row, to the end of the table, for update.
			writeToLast	       ;; Write a table row, perhaps previously read, for update.
			;; Private Child Lambdas
			__clear             ;; Clears the memory cursor.
			__errorStop         ;; Handles error conditions during sensitive operations.
			__open              ;; Begin an update transaction on a table object.
			__truncateEnds      ;; Truncate records from both ends of a table.
			__truncateMid       ;; Truncate records from the center of a table.
			__viewAND           ;; Combines two view vectors using the logical AND operation.
			__viewOR            ;; Combines two view vectors using the logical OR operation.
			__viewXOR           ;; Combines two view vectors using the logical XOR operation.
			) ;; end of persistent variables
	;; Temporary Variables
	vars:(myCopy)
	;; Initialize the inline child Lambdas.
	;; *******************************************************************
	;; Summary:  Attaches the memory cursor table to the specified Brick argument.
	;; Arguments
	;;	aBrick
	;;	RecordsVector	a records vector referencing a subset of the brick content
	;; *******************************************************************
	(defun attachBrick(aBrick ...)
		regs:(n N) 
		;; Clear the memory cursor note pads and indices memory.
		(setq showLimit 10)
		(setq myIndex #void)
		(setq myTableCursor #void)
		(setq myCursorNotes (new Dictionary:))
		(setq viewDirectory (new Directory:))
		(setq myMemoPad (new Dictionary:))
		(setq fileID #void)
		;; Use the Brick column names as the table's column names.
		(setq colVector (refAttributes aBrick["FieldList"]))
		(setq colCount (length colVector))
		(setq validFields (objectToStructure colVector #(#void)))
		(setq recordStructure (new Vector: colCount))
		(setAttributes recordStructure colVector)
		(if (> (argCount) 1) (begin
			(setq rowVector (argFetch 1))
			(setq recordCount (length rowVector))
			)
		else (begin
			;; Use the Brick's rows as the table's records.
			(setq recordCount (length aBrick))
			(setq N recordCount)       
			(setq rowVector (new Vector: N))
			(loop for n from 0 until N do (setq rowVector[n] aBrick[n]))
			))
		(setq bckVector rowVector)
		recordCount) ; end attachBrick
	;; *******************************************************************
	;; Summary:  Averages a lambda value on each record of a table.
	;; *******************************************************************
	(defun average(valueLambda)
		vars:(result) 
		(setq result (total valueLambda))
		(if (isPositive recordCount) 
			(/= result recordCount)
			(setq result #void)
			) ; end if
		result) ; end average
	;; *******************************************************************
	;; Summary:  Averages a lambda value on ALL records of a table.
	;; *******************************************************************
	(defun averageForAll(valueLambda)
		vars:(result) 
		(setq result (totalForAll valueLambda))
		(if (isPositive (length bckVector)) 
			(/= result (length bckVector))
			(setq result #void)
			) ; end if
		result) ; end averageForAll
	(defun checkin()
	;; *******************************************************************
	;; Summary:  Checks in an ascii tab delimited table file.
	;; *******************************************************************
		(exportTab myPathOutFileName)) ;; end of checkin
	(defun close()
	;; *******************************************************************
	;; Summary:  Terminates an update transaction on the table cursor.
	;; *******************************************************************
		(__clear)
		(setq myObjectName #void)
		(setq myIndex #void)
		(setq colVector #void)
		(setq colCount #void)
		(setq validFields #void)
		(setq colVector #void)
		(setq recordStructure #void)
		(setq myCursorType #void)
		(setq showLimit #void)
		(setq myParent #void)
		(setq myTableCursor #void)
		(setq myCursorNotes #void)
		(setq viewDirectory #void)
		(setq myMemoPad #void)
		(setq colCount #void)
		true) ;; end of close
	(defun delete(row)
	;; *******************************************************************
	;; Summary:  Delete the specified row from the table object.
	;; Args:     row      Row number of row to be deleted.
	;; Return:   true     Always return true. 
	;; *******************************************************************
		;; Make sure the row number is valid.
		(if (or (< row 0) (>= row recordCount))
			(error (append "badRowIndex" 
							"memoryCursor:delete:" myObjectName " - " row
							":An attempt was made to delete with a bad row number.")))
		;; Make sure the backup records are restored.
		(setq rowVector bckVector)
		(setq viewDirectory (new Directory:))
		(setq myIndex (new Directory:))
		(^delete rowVector row)
		(setq recordCount (length rowVector))
		true) ;; end of delete
	(defun delimitedCells(startRow endRow startCol endCol)
	;; *******************************************************************
	;; Summary:  Returns a block of cells as a tab delimited string.
	;; *******************************************************************
		vars:(rowIndex colIndex dls tab cellValue record) 
		;; Return a tab delimited string of cell values for
		;; all cells between the following rows and columns.
		(setq dls "")
		(setq tab (append "" (char 09)))
		(if (> startRow endRow) (return dls))
		(loop for rowIndex from startRow to endRow do
			(if (< rowIndex recordCount)
				(setq record (read rowIndex))
				(setq record #void)
				) ; end if
			(loop for colIndex from startCol to endCol do
				(setq cellValue record[colIndex])
				(if (isString cellValue) (setq cellValue (substitute cellValue tab " ")))
				(if (= cellValue #void) (setq cellValue ""))
				(if (isVector cellValue) (setq cellValue (string cellValue true)))
				(setq cellValue (append "" cellValue)) 
				(setq cellValue (substitute cellValue (char 09)  " "))
				(setq cellValue (substitute cellValue (char 13)  " "))
				(if (<> colIndex endCol) 
					(setq dls (append dls cellValue (char 09)))
					(setq dls (append dls cellValue))
					) ; end if
				) ;;end column loop
			(if (<> rowIndex endRow) 
				(setq dls (append dls (char 13)))
				) ; end if
			) ;;end row loop
		(append dls "")) ;; end of delimitedCells
	(defun deviation(totalLambda)
	;; *******************************************************************
	;; Summary:  Returns the standard deviation of a lambda value on each 
	;;           record of a table.
	;; *******************************************************************
		vars:(rowIndex (result 0) score aSum aSsq anAvg)
		(if (<= recordCount 0) (return 0))
		;; Compute the stadard devisation lambda value for each record in the table.
		(loop for rowIndex from 0 until recordCount do
			(setq score (totalLambda rowVector[rowIndex]))
			(setq aSum (+ aSum score))
			(setq aSsq (+ aSsq (* score score)))
			) ;; end loop
		(setq anAvg (/ aSum recordCount))
		(setq result (sqrt (- (/ aSsq recordCount) (* anAvg anAvg))))
		result) ;; end deviation
	(defun drop()
	;; *******************************************************************
	;; Summary:  Drops all records from the table.
	;; *******************************************************************
		(__clear)) ;; end of drop
	(defun dropView(key)
	;; *******************************************************************
	;; Summary: Drops the current record view as specified.
	;; *******************************************************************
		(setq viewDirectory[key] 1)
		(delete viewDirectory key)
		true) ;; end of dropView
	(defun exportTab(fileName ...)
	;; *******************************************************************
	;; Summary:  Exports ascii tab delimited records from the table.
	;; Args:     fileName   The path and file name to receive the exported data.
	;;           quoteSW    (Optional)If false, text and string fields are NOT to be enclosed in double quotes.
	;; *******************************************************************
		regs:(m M n N)
		vars:((quoteSW true) fieldText record)
		;; Make sure we catch all errors which might occur.
		(onError __errorStop)
		;; Gather the arguments
		(if (> (argCount) 1) (setq quoteSW (argFetch 1)))
		;; Open the specified .tab file and export.
		(setq fileID (fileOpen fileName 1 0))
		;; Export the contents of the table to the receiving file
		(cond
		((= quoteSW true) (^exportTab fileID myParent recordVectors:))
		(else 
			(begin
			;; Export the colum headers as the first record of the .tab delimited file
			(setq record colVector)
			(setq M (sub1 (length colVector)))
			(if (< M 1) (error "browseLib.memoryCursor: cannot export an empty table cursor"))  
			(setq fieldText "")
			(loop for m from 0 until M do (setq fieldText (append fieldText (string record[m]) #\tab)))
			(setq fieldText (append fieldText (string record[M]) _eol))
			(fileWrite fileID fieldText)
			;; Export the records as the remaining rowsa of the .tab delimited file
			(setq N (length bckVector)) 
			(loop for n from 0 until N do
				(setq fieldText "")
				(setq record bckVector[n])
				(loop for m from 0 until M do (setq fieldText (append fieldText (string record[m]) #\tab)))
				(setq fieldText (append fieldText (string record[M]) _eol))
				(fileWrite fileID fieldText)
				) ; end record loop 
			)) ; end no quotes case
		) ; end cond
		;; Close the specified .tab file and return.
		(fileClose fileID 1)
		(setq fileID #void)
		recordCount) ;; end exportTab
	(defun fillFromDirectory(theDirectory)
	;; *******************************************************************
	;; Summary:  Fills a table cursor with records from a unique directory index.
	;; *******************************************************************
		vars:(record rowIndex numRecords colIndex)
		;; Make sure the backup records are used to create the index.
		(__clear)
		(setq numRecords (length theDirectory))
		(setq myMemoPad theDirectory)
		(if (<= numRecords 0) (return numRecords))
		;; Place all records in the index.
		(loop for rowIndex from 0 until numRecords do
			(setq record theDirectory[rowIndex 1])
			(write rowIndex record)
			) ;; end loop
		recordCount) ;; end fillFromDirectory
	(defun getColumnHeaders(startCol endCol) 
	;; *******************************************************************
	;; Summary:  Returns the table headers as a tab delimited string.
	;; *******************************************************************
		vars:(colIndex dls colName)  
		(setq dls "")
		(loop for colIndex from startCol to endCol do
			(setq colName (string colVector[colIndex]))
			(if (= colName "#void") (setq colName ""))
			(if (= (left colName 1) "|") (setq colName (mid colName 1 (subi (length colName) 2))))
			(if (<> colIndex endCol) 
				(setq dls (append dls colName (char 09)))
				(setq dls (append dls colName))
				) ; end if
			) ;;end column loop
		(append dls "")) ;; end getColumnHeaders
	;; Summary:  Returns a blank new record.
	(defun getNewRecord() (copy recordStructure))
	(defun importTab(fileName)
	;; *******************************************************************
	;; Summary:  Performs an import tab transaction on the table cursor.
	;; *******************************************************************
		;; Make sure we catch all errors which might occur.
		(onError __errorStop)
		;; Make sure the backup records are restored.
		(setq rowVector bckVector)
		(setq recordCount (length rowVector))
		(setq viewDirectory (new Directory:))
		(setq myIndex (new Directory:))
		;; Open the specified .tab file and import.
		(setq fileID (fileOpen fileName 0 0))
		(|Gv:importTab| fileID myParent recordVectors:)
		(fileClose fileID 1)
		(setq fileID #void)
		recordCount) ;; end importTab
	(defun importTabIFF(fileName)
	;; *******************************************************************
	;; Summary:  Imports ascii tab delimited records into the table (if and only if they exit).
	;; *******************************************************************
		;; Make sure we return zero records imported if there are errors.
		(onError (lambda(err) 0))
		;; Make sure the backup records are restored.
		(setq rowVector bckVector)
		(setq recordCount (length rowVector))
		(setq viewDirectory (new Directory:))
		(setq myIndex (new Directory:))
		;; Open the specified .tab file and import.
		;; Note: If the specified tab delimited ascii file
		;;       does not exist, then this will cause an error
		;;       which will be trapped and return zero records imported.
		(setq fileID (fileOpen fileName 0 0))
		(|Gv:importTab| fileID myParent recordVectors:)
		(fileClose fileID 1)
		(setq fileID #void)
		recordCount) ;; end importTabIFF
	(defun insert(row record)
	;; *******************************************************************
	;; Summary:  Insert a record into the table for update.
	;; Args:     row      Row number of row to be inserted (before).
	;;           record   Row to be written to the table database.
	;; Return:   record   The row just written. 
	;; *******************************************************************
		;; Make sure the backup records are restored.
		(setq rowVector bckVector)
		(setq recordCount (length rowVector))
		(setq viewDirectory (new Directory:))
		(setq myIndex (new Directory:))
		;; Make sure the row number is valid.
		(if (or (< row 0) (>= row recordCount))
			(error (append "badRowIndex" 
							"memoryCursor:insert:" myObjectName " - " row
							":An attempt was made to insert with a bad row number.")))
		(setq recordCount (addi recordCount 1))
		(setq record (copy record))
		(setAttributes record colVector)
		(^insert rowVector row record)
		;; Return the record just inserted.
		record) ;; end of insert
	(defun insertColumn(colName position)
	;; *******************************************************************
	;; Summary:  Insert a column into the table.
	;; Args:     colName    The name of the column to be inserted.
	;;           position   The position of the column to be inserted (before).
	;; Return:   true       Always return true iff no error occurs. 
	;; *******************************************************************
		vars:(n N record)
		;; Make sure the backup records are restored.
		(setq rowVector bckVector)
		(setq recordCount (length rowVector))
		(setq colCount (length colVector))
		(setq viewDirectory (new Directory:))
		(setq myIndex (new Directory:))
		;; Make sure the position number is valid.
		(if (or (< position 0) (> position colCount))
			(error (append "badColumnIndex" 
							"memoryCursor:insertColumn:" myObjectName " - " position
							":An attempt was made to insert with a bad column number.")))
		;; Insert the column in the column vector.
		(setq colName (symbol colName))
		(^insert colVector position colName)
		(setq colCount (length colVector))
		(setq recordStructure (new Vector: colCount))
		(setAttributes recordStructure colVector)
		;; Insert the column into each record.
		(loop for n from 0 until recordCount do
			(setq record bckVector[n])
			(setAttributes record #void)
			(^insert record position #void)
			(setAttributes record colVector)
			) ; end loop
		true) ;; end of insertColumn
	;; Returns true iff the specified key is a saved view of this cursor.
	(defun isView(key) (<> viewDirectory[key] #void)) 
	(defun maximum(totalLambda)
	;; *******************************************************************
	;; Summary:  Returns the max of a lambda value on each record of a table.
	;; *******************************************************************
		vars:(rowIndex result)
		(if (<= recordCount 0) (return 0))
		(setq result (totalLambda rowVector[0]))
		;; Compute the min lambda value for each record in the table.
		(loop for rowIndex from 1 until recordCount do
			(setq result (max result (totalLambda rowVector[rowIndex])))
			) ;; end loop
		result) ;; end maximum
	(defun minimum(totalLambda)
	;; *******************************************************************
	;; Summary:  Returns the min of a lambda value on each record of a table.
	;; *******************************************************************
		vars:(rowIndex result)
		(if (<= recordCount 0) (return 0))
		(setq result (totalLambda rowVector[0]))
		;; Compute the min lambda value for each record in the table.
		(loop for rowIndex from 1 until recordCount do
			(setq result (min result (totalLambda rowVector[rowIndex])))
			) ;; end loop
		result) ;; end minimum
	(defun newIndex(colName)
	;; *******************************************************************
	;; Summary:  Creates unique index of each record in a table.
	;; *******************************************************************
		vars:(record rowIndex numRecords colIndex)
		;; Make sure the backup records are used to create the index.
		(setq colIndex (member (symbol colName) colVector))
		(if (= colIndex false) (error (append "memoryCursor: unknown index field name[" colName "]")))
		(setq numRecords (length bckVector))
		(setq myIndex (new Directory:))
		(if (<= numRecords 0) (return numRecords))
		;; Place all records in the index.
		(loop for rowIndex from 0 until numRecords do
			(setq record bckVector[rowIndex])
			(setq myIndex[record[colIndex]] rowIndex)
			) ;; end loop
		numRecords) ;; end newIndex
	(defun newMemoPadIndex(colName)
	;; *******************************************************************
	;; Summary:  Creates unique memoPad index of each record in a table.
	;; *******************************************************************
		vars:(record rowIndex numRecords colIndex)
		;; Make sure the backup records are used to create the index.
		(setq colIndex (member (symbol colName) colVector))
		(if (= colIndex false) (error (append "memoryCursor: unknown index field name[" colName "]")))
		(setq numRecords (length bckVector))
		(setq myMemoPad (new Directory:))
		(if (<= numRecords 0) (return numRecords))
		;; Place all records in the index.
		(loop for rowIndex from 0 until numRecords do
			(setq record bckVector[rowIndex])
			(if (= myMemoPad[record[colIndex]] #void)
				(setq myMemoPad[record[colIndex]] record)
				(error (append "browseLib.memoryCursor: duplicate memoPad index key [" colName "," record[colIndex] "," rowIndex "]"))
				) ; end if
			) ;; end loop
		numRecords) ;; end newMemoPadIndex
	(defun omit(findLambda)
	;; *******************************************************************
	;; Summary:  Deletes those records from a table for which a lambda predicate is true.
	;; *******************************************************************
		vars:(rowIndex)
		;; Make sure the backup records are restored.
		(setq rowVector bckVector)
		(setq recordCount (length rowVector))
		(setq viewDirectory (new Directory:))
		(setq myIndex (new Directory:))
		(if (<= recordCount 0) (return recordCount))
		;; Omit those rows for which the find Lambda returns true.
		(loop for rowIndex from 0 until (length rowVector) do
			(if (findLambda rowVector[rowIndex])
				(begin 
					(delete rowIndex)
					(-- rowIndex)
				)) ; end if
			) ;; end loop
		(setq recordCount (length rowVector))
		recordCount) ;; end omit
	(defun read(row)
	;; *******************************************************************
	;; Summary:  Reads a record from the table.
	;; Args:     row      Row number of row to be read.
	;; Return:   record   The row just read. 
	;; *******************************************************************
		(if (= row #void) (setq row 0))
		(if (or (< row 0) (> row recordCount))
			(error (append "badRowIndex" 
							"memoryCursor:read:" myObjectName " - " row
							":An attempt was made to read with a bad row number.")))
		rowVector[row]) ;; end of read
	(defun readByKey(key)
	;; *******************************************************************
	;; Summary:  Reads a record from the table (using the specified key).
	;; Args:     key      Key of row to be read (found in index).
	;; Return:   record   The row just read. 
	;; *******************************************************************
		vars:(row)
		(setq row myIndex[key])
		(if (= row #void) (return #void))
		(if (or (< row 0) (> row recordCount))
			(error (append "badRowKey" 
							"memoryCursor:readByKey:" myObjectName "[" key
							"] :An attempt was made to read with a bad index key.")))
		rowVector[row]) ;; end of readByKey
	(defun readLast()
	;; *******************************************************************
	;; Summary:  Reads the last record from the table.
	;; Args:     none     
	;; Return:   record   The row just read. 
	;; *******************************************************************
		(if (<= recordCount 0) (return #void))
		rowVector[(subi recordCount 1)]) ;; end of readLast
	(defun refExport(row)
	;; *******************************************************************
	;; Summary:  Returns a record, for export, to the exportTab function.
	;; *******************************************************************
		;; If the row is zero, return the column names for the header record.
		(if (= row 0) (return colVector))
		;; Adjust the record index for the header record.
		(setq row (subi row 1))
		;; If there are no more records, tell exportTab to stop exporting.
		(if (>= row recordCount) (return false))
		;; Otherwise return the next record for export.
		(read row)) ;; end of refExport 
	(defun refImport(row)
	;; *******************************************************************
	;; Summary:  Returns an empty record to the importTab function.
	;; *******************************************************************
		;; If the row is zero, return an empty vector to hold the column names,
		;; Otherwise, return an empty record (use the recordStructure as a template).
		(if (= row 0)
			(return (^new Vector: 0))
			(return (copy recordStructure)))) ;; end of refImport
	(defun reset()
	;; *******************************************************************
	;; Summary:  Resets the backup copy and views of the investor colony table. 
	;; *******************************************************************
		;; Make sure the backup records and views are restored.
		(setq rowVector bckVector)
		(setq recordCount (length rowVector))
		(setq viewDirectory (new Directory:))
		(setq myIndex (new Directory:))
		(setq UPDIamDirty true)
		true) ;; end of reset
	(defun restore()
	;; *******************************************************************
	;; Summary: Restores the backup copy of the investor colony table.
	;; *******************************************************************
		(setq rowVector bckVector)
		(setq recordCount (length rowVector))
		true) ;; end of restore
	(defun restoreView(key)
	;; *******************************************************************
	;; Summary: Restores the current record view as specified.
	;; *******************************************************************
		(setq rowVector viewDirectory[key])
		(if (= rowVector #void) (setq rowVector bckVector)) 
		(setq rowVector (copy rowVector))
		(setq recordCount (length rowVector))
		true) ;; end of restoreView
	;; Run the specified javaFilter Lambda against this cursor.
	(defun run(filterString)
		vars:(filterLambda)
		(setq filterLambda (compile (morph (javaScript filterString))))
		(setq filterLambda (filterLambda))
		(filterLambda myParent)) ;; end of run
	(defun save(tableName)
	;; *******************************************************************
	;; Summary:  Save the contents of the table cursor back to disk.
	;; *******************************************************************
		true) ;; end of save
	(defun saveView(key)
	;; *******************************************************************
	;; Summary: Saves the current record view as specified.
	;; *******************************************************************
		(setq viewDirectory[key] (copy rowVector))
		true) ;; end of saveView
	(defun search(findLambda ...)
	;; *******************************************************************
	;; Summary:  Returns the row index for the first record for which a lambda predicate is true.
	;; *******************************************************************
		vars:(startIndex rowIndex n colName colValue (argc 2))
		;; Define the temporary search lambda.
		(defun __searchLambda(x) vars:(colName colValue) (= x[colName] colValue))
		;; If the findLambda is a columnName, we must make our own lambda predicate.
		(if (not (isLambda findLambda))
			(begin
				(setq colName findLambda)
				(setq colValue (argFetch 1))
				(++ argc)
				(setq findLambda __searchLambda)
				(setq findLambda.Tv.colName colName)
				(setq findLambda.Tv.colValue colValue)
			)) ; end if
		;; Make sure we capture the startIndex argument (if any).
		(if (= (argCount) argc)
			(setq startIndex (argFetch (sub1 argc)))
			(setq startIndex 0)
			) ; end if
		(if (<= recordCount 0) (return false))
		;; Select the first row for which the find Lambda returns true.
		(setq n (length rowVector))
		(loop for rowIndex from startIndex until n do
			(if (findLambda rowVector[rowIndex]) (return rowIndex))
			) ;; end loop
		false) ;; end search
	(defun setImport(row recordVector)
	;; *******************************************************************
	;; Summary:  Receives a single record from the importTab function.
	;; *******************************************************************
		regs:(n N)
		;; Import the .tab column header record (if necessary).
		(if (= row 0)
			(if (= recordCount 0)
				(begin
					(setq N (length recordVector))(loop for n from 0 until N do (setq recordVector[n] (symbol (string recordVector[n])))) 
					(setq validFields (objectToStructure recordVector #(#void)))
					(setq colVector (refAttributes validFields))
					(setq recordStructure (new Vector: (length colVector)))
					(setAttributes recordStructure colVector)
					(setq colCount (length colVector))
					(return true)) ;; end setting new column names.
				(begin 
					(setq recordVector (objectToStructure recordVector #(#void)))
					(setq recordVector (refAttributes recordVector))
					(if (<> colVector recordVector)
						(error 
							(append "importTab: An attempt was made to import a file with mismatched column names: "
									myObjectName)))
					(return true)) ;; end checking new column names against old column names.
			)) ;; end if
		;; Import all other .tab records at the end of the table.
		(write recordCount recordVector)
		true) ;; end of setImport
	(defun sharpe(totalLambda)
	;; *******************************************************************
	;; Summary:  Returns the sharpe ratio of a lambda value on each 
	;;           record of a table.
	;; *******************************************************************
		vars:(rowIndex (result 0) score aSum aSsq anAvg aStd)
		(if (<= recordCount 0) (return 0))
		;; Compute the stadard devisation lambda value for each record in the table.
		(loop for rowIndex from 0 until recordCount do
			(setq score (totalLambda rowVector[rowIndex]))
			(setq aSum (+ aSum score))
			(setq aSsq (+ aSsq (* score score)))
			) ;; end loop
		(setq anAvg (/ aSum recordCount))
		(setq aStd (sqrt (- (/ aSsq recordCount) (* anAvg anAvg))))
		(setq result (/ anAvg aStd))
		result) ;; end sharpe
	(defun show(startIndex ...) 
	;; *******************************************************************
	;; Summary:  Shows a group of records starting from the specified row.
	;; *******************************************************************
		vars:(i n limit)
		(if (>= (argCount) 2) (setq limit (argFetch 1)) (setq limit showLimit)) 
		(setq n (integer (min recordCount (addi startIndex limit))))
		(writeln "ColNames = " colVector)
		(loop for i from startIndex until n do
			(writeln "[" i "] " (read i))
			) ;; end loop
		true) ;; end show
	(defun sort(sortLambda ...)
	;; *******************************************************************
	;; Summary:  Sorts each record in the table cursor.
	;; *******************************************************************
		vars:(backupSW)
		;; Are we sorting the backup view as well as the current view?
		(if (and (= (argCount) 2) (= (argFetch 1) backup:))
			then ;; Sort the backup view as well as the current view.
			(begin
				(setq backupSW true)
				(setq viewDirectory (new Directory:))
				(setq myIndex (new Directory:))
				(setq rowVector bckVector)
				(setq recordCount (length rowVector))
				) ; end then
			else ;; Sort only the current view.
			(begin
				(setq backupSW false)
				(setq rowVector (copy rowVector))
				)) ; end if
		(if (<= recordCount 0) (return recordCount))       
		(^sort rowVector sortLambda)
		(setq UPDIamDirty true)
		recordCount) ;; end sort
	(defun total(totalLambda)
	;; *******************************************************************
	;; Summary:  Totals a lambda value on each record of a table.
	;; *******************************************************************
		vars:(rowIndex result)
		(if (<= recordCount 0) (return 0))
		;; Compute the total lambda value for each record in the table.
		(loop for rowIndex from 0 until recordCount do
			(+= result (totalLambda rowVector[rowIndex]))
			) ;; end loop
		result) ;; end total
	(defun totalForAll(totalLambda)
	;; *******************************************************************
	;; Summary:  Totals a lambda value on ALL records of a table.
	;; *******************************************************************
		vars:(rowIndex (result 0))
		(if (<= (length bckVector) 0) (return 0))
		;; Compute the total lambda value for each record in the table.
		(loop for rowIndex from 0 until (length bckVector) do
			(+= result (totalLambda bckVector[rowIndex]))
			) ;; end loop
		result) ;; end totalForAll
	(defun truncate(findLambda)
	;; *******************************************************************
	;; Summary:  Truncates a table to those records for which a lambda predicate is true.
	;; *******************************************************************
		vars:(rowIndex newIndex maxRecords n vec)
		(if (<= recordCount 0) (return recordCount))
		(setq rowVector (copy rowVector))
		(setq recordCount (length rowVector))
		;; If the selectLambda is a number, then keep only the first N records.
		(if (isNumber findLambda)
			(begin
				(setq maxRecords (min recordCount findLambda))
				(setq rowVector (resize rowVector maxRecords))
				(setq recordCount (length rowVector))
				(return recordCount)
			)) ; end if
		;; Select only those rows for which the find Lambda returns true.
		(setq n (length rowVector))
		(setq vec (^new Vector: 0))
		(setq newIndex -1)
		(loop for rowIndex from 0 until n do
			(if (findLambda rowVector[rowIndex]) (setq vec[(++ newIndex)] rowVector[rowIndex]))
			) ;; end loop
		(setq rowVector vec)
		(setq recordCount (length rowVector))
		recordCount) ;; end truncate
	(defun updateView(updateLambda ...)
	;; *******************************************************************
	;; Summary:  Updates each record in the current view and restores the altered backup view.
	;; *******************************************************************
		vars:(_rowVector rowIndex n resetSW)
		;; Check for a no reset request.
		(setq resetSW true)
		(if (and (= (argCount) 2) (= (argFetch 1) noreset:)) (setq resetSW false))
		;; Update each row in the current view.
		(setq _rowVector (copy rowVector))
		(setq n (length _rowVector))
		(loop for rowIndex from 0 until n do
			(updateLambda _rowVector[rowIndex])
			) ;; end loop
		(if (= resetSW true) (restore) myParent)) ;; end updateView
	(defun viewMath(operator key1 key2)
	;; *******************************************************************
	;; Summary: Combines two saved views using a mathematical operator.
	;; *******************************************************************
		(if (= operator and:) 
			(setq rowVector (__viewAND (new Vector: 0) viewDirectory[key1] viewDirectory[key2])))
		(if (= operator or:) 
			(setq rowVector (__viewOR (new Vector: 0) viewDirectory[key1] viewDirectory[key2])))
		(if (= operator xor:) 
			(setq rowVector (__viewXOR (new Vector: 0) viewDirectory[key1] viewDirectory[key2])))
		(setq recordCount (length rowVector))) ;; end of viewMath
	(defun write(row record)
	;; *******************************************************************
	;; Summary:  Write a record to the table for update.
	;; Args:     row      Row number of row to be written.
	;;           record   Row to be written to the table.
	;; Return:   record   The row just written. 
	;; *******************************************************************
		;; Make sure the backup records are restored.
		(setq rowVector bckVector)
		(setq recordCount (length rowVector))
		(setq viewDirectory (new Directory:))
		(setq myIndex (new Directory:))
		;; Make sure the row number is valid.
		(if (or (< row 0) (> row recordCount))
			(error (append "badRowIndex" 
							"memoryCursor:write:" myObjectName " - " row
							":An attempt was made to write with a bad row number.")))
		(if (= row recordCount) (setq recordCount (addi recordCount 1)))
		(setq record (copy record))
		(setAttributes record colVector)
		(setq rowVector[row] record)
		;; Return the record just written.
		record) ;; end of write
	(defun writeByKey(key record)
	;; *******************************************************************
	;; Summary:  Write a record to the table for update (using the specified key).
	;; Args:     key      Index key of row to be written.
	;;           record   Row to be written to the table.
	;; Return:   record   The row just written. 
	;; *******************************************************************
		vars:(row)
		;; Make sure the backup records are restored.
		(setq rowVector bckVector)
		(setq recordCount (length rowVector))
		(setq viewDirectory (new Directory:))
		(if (not (isDirectory myIndex)) (setq myIndex (new Directory:)))
		(setq row myIndex[key])
		(if (= row #void) (begin (setq row recordCount) (setq myIndex[key] row)))
		;; Make sure the row number is valid.
		(if (or (< row 0) (> row recordCount))
			(error (append "badRowKey" 
							"memoryCursor:writeByKey:" myObjectName "[" key
							"] :An attempt was made to write with a bad record key.")))
		(if (= row recordCount) (setq recordCount (addi recordCount 1)))
		(setq record (copy record))
		(setAttributes record colVector)
		(setq rowVector[row] record)
		;; Return the record just written.
		record) ;; end of writeByKey
	(defun writeLast(record)
	;; *******************************************************************
	;; Summary:  Write a record to the end of the table for update.
	;; Args:     record   Row to be written to the end of the table.
	;; Return:   record   The row just written. 
	;; *******************************************************************
		;; Make sure the backup records are restored.
		(setq rowVector bckVector)
		(setq recordCount (length rowVector))
		(setq viewDirectory (new Directory:))
		(setq myIndex (new Directory:))
		(setq record (copy record))
		(setAttributes record colVector)
		(setq rowVector[recordCount] record)
		(setq recordCount (addi recordCount 1))
		;; Return the record just written.
		record) ;; end of writeLast
	(defun writeToLast(record) (writeLast record))
	;; -------------------------------------
	;; Private methods (not for public use).
	;; -------------------------------------
	(defun __clear()
		(setq rowVector (new Vector: 0))
		(setq bckVector rowVector)
		(setq viewDirectory (new Directory:))
		(setq myIndex #void)
		(setq recordCount 0)
		(setq fileID #void)
		true) ;; end of __clear
	(defun __open(tableName colNames)
	;; *******************************************************************
	;; Summary:  Begins an update transaction on the table cursor.
	;; *******************************************************************
		;; Search for the specified table in the reference and blackboard areas.
		vars:(indexOf record)
		(__clear)
		(setq myObjectName (symbol tableName))
		(setq colVector (copy colNames))
		(setq colCount (length colVector))
		(setq validFields (objectToStructure colVector #(#void)))
		(setq colVector (refAttributes validFields))
		(setq recordStructure (new Vector: colCount))
		(setAttributes recordStructure colVector)
		;; Load the memo pad Dictionary into the cursor memory.
		(setq myCursorType memory:)
		(setq showLimit 10)
		(setq myIndex #void)
		(setq myObjectName (symbol tableName))
		(setq myTableCursor #void)
		(setq myCursorNotes (new Dictionary:))
		(setq viewDirectory (new Directory:))
		(setq myMemoPad (new Dictionary:))
		(setq colCount 0)
		true) ;; end of __open
	(defun __errorStop(errMsg)
		(if (isNumber fileID) (fileClose fileID 1))
		(setq fileID #void)
		(error (mid errMsg 1 (subi (length errMsg) 2)))) ;; end __errorStop
	(defun __truncateEnds(begRowLimit endRowLimit)
	;; *******************************************************************
	;; Summary:  Truncate records from both ends of a table.
	;; *******************************************************************
		vars:(rowIndex newIndex n vec endRowIndex)
		(if (<= recordCount 0) (return recordCount))
		;; Select only those rows in between the begin and end row limits.
		(setq n (length rowVector))
		(setq endRowIndex (subi n endRowLimit))
		(setq vec (^new Vector: 0))
		(setq newIndex -1)
		(loop for rowIndex from (addi begRowLimit 1) until endRowIndex do
			(setq vec[(++ newIndex)] rowVector[rowIndex])
			) ;; end loop
		(setq rowVector vec)
		(setq recordCount (length rowVector))
		recordCount) ;; end truncateEnds
	(defun __truncateMid(begRowLimit endRowLimit)
	;; *******************************************************************
	;; Summary:  Truncate records from the center of a table.
	;; *******************************************************************
		vars:(rowIndex newIndex n vec endRowIndex)
		(if (<= recordCount 0) (return recordCount))
		;; Delete those rows in between the begin and end row limits.
		(setq n (length rowVector))
		(setq endRowIndex (subi n endRowLimit))
		(setq vec (^new Vector: 0))
		(setq newIndex -1)
		(loop for rowIndex from 0 until begRowLimit do
			(setq vec[(++ newIndex)] rowVector[rowIndex])
			) ;; end loop
		(loop for rowIndex from endRowIndex until n do
			(setq vec[(++ newIndex)] rowVector[rowIndex])
			) ;; end loop
		(setq rowVector vec)
		(setq recordCount (length rowVector))
		recordCount) ;; end truncateMid
	(defun __viewAND(result vector1 vector2)
	;; *******************************************************************
	;; Summary:  Combines two view vectors using the logical AND operation.
	;; *******************************************************************
		vars:(time record1 record2 index1 index2)
		(setq vector1 (sort (copy vector1) <))
		(setq vector2 (sort (copy vector2) <))
		(setq record1 vector1[(setq index1 0)])
		(setq record2 vector2[(setq index2 0)])
		(while (or (<> record1 #void) (<> record2 #void)) do
		(cond ;; If records match they must be returned.
			((= record1 record2)
				(begin
				(setq result[(length result)] record1)
				(setq record1 vector1[(++ index1)])
				(setq record2 vector2[(++ index2)])
				)) ; end equal case
			;; If the first record is #void, we must promote vector two.
			((= record1 #void)
				(begin
				(setq record2 vector2[(++ index2)])
				)) ; end promote vector two case
			;; If the second record is #void, we must promote vector one.
			((= record2 #void)
				(begin
				(setq record1 vector1[(++ index1)])
				)) ; end promote vector one case
			;; If the first record is low, we must promote vector one.
			((< record1 record2)
				(setq record1 vector1[(++ index1)])
				) ; end promote vector one case
			;; If the second record is low, we must promote vector two.
			(else
				(setq record2 vector2[(++ index2)])
				) ; end promote vector two case
			) ; end of cond
		) ;; end while
		result) ;; end of __viewAND
	(defun __viewOR(result vector1 vector2)
	;; *******************************************************************
	;; Summary:  Combines two view vectors using the logical OR operation.
	;; *******************************************************************
		vars:(time record1 record2 index1 index2)
		(setq vector1 (sort (copy vector1) <))
		(setq vector2 (sort (copy vector2) <))
		(setq record1 vector1[(setq index1 0)])
		(setq record2 vector2[(setq index2 0)])
		(while (or (<> record1 #void) (<> record2 #void)) do
			(cond ;; If records match only one may be returned.
			((= record1 record2)
				(begin
				(setq result[(length result)] record1)
				(setq record1 vector1[(++ index1)])
				(setq record2 vector2[(++ index2)])
				)) ; end equal case
			;; If the first record is #void, we must promote vector two.
			((= record1 #void)
				(begin
				(setq result[(length result)] record2)
				(setq record2 vector2[(++ index2)])
				)) ; end promote vector two case
			;; If the second record is #void, we must promote vector one.
			((= record2 #void)
				(begin
				(setq result[(length result)] record1)
				(setq record1 vector1[(++ index1)])
				)) ; end promote vector one case
			;; If the first record is low, we must promote vector one.
			((< record1 record2)
				(begin
				(setq result[(length result)] record1)
				(setq record1 vector1[(++ index1)])
				)) ; end promote vector one case
			;; If the second record is low, we must promote vector two.
			(else
				(begin
				(setq result[(length result)] record2)
				(setq record2 vector2[(++ index2)])
				)) ; end promote vector two case
			) ; end of cond
			) ;; end while
		result) ;; end of __viewOR
	(defun __viewXOR(result vector1 vector2)
	;; *******************************************************************
	;; Summary:  Combines two view vectors using the logical XOR operation.
	;; *******************************************************************
		vars:(time record1 record2 index1 index2)
		(setq vector1 (sort (copy vector1) <))
		(setq vector2 (sort (copy vector2) <))
		(setq record1 vector1[(setq index1 0)])
		(setq record2 vector2[(setq index2 0)])
		(while (or (<> record1 #void) (<> record2 #void)) do
			(cond ;; If records match neither may be returned.
			((= record1 record2)
				(begin
				(setq record1 vector1[(++ index1)])
				(setq record2 vector2[(++ index2)])
				)) ; end equal case
			;; If the first record is #void, we must promote vector two.
			((= record1 #void)
				(begin
				(setq result[(length result)] record2)
				(setq record2 vector2[(++ index2)])
				)) ; end promote vector two case
			;; If the second record is #void, we must promote vector one.
			((= record2 #void)
				(begin
				(setq result[(length result)] record1)
				(setq record1 vector1[(++ index1)])
				)) ; end promote vector one case
			;; If the first record is low, we must promote vector one.
			((< record1 record2)
				(begin
				(setq result[(length result)] record1)
				(setq record1 vector1[(++ index1)])
				)) ; end promote vector one case
			;; If the second record is low, we must promote vector two.
			(else
				(begin
				(setq result[(length result)] record2)
				(setq record2 vector2[(++ index2)])
				)) ; end promote vector two case
			) ; end of cond
			) ;; end while
		result) ;; end of __viewXOR
	;; *******************************************************************
	;; Begin MAIN logic section.
	;; Note: Initialize this memory table cursor.
	;; *******************************************************************
	Continue::
	(setq myCopy (copy (myself)))
	(setq myCopy.myParent myCopy)
	(if (= (type colNames) Brick:)
		(myCopy.attachBrick colNames)
		(myCopy.__open tableName colNames)
		) ; end if
	myCopy) ;; end of memoryCursor






































;;**EXPORTKEY**:browseLib.precompiler
(deforphan browseLib:precompiler(inSource)  
;; *******************************************************************
;; summary:  A pre-compiler for adding C-like pre-compiler directives
;;           to any source file. One note of caution, the pre-compiler 
;;           directives use Lisp S-expressions instead of tiny C 
;;           expressions.
;;
;; Notes:    Provides the following C-like precompiler directives
;;           to any source file. 
;;
;;           #define name	S-expression
;;           #ifdef S-expression
;;           #ifndef S-expression
;;           #if S-expression
;;			 #else
;;			 #endif			 
;;
;; Warning:  Each compiler directive MUST start at the very beginning 
;;           of the source line or it will NOT be recognized as a 
;;           pre-compiler directive.
;;
;; Args:     inSource:         	The input source to be precompiled.
;;
;; Return:   outSource			The resulting pre-compiled source code. 
;; *******************************************************************
	pvars:(;; Public Child Lambdas
			checkForPrecompilerDirectives;; Look for any pre-compiler directives in the source
			defineMgr                  	;; Manage "#define" pre-compiler directives
			elseMgr                    	;; Manage "#else" pre-compiler directives
			endIfMgr                   	;; Manage "#endif" pre-compiler directives
			ifMgr                      	;; Manage "#if" pre-compiler directives
			ifdefMgr                   	;; Manage "#ifdef" pre-compiler directives
			ifndefMgr                  	;; Manage "#ifndef" pre-compiler directives
			;; Private Variables
			conditionSW	              	;; The conditional switch for input source lines 
			conditionScan	          	;; The conditional directive we a re currently scanning for 
			lineCount	              	;; The number of input source lines 
			lineIndex	              	;; The index of the current input source line being scanned 
			lines		              		;; The vector of input source lines 
			outSource	              	;; The pre-compiled output source 
			passThruSW	              	;; The pass thru switch for input source lines 
			thisLine  	              	;; The the current input source line being scanned 
			) ; end persistant variables
	vars: (n N 
			) ; end temporary variables
	;;****************************************************************
	;; Define Public Child Lambdas
	;;****************************************************************
	;; Look for any pre-compiler directives in the source 
	(defun checkForPrecompilerDirectives(inString)
		regs:(CharPointer:p Integer:c Integer:m Integer:n Integer:N)
		(if (= inString #void) (return false))
		(setq p inString)
		(setq N (length inString))
		(loop for n from 0 until N do
			(setq c p[n])
			(if (= c #\#)
				(begin
					(setq m (+ n 1))
					(setq c p[m])
					(if (and (or (= c #\i) (= c #\I)) (stringCiEQ (mid inString n 3) "#if")) (goto OneFound:)) 
					(if (and (or (= c #\e) (= c #\E)) (stringCiEQ (mid inString n 6) "#endif")) (goto OneFound:)) 
					(if (and (or (= c #\d) (= c #\D)) (stringCiEQ (mid inString n 7) "#define")) (goto OneFound:)) 
				)) ; end # found 
			) ; end loop
		NoneFound::
		(return false)
		OneFound::
		true)
	;; Manage "#define" pre-compiler directives 
	(defun defineMgr() 
		(if (bcompareEQ passThruSW true)
			(eval (compile (lisp (append "(setq " (mid thisLine 8 100000000) ")"))))
			) ; end if
		true)
	;; Manage "#else" pre-compiler directives 
	(defun elseMgr()
		(if (bcompareEQ conditionSW false) (error "precompiler: unmatched #else directive")) 
		(if (<> conditionScan |else|:) (error "precompiler: nested #else directives NOT supported")) 
		(if (bcompareEQ passThruSW true) (setq passThruSW false) (setq passThruSW true))
		(setq conditionScan |endif|:) 
		true) ; end elseMgr
	;; Manage "#endif" pre-compiler directives 
	(defun endIfMgr()
		(if (bcompareEQ conditionSW false) (error "precompiler: unmatched #endif directive")) 
		(if (and (<> conditionScan |else|:) (<> conditionScan |endif|:)) (error "precompiler: nested #endif directives NOT supported")) 
		(setq passThruSW true)
		(setq conditionSW false) 
		(setq conditionScan |if|:) 
		true) ; end endIfMgr
	;; Manage "#if" pre-compiler directives 
	(defun ifMgr()
		vars:(c s n)
		(if (bcompareEQ conditionSW true) (error "precompiler: nested #if directives NOT supported"))
		(if (bcompareEQ (eval (compile (lisp (mid thisLine 4 100000000)))) true) 
			(setq  passThruSW true)
			(setq  passThruSW false)
			) ; end if
		(setq conditionSW true) 
		(setq conditionScan |else|:) 
		true) ; end ifMgr
	;; Manage "#ifdef" pre-compiler directives 
	(defun ifdefMgr()
		(if (bcompareEQ conditionSW true) (error "precompiler: nested #ifdef directives NOT supported")) 
		(if (<> (eval (compile (lisp (mid thisLine 7 100000000)))) #void) 
			(setq  passThruSW true)
			(setq  passThruSW false)
			) ; end if
		(setq conditionSW true) 
		(setq conditionScan |else|:) 
		true) ; end ifdefMgr
	;; Manage "#ifndef" pre-compiler directives 
	(defun ifndefMgr()
		(if (bcompareEQ conditionSW true) (error "precompiler: nested #ifdef directives NOT supported")) 
		(if (= (eval (compile (lisp (mid thisLine 8 100000000)))) #void) 
			(setq  passThruSW true)
			(setq  passThruSW false)
			) ; end if
		(setq conditionSW true) 
		(setq conditionScan |else|:) 
		true) ; end ifndefMgr 
		
	;;****************************************************************
	;; MAIN initialization section
	;;****************************************************************
	;; Make sure the input source is a String.
	(if (<> (isString inSource) true) (error "preCompiler: expecting a String as input"))

	; do one pass through inSource to see if any "potential" compiler directives exist" - skip
	; processing the file if none are found.
	(if (not (checkForPrecompilerDirectives inSource)) (return inSource))
	
	;; Set aside the maximum output length for the pre-compiled source code.
	(setq outSource (new Vector: byte: (addi (length inSource) 100000) 0))
	(setq passThruSW true)
	(setq conditionSW false)
	(setq conditionScan |if|:)
	(setq thisLine "")
	;; Convert the input source into its component source lines.
	;; Note: We must try the several types of line endings in each popular host OS.
	(setq inSource (substitute inSource (append #\return #\newline) #\return))
	(setq inSource (substitute inSource (append #\newline #\return) #\return))
	(setq inSource (substitute inSource (append #\newline "") #\return))
	(setq lines (stringToVector inSource #\return))
	;; Prepare to scan each input source line for precompiler directives.
	(setq lineCount (length lines))
	(loop for lineIndex from 0 until lineCount do
		;; Look for pre-compiler directives
		(setq thisLine (refVector lines lineIndex))
		;; Look for a possible pre-compiler directive at the beginning of the line ONLY.
		(if (ccompareNE (refString thisLine 0) #\#)
			then
			;; No directive? ...then we pass through iff the switch is turned on.
			(if (bcompareEQ passThruSW true) (appendWriteln outSource thisLine _eol (char 0)))
			else
			(begin
				(cond
					;; #define directive? 
					((= (left thisLine 8) "#define ") (defineMgr)) 
					;; #if directive? 
					((= (left thisLine 4) "#if ") (ifMgr)) 
					;; #ifdef directive? 
					((= (left thisLine 7) "#ifdef ") (ifdefMgr)) 
					;; #ifndef directive? 
					((= (left thisLine 8) "#ifndef ") (ifndefMgr)) 
					;; #else directive? 
					((= (left thisLine 5) "#else") (elseMgr)) 
					;; #endif directive? 
					((= (left thisLine 6) "#endif") (endIfMgr)) 
					;; No directive? ...then we pass through iff the switch is turned on. 
					((bcompareEQ passThruSW true) (appendWriteln outSource thisLine _eol (char 0))) 
				) ; end declarative cond
			)) ; end if
		) ; end source line scan loop
	;; Convert the final output from a Byte Vector back into a String.
	;; Note: We converted to a Byte Vector and used appendWriteln because,
	;;       even with the conversion cost, appendWriteln is much faster
	;;       than append when the character count is high.
	(string outSource)) ; end preCompiler

































;;**EXPORTKEY**:browseLib.profiler
(deforphan browseLib:profiler()  
;; *******************************************************************
;; summary:  The profiler Lambda defines data structures, child Lambdas
;; 			and lisp macros that together allow you to instrument
;;			an application written in any LambdaBase language.
;;			See the online documentation for an overview of how to use
;;			profiler.
;; Args:     None. Profiler is never executed directly.
;; Globals set by browseLib:
;;           None.
;; Return:   NA. Profiler is never executed directly.
;; *******************************************************************
	pvars:(;; Persistant variables
			tPosted         ;; Timing structures from all active Lambdas
			tcPosted        ;; Iiming structures from all active Lambdas
			clearTiming		;; Clear the timing structures from all active Lambdas
			showTiming		;; Collect and report the profiling information from all active Lambdas and local t and tc directories
			postTimes		;; Post profiling information from a sourceLambda into profiler.t and profiler.tc directories	
			)  ; end persistant variables


	(defun postTimes(sourceLambda keyPrefix keySuffix) ; invoked from the POSTTIMES macro
	;; Summary: The postTimes function is only called from the POSTTIMES macro. Never call it directly in your code. This Lambda
	;; recurses through the Pv strucuture of sourceLambda looking for child Lambdas of sourceLambda. It posts and profiling information
	;; found in these child Lambdas, and sourceLambda itself, into directories maintained in browseLib.profiler. The intent of postTimes
	;; is to capture the collected profiling information from an Lambda before it is garbage collected. Never use the POSTTIMES macro
	;; in an Lambda that is persistent. Use the POSTTIMES macro to collect profiling information from Lambdas that are instantiated and
	;; then released in your application.
	;; Args:
	;;	sourceLambda			An Lambda object from which collected timings are to be posted into browseLib.profiler.
	;;	keyPrefix			A string or symbol argument that will be prepended to the sourceLambda's binding when creating the savekey
	;;	keySufix			A string or symbol argument that will be appended to the sourceLambda's binding when creating the save key 
	;;	Note on the save key. Post times posts values from the sourceLambda and all sourceLambda child Lambdas into the profiler.t and 
	;;	profiler.tc directories based on a composite key comprised of keyPrefix+LambdaName+keySuffix.
	;; Returns: 		true
		vars:(i k st obj len len2 LambdaKey)
		;make sure t and tc directories are initialized in profiler
		(if (= tPosted #void) (setq tPosted (new Directory:)))
		(if (= tcPosted #void) (setq tcPosted (new Directory:)))
		;iterate through each child Lambda of source Lambda, posting any profiling information into the profiler.t and profiler.tc directories
		(setq len (length sourceLambda.Pv))
		(loop for i from 0 until len do
			(setq obj sourceLambda.Pv[i])
			(if (isLambda obj)
				(if (and 	(<> obj[In:].t #void) ;ignore Lambdas that do not have profiling information in them
							(<> (find (string sourceLambda[In:].Binding) (string obj[In:].Binding) 0) false) ;ignore Lambdas that are not root Lambdas or children
							)
					(begin
;					(writeln obj.myCursorType " " obj " " obj[In:].Binding " " obj[In:].tc )
					(setq LambdaKey (symbol (append (if (<> keyPrefix "") (append (string keyPrefix) "+") "") (string obj[In:].Binding) (if (<> keySuffix "") (append "+" (string keySuffix)) "") )  ))
					(if (= tPosted[LambdaKey] #void) (setq tPosted[LambdaKey] (new Vector:)))
					(setq st obj[In:].t)
					(setq len2 (length st))
					(loop for k from 0 until len2 do
						(setq tPosted[LambdaKey][k] (+ tPosted[LambdaKey][k] st[k]))
					);k
					(setq tcPosted[LambdaKey] (iadd tcPosted[LambdaKey] obj[In:].tc)) 
					end)
				);if
			);if
		);i
		true)

	(defun clearTiming()
	;; 	Summary		Clears all collected profiling information from active Lambdas. Use this call
	;;				before running your application.
	;; 	Args:		none
	;;	Returns:	true
		vars:( i Lambdas LambdaIn len LambdaKey lt ltc lenT lenK tVec tTot s)
		(setq tPosted (new Directory:))
		(setq tcPosted (new Directory:))
		(setq Lambdas (inspect objectList:)); get an object vector containing all active Lambdas
		(loop for i from 0 until (length Lambdas) do
			(if (isLambda Lambdas[i]) 
				(begin
				(setq LambdaIn Lambdas[i][In:])
				(if (and (<> LambdaIn.tc #void) (<> LambdaIn.Binding #void))
					(begin 
					(setq LambdaIn.t #void)
					(setq LambdaIn.tc #void)
					end)
				);if  
				end)
			);if
		);i
		true);end of clearTiming
	
	(defun showTiming()
	;;	Summary		Reports on all collected profiling information from active Lambdas.
	;;	Args:		None
	;;	Returns:	true
		vars:( i Lambdas LambdaIn len LambdaKey lt ltc lenT lenK tVec tTot s)
		(if (= tPosted #void) (setq tPosted (new Directory:)))
		(if (= tcPosted #void) (setq tcPosted (new Directory:)))
		(setq lt (copy tPosted))
		(setq ltc (copy tcPosted))
		
		(setq Lambdas (inspect objectList:)); get an object vector containing all active Lambdas
		(loop for i from 0 until (length Lambdas) do
			(if (isLambda Lambdas[i]) 
				(begin
				(setq LambdaIn Lambdas[i][In:])
				(if (and 	(<> LambdaIn.tc #void) 
							(<> LambdaIn.Binding #void)
							(<> obj[In:].postTimeSW true) ;ignore Lambda if it uses posttime (avoid double counting)
							)
						(begin 
						(setq LambdaKey (symbol (append (string LambdaIn.Binding))))
						(setq lt[LambdaKey] LambdaIn.t)
						(setq ltc[LambdaKey] LambdaIn.tc)
						end)
				);if  
				end)
			);if
		);i
			
		(setq lenT (length lt))
		(loop for i from 0 until lenT do
			(setq tVec lt[i 1]); vector of timings withing an Lambda. ie: multiple calls to SETTIME in an Lambda
			(setq lenK (length tVec))
			;Get total time in Lambda and construct string of times for display
			(setq s "")
			(setq tTot 0.0)
			(loop for k from 0 until lenK do
				(+= tTot tVec[k])
				(setq s (append s 			(right (+ "               " (text tVec[k] "###,###,###.##") ) 15)))
			);k
				
			(if (= lenK 1) (setq s "")) ;don't print the tVec if we have only a single timing in the Lambda
			
			(writeln
			(left (+ (string lt[i 0])   "                                                                            ") 60)
			(right (+ "               " (text ltc[i 1] "###,###,###") ) 12)
			(right (+ "               " (text tTot "###,###,###.##") ) 15)
			(right (+ "               " (text (if (<> ltc[i 1] 0) (/ tTot ltc[i 1]) 0) "###,###.####")) 12)
			s
			);display
		);i
		
		true)
	true);end of profiler


;;*************************************
;;*************************************
;; Exported Lambda File Cabinet Document
;;*************************************
;;*************************************


































;;**EXPORTKEY**:browseLib.scanFile
(deforphan browseLib:scanFile(fileName)
	vars:(
	fileID
	source)
	(setq fileID (fileOpen fileName 0 0) ) ;; Open a pre-existing script file
	(setq source (fileRead fileID))
	(fileClose fileID 1) ;; Close the file
	(scanSource source)
true)






























;;**EXPORTKEY**:browseLib.scanSource
(deforphan browseLib:scanSource(source)
	vars:(
	cp
	c
	len
	line
	newlineFlag
	)
	;; Find all non printable characters in source.
	(setq newlineFlag false)
	(setq len (length source))
	(setq line "")
	(loop for cp from 0 until len do
		(setq c source[cp])
		(if (and (<> c 9) (or (< c 32) (> c 127))) (begin
			(setq newlineFlag true)
			(setq line (append line "|" (integer c) "|"))
			)
		else (begin
			(if newlineFlag (begin
				(setq newlineFlag false)
				(writeln line)
				(setq line "")
				))
			(setq line (append line c))
			))
	) ; cp
true)

;;*************************************
;;*************************************
;; Exported Lambda File Cabinet Document
;;*************************************
;;*************************************































;;**EXPORTKEY**:browseLib.showGlobals
;; showGlobals uses the disassemble function to 
;; find global references in compile Lambdas.
;; To use showGlobals do something like the following:
;; (browseLib.showGlobals myLambda)
(deforphan browseLib:showGlobals(Lambda)
	pvars:(
		Lambdas
		_showGlobals
		_showLambda
		excludeList
		)
	(setq excludeList (stringToVector "_eol _path _ais _ais.browseLibExtents end" #\space))

	(setq Lambdas (new Vector:))
	(writeln "Parsing " Lambda.In.Binding)
	(_showGlobals Lambda) 
	(_showLambda Lambda Lambda.In.Binding)

(defun _showLambda(Lambda LambdaName)
	vars:(n N pv childname)
	(setq pv Lambda.Pv)
	(setq Lambdas[(length Lambdas)] Lambda)


	(setq N (length pv));
	(loop for n from 0 until N do
		(if (and (isLambda pv[n]) ;it is an Lambda
				(not (isInside pv[n] Lambdas))) ;we have not visited it before
			(if (<> pv[n].Pv pv) ; and its not a child Lambda. It may be a friend, orphan etc.
				(begin
				(setq childname (append LambdaName "." pv[n 0]))
				(writeln "Parsing " LambdaName)
				(_showLambda pv[n] childname)
				)
			else
				(begin
				(setq childname (append LambdaName "." pv[n 0]))
				(writeln "Parsing " childname)
				(_showGlobals pv[n])
				)))
	);n
	true)

(defun _showGlobals(Lambda)
	vars:(n n2 n3 n4 name N src c tokenList)
	(setq tokenList (new Directory:))
	;; Examine code vector for global references
	(setq src (disassemble Lambda))
	(setq N (length src))
	(loop for n from 0 until N do
		(setq c (character src[n]))
		(if (compareEQ (character src[n]) "G")
			(if (and (< (setq n2 (+ n 1)) N) (compareEQ (character src[n2]) "v"))
				(begin
				(if (and (< (setq n3 (+ n 2)) N) (compareEQ (character src[n3]) "["))
					(begin
					(setq n4 (+ n3 1))
					(while (and (< n4 N) (compareNE src[n4] "]")) 
						(setq n4 (+ n4 1)))
					(setq name (substring src (+ n3 1) (- n4 1)))
					(if (and (not (isLambda (getGlobalValue (symbol name))))
							(not (isFunction (getGlobalValue (symbol name))))
							(not (isInside name excludeList)))
						(begin
						(setq m (member name tokenList))
						(if (= m false)
							(setq tokenList[name] 1)
							(setq tokenList[name] (+ tokenList[name] 1)))
						))
					(setq n n4)
					end)))))
	);n
	(setq N (length tokenList))
	(loop for n from 0 until N do
		(writeln "   " tokenList[n 0] " " tokenList[n 1]))
	true);end _showGlobals
true)








