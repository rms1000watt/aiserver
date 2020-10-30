;;*************************************
;;*************************************
;; Exported Agent File Cabinet Document
;;*************************************
;;*************************************

;;**EXPORTKEY**:config
;; *******************************************************************
;;  
;;  Title:    DeepGreen Configuration Script
;;
;;  Author:   Tim May
;;
;;  Notes:    No other dependencies.  
;; 
;;  Copyright c 2004, InvestByAgent.com
;;  Private and proprietary. All rights reserved.
;; *******************************************************************

;; *******************************************************************
;; config
;; Helper agent for reloading and exporting the environment and application.
;; This function is completly generic and should contain no applciation specific
;; references. 
;; This agent manages the creation, importation and compiliation of agents for
;; the appliation environment and applciation suite.
;; *******************************************************************
;; Note: browseLib must be loaded prior to this agent being run.
(defun config()
	pvars:(
	CabList				;; Dictionary of cabinets
	CoreCabinets 		;; Vector of core cabinets that need to be loaded before other application agents
	BadImports			;; Dictionary of bad imports if any

	;; Public agents
	addDataExtent       ;; Register an extent in the CabList with defaults
	addExtent			;; Register an extent in the CabList with options

	exportCore			;; export core extents that have a single source file path
	exportApp			;; export applciatione extents that have a single source path

	reloadCore      	;; Reload and compile those agents in the CoreCabinets list
	reloadApp           ;; Reload and compile those agents in the CabList
	
	reportBadImports	;; print out a list of bad imports if any
	
	showCabList			;; Print out CabList and CoreCabinets report

	fileExists			;; Check if a specified file exists
	
	;; Private agents
	_init				;; initialize agent

	);pvars     

	;; *******************************************************************
	;; _init - conditionally initialize agent pvars
	;; ******************************************************************* 
	(defun _init()
	 	(if (= CabList #void) (setq CabList (new Dictionary:)))
	 	(if (= CoreCabinets #void) (setq CoreCabinets (new Vector: object:)))
	 	(if (= BadImports #void) (setq BadImports (new Dictionary:)))
	)

	(defun showCabList()
		vars:(i len)
		(setq len (length CabList))
		(writeln "======= Cabinets List ==========")	
		(loop for i from 0 until len do
			(writeln CabList[i 0] "  " CabList[i 1])
		);i
		(setq len (length CoreCabinets))
		(writeln "Core Cabinets")
		(loop for i from 0 until len do
			(writeln CoreCabinets[i])
		);i
		(writeln "======== END REPORT ============")	
	true)

	
	;; *******************************************************************
	;; addExtent
	;; ******************************************************************* 
	;; addExtent registers the specified cabinet in the load list
	;; and adds the extent to those managed by browseLib.
	;; Args
	;; searchSW 	- "true" indicates the cabinets should be searched for an agent by
	;;				browseLib when a checkout is performed without the user specifying a cabinet.
	;; compileSW	- "true" indicates the cabinet should be compiled on reload
	;; coreSW		- "true" indices the cabinet should be added to the CoreCabinets vector
	;;					Note that order of core cabinets is maintained so call addExent, for 
	;;					core cabinets, in the order you want them compiled.
	;; cabinetKey 	- The unique symbol that the cabinet is known by.
	;; dbPath		- The path to the extent file.
	;; srcPath		- Path specifications for agent source file to be imported.
	;; dependName   - (Optional) None or more cabinet names upon which this cabinet depends.
    ;;
    ;;                       cabName    CabRepo    CabSource         impt    expt   acomp searchSW  dependencies)
    ;;(browseLib.addExtent libs[i][0] libs[i][1] libs[i][3] "file" "none" "none" "true" searchSW)
	(defun addExtent (searchSW compileSW coreSW cabinetKey dbPath srcPath ...)
		vars:(i dependName)
		(_init)
		(setq CabList[cabinetKey] (new Structure: searchSW: searchSW compileSW: compileSW dbPath: dbPath srcPath: srcPath))
        (if (> (argCount) 6) 
            then 
            (begin 
              (setq dependName (symbol (argFetch 6)))
		      ;(browseLib.addExtent cabinetKey dbPath searchSW dependName) ;; Registers the extent with browseLib and with dependencies
		      (browseLib.addExtent cabinetKey dbPath srcPath "file" "none" "none" (string compileSW) searchSW dependName) ;; Registers the extent with browseLib and with dependencies
            ) else 
		    ;(browseLib.addExtent cabinetKey dbPath searchSW) ;; Registers the extent with browseLib and NO dependencies
		    (browseLib.addExtent cabinetKey dbPath srcPath "file" "none" "none" (string compileSW) searchSW) ;; Registers the extent with browseLib and NO dependencies
            ) ; end if
		(if (= compileSW true) (begin
			(browseLib.compileAll true) ;; load binary (compiled agent) into memory if it is available
			end))
		(if (= coreSW true) (begin
			(setq CoreCabinets[(length CoreCabinets)] cabinetKey);; see reloadCore for usage
			end))
			
	); addExtent
	
	;; *******************************************************************
	;; addDataExtent
	;; ******************************************************************* 
	;; addDataExtent registers the specified cabinet in the load list
	;; and adds the extent to the data exents managed by browseLib.
	;; Args
	;; cabinetKey 	- The unique symbol that the cabinet is known by.
	;; dbPath		- The path to the extent file.
	;; srcPath		- Path specifications for agent source files to be imported.
	;; Note: addDataExtent does the same job as addExtent. It just has default values of false for searchSW and 
	;; true for compileSW. See the browseLib.addDataExtent child agent for more details.
	(defun addDataExtent (cabinetKey dbPath srcPath)
	    vars:(i numberOfSrcPaths srcPath)
	    (_init)
		(setq CabList[cabinetKey] (new Structure: searchSW: false compileSW: true dbPath: dbPath srcPath: srcPath))
		(browseLib.addDataExtent cabinetKey dbPath)
	); addDataExtent


	;; *******************************************************************
	;; exportCore
	;; ******************************************************************* 
	;; exportCore exports those cabinets specified in the CoreCabinets vector
	(defun exportCore()
		vars:(i len)
		(_init)
		(setq len (length CoreCabinets))
		(loop for i from 0 until len do
			(browseLib.setFocus CoreCabinets[i])  
			(if (> (length (browseLib.getChildNames)) 0) (begin ;; export only if cabinet has content
				(if (> (length CabList[CoreCabinets[i]].srcPath) 0) (begin ; export only if we know where to export too
					(writeln "Exporting " CoreCabinets[i] " to " CabList[CoreCabinets[i]].srcPath)
					(browseLib.exportSource (append _path CabList[CoreCabinets[i]].srcPath) "..all agents..")
					end))
				end))
		); i
	
	true); exportCore


	;; *******************************************************************
	;; exportApp
	;; ******************************************************************* 
    ;; exportApp exports those cabinets specified in the CabList excluding those in the CoreCabinets vector
	(defun exportApp()
		vars:(i len )
		(_init)
		(setq len (length CabList))
		(loop for i from 0 until len do 
			(if (isMember CabList[i 0] CoreCabinets) (goto SKIP:))
			(browseLib.setFocus CabList[i 0])
			(if (> (length (browseLib.getChildNames)) 0) (begin ;; export only if cabinet has content
				(if (> (length CabList[i 1].srcPath) 0) (begin ;; export only if we know where to export too
					(writeln "Exporting " CabList[i 0] " to " CabList[i 1].srcPath)
					(browseLib.exportSource (append _path CabList[i 1].srcPath) "..all agents..")
					end))
	  			end))
			SKIP::
		); end i
	true); exportApp


	;; *******************************************************************
	;; reloadCore
	;; ******************************************************************* 
	;; reloadCore imports and compiles those cabinets specified in the CoreCabinets 
	;; persistent variable.
    ;; Args:
    ;;	clearSW	- the symbol clear: can be passed to cause the existing extents to be cleared prior to reload.
	(defun reloadCore(...)
		vars:(i len clearSW cabKey srcPath fullScrPath)
		(_init)
		(setq clearSW (and (= (argCount) 1) (= (argFetch 0) clear:)))
		(setq len (length CoreCabinets))
		(loop for i from 0 until len do
			(setq cabKey CoreCabinets[i])
			(setq srcPath CabList[cabKey].srcPath)
			(setq fullSrcPath (append _path srcPath))
			(browseLib.setFocus cabKey)  
			(if clearSW (browseLib.eraseChildren "..all agents.."))
			(if (= (length (browseLib.getChildNames)) 0) (begin ;; import and compile only if cabinet is empty
				(if (> (length CabList[cabKey].srcPath) 0) (begin
					(if (= (existsOSFile fullSrcPath) 1) (begin
						(writeln "Importing into " cabKey " from " srcPath)
						(browseLib.importSource fullSrcPath)
						(if CabList[cabKey].compileSW (begin
							(writeln "Compiling " cabKey)
							(browseLib.compileAll true)
							end))
						end)
					else (begin
					    (writeln "Import file " srcPath"  for " cabKey " not found!")
					    (setq BadImports[cabKey] srcPath)
						end))
					end))
				end))
		); i
	true); reloadCore
	
	;; *******************************************************************
	;; reloadApp 
	;; ******************************************************************* 
    ;; reloadApp imports and compiles those cabinets specified in the load lists
    ;; This excludes those already processed in the CoreCabinets list.
    ;; Args:
    ;;	clearSW	- the symbol clear: can be passed to cause the existing extents to be cleared prior to reload.
	(defun reloadApp(...)
		vars:(i len clearSW cabKey srcPath fullSrcPath)
		(_init)
		(setq clearSW (and (= (argCount) 1) (= (argFetch 0) clear:)))
 		(setq len (length CabList))
		(loop for i from 0 until len do               
			(setq cabKey CabList[i 0])
			(if (isMember cabKey CoreCabinets) (goto SKIP:))
			(setq srcPath CabList[cabKey].srcPath)
			(setq fullSrcPath (append _path srcPath))
			(browseLib.setFocus cabKey)
			(if clearSW (browseLib.eraseChildren "..all agents.."))
			(if (= (length (browseLib.getChildNames)) 0) (begin ;; import and compile only if cabinet is empty
				(if (> (length srcPath) 0) (begin
				  	(if (= (existsOSFile fullSrcPath) 1) (begin
						(writeln "Importing into " cabKey " from " srcPath)
						(browseLib.importSource fullSrcPath)
						(if CabList[i 1].compileSW (begin
							(writeln "Compiling " cabKey)
							(browseLib.compileAll true)
							end))
						end)
					else (begin
					    (writeln "Import file " srcPath"  for " cabKey " not found!")
					    (setq BadImports[cabKey] srcPath)
						end))						
					end))
	  			end))
			SKIP::
		); end i
	true); reloadApp

	(defun reportBadImports()                          
		(if (> (length BadImports) 0)
			(writeln "Config agent reports the following bad imports:"))
	    (loop for i from 0 until (length BadImports) do
	     	(writeln BadImports[i 0] " " BadImports[i 1])
	    )
	true)


	(defun fileExists (path)
		vars: (f)
		(setq f (^new browseLib.fileInfo path))
		(f.exists) ;; Return true if file exists
	)


); config


