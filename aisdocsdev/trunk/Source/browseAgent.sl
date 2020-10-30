;;*************************************
;;*************************************
;; Exported Agent File Cabinet Document
;;*************************************
;;*************************************

;;**EXPORTKEY**:browseAgent
(defun browseAgent(extentName extentFileName)
;; *******************************************************************
;; summary:  Manages all Agent source code stored in the file cabinet.
;;           Includes checking source in and out of the file cabinet,
;;           importing, exporting, and deleting source from the file
;;           cabinet.
;; Args:     extentName:          Name of the new file cabinet repository extent.
;;           extentFileName:      Path and file name of new file cabinet repository extent, or
;;                                 the extent manager agent for managing the foreign extent.
;; Globals set by browseAgent:
;;           _browseAgentExtents  The current browseAgent extent structure.
;; Return:   true
;; *******************************************************************
    vars:(name s i n commands keys mainExtent altExtent)
    pvars:(;; Persistent Variables
           dString                 ;; Delimited string result
           _compileName            ;; Name of agent being compiled
           htmlPageRegistry        ;; HTML page registry (see htmlPageServer)
           (maxLine 140)           ;; Maximum delimited string line length
           myImportFileID          ;; File ID during file cabinet import from exported source
           myImportBuffer          ;; File buffer during file cabinet import from exported source
           myImportName            ;; Next agent name during file cabinet import from exported source
           ;; Public Child Agents
           abortAll                ;; Close, with abort, all extents in File Cabinet
           addDataExtent           ;; Register a data file cabinet extent to the browse agent
           addExtent               ;; Register an agent file cabinet extent to the browse agent
           addHostTextExtent       ;; Register a host text file folder extent to the browse agent
           addMemoryExtent         ;; Register the memory manager extent to the browse agent
           checkin                 ;; Check an agent script into the File Cabinet
           checkout                ;; Check an agent script out of the File Cabinet
           checkoutAgent	       ;; Check an agent binary out of the File Cabinet
           checkoutParent          ;; Check a whole parent agent script including all its children
           clearCabinet            ;; Clear the specified File Cabinet of all data    
           closeImportSourceFile   ;; Close import source file.
           commitAll               ;; Close, with commit, all extents in File Cabinet
           compileAll              ;; Compile all agents in File Cabinet
           compileSource           ;; Manage source compilation
           copyCabinetSource       ;; Copy the specified source File Cabinet contents to the specified destination File Cabinet   
           delimitedGlobals        ;; Convert globals to a delimited string
           delimitedString         ;; Convert object to a delimited string
           dir						;; dir agent surfaces the QDir object
           dropExtent              ;; Unregister a file cabinet extent from the browse agent
           eraseChildren           ;; Manage source erasing including all related child agents
           eraseSource             ;; Manage source erasing
           errorHandler            ;; Handle errors gracefully
           exportSource            ;; Manage source exporting 
           fileInfo					;; fileInfo agent surfaces the QFileInfo object
           getChildNames           ;; Return a vector of all source names 
           getExtentCount          ;; Return the count of file cabinet extents 
           getExtentNames          ;; Return a vector of extent names 
           getFocus                ;; Return the name of the extent currently in focus 
           getFocusIndex           ;; Return the index of the extent currently in focus 
           getKeys                 ;; Return a vector of source names
           getNextLevel            ;; Return an XPath directory string list for the next repository directory level.
           getParentNames          ;; Return a vector of parent agent names 
           getTypes                ;; Return an XPath directory type list string for all possible types in a hierarchical descendents list 
           htmlPageServer          ;; Return the name of an html page for downloading
           htmlParmToStructure     ;; Convert the HTML parameter string to a structure
           importSmallSource       ;; Manage source importing for small files 
           importSource            ;; Manage source importing for files of all sizes
           inspect                 ;; Return a Structure of symbols and their closure size
           loadApplication		   ;; Loads the application from the specified cabinet
           openImportSourceFile    ;; Open source file for import 
           profiler                ;; Profiler for adding timing/profiling/statistical directives to active agents 
           precompiler             ;; Precompiler for adding C-like precompiler directives to source files 
           readImportSourceRecord  ;; Read next import source file record 
           readSourceFile          ;; Read source file 
           removeDelimiters        ;; Remove all delimiters from a string 
           saveApplication		   ;; Saves the current application in the specified cabinet
           scanFile				   ;; Scans the source code in the specified file
           scanSource			   ;; Scans the specified source code string
           setFocus                ;; Set the specified extent in focus  
           showGlobals			   ;; Finds all global references in the specified agent
           stripAll                ;; Remove the source from every agent in the current file cabinet
           systemCheck             ;; Perform a complete inspection of each registered repository extent
           tabbedNameList          ;; Return a tab delimted string of agent script names
           takeout                 ;; Take an agent out of the File Cabinet
           writeSourceFile         ;; Write source file 
           ;; Private Child Agents
           _dataManagerTemplate    ;; Factory for creating standard data manager agents.
           _exportSource           ;; Manage source exporting 
           _extentManagerTemplate  ;; Factory for creating standard extent manager agents.
           _hostTextManagerTemplate;; Factory for creating host text file folder extent manager agents.
           _memoryManagerTemplate  ;; Factory for creating standard memory manager agents.
           _initialize             ;; Initialize the browseAgent (if necessary) 
           _sysErrorStop           ;; Recover of all system errors.
           ) ;; end of pvars
    ;; Never initialize this Agent more than once, because the
    ;; inline child agents will overlay the cloned child agents
    ;; in any clone copies of the Agent and this causes serious
    ;; confusion when the reinitialized clone begins to affect
    ;; the persistant storage of the original Agent.
    ;; *******************************************************************
    ;; Define Private Child Agents
    ;; *******************************************************************
    ;;  scripts in the agent file cabinet to a source file.
    (defun _exportSource(fileID command)
        vars:(name agentName agentSource s i n
              startIndex endIndex
              parentName parentLen
              childName childLen
              file brk focus keys
              ) ;; end of temporary variables
        ;; Check for a request to export "all" agents.
        (if (= command "") (setq command "..all agents.."))
        (if (= command "..all agents..") (setq agentName "") (setq agentName (string command)))        
        ;; Compute the parent agent name and length.
        ;; If the name contains an imbedded colon (:) symbol,
        ;; then we keep only the parent agent part of the name
        ;; preceeding the colon (:) symbol.
        (setq n (find ":" agentName))
        (if (isNumber n)
            (setq parentName (left agentName n))
            (setq parentName agentName))
        (setq parentLen (length parentName))
        ;; Compute the parent agent name and length.
        (if (= parentName "")
            (setq childName "")         
            (setq childName (append "" parentName ":")))
        (setq childLen (length childName))
        ;; Position the start and end index at the agent to be exported.
        (if (= command "..all agents..")
            ;; We are exporting all agents. 
            (begin
               (setq startIndex 0)
               (setq endIndex (_browseAgentExtents.Focus.refLength))
               ) ;; end of then
            ;; We are exporting one parent and its child agents. 
            (begin
               (setq endIndex (_browseAgentExtents.Focus.refLength))
		       (loop for startIndex from 0 until endIndex do
		           ;; Quit if this agent is not to be exported.
		           (setq name (_browseAgentExtents.Focus.refKeyByIndex startIndex))
		           (if (or (= parentName name) (= childName (left name childLen))) (goto FoundStartIndex:))
		           ) ; end loop
               FoundStartIndex::
               ) ;; end of else
            ) ;; end of if
        ;; Write each agent script to the export file.
        (_browseAgentExtents.Focus.beginTransaction) 
        (loop for i from startIndex until endIndex do
            ;; Quit if this agent is not to be exported.
            (setq name (_browseAgentExtents.Focus.refKeyByIndex i))
            (if (and (<> command "..all agents..")
                     (<> parentName name)
                     (<> childName (left name childLen)))
                (goto CloseFileCabinet:)
                ) ;; end if
            ;; Export this agent.
            (fwriteln fileID _eol ";;**EXPORTKEY**:" name)
            (setq agentSource (_browseAgentExtents.Focus.refSourceExByIndex i))
            (fwriteln fileID agentSource)
            ) ; end loop
        CloseFileCabinet::
        (_browseAgentExtents.Focus.commitTransaction) 
        true) ;; end _exportSource
    ;; Initialize the browseAgent (if necessary)
    (defun _initialize()
        vars:(n N extMgr extentName)
        (if (= _browseAgentExtents #void)
            (begin
              (setq _browseAgentExtents (new Structure: Focus: #void FocusIndex: -1 Precompiler: precompiler Extents: (new Dictionary:)))
              (addMemoryExtent)
              )) ; end if       
        (if (<> _browseAgentExtents.FocusIndex -1) (setq _browseAgentExtents.Focus _browseAgentExtents.Extents[_browseAgentExtents.FocusIndex]))
        true) ; end of _initialize
    ;; Recover from any system errors which occur.
    (defun _sysErrorStop(errCode) (append dString " !sysErr#" errCode))
    ;; *******************************************************************
    ;; Define Public Child Agents
    ;; *******************************************************************
    ;; Close, with abort, all extents in File Cabinet
    (defun abortAll()
        vars:(n N)
        (setq N (length _browseAgentExtents.Extents))
        (loop for n from 0 until N do
           (_browseAgentExtents.Extents[n 1].abortTransaction)
           ) ; end loop
        true) ;; end of abortAll
    ;; Register a data file cabinet extent to the browse agent
    (defun addDataExtent(extentName extentFileName)
        vars:(objRepo extMgr)
        (_initialize)
        (setq extentName (string extentName))
        (if (not (isAgent extentFileName))
            (begin
               (setq objRepo (new ObjectRepository: extentFileName))
               (setq extMgr (copy _dataManagerTemplate))
               (setq extMgr.myRepository objRepo)
               (setq extMgr.myExtentName extentName)
               ) ; end then
            else
            (setq extMgr extentFileName)
            ) ; end if
        (setq _browseAgentExtents.Extents[extentName] extMgr)
        (setq _browseAgentExtents.Focus extMgr)
        (setq _browseAgentExtents.FocusIndex (member extentName _browseAgentExtents.Extents))
        true) ; end of addDataExtent
    ;; Register an agent file cabinet extent to the browse agent
    (defun addExtent(extentName extentFileName ...)
        vars:(objRepo extMgr searchSW)
        (_initialize)
        (if (>= (argCount) 3) (setq searchSW (argFetch 2)))
        (setq extentName (string extentName))
        (if (not (isAgent extentFileName))
            (begin
               (setq objRepo (new ObjectRepository: extentFileName))
               (setq extMgr (copy _extentManagerTemplate))
               (setq extMgr.myRepository objRepo)
               (setq extMgr.myExtentName extentName)
               ) ; end then
            else
            (setq extMgr extentFileName)
            ) ; end if
        (if (= searchSW true) (setq extMgr.Pv.mySearchSW searchSW))
        (setq _browseAgentExtents.Extents[extentName] extMgr)
        (setq _browseAgentExtents.Focus extMgr)
        (setq _browseAgentExtents.FocusIndex (member extentName _browseAgentExtents.Extents))
        true) ; end of addExtent
    ;; Register a host text file folder extent to the browse agent
    (defun addHostTextExtent(extentName folderName fileSuffix searchSW compileSW)
        vars:(extMgr)
        (_initialize)
        (setq extentName (string extentName))
        (setq extMgr (copy _hostTextManagerTemplate))
        (extMgr extentName folderName fileSuffix)
        (setq extMgr.Pv.mySearchSW searchSW)
        (setq _browseAgentExtents.Extents[extentName] extMgr)
        (setq _browseAgentExtents.Focus extMgr)
        (setq _browseAgentExtents.FocusIndex (member extentName _browseAgentExtents.Extents))
        (if (= compileSW true) (compileAll true))
        true) ; end of addHostTextExtent
    ;; Register the memory manager extent to the browse agent
    (defun addMemoryExtent()
        vars:(objRepo extMgr)
        (_initialize)
        (setq extentName ".Memory")
        (setq extMgr (copy _memoryManagerTemplate))
        (setq extMgr.myRepository (getSymbolTable 1 1 1))
        (setq extMgr.myExtentName extentName)
        (setq _browseAgentExtents.Extents[extentName] extMgr)
        (setq _browseAgentExtents.Focus extMgr)
        (setq _browseAgentExtents.FocusIndex (member extentName _browseAgentExtents.Extents))
        true) ; end of addMemoryExtent
    ;; Bind all unbound global agents based upon their global bindings
    ;; Note: Also binds the child agents of any unbound global agents
    (defun bind(...)
       regs:(n N)
       vars:(symbolList theAgent childAgent childName agentName)
       ;; Are we starting out with no arguments?
       (if (= (argCount) 0)
           (begin
              (setq symbolList (getSymbolTable 0 0 1))
              (setq N (length symbolList))
              (loop for n from 0 until N do
                 (setq theAgent (getGlobalValue (setq agentName symbolList[n])))
                 (if (and (isAgent theAgent) (<> agentName _currentResult:) (= theAgent.In.Binding #void))
                     (bind theAgent agentName)
                     ) ; end if
                 ) ; end loop
              (goto Last:)
           )) ; end if 
       ;; Are we starting out with two arguments?
       (if (= (argCount) 2)
           (begin
              (setq theAgent (argFetch 0))
              (setq agentName (argFetch 1))
		      (if (not (isStructure theAgent.In)) (setq theAgent.In (new Structure:)))
		      (setq theAgent.In.Binding agentName)
              (setq N (length theAgent.Pv))
		      (loop for n from 0 until N do
		         (setq childAgent theAgent.Pv[n 1])
		         (setq childName theAgent.Pv[n 0])
                 (if (and (isAgent childAgent) (= childAgent.In.Binding #void))
                     (bind childAgent (append agentName ":" childName))
                     ) ; end if
		         ) ; end Pv loop
		      (loop for n from 0 until (length theAgent.Cv) do
		         (setq childAgent theAgent.Cv[n 1])
		         (setq childName theAgent.Cv[n 0])
                 (if (and (isAgent childAgent) (= childAgent.In.Binding #void))
                     (bind childAgent (append agentName ":" childName))
                     ) ; end if
		         ) ; end Cv loop
              (goto Last:)
           )) ; end if 
       ;; Anything else is an error
       (error "browseAgent.bind: invalid arglist")
       ;; Return after binding all unbound agents
	   Last::
       true) ; end of bind
    ;; Check an agent script into the File Cabinet.
    (defun checkin(...)
        vars:(agentName newAgent cabinetName cabinetIndex)
        (_initialize)
        (if (= (argCount) 2) (begin (setq agentName (string (argFetch 0))) (setq newAgent (argFetch 1))))
        (if (= (argCount) 3) (begin (setq cabinetName (string (argFetch 0))) (setq agentName (string (argFetch 1))) (setq newAgent (argFetch 2))))
        (if (<> cabinetName #void) (setq cabinetIndex (member cabinetName _browseAgentExtents.Extents)))
        ;; Do not check in anything but a script or an agent.
        ;(if (not (or (isString newAgent) (isAgent newAgent))) (error "browseAgent.checkin requires either a script or an agent"))
        ;; If no cabinet name specified,  check in to the file cabinet in focus.
        (if (isNumber cabinetIndex) 
            (_browseAgentExtents.Extents[cabinetIndex].setAgentByKey agentName newAgent)
            (_browseAgentExtents.Focus.setAgentByKey agentName newAgent)
            ) ; end if
        true) ; end of checkin
    ;; Check an agent script out of the File Cabinet.
    (defun checkout(...) 
       vars:(result i agentName cabinetName cabinetIndex)
       (_initialize)
       (if (= (argCount) 1) (setq agentName (string (argFetch 0))))
       (if (= (argCount) 2) (begin (setq cabinetName (string (argFetch 0))) (setq agentName (string (argFetch 1)))))
       (if (<> cabinetName #void) (setq cabinetIndex (member cabinetName _browseAgentExtents.Extents)))
       (if (isNumber cabinetIndex) (return (_browseAgentExtents.Extents[cabinetIndex].refSourceByKey agentName)))
       ;; Perform general search of all file cabinet extents.
       (setq result (_browseAgentExtents.Focus.refSourceByKey agentName))
       (if (<> result #void) (return result))
       (loop for i from 0 until (length _browseAgentExtents.Extents) do
           ;; Search only those extents which have their auto-search switch turned on.
           (if (= _browseAgentExtents.Extents[i 1].Pv.mySearchSW true)
               (setq result (_browseAgentExtents.Extents[i 1].refSourceByKey agentName))
               ) ; end if
           (if (<> result #void) (return result))
           ) ; end loop
       result) ;; end of checkout
    ;; Check an agent binary out of the File Cabinet.
    (defun checkoutAgent(...) 
       vars:(result i agentName cabinetName cabinetIndex)
       (_initialize)
       (if (= (argCount) 1) (setq agentName (string (argFetch 0))))
       (if (= (argCount) 2) (begin (setq cabinetName (string (argFetch 0))) (setq agentName (string (argFetch 1)))))
       (if (<> cabinetName #void) (setq cabinetIndex (member cabinetName _browseAgentExtents.Extents)))
       (if (isNumber cabinetIndex) (return (_browseAgentExtents.Extents[cabinetIndex].refAgentByKey agentName)))
       ;; Perform general search of all file cabinet extents.
       (setq result (_browseAgentExtents.Focus.refAgentByKey agentName))
       (if (<> result #void) (return result))
       (loop for i from 0 until (length _browseAgentExtents.Extents) do
           ;; Search only those extents which have their auto-search switch turned on.
           (if (= _browseAgentExtents.Extents[i 1].Pv.mySearchSW true)
               (setq result (_browseAgentExtents.Extents[i 1].refAgentByKey agentName))
               ) ; end if
           (if (<> result #void) (return result))
           ) ; end loop
       result) ;; end of checkoutAgent
    ;; Compile the specified agent script.
    (defun checkoutParent(...)
    ;; *******************************************************************
    ;; summary:  Checks out the specified (agentName) parent agent script along
    ;;	         with all its children as a single appended string object. 
    ;; Args:     agentName	Name of the parent agent to be checked out from 
    ;;           the agent file cabinet.
    ;; Return:   result The parent agent and all its children as a single string.
    ;; *******************************************************************
        vars:(childName n startIndex result ix i agentName cabinetName cabinetIndex)
        (defun strip(s) (if (= (left s 7) ";#text#") (mid s 7 100000000) s))
        (if (= (argCount) 1) (setq agentName (string (argFetch 0))))
        (if (= (argCount) 2) (begin (setq cabinetName (string (argFetch 0))) (setq agentName (string (argFetch 1)))))
        (if (<> cabinetName #void) (setq cabinetIndex (member cabinetName _browseAgentExtents.Extents)))
        ;; We treat the agentName as the parent agent part of the agentName
        ;; appending the colon (:) symbol, so we can check out any 
        ;; child agents (if necessary).
        (setq childName (append "" agentName ":"))
        (setq n (length childName))
        (setq result "")
        (if (isNumber cabinetIndex) 
            (begin 
               (setq result (_browseAgentExtents.Extents[cabinetIndex].refSourceByKey agentName))
               (if (<> result #void) (goto Continue:))
               (return #void)
            )) ; end if
        ;; Perform general search of all file cabinet extents.
        (loop for cabinetIndex from 0 until (length _browseAgentExtents.Extents) do
           ;; Search only those extents which have their auto-search switch turned on.
           (if (= _browseAgentExtents.Extents[i 1].Pv.mySearchSW true)
               (setq result (_browseAgentExtents.Extents[cabinetIndex].refSourceByKey agentName))
               ) ; end if
           (if (<> result #void) (goto Continue:))
           ) ; end loop
        (if (= result #void) (return result))
        Continue::
        (setq startIndex (_browseAgentExtents.Extents[cabinetIndex].memberIndex agentName))
        (if (isNumber startIndex)
            (begin
                (setq result "")
                (_browseAgentExtents.Extents[cabinetIndex].beginTransaction)
                (setq ix startIndex)
                (setq result (append result (strip (_browseAgentExtents.Extents[cabinetIndex].refSourceByIndex ix))))
                (setq ix (addi ix 1))
                (while (and (< ix (_browseAgentExtents.Extents[cabinetIndex].refLength))
                            (= (left (_browseAgentExtents.Extents[cabinetIndex].refKeyByIndex ix) n) childName))
                    (setq result (append result (strip (_browseAgentExtents.Extents[cabinetIndex].refSourceByIndex ix))))
                    (setq ix (addi ix 1))
                    ) ; end while
                (_browseAgentExtents.Extents[cabinetIndex].commitTransaction)
            )) ;; end of if
        result) ;; end of checkoutParent
    ;; Clear the specified File Cabinet of all data.
    (defun clearCabinet(cabinetName) 
       vars:(cabinetIndex)
       (_initialize)
       (if (<> cabinetName #void) (setq cabinetIndex (member cabinetName _browseAgentExtents.Extents)))
       (if (isNumber cabinetIndex) 
           (_browseAgentExtents.Extents[cabinetIndex].clearRepository)
           (error (append "browseAgent.clearCabinet: unknown file cabinet [" cabinetName "]"))
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
        (setq N (length _browseAgentExtents.Extents))
        (loop for n from 0 until N do
           (_browseAgentExtents.Extents[n 1].commitTransaction)
           ) ; end loop
        true) ;; end of commitAll
    ;; Compile all agents in the current file cabinet.
    (defun compileAll(alwaysCompile)
        vars:(holdMe result errSwitch ix n name anItem)
        (if (<> (type _browseAgentExtents.Focus) Agent:) 
                (error (append "browseAgent: Current focus is not an extent")))
        (_browseAgentExtents.Focus.beginTransaction)
        (setq n (_browseAgentExtents.Focus.refLength))
        (loop for ix from 0 until n do
           (setq result (_browseAgentExtents.Focus.compileByIndex ix alwaysCompile))
           (if (<> result true) (setq errSwitch true))  
           ) ; end loop
        (_browseAgentExtents.Focus.commitTransaction)
        ;(setq browseAgent holdMe)
        (if (= errSwitch true) (return "!browseAgent: Errors occured during compilation. (see console log)!"))
        true) ;; end of compileAll
    ;; Compile the specified agent script.
    (defun compileSource(name)
    ;; *******************************************************************
    ;; summary:  Loads, compiles, and evaluates the specified (name) agent 
    ;;	         script in the agent file cabinet. If an error occurs, 
    ;;	         the error message is displayed. 
    ;; Args:     name	Name of the agent to be compiled from the agent file cabinet.
    ;; Return:   false	Always returns false or an error.
    ;; *******************************************************************
        vars:(parentName keys childName s n startIndex 
              file brk fileID (typ 0) err focus ix
              ) ;; end of temporary variables
        ;; If the name contains an imbedded colon (:) symbol,
        ;; then we keep only the parent agent part of the name
        ;; preceeding the colon (:) symbol. We also compile any 
        ;; child agents (if necessary).
        (setq parentName name)
        (setq n (find ":" name))
        (if (isNumber n)
            (setq parentName (left name n)))
        ;; Search for any child agents
        (setq childName (append "" parentName ":"))
        (setq n (length childName))
        (setq startIndex (_browseAgentExtents.Focus.memberIndex parentName))
        (if (isNumber startIndex)
            then
            (begin
                ;; We have found a parent by this name in the file cabinet.
                (_browseAgentExtents.Focus.beginTransaction)
                (setq ix startIndex)
                (setq err (_browseAgentExtents.Focus.compileByIndex ix true))
                (if (isString err)
                    (begin
                       (_browseAgentExtents.Focus.commitTransaction)
                       (error err)
                    )) ; end if
                (setq ix (addi ix 1))
                (while (and (< ix (_browseAgentExtents.Focus.refLength))
                            (= (left (_browseAgentExtents.Focus.refKeyByIndex ix) n) childName))
                    (setq err (_browseAgentExtents.Focus.compileByIndex ix true))
                    (if (isString err)
                        (begin
                           (_browseAgentExtents.Focus.commitTransaction)
                           (error err)
                        )) ; end if
                    (setq ix (addi ix 1))
                    ) ; end while
                (_browseAgentExtents.Focus.commitTransaction)
            )
            else
            (begin
                ;; We have not found a parent by this name in the file cabinet,
                ;; so we compile only the original name as specified and do not
                ;; attempt to compile all of its siblings.
                (_browseAgentExtents.Focus.beginTransaction)
                (setq err (_browseAgentExtents.Focus.compileByKey name true))
                (if (isString err)
                    (begin
                       (_browseAgentExtents.Focus.commitTransaction)
                       (error err)
                    )) ; end if
                (_browseAgentExtents.Focus.commitTransaction)
            )) ;; end of if
        true) ;; end of compileSource
    ;; Copy the specified source File Cabinet contents to the specified destination File Cabinet    
    (defun copyCabinetSource(sourceFileCabinetName destinationFileCabinetName)
       regs:(n N)
       vars:(childSource sourceIndex destinationIndex sourceChildNames)
       vars:(sourceFileCabinet destinationFileCabinet)
       (_initialize)
       (if (<> sourceFileCabinetName #void) (setq sourceIndex (member sourceFileCabinetName _browseAgentExtents.Extents)))
       (if (isNumber sourceIndex) 
           (setq sourceFileCabinet _browseAgentExtents.Extents[sourceIndex])
           (error (append "browseAgent.copyCabinet: unknown cabinet name [" sourceFileCabinetName "]"))
           ) ; end if
       (if (<> destinationFileCabinetName #void) (setq destinationIndex (member destinationFileCabinetName _browseAgentExtents.Extents)))
       (if (isNumber destinationIndex) 
           (setq destinationFileCabinet _browseAgentExtents.Extents[destinationIndex])
           (error (append "browseAgent.copyCabinet: unknown cabinet name [" destinationFileCabinetName "]"))
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
    ;;           element of the result string is separated by the endDel
    ;;           delimiter, and two item elements have each item separated
    ;;           by the midDel delimiter.
    ;; args:     anObj       The object to be converted into a string.
    ;;           midDel      The delimiter between elements items.
    ;;           endDel      The delimiter separating different elements.
    ;;           lineNum     (Optional) Include line numbers before Vector elements.
    ;; return:   dString     The converted delimited string   
    ;; *******************************************************************
       vars:(title i n lineNum cols)
       (if (= (argCount) 4) (setq lineNum (argFetch 3)) (setq lineNum false))
       (setq dString "")
       ;; Use the object type to determine the string format.
       (if (isType Agent: anObj) (setq n 0) (setq n (length anObj)))
       (cond  
           ;; Disassemble an Agent into a string.
           ((isType Agent: anObj)
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
           ;; Format a Record with attributes.
           ((and (= (type anObj) Record:) (>= (setq n (length (setq cols anObj["FieldList"]))) 0))
            (loop for i from 0 until n do
               (if (<> dString "") (setq dString (append dString endDel)))
               (setq dString (append dString (removeDelimiters cols[i 0]) midDel))
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
               (if (= lineNum true)
                   (setq dString (append dString "[" i "] " (removeDelimiters anObj[i])))
                   (setq dString (append dString (removeDelimiters anObj[i])))
                   ) ; end if
               ))) ;; end cond
       dString) ;; objectToDelimitedString
    ;; Unregister a file cabinet extent from the browse agent
    (defun dropExtent(extentName)
        (setq extentName (string extentName))
        (setq _browseAgentExtents.Focus #void)
        (setq _browseAgentExtents.FocusIndex -1)
        (setq _browseAgentExtents.Extents[extentName] #void)
        (if (> (length _browseAgentExtents.Extents) 0)
            (setFocus _browseAgentExtents.Extents[0 0]))
        true) ; end of dropExtent
    ;; Manage source erasing including all related child agents.
    (defun eraseChildren(command)
        vars:(name agentName agentSource s i n
              startIndex endIndex
              parentName parentLen
              childName childLen
              file brk focus keys
              ) ;; end of temporary variables
        ;; Check for a request to erase "all" agents.
        (if (= command "") (setq command "..all agents.."))
        (if (= command "..all agents..") (setq agentName "") (setq agentName (string command)))        
        ;; Compute the parent agent name and length.
        ;; If the name contains an imbedded colon (:) symbol,
        ;; then we keep only the parent agent part of the name
        ;; preceeding the colon (:) symbol.
        (setq n (find ":" agentName))
        (if (isNumber n)
            (setq parentName (left agentName n))
            (setq parentName agentName))
        (setq parentLen (length parentName))
        ;; Compute the parent agent name and length.
        (if (= parentName "")
            (setq childName "")         
            (setq childName (append "" parentName ":")))
        (setq childLen (length childName))
        ;; Position the start and end index at the agent to be erased.
        (if (= command "..all agents..")
            ;; We are erasing all agents. 
            (begin
               (setq startIndex 0)
               (setq endIndex (_browseAgentExtents.Focus.refLength))
               ) ;; end of then
            ;; We are erasing one parent and its child agents. 
            (begin
               (setq endIndex (_browseAgentExtents.Focus.refLength))
		       (loop for startIndex from 0 until endIndex do
		           ;; Quit if this agent is not to be erased.
		           (setq name (_browseAgentExtents.Focus.refKeyByIndex startIndex))
		           (if (or (= parentName name) (= childName (left name childLen))) (goto FoundStartIndex:))
		           ) ; end loop
               FoundStartIndex::
               ) ;; end of else
            ) ;; end of if
        ;; Erase each agent script from the file cabinet.
        (_browseAgentExtents.Focus.beginTransaction) 
        (loop for i from startIndex until endIndex do
            ;; Quit if this agent is not to be erased.
            (setq name (_browseAgentExtents.Focus.refKeyByIndex i))
            (if (and (<> command "..all agents..")
                     (<> parentName name)
                     (<> childName (left name childLen)))
                (goto CloseFileCabinet:)
                ) ;; end if
            ;; Erase this agent.
            (setq name (string name))
            (_browseAgentExtents.Focus.deleteAgentByKey name)
            (setq i (subi i 1))
            (setq endIndex (subi endIndex 1))
            ) ; end loop
        CloseFileCabinet::
        (_browseAgentExtents.Focus.commitTransaction) 
        true) ;; end eraseChildren
    ;; Erases an agent (including all its source) from the file cabinet. 
    (defun eraseSource(...)
        vars:(agentName cabinetName cabinetIndex)
        (_initialize)
        (if (= (argCount) 1) (begin (setq agentName (string (argFetch 0)))))
        (if (= (argCount) 2) (begin (setq cabinetName (string (argFetch 0))) (setq agentName (string (argFetch 1)))))
        (if (<> cabinetName #void) (setq cabinetIndex (member cabinetName _browseAgentExtents.Extents)))
        ;; If no cabinet name specified,  check in to the file cabinet in focus.
        (if (isNumber cabinetIndex) 
            (_browseAgentExtents.Extents[cabinetIndex].deleteAgentByKey agentName)
            (_browseAgentExtents.Focus.deleteAgentByKey agentName)
            ) ; end if
        true)
    ;; Handle errors gracefully
    (defun errorHandler(errmsg) (msgbox (append "browseAgent got this error: " errmsg)))
    ;; Uses the Workbench IO functions to export agent
    ;;  scripts in the agent file cabinet to a source file.
    (defun exportSource(fileName agentName)
        vars:(oldPath fileID (typ 0))
        ;; Create the output source file containing all 
        ;; the agent scripts in the agent file cabinet.
        (setq oldPath _path)
        (setq _path "")
        (setq fileID  (fileOpen fileName 1 typ))
        (setq _path oldPath)
        ;; Write the export source file header records.
        (fwriteln fileID ";;*************************************")
        (fwriteln fileID ";;*************************************")
        (fwriteln fileID ";; Exported Agent File Cabinet Document")
        (fwriteln fileID ";;*************************************")
        (fwriteln fileID ";;*************************************")
        ;; Write the export source file records.
        (_exportSource fileID agentName)
        ;; Close the export source file.
        (fileClose fileID 1)
        true) ;; end exportSource
    ;; Uses the Workbench IO functions to export agent
    ;; Return a vector of all source file names in the file cabinet.
    (defun getChildNames()
        vars:(i v)
        (_browseAgentExtents.Focus.beginTransaction)
        (setq v (new Vector: (_browseAgentExtents.Focus.refLength)))
        (loop for i from 0 until (_browseAgentExtents.Focus.refLength) do
            (setq v[i] (_browseAgentExtents.Focus.refKeyByIndex i))
            ) ;; end of loop
        (_browseAgentExtents.Focus.commitTransaction)
        v)
    ;; Return the count of all file cabinet extents.
    (defun getExtentCount() (length _browseAgentExtents.Extents))
    ;; Return a vector of all file cabinet extent names.
    (defun getExtentNames() (refAttributes _browseAgentExtents.Extents))
    ;; Return the name of the file cabinet extent currently in focus.
    (defun getFocus() _browseAgentExtents.Extents[_browseAgentExtents.FocusIndex 0])
    ;; Return the index of the file cabinet extent currently in focus.
    (defun getFocusIndex() _browseAgentExtents.FocusIndex)
    ;; Return a vector of all source file names in the file cabinet.
    (defun getKeys() (getChildNames))
    ;; Return an XPath directory string list for the next repository directory level.
    (defun getNextLevel(cabinetName agentName startLine lineCount ...)
        vars:(i I cabinetIndex s option1 options)
        ;; Note1: Each XPath directory line has the following tab delimited fields:
        ;;         type    		{..data type..}
        ;;         value   		{agentName for each node in the repository.}
        ;;         size			none
        ;;         date			none
        ;;         time			none
        ;;         version			{current AIS version}
        ;;         symbolicKey		{agentName for each node in the repository}
        ;;         uniqueKey		{agentIndex for each node in the repository}
        (if (= (setq cabinetIndex (member cabinetName _browseAgentExtents.Extents)) false) 
            (error (append "browseAgent: cannot focus on " cabinetName)))
        (if (>= (argCount) 5) (setq options (argFetch 4)))
        (setq s (_browseAgentExtents.Extents[cabinetIndex 1].getNextLevel agentName  startLine lineCount options))
        s)2
    ;; Return a vector of all source file names in the file cabinet.
    (defun getParentNames()
        vars:(i parentCount name v)
        (_browseAgentExtents.Focus.beginTransaction)
        (setq parentCount 0)
        (setq v (new Vector: (addi (_browseAgentExtents.Focus.refLength) 1)))
        (setq v[0] "..all agents..")
        (loop for i from 0 until (_browseAgentExtents.Focus.refLength) do
            (setq name (_browseAgentExtents.Focus.refKeyByIndex i))
            ;; Only save the name if it is a parent agent.              
            (if (= (find ":" name) false)                
                (setq v[(setq parentCount (addi parentCount 1))] name))
            ) ;; end of loop
        (_browseAgentExtents.Focus.commitTransaction)
        (resize v (addi parentCount 1))
        v)
    ;; Return an XPath directory type list string for all possible types in a hierarchical descendents list 
    (defun getTypes(cabinetName)
        vars:(cabinetIndex result)
        (if (= (setq cabinetIndex (member cabinetName _browseAgentExtents.Extents)) false) 
            (error (append "browseAgent: cannot focus on " cabinetName)))
        (setq result (_browseAgentExtents.Extents[cabinetIndex 1].getTypes))
        result)
    ;; Return the name of a default html page.
    (defun htmlPageServer(pageName pageParm)
        vars:(page action)
        (setq pageName (string pageName))
        (if (= (setq action htmlPageRegistry[pageName]) #void)
            then
            (begin
               (setq page "<HTML><BODY><H1 ALIGN=center>SmartBase/AgentServer Blank Home Page</H1></BODY></HTML>")
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
    ;; Imports a source file of multiple agents into the file
    ;;  cabinet. The source file must be in the export format
    ;;  output from the export function.
    (defun importSmallSource(name)
        vars:(s v i n agentName focus agent childSize matchStr)
        (_initialize)
        ;; Set up child agent focus vector.
        (setq focus (^new Vector: 0)) 
        ;; Read the import source file containing all 
        ;; the agent scripts for the agent file cabinet.
        (setq s (string (readSourceFile name)))
        ;; Break the source into a vector of components.
        (setq v (stringToVector s (append _eol ";;**EXPORTKEY**:")))
        (if (and (= (length v) 1) (= v[0] s)) (setq v (stringToVector s (append #\return ";;**EXPORTKEY**:"))))
        (if (and (= (length v) 1) (= v[0] s)) (setq v (stringToVector s (append #\newline ";;**EXPORTKEY**:"))))
        (if (and (= (length v) 1) (= v[0] s)) (setq v (stringToVector s (append #\newline #\return ";;**EXPORTKEY**:"))))
        ;; Write each agent script to the file cabinet.
        (_browseAgentExtents.Focus.beginTransaction)
        (loop for i from 1 until (length v) do
            ;; Isolate the agent script from the ;;**EXPORTKEY**: header.
            (setq n (find (setq matchStr (append "" #\return #\newline)) v[i]))
            (if (= n false) (setq n (find (setq matchStr (append "" #\newline #\return)) v[i])))
            (if (= n false) (setq n (find (setq matchStr (append "" #\return)) v[i])))
            (if (= n false) (setq n (find (setq matchStr (append "" #\newline)) v[i])))
            (setq agentName (left v[i] n))
            (setq agentName (string agentName))
            (setq agent (mid v[i] (+ n (length matchStr)) 100000000))
            ;; Import the script for an agent
            (writeln "[" i "] Importing source for " agentName)
            (_browseAgentExtents.Focus.setSourceExByKey agentName agent)
            ) ; end loop
        (writeln "Completed importing [" (subi i 1) "] Agents.")
        (_browseAgentExtents.Focus.commitTransaction) 
        true)
    ;; Imports a source file of multiple agents into the file
    ;;  cabinet. The source file must be in the export format
    ;;  output from the export function.
    (defun importSource(name)
        vars:(n agent agentName fileID fileSize (type 0))
        vars:(fileID self )
        (_initialize)
        ;; If the import source file is small, then use the faster import agent.
        (setq fileID (fileOpen name 0 type))
        (setq fileSize (fileSeek fileID 0 2)) 
        (fileClose fileID 1)
        ;(if (<= fileSize 30000000) (return (importSmallSource name)))        
        ;; Open the import source file containing all 
        ;; the agent scripts for the agent file cabinet.
        (openImportSourceFile name)
        (_browseAgentExtents.Focus.beginTransaction)
        ;; Write each agent script to the file cabinet.
        (while (<> myImportName #void) do
           (setq agentName myImportName)
           ;; Import the script for an agent
           (writeln "[" (setq n (addi n 1)) "] Importing source for " agentName)
           (setq agent (readImportSourceRecord))
           (_browseAgentExtents.Focus.setSourceExByKey agentName agent)
           ) ; end while   
        (writeln "Completed importing [" (subi n 1) "] Agents.")
        (_browseAgentExtents.Focus.commitTransaction) 
        (closeImportSourceFile)
        true)
    ;; Return a Structure of symbols and their closure size.
    (defun inspect(typeSW)
        vars:(i n result valueVector)
        (setq result (new Structure:))
        (cond
           ((= typeSW agents:) (setq valueVector (getParentNames)))
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
    (defun loadApplication(cabinetName agentName saveSW)
        regs:(n N)
        vars:(newAgent cabinetIndex globals sym app newAgent)
        (_initialize)
        (setq newAgent (checkoutAgent cabinetName agentName))
        (if (= newAgent #void)
            (if (<> saveSW true)
                (error "browseAgent.loadApplication: no application found under the specified cabinet name and agent name")
                (return (saveApplication cabinetName agentName))
                ) ; end if
            ) ; end if
        ;; Load the specified application.
        (newAgent)
        (^gc compact:)
        newAgent) ; end of loadApplication
    ;; Open source file for import.
    (defun openImportSourceFile(name) 
        vars:(nextLine (type 0))
        (setq myImportFileID (fileOpen name 0 type))
        (setq myImportBuffer (fileReadRecord myImportFileID))
        (setq nextLine (fileReadRecord myImportFileID myImportBuffer))
        ;; Read until we encounter the start of the first exported agent header.
        (while (and (<> nextLine #void) (<> (left nextLine 16) ";;**EXPORTKEY**:")) do
           (setq nextLine (fileReadRecord myImportFileID myImportBuffer))
           ) ; end while
        (if (= nextLine #void) (setq myImportName #void) (setq myImportName (mid nextLine 16 1000000)))
        true)
    ;; Read next import source file record 
    (defun readImportSourceRecord() 
        vars:(record nextLine)
        (if (= myImportName #void) (return #void))
        (setq record (new Vector: byte: 32000000))
        (setq nextLine (fileReadRecord myImportFileID myImportBuffer))
        ;; Read until we encounter the start of the next exported agent header.
        (while (and (<> nextLine #void) (<> (left nextLine 16) ";;**EXPORTKEY**:")) do
           (appendWriteln record nextLine _eol)
           (setq nextLine (fileReadRecord myImportFileID myImportBuffer))
           ) ; end while
        (if (= nextLine #void) (setq myImportName #void) (setq myImportName (mid nextLine 16 1000000)))
        (string record))
    ;; Perform a complete read of the specified source file.
    (defun readSourceFile(name) 
        vars:(fileID self (type 0))
        (setq fileID (fileOpen name 0 type))
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
    (defun saveApplication(cabinetName agentName)
        regs:(n N)
        vars:(newAgent cabinetIndex globals sym app index newAgent)
        (_initialize)
        ;; Create a current application agent containing the values of all currently defined global variables.
        (setq newAgent (eval (compile (lisp {(lambda() regs:(n N) pvars:(myGlobals) (setq N (length myGlobals)) (loop for n from 0 until N do (set myGlobals[n 0] myGlobals[n 1])) true)}))))
        (setq app (new Structure:))
        (setq globals (getSymbolTable 1 0 0))
        (if (isNumber (setq index (member _browseAgentExtents: globals))) (delete globals index))
        (if (isNumber (setq index (member browseAgent: globals))) (delete globals index))
        (setq N (length globals))
        (loop for n from 0 until N do (setq sym globals[n]) (setq app[sym] (getGlobalValue sym)))
        (setq newAgent.Pv.myGlobals app)
        ;; Save the current application in the cabinet name and agent name specified.
        (browseAgent.checkin cabinetName agentName newAgent)
        (^gc compact:)
        newAgent) ; end of saveApplication
    ;; Set the file cabinet extent focus to the specified extent.
    (defun setFocus(extentName)
        vars:(focusIndex)
        (if (<> _browseAgentExtents.Focus #void) (_browseAgentExtents.Focus.abortFocus))
        (setq extentName (string extentName))
        (if (= (setq focusIndex (member extentName _browseAgentExtents.Extents)) false) 
            (error (append "browseAgent: cannot focus on " extentName)))
        (setq _browseAgentExtents.FocusIndex focusIndex)
        (setq _browseAgentExtents.Focus _browseAgentExtents.Extents[focusIndex 1])
        (_browseAgentExtents.Focus.setFocus)
        true) ; end of setFocus
    ;; Remove the source from every agent in the current file cabinet.
    (defun stripAll(...)
        regs:(n N)
        vars:(holdMe result errSwitch name anItem cabinetName cabinetIndex mgr)
        (setq holdMe browseAgent)
        (if (>= (argCount) 1) (setq cabinetName (argFetch 0)))
        (if (= (setq cabinetIndex (member cabinetName _browseAgentExtents.Extents)) false) (setq cabinetIndex _browseAgentExtents.FocusIndex))
        (setq mgr _browseAgentExtents.Extents[cabinetIndex 1])
        (mgr.beginTransaction)
        (setq N (mgr.refLength))
        (loop for n from 0 until N do
	       (mgr.deleteSourceByIndex n)
           ) ; end loop
        (mgr.commitTransaction)
        (setq browseAgent holdMe)
        (if (= errSwitch true) (return "!Errors occured during strip source. (see console log)!"))
        true) ;; end of stripAll
    ;; Perform a complete inspection of each registered repository extent.
    (defun systemCheck()
        vars:(n N)
		(writeln _eol "Starting a complete system check of all repository extents.")
        (setq N (length _browseAgentExtents.Extents))
        (loop for n from 0 until N do
		   (writeln "Starting inspection of " _browseAgentExtents.Extents[n 0] " repository extent.")
           (_browseAgentExtents.Extents[n 1].beginTransaction)
           (_browseAgentExtents.Extents[n 1].inspectRepository)
           (_browseAgentExtents.Extents[n 1].abortTransaction)
           ) ; end loop
        true) ;; end of systemCheck
    ;; Return a tab delimited string of agent script names.
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
    ;; Take an agent out of the File Cabinet.
    (defun takeout(...) 
       vars:(result i agentName cabinetName cabinetIndex)
       (_initialize)
       (if (= (argCount) 1) (setq agentName (string (argFetch 0))))
       (if (= (argCount) 2) (begin (setq cabinetName (string (argFetch 0))) (setq agentName (string (argFetch 1)))))
       (if (<> cabinetName #void) (setq cabinetIndex (member cabinetName _browseAgentExtents.Extents)))
       (if (isNumber cabinetIndex) (return (_browseAgentExtents.Extents[cabinetIndex].refAgentByKey agentName)))
       ;; Perform general search of all file cabinet extents.
       (setq result (_browseAgentExtents.Focus.refAgentByKey agentName))
       (if (<> result #void) (return result))
       (loop for i from 0 until (length _browseAgentExtents.Extents) do
           ;; Search only those extents which have their auto-search switch turned on.
           (if (= _browseAgentExtents.Extents[i 1].Pv.mySearchSW true)
               (setq result (_browseAgentExtents.Extents[i 1].refAgentByKey agentName))
               ) ; end if
           (if (<> result #void) (return result))
           ) ; end loop
       result) ;; end of takeout
    ;; Perform a complete write of the specified source file.
    (defun writeSourceFile(name data) 
        vars:(fileID self (type 0))
        (setq fileID (fileOpen name 1 type))
        (setq self (fileWrite fileID data))
        (fileClose fileID 1)
        self)
    ;; *******************************************************************
    ;; Main logic of browseAgent
    ;; *******************************************************************
    (onError errorHandler)
    (addExtent extentName extentFileName true)
    
    (compileAll true)) ;; end of browseAgent



;;**EXPORTKEY**:browseAgent:%MISCELLANEOUS
;; *******************************************************************
;;  summary:  This browseAgent file loads miscellaneous utility agents
;;            useful in many projects. 
;;
;;            These utility agents are used so often, that they are
;;            included in the main AIS online documentation.
;;
;;  Agents:   loadWorkspace
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














;;**EXPORTKEY**:browseAgent:_dataManagerTemplate
(deforphan browseAgent:_dataManagerTemplate(extentName extentRepository)  
;; *******************************************************************
;; summary:  Factory for creating standard data manager agents.
;;           Manages all Agent source/binary code stored in each file
;;           cabinet extent. Includes checking Agent source/binary 
;;           in and out of the file cabinet, importing, exporting, 
;;           and deleting source from the file cabinet.
;;
;; Args:     extentName:          Name of the new file cabinet repository extent.
;;           extentRepository:    Path and file name of new file cabinet repository extent.
;;
;; Globals set by browseAgent:
;;           _browseAgentExtents  The current browseAgent extent structure.
;; Return:   true
;; *******************************************************************
    pvars:(;; Public Child Agents
           abortFocus                 ;; Remove the specified extent from focus 
           abortTransaction           ;; Abort a transaction on the extent repository
           beginTransaction           ;; Begin a transaction on the extent repository
           clearRepository            ;; Clear the extent repository
           commitTransaction          ;; Commit a transaction on the extent repository
           compileByIndex             ;; Compile the specified Agent source using an index
           compileByKey               ;; Compile the specified Agent source using a key
           deleteAgentByIndex         ;; Delete an Agent from the extent using an index
           deleteAgentByKey           ;; Delete an Agent from the extent using a key
           deleteBinaryByIndex        ;; Delete an Agent binary value using an index
           deleteBinaryByKey          ;; Delete an Agent binary value using a key
           deleteSourceByIndex        ;; Delete an Agent source value using an index
           deleteSourceByKey          ;; Delete an Agent source value using a key
           getNextLevel               ;; Return an XPath directory string list for the next repository directory level.
           getTypes                   ;; Return an XPath directory type list string for all possible types in a hierarchical descendents list
           inspectRepository          ;; Perform a com plete inspection of my extent repository.
           memberIndex                ;; Return the index of a key in an extent
           refAgentByIndex            ;; Retrieve an Agent from an extent using an index
           refAgentByKey              ;; Retrieve an Agent from an extent using a key
           refBinaryByIndex           ;; Retrieve an Agent binary value using an index
           refBinaryByKey             ;; Retrieve an Agent binary value using a key
           refKeyByIndex              ;; Retrieve an extent key using an index
           refLength                  ;; Retrieve the number of items in an extent
           refSourceByIndex           ;; Retrieve an Agent source value using an index
           refSourceByKey             ;; Retrieve an extent source value using a key
           refSourceExByIndex         ;; Retrieve an Agent source (for export) using an index
           refSourceExByKey           ;; Retrieve an extent source (for export) using a key
           setAgentByKey              ;; Store an extent value using a key
           setBinaryByKey             ;; Store an extent Agent binary value using a key
           setFocus                   ;; Set the specified extent in focus 
           setSourceByKey             ;; Store an extent Agent source value using a key
           setSourceExByKey           ;; Store an extent Agent source (from export) using a key
           ;; Private Variables
           myRepository               ;; The object repository for this extent 
           myExtentName               ;; The name for this extent
           myAgentName                ;; The name for the current agent being managed
           ;; Private Child Agents
           _lambdaTemplate		       ;; Orphan agent template for use in storing training memory in dataOnly file cabinet agents (does not share pvars or cvars).
           _prettyPrintMemory         ;; Create a pretty print source string of myTrainingMemory for dataOnly file cabinet agents.
           ) ; end persistant variables
    ;;****************************************************************
    ;; Define Public Child Agents
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
    ;; Compile the specified Agent source using an index 
    (defun compileByIndex(index alwaysCompile) (compileByKey myRepository[index 0] alwaysCompile))
    ;; Compile the specified Agent source using a key 
    (defun compileByKey(key alwaysCompile)
       vars:(theSource theAgent anItem key n N childAgent childName)
       ;; Capture and return any compile errors as strings
       (defun _compileItErr(err)
          vars:(msg) 
          (setq msg (append "browseAgent: Compiling " myExtentName "[" myAgentName "] returned this error: " err))
          (writeln msg) 
          msg)
       ;; Prepare to compile the named Agent and then evaluate the result.
       ;; Note: the Agent may already be compiled, if so, it will only be
       ;;       recompiled if the always compile switch is true.
       (onError _compileItErr)
       (setq myAgentName key)
       (setq anItem myRepository[key])
       ;; If the agent is already compiled, do not compile it again.
       (if (isAgent anItem)
           (if alwaysCompile
               (setq anItem anItem.Sc)
               (setq theAgent anItem)
               ) ; end if
           ) ; end if
       ;; If the item is not already compiled, then compile it, and
       ;; save then compiled agent back into the file cabinet so
       ;; that it will not be compiled again.
       (if (isString anItem)
           (begin
              (setq theSource anItem)
		      ;; Do not compile any script with a text directive.
		      (if (= (left theSource 7) ";#text#") (return true))
		      ;; Compile any script without a text directive.
              (setq theAgent (|Gv:compile| (lisp (_browseAgentExtents.Precompiler theSource))))
              (setq theAgent.Sc theSource)
              (setq myRepository[key] theAgent)
           )) ; end if
	   ;; Evaluate the compiled result so that any defines will
	   ;; be assigned to the global variable names specified in
	   ;; the source definition of the agent 
       (if (isAgent theAgent) (setq theAgent (eval theAgent)))
	   ;; Set the Interfaces bindings of the agent and all its
	   ;; child agents to match the names taken from the file cabinet.
	   (if (isAgent theAgent)
	       (begin 
		       (if (not (isStructure theAgent.In)) (setq theAgent.In (new Structure:)))
		       (setq theAgent.In.Binding myAgentName)
		       (loop for n from 0 until (length theAgent.Pv) do
		          (setq childAgent theAgent.Pv[n 1])
		          (setq childName theAgent.Pv[n 0])
		          (if (and (isAgent childAgent) (not (isStructure childAgent.In)))
		              (setq childAgent.In (new Structure: Binding: (append myAgentName ":" childName)))
		              ) ; end if 
		          ) ; end Pv loop
		       (loop for n from 0 until (length theAgent.Cv) do
		          (setq childAgent theAgent.Cv[n 1])
		          (setq childName theAgent.Cv[n 0])
		          (if (and (isAgent childAgent) (not (isStructure childAgent.In)))
		              (setq childAgent.In (new Structure: Binding: (append myAgentName ":" childName)))
		              ) ; end if 
		          ) ; end Cv loop
           )) ; end if
       true) ; end compileByKey
    ;; Delete an Agent from the extent using an index 
    (defun deleteAgentByIndex(index) (deleteAgentByKey myRepository[index 0]))
    ;; Delete an Agent from the extent using a key 
    (defun deleteAgentByKey(key) (setq myRepository[key] #void))
    ;; Delete an Agent binary value using an index 
    (defun deleteBinaryByIndex(index) (deleteBinaryByKey myRepository[index 0]))
    ;; Delete an Agent binary value using a key 
    (defun deleteBinaryByKey(key)
       vars:(anItem) 
       (setq anItem myRepository[key])
       ;; Save only the source from any binary agent found
       (if (isAgent anItem) (setq anItem anItem.Sc)) 
       (setq myRepository[key] anItem)) ; end deleteBinaryByKey
    ;; Delete an Agent source value using an index 
    (defun deleteSourceByIndex(index) (deleteSourceByKey myRepository[index 0]))
    ;; Delete an Agent source value using a key 
    (defun deleteSourceByKey(key)
       vars:(anItem) 
       (setq anItem myRepository[key])
       ;; Remove the source from any binary agent found
       (if (isAgent anItem) (setq anItem.Sc #void) (setq anItem #void)) 
       (setq myRepository[key] anItem)) ; end deleteSourceByKey
    ;; Return an XPath directory string list for the next repository directory level.
    (defun getNextLevel(agentName startLine lineCount ...)
        vars:(i I s stockName index)
        ;; Note1: Each XPath directory line has the following tab delimited fields:
        ;;         type    		{Agent}
        ;;         value   		{agentName for each node in the repository.}
        ;;         size			none
        ;;         date			none
        ;;         time			none
        ;;         version			{current AIS version}
        ;;         symbolicKey		{agentName for each node in the repository}
        ;;         uniqueKey		{agentName for each node in the repository}
        ;; Note2: We support only one hierarchy level of repository directory.
        ;;        Stock History names are the only keys allowed.
        (setq index (inspect myRepository directory:))
        (setq I (length index))
        (setq I (min I (+ startLine lineCount)))
        (setq s (append "" startLine #\tab lineCount #\tab I )) 
        (setq s (append s #\newline "Current" #\tab agentName #\tab I #\tab "" #\tab "" #\tab (isVersion) #\tab agentName #\tab agentName)) 
        (loop for i from 0 until I do
            (setq s (append s #\newline "Agent" #\tab (setq stockName index[i 0]) #\tab "" #\tab "" #\tab "" #\tab (isVersion) #\tab stockName #\tab stockName))
            ) ;; end of loop
        s)
    ;; Return an XPath directory type list string for all possible types in a hierarchical descendents list 
    (defun getTypes()
        vars:(result)
        (setq result (append ".default." #\tab "edit" #\newline
                             "Agent" #\tab "edit,erase,export,import,compile,checkin" #\newline
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
    (defun refAgentByIndex(index) myRepository[index 1])
    ;; Retrieve an extent value using a key 
    (defun refAgentByKey(key) myRepository[key])
    ;; Retrieve an agent binary value using an index 
    (defun refBinaryByIndex(index) (refBinaryByKey myRepository[index 0])) 
    ;; Retrieve an agent binary value using a key 
    (defun refBinaryByKey(key)
       vars:(result)       
       (setq result myRepository[key])
       (if (not (isAgent result)) (setq result #void))
       result) ; end refBinaryByKey
    ;; Retrieve an extent key using an index 
    (defun refKeyByIndex(index) myRepository[index 0])
    ;; Retrieve the number of items in an extent 
    (defun refLength() (length myRepository))
    ;; Retrieve an extent scource value using an index 
    (defun refSourceByIndex(index) (refSourceByKey myRepository[index 0])) 
    ;; Retrieve an agent source value using a key 
    (defun refSourceByKey(key)
       vars:(result)       
       (setq result myRepository[key])
       (if (isAgent result) 
           (begin
             (if (and (= result.Sc #void) (= result.In.dataOnly true)) 
                 (setq result (_prettyPrintMemory #void result.Pv.myTrainingMemory 12))
                 (setq result result.Sc)
                 ) ; end dataOnly if 
           )) ; end isAgent if
       result) ; end refSourceByKey
    ;; Retrieve an extent scource (for export) using an index 
    (defun refSourceExByIndex(index) (refSourceExByKey myRepository[index 0])) 
    ;; Retrieve an agent source (for export) using a key 
    (defun refSourceExByKey(key) (refSourceByKey key))
    ;; Store an extent value using a key 
    (defun setAgentByKey(key newAgent) (setq myRepository[key] newAgent))
    ;; Store an extent Agent binary value using a key 
    (defun setBinaryByKey(key newBinary) (setq myRepository[key] newBinary))
    ;; Set the specified extent in focus 
    (defun setFocus() true)
    ;; Store an extent Agent source value using a key 
    (defun setSourceByKey(key newSource) (setq myRepository[key] newSource))
    ;; Store an extent Agent source (from export) using a key 
    (defun setSourceExByKey(key newSource) (setq myRepository[key] newSource))
    ;;****************************************************************
    ;; Define Private Child Agents
    ;;****************************************************************
    ;; Create a pretty print source string of myTrainingMemory for dataOnly file cabinet agents.
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
             (setq sourceTemplate (browseAgent.checkout BrowseAgent: "browseAgent:_dataManagerTemplate:%MEMORY_HDR"))
             (appendWriteln buffer sourceTemplate _eol frontMargin)
             (appendWriteln buffer ";; The current historical and training memory for this dataOnly file cabinet agent." _eol frontMargin)
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



















;;**EXPORTKEY**:browseAgent:_dataManagerTemplate:%MEMORY_HDR
;; *******************************************************************
;;  summary:  This browseAgent my Training Memory Template serves
;;            as a storage module for the training history of this 
;;            dataOnly file cabinet agent.
;;
;;            All dataOnly file cabinet agents contain a pvars with
;;            only one variable: "myTrainingMemory", and a faces with
;;            the variable "dataOnly" equal to true.
;;            
;;            Only historical training data, of the format manageable
;;            by the _prettyPrintMemory function may be place the dataOnly
;;            file cabinet agent's "myTrainingMemory" persistant variable.
;;            Currently the _prettyPrintMemory function expects the
;;            myTrainingMemory variable to be a Structure, whose elements
;;            contain only Vectors, Dictionaries, Directories, and native
;;            types.
;;            
;;            The dataOnly agent may be checked into the file cabinet
;;            without attached source in its Sc attribute. Upon checkout,
;;            the browseAgent data extent manager will create source
;;            for the dataOnly agent, using the _prettyPrintMemory
;;            function and this blank agent template. 
;;
;;  Args:     none
;;  Return:   myself
;; *******************************************************************



















;;**EXPORTKEY**:browseAgent:_extentManagerTemplate
(deforphan browseAgent:_extentManagerTemplate(extentName extentRepository)  
;; *******************************************************************
;; summary:  Factory for creating standard extent manager agents.
;;           Manages all Agent source/binary code stored in each file
;;           cabinet extent. Includes checking Agent source/binary 
;;           in and out of the file cabinet, importing, exporting, 
;;           and deleting source from the file cabinet.
;;
;; Args:     extentName:          Name of the new file cabinet repository extent.
;;           extentRepository:    Path and file name of new file cabinet repository extent.
;;
;; Globals set by browseAgent:
;;           _browseAgentExtents  The current browseAgent extent structure.
;; Return:   true
;; *******************************************************************
    pvars:(;; Public Child Agents
           abortFocus                 ;; Remove the specified extent from focus 
           abortTransaction           ;; Abort a transaction on the extent repository
           beginTransaction           ;; Begin a transaction on the extent repository
           clearRepository            ;; Clear the extent repository
           commitTransaction          ;; Commit a transaction on the extent repository
           compileByIndex             ;; Compile the specified Agent source using an index
           compileByKey               ;; Compile the specified Agent source using a key
           deleteAgentByIndex         ;; Delete an Agent from the extent using an index
           deleteAgentByKey           ;; Delete an Agent from the extent using a key
           deleteBinaryByIndex        ;; Delete an Agent binary value using an index
           deleteBinaryByKey          ;; Delete an Agent binary value using a key
           deleteSourceByIndex        ;; Delete an Agent source value using an index
           deleteSourceByKey          ;; Delete an Agent source value using a key
           getNextLevel               ;; Return an XPath directory string list for the next repository directory level.
           getTypes                   ;; Return an XPath directory type list string for all possible types in a hierarchical descendents list
           inspectRepository          ;; Perform a com plete inspection of my extent repository.
           memberIndex                ;; Return the index of a key in an extent
           refAgentByIndex            ;; Retrieve an Agent from an extent using an index
           refAgentByKey              ;; Retrieve an Agent from an extent using a key
           refBinaryByIndex           ;; Retrieve an Agent binary value using an index
           refBinaryByKey             ;; Retrieve an Agent binary value using a key
           refKeyByIndex              ;; Retrieve an extent key using an index
           refLength                  ;; Retrieve the number of items in an extent
           refSourceByIndex           ;; Retrieve an Agent source value using an index
           refSourceByKey             ;; Retrieve an extent source value using a key
           refSourceExByIndex         ;; Retrieve an Agent source (for export) using an index
           refSourceExByKey           ;; Retrieve an extent source (for export) using a key
           setAgentByKey              ;; Store an extent value using a key
           setBinaryByKey             ;; Store an extent Agent binary value using a key
           setFocus                   ;; Set the specified extent in focus 
           setSourceByKey             ;; Store an extent Agent source value using a key
           setSourceExByKey           ;; Store an extent Agent source (from export) using a key
           ;; Public Variables
           mySearchSW                 ;; The cabinet search switch (true iff cabinet is to be auto-searched). 
           ;; Private Variables
           myRepository               ;; The object repository for this extent 
           myExtentName               ;; The name for this extent
           myAgentName                ;; The name for the current agent being managed
           ) ; end persistant variables
    ;;****************************************************************
    ;; Define Public Child Agents
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
    ;; Compile the specified Agent source using an index 
    (defun compileByIndex(index alwaysCompile) (compileByKey myRepository[index 0] alwaysCompile))
    ;; Compile the specified Agent source using a key 
    (defun compileByKey(key alwaysCompile)
       vars:(theSource theAgent anItem key n N childAgent childName)
       ;; Capture and return any compile errors as strings
       (defun _compileItErr(err)
          vars:(msg) 
          (setq msg (append "browseAgent: Compiling " myExtentName "[" myAgentName "] returned this error: " err))
          (writeln msg) 
          msg)
       ;; Prepare to compile the named Agent and then evaluate the result.
       ;; Note: the Agent may already be compiled, if so, it will only be
       ;;       recompiled if the always compile switch is true.
       (onError _compileItErr)
       (setq myAgentName key)
       (setq anItem myRepository[key])
       ;; If the agent is already compiled, do not compile it again.
       (if (isAgent anItem)
           (if alwaysCompile
               (setq anItem anItem.Sc)
               (setq theAgent anItem)
               ) ; end if
           ) ; end if
       ;; If the item is not already compiled, then compile it, and
       ;; save then compiled agent back into the file cabinet so
       ;; that it will not be compiled again.
       (if (isString anItem)
           (begin
              (setq theSource anItem)
		      ;; Do not compile any script with a text directive.
		      (if (= (left theSource 7) ";#text#") (return true))
		      ;; Compile any script without a text directive.
              (setq theAgent (|Gv:compile| (lisp (_browseAgentExtents.Precompiler theSource))))
              (setq theAgent.Sc theSource)
              (setq myRepository[key] theAgent)
           )) ; end if
	   ;; Evaluate the compiled result so that any defines will
	   ;; be assigned to the global variable names specified in
	   ;; the source definition of the agent 
       (if (isAgent theAgent) (setq theAgent (eval theAgent)))
	   ;; Set the Interfaces bindings of the agent and all its
	   ;; child agents to match the names taken from the file cabinet.
	   (if (isAgent theAgent)
	       (begin 
		       (if (not (isStructure theAgent.In)) (setq theAgent.In (new Structure:)))
		       (setq theAgent.In.Binding myAgentName)
		       (loop for n from 0 until (length theAgent.Pv) do
		          (setq childAgent theAgent.Pv[n 1])
		          (setq childName theAgent.Pv[n 0])
		          (if (and (isAgent childAgent) (not (isStructure childAgent.In)))
		              (setq childAgent.In (new Structure: Binding: (append myAgentName ":" childName)))
		              ) ; end if 
		          ) ; end Pv loop
		       (loop for n from 0 until (length theAgent.Cv) do
		          (setq childAgent theAgent.Cv[n 1])
		          (setq childName theAgent.Cv[n 0])
		          (if (and (isAgent childAgent) (not (isStructure childAgent.In)))
		              (setq childAgent.In (new Structure: Binding: (append myAgentName ":" childName)))
		              ) ; end if 
		          ) ; end Cv loop
           )) ; end if
       true) ; end compileByIndex
    ;; Delete an Agent from the extent using an index 
    (defun deleteAgentByIndex(index) (deleteAgentByKey myRepository[index 0]))
    ;; Delete an Agent from the extent using a key 
    (defun deleteAgentByKey(key) (setq myRepository[key] #void))
    ;; Delete an Agent binary value using an index 
    (defun deleteBinaryByIndex(index) (deleteBinaryByKey myRepository[index 0]))
    ;; Delete an Agent binary value using a key 
    (defun deleteBinaryByKey(key)
       vars:(anItem) 
       (setq anItem myRepository[key])
       ;; Save only the source from any binary agent found
       (if (isAgent anItem) (setq anItem anItem.Sc)) 
       (setq myRepository[key] anItem)) ; end deleteBinaryByKey
    ;; Delete an Agent source value using an index 
    (defun deleteSourceByIndex(index) (deleteSourceByKey myRepository[index 0]))
    ;; Delete an Agent source value using a key 
    (defun deleteSourceByKey(key)
       vars:(anItem) 
       (setq anItem myRepository[key])
       (cond 
         ;; Remove the source from any agent found
         ;; Note: Do NOT remove any source which begins with the ";#text#" directive!
         ((isAgent anItem)
          (begin        
           (if (and (isString anItem.Sc) (<> (left anItem.Sc 7) ";#text#"))
               (begin 
                 (setq anItem.Sc #void)
                 (setq myRepository[key] anItem)
               )) ; end if
           )) ; end case agent
         ;; Remove the source from any text found
         ;; Note: Do NOT remove any source which begins with the ";#text#" directive!
         ((isString anItem)
          (begin        
           (if (<> (left anItem 7) ";#text#")
               (begin 
                 (setq anItem "")
                 (setq myRepository[key] anItem)
               )) ; end if
           )) ; end case agent
        ) ; end cond 
       true) ; end deleteSourceByKey
    ;; Return an XPath directory string list for the next repository directory level.
    (defun getNextLevel(agentName startLine lineCount ...)
        vars:(i I s stockName index)
        ;; Note1: Each XPath directory line has the following tab delimited fields:
        ;;         type    		{Agent}
        ;;         value   		{agentName for each node in the repository.}
        ;;         size			none
        ;;         date			none
        ;;         time			none
        ;;         version			{current AIS version}
        ;;         symbolicKey		{agentName for each node in the repository}
        ;;         uniqueKey		{agentName for each node in the repository}
        ;; Note2: We support only one hierarchy level of repository directory.
        ;;        Stock History names are the only keys allowed.
        (setq index (inspect myRepository directory:))
        (setq I (length index))
        (setq I (min I (+ startLine lineCount)))
        (setq s (append "" startLine #\tab lineCount #\tab I )) 
        (setq s (append s #\newline "Current" #\tab agentName #\tab I #\tab "" #\tab "" #\tab (isVersion) #\tab agentName #\tab agentName)) 
        (loop for i from 0 until I do
            (setq s (append s #\newline "Agent" #\tab (setq stockName index[i 0]) #\tab "" #\tab "" #\tab "" #\tab (isVersion) #\tab stockName #\tab stockName))
            ) ;; end of loop
        s)
    ;; Return an XPath directory type list string for all possible types in a hierarchical descendents list 
    (defun getTypes()
        vars:(result)
        (setq result (append ".default." #\tab "edit" #\newline
                             "Agent" #\tab "edit,erase,export,import,compile,checkin" #\newline
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
    (defun refAgentByIndex(index) myRepository[index 1])
    ;; Retrieve an extent value using a key 
    (defun refAgentByKey(key) myRepository[key])
    ;; Retrieve an agent binary value using an index 
    (defun refBinaryByIndex(index) (refBinaryByKey myRepository[index 0])) 
    ;; Retrieve an agent binary value using a key 
    (defun refBinaryByKey(key)
       vars:(result)       
       (setq result myRepository[key])
       (if (not (isAgent result)) (setq result #void))
       result) ; end refBinaryByKey
    ;; Retrieve an extent key using an index 
    (defun refKeyByIndex(index) myRepository[index 0])
    ;; Retrieve the number of items in an extent 
    (defun refLength() (length myRepository))
    ;; Retrieve an extent scource value using an index 
    (defun refSourceByIndex(index) (refSourceByKey myRepository[index 0])) 
    ;; Retrieve an agent source value using a key 
    (defun refSourceByKey(key)
       vars:(result)       
       (setq result myRepository[key])
       (if (isAgent result) (setq result result.Sc))
       result) ; end refSourceByKey
    ;; Retrieve an extent scource (for export) using an index 
    (defun refSourceExByIndex(index) (refSourceExByKey myRepository[index 0])) 
    ;; Retrieve an agent source (for export) using a key 
    (defun refSourceExByKey(key)
       vars:(result)       
       (setq result myRepository[key])
       (if (isAgent result) (setq result result.Sc))
       result) ; end refSourceExByKey
    ;; Store an extent value using a key 
    (defun setAgentByKey(key newAgent) (setq myRepository[key] newAgent))
    ;; Store an extent Agent binary value using a key 
    (defun setBinaryByKey(key newBinary) (setq myRepository[key] newBinary))
    ;; Set the specified extent in focus 
    (defun setFocus() true)
    ;; Store an extent Agent source value using a key 
    (defun setSourceByKey(key newSource) (setq myRepository[key] newSource))
    ;; Store an extent Agent source (from export) using a key 
    (defun setSourceExByKey(key newSource) (setq myRepository[key] newSource))
    ;;****************************************************************
    ;; MAIN initialization section
    ;;****************************************************************
    (setq myExtentName extentName)
    (setq myRepository extentRepository)
    myRepository) ; end _extentManagerTemplate



















;;**EXPORTKEY**:browseAgent:_hostTextManagerTemplate
(deforphan browseAgent:_hostTextManagerTemplate(extentName folderName fileSuffix)  
;; *******************************************************************
;; summary:  Factory for creating standard host text manager agents.
;;           Manages all Host source/text code stored in each host
;;           file folder. Includes checking Agent source/text 
;;           in and out of the host file folder, importing, exporting, 
;;           and deleting source from the host file folder.
;;
;; Args:     extentName:          The name of the new extent to be managed.
;;           folderName:          Dir path name of the new host file folder.
;;           fileSuffix:          Case insensitive file suffix for new text files.
;;
;; Globals set by browseAgent:
;;           _browseAgentExtents  The current browseAgent extent structure.
;; Return:   true
;; *******************************************************************
    pvars:(;; Public Child Agents
           abortFocus                 ;; Remove the specified extent from focus 
           abortTransaction           ;; Abort a transaction on the extent repository
           beginTransaction           ;; Begin a transaction on the extent repository
           clearRepository            ;; Clear the extent repository
           commitTransaction          ;; Commit a transaction on the extent repository
           compileByIndex             ;; Compile the specified Agent source using an index
           compileByKey               ;; Compile the specified Agent source using a key
           deleteAgentByIndex         ;; Delete an Agent from the extent using an index
           deleteAgentByKey           ;; Delete an Agent from the extent using a key
           deleteBinaryByIndex        ;; Delete an Agent binary value using an index
           deleteBinaryByKey          ;; Delete an Agent binary value using a key
           deleteSourceByIndex        ;; Delete an Agent source value using an index
           deleteSourceByKey          ;; Delete an Agent source value using a key
           getNextLevel               ;; Return an XPath directory string list for the next repository directory level.
           getTypes                   ;; Return an XPath directory type list string for all possible types in a hierarchical descendents list
           inspectRepository          ;; Perform a com plete inspection of my extent repository.
           memberIndex                ;; Return the index of a key in an extent
           refAgentByIndex            ;; Retrieve an Agent from an extent using an index
           refAgentByKey              ;; Retrieve an Agent from an extent using a key
           refBinaryByIndex           ;; Retrieve an Agent binary value using an index
           refBinaryByKey             ;; Retrieve an Agent binary value using a key
           refKeyByIndex              ;; Retrieve an extent key using an index
           refLength                  ;; Retrieve the number of items in an extent
           refSourceByIndex           ;; Retrieve an Agent source value using an index
           refSourceByKey             ;; Retrieve an extent source value using a key
           refSourceExByIndex         ;; Retrieve an Agent source (for export) using an index
           refSourceExByKey           ;; Retrieve an extent source (for export) using a key
           setAgentByKey              ;; Store an extent value using a key
           setBinaryByKey             ;; Store an extent Agent binary value using a key
           setFocus                   ;; Set the specified extent in focus 
           setSourceByKey             ;; Store an extent Agent source value using a key
           setSourceExByKey           ;; Store an extent Agent source (from export) using a key
           ;; Public Variables
           mySearchSW                 ;; The cabinet search switch (true iff cabinet is to be auto-searched). 
           ;; Private Variables
           myAgentName                ;; The name for the current agent being managed
           myExtentName               ;; The name for this extent
           myFileNames                ;; The current file names in the host file folder being managed
           myFolderName               ;; The path name for the current host file folder being managed
           myHostDirObject            ;; The host dir object for this host file folder
           mySuffix                   ;; The case insensitive suffix for any new text files
           ;; Private Child Agents
           _checkin        	          ;; Write the host text file into the host file folder being managed.
           _checkout        	      ;; Read the host text file from the host file folder being managed.
	       _compileFile                ;; Compile the specified host source file
           _getFileNames        	  ;; Fills the myFilesNames vector with the latest list of file names in the host file folder being managed.
           _lambdaTemplate		      ;; Orphan agent template for use in storing training memory in dataOnly file cabinet agents (does not share pvars or cvars).
           ) ; end persistant variables
    ;;****************************************************************
    ;; Define Public Child Agents
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
    ;; Compile the specified Agent source using an index 
    (defun compileByIndex(index alwaysCompile) (compileByKey myFileNames[index] alwaysCompile))
    ;; Compile the specified Agent source using a key 
    (defun compileByKey(key alwaysCompile)
       vars:(theSource theAgent anItem key n N childAgent childName src)
       ;; Capture and return any compile errors as strings
       (defun _compileItErr(err)
          vars:(msg) 
          (setq msg (append "browseAgent: Compiling " myExtentName "[" myAgentName "] returned this error: " err))
          (writeln msg) 
          msg)
       ;; Prepare to compile the named Agent and then evaluate the result.
       (onError _compileItErr)
       (setq myAgentName key)
       (setq theSource (_checkout key))
       ;; Do not compile any script with a text directive.
       (if (= (left theSource 7) ";#text#") (return true))
       ;; Compile any script without a text directive.
       (setq theAgent (|Gv:compile| (lisp (_browseAgentExtents.Precompiler theSource))))
       (setq theAgent (eval theAgent))
	   ;; Evaluate the compiled result so that any defines will
	   ;; be assigned to the global variable names specified in
	   ;; the source definition of the agent 
       (if (isAgent theAgent) (setq theAgent (eval theAgent)))
	   ;; Set the Interfaces bindings of the agent and all its
	   ;; child agents to match the names taken from the file cabinet.
	   (if (isAgent theAgent)
	       (begin 
		       (if (not (isStructure theAgent.In)) (setq theAgent.In (new Structure:)))
		       (setq theAgent.In.Binding myAgentName)
		       (loop for n from 0 until (length theAgent.Pv) do
		          (setq childAgent theAgent.Pv[n 1])
		          (setq childName theAgent.Pv[n 0])
		          (if (and (isAgent childAgent) (not (isStructure childAgent.In)))
		              (setq childAgent.In (new Structure: Binding: (append myAgentName ":" childName)))
		              ) ; end if 
		          ) ; end Pv loop
		       (loop for n from 0 until (length theAgent.Cv) do
		          (setq childAgent theAgent.Cv[n 1])
		          (setq childName theAgent.Cv[n 0])
		          (if (and (isAgent childAgent) (not (isStructure childAgent.In)))
		              (setq childAgent.In (new Structure: Binding: (append myAgentName ":" childName)))
		              ) ; end if 
		          ) ; end Cv loop
           )) ; end if
       true) ; end compileByIndex
    ;; Delete an Agent from the extent using an index 
    (defun deleteAgentByIndex(index) (deleteAgentByKey myFileNames[index]))
    ;; Delete an Agent from the extent using a key 
    (defun deleteAgentByKey(key) (myHostDirObject.remove (append key "." mySuffix)))
    ;; Delete an Agent binary value using an index 
    (defun deleteBinaryByIndex(index) (deleteBinaryByKey myFileNames[index]))
    ;; Delete an Agent binary value using a key 
    (defun deleteBinaryByKey(key) (myHostDirObject.remove key))
    ;; Delete an Agent source value using an index 
    (defun deleteSourceByIndex(index) (deleteSourceByKey myFileNames[index]))
    ;; Delete an Agent source value using a key 
    (defun deleteSourceByKey(key) (myHostDirObject.remove key))
    ;; Return an XPath directory string list for the next repository directory level.
    (defun getNextLevel(agentName startLine lineCount ...)
        vars:(i I s stockName index)
        ;; Note1: Each XPath directory line has the following tab delimited fields:
        ;;         type    		{Agent}
        ;;         value   		{agentName for each node in the repository.}
        ;;         size			none
        ;;         date			none
        ;;         time			none
        ;;         version			{current AIS version}
        ;;         symbolicKey		{agentName for each node in the repository}
        ;;         uniqueKey		{agentName for each node in the repository}
        ;; Note2: We support only one hierarchy level of repository directory.
        ;;        Stock History names are the only keys allowed.
        (setq index (_getFileNames))
        (setq I (length index))
        (setq I (min I (+ startLine lineCount)))
        (setq s (append "" startLine #\tab lineCount #\tab I ))
        (setq s (append s #\newline "Current" #\tab agentName #\tab I #\tab "" #\tab "" #\tab (version) #\tab agentName #\tab agentName))
        (loop for i from 0 until I do
            (setq s (append s #\newline "Agent" #\tab (setq stockName index[i]) #\tab "" #\tab "" #\tab "" #\tab (isVersion) #\tab stockName #\tab stockName))
            ) ;; end of loop
        s)
    ;; Return an XPath directory type list string for all possible types in a hierarchical descendents list 
    (defun getTypes()
        vars:(result)
        (setq result (append ".default." #\tab "edit" #\newline
                             "Agent" #\tab "edit,erase,compile,checkin" #\newline
                      ))
        result)
    ;; Perform a complete inspection of my extent repository.
    (defun inspectRepository() true)
    ;; Return the index of a key in an extent
    (defun memberIndex(key) (member key myFileNames))
    ;; Retrieve an extent value using an index
    (defun refAgentByIndex(index) (refAgentByKey myFileNames[index]))
    ;; Retrieve an extent value using a key
    (defun refAgentByKey(key) (refBinaryByKey key))
    ;; Retrieve an agent binary value using an index
    (defun refBinaryByIndex(index) (refBinaryByKey myFileNames[index]))
    ;; Retrieve an agent binary value using a key
    (defun refBinaryByKey(key)
       vars:(result)
       (setq myAgentName key)
       (setq result (_checkout key))
       (setq result (_compileFile result))
       (if (not (isAgent result)) (setq result #void))
       result) ; end refBinaryByKey
    ;; Retrieve an extent key using an index
    (defun refKeyByIndex(index) myFileNames[index])
    ;; Retrieve the number of items in an extent
    (defun refLength() (length myFileNames))
    ;; Retrieve an extent scource value using an index
    (defun refSourceByIndex(index) (refSourceByKey myFileNames[index]))
    ;; Retrieve an agent source value using a key
    (defun refSourceByKey(key) (_checkout key))
    ;; Retrieve an extent scource (for export) using an index 
    (defun refSourceExByIndex(index) (refSourceExByKey myFileNames[index])) 
    ;; Retrieve an agent source (for export) using a key 
    (defun refSourceExByKey(key) (_checkout key))
    ;; Store an extent value using a key 
    (defun setAgentByKey(key newAgent) (_checkin key (if (isAgent newAgent) newAgent.Sc newAgent)))
    ;; Store an extent Agent binary value using a key 
    (defun setBinaryByKey(key newBinary) (setAgentByKey key newBinary))
    ;; Set the specified extent in focus 
    (defun setFocus() true)
    ;; Store an extent Agent source value using a key 
    (defun setSourceByKey(key newSource) (_checkin key newSource))
    ;; Store an extent Agent source (from export) using a key 
    (defun setSourceExByKey(key newSource) (_checkin key newSource))
    ;;****************************************************************
    ;; Define Private Child Agents
    ;;****************************************************************
    ;; Write the host text file into the host file folder being managed.
    (defun _checkin(fileName fileText)
      (browseAgent.writeSourceFile (append myFolderName "/" fileName "." mySuffix) fileText)
      true) ; end _checkin
    ;; Write the host text file into the host file folder being managed.
    (defun _checkout(fileName)
      (browseAgent.readSourceFile (append myFolderName "/" fileName "." mySuffix))
      ) ; end _checkout
    ;; Compile the specified host source file
    (defun _compileFile(theSource)
       vars:(theAgent anItem key n N childAgent childName src)
       ;; Prepare to compile the named Agent and then evaluate the result.
       (onError _compileItErr)
       ;; Do not compile any script with a text directive.
       (if (= (left theSource 7) ";#text#") (return true))
       ;; Compile any script without a text directive.
       (setq theAgent (|Gv:compile| (lisp (_browseAgentExtents.Precompiler theSource))))
       (setq theAgent (eval theAgent))
	   ;; Evaluate the compiled result so that any defines will
	   ;; be assigned to the global variable names specified in
	   ;; the source definition of the agent 
       (if (isAgent theAgent) (setq theAgent (eval theAgent)))
       theAgent) ; end _compileFile
    ;; Fills the myFilesNames vector with the latest list of file names in the host file folder being managed.
    (defun _getFileNames()
      vars:(n N L temp)
      ;; Initialize the host dir object (if necessary).
      (if (= myHostDirObject #void)
          (begin 
		    (setq myHostDirObject (^new browseAgent.dir myFolderName))
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
    (setq myHostDirObject (^new browseAgent.dir myFolderName))
    (myHostDirObject.setNameFilter (append "*." mySuffix))
    (_getFileNames)
    myHostDirObject) ; end _hostTextManagerTemplate















;;**EXPORTKEY**:browseAgent:_memoryManagerTemplate
(deforphan browseAgent:_memoryManagerTemplate(extentName extentRepository)  
;; *******************************************************************
;; summary:  Factory for creating standard memory manager agents.
;;           Manages all Agent source/binary code stored in each file
;;           cabinet extent. Includes checking Agent source/binary 
;;           in and out of the file cabinet, importing, exporting, 
;;           and deleting source from the file cabinet.
;;
;; Args:     extentName:          Name of the new file cabinet repository extent.
;;           extentRepository:    Path and file name of new file cabinet repository extent.
;;
;; Globals set by browseAgent:
;;           _browseAgentExtents  The current browseAgent extent structure.
;; Return:   true
;; *******************************************************************
    pvars:(;; Public Child Agents
           abortFocus                 ;; Remove the specified extent from focus 
           abortTransaction           ;; Abort a transaction on the extent repository
           beginTransaction           ;; Begin a transaction on the extent repository
           clearRepository            ;; Clear the extent repository
           commitTransaction          ;; Commit a transaction on the extent repository
           compileByIndex             ;; Compile the specified Agent source using an index
           compileByKey               ;; Compile the specified Agent source using a key
           deleteAgentByIndex         ;; Delete an Agent from the extent using an index
           deleteAgentByKey           ;; Delete an Agent from the extent using a key
           deleteBinaryByIndex        ;; Delete an Agent binary value using an index
           deleteBinaryByKey          ;; Delete an Agent binary value using a key
           deleteSourceByIndex        ;; Delete an Agent source value using an index
           deleteSourceByKey          ;; Delete an Agent source value using a key
           getNextLevel               ;; Return an XPath directory string list for the next repository directory level.
           getTypes                   ;; Return an XPath directory type list string for all possible types in a hierarchical descendents list
           inspectRepository          ;; Perform a com plete inspection of my extent repository.
           memberIndex                ;; Return the index of a key in an extent
           refAgentByIndex            ;; Retrieve an Agent from an extent using an index
           refAgentByKey              ;; Retrieve an Agent from an extent using a key
           refBinaryByIndex           ;; Retrieve an Agent binary value using an index
           refBinaryByKey             ;; Retrieve an Agent binary value using a key
           refKeyByIndex              ;; Retrieve an extent key using an index
           refLength                  ;; Retrieve the number of items in an extent
           refSourceByIndex           ;; Retrieve an Agent source value using an index
           refSourceByKey             ;; Retrieve an extent source value using a key
           refSourceExByIndex         ;; Retrieve an Agent source (for export) using an index
           refSourceExByKey           ;; Retrieve an extent source (for export) using a key
           setAgentByKey              ;; Store an extent value using a key
           setBinaryByKey             ;; Store an extent Agent binary value using a key
           setFocus                   ;; Set the specified extent in focus 
           setSourceByKey             ;; Store an extent Agent source value using a key
           setSourceExByKey           ;; Store an extent Agent source (from export) using a key
           ;; Private Variables
           myCurrentTreeLocation      ;; The current memory tree location for this file cabinet 
           myRepository               ;; The object repository for this extent 
           myExtentName               ;; The name for this extent
           myAgentName                ;; The name for the current agent being managed
           ;; Private Child Agents
           _length                    ;; Perform the length function (but not on Agents).
           _isName                    ;; Return true IFF the argument is a valid name.
           ) ; end persistant variables
    ;;****************************************************************
    ;; Define Private Child Agents
    ;;****************************************************************
    (defun _length(x) (if (not (or (isMacro x) (isAgent x))) (length x) 1))
    (defun _isName(name) (setq name (symbol (string name))) (or (isCharAlphabetic name[0]) (= name[0] #\_)))
    ;;****************************************************************
    ;; Define Public Child Agents
    ;;****************************************************************
    ;; Remove the specified extent from focus 
    (defun abortFocus() true)
    ;; Abort a transaction on the extent repository 
    (defun abortTransaction() (setq myRepository #void) true)
    ;; Begin a transaction on the extent repository 
    (defun beginTransaction() (setq myRepository (getSymbolTable 1 1 1)) true)
    ;; Clear the extent repository
    (defun clearRepository() (error "browseAgent._memoryManagerTemplate: not implemented"))
    ;; Commit a transaction on the extent repository 
    (defun commitTransaction() (setq myRepository #void) true)
    ;; Compile the specified Agent source using an index 
    (defun compileByIndex(index alwaysCompile) (error "browseAgent._memoryManagerTemplate: not implemented"))
    ;; Compile the specified Agent source using a key 
    (defun compileByKey(key alwaysCompile)(error "browseAgent._memoryManagerTemplate: not implemented"))
    ;; Delete an Agent from the extent using an index 
    (defun deleteAgentByIndex(index) (error "browseAgent._memoryManagerTemplate: not implemented"))
    ;; Delete an Agent from the extent using a key 
    (defun deleteAgentByKey(key) (error "browseAgent._memoryManagerTemplate: not implemented"))
    ;; Delete an Agent binary value using an index 
    (defun deleteBinaryByIndex(index) (error "browseAgent._memoryManagerTemplate: not implemented"))
    ;; Delete an Agent binary value using a key 
    (defun deleteBinaryByKey(key)(error "browseAgent._memoryManagerTemplate: not implemented"))
    ;; Delete an Agent source value using an index 
    (defun deleteSourceByIndex(index) (error "browseAgent._memoryManagerTemplate: not implemented"))
    ;; Delete an Agent source value using a key 
    (defun deleteSourceByKey(key) (error "browseAgent._memoryManagerTemplate: not implemented"))
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
                  (setq s (append s #\newline (type item) #\tab (append agentName " = " value)  #\tab I #\tab "" #\tab "" #\tab "" #\tab (isVersion) #\tab memoryItemName #\tab memoryItemName))
                 )) ; end case
               ;; Agent Item case
               ((or (isAgent item) (isMacro item))
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
	           ((= (type item) Record:)
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
    (defun memberIndex(key) (error "browseAgent._memoryManagerTemplate: not implemented"))
    ;; Retrieve an extent value using an index 
    (defun refAgentByIndex(index) (getGlobalValue myRepository[index]))
    ;; Retrieve an extent value using a key 
    (defun refAgentByKey(key)
       vars:(i I v exp item)
       (setq v (stringToVector key "/")) 
       (setq I (length v))
       (setq exp (append "|" (string v[0]) "|"))
       (loop for i from 1 until I do
          (cond ((= (left (string v[i]) 1) "[") (setq exp (append exp (string v[i])))) (else (setq exp (append exp "." (string v[i]))))) 
          ) ;; end of loop
       (setq item (eval exp))
       item) ; end refAgentByKey
    ;; Retrieve an agent binary value using an index 
    (defun refBinaryByIndex(index) (getGlobalValue myRepository[index])) 
    ;; Retrieve an agent binary value using a key 
    (defun refBinaryByKey(key) (refAgentByKey key))
    ;; Retrieve an extent key using an index 
    (defun refKeyByIndex(index) myRepository[index])
    ;; Retrieve the number of items in an extent 
    (defun refLength() (length myRepository))
    ;; Retrieve an extent scource value using an index 
    (defun refSourceByIndex(index) (refSourceByKey (refKeyByIndex index))) 
    ;; Retrieve an agent source value using a key 
    (defun refSourceByKey(key)
        vars:(i I s exp v name value item itemType)
        (setq item (refAgentByKey key))
        ;; Produce directory list of memory elements depending on data type.
        (cond
          ;; String Item case
          ((or (isString item) (isSymbol item) (isByteVector item))
           (begin
             (setq s (string item true))
            )) ; end case
          ;; Agent Item case
          ((or (isAgent item) (isMacro item))
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
    ;; Retrieve an agent source (for export) using a key 
    (defun refSourceExByKey(key) (refSourceByKey key))
    ;; Store an extent value using a key 
    (defun setAgentByKey(key newAgent) (error "browseAgent._memoryManagerTemplate: not implemented"))
    ;; Store an extent Agent binary value using a key 
    (defun setBinaryByKey(key newBinary) (error "browseAgent._memoryManagerTemplate: not implemented"))
    ;; Set the specified extent in focus 
    (defun setFocus() true)
    ;; Store an extent Agent source value using a key 
    (defun setSourceByKey(key newSource) (error "browseAgent._memoryManagerTemplate: not implemented"))
    ;; Store an extent Agent source (from export) using a key 
    (defun setSourceExByKey(key newSource) (error "browseAgent._memoryManagerTemplate: not implemented"))
    ;;****************************************************************
    ;; MAIN initialization section
    ;;****************************************************************
    (setq myExtentName extentName)
    (setq myRepository #void)
    myRepository) ; end _memoryManagerTemplate











;;**EXPORTKEY**:browseAgent:dir
;; dir Agent
; The dir agent provides access to directory structures and 
; their contents in a platform-independent way. 
; A dir instance is used to manipulate path names, access 
; information regarding paths and files, and manipulate 
; the underlying file system.
; 
; A dir instance can point to a file using either a relative 
; or an absolute path. Absolute paths begin with the directory 
; separator "/" (optionally preceded by a drive specification 
; under Windows). If you always use "/" as a directory 
; separator, the dir agent will translate your paths to conform 
; to the underlying operating system. Relative file names 
; begin with a directory name or a file name and 
; specify a path relative to the current directory. 
;
; The "current" path refers to the application's working 
; directory. A dir's own path is set and retrieved with 
; setPath and path. An example of an absolute path is the 
; string "/tmp/quartz", a relative path might look like "src/fatlib". 
; You can use the child agent isRelative to check if a dir is 
; using a relative or an absolute file path. Call convertToAbs 
; to convert a relative dir to an absolute one. For a simplified 
; path use cleanDirPath. To obtain a path which has no symbolic 
; links or redundant ".." elements use canonicalPath. The path 
; can be set with setPath, and changed with cd and cdUp. 
;
; dir provides several child agents, for example, setCurrent to 
; set the application's working directory and currentDirPath to 
; retrieve the application's working directory. Access to some 
; common paths is provided with the child agents, current, home 
; and root which return the dir instance or currentDirPath, 
; homeDirPath and rootDirPath which return the path as a string.
;
; The number of entries in a directory is returned by count. 
; Obtain a string list of the names of all the files and directories 
; in a directory with entryList. If you prefer a list of fileInfo 
; agents use entryInfoList. Both these functions can apply a name 
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
; of that class. This is reflected in the dir agent. Some child agents are
; listed as "STATIC". You may call these agents directly without creating
; an agent instance of dir. The other child agents work only on an
; instance of the dir agent.
;
; Notes
; This agent surfaces the full functionality of the QDir object. Most
; of the functionality of this agent resides in the glueLayer and is
; accessed by calls to the _QDir function. When you create a new 
; dir instance, a corrosponding QDir C++ object is created. Calls
; to child agents of the dir agent result in corrosponding calls to the
; member functions of the C++ QDir object. All of these calls are
; made through the _AISQDir function.
; 
; A child agent called "free" is provided to force the early destruction
; of the underlying C++ QDir object prior to garbage collection of a
; dir agent. The C++ object always be destroyed when the agent that created
; it is garbage collected.
;
;Extensions to underlying QDir object (features not in the underlying QDir object)
; See fileList - this child agent returns a vector of file information structures.
(deforphan browseAgent:dir()
	faces:((myType dir:))
	pvars:(
		; Public variables
		; Public Child Agents
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
		free			;Force the early destruction of the C++ resources allocated by a dir agent instance
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

		;; Utility agents
		selfTest		;Perform full self test on dir agent. ex: (browseAgent.dir.selfTest)

		;; private variables and child agents
		_AISQDirHandle	;Pointer to C++ QtDir object
		_CleanUp		;Child agent responsible for cleaning up on garbage collection of a dir instance
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
	(writeln "Checking static agents")
	(writeln {(browseAgent.dir.cleanDirPath "C:/somepath") ->} (browseAgent.dir.cleanDirPath "C:/somepath"))
	(writeln {(browseAgent.dir.currentDirPath) ->} (browseAgent.dir.currentDirPath))
	(writeln {(browseAgent.dir.convertSeparators "C:/somepath") ->} (browseAgent.dir.convertSeparators "C:/somepath"))
	(writeln {(browseAgent.dir.convertSeparators "C:\\somepath") ->} (browseAgent.dir.convertSeparators "C:\\somepath"))
	;(writeln {(browseAgent.dir.drives) ->} (browseAgent.dir.drives))
	(writeln {(setq h (browseAgent.dir.home)) (h.path) ->} (setq h (browseAgent.dir.home)) " " (h.path))
	(writeln {(browseAgent.dir.homeDirPath) ->} (browseAgent.dir.homeDirPath))
	(writeln {(browseAgent.dir.isRelativePath "C:/somepath") (browseAgent.dir.isRelativePath "somepath") ->} (browseAgent.dir.isRelativePath "C:/somepath") " " (browseAgent.dir.isRelativePath "somepath"))
	(writeln {(browseAgent.dir.match "*.txt" "myfile.txt") ->} (browseAgent.dir.match "*.txt" "myfile.txt"))
	(writeln {(browseAgent.dir.match "*.xml" "myfile.txt") ->} (browseAgent.dir.match "*.xml" "myfile.txt"))

	(writeln {(setq r (browseAgent.dir.root)) (r.path) ->} (setq r (browseAgent.dir.root)) " " (r.path))
 
	(writeln {(browseAgent.dir.rootDirPath) ->} (browseAgent.dir.rootDirPath))
	(writeln {(browseAgent.dir.separator) ->} (browseAgent.dir.separator))
	(writeln {(setq InfoItemList (browseAgent.dir.drives)) ->} (setq InfoItemList (browseAgent.dir.drives)))
	(writeln {(loop for i from 0 until (length InfoItemList) do (writeln (InfoItemList[i].fileName))) ->})
	(loop for i from 0 until (length InfoItemList) do (writeln (InfoItemList[i].dirPath)))

	; setCurrent?
 
	(writeln "****Check instance agents***")
	(writeln {(setq d (^new browseAgent.dir)) " " (d.path) ->} (setq d (^new browseAgent.dir)) " " (d.path))
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
	(writeln {(setq newdir (^new browseAgent.dir "newdirtwo")) ->} (setq newdir (^new browseAgent.dir "newdirtwo")))
	(writeln {(if (newdir.exists) (newdir.rmdir (newdir.absPath))) ->} (if (newdir.exists) (newdir.rmdir (newdir.absPath))))
	(writeln "Create a dir instance for an OS directory called 'newdir'")
	(writeln {(setq newdir (^new browseAgent.dir "newdir")) ->} (setq newdir (^new browseAgent.dir "newdir")))
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
	;; dir agent with instances lying around in memory - errors will occur.
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
;vector. This centralized type checking greatly reduces the size of this agent at the cost of an additional function 
;call and one new vector object per child agent.
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
		(if (not (isMember argType spec)) (begin
			(setq types "")
			(loop for k from 0 until K do (append types ", " (string spec[k])))
			(writeln "!Error: dir." functionName ", argument " n " has type " argType ", it should be one of " types)
			(return true)
			))
	);n

	false)


;_CleanUp
;Child agent responsible for cleaning up on garbage collection of a dir instance
; Arguments
;	None
;Returns #void
; Note: The _clearUp agent is called when the dir instance is garbage collected and the _cleanUp agent's
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
				; Return a string to be used in an error message constructed by the calling agent
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
				; Return a string to be used in an error message constructed by the calling agent
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
;NOTE: This is a static function and may be called on the base agent. eg:
; (setq path (browseAgent.dir.cleanDirPath path))
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
;NOTE: This is a static function and may be called on the base agent. eg
; (setq path (browseAgent.dir.currentDirPath)) 
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
;NOTE: This is a static function and may be called on the base agent. eg:
; (browseAgent.dir.convertSeparaters "some path") 
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
;NOTE: This is a static function and may be called on the base agent. eg
; (browseAgent.dir.current)
(defun current() 
	vars:(d
		(functionHandle 11)
		)
	(setq d (^new browseAgent.dir NOOBJECT:)) ; create a dir agent without a corrosponding C++ QtDir object
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
;The vector returned is a copy of the information maintained internally by the dir agent.
;NOTE: This is a static function and may be called on the base agent. eg:
; (setq drives (browseAgent.dir.drives)
(defun drives() 
	vars:(a A
		(functionHandle 13)
		InfoItemList
		)
	(setq InfoItemList (_AISQDir #void functionHandle))
	(setq A (length InfoItemList))
	(loop for a from 0 until A do
		;; Create infoItem agents for each pointer (to a QInfoItem) in the vector
		(setq InfoItemList[a] (^new browseAgent.fileInfo NOOBJECT: InfoItemList[a]))
	);a
	InfoItemList)

;entryInfoList
; Arguments
;	nameFilter 	optional, use #void for default or string
;	filterSpec 	optional, use #void for default or vector of filterSpec symbols
;	sortSpec	optional, use #void for default or vector of sortSpec symbols
;Returns a vector of fileInfo objects for all the files and directories in the directory, 
;ordered in accordance with setSorting and filtered in accordance with setFilter and setNameFilter. 
; The vector returned is a COPY of the information maintained internally by the dir agent.
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
		;; Create infoItem agents for each pointer (to a QInfoItem) in the vector
		(setq InfoItemList[a] (^new browseAgent.fileInfo NOOBJECT: InfoItemList[a]))
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
; NOTE: This child agent has no corrosponding QFileInfo counterpart. We are taking
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
;NOTE: This is a static function and may be called on the base agent. eg
; (setq d (browseAgent.dir.home))
(defun home() 
	vars:(d
		(functionHandle 20))
	(setq d (^new browseAgent.dir NOOBJECT:)) ; create a dir agent without a corrosponding C++ QtDir object
	(setq d._AISQDirHandle (_AISQDir #void functionHandle)); This call creates the C++ QtDir object
	(return d))

;homeDirPath - STATIC
; Arguments
;	none
;Returns the absolute path of the user's home directory. 
;NOTE: This is a static function and may be called on the base agent. eg
; (setq path (browseAgent.dir.homeDirPath))
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
;NOTE: This is a static function and may be called on the base agent. eg:
; (setq testResult (browseAgent.dir.isRelativePath somepath))
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
;   (setq d (^new browseAgent.dir "/tmp/root_link" ))
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
;NOTE: This is a static function and may be called on the base agent. eg
; (setq m (browseAgent.dir.match filter fileName))
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
;Construct a new dir agent instance 
;Constructs a QDir with path path, that filters its entries by name using 
;nameFilter and by attributes using filterSpec. It also sorts the names 
;using sortSpec. 
;The default nameFilter is an empty string, which excludes nothing; the 
;default filterSpec is All, which also means exclude nothing. The 
;default sortSpec is Name|IgnoreCase, i.e. sort by name case-insensitively. 
;Example that lists all the files in "/tmp": 
;
;	(setq d (new browseAgent.dir "/tmp" ));	
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
		newDirAgent
		sortSpecArg
		(sortSpecArgInt 0)
		filterSpecArg
		(filterSpecArgInt 0)
		(functionHandle 30)
		)

	(setq aCount (argCount)) ;; new receives a default first argument

;	(writeln "browseAgent.dir.new called")
;	(loop for n from 0 until aCount (writeln "argument " n " " (argFetch n)))

	(setq args (^new Vector: aCount))
	; Check for valid arguments. We check at runtime what C++ checks during
	; compilation. This is important as there are usually no runtime checks
	; in the underlying C++ object for these conditions.
	; We also convert the sortSpec and filterSpec arguments to the
	; form expected by the C++ constructor.
	(if (> aCount 0) (begin ; Check path argument. Must be string or text
		(setq pathArg (argFetch 0))

		;; The following special case code is used when we want to construct a dir agent
		;; without a corrosponding C++ QtDir object instance. See browseAgent.dir.current for an
		;; example of this useage.
		(if (= pathArg NOOBJECT:) (begin 
			(setq newDirAgent (myself))
			(setq newDirAgent._CleanUp.EvalWhenDoomed true)
			(return newDirAgent)
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

	(setq newDirAgent (myself))

	;Create the QtDir object and associate it with the dir agentinstance
	(setq newDirAgent._AISQDirHandle (_AISQDir #void functionHandle args))
	;Setting the EvalWhenDoomed flag on the _CleanUp child agent allows the dir agent instance to 
	;clean up resources when it is garbage collected.
	(setq newDirAgent._CleanUp.EvalWhenDoomed true)
	newDirAgent)

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
;NOTE: This is a static agent and may be called on the base agent. eg
; (setq d (browseAgent.dir.root)) 
(defun root() 
	vars:(d
		(functionHandle 36)
		)
	(setq d (^new browseAgent.dir NOOBJECT:)) ; create a dir agent without a corrosponding C++ QtDir object
	(setq d._AISQDirHandle (_AISQDir #void functionHandle)); This call creates the C++ QtDir object
	(return d))

;rootDirPath - STATIC
; Arguments
;	none
;Returns the absolute path for the root directory.
;For UNIX operating systems this returns "/". For Windows file systems 
;this normally returns "c:/". 
;See also root and drives.
;NOTE: This is a static function and may be called on the base agent. eg
;(setq path (browseAgent.dir.rootDirPath)) 
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
;NOTE: This is a static function and may be called on the base agent. eg:
; (setq s (browseAgent.dir.separator))
(defun separator() 
	vars:((functionHandle 38))
	(_AISQDir #void functionHandle))

;setCurrent - STATIC
;Sets the application's current working directory
; Arguments
;	path
;Returns TRUE if the directory was successfully changed; otherwise 
;returns FALSE.
;NOTE: This is a static function and may be called on the base agent. eg:
; (browseAgent.dir.setCurrent "somepath") 
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
(writeln "!Error: dir agent can not be called directly - please create a dir instance. ex: (setq d (new browseAgent.dir))")
true)


















;;**EXPORTKEY**:browseAgent:fileInfo
;; fileInfo
;
;The fileInfo agent provides system-independent file information. 
;A fileInfo agent provides information about a file's name and position (path) 
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
;If you need to read and traverse directories, see the dir agent. 
;
; Notes
; This agent surfaces the full functionality of the QFileInfo object. Most
; of the functionality of this agent resides in the glueLayer and is
; accessed by calls to the _AISQFileInfo function. When you create a new 
; fileInfo instance, a corrosponding QFileInfo C++ object is created. Calls
; to child agents of the dir agent result in corrosponding calls to the
; member functions of the C++ QFileInfo object. All of these calls are
; made through the _AISQFileInfo function.
; 
; A child agent called "free" is provided to force the early destruction
; of the underlying C++ QDir object prior to garbage collection of a
; dir agent. The C++ object will always be destroyed when the agent that 
; created it is garbage collected.
;
;Extensions to underlying QDir object (features not in the underlying QDir object)
; See getFileInfoStructure - this child agent returns file information structure.
;
(deforphan browseAgent:fileInfo()
	faces:((myType fileInfo:))
	pvars:(
		;Public child agents
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

		;Privage child agents
		_checkForBadArgs
		_checkIfInitialized
		_CleanUp
		)
;selfTest
(defun selfTest()
	vars:(f)
	(writeln "****Check instance agents***")
	(writeln {(setq f (^new browseAgent.fileInfo "C:/AisDev/libraries/dir/astartup.sl"))  ->} (setq f (^new browseAgent.fileInfo "C:/AisDev/libraries/dir/astartup.sl")))
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
;vector. This centralized type checking greatly reduces the size of this agent at the cost of an additional function 
;call and one new vector object per child agent.
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
				; Return a string to be used in an error message constructed by the calling agent
				(return (append " permissionSpec vector contains an unknown permission specification:" (string specArg[n])))
				))
			(setq specArgInt (bitwiseOr specArgInt _PermissionSpec[m 1]))
		)
	specArgInt)

;_CleanUp
;Child agent responsible for cleaning up on garbage collection of a fileInfo instance
; Arguments
;	None
;Returns #void
; Note: The _clearUp agent is called when the fileInfo instance is garbage collected and the _cleanUp agent's
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
;This agent returns the same as fileInfo.filePath, unless fileInfo.isRelative
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
;Returns true. This agent converts the file's path to an absolute path.
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
;Returns a dir agent instance for the file's path.
;If the fileInfo agent is relative and the absPath is false, the dir will be
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
	(setq d (^new browseAgent.dir NOOBJECT:)) ; create a dir agent without a corrosponding C++ QtDir object
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
		newAgent
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
		((and (= A 1) (= argsType[0] Agent:) (= args[0].In.myType fileInfo:))
			(setq QtFileInfoHandle (_AISQFileInfo #void functionHandle #void  #void args[0]._AISQFileInfoHandle))
		);
		;(new fileInfo dirInstance fileName)
		((and (= A 2) (= argsType[0] Agent:) (= args[0].In.myType dir:) (or (= argsType[1] String:) (= argsType[1] Text:)))
			(setq QtFileInfoHandle (_AISQFileInfo #void functionHandle args[1] args[0]._AISQDirHandle #void))
		);
		;BAD Signature
		(true 
			(writeln "!Error:fileInfo.new - bad arguments")
			(return #void)
		)
	)

	(setq newAgent (myself))
	(setq newAgent._AISQFileInfoHandle QtFileInfoHandle)
	;Setting the EvalWhenDoomed flag on the _CleanUp child agent allows the dir agent instance to 
	;clean up resources when it is garbage collected.
	(setq newAgent._CleanUp.EvalWhenDoomed true)
	newAgent)

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
(writeln "!Error: fileInfo agent can not be called directly - please create a fileInfo instance. ex: (setq fi (new fileInfo))")

true)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; support PROFILING macros 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro STARTTIME(...)
;;	Summary 		Setup and initialize the variables required for profiling in the faces: structure
;;	Args:		
;;		posttime:	Symbol argument specifying non-persistent agent (ie: call POSTTIMES will be made)
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


(defmacro POSTTIMES(sourceAgent agentKeyPrefix agentKeySuffix)
;;	Summary		The POSTTIMES macro calls the browseAgent.profiler.postTimes agent.
;;	Args:       See the postTimes agent for specification of these arguments
;;		sourceAgent
;;		agentKeyPrefix
;;		agentKeySuffix
	vars:(args)
	(if (= profiling true)
		(list (list ref: (list ref: (list ref: browseAgent:) (makeQuotedSymbol "profiler")) (makeQuotedSymbol "postTimes")) sourceAgent agentKeyPrefix agentKeySuffix)
	else
		(list (makeSymbol "begin"))
	);if
	); POSTTIMES













;;**EXPORTKEY**:browseAgent:precompiler
(deforphan browseAgent:precompiler(inSource)  
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
    pvars:(;; Public Child Agents
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
    ;; Define Public Child Agents
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

















;;**EXPORTKEY**:browseAgent:profiler
(deforphan browseAgent:profiler()  
;; *******************************************************************
;; summary:  The profiler agent defines data structures, child agents
;; 			and lisp macros that together allow you to instrument
;;			an application written in any AgentBase language.
;;			See the online documentation for an overview of how to use
;;			profiler.
;; Args:     None. Profiler is never executed directly.
;; Globals set by browseAgent:
;;           None.
;; Return:   NA. Profiler is never executed directly.
;; *******************************************************************
	pvars:(;; Persistant variables
			tPosted         ;; Timing structures from all active agents
			tcPosted        ;; Iiming structures from all active agents
			clearTiming		;; Clear the timing structures from all active agents
			showTiming		;; Collect and report the profiling information from all active agents and local t and tc directories
			postTimes		;; Post profiling information from a sourceAgent into profiler.t and profiler.tc directories	
		  )  ; end persistant variables


    (defun postTimes(sourceAgent keyPrefix keySuffix) ; invoked from the POSTTIMES macro
    ;; Summary: The postTimes function is only called from the POSTTIMES macro. Never call it directly in your code. This agent
    ;; recurses through the Pv strucuture of sourceAgent looking for child agents of sourceAgent. It posts and profiling information
    ;; found in these child agents, and sourceAgent itself, into directories maintained in browseAgent.profiler. The intent of postTimes
    ;; is to capture the collected profiling information from an agent before it is garbage collected. Never use the POSTTIMES macro
    ;; in an agent that is persistent. Use the POSTTIMES macro to collect profiling information from agents that are instantiated and
    ;; then released in your application.
	;; Args:
	;;	sourceAgent			An agent object from which collected timings are to be posted into browseAgent.profiler.
	;;	keyPrefix			A string or symbol argument that will be prepended to the sourceAgent's binding when creating the savekey
	;;	keySufix			A string or symbol argument that will be appended to the sourceAgent's binding when creating the save key 
	;;	Note on the save key. Post times posts values from the sourceAgent and all sourceAgent child agents into the profiler.t and 
	;;	profiler.tc directories based on a composite key comprised of keyPrefix+agentName+keySuffix.
	;; Returns: 		true
		vars:(i k st obj len len2 agentKey)
		;make sure t and tc directories are initialized in profiler
		(if (= tPosted #void) (setq tPosted (new Directory:)))
		(if (= tcPosted #void) (setq tcPosted (new Directory:)))
		;iterate through each child agent of source agent, posting any profiling information into the profiler.t and profiler.tc directories
		(setq len (length sourceAgent.Pv))
		(loop for i from 0 until len do
			(setq obj sourceAgent.Pv[i])
			(if (isAgent obj)
				(if (and 	(<> obj[In:].t #void) ;ignore agents that do not have profiling information in them
							(<> (find (string sourceAgent[In:].Binding) (string obj[In:].Binding) 0) false) ;ignore agents that are not root agents or children
							)
					(begin
;					(writeln obj.myCursorType " " obj " " obj[In:].Binding " " obj[In:].tc )
					(setq agentKey (symbol (append (if (<> keyPrefix "") (append (string keyPrefix) "+") "") (string obj[In:].Binding) (if (<> keySuffix "") (append "+" (string keySuffix)) "") )  ))
				    (if (= tPosted[agentKey] #void) (setq tPosted[agentKey] (new Vector:)))
				    (setq st obj[In:].t)
				    (setq len2 (length st))
				    (loop for k from 0 until len2 do
				    	(setq tPosted[agentKey][k] (+ tPosted[agentKey][k] st[k]))
				    );k
				    (setq tcPosted[agentKey] (iadd tcPosted[agentKey] obj[In:].tc)) 
					end)
				);if
			);if
		);i
		true)

    (defun clearTiming()
    ;; 	Summary		Clears all collected profiling information from active agents. Use this call
    ;;				before running your application.
    ;; 	Args:		none
    ;;	Returns:	true
		vars:( i agents agentIn len agentKey lt ltc lenT lenK tVec tTot s)
		(setq tPosted (new Directory:))
		(setq tcPosted (new Directory:))
		(setq agents (inspect objectList:)); get an object vector containing all active agents
 		(loop for i from 0 until (length agents) do
			(if (isAgent agents[i]) 
				(begin
	 			(setq agentIn agents[i][In:])
				(if (and (<> agentIn.tc #void) (<> agentIn.Binding #void))
	 				(begin 
	 				(setq agentIn.t #void)
	 				(setq agentIn.tc #void)
	 				end)
	 			);if  
	 			end)
	 		);if
 		);i
	    true);end of clearTiming
    
    (defun showTiming()
    ;;	Summary		Reports on all collected profiling information from active agents.
    ;;	Args:		None
    ;;	Returns:	true
		vars:( i agents agentIn len agentKey lt ltc lenT lenK tVec tTot s)
		(if (= tPosted #void) (setq tPosted (new Directory:)))
		(if (= tcPosted #void) (setq tcPosted (new Directory:)))
		(setq lt (copy tPosted))
		(setq ltc (copy tcPosted))
		
		(setq agents (inspect objectList:)); get an object vector containing all active agents
 		(loop for i from 0 until (length agents) do
 			(if (isAgent agents[i]) 
 				(begin
	 			(setq agentIn agents[i][In:])
 				(if (and 	(<> agentIn.tc #void) 
 							(<> agentIn.Binding #void)
 							(<> obj[In:].postTimeSW true) ;ignore agent if it uses posttime (avoid double counting)
							)
		 				(begin 
		 				(setq agentKey (symbol (append (string agentIn.Binding))))
		 				(setq lt[agentKey] agentIn.t)
		 				(setq ltc[agentKey] agentIn.tc)
		 				end)
	 			);if  
	 			end)
 			);if
 		);i
 		 
	    (setq lenT (length lt))
		(loop for i from 0 until lenT do
  			(setq tVec lt[i 1]); vector of timings withing an agent. ie: multiple calls to SETTIME in an agent
			(setq lenK (length tVec))
			;Get total time in agent and construct string of times for display
			(setq s "")
			(setq tTot 0.0)
			(loop for k from 0 until lenK do
				(+= tTot tVec[k])
				(setq s (append s 			(right (+ "               " (text tVec[k] "###,###,###.##") ) 15)))
			);k
             
            (if (= lenK 1) (setq s "")) ;don't print the tVec if we have only a single timing in the agent
            
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
;; Exported Agent File Cabinet Document
;;*************************************
;;*************************************


















;;**EXPORTKEY**:browseAgent:scanFile
(deforphan browseAgent:scanFile(fileName)
	vars:(
	fileID
	source)
	(setq fileID (fileOpen fileName 0 0) ) ;; Open a pre-existing script file
	(setq source (fileRead fileID))
	(fileClose fileID 1) ;; Close the file
	(scanSource source)
true)














;;**EXPORTKEY**:browseAgent:scanSource
(deforphan browseAgent:scanSource(source)
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
;; Exported Agent File Cabinet Document
;;*************************************
;;*************************************















;;**EXPORTKEY**:browseAgent:showGlobals
;; showGlobals uses the disassemble function to 
;; find global references in compile agents.
;; To use showGlobals do something like the following:
;; (browseAgent.showGlobals myagent)
(deforphan browseAgent:showGlobals(agent)
	pvars:(
		agents
		_showGlobals
		_showAgent
		excludeList
		)
	(setq excludeList (stringToVector "_eol _path _ais _browseAgentExtents end" #\space))

	(setq agents (new Vector:))
	(writeln "Parsing " agent.In.Binding)
	(_showGlobals agent) 
	(_showAgent agent agent.In.Binding)

(defun _showAgent(agent agentName)
	vars:(n N pv childname)
	(setq pv agent.Pv)
	(setq agents[(length agents)] agent)


	(setq N (length pv));
	(loop for n from 0 until N do
		(if (and (isAgent pv[n]) ;it is an agent
				(not (isInside pv[n] agents))) ;we have not visited it before
			(if (<> pv[n].Pv pv) ; and its not a child agent. It may be a friend, orphan etc.
				(begin
				(setq childname (append agentName "." pv[n 0]))
				(writeln "Parsing " agentName)
				(_showAgent pv[n] childname)
				)
			else
				(begin
				(setq childname (append agentName "." pv[n 0]))
				(writeln "Parsing " childname)
				(_showGlobals pv[n])
				)))
	);n
	true)

(defun _showGlobals(agent)
	vars:(n n2 n3 n4 name N src c tokenList)
	(setq tokenList (new Directory:))
	;; Examine code vector for global references
	(setq src (disassemble agent))
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
					(if (and (not (isAgent (getGlobalValue (symbol name))))
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








