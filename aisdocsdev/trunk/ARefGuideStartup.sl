;;  
;;  Title:    IndexRefGuide/RefGuide Startup Script
;;
;;  Author:   Michael F. Korns, Tim May, Loryfel Nunez
;;
;;  Project:  Create a live searching capability of RefGuide docs
;;
;;  Notes:    wwwroot folder has already been populated with RefGuide generated docs  
;; 

;; *******************************************************************
;; Application .ini file overrides.
;; *******************************************************************
;#AppPort==0
;#HttpPort==0
;#XmlPort==0
;#memory=500
;#memoryObjectHeaders=300

(setq _AlicePath _path)        
(setq _AliceWebPath (append _path "wwwroot/"))
(setq _AISRefGuidePath _AliceWebPath)
(setq _TestPath _AlicePath)
(setq libPath (append _ais.installPath "Libraries/"))   
(setq _LL (new Directory: )) ; Import and Compile list
(setq _LL2 (new Directory: )) ; Import only list
(setq _Core (new Vector: 6 RulesLambda: ParseLambda: JavaScript: Math: ))        

;; *******************************************************************
;; Fixed Memory Block Pre-allocations. 
;; Increased memory block allocation from 6000000 to 12000000
;; *******************************************************************
(preAllocateFixedMemoryBlocks  12000000 4)  

;; *******************************************************************
;; Load browseLib - browseLib is almost always required
;; *******************************************************************   
(runScript (append libPath "BrowseLib/BrowseLib.sl"))    
(runScript "Config.sl")

;;********************************************************************
;; Set the Startup Script welcome switch
;;********************************************************************
(setq _welcome true)      
(if (= browseLib #void)
	(begin
		(setq _welcome false)
		(loadBrowseLib) 
	);; end begin
) ;; end if 

 

;; *******************************************************************
;; configIndexRefGuide
;; Helper Lambda for reloading and exporting the application specific portions
;; of IndexRefGuide.
;; This function is completly application specific. Place all generic config
;; in the config Lambda.
;; *******************************************************************
(defun configIndexRefGuide() 
	vars:(currentBinaries
	      currentFile 		 
     );vars  
     pvars:( isBinariesComplete	)

	(browseLib.setFocus Binaries: )
	(setq currentBinaries (^new Vector: 0))
	(setq currentBinaries (browseLib.getChildNames))        
	(setq currentBinaries (browseLib.getChildNames))  
	(browseLib.setFocus IndexFile:)
	(setq currentFile (^new Vector: 0))
	(setq currentFile (browseLib.getChildNames))    
	(setq currentFile (browseLib.getChildNames))    
  
	;; Check for binary dependencies in the Bin folder
	(if (and (isMember "baseFile" currentFile) 
			 (isMember "docListREPO" currentBinaries)  
			 (isMember "docREPOS" currentBinaries) 
			 (isMember "freqREPOS" currentBinaries)  
			 (isMember "matrixREPOS" currentBinaries) 
			 (isMember "wordREPO" currentBinaries) 
		 ) ;; end and     		  
		(begin     
			(setq isBinariesComplete true)       
		) ;; end begin
		else
		(begin
			(if (= IndexRefGuide #void)
				(begin
					(browseLib.setFocus IndexRefGuide:)
	 			    (browseLib.importSource "Source/IndexRefGuide.sl")   
	 	 		    (browseLib.compileAll true)   
	 	 		    (setq isBinariesComplete false)
	 		     )) ;; end if 
	 	) ;; end begin
   ) ;; end if

); configIndexRefGuide

;; *******************************************************************
;; Register the main applciation code cabinets
;; Note that order is important here as it determines the import
;; and compile order.
;; *******************************************************************  
;; Core Cabinets
;; *******************************************************************
;; Load the server code from the file cabinet and the customer
;; import and fix up code from the alternate file cabinet.
;; *******************************************************************
(defun _browseLib (cabinetKey dbPath srcPath)
	(setq _LL[cabinetKey] srcPath)
	(browseLib cabinetKey dbPath srcPath "file" "auto" "auto" "true"))

(defun _browseLibAddExtent (cabinetKey dbPath srcPath)
	(setq _LL[cabinetKey] srcPath)
	(browseLib.addExtent cabinetKey dbPath srcPath "file" "auto" "auto" "true"))    
	
(defun _browseLibAddDataExtent (cabinetKey dbPath srcPath)
	(setq _LL[cabinetKey] srcPath)
	(browseLib.addDataExtent cabinetKey dbPath))

(defun _browseLibAddHostTextExtent (cabinetKey folderPath fileSuffix searcSW compileSW)
	(browseLib.addHostTextExtent cabinetKey folderPath fileSuffix searcSW compileSW))    
	

;*****************************************************************************
;********** Load the Lambda cabinets necessary for this application ***********
;*****************************************************************************
;; Core Cabinets
(_browseLib Index: 		"Bin/Index.db"			(append libPath "Index/Index.sl"))
(_browseLib Xml: 			"Bin/Xml.db" 		    (append libPath "Xml/Xml.sl"))

;; Application Cabinets              
(_browseLib AISRefGuide: 	"Bin/AISRefGuide.db" "Source/AISRefGuide.sl")
(_browseLib Thesaurus: 	"Bin/Thesaurus.db" "Source/Thesaurus.sl")
(_browseLib IndexRefGuide: 	"Bin/IndexRefGuide.db" "Source/IndexRefGuide.sl")  
(_browseLib PorterStemmer: "Bin/PorterStemmer.db"	 "Source/PorterStemmer.sl")        

(_browseLibAddHostTextExtent Foundry: "Foundry" "xml" true false)
(_browseLibAddHostTextExtent CoreContent: "CoreContent" "xml" true false)
(_browseLibAddHostTextExtent Guides: "Guides" "xml" true false)  
(_browseLibAddHostTextExtent ImageContent: "Foundry" "gif" true false)
(_browseLibAddHostTextExtent HtmlOutput: "wwwroot" "html" false false)
(_browseLibAddHostTextExtent ImageOutput: "wwwroot" "gif" false false)
(_browseLibAddHostTextExtent Binaries: "Bin" "db" false false)  
(_browseLibAddHostTextExtent IndexFile: "Bin" "txt" false false)
(_browseLibAddHostTextExtent HtmlTemplates: "Foundry" "html" true false)
(_browseLibAddHostTextExtent Templates: "Templates" "html" true false)
(_browseLibAddHostTextExtent DTD: "DTD" "dtd" true false)        

(browseLib.setFocus AISRefGuide:)
                                         
;; *******************************************************************
;; Register the application's Lambda/data cabients
;; *******************************************************************   

(configIndexRefGuide) 
 
(if (= index #void)
	(begin
		(browseLib.setFocus Index:)
	 	(browseLib.importSource "Source/index.sl")
        (browseLib.compileAll true)   
     ))                
     
    
(if (= Thesaurus #void)
	(begin
		(browseLib.setFocus Thesaurus:)
	 	(browseLib.importSource "Source/Thesaurus.sl")
        (browseLib.compileAll true)   
     )) 
	


;; If not existing, importing other source files
(if (= aisRefGuide #void)
	(begin
		(browseLib.setFocus AISRefGuide:)
	 	(browseLib.importSource "Source/AISRefGuide.sl")
        (browseLib.compileAll true)   
     ))      
(if (= porterStemmer #void)
	(begin
		(browseLib.setFocus PorterStemmer:)
	 	(browseLib.importSource "Source/PorterStemmer.sl")
        (browseLib.compileAll true)   
     ))  
        
     

;; *******************************************************************
;; Lock all globals and display the welcome message.
;; *******************************************************************
(lock _globals)             


;; *******************************************************************
;; Print Diagnostics from startup process
;; *******************************************************************
(config.reportBadImports)   
                           
;; *******************************************************************
;; Load the C/C++ xml compiler
;; *******************************************************************
;(loadLib "xml")
                           
;; *******************************************************************
;; Initialize the Alice Demo application.
;; ******************************************************************* 
(browseLib.bind)
(lock _globals)  
 
(writeln "************************************************************************")
(writeln "Welcome to the IndexRefGuide Demo Application")
(writeln "************************************************************************")
(writeln "Welcome to aisRefGuide running on " _path)
(writeln "Note: run (aisRefGuide true) to generate (Windows) new HTML reference documentation in wwwroot WITH copying to foundry.")
(writeln "Note: run (aisRefGuide true true) to generate (Linux) new HTML reference documentation in wwwroot WITH copying to foundry.")
(writeln "Note: run (aisRefGuide) to generate new HTML reference documentation in wwwroot WITHOUT copying to foundry.")
(writeln "Note: run (IndexRefGuide) to generate an index to the HTML reference documentation in wwwroot.")
(writeln {Note: run (setq aisRefGuide.exampleChecking true) BEFORE running asiRefGuide to turn example checking on during new HTML reference documentation creation.})
(writeln {Note: run (aisRefGuide.exportEmbeddedOntology "JavaScript" "javaScript" "CoreContent\\") to export embedded reference ontology objects.})
(if (= configIndexRefGuide.isBinariesComplete true)
	(begin
		(writeln "**********************************************************************************")
		(writeln "Note: Binaries for IndexRefGuide are complete.")
	    (writeln "Click on the View button to launch IE interface to launch Alice and IndexRefGuide.")    
	    (writeln "**********************************************************************************")
	 )
	 else
	 (begin            
	 	 (writeln "Note: run (IndexRefGuide) (IndexRefGuide.generateDocList) (IndexRefGuide.generateMatrix) to generate an index to the HTML reference documentation in wwwroot.")
     ))
     
    

		

 	

