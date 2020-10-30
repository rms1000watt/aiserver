;;*************************************
;;*************************************
;; Exported Lambda File Cabinet Document
;;*************************************
;;*************************************
;;version=5.0011-32bit
;;location=Source/AISRefGuide.sl
;;storageScope=file
;;importSync=auto
;;exportSync=auto
;;autoCompile=true
;;objRepoName=Bin/AISRefGuide.db
;;lastImport=#Jul,26,2009:21:15:16
;;END __DATA

;;**EXPORTKEY**:aisRefGuide
(defun aisRefGuide( ... )
;; *******************************************************************
;;  summary:  Analytic Information Server Reference Guide Document Manager
;;            Lambda.
;;
;;            Acts as a semi-intelligent document manager Lambda which
;;            is able to generate cross referenced and cross indexed 
;;            HTML documents in the specified output folder (wwwroot).
;;
;;            The final generated output is navigable by IE6.0 starting 
;;            from the following generated HTML file: "wwwroot\AStartup.html"
;;
;;  Args:     true			(Optional) If true then copy all input cabinets to
;;                             the Foundry cabinet before making the refernce guide.
;;  Return:   true			Always returns true (unless there was an error).
;;
;;  Notes:    Later versions of aisRefGuide will generate AIML rules and
;;            facts which will allow Alice to answer user technical FAQ's
;;            concerning Analytic Information Server and its components.
;;
;;  Requires: Must be run within a browseLib file cabinet environment.
;;            Requires the following additional resources:
;;              _AISRefGuideWebPath   path name of HTML output folder
;;              browseLib:          file cabinet database manager Lambda.
;;              Foundry:              file cabinet containing AIS reference guide ontology objects.
;;              HtmlOutput:           file cabinet containing HTML output file folder.
;; *******************************************************************
   pvars:(;; Public variables
          (myHTMLOutput "HtmlOutput")      ;; Name of HTML output file folder.
          myHTMLTemplateNames              ;; Vector of HTML Template names.
          (myImageContent "ImageContent")  ;; Name of Image objects file folder.
          myImageNames                     ;; Vector of Image object names.
          (myImageOutput "ImageOutput")    ;; Name of Image output file folder.
          (myOntology "Foundry")           ;; Name of XML ontology objects file folder.
          (myTemplates "HtmlTemplates")    ;; Name of HTML template objects file folder.
          (myVerboseSwitch false)          ;; Siwtch to turn the timing tests on/off
          myVMNames                        ;; Vector of VM Instruction ontology object names.
          myGenericIns                     ;; Vector of Generic Instructions
          myRegisterIns                    ;; Vector of Register Instructions
          myVectorIns                      ;; Vector of Vector Instructions  
          myNativeIns                      ;; Vector of Native Instructions  
          myExamples                       ;; Vector of Examples
          myFunctions                      ;; Vector of Functions
          myDatatypes                      ;; Vector of AIS Data Types
          myEssays                         ;; Vector of Essays
          myDocuments                      ;; Vector of Documents
          myEmbeddedOntologyNames          ;; Vector of Embedded Ontology object names
          myEmbeddedOntologyPrefix         ;; Name of HTML ontology objects file folder.
          myTargetLambdaName		           ;; Name of the target Lambda within the target cabinet.
          myTargetCabinetName		       ;; Name of the cabinet in which the target Lambda is stored.
          myVectorMachineLearning          ;; Vector of Vector Machine Learning Functions
          myMathFunctions                  ;; Vector of Math Functions
          myTrigFunctions                  ;; Vector of Trig Functions
          myBitwiseFunctions               ;; Vector of Bitwise Functions
          myFileFunctions                  ;; Vector of FileIO Functions
          myMacros                         ;; Vector of Built-in Macros
          myContextFunctions               ;; Vector of Context Functions
          myRepositoryFunctions            ;; Vector of Repository Functions
          myCrossReferenceTable            ;; Dictionary of ArgumentType-VMInstructions
          myCrossReferenceFunctionTable    ;; Dictionary of ArgumentType-Functions
          myCrossReferenceExArgTypeTable   ;; Dictionary of ArgumentType-Examples
          myCrossReferenceExFunctionTypeTable  ;; Dictionary of Function-Examples
          myCrossReferenceFunctionHKeys     ;; Dictionary of HumanKeyWords-VMInstructions
          myCrossReferenceDatatypeTable     ;; Dictionary of DataType-Function
          myCrossReferenceExTypeTable       ;; Dictionary of DataType-Example
          myFunctionDictionary              ;; Dictionary of FunctionName-FunctionType
          ;; Private variables
          myCurrentOntologyHtml            ;; Current VM ontology object processed. 
          myCurrentOntologyObject          ;; Current VM ontology object processed in XML format.
          myCurrentFunctionHtml            ;; Current Function ontology object processed.
          myCurrentFunctionObject          ;; Current Function ontology object processed in XML format.
          myCurrentExampleHtml             ;; Current Example ontology object processed.
          myCurrentExampleObject           ;; Current Example ontology processed in XML format.
          myCurrentFunction                ;; Current Function processed
          myCurrentFunctionName            ;; Current Function name processed
          myCurrentFunctionSpecialType     ;; Current Function type of Function processed
          myCurrentEssayHtml               ;; Current Essay ontology processed
          myCurrentEssayObject             ;; Current Essay ontology processed in XML format
          myCurrentEssayLink
          myCurrentDocumentHtml            ;; Current Document ontology processed 
          myCurrentDocumentObject          ;; Current Document ontology processed in XML format
          myCurrentDatatypeHtml            ;; Current Document ontology processed 
          myCurrentDatatypeObject          ;; Current Document ontology processed in XML format
          newFunctions
          newName
         (vmPrefix "VMInstruction_")       ;; Prefix for VM Instruction ontology object names.
         (exPrefix "Example_")             ;; Prefix for Example ontology object names
         (funcPrefix "Function_")          ;; Prefix for Function ontology object names
         (essayPrefix "Essay_")            ;; Prefix for Essay ontology objects
         (docPrefix "Document_")           ;; Prefix for Document ontology objects
         (dataPrefix "Datatype_")          ;; Prefix for Datatype ontology objects
          ;; Public child Lambdas
          checkout	                       ;; Check out the specified embedded ontology object
          checkoutTemplate                 ;; Check out the specified embedded template object
          exportEmbeddedOntology           ;; Export all embedded ontology objects from the specified Lambda.
          evalAllScripts                   ;; Evaluate any AIS Ref Guide scripts in the specified HTML Page.
          importEmbeddedOntologyObjects    ;; Import a Vector of embedded Lambda ontology object names.
          importVMNames                    ;; Import a Vector of VM instruction ontology object names.
          importHTMLTemplateNames          ;; Import a Vector of HTML Template names.
          importImageNames                 ;; Import a Vector of Image object names.
          createXTable                     ;; Create a new cross reference table
          makeXUpperCase
          ;; AIS Ref Guide script Lambdas
          vmLinkTable                      ;; Return an HTML table of VM Instruction links.
          exampleLinkTable                 ;; Return an HTML table of Example Pages links.
          functionLinkTable                ;; Return an HTML table of Function Pages links
          vmDescription                    ;; Returns the html text Description of each VM Instruction
          functionDescription              ;; Returns the html text Description of each Function
          exampleDescription               ;; Returns the html text Description of each Example
          vmSyntaxTable                    ;; Returns an HTML table of the syntax of each VMInstruction
          functionSyntaxTable              ;; Returns an HTML table of the syntax of each Function
          exampleSyntaxTable               ;; Returns the syntax of each Example
          functionExamples                 ;; Returns links to examples of each Function
          exampleInfo                      ;; Returns text and related links for each Example
          vmInsType                        ;; Returns an HTML table of VM Instructions with the same
                                           ;; Instruction Types
		  vmExamples
          vmKeyWordLinks                   ;; checks the CrossReference Table for VMInstructions
          functionKeyWordLinks             ;; checks the CrossReference Table
          exampleKeyWordLinks              ;; checks the CrossReference Table for Example Pages 
          exampleLinks                     ;; checks the CrossReference Table for examples of the same function  
          exampleFunctionLinks             ;; Returns links of functions in the Example Pages
          functionType
          exName
          essayName
		  (Boolean:exampleChecking false)  ;; (aisRefGuide) by default does not check for examples
			macroVector
        ; vmScript
		; (Boolean:errorValue false)
		;scriptTest
         ) ;; end of persistent variables 
    (defun itemPrep(prep) 
        (if (= prep #void) "" (string prep)))   
   ;; ****************************************************************
   ;; Define public child Lambdas
   ;; **************************************************************** 
   ;; Export all embedded ontology objects from the specified Lambda.
   (defun exportEmbeddedOntology(targetCabinetName targetLambdaName exportFolder)
      regs:(n N)
      vars:(ontologyObject (cabinetPrefix ";#text#") (cabinetPrefixLength 7))

	  ;; Save the target cabinet and Lambda  arguments.
	  (setq myTargetCabinetName (string targetCabinetName))
	  (setq myTargetLambdaName (string targetLambdaName))
	  (setq exportFolder (string exportFolder))
	  (setq myEmbeddedOntologyPrefix (append myTargetLambdaName ":%%"))
	
      ;; Load the names of all embedded Ontology objects.
      (importEmbeddedOntologyObjects)

      ;; Export all embedded Ontology objects (only export no HTML generation).
      (setq N (length myEmbeddedOntologyNames))
      (loop for n from 0 until N do
         (setq ontologyObject (checkout myEmbeddedOntologyNames[n]))  
         ;;(writeln "check-out: "ontologyObject)        
         (browseLib.writeSourceFile (append exportFolder "Foundry/" myEmbeddedOntologyNames[n] ".xml") ontologyObject)
      ) ; end loop
      
 
      (return true)
   ) ; end exportEmbeddedOntology
   ;; Check out the specified embedded ontology object
   (defun checkout(ontologyObjectName)
      vars:(ontologyObject (cabinetPrefix ";#text#") (cabinetPrefixLength 7))
      (setq ontologyObject (browseLib.checkout myTargetCabinetName (append myEmbeddedOntologyPrefix ontologyObjectName)))
      (mid ontologyObject cabinetPrefixLength 10000000)
   ) ; end checkout
   ;; Check out the specified embedded template object
   (defun checkoutTemplate(templateName)
      vars:(templateObject)
     (setq templateObject (browseLib.checkout myTemplates templateName))
   ) ; end checkoutTemplate
   ;; Import a Vector of HTML Template names.
   (defun importHTMLTemplateNames()
      vars:(K n N ontologyNames)
      (browseLib.setFocus myTemplates)
      (setq ontologyNames (browseLib.getChildNames))
      (setq N (length ontologyNames))
      (setq myHTMLTemplateNames (|Gv:new| Vector:))
      (loop for n from 0 until N do
         (setq myHTMLTemplateNames[(length myHTMLTemplateNames)] ontologyNames[n])
         ) ; end loop
      (length myHTMLTemplateNames)) ; end importHTMLTemplateNames
   ;; Import a Vector of Image Object names.
   (defun importImageNames()
      vars:(K n N ontologyNames)
      (browseLib.setFocus myImageContent)
      (setq ontologyNames (browseLib.getChildNames))
      (setq N (length ontologyNames))
      (setq myImageNames (|Gv:new| Vector:))
      (loop for n from 0 until N do
         (setq myImageNames[(length myImageNames)] ontologyNames[n])
         ) ; end loop
      (length myImageNames)) ; end importImageNames
   ;; Import a Vector of embedded Lambda ontology object names.
   (defun importEmbeddedOntologyObjects()
      vars:(k K m M n N tempNames currentFocus)
      (setq currentFocus (browseLib.getFocus))  
      (browseLib.setFocus myTargetCabinetName)
      (setq tempNames (browseLib.getChildNames))       
      (browseLib.setFocus currentFocus)      
      (setq K (length myEmbeddedOntologyPrefix))
      (setq N (length tempNames))
      (setq m -1)(setq myEmbeddedOntologyNames (new Vector:))
      (loop for n from 0 until N do (if (stringCiEQ (left tempNames[n] K) myEmbeddedOntologyPrefix) (setq myEmbeddedOntologyNames[(++ m)] (mid tempNames[n] K 100000))))  
      (setq N (length myEmbeddedOntologyNames))  
           
      (setq myExamples (^new Vector: 0))
      (setq myFunctions (^new Vector: 0))
      (setq myEssays (^new Vector: ))
      (setq myDocuments (^new Vector:))
      (loop for n from 0 until N do
          (if (stringCiEQ (left myEmbeddedOntologyNames[n] 8) exPrefix ) 
              (setq myExamples[(length myExamples)]  myEmbeddedOntologyNames[n]) 
          ) ;; end if
          (if (stringCiEQ (left myEmbeddedOntologyNames[n] 9) funcPrefix ) 
              (setq myFunctions[(length myFunctions)]  myEmbeddedOntologyNames[n]) 
           ) ;;end if
          (if (stringCiEQ (left myEmbeddedOntologyNames[n] 6) essayPrefix) 
              (setq myEssays[(length myEssays)] myEmbeddedOntologyNames[n]) 
          ) ;; end if
          (if (stringCiEQ (left myEmbeddedOntologyNames[n] 9) docPrefix)
              (setq myDocuments[(length myDocuments)] myEmbeddedOntologyNames[n])
          )
       ) ; end loop
        
      (length myDocuments)
   ) ; end importEmbeddedOntologyObjects
   ;; Import a Vector of VM instruction ontology object names.
   (defun importVMNames()
      vars:(K n N ontologyNames  vmInsName)
      (browseLib.setFocus myOntology)
      (setq ontologyNames (browseLib.getChildNames))
      (setq K (length vmPrefix))
      (setq N (length ontologyNames))
      (setq myVMNames (^new Vector: 0))
      (setq myGenericIns (^new Vector: 0))
      (setq myRegisterIns (^new Vector: 0))
      (setq myVectorIns (^new Vector: 0))
      (setq myNativeIns (^new Vector: 0))
      (setq myExamples (^new Vector: 0))
      (setq myFunctions (^new Vector: 0))
      (setq myEssays (^new Vector: ))
      (setq myDocuments (^new Vector:))
      (setq myDatatypes (^new Vector:))
      (loop for n from 0 until N do
          (if (stringCiEQ (left ontologyNames[n] K) vmPrefix) 
              (begin
                 (setq myVMNames[(length myVMNames)] ontologyNames[n])
                 (setq vmInsName (substring  ontologyNames[n] 16 18 ))
                 (if (compareEQ vmInsName "reg")
                     (setq myRegisterIns[(length myRegisterIns)] ontologyNames[n])
                  ) ;; end if
                 (if (isEqual vmInsName "nat")
                     (setq myNativeIns[(length myNativeIns)]  ontologyNames[n])
                  ) ;; end if 
                 (if (compareEQ vmInsName "vec")
                     (setq myVectorIns[(length myVectorIns)]  ontologyNames[n])
                  ) ;; end if 
                 (if (not (or (compareEQ vmInsName "vec") (compareEQ vmInsName "nat")(compareEQ vmInsName "reg")))
                     (setq myGenericIns[(length myGenericIns)]  ontologyNames[n])
                  ) ;; end if 
               );; end begin
          ) ; end if
          (if (stringCiEQ (left ontologyNames[n] 8) exPrefix ) 
              (setq myExamples[(length myExamples)]  ontologyNames[n]) 
          ) ;; end if
          (if (stringCiEQ (left ontologyNames[n] 9) funcPrefix ) 
              (setq myFunctions[(length myFunctions)]  ontologyNames[n]) 
           ) ;;end if
          (if (stringCiEQ (left ontologyNames[n] 6) essayPrefix)
              (setq myEssays[(length myEssays)] ontologyNames[n])
          ) ;; end if
          (if (stringCiEQ (left ontologyNames[n] 9) docPrefix)
              (setq myDocuments[(length myDocuments)] ontologyNames[n])
          )
         (if (stringCiEQ (left ontologyNames[n] 9) dataPrefix)
              (setq myDatatypes[(length myDatatypes)] ontologyNames[n])
          )
       ) ; end loop
      
   ) ; end importVMNames

 ;; create a persistent cross reference dictionary of key word links
 (defun createXTable()
   vars: ( N n ctr
           temp
           myCurrentOntologyXml
           tempVector
           vectorLength
           keyword 
           reference
           tempString )
   ;; creating new dictionary myCrossReferenceTable
  (setq myCrossReferenceTable (new Dictionary:))   
  (setq temp (new Vector: Object:)) 
  (setq N (length myVMNames))  
  ;; creating Cross Reference Dictionary for VMInstructions
  (loop for n from 0 until N do
      (setq myCurrentOntologyXml (xml (browseLib.checkout "Foundry" myVMNames[n])))
      (setq tempString myCurrentOntologyXml.VMInstruction.KnowledgeBase.ArgumentTypes)
      (setq tempVector (stringToVector (clean tempString) (append " " #\newline #\return #\tab)  true))
      (setq vectorLength (count tempVector))
      (loop for ctr from 0 until vectorLength
          (setq keyword tempVector[ctr] )
          (setq reference myVMNames[n])
         (setq reference (string reference))
          (setq keyword (string keyword))
          (setq keyword (symbol keyword))
          (setq reference (symbol reference))
          ;; create Vector Table   
          (if  (not (isVector myCrossReferenceTable[keyword]))
              (begin
              (setq myCrossReferenceTable[keyword] (new Vector: Object:))
              )) ;; end of if condition
          ;; set the VMInstruction Title to the Cross Reference Vecetor
         (setq temp myCrossReferenceTable[keyword])
         ;;(if (not (isMember reference temp))
         ;;   (setq temp[(length temp1)] reference) ) 
         (binaryInsert temp reference)       
       ) ;; end of loop for vectorLength 
    ) ;; end of loop for VMInstructions
  ) ;; end createXTable

 (defun setDescDictionary()
    vars: (index1 index2 htmlWriteText n vecLength temporary )            
          (setq index1 (find "<Description>" myCurrentExampleHtml ))
          (setq index2 (find "</Description>" myCurrentExampleHtml )) 
          (setq htmlWriteText (substring myCurrentExampleHtml index1 (+ 15 index2)) )           
          (setq reference htmlWriteText)   
          (setq keyword exName)
          (if  (not (isVector myExampleDesc[keyword]))
               (begin
                   (setq myExampleDesc[keyword] (new Vector: Object:))
                ) ;; end begin
          ) ;; end if
          (setq temporary myExampleDesc[keyword])
          ;;(if (not (isMember reference temporary))
          ;;    (begin
          ;;        (setq temporary[(length temporary)] reference )
          ;;     ) ;; end begin
          ;; ) ;; end if 
           (binaryInsert temporary reference)         
 ) ;; end setDescDictionary
 
 ;; create a persistent cross reference tables of ArgumentType-Functions
 ;; and DataType-Functions
 (defun createXFunctionTable()
   vars: ( N n ctr
           temp
           myCurrentOntologyXml
           myCurrentFunctionType
           tempVector
           tempString
           functionType
           newName
           argType
           argVector
           argVectorLength
           vectorLength
           keyword 
           reference functionType
           newFunctionNames
           ctr
           keywordData
           referenceData
           tempData
            )

   ;; creating new dictionary myCrossReferenceFunctionTable
  (setq myCrossReferenceFunctionTable (new Dictionary:))  
  (setq myCrossReferenceDatatypeTable (new Dictionary:)) 
  (setq temp (new Vector: Object:)) 
  (setq tempData (new Vector: Object:))
  (setq newFunctionNames (^new Vector: 0))
  (setq N (length myFunctions)) 

   ;; getting Data from Function Pages in Core Content
   (loop for n from 0 until N do
       (setq myCurrentOntologyXml (xml (browseLib.checkout "Foundry" myFunctions[n])))                     
       (setq myCurrentFunctionType myCurrentOntologyXml.Function.KnowledgeBase.DataType )
       (setq tempString (string myCurrentFunctionType))
       (setq tempVector (stringToVector (clean tempString) (append " " #\newline #\return #\tab)  true))
       ;;(sort tempVector <)
       (setq Q (length tempVector))
       ;; create the newFunction Names for the functions  
       ;; looping through Data types    
       (loop for q from 0 until Q do          
           (setq functionType tempVector[q])
           (setq newName (append  myFunctions[n] "_" tempVector[q]))
           ;;(setq newFunctionNames[(length newFunctionNames)] newName)
            
           ;; set dictionary entries for DataTypes-Function
           (setq keywordData tempVector[q])
           (setq referenceData newName)
           (setq keywordData (symbol keywordData))
           (setq referenceData (symbol referenceData))
           ;; create Vector Table
           (if  (not (isVector myCrossReferenceDatatypeTable[keywordData]))
                  (begin
                      (setq myCrossReferenceDatatypeTable[keywordData] (new Vector: Object:))
                   )
            );; end if condition
            ;; set the Function Title to the Cross Reference Vector
            (setq tempData myCrossReferenceDatatypeTable[keywordData])
            ;;(if (not (isMember referenceData tempData))
            ;;    (setq tempData[(length tempData)] referenceData)
            ;; ) ;; end of if condition
            (binaryInsert tempData referenceData)

           (if (= functionType #void)              
               (setq argType " none ")
           )
           (if (<> functionType #void)
               (begin               
               (setq argType myCurrentOntologyXml.Function.KnowledgeBase[(symbol (append
                  "ArgumentTypes-" functionType))])
              )
           )
           (if (and (<> functionType #void) (= argType #void))
                 (setq argType myCurrentOntologyXml.Function.KnowledgeBase.ArgumentTypes)
           )
                        
           (setq argVector (stringToVector (clean argType) (append " " #\newline #\return #\tab)  true)) 
           (setq argVectorLength (count argVector))
               
         
           ;; set dictionary entries for ArgumentTypes-Function
           (loop for ctr from 0 until argVectorLength
              (setq keyword argVector[ctr] )
              (setq reference newName)
              (setq keyword (symbol keyword))
              (setq reference (symbol reference))
              ;; create Vector Table   
              (if  (not (isVector myCrossReferenceFunctionTable[keyword]))
                  (begin
                      (setq myCrossReferenceFunctionTable[keyword] (new Vector: Object:))
                   )
              ) ;; end of if condition
              ;; set the Function Title to the Cross Reference Vector
             (setq temp myCrossReferenceFunctionTable[keyword])
             ;;(if (not (isMember reference temp))
             ;;   (setq temp[(length temp)] reference)
             ;;) ;; end of if condition
             (binaryInsert temp reference)
           ) ;; end of loop for argVectorLength              
        ) ;; end loop for New Function Names
    ) ;; end loop for new functions in Core Content
    
 );; end createXFunctionTable

  ;; creating Cross Reference Dictionary for ArgumentType-Examples
 (defun createXArgTypeTable()
   vars: ( N n ctr ctr2
           temp temp2
           myCurrentOntologyXml
           tempVector tempVector2
           vectorLength vectorLength2
           keyword keyword2
           reference reference2
           tempString tempString2
           exTypeVector
           exType keywordEx referenceEx tempEx
          )
   ;; creating new dictionary myCrossReferenceFunctionTable
  (setq myCrossReferenceExArgTypeTable (new Dictionary:))
  ;;new
  (setq myCrossReferenceExFunctionTypeTable (new Dictionary: )) 
  (setq temp2 (new Vector: Object:))  
  ;; end new   
  (setq temp (new Vector: Object:)) 
  (setq myCrossReferenceExTypeTable (new Dictionary:))   
  (setq tempEx (new Vector: Object:)) 
  (setq N (length myExamples))     
  (loop for n from 0 until N do
      (setq myCurrentOntologyXml (xml (browseLib.checkout "Foundry" myExamples[n])))
      (setq tempString  myCurrentOntologyXml.Example.KnowledgeBase.TypeKeywords)       
      (setq tempVector (stringToVector (clean tempString) (append " " #\newline #\return #\tab)  true))
      (setq vectorLength (count tempVector))
       ;; new
      (setq tempString2  myCurrentOntologyXml.Example.KnowledgeBase.FunctionKeywords)
      (setq tempVector2 (stringToVector (clean tempString2) (append " " #\newline #\return #\tab)  true))
      (setq vectorLength2 (count tempVector2))
 
       ;; end new

      ;; set the Example Type
      (setq exTypeVector (stringToVector myExamples[n] "_"))
      (setq exType exTypeVector[1])
      ;; Building Dictionary for Example Pages with the same Data Type
      (setq keywordEx exType)
      (setq referenceEx myExamples[n])
      (setq keywordEx (symbol keywordEx))
      (setq referenceEx (symbol referenceEx))
      ;; create Vector Table   
      (if  (not (isVector myCrossReferenceExTypeTable[keywordEx]))
           (begin
           (setq myCrossReferenceExTypeTable[keywordEx] (new Vector: Object:))
       )) ;; end of if condition
      ;; set the Example Title to the Cross Reference Vector
      (setq tempEx myCrossReferenceExTypeTable[keywordEx])  
      ;;(if (not (isMember referenceEx tempEx))
      ;;(setq tempEx[(length tempEx)] referenceEx) )  
      (binaryInsert tempEx referenceEx)    
 

      ;; Building Dictionary for Example Pages with the same Argument Type
      (loop for ctr from 0 until vectorLength
          (setq keyword tempVector[ctr] )
          (setq reference myCurrentOntologyXml.Example.KnowledgeBase.Title)
 
;; Bug solution for arglist in refguide with the LISP compiler
         (setq reference (string reference))
         (setq keyword (string keyword))
          (setq keyword (symbol keyword))
          (setq reference (symbol reference))
          ;; create Vector Table   
          (if  (not (isVector myCrossReferenceExArgTypeTable[keyword]))
              (begin
              (setq myCrossReferenceExArgTypeTable[keyword] (new Vector: Object:))
              )) ;; end of if condition
          ;; set the Example Title to the Cross Reference Vector
         (setq temp myCrossReferenceExArgTypeTable[keyword])  
 
         ;;(if (not (isMember reference temp))
         ;;   (setq temp[(length temp)] reference) )   
         (binaryInsert temp reference)     
       ) ;; end of loop for vectorLength1 for Argument Type
 

      ;; Building Dictionary for Example Pages with the same Function Keywords
      (loop for ctr2 from 0 until vectorLength2
          (setq keyword2 tempVector2[ctr2] )
          (setq reference2 myCurrentOntologyXml.Example.KnowledgeBase.Title)
           ;; Bug solution for arglist in refguide
           (setq reference2 (string reference2))
           (setq keyword2 (string keyword2))
          (setq keyword2 (symbol keyword2))
          (setq reference2 (symbol reference2))
          ;; create Vector Table   
         (if  (not (isVector myCrossReferenceExFunctionTypeTable[keyword2]))
              (begin
              (setq myCrossReferenceExFunctionTypeTable[keyword2] (new Vector: Object:))
              )) ;; end of if condition
          ;; set the Example Title to the Cross Reference Vector
         (setq temp2 myCrossReferenceExFunctionTypeTable[keyword2])  
         ;;(if (not (isMember reference2 temp2))
         ;;   (setq temp2[(length temp2)] reference2) ) 
         (binaryInsert temp2 reference2)       
       ) ;; end of loop for vectorLength2 for Argument Type

 

   ) ;; end loop for all examples
      
 ) ;;end for createXArgTypeTable

 

 ;; create a Cross Reference Dictionary for Function-HumanKeywords
 (defun createXFunctionHKeys()
   vars: ( N n ctr
           temp
           myCurrentOntologyXml
           tempVector
           vectorLength
           keyword 
           reference
           tempString )
   ;; creating new dictionary myCrossReferenceFunctionHKeys
  (setq myCrossReferenceFunctionHKeys(new Dictionary:))   
  (setq temp (new Vector: Object:)) 
  (setq N (length myFunctions))  
  (loop for n from 0 until N do
      (setq myCurrentOntologyXml (xml (browseLib.checkout "Foundry" myFunctions[n])))
      (setq tempString  myCurrentOntologyXml.Function.KnowledgeBase.HumanKeywords)
      (setq tempVector (stringToVector (clean tempString) (append " " #\newline #\return #\tab)  true))
      (setq vectorLength (count tempVector))
      ;; Building Dictionary for HumanKeywords for each function
      (loop for ctr from 0 until vectorLength
          (setq reference tempVector[ctr] )
          (setq keyword myCurrentOntologyXml.Function.KnowledgeBase.Title)
;; Bug solution for arglist in refguide
(setq reference (string reference))
(setq keyword (string keyword))
          (setq keyword (symbol keyword))
          (setq reference (symbol reference))
          ;; create Vector Table   
          (if  (not (isVector myCrossReferenceFunctionHKeys[keyword]))
              (begin
              (setq myCrossReferenceFunctionHKeys[keyword] (new Vector: Object:))
              )) ;; end of if condition
          ;; set the Example Title to the Cross Reference Vector
         (setq temp myCrossReferenceFunctionHKeys[keyword])  
         (if (not (isMember reference temp))
            (setq temp[(length temp)] reference) )        
       ) ;; end of loop for vectorLength for each Function
   ) ;; end of loop
 ) ;; end for createXFunctionHKeys


(defun specialFunctionNames(functionName)
    vars: ( newFunctionName)    
   (if (isInteger (find "mul" functionName))(setq newFunctionName "&#42")) 
   (if (isInteger (find "add" functionName))(setq newFunctionName "&#43"))
   (if (isInteger (find "sub" functionName))(setq newFunctionName "&#45"))
   (if (isInteger (find "div" functionName))(setq newFunctionName "&#47"))
   (if (isInteger (find "mulValue" functionName))(setq newFunctionName "&#42&#61"))
   (if (isInteger (find "addValue" functionName))(setq newFunctionName "&#43&#61"))
   (if (isInteger (find "subValue" functionName))(setq newFunctionName "&#45&#61"))
   (if (isInteger (find "divValue" functionName))(setq newFunctionName "&#47&#61"))
   (if (isInteger (find "add1" functionName))(setq newFunctionName "&#43&#43"))
   (if (isInteger (find "sub1" functionName))(setq newFunctionName "&#45&#45"))
newFunctionName ) ;; end specialFunctionNames

(defun asciiConvert(inputString)
vars:( testExNew testEx 
) ;; end vars
regs: ((CharPointer:testExp) (Integer:testExLen 0) (Integer:len 0) (Integer:ascii 1)
		cc c1 c2 c3
) ;; end regs
 
(setq testEx (new String: ""))
 
(setq testEx inputString)
 
	(vmregObjPointer testEx testExp)
 

	(setq testExNew (new Vector: Byte: 10)) 
	(setq testExLen (* 2 (length testEx)))
 	(resize testExNew testExLen) 
  
	(while (<> (setq cc testExp[0]) 0) do
 		 
		;; For ASCII &#	  
		(if (and (= cc 38) (= (setq c1 testExp[1]) 35))
			(begin

				;; for &#42 to *	
				(if (and  (= (setq c2 testExp[2]) 52)(= (setq c3 testExp[3]) 50)) 
					(begin
 						(setq testExNew len  42)
						(setq testExNew (++ len) null)
			 			(setq testExp (+ testExp 4))
						(setq ascii 1)		 
					)
				) ;; end if
	 			;; for &#43 to +	
				(if (and  (= (setq c2 testExp[2]) 52)(= (setq c3 testExp[3]) 51)) 
					(begin
 						(setq testExNew len  43)
						(setq testExNew (++ len) null)
			 			(setq testExp (+ testExp 4))
						(setq ascii 1)			 
					)
				) ;; end if	
				;; for &#45 to -	
				(if (and  (= (setq c2 testExp[2]) 52)(= (setq c3 testExp[3]) 53)) 
					(begin
 						(setq testExNew len  45)
						(setq testExNew (++ len) null)
			 			(setq testExp (+ testExp 4))
						(setq ascii true)			 
					)
				) ;; end if	
				;; for &#47 to /
			  	(if (and  (= (setq c2 testExp[2]) 52)(= (setq c3 testExp[3]) 55)) 
					(begin
 						(setq testExNew len  47)
						(setq testExNew (++ len) null)
			 			(setq testExp (+ testExp 4))
						(setq ascii 1)			 
					)
				) ;; end if	
		 
				;; from &#60 to <
				(if (and (= (setq c2 testExp[2]) 54)(= (setq c3 testExp[3]) 48)) 
					(begin						 
						(setq testExNew len  60)
			 			(setq testExNew (++ len)  0)
				 	
						(setq testExp (+ testExp 4))
						(setq ascii 1)	
					)
				) ;; end if
				;; from &#61 to =
				(if (and (= (setq c2 testExp[2]) 54)(= (setq c3 testExp[3]) 49)) 
					(begin						 
						(setq testExNew len  61)
			 			(setq testExNew (++ len)  0)
				 	 
						(setq testExp (+ testExp 4))
						(setq ascii 1)
					)
				) ;; end if
				;; from &#62 to >
				(if (and  (= (setq c2 testExp[2]) 54)(= (setq c3 testExp[3]) 50)) 
					(begin
						(setq testExNew len  62)
			 			(setq testExNew (++ len)  0)
			 			(setq testExp (+ testExp 4))
						(setq ascii 1)				 
					)
					
				) ;; end if
			) ;; end begin
		)
		(if (= ascii 0) 
			(begin 
			 
				(setq testExNew len cc)		
				(setq testExNew (++ len)  null)
				  
				(++ testExp)
			) ;; end begin
		) ;; end if
			(setq ascii false)
	) ;; end while 
testExNew)
 

 

   ;; ****************************************************************
   ;; Begin MAIN logic
   ;; ****************************************************************
   vars:(k K n N M p
         imageFile (copySW false) (linuxSW false)
         htmlPage htmlTarget htmlText
         insType startTime endTime
         ) ;; end of temporary variables 


   ;; Determine if we are to copy all input cabinets to the Foundry cabinet.
   (setq startTime (getTickCount 0))
   (if (> (argCount) 0) (setq copySW (argFetch 0)))
   (if (> (argCount) 1) (setq linuxSW (argFetch 1)))
   (if (= copySW true)
       then
       (begin
           (if (= linuxSW true)
               then
               (begin
                   (writeln "Starting AisRefGuide with copy on")
                   ;; Create the Foundry directory
                   (system {mkdir -p Foundry})
                   ;; Copy the CoreContents cabinet to Foundry
                   (writeln "Copying CoreContent files")
                   (system {cp -r CoreContent/* Foundry/})
                   ;; Copy the Sources cabinet to Foundry
                   ;;(writeln "Copying Source files")
                   ;;(system {cp -r Source/* Foundry/})
                   ;; Copy the Templates cabinet to Foundry
                   (writeln "Copying Template files")
                   (system {cp -r Templates/* Foundry/})
               )
               else
               (begin
                   (writeln "Starting AisRefGuide with copy on")
                   ;; Delete the CoreContents cabinet
                   (system {del /Q Foundry\*.*})
                   ;; Delete the wwwroot cabinet
                   (system {del /Q wwwroot\*.*})
                   ;; Copy the CoreContents cabinet to Foundry
                   (system {copy CoreContent\*.* Foundry\*.*})
                   ;; Copy the Guides cabinet to Foundry
                   (system {copy Guides\*.* Foundry\*.*})
                   ;; Copy the Templates cabinet to Foundry
                   (system {copy Templates\*.* Foundry\*.*})
               )
           )
       )
       else
       (writeln "Starting AisRefGuide with copy off")) ; end if 

   ;; Import the names of all core components such as images, ontology objects, etc.
   (importHTMLTemplateNames)
   (importImageNames)
   (importVMNames)
   ;;(loadLib "xml")
	(setq macroVector #("defmacro" "defriend" "defineStructure"))

   (if (compareEQ 3 (argCount))
        (begin
           (exportEmbeddedOntology (argFetch 0) (argFetch 1) (argFetch 2)) 
           (setq userFolder (argFetch 2))
           ;;(setq myOntology (append (argFetch 2) "Foundry/"))
           ;; generating and writing the HTML pages for Documents
           (setq N (length myDocuments)) 
           (loop for n from 0 until N do           
              (setq myCurrentDocumentHtml  (checkout myDocuments[n]))
              (setq myCurrentDocumentObject (xml myCurrentDocumentHtml)) 
              (setq htmlPage (checkoutTemplate "Template_Document"))
              (setq htmlPage (evalAllScripts htmlPage))              
              (browseLib.checkin myHTMLOutput myDocuments[n] htmlPage)
              (browseLib.writeSourceFile (append userFolder "wwwroot/" myDocuments[n] ".html") htmlPage)
            ) ;; end loop
           ;; generating and writing the HTML pages for Essays
           (setq N (length myEssays)) 
           (loop for n from 0 until N do                   
              (setq myCurrentEssayHtml  (checkout myEssays[n]))                
              (setq myCurrentEssayObject (xml myCurrentEssayHtml))    
              (setq myCurrentEssayLink  myCurrentEssayObject.Essay.KnowledgeBase.Title)
              (setq htmlPage (checkoutTemplate "Template_Essay"))
              (setq htmlPage (evalAllScripts htmlPage))
              (browseLib.checkin myHTMLOutput myEssays[n] htmlPage)
              (browseLib.writeSourceFile (append userFolder "wwwroot/" myEssays[n] ".html") htmlPage)
           ) ;;end loop 
           (goto VERBOSESWITCH:)
         ) ;; end begin      
    ) ;; end if
 
#if 0  ;; MFK 
   ;; Move startup and menu pages to the output web site folder.
   (setq htmlPage (browseLib.checkout myTemplates "AStartup"))
   (browseLib.checkin myHTMLOutput "AStartup" htmlPage)
   (setq htmlPage (browseLib.checkout myTemplates "AliceStartup"))
   (browseLib.checkin myHTMLOutput "AliceStartup" htmlPage)
   (setq htmlPage (browseLib.checkout myTemplates "Scripts"))
   (browseLib.checkin myHTMLOutput "Scripts" htmlPage)
   (setq htmlPage (browseLib.checkout myTemplates "WelcomeMenu"))
   (browseLib.checkin myHTMLOutput "WelcomeMenu" htmlPage)
#endif ;; MFK 

   ;; Move all HTML templates to the output web site folder.
   (setq N (length myHTMLTemplateNames))
   (loop for n from 0 until N do
      (setq htmlPage (browseLib.checkout myTemplates myHTMLTemplateNames[n]))
      (browseLib.checkin myHTMLOutput myHTMLTemplateNames[n] htmlPage)
   ) ; end loop

   ;; Move all images to the output web site folder.
   (setq N (length myImageNames))
   (loop for n from 0 until N do
      (setq imageFile (browseLib.checkout myImageContent myImageNames[n]))
      (browseLib.checkin myImageOutput myImageNames[n] imageFile)
   ) ; end loop
 
   ;; Generate the VM Instruction pages.
   (setq timeVMDesc 0)
   (setq timeVMIns 0)
   (setq timeVMKW 0)
   (setq timeVMLink 0)
   (setq timeVMSyn 0)
   (setq startTimeVM (getTickCount 0))
   (setq N (length myVMNames))
   (loop for n from 0 until N do
      ;;(writeln "Building HTML page for [" myVMNames[n] "]") 
      (setq myCurrentOntologyHtml  (browseLib.checkout myOntology myVMNames[n])) 
      
      (setq myCurrentOntologyHtml (substitute (substitute myCurrentOntologyHtml {<![CDATA[} "") {]]>} ""))
      (setq myCurrentOntologyObject (xml myCurrentOntologyHtml))
      (setq htmlPage (browseLib.checkout myTemplates "Template_VMInstruction"))
      (setq htmlPage (evalAllScripts htmlPage))
		  
      (browseLib.checkin myHTMLOutput myVMNames[n] htmlPage)
      ) ; end loop
    (setq endTimeVM (getTickCount startTimeVM))

   ;; Generate the Function pages
   (setq M (length myFunctions))
   (setq exLength (length myExamples)) 
   (setq newFunctions (^new Vector: 0))
   (setq myExampleDictionary (new Dictionary:))
   (setq myExampleDesc (new Dictionary:))
   (setq myVectorMachineLearning (^new Vector: 0))
   (setq myMathFunctions (^new Vector: 0))
   (setq myTrigFunctions (^new Vector: 0))
   (setq myBitwiseFunctions (^new Vector: 0))
   (setq myFileFunctions (^new Vector: 0))
   (setq myMacros (^new Vector: 0))
   (setq myContextFunctions (^new Vector: 0))
   (setq myRepositoryFunctions (^new Vector: 0))

   ;; varibales for timing
   (setq timeEx 0)
   (setq timeFunc 0)
   (setq timeKW 0)
   (setq timeFuncEx 0)
   (setq timeFuncSyn 0)
   (setq timeDesc 0)
   (setq timeFuncLink 0)
   (setq timeExDesc 0)
   (setq timeExLink 0)
   (setq timeExKW 0)
   (setq timeExTable 0)
   (setq timeExLinks 0)
   (setq timeExSyn 0)

   ;; loop for all function in Original Functions in Foundry  
   (loop for m from 0 until M do
      ;; check out function pages to get function information   
      (setq myCurrentFunctionHtml  (browseLib.checkout myOntology myFunctions[m])) 
      
      (setq myCurrentFunctionHtml (substitute (substitute myCurrentFunctionHtml {<![CDATA[} "") {]]>} ""))
      (setq myCurrentFunctionObject (xml myCurrentFunctionHtml ))
      ;; get current function name
      (setq myCurrentFunction (trim myCurrentFunctionObject.Function.KnowledgeBase.FunctionName))              
      ;;(writeln "myCurrentFunction: " myCurrentFunction) ;; LTNTemp
      ;; get current data type
      (setq myCurrentFunctionType (trim myCurrentFunctionObject.Function.KnowledgeBase.DataType))  
      ;; get current function type
      (setq myCurrentFunctionSpecialType  (trim myCurrentFunctionObject.Function.KnowledgeBase.FunctionType))
       
      ;; add additional Function pages
      (setq tempString (string myCurrentFunctionType))    
      (setq myVector (stringToVector (clean tempString) (append " " #\newline #\return #\tab)  true))
      (setq dataLength (length myVector))
      
      ;; loop for each supported data type in the function
      (loop for dataNum from 0 until dataLength  
          ;; current data type for the function
          (setq functionType myVector[dataNum])
          (setq newName (append  myFunctions[m] "_" myVector[dataNum]))           
          ;; loop for the example names          
          (loop for b from 0 until exLength  
              ;;(setq myCurrentExampleHtml  (browseLib.checkout myOntology myExamples[b]))            
              (setq exName myExamples[b])                           
              (setq tempExVector (stringToVector exName "_"))
              (setq exTypeName tempExVector[1])
              (setq exampleName tempExVector[2])              
              (if (and (stringCiEQ functionType exTypeName) (compareEQ exampleName myCurrentFunction))
                  (begin   
                      ;;(setq myCurrentExampleHtml  (browseLib.checkout myOntology myExamples[b]))                
                      ;; Setting the Function:Examples Dictionary                                         
                      (setq keyword newName)
                      (setq reference exName)
                      (setq reference (symbol reference))
                      (setq keyword (symbol keyword))       
                 
                      (if  (not (isVector myExampleDictionary[keyword]))
                           (begin
                              (setq myExampleDictionary[keyword] (new Vector: Object:))
                            )
                      ) ;; end of if condition
                      ;; set the Function Title to the Cross Reference Vector
                     (setq temporary myExampleDictionary[keyword])
                     (if (not (isMember reference temporary))
                         (begin
                             (setq temporary[(length temporary)] reference )
                          ) ;; end begin
                     ) ;; end if   
                   
                    ;; Generating the Example Page 
                    (setq startTimeEx (getTickCount timeEx)) 
                    (setq myCurrentExampleHtml  (browseLib.checkout myOntology myExamples[b]))                                                        
                    (setq myCurrentExampleHtml (substitute (substitute myCurrentExampleHtml {<![CDATA[} "" )  {]]>} "")   )   
                    (setq myCurrentExampleObject (xml myCurrentExampleHtml))           
                    (setq htmlPage (browseLib.checkout myTemplates "Template_Example"))                                                               
                    (setDescDictionary)
				 
                    (setq htmlPage (evalAllScripts htmlPage)) 
 
                    ;;(writeln " myExamples[b] : "  myExamples[b] )
                    (browseLib.checkin myHTMLOutput myExamples[b] htmlPage)
 
                    (setq endTimeEx (getTickCount startTimeEx)) 
                    (setq timeEx endTimeEx)                                                                                                       
               ) ;; end begin
            ) ;; end if function and example match 
                         
          ) ;; end loop for myExamples
 

          ;; setting the Vector of Vector Machine Learning Functions
          (if (isInteger (find "Vector Machine Learning" myCurrentFunctionSpecialType))
              (setq myVectorMachineLearning[(length myVectorMachineLearning)] newName))
          ;; setting the Vector of Math Functions
          (if (isInteger (find "(Math)" myCurrentFunctionSpecialType))
              (setq myMathFunctions[(length myMathFunctions)] newName))
          ;; setting the Vector of Trig Functions
          (if (isInteger (find "(Trig)" myCurrentFunctionSpecialType))
              (setq myTrigFunctions[(length myTrigFunctions)] newName))
          ;; setting the Vector of Bitwise Functions
          (if (isInteger (find "(Bitwise)" myCurrentFunctionSpecialType))
              (setq myBitwiseFunctions[(length myBitwiseFunctions)] newName))
         ;; setting the Vector of FileIO Functions
          (if (isInteger (find "(FileIO)" myCurrentFunctionSpecialType))
              (setq myFileFunctions[(length myFileFunctions)] newName))
         ;; setting the Vector of Macros 
          (if (compareEQ myCurrentFunctionSpecialType "Macro")
              (setq myMacros[(length myMacros)] newName))
         ;; setting the Vector of Context Functions 
          (if (compareEQ myVector[dataNum] "Context")
              (setq myContextFunctions[(length myContextFunctions)] newName))
         ;; setting the Vector of Repoository Functions
          (if (compareEQ myVector[dataNum] "ObjectRepository")
              (setq myRepositoryFunctions[(length myRepositoryFunctions)] newName))
         
          ;; Generating the Function Page 
         (setq startTimeFunc (getTickCount timeFunc))        
         (setq htmlPage (browseLib.checkout myTemplates "Template_Function")) 
         (setq newFunctions[(length newFunctions)] newName)                                       
         (setq htmlPage (evalAllScripts htmlPage))                    
         (browseLib.checkin myHTMLOutput newName htmlPage) 
         (setq endTimeFunc (getTickCount startTimeFunc)) 
         (setq timeFunc endTimeFunc)    
         ;;(writeln "function checked-in: " newName)           
       ) ;; end loop for additional functions        
    ) ;; end loop for  original functions in core content


  ;; Generate the Datatype Pages  
   (setq timeDataLink 0)
   (setq timeDataAll 0)
   (setq timeDataEx 0)
   (setq timeDataFunc 0)
   (setq timeDataSec 0)
   (setq startTimeDType (getTickCount 0)) 
   (setq N (length myDatatypes)) 
   (loop for n from 0 until N do           
      (setq myCurrentDatatypeHtml  (browseLib.checkout myOntology myDatatypes[n]))
      
      (setq myCurrentDatatypeHtml (substitute (substitute myCurrentDatatypeHtml {<![CDATA[} "") {]]>} "")) 
      (setq myCurrentDatatypeObject (xml myCurrentDatatypeHtml))
      (setq htmlPage (browseLib.checkout myTemplates "Template_Datatype"))
      (setq htmlPage (evalAllScripts htmlPage))
      (browseLib.checkin myHTMLOutput myDatatypes[n] htmlPage)
      ;;(writeln "datatype checked-in: " myDatatypes[n])
    ) ;; end loop
    (setq endTimeDType (getTickCount startTimeDType))

    ;; Generate the Document Pages  

   (setq timeDocLink 0) 
   (setq timeDocSec 0)
   (setq startTimeDoc (getTickCount 0))    
   (setq N (length myDocuments)) 
   (loop for n from 0 until N do           
      (setq myCurrentDocumentHtml  (browseLib.checkout myOntology myDocuments[n]))
       
      (setq myCurrentDocumentHtml (substitute (substitute myCurrentDocumentHtml {<![CDATA[} "") {]]>} ""))
      (setq myCurrentDocumentObject (xml myCurrentDocumentHtml))
      (setq htmlPage (browseLib.checkout myTemplates "Template_Document"))
      (setq htmlPage (evalAllScripts htmlPage))
      (browseLib.checkin myHTMLOutput myDocuments[n] htmlPage)
      ;;(writeln "documents checked-in: " myDocuments[n])
       
    ) ;; end loop
    (setq endTimeDoc (getTickCount startTimeDoc))

   ;; Generate the Essay 
   (setq timeEssAll 0)
   (setq timeEssSec 0)
   (setq timeEssBit 0)
   (setq timeEssCon 0)
   (setq timeEssFile 0)
   (setq timeEssMac 0)
   (setq timeEssMath 0)
   (setq timeEssRep 0)
   (setq timeEssTrig 0)
   (setq startTimeEssay (getTickCount 0))
   (setq N (length myEssays)) 
   (loop for n from 0 until N do    
      (setq essayName myEssays[n])   
      ;;(writeln "essayName: " myEssays[n])           
      (setq myCurrentEssayHtml  (browseLib.checkout myOntology myEssays[n]))    
      
      (setq myCurrentEssayHtml (substitute (substitute myCurrentEssayHtml {<![CDATA[} "") {]]>} ""))    
      (setq myCurrentEssayObject (xml myCurrentEssayHtml))
      (setq myCurrentEssayLink  myCurrentEssayObject.Essay.KnowledgeBase.Title)
      (setq htmlPage (browseLib.checkout myTemplates "Template_Essay"))
      (setq htmlPage (evalAllScripts htmlPage))
      (browseLib.checkin myHTMLOutput myEssays[n] htmlPage)
     
    ) ;;end loop
    (setq endTimeEssay (getTickCount startTimeEssay))
   
   ;; Generate the new home page.
   (setq htmlPage (browseLib.checkout myTemplates "Page_Help_Home"))
   (setq htmlPage (evalAllScripts htmlPage))
   (browseLib.checkin myHTMLOutput "Page_Help_Home" htmlPage)
   (setq endTime (getTickCount startTime))
  
    VERBOSESWITCH::
   (if (compareEQ true myVerboseSwitch)
   (begin
   (writeln "Elapsed time for VM Description is " endTimeVMDesc " Seconds" )
   (writeln "Elapsed time for VM Instructions is " endTimeVMIns " Seconds" )
   (writeln "Elapsed time for VM Keyword Links is " endTimeVMKW " Seconds" )
   (writeln "Elapsed time for VM Link Table is " endTimeVMLink " Seconds" )
   (writeln "Elapsed time for VM Syntax Table is " endTimeVMSyn " Seconds" )
   (writeln "ELAPSED TIME FOR VM INSTRUCTIONS is " endTimeVM " Seconds" )
   (writeln " ------------------------------------ ") 
   (writeln "Elapsed time for Function Description is " endTimeDesc " Seconds" )
   (writeln "Elapsed time for Function KW is " endTimeKW " Seconds" )
   (writeln "Elapsed time for Function Examples is " endTimeFuncEx " Seconds" )
   (writeln "Elapsed time for Function Link Table is " endTimeFuncLink " Seconds" )
   (writeln "Elapsed time for Function SyntaxTable is " endTimeFuncSyn " Seconds" )
   (writeln "ELAPSED TIME FOR FUNCTION is " endTimeFunc " Seconds" )
   (writeln " ------------------------------------ ")
   (writeln "Elapsed time for Example Description is " endTimeExDesc " Seconds" )
   (writeln "Elapsed time for Example Function Link is " endTimeExLink " Seconds" )
   (writeln "Elapsed time for Example KW is " endTimeExKW " Seconds" )
   (writeln "Elapsed time for Example Link Table is " endTimeExTable " Seconds" )
   (writeln "Elapsed time for Example Links is " endTimeExLinks " Seconds" )
   (writeln "Elapsed time for Example Syntax Table is " endTimeExSyn " Seconds" )
   (writeln "ELAPSED TIME FOR EXAMPLE is " endTimeEx " Seconds" )
   (writeln " ------------------------------------ ")   
   (writeln "Elapsed time for Document Link Table is " endTimeDocLink " Seconds" )
   (writeln "Elapsed time for Document Section Link List is " endTimeDocSec " Seconds" )
   (writeln "ELAPSED TIME FOR DOCUMENTS is " endTimeDoc " Seconds" ) 
   (writeln " ------------------------------------ ")
   (writeln "Elapsed time for Essay AllSection Rendering is " endTimeEssAll " Seconds" )
   (writeln "Elapsed time for Essay SectionLink List is " endTimeEssSec " Seconds" )
   (writeln "Elapsed time for Bitwise Link Table is " endTimeEssBit " Seconds" )
   (writeln "Elapsed time for Context Link Table is " endTimeEssCon " Seconds" )
   (writeln "Elapsed time for File Link Table is " endTimeEssFile " Seconds" )
   (writeln "Elapsed time for Macro Link Table is " endTimeEssMac " Seconds" )
   (writeln "Elapsed time for Math Link Table is " endTimeEssMath " Seconds" )
   (writeln "Elapsed time for Repository Link Table is " endTimeEssRep " Seconds" )
   (writeln "Elapsed time for Trig Link Table is " endTimeEssTrig " Seconds" )
   (writeln "ELAPSED TIME FOR ESSAYS is " endTimeEssay " Seconds" ) 
   (writeln " ------------------------------------ ")
   (writeln "Elapsed time for Data Type Link Table is " endTimeDataLink " Seconds" )
   (writeln "Elapsed time for Data Type AllSection Rendering is " endTimeDataAll " Seconds" )
   (writeln "Elapsed time for Data Type Examples is " endTimeDataEx " Seconds" )
   (writeln "Elapsed time for Data Type Functions is " endTimeDataFunc " Seconds" )
   (writeln "Elapsed time for Data Type SectionLinks is " endTimeDataSec " Seconds" )
   (writeln "ELAPSED TIME FOR DATATYPES IS " endTimeDType " Seconds" ) 
   (writeln " ------------------------------------ ")
   (writeln "Total Elapsed time is " endTime " Seconds" )
   );; end begin
   ) ;; end if

Last::
(setq endTime (getTickCount startTime))
(writeln "AisRefGuide elapsed time is " (/ endTime 60) " Minutes" )     
 
true) ;; end aisRefGuide
































;;**EXPORTKEY**:aisRefGuide.%README
;#text#
;; *******************************************************************
;;  README
;;
;;  Analytic Information Server Reference Guide Document Manager
;;
;; *******************************************************************


[...under construction...]










































;;**EXPORTKEY**:aisRefGuide.bitwiseLinkTable
(defchild aisRefGuide:bitwiseLinkTable()  
;; *******************************************************************
;; summary:  Return an HTML table of Trig Function Links
;;
;; Args:     none
;;
;; Return:   htmlText   An HTML table of Function Links
;; *******************************************************************
   vars:(     N htmlText  ctr tempVector  funcName  
         ) ;; end of temporary variables 

   (setq startTimeEssBit (getTickCount timeEssBit))
 
   (setq N (length myBitwiseFunctions))
   (setq htmlText (new Vector: Byte: 1000000))

    (appendWriteln htmlText {<table width="100%"><tr>} _eol)
    (loop for ctr from 0 until N do  
        (if (and (<> ctr 0) (= (modi ctr 5) 0)) (appendWriteln htmlText "</tr><tr>" _eol)) 
        (setq tempVector (stringToVector myBitwiseFunctions[ctr] "_"))
        (setq funcName tempVector[1])
        (appendWriteln htmlText {<td><A HREF="} myBitwiseFunctions[ctr] ".html" {">} funcName "</A></td>")
    ) ;; end loop
    (appendWriteln htmlText {</table></tr>} _eol)

    (setq endTimeEssBit (getTickCount startTimeEssBit))
    (setq timeEssBit endTimeEssBit)
    
   htmlText) ;; end bitwiseLinkTable




 




























;;**EXPORTKEY**:aisRefGuide.contextLinkTable
(defchild aisRefGuide:contextLinkTable()  
;; *******************************************************************
;; summary:  Return an HTML table of Math Function Links
;;
;; Args:     none
;;
;; Return:   htmlText   An HTML table of Context Function Links
;; *******************************************************************
   vars:(     N htmlText  ctr tempVector  funcName  
         ) ;; end of temporary variables 
 
   (setq startTimeEssCon (getTickCount timeEssCon))

   (setq N (length myContextFunctions))
   (setq htmlText (new Vector: Byte: 1000000))

    (appendWriteln htmlText {<table width="100%"><tr>} _eol)
    (loop for ctr from 0 until N do  
        (if (and (<> ctr 0) (= (modi ctr 5) 0)) (appendWriteln htmlText "</tr><tr>" _eol)) 
        (setq tempVector (stringToVector myContextFunctions[ctr] "_"))
        (setq funcName tempVector[1])
        (appendWriteln htmlText {<td><A HREF="} myContextFunctions[ctr] ".html" {">} funcName "</A></td>")
    ) ;; end loop
    (appendWriteln htmlText {</table></tr>} _eol)

    (setq endTimeEssCon (getTickCount startTimeEssCon))
    (setq timeEssCon endTimeEssCon)
    
   htmlText) ;; end contextLinkTable




 




























;;**EXPORTKEY**:aisRefGuide.dataTypeLinkTable
(defchild aisRefGuide:dataTypeLinkTable()  
;; *******************************************************************
;; summary:  Return an HTML table of Data Type links.
;;
;; Args:     none
;;
;; Return:   htmlText   An HTML table of Data Type links.
;; *******************************************************************
 
  vars:(N htmlText n  ctr
             tempVector typeName
         ) ;; end of temporary variables 

   (setq startTimeDataLink (getTickCount timeDataLink))

 
   (setq N (length myDatatypes))
 
   (setq htmlText (new Vector: Byte: 1000000))
  

    (appendWriteln htmlText {<table width="100%"><tr>} _eol)
    (loop for ctr from 0 until N do  
        (if (and (<> ctr 0) (= (modi ctr 5) 0)) (appendWriteln htmlText "</tr><tr>" _eol))  
         (setq tempVector (stringToVector myDatatypes[ctr] "_"))
        (setq typeName tempVector[1]) 
        (appendWriteln htmlText {<td><A HREF="} myDatatypes[ctr] {.html">} typeName "</A></td>")
    ) ;; end loop
    (appendWriteln htmlText {</table></tr>} _eol)

   (setq endTimeDataLink (getTickCount startTimeDataLink))
   (setq timeDataLink endTimeDataLink)
   

htmlText) ;; end dataTypeLinkTable


































;;**EXPORTKEY**:aisRefGuide.datatypeAllSectionRendering
(defchild aisRefGuide:datatypeAllSectionRendering()  
;; *******************************************************************
;; summary:  Returns the Text and Links in each <Description> Field in
;;           the Essay object
;; Args:     none
;;
;; Return:   htmlText   Text and Links
;; *******************************************************************
   vars:( N K ctr htmlText
        ctrSection
         indexStartDesc
         indexEndDesc myCurrentSectionHtml
         myCurrentDescHtml indexSection indexNextSection
         htmlWriteText mySectionHeading indexStart
         ) ;; end of temporary variables 

(setq startTimeDataAll (getTickCount timeDataAll))
 
   (setq htmlText myCurrentDatatypeHtml)
     
   (setq htmlWriteText (new Vector: Byte: 1000000))
    
   (setq ctrSection 0)
   (setq indexSection 0)
   (setq indexNextSection 0)
   (while (isNumber ( setq indexSection (find "<Section>" htmlText indexSection )))
       (setq ctrSection (+ 1 ctrSection))
       (setq indexNextSection (find "<Section>" htmlText (+  10 indexSection )))
       ;; Checking for Pages with more than one Section
       (if (isNumber indexNextSection)
           (begin
               (setq myCurrentSectionHtml (substring htmlText indexSection  (sub1 indexNextSection) ))               
               (setq indexSection indexNextSection)
            ) ;; end begin
       else
           (begin 
               (setq indexNextSection (length htmlText))                
               (setq myCurrentSectionHtml (substring htmlText indexSection  (sub1 (find "</Section>" htmlText indexSection ))  ) )
               (setq indexSection (- (length htmlText)))   
           ) ;; end begin
       ) ;; end if
    
   ;; Displaying the Section Heading
    
   (setq myCurrentSectionObj (xml myCurrentSectionHtml))
    
   (setq mySectionHeading myCurrentSectionObj.Section.Heading)
    
   (appendWriteln htmlWriteText {<P><FONT COLOR="#0000ff"><H4><A NAME="S} ctrSection {"></A>} mySectionHeading {</H4></FONT></P>})
    

  ;; Checking the Description field in the Section
   (setq indexStart 0)
   (setq indexStartDesc 0)
   (setq indexEndDesc 0)
   (setq indexStartDesc ( find "<Description>" myCurrentSectionHtml indexStart))
   (setq indexEndDesc   ( find "</Description>" myCurrentSectionHtml indexStart))
   (setq myCurrentDescHtml (substring myCurrentSectionHtml (+ 13 indexStartDesc) (sub1 indexEndDesc)))    
   (appendWriteln htmlWriteText myCurrentDescHtml)  
   ;; parsing the <!docscripts>.....</docscript>
    

  (appendWriteln htmlWriteText {<P ALIGN="CENTER"><INPUT TYPE='button' VALUE='Top of Page' onClick='navigate("#TOP");'></P>})
 );; end of while loop
        
   (setq endTimeDataAll (getTickCount startTimeDataAll))
   (setq timeDataAll endTimeDataAll)
htmlWriteText) ;; end of while loop
  



 






























;;**EXPORTKEY**:aisRefGuide.datatypeExamples
(defchild aisRefGuide:datatypeExamples()
;; *****************************************************************************
;; summary:  Checks for the presence of a cross reference dictionary.
;;           Retrieves the Examples for all the Argument Types of the 
;;           currently evaluated Function.
;; Args:     none
;;
;; Return:   htmlText          HTML table of Example Pages 
;; *******************************************************************************

 vars: (  currentDatatype n N
          index htmlText tempVector
       );; end of temporary variables

(setq startTimeDataEx (getTickCount timeDataEx))
    ;; Create a Dictionary
    (if (not (isDictionary myCrossReferenceExTypeTable) )
       (begin
         (aisRefGuide.createXArgTypeTable)
     )) ;; end of if condition
 
     (setq htmlText (new Vector: Byte: 100000))
     (setq currentDatatype myCurrentDatatypeObject.Datatype.KnowledgeBase.Title)

     (if (compareNE "Text" currentDatatype)
         (begin
             (appendWriteln htmlText {<P>The } currentDatatype { object can be demonstrated by the following examples.</p>})
             (setq currentDatatype (string currentDatatype))
             (setq currentDatatype (symbol currentDatatype))     
             ;;(setq currentDatatype (append currentDatatype 58))
             (setq index (isBound myCrossReferenceExTypeTable currentDatatype))
             (if (isNumber index)
                (begin
                    (setq tempVector (ref myCrossReferenceExTypeTable index))         
                    (appendWriteln htmlText {<table width="100%"><tr>} _eol)
                    (setq N (length tempVector))
                    (loop for n from 0 until N 
                        (if (and (<> n 0) (= (modi n 4) 0)) (appendWriteln htmlText "</tr><tr>" _eol))               
                    
                        (appendWriteln htmlText {<td><A HREF="} tempVector[n] ".html" {">} 
                         tempVector[n]   "</A></td>" _eol)
                     ) ;; end of loop for myLength
                    (appendWriteln htmlText "</tr>" _eol)
                    (appendWriteln htmlText "</table>" _eol)       
                );; end begin
             ) ;; end if
          ) ;; end begin
        else
          (begin
             (appendWriteln htmlText {<P>The String functions also work with the Text Data Type. See the String Data Type examples.</p>}) 
           ) ;; end begin
        ) ;; end if

   (setq endTimeDataEx (getTickCount startTimeDataEx))
   (setq timeDataEx endTimeDataEx)
   


htmlText) ;; end exampleKeyWordLinks

































;;**EXPORTKEY**:aisRefGuide.datatypeFunctions
(defchild aisRefGuide:datatypeFunctions()
;; *****************************************************************************
;; summary:  Checks for the presence of a cross reference dictionary.
;;           Retrieves the Functions for all the Argument Types of the 
;;           currently evaluated VMInstruction.
;; Args:     none
;;
;; Return:   htmlText          HTML table of Functions 
;; *******************************************************************************

 vars: (   
           
         n N
         htmlText 
         currentDatatype
         funcName
         funcVectorName
         tempVector 
         index
       );; end of temporary variables

    
    (setq startTimeDataFunc (getTickCount timeDataFunc))   
    ;; Create a Dictionary
    (if (not (isDictionary myCrossReferenceDatatypeTable) )
       (begin
         (aisRefGuide.createXFunctionTable)
     )) ;; end of if condition

     
    ;;(writeln myCurrentDatatypeObject.Datatype.KnowledgeBase.Title)
     (setq htmlText (new Vector: Byte: 100000))
     (setq currentDatatype myCurrentDatatypeObject.Datatype.KnowledgeBase.Title) 
     (setq currentDatatype (string currentDatatype))
     (if (compareNE "Text" currentDatatype)
       (begin
         (appendWriteln htmlText {<P>The } currentDatatype { object can be demonstrated by the following functions.</p>})
         (setq currentDatatype (symbol currentDatatype))    
         (setq index (isBound myCrossReferenceDatatypeTable currentDatatype))
         (if (isNumber index)
            (begin
                (setq tempVector (ref myCrossReferenceDatatypeTable index))         
                (appendWriteln htmlText {<table width="100%"><tr>} _eol)
                (setq N (length tempVector))
                (loop for n from 0 until N 
                    (if (and (<> n 0) (= (modi n 4) 0)) (appendWriteln htmlText "</tr><tr>" _eol))               
                    (setq funcVectorName  (stringToVector tempVector[n] "_"))             
                    (setq funcName funcVectorName[1])           
                    (appendWriteln htmlText {<td><A HREF="} tempVector[n] ".html" {">} 
                     funcName   "</A></td>" _eol)
                 ) ;; end of loop for myLength
                (appendWriteln htmlText "</tr>" _eol)
               (appendWriteln htmlText "</table>" _eol)       
           );; end begin
         );; end if
       )  ;; end begin
     else
       (begin
           (appendWriteln htmlText {<P>The String functions also work with the Text Data Type.</p>})
        ) ;; end begin
    ) ;; end if

   (setq endTimeDataFunc (getTickCount startTimeDataFunc))
   (setq timeDataFunc endTimeDataFunc)
              
htmlText) ;; end functionKeyWordLinks































;;**EXPORTKEY**:aisRefGuide.datatypeSectionLinks
(defchild aisRefGuide:datatypeSectionLinks()  
;; *******************************************************************
;; summary:  Return an HTML table of Section links.
;;
;; Args:     none
;;
;; Return:   htmlText   An HTML table of Section links.
;; *******************************************************************
   vars:( N K ctr htmlText
        (SecStartTagLen 10)
         indexNextSection
         indexSection htmlWriteText
         indexSectionEnd  htmlText ctrSection
         ) ;; end of temporary variables 


(setq startTimeDataSec (getTickCount timeDataSec))
   (setq htmlWriteText (new Vector: Byte: 1000000))   
   (setq htmlText myCurrentDatatypeHtml)  
       
   ;; Get the string between the Section tags
    
   (setq indexSection 0)
   (setq indexNextSection 0)
   (setq ctrSection 0)
 
   ;; Formatting for the Section Links
    

         
   (while (isNumber ( setq indexSection (find "<Section>" htmlText indexSection )))
       (setq ctrSection (+ 1 ctrSection))
       (setq indexNextSection (find "<Section>" htmlText (+  SecStartTagLen indexSection )))
       ;; Checking for Pages with more than one Section
       (if (isNumber indexNextSection)
           (begin
               (setq myCurrentSectionHtml (substring htmlText indexSection  (sub1 indexNextSection) ))
               ;;(writeln " there is next section: " myCurrentSectionHtml)
               (setq indexSection indexNextSection)
            ) ;; end begin
       else
           (begin 
               (setq indexNextSection (length htmlText))                
               (setq myCurrentSectionHtml (substring htmlText indexSection  (sub1 (find "</Section>" htmlText indexSection ))  ) )
               (setq indexSection (- (length htmlText)))   
           ) ;; end begin
       ) ;; end if
    
      ;; (writeln "myCurrentSectionHtml: " myCurrentSectionHtml)
       ;; Retrieving the Heading field in the Section Object
         
       (setq myCurrentSectionObj (xml myCurrentSectionHtml)) 
         
       ;;(writeln " myCurrentSectionHeading: " myCurrentSectionHeading)
       (appendWriteln htmlWriteText {<li><a href="#S} ctrSection {"><FONT SIZE=2>})
       (setq myCurrentSectionHeading  myCurrentSectionObj.Section.Heading)
        
        
       (appendWriteln htmlWriteText myCurrentSectionHeading)
       (appendWriteln htmlWriteText {</font></a></li>})
        
   ) ;; end of while loop
   (setq endTimeDataSec (getTickCount startTimeDataSec))
   (setq timeDataSec endTimeDataSec)
  
 
 htmlWriteText) ; end essaySectionLink































;;**EXPORTKEY**:aisRefGuide.documentLinkTable
(defchild aisRefGuide:documentLinkTable()  
;; *******************************************************************
;; summary:  Return an HTML table of Section links.
;;
;; Args:     none
;;
;; Return:   htmlText   An HTML table of Section links.
;; *******************************************************************
   vars:(  N n htmlText newDocName tempVector   d D
         ) ;; end of temporary variables 

   (setq startTimeDocLink (getTickCount timeDocLink))

   (setq htmlText (new Vector: Byte: 1000000))
   (setq N (length myDocuments))
     

   (appendWriteln htmlText {<table width="100%"><tr>} _eol)
   (loop for n from 0 until N do   
      (setq docTitle myDocuments[n])       
      (if (and (<> n 0) (= (modi n 5) 0)) (appendWriteln htmlText "</tr><tr>" _eol))
      ;; Displaying Document Link
      (appendWriteln htmlText {<td><H4><A HREF="} docTitle {.html">})
      ;; Displaying the Document Name
      (setq tempVector (stringToVector docTitle "_"))    
      (setq D (length tempVector))
      (loop for d from 1 to D    
          (if (= (isType Void: tempVector[d]) false)         
              (setq newDocName (append newDocName tempVector[d] " "))         
          )  
       ) ;; end loop
      (appendWriteln htmlText  newDocName{</H4></A></td>}) 
      (setq newDocName "")    
   ) ;; end loop
   (appendWriteln htmlText {</table></tr>} _eol)

   
  (setq endTimeDocLink (getTickCount startTimeDocLink))
  (setq timeDocLink endTimeDocLink)
   
  
 htmlText) ;;end documentLinkTable
































;;**EXPORTKEY**:aisRefGuide.documentSectionLinkList
(defchild aisRefGuide:documentSectionLinkList()  
;; *******************************************************************
;; summary:  Return an HTML table of Section links.
;;
;; Args:     none
;;
;; Return:   htmlText   An HTML table of Section links.
;; *******************************************************************
   vars:( htmlWriteText 
          indexEssay 
          htmlText
          indexNextEssay 
          n N d D
          (EssayStartTagLen 7)
          myCurrentEssayHtml 
          newEssayName 
          tempVector
          tempDocVector
          docTitle
          docName
          newDocName
         titleLength
         ) ;; end of temporary variables 

   
  
   (setq startTimeDocSec (getTickCount timeDocSec)) 
   (setq htmlWriteText (new Vector: Byte: 1000000))   
   (setq htmlText myCurrentDocumentHtml)
 
 
   ;; Get the string between the Essay tags    
   (setq indexEssay 0)
   (setq indexNextEssay 0)
   ;; Formatting for the Essay Links         
   (while (isNumber ( setq indexEssay  (find "<Essay>" htmlText indexEssay  )))      
       (setq indexNextEssay (find "<Essay>" htmlText (+  EssayStartTagLen indexEssay )))
       ;; Checking for Pages with more than one Section
       (if (isNumber indexNextEssay )
           (begin
               (setq myCurrentEssayHtml (substring htmlText indexEssay indexNextEssay 11 )) 
               (setq myCurrentEssayHtml (substring myCurrentEssayHtml (+ 7 (find "<Essay>" myCurrentEssayHtml 0)) (sub1 (find "</Essay>" myCurrentEssayHtml 0))))          
               (setq indexEssay indexNextEssay)
            ) ;; end begin
       else
           (begin 
               (setq indexNextEssay (length htmlText))                
               (setq myCurrentEssayHtml (substring htmlText (+ 7 indexEssay) (sub1 (find "</Essay>" htmlText indexEssay ))  ) )
               (setq indexEssay (- (length htmlText)))           
           ) ;; end begin
       ) ;; end if
 
       ;; Getting new Essay Name
       (setq tempVector (stringToVector myCurrentEssayHtml "_"))
       (setq N (length tempVector))
       (loop for n from 1 to N
            (if (isEqual false (isType Void: tempVector[n]))
                (setq newEssayName (append newEssayName tempVector[n] " "))
             )
       ) ;; end for loop
        
       ; Displaying the Essay Link
       (appendWriteln htmlWriteText {<LI><A HREF="} myCurrentEssayHtml {.html" target="ShowEssay">})
       (appendWriteln htmlWriteText {<FONT SIZE=2>}newEssayName{</FONT></A></LI>})
       (setq newEssayName "")
   ) ;; end of while loop

   (setq endTimeDocSec (getTickCount startTimeDocSec))
   (setq timeDocSec endTimeDocSec)

  
 
 htmlWriteText) ;;end essaySectionLink
































;;**EXPORTKEY**:aisRefGuide.essayAllSectionRendering
 (defchild aisRefGuide:essayAllSectionRendering()  
;; *******************************************************************
;; summary:  Returns the Text and Links in each <Description> Field in
;;           the Essay object
;; Args:     none
;;
;; Return:   htmlText   Text and Links
;; *******************************************************************
   vars:( N K ctr htmlText
        ctrSection
         indexStartDesc  
         indexEndDesc myCurrentSectionHtml
         myCurrentDescHtml indexSection indexNextSection
         htmlWriteText mySectionHeading indexStart
         indexFirstSection htmlText
         indexKbaseStart indexKbaseEnd kBase
         ) ;; end of temporary variables 

    
   (setq startTimeEssAll (getTickCount timeEssAll))
 
   (setq htmlText myCurrentEssayHtml)
     
   (setq htmlWriteText (new Vector: Byte: 1000000))

     
   ;; checking KnowledgeBase
   (setq indexKbaseStart (find "<KnowledgeBase>" htmlText 0))
   (setq indexKbaseEnd (find "</KnowledgeBase>" htmlText 0))
   (setq kBase (substring htmlText indexKbaseStart (+ 16 indexKbaseEnd)))
   (if (or (isBoolean indexKbaseStart) (isBoolean indexKbaseEnd))
      (begin 
         (error "aisRefGuide.essayAllSectionRendering: missing <KnowledgeBase> or </KnowledgeBase> tag " "The error is in: " essayName " [ " kBase "]")
      ) ;; end begin
   ) ;; end if

   
   ;; checking double </KnowledgeBase> tags
   (if (isNumber (find "</KnowledgeBase>" htmlText (+ 16 indexKbaseEnd)))
      (begin
          (error "aisRefGuide.essayAllSectionRendering: double </KnowledgeBase> tags "  "The error is in: " essayName   " [ " kBase "]")
      ) ;; end begin
   ) ;; end if

    

   ;; find <Title> .. </Title> tags
   (setq indexTitleStart (find "<Title>" kBase 0))
   (if (isBoolean indexTitleStart) (error "aisRefGuide.essayAllSectionRendering: missing <Title> tag " "The error is in: " essayName  " [ " kBase "]"))
   (setq indexTitleEnd (find "</Title>" kBase indexTitleStart)) 
   (if (isBoolean indexTitleEnd) (error "aisRefGuide.essayAllSectionRendering: missing </Title> tag " "The error is in: " essayName    " [ " kBase "]"))
   (if (isNumber (find "</Title>" kBase (+ 8 indexTitleEnd))) (error "aisRefGuide.essayAllSectionRendering: double </Title> tags "  "The error is in:  " essayName    " [ " kBase "]"))



   ;; find <Topic> .. </Topic> tags
   (setq indexTopicStart (find "<Topic>" kBase 0)) 
   (if (isBoolean indexTopicStart) (error "aisRefGuide.essayAllSectionRendering: missing <Topic> tag " "The error is in:  " essayName    " [ " kBase "]"))
   (setq indexTopicEnd (find "</Topic>" kBase indexTopicStart))  
   (if (isBoolean indexTopicEnd) (error "aisRefGuide.essayAllSectionRendering: missing </Topic> tag " "The error is in:  " essayName    " [ " kBase "]"))
   (if (isNumber (find "</Topic>" kBase (+ 8 indexTopicEnd))) (error "aisRefGuide.essayAllSectionRendering: double </Topic> tags " "The error is in:  " essayName     " [ " kBase "]"))

 
 
   ;; find <sTopic> .. </sTopic> tags
   (setq indexsTopicStart (find "<SubTopic>" kBase 0))
   (if (isBoolean indexsTopicStart) (error "aisRefGuide.essayAllSectionRendering: missing <SubTopic> tag " "The error is in:  " essayName    " [ " kBase "]"))
   (setq indexsTopicEnd (find "</SubTopic>" kBase indexsTopicStart)) 
   (if (isBoolean indexsTopicEnd) (error "aisRefGuide.essayAllSectionRendering: missing </SubTopic> tag " "The error is in:  " essayName    " [ " kBase "]"))
   (if (isNumber (find "</SubTopic>" kBase (+ 8 indexsTopicEnd))) (error "aisRefGuide.essayAllSectionRendering: double </SubTopic> tags " "The error is in: " essayName    " [ " kBase "]"))

 

   ;; find <HumanKeywords> .. </HumanKeywords> tags
   (setq indexHKStart (find "<HumanKeywords>" kBase 0))
   (if (isBoolean indexHKStart) (error "aisRefGuide.essayAllSectionRendering: missing <HumanKeywords> tag " "The error is in:  " essayName    " [ " kBase "]"))
   (setq indexHKEnd (find "</HumanKeywords>" kBase indexHKStart)) 
   (if (isBoolean indexHKEnd) (error "aisRefGuide.essayAllSectionRendering: missing </HumanKeywords> tag " "The error is in:  " essayName   " [ " kBase "]"))
   (if (isNumber (find "</HumanKeywords>" kBase (+ 8 indexHKEnd))) (error "aisRefGuide.essayAllSectionRendering: double </HumanKeywords> tags " "The error is in:  " essayName     " [ " kBase "]"))
 
  
    
   (setq ctrSection 0)
   (setq indexSection 0)
   (setq indexNextSection 0)

   (while (isNumber ( setq indexSection (find "<Section>" htmlText indexSection )))
       (setq ctrSection (+ 1 ctrSection))
       (setq indexNextSection (find "<Section>" htmlText (+  10 indexSection )))

       ;; Checking for Pages with more than one Section
       (if (isNumber indexNextSection)
           (begin
               (setq myCurrentSectionHtml (substring htmlText indexSection  (sub1 indexNextSection) ))  
               ;; looking for first </Section> tag
               (setq indexFirstSection (find "</Section>" myCurrentSectionHtml 0 ))                
               (setq indexSection indexNextSection)
            ) ;; end begin
       else
           (begin 
               (setq indexNextSection (length htmlText))                
               (setq myCurrentSectionHtml (substring htmlText indexSection  (+ 10 (find "</Section>" htmlText indexSection ))  ) )
               ;; looking for first </Section> tag
               (setq indexFirstSection (find "</Section>" myCurrentSectionHtml 0 ))
               (setq indexSection (- (length htmlText)))   
           ) ;; end begin
       ) ;; end if

      

       ;; check for missing </Section> tag
       (if (isBoolean (find "</Section>" myCurrentSectionHtml))
           (begin
               (error "aisRefGuide.essayAllSectionRendering: missing end of Section " "The error is in: " essayName  " [ " myCurrentSectionHtml "]")
            ) ;; end begin
        )  ;; end if 
       ;; checking for double </Section> tags
  
       (if (isNumber (find "</Section>" myCurrentSectionHtml  (+ 10 indexFirstSection)))
           (begin
               (error "aisRefGuide.essayAllSectionRendering: double </Section> tags " "The error is in: " essayName   " [ " myCurrentSectionHtml "]")
            );; end begin
       );; end if   
    
       ;; Displaying the Section Heading            
       (setq myCurrentSectionObj (xml myCurrentSectionHtml))     
       (setq mySectionHeading myCurrentSectionObj.Section.Heading)    
       (appendWriteln htmlWriteText {<P><H2><A NAME="S} mySectionHeading {"></A>} mySectionHeading {</H2></P>})
    

      ;; Checking the Description field in the Section
       (setq indexStart 0)
       (setq indexStartDesc 0)
       (setq indexEndDesc 0)
       (setq indexStartDesc ( find "<Description>" myCurrentSectionHtml indexStart))
       (setq indexEndDesc   ( find "</Description>" myCurrentSectionHtml indexStart))

       (setq myCurrentDescHtml (substring myCurrentSectionHtml (+ 13 indexStartDesc) (sub1 indexEndDesc)))

       ;; checking for </Description> tag
       (if (isBoolean indexEndDesc)
               (begin
                   (error "aisRefGuide.essayAllSectionRendering: missing end of Description " "The error is in: " essayName  " [ " (mid myCurrentSectionHtml indexStartDesc 1000)   " ... " )
               ) ;; end begin 
        )  ;; end if   
       ;; checking for double </Description> tags
       (if (isNumber (find "</Description>" myCurrentSectionHtml  (+ 14 indexEndDesc)))
           (begin
               (error "aisRefGuide.essayAllSectionRendering:  double </Description> tags " "The error is in: " essayName " [ " myCurrentDescHtml "]")
            );; end begin
       );; end if   

    

       (if (isNumber (find "<!docscripts>" myCurrentDescHtml))
           (begin
            (appendWriteln htmlWriteText (evalAllScripts myCurrentDescHtml)))
       else
           (begin
            (appendWriteln htmlWriteText  myCurrentDescHtml))
       ) ;; end if
     (appendWriteln htmlWriteText {<P ALIGN="CENTER"><INPUT TYPE='button' VALUE='Top of Page' onClick='navigate("#TOP");'></P>})
 );; end of while loop

   (setq endTimeEssAll (getTickCount startTimeEssAll))
   (setq timeEssAll endTimeEssAll)
    
        
htmlWriteText)  ;; end of essayAllSectionRendering



  
  



 























;;**EXPORTKEY**:aisRefGuide.essaySectionLinkList
(defchild aisRefGuide:essaySectionLinkList()  
;; *******************************************************************
;; summary:  Return an HTML table of Section links.
;;
;; Args:     none
;;
;; Return:   htmlText   An HTML table of Section links.
;; *******************************************************************
   vars:( N K ctr htmlText
        (SecStartTagLen 10)
         indexNextSection
         indexSection htmlWriteText
         indexSectionEnd  htmlText ctrSection
         ) ;; end of temporary variables 


   (setq startTimeEssSec (getTickCount timeEssSec))
   (setq htmlWriteText (new Vector: Byte: 1000000))   
   (setq htmlText myCurrentEssayHtml)  
       

   ;; Get the string between the Section tags
    
   (setq indexSection 0)
   (setq indexNextSection 0)
   (setq ctrSection 0)
 
   ;; Formatting for the Section Links
    

         
   (while (isNumber ( setq indexSection (find "<Section>" htmlText indexSection )))
       (setq ctrSection (+ 1 ctrSection))
       (setq indexNextSection (find "<Section>" htmlText (+  SecStartTagLen indexSection )))
       ;; Checking for Pages with more than one Section
       (if (isNumber indexNextSection)
           (begin
               (setq myCurrentSectionHtml (substring htmlText indexSection  (sub1 indexNextSection) ))
               ;;(writeln " there is next section: " myCurrentSectionHtml)
               (setq indexSection indexNextSection)
            ) ;; end begin
       else
           (begin 
               (setq indexNextSection (length htmlText))                
               (setq myCurrentSectionHtml (substring htmlText indexSection  (sub1 (find "</Section>" htmlText indexSection ))  ) )
               (setq indexSection (- (length htmlText)))   
           ) ;; end begin
       ) ;; end if
    
      ;; (writeln "myCurrentSectionHtml: " myCurrentSectionHtml)
       ;; Retrieving the Heading field in the Section Object
         
       (setq myCurrentSectionObj (xml myCurrentSectionHtml)) 
         
       ;;(writeln " myCurrentSectionHeading: " myCurrentSectionHeading)
       
       (setq myCurrentSectionHeading  myCurrentSectionObj.Section.Heading)
       (appendWriteln htmlWriteText {<li><a href="#S} myCurrentSectionHeading {"><FONT SIZE=2>}) 
         
       (appendWriteln htmlWriteText myCurrentSectionHeading)
       (appendWriteln htmlWriteText {</font></a></li>})
        
   ) ;; end of while loop

   (setq endTimeEssSec (getTickCount startTimeEssSec)) 
   (setq timeEssSec endTimeEssSec)
 
 htmlWriteText) ; end essaySectionLink
































;;**EXPORTKEY**:aisRefGuide.evalAllScripts
(defchild aisRefGuide:evalAllScripts(htmlPage)  
;; ***************************************************************************************
;; summary:  Evaluate any AIS Ref Guide scripts in the specified HTML Page.
;;
;;           AIS Ref Guide scripts always appear with the following syntax:
;;           <!--docscript>...Lisp expression...</docscript-->
;;
;;           For each AIS Ref Guide script, the Lisp expression is evaluated,
;;           and the result is substituted IN PLACE OF the original AIS Ref Guide
;;           script including the leading <!--docscript> tag and the trailing 
;;           </docscript--> tag. 
;;
;;           Note: If only script evaluation, but no substitution is desired, then
;;                 Lisp expression should be terminated with the term "":
;;                 <!--docscript>(doSomething) ""</docscript-->
;;
;; Args:     htmpPage	The HTML page containing zero or more AIS Ref Guide scripts.
;;
;; Return:   htmlPage   The HTML page after all AIS Ref Guide scripts have been evaluated.
;; ***************************************************************************************
   regs:(k K m M n N
         ) ;; end of register variables 
   vars:(htmlTarget htmlText
         script scriptResult
         (startTag "<!--docscript>")
         (stopTag "</docscript-->")
         ) ;; end of temporary variables 

 
   ;; Examine the HTML page for any AIS Ref Guide scripts.
   ;; Note: We keep looking for scripts until we do not find anymore.
   ;;       We also look in the substituted string, since the result
   ;;       may contain recursively imbedded AIS Ref Guide scripts.
   (setq k 0)
   (while (isNumber (setq n (find startTag htmlPage k)))
      (begin
 
         (setq m (find stopTag htmlPage (+ n (length startTag))))
         (if (isBoolean m) (error (append "aisRefGuide.evalAllScripts: missing </docscript--> tag at [" (mid htmlPage n 40) "]")))
         (setq script (mid htmlPage (+ n (length startTag)) (- m (+ n (length startTag)))))       
         (setq script ( eval (compile (lisp (append "(lambda () pvars:(main document) " script ")")))))    
         (setq script.Pv.main aisRefGuide)
         (setq script.Pv.document myCurrentOntologyObject)
         (setq scriptResult (script ))
      
         (setq htmlPage (append (mid htmlPage 0 n) scriptResult (mid htmlPage (+ m (length stopTag)) (length htmlPage))))
         
       ;;(setq htmlPage (append (mid htmlPage 0 n) scriptResult (mid htmlPage (+ m (length stopTag)) (length htmlPage))))
         (setq k n)
    
      )
   ) ; end while
 
 
   htmlPage) ; end evalAllScripts







































;;**EXPORTKEY**:aisRefGuide.exampleDescription
(defchild aisRefGuide:exampleDescription()  
;; *****************************************************************************
;; summary:  Returns the Description of each Example  
;;
;; Args:     none
;;
;; Return:   htmlWriteText   The content of <Description>...</Description> 
;;                           in the Example pages in the Foundry Folder  
;; *******************************************************************************
   vars:(
        htmlText
        htmlWriteText
         ) ;; end of temporary variables 
  
   (setq startTimeExDesc (getTickCount timeExDesc))

   (setq htmlText myCurrentExampleHtml) 
   ;;(writeln "in Example Description: " htmlText)
   (setq index1 (find "<Description>" htmlText ))
   (setq index2 (find "</Description>" htmlText ))
   (setq htmlText (substring htmlText index1 (+ 15 index2)) )
   ;;(writeln "inside exampleDescription: " htmlText)
   (setq htmlWriteText htmlText)

   (setq endTimeExDesc (getTickCount startTimeExDesc))
   (setq timeExDesc endTimeExDesc)
 
   htmlWriteText) ;; end exampleDescription





































;;**EXPORTKEY**:aisRefGuide.exampleFunctionLinks
(defchild aisRefGuide:exampleFunctionLinks()
;; *****************************************************************************
;; summary:  Returns links to the current function and the related functions
;;           of the example ontology currently evaluated
;;            
;; Args:     none
;;
;; Return:   htmlText          HTML Table of Function Links
;; *******************************************************************************
    vars:( htmlText
           functionName
           myName
           tempString
           tempVector
           vecLength n 
           ;; displayFunc
           myFunctionName )


   (setq startTimeExLink (getTickCount timeExLink))  
  ;; setting the variables
  (setq htmlText (new Vector: Byte: 1000000))
  (setq functionName myCurrentExampleObject.Example.KnowledgeBase.FunctionKeywords)

  ;; Link to the current function in the example
  (setq myName (append funcPrefix functionName "_" functionType ))
  (appendWriteln htmlText {Here is the link to the <b>current function</b> used in this example. <br>})
  (appendWriteln htmlText {<p><A HREF="} myName ".html" {">} functionName "</A></p>" _eol )
  
  ;; Links to other related functions
  (appendWriteln htmlText {<p>Here are a number of links to other <b>related functions</b>.</p>}  _eol )
  (setq tempString myCurrentExampleObject.Example.KnowledgeBase.RelatedFunctions)
  (setq tempVector (new Vector: Object: ))
  (setq tempVector (stringToVector (clean tempString) (append " " #\newline #\return #\tab)  true))
  (setq vecLength (count tempVector))
  (appendWriteln htmlText {<table width="100%"><tr>} _eol)
  (loop for n from 0 until vecLength
     (if (and (<> n 0) (= (modi n 4) 0)) (appendWriteln htmlText "</tr><tr>" _eol))
     (setq myFunctionName (append funcPrefix tempVector[n] "_" functionType ))
     ;;(setq displayFunc (substring myFunctionName 9))
     (appendWriteln htmlText {<td><A HREF="} myFunctionName ".html" {">} tempVector[n] "(" functionType ")</A></td>" _eol )      
   ) ;; end loop
   (appendWriteln htmlText "</tr>" _eol)
   (appendWriteln htmlText "</table>" _eol)

   (setq endTimeExLink (getTickCount startTimeExLink))
   (setq timeExLink endTimeExLink)

htmlText) ;; end exampleFunctionLinks


































;;**EXPORTKEY**:aisRefGuide.exampleKeyWordLinks
(defchild aisRefGuide:exampleKeyWordLinks()
;; *****************************************************************************
;; summary:  Checks for the presence of a cross reference dictionary.
;;           Retrieves the Examples for all the Argument Types of the 
;;           currently evaluated Function.
;; Args:     none
;;
;; Return:   htmlText          HTML table of Example Pages 
;; *******************************************************************************

 vars: ( foundIdx 
         currentText 
         p n myLength
         htmlText 
         tempString 
         tempVector 
         myLength 
         textLength
         currentVector 
         myLink       
       );; end of temporary variables

    (setq startTimeExKW (getTickCount timeExKW))
    ;; Create a Dictionary
    (if (not (isDictionary myCrossReferenceExArgTypeTable) )
      (begin
         (aisRefGuide.createXArgTypeTable)         
     )) ;; end of if condition

    ;; Argument Types of each Example
    (setq tempString myCurrentExampleObject.Example.KnowledgeBase.TypeKeywords)    
    (setq htmlText (new Vector: Byte: 100000))
    (setq currentVector (new Vector: Object: ))
    (setq tempVector (new Vector: Object: ))
    (setq tempVector (stringToVector (clean tempString) (append " " #\newline #\return #\tab)  true))
    (setq vecLength (count tempVector))

    (appendWriteln htmlText {<p> Here are the links to the <b> data types </b> of the arguments used in this example.  </p>} _eol)
    (appendWriteln htmlText {<table width="100%"><tr>} _eol)
    (loop for n from 0 until vecLength
        ;; write Argument Type
        (if (and (<> n 0) (= (modi n 4) 0)) 
                 (appendWriteln htmlText "</tr><tr>" _eol)) 
        (appendWriteln htmlText {<td><A HREF="} tempVector[n] ".html" {">} tempVector[n]   "</A></td>" _eol)
        ;; finding the matching Vector in the created Dictionary
        ;;(setq foundIdx (member tempVector[n] myCrossReferenceExArgTypeTable))
        (setq foundIdx (binarySearch myCrossReferenceExArgTypeTable tempVector[n] ))
        (setq currentText myCrossReferenceExArgTypeTable[foundIdx])
          
         
        (setq textLength (length currentText))
        (loop for nn from 0 until textLength
            ;;(if (not (isMember currentText[nn] currentVector ))
            ;;(begin
            ;;    (setq currentVector[(length currentVector)] currentText[nn])  
            ;; )) ;; end of if condition
            (binaryInsert currentVector currentText[nn])
         ) ;; end  of loop for textLength
    );; end of loop for vecLength
    (appendWriteln htmlText "</tr>" _eol)
    (appendWriteln htmlText "</table>" _eol)

    (setq K (length exPrefix))
    (setq myLength (length currentVector))
     
    (appendWriteln htmlText {<p> Here are a number of links to examples having <b>similar argument types</b>. 
  </p>} _eol)
    (appendWriteln htmlText {<table width="100%"><tr>} _eol)
    (loop for p from 0 until myLength 
        (setq myLink (substitute currentVector[p] #\space "_" ))
        (if (and (<> p 0) (= (modi p 4) 0)) 
                 (appendWriteln htmlText "</tr><tr>" _eol)) 
        (appendWriteln htmlText {<td><A HREF="} myLink ".html" {">} 
        (mid currentVector[p] K 10000)   "</A></td>" _eol)
          
     ) ;; end of loop for myLength

    (appendWriteln htmlText "</tr>" _eol)
    (appendWriteln htmlText "</table>" _eol)
     
     (setq endTimeExKW (getTickCount startTimeExKW))
     (setq timeExKW endTimeExKW)
htmlText) ;; end exampleKeyWordLinks


































;;**EXPORTKEY**:aisRefGuide.exampleLinkTable
(defchild aisRefGuide:exampleLinkTable()  
;; *******************************************************************
;; summary:  Return an HTML table of Example Pages in Core Content
;;
;; Args:     none
;;
;; Return:   htmlText   An HTML table of Example links.
;; *******************************************************************
   vars:(k K n N tempVector keyword reference temp  
         htmlText exampleTypes lengthTypes exNameVector 
         ctr p   numberOfExamples numEx functionSymbol
         ) ;; end of temporary variables 
   
   (setq startTimeExTable (getTickCount timeExTable))  

   (setq N (length myExamples))
   (setq K (length exPrefix))
   (setq htmlText (new Vector: Byte: 1000000))
   (setq myExampleDictionary (new Dictionary:))   
   (setq myExampleVector (new Vector:))

   ;; loop for all the Examples to create Dictionary
   (loop for n from 0 until N do   
      (setq tempVector (stringToVector myExamples[n] "_" )) 
      ;; Get the Function Type
      (setq keyword tempVector[1])
      ;; Get the Function Name
      (setq reference (append tempVector[2] "_" tempVector[3]))
      (setq keyword (symbol keyword))
      (setq reference (symbol reference))
      (if  (not (isVector myExampleDictionary[keyword]))
            (begin
                (setq myExampleDictionary[keyword] (new Vector: Object:))               
             )
       ) ;; end of if condition
       ;; set the Function Title to the Cross Reference Vector
       (setq temp myExampleDictionary[keyword])
       (if (not (isMember reference temp))
            (setq temp[(length temp)] reference)
        ) ;; end of if condition
    ) ;; end loop

    (setq exampleTypes (refAttributes myExampleDictionary))     
    (setq lengthTypes (length exampleTypes))
    ;; Display the Data Type Links
    (appendWriteln htmlText {<table width="100%"><tr>} _eol)
    (loop for ctr from 0 until lengthTypes do  
        (if (and (<> ctr 0) (= (modi ctr 5) 0)) (appendWriteln htmlText "</tr><tr>" _eol))  
        (appendWriteln htmlText {<td><A HREF="#} exampleTypes[ctr] {Examples">} exampleTypes[ctr]{ Examples </p> </A></td>})
    ) ;; end loop
    (appendWriteln htmlText {</table></tr>} _eol)

    (appendWriteln htmlText {<!--- Button which links to top of page --->
                              <P ALIGN="CENTER">
                              <INPUT TYPE='button' VALUE='Top of Page' onClick='scroll(0,0)'></P>})
    
    (loop for p from 0 until lengthTypes do
        (appendWriteln htmlText {<FONT COLOR="#0000ff"><H4><A NAME="} exampleTypes[p] {Examples"></A>}
                 exampleTypes[p] { Examples </H4></FONT>})
        (appendWriteln htmlText {<p>The sections which follow, show
                   all of the functions of the  } exampleTypes[p] { data type at work.</p>})
        (appendWriteln htmlText {<table width="100%"><tr>} _eol)
        (setq myExampleVector (ref myExampleDictionary exampleTypes[p]))
        (setq numberOfExamples (length myExampleVector))
        (appendWriteln htmlText {<table width="100%"><tr>} _eol)
        (loop for numEx from 0 until numberOfExamples do  
            (if (and (<> numEx 0) (= (modi numEx 5) 0)) (appendWriteln htmlText "</tr><tr>" _eol))
            (setq exNameVector (stringToVector myExampleVector[numEx] "_"))
            ;; for special function names
            (if (isInteger (find "(sym)" exNameVector[0]))
                (begin 
                  (setq functionSymbol (aisRefGuide.specialFunctionNames exNameVector[0]))
                  (setq functionSymbol (append functionSymbol "_" exNameVector[1]) ))                                   
                 else
                (begin (setq functionSymbol myExampleVector[numEx]))
             ) ;; end if for special function names                   
            (appendWriteln htmlText {<td><A HREF="} (append "Example_" exampleTypes[p] "_"
                     myExampleVector[numEx]) ".html" {">} functionSymbol "</A></td>")
        ) ;; end loop
        (appendWriteln htmlText {</table></tr>} _eol)
        (appendWriteln htmlText {<!--- Button which links to top of page --->
                              <P ALIGN="CENTER">
                              <INPUT TYPE='button' VALUE='Top of Page' onClick='scroll(0,0)'></P>})
       );; end loop

     (setq endTimeExTable (getTickCount startTimeExTable))
     (setq timeExTable endTimeExTable)

 htmlText) ; end exampleLinkTable
































;;**EXPORTKEY**:aisRefGuide.exampleLinks
(defchild aisRefGuide:exampleLinks()
;; *****************************************************************************
;; summary:  Checks for the presence of a cross reference dictionary.
;;           Retrieves the Examples with Function Keywords the same as that of the 
;;           currently evaluated Function.
;; Args:     none
;;
;; Return:   htmlText          HTML table of Example Pages 
;; *******************************************************************************

 vars: ( foundIdx 
         currentText 
         p n myLength
         htmlText 
         tempString 
         tempVector 
         myLength 
         textLength
         currentVector 
         currentVector2
         myLink       
       );; end of temporary variables
 

    (setq startTimeExLinks (getTickCount timeExLinks))
 
    ;; Create a Dictionary
    (if (not (isDictionary myCrossReferenceExFunctionTypeTable) )
       (begin
         (aisRefGuide.createXArgTypeTable)
     )) ;; end of if condition

    ;; Function Keywords of each Example
    (setq tempString myCurrentExampleObject.Example.KnowledgeBase.FunctionKeywords)
    (setq htmlText (new Vector: Byte: 100000))
    (setq currentVector (new Vector: Object: ))
    (setq tempVector (new Vector: Object: ))
    (setq tempVector (stringToVector (clean tempString) (append " " #\newline #\return #\tab)  true))
    (setq vecLength (count tempVector))
 
     
    (loop for n from 0 until vecLength
        (setq foundIdx (member tempVector[n] myCrossReferenceExFunctionTypeTable))
        (setq currentText myCrossReferenceExFunctionTypeTable[foundIdx])
        (setq textLength (length currentText))
        (loop for nn from 0 until textLength
            (if (not (isMember currentText[nn] currentVector ))
            (begin
                (setq currentVector[(length currentVector)] currentText[nn])  
             )) ;; end of if condition
         ) ;; end  of loop for textLength
    );; end of loop for vecLength

    (appendWriteln htmlText {<p>Here are examples of the <b>} tempString {</b> function at work. </p>} )
    (setq K (length exPrefix))
    


    (setq myLength (length currentVector))
     (if (compareEQ myLink myExamples[b])(delete currentVector myLink))
    (appendWriteln htmlText {<table width="100%"><tr>} _eol)
    (loop for p from 0 until myLength 
        (setq myLink (substitute currentVector[p] #\space "_" ))       
        (if (and (<> p 0) (= (modi p 4) 0)) 
                 (appendWriteln htmlText "</tr><tr>" _eol)) 
         (appendWriteln htmlText {<td><A HREF="} myLink ".html" {">} 
        (mid currentVector[p] K 10000)   "</A></td>" _eol)       
     ) ;; end of loop for myLength

   

    (appendWriteln htmlText "</tr>" _eol)
    (appendWriteln htmlText "</table>" _eol)
 
     (setq endTimeExLinks (getTickCount startTimeExLinks)) 
      (setq timeExLinks endTimeExLinks)

 

htmlText) ;; end exampleLinks






































;;**EXPORTKEY**:aisRefGuide.exampleSyntaxTable
(defchild aisRefGuide:exampleSyntaxTable()  
;; *****************************************************************************
;; summary:  Returns the Syntax of each Example with the notes and return value
;;           of each expression.
;;           Returns links to VMInstruction pages if usch instructions are used
;;           in the example.
;;
;; Args:     none
;;
;; Return:   htmlSyntaxTable   HTML Table containing the syntax and description of 
;;                             each expression of the example.
;; *******************************************************************************
   vars:( htmlSyntaxTable
          vmVector
          index1 index2 n   index3 index4   tagEnd
          indexNextExpr
          indexExpression
          htmlText
          htmlExpressionText
          myCurrentExpressionText
          myCurrentNoteText
          myCurrentReturnText
          (ExStartTagLen 12)
          (ExEndTagLen 13)
          (NoteStartTagLen 6)
          (NoteEndTagLen 7)
          (RtnEndTagLen 9)
          (RtnEndTagLen 10)
          tempVector                             
          hintsText
		  evalExpression
		  testEx
		  testExNew
		  testExLen
		 
		   mm macLen m nn
        ) ;; end of temporary variables 
 
	regs: ((CharPointer: expressionp) (Integer:nn 0) (Integer:x 0) (CharPointer:testExp)  (Integer:len 0)
			cc c1 c2 c3 (add 43) (null 0)
	)
   
   
   (setq startTimeExSyn (getTickCount timeExSyn)) 
   ;; defining the variables
   (setq htmlSyntaxTable (new Vector: Byte: 1000000))
   (setq vmVector (new Vector: Object:))
   (setq tempPrint (new Vector: Object:))
   (setq evalExpression (new String: ""))
   (setq testEx (new String: ""))
   ;(setq textExNew (new String: ""))
   (setq myCurrentExpressionText (new String: ""))
     
(setq tagStart 0)
   ;; Get the content inside the Syntax tags
   (setq index1 (find "<Syntax>" myCurrentExampleHtml ))
   (setq index2 (find "</Syntax>" myCurrentExampleHtml )) 
   (setq htmlText (substring myCurrentExampleHtml index1 (+ 9 index2)) )
  
   ;; Get the string between the Expression tags
   (setq indexExpression 0 )
   (setq indexNextExpr 0 )
   
   (while (isNumber (setq indexExpression (find "<Expression>" htmlText indexExpression )))
	   (setq evalExpression "")
		(setq htmlExpressionText "")
        (setq  myCurrentExpressionText "")
       (setq indexNextExpr (find "<Expression>" htmlText (+ ExStartTagLen indexExpression )))
       (if (isNumber indexNextExpr)
           (begin
               (setq htmlExpressionText (substring htmlText indexExpression indexNextExpr ))  
       
               (setq indexExpression indexNextExpr)              
           ) ;; end begin
       else
           (begin 
               (setq indexNextExpr (length htmlText))
               (setq htmlExpressionText (substring htmlText indexExpression (find "</Syntax>" htmlText indexExpression ) ) )  
               (setq indexExpression (- (length htmlText)))               
           ) ;; end begin
       ) ;; end if
       
        ;; parse Expression, Note and Return text 
       (string htmlExpressionText)
 
        (setq myCurrentExpressionText (substring htmlExpressionText (find "<Expression>" htmlExpressionText 1) (+ ExEndTagLen (find "</Expression>" htmlExpressionText 1))))       
  		;;(setq myCurrentExpressionText (substitute (substitute myCurrentExpressionText {<b>}  {<sc>}  )))
		;(setq myCurrentExpressionText (substitute (substitute myCurrentExpressionText {</b>}  {</sc>}  )))
		(setq tagEnd (- (length myCurrentExpressionText) 15))
 
		(setq tagStart 13)
		(while (<>  myCurrentExpressionText[tagStart] (char 62) ) do    (++ tagStart))
		(++ tagStart)
		(while (<>  myCurrentExpressionText[tagEnd] (char 60) ) do     (-- tagEnd)) 
		(-- tagEnd)
 
        (setq evalExpression (substring myCurrentExpressionText tagStart tagEnd)) 
  
 		(setq testEx (append testEx evalExpression (char 10)))
 	 
 
  	    


       (setq myCurrentNoteText (substring htmlExpressionText (find "<Note>" htmlExpressionText 1) (+ NoteEndTagLen (find "</Note>" htmlExpressionText 1))))
       (setq myCurrentReturnText (substring htmlExpressionText (find "<Returns>" htmlExpressionText 1) (+ RtnEndTagLen (find "</Returns>" htmlExpressionText 1))))
       ;; check if there is text in the Note field
       (if (= false (find "none" myCurrentNoteText))
           (appendWriteln htmlSyntaxTable "<p>" myCurrentNoteText "</p>" _eol)
        ) ;; end if
       (appendWriteln htmlSyntaxTable  "<table border=0 cellspacing=5 cellpadding=5 width=100%>" _eol)
       (appendWriteln htmlSyntaxTable "<tr><td width=70% align=left>" myCurrentExpressionText "</td>" _eol)
       (appendWriteln htmlSyntaxTable "<td width=30% align=left> Returns: <b>" myCurrentReturnText "</b></td></tr>" _eol)
       (appendWriteln htmlSyntaxTable "</table>" )
       
 
    ) ;; end of while

	(setq mm false)
	(setq nn #void)
	(setq nn (find {<sc>} myCurrentExpressionText) )
  
	(if (and (= exampleChecking true) (isInteger nn))
		(begin
  			(if (<> testEx #void)
				(begin
 
					(setq testExNew (aisRefGuide.asciiConvert testEx))
 					(setq xmlString myCurrentExampleObject.Example.KnowledgeBase.Title)
				
					(setq macLen (length macroVector))
 
					(setq m 0)
					(while (and (< m macLen) (isBoolean mm)) do
  
						(setq mm (find macroVector[m] testExNew) ) 
						(++ m)
					) 					
			 
					(if (isInteger mm) (begin (writeln "NO LAMBDA: " xmlString)(setq scriptTest testExNew)) else (begin (setq scriptTest (append "(lambda() " testExNew ")"))))
 					(setq scriptTest (string scriptTest)) 
					(setq displayText testExNew) 
					;(errorTrap '(eval scriptTest)' (writeln "The expression caused an error: " #\newline xmlString  #\newline) )
 					(errorTrap '(eval scriptTest) '(setq errorValue true) )

		 			(if (= errorValue true)
						(begin
							(writeln " <<<<<<<<<< ERROR >>>>>>>>>>  " #\newline)
							(writeln "The expression caused an error: " #\newline xmlString  #\newline) 
							(writeln "Error found in:  " scriptTest)
							(setq errorValue false)
						) ;; end begin
					) ;;end if 
					(setq scriptTest "")
					(setq xmlString "")
				) ;; end begin
			) ;; end if
		) ;; end begin
	) ;; end exampleChecking
    ;; find Notes and Hints
   (setq index3 (find "<Hints>" htmlText))
   (setq index4 (find "</Hints>" htmlText))
   (setq hintsText (substring htmlText index3 (+ 9 index4)))
   ;; Check if there is text in the Notes field  
   (if (= false (find "none" hintsText))
       (appendWriteln htmlSyntaxTable {<p><FONT COLOR="#0000ff"><u><b> Notes and Hints </b></u></FONT></p>} hintsText)
    ) ;; end if
    
    (setq endTimeExSyn (getTickCount startTimeExSyn))
    (setq timeExSyn endTimeExSyn) 

htmlSyntaxTable) ;; exampleSyntaxTable






































;;**EXPORTKEY**:aisRefGuide.fileLinkTable
(defchild aisRefGuide:fileLinkTable()  
;; *******************************************************************
;; summary:  Return an HTML table of Trig Function Links
;;
;; Args:     none
;;
;; Return:   htmlText   An HTML table of Function Links
;; *******************************************************************
   vars:(     N htmlText  ctr tempVector  funcName  
         ) ;; end of temporary variables 

(setq startTimeEssFile (getTickCount timeEssFile)) 
   (setq N (length myFileFunctions))
   (setq htmlText (new Vector: Byte: 1000000))

    (appendWriteln htmlText {<table width="100%"><tr>} _eol)
    (loop for ctr from 0 until N do  
        (if (and (<> ctr 0) (= (modi ctr 5) 0)) (appendWriteln htmlText "</tr><tr>" _eol)) 
        (setq tempVector (stringToVector myFileFunctions[ctr] "_"))
        (setq funcName tempVector[1])
        (appendWriteln htmlText {<td><A HREF="} myFileFunctions[ctr] ".html" {">} funcName "</A></td>")
    ) ;; end loop
    (appendWriteln htmlText {</table></tr>} _eol)

    (setq endTimeEssFile  (getTickCount startTimeEssFile))
    (setq timeEssFile endTimeEssFile)
    
   htmlText) ; end trigLinkTable




 




























;;**EXPORTKEY**:aisRefGuide.functionDescription
(defchild aisRefGuide:functionDescription()  
;; *****************************************************************************
;; summary:  Returns the Overview, Notes and Usage of each Function Ontology Object
;;
;; Args:     none
;;
;; Return:   htmlWriteText   The content of <Description>...</Description> 
;;                           in the Function pages in the Foundry Folder  
;; *******************************************************************************
   vars:(index1 index2 index3 index4 
         index5 index6  
         htmlText
         htmlWriteText 
         descriptionText
         overviewText 
         usageText
         notesText
         findOverviewStart
         findOverviewEnd
         findUsageStart
         findUsageEnd
         ) ;; end of temporary variables 
   
    (setq startTimeDesc (getTickCount timeDesc))  
      
   (setq htmlText myCurrentFunctionHtml) 
   (setq index1 (find "<Brief>" htmlText ))
   (setq index2 (find "</Brief>" htmlText ))
   (setq descriptionText (substring htmlText index1 (+ 15 index2)))

   (if (<> functionType #void)
       (begin
            (setq findOverviewStart (append "<Overview-" functionType ">"))
            (setq findOverviewEnd(append "</Overview-" functionType ">"))
            (setq findUsageStart (append "<Usage-" functionType ">"))
            (setq findUsageEnd (append "</Usage-" functionType ">"))
        ) ;; end begin
    ) ;; end if for Type
 
 
    (if  (isBoolean (find findOverviewStart htmlText))   
       (begin
           (setq findOverviewStart "<Overview>")
           (setq findOverviewEnd "</Overview>")
       )
    )



   ;; if usage Text is the same for Data Types
   (if (isBoolean (find findUsageStart htmlText))
       (begin
           (setq findUsageStart "<Usage>")
           (setq findUsageEnd "</Usage>")
       )
    )
 
   ;; Getting Overview Text  
   (setq index3 (find findOverviewStart descriptionText))
   (setq index4 (find findOverviewEnd descriptionText))
   (setq overviewText (substring descriptionText index3 (+ 12 index4)))
   (setq htmlWriteText (append overviewText))
 
   ;; Gettting Usage Text
   (setq index5 (find findUsageStart descriptionText))
   (setq index6 (find findUsageEnd descriptionText))
   (setq usageText (substring descriptionText index5 (+ 9 index6)))
   ;; Check if there is text in the Usage field
   (if (= false (find "none" usageText))
       (setq htmlWriteText (append htmlWriteText {<p><FONT COLOR="#0000ff"><u><b> When to use </b></u></FONT></p>} usageText))
    );; end if
  
   ;;(writeln "end function description") 

 
   (setq endTimeDesc (getTickCount startTimeDesc))
   (setq timeDesc endTimeDesc)

 
htmlWriteText) ;; end functionDescription
































;;**EXPORTKEY**:aisRefGuide.functionExamples
(defchild aisRefGuide:functionExamples()
;; *****************************************************************************
;; summary:  Checks out, evaluates and checks in Example Pages
;;           Returns the description and links to the examples of the current
;;           function being evaluated
;;
;; Args:     none
;;
;; Return:   htmlText          HTML Text containing the description and links
;;                             to the examples of each function
;; *******************************************************************************
    vars:( htmlText
           index1 index2 n 
           exVector
           vecLength
           htmlPage        
           lengthTemp
           descVector
           descString)

    (setq startTimeFuncEx (getTickCount timeFuncEx))
   
   (setq htmlText (new Vector: Byte: 1000000))
   (setq exVector (ref myExampleDictionary newName)) 
   (setq vecLength (length exVector))
   (loop for n from 0 until vecLength
        (setq descVector (ref myExampleDesc exVector[n]))    
        (setq descString (string descVector true))
        (setq descString (substring descString 8))
        (setq descString (substitute descString {" )} { }))
        (appendWriteln htmlText {<A HREF="}  exVector[n] ".html" {">} exVector[n] "</A>" _eol) 
        (appendWriteln htmlText {<p>} descString {</p>} _eol)
    ) ;; end loop
    
    ;;(writeln "end functionExamples ")

    (setq endTimeFuncEx (getTickCount startTimeFuncEx))
    (setq timeFuncEx endTimeFuncEx)
  
htmlText) ;; end functionExamples
































;;**EXPORTKEY**:aisRefGuide.functionKeyWordLinks
(defchild aisRefGuide:functionKeyWordLinks()
;; *****************************************************************************
;; summary:  Checks for the presence of a cross reference dictionary.
;;           Retrieves the Functions for all the Argument Types of the 
;;           currently evaluated VMInstruction.
;; Args:     none
;;
;; Return:   htmlText          HTML table of Functions 
;; *******************************************************************************

 vars: ( foundIdx 
         currentText 
         p n myLength nn
         htmlText 
         tempString 
         tempString2
         tempVector 
         myLength 
         textLength
         currentVector
         tokenVector
         functionSymbol
       );; end of temporary variables

    (setq startTimeKW (getTickCount timeKW))
       
    ;; Create a Dictionary
    (if (not (isDictionary myCrossReferenceFunctionTable) )
       (begin
         (aisRefGuide.createXFunctionTable)
     )) ;; end of if condition

       
  (setq tempString myCurrentFunctionObject.Function.KnowledgeBase[(symbol (append
         "ArgumentTypes-" functionType))])

  (setq tempString2 myCurrentFunctionObject.Function.KnowledgeBase.ArgumentTypes)

   (if  (= tempString #void)  
        (begin
            (setq tempString tempString2)
            (if (= tempString #void)
                (setq tempString " none " )
             ) ;; end if
         );;   end begin  
      ) ;; end if
    
     
    (setq htmlText (new Vector: Byte: 1000000))
    (setq currentVector (new Vector: Object: ))
    (setq tempVector (new Vector: Object: ))    
    (setq tempVector (stringToVector (clean tempString) (append " " #\newline #\return #\tab)  true))
    (setq vecLength (count tempVector))  
    (appendWriteln htmlText {<p>} "Here are the links to the data types of the function arguments.  " {</p>} _eol)
    (appendWriteln htmlText {<table width="100%"><tr>} _eol)
 
    (loop for n from 0 until vecLength 
        ;; write Argument Type
          (if (and (<> n 0) (= (modi n 4) 0)) 
          (appendWriteln htmlText "</tr><tr>" _eol)) 
          (appendWriteln htmlText {<td><A HREF="Datatype_} tempVector[n] ".html" {">} tempVector[n]   "</A></td>" _eol)
          ;; finding matching vector in created dictionary
          (setq foundIdx (binarySearch  myCrossReferenceFunctionTable tempVector[n]))
          ;;(if (isInteger foundIdx)
           ;;   (begin
                (setq currentText myCrossReferenceFunctionTable[foundIdx])
                (setq textLength (length currentText))
                (loop for nn from 0 until textLength
                    ;;(if (not (isMember currentText[nn] currentVector ))
                    ;;    (setq currentVector[(length currentVector)] currentText[nn])      
                    ;; ) ;; end of if condition 
                   (binaryInsert currentVector currentText[nn])    
                ) ;; end  of loop for textLength
            ;;   ) ;; end of begin
          ;;  );; end of isInteger
     ) ;; end of loop for vecLength
            (appendWriteln htmlText "</tr>" _eol)
            (appendWriteln htmlText "</table>" _eol)
            (setq K (length funcPrefix))
            (setq myLength (length currentVector))
            (appendWriteln htmlText {<p>} "Here are also a number of links to functions having arguments with any of these data types. 
             " {</p>} _eol)
            (appendWriteln htmlText {<table width="100%"><tr>} _eol)
            (loop for p from 0 until myLength 
                (if (and (<> p 0) (= (modi p 4) 0))               
                (appendWriteln htmlText "</tr><tr>" _eol)) 
                (setq tokenVector (stringToVector currentVector[p] "_")) 
                ;; for special function names
                (if (isInteger (find "(sym)" tokenVector[1]))
                (begin (setq functionSymbol (aisRefGuide.specialFunctionNames tokenVector[1])))                                   
                 else
                (begin (setq functionSymbol  tokenVector[1]))
                 ) ;; end if for special function names                    
                (appendWriteln htmlText {<td><A HREF="} currentVector[p] ".html" {">} 
                functionSymbol "(" tokenVector[2] ")</A></td>" _eol)         
             ) ;; end of loop for myLength
             (appendWriteln htmlText "</tr>" _eol)
             (appendWriteln htmlText "</table>" _eol)
       

   ;;(writeln "end function keywordlinks")
     
    (setq endTimeKW (getTickCount startTimeKW))
    (setq timeKW endTimeKW)

htmlText) ;; end functionKeyWordLinks
































;;**EXPORTKEY**:aisRefGuide.functionLinkTable
(defchild aisRefGuide:functionLinkTable()  
;; *******************************************************************
;; summary:  Return an HTML table of Function links.
;;
;; Args:     none
;;
;; Return:   htmlText   An HTML table of Function links.
;; *******************************************************************
   vars:( N K htmlText  n tempVector keyword temp functionNames funcTypesLength funcTypes p
          temp reference myFunctionDictionary tempVector len lengthNames index 
          currentFuncName funcTypesVector functionSymbol
         ) ;; end of temporary variables 

 
   (setq startTimeFuncLink (getTickCount timeFuncLink))

   (setq N (length newFunctions))
   (setq K (length funcPrefix))
   (setq htmlText (new Vector: Byte: 1000000))

   (if (not (isDictionary myFunctionDictionary))
        (begin
        (setq myFunctionDictionary (new Dictionary:))   
         ;; loop for all the Functions to create Dictionary
        (loop for n from 0 until N do   
            (setq tempVector (stringToVector newFunctions[n] "_" )) 
            ;; Get the Function Type
            (setq keyword tempVector[1]) ;; function Name                     
            (setq reference tempVector[2]) ;; function Type
            (setq keyword (symbol keyword))
            (setq reference (symbol reference))
            (if  (not (isVector myFunctionDictionary[keyword]))
                (begin
                (setq myFunctionDictionary[keyword] (new Vector: Object:))               
                )
            ) ;; end of if condition
           ;; set the Function Title to the Cross Reference Vector
           (setq temp myFunctionDictionary[keyword])
           (if (not (isMember reference temp))
                (setq temp[(length temp)] reference)
            ) ;; end of if condition
         ) ;; end loop
      )
   ) ;; end if for creating Dictionary

        
    (setq functionNames (refAttributes myFunctionDictionary)) 
    (setq funcTypes (refValues myFunctionDictionary))    
    (setq lengthNames (length functionNames))
    ;; looping through all the elements of the functionNames Vector
    (appendWriteln htmlText {<table width="100%" >}  )     
    (loop for len from 0 until lengthNames do  
        (symbol functionNames[len])
        (if (isInteger (find "(sym)" functionNames[len]))
              (begin
                  (setq functionSymbol (aisRefGuide.specialFunctionNames functionNames[len]))                                    
              ) 
              else
              (begin
                  (setq functionSymbol  functionNames[len])
              )
            )
        (setq index (member functionNames[len] myFunctionDictionary))                 
        (setq funcTypesVector funcTypes[index])
        (setq funcTypesVector (sort funcTypesVector <))
        (setq funcTypesLength (length funcTypesVector))
                   
        (if (= funcTypesLength 1 )
            (begin                          
            (appendWriteln htmlText {<tr><td><A HREF="} (append "Function_" functionNames[len] "_" funcTypesVector[0]) ".html" {">} functionSymbol "</A></td></tr>" _eol) 
            )
         else
           (begin
           (loop for p from 0 until funcTypesLength             
               (if (isEqual 0 p)
                   (appendWriteln htmlText "<tr><td><b><font color=blue>" functionSymbol " : </b></font>(" {<A HREF="} (append "Function_" 
                   functionNames[len] "_" funcTypesVector[p]) ".html" {">}  funcTypesVector[p]"</A> "))
               (if (isEqual  (- funcTypesLength 1)  p)       
                   (appendWriteln htmlText   {<A HREF="} (append "Function_" functionNames[len] "_" funcTypesVector[p]) ".html" {">}  funcTypesVector[p]   ")" {</A></td> </tr>} _eol  )
                
               )
               (if (and (compareNE (-  funcTypesLength 1) p) (compareNE 0 p))
                   (begin                
                      (appendWriteln htmlText    {<A HREF="} (append "Function_" functionNames[len] "_" funcTypesVector[p]) ".html" {">}  funcTypesVector[p]"</A>  ")
                   )                 
               )
           ) ;; end loop
           ) ;; end begin
           
        )  ;; end if   
     
    ) ;; end loop
      (appendWriteln htmlText {</table>}  )
 
   ;;(writeln "end functionLinkTable")

   (setq endTimeFuncLink (getTickCount startTimeFuncLink))
   (setq timeFuncLink endTimeFuncLink)
   htmlText) ; end functionLinkTable































;;**EXPORTKEY**:aisRefGuide.functionSyntaxTable
(defchild aisRefGuide:functionSyntaxTable()  
;; *****************************************************************************
;; summary:  Returns the Syntax of each function expression with the arguments 
;;           for each expression
;;
;; Args:     none
;;
;; Return:   htmlSyntaxTable   HTML Table containing the description of each
;;                             argument for each Function
;; *******************************************************************************
   vars:( htmlText
          htmlSyntaxTable
          index1 index2
          findSyntaxStart
          findSyntaxEnd
          startTagLength
          endTagLength
          indexExpression
          indexNextExpr
          htmlExpressionText
          indexArgument
          myCurrentExpressionText
          (ExStartTagLen 13)
          (ExEndTagLen 14)
          myCurrentArgumentHtml
          myCurrentArgumentObject
          indexArgumentEnd
          myTempStringXml
          myTempTypeXml
          indexReturn1 indexReturn2
          returnText
) ;; end of temporary variables 
 
  (setq startTimeFuncSyn (getTickCount timeFuncSyn))


  ;; defining the variables
  (setq htmlText myCurrentFunctionHtml) 
  (setq htmlSyntaxTable (new Vector: Byte: 1000000))

  ;; here old syntax implementation
  (setq findSyntaxStart (append "<Syntax-" functionType ">"))
  (setq findSyntaxEnd(append "</Syntax-" functionType ">"))

  
  (if (and (<> functionType #void) (isInteger (find findSyntaxStart htmlText)))
     (begin 
          (setq startTagLength (length findSyntaxStart))
          (setq endTagLength (length findSyntaxEnd))          
      )
     else
     (begin
            (setq findSyntaxStart "<Syntax>")
            (setq findSyntaxEnd "</Syntax>")
            (setq startTagLength (length findSyntaxStart))
            (setq endTagLength (length findSyntaxEnd))
      )
   ) ;; end if for type

    
    
       
    
  ;; (if (compareEQ functionType "#void")
  ;;     (begin
  ;;          (setq findSyntaxStart "<Syntax>")
  ;;          (setq findSyntaxEnd "</Syntax>")
  ;;          (setq startTagLength 8)
  ;;          (setq endTagLength 9)
  ;;      ) ;; end begin
  ;;  )  ;; end if for no DataType



   (setq index1 (find findSyntaxStart htmlText ))
   (setq index2 (find findSyntaxEnd htmlText ))
   (setq htmlText (substring htmlText index1 (+ endTagLength index2)) )
    
  
   ;; Get the string between the Expression tags
   (setq indexExpression 0 )
   (setq indexNextExpr 0 )
   
   (while (isNumber ( setq indexExpression (find "<Expression>" htmlText indexExpression )))
       (setq indexNextExpr (find "<Expression>" htmlText (+ ExStartTagLen indexExpression )))
       ;; Checking for Functions with more than one expression
       (if (isNumber indexNextExpr)
           (begin
               (setq htmlExpressionText (substring htmlText indexExpression  indexNextExpr ))
               (setq indexExpression indexNextExpr)
            ) ;; end begin
       else
           (begin 
               (setq indexNextExpr (length htmlText))                
               (setq htmlExpressionText (substring htmlText indexExpression  (sub1 (find findSyntaxEnd htmlText indexExpression ))  ) )
               (setq indexExpression (- (length htmlText)))   
           ) ;; end begin
       ) ;; end if
        
       ;; set-up table for the Argument fields
       (setq indexArgument 0 )
       (setq myCurrentExpressionText (substring htmlExpressionText (find "<Expression>" htmlExpressionText 1) (+ ExEndTagLen (find "</Expression>" htmlExpressionText 1))))
       (appendWriteln htmlSyntaxTable "<table border=0 cellspacing=5 cellpadding=5>" _eol )
       (appendWriteln htmlSyntaxTable "<tr><td><b>" myCurrentExpressionText "</b></td></tr>" _eol)
       (appendWriteln htmlSyntaxTable "</table><br>" )
       (appendWriteln htmlSyntaxTable {<hr align=left color="#000000" width=80% size="1">} _eol)
       (appendWriteln htmlSyntaxTable "<tr><td align=center><table border=0 cellspacing=5 cellpadding=5>" _eol )
       ;; special condition for the debug function
       (if (compareNE myCurrentFunctionObject.Function.KnowledgeBase.Title "debug")
       (appendWriteln htmlSyntaxTable {<tr><td width=20%><FONT COLOR="#0000FF"><b>Name</b></FONT></td>
                                           <td width=40%><FONT COLOR="#0000FF"><b>Description</b></FONT></td>
                                           <td><FONT COLOR="#0000FF"><b>AIS Types</b></FONT></td></tr> } _eol))
      
       ;; Get the content between the Argument tags 
       ;; and convert the retrieved string into xml so
       ;; the  Name, Description and AISTypes arguments can be accessed
       ;; through the dot command of the XML Structure

       (if (compareEQ myCurrentFunctionObject.Function.KnowledgeBase.Title "debug")
            (begin
                ( setq indexArgument ( find "<Argument>" htmlExpressionText indexArgument ))
                (setq indexArgumentEnd (find "</Argument>" htmlExpressionText indexArgument))
                (setq myCurrentArgumentHtml (substring htmlExpressionText indexArgument (+ indexArgumentEnd (length "</Argument>") 1)))
                (appendWriteln htmlSyntaxTable myCurrentArgumentHtml)
                (appendWriteln htmlSyntaxTable {<hr align=left color="#000000" width=80% size="1">} _eol)
            )
       else
            (begin
       (while( isNumber( setq indexArgument ( find "<Argument>" htmlExpressionText indexArgument )))
           (setq indexArgumentEnd (find "</Argument>" htmlExpressionText indexArgument))
           (setq myCurrentArgumentHtml (substring htmlExpressionText indexArgument (+ indexArgumentEnd (length "</Argument>") 1)))
           (setq myCurrentArgumentObject (xml myCurrentArgumentHtml))
           (setq indexArgument indexArgumentEnd)
           (appendWriteln htmlSyntaxTable "<tr><td align=top ><b>")
           (appendWriteln htmlSyntaxTable myCurrentArgumentObject.Argument.Name)
           (appendWriteln htmlSyntaxTable "</b></td><td>")
           ;; parse the text for the <Note> field result
           (setq myTempStringXml myCurrentArgumentObject.Argument.Note)
           (setq myTempStringXml (string myTempStringXml true))
           (if (<> myTempStringXml #void)
               (setq myTempStringXml (substring myTempStringXml 6 (- (length myTempStringXml) 3 )))
           ) ;; end if
           ;; end  of parsing
           (appendWriteln htmlSyntaxTable myTempStringXml)
           (appendWriteln htmlSyntaxTable "</td><td>")
           ;; parse the text for the <AisType> field result
           (setq myTempTypeXml myCurrentArgumentObject.Argument.AISTypes)
           (if (= false (isNumber (find "none" myTempTypeXml)))
               (begin
               (appendWriteln htmlSyntaxTable myTempTypeXml) 
               (appendWriteln htmlSyntaxTable "</td></tr>" _eol )
               ) ;; end of begin
           ) ;; end for AisType
       ) ;; end of while loop for argument

       ;;(appendWriteln htmlSyntaxTable "</table>" )
       (appendWriteln htmlSyntaxTable "</td></tr></table><br> " _eol )
       (appendWriteln htmlSyntaxTable {<hr align=left color="#000000" width=80% size="1">} _eol)

       ;; retrieve the Return value
       (setq indexReturn1 (find "<Return>" htmlExpressionText))
       (setq indexReturn2 (find "</Return>" htmlExpressionText))
       (setq returnText (substring htmlExpressionText indexReturn1 (+ startTagLength indexReturn2)))
       (appendWriteln htmlSyntaxTable {<FONT COLOR="#0000ff"><u><b>} "Returns:" {</b></u></FONT>} _eol) 
       (appendWriteln htmlSyntaxTable {<FONT COLOR=black>} returnText {</FONT>})


        );; end of begin
     ) ;; end of if for debug
    ) ;; end of while loop for Expression
 
   ;;(writeln "end function syntax Table")

   (setq endTimeFuncSyn (getTickCount startTimeFuncSyn))
   (setq timeFuncSyn endTimeFuncSyn)

htmlSyntaxTable) ;; end functionSyntaxTable
































;;**EXPORTKEY**:aisRefGuide.macroLinkTable
(defchild aisRefGuide:macroLinkTable()  
;; *******************************************************************
;; summary:  Return an HTML table of Trig Function Links
;;
;; Args:     none
;;
;; Return:   htmlText   An HTML table of Function Links
;; *******************************************************************
   vars:(     N htmlText  ctr tempVector  funcName  
         ) ;; end of temporary variables 

   (setq startTimeEssMac (getTickCount timeEssMac))
 
   (setq N (length myMacros))
   (setq htmlText (new Vector: Byte: 1000000))

    (appendWriteln htmlText {<table width="100%"><tr>} _eol)
    (loop for ctr from 0 until N do  
        (if (and (<> ctr 0) (= (modi ctr 5) 0)) (appendWriteln htmlText "</tr><tr>" _eol)) 
        (setq tempVector (stringToVector myMacros[ctr] "_"))
        (setq funcName tempVector[1])
        (appendWriteln htmlText {<td><A HREF="} myMacros[ctr] ".html" {">} funcName "</A></td>")
    ) ;; end loop
    (appendWriteln htmlText {</table></tr>} _eol)

    (setq endTimeEssMac (getTickCount startTimeEssMac))
    (setq timeEssMac endTimeEssMac)
    
   htmlText) ;; end macroLinkTable




 




























;;**EXPORTKEY**:aisRefGuide.mathLinkTable
(defchild aisRefGuide:mathLinkTable()  
;; *******************************************************************
;; summary:  Return an HTML table of Math Function Links
;;
;; Args:     none
;;
;; Return:   htmlText   An HTML table of Function Links
;; *******************************************************************
   vars:(     N htmlText  ctr tempVector  funcName functionSymbol
         ) ;; end of temporary variables 

   (setq startTimeEssMath (getTickCount timeEssMath))
 
   (setq N (length myMathFunctions))
   (setq htmlText (new Vector: Byte: 1000000))

    (appendWriteln htmlText {<table width="100%"><tr>} _eol)
    (loop for ctr from 0 until N do  
        (if (and (<> ctr 0) (= (modi ctr 5) 0)) (appendWriteln htmlText "</tr><tr>" _eol)) 
        (setq tempVector (stringToVector myMathFunctions[ctr] "_"))
        (setq funcName tempVector[1])
       (if (isInteger (find "(sym)" funcName))
              (begin
                  (setq functionSymbol (aisRefGuide.specialFunctionNames funcName))                                    
              ) 
              else
              (begin
                  (setq functionSymbol  funcName)
              )
            )
        (appendWriteln htmlText {<td><A HREF="} myMathFunctions[ctr] ".html" {">} functionSymbol "</A></td>")
    ) ;; end loop
    (appendWriteln htmlText {</table></tr>} _eol)

   (setq endTimeEssMath (getTickCount startTimeEssMath))
    (setq timeEssMath endTimeEssMath)
    
   htmlText) ;; end mathLinkTable




 




























;;**EXPORTKEY**:aisRefGuide.repositoryLinkTable
(defchild aisRefGuide:repositoryLinkTable()  
;; *******************************************************************
;; summary:  Return an HTML table of Trig Function Links
;;
;; Args:     none
;;
;; Return:   htmlText   An HTML table of Function Links
;; *******************************************************************
   vars:(     N htmlText  ctr tempVector  funcName  
         ) ;; end of temporary variables 

(setq startTimeEssRep (getTickCount timeEssRep))
 
   (setq N (length myRepositoryFunctions))
   (setq htmlText (new Vector: Byte: 1000000))

    (appendWriteln htmlText {<table width="100%"><tr>} _eol)
    (loop for ctr from 0 until N do  
        (if (and (<> ctr 0) (= (modi ctr 5) 0)) (appendWriteln htmlText "</tr><tr>" _eol)) 
        (setq tempVector (stringToVector myRepositoryFunctions[ctr] "_"))
        (setq funcName tempVector[1])
        (appendWriteln htmlText {<td><A HREF="} myRepositoryFunctions[ctr] ".html" {">} funcName "</A></td>")
    ) ;; end loop
    (appendWriteln htmlText {</table></tr>} _eol)

  (setq endTimeEssRep (getTickCount startTimeEssRep))
    (setq timeEssRep endTimeEssRep)
    
    
   htmlText) ;; end repositoryLinkTable




 




























;;**EXPORTKEY**:aisRefGuide.trigLinkTable
(defchild aisRefGuide:trigLinkTable()  
;; *******************************************************************
;; summary:  Return an HTML table of Trig Function Links
;;
;; Args:     none
;;
;; Return:   htmlText   An HTML table of Function Links
;; *******************************************************************
   vars:(     N htmlText  ctr tempVector  funcName  
         ) ;; end of temporary variables 
 
(setq startTimeEssTrig (getTickCount timeEssTrig))

   (setq N (length myTrigFunctions))
   (setq htmlText (new Vector: Byte: 1000000))

    (appendWriteln htmlText {<table width="100%"><tr>} _eol)
    (loop for ctr from 0 until N do  
        (if (and (<> ctr 0) (= (modi ctr 5) 0)) (appendWriteln htmlText "</tr><tr>" _eol)) 
        (setq tempVector (stringToVector myTrigFunctions[ctr] "_"))
        (setq funcName tempVector[1])
        (appendWriteln htmlText {<td><A HREF="} myTrigFunctions[ctr] ".html" {">} funcName "</A></td>")
    ) ;; end loop
    (appendWriteln htmlText {</table></tr>} _eol)

  (setq endTimeEssTrig (getTickCount startTimeEssTrig))
    (setq timeEssTrig endTimeEssTrig)
    
   htmlText) ;; end trigLinkTable




 




























;;**EXPORTKEY**:aisRefGuide.vmDescription
(defchild aisRefGuide:vmDescription()  
;; *****************************************************************************
;; summary:  Returns the Description of each Ontology Object
;;
;; Args:     none
;;
;; Return:   htmlWriteText   The content of <Description>...</Description> 
;;                           in the VMInstruction pages in the Foundry Folder  
;; *******************************************************************************
   vars:(
        htmlText
        htmlWriteText
         ) ;; end of temporary variables 
    
   
   (setq startTimeVMDesc (getTickCount timeVMDesc))
   (setq htmlText myCurrentOntologyHtml) 
   (setq index1 (find "<Description>" htmlText ))
   (setq index2 (find "</Description>" htmlText ))
   (setq htmlText (substring htmlText index1 (+ 15 index2)) )
   (setq htmlWriteText htmlText)

   (setq endTimeVMDesc (getTickCount startTimeVMDesc))
   (setq timeVMDesc endTimeVMDesc)
  
   
   htmlWriteText) ;; end vmLinkTable






































;;**EXPORTKEY**:aisRefGuide.vmExamples
(defchild aisRefGuide:vmExamples()  
;; *****************************************************************************
;; summary:  Returns the Examples of each VMInstruction
;;
;; Args:     none
;;
;; Return:   htmlWriteText   The content of <Description>...</Description> 
;;                           in the VMInstruction pages in the Foundry Folder  
;; *******************************************************************************
   vars:(htmlWriteText
		 
        htmlText
         index1 index2
	displayVMText
	 
         ) ;; end of temporary variables 

    (setq errorValue false)
   (setq vmScript (new String: ""))
	(setq htmlText (new String: ""))
	(setq vmString (new String: ""))
	(setq displayVMText (new String: ""))
   (setq startTimeVMDesc (getTickCount timeVMDesc))
   (setq htmlText myCurrentOntologyHtml) 
   (setq index1 (find "<Example>" htmlText ))
   (setq index2 (find "</Example>" htmlText ))
   (setq htmlText (substring htmlText (+ 9 index1) (- index2 1)) )
   (setq htmlWriteText htmlText)
   (if (= exampleChecking true)
		(begin
  			(setq displayVMText htmlText)
 			(setq vmString myCurrentOntologyObject.VMInstruction.KnowledgeBase.Title)
 			(setq htmlText (append "(lambda() " htmlText ")"))  
  		 	(setq vmScript (string htmlText))	  
			(errorTrap '(eval vmScript)'(setq errorValue true))
			(if (= errorValue true)
				(begin
 
					(writeln " <<<<<<<<<< ERROR >>>>>>>>>>  " #\newline)
					(writeln "The expression caused an error: " #\newline vmScript  #\newline) 
					(writeln "Error found in:  " vmString)
					(setq errorValue false)
				) ;; end begin
			) ;;end if
	 	)
	)
   (setq endTimeVMDesc (getTickCount startTimeVMDesc))
   (setq timeVMDesc endTimeVMDesc)
    

   htmlWriteText) ;; end vmLinkTable






































;;**EXPORTKEY**:aisRefGuide.vmInsType
(defchild aisRefGuide:vmInsType()  
;; *******************************************************************
;; summary:  Return an HTML table of links to VMInstruction of the 
;;           same instruction type
;;
;; Args:     none
;;
;; Return:   htmlTable   An HTML table of VM Instruction links.
;; *******************************************************************
vars: (  instrucType 
         regLength genLength vecLength natLength
         K
         htmlText
      ) ;; end of temporary variables
       
     ;; define variables

     (setq startTimeVMIns (getTickCount timeVMIns))
    
     (setq regLength (length myRegisterIns))
     (setq genLength (length myGenericIns))
     (setq vecLength (length myVectorIns))
     (setq natLength (length myNativeIns))
     
     
     (setq K (length vmPrefix))
     (setq htmlText (new Vector: Byte: 1000000))
     (setq instrucType myCurrentOntologyObject.VMInstruction.KnowledgeBase.InstructionType )
     
     (appendWriteln htmlText {<table width="100%"><tr>} _eol)
 
     ;; check Instruction Type
     (if (compareEQ instrucType "Generic" )
          (begin 
          (loop for n from 0 until genLength do
             (if (and (<> n 0) (= (modi n 4) 0)) 
                 (appendWriteln htmlText "</tr><tr>" _eol))
             (appendWriteln htmlText {<td><A HREF="} myGenericIns[n] ".html" {">} 
             (mid myGenericIns[n] K 10000)   "</A></td>" _eol) 
           ) ;; end loop
      )) ;; end checking for Generic Instruction Types
     (if (compareEQ instrucType "Register" )
          (begin 
          (loop for n from 0 until regLength do
              (if (and (<> n 0) (= (modi n 4) 0))
                 (appendWriteln htmlText "</tr><tr>" _eol))
              (appendWriteln htmlText {<td><A HREF="} myRegisterIns[n] ".html" {">} (mid myRegisterIns[n] K 10000)   "</A></td>" _eol) 
            ) ;; end loop
     )) ;; end checking for Register Instruction Types
     (if (compareEQ instrucType "Native" )
         (begin 
         (loop for n from 0 until natLength do
             (if (and (<> n 0) (= (modi n 4) 0))
                 (appendWriteln htmlText "</tr><tr>" _eol))
             (appendWriteln htmlText {<td><A HREF="} myNativeIns[n] ".html" {">} (mid myNativeIns[n] K 10000)   "</A></td>" _eol) 
            ) ;; end loop
     )) ;; end checking for Native Instruction Types
     ( if ( compareEQ instrucType "Vector" )
          (begin 
          (loop for n from 0 until vecLength do
              (if (and (<> n 0) (= (modi n 4) 0)) 
              (appendWriteln htmlText "</tr><tr>" _eol))
              (appendWriteln htmlText {<td><A HREF="} myVectorIns[n] ".html" {">}  (mid myVectorIns[n] K 10000)  "</A></td>" _eol) 
           ) ;; end loop
     )) ;; end checking for Vector Types
    (appendWriteln htmlText "</tr>" _eol)
    (appendWriteln htmlText "</table>" _eol)
  
    (setq endTimeVMIns (getTickCount startTimeVMIns))
    (setq timeVMIns endTimeVMIns)

htmlText)







































;;**EXPORTKEY**:aisRefGuide.vmKeyWordLinks
(defchild aisRefGuide:vmKeyWordLinks()
;; *****************************************************************************
;; summary:  Checks for the presence of a cross reference dictionary.
;;           Retrieves the VMInstructions for all the Argument Types of the 
;;           currently evaluated VMInstruction.
;; Args:     none
;;
;; Return:   htmlText          HTML table of VMInstructions 
;; *******************************************************************************

 vars: ( foundIdx 
         currentText 
         p n myLength
         htmlText 
         tempString 
         tempVector 
         myLength 
         textLength
         currentVector        
       );; end of temporary variables

   (setq startTimeVMKW (getTickCount timeVMKW))
    ;; Create a Dictionary
    (if (not (isDictionary myCrossReferenceTable) )
       (begin
         (aisRefGuide.createXTable)
     )) ;; end of if condition

    ;; Argument Types of each VMInstruction
    (setq tempString myCurrentOntologyObject.VMInstruction.KnowledgeBase.ArgumentTypes)
    (setq htmlText (new Vector: Byte: 100000))
    (setq currentVector (new Vector: Object: ))
    (setq tempVector (new Vector: Object: ))
    (setq tempVector (stringToVector (clean tempString) (append " " #\newline #\return #\tab)  true))
    (setq vecLength (count tempVector))

    (loop for n from 0 until vecLength
        ;;(setq foundIdx (member tempVector[n] myCrossReferenceTable))
        (setq foundIdx (binarySearch myCrossReferenceTable  tempVector[n]))
        (setq currentText myCrossReferenceTable[foundIdx])
        (setq textLength (length currentText))
        (loop for nn from 0 until textLength
            ;;(if (not (isMember currentText[nn] currentVector ))
            ;;(begin
            ;;    (setq currentVector[(length currentVector)] currentText[nn])  
            ;; )) ;; end of if condition
            (binaryInsert currentVector currentText[nn])
         ) ;; end  of loop for textLength
    );; end of loop for vecLength
 
    (setq K (length vmPrefix))
    (setq myLength (length currentVector))
    (appendWriteln htmlText {<table width="100%"><tr>} _eol)
    (loop for p from 0 until myLength 
        (if (and (<> p 0) (= (modi p 4) 0)) 
                 (appendWriteln htmlText "</tr><tr>" _eol)) 
        (appendWriteln htmlText {<td><A HREF="} currentVector[p] ".html" {">} 
        (mid currentVector[p] K 10000)   "</A></td>" _eol)
     ) ;; end of loop for myLength

    (appendWriteln htmlText "</tr>" _eol)
    (appendWriteln htmlText "</table>" _eol)

    (setq endTimeVMKW (getTickCount startTimeVMKW))
    (setq timeVMKW endTimeVMKW)
 
htmlText)
 ;; end vmKeyWordLinks






































;;**EXPORTKEY**:aisRefGuide.vmLinkTable
(defchild aisRefGuide:vmLinkTable()  
;; *******************************************************************
;; summary:  Return an HTML table of VM Instruction links.
;;
;; Args:     none
;;
;; Return:   htmlText   An HTML table of VM Instruction links.
;; *******************************************************************
   vars:(     N vmInsName    
         htmlText  temp    ctr
         ) ;; end of temporary variables 
 
   (setq startTimeVMLink (getTickCount timeVMLink))
   (setq N (length myVMNames))
   (setq htmlText (new Vector: Byte: 1000000))

    (appendWriteln htmlText {<table width="100%"><tr>} _eol)
    (loop for ctr from 0 until N do  
        (if (and (<> ctr 0) (= (modi ctr 5) 0)) (appendWriteln htmlText "</tr><tr>" _eol)) 
        (setq temp (stringToVector myVMNames[ctr] "_"))   
        (setq vmInsName temp[1])
        (appendWriteln htmlText {<td><A HREF="} myVMNames[ctr] ".html#topage" {">} vmInsName "</A></td>")
    ) ;; end loop
    (appendWriteln htmlText {</table></tr>} _eol)

    (setq endTimeVMLink (getTickCount startTimeVMLink))
    (setq timeVMLink endTimeVMLink)
    
   htmlText) ; end vmLinkTable







;;**EXPORTKEY**:aisRefGuide.vmSyntaxTable
(defchild aisRefGuide:vmSyntaxTable()  
;; *****************************************************************************
;; summary:  Returns the Syntax of each VMInstruction with the arguments of each
;;           expression per VMInstruction
;;
;; Args:     none
;;
;; Return:   htmlSyntaxTable   HTML Table containing the description of each
;;                             argument per VMInstruction 
;; *******************************************************************************
   vars:(
          htmlText index1 index2
          htmlSyntaxTable       
         (ExStartTag "<Expression>" )
         (ExEndTag "</Expression>")
         (ExStartTagLen 12)
         (ExEndTagLen 13)
         (ArgStartTagLen (length "<Argument>"))
         (ArgEndTagLen (length "</Argument>"))
         indexNextExpr
         indexExpression
         indexArgument
         ) ;; end of temporary variables 

  (setq startTimeVMSyn (getTickCount timeVMSyn)) 

  ;; defining the variables
  (setq htmlText myCurrentOntologyHtml) 
  (setq htmlSyntaxTable (new Vector: Byte: 1000000))

   ;; Get the content inside the Syntax tags
   (setq htmlText myCurrentOntologyHtml) 
   (setq index1 (find "<Syntax>" htmlText ))
   (setq index2 (find "</Syntax>" htmlText ))
   (setq htmlText (substring htmlText index1 (+ 9 index2)) )
  
   ;; Get the string between the Expression tags
   (setq indexExpression 0 )
   (setq indexNextExpr 0 )
   (while (isNumber ( setq indexExpression (find "<Expression>" htmlText indexExpression )))
       (setq indexNextExpr (find "<Expression>" htmlText (+ ExStartTagLen indexExpression )))
       ;; Checking for VMInstructions with more than one expression
       (if (isNumber indexNextExpr)
       (begin
           (setq htmlExpressionText (substring htmlText indexExpression indexNextExpr ))
           (setq indexExpression (+ ExStartTagLen indexNextExpr))
       )
       else
       (begin 
           (setq indexNextExpr (length htmlText))
           (setq htmlExpressionText (substring htmlText indexExpression (find "</Syntax>" htmlText indexExpression ) ) )
           (setq indexExpression (- (length htmlText))) 
       )
       )
       (setq indexArgument 0 )
       (setq myCurrentExpressionText (substring htmlExpressionText (find "<Expression>" htmlExpressionText 1) (+ ExEndTagLen (find "</Expression>" htmlExpressionText 1))))
    
       (appendWriteln htmlSyntaxTable "<table border=0 cellspacing=5 cellpadding=5>" _eol )
       (appendWriteln htmlSyntaxTable "<tr><td><b>" myCurrentExpressionText "</b></td></tr>" _eol)
       (appendWriteln htmlSyntaxTable "</table><br>" )
       (appendWriteln htmlSyntaxTable {<hr align=left color="#000000" width=50% size="1">} _eol)
       (appendWriteln htmlSyntaxTable "<tr><td align=center><table border=0 cellspacing=5 cellpadding=5>" _eol )
       (appendWriteln htmlSyntaxTable {<tr><td width=20%><FONT COLOR="#0000FF"><b>Name</b></FONT></td>
                                           <td width=20%><FONT COLOR="#0000FF"><b>Format</b></FONT></td>
                                           <td><FONT COLOR="#0000FF"><b>AIS Types</b></FONT></td></tr> } _eol)
 
       ;; Get the content between the Argument tags 
       ;; and convert the retrieved string into xml so
       ;; the  Name, Format and AISTypes arguments can be accessed
       ;; through the dot command of the XML Structure
       (while( isNumber( setq indexArgument ( find "<Argument>" htmlExpressionText indexArgument )))
           (setq indexArgumentEnd (find "</Argument>" htmlExpressionText indexArgument))
           (setq myCurrentArgumentHtml (substring htmlExpressionText indexArgument (+ indexArgumentEnd (length "</Argument>") 1)))
           (setq myCurrentArgumentObject (xml myCurrentArgumentHtml))
           (setq indexArgument indexArgumentEnd)
           (appendWriteln htmlSyntaxTable "<tr><td><b>")
           (appendWriteln htmlSyntaxTable myCurrentArgumentObject.Argument.Name)
           (appendWriteln htmlSyntaxTable "</b></td><td>")
           (appendWriteln htmlSyntaxTable myCurrentArgumentObject.Argument.Format)
           (appendWriteln htmlSyntaxTable "</td><td>")
           (appendWriteln htmlSyntaxTable myCurrentArgumentObject.Argument.AISTypes)
           (appendWriteln htmlSyntaxTable "</td></tr>" _eol )
       )
       (appendWriteln htmlSyntaxTable "</table>" )
       (appendWriteln htmlSyntaxTable "</td></tr></table><br>" _eol )
       (appendWriteln htmlSyntaxTable {<hr align=left color="#000000" width=50% size="1">} _eol)
   )

  (setq endTimeVMSyn (getTickCount startTimeVMSyn))
  (setq timeVMSyn endTimeVMSyn) 
   htmlSyntaxTable)






































;;**EXPORTKEY**:aisRefGuide.vmachineLearningLinkTable
(defchild aisRefGuide:vmachineLearningLinkTable()  
;; *******************************************************************
;; summary:  Return an HTML table of Vector Machine Learning Function Links
;;
;; Args:     none
;;
;; Return:   htmlText   An HTML table of Function Links
;; *******************************************************************
   vars:(     N htmlText  ctr tempVector  funcName
         ) ;; end of temporary variables 
 
   (setq N (length myVectorMachineLearning))
   (setq htmlText (new Vector: Byte: 1000000))

    (appendWriteln htmlText {<table width="100%"><tr>} _eol)
    (loop for ctr from 0 until N do  
        (if (and (<> ctr 0) (= (modi ctr 5) 0)) (appendWriteln htmlText "</tr><tr>" _eol)) 
        (setq tempVector (stringToVector myVectorMachineLearning[ctr] "_"))
        (setq funcName tempVector[1])
        (appendWriteln htmlText {<td><A HREF="} myVectorMachineLearning[ctr] ".html" {">} funcName "</A></td>")
    ) ;; end loop
    (appendWriteln htmlText {</table></tr>} _eol)
    
   htmlText) ;; end vmachineLearningLinkTable

























