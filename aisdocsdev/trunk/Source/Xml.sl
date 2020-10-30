;;*************************************
;;*************************************
;; Exported Lambda File Cabinet Document
;;*************************************
;;*************************************

;;**EXPORTKEY**:xml
(defun xml(...)
;; *******************************************************************
;;  Summary: The XML compiler is a low-level Lisp Lambda designed as a 
;;           high-speed base processor for future SAX and DOM compliant 
;;           XML implementations. The xml function is not, itself, 
;;           a SAX and DOM compliant XML compiler; but, is designed to 
;;           form the substrate for future standards compliant XML 
;;           compilers.
;; 
;;           The xml Function implements an XML compiler via the method 
;;           of recursive descent. The xml function is a complete XML 
;;           compiler and not a parser such as the lisp or javaScript 
;;           functions. The result of running the xml function on an 
;;           XML input string is a completed XML document tree model.
;; 
;; Args:     document    (Optional) A Structure to be extended as an 
;;                         XML document in tree model form. After being 
;;                         extended, this structure is then returned 
;;                         as the result of XML compilation.
;;           inputSource (Mandatory) Either: 
;;                         (a) A string containing the XML source 
;;                             to be compiled,  
;;                         (b) An Lambda which provides the input 
;;                             string, containing the XML source, 
;;                             to the xml function on demand 
;;                             (see the Notes below).
;;           eventMgr    (Optional) Either: 
;;                         (a) A Structure containing the process 
;;                             instruction Lambdas bound to the 
;;                             active pi target names, or
;;                         (b) An Lambda which will manage xml document 
;;                             events as described in the Ref Guide 
;;                             Chapter on Document Event Handlers 
;;                             (see the Notes below).
;;
;; Return:   Document     A compiled XML document in tree model form.
;;
;; Notes:
;;        ============
;;        Input Lambdas
;;        ============
;; 
;;        The xml function supports the compilation of a multiple input strings 
;;        served from an input Lambda. This allows compilation of include files 
;;        and other instances where multiple XML source strings must be compiled 
;;        as a single XML document. 
;;
;;        The inputLambda must be an Lambda which will provide strings of complete
;;        XML source via its (moreSource) child Lambda. The inputLambda must support 
;;        the moreSource child Lambda by returning either #void (if there is no 
;;        more source) or the next input string in the proper sequence.
;;
;;        Each input string must be a complete XML fragment.
;;
;;          Syntax:(inputLambda.moreSource) 
;;
;;          Returns An complete XML input string or #void
;; 
;;          Example
;; 
;;               (inputLambda.moreSource)
;;
;;             Returns the string
;;
;;               "<?xml version=?1??><Name>John</Name>"
;; 
;;          Each input string must be a complete XML fragment. 
;;          This means that each input string must end at an end tag boundary.
;;    
;;        ====================
;;        Event Manager Lambdas
;;        ====================
;; 
;;        Compiling A String with event handling
;;        The xml function supports the compilation of a single source string with event handling. 
;;        No XML document tree model is returned. The second argument must be an Lambda which will 
;;        handle document events (eventMgr). 
;;
;;        The eventMgr must support the methods exposed in the Chapter on Document Event Handlers.
;;
;;          Syntax: (xml inputString eventMgr)
;;
;;        Example
;;
;;            (xml "<?xml version=?1??><?javaScript writeln(?Hello world?);?>"  eventMgr)
;;
;;        Sends the following events to the eventMgr Lambda:
;;
;;            (eventMgr.startDocument)
;;            (eventMgr.startElement "xml" #{version: 1})
;;            (eventMgr.processingInstruction "javaScript"  "writeln(?Hello world?);")
;;            (eventMgr.endElement "xml")
;;            (eventMgr.endDocument)
;;
;;        ==================
;;        XML Document Model
;;        ==================
;; 
;;        The XML document model is a recursive Structure with the data inside the 
;;        XML document addressable by attribute as well as by numeric index. For 
;;        example the following XML input string:
;;
;;
;;      	<?xml version = '1.0' standalone='yes' encoding = 'hello' ?>
;;       	  <!-- This is a dynamic name and address example -->
;;            <Address FirstName = "yes" LastName = 'yes'> 
;;              This is just general content for the Address element.
;;              <FirstName>Michael</FirstName>
;;              <LastName>Korns</LastName>
;;              <Street>214 Shorebreaker</Street>
;;              <City>Laguna Niguel</City>
;;              <State>California</State>
;;              This is more content for the Address element.
;;            </Address>
;;
;;        Returns the following XML document model:
;;
;;            #{
;;              __attlist: #{version: '1.0' standalone: 'yes' encoding: 'hello'}
;;              Address: #{
;;                         __attlist: #{FirstName: "yes" LastName: 'yes'}
;;                         __content: "This is just general content for the Address element."
;;                         FirstName: "Michael"
;;                         LastName:  "Korns"
;;                         Street:    "214 Shorebreaker"
;;                         City:      "Laguna Niguel"
;;                         State:     "California"
;;                         __content: "This is more content for the Address element."
;;                        }
;;             }
;;
;;        Notice how the terminal nodes of the Structure are all singletons, 
;;        while the intermediate nodes of document Structure are recursive element
;;        Structures with attributes and values. Finally, notice how the various
;;        parts of the document model can be referenced either by attribute or
;;        by element index number:
;;
;;           document.Address.FirstName == "Michael"
;;                  
;;                      -or-
;;
;;           document[4][2] == "Michael"
;;
;;        The XML document model also handles multiple elements with the same tag 
;;        using the simple expedient that only the first (of non-unique) element can
;;        be referenced by its tag, all others must be referenced by numeric index. 
;;        For example the following XML input string:
;;
;;           <?xml version="1.0"?>
;;             <poem>
;;               <line>Roses are red,</line>
;;               <line>Violets are blue.</line>
;;               <line>Sugar is sweet,</line>
;;               <line>and I love you.</line>
;;             </poem>
;;
;;        Returns the following XML document model:
;;
;;           #{
;;             __attlist: #{version: "1.0"}
;;             poem: #{
;;                     line: "Roses are red,"
;;                     line: "Violets are blue."
;;                     line: "Sugar is sweet,"
;;                     line: "and I love you."
;;                     }
;;            }
;;
;;       Notice how the terminal nodes of the Structure are all singletons, 
;;       while the intermediate nodes of document Structure are recursive element
;;       Structures with attributes and values. Finally, notice how the first
;;       "line" element can be referenced either by name or by index and all
;;       other "line" elements can only be referenced by index number:
;;
;;          document.poem.line == "Roses are red,"
;;                    
;;                       -or-
;;
;;          document.poem[0] == "Roses are red,"
;;                    
;;                       -or-
;;
;;          document.poem[2] == "Sugar is sweet,"
;;
;; *******************************************************************
    pvars:(;; Public child methods
           recAttributes					;; Recognize the attributes in an XML document.
           recCdata 					    ;; Recognize the CDATA element of an XML document.
           recComments					    ;; Recognize the comments in an XML document.
           recContents					    ;; Recognize the content of an XML document.
           recDoctypeHeader                 ;; Recognize a document type header in an XML document.
		   recElement						;; Recognize an element tag in an XML document.
           recName   						;; Recognize a name in an XML document.
           recProcessInstruction			;; Recognize a process instruction in an XML document.
           recScriptInstruction				;; Recognize a script in an XML document.
           recValue							;; Recognize a value in an XML document.
           xmlError							;; Generate a compiler error for an XML document.
           myDOM                            ;; Document model in Structure form
           buffer                           ;; Incomplete document model referenced by the Lambdas
          (Boolean:traceLinesOn false)      ;; Option to display tracelines
          (Boolean:errorCheckOn false)      ;; Option to call the xmlError Lambda when there are errors
                                            ;; in the HTML page evaluated (e.g. mismatched tags, no contents
                                            ;; between html tags, etc.)
          (Boolean:stemmingOn false)        ;; Option to call the porterStemmer, passing to it
                                            ;; a byte vector containing text between html tags
          (Boolean:inputData false)         ;; Option to put meaningful data resulting from the XML compilation
                                            ;; inside a string for further processing
           outputVector                     ;; Output of the runStructure Lambda        
           runStructureTop                  ;; Functions that walks the structure, returning the
           runStructure                     ;; values of the structure as a String
           stemmedVector                    ;; Ouput of the runStructure and runStructureTop functions
          (ByteVector:stemVector)           ;; Ouput if te stemming option is true, this will be 
                                            ;; passed to the porterStemmer for stemming
          (nn 0)                            ;; Number of words inside the stemVector persistent variable(above)
           global
		    tempChar
           ) ;; end of persistent variables
    
    vars:(arg                          	    ;; Argument temporary variable
          elementName                       ;;  XML name to be evaluated
  
           ) ;; end of temporary variables

       pvars:(;; LTN Note: The persistent variables below are the elements of the work Structure being updated           	 
            	(Integer:past #void)    			;; for work.past
            	(Integer:start #void)				;; Integer representation for CharPointer work.start
            	(Integer:INLEN #void)  				;; Integer representation for CharPointer work.INLEN
            	(Integer:IP #void)   				;; Integer representation for CharPointer work.IP
	         	(Boolean:inputProvider false)  		;; for work.inputProvider           	
            	(Boolean:eventHandler false)		;; for work.eventHandler            	
            	(Boolean:piHandler false)   		;; for work.piHandler
            	(Boolean:htmlOn false)      		;; for work.htmlOn
            	(piStructure #void)                 ;; Structure: for work.piStructure
                (Document #void) 				    ;; Structure: for work.Document 
                (inputLambda #void)           		;; Lambda: for work.inputLambda
				(eventLambda #void)                  ;; Lambda:for work.eventLambda
            	(inputString #void) 		        ;; String: for work.inputString
            	 stringBuf                  		;; String: for work.stringBuf LTN Note: Tobe initilaized later
				stringBufNew
             )  ;;end pvars: 


    regs: ((CharPointer:p) Integer:c Integer:n)
    regs: ((WordPointer:wp)) ;; MFK Added to allow memory speed instruction access to the critical work structure in persistant memory.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LTN: This functions allows Structures, Dictionary and 
;;      Vectors to be displayed per key-value pair
;; Input:    buffer     Byte Vector where the new structure,
;;                      dictionary, or vector are temporarily stored.
;;           memory     The input structure, dictionary or structure
;;                      to be evaluated.
;;           margin     Margins of the printed structure, dictionary
;;                      or vector.
;; Output:              Prints out the output.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun prettyPrint (buffer memory margin)
vars:(n N key newMargin sourceTemplate
            (blanks "                                                                                                                                                               ")
            (tailSW false)
            ) ;; end temporary variables
      
;; Initialize the display buffer (if necessary).
 
(if (= buffer #void)
    (begin 
         (setq buffer (new Vector: byte: 2000000))
         (appendWriteln buffer  _eol (left blanks margin))
         (setq tailSW true)
     ) ;; end begin
) ;; end if
 
(cond
    ;; Manage pretty printing of a Structure 
    ((= (type memory) Structure:)
        (begin    
            (setq N (length memory))             
            (setq newMargin (+ margin 2))
            (appendWriteln buffer "#{")
             
            (loop for n from 0 until N do                   
                (setq key memory[n 0])               
                (appendWriteln buffer key ": ")                 
                (if (isStructure memory[n 1])
                   (begin
                      (prettyPrint buffer memory[n 1] (+ newMargin (length key) 2))
                    ) ;; end begin
                   else
                  (begin
                      (appendWriteln buffer memory[n 1])
                   ) ;; end begin
                 ) ;; end if
                (appendWriteln buffer _eol (left blanks newMargin))
            ) ;; end loop
            (appendWriteln buffer "} ; end Structure")
          ) ;; end begin
     ) ;; end if

      ;; Manage pretty printing of a Dictionary
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
    
        ;; Manage pretty printing of a Vector
       ((= (type memory) Vector:)
           (begin              
              (setq N (length memory))
              (setq newMargin (+ margin 2))
              (appendWriteln buffer "#( " _eol  (left blanks newMargin))
 
              (loop for n from 0 until N do  
                 
                 (appendWriteln buffer (string memory[n] true))
                 (appendWriteln buffer _eol (left blanks newMargin))
                 ) ; end loop
              (appendWriteln buffer "} ; end Vector")
           ))
 );; end cond
 
;; Terminate the display buffer (if necessary).
(if (= tailSW true)
    (begin 
       (setq buffer (string buffer))
     )
) ;; end if
       
buffer) ;; end prettyPrint

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LTN:   This Function creates a First Level Dictionary that contains all the words in 
;;        the input string and the number of ocurrences of that word in the inputString.
;; Input: inputString        The String to be evaluated
;; Output myDictionaryOne    (display xml.myDictionaryOne)
;;                            A dictionary containing each word and its number of
;;                            occurences in the String
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun dictionaryOne(inputText)
   pvars: (myDictionaryOne)
   vars: (textVector n N tempString tempStringCount)
 
  (setq tempStringCount 0)
  (setq myDictionaryOne (new Dictionary:))

  (setq textVector (new Vector: Byte: 20000))
  (if (isVector inputText) 
      (begin
          (setq textVector inputText)
       ) ;; end begin
       else
       (begin
          (setq textVector (stringToBVector (clean inputText) (append " " #\newline #\return #\tab)  true))
       ) ;; end begin
  ) ;; end if


  
  
  (setq N (length textVector)) 
  
  (loop for n from 0 until N do      
      (setq tempString  (ref textVector n))
      ;;(setq tempString (substitute tempString "." ""))
      (setq tempString (trim tempString))
                
      (if (isInteger (member tempString myDictionaryOne)) 
            (begin                 
                 (setq tempStringCount (ref myDictionaryOne (symbol tempString)))                  
                 (++ tempStringCount)                 
                 (setq myDictionaryOne tempString tempStringCount)                  
            ) ;; end begin
            else
           (begin                  
                 (if (compareNE tempString "")(setq myDictionaryOne (symbol tempString) 1))                 
           ) ;; end begin
      ) ;; end if
    ) ;; end loop 
 ) ;; end for dictionaryOne


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LTN:   This Function creates a Second Level Dictionary that contains all the words in 
;;        the input string and a dictionary of the words after the current word being
;;        evaluated.
;; Note:   This function requires that the function dictionaryOne has been exceuted in
;;         the same input string.
;; Input:  inputString        The String to be evaluated
;; Output: myDictionaryTwo    (display xml.myDictionaryTwo)
;;                            A dictionary containing each word and the dictionary of the
;;                            words subsequent to the words begin evaluated. 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun dictionaryTwo(inputText)
  pvars: (myDictionaryTwo)
   vars: (textVector  nextWord keyword keywordDict valueDict m n M N temp lastWord )

  (setq temp (new Dictionary: ))
  (setq myDictionaryTwo (new Dictionary:))
  (setq textVector (new Vector: Byte: 20000))
 
 
  (if (isVector inputText) 
      (begin           
          (setq textVector inputText)
       ) ;; end begin
       else
       (begin
          (setq textVector (stringToBVector (clean inputText) (append " " #\newline #\return #\tab)  true))
       ) ;; end begin
  ) ;; end if
  
    
    
  (setq N (length textVector)) 
  (setq M (length myDictionaryOne))

  ;; looping for text vector    
  (loop for n from 0 until N do      
      (setq keyword  (ref textVector n))     
      (setq keyword (trim keyword))      
      (if (not (isCharWhitespace keyword))
          (begin
              (setq next (+ n 1))  
              (if (and (compareNE (right keyword 1) ".") (< next N))
                  (begin
                      (setq nextWord (ref textVector next))
                      (setq nextWord (trim nextWord))
                      
                      (if (compareEQ (right nextWord 1) ".") 
                          (begin                              
                             (setq nextWord (substitute nextWord "." ""))
                           ) ;; end begin
                      ) ;; end if 
                      (setq lastWord false)
                   )
                  else
                 (begin
                     (setq nextWord "")
                     ;;(setq keyword (substitute keyword "." ""))
                     (setq lastWord true)
                 ) ;; end begin
               ) ;; end if for period
               
            ;; create dictionary value   
            (if  (and (not (isDictionary myDictionaryTwo[keyword])) (= lastWord false))
                (begin
                    (setq myDictionaryTwo[keyword] (new Dictionary:))
                 ) ;; end begin
             ) ;; end of if condition 
 
             ;; loop over the values of dictionaryOne       
             (loop for m from 0 until M do
                 
                 (setq keywordDict (ref myDictionaryOne m 0))
                 (setq keywordDict (string keywordDict))
                  
                 (if (and (compareEQ keywordDict keyword) (= lastWord true))
                      (begin                           
                          (setq valueDict (ref myDictionaryOne m)) 
                          (setq myDictionaryTwo keywordDict valueDict)
                      ) ;; end begin
                 ) ;; end if
  
                 (if (and (compareNE keywordDict "") (compareNE nextWord "") (compareEQ keywordDict nextWord))
                     (begin
                         (setq valueDict (ref myDictionaryOne m))                        
                         ;; set the key-value pair in the dictionary
                         (setq keywordDict (symbol keywordDict))
                         (setq temp myDictionaryTwo[keyword])
                         (setq temp  keywordDict valueDict) 
                         (setq myDictionaryTwo[keyword] temp)                       
                      ) ;; end begin
                 ) ;; end if
             ) ;; end loop for values of dictionaryOne             
         ) ;; end begin for isCharWhitespace
       ) ;; end if for idCharWhitespace
    ) ;; end loop for dictionaryTwo
) ;; end dictionaryTwo
               

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LTN:   This Function creates a Third Level Dictionary that contains all the words in 
;;        the input string and dictionaries of the words after the current word being
;;        evaluated.
;; Note:   This function requires that the functions dictionaryOne and dictionaryTwo
;;         has been exceuted in the same input string.
;; Input:  inputString        The String to be evaluated
;; Output: myDictionaryThree  (display xml.myDictionaryThree)
;;                            A dictionary containing each word and the dictionaries of the
;;                            words subsequent to the words begin evaluated. 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun dictionaryThree(inputText)
   pvars: (myDictionaryThree)
   vars: (textVector nextWord keyword keywordDict valueDict m n M N temp lastWord )

  (setq temp (new Dictionary: ))
  (setq myDictionaryThree (new Dictionary:))   
  (setq lastWord false)

   (setq textVector (new Vector: Byte: 20000))
   (if (isVector inputText) 
      (begin           
          (setq textVector inputText)
       ) ;; end begin
       else
       (begin
          (setq textVector (stringToBVector (clean inputText) (append " " #\newline #\return #\tab)  true))
       ) ;; end begin
  ) ;; end if
  
  (setq N (length textVector)) 
  (setq M (length myDictionaryTwo))
   
   ;; looping for values in the input Text
  (loop for n from 0 until N do           
      (setq keyword  (ref textVector n))     
      (setq keyword (trim keyword))
      (if (not (isCharWhitespace keyword))
          (begin
              (setq next (+ n 1))     
              (if (and (compareNE (right keyword 1) "." ) (< next N))
                  (begin
                      (setq nextWord (ref textVector next))
                      (setq nextWord (trim nextWord))
                      (if (compareEQ (right nextWord 1) ".") 
                          (begin                              
                             (setq nextWord (substitute nextWord "." ""))
                           ) ;; end begin
                      ) ;; end if 
                      (setq lastWord false)
                   ) ;; end begin
                   else
                  (begin
                      (setq nextWord "")
                      ;;(setq keyword (substitute keyword "." ""))
                      (setq lastWord true)
                   ) ;; end begin
               ) ;; end if for period
                
                ;; create dictionary value   
               (if  (and (not (isDictionary myDictionaryThree[keyword])) (= lastWord false))
                   (begin
                       (setq myDictionaryThree[keyword] (new Dictionary:))
                    ) ;; end begin
                ) ;; end of if condition

               ;; loop over the values of dictionayTwo
               (loop for m from 0 until M do
                    (setq keywordDict (ref myDictionaryTwo m 0))
                    (setq keywordDict (string keywordDict))

                    (if (and (compareEQ keywordDict keyword) (= lastWord true))
                        (begin
                             (setq valueDict (ref myDictionaryOne m)) 
                             (setq myDictionaryTwo keywordDict valueDict)
                        ) ;; end begin
                    ) ;; end if
             
                    (if (and (compareNE keywordDict "") (compareNE nextWord "") (compareEQ keywordDict nextWord))
                        (begin
                             (setq valueDict (ref myDictionaryTwo m))
                              ;; set the key-value pair in the dictionary
                             (setq keywordDict (symbol keywordDict))
                             (setq temp myDictionaryThree[keyword])
                             (setq temp  keywordDict valueDict)
                             (setq myDictionaryThree[keyword] temp)
                         ) ;; end begin                
                    ) ;; end if
                ) ;; end loop for values of dictionaryTwo
             ) ;; end begin for isCharWhitespace
          ) ;; end if for isCharWhitespace
    ) ;; end loop for inputText
)  ;; end dictionaryThree


(defun runStructureTop (outputStructure)
vars: (n N outputVector v vecLength p testItem (ByteVector:testingXml) (ByteVector:outputVector))
pvars: ((Vector:stemmedVector) )   
   (setq outputVector (new Vector: Byte:1000000  ))
   ;;(setq stemmedVector (new Vector: Byte:100000000))
   (setq outputVector (xml.runStructure outputStructure outputVector)) 
   (setq outputVector (string outputVector true))    
   (setq stemmedVector (porterStemmer  outputVector ))
    
) ;; end runStructureTop


(defun runStructure (outputStructure outputVector)
vars: (n N  v vecLength p testItem  )
pvars: (ByteVector:outputVector) 

(if (= outputVector #void) (setq outputVector (new Vector: Byte:1000000  )))
(setq p 0)
(setq n 1)
(setq N  (length outputStructure) )
 
(loop for n from 0 until N do    
    (setq testItem (ref outputStructure n))       
    (if (not (isStructure testItem))
        (begin
            ;;(setq testItem (string testItem true))
            (appendWriteln outputVector testItem " ")            
         ) ;; end begin
         else
         (begin              
             (runStructure testItem outputVector)                                              
         ) ;; end begin
    ) ;; end if
) ;; end loop
outputVector) ;; end defun

 

    ;; *******************************************************************
    ;;  Begin main compiler code.
    ;; *******************************************************************
 
    (setq recElementCtr 0)
    (setq recContentsCtr 0)
	(setq global #void)
	(setq startTime 0)
	(setq endTime 0)
	(setq startTime (getTickCount 0))
 
    ;; Initialize the basic state of the compiler.
    (if (= traceLinesOn true)(writeln "NEW XML") );; LTN
    (if (= traceLinesOn true)(writeln "xml: errorCheckOn: "  errorCheckOn)) ;; LTN
    (setq stemVector #void)
    (if (= stemmingOn true) (setq stemVector (new Vector: Byte: 100000000)))
    (setq n 0)
#if 0  ;; MFK 
    ;;(setq work (new work)) ;; MFK This is an error as it replaces work with a copy of a constant which destroys the constant.
#else  ;; MFK 
    ;;(setq work (new WORK)) ;; MFK This is a better way of getting a new constant.
#endif ;; MFK 

    ;; WORK VARIABLE  start
    ;;(setq wp work) ;; MFK Note: This register pointer assignment allows register offset addressing for extra speed.
    ;;(setq wp[WORK(inputString)] #void)  ;; MFK Note: This register offset memory instruction works about 20 times faster than the Lisp Generic (setq work.inputString #void)
    ;;(setq wp[WORK(inputProvider)] false)
    ;;(setq wp[WORK(inputLambda)] #void)
    ;;(setq wp[WORK(eventHandler)] false)
    ;;(setq wp[WORK(eventLambda)] #void)
    ;;(setq wp[WORK(piHandler)] false)
    ;;(setq wp[WORK(piStructure)] #void)
    ;;(setq wp[WORK(htmlOn)] true)
    ;;(setq wp[WORK(stringBuf)] (new String: ""))
    ;;(resize wp[WORK(stringBuf)] 1024)
    ;; WORK VARIABLE end

    ;; LTN: Additional initialization for work.stringBuf
    (setq stringBuf (new String: ""))    
	(setq stringBufNew (new String: ""))  
	(resize stringBufNew 1024)        
    (resize stringBuf 1024)
	(setq tempChar (new String: ""))
	(resize tempChar 1024)
            	 

    ;; Initialize the xml document model (if any)
    (if (and (> (argCount) n) (isStructure (setq arg (argFetch n))))
        ;; We have an existing Document model to extend.
        (begin
           (++ n)
           (setq Document arg) 
        )
        ;; We must create a new Document model.
        (begin
           (setq Document (|Gv:new| Structure:)) 
        )) ; end if
    ;; Initialize the xml input source
    (if (and (> (argCount) n) (isLambda (setq arg (argFetch n))))
        ;; We have an input Lambda.
        (begin
           (++ n)     
           (if (= traceLinesOn true)(writeln "xml: in argCount isLambda: arg:  " arg )) ;; LTN
           (setq inputProvider true)
           (setq inputLambda arg) 
           (setq inputString (setq arg (inputLambda.moreSource)))
           (setq INLEN (length arg)) 
           (setq IP (setq p arg)) 
        )
        ;; We have a single input source string.
        (begin
           (++ n)
           (setq inputProvider false)
           (setq inputString arg) 
           (setq inputLambda #void) 
           (setq INLEN (length arg)) 
           (setq IP (setq p arg)) 
        )) ; end if
    ;; Initialize the event manager (if any)
    (if (and (> (argCount) n) (isLambda (setq arg (argFetch n))))
        ;; We have an event manager Lambda.
        (begin
           (++ n)
           (if (= traceLinesOn true)(writeln "xml: in argCount isEventManagerLambda:arg " arg)) ;; LTN
           (setq eventLambda arg) 
           (setq eventHandler true) 
           (setq piStructure #void) 
           (setq piHandler false) 
        )) ; end if
    ;; Initialize the pi structure (if any)
    (if (and (> (argCount) n) (isLambda (setq arg (argFetch n))))
        (begin
           (++ n)
           (if (= traceLinesOn true)(writeln "xml: in argCount pistructure:arg " arg) );; LTN
           (setq piStructure arg) 
           (setq piHandler true) 
           (setq eventLambda #void) 
           (setq eventHandler false) 
        )) ; end if
    (if (= n 0) (error "xml: expecting at least one argument"))

    ;; Create the XML document object which will be the result of this compilation.
    ;; Note: If we are using an event handler, we do not create a document. */
    (if (= eventHandler true)
	    (begin
	       ;; Send the startDocument event to the eventLambda.
	       (setq Document #void)
           (if (= traceLinesOn true)(writeln " xml: EventLambda: startDocument" )) ;; LTN            
	       (eventHandler.startDocument)
            ;;(writeln "startDocument: myDOM: " myDOM)
	    )) ; end if

	;; Initialize the input string pointer and the starting input pointer.

    (setq past 0)
    (setq start IP)

    ;; Recognize the content of the XML document.
    
    (recContents Document 0)
 
    (if (= traceLinesOn true)(writeln "xml: result of recContents: " Document) );; LTN

    ;; There should be no more content left.
    (setq p (Integer: IP))
    (setq c p[0])
    (if (= traceLinesOn true)(writeln "xml: work.ip is: " c))
    (if (= errorCheckOn true)(if (<> c 0) (error "xml: expected end of file")))
     
    ;; Return the XML document object resulting from this compilation.
    ;; Note: If we are using an event handler, we send the endDocument message.
    (if (= eventHandler true)
	    (begin
	       ;; Send the endDocument event to the eventLambda.
	       (setq Document #void)
	       (eventLambda.endDocument)
	    )) ; end if
   
    ;;(setq buffer (prettyPrint buffer work.Document 2))
    
    ;;(runStructureTop work.Document)
    (if (= stemmingOn true) (setq stemVector (porterStemmer stemVector)))
    ;;(if (= traceLinesOn true) (writeln "afterStemming: " stemVector))
    (setq endTime (getTickCount startTime))
     ;;(writeln "endTime: " endTime)

  
Document) ;; end xml


































;;**EXPORTKEY**:xml:recAttributeList
(defchild xml:recAttributeList()
;; ******************************************************************************************
;;  Summary: This Function recognizes the contents of XML elements. These 
;;           can be either normal elements or process instruction elements 
;;           For example:
;;
;;             <?javaScript writeln("Hello World!"); ?>
;;
;;          				-or-
;;
;;             <Address> <FirstName>Michael</FirstName> <LastName>Korns</LastName> </Address> 
;;
;;           If no content is recognized, an empty string literal is returned.
;;           If singleton content is recognized, a string literal is returned;
;;           otherwise, a Structure of the attributed contents is returned. 
;;           The current input pointer is always left pointing to the first 
;;           non whitespace character which is not element content.
;; 
;; Args:     document      A Structure to be extended as an XML document
;;                         in tree model form. After being extended this 
;;                         structure is then returned as the result.
;;           work          The compiler work space which allows the xml
;;                         compiler to be invoked recursively.
;;           tagDepth      The current xml tag recursion level.
;;
;; Return:   result        The extended XML document in tree model form (if present).
;;
;; ******************************************************************************************
regs:(CharPointer:tIP  cc)
regs:((_a #\a) (_z #\z) (_A #\A) (_Z #\Z) (underScore #\_) (space 32) (nill 0) (equal 61)  )
vars:(name value attributes)

(if (= traceLinesOn true)(writeln "---recAttributeList START--- " )) ;; LTN
;;(setq tIP work.IP)
(vmregLoadInteger IP tIP)
(setq cc tIP[0])
(if (= traceLinesOn true)(writeln "recAttributeList: start is: " (char cc))) ;; LTN
 
Fetch::
;; Try to recognize the attribute name. 
(if (or (and (>= cc _a) (<= cc _z)) (and (>= cc _A) (<= cc _Z)) (= cc underScore)  ) 
    (begin   
    (setq name (recName))
    (if (= traceLinesOn true)(writeln "recAttributeList: in CHARACTERS attribute name: " name )) ;; LTN 
    (if (isEqual name #void) then (if (= errorCheckOn true)(xmlError "xml: Invalid element attribute name"  )))
     ;;Promote input pointer past whitespace
    (vmregRunInHardware start:)
    ;;(setq tIP work.IP) 
    (vmregLoadInteger IP tIP)
    (if (= traceLinesOn true)(writeln "recAttributeList: after recName: " (char tIP[0]) )) ;; LTN 
    (setq cc tIP[0])      
    (while (and  (<= cc space) (> cc nill)) do (++ tIP)(setq cc tIP[0]))(setq IP tIP)
    (vmregRunInHardware stop:)
    (if (= traceLinesOn true)(writeln "recAttributeList: after promoting whitespace: " (char tIP[0]) )) ;; LTN   
     
    ;; Check if character is "=" indicating an attribute
    (if (compareEQ tIP[0] equal)
        (begin            
            (++ tIP)
            (setq IP tIP)                   
            ;;Promote input pointer past whitespace
            (if (= traceLinesOn true)(writeln "recAttributeList: past = sign: " (char tIP[0]) )) ;; LTN 
            (vmregRunInHardware start:)
            ;;(setq tIP IP) 
            (vmregLoadInteger IP tIP)
            (setq cc tIP[0])
            (while (and  (<= cc 32) (> cc 0)) do (++ tIP)(setq cc tIP[0]))(setq IP tIP)
            (vmregRunInHardware stop:)     
            (if (= traceLinesOn true)(writeln "recAttributeList: after promoting past whitespace: " (char tIP[0]) )) ;; LTN        
            (setq value (recValue))
            (if (= traceLinesOn true)(writeln "recAttributeList: value is: " value)) ;; LTN
            (if (compareEQ value #void) (if (= errorCheckOn true)(xmlError "xml: Invalid element attribute value"  )))

            ;;Promote input pointer past whitespace
            (vmregRunInHardware start:)          
            ;;(setq tIP IP) 
            (vmregLoadInteger IP tIP)
            (setq cc tIP[0])              
            (while (and  (<= cc 32) (> cc 0)) do (++ tIP)(setq cc tIP[0]))(setq IP tIP)
            (vmregRunInHardware stop:)
            (if (= traceLinesOn true)(writeln "recAttributeList: after promoting past whitespace: " (char cc) )) ;; LTN  
        ) ;; end begin
        else
        (begin
             (if (= traceLinesOn true)(writeln "recAttributeList: there is no = sign ")) ;; LTN 
             (if (compareEQ htmlOn true)
                 (begin (setq value true))
                 else
                 ;;(begin (error "xml:  Expecting = symbol"))
                 ;; for PI attributes bug
                 (goto Default: ) ;; LTN
             ) ;; end if
         ) ;; end begin
     ) ;; end if
      
      
     ;; Append the attribute to the attribute list Structure.
     (if (compareEQ  attributes #void)
         (begin
            (setq attributes (new Structure:))   
         ) ;; end begin
     ) ;; end if         
             
     ;; Note: the attributes will be assigned in the last position of the Structure. 
     (set attributes last: name value)          
     (setq result attributes)       
     (goto Fetch:)
   ) ;; end begin
) ;; end if

   (if (and (> cc nill) (<= cc space))   
       (begin    
           ;;Promote input pointer past whitespace
           (if (= traceLinesOn true)(writeln "recAttributeList: in WHITESPACE attribute name" )) ;; LTN 
           (vmregRunInHardware start:)
           ;;(setq tIP work.IP)      
           (vmregLoadInteger IP tIP)   
           (setq cc tIP[0])(while (and  (<= cc 32) (> cc 0)) do (++ tIP)(setq cc tIP[0]))(setq IP tIP)    
           (vmregRunInHardware stop:)
           (if (= traceLinesOn true)(writeln "recAttributeList: after promoting past whitespace: " (char tIP[0]) )) ;; LTN      
           (goto Fetch:)
        ) ;; end begin
    ) ;; end if

    Default::
    (if (= traceLinesOn true)(writeln "recAttributeList: in Default label  "  )) ;; LTN  
    ;; return the attribute     
   (if (= traceLinesOn true)(writeln "---recAttributeList END--- result:  " result  )) ;; LTN  
 
 
result) ;; end recAttributes


#if 0

FXml_AttributeList

This Function recognizes attribute lists inside the element tag of an XML
element. For example:

	<?xml version = '1.0' standalone='yes' encoding = 'hello' ?>

						-or-

	<Address FirstName = "yes" LastName = 'yes'> 


If an attribute list is recognized, a Structure of the attributes and
their values is returned; otherwise, void is returned. The current
input pointer is always left pointing to the first non whitespace
character which is not an attribute name.

TVAL FXml_AttributeList(LpXCONTEXT gCP,LpTHREAD gTP,XMLWork* work)       
{
StartFrame
DeclareTVAL(attributes);
DeclareTVAL(name);
DeclareTVAL(value);
DeclareTVAL(ec);
EndFrame

/* Parse the input string looking for the attributes. */

Fetch:
switch (*work->IP)
	{
	case 'a':
	case 'b':
	case 'c':
	case 'd':
	case 'e':
	case 'f':
	case 'g':
	case 'h':
	case 'i':
	case 'j':
	case 'k':
	case 'l':
	case 'm':
	case 'n':
	case 'o':
	case 'p':
	case 'q':
	case 'r':
	case 's':
	case 't':
	case 'u':
	case 'v':
	case 'w':
	case 'x':
	case 'y':
	case 'z':
	case 'A':
	case 'B':
	case 'C':
	case 'D':
	case 'E':
	case 'F':
	case 'G':
	case 'H':
	case 'I':
	case 'J':
	case 'K':
	case 'L':
	case 'M':
	case 'N':
	case 'O':
	case 'P':
	case 'Q':
	case 'R':
	case 'S':
	case 'T':
	case 'U':
	case 'V':
	case 'W':
	case 'X':
	case 'Y':
	case 'Z':
	case '_':

		/* Try to recognize the attribute name. */

		Stack(name) = FXml_recName(gCP,gTP,work);
		if (Stack(name).Tag == TYVOID) FrameExit(FXml_Error(gCP,gTP,"Invalid element attribute name",work));;
		SP		/* Promote input pointer past whitespace */

		/* Do we have an = symbol, indicating a attribute value follows? */
 
		if (*work->IP == '=') 
			{
			++work->IP;	/* Promote input pointer past = symbol */
			SP		/* Promote input pointer past whitespace */
			Stack(value) = FXml_recValue(gCP,gTP,work);
			if (Stack(value).Tag == TYVOID) FrameExit(FXml_Error(gCP,gTP,"Invalid element attribute value",work));;
			SP		/* Promote input pointer past whitespace */
			}
		else
			{
			if (work->htmlOn == TRUE)
				{
				Stack(value) = TBOOL(TRUE);
				}
			else
				{
				FrameExit(FXml_Error(gCP,gTP,"Expecting = symbol",work));;
				}
			}

		/* Append the attribute to the attribute list Structure. */

		if (Stack(attributes).Tag == TYVOID)
			{
			/* Create the attributes Structure (if necessary). */

			Stack(attributes) = FSmartbase_Eval(gCP,gTP,gNewStructure,0);
			ExitOnError(Stack(attributes));
			}

        /* Note: the attributes will be assigned in the last position of the Structure. */

		Stack(ec) = FSmartbase_Set(gCP,gTP,4,Stack(attributes),gLast,Stack(name),Stack(value));
		ExitOnError(Stack(ec));

		goto Fetch;
    
		break;

	case 1:
	case 2:
	case 3:
	case 4:
	case 5:
	case 6:
	case 7:
	case 8:
	case 9:
	case 11:
	case 12:
	case 14:
	case 15:
	case 16:
	case 17:
	case 18:
	case 19:
	case 20:
	case 21:
	case 22:
	case 23:
	case 24:
	case 25:
	case 26:
	case 27:
	case 28:
	case 29:
	case 30:
	case 31:
	case 32:
		SP		/* Promote input pointer past whitespace */
		goto Fetch;
		break;

	default:
		goto Done;
		break;

	}

/* Return the XML attribute list object (if any). */

Done:
FrameExit(Stack(attributes));

/* Return the XML attribute list object (if any). */

Error:
FrameExit(FXml_Error(gCP,gTP,"Invalid element attribute",work));
}
#endif
  


































;;**EXPORTKEY**:xml:recCData
(defchild xml:recCData(result)
;; ******************************************************************************************
;; This Function recognizes HTML CDATA sections.
;; For example:
;;
;;
;;	<![CDATA[ any data at all can be in here ]]>
;;
;;          				-or-
;;
;;             <Address> <FirstName>Michael</FirstName> <LastName>Korns</LastName> </Address> 
;;
;;           If no content is recognized, an empty string literal is returned.
;;           If singleton content is recognized, a string literal is returned;
;;           otherwise, a Structure of the attributed contents is returned. 
;;           The current input pointer is always left pointing to the first 
;;           non whitespace character which is not element content.
;; 
;; Args:     document      A Structure to be extended as an XML document
;;                         in tree model form. After being extended this 
;;                         structure is then returned as the result.
;;           work          The compiler work space which allows the xml
;;                         compiler to be invoked recursively.
;;           tagDepth      The current xml tag recursion level.
;;
;; Return:   result        The extended XML document in tree model form (if present).
;;
;; ******************************************************************************************
 
regs: ((CharPointer:namep)
       (CharPointer:startp)                  ;; Temporary variable to store the position of the pointer
       (CharPointer:tIP)                     ;; Temporary variable fore saving comment characters
       (Integer:cc)                         ;; Temporary input character
       (Integer:c1) (Integer:c2) (Integer:c3) (Integer:c4) (Integer:c5) (Integer:c6) (Integer:c7) (Integer:c8)
       (_! 33) (leftbracket 91) (_C 67) (_D 68) (_A 65) (_T 84)  (nill 0)
       (Integer:saveCH)                     ;; Temporary input character to save
       (Integer:len )
       (Integer:n 0)
       (Integer:null 0)
      )
vars:(name
      result
      contents
     (ByteVector:cdata)
      tmp 
      name
) 
  
(if (= traceLinesOn true)(writeln "---recCData START---")) ;; LTN 

;;(setq startp IP)
(vmregLoadInteger IP startp)

(if (= traceLinesOn true)(writeln "recCData: startp is : "  (char startp[0]))) ;; LTN
(setq cdata (new Vector: Byte: 100000))
(setq cc startp[0])
(setq c1 startp[1])
(setq c2 startp[2])
(setq c3 startp[3])
(setq c4 startp[4])
(setq c5 startp[5])
(setq c6 startp[6])
(setq c7 startp[7])
(setq c8 startp[8])

;; We recognize marked sections here.                              
;; Note: <![CDATA[ ... ]]> is an example of a marked section.      
;;       Marked sections may occur anywhere in contents sections.  
(if (and (= c1 _!) (= c2 leftbracket) (= c3 _C) (= c4 _D) (= c5 _A) (= c6 _T) (= c7 _A) (= c8 leftbracket))
    (begin
         ;; Promote the input pointer past the mark header. 
         (setq startp (+ 9 startp))
         (if (= traceLinesOn true)(writeln "recCData: after promotion of mark header is: " (char startp[0]))) ;; LTN
         (vmregRunInHardware start:)
         (setq n 0)
         (while  (and (<> (setq cc startp[0]) 0)(or   (<> (setq cc startp[0]) 93) (<> (setq c1 startp[1]) 93) (<> (setq c2 startp[2]) 62)  )) do
             ;; Saves the characters
             (setq tIP startp)
             (setq saveCH tIP[0])             
             (setq cdata n saveCH)
             (setq cdata (++ n) 0) 
             ;; Promote the pointer
             (++ startp)            
         ) ;; end while
         (if (= traceLinesOn true)(writeln "recCData: after getting getting cdata contents startp[0] is: " (char startp[0]))) ;; LTN
         (vmregRunInHardware stop:)

         (resize cdata (++ n))
         ;; FOR STRING
       ;;   debug lines for ByteVector bug
       ;;  (writeln "SystemCheck 02")(systemCheck false) ;; LTN 
       ;;  (writeln "recCData: after while n is: " n) ;; LTN
       ;;  (writeln "recCData: after while (length cdata) is: " (length cdata)) ;; LTN
       ;;   (writeln "recCData: after while (sizeof cdata) is: " (sizeof cdata))
       ;;  (writeln "recCData: after while (type cdata) is: " (type cdata)) ;; LTN
       ;;  (writeln "recCData: after while cdata[0] is: " cdata[0]) ;; LTN
       ;;  (writeln "recCData: after while sizeof (string cdata true) is: " (sizeof (string cdata true))) ;; LTN
         ;;(writeln "recCData: after while cdata is: " cdata) ;; LTN
       ;;  (writeln "recCData: after while cdata is: " cdata) ;; LTN
          ;; Is the CDATA section not closed?  	
	     (if (= cc nill)
		    (begin
               (vmregLoadInteger IP startp)
		       ;;(setq startp IP)
               (if (= traceLinesOn true)(writeln "recCData: error: cdata not closed " (char startp[0]))) ;; LTN
		       (if (= errorCheckOn true)(xmlError "xml: CDATA section not closed"  ))
		    ) ;;end begin
         ) ;; end if

       ;; Promote pointer to end of cdata trailer
       (if (<> cc nill) (setq startp (+ 3 startp)))
       (if (= traceLinesOn true)(writeln "recCData: end of cdata trailer ip gets start: " (char startp[0]))) ;; LTN            
       (setq IP startp)
 
      ;; Note: the imbedded comment will be assigned in the last position of the Structure.  
	  (if (= eventHandler true)
          (begin
               (if (= traceLinesOn true)(writeln "recCData: workEvent Handler is true before setting result: " )) ;; LTN
		       ;; Send the comment message symbol to the eventLambda.  
		       ;; Note: we do this only if we are in event handler mode.  
               ;;(setq result last: work.eventLambda 1 contents)
               (eventLambda.characters cdata)
               ;;(writeln "recCData: workEvent Handler is true before setting result: " ) ;; LTN
		  ) ;; end begin
	      else
         (begin
               ;; Add the new comment to the old content using the proper heuristic.  
		       ;;  Note: we do this only if we are in document mode.  
		       (if (or (= result "")(= #void result))
                   (begin
			           ;; If the old content is empty, then just set the new comment.  
		               (setq result cdata)
                       ;;(writeln "recCData: in result is void: result is: " result) ;; LTN
                    ) ;; end begin
                ) ;; end if
		 
                ;; Note: This was integrated with the if statement above
		        ;;(if (= "" result)  
                ;;   (begin
		                ;; If the old content is a null string, then just set the new comment. 
			    ;;        (setq result cdata)
                ;;        (writeln "recCData: in result is null string: result is: " result) ;; LTN
                ;;    );; end begin
                ;;) ;; end if     
                  
                (if  (not (isStructure result))  
                    (begin
				       ;;  If the old content is cdata, then append the new content.  
                       (setq result (append result cdata))
                       ;;(writeln "recCData: in result is not a structure: result is: " result) ;; LTN
	                 ) ;; end begin
                 ) ;; end if
                       
                 (if (isStructure result)
                   (begin                                                        
                       (setq len (length result))
                       ;;(writeln "recCData in result is a Structure  " result) ;; LTN 
                       ;;(writeln "recCData in result is a Structure INLEN is " INLEN) ;; LTN  
                       
                       (if (> len 0)
                         ;;  If the old content is a Structure, then insert the new content.  
			             ;; Note: If the last item inserted was character content,  
			             ;;       then append this content to it.  
                          (begin
                             (setq name (ref result (sub1 len) 0))
                             (if (= traceLinesOn true)(writeln "recCData in  result is a Structure: name is:  " name)) ;; LTN 
                             (setq value (ref result (sub1 len) 1))
                             (if (= traceLinesOn true)(writeln "recCData in  result is a Structure: value is:  " value))  ;; LTN 
                             (if (and (isSymbol name) (compareEQ name "__content")
                             (or (isString value) (isText value)))
                                 (begin
                                     (setq cdata (append value cdata))                                 
                                     ;; old implementation
                                     ;;(setq tmp (symbol "__content"))
                                     ;;(setq result (new Structure:))
                                     ;;(setq tmp cdata)
                                     ;; remove from the structure the last key-value pair
                                     (delete result (sub1 len))
                                     (if (= traceLinesOn true)(writeln "recCData in   result is a Structure: result in if: " result)) ;; LTN                                 
                                 ) ;; end begin
                              ) ;; end if
                           ) ;; end begin
                        ) ;; end if
                             (setq tmp (symbol "__content"))
                             (set result last:  tmp cdata)                            
                             ;;(writeln "recCData in  result is a Structure: result outside if: " result) ;; LTN                            
                          
                     );; end begin
                  ) ;; end if                            
          ) ;; end begin for else eventHandler		 
		);; end if for eventHandler
        (if (= traceLinesOn true)(writeln "---recCData END--- result: " result))
        (return result)
     ) ;; end begin
) ;; end if

   Error::
  (if (= errorCheckOn true)(xmlError "xml: Expecting CDATA section"  ))

#void) ;; end recCData

































;;**EXPORTKEY**:xml:recComments
(defchild xml:recComments(result)
;; ******************************************************************************************
;;  Summary: This Function recognizes the contents of XML elements. These 
;;           can be either normal elements or process instruction elements 
;;           For example:
;;
;;             <?javaScript writeln("Hello World!"); ?>
;;
;;          				-or-
;;
;;             <Address> <FirstName>Michael</FirstName> <LastName>Korns</LastName> </Address> 
;;
;;           If no content is recognized, an empty string literal is returned.
;;           If singleton content is recognized, a string literal is returned;
;;           otherwise, a Structure of the attributed contents is returned. 
;;           The current input pointer is always left pointing to the first 
;;           non whitespace character which is not element content.
;; 
;; Args:     document      A Structure to be extended as an XML document
;;                         in tree model form. After being extended this 
;;                         structure is then returned as the result.
;;           work          The compiler work space which allows the xml
;;                         compiler to be invoked recursively.
;;           tagDepth      The current xml tag recursion level.
;;
;; Return:   result        The extended XML document in tree model form (if present).
;;
;; ******************************************************************************************
regs: ((CharPointer:namep)
       (CharPointer:startp)                  ;; Temporary variable to store the position of the pointer
       (CharPointer:tIP)                     ;; Temporary variable fore saving comment characters
       (Integer:cc)                         ;; Temporary input character
       (Integer:c1) (Integer:c2) (Integer:c3)
       (_! 33) (dash 45) (nill 0) 
       (Integer:n 0)
       (Integer:saveCH)                     ;; Temporary input character to save                
      )
vars:(name n
      result
      contents
     (ByteVector:comment) 
       tmp
) 
 (setq traceLinesOn (boolean traceLinesOn)) 

(if (= traceLinesOn 1)(writeln "---recComments START---") );; LTN
(if (= traceLinesOn true)(writeln "recComments: arguments passed: result: " result) );; LTN

(vmregLoadInteger IP startp)
;;(setq startp work.IP)
(if (= traceLinesOn true)(writeln "recComments: arguments passed: startp get IP: " (char startp[0]))) ;; LTN
(setq comment (new Vector: Byte: 10000))

(setq cc startp[0])
(setq c1 startp[1])
(setq c2 startp[2])
(setq c3 startp[3])

;;  Check for a start of a comment.   
;;  Note: <!--  --> are comment tags. 
(if (and (= c1 _!)(= c2 dash) (= c3 dash))
   (begin
        ;;(writeln "recComments: inside if exp is a comment" ) ;; LTN
	    ;;  Promote the input pointer past the comment header.  
        (setq startp (+ 4 startp))
        (if (= traceLinesOn true)(writeln "recComments: start: past comment header: " (char startp[0]))) ;; LTN
        (setq tIP startp)        
        (setq cc startp[0])
        (setq c1 startp[1])
        (setq c2 startp[2])
        (setq c3 startp[3])
        (if (= traceLinesOn true)(writeln "recComments: start character of comment before while: " cc c1 c2 c3)) ;; LTN
	    (vmregRunInHardware start:)
        ;; We look for the end of the comment.          
        (while (and (<> (setq cc startp[0]) 0) (or (<> cc dash) (<> (setq c1 startp[1]) dash) (<> (setq c2 startp[2]) #\>))) do
	          ;; ip variable saves the characters until the end of comment
              (setq tIP startp)
              (setq saveCH tIP[0])
              (setq comment n saveCH)
              (setq comment (++ n) 0)
              ;; start variable ignores comment characters.             
              (++ startp)
       ) ;; end while
      (vmregRunInHardware stop:)
      (resize comment (++ n))
      
      (if (= traceLinesOn true)(writeln "recComments: comment is: "  comment)) ;; LTN
      (if (= traceLinesOn true)(writeln "recComments: start: end of comment: " (char startp[0]))) ;; LTN
       
        ;; Is the comment not closed? 
        (if (<> (setq cc startp[0]) dash)
            (begin
               (if (= traceLinesOn true)(writeln "recComments: inside comment not closed" ) );; LTN
               (setq IP (- 4 startp))
               (if (= traceLinesOn true)(writeln "recComments: start: error: comment not closed: " (char startp[0]))) ;; LTN               
               (if (= errorCheckOn true)(xmlError "xml: Comment not closed"  ))
             ) ;; end begin
        ) ;; end if

       ;; Promote pointer to end of comment trailer
       (if (<> cc nill)
           (begin
              (setq startp (+ 3 startp))
              (if (= traceLinesOn true)(writeln "recComments: start: past end of comment trailer: " (char startp[0]))) ;; LTN
           );; end begin
       ) ;; end if
       (setq IP startp)
                    
       (if (= traceLinesOn true)(writeln "recComments: current position of pointer: " (char startp[0]))) ;; LTN    
       (if (= traceLinesOn true)(writeln "recComments: before setting or result: result is: " result)) ;; LTN
   
        
      ;; Note: the imbedded comment will be assigned in the last position of the Structure.  
	  (if (= eventHandler true)
          (begin
		       ;; Send the comment message symbol to the eventLambda.  
		       ;; Note: we do this only if we are in event handler mode.  
               (eventLambda.comments comment)
		  ) ;; end begin
	      else
         (begin
               ;; Add the new comment to the old content using the proper heuristic.  
		       ;;  Note: we do this only if we are in document mode.  
		       (if (= #void result)
                   (begin
			           ;; If the old content is empty, then just set the new comment.  
		               (setq result comment)
                       ;;(writeln "recComments: in result is void: result is: " result) ;; LTN
                    ) ;; end begin
                ) ;; end if
		 
		       (if (= "" result)  
                   (begin
		               ;; If the old content is a null string, then just set the new comment. 
			           (setq result comment)
                       ;;(writeln "recComments: in result is null string: result is: " result) ;; LTN
                   );; end begin
                   else
                  (begin
                       (if (not (isStructure result))
                           (begin
				              ;;  If the old content is cdata, then append the new content.  
				              (setq tmp (symbol "__comment"))
                              (setq result (new Structure: tmp: result ))
                              ;;(writeln "recComments: in result is not a structure: result is: " result) ;; LTN
                            ) ;; end begin
                        ) ;; end if
                        ;;(writeln "recComments: in result is   a structure: result is: " result) ;; LTN
                        (setq tmp (symbol "__comment")) 
                        (set result last: tmp comment)
                  ) ;; end begin
               ) ;; end if		   
		    ) ;; end begin		 
      ) ;; end if wor.eventHandler
     (if (= traceLinesOn true)(writeln "---recComments END---")) ;; LTN 
     (return result)
    ) ;; end begin
  ) ;; end for if

  Error::
  
  (if (= errorCheckOn true)(xmlError "xml: Expecting comment expression"  ))

#void) ;; end recComments


































;;**EXPORTKEY**:xml:recContents
(defchild xml:recContents(tDocument Integer:tagDepth) ;; MFK strongly typing variables makes the Lisp code run much faster.
;; ******************************************************************************************
;;  Summary: This Function recognizes the contents of XML elements. These 
;;           can be either normal elements or process instruction elements 
;;           For example:
;;
;;             <?javaScript writeln("Hello World!"); ?>
;;
;;          				-or-
;;
;;             <Address> <FirstName>Michael</FirstName> <LastName>Korns</LastName> </Address> 
;;
;;           If no content is recognized, an empty string literal is returned.
;;           If singleton content is recognized, a string literal is returned;
;;           otherwise, a Structure of the attributed contents is returned. 
;;           The current input pointer is always left pointing to the first 
;;           non whitespace character which is not element content.
;; 
;; Args:     document      A Structure to be extended as an XML document
;;                         in tree model form. After being extended this 
;;                         structure is then returned as the result.
;;            
;;           tagDepth      The current xml tag recursion level.
;;
;; Return:   result        The extended XML document in tree model form (if present).
;;
;; ******************************************************************************************
	vars: ((Boolean:whitespace false) 
			(Boolean:compareChar false)
            tmp 
            result                    
            name 		 
			theContents
            (ByteVector:stemWord)
            ename 
            value       
			tempCharTemp
            tempName 
			(tempLen 1024)
           ) ;; end of temporary variables
    regs: ( 
           	(CharPointer:startp)					;; Temporary input string pointer
           	(CharPointer:tempCharp)					;; Pointer to the tempBuf buffer
           	(CharPointer:tIP)
           	(CharPointer:stemPointer)
           
           	(Integer:test) 				      		;; Temporary string compare switch
           	(Integer:cc)                         	;; Temporary input character
           	(Integer:c1)                         	;; Temporary input character
           	(Integer:c2)
			(Integer:c3)
			(Integer:c4)
			(Integer:c5)
			(Integer:c6) 
           (Integer:saveCH)                      
           (Integer:len1)                     	 	;; Input string length
           (Integer:len 0)
           (Integer:n 0) 
           (Integer:n1 0)
           (Integer:null 0)
       ) ;; end of register variables 
    
      
   	(if (and (= stemmingOn true)(= stemVector #void)) (setq stemVector (new Vector:)))
 
    
    (if (<> tDocument #void) (setq result tDocument))
	
   
    (if  (= result #void) 
         (begin 
         	(setq result "")
         	(setq whitespace true)          
         );; end begin
         else
         	(begin (setq result tDocument))
     ) ;; end if
 
    Fetch::
 
    (vmregLoadInteger IP startp)
	(vmregRefCharacter startp cc)
  
    (if (or (= cc #void) (= cc 0))(goto Zero:))
    (if (= cc #\<) (goto LessThan:))
    (goto Default:)
    
    ;; ===================================================
    ;; Input Character is 0
    ;; ===================================================
    Zero::   
	(if (= inputProvider true)
        (begin
		  ;; Send the moreSource message symbol to the inputLambda.
		  ;; Note: we do this only if we are in event handler mode.
		  (setq tmp (inputLambda.moreSource))
		  (if (<> tmp #void)
			  (begin
				 (setq inputString tmp)
                 (setq INLEN (length tmp))
                 (setq startp tmp) 
				 (setq past 0)
				 (setq start startp)
				 (setq IP startp)
			     (goto Fetch:)
			  )) ; end if
 		)) ; end if

	(setq IP startp)  
  
	(goto Done:)

    ;; ===================================================
    ;; Input Character is <
    ;; ===================================================
    LessThan::
 
	;; ******************************************** 
	;; Check for an end of element content tag. 
	;; ********************************************
    ;; Note: </Name> are end of element content tags.
    (if (= (setq cc startp[1]) #\/)
		;; We have found an end of element content tag.
        (begin    
		  ;; Try to recognize the element end tag name.
		  	(setq IP (+= startp 2))  ;; Promote input pointer past the </ symbols.
 
			(setq ename (recName))
 
          	(vmregLoadInteger IP startp)
        
            ;; checking if end of tag matches the beginning tag
            (if (compareEQ ename elementName) 
                (begin       
                     (vmregRefCharacter startp cc)                      
                     (if (<> cc #\>) (if (= errorCheckOn true)(xmlError "xml: expected element end tag > symbol"  )))                                   
                      ;; Promote pointer past the whitespace
                     (vmregRunInHardware start:)
                     (vmregLoadInteger IP startp)
					 (vmregAddImmediate 1 startp)
					 (vmregRefCharacter startp cc)                                             
                     (while (and (> cc 0) (<= cc 32)) (begin (vmregAddImmediate 1 startp) (setq cc startp[0])))
                     (vmregRunInHardware stop:)                  
                     (setq IP startp)                     
                     (setq elementName #void)                                      
                     (goto Done:)
                ) ;; end begin
            ) ;; end if

		  (if (= ename #void) 
               (if (= errorCheckOn true)
                   (begin                        
                      (xmlError "xml: Expected element end tag name"  )
                    ) ;; end begin
                ) ;; end if  
           ) ;; end if

			(vmregLoadInteger IP startp)
            
		  ;; An HTML end name sets the HTML processing flag to true.
          (if (<> ename #void)
              (begin
                  (vmregStringiCompare ename "html" test)
		          (if (= test 0) (setq htmlOn true))
               ) ;; end begin
          ) ;; end if 

		  ;; Promote input pointer past whitespace
          (vmregRunInHardware start:)
          (setq cc startp[0])
          (while (and (> cc 0) (<= cc 32)) (begin (vmregAddImmediate 1 startp) (vmregRefCharacter startp cc)))
          (vmregRunInHardware stop:)           
		  (if (<> cc #\>) (if (= errorCheckOn true)(xmlError "xml: expected element end tag > symbol"  )))
		  (setq IP  (vmregAddImmediate 1 startp)) ;; Promote input pointer past the > symbol.
                  
          ;; Return to the calling recursion          
          (if (> tagDepth 0) (goto Done:))
		  (goto Fetch:)
		)) ;; end if
 
	;;**********************************************************
	;; We are now recognizing imbedded content, so we convert	
	;; the parent contents into a Structure and turn off the	
	;; whitespace flag to indicate that we have important data. 
	;;**********************************************************
  
    ;; Note: <Name ..attlist..> ...contents... </Name> are imbedded element tags.
    ;;       <Name ..attlist../> are imbedded element tags without contents.
    ;;       <?Name ...contents... ?> are imbedded process instruction tags.
 
		(if (not (isStructure result)) 
			(begin
				;; Save the current contents
				(setq theContents result)
            	;; for the  void contents bug with another html tag
            
				;; Make sure the contents are converted to a Structure.
		        (setq result (|Gv:new| Structure:))
             
             
			;; Make sure that any non-whitespace contents are saved in the Structure.
			;; Note: the whitespace flag will tell us whether we have seen important data.
			(if (= whitespace 0)
				(if (= eventHandler true)
                 (begin              
					;; Note: we do this only if we are in eventHandler mode.
	       			 (eventLambda.characters theContents) 
                  ) ;; end begin
				else
                 (begin
					;; Note: we do this only if we are in document mode.             
                    (setq tmp (symbol "__content"))        
					(set result last: tmp theContents)      
				) ;; end begin			
			) ;; end if work event handler
		   ) ;; end if whitespace is false
       ) ;; end begin is not structure
    ) ;; end if is not structure
 
	;;************************************
	;; Check for a start of a comment.   
       ;; Note: <!--  --> are comment tags. 
	;;************************************
  
       (if (and (= (setq cc startp[1]) #\!) (= (setq cc startp[2]) #\-) (= (setq cc startp[3]) #\-))
		    ;; We recognize and process the comment.
		    (begin
                 
		        (if (= inputData false)
                   (begin
                       (setq result (recComments result))                       
		               (goto Fetch:)
		            ) ;; end begin
                    else
                   (begin
                         ;; Go through the comment text
                         (vmregRunInHardware start:)
                         (while (and (<> (setq cc startp[0]) 0) (<> cc #\>)) do (vmregAddImmediate 1 startp))
                         (vmregRunInHardware stop:) 
                         (setq IP (vmregAddImmediate 1 startp))
                         (goto Fetch:)
                    ) ;; end begin for else
                ) ;; end if for inputData
             ) ;; end for begin
        ) ;; end for if

	;;*****************************************************************
	;; We recognize marked sections here.                             
	;; Note: <![CDATA[ ... ]]> is an example of a marked section.     
	;;       Marked sections may occur anywhere in contents sections. 
	;;*****************************************************************

	(if (and 
             (= (setq cc startp[1]) #\!)
             (= (setq cc startp[2]) #\[)
             (= (setq cc startp[3]) #\C)
             (= (setq cc startp[4]) #\D)
             (= (setq cc startp[5]) #\A)
             (= (setq cc startp[6]) #\T)
             (= (setq cc startp[7]) #\A)
             (= (setq cc startp[8]) #\[)
            )
		(begin
           (setq whitespace false)
		   (setq result (recCData result))    
		   (goto Fetch:)
		) ;; end begin
    ) ;; end if

	;;************************************************************************ 
	;; Check for an imbedded process instruction tags.                        
       ;; Note: <?Name ...contents... ?> are imbedded process instruction tags.  
	;;************************************************************************

     (if (= (setq cc startp[1]) #\?)
		;; We have found an imbedded process instruction.
		(begin
            (if (= inputData false)
             (begin
                   (setq result (recProcessInstruction theContents result ))
                   (goto Fetch:)
                ) ;; end begin
                else
                   (begin
 						(vmregRefCharacter cc startp)
                    	(setq c1 startp[1])
                       	(vmregRunInHardware start:)
                    	(while   (and   (<> (setq startp[0] cc) 63) (<> (setq c1 startp[1]) 62)  )    (++ startp)     )
                  		(vmregRunInHardware stop:) 
                   		(setq IP (+= startp 2))
                   		(goto Fetch:)
                ) ;; end begin
           ) ;; end if for inputData
        ) ;; end begin
     ) ;; end if

	;;************************************************************************ 
	;; Check for an imbedded document type definition tag.                        
       ;; Note: <!Name ...contents... > are document type definition tags. 
	;;************************************************************************

     (if (= (setq cc startp[1]) #\!)
		 ;; We have found an document type definition.
		(begin
              (setq result (recDoctypeHeader result  tagDepth))
              (goto Fetch:)
         ) ;; end begin
      ) ;; end if

	;;************************************************************************ 
	;; Check for a SCRIPT tag which has meaning in HTML mode.                        
       ;; Note: <script ...contents... <script> are HTML script tags. 
	;;************************************************************************
      (if (and (= htmlOn true)
                (or (= (setq cc startp[1]) #\S) (= cc #\s))
                (or (= (setq cc startp[2]) #\C) (= cc #\c))
                (or (= (setq cc startp[3]) #\R) (= cc #\r))
                (or (= (setq cc startp[4]) #\I) (= cc #\i))
                (or (= (setq cc startp[5]) #\P) (= cc #\p))
                (or (= (setq cc startp[6]) #\T) (= cc #\t))
           )
		;; We have found an HTML script tag.
		    (begin                 
		        (setq result (recScriptInstruction result))
		        (goto Fetch:)
		    ) ;; end begin
        ) ;; end if

      

	;;************************************************************************ 
	;; We are now processing an element tag.                        
       ;; Note: <Name ..attlist..> ...contents... </Name> are element tags. 
       ;;       <Name ..attlist../> are element tags without contents. 
	;;************************************************************************

    (setq result (recElement theContents result tagDepth))
    (goto Fetch:)


    ;; ===================================================
    ;; Input Character is default
    ;; ===================================================
    Default::
 
   	(vmregLoadInteger IP startp)
    (setq tIP startp)
    (setq whitespace false)  
   
	;; Note: Bug for long contents
	;; Bug Description: Cannot copy characters to String "tempCharTemp" 
 
	(setq tempCharTemp tempChar)
	(setq tempCharp tempCharTemp)
	(setq tempCharp[0] 0)
    ;; check if there are character contents.. if there are do not promote the whitespace and evaluate
    ;; the character contents
    (vmregRunInHardware start:)
    (setq n 0)
     
    (while (and (<> (vmregRefCharacter startp cc) 60) (<> cc 0))
			(vmregMoveInteger cc saveCH)
			(if (= n tempLen)
				(begin
			 		(+= tempLen 1024)
             		(resize tempCharTemp tempLen)
             		(setq tempCharp tempCharTemp)
				) ;; end begin
	 		);; end if
  		 	(vmregSetXCharacter saveCH n tempCharp  )
 			(vmregAddImmediate    1  n  )
 		  	(vmregSetXCharacter  null  n   tempCharp  )
         	(setq c1 startp[1])
			(vmregRunInHardware stop:)
         	;; for doctypeheaders, once trailer is found, recContents terminates and
         	;; goes back to recDoctypeheader
         	;;(writeln "recContents: in doctypeheader: cc: " cc " c1: " c1 " doctypeFlag: " doctypeFlag )
         	(if (and (= cc #\]) (= c1 62) (= doctypeFlag true))
            	(begin
                	(setq result tempCharTemp)
                	(setq  IP (vmregAddImmediate 1 startp)  )
                	(goto Done:)
            	 ) ;; end begin
          	) ;; end if   
          	(vmregAddImmediate 1 startp)      
      ) ;; end while
      
      ;(resize tempChar  n) 
   
      (if (isCharWhitespace tempCharTemp)
          (begin
            (setq whitespace true)
            (setq  IP (-- startp))
          ) ;; end begin
         else
         (begin
            (setq whitespace false)            
         ) ;; end begin
      ) ;; end if
 
 
	;; Make sure that any non-whitespace contents are saved in the Structure. 
	;; Note: the whitespace flag will tell us whether we have seen important data.  
	(if (= whitespace 0)
        (begin
		;; Manage the characters event according to the mode we're in.  
        ;; This code is transferred here to get the contents first to save in the Structure later
        ;; Note: we do this only if we are in document mode.  
	    ;; Convert the recognized char data into a string.  	
			(setq theContents (makeString tempCharTemp))
 
			;; For stemming option
            (if (= stemmingOn true)
             	(begin
                	(setq stemPointer theContents)                            
               		(while (> (setq cc stemPointer[0]) 0) do
                    	(setq stemVector nn cc)
                    	(setq stemVector (++ nn) 0)
                    	(++ stemPointer) 
                     ) ;; end while
                    (setq stemVector nn 32)
                    (++ nn)
                 ) ;; end begin
             ) ;; end if
               
              (if (=  eventHandler true)
			      (begin
			           ;; Send the characters message symbol to the eventLambda.  
			           ;; Note: we do this only if we are in event handler mode.   
			           ;; Convert the recognized char data into a string. 
                       (if (> (- startp IP)0)
				            (begin
				                (eventLambda.characters theContents) 
                             ) ;; end begin
                        ) ;; end if
                    ) ;; end begin for work.eventHandler
        	       else
			       (begin
			            (setq IP startp) 
                     
			           	(if (= #void result)(begin (setq result theContents)))
                          
                         (if (= (isStructure result) false) (begin (setq result theContents)  ))
				
                        ;; If the old content is a null string, then just set the new content.
                        ;; possible debug for tempchar result to document
			            (if (and (isString result) (= result "") )(begin (setq result theContents) ))
				
                         
                        ;; If the old content is a Structure, then insert the new content.  
		                ;; Note: If the last item inserted was character content,  
		                ;;  then append this content to it.  
                        (if (isStructure result)
                            (begin                                                        
                                 (setq len1 (length result))
                                 (if (> len1 0)
                                     (begin
                                          (setq name (ref result (sub1 len1) 0))                                         
                                          (setq value (ref result (sub1 len1) 1))
                                         
                                          (if (and (isSymbol name) (compareEQ name "__content")
                                              (or (isString value) (isText value) (isByteVector value)))
                                              (begin
                                                   
                                                  	(setq theContents  (append (string value) (string theContents)))
                                                 	(setq tmp (symbol "__content"))
                                                	(delete result (sub1 len1))
                                                   	(set result last:  name theContents)  
                                                	(goto Fetch:)
                                               ) ;; end begin
                                           ) ;; end if
                                           (setq tmp (symbol "__content"))                
                                           (set result last:  tmp theContents)   
                                    ) ;; end begin
                                    ;;else
                                    (begin
                                     	(setq tmp (symbol "__content"))
                                     	(set result last: tmp theContents)                           
                                   	) ;; end begin
                                  ) ;; end if for len1
                              );; end begin
                        ) ;; end if isStructure
                   ) ;; end begin for else eventHandler
               ) ;; end if for eventHandler
           ) ;; end for begin for whitespace = false                    
           ;; Manage the ignorable whitespace event according to the mode we're in.  
           else
           ;; Here whitespace is TRUE
           (begin
               ;; Send the ignorableWhitespace message symbol to the eventLambda.  
		       ;; Note: we do this only if we are in event handler mode.  
		       (if (= true eventHandler)
                   (begin
                       (if (> (- startp IP) 0)
                          (begin                             
                              (eventLambda.ignorableWhitespace tempCharTemp) 
                           ) ;; end begin
                        ) ;; end if
                    ) ;; end begin
                ) ;; end if
                (++ startp)
            ) ;; end begin for else    
        );; end for whitespace is false

  
     (setq IP startp)
     (goto Fetch:)
 
    ;; ===================================================
    ;; We have an input error
    ;; ===================================================
    Bad::
    (if (= errorCheckOn true)(xmlError "xml: invalid element contents"  ))

    ;; ===================================================
    ;; We have no more input
    ;; ===================================================
 
    Done::
    
    (setq Document result)
 
 
result) ;; end recContents


































;;**EXPORTKEY**:xml:recDoctypeHeader
(defchild xml:recDoctypeHeader(result Integer:tagDepth)
;; ******************************************************************************************
;;  Summary: This Function recognizes XML Doctype Headers.
;;           For example:
;;
;;	<!DOCTYPE HTML "version 2345">
;;
;;		also
;;
;;	<![if !supportEmptyParas]>
;;
;;
;;		also
;;
;;	<![endif]>
;;
;;
;;
;;
;; ******************************************************************************************
regs: ((CharPointer:startp)                  ;; Temporary variable to store the position of the pointer
       (CharPointer:tIP)                     ;; Temporary variable fore saving comment characters
       (Integer:cc)                         ;; Temporary input character
       (Integer:c1)
       (Integer:n 0)  
       (Integer:saveCH)                     ;; Temporary input character to save
       (Integer:len 0)
       (nill 0) (lessThan 60) (greaterThan 62) (_! 33) (leftbracket 91) (rightbracket 93)(space 32)
      )
vars:(name
      result 
      testResult
      dtdContent
      contents 
      ename
      dtd 
      script 
      tmp
      (ByteVector:doctypeHeader)
)   
 

;; Check for an processing instruction tag.  
;; Note: <!Name ...contents... > is a doctype header tag.  
(if (= traceLinesOn true)(writeln "---recDoctypeHeader START---")) ;; LTN


(if (= inputData true) (setq testResult (copy result)))


(if (= traceLinesOn true)(writeln "recDoctypeHeader: arguments passed: result: " result)) ;; LTN 
;;(setq startp work.IP)
(vmregLoadInteger IP startp)
(if (= traceLinesOn true)(writeln "recDoctypeHeader: argument passed: IP and startp is:  " startp[0])) ;; LTN 
(setq doctypeHeader (new Vector: Byte: 5000))

(setq cc startp[0])
(setq c1 startp[1])

(if (and (= cc lessThan)   (= c1 _!))
    (begin
	    ;; We have found a doctype header. 
	    ;; Promote the input pointer to the doctype tag name.  
	    (setq startp (+ startp 2))
        (setq IP startp)
	    (if (= traceLinesOn true)(writeln "recDoctypeHeader: after doctype header: " (char startp[0]))) ;; LTN 
	    ;; Try to recognize the doctype tag name.  
        (setq name (recName))
        (if (= name #void) (setq name "dtd:unnamed"))
        (if (= traceLinesOn true)(writeln "recDoctypeHeader:  name is: " name)) ;; LTN
        ;;(setq startp work.IP)
        (vmregLoadInteger IP startp)
        (if (= traceLinesOn true)(writeln "recDoctypeHeader: after recName: start gets  IP " (char startp[0])(char startp[1]) (char startp[2]))) ;; LTN 
 
	        
	    ;; Brick the doctype contents. 
        (vmregRunInHardware start:) 	      
        (while (and (<> (setq cc startp[0]) 0) (<> cc greaterThan) (<> cc #\[)  ) do
	          ;; ip variable saves the characters until the end of comment
              (setq tIP startp)
              (setq saveCH tIP[0])
              ;;(writeln "recDoctypeHeader: saveCH in while is: " saveCH) ;; LTN
              (setq doctypeHeader n saveCH)
              (setq doctypeHeader (++ n) 0)
              ;;(setq doctypeHeader (append doctypeHeader (char saveCH)))
              ;; start variable ignores comment characters.             
              (++ startp)                
       ) ;; end while
      (vmregRunInHardware stop:)

      (resize doctypeHeader (++ n))
      (if (= traceLinesOn true)(writeln "recDoctypeHeader: after PI contents: start: " (char startp[0]))) ;; LTN 
      (if (= traceLinesOn true)(writeln "recDoctypeHeader:  doctypeHeader is: " doctypeHeader)) ;; LTN
    
      ;; Manage the doctype according to the mode we're in.
	  (if (= eventHandler true)
        (begin		 
		;;  Send the processingInstruction message symbol to the eventLambda.  
		;;  Note: we do this only if we are in event handler mode.		 
        (eventLambda.doctypeDefinition name doctypeHeader)
		) ;; end begin
	    else
		(begin
		;;  Locate the __dtd Structure (if not present, create one)  
		;; Note: the imbedded DTD will be assigned in the last  
		;;       position of the __dtd Structure.  
		;; Note: we do this only if we are in document mode.  
		(setq dtd (symbol "__dtd"))      
		(setq dtdContent (ref result dtd))
        (if (= traceLinesOn true)(writeln "recDoctypeHeader: after ref function result is: " result)) ;; LTN
        (if (= traceLinesOn true)(writeln "recDoctypeHeader: dtdContent is:  " dtdContent)) ;; LTN

		(if (= dtdContent #void)
			(begin
              (setq dtdContent (new Structure:))
			  (set result dtd dtdContent)
			 ) ;; end begin
        ) ;; end if

        (setq dtdContent last: name doctypeHeader)
        (if (= traceLinesOn true)(writeln "recDoctypeHeader: after setting result is: " result)) ;; LTN
        ) ;; end begin for else
    )  ;; end if

 
   ;; Promote past whitespace
    (vmregRunInHardware start:)
    ;;(++ start)
    (setq cc startp[0])                      
    (while (and (> cc nill) (<= cc space)) (begin (++ startp) (setq cc startp[0])))
    (vmregRunInHardware stop:) 
    (setq IP startp)
    (if (= traceLinesOn true)(writeln "recDocytypeHeader: after promote past whitespace: " (char startp[0])));; LTN
    

	;; Is the document definition nested?  	
	(if (= (setq cc startp[0])leftbracket)
		(begin 
           (if (= traceLinesOn true)(writeln "recDoctypeHeader: inside nested doctype header ")) ;; LTN
           (vmregRunInHardware start:)
		   (++ startp)
           (setq cc startp[0])                      
           (while (and (> cc nill) (<= cc space)) (begin (++ startp) (setq cc startp[0])))
           (vmregRunInHardware stop:) 
           (setq IP startp)           
           (if (= traceLinesOn true)(writeln "recDoctypeHeader: nested doctype: after promoting whitespace is: " (char startp[0]))) ;; LTN
           ;; testing --- promoting past whitespace
		   (while (and (<> (setq cc startp[0] ) nill) (<> cc rightbracket)) do              
           ;;   (setq saveCH start[0])
           ;;   (writeln "recDoctypeHeader: saveCH1 in while is: " saveCH) ;; LTN
           ;;   (setq doctypeHeader (append doctypeHeader (char saveCH)))
              ;; start variable ignores comment characters.             
              (++ startp)                
           ) ;; end while
          
           ;; Convert the recognized char data into a nested document type definition. 
           ;;(writeln "recDoctypeHeader: calling recContents ") ;; LTN 
           ;;(writeln "recDoctypeHeader: dtdContent before recContents: " dtdContent) ;; LTN 
           (setq doctypeFlag true)
           (setq dtdContent (recContents result (+ tagDepth 1)))
           (if (= traceLinesOn true)(writeln "recDoctypeHeader: after calling recContents:  " dtdContent)) ;; LTN 
           ;;(setq startp work.IP)
           (vmregLoadInteger IP startp)
           (setq cc startp[0])
            
           (if (= traceLinesOn true)(writeln "recDoctypeHeader: start=cc=IP is: "  cc)) ;; LTN
		   (if (<> cc #\>) (if (= errorCheckOn true)(xmlError "xml: Document definition not closed" )))
           ;;(++ start)
		
	       ;;(while (and (<> (setq cc start[0]) #\0)  (<> cc #\>) ) do
			;;  (++ start)
		   ;; )
            (if (= traceLinesOn true)(writeln "recDoctypeHeader: in nested DD startp[0] is: " startp[0])) ;; LTN
       ) ;; end begin
    ) ;; end if

  
	;; Is the document definition not closed?  	
	(if (<> (setq cc startp[0]) greaterThan) (if (= errorCheckOn true)(xmlError "xml: Document definition not closed" )))
	;; Promote the input pointer past the doctype trailer. 
    (++ startp)          
    (vmregRunInHardware start:)
    (setq cc startp[0])
    (while (and (> cc nill) (<= cc space)) (begin (++ startp) (setq cc startp[0])))
    (vmregRunInHardware stop:)
    (setq IP startp)
   
    (if (= inputData true) (return testResult))
    (return result)
   
   ) ;; end begin
) ;; end if


;; Return an XML invalid processing instruction error.  

Error:;
(if (= errorCheckOn true)(xmlError "xml: Expecting document type definition"  ))


#void) ;; end recDoctypeHeader


































;;**EXPORTKEY**:xml:recElement
(defchild xml:recElement(contents tDocument  Integer:tagDepth)
;; ******************************************************************************************
;;  Summary: This Function recognizes the contents of XML elements.  
;;
 
;;
;;             <Address> <FirstName>Michael</FirstName> <LastName>Korns</LastName> </Address> 
;;
;;           If no content is recognized, an empty string literal is returned.
;;           If singleton content is recognized, a string literal is returned;
;;           otherwise, a Structure of the attributed contents is returned. 
;;           The current input pointer is always left pointing to the first 
;;           non whitespace character which is not element content.
;; 
;; Args:     contents      A Structure to be extended as an XML document
;;                         in tree model form. After being extended this 
;;                         structure is then returned as the result.
;;           document      A Structure to be extended as an XML document
;;                         in tree model form. After being extended this 
;;                         structure is then returned as the result.
;;           work          The compiler work space which allows the xml
;;                         compiler to be invoked recursively.
;;           tagDepth      The current xml tag recursion level.
;;
;; Return:   tDocument        The extended XML document in tree model form (if present).
;;
;; ******************************************************************************************
regs: (	(CharPointer:tIP)				;; Temporary input string pointer
       	(CharPointer:startp) 
	    (CharPointer:enamep)			;; Pointer to stringBuf
		(CharPointer:tempCharp)			;; Pointer to tempChar
       	(Integer:saveCH) 
       	(Integer:n 0) 					;; Counter
       	(Integer:cc 0)
       	(Integer:c1 0)
		(Integer:start0 0) (Integer:start1 0)  
      	(Integer:comp)
		(Integer:m)
		(Integer:lastChar)
       	(space 32) (nill 0) (greaterThan 62) (lesserThan 60)(forwardSlash 47))
vars:(name       
      attributes 
      tempCharTemp
	 (tempLen 1024)
	  (Boolean:compareChar false)
)

 


(++ IP)
 
(vmregLoadInteger IP startp)
 
;; Inlining recName
(setq name stringBuf)
(setq enamep name)
(setq compareChar false)
(setq n 0)
(vmregLoadInteger IP tIP)

(setq cc tIP[0])
(setq c1 tIP[1])
(setq compareChar (or (and (>= cc #\a) (<= cc #\z)) (and (>= cc #\A) (<= cc #\Z)) 
		(and (>= cc 49) (<= cc 57)) (= cc #\_) (= cc 58) 
		(= cc 45) (= cc 35) (= cc 46) (= cc 48)(and (= cc 47) (<> c1 62))))
(while  (= compareChar true)do	      
   		(vmregMoveInteger cc saveCH)
 		(vmregSetXCharacter saveCH n enamep)
 		(vmregAddImmediate 1 n)
		(vmregAddImmediate 1 tIP)
		(vmregRefCharacter tIP cc)
		(setq compareChar (or (and (>= cc #\a) (<= cc #\z)) (and (>= cc #\A) (<= cc #\Z)) 
			(and (>= cc 49) (<= cc 57)) (= cc #\_) (= cc 58) 
			(= cc 45) (= cc 35) (= cc 46) (= cc 48)(and (= cc 47) (<> c1 62))))
) ;; end while
(setq m (- n 1))
 
(setq lastChar enamep[m])
 
(setq IP tIP)
(if (<> lastChar 47)
	(begin 
		(resize name n)
		(setq name (symbol name))
		(setq elementName name)
	) 
	else 
	(begin 
		(resize name m)
		(-- IP)
	)
)    

;; End inlining recName
 
 
(if (isEqual name #void)  (if (= errorCheckOn true)(xmlError "xml: Expected element tag name"  )))
  
(if (<> name #void)
    (begin
        (vmregStringiCompare name "html"  comp)
        (if (= comp 0) then (setq htmlOn true))
    ) ;; end begin
) ;; end if

  
;;Promote input pointer past whitespace
(setq tIP (Integer: IP))
(setq cc tIP[0])  
 
(vmregRunInHardware start:)
(while (and  (<= cc space) (> cc nill)) do (++ tIP)(setq cc tIP[0]))(setq IP tIP)
(vmregRunInHardware stop:)
 

(setq cc tIP[0])
(setq c1 tIP[1])

;; Recognize attributes if any
(if (and (<> cc 47) (<> cc 62))
   (begin
    	(setq attributes (recAttributeList))   
   ) ;; end begin   
) ;; end if
 
;;Manage the start element event according to the mode we're in. 
(if (= eventHandler true)
	(begin
	;;Send the startElement message symbol to the eventLambda. 
	;;Note: we do this only if we are in event handler mode.
    (eventLambda.startElement name attributes)  
	) ;; end begin
    else
	(begin
    ;; Assign any attributes to the element Structure under the key: __attlist 
	;; Note: the attributes will be assigned in the first position of the Structure. 
	;; Note: we do this only if we are in document mode. 
	    (if (<> attributes #void)
            (begin
               (setq contents (new Structure: __attlist: attributes))  
             ) ;; end begin
        );; end if
     );; end begin
);; end if

 
;; Are there no contents for this imbedded element tag? 
;; Note: <Name ..attlist../> are imbedded element tags without contents. 
(setq tIP (Integer: IP))
(setq cc tIP[0]) 
(setq c1 tIP[1])      
 
 
(if (and (= cc forwardSlash)  (= c1 greaterThan)  )
  	;; There are no contents for this element tag. 
    (begin
         (setq tIP (+ tIP 2))
         (setq IP tIP)
          
         (if (= eventHandler true) 
             (begin
                 (eventLambda.endElement name)                  
              ) ;; end begin
              ;; work.eventHandler is false  
              else
              (begin
               ;; Note: the imbedded element will be assigned in the last position of the Structure. */
		       ;; Note: we do this only if we are in document mode. */
               ;; fix for the empty element bug
                
               (if (or (= attributes #void) (= attributes "")) 
                   (begin                                  
                      	(set tDocument last: name true)
                   ) ;; end begin
                    else
                   (begin
                       	(set tDocument last: name contents)
                    ) ;; end begin
                ) ;; end if
                
               (goto last:)
               );; end begin
        ) ;; end if
    ) ;; end begin
);; end if             

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; for XML tags with contents
;; Note: <Name .. attlist> content here </Name> are imbedded element tags with contents. 
;; The contents may be text or other embedded HTML tags
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(if (<> cc greaterThan) 
    (begin        
        (if (= errorCheckOn true)(xmlError "xml: Expected end element tag"  ))
     ) ;; end begin
) ;; end if
 
(++ IP)
 
;; Note: Bug for long contents
;; Bug Description: Cannot copy characters to String "tempCharTemp"
 
(setq tempCharTemp tempChar)
(setq tempCharp tempCharTemp)
(setq tempCharp[0] 0)
 
(vmregLoadInteger IP startp)
(setq tIP startp)
(setq cc startp[0])
(setq n 0)
 
(while (and (> (vmregRefCharacter startp cc) 0) (<> cc #\<)  (<> (isNegative cc) true)) do        
 	 (if (= n tempLen)
		(begin
			 (+= tempLen 1024)
             (resize tempCharTemp tempLen)
             (setq tempCharp tempCharTemp)
		) ;; end begin
	 );; end if
	 (vmregMoveInteger cc saveCH)	  
     (vmregSetXCharacter saveCH n tempCharp)
 	 (vmregAddImmediate    1  n  )
 	 (vmregSetXCharacter  nill  n   tempCharp  )
 	 (vmregAddImmediate 1 startp)  
) ;; end while
   

;(resize tempCharTemp n)
 
 
(if (isCharWhitespace tempCharTemp)
    (begin
        (setq IP startp)
     ) ;; end begin
     else
     (begin
       
        (setq IP tIP)
        (if  (= (isStructure contents) false) 
			(begin
        		(setq contents tempCharTemp) 
			)
		) ;; end if
     ) ;; end begin
) ;; end if

      
(setq contents (recContents contents  (+ 1 tagDepth)))
 
 

(if (= eventHandler false)
     (begin     
        	(set tDocument last: name contents) 
 			(goto last:)
      ) ;; end begin
 ) ;; end if
 
;; recognize name of end of element
 
(vmregLoadInteger IP startp)

(setq start0 startp[0])
(setq start1 startp[1]) 
  
(if (and (= start0  lesserThan)  (= start1 forwardSlash))
  	;; There are no contents for this element tag. 
    (begin
    
         (+= IP 2)
         (setq name (recName))
        
         (if (= name #void) then (if (= errorCheckOn true)(xmlError "xml: Expected element tag name"  )))

         ;; An HTML end name never resets the HTML processing flag.  
         ;; Note: This is because there are so many errors in web  
	     ;;       HTML pages that we can never be sure that we are  
	     ;;      not required to parse loosely once we have seen  
	     ;;       the HTML start tag. */         
         (vmregStringiCompare name "html"  comp)
         (if (= comp 0) then (setq htmlOn true))
         ;;Promote input pointer past whitespace
         (vmregRunInHardware start:)
         (setq tIP (Integer: IP))
         (setq cc tIP[0])
         (if (<> cc 62) (if (= errorCheckOn true)(xmlError "xml: Expected element end tag > symbol"  )))
         (setq cc tIP[0])(while (and  (<= cc space) (> cc nill)) do (++ tIP)(setq cc tIP[0]))(setq IP tIP)
         (++ IP)
        (vmregRunInHardware stop:)        
      ) ;; end begin
);; end if

 

last:: 
 
tDocument) ;; end recElement
 


































;;**EXPORTKEY**:xml:recName
(defchild xml:recName()
;; ******************************************************************************************
;;  Summary: This Function recognizes the contents of XML elements. These 
;;           can be either normal elements or process instruction elements 
;;           For example:
;;
;;             <?javaScript writeln("Hello World!"); ?>
;;
;;          				-or-
;;
;;             <Address> <FirstName>Michael</FirstName> <LastName>Korns</LastName> </Address> 
;;
;;           If no content is recognized, an empty string literal is returned.
;;           If singleton content is recognized, a string literal is returned;
;;           otherwise, a Structure of the attributed contents is returned. 
;;           The current input pointer is always left pointing to the first 
;;           non whitespace character which is not element content.
;; 
;; Args:     work          The compiler work space which allows the xml
;;                         compiler to be invoked recursively.
;;
;; Return:   result        The extended XML document in tree model form (if present).
;;
;; ******************************************************************************************
regs:(CharPointer:tIP cc c1 (len 0) CharPointer:stringBufp (Integer:null 0))
regs:((_a #\a) (_z #\z) (_A #\A) (_Z #\Z) (underScore #\_) (space 32) (colon 58) (dash 45) (one 49) (nine 57) (period 46) (greaterThan 62))
vars:(String:tStringBuf result)
 
(if (= traceLinesOn true)(writeln "---recName START---")) ;; LTN

;; Initialize a String object with enough space for a 1024 character name
(setq tStringBuf stringBuf)
(setq stringBufp tStringBuf)
(setq stringBufp[0] null)

;; An XML name must begine with an underscore _ or an alpha character
;; LTN
;;(setq tIP (Integer: IP))
(vmregLoadInteger IP tIP)

(setq cc tIP[0])
(setq c1 tIP[1])
 

  
 
(if (or (and (>= cc _a) (<= cc _z)) (and (>= cc _A) (<= cc _Z)) (and (>= cc one) (<= cc nine)) (= cc underScore) (= cc colon) (= cc dash) (= cc 35) (= cc period) (= cc 48)(and (= cc 47) (<> c1 62)))
    (begin (goto StartToRecognize:) )
     else
    (begin (goto Bad:)  )
) ;; end if

StartToRecognize::
  
 
(setq stringBufp[0] cc)
(setq stringBufp[1] null)


;; Recognize names up to 1024 characters

TryAnotherChar::
(++ len)
(++ tIP)

(if (>= len 1020) (goto Bad:))

(setq cc tIP[0])
  
(if (<= cc 32) (goto Good:))

(if (or (and (>= cc _a) (<= cc _z)) (and (>= cc _A) (<= cc _Z)) (and (>= cc one) (<= cc nine)) (= cc underScore) (= cc 48)(= cc 47) (= cc colon) (= cc period) (= cc 35)(= cc dash))    
    (begin
         (setq stringBufp[len] cc)
         (setq stringBufp[(+ len 1)] null)
         (goto TryAnotherChar:)
     )
)

Good::

(setq result (symbol tStringBuf))
 
(setq IP tIP)
(if (= traceLinesOn true)(writeln "---recName END--- result1: "  result)) ;; LTN
(return result)


;; Cannot recognize this as a name

Bad::

;; if there is no error checking we recognize any element name
(if (= errorCheckOn false)
    (begin
        
       (while (<> cc 62) do
         (setq stringBufp[len] cc)          
         (setq stringBufp[(+ len 1)] null)
         (++ tIP)
         (setq cc tIP[0])
         
       ) ;; end while
       (setq result tStringBuf)
       (setq IP tIP)
       (if (= traceLinesOn true)(writeln "---recName END--- result2: "  result)) ;; LTN
       (return result)
    ) ;; end begin
);; end if

;; LTN
;;(setq tIP (Integer: IP))
(vmregLoadInteger IP tIP)
  
#void) ;; end recName

 































;;**EXPORTKEY**:xml:recProcessInstruction
(defchild xml:recProcessInstruction(theContents result)
;; ******************************************************************************************
;;  Summary: This Function recognizes XML processing instructions.
;; For example:
;;
;;             <?javaScript writeln("Hello World!"); ?>
;;
;; ******************************************************************************************
regs: ((CharPointer:namep)
       (CharPointer:startp)                  ;; Temporary variable to store the position of the pointer
       (CharPointer:tIP)                     ;; Temporary variable fore saving comment characters
       (Integer:cc)                         ;; Temporary input character
       (Integer:c1)  
       (Integer:n 0)
       (Integer:saveCH)                     ;; Temporary input character to save
       (Integer:len 0)
       (nill 0) (space 32) (lessThan 60) (greaterThan 62) (_? 63)
      )
vars:(name
      result
      contents pi attributes attlist content attStructure
      (ByteVector:pInstruction) tmp
)  
 
(if (= traceLinesOn true)(writeln "---recProcessInstruction START---")) ;; LTN
(vmregLoadInteger IP startp)
;;(setq startp  IP)
(if (= traceLinesOn true)(writeln "recProcessInstruction: start: " (char startp[0])))
(setq pInstruction (new Vector: Byte: 1000000))

(setq cc startp[0])
(setq c1 startp[1])
 
;; Check for an processing instruction tag.  
;; Note: <?Name ...contents... ?> is a process instruction tag. 
(if (and (= cc lessThan)(= c1 _?))
   (begin
         ;;(writeln "recProcessInstruction: inside if exp is a  PI " ) ;; LTN
	     ;;   Promote the input pointer to the PI tag name.
         (setq startp (+ 2 startp))  
         (setq IP startp)  
         (if (= traceLinesOn true)(writeln "recProcessInstruction: after promotion of PI header: " (char startp[0])));; LTN   
          
         ;; Try to recognize the PI tag name. 
         (setq name (recName))
         (if (= traceLinesOn true)(writeln "recProcessInstructions: name is: " name)) ;; LTN 
         (if (= #void name) (if (= errorCheckOn true)(xmlError "xml: Expecting processing instruction target name"  )))
		 (vmregLoadInteger IP startp) 
         ;; (setq startp work.IP)
         (if (= traceLinesOn true)(writeln "recProcessInstruction: after recognizing name start gets ip " (char startp[0]))) ;; LTN  

          ;; Try to get the attribute list
		  ;; Promote input pointer past whitespace
          (vmregRunInHardware start:)
          (setq cc startp[0])
          ;;(writeln "recProcessInstruction: in Less than end of element tag before while cc is: " cc) ;; LTN      
          (while (and (> cc nill) (<= cc space)) (begin (++ startp) (setq cc startp[0])))
          (vmregRunInHardware stop:)
          (if (= traceLinesOn true)(writeln "recProcessInstruction: after promoting past whitespace " (char startp[0]))) ;; LTN
        
          (vmregRunInHardware start:)        
          (while  (and (<> (setq cc startp[0]) nill) (or   (<> (setq cc startp[0]) _?) (<> (setq c1 startp[1]) greaterThan)  )) do
             ;; Saves the characters
             (setq tIP startp)
             (setq saveCH tIP[0])             
             (setq pInstruction n saveCH)
             (setq pInstruction (++ n) 0)
             ;; Promote the pointer
             (++ startp)            
         ) ;; end while          
         (vmregRunInHardware stop:)
         (resize pInstruction (++ n))
	 
         (if (= traceLinesOn true)(writeln "recPI: " pInstruction)) ;; LTN
         (if (= traceLinesOn true)(writeln "recProcessInstruction: after getting pi contents: " (char startp[0]))) ;; LTN

	     ;; Is the processing instruction not closed? */
	     (if (<> (setq cc startp[0]) _?)
		    (begin
		       (setq IP startp)
               (if (= traceLinesOn true)(writeln "recProcessInstruction: error: pi not closed: " (char startp[0]))) ;; LTN
		       (if (= errorCheckOn true)(xmlError "xml: Processing instruction not closed"  ))
		    );; end begin
        ) ;; end if

	    ;; Promote the input pointer past the PI trailer.
        (if (<> cc nill) 
           (begin 
            ;; (writeln "recProcessInstruction: inside if: cc is: "  cc)
             (setq startp (+ 2 startp))))
             (setq IP startp)
             (if (= traceLinesOn true)(writeln  "recProcessInstruction: after past PI trailer: " (char startp[0]))) ;; LTN

	     ;; Manage the process instruction according to the mode we're in.  
        (if (= true eventHandler)
            (begin
	           ;; Send the processingInstruction message symbol to the eventLambda. 
		       ;; Note: we do this only if we are in event handler mode.                
               (eventLambda.processingInstruction name pInstruction)
		     ) ;; end begin
		    else
		    (begin
		       ;; Locate the __pi Structure (if not present, create one)  
		       ;;  Note: the imbedded PI will be assigned in the last  
		       ;;     position of the __pi Structure.  
		       ;;  Note: we do this only if we are in document mode.  
		       (setq pi (symbol "__pi"))                          
               (setq tmp (ref result pi))
               (if (= traceLinesOn true)(writeln "recProcessInstructions: inside else tmp is: " tmp)) ;; LTN
		       (if (= traceLinesOn true)(writeln "recProcesInstruction: result1: " result)) ;; LTN

		       (if (= #void tmp)
		           (begin 
			           (setq tmp (new Structure:))
                       ;; for PI attlist bug
                       (if (and (<> pInstruction "") (compareEQ attributes #void))
                          (begin
                              ;;(writeln "recProcessInstructions: both PI and attributes are void " ) ;; LTN
                              (setq tmp last: name pInstruction)
                          ) ;; end begin
                       ) ;; end if
                       (if (and (isStructure attributes) (= pInstruction ""))
                           (begin
                              ;;(writeln "recProcessInstructions: PI is empty attributes is structure " ) ;; LTN
                              (setq attlist (symbol "__attlist"))
                              (setq attStructure (new Structure:))
                              (set attStructure last: attlist attributes)
                              (set tmp last: name attStructure)
                           );; end begin
                        ) ;; end if
                       (if (and (isStructure attributes) (<> pInstruction ""))
                           (begin
                              ;;(writeln "recProcessInstructions: PI is not empty attributes is structure " ) ;; LTN
                              (setq attlist (symbol "__attlist"))
                              (setq content (symbol "__content"))
                              (setq attStructure (new Structure:))
                              (set attStructure last: attlist attributes)
                              (set tmp last: name attStructure)
                              (set tmp last: content pInstruction)
                           );; end begin
                        ) ;; end if
                       (if (and (= pInstruction "") (compareEQ attributes #void))
                          (begin
                              ;;(writeln "recProcessInstructions: both PI and attributes are void " ) ;; LTN
                              (setq tmp last: name true)
                          ) ;; end begin
                       ) ;; end if
                       
                      ;; (writeln "recProcessInstructions: new structure tmp is:  " tmp) ;; LTN
                        
			       ) ;; end begin
                   else
                   (begin
                        (setq tmp last: name pInstruction)
                   ) ;; end begin
                ) ;; end if
               
               (setq piStructure (makeStructure name: pInstruction))            
               (set result  pi tmp )
           		 
		       ;;  Are we processing any process instructions?  
               (if (= true piHandler)
			      (begin 
			          ;; Are we processing this process instruction? */
			          (setq piLambda (ref piStructure name)) 			 
			          (if (= #void piLambda)
				          (begin
                              (eval (piLambda piStructure document result theContents))
				           ) ;; end begin
                       ) ;; end if
                   ) ;; end begin
                ) ;; end if
           ) ;; end begin for else			 
          ) ;; end if for eventHandler


	;; Return the XML result object.  
    ;;(writeln "recProcessInstruction --END-- result: " result);; LTN
	(return result)

    ) ;; end begin
 ) ;; end if
	 ;; Return an XML invalid processing instruction error. */

Error::
(if (= errorCheckOn true)(xmlError "xml: Expecting processing instruction" ))

#void)
 

































;;**EXPORTKEY**:xml:recScriptInstruction
(defchild xml:recScriptInstruction(result)
;; ******************************************************************************************
;;  Summary: This Function recognizes HTML scripting instructions.
;; For example:
;;
;;             <script language="javaScript"> writeln("Hello World");</script>
;;
;;          				-or-
;;
;;             <Address> <FirstName>Michael</FirstName> <LastName>Korns</LastName> </Address> 
;;
;;           If no content is recognized, an empty string literal is returned.
;;           If singleton content is recognized, a string literal is returned;
;;           otherwise, a Structure of the attributed contents is returned. 
;;           The current input pointer is always left pointing to the first 
;;           non whitespace character which is not element content.
;; 
;; Args:     document      A Structure to be extended as an XML document
;;                         in tree model form. After being extended this 
;;                         structure is then returned as the result.
;;           work          The compiler work space which allows the xml
;;                         compiler to be invoked recursively.
;;           tagDepth      The current xml tag recursion level.
;;
;; Return:   result        The extended XML document in tree model form (if present).
;;
;; ******************************************************************************************
regs: ((CharPointer:namep)
       (CharPointer:startp)                  ;; Temporary variable to store the position of the pointer
       (CharPointer:tIP)                     ;; Temporary variable fore saving comment characters
       (Integer:cc)                         ;; Temporary input character
       (Integer:c1)  (Integer:c2) (Integer:c3) (Integer:c4) (Integer:c5) (Integer:c6) (Integer:c7)
       (Integer:saveCH)                     ;; Temporary input character to save
       (Integer:len 0)
       (Integer:n 0)
       (nill 0) (_S 83) (_C 67) (_R 82) (_I 73) (_P 80) (_T 84)
       (space 32) (_s 115) (_c 99) (_r 114) (_i 105) (_p 112) (_t 116)
       (greaterThan 62) (lessThan 60) (forwardSlash 47)
      )
vars:(name
         
      contents 
      ename
      attributes 
      testResult
      (ByteVector:script) 
     tmp
)   


;; Save the original variables 

 
    (setq testResult (copy  result  ))
 


 
;; Check for a SCRIPT tag which has meaning in HTML mode.       
;; Note: <script ...contents... <script> are HTML script tags.  

(if (= traceLinesOn true)(writeln "---recScriptInstruction START---") );; LTN 
;;(writeln "recScriptInstruction: type of result is: "  (type result))
;;(setq startp work.IP)
(vmregLoadInteger IP startp)
(if (= traceLinesOn true)(writeln "recScriptInstruction: start: " (char startp[0]))) ;;LTN 
(setq script (new Vector: Byte: 10000))
  
;;(setq cc start[0])
;;(setq c1 start[1])
;;(setq c2 start[2])
;;(setq c3 start[3])
;;(setq c4 start[4])
;;(setq c5 start[5])
;;(setq c6 start[6])
  

  (if (and (= htmlOn true)
                (or (= (setq cc startp[1]) #\S) (= cc #\s))
                (or (= (setq cc startp[2]) #\C) (= cc #\c))
                (or (= (setq cc startp[3]) #\R) (= cc #\r))
                (or (= (setq cc startp[4]) #\I) (= cc #\i))
                (or (= (setq cc startp[5]) #\P) (= cc #\p))
                (or (= (setq cc startp[6]) #\T) (= cc #\t))
           )

    ;;  We have found an HTML script tag. 
	(begin
        ;; Promote the input pointer to the script tag. */
	    (setq startp ( + startp 7))	
        (if (= traceLinesOn true)(writeln "recScriptInstruction: after script header tag: " (char startp[0]))) ;;LTN
        (setq name "script")
	    ;;  Promote input pointer past whitespace     
        (vmregRunInHardware start:)
        (setq cc startp[0])
        (if (= traceLinesOn true)(writeln "recScriptInstruction: in Less than end of element tag before while cc is: " cc)) ;; LTN      
        (while (and (> cc nill) (<= cc space)) (begin (++ startp) (setq cc startp[0])))
        (vmregRunInHardware stop:)
        (if (= traceLinesOn true)(writeln "recScriptInstruction: after promoting past whitespace: " (char startp[0])(char startp[1]) (char startp[2]))) ;;LTN
        ;; Stack(contents) = gCP->Tval_VOID;
        (setq IP startp)
        ;; We need to recognize the script attributes.  
        (setq attributes (recAttributeList))
 
        ;;(setq startp work.IP)
        (vmregLoadInteger IP startp)
	    (if (= traceLinesOn true)(writeln "recScriptInstruction: after recAttributeList " (char startp[0]))) ;;LTN
        (setq cc startp[0])
        ;; Check the end of element
        (if (<> cc greaterThan) (if (= errorCheckOn true)(xmlError "xml: Expecting element tag > symbol")))
	  
        ;; Promote input pointer past the > symbol.  
	    (++ startp)
        (if (= traceLinesOn true)(writeln "recScriptInstruction: after > symbol " (char startp[0]))) ;;LTN
	    ;; Manage the start element event according to the mode we're in.  
        (if (= true eventHandler)
		    (begin
		        ;; Send the startElement message symbol to the eventLambda.  
		        ;; Note: we do this only if we are in event handler mode.  		        
                 (eventLambda.startElement name attributes)
		    ) ;; end begin
		    else
		   (begin
		        ;; Assign any attributes to the element Structure under the key: __attlist  
		        ;; Note: the attributes will be assigned in the first position of the Structure. 
		        ;; Note: we do this only if we are in document mode.  
                (if (<> attributes #void)
			        (begin
			            (setq tmp (symbol "__attlist")) 
                        (setq contents (new Structure: tmp attributes))
                        (if (= traceLinesOn true)(writeln "recScriptInstruction: contents: " contents)) 
			        ) ;; end begin
                ) ;; end if for attributes
           ) ;; end else
        ) ;; end if
 

		;;  Brick the script contents. 
	   ;;(setq startp  IP)
		(vmregLoadInteger IP startp)
       (++ startp)
  
       (vmregRunInHardware start:)
       (while (or  
                (and (<> (setq cc startp[2]) #\S) (<> cc #\s))
                (and (<> (setq cc startp[3]) #\C) (<> cc #\c))
                (and (<> (setq cc startp[4]) #\R) (<> cc #\r))
                (and (<> (setq cc startp[5]) #\I) (<> cc #\i))
                (and (<> (setq cc startp[6]) #\P) (<> cc #\p))
                (and (<> (setq cc startp[7]) #\T) (<> cc #\t))
                ) 

           do
           ;;(writeln "recScriptInstruction before getting script: " (char cc) (char c1) (char c2) (char c3) (char c4) (char c5) (char c6) (char c7))
            (setq tIP startp)
            (setq saveCH tIP[0])
            ;;(setq script (append script (char saveCH)))
            (setq script n saveCH)
            (setq script (++ n) 0)
            ;; Promote the pointer
            (++ startp)
       ) ;; end while
       (vmregRunInHardware stop:)
       (resize script (++ n))
       (if (= traceLinesOn true)(writeln "recScriptInstruction: script: " script)) ;; LTN
       (if (= traceLinesOn true)(writeln "recScriptInstruction: after getting script start is: " (char startp[0]) (char startp[1]) (char startp[2]))) ;; LTN
       ;;  Manage the character data according to the mode we're in. */
 
	  (if (= eventHandler true)
		(begin
		    ;; Send the characters message symbol to the eventLambda.  
		    ;;  Note: we do this only if we are in event handler mode.  		     
            (eventLambda.characters script)
		) ;; end begin
	   else
		(begin
		    ;;  Add the new script to its attributes using the proper heuristic. 
		    ;;  Note: we do this only if we are in document mode.  
		    (if (= contents #void)
                (begin
			        ;;If the old content is empty, then just set the new content. 
                   (setq contents (new Structure:))
			        (setq contents script)
			     ) ;; end begin
             ) ;; end if
			 
		   (if (= contents "")
              (begin
		          ;; If the old content is a null string, then just set the new comment.
                  (setq contents (new Structure:))  
			      (setq contents script)
			  ) ;;end begin
              else
              (begin
		        (if (not (isStructure contents))
                    (begin
			          ;;  If the old content is cdata, then append the new content. */
				      (setq tmp (symbol "script")) 
				      (setq contents (new Structure: tmp script))  
				     ) ;; end begin
                 ) ;; end if
                (setq tmp (symbol "script"))
                (setq contents last: tmp script)
                (if (= traceLinesOn true)(writeln "recScriptInstructions: contents after: " contents));; LTN
			   ) ;; end begin
            ) ;; end if for contents
         ) ;; end begin in else
      ) ;; end if
 

	  ;; Add the new script to the old content using the proper heuristic. */
	  ;; Note: we do this only if we are in document mode. */
     (if (= result #void)
	     ;; If the old content is empty, then just set the new content. */
	     (begin
             (if (= traceLinesOn true)(writeln "recScriptInstructions: result is void. " ))
	         (setq result contents)
 
		  ) ;; end begin
		  else
          (begin
              (if (= result "")
			      ;; If the old content is a null string, then just set the new comment. */
			      (begin
                      (if (= traceLinesOn true)(writeln "recScriptInstructions: result is empty. " )) ;; LTN
			          (setq result contents)
 
			       ) ;; end  begin
		           else
                   (begin
			            (if (not (isStructure result))
				            ;; If the old content is cdata, then append the new content. */
				            (begin
                                (if (= traceLinesOn true)(writeln "recScriptInstructions: result not a structure. " )) ;; LTN
 
				                (setq tmp (symbol "__content"))
				                (setq result last: tmp result) 
 
 
				             ) ;; end begin
		                  ) ;; end if not structure

			              (setq tmp (symbol "script")) 
 
			              (setq result last: tmp contents) 
  
 
			         ) ;; end begin for else
                ) ;; end for if tempty string 
             ) ;; end begin for else in void
         ) ;; end if for void

        ;; for long script bugs
        (setq IP startp)
 

	       ;;  Try to recognize the script end tag name.  
           (if (and (= cc lessThan) (= c1 forwardSlash))  
		      (begin
                 ;; Promote input pointer past the </ symbols.  
		         (setq startp (+ startp 2))
                 (if (= traceLinesOn true)(writeln "recScriptInstruction: end tag name: " (char startp[0]))) ;; LTN
                 (setq IP startp)
		         (setq ename (recName))
                 (if (= traceLinesOn true)(writeln "ename after recName: " ename)) ;; LTN
                 (if (= ename #void) (if (= errorCheckOn true)(xmlError "xml:Expecting end script tag" ))) 
		         ;;  An HTML or XML name resets the HTML processing flag.  
                 (if (= name "html")
			        (begin
                        (setq htmlOn false)
                    ) ;; end begin
                 ) ;; end if
			 
                 ;; Promote input pointer past whitespace  
                 ;;(setq startp IP) 
               	 (vmregLoadInteger IP startp)
                 (vmregRunInHardware start:)
                 (setq cc startp[0])
                   
                 ;;(writeln "recScriptInstruction: in Less than end of element tag before while cc is: " cc) ;; LTN      
                 (while (and (> cc nill) (<= cc space)) (begin (++ startp) (setq cc startp[0])))
                 (vmregRunInHardware stop:)
                 (if (= traceLinesOn true)(writeln "recScriptInstruction: after promoting whitespace: " (char startp[0]))) ;; LTN
		 
		         (if (<> cc greaterThan) (if (= errorCheckOn true)(xmlError "xml: Expecting end script > symbol"  )))  
		         (++ startp)
                 ;; Promote past whitespace
                 (vmregRunInHardware start:)
                 (setq cc startp[0])
                 (while (and (> cc nill) (<= cc space)) (begin (++ startp) (setq cc startp[0])))
                 (vmregRunInHardware stop:)
                 (if (= traceLinesOn true)(writeln "recScriptInstruction: after > symbol: " (char startp[0]))) ;; LTN

	             (if (= ename "script") (setq IP startp)) 
             ) ;; end begin
         ) ;; end if
 
	    ;;  Send the end element event according to the mode we're in. 
       (if (= eventHandler true)
		    (begin
               ;;  Send the endElement message symbol to the eventLambda.  
		       ;;  Note: we do this only if we are in event handler mode. 	     
              (eventLambda.endElement name)
		     ) ;; end begin
       ) ;; end if work.eventHandler
      ) ;; end begin
     ) ;; end if
 
;;(if (= inputData true) (setq result originalResult))
;;(if (= inputData true) (setq result originalResult1))
 

 
    (if (= traceLinesOn true)(writeln "--recScriptInstruction END --" )) ;; LTN
(if (= true inputData ) (return testResult))
    (return result)
    
    
  
   
;; Return an XML invalid scripting instruction error. */

Error::
(if (= errorCheckOn true)(xmlError "xml: Expecting scripting instruction"  ))
  

#void) ;; end recScriptInstruction


































;;**EXPORTKEY**:xml:recValue
 (defchild xml:recValue()
;; ******************************************************************************************
;;  Summary: Recognize a value in an XML source string. If a value is found, the work->IP pointer is
;; promoted and the value is returned; otherwise, void is returned.
;;
;;             <?javaScript writeln("Hello World!"); ?>
;;
;;          				-or-
;;
;;             <Address> <FirstName>Michael</FirstName> <LastName>Korns</LastName> </Address> 
;;
;;           If no content is recognized, an empty string literal is returned.
;;           If singleton content is recognized, a string literal is returned;
;;           otherwise, a Structure of the attributed contents is returned. 
;;           The current input pointer is always left pointing to the first 
;;           non whitespace character which is not element content.
;; 
;; Args:     document      A Structure to be extended as an XML document
;;                         in tree model form. After being extended this 
;;                         structure is then returned as the result.
;;           work          The compiler work space which allows the xml
;;                         compiler to be invoked recursively.
;;           tagDepth      The current xml tag recursion level.
;;
;; Return:   result        The extended XML document in tree model form (if present).
;;
;; ****************************************************************************************** 
regs:((CharPointer:tIP) 
      (Integer:cc) 
      (len 0) 
      (Integer:n 0) 
      (CharPointer:stringBufp) 
      (CharPointer:startp)
      (CharPointer:ipNext)  
      (Integer:pp)
      (CharPointer:ipNextNext) 
      (Integer:ppp)
      (singlequote 39) (doublequote 34) (_0 48) (_9 57) (_A 65) (_a 97) (_Z 90) (_z 122) (underscore 95))
vars: (result 
      (ByteVector:valueName) 
      tStringBuf)


(if (= traceLinesOn true)(writeln "---recValue START---" ) );; LTN
;; Initialize the objects
;;(setq tIP IP)
(vmregLoadInteger IP tIP)
(setq cc tIP[0])
(setq stringBufp tIP)
(if (= traceLinesOn true)(writeln "recValue: start: " (char cc)) );; LTN

;;  An XML symbol value may begin with a single quote character. 
(if (= cc singlequote)
    (begin
       (if (= traceLinesOn true)(writeln "recValue: in singlequote " )) ;; LTN
       (setq valueName (new Vector: Byte: 5000))
       (setq n 0)
       (setq len 0)
       ;;(setq stringBufp[len] 0)
       (setq ipNext (++ tIP))
       (setq pp ipNext[0])
       (if (= traceLinesOn true)(writeln "recValue: singlequote: ipNext is: " (char pp))) ;; LTN
       (vmregRunInHardware start:)
       (while (<> (setq pp ipNext[0]) singlequote) 
           ;;(writeln "recValue: in singlequote pp to be appended is: " pp) ;; LTN
           (if (>= len 1020)(if (= errorCheckOn true)(xmlError "xml: Symbol constant too long!" )))
           ;;(setq valueName (append valueName (char pp))) 
           (setq valueName n pp)
           (setq valueName (++ n) 0)           
           (++ ipNext)          
        ) ;; end while
        (vmregRunInHardware stop:)
        (resize valueName (++ n))
;; debugging for Math
;;(setq valueName (symbol valueName))

        (if (= traceLinesOn true)(writeln "recValue: valueName in singlequote: " valueName)) ;; LTN
         ;; promote input pointer past single quote symbol
        (setq  IP (++ ipNext)) 
        ;;(setq startp work.IP) ;; LTN for debugging only
        (vmregLoadInteger IP startp)
        (if (= traceLinesOn true)(writeln "recValue: singlequote: after getting value name: " (char startp[0]) )) ;; LTN
        ;;(setq result (symbol valueName))
        (setq result valueName)
        ;;(writeln "---recValue END--- result: " result) ;; LTN
        (return result)
    ) ;; end begin
);; end if

;;  An XML symbol value may begin with a double quote character. 
(if (= cc doublequote)
    (begin
       (if (= traceLinesOn true)(writeln "recValue: in doublequote " )) ;; LTN
       (setq valueName (new Vector: Byte: 5000))
       (setq n 0)
       ;;(setq len 0)
       ;;(setq stringBufp[len] 0)
       (setq ipNext (++ tIP))
       (setq pp ipNext[0])
       (if (= traceLinesOn true)(writeln "recValue: doublequote: ipNext is: " (char pp))) ;; LTN
       (vmregRunInHardware start:)
       (while (<> (setq pp ipNext[0]) doublequote) do
            ;;(writeln "recValue: in doublequote pp to be appended is: " pp) ;; LTN
           (if (>= len 1020)(if (= errorCheckOn true)(xmlError "xml: String constant too long!" )))
           ;;(setq valueName (append valueName (char pp)))   
           (setq valueName n pp)
            (setq valueName (++ n) 0)          
           (++ ipNext)
           
         ) ;; end while
        (vmregRunInHardware stop:)
        (if (= traceLinesOn true)(writeln "recValue: valueName in doublequote is: " valueName)) ;; LTN
        (resize valueName (++ n))
        
;;(setq valueName (symbol valueName))
         ;; promote input pointer past single quote symbol
        (setq IP (++ ipNext))
        (setq result valueName)
     
       ;; (writeln "---recValue END--- result: " result) ;; LTN
        (return result)
    ) ;; end begin
);; end if

;;  An XML symbol value may begin with a digit
(if (and (>= cc _0) (<= cc _9))
    (begin
       (if (= traceLinesOn true)(writeln "recValue: digit: tIP[0] is: " (char tIP[0]))) ;; LTN
       (setq valueName (new Vector: Byte: 5000))
       (setq n 0)
       ;;(setq len 0)
       ;;(setq stringBufp[len] 0)
       ;;(setq valueName (append valueName (char cc)))
       (setq valueName n cc)
       (setq valueName (++ n) 0)
       (setq ipNext (++ tIP))        
       (setq pp ipNext[0])
       (if (= traceLinesOn true)(writeln "recValue: digit ipNext is: " (char pp))) ;; LTN

       (vmregRunInHardware start:)
       (while (or (and (>= (setq pp ipNext[0]) _0) (<= pp _9)) (= pp 37))
           ;;(writeln "recValue: in digit pp to be appended is: " pp) ;; LTN
           (if (>= len 1020)(if (= errorCheckOn true)(xmlError "xml: Numeric constant too long!"  )))
           ;;(setq valueName (append valueName (char pp)))      
           (setq valueName n pp)
           (setq valueName (++ n) 0)          
           (++ ipNext)
           (setq IP ipNext)           
        ) ;; end while
        (vmregRunInHardware stop:)

        (resize valueName (++ n))

;;(setq valueName (symbol valueName))
        (if (= n 2) (setq IP ipNext))
        (if (= traceLinesOn true)(writeln "recValue: valueName in digit is: " valueName)) ;; LTN                      
        (setq result valueName)
        ;;(writeln "---recValue END--- result: " result) ;; LTN   
        (return result)
    ) ;; end begin
);; end if


;;  An XML symbol value may begin with an underscore or an alpha character 
(if (or (and (>= cc _a) (<= cc _z)) (and (>= cc _A) (<= cc _Z)) (= cc underscore) (= cc 35) (= cc 45))
    (begin  
        (if (= traceLinesOn true)(writeln "recValue: in alpha or underscore  " )) ;; LTN   
        ;;(writeln "recValue calling recName in alpha or underscore" ) ;; LTN               
        (setq result (recName))
         
        (return result)
    ) ;; end begin
);; end if

;;  Any other HTML attribute value ends with a whitespace character, or ! ? , >. 
(if (compareEQ htmlOn true)
    (begin
       (if (= traceLinesOn true)(writeln "recValue: other value " )) ;; LTN
       (setq valueName (new Vector: Byte: 5000))
       (setq n 0)
       ;;(setq len 0)
       ;;(setq stringBufp[len] 0)
       (setq ipNext (++ tIP))
       (setq pp ipNext[0])
       (setq ipNextNext (++ ipNext))
       (setq ppp ipNexNext[0])
       (if (= traceLinesOn true)(writeln "recValue: other value: ipNext is: " (char pp))) ;; LTN
       (if (= traceLinesOn true)(writeln "recValue: other value: ipNextNext is: " (char ppp))) ;; LTN
       (vmregRunInHardware start:)
       (while (and (> (setq ppp ipNextNext[0]) 32) (<> (setq pp ipNext[0]) 33) (<> pp 63) (<> pp 44) (<> pp 62))
           (if (>= len 1020)(if (= errorCheckOn true)(xmlError "xml: Attribute constant too long!"  )))
           ;;(setq valueName (append valueName (char pp)))    
           (setq valueName n pp)
           (setq valueName (++ n) 0)         
           (++ ipNext)
           (++ ipNextNext)           
        ) ;; end while
       (vmregRunInHardware stop:)
        (if (= traceLinesOn true)(writeln "recValue: valueName: other value is: " valueName)) ;; LTN       
        (resize valueName (++ n))  

		(setq valueName (symbol valueName))
           
        (setq result valueName)
        ;;(writeln "---recValue END--- result: " result) ;; LTN
        (return result)
    ) ;; end begin
);; end if


#void)




























;;**EXPORTKEY**:xml:xmlError
(defchild xml:xmlError(errorString)
;; ******************************************************************************************
;;  Summary: Return a parsing error in an XML source string. The error is returned
;;   in a standard format.
;;
;;
;; Return:   result        The extended XML document in tree model form (if present).
;;
;; ******************************************************************************************
regs:((CharPointer:startp)                   ;; Temporary variable to store the starting pointer
      (CharPointer:current)                 ;; Temporary variable to store the starting pointer
      (Integer:pos)
      (Integer:endline1 0)
      (Integer:endline2 0)
      (Integer:endline3 0)
      (Integer:i 0)
      (Integer:n 0)
      (Integer:errorPos 0)
      (Integer:cc)                         ;; Temporary input character

       (CharPointer:tIP)                     ;; Temporary variable for saving error characters
       
       (Integer:c1) (Integer:c2) (Integer:c3)
       (Integer:saveCH)                     ;; Temporary input character to save
       (Integer:pos 0)
       (Integer:eof 0)
        
       (Integer:i 0) (Integer:j 0)
          
       (Integer:null 0)   )
vars: (  line1 tempLine1
         line2
         line3
         line4
         (ByteVector:inputString) )

(if (= traceLinesOn true)(writeln "---xmlError START --- " ) );; LTN 
(if (= traceLinesOn true)(writeln "xmlError: errorString passed is: "  errorString)) ;; LTN

;;  Initialize the starting variables.  
;;(setq startp start)  
;;(setq current work.IP)  
(vmregLoadInteger start startp)
(vmregLoadInteger IP current)    
(setq pos (-  current startp))
;;(setq eof work.INLEN)
(setq tempLine "")
(setq line1 "") 
(setq inputString (new Vector: Byte: 5000))
 

;; extract the input string until the error point 
(while (and (<> (setq cc startp[0]) 0) (< startp current)) do  
     ;; if newline is encountered
     (if (or (= cc 13) (= cc 10))
          (begin
              (setq errorPos 0)
              (++ i)
              (setq  saveCH startp[0])                
              ;;(setq inputString (append inputString (char saveCH))) 
              (setq inputString n saveCH)
              (setq inputString (++ n) 0)
              (setq line1 tempLine)
              ;;(writeln "xmlError: line1 is: " line1 ) ;; LTN
              (setq tempLine "")    
              (++ startp)
          ) ;; end begin
          else
         (begin
              (++ i)
              (setq  saveCH startp[0])
              ;;(writeln "xmlError: ch appended to templine in normal: " (char saveCH)) ;; LTN              
              ;;(setq inputString (append inputString (char saveCH)))
              (setq inputString n saveCH)
              (setq inputString (++ n) 0)
              (setq tempLine (append tempLine (char saveCH)))     
              (++ startp)
              (++ errorPos)
          ) ;; end begin
     ) ;; end if     
) ;; end while
 

;; getting the last of the error line
(while (and (<> (setq cc current[0]) 10) (<> cc 13) (<> cc 0)) do
    ;;(setq inputString (append inputString (char cc)))
    (setq inputString n saveCH)
    (setq inputString (++ n) 0)
    (setq tempLine (append tempLine (char cc)))
    ;;(writeln " appended char is: " (char cc) " -- i is: " i) ;; LTN
    (++ current)
    (++ i)
) ;; end while

(resize inputString (++ n))
(if (= traceLinesOn true)(writeln "xmlError: inputString: " inputString)) ;; LTN

;; getting the error line and different line1 scenarios
(if (= line1 #void) 
    (begin
        (setq line1 "")
        (setq line2 inputString)
    ) ;; end begin
    else
    (begin
        (setq line2 tempLine)
    ) ;; end begin
) ;; end if

(if (and (or (= (setq cc current[1]) 10) (= cc 13)) (<> cc 0))
    (begin
        ;;(setq inputString (append inputString (char cc)))
        (setq inputString (-- n) saveCH)
        (setq inputString n 0)
        (setq line3 (char cc))
        (++ current)
        (++ i)
     ) ;; end begin
) ;; end if
 
(resize inputString n)
(++ current) 

 
(while  (and (<> (setq cc current[0]) 10) (<> cc 13) (<> cc 0) ) do
    (setq line3 (append line3 (char cc)))
    ;;(writeln "xmlError: line3 appended is: " (char cc));; LTN
    (++ current)      
    (++ i)
) ;; end while

(if (= line3 #void) (setq line3 ""))

(setq tabs 0)
(setq notabs 0)
;;  Count number of tabs and non-tabs up to error in error line.  
(loop for x from 0 until  errorPos     
    (if (= (ref line2 x) (char 9)) 
        (begin (++ tabs))
     else
        (begin (++ notabs) 
         ;;(writeln "xmlError: Here: notabs: " notabs)
       )
    ) ;; end if
) ;; end loop 


;; Count number of lines up to the error line.  
(setq j 0)
(setq lineno 0)
(while (< j pos) do
	(if (= (ref inputString j) (char 13)) (++ lineno))
    (++ j)
) ;; end while
(++ lineno)
 
(setq t 0)
(setq n 0)
(setq errorLine "")

;; Create error message
;;(if (< (- line2 line4) 1024)
;;    (begin
;;        ;; Create a short error message
;;        (setq errorMsg (substring inputString pos (+ 20 pos)))
;;        (error errorString errorMsg " at[ " pos)
;;    );; end begin
;;    else
;;   (begin
;;         ;;Create a long error message
        (while (< t tabs) do (setq errorLine (append errorLine (char 9))) (++ t))
        (while (< n notabs) do (setq errorLine (append errorLine " ")) (++ n))
        (setq errorLine (append errorLine "^error"))
        (if (and (= line1 "") (<> line3 "")) (setq errorMsg (append  line2 #\newline errorLine #\newline line3)))
        (if (and (= line3 "") (<> line3 ""))  (setq errorMsg (append  line1 #\newline line2 #\newline errorLine)))
        (if (and (= line1 "") (= line3 ""))  (setq errorMsg (append  line2 #\newline errorLine  )))
        (if (and (<> line1 "") (<> line3 ""))  (setq errorMsg (append   line1 #\newline line2 #\newline errorLine #\newline line3  )))
     
         (setq errorString (append errorString " at line: " lineno  " char: "  (+ 1 errorPos)))
         (clean errorMsg)
         (writeln errorMsg)          
         (error errorString )

;; Send the errorMessage event (if necessary)
(if (= eventHandler true)
    (begin
    ;; Send the errorMessage message symbol to the eventLambda.  
    ;; Note: we do this only if we are in event handler mode.  
       (eventLambda.errorMsg errorString)        
    ) ;; end begin
) ;; end if

(if (= traceLinesOn true)(writeln "---xmlError END --- " ) );; LTN 

#void) ;; end xmlError

































