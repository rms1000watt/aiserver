;;*************************************
;;*************************************
;; Exported Agent File Cabinet Document
;;*************************************
;;*************************************

;;**EXPORTKEY**:xml
(defun xml(...)
;; *******************************************************************
;;  Summary: The XML compiler is a low-level Lisp agent designed as a 
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
;;                             to be compiled, or 
;;                         (b) An Agent which provides the input 
;;                             string, containing the XML source, 
;;                             to the xml function on demand 
;;                             (see the Notes below).
;;           eventMgr    (Optional) Either: 
;;                         (a) A Structure containing the process 
;;                             instruction agents bound to the 
;;                             active pi target names, or
;;                         (b) An Agent which will manage xml document 
;;                             events as described in the Ref Guide 
;;                             Chapter on Document Event Handlers 
;;                             (see the Notes below).
;;
;; Return:   Document     A compiled XML document in tree model form.
;;
;; Notes:
;;        ============
;;        Input Agents
;;        ============
;; 
;;        The xml function supports the compilation of a multiple input strings 
;;        served from an input agent. This allows compilation of include files 
;;        and other instances where multiple XML source strings must be compiled 
;;        as a single XML document. 
;;
;;        The inputAgent must be an Agent which will provide strings of complete
;;        XML source via its (moreSource) child agent. The inputAgent must support 
;;        the moreSource child agent by returning either #void (if there is no 
;;        more source) or the next input string in the proper sequence.
;;
;;        Each input string must be a complete XML fragment.
;;
;;          Syntax:(inputAgent.moreSource) 
;;
;;          Returns An complete XML input string or #void
;; 
;;          Example
;; 
;;               (inputAgent.moreSource)
;;
;;             Returns the string
;;
;;               "<?xml version=?1??><Name>John</Name>"
;; 
;;          Each input string must be a complete XML fragment. 
;;          This means that each input string must end at an end tag boundary.
;;    
;;        ====================
;;        Event Manager Agents
;;        ====================
;; 
;;        Compiling A String with event handling
;;        The xml function supports the compilation of a single source string with event handling. 
;;        No XML document tree model is returned. The second argument must be an Agent which will 
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
;;        Sends the following events to the eventMgr agent:
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
           buffer                           ;; Incomplete document model referenced by the agents
          (Boolean:traceLinesOn false)      ;; Option to display tracelines
          (Boolean:errorCheckOn false)      ;; Option to call the xmlError agent when there are errors
                                            ;; in the HTML page evaluated (e.g. mismatched tags, no contents
                                            ;; between html tags, etc.)
          (Boolean:stemmingOn false)        ;; Option to call the porterStemmer, passing to it
                                            ;; a byte vector containing text between html tags
           outputVector                     ;; Output of the runStructure agent        
           runStructureTop                  ;; Functions that walks the structure, returning the
           runStructure                     ;; values of the structure as a String
           stemmedVector                    ;; Ouput of the runStructure and runStructureTop functions
          (ByteVector:stemVector)           ;; Ouput if te stemming option is true, this will be 
                                            ;; passed to the porterStemmer for stemming
          (nn 0)                            ;; Number of words inside the stemVector persistent variable(above)
           global
           ) ;; end of persistent variables
    vars:(arg                          	    ;; Argument temporary variable
          elementName                       ;;  XML name to be evaluated
           ;; Compiler work space to allow recusive xml invocations.
           (work #{past 			#void	;; The XML past input character count (Integer)
		           start 			#void	;; The XML starting input pointer (CharPointer)
		           INLEN   		    #void	;; The XML input string fragment length (CharPointer)
		           IP 				#void	;; The XML input pointer (CharPointer)
		           inputString 	    #void	;; The XML current input string
		           Document 		#void	;; The XML document model (Structure)
		           inputProvider 	#void	;; The input provider is present flag (Boolean)
		           inputAgent 		#void	;; The input provider agent (Agent)
		           eventHandler 	#void	;; The event handler is present flag (Boolean)
		           eventAgent 		#void	;; The event agent (Agent)
		           piHandler 		#void	;; The process instruction handler is present flag (Boolean)
		           piStructure 	    #void	;; The process instruction Structure (Structure)
		           htmlOn 			#void	;; The HTML is processing flag (Boolean)
                   stringBuf        #void   ;; The string buffer for quick name recognition
                  
		           })
           ) ;; end of temporary variables
    regs: ((CharPointer:p) Integer:c Integer:n)

(defun prettyPrint (buffer memory margin)
vars:(n N key newMargin sourceTemplate
            (blanks "                                                                                                                                                               ")
            (tailSW false)
            ) ;; end temporary variables
      
;; Initialize the display buffer (if necessary).
 
(if (= buffer #void)
    (begin 
         (setq buffer (new Vector: byte: 2000000000))
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


 ;; create a First Level Dictionary
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

 ;; create a Second level dictionary
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
               
;; Create Third Level Dictionary
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
   (setq outputVector (new Vector: Byte:100000000 ))
   ;;(setq stemmedVector (new Vector: Byte:100000000))
   (setq outputVector (xml.runStructure outputStructure outputVector))
   (setq outputVector (string outputVector true))    
   (setq stemmedVector (porterStemmer  outputVector ))
    
) ;; end runStructureTop


(defun runStructure (outputStructure outputVector)
vars: (n N  v vecLength p testItem  )
;;pvars: (ByteVector:outputVector) 

(setq p 0)
(setq n 1)
(setq N  (length outputStructure) )


(loop for n from 0 until N do    
    (setq testItem (ref outputStructure n 1))   
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
 
    ;; Initialize the basic state of the compiler.
    (if (= traceLinesOn true)(writeln "NEW XML") );; LTN
    (if (= traceLinesOn true)(writeln "xml: errorCheckOn: "  errorCheckOn)) ;; LTN
    (setq stemVector #void)
    (if (= stemmingOn true) (setq stemVector (new Vector: Byte: 100000000)))
    (setq n 0)
    (setq work (new work))
    (setq work.inputString #void)
    (setq work.inputProvider false)
    (setq work.inputAgent #void)
    (setq work.eventHandler false)
    (setq work.eventAgent #void)
    (setq work.piHandler false)
    (setq work.piStructure #void)
    (setq work.htmlOn false)
    (setq work.stringBuf (new String: ""))
    (resize work.stringBuf 1024)

    ;; Initialize the xml document model (if any)
    (if (and (> (argCount) n) (isStructure (setq arg (argFetch n))))
        ;; We have an existing Document model to extend.
        (begin
           (++ n)
           (setq work.Document arg) 
        )
        ;; We must create a new Document model.
        (begin
           (setq work.Document (|Gv:new| Structure:)) 
        )) ; end if
    ;; Initialize the xml input source
    (if (and (> (argCount) n) (isAgent (setq arg (argFetch n))))
        ;; We have an input agent.
        (begin
           (++ n)     
           (if (= traceLinesOn true)(writeln "xml: in argCount isAgent: arg:  " arg )) ;; LTN
           (setq work.inputProvider true)
           (setq work.inputAgent arg) 
           (setq work.inputString (setq arg (work.inputAgent.moreSource)))
           (setq work.INLEN (length arg)) 
           (setq work.IP (setq p arg)) 
        )
        ;; We have a single input source string.
        (begin
           (++ n)
           (setq work.inputProvider false)
           (setq work.inputString arg) 
           (setq work.inputAgent #void) 
           (setq work.INLEN (length arg)) 
           (setq work.IP (setq p arg)) 
        )) ; end if
    ;; Initialize the event manager (if any)
    (if (and (> (argCount) n) (isAgent (setq arg (argFetch n))))
        ;; We have an event manager agent.
        (begin
           (++ n)
           (if (= traceLinesOn true)(writeln "xml: in argCount isEventManagerAgent:arg " arg)) ;; LTN
           (setq work.eventAgent arg) 
           (setq work.eventHandler true) 
           (setq work.piStructure #void) 
           (setq work.piHandler false) 
        )) ; end if
    ;; Initialize the pi structure (if any)
    (if (and (> (argCount) n) (isAgent (setq arg (argFetch n))))
        (begin
           (++ n)
           (if (= traceLinesOn true)(writeln "xml: in argCount pistructure:arg " arg) );; LTN
           (setq work.piStructure arg) 
           (setq work.piHandler true) 
           (setq work.eventAgent #void) 
           (setq work.eventHandler false) 
        )) ; end if
    (if (= n 0) (error "xml: expecting at least one argument"))

    ;; Create the XML document object which will be the result of this compilation.
    ;; Note: If we are using an event handler, we do not create a document. */
    (if (= work.eventHandler true)
	    (begin
	       ;; Send the startDocument event to the eventAgent.
	       (setq work.Document #void)
           (if (= traceLinesOn true)(writeln " xml: EventAgent: startDocument" )) ;; LTN            
	       (eventHandler.startDocument)
            ;;(writeln "startDocument: myDOM: " myDOM)
	    )) ; end if

	;; Initialize the input string pointer and the starting input pointer.

    (setq work.past 0)
    (setq work.start work.IP)

    ;; Recognize the content of the XML document.
    
    (recContents work.Document work 0)
    (if (= traceLinesOn true)(writeln "xml: result of recContents: " work.Document) );; LTN

    ;; There should be no more content left.
    (setq p work.IP)
    (setq c p[0])
    (if (= traceLinesOn true)(writeln "xml: work.ip is: " c))
    (if (= errorCheckOn true)(if (<> c 0) (error "xml: expected end of file")))
     
    ;; Return the XML document object resulting from this compilation.
    ;; Note: If we are using an event handler, we send the endDocument message.
    (if (= work.eventHandler true)
	    (begin
	       ;; Send the endDocument event to the eventAgent.
	       (setq work.Document #void)
	       (work.eventAgent.endDocument)
	    )) ; end if
   
    ;;(setq buffer (prettyPrint buffer work.Document 2))
    
    ;;(runStructureTop work.Document)
    (if (= stemmingOn true) (setq stemVector (porterStemmer stemVector)))
    ;;(if (= traceLinesOn true) (writeln "afterStemming: " stemVector))
    
work.Document) ;; end xml





















;;**EXPORTKEY**:xml:recAttributeList
(defchild xml:recAttributeList(work)
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
regs:(CharPointer:ip  cc)
regs:((_a #\a) (_z #\z) (_A #\A) (_Z #\Z) (underScore #\_) (space 32) (nill 0) (equal 61)  )
vars:(name value attributes)

(if (= traceLinesOn true)(writeln "---recAttributeList START--- " )) ;; LTN
(setq ip work.IP)
(setq cc ip[0])
(if (= traceLinesOn true)(writeln "recAttributeList: start is: " (char cc))) ;; LTN
 
Fetch::
;; Try to recognize the attribute name. 
(if (or (and (>= cc _a) (<= cc _z)) (and (>= cc _A) (<= cc _Z)) (= cc underScore)  ) 
    (begin   
    (setq name (recName work))
    (if (= traceLinesOn true)(writeln "recAttributeList: in CHARACTERS attribute name: " name )) ;; LTN 
    (if (isEqual name #void) then (if (= errorCheckOn true)(xmlError "xml: Invalid element attribute name" work)))
     ;;Promote input pointer past whitespace
    (vmregRunInHardware start:)
    (setq ip work.IP) 
    (if (= traceLinesOn true)(writeln "recAttributeList: after recName: " (char ip[0]) )) ;; LTN 
    (setq cc ip[0])      
    (while (and  (<= cc space) (> cc nill)) do (++ ip)(setq cc ip[0]))(setq work.IP ip)
    (vmregRunInHardware stop:)
    (if (= traceLinesOn true)(writeln "recAttributeList: after promoting whitespace: " (char ip[0]) )) ;; LTN   
     
    ;; Check if character is "=" indicating an attribute
    (if (compareEQ ip[0] equal)
        (begin            
            (++ ip)
            (setq work.IP ip)                   
            ;;Promote input pointer past whitespace
            (if (= traceLinesOn true)(writeln "recAttributeList: past = sign: " (char ip[0]) )) ;; LTN 
            (vmregRunInHardware start:)
            (setq ip work.IP) (setq cc ip[0])
            (while (and  (<= cc 32) (> cc 0)) do (++ ip)(setq cc ip[0]))(setq work.IP ip)
            (vmregRunInHardware stop:)     
            (if (= traceLinesOn true)(writeln "recAttributeList: after promoting past whitespace: " (char ip[0]) )) ;; LTN        
            (setq value (recValue work))
            (if (= traceLinesOn true)(writeln "recAttributeList: value is: " value)) ;; LTN
            (if (compareEQ value #void) (if (= errorCheckOn true)(xmlError "xml: Invalid element attribute value" work)))

            ;;Promote input pointer past whitespace
            (vmregRunInHardware start:)          
            (setq ip work.IP) (setq cc ip[0])              
            (while (and  (<= cc 32) (> cc 0)) do (++ ip)(setq cc ip[0]))(setq work.IP ip)
            (vmregRunInHardware stop:)
            (if (= traceLinesOn true)(writeln "recAttributeList: after promoting past whitespace: " (char cc) )) ;; LTN  
        ) ;; end begin
        else
        (begin
             (if (= traceLinesOn true)(writeln "recAttributeList: there is no = sign ")) ;; LTN 
             (if (compareEQ work.htmlOn true)
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
           (setq ip work.IP)         
           (setq cc ip[0])(while (and  (<= cc 32) (> cc 0)) do (++ ip)(setq cc ip[0]))(setq work.IP ip)    
           (vmregRunInHardware stop:)
           (if (= traceLinesOn true)(writeln "recAttributeList: after promoting past whitespace: " (char ip[0]) )) ;; LTN      
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
(defchild xml:recCData(theContents result work)
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
       (CharPointer:start)                  ;; Temporary variable to store the position of the pointer
       (CharPointer:ip)                     ;; Temporary variable fore saving comment characters
       (Integer:cc)                         ;; Temporary input character
       (Integer:c1) (Integer:c2) (Integer:c3) (Integer:c4) (Integer:c5) (Integer:c6) (Integer:c7) (Integer:c8)
       (_! 33) (leftbracket 91) (_C 67) (_D 68) (_A 65) (_T 84)  (nill 0)
       (Integer:saveCH)                     ;; Temporary input character to save
       (Integer:INLEN )
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
(setq start work.IP)
(if (= traceLinesOn true)(writeln "recCData: start is : "  (char start[0]))) ;; LTN
(setq cdata (new Vector: Byte: 100000))
(setq cc start[0])
(setq c1 start[1])
(setq c2 start[2])
(setq c3 start[3])
(setq c4 start[4])
(setq c5 start[5])
(setq c6 start[6])
(setq c7 start[7])
(setq c8 start[8])

;; We recognize marked sections here.                              
;; Note: <![CDATA[ ... ]]> is an example of a marked section.      
;;       Marked sections may occur anywhere in contents sections.  
(if (and (= c1 _!) (= c2 leftbracket) (= c3 _C) (= c4 _D) (= c5 _A) (= c6 _T) (= c7 _A) (= c8 leftbracket))
    (begin
         ;; Promote the input pointer past the mark header. 
         (setq start (+ 9 start))
         (if (= traceLinesOn true)(writeln "recCData: after promotion of mark header is: " (char start[0]))) ;; LTN
         (vmregRunInHardware start:)
         (setq n 0)
         (while  (and (<> (setq cc start[0]) 0)(or   (<> (setq cc start[0]) 93) (<> (setq c1 start[1]) 93) (<> (setq c2 start[2]) 62)  )) do
             ;; Saves the characters
             (setq ip start)
             (setq saveCH ip[0])             
             (setq cdata n saveCH)
             (setq cdata (++ n) 0) 
             ;; Promote the pointer
             (++ start)            
         ) ;; end while
         (if (= traceLinesOn true)(writeln "recCData: after getting getting cdata contents start[0] is: " (char start[0]))) ;; LTN
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
		       (setq start work.IP)
               (if (= traceLinesOn true)(writeln "recCData: error: cdata not closed " (char start[0]))) ;; LTN
		       (if (= errorCheckOn true)(xmlError "xml: CDATA section not closed" work))
		    ) ;;end begin
         ) ;; end if

       ;; Promote pointer to end of cdata trailer
       (if (<> cc nill) (setq start (+ 3 start)))
       (if (= traceLinesOn true)(writeln "recCData: end of cdata trailer work.ip gets start: " (char start[0]))) ;; LTN            
       (setq work.IP start)
 
      ;; Note: the imbedded comment will be assigned in the last position of the Structure.  
	  (if (= work.eventHandler true)
          (begin
               (if (= traceLinesOn true)(writeln "recCData: workEvent Handler is true before setting result: " )) ;; LTN
		       ;; Send the comment message symbol to the eventAgent.  
		       ;; Note: we do this only if we are in event handler mode.  
               ;;(setq result last: work.eventAgent 1 contents)
               (work.eventAgent.characters cdata)
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
                       (setq INLEN (length result))
                       ;;(writeln "recCData in result is a Structure  " result) ;; LTN 
                       ;;(writeln "recCData in result is a Structure INLEN is " INLEN) ;; LTN  
                       
                       (if (> INLEN 0)
                         ;;  If the old content is a Structure, then insert the new content.  
			             ;; Note: If the last item inserted was character content,  
			             ;;       then append this content to it.  
                          (begin
                             (setq name (ref result (sub1 INLEN) 0))
                             (if (= traceLinesOn true)(writeln "recCData in  result is a Structure: name is:  " name)) ;; LTN 
                             (setq value (ref result (sub1 INLEN) 1))
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
                                     (delete result (sub1 INLEN))
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
  (if (= errorCheckOn true)(xmlError "xml: Expecting CDATA section" work))

#void) ;; end recCData




















;;**EXPORTKEY**:xml:recComments
(defchild xml:recComments(theContents result work)
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
       (CharPointer:start)                  ;; Temporary variable to store the position of the pointer
       (CharPointer:ip)                     ;; Temporary variable fore saving comment characters
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
(setq start work.IP)
(if (= traceLinesOn true)(writeln "recComments: arguments passed: start get work.IP: " (char start[0]))) ;; LTN
(setq comment (new Vector: Byte: 10000))

(setq cc start[0])
(setq c1 start[1])
(setq c2 start[2])
(setq c3 start[3])

;;  Check for a start of a comment.   
;;  Note: <!--  --> are comment tags. 
(if (and (= c1 _!)(= c2 dash) (= c3 dash))
   (begin
        ;;(writeln "recComments: inside if exp is a comment" ) ;; LTN
	    ;;  Promote the input pointer past the comment header.  
        (setq start (+ 4 start))
        (if (= traceLinesOn true)(writeln "recComments: start: past comment header: " (char start[0]))) ;; LTN
        (setq ip start)        
        (setq cc start[0])
        (setq c1 start[1])
        (setq c2 start[2])
        (setq c3 start[3])
        (if (= traceLinesOn true)(writeln "recComments: start character of comment before while: " cc c1 c2 c3)) ;; LTN
	    (vmregRunInHardware start:)
        ;; We look for the end of the comment.          
        (while (and (<> (setq cc start[0]) 0) (or (<> cc dash) (<> (setq c1 start[1]) dash) (<> (setq c2 start[2]) #\>))) do
	          ;; ip variable saves the characters until the end of comment
              (setq ip start)
              (setq saveCH ip[0])
              (setq comment n saveCH)
              (setq comment (++ n) 0)
              ;; start variable ignores comment characters.             
              (++ start)
       ) ;; end while
      (vmregRunInHardware stop:)
      (resize comment (++ n))
      
      (if (= traceLinesOn true)(writeln "recComments: comment is: "  comment)) ;; LTN
      (if (= traceLinesOn true)(writeln "recComments: start: end of comment: " (char start[0]))) ;; LTN
       
        ;; Is the comment not closed? 
        (if (<> (setq cc start[0]) dash)
            (begin
               (if (= traceLinesOn true)(writeln "recComments: inside comment not closed" ) );; LTN
               (setq work.IP (- 4 start))
               (if (= traceLinesOn true)(writeln "recComments: start: error: comment not closed: " (char start[0]))) ;; LTN               
               (if (= errorCheckOn true)(xmlError "xml: Comment not closed" work))
             ) ;; end begin
        ) ;; end if

       ;; Promote pointer to end of comment trailer
       (if (<> cc nill)
           (begin
              (setq start (+ 3 start))
              (if (= traceLinesOn true)(writeln "recComments: start: past end of comment trailer: " (char start[0]))) ;; LTN
           );; end begin
       ) ;; end if
       (setq work.IP start)
                    
       (if (= traceLinesOn true)(writeln "recComments: current position of pointer: " (char start[0]))) ;; LTN    
       (if (= traceLinesOn true)(writeln "recComments: before setting or result: result is: " result)) ;; LTN
   
        
      ;; Note: the imbedded comment will be assigned in the last position of the Structure.  
	  (if (= work.eventHandler true)
          (begin
		       ;; Send the comment message symbol to the eventAgent.  
		       ;; Note: we do this only if we are in event handler mode.  
               (work.eventAgent.comments comment)
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
  
  (if (= errorCheckOn true)(xmlError "xml: Expecting comment expression" work))

#void) ;; end recComments





















;;**EXPORTKEY**:xml:recContents
(defchild xml:recContents(document work tagDepth)
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
	vars: ((Boolean:whitespace false) 
            tmp 
            attributes 
            __pi 
            stringBuf
            result 
            theContents 
            piAgent 
            name 
            (ByteVector:tempChar)
            (ByteVector:contentName)
            (ByteVector:stemWord)
            ename 
            value 
            last 
            tempName 
           ) ;; end of temporary variables
    regs: ((CharPointer:namep)					;; Temporary input name pointer
           (CharPointer:start)					;; Temporary input string pointer
           (CharPointer:stringBufp)
           (CharPointer:ip)
           (CharPointer:stemPointer)
           (Integer:next)					    ;; Temporary input string pointer
           (Integer:test) 				        ;; Temporary string compare switch
           (Integer:cc)                         ;; Temporary input character
           (Integer:c1)                         ;; Temporary input character
           (Integer:saveCH)                     ;; Temporary input character to save
           (Integer:INLEN)                      ;; Temporary input string length
           (Integer:len 0)
           (Integer:n 0) 
           (Integer:n1 0)
           (Integer:nill 0)
           ) ;; end of register variables 
    ;;pvars: (stemVector2 (nn 0))

    (++ recContentsCtr) ;; LTNTemp    
    (if (and (= stemmingOn true)(= stemVector #void)) (setq stemVector (new Vector:)))


    ;; Default the xml document to a single empty text string (if necessary);
    ;; otherwise, return the extended xml document as the contents result.
    (setq traceLinesOn (boolean traceLinesOn))
    (if (= traceLinesOn true)(writeln "---recContents START--- "  )) ;;  LTN
    (if (= traceLinesOn true)(writeln "recContents: arguments passed: document:  " document));;  LTN
    (if (= traceLinesOn true)(writeln "recContents: arguments passed: tagDepth :  " tagDepth));; LTN
    (if (= traceLinesOn true)(writeln "recContentsCtr: " recContentsCtr)) ;; LTN

    ;; may be possible source for no content bug
     
    (if (<> document #void) (setq result document))

    (if (= traceLinesOn true)(writeln "recContents: in the beginning result is: "result)) ;; LTN
    ;; Possbiel void source
    (if  (= result #void) 
         (begin 
         (setq result "")
         ;;(setq result document)
         (setq whitespace true)
         (if (= traceLinesOn true)(writeln "recContents: inside result whitespace is: " whitespace)) ;; LTN
         );; end begin
         else
         (begin (setq result document))
     ) ;; end if

   

    ;; Begin central recognition loop through the input.
    Fetch::
    ;;(debug traceon: ) ;;LTN testing

 

    (setq start work.IP)   
    (setq cc start[0])
    (if (= traceLinesOn true)(writeln "recContents: Fetch:: start gets work.IP: " (char start[0]) (char start[1]) (char start[2])(char start[3]) (char start[4]) (char start[5]) (char start[6]))) ;; LTN
    (if (= traceLinesOn true)(writeln "recContents: Fetch:: result is:  " result));;  LTN 
    (if (= traceLinesOn true)(writeln "recContents: Fetch:: whitespace is:  " whitespace));;  LTN
    ;; Possible source of endless recursion bug
    (if (or (= cc #void) (= cc 0))(goto Zero:))
    (if (= cc #\<) (goto LessThan:))
    (goto Default:)
    
    ;; ===================================================
    ;; Input Character is 0
    ;; ===================================================
    Zero::   
	(if (= work.inputProvider true)
        (begin
		  ;; Send the moreSource message symbol to the inputAgent.
		  ;; Note: we do this only if we are in event handler mode.
		  (setq tmp (work.inputAgent.moreSource))
		  (if (<> tmp #void)
			  (begin
				 (setq work.inputString tmp)
                 (setq work.INLEN (length tmp))
                 (setq start tmp) 
				 (setq work.past 0)
				 (setq work.start start)
				 (setq work.IP start)
			     (goto Fetch:)
			  )) ; end if
 		)) ; end if

	(setq work.IP start)  
    (if (= traceLinesOn true)(writeln "recContents: Zero:: work.IP gets start: " (char start[0])));;  LTN  
   
	(goto Done:)

    ;; ===================================================
    ;; Input Character is <
    ;; ===================================================
    LessThan::
 
    (if (= traceLinesOn true)(writeln "recContents: LessThan:: start is: " (char cc)));;  LTN
	;; ******************************************** 
	;; Check for an end of element content tag. 
	;; ********************************************
    ;; Note: </Name> are end of element content tags.
    (if (= (setq cc start[1]) #\/)
		;; We have found an end of element content tag.
        (begin   

          (if (= traceLinesOn true)(writeln "recContents: LessThan:: end of element tag:  start[1]: " (char cc))) ;; LTN     
  
		  ;; Return to the calling recursion used to be here but I transferred it below 
          ;; to check the matching end of element and ignore the whitespaces after it
          ;;(if (> tagDepth 0) (goto Done:))

		  ;; Try to recognize the element end tag name.
		  (setq work.IP (+= start 2))  ;; Promote input pointer past the </ symbols.
          (if (= traceLinesOn true)(writeln "recContents: LessThan:: end of element::  start is after promotion: " (char start[0]))) ;; LTN
           
          ;; BUG : for ending recursion
          ;;(writeln "crashPOINT before: " loryfel )
          ;;(if  (>= loryfel 3050)(begin (systemCheck) ))
          ;;(if (or (= loryfel 6425) (= loryfel 2969)) (begin   (debug traceon: erroron: )))
          ;;(writeln "going to recName")
           

		  (setq ename (recName work))
          (if (= traceLinesOn true)(writeln "recContents: in LessThan Label end of element content name is: " ename) );;  LTN
          (setq start work.IP)
          (if (= traceLinesOn true)(writeln "recContents: LessThan:: end of element::  start gets work.IP after recName "  start[0])) ;; LTN
          (if (= traceLinesOn true) (writeln "recContents: elementName is: " elementName)) ;; LTN
           ;; checking if end of tag matches the beginning tag
            (if (compareEQ ename elementName) 
                (begin       
                     (setq cc start[0])                        
                     (if (<> cc #\>) (if (= errorCheckOn true)(xmlError "xml: expected element end tag > symbol" work)))                                   
                      ;; Promote pointer past the whitespace
                     (vmregRunInHardware start:)
                     (setq start work.IP)
                     (++ start)
                     (setq cc start[0])                                             
                     (while (and (> cc 0) (<= cc 32)) (begin (++ start) (setq cc start[0])))
                     (vmregRunInHardware stop:)                  
                     (setq work.IP start)
                     (if (= traceLinesOn true)(writeln "recContents: LessThan:: end of element: Tag Match: start promoted past whitespace: " (char start[0]) )) ;; LTN 
                     (setq elementName #void) 
                     ;;(writeln "tagmatch: tempchar: " tempChar  )                                       
                     (goto Done:)
                ) ;; end begin
            ) ;; end if

          
		  (if (= ename #void) 
               (if (= errorCheckOn true)
                   (begin                        
                      (xmlError "xml: Expected element end tag name" work)
                    ) ;; end begin
                ) ;; end if  
           ) ;; end if

          (setq start work.IP)
            

		  ;; An HTML end name sets the HTML processing flag to true.
          (if (<> ename #void)
              (begin
                  (vmregStringiCompare ename "html" test)
		          (if (= test 0) (setq work.htmlOn true))
               ) ;; end begin
          ) ;; end if 

		  ;; Promote input pointer past whitespace
          (vmregRunInHardware start:)
          (setq cc start[0])
          (while (and (> cc 0) (<= cc 32)) (begin (++ start) (setq cc start[0])))
          (vmregRunInHardware stop:)
          (if (= traceLinesOn true)(writeln "recContents: LessThan:: start promoted past whitespace: " (char start[0]) )) ;; LTN
		  (if (<> cc #\>) (if (= errorCheckOn true)(xmlError "xml: expected element end tag > symbol" work)))
		  (setq work.IP  (++ start)) ;; Promote input pointer past the > symbol.
          (if (= traceLinesOn true)(writeln "recContents: LessThan:: start promoted past > symbol: " (char start[0]) )) ;; LTN
                   
          ;; Return to the calling recursion
          (if (= traceLinesOn true)(writeln "recContents: Returning to calling recursion: " (char start[0]) ))
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

;;(if (= traceLinesOn true)(writeln "recContents: for appending result which is text: "  result ))
;;(if (= traceLinesOn true)(writeln "recContents: document: " document))
		(if (not (isStructure result)) 
		(begin
			;; Save the current contents
			(setq theContents result)
            ;; for the  void contents bug with another html tag
            ;;(if (not (isCharWhitespace theContents)) (setq whitespace false)) 
            (if (= traceLinesOn true)(writeln "recContents: for not isStructure " theContents ))
          
			;; Make sure the contents are converted to a Structure.
			;; possible void bug             
             (setq result (^new Structure:))
             
             
			;; Make sure that any non-whitespace contents are saved in the Structure.
			;; Note: the whitespace flag will tell us whether we have seen important data.
			(if (= whitespace 0)
				(if (= work.eventHandler true)
                 (begin
                       (if (= traceLinesOn true)(writeln "recContents: in NOT appending __contents "   ))
					;; Note: we do this only if we are in eventHandler mode.
	       			 (work.eventAgent.characters theContents) 
                  ) ;; end begin
				else
                 (begin
					;; Note: we do this only if we are in document mode.   
                    (if (= traceLinesOn true)(writeln "recContents: in appending __contents " theContents ))             
                    (setq tmp (symbol "__content"))
                    ;;(setq theContents (append theContents #\newline))
					(set result last: tmp theContents)
                    ;;(writeln "appended in the embedded tag: " theContents) ;; LTNTemp
                    ;;(writeln "actual last embedded tag: " result[(sub1 (length result)) 1]) ;; LTNTemp
                     
                   
                    ;; FOR STRING START
                    ;;(if (= stemmingOn true)
                    ;;    (begin
                    ;;        (setq stemPointer theContents)
                    ;;        (setq n 0)
                    ;;        (while (> (setq cc stemPointer[0]) 32) do
                    ;;            (setq saveCH stemPointer[0])
                    ;;            (setq stemVector n saveCH)
                    ;;;            (setq stemVector (++ n) 0)
                    ;;            (++ stemPointer) 
                    ;;        ) ;; end while
                    ;;        (setq stemVector n 32)
                    ;;        (writeln "stemVector is: " stemVector)
                    ;;     ) ;; end begin
                    ;;) ;; end if
                    ;; FOR STRING END
                    ;;(setq result (append result #\newline))
				) ;; end begin			
			) ;; end if work event handler
		   ) ;; end if whitespace is false
       ) ;; end begin is not structure
    ) ;; end if is not structure


	;;************************************
	;; Check for a start of a comment.   
       ;; Note: <!--  --> are comment tags. 
	;;************************************
  
       (if (and (= (setq cc start[1]) #\!) (= (setq cc start[2]) #\-) (= (setq cc start[3]) #\-))
		    ;; We recognize and process the comment.
		    (begin
                (if (= traceLinesOn true)(writeln "recContents: LessThan:: Comments "  )) ;; LTN 
		       (setq result (recComments theContents result work))  
               (if (= traceLinesOn true)(writeln "recContets: result of recComments: " result))  ;; LTN
		       (goto Fetch:)
		    ) ;; end begin
        ) ;; end if

	;;*****************************************************************
	;; We recognize marked sections here.                             
	;; Note: <![CDATA[ ... ]]> is an example of a marked section.     
	;;       Marked sections may occur anywhere in contents sections. 
	;;*****************************************************************

	(if (and 
             (= (setq cc start[1]) #\!)
             (= (setq cc start[2]) #\[)
             (= (setq cc start[3]) #\C)
             (= (setq cc start[4]) #\D)
             (= (setq cc start[5]) #\A)
             (= (setq cc start[6]) #\T)
             (= (setq cc start[7]) #\A)
             (= (setq cc start[8]) #\[)
            )
		(begin
           (if (= traceLinesOn true)(writeln "recContents: LessThan:: CDATA "  )) ;; LTN
           (setq whitespace false)
           (if (= traceLinesOn true)(writeln "recContents calling recCData with result: " result)) ;; LTN
		   (setq result (recCData theContents result work))    
		   (goto Fetch:)
		) ;; end begin
    ) ;; end if

	;;************************************************************************ 
	;; Check for an imbedded process instruction tags.                        
       ;; Note: <?Name ...contents... ?> are imbedded process instruction tags.  
	;;************************************************************************

     (if (= (setq cc start[1]) #\?)
		;; We have found an imbedded process instruction.
		(begin
		  (setq result (recProcessInstruction theContents result work))
		  (goto Fetch:)
		 ) ;; end begin
      ) ;; end if

	;;************************************************************************ 
	;; Check for an imbedded document type definition tag.                        
       ;; Note: <!Name ...contents... > are document type definition tags. 
	;;************************************************************************

     (if (= (setq cc start[1]) #\!)
		 ;; We have found an document type definition.
		(begin
              (if (= traceLinesOn true)(writeln "recContents calling recDoctypeHeader: ")) ;; LTN
              (setq result (recDoctypeHeader theContents result work tagDepth))
              (goto Fetch:)
         ) ;; end begin
      ) ;; end if

	;;************************************************************************ 
	;; Check for a SCRIPT tag which has meaning in HTML mode.                        
       ;; Note: <script ...contents... <script> are HTML script tags. 
	;;************************************************************************

       (if (and (= work.htmlOn true)
                (or (= (setq cc start[1]) #\S) (= cc #\s))
                (or (= (setq cc start[2]) #\C) (= cc #\c))
                (or (= (setq cc start[3]) #\R) (= cc #\r))
                (or (= (setq cc start[4]) #\I) (= cc #\i))
                (or (= (setq cc start[5]) #\P) (= cc #\p))
                (or (= (setq cc start[6]) #\T) (= cc #\t))
           )
		;; We have found an HTML script tag.
		    (begin
                ;;(setq work.IP start)
                (if (= traceLinesOn true)(writeln "work.IP is : " work.IP))
		        (setq result (recScriptInstruction theContents result work))
		        (goto Fetch:)
		    ) ;; end begin
        ) ;; end if

	;;************************************************************************ 
	;; We are now processing an element tag.                        
       ;; Note: <Name ..attlist..> ...contents... </Name> are element tags. 
       ;;       <Name ..attlist../> are element tags without contents. 
	;;************************************************************************


    (if (= traceLinesOn true)(writeln "recContents: calling on recElement")) ;; LTN     
 
    (setq result (recElement theContents result work tagDepth))
    ;;(if (= traceLinesOn true)(writeln "recContents: after recElement: result is: " result)) ;; LTN
    (goto Fetch:)


    ;; ===================================================
    ;; Input Character is default
    ;; ===================================================
    Default::
 
	;; Record the first occurance of character contents.
    (setq start work.IP)
    (setq ip start)
    (if (= traceLinesOn true)(writeln "recContents: Default Label: start[0]:  " (char start[0]))) ;; LTN 
    (setq whitespace false)  
    (setq tempChar (new Vector: Byte: 50000))
    ;; check if there are character contents.. if there are do not promote the whitespace and evaluate
    ;; the character contents
    (vmregRunInHardware start:)
    (setq n 0)
    ;;(while (and (> (setq cc start[0]) nill) (<> cc #\<)) do 
    (while (and (<> (setq cc start[0]) #\<) (<> cc nill)) 
         (if (isNegative cc) (setq cc 0))   
         (setq saveCH cc)
         ;;(writeln " saveCH for tempChar is: " saveCH)
         (setq tempChar n saveCH)
         (setq tempChar (++ n) 0)
         (setq c1 start[1])
         ;; for doctypeheaders, once trailer is found, recContents terminates and
         ;; goes back to recDoctypeheader
         ;;(writeln "recContents: in doctypeheader: cc: " cc " c1: " c1 " doctypeFlag: " doctypeFlag )
         (if (and (= cc #\]) (= c1 #\>) (= doctypeFlag true))
            (begin
                (setq result tempChar)
                (setq work.IP (++ start))
                (goto Done:)
             ) ;; end begin
          ) ;; end if   
          (++ start)        
      ) ;; end while
      (vmregRunInHardware stop:)
      (resize tempChar (++ n))

      ;; temporarily commented this out
      ;;(if (= tempChar "") (++ start))

      (if (= traceLinesOn true)(writeln "recContents: inside Default label: tempChar: " tempChar) );; LTN      
      (if (= traceLinesOn true)(writeln "recContents: after getting tempChar: pointer is: " (char start[0]) (char start[1]) (char start[2]))) ;; LTN
      (if (isCharWhitespace tempChar)
          (begin
            (setq whitespace true)
            (setq work.IP (-- start))
            (if (= traceLinesOn true)(writeln "recContents:  Whitespace is set to TRUE: " ));; LTN
          ) ;; end begin
         else
         (begin
            (setq whitespace false)
            (if (= traceLinesOn true)(writeln "recContents: Whitespace is set to FALSE: " )) ;; LTN
            ;; possible debug
            ;;(setq work.IP ip)
         ) ;; end begin
      ) ;; end if

    ;; this previous implementation saves the contents between element tags
    ;; even if the contents just have whitespaces

    ;; only get content whitespace for whitespaces inside element tags
    ;;(if (compareEQ tagDepth 0)
        ;;(begin
        ;;      (vmregRunInHardware start:)
        ;;      (while (and (<> (setq cc start[0]) 0) (<> cc #\<)) do 
        ;;      (++ start)
        ;;      (begin 
        ;;          (writeln "recContents: here in setting whitespace to false " ) ;; LTN
        ;;          (setq whitespace false)
        ;;      );; end begin
        ;;else
        ;;(begin
        ;;     (writeln "recContents: here in setting whitespace to true " ) ;; LTN
        ;;     (setq whitespace true)
        ;;) ;; end begin
     ;;  ) ;; end if compareEQ 

    ;; Setting the String buffer for the content String
    (setq stringBuf (new String: ""))
    (resize stringBuf (++ len))	
 
	;; Make sure that any non-whitespace contents are saved in the Structure. 
	;; Note: the whitespace flag will tell us whether we have seen important data.  
	(if (= whitespace 0)
        (begin
		;; Manage the characters event according to the mode we're in.  
        ;; This code is transferred here to get the contents first to save in the Structure later
        ;; Note: we do this only if we are in document mode.  
	    ;; Convert the recognized char data into a string. 
             (setq stringBufp stringBuf)
             (setq stringBufp work.IP) 
             (if (= traceLinesOn true)(writeln "recContents:  Default Label:  whitespace is false:  " (char stringBufp[0]))) ;; LTN                 
			 (setq work.IP 0)                      
             (setq len 0)              
             (setq contentName (new Vector: Byte: 1000000))  
             (vmregRunInHardware start:)  
             (setq n1 0)           
             (while (and (<> (setq saveCH stringBufp[0]) 0) (<> saveCH #\<)) 
                 (setq contentName n1 saveCH)
                 (setq contentName (++ n1) 0)
                 (++ stringBufp)
              ) ;; end of while
              (vmregRunInHardware stop:)
              (resize contentName (++ n1))
              (setq theContents contentName)
              (if (= traceLinesOn true)(writeln "recContents  in Default Label theContents is: " theContents)) ;; LTN  

              ;; FOR STRING START
                    (if (= stemmingOn true)
                        (begin
                            (setq stemPointer theContents)
                            ;;(setq nn 0)
                            ;;(setq nn 0)
                            (while (> (setq cc stemPointer[0]) 0) do
                                 (setq stemVector nn cc)
                                 (setq stemVector (++ nn) 0)
                                 (++ stemPointer) 
                             ) ;; end while
                            (setq stemVector nn 32)
                            (if (= traceLines true)(writeln "stemVector is: " stemVector))
                            ;;(setq stemVector nn contentName)
                            (++ nn)
                         ) ;; end begin
                    ) ;; end if
                    ;; FOR STRING END
              ;;(writeln "recContents: stemVector: " stemVector)
              ;; temporarily commented out
              ;;(writeln "recContents  in Default Label work.IP reset to start[0] is: " start[0]) ;; LTN     
		      ;;(setq work.IP start)
                  
              (if (= work.eventHandler true)
			      (begin
			           ;; Send the characters message symbol to the eventAgent.  
			           ;; Note: we do this only if we are in event handler mode.   
			           ;; Convert the recognized char data into a string. 
                       (if (> (- start work.IP)0)
				            (begin
				                (work.eventAgent.characters theContents) 
                             ) ;; end begin
                        ) ;; end if
                    ) ;; end begin for work.eventHandler
        	       else
			       (begin
			            (setq work.IP start) 
                        (if (= traceLinesOn true)(writeln "recContents: eventHandler is false: work.IP gets start: " (char start[0]))) ;; LTN  
                       (if (= traceLinesOn true)(writeln "recContents: before modification and appending: result: " (type result)))
			            ;;Add the new content to the old content using the proper heuristic.  
                        ;; If the old content is empty, then just set the new content.  
			           (if (= #void result)(begin (setq result theContents)                       
                        ))
                          ;; added for bug
                         (if (= (isStructure result) false) (begin (setq result theContents)  ))
				
                        ;; If the old content is a null string, then just set the new content.
                        ;; possible debug for tempchar result to document
			            (if (and (isString result) (= result "") )(begin (setq result theContents) 
                        ;;(writeln "recContents in Default Label result before is a null string, now is: " result) ;; LTN
                        ))
				
                        (if (= traceLinesOn true)(writeln "recContents: before appending: result: " result))
                         
			            ;;If the old content is cdata, then append the new content.	 
                        ;;(if (not (isStructure result)) (begin (setq result (append result theContents)) (writeln " not structure: " result)))
                
                        ;; If the old content is a Structure, then insert the new content.  
		                ;; Note: If the last item inserted was character content,  
		                ;;  then append this content to it.  
                        (if (isStructure result)
                            (begin                                                        
                                 (setq INLEN (length result))
                                 (if (= traceLinesOn true)(writeln "recContents in Default Label: INLEN:  " INLEN)) ;; LTN 
                                 (if (> INLEN 0)
                                     (begin
                                          (setq name (ref result (sub1 INLEN) 0))
                                          (if (= traceLinesOn true)(writeln "recContents in Default Label result is a Structure: name is:  " name)) ;; LTN 
                                          (setq value (ref result (sub1 INLEN) 1))
                                          ;;(writeln "name type: " (type name))
                                          ;;(writeln "value type: " (type value))
                                          (if (= traceLinesOn true)(writeln "recContents in Default Label result is a Structure: value is:  " value)) ;; LTN 
                                          (if (and (isSymbol name) (compareEQ name "__content")
                                              (or (isString value) (isText value) (isByteVector value)))
                                              (begin
                                                  (setq theContents (append value theContents))
                                                  ;;(writeln "recContents in Default Label result is a Structure: result in if: value is:  " value)
                                                  (if (= traceLinesOn true)(writeln "recContents in Default Label result is a Structure: result in if: theContents is:  " theContents)) ;; LTN 
                                                  (setq tmp (symbol "__content"))
                                                  ;;(setq result name theContents)
                                                   ;;(writeln "delete result before: " result)
                                                  (delete result (sub1 INLEN))
                                                  ;;(writeln "delete result after: " result)
                                                  ;;(writeln "theContents before: " theContents)
                                                  ;;(writeln "tempChar: " tempChar)
                                                  (setq theContents (append (string theContents) (string tempChar)))
                                                   ;;(writeln "theContents after: " theContents)
                                                  (if (= traceLinesOn true)(writeln "recContents in default Label: after append theContents is: "  theContents))
                                                  (set result last:  name theContents)  
                                                  ;;(writeln "appended in the Default tag: " name " - " theContents )  ;; LTNTemp
                                                  ;;(writeln "actual last embedded tag: " result[(sub1 (length result)) 1]) ;; LTNTemp  
                                                                                              
                                                  ;;(writeln "recContents in Default Label result is a Structure: result in if: " result) ;; LTN 
                                                  (goto Fetch:)
                                               ) ;; end begin
                                           ) ;; end if
                                           (setq tmp (symbol "__content"))
                                           ;;(setq theContents (append theContents #\newline))
                                           (if (= traceLinesOn true)(writeln "recContents in Default Label result is a Structure: result outside if: " theContents))
                                           (set result last:  tmp theContents)
                                           ;;(writeln "appended in the  __content tag: " theContents) ;; LTNTemp
                                           ;;(writeln "actual last embedded tag: " result[(sub1 (length result)) 1]) ;; LTNTemp
                                            
                                            ;; FOR STRING
                                           
                                            
                                           ;;(writeln "recContents in Default Label result is a Structure: result outside if: " result) ;; LTN 
                           
                                    ) ;; end begin
                                    ;;else
                                    (begin
                                     (setq tmp (symbol "__content"))
                                                
                                                    
                                                 (set result last: tmp theContents)  
                                             
                                   ) ;; end begin
                                  ) ;; end if for INLEN
                              );; end begin
                        ) ;; end if isStructure
                   ) ;; end begin for else eventHandler
               ) ;; end if for eventHandler
           ) ;; end for begin for whitespace = false                    
           ;; Manage the ignorable whitespace event according to the mode we're in.  
           else
           ;; Here whitespace is TRUE
           (begin
               ;; Send the ignorableWhitespace message symbol to the eventAgent.  
		       ;; Note: we do this only if we are in event handler mode.  
               (if (= traceLinesOn true)(writeln "recContents: in Default label:  whitespace is true " ) );; LTN 
		       (if (= true work.eventHandler)
                   (begin
                       (if (> (- start work.IP) 0)
                          (begin                             
                              (work.eventAgent.ignorableWhitespace tempChar) 
                           ) ;; end begin
                        ) ;; end if
                    ) ;; end begin
                ) ;; end if
                (++ start)
            ) ;; end begin for else    
        );; end for whitespace is false

     ;; for Process Instruction bug
     (setq work.IP start)
     (if (= traceLinesOn true)(writeln "recContents: end of reContents processing: " (char start[0]) )) ;; LTN
     (goto Fetch:)
 
    ;; ===================================================
    ;; We have an input error
    ;; ===================================================
    Bad::
    (if (= errorCheckOn true)(xmlError "xml: invalid element contents" work))

    ;; ===================================================
    ;; We have no more input
    ;; ===================================================
 
    Done::
    ;;(if (= traceLinesOn true)(writeln "---recContents END output is: -- result  " result)) ;;  LTN
    (setq work.Document result)
 
     
    (if (= traceLinesOn true)(-- recContentsCtr)) ;; LTN
    (if (= traceLinesOn true)(writeln "decrementing recContents: " recContentsCtr) );; LTN
 
result) ;; end recContents





















;;**EXPORTKEY**:xml:recDoctypeHeader
(defchild xml:recDoctypeHeader(theContents result work tagDepth)
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
regs: ((CharPointer:start)                  ;; Temporary variable to store the position of the pointer
       (CharPointer:ip)                     ;; Temporary variable fore saving comment characters
       (Integer:cc)                         ;; Temporary input character
       (Integer:c1)
       (Integer:n 0)  
       (Integer:saveCH)                     ;; Temporary input character to save
       (Integer:len 0)
       (nill 0) (lessThan 60) (greaterThan 62) (_! 33) (leftbracket 91) (rightbracket 93)(space 32)
      )
vars:(name
      result 
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
(if (= traceLinesOn true)(writeln "recDoctypeHeader: arguments passed: result: " result)) ;; LTN 
(setq start work.IP)
(if (= traceLinesOn true)(writeln "recDoctypeHeader: argument passed: work.IP and start is:  " start[0])) ;; LTN 
(setq doctypeHeader (new Vector: Byte: 5000))

(setq cc start[0])
(setq c1 start[1])

(if (and (= cc lessThan)   (= c1 _!))
    (begin
	    ;; We have found a doctype header. 
	    ;; Promote the input pointer to the doctype tag name.  
	    (setq start (+ start 2))
        (setq work.IP start)
	    (if (= traceLinesOn true)(writeln "recDoctypeHeader: after doctype header: " (char start[0]))) ;; LTN 
	    ;; Try to recognize the doctype tag name.  
        (setq name (recName work))
        (if (= name #void) (setq name "dtd:unnamed"))
        (if (= traceLinesOn true)(writeln "recDoctypeHeader:  name is: " name)) ;; LTN
        (setq start work.IP)
        (if (= traceLinesOn true)(writeln "recDoctypeHeader: after recName: start gets work.IP " (char start[0])(char start[1]) (char start[2]))) ;; LTN 
 
	        
	    ;; Record the doctype contents. 
        (vmregRunInHardware start:) 	      
        (while (and (<> (setq cc start[0]) 0) (<> cc greaterThan) (<> cc #\[)  ) do
	          ;; ip variable saves the characters until the end of comment
              (setq ip start)
              (setq saveCH ip[0])
              ;;(writeln "recDoctypeHeader: saveCH in while is: " saveCH) ;; LTN
              (setq doctypeHeader n saveCH)
              (setq doctypeHeader (++ n) 0)
              ;;(setq doctypeHeader (append doctypeHeader (char saveCH)))
              ;; start variable ignores comment characters.             
              (++ start)                
       ) ;; end while
      (vmregRunInHardware stop:)

      (resize doctypeHeader (++ n))
      (if (= traceLinesOn true)(writeln "recDoctypeHeader: after PI contents: start: " (char start[0]))) ;; LTN 
      (if (= traceLinesOn true)(writeln "recDoctypeHeader:  doctypeHeader is: " doctypeHeader)) ;; LTN
    
      ;; Manage the doctype according to the mode we're in.
	  (if (= work.eventHandler true)
        (begin		 
		;;  Send the processingInstruction message symbol to the eventAgent.  
		;;  Note: we do this only if we are in event handler mode.		 
        (work.eventAgent.doctypeDefinition name doctypeHeader)
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
    (setq cc start[0])                      
    (while (and (> cc nill) (<= cc space)) (begin (++ start) (setq cc start[0])))
    (vmregRunInHardware stop:) 
    (setq work.IP start)
    (if (= traceLinesOn true)(writeln "recDocytypeHeader: after promote past whitespace: " (char start[0])));; LTN
    

	;; Is the document definition nested?  	
	(if (= (setq cc start[0])leftbracket)
		(begin 
           (if (= traceLinesOn true)(writeln "recDoctypeHeader: inside nested doctype header ")) ;; LTN
           (vmregRunInHardware start:)
		   (++ start)
           (setq cc start[0])                      
           (while (and (> cc nill) (<= cc space)) (begin (++ start) (setq cc start[0])))
           (vmregRunInHardware stop:) 
           (setq work.IP start)           
           (if (= traceLinesOn true)(writeln "recDoctypeHeader: nested doctype: after promoting whitespace is: " (char start[0]))) ;; LTN
           ;; testing --- promoting past whitespace
		   (while (and (<> (setq cc start[0] ) nill) (<> cc rightbracket)) do              
           ;;   (setq saveCH start[0])
           ;;   (writeln "recDoctypeHeader: saveCH1 in while is: " saveCH) ;; LTN
           ;;   (setq doctypeHeader (append doctypeHeader (char saveCH)))
              ;; start variable ignores comment characters.             
              (++ start)                
           ) ;; end while
          
           ;; Convert the recognized char data into a nested document type definition. 
           ;;(writeln "recDoctypeHeader: calling recContents ") ;; LTN 
           ;;(writeln "recDoctypeHeader: dtdContent before recContents: " dtdContent) ;; LTN 
           (setq doctypeFlag true)
           (setq dtdContent (recContents result work (+ tagDepth 1)))
           (if (= traceLinesOn true)(writeln "recDoctypeHeader: after calling recContents:  " dtdContent)) ;; LTN 
           (setq start work.IP)
           (setq cc start[0])
            
           (if (= traceLinesOn true)(writeln "recDoctypeHeader: start=cc=work.IP is: "  cc)) ;; LTN
		   (if (<> cc #\>) (if (= errorCheckOn true)(xmlError "xml: Document definition not closed" work)))
           ;;(++ start)
		
	       ;;(while (and (<> (setq cc start[0]) #\0)  (<> cc #\>) ) do
			;;  (++ start)
		   ;; )
            (if (= traceLinesOn true)(writeln "recDoctypeHeader: in nested DD start[0] is: " start[0])) ;; LTN
       ) ;; end begin
    ) ;; end if

  
	;; Is the document definition not closed?  	
	(if (<> (setq cc start[0]) greaterThan) (if (= errorCheckOn true)(xmlError "xml: Document definition not closed" work)))
	;; Promote the input pointer past the doctype trailer. 
    (++ start)          
    (vmregRunInHardware start:)
    (setq cc start[0])
    (while (and (> cc nill) (<= cc space)) (begin (++ start) (setq cc start[0])))
    (vmregRunInHardware stop:)
    (setq work.IP start)
   
    (return result)
   
   ) ;; end begin
) ;; end if


;; Return an XML invalid processing instruction error.  

Error:;
(if (= errorCheckOn true)(xmlError "xml: Expecting document type definition" work ))


#void) ;; end recDoctypeHeader





















;;**EXPORTKEY**:xml:recElement
(defchild xml:recElement(contents document work tagDepth)
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
;; Return:   result        The extended XML document in tree model form (if present).
;;
;; ******************************************************************************************
regs: ((CharPointer:ip)
       (CharPointer:start) 
       (CharPointer:contentp) 
       (Integer:saveCH) 
       (Integer:n 0) 
       (Integer:cc 0)
       (Integer:c1 0)
(Integer:start0 0) (Integer:start1 0) (Integer:start2 0)
       (Integer:comp)
       (space 32) (nill 0) (greaterThan 62) (lesserThan 60)(forwardSlash 47))
vars:(name
      result 
      tempCharFlag
      attributes 
     (ByteVector:tempChar)
      lastChar
      tempName
)


;; Initialize for speed
(setq traceLinesOn (boolean traceLinesOn))
(if (= traceLinesOn true)(writeln "---recElement START---  " )) ;;  LTN
;; For Data passing problem in recursive calls
(if (= traceLinesOn true)(writeln "possbile: content passed from recContents: " content)) ;; LTNTemp
(if (= traceLinesOn true)(writeln "possbile: document passed from recContents: " document)) ;; LTNTemp


(++ recElementCtr)
(if (= traceLinesOn true)(writeln "recElementCtr is: " recElementCtr)) ;; LTN
;;(writeln "recElementCtr is: " recElementCtr)
(++ work.IP)
(setq start work.IP)
(if (= traceLinesOn true)(writeln "recElement: start[0] for processing is: " start[0])) ;; LTN
  
;; Try to recognize the element tag name.
(setq name (recName work)) 
(if (= traceLinesOn true)(writeln "recElement: name after recName: " name)) ;; LTN
 
;; closing the structure for HTML tags without contents
(setq lastChar (ref name (- (length name) 1)))
(if (ccompareNE lastChar "/" )(begin (setq elementName name)) else (begin (-- work.IP)))
(if (= traceLinesOn true)(writeln "recElement: lastChar " lastChar)) ;; LTN 
(if (= traceLinesOn true)(writeln "recElement: elementName " elementName))
 
(if (isEqual name #void)  (if (= errorCheckOn true)(xmlError "xml: Expected element tag name" work)))
  
(if (<> name #void)
    (begin
        (vmregStringiCompare name "html"  comp)
        (if (= comp 0) then (setq work.htmlOn true))
    ) ;; end begin
) ;; end if

  
;;Promote input pointer past whitespace
(setq ip work.IP)
(setq cc ip[0])  
(if (= traceLinesOn true)(writeln "recElement: after recName work.IP is: " (char cc))) ;; LTN
(vmregRunInHardware start:)
(while (and  (<= cc space) (> cc nill)) do (++ ip)(setq cc ip[0]))(setq work.IP ip)
(vmregRunInHardware stop:)
(if (= traceLinesOn true)(writeln "recElement: after promoting past whitespace: " (char cc))) ;; LTN


;; Recognize attributes if any
(if (and (<> cc greaterThan) (<> cc forwardSlash))
   (begin
   ;;(setq work.IP start)
    (if (= traceLinesOn true)(writeln "recElement calling recAttributeList cc is: " (char cc))) ;; LTN
    (setq attributes (recAttributeList work))   
   ) ;; end begin   
) ;; end if
 
;;Manage the start element event according to the mode we're in. 
(if (= work.eventHandler true)
	(begin
	;;Send the startElement message symbol to the eventAgent. 
	;;Note: we do this only if we are in event handler mode.
    (work.eventAgent.startElement name attributes)  
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
(setq ip work.IP)
(setq cc ip[0]) 
(setq c1 ip[1])      
(if (= traceLinesOn true)(writeln "recElement: after recAttributes: ip and work.ip is the same: "  (char cc) (char c1))) ;; LTN 
 
(if (and (= cc forwardSlash)  (= c1 greaterThan)  )
  	;; There are no contents for this element tag. 
    (begin
         (setq ip (+ ip 2))
         (setq work.IP ip)
         (if (= traceLinesOn true)(writeln "recElement: end of element: no contents ip and work.ip is the same: "  (char ip[0]) (char ip[1]) (char ip[2]) (char ip[3])    )) ;; LTN    
         (if (= work.eventHandler true) 
             (begin
                 (work.eventAgent.endElement name)                  
              ) ;; end begin
              ;; work.eventHandler is false  
              else
              (begin
               ;; Note: the imbedded element will be assigned in the last position of the Structure. */
		       ;; Note: we do this only if we are in document mode. */
               ;; fix for the empty element bug
                
               (if (or (= attributes #void) (= attributes "")) 
                   (begin 
                       
                      (setq tempName (string name))
                      ;; fix for end of element bug /name
                      (setq tempName (left tempName (- (length tempName) 1)))                                     
                      (set document last: tempName true)
                   ) ;; end begin
                    else
                   (begin
                       (set document last: name contents)
                    ) ;; end begin
                ) ;; end if
                ;; double value bug
               (++ work.ip)
               ;;(setq document (append document #\newline)) 
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
        (if (= errorCheckOn true)(xmlError "xml: Expected end element tag" work))
     ) ;; end begin
) ;; end if

;;test


 (++ work.IP)
 
 
;;(setq contents (recContents contents work (+ 1 tagDepth)))

;; Get the next character contents of the evaluated XML Structure
(setq tempChar (new Vector: Byte: 5000))
 
;;Promote input pointer past whitespace if there are no character contents
(setq start work.IP)
(setq ip start)
(setq cc start[0])
(setq n 0)
(if (= traceLinesOn true)(writeln "recElement: tags with contents: not equal to > "  (char cc))) ;; LTN
(vmregRunInHardware start:)
(while (and (> (setq cc start[0]) 0) (<> cc #\<)) do      
     (setq saveCH start[0])
     (setq tempChar n saveCH)
     (setq tempChar (++ n) 0)
     (++ start)        
) ;; end while
(vmregRunInHardware stop:)
(resize tempChar (++ n))
(if (= traceLinesOn true) (writeln "recElement: tempChar is: " tempChar))
(if (isCharWhitespace tempChar)
    (begin
        (setq work.IP start)
        (if (= traceLinesOn true)(writeln "recElement: tags without contents: after value of tempChar: "  cc) );; LTN
     ) ;; end begin
     else
     (begin
         ;; for double value bug
        (setq work.IP ip)
        (if (= traceLinesOn true)(writeln "recElement: tags with contents: after value of tempChar: "  (char ip[0])))
       (if  (= (isStructure contents) false) (begin
        (setq contents tempChar) )) ;; LTN
     ) ;; end begin
) ;; end if

      
(setq contents (recContents contents work (+ 1 tagDepth)))

 
;; this is replaced by the while statements at the top
;;(setq cc ip[0])(while (and  (<= cc 32) (> cc 0)) do (++ ip)(setq cc ip[0]))(setq work.IP ip)
 

(if (= work.eventHandler false)
     (begin     
 
 

        (set document last: name contents) 
 
         
         
        ;;(writeln "actual last embedded tag: " document[(sub1 (length document)) 1]) ;; LTNTemp
         ;;(setq document (append document #\newline))
        ;; for ending the recElement recursive call from recContents
        (goto last:)
      ) ;; end begin
 ) ;; end if
 
;; recognize name of end of element
(setq start work.IP)
(setq start0 start[0])
(setq start1 start[1])
(setq start2 start[2])
  
(if (and (= start0  lesserThan)  (= start1 forwardSlash))
  	;; There are no contents for this element tag. 
    (begin
   (if (= traceLinesOn true)(writeln "recElement: start0 " start0 " start1: " start1))
         (if (= traceLinesOn true)(writeln "recElement: in end element tag name")) ;; LTN
         ;;(setq start work.IP)
         (setq work.IP (+ 2 work.IP))
         (setq name (recName work))
         (if (= traceLinesOn true)(writeln "recElement: for end tag name is:  " name)) ;; LTN        
         (if (= name #void) then (if (= errorCheckOn true)(xmlError "xml: Expected element tag name" work)))

         ;; An HTML end name never resets the HTML processing flag.  
         ;; Note: This is because there are so many errors in web  
	     ;;       HTML pages that we can never be sure that we are  
	     ;;      not required to parse loosely once we have seen  
	     ;;       the HTML start tag. */         
         (vmregStringiCompare name "html"  comp)
         (if (= comp 0) then (setq work.htmlOn true))
         ;;Promote input pointer past whitespace
         (vmregRunInHardware start:)
         (setq ip work.IP)
         (setq cc ip[0])
         (if (<> cc 62) (if (= errorCheckOn true)(xmlError "xml: Expected element end tag > symbol" work)))
         (if (= traceLinesOn true)(writeln "recElement: end of element: without contents: "  (char ip[0]))) ;; LTN 
         (setq cc ip[0])(while (and  (<= cc space) (> cc nill)) do (++ ip)(setq cc ip[0]))(setq work.IP ip)
        (++ work.IP)
        (vmregRunInHardware stop:)        
      ) ;; end begin
);; end if

 

last:: 
 
(if (= traceLinesOn true)(writeln "---recElement END--- document: "  document)) ;; LTN
(-- recElementCtr) ;; LTNTemp

(if (= traceLinesOn true)(writeln "Decrementing reCElement: " recElementCtr)) ;; LTN
;;(writeln "Decrementing reCElement: " recElementCtr)
document) ;; end recElement

#if 0

FXml_Element

This Function recognizes HTML element tag sections.
For example:


	<Name> ...content...</Name>


TVAL FXml_Element(LpXCONTEXT gCP,LpTHREAD gTP,TVAL theContents,TVAL theResult,XMLWork* work,NUM tagDepth)       
{
LpCHAR				header;
LpCHAR				start;
LpCHAR				namep;
CHAR				saveCH;
NUM					INLEN;
StartFrame
DeclareTVAL(tmp);
DeclareTVAL(name);
DeclareTVAL(ename);
DeclareTVAL(value);
DeclareTVAL(__pi);
DeclareTVAL(attributes);
DeclareTVAL(piAgent);
DeclareTVAL(result);
DeclareTVAL(contents);
DeclareTVAL(ec);
EndFrame

/* Set up the arguments in protected memory. */

Stack(contents) = theContents;
Stack(result) = theResult;

/*********************************************************************/
/* We are now processing an element tag.                             */
/* Note: <Name ..attlist..> ...contents... </Name> are element tags. */
/*       <Name ..attlist../> are element tags without contents.      */
/*********************************************************************/

++work->IP;	/* Promote the input pointer to the element tag name. */
Stack(contents) = gCP->Tval_VOID;

;; LN ...I am here...

/* Try to recognize the element tag name. */

Stack(name) = FXml_recName(gCP,gTP,work);
if (Stack(name).Tag == TYVOID) FrameExit(FXml_Error(gCP,gTP,"Expected element tag name",work));

/* An HTML tag name sets the HTML processing flag. */

if (FXml_strcmpi(gCP,gTP,FSmartbase_ObjectPtr(gCP,gTP,&Stack(name)),"html") == 0)
	{
	work->htmlOn = TRUE;
	}

SP		/* Promote input pointer past whitespace */
Stack(attributes) = gCP->Tval_VOID;
if ((*work->IP != '>') && (*work->IP != '/'))
	/* We need to recognize the element attributes. */
	{
	Stack(attributes) = FXml_AttributeList(gCP,gTP,work);
	ExitOnError(Stack(attributes));
	}

/* Manage the start element event according to the mode we're in. */

if (work->eventHandler)
	{
	/* Send the startElement message symbol to the eventAgent. */
	/* Note: we do this only if we are in event handler mode. */
	Stack(ec) = FSmartbase_SendMsg(gCP,gTP,gStartElement,work->eventAgent,2,Stack(name),Stack(attributes));
	ExitOnError(Stack(ec));
	}
else
	{
	/* Assign any attributes to the element Structure under the key: __attlist */
	/* Note: the attributes will be assigned in the first position of the Structure. */
	/* Note: we do this only if we are in document mode. */

	if (Stack(attributes).Tag != TYVOID)
		{
		Stack(tmp) = TSYMBOL("__attlist");
		Stack(contents) = FSmartbase_Eval(gCP,gTP,gNewStructure,2,Stack(tmp),Stack(attributes));
		ExitOnError(Stack(contents));
		}
	}

/* Are there no contents for this imbedded element tag? */
/* Note: <Name ..attlist../> are imbedded element tags without contents. */

if ((*work->IP == '/') && (*(work->IP+1) == '>'))
	/* There are no contents for this element tag. */
	{
	work->IP += 2;	/* Promote input pointer past /> symbols. */

	/* Manage the start element event according to the mode we're in. */

	if (work->eventHandler)
		{
		/* Send the endElement message symbol to the eventAgent. */
		/* Note: we do this only if we are in event handler mode. */
		Stack(ec) = FSmartbase_SendMsg(gCP,gTP,gEndElement,work->eventAgent,1,Stack(name));
		ExitOnError(Stack(ec));
		}
	else
		{
		/* Note: the imbedded element will be assigned in the last position of the Structure. */
		/* Note: we do this only if we are in document mode. */

		if (Stack(contents).Tag == TYVOID) Stack(contents) = gCP->Tval_TRUE;
		Stack(ec) = FSmartbase_Set(gCP,gTP,4,Stack(result),gLast,Stack(name),Stack(contents));
		ExitOnError(Stack(ec));
		}

	FrameExit(Stack(result));
	}

/* We are now processing an imbedded element tag with contents. */
/* Note: <Name ..attlist..> ...contents... </Name> are imbedded element tags. */

if (*work->IP != '>') FXml_Error(gCP,gTP,"Expecting element tag > symbol",work);
++work->IP;	/* Promote input pointer past the > symbol. */
SP		/* Promote input pointer past whitespace */

Stack(contents) = FXml_Contents(gCP,gTP,Stack(contents),work,tagDepth+1);
ExitOnError(Stack(contents));

if (work->eventHandler == FALSE)
	{
	/* Note: the contents will be assigned in the last position of the Structure. */

	Stack(ec) = FSmartbase_Set(gCP,gTP,4,Stack(result),gLast,Stack(name),Stack(contents));
	ExitOnError(Stack(ec));
	}


if ((*work->IP == '<') && (*(work->IP+1) == '/'))
	{
	/* Try to recognize the element end tag name. */

	start = work->IP; /* Save the input pointer for backtracking. */
	work->IP += 2;	/* Promote input pointer past the </ symbols. */
	Stack(ename) = FXml_recName(gCP,gTP,work);
	if (Stack(ename).Tag == TYVOID) FrameExit(FXml_Error(gCP,gTP,"Expected element end tag name",work));

	/* An HTML end name never resets the HTML processing flag. */
    /* Note: This is because there are so many errors in web */
	/*       HTML pages that we can never be sure that we are */
	/*       not required to parse loosely once we have seen */
	/*       the HTML start tag. */

	if (FXml_strcmpi(gCP,gTP,FSmartbase_ObjectPtr(gCP,gTP,&Stack(name)),"html") == 0)
		{
		work->htmlOn = TRUE;
		}

	SP		/* Promote input pointer past whitespace */
	if (*work->IP != '>') FrameExit(FXml_Error(gCP,gTP,"Expected element end tag > symbol",work));
	++work->IP;	/* Promote input pointer past the > symbol. */

	/* Make sure this is the proper end tag for this element. */

	if (FXml_strcmpi(gCP,gTP,FSmartbase_ObjectPtr(gCP,gTP,&Stack(name)),FSmartbase_ObjectPtr(gCP,gTP,&Stack(ename))) != 0)
		{
		work->IP = start;
		}
	}

/* Send the end element event according to the mode we're in. */

if (work->eventHandler)
	{
	/* Send the endElement message symbol to the eventAgent. */
	/* Note: we do this only if we are in event handler mode. */
	Stack(ec) = FSmartbase_SendMsg(gCP,gTP,gEndElement,work->eventAgent,1,Stack(name));
	ExitOnError(Stack(ec));
	}


/* Return the XML result object. */

FrameExit(Stack(result));

/* Return an XML invalid scripting instruction error. */

Error:
FrameExit(FXml_Error(gCP,gTP,"Expecting element tag",work));
}
#endif
   





















;;**EXPORTKEY**:xml:recName
(defchild xml:recName(work)
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
regs:(CharPointer:ip cc c1 (len 0) CharPointer:stringBufp (Integer:null 0))
regs:((_a #\a) (_z #\z) (_A #\A) (_Z #\Z) (underScore #\_) (space 32) (colon 58) (dash 45) (one 49) (nine 57) (period 46) (greaterThan 62))
vars:(String:stringBuf result)

  
(if (= traceLinesOn true)(writeln "---recName START---")) ;; LTN

;; Initialize a String object with enough space for a 1024 character name
(setq stringBuf work.stringBuf)
(setq stringBufp stringBuf)
(setq stringBufp[0] null)

;; An XML name must begine with an underscore _ or an alpha character
(setq ip work.IP)
(setq cc ip[0])
(setq c1 ip[1])
 

  
 
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
(++ ip)

(if (>= len 1020) (goto Bad:))

(setq cc ip[0])
  
(if (<= cc 32) (goto Good:))

(if (or (and (>= cc _a) (<= cc _z)) (and (>= cc _A) (<= cc _Z)) (and (>= cc one) (<= cc nine)) (= cc underScore) (= cc 48)(= cc 47) (= cc colon) (= cc period) (= cc 35)(= cc dash))    
    (begin
         (setq stringBufp[len] cc)
         (setq stringBufp[(+ len 1)] null)
         (goto TryAnotherChar:)
     )
)

Good::

(setq result (symbol stringBuf))
(setq work.IP ip)
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
         (++ ip)
         (setq cc ip[0])
         
       ) ;; end while
       (setq result stringBuf)
       (setq work.IP ip)
       (if (= traceLinesOn true)(writeln "---recName END--- result2: "  result)) ;; LTN
       (return result)
    ) ;; end begin
);; end if


(setq ip work.IP)
  
#void) ;; end recName

 


















;;**EXPORTKEY**:xml:recProcessInstruction
(defchild xml:recProcessInstruction(theContents result work)
;; ******************************************************************************************
;;  Summary: This Function recognizes XML processing instructions.
;; For example:
;;
;;             <?javaScript writeln("Hello World!"); ?>
;;
;; ******************************************************************************************
regs: ((CharPointer:namep)
       (CharPointer:start)                  ;; Temporary variable to store the position of the pointer
       (CharPointer:ip)                     ;; Temporary variable fore saving comment characters
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
(setq start work.IP)
(if (= traceLinesOn true)(writeln "recProcessInstruction: start: " (char start[0])))
(setq pInstruction (new Vector: Byte: 1000000))

(setq cc start[0])
(setq c1 start[1])
 
;; Check for an processing instruction tag.  
;; Note: <?Name ...contents... ?> is a process instruction tag. 
(if (and (= cc lessThan)(= c1 _?))
   (begin
         ;;(writeln "recProcessInstruction: inside if exp is a  PI " ) ;; LTN
	     ;;   Promote the input pointer to the PI tag name.
         (setq start (+ 2 start))  
         (setq work.IP start)  
         (if (= traceLinesOn true)(writeln "recProcessInstruction: after promotion of PI header: " (char start[0])));; LTN   
          
         ;; Try to recognize the PI tag name. 
         (setq name (recName work))
         (if (= traceLinesOn true)(writeln "recProcessInstructions: name is: " name)) ;; LTN 
         (if (= #void name) (if (= errorCheckOn true)(xmlError "xml: Expecting processing instruction target name" work)))
         (setq start work.IP)
         (if (= traceLinesOn true)(writeln "recProcessInstruction: after recognizing name start gets work.ip " (char start[0]))) ;; LTN  

          ;; Try to get the attribute list
		  ;; Promote input pointer past whitespace
          (vmregRunInHardware start:)
          (setq cc start[0])
          ;;(writeln "recProcessInstruction: in Less than end of element tag before while cc is: " cc) ;; LTN      
          (while (and (> cc nill) (<= cc space)) (begin (++ start) (setq cc start[0])))
          (vmregRunInHardware stop:)
          (if (= traceLinesOn true)(writeln "recProcessInstruction: after promoting past whitespace " (char start[0]))) ;; LTN
        
          (vmregRunInHardware start:)        
          (while  (and (<> (setq cc start[0]) nill) (or   (<> (setq cc start[0]) _?) (<> (setq c1 start[1]) greaterThan)  )) do
             ;; Saves the characters
             (setq ip start)
             (setq saveCH ip[0])             
             (setq pInstruction n saveCH)
             (setq pInstruction (++ n) 0)
             ;; Promote the pointer
             (++ start)            
         ) ;; end while          
         (vmregRunInHardware stop:)
         (resize pInstruction (++ n))
	 
         (if (= traceLinesOn true)(writeln "recPI: " pInstruction)) ;; LTN
         (if (= traceLinesOn true)(writeln "recProcessInstruction: after getting pi contents: " (char start[0]))) ;; LTN

	     ;; Is the processing instruction not closed? */
	     (if (<> (setq cc start[0]) _?)
		    (begin
		       (setq work.IP start)
               (if (= traceLinesOn true)(writeln "recProcessInstruction: error: pi not closed: " (char start[0]))) ;; LTN
		       (if (= errorCheckOn true)(xmlError "xml: Processing instruction not closed" work ))
		    );; end begin
        ) ;; end if

	    ;; Promote the input pointer past the PI trailer.
        (if (<> cc nill) 
           (begin 
            ;; (writeln "recProcessInstruction: inside if: cc is: "  cc)
             (setq start (+ 2 start))))
             (setq work.IP start)
             (if (= traceLinesOn true)(writeln  "recProcessInstruction: after past PI trailer: " (char start[0]))) ;; LTN

	     ;; Manage the process instruction according to the mode we're in.  
        (if (= true work.eventHandler)
            (begin
	           ;; Send the processingInstruction message symbol to the eventAgent. 
		       ;; Note: we do this only if we are in event handler mode.                
               (work.eventAgent.processingInstruction name pInstruction)
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
               
               (setq work.piStructure (makeStructure name: pInstruction))            
               (set result  pi tmp )
           		 
		       ;;  Are we processing any process instructions?  
               (if (= true work.piHandler)
			      (begin 
			          ;; Are we processing this process instruction? */
			          (setq work.piAgent (ref work.piStructure name)) 			 
			          (if (= #void work.piAgent)
				          (begin
                              (eval (work.piAgent work.piStructure work.document result theContents))
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
(if (= errorCheckOn true)(xmlError "xml: Expecting processing instruction" work))

#void)
 




















;;**EXPORTKEY**:xml:recScriptInstruction
(defchild xml:recScriptInstruction(theContents result work)
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
       (CharPointer:start)                  ;; Temporary variable to store the position of the pointer
       (CharPointer:ip)                     ;; Temporary variable fore saving comment characters
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
      result
      contents 
      ename
      attributes 
      (ByteVector:script) 
     tmp
)   


 
;; Check for a SCRIPT tag which has meaning in HTML mode.       
;; Note: <script ...contents... <script> are HTML script tags.  

(if (= traceLinesOn true)(writeln "---recScriptInstruction START---") );; LTN 
;;(writeln "recScriptInstruction: type of result is: "  (type result))
(setq start work.IP)
(if (= traceLinesOn true)(writeln "recScriptInstruction: start: " (char start[0]))) ;;LTN 
(setq script (new Vector: Byte: 10000))
  
;;(setq cc start[0])
;;(setq c1 start[1])
;;(setq c2 start[2])
;;(setq c3 start[3])
;;(setq c4 start[4])
;;(setq c5 start[5])
;;(setq c6 start[6])
  

  (if (and (= work.htmlOn true)
                (or (= (setq cc start[1]) #\S) (= cc #\s))
                (or (= (setq cc start[2]) #\C) (= cc #\c))
                (or (= (setq cc start[3]) #\R) (= cc #\r))
                (or (= (setq cc start[4]) #\I) (= cc #\i))
                (or (= (setq cc start[5]) #\P) (= cc #\p))
                (or (= (setq cc start[6]) #\T) (= cc #\t))
           )

    ;;  We have found an HTML script tag. 
	(begin
        ;; Promote the input pointer to the script tag. */
	    (setq start ( + start 7))	
        (if (= traceLinesOn true)(writeln "recScriptInstruction: after script header tag: " (char start[0]))) ;;LTN
        (setq name "script")
	    ;;  Promote input pointer past whitespace     
        (vmregRunInHardware start:)
        (setq cc start[0])
        (if (= traceLinesOn true)(writeln "recScriptInstruction: in Less than end of element tag before while cc is: " cc)) ;; LTN      
        (while (and (> cc nill) (<= cc space)) (begin (++ start) (setq cc start[0])))
        (vmregRunInHardware stop:)
        (if (= traceLinesOn true)(writeln "recScriptInstruction: after promoting past whitespace: " (char start[0])(char start[1]) (char start[2]))) ;;LTN
        ;; Stack(contents) = gCP->Tval_VOID;
        (setq work.IP start)
        ;; We need to recognize the script attributes.  
        (setq attributes (recAttributeList work))
        (setq start work.IP)
	    (if (= traceLinesOn true)(writeln "recScriptInstruction: after recAttributeList " (char start[0]))) ;;LTN
        (setq cc start[0])
        ;; Check the end of element
        (if (<> cc greaterThan) (if (= errorCheckOn true)(xmlError "xml: Expecting element tag > symbol" work)))
	 
        ;; Promote input pointer past the > symbol.  
	    (++ start)
        (if (= traceLinesOn true)(writeln "recScriptInstruction: after > symbol " (char start[0]))) ;;LTN
	    ;; Manage the start element event according to the mode we're in.  
        (if (= true work.eventHandler)
		    (begin
		        ;; Send the startElement message symbol to the eventAgent.  
		        ;; Note: we do this only if we are in event handler mode.  		        
                 (work.eventAgent.startElement name attributes)
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

		;;  Record the script contents. 
	   (setq start work.IP)
       (++ start)
  
       (vmregRunInHardware start:)
       (while (or  
                (and (<> (setq cc start[2]) #\S) (<> cc #\s))
                (and (<> (setq cc start[3]) #\C) (<> cc #\c))
                (and (<> (setq cc start[4]) #\R) (<> cc #\r))
                (and (<> (setq cc start[5]) #\I) (<> cc #\i))
                (and (<> (setq cc start[6]) #\P) (<> cc #\p))
                (and (<> (setq cc start[7]) #\T) (<> cc #\t))
                ) 

           do
           ;;(writeln "recScriptInstruction before getting script: " (char cc) (char c1) (char c2) (char c3) (char c4) (char c5) (char c6) (char c7))
            (setq ip start)
            (setq saveCH ip[0])
            ;;(setq script (append script (char saveCH)))
            (setq script n saveCH)
            (setq script (++ n) 0)
            ;; Promote the pointer
            (++ start)
       ) ;; end while
       (vmregRunInHardware stop:)
       (resize script (++ n))
       (if (= traceLinesOn true)(writeln "recScriptInstruction: script: " script)) ;; LTN
       (if (= traceLinesOn true)(writeln "recScriptInstruction: after getting script start is: " (char start[0]) (char start[1]) (char start[2]))) ;; LTN
       ;;  Manage the character data according to the mode we're in. */

	  (if (= work.eventHandler true)
		(begin
		    ;; Send the characters message symbol to the eventAgent.  
		    ;;  Note: we do this only if we are in event handler mode.  		     
            (work.eventAgent.characters script)
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
        (setq work.IP start)

	       ;;  Try to recognize the script end tag name.  
           (if (and (= cc lessThan) (= c1 forwardSlash))  
		      (begin
                 ;; Promote input pointer past the </ symbols.  
		         (setq start (+ start 2))
                 (if (= traceLinesOn true)(writeln "recScriptInstruction: end tag name: " (char start[0]))) ;; LTN
                 (setq work.IP start)
		         (setq ename (recName work))
                 (if (= traceLinesOn true)(writeln "ename after recName: " ename)) ;; LTN
                 (if (= ename #void) (if (= errorCheckOn true)(xmlError "xml:Expecting end script tag" work))) 
		         ;;  An HTML or XML name resets the HTML processing flag.  
                 (if (= name "html")
			        (begin
                        (setq work.htmlOn false)
                    ) ;; end begin
                 ) ;; end if
			 
                 ;; Promote input pointer past whitespace  
                 (setq start work.IP) 
                 (vmregRunInHardware start:)
                 (setq cc start[0])
                 (if (= traceLinesOn true)(writeln "recScriptInstruction: after recName: " (char cc))) ;; LTN
                 ;;(writeln "recScriptInstruction: in Less than end of element tag before while cc is: " cc) ;; LTN      
                 (while (and (> cc nill) (<= cc space)) (begin (++ start) (setq cc start[0])))
                 (vmregRunInHardware stop:)
                 (if (= traceLinesOn true)(writeln "recScriptInstruction: after promoting whitespace: " (char start[0]))) ;; LTN
		 
		         (if (<> cc greaterThan) (if (= errorCheckOn true)(xmlError "xml: Expecting end script > symbol" work)))  
		         (++ start)
                 ;; Promote past whitespace
                 (vmregRunInHardware start:)
                 (setq cc start[0])
                 (while (and (> cc nill) (<= cc space)) (begin (++ start) (setq cc start[0])))
                 (vmregRunInHardware stop:)
                 (if (= traceLinesOn true)(writeln "recScriptInstruction: after > symbol: " (char start[0]))) ;; LTN

	             (if (= ename "script") (setq work.IP start)) 
             ) ;; end begin
         ) ;; end if

	    ;;  Send the end element event according to the mode we're in. 
       (if (= work.eventHandler true)
		    (begin
               ;;  Send the endElement message symbol to the eventAgent.  
		       ;;  Note: we do this only if we are in event handler mode. 	     
              (work.eventAgent.endElement name)
		     ) ;; end begin
       ) ;; end if work.eventHandler
      ) ;; end begin
     ) ;; end if
    (if (= traceLinesOn true)(writeln "--recScriptInstruction END --" )) ;; LTN
    (return result)
   
;; Return an XML invalid scripting instruction error. */

Error::
(if (= errorCheckOn true)(xmlError "xml: Expecting scripting instruction" work))
 
#void) ;; end recScriptInstruction





















;;**EXPORTKEY**:xml:recValue
 (defchild xml:recValue(work)
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
regs:((CharPointer:ip) 
      (Integer:cc) 
      (len 0) 
      (Integer:n 0) 
      (CharPointer:stringBufp) 
      (CharPointer:start)
      (CharPointer:ipNext)  
      (Integer:pp)
      (CharPointer:ipNextNext) 
      (Integer:ppp)
      (singlequote 39) (doublequote 34) (_0 48) (_9 57) (_A 65) (_a 97) (_Z 90) (_z 122) (underscore 95))
vars: (result 
      (ByteVector:valueName) 
      stringBuf)


(if (= traceLinesOn true)(writeln "---recValue START---" ) );; LTN
;; Initialize the objects
(setq ip work.IP)
(setq cc ip[0])
(setq stringBufp ip)
(if (= traceLinesOn true)(writeln "recValue: start: " (char cc)) );; LTN

;;  An XML symbol value may begin with a single quote character. 
(if (= cc singlequote)
    (begin
       (if (= traceLinesOn true)(writeln "recValue: in singlequote " )) ;; LTN
       (setq valueName (new Vector: Byte: 5000))
       (setq n 0)
       (setq len 0)
       ;;(setq stringBufp[len] 0)
       (setq ipNext (++ ip))
       (setq pp ipNext[0])
       (if (= traceLinesOn true)(writeln "recValue: singlequote: ipNext is: " (char pp))) ;; LTN
       (vmregRunInHardware start:)
       (while (<> (setq pp ipNext[0]) singlequote) 
           ;;(writeln "recValue: in singlequote pp to be appended is: " pp) ;; LTN
           (if (>= len 1020)(if (= errorCheckOn true)(xmlError "xml: Symbol constant too long!" work)))
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
        (setq work.IP (++ ipNext)) 
        (setq start work.IP) ;; LTN for debugging only
        (if (= traceLinesOn true)(writeln "recValue: singlequote: after getting value name: " (char start[0]) )) ;; LTN
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
       (setq ipNext (++ ip))
       (setq pp ipNext[0])
       (if (= traceLinesOn true)(writeln "recValue: doublequote: ipNext is: " (char pp))) ;; LTN
       (vmregRunInHardware start:)
       (while (<> (setq pp ipNext[0]) doublequote) do
            ;;(writeln "recValue: in doublequote pp to be appended is: " pp) ;; LTN
           (if (>= len 1020)(if (= errorCheckOn true)(xmlError "xml: String constant too long!" work)))
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
        (setq work.IP (++ ipNext))
        (setq result valueName)
        (setq start work.IP) ;; LTN for debugging only
        (if (= traceLinesOn true)(writeln "recValue: doublequote: after getting value name: " (char start[0]) )) ;; LTN
       ;; (writeln "---recValue END--- result: " result) ;; LTN
        (return result)
    ) ;; end begin
);; end if

;;  An XML symbol value may begin with a digit
(if (and (>= cc _0) (<= cc _9))
    (begin
       (if (= traceLinesOn true)(writeln "recValue: digit: ip[0] is: " (char ip[0]))) ;; LTN
       (setq valueName (new Vector: Byte: 5000))
       (setq n 0)
       ;;(setq len 0)
       ;;(setq stringBufp[len] 0)
       ;;(setq valueName (append valueName (char cc)))
       (setq valueName n cc)
       (setq valueName (++ n) 0)
       (setq ipNext (++ ip))        
       (setq pp ipNext[0])
       (if (= traceLinesOn true)(writeln "recValue: digit ipNext is: " (char pp))) ;; LTN

       (vmregRunInHardware start:)
       (while (or (and (>= (setq pp ipNext[0]) _0) (<= pp _9)) (= pp 37))
           ;;(writeln "recValue: in digit pp to be appended is: " pp) ;; LTN
           (if (>= len 1020)(if (= errorCheckOn true)(xmlError "xml: Numeric constant too long!" work)))
           ;;(setq valueName (append valueName (char pp)))      
           (setq valueName n pp)
           (setq valueName (++ n) 0)          
           (++ ipNext)
           (setq work.IP ipNext)           
        ) ;; end while
        (vmregRunInHardware stop:)

        (resize valueName (++ n))

;;(setq valueName (symbol valueName))
        (if (= n 2) (setq work.IP ipNext))
        (if (= traceLinesOn true)(writeln "recValue: valueName in digit is: " valueName)) ;; LTN     
        (setq start work.IP) ;; LTN for debugging only
        (if (= traceLinesOn true)(writeln "recValue: digit: after getting value name: " (char start[0]) )) ;; LTN                    
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
        (setq result (recName work))
        (setq start work.IP) ;; LTN for debugging only
        (if (= traceLinesOn true)(writeln "recValue: alpha: after getting value name: " (char start[0]) )) ;; LTN   
        ;;(writeln "---recValue END--- result: " result) ;; LTN
        (return result)
    ) ;; end begin
);; end if

;;  Any other HTML attribute value ends with a whitespace character, or ! ? , >. 
(if (compareEQ work.htmlOn true)
    (begin
       (if (= traceLinesOn true)(writeln "recValue: other value " )) ;; LTN
       (setq valueName (new Vector: Byte: 5000))
       (setq n 0)
       ;;(setq len 0)
       ;;(setq stringBufp[len] 0)
       (setq ipNext (++ ip))
       (setq pp ipNext[0])
       (setq ipNextNext (++ ipNext))
       (setq ppp ipNexNext[0])
       (if (= traceLinesOn true)(writeln "recValue: other value: ipNext is: " (char pp))) ;; LTN
       (if (= traceLinesOn true)(writeln "recValue: other value: ipNextNext is: " (char ppp))) ;; LTN
       (vmregRunInHardware start:)
       (while (and (> (setq ppp ipNextNext[0]) 32) (<> (setq pp ipNext[0]) 33) (<> pp 63) (<> pp 44) (<> pp 62))
           (if (>= len 1020)(if (= errorCheckOn true)(xmlError "xml: Attribute constant too long!" work)))
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
        (setq start work.IP) ;; LTN for debugging only
        (if (= traceLinesOn true)(writeln "recValue: other value: after getting value name: " (char start[0]) )) ;; LTN          
        (setq result valueName)
        ;;(writeln "---recValue END--- result: " result) ;; LTN
        (return result)
    ) ;; end begin
);; end if


#void)















;;**EXPORTKEY**:xml:xmlError
(defchild xml:xmlError(errorString work)
;; ******************************************************************************************
;;  Summary: Return a parsing error in an XML source string. The error is returned
;;   in a standard format.
;;
;;
;; Return:   result        The extended XML document in tree model form (if present).
;;
;; ******************************************************************************************
regs:((CharPointer:start)                   ;; Temporary variable to store the starting pointer
      (CharPointer:current)                 ;; Temporary variable to store the starting pointer
      (Integer:pos)
      (Integer:endline1 0)
      (Integer:endline2 0)
      (Integer:endline3 0)
      (Integer:i 0)
      (Integer:n 0)
      (Integer:errorPos 0)
      (Integer:cc)                         ;; Temporary input character

       (CharPointer:ip)                     ;; Temporary variable for saving error characters
       
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
(setq start work.start)  
(setq current work.IP)      
(setq pos (-  current start))
;;(setq eof work.INLEN)
(setq tempLine "")
(setq line1 "") 
(setq inputString (new Vector: Byte: 5000))
 

;; extract the input string until the error point 
(while (and (<> (setq cc start[0]) 0) (< start current)) do  
     ;; if newline is encountered
     (if (or (= cc 13) (= cc 10))
          (begin
              (setq errorPos 0)
              (++ i)
              (setq  saveCH start[0])                
              ;;(setq inputString (append inputString (char saveCH))) 
              (setq inputString n saveCH)
              (setq inputString (++ n) 0)
              (setq line1 tempLine)
              ;;(writeln "xmlError: line1 is: " line1 ) ;; LTN
              (setq tempLine "")    
              (++ start)
          ) ;; end begin
          else
         (begin
              (++ i)
              (setq  saveCH start[0])
              ;;(writeln "xmlError: ch appended to templine in normal: " (char saveCH)) ;; LTN              
              ;;(setq inputString (append inputString (char saveCH)))
              (setq inputString n saveCH)
              (setq inputString (++ n) 0)
              (setq tempLine (append tempLine (char saveCH)))     
              (++ start)
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
(if (= work.eventHandler true)
    (begin
    ;; Send the errorMessage message symbol to the eventAgent.  
    ;; Note: we do this only if we are in event handler mode.  
       (work.eventAgent.errorMsg errorString)        
    ) ;; end begin
) ;; end if

(if (= traceLinesOn true)(writeln "---xmlError END --- " ) );; LTN 

#void) ;; end xmlError




















