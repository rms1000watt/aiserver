;;*************************************
;;*************************************
;; Exported Lambda File Cabinet Document
;;*************************************
;;*************************************

;;**EXPORTKEY**:IndexRefGuide
(defun IndexRefGuide()


;;************************************************************************
; INDEXREFGUIDE 
;;***************************************************************************
;; Algorithm used: 
;; An implementation of Vector Space Search Engine
;;
;; Each word is represented by an axis in the vector space.
;; Each document is treated as a vector, where each coordinate is represented by the frequency of a word
;;
;; For example:
;;		Suppose the universal U set of documents consists only of 3 words: "cat", "dog", "mouse"
;;		Therefore, U is represented as a 3-tuple ("cat", "dog", "mouse"); each word is an axis in the vector space
;;		Vector (3,1,4) for a document X, will mean that at document X "cat" has a frequency of 3; "dog" has frequency of 1; "mouse" has frequency of 4.
;;		
;; The magnitude for each vector is computed using Phytagorean theorem.
;; If a search word is an element of U, then it can be represented as a unit vector. 
;;		For example, the search word "cat" will be represented as (1,0,0)
;;
;; The cosine of the angles formed by the search word unit vector and each of the document vectors are compared. 
;; The smaller the cosine of the angle, the more prioritized is the document that will be returned during a search.
;;
;; Data Structures Generated
;;
;;************************************************************************

pvars:( 		
		 myCurrentDocXML				;; XML Version of Webpage   		  			 
		(Integer:pTotalDocNum)			;; current document evaluated
		(Integer:pWordCount)
		(myOntology "CoreContent")   	;; Where Documents are found    
		 ontologyNames 					;; Vector of Document names in CoreContent		 
		;; Child Lambdas	
		generateLexicon					;; Lambda generating preparatory Repositories and files
		defaultLexer					;; Lambda separating the documents into words and its lexemes
		generateMatrix					;; Lambda generating document x query matrix from information
										;; taken from generateLexicon in prepartion for the query function
		searchRefGuide
		queryHtml
		isString
		xmlSocketEvals
		wordWeights
		queryHtml
		(Integer:wordCount 0)
	 
) ;; end pvars
vars:(tempDir
 (myHtmlOutput "AnswerDetails.html"))


;;================
;; MAIN LOGIC
;;================
   ;; Respond to a simple query with an HTML answer page.
   ;; Note: Can be used by an Alice client (performs initialization and cleanup).
   (defun queryHtml(queryString ...)
      vars:(n N answer htmlPage answer 
            htmlExplanation oldExplanation 
            pageName wordVector 
            webText webTextLen topic
            docVec htmlWriteText 
 (myHtmlOutput "AnswerDetails.html")
            ) ; end temporary variables
      regs:((Integer:numDocs)
            (Integer:iNumDocs)
            (Integer:docIndex))
      ;   (browseLib.setFocus "wwwroot")
 (browseLib.setFocus "CoreContent")
      (setq ontologyNames (^new Vector: 0))
      (setq ontologyNames (new Vector: 5 "DocOne" "ltn" "DocThree" "DocFour" "DocFive")) 
      ;(browseLib.setFocus "CoreContent") ;; MFK ...testing...
      ;(setq ontologyNames (browseLib.getChildNames)) ;; MFK ...testing...
      (if (= (trim queryString) "") (setq queryString "Help?"))
      ;(setq docVec (searchRefGuide queryString)) ;; MFK ...Testing...
      (setq docVec (new Vector: 3 1 100 6)) ;; MFK ...Testing...
 (writeln "queryString: " queryString)
      ;; From here we create an HTML answer page and write it to the disk,
      ;; then we return the HTML page URL so the Web client can load it.
      (setq htmlPage (mid (browseLib.checkout "ResultDetails") 7 10000))
      (setq numDocs (length docVec))
      (setq iNumDocs (- numDocs 1))
      ;; Create a list of the document names and its links
      (setq answer (new Vector: Byte: 100000))
      (loop for docIndex from 0 to iNumDocs
         (appendWriteln answer (string ontologyNames[docIndex]) {.xml, })
         )
      (setq myHtmlResponseHeading (append " TOTALDOCS == " (string numDocs)))
      (setq myHtmlResponseResults "")
 
      ;; From here we create an HTML answer page and write it to the disk,
      ;; then we return the HTML page URL so the Web client can load it.
      (setq answer (string answer true))
      (if (and (= (left answer 1) {"})(= (right answer 1) {"})) (setq answer (mid answer 1 (- (length answer) 2))))
      (setq htmlPage (substitute htmlPage "<!--$$$QUERYSTRING$$$-->" queryString))
      (setq htmlPage (substitute htmlPage "<!--#MyTextResponse#-->" answer))
      (if (isString myHtmlResponseHeading) (setq htmlPage (substitute htmlPage "<!--#MyHTMLResponseHeading#-->" myHtmlResponseHeading)))
      (if (isString myHtmlResponseResults) (setq htmlPage (substitute htmlPage "<!--#MyHTMLResponseResults#-->" myHtmlResponseResults)))
      (browseLib.writeSourceFile (append _AliceWebPath myHtmlOutput) htmlPage) 
 ;(browseLib.writeSourceFile (append "wwwroot/" myHtmlOutput) htmlPage) 
      myHtmlOutput) ;; end queryHtml
  
   ;; Execute a lisp command sent over the remote XML socket interface.
   ;; Note1: This child Lambda registers Alice as remotely executable over
   ;;        the TCP/IP connection by any AIS client using the AMP protocol.
   ;; Note2: The XML message is transmitted in the proprietary Lambda
   ;;        Information Server Lambda Message Protocol (AMP) format.
   (defun xmlSocketEvals(msg)
      faces:(;; Child Lambda Interface specifications.
             (public Amp:)  ;; Can be executed by AIS AMP
             (security 0)   ;; No or lowest level security required
             ) ;; end Interface specifications
      vars:(result)	
      (setq result (new Structure: result:((compile (morph (lisp msg.cmd))))))
      result) ;; end xmlSocketEvals
             
   ;; Returns true if the argument is a String or a ByteVector
   (defun  isString(word) (or (|Gv:isString| word) (isByteVector word)))
   ;; Merge an existing AIML model with a newly compiled Directory of pattern phrases.        
     

(writeln "GENERATE LEXICON")
(browseLib.setFocus myOntology)
(setq ontologyNames (^new Vector: 0))
(setq ontologyNames (browseLib.getChildNames))
(setq pTotalDocNum  (length ontologyNames) )
(writeln "pTotalDocNum: " pTotalDocNum)

(IndexRefGuide.generateLexicon)

;(IndexRefGuide.generateMatrix)
 ;; count number of indexed docs , may be modified
 
;(setq wordREPOS (new ObjectRepository: "wordREPO.db")) 
;(setq freqREPOS (new ObjectRepository: "freqREPOS.db")) 
;(beginTransaction wordREPOS)
;(beginTransaction freqREPOS)

#if 0
(setq tempDir (ref wordREPOS (- (length wordREPOS) 1) 1) )

(setq pWordCount (refDirValue tempDir (- (length tempDir) 1))) 
(++ pWordCount)
(writeln "pWordCount: " pWordCount)
(commitTransaction wordREPOS)
#endif

(writeln "GENERATE MATRIX")
;(IndexRefGuide.generateMatrix)
;(commitTransaction wordREPOS)
;(commitTransaction freqREPOS)

 
) ;; end indexRefGuide
 



;;**EXPORTKEY**:IndexRefGuide:%ResultDetails
;#text#

<HTML>
<HEAD><TITLE>Alice: Answer Details</TITLE></HEAD>
<!--Page Parameters -->
<BODY BGCOLOR="#FFFFF0" TEXT="#000000" LINK="#0000ff">
<BGSOUND SRC="Dong.wav" LOOP=1 AUTOSTART=true>
<!--------------------------------------------------------------------------------->
<!---                       Scripts for Control Mnagement                      ---->
<!--- Note: These scripts are needed only for the Developer's Only version     ---->
<!--------------------------------------------------------------------------------->
<script language="JavaScript">
var EventBlockCount = 0;		// Counter to block stacked events. See messageBar function docs below.

// -------------------------------------------------------------------------------
// Summary:  Decode all XML special characters in the specified string.
// Args:     iMsg:         The string whose XML special characters are to be decoded.
// Return:   result:       The string with its XML special characters decoded.
// -------------------------------------------------------------------------------
function decodeXml(iMsg) {
	// Convert predefined entities, &lt;, &amp;, &gt;, &apos;, &quot;
	var aRet = iMsg.replace(/&quot;/g, '"');
	aRet = aRet.replace(/&lt;/g, "<");
	aRet = aRet.replace(/&gt;/g, ">");
	aRet = aRet.replace(/&apos;/g, "'");
	// Do & last!
	aRet = aRet.replace(/&amp;/g, "&");
	return aRet;
}

// -------------------------------------------------------------------------------
// Summary:  Encodes the <, &, and " special XML characters in the specified string.
// Args:     iMsg:         The string whose <, &, and " special XML characters are to be decoded.
// Return:   result:       The string with its <, &, and " XML special characters decoded.
// Note:     Encode all strings w/ special chars before including them in an XML doc.
// -------------------------------------------------------------------------------
function encodeXml(iMsg) {
	// Do & first!
	var aRet = iMsg.replace(/&/g, "&amp;");
	aRet = aRet.replace(/</g, "&lt;");
	aRet = aRet.replace(/"/g, "&quot;");
	return aRet;
}

// -------------------------------------------------------------------------------
// Summary:  Evaluate the command expression in the SmartBase engine.
// Args:     command:      The source expression to be evaluated.
// Return:   result:       The result of evaluating the expression.
// -------------------------------------------------------------------------------
function Evals(command) {
	//return top.scripts.Evals(command);
    var cmd = "xml=<amp target='alice' act='xmlSocketEvals'><cmd>" + encodeXml(command) + '</cmd></amp>';
	var buffer = "";
    if (window.XMLHttpRequest) { // branch for native XMLHttpRequest object
        req = new XMLHttpRequest();
        req.open("POST", "amp.dll", false); // load sync!
		//alert("cmd=" + cmd);
        req.send(cmd);
    } else if (window.ActiveXObject) { // branch for IE/Windows ActiveX version
        req = new ActiveXObject("Microsoft.XMLHTTP");
        if (req) {
            req.open("POST", "amp.dll", false); // load sync!
            req.send(cmd);
        }
    }

	if(req.status == 200) {
		var buffer = req.responseText;
	} else {
		alert("Evals failed"); 
	}

    // Extract just the result information from the returned string
    buffer = buffer.slice(buffer.indexOf("<result>") + 8,buffer.lastIndexOf("</result>"))
    return (buffer);
}


// -------------------------------------------------------------------------------
// Summary:  Display the current Alice response in the main window viewer.
// Args:     question:		The argument must be a question for Alice.
// Return:     switch: 		Returns true if record loaded; otherwise, returns false.
// -------------------------------------------------------------------------------
function loadDetails(question) {
	var command = "";
	var result = "";

	top.main.navigate("PleaseWait.html");

	// Load the record details into the detail record viewer.
	command = "(alice.queryHtml {" + question + "})";
	result = Evals(command);
	top.main.navigate(result);

	// Brick details have been successfully loaded.
	return true;
} 
        
</script>
<!--------------------------------------------------------------------------------->
<!---                     End Scripts for Control Mnagement                    ---->
<!--------------------------------------------------------------------------------->

<TABLE>
<TR>

<TD>
	<TABLE><TR><TD>
	<TEXTAREA NAME="IndexRefGuide" ALIGN="left" ROWS="2" COLS="80"><!--$$$QUERYSTRING$$$--></TEXTAREA>
	</TD><TD>
	<INPUT TYPE='button' VALUE='Next Query' onClick='loadDetails(IndexRefGuide.value);'>
	</TD><TD>
	<INPUT TYPE='button' VALUE='Help' onClick='loadDetails("Help?");'>
	</TD></TR>
	</TABLE>
</TD>
</TR>
<TR><TD><FONT COLOR="#000080"><H4>There are <!--$$$TOTALDOCS$$$--> Documents matching your query. </H4></FONT></TD></TR>
</TABLE>

<P>
<B>
<!--#MyTextResponse#-->
</B>
</P>
<BR>




</BODY>
</HTML>







;;**EXPORTKEY**:IndexRefGuide:defaultLexer
(defchild IndexRefGuide:defaultLexer(inString)
;; ********************************************************************
;; summary:  This Lambda converts an input string into a vector of
;;           recognized lexemes. It is the default lexical analyzer
;;           for the parseLib compiler generator.
;;           This Lambda may be modified, in any way, by the user.
;; Parms:    inString   The source string to be broken into lexemes.
;; return:   tokenList  The vector of recognized lexemes.
;; ********************************************************************
    pvars:(;; Persistent variables
           CH                  ;; The current input character from the input string
           INLEN               ;; The length of the input string
           IP                  ;; The input pointer for the input string
           INSTRING            ;; The string of the input characters to be parsed
           oldKB               ;; The old vector of character break parsing routines
           operatorList        ;; The vector of operator symbols
           KB                  ;; The vector of character break parsing routines
           SB                  ;; The vector of string terminator pairs
           specialList         ;; The vector of special character symbols
           tokenList           ;; The directory of lexical tokens
           TP                  ;; The output pointer for the token output vector
		   (Boolean:charOnly false)
           ;; Methods list
           addStringDelimiters ;; Add a pair of string delimiters to the lexical analyzer
           _default            ;; Recognize this one character
           _Ignore             ;; Ignore this character parsing routine
           _Initialize         ;; Initialize the vector of character break parsing routines
           _recChar            ;; Recognize special characters
           _recFraction        ;; Recognize all fractions
           _recName            ;; Recognize all names
           _recNumber          ;; Recognize all numbers
           _recOperators       ;; Recognize all operator symbols
           _recSpecial         ;; Recognize all special symbols
           _recString          ;; Recognize all delimited strings
           _whiteSpace         ;; Ignore all whitespace characters
           ) ;; end of persistent variables
    ;;************************************************************************
    ;;  Define the child Lambdas for this parent.
    ;;************************************************************************
    ;; Add a named pair of string delimiters to the lexical analyzer.
    (defun addStringDelimiters(name start end)
       vars:(tmpLambda)
       ;;  Initialize the parseLib once and only once.
       (if (= KB #void) (_Initialize))
       ;;  If this is the first delimiter pair, start a new directory.
       (setq CH start[0])
       (if (= SB[CH] #void) 
           (begin
              (setq SB[CH] (new Directory:))
              (setq KB[CH] _recString)
           )) ;; end if
       ;;  Set the character directory with this new string delimiter pair.
       (setq SB[CH][name] (new Vector: 2 start end))
       ) ;; end addStringDelimiters
    ;;  Ignore this character parsing routine.
    (defun _Ignore() (++ IP))
    ;;  Create the character break vector.
    (defun _Initialize()
        vars:(i)
        (setq KB (new Vector: 256))
        (setq SB (new Vector: 256))
        (setq operatorList #(#\= #\< #\> #\! #\^ #\~ #\+ #\/ #\* #\- #\| #\& #\?))
        (setq specialList #(#\( #\) #\[ #\] #\{ #\} #\" #\' #\: #\; #\, #\.))
        ;; Actual mapping of parse routines to break character positions.
        (loop for i from 0 until 256 do (setq KB[i] _recSpecial))
        (loop for i from 0 to 32 do (setq KB[i] _whiteSpace)) 
        (loop for i from 128 until 256 do (setq KB[i] _whiteSpace)) 
        (loop for i from (code #\a) to (code #\z) do (setq KB[i] _recName)) 
        (loop for i from (code #\A) to (code #\Z) do (setq KB[i] _recName)) 
        (loop for i from (code #\0) to (code #\9) do (setq KB[i] _recNumber)) 
        (loop for i from 0 until (length operatorList) do (setq KB[operatorList[i]] _recOperators)) 
		
        (setq KB[(code #\_)] _recName) 
        (setq KB[(code #\.)] _recFraction) 
        (setq KB[(code #\|)] _recChar) 
        (setq oldKB (copy KB))
        ) ;; end of _Initialize
    ;;  Recognize all special characters.
    ;;  Note: These are any chars in the special list enclosed in vertical bars (ie |;|).
    (defun _recChar()
        vars:(oldIP ch2 ch3)

 
        (setq oldIP IP)
        (setq ch2 INSTRING[(++ IP)]) 
        (setq ch3 INSTRING[(++ IP)]) 
        (if (not (and (isMember ch2 specialList) (= ch3 #\|)))
            (begin
               (setq IP oldIP)   
               (return (_recOperators))
               )) ;; end if
        
		(setq recCharName (string ch2))	 
		(if (or (= i #void) (isBoolean i) )
			(begin
				(setq tokenList[recCharName] 1)
			) ;; end begin
			else
			(begin
				(setq tokenList[i 1] (+ tokenList[i 1] 1))
			)
		) ;; end if	
 	 
        (++ IP)
        (++ TP)
        ) ;; end _recOperators
    ;;  Recognize all fractions.
    (defun _recFraction()
        vars:(oldIP result)
 
        (setq oldIP IP)
        (setq CH INSTRING[(++ IP)])
        ;; Recognize fraction portion of number (if any)
        (if (isCharNumeric CH)
            then
            (begin
               (setq CH INSTRING[(++ IP)])
               ;; Recognize fraction portion of number
               (while (isCharNumeric CH) do
                  (setq CH INSTRING[(++ IP)]) 
                  ) ;; end while
               (setq result (number (substring INSTRING oldIP (subi IP 1))))
               ) ; end then
            else
            (setq result (symbol ".")) 
            ) ; end recognize fraction.
       
		(if (= charOnly false)
			(begin 				 
				(if (or (= i #void) (isBoolean i) )
					(begin
						(setq tokenList[result] 1)
					) ;; end begin
					else
					(begin
						(setq tokenList[i 1] (+ tokenList[i 1] 1))
					) ;; end begin
			 	) ;; end if	
			) ;; end begin
		) ;; end if
        
        ) ;; end _recFraction
    ;;  Recognize all names.
    (defun _recName()
        vars:(oldIP )
	 
        (setq oldIP IP)
        (setq CH INSTRING[(++ IP)]) 
        (while (isCharName CH) do
           (setq CH INSTRING[(++ IP)]) 
 
           ) ;; end while
        ;; Recognize trailing BNF commands
        (cond 
           ((= CH #\*) (++ IP))
           ;((= CH #\?) (++ IP))
           ((= CH #\+) (++ IP))
           ) ; end cond
      
		(if (= charOnly true) 
			(begin
				(setq recNameName  (substring INSTRING oldIP (subi IP 1)))
			)
			else
			(begin
				(setq recNameName (symbol (substring INSTRING oldIP (subi IP 1))))
			) 
		) ;; end
			 
		(setq i (member recNameName tokenList))
 
		(if (or (= i #void) (isBoolean i) )
			(begin
 
				(setq tokenList[recNameName] 1)
			 
			) ;; end begin
			else
			(begin				 
				(setq tokenList[recNameName] (+ tokenList[recNameName] 1))	
			 
			) ;; end begin
		) ;; end if	
	  
        (++ TP)
        ) ;; end _recName
    ;;  Recognize all numbers.
    (defun _recNumber()
        vars:(oldIP num fraction)
 
        (setq oldIP IP)
        (setq CH INSTRING[(++ IP)])
        ;; Recognize integer portion of number
        (while (isCharNumeric CH) do
           (setq CH INSTRING[(++ IP)]) 
           ) ;; end while
        ;; Recognize fraction portion of number (if any)
        (if (= CH #\.)
            (begin
               (setq fraction true)
               (setq CH INSTRING[(++ IP)])
               ;; Recognize fraction portion of number
               (while (isCharNumeric CH) do
                  (setq CH INSTRING[(++ IP)]) 
                  ) ;; end while
            )) ; end recognize fraction.
        (setq num (number (substring INSTRING oldIP (subi IP 1))))
        (if (= (integer num) num) (setq num (integer num)))
        (if (= fraction true) (setq num (number num)))
      
		(if (= charOnly false)
			(begin
 				(setq symnum (symbol (string num)))
 
				(setq i (member symnum tokenList))
  
				(if (or (= i #void) (isBoolean i) )
					(begin
					 
						(setq tokenList[num] 1)
 
					) ;; end begin
					else
					(begin
 
						(setq tokenList[i 1] (+ tokenList[i 1] 1))
					) ;; end begin
				) ;; end if	
			) ;; begin
		) ;; end if
 
        (++ TP)
        ) ;; end _recNumber
    ;;  Recognize all operator symbols.
    (defun _recOperators()
        vars:(oldIP)
        (setq oldIP IP)
        (setq CH INSTRING[(++ IP)]) 
        (while (isMember CH operatorList) do
           (setq CH INSTRING[(++ IP)]) 
           ) ;; end while
      
 		(setq recOperatorsName  (symbol (substring INSTRING oldIP (subi IP 1))))	  
 		(setq i (member recOperatorsName tokenList))
		(if (= charOnly false)
			(begin
				(if (or (= i #void) (isBoolean i) )
					(begin
						(setq tokenList[recOperatorsName] 1)
					) ;; end begin
					else
					(begin
						(setq tokenList[i 1] (+ tokenList[i 1] 1))
					) ;; end begin
				) ;; end if
			) ;; end begin
		) ;; end if	
 
        (++ TP)
        ) ;; end _recOperators
    ;; Recognize all special symbols.
    (defun _recSpecial() 	 
		(setq recSpecialName (symbol (string CH)))	 
 
 		(setq i (member recSpecialName tokenList ))
		(if (= charOnly false)
			(begin
				(if (or (= i #void) (isBoolean i) )
					(begin
						(setq tokenList[recSpecialName] 1)
					) ;; end begin
					else
					(begin
						(setq tokenList[i 1] (+ tokenList[i 1] 1))
					) ;; end begin
				) ;; end if
			) ;; end begin
		) ;; end if	
 
		(++ IP) (++ TP))
    ;; Recognize all delimited strings.
    (defun _recString()
        vars:(oldIP i delimPairs delimLen result count              
              name this start end startLen endLen bump)
        (setq oldIP IP)
        ;; Check for a starting string delimiter.
        (setq delimPairs SB[CH])
        (setq delimLen (length delimPairs))
        (loop for i from 0 until delimLen do
           (setq name delimPairs[i 0])
           (setq start delimPairs[i 1][0])
           (setq startLen (length start))
           (setq this (mid INSTRING IP startLen))
           (if (= start this)
               (begin
                  (setq count 1)
                  (if (= start "(") (setq bump 1) (setq bump 0))
                  (setq end delimPairs[i 1][1])
                  (setq endLen (length end))
                  (+= IP startLen)
                  (while (< IP INLEN) do
                     (if (= (setq CH INSTRING[IP]) #\() (+= count bump))
                     (if (= CH end[0])
                         (begin
                            (setq this (mid INSTRING IP endLen))
                            (if (= end this) (-= count 1))
                            (if (and (= end this) (<= count 0))
                                (begin
                                   (+= IP endLen)
                                   ;; Recognize trailing BNF commands
                                   (if (or (= CH #\}) (= CH #\)))
                                       (cond 
                                         ((= INSTRING[IP] #\*) (++ IP))
                                         ((= INSTRING[IP] #\?) (++ IP))
                                         ((= INSTRING[IP] #\+) (++ IP))
                                         ) ; end cond
                                       ) ; end if
                                   (setq result (substring INSTRING oldIP (subi IP 1)))
                                   ;; Ignore all whitespace delimited strings
                                   (if (<> (left name 10) "Whitespace")
                                       (begin
                                          (setq tokenList[TP] (new Vector: 2 name result))
                                          (++ TP)
                                          )) ; end  if
                                   (return TP)               
                                   )) ; end inner if
                            )) ; end outter if
                     (++ IP)
                     ) ; end while
                  (setq result (substring INSTRING oldIP (subi IP 1)))
                  ;; Ignore all whitespace delimited strings
                  (if (<> (left name 10) "Whitespace")
                      (begin
                         (setq tokenList[TP] (new Vector: 2 name result))
                         (++ TP)
                         )) ; end  if
                  (return TP)              
                  )) ; end if 
           ) ;; end loop
        ;; If we get here, this is not the start of a delimited string,
        ;; so invoke the old lexeme parser for this character.
        (oldKB[CH])) ;; end _recString
    ;;  Ignore all whitespace characters.
    (defun _whiteSpace()        
        (setq CH INSTRING[(++ IP)]) 
        (while (and (> CH 0) (<= CH 32)) do
           (setq CH INSTRING[(++ IP)]) 
           ) ;; end while
        ) ;; end _whiteSpace
    ;;************************************************************************
    ;;  Define the main code routines for this parent.
    ;;************************************************************************
    ;;  Initialize the parseLib once and only once.
 
    (if (= KB #void) (_Initialize))
    ;;  Initialize the output token vector. 
    (setq tokenList (new Directory:))
    
    ;;  Recognize each character in the input string.
    (setq INSTRING inString)
    (setq INLEN (length INSTRING))
    (setq IP 0)
    (while (< IP INLEN) do
        ;; Retrieve the next input character
	
        (setq CH INSTRING[IP])
	
        ;; Invoke the parse routine for this input character
        (KB[CH])
 	  
        ) ;; end while
    ;;  Return the token list as the output
 
    tokenList) ;; end defaultLexer









































;;**EXPORTKEY**:IndexRefGuide:generateLexicon
;;==============================================================================================================
;; generateLexicon
;;==============================================================================================================
;; Data Structures Generated:
;;			myDir				Directory of the list of junk words not included
;;			wordREPO			ObjectRepository on disk
;;								key: Text    - First 2 letters of the word evaluated
;;								value: Directory
;;									The Directory contains the current word evaluated 
;;									as the key and its absolute word index (AWI) 
;;									as its value.  AWI values start from 0.
;;			frequencyREPOS		Object Repository on disk
;;								key: Text    - The current word evaluated
;;								value: Integer - The frequency of the word in all of the documents evaluated.								 
;;			baseFile.txt		File saved on disk.
;;								This file contains the information per document saved 
;;								in this format:
;;								Integer1 - Total Number of bytes in the document record saved to the file
;;								Integer2 - Total number of words in the Document
;;								Word - Frequency Pair - As contained in the document
;;
;; Sample Document: 
;;		Please ship the bag and my shoe.  We await the shipment.
;;
;;	WordREPOS       aw: #{dir||"await"  0}
;;					ba: #{dir||"bag"   1}
;;					sh: #{dir||"ship" 2 "shoe" 3}
;;  baseFile.txt
;;	26     4     await 1 bag 1 ship 2 shoe 1
;;================================================================================================================

(defchild IndexRefGuide:generateLexicon()
pvars:((myDir #{dir||    "|'|" 1  "-year-old" 1  "a"   1  "ability" 1 "able" 1 " aboard" 1 "about" 1 "above" 1 "absolute" 1 "absolutely" 1 "across" 1 "act" 1 "acts" 1 "actual" 1 "actually" 1 "add" 1 "additional" 1 
				"additionally" 1  "after" 1 "afterwards" 1 "again" 1 "against" 1 "ago" 1 "ahead" 1 "aimless" 1 "aimlessly" 1  "ain't" 1 "al" 1 "albeit" 1 "align" 1 "all" 1 "allow" 1 "almost" 1 "along" 1 "alongside" 1
	        	"already"  1 "also" 1 "alternate" 1 "alternately" 1 "although" 1 "always" 1 "am"  1 "amid" 1 "amidst" 1 "among" 1 "amongst"  1 "an" 1 "and"  1 "announce" 1 "announced" 1 
	       		"announcement" 1 "announces" 1 "another" 1 "anti"  1 "any" 1 "anyone" 1  "anything"  1 "appaling" 1  "appalingly"  1 "appear" 1  "appeared" 1 "appears"  1  "are" 1
	       		"aren't" 1 "around" 1 "as"  1 "ask" 1 "asked" 1 "asking" 1 "asks" 1 "at" 1 "await"  1 "awaited" 1  "awaits"  1 "awaken"   1  "awakened" 1  "awakens" 1  "aware" 1  "away" 1 
	      		"b" 1 "back" 1 "backed"  1 "backing"  1  "backs" 1 "be"  1 "became" 1  "because"  1 "become" 1 "becomes" 1  "becoming" 1 "been" 1 "before" 1 "began" 1  "begin"  1 "begins"  1  "behind" 1
	       	    "being"  1  "believe" 1  "believed" 1 "between"  1 "both" 1 "brang" 1 "bring" 1  "brings" 1 "brought" 1 "build" 1 "builds" 1 "built" 1 "busy" 1 "but" 1 "by" 1 "c" 1 "call" 1 
				"called" 1  "calling" 1 "calls" 1  "can" 1 "cannot" 1 "can't" 1 "carried" 1 "carries" 1 "carry" 1 "carrying" 1
	        	"certainly" 1 "change" 1 "changed" 1 "changes" 1 "choose" 1 "chooses"  1 "chose" 1 "clearly" 1 "close" 1 "closed" 1 "closes" 1 "closing" 1 "com" 1  "come" 1  "comes"  1 "coming" 1 "consider" 1 
	    	    "considerable" 1 "considering" 1  "could"  1 "couldn't" 1  "d" 1 "dare"  1 "daren"   1 "day" 1 "days" 1  "despite" 1 "did" 1 "didn't" 1 "do" 1 "does" 1 "doesn't" 1 "doing" 1 "don't" 1  "done" 1 "down" 1 "downward" 1 "downwards" 1 "e" 1
	            "each" 1 "eight" 1 "either" 1 "else" 1 "elsewhere" 1 "especially" 1 "etc"  1  "even" 1 "eventually"  1 "ever" 1  "every" 1 "everybody" 1  "everyone"  1 "exactly" 1 "example" 1 "examples" 1 
	            "f" 1 "far" 1 "feel" 1 "felt" 1 "few" 1 "final" 1 "finally" 1 "find"  1 "five" 1 "for" 1 "found"  1 "four" 1 "fourth" 1 "from"  1 "gave" 1 "get" 1  "gets" 1 "getting" 1 "give"  1 "gives" 1
	     	    "go" 1 "goes" 1 "going" 1 "gone" 1 "good" 1 "got" 1 "great" 1 "h" 1 "had" 1  "has" 1 "have" 1 "haven't" 1 "he" 1 "he'd" 1  "he's" 1 "held"  1 "her"  1 "here" 1 "heretofore" 1 "hereby" 1 
			    "herewith" 1 "hers"  1 "herself" 1  "high" 1  "him" 1 "himself" 1 "his" 1 "hitherto" 1 "happen" 1 "happened" 1 "happens" 1 "hour"  1 "hours" 1  "how" 1  "however" 1
	   	        "i" 1 "i'd" 1 "i'll" 1 "i'm" 1 "i've" 1 "if"  1 "ii" 1 "iii" 1 "in" 1 "include" 1 "included" 1  "includes" 1 "including" 1 "inside" 1  "into" 1  "is"  1 
	            "isn't" 1 "it" 1 "its" 1  "itself" 1 "it'll" 1 "it'd" 1 "it's" 1  "kind"  1 "kinds" 1 "l" 1 "la" 1 "larger" 1 "largest" 1 "last" 1 "later" 1 "latest" 1 "le" 1 "least"  1 
			    "leave" 1 "leaves" 1 "leaving" 1 "les" 1  "less" 1 "let"  1 "like" 1 "ll" 1 "lya" 1 "m" 1 "made" 1 "mainly" 1 "make" 1 "makes" 1 "making" 1 "man" 1 "many" 1 "may" 1 "me" 1 
				"means" 1 "meant" 1 "meanwhile" 1 "men" 1 "might" 1 "missed" 1 "more" 1 "moreover" 1 "most" 1  "mostly"  1  "move" 1 "moved" 1 "moving" 1 "mr" 1 "mrs" 1 "much" 1 "must" 1 "mustn't" 1 "my" 1 "myself" 1
	            "need" 1 "needs" 1 "neither" 1 "never"  1 "new" 1 "newer"   "news" 1  "night" 1  "nights" 1 "nine"  1  "no" 1 "non" 1 "none" 1 "nor"  1 "not" 1 "now" 1
	            "o" 1  "of"  1 "off" 1 "often" 1 "on"  "once" 1 "one"  1 "only"  1 "old" 1  "or" 1  "other"  1 "our"  1 "out" 1 "over" 1 "own" 1  "owns" 1
	            "p" 1 "particularly" 1 "per"  1  "present" 1 "presentation" 1 "presented" 1 "presenter" 1 "presenting" 1 "presents" 1  "primarily" 1 "put"  1
	            "q" 1  "quickly" 1 "r"  1 "remain" 1 "remaining" 1 "respond" 1 "responded" 1  "responding" 1 "responds" 1 "return" 1 "ran" 1 "rather"  1 "run" 1 "running" 1 "runs" 1
	            "s" 1 "said" 1 "same" 1 "say" 1 "says" 1 "see" 1  "seek"  1 "seeking" 1 "seeks" 1 "seen" 1  "send" 1 "sent" 1  "set" 1 "sets" 1 "seven"  1 "several" 1 "she" 1
	            "she's" 1  "should" 1  "shouldn't" 1  "shown" 1  "side" 1  "since"  1  "six" 1  "sixes" 1  "slow" 1  "slowed" 1  "slows" 1  "small" 1  "smaller" 1   "so" 1  
				"some"  1 "someone"  1  "something" 1  "somewhat" 1 "somewhere" 1 "soon" 1 "sought" 1  "spread" 1 "stay"  1 "stayed"  1 "still"  1 "substantially" 1 "such" 1 "suppose" 1
	            "t" 1  "take" 1  "taken" 1  "takes" 1  "taking" 1  "tell" 1  "tells"  1  "th" 1  "than" 1  "that" 1   "the" 1  "their" 1  "them" 1   "themselves" 1  "then" 1  "there" 1  "thereby" 1
			    "therefore" 1 "these" 1 "they" 1  "they'd" 1   "they're" 1 "they've"  1 "thing"  1 "things" 1  "thi" 1 "this" 1 "those"  1  "though" 1 "three" 1 "through" 1  "throughout" 1  "thus" 1
	            "to" 1 "to-day" 1 "today" 1 "together" 1 "too" 1 "took"  1 "toward" 1 "towards" 1 "tried"  1  "tries"  1  "try" 1  "trying" 1  "two" 1
	            "u" 1  "unable"  1 "under"  1 "underneath" 1 "undid" 1 "undo" 1 "undoes"  1 "undone" 1 "undue" 1  "undoubtedly" 1  "unnecessarily" 1
	            "unfortunately" 1  "unless" 1 "unofficially"  1 "until"  1  "unusually" 1 "unsure" 1  "up"  1  "upon"  1  "upward" 1  "us" 1
	            "use" 1  "used" 1  "uses" 1  "using" 1  "usual" 1  "usually" 1 
                "v" 1   "used" 1  "uses" 1  "using"  1  "very"  1  "via" 1  "view"  1  "wait" 1  "waited" 1  "waits" 1  "want"  1  "wanted" 1  "wants" 1 "was" 1
	            "wasn't" 1  "watched" 1  "watching" 1  "way" 1  "ways" 1  "we" 1  "we'd" 1  "we'll" 1  "we're" 1  "we've" 1  "web" 1  "went"  1 "were" 1 "what" 1 "whatever"  1 "when" 1 
                "whenever" 1  "where" 1  "wherever" 1  "whether" 1  "which" 1   "whichever" 1   "while" 1  "who"  1
	            "who've" 1 "whoever" 1 "whom" 1 "whomsoever" 1  "whose" 1  "whosever" 1 "why" 1 "wide" 1 "wider"   1  "will"  1  "with"  1  "without"  1  "won" 1  "won't" 1  "would" 1
	            "wouldn't"  1  "wow" 1  "wows" 1  "www" 1  "x" 1  "xii" 1  "xiii"  1  "xiv" 1  "xv" 1  "xvi" 1   "xvii" 1  "xviii" 1  "xix" 1  "xx" 1 
	            "y" 1  "yeah" 1 "year" 1  "you" 1  "your" 1  "yours" 1  "you'll" 1  "you're" 1  "your's" 1  "yourself" 1   "yourselves" 1
	       }) ;; myDir	
		 
	) ;;pvars
vars:(	
		(String:myCurrentDocTXTVEC)		;; Vector of stemmed, html tag-stripped and lexed words
		(String:myCurrentDocHTML)		;; Document without the HTML tags removed
		(String:currentWord)			;; Current word evaluated
		myCurrentWordWeights
		wordDir							;; Directory of words in wordREPOS with the specified key
		(ByteVector:key	)				;; Byte vector of 2 elements representing work key										
		(Integer:fileID	)				;; fileID of baseFile.txt document
		strFrequency					;; String Representation of word frequency in baseFile.txt
		strTotalWords					;; String representation of total words in a document
		sTotalWords						;; String Buffer for strTotalWords
		strTotalBytes					;; String Representation of byteCount in a single record
		sTotalBytes						;; String Buffer for strTotalBytes
		dirKey							;; Word directory key
		myCurrentDocWordVec				;; ByteVector of all information in a document record to be 
										;; written to baseFile.txt
	
		  
	 ) ;; end vars
 
regs:(	(Integer:currentDocNum)			;; Current Document evaluated
		(Integer:lengthDoc 0)	 		;; Length of the document
		(Integer:lengthVec 0)			;; Length of myCurrentDocTXTVEC
		(Integer:byteCount 0)			;; Number of bytes written in a single Document record in baseFile.txt
		(Integer:m 0)					;; Word Counter in a document
		(Integer:freqNum 0)				;; Frequency of the word in a document
		(Integer:weightedFreq 0)		;; Weighted freqeuncy of the word
        (Integer:wordIndex1 0)			;; Index for retrieving wordREPOS value
		(Integer:wordIndex2 0)    		;; Index for retrieving wordDir value	
		(Integer:wordSetCount 0)		;; Number of unique, stemmed and non-junk words in a document
		(CharPointer:wp)				;; Pointer for currentWord
		(CharPointer:kp)				;; Pointer for key
		(CharPointer:vp)				;; Pointer for myCurrentDocWordVec
		(CharPointer:fp)				;; Pointer to strFrequency
		(CharPointer:ip)				;; Pointer to strTotalBytes
		(CharPointer:iip)				;; Pointer to sTotalBytes		 
		(CharPointer:sp)				;; Pointer to strTotalWords
		(CharPointer:ssp)				;; Pointer to sTotalWords
		(Integer:wordCount)				;; Total Number of words
		(Integer:wordIndex)
		(Integer:weight)
		(Integer:cc 0) (Integer:c1 0) (Integer:n 0) 	(Integer:n 0)	 
	 ) ;; end regs


 
	(if (= wordREPO #void)
		(begin
			(setq wordREPO (new ObjectRepository: "wordREPO.db"))
			(clear wordREPO)
		) ;; end begin   
		else
		(begin
			(setq wordREPO (new ObjectRepository: "wordREPO.db"))		 
		)
	) ;; end if

	(if (= indexREPOS #void)
		(begin
			(setq indexREPOS (new ObjectRepository: "indexREPOS.db"))
			(clear indexREPOS)
		) ;; end begin   
		else
		(begin
			(setq indexREPOS (new ObjectRepository: "indexREPOS.db"))		 
		)
	) ;; end if

 
 
	(beginTransaction wordREPO)
 
  
	;; Looping though all the documents in CoreContent
	(loop for currentDocNum from 0 until  pTotalDocNum  do
		(setq myCurrentDocTXTVEC (new Vector:))
		(setq myCurrentDocHTML (resize (new String: "" ) 300))	 	 
        (setq myCurrentDocHTML (browseLib.checkout myOntology ontologyNames[currentDocNum])) 

		(writeln "checking out: ======  " ontologyNames[currentDocNum]  " = " currentDocNum  " ===== " )
	 	;; Stemming the input string 
		(setq porterStemmer.htmlTagsOn false)
		(setq myCurrentDocHTML  (porterStemmer myCurrentDocHTML 1))
		;(writeln "after porter myCurrentDocHTML: " myCurrentDocHTML )
		(setq lengthDoc  (length myCurrentDocHTML))	 
		;; Getting the weights based on the XML Structure
		;(writeln "calling wordWeights: ")
		(setq myCurrentWordWeights (IndexRefGuide.wordWeights))
	  	;; Separating the words into lexemes
		(setq IndexRefGuide.defaultLexer.charOnly true) 
		(setq myCurrentDocTXTVEC (IndexRefGuide.defaultLexer myCurrentDocHTML))	 		 
		(setq lengthVec (length myCurrentDocTXTVEC))	 
 	 
		(setq m 0)
	 	(setq wordSetCount 0)
	  
		;; Routine to loop over all the words in the document
		FetchWord::	 
 		
		(if (>= m lengthVec) (goto End:))
		(setq currentWord (ref myCurrentDocTXTVEC m 0))
	 	;(writeln "currentWord: " currentWord)
 
		
		;; Remove Junk words and Non words
		(if (or (>=  myDir[currentWord] 0) (= (length currentWord) 1)) (begin (++ m)   (goto FetchWord:))) 
	 
		(setq freqNum myCurrentDocTXTVEC[m 1] )
		 
		;; no need for line below since directory is of unique terms: 
		;(if (> freqNum 1) (begin  (setq m (+ m freqNum)) ))
	 
		;; Add weights from XML function
		(setq wordIndex myCurrentWordWeights[currentWord] )
	 	(setq weightedFreq freqNum)
		(if (<> wordIndex #void)
			(begin
				;(writeln "currentWord is weighted :  " currentWord)
				;(writeln "old freqNum: " freqNum)
				(setq weight (ref myCurrentWordWeights currentWord))
				;(writeln "weight: " weight)
				(setq weightedFreq (+ freqNum weight))
				;(writeln "new freqNum: " freqNum)
			) ;; end begin
		) ;; end if
 
 
 
	 	(setq wp currentWord)
		(setq key (new String: "abc"))
		(setq kp key)
		(setq n 0)

		;; Get the first two letters of the word as key
		(vmregRunInHardware start:)
        (while (< n 2)  do 
			 
			(setq cc wp[n])
			(setq kp[n] cc) 
			(setq kp[(++ n)] 0)
			;; Get the integer value of the key			 
		) ;; end while
		(vmregRunInHardware stop:)	 

		;; Make copies
		(setq keyCopy (copy key))
		(setq wordCopy (copy currentWord))
       
		(setq wordDir wordREPO[keyCopy])
 		;(writeln "wordDir before: " wordDir)
		(if (= wordDir #void) 
			(begin              			 
				(setq wordDir (new Directory:))
				(setq wordDir wordCopy freqNum)
				(setq wordREPO keyCopy wordDir)		 
			) ;; end begin
			else 
			(begin                			 
				(if (= wordDir[wordCopy] #void)
					(begin 							  
						(setq wordDir wordCopy freqNum)					 	  				 	 
					) ;; end begin
					else 
					(begin 	
						(setq oldFreqNum wordDir[wordCopy])
						(setq wordDir wordCopy (+ oldFreqNum freqNum))
						(setq wordREPO keyCopy wordDir)				 
						(goto SetWord:)
					) ;; end begin
				) ;; end if
				(setq wordREPO keyCopy wordDir)
			 
			) ;; end begin
		) ;; end if

	 	;(writeln "wordDir after: " wordDir)

		SetWord:: 
	 
		(++ wordSetCount)	 
	
		;; Setting value for freqREPOS
	

		;; Setting directory value for indexREPOS
		(if (= aDir #void)
			(begin
				(setq aDir (new Directory: ))
				(setq aDir currentWord weightedFreq)
			) ;; end begin
			else
			(begin
				(setq aDir currentWord weightedFreq)
			)
		) ;; end begin
	

		(if (<= m lengthVec) 
			(begin 
				(if (isExact (ndiv m 200)) 
					(begin 					
						(checkPointTransaction wordREPO) 									 
					) ;; end begin
				) ;; end if
				(++ m) 			 
				(goto FetchWord:)
			) ;; end begin
		) ;; end if	
		;; End Routine 
	
		End::
		;(writeln "currentDocNum: " currentDocNum)
		;(writeln "aDir: " aDir)
		(setq indexREPOS currentDocNum aDir)
	 
	) ;; end loop for document
	
	(commitTransaction indexREPOS)
 
	;; Loop over wordREPOS keys to assign AWI to each of the words
	(setq wordCount 0)
	(loop for wordIndex1 from 0 to (- (length wordREPO) 1) do
		;; Get the keys
		(setq key (ref wordREPO wordIndex1 0))
		(writeln " == key: == " key)	 
		;; Get the value of the word structure 
		(setq wordDir (ref wordREPO wordIndex1  1))		 
	 	;; Loop over the word structure
		(loop for wordIndex2 from 0 to (- (length wordDir) 1)  do		 
			(setq dirKey (refDirKey wordDir wordIndex2))
			(setq dirValue (refDirValue wordDir wordIndex2))
			;(writeln "dirValue: " dirValue)
			;(writeln "wordCount: " wordCount)
			(setq wordDir dirKey (+ wordCount (* dirValue .00001)))					 
			;(setq wordDir dirKey wordCount)			 	 	 
			(++ wordCount) 
		) ;; end loop
	 
		(setq wordREPO key wordDir)	
		;(writeln "setting repo: key: " key " == wordDir: "  wordDir) 
	) ;; end loop

	(setq pWordCount wordCount)
	 
	;(fileClose fileID 1)
	(commitTransaction wordREPO)


) ;; end defun generateLexicon

 



;;**EXPORTKEY**:IndexRefGuide:generateMatrix
;;===========================================================
;; 	generateMatrix 
;;===========================================================
;; 	This implementation first saves the 
;;	awi: frequency/word in a doc in a record.
;; 	Then it loops over the record to store row information
;;	in a byte vector before writing it to a file
;;  Data Structures Generated:
;;		docREPOS			ObjecRepository
;;								key: Document Number
;;								value AWI of word contained in the document
;;		matrix.bin			File stored on disk representing a matrix
;;		newRecord			Rows: Document Numbers
;;							Columns: AWI
;;
;
;;==========================================================
(defchild IndexRefGuide:generateMatrix() 
vars:(	fileID1						;; fileID of baseFile.txt
	 	fileID2						;; fileID of matrix.txt
		currentWord					;; Current Word evalauted in the vector.txt file
		strTotalWords				;; String Representation of Total Words in Doc
		strTotalBytes				;; String Representation of Total bytes in a Brick
		(Integer:totalWordsInDoc)	;; Total words in a document represented as an Integer	
		(Integer:recordSize)		;; Size of the information of one document in baseFile.txt	 
		(ByteVector:fileRecord)		;; Byte Vector of the words and frequency of each word in a doc
		key							;; Word key for each word evaluated
		currentWord					;; Current word evaluated
		strFrequency				;; Frequency of a word in a document represented as a string
		aDir						;; Word Structure returned from the wordREPOS
		awi							;; Integer: absolute word index
		(Number:frequency)			;; Word frequency in a document
		strTotalWords				;; File Information on the total word number in a document
		totalWordsInDoc				;; Integer representation of strTotalWords
	 	singleRowRecord				;; Single row record information represented as a Brick
		(Number:totalWordFrequency) ;; Total word count of a word in all of the documents
		wordVec						;; Integer Vector of AWIs in a document
		(Integer:wordCount)			;; Number of words in a document currently evaluated
		(Number:IDF)				;; Result of log10(totalDocs/term Frequency in a doc)
		(Number:weights)			;; Result of IDF * term Frequency in a doc
		(Number:docMagnitude)		;; The result of the square root of the sum of squares of the 
									;; term IDF in the doc
		(Number:zero 0)		
		(Number:maxFrequency)		
		docVec						;; IntVector of document numbers where a word can be found
		tempDir
		wordREPOS freqREPOS
		(Number:sqWeights)
		)  ;; end vars
regs:(	(Integer:currentDocNum 0)    	;; Document Evaluated
		(Integer:iTotalDocs 0)			;; Total Number of Docs in CoreContent
		(Integer:iTotalWords 0)			;; Total Number of Words
		(Integer:rowNum 0)				;; Current Brick Row evaluated	 
		(CharPointer:rp)				;; pointer to the file Brick loaded from vector.txt
		(CharPointer:kp)				;; pointer to the key
		(CharPointer:fp)				;; pointer to the frequency
		(CharPointer:wp)				;; pointer to the word
		(Integer:wordLen 0)				;; length of the current word evaluated
		(CharPointer:sp)				;; pointer to singleRowRecord
		(Integer:rowSize)				;; Byte size of single row
		(Integer:wordVecCount 0)		;; Number of words in a document
		(Integer:cc) (Integer:n) (Integer:m)

		) ;; end regs

 
	(setq fileID1 (fileOpen "baseFile.txt" 0 0))
 
 
;	(if (= docREPOS #void)
;		(begin
;			(setq docREPOS (new ObjectRepository: "docREPOS.db"))
;			(clear docREPOS)
;		) ;; end begin   
;		else
;		(begin
;			(setq docREPOS (new ObjectRepository: "docREPOS.db"))		 
;		)
;	) ;; end if

 
	(setq wordREPOS (new ObjectRepository: "wordREPO.db")) 
;	(setq myDocIndex (new index  myDocIndex: docREPOS create: memory: ))  
	(beginTransaction wordREPOS)
;	(setq tempDir (ref wordREPOS (- (length wordREPOS) 1) 1) )
;	(commitTransaction wordREPOS)

;	(setq pWordCount (refDirValue tempDir (- (length tempDir) 1))) 
;	(++ pWordCount)
 
;	(setq currentWord (new String: "abcdefghijklmnopqrstuvwxyz"))
	;; Transferring pvars to registers
	 
	(setq iTotalDocs (- pTotalDocNum 1))
	(setq currentDocNum 0)
	(setq iTotalWords  (- pWordCount 1))
	(writeln "pTotalDocNum: " pTotalDocNum)
  
	;; With extra repetition for docMagnitude and maxFrequency
;	(setq newRecord (new Brick: pTotalDocNum  wWeight:Float:(+ pWordCount 1) ))
 	(setq wordDir (new Directory:))
;	(setq rowNum 0)

#if 0
 	(setq currentDocNum 0)
	(setq lengthREPO (- (length wordREPOS) 1))
	(loop for n from 0 to lengthREPO
		(setq aDir (new Directory:))
		(setq key (ref wordREPOS n 0))
		(writeln " ===== KEY ======= : " key)
		(setq aDir (ref wordREPOS n 1))
		(setq lengthDir (- (length aDir) 1))
		(loop for m from 0 to lengthDir
			(setq currentWord (refDirKey aDir m))
			(setq currentWordNum (refDirValue aDir m))
			(writeln "curentWord: " currentWord " == currentWordNum: " currentWordNum)
		);; end loop
	) ;; end loop



	(commitTransaction wordREPOS)
#endif

 
	;; Load Document Brick as a Byte Vector
	LoadRecord::

 	(writeln "currentDoc: ==  " ontologyNames[currentDocNum] " ==")
;	(systemCheck)
	(setq wordCount 0)
 
	;; Getting the record size
 	(setq strTotalBytes (fileRead fileID1 10))
	(writeln "strTotalbyte: " strTotalBytes)
	(setq recordSize (integer strTotalBytes))
	(setq strTotalWords (fileRead fileID1 10))
 	(writeln "strTotalWords: " strTotalWords)
	(setq totalWordsInDoc (integer strTotalWords) )
	(setq fileRecord (fileRead fileID1 recordSize))
	(setq fileRecord (string fileRecord))	 
 	(setq rp fileRecord)
;	(setq wordCount 0)
;	(setq maxFrequency 0)
  
 
	GetWordFromFile::
	;; Each word has a word key and
	;; Each word has a frequency
	(writeln "wordCount: " wordCount)
	(setq aDir (new Directory:))
  	(if (= wordCount 1000) (checkPointTransaction wordREPOS))
	;(if (< wordCount totalWordsInDoc) (goto GetWordFromFile:))
	(setq key (new String: "abc"))
	(setq strFrequency (new String: "123456789"))
	(setq currentWord  (new String: "abcdefghijklmnopqrstuvwxyz123456789abcdefghijklmnopqrstuvwxyz")  )
  	
	(setq kp key)
	(setq fp strFrequency)
	(setq wp currentWord)
 
	(vmregRunInHardware start:)
	;; Ignoring whitespace
	(while (and (> (setq cc rp[0]) 0) (<= cc 32) ) do (vmregAddImmediate 1 rp))
	;(if (= cc 0) (begin (writeln "end right away: " )  (goto EndFile:)))
 

	;; Getting word and wordKey
	(setq wordLen 0)
	(while (> (setq cc rp[0]) 32) do 	 
		(if (< wordLen 2)
			(begin  				 
				(setq kp[0] cc) 
				(setq kp[1] 0)
				(++ kp)
			) ;; end begin
		) ;; end begin
		(setq wp[0] cc) 
		(setq wp[1] 0) 
		(++ wordLen)
		(++ wp)
		(++ rp)
		(setq cc rp[0])	
	) ;; end begin
	(vmregRunInHardware stop:)
  
	;; Ignore whitespaces
	(vmregRunInHardware start:)
	(setq cc rp[0])
	(while (and (> (setq cc rp[0]) 0) (<= cc 32) (<> cc 10)) do (vmregAddImmediate 1 rp) )	
 	;; Getting the frequency 
	(while (> (setq cc rp[0]) 32) do  (setq fp[0] cc) (setq fp[1] 0) (++ fp) (++ rp))
	(vmregRunInHardware stop:)

	(writeln "key: " key " ==  currentWord: " currentWord " == frequency: " strFrequency)


	;; Compute for the word AWI

 	(setq aDir (ref wordREPOS (copy key)))
 
	;(writeln "aDir: " aDir)
	(setq awi aDir[(copy currentWord)])
	(writeln "awi: " awi)

 
	;; Set value to Brick	
 
;;	(setq frequency (number strFrequency)) 	 
 
  	;; Setting values in the record 
 	 
;	(setq newRecord[0 awi currentDocNum] frequency)
 
;	(if (> frequency maxFrequency) (begin  (setq maxFrequency frequency)))
;	(writeln "newRecord: awi: "  awi " currentDocNum: " currentDocNum " frequency: " frequency)
 
	;; Setting value to docREPOS
	;(setq docVec (ref docREPOS awi))
;	(writeln "awi: " awi " currentWord : " currentWord  " currentDocNum: " currentDocNum)
  
	;; Setting index
;	(setq myDocIndex[awi] currentDocNum)
 
	(++ wordCount)

;	EndFile::
	(if (= (setq cc rp[0]) 0) (begin  (goto EndRecord:)))
	(if (< wordCount totalWordsInDoc) (goto GetWordFromFile:))
	
 ;	(writeln "setting maxfrequency: " maxFrequency)
;	(setq newRecord[0  pWordCount currentDocNum] maxFrequency)
 
	EndRecord::
 
	(if (>= currentDocNum iTotalDocs) (goto EndDocs:))
	(++ currentDocNum)

	(goto LoadRecord:)
 
	EndDocs::	 
	(commitTransaction wordREPOS)
	(fileClose fileID1 1)
 ; (writeln "EndDocs: " )
 	;; Saving Brick information in file
;	(setq fileID2 (fileOpen "matrix.bin" 1 4))
	;(setq rowSize newRecord["RowByteCount"])
 ;(myDocIndex.Pv.save)
 
#if 0
(setq freqREPOS (new ObjectRepository: "freqREPOS.db")) 
(beginTransaction freqREPOS)
(if (= matrixREPOS #void)
		(begin
			(setq matrixREPOS (new ObjectRepository: "matrixREPOS.db"))
			(clear matrixREPOS)
		) ;; end begin   
		else
		(begin
			(setq matrixREPOS (new ObjectRepository: "matrixREPOS.db"))		 
		)
	) ;; end if
(beginTransaction matrixREPOS)
	
	(loop for m from 0 to iTotalDocs
	 ;(writeln " m: " m)
		(setq wordVec (new Vector: Integer: 0))
		(setq wordVecCount 0)
		(setq docMagnitude 0)
		;; With extra repetition for docMagnitude
	 	(setq singleRowRecord (new Brick: 1 wWeight:Float:(+ 1 pWordCount)))
	;	(setq sp singleRowRecord)
		(writeln "Writing Document:  === " ontologyNames[m] " ===  m:  "  m  )
		(setq maxFrequency newRecord[0 pWordCount m])
		;(writeln "maxFrequency: " maxFrequency)	
		(loop for n from 0 to iTotalWords  
			(setq totalWordFrequency (ref freqREPOS n 1))	
	;	(writeln "totalWordFrequency: " 	 totalWordFrequency)
			(setq IDF (log10 (ndiv (number pTotalDocNum) (number totalWordFrequency))))
					 
	 	 	;(setq weights (* IDF newRecord[0 n m] ))
			;; Normalized frequency information
			(setq frequency newRecord[0 n m] )
			(setq weights  (* IDF (ndiv frequency maxFrequency)))
			(setq sqWeights  (* weights weights))
			;(validate sqWeights)
			(if (and (> sqWeights  3.731151215141E+303) (< sqWeights -3.731151215141E+303)) (begin  (setq sqWeights 0.0)))
			;; Compute for document magnitude
			(if  (<> sqWeights 0) (begin (setq docMagnitude (+ sqWeights docMagnitude))  ))
			;(if (= n 283) (writeln "newRecord[0 n m] : " newRecord[0 n m] ))	 
			;(if (<> weights zero) (begin (setq wordVec wordVecCount n) (++ wordVecCount)))		 
			(setq singleRowRecord[0 n 0] weights)
			;(if (= n 283) (writeln "IDF: " IDF " weights for 283: " weights))
		 ;	(writeln "singleRowRecord[0 n 0]: " n " == weights: " weights)
		) ;; end loop 1
	;	(writeln "before: docMagnitude: " docMagnitude)
		(setq docMagnitude (sqrt docMagnitude))
	;	(writeln "m: " m " == docMagnitude: " docMagnitude)
		(setq singleRowRecord[0 n 0] docMagnitude)
		;(setq docREPOS m wordVec)
	
	 	;(writeln "test: singleRowRecord[0 10 0]  iTotalWords: " singleRowRecord[0 283 0])
		;(fileWrite fileID2 sp rowSize)
		(setq matrixREPOS m singleRowRecord)
		;(writeln "test2: " (ref matrixREPOS m 1)[0 283 0])
		(if (= m 1000) (checkPointTransaction matrixREPOS))
	 
	) ;; end loop 2

 ;	(writeln "Closing File: " )
;	(fileClose fileID2 1)
;	(writeln "Close Completed: " )
	;(commitTransaction docREPOS) 
;(writeln "test index: " myDocIndex[0])

	(commitTransaction matrixREPOS)
    
    (commitTransaction freqREPOS)
#endif   
;(writeln "after save: " )
) ;; end generateMatrix2
 



;;**EXPORTKEY**:IndexRefGuide:searchRefGuide
;;===========================================================
;; 	SearchRefGuide 
;;===========================================================
;; 	This implementation first saves the 
;;	awi: frequency/word in a doc in a record.
;; 	Then it loops over the record to store row information
;;	in a byte vector before writing it to a file
;;  Data Structures Generated:
;;		docREPOS			ObjecRepository
;;								key: Document Number
;;								value AWI of word contained in the document
;;		matrix.bin			File stored on disk representing a matrix
;;		newRecord			Rows: Document Numbers
;;							Columns: AWI
;;
;
;;==========================================================  
(defchild IndexRefGuide:searchRefGuide(inString)
pvars:((myOntology "CoreContent")   	;; Where Documents are found  
		myOntologyNames
	)
vars:(  vindex
		ontologyNames					;; Vector of Document Names
		fileID							;; fileID of matrix.bin  
		queryVec						;; Query Words after stemming and lexing
		Q								;; Vector of Similarity values per document
		totalWordNum					;; total number of words in all the documents
		wordDir							;; wordREPOS Value
		rowLen							;; Byte size of each record row stored in matrix.bin
		queryRecord						;; Row from matrix.bin
		key								;; Key for wordREPOS
		currentWord						;; Current word evaluated
		qDocVec							;; Vector of Document numbers where query words are found
		(Number:qWeight)				;; IDF * frequency of the frequery term per document
		(Number:queryMagnitude)			;; Magnitude of the query vector
		record							;; Matrix row extracted from matrixREPOS
		sortedVec
		dirValues
  key wordStruct    currentWord   
		(Number:xProduct)
		(Number:nQuery)
	returnedDocs
		
	 ) ;; end vars
regs:(	(CharPointer:qp)				;; Pointer to queryRecord
		(Integer:iQueryVec)				;; Number of query words minus 1
		(Integer:iDocVec)				;; Number of documents wher query terms appear
		(Integer:totalDocNum)			;; Total number of documents in CoreContent
		(Integer:n) (Integer:cc) (Integer:m) (Integer:x)
		(CharPointer:kp)				;; Pointer to the key
		(CharPointer:wp)				;; Pointer to current word
		(Integer:keyLen 2)				;; Number of bytes for the wordREPOS key
		(Integer:awi)					;; Absolute Word Index
		(Integer:rankedDocNum)			;; Seek to the location of teh 
		 
	) 

;; count number of indexed docs , may be modified
 
(browseLib.setFocus myOntology)
(setq myOntologyNames (^new Vector: 0))
(setq myOntologyNames (browseLib.getChildNames))
(setq totalDocNum  (length myOntologyNames) )
(setq returnedDocs (new Vector:)) 
;; Open necessary Repositories and files
;(setq repo (new ObjectRepository: "docREPOS.db"))
;
;(setq docIndex (new index myDocIndex: repo create: memory:)) 
;
 
(setq wordREPOS (new ObjectRepository: "wordREPO.db"))
(setq freqREPOS (new ObjectRepository: "freqREPOS.db"))
(setq docREPOS (new ObjectRepository: "docREPOS.db"))

(setq matrixREPOS (new ObjectRepository: "matrixREPOS.db"))
 
(beginTransaction wordREPOS)
(beginTransaction freqREPOS)
(beginTransaction matrixREPOS)
(setq myDocIndex (new index myDocIndex: docREPOS create: memory: )) 
(writeln "myDocIndex[0]: " myDocIndex[0])
;; Make word query vector
(setq inString (porterStemmer inString 1))
(setq IndexRefGuide.defaultLexer.charOnly true)
(setq queryVec (IndexRefGuide.defaultLexer inString))
(writeln "queryVec: " queryVec)
(setq iQueryVec (- (length queryVec) 1))
(setq Q (new Vector: Number: (length queryVec)))
(writeln "iQueryVec : " iQueryVec)
(setq totalFreq 0)
(writeln "ref: " (refDirKey queryVec 0))
(loop for n from 0 to iQueryVec 
	(writeln "word: " (refDirKey queryVec n))
	(setq totalFreq (+ freqREPOS[(refDirKey queryVec n)] totalFreq))
	(writeln "totalFreq: " totalFreq)
) ;; end loop

;; get total words evaluated
(setq wordDir (ref wordREPOS (- (length wordREPOS) 1) 1) )
(writeln "wordDir: " wordDir)
(setq totalWordNum (refDirValue wordDir (- (length wordDir) 1))) 
 (writeln "totalWordNum: " totalWordNum)
 (writeln "here1: ")
(setq dWeightVecLen 0)
(setq qWeightDir (new Directory: ))
(setq simDir (new Directory:))
(setq qMagDir (new Directory:))

(setq key (new String: "abc"))
(setq queryMagnitude 0)
(setq n 0)
(setq newDocVec (new Vector: Integer:))
(setq q 0)
	GetQueryTerm::
	(writeln " q: " q)
	(if (> q iQueryVec) (goto ComputeSimilarity:)) 
(writeln "Get query term ")
	;; Search for query words
	;; Get Key
	(setq kp key)
	(setq currentWord (refDirKey queryVec q))
(writeln "currentWord: " currentWord)
;(writeln "type currentWord: " (type currentWord))
	(setq wp currentWord)
	(setq keyLen 0)

	(while (< keyLen 2) do 	
		
		(setq cc wp[0])
		(if (and (< cc 91) (> cc 64)) (+ 32 cc))
	 
		(setq kp[0] cc) 
		(setq kp[1] 0)
		(++ kp)
		(++ wp)
		(++ keyLen)
	) ;; end while
;(writeln "here3: ")
;(writeln "key : " key)
	(setq wordDir (ref wordREPOS key))
;(writeln "wordDir: " wordDir)
	(if (= wordDir #void) (begin (++ q)(goto GetQueryTerm: )))
	(setq qDocVec (new Vector: Integer:))
	(setq awi  (ref wordDir currentWord))
	(writeln "awi: " awi)
	;(writeln "here4: ")
	(if (= awi #void) (begin (++ q) (goto GetQueryTerm:)))
	;; Normalize query:
	
	;; Get the tf * IDF values for the query
	(setq wordFreq freqREPOS[currentWord])
	(writeln "wordFreq: " wordFreq)
	(setq nQuery (/ wordFreq totalFreq))
	(setq nQuery (- (+ nQuery wordFreq) 1))
	(setq qWeight (log10 (ndiv (number totalDocNum) nQuery)))
	(writeln "frequency[currentWord] type: " freqREPOS[currentWord])
	(writeln "totalDocNUm: " totalDocNum)
	;(writeln "totalDocNum: " (type totalDocNum))
	(writeln "qWeight: " qWeight)
	(setq qWeightDir awi qWeight)
	(setq docVec myDocIndex[awi])
	(writeln "docVec : " docVec)
	(setq iDocVec (- (length docVec) 1))
	(loop for d from 0 to iDocVec
	 (setq vecObj docVec[d])
	 (uniqueInsert newDocVec vecObj)
	)
	(if (< q iQueryVec) (begin (++ q) (goto GetQueryTerm:)))
	(writeln "newDocVec : " newDocVec)
	(setq iDocVec (- (length newDocVec) 1))

	ComputeSimilarity::
	(if (= (length qWeightDir) 0) (begin (writeln "No Matching Documents") (goto End:)))

	;; Preparing for query Magnitude computation
	(setq queryMagnitude (+ (* qWeight qWeight) queryMagnitude))


	(writeln "queryMagnitude: " queryMagnitude)


	;; Loop through all the documents where the query term appeared	
 	(loop for m from 0 to iDocVec 
		(setq docNum (ref newDocVec m))	
		(setq xProduct 0)
		(setq dMagnitude 0)
		(setq record (ref matrixREPOS docNum 1))
		(writeln "looping for docNum: " docNum " - " myOntologyNames[docNum])
 		
		(loop for p from 0 to (- (length qWeightDir) 1)	
			(setq awi (refDirKey qWeightDir p))
			(writeln "awi in query vec: " awi) 
			(setq dWeight record[0 awi 0])
			(writeln "dWeight: " dWeight)
 			(setq qWeight qWeightDir[awi])
			(writeln "qWeight: " qWeight)
			(setq xProduct (+ xProduct (* qWeight dWeight)))
			(writeln "xProduct: " xProduct)
		) ;; end loop for docVec	
		(writeln "record 1 : " record[0 totalWordNum 0])
		(writeln "reccord 2 :  "  record[0 (+ totalWordNum 1) 0])
		(writeln "record 3: " record[0 (- totalWordNum 1) 0])
		(setq docMagnitude  record[0 (+ totalWordNum 1) 0])
		(writeln "docMagnitude: " docMagnitude)
		
		(setq simDir docNum (ndiv xProduct (* queryMagnitude docMagnitude)))
		 
	) ;; end loop
 
 	(setq dirValues (refValues simDir))
	(writeln "dirValue: " dirValues)
	(setq sortedVec (sort dirValues > true))
	(writeln "sortedVec: " sortedVec)
	(writeln "== RESULT: == ")
	(loop for x from 0 to (- (length sortedVec) 1)
		(setq vindex (ref sortedVec x) )
		(writeln "vindex: " vindex)
		(setq rankedDocNum (refDirKey simDir vindex))
		(writeln "rankedDocNum: " rankedDocNum)
		(writeln myOntologyNames[rankedDocNum])
		(setq returnedDocs x rankedDocNum)
	) ;; end loop
 

	(commitTransaction wordREPOS)
	(commitTransaction freqREPOS)
	End::
	;(commitTransaction docREPOS)
	(commitTransaction matrixREPOS)
; (myIndex.Pv.close)
returnedDocs)	 
 
 




;;**EXPORTKEY**:IndexRefGuide:wordWeights
(defchild IndexRefGuide:wordWeights()
 
vars: (	lengthXML  					;; Length of currentXML Document evaluated 	
		(Structure:docStruct)		;; Structure containg part of the XML structure to be evaluated
		(Structure:knowledgeBase)	;; Structure containing knowledgebase elements
		(String:newString)			;; Resulting string after XML parsing
		(String:currentWord)		;; Word evaluated in FetchWord:: Label

		weightedDir
	  );; end vars 
regs: (	(Integer:kLen)
		(Integer:docLen)
		(Integer:maxWeight 80)
		(CharPointer:np)			;; Pointer to newString
		(CharPointer:cp)			;; Pointer to currentWord

		) ;; end regs

	;	(writeln "in wordWeights: " )


		(setq myCurrentDocXML (xml porterStemmer.modifiedHTML))
		(setq weightedDir (new Directory:))
 
		;; ===========  Adding weights based on XML heirarchy
		(setq lengthXML (- (length myCurrentDocXML) 1))
		;(writeln "lengthXML: " lengthXML)
		;; Heuristic: (ref myCurrentDocXML lengthXML): The main body of the xml doc
		;; is containedin the last tag
		(setq docStruct (ref myCurrentDocXML lengthXML))
		(setq knowledgeBase docStruct[0])
	;	(writeln "knowlegeBase: " knowledgeBase)
		;; Get all the words in knowledgeBase
		(setq kLen (- (length knowledgeBase) 1))	 
		(loop for n from 0 to  kLen
			;(setq maxWeight (- maxWeight (+ n 5)))
			(setq newString knowledgeBase[n])
		;	(writeln "newString: " newString)
			(setq np newString)
			FetchWordKB::
 		;	(writeln "FetchWordKB: " )
			(setq currentWord "abcdefghijklmnopqrstuvwxyz123456789abcdefghijklmnopqrstuvwxyz123456789")
			(setq cp currentWord)
			;; Run through whitespace and garbage characters
			(setq cc np[0])
		;	(writeln "cc: 1: " cc)
			(while (or (<= (setq cc np[0]) 32) (< cc 0)) do (++ np))
		 
			(while (and (<> (setq cc np[0]) 0) (> cc 32)) do
		;	 	(writeln "cc in while: " cc)
				(setq cp[0] cc)
				(setq cp[1] 0)
				(++ cp)
				(++ np)			
			) ;; end while
		;	(writeln "currentWord: " currentWord)
			(if (or (>=  myDir[currentWord] 0) (= (length currentWord) 1)) (begin  (goto FetchWordKB:))) 
			(setq currentWeight weightedDir[currentWord])
 
			(setq newWeight (+ currentWeight maxWeight))
 
			(setq weightedDir (copy currentWord) newWeight)	
 
			;; Run through whitespace and garbage characters
			(setq cc np[0])
		;	(writeln "cc: 3: " (char cc) " and " (code cc))
			(while (and (<= (setq cc np[0]) 32) (<> cc 0) ) do  (++ np))
		    (if (and (<> (setq cc np[0]) 0) (> cc 32)) (begin (goto FetchWordKB:)))
			(setq maxWeight (- maxWeight (+ n 10)))
	;		(writeln "end loop: " )
		) ;; end loop

#if 0
(writeln "weightedDir: " weightedDir)
		;; Getting Headings
		(setq docLen (- (length docStruct) 1))
(writeln "docSTruct: " docStruct)
		;; Section Headings have the same weight
(writeln "Section: " )
 		(setq maxWeight (- maxWeight 4))
		(loop for n from 1 to  docLen
			(setq newString docStruct[n])
			(if (isStructure newString) (setq newString newString[0]))
	;	(writeln "newString: " newString)
			(setq np newString)
		;	(writeln "newString: " newString)
			FetchWordSection::
;(writeln "Fetch Word SEction: ")
			(setq currentWord "abcdefghijklmnopqrstuvwxyz123456789abcdefghijklmnopqrstuvwxyz123456789")
			(setq cp currentWord)
			;; Run through whitespace and garbage characters
			;(setq cc np[0])
			(while (< (setq cc np[0]) 32) do (++ np))
			(while (and (<> (setq cc np[0]) 0) (> cc 32))  do
				(writeln "cc: " cc)
				(setq cp[0] cc)
				(setq cp[1] 0)
				(++ cp)
				(++ np)			
			) ;; end while
			(if (or (>=  myDir[currentWord] 0) (= (length currentWord) 1)) (goto FetchWordSection:)) 
			(setq weightedDir (copy currentWord) (+ maxWeight weightedDir[currentWord]))
		    ;; Run through whitespace and garbage characters
			(setq cc np[0])
			(writeln "cc: 3: " (char cc) " and " (code cc))
			(while (and (<= (setq cc np[0]) 32) (<> cc 0) ) do  (++ np))
		    (if (and (<> (setq cc np[0]) 0) (> cc 32)) (begin (goto FetchWordSection:)))
			(writeln "end loop: " )
		) ;; end loop

#endif

	;	(writeln "weightedDir: " weightedDir)
 
		;; ======= end weighting function

weightedDir)



