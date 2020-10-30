;;/**********************************************************************************
;;    Copyright (C) 2008 Investment Science Corp.
;;
;;    This program is free software: you can redistribute it and/or modify
;;    it under the terms of the GNU General Public License as published by
;;    the Free Software Foundation, either version 3 of the License, or
;;    any later version.
;;
;;    This program is distributed in the hope that it will be useful,
;;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;    GNU General Public License for more details.
;;
;;    You should have received a copy of the GNU General Public License
;;    along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;***********************************************************************************/
;;
;;*************************************
;;*************************************
;; Exported Lambda File Cabinet Document
;;*************************************
;;*************************************

;;**EXPORTKEY**:copyTest2Table
;; **********************************************************************************
;; copyTest2Table - Copy the source table to the destination table.
;; Args:	iSrcTable		The name of the source table.
;;		oDstRepository	A symbol specifiying the destination repository
;;		oDstDb		The name of the destination database
;;		oDstTable		The name of the destination table
;; Returns: true
;; Use:
;;	(copyTest2Table "FASummary19870102" testRepository: "testDatabase" "bigTable")
;; **********************************************************************************
(defun copyTest2Table(iSrcTable oDstRepository oDstDb oDstTable)
	vars:(aColNames aColList aDstCur aLastCol aNRecords aRecord aRow aSrcCur aSrcDb)

	(writeln "iSrcTable=" iSrcTable)
	( if (= iSrcTable oDstTable)
		(begin
			(writeln "Can't copy a table to itself")
			(return false)
		end)
	)
	; Open the source table
	(dataMineLib.clearCursors)
	(setq aSrcCur (dataMineLib.open iSrcTable disk:))
	(setq aLastCol (- aSrcCur.colCount 1))
	(setq aNRecords aSrcCur.recordCount)
	(setq aColNames (aSrcCur.getColumnHeaders 0 aLastCol))
	(writeln "aLastCol=" aLastCol ", aNRecords=" aNRecords)

	; Create the destination table
	(dataMineLib.dropTable oDstTable)
	(dataMineLib.usingForCreate oDstRepository oDstDb)
	(setq aColList (stringToVector aColNames #\tab))
	(writeln "aColList=" aColList)
	(dataMineLib.createTable oDstTable aColList)
	(setq aDstCur (dataMineLib.open oDstTable disk:))
	(writeln "aColNames=" aColNames)

	; Copy the records from the source table into the destination table
	(writeln "Creating table of cursor type =" aDstCur.myCursorType)
	(writeln  "Starting transfer of records")
	(loop for i from 0 until aNRecords do
		(setq aRecord (aSrcCur.read i))
		(aDstCur.write i aRecord)
	)
	(dataMineLib.close aSrcCur)
	(dataMineLib.close aDstCur)
	(writeln "Finished copying " iSrcTable " into " oDstTable)
	(writeln "********************************************")
)

;;**EXPORTKEY**:createTest2Table
;; Main:createTable2                      1.0029                              7/7/03

;;	                             CREATE TEST TABLE

;; createTable is an Lambda to use with the AIS test suite.
;; This Lambda creates a table in the testData repository for testing purposes

;; ADDING A CABINET
;; Cabinets are persistent-storage (files) managed by browse Lambda. In the test suite,
;; there are 2 cabinets, Main to hold the test Lambdas and Tools to hold the DataMine
;; Lambda (and possibly other tools later).  To add a cabinet:
;;	1. In the Lambdas tab, cabinet pane, right-click. Select New Cabinet.  Enter the
;;		new cabinet name and the name of the file.  If the file does not exist, an
;;		empty one will be created for you.
;; 2.	In the same context menu, select Import Cabinet Content... and import the .sl
;;		file of your choice.
;; 3.	In the startup.sl file, add this cabinet to Browse Lambda in the same fashion as
;;		the code for Main.

;;	ADDING A REPOSITORY
;;	Repositories are persistent-storage (files) managed by dataMine Lambda.  In the test
;;	suite, there is one repository named testData.  A repository cannot be a cabinet
;;	and vice versa.  A repository can hold one or more tables.  Each table can belong
;; to a database.  To add a repository:
;;	1. In the startup.sl file, add the repository name and the file path to the
;;		_dataMineExtents definition.  If the repository file does not exist, an empty
;;		repository will be created.
;; 2. Use an Lambda, such as this one, to add or manipulate tables in the repository.

;; EXTENTS
;;	In classical databases, an extent is a chunk of the disk reserved for holding tables,
;; indices, etc.  A database could span one or more extents.  In this discussion, the
;; same concept is applied to the dataMine Lambda which can manage up to 100 repositories
;; which are also referred to as extents.  Each extent can hold zero or more tables.
;; A table cannot span extents.   If two extents have a table by the same name, only the
;; table in the first extent mentioned in the _dataMineExtents definition is visible.

;; IMHO: To extend the analogy from classical databases, it seems to me that each file
;; should be called an extent and the set of extents managed by a dataMine Lambda should
;; be called a repository rather than using these two terms interchangeably.

;; DATABASES
;; Each table has a database attribute.  So a database consists of those tables with
;; a common database name.

;; NAMING CONVENTIONS
;; These naming conventions make your code easier for others to understand, modify,
;;	and maintain.	The following prefixes are recommended as follows:
;;		my		global pvars
;;		_		private child Lambdas
;;		__		speechAct wrappers
;;		c		targetLambda (class) variables
;;		a		local (automatic) vars
;;		i		formal input parameters (input args)
;;		o		formal output parameters (args that are set by called function)
;;		io		formal parameters that are modified (args that are input and output)
;; **************************************************************************************

;; **************************************************************************************
;; createTestTable
;; summary:  Create a table named smallTable with a few records for testing LambdaBase
;;		Internet Server (AIS)
;; Args:     recCount       The number of records to create.
;;           printSW        Switch to turn on/off debug printing.
;; Return:   true
;; **************************************************************************************
(defun createTestTable()
	vars:(dbRec recCount printSW i j cursor x)
	pvars:(aStruct)

	(dataMineLib.usingForCreate testRepository: "testDatabase")
	(dataMineLib.dropTable smallTable:)
	(dataMineLib.createTable smallTable: Name: Title: Salary:)
	(writeln _eol "***************************") 
	(writeln "createTestTable:")
	(setq aStruct #{name: "hello"}) 

	;; Create the table
	(setq recCount 10)
	(setq printSW true)
	(setq cursor (dataMineLib.open smallTable:))
	(writeln "Creating table of cursor type =" cursor.myCursorType)
	(writeln  "Populating smallTable with a few records...")
	(loop for i from 0 until recCount do
		(setq dbRec (new Vector: 3 (append "John" i) aStruct (money (+ $5 i))))
		(cursor.write i dbRec)
		(if printSW (writeln "Writing record [" i "]"))
	)	;; end loop
	(setq cursor (dataMineLib.close cursor))
	(writeln "Finished creating smallTable in the testRepository repository")
	(writeln "***************************")
	true) ;; end createTestTable

;;**EXPORTKEY**:readTest2Table
;; **************************************************************************************
;; readTest2Table
;; summary:  Read the table named smallTable to check contents.
;; Return:   true
;; **************************************************************************************
(defun readTest2Table()
	vars:(dbRec i cursor row)

	(setq cursor (dataMineLib.open smallTable:))
	(loop for i from 0 until 10 do
		(setq row (cursor.read i))
		(writeln i ": " row)
	) ;; end loop
	(setq cursor (dataMineLib.close cursor))
	(writeln "Finished reading smallTable from readTest2Table")
)

;;**EXPORTKEY**:sleepy2
(defun sleepy2 (nSleep)
	vars: (i j)

	(loop for i from 0 until nSleep do
		(sleep 100)				; Time in msec.
		(writeln "sleepy2: i=" i)
	)
	(writeln "done")
)

;;**EXPORTKEY**:syntaxLambda2
; Colors
; 0 std		black
; 1 comment	darkGreen
; 2 number		darkBlue
; 3 string		darkMLambdaa
; 4 symbol		darkCyan
; 5 word		darkGray
; 6 keyword 	blue
; 7 label		darkRed
(defun   syntaxLambda2()
	vars:(aMessage |aV)ar|)	; Words
pvars:(foobar_ frybar.)
LABEL::
	(setq |aV)ar| 123456)
	(setq aMessage "a(b" )
	(defriend frybar.()
	 	writeln("defriend")
	)
)
; defchild
(defchild syntaxLambda2:bar3(x)
	(+ x |aV)ar|) 
)
; deforphan
(deforphan syntaxLambda2:foobar_(x)
	(writeln "foo:foobar_")
)

;;**EXPORTKEY**:test2Lambda
; Main:test2Lambda                      1.0029                               7/7/03

;;	                                TEST2 Lambda

;; Lambda to use with the WebTest test suite.
;; This Lambda also serves as a template for the AMP Lambda infrastructure
;; DOCUMENTATION
;;		ais/Lambda Message Protocol.doc

;; DEBUGGING
;;	1.	Insert the following line of code wherever you wish to start debugging:
;;			(debug traceon:)

;; BUILT-IN COMMANDS
;; Here are a few built-in commands:
;;	%_ampopencontextcontext%ssizeMb%d
;; %_ampopensessioncontext%suserid%dseclvl%d

;; **************************************************************************************

;; ************************************ NOTES *******************************************
;; 1. Faces
;; The faces statement must be included in the definition of a target Lambda or
;; speech act that is accessed via an AMP call to the engine.  Here are the options:
;; faces:((public false))	; No access. This is the default.
;; faces:((public true))	; Functional interface. Just the values of the name-value
;;		pairs in the AMP message are passed to the function. (not currently implemented)
;; faces:((public Amp:))	; AMP interface. AMP message structure passed to Lambda.
;; The target Lambda faces statement controls the access  to the just the target Lambda.
;; The speech act can utilize an AMP interface even if the target Lambda has no access.

;; 2. AMP Message
;; The client AMP message format depends upon the protocol used.  HTTP uses a query
;; string, XML uses and XML document with a root element and one child element for each
;; name-value pair.  The internal representation of the AMP input message is a DEL-
;; delimited string.  The possible formats are:
;;		myTargetLambda|*|myArgName|argValue|...
;;		myTargetLambda|mySpeechAct|myArgName|argValue|...
;; In the first case, the Lambda myTargetLambda is called. In the second case, the Lambda
;; mySpeechAct is called.  If public is set to Amp: the AMP input message is converted
;; to an AMP Message Structure and passed to the Lambda.

;; 2. AMP Message Structure
;; The incoming AMP message is used to form the AMP Message Structure which is passed
;; to the Lambda specified in the AMP message.  In addition to the name-value pairs
;; in the AMP message, the AMP Message Structure includes the following elements:
;;		someLambda: "someSpeechAct"
;;		_sessionid: 0
;;		_userid: 1
;;		_level: 7

;; 3. Invoking an AMP message from the console
;; An AMP message may be passed to the engine from the console command line edit bar.
;; Use a leading % sign followed by the elements of the AMP message separated with
;; DEL characters.  Use the Ctrl-DEL key to separate AMP message elements. For example,
;;		%myTargetLambdamySpeechActmyArgNameargValue
;; The above dummy names must be replaced with the actual names for the Lambda being
;; called. The error message "

;; 4. Lambda Structure
;; First, define a target Lambda.  The target Lambda should contain one child Lambda for
;; each speech act to be provided.  Put the body of the target Lambda, if any, at the
;; end.  The main body of the target Lambda is typically used to perform initialization
;; or other housekeeping tasks.

;; ******************************* ERROR MESSAGES ***********************************
;; Function call:				!Standard function call interface not supported yet!
;; Access denied:				!Requested Lambda is not public!
;; Undefined speech act:	!Requested Lambda does not exist!
;; Undefined target Lambda:	!Target Lambda does not exist!

;; ******************************* AMP RESPONSES ************************************
;; The format of the returned response is determined by the client protocol.  For
;; HTTP clients, the response is a DEL-delimited string (usually intercepted by java
;; script). For XML clients, the response is an XML document.  The root element is
;; named amp and includes the initiating target Lambda and speech act as attributes.
;; The name-value pairs in the returned message structure are returned in child
;; elements of the root element. 
 
;; The AMP response is shown in the following examples as text set of name-value
;; pairs, separated by a DEL character.
 
;; ****************************** DISPLAY OUTPUT ************************************
;; Display output (from display, writeln, etc) is not returned to XML and HTTP clients.
;; However, any APP client can request display output which appears in the consoleOut
;; tab if the user enables output to this pane in the AIS status window.
  
;; ******************************* PUSHED OUTPUT ************************************
;; Send-to-client can generate unsolicited output.  HTTP clients cannot receive
;; pushed output. XML clients receive send-to-client output in the same format as
;; described above for AMP responses.   Only APP clients receive display output.

;; ******************************** TARGET Lambda ************************************
;; NAMING CONVENTIONS
;; These naming conventions make your code easier for others to understand, modify,
;;	and maintain.	The following prefixes are highly recommended:
;;		my		global pvars
;;		_		private child Lambdas (single underscore)
;;		c		targetLambda (class) variables
;;		a		local (automatic) vars
;;		i		formal input parameters (input args)
;;		o		formal output parameters (args that are set by called function)
;;		io		formal parameters that are modified (args that are input and output)
;;
;; I use two semicolons for documentation and a single semicolon for comments.
;; **********************************************************************************
;; test2Lambda - A target Lambda for a small test application demonstrating AMP
;; Input:	%test2Lambda*
;; Return: test2Lambda*
(defun test2Lambda(iInitMsg)
pvars:(						; Global persistent variables (prepended with my)   
	mySelf					; Self reference
	; Public child Lambdas (these functions form the API for this Lambda)
	ampAct					; Test returning an AMP DEL-delimited string
	fcnAct					; Example of a functional interface
	fileAct					; Fetches a file from AIS
	noAccessAct				; Access denied. Returns error message
	noReturnAct				; Lambda never returns (waits forever)
	postHttpAct				; Posts a request to the named web server
	sendToClientAct			; Sends an unsolicited message to client
	showMsgAct				; Returns contents of internal msg structure
	sleepyAct				; Spends a lot of time sleeping
	textAct					; Returns plain text
	urlAct					; Returns a url suitable for redirection
	; Private child Lambdas (one underscore)
	_privateAct				; Not externally accessible
)
vars:(						; Local class variables (prepended with c)
	cTargetName				; Only accessible by child Lambdas and target Lambda
)
faces:(
		(public Amp:) 		; This Lambda accepts and returns an AMP message
		(security 1)		; Anyone can request this Lambda or its children
	)

;; ******************************** SPEECH ACTS *************************************
;; ampAct - Return type 0. Example of an AMP call
;; Input:	%test2LambdaampActparam1Some Value
;; Return:	test2LambdaampActresultInput is Some Value 
(defun ampAct(iMsg)
	faces:((public Amp:))
	vars: (aResult aMsg)

	(if (isMember param1: iMsg)
		(setq aResult (append "Input is " iMsg.param1))
	else
		(setq aResult "param1 is missing")
	)
	(setq aMsg (new Structure: result: aResult))
	; (setq aMsg[_returntype:] 0)		; Optional. _returntype defaults to 0
	;(writeln "AmpAct: Session is " iMsg[_sessionid:])
	;(writeln "_ais.machineName is " _ais.machineName)
	;(writeln "iMsg is " iMsg)
	(return aMsg)
)

;; busyAct - Generates voluminous writelines
;; Input: 	%test2LambdabusyActlength100
;; Return:	test2LambdabusyAct
(defun busyAct(iMsg)
	faces:((public Amp:))
	vars: (aMsg i n)

	(if (isMember length: iMsg)
		(begin
			(setq n (integer iMsg[length:]))
			(^delete iMsg length:)
		end)
	else
		(setq n 200)
	)
	(loop for i from 0 until n do
		(writeln i ": busy");
	)
	(setq aMsg #{ result:  "busyAct finished."}) 
	(return aMsg)
)

;; fcnAct - Example of a functional interface
;; Note: The value of the first parameter in the AMP message is passed to fcnAct
;; Input:	%test2LambdafcnActparamstring input
;; Return:	!Standard function call interface not supported yet!
(defun fcnAct(iParamValue)
	faces:((public true))
	vars:(aRet)
	(if (= iParamValue "string input")
		(setq aRet "Expected arg")
	else
		(setq aRet "Unexpected arg")
 	)
	(return aRet)
)

;; fileAct - Return type 2. Return contents of a file.
;; Input:	%test2LambdafileActfilenamedefault.htm
;; Return:	(contents of default.htm)
(defun fileAct(iMsg)
	faces:((public Amp:))
	vars:(aResult)

	(if (isMember filename: iMsg)
		(begin
			(setq aResult iMsg[filename:])
			(^delete iMsg filename:)
		end)
	else
		(setq aResult "default.htm")
	)
	(setq iMsg[_file:] aResult)
	(setq iMsg[_returntype:] 2)		; Return contents of file
	; iMsg = #{testLambda: "fileAct" _sessionid: 2 _userid: 3 _level: 7
	;    _file: "default.htm" _returntype: 2}
	(writeln iMsg)
	(return iMsg)
)

;; noAccessAct - No external access to this Lambda allowed
;; Input:	%test2LambdanoAccessAct
;; Return:	!Requested Lambda is not public!
;; Note: Result is the same if the faces statement is omitted
(defun noAccessAct(iMsg)
	faces:((public false))
	(return iMsg)
)

;; noReturnAct - An Lambda that never returns
;; Input:	%test2LambdanoReturnAct
;; Returns:	(never returns)
(defun noReturnAct(iMsg)
	faces: ((public Amp:))
	;; Wait forever
	(while (true) (sleep 100))
	(return iMsg)
)

;; postHttpAct - Post a request to a server
;; Input: %test2LambdapostHttpAct
;; PENDING: add args: %test2LambdapostHttpActurllocalhost
;; body#{user: "tmay" passwd: "tim"}
(defun postHttpAct(iMsg)
	faces: ((public Amp:))
	vars: (aBody)

	; post a request in response to a form
	(postHttp "localhost/amp.dll" #{user: "tmay" passwd: "tim"} )
	(setq aMsg #{ result:  "postHttp to localhost/amp.dll"})
	(return iMsg)
)

;; sendToClientAct - Sends back an unsolicited message to the client
;; Input: %test2LambdasendToClientAct
;; Returns:
;;		test2LambdasendToClientActmsgAsync message from testLambda|sendToClientAct
;;		test2LambdasendToClientActtextmessage has been sent
(defun sendToClientAct(iMsg)
	faces: ((public Amp:))
	vars:(aResult aSessionId aMsg)
	;; Send a message
	(setq aSessionId iMsg[_sessionid:])
	(setq aMsg #{
		test2Lambda: "pushed"
		msg: "Async message from test2Lambda|sendToClientAct"})
	(sendToClient aSessionId aMsg)
	;; Set the result
	(setq aResult "message has been sent back to client")

	;; Put the result into the returned structure.
	(setq aRetMsg (new Structure: result: aResult))
	(setq aRetMsg[_returntype:] 0)		; Optional. _returntype defaults to 0
	(return aRetMsg)
)

;; showMsgAct - Return internal message structure
;; Input:	%test2LambdashowMsgAct
;; Return:	test2LambdashowMsgActsessionid1userid2level7result...
(defun showMsgAct(iMsg)
	faces:((public Amp:))
	vars: (aMsg aKey aValue aStg)

	; (writeln iMsg)
	(setq aMsg (copy iMsg))
	(loop for i from 0 until (length aMsg) do
		(setq aStg (string aMsg[i 0]))
		(if (= (substring aStg 0 0) "_")
			(begin
				(setq aValue aMsg[i 1])
				(setq aStg (string aMsg[i 0]))
				(setq aKey (symbol (substring aStg 1)))
				(setq aMsg[aKey] aValue) 
			end)
		)
	)
	(setq aMsg[result:] "internal message structure")
	(return aMsg)
)

;; sleepyAct - Spends a lot of time sleeping
;; Input: 	%test2LambdasleepyActlength10
;; Return:	test2LambdasleepyAct
(defun sleepyAct(iMsg)
	faces:((public Amp:))
	vars: (aMsg aMsec aResult i n)

	(if (isMember length: iMsg)
		(begin
			(setq n (integer iMsg[length:]))
			(^delete iMsg length:)
		end)
	else
		(setq n 20)
	)
	(setq aMsec 500)
	(loop for i from 0 until n do
		(sleep aMsec)				; Time in msec.
		(writeln i ": I'm awake")
	)
	(setq aResult (append "sleepy slept " n " times for " aMsec " msec."))
	(setq aMsg (new Structure: result: aResult))
	(setq aMsg[_returntype:] 0)		; Optional. _returntype defaults to 0
	(return aMsg)
)

;; textAct - case 3. Returns mimetext to client.
;; Input:	%test2LambdatextActinputX%3Da%3C%3Ebarg1v%3D%22quoted%22g
;; Return:	test2LambdatextActtextThe input is Bill House
(defun textAct(iMsg)
	faces: ((public Amp:))
	vars:(aResult aArg0 aArg1)

	; Use the url-encoded string: arg0|X=a<>b|arg1|v="quoted"g
	; For now, just use a couple default values
	;(if (isMember arg0: iMsg)
	;	(setq aArg0 iMsg[arg0:])
	;else  
		(setq aArg0 "X%3Da%3C%3Eb")
	;)
	;(if (isMember arg1: iMsg)
	;	(setq aArg1 iMsg[arg1:])
	;else  
		(setq aArg1 "v%3D%22quoted%22g")
	;)
	(setq aResult (append "arg0=" aArg0 "&arg1=" aArg1))
	(setq aMsg (new Structure: _mimetext: aResult))
	(setq aMsg[_enctype:] "application/x-ampmsg")
	(setq aMsg[_returntype:] 3)		; Return mimetext encoded as a query string 
	(return aMsg)
)

(defun timsact(msg)
	faces: ((public Amp:))
	(setq msg (new Structure: screenID: 2 key: #(41 -20.0 1 -10.0 1 -5.0 2 )
		pickstable: "yada yada"))
msg)

;; urlAct - Return type 1. Returns request to redirect to another page.
;; Input: 	%test2LambdaurlActredirectTohttp://localhost/
;; Return:	test2LambdaurlActurlhttp://localhost/
(defun urlAct(iMsg)
	faces:((public Amp:))
	vars: (aMsg aUrl)
	
	(if (isMember redirectTo: iMsg)
		(begin
			(setq aUrl iMsg[redirectTo:])
			(^delete iMsg redirectTo:)	; Remove input parameter from return msg
		end)
	else
		(setq aUrl "http://localhost/redirect.htm")
	)
	(setq aMsg (new Structure: _url: aUrl))
	(setq aMsg[_returntype:] 1)		; Send request to redirect to url
	(return aMsg)
)

;; _initLambda - A private Lambda that is only accessible locally
(defun _initLambda()

	(setq myInitialized true)
	(return true)
)

;; ****************************** TARGET Lambda CODE *********************************
;; Main body of the target Lambda.  Mostly used for setup and tear down
	(if (= mySelf #void) (setq mySelf (myself)))
	(if (= myInitialized #void)
		 (_initLambda)
	)
	(setq cTargetName "test2Lambda")
	(return iInitMsg)
)

;;**EXPORTKEY**:test2Filter
(defun test2Filter ()
	vars:(i cursor)
	;;(debug traceon:)
	(setq cursor (dataMineLib.open "FASummary19870102" memory:))
	(cursor.run "bottom PriceToValue 50;")
	(cursor.show 0)
	(setq cursor (dataMineLib.close cursor))
)

;;**EXPORTKEY**:veryBusy2
(defun veryBusy2 (nLong)
	vars:(i j k )
	(loop for i from 0 until nLong do
		;(sleep 10)			; Time in msec.
		(writeln i ": Hello from veryBusy2")
	)
	;	(writeln "_path=" _path)
	;	(writeln "_ais=" _ais)
)

;;**EXPORTKEY**:veryLong2
; Generates a very long string with nLgth lines of 100+ characters each.
(defun veryLong2 (nLgth)
	vars: (i aBody aLine aMsg)
	(setq aBody (new String: "1234567890"))
	(setq aBody (rept aBody 10))
	(setq aBody (append #\tab aBody _eol))
	(setq aMsg "")
	(loop for i from 0 until nLgth do
		(setq aLine (append "Line:" (string i) aBody))
		(setq aMsg (append aMsg aLine))
	)                                                         
	(display aMsg)
)
