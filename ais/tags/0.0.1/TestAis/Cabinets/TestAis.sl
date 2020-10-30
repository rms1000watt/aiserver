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

;;**EXPORTKEY**:sleepy
(defun sleepy (nSleep)
	vars: (i aNap aResult aTotal)
	(setq aNap 500)
	(loop for i from 0 until nSleep do
		(sleep aNap)				; Nap time in msec
		;(debug traceon:)
		(writeln "sleepy: Nap=" i)
	)
	(setq aTotal (* aNap nSleep))
	(setq aResult (append "sleepy took " nSleep " naps. He slept for " aTotal " mSec."))
	(return aResult)
)

; end


;;**EXPORTKEY**:sumProducts
; Generates a sum of products
(defun sumProducts ()
    vars:( (n1 1) (n2 2) (n3 3) (n4 4) (n5 5) (n6 6) (n7 7))
 
	(setq n2 (* n1 n2))
	(setq n3 (* n2 n3))
	(setq n4 (* n3 n4))
	(setq n5 (* n4 n5))
	(setq n6 (* n5 n6))
	(setq n7 (* n6 n7))
	(setq n6 (* n7 n6))
	(setq n5 (* n6 n5))
	(setq n4 (* n5 n4))
	(setq n3 (* n4 n3))
	(setq n2 (* n3 n2))
	(setq n1 (* n2 n1))
    (+ n1 n2 n3 n4 n5 n6 n7)
	;; Result is 324418353840.0
)

(defun sumRegProducts()
    regs:((Integer:n1 1) (Integer:n2 2) (Integer:n3 3) (Integer:n4 4) (Integer:n5 5) (Integer:n6 6) (Integer:n7 7))
    (vmregRunInHardware start:)
	(vmregMulInteger n1 n2)    
	(vmregMulInteger n2 n3)    
	(vmregMulInteger n3 n4)    
	(vmregMulInteger n4 n5)    
	(vmregMulInteger n5 n6)    
	(vmregMulInteger n6 n7)    
	(vmregMulInteger n7 n6)    
	(vmregMulInteger n6 n5)    
	(vmregMulInteger n5 n4)    
	(vmregMulInteger n4 n3)    
	(vmregMulInteger n3 n2)    
	(vmregMulInteger n2 n1)    
    (vmregRunInHardware stop:)
	(writeln "n1=" n1 ", n2=" n2 ", n3=" n3 ", n4=" n4 ", n5=" n5 ", n6=" n6 ", n7=" n7)
    (+ n1 n2 n3 n4 n5 n6 n7))
	;; Expect 324418353840.0, get 
; end


;;**EXPORTKEY**:syntaxLambda
; Colors
; 0 std		black
; 1 comment	darkGreen
; 2 number		darkBlue
; 3 string		darkMLambdaa
; 4 symbol		darkCyan
; 5 word		darkGray
; 6 keyword 	blue
; 7 label		darkRed
(defun   syntaxLambda()
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
(defchild syntaxLambda:bar3(x)
	(+ x |aV)ar|) 
)
; deforphan
(deforphan syntaxLambda:foobar_(x)
	(writeln "foo:foobar_")
)

; end





;;**EXPORTKEY**:testLambda
;;; TestAis:testLambda
;; Last Update: 8/17/2006

;;	                                 TEST Lambda

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

;; ******************************* ERROR MESSAGES ***************************************
;; Function call:				!Standard function call interface not supported yet!
;; Access denied:				!Requested Lambda is not public!
;; Undefined speech act:	!Requested Lambda does not exist!
;; Undefined target Lambda:	!Target Lambda does not exist!

;; ******************************* AMP RESPONSES ****************************************
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
;; testLambda - A target Lambda for a small test application demonstrating AMP
;; Input:	%testLambda*
;; Return: testLambda*
(defun testLambda(iInitMsg)
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
;; Input:	%testLambdaampActparam1Some Value
;; Return:	testLambdaampActresultInput is Some Value 
(defun ampAct(iMsg)
	faces:((public Amp:))
	vars: (aResult aMsg)

	(if (isMember param1: iMsg)
		(setq aResult (append "Input is " iMsg.param1))
	else
		(setq aResult "param1 is missing")
	)
	(setq aMsg (new Structure: result: aResult))
	;(setq aMsg[_returntype:] 0)		; Optional. _returntype defaults to 0
	;(writeln "Session is " iMsg[_sessionid:])
	;(writeln "_ais.machineName is " _ais.machineName)
	;(writeln "iMsg is " iMsg)
	(return aMsg)
)

;; busyAct - Generates voluminous writelines
;; Input: 	%testLambdabusyActlength100
;; Return:	testLambdabusyAct
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
;; Input:	%testLambdafcnActparamstring input
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
;; Input:	%testLambdafileActfilenamedefault.htm
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
;; Input:	%testLambdanoAccessAct
;; Return:	!Requested Lambda is not public!
;; Note: Result is the same if the faces statement is omitted
(defun noAccessAct(iMsg)
	faces:((public false))
	(return iMsg)
)

;; noReturnAct - An Lambda that never returns
;; Input:	%testLambdanoReturnAct
;; Returns:	(never returns)
(defun noReturnAct(iMsg)
	faces: ((public Amp:))
	;; Wait forever
	(while (true) (sleep 100))
	(return iMsg)
)

;; postHttpAct - Post a request to a server
;; Input: %testLambdapostHttpAct
;; PENDING: add args: %testLambdapostHttpActurllocalhost
;; body#{user: "tmay" passwd: "tim"}
(defun postHttpAct(iMsg)
	faces: ((public Amp:))
	vars: (aBody aMsg)

	; post a request in response to a form
	(postHttp "localhost/amp.dll" #{user: "tmay" passwd: "tim"} )
	(setq aMsg #{ result:  "postHttp to localhost/amp.dll"})
	(return aMsg)
)

;; sendToClientAct - Sends back an unsolicited message to the client
;; Input: %testLambdasendToClientAct
;; Returns:
;;		testLambdasendToClientActmsgAsync message from testLambda|sendToClientAct
;;		testLambdasendToClientActtextmessage has been sent
(defun sendToClientAct(iMsg)
	faces: ((public Amp:))
	vars:(aResult aSessionId aMsg)
	;; Send a message
	(setq aSessionId iMsg[_sessionid:])
	(setq aMsg #{
		testLambda: "pushed"
		msg: "Async message from testLambda|sendToClientAct"})
	(sendToClient aSessionId aMsg)
	;; Set the result
	(setq aResult "message has been sent back to client")

	;; Put the result into the returned structure.
	(setq aRetMsg (new Structure: result: aResult))
	(setq aRetMsg[_returntype:] 0)		; Optional. _returntype defaults to 0
	(return aRetMsg)
)

;; showMsgAct - Return internal message structure
;; Input:	%testLambdashowMsgAct
;; Return:	testLambdashowMsgActsessionid1userid2level7result...
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
;; Input: 	%testLambdasleepyActlength10
;; Return:	testLambdasleepyAct
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
;; Input:	%testLambdatextActinputX%3Da%3C%3Ebarg1v%3D%22quoted%22g
;; Return:	testLambdatextActtextThe input is Bill House
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
	(setq cTargetName "testLambda")
	(return iInitMsg)
)
; end





;;**EXPORTKEY**:testDisconnect

; Main:testDisconnect
; A routine to test the onDisconnect facility.
; Use a browser or other client to establish a connection to AIS, then close the browser.
; This routine will return results to the Context Admin Session.
; testDisconnect is called after the connection to iSessionId has been lost.  No further
; communication with this client is possible.  testDisconnect can send messages to other
; sessions or do housekeeping tasks.
(defun onDisconnect(iSessionId)
faces:((public Amp:))
	vars:(aSessionId aMsg)
	;; Send a message to another session
	(setq aSessionId 1)
	(setq aMsg #{msg: "Message from TestAis:testDisconnect"})
	;; (sendToClient aSessionId aMsg)
)

; end





;;**EXPORTKEY**:testErase
(defun testErase() (writeln {testing...}))

;;**EXPORTKEY**:veryBusy
(defun veryBusy (nLong)
	vars:(i j k )
	(loop for i from 0 until nLong do
		(sleep 50)		; Time in msec.
		(writeln i ": Hello Now from veryBusy.")
	)
	; (writeln "_path1=" _path1)
	; (writeln "_ais=" _ais)
)

; end





;;**EXPORTKEY**:veryLong
; Generates a very long string with nLgth lines of 1000+ characters each.
(defun veryLong (nLgth)
	vars: (i aBody aLine aMsg)
	; (debug traceon:)
	(setq aBody (new String: "1234567890"))
	(setq aBody (rept aBody 100))
	(setq aBody (append #\tab aBody _eol))
	(setq aMsg "")
	(loop for i from 0 until nLgth do
		(setq aLine (append (string i) aBody))
		(setq aMsg (append aMsg aLine))
	)
	(debug traceon:)
	(display aMsg)
)
; end

