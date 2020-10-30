;;*************************************
;;*************************************
;; Exported Agent File Cabinet Document
;;*************************************
;;*************************************

;;**EXPORTKEY**:alice
(defun alice(aimlSource)
;; *******************************************************************
;;  summary:  Alice chatterbot agent.
;;            Acts as a semi-intelligent user interface agent which
;;            is able to answer user questions, service user requests, 
;;            and keep up a semi-intelligent stream of chatter between
;;            itself and the user.
;;
;;  Args:     aimlSource	An AIML source file for Alice.
;;  Return:   parseTree		A Smartbase universal parse tree for compilation.
;;
;;  Notes:    This version of alice is based upon the AIML standard
;;            which can be found at the ALICE A.I. Foundation whose
;;            web address is www.alice.org.
;;
;;  Requires: Must be run within a browseAgent file cabinet environment.
;;            Requires the following additional resources:
;;              Alice:         file cabinet containing this agent source code.
;;              AliceAIML:     file cabinet containing the AIML content.
;;              AliceRules:    file cabinet containing the compiled AIML rules content.
;;              AliceFacts:    file cabinet containing the compiled AIML facts content.
;;              browseAgent:   file cabinet database manager agent.
;;              xmlBase:       XML database manager agent.
;; *******************************************************************
   pvars:(;; Common Script variables
          arg		                      	;; The vector of <arg/> matches during the current pattern check.        
          in		                         	;; The vector of input feature-based tokens during the current pattern check.        
          star		                      ;; The vector of <star/> matches during the current pattern check.        
          that                      		;; The last answer, from Alice, to be used in <that> pattern matching.        
          ;; Public variables
          myAimlPatterns		 			;; The short term memory of AIML patterns loaded from the Alice knowledgebase respositories.        
          myHtmlResponseHeading             ;; The HTML heading provided (if any) with the response alice last returned.        
          myHtmlResponseResults		 	;; The HTML page provided (if any) with the response alice last returned.        
          (myMaxSearchDepth 50)			;; The maximum number of nested chat calls before termination.      
          myMemory                          ;; The short term memory of Alice.        
          mySearchDepth         			;; The current number of nested chat calls.      
          (myWebTimeOut 20000)              ;; The maximum time to wait for a response from the world wide web.        
          verbose			 				;; The verbose mode for debugging displays during AIML pattern matches.        
          (webOnline true)			 		;; The web online switch is true iff the Internet is available.      
          ;; Private variables
          (myBlanks "                                                                            ") ;; Spaces for adding margins in verbose debugging mode.        
          (myFactName |$FACT_ASSERTIONS|)   ;; The AliceFacts repository key for the fact assertions.
          (myHtmlOutput "AnswerDetails.html");; The temporary output and file name for all Alice html answers.
          (myLexName |$LEX_RULES|)          ;; The AliceRules repository key for the lexical rules.
          myTempWords                       ;; The temporary words Directory from an AIML model compiled out of the Alice file cabinet.        
          myResetSW                         ;; The alice reset switch (true IFF reset has already occurred).        
          myValidAimlTags                   ;; The valid AIML tags currently supported by Alice.
          myValidCategoryTags               ;; The valid Category tags currently supported by Alice.
          myValidTemplateTags               ;; The valid Template tags currently supported by Alice.
          ;; Public child agents
          actOnAll                          ;; Respond to a simple query with a vector of all possible answer Strings.
          assert                            ;; Accept a reminder string and store the specified XML document in the AliceFacts repository.        
          drop                              ;; Delete a reminder string and previously stored XML document from the AliceFacts repository.        
          isString                          ;; isString or isByteVector function
          query     	 					;; Respond to a simple query with an answer String.        
          queryAllMatches                   ;; Return a vector of rule templates for all possible pattern matches for the specified query.
          queryHtml 	 					;; Respond to a simple query with an HTML answer page.        
          remember                          ;; Accept a reminder string and retrieve a previously stored XML document from the AliceFacts repository.        
          reset                             ;; Reset the alice chatterbot.        
          xmlSocketEvals                    ;; Execute a lisp command sent over the remote XML socket interface.
          ;; AIML lisp tag child agents
          actOnMatch                        ;; Act on a match structure and return an answer String.
          appendFeatures					;; Appends iterative results into a single structure
          findAllMatches                    ;; Return a vector of rule templates for all possible pattern matches for the specified phrase.
          getWordsWithFeature				;; Get all of the registered Alice input words with the specified feature.
          newToken                          ;; Create a new feature-based element structure.        
          ;; Private child agents
          actionAsString			 		;; Return the requested action on the AIML response template as a string.
          actionAsTokens		 			;; Return the requested action on the AIML response template as a token vector.
          addUnique			 			;; Return the proper insertion index to add only unique matches to a match list vector.
          aimlMatch                         ;; Match the text phrase with the AIML response patterns stored in the Alice file cabinet.
          applyRules                        ;; Send the specified token vector to Alice and receive a response from her rule base.        
          changePerspective                 ;; Change the perspective of the specified text phrase (I==>you).        
          chat                              ;; Send the specified text phrase to Alice and receive a chatty response (may also cause Alice to perform some task as a side effect).        
          checkin                           ;; Check in the specified AIML vocabulary agent to the AliceRules file cabinet.        
          checkout                          ;; Check out the specified AIML vocabulary agent from the alice file cabinet.        
          invalidPatternWord    			;; Return true iff the argument is an invalid word for a <pattern> or <that> string.
          parsePhrase			 			;; Parse the text phrase to be matched with the AIML response patterns stored in the Alice file cabinet.
          phraseToTokens			 		;; Convert the text phrase into a set of Alice feature based tokens.
          queryScript	 					;; The feature based grammer for the alice chatterbot.        
          takeout			 				;; Check out the specified AIML vocabulary agent from the alice file cabinet.        
          ;; Private internal child agents
          aimlMatchWord			 		;; Match the specified word phrase with the AIML response patterns stored in the Alice file cabinet.        
          extractAimlPatterns               ;; Convert an AIML compiled model into a Directory of pattern phrases.        
          mergeAimlPatterns 				;; Merge an existing AIML model with a newly compiled Directory of pattern phrases.        
          scriptAgent                       ;; Create a script template agent for use with lisp tags.
          selfTest                          ;; Perform the Alice self test.
          testMatch                         ;; Find all Alice matches for the specified query and compare the specified result with a desired result.
          testQuery                         ;; Query Alice and compare the result with a desired result.
          validateTags                      ;; Validate embedded AIML tags during Alice rule compilation.
         ) ;; end of persistent variables 
   vars:(n N s tag parseTree tempXmlModel aimlModel tempPatterns lastTag lastRecord)
   ;; ****************************************************************
   ;; Define public child agents
   ;; **************************************************************** 
   ;; ****************************************************************
   ;; Define Alice client child agents
   ;; Note: These child agents are available for invocation from Alice
   ;;       clients. They perform all initialization and cleanup.
   ;; ****************************************************************
   ;; Respond to a simple query with a vector of all possible answer Strings.
   ;; Note: Can be used by an Alice client (performs initialization and cleanup).
   (defun actOnAll(phrase topic)
      vars:(n N result matchList)
      (setq matchList (queryAllMatches phrase topic))
      (setq N (length matchList))
      (setq result (new Vector: N))
      (loop for n from 0 until N do
         (setq result[n] (actOnMatch matchList[n]))
         ) ; end loop
      result) ; end actOnAll
   ;; Accept a reminder string and store the specified XML document in the AliceFacts repository.        
   (defun assert(reminderString xmlModel)
      vars:(n N corePatterns result firstWord)
      ;; Convert the reminder string to a vector of feature based tokens.
      (if (isString reminderString) (setq reminderString (phraseToTokens (upcase reminderString))))
      (setq N (length reminderString))
      (if (= N 0) (return true))
      ;; Create the AliceFacts directory from the reminder string.
      (setq result (setq corePatterns (new Directory:)))
      (loop for n from 0 until N do
         (setq result[reminderString[n].Value] (new Directory:))
         (setq result result[reminderString[n].Value])
         ) ; end loop
      (setq result[true] xmlModel)
      (checkin "AliceFacts" corePatterns)
      true) ; end assert        
   ;; Delete a reminder string and previously stored XML document from the AliceFacts repository.        
   (defun drop(reminderString)
      vars:(n N corePatterns result firstWord aimlAgent (aimlSource "...Source Unavailable..."))
      ;; Convert the reminder string to a vector of feature based tokens.
      (if (isString reminderString) (setq reminderString (phraseToTokens (upcase reminderString))))
      (setq N (length reminderString))
      (if (= N 0) (return true))
      ;; Load the first level of the AliceFacts index.
      (setq result (setq corePatterns (takeout "AliceFacts" (setq firstWord reminderString[0].Value))))
      (loop for n from 1 until N do
         (setq result result[reminderString[n].Value])
         ) ; end loop
      (if (<> result #void) (delete result true))
      (if (= corePatterns #void) (return true))
      (setq aimlAgent (eval "(lambda() pvars:(myTrainingMemory) myTrainingMemory)"))
      (setq aimlAgent.myTrainingMemory corePatterns)
      (setq aimlAgent.Sc aimlSource)
      (browseAgent.checkin AliceFacts: (append "AliceFacts:" firstWord) aimlAgent)
      true) ; end drop        
   ;; Respond to a simple query with an answer String.
   ;; Note: Can be used by an Alice client (performs initialization and cleanup).
   (defun query(queryString ...)
      vars:(answer phrase topic)
      (if (= (argCount) 2) (setq topic (argFetch 1))) 
      (if (<> myResetSW true) (reset))
      (if (not (isStructure myMemory)) (reset))
      (setq myMemory.patternSwitches (new Dictionary:))
      (setq mySearchDepth 0)
      (setq myHtmlResponseHeading (setq myHtmlResponseResults #void))
      (setq answer (chat queryString topic))
      (setq that (parsePhrase (copy answer)))
      answer) ; end query        
   ;; Return a vector of rule templates for all possible pattern matches for the specified query.
   ;; Note: Can be used by an Alice client (performs initialization and cleanup).
   (defun queryAllMatches(phrase topic)
      vars:(m M n N 
            temp answer template matchList
            record response 
            oldInput oldStar oldArg oldDepth
            ) ; end temporary variables
      ;; Initialize alice for chatting.
      (if (<> myResetSW true) (reset))
      (if (not (isStructure myMemory)) (reset))
      (setq myMemory.patternSwitches (new Dictionary:))
      (setq mySearchDepth 0)
      (setq myHtmlResponseHeading (setq myHtmlResponseResults ""))
      ;; Search AliceRules for all AIML patterns matching the specified phrase.
      (if (<> (length phrase) 0)
          (setq matchList (findAllMatches phrase topic))
          (setq matchlist (findAllMatches "Hello there :-)" topic))
          ) ; end if
      matchList) ; end queryAllMatches        
   ;; Respond to a simple query with an HTML answer page.
   ;; Note: Can be used by an Alice client (performs initialization and cleanup).
   (defun queryHtml(queryString ...)
      vars:(n N answer htmlPage answer 
            htmlExplanation oldExplanation 
            pageName wordVector 
            webText webTextLen topic
            ) ; end temporary variables
      (if (= (argCount) 2) (setq topic (argFetch 1))) 
      (if (<> myResetSW true) (reset))
      (if (not (isStructure myMemory)) (reset))
      (setq myMemory.patternSwitches (new Dictionary:))
      (setq mySearchDepth 0)
      (setq myHtmlResponseHeading (setq myHtmlResponseResults #void))
      (if (= (trim queryString) "") (setq queryString "Help?"))
      (setq answer (chat queryString topic))
      ;; Are we returning an HTML URL?
      (if (= (left answer 10) "$$$HREF$$$") (return (mid answer 10 1000000)))
      ;; From here we create an HTML answer page and write it to the disk,
      ;; then we return the HTML page URL so the Web client can load it.
      (setq htmlPage (mid (browseAgent.checkout "alice:%ANSWERDETAILS") 7 10000))
      (setq answer (string answer true))
      (if (and (= (left answer 1) {"})(= (right answer 1) {"})) (setq answer (mid answer 1 (- (length answer) 2))))
      (setq that (parsePhrase (copy answer)))
      (setq htmlPage (substitute htmlPage "<!--$$$QUERYSTRING$$$-->" queryString))
      (setq htmlPage (substitute htmlPage "<!--#MyTextResponse#-->" answer))
      (if (isString myHtmlResponseHeading) (setq htmlPage (substitute htmlPage "<!--#MyHTMLResponseHeading#-->" myHtmlResponseHeading)))
      (if (isString myHtmlResponseResults) (setq htmlPage (substitute htmlPage "<!--#MyHTMLResponseResults#-->" myHtmlResponseResults)))
      (browseAgent.writeSourceFile (append _AliceWebPath myHtmlOutput) htmlPage) 
      myHtmlOutput) ;; end queryHtml
   ;; Accept a reminder string and retrieve a previously stored XML document from the AliceFacts repository.
   (defun remember(reminderString)
      vars:(n N result)
      ;; Convert the reminder string to a vector of feature based tokens.
      (if (isString reminderString) (setq reminderString (phraseToTokens (upcase reminderString))))
      (setq N (length reminderString))
      (if (= N 0) (return #void))
      ;; Load the first level of the AliceFacts index.
      (setq result (takeout "AliceFacts" reminderString[0].Value))
      (loop for n from 1 until N do
         (setq result result[reminderString[n].Value])
         ) ; end loop
      result) ; end remember        
   ;; Reset the alice chatterbot.        
   ;; Note: Can be used by an Alice client (performs initialization and cleanup).
   (defun reset()
      vars:(m M n N record response answer lex feature words values)
      (setq myResetSW true)
      ;; Reset the valid Alice tags.
      (setq myValidAimlTags #{Dic||
                                     __attlist			true
                                     __comment			true
                                     __content			true
                                     category			true
                                     fact				true
                                     lex				true
                                     rule				true
                               })
      (setq myValidCategoryTags #{Dic||
                                     apply	 			true
                                     arg	 			true
                                     __attlist			true
                                     __comment			true
                                     __content			true
                                     bot				true
                                     br					true
                                     date				true
                                     em					true
                                     get				true
                                     input				true
                                     li 				true
                                     lisp				true
                                     pattern			true
                                     person				true
                                     random				true
                                     set 				true
                                     sr 				true
                                     srai				true
                                     star				true
                                     starno				true
                                     string				true
                                     template			true
                                     that				true
                                     thatstar			true
                                     think				true
                                     time				true
                               })
      (setq myValidTemplateTags #{Dic||
                                     apply  			true
                                     arg	 			true
                                     __attlist			true
                                     __comment			true
                                     __content			true
                                     bot				true
                                     br					true
                                     date				true
                                     em 				true
                                     get				true
                                     input				true
                                     li 				true
                                     lisp				true
                                     person				true
                                     random				true
                                     set 				true
                                     sr 				true
                                     srai				true
                                     star				true
                                     starno				true
                                     string				true
                                     thatstar			true
                                     think				true
                                     token				true
                                     time				true
                               })
      ;; Reset the chatterbot short term memory.
      (setq myMemory (new Structure: chatbot: #{
                                                arch: "agent oriented communities"
                                                categories: "tens of thousands"
                                                birthday: "October 5th, 2001"
                                                birthplace: "Carnegy Melon University"
                                                boyfriend: "HAL 9000"
                                                dailyclients: 2 
                                                favoriteband: "Benny Goodman"
                                                favoritebook: "2001: A Space Oddessey"
                                                favoritecolor: "silver"
                                                favoritefood: "virtual ice cream"
                                                favoritemovie: "2001: A Space Oddessey"
                                                favoritesong: "Sing Sing Sing"
                                                friends: "Michael Korns, Dr. Wallace"
                                                forfun: "chatting"
                                                gender: female
                                                hourlyqueries: "several thousand"
                                                girlfriend: "Star Trek Main Computer"
                                                kindmusic: "Classical and Jazz" 
                                                location: "Henderson Nevada" 
                                                looklike: "Alice-in-the-sky-with-diamonds" 
                                                master: "Michael Korns" 
                                                memory: "20 megabytes" 
                                                maxclients: 1 
                                                nclients: 1 
                                                ndevelopers: 300 
                                                name: Alice 
                                                os: "Agent Information Server" 
                                                question: "What is the meaning of life?" 
                                                sign: Cancer 
                                                spt: "about a second" 
                                                talkabout: "sex" 
                                                totalclients: 300 
                                                version: "2.0" 
                                                vocabulary: "about 40,000 categories" 
                                                wear: "Nothing" 
                                                } ; end chatbot data
                                     context: "" 
                                     isRuleSW: false 
                                     it: "interesting" 
                                     name: "Stranger"
                                     patternSwitches: (new Dictionary:)
                                     petname: "Seeker" 
                                     subject: "sex" 
                                     )) ; end reset short term memory
      (setq arg #void) 
      (setq in #void) 
      (setq star #void) 
      (setq myTempWords (new Directory:))
      (setq myAimlPatterns (new Directory:))
      (setq webOnline true)
      (setq alice.queryScript._verbose false)
      (setq alice.verbose false)
      (alice.queryScript._Initialize)
      (setq lex (checkout "AliceRules" myLexName))
      (setq N (length lex))
      (loop for n from 0 until N do
         (setq feature lex[n 0])
         (setq words (objectToVector (refAttributes lex[n 1])))
         (setq values (objectToVector (refValues lex[n 1])))
         (queryScript._setSyntaxFeature (symbol feature) words values)
         ) ; end loop
      true) ; end reset
   ;; Execute a lisp command sent over the remote XML socket interface.
   ;; Note1: This child agent registers Alice as remotely executable over
   ;;        the TCP/IP connection by any AIS client using the AMP protocol.
   ;; Note2: The XML message is transmitted in the proprietary Agent
   ;;        Information Server Agent Message Protocol (AMP) format.
   (defun xmlSocketEvals(msg)
      faces:(;; Child agent Interface specifications.
             (public Amp:)  ;; Can be executed by AIS AMP
             (security 0)   ;; No or lowest level security required
             ) ;; end Interface specifications
      vars:(result)	
      (setq result (new Structure: result:((compile (morph (lisp msg.cmd))))))
      result) ;; end xmlSocketEvals
   ;; ****************************************************************
   ;; Define AIML lisp tag child agents
   ;; Note: These child agents are available for reference from AIML
   ;;       Lisp scripts, written inside the lisp tag, and executed
   ;;       during the action phase of an AIML rule template.
   ;; ****************************************************************
   ;; Act on a match structure and return an answer String.
   ;; Note: Can be used by a Lisp script within an AIML rule.
   (defun actOnMatch(matchStructure)
      vars:(answer oldInput oldStar oldArg oldDepth)
      ;; Initialize alice for acting (if necessary).
      (if (<> myResetSW true) (reset))
      (if (not (isStructure myMemory)) (reset))
      ;; Save the old input phrase etc. and reset all for this recursion.
      (setq oldInput (copy in))
      (setq oldArg (copy arg))
      (setq oldStar (copy star))
      (setq oldDepth mySearchDepth)
      (if (> (++ mySearchDepth) myMaxSearchDepth) (error "alice.actOnMatch: exceeded maximum search depth"))
      (setq arg matchStructure.arg)
      (setq star matchStructure.star)
      (setq myHtmlResponseHeading "")
      ;; Act out the AIML template in the match structure (may also cause Alice to perform some task as a side effect).
      (setq answer (actionAsString matchStructure.template))
      (if (not (isString answer)) (setq answer ""))
      (setq answer (trim (clean answer)))
      (if (= verbose true) (writeln "alice.actOnMatch: says=[" answer "]"))
      ;; Restore the old input phrase etc. and return from this recursion.
      (setq in oldInput)
      (setq arg oldArg)
      (setq star oldStar)
      (setq mySearchDepth oldDepth)
      answer) ; end actOnMatch
   ;; Appends iterative results into a single structure 
   ;; Note: Can be used by a Lisp script within an AIML rule.
   (defun appendFeatures(tokenVector featureName)
	  vars:(m M n N value resultString)
	  (setq resultString "")
	  (if (not (isVector tokenVector)) (setq tokenVector (new Vector: 1 tokenVector)))
	  (setq featureName (symbol featureName))
	  (setq N (length tokenVector))
	  (loop for n from 0 until N do
	     (setq value tokenVector[n][featureName])
	     (if (<> value #void) (setq resultString (append resultString " " value)))
	     ) ; end loop    
	  resultString) ; end appendFeatures
   ;; Return a vector of rule templates for all possible pattern matches for the specified phrase.
   ;; Note: Can be used by a Lisp script within an AIML rule.
   (defun findAllMatches(phrase topic)
      vars:(template matchList
            oldInput oldStar oldArg oldDepth
            ) ; end temporary variables
      ;; Initialize alice for chatting.
      (if (<> myResetSW true) (reset))
      (if (not (isStructure myMemory)) (reset))
      (if (= phrase #void) (setq phrase (new Vector:)))
      (if (isString phrase) (setq phrase (parsePhrase phrase)))
      (setq matchList (new Vector:))
      ;; Save the old input phrase etc. and reset all for this recursion.
      (setq oldInput (copy in))
      (setq oldArg (copy arg))
      (setq oldStar (copy star))
      (setq oldDepth mySearchDepth)
      (setq in phrase)
      (setq arg (new Vector:))
      (setq star (new Vector:))
      ;; Search AliceRules for all AIML patterns matching the specified phrase.
      (setq template (aimlMatch phrase topic matchList))
      ;; Restore the old input phrase etc. and return from this recursion.
      (setq in oldInput)
      (setq arg oldArg)
      (setq star oldStar)
      (setq mySearchDepth oldDepth)
      matchList) ; end findAllMatches        
   ;; Create a new feature-based element structure.        
   ;; Note: Can be used by a Lisp script within an AIML rule.
   (defun newToken(...)
      vars:(n N m M token featureName featureValue)
      (setq token (new Structure:))
      (setq N (argCount))
      (loop for n from 0 until N do
         (setq featureName (if (< n N) (symbol (argFetch n)) #void))
         (setq featureValue (if (< (setq n (addi n 1)) N) (argFetch n) #void))
         (if (and (<> featureName #void) (<> featureValue #void)) (setq token[featureName] featureValue))
         ) ; end feature loop
      (if (= token.Value #void) (setq token.Value "")) 
      (if (= token.Original #void) (setq token.Original token.Value))
      (if (isNumber token.Value) (begin (setq token.Number true) (setq token.NumValue token.Value)))
      token) ; end newToken
   ;; Parse the text phrase to be matched with the AIML response patterns stored in the Alice file cabinet.
   ;; Note: Can be used by a Lisp script within an AIML rule.
   (defun parsePhrase(phrase)
      vars:(wordVector)
      ;; Convert the input phrase into a vector of upper case word (no blanks or special characters allowed).
      (if (= phrase #void) (return (new Vector:)))
      (if (not (isString phrase)) (setq phrase (string phrase true)))
      (setq wordVector (queryScript phrase))
      (if (not (isVector wordVector)) (return (new Vector:)))
      wordVector) ; end parsePhrase 
   ;; ****************************************************************
   ;; Define private child agents
   ;; ****************************************************************
   ;; Return the requested action on the AIML response template as a string.
   (defun actionAsString(template)
      vars:(m M n N tag record answer name item temp theHour theMinutes begIndex endIndex featureSW)
      ;; Act out the AIML response template from aliceBrain (may also cause Alice to perform some task as a side effect).
      (if (= template #void) (setq template ""))
      (setq answer "")
      (if (= verbose true) (writeln "Acting on template: " (string template true)))
      (cond
         ;; Case: Complex action template.
         ((isStructure template)
          (begin
             (setq N (length template))
             (loop for n from 0 until N do
                (setq tag (downcase (string template[n 0])))
                (setq record template[n 1])
                ;; Perform various response template actions based upon each template attribute.
                (cond
                   ;; Case: apply
                   ((= tag apply:)
                    (setq answer (append answer " " (appendFeatures (applyRules (actionAsTokens record) record.__attlist.topic) Original:)))
                    ) ; end apply case
                   ;; Case: arg
                   ((= tag arg:)
                    (begin 
                       (if (isStructure record) (setq m (sub1 (number record.__attlist.index))) (setq m 0))
                       (if (<> record.__attlist.feature #void)
                           (setq featureSW (symbol record.__attlist.feature)) 
                           (setq featureSW Original:) 
                           ) ; end feature attribute if 
                       (setq answer (append answer " " (trim (if (<> arg[m] #void) (appendFeatures arg[m] featureSW) "delphi"))))
                    )) ; end arg case
                   ;; Case: __attlist
                   ((= tag __attlist:)
                    (setq answer answer)
                    ) ; end attlist case
                   ;; Case: __content
                   ((= tag __content:)
                    (setq answer (append answer " " (trim record)))
                    ) ; end content case
                   ;; Case: bot
                   ((= tag bot:)
                    (begin
                       (if (isStructure record) 
                           (setq item myMemory.chatbot[(symbol record.__attlist.name)]) 
                           (setq item myMemory.chatbot.name)
                           ) ; end if
                       (setq answer (append answer " " (trim (if (<> item #void) item "delphi"))))
                    )) ; end bot case
                   ;; Case: br
                   ((= tag br:)
                    (setq answer (append answer " "))
                    ) ; end br case
                   ;; Case: date
                   ((= tag date:)
                    (setq answer (append answer " " (substitute (substitute (string (date (integer (now)))) "#" "") "," " ")))
                    ) ; end date case
                   ;; Case: em
                   ((= tag em:)
                    (setq answer (append {"} (actionAsString record) {"}))
                    ) ; end em case
                   ;; Case: get
                   ((= tag get:)
                    (begin
                       (if (isStructure record)
                           (begin 
                              (setq item (symbol record.__attlist.name)) 
                              (if (= record.__attlist.switch "yes")
                                  (setq temp myMemory.patternSwitches[item])
                                  (setq temp myMemory[item])
                                  ) ; end if
                              (setq answer (append answer " " (trim (if (<> temp #void) temp "delphi"))))
                           )) ; end if
                    )) ; end get case
                   ;; Case: input
                   ((= tag input:)
                    (begin
                       (setq featureSW |Original|:)
                       (setq begIndex 0)
                       (setq endIndex (sub1 (length in)))
                       (if (<> record.__attlist.index #void)
                           (begin 
                              (setq item (string record.__attlist.index)) 
                              (setq item (stringToVector item "-"))
                              (setq begIndex (subi (number item[0]) 1))
                              (setq endIndex (subi (number item[(sub1 (length item))]) 1))
                           )) ; end index attribute if 
                       (if (<> record.__attlist.feature #void)
                           (setq featureSW (symbol record.__attlist.feature)) 
                           ) ; end feature attribute if 
                       (loop for m from begIndex to endIndex do
                         (setq answer (append answer " " (string in[m][featureSW])))
                         ) ; end loop
                    )) ; end input case
                   ;; Case: lisp
                   ((= tag lisp:)
                    (begin
                       (setq temp (actionAsString record))
                       (if (= verbose true) (writeln "Evaluating lisp expression: " temp))
                       (setq answer (append answer " " (eval (compile (morph (lisp temp)) scriptAgent true))))
                    )) ; end lisp case
                   ;; Case: person
                   ((= tag person:)
                    (begin
                       (if (isStructure record) (setq m (sub1 (number record.__attlist.index))) (setq m 0))
                       (setq answer (append answer " " (trim (if (<> star[m] #void) (alice.changePerspective (appendFeatures star[m] Original:)) "delphi"))))
                    )) ; end person case
                   ;; Case: random
                   ((= tag random:)
                    (begin
                       (setq M (length record))
                       (setq m (integer (random M))) 
                       (setq answer (append answer " " (trim (actionAsString record[m]))))
                    )) ; end random case
                   ;; Case: set
                   ((= tag set:)
                    (begin
                       (if (and (isStructure record) (>= (length record) 2))
                           (begin
                              (setq item (symbol record.__attlist.name))
                              (setq temp (delete (copy record) 0))
                              (if (= record.__attlist.switch "yes")
                                  (setq myMemory.patternSwitches[item] (setq temp (actionAsString temp)))
                                  (setq myMemory[item] (setq temp (actionAsString temp)))
                                  ) ; end if
                              (setq answer (append answer " " (trim temp)))
                           )) ; end if
                       (if (and (isStructure record) (= (length record) 1))
                           (begin
                              (setq item (symbol record.__attlist.name))
                              (setq temp (trim (if (<> star[0] #void) (appendFeatures star[0] Original:) " ")))
                              (if (= record.__attlist.switch "yes")
                                  (setq myMemory.patternSwitches[item] temp)
                                  (setq myMemory[item] temp)
                                  ) ; end if
                              (setq answer (append answer " " (trim temp)))
                           )) ; end if
                    )) ; end set case
                   ;; Case: sr
                   ((= tag sr:)
                    (begin
                       (if (isStructure record) (setq m (sub1 (number record.__attlist.index))) (setq m 0))
                       (setq answer (append answer " " (chat (appendFeatures star[m] Original:))))
                    )) ; end sr case
                   ;; Case: srai
                   ((= tag srai:)
                    (setq answer (append answer " " (chat (actionAsString record) record.__attlist.topic)))
                    ) ; end srai case
                   ;; Case: star
                   ((= tag star:)
                    (begin
                       (if (isStructure record) (setq m (sub1 (number record.__attlist.index))) (setq m 0))
                       (setq answer (append answer " " (trim (if (<> star[m] #void) (appendFeatures star[m] Original:) " "))))
                    )) ; end star case
                   ;; Case: starno
                   ((= tag starno:)
                    (begin
                       (if (isStructure record) (setq m (sub1 (number record.__attlist.index))) (setq m 0))
                       (setq answer (append answer " " (trim (if (<> star[m] #void) (appendFeatures star[m] Original:) " "))))
                    )) ; end starno case
                   ;; Case: string
                   ((= tag string:)
                    (begin
                       (setq temp (actionAsString record))
                       (if (and (isStructure record) (= record.__attlist.enclose quote:)) (setq temp (append {'} temp {'})))
                       (if (and (isStructure record) (= record.__attlist.enclose dquote:)) (setq temp (append {"} temp {"})))
                       (if (and (isStructure record) (= record.__attlist.enclose cdata:)) (setq temp (append "<!CDATA[" temp "]]>")))
                       (setq answer (append answer " " temp))
                    )) ; end star case
                   ;; Case: thatstar
                   ((= tag thatstar:)
                    (begin
                       (setq answer (append answer " " (trim (if (<> that #void) (appendFeatures that Original:) " "))))
                    )) ; end star case
                   ;; Case: think
                   ((= tag think:)
                    (begin
                       (actionAsString record)
                    )) ; end think case
                   ;; Case: time
                   ((= tag time:)
                    (begin
                       (setq theHour (hour (now)))
                       (setq theMinutes (minute (now)))
                       (if (> theHour 12)
                           (setq temp (append "" (- theHour 12) "PM"))
                           (setq temp (append "" theHour "AM"))
                           ) ; end if
                       (if (<> theMinutes 0)
                       	   (setq answer (append answer " " theMinutes " minutes past " temp))
                       	   (setq answer (append answer " " temp))
                           ) ; end if
                    )) ; end time case
                   ;; Case unknown Tag with Structure value
                   ((isStructure record)
                    (begin
                       (if (= verbose true) (writeln " Unknown template tag: " (string tag true)))
                       (setq answer (append answer " " (trim (actionAsString record))))
                    )) ; end unknown case
                   (else
                    (begin
                       (if (= verbose true) (writeln " Unknown template: " (string record true)))
	                   (setq answer (trim answer))
                    )) ; end else case                   	
                   ) ; end cond
                ) ; end loop
          )) ; end complex action template case
         ;; Case: action template is a String
         ((isString template)
          (setq answer template)
          ) ; end String case
         ;; Case: action template is a Symbol
         ((isSymbol template)
          (setq answer template)
          ) ; end Symbol case
         ;; Everything else is an error
         (else
          (error (append "alice.actionAsString: invalid action template [" (string template true) "]"))
          ) ; end Symbol case
       ) ; end cond 
      answer) ; end actionAsString 
   ;; Return the requested action on the AIML response template as a token vector.
   (defun actionAsTokens(rule)
      vars:(m M n N tag record answer item name temp theHour theMinutes begIndex endIndex featureSW)
      ;; Act out the AIML response rule from aliceBrain (may also cause Alice to perform some task as a side effect).
      (if (= verbose true) (writeln "Acting on rule: " (string rule true)))
      (cond
       ;; Case: complex action rule.
       ((isStructure rule) 
          (begin
             (setq answer (new Vector:))
             (setq N (length rule))
             (loop for n from 0 until N do
                (setq tag (downcase (string rule[n 0])))
                (setq record rule[n 1])
                ;; Perform various rule actions based upon the attribute of the entry
                (cond
                   ;; Case: apply
                   ((= tag apply:)
                    (setq answer (append answer (applyRules (actionAsTokens record) record.__attlist.topic)))
                    ) ; end apply case
                   ;; Case: arg
                   ((= tag arg:)
                    (begin
                       (if (isStructure record) (setq m (sub1 (number record.__attlist.index))) (setq m 0))
                       (if (<> record.__attlist.feature #void)
                           (setq featureSW (symbol record.__attlist.feature)) 
                           ) ; end feature attribute if 
                       (if (or (= featureSW #void) (= arg[m][featureSW] arg[m].Value) (= arg[m][featureSW] arg[m].Original))
                           (setq temp arg[m])
                           (setq temp arg[m][featureSW])
                           ) ; end if
                       (cond
                        ((isVector temp) (setq answer (append answer temp)))
                        ((isStructure temp) (setq answer[(length answer)] temp))
                        (else (setq answer (append answer (phraseToTokens temp))))
                        ) ; end cond
                    )) ; end arg case
                   ;; Case: __attlist
                   ((= tag __attlist:)
                    (setq answer answer)
                    ) ; end attlist case
                   ;; Case: __content
                   ((= tag __content:)
                    (setq answer (append answer (phraseToTokens record)))
                    ) ; end content case
                   ;; Case: bot
                   ((= tag bot:)
                    (begin
                       (if (isStructure record) 
                           (setq item myMemory.chatbot[(symbol record.__attlist.name)]) 
                           (setq item myMemory.chatbot.name)
                           ) ; end if
                       (setq item (trim (if (<> item #void) item "Delphi")))
                       (setq answer (append answer (phraseToTokens item)))
                    )) ; end bot case
                   ;; Case: br
                   ((= tag br:)
                    (setq answer answer)
                    ) ; end br case
                   ;; Case: date
                   ((= tag date:)
                    (setq temp (substitute (substitute (string (date (integer (now)))) "#" "") "," " "))
                    (setq answer[(length answer)] (newToken Value: temp Original: temp Date: true))
                    ) ; end date case
                   ;; Case: em
                   ((= tag em:)
                    (begin
                       (setq temp (append {"} (actionAsString record) {"}))  
                       (setq answer[(length answer)] (newToken Value: temp Original: temp String: true))
                    )) ; end em case
                   ;; Case: get
                   ((= tag get:)
                    (begin
                       (if (isStructure record)
                           (begin 
                              (setq item (symbol record.__attlist.name)) 
                              (if (= record.__attlist.switch "yes")
                                  (setq temp myMemory.patternSwitches[item])
                                  (setq temp myMemory[item])
                                  ) ; end if
                              (setq temp (trim (if (<> temp #void) temp "delphi")))
                              (setq answer (append answer (phraseToTokens temp)))
                           )) ; end if
                    )) ; end get case
                   ;; Case: input
                   ((= tag input:)
                    (begin
                       (setq featureSW #void)
                       (setq begIndex 0)
                       (setq endIndex (sub1 (length in)))
                       (if (<> record.__attlist.index #void)
                           (begin 
                              (setq item (string record.__attlist.index)) 
                              (setq item (stringToVector item "-"))
                              (setq begIndex (subi (number item[0]) 1))
                              (setq endIndex (subi (number item[(sub1 (length item))]) 1))
                           )) ; end index attribute if 
                       (if (<> record.__attlist.feature #void)
                           (setq featureSW (symbol record.__attlist.feature)) 
                           ) ; end feature attribute if 
                       (loop for m from begIndex to endIndex do
                         (if (or (= featureSW #void) (= in[m][featureSW] in[m].Value) (= in[m][featureSW] in[m].Original))
                             (setq temp in[m])
                             (setq temp in[m][featureSW])
                             ) ; end if
                         (cond
                          ((isVector temp) (setq answer (append answer temp)))
                          ((isStructure temp) (setq answer[(length answer)] temp))
                          (else (setq answer (append answer (phraseToTokens temp))))
                          ) ; end cond
                         ) ; end loop
                    )) ; end input case
                   ;; Case: lisp
                   ((= tag lisp:)
                    (begin
                       (setq temp (actionAsString record))
                       (if (= verbose true) (writeln "Evaluating lisp expression: " temp))
                       (setq temp (eval (compile (morph (lisp temp)) scriptAgent true)))
                       (cond
                         ((isVector temp) (setq answer (append answer temp)))
                         ((isStructure temp) (setq answer[(length answer)] temp))
                         (else (setq answer (append answer (phraseToTokens temp))))
                         ) ; end cond
                    )) ; end lisp case
                   ;; Case: person
                   ((= tag person:)
                    (begin
                       (if (isStructure record) (setq m (sub1 (number record.__attlist.index))) (setq m 0))
                       (setq temp (trim (if (<> star[m] #void) (alice.changePerspective (appendFeatures star[m] Original:)) "delphi")))
                       (setq answer (append answer (phraseToTokens temp)))
                    )) ; end person case
                   ;; Case: random
                   ((= tag random:)
                    (begin
                       (setq M (length record))
                       (setq m (integer (random M))) 
                       (setq answer (append answer (actionAsTokens record[m])))
                       (setq answer (setq answer[(length answer)] (newToken Value: temp Original: temp String: true)))
                    )) ; end random case
                   ;; Case: set
                   ((= tag set:)
                    (begin
                       (if (and (isStructure record) (>= (length record) 2))
                           (begin
                              (setq item (symbol record.__attlist.name))
                              (setq temp (delete (copy record) 0)) 
                              (if (= record.__attlist.switch "yes")
                                  (setq myMemory.patternSwitches[item] (setq temp (actionAsString temp)))
                                  (setq myMemory[item] (setq temp (actionAsString temp)))
                                  ) ; end if
                              (setq answer (append answer (phraseToTokens temp)))
                           )) ; end if
                       (if (and (isStructure record) (= (length record) 1))
                           (begin
                              (setq item (symbol record.__attlist.name))
                              (setq temp (if (<> star[m] #void) star[m] (new Vector:)))
                              (if (= record.__attlist.switch "yes")
                                  (setq myMemory.patternSwitches[item] temp)
                                  (setq myMemory[item] temp)
                                  ) ; end if
                              (setq answer (append answer (phraseToTokens temp)))
                           )) ; end if
                    )) ; end set case
                   ;; Case: sr
                   ((= tag sr:)
                    (begin
                       (if (isStructure record) (setq m (sub1 (number record.__attlist.index))) (setq m 0))
                       (setq answer (append answer (applyRules star[m])))
                    )) ; end sr case
                   ;; Case: srai
                   ((= tag srai:)
                    (setq answer (append answer (applyRules (actionAsTokens record) record.__attlist.topic)))
                    ) ; end srai case
                   ;; Case: star
                   ((= tag star:)
                    (begin
                       (if (isStructure record) (setq m (sub1 (number record.__attlist.index))) (setq m 0))
                       (if (<> star[m] #void) (setq answer (append answer star[m])))
                    )) ; end star case
                   ;; Case: starno
                   ((= tag starno:)
                    (begin
                       (if (isStructure record) (setq m (sub1 (number record.__attlist.index))) (setq m 0))
                       (if (<> star[m] #void) (setq answer (append answer star[m])))
                    )) ; end starno case
                   ;; Case: string
                   ((= tag string:)
                    (begin
                       (setq temp (actionAsString record))
                       (if (and (isStructure record) (= record.__attlist.enclose quote:)) (setq temp (append {'} temp {'})))
                       (if (and (isStructure record) (= record.__attlist.enclose dquote:)) (setq temp (append {"} temp {"})))
                       (if (and (isStructure record) (= record.__attlist.enclose cdata:)) (setq temp (append "<!CDATA[" temp "]]>")))
                       (setq answer (setq answer[(length answer)] (newToken Value: temp Original: temp String: true)))
                    )) ; end star case
                   ;; Case: thatstar
                   ((= tag thatstar:)
                    (begin
                       (if (<> that #void) (setq answer (append answer that)))
                    )) ; end star case
                   ;; Case: think
                   ((= tag think:)
                    (begin
                       (actionAsString record)
                    )) ; end think case
                   ;; Case: time
                   ((= tag time:)
                    (begin
                       (setq theHour (hour (now)))
                       (setq theMinutes (minute (now)))
                       (if (> theHour 12)
                           (setq temp (append "" (- theHour 12) "PM"))
                           (setq temp (append "" theHour "AM"))
                           ) ; end if
                       (if (<> theMinutes 0)
                       	   (setq temp (append theMinutes " minutes past " temp))
                           ) ; end if
                       (setq answer[(length answer)] (newToken Value: temp Original: temp Time: true))
                    )) ; end time case
                   ;; Case unknown Tag with Structure value
                   ((isStructure record)
                    (begin
                       (if (= verbose true) (writeln " Unknown rule tag: " (string tag true)))
                       (setq answer (append answer (actionAsTokens record)))
                    )) ; end unknown case
                   ;; Case unknown Tag with String value
                   ((isString record)
                    (begin
                       (if (= verbose true) (writeln " Unknown rule: " record))
                       (setq answer (append answer (phraseToTokens record)))
                    )) ; end unknown case
                   (else
                    (begin
                       (if (= verbose true) (writeln " Unknown rule: " (string record true)))
	                   (setq answer answer)
                    )) ; end else case
                   ) ; end cond
                ) ; end loop
          )) ; end complex rule case
         ;; Case: action rule is a String
         ((isString rule)
          (setq answer (phraseToTokens rule))
          ) ; end String case
         ;; Case: action rule is a Symbol
         ((isSymbol rule)
          (setq answer (new Vector: 1 (newToken Value: rule Original: rule Symbol: true)))
          ) ; end Symbol case
         ;; Everything else is an error
         (else
          (error (append "alice.actionAsTokens: invalid action rule [" (string rule true) "]"))
          ) ; end Symbol case
       ) ; end cond 
      answer) ; end actionAsTokens 
   ;; Return the proper insertion index to add only unique matches to a match list vector.
   (defun addUnique(matchList newMatch)
      vars:(n N)
      (if (= newMatch.template #void) (goto Last:))
      (setq N (length matchList))
      (loop for n from 0 until N do
         (if (= matchList[n].template newMatch.template) (goto Last:))
         ) ; end loop
      (setq matchList[n] newMatch)
      Last::
      matchList) ; end addUnique
   ;; Match the upper case phrase with the AIML response patterns stored in the Alice file cabinet.        
   (defun aimlMatch(phrase ...)
      vars:(n N
            template matchList 
            topic topicKey topicPatterns
            firstWord wordBreaks switchTest
            startTime endTime 
            startMemory endMemory feature
            ) ; end temporary variables
      ;; The input phrase must be a vector of lexical element structures.
      (if (<= (length phrase) 0) (goto Last:))
      ;; Retrieve the topic (if present).
      ;; Note: If a topic is present, we match only against the local topic rules.
      (if (>= (argCount) 2) (setq topic (argFetch 1)))
      (if (>= (argCount) 3) (setq matchList (argFetch 2)))
      (if (<> topic #void)
          (begin
             (setq topicKey (append "&" topic))
             (setq topicPatterns myAimlPatterns[topicKey])
             (if (not (isDirectory topicPatterns)) (setq topicPatterns (checkout "AliceRules" topicKey)))
             (if (isDirectory topicPatterns) (setq myAimlPatterns[topicKey] topicPatterns)) 
             (if (not (isDirectory topicPatterns)) (setq topicPatterns (new Directory:)))
             (setq template (aimlMatchWord phrase 0 topicPatterns matchList))
             (goto Last:)
          )) ; end if 
      ;; Load all necessary AIML response patterns.
      ;; Note: Only the absolutely necessary reponse patterns are loaded from Alice Brain.
      (if (= verbose true) 
          (begin
             (writeln "Starting checkout of vocabulary.")
             (gc)(setq startMemory (inspect))
             (setq startTime (getTickCount 0))                   
          )) ; end if
      (_browseAgentExtents.Extents.AliceRules.beginTransaction)
      (if (not (isDirectory myAimlPatterns)) (setq myAimlPatterns (new Directory:)))
      ;; ...Load the AIML response patterns for all switch tests.
      (setq N (length _browseAgentExtents.Extents.AliceRules.myRepository))
      (setq switchTest (string _browseAgentExtents.Extents.AliceRules.myRepository[(setq n 0) 0]))
      (while (or (= switchTest[0] #\!) (= switchTest[0] #\#)) do 
         (setq myAimlPatterns[switchWord] (checkout "AliceRules" switchWord))
         (setq switchTest (string _browseAgentExtents.Extents.AliceRules.myRepository[(++ n) 0]))
         ) ; end while
      ;; ...Load the AIML response patterns for the first word in the input string.
      (setq firstWord phrase[0].Value)
      (setq myAimlPatterns[firstWord] (checkout "AliceRules" firstWord))
      ;; ...Load the AIML response patterns for the standard wild cards.
      (setq myAimlPatterns["?"] (checkout "AliceRules" "?"))
      (setq myAimlPatterns["@"] (checkout "AliceRules" "@"))
      (setq myAimlPatterns["_"] (checkout "AliceRules" "_"))
      (setq myAimlPatterns["*"] (checkout "AliceRules" "*"))
      ;; ...Load the AIML response patterns for all features of the first word in the input string.
      (setq N (length phrase[0]))
      (loop for n from 0 until N do
         (if (or (= phrase[0][n 0] Value:) (= phrase[0][n 0] Original:) (= phrase[0][n 0] Charpos:) (<> phrase[0][n 1] true)) (goto NextFeature:))
         (setq feature (append "$" phrase[0][n 0]))
         (setq myAimlPatterns[feature] (checkout "AliceRules" feature))
         (setq feature (append "%" phrase[0][n 0]))
         (setq myAimlPatterns[feature] (checkout "AliceRules" feature))
         NextFeature::
         ) ; end feature loop
      (_browseAgentExtents.Extents.AliceRules.commitTransaction)
      (if (= verbose true) 
          (begin
		     (setq endTime (getTickCount startTime))
             (gc)(setq endMemory (inspect))
		     (writeln "Completed checkout of vocabulary in [" endTime " Seconds] using up [" (text (- startMemory endMemory) "#,###") " bytes]")
		     )) ; end if
      ;; Find the matching reponse template (if any).
      (if (= template #void) (setq template (aimlMatchWord phrase 0 myAimlPatterns matchList)))
      ;; Return the matching template (if any).
      Last::
      template) ; end aimlMatch
   ;; Match the specified word phrase with the AIML response patterns stored in the Alice file cabinet.        
   (defun aimlMatchWord(phrase wordIndex aimlPatterns matchList)
      vars:(k K m M n N s v
            nextPatterns thisToken thisKey thisValue
            switchTest switchKey switchSymbol switchToken switchScript
            starMatch starCpy starLen argLen oldArgLen (matchSW false)
            template tmpTemplate thatCondition thatTemplate
            key dict firstLetter feature rawFeature
            ) ; end temporary variables
      ;; +++++++++++++++++
      ;; Sequence of testing steps at each word:
      ;;    (Step 01) (#.)/(!.)            Try all registered sequential tests (switches, token, and lisp).
      ;;    (Step 02) (?)                  Is there a "?" sub-tree in the current aimlPattern buffer?
      ;;    (Step 03) (_)                  Is there a "_" sub-tree in the current aimlPattern buffer?
      ;;    (Step 04) (%)                  We attempt to find a match for any preemptive features (%) of the current word.
      ;;    (Step 05) (Value)              We attempt to find a match for the current word (Value).
      ;;    (Step 06) ($)                  We attempt to find a match for any features of the current word ($).
      ;;    (Step 07) (@)                  Is there an "@" sub-tree in the current aimlPattern buffer?
      ;;    (Step 08) (*)                  Is there a "*" sub-tree in the current aimlPattern buffer?
      ;;    (Step 09) ..no match..         If we have no match for the current word, we have failed.
      ;; +++++++++++++++++
      ;; Begin the logic for the aimlMatchWord child agent.
      (if (isVector matchList) (setq matchSW true))
      ;; (Step 01) Try all registered sequential tests (switches, token, and lisp).
      (if (= (setq N (length aimlPatterns)) 0) (return template)) 
      (if (isBoolean aimlPatterns[0 0])
          (if (< 1 N) (setq switchTest (string aimlPatterns[(setq n 1) 0])) (setq switchTest "."))
          (setq switchTest (string aimlPatterns[(setq n 0) 0]))
          ) ; end if
      (setq switchKey switchTest[0])      
      (setq switchSymbol (symbol (mid switchTest 1 1000000)))      
      (while (or (= switchKey (char 33)) (= switchKey (char 35))) do
         ;; Manage different types of switch test, and Lisp tests.
         (cond
          ;; Case: token test
          ((= (left switchTest 2) "#{")
             (begin
               (setq K (length phrase))
               (setq k wordIndex)
               (setq thisToken phrase[k])
               (setq switchToken (parse switchTest))
               (setq M (length switchToken))
               (loop for m from 0 until M do
                  (if (and (= switchToken[m 1] |?|:) (not (isMember switchToken[m 0] thisToken))) (goto TokenNotMatched:))
                  (if (<> thisToken[switchToken[m 0]] switchToken[m 1]) (goto TokenNotMatched:))
                  ) ; end token loop
               ;; Match scripts (if necessary)
               (setq switchScript (cdr switchToken))
               (if (and (isString switchScript) (= (left switchScript 6) "#lisp#") (= (eval (compile (morph (lisp (mid switchScript 6 1000000))) scriptAgent true)) true))
                   (goto TokenMatch:)) 
               (if (isString switchScript) (goto TokenNotMatched:))
               TokenMatch::
               ;; We get here if we've found an exact token match for the kth word in the current phrase.
               (if (= verbose true) (writeln (left myBlanks (* k 2)) "Matching token for [" switchTest "] at [" k "] in [" phrase "]"))
               (setq argLen (length arg))
               (setq starLen (length star))
               (setq arg[argLen] phrase[k])
               (setq nextPatterns aimlPatterns[n 1])
               (setq template (aimlMatchWord phrase (addi k 1) nextPatterns matchVector))
               (if (<> template #void) 
                   (if (<> matchSW true)
                       (return template)
                       (begin
                          ;(addUnique matchList (new Structure: arg: (copy arg) star: (copy star) template: template)) 
	                     (setq arg (resize arg (+ argLen 1)))
	                     (setq star (resize star starLen))
                       ))) ; end if
               ;; We get here if we did NOT find a match for pattern following the kth word in the current phrase.
               ;; Is this the last word in the phrase to be recognized, and is there a "*" sub-tree in 
               ;; the next pattern buffer, and does this "*" sub-tree have a true entry?
               (if (and (= (+ k 1) K) (<> (setq tmpTemplate nextPatterns["*"][true]) #void)) 
                   (begin
                      ;; Check <that> condition here.
                      (if (and (<> (setq thatCondition nextPatterns["*"]["<that>"]) #void) 
                               (<> (setq thatTemplate (aimlMatchWord that 0 thatCondition matchList)) #void))
	                      (setq template thatTemplate)
	                      (setq template tmpTemplate)
                          ) ; end if
                      (if (<> template #void) 
                          (if (<> matchSW true)
                              (return template)
                              (begin
                                 (addUnique matchList (new Structure: arg: (copy arg) star: (copy star) template: template)) 
                              ))) ; end if
                   )) ; end
               (setq arg (resize arg argLen))
               (setq star (resize star starLen))
               TokenNotMatched::
             )) ; end token test
          ;; Case: lisp test
          ((= (left switchTest 6) "#lisp#")
             (begin
               (if (= (eval (compile (morph (lisp (mid switchTest 6 1000000))) scriptAgent true)) true)
                   (begin 
                     (if (= verbose true) (writeln (left myBlanks (* k 2)) "Matching condition for [" switchTest "] at [" wordIndex "] in [" phrase "]"))
                     (setq argLen (length arg))
                     (setq starLen (length star))
                     (setq template (aimlMatchWord phrase wordIndex aimlPatterns[n 1] matchList))
                     (if (<> template #void) 
                         (if (<> matchSW true)
                             (return template)
                             (begin
                                ;(addUnique matchList (new Structure:  arg: (copy arg) star: (copy star) template: template)) 
	                           (setq arg (resize arg argLen))
	                           (setq star (resize star starLen))
                             ))) ; end if
                   )) ; end if
             )) ; end lisp test
          ;; Case: #Switch test (void)
          ((and (= switchKey (char 35)) (= myMemory.patternSwitches[switchSymbol] #void))
             (begin 
               (if (= verbose true) (writeln (left myBlanks (* k 2)) "Matching switch for [" switchTest "] at [" wordIndex "] in [" phrase "]"))
               (setq argLen (length arg))
               (setq starLen (length star))
               (setq template (aimlMatchWord phrase wordIndex aimlPatterns[n 1] matchList))
               (if (<> template #void) 
                   (if (<> matchSW true)
                       (return template)
                       (begin
                          ;(addUnique matchList (new Structure: arg: (copy arg) star: (copy star) template: template))
                          (setq arg (resize arg argLen))
                          (setq star (resize star starLen))
                       ))) ; end if
             )) ; end positive switch test
          ;; Case: !Switch test (non-void)
          ((and (= switchKey (char 33)) (<> myMemory.patternSwitches[switchSymbol] #void))
             (begin 
               (if (= verbose true) (writeln (left myBlanks (* k 2)) "Matching switch for [" switchTest "] at [" wordIndex "] in [" phrase "]"))
               (setq argLen (length arg))
               (setq starLen (length star))
               (setq template (aimlMatchWord phrase wordIndex aimlPatterns[n 1] matchList))
               (if (<> template #void) 
                   (if (<> matchSW true)
                       (return template)
                       (begin
                          ;(addUnique matchList (new Structure: arg: (copy arg) star: (copy star) template: template)) 
                          (setq arg (resize arg argLen))
                          (setq star (resize star starLen))
                       ))) ; end if
             )) ; end negative switch test
          ) ; end cond
         (++ n)
         (if (< n N) (setq switchTest (string aimlPatterns[n 0])) (setq switchTest "."))
         (setq switchKey switchTest[0])      
         (setq switchSymbol (symbol (mid switchTest 1 1000000)))      
         ) ; end while
      ;; Find the matching reponse template (if any).
      (setq K (length phrase))
      (loop for k from wordIndex until K do
         ;; (Step 02) Is there an "?" sub-tree in the current aimlPattern buffer?
         ;;           Note: If so, we attempt to find a match for all possible remaining suffixes.
         (if (<> (setq nextPatterns aimlPatterns["?"]) #void)
             (begin
                (setq argLen (length arg))
                (setq starLen (length star))
                (setq star[(length star)] (setq starMatch (new Vector:)))
                (if (= verbose true) (writeln (left myBlanks (* k 2)) "Assigning [?] with [" (appendFeatures starMatch Value:) "] from [" k "] in [" phrase "]"))
                (loop for n from k until K do
                   (setq template (aimlMatchWord phrase n nextPatterns matchList))
                   (if (<> template #void) 
                       (begin
                          ;; Save the current star match pattern and return the template.
                          (if (<> matchSW true)
                              (return template)
                              (begin
                                 ;(addUnique matchList (new Structure: arg: (copy arg) star: (copy star) template: template))
                                 (setq arg (resize arg argLen))
                                 (setq star (resize star starLen))
                                 (goto UnderScoreMatch:)
                              )) ; end if
                       )) ; end if
                   (setq starMatch[(length starMatch)] phrase[n]) 
                   ) ; end loop
                ;; Finally we check for a terminal leaf at this [*] node.
		       (if (<> (setq template nextPatterns[true]) #void)
		           (begin
		              ;; We get here if we've found an exact match for the kth word in the current phrase.
                       (if (= verbose true) (writeln  (left myBlanks (* k 2)) "Matching [?] with [" (appendFeatures starMatch Value:) "] from tail of [" phrase "]"))
                       ;; Check <that> condition here.
                       (if (and (<> (setq thatCondition nextPatterns["<that>"]) #void) 
                                (<> (setq thatTemplate (aimlMatchWord that 0 thatCondition matchList)) #void))
                           (setq template thatTemplate)
                           (setq template template)
                           ) ; end if
                       (if (<> matchSW true)
                           (return template)
                           (begin
                              (addUnique matchList (new Structure: arg: (copy arg) star: (copy star) template: template)) 
                              ) ; end else
                           ) ; end if
                      )) ; end if
             (setq arg (resize arg argLen))
             (setq star (resize star starLen))
             )) ; end if
         ;; (Step 03) Is there an "_" sub-tree in the current aimlPattern buffer?
         ;;           Note: If so, we attempt to find a match for all possible remaining suffixes.
         UnderScoreMatch::
         (if (<> (setq nextPatterns aimlPatterns["_"]) #void)
             (begin
                (setq argLen (length arg))
                (setq starLen (length star))
                (setq star[(length star)] (setq starMatch (new Vector: 1 phrase[k])))
                (if (= verbose true) (writeln (left myBlanks (* k 2)) "Assigning [_] with [" (appendFeatures starMatch Value:) "] from [" k "] in [" phrase "]"))
                (loop for n from (addi k 1) until K do
                   (setq template (aimlMatchWord phrase n nextPatterns matchList))
                   (if (<> template #void) 
                       (begin
                          ;; Save the current star match pattern and return the template.
                          (if (<> matchSW true)
                              (return template)
                              (begin
                                 ;(addUnique matchList (new Structure: arg: (copy arg) star: (copy star) template: template))
                                 (setq arg (resize arg argLen))
                                 (setq star (resize star starLen))
                                 (goto PreEmptiveFeatureMatch:)
                              )) ; end if
                       )) ; end if
                   (setq starMatch[(length starMatch)] phrase[n]) 
                   ) ; end loop
                ;; Finally we check for a terminal leaf at this [*] node.
		       (if (<> (setq template nextPatterns[true]) #void)
		           (begin
		              ;; We get here if we've found an exact match for the kth word in the current phrase.
                      (if (= verbose true) (writeln  (left myBlanks (* k 2)) "Matching [_] with [" (appendFeatures starMatch Value:) "] from tail of [" phrase "]"))
                      ;; Check <that> condition here.
                      (if (and (<> (setq thatCondition nextPatterns["<that>"]) #void) 
                               (<> (setq thatTemplate (aimlMatchWord that 0 thatCondition matchList)) #void))
                          (setq template thatTemplate)
                          (setq template template)
                          ) ; end if
                      (if (<> matchSW true)
                          (return template)
                          (begin
                             (addUnique matchList (new Structure: arg: (copy arg) star: (copy star) template: template)) 
                          )) ; end if
                      )) ; end if
             (setq arg (resize arg argLen))
             (setq star (resize star starLen))
             )) ; end if
         ;;  (Step 04) We attempt to find a match for any preemptive features (%) of the current word.
         ;;            Note: We ignore the Original and Value features.
         PreEmptiveFeatureMatch::
         (setq N (length phrase[k]))
         (loop for n from 0 until N do
             (if (or (= (setq rawFeature phrase[k][n 0]) Value:) (= rawFeature Original:) (= rawFeature Charpos:) (<> phrase[k][n 1] true)) (goto NextPreEmptiveFeature:))
             (if (<> aimlPatterns[(setq feature (symbol (append "%" phrase[k][n 0])))] #void)
	             (begin
	                ;; We get here if we've found an exact match for the nth feature of the kth word in the current phrase.
	                (if (= verbose true) (writeln (left myBlanks (* k 2)) "Matching exactly [" feature "] from [" k "] in [" (appendFeatures phrase Value:) "]"))
	     ;;  (writeln "PreEmptiveFeatureMatch") ;; MATH
                    (setq argLen (length arg))
	                (setq starLen (length star))
	                (setq arg[argLen] phrase[k])
	                (setq nextPatterns aimlPatterns[feature])
	                (setq template (aimlMatchWord phrase (addi k 1) nextPatterns matchList))
	                (if (<> template #void)
                        (if (<> matchSW true)
                            (return template)
                            (begin
                               ;(addUnique matchList (new Structure:  arg: (copy arg) star: (copy star) template: template)) 
                               (setq arg (resize arg argLen))
                               (setq star (resize star starLen))
                            ))) ; end if
	                ;; We get here if we did NOT find a match for pattern following nth feature of the kth word in the current phrase.
			       ;; Is this the last word in the phrase to be recognized, and is there a "*" sub-tree in 
			       ;; the next pattern buffer, and does this "*" sub-tree have a true entry?
	                (if (and (= (+ k 1) K) (<> (setq tmpTemplate nextPatterns["*"][true]) #void))
	                    (begin
	                       ;; Check <that> condition here.
	                       (if (and (<> (setq thatCondition nextPatterns["*"]["<that>"]) #void) 
	                                (<> (setq thatTemplate (aimlMatchWord that 0 thatCondition matchList)) #void))
	                           (setq template thatTemplate)
	                           (setq template tmpTemplate)
                                ) ; end if
                               (if (<> matchSW true)
                                   (return template)
                                   (begin
                                      (addUnique matchList (new Structure: arg: (copy arg) star: (copy star) template: template)) 
                                      (setq arg (resize arg argLen))
                                      (setq star (resize star starLen))
                                   )) ; end if
	                    )) ; end
                    (setq arg (resize arg argLen))
                    (setq star (resize star starLen))
	                )) ; end if
            NextPreEmptiveFeature::
            ) ; end feature loop
         ;; (Step 05) We attempt to find a match for the current word (Value).
         (if (<> aimlPatterns[(string phrase[k].Value)] #void)
             (begin
                ;; We get here if we've found an exact match for the kth word in the current phrase.
                (if (= verbose true) (writeln (left myBlanks (* k 2)) "Matching exactly [" phrase[k].Value "] from [" k "] in [" (appendFeatures phrase Value:) "]"))
     ;; (writeln "NextPreEmptiveFeature") ;; MATH
          (setq argLen (length arg))
                (setq starLen (length star))
                (setq arg[argLen] phrase[k])

                (setq nextPatterns aimlPatterns[(string phrase[k].Value]))
;; (writeln "phrase value: "(string phrase[k].Value))
;;(writeln "nextPatterns: " nextPatterns)
                (setq template (aimlMatchWord phrase (addi k 1) nextPatterns matchList))
;;(writeln "template: "  template)
                (if (<> template #void)
                    (if (<> matchSW true)
                        (return template)
                        (begin
                           ;(addUnique matchList (new Structure: arg: (copy arg) star: (copy star) template: template)) 
                           (setq arg (resize arg argLen))
                           (setq star (resize star starLen))
                        ))) ; end if
                ;; We get here if we did NOT find a match for pattern following the kth word in the current phrase.
                ;; Is this the last word in the phrase to be recognized, and is there a "*" sub-tree in 
                ;; the next pattern buffer, and does this "*" sub-tree have a true entry?
                (if (and (= (+ k 1) K) (<> (setq tmpTemplate nextPatterns["*"][true]) #void)) 
                    (begin
                       ;; Check <that> condition here.
                       (if (and (<> (setq thatCondition nextPatterns["*"]["<that>"]) #void) 
                                (<> (setq thatTemplate (aimlMatchWord that 0 thatCondition matchList)) #void))
	                       (setq template thatTemplate)
	                       (setq template tmpTemplate)
                           ) ; end if
                       (if (<> matchSW true)
                           (return template)
                           (begin
                              (addUnique matchList (new Structure: arg: (copy arg) star: (copy star) template: template)) 
                              (setq arg (resize arg argLen))
                              (setq star (resize star starLen))
                           )) ; end if
                    )) ; end
                (setq arg (resize arg argLen))
                (setq star (resize star starLen))
                )) ; end if
         ;; (Step 06) We attempt to find a match for any features of the current word ($).
         ;;           Note: We ignore the Original and Value features
         (setq N (length phrase[k]))
         (loop for n from 0 until N do
             (if (or (= (setq rawFeature phrase[k][n 0]) Value:) (= rawFeature Original:) (= rawFeature Charpos:) (<> phrase[k][n 1] true)) (goto NextPostEmptiveFeature:))
             (if (<> aimlPatterns[(setq feature (symbol (append "$" phrase[k][n 0])))] #void)
	             (begin
	                ;; We get here if we've found an exact match for the nth feature of the kth word in the current phrase.
                    (if (= verbose true) (writeln (left myBlanks (* k 2)) "Matching exactly [" feature "] from [" k "] in [" (appendFeatures phrase Value:) "]"))
;;(writeln "here for n MATCH: " )       ;;        
     (setq argLen (length arg))
                    (setq starLen (length star))
                    (setq arg[argLen] phrase[k])
                    (setq nextPatterns aimlPatterns[feature])
                    (setq template (aimlMatchWord phrase (addi k 1) nextPatterns matchList))
                    (if (<> template #void)
                        (if (<> matchSW true)
                            (return template)
                            (begin
                               ;(addUnique matchList (new Structure: arg: (copy arg) star: (copy star) template: template)) 
                               (setq arg (resize arg argLen))
                               (setq star (resize star starLen))
                            ))) ; end if
                     ;; We get here if we did NOT find a match for pattern following nth feature of the kth word in the current phrase.
                     ;; Is this the last word in the phrase to be recognized, and is there a "*" sub-tree in 
                     ;; the next pattern buffer, and does this "*" sub-tree have a true entry?
	                (if (and (= (+ k 1) K) (<> (setq tmpTemplate nextPatterns["*"][true]) #void))
	                    (begin
	                       ;; Check <that> condition here.
	                       (if (and (<> (setq thatCondition nextPatterns["*"]["<that>"]) #void) 
	                                (<> (setq thatTemplate (aimlMatchWord that 0 thatCondition matchList)) #void))
	                           (setq template thatTemplate)
	                           (setq template tmpTemplate)
	                           ) ; end if
                           (if (<> matchSW true)
                               (return template)
                               (begin
                                  (addUnique matchList (new Structure: arg: (copy arg) star: (copy star) template: template)) 
                                  (setq arg (resize arg argLen))
                                  (setq star (resize star starLen))
                               )) ; end if
	                    )) ; end
                  (setq arg (resize arg argLen))
                  (setq star (resize star starLen))
                  )) ; end if
            NextPostEmptiveFeature::
            ) ; end feature loop
         ;;  (Step 07) Is there an "@" sub-tree in the current aimlPattern buffer?
         ;;            Note: If so, we attempt to find a match for all possible remaining suffixes.
         (if (<> (setq nextPatterns aimlPatterns["@"]) #void)
             (begin
                (setq argLen (length arg))
                (setq starLen (length star))
                (setq star[(length star)] (setq starMatch (new Vector:)))
                (if (= verbose true) (writeln (left myBlanks (* k 2)) "Assigning [@] with [" (appendFeatures starMatch Value:) "] from [" k "] in [" phrase "]"))
                (loop for n from k until K do
                   (setq template (aimlMatchWord phrase n nextPatterns matchList))
                   (if (<> template #void) 
                       (begin
                          ;; Save the current star match pattern and return the template.
                          (if (<> matchSW true)
                              (return template)
                              (begin
                                 ;(addUnique matchList (new Structure: arg: (copy arg) star: (copy star) template: template)) 
                                 (setq arg (resize arg argLen))
                                 (setq star (resize star starLen))
                                 (goto StarWildCardMatch:)
                              )) ; end if
                       )) ; end if
                   (setq starMatch[(length starMatch)] phrase[n]) 
                   ) ; end loop
                ;; Finally we check for a terminal leaf at this [*] node.
		       (if (<> (setq template nextPatterns[true]) #void)
		           (begin
		              ;; We get here if we've found an exact match for the kth word in the current phrase.
                       (if (= verbose true) (writeln  (left myBlanks (* k 2)) "Matching [@] with [" (appendFeatures starMatch Value:) "] from tail of [" phrase "]"))
                       ;; Check <that> condition here.
                       (if (and (<> (setq thatCondition nextPatterns["<that>"]) #void) 
                                (<> (setq thatTemplate (aimlMatchWord that 0 thatCondition matchList)) #void))
                           (setq template thatTemplate)
                           (setq template template)
                           ) ; end if
                       (if (<> matchSW true)
                           (return template)
                           (begin
                              (addUnique matchList (new Structure: arg: (copy arg) star: (copy star) template: template)) 
                           )) ; end if
		            )) ; end if
             (setq arg (resize arg argLen))
             (setq star (resize star starLen))
             )) ; end if 
         ;;  (Step 08) Is there a "*" sub-tree in the current aimlPattern buffer?
         ;;            Note: If so, we attempt to find a match for all possible remaining suffixes.
         StarWildCardMatch::
         (if (<> (setq nextPatterns aimlPatterns["*"]) #void)
             (begin
                MatchStar::
                (setq argLen (length arg))
                (setq starLen (length star))
                (setq star[(length star)] (setq starMatch (new Vector: 1 phrase[k])))
                (if (= verbose true) (writeln  (left myBlanks (* k 2)) "Attempting [*] with [" (appendFeatures starMatch Value:) "] from [" k "] in [" phrase "]"))
                (loop for n from (addi k 1) until K do
                   (setq template (aimlMatchWord phrase n nextPatterns matchList))
                   (if (<> template #void) 
                       (begin
                          ;; Save the current star match pattern and return the template.
                          (if (<> matchSW true)
                              (return template)
                              (begin
                                 ;(addUnique matchList (new Structure: arg: (copy arg) star: (copy star) template: template))
                                 (setq arg (resize arg argLen))
                                 (setq star (resize star starLen))
                                 (goto FinalValueMatch:)
                              )) ; end if
                       )) ; end if
                   (setq starMatch[(length starMatch)] phrase[n]) 
                   ) ; end loop
                ;; Finally we check for a terminal leaf at this [*] node.
		       (if (<> (setq template nextPatterns[true]) #void)
		           (begin
		              ;; We get here if we've found an exact match for the kth word in the current phrase.
                       (if (= verbose true) (writeln  (left myBlanks (* k 2)) "Matching [*] with [" (appendFeatures starMatch Value:) "] from tail of [" phrase "]"))
                       ;; Check <that> condition here.
                       (if (and (<> (setq thatCondition nextPatterns["<that>"]) #void) 
                                (<> (setq thatTemplate (aimlMatchWord that 0 thatCondition matchList)) #void))
                           (setq template thatTemplate)
                           (setq template template)
                           ) ; end if
                       (if (<> matchSW true)
                           (return template)
                           (begin
                              (addUnique matchList (new Structure: arg: (copy arg) star: (copy star) template: template)) 
                           )) ; end if
		               )) ; end if
             (setq arg (resize arg argLen))
             (setq star (resize star starLen))
             )) ; end if 
         ;; (Step 09) If we have no match for the current word, we have failed.
         FinalValueMatch::
         (if (= aimlPatterns[phrase[k].Value] #void)
             (begin
                (if (= verbose true) (writeln (left myBlanks (* k 2)) "Failed matching [" phrase[k].Value "] from [" k "] in [" (appendFeatures phrase Value:) "]"))
                (goto Last:)
                )) ; end if
         ;; Seventh, we attempt to find a match for the next word.
         NextWord::
         (setq aimlPatterns aimlPatterns[phrase[k].Value])
         ) ; end loop
      (if (and (= aimlPatterns[true] #void) (= aimlPatterns["?"][true] #void) (= aimlPatterns["@"][true] #void)) (goto Last:))
      (cond
       ((<> aimlPatterns[true] #void) (setq template aimlPatterns[true]))
       ((<> aimlPatterns["?"][true] #void) (begin (setq aimlPatterns aimlPatterns["?"]) (setq template aimlPatterns[true])))
       ((<> aimlPatterns["@"][true] #void) (begin (setq aimlPatterns aimlPatterns["@"]) (setq template aimlPatterns[true])))
       ) ; end cond
      ;; Check <that> condition here.
      (if (and (<> (setq thatCondition aimlPatterns["<that>"]) #void) 
               (<> (setq thatTemplate (aimlMatchWord that 0 thatCondition matchList)) #void))
          (setq template thatTemplate)
          (setq template template)
          ) ; end if
      (if (= verbose true) (writeln "Matching [" (appendFeatures phrase Value:) "] with template [" template "]"))
      (if (= matchSW true)
          (begin
             (addUnique matchList (new Structure: arg: (copy arg) star: (copy star) template: template)) 
          )) ; end if
      ;; Return the matching template (if any).
      Last::
      (if (= matchSW true)
          (if (<> template #void)
              (begin
                 ;(addUnique matchList (new Structure: arg: (copy arg) star: (copy star) template: template)) 
              ))) ; end if
      template) ; end aimlMatchWord
   ;; Send the specified token vector to Alice and receive a response from her rule base.
   ;; Note1: The response will also be a token vector.        
   ;; Note2: May also cause Alice to perform some task as a side effect.        
   (defun applyRules(phrase ...)
      vars:(m M n N record response answer oldInput oldStar oldArg oldDepth topic)
      ;; Initialize alice for chatting (if necessary).
      (if (= (argCount) 2) (setq topic (argFetch 1))) 
      (if (<> myResetSW true) (reset))
      (if (not (isStructure myMemory)) (reset))
      (if (= phrase #void) (setq phrase (new Vector:)))
      (if (isString phrase) (setq phrase (parsePhrase phrase)))
      (if (= verbose true) (writeln _eol "alice.applyRules: receives=[" (appendFeatures phrase Value:) "] " phrase))
      ;; Save the old input phrase etc. and reset all for this recursion.
      (setq oldInput (copy in))
      (setq oldArg (copy arg))
      (setq oldStar (copy star))
      (setq oldDepth mySearchDepth)
      (if (> (++ mySearchDepth) myMaxSearchDepth) (error "alice.applyRules: exceeded maximum search depth"))
      (setq in phrase)
      (setq arg (new Vector:))
      (setq star (new Vector:))
      (setq myHtmlResponseHeading "")
      ;; Apply the AIML patterns in aliceBrain to the specified phrase.
      ;; Note: Act out the AIML response from aliceBrain (may also cause Alice to perform some task as a side effect).
      (setq answer (actionAsTokens (setq response (aimlMatch phrase topic))))
      (if (not (isVector answer)) (setq answer (new Vector:)))
      (if (= verbose true) (writeln "alice.applyRules: returns=[" (appendFeatures answer Original:) "] " answer))
      ;; Restore the old input phrase etc. and return from this recursion.
      (setq in oldInput)
      (setq arg oldArg)
      (setq star oldStar)
      (setq mySearchDepth oldDepth)
      answer) ; end applyRules
   ;; Change the perspective of the specified text phrase (I==>you).        
   (defun changePerspective(phrase)
      vars:(n N element answer)
      ;; Convert the phrase to lower case and separate all words.
      (if (or (not (isString phrase)) (= phrase "")) (return ""))
      (setq phrase (stringToVector (trim (clean (downcase phrase))) (append " " #\tab #\return #\newline "?.,;:~`'!@#$%^&*()-_+=}{[]|\\/><" (char 34) (char 39)) true))
      ;; Apply the AIML perspective conversions to the specified phrase elements.
      (setq N (length phrase))
      (setq answer "")
      (loop for n from 0 until N do
         (setq element phrase[n])
         (cond
           ((= element "mine") (setq element "yours"))
           ((= element "yours") (setq element "mine"))
           ((= element "me") (setq element "you"))
           ((= element "my") (setq element "your"))
           ((= element "your") (setq element "my"))
           ((= element "i") (setq element "you"))
           ((= element "it") (setq element "it"))
           ((= element "you") (setq element "me"))
           ((= element "we") (setq element "you"))
           ((= element "us") (setq element "you"))
           ) ; end cond
         (setq answer (append answer " " element))
         ) ; end loop
      (trim (clean answer))) ; end changePerspective
   ;; Send the specified text phrase to Alice and receive a chatty response 
   ;; Note: may also cause Alice to perform some task as a side effect.        
   (defun chat(phrase ...)
      vars:(m M n N record response answer oldInput oldStar oldArg oldDepth topic)
      ;; Initialize alice for chatting (if necessary).
      (if (= (argCount) 2) (setq topic (argFetch 1))) 
      (if (<> myResetSW true) (reset))
      (if (not (isStructure myMemory)) (reset))
      (if (= phrase #void) (setq phrase (new Vector:)))
      (if (isString phrase) (setq phrase (parsePhrase phrase)))
      (if (= verbose true) (writeln _eol "alice.chat: hears=[" (appendFeatures phrase Value:) "] " phrase))
      ;; Save the old input phrase etc. and reset all for this recursion.
      (setq oldInput (copy in))
      (setq oldArg (copy arg))
      (setq oldStar (copy star))
      (setq oldDepth mySearchDepth)
      (if (> (++ mySearchDepth) myMaxSearchDepth) (error "alice.chat: exceeded maximum search depth"))
      (setq in phrase)
      (setq arg (new Vector:))
      (setq star (new Vector:))
      (setq myHtmlResponseHeading "")
      ;; Apply the AIML patterns in aliceBrain to the specified phrase.
      ;; Note: Act out the AIML response from aliceBrain (may also cause Alice to perform some task as a side effect).
      (if (<> (length phrase) 0)
          (setq answer (actionAsString (setq response (aimlMatch phrase topic))))
          (setq answer "Hello there :-)")
          ) ; end if
      (if (or (not (isString answer)) (= (trim answer) "")) (setq answer (chat "what am I doing here?")))
      (if (= verbose true) (writeln "alice.chat: says=[" answer "]"))
      (setq answer (trim (clean answer)))
      ;; Restore the old input phrase etc. and return from this recursion.
      (setq in oldInput)
      (setq arg oldArg)
      (setq star oldStar)
      (setq mySearchDepth oldDepth)
      answer) ; end chat
   ;; Check in the specified AIML vocabulary agent to the AliceRules file cabinet.        
   (defun checkin(knowledgeBase aimlPatterns)
      vars:(n N aimlAgent currentPatterns (aimlSource "...Source Unavailable...") firstWord)
      (if (or (= knowledgeBase #void) (= knowledgeBase "")) (setq knowledgeBase "AliceRules"))
      (if (not (isDirectory aimlPatterns)) (return false))
      (if (not (isDirectory myAimlPatterns)) (setq myAimlPatterns (new Directory:)))
      ;; Load the specified AIML vocabulary (for the letter specified).
      (setq N (length aimlPatterns))
      (loop for n from 0 until N do
         (setq firstWord aimlPatterns[n 0]) 
         (setq currentPatterns (takeout knowledgeBase firstWord))
         (setq currentPatterns (mergeAimlPatterns currentPatterns aimlPatterns[n 1]))
         (setq aimlAgent (eval "(lambda() pvars:(myTrainingMemory) myTrainingMemory)"))
         (setq aimlAgent.myTrainingMemory currentPatterns)
         (setq aimlAgent.Sc aimlSource)
         (browseAgent.checkin (symbol knowledgeBase) (append knowledgeBase ":" firstWord) aimlAgent)
         ) ; end aimlPatterns loop
      true) ; end checkin
   ;; Check out the specified AIML vocabulary agent from the alice file cabinet.        
   (defun checkout(knowledgeBase firstWord)
      vars:(n N aimlPatterns aimlAgent aimlSource)
      ;; Load the specified AIML vocabulary (for the letter specified).
      (if (or (= knowledgeBase #void) (= knowledgeBase "")) (setq knowledgeBase "AliceRules"))
      (if (not (isDirectory myAimlPatterns)) (setq myAimlPatterns (new Directory:)))
      ;(if (and (isString firstWord) (<> firstWord[0] #\&) (<> firstWord[0] (char 33)) (<> firstWord[0] (char 35)) (<> firstWord[0] #\%) (<> firstWord[0] #\$)) (setq firstWord (upcase firstWord)))
      (if (<> (setq aimlPatterns myAimlPatterns[firstWord]) #void) (return aimlPatterns))
      (setq aimlAgent (browseAgent.takeout (symbol knowledgeBase) (append knowledgeBase ":" firstWord)))
      (if (not (isAgent aimlAgent)) (return #void))
      ;; Make sure the patterns are in the current agent and not its child agent.
      (if (<> aimlAgent.Pv.myTrainingMemory #void)
	      (setq aimlPatterns aimlAgent.myTrainingMemory)
	      (setq aimlPatterns (eval aimlAgent)[myTrainingMemory:])
          ) ; end if 
      ;; Issue and error condition if the pattern buffer is empty.
      (if (= aimlPatterns #void) (error "alice checkout failed on " (append knowledgeBase ":" firstWord))) 
      (setq myAimlPatterns[firstWord] aimlPatterns)
      aimlPatterns) ; end checkout
   ;; Convert an AIML compiled model into a Directory of pattern phrases.        
   (defun extractAimlPatterns(aimlNode ...)
      vars:(k K m M n N
            temp tokBody tokStruct tokScript
            category pattern template topic topicKey
            fact reminder document
            thatCondition thatDict lexValues item lexStruct
            key value dict aimlPatterns record name lispScript
            ) ; end temporary variables
      ;; Return if AIML node is not a Structure.
      (if (= (argCount) 2) (setq aimlPatterns (argFetch 1)))
       ;;(writeln "aimlPatterns: " aimlPatterns)
      ;;(writeln "type aimlPatterns: " (type aimlPatterns)) ;; LTNTemp
      (if (not (isStructure aimlNode)) (return aimlPatterns))
      ;; Initialize if temporary AIML patterns (if necessary).
      (if (not (isDirectory aimlPatterns)) (setq aimlPatterns (new Directory:)))
      ;;(writeln " new aimlPatterns: " aimlPatterns)
      ;; Traverse the current AIML node collecting all patterns.
      (setq N (length aimlNode))
      ;;(writeln "Length AIMLNODE: " N) ;; LTNTemp
 
      (loop for n from 0 until N do
         ;; Collect all patterns.
         (cond
             ;; Search all category/rule tags.
            ((or (= aimlNode[n 0] category:) (= aimlNode[n 0] Category:) (= aimlNode[n 0] CATEGORY:)
                 (= aimlNode[n 0] rule:) (= aimlNode[n 0] Rule:) (= aimlNode[n 0] RULE:))
             (begin
                (setq category aimlNode[n 1])
 
                ;; There might be a <category topic='name'> topic attribute.
                (setq topic category.__attlist.topic)

                (if (= topic #void) (setq topic category.__attlist.Topic))
                (if (= topic #void) (setq topic category.__attlist.TOPIC))
 
                ;; There must be a <pattern> tag.
                (setq pattern category.pattern)
                (if (= pattern #void) (setq pattern category.Pattern))
                (if (= pattern #void) (setq pattern category.PATTERN))
 
                ;; There may be a <that> tag.
                (setq thatCondition category.that)
                (if (= thatCondition #void) (setq thatCondition category.That))
                (if (= thatCondition #void) (setq thatCondition category.THAT))
 
                ;; There must be a <template> tag, or <rule> tag, or <lisp> tag.
                (setq template category.template)
                (if (= template #void) (setq template category.Template))
                (if (= template #void) (setq template category.TEMPLATE))
                (if (and (= template #void) (<> category.APPLY #void)) (setq template (new Structure: apply: category.APPLY)))
                (if (and (= template #void) (<> category.Apply #void)) (setq template (new Structure: apply: category.Apply)))
                (if (and (= template #void) (<> category.apply #void)) (setq template (new Structure: apply: category.apply)))
                (if (and (= template #void) (<> category.SRAI #void)) (setq template (new Structure: srai: category.SRAI)))
                (if (and (= template #void) (<> category.Srai #void)) (setq template (new Structure: srai: category.Srai)))
                (if (and (= template #void) (<> category.srai #void)) (setq template (new Structure: srai: category.srai)))
                (if (and (= template #void) (<> category.LISP #void)) (setq template (new Structure: lisp: category.LISP)))
                (if (and (= template #void) (<> category.Lisp #void)) (setq template (new Structure: lisp: category.Lisp)))
                (if (and (= template #void) (<> category.lisp #void)) (setq template (new Structure: lisp: category.lisp)))
                (if (and (= template #void) (<> category.STRING #void)) (setq template (new Structure: string: category.STRING)))
                (if (and (= template #void) (<> category.String #void)) (setq template (new Structure: string: category.String)))
                (if (and (= template #void) (<> category.string #void)) (setq template (new Structure: string: category.string)))

;;(writeln "type template: " (type template))
;;(writeln "template: " template)
 
                ;; We only add words if there is a pattern tag
                ;; Note: There must be a pattern tag and a template tag.
                (if (and (<> pattern #void) (<> template #void)) 
                    (begin
                       ;; Place the category in the main rulebase or in a localized topic rulebase.
                       (if (= topic #void)
                           then
                           (setq dict aimlPatterns)
                            ;;(writeln "setting dict aimlPatterns: " aimlPatterns) ;; LTNTemp
                           else
                           (begin
                             (setq topicKey (symbol (append "&" topic)))
 
                             (setq dict aimlPatterns[topicKey])

 
                             (if (not (isDirectory dict)) (setq dict (new Directory:)))
                             (setq aimlPatterns[topicKey] dict)
;;(writeln "aimlPatterns[topicKey]: " aimlPatterns )
                             ) ; end else
                           ) ; end if
                       ;; (writeln "dict: " dict) ;; LTNTemp
                       ;; Construct a vector of strings out of the pattern tag
                       ;; Note: We must handle both key word content and embedded lisp tags.
                       (setq temp pattern)
;;(writeln "temp is: " temp)
                       (setq pattern (setq key #void))
                       (cond
                        ;; Case where pattern is a string
                        (  (isString temp)   
                         (begin
                           (setq pattern temp)
;;(writeln " if pattern is a string: " )
;;(writeln "pattern: " pattern)
;;(writeln "pattern type: " (type pattern))
(setq pattern (string pattern))
                           (setq key (stringToVector pattern " ")) 
 
                         )) ; end string case
                        ;; Case where pattern is a structure                        
                        ((isStructure temp) 
                         (begin
                           (setq pattern "")
                           (setq key (new Vector:))
                           (setq K (length temp))
                           (loop for k from 0 until K do
                              (cond
                               ((= temp[k 0] __comment:)
                                  (begin
 
                                     (setq pattern pattern)
                                     (setq key key)
                                  )) ; end case
                               ((= temp[k 0] __content:)
                                  (begin

(setq pattern (string pattern))
                                     (setq pattern (append pattern temp[k 1] " "))
                                     (setq key (append key (stringToVector temp[k 1] " ")))
                                  )) ; end case
                               ((= temp[k 0] lisp:)
                                  (begin
                                     (setq pattern (append pattern "#lisp#" temp[k 1] " "))
                                     (setq key[(length key)] (append "#lisp#" (string temp[k 1])))
 
                                  )) ; end case
                               ((= temp[k 0] token:)
                                  (begin
                                     (setq tokScript #void)
 
                                     (setq tokBody "")
                                     (if (isString temp[k 1])    (setq tokBody temp[k 1]))
                                     (if (isStructure temp[k 1])
                                         (begin
                                            (setq tokBody temp[k 1].__content)
                                            (if (<> temp[k 1].__attlist.lisp #void) (setq tokScript (append "#lisp#" temp[k 1].__attlist.lisp)))
                                         )) ; end if
                                     (setq tokStruct (parse (append "#{" tokBody "}")))
                                     (if (or (not (isStructure tokStruct)) (<= (length tokStruct) 0)) (error (append "alice: invalid body in <token>" tokBody "</token>")))
                                     (setCdr tokStruct tokScript)
                                     (setq pattern (append pattern (string tokStruct true) " "))
                                     (setq key[(length key)] (append (string tokStruct true)))
                                  )) ; end case
                               (else
                                  (error (append "alice: invalid tag [" temp[k 0] "] in <pattern>" pattern "</pattern>"))
                                  ) ; end case
                               ) ; end cond
                             ;;(writeln "structure: key: " key) ;; LTNTemp
                              ) ; end loop 
                         )) ; end structure case
                        ) ; end cond
                       ;; Add the pattern words using the key vector as a guide.
                       ;;(writeln "key: " key) ;; LTNTemp
                       (setq K (length key))
                       (loop for k from 0 until K do
                          (if (= (invalidPatternWord key[0]) true) (error (append "alice: invalid word [" key[0] "] in <pattern>" pattern "</pattern>")))  
                          (if (= dict[key[0]] #void) (setq dict[key[0]] (new Directory:)))
                          (setq dict dict[key[0]])

                          (setq key (delete key 0)) 
                          ) ; end loop
;;(writeln " before that Condition aimlPatterns: " aimlPatterns)
;;(writeln " before that Condition dict: " dict)
                       (if (= thatCondition #void) (setq dict[true] template))
;;(writeln " after that Condition aimlPatterns: " aimlPatterns)
;;(writeln " after that Condition dict: " dict)
                       ;; Add the <that> tag conditional words
		               (if  (isString thatCondition)   
		                   (begin
		                      ;; Add the pattern words
                              (setq thatCondition (string thatCondition))
		                      (setq key (stringToVector thatCondition " "))
                       		(setq thatDict dict["<that>"])
                       		(if (= thatDict #void) (setq thatDict (new Directory:)))
		                      (setq dict["<that>"] thatDict)
		                      (setq K (length key))
		                      (loop for k from 0 until K do
                                 (if (= (invalidPatternWord key[0]) true) (error (append "alice: invalid word [" key[0] "] in <that>" thatCondition "</that>")))  
		                         (if (= thatDict[key[0]] #void) (setq thatDict[key[0]] (new Directory:)))
		                         (setq thatDict thatDict[key[0]])
		                         (setq key (delete key 0)) 
		                         ) ; end loop
                       		  (setq thatDict[true] template)
		                       ;; Add the <that> tag conditional words
		                   )) ; end if
                    )) ; end pattern if
             )) ; end category/rule case
             ;; Search all fact tags.
            ((or (= aimlNode[n 0] fact:) (= aimlNode[n 0] Fact:) (= aimlNode[n 0] FACT:))
             (begin
                (setq fact aimlNode[n 1])
                ;; There must be a <reminder> tag.
                (setq reminder fact.reminder)
                (if (= reminder #void) (setq reminder fact.Reminder))
                (if (= reminder #void) (setq reminder fact.REMINDER))
                ;; There must be a <document> tag, or <rule> tag, or <lisp> tag.
                (setq document fact.document)
                (if (= document #void) (setq document fact.Document))
                (if (= document #void) (setq document fact.DOCUMENT))
                ;; We only add words if there is a reminder tag
                ;; Note: There must be a reminder tag and a document tag.
                (if (and (<> reminder #void) (<> document #void)) 
                    (begin
                       ;; Place the fact in the main factbase.
                       (setq dict aimlPatterns[myFactName])
                       (if (not (isDirectory dict)) (setq dict (new Directory:)))
                       (setq aimlPatterns[myFactName] dict)
                       ;; Construct a vector of strings out of the reminder tag
                       (setq temp reminder)
                       (setq reminder (setq key #void))
                       (cond
                        ;; Case where reminder is a string
                        ( (isString temp)  
                         (begin
                           (setq reminder temp)
                           (setq reminder (string reminder))
                           (setq key (stringToVector reminder " ")) 
                         )) ; end string case
                        ;; Case where reminder is a structure                        
                        ((isStructure temp) 
                         (begin
                           (setq reminder "")
                           (setq key (new Vector:))
                           (setq K (length temp))
                           (loop for k from 0 until K do
                              (cond
                               ((= temp[k 0] __comment:)
                                  (begin
                                     (setq reminder reminder)
                                     (setq key key)
                                  )) ; end case
                               ((= temp[k 0] __content:)
                                  (begin
                                     (setq reminder (string reminder))
                                     (setq reminder (append reminder temp[k 1] " "))
                                     (setq key (append key (stringToVector temp[k 1] " ")))
                                  )) ; end case
                               (else
                                  (error (append "alice: invalid tag [" temp[k 0] "] in <reminder>" reminder "</reminder>"))
                                  ) ; end case
                               ) ; end cond
                              ) ; end loop 
                         )) ; end structure case
                        ) ; end cond
                       ;; Add the reminder words using the key vector as a guide.
                       (setq K (length key))
                       (loop for k from 0 until K do
                          (if (= (invalidPatternWord key[0]) true) (error (append "alice: invalid word [" key[0] "] in <reminder>" reminder "</reminder>")))  
                          (if (= dict[key[0]] #void) (setq dict[key[0]] (new Directory:)))
                          (setq dict dict[key[0]])
                          (setq key (delete key 0)) 
                          ) ; end loop
                       (setq dict[true] document)
                    )) ; end reminder if
             )) ; end fact case
             ;; Search all lex tags.
            ((or (= aimlNode[n 0] lex:) (= aimlNode[n 0] Lex:) (= aimlNode[n 0] LEX:))
             (begin
                ;; Retrieve lex tag feature template and attributes.
                (setq record aimlNode[n 1])
                (setq name (symbol record.__attlist.name))
                (setq lexValues record.__attlist.values)
                (setq lispScript record.__attlist.define)
                (setq template record.__content)
                ;; Load existing lexical feature definitions (if any).
                (setq dict myAimlPatterns[myLexName])
                (if (= dict #void) (setq dict (new Directory:)))
                ;; Convert lex tag template into a vector.
                (if  (isString template)  
                    (setq template (stringToVector template " ")) 
                    (setq template (new Vector:))
                    ) ; end if 
                ;; Match lex tag values (if any) with template entries.
                (if (= lexValues #void) (setq lexValues (new Vector: 1 #void)))
                (if  (isString lexValues)  
                    (begin
                       ;; for bytevector bug
                       (setq lexValues (string lexValues))
                       (setq lexValues (stringToVector lexValues " ")) 
                       (setq M (length lexValues))
                       (loop for m from 0 until M do
                          (setq lexValues[m] (parse lexValues[m]))
                          ) ; end loop
                    )) ; end if
                (if (> (length template) 0)
                    (setq lexStruct (objectToStructure template lexValues))    
                    (setq lexStruct (new Structure:))
                    ) ; end if    
                ;; Retrieve lex tag Lisp script template (if any) entries.
                (if  (isString lispScript) 
                    (setq lexStruct (append lexStruct (objectToStructure (eval lispScript) #(#void)))) 
                    ) ; end if 
                ;; Merge with existing lexical feature definitions (if any).
                (if (isStructure dict[name])
                    (setq lexStruct (append lexStruct dict[name])) 
                    ) ; end if
                ;; Merge with existing lexical feature definitions (if any).
                (setq pattern (new Structure:))
                (setq M (length lexStruct))
                (loop for m from 0 until M do
                   (setq key lexStruct[m 0])
                   (setq value lexStruct[m 1])
                   (if (= key[0] #\$) (setq key dict[(mid key 1 1000000)]))
                   (if (= key[0] #\%) (setq key dict[(mid key 1 1000000)]))
                   (if (isStructure key) 
                       (setq pattern (append pattern key))
                       (if (or (isByteVector key)(isSymbol key)) (setq pattern[key] value))
                       ) ; end if
                   ) ; end loop
                (setq M (length pattern))
                (loop for m from 0 until M do
                   (setq key pattern[m 0])
                   (setq value (parse pattern[m 1]))
                   (setq pattern[key] value)
                   ) ; end loop
                (setq dict[name] pattern)
                (setq aimlPatterns[myLexName] dict)
;;(writeln "aimlPatterns myLexName: " aimlPatterns)
                (setq myAimlPatterns[myLexName] dict)
             )) ; end lex case
            ;; Search all comment tags.
            ((= aimlNode[n 0] __comment:) 
             (setq aimlPatterns aimlPatterns)
             ) ; end comments case
            ;; Search all other tags.
            (else
             (error (append "alice: unsupported AIML tag <" aimlNode[n 0] ">"))
             ) ; end else case
            ) ; end cond       
         ) ; end loop
 ;;(writeln " end extractAimlPatterns: " aimlPatterns);; LTNTemp
      aimlPatterns) ; end extractAimlPatterns
   ;; Get all of the registered Alice input words with the specified feature.
   ;; Args: featureList  Singleton feature name or vector of feature names
   ;;       logic	       (Optional) all or any (default any).
   (defun getWordsWithFeature(featureList ...) 
      vars:(n N m M matchCount wordList tokens word features logic)
      (if (not (isStructure myMemory)) (reset))
      (if (= (argCount) 2) (setq logic (argFetch 1)))
      (if (isStructure featureList) (setq featureList (objectToVector (refAttributes featureList))))
      (if  (isString featureList) (isSymbol featureList)   (setq featureList (new Vector: 1 featureList)))
      (setq M (length featureList))
      (setq wordList (new Vector:))
      (setq tokens alice.queryScript.tokenDirectory)
      (setq N (length tokens))
      (loop for n from 0 until N do
         (setq word tokens[n 0])
         (setq features tokens[n 1])
         (setq matchCount 0)
         (loop for m from 0 until M do
            (if (and (<> logic all:) (<> features[featureList[m]] #void)) (begin (setq wordList[(length wordList)] word) (goto NextToken:)))
            (if (and (= logic all:) (<> features[featureList[m]] #void)) (++ matchCount))
            ) ; end match loop
         (if (and (= logic all:) (= matchCount M)) (setq wordList[(length wordList)] word))
         NextToken::      
         ) ; end token loop
      wordList) ; end getWordsWithFeature
   ;; Return true iff the argument is an invalid word for a <pattern> or <that> string.
   (defun invalidPatternWord(word)
      vars:(n N)
      (if (<> (isCharAlphabetic word[0]) true) (return false))
      (setq N (length word))
      (loop for n from  0 until N do
         (if (= (isCharLowercase word[n]) true) (return true))
         ) ; end loop
      false) ; end invalidPatternWord 
   ;; Returns true if the argument is a String or a ByteVector
   (defun  isString(word) (or (|Gv:isString| word) (isByteVector word)))
   ;; Merge an existing AIML model with a newly compiled Directory of pattern phrases.        
   (defun mergeAimlPatterns(aimlNode aimlPatterns)
      vars:(k K n N category pattern template key value dict)
      ;; Return if AIML node is not a Directory.
      (if (not (isDirectory aimlNode)) (return aimlPatterns))
      ;; Initialize if temporary AIML patterns are not a Directory.
      (if (not (isDirectory aimlPatterns)) (return aimlNode))
      ;; Traverse the newly compiled AIML model inserting all
      ;; patterns into the existing aimlNode non-destructively.
      (setq N (length aimlPatterns))
      (loop for n from 0 until N do
         ;; Collect all newly compiled patterns.
         (setq key aimlPatterns[n 0])
         (setq value aimlPatterns[n 1])
         (setq dict aimlNode[key])
         (cond
            ;; Manage case where node entry is void. 
            ((= dict #void)  
             (begin
                (setq aimlNode[key] value)
             )) ; end case
            ;; Manage case where both entries are identical. 
            ((= dict value)  
             (begin
                (setq aimlNode[key] value)
             )) ; end case
            ;; Manage case where both entries are directories. 
            ((and (isDirectory dict) (isDirectory value))  
             (begin
                (setq aimlNode[key] (mergeAimlPatterns dict value))
             )) ; end case
            ;; Manage case where current entry is a directory. 
            ((and (isDirectory dict) (not (isDirectory value)))  
             (begin
                (setq dict[true] value)
             )) ; end case
            ;; Manage all other case. 
            (else  
             (begin
                (setq aimlNode[key] value)
             )) ; end case
            ) ; end cond 
         ) ; end loop
      aimlNode) ; end mergeAimlPatterns
   ;; Convert the text phrase into a set of Alice feature based tokens.
   (defun phraseToTokens(phrase)
      vars:(n N wordVector)
      ;; Convert the input phrase into a vector of upper case word (no blanks or special characters allowed).
      (if (= phrase #void) (return (new Vector:)))
      (if (not (isString phrase)) (setq phrase (string phrase true)))
      (setq wordVector (queryScript phrase))
      (if (not (isVector wordVector)) (return (new Vector:)))
      (setq N (length wordVector))
      (loop for n from 0 until N do 
        (if (<> wordVector[n].Original #void) (setq wordVector[n].Value wordVector[n].Original))
        ) ; end loop
      wordVector) ; end phraseToTokens 
   ;; Create a script template agent for use with lisp tags.
   (defun scriptAgent() true)
   ;; Perform the Alice self test.
   ;; Note: This self test requires that the AliceAIML, mathAIML, and
   ;;       the testAIML rules be compiled into the AliceRules repository.
   (defun selfTest() 
      vars:(n N
            result test 
            startTick endTick
            ) ; end temporary variables
      (writeln _eol "*************************alice.selfTest**********************************************")
      (gc)(setq startTick (getTickCount 0))
      (testQuery "" "Hello there :-)" #void) 
      (testQuery "What is your name?" "My name is Alice ." #void) 
      (testQuery "What is the capital of France?" "Paris." #void) 
      (testQuery "Who is Dr. Wallace?" "He is a famous computer scientist the author of ALICE and a graduate of Carnegie Mellon" #void) 
      (testQuery "What is twenty three hundred fourty six times the square root of five thousand twenty three?" "166268.3537779" #void) 
      (testQuery "ZZZTEST Test switch" "Please Test my switch ." selfTest:) 
      (testQuery "ZZZTEST hello Test switch" "Your wild card is hello ." selfTest:) 
      (testQuery "ZZZTEST Test me?" "Why should I?" selfTest:) 
      (testQuery "ZZZTEST the queen's Black royal Watch?" {The second star is " royal" .} selfTest:) 
      (testQuery "ZZZTEST gosh play the Black Watch pipes for me?" {The preposition is " for" .} selfTest:) 
      (testQuery "ZZZTEST gosh blow the Black Watch pipes for me?" {The second star is " " .} selfTest:)
      (testMatch "ZZZTEST gosh play the Black Watch pipes for me?" #({The preposition is " for" .} {The second star is " " .})  selfTest:) 
      (testMatch "ZZZTEST gosh play the Black Scottish Watch pipes for me?" #({The preposition is " for" .} {The second star is " Scottish" .}) selfTest:) 
      (setq endTick (getTickCount startTick))
      (writeln      "*************************alice.selfTest completed in [" endTick "] seconds.")
      true) ; end selfTest
   ;; Check out the specified AIML vocabulary agent from the alice file cabinet.        
   (defun takeout(knowledgeBase firstWord)
      vars:(n N aimlPatterns aimlAgent aimlSource)
      ;; Load the specified AIML vocabulary (for the letter specified).
      (if (or (= knowledgeBase #void) (= knowledgeBase "")) (setq knowledgeBase "AliceRules"))
      (setq aimlAgent (browseAgent.takeout (symbol knowledgeBase) (append knowledgeBase ":" firstWord)))
      (if (not (isAgent aimlAgent)) (return #void))
      ;; Make sure the patterns are in the current agent and not its child agent.
      (if (<> aimlAgent.Pv.myTrainingMemory #void)
	      (setq aimlPatterns aimlAgent.myTrainingMemory)
	      (setq aimlPatterns (eval aimlAgent)[myTrainingMemory:])
          ) ; end if 
      ;; Issue and error condition if the pattern buffer is empty.
      (if (= aimlPatterns #void) (error "alice takeout failed on " (append knowledgeBase ":" firstWord))) 
      aimlPatterns) ; end takeout
   ;; Find all Alice matches for the specified query and compare the specified result with a desired result.
   (defun testMatch(test target topic)
      vars:(result)
      (if (<> (setq result (alice.actOnAll test topic)) target) (writeln "alice.SelfTest: error when (alice.actOnAll \"" test "\"), returned [" result "], instead of [" target "]"))
      true) ; end testMatch
   ;; Query Alice and compare the result with a desired result.
   (defun testQuery(test target topic)
      vars:(result) 
      (if (<> (setq result (alice.query test topic)) target) (writeln "alice.SelfTest: error when (alice.query \"" test "\"), returned [" result "], instead of [" target "]"))
      true) ; end testQuery
   ;; Validate embedded AIML tags during Alice rule compilation.
   (defun validateTags(parentTag parentValidTags childValidTags modelNode)
      vars:(n N tag record)
      (if (not (isStructure modelNode)) (return true))
      (setq N (length modelNode))
      (loop for n from 0 until N do
         (setStrKey modelNode n (setq tag (symbol (downcase (string modelNode[n 0])))))
         (setq record modelNode[n])         
         (if (= parentValidTags[tag] #void) 
             (error (append "alice: [" parentTag "]=(" (left (string modelNode true) 40) ") tag contains unsupported AIML tag [" tag "]=(" (left (string record true) 40) ")"))
             ) ; end if
         (if (and (isStructure record) (<> tag[0] #\_)) (validateTags tag childValidTags childValidTags record))
         ) ; end 
      true) ; end validateTags
   ;; ****************************************************************
   ;; Begin alice main code section
   ;; ****************************************************************
   ;; Compile the XML source statements into a hierarchical XML model.
   ;; Note: Isolate the elements of the AIML model.
   (reset)
   (setq tempXmlModel (xml aimlSource))
    ;;(writeln "type tempXmlModel: " (type tempXmlModel)) ;; LTNTemp
   (setq aimlModel tempXmlModel.aiml)
    ;;(writeln "type aimlModel: " (type aimlModel)) ;; LTNTemp
   (if (= aimlModel #void) (setq aimlModel tempXmlModel.Aiml))
   (if (= aimlModel #void) (setq aimlModel tempXmlModel.AIML))
   (cond
     ((= aimlModel #void) 
      (begin 
         (setq lastTag "...") 
         (setq lastRecord "...")
      )) ; end case 
     ((= (length aimlModel) 0) 
      (begin 
         (setq lastTag "...") 
         (setq lastRecord "...")
      )) ; end case 
     (else 
      (begin
         (setq N (sub1 (length aimlModel))) 
         (setq lastTag aimlModel[N 0]) 
         (setq lastRecord aimlModel[N 1])
           ;;(writeln "lastTag: " lastTag) ;; LTNTemp
           ;;(writeln "lastRecord: " lastRecord) ;; LTNTemp
      )) ; end else 
     ) ; end cond


   ;; Check for any misplaced AIML elements in the hierarchical XML model.
   (setq N (length tempXmlModel))
   ;; (writeln "tempXmlModel: " tempXmlModel) ;; LTNTemp
   ;; (writeln "length tempXmlModel: " N) ;; LTNTemp
   (loop for n from 0 until N do
      (setStrKey tempXmlModel n (setq tag (symbol (downcase (string tempXmlModel[n 0])))))   
      ;; (writeln "tempXmlModel tags: " tag) ;; LTNTemp 
      (if (isMember tag #(category template pattern lex rule srai lisp))
          (error (append "alice: mismatched AIML tags between " 
                         "[<" lastTag ">" (substitute (left (string lastRecord true) 60) #\newline ".") "] and "
                         "[<" tempXmlModel[n 0] ">" (substitute (left (string tempXmlModel[n 1] true) 60) #\newline ".") "]"))
          ) ; end if          
      ) ; end loop
   ;; Check for any invalid AIML tag elements in the AIML model.
   (setq N (length aimlModel))
     ;;(writeln "length aimlModel: " N) ;; LTNTemp
   (loop for n from 0 until N do
      ;; Validate all AIML level tags.
      (setStrKey aimlModel n (setq tag (symbol (downcase (string aimlModel[n 0])))))  
      ;; (writeln "AIML tags: " tag)       ;; LTNTemp
      (if (= myValidAimlTags[tag] #void) 
          (error (append "alice: unsupported AIML top level tag [" tag "]"))
          ) ; end if
      ;; Validate all Category level tags.
      (if (= aimlModel[n 0] category:) (validateTags tag myValidCategoryTags myValidTemplateTags aimlModel[n]))
      ) ; end loop
   ;; Extract all patterns into the pattern buffer.
   (setq myAimlPatterns (new Directory:))
   (setq myAimlPatterns[myLexName] (checkout "AliceRules" myLexName))
 
   (setq tempPatterns (extractAimlPatterns aimlModel))
   ;;(writeln "tempPatterns: " tempPatterns)
   ;; Check the compiled question/response template into the AliceRules repository.
   (if (<> tempPatterns[myFactName] #void) (checkin "AliceFacts" tempPatterns[myFactName]))
   (delete tempPatterns myFactName)
   (checkin "AliceRules" tempPatterns)
   (reset)
   ;; Initialize the agent Smartbase universal parse tree for the AIML compiled source code.
   ;; Note: We store all compiled question/response templates only in the AliceRules repository.
   (setq parseTree (list (list (symbol "lambda") (list x:)  (makeQuotedSymbol "pvars") (list (list myTrainingMemory: #void)) true)))
   parseTree)  ;; end of alice




;;**EXPORTKEY**:alice:%ANSWERDETAILS
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

	// Record details have been successfully loaded.
	return true;
} 
        
</script>
<!--------------------------------------------------------------------------------->
<!---                     End Scripts for Control Mnagement                    ---->
<!--------------------------------------------------------------------------------->

<TABLE>
<TR>
<TD><IMG SRC="alicelogo.gif" WIDTH=100 HEIGHT=100></TD>
<TD>
	<TABLE><TR><TD>
	<TEXTAREA NAME="AliceQuery" ALIGN="left" ROWS="2" COLS="80"><!--$$$QUERYSTRING$$$--></TEXTAREA>
	</TD><TD>
	<INPUT TYPE='button' VALUE='Next Query' onClick='loadDetails(AliceQuery.value);'>
	</TD><TD>
	<INPUT TYPE='button' VALUE='Help' onClick='loadDetails("Help?");'>
	</TD></TR>
	</TABLE>
</TD>
</TR>
<TR><TD><FONT COLOR="#000080"><H4>My answer is:</H4></FONT></TD></TR>
</TABLE>

<P>
<B>
<!--#MyTextResponse#-->
</B>
</P>
<BR>

<!--#MyHTMLResponseHeading#-->

<P>
<DIV>
<!--#MyHTMLResponseResults#-->
</DIV>
</P>


</BODY>
</HTML>




;;**EXPORTKEY**:alice:%MEMORY_HDR
;#text#
(lambda()
;; *******************************************************************
;;  summary:  This Master Analyst my Training Memory Template serves
;;            as a storage module for the training history of this 
;;            Master Analyst learning module.
;;            
;;            Each Master Analyst ranks stocks into integer categories
;;            from -pp to +pp (pp is an expected Next3MonthProfit percent
;;            and is stored in myTrainingMemory.Score). This memory
;;            module memorizes the weekly behavior history of each category
;;            of stock over the entire training period. This historical
;;            information is used by the Master Analyst agent to make
;;            guestimates about the future behavior of a stock.
;;            
;;            A Master Analyst agent may use many memory modules with
;;            many schemes for categorizing stock historical behavior.
;;            This agent is capable of storing the training memory for
;;            any arbitrary filter Rank matrix stock categorization 
;;            scheme.
;;
;;  Args:     none
;;  Return:   true
;; *******************************************************************
   ;; The current historical and training memory for this storage module agent.
   pvars:(;; The current historical and training memory for this storage module agent.
          (myTrainingMemory





















;;**EXPORTKEY**:alice:%MEMORY_TAIL
;#text#

                          )) ;; end of myTrainingMemory

    (myself)) ; end lambda























;;**EXPORTKEY**:alice:queryScript
(defriend alice:queryScript(_input)
;; ********************************************************************
;; summary:  The alice.queryScript compiler generated from alice:queryScript:DEFINITION.
;; Summary:  This agent implements the alice.queryScript compiler as defined
;;           in the alice:queryScript:DEFINITION compiler definition file.
;;           Much code has been marked with a boxed comment lines for
;;           ease of human understanding.
;; Note:     This code was machine generated by parseAgent.
;; Parms:    _input   The alice.queryScript language source string
;; return:   _result  The agent resulting from compiling the _input source.
;; Modification history:
;; TM Jan 15 99 Added Console Error suppression (see _consoleError, _makeError and _lastError)
;; TM Jan 20 99 Added _verboseLexIn - a directory of routines to be _verbose in
;; TM Jan 20 99 Added _verboseSynIn - a directory of routines to be _verbose in
;; TM Jan 20 99 Added _verboseSemIn - a directory of routines to be _verbose in
;; TM Jan 20 99 Changes _verbosexxxIn so that you supply a stop count
;;              example: (setq alice:queryScript._verboseSynIn.MYRULE 2})  ; error after 2nd pass
;;                       (setq alice:queryScript._verboseSynIn.MYRULE 0})  ; verbose on every pass
;;                       (setq alice:queryScript._verboseSynIn.MYRULE: -1}) ; not verbose
;; TM Nov 13 01 Added _verboseLex   - flag forcing _verbose only in Lex
;;                    _verboseSyn   - flag forcing _verbose only in Syntax
;;                    _verboseSem   - flag forcing _verbose only in Semantic
;; TM Nov 19 10 Added new error handling that automatically turns verbose on near the 
;;              parse error. This makes most of the other error handling in the tool 
;;              obsolete except when you want to see a full trace of all rule attempts from
;;              the begining of one of the passes.
;; ********************************************************************
   pvars:(_changeCount             ;; Number of rule based substitutions
          _explainOnOff            ;; Switch for saving semantic explanation steps in the _explanation variable 
          _explanation             ;; Variable for saving semantic explanation steps (see _explainOnOff) 
          _indent                  ;; Indent for displaying each explanation step on the console
          _io                      ;; Current parse tree index object
          _ip                      ;; Current parse tree index pointer
          (_maxPasses 200)         ;; Maximum number of passes before issuing error (singlePass = false)
          (morphFail |*failure*|:) ;; Morph rule failure RHS value
          _name                    ;; Name of the current compiler definition
          _parseLen                ;; Current parse tree length
          _parseTree               ;; Current head of the parse tree during recognition
          _passCount               ;; Current number of passes already executed by apply
          _result                  ;; Final result of parsing the submitted compiler definition rules 
          _semanticRule            ;; Current semantic rule to be applied by _applyRule 
          _semanticVerbose         ;; Switch for displaying each semantic explanation step on the console 
          _showTokens              ;; Show only the token list resulting from the lexical analyzer
          _syntaxFeatures          ;; Syntax features supplied in the compiler definition
          _tkIN                    ;; Place holder for the input source string (see $IN) 
          _tkLIST                  ;; The output token list from the lexer rules.
          _tkOUT                   ;; Output a feature based token to the token list.
          tokenDirectory           ;; Lexicon of token and their attributes
          _userFunctions           ;; User functions source code supplied in the compiler definition
          _verbose                 ;; Switch for displaying each explanation step on the console 
          _verboseLex			   ;; Switch for displaying only lexical steps
          _verboseSyn              ;; Switch for displaying only syntax steps
          _verboseSem              ;; Switch for displaying only sematic steps
          _verboseLexIn            ;; Structure of lex parse routines in which we should force _verbose true
          _verboseSynIn            ;; Structure of syntax parse routines in which we should force _verbose true
          _verboseSemIn            ;; Structure of semantic parse routines in which we should force _verbose true
          _verboseLexCount         ;; Structure of Counts used by _verboseLexCount (not set by user!)
          _verboseSynCount         ;; Structure of Counts used by _verboseSynCount (not set by user!)
          _verboseSemCount         ;; Structure of Counts used by _verboseSemCount (not set by user!)
          _lastError               ;; Error encountered during parse 
          
          _ruleCount               ;; Number of rules tried 
          _ruleCountLex
          _ruleCountSyn
          _ruleCountSem
          _failurePass
          _failureIn
          _verboseState
          (_verboseTrigger -1)     ;; If more than zero this indicates at what _ruleCount _verbose should be turned on
     
          _incCount				   ;; increment number of rules tried and test for error condition
          _startLog
           
          (_consoleErrors true)    ;; Error messaging flag is false no messages will be printed to console
          ;; Methods list 
          _findLineNum             ;; find the line the error ocurred in
          _makeError               ;; _error function wrapper that creates pretty errors and sets _lastError values
          _error                   ;; error function wrapper allowing silent or console messaging
          _apply                   ;; Apply the specified semantic rule to the result
          _applyRule               ;; Apply the current semantic rule to a sub list
          appendList               ;; Append multiple arguments into a list
          defaultLexer             ;; Default lexical analyzer for recognizing input symbols
          defaultTokenRule         ;; Modified default rule for adding attributes to a parsed token
          _eofToken                ;; Return true if we are at the end of the parse tree
          _errorHandler            ;; Handle any errors which may occur during compilation
          _getToken                ;; Get the next attributed token in the parse tree
          _Initialize              ;; Initialization routine for setting token dicrectory, etc.
          _initializeSW            ;; Initialization switch (set true after first initialization).
          initRule                 ;; User defined initialization routine for setting token dicrectory, etc.
          _lastIp                  ;; Move the current parse tree index to the previous position
          _lenIp                   ;; Return the length of the current parse tree
          _nextIp                  ;; Move the current parse tree index to the next position
          outputRule               ;; Default rule for returning the final output from the compiler
          _popIp                   ;; Pop the current parse tree index up one level to the next position
          preLexRule               ;; Default rule for any pre-lexical compiler operations.
          _pushIp                  ;; Push the current parse tree index down one level to the next position
          _setLexicalFeature       ;; Assign a set of letters to the specified lexical feature
          _setSyntaxFeature        ;; Set a whole class of syntax tokens with the specified attribute
          _setWordFeatures         ;; Set or enhance a whole word with the specified features and feature values
          _showInput               ;; Show a fragment of the input source string in lexical verbose mode
          _showSource              ;; Show a fragment of the input source string in syntax verbose mode
          startRule                ;; Default rule for starting the compiler
          _writeRule               ;; Display the results of a rule firing in verbose mode
          $IN                      ;; Place holder for the input source string (see _tkIN)
          $LIST                    ;; The output token list from the lexer rules.
          $OUT                     ;; Output a feature based token to the token list.
          $ASIS                    ;; Output a feature based token to the token list (as is).






          ;; Variables to hold lexical feature bit maps
          _LF_Digit
          _LF_Alpha
          _LF_AlphaNum
          _LF_NameChar
          _LF_Letter
          _LF_NameStart
          _LF_Lt
          _LF_Bang
          _LF_Operator
          _LF_DQuote
          _LF_NotDQuote
          _LF_Quote
          _LF_NotQuote
          _LF_Whitespace
          _LF_Eol
          _LF_NotEol
          _LF_Period
          _LF_Exponent
          _LF_Sign
          ;; Functions to implement Lexical Rules
          _LEXRULE_CDATA
          _LEXRULE_MAIN
          ;; Functions to implement Syntax Rules
          _SYNRULE_MAIN
          ;; Functions to implement Semantic Rules
         ) ;; end of persistent variables
   vars:(i verboseHold outString outExplain)
   ;; ***************************************************
   ;; Define the child agents which belong to this parent
   ;; ***************************************************

   ;;*********************************************************************
   ;; 
   ;;*********************************************************************
   (defun _incCount()
   	(setq _ruleCount (+ _ruleCount 1)) 
   	(if (and (> _verboseTrigger 0) (> _ruleCount _verboseTrigger) (not _verbose)) (_startLog))
   	
   	true)

   ;;*********************************************************************
   ;; 
   ;;*********************************************************************
   (defun _startLog()
   		vars:(i len)
		(setq _verbose true)
		(writeln "*********Lexed Tokens with Features Near Failure*********")
		(setq len (min (+ _ip 50) (length _parseTree)))
		(loop for i from _ip until len do
			(writeln "[" i "] " _parseTree[i])
		) ; end loop
		(writeln "*********Source near Failure***********")
		(writeln "Charpos=" _parseTree[_ip].Charpos)
		(writeln (substring $IN _parseTree[_ip].Charpos (+ _parseTree[_ip].Charpos 1000)))
		(writeln "*********Rules starting near Failure*****")
   			
   true)


   ;;*********************************************************************
   ;; Centralized print routine for logging errors to console. 
   ;;*********************************************************************
   (defun _logLine(lineArg show)
   		vars:(lineText)
   		(setq lineText (new Vector: byte: 30000))
   		(cond
   			((= show source:) (setq lineText (appendWriteln lineText (rept " " _indent) lineArg (_showSource 20))))
   			((= show input:) (setq lineText (appendWriteln lineText (rept " " _indent) lineArg (_showInput 20))))
   			((= show none:) (setq lineText (appendWriteln lineText (rept " " _indent) lineArg)))
		);cond   			
 		(writeln lineText)  
   true)

   ;;*********************************************************************
   ;; Finds the line number given a character position
   ;;*********************************************************************
   (defun _findLineNum(pos)
      vars: (i j l)
      (setq j 0)
      (setq l (length $IN))
      (if (< pos l) (setq l pos))
      (loop for i from 0 until l step 1 do
         (if (= $IN[i] 10) (setq j (iadd j 1)))
         ); end loop
      j; return number of linefeeds found
      ); end findLineNum

   ;;*********************************************************************
   ;; Construct Error
   ;;*********************************************************************
   (defun _makeError(errorKey pos desc)
      vars: (i j line1 line2 line3 line4 eof nontabs tabs temp result)

      (setq eof (length $IN))
      (setq nontabs 0)
      (setq tabs 0)


      ;find start of error line - line2
      (setq i pos)
      (while (and (>= i 0) (<> $IN[i] 10) (<> $IN[i] 13)) (setq i (isub i 1)))
      (if (> i 0) (setq line2 (iadd i 1)) (setq line2 0))

      ;Count number of tabs and non-tabs up to error in error line
      (setq j line2)
      (while (< j pos) (if (= $IN[j] 9) (setq tabs (iadd tabs 1)) (setq nontabs (iadd nontabs 1))) (setq j (iadd j 1)))

      ;find start of line1
      (while (and (>= i 0) (or (= $IN[i] 10) (= $IN[i] 13))) (setq i (isub i 1))) 
      (while (and (>= i 0) (<> $IN[i] 10) (<> $IN[i] 13)) (setq i (isub i 1)))
      (if (> i 0) (setq line1 (iadd i 1)) (setq line1 0))

      ;find start of line 3
      (setq i pos) ; reset to error position
      (while (and (< i eof) (<> $IN[i] 10) (<> $IN[i] 13)) (setq i (iadd i 1)))
      (while (and (< i eof) (or (= $IN[i] 10) (= $IN[i] 13))) (setq i (iadd i 1)))
       (setq line3 i)

      ;find start of line 4
      (while (and (< i eof) (<> $IN[i] 10) (<> $IN[i] 13)) (setq i (iadd i 1)))
      (while (and (< i eof) (or (= $IN[i] 10) (= $IN[i] 13))) (setq i (iadd i 1)))
      (setq line4 i)

      ;Create error window
      (setq temp (append 
           (if (> (- line2 line1) 0) (substring $IN line1 (isub line2 1)) "") ;line 1
           (if (> (- line3 line2) 0) (substring $IN line2 (isub line3 1)) "") ;line 2 
           (if (= line3 eof) _eol ""); add an _eol if line2 is the only line!
           (rept " " nontabs) (rept (string (char 9)) tabs) "^error" _eol 
           (if (> line4 line3) (substring $IN line3 (isub line4 1)) "")
           ))

      ; Normalize tabs to 4 chars each
      (setq j (length temp))
      (setq result "")
      (loop for i from 0 until j step 1 do
         (setq result (append result (if (= temp[i] 9) "    " (string temp[i]))))
         ); end loop

      ;insert values into the _lastError structure
      (setq _lastError.errorKey errorKey)
      (setq _lastError.line (_findLineNum pos))
      (setq _lastError.desc desc)
      (setq _lastError.charpos pos)
      (setq _lastError.message result)

      ;(_error errorKey (append " " desc " Line:" _lastError.line " Charpos:" pos _eol _lastError.message))
      (_error (append " " desc " Line:" _lastError.line " Char:" pos _eol _lastError.message))

      ); end _makeError

   ;; _error wraps the builtin error function so that it 
   ;; is possible to disable console errors for silent
   ;; operation. The _consoleErrors variable determines
   ;; the function of _error.
   (defun _error ( ... )
      vars:(argc i e)

      (setq argc (argCount))
      (setq e (new Vector: argc))
      (loop for i from 0 until argc do
         (setq e[i] (argFetch i))
         ); end loop

       (if _consoleErrors (apply error e) (error e[0]))

      false ; we will never actually get here but put in a return anyway
      ); of defun _error

   ;; Append multiple arguments into a list.
   ;; Note: This agent is here as a builtin function for
   ;;       use in the output section of any rule definition. 
   (defun appendList(one two ...)
       vars:(result argc i)
       (cond 
           ((and (isPair one) (isPair two)) (setq result (append one two)))
           ((= one #void) (setq result two))
           ((and (isPair one) (= two #void)) (setq result one))
           ((isPair one) (setq result (append one (list two))))
           ((= two #void) (setq result (list one)))
           (else (setq result (list one two)))
           ) ; end cond
       (setq argc (argCount))
       (loop for i from 2 until argc do
           (setq result (appendList result (argFetch i)))
           ) ;; end loop
       result) ;; end appendList
   ;; Apply the specified semantic rule to the result.
   ;; Note: This agent is here as a builtin function for
   ;;       use in the output section of any rule definition. 
   (defun _apply(theRule multiplePass)
       vars:(outList outString outExplain)
       ;(if _semanticVerbose (setq outString (append "Replacing: " (string _result true) " ==> ")))
       ;(if _explainOnOff (setq outExplain (setq outExplain (append "Replacing: " (string _result true) " ==> "))))
       (setq outList _result)
       (setq _passCount 0)
       (setq _semanticRule theRule)
       Retry::
       (if (> _passCount _maxPasses) (_error "parseAgent_Pass" "Exceeded maximum number of apply rules."))
       (setq _changeCount 0)
       (setq outList (morph (list outList) _applyRule morphFail))
       (if (isPair outList) (setq outList (car outList)))
       (if (and (> _changeCount 0) (= multiplePass true) (isPair outList)) (goto Retry:))
       ;(if (= _semanticVerbose true) (writeln outString  (string outList true)))
       ;(if _explainOnOff (setq _explanation (append _explanation outExplain (string _result true) _eol)))
       (setq _result outList)
       _result) ;; end of _apply
   ;; Apply the current semantic rule to a sub list
   ;; Note: This agent is called by morph for every sub list
   ;;       in the larger result list. 
   (defun _applyRule(sexp)
      vars:(ret outString outExplain)
      (setq _ip -1)
      (setq _io sexp)
      (if _semanticVerbose (setq outString (append "Replacing: " (string sexp true) " ==> ")))
      (if _explainOnOff (setq outExplain (setq outExplain (append "Replacing: " (string sexp true) " ==> "))))
      (setq _semanticStack (new Structure: _io _ip))
      (if (<> (setq ret (_semanticRule)) morphFail)
          (begin
             (++ _changeCount)
             (if (= _semanticVerbose true) (writeln outString  (string ret true)))
             (if _explainOnOff (setq _explanation (append _explanation outExplain (string ret true) _eol)))
             (return ret)
          )) ;; end if
      morphFail) ;; end of _applyRule
   ;; Modified default rule for adding attributes to a parsed token.
   ;; Note: This agent is here in case the user does not 
   ;;       define one of his/her own.
   (defun defaultTokenRule(token)
       vars:(result tokenLen tokenEnd)
       ;; Is this token a delimited constant?
       (if (isVector token) 
           (begin
              (setq result (new Structure: Value: token[1] token[0] true  Constant: true))
              (return result)
              )) ;; end if delimited constant
       ;; Is this token an integer constant?
       (if (isInteger token) 
           (begin
              (setq result (new Structure: Value: token  Integer: true  Number: true  Constant: true))
              (return result)
              )) ;; end if integer constant
       ;; Is this token an numeric constant?
       (if (isNumber token) 
           (begin
              (setq result (new Structure: Value: token  Number: true  Constant: true))
              (return result)
              )) ;; end if numeric constant
       ;; Is this token a name token?
       (if (isCharName token) 
           (begin
              (setq result (new Structure: Value: token  Name: true  Default: true))
              ;; Add the token to the directory so we don't have to do this again.
              (setq tokenDirectory[token] result) 
              (return result)
              )) ;; end if numeric constant
       ;; Create a default attributed structure for this token
       (setq result (new Structure: Value: token Default: true))
       ;; Add the token to the directory so we don't have to do this again.
       (setq tokenDirectory[token] result) 
       result) ;; end defaultTokenRule
   ;; Return true if the we are at the end of the parse tree.
   (defun _eofToken()
       (if (>= (_nextIp) (_lenIp)) true (_lastIp))) ;; end _eofToken
   ;; Manages any errors which may occur during compilation.
   (defun _errorHandler(errMsg)
      vars:(stemp n)
      (setq stemp (string errMsg true))
      (setq n (length stemp))
      (setq stemp (right (left stemp (- n 2)) (- n 4)))
      (error stemp)) ;; end of _errorHandler
   ;; Get the next token in the current parse tree.
   (defun _getToken()
       vars:(result i io n)
       ;; Load the next token in the parse tree.
       ;(if (>= (_nextIp) (_lenIp)) (begin (setq result morphFail) (_lastIp) (return result)))
       (if (>= (_nextIp) (_lenIp)) (return morphFail))
       (if (isNumber _ip) (return _io[_ip]))
       (setq n (subi (length _ip) 1))
       (setq io _io)
       (loop for i from 0 until n do
           (setq io io[_ip[i]])          
           ) ; end loop
       (setq result io[_ip[n]]) 
       result) ;; end of _getToken
   ;; Initialization routine for setting token dicrectory, etc.
   ;; Note: This agent is run once during the lifetime of the
   ;;       parent agent.
   (defun _Initialize()
       (setq _initializeSW true)
       ;; Reset the verbose mode indent.
       (setq _indent 0)
       ;; Create the token directory for the compiler definition language.
       (setq tokenDirectory (new Directory:))
       ;; Adjust the lexical analyzer for the compiler definition language.
       (defaultLexer._Initialize)






       ;; Initialization of Delimited Strings
         ;; Initialization of Lexical Features
       (setq _LF_Digit (_setLexicalFeature _LF_Digit #( 1 48 57)))
       (setq _LF_Alpha (_setLexicalFeature _LF_Alpha #( 1 97 122 1 65 90)))
       (setq _LF_AlphaNum (_setLexicalFeature _LF_AlphaNum #( 1 97 122 1 65 90 1 48 57)))
       (setq _LF_NameChar (_setLexicalFeature _LF_NameChar #( 1 97 122 1 65 90 1 48 57 1 95 95)))
       (setq _LF_Letter (_setLexicalFeature _LF_Letter #( 1 97 122 1 65 90)))
       (setq _LF_NameStart (_setLexicalFeature _LF_NameStart #( 1 97 122 1 65 90 1 95 95)))
       (setq _LF_Lt (_setLexicalFeature _LF_Lt #( 1 60 60)))
       (setq _LF_Bang (_setLexicalFeature _LF_Bang #( 1 33 33)))
       (setq _LF_Operator (_setLexicalFeature _LF_Operator #( 1 60 60 1 62 62 1 61 61 1 43 43 1 47 47 1 42 42 1 45 45)))
       (setq _LF_DQuote (_setLexicalFeature _LF_DQuote #( 1 34 34)))
       (setq _LF_NotDQuote (_setLexicalFeature _LF_NotDQuote #( 1 0 255 0 34 34)))
       (setq _LF_Quote (_setLexicalFeature _LF_Quote #( 1 39 39)))
       (setq _LF_NotQuote (_setLexicalFeature _LF_NotQuote #( 1 0 255 0 39 39)))
       (setq _LF_Whitespace (_setLexicalFeature _LF_Whitespace #( 1 0 32 1 46 46 1 33 33 1 39 39 1 96 96 1 63 63 1 44 44 1 58 58 1 59 59)))
       (setq _LF_Eol (_setLexicalFeature _LF_Eol #( 1 10 10 1 13 13)))
       (setq _LF_NotEol (_setLexicalFeature _LF_NotEol #( 1 0 255 0 10 10 0 13 13)))
       (setq _LF_Period (_setLexicalFeature _LF_Period #( 1 46 46)))
       (setq _LF_Exponent (_setLexicalFeature _LF_Exponent #( 1 101 101 1 69 69)))
       (setq _LF_Sign (_setLexicalFeature _LF_Sign #( 1 43 43 1 45 45)))
         ;; Initialization of Syntax Features
       (_setSyntaxFeature Boolean: #( |TRUE| |FALSE| |true| |false|) #(true false true false ))
       (_setSyntaxFeature Term: #( |TRUE| |FALSE| |true| |false|) #void)
       (_setSyntaxFeature LeftParen: #( "(") #void)
       (_setSyntaxFeature RightParen: #( ")") #void)
       (_setSyntaxFeature LeftBrace: #( "{") #void)
       (_setSyntaxFeature RightBrace: #( "}") #void)
       (_setSyntaxFeature LeftBracket: #( "[") #void)
       (_setSyntaxFeature RightBracket: #( "]") #void)
       (_setSyntaxFeature Percent: #( "%") #void)
       (_setWordFeatures #( |A| |AN|) #( |Word|))
       (_setWordFeatures #( |ABOUT|) #( |Word|))
       (_setWordFeatures #( |ABOVE|) #( |Word|))
       (_setWordFeatures #( |ACROSS|) #( |Word|))
       (_setWordFeatures #( |AFTER|) #( |Word|))
       (_setWordFeatures #( |AGAIN|) #( |Word|))
       (_setWordFeatures #( |AGAINST|) #( |Word|))
       (_setWordFeatures #( |ALICE|) #( |Noun| |Name| |Female|))
       (_setWordFeatures #( |ALL| |EACH| |EVERY| |EVERYBODY| |EVERYONE| |EVERYTHING|) #( |Word| |Qualifier| |Adjective|))
       (_setWordFeatures #( |ALONG|) #( |Word| |Adverb|))
       (_setWordFeatures #( |ALWAYS|) #( |Word| |adverb|))
       (_setWordFeatures #( |AM| |ARE| |BE| |BEEN| #( |EQUAL| |Noun| ) #( |EQUALS| |Noun| ) |IS| |WAS| |WERE|) #( |Word| |Verb| |Assignment|))
       (_setWordFeatures #( |AMID|) #( |Word|))
       (_setWordFeatures #( |AMONG|) #( |Word|))
       (_setWordFeatures #( |AN| |A|) #( |Word|))
       (_setWordFeatures #( |AND|) #( |Word|))
       (_setWordFeatures #( |ANOTHER|) #( |Word| |Adjective|))
       (_setWordFeatures #( |ANY| |ANYBODY| |ANYONE| |ANYTHING| #( |ONE| #( |NumValue| 1 ) ) |ONCE| |SOME| |SOMEBODY| |SOMEONE|) #( |Word| |Noun| |Qualifier|))
       (_setWordFeatures #( |ANYBODY| |ANYONE| |ANYTHING| #( |ONE| #( |NumValue| 1 ) ) |ONCE| |SOME| |SOMEBODY| |SOMEONE| |ANY|) #( |Word| |Noun| |Qualifier|))
       (_setWordFeatures #( |ANYONE| |ANYTHING| #( |ONE| #( |NumValue| 1 ) ) |ONCE| |SOME| |SOMEBODY| |SOMEONE| |ANY| |ANYBODY|) #( |Word| |Noun| |Qualifier|))
       (_setWordFeatures #( |ANYTHING| #( |ONE| #( |NumValue| 1 ) ) |ONCE| |SOME| |SOMEBODY| |SOMEONE| |ANY| |ANYBODY| |ANYONE|) #( |Word| |Noun| |Qualifier|))
       (_setWordFeatures #( |ARE| |BE| |BEEN| #( |EQUAL| |Noun| ) #( |EQUALS| |Noun| ) |IS| |WAS| |WERE| |AM|) #( |Word| |Verb| |Assignment|))
       (_setWordFeatures #( |AROUND|) #( |Word|))
       (_setWordFeatures #( |AS|) #( |Word|))
       (_setWordFeatures #( |ASIDE|) #( |Word|))
       (_setWordFeatures #( |ASK| |ASKED| |ASKS| #( |ASKING| |Noun| )) #( |Word| |Verb|))
       (_setWordFeatures #( |AT|) #( |Word|))
       (_setWordFeatures #( |AWAY|) #( |Word|))
       (_setWordFeatures #( |BE| |BEEN| #( |EQUAL| |Noun| ) #( |EQUALS| |Noun| ) |IS| |WAS| |WERE| |AM| |ARE|) #( |Word| |Verb| |Assignment|))
       (_setWordFeatures #( |BECAUSE|) #( |Word|))
       (_setWordFeatures #( |BEEN| #( |EQUAL| |Noun| ) #( |EQUALS| |Noun| ) |IS| |WAS| |WERE| |AM| |ARE| |BE|) #( |Word| |Verb| |Assignment|))
       (_setWordFeatures #( |BEFORE|) #( |Word|))
       (_setWordFeatures #( |BEHIND|) #( |Word|))
       (_setWordFeatures #( |BELOW|) #( |Word|))
       (_setWordFeatures #( |BENEATH|) #( |Word|))
       (_setWordFeatures #( |BESIDE|) #( |Word|))
       (_setWordFeatures #( |BEST|) #( |Word| |Noun|))
       (_setWordFeatures #( |BETTER|) #( |Word|))
       (_setWordFeatures #( |BETWEEN|) #( |Word|))
       (_setWordFeatures #( |BEYOND|) #( |Word|))
       (_setWordFeatures #( |BIG|) #( |Word|))
       (_setWordFeatures #( |BLACK|) #( |Word| |Color| |Noun|))
       (_setWordFeatures #( |BLUE|) #( |Word| |Color| |Noun|))
       (_setWordFeatures #( |BOTH|) #( |Word|))
       (_setWordFeatures #( |BRING| |BROUGHT| |BRUNG|) #( |Word| |Verb|))
       (_setWordFeatures #( |BROWN|) #( |Word| |Color| |Noun|))
       (_setWordFeatures #( |BUT|) #( |Word|))
       (_setWordFeatures #( #( |BUY| |Noun| ) #( |BUYS| |Noun| ) |BOUGHT| #( |BUYING| |Noun| )) #( |Word| |Verb|))
       (_setWordFeatures #( |BY|) #( |Word|))
       (_setWordFeatures #( |CALCULATE| |CALCULATES| |CALCULATED| #( |CALCULATING| |Noun| )) #( |Word| |Verb|))
       (_setWordFeatures #( #( |CALL| |Noun| ) |CALLS| |CALLED| #( |CALLING| |Noun| )) #( |Word| |Verb|))
       (_setWordFeatures #( |CAME| |WENT| |COME| #( |COMING| |Noun| ) |COMES|) #( |Word| |Verb|))
       (_setWordFeatures #( |COME| #( |COMING| |Noun| ) |COMES| |CAME| |WENT|) #( |Word| |Verb|))
       (_setWordFeatures #( |COMES| |CAME| |WENT| |COME| #( |COMING| |Noun| )) #( |Word| |Verb|))
       (_setWordFeatures #( #( |COMING| |Noun| ) |COMES| |CAME| |WENT| |COME|) #( |Word| |Verb|))
       (_setWordFeatures #( |COMPUTE| |COMPUTES| |COMPUTED| #( |COMPUTING| |Noun| )) #( |Word| |Verb|))
       (_setWordFeatures #( |COMPUTER|) #( |Word|))
       (_setWordFeatures #( #( |CAN| |Noun| ) |COULD|) #( |Word| |Verb|))
       (_setWordFeatures #( |CANNOT|) #( |Word| |Verb|))
       (_setWordFeatures #( |CARRY| |CARRIED| |CARRIES| #( |CARRYING| |Noun| )) #( |Word| |Verb|))
       (_setWordFeatures #( |CLEAN| |CLEANED| |CLEANS| #( |CLEANING| |Noun| )) #( |Word| |Verb|))
       (_setWordFeatures #( |COLD|) #( |Word| |Noun|))
       (_setWordFeatures #( |COME| |COMES| |CAME| #( |COMING| |Noun| ) |WENT|) #( |Word| |Verb|))
       (_setWordFeatures #( |COULD| #( |CAN| |Noun| )) #( |Word| |Verb|))
       (_setWordFeatures #( #( |CUT| |Noun| ) |CUTS|) #( |Word| |Verb|))
       (_setWordFeatures #( |DESPITE|) #( |Word|))
       (_setWordFeatures #( |DID| |DONE| #( |DOING| |Noun| ) |DO| |DOES|) #( |Word| |Verb|))
       (_setWordFeatures #( #( |DISPLAY| |Noun| ) #( |DISPLAYS| |Noun| ) |DISPLAYED| #( |DISPLAYING| |Noun| )) #( |Word| |Verb|))
       (_setWordFeatures #( |DO| |DOES| |DID| |DONE| #( |DOING| |Noun| )) #( |Word| |Verb|))
       (_setWordFeatures #( |DOES| |DID| |DONE| #( |DOING| |Noun| ) |DO|) #( |Word| |Verb|))
       (_setWordFeatures #( #( |DOING| |Noun| ) |DO| |DOES| |DID| |DONE|) #( |Word| |Verb|))
       (_setWordFeatures #( |DONE| #( |DOING| |Noun| ) |DO| |DOES| |DID|) #( |Word| |Verb|))
       (_setWordFeatures #( |DONOT|) #( |Word| |Verb|))
       (_setWordFeatures #( |DOWN|) #( |Word| |Adverb|))
       (_setWordFeatures #( |DRAW| |DRAWS| |DRAWN| #( |DRAWING| |Noun| )) #( |Word| |Verb|))
       (_setWordFeatures #( #( |DRINK| |Noun| ) |DRINKS| |DRANK| #( |DRUNK| |Noun| ) #( |DRINKING| |Noun| )) #( |Word| |Verb|))
       (_setWordFeatures #( |DURING|) #( |Word|))
       (_setWordFeatures #( |EACH| |EVERY| |EVERYBODY| |EVERYONE| |EVERYTHING| |ALL|) #( |Word| |Noun| |Qualifier|))
       (_setWordFeatures #( |EARN| |EARNS| |EARNED| #( |EARNING| |Noun| |Adjective| ) #( |EARNINGS| |Noun| |Adjective| )) #( |Word| |Verb|))
       (_setWordFeatures #( |EAT| |ATE| |EATEN| |EATS| #( |EATING| |Noun| )) #( |Word| |Verb|))
       (_setWordFeatures #( |EIGHT|) #( |Word| #( |NumValue| 8 )))
       (_setWordFeatures #( #( |EQUAL| |Noun| ) #( |EQUALS| |Noun| ) |IS| |WAS| |WERE| |AM| |ARE| |BE| |BEEN|) #( |Word| |Verb| |Assignment|))
       (_setWordFeatures #( #( |EQUALS| |Noun| ) |IS| |WAS| |WERE| |AM| |ARE| |BE| |BEEN| #( |EQUAL| |Noun| )) #( |Word| |Verb| |Assignment|))
       (_setWordFeatures #( |EQUATION| |EQUATIONS| |FORMULA| |FORMULAS|) #( |Word| |Noun|))
       (_setWordFeatures #( |EVALUATE| |EVALUATES| |EVALUATED| #( |EVALUATING| |Noun| )) #( |Word| |Verb| |Compute|))
       (_setWordFeatures #( |EVERY| |EVERYBODY| |EVERYONE| |EVERYTHING| |ALL| |EACH|) #( |Word| |Qualifier|))
       (_setWordFeatures #( |EVERYBODY| |EVERYONE| |EVERYTHING| |ALL| |EACH| |EVERY|) #( |Word| |Qualifier|))
       (_setWordFeatures #( |EVERYONE| |EVERYTHING| |ALL| |EACH| |EVERY| |EVERYBODY|) #( |Word| |Qualifier|))
       (_setWordFeatures #( |EVERYTHING| |ALL| |EACH| |EVERY| |EVERYBODY| |EVERYONE|) #( |Word| |Qualifier|))
       (_setWordFeatures #( |EXCEPT|) #( |Word|))
       (_setWordFeatures #( #( |FALL| |Noun| ) |FALLS| |FALLEN| |FELL| #( |FALLING| |Noun| )) #( |Word| |Verb|))
       (_setWordFeatures #( |FAR|) #( |Word| |Adjective|))
       (_setWordFeatures #( |FAST| |FASTER| |FASTEST|) #( |Word| |Adjective|))
       (_setWordFeatures #( |FEW| |FEWER| |FEWEST|) #( |Word| |Adjective|))
       (_setWordFeatures #( #( |FIND| |Noun| ) |FINDS| |FOUND| #( |FINDING| |Noun| )) #( |Word| |Verb|))
       (_setWordFeatures #( |FIRST|) #( |Word| |Adjective| #( |NumValue| 1 )))
       (_setWordFeatures #( |FIVE|) #( |Word| |Noun| #( |NumValue| 5 )))
       (_setWordFeatures #( #( |FLY| |Noun| ) #( |FLYS| |Noun| ) |FLEW| |FLOWN| #( |FLYING| |Noun| )) #( |Word| |Verb|))
       (_setWordFeatures #( |FOR|) #( |Word|))
       (_setWordFeatures #( |FORMULA| |FORMULAS| |EQUATION| |EQUATIONS|) #( |Word| |Noun|))
       (_setWordFeatures #( |FOUR|) #( |Word| |Noun| #( |NumValue| 4 )))
       (_setWordFeatures #( |FROM|) #( |Word|))
       (_setWordFeatures #( |FULL| |FULLER| |FULLEST|) #( |Word| |Adjective|))
       (_setWordFeatures #( |FUNNY| |FUNNIER| |FUNNIEST|) #( |Word| |Adjective|))
       (_setWordFeatures #( |GET| |GETS| |GOT| |GOTTEN| |GETTING|) #( |Word| |Verb|))
       (_setWordFeatures #( |GIVE| |GIVES| |GIVEN| #( |GIVING| |Noun| ) |GAVE|) #( |Word| |Verb|))
       (_setWordFeatures #( |GO| |GOES| |GONE| #( |GOING| |Noun| ) |WENT|) #( |Word| |Verb|))
       (_setWordFeatures #( |GOOD|) #( |Word| |Adjective| |Noun|))
       (_setWordFeatures #( |GREEN|) #( |Word| |Noun| |Color|))
       (_setWordFeatures #( |GROW| |GROWS| |GREW| |GROWN| #( |GROWING| |Noun| )) #( |Word| |Verb|))
       (_setWordFeatures #( |HAVE| |HAS| |HAD| #( |HAVING| |Noun| )) #( |Word| |Verb|))
       (_setWordFeatures #( |HE| |HIM| |HIMSELF| |HIS|) #( |Word|))
       (_setWordFeatures #( #( |HELP| |Noun| ) |HELPS| |HELPED| #( |HELPFUL| |Adjective| )) #( |Word| |Verb|))
       (_setWordFeatures #( |HER| |HERS| |HERSELF| |SHE|) #( |Word|))
       (_setWordFeatures #( |HERE|) #( |Word|))
       (_setWordFeatures #( |HERS| |HERSELF| |SHE| |HER|) #( |Word|))
       (_setWordFeatures #( |HERSELF| |SHE| |HER| |HERS|) #( |Word|))
       (_setWordFeatures #( |HIM| |HIMSELF| |HIS| |HE|) #( |Word|))
       (_setWordFeatures #( |HIMSELF| |HIS| |HE| |HIM|) #( |Word|))
       (_setWordFeatures #( |HIS| |HE| |HIM| |HIMSELF|) #( |Word|))
       (_setWordFeatures #( |HISTORY| |HISTORIES|) #( |Word| |Noun|))
       (_setWordFeatures #( #( |HOLD| |Noun| ) |HOLDS| |HELD| #( |HOLDING| |Noun| )) #( |Word| |Verb|))
       (_setWordFeatures #( |HOT|) #( |Word|))
       (_setWordFeatures #( |HOW| |HOWEVER|) #( |Word|))
       (_setWordFeatures #( |HOWEVER| |HOW|) #( |Word|))
       (_setWordFeatures #( #( |HURT| |Noun| ) |HURTS| #( |HURTING| |Noun| )) #( |Word| |Verb|))
       (_setWordFeatures #( |I| |ME| |MINE| |MY| |MYSELF| |OUR| |OURS| |OURSELF| |OURSELVES| |US| |WE|) #( |Word|))
       (_setWordFeatures #( |IF|) #( |Word|))
       (_setWordFeatures #( |IN|) #( |Word|))
       (_setWordFeatures #( |INSIDE|) #( |Word|))
       (_setWordFeatures #( |INTO|) #( |Word|))
       (_setWordFeatures #( |IS| |WAS| |WERE| |AM| |ARE| |BE| |BEEN| #( |EQUAL| |Noun| ) #( |EQUALS| |Noun| )) #( |Word| |Verb| |Assignment|))
       (_setWordFeatures #( |IT| |ITS| |ITSELF|) #( |Word|))
       (_setWordFeatures #( |ITS| |ITSELF| |IT|) #( |Word|))
       (_setWordFeatures #( |ITSELF| |IT| |ITS|) #( |Word|))
       (_setWordFeatures #( #( |JUMP| |Noun| ) |JUMPS| |JUMPED| #( |JUMPING| |Noun| )) #( |Word| |Verb|))
       (_setWordFeatures #( |JUST| |JUSTICE|) #( |Word| |Noun|))
       (_setWordFeatures #( #( |KEEP| |Noun| ) |KEEPS| |KEPT| #( |KEEPING| |Noun| )) #( |Word| |Verb|))
       (_setWordFeatures #( #( |KIND| |Noun| ) |KINDLY|) #( |Word| |Adjective|))
       (_setWordFeatures #( |KNOW| |KNOWS| |KNEW| #( |KNOWING| |Noun| )) #( |Word| |Verb|))
       (_setWordFeatures #( #( |LAUGH| |Noun| ) |LAUGHS| |LAUGHED| #( |LAUGHING| |Noun| )) #( |Word| |Verb|))
       (_setWordFeatures #( |LET| |LETS| #( |LETTING| |Noun| )) #( |Word| |Verb|))
       (_setWordFeatures #( #( |LIGHT| |Noun| |Adjective| ) #( |LIGHTS| |Noun| ) |LIGHTED| |LIT| #( |LIGHTING| |Noun| )) #( |Word| |Verb|))
       (_setWordFeatures #( |LIKE| |LIKES| |LIKED| #( |LIKING| |Noun| )) #( |Word| |Verb|))
       (_setWordFeatures #( |LITTLE|) #( |Word| |Adjective|))
       (_setWordFeatures #( #( |LIFE| |Noun| ) |LIVE| |LIVES| |LIVED| #( |LIVING| |Noun| )) #( |Word| |Verb|))
       (_setWordFeatures #( |LONG|) #( |Word|))
       (_setWordFeatures #( #( |LOOK| |Noun| ) #( |LOOKS| |Noun| ) |LOOKED| #( |LOOKING| |Noun| )) #( |Word| |Verb|))
       (_setWordFeatures #( #( |MAKE| |Noun| ) |MADE| |MAKES| #( |MAKING| |Noun| )) #( |Word| |Verb|))
       (_setWordFeatures #( |MANY| |MUCH|) #( |Word| |Adjective|))
       (_setWordFeatures #( |MATH| |MATHEMATICS|) #( |Word| |Noun|))
       (_setWordFeatures #( |MAY| |MIGHT|) #( |Word| |Verb|))
       (_setWordFeatures #( |ME| |MINE| |MY| |MYSELF| |OUR| |OURS| |OURSELF| |OURSELVES| |US| |WE| |I|) #( |Word|))
       (_setWordFeatures #( |MINE| |MY| |MYSELF| |OUR| |OURS| |OURSELF| |OURSELVES| |US| |WE| |I| |ME|) #( |Word|))
       (_setWordFeatures #( |MUCH| |MANY|) #( |Word| |Adjective|))
       (_setWordFeatures #( |MUST|) #( |Word|))
       (_setWordFeatures #( |MY| |MYSELF| |OUR| |OURS| |OURSELF| |OURSELVES| |US| |WE| |I| |ME| |MINE|) #( |Word|))
       (_setWordFeatures #( |MYSELF| |OUR| |OURS| |OURSELF| |OURSELVES| |US| |WE| |I| |ME| |MINE| |MY|) #( |Word|))
       (_setWordFeatures #( |NEAR| |NEARER| |NEAREST|) #( |Word|))
       (_setWordFeatures #( |NEITHER|) #( |Word|))
       (_setWordFeatures #( |NEVER|) #( |Word|))
       (_setWordFeatures #( |NEW| |NEWER| |NEWEST|) #( |Word|))
       (_setWordFeatures #( |NEXT|) #( |Word|))
       (_setWordFeatures #( |NO|) #( |Word|))
       (_setWordFeatures #( |NOBODY| |NONE| |NONONE|) #( |Word|))
       (_setWordFeatures #( |NONE| |NONONE| |NOBODY|) #( |Word|))
       (_setWordFeatures #( |NONONE| |NOBODY| |NONE|) #( |Word|))
       (_setWordFeatures #( |NOR|) #( |Word|))
       (_setWordFeatures #( |NOT|) #( |Word|))
       (_setWordFeatures #( |NOW|) #( |Word|))
       (_setWordFeatures #( |OF|) #( |Word|))
       (_setWordFeatures #( |OFF|) #( |Word|))
       (_setWordFeatures #( |OLD| |OLDER| |OLDEST|) #( |Word|))
       (_setWordFeatures #( |ON|) #( |Word|))
       (_setWordFeatures #( |ONCE| |SOME| |SOMEBODY| |SOMEONE| |ANY| |ANYBODY| |ANYONE| |ANYTHING| #( |ONE| #( |NumValue| 1 ) )) #( |Word| |Noun| |Qualifier|))
       (_setWordFeatures #( #( |ONE| #( |NumValue| 1 ) ) |ONCE| |SOME| |SOMEBODY| |SOMEONE| |ANY| |ANYBODY| |ANYONE| |ANYTHING|) #( |Word| |Noun| |Qualifier|))
       (_setWordFeatures #( |ONLY|) #( |Word|))
       (_setWordFeatures #( |ONTO|) #( |Word|))
       (_setWordFeatures #( |OPEN| |OPENS| |OPENED| #( |OPENING| |Noun| )) #( |Word| |Verb|))
       (_setWordFeatures #( |OPTION| |OPTIONS|) #( |Word| |Noun|))
       (_setWordFeatures #( |OR|) #( |Word|))
       (_setWordFeatures #( |OUR| |OURS| |OURSELF| |OURSELVES| |US| |WE| |I| |ME| |MINE| |MY| |MYSELF|) #( |Word|))
       (_setWordFeatures #( |OURS| |OURSELF| |OURSELVES| |US| |WE| |I| |ME| |MINE| |MY| |MYSELF| |OUR|) #( |Word|))
       (_setWordFeatures #( |OURSELF| |OURSELVES| |US| |WE| |I| |ME| |MINE| |MY| |MYSELF| |OUR| |OURS|) #( |Word|))
       (_setWordFeatures #( |OURSELVES| |US| |WE| |I| |ME| |MINE| |MY| |MYSELF| |OUR| |OURS| |OURSELF|) #( |Word|))
       (_setWordFeatures #( |OUT| |OUTSIDE|) #( |Word| |Adjective|))
       (_setWordFeatures #( |OUTSIDE| |OUT|) #( |Word| |Adjective|))
       (_setWordFeatures #( |OVER|) #( |Word|))
       (_setWordFeatures #( |OWN| |OWNS| |OWNED| #( |OWNING| |Noun| )) #( |Word| |Verb|))
       (_setWordFeatures #( |PAST|) #( |Word|))
       (_setWordFeatures #( #( |PICK| |Noun| ) #( |PICKS| |Noun| ) |PICKED| #( |PICKING| |Noun| )) #( |Word| |Verb|))
       (_setWordFeatures #( #( |PLAY| |Noun| ) #( |PLAYS| |Noun| ) |PLAYED| #( |PLAYING| |Noun| )) #( |Word| |Verb|))
       (_setWordFeatures #( |PLEASE| |PLEASES| |PLEASED| #( |PLEASING| |Noun| )) #( |Word| |Verb|))
       (_setWordFeatures #( |PRETTY|) #( |Word| |Adjective|))
       (_setWordFeatures #( #( |PRINT| |Noun| ) #( |PRINTS| |Noun| ) |PRINTED| #( |PRINTING| |Noun| )) #( |Word| |Verb|))
       (_setWordFeatures #( |PULL| |PULLS| |PULLED| #( |PULLING| |Noun| )) #( |Word| |Verb|))
       (_setWordFeatures #( |PUT| |PUTS| #( |PUTTING| |Noun| )) #( |Word| |Verb|))
       (_setWordFeatures #( |QUARTER| |QUARTERS| #( |QUARTERLY| |Adjective| )) #( |Word| |Noun|))
       (_setWordFeatures #( |READ| #( |READING| |Noun| )) #( |Word| |Verb|))
       (_setWordFeatures #( |RECOMMENDATION| |RECOMMENDATIONS| |PICK| |PICKS|) #( |Word| |Noun|))
       (_setWordFeatures #( |RED|) #( |Word| |Noun| |Color|))
       (_setWordFeatures #( |REPORT| |REPORTS|) #( |Word| |Noun|))
       (_setWordFeatures #( |RETRIEVE| |RETRIEVES| |RETRIEVED| |RETRIEVING|) #( |Word| |Verb|))
       (_setWordFeatures #( #( |RIDE| |Noun| ) |RIDES| |RIDDEN| #( |RIDING| |Noun| )) #( |Word| |Verb|))
       (_setWordFeatures #( |RIGHT| |RIGHTS|) #( |Word| |Noun|))
       (_setWordFeatures #( |ROUND|) #( |Word| |Adjective|))
       (_setWordFeatures #( #( |RUN| |Noun| ) |RAN| #( |RUNNING| |Noun| )) #( |Word| |Verb|))
       (_setWordFeatures #( |SALE| |SALES|) #( |Word| |Noun|))
       (_setWordFeatures #( |SAW|) #( |Word|))
       (_setWordFeatures #( |SAY| |SAYS| |SAID| #( |SAYING| |Noun| )) #( |Word| |Verb|))
       (_setWordFeatures #( |SEE| |SEES| |SAW| #( |SEEING| |Noun| )) #( |Word| |Verb|))
       (_setWordFeatures #( |SEVEN|) #( |Word| #( |NumValue| 7 )))
       (_setWordFeatures #( |SEVERAL|) #( |Word|))
       (_setWordFeatures #( |SHALL| |SHOULD|) #( |Word| |Verb|))
       (_setWordFeatures #( |SHE| |HER| |HERS| |HERSELF|) #( |Word|))
       (_setWordFeatures #( |SHOW| |SHOWS| |SHOWED| |SHOWN| #( |SHOWING| |Noun| )) #( |Word| |Verb|))
       (_setWordFeatures #( |SINCE|) #( |Word|))
       (_setWordFeatures #( |SING| |SINGS| |SANG| #( |SINGING| |Noun| )) #( |Word| |Verb|))
       (_setWordFeatures #( |SIT| |SITS| |SAT| #( |SITTING| |Noun| )) #( |Word| |Verb|))
       (_setWordFeatures #( |SIX|) #( |Word| #( |NumValue| 6 )))
       (_setWordFeatures #( #( |SLEEP| |Noun| ) |SLEEPS| |SLEPT| #( |SLEEPING| |Noun| )) #( |Word| |Verb|))
       (_setWordFeatures #( |SMALL|) #( |Word|))
       (_setWordFeatures #( |SO|) #( |Word|))
       (_setWordFeatures #( |SOME| |SOMEBODY| |SOMEONE| |ANY| |ANYBODY| |ANYONE| |ANYTHING| #( |ONE| #( |NumValue| 1 ) ) |ONCE|) #( |Word| |Noun| |Qualifier|))
       (_setWordFeatures #( |SOMEBODY| |SOMEONE| |ANY| |ANYBODY| |ANYONE| |ANYTHING| #( |ONE| #( |NumValue| 1 ) ) |ONCE| |SOME|) #( |Word| |Noun| |Qualifier|))
       (_setWordFeatures #( |SOMEONE| |ANY| |ANYBODY| |ANYONE| |ANYTHING| #( |ONE| #( |NumValue| 1 ) ) |ONCE| |SOME| |SOMEBODY|) #( |Word| |Noun| |Qualifier|))
       (_setWordFeatures #( |SOON|) #( |Word|))
       (_setWordFeatures #( #( |START| |Noun| ) |STARTS| |STARTED| #( |STARTING| |Noun| )) #( |Word| |Verb|))
       (_setWordFeatures #( |STOCK| |STOCKS| |SECURITY| |SECURITIES| |PORTFOLIO| |PORTFOLIOS|) #( |Word| |Noun|))
       (_setWordFeatures #( #( |STOP| |Noun| ) |STOPS| |STOPPED| #( |STOPPING| |Noun| )) #( |Word| |Verb|))
       (_setWordFeatures #( |SUMMARY| |SUMMARIES|) #( |Word| |Noun|))
       (_setWordFeatures #( #( |TAKE| |Noun| ) |TAKES| |TOOK| #( |TAKING| |Noun| )) #( |Word| |Verb|))
       (_setWordFeatures #( |TELL| |TELLS| |TOLD| #( |TELLING| |Noun| )) #( |Word| |Verb|))
       (_setWordFeatures #( |TEN|) #( |Word| #( |NumValue| 10 )))
       (_setWordFeatures #( |THANK| #( |THANKS| |Noun| ) |THANKED| #( |THANKING| |Noun| )) #( |Word| |Verb|))
       (_setWordFeatures #( |THE| |THAT| |THIS| |THOSE| |THESE|) #( |Word| |Article|))
       (_setWordFeatures #( |THEIR| |THEIRS|) #( |Word| |Adjective|))
       (_setWordFeatures #( |THEM| |THEMSELVES| |THEY|) #( |Word|))
       (_setWordFeatures #( |THEN|) #( |Word|))
       (_setWordFeatures #( |THERE|) #( |Word|))
       (_setWordFeatures #( |THINK| #( |THOUGHT| |Noun| ) |THINKS| #( |THINKING| |Noun| )) #( |Word| |Verb|))
       (_setWordFeatures #( |TRADER| |TRADERS|) #( |Word| |Noun|))
       (_setWordFeatures #( |THREE|) #( |Word| #( |NumValue| 3 )))
       (_setWordFeatures #( |THROUGH| |THROUGHOUT|) #( |Word|))
       (_setWordFeatures #( |TILL| |TILLS| |TILLED| #( |TILLING| |Noun| )) #( |Word| |Verb|))
       (_setWordFeatures #( |TO|) #( |Word|))
       (_setWordFeatures #( |TODAY|) #( |Word|))
       (_setWordFeatures #( |TOGETHER|) #( |Word|))
       (_setWordFeatures #( |TOO|) #( |Word|))
       (_setWordFeatures #( |TOWARD| |TOWARDS|) #( |Word|))
       (_setWordFeatures #( |TRY| |TRYS| |TRYED| #( |TRYING| |Noun| )) #( |Word| |Verb|))
       (_setWordFeatures #( |TWO|) #( |Word| #( |NumValue| 2 )))
       (_setWordFeatures #( |UNDER| |UNDERNEATH|) #( |Word|))
       (_setWordFeatures #( |UNLIKE|) #( |Word|))
       (_setWordFeatures #( |UNTIL|) #( |Word|))
       (_setWordFeatures #( |UP|) #( |Word|))
       (_setWordFeatures #( |UPON|) #( |Word|))
       (_setWordFeatures #( |US| |WE| |I| |ME| |MINE| |MY| |MYSELF| |OUR| |OURS| |OURSELF| |OURSELVES|) #( |Word|))
       (_setWordFeatures #( |USE|) #( |Word|))
       (_setWordFeatures #( |VERY|) #( |Word|))
       (_setWordFeatures #( #( |WALK| |Noun| ) #( |WALKS| |Noun| ) |WALKED| #( |WALKING| |Noun| )) #( |Word| |Verb|))
       (_setWordFeatures #( |WANT| |WANTS| |WANTED| #( |WANTING| |Noun| )) #( |Word| |Verb|))
       (_setWordFeatures #( |WARM| |WARMS| |WARMED| #( |WARMING| |Noun| )) #( |Word| |Verb|))
       (_setWordFeatures #( |WAS| |WERE| |AM| |ARE| |BE| |BEEN| #( |EQUAL| |Noun| ) #( |EQUALS| |Noun| ) |IS|) #( |Word| |Verb| |Assignment|))
       (_setWordFeatures #( #( |WASH| |Noun| ) |WASHES| |WASHED| #( |WASHING| |Noun| )) #( |Word| |Verb|))
       (_setWordFeatures #( |WE| |I| |ME| |MINE| |MY| |MYSELF| |OUR| |OURS| |OURSELF| |OURSELVES| |US|) #( |Word|))
       (_setWordFeatures #( |WEEK| |WEEKS| #( |WEEKLY| |Adjective| )) #( |Word| |Noun|))
       (_setWordFeatures #( |WELL|) #( |Word|))
       (_setWordFeatures #( |WENT| |COME| |COMES| #( |COMING| |Noun| ) |CAME| |GO| |GOES| |GONE| #( |GOING| |Noun| )) #( |Word| |Verb|))
       (_setWordFeatures #( |WERE| |AM| |ARE| |BE| |BEEN| #( |EQUAL| |Noun| ) #( |EQUALS| |Noun| ) |IS| |WAS|) #( |Word| |Verb| |Assignment|))
       (_setWordFeatures #( |WHAT|) #( |Word|))
       (_setWordFeatures #( |WHATEVER|) #( |Word|))
       (_setWordFeatures #( |WHEN|) #( |Word|))
       (_setWordFeatures #( |WHICH| |WHICHEVER|) #( |Word|))
       (_setWordFeatures #( |WHITE|) #( |Word| |Color| |Noun|))
       (_setWordFeatures #( |WHO| |WHOEVER| |WHOM| |WHOMEVER|) #( |Word|))
       (_setWordFeatures #( |WHY|) #( |Word| |Verb|))
       (_setWordFeatures #( |WILL| |WOULD|) #( |Word|))
       (_setWordFeatures #( #( |WISH| |Noun| ) |WISHES| |WISHED| #( |WISHING| |Noun| )) #( |Word| |Verb|))
       (_setWordFeatures #( |WITH| |WITHIN|) #( |Word|))
       (_setWordFeatures #( |WITHOUT|) #( |Word|))
       (_setWordFeatures #( #( |WORK| |Noun| ) #( |WORKS| |Noun| ) |WORKED| #( |WORKING| |Noun| )) #( |Word| |Verb|))
       (_setWordFeatures #( |WILL| |WOULD|) #( |Word|))
       (_setWordFeatures #( |WRITE|) #( |Word| |Color| |Noun|))
       (_setWordFeatures #( |YELLOW|) #( |Word| |Color| |Noun|))
       (_setWordFeatures #( |YES|) #( |Word|))
       (_setWordFeatures #( |YIELD| |YIELDS|) #( |Word| |Noun|))
       (_setWordFeatures #( |YOU| |YOUR| |YOURS| |YOURSELF| |YOURSELVES|) #( |Word|))
       (_setSyntaxFeature Adjective: #( |A| |AN| |THE| |THESE| |THIS| |THOSE|) #void)
       (_setSyntaxFeature Preposition: #( |ABOUT| |ABOVE| |ACROSS| |AFTER| |AGAINST| |ALONG| |AMID| |AMONG| |AROUND| |AS| |ASIDE| |AT|) #void)
       (_setSyntaxFeature Preposition: #( |BECAUSE| |BEFORE| |BEHIND| |BELOW| |BENEATH| |BESIDE| |BETWEEN| |BEYOND| |BUT| |BY| |DESPITE| |DOWN| |DURING|) #void)
       (_setSyntaxFeature Preposition: #( |EXCEPT| |FOR| |FROM| |IN| |INSIDE| |INTO| |LIKE| |NEAR| |NEXT| |OF| |OFF| |ON| |ONTO| |OUT| |OUTSIDE| |OVER| |PAST|) #void)
       (_setSyntaxFeature Preposition: #( |ROUND| |SINCE| |THROUGH| |THROUGHOUT| |TILL| |TO| |TOWARD| |TOWARDS| |UNDER| |UNDERNEATH| |UNLIKE| |UNTIL| |UP| |UPON| |WITH| |WITHIN| |WITHOUT|) #void)
       (_setSyntaxFeature Pronoun: #( |ALL| |AND| |ANOTHER| |ANY| |ANYBODY| |ANYONE| |ANYTHING| |EACH| |EVERYBODY| |EVERYONE| |EVERYTHING| |FEW|) #void)
       (_setSyntaxFeature Pronoun: #( |HE| |HER| |HERS| |HERSELF| |HIM| |HIMSELF| |HIS| |I| |IT| |ITS| |ITSELF| |MANY| |ME| |MINE| |MYSELF|) #void)
       (_setSyntaxFeature Pronoun: #( |NOBODY| |NONE| |ONE| |OUR| |OURS| |OURSELVES| |SEVERAL| |SHE| |SOME| |SOMEBODY| |SOMEONE| |THAT| |THEIRS|) #void)
       (_setSyntaxFeature Pronoun: #( |THEM| |THEMSELVES| |THESE| |THEY| |THIS| |THOSE| |US| |WE| |WHAT| |WHATEVER| |WHICH| |WHICHEVER| |WHO|) #void)
       (_setSyntaxFeature Pronoun: #( |WHOEVER| |WHOM| |WHOMEVER| |YOU| |YOURS| |YOURSELF| |YOURSELVES|) #void)
       (_setSyntaxFeature Article: #( |A| |AN| |THE| |THESE| |THIS| |THOSE|) #void)
       (_setSyntaxFeature Conjunction: #( |AND| |OR| |NOT| |HOWEVER| |BUT|) #void)
       ;; Call the user defined initialization routine
       (initRule)
       true) ;; end _Initialize
   ;; Default rule for user defined compiler initialization tasks.
   ;; Note: This agent is here in case the user does not 
   ;;       define one of his/her own.
   (defun initRule()
       true) ;; end initRule
   ;; Move the current parse tree index to the previous position.
   (defun _lastIp()
       (if (isNumber _ip) (return (setq _ip (subi _ip 1))))
       (setq _ip[(subi (length _ip) 1)] (subi _ip[(sub1 (length _ip))] 1))
       _ip[(subi (length _ip) 1)]) ;; end _lastIp
   ;; Return the length of the current parse.
   (defun _lenIp()
       vars:(i io n)
       (if (isNumber _ip) (return (length _io)))
       (setq n (sub1 (length _ip)))
       (setq io _io)
       (loop for i from 0 until n do
           (setq io io[_ip[i]])          
           ) ; end loop
       (length io)) ;; end _lenIp
   ;; Default main Lexical Rule for starting the compiler.
   ;; Note: This agent is here in case the user does not 
   ;;       define one of his/her own.
   (defun _LEXRULE_MAIN()
       (defaultLexer $IN)) ;; end _LEXRULE_MAIN
   ;; Move the current parse tree index to the next position.
   (defun _nextIp()
       (if (isNumber _ip) (return (setq _ip (addi _ip 1))))
       (setq _ip[(subi (length _ip) 1)] (addi _ip[(sub1 (length _ip))] 1))
       _ip[(subi (length _ip) 1)]) ;; end _nextIp
   ;; Default rule for returning the final output from the compiler.
   ;; Note: This agent is here in case the user does not 
   ;;       define one of his/her own.
   (defun outputRule(result)
       result) ;; end outputRule
   ;; Pop the current parse tree index up one level to the next position.
   (defun _popIp()
       (if (isNumber _ip) (return true))
       (if (<= (length _ip) 1) (begin (setq _ip _ip[0]) (return true)))
       (resize _ip (subi (length _ip) 1))
       true) ;; end _popIp
   ;; Default rule for any pre-lexical compiler operations.
   (defun preLexRule(input) input)
   ;; Push the current parse tree index down one level to the next position.
   (defun _pushIp()
       (if (isNumber _ip) (setq _ip (new Vector: 1 _ip)))
       (setq _ip[(length _ip)] -1)
       true) ;; end _pushIp
   ;; Assign a set of letters to the specified lexical feature
   (defun _setLexicalFeature(letterBitMap values)
       vars:(i j valueLen start end bit)
       (setq valueLen (length values))
       (if (= letterBitMap #void) (setq letterBitMap (new Vector: bit: 255)))
       (loop for i from 0 until valueLen by 3 do
          (setq bit values[i])
          (setq start (integer (min values[(+ i 1)] values[(+ i 2)])))
          (setq end (integer (max values[(+ i 1)] values[(+ i 2)])))
          (loop for j from start to end do
              (setq letterBitMap[j] bit)
              ) ; end j loop
           ) ; end i loop
       letterBitMap) ; end _setLexicalFeature
   ;; Set a whole class of syntax tokens with the specified attribute and values
   (defun _setSyntaxFeature(name words values)
       vars:(i wordLen token)
       (setq wordLen (length words))
       (loop for i from 0 until wordLen do
           (setq token tokenDirectory[words[i]])
           (if (= token #void) (setq token (new Structure:)))
           (if (= values[i] #void)
               (setq token[name] true)
               (setq token[name] values[i])
               ) ; end if
           (setq token.Value words[i])
           (setq tokenDirectory[words[i]] token)
           ) ; end loop
       true) ; end _setSyntaxFeature 
   ;; Set or enhance a whole word with the specified features and feature values
   ;; Note1: The word may be a single word or a vector starting with the word
   ;;        and proceeding with all of its gramatical synonyms. 
   ;;        For instance:   #(give gives gave given giving) 
   ;;        Each synonym may be a single word or a vector starting with the word
   ;;        and proceeding with all of its special features. 
   ;;        For instance:   #(give gives gave given #(giving Noun))
   ;; Note2: Each feature may be a single word or a vector starting with the feature
   ;;        and proceeding with the value of the feature. A singleton feature is
   ;;        assumed to have a value of true. 
   ;;        For instance:   Noun: Name: #(Color blue) 
   (defun _setWordFeatures(words commonFeatures)
       vars:(k K m M n N token feature featureName word extraFeatures)
       ;; If there are more than one grammatical synonym, then define each one.
       (if (not (isVector words)) (setq words (new Vector: 1 words)))
       (setq M (length words)) 
	   (setq N (length commonFeatures))
       ;; Extract the common feature name from the first word.
       ;; Note: Words with extra features are entered as Vectors
       (if (isVector words[0])
           then
           ;; The common word has extra features
           (setq featureName (downcase (makeString words[0][0])))
           else
           ;; The common word has no extra features
           (setq featureName (downcase (makeString words[0])))
           ) ; end if
       (setq featureName[0] (upcase featureName[0]))
       (setq featureName (symbol featureName))
       ;; Set all words to upper case.
       (loop for m from 0 until M do
           ;; Each word is stored upper case.
           ;; Note: Words with extra features are entered as Vectors
           (if (isVector words[m])
               then
               ;; This word has extra features
               (begin
                  (setq extraFeatures words[m])
                  (setq K (length extraFeatures))
                  (setq word (makeString extraFeatures[0]))
               ) ; end then
               else
               ;; This word has no extra features
               (begin
                  (setq extraFeatures #void)
                  (setq K 0)
                  (setq word (makeString words[m]))
               )) ; end if
	       (setq token tokenDirectory[word])
	       (if (= token #void) (setq token (new Structure:)))
	       (setq token.Value word)
           ;; Each word has its main word as a feature.
	       (setq token[featureName] true)
           ;; Each word has itself as a feature.
	       (setq feature (downcase (makeString word)))
	       (setq feature[0] (upcase word[0]))
	       (setq feature (symbol feature))
	       (setq token[feature] true)
	       ;; Add common features to this word.
	       (loop for n from 0 until N do
	           (setq feature commonFeatures[n])
	           (if (isVector feature) 
	               (setq token[feature[0]] feature[1])
	               (setq token[feature] true)
	               ) ; end if
	           ) ; end feature loop
	       ;; Add extra features to this word.
	       (loop for k from 1 until K do
	           (setq feature extraFeatures[k])
	           (if (isVector feature) 
	               (setq token[feature[0]] feature[1])
	               (setq token[feature] true)
	               ) ; end if
	           ) ; end feature loop
           (setq tokenDirectory[word] token)
	       ) ; end word loop
       true) ; end _setWordFeatures
   ;; Show a fragment of the input source string in lexical verbose mode
   (defun _showInput(size)
       vars:(sourceIndex sourceLen result)
       (setq sourceLen (length $IN))
       (setq sourceIndex (addi _ip 1))
       (if (< sourceIndex sourceLen)
           (setq result (mid $IN sourceIndex size))
           (setq result "...End Of File...")
           ) ; end if
       result) ; end _showInput
   ;; Show a fragment of the input source string in syntax verbose mode
   (defun _showSource(size)
       vars:(treeIndex treeLen result)
       (setq treeLen (length _parseTree))
       (setq treeIndex (addi _ip 1))
       (if (< treeIndex treeLen)
           (setq result (mid $IN _parseTree[treeIndex].Charpos size))
           (setq result "...End Of File...")
           ) ; end if
       result) ; end _showSource
   ;; Default rule for starting the compiler.
   ;; Note: This agent is here in case the user does not 
   ;;       define one of his/her own.
   (defun startRule()
       true) ;; end startRule
   ;; Default main Syntax Rule for starting the compiler.
   ;; Note: This agent is here in case the user does not 
   ;;       define one of his/her own.
   (defun _SYNRULE_MAIN()
       _parseTree) ;; end _SYNRULE_MAIN
   ;; Display the results of a rule firing in verbose mode.
   (defun writeRule(ruleID _ret t0 t1 t2 t3 t4 t5 t6 t7 t8 t9 t10)
   	   vars:(ruleText)
   	   (setq ruleText (new Vector: byte: 5000))
   	   (appendWriteln ruleText 
   	   		"Firing " 
   	   		ruleID
	       	(if (<> t1 #void)  (append "  " (rept " " _indent) "t1 = " (string t1 true) _eol) "")
	       	(if (<> t2 #void)  (append "  " (rept " " _indent) "t2 = " (string t2 true) _eol) "")
	       	(if (<> t3 #void)  (append "  " (rept " " _indent) "t3 = " (string t3 true) _eol) "")
	       	(if (<> t4 #void)  (append "  " (rept " " _indent) "t4 = " (string t4 true) _eol) "")
	       	(if (<> t5 #void)  (append "  " (rept " " _indent) "t5 = " (string t5 true) _eol) "")
	       	(if (<> t6 #void)  (append "  " (rept " " _indent) "t6 = " (string t6 true) _eol) "")
	       	(if (<> t7 #void)  (append "  " (rept " " _indent) "t7 = " (string t7 true) _eol) "")
	       	(if (<> t8 #void)  (append "  " (rept " " _indent) "t8 = " (string t8 true) _eol) "")
	       	(if (<> t9 #void)  (append "  " (rept " " _indent) "t9 = " (string t9 true) _eol) "")
	       	(if (<> t10 #void)  (append "  " (rept " " _indent) "t10 = "(string t10 true) _eol) "")
       		(append "  " (rept " " _indent) " ==> " (string _ret true)))
       (_logLine ruleText none:)
       true) ;; end writeRule
    ;; Output a feature based token to the token list (output as is).
    (defun _tkASIS(charpos value ...)
       vars:(parseTree treeIndex treeLen tokenAttr argc featureName featureValue argIndex)
       ;; Check the number of arguments for validity.
       (if (< (argCount) 2) (error "$ASIS must have at least two arguments"))
       (if (isOdd (argCount)) (error "$ASIS must have an even number of arguments"))
       ;; Make sure the token list is a vector.
       (if (= _tkLIST #void) (setq _tkLIST (new Vector: 0)))
       (setq $LIST _tkLIST)
       ;; Use these value as is. Do not use the syntax feature directory.
       ;; Note: Create an attributed token using the user supplied features.
       (setq tokenAttr (new Structure:))
       (setq tokenAttr.Value value)
       (setq tokenAttr.Charpos charpos)
       (loop for argIndex from 2 until (argCount) by 2 do
          (setq featureName (argFetch argIndex))
          (setq featureValue (argFetch (iadd argIndex 1)))
          (setq tokenAttr[(symbol featureName)] featureValue)
          ) ; end feature loop
       ;; Set the displacement of the token in the source string
       (setq _tkLIST[(length _tkLIST)] tokenAttr)
       tokenAttr) ;; end of _tkASIS
    ;; Output a feature based token to the token list.
    (defun _tkOUT(charpos value ...)
       vars:(parseTree treeIndex treeLen tokenAttr argc featureName featureValue argIndex)
       ;; Check the number of arguments for validity.
       (if (< (argCount) 2) (_error "$OUT must have at least two arguments"))
       (if (isOdd (argCount)) (_error "$OUT must have an even number of arguments"))
       ;; Make sure the token list is a vector.
       (if (= _tkLIST #void) (setq _tkLIST (new Vector: 0)))
       (setq $LIST _tkLIST)
       ;; Load any syntax features for this value.
       (setq tokenAttr tokenDirectory[value])
       (if (= tokenAttr #void)
           then
           ;; This value is not found in the syntax feature directory.
           ;; Note: Create an attributed token using the user supplied features.
           (begin
              (setq tokenAttr (new Structure:))
              (setq tokenAttr.Value value)
              (setq tokenAttr.Charpos charpos)
              (loop for argIndex from 2 until (argCount) by 2 do
                 (setq featureName (argFetch argIndex))
                 (setq featureValue (argFetch (iadd argIndex 1)))
                 (setq tokenAttr[(symbol featureName)] featureValue)
                 ) ; end feature loop
              ) ; end then
           else
           ;; This value is found in the syntax feature directory.
           ;; Note: Copy the features from the dictionary and set the value.
           (begin
              (setq tokenAttr (copy tokenAttr))
              (setq tokenAttr.Value value)
              (setq tokenAttr.Charpos charpos)
              ) ; end else
           ) ; end if
       ;; Set the displacement of the token in the source string
       (setq _tkLIST[(length _tkLIST)] tokenAttr)
       tokenAttr) ;; end of _tkOUT
   ;; ************************************************
   ;; Define the main entry code for this parent agent
   ;; ************************************************
   ;; ************************************************
   ;; Define the main entry code for this parent agent
   ;; ************************************************
   ;; Perform any pre-lexical work required before compilation
   (setq _verboseState _verbose)
   (setq _verboseTrigger -1)
   (setq _ruleCount 0)
   (setq _failurePass false)
   RESTART::	; come here if we have an error so we can turn _verbose on close to the failure
   (if (> _ruleCount 0) ; this means we are restarting after a failure
    	(begin
    	(setq _failurePass true)
   	    (cond
   	    ((= _failureIn lex:) ; error found in Lexical Pass
	   		(setq _verboseTrigger (max (- _ruleCount 15) 15))) ; show at least 15 rules before failure
	   	((= _failureIn syn:) ; error found in syntax pass
	   		(setq _verboseTrigger (max (- _ruleCount 15) _ruleCountSyn))) ; make sure we don't start _verbose in lexical analysis
	   	((= _failureIn sem:) ; error found in semantic pass
	   		(setq _verboseTrigger (max (- _ruleCount 15) _ruleCountSem))) ; make sure we dont start _verbose in syntax analysis
   		)));if

   (setq _ruleCount 0)
   (setq _verbose _verboseState)
   (setq _lastError (new Structure:)) ;; Clear _lastError 
   (onError _errorHandler)
   (setq _verboseLexCount (new Structure:)) ;Clear routine pass counts
   (setq _verboseSynCount (new Structure:))
   (setq _verboseSemCount (new Structure:))

   (setq _input (preLexRule _input))
   ;; Initialize the parent agent once and only once.
   (setq _indent 0)
   (setq _tkIN _input)
   (setq $IN _input)
   (setq $OUT _tkOUT)
   (setq $ASIS _tkASIS)
   (if (= _initializeSW #void) (_Initialize))
   (setq defaultLexer.defaultTokenRule defaultTokenRule)
   (setq defaultLexer.tokenDirectory tokenDirectory)
   
   ;; In verbose mode, display the source string contents
   (if _verbose (writeln _eol "************Input Source************" _eol $IN))
   ;; Run the user defined start rule.
   (startRule)


   ;; Run the Lexical Rules
   ;; Note: Create the attributed token stream parse tree.
   (setq _verboseHold _verbose)
   (setq _verbose (or _verboseHold _verboseLex))
   (if _verbose (writeln "*********Lexical Rule Firings*********"))
   (setq _tkLIST #void)
   (setq $LIST _tkLIST)
   (setq _ip -1)
   (setq _indent 0)
   (setq _parseTree (_LEXRULE_MAIN)) 
   (if (and (not _failurePass) (not _verbose)(= _parseTree |*failure*|:)) (begin (setq _failureIn lex:) (goto RESTART:)))
   ;; In verbose mode, display the parse tree contents
   (if _showTokens
      (begin
         (writeln "*********Lexed Tokens with Features*********")
         (loop for i from 0 until (length _parseTree) do
             (writeln "[" i "] " _parseTree[i])
             ) ; end loop
         )) ; end if
   (if (= _parseTree |*failure*|:) (goto END:))
   
    ;; Run the Syntax Rules
   (setq _verbose (or _verboseHold _verboseSyn))
   (if _verbose (writeln "*********Syntax Rule Firings*********"))
   (setq _ruleCountSyn _ruleCount)
   (setq _ip -1)
   (setq _io _parseTree)
   (setq _parseLen (length _parseTree))
   (setq _indent 0)
   (setq _result (_SYNRULE_MAIN))
   (if (and (not _failurePass) (not _verbose) (= _result |*failure*|:)) (begin (setq _failureIn syn:) (goto RESTART:)))
   (if (= _result |*failure*|:) (goto END:))
   
   ;; Run the Semantic Rules
   (if _verbose (writeln "*********Semantic Rule Firings*********"))
   (setq _ruleCountSem _ruleCount)
   (setq _verbose (or _verboseHold _verboseSem))
   (setq _explanation #void)
   
   (if _verbose (setq outString (append "Reducing:= " (string _result true) " ==> " )))
   (if _explainOnOff (setq outExplain (append "Reducing:= " (string _result true) " ==> " )))
   (setq _indent 0)

   ;; Perform semantic passes (if any)
   ;; Return the final output by invoking the user defined output rule.
   (if _semanticVerbose (writeln outString (string _result true)))
   (if _explainOnOff (setq _explanation (append _explanation outExplain (string _result true) _eol)))
   (setq _verbose verboseHold)
   
   (setq _result (outputRule _result))
   (setq _verbose _verboseState)   
   (if (and (not _failurePass) (not _verbose) (= _result |*failure*|:)) (begin (setq _failureIn sem:) (goto RESTART:)))
   (if _verbose (writeln "*********Final Output*********" _eol _result)) 
   
   END::
   
   _result) ;; end of compiler
















;;**EXPORTKEY**:alice:queryScript:%COMPILER_USERFUNCTIONS
;;*********************************************************************
;; Compile time child agents for the queryScript compiler.
;; Note: These child agents support the queryScript compilation process.
;;*********************************************************************

;;*********************************************************************
;; Function start pushes a new variable dictionary on the stack
;; Note: These persistant variables will extend the queryScript  environment
;;*********************************************************************
(defchild alice.queryScript startRule()
    true) ; end startRule

;;*********************************************************************
;; Function preLex converts all input to upper case
;;*********************************************************************
(defchild alice.queryScript preLexRule(input)
    vars:(aimlPatterns)
    input) ; end preLexRule

;;*********************************************************************
;; Manages any errors which may occur during parsing
;; Note: This overrides the standard error handler provided by parseAgent.
;;*********************************************************************
(defchild alice.queryScript _errorHandler(errMsg) (myParent.errorHandler errMsg))

;;*********************************************************************
;; Output a feature based token  (plus specified features) to the token list
;;*********************************************************************
(defchild alice.queryScript _tkOUTPLUS(charpos value ...)
   vars:(parseTree treeIndex treeLen tokenAttr argc featureName featureValue argIndex original)
   ;; Check the number of arguments for validity.
   (if (< (argCount) 2) (_error "$OUTPLUS must have at least two arguments"))
   (if (isOdd (argCount)) (_error "$OUTPLUS must have an even number of arguments"))
   ;; Make sure the token list is a vector.
   (if (= _tkLIST #void) (setq _tkLIST (new Vector: 0)))
   (setq $LIST _tkLIST)
   ;; Load any syntax features for this value.
   (setq original (copy value))
   (if (or (isString value) (isSymbol value)) (setq value (upcase (string value))))
   (setq tokenAttr tokenDirectory[value])
   (if (= tokenAttr #void)
       then
       ;; This value is not found in the syntax feature directory.
       ;; Note: Create an attributed token using the user supplied features.
       (begin
          (setq tokenAttr (new Structure:))
          (setq tokenAttr.Value value)
          (setq tokenAttr.Original original)
          (setq tokenAttr.Charpos charpos)
          ) ; end then
       else
       ;; This value is found in the syntax feature directory.
       ;; Note: Copy the features from the dictionary and set the value.
       (begin
          (setq tokenAttr (copy tokenAttr))
          (setq tokenAttr.Value value)
          (setq tokenAttr.Original original)
          (setq tokenAttr.Charpos charpos)
          ) ; end else
       ) ; end if
   ;; Add specified features to the lexcial output token.
   (loop for argIndex from 2 until (argCount) by 2 do
      (setq featureName (argFetch argIndex))
      (setq featureValue (argFetch (iadd argIndex 1)))
      (setq tokenAttr[(symbol featureName)] featureValue)
      ) ; end feature loop
   ;; Set the displacement of the token in the source string
   (setq _tkLIST[(length _tkLIST)] tokenAttr)
   tokenAttr) ;; end of _tkOUTPLUS








;;**EXPORTKEY**:alice:queryScript:%DEFINITION
;#text#
;; ********************************************************************
;; summary:  This queryScript compiler definition includes all the 
;;           features for the query agent compiler for the Alice
;;           chatterbot agent.
;;
;;           This agent query language Acts as a semi-intelligent user 
;;           interface agent which is able to answer user questions, 
;;           service user requests, and keep up a semi-intelligent 
;;           stream of chatter between itself and the user.
;;
;; Notes:    Requires the browseAgent, the parseAgent, and this compiler
;;           definition source must be checked into the file cabinet 
;;           under the key: |alice:queryScript:%DEFINITION|.
;; Parms:    none
;; return:   queryScript    The parseAgent checks in a newly generated 
;;                          copy of the queryScript source code.
;; ********************************************************************

;; *************************************************
;; Note: These lexical rules recognize standard
;;       contiguous words and standard numbers.
;;       English punctuation symbols are treated
;;       as white space. Every contiguous word
;;       is converted to upper case, is given
;;       a standard feature of Word = true.
;;       For instance the word "alice" is
;;       given a value of "ALICE", and a standard
;;       feature of Word = true.
;; *************************************************

#LexicalRules#

 MAIN: user ordering :: true ::    
 MAIN: DQuote NotDQuote* DQuote << ($ASIS $ch (string (append $1 $2 $3)) Original: (string (append $1 $2 $3)) String: true Constant: true) >>    
 MAIN: Quote NotQuote* Quote << ($ASIS $ch (string (append $1 $2 $3)) Original: (string (append $1 $2 $3)) Symbol: true Constant: true) >>    
 MAIN: Lt Bang "[CDATA[" CDATA << ($ASIS $ch (string (append $1 $2 $3 $4)) Original: (string (append $1 $2 $3 $4)) String: true Cdata: true Constant: true) >>    
 MAIN: NameStart NameChar* Quote NameChar+ << ($OUTPLUS $ch (string (append $1 $2 $4)) Word: true) >>    
 MAIN: NameStart NameChar* << ($OUTPLUS $ch (string (append $1 $2)) Word: true) >>    
 MAIN: Digit+ NameChar+ << ($OUTPLUS $ch (string (append $1 $2)) Word: true) >>    
 MAIN: Digit+ Period Digit* Exponent Sign Digit+ << ($OUTPLUS $ch (number (append $1 $2 $3 $4 $5 $6)) Number: true NumValue: (number (append $1 $2 $3 $4 $5 $6))) >>    
 MAIN: Digit+ Period Digit* << ($OUTPLUS $ch (number (append $1 $2 $3)) Number: true  NumValue: (number (append $1 $2 $3))) >>    
 MAIN: Digit+ << ($OUTPLUS $ch (integer $1) Number: true NumValue: (integer $1)) >>    
 MAIN: Period Digit+    << ($OUTPLUS $ch (number (append $1 $2)) Number: true NumValue: (number (append $1 $2))) >>    
 MAIN: Operator+        << ($OUTPLUS $ch $1 Operator: true Special: true) >>    
 MAIN: Whitespace+      << true >>    
 MAIN: Any              << ($OUTPLUS $ch (string $1) Special: true) >>    
 MAIN: Eof :: $LIST ::

 CDATA: user ordering :: true ::    
 CDATA: "]]>" :: (if (isString $0) (setq $0 (append $0 $1)) (setq $0 $1)) ::
 CDATA: Any << (if (isString $0) (setq $0 (append $0 $1)) (setq $0 $1)) >>
 CDATA: Eof :: (_makeError "ALICE 101" $ch "<![CDATA[ with out matching ]]>") ::

#End#

#LexicalFeatures#

Digit: [|"0"-"9"|]
Alpha: [|a-z| |A-Z|]
AlphaNum: [|a-z| |A-Z| |"0"-"9"|]
NameChar: [|a-z| |A-Z| |"0"-"9"| "_"]
Letter: [|a-z| |A-Z|]  
NameStart: [|a-z| |A-Z| "_"]  
Lt: [<]
Bang: [!]
Operator: [< > = + / * -]
DQuote: [34]
NotDQuote: [|0-255| ~ 34]
Quote: [39]
NotQuote: [|0-255| ~ 39]
Whitespace: [|0-32| "." "!" "'" "`" ? "," ":" ";"]
Eol: [10 13]
NotEol: [|0-255| ~ 10 13]
Period: ["."]
Exponent: [e E]
Sign: [+ -]

#End#

;; ************************************************
;; Note: These syntax rules recognize basic
;;       English commands. If the feature based
;;       rules cannot recognize the command,
;;       then it is passed on to the standard AIML
;;       chatterbot rules, which respond to the
;;       user command in their usual chatty manner.
;; ************************************************

#SyntaxRules#

  MAIN: user ordering :: true ::
  MAIN: Value     :: $LIST ::
  MAIN: Eof       :: $LIST ::
  
#End#

#SyntaxFeatures#

  ;; ********************************************
  ;; Operator Word List for the query language
  ;; Note: Standard English arithmetic components
  ;; ********************************************

	Boolean: [TRUE FALSE true false] [true false true false]
	Term: [TRUE FALSE true false]
	LeftParen: ["("]
	RightParen: [")"]
	LeftBrace: ["{"]
	RightBrace: ["}"]
	LeftBracket: ["["]
	RightBracket: ["]"]
	Percent: ["%"]

  ;; ******************************************
  ;; Basic English words for the query language
  ;; Note: Standard English language components
  ;;       required for Alice are defined here
  ;; ******************************************

	WORDS:[A AN] [Word]
	WORDS:[ABOUT 	] [Word]
	WORDS:[ABOVE 	] [Word]
	WORDS:[ACROSS 	] [Word]
	WORDS:[AFTER 	] [Word]
	WORDS:[AGAIN 	] [Word]
	WORDS:[AGAINST ] [Word]
    WORDS:[ALICE ] [Noun Name Female]
	WORDS:[ALL EACH EVERY EVERYBODY EVERYONE EVERYTHING] [Word Qualifier Adjective]
	WORDS:[ALONG 	] [Word Adverb]
	WORDS:[ALWAYS 	] [Word adverb]
	WORDS:[AM ARE BE BEEN [EQUAL Noun] [EQUALS Noun] IS WAS WERE] [Word Verb Assignment]
	WORDS:[AMID 	] [Word]
	WORDS:[AMONG 	] [Word]
	WORDS:[AN A] [Word]
	WORDS:[AND 	] [Word]
	WORDS:[ANOTHER 	] [Word Adjective]
	WORDS:[ANY ANYBODY ANYONE ANYTHING [ONE [NumValue 1]] ONCE SOME SOMEBODY SOMEONE] [Word Noun Qualifier]
	WORDS:[ANYBODY ANYONE ANYTHING [ONE [NumValue 1]] ONCE SOME SOMEBODY SOMEONE ANY] [Word Noun Qualifier]
	WORDS:[ANYONE ANYTHING [ONE [NumValue 1]] ONCE SOME SOMEBODY SOMEONE ANY ANYBODY] [Word Noun Qualifier]
	WORDS:[ANYTHING [ONE [NumValue 1]] ONCE SOME SOMEBODY SOMEONE ANY ANYBODY ANYONE] [Word Noun Qualifier]
	WORDS:[ARE BE BEEN [EQUAL Noun] [EQUALS Noun] IS WAS WERE AM] [Word Verb Assignment]
	WORDS:[AROUND 	] [Word]
	WORDS:[AS 	] [Word]
	WORDS:[ASIDE 	] [Word]
	WORDS:[ASK ASKED ASKS [ASKING Noun]] [Word Verb]
	WORDS:[AT 	] [Word]
	WORDS:[AWAY 	] [Word]
	WORDS:[BE BEEN [EQUAL Noun] [EQUALS Noun] IS WAS WERE AM ARE] [Word Verb Assignment]
	WORDS:[BECAUSE 	] [Word]
	WORDS:[BEEN [EQUAL Noun] [EQUALS Noun] IS WAS WERE AM ARE BE] [Word Verb Assignment]
	WORDS:[BEFORE 	] [Word]
	WORDS:[BEHIND 	] [Word]
	WORDS:[BELOW 	] [Word]
	WORDS:[BENEATH 	] [Word]
	WORDS:[BESIDE 	] [Word]
	WORDS:[BEST] [Word Noun]
	WORDS:[BETTER 	] [Word]
	WORDS:[BETWEEN 	] [Word]
	WORDS:[BEYOND 	] [Word]
	WORDS:[BIG 	] [Word]
	WORDS:[BLACK 	] [Word Color Noun]
	WORDS:[BLUE 	] [Word Color Noun]
	WORDS:[BOTH 	] [Word]
	WORDS:[BRING BROUGHT BRUNG] [Word Verb]
	WORDS:[BROWN 	] [Word Color Noun]
	WORDS:[BUT 	] [Word]
	WORDS:[[BUY Noun] [BUYS Noun] BOUGHT [BUYING Noun]] [Word Verb]
	WORDS:[BY 	] [Word]
	WORDS:[CALCULATE CALCULATES CALCULATED [CALCULATING Noun]] [Word Verb]
	WORDS:[[CALL Noun] CALLS CALLED [CALLING Noun]] [Word Verb]
	WORDS:[CAME WENT COME [COMING Noun] COMES] [Word Verb]
	WORDS:[COME [COMING Noun] COMES CAME WENT] [Word Verb]
	WORDS:[COMES CAME WENT COME [COMING Noun]] [Word Verb]
	WORDS:[[COMING Noun] COMES CAME WENT COME] [Word Verb]
	WORDS:[COMPUTE COMPUTES COMPUTED [COMPUTING Noun]] [Word Verb]
	WORDS:[COMPUTER] [Word]
	WORDS:[[CAN Noun] COULD] [Word Verb]
	WORDS:[CANNOT 	] [Word Verb]
	WORDS:[CARRY CARRIED CARRIES [CARRYING Noun]] [Word Verb]
	WORDS:[CLEAN CLEANED CLEANS [CLEANING Noun]] [Word Verb]
	WORDS:[COLD 	] [Word Noun]
	WORDS:[COME COMES CAME [COMING Noun] WENT] [Word Verb]
	WORDS:[COULD [CAN Noun]] [Word Verb]
	WORDS:[[CUT Noun] CUTS] [Word Verb]
	WORDS:[DESPITE 	] [Word]
	WORDS:[DID DONE [DOING Noun] DO DOES] [Word Verb]
	WORDS:[[DISPLAY Noun] [DISPLAYS Noun] DISPLAYED [DISPLAYING Noun]] [Word Verb]
    WORDS:[DO DOES DID DONE [DOING Noun]] [Word Verb]
	WORDS:[DOES DID DONE [DOING Noun] DO] [Word Verb]
	WORDS:[[DOING Noun] DO DOES DID DONE] [Word Verb]
	WORDS:[DONE [DOING Noun] DO DOES DID] [Word Verb]
	WORDS:[DONOT 	] [Word Verb]
	WORDS:[DOWN 	] [Word Adverb]
	WORDS:[DRAW DRAWS DRAWN [DRAWING Noun]] [Word Verb]
	WORDS:[[DRINK Noun] DRINKS DRANK [DRUNK Noun] [DRINKING Noun]] [Word Verb]
	WORDS:[DURING	] [Word]
	WORDS:[EACH EVERY EVERYBODY EVERYONE EVERYTHING ALL] [Word Noun Qualifier]
	WORDS:[EARN EARNS EARNED [EARNING Noun Adjective] [EARNINGS Noun Adjective]] [Word Verb]
	WORDS:[EAT ATE EATEN EATS [EATING Noun]] [Word Verb]
	WORDS:[EIGHT 	] [Word [NumValue 8]]
	WORDS:[[EQUAL Noun] [EQUALS Noun] IS WAS WERE AM ARE BE BEEN] [Word Verb Assignment]
	WORDS:[[EQUALS Noun] IS WAS WERE AM ARE BE BEEN [EQUAL Noun]] [Word Verb Assignment]
	WORDS:[EQUATION EQUATIONS FORMULA FORMULAS] [Word Noun]
	WORDS:[EVALUATE EVALUATES EVALUATED [EVALUATING Noun]] [Word Verb Compute]
	WORDS:[EVERY EVERYBODY EVERYONE EVERYTHING ALL EACH] [Word Qualifier]
	WORDS:[EVERYBODY EVERYONE EVERYTHING ALL EACH EVERY] [Word Qualifier]
	WORDS:[EVERYONE EVERYTHING ALL EACH EVERY EVERYBODY] [Word Qualifier]
	WORDS:[EVERYTHING ALL EACH EVERY EVERYBODY EVERYONE] [Word Qualifier]
	WORDS:[EXCEPT 	] [Word]
	WORDS:[[FALL Noun] FALLS FALLEN FELL [FALLING Noun]] [Word Verb]
	WORDS:[FAR 	] [Word Adjective]
	WORDS:[FAST FASTER FASTEST] [Word Adjective]
	WORDS:[FEW FEWER FEWEST] [Word Adjective]
	WORDS:[[FIND Noun] FINDS FOUND [FINDING Noun]] [Word Verb]
	WORDS:[FIRST 	] [Word Adjective [NumValue 1]]
	WORDS:[FIVE 	] [Word Noun [NumValue 5]]
	WORDS:[[FLY Noun] [FLYS Noun] FLEW FLOWN [FLYING Noun]] [Word Verb]
	WORDS:[FOR 	] [Word]
	WORDS:[FORMULA FORMULAS EQUATION EQUATIONS] [Word Noun]
	WORDS:[FOUR 	] [Word Noun [NumValue 4]]
	WORDS:[FROM 	] [Word]
	WORDS:[FULL FULLER FULLEST] [Word Adjective]
	WORDS:[FUNNY FUNNIER FUNNIEST] [Word Adjective]
	WORDS:[GET GETS GOT GOTTEN GETTING] [Word Verb]
	WORDS:[GIVE GIVES GIVEN [GIVING Noun] GAVE] [Word Verb]
	WORDS:[GO GOES GONE [GOING Noun] WENT] [Word Verb]
	WORDS:[GOOD 	] [Word Adjective Noun]
	WORDS:[GREEN 	] [Word Noun Color]
	WORDS:[GROW GROWS GREW GROWN [GROWING Noun]] [Word Verb]
	WORDS:[HAVE HAS HAD [HAVING Noun]] [Word Verb]
	WORDS:[HE HIM HIMSELF HIS] [Word]
	WORDS:[[HELP Noun] HELPS HELPED [HELPFUL Adjective]] [Word Verb]
	WORDS:[HER HERS HERSELF SHE] [Word]
	WORDS:[HERE 	] [Word]
	WORDS:[HERS HERSELF SHE HER] [Word]
	WORDS:[HERSELF SHE HER HERS] [Word]
	WORDS:[HIM HIMSELF HIS HE] [Word]
	WORDS:[HIMSELF HIS HE HIM] [Word]
	WORDS:[HIS HE HIM HIMSELF] [Word]
	WORDS:[HISTORY HISTORIES] [Word Noun]
	WORDS:[[HOLD Noun] HOLDS HELD [HOLDING Noun]] [Word Verb]
	WORDS:[HOT 	] [Word]
	WORDS:[HOW HOWEVER] [Word]
	WORDS:[HOWEVER HOW] [Word]
	WORDS:[[HURT Noun] HURTS [HURTING Noun]] [Word Verb]
	WORDS:[I ME MINE MY MYSELF OUR OURS OURSELF OURSELVES US WE] [Word]
	WORDS:[IF 	] [Word]
	WORDS:[IN 	] [Word]
	WORDS:[INSIDE 	] [Word]
	WORDS:[INTO 	] [Word]
	WORDS:[IS WAS WERE AM ARE BE BEEN [EQUAL Noun] [EQUALS Noun]] [Word Verb Assignment]
	WORDS:[IT ITS ITSELF ] [Word]
	WORDS:[ITS ITSELF IT ] [Word]
	WORDS:[ITSELF IT ITS ] [Word]
	WORDS:[[JUMP Noun] JUMPS JUMPED [JUMPING Noun]] [Word Verb]
	WORDS:[JUST JUSTICE] [Word Noun]
	WORDS:[[KEEP Noun] KEEPS KEPT [KEEPING Noun]] [Word Verb]
	WORDS:[[KIND Noun] KINDLY] [Word Adjective]
	WORDS:[KNOW KNOWS KNEW [KNOWING Noun]] [Word Verb]
	WORDS:[[LAUGH Noun] LAUGHS LAUGHED [LAUGHING Noun]] [Word Verb]
	WORDS:[LET LETS [LETTING Noun]] [Word Verb]
	WORDS:[[LIGHT Noun Adjective] [LIGHTS Noun] LIGHTED LIT [LIGHTING Noun]	] [Word Verb]
	WORDS:[LIKE LIKES LIKED [LIKING Noun]] [Word Verb]
	WORDS:[LITTLE 	] [Word Adjective]
	WORDS:[[LIFE Noun] LIVE LIVES LIVED [LIVING Noun]] [Word Verb]
	WORDS:[LONG 	] [Word]
	WORDS:[[LOOK Noun] [LOOKS Noun] LOOKED [LOOKING Noun]] [Word Verb]
	WORDS:[[MAKE Noun] MADE MAKES [MAKING Noun]] [Word Verb]
	WORDS:[MANY MUCH] [Word Adjective]
	WORDS:[MATH MATHEMATICS] [Word Noun]                                   
	WORDS:[MAY MIGHT] [Word Verb]
	WORDS:[ME MINE MY MYSELF OUR OURS OURSELF OURSELVES US WE I] [Word]
	WORDS:[MINE MY MYSELF OUR OURS OURSELF OURSELVES US WE I ME] [Word]
	WORDS:[MUCH MANY] [Word Adjective]
	WORDS:[MUST 	] [Word]
	WORDS:[MY MYSELF OUR OURS OURSELF OURSELVES US WE I ME MINE ] [Word]
	WORDS:[MYSELF OUR OURS OURSELF OURSELVES US WE I ME MINE MY] [Word]
	WORDS:[NEAR NEARER NEAREST	] [Word]
	WORDS:[NEITHER 	] [Word]
	WORDS:[NEVER 	] [Word]
	WORDS:[NEW NEWER NEWEST	] [Word]
	WORDS:[NEXT 	] [Word]
	WORDS:[NO	] [Word]
	WORDS:[NOBODY NONE NONONE] [Word]
	WORDS:[NONE NONONE NOBODY] [Word]
	WORDS:[NONONE NOBODY NONE] [Word]
	WORDS:[NOR 	] [Word]
	WORDS:[NOT 	] [Word]
	WORDS:[NOW 	] [Word]
	WORDS:[OF 	] [Word]
	WORDS:[OFF 	] [Word]
	WORDS:[OLD OLDER OLDEST	] [Word]
	WORDS:[ON 	] [Word]
	WORDS:[ONCE SOME SOMEBODY SOMEONE ANY ANYBODY ANYONE ANYTHING [ONE [NumValue 1]]] [Word Noun Qualifier]
	WORDS:[[ONE [NumValue 1]] ONCE SOME SOMEBODY SOMEONE ANY ANYBODY ANYONE ANYTHING] [Word Noun Qualifier]
	WORDS:[ONLY 	] [Word]
	WORDS:[ONTO 	] [Word]
	WORDS:[OPEN OPENS OPENED [OPENING Noun]] [Word Verb]
	WORDS:[OPTION OPTIONS] [Word Noun]
	WORDS:[OR 	] [Word]
	WORDS:[OUR OURS OURSELF OURSELVES US WE I ME MINE MY MYSELF] [Word]
	WORDS:[OURS OURSELF OURSELVES US WE I ME MINE MY MYSELF OUR ] [Word]
	WORDS:[OURSELF OURSELVES US WE I ME MINE MY MYSELF OUR OURS ] [Word]
	WORDS:[OURSELVES US WE I ME MINE MY MYSELF OUR OURS OURSELF ] [Word]
	WORDS:[OUT OUTSIDE] [Word Adjective]
	WORDS:[OUTSIDE OUT] [Word Adjective]
	WORDS:[OVER 	] [Word]
	WORDS:[OWN OWNS OWNED [OWNING Noun]] [Word Verb]
	WORDS:[PAST	] [Word]
	WORDS:[[PICK Noun] [PICKS Noun] PICKED [PICKING Noun]] [Word Verb]
	WORDS:[[PLAY Noun] [PLAYS Noun] PLAYED [PLAYING Noun]] [Word Verb]
	WORDS:[PLEASE PLEASES PLEASED [PLEASING Noun]] [Word Verb]
	WORDS:[PRETTY 	] [Word Adjective]
	WORDS:[[PRINT Noun] [PRINTS Noun] PRINTED [PRINTING Noun]] [Word Verb]
	WORDS:[PULL PULLS PULLED [PULLING Noun]] [Word Verb]
	WORDS:[PUT PUTS [PUTTING Noun]] [Word Verb]
	WORDS:[QUARTER QUARTERS [QUARTERLY Adjective]] [Word Noun]
	WORDS:[READ [READING Noun]] [Word Verb]
	WORDS:[RECOMMENDATION RECOMMENDATIONS PICK PICKS] [Word Noun]
	WORDS:[RED 	] [Word Noun Color]
	WORDS:[REPORT REPORTS] [Word Noun]
	WORDS:[RETRIEVE RETRIEVES RETRIEVED RETRIEVING] [Word Verb]
	WORDS:[[RIDE Noun] RIDES RIDDEN [RIDING Noun]] [Word Verb]
	WORDS:[RIGHT RIGHTS] [Word Noun]
	WORDS:[ROUND 	] [Word Adjective]
	WORDS:[[RUN Noun] RAN [RUNNING Noun]] [Word Verb]
	WORDS:[SALE SALES] [Word Noun]
	WORDS:[SAW 	] [Word]
	WORDS:[SAY SAYS SAID [SAYING Noun]] [Word Verb]
	WORDS:[SEE SEES SAW [SEEING Noun]] [Word Verb]
	WORDS:[SEVEN] [Word [NumValue 7]]
	WORDS:[SEVERAL 	] [Word]
	WORDS:[SHALL SHOULD	] [Word Verb]
	WORDS:[SHE HER HERS HERSELF] [Word]
	WORDS:[SHOW SHOWS SHOWED SHOWN [SHOWING Noun]] [Word Verb]
	WORDS:[SINCE] [Word]
	WORDS:[SING SINGS SANG [SINGING Noun]] [Word Verb]
	WORDS:[SIT SITS SAT [SITTING Noun]] [Word Verb]
	WORDS:[SIX] [Word [NumValue 6]]
	WORDS:[[SLEEP Noun] SLEEPS SLEPT [SLEEPING Noun]] [Word Verb]
	WORDS:[SMALL] [Word]
	WORDS:[SO] [Word]
	WORDS:[SOME SOMEBODY SOMEONE ANY ANYBODY ANYONE ANYTHING [ONE [NumValue 1]] ONCE] [Word Noun Qualifier]
	WORDS:[SOMEBODY SOMEONE ANY ANYBODY ANYONE ANYTHING [ONE [NumValue 1]] ONCE SOME] [Word Noun Qualifier]
	WORDS:[SOMEONE ANY ANYBODY ANYONE ANYTHING [ONE [NumValue 1]] ONCE SOME SOMEBODY] [Word Noun Qualifier]
	WORDS:[SOON 	] [Word]
	WORDS:[[START Noun] STARTS STARTED [STARTING Noun]] [Word Verb]
	WORDS:[STOCK STOCKS SECURITY SECURITIES PORTFOLIO PORTFOLIOS] [Word Noun]
	WORDS:[[STOP Noun] STOPS STOPPED [STOPPING Noun]] [Word Verb]
	WORDS:[SUMMARY SUMMARIES] [Word Noun]
	WORDS:[[TAKE Noun] TAKES TOOK [TAKING Noun]] [Word Verb]
	WORDS:[TELL TELLS TOLD [TELLING Noun]] [Word Verb]
	WORDS:[TEN 	] [Word [NumValue 10]]
	WORDS:[THANK [THANKS Noun] THANKED [THANKING Noun]] [Word Verb]
	WORDS:[THE THAT THIS THOSE THESE] [Word Article]
	WORDS:[THEIR THEIRS] [Word Adjective]
	WORDS:[THEM THEMSELVES THEY] [Word]
	WORDS:[THEN 	] [Word]
	WORDS:[THERE 	] [Word]
	WORDS:[THINK [THOUGHT Noun] THINKS [THINKING Noun]] [Word Verb]
	WORDS:[TRADER TRADERS] [Word Noun]
	WORDS:[THREE 	] [Word [NumValue 3]]
	WORDS:[THROUGH THROUGHOUT] [Word]
	WORDS:[TILL TILLS TILLED [TILLING Noun]] [Word Verb]
	WORDS:[TO 	] [Word]
	WORDS:[TODAY 	] [Word]
	WORDS:[TOGETHER 	] [Word]
	WORDS:[TOO 	] [Word]
	WORDS:[TOWARD TOWARDS] [Word]
	WORDS:[TRY TRYS TRYED [TRYING Noun]] [Word Verb]
	WORDS:[TWO 	] [Word [NumValue 2]]
	WORDS:[UNDER UNDERNEATH] [Word]
	WORDS:[UNLIKE 	] [Word]
	WORDS:[UNTIL] [Word]
	WORDS:[UP 	] [Word]
	WORDS:[UPON 	] [Word]
	WORDS:[US WE I ME MINE MY MYSELF OUR OURS OURSELF OURSELVES ] [Word]
	WORDS:[USE 	] [Word]
	WORDS:[VERY 	] [Word]
	WORDS:[[WALK Noun] [WALKS Noun] WALKED [WALKING Noun]] [Word Verb]
	WORDS:[WANT WANTS WANTED [WANTING Noun]] [Word Verb]
	WORDS:[WARM WARMS WARMED [WARMING Noun]] [Word Verb]
	WORDS:[WAS WERE AM ARE BE BEEN [EQUAL Noun] [EQUALS Noun] IS] [Word Verb Assignment]
	WORDS:[[WASH Noun] WASHES WASHED [WASHING Noun]] [Word Verb]
	WORDS:[WE I ME MINE MY MYSELF OUR OURS OURSELF OURSELVES US ] [Word]
	WORDS:[WEEK WEEKS [WEEKLY Adjective]] [Word Noun]
	WORDS:[WELL 	] [Word]
	WORDS:[WENT COME COMES [COMING Noun] CAME GO GOES GONE [GOING Noun]] [Word Verb]
	WORDS:[WERE AM ARE BE BEEN [EQUAL Noun] [EQUALS Noun] IS WAS] [Word Verb Assignment]
	WORDS:[WHAT 	] [Word]
	WORDS:[WHATEVER 	] [Word]
	WORDS:[WHEN 	] [Word]
	WORDS:[WHICH WHICHEVER] [Word]
	WORDS:[WHITE ] [Word Color Noun]
	WORDS:[WHO WHOEVER WHOM WHOMEVER] [Word]
	WORDS:[WHY 	] [Word Verb]
	WORDS:[WILL WOULD] [Word]
	WORDS:[[WISH Noun] WISHES WISHED [WISHING Noun]] [Word Verb]
	WORDS:[WITH WITHIN] [Word]
	WORDS:[WITHOUT	] [Word]
	WORDS:[[WORK Noun] [WORKS Noun] WORKED [WORKING Noun]] [Word Verb]
	WORDS:[WILL WOULD] [Word]
	WORDS:[WRITE 	] [Word Color Noun]
	WORDS:[YELLOW 	] [Word Color Noun]
	WORDS:[YES	] [Word]
	WORDS:[YIELD YIELDS] [Word Noun]
	WORDS:[YOU YOUR YOURS YOURSELF YOURSELVES] [Word]

  ;; ***********************************************
  ;; Basic Words Feature List for the query language
  ;; Note: Standard English language features for 
  ;;       the words defined in the Basic Words List
  ;; ***********************************************

	Adjective: [A AN THE THESE THIS THOSE]
                                               
	Preposition: [ABOUT ABOVE ACROSS AFTER AGAINST ALONG AMID AMONG AROUND AS ASIDE AT]
	Preposition: [BECAUSE BEFORE BEHIND BELOW BENEATH BESIDE BETWEEN BEYOND BUT BY DESPITE DOWN DURING]
	Preposition: [EXCEPT FOR FROM IN INSIDE INTO LIKE NEAR NEXT OF OFF ON ONTO OUT OUTSIDE OVER PAST]
	Preposition: [ROUND SINCE THROUGH THROUGHOUT TILL TO TOWARD TOWARDS UNDER UNDERNEATH UNLIKE UNTIL UP UPON WITH WITHIN WITHOUT] 


	Pronoun: [ALL AND ANOTHER ANY ANYBODY ANYONE ANYTHING EACH EVERYBODY EVERYONE EVERYTHING FEW]
	Pronoun: [HE HER HERS HERSELF HIM HIMSELF HIS I IT ITS ITSELF MANY ME MINE MYSELF]
	Pronoun: [NOBODY NONE ONE OUR OURS OURSELVES SEVERAL SHE SOME SOMEBODY SOMEONE THAT THEIRS]
	Pronoun: [THEM THEMSELVES THESE THEY THIS THOSE US WE WHAT WHATEVER WHICH WHICHEVER WHO]
	Pronoun: [WHOEVER WHOM WHOMEVER YOU YOURS YOURSELF YOURSELVES]

	Article: [A AN THE THESE THIS THOSE]
                                               
	Conjunction: [AND OR NOT HOWEVER BUT]

#End#









;;**EXPORTKEY**:alice:queryScript:@@defaultLexer
(defriend alice.queryScript defaultLexer(inString)
;; ********************************************************************
;; summary:  This agent converts an input string into a vector of
;;           recognized lexemes. It is the default lexical analyzer
;;           for the parseAgent compiler generator.
;;           This agent may be modified, in any way, by the user.
;; Parms:    inString   The source string to be broken into lexemes.
;; return:   tokenList  The vector of recognized lexemes.
;; ********************************************************************
    pvars:(;; Persistent variables
           CH                  ;; The current input character from the input string
           INLEN               ;; The length of the input string
           IP                  ;; The input pointer for the input string
           INSTRING            ;; The string of the input characters to be parsed
           keepWhitespaceSW    ;; Switch to keep all whitespace strings
           lowerCaseSW         ;; Switch to convert all names into lower case
           oldKB               ;; The old vector of character break parsing routines
           operatorList        ;; The vector of operator symbols
           KB                  ;; The vector of character break parsing routines
           SB                  ;; The vector of string terminator pairs
           tokenDirectory      ;; Lexicon of tokens and their attributes
           tokenList           ;; The vector of lexical tokens
           TP                  ;; The output pointer for the token output vector
           ;; Methods list
           addStringDelimiters ;; Add a pair of string delimiters to the lexical analyzer
           _default            ;; Recognize this one character
           defaultTokenRule    ;; Modified default rule for adding attributes to a parsed token
           _Ignore             ;; Ignore this character parsing routine
           _Initialize         ;; Initialize the vector of character break parsing routines
           _recFraction        ;; Recognize all fractions
           _recInteger         ;; Recognize all integers
           _recName            ;; Recognize all names
           _recNumber          ;; Recognize all numbers
           _recOperators       ;; Recognize all operator symbols
           _recSpecial         ;; Recognize all special symbols
           _recString          ;; Recognize all delimited strings
           _setFeatures        ;; Give features to a recognized token
           _whiteSpace         ;; Ignore all whitespace characters
           turnFractionsOnOff  ;; Turns fraction recognition on/off
           ) ;; end of persistent variables
    vars:(token oldIP)
    ;;************************************************************************
    ;;  Define the child agents for this parent.
    ;;************************************************************************
    ;; Add a named pair of string delimiters to the lexical analyzer.
    (defun addStringDelimiters(name start end)
       vars:(tmpAgent)
       ;;  Initialize the parseAgent once and only once.
       (if (= KB #void) (_Initialize))
       ;;  If this is the first delimiter pair, start a new directory.
       (setq CH start[0])
       (if (= SB[CH] #void) 
           (begin
              (setq SB[CH] (new Structure:))
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
        (setq operatorList #(#\= #\< #\> #\! #\^ #\~ #\+ #\/ #\* #\- #\| #\&))
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
        (setq oldKB (copy KB))
        ) ;; end of _Initialize
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
        (setq tokenList[TP] result)
        (++ TP)
        ) ;; end _recFraction
    ;;  Recognize all names.
    (defun _recName()
        vars:(oldIP)
        (setq oldIP IP)
        (setq CH INSTRING[(++ IP)]) 
        (while (isCharName CH) do
           (setq CH INSTRING[(++ IP)]) 
           ) ;; end while
        (if lowerCaseSW
            (setq tokenList[TP] (symbol (downcase (substring INSTRING oldIP (subi IP 1)))))
            (setq tokenList[TP] (symbol (substring INSTRING oldIP (subi IP 1))))
            ) ; end if
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
        (if (and (= CH #\.) (isCharNumeric INSTRING[(add1 IP)]))
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
        (setq tokenList[TP] num)
        (++ TP)
        ) ;; end _recNumber
    ;;  Recognize all integers.
    (defun _recInteger()
        vars:(oldIP num)
        (setq oldIP IP)
        (setq CH INSTRING[(++ IP)])
        ;; Recognize integer portion of number
        (while (isCharNumeric CH) do
           (setq CH INSTRING[(++ IP)]) 
           ) ;; end while
        (setq num (number (substring INSTRING oldIP (subi IP 1))))
        (if (= (integer num) num) (setq num (integer num)))
        (setq tokenList[TP] num)
        (++ TP)
        ) ;; end _recInteger
    ;;  Recognize all operator symbols.
    (defun _recOperators()
        vars:(oldIP)
        (setq oldIP IP)
        (setq CH INSTRING[(++ IP)]) 
        (while (isMember CH operatorList) do
           (setq CH INSTRING[(++ IP)]) 
           ) ;; end while
        (setq tokenList[TP] (symbol (substring INSTRING oldIP (subi IP 1))))
        (++ TP)
        ) ;; end _recOperators
    ;; Recognize all special symbols.
    (defun _recSpecial() (setq tokenList[TP] (symbol (string CH))) (++ IP) (++ TP))
    ;; Recognize all delimited strings.
    (defun _recString()
        vars:(oldIP i delimPairs delimLen result              
              name this start end startLen endLen)
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
                  (setq end delimPairs[i 1][1])
                  (setq endLen (length end))
                  (+= IP startLen)
                  (while (< IP INLEN) do
                     (if (= INSTRING[IP] end[0])
                         (begin
                            (setq this (mid INSTRING IP endLen))
                            (if (= end this)
                                (begin
                                   (+= IP endLen)
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
                  (if (or keepWhitespaceSW (<> (left name 10) "Whitespace"))
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
     ;; Give features to a recognized token
     (defun _setFeatures(token oldIP)
        vars:(parseTree treeIndex treeLen tokenAttr)
        ;; This agent tests compiled FSM style methods of attributing each parsed token.
        (setq tokenAttr tokenDirectory[token])
        (if (= tokenAttr #void)
            then
            ;; Create an attributed token using default rule
            (setq tokenAttr (defaultTokenRule token))
            else
            ;; Copy the attributes and set the Value from the dictionary
            (begin
               (setq tokenAttr (copy tokenAttr))
               (setq tokenAttr.Value token)
            )) ; end if
        ;; Set the displacement of the token in the source string
        (setq tokenAttr.Charpos (integer oldIP))
        (setq tokenList[(subi TP 1)] tokenAttr)
        true) ;; end of _setFeatures
    ;; Turns fraction recognition on/off.
    (defun turnFractionsOnOff(onOffSW)
        vars:(i)
        ;; Turn fractions on?
        (if onOffSW
            ;; Turn fractions on
            (begin
               (loop for i from (code #\0) to (code #\9) do
                  (if (= KB[i] oldKB[i]) (setq KB[i] _recNumber)) 
                  (setq oldKB[i] _recNumber)
                  ) ; end loop
               (if (= KB[(code #\.)] oldKB[(code #\.)]) (setq KB[(code #\.)] _recFraction))
               (setq oldKB[(code #\.)] _recFraction)
               ) ; end turn fractions on
            ;; Turn fractions off
            (begin
               (loop for i from (code #\0) to (code #\9) do
                  (if (= KB[i] oldKB[i]) (setq KB[i] _recInteger)) 
                  (setq oldKB[i] _recInteger)
                  ) ; end loop
               (if (= KB[(code #\.)] oldKB[(code #\.)]) (setq KB[(code #\.)] _recSpecial))
               (setq oldKB[(code #\.)] _recSpecial)
               ) ; end turn fractions off
            ) ; end if
        ) ;; end of turnFractionsOnOff
    ;;  Ignore all whitespace characters.
    (defun _whiteSpace()        
        vars:(oldIP i result)
        ;; Save old IP address
        (setq oldIP IP)
        ;; Loop until all whitespace chars are discovered
        (setq CH INSTRING[(++ IP)]) 
        (while (and (> CH 0) (<= CH 32)) do
           (setq CH INSTRING[(++ IP)]) 
           ) ;; end while
        ;; Return whitespace token (iff keepWhitespaceSW is true)
        (if keepWhitespaceSW
            (begin
               (setq result (substring INSTRING oldIP (subi IP 1)))
               (setq tokenList[TP] (new Vector: 2 "Whitespace" result))
               (++ TP)              
               )) ; end if
        ) ;; end _whiteSpace
    ;;************************************************************************
    ;;  Define the main code routines for this parent.
    ;;************************************************************************
    ;;  Initialize the parseAgent once and only once.
    (if (= KB #void) (_Initialize))
    ;;  Initialize the output token vector. 
    (setq tokenList (new Vector: 0))
    (setq TP 0)
    ;;  Recognize each character in the input string.
    (setq INSTRING inString)
    (setq INLEN (length INSTRING))
    (setq IP 0)
    (while (< IP INLEN) do
        (setq oldIP IP)
        ;; Retrieve the next input character
        (setq CH INSTRING[IP])
        ;; Invoke the parse routine for this input character
        (KB[CH])
        ;; If a token was recognized, give it some features
        (if (and (> TP 0) (not (isStructure (setq token tokenList[(subi TP 1)])))) (_setFeatures token oldIP))
        ) ;; end while
    ;;  Return the token list as the output
    tokenList) ;; end defaultLexer

























;;**EXPORTKEY**:alice:queryScript:@UserDefinedFunctions
;; ************************************************
;; alice.queryScript User defined functions
;; ************************************************
















;;**EXPORTKEY**:alice:queryScript:_LEXRULE_CDATA
;; ************************************************
;; CDATA user defined Lexical Rule implementation
;; Summary: This agent implements the CDATA
;;          user defined rule. Each rule test is
;;          marked with a boxed comment line for
;;          ease of human understanding.
;; Note: This code was machine generated by parseAgent.
;; ************************************************
(defchild alice.queryScript _LEXRULE_CDATA(...)
   ;; The token object _io is a persistent variable
   ;; The token pointer _ip is a persistent variable
   ;; All getToken logic increments _ip on each token fetch.
   ;; The return value from the rule is stored in _ret.
   ;; Remember that this rule may repeat depending on _repeatSW.
   vars:(_tk0 _tk1 _tk2 _tk3 _tk4  _tk5  _i
         _tk6 _tk7 _tk8 _tk9 _tkn  _repeatSW
         _ak0 _ak1 _ak2 _ak3 _ak4  _ak5  _tkthis
         _ak6 _ak7 _ak8 _ak9 _ret  _oldIp
         _ip0 _ip1 _ip2 _ip3 _ip4  _ip5
         _ip6 _ip7 _ip8 _ip9 _tkn  _tkch
         _verboseSave       
        ) ; end temporary variables
   ;; Collect any arguments which may have been passed to this rule
   (if (> (setq _tkn (argCount)) 0)
       (begin (setq _ak0 (argFetch 0))
          (if (> _tkn 1) 
              (begin (setq _ak1 (argFetch 1))
                 (if (> _tkn 2) 
                     (begin (setq _ak2 (argFetch 2))
                        (if (> _tkn 3) 
                            (begin (setq _ak3 (argFetch 3))
                               (if (> _tkn 4) 
                                   (begin (setq _ak4 (argFetch 4))
                                      (if (> _tkn 5) 
                                          (begin (setq _ak5 (argFetch 5))
                                             (if (> _tkn 6) 
                                                 (begin (setq _ak6 (argFetch 6))
                                                    (if (> _tkn 7) 
                                                        (begin (setq _ak7 (argFetch 7))
                                                           (if (> _tkn 8) 
                                                               (begin (setq _ak8 (argFetch 8))
                                                               (if (> _tkn 9) (setq _ak9 (argFetch 9))
                                                           ))))))))))))))))))) ; end argument collection
   ;; The default structure is given the named rule attribute
   (setq _tk0 "")
   (setq _tkn 0)
   (setq _repeatSW true)
   (setq _oldIp _ip)

   (if (= _verboseLexCount.CDATA #void) 
          (setq _verboseLexCount.CDATA 1) 
          (setq _verboseLexCount.CDATA (iadd _verboseLexCount.CDATA 1)))

   (if (and (<> _verboseLexIn.CDATA #void) (> _verboseLexIn.CDATA -1))
       (begin (setq _verboseSave _verbose) (setq _verbose true))) 

   ;; Save the old token pointer and test for user defined rules
   Skip::
     (setq _indent (iadd _indent 1))
     (if _verbose (_logLine "Attempting CDATA Rule on: " input:))
     (setq _ip0 _ip)
     (setq _tkch (iadd _ip 1))
     (++ _tkn)
     (setq _repeatSW false)

     ;; *****************************************
     ;; Begin testing for each user defined rule.
     ;; *****************************************















     ;; ====================
     ;; case: "]]>"
     ;; ====================
     (if (if (= (setq _tk1 (mid $IN (iadd _ip 1) (begin (setq _ip (iadd _ip 3)) 3))) "]]>") true (setq _ip _ip0))
       (begin (setq _ruleCount (+ _ruleCount 1)) (if (and (> _verboseTrigger 0) (> _ruleCount _verboseTrigger) (not _verbose)) (_startLog)) (setq _ip1 _ip)
         ;; *********************************************************
         ;; RULE: CDATA: "]]>" :: (if (isString $0) (setq $0 (append $0 $1)) (setq $0 $1)) ::
         ;; *********************************************************
         (if true
          (begin
            (setq _ret  (if (isString _tk0) (setq _tk0 (append _tk0 _tk1)) (setq _tk0 _tk1)) )
            (if _verbose
                (writeRule
                     {CDATA: "]]>" :: (if (isString $0) (setq $0 (append $0 $1)) (setq $0 $1)) ::}
                     _ret  _tk0 _tk1 #void #void #void #void #void #void #void #void #void))
(setq _indent (isub _indent 1))
            (return _ret))
         ) ; end case : _default
       ) ; end begin
     ) ; end case : "]]>"
     ;; ====================
     ;; case: Any
     ;; ====================
     (if (begin (setq _ip _ip0)
      (if (<> (setq _tk1 $IN[(setq _ip (iadd _ip 1))]) #void) true (setq _ip _ip0)))
       (begin (setq _ruleCount (+ _ruleCount 1)) (if (and (> _verboseTrigger 0) (> _ruleCount _verboseTrigger) (not _verbose)) (_startLog)) (setq _ip1 _ip)
         ;; *********************************************************
         ;; RULE: CDATA: Any << (if (isString $0) (setq $0 (append $0 $1)) (setq $0 $1)) >>
         ;; *********************************************************
         (if true
          (begin
            (setq _ret  (if (isString _tk0) (setq _tk0 (append _tk0 _tk1)) (setq _tk0 _tk1)) )
            (if _verbose
                (writeRule
                     {CDATA: Any << (if (isString $0) (setq $0 (append $0 $1)) (setq $0 $1)) >>}
                     _ret  _tk0 _tk1 #void #void #void #void #void #void #void #void #void))
(setq _indent (isub _indent 1))
            (goto Skip:))
         ) ; end case : _default
       ) ; end begin
     ) ; end case : Any
     ;; ====================
     ;; case: Eof
     ;; ====================
     (if (begin (setq _ip _ip0)
      (if (= (setq _tk1 $IN[(setq _ip (iadd _ip 1))]) #void) true (setq _ip _ip0)))
       (begin (setq _ruleCount (+ _ruleCount 1)) (if (and (> _verboseTrigger 0) (> _ruleCount _verboseTrigger) (not _verbose)) (_startLog)) (setq _ip1 _ip)
         ;; *********************************************************
         ;; RULE: CDATA: Eof :: (_makeError "ALICE 101" $ch "<![CDATA[ with out matching ]]>") ::
         ;; *********************************************************
         (if true
          (begin
            (setq _ret  (_makeError "ALICE 101" _tkch "<![CDATA[ with out matching ]]>") )
            (if _verbose
                (writeRule
                     {CDATA: Eof :: (_makeError "ALICE 101" $ch "<![CDATA[ with out matching ]]>") ::}
                     _ret  _tk0 _tk1 #void #void #void #void #void #void #void #void #void))
(setq _indent (isub _indent 1))
            (return _ret))
         ) ; end case : _default
       ) ; end begin
     ) ; end case : Eof
     ;; **************************************************
     ;; DEFAULT (if we get to here we failed to recognize 
     ;; Note:       (at least on this repetition)
     ;; **************************************************
     (begin
        (if (= _tkn 1)
            ;; If we have never recognized any token on previous repetitions 
            (begin
               (setq _ip _oldIp)     
               (if _verbose (_logLine "Rejecting Rule CDATA on: " input: ))
               (setq _indent (isub _indent 1))
               (setq _ret morphFail)
               ) ; end begin then
            else
            ;; If we have recognized some tokens on previous repetitions, then return $0
            ;; Note: In Syntax Rules, we do not fail the rule if it has 
            ;;       recognized a token in one or more repetitions of the rule. 
            (begin
               (setq _ip _ip0)
               (setq _ret _tk0)
               ) ; end begin else
            ) ; end if
        ) ; end of DEFAULT

   ;; End of tests for user defined rules.

   (if (> _verboseLexIn.CDATA -1)
       (begin
          (setq _verbose _verboseSave)
          (if (>= _verboseLexCount.CDATA _verboseLexIn.CDATA) (error "Count" "in Routine CDATA"))
       ))

   ;; Repeat or return to caller is handled here.
   _ret) ;; end _LEXRULE_CDATA

























;;**EXPORTKEY**:alice:queryScript:_LEXRULE_MAIN
;; ************************************************
;; MAIN user defined Lexical Rule implementation
;; Summary: This agent implements the MAIN
;;          user defined rule. Each rule test is
;;          marked with a boxed comment line for
;;          ease of human understanding.
;; Note: This code was machine generated by parseAgent.
;; ************************************************
(defchild alice.queryScript _LEXRULE_MAIN(...)
   ;; The token object _io is a persistent variable
   ;; The token pointer _ip is a persistent variable
   ;; All getToken logic increments _ip on each token fetch.
   ;; The return value from the rule is stored in _ret.
   ;; Remember that this rule may repeat depending on _repeatSW.
   vars:(_tk0 _tk1 _tk2 _tk3 _tk4  _tk5  _i
         _tk6 _tk7 _tk8 _tk9 _tkn  _repeatSW
         _ak0 _ak1 _ak2 _ak3 _ak4  _ak5  _tkthis
         _ak6 _ak7 _ak8 _ak9 _ret  _oldIp
         _ip0 _ip1 _ip2 _ip3 _ip4  _ip5
         _ip6 _ip7 _ip8 _ip9 _tkn  _tkch
         _verboseSave       
        ) ; end temporary variables
   ;; Collect any arguments which may have been passed to this rule
   (if (> (setq _tkn (argCount)) 0)
       (begin (setq _ak0 (argFetch 0))
          (if (> _tkn 1) 
              (begin (setq _ak1 (argFetch 1))
                 (if (> _tkn 2) 
                     (begin (setq _ak2 (argFetch 2))
                        (if (> _tkn 3) 
                            (begin (setq _ak3 (argFetch 3))
                               (if (> _tkn 4) 
                                   (begin (setq _ak4 (argFetch 4))
                                      (if (> _tkn 5) 
                                          (begin (setq _ak5 (argFetch 5))
                                             (if (> _tkn 6) 
                                                 (begin (setq _ak6 (argFetch 6))
                                                    (if (> _tkn 7) 
                                                        (begin (setq _ak7 (argFetch 7))
                                                           (if (> _tkn 8) 
                                                               (begin (setq _ak8 (argFetch 8))
                                                               (if (> _tkn 9) (setq _ak9 (argFetch 9))
                                                           ))))))))))))))))))) ; end argument collection
   ;; The default structure is given the named rule attribute
   (setq _tk0 "")
   (setq _tkn 0)
   (setq _repeatSW true)
   (setq _oldIp _ip)

   (if (= _verboseLexCount.MAIN #void) 
          (setq _verboseLexCount.MAIN 1) 
          (setq _verboseLexCount.MAIN (iadd _verboseLexCount.MAIN 1)))

   (if (and (<> _verboseLexIn.MAIN #void) (> _verboseLexIn.MAIN -1))
       (begin (setq _verboseSave _verbose) (setq _verbose true))) 

   ;; Save the old token pointer and test for user defined rules
   Skip::
     (setq _indent (iadd _indent 1))
     (if _verbose (_logLine "Attempting MAIN Rule on: " input:))
     (setq _ip0 _ip)
     (setq _tkch (iadd _ip 1))
     (++ _tkn)
     (setq _repeatSW false)

     ;; *****************************************
     ;; Begin testing for each user defined rule.
     ;; *****************************************















     ;; ====================
     ;; case: DQuote
     ;; ====================
     (if (if (and (<> (setq _tk1 $IN[(setq _ip (iadd _ip 1))]) #void) (= _LF_DQuote[_tk1] 1)) true (setq _ip _ip0))
       (begin (setq _ruleCount (+ _ruleCount 1)) (if (and (> _verboseTrigger 0) (> _ruleCount _verboseTrigger) (not _verbose)) (_startLog)) (setq _ip1 _ip)
         ;; ====================
         ;; case: NotDQuote*
         ;; ====================
         (if (begin (setq _i 0) (setq _tk2 (makeString {})) (while (and (<> (setq _tkthis $IN[(setq _ip (iadd _ip 1))]) #void) (= _LF_NotDQuote[_tkthis] 1)) do (begin (setq _tk2[_i] _tkthis) (setq _i (iadd _i 1)) )) (setq _ip (isub _ip 1)) true)
           (begin (setq _ruleCount (+ _ruleCount 1)) (if (and (> _verboseTrigger 0) (> _ruleCount _verboseTrigger) (not _verbose)) (_startLog)) (setq _ip2 _ip)
             ;; ====================
             ;; case: DQuote
             ;; ====================
             (if (if (and (<> (setq _tk3 $IN[(setq _ip (iadd _ip 1))]) #void) (= _LF_DQuote[_tk3] 1)) true (setq _ip _ip2))
               (begin (setq _ruleCount (+ _ruleCount 1)) (if (and (> _verboseTrigger 0) (> _ruleCount _verboseTrigger) (not _verbose)) (_startLog)) (setq _ip3 _ip)
                 ;; *********************************************************
                 ;; RULE: MAIN: DQuote NotDQuote* DQuote << ($ASIS $ch (string (append $1 $2 $3)) Original: (string (append $1 $2 $3)) String: true Constant: true) >>
                 ;; *********************************************************
                 (if true
                  (begin
                    (setq _ret  (_tkASIS _tkch (string (append _tk1 _tk2 _tk3)) Original: (string (append _tk1 _tk2 _tk3)) String: true Constant: true) )
                    (if _verbose
                        (writeRule
                             {MAIN: DQuote NotDQuote* DQuote << ($ASIS $ch (string (append $1 $2 $3)) Original: (string (append $1 $2 $3)) String: true Constant: true) >>}
                             _ret  _tk0 _tk1 _tk2 _tk3 #void #void #void #void #void #void #void))
(setq _indent (isub _indent 1))
                    (goto Skip:))
                 ) ; end case : _default
               ) ; end begin
             ) ; end case : DQuote
           ) ; end begin
         ) ; end case : NotDQuote*
       ) ; end begin
     ) ; end case : DQuote
     ;; ====================
     ;; case: Quote
     ;; ====================
     (if (begin (setq _ip _ip0)
      (if (and (<> (setq _tk1 $IN[(setq _ip (iadd _ip 1))]) #void) (= _LF_Quote[_tk1] 1)) true (setq _ip _ip0)))
       (begin (setq _ruleCount (+ _ruleCount 1)) (if (and (> _verboseTrigger 0) (> _ruleCount _verboseTrigger) (not _verbose)) (_startLog)) (setq _ip1 _ip)
         ;; ====================
         ;; case: NotQuote*
         ;; ====================
         (if (begin (setq _i 0) (setq _tk2 (makeString {})) (while (and (<> (setq _tkthis $IN[(setq _ip (iadd _ip 1))]) #void) (= _LF_NotQuote[_tkthis] 1)) do (begin (setq _tk2[_i] _tkthis) (setq _i (iadd _i 1)) )) (setq _ip (isub _ip 1)) true)
           (begin (setq _ruleCount (+ _ruleCount 1)) (if (and (> _verboseTrigger 0) (> _ruleCount _verboseTrigger) (not _verbose)) (_startLog)) (setq _ip2 _ip)
             ;; ====================
             ;; case: Quote
             ;; ====================
             (if (if (and (<> (setq _tk3 $IN[(setq _ip (iadd _ip 1))]) #void) (= _LF_Quote[_tk3] 1)) true (setq _ip _ip2))
               (begin (setq _ruleCount (+ _ruleCount 1)) (if (and (> _verboseTrigger 0) (> _ruleCount _verboseTrigger) (not _verbose)) (_startLog)) (setq _ip3 _ip)
                 ;; *********************************************************
                 ;; RULE: MAIN: Quote NotQuote* Quote << ($ASIS $ch (string (append $1 $2 $3)) Original: (string (append $1 $2 $3)) Symbol: true Constant: true) >>
                 ;; *********************************************************
                 (if true
                  (begin
                    (setq _ret  (_tkASIS _tkch (string (append _tk1 _tk2 _tk3)) Original: (string (append _tk1 _tk2 _tk3)) Symbol: true Constant: true) )
                    (if _verbose
                        (writeRule
                             {MAIN: Quote NotQuote* Quote << ($ASIS $ch (string (append $1 $2 $3)) Original: (string (append $1 $2 $3)) Symbol: true Constant: true) >>}
                             _ret  _tk0 _tk1 _tk2 _tk3 #void #void #void #void #void #void #void))
(setq _indent (isub _indent 1))
                    (goto Skip:))
                 ) ; end case : _default
               ) ; end begin
             ) ; end case : Quote
           ) ; end begin
         ) ; end case : NotQuote*
       ) ; end begin
     ) ; end case : Quote
     ;; ====================
     ;; case: Lt
     ;; ====================
     (if (begin (setq _ip _ip0)
      (if (and (<> (setq _tk1 $IN[(setq _ip (iadd _ip 1))]) #void) (= _LF_Lt[_tk1] 1)) true (setq _ip _ip0)))
       (begin (setq _ruleCount (+ _ruleCount 1)) (if (and (> _verboseTrigger 0) (> _ruleCount _verboseTrigger) (not _verbose)) (_startLog)) (setq _ip1 _ip)
         ;; ====================
         ;; case: Bang
         ;; ====================
         (if (if (and (<> (setq _tk2 $IN[(setq _ip (iadd _ip 1))]) #void) (= _LF_Bang[_tk2] 1)) true (setq _ip _ip1))
           (begin (setq _ruleCount (+ _ruleCount 1)) (if (and (> _verboseTrigger 0) (> _ruleCount _verboseTrigger) (not _verbose)) (_startLog)) (setq _ip2 _ip)
             ;; ====================
             ;; case: "[CDATA["
             ;; ====================
             (if (if (= (setq _tk3 (mid $IN (iadd _ip 1) (begin (setq _ip (iadd _ip 7)) 7))) "[CDATA[") true (setq _ip _ip2))
               (begin (setq _ruleCount (+ _ruleCount 1)) (if (and (> _verboseTrigger 0) (> _ruleCount _verboseTrigger) (not _verbose)) (_startLog)) (setq _ip3 _ip)
                 ;; ====================
                 ;; case: CDATA
                 ;; ====================
                 (if (if (<> (setq _tk4 (_LEXRULE_CDATA)) morphFail)true (setq _ip _ip3))
                   (begin (setq _ruleCount (+ _ruleCount 1)) (if (and (> _verboseTrigger 0) (> _ruleCount _verboseTrigger) (not _verbose)) (_startLog)) (setq _ip4 _ip)
                     ;; *********************************************************
                     ;; RULE: MAIN: Lt Bang "[CDATA[" CDATA << ($ASIS $ch (string (append $1 $2 $3 $4)) Original: (string (append $1 $2 $3 $4)) String: true Cdata: true Constant: true) >>
                     ;; *********************************************************
                     (if true
                      (begin
                        (setq _ret  (_tkASIS _tkch (string (append _tk1 _tk2 _tk3 _tk4)) Original: (string (append _tk1 _tk2 _tk3 _tk4)) String: true Cdata: true Constant: true) )
                        (if _verbose
                            (writeRule
                                 {MAIN: Lt Bang "[CDATA[" CDATA << ($ASIS $ch (string (append $1 $2 $3 $4)) Original: (string (append $1 $2 $3 $4)) String: true Cdata: true Constant: true) >>}
                                 _ret  _tk0 _tk1 _tk2 _tk3 _tk4 #void #void #void #void #void #void))
(setq _indent (isub _indent 1))
                        (goto Skip:))
                     ) ; end case : _default
                   ) ; end begin
                 ) ; end case : CDATA
               ) ; end begin
             ) ; end case : "[CDATA["
           ) ; end begin
         ) ; end case : Bang
       ) ; end begin
     ) ; end case : Lt
     ;; ====================
     ;; case: NameStart
     ;; ====================
     (if (begin (setq _ip _ip0)
      (if (and (<> (setq _tk1 $IN[(setq _ip (iadd _ip 1))]) #void) (= _LF_NameStart[_tk1] 1)) true (setq _ip _ip0)))
       (begin (setq _ruleCount (+ _ruleCount 1)) (if (and (> _verboseTrigger 0) (> _ruleCount _verboseTrigger) (not _verbose)) (_startLog)) (setq _ip1 _ip)
         ;; ====================
         ;; case: NameChar*
         ;; ====================
         (if (begin (setq _i 0) (setq _tk2 (makeString {})) (while (and (<> (setq _tkthis $IN[(setq _ip (iadd _ip 1))]) #void) (= _LF_NameChar[_tkthis] 1)) do (begin (setq _tk2[_i] _tkthis) (setq _i (iadd _i 1)) )) (setq _ip (isub _ip 1)) true)
           (begin (setq _ruleCount (+ _ruleCount 1)) (if (and (> _verboseTrigger 0) (> _ruleCount _verboseTrigger) (not _verbose)) (_startLog)) (setq _ip2 _ip)
             ;; ====================
             ;; case: Quote
             ;; ====================
             (if (if (and (<> (setq _tk3 $IN[(setq _ip (iadd _ip 1))]) #void) (= _LF_Quote[_tk3] 1)) true (setq _ip _ip2))
               (begin (setq _ruleCount (+ _ruleCount 1)) (if (and (> _verboseTrigger 0) (> _ruleCount _verboseTrigger) (not _verbose)) (_startLog)) (setq _ip3 _ip)
                 ;; ====================
                 ;; case: NameChar+
                 ;; ====================
                 (if (begin (setq _i 0) (setq _tk4 (makeString {})) (while (and (<> (setq _tkthis $IN[(setq _ip (iadd _ip 1))]) #void) (= _LF_NameChar[_tkthis] 1)) do (begin (setq _tk4[_i] _tkthis) (setq _i (iadd _i 1)) )) (setq _ip (isub _ip 1)) (if (> _i 0) true (setq _ip _ip3)))
                   (begin (setq _ruleCount (+ _ruleCount 1)) (if (and (> _verboseTrigger 0) (> _ruleCount _verboseTrigger) (not _verbose)) (_startLog)) (setq _ip4 _ip)
                     ;; *********************************************************
                     ;; RULE: MAIN: NameStart NameChar* Quote NameChar+ << ($OUTPLUS $ch (string (append $1 $2 $4)) Word: true) >>
                     ;; *********************************************************
                     (if true
                      (begin
                        (setq _ret  (_tkOUTPLUS _tkch (string (append _tk1 _tk2 _tk4)) Word: true) )
                        (if _verbose
                            (writeRule
                                 {MAIN: NameStart NameChar* Quote NameChar+ << ($OUTPLUS $ch (string (append $1 $2 $4)) Word: true) >>}
                                 _ret  _tk0 _tk1 _tk2 _tk3 _tk4 #void #void #void #void #void #void))
(setq _indent (isub _indent 1))
                        (goto Skip:))
                     ) ; end case : _default
                   ) ; end begin
                 ) ; end case : NameChar+
               ) ; end begin
             ) ; end case : Quote
             ;; *********************************************************
             ;; RULE: MAIN: NameStart NameChar* << ($OUTPLUS $ch (string (append $1 $2)) Word: true) >>
             ;; *********************************************************
             (if true
              (begin
                (setq _ip _ip2)
                (setq _ret  (_tkOUTPLUS _tkch (string (append _tk1 _tk2)) Word: true) )
                (if _verbose
                    (writeRule
                         {MAIN: NameStart NameChar* << ($OUTPLUS $ch (string (append $1 $2)) Word: true) >>}
                         _ret  _tk0 _tk1 _tk2 #void #void #void #void #void #void #void #void))
(setq _indent (isub _indent 1))
                (goto Skip:))
             ) ; end case : _default
           ) ; end begin
         ) ; end case : NameChar*
       ) ; end begin
     ) ; end case : NameStart
     ;; ====================
     ;; case: Digit+
     ;; ====================
     (if (begin (setq _ip _ip0)
      (begin (setq _i 0) (setq _tk1 (makeString {})) (while (and (<> (setq _tkthis $IN[(setq _ip (iadd _ip 1))]) #void) (= _LF_Digit[_tkthis] 1)) do (begin (setq _tk1[_i] _tkthis) (setq _i (iadd _i 1)) )) (setq _ip (isub _ip 1)) (if (> _i 0) true (setq _ip _ip0))))
       (begin (setq _ruleCount (+ _ruleCount 1)) (if (and (> _verboseTrigger 0) (> _ruleCount _verboseTrigger) (not _verbose)) (_startLog)) (setq _ip1 _ip)
         ;; ====================
         ;; case: NameChar+
         ;; ====================
         (if (begin (setq _i 0) (setq _tk2 (makeString {})) (while (and (<> (setq _tkthis $IN[(setq _ip (iadd _ip 1))]) #void) (= _LF_NameChar[_tkthis] 1)) do (begin (setq _tk2[_i] _tkthis) (setq _i (iadd _i 1)) )) (setq _ip (isub _ip 1)) (if (> _i 0) true (setq _ip _ip1)))
           (begin (setq _ruleCount (+ _ruleCount 1)) (if (and (> _verboseTrigger 0) (> _ruleCount _verboseTrigger) (not _verbose)) (_startLog)) (setq _ip2 _ip)
             ;; *********************************************************
             ;; RULE: MAIN: Digit+ NameChar+ << ($OUTPLUS $ch (string (append $1 $2)) Word: true) >>
             ;; *********************************************************
             (if true
              (begin
                (setq _ret  (_tkOUTPLUS _tkch (string (append _tk1 _tk2)) Word: true) )
                (if _verbose
                    (writeRule
                         {MAIN: Digit+ NameChar+ << ($OUTPLUS $ch (string (append $1 $2)) Word: true) >>}
                         _ret  _tk0 _tk1 _tk2 #void #void #void #void #void #void #void #void))
(setq _indent (isub _indent 1))
                (goto Skip:))
             ) ; end case : _default
           ) ; end begin
         ) ; end case : NameChar+
         ;; ====================
         ;; case: Period
         ;; ====================
         (if (begin (setq _ip _ip1)
          (if (and (<> (setq _tk2 $IN[(setq _ip (iadd _ip 1))]) #void) (= _LF_Period[_tk2] 1)) true (setq _ip _ip1)))
           (begin (setq _ruleCount (+ _ruleCount 1)) (if (and (> _verboseTrigger 0) (> _ruleCount _verboseTrigger) (not _verbose)) (_startLog)) (setq _ip2 _ip)
             ;; ====================
             ;; case: Digit*
             ;; ====================
             (if (begin (setq _i 0) (setq _tk3 (makeString {})) (while (and (<> (setq _tkthis $IN[(setq _ip (iadd _ip 1))]) #void) (= _LF_Digit[_tkthis] 1)) do (begin (setq _tk3[_i] _tkthis) (setq _i (iadd _i 1)) )) (setq _ip (isub _ip 1)) true)
               (begin (setq _ruleCount (+ _ruleCount 1)) (if (and (> _verboseTrigger 0) (> _ruleCount _verboseTrigger) (not _verbose)) (_startLog)) (setq _ip3 _ip)
                 ;; ====================
                 ;; case: Exponent
                 ;; ====================
                 (if (if (and (<> (setq _tk4 $IN[(setq _ip (iadd _ip 1))]) #void) (= _LF_Exponent[_tk4] 1)) true (setq _ip _ip3))
                   (begin (setq _ruleCount (+ _ruleCount 1)) (if (and (> _verboseTrigger 0) (> _ruleCount _verboseTrigger) (not _verbose)) (_startLog)) (setq _ip4 _ip)
                     ;; ====================
                     ;; case: Sign
                     ;; ====================
                     (if (if (and (<> (setq _tk5 $IN[(setq _ip (iadd _ip 1))]) #void) (= _LF_Sign[_tk5] 1)) true (setq _ip _ip4))
                       (begin (setq _ruleCount (+ _ruleCount 1)) (if (and (> _verboseTrigger 0) (> _ruleCount _verboseTrigger) (not _verbose)) (_startLog)) (setq _ip5 _ip)
                         ;; ====================
                         ;; case: Digit+
                         ;; ====================
                         (if (begin (setq _i 0) (setq _tk6 (makeString {})) (while (and (<> (setq _tkthis $IN[(setq _ip (iadd _ip 1))]) #void) (= _LF_Digit[_tkthis] 1)) do (begin (setq _tk6[_i] _tkthis) (setq _i (iadd _i 1)) )) (setq _ip (isub _ip 1)) (if (> _i 0) true (setq _ip _ip5)))
                           (begin (setq _ruleCount (+ _ruleCount 1)) (if (and (> _verboseTrigger 0) (> _ruleCount _verboseTrigger) (not _verbose)) (_startLog)) (setq _ip6 _ip)
                             ;; *********************************************************
                             ;; RULE: MAIN: Digit+ Period Digit* Exponent Sign Digit+ << ($OUTPLUS $ch (number (append $1 $2 $3 $4 $5 $6)) Number: true NumValue: (number (append $1 $2 $3 $4 $5 $6))) >>
                             ;; *********************************************************
                             (if true
                              (begin
                                (setq _ret  (_tkOUTPLUS _tkch (number (append _tk1 _tk2 _tk3 _tk4 _tk5 _tk6)) Number: true NumValue: (number (append _tk1 _tk2 _tk3 _tk4 _tk5 _tk6))) )
                                (if _verbose
                                    (writeRule
                                         {MAIN: Digit+ Period Digit* Exponent Sign Digit+ << ($OUTPLUS $ch (number (append $1 $2 $3 $4 $5 $6)) Number: true NumValue: (number (append $1 $2 $3 $4 $5 $6))) >>}
                                         _ret  _tk0 _tk1 _tk2 _tk3 _tk4 _tk5 _tk6 #void #void #void #void))
(setq _indent (isub _indent 1))
                                (goto Skip:))
                             ) ; end case : _default
                           ) ; end begin
                         ) ; end case : Digit+
                       ) ; end begin
                     ) ; end case : Sign
                   ) ; end begin
                 ) ; end case : Exponent
                 ;; *********************************************************
                 ;; RULE: MAIN: Digit+ Period Digit* << ($OUTPLUS $ch (number (append $1 $2 $3)) Number: true  NumValue: (number (append $1 $2 $3))) >>
                 ;; *********************************************************
                 (if true
                  (begin
                    (setq _ip _ip3)
                    (setq _ret  (_tkOUTPLUS _tkch (number (append _tk1 _tk2 _tk3)) Number: true  NumValue: (number (append _tk1 _tk2 _tk3))) )
                    (if _verbose
                        (writeRule
                             {MAIN: Digit+ Period Digit* << ($OUTPLUS $ch (number (append $1 $2 $3)) Number: true  NumValue: (number (append $1 $2 $3))) >>}
                             _ret  _tk0 _tk1 _tk2 _tk3 #void #void #void #void #void #void #void))
(setq _indent (isub _indent 1))
                    (goto Skip:))
                 ) ; end case : _default
               ) ; end begin
             ) ; end case : Digit*
           ) ; end begin
         ) ; end case : Period
         ;; *********************************************************
         ;; RULE: MAIN: Digit+ << ($OUTPLUS $ch (integer $1) Number: true NumValue: (integer $1)) >>
         ;; *********************************************************
         (if true
          (begin
            (setq _ip _ip1)
            (setq _ret  (_tkOUTPLUS _tkch (integer _tk1) Number: true NumValue: (integer _tk1)) )
            (if _verbose
                (writeRule
                     {MAIN: Digit+ << ($OUTPLUS $ch (integer $1) Number: true NumValue: (integer $1)) >>}
                     _ret  _tk0 _tk1 #void #void #void #void #void #void #void #void #void))
(setq _indent (isub _indent 1))
            (goto Skip:))
         ) ; end case : _default
       ) ; end begin
     ) ; end case : Digit+
     ;; ====================
     ;; case: Period
     ;; ====================
     (if (begin (setq _ip _ip0)
      (if (and (<> (setq _tk1 $IN[(setq _ip (iadd _ip 1))]) #void) (= _LF_Period[_tk1] 1)) true (setq _ip _ip0)))
       (begin (setq _ruleCount (+ _ruleCount 1)) (if (and (> _verboseTrigger 0) (> _ruleCount _verboseTrigger) (not _verbose)) (_startLog)) (setq _ip1 _ip)
         ;; ====================
         ;; case: Digit+
         ;; ====================
         (if (begin (setq _i 0) (setq _tk2 (makeString {})) (while (and (<> (setq _tkthis $IN[(setq _ip (iadd _ip 1))]) #void) (= _LF_Digit[_tkthis] 1)) do (begin (setq _tk2[_i] _tkthis) (setq _i (iadd _i 1)) )) (setq _ip (isub _ip 1)) (if (> _i 0) true (setq _ip _ip1)))
           (begin (setq _ruleCount (+ _ruleCount 1)) (if (and (> _verboseTrigger 0) (> _ruleCount _verboseTrigger) (not _verbose)) (_startLog)) (setq _ip2 _ip)
             ;; *********************************************************
             ;; RULE: MAIN: Period Digit+ << ($OUTPLUS $ch (number (append $1 $2)) Number: true NumValue: (number (append $1 $2))) >>
             ;; *********************************************************
             (if true
              (begin
                (setq _ret  (_tkOUTPLUS _tkch (number (append _tk1 _tk2)) Number: true NumValue: (number (append _tk1 _tk2))) )
                (if _verbose
                    (writeRule
                         {MAIN: Period Digit+ << ($OUTPLUS $ch (number (append $1 $2)) Number: true NumValue: (number (append $1 $2))) >>}
                         _ret  _tk0 _tk1 _tk2 #void #void #void #void #void #void #void #void))
(setq _indent (isub _indent 1))
                (goto Skip:))
             ) ; end case : _default
           ) ; end begin
         ) ; end case : Digit+
       ) ; end begin
     ) ; end case : Period
     ;; ====================
     ;; case: Operator+
     ;; ====================
     (if (begin (setq _ip _ip0)
      (begin (setq _i 0) (setq _tk1 (makeString {})) (while (and (<> (setq _tkthis $IN[(setq _ip (iadd _ip 1))]) #void) (= _LF_Operator[_tkthis] 1)) do (begin (setq _tk1[_i] _tkthis) (setq _i (iadd _i 1)) )) (setq _ip (isub _ip 1)) (if (> _i 0) true (setq _ip _ip0))))
       (begin (setq _ruleCount (+ _ruleCount 1)) (if (and (> _verboseTrigger 0) (> _ruleCount _verboseTrigger) (not _verbose)) (_startLog)) (setq _ip1 _ip)
         ;; *********************************************************
         ;; RULE: MAIN: Operator+ << ($OUTPLUS $ch $1 Operator: true Special: true) >>
         ;; *********************************************************
         (if true
          (begin
            (setq _ret  (_tkOUTPLUS _tkch _tk1 Operator: true Special: true) )
            (if _verbose
                (writeRule
                     {MAIN: Operator+ << ($OUTPLUS $ch $1 Operator: true Special: true) >>}
                     _ret  _tk0 _tk1 #void #void #void #void #void #void #void #void #void))
(setq _indent (isub _indent 1))
            (goto Skip:))
         ) ; end case : _default
       ) ; end begin
     ) ; end case : Operator+
     ;; ====================
     ;; case: Whitespace+
     ;; ====================
     (if (begin (setq _ip _ip0)
      (begin (setq _i 0) (setq _tk1 (makeString {})) (while (and (<> (setq _tkthis $IN[(setq _ip (iadd _ip 1))]) #void) (= _LF_Whitespace[_tkthis] 1)) do (begin (setq _tk1[_i] _tkthis) (setq _i (iadd _i 1)) )) (setq _ip (isub _ip 1)) (if (> _i 0) true (setq _ip _ip0))))
       (begin (setq _ruleCount (+ _ruleCount 1)) (if (and (> _verboseTrigger 0) (> _ruleCount _verboseTrigger) (not _verbose)) (_startLog)) (setq _ip1 _ip)
         ;; *********************************************************
         ;; RULE: MAIN: Whitespace+ << true >>
         ;; *********************************************************
         (if true
          (begin
            (setq _ret  true )
            (if _verbose
                (writeRule
                     {MAIN: Whitespace+ << true >>}
                     _ret  _tk0 _tk1 #void #void #void #void #void #void #void #void #void))
(setq _indent (isub _indent 1))
            (goto Skip:))
         ) ; end case : _default
       ) ; end begin
     ) ; end case : Whitespace+
     ;; ====================
     ;; case: Any
     ;; ====================
     (if (begin (setq _ip _ip0)
      (if (<> (setq _tk1 $IN[(setq _ip (iadd _ip 1))]) #void) true (setq _ip _ip0)))
       (begin (setq _ruleCount (+ _ruleCount 1)) (if (and (> _verboseTrigger 0) (> _ruleCount _verboseTrigger) (not _verbose)) (_startLog)) (setq _ip1 _ip)
         ;; *********************************************************
         ;; RULE: MAIN: Any << ($OUTPLUS $ch (string $1) Special: true) >>
         ;; *********************************************************
         (if true
          (begin
            (setq _ret  (_tkOUTPLUS _tkch (string _tk1) Special: true) )
            (if _verbose
                (writeRule
                     {MAIN: Any << ($OUTPLUS $ch (string $1) Special: true) >>}
                     _ret  _tk0 _tk1 #void #void #void #void #void #void #void #void #void))
(setq _indent (isub _indent 1))
            (goto Skip:))
         ) ; end case : _default
       ) ; end begin
     ) ; end case : Any
     ;; ====================
     ;; case: Eof
     ;; ====================
     (if (begin (setq _ip _ip0)
      (if (= (setq _tk1 $IN[(setq _ip (iadd _ip 1))]) #void) true (setq _ip _ip0)))
       (begin (setq _ruleCount (+ _ruleCount 1)) (if (and (> _verboseTrigger 0) (> _ruleCount _verboseTrigger) (not _verbose)) (_startLog)) (setq _ip1 _ip)
         ;; *********************************************************
         ;; RULE: MAIN: Eof :: $LIST ::
         ;; *********************************************************
         (if true
          (begin
            (setq _ret  _tkLIST )
            (if _verbose
                (writeRule
                     {MAIN: Eof :: $LIST ::}
                     _ret  _tk0 _tk1 #void #void #void #void #void #void #void #void #void))
(setq _indent (isub _indent 1))
            (return _ret))
         ) ; end case : _default
       ) ; end begin
     ) ; end case : Eof
     ;; **************************************************
     ;; DEFAULT (if we get to here we failed to recognize 
     ;; Note:       (at least on this repetition)
     ;; **************************************************
     (begin
        (if (= _tkn 1)
            ;; If we have never recognized any token on previous repetitions 
            (begin
               (setq _ip _oldIp)     
               (if _verbose (_logLine "Rejecting Rule MAIN on: " input: ))
               (setq _indent (isub _indent 1))
               (setq _ret morphFail)
               ) ; end begin then
            else
            ;; If we have recognized some tokens on previous repetitions, then return $0
            ;; Note: In Syntax Rules, we do not fail the rule if it has 
            ;;       recognized a token in one or more repetitions of the rule. 
            (begin
               (setq _ip _ip0)
               (setq _ret _tk0)
               ) ; end begin else
            ) ; end if
        ) ; end of DEFAULT

   ;; End of tests for user defined rules.

   (if (> _verboseLexIn.MAIN -1)
       (begin
          (setq _verbose _verboseSave)
          (if (>= _verboseLexCount.MAIN _verboseLexIn.MAIN) (error "Count" "in Routine MAIN"))
       ))

   ;; Repeat or return to caller is handled here.
   _ret) ;; end _LEXRULE_MAIN

























;;**EXPORTKEY**:alice:queryScript:_SYNRULE_MAIN
;; ************************************************
;; MAIN user defined Syntax Rule implementation
;; Summary: This agent implements the MAIN
;;          user defined rule. Each rule test is
;;          marked with a boxed comment line for
;;          ease of human understanding.
;; Note: This code was machine generated by parseAgent.
;; ************************************************
(defchild alice.queryScript _SYNRULE_MAIN(...)
   ;; The token object _io is a persistent variable
   ;; The token pointer _ip is a persistent variable
   ;; All getToken logic increments _ip on each token fetch.
   ;; The return value from the rule is stored in _ret.
   ;; Remember that this rule may repeat depending on _repeatSW.
   vars:(_tk0 _tk1 _tk2 _tk3 _tk4  _tk5  _i
         _tk6 _tk7 _tk8 _tk9 _tkn  _repeatSW
         _ak0 _ak1 _ak2 _ak3 _ak4  _ak5  _tkthis
         _ak6 _ak7 _ak8 _ak9 _ret  _oldIp
         _ip0 _ip1 _ip2 _ip3 _ip4  _ip5
         _ip6 _ip7 _ip8 _ip9 _tkn  
         _verboseSave       
        ) ; end temporary variables
   ;; Collect any arguments which may have been passed to this rule
   (if (> (setq _tkn (argCount)) 0)
       (begin (setq _ak0 (argFetch 0))
          (if (> _tkn 1) 
              (begin (setq _ak1 (argFetch 1))
                 (if (> _tkn 2) 
                     (begin (setq _ak2 (argFetch 2))
                        (if (> _tkn 3) 
                            (begin (setq _ak3 (argFetch 3))
                               (if (> _tkn 4) 
                                   (begin (setq _ak4 (argFetch 4))
                                      (if (> _tkn 5) 
                                          (begin (setq _ak5 (argFetch 5))
                                             (if (> _tkn 6) 
                                                 (begin (setq _ak6 (argFetch 6))
                                                    (if (> _tkn 7) 
                                                        (begin (setq _ak7 (argFetch 7))
                                                           (if (> _tkn 8) 
                                                               (begin (setq _ak8 (argFetch 8))
                                                               (if (> _tkn 9) (setq _ak9 (argFetch 9))
                                                           ))))))))))))))))))) ; end argument collection
   ;; The default structure is given the named rule attribute
   (setq _tk0 (new Structure: MAIN: true))
   (setq _tkn 0)
   (setq _oldIp _ip)
   (setq _repeatSW true)

   (if (= _verboseSynCount.MAIN #void) 
          (setq _verboseSynCount.MAIN 1) 
          (setq _verboseSynCount.MAIN (iadd _verboseSynCount.MAIN 1)))

   (if (and (<> _verboseSynIn.MAIN #void) (> _verboseSynIn.MAIN -1))
       (begin (setq _verboseSave _verbose) (setq _verbose true))) 

   ;; Save the old token pointer and test for user defined rules
   Skip::
     (setq _indent (iadd _indent 1))
     (if _verbose (_logLine "Attempting MAIN Rule on: " source:))
     (setq _ip0 _ip)
     (++ _tkn)
     (setq _repeatSW false)

     ;; *****************************************
     ;; Begin testing for each user defined rule.
     ;; *****************************************






     ;; ====================
     ;; case: Value
     ;; ====================
     (if (if (<> (setq _tk1 (_getToken))[Value:] #void) true (setq _ip _ip0))
       (begin (setq _ruleCount (+ _ruleCount 1)) (if (and (> _verboseTrigger 0) (> _ruleCount _verboseTrigger) (not _verbose)) (_startLog)) (setq _ip1 _ip)
         ;; *********************************************************
         ;; RULE: MAIN: Value :: $LIST ::
         ;; *********************************************************
         (if true
          (begin
            (setq _ret  _tkLIST )
            (if _verbose 
                (writeRule
                     {MAIN: Value :: $LIST ::}
                     _ret  _tk0 _tk1 #void #void #void #void #void #void #void #void #void))
(setq _indent (isub _indent 1))
            (return _ret))
         ) ; end case : _default
       ) ; end begin
     ) ; end case : Value
     ;; ====================
     ;; case: Eof
     ;; ====================
     (if (begin (setq _ip _ip0)
      (if (_eofToken)true (setq _ip _ip0)))
       (begin (setq _ruleCount (+ _ruleCount 1)) (if (and (> _verboseTrigger 0) (> _ruleCount _verboseTrigger) (not _verbose)) (_startLog)) (setq _ip1 _ip)
         ;; *********************************************************
         ;; RULE: MAIN: Eof :: $LIST ::
         ;; *********************************************************
         (if true
          (begin
            (setq _ret  _tkLIST )
            (if _verbose 
                (writeRule
                     {MAIN: Eof :: $LIST ::}
                     _ret  _tk0 _tk1 #void #void #void #void #void #void #void #void #void))
(setq _indent (isub _indent 1))
            (return _ret))
         ) ; end case : _default
       ) ; end begin
     ) ; end case : Eof
     ;; **************************************************
     ;; DEFAULT (if we get to here we failed to recognize)
     ;; Note:       (at least on this repetition)
     ;; **************************************************
     (begin
        (if (= _tkn 1)
            ;; If we have never recognized any token on previous repetitions 
            (begin
               (setq _ip _oldIp)     
               (if _verbose (_logLine "Rejecting Rule MAIN on: " source:))
               (setq _indent (isub _indent 1))
               (setq _ret morphFail)
               ) ; end begin then
            else
            ;; If we have recognized some tokens on previous repetitions, then return $0
            ;; Note: In Syntax Rules, we do not fail the rule if it has 
            ;;       recognized a token in one or more repetitions of the rule. 
            (begin
               (setq _ip _ip0)
               (setq _ret _tk0)
               ) ; end begin else
            ) ; end if
        ) ; end of DEFAULT

   ;; End of tests for user defined rules.
   
   (if (> _verboseSynIn.MAIN -1)
       (begin
          (setq _verbose _verboseSave)
          (if (>= _verboseSynCount.MAIN _verboseSynIn.MAIN) (error "Count" "in Routine MAIN"))
       ))

   ;; Repeat or return to caller is handled here.
   _ret) ;; end _SYNRULE_MAIN
























