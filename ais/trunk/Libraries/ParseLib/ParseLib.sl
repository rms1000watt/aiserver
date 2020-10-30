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

;;**EXPORTKEY**:ParseLib
(defun ParseLib(cabinetName _input)
;; ********************************************************************
;; summary:  This Lambda converts a compiler definition into a generated
;;           Lambda, which is a compiler or translator for the language
;;           specified in the compiler definition. The compiler definition
;;           language may be described as a "meta language" with which
;;           to describe compilers, and is capable of describing, not
;;           only the formal syntax of the language to be compiled, but
;;           also the semantics, code optimization, and code generation
;;           required from the compiler.
;; Notes:    Requires the browseLib, and that the compiler definition
;;           source be checked into the file cabinet.
;; Parms:    cabinetName   The name of the compiler definition file cabinet
;; Parms:    _input        The name of the compiler definition language source
;; return:   _result       The name of the generated compiler as checked into
;;                         the file cabinet.
;; ********************************************************************
   pvars:(;; Persistent Variables
          _cabinetName			;; The name of the compiler definition file cabinet
          _delimitedStrings        ;; Delimited strings supplied in the compiler definition
          _friendCompiler          ;; True if the compiler is to be generater as a friend
          _ip                      ;; Current parse tree pointer
          _leftRecursionFixup      ;; Convert rules dictionaries from left to right recursion
          _lexicalFeatureNames     ;; Lexical feature names supplied in the compiler definition
          _lexicalFeatures         ;; Lexical features supplied in the compiler definition
          _lexicalRuleDictionary   ;; Current dictionary of Lexical Rule definitions
          _name                    ;; Name of the current compiler definition (prepared for browseLib)
          _nameDef                 ;; Name of the current compiler definition (prepared for definitions)
          _parseLen                ;; Current parse tree length
          _parseTree               ;; Current head of the parse tree during recognition
          _result                  ;; Final result of parsing the submitted compiler definition rules. 
          _semanticRuleDictionary  ;; Current dictionary of Semantic Rule definitions
          _syntaxRuleDictionary    ;; Current dictionary of Syntax Rule definitions
          _rulePrefix              ;; Current prefix for rule definitions
          rulesDirectory           ;; Special tool for holding rules
          _semanticPasses          ;; Requested semantic rule passes
          _showTokens              ;; Switch for displaying the lexed tokens on the console
          _syntaxFeatures          ;; Syntax features supplied in the compiler definition
          tokenDirectory           ;; Lexicon of token and their attributes
          _userFunctions           ;; User functions source code supplied in the compiler definition
          _verbose                 ;; Switch for displaying each explanation step on the console
          ;; Methods list 
          appendList               ;; Append multiple arguments into a list
          defaultLexer             ;; The lexical analyzer for recognizing input symbols
          defaultTokenRule         ;; Modified default rule for adding attributes to a parsed token
          _getToken                ;; Get the next attributed token in the parse tree
          _Initialize              ;; Initialization routine for setting token dicrectory, etc.
          outputRule               ;; Default rule for returning the final output from the compiler
          _parse                   ;; Convert an input string into a token stream parse tree
          precompiler              ;; Precompiler for adding C-like precompiler directives to source files 
          _setSyntaxFeature        ;; Set a whole class of tokens with the specified attribute (also compiler runtime)
          startRule                ;; Default rule for starting the compiler
          ;; Methods for Lexical Rules 
          _lexical_outputARule     ;; Output a single Lexical Rule as a child Lambda
          _lexical_outputRules     ;; Output all Lexical Rules as child Lambdas
          _lexical_saveARule       ;; Save a single Lexical Rule in a rule dictionary
          _lexical_saveRules       ;; Save all Lexical Rules in the rule dictionary
          ;; Methods for Semantic Rules 
          _semantic_outputARule    ;; Output a single Semantic Rule as a child Lambda
          _semantic_outputRules    ;; Output all Semantic Rules as child Lambdas
          _semantic_saveARule      ;; Save a single Semantic Rule in a rule dictionary
          _semantic_saveRules      ;; Save all Semantic Rules in the rule dictionary
          ;; Methods for Syntax Rules 
          _syntax_outputARule      ;; Output a single Syntax Rule as a child Lambda
          _syntax_outputRules      ;; Output all Syntax Rules as child Lambdas
          _syntax_saveARule        ;; Save a single Syntax Rule in a rule dictionary
          _syntax_saveRules        ;; Save all Syntax Rules in the rule dictionary
          ;; User Defined Recognition Rules
          MAIN
          LEXICALFEATURES
          LEXICALRULES
          SEMANTICRULES
          SYNTAXFEATURES
          SYNTAXRULES
          ) ;; end of persistent variables
   vars:(i)
   ;; ***************************************************
   ;; Define the child Lambdas which belong to this parent
   ;; ***************************************************
   ;; Append multiple arguments into a list.
   ;; Note: This Lambda is here as a builtin function for
   ;;       use in the output section of any rule definition. 
   (defun appendList(one two)
       vars:(result)
       (cond 
           ((and (isPair one) (isPair two)) (setq result (append one two)))
           ((= one #void) (setq result two))
           ((and (isPair one) (= two #void)) (setq result one))
           ((isPair one) (setq result (append one (list two))))
           ((= two #void) (setq result (list one)))
           (else (setq result (list one two)))
           ) ; end cond
       result) ;; end appendList
   ;; Modified default rule for adding attributes to a parsed token.
   ;; Note: This Lambda is here in case the user does not 
   ;;       define one of his/her own.
   (defun defaultTokenRule(token)
       vars:(result tokenLen tokenEnd)
       ;; Is this token a delimited constant?
       (if (isVector token)
           (begin
              (setq result (new Structure: Value: token[1] token[0] true  Constant: true))
              ;; Make sure Repeat delimited strings are also action rules
              (if (= token[0] Repeat:) (setq result.ActionRule true))
              (return result)
              )) ;; end if delimited constant
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
   ;; Get the next attributed token in the parse tree.
   ;; Note: This Lambda skips comment tokens.
   (defun _getToken()
       vars:(_tk0)
       ;; Search for the User Functions section
       (while (< _ip _parseLen) do
          (if (<> (setq _tk0 _parseTree[_ip])[Comment:] true)
              (begin
                 (++ _ip)
                 (return _tk0)
                 )) ; end if
          (++ _ip)
          ) ; end scan tokens while
       ;; If at end of parse tree, return EOF attribute
       #{Eof: true}) ;; end of _getToken
   ;; Initialization routine for setting token dicrectory, etc.
   ;; Note: This Lambda is run once during the lifetime of the
   ;;       parent Lambda.
   (defun _Initialize()
       ;; Create the token directory for the compiler definition language.
       (setq tokenDirectory (new Directory:))
       ;; Adjust the lexical analyzer for the compiler definition language.
       (defaultLexer._Initialize)
       (defaultLexer.addStringDelimiters String: {"} {"})
       (defaultLexer.addStringDelimiters UserFunctions: {#UserFunctions#} {#End#})
       (defaultLexer.addStringDelimiters DelimitedStrings: {#DelimitedStrings#} {#End#})
       (defaultLexer.addStringDelimiters SemanticPasses: {#SemanticPasses#} {#End#})
       (defaultLexer.addStringDelimiters ActionRule: {::} {::})
       (defaultLexer.addStringDelimiters Repeat: {<<} {>>})
	   (defaultLexer.addStringDelimiters SubRule: {&&} {&&})
       (defaultLexer.addStringDelimiters ConditionRule: {||} {||})
       (defaultLexer.addStringDelimiters ArgumentRule: {(} {)})
       (defaultLexer.addStringDelimiters UserCondition: "{" "}")
       (defaultLexer.addStringDelimiters Whitespace: ";" _eol)
       true) ;; end _Initialize
   ;; Default rule for returning the final output from the compiler.
   ;; Note: This Lambda checks its results into the file cabinet 
   ;;       under the name of the compiler definition.
   (defun outputRule(result)
       vars:(i ruleLen)
       ;; Compose and add rule names to the main compiler header
       (setq result (mid (browseLib.checkout "ParseLib:%COMPILER_HEADER") (addi 7 (length _eol)) 100000))
       (setq result (substitute result "#??#" _name))
       (setq result (substitute result "@??@" _nameDef))
       (if (= _friendCompiler true)
           (setq result (substitute result "@defun@" "defriend"))
           (setq result (substitute result "@defun@" "defun"))
           ) ; end if
       (setq result (append result "          ;; Variables to hold lexical feature bit maps" _eol))
       (setq ruleLen (length _lexicalFeatureNames))
       (loop for i from 0 until ruleLen do
           (setq result (append result "          _LF_" _lexicalFeatureNames[i] _eol))
           ) ; end loop
       (setq result (append result "          ;; Functions to implement Lexical Rules" _eol))
       (setq ruleLen (length _lexicalRuleDictionary))
       (loop for i from 0 until ruleLen do
           (setq result (append result "          _LEXRULE_" _lexicalRuleDictionary[i 0] _eol))
           ) ; end loop
       (setq result (append result "          ;; Functions to implement Syntax Rules" _eol))
       (setq ruleLen (length _syntaxRuleDictionary))
       (loop for i from 0 until ruleLen do
           (setq result (append result "          _SYNRULE_" _syntaxRuleDictionary[i 0] _eol))
           ) ; end loop
       (setq result (append result "          ;; Functions to implement Semantic Rules" _eol))
       (setq ruleLen (length _semanticRuleDictionary))
       (loop for i from 0 until ruleLen do
           (setq result (append result "          _SEMRULE_" _semanticRuleDictionary[i 0] _eol))
           ) ; end loop
       ;; Compose and checkin the main compiler function
       (setq result (append result 
                       (mid (browseLib.checkout "ParseLib:%COMPILER_INITIALIZE") (addi 7 (length _eol)) 1000000)))
       (setq result (append result _delimitedStrings))
       (setq result (append result _lexicalFeatures))
       (setq result (append result _syntaxFeatures))
       (setq result (append result 
                       (mid (browseLib.checkout "ParseLib:%COMPILER_MAINCODE") (addi 7 (length _eol)) 1000000)))
       (setq result (append result _semanticPasses))
       (setq result (append result 
                       (mid (browseLib.checkout "ParseLib:%COMPILER_FINALCODE") (addi 7 (length _eol)) 1000000)))
       (browseLib.checkin _cabinetName (symbol _name) result)
       ;; Compose and checkin the default lexer for the compiler
       (setq result (mid (browseLib.checkout "ParseLib:%COMPILER_LEXER") 9 100000))
       (if (= _friendCompiler true)
           (setq result (substitute result "#??#" (append _nameDef " ")))
           (setq result (substitute result "#??#" (append _nameDef ":")))
           ) ; end if
       (browseLib.checkin _cabinetName (symbol (append _name ":@@defaultLexer")) result)
       ;; Compose and checkin the user defined functions for the compiler
       (setq result (mid (browseLib.checkout "ParseLib:%COMPILER_FUNCTIONS") (addi 7 (length _eol)) 100000))
       (setq result (substitute result "#??#" _nameDef))
       (setq result (append result _userFunctions))
       (browseLib.checkin _cabinetName (symbol (append _name ":@UserDefinedFunctions")) result)
       ;; Output the Lexical Rules as child Lambdas
       (_lexical_outputRules)
       ;; Output the Syntax Rules as child Lambdas
       (_syntax_outputRules)
       ;; Output the Semantic Rules as child Lambdas
       (_semantic_outputRules)
       ;; Compile the final output using the browseLib
       ;; Note: Compilation has been turned off until we can fix
       ;;       the intermittent macro !Missing function name error in the engine.
       (browseLib.compileSource (string _name))
       true) ;; end outputRule
   ;; Convert an input string into a token stream parse tree.
   (defun _parse(inString)
      vars:(parseTree treeIndex treeLen token tokenAttr)
      ;; This Lambda tests compiled FSM style methods of attributing each parsed token.
      (setq parseTree (defaultLexer inString))
      (setq treeLen (length parseTree))
      (loop for treeIndex from 0 until treeLen do
          (setq token parseTree[treeIndex])
          (setq tokenAttr tokenDirectory[token])
          (if (= tokenAttr #void)
              then
              ;; Create an attributed token using default rule
              (setq tokenAttr (defaultTokenRule token))
              else
              ;; Copy the attributes and set the Value from the dictionary
              (begin
                 (setq tokenAttr (copy tokenAttr))
                 (setq tokenAttr.Value token))
              ) ; end if
          (setq parseTree[treeIndex] tokenAttr)
          ) ;; end loop
      parseTree) ;; end of _parse
   ;; Set a whole set of syntax tokens with the specified attribute
   (defun _setSyntaxFeature(feature words)
       vars:(i wordLen token WORD)
       (setq wordLen (length words))
       (loop for i from 0 until wordLen do
           (setq WORD (symbol words[i]))
           (setq token tokenDirectory[WORD])
           (if (= token #void) (setq token (new Structure:)))
           (setq token[feature] true)
           (setq token.Value WORD)
           (setq tokenDirectory[WORD] token)
           ) ; end loop
       true) ; end _setSyntaxFeature
   ;; Default rule for starting the compiler.
   ;; Note: This Lambda is here in case the user does not 
   ;;       define one of his/her own.
   (defun startRule()
       true) ;; end startRule
   ;; ************************************************
   ;; Define the main entry code for this parent Lambda
   ;; ************************************************
   ;; Initialize the parent Lambda once and only once.
   (if (= tokenDirectory #void) (_Initialize))
   (setq _cabinetName (symbol cabinetName))
   ;; Are we generating a friend compiler?
   (setq _friendCompiler (isNumber (find ":" _input))) 
   ;; Save the name of and retrieve the source of the compiler definition.
   ;(writeln "Starting ParseLib on: " _input)
   (setq _nameDef (substitute _input ":" "."))
   (setq _name (symbol _input))
   (setq _input (browseLib.checkout _cabinetName (symbol (append _name ":%DEFINITION"))))
   (setq _input (precompiler _input))
   ;; In verbose mode, display the source string contents
   ;(if _verbose (writeln _eol "*********Pre-Lex Definition Source*********" _eol _input))
   ;; Run the user defined start rule.
   (startRule)
   ;; Create the attributed token stream parse tree.
   (setq _parseTree (_parse _input))
   ;; In verbose mode, display the parse tree contents
   (if (or _verbose _showTokens)
      (begin
         (writeln _eol "*********Post-Lex Parsed Tokens with Features*********")
         (loop for i from 0 until (length _parseTree) do
             (writeln "[" i "] " _parseTree[i])
             ) ; end loop
         )) ; end if
   ;; Run the main recognition rule.
   (setq _ip 0)
   (setq _lexicalFeatureNames (new Vector: 0))
   (setq _lexicalRuleDictionary (new Dictionary:))
   (setq _syntaxRuleDictionary (new Dictionary:))
   (setq _semanticRuleDictionary (new Dictionary:))
   (setq _userFunctions _eol)
   (setq _delimitedStrings (append "       ;; Initialization of Delimited Strings" _eol))
   (setq _lexicalFeatures (append "         ;; Initialization of Lexical Features" _eol))
   (setq _syntaxFeatures (append "         ;; Initialization of Syntax Features" _eol))
   (setq _semanticPasses   (append "   ;; Perform semantic passes (if any)" _eol))
   (setq _parseLen (length _parseTree))
   (setq _result (MAIN))
   ;; Return the final output by invoking the user defined output rule.
   (outputRule _result)) ;; end of ParseLib




;;**EXPORTKEY**:ParseLib:%COMPILER_FINALCODE
;#text#
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












;;**EXPORTKEY**:ParseLib:%COMPILER_FUNCTIONS
;#text#
;; ************************************************
;; #??# User defined functions
;; ************************************************











;;**EXPORTKEY**:ParseLib:%COMPILER_HEADER
;#text#
(@defun@ #??#(_input)
;; ********************************************************************
;; summary:  The @??@ compiler generated from #??#:DEFINITION.
;; Summary:  This Lambda implements the @??@ compiler as defined
;;           in the #??#:DEFINITION compiler definition file.
;;           Much code has been marked with a boxed comment lines for
;;           ease of human understanding.
;; Note:     This code was machine generated by ParseLib.
;; Parms:    _input   The @??@ language source string
;; return:   _result  The Lambda resulting from compiling the _input source.
;; Modification history:
;; TM Jan 15 99 Added Console Error suppression (see _consoleError, _makeError and _lastError)
;; TM Jan 20 99 Added _verboseLexIn - a directory of routines to be _verbose in
;; TM Jan 20 99 Added _verboseSynIn - a directory of routines to be _verbose in
;; TM Jan 20 99 Added _verboseSemIn - a directory of routines to be _verbose in
;; TM Jan 20 99 Changes _verbosexxxIn so that you supply a stop count
;;              example: (setq #??#._verboseSynIn.MYRULE 2})  ; error after 2nd pass
;;                       (setq #??#._verboseSynIn.MYRULE 0})  ; verbose on every pass
;;                       (setq #??#._verboseSynIn.MYRULE: -1}) ; not verbose
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
          _semanticStack           ;; Current semantic stack for use by _applyRule 
          _semanticVerbose         ;; Switch for displaying each semantic explanation step on the console 
          _showTokens              ;; Show only the token list resulting from the lexical analyzer
          _syntaxFeatures          ;; Syntax features supplied in the compiler definition
          _tkIN                    ;; Place holder for the input source string (see $IN) 
          _tkLIST                  ;; The output token list from the lexer rules.
          _tkOUT                   ;; Output a feature based token to the token list.
          tokenDirectory           ;; Lexicon of token and their attributes
          _userFunctions           ;; User functions source code supplied in the compiler definition
          _verbose                 ;; Switch for displaying each explanation step on the console
          _verboseHold            ;; Switch for displaying each explanation step on the console
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












;;**EXPORTKEY**:ParseLib:%COMPILER_INITIALIZE
;#text#
         ) ;; end of persistent variables
   vars:(i verboseHold outString outExplain)
   ;; ***************************************************
   ;; Define the child Lambdas which belong to this parent
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
      (loop for i from 0 until l do
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
      (loop for i from 0 until j do
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
   ;; Note: This Lambda is here as a builtin function for
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
   ;; Note: This Lambda is here as a builtin function for
   ;;       use in the output section of any rule definition. 
   (defun _apply(theRule multiplePass)
       vars:(outList outString outExplain)
       ;(if _semanticVerbose (setq outString (append "Replacing: " (string _result true) " ==> ")))
       ;(if _explainOnOff (setq outExplain (setq outExplain (append "Replacing: " (string _result true) " ==> "))))
       (setq outList _result)
       (setq _passCount 0)
       (setq _semanticRule theRule)
       Retry::
       (if (> _passCount _maxPasses) (_error "ParseLib_Pass" "Exceeded maximum number of apply rules."))
       (setq _changeCount 0)
       (setq outList (morph (list outList) _applyRule morphFail))
       (if (isPair outList) (setq outList (car outList)))
       (if (and (> _changeCount 0) (= multiplePass true) (isPair outList)) (goto Retry:))
       ;(if (= _semanticVerbose true) (writeln outString  (string outList true)))
       ;(if _explainOnOff (setq _explanation (append _explanation outExplain (string _result true) _eol)))
       (setq _result outList)
       _result) ;; end of _apply
   ;; Apply the current semantic rule to a sub list
   ;; Note: This Lambda is called by morph for every sub list
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
   ;; Note: This Lambda is here in case the user does not 
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
   ;; Note: This Lambda is run once during the lifetime of the
   ;;       parent Lambda.
   (defun _Initialize()
       (setq _initializeSW true)
       ;; Reset the verbose mode indent.
       (setq _indent 0)
       ;; Create the token directory for the compiler definition language.
       (setq tokenDirectory (new Directory:))
       ;; Adjust the lexical analyzer for the compiler definition language.
       (defaultLexer._Initialize)












;;**EXPORTKEY**:ParseLib:%COMPILER_LEXER
;#text#
 (defriend #??#defaultLexer(inString)
;; ********************************************************************
;; summary:  This Lambda converts an input string into a vector of
;;           recognized lexemes. It is the default lexical analyzer
;;           for the ParseLib compiler generator.
;;           This Lambda may be modified, in any way, by the user.
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
    ;;  Define the child Lambdas for this parent.
    ;;************************************************************************
    ;; Add a named pair of string delimiters to the lexical analyzer.
    (defun addStringDelimiters(name start end)
       vars:(tmpLambda)
       ;;  Initialize the ParseLib once and only once.
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
        ;; This Lambda tests compiled FSM style methods of attributing each parsed token.
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
    ;;  Initialize the ParseLib once and only once.
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





















;;**EXPORTKEY**:ParseLib:%COMPILER_MAINCODE
;#text#
       ;; Call the user defined initialization routine
       (initRule)
       true) ;; end _Initialize
   ;; Default rule for user defined compiler initialization tasks.
   ;; Note: This Lambda is here in case the user does not 
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
   ;; Note: This Lambda is here in case the user does not 
   ;;       define one of his/her own.
   (defun _LEXRULE_MAIN()
       (defaultLexer $IN)) ;; end _LEXRULE_MAIN
   ;; Move the current parse tree index to the next position.
   (defun _nextIp()
       (if (isNumber _ip) (return (setq _ip (addi _ip 1))))
       (setq _ip[(subi (length _ip) 1)] (addi _ip[(sub1 (length _ip))] 1))
       _ip[(subi (length _ip) 1)]) ;; end _nextIp
   ;; Default rule for returning the final output from the compiler.
   ;; Note: This Lambda is here in case the user does not 
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
   ;; Note: This Lambda is here in case the user does not 
   ;;       define one of his/her own.
   (defun startRule()
       true) ;; end startRule
   ;; Default main Syntax Rule for starting the compiler.
   ;; Note: This Lambda is here in case the user does not 
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

	(defun _substitute(begIp endIp newItem)
		vars:(n N)
		(setq _ip (- begIp 1))
		(setq N (+ (- endIp begIp) 1))
		(setq _parseTree[begIp] newItem)
		(++ begIp)
		(loop for n from 1 until N do
			(delete _parseTree begIp)
		);n
		true)

   ;; ************************************************
   ;; Define the main entry code for this parent Lambda
   ;; ************************************************
   ;; ************************************************
   ;; Define the main entry code for this parent Lambda
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
   ;; Initialize the parent Lambda once and only once.
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




;;**EXPORTKEY**:ParseLib:%LEXICAL_RULEHEADER
;#text#
;; ************************************************
;; #%%# user defined Lexical Rule implementation
;; Summary: This Lambda implements the #%%#
;;          user defined rule. Each rule test is
;;          marked with a boxed comment line for
;;          ease of human understanding.
;; Note: This code was machine generated by ParseLib.
;; ************************************************
(defchild #??##!!#(...)
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

   (if (= _verboseLexCount.#%%# #void) 
          (setq _verboseLexCount.#%%# 1) 
          (setq _verboseLexCount.#%%# (iadd _verboseLexCount.#%%# 1)))

   (if (and (<> _verboseLexIn.#%%# #void) (> _verboseLexIn.#%%# -1))
       (begin (setq _verboseSave _verbose) (setq _verbose true))) 

   ;; Save the old token pointer and test for user defined rules
   Skip::
     (setq _indent (iadd _indent 1))
     (if _verbose (_logLine "Attempting #%%# Rule on: " input:))
     (setq _ip0 _ip)
     (setq _tkch (iadd _ip 1))
     (++ _tkn)
     (setq _repeatSW false)

     ;; *****************************************
     ;; Begin testing for each user defined rule.
     ;; *****************************************





















;;**EXPORTKEY**:ParseLib:%LEXICAL_RULETRAILER
;#text#
     ;; **************************************************
     ;; DEFAULT (if we get to here we failed to recognize 
     ;; Note:       (at least on this repetition)
     ;; **************************************************
     (begin
        (if (= _tkn 1)
            ;; If we have never recognized any token on previous repetitions 
            (begin
               (setq _ip _oldIp)     
               (if _verbose (_logLine "Rejecting Rule #%%# on: " input: ))
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

   (if (> _verboseLexIn.#%%# -1)
       (begin
          (setq _verbose _verboseSave)
          (if (>= _verboseLexCount.#%%# _verboseLexIn.#%%#) (error "Count" "in Routine #%%#"))
       ))

   ;; Repeat or return to caller is handled here.
   _ret) ;; end #!!#





















;;**EXPORTKEY**:ParseLib:%SEMANTIC_RULEHEADER
;#text#
;; ************************************************
;; #%%# user defined Semantic Rule implementation
;; Summary: This Lambda implements the #%%#
;;          user defined rule. Each rule test is
;;          marked with a boxed comment line for
;;          ease of human understanding.
;; Note: This code was machine generated by ParseLib.
;; ************************************************
(defchild #??##!!#(...)
   ;; The token object _io is a persistent variable
   ;; The token pointer _ip is a persistent variable
   ;; All getToken logic increments _ip on each token fetch.
   ;; The return value from the rule is stored in _ret.
   ;; Remember that this rule may repeat depending on _repeatSW.
   vars:(_tk0 _tk1 _tk2 _tk3 _tk4  _tk5 _i
         _tk6 _tk7 _tk8 _tk9 _tkn  _repeatSW
         _ak0 _ak1 _ak2 _ak3 _ak4  _ak5 _tkthis
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
   (setq _tk0 (new Structure: #%%#: true))
   (setq _tkn 0)
   (setq _oldIp (copy _ip))
   (setq _repeatSW true)

   (if (= _verboseSemCount.#%%# #void) 
          (setq _verboseSemCount.#%%# 1) 
          (setq _verboseSemCount.#%%# (iadd _verboseSemCount.#%%# 1)))

   (if (and (<> _verboseSemIn.#%%# #void) (> _verboseSemIn.#%%# -1))
       (begin (setq _verboseSave _verbose) (setq _verbose true))) 

   ;; Save the old token pointer and test for user defined rules
   Skip::         
     (setq _indent (iadd _indent 1))
     (if _verbose (_logLine "Attempting #%%# Rule on: " source:))
     (setq _ip0 (copy _ip))
     (++ _tkn)
     (setq _repeatSW false)

     ;; *****************************************
     ;; Begin testing for each user defined rule.
     ;; *****************************************











;;**EXPORTKEY**:ParseLib:%SEMANTIC_RULETRAILER
;#text#
     ;; **************************************************
     ;; DEFAULT (if we get to here we failed to recognize)
     ;; Note:       (at least on this repetition)
     ;; **************************************************
     (begin
        (if (= _tkn 1)
            ;; If we have never recognized any token on previous repetitions 
            (begin
               (setq _ip _oldIp)     
               (if _verbose (_logLine "Rejecting Rule #%%# on: " source:))
               (setq _indent (isub _indent 1))
               (setq _ret morphFail)
               ) ; end begin then
            else
            ;; If we have recognized some tokens on previous repetitions, then return $0
            ;; Note: In Semantic Rules, we do not fail the rule if it has 
            ;;       recognized a token in one or more repetitions of the rule. 
            (begin
               (setq _ip _ip0)
               (setq _ret _tk0)
               ) ; end begin else
            ) ; end if
        ) ; end of DEFAULT

   ;; End of tests for user defined rules.

   (if (> _verboseSemIn.#%%# -1)
       (begin
          (setq _verbose _verboseSave)
          (if (>= _verboseSemCount.#%%# _verboseSemIn.#%%#) (error "Count" "in Routine #%%#"))
       ))
   ;; Repeat or return to caller is handled here.
   _ret) ;; end #!!#












;;**EXPORTKEY**:ParseLib:%SYNTAX_RULEHEADER
;#text#
;; ************************************************
;; #%%# user defined Syntax Rule implementation
;; Summary: This Lambda implements the #%%#
;;          user defined rule. Each rule test is
;;          marked with a boxed comment line for
;;          ease of human understanding.
;; Note: This code was machine generated by ParseLib.
;; ************************************************
(defchild #??##!!#(...)
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
   (setq _tk0 (new Structure: #%%#: true  ))
   (setq _tkn 0)
   (setq _oldIp _ip)
   (setq _repeatSW true)

   (if (= _verboseSynCount.#%%# #void) 
          (setq _verboseSynCount.#%%# 1) 
          (setq _verboseSynCount.#%%# (iadd _verboseSynCount.#%%# 1)))

   (if (and (<> _verboseSynIn.#%%# #void) (> _verboseSynIn.#%%# -1))
       (begin (setq _verboseSave _verbose) (setq _verbose true))) 

   ;; Save the old token pointer and test for user defined rules
   Skip::
     (setq _indent (iadd _indent 1))
     (if _verbose (_logLine "Attempting #%%# Rule on: " source:))
     (setq _ip0 _ip)
     (++ _tkn)
     (setq _repeatSW false)

     ;; *****************************************
     ;; Begin testing for each user defined rule.
     ;; *****************************************




;;**EXPORTKEY**:ParseLib:%SYNTAX_RULETRAILER
;#text#
     ;; **************************************************
     ;; DEFAULT (if we get to here we failed to recognize)
     ;; Note:       (at least on this repetition)
     ;; **************************************************
     (begin
        (if (= _tkn 1)
            ;; If we have never recognized any token on previous repetitions 
            (begin
               (setq _ip _oldIp)     
               (if _verbose (_logLine "Rejecting Rule #%%# on: " source:))
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
   
   (if (> _verboseSynIn.#%%# -1)
       (begin
          (setq _verbose _verboseSave)
          (if (>= _verboseSynCount.#%%# _verboseSynIn.#%%#) (error "Count" "in Routine #%%#"))
       ))

   ;; Repeat or return to caller is handled here.
   _ret) ;; end #!!#





















;;**EXPORTKEY**:ParseLib:LEXICALFEATURES
(defriend ParseLib:LEXICALFEATURES()
;; *******************************************************************
;; summary:  This Lambda recognizes lexical feature definitions.
;;           Lexical feature definitions appear as follows:
;;
;;           Digit: [ |"0"-"9"|]
;;           Letter: [|a-z| |A-Z|]
;;           Alphanum: [|a-z| |A-Z| |"0"-"9"|]
;;           CharData: [|1-255| ~ < >]
;;
;; Notes:    Each lexical feature definition is translated into a
;;           call to the setLexicalFeature function in the new compiler. 
;;
;; Parms:    none
;; return:   _tk0    The current token resulting after recognition.
;; *******************************************************************
    vars:(_tk0 _tk1 _tk2 _tk3 _tk4 start end letters value name)
    ;; *******************************************************************
    ;; Define child Lambdas
    ;; *******************************************************************
    (defun cnvToInt(value)
        (cond
           ((isSymbol value) (integer value[0]))
           ((isString value) (integer value[1]))
           ((isNumber value) (integer value))
           (else             (integer (symbol value[0])))
            ) ; end cond
         ) ; end cnvToInt
    ;; *******************************************************************
    ;; Start main processing
    ;; *******************************************************************
    ;; Search for each feature definition section
    Retry::
    (setq _tk1 (_getToken))
    (setq _tk2 (_getToken))
    (setq _tk3 (_getToken))
    ;; Recognize each feature definition section
    (cond
       ;; Recognize feature Definition Section
       ((and (= _tk1.Name true)
             (= _tk2.Value |:|:)
             (= _tk3.Value |[|:))
        (begin
           (setq name _tk1.Value)
           (setq letters "#(")
           (setq value 1)
           ;; Collect the letters to be assigned to this feature
           (while (and (<> (setq _tk1 (_getToken))[Eof:] true)
                       (<> _tk1.Value |]|:)) do
              (cond
                 ((= _tk1.Value "|")
                  (begin
                     (setq _tk1 (_getToken))
                     (setq _tk2 (_getToken))
                     (setq _tk3 (_getToken))
                     (setq _tk4 (_getToken))
                     (if (or (<> _tk2.Value "-") (<> _tk4.Value "|"))
                         (error (append "Invalid lexcial feature range: | " 
                                                                          " " _tk1.Value
                                                                          " " _tk2.Value
                                                                          " " _tk3.Value
                                                                          " " _tk4.Value))
                         ) ; end if
                     (setq letters (append letters " " value " " (cnvToInt _tk1.Value) " " (cnvToInt _tk3.Value)))
                     )) ; end range
                 ((= _tk1.Value "~") 
                  (setq value 0))
                 (else 
                  (setq letters (append letters " " value " " (cnvToInt _tk1.Value) " " (cnvToInt _tk1.Value))))
                  ) ; end cond
              ) ; end while
           ;; Record the lexical feature persistent variable name.
           (setq _lexicalFeatureNames[(length _lexicalFeatureNames)] name)
           ;; Output the feature assignment source code
           (setq name (append "_LF_" name))
           (setq _lexicalFeatures 
                    (append _lexicalFeatures 
                                "       (setq " name " (_setLexicalFeature " name " " letters ")))" _eol))
           )) ; end Feature Definition Section
       ;; Recognize end of section
       ((and (= _tk1.Value |#|:)
             (= _tk2.Value End:)
             (= _tk3.Value |#|:))
        (return _tk0)) ; end Recognize end of section
       ;; Recognize end of file
       ((= _tk1.Eof true)
        (error "Unexpected end of Lexical Features Section")) ; end Recognize end of file
       ) ; end cond
    (goto Retry:)
    _tk0) ;; end of LEXICALFEATURES





















;;**EXPORTKEY**:ParseLib:LEXICALRULES
(defriend ParseLib:LEXICALRULES()
;; *******************************************************************
;; summary:  This Lambda recognizes lexical rules definitions.
;; Parms:    none
;; return:   _tk0    The current token resulting after recognition.
;; *******************************************************************
   vars:(_tk0 _tk1 _tk2 _tk3 words name action condition lastWord n directSW)
   ;; Search for each rule definition section
   Retry::
   (setq _tk1 (_getToken))
   (setq _tk2 (_getToken))
   ;; Recognize each rule definition section
   (cond
      ;; Recognize rule Definition name
      ((and (= _tk1.Name true)
            (or (= _tk2.Value ":") (= _tk2.Value "|")))
       (begin
          (if (= _tk2.Value ":") (setq directSW false) (setq directSW true))
          (setq name _tk1.Value)
          (if (not (isCharUppercase name)) 
              (error (append "Rule " name " is not upper case") (append "Rule " name " is not upper case")))
          (setq words (new Vector: 0))
          ;; Collect the words to be used in this rule definition
          (while (and (<> (setq _tk1 (_getToken))[Eof:] true)
                      (<> _tk1.ActionRule true)
                      (<> _tk1.ConditionRule true)) do
             ;; Collect the term, user conditions, and arguments for this user defined rule
             (cond
                ;; Arguments can be passed to user defined rules.
                ((= _tk1.ArgumentRule true)
                 (begin
                    ;; Arguments can only be passed to user defined rules.
                    (setq lastWord (subi (length words) 1))
                    (if (or (< lastWord 0) (not (isCharUppercase words[lastWord])))
                        (error (append "Rule: " 
                                       name " attempted to pass arguments "
                                       _tk1.Value " to this non user defined rule: " words[lastWord]))
                        ) ; end if error
                    ;; Reconstruct the user defined rule to include argument passing
                    (setq words[lastWord] (append words[lastWord] _tk1.Value))
                    )) ; end begin argument passing
                ;; User conditions can be added to user defined rules.
                ((= _tk1.UserCondition true)
                 (begin
                    ;; User conditions can be added to user defined rules.
                    (setq lastWord (subi (length words) 1))
                    (if (< lastWord 0)
                        (error (append "Rule: " name " attempted invalid user conditions: "_tk1.Value))
                        ) ; end if error
                    ;; Reconstruct the user defined rule to include user condition
                    (setq words[lastWord] (append words[lastWord] _tk1.Value))
                    )) ; end begin user condition
                ;; User defined rules can be in the form of $N variables.
                ((= _tk1.Value "$")
                 (begin
                    ;; User defined rules can be in the form of $N variables
                    (if (<> (setq _tk1 (_getToken))[Number:] true)
                        (error (append "Rule: " name " attempted invalid user conditions: $"_tk1.Value))
                        ) ; end if error
                    ;; Record another term for the user defined rule.
                    (setq n (add1 (length words))) 
                    (setq words[(length words)] (append "Self{(= _tk" n " _tk" _tk1.Value ")}"))
                    )) ; end begin user defined $N rule
                ;; User defined rules can be in the form of %N variables.
                ((= _tk1.Value "%")
                 (begin
                    ;; User defined rules can be in the form of %N variables
                    (if (<> (setq _tk1 (_getToken))[Number:] true)
                        (error (append "Rule: " name " attempted invalid user conditions: %"_tk1.Value))
                        ) ; end if error
                    ;; Record another term for the user defined rule.
                    (setq n (add1 (length words))) 
                    (setq words[(length words)] (append "Self{(= _tk" n " _ak" _tk1.Value ")}"))
                    )) ; end begin user defined %N rule
                ;; In direct mode, constant user defined rules use the Self symbol.
                ((and directSW (or (not (isSymbol _tk1.Value)) (not (isCharAlphanumeric _tk1.Value))))
                 (begin
                    ;; Record another term for the user defined rule.
                    (setq n (add1 (length words)))
                    (cond
                       ((isSymbol _tk1.Value)
                        (setq words[(length words)] (append "Self{(= _tk" n " |" _tk1.Value "|:)}")))
                       ((isString _tk1.Value)
                        (setq words[(length words)] (append "Self{(= _tk" n " " _tk1.Value ")}")))
                       (else
                        (setq words[(length words)] (append "Self{(= _tk" n " " _tk1.Value ")}")))
                       ) ; end cond
                    )) ; end begin user defined constant rule
                ;; Record another term for the user defined rules.
                (else
                 (setq words[(length words)] _tk1.Value))
                 ) ; end cond
                 ) ; end while
          (if (> (length words) 9) 
              (error (append "Rule " name " has too many terms: " (string words true))))
          (setq condition #void)
          (if (= _tk1.ConditionRule true)
              (begin
                 (setq condition (substitute _tk1.Value _eol " "))
                 (setq _tk1 (_getToken))
                 )) ; end if
          (setq action #void)
          (if (and (<> _tk1.ActionRule true)  (<> _tk1.SubRule true))
              (error (append "Rule " name " has no action") (append "Rule " name " has no action")))
          (setq action (substitute _tk1.Value _eol " "))
          (_lexical_saveRules name words condition action)
          )) ; end Attribute Definition Section
      ;; Recognize end of section
      ((and (= _tk1.Value |#|:)
            (= _tk2.Value End:)
            (= (setq _tk3 (_getToken))[Value:] |#|:))
       (return (begin (setq _lexicalRuleDictionary (_leftRecursionFixup _lexicalRuleDictionary))) _tk0)
       ) ; end Recognize end of section
      ;; Recognize end of file
      ((= _tk1.Eof true)
       ((error "Unexpected end of Lexical Rule Definition Section"))) ; end Recognize end of file
      ) ; end cond
   (goto Retry:)
   _tk0) ;; end of LEXICALRULES




;;**EXPORTKEY**:ParseLib:MAIN
(defriend ParseLib:MAIN()
;; *******************************************************************
;; summary:  This Lambda recognizes ParseLib %DEFINITION files, and
;;           defines the main recognition rules for ParseLib.
;; Parms:    none
;; return:   _tk0    The current token resulting after recognition.
;; *******************************************************************
   vars:(_tk0 _tk1 _tk2 _tk3 
         tmp i vec _oldIp)
   ;; Search for the major compiler definition section
   Retry::
   (setq _oldIp _ip)

   ;; Recognize each major compiler definition section
   (cond
      ;; Recognize User Functions Section
      ((begin
          (setq _tk1 (_getToken))
          (= _tk1.UserFunctions true))
       (begin
          (setq _userFunctions (mid _tk1.Value 15 (subi (length _tk1.Value) 15 5)))
          )) ; end UserFunctions Section
      ;; Recognize Delimited Strings Section
      ((= _tk1.DelimitedStrings true)
       (begin
          (setq tmp (mid _tk1.Value 20 (subi (length _tk1.Value) 21 5)))
          (setq vec (stringToVector tmp _eol))
          (loop for i from 0 until (length vec) do
              (setq tmp vec[i])
              (if (and (<> (length (trim tmp)) 0) 
                       (<> (lisp tmp) '(#void) ;'
						)) 
                  (begin
                     (setq tmp vec[i])
                     (if (= (left tmp (length _eol)) _eol) (setq tmp (mid tmp (length _eol) 10000)))
                     (setq _delimitedStrings 
                             (append _delimitedStrings 
                                     "       (defaultLexer.addStringDelimiters " tmp ")" _eol
                                     )) ; end append
                     )) ; end if
              ) ; end loop
          )) ; end DelimitedStrings Section
      ;; Recognize Semantic Passes Section
      ((= _tk1.SemanticPasses true)
       (begin
          (setq tmp (mid _tk1.Value 18 (subi (length _tk1.Value) 19 5)))
          (setq vec (stringToVector tmp _eol))
          (loop for i from 0 until (length vec) do
              (setq tmp vec[i])
              (if (and (<> (length (trim (clean tmp))) 0) 
                       (<> (lisp tmp) '(#void) ;'
						))  
                  (begin
                     (setq tmp vec[i])
                     (if (= (left tmp (length _eol)) _eol) (setq tmp (mid tmp (length _eol) 10000)))
                     (setq tmp (trim (clean tmp)))
                     (setq _semanticPasses 
                             (append _semanticPasses 
                                     "   (_apply _SEMRULE_" tmp ")" _eol
                                     )) ; end append
                     )) ; end if
              ) ; end loop
          )) ; end SemanticPasses Section
      ;; Recognize Syntax Features Section
      ((begin
          (setq _tk2 (_getToken))
          (setq _tk3 (_getToken))
          (and (= _tk1.Value |#|:)
               (= _tk2.Value SyntaxFeatures:)
               (= _tk3.Value |#|:)))
       (SYNTAXFEATURES)) ; end SyntaxFeatures Section
      ;; Recognize Lexical Features Section
      ((and (= _tk1.Value |#|:)
            (= _tk2.Value LexicalFeatures:)
            (= _tk3.Value |#|:))
       (LEXICALFEATURES)) ; end LexicalFeatures Section
      ;; Recognize Lexical Rule Definitions Section
      ((and (= _tk1.Value |#|:)
            (= _tk2.Value LexicalRules:)
            (= _tk3.Value |#|:))
       (LEXICALRULES)) ; end LexicalRules Section
      ;; Recognize Syntax Rule Definitions Section
      ((and (= _tk1.Value |#|:)
            (= _tk2.Value SyntaxRules:)
            (= _tk3.Value |#|:))
       (SYNTAXRULES)) ; end SyntaxRules Section
      ;; Recognize Semantic Rule Definitions Section
      ((and (= _tk1.Value |#|:)
            (= _tk2.Value SemanticRules:)
            (= _tk3.Value |#|:))
       (SEMANTICRULES)) ; end SemanticRules Section
      ;; Recognize end of file
      ((= _tk1.Eof true)
       (return _tk1)) ; end Recognize end of file
      ;; Syntax error
      (else
       (error "Compiler Definition Syntax Error")) ; end Syntax error
      ) ; end cond
   (goto Retry:)
   _tk0) ;; end of MAIN




;;**EXPORTKEY**:ParseLib:SEMANTICRULES
(defriend ParseLib:SEMANTICRULES()
;; *******************************************************************
;; summary:  This Lambda recognizes semantic rules definitions.
;; Parms:    none
;; return:   _tk0    The current token resulting after recognition.
;; *******************************************************************
   vars:(_tk0 _tk1 _tk2 _tk3 words name action condition lastWord m n directSW rule$Bindings)
   ;; ************************************************
   ;; Define the child Lambdas for this parent Lambda
   ;; ************************************************
   ;; Replace all $X variables with their proper $N bindings.
   (defun replaceBindings(aString rule$Bindings)
      vars:(i n oldString newString)
      (setq n (isub (length rule$Bindings) 1))
      (loop for i from n to 0 by -1 do
          (setq oldString (append "$" rule$Bindings[i 0]))
          (setq newString (append "$" rule$Bindings[i 1]))
          (setq aString (substitute aString oldString newString))
          ) ; end loop
      ;; Return the string with all $X variables with their proper $N bindings. 
      aString) ; end replaceBindings
   ;; ************************************************
   ;; Define the main entry code for this parent Lambda
   ;; ************************************************
   ;; Search for each rule definition section
   Retry::
   (setq _tk1 (_getToken))
   (setq _tk2 (_getToken))
   ;; Recognize each rule definition section
   (cond
      ;; Recognize rule Definition name
      ((and (= _tk1.Name true)
            (or (= _tk2.Value ":") (= _tk2.Value "|")))
       (begin
          (if (= _tk2.Value ":") (setq directSW false) (setq directSW true))
          (setq name _tk1.Value)
          (if (not (isCharUppercase name)) 
              (error (append "Rule " name " is not upper case") (append "Rule " name " is not upper case")))
          (setq words (new Vector: 0))
          ;; Collect the words to be used in this rule definition
          (setq rule$Bindings (new Directory:))
          (while (and (<> (setq _tk1 (_getToken))[Eof:] true)
                      (<> _tk1.ActionRule true)
                      (<> _tk1.ConditionRule true)) do
             ;; Collect the term, user conditions, and arguments for this user defined rule
             (cond
                ;; Arguments can be passed to user defined rules.
                ((= _tk1.ArgumentRule true)
                 (begin
                    ;; Arguments can only be passed to user defined rules.
                    (setq lastWord (subi (length words) 1))
                    (if (or (< lastWord 0) (not (isCharUppercase words[lastWord])))
                        (error (append "Rule: " 
                                       name " attempted to pass arguments "
                                       _tk1.Value " to this non user defined rule: " words[lastWord]))
                        ) ; end if error
                    ;; Reconstruct the user defined rule to include argument passing
                    (setq words[lastWord] (append words[lastWord] (replaceBindings _tk1.Value rule$Bindings)))
                    )) ; end begin argument passing
                ;; User conditions can be added to user defined rules.
                ((= _tk1.UserCondition true)
                 (begin
                    ;; User conditions can be added to user defined rules.
                    (setq lastWord (subi (length words) 1))
                    (if (< lastWord 0)
                        (error (append "Rule: " name " attempted invalid user conditions: "_tk1.Value))
                        ) ; end if error
                    ;; Reconstruct the user defined rule to include user condition
                    (setq words[lastWord] (append words[lastWord] (replaceBindings _tk1.Value rule$Bindings)))
                    )) ; end begin user condition
                ;; User defined rules can be in the form of $N variables.
                ((= _tk1.Value "$")
                 (begin
                    ;; User defined rules can begin with a $ sign.
                    ;; Note: Retrieve the next portion of the $rule.
                    ;;       It must be either numeric or alphabetic.
                    (setq _tk1 (_getToken))
                    (cond 
                       ;; User defined rules can be in the form of $N variables
                       ((= _tk1.Number true)
                        (begin
                           ;; Record another term for the user defined rule.
                           (setq n (add1 (length words))) 
                           (setq words[(length words)] (append "Any{(= _tk" n " _tk" _tk1.Value ")}"))
                           ))
                       ;; User defined rules can be in the form of $X variables
                       ((= _tk1.Name true)
                        (begin
                           ;; Compute the term position for this user defined $X rule.
                           (setq n (add1 (length words))) 
                           ;; Have we seen this $X term before (in this rule)?
                           (if (<> (setq m rule$Bindings[_tk1.Value]) #void)
                               then
                               ;; We have we seen this $X term before (in this rule)!
                               (begin
                                  (if directSW
                                      then
                                      (setq words[(length words)] 
                                                 (append "Any{(= _tk" n " _tk" m ")}"))
                                      else
                                      (setq words[(length words)] 
                                                 (append "Any{(= _tk" n ".Value _tk" _tk1.Value ".Value)}"))
                                      ) ; end if
                                  ) ; end then
                               else
                               ;; We have never seen this $X term before (in this rule)!
                               (begin
                                  (setq rule$Bindings[_tk1.Value] n)
                                  (if directSW
                                      then
                                      (setq words[(length words)] Any:)
                                      else
                                      (setq words[(length words)] Any:)
                                      ) ; end if
                                  ) ; end then
                           )))
                       ;; User defined $rules must be either numeric or alphabetic
                       (else
                        (error (append "Rule: " name " attempted invalid user conditions: $"_tk1.Value)))
                       ) ; end cond
                    )) ; end begin user defined $rule
                ;; User defined rules can be in the form of %N variables.
                ((= _tk1.Value "%")
                 (begin
                    ;; User defined rules can be in the form of %N variables
                    (if (<> (setq _tk1 (_getToken))[Number:] true)
                        (error (append "Rule: " name " attempted invalid user conditions: %"_tk1.Value))
                        ) ; end if error
                    ;; Record another term for the user defined rule.
                    (setq n (add1 (length words))) 
                    (setq words[(length words)] (append "Any{(= _tk" n " _ak" _tk1.Value ")}"))
                    )) ; end begin user defined %N rule
                ;; In direct mode, constant user defined rules use the Any symbol.
                ((and directSW (<> _tk1.Value "[") 
                               (<> _tk1.Value "]") 
                               (or (not (isSymbol _tk1.Value)) (not (isCharAlphanumeric _tk1.Value))))
                 (begin
                    ;; Record another term for the user defined rule.
                    (setq n (add1 (length words)))
                    (cond
                       ((isSymbol _tk1.Value)
                        (setq words[(length words)] (append "Any{(= _tk" n " |" _tk1.Value "|:)}")))
                       ((isString _tk1.Value)
                        (setq words[(length words)] (append "Any{(= _tk" n " " _tk1.Value ")}")))
                       (else
                        (setq words[(length words)] (append "Any{(= _tk" n " " _tk1.Value ")}")))
                       ) ; end cond
                    )) ; end begin user defined constant rule
                ;; Record another term for the user defined rules.
                (else
                 (setq words[(length words)] _tk1.Value))
                 ) ; end cond
             ) ; end while
          (if (> (length words) 9) 
              (error (append "Rule " name " has too many terms: " (string words true))))
          (setq condition #void)
          (if (= _tk1.ConditionRule true)
              (begin
                 (setq condition (substitute (replaceBindings _tk1.Value rule$Bindings) _eol " "))
                 (setq _tk1 (_getToken))
                 )) ; end if
          (setq action #void)
          (if (<> _tk1.ActionRule true) 
              (error (append "Rule " name " has no action") (append "Rule " name " has no action")))
          (setq action (substitute (replaceBindings _tk1.Value rule$Bindings) _eol " "))
          (_semantic_saveRules name words condition action)
          )) ; end Attribute Definition Section
      ;; Recognize end of section
      ((and (= _tk1.Value |#|:)
            (= _tk2.Value End:)
            (= (setq _tk3 (_getToken))[Value:] |#|:))
       (return (begin (setq _semanticRuleDictionary (_leftRecursionFixup _semanticRuleDictionary))) _tk0)
       ) ; end Recognize end of section
      ;; Recognize end of file
      ((= _tk1.Eof true)
       ((error "Unexpected end of Rule Definition Section"))) ; end Recognize end of file
      ) ; end cond
   (goto Retry:)
   _tk0) ;; end of SEMANTICRULES












;;**EXPORTKEY**:ParseLib:SYNTAXFEATURES
(defriend ParseLib:SYNTAXFEATURES()
;; *******************************************************************
;; summary:  This Lambda recognizes syntax feature definitions.
;;           Syntax feature definitions appear as follows:
;;
;;           Operator: [ < > <= => == != + - / * ]
;;           Control: [if then else]
;;
;;           This Lambda also recognizes word feature definitions.
;;           Word feature definitions appear as follows:
;;
;;              WORDS:[compute computes computed (computing Noun)] [Command Verb]
;;              WORDS:[computer computers] [Machine Noun]
;;
;; Notes:    Each syntax feature definition is translated into a
;;           call to the setSyntaxFeature function in the new compiler. 
;;           Each word feature definition is translated into a
;;           call to the setWordFeatures function in the new compiler. 
;;
;; Parms:    none
;; return:   _tk0    The current token resulting after recognition.
;; *******************************************************************
   vars:(_tk0 _tk1 _tk2 _tk3 words values name counter)
   ;; Search for each attribute definition section
   Retry::
   (setq _tk1 (_getToken))
   (setq _tk2 (_getToken))
   (setq _tk3 (_getToken))
   ;; Recognize each attribute definition section
   (cond
      ;; Recognize Word Features Definition
      ((and (= _tk1.Value |WORDS|:)
            (= _tk2.Value |:|:)
            (= _tk3.Value |[|:))
       (begin
          (setq words "#(")
          (setq counter 0)
          ;; Collect the words to be given these feature values
          (while (and (<> (setq _tk1 (_getToken))[Eof:] true)
                      (or (<> _tk1.Value |]|:) (> counter 0))) do
             (cond
                ((= _tk1.Value "[") 
                 (begin (++ counter) (setq words (append words " #("))))
                ((= _tk1.Value "]") 
                 (begin (-- counter) (setq words (append words " )"))))
                ((isSymbol _tk1.Value) 
                 (setq words (append words " |" _tk1.Value "|")))
                ((isString _tk1.Value) 
                 (setq words (append words " " _tk1.Value)))
                ((isNumber _tk1.Value) 
                 (setq words (append words " " _tk1.Value)))
                (else 
                 (setq words (append words " " (symbol _tk1.Value))))
                 ) ; end cond
             ) ; end while
          ;; Collect the feature values for this attribute
          (if (<> (setq _tk3 (_getToken))[Value:] |[|:) (error "Missing features list in Word Features entry"))
          (setq values "#(")
          (setq counter 0)
          ;; Collect the feature value to be given these words
          (while (and (<> (setq _tk1 (_getToken))[Eof:] true)
                      (or (<> _tk1.Value |]|:) (> counter 0))) do
             (cond
                ((= _tk1.Value "[") 
                 (begin (++ counter) (setq values (append values " #("))))
                ((= _tk1.Value "]") 
                 (begin (-- counter) (setq values (append values " )"))))
                ((isSymbol _tk1.Value) 
                 (setq values (append values " |" _tk1.Value "|")))
                ((isString _tk1.Value) 
                 (setq values (append values " " _tk1.Value)))
                ((isNumber _tk1.Value) 
                 (setq values (append values " " _tk1.Value)))
                (else 
                 (setq values (append values " " (symbol _tk1.Value))))
                 ) ; end cond
             ) ; end while
          ;; Output the attribute assignment source code
          (setq _syntaxFeatures 
                   (append _syntaxFeatures 
                           "       (_setWordFeatures "
                           words ") "
                           values "))" _eol))
          )) ; end Word Features Definition
      ;; Recognize Syntax Feature Definition
      ((and (= _tk1.Name true)
            (= _tk2.Value |:|:)
            (= _tk3.Value |[|:))
       (begin
          (setq name _tk1.Value)
          (setq words "#(")
          (setq values #void)
          ;; Collect the words to be given this attribute
          (while (and (<> (setq _tk1 (_getToken))[Eof:] true)
                      (<> _tk1.Value |]|:)) do
             (cond
                ((isSymbol _tk1.Value) 
                 (setq words (append words " |" _tk1.Value "|")))
                ((isString _tk1.Value) 
                 (setq words (append words " " _tk1.Value)))
                ((isNumber _tk1.Value) 
                 (setq words (append words " " _tk1.Value)))
                (else 
                 (setq words (append words " " (symbol _tk1.Value))))
                 ) ; end cond
             ) ; end while
          ;; Collect the optional values for this attribute
          (if (= (setq _tk3 (_getToken))[Value:] |[|:)
              (begin
                 (setq values (new Vector: 0))
                 (while (and (<> (setq _tk1 (_getToken))[Eof:] true)
                             (<> _tk1.Value |]|:)) do
                    (setq values[(length values)] _tk1.Value)
                    ) ; end while
                 ) ; end begin
              else
              (-- _ip) 
              ) ; end if
          ;; Output the attribute assignment source code
          (setq _syntaxFeatures 
                   (append _syntaxFeatures 
                           "       (_setSyntaxFeature "
                           name ": "
                           words ") "
                           (string values true) ")" _eol))
          )) ; end Syntax Feature Definition
      ;; Recognize End of Syntax Features section
      ((and (= _tk1.Value |#|:)
            (= _tk2.Value End:)
            (= _tk3.Value |#|:))
       (return _tk0)) ; end End of Syntax Features section
      ;; Recognize end of file
      ((= _tk1.Eof true)
       (error "Unexpected end of Syntax Features Section")) ; end Recognize end of file
      ) ; end cond
   (goto Retry:)
   _tk0) ;; end of SYNTAXFEATURES





















;;**EXPORTKEY**:ParseLib:SYNTAXRULES
(defriend ParseLib:SYNTAXRULES()
;; *******************************************************************
;; summary:  This Lambda recognizes syntax rules definitions.
;; Parms:    none
;; return:   _tk0    The current token resulting after recognition.
;; *******************************************************************
   vars:(_tk0 _tk1 _tk2 _tk3 words name action condition lastWord n directSW)
   ;; Search for each rule definition section
   Retry::
   (setq _tk1 (_getToken))
   (setq _tk2 (_getToken))
   ;; Recognize each rule definition section
   (cond
      ;; Recognize rule Definition name
      ((and (= _tk1.Name true)
            (or (= _tk2.Value ":") (= _tk2.Value "|")))
       (begin
          (if (= _tk2.Value ":") (setq directSW false) (setq directSW true))
          (setq name _tk1.Value)
          (if (not (isCharUppercase name)) 
              (error (append "Rule " name " is not upper case") (append "Rule " name " is not upper case")))
          (setq words (new Vector: 0))
          ;; Collect the words to be used in this rule definition
          (while (and (<> (setq _tk1 (_getToken))[Eof:] true)
                      (<> _tk1.ActionRule true)
                      (<> _tk1.ConditionRule true)
					  (<> _tk1.SubRule true)) do
             ;; Collect the term, user conditions, and arguments for this user defined rule
             (cond
                ;; Arguments can be passed to user defined rules.
                ((= _tk1.ArgumentRule true)
                 (begin
                    ;; Arguments can only be passed to user defined rules.
                    (setq lastWord (subi (length words) 1))
                    (if (or (< lastWord 0) (not (isCharUppercase words[lastWord])))
                        (error (append "Rule: " 
                                       name " attempted to pass arguments "
                                       _tk1.Value " to this non user defined rule: " words[lastWord]))
                        ) ; end if error
                    ;; Reconstruct the user defined rule to include argument passing
                    (setq words[lastWord] (append words[lastWord] _tk1.Value))
                    )) ; end begin argument passing
                ;; User conditions can be added to user defined rules.
                ((= _tk1.UserCondition true)
                 (begin
                    ;; User conditions can be added to user defined rules.
                    (setq lastWord (subi (length words) 1))
                    (if (< lastWord 0)
                        (error (append "Rule: " name " attempted invalid user conditions: "_tk1.Value))
                        ) ; end if error
                    ;; Reconstruct the user defined rule to include user condition
                    (setq words[lastWord] (append words[lastWord] _tk1.Value))
                    )) ; end begin user condition
                ;; User defined rules can be in the form of $N variables.
                ((= _tk1.Value "$")
                 (begin
                    ;; User defined rules can be in the form of $N variables
                    (if (<> (setq _tk1 (_getToken))[Number:] true)
                        (error (append "Rule: " name " attempted invalid user conditions: $"_tk1.Value))
                        ) ; end if error
                    ;; Record another term for the user defined rule.
                    (setq n (add1 (length words))) 
                    (setq words[(length words)] (append "Self{(= _tk" n " _tk" _tk1.Value ")}"))
                    )) ; end begin user defined $N rule
                ;; User defined rules can be in the form of %N variables.
                ((= _tk1.Value "%")
                 (begin
                    ;; User defined rules can be in the form of %N variables
                    (if (<> (setq _tk1 (_getToken))[Number:] true)
                        (error (append "Rule: " name " attempted invalid user conditions: %"_tk1.Value))
                        ) ; end if error
                    ;; Record another term for the user defined rule.
                    (setq n (add1 (length words))) 
                    (setq words[(length words)] (append "Self{(= _tk" n " _ak" _tk1.Value ")}"))
                    )) ; end begin user defined %N rule
                ;; In direct mode, constant user defined rules use the Self symbol.
                ((and directSW (or (not (isSymbol _tk1.Value)) (not (isCharAlphanumeric _tk1.Value))))
                 (begin
                    ;; Record another term for the user defined rule.
                    (setq n (add1 (length words)))
                    (cond
                       ((isSymbol _tk1.Value)
                        (setq words[(length words)] (append "Self{(= _tk" n " |" _tk1.Value "|:)}")))
                       ((isString _tk1.Value)
                        (setq words[(length words)] (append "Self{(= _tk" n " " _tk1.Value ")}")))
                       (else
                        (setq words[(length words)] (append "Self{(= _tk" n " " _tk1.Value ")}")))
                       ) ; end cond
                    )) ; end begin user defined constant rule
                ;; Record another term for the user defined rules.
                (else
                 (setq words[(length words)] _tk1.Value))
                 ) ; end cond
             ) ; end while
          (if (> (length words) 9) 
              (error (append "Rule " name " has too many terms: " (string words true))))
          (setq condition #void)
          (if (= _tk1.ConditionRule true)
              (begin
                 (setq condition (substitute _tk1.Value _eol " "))
                 (setq _tk1 (_getToken))
                 )) ; end if
          (setq action #void)
          (if (and (<> _tk1.ActionRule true)  (<> _tk1.SubRule true))
              (error (append "Rule " name " has no action") (append "Rule " name " has no action")))
          (setq action (substitute _tk1.Value _eol " "))
          (_syntax_saveRules name words condition action)
          )) ; end Attribute Definition Section
      ;; Recognize end of section
      ((and (= _tk1.Value |#|:)
            (= _tk2.Value End:)
            (= (setq _tk3 (_getToken))[Value:] |#|:))
       (return (begin (setq _syntaxRuleDictionary (_leftRecursionFixup _syntaxRuleDictionary))) _tk0)
       ) ; end Recognize end of section
      ;; Recognize end of file
      ((= _tk1.Eof true)
       ((error "Unexpected end of Rule Definition Section"))) ; end Recognize end of file
      ) ; end cond
   (goto Retry:)
   _tk0) ;; end of SYNTAXRULES




;;**EXPORTKEY**:ParseLib:_leftRecursionFixup
(defriend ParseLib:_leftRecursionFixup(ruleDictionary)
;; *******************************************************************
;; summary:  Convert all rules in the rule dictionary from left to
;;           right recursion format. 
;; Parms:    ruleDictionary  The dictionary of all rules and their clauses.
;; return:   ruleDictionary  The converted rules dictionary.
;;
;; Note: This function is called for lexical, syntax, and semantic rule dictionaries.
;;       Each rule dictionary has entries which are keyed by rule name and which
;;       have values which are rule clause vectors from 1 to N clauses in length.
;;       Each rule clause vector contains rule clauses which are themselves vectors
;;       and are always composed of the following four elements.
;;
;;           words      Each iemized condition for this rule (ie Name + Name)
;;           condition  Any trailing rule condition (ie || (= $1.Value "if") ||)
;;           action     Any trailing rule action (ie :: (setq $0.Value $1.Value) ::)      
;;           ruleID     The display version of the rule for printing to the console      
;;
;; Note: Converting a set of rules from left to right recursion is a
;;       process which can best be described by the following examples.
;;
;;       Example 1: This rule is in left recursion form.
;;
;;           EXPR: EXPR Operator EXPR :: ... ::
;;           EXPR: ( EXPR ) :: ... ::
;;           EXPR: Number :: ... ::
;;           EXPR: Name :: ... ::
;;
;;       The _leftRecursionFixup function converts the EXPR rule
;;       into two rules EXPR and _EXPR in right recursion form:
;;
;;           EXPR: LEXPR Operator EXPR :: ... ::
;;           EXPR: LEXPR :: $1 ::
;;
;;           LEXPR: ( EXPR ) :: ... ::
;;           LEXPR: Number :: ... ::
;;           LEXPR: Name :: ... ::
;;
;;       Example 2: This rule is in left recursion form.
;;
;;           EXPR: user ordering :: true ::
;;           EXPR: ( EXPR ) :: ... ::
;;           EXPR: Number :: ... ::
;;           EXPR: EXPR Operator EXPR :: ... ::
;;           EXPR: Name :: ... ::
;;
;;       The _leftRecursionFixup function converts the EXPR rule
;;       into two rules EXPR and _EXPR in right recursion form:
;;
;;           EXPR: user ordering :: true ::
;;           EXPR: LEXPR Operator EXPR :: ... ::
;;           EXPR: LEXPR :: $1 ::
;;
;;           LEXPR: user ordering :: true ::
;;           LEXPR: ( EXPR ) :: ... ::
;;           LEXPR: Number :: ... ::
;;           LEXPR: Name :: ... ::
;;
;; *******************************************************************
   vars:(i j k
         firstTerm nameLen rightVector
         ruleCount ruleName leftRulesList 
         ruleClause clauseVector clauseCount)
   ;; Search the rule dictionary identifying any left recursive rules.
   (setq leftRulesList (new Dictionary:))
   (setq ruleCount (length ruleDictionary)) 
   (loop for i from 0 until ruleCount do
       (setq ruleName ruleDictionary[i 0])
       (setq nameLen (length ruleName)) 
       (setq clauseVector ruleDictionary[i 1])
       (setq clauseCount (length clauseVector))
       (loop for j from 0 until clauseCount do 
           ;; Load the first term from the current rule clause.
           ;; If it refers to its own rule name, then it is
           ;; a rule clause in left recursive form.
           (setq firstTerm clauseVector[j][0][0])
           (if (and (= ruleName (left firstTerm nameLen)) (not (isCharUppercase firstTerm[nameLen])))
               (setq leftRulesList[ruleName] true)
               ) ; end if
           ) ; end clause loop
       ) ; end rule loop 
   ;; Convert all rules with left recursive rule clauses into two rules as shown above.
   (setq ruleCount (length leftRulesList))
   (loop for i from 0 until ruleCount do
       (setq ruleName leftRulesList[i 0])
       (setq nameLen (length ruleName)) 
       (setq clauseVector ruleDictionary[i 1])
       (setq rightVector (new Vector: 0))
       (loop for j from 0 until (length clauseVector) do
           ;; Load the first term from the current rule clause.
           (setq firstTerm clauseVector[j][0][0])
           (cond
              ;; Is this a user ordering clause?
              ((= firstTerm "user ordering")
               (setq rightVector[(length rightVector)] clauseVector[j])
               ) ; end user ordering clause

              ;; Is this clause in left recursive form?
              ((and (= ruleName (left firstTerm nameLen)) (not (isCharUppercase firstTerm[nameLen])))
               ;; Append "L" to the first term rule name so it is not left recursive.
               (setq clauseVector[j][0][0] (symbol (append "L" firstTerm)))
               ) ; end left recursion clause

              ;; Is this clause not in left recursive form?
              (else
               ;; Move the clause to the new right recursive rule.
               (begin
                  (setq rightVector[(length rightVector)] clauseVector[j])
                  (setq clauseVector (delete clauseVector j))
                  (setq j (isub j 1))
               )) ; end left recursion clause

              ) ; end cond       
           ) ; end clause loop
       ;; Add the singleton reference to the new rule.
       (setq clauseVector[(length clauseVector)] (new Vector: 4 
                                                      (new Vector: 1 (symbol (append "L" ruleName)))
                                                      #void
                                                      ":: $1 ::"
                                                      (append ruleName " :: $1 ::")
                                                 ))
       ;; Replace the old rule and add the new rule
       (setq ruleDictionary[ruleName] clauseVector)
       (if (<> ruleDictionary[(append "L" ruleName)] #void)
           (error (append "ParseLib is creating a left recursion rule which conflicts with rule: " "L" ruleName)))
       (setq ruleDictionary[(append "L" ruleName)] rightVector)
       ) ; end rule loop 
   ;; Return the rule dictionary converted to right recursive form.
   ruleDictionary) ; end _leftRecursionFixup






















;;**EXPORTKEY**:ParseLib:_lexical_outputARule
(defriend ParseLib:_lexical_outputARule(name v)
;; *******************************************************************
;; summary:  This Lambda converts a vector of rules into a output
;;           Lisp source string. This output forms the core of the
;;           generated Lisp rule implementation. The rules are
;;           sorted so that the least expensive approach may be
;;           taken to implementin them. (see the RTN ideas in
;;           "Natural Languiage Processing in Lisp".
;; Notes:    The rule vector (v) is a vector of user defined rule
;;           clauses all having the same rule name (name). Each rule 
;;           is a vector as follows:
;;                 clause[0]   Vector of rule terms
;;                 clause[1]   condition
;;                 clause[2]   action
;;                 clause[3]   rulePrettyPrintForm           
;; Parms:    name       The name of the user defined rule.
;;           v          The vector of the user defined rule.
;; return:   _result    The Lisp source string implementing the rule.
;; *******************************************************************
   pvars:(_name              ;; Name of the user defined rule
          _result            ;; Resulting user defined Lisp action source
          _userOrderingSW    ;; True if user ordering is turned on
          _generateTest      ;; isLambda for user ordering, isDirectory for auto ordering
          ;; Child methods
          _action            ;; Output an action for a rule 
          _buildRuleTree     ;; Build a condition graph for the rules
          _condition         ;; Output a condition for a rule    
          _fixup             ;; Fixup a Lisp sequence for $N and %N references
          _generateSource    ;; Ouput condition graph for the rules
         ) ; end persistant variables
   vars:(n i d)
   ;; Output an action for a rule
   (defun _action(ruleIndex action ruleID bk termIX)
      vars:(i (result "") term repeatSW)
       ;; Compose the action for this user defined rule
       (setq repeatSW (= (left action 2) "<<"))
       (setq action (_fixup action))
       (setq result (append result    " (begin" _eol))

       ;; Is this a default action in a mid level rule group?
       (if (and (> termIX 0) (> ruleIndex 0))
           (setq result (append result bk "     (setq _ip _ip" termIX ")" _eol))
           ) ; end if

       (setq result (append result bk "     (setq _ret " action ")" _eol))
       (setq result (append result bk "     (if _verbose" _eol 
                                   bk "         (writeRule" _eol 
                                   bk "              {" ruleID "}" _eol
                                   bk "              _ret ")) 
       (loop for i from 0 to termIX do
           (setq result (append result " _tk" i))
           ) ; end loop
       (loop for i from termIX until 10 do
           (setq result (append result " #void"))
           ) ; end loop
       (setq result (append result "))" _eol "(setq _indent (isub _indent 1))" _eol  ))
       (if repeatSW
          (setq result (append result bk "     (goto Skip:))"))
          (setq result (append result bk "     (return _ret))"))
          ) ; end if
       result) ; end _action
   ;; Build a condition graph for the rules
   (defun _buildRuleTree(ruleTree i ruleClause)
       vars:(d2)
       ;; Is this the default condition for this rule?
       ;; Note the ruleClause contains [0]=term vector,[1]=condition,[2]=action,[3]=ruleID
       (if (= ruleClause[0][i] #void) 
           (begin 
              (setq ruleTree._default (new Vector: 3 ruleClause[1] ruleClause[2] ruleClause[3])) 
              (return ruleTree)
              )) ; end if
       ;; Is this a new condition for this level of the rule?
       (if (= ruleTree[ruleClause[0][i]] #void) 
           (if _userOrderingSW
              (setq ruleTree[ruleClause[0][i]] (setq d2 (new rulesDirectory)))
              (setq ruleTree[ruleClause[0][i]] (setq d2 (new Directory:)))
              ) ; end if
           ) ; end if
       (_buildRuleTree ruleTree[ruleClause[0][i]] (addi i 1) ruleClause)) ; end _buildRuleTree
   ;; Output a condition for a rule
   (defun _condition(ruleIndex termName ruleClause termIX bk)
       vars:(condition (result "") term termLen termCondition termBackup pos userCondition tmpResult bnfCommand)
       ;; Is this the default condition for this rule?
       ;; Note the ruleClause contains [0]=condition,[1]=action,[2]=ruleID
       (if (= termName "_default") 
           (begin
              (setq condition ruleClause[0])
              (if (<> condition #void)
                  (begin
                     (setq condition (_fixup condition))
                     (return condition)
                     )) ; end if
              (return "true")
              )) ; end default if
       ;; Is this a new condition for this level of the rule?
       (setq term termName)
       ;; Remove any trailing BNF commands from the term.
       (setq bnfCommand #void)
       (if (and (isCharName (left term 1))
                (or (= (right term 1) "*") (= (right term 1) "?") (= (right term 1) "+")))
           (begin
              (setq termLen (length term))
              (setq bnfCommand (right term 1))
              (setq term (symbol (left term (sub1 termLen))))        
              )) ; end if
       ;; Remove any user condition from the term.
       (setq userCondition #void)
       (if (= (right term 1) "}")
           (begin
              (setq pos (find "{" term))
              (setq userCondition (_fixup (mid term pos 10000)))
              (setq term (symbol (trim (left term pos))))        
              )) ; end if
         (cond
           ;; Build condition for a user defined rule with argument passing
           ((= (right term 1) ")")
            ;; We must generate code based upon the associated trailing BNF command.
            (cond
               ;; BNF command is none.
               ((= bnfCommand #void)
                (setq tmpResult (append 
                          "(if (<> (setq _tk" (addi termIX 1) " (" (_fixup (append _rulePrefix term)) ")) morphFail)"
                               "true "
                               "(setq _ip _ip" termIX "))"
                          ))
                ) ; end BNF command is none
               ;; BNF command is *.
               ((= bnfCommand "*")
                (if (= userCondition #void)
                    then
                    (setq tmpResult (append 
                          "(begin "
                              "(setq _i 0) " 
                              "(setq _tk" (addi termIX 1) " (new Vector: 0)) " 
                              "(while " 
                                  "(<> (setq _tkthis (" (_fixup (append _rulePrefix term)) ")) morphFail) do "
                                  "(begin "
                                     "(setq _tk" (addi termIX 1) "[_i] _tkthis) "
                                     "(setq _i (iadd _i 1)) " 
                                  ")) " 
                              "true)"
                        ))
                    else
                    (setq tmpResult (append 
                          "(begin "
                              "(setq _i 0) " 
                              "(setq _tk" (addi termIX 1) " (new Vector: 0)) " 
                              "(while "
                                  "(and "
                                    "(<> (setq _tkthis (" (_fixup (append _rulePrefix term)) ")) morphFail) "
                                     userCondition " ) do "
                                    "(begin "
                                       "(setq _tk" (addi termIX 1) "[_i] _tkthis) "
                                       "(setq _i (iadd _i 1)) " 
                                    ")) " 
                              "true)"
                        )))
                ) ; end BNF command is *
               ;; BNF command is +.
               ((= bnfCommand "+")
                (if (= userCondition #void)
                    then
                    (setq tmpResult (append 
                          "(begin "
                              "(setq _i 0) " 
                              "(setq _tk" (addi termIX 1) " (new Vector: 0)) " 
                              "(while " 
                                  "(<> (setq _tkthis (" (_fixup (append _rulePrefix term)) ")) morphFail) do "
                                  "(begin "
                                     "(setq _tk" (addi termIX 1) "[_i] _tkthis) "
                                     "(setq _i (iadd _i 1)) " 
                                  ")) " 
                              "(if (> _i 0) true (setq _ip _ip" termIX ")))"
                        ))
                    else
                    (setq tmpResult (append 
                          "(begin "
                              "(setq _i 0) " 
                              "(setq _tk" (addi termIX 1) " (new Vector: 0)) " 
                              "(while " 
                                  "(and "
                                    "(<> (setq _tkthis (" (_fixup (append _rulePrefix term)) ")) morphFail) "
                                     userCondition " ) do "
                                  "(begin "
                                     "(setq _tk" (addi termIX 1) "[_i] _tkthis) "
                                     "(setq _i (iadd _i 1)) " 
                                  ")) " 
                              "(if (> _i 0) true (setq _ip _ip" termIX ")))"
                        )))
                ) ; end BNF command is +
               ;; BNF command is ?.
               ((= bnfCommand "?")
                (if (= userCondition #void)
                    then
                    (setq tmpResult (append 
                          "(if (<> (setq _tk" (addi termIX 1) " (" (_fixup (append _rulePrefix term)) ")) morphFail) "
                               "true "
                               "(begin "
                                  "(setq _tk" (addi termIX 1) " #void) "
                                  "true))"
                          ))
                    else
                    (setq tmpResult (append 
                          "(if "
                             "(and "
                              "(<> (setq _tk" (addi termIX 1) " (" (_fixup (append _rulePrefix term)) ")) morphFail) "
                               userCondition " )"
                               "true "
                               "(begin "
                                  "(setq _tk" (addi termIX 1) " #void) "
                                  "true))"
                          )))
                ) ; end BNF command is ?
               ) ; end BNF cond            
            ) ; end Build condition for a user defined rule with argument passing

           ;; Build condition for a user defined rule
           ((and (isSymbol term) (isCharUppercase term))
            ;; We must generate code based upon the associated trailing BNF command.
            (cond
               ;; BNF command is none.
               ((= bnfCommand #void)
                (setq tmpResult (append 
                          "(if (<> (setq _tk" (addi termIX 1) " (" (append _rulePrefix term) ")) morphFail)"
                               "true "
                               "(setq _ip _ip" termIX "))"
                          ))
                ) ; end BNF command is none
               ;; BNF command is *.
               ((= bnfCommand "*")
                (if (= userCondition #void)
                    then
                    (setq tmpResult (append 
                          "(begin "
                              "(setq _i 0) " 
                              "(setq _tk" (addi termIX 1) " (new Vector: 0)) " 
                              "(while " 
                                  "(<> (setq _tkthis (" (append _rulePrefix term) ")) morphFail) do "
                                  "(begin "
                                     "(setq _tk" (addi termIX 1) "[_i] _tkthis) "
                                     "(setq _i (iadd _i 1)) " 
                                  ")) " 
                              "true)"
                        ))
                    else
                    (setq tmpResult (append 
                          "(begin "
                              "(setq _i 0) " 
                              "(setq _tk" (addi termIX 1) " (new Vector: 0)) " 
                              "(while "
                                  "(and "
                                    "(<> (setq _tkthis (" (append _rulePrefix term) ")) morphFail) "
                                     userCondition " ) do "
                                    "(begin "
                                       "(setq _tk" (addi termIX 1) "[_i] _tkthis) "
                                       "(setq _i (iadd _i 1)) " 
                                    ")) " 
                              "true)"
                        )))
                ) ; end BNF command is *
               ;; BNF command is +.
               ((= bnfCommand "+")
                (if (= userCondition #void)
                    then
                    (setq tmpResult (append 
                          "(begin "
                              "(setq _i 0) " 
                              "(setq _tk" (addi termIX 1) " (new Vector: 0)) " 
                              "(while " 
                                  "(<> (setq _tkthis (" (append _rulePrefix term) ")) morphFail) do "
                                  "(begin "
                                     "(setq _tk" (addi termIX 1) "[_i] _tkthis) "
                                     "(setq _i (iadd _i 1)) " 
                                  ")) " 
                              "(if (> _i 0) true (setq _ip _ip" termIX ")))"
                        ))
                    else
                    (setq tmpResult (append 
                          "(begin "
                              "(setq _i 0) " 
                              "(setq _tk" (addi termIX 1) " (new Vector: 0)) " 
                              "(while " 
                                  "(and "
                                    "(<> (setq _tkthis (" (append _rulePrefix term) ")) morphFail) "
                                     userCondition " ) do "
                                  "(begin "
                                     "(setq _tk" (addi termIX 1) "[_i] _tkthis) "
                                     "(setq _i (iadd _i 1)) " 
                                  ")) " 
                              "(if (> _i 0) true (setq _ip _ip" termIX ")))"
                        )))
                ) ; end BNF command is +
               ;; BNF command is ?.
               ((= bnfCommand "?")
                (if (= userCondition #void)
                    then
                    (setq tmpResult (append 
                          "(if (<> (setq _tk" (addi termIX 1) " (" (append _rulePrefix term) ")) morphFail) "
                               "true "
                               "(begin "
                                  "(setq _tk" (addi termIX 1) " #void) "
                                  "true))"
                          ))
                    else
                    (setq tmpResult (append 
                          "(if "
                             "(and "
                              "(<> (setq _tk" (addi termIX 1) " (" (append _rulePrefix term) ")) morphFail) "
                               userCondition " )"
                               "true "
                               "(begin "
                                  "(setq _tk" (addi termIX 1) " #void) "
                                  "true))"
                          )))
                ) ; end BNF command is ?
               ) ; end BNF cond            
            ) ; end Build condition for a user defined rule

           ;; Build condition for an Eof test
           ((and (isSymbol term) (= term Eof:))
            (setq tmpResult (append "(if (= (setq _tk" (addi termIX 1) 
                                    " $IN[(setq _ip (iadd _ip 1))]) #void) true (setq _ip _ip" termIX "))")))

           ;; Build condition for a Nop test
           ((and (isSymbol term) (= term Nop:))
            (setq tmpResult (append "(begin (setq _tk" (addi termIX 1) " #void) true)")))

           ;; Build condition for an Any test
           ((and (isSymbol term) (= term Any:))
            ;; We must generate code based upon the associated trailing BNF command.
            (cond
               ;; BNF command is none.
               ((= bnfCommand #void)
                (setq tmpResult (append 
                          "(if (<> (setq _tk" (addi termIX 1) " $IN[(setq _ip (iadd _ip 1))]) #void) "
                               "true "
                               "(setq _ip _ip" termIX "))"
                          ))
                ) ; end BNF command is none
               ;; BNF command is *.
               ((= bnfCommand "*")
                (if (= userCondition #void)
                    then
                    (setq tmpResult (append 
                          "(begin "
                              "(setq _i 0) " 
                              "(setq _tk" (addi termIX 1) " (makeString {})) " 
                              "(while " 
                                  "(<> (setq _tkthis $IN[(setq _ip (iadd _ip 1))]) #void) do "
                                  "(begin "
                                     "(setq _tk" (addi termIX 1) "[_i] _tkthis) "
                                     "(setq _i (iadd _i 1)) " 
                                  ")) " 
                              "(setq _ip (isub _ip 1)) "
                              "true)"
                        ))
                    else
                    (setq tmpResult (append 
                          "(begin "
                              "(setq _i 0) " 
                              "(setq _tk" (addi termIX 1) " (makeString {})) " 
                              "(while " 
                                  "(and "
                                    "(<> (setq _tkthis $IN[(setq _ip (iadd _ip 1))]) #void) "
                                     userCondition " ) do "
                                  "(begin "
                                     "(setq _tk" (addi termIX 1) "[_i] _tkthis) "
                                     "(setq _i (iadd _i 1)) " 
                                  ")) " 
                              "(setq _ip (isub _ip 1)) "
                              "true)"
                        )))
                ) ; end BNF command is *
               ;; BNF command is +.
               ((= bnfCommand "+")
                (if (= userCondition #void)
                    then
                    (setq tmpResult (append 
                          "(begin "
                              "(setq _i 0) " 
                              "(setq _tk" (addi termIX 1) " (makeString {})) " 
                              "(while " 
                                  "(<> (setq _tkthis $IN[(setq _ip (iadd _ip 1))]) #void) do "
                                  "(begin "
                                     "(setq _tk" (addi termIX 1) "[_i] _tkthis) "
                                     "(setq _i (iadd _i 1)) " 
                                  ")) " 
                              "(setq _ip (isub _ip 1)) "
                              "(if (> _i 0) true (setq _ip _ip" termIX ")))"
                        ))
                    else
                    (setq tmpResult (append 
                          "(begin "
                              "(setq _i 0) " 
                              "(setq _tk" (addi termIX 1) " (makeString {})) " 
                              "(while " 
                                  "(and "
                                    "(<> (setq _tkthis $IN[(setq _ip (iadd _ip 1))]) #void) "
                                     userCondition " ) do "
                                  "(begin "
                                     "(setq _tk" (addi termIX 1) "[_i] _tkthis) "
                                     "(setq _i (iadd _i 1)) " 
                                  ")) " 
                              "(setq _ip (isub _ip 1)) "
                              "(if (> _i 0) true (setq _ip _ip" termIX ")))"
                        )))
                ) ; end BNF command is +
               ;; BNF command is ?.
               ((= bnfCommand "?")
                (if (= userCondition #void)
                    then
                    (setq tmpResult (append 
                          "(if (<> (setq _tk" (addi termIX 1) " $IN[(setq _ip (iadd _ip 1))]) #void) "
                               "true "
                               "(begin "
                                  "(setq _ip (isub _ip 1)) "
                                  "(setq _tk" (addi termIX 1) " {}) "
                                  "true))"
                          ))
                    else
                    (setq tmpResult (append 
                          "(if "
                            "(and "
                              "(<> (setq _tk" (addi termIX 1) " $IN[(setq _ip (iadd _ip 1))]) #void) "
                               userCondition " ) "
                               "true "
                               "(begin "
                                  "(setq _ip (isub _ip 1)) "
                                  "(setq _tk" (addi termIX 1) " {}) "
                                  "true))"
                          )))
                ) ; end BNF command is ?
               ) ; end BNF cond            
            ) ; end Build condition for an Any test

           ;; Build condition for a feature test
           ((and (isSymbol term) (isCharAlphanumeric term))
            ;; We must generate code based upon the associated trailing BNF command.
            (cond
               ;; BNF command is none.
               ((= bnfCommand #void)
                (setq tmpResult (append 
                          "(if (and (<> (setq _tk" (addi termIX 1) " $IN[(setq _ip (iadd _ip 1))]) #void) "
                                   "(= _LF_" term "[_tk" (addi termIX 1) "] 1)) "
                               "true "
                               "(setq _ip _ip" termIX "))"
                          ))
                ) ; end BNF command is none
               ;; BNF command is *.
               ((= bnfCommand "*")
                (if (= userCondition #void)
                    then
                    (setq tmpResult (append 
                          "(begin "
                              "(setq _i 0) " 
                              "(setq _tk" (addi termIX 1) " (makeString {})) " 
                              "(while " 
                                  "(and (<> (setq _tkthis $IN[(setq _ip (iadd _ip 1))]) #void) "
                                       "(= _LF_" term "[_tkthis] 1)) do "
                                  "(begin "
                                     "(setq _tk" (addi termIX 1) "[_i] _tkthis) "
                                     "(setq _i (iadd _i 1)) " 
                                  ")) " 
                              "(setq _ip (isub _ip 1)) "
                              "true)"
                        ))
                    else
                    (setq tmpResult (append 
                          "(begin "
                              "(setq _i 0) " 
                              "(setq _tk" (addi termIX 1) " (makeString {})) " 
                              "(while " 
                                  "(and (<> (setq _tkthis $IN[(setq _ip (iadd _ip 1))]) #void) "
                                       "(= _LF_" term "[_tkthis] 1) "
                                       userCondition " ) do "
                                  "(begin "
                                     "(setq _tk" (addi termIX 1) "[_i] _tkthis) "
                                     "(setq _i (iadd _i 1)) " 
                                  ")) " 
                              "(setq _ip (isub _ip 1)) "
                              "true)"
                        )))
                ) ; end BNF command is *
               ;; BNF command is +.
               ((= bnfCommand "+")
                (if (= userCondition #void)
                    then
                    (setq tmpResult (append 
                          "(begin "
                              "(setq _i 0) " 
                              "(setq _tk" (addi termIX 1) " (makeString {})) " 
                              "(while " 
                                  "(and (<> (setq _tkthis $IN[(setq _ip (iadd _ip 1))]) #void) "
                                       "(= _LF_" term "[_tkthis] 1)) do "
                                  "(begin "
                                     "(setq _tk" (addi termIX 1) "[_i] _tkthis) "
                                     "(setq _i (iadd _i 1)) " 
                                  ")) " 
                              "(setq _ip (isub _ip 1)) "
                              "(if (> _i 0) true (setq _ip _ip" termIX ")))"
                        ))
                    else
                    (setq tmpResult (append 
                          "(begin "
                              "(setq _i 0) " 
                              "(setq _tk" (addi termIX 1) " (makeString {})) " 
                              "(while " 
                                  "(and (<> (setq _tkthis $IN[(setq _ip (iadd _ip 1))]) #void) "
                                       "(= _LF_" term "[_tkthis] 1) "
                                        userCondition " ) do "
                                  "(begin "
                                     "(setq _tk" (addi termIX 1) "[_i] _tkthis) "
                                     "(setq _i (iadd _i 1)) " 
                                  ")) " 
                              "(setq _ip (isub _ip 1)) "
                              "(if (> _i 0) true (setq _ip _ip" termIX ")))"
                        )))
                ) ; end BNF command is +
               ;; BNF command is ?.
               ((= bnfCommand "?")
                (if (= userCondition #void)
                    then
                    (setq tmpResult (append 
                          "(if (and (<> (setq _tk" (addi termIX 1) " $IN[(setq _ip (iadd _ip 1))]) #void) "
                                   "(= _LF_" term "[_tk" (addi termIX 1) "] 1)) "
                               "true "
                               "(begin "
                                  "(setq _ip (isub _ip 1)) "
                                  "(setq _tk" (addi termIX 1) " {}) "
                                  "true))"
                          ))
                    else
                    (setq tmpResult (append 
                          "(if (and (<> (setq _tk" (addi termIX 1) " $IN[(setq _ip (iadd _ip 1))]) #void) "
                                   "(= _LF_" term "[_tk" (addi termIX 1) "] 1) "
                                    userCondition " )"
                               "true "
                               "(begin "
                                  "(setq _ip (isub _ip 1)) "
                                  "(setq _tk" (addi termIX 1) " {}) "
                                  "true))"
                          )))
                ) ; end BNF command is ?
               ) ; end BNF cond            
            ) ; end Build condition for a feature test

           ;; Build condition for a general constant test
           (else
            (begin
               (cond
                (set termBackup 1)
                ((isSymbol term)
                 (setq termCondition (append " $IN[(setq _ip (iadd _ip 1))]) #\\" term[0])))
                ((and (isString term) (= (length term) 3))
                 (setq termCondition (append " $IN[(setq _ip (iadd _ip 1))]) #\\" term[1])))
                ((and (isString term) (begin (setq termBackup (isub (length term) 2)) true))
                 (setq termCondition (append " (mid $IN (iadd _ip 1) "
                                                  "(begin (setq _ip (iadd _ip " termBackup ")) " termBackup "))) " term))) 
                (else
                 (setq termCondition (append " $IN[(setq _ip (iadd _ip 1))]) #\\" (string term)[0])))
                ) ; end cond
               (setq tmpResult (append "(if (= (setq _tk" (addi termIX 1) termCondition ") true (setq _ip _ip" termIX "))"
               ) ; end begin
            ))) ; end Build condition for a general constant test

           ) ; end cond
       ;; Add the user condition to the term (if any)
       (if (or (= userCondition #void) (<> bnfCommand #void))
           (setq result (append result tmpResult))
           else
           (setq result (append "(and " tmpResult " " userCondition ")"))
           ) ; end if
       ;; Is this the condition for the first level of this rule?
       (cond
           ;; First test of a new top level rule group 
           ((and (= termIX 0) (> ruleIndex 0))
            (setq result (append "(begin (setq _ip _ip0)" _eol bk "   " result ")")))
           ;; Second+ test in a mid level rule group 
           ((and (> termIX 0) (> ruleIndex 0))
            (setq result (append "(begin (setq _ip _ip" termIX ")" _eol bk "   " result ")")))
           ) ; end cond
       result) ; end _condition
   ;; Fixup a Lisp sequence for $N and %N references
   (defun _fixup(lispSource)
       vars:(result)
       (setq result lispSource)
       (cond
          ;; Is this a condition for this rule?
          ((= (left result 2) "||")
           (setq result (mid result 2 (subi (length result) 4))))
          ;; Is this an action for this rule?
          ((or (= (left result 2) "::") (= (left result 2) "<<"))
           (setq result (mid result 2 (subi (length result) 4))))
          ;; Is this an argument list for this rule?
          ((= (right result 1) ")")
           (setq result (substitute (left result (subi (length result) 1)) "(" " " 1)))
          ;; Is this an user condition for this rule?
          ((= (right result 1) "}")
           (setq result (mid result 1 (subi (length result) 2))))
           ) ; end cond
       ;; Fix up all references to $N variables 
       (setq result (substitute result "$" "_tk"))
       ;; Fix up all references to %N variables 
       (setq result (substitute result "%" "_ak")) 
       result) ; end _fixup
   ;; Ouput condition graph for the rules
   (defun _generateSource(d bk termIX)
       vars:(i)
       ;; Initialize the condition clause for this level of the rule
       (if (> termIX 0)
           (setq _result (append _result bk "(begin (setq _ruleCount (+ _ruleCount 1)) (if (and (> _verboseTrigger 0) (> _ruleCount _verboseTrigger) (not _verbose)) (_startLog)) (setq _ip" termIX " _ip)" _eol)))
       ;; Is this a new condition for this level of the rule?
       (loop for i from 0 until (length d) do
           ;; Is this a rule firing, or just another case test?
           (if (= d[i 0] "_default")
               (begin
                  (setq _result (append _result bk "  ;; *********************************************************" _eol))
                  (setq _result (append _result bk "  ;; RULE: " d[i 1][2] _eol))
                  (setq _result (append _result bk "  ;; *********************************************************" _eol))
                  ) ; end then begin
               (begin
                  (setq _result (append _result bk "  ;; ====================" _eol))
                  (setq _result (append _result bk "  ;; case: " d[i 0] _eol))
                  (setq _result (append _result bk "  ;; ====================" _eol))
                  ) ; end else begin
               ) ; end if
           (setq _result (append _result bk "  (if " (_condition i d[i 0] d[i 1] termIX bk) _eol))
           (if (_generateTest d[i 1])
               (_generateSource d[i 1] (append bk "    ") (addi termIX 1))
               (setq _result (append _result bk "  " (_action i d[i 1][1] d[i 1][2] bk termIX) _eol))
               ) ; end if
           (setq _result (append _result bk "  ) ; end case : " d[i 0] _eol))
           ) ; end loop
       (if (> termIX 0)
           (setq _result (append _result bk ") ; end begin" _eol)))
       ) ; end _generateSource
   ;; ********************************************************
   ;; Begin the _outputOneSyntaxRule function
   ;; ********************************************************
   ;; Determine the rule ordering (the default is user ordering).
   (setq _userOrderingSW false)
   (setq _generateTest isDirectory)
   (if (= v[0][0] #(user ordering))
       (begin
          (setq _userOrderingSW true)
          (setq _generateTest isLambda)
          (delete v 0)
          )) ; end if
   ;; Compute the maximum number of terms in the largest rule
   (setq _name name)
   (if _userOrderingSW
      (setq d (new rulesDirectory))
      (setq d (new Directory:))
      ) ; end if
   (setq _result "")
   (loop for i from 0 until (length v) do
       (_buildRuleTree d 0 v[i])
       ) ; end loop
   (_generateSource d "   " 0)
   _result) ; end _lexical_outputARule



































;;**EXPORTKEY**:ParseLib:_lexical_outputRules
(defchild ParseLib:_lexical_outputRules()
;; *******************************************************************
;; summary:  Output the Lexical Rules as child Lambdas. Each rule is
;;           output as a separate child Lambda. The header and trailer
;;           for each child Lambda are templates. The rule clauses form
;;           the body of the chgild Lambda and are output by the
;;           _lexical_outputARule function.
;; Parms:    none       
;; return:   _result    Always true.
;; *******************************************************************
   vars:(ruleIndex itemIndex result temp ruleName ruleNameExt ruleItems)
   ;; Set the rule name prefix to Syntax Rules.
   (setq _rulePrefix "_LEXRULE_")
   ;; Compose the main header to the user defined rules section
   ;; Output each group of like named rules
   (loop for ruleIndex from 0 until (length _lexicalRuleDictionary) do
       (setq ruleName _lexicalRuleDictionary[ruleIndex 0])
       (setq ruleNameExt (append _rulePrefix _lexicalRuleDictionary[ruleIndex 0]))
       (setq ruleItems _lexicalRuleDictionary[ruleIndex 1])
       ;; Compose the rule header for this user defined rule
       (setq temp (mid (browseLib.checkout "ParseLib:%LEXICAL_RULEHEADER") (addi 7 (length _eol)) 100000))
       (if (= _friendCompiler true)
           (setq temp (substitute temp "#??#" (append _nameDef " ")))
           (setq temp (substitute temp "#??#" (append _nameDef ":")))
           ) ; end if
       (setq result (substitute (substitute temp "#!!#" ruleNameExt) "#%%#" ruleName))
       (setq result (append result (_lexical_outputARule ruleName ruleItems)))
       ;; Compose the rule trailer for the previous user defined rule
       (setq temp (mid (browseLib.checkout "ParseLib:%LEXICAL_RULETRAILER") (addi 7 (length _eol)) 100000))
       (if (= _friendCompiler true)
           (setq temp (substitute temp "#??#" (append _nameDef " ")))
           (setq temp (substitute temp "#??#" (append _nameDef ":")))
           ) ; end if
       (setq result (append result (substitute (substitute temp "#!!#" ruleNameExt) "#%%#" ruleName)))
       ;; Compose and checkin this user defined rule for the compiler
       (browseLib.checkin _cabinetName (symbol (append _name ":" ruleNameExt)) result)
       ) ; end _lexicalRuleDictionary loop
   true) ;; end _lexical_outputRules





;;**EXPORTKEY**:ParseLib:_lexical_saveARule
(defchild ParseLib:_lexical_saveARule(ruleName ruleItems condition action)
;; *******************************************************************
;; summary:  Save a single Syntax Rule in a rule dictionary. Each rule is
;;           output as a separate child Lambda. The header and trailer
;;           for each child Lambda are templates. The rule clauses form
;;           the body of the chgild Lambda and are output by the
;;           _lexical_outputARule function.
;; Parms:    ruleName   The name of the lexical rule (always uppercase)
;;           ruleItems  Each iemized condition for this rule (ie Name + Name)
;;           condition  Any trailing rule condition (ie || (= $1.Value "if") ||)
;;           action     Any trailing rule action (ie :: (setq $0.Value $1.Value) ::)      
;; return:   _result    A vector of rule specifications.
;; *******************************************************************
   vars:(i itemLen temp term ruleID repeatSW result)
   ;; Compose the header to this user defined rule
   (setq ruleID (append ruleName ":"))
   (setq itemLen (length ruleItems))
   (loop for i from 0 until itemLen do
       (if (<> ruleItems[i] #void)
           (setq ruleID (append ruleID " " ruleItems[i])))
       ) ; end loop
   (if (<> condition #void)
       (setq ruleID (append ruleID " " condition)))
   (if (<> action #void)
       (setq ruleID (append ruleID " " action)))
   ;; Compose the condition for this user defined rule
   (setq result (new Vector: 4 ruleItems condition action ruleID))
   result) ;; end _lexical_saveARule






















;;**EXPORTKEY**:ParseLib:_lexical_saveRules
(defchild ParseLib:_lexical_saveRules(name words condition action)
;; *******************************************************************
;; summary:  Save all Syntax Rule in the rule dictionary. Each rule is
;;           output as a separate child Lambda. The header and trailer
;;           for each child Lambda are templates. The rule clauses form
;;           the body of the child Lambda.
;; Parms:    name       The name of the lexical rule (always uppercase)
;;           words      Each iemized condition for this rule (ie Name + Name)
;;           condition  Any trailing rule condition (ie || (= $1.Value "if") ||)
;;           action     Any trailing rule action (ie :: (setq $0.Value $1.Value) ::)      
;; return:   _result    A vector of rule specifications.
;; *******************************************************************
   vars:(i wordLen rules)
   (setq rules _lexicalRuleDictionary[name])
   (if (= rules #void) (setq rules (new Vector: 0)))
   ;; Add the new rule primatives to the rules vector
   (setq rules[(length rules)] (_lexical_saveARule name words condition action))
   (setq _lexicalRuleDictionary[name] rules)
   ;; Check for empty condition errors in the new rule.
   (if (= words[0] #void)
       (error (append "Empty condition Error in " name ": ...use Nop special rule syntax for unconditional rules")))
   true) ; end _lexical_saveRules






















;;**EXPORTKEY**:ParseLib:_semantic_outputARule
(defriend ParseLib:_semantic_outputARule(name v)
;; *******************************************************************
;; summary:  This Lambda converts a vector of rules into a output
;;           Lisp source string. This output forms the core of the
;;           generated Lisp rule implementation. The rules are
;;           sorted so that the least expensive approach may be
;;           taken to implementin them. (see the RTN ideas in
;;           "Natural Languiage Processing in Lisp".
;; Notes:    The rule vector (v) is a vector of user defined rule
;;           clauses all having the same rule name (name). Each rule 
;;           is a vector as follows:
;;                 clause[0]   Vector of rule terms
;;                 clause[1]   condition
;;                 clause[2]   action
;;                 clause[3]   rulePrettyPrintForm           
;; Parms:    name       The name of the user defined rule.
;;           v          The vector of the user defined rule.
;; return:   _result    The Lisp source string implementing the rule.
;; *******************************************************************
   pvars:(_name              ;; Name of the user defined rule
          _result            ;; Resulting user defined Lisp action source
          _userOrderingSW    ;; True if user ordering is turned on
          _generateTest      ;; isLambda for user ordering, isDirectory for auto ordering
          ;; Child methods
          _action            ;; Output an action for a rule 
          _buildRuleTree     ;; Build a condition graph for the rules
          _condition         ;; Output a condition for a rule    
          _fixup             ;; Fixup a Lisp sequence for $N and %N references
          _generateSource    ;; Ouput condition graph for the rules
         ) ; end persistant variables
   vars:(n i d)
   ;; Output an action for a rule
   (defun _action(ruleIndex action ruleID bk termIX)
      vars:(i (result "") term repeatSW)
       ;; Compose the action for this user defined rule
       (setq repeatSW (= (left action 2) "<<"))
       (setq action (_fixup action))
       (setq result (append result    " (begin" _eol))

       ;; Is this a default action in a mid level rule group?
       (if (and (> termIX 0) (> ruleIndex 0))
           (setq result (append result bk "     (setq _ip _ip" termIX ")" _eol))
           ) ; end if

       (setq result (append result bk "     (setq _ret " action ")" _eol))
       (setq result (append result bk "     (if _verbose " _eol 
                                   bk "         (writeRule" _eol 
                                   bk "              {" ruleID "}" _eol
                                   bk "              _ret ")) 
       (loop for i from 0 to termIX do
           (setq result (append result " _tk" i))
           ) ; end loop
       (loop for i from termIX until 10 do
           (setq result (append result " #void"))
           ) ; end loop
       (setq result (append result "))" _eol "(setq _indent (isub _indent))" _eol))
       (if repeatSW
          (setq result (append result bk "     (goto Skip:))"))
          (setq result (append result bk "     (return _ret))"))
          ) ; end if
       result) ; end _action
   ;; Build a condition graph for the rules
   (defun _buildRuleTree(ruleTree i ruleClause)
       vars:(d2)
       ;; Is this the default condition for this rule?
       ;; Note the ruleClause contains [0]=term vector,[1]=condition,[2]=action,[3]=ruleID
       (if (= ruleClause[0][i] #void) 
           (begin 
              (setq ruleTree._default (new Vector: 3 ruleClause[1] ruleClause[2] ruleClause[3])) 
              (return ruleTree)
              )) ; end if
       ;; Is this a new condition for this level of the rule?
       (if (= ruleTree[ruleClause[0][i]] #void) 
           (if _userOrderingSW
              (setq ruleTree[ruleClause[0][i]] (setq d2 (new rulesDirectory)))
              (setq ruleTree[ruleClause[0][i]] (setq d2 (new Directory:)))
              ) ; end if
           ) ; end if
       (_buildRuleTree ruleTree[ruleClause[0][i]] (addi i 1) ruleClause)) ; end _buildRuleTree
   ;; Output a condition for a rule
   (defun _condition(ruleIndex termName ruleClause termIX bk)
       vars:(condition (result "") term pos userCondition tmpResult termLen bnfCommand)
       ;; Is this the default condition for this rule?
       ;; Note the ruleClause contains [0]=condition,[1]=action,[2]=ruleID
       (if (= termName "_default") 
           (begin
              (setq condition ruleClause[0])
              (if (<> condition #void)
                  (begin
                     (setq condition (_fixup condition))
                     (return condition)
                     )) ; end if
              (return "true")
              )) ; end default if
       ;; Is this a new condition for this level of the rule?
       (setq term termName)
       ;; Remove any trailing BNF commands from the term.
       (setq bnfCommand #void)
       (if (and (isCharName (left term 1))
                (or (= (right term 1) "*") (= (right term 1) "?") (= (right term 1) "+")))
           (begin
              (setq termLen (length term))
              (setq bnfCommand (right term 1))
              (setq term (symbol (left term (sub1 termLen))))        
              )) ; end if
       ;; Remove any user condition from the term.
       (setq userCondition #void)
       (if (= (right term 1) "}")
           (begin
              (setq pos (find "{" term))
              (setq userCondition (_fixup (mid term pos 10000)))
              (setq term (symbol (trim (left term pos))))        
              )) ; end if
         (cond
           ;; Build condition for a Start Pair test
           ((and (isSymbol term) (= term |[|:))
            (setq tmpResult (append "(if (isPair (setq _tk" (addi termIX 1) " (_getToken))) "
                                         "(_pushIp) "
                                         "(_lastIp))")))

           ;; Build condition for an End Pair test
           ((and (isSymbol term) (= term |]|:))
            (setq tmpResult (append "(if (_eofToken) (_popIp) false)")))

           ;; Build condition for a user defined rule with argument passing
           ((= (right term 1) ")")
            ;; We must generate code based upon the associated trailing BNF command.
            (cond
               ;; BNF command is none.
               ((= bnfCommand #void)
                (setq tmpResult (append 
                          "(if (<> (setq _tk" (addi termIX 1) " (" (_fixup (append _rulePrefix term)) ")) morphFail)"
                               "true "
                               "(setq _ip _ip" termIX "))"
                          ))
                ) ; end BNF command is none
               ;; BNF command is *.
               ((= bnfCommand "*")
                (if (= userCondition #void)
                    then
                    (setq tmpResult (append 
                          "(begin "
                              "(setq _i 0) " 
                              "(setq _tk" (addi termIX 1) " (new Vector: 0)) " 
                              "(while " 
                                  "(<> (setq _tkthis (" (_fixup (append _rulePrefix term)) ")) morphFail) do "
                                  "(begin "
                                     "(setq _tk" (addi termIX 1) "[_i] _tkthis) "
                                     "(setq _i (iadd _i 1)) " 
                                  ")) " 
                              "true)"
                        ))
                    else
                    (setq tmpResult (append 
                          "(begin "
                              "(setq _i 0) " 
                              "(setq _tk" (addi termIX 1) " (new Vector: 0)) " 
                              "(while " 
                                  "(and "
                                    "(<> (setq _tkthis (" (_fixup (append _rulePrefix term)) ")) morphFail) "
                                     userCondition " ) do "
                                  "(begin "
                                     "(setq _tk" (addi termIX 1) "[_i] _tkthis) "
                                     "(setq _i (iadd _i 1)) " 
                                  ")) " 
                              "true)"
                        )))
                ) ; end BNF command is *
               ;; BNF command is +.
               ((= bnfCommand "+")
                (if (= userCondition #void)
                    then
                    (setq tmpResult (append 
                          "(begin "
                              "(setq _i 0) " 
                              "(setq _tk" (addi termIX 1) " (new Vector: 0)) " 
                              "(while " 
                                  "(<> (setq _tkthis (" (_fixup (append _rulePrefix term)) ")) morphFail) do "
                                  "(begin "
                                     "(setq _tk" (addi termIX 1) "[_i] _tkthis) "
                                     "(setq _i (iadd _i 1)) " 
                                  ")) " 
                              "(if (> _i 0) true (setq _ip _ip" termIX ")))"
                        ))
                    else
                    (setq tmpResult (append 
                          "(begin "
                              "(setq _i 0) " 
                              "(setq _tk" (addi termIX 1) " (new Vector: 0)) " 
                              "(while " 
                                  "(and "
                                    "(<> (setq _tkthis (" (_fixup (append _rulePrefix term)) ")) morphFail) "
                                     userCondition " ) do "
                                  "(begin "
                                     "(setq _tk" (addi termIX 1) "[_i] _tkthis) "
                                     "(setq _i (iadd _i 1)) " 
                                  ")) " 
                              "(if (> _i 0) true (setq _ip _ip" termIX ")))"
                        )))                      
                ) ; end BNF command is +
               ;; BNF command is ?.
               ((= bnfCommand "?")
                (if (= userCondition #void)
                    then
                    (setq tmpResult (append 
                          "(if (<> (setq _tk" (addi termIX 1) " (" (_fixup (append _rulePrefix term)) ")) morphFail) "
                               "true "
                               "(begin "
                                  "(setq _tk" (addi termIX 1) " #void) "
                                  "true))"
                          ))
                    else
                    (setq tmpResult (append 
                          "(if "
                             "(and "
                               "(<> (setq _tk" (addi termIX 1) " (" (_fixup (append _rulePrefix term)) ")) morphFail) "
                                userCondition " ) "
                               "true "
                               "(begin "
                                  "(setq _tk" (addi termIX 1) " #void) "
                                  "true))"
                          )))
                ) ; end BNF command is ?
               ) ; end BNF cond            
            ) ; end Build condition for a user defined rule with argument passing


           ;; Build condition for a user defined rule
           ((and (isSymbol term) (isCharUppercase term))
            ;; We must generate code based upon the associated trailing BNF command.
            (cond
               ;; BNF command is none.
               ((= bnfCommand #void)
                (setq tmpResult (append 
                          "(if (<> (setq _tk" (addi termIX 1) " (" (append _rulePrefix term) ")) morphFail)"
                               "true "
                               "(setq _ip _ip" termIX "))"
                          ))
                ) ; end BNF command is none
               ;; BNF command is *.
               ((= bnfCommand "*")
                (if (= userCondition #void)
                    then
                    (setq tmpResult (append 
                          "(begin "
                              "(setq _i 0) " 
                              "(setq _tk" (addi termIX 1) " (new Vector: 0)) " 
                              "(while " 
                                  "(<> (setq _tkthis (" (append _rulePrefix term) ")) morphFail) do "
                                  "(begin "
                                     "(setq _tk" (addi termIX 1) "[_i] _tkthis) "
                                     "(setq _i (iadd _i 1)) " 
                                  ")) " 
                              "true)"
                        ))
                    else
                    (setq tmpResult (append 
                          "(begin "
                              "(setq _i 0) " 
                              "(setq _tk" (addi termIX 1) " (new Vector: 0)) " 
                              "(while " 
                                  "(and "
                                    "(<> (setq _tkthis (" (append _rulePrefix term) ")) morphFail) "
                                     userCondition " ) do "
                                  "(begin "
                                     "(setq _tk" (addi termIX 1) "[_i] _tkthis) "
                                     "(setq _i (iadd _i 1)) " 
                                  ")) " 
                              "true)"
                        )))
                ) ; end BNF command is *
               ;; BNF command is +.
               ((= bnfCommand "+")
                (if (= userCondition #void)
                    then
                    (setq tmpResult (append 
                          "(begin "
                              "(setq _i 0) " 
                              "(setq _tk" (addi termIX 1) " (new Vector: 0)) " 
                              "(while " 
                                  "(<> (setq _tkthis (" (append _rulePrefix term) ")) morphFail) do "
                                  "(begin "
                                     "(setq _tk" (addi termIX 1) "[_i] _tkthis) "
                                     "(setq _i (iadd _i 1)) " 
                                  ")) " 
                              "(if (> _i 0) true (setq _ip _ip" termIX ")))"
                        ))
                    else
                    (setq tmpResult (append 
                          "(begin "
                              "(setq _i 0) " 
                              "(setq _tk" (addi termIX 1) " (new Vector: 0)) " 
                              "(while " 
                                  "(and "
                                    "(<> (setq _tkthis (" (append _rulePrefix term) ")) morphFail) "
                                     userCondition " ) do "
                                  "(begin "
                                     "(setq _tk" (addi termIX 1) "[_i] _tkthis) "
                                     "(setq _i (iadd _i 1)) " 
                                  ")) " 
                              "(if (> _i 0) true (setq _ip _ip" termIX ")))"
                        )))                      
                ) ; end BNF command is +
               ;; BNF command is ?.
               ((= bnfCommand "?")
                (if (= userCondition #void)
                    then
                    (setq tmpResult (append 
                          "(if (<> (setq _tk" (addi termIX 1) " (" (append _rulePrefix term) ")) morphFail) "
                               "true "
                               "(begin "
                                  "(setq _tk" (addi termIX 1) " #void) "
                                  "true))"
                          ))
                    else
                    (setq tmpResult (append 
                          "(if "
                             "(and "
                               "(<> (setq _tk" (addi termIX 1) " (" (append _rulePrefix term) ")) morphFail) "
                                userCondition " ) "
                               "true "
                               "(begin "
                                  "(setq _tk" (addi termIX 1) " #void) "
                                  "true))"
                          )))
                ) ; end BNF command is ?
               ) ; end BNF cond            
            ) ; end Build condition for a user defined rule

           ;; Build condition for an Eof test
           ((and (isSymbol term) (= term Eof:))
            (setq tmpResult (append "(if (_eofToken)"
                                                     "true (setq _ip _ip" termIX "))")))

           ;; Build condition for a Nop test
           ((and (isSymbol term) (= term Nop:))
            (setq tmpResult (append "(begin (setq _tk" (addi termIX 1) " #void) true)")))

           ;; Build condition for an Any test
           ((and (isSymbol term) (= term Any:))
            ;; We must generate code based upon the associated trailing BNF command.
            (cond
               ;; BNF command is none.
               ((= bnfCommand #void)
                (setq tmpResult (append 
                          "(if (<> (setq _tk" (addi termIX 1) " (_getToken)) morphFail) "
                               "true "
                               "(setq _ip _ip" termIX "))"
                          ))
                ) ; end BNF command is none
               ;; BNF command is *.
               ((= bnfCommand "*")
                (if (= userCondition #void)
                    then
                    (setq tmpResult (append 
                          "(begin "
                              "(setq _i 0) " 
                              "(setq _tk" (addi termIX 1) " (new Vector: 0)) " 
                              "(while " 
                                  "(<> (setq _tkthis (_getToken)) morphFail) do "
                                  "(begin "
                                     "(setq _tk" (addi termIX 1) "[_i] _tkthis) "
                                     "(setq _i (iadd _i 1)) " 
                                  ")) " 
                              "(setq _ip (isub _ip 1)) "
                              "true)"
                        ))
                    else
                    (setq tmpResult (append 
                          "(begin "
                              "(setq _i 0) " 
                              "(setq _tk" (addi termIX 1) " (new Vector: 0)) " 
                              "(while " 
                                  "(and "
                                    "(<> (setq _tkthis (_getToken)) morphFail) "
                                     userCondition ") do "
                                  "(begin "
                                     "(setq _tk" (addi termIX 1) "[_i] _tkthis) "
                                     "(setq _i (iadd _i 1)) " 
                                  ")) " 
                              "(setq _ip (isub _ip 1)) "
                              "true)"
                        )))
                ) ; end BNF command is *
               ;; BNF command is +.
               ((= bnfCommand "+")
                (if (= userCondition #void)
                    then
                    (setq tmpResult (append 
                          "(begin "
                              "(setq _i 0) " 
                              "(setq _tk" (addi termIX 1) " (new Vector: 0)) " 
                              "(while " 
                                  "(<> (setq _tkthis (_getToken)) morphFail) do "
                                  "(begin "
                                     "(setq _tk" (addi termIX 1) "[_i] _tkthis) "
                                     "(setq _i (iadd _i 1)) " 
                                  ")) " 
                              "(setq _ip (isub _ip 1)) "
                              "(if (> _i 0) true (setq _ip _ip" termIX ")))"
                        ))
                    else
                    (setq tmpResult (append 
                          "(begin "
                              "(setq _i 0) " 
                              "(setq _tk" (addi termIX 1) " (new Vector: 0)) " 
                              "(while " 
                                  "(and "
                                    "(<> (setq _tkthis (_getToken)) morphFail) "
                                     userCondition ") do "
                                  "(begin "
                                     "(setq _tk" (addi termIX 1) "[_i] _tkthis) "
                                     "(setq _i (iadd _i 1)) " 
                                  ")) " 
                              "(setq _ip (isub _ip 1)) "
                              "(if (> _i 0) true (setq _ip _ip" termIX ")))"
                        )))
                ) ; end BNF command is +
               ;; BNF command is ?.
               ((= bnfCommand "?")
                (if (= userCondition #void)
                    then
                    (setq tmpResult (append 
                          "(if (<> (setq _tk" (addi termIX 1) " (_getToken)) morphFail) "
                               "true "
                               "(begin "
                                  "(setq _ip (isub _ip 1)) "
                                  "(setq _tk" (addi termIX 1) " #void) "
                                  "true))"
                          ))
                    else
                    (setq tmpResult (append 
                          "(if "
                            "(and "
                              "(<> (setq _tk" (addi termIX 1) " (_getToken)) morphFail) "
                               userCondition ") "
                               "true "
                               "(begin "
                                  "(setq _ip (isub _ip 1)) "
                                  "(setq _tk" (addi termIX 1) " #void) "
                                  "true))"
                          )))
                ) ; end BNF command is ?
               ) ; end BNF cond            
            ) ; end Build condition for an Any or Self test

           ;; Build condition for an feature test
           ((and (isSymbol term) (isCharAlphanumeric term))
            ;; We must generate code based upon the associated trailing BNF command.
            (cond
               ;; BNF command is none.
               ((= bnfCommand #void)
                (setq tmpResult (append 
                          "(if (<> (setq _tk" (addi termIX 1) " (_getToken))[" term ":] #void) "
                               "true "
                               "(setq _ip _ip" termIX "))"
                          ))
                ) ; end BNF command is none
               ;; BNF command is *.
               ((= bnfCommand "*")
                (if (= userCondition #void)
                    then
                    (setq tmpResult (append 
                          "(begin "
                              "(setq _i 0) " 
                              "(setq _tk" (addi termIX 1) " (new Vector: 0)) " 
                              "(while " 
                                  "(<> (setq _tkthis (_getToken))[" term ":] #void) do "
                                  "(begin "
                                     "(setq _tk" (addi termIX 1) "[_i] _tkthis) "
                                     "(setq _i (iadd _i 1)) " 
                                  ")) " 
                              "(setq _ip (isub _ip 1)) "
                              "true)"
                        ))
                    else
                    (setq tmpResult (append 
                          "(begin "
                              "(setq _i 0) " 
                              "(setq _tk" (addi termIX 1) " (new Vector: 0)) " 
                              "(while " 
                                  "(and "
                                    "(<> (setq _tkthis (_getToken))[" term ":] #void) "
                                     userCondition " ) do "
                                  "(begin "
                                     "(setq _tk" (addi termIX 1) "[_i] _tkthis) "
                                     "(setq _i (iadd _i 1)) " 
                                  ")) " 
                              "(setq _ip (isub _ip 1)) "
                              "true)"
                        )))
                ) ; end BNF command is *
               ;; BNF command is +.
               ((= bnfCommand "+")
                (if (= userCondition #void)
                    then
                    (setq tmpResult (append 
                          "(begin "
                              "(setq _i 0) " 
                              "(setq _tk" (addi termIX 1) " (new Vector: 0)) " 
                              "(while " 
                                  "(<> (setq _tkthis (_getToken))[" term ":] #void) do "
                                  "(begin "
                                     "(setq _tk" (addi termIX 1) "[_i] _tkthis) "
                                     "(setq _i (iadd _i 1)) " 
                                  ")) " 
                              "(setq _ip (isub _ip 1)) "
                              "(if (> _i 0) true (setq _ip _ip" termIX ")))"
                        ))
                    else
                    (setq tmpResult (append 
                          "(begin "
                              "(setq _i 0) " 
                              "(setq _tk" (addi termIX 1) " (new Vector: 0)) " 
                              "(while " 
                                  "(and "
                                    "(<> (setq _tkthis (_getToken))[" term ":] #void) "
                                     userCondition " ) do "
                                  "(begin "
                                     "(setq _tk" (addi termIX 1) "[_i] _tkthis) "
                                     "(setq _i (iadd _i 1)) " 
                                  ")) " 
                              "(setq _ip (isub _ip 1)) "
                              "(if (> _i 0) true (setq _ip _ip" termIX ")))"
                        )))
                ) ; end BNF command is +
               ;; BNF command is ?.
               ((= bnfCommand "?")
                (if (= userCondition #void)
                    then
                    (setq tmpResult (append 
                          "(if (<> (setq _tk" (addi termIX 1) " (_getToken))[" term ":] #void) "
                               "true "
                               "(begin "
                                  "(setq _ip (isub _ip 1)) "
                                  "(setq _tk" (addi termIX 1) " #void) "
                                  "true))"
                          ))
                    else
                    (setq tmpResult (append 
                          "(if "
                            "(and "
                              "(<> (setq _tk" (addi termIX 1) " (_getToken))[" term ":] #void) "
                               userCondition " ) "
                               "true "
                               "(begin "
                                  "(setq _ip (isub _ip 1)) "
                                  "(setq _tk" (addi termIX 1) " #void) "
                                  "true))"
                          )))
                ) ; end BNF command is ?
               ) ; end BNF cond            
            ) ; end Build condition for a feature test

           ;; Build condition for a symbol constant test
           ((isSymbol term)
            (setq tmpResult (append "(if (= (setq _tk" (addi termIX 1) 
                                                     " (_getToken))[Value:] |" term "|:) true (setq _ip _ip" termIX "))")))

           ;; Build condition for a constant test
           (else
            (setq tmpResult (append "(if (= (setq _tk" (addi termIX 1) 
                                                     " (_getToken))[Value:] " term ") true (setq _ip _ip" termIX "))")))

           ) ; end cond
       ;; Add the user condition to the term (if any)
       (if (or (= userCondition #void) (<> bnfCommand #void))
           (setq result (append result tmpResult))
           else
           (setq result (append "(and " tmpResult " " userCondition ")"))
           ) ; end if
       ;; Is this the condition for the first level of this rule?
       (cond
           ;; First test of a new top level rule group 
           ((and (= termIX 0) (> ruleIndex 0))
            (setq result (append "(begin (setq _ip _ip0)" _eol bk "   " result ")")))
           ;; Second+ test in a mid level rule group 
           ((and (> termIX 0) (> ruleIndex 0))
            (setq result (append "(begin (setq _ip _ip" termIX ")" _eol bk "   " result ")")))
           ) ; end cond
       result) ; end _condition
   ;; Fixup a Lisp sequence for $N and %N references
   (defun _fixup(lispSource)
       vars:(result)
       (setq result lispSource)
       (cond
          ;; Is this a condition for this rule?
          ((= (left result 2) "||")
           (setq result (mid result 2 (subi (length result) 4))))
          ;; Is this an action for this rule?
          ((or (= (left result 2) "::") (= (left result 2) "<<"))
           (setq result (mid result 2 (subi (length result) 4))))
          ;; Is this an argument list for this rule?
          ((= (right result 1) ")")
           (setq result (substitute (left result (subi (length result) 1)) "(" " " 1)))
          ;; Is this an user condition for this rule?
          ((= (right result 1) "}")
           (setq result (mid result 1 (subi (length result) 2))))
           ) ; end cond
       ;; Fix up all references to $N variables 
       (setq result (substitute result "$" "_tk"))
       ;; Fix up all references to %N variables 
       (setq result (substitute result "%" "_ak")) 
       result) ; end _fixup
   ;; Ouput condition graph for the rules
   (defun _generateSource(d bk termIX)
       vars:(i)
       ;; Initialize the condition clause for this level of the rule
       (if (> termIX 0)
           (setq _result (append _result bk "(begin (setq _ruleCount (+ _ruleCount 1)) (if (and (> _verboseTrigger 0) (> _ruleCount _verboseTrigger) (not _verbose)) (_startLog)) (setq _ip" termIX " (copy _ip))" _eol)))
       ;; Is this a new condition for this level of the rule?
       (loop for i from 0 until (length d) do
           ;; Is this a rule firing, or just another case test?
           (if (= d[i 0] "_default")
               (begin
                  (setq _result (append _result bk "  ;; *********************************************************" _eol))
                  (setq _result (append _result bk "  ;; RULE: " d[i 1][2] _eol))
                  (setq _result (append _result bk "  ;; *********************************************************" _eol))
                  ) ; end then begin
               (begin
                  (setq _result (append _result bk "  ;; ====================" _eol))
                  (setq _result (append _result bk "  ;; case: " d[i 0] _eol))
                  (setq _result (append _result bk "  ;; ====================" _eol))
                  ) ; end else begin
               ) ; end if
           (setq _result (append _result bk "  (if " (_condition i d[i 0] d[i 1] termIX bk) _eol))
           (if (_generateTest d[i 1])
               (_generateSource d[i 1] (append bk "    ") (addi termIX 1))
               (setq _result (append _result bk "  " (_action i d[i 1][1] d[i 1][2] bk termIX) _eol))
               ) ; end if
           (setq _result (append _result bk "  ) ; end case : " d[i 0] _eol))
           ) ; end loop
       (if (> termIX 0)
           (setq _result (append _result bk ") ; end begin" _eol)))
       ) ; end _generateSource
   ;; ********************************************************
   ;; Begin the _outputOneSemanticRule function
   ;; ********************************************************
   ;; Determine the rule ordering (the default is user ordering).
   (setq _userOrderingSW false)
   (setq _generateTest isDirectory)
   (if (= v[0][0] #(user ordering))
       (begin
          (setq _userOrderingSW true)
          (setq _generateTest isLambda)
          (delete v 0)
          )) ; end if
   ;; Compute the maximum number of terms in the largest rule
   (setq _name name)
   (if _userOrderingSW
      (setq d (new rulesDirectory))
      (setq d (new Directory:))
      ) ; end if
   (setq _result "")
   (loop for i from 0 until (length v) do
       (_buildRuleTree d 0 v[i])
       ) ; end loop
   (_generateSource d "   " 0)
   _result) ; end _semantic_outputARule












;;**EXPORTKEY**:ParseLib:_semantic_outputRules
(defchild ParseLib:_semantic_outputRules()
;; *******************************************************************
;; summary:  Output the Semantic Rules as child Lambdas. Each rule is
;;           output as a separate child Lambda. The header and trailer
;;           for each child Lambda are templates. The rule clauses form
;;           the body of the chgild Lambda and are output by the
;;           _semantic_outputARule function.
;; Parms:    none       
;; return:   _result    Always true.
;; *******************************************************************
   vars:(ruleIndex itemIndex result temp ruleName ruleNameExt ruleItems)
   ;; Set the rule name prefix to Semantic Rules.
   (setq _rulePrefix "_SEMRULE_")
   ;; Compose the main header to the user defined rules section
   ;; Output each group of like named rules
   (loop for ruleIndex from 0 until (length _semanticRuleDictionary) do
       (setq ruleName _semanticRuleDictionary[ruleIndex 0])
       (setq ruleNameExt (append _rulePrefix _semanticRuleDictionary[ruleIndex 0]))
       (setq ruleItems _semanticRuleDictionary[ruleIndex 1])
       ;; Compose the rule header for this user defined rule
       (setq temp (mid (browseLib.checkout "ParseLib:%SEMANTIC_RULEHEADER") (addi 7 (length _eol)) 100000))
       (if (= _friendCompiler true)
           (setq temp (substitute temp "#??#" (append _nameDef " ")))
           (setq temp (substitute temp "#??#" (append _nameDef ":")))
           ) ; end if
       (setq result (substitute (substitute temp "#!!#" ruleNameExt) "#%%#" ruleName))
       (setq result (append result (_semantic_outputARule ruleName ruleItems)))
       ;; Compose the rule trailer for the previous user defined rule
       (setq temp (mid (browseLib.checkout "ParseLib:%SEMANTIC_RULETRAILER") (addi 7 (length _eol)) 100000))
       (if (= _friendCompiler true)
           (setq temp (substitute temp "#??#" (append _nameDef " ")))
           (setq temp (substitute temp "#??#" (append _nameDef ":")))
           ) ; end if
       (setq result (append result (substitute (substitute temp "#!!#" ruleNameExt) "#%%#" ruleName)))
       ;; Compose and checkin this user defined rule for the compiler
       (browseLib.checkin _cabinetName (symbol (append _name ":" ruleNameExt)) result)
       ) ; end _semanticRuleDictionary loop
   true) ;; end _semantic_outputRules





;;**EXPORTKEY**:ParseLib:_semantic_saveARule
(defchild ParseLib:_semantic_saveARule(ruleName ruleItems condition action)
;; *******************************************************************
;; summary:  Save a single Semantic Rule in a rule dictionary. Each rule is
;;           output as a separate child Lambda. The header and trailer
;;           for each child Lambda are templates. The rule clauses form
;;           the body of the chgild Lambda and are output by the
;;           _semantic_outputARule function.
;; Parms:    ruleName        The name of the semantic rule (always uppercase)
;;           ruleItems       Each iemized condition for this rule (ie Name + Name)
;;           condition       Any trailing rule condition (ie || (= $1.Value "if") ||)
;;           action          Any trailing rule action (ie :: (setq $0.Value $1.Value) ::)      
;; return:   _result    A vector of rule specifications.
;; *******************************************************************
   vars:(i itemLen temp term ruleID repeatSW result)
   ;; Compose the header to this user defined rule
   (setq ruleID (append ruleName ":"))
   (setq itemLen (length ruleItems))
   (loop for i from 0 until itemLen do
       (if (<> ruleItems[i] #void)
           (setq ruleID (append ruleID " " ruleItems[i])))
       ) ; end loop
   (if (<> condition #void)
       (setq ruleID (append ruleID " " condition)))
   (if (<> action #void)
       (setq ruleID (append ruleID " " action)))
   ;; Compose the condition for this user defined rule
   (setq result (new Vector: 4 ruleItems condition action ruleID))
   result) ;; end _semantic_saveARule






















;;**EXPORTKEY**:ParseLib:_semantic_saveRules
(defchild ParseLib:_semantic_saveRules(name words condition action)
;; *******************************************************************
;; summary:  Save all Semantic Rule in the rule dictionary. Each rule is
;;           output as a separate child Lambda. The header and trailer
;;           for each child Lambda are templates. The rule clauses form
;;           the body of the child Lambda.
;; Parms:    name       The name of the semantic rule (always uppercase)
;;           words      Each iemized condition for this rule (ie Name + Name)
;;           condition  Any trailing rule condition (ie || (= $1.Value "if") ||)
;;           action     Any trailing rule action (ie :: (setq $0.Value $1.Value) ::)      
;; return:   _result    A vector of rule specifications.
;; *******************************************************************
   vars:(i wordLen rules)
   (setq rules _semanticRuleDictionary[name])
   (if (= rules #void) (setq rules (new Vector: 0)))
   ;; Add the new rule primatives to the rules vector
   (setq rules[(length rules)] (_semantic_saveARule name words condition action))
   (setq _semanticRuleDictionary[name] rules)
   ;; Check for empty condition errors in the new rule.
   (if (= words[0] #void)
       (error (append "Empty condition Error in " name ": ...use Nop special rule semantic for unconditional rules")))
   true) ; end _semantic_saveRules






















;;**EXPORTKEY**:ParseLib:_syntax_outputARule
(defriend ParseLib:_syntax_outputARule(name v)
;; *******************************************************************
;; summary:  This Lambda converts a vector of rules into a output
;;           Lisp source string. This output forms the core of the
;;           generated Lisp rule implementation. The rules are
;;           sorted so that the least expensive approach may be
;;           taken to implementin them. (see the RTN ideas in
;;           "Natural Languiage Processing in Lisp".
;; Notes:    The rule vector (v) is a vector of user defined rule
;;           clauses all having the same rule name (name). Each rule 
;;           is a vector as follows:
;;                 clause[0]   Vector of rule terms
;;                 clause[1]   condition
;;                 clause[2]   action
;;                 clause[3]   rulePrettyPrintForm           
;; Parms:    name       The name of the user defined rule.
;;           v          The vector of the user defined rule.
;; return:   _result    The Lisp source string implementing the rule.
;; *******************************************************************
   pvars:(_name              ;; Name of the user defined rule
          _result            ;; Resulting user defined Lisp action source
          _userOrderingSW    ;; True if user ordering is turned on
          _generateTest      ;; isLambda for user ordering, isDirectory for auto ordering
          ;; Child methods
          _action            ;; Output an action for a rule 
          _buildRuleTree     ;; Build a condition graph for the rules
          _condition         ;; Output a condition for a rule    
          _fixup             ;; Fixup a Lisp sequence for $N and %N references
          _generateSource    ;; Ouput condition graph for the rules
         ) ; end persistant variables
   vars:(n i d)
   ;; Output an action for a rule
   (defun _action(ruleIndex action ruleID bk termIX)
      vars:(i (result "") term subSW repeatSW)
       ;; Compose the action for this user defined rule
	   (setq subSW false)
       (setq repeatSW (= (left action 2) "<<"))
	   (setq subSW (= (left action 2) "&&"))
       (setq action (_fixup action))
       (setq result (append result    " (begin" _eol))

       ;; Is this a default action in a mid level rule group?
       (if (and (> termIX 0) (> ruleIndex 0))
           (setq result (append result bk "     (setq _ip _ip" termIX ")" _eol))
           ) ; end if

       (setq result (append result bk "     (setq _ret " action ")" _eol))
       (setq result (append result bk "     (if _verbose " _eol 
                                   bk "         (writeRule" _eol 
                                   bk "              {" ruleID "}" _eol
                                   bk "              _ret ")) 
       (loop for i from 0 to termIX do
           (setq result (append result " _tk" i))
           ) ; end loop
       (loop for i from termIX until 10 do
           (setq result (append result " #void"))
           ) ; end loop
       (setq result (append result "))" _eol bk "     (setq _indent (isub _indent 1))" _eol  ))
       (if repeatSW
          (setq result (append result bk "     (goto Skip:))" _eol))
		  (if subSW
			(setq result (append result bk "     (_substitute _ip1 _ip"termIX " _ret)" _eol
										bk "     (goto Skip:))"))
          	(setq result (append result bk "     (return _ret))"))
			);end if
          ) ; end if
       result) ; end _action
   ;; Build a condition graph for the rules
   (defun _buildRuleTree(ruleTree i ruleClause)
       vars:(d2)
       ;; Is this the default condition for this rule?
       ;; Note the ruleClause contains [0]=term vector,[1]=condition,[2]=action,[3]=ruleID
       (if (= ruleClause[0][i] #void) 
           (begin 
              (setq ruleTree._default (new Vector: 3 ruleClause[1] ruleClause[2] ruleClause[3])) 
              (return ruleTree)
              )) ; end if
       ;; Is this a new condition for this level of the rule?
       (if (= ruleTree[ruleClause[0][i]] #void) 
           (if _userOrderingSW
              (setq ruleTree[ruleClause[0][i]] (setq d2 (new rulesDirectory)))
              (setq ruleTree[ruleClause[0][i]] (setq d2 (new Directory:)))
              ) ; end if
           ) ; end if
       (_buildRuleTree ruleTree[ruleClause[0][i]] (addi i 1) ruleClause)) ; end _buildRuleTree
   ;; Output a condition for a rule
   (defun _condition(ruleIndex termName ruleClause termIX bk)
       vars:(condition (result "") term termLen pos userCondition tmpResult bnfCommand)
       ;; Is this the default condition for this rule?
       ;; Note the ruleClause contains [0]=condition,[1]=action,[2]=ruleID
       (if (= termName "_default") 
           (begin
              (setq condition ruleClause[0])
              (if (<> condition #void)
                  (begin
                     (setq condition (_fixup condition))
                     (return condition)
                     )) ; end if
              (return "true")
              )) ; end default if
       ;; Is this a new condition for this level of the rule?
       (setq term termName)
       ;; Remove any trailing BNF commands from the term.
       (setq bnfCommand #void)
       (if (and (isCharName (left term 1))
                (or (= (right term 1) "*") (= (right term 1) "?") (= (right term 1) "+")))
           (begin
              (setq termLen (length term))
              (setq bnfCommand (right term 1))
              (setq term (symbol (left term (sub1 termLen))))        
              )) ; end if
       ;; Remove any user condition from the term.
       (setq userCondition #void)
       (if (= (right term 1) "}")
           (begin
              (setq pos (find "{" term))
              (setq userCondition (_fixup (mid term pos 10000)))
              (setq term (symbol (trim (left term pos))))        
              )) ; end if
         (cond
           ;; Build condition for a user defined rule with argument passing
           ((= (right term 1) ")")
            ;; We must generate code based upon the associated trailing BNF command.
            (cond
               ;; BNF command is none.
               ((= bnfCommand #void)
                (setq tmpResult (append 
                          "(if (<> (setq _tk" (addi termIX 1) " (" (_fixup (append _rulePrefix term)) ")) morphFail)"
                               "true "
                               "(setq _ip _ip" termIX "))"
                          ))
                ) ; end BNF command is none
               ;; BNF command is *.
               ((= bnfCommand "*")
                (if (= userCondition #void)
                    then
                    (setq tmpResult (append 
                          "(begin "
                              "(setq _i 0) " 
                              "(setq _tk" (addi termIX 1) " (new Vector: 0)) " 
                              "(while " 
                                  "(<> (setq _tkthis (" (_fixup (append _rulePrefix term)) ")) morphFail) do "
                                  "(begin "
                                     "(setq _tk" (addi termIX 1) "[_i] _tkthis) "
                                     "(setq _i (iadd _i 1)) " 
                                  ")) " 
                              "true)"
                        ))
                    else
                    (setq tmpResult (append 
                          "(begin "
                              "(setq _i 0) " 
                              "(setq _tk" (addi termIX 1) " (new Vector: 0)) " 
                              "(while " 
                                  "(and "
                                    "(<> (setq _tkthis (" (_fixup (append _rulePrefix term)) ")) morphFail) "
                                     userCondition " ) do "
                                  "(begin "
                                     "(setq _tk" (addi termIX 1) "[_i] _tkthis) "
                                     "(setq _i (iadd _i 1)) " 
                                  ")) " 
                              "true)"
                        )))
                ) ; end BNF command is *
               ;; BNF command is +.
               ((= bnfCommand "+")
                (if (= userCondition #void)
                    then
                    (setq tmpResult (append 
                          "(begin "
                              "(setq _i 0) " 
                              "(setq _tk" (addi termIX 1) " (new Vector: 0)) " 
                              "(while " 
                                  "(<> (setq _tkthis (" (_fixup (append _rulePrefix term)) ")) morphFail) do "
                                  "(begin "
                                     "(setq _tk" (addi termIX 1) "[_i] _tkthis) "
                                     "(setq _i (iadd _i 1)) " 
                                  ")) " 
                              "(if (> _i 0) true (setq _ip _ip" termIX ")))"
                        ))
                    else
                    (setq tmpResult (append 
                          "(begin "
                              "(setq _i 0) " 
                              "(setq _tk" (addi termIX 1) " (new Vector: 0)) " 
                              "(while " 
                                  "(and "
                                    "(<> (setq _tkthis (" (_fixup (append _rulePrefix term)) ")) morphFail) "
                                     userCondition " ) do "
                                  "(begin "
                                     "(setq _tk" (addi termIX 1) "[_i] _tkthis) "
                                     "(setq _i (iadd _i 1)) " 
                                  ")) " 
                              "(if (> _i 0) true (setq _ip _ip" termIX ")))"
                        )))                      
                ) ; end BNF command is +
               ;; BNF command is ?.
               ((= bnfCommand "?")
                (if (= userCondition #void)
                    then
                    (setq tmpResult (append 
                          "(if (<> (setq _tk" (addi termIX 1) " (" (_fixup (append _rulePrefix term)) ")) morphFail) "
                               "true "
                               "(begin "
                                  "(setq _tk" (addi termIX 1) " #void) "
                                  "true))"
                          ))
                    else
                    (setq tmpResult (append 
                          "(if "
                             "(and "
                               "(<> (setq _tk" (addi termIX 1) " (" (_fixup (append _rulePrefix term)) ")) morphFail) "
                                userCondition " ) "
                               "true "
                               "(begin "
                                  "(setq _tk" (addi termIX 1) " #void) "
                                  "true))"
                          )))
                ) ; end BNF command is ?
               ) ; end BNF cond            
            ) ; end Build condition for a user defined rule with argument passing

           ;; Build condition for a user defined rule
           ((and (isSymbol term) (isCharUppercase term))
            ;; We must generate code based upon the associated trailing BNF command.
            (cond
               ;; BNF command is none.
               ((= bnfCommand #void)
                (setq tmpResult (append 
                          "(if (<> (setq _tk" (addi termIX 1) " (" (append _rulePrefix term) ")) morphFail)"
                               "true "
                               "(setq _ip _ip" termIX "))"
                          ))
                ) ; end BNF command is none
               ;; BNF command is *.
               ((= bnfCommand "*")
                (if (= userCondition #void)
                    then
                    (setq tmpResult (append 
                          "(begin "
                              "(setq _i 0) " 
                              "(setq _tk" (addi termIX 1) " (new Vector: 0)) " 
                              "(while " 
                                  "(<> (setq _tkthis (" (append _rulePrefix term) ")) morphFail) do "
                                  "(begin "
                                     "(setq _tk" (addi termIX 1) "[_i] _tkthis) "
                                     "(setq _i (iadd _i 1)) " 
                                  ")) " 
                              "true)"
                        ))
                    else
                    (setq tmpResult (append 
                          "(begin "
                              "(setq _i 0) " 
                              "(setq _tk" (addi termIX 1) " (new Vector: 0)) " 
                              "(while " 
                                  "(and "
                                    "(<> (setq _tkthis (" (append _rulePrefix term) ")) morphFail) "
                                     userCondition " ) do "
                                  "(begin "
                                     "(setq _tk" (addi termIX 1) "[_i] _tkthis) "
                                     "(setq _i (iadd _i 1)) " 
                                  ")) " 
                              "true)"
                        )))
                ) ; end BNF command is *
               ;; BNF command is +.
               ((= bnfCommand "+")
                (if (= userCondition #void)
                    then
                    (setq tmpResult (append 
                          "(begin "
                              "(setq _i 0) " 
                              "(setq _tk" (addi termIX 1) " (new Vector: 0)) " 
                              "(while " 
                                  "(<> (setq _tkthis (" (append _rulePrefix term) ")) morphFail) do "
                                  "(begin "
                                     "(setq _tk" (addi termIX 1) "[_i] _tkthis) "
                                     "(setq _i (iadd _i 1)) " 
                                  ")) " 
                              "(if (> _i 0) true (setq _ip _ip" termIX ")))"
                        ))
                    else
                    (setq tmpResult (append 
                          "(begin "
                              "(setq _i 0) " 
                              "(setq _tk" (addi termIX 1) " (new Vector: 0)) " 
                              "(while " 
                                  "(and "
                                    "(<> (setq _tkthis (" (append _rulePrefix term) ")) morphFail) "
                                     userCondition " ) do "
                                  "(begin "
                                     "(setq _tk" (addi termIX 1) "[_i] _tkthis) "
                                     "(setq _i (iadd _i 1)) " 
                                  ")) " 
                              "(if (> _i 0) true (setq _ip _ip" termIX ")))"
                        )))                      
                ) ; end BNF command is +
               ;; BNF command is ?.
               ((= bnfCommand "?")
                (if (= userCondition #void)
                    then
                    (setq tmpResult (append 
                          "(if (<> (setq _tk" (addi termIX 1) " (" (append _rulePrefix term) ")) morphFail) "
                               "true "
                               "(begin "
                                  "(setq _tk" (addi termIX 1) " #void) "
                                  "true))"
                          ))
                    else
                    (setq tmpResult (append 
                          "(if "
                             "(and "
                               "(<> (setq _tk" (addi termIX 1) " (" (append _rulePrefix term) ")) morphFail) "
                                userCondition " ) "
                               "true "
                               "(begin "
                                  "(setq _tk" (addi termIX 1) " #void) "
                                  "true))"
                          )))
                ) ; end BNF command is ?
               ) ; end BNF cond            
            ) ; end Build condition for a user defined rule

           ;; Build condition for an Eof test
           ((and (isSymbol term) (= term Eof:))
            (setq tmpResult (append "(if (_eofToken)"
                                                     "true (setq _ip _ip" termIX "))")))

           ;; Build condition for a Nop test
           ((and (isSymbol term) (= term Nop:))
            (setq tmpResult (append "(begin (setq _tk" (addi termIX 1) " #void) true)")))


           ;; Build condition for an Any test
           ((and (isSymbol term) (= term Any:))
            ;; We must generate code based upon the associated trailing BNF command.
            (cond
               ;; BNF command is none.
               ((= bnfCommand #void)
                (setq tmpResult (append 
                          "(if (<> (setq _tk" (addi termIX 1) " (_getToken)) morphFail) "
                               "true "
                               "(setq _ip _ip" termIX "))"
                          ))
                ) ; end BNF command is none
               ;; BNF command is *.
               ((= bnfCommand "*")
                (if (= userCondition #void)
                    then
                    (setq tmpResult (append 
                          "(begin "
                              "(setq _i 0) " 
                              "(setq _tk" (addi termIX 1) " (new Vector: 0)) " 
                              "(while " 
                                  "(<> (setq _tkthis (_getToken)) morphFail) do "
                                  "(begin "
                                     "(setq _tk" (addi termIX 1) "[_i] _tkthis) "
                                     "(setq _i (iadd _i 1)) " 
                                  ")) " 
                              "(setq _ip (isub _ip 1)) "
                              "true)"
                        ))
                    else
                    (setq tmpResult (append 
                          "(begin "
                              "(setq _i 0) " 
                              "(setq _tk" (addi termIX 1) " (new Vector: 0)) " 
                              "(while " 
                                  "(and "
                                    "(<> (setq _tkthis (_getToken)) morphFail) "
                                     userCondition ") do "
                                  "(begin "
                                     "(setq _tk" (addi termIX 1) "[_i] _tkthis) "
                                     "(setq _i (iadd _i 1)) " 
                                  ")) " 
                              "(setq _ip (isub _ip 1)) "
                              "true)"
                        )))
                ) ; end BNF command is *
               ;; BNF command is +.
               ((= bnfCommand "+")
                (if (= userCondition #void)
                    then
                    (setq tmpResult (append 
                          "(begin "
                              "(setq _i 0) " 
                              "(setq _tk" (addi termIX 1) " (new Vector: 0)) " 
                              "(while " 
                                  "(<> (setq _tkthis (_getToken)) morphFail) do "
                                  "(begin "
                                     "(setq _tk" (addi termIX 1) "[_i] _tkthis) "
                                     "(setq _i (iadd _i 1)) " 
                                  ")) " 
                              "(setq _ip (isub _ip 1)) "
                              "(if (> _i 0) true (setq _ip _ip" termIX ")))"
                        ))
                    else
                    (setq tmpResult (append 
                          "(begin "
                              "(setq _i 0) " 
                              "(setq _tk" (addi termIX 1) " (new Vector: 0)) " 
                              "(while " 
                                  "(and "
                                    "(<> (setq _tkthis (_getToken)) morphFail) "
                                     userCondition ") do "
                                  "(begin "
                                     "(setq _tk" (addi termIX 1) "[_i] _tkthis) "
                                     "(setq _i (iadd _i 1)) " 
                                  ")) " 
                              "(setq _ip (isub _ip 1)) "
                              "(if (> _i 0) true (setq _ip _ip" termIX ")))"
                        )))
                ) ; end BNF command is +
               ;; BNF command is ?.
               ((= bnfCommand "?")
                (if (= userCondition #void)
                    then
                    (setq tmpResult (append 
                          "(if (<> (setq _tk" (addi termIX 1) " (_getToken)) morphFail) "
                               "true "
                               "(begin "
                                  "(setq _ip (isub _ip 1)) "
                                  "(setq _tk" (addi termIX 1) " #void) "
                                  "true))"
                          ))
                    else
                    (setq tmpResult (append 
                          "(if "
                            "(and "
                              "(<> (setq _tk" (addi termIX 1) " (_getToken)) morphFail) "
                               userCondition ") "
                               "true "
                               "(begin "
                                  "(setq _ip (isub _ip 1)) "
                                  "(setq _tk" (addi termIX 1) " #void) "
                                  "true))"
                          )))
                ) ; end BNF command is ?
               ) ; end BNF cond            
            ) ; end Build condition for an Any or Self test

           ;; Build condition for an feature test
           ((and (isSymbol term) (isCharAlphanumeric term))
            ;; We must generate code based upon the associated trailing BNF command.
            (cond
               ;; BNF command is none.
               ((= bnfCommand #void)
                (setq tmpResult (append 
                          "(if (<> (setq _tk" (addi termIX 1) " (_getToken))[" term ":] #void) "
                               "true "
                               "(setq _ip _ip" termIX "))"
                          ))
                ) ; end BNF command is none
               ;; BNF command is *.
               ((= bnfCommand "*")
                (if (= userCondition #void)
                    then
                    (setq tmpResult (append 
                          "(begin "
                              "(setq _i 0) " 
                              "(setq _tk" (addi termIX 1) " (new Vector: 0)) " 
                              "(while " 
                                  "(<> (setq _tkthis (_getToken))[" term ":] #void) do "
                                  "(begin "
                                     "(setq _tk" (addi termIX 1) "[_i] _tkthis) "
                                     "(setq _i (iadd _i 1)) " 
                                  ")) " 
                              "(setq _ip (isub _ip 1)) "
                              "true)"
                        ))
                    else
                    (setq tmpResult (append 
                          "(begin "
                              "(setq _i 0) " 
                              "(setq _tk" (addi termIX 1) " (new Vector: 0)) " 
                              "(while " 
                                  "(and "
                                    "(<> (setq _tkthis (_getToken))[" term ":] #void) "
                                     userCondition " ) do "
                                  "(begin "
                                     "(setq _tk" (addi termIX 1) "[_i] _tkthis) "
                                     "(setq _i (iadd _i 1)) " 
                                  ")) " 
                              "(setq _ip (isub _ip 1)) "
                              "true)"
                        )))
                ) ; end BNF command is *
               ;; BNF command is +.
               ((= bnfCommand "+")
                (if (= userCondition #void)
                    then
                    (setq tmpResult (append 
                          "(begin "
                              "(setq _i 0) " 
                              "(setq _tk" (addi termIX 1) " (new Vector: 0)) " 
                              "(while " 
                                  "(<> (setq _tkthis (_getToken))[" term ":] #void) do "
                                  "(begin "
                                     "(setq _tk" (addi termIX 1) "[_i] _tkthis) "
                                     "(setq _i (iadd _i 1)) " 
                                  ")) " 
                              "(setq _ip (isub _ip 1)) "
                              "(if (> _i 0) true (setq _ip _ip" termIX ")))"
                        ))
                    else
                    (setq tmpResult (append 
                          "(begin "
                              "(setq _i 0) " 
                              "(setq _tk" (addi termIX 1) " (new Vector: 0)) " 
                              "(while " 
                                  "(and "
                                    "(<> (setq _tkthis (_getToken))[" term ":] #void) "
                                     userCondition " ) do "
                                  "(begin "
                                     "(setq _tk" (addi termIX 1) "[_i] _tkthis) "
                                     "(setq _i (iadd _i 1)) " 
                                  ")) " 
                              "(setq _ip (isub _ip 1)) "
                              "(if (> _i 0) true (setq _ip _ip" termIX ")))"
                        )))
                ) ; end BNF command is +
               ;; BNF command is ?.
               ((= bnfCommand "?")
                (if (= userCondition #void)
                    then
                    (setq tmpResult (append 
                          "(if (<> (setq _tk" (addi termIX 1) " (_getToken))[" term ":] #void) "
                               "true "
                               "(begin "
                                  "(setq _ip (isub _ip 1)) "
                                  "(setq _tk" (addi termIX 1) " #void) "
                                  "true))"
                          ))
                    else
                    (setq tmpResult (append 
                          "(if "
                            "(and "
                              "(<> (setq _tk" (addi termIX 1) " (_getToken))[" term ":] #void) "
                               userCondition " ) "
                               "true "
                               "(begin "
                                  "(setq _ip (isub _ip 1)) "
                                  "(setq _tk" (addi termIX 1) " #void) "
                                  "true))"
                          )))
                ) ; end BNF command is ?
               ) ; end BNF cond            
            ) ; end Build condition for a feature test

           ;; Build condition for a symbol constant test
           ((isSymbol term)
            (setq tmpResult (append "(if (= (setq _tk" (addi termIX 1) 
                                                     " (_getToken))[Value:] |" term "|:) true (setq _ip _ip" termIX "))")))

           ;; Build condition for a constant test
           (else
            (setq tmpResult (append "(if (= (setq _tk" (addi termIX 1) 
                                                     " (_getToken))[Value:] " term ") true (setq _ip _ip" termIX "))")))

           ) ; end cond
       ;; Add the user condition to the term (if any)
       (if (or (= userCondition #void) (<> bnfCommand #void))
           (setq result (append result tmpResult))
           else
           (setq result (append "(and " tmpResult " " userCondition ")"))
           ) ; end if
       ;; Is this the condition for the first level of this rule?
       (cond
           ;; First test of a new top level rule group 
           ((and (= termIX 0) (> ruleIndex 0))
            (setq result (append "(begin (setq _ip _ip0)" _eol bk "   " result ")")))
           ;; Second+ test in a mid level rule group 
           ((and (> termIX 0) (> ruleIndex 0))
            (setq result (append "(begin (setq _ip _ip" termIX ")" _eol bk "   " result ")")))
           ) ; end cond
       result) ; end _condition
   ;; Fixup a Lisp sequence for $N and %N references
   (defun _fixup(lispSource)
       vars:(result)
       (setq result lispSource)
       (cond
          ;; Is this a condition for this rule?
          ((= (left result 2) "||")
           (setq result (mid result 2 (subi (length result) 4))))
          ;; Is this an action for this rule?
          ((or (= (left result 2) "::") (= (left result 2) "<<") (= (left result 2) "&&"))
           (setq result (mid result 2 (subi (length result) 4))))
          ;; Is this an argument list for this rule?
          ((= (right result 1) ")")
           (setq result (substitute (left result (subi (length result) 1)) "(" " " 1)))
          ;; Is this an user condition for this rule?
          ((= (right result 1) "}")
           (setq result (mid result 1 (subi (length result) 2))))
           ) ; end cond
       ;; Fix up all references to $N variables 
       (setq result (substitute result "$" "_tk"))
       ;; Fix up all references to %N variables 
       (setq result (substitute result "%" "_ak")) 
       result) ; end _fixup
   ;; Ouput condition graph for the rules
   (defun _generateSource(d bk termIX)
       vars:(i)
       ;; Initialize the condition clause for this level of the rule
       (if (> termIX 0)
           (setq _result (append _result bk "(begin (++ _ruleCount) (if (and (> _verboseTrigger 0) (> _ruleCount _verboseTrigger) (not _verbose)) (_startLog)) (setq _ip" termIX " _ip) " _eol)))
       ;; Is this a new condition for this level of the rule?
       (loop for i from 0 until (length d) do
           ;; Is this a rule firing, or just another case test?
           (if (= d[i 0] "_default")
               (begin
                  (setq _result (append _result bk "  ;; *********************************************************" _eol))
                  (setq _result (append _result bk "  ;; RULE: " d[i 1][2] _eol))
                  (setq _result (append _result bk "  ;; *********************************************************" _eol))
                  ) ; end then begin
               (begin
                  (setq _result (append _result bk "  ;; ====================" _eol))
                  (setq _result (append _result bk "  ;; case: " d[i 0] _eol))
                  (setq _result (append _result bk "  ;; ====================" _eol))
                  ) ; end else begin
               ) ; end if
           (setq _result (append _result bk "  (if " (_condition i d[i 0] d[i 1] termIX bk) _eol))
           (if (_generateTest d[i 1])
               (_generateSource d[i 1] (append bk "    ") (addi termIX 1))
               (setq _result (append _result bk "  " (_action i d[i 1][1] d[i 1][2] bk termIX) _eol))
               ) ; end if
           (setq _result (append _result bk "  ) ; end case : " d[i 0] _eol))
           ) ; end loop
       (if (> termIX 0)
           (setq _result (append _result bk ") ; end begin" _eol)))
       ) ; end _generateSource
   ;; ********************************************************
   ;; Begin the _outputOneSyntaxRule function
   ;; ********************************************************
   ;; Determine the rule ordering (the default is user ordering).
   (setq _userOrderingSW false)
   (setq _generateTest isDirectory)
   (if (= v[0][0] #(user ordering))
       (begin
          (setq _userOrderingSW true)
          (setq _generateTest isLambda)
          (delete v 0)
          )) ; end if
   ;; Compute the maximum number of terms in the largest rule
   (setq _name name)
   (if _userOrderingSW
      (setq d (new rulesDirectory))
      (setq d (new Directory:))
      ) ; end if
   (setq _result "")
   (loop for i from 0 until (length v) do
       (_buildRuleTree d 0 v[i])
       ) ; end loop
   (_generateSource d "   " 0)
   _result) ; end _syntax_outputARule




;;**EXPORTKEY**:ParseLib:_syntax_outputRules
(defchild ParseLib:_syntax_outputRules()
;; *******************************************************************
;; summary:  Output the Syntax Rules as child Lambdas. Each rule is
;;           output as a separate child Lambda. The header and trailer
;;           for each child Lambda are templates. The rule clauses form
;;           the body of the chgild Lambda and are output by the
;;           _syntax_outputARule function.
;; Parms:    none       
;; return:   _result    Always true.
;; *******************************************************************
   vars:(ruleIndex itemIndex result temp ruleName ruleNameExt ruleItems)
   ;; Set the rule name prefix to Syntax Rules.
   (setq _rulePrefix "_SYNRULE_")
   ;; Compose the main header to the user defined rules section
   ;; Output each group of like named rules
   (loop for ruleIndex from 0 until (length _syntaxRuleDictionary) do
       (setq ruleName _syntaxRuleDictionary[ruleIndex 0])
       (setq ruleNameExt (append _rulePrefix _syntaxRuleDictionary[ruleIndex 0]))
       (setq ruleItems _syntaxRuleDictionary[ruleIndex 1])
       ;; Compose the rule header for this user defined rule
       (setq temp (mid (browseLib.checkout "ParseLib:%SYNTAX_RULEHEADER") (addi 7 (length _eol)) 100000))
       (if (= _friendCompiler true)
           (setq temp (substitute temp "#??#" (append _nameDef " ")))
           (setq temp (substitute temp "#??#" (append _nameDef ":")))
           ) ; end if
       (setq result (substitute (substitute temp "#!!#" ruleNameExt) "#%%#" ruleName))
       (setq result (append result (_syntax_outputARule ruleName ruleItems)))
       ;; Compose the rule trailer for the previous user defined rule
       (setq temp (mid (browseLib.checkout "ParseLib:%SYNTAX_RULETRAILER") (addi 7 (length _eol)) 100000))
       (if (= _friendCompiler true)
           (setq temp (substitute temp "#??#" (append _nameDef " ")))
           (setq temp (substitute temp "#??#" (append _nameDef ":")))
           ) ; end if
       (setq result (append result (substitute (substitute temp "#!!#" ruleNameExt) "#%%#" ruleName)))
       ;; Compose and checkin this user defined rule for the compiler
       (browseLib.checkin _cabinetName (symbol (append _name ":" ruleNameExt)) result)
       ) ; end _syntaxRuleDictionary loop
   true) ;; end _syntax_outputRules





;;**EXPORTKEY**:ParseLib:_syntax_saveARule
(defchild ParseLib:_syntax_saveARule(ruleName ruleItems condition action)
;; *******************************************************************
;; summary:  Save a single Syntax Rule in a rule dictionary. Each rule is
;;           output as a separate child Lambda. The header and trailer
;;           for each child Lambda are templates. The rule clauses form
;;           the body of the chgild Lambda and are output by the
;;           _syntax_outputARule function.
;; Parms:    ruleName   The name of the syntax rule (always uppercase)
;;           ruleItems  Each iemized condition for this rule (ie Name + Name)
;;           condition  Any trailing rule condition (ie || (= $1.Value "if") ||)
;;           action     Any trailing rule action (ie :: (setq $0.Value $1.Value) ::)      
;; return:   _result    A vector of rule specifications.
;; *******************************************************************
   vars:(i itemLen temp term ruleID repeatSW result)
   ;; Compose the header to this user defined rule
   (setq ruleID (append ruleName ":"))
   (setq itemLen (length ruleItems))
   (loop for i from 0 until itemLen do
       (if (<> ruleItems[i] #void)
           (setq ruleID (append ruleID " " ruleItems[i])))
       ) ; end loop
   (if (<> condition #void)
       (setq ruleID (append ruleID " " condition)))
   (if (<> action #void)
       (setq ruleID (append ruleID " " action)))
   ;; Compose the condition for this user defined rule
   (setq result (new Vector: 4 ruleItems condition action ruleID))
   result) ;; end _syntax_saveARule






















;;**EXPORTKEY**:ParseLib:_syntax_saveRules
(defchild ParseLib:_syntax_saveRules(name words condition action)
;; *******************************************************************
;; summary:  Save all Syntax Rule in the rule dictionary. Each rule is
;;           output as a separate child Lambda. The header and trailer
;;           for each child Lambda are templates. The rule clauses form
;;           the body of the child Lambda.
;; Parms:    name       The name of the syntax rule (always uppercase)
;;           words      Each iemized condition for this rule (ie Name + Name)
;;           condition  Any trailing rule condition (ie || (= $1.Value "if") ||)
;;           action     Any trailing rule action (ie :: (setq $0.Value $1.Value) ::)      
;; return:   _result    A vector of rule specifications.
;; *******************************************************************
   vars:(i wordLen rules)
   (setq rules _syntaxRuleDictionary[name])
   (if (= rules #void) (setq rules (new Vector: 0)))
   ;; Add the new rule primatives to the rules vector
   (setq rules[(length rules)] (_syntax_saveARule name words condition action))
   (setq _syntaxRuleDictionary[name] rules)
   ;; Check for empty condition errors in the new rule.
   (if (= words[0] #void)
       (error (append "Empty condition Error in " name ": ...use Nop special rule syntax for unconditional rules")))
   true) ; end _syntax_saveRules






















;;**EXPORTKEY**:ParseLib:defaultLexer
(defriend ParseLib:defaultLexer(inString)
;; ********************************************************************
;; summary:  This Lambda converts an input string into a vector of
;;           recognized lexemes. It is the default lexical analyzer
;;           for the ParseLib compiler generator.
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
           tokenList           ;; The vector of lexical tokens
           TP                  ;; The output pointer for the token output vector
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
       ;;  Initialize the ParseLib once and only once.
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
        (setq operatorList #(#\= #\< #\> #\! #\^ #\~ #\+ #\/ #\* #\- #\| #\&))
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
        (setq tokenList[TP] (symbol (string ch2)))
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
        ;; Recognize trailing BNF commands
        (cond 
           ((= CH #\*) (++ IP))
           ((= CH #\?) (++ IP))
           ((= CH #\+) (++ IP))
           ) ; end cond
        (setq tokenList[TP] (symbol (substring INSTRING oldIP (subi IP 1))))
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
        (setq tokenList[TP] num)
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
        (setq tokenList[TP] (symbol (substring INSTRING oldIP (subi IP 1))))
        (++ TP)
        ) ;; end _recOperators
    ;; Recognize all special symbols.
    (defun _recSpecial() (setq tokenList[TP] (symbol (string CH))) (++ IP) (++ TP))
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
    ;;  Initialize the ParseLib once and only once.
    (if (= KB #void) (_Initialize))
    ;;  Initialize the output token vector. 
    (setq tokenList (new Vector: 0))
    (setq TP 0)
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






































;;**EXPORTKEY**:ParseLib:precompiler
(deforphan ParseLib:precompiler(inSource)  
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
    pvars:(;; Public Child Lambdas
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
    ;; Define Public Child Lambdas
    ;;****************************************************************
    ;; Manage "#define" pre-compiler directives 
    (defun defineMgr() 
       (if (bcompareEQ passThruSW true)
           (eval (compile (morph (lisp (append "(setq " (mid thisLine 8 100000000) ")")))))
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
       (if (bcompareEQ (eval (compile (morph (lisp (mid thisLine 4 100000000))))) true) 
           (setq  passThruSW true)
           (setq  passThruSW false)
           ) ; end if
       (setq conditionSW true) 
       (setq conditionScan |else|:) 
       true) ; end ifMgr
    ;; Manage "#ifdef" pre-compiler directives 
    (defun ifdefMgr()
       (if (bcompareEQ conditionSW true) (error "precompiler: nested #ifdef directives NOT supported")) 
       (if (<> (eval (compile (morph (lisp (mid thisLine 7 100000000))))) #void) 
           (setq  passThruSW true)
           (setq  passThruSW false)
           ) ; end if
       (setq conditionSW true) 
       (setq conditionScan |else|:) 
       true) ; end ifdefMgr
    ;; Manage "#ifndef" pre-compiler directives 
    (defun ifndefMgr()
       (if (bcompareEQ conditionSW true) (error "precompiler: nested #ifdef directives NOT supported")) 
       (if (= (eval (compile (morph (lisp (mid thisLine 8 100000000))))) #void) 
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





;;**EXPORTKEY**:ParseLib:rulesDirectory
(defriend ParseLib:rulesDirectory()
;; ********************************************************************
;; summary:  Create, maintain, and apply a Directory of IF -> THEN
;;           production rules for the ParseLib. This Lambda is
;;           designed to behave similar to a Directory, but without
;;           the inherent sorting.
;; Parms:    none  
;; return:   true
;; ********************************************************************
   pvars:(leftRULES                ;; Vector of left hand production rules
          rightRULES               ;; Vector of right hand production rules
          ;; Methods list 
          len                      ;; Method to return the number of rules in the rules Directory 
          new                      ;; Method to create a new rules Directory 
          ref1                     ;; Method to return the THEN form corresponding to an IF form 
          ref2                     ;; Method to return the rules contained in the rules Directory 
          refCount                 ;; Method to return the number of production rules 
          set1                     ;; Method to add a single new rule to the rules Directory 
         ) ;; end of persistent variables
   ;; Never initialize this Lambda more than once, because the
   ;; inline child Lambdas will overlay the cloned child Lambdas
   ;; in any clone copies of the Lambda and this causes serious
   ;; confusion when the reinitialized clone begins to affect
   ;; the persistant storage of the original Lambda.
   (if (<> new #void) (return true))
   ;; Initialize the inline child Lambdas.
   (defun len() (length leftRULES))
   (defun new()
      (setq leftRULES (^new Vector: 0))
      (setq rightRULES (^new Vector: 0))
      true) ;; end of new
   (defun ref1(key)
      vars:(i)
      ;; We retrieving right hand production rules.
      (if (isNumber (setq i (member key leftRULES)))
          (return rightRULES[i]))
      #void) ;; end of ref1
   (defun ref2(index1 index2)
      vars:(n)
      (setq n (length leftRULES))
      ;; Are we retrieving an out of range rule?
      (if (or (< index1 0) (>= index1 n))
          (return #void))
      ;; Are we retrieving left hand production rules.
      (if (= index2 0)
          (return leftRULES[index1]))
      ;; Are we retrieving right hand production rules.
      (if (= index2 1)
          (return rightRULES[index1]))
      (error "arglist")) ;; end of ref2
   (defun refCount() (length leftRULES))
   (defun set1(key newValue)
      vars:(i n)
      ;; We are asserting a production rule.
      (setq n (length leftRULES))
      (setq i (member key leftRULES))
      (if (isNumber i)
          (if (= newValue #void)
              ;; We are deleting an existing production rule.
              (begin
                 (delete leftRULES i)
                 (delete rightRULES i)
                 (return #void)) ;; end delete production rule
              ;; We are updating an existing production rule.
              (begin
                 (setq rightRULES[i] newValue)
                 (return newValue))) ;; end update production rule
          ) ;; end if
      ;; Do not assert new rules with #void right hand productions.
      (if (= newValue #void) (error "rightHand" (append "rulesDirectory found right hand #void: " key " ==> " newValue)))
      ;; Assert new production rule
      (setq leftRULES[n] key)
      (setq rightRULES[n] newValue)
      newValue) ;; end of set1
   ;; Initialize the Lambda and clear the rules Directory.
   (new)) ;; end of rulesDirectory














































