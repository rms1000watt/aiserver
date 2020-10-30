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
;;  Title:    RegTestLoad.sl
;;  Author:   Tim May
;;  Project:  Regression Test Suites for AIS
;;	Loads the libraries and defines the regTest and other regression test
;;	lambdas. This script is conditionally run from RegTest.sl as required.
;; 	Do not run this script directly from a test script. Run RegTest.sl instead.
;;	See the readme.txt file in the Tests folder for instructions on how to 
;;	use this script and create and manage tests.

;; *******************************************************************
;; Load browseLib
;; *******************************************************************
;;(debug compileon:) ;;generate debug information
;;(debug erroron:) ;;stop on error in debugger
(runScript (append _ais.installPath "Libraries/BrowseLib/BrowseLib.sl"))
(writeln "browseLib installed")

;; Add libs information to _ais structure
(setq _ais.libs #(
	;; cabinet name, database name, cabinet source
	#(DataMineLib:,	"Libraries/DataMineLib.db"	,"Libraries/DataMineLib/DataMineLib.sl")
	#(Index:,		"Libraries/Index.db"		,"Libraries/Index/Index.sl")
	#(ParseLib:,	"Libraries/ParseLib.db"		,"Libraries/ParseLib/ParseLib.sl")
	#(JavaScript:,	"Libraries/JavaScript.db"	,"Libraries/JavaScript/JavaScript.sl")
	#(RulesLib:,	"Libraries/RulesLib.db"		,"Libraries/RulesLib/RulesLib.sl")
	#(Math:,		"Libraries/Math.db"			,"Libraries/Math/Math.sl")
	#(Xml:,			"Libraries/Xml.db"			,"Libraries/Xml/Xml.sl")
	#(Svm:,			"Libraries/Svm.db"			,"Libraries/Svm/Svm.sl")
	#(Esm:,			"Libraries/Esm.db"			,"Libraries/Esm/Esm.sl")
	#(RunQueue:,    "Libraries/RunQueue.db"     ,"Libraries/RunQueue/RunQueue.sl")
    #(Gsm:,			"Libraries/Gsm.db"			,"Libraries/Gsm/Gsm.sl")
	)
)

(defun loadLibs()
	vars:(i libs)
	(setq libs _ais.libs)
	(loop for i from 0 until (length libs) do
			;;(browseLib aKey aExtentFileName aExtentLocation aExtentStorageScope aImportSync aExportSync aAutoCompile aForceSetting)
		   (browseLib libs[i][0] (append _ais.installPath libs[i][1]) (append _ais.installPath libs[i][2]) "file" "auto" "auto" "true" "true")
	) ; end loop
true) ; end loadLib

(loadLibs) 

;; *******************************************************************
;; regTest lambda
;; The regTest lambda manages the collection and saving of test
;; results for a run of part or all of the regression suite.
;; *******************************************************************
(defun regTest()
	pvars:(
		(CurrentSuite "")
		(CurrentTest "")
		(JitMode "Both")
		TestLog
		StartTime
		EndTime
		TestDir			;browseAgent.Dir object for test folder
		Tests			;vector of test files in test folder
		(TestIndex 0) 	;Current test
		ReportFilePath	;String containing the name of the current report.

		;Methods
		openReportFile	;method to name and open a report file
		)

;;Generate report name using current date and occurance suffix etc.
(defun openReportFile()
	vars:(aTime aName aPrefix (aSuffixCount 1) aDir aNameNotOK aNewName)
	(if (<> CurrentSuite "")
		(setq aName CurrentSuite)
		(if (= (right CurrentTest 3) ".sl")
				(setq aName (left CurrentTest (- (length CurrentTest) 3)))
				(error "regTest.MakeReportNum: Error-- No current Suite or Test specified")))
	(setq aPrefix (append (year (today)) (right (append "0" (month (today))) 2) (right (append "0" (day (today))) 2)))
	
	(setq aDir (new browseLib.dir))
	(if (not (aDir.exists "TestLogs")) 
		(aDir.mkdir "TestLogs"))

	(setq aDir (new browseLib.dir "TestLogs"))

	(setq aNameNotOK true)
	(while aNameNotOK
		(setq aNewName (append aPrefix aName "_" aSuffixCount ".xls"))
		(if (aDir.exists aNewName)
			(++ aSuffixCount)
			(setq aNameNotOK false))
	)
	
	(setq aNewName (append "TestLogs/" aNewName))
	(setq aFileID (fileOpen aNewName 1 1))
	(if (> aFileID 0) (begin
		(setq aFileID (fileClose aFileID 1)) ;close file
		(setq ReportFilePath aNewName)
		(writeln "Created report file " aNewName)
		)
	else (begin
		(error (append "regTest.OpenReportFile -- could not open files " aNewName))
		))
true)

;;Atomic append to report file. Slow but necessary.
(defun writeLineToReportFile(aVal)
	vars:(aFileID)
	(setq aFileID (fileOpen ReportFilePath 0 0))
	(if (> aFileID 0) (begin
		(fileSeek aFileID 0 2);seek to end of file
		(fileWriteln aFileID aVal)
		(setq aFileID (fileClose aFileID 1)) ;close file
		)
	else (begin
		(error (append "regTest.writeLineToReportFile -- could not open files " ReportFilePath))
		))	
true)

;;Atomic append to report file. Slow but necessary.
(defun displayToReportFile(aVal)
	vars:(aFileID)
	(setq aFileID (fileOpen ReportFilePath 0 0))
	(if (> aFileID 0) (begin
		(fileSeek aFileID 0 2);seek to end of file
		(fileDisplay aFileID aVal)
		(setq aFileID (fileClose aFileID 1)) ;close file
		)
	else (begin
		(error (append "regTest.displayToReportFile -- could not open files " ReportFilePath))
		))	
true)

true)


;; runSuite(aSuite [aJitMode] [aIterations])
;; Arguments
;;	aTest		Name of suite. See list of available suites in aRunMatrix[0]
;;	aJitMode 	One of "On", "Off", "Both". Default "Both"
;;	aIterations	Number of times to run. Default 1.
(defun runSuite(aSuite ...)
	vars:(i t T aCurrentSuiteIdx aRunMatrix (aJitMode "Both") (aIterations 1)
			aStartTime aEndTime aTotStartTime aTotEndTime)
	(if (> (argCount) 1) (setq aJitMode (argFetch 1)))
	(if (> (argCount) 2) (setq aIterations (argFetch 2)))

	(debug traceoff:)
	(debug compileoff:)
	(debug erroroff:)

	(setq regTest.CurrentSuite aSuite)
	(setq regTest.JitMode aJitMode)

	(setq aRunMatrix (new Vector: "~" 
	#( "na"						"All" 	"Engine" "IO"	"Libs"	"Timing")
    #( "Test_Complex.sl"		true	true	false	false	false	)
    #( "Test_SpcForms.sl"		true	true	false	false	false	)
    #( "Test_Macros.sl"			true	true	false	false	false	)
    #( "Test_BuiltIn.sl"		true	true	false	false	false	)
    #( "Test_LispProc.sl"		true	true	false	false	false	)
    #( "Test_DateTime.sl"		true	true	false	false	false	)
    #( "Test_MathTest.sl"		true	true	false	false	false	)
    #( "Test_Finance.sl"		true	true	false	true	false	)
    #( "Test_Trig.sl"			true	true	false	false	false	)
    #( "Test_Brick.sl"			true	true	false	false	false	)
    #( "Test_Brick2.sl"			true	true	false	false	false	)
    #( "Test_String.sl"			true	true	false	false	false	)
    #( "Test_Substring.sl"		true	true	false	false	false	)
    #( "Test_MatrixRow.sl"		true	true	false	false	false	)
    #( "Test_NumMatrixRow.sl"	true	true	false	false	false	)
    #( "Test_BadRef.sl"			true	true	false	false	false	)
    #( "Test_VmTest.sl"			true	true	false	true	false	)
    #( "Test_VmJmpcc.sl"		true	true	false	true	false	)
    #( "Test_MySQLEmbedded.sl"	true	false	true	false	false	)
    #( "Test_ListRule.sl"		true	false	false	true	false	)
    #( "Test_Neural.sl"			true	false	false	true	false	)
    #( "Test_LispTime.sl"		true	true	false	false	true	)
    #( "Test_ObjRepo.sl"		true	false	true	true	true	)
    #( "Test_OOFiles.sl"		true	false	true	true	false	)
    #( "Test_TableAgt.sl"		true	false	true	true	false	)
    #( "Test_Import.sl"			true	false	true	true	false	)
    #( "Test_Import2.sl"		true	false	true	true	false	)
    #( "Test_ObjectPersist.sl"	true	false	true	true	true	)
    #( "Test_ObjectRepo.sl"		true	false	true	true	true	)
	#( "Test_WorkSpac.sl"		true	false	true	true	false	)
	))

	;;Determine test suite
	(setq aCurrentSuiteIdx (member regTest.CurrentSuite aRunMatrix[0]))
 	(if (= aCurrentSuiteIdx false) (begin
		(writeln "Error -- could not find " aSuite " in list of valid test suites")
		(exit)))

	(regTest.openReportFile)
	(regTest.writeLineToReportFile (append "Suite" #\tab "Iteration" #\tab "Test" #\tab "Jit" #\tab "Time" #\tab "TotalTime"))
	(setq aTotStartTime (getTickCount 0))

	(loop for i from 0 until aIterations do
		(setq T (length aRunMatrix))
		(loop for t from 1 until T do ;;For each test -- check if we should run it in the curren suite
			(if (= aRunMatrix[t][aCurrentSuiteIdx] true) (begin
				(if (or (= regTest.JitMode "Both") (= regTest.JitMode "Off")) (begin;
					(debug jitoff:)
					(regTest.displayToReportFile (append aSuite #\tab i #\tab aRunMatrix[t][0] #\tab "Off" #\tab))
					(setq aStartTime (getTickCount 0))
					(runScript aRunMatrix[t][0])
					(setq aEndTime (getTickCount aStartTime))
					(setq aTotEndTime (getTickCount aTotStartTime))
					(regTest.writeLineToReportFile (append (/ aEndTime 60) #\tab (/ aTotEndTime 60)))
					))
				(if (or (= regTest.JitMode "Both") (= regTest.JitMode "On")) (begin
					(debug jiton:)
					(regTest.displayToReportFile (append aSuite #\tab i #\tab aRunMatrix[t][0] #\tab "On" #\tab))
					(setq aStartTime (getTickCount 0))
					(runScript aRunMatrix[t][0])
					(setq aEndTime (getTickCount aStartTime))
					(setq aTotEndTime (getTickCount aTotStartTime))
					(regTest.writeLineToReportFile (append (/ aEndTime 60) #\tab (/ aTotEndTime 60)))
					))
			))
		);t
	);i
	(if (> aTotEndTime 0)
		(writeln "Cumulative Run Suite Time:" (/ aTotEndTime 60)))

true)

;; runTest(aTest [aJitMode] [aIterations] )
;; Arguments
;;	aTest		Name of Test_*.sl file
;;	aJitMode 	One of "On", "Off", "Both". Default "Both"
;;	aIterations Number of time to run tests. Default 1
(defun runTest(aTest ...)
	vars:(i (aJitMode "Both") (aIterations 1) aStartTime aEndTime aTotStartTime aTotEndTime)
	
	(if (> (argCount) 1) (setq aJitMode (argFetch 1)))
	(if (> (argCount) 2) (setq aIterations (argFetch 2)))

	(setq regTest.CurrentSuite "")
	(setq regTest.CurrentTest aTest)

	(debug traceoff:)
	(debug compileoff:)
	(debug erroroff:)

	(regTest.openReportFile)
	(regTest.writeLineToReportFile (append "Suite" #\tab "Iteration" #\tab "Test" #\tab "Jit" #\tab "Time" #\tab "TotalTime"))

	(setq aTotStartTime (getTickCount 0))
	(setq regTest.JitMode aJitMode)
	(loop for i from 0 until aIterations do
		(if (or (= regTest.JitMode "Both") (= regTest.JitMode "Off")) (begin;
			(debug jitoff:)
			(regTest.displayToReportFile (append "none" #\tab i #\tab aTest #\tab "Off" #\tab))
			(setq aStartTime (getTickCount 0))
			(runScript aTest)
			(setq aEndTime (getTickCount aStartTime))
			(setq aTotEndTime (getTickCount aTotStartTime))
			(regTest.writeLineToReportFile (append (/ aEndTime 60) #\tab (/ aTotEndTime 60)))
			))
		(if (or (= regTest.JitMode "Both") (= regTest.JitMode "On")) (begin
			(debug jiton:)
			(regTest.displayToReportFile (append "none" #\tab i #\tab aTest #\tab "On" #\tab))
			(setq aStartTime (getTickCount 0))
			(runScript aTest)
			(setq aEndTime (getTickCount aStartTime))
			(setq aTotEndTime (getTickCount aTotStartTime))
			(regTest.writeLineToReportFile (append (/ aEndTime 60) #\tab (/ aTotEndTime 60)))
			))
	)
	(if (> aTotEndTime 0)
		(writeln "Cumulative RunTest Time:" (/ aTotEndTime 60)))
true)


;; *******************************************************************
;; testStart
;; Call at the begining of a regression test.
;; *******************************************************************
(defun testStart(testName)
	(setq regTest.CurrentTest testName)
	(setq regTest.StartTime (getTickCount 0))
	(setq header (append "******* [" testName "] ********"))
	(writeln "[(" regTest.TestIndex "): " (string (date (now))) "] " 
		(left "*************************************************************************************************************************" (length header)))
	(writeln "[(" regTest.TestIndex "): " (string (date (now))) "] " header)
	(writeln "[" retTest.TestIndex "] " 
		(left "*************************************************************************************************************************" (length header)))
	(writeln "[(" regTest.TestIndex "): " (string (date (now))) "] " "======> TEST WITH JIT " (if (debug getjiton:) "ON" "OFF") " <======")

true)

;; *******************************************************************
;; testit
;; call to test individual function points in a regression test
;; *******************************************************************
(defun testit(evalTxt result)
	vars:(
		testResult
		)
	(writeln evalTxt) 
	(if (not (equal (setq testResult (eval evalTxt)) result ))
     	(begin
        (writeln regTest.CurrentTest " *FAILURE* " evalTxt)
		(testError (append regTest.CurrentTest " *FAILURE* " evalTxt))
        ) 
   	else
    	true
   	) ; end if
)

;; *******************************************************************
;; testError
;; Call when an error is encountered in a regression test. Usually called
;; called from testit.
;; *******************************************************************
(defun testError(errorstring)
	(error errorstring)
true)

;; *******************************************************************
;; testEnd
;; Call at the end of a regression test.
;; *******************************************************************
(defun testEnd(testName)
	vars:(aEndTime)
	(writeln "[(" regTest.TestIndex "): " (string (date (now))) "] " "======> POST-TEST MEMORY STATS <======")
	(systemCheck)
	(writeln "[(" regTest.TestIndex "): " (string (date (now))) "] " "======> END OF TEST <======")
	(setq header (append "******* [" testName "] ********"))
	(setq aEndTime (getTickCount regTest.StartTime))
	(writeln "Test Time: " (/ aEndTime 60))
	(setq regTest.CurrentTest #void)
true)

;;_ais.bits contains the number of bits in the architecture of the system
;;This is important as some test routines are sensitive to this aspect of the architecture
(setq _ais.bits (integer (substring (left (right (version) 5) 2))))

(browseLib.bind)
(lock _globals)
(writeln "regTest lambda installed and AIS libraries loaded and globals are locked")
