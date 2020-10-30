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
;;  Title:    GSMDemo.sl
;;  Author:   Tim May
;;  Project:  Demos Project
;;	Loads the libraries and defines the GSMDemo lambda

;; *******************************************************************
;; Load browseLib
;; *******************************************************************
(debug compileon:) ;;generate debug information
(debug erroron:) ;;stop on error in debugger

;; For Linux (From Packages)
;; (runScript "/usr/share/ais/Libraries/BrowseLib/BrowseLib.sl")

;; For Windows, and Linux (From Source)
(runScript (append _ais.installPath "Libraries/BrowseLib/BrowseLib.sl"))
(writeln "BrowseLib installed.")

;; Add libs information to _ais structure
(setq _ais.libs #(
	;; cabinet name, database name, cabinet source
;;	#(DataMineLib:,	"Libraries/DataMineLib.db"	,"Libraries/DataMineLib/DataMineLib.sl")
;;	#(Index:,		"Libraries/Index.db"		,"Libraries/Index/Index.sl")
;;	#(ParseLib:,	"Libraries/ParseLib.db"		,"Libraries/ParseLib/ParseLib.sl")
	#(JavaScript:,	"Libraries/JavaScript.db"	,"Libraries/JavaScript/JavaScript.sl")
	#(RulesLib:,	"Libraries/RulesLib.db"		,"Libraries/RulesLib/RulesLib.sl")
	#(Math:,		"Libraries/Math.db"			,"Libraries/Math/Math.sl")
;;	#(Xml:,			"Libraries/Xml.db"			,"Libraries/Xml/Xml.sl")
;;	#(Svm:,			"Libraries/Svm.db"			,"Libraries/Svm/Svm.sl")
;;	#(Esm:,			"Libraries/Esm.db"			,"Libraries/Esm/Esm.sl")
	#(Gsm:,			"Libraries/Gsm.db"			,"Libraries/Gsm/Gsm.sl")
	)
)

(defun loadLibs()
	vars:(i libs)
	(setq libs _ais.libs)
	(loop for i from 0 until (length libs) do
		;;(browseLib aKey aExtentFileName aExtentLocation aExtentStorageScope aImportSync aExportSync aAutoCompile aForceSetting)
		(browseLib libs[i][0] (append _ais.installPath libs[i][1]) (append _ais.installPath libs[i][2]) "file" "auto" "auto" "true" "true")
		(writeln (string _ais.libs[i][0]) " installed.")
	) ; end loop
true) ; end loadLib

(loadLibs) 

;;(browseLib aKey aExtentFileName aExtentLocation aExtentStorageScope aImportSync aExportSync aAutoCompile aForceSetting)
(browseLib GsmDemo: "GsmDemo.db" "GsmDemo.sl" "file" "auto" "auto" "true" "true")
(writeln "GsmDemo installed.")
 
(browseLib.bind)

(lock _globals)
(writeln "Globals are locked.")

(writeln "")
(writeln "")
(writeln "This GSM Demo is run from the console command line. There are two steps you need to perform: ")
(writeln "  1. Data Generation")
(writeln "  2. GSM Run")
(writeln "")
(writeln "1. Data Generation Command:")
(writeln "     (gsmDemo.generateData aTestName aTestCase aRows aColumns aNoise aGenerations aMaxTime aModelName aHaltingScore aSVMKernelID aNumChampions)")
(writeln "     (gsmDemo.generateData aTestName aTestCase aRows aColumns)")
(writeln "")
(writeln "   Parameters:")
(writeln "     aTestName - Name of the test")
(writeln "     aTestCase - Test case")
(writeln "     aRows - No. of rows to be generated for the training and testing data")
(writeln "     aColumns - No. of independent columns for the training and testing data")
(writeln "     aNoise - Amount of noise in percentage")
(writeln "     aGenerations - No. of generations")
(writeln "     aMaxTime - Maximum amount of learning time in hours")
(writeln "     aModelName - Set of GSM learning options")
(writeln "     aHaltingScore - Training run halting score")
(writeln "     aSVMKernelID - KernelID to use for SVM learning")
(writeln "     aNumChampions - Number of champions")
(writeln "")
(writeln "   Valid Test Cases:")
(writeln "     CrossCorrelation Cubic CyclicSeries Elipsoid HiddenModel HyperTangent")
(writeln "     Linear MixedModel RandomModal RandomRoot Ratio SquareRoot Trigonometric")
(writeln "")
(writeln "2. GSM Run Command:")
(writeln "     (gsmDemo aTestName)")
(writeln "")
(writeln "Sample Runs. You can cut and paste individual command lines into the console command line or highlight a line,")
(writeln "right click for the context menu and select execute selection to run the selected command.")
(writeln "")
(writeln "CrossCorrelation Test:")
(writeln "  (gsmDemo.generateData \"CrossCorrelation_01\" \"CrossCorrelation\" 3000 4)")
(writeln "  (gsmDemo \"CrossCorrelation_01\")")
(writeln "")
(writeln "Cubic Test:")
(writeln "  (gsmDemo.generateData \"Cubic_01\" \"Cubic\" 2000 5)")
(writeln "  (gsmDemo \"Cubic_01\")")
(writeln "")
(writeln "CyclicSeries Test:")
(writeln "  (gsmDemo.generateData \"CyclicSeries_01\" \"CyclicSeries\" 2000 3)")
(writeln "  (gsmDemo \"CyclicSeries_01\")")
(writeln "")
(writeln "Elipsoid Test:")
(writeln "  (gsmDemo.generateData \"Elipsoid_01\" \"Elipsoid\" 1000 5 0.5 10 0.5 \"regressGSOALPS\" 0 \"composite\" 30)")
(writeln "  (gsmDemo \"Elipsoid_01\")")
(writeln "")
