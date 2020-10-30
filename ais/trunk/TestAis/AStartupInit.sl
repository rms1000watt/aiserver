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
;;	ais/testais/astartupInit.sl  

;;	CHANGE HISTORY
;;	Revision	Date	 Who	Change
;;	1.0052		1/14/05  tlw	Import and compile libraries

;;	NOTE:
;; 1. To set the AIS executable that executes when a .sl is selected: 
;; 	In Microsoft's explorer, select Tools->Folder Options->File Types
;;	Scroll down to SL Files, select Advanced->open->Edit->Browse
;;	Navigate to the webide.exe in the aisdev dir	  
;; ****************************************************************************
;; Application .ini file overrides.
;; ****************************************************************************
;#memory=100
;#StartupPrefix=(debug jitoff:)

;; ****************************************************************************
;; Load browse Lambda from a central location
;; ****************************************************************************
(setq _libPath (append _ais.installPath "Libraries/"))
(runScript (append _libPath "BrowseLib/BrowseLib.sl"))

;; ****************************************************************************
;; Load the application cabinets locally. The rest from installPath/libraries
;; ****************************************************************************
vars: (d)

;(setq d (new browseLib.dir "Libraries"))
;(d.remove "DataMineLib.db")
;(browseLib dataMineLib:		(append _libPath "DataMineLib.db"))
;(browseLib.importSource (append _libPath "DataMineLib/DataMineLib.sl"))
;(browseLib.setFocus DataMineLib:)
;(browseLib.compileAll true)
;
;(d.remove "Index.db")
;(browseLib index:				(append _libPath "Index.db"))
;(browseLib.importSource (append _libPath "Index/Index.sl"))
;(browseLib.setFocus index:)
;(browseLib.compileAll true)
;
;(d.remove "JavaScript.db")
;(browseLib javaScript:		(append _libPath "JavaScript.db"))
;(browseLib.importSource (append _libPath "JavaScript/JavaScript.sl"))
;(browseLib.setFocus javaScript:)
;(browseLib.compileAll true)
;
;(d.remove "Math.db")
;(browseLib math:		(append _libPath "Math.db"))
;(browseLib.importSource (append _libPath "Math/Math.sl"))
;(browseLib.setFocus math:)
;(browseLib.compileAll true)
;
;(d.remove "ParseLib.db")
;(browseLib ParseLib:		(append _libPath "ParseLib.db"))
;(browseLib.importSource (append _libPath "ParseLib/ParseLib.sl"))
;(browseLib.setFocus ParseLib:)
;(browseLib.compileAll true)
;
;(d.remove "RulesLib.db")
;(browseLib rulesLib:		(append _libPath "RulesLib.db"))
;(browseLib.setFocus rulesLib:)
;(browseLib.compileAll true)
;(browseLib.importSource (append _libPath "RulesLib/RulesLib.sl"))
;
;(d.remove "Svm.db")
;(browseLib svm:		(append _libPath "Svm.db"))
;(browseLib.setFocus svm:)
;(browseLib.compileAll true)
;(browseLib.importSource (append _libPath "Svm/Svm.sl"))
;
;(setq d (new browseLib.dir "Cabinets"))
;(d.remove "TestAis.db")
;(browseLib TestAis:					"Cabinets/TestAis.db")
;(browseLib.importSource "Cabinets/TestAis.sl")
;(browseLib.setFocus testAis:)
;(browseLib.compileAll true)

(setq d (new browseLib.dir "Cabinets"))
(d.remove "TestAis.db")
(browseLib testAis:			"Cabinets/Testais.db")
(browseLib.importSource "cabinets/testais.sl")
(browseLib.setFocus testAis:)
(browseLib.compileAll true)

;; ****************************************************************************
;; Connect to the database
;; Attach the dataMineLib to the data repositories.
;; ****************************************************************************
(define _dataMineExtents #{
		testRepository:   	"Datamine/TestRepository.db"
	})

;; ****************************************************************************
;; Redefine reset function to reset repositories.
;; ****************************************************************************
(define _OldReset reset)
(defun reset()
   (dataMineLib.clearMemoPad)
    (_OldReset))

;; ****************************************************************************
;; Initialize application-specific Lambdas
;; ****************************************************************************
;(testLambda)

;; ****************************************************************************
;; Open other contexts for this instance of AIS
;; ****************************************************************************
;;;(openContext "Test2/AStartup2.sl")

;; ****************************************************************************
;; Debug startup script.  Use StartupScript above to turn the jit off
;; Then, (debug traceon:) below takes effect immediately.
;; ****************************************************************************
;(debug traceon:)

;; ****************************************************************************
;; Lock all globals and display the welcome message.
;; ****************************************************************************
(lock _globals)
(writeln "Welcome to the TestAis context")
(writeln "libPath=" _libPath)
;; end
