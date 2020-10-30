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
;;	ais/testais/astartup.sl  
;;								AIS Test Suite


;;	CHANGE HISTORY
;;	Revision	Date		Who		Change
;;	3.0009		6/21/2008	tlw		Change paths and names to uppercase
;;	1.0114		11/15/2006	tlw		Change cabinet name to TestAis
;;	1.0052		1/14/2005	tlw		Import and compile main

;;	NOTE:
;; 1. To set the AIS executable that executes when a .sl is selected: 
;; 	In Microsoft's explorer, select Tools->Folder Options->File Types
;;	Scroll down to SL Files, select Advanced->open->Edit->Browse
;;	Navigate to the webide.exe in the aisdev dir	  
;; ****************************************************************************
;; Application .ini file overrides.
;; ****************************************************************************
;#memory=100
;#memoryObjectHeaders=20
;@$%#StartupPrefix=(debug jitoff:)

;; ****************************************************************************
;; Load browse Lambda from a central location
;; ****************************************************************************
(setq _libPath (append _ais.installPath "Libraries/"))
;; (debug compileon:)
(runScript (append _libPath "BrowseLib/BrowseLib.sl"))

;; ****************************************************************************
;; Load the application cabinets locally. The rest from installPath
;; ****************************************************************************
vars: (d)
(browseLib DataMineLib:	(append _libPath "DataMineLib.db") (append _libPath "DataMineLib/DataMineLib.sl") "file" "ask" "ask" "true" "true")
(browseLib Index:			(append _libPath "Index.db") (append _libPath "Index/Index.sl") "file" "ask" "ask" "true" "true")
(browseLib JavaScript:	(append _libPath "JavaScript.db") (append _libPath "JavaScript/JavaScript.sl") "file" "ask" "ask" "true" "true")
(browseLib Math:			(append _libPath "Math.db") (append _libPath "Math/Math.sl") "file" "ask" "ask" "true" "true")
(browseLib ParseLib:	(append _libPath "ParseLib.db") (append _libPath "ParseLib/ParseLib.sl") "file" "ask" "ask" "true" "true")
(browseLib RulesLib:	(append _libPath "RulesLib.db") (append _libPath "RulesLib/RulesLib.sl") "file" "ask" "ask" "true" "true")
(browseLib Svm:	(append _libPath "Svm.db") (append _libPath "Svm/Svm.sl") "file" "ask" "ask" "true" "true")

;(setq d (new browseLib.dir "Cabinets"))
;(d.remove "TestAis.db")
;(browseLib TestAis: "Cabinets/TestAis.db" "Cabinets/TestAis.sl" "file" "ask" "ask" "true" "true")
;(browseLib.importSource "Cabinets/TestAis.sl")
;(browseLib.setFocus TestAis:)
; (browseLib.compileAll true)

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
(writeln "installPath=" _ais.installPath)
(writeln "currentPath=" _path)
;; end
