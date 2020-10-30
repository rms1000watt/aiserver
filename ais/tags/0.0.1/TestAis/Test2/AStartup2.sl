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
;; testais/astartup2.sl       tlwms         4/24/03
;;  
;;  Title:    TestLambda
;;  Author:   Ted Williams
;;  Project:  testLambda
;;  Notes:    No other dependencies.  
;; 
;;	NOTE:
;; 1. To set the AIS executable that executes when a .sl is selected: 
;; 	In Microsoft's explorer, select Tools->Folder Options->File Types
;;	Scroll down to SL Files, select Advanced->open->Edit->Browse
;;	Navigate to the webide.exe in the aisdev dir	  
;; ****************************************************************************
;; Application .ini file overrides.
;; ****************************************************************************
;#memory=64

;; ****************************************************************************
;; Load browse Lambda from a central location
;; ****************************************************************************
(setq _libPath (append _ais.installPath "libraries/"))
(runScript (append _libPath "browseLib/browseLib.sl"))

;; ****************************************************************************
;; Load the application cabinets locally. The rest from installPath
;; ****************************************************************************
(browseLib Main:				"cabinets/Main.db")
(browseLib DataMineLib:	(append _libPath "dataMineLib.db"))
(browseLib Index:					(append _libPath "index.db"))
(browseLib JavaScript:		(append _libPath "javaScript.db"))
(browseLib ParseLib:		(append _libPath "ParseLib.db"))
(browseLib RulesLib:		(append _libPath "rulesLib.db"))
(browseLib.setFocus Main:)
(browseLib.compileAll true)

;; ****************************************************************************
;; Connect to the database
;; Attach the dataMineLib to the data repositories.
;; **************************************************************************** 
(define _dataMineExtents #{
		testRepository:   	"datamine/testRepository.db"
	})
(dataMineLib _dataMineExtents compress:)

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
;;(test2Lambda)

;; ****************************************************************************
;; Lock all globals and display the welcome message.
;; ****************************************************************************
(lock _globals)
(writeln "Welcome the the Test2 context")
;;;(writeln "libPath=" _libPath)


