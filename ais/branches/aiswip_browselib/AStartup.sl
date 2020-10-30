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
;;  ais/AStartup.sl
;;  Title:    Default Startup Script
;;
;;  Project:  Debugging
;;
;;  Notes:    No other dependencies.  
;;  Do not use this startup to do any particular application 
;;  startup. Instead, create your own directory for your debug
;;  application and create a new AStartup.sl for that application.
;;   

;; ****************************************************************************
;; Application .ini file overrides.
;; ****************************************************************************
;#memory=400
;;;#StartupPrefix=(debug jitoff:)

;; ****************************************************************************
;; Load browse Lambda from a central location
;; ****************************************************************************
(setq _libPath (append _ais.installPath "Libraries/"))
(runScript (append _libPath "BrowseLib/BrowseLib.sl"))

;; ****************************************************************************
;; Load the application cabinets .
;; ****************************************************************************
(browseLib main:					"Cabinets/Main.db" "Test/TestSuiteCabinet.sl" "file" "ask" "ask" "true" "true")
(browseLib dataMineLib:	(append _libPath "DataMineLib.db") (append _libPath "DataMineLib/DataMineLib.sl") "file" "ask" "ask" "true" "true")
(browseLib.setFocus Main:)
(browseLib.compileAll true)

;; ****************************************************************************
;; Connect to the database
;; Attach the dataMineLib to the data repositories.
;; **************************************************************************** 
;(define _dataMineExtents #{
;		testRepository:   	"Datamine/testRepository.db"
;	})
;(dataMineLib _dataMineExtents compress:)

;; ****************************************************************************
;; Initialize application-specific Lambdas
;; ****************************************************************************
;(testLambda)

;; ****************************************************************************
;; Open other contexts for this instance of AIS
;; ****************************************************************************
;(openContext "Test2/AStartup2.sl")

;; ****************************************************************************
;; Debug startup script.  Use StartupScript above to turn the jit off
;; Then, (debug traceon:) below takes effect immediately.
;; ****************************************************************************
;(debug traceon:)

;; ****************************************************************************
;; Lock all globals and display the welcome message.
;; ****************************************************************************
(lock _globals)
(writeln "Welcome to AIS.")

;; end

