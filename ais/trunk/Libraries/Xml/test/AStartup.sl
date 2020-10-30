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
;;  
;;libraries/xml/test/astartup.sl 
;;  Title:    Default Startup Script for aXml Lambda development
;;
;;  Project:  aXml Lambda
;;
;;  Notes:    No other dependencies.  
;;   

;; ****************************************************************************
;; Application .ini file overrides.
;; ****************************************************************************
;#memory=200

;; ****************************************************************************
;; Load browseLib from a central location
;; ****************************************************************************
(setq _libPath (append _ais.installPath "libraries/"))
(runScript (append _libPath "browseLib/browseLib.sl"))

;; ****************************************************************************
;; Load the application cabinets .
;; ****************************************************************************
(browseLib aXML:					"aXml.db")
(browseLib DataMineLib:	(append _libPath "dataMineLib.db"))
(browseLib.addExtent browseLib: (append _libPath "/browseLib.db"))
(browseLib.setFocus aXML:)
(browseLib.compileAll true)

;; ****************************************************************************
;; Connect to the database
;; Attach the dataMineLib to the data repositories.
;; **************************************************************************** 
;(define _dataMineExtents #{
;		testRepository:   	"datamine/testRepository.db"
;	})
;(dataMineLib _dataMineExtents compress:)

;; ****************************************************************************
;; Initialize application-specific Lambdas
;; ****************************************************************************
; (testLambda)


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

