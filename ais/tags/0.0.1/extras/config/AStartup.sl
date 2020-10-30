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
;;  AStartup.sl
;;  Title:    Default Startup Script
;;
;;  Project:
;;
;;  Notes:    No other dependencies.  
;;  Do not use this startup to do any particular application 
;;  startup. Instead, create your own directory for your debug
;;  application and create a new AStartup.sl for that application.
;;   

;; ****************************************************************************
;; Application .ini file overrides.
;; ****************************************************************************
;#memory=100

;; ****************************************************************************
;; Load browse agent from a central location
;; ****************************************************************************
(setq _libPath (append _ais.installPath "Libraries/"))
(runScript (append _libPath "BrowseLib/BrowseLib.sl"))

;; ****************************************************************************
;; Lock all globals and display the welcome message.
;; ****************************************************************************
(lock _globals)
(writeln "Welcome to AIS.")

;; end

