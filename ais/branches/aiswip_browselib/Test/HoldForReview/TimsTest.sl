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
(setq _TestPath _path)
(runScript (append _ais.installPath "Libraries/BrowseLib/BrowseLib.sl"))
(browseLib Test: "TestCabinet.db" "TestSuiteCabinet.sl" "file" "ask" "ask" "true" "true")
(browseLib.importSource "TestSuiteCabinet.sl")
(browseLib.compileAll true)
(define _HoneyCombExtents   #("HoneyComb.db"))

;; *******************************************************************
;; Define these global variables so they are protected
;; from the (clear) after each test.
;; *******************************************************************
(define stateInspect (makeVector (add1 testMaxLoops)))
(define testIndex 0)
(lock _globals)

(setq _path _TestPath)
(runScript "ObjRepo.sl")


(systemCheck)(clear)
