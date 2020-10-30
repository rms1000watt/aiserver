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
;;  Title:    RunAll.sl
;;  Author:   Michael F. Korns, Tim May
;;  Project:  AIS Regression Test Suite 
;;  Files:    RegTiny.sl

;#memory=1000
;#memoryObjectHeaders=300

;;Initilize Regression Test Suite Utilities
(runScript "RegTest.sl")

;Locking globals protects global variables created to this point from (clear) calls
(lock _globals)

(runSuite "All" "Both" 1)

