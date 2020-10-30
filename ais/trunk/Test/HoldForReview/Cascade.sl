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
;;  Title:    Cascade test
;;
;;  Author:   Michael F. Korns
;;
;;  Project:  SpreadLisp Automated test suite 
;;
;;  Notes:
;;
;;  Files:   :Spreadsheets:cascade
;;

;; This script will test opening recalculating and closing a SpreadSheet.

(writeln "recalc autotest started")
(open-window ":Spreadsheets:cascade" 0 1)
(setq startTime (getTickCount 0))
(recalc @|:Spreadsheets:cascade| 1)
(setq endTime (getTickCount startTime))
(closeWindow  @|:Spreadsheets:cascade|.window true)
(writeln "recalc autotest successful: " endTime " Seconds" ) 
