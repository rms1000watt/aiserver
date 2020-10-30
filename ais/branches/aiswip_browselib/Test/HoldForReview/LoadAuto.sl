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
;;  Title:    load autotest
;;
;;  Author:   Michael F. Korns
;;
;;  Project:  SpreadLisp Automated test suite 
;;
;;  Notes:
;;
;;  Files:    :Spreadsheets:largedata
;;

;; This script will test opening recalculating and closing a SpreadSheet.

(writeln "load autotest started")
(setq startTime (getTickCount 0))
(loop for  n  from 1  to 3  by 1  do  
   (closeWindow (open-window ":Spreadsheets:largeData" 0 1) true))
(setq endTime (getTickCount startTime))
(writeln "load autotest successful: " endTime " Seconds" )
