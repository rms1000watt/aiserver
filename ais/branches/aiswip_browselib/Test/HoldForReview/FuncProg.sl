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
;;  Title:    SmartLisp Functional Programming Test
;;
;;  Author:   Michael F. Korns
;;
;;  Project:  SmartBase Automated test suite 
;;
;;  Notes:    The SmartBase features in the Functional Programming
;;            chapter are tested in this test suite script.
;;
;;  Files:    No dependencies. 
;;

;;************************************************************************
;; Test script global variables
;;************************************************************************
(setq _path "e:/abase/test/")
;;#CLEARCONSTRAINTS#
(setq scriptName "Functional Programming Test")

;;************************************************************************
;;  Test Script diagnostic and utility functions
;;************************************************************************
(setq diagnostic writeln)

(defun testit(evalTxt result) 
   (setq lt evalTxt)
   (setq lr result)
   (if (not (isEqual (eval evalTxt) result ))
       (begin
         (diagnostic scriptName " *FAILURE* " evalTxt)
         (error "funcprog"))
       true))

;;************************************************************************
;;  Start the Test Script
;;************************************************************************
(writeln   scriptName " started")

;;************************************************************************
;; Tests for the setf Functional Procedure
;;************************************************************************

;;(setq %y 25)
;;(setf %x { (sqrt %y) })
;;(testit "%x" 5)

;;************************************************************************
;; Tests for the setq Functional Procedure
;;************************************************************************

(setq %y 64)
(testit "%x" 8)

;;************************************************************************
;; Tests for the setv Functional Procedure
;;************************************************************************

;; (setf %z { (1+ %z) })
(setq %z 4)
(testit "%z" 5)
(setv %z 0)
(testit "%z" 0)

(writeln   scriptName " completed")



