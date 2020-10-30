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
x
;;  
;;  Title:    SmartLisp Host object test suite;;
;;  Author:   Gilda Cabral
;;
;;  Project:  SmartBase Automated test suite 
;;
;;  Notes:    The creation and modification of Host Objects (COM)
;;            are tested in this test suite script.
;;
;;  Files:    No dependencies. 
;;

;;************************************************************************
;; Test script global variables
;;************************************************************************
(setq scriptName "Host Object Procedures Test")

;;************************************************************************
;;  Test Script diagnostic and utility functions
;;************************************************************************
(setq diagnostic writeln)

(defun testit(evalTxt result) 
   (setq lt evalTxt)
   (setq lr result)
   (if (not (equal (eval evalTxt) result ))
       (begin
         (diagnostic scriptName " *FAILURE* " evalTxt)
         (error "hostprocs"))
       true))

;;************************************************************************
;;  Start the Test Script
;;************************************************************************
(writeln   scriptName " started")


;;************************************************************************
;; Tests the Host object set and ref
;;************************************************************************

(setq x (new HostObject: "ActiveXObject.Test"))

(testit "(setq x.Color \"BLUE\")" "BLUE")

(testit "x.Color" "BLUE")

(testit "x[\"Color\"]" "BLUE")

(testit "(setq x.NewTestFlag true)" true)

(testit "x.NewTestFlag" true)


;;************************************************************************
;; Tests the Host object send
;;************************************************************************


(testit "(send callme: x \"YELLOW\")" "Hello:YELLOW")

(testit "x.Color" "YELLOW")

;;************************************************************************
;; Tests the Host Object print
;;************************************************************************

(setq x.NewTestFlag "RED")
(setq x.Color "BLUE")
(writeln x)

 
;;************************************************************************
;; Tests the Host object garbage collection
;;************************************************************************

(setq x #void)
(gc)

(writeln   scriptName " completed")









