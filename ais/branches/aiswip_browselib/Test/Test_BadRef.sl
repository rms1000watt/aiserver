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
;;  Title:    Bad Variable Ref Test
;;
;;  Author:   Michael F. Korns, Tim May
;;
;;  Project:  SpreadLisp Automated test suite 
;;
;;  Notes:    A subset of the compiler features are acceptance tested 
;;            in this test script.
;;
;;  Files:    No dependencies. 
;;

(setq scriptName "Bad Variable Ref Test")

;#memory=500
;#memoryObjectHeaders=100
(runScript "RegTest.sl")
(testStart "Test_BadRef.sl")


;; *******************************************************************
;; name:     foo
;; summary:  This tests the compiler.
;; *******************************************************************
(defun foo(...) 
   vars:(i n x)
   (setq n (argCount))
   (setq x (makeVector n))
   (loop for i from 0 until n do
      (setq x[i] (argFetch i)))
   x)

;; *******************************************************************
;; note:     ...start the compiler test...
;; *******************************************************************


(testit "(foo 1 2 3 4)" #(1 2 3 4))

(testEnd "Test_BadRef.sl")


