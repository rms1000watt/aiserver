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
;;  Title:    SmartLisp Workspace Objects Test
;;
;;  Author:   Michael F. Korns, Tim May
;;
;;  Project:  AIS Regression Suite
;;
;;  Notes:    The SmartBase features in the Workspace Objects
;;            chapter are tested in this test suite script.
;;
;;  Files:    RegTest.sl
;;

;#memory=500
;#memoryObjectHeaders=100
(runScript "RegTest.sl")
(testStart "Test_WorkSpac.sl")

;;************************************************************************
;; Tests for the _currentViews Workspace Object
;;************************************************************************

(testit "(type _currentViews)" Workspace:)


(testit "(length _currentViews)" 0)

;;(setq anLambda (defun foo (i) (+ i 1)))
;;(addView anLambda)

;;(testit "(length _currentViews)" 1)

;;(deleteView anLambda)

(testit "(length _currentViews)" 0)



;;************************************************************************
;; Tests for the new Workspace Workspace Object
;;************************************************************************

(setq x (new Workspace:))
(testit "(type x)" Workspace:)

(testEnd "Test_WorkSpac.sl")



