;;/**********************************************************************************
;;    Copyright (C) 2013 Analytic Research Foundation
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
;;  Title:    Lisp MultiTasking Procedures Test
;;
;;  Author:   Michael F. Korns
;;
;;  Project:  AIS Regression Suite
;;
;;  Notes:    The SmartBase features in the MultiTasking Procedures
;;            chapter are tested in this test suite script.
;;
;;  Files:    RegTest.sl
;;            BrowseLib.sl
;;

;#memory=50
;#memoryObjectHeaders=20
(runScript "RegTest.sl")
(testStart "Test_MultiTasking.sl") 

;; Note: At the moment we do not test closeContext because there
;;       is an as yet undiscovered memory leak using contextClose.
(setq testContextCloseSW false)

;;************************************************************************
;;  Define wait function
;;************************************************************************ 
(defun contextWait(context)
  vars:((Integer:waitForBusyLimit 30))
  ;; First wait for the asynchronous context to become busy (BUT don't wait for too long)
  (while (and (= (contextBusy context) false) (> (-- waitForBusyLimit) 0)) do (sleep 1000.0))
  ;; Second wait for the asynchronous context to become not busy
  (while (contextBusy context) do (sleep 100.0))
  true) ; end contextWait
  
;;************************************************************************
;;  Create asynchronous child context startup script in temp folder
;;************************************************************************ 
(browseLib.writeSourceFile "Temp\\Temp_MultiTasking_Child_Startup.sl"
     (append  {;; Lisp MultiTasking asynchronous child startup script} _eol
              {;#ContextName=multitaskingchild} _eol
              {;#memory=50} _eol
              {;#memoryObjectHeaders=20} _eol
              {(runScript "RegTest.sl")} _eol)
     ) ; end write child startup script file

;;************************************************************************
;;  Open an asynchronous child context and try to run test suites therein
;;************************************************************************
(testit {(if (= (setq context (contextExists "multitaskingchild")) false) then (setq context (contextOpen "Temp\\Temp_MultiTasking_Child_Startup.sl" true))) context.ContextName} "multitaskingchild")
(contextWait context)

(testit {(isNumber (contextSubmit context "(runScript \"Test_Trig.sl\")"))} true)
(contextWait context)

(while (contextBusy context) do (sleep 100.0))

(contextSubscribe context false)
(contextSubmit context {(setq x 0)(loop for n from 0 until 10000 do (writeln (++ x))) (append "Finished - " x)})
(while (not (contextBusy context)) do (sleep 100.0))(writeln "Process is starting")
(while (contextBusy context) do (writeln "Progress is: " (setq result (trim (contextGetResult context)))) (sleep 100.0))(writeln "Result is: " (contextGetResult context))

(contextSubscribe context true)
(setq names (contextList))

(testit {(isNumber (contextSubmit context "(runScript \"Test_String.sl\")"))} true)
(contextWait context)

(testit {(isNumber (contextSubmit context "(runScript \"Test_MySQLEmbedded.sl\")"))} true)
(contextWait context)
     
(if (= testContextCloseSW true) then (testit {(contextClose context)} true))

(runScript "Test_MySQLEmbedded.sl")

(testit "(gc compact:)" #void)

(testEnd "Test_MutilTasking.sl")