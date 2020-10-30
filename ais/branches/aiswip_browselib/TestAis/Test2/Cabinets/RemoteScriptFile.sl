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
; testLambda/remotecriptfile.sl
; In the IDE session window, select Console menu->Run Remote->remotescriptfile.sl->Open
; The following line should appear in the  console tab output pane
(writeln "Hello from RemoteScriptFile.sl")
(defun remoteScriptLambda()
	(writeln "Executing remoteScriptLambda")
)
