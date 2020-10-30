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
;;  Title:    AStartup.sl
;;  Author:   Franklin Chua, Tim May
;;  Project:  Demos Project
;;	Loads the libraries

;; *******************************************************************
;; Load browseLib
;; *******************************************************************
;; For Linux (From Packages)
;; (setq libPath "/usr/share/ais/Libraries/")

;; For Windows, and Linux (From Source)
(setq libPath (append _ais.installPath "Libraries/"))

(runScript (append libPath "BrowseLib/BrowseLib.sl"))
(writeln "BrowseLib installed.")

;; Add libs information to _ais structure
(setq _ais.libs #(
	;; cabinet name, database name, cabinet source
;;	#(DataMineLib:,	"Binaries/DataMineLib.db"	,"DataMineLib/DataMineLib.sl")
;;	#(Index:,		"Binaries/Index.db"			,"Index/Index.sl")
;;	#(ParseLib:,	"Binaries/ParseLib.db"		,"ParseLib/ParseLib.sl")
	#(JavaScript:,	"Binaries/JavaScript.db"	,"JavaScript/JavaScript.sl")
	#(RulesLib:,	"Binaries/RulesLib.db"		,"RulesLib/RulesLib.sl")
	#(Math:,		"Binaries/Math.db"			,"Math/Math.sl")
;;	#(Xml:,			"Binaries/Xml.db"			,"Xml/Xml.sl")
;;	#(Svm:,			"Binaries/Svm.db"			,"Svm/Svm.sl")
;;	#(Esm:,			"Binaries/Esm.db"			,"Esm/Esm.sl")
    #(RunQueue:,    "Binaries/RunQueue.db"		,"RunQueue/RunQueue.sl")
	#(Gsm:,			"Binaries/Gsm.db"			,"Gsm/Gsm.sl")
	)
)

(defun loadLibs()
	vars:(i libs)
	(setq libs _ais.libs)
	(loop for i from 0 until (length libs) do
		;;(browseLib aKey aExtentFileName aExtentLocation aExtentStorageScope aImportSync aExportSync aAutoCompile aForceSetting)
		(browseLib libs[i][0] libs[i][1] (append libPath libs[i][2]) "file" "auto" "auto" "true" "true")
		(writeln (string _ais.libs[i][0]) " installed.")
	) ; end loop
true) ; end loadLib

(loadLibs) 
 
(browseLib.bind)

(lock _globals)
(writeln "Globals are locked.")
(writeln "")

(writeln "This script executes available jobs in the JobQueue.")
(writeln "Click Stop to terminate the script.")
(runQueue.run)

