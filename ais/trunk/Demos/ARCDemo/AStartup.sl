;; *******************************************************************
;; Set Flags, allocate space
;; *******************************************************************
;;; (debug compileon:) ;;generate debug information
;;; (debug erroron:) ;;stop on error in debugger
;#ContextName=ARCDemo
;#memory=2000
;#memoryObjectHeaders=400
(writeln "Memory 2000 Mb, Object Headers 400 Mb")

;; *******************************************************************
;; Load browseLib
;; *******************************************************************
;; For Linux (From Packages)
;; (runScript "/usr/share/ais/Libraries/BrowseLib/BrowseLib.sl")

;; For Windows, and Linux (From Source)
(runScript (append _ais.applicationPath "../../libraries/BrowseLib/BrowseLib.sl"))
(writeln "BrowseLib installed.")

;; *******************************************************************
;; Load Libraries
;; *******************************************************************
;; Add libs information to _ais structure
(setq _ais.libs #(
	;; cabinet name, database name, cabinet source
	#(JavaScript:,	"../../Libraries/JavaScript.db"	,"../../Libraries/JavaScript/JavaScript.sl")
	#(Math:,		"../../Libraries/Math.db"			,"../../Libraries/Math/Math.sl")
	#(ParseLib:,	"../../Libraries/ParseLib.db"		,"../../Libraries/ParseLib/ParseLib.sl")
	#(RulesLib:,	"../../Libraries/RulesLib.db"		,"../../Libraries/RulesLib/RulesLib.sl")
	#(Svm:,			"../../Libraries/Svm.db"			,"../../Libraries/Svm/Svm.sl")
	)
)
(defun loadLibs()
	vars:(i libs)
	(setq libs _ais.libs)
	(loop for i from 0 until (length libs) do
		;;(browseLib aKey aExtentFileName aExtentLocation aExtentStorageScope aImportSync aExportSync aAutoCompile aForceSetting)
		(browseLib libs[i][0] (append _ais.applicationPath libs[i][1]) (append _ais.applicationPath libs[i][2]) "file" "none" "none" "true" "true")
		(writeln (string _ais.libs[i][0]) " installed.")
	) ; end loop
true) ; end loadLibs
(writeln "Loading libraries from " _ais.applicationPath "../../libraries")
(loadLibs) 

;; *******************************************************************
;; Load arcDemo
;; *******************************************************************
(browseLib Arc:     "Arc.db"     "Arc.sl"      "file" "auto" "auto" "true" "true")
(browseLib ArcDemo: "ArcDemo.db" "ArcDemo.sl"  "file" "auto" "auto" "true" "true")
(writeln "ArcDemo installed.")

;; *******************************************************************
;; Intialize arc
;; *******************************************************************
(arc.setOptions sampledREGRESSION: 0% false)	;;  Allow initial scoring, 0% random noise, verbose Off
(browseLib.bind)
(lock _globals)
(writeln "Globals are locked.")
(gc)

;; *******************************************************************
;; Describe arcDemo features
;; *******************************************************************
(writeln "Welcome to the Abstract Regression Classification (ARC) Demo interface running from " _path)
(writeln "ARC Demo provides the steps for you to perform an entire symbolic regression on the data of your choice")  
(writeln "You may use your own training data and test data or you may generate sample data using arcDemo")
(writeln "The following functions provide lots of details on ways to perform a regression:")
(writeln "(arcDemo.showAgents)")
(writeln "(arcDemo.showModels)")
(writeln "(arcDemo.showLinearExample1)")
(writeln "(arcDemo.showHypertangentExample1)")
(writeln "(arc.selfStat)")
(writeln "Ready")
;; end
