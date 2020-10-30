;;*************************************
;;*************************************
;; Exported Lambda File Cabinet Document
;;*************************************
;;*************************************
;;version=5.005-32bit
;;location=DataMineLib.sl
;;storageScope=file
;;importSync=auto
;;exportSync=auto
;;autoCompile=true
;;cabinetHash=#<Dictionary 1842452>
;;objRepoName=DataMineLib.db
;;lastImport=#Feb,23,2009:02:01:35
;;fileHash=gv
Îâ'ƒÊf¸}pû
;;END __DATA

;;**EXPORTKEY**:dataMineLib
(defun dataMineLib(...)
;; *******************************************************************
;; Summary:  Data Mining Librarian Lambda which manages a multiple table 
;;           data mine and controls the data mine schema. The data mine
;;           schema is similar to a relational schema with enhancements  
;;           for warehouse data models, relational tables, blackboard 
;;           knowledge sharing areas, table scoring, and table filtering.
;;
;;           The data mine contains multiple physical areas, called extents.
;;           Each of these physical areas may contain tables. The
;;           usingForCreate function determines in which extent new
;;           tables are created.
;;
;; Args:    dataMineExtents   The Structure of data mine file extents.
;;          compress:         Force repository extents to use compression (optional).
;;
;; Return:  true
;;			Note: If you call dataMineExtents with no argument after having called 
;;			it at least once with arguments, dataMineLib will reinitialize itself.
;;			This is useful if you have bombed out of some dataMineLib activity
;;			due to an error and would like to resume working in dataMineLib.
;;
;;			On the first call to dataMineLib, the initialization argument 
;;			must contain a Structure of data mine file extents. There may 
;;			be multiple named data mine extents. The attributes of the data 
;;			mine extent structure is as follows.
;; 
;;        Attributes:  
;;              name:         "The extent path and file name for the extent"
;;
;; Example:
;;	
;;   This example initializes a data mine with a reference extent 
;;   and a blackboard extent. Each extent must specify the complete 
;;   path and file name. Extents may be placed on different physical devices 
;;   as database management strategy requires. Each extent supports up to 
;;   2 gigabytes of storage space. For example:
;;
;;       (define dm #{Reference: "Reference.db" Blackboard: "Blackboard.db"})
;;       (dataMineLib dm)
;;
;;       Notes: The start up script should contain code to this effect, or 
;;              the client should issue code like this at startup.
;;
;; Args:    filterString      The javaScript source to be parsed.
;; Return:  parseTree         The LambdaServer universal parse tree. 
;;
;;          The argument must contain a string of valid javaScript language 
;;          source code to be parsed.
;;
;;tm! My notes
;; dmExt[] is a structure that contains the extent schemas that comprise the data mine.
;; An extent schema contains these elements
;; dmExt[extent].name	  	String containing name of extent file ex: "Blackboard.db"
;; dmExt[extent].repos 		The object repository for the extent
;; dmExt[extent].transCount	A count of open beginTransactions (new added by tim, replaces use of cdr) 
;; openTables				A dictionary of cursorList vectors keyed by tablename
;; databaseNames			A vector of database names.
;; datamineTables			A dictionary of composite extentIndex/databaseNamesIndex entries keyed by tablename
;;								ExtentIndex is the extent of the tables respository in dmExt.
;;								DatabaseNamesIndex is the indext to the tables database in the databaseNames vector
;; openBufs					A dictionary of buffer structures keyed by tablename
;;								Note that only static and disk cursors use openBufs
;;								The buffer structure is #{bckVector: #void buffer: #void hits: #void bufSize: 0 bufUsed: 0 chunckSize: 128000 numBufRecs: 0}
;;								bckVector -- A vector frame ids for the table. bckVector is share by static and disk cursors for table
;;								buffer    -- A vector of objects contain the buffered table records
;;								bufSize   -- An integer containing the maximum number of records to buffer
;;								bufUsed:  -- An integer containing thecurrent number of records buffered.
;; Buffering Strategy
;; 	Buffering has been introduced to enhance random access on static and disk cursors. 
;;	When you open a static or disk cusor you can specify a bufSize of -1 to open a non-buffered 
;;	cursor, 0 to open a buffered cursor that uses a previously specified
;; 	bufSize or a positive integer specifying a new bufSize to be used for all open buffered 
;;	cursors on the table.
;;
;; 	The buffering strategy is a very simple one. The first time a record is read it becomes an 
;;	object allocated on the heap. A reference to the record object is assigned into buffer[i] 
;;	where i is the corresponding row number in bckVector. Each record object is a vector and 
;;	as such it has a cdr value. We use the cdr value of the buffered object as a place to store a
;;	composite linked list value. The integer portion of the cdr value contains the index into
;;	buffer[] of the previous buffered item. The fractional portion of the cdr value contains an
;;	index into buffer[] of the next buffered item. In this manner we are able to maintain a 
;; 	linked list of buffered items and manipulate this list easily when new records are read
;;	and need to be inserted into the buffer. The linked list is especially useful when we need
;;	to remove buffered items because we bufUsed exceeds bufSize and we need to remove buffered
;;	items from the buffer[] vector. The advantage of this approach is that we use no extra storage
;;	for the linked list as each buffered object already has memory allocated for a cdr value.
;;
;; 	Notes on Mobile Extent technology. dataMineLib allows exents to be used in different dataMines 
;;	by simple inclusion. For this reason it is possible for a table with the same name to appear in 
;;	more than one extent belonging to the same dataMine. Multiple same name tables can not be 
;;	accessible at the same time so the order in which the extents are included in the dataMine
;; 	is used to determine which table has "precedence". Note that the deletion or addition of tables 
;;	may affect this precedence and appropriate logic exists to update the dataMineTables as necessary 
;;	with deletion or addition of tables.
;;
;;
;; need api to add, swap and delete extents. Note that these api will need to support update of the dataMineTables!
;;
;;
;; tm! end of my notes
;;
;; Repository Extent Layout:
;; Notes:    Each data mine repository extent contains tables in the data mine. Each
;;           repository extent is a simple associative memory containing the
;;           extent schema and all table record vectors. All table records are
;;           stored at the individual repository frame referenced in the table record
;;           vectors. The schema and table record vector blocks are also stored at 
;;           frame referenced.
;;
;;           The extent Schema is housed in the main repository to
;;           allow faster schema access, and to provide a single locking
;;           point for the entire data mine extent. The schema is a simple 
;;           associative memory with data mine "tablenames" as its keys.
;;           Each data mine "tablename" is associated with its 
;;           corresponding schema record. The schema record layouts are as follows:
;;
;;           table cursor	bckVectorKey        The repository key of the backup record view vector.
;;                          colCount	      	The number of columns in the table.
;;                          colVector	     	The table's vector of column names.
;;                          myMemoPad           The table's memo pad (always a Dictionary).
;;                          myDatabaseName      The table's logical database name (not the name of the table).
;;                                                (Note: tables are grouped into logical databases.)
;;                          myObjectName        The table's name (identical to its data mine object name).
;;                          myObjectType        The object's type (table or view).
;;                          myTableName         NOT USED
;;                          myTableDate         NOT USED
;;                          recordCount	      	The table's current total record count.
;;                          recordStructure		The table's record structure (an attributed vector).
;;                          rowVectorKey        NOT USED
;;                          validFields   		The table's valid fields and types (necessary for query building).
;;                          viewDirectoryKey    The repository key for the Directory of saved table record view vectors.
;;
;; Depends:  browseLib
;;           rulesLib
;; Args:     none
;; Return:   true
;; *******************************************************************
  pvars:(autoNameSerial          ;; Unique modular automatic name serial number.
		 currentTable			 ;; Filter Lambda uses this to keep track of the table being filtered
         dmExt                   ;; The Structure of data mine extent path and file names.
         memoPad                 ;; Users may store notes to themselves here.
         myParent                ;; Holds the parent Lambda object reference.
         mySelf					 ;; Holds a reference to dataMineLib (myself)
         						 ;; This allows child Lambdas to find their parent Lambda
         usingDatabase           ;; The name of the logical database (current default).
         usingRepos              ;; Repository object last set by the usingForCreate Lambda.
         usingExt			 	 ;; Location name of extent last set by usingForCreate Lambda
	 	 usingExtIndex			 ;; Index into dmExt of extent last set by usingForCreat 
         openTables              ;; Dictionary of currently opened tables and the cursors that refer to them
         openBufs			     ;; Dictionary of currently open buffers 
         databaseNames			 ;; Vector containing the names of each database
         dataMineTables			 ;; Dictionary of currently accessible tables in dataMineExtents
         						 ;; Note: Some tables may be inaccessible because of our mobile extent technology. See discussion above.
         ;; public methods
         delimitedString         ;; Returns a delimited string of HTML output.
         addTableColumn          ;; Adds a new column to the specified primary table.
         clearCursors            ;; Closes all open cursors
         clearDataMine           ;; Initializes the data mine multiple repository extents.
         clearMemoPad            ;; Clears the data mine memo pad of any open cursors.
         close                   ;; Terminates a transaction with the specified table in the data mine.
         compileLambda            ;; Compile a javaScript data mine Lambda.
         copyTable               ;; Copies one data mine table to another.              
         createIndex			 ;; Create an index on specified table columns
         createMetaTable		 ;; Creates a table containing references to the records from a list of member tables
         createTable             ;; Creates an entry for the specified relational table.
         deleteIndex			 ;; Delete an index from table
         deleteRecord            ;; Deletes the specified row from the table in the data mine.
         dropTable               ;; Deletes the specified object from the data mine.
         exportTab               ;; Exports the specified ASCII tab delimited file from the table in the data mine.
         filterTable             ;; Runs the specified javaScript Lambda against the specified data mine table.
         getDatabaseNames        ;; Returns the names of all logical databases of any tables and views in the data mine.
         getExtentNames          ;; Returns the names of all physical database extents in the data mine.
         getTableDatabaseName    ;; Returns the logical database name of the specified table or view.
         getTableExtent          ;; Returns the index of the data mine extent containing the specified table.
         getTableNames           ;; Returns the names of all tables and views in the data mine.
         importTab               ;; Imports an ASCII tab-delimited file into a data mine table.
         isTable                 ;; Returns true iff the specified table exists.
         (keyMask 1048576)       ;; Meta index row mask
         mergeTables             ;; Merges two tables based upon a matching merge field.
         mergeThreeTables        ;; Merges three tables based upon a matching merge field.
         miner                   ;; Manages the data mining operations of the data mine Lambda.
         open                    ;; Begins a transaction with the specified table in the data mine.
         readRecord              ;; Reads the specified row from the table in the data mine.
         recordDetails           ;; Converts a data mine table record into a record details HTML form.
         recordDetailsURL        ;; Converts a data mine table record into a record details url.
         renameTable             ;; Renames the specified relational table.
         reportTiming			 ;; Report timing collected from cursor activity
         restoreTable            ;; Restore a table to its back up view.
         rollback				 ;; Rollback open transactions on specified extent
         setTableDatabaseName    ;; Sets the logical database name of the specified table or view.
         sortTable				 ;; Sort a table
         systemCheck             ;; Performs a system check on the data mine.
         updateTable             ;; Update a table by updating each record.
         usingForCreate          ;; Sets which extent new objects are created in.
         writeRecord             ;; Writes the specified row to the table in the data mine. 
         tempTable               ;; Temporary scratchpad memory table table manager unassociated with any data mine database table.
         TmemoryCursorTimes		 ;; structure containing timing data for memory cursors
         TdiskCursorTimes		 ;; structure containing timing data for disk cursors
         TstaticCursorTimes		 ;; structure containing timing data for disk cursors
         TmemoryCursorCounts	 ;; structure containing timing data for memory cursors
         TdiskCursorCounts		 ;; structure containing timing data for disk cursors
         TstaticCursorCounts	 ;; structure containing timing data for disk cursors
         
         ;; private methods
         __beginTrans			 ;; Begin transaction and bump trans count
         __rollbackTrans		 ;; Rollback transaction and decrease trans count
         __commitTrans			 ;; Commit transaction and decrease trans count
         __compileLambda          ;; Compile a javaScript data mine Lambda.
         __collectTableNames     ;; Collect all tables names into in memory dictionaries
         __tableCursor           ;; Cursor manager for tables
         __tableExtentMgrTemplate;; The extent manager template to register the data mine to the browseLib. 
         __newAutoName           ;; Returns a new data mine automatic name (guaranteed to be unique).
         __updateBegin           ;; Begins a transaction with the specified table in the data mine.
         __viewAND               ;; Combines two view vectors using the logical AND operation.
         __viewOR                ;; Combines two view vectors using the logical OR operation.
         __viewXOR               ;; Combines two view vectors using the logical XOR operation.
         )   ;;end of persistant variables
  ;; If no user name, generate a new name automatically.
  vars:(indexOf extentName cursorName cursor compressSW tableName dataMineExtents
        tableSchema extentIndex extentCount schemaIndex schemaCount namesDictionary)
    ;; Compiles a javaScript data mine Lambda against the specified table.
    (defun __compileLambda(javaString)
       vars:(theLambda)
	   (STARTTIME)
       (onError (lambda(s) s))
       (setq theLambda (compile (morph (javaScript javaString))))
       (setq theLambda (theLambda))
       (SETTIME)
       theLambda) ;; end of compileLambda
    ;; Returns a new data mine automatic name (guaranteed to be unique).
    (defun __newAutoName()
    	vars:(result) 
	   (STARTTIME)
       (setq autoNameSerial (modi (add1 autoNameSerial) 100000)) 
       (setq result (symbol (append "_Auto__" autoNameSerial "_" (now))))
       (SETTIME)
       result) ;; end __newAutoName

    ;; summary:  Begins a transaction with the specified table in the data mine (does not return an error).
    (defun __updateBegin(tableName) 
    vars:(result)
   		(STARTTIME)
    	(onError (lambda(s) #void)) 
    	(setq result (open tableName))
        (SETTIME)
    	result)
    ;; *******************************************************************
    ;; Close all open table cursors
    ;; *******************************************************************
    (defun clearCursors()
    	vars:(i j numTables numCursors)
	    (STARTTIME)
    	(setq numTables (length openTables)) 
    	(loop for i from 0 until numTables do
    		(setq numCursors (length openTables[i].cursorList))
    	 	(loop for j from 0 until numCursors do
    	 	 	(setq openTables[i].cursorList[j] (close openTables[i].cursorList[j]))
    	 	) 
    	)
       (SETTIME)
    true)
    (defun close(cursor ...)
       ;; *******************************************************************
       ;; summary:  Ends the current transaction
       ;; Args:     cursor     The transaction cursor
       ;;           save:  	   Save the memoPad on a disk cursor even if no other
       ;;						data has been modified.
       ;; Return:   true
       ;; *******************************************************************
	   (STARTTIME)
       (cond
         ((and (isLambda cursor) (= (argCount) 2)) (cursor.__close (argFetch 1)))
         ((isLambda cursor) (cursor.__close))
         ) ; end cond
       (SETTIME)
       #void) ;; end of close
    ;; Compiles a javaScript data mine Lambda against the specified table.
    (defun compileLambda(javaString)
       vars:(theLambda)
	   (STARTTIME)
       (setq theLambda (compile (morph (javaScript javaString))))
       (setq theLambda (theLambda))
       (SETTIME)
       theLambda) ;; end of compileLambda
   ;; Copies one data mine table to another.
   (defun copyTable(oldTableName newTableName physicalArea databaseName)
       vars:(record cursorIn cursorOut i n)
	   (STARTTIME)
       ;; Create the new table in the data mine.
       (if (= oldTableName newTableName) (error "dataMineLib.copyTable: table names must be different."))
       (setq cursorIn (open oldTableName))
       (dropTable newTableName)
       (usingForCreate physicalArea databaseName)
       (createTable newTableName cursorIn.colVector)
       (setq cursorOut (open newTableName))
       (loop for i from 0 until cursorIn.recordCount do
          (setq record (cursorIn.read i))
          (cursorOut.write i record)
          ) ; end loop
       (setq cursorIn (close cursorIn))
       (setq cursorOut (close cursorOut))
       (SETTIME)
       true) ; end copyTable

	;; Create an index on the specified table
   (defun createIndex (tableName indexName keySpec ...)
		vars:(args numArgs arg i cursor)
	    (STARTTIME)
		(setq args (new Vector:))
		(setq args[0] indexName)
		(setq args[1] keySpec)
		(setq numArgs (argFetch))
		(if (> numArgs 4) (error "dataMineLib.createIndex: More than 4 arguments passed"))
		(if (> numArgs 3) 
			(begin
			(setq arg (argFetch 3))
			(if (<> arg unique:) (error "dataMineLib.createIndex: 4th argument must be the symbol unique:"))
			(setq args[2] unique:)
			end)
		);if
		
		(setq cursor (open tableName))
		(apply cursor.createIndex args)
		(setq cursor (close cursor))				
        (SETTIME)
	true)

	;; Delete a specified index on a specified table
   (defun deleteIndex (tableName indexName)
   		vars:(cursor)
	    (STARTTIME)
   		(setq cursor (open tableName))
   		(cursor.deleteIndex indexName)
   		(setq cursor (close tableName))
        (SETTIME)
   	true)


    ;; Deletes the specified row from the table in the data mine.
    (defun deleteRecord(tableName rowIndex)
       vars:(cursor)
	   (STARTTIME)
       (setq cursor (open tableName disk:))
       (cursor.delete rowIndex)
       (setq cursor (close cursor))
       (SETTIME)
       true) ;; end of deleteRecord
    ;; Returns a delimited string of HTML output.
    (defun delimitedString(field)
       vars:(result fieldIndex fieldLen n)
	   (STARTTIME)
       ;; If the field is void, display as a blank.
       (if (= field #void) (return " "))
       ;; If the field is a Directory, etc., display as a list box with the last item selected.
       (setq fieldLen (length field))
       (if (or (= (type field) Directory:) (= (type field) Dictionary:) (= (type field) Structure:))
           (begin
              (if (= fieldLen 0) (return " "))
              (setq lastIndex (subi fieldLen 1))
              (setq result " <SELECT>")
              (loop for fieldIndex from 0 until fieldLen do
                 (if (<> fieldIndex lastIndex)
                     (setq result (append result "<OPTION>"))
                     (setq result (append result "<OPTION SELECTED>"))
                     ) ; end if
                 (setq result (append result field[fieldIndex 0] " = " 
                                              (substitute (string field[fieldIndex 1] true) "#" "")))
                 ) ; end loop
              (setq result (append result "</SELECT>"))
              (return result)
              )) ; end if  
       ;; If the field is a Vector, display as a list box with the last item selected.
       (setq fieldLen (length field))
       (if (= (isVector field) true)
           (begin
              (if (= fieldLen 0) (return " "))
              (setq lastIndex (subi fieldLen 1))
              (setq result " <SELECT>")
              (loop for fieldIndex from 0 until fieldLen do
                 (if (<> fieldIndex lastIndex)
                     (setq result (append result "<OPTION>"))
                     (setq result (append result "<OPTION SELECTED>"))
                     ) ; end if
                 (setq result (append result (substitute (string field[fieldIndex] true) "#" "")))
                 ) ; end loop
              (setq result (append result "</SELECT>"))
	          (SETTIME)
              (return result)
              )) ; end if  
       (SETTIME)
       ;; If the field is a singleton, display as a string.
       field) ;; end delimitedString
    (defun exportTab(tableName asciiFile)
       ;; *******************************************************************
       ;; summary:  Exports the specified ASCII tab delimited file from the 
       ;;           table in the data mine.
       ;; Args:     tableName      Name of the table from which to export data
       ;;           asciiFile      Fully-qualified pathname of the file to export
       ;; Return:   true
       ;; *******************************************************************
       vars:(cursor)
	   (STARTTIME)
       (setq cursor (open tableName))
       (cursor.exportTab asciiFile)
       (close cursor)
       (SETTIME)
       true) ;; end of exportTab
    ;; Runs the specified javaScript Lambda against the specified data mine table.
    (defun filterTable(tableName javaLambda)
       vars:(result cursor)
	   (STARTTIME)
       (setq cursor (open tableName))
       (if (isString javaLambda) (setq javaLambda (compileLambda javaLambda)))
       (setq result (javaLambda cursor))
       (setq cursor (close cursor))
       (SETTIME)
       result) ;; end filterTable
    ;; Returns the index of the data mine extent containing the specified table.
    (defun getTableExtent(tableName)
       	vars:(indexOf extIndex)
	    (STARTTIME)
       	(setq indexOf (member tableName dataMineTables ))
		(if (isNumber indexOf)
			(return (integer dataMineTables[indexOf 1])); extract the extent index from coposite index
		);if
       (SETTIME)
       	false) ;; end getTableExtent
    (defun importTab(tableName option asciiFile)
       ;; *******************************************************************
       ;; summary:  Imports the specified ASCII tab delimited file into the 
       ;;           table in the data mine. 
       ;; Args:     tableName      Name of the table from which to export data
       ;;           option         If overwrite: the table is overwritten, 
       ;;                          if append: the rows are added to the end of the table
       ;;           asciiFile      Fully-qualified pathname of the file to export
       ;; Return:   true
       ;; *******************************************************************
       vars:(cursor)
	   (STARTTIME)
       (setq cursor (open tableName))
       (if (= option overwrite:) (cursor.drop))
       (cursor.importTab asciiFile)
       (close cursor)
       (SETTIME)
       true) ;; end of importTab



    ;; Returns true iff the specified table exists.
    (defun isTable(tableName) 
      vars:(result)
      	(STARTTIME)
    	(setq result (isNumber (getTableExtent tableName)))
       (SETTIME)
    	result)
    (defun open(tableName ...)
       ;; *******************************************************************
       ;; summary:  Begins a transaction with the specified table in the data mine.
       ;; Args:     tableName   The table on which to operate. Must be first argument
       ;;           cursorType	memory: 				A memory cursor is to be opened.
       ;;                      	disk: or exclusive: 	An exclusive write disk cursor is opened
       ;;                       static: 				A read-only disk cursor is opened.
       ;;           columns     Column. Vector of Column symbols.
       ;;			bufferSize	Buffer Size. Number specifying number of records to buffer.
       ;; 			createEmpty	Optional argument specifying that a memory cursor should be opened with no data. 
	   ;;	Index Management arguments
       ;;			noIndices:		Optional argument specifying that indices should not be loaded
       ;;			withIndices:	Optional argument specifying that indices should be loaded
       ;;			loadByIndex:	Optional argument specifying that backVector should be loaded in index order
       ;;			indexName		Argument namning index to load if loadByIndex specified (must immediately follow loadByIndex:)
       ;;			keyVal			Optional argument for filtering records read in index (affects current view only)
       ;; Return:   cursor       The transaction cursor
       ;; *******************************************************************
       vars:(cursor myCols myBufSize myCursorType cursorTable i j
       		loadByIndexSW enableIndicesSW withIndicesSW indexName keyVal partialSW createEmptySW arg numArgs)
	   (STARTTIME)
	   ; Set defaults       
       	(setq myCols #void)			; set default column spec
		(setq myBufSize -1)			; use non-buffered cursor
       	(setq myCursorType disk:)	; set default cursor type to disk:
       	(setq loadByIndexSW false)
       	(setq withIndicesSW #void)
       	(setq createEmptySW false)

		;; Before we create a new cursor, we need to ensure that dataMineLibs Pv variables that will be
		;; used by the cursor are initilized. Remember that the cursor gets a copy of the content of the referenced
		;; variables!
		(if (= openBufs #void)   (setq openBufs (new Dictionary:)))

       ; Determine by type which arguments have been passed
       (setq j 0); j is the counter for positional matching of the loadByIndex arguments
       (setq numArgs (argCount))
	   (loop for i from 1 until numArgs do
	   		(setq arg (argFetch i))
	   		(if (not loadByIndexSW)
		   		(cond ; check for arguments that must preceed load by index arguments
		    	((isNumber arg) (setq myBufSize arg))
		    	((isVector arg) (setq myCols arg))
		    	((= arg #void) (setq myCols #void))
		    	((isSymbol arg) 
		    		(cond
		    		((or (= arg memory:) (= arg static:) (= arg disk:) (= arg exclusive:)) (setq myCursorType arg))
		    		((= arg noIndices:)			(setq withIndicesSW false))
		    		((= arg withIndices:)		(setq withIndicesSW true))
		    		((= arg createEmpty:)		(setq createEmptySW true))
		    		((= arg loadByIndex:)		(begin (setq loadByIndexSW true) (setq withIndicesSW true) end))
		    		);cond
		    	)
		    	(true (error "dataMineLib:open -- Invalid argument passed"))
		    	);cond
		    else ; check for correct loadByIndex arguments and other misc arguments
		    	(cond
		    	((= j 0) (setq indexName arg) (setq j (iadd j 1)))
		    	((= j 1) (setq keyVal arg) (setq j (iadd j 1)))
		    	(true (error "dataMineLib.__tableCursor.open: bad argument"))
		    	);cond
		    );if (not loadByIndexSW) ..
	   );i

	   (if (= myCursorType exclusive:) (setq myCursorType disk:)) ;exclusive: and disk: are the same
	   (if (= myCursorType disk:) ; Make sure we do not already have a disk: cursor open on this table
	       (if (<> (setq cursorTable openTables[tableName]) #void)
	          (if (> (length cursorTable.cursorList) 0)
                (loop for i from 0 until (length cursorTable.cursorList) do 
                   (if (= cursorTable.cursorList[i].myCursorType disk:)
                       (error (append "dataMineLib.open: an exclusive disk cursor already exists on table " tableName))
                   )
                );i
	          ); if
	       ); if
	    ); if

;(writeln "myBufSzie=" myBufSize " myCols=" myCols " myCursorType=" myCursorType)


		;; Create default table definitions for memory cursors whose tableNames do not already exist
		(if (and (= myCursorType memory:) (not (isTable tableName)))
            (if (<> myCols #void) then (createTable tableName myCols) else (createTable tableName #(obj| A:)))
		);if		

		;; set enableIndicesSW and withIndicesSW for cursor type
		(if (= myCursorType memory:)
			(if (= withIndicesSW #void) (setq withIndicesSW false)) ;; << default is to not load defined indices >>
			(if (= withIndicesSW #void) (setq withIndicesSW true))  ;; << default is to load any defined indices >>
		);if		

 		;;make sure option combinations make sense
		(if (and loadByIndexSW (not  withIndicesSW))
				(error "dataMineLib.__tableCursor.open: bad argument options"))
	
		(setq cursor (new __tableCursor))  
;(writeln "tableName "tableName " myCursorType "myCursorType " myCols "myCols " myBufSize " myBufSize " enableIndicesSW "enableIndicesSW " withIndicesSW "withIndicesSW " loadByIndexSW "loadByIndexSW " indexName "indexName " keyVal "keyVal " partialSW "partialSW " createEmptySW" createEmptySW) ; init cursor and open on specified table

	   	(cursor mySelf tableName myCursorType myCols myBufSize withIndicesSW loadByIndexSW indexName keyVal createEmptySW) ; init cursor and open on specified table

       (SETTIME)
       	cursor) ;; end of open

    ;; Reads the specified row from the table in the data mine.
    (defun readRecord(tableName rowIndex)
       vars:(cursor record)
	   (STARTTIME)
       (setq cursor (open tableName static: 0))
       (setq record (cursor.read rowIndex))
       (setq cursor (close cursor))
       (SETTIME)
       record) ;; end of readRecord


    ;; Converts a data mine table record into a record details HTML form.
    (defun recordDetails(tableCursor recordNumber)
        vars:(record htmlPage colVector i recLen)
	    (STARTTIME)
        (onError (lambda(err) (mid (browseLib.checkout "dataMineLib:NORECORDDETAILS") 9 1000000)))
        ;; Retrieve the record from the data mine.
        (setq record (tableCursor.read recordNumber))
        (setq colVector tableCursor.colVector)
        ;; Create an HTML record details page, and write
        ;; it to the disk, then return the record details
        ;; html page URL so the Web client will load it.
        (setq htmlPage (mid (browseLib.checkout "dataMineLib:RECORDDETAILSHEADER") 9 1000000))
        (setq recLen (length colVector))
        (loop for i from 0 until recLen do
            (setq htmlPage (append htmlPage 
                                   {<TR>} _eol
                                   {<TH WIDTH="25%" ALIGN="left"><FONT FACE="Arial"><B>}
                                   colVector[i] "</B></FONT></TH>" _eol
                                   {<TD WIDTH="75%" ALIGN="left"><FONT FACE="Arial">}
                                   (delimitedString record[i])
                                   "</FONT></TD></TR>" _eol))                                   
            ) ;; end loop
        (setq htmlPage (append htmlPage (mid (browseLib.checkout "dataMineLib:RECORDDETAILSTRAILER") 9 1000000)))
       (SETTIME)
        htmlPage) ;; end recordDetails

    ;; Converts a data mine table record into a record details HTML form.
    (defun recordDetailsURL(tableCursor recordNumber)
       vars:(htmlPage (url "AnswerDetails.htm"))
       (STARTTIME)
       (setq htmlPage (recordDetails tableCursor recordNumber))
       (browseLib.writeSourceFile (append _ais.httpDir url) htmlPage)
       (SETTIME)
       url) ;; end recordDetailsURL

    (defun rollback (extent) 
	;; *******************************************************************
	;; Rollback any active transactions on the specified extent
	;;	Args:
	;;		extent		Location name or Index of extent to rollback
	;; *******************************************************************
	vars:(repos extIndex i j)
    (STARTTIME)
	(if (isNumber extent) ; extent passed as a numeric index
		(setq extIndex (if (or (< extent 0) (> extent (- (length dmExt) 1))) false extent))
	else ; extent passed as a symbol into dmExt
		(setq extIndex (member dmExt[extent]))
	);if
	(if (= extIndex false)
		(error (append "Illegal Argument, dataMineLib:rollback - extent argument=" extent)))
		
	;; Close all open cursors in the extent being rolled back
	;; Note: It may be possible to be smarter about this and only delete some of the 
	;; open cursors. However, a fairly deep analysis would be required to nail down
	;; what is reasonable. 
	(loop for i from (- (length openTables) 1) to 0 do
		(if (= extIndex (getTableExtent openTables[i 0]))
			(begin
				(loop for j from (- (length openTables[i].cursorList) 1 to 0)  to 0 do
					;; It may be that the overhead associated with closing a disk
					;; cursor might to good to avoid. For now, since we do no
					;; write buffering, I assume the close overhead is minimal.
					;; The nocommit: option keeps the currently active transactions
					;; in the repository open until the rollbackTrans call below.
				 	(close openTables[i].cursorList[j] nocommit:)
				); j
			end)
		);
	); i 
	(__rollbackTrans extIndex)	
    (SETTIME)
	true)
    ;; Sets which extent new objects are created in.
    (defun usingForCreate(location databaseName)
	   (STARTTIME)
       (setq databaseName (symbol databaseName))
       (setq location (symbol location))
       (setq usingDatabase databaseName)
       (if (= dmExt[location] #void) (error "dataMineLib: invalid data mine extent"))
       (setq usingExt location) ;tm! 
       (setq usingExtIndex (member location dmExt))
       (setq usingRepos dmExt[location].repos)
       (SETTIME)
       true) ;;
       
    ;; Writes the specified row to the table in the data mine.
    (defun writeRecord(tableName rowIndex record)
       vars:(cursor)
	   (STARTTIME)
       (setq cursor (open tableName disk:))
       (cursor.write rowIndex record)
       (setq cursor (close cursor))
       (SETTIME)
       true) ;; end of writeRecord

    (defun __beginTrans (argRepo)
    ;; argRepo is either an index or a reference to a repository
    	vars:(repoIndex len i result)
	    ;
	    (STARTTIME)
    	(setq len (length dmExt))
    	(setq repoIndex -1)

  		(if (isNumber argRepo)
  			(setq repoIndex argRepo)
  			(begin
  			(loop for i from 0 until len do
  				(if (= argRepo dmExt[i].repos)
  					(begin
  					(setq repoIndex i)
  					(goto BREAK1:)
  					end))
  			);i
  			end))
        BREAK1::

		(if (= repoIndex -1) (error "__beginTrans: bad repository argument supplied"))
        
		(setq dmExt[repoIndex].transCount (add1 dmExt[repoIndex].transCount))
	   	(beginTransaction dmExt[repoIndex].repos)

;   		(writeln "beginTrans called on extent=" repoIndex " " dmExt[repoIndex].transCount)
	
		(if (not (inspect dmExt[repoIndex].repos open:)) (error "__beginTrans failed"))

		(setq result dmExt[repoIndex].transCount)
        (SETTIME)
		result);; end of beginTrans

    (defun __rollbackTrans (argRepo)
    ;; argRepo is either an index or a reference to a repository
    	vars:(repoIndex len i result)
		(STARTTIME)

    	(setq len (length dmExt))
    	(setq repoIndex -1)

  		(if (isNumber argRepo)
  			(setq repoIndex argRepo)
  			(begin
  			(loop for i from 0 until len do
  				(if (= argRepo dmExt[i].repos)
  					(begin
  					(setq repoIndex i)
  					(goto BREAK1:)
  					end))
  			);i
  			end))
        BREAK1::
		(if (= repoIndex -1) (error "__rollbackTrans: bad repository argument supplied"))


        (if (> dmExt[repoIndex].transCount 0) 
        	(begin
    		(abortTransaction dmExt[repoIndex].repos)
 ;  			(writeln "rollbackTrans called on extent=" repoIndex " " dmExt[repoIndex].transCount " open transactions rolled back" " TransOpen=" (inspect dmExt[repoIndex].repos open:))
    		(setq dmExt[repoIndex].transCount 0)
    		end)
    	else
    		(begin
 	    	(writeln "////////////////////////////Warning! rollbackTrans called with transCount[" repoIndex "] already 0.")
    		end))
    	(setq result dmExt[repoIndex].transCount)
        (SETTIME)
    	result);; end of rollbackTrans

    (defun __commitTrans (argRepo)
    ;; argRepo is either an index or a reference to a repository
    	vars:(repoIndex len i result)
    	(STARTTIME)

    	(setq len (length dmExt))
    	(setq repoIndex -1)

  		(if (isNumber argRepo)
  			(setq repoIndex argRepo)
  			(begin
  			(loop for i from 0 until len do
  				(if (= argRepo dmExt[i].repos)
  					(begin
  					(setq repoIndex i)
  					(goto BREAK1:)
  					end))
  			);i
  			end))
        BREAK1::

		(if (= repoIndex -1) (error "__commitTrans: bad repository argument supplied"))

    	(if (> dmExt[repoIndex].transCount 0)
    		(begin
    		(setq dmExt[repoIndex].transCount (sub1 dmExt[repoIndex].transCount))
    		(if (= dmExt[repoIndex].transCount 0)
 	    		(commitTransaction dmExt[repoIndex].repos))
;	    	(writeln "commitTrans called. transCount[" repoIndex "]=" dmExt[repoIndex].transCount  " TransOpen=" (inspect dmExt[repoIndex].repos open:))
 	    	end)
 	    else
 	    	(begin
 	    	(writeln "////////////////////////////Warning! commitTrans called with transCount[" repoIndex "] already 0.")
 	    	end))
 	    	
 	   	(if (and (= dmExt[repoIndex].transCount 0) (inspect dmExt[repoIndex].repos open:))
 	   		(error "__commitTrans failed"))
    	(setq result dmExt[repoIndex].transCount)
        (SETTIME)
    	result);;end of commitTrans
    	
    	
  ;; *******************************************************************
  ;; Initialize the dataMineLib
  ;; *******************************************************************
  ;; simple inits
  (STARTTIME)
  (setq _t (new Directory:))
  (setq _tc (new Directory:))
  
  (setq myParent (myself))
  (setq mySelf (myself))
  
  ;; Collect the first argument.
  	(if (>= (argCount) 1)
      	(setq dataMineExtents (argFetch 0))
  	else
		(begin
		(if (= _dataMineExtents #void)
			(error "Error, dataMineLib called for first time with no extents specified")
      		(setq dataMineExtents _dataMineExtents)  ;;tm! _dataMineExtents is a global. 
      	)
      	end)
  	) ; end if

  
  ;; If this is javaScript source, then parse it.
  (if (<> (type dataMineExtents) Structure:) (return (javaScript dataMineExtents)))
  
  ;;tm dataMineCompressSW is set so that the dataMineLib can be restarted without arguments
  (if (and (= (argCount) 2) (= (argFetch 1) compress:)) 
      (setq _dataMineCompressSW (setq compressSW true)) ;;tm _dataMineCompressSW is a global variable 
  );if
  (if (and (<> (argCount) 2) (= _dataMineCompressSW #void)) ;;tm if the global _dataMineCompressSW has not been set then
      then 
      (setq _dataMineCompressSW (setq compressSW false)) ;;tm assume no compression
      else
      (setq compressSW _dataMineCompressSW) ;;tm use current global compression setting for this dataMineLib
  ) ; end if
  (if (not (isBoolean compressSW)) 
      (setq _dataMineCompressSW (setq compressSW false))
  ) ; end if

  ;; Initialize the internal extent structure for each data mine extent. 
  ;;tm example of dmExt structure #{Reference: "Reference.db" Blackboard: "Backtst.db"}
  ;; Initialize the data mine and establish the multiple extent repositories.
  (setq _dataMineExtents (copy dataMineExtents))

  (setq dmExt (copy dataMineExtents))
  (if (<> (type dmExt) Structure:) (error "dataMineLib" "dataMineExtents not installed properly"))
  (setq openTables (new Dictionary:))
  (setq openBufs (new Dictionary:))
  (setq memoPad (new Directory:))
  
  	;;tm The following loop converts the simple dmExt structure to a more complex structure more useful as an internal
  	;;tm map of extents.
	(loop for extentIndex from 0 until (length dmExt) do

		;;tm Replace string with structure in dmExt
		(setq extentName dmExt[extentIndex]) 
		(setq dmExt[extentIndex] 
			(new Structure: 
			name: extentName 
			repos: (new ObjectRepository: extentName key: compressSW) 
			transCount: 0
			extentTableNames: #void
			extentDatabaseNames: #void
			))
		(if (= (length dmExt[extentIndex].repos) 0) ; getting length does a read on the repository
			(begin
		  	(abortTransaction dmExt[extentIndex].repos)  ;tm CYA in case a transaction got started somewhere else
		   	(clear dmExt[extentIndex].repos) ;tm CYA in case it is not really 0
		  	end)
		) ; end if
	) ;; end repository extent loop
	(__collectTableNames) ;tm! Collect all tables names

	;; Set default to first extent.
	(usingForCreate dmExt[0 0] Default:)
    (browseLib.addDataExtent "~~Datamine" (__tableExtentMgrTemplate "~~Datamine"))
    (SETTIME)
	true) ;; end of dataMineLib





;;**EXPORTKEY**:dataMineLib.NORECORDDETAILS
;#text#
<HTML>
<HEAD><TITLE>Data Mine Lambda: No Record Details</TITLE></HEAD>
<!--Page Parameters -->
<BODY BGCOLOR="#FFFFF0" TEXT="#000000" LINK="#0000ff">

<A NAME="topage"></A>

<FONT COLOR="#000080"><H1>No Record Details Available</H1></FONT>

<TABLE BORDER=3 CELLPADDING=2 WIDTH="100%" BGCOLOR="#99CCCC">
<TR><TH BGCOLOR="#99CC99"><FONT SIZE=+2 COLOR="#888800"></FONT></TH></TR>
<!--- Start Of Growth Portfolio Statistical Table Insert --->

<TR VALIGN="bottom">
<TD ALIGN="left"><FONT FACE="Arial"><B>
...</B></FONT></TD>
<TD ALIGN="left"><FONT FACE="Arial"><B>
...</B></FONT></TD>
</TR>

<!--- End Of Record Details Table Insert --->
</TABLE>


</BODY>
</HTML>











































































;;**EXPORTKEY**:dataMineLib.RECORDDETAILSHEADER
;#text#
<HTML>
<HEAD><TITLE>Data Mine Lambda: Record Details</TITLE></HEAD>
<!--Page Parameters -->
<BODY BGCOLOR="#FFFFF0" TEXT="#000000" LINK="#0000ff">

<A NAME="topage"></A>

<FONT COLOR="#000080"><H4>Record Details</H4></FONT>

<TABLE BORDER=3 CELLPADDING=2 WIDTH="100%" BGCOLOR="#99CCCC">
<TR><TH BGCOLOR="#99CC99"><FONT SIZE=+2 COLOR="#888800"></FONT></TH></TR>
<!--- Start Of Record Details Table Insert --->















































































;;**EXPORTKEY**:dataMineLib.RECORDDETAILSTRAILER
;#text#

<!--- End Of Record Details Table Insert --->
</TABLE>


<!--- Button which links to top of page --->
<P ALIGN="CENTER">
<INPUT TYPE='button' VALUE='Top of Page' onClick='scroll(0,0)'>
</P>

</BODY>
</HTML>














































































;;**EXPORTKEY**:dataMineLib.__collectTableNames
(defchild dataMineLib:__collectTableNames(...)
   ;; *******************************************************************
   ;; summary:  Collects the names of all tables currently in the data
   ;;           mine and places them in in-memory dictionaries. 
   ;;				dataMineLib.dataMineTables      dictionary
   ;;			    dataMineLib.databaseNames       vector
   ;; Args:     none.
   ;; Return:   true
   ;; *******************************************************************
	vars:(time extentIndex extentCount schemaIndex 
	 	schemaCount tableSchema tableName dbIndex compositeIndex reloadSW ext
	 	extentTableNames
	 	extentDatabaseNames
	 	)
	;; Initialize the internal extent structure for each data mine extent. 
	(STARTTIME)		
	(setq dataMineTables (new Dictionary:))
	(setq databaseNames (new Vector:))

	(setq reloadSW false)
	(if (> (argCount) 0)
		(if (= (argFetch 0) reload:) (setq reloadSW true)))
	
	; if first time called or force load specified reload from repositories
	(loop for extentIndex from 0 until (length dmExt) do
		;; Gather all table names in this data mine extents.

		; To avoid having to reload the table schemas for the extent each time _collectTableNames is called
		; we fill the extentTableNames and extentDatabaseNames vectors once. To force these two vectors to
		; be reloaded from the extents you pass the reload: argument to this routine.
		(if (or (= dmExt[extentIndex].extentTableNames #void) reloadSW)
			(begin ; reload extent index directory 
;			(writeln "reloading extent index directory")
			(__beginTrans extentIndex)
			(setq schemaCount (length dmExt[extentIndex].repos))
			(setq dmExt[extentIndex].extentTableNames (new Vector: schemaCount))
			(setq extentTableNames dmExt[extentIndex].extentTableNames)
			(setq dmExt[extentIndex].extentDatabaseNames (new Vector: schemaCount))
			(setq extentDatabaseNames dmExt[extentIndex].extentDatabaseNames) 
			(loop for schemaIndex from 0 until schemaCount do    
				(setq extentTableNames[schemaIndex] dmExt[extentIndex].repos[schemaIndex 0]) 
				(setq tableSchema dmExt[extentIndex].repos[schemaIndex 1]) 
				(setq extentDatabaseNames[schemaIndex] tableSchema.myDatabaseName)
			);schemaIndex
			(__commitTrans extentIndex)
			end)
		);if

		(setq extentTableNames dmExt[extentIndex].extentTableNames)
		(setq extentDatabaseNames dmExt[extentIndex].extentDatabaseNames)			
		(setq schemaCount (length extentTableNames))

		;(writeln "_collectTableNames 1=" (getTickCount time))
;		(setq time (getTickCount 0))
		(loop for schemaIndex from 0 until schemaCount do 
			(setq tableName extentTableNames[schemaIndex])
			(setq databaseName extentDatabaseNames[schemaIndex] )

			;;Append unique database name into databaseNames preserving original insert order
			(setq dbIndex (member databaseName databaseNames))
			(if (not (isNumber dbIndex)) (begin
				(setq databaseNames[(length databaseNames)] databaseName)
				(setq dbIndex (member databaseName databaseNames))
				end))

			;;Insert table into dataMineTables - note that later tables overwrite earlier tables of the same name!
			(setq compositeIndex (+ extentIndex (/ dbIndex 65536)))
			(setq dataMineTables[(symbol tableName)] compositeIndex) 
		 ) ; end schema loop
;		(writeln "_collectTableNames 2=" (getTickCount time))
	) ;; end repository extent loop  

	(if (= (length databaseNames) 0) (usingForCreate dmExt[0 0] Default:))
	(SETTIME)
	true) ;; end of __collectTableNames













































































;;**EXPORTKEY**:dataMineLib.__tableCursor
(deforphan dataMineLib:__tableCursor(argParent argTable argCursorType argColumns argBufSize argWithIndices argLoadByIndex argIndexName argIndexKeyVal argCreateEmpty)
;; *******************************************************************
;; Summary:  Data Mine table cursor which manages a relation in
;;           the data mine schema.
;; Notes:    Each element in the table is called a record.
;;           The index of a table record is called its row (rows begin with zero).
;; Depends:  dataMineLib
;; Arguments:                                    
;;			argParent			Parent Lambda of instance 
;;			argTable			The table to open the cursor on
;;			argCursorType		The type of cursor; disk:, static: or memory:
;;			argColumns			Select columns to load for memory cursor. Not implemented for disk: or static:
;;			argBufSize			Buffer size. Pass -1 for no buffering. Pass 0 for no change in buffer size.
;; Index management arguments
;;			argEnableIndices	True/False
;;			argWithIndices		True/False
;;			argLoadByIndex		True/False
;;			argIndexName		name if argLoadByIndex else #void
;;			argIndexKeyVal		key value to match or #void if load all keys
;;			argPartialKeyCompare	True if partial match on argKeyVal is desired. False for full match.
;; Misc. arguments
;;			argCreateEmpty		True/False Create the cursor without loading any data. Useful for creating empty memory cursors.
;;								Note: This is the only way you can open a memory cursor on a metatable!
;; Return:   self           This newly initialized table cursor.
;; *******************************************************************
;; 
; Notes on the difference between memory, disk and static cursors
; Memory cursors load the entire content of a table into memory, assigning each row object
; into an object vector pointed to by bckVector. The current view of the table is pointed to 
; by rowVector. If the current view is not filtered then rowVector points to the same object vector
; pointed to by bckVector. If the current view is filtered or sorted then a new object vector is created
; and rowVector points to it while bckVector continues to point to the original object vector. Note that
; both object vector's elements are pointers to the same row objects.
;
; Disk and static cursors opened on the same table share a common bckVector which points to a number vector
; that contains the frameIDs of the table row objects stored in the repository. The cursor's current view
; is pointed to by the cursor's rowVector which is a number vector containing indices into the shared bckVector.
; Reading a record by row from a static or disk cursors current view involves geting the bckVector index from the
; rowVector. Getting the frameID from the bckVector by that index and then reading the row from the repository using
; the frameID.
;
; Disk and static cursors on the same table may share a buffer. The buffer is implemented as a object vector with
; the same number of elements as the bckVector. When a row object is read by frameID, it's object location on the heap
; is stored in the buffer vector at the same index location as the bckVector. That is to say that the frameID in element
; n of bckVector is the record object pointed to by element n in the buffer vector.
;
; Note that the shared bckVector and buffer are part of the openTables vector in the dataMineLib.
;*************************************************************************
; Notes on the implmentation of indices
; 
;
;
;**********************************************************************

    ;; Persistent Variables
    pvars:(fileID              ;; The file id used during import and export.
    	   myObjectState	   ;; #void if Lambda instance is uninitilized, true if initialized, false if closed!
    	   						; This flag is check in every method to make sure only calls on a valid 
    	   						; initialized object are made.
    	   myDirtyFlag		   ;; Dirty flag
           myCursorNotes       ;; The notes Dictionary of this data mine table cursor.
           myCursorType        ;; The type of this data mine table cursor. disk: static: or memory:
           myExtIndex          ;; Index of my repository extent.
           myRepos             ;; Reference to myParent.Pv.dmExt[i] where i is the extent the table is stored in
           mySortLambda         ;; The current sort Lambda to use with a table sort.
           myParent			   ;; Reference to the calling dataMineLib
           mySelf			   ;; Reference to this Lambda. ie: (myself)
           showLimit           ;; The maximum number of records to display with the show function.
           openTables		   ;; Assigned from myParent.Pv.openTables -- see init
           myBufferUseSW	   ;; True if using buffering, false otherwise. Memory cursors are always false
           openBufs			   ;; Assigned from myParent.Pv.openBufs -- see init 
           myBuf			   ;; Assigned from myParent.Pv.openBufs[tablename] -- this tables buffer structure
           transStruct		   ;; structure containing transaction Lambdas to be passed to index objects on thier initialization

           myExportTabSW       ;; True iff we are to export tab delimited records.
           
           dbNumBufHits			;; Number of times a record was read from a buffer instead of from disk
           dbNumDiskHits		;; Number of times a record was read from disk instead of from memory
           (NEXTRECMASK 16777216) ;; mask for fractional part of real that stores nextRec in buffer list
 
           rowVectorSeed
           ;; Schema globals (see dataMineLib table schema).
           ;; Note: We load the schema into our Pv to speed record processing.
           mySchema            ;; The table object schema record.
            bckVectorKey       ;; The repository key of the backup vector (bckVector).
            colCount           ;; The number of columns in the table.
            colVector          ;; The table's vector of column names (used with setAttribute on recordStructure).
            myMemoPad          ;; The table's memo pad (always a Dictionary).
            myDatabaseName     ;; The table's logical database name (not the name of the table).
            myObjectName       ;; The name of the data mine table
            myObjectType       ;; The name of the data mine object type.
            myTableName        ;; NOT USED
            myTableDate        ;; NOT USED
            recordCount        ;; The table's current total record count.
            recordStructure    ;; The table's record structure used to create new records.
            rowVectorKey       ;; NOT USED
            validFields        ;; The table's valid fields and types. TYPES not yet implemented.
            viewDirectoryKey   ;; The repository key for the Directory of saved table record view vectors.
            indexDictionaryKey ;; The repository key for the Directory of saved indices.
;            primaryIndexKey    ;; 
            sjIndexKey   		;; The repository key for the starJoinIndex. Only used in a metaTable
            memberTableDirectoryKey	; The memberTableDirectory holds the list of tables that comprise the metaTable
            metaTable		   ;; Name of metaTable this table belongs to. #void if not a member table of a metaTable


           ;; In memory record vectors and block lists.
           rowVector           ;; The current record vector.
           bckVector           ;; The backup record vector.
           viewDirectory       ;; The the Directory of saved record vectors.
           bckVectorIndex	   ;; A directory keyed by frameID with values being the index into the bckVector for record having the frameID
           indexDictionary	   ;; A dictionary of saved index headers and index meta information managed by table
           bckVectorFrameID    ;; A vector of frameIDs that parallels the memory cursor's bckVector (memory cursor only)
           lastMemberTableSW		; Set on memberTable open if member table is the last table in the metaTable. 
           memberTableDirectory
           

           ;; MetaTable information built at open of metaTable
		   memberTableOffsets	;; Vector of offsets into the bckVector. Used by metaTable to index into bckVector for a given table
		   memberTableExtents	;; Vector of extent indices for member tables
		   uniqueIDColNum		;; index column for member tables rowKey 

		   ;; StarJoinIndex management variables
		   sjIndex				;; Structure used to collect and save star join index information to the repository
		   sjIndexBlockSize		;; maximum number of records in a member table
		   sjBckVectorIndex		;; index into back vector -- loaded as part of sjIndex and then assigned to this variable for fast access
		   sjUniqueIDs			;; dictionary of rowID/bckVectorOffsets -- loaded as part of sjIndex and then assigned to this variable for fast access
		   sjRowKeyCol 			;; name of the row key column
           
           ;; Index management variables (these are set according to index management arguments passed on initialization of Lambda)
           enableIndicesSW
           withIndicesSW
           loadByIndexSW
           indexNameSW
           indexKeyVal
           indicesDirtySW		;; flag is set to true if table modified while indices are disabled (disk cursor only)
           partialKeyCompareSW
           (fakeFrameID -1.0)

			;; Misc. timers
			(wrtTime0	0)		;; actual time of writes

           ;; Public Child Lambdas
           appendToMetaTable	;; Append a new member table to the current meta table
           average             ;; Averages a lambda value on each record of a table.
           averageForAll       ;; Averages a lambda value on ALL records of a table.
           createIndex		   ;; Create an index on table
           createMemoryCursor  ;; Create a memory cursor from the current view
           delete              ;; Delete a table row (specified by row number).
           deleteIndex		   ;; Delete an index from table
           delimitedCells      ;; Returns a block of cells as a tab delimited string.
           destroyTable		   ;; Completly remove table content from repository and invalidate cursor
           deviation           ;; Returns the standard deviation of a lambda value on each record of a table.
           disableIndices	   ;; disable index processing on the cursor's indices
           drop                ;; Drop all records from a table object.
           dropView            ;; Drop the specified record view.
           enableIndices	   ;; enable index processing on the cursor's indices.
           export              ;; Exports ascii tab or comma delimited records from the table.
		   exportCsv		   ;; Exports ascii comma delimited records from the table.
           exportTab           ;; Exports ascii tab delimited records from the table.
           flushBuffer         ;; Flushes row buffer.
           getBestIndex		   ;; return the name of the best index to use to search on a list of specified columns
           getColumnHeaders    ;; Returns the table headers as a tab delimited string.
           getNewRecord        ;; Returns a blank new record.
		   getRecordsByKey	   ;; find and return one or more records in the table by key
		   getRecordBySJKey		; find and return a record using the star join index
		   getRecordsRowByKey   ;; find and return or more record rows in the table by key
		   getRecordRowBYSJKey	; find and return a records row in the table using the star join index 
           import              ;; Imports ascii tab or comma delimited records into the table.
           importCsv           ;; Imports ascii comma delimited records into the table.
           importTab           ;; Imports ascii tab delimited records into the table.
           indexView		   ;; reorder and filter the current view to match the order and content of the specified index and key
           indexViewByTableKey ;; calls index view 
           indexViewByRowKey   ;; calls index view 
           insert              ;; Insert a table row.
           isView              ;; Returns true iff the specified key is a saved view of this cursor.
           maximum             ;; Returns the max of a lambda value on each record of a table.
           minimum             ;; Returns the min of a lambda value on each record of a table.
           omit                ;; Deletes those records from a table for which a lambda predicate is true.
           read                ;; Read a table row and return a copy of the record.
           refExport           ;; Returns a record, for export, to the exportTab function.
           refImport           ;; Returns an empty record to the importTab function.
           reset               ;; Resets the backup copy and views of the data mine table.
           restore             ;; Restores the backup copy of the data mine table.
           restoreView         ;; Restore the specified record view.
           run                 ;; Run the specified javaScript Lambda against this cursor.
           save                ;; Save the contents of the table cursor back to disk.
           saveView            ;; Save the current record view for later retrieval.
           search              ;; Returns the row index of the first record for which a lambda predicate is true.
           setImport           ;; Receives a single record from the importTab function.
           sharpe              ;; Returns the sharpe ratio of a lambda value on each record of a table.
           show                ;; Shows a group of records starting from the specified row.
           sort                ;; Sort the table rows using a lambda sort function.
           tile          	   ;; Truncates a table to those records which are in the nth of N tiles.
           total               ;; Totals a lambda value on each record of a table.
           totalForAll         ;; Totals a lambda value on ALL records of a table.
           truncate            ;; Truncates a table to those records for which a lambda predicate is true.
           updateView          ;; Updates each record in the current view and restores the altered backup view.
           viewMath            ;; Combines two saved views using a mathematical operator.
           write               ;; Write a table row, perhaps previously read, for update.
           writeBySJKey			; Write by star join index key. 
           ;; Private Child Lambdas
           __clear             ;; Clears the memory cursor.
           __close             ;; Terminate an update transaction on the table object.
           __clearBuffer		;; Clear the record buffer  
           __delete             ;; internal guts of delete
           __deleteFromIndices  ;;
           __disableIndices  	;; internal guts of disableIndices
           __error				;; general error messaging routine
           __errorStop         ;; Handles error conditions during sensitive operations.
           __enableIndices		;; internal guts of enableIndices
           __getEnableIndicesSW   ;; Return current value of enableIndicesSW
           __getIndexDirtySW	;; Return current value of indexDirtySW
           __getRecordRepos		;; Return the repository for a given record in meta table bckVector
           __indexContent	   ;; Index content
           __indexView 			;; internal guts of indexView
           __insertIntoIndices ;; 
           __loadIndices		;; load disk or static cursor indices
           __makeRowVector	   ;; Make the rowVector for disk: and static: cursors
           __makeFakeFrameID	; make a fake frameID for memory cursor indexing
           __read			   ;; Read and return a reference to a buffered record.
           __reset				;; internal guts of reset
           __restore			;;internal guts of restore
           __setEnableIndicesSW ; Set the enableIndicesSW
           __setIndexDirtySW	; Set the indexDirtySW 
           __sortWrapper       ;; Wrapper for sort Lambda during sort of a table. 
           __syncUp            ;; Synchronizes all open static cursors of this data mine table.
           __total				;;internal guts of total
           __totalForAll		;; internal guts of totalForAll
           __truncateEnds      ;; Truncate records from both ends of a table.
           __truncateMid       ;; Truncate records from the center of a table.
           __write			 	; internal write. write wrappers __write
           __updateBckVectorIndex ;; Update the bckVectorIndex directory
           __updateMetaTable    ;; read each member table and update the current meta table
           __buildBckVectorIndex ;; Build the bckVectorIndex
           __updateKill        ;; Terminate an update transaction and delete the table object.
           __updateIndices	   ;; Iterate indices updating as necessary.
           __errorState		   ;; Function to handle errors related to uninitialized object
           
          ) ;; end of persistent variables
	vars: (i j k m t x o
			cursorList
			endpos
			memberTableExtent
			memberTableSchema
			memberTableRepos
			memberTableBckVector
			metaTableRepos
			metaTableSchema
			metaTableExtent
			memberExtentsOpened
			numRecs
			numTables
		 	obj 
		 	record 
		 	rec
		 	savedColVector
		 	tempMemberTableDirectory
			totRecordCount
		 	vec 
		 	)

	;; Buffer implementation Notes
    ;; Table buffers are implmented partially in dataMineLib and partially in __Cursor for 
    ;; speed. Each table has zero or 1 buffers in the dataMineLib.openBufs dictionary.
    ;; See the dataMineLib.__openBuffer routine for the structure of the buffer.
    ;;
    ;; Initialize the inline child Lambdas.


;**************** average *******************
    (defun average(valueLambda)
    ;; *******************************************************************
    ;; Summary:  Averages a lambda value on each record of rowVector
    ;; *******************************************************************
       vars:(result)
        (STARTTIME posttime:)
       (if (not myObjectState) (__errorState)); make sure object has been initialized
       (setq result (__total valueLambda))
       (if (isPositive recordCount) 
           (/= result recordCount) 	
           (setq result #void)
           ) ; end if
       (SETTIME)
       result) ; end average

;************* averageForAll *************************
    (defun averageForAll(valueLambda)
    ;; *******************************************************************
    ;; Summary:  Averages a lambda value on ALL records of rowVector
    ;; *******************************************************************
       vars:(result)
       (STARTTIME posttime:) 
       (if (not myObjectState) (__errorState)); make sure object has been initialized
       (setq result (__totalForAll valueLambda))
       (if (isPositive (length bckVector)) 
           (/= result (length bckVector))
           (setq result #void)
           ) ; end if
       (SETTIME)
       result) ; end averageForAll

;************* createIndex **********************
    (defun createIndex (argIndexName argKeySpec ...)
    ;; Use the createIndex function to add a new index to a table.
    vars:(i errorString len colName uniqueSW indexStruct initArgs s arg)
    (STARTTIME posttime:)
	(if (not (__getEnableIndicesSW)) 
		(__enableIndices force:))
	(if (<> myCursorType memory:)
		(begin
	
		(if (<> metaTable #void)
			(__error (append ":createIndex " myObjectName 
							" :An attempt was made to create an index on a member table.")))
	
		(if (= myObjectType metaTable:)
			(__error (append ":createIndex " myObjectName
						" :An attempt was made to create an index on a meta table.")))
		end)
	);if
	
    ;;Make sure an index named argIndexName does not aready exist
    (setq argIndexName (symbol argIndexName))
    (if (isMember argIndexName indexDictionary)
    	(__error (append "createIndex: Attempt to create index that already exists:" argIndexName)))
    
    ;;Make sure the key spec is valid
    ;; Note: We are going to restrict the column specifications to Ascending until we get the ability to pass key compare args to the index object!
    (setq len (length argKeySpec))
    (if (isStructure argKeySpec)
    	(begin
	    (if (= len 0) (__error "createIndex: No keys defined in key specification"))
	    (loop for i from 0 until (length argKeySpec) do
	    	(if (not (isMember argKeySpec[i 0]  colVector))
	    		(__error (append "createIndex: Invalid column name: " argKeySpec[i 0])))
			(if (<> argKeySpec[i 1] A:)
				(__error "createIndex: Invalid sort specification. Only A: is valid at this time."))
	    );i
		end)
	);if
		
	;;Set defaults for optional arguments
	(setq uniqueSW false)

	;; Check for valid optional arguments
	(setq len (argCount))
	(loop for i from 2 until len do
		(setq arg (argFetch i))
		(cond
		((= arg unique:) (setq uniqueSW true))
		(true (__error "createIndex: Too many or bad arguments passed."))
		);cond
	);i
	
	;; Create the new index entry in indexDictionary
	(setq indexDictionary[argIndexName] 
		(new Structure:
		index:		#void
		frameID:	#void
		keySpec:	argKeySpec
		numCols:    #void
		getKey:		#void
		keyCompare:	#void
		unique:		uniqueSW
		state:		OutOfDate:
		noUpdating:	false			;only used in metaTable. See createMetaTableByIndex Lambda.
		))

	; assign local reference to indexStruct for fast local access
	(setq indexStruct indexDictionary[argIndexName])

	;; create getKey function
	;; Note: keySpec is a specification of the columns in the table to index. getKey is an Lambda 
	;; created to extract the appropriate columns from the record. If keySpec is a string then it is expected
	;; to be lamda expression and getKey is simply the result of an eval on keySpec. If keySpec is a structure then this routine
	;; is responsible for creating a lamda expression from keySpec.
	(if (isString argKeySpec)
		(setq indexStruct.getKey (eval (compile (morph (lisp argKeySpec)))))
	else
		(begin
	    (setq len (length argKeySpec))
	    
	    (if (= 1 len)
	    	(begin
	    	(setq s (append "(lambda(rec) ( (return rec." argKeySpec[0 0] " )))"))
	    	(setq indexStruct.getKey (eval (compile (morph (lisp s)))))
	    	(setq indexStruct.numCols 1)
	    	end)
	    else
	    	(begin
			(setq s (append (char 40) "lambda(rec) " (char 40) "new Vector: " len))
		    (loop for i from 0 until (length argKeySpec) do
		    	(setq s (append s " rec." argKeySpec[i 0]))
		    );i
		    (setq s (append s (char 41) (char 41))) 
			(setq indexStruct.getKey  (eval (compile (morph (lisp s)))))
			(setq indexStruct.numCols (length argKeySpec))
			end)
		end)
		);if
	);if

	
	(setq initArgs (new Vector: 6
		index 
		#void 
		myRepos 
		create:
		indexStruct.numCols
		transStruct
		))

	;; determine remaining arguments for init
	(if indexStruct.unique (setq initArgs[(length initArgs)] unique:))
	(if (= myCursorType memory:) 
		(begin
		(setq initArgs[(length initArgs)]  snapshot:)
		(setq initArgs[(length initArgs)] memory:)
		end)
	);if


;(writeln "getKey =" s)	    

;(writeln "************** initArgs=" initArgs)
    ;; initialize index

	(setq lenargs (length initArgs))
;	(cond
;		((= lenargs 6) (setq indexStruct.index (new initArgs[0] initArgs[1] initArgs[2] initArgs[3] initArgs[4] initArgs[5])))
;		((= lenargs 8) (setq indexStruct.index (new initArgs[0] initArgs[1] initArgs[2] initArgs[3] initArgs[4] initArgs[5] initArgs[6] initArgs[7])))
;		(true (error "__tableCursor.createIndex -- bad number of args in (new ...) call"))
;	)
	(setq indexStruct.index (apply new initArgs))
; This seems to have stopped working in version 4.x of the smtbase engine
	;(apply indexStruct.index.Pv.init initArgs)
	
	;; insert index content if indices are enabled
	(setq indexStruct.state OutOfDate:); flag index as requiring update
	(if (__getEnableIndicesSW) 
		(begin
		(__indexContent)
		(setq myDirtyFlag true)
		end)
	);if

    (SETTIME)
    true)



;************ delete *********************
	(defun delete(row)
	vars:(result)


	(STARTTIME posttime:)

	(setq result (__delete row))

    (SETTIME)
    result)		
			
    (defun __delete(row)
	;; *******************************************************************
	;; Summary:  Delete a table row (specified by row number).
	;; Args:     row      Row number of the record to be deleted.
	;; Return:   true 
	;; *******************************************************************
	vars:(len i j k bufferRow nextRec ptrs prevRec value bckRecordCount)
;   (STARTTIME posttime:) 	

    (if (<> myCursortype memory:)
    	(begin
		(if (= myObjectType metaTable:)
			(__error (append ":__delete " myObjectName 
							" :An attempt was made to call delete on a metaTable cursor")))

		(if (and (<> metaTable #void) (not lastMemberTableSW))
			(__error (append ":__delete " myObjectName 
							" :An attempt was made to call delete on a metaTable member other than last member.")))
		end)
	);if

	(if (not myObjectState) (__errorState)); make sure object has been initialized
	(setq myDirtyFlag true)

	
	;; Make sure we are not a static cursor
	(if (= myCursorType static:)
		(__error (append ":__delete " myObjectName 
						" :An attempt was made to delete a record on a static cursor")))
	
	(setq bckRecordCount (length bckVector))

	;; Make sure the specified row number is valid.
	(if (or (< row 0) (>= row bckRecordCount))
	   (__error (append  ":delete " myObjectName " - " row ":An attempt was made to delete with a bad row number.")))


	;; delete from data structures
	(if (= myCursorType disk:)
		(begin ; handle disk cursor 
		(setq value (refNumVector bckVector row))
		(if myBuf.enableIndicesSW
			(__deleteFromIndices value (__read row bckVector:))
			(setq myBuf.indicesDirtySW true));if
		
		(setq myRepos[frame: value] #void)
		(__clearBuffer); 
		(^delete bckVector row)
		(setq recordCount (length bckVector))

		(if myBuf.enableIndicesSW
			(begin
			(^delete bckVectorIndex (member value bckVectorIndex))

			(setq len (length bckVectorIndex))
			(loop for i from 0 until len do
				(setq j (refDirValue bckVectorIndex i))
				(if (> j row) 
					(setDirValue bckVectorIndex i (isub j 1)))
			); i

			;update bckVectorIndex for each row higher than the deleted row
;			(loop for i from row until recordCount do
;				(setq bckVectorIndex[bckVector[i]] i)
;			);i
			end)
		);if

		(resize myBuf.buffer recordCount)

		(__reset)
		(__syncUp) ;; synchronize all open static cursors of this table
		end)
	else ; handle memory cursor
		(begin
		(if enableIndicesSW
			(begin
			(setq value (refNumVector bckVectorFrameID row))
			(^delete bckVectorFrameID row)
			(__deleteFromIndices value (__read row))
			end)
		else
			(setq indicesDirtySW true)
		);if

		(^delete bckVector row)
		(setq recordCount (length bckVector))

		(if enableIndicesSW
			(begin
			(^delete bckVectorIndex (member value bckVectorIndex))

			(setq len (length bckVectorIndex))
			(loop for i from 0 until len do
				(setq j (refDirValue bckVectorIndex i))
				(if (> j row) 
					(setDirValue bckVectorIndex i (isub j 1)))
			); i

;   			(loop for i from row until recordCount do
;   				(setq bckVectorIndex[bckVectorFrameID[i]] i)
;   			);
   			end)
   		);if

		(__reset)

		end)
	);if
;	(SETTIME)
	true) ;; end of delete

;************ deleteIndex ************************
    (defun deleteIndex (argIndexName)
    ;; Use the deleteIndex function to delete an existing index from the table. 
    vars:(i index)

    (STARTTIME posttime:)

    (setq i (member argIndexName indexDictionary))
;    (if (= i false) (__error (append "dataMineLib.__tableCursor.deleteIndex: bad index name '" argIndexName "' passed")))
	(if (<> i false)
		(begin
	    (setq index indexDictionary[argIndexName])
	    (index.index.Pv.drop); drop the index
	    (^delete indexDictionary argIndexName)
		end)
	);if
    (SETTIME)
    true)

;*********** delimitedCells ********************
    (defun delimitedCells(startRow endRow startCol endCol)
    ;; *******************************************************************
    ;; Summary:  Returns a block of cells as a tab delimited string.
    ;; *******************************************************************
       vars:(rowIndex colIndex dls tab cellValue record)
       

       (STARTTIME posttime:)
        
       ;; Return a tab delimited string of cell values for
       ;; all cells between the following rows and columns.
       (if (not myObjectState) (__errorState)); make sure object has been initialized
       (setq dls "")
       (setq tab (append "" (char 09)))
       (if (> startRow endRow) (begin (SETTIME) (return dls)))
       (loop for rowIndex from startRow to endRow do
          (if (< rowIndex recordCount)
              (setq record (__read rowIndex))
              (setq record #void)
              ) ; end if
          (loop for colIndex from startCol to endCol do
      	  	  (setq cellValue record[colIndex]);tm Note that even if we get a reference to an object, somewhere in the next few lines
      	  	  									; it will become a copy of the object and the original will not be modified!
              (if (isString cellValue) (setq cellValue (substitute cellValue tab " ")));tm substitute returns a modified copy of string object!
              (if (= cellValue #void) (setq cellValue "")); immediate type so no change to record[colIndex]
              (if (isVector cellValue) (setq cellValue (string cellValue true)));tm expands the vector to a string of values
              (setq cellValue (append "" cellValue));tm Force conversion of cellValue to string. 
              (setq cellValue (substitute cellValue (char 09)  " ")); get rid of special tabs
              (setq cellValue (substitute cellValue (char 13)  " ")); get rid of returns
              (if (<> colIndex endCol) 
                  (setq dls (append dls cellValue (char 09))); separate each column of data with a tab
                  (setq dls (append dls cellValue))
                  ) ; end if
              ) ;;end column loop
          (if (<> rowIndex endRow) 
              (setq dls (append dls (char 13))); end each record with a return
              ) ; end if
          ) ;;end row loop
          

	   (SETTIME)
       (append dls "")) ;; end of delimitedCells

;*********** destroyTable ****************
    (defun destroyTable ()
    ;; **************************************************************
    ;; Summary: Completly remove table content from repository
    ;; Returns: #void
    ;; **************************************************************
	vars:(i j numTables len bckVectorCount indexLambda cursorIndex
		memberTableSchema
		memberTableExtent
		memberTableRepos
		memberExtentsClosed
		)

   	(if (not myObjectState) (__errorState)); make sure object has been initialized

	(if (<> metaTable #void)
		(__error (append ":destroyTable " myObjectName " :An attempt was made to destroy a member table.")))

	(STARTTIME posttime:)

	;; Make sure we are not a static or memory cursor
	(if (<> myCursorType disk:)
		(__error (append ":destroyTable " myObjectName " :An attempt was made to destroy a table on a non-disk cursor")))

	;; Test to see if static cursors exist
  	(if (> (length openTables[myObjectName].cursorList) 1)
		(__error (append ":destroyTable " myObjectName " :An attempt was made to destroy a table when other cursors exist")))

	;drop all indices
	(setq len (length indexDictionary))
	(loop for i from 0 until len do
		(setq indexLambda indexDictionary[i].index)
		(if (<> indexLambda #void)
			(indexDictionary[i].index.Pv.drop))
	);i
	(resize indexDictionary 0)

	;erase indexDictionary from repository
	(if (<> indexDictionaryKey #void) (setq myRepos[frame: indexDictionaryKey] #void))
	
	;erase all records (if not metaTable)
	(setq bckVectorCount (length bckVector))
	(if (<> myObjectType metaTable:)
	  	(loop for i from 0 until bckVectorCount do
	      	(setq myRepos[frame: (refNumVector bckVector i)] #void);; delete table row objects
	   	) ;i
	 );if


	(if (= myObjectType metaTable:)
		(begin 
		(setq len (length memberTableDirectory))
		(loop for i from 0 until len do
			(myParent.__beginTrans memberTableExtents[i])
			(setq memberTableRepos myParent.dmExt[memberTableExtents[i]].repos)
			(setq memberTableSchema memberTableRepos[memberTableDirectory[i 1].tableName])
			(setq memberTableSchema.metaTable #void)
			(setq memberTableRepos[memberTableDirectory[i 1].tableName] memberTableSchema) 
			(myParent.__commitTrans memberTableExtents[i])
		);i 



		;close transactions on repositores member tables reside in
		; This is necessary because we created transactions on these extents when we opened
		; the meta table for performance reasons.
		(setq numTables (length memberTableDirectory))
		(setq memberExtentsClosed (new Directory:))
		(loop for j from 0 until numTables do
			(setq memberTableExtent (myParent.getTableExtent memberTableDirectory[j 1].tableName))
			(if (and (not (isMember memberTableExtent memberExtentsClosed)) (<> memberTableExtent myExtIndex))
				(begin
				(myParent.__commitTrans memberTableExtent)
				(setq memberExtentsClosed[memberTableExtent] true)
				end)
			);if
		);j

		
		end)
	);if


	;Erase memberTableDirectory if one exists
	(if (<> memberTableDirectoryKey #void) (setq myRepos[frame: memberTableDirectoryKey] #void))

	;erase star join index if one exists
	(if (<> sjIndexKey #void) (setq myRepos[frame: sjIndexKey] #void))
	
	;erase bckVector from respository
	(if (<> bckVectorKey #void) (setq myRepos[frame: bckVectorKey] #void))

	;erase viewdirectory from respository
	(if (<> viewDirectoryKey #void) (setq myRepos[frame: viewDirectoryKey] #void))
	
	;erase table schema from repository
	(setq myRepos[myObjectName] #void)  

	; remove table entry from openBufs
	(^delete openBufs myObjectName)
	(setq myBuf #void)

	; commit the open transaction on the disk cursor
	(myParent.__commitTrans myExtIndex)

	;; Clear the cursor object.
	(setq cursorIndex (inside mySelf openTables[myObjectName].cursorList))
	(setq openTables[myObjectName].cursorList (^delete openTables[myObjectName].cursorList cursorIndex))
    
	(setq openTables[myObjectName] #void)
    (^delete myParent.dataMineTables myObjectName)
    (setq i (member myObjectName myParent.dmExt[myExtIndex].extentTableNames))
    (^delete myParent.dmExt[myExtIndex].extentTableNames i)
    (^delete myParent.dmExt[myExtIndex].extentDatabaseNames i)

	(__clear)

    (setq myObjectState false); invalidate object
    

    (SETTIME)
    
    #void)


;*********** deviation *******************
    (defun deviation(totalLambda)
    ;; *******************************************************************
    ;; Summary:  Returns the standard deviation of a lambda value on each 
    ;;           record of a table.
    ;; *******************************************************************
       vars:(rowIndex (result 0) score aSum aSsq anAvg)


		(STARTTIME posttime:)

       (if (not myObjectState) (__errorState)); make sure object has been initialized
       (if (<= recordCount 0) (begin (SETTIME) (return 0)))
       ;; Compute the standard deviation lambda value for each record in the table.
       (loop for rowIndex from 0 until recordCount do
           (setq score (totalLambda (__read rowIndex)))
           (setq aSum (+ aSum score))
           (setq aSsq (+ aSsq (* score score)))
           ) ;; end loop
       (setq anAvg (/ aSum recordCount))
       (setq result (sqrt (- (/ aSsq recordCount) (* anAvg anAvg))))


	   (SETTIME)
       result) ;; end deviation


;********** disableIndices *****************
    (defun disableIndices()
    (STARTTIME posttime:)

    (__disableIndices)

  	(SETTIME)
    true)

    (defun __disableIndices()
    ; Use the disableIndices function to disable index processing on the cursor.
	(__setEnableIndicesSW false)
    true)
	
;************ drop *******************
    (defun drop()
    ;; *******************************************************************
    ;; Summary:  Drops all records from the table.
    ;; *******************************************************************
       	vars:(row i bckRecordCount)
       	

       	(STARTTIME posttime:)
       	
       	(if (not myObjectState) (__errorState)); make sure object has been initialized
       	(setq myDirtyFlag true)
		;; Make sure we are not a static cursor
		(if (= myCursorType static:)
			(__error (append "drop " myObjectName " :An attempt was made to drop records on a static cursor")))

		(if (<> myCursorType memory:)
			(begin

			(if (= myObjectType metaTable:)
				(__error (append "drop " myObjectName " :An attempt was made to drop records on a metaTable")))
	
			(if (and (<> metaTable #void) (not lastMemberTableSW))
				(__error (append "drop " myObjectName
							" :An attempt was made to drop records from a member table that was not the last member table.")))
			end)
		);if

		(setq bckRecordCount (length bckVector))
		
       ;; Erase all records from the disk.
 	   (if (= myCursorType disk:)
 	   		(begin
			(if (<> myObjectType metaTable:)
			  	(loop for row from 0 until bckRecordCount do
			      	(setq myRepos[frame: (refNumVector bckVector row)] #void);; delete table row objects
			   	) ; end loop
			 );end of if
          	(setq recordCount 0)
			(setq bckVectorKey (setq myRepos[frame: bckVectorKey] #void))
			(setq viewDirectoryKey (setq myRepos[frame: viewDirectoryKey] #void))
            (resize bckVector 0)
            (if myBuf.enableIndicesSW 
            	(resize bckVectorIndex 0))
			(resize rowVector 0) ;                     
			(resize myBuf.buffer 0); 
			(setq myBuf.bufUsed 0);
			(setq myBuf.numBufRecs 0);
 	   		(setq myBuf.indicesDirtySW (not myBuf.enableIndicesSW))
			(__reset)			
			(if myBuf.enableIndicesSW
				(__indexContent all:)); re-index current indices
			(__syncUp) ; sync all static cursors
			end)
		else ; memory: cursor
			(begin
			(__reset)
			(__clear)
          	(setq recordCount 0)
            (if enableIndicesSW
            	(begin
	            (resize bckVectorIndex 0)
            	(resize bckVectorFrameID 0)
				(__indexContent all:); re-index current indices
				end)
			else
				(setq indicesDirtySW true)
			);if
			end)
		);if  
		

	   (SETTIME)
		true)

;************ dropView ****************	
    (defun dropView(key)
    ;; *******************************************************************
    ;; Summary: Drops the current record view as specified.
    ;; *******************************************************************
    ;;tm The following steps are necessary to circumvent a bug-- Mike to check if the bug is fixed

		(STARTTIME posttime:)
		(if (not myObjectState) (__errorState)); make sure object has been initialized
		(setq myDirtyFlag true)
		(setq viewDirectory[key] 1) 
		(^delete viewDirectory key)
		
		(SETTIME)
		true) ;; end of dropView

;************ enableIndices ******************

	(defun enableIndices (...)
		vars: (i len args result)

		(STARTTIME posttime:)      
	
		(setq len (argCount))
		(setq args (new Vector: len))
		(loop for i from 0 until len do
			(setq args[i] (argFetch i))
		);i
		(setq result (apply __enableIndices args))
	
	   (SETTIME)
		result); end of enableIndices


    (defun __enableIndices (...); internal with no timer
    ;;Use the enableIndices function to enable index processing on the cursor after a disableIndices. enableIndices will
    ;; force a reindexing of all indices defined on the cursor if the reindex: optional argument is supplied. reindexing 
    ;; will also occur if the table has been updated since the indices were disabled or if the table is a memory cursor.

	vars:(i len value arg forceSW reindexSW)

	;; Make the current view the same as the bckVector.
	(__reset)	; For memory cursors this means that rowVector and bckVector point to the same object vector
			; For disk cursors this means that __makeRowVector will be called

	;set default values for optional arguments
	(setq reindexSW false)
	(setq len (argCount)) 
	(loop for i from 0 until len do
		(setq arg (argFetch i))
		(cond
			( (= arg reindex:) (setq reindexSW true))
			( (= arg force:) (setq forceSW true))
			(true (__error "enableIndices: bad argument passed"))
		);cond
	);i

	(cond
		((= myCursorType memory:)
			;rebuild bckVectorIndex and bckVectorFrameID
			; Note that these objects are local to this Lambda and creating new objects here
			; will not affect other cursors
			(setq enableIndicesSW (or (> (length indexDictionary) 0) forceSW))
			(if enableIndicesSW
				(begin
				(if (= indexDictionary #void) (setq indexDictionary (new Dictionary:)))
				(__buildBckVectorIndex)
				(__indexContent all:) ; must reindex all because of new values in bckVectorIndex
				end)
			);if
		);memory

		((= myCursorType disk:)
			(setq myBuf.enableIndicesSW (or (> (length myBuf.indexDictionary) 0) forceSW))
			(if myBuf.enableIndicesSW
				(begin
				(__buildBckVectorIndex); must build instead of update because of possible insertions or deletions
				(__loadIndices); load any unloaded indices
				(if reindexSW 
					(__indexContent all:) ;force reindexing of all indices
					(__indexContent))	  ; reindex only if individual index is marked OutOfDate:
				end)
			);if
		);disk

		((= myCursorType static:)
			(setq myBuf.enableIndicesSW (or (> (length myBuf.indexDictionary) 0) forceSW))
			(if myBuf.enableIndicesSW
				(begin
				(__buildBckVectorIndex); this is not always necessary because some other cursor may have enabled indices
									   ; we may want to figure out a way to know this and build the bckVectorIndex conditionally!
				(__loadIndices); load any unloaded indices
				(__indexContent) ;Note that we let indexContent decide which indices to re-index			
				; The rational for this is that it is only the disk cursor that can update the 
				; underlying records in the table. So we must rely on the disk cursor to 
				; update the state flag on the indices so that indexing will occur if it is needed.
				end)
			);if
		);static
	);cond

    true)


;************* export *******************
	(defun export(aFilename ...)
	;; **********************************************************************
    ;; Summary:  Exports a comma or tab delimited file from the table cursor.
	;; **********************************************************************
		regs:(n N)
		vars:(aExtension _ExportType)
		(if (> (argCount) 1) 
            (begin ;; Check for export type specifier			  
			  (setq _ExportType (argFetch 1))
			  (if (and (<> _ExportType csv:) (<> _ExportType tab:)) (begin
	       		  (__error (append "export: " myObjectName " :bad export type: " _ExportType))))
		    ) else 
            (begin ;; guess at export type based on file extension
			  (setq n (- (length aFilename) 1))
			  (while (and (> n 1) (<> (mid aFilename n 1) ".")) (-- n))
			  (if (= n 0) ;; use default as no extension was found
				  (setq _ExportType exportTab)
			      else 
                 (begin
				   (setq aExtension (mid aFilename n 10))
				   (cond
				    ((= aExtension ".csv") (setq _ExportType exportCsv))
				    ((= aExtension ".tab") (setq _ExportType exportTab))
				    ((= aExtension ".txt") (setq _ExportType exportTab))
				    ((= aExtension ".xls") (setq _ExportType exportTab))
				    (else (setq _ExportType exportTab))
			    	) ; end cond
			     )) ; end inner if
		     )) ; end outter if

		;; Invoke the appropriate export method.
		(_ExportType aFilename)) ; end export

;*********** exportCsv ********************
    (defun exportCsv(fileName)
    ;; *******************************************************************
    ;; Summary:  Exports ascii comma delimited records from the table.
    ;; *******************************************************************
		(STARTTIME posttime:)

       (if (not myObjectState) (__errorState)); make sure object has been initialized

		(if (and (= myObjectType metaTable) (<> myObjectType memory:))
			(__error (append "exportCsv: " myObjectName " :Attempt to export meta table")))

       ;; Make sure we catch all errors which might occur.
       (onError __errorStop)
       ;; Open the specified .csv file and export.
       (setq myExportTabSW false)
       (setq fileID (fileOpen fileName 1 0)) 
       (|Gv:exportTab| fileID mySelf recordsOnly:)
       (fileClose fileID 1)
       (setq fileID #void)
     

	   (SETTIME)
       recordCount) ;; end exportCsv

;*********** exportTab ********************
    (defun exportTab(fileName)
    ;; *******************************************************************
    ;; Summary:  Exports ascii tab delimited records from the table.
    ;; *******************************************************************
		(STARTTIME posttime:)

       (if (not myObjectState) (__errorState)); make sure object has been initialized

		(if (and (= myObjectType metaTable) (<> myObjectType memory:))
			(__error (append "exportTab: " myObjectName " :Attempt to export meta table")))

       ;; Make sure we catch all errors which might occur.
       (onError __errorStop)
       ;; Open the specified .tab file and export.
       (setq myExportTabSW true)
       (setq fileID (fileOpen fileName 1 0)) 
       (|Gv:exportTab| fileID mySelf recordVectors:)
       (fileClose fileID 1)
       (setq fileID #void)
       

	   (SETTIME)
       recordCount) ;; end exportTab

;************ getBestIndex ***************
    (defun getBestIndex (argColSpec)
    ;; Inspect the cursor's indices and return the name of the best index to use to search on a list of specified columns.
	    vars:(i j high indexKeySpec numIndices bestIndex result)
	
	    (STARTTIME posttime:)
	
		(if (not (__getEnableIndicesSW)) (__error "getBestIndex: indices not enabled"))
		
		;This is a very simple routine. It simply looks for an index that has the greatest number of sequential column matches 
		; for the supplied columnm spec argument. Columns are matched begining at the first column only.  
		(setq max 0)
		(setq bestIndex -1)
		(setq numIndices (length indexDictionary))  
		(loop for i from 0 until numIndices do
			(setq indexKeySpec indexDictionary[i 1].keySpec)
			(if (isStructure indexKeySpec)
				(begin
				(setq j 0)
				(while (and (< j (length indexKeySpec)) (< j (length argColSpec)) (= indexKeySpec[j 0] argColSpec[j]))
					(setq j (iadd j 1))
				);while
				(if (> j max) 
					(begin
					(setq max j)
					(setq bestIndex i)
					end)
				);if
				end)
			);if
		);i
	
	    (if (> bestIndex -1) (setq result bestIndex) (setq result false))
	

	   (SETTIME)
	    result)

;*********** getRecordsByKey ****************
    (defun getRecordsByKey (argIndexName ...)
    ;; Use the getRecordByKey fucntion to find and return one or more record(s) in the table by key. 
    ;; Args:
    ;;		zero or more key components
    ;; Returns a vector of records or false if no matches found
		vars:(index values errstr result rowIndex argKeyValues numArgs v i m len)
	
		(STARTTIME posttime:)

		(if (not (__getEnableIndicesSW)) (begin (setq errstr " indices not enabled") (goto ERROR:) end))
		
		(setq i (member argIndexName indexDictionary))
		(if (= i false) (begin (setq errstr (apend " invalid index name '" argIndexName "' passed")) (goto ERROR:) end))
		(setq index indexDictionary[argIndexName])
		(setq numArgs (argCount))

		(if (= index.numCols 1)
			(if (> numArgs 1) (setq argKeyValues (argFetch 1)) (setq argKeyValues #void))
		else
			(begin
			(setq argKeyValues (new Vector:))
			(loop for i from 1 until numArgs do
				(setq argKeyValues[i] (argFetch i))
			);i
			end)
		);if
		
		;; Call refValues to get back a vector of index values matching key specifications
		(if (= argKeyValues #void)
			(setq values (index.index.Pv.refValues)) 
			(setq values (index.index.Pv.refValues argKeyValues))) 
	
		(if (= (length values) 0) (begin (SETTIME) (return false)))

		;; Now read the row and return it

		;; values contaqins the frameIDs of the records indexed by the argKeyVal. Now find the 
		;; rowIndex in bckVector for that
		;; record using the bckVectorIndex dictionary
		(setq len (length values))
		(setq result (new Vector: len))
		(loop for i from 0 until len do
			(setq rowIndex bckVectorIndex[values[i]])
			(setq result[i] (read rowIndex)) 
		);i	
	   	(SETTIME)
		(return result)
	
		ERROR::
		(__error (append "getRecordsByKey:" errstr))
	    
	    false)


;*********** getRecordBySJKey ****************
    (defun getRecordBySJKey (argTableKey argRowKey)
    ;; Use the getRecordBySJKey fucntion to find and return a record in the table using the star join index 

		vars:(index value errstr result rowIndex argKeyVal numArgs v i m)
	
		(STARTTIME posttime:)

  		(if (<> myObjectType metaTable:) 
  				(begin (setq errstr " not meta table") (goto ERROR:)))
  
  		(if (and (<> myCursorType disk:) (<> mCursorType static:))
  				(begin (setq errstr " not disk or static cursor") (goto ERROR:)))

		(setq v (member argTableKey memberTableDirectory))
		(if (= v false)
			(setq i -1)
		else
			(begin
			(setq m sjUniqueIDs[argRowKey])
			(if (= m #void)
				(setq i -1)
				(setq i (+ (* v sjIndexBlockSize) m)))
			end)
		);if

		(if (= i -1) 
			(setq rowIndex false)
			(setq rowIndex sjBckVectorIndex[i]))

		;; Now read the row and return it
	   	(SETTIME)
	   	(if (= rowIndex false)
	   		(return false)
			(setq result (read rowIndex)))

		(return result)
	
		ERROR::
		(__error (append "getRecordBySJKey:" errstr))
	    
	    false)

;*********** getRecordRowByKey ****************
    (defun getRecordRowsByKey (argIndexName ...)
    ;; Use the getRecordRowByKey fucntion to find and return a record's row in the table by key. 
    ;; Args:
    ;;		zero or more key components
    
		vars:(index values errstr result argKeyValues numArgs len v i m )
				
		(STARTTIME posttime:)

		(if (not (__getEnableIndicesSW)) (begin (setq errstr " indices not enabled") (goto ERROR:) end))
		
		(setq i (member argIndexName indexDictionary))
		(if (= i false) (begin (setq errstr (apend " invalid index name '" argIndexName "' passed")) (goto ERROR:) end))
		(setq index indexDictionary[argIndexName])
		(setq numArgs (argCount))

		(if (= index.numCols 1)
			(if (> numArgs 1) (setq argKeyValues (argFetch 1)) (setq argKeyValues #void))
		else
			(begin
			(setq argKeyValues (new Vector:))
			(loop for i from 1 until numArgs do
				(setq argKeyValues[i] (argFetch i))
			);i
			end)
		);if
		
		(if (= argKeyValues #void)
			(setq values (index.index.Pv.refValues)) 
			(setq values (index.index.Pv.refValues argKeyValues))) 
		
		;; value is the frameID of the record indexed by the argKeyVal. Now find the rowIndex in bckVector for that
		;; record using the bckVectorIndex dictionary
		(setq len (length values))
		(if (= len 0)
			(begin 
			(SETTIME)
			(return false)
			end)
		);if

		(setq result (new Vector: len))			
		(loop for i from 0 until len do
			(setq result[i] bckVectorIndex[values[i]])
  		);i
		(SETTIME)
  		(return result)
		
		ERROR::
		(__error (append "getRecordRowByKey:" errstr))
		
		false)


;*********** getRecordRowBySJKey ****************
    (defun getRecordRowBySJKey (argTableKey argRowKey)
    ;; Use the getRecordRowBySJKey fucntion to find and return a record row in the table using the star join index 

		vars:(index value errstr result rowIndex argKeyVal numArgs v i m)
	
		(STARTTIME posttime:)

  		(if (<> myObjectType metaTable:) 
  				(begin (setq errstr " not meta table") (goto ERROR:)))
  
  		(if (and (<> myCursorType disk:) (<> mCursorType static:))
  				(begin (setq errstr " not disk or static cursor") (goto ERROR:)))

		(setq v (member argTableKey memberTableDirectory))
		(if (= v false)
			(setq i -1)
		else
			(begin
			(setq m sjUniqueIDs[argRowKey])
			(if (= m #void)
				(setq i -1)
				(setq i (+ (* v sjIndexBlockSize) m)))
			end)
		);if

		(if (= i -1) 
			(setq rowIndex false)
			(setq rowIndex sjBckVectorIndex[i]))

	   	(SETTIME)
		(if (= rowIndex false)
			(return false)
			(return rowIndex))
	
		ERROR::
		(__error (append "getRecordRowBySJKey: " errstr))
	    
	    false)

;************ getColumnHeaders ***************
    (defun getColumnHeaders(startCol endCol) 
    ;; *******************************************************************
    ;; Summary:  Returns the table headers as a tab delimited string.
    ;; *******************************************************************
       vars:(colIndex dls colName)
       

       (STARTTIME posttime:)
         
       (if (not myObjectState) (__errorState)); make sure object has been initialized
       (setq dls "")
       (loop for colIndex from startCol to endCol do
           (setq colName (string colVector[colIndex]))
           (if (= colName "#void") (setq colName ""))
           (if (= (left colName 1) "|") (setq colName (mid colName 1 (subi (length colName) 2))))
           (if (<> colIndex endCol) 
               (setq dls (append dls colName (char 09)))
               (setq dls (append dls colName))
               ) ; end if
           ) ;;end column loop
           

	   (SETTIME)
       (append dls "")) ;; end getColumnHeaders

;************ getNewRecord *********************       
    ;; Summary:  Returns a blank new record.
    (defun getNewRecord()
    	vars:(record) 
       (if (not myObjectState) (__errorState)); make sure object has been initialized
    	(setq record (copy recordStructure))
		(setAttributes record colVector)
	   	record)	;;tm Note that it does not put the record into a view!! 
    			;;tm You call insert to do that.

;*********** indexView **********************
    (defun indexView (argIndexName ...)
    ;; Use the indexViewByKey function to filter the current view so that it contains only records from the
    ;; table that match the specified key value.
    ;; Optional Args:
    ;; keyVal		-- key value to use to filter current view
    ;;	partial:	-- allow partial match on keyVal

		vars:(i args len result)
	
		(STARTTIME posttime:)
	
		(setq len (argCount))
		(setq args (new Vector: len))
		(loop for i from 0 until len do
			(setq args[i] (argFetch i))
		);i
		(setq result (apply __indexView args))
	
	   (SETTIME)
		result)

    (defun __indexView (argIndexName ...) ; internal routine -- no timer
    ;; Use the indexViewByKey function to filter the current view so that it contains only records from the
    ;; table that match the specified key value.
    ;; Args:
    ;;		argIndexName	Name of index to use
    ;;		zero or more key components

		vars:(index i values errstr rowIndex argKeyValues len numArgs)

		(if (not (__getEnableIndicesSW)) (begin (setq errstr " indices not enabled") (goto ERROR:) end))
		
		(setq i (member argIndexName indexDictionary))
		(if (= i false) (begin (setq errstr (append " invalid index name '" argIndexName "' passed")) (goto ERROR:) end))
		(setq index indexDictionary[argIndexName])
		(setq numArgs (argCount))
		;set defaults for optional arguments
		(if (= index.numCols 1)
			(if (> numArgs 1)(setq argKeyValues (argFetch 1)) (setq argKeyValues #void))
		else
			(begin
			(setq argKeyValues (new Vector:))
			(loop for i from 1 until numArgs do
				(setq argKeyValues[i] (argFetch i))
			); i
			end)
		);if
		(if (= argKeyValues #void)
			(setq values (index.index.Pv.refValues))
			(setq values (index.index.Pv.refValues argKeyValues)))
	
	  	(setq len (length values))
	
		(if (= myCursorType memory:)
			(setq rowVector (new Vector: object: len)); create new so we don't distirb the bckVector if rowVector points to it
			(resize rowVector len));if Just resize rowVector because it never points to bckVector on disk and static cursors
		
		(if (<> myCursorType memory:)
			;handle disk or static cursor
			(loop for i from 0 until len do
				; rowVector contains offsets into bckVector
				;(writeln "values[i]=" values[i] " bckVectorIndex[values[i]]=" bckVectorIndex[values[i]])
				(setIntVector rowVector i bckVectorIndex[values[i]])
			);i
		else ; handle memory cursor
			(loop for i from 0 until len do
				; rowVector contains pointer to the same records pointed to by bckVector
				(setObjVector rowVector i (refObjVector bckVector bckVectorIndex[values[i]]))
			);i
		);if
	
		(setq recordCount (length rowVector))
		
		(return len)
	
		ERROR::
		(__error (append "indexView: " errstr))
	    true); end of __indexView

;************* indexViewBySJRowKey *******************
(defun indexViewBySJRowKey(argRowKey)
	vars:(i k v w len )
	(STARTTIME posttime:)

	;get index of table based on supplied unique row key
	(setq len (length memberTableDirectory))
	(setq i sjUniqueIDs[argRowKey])
	(setq k 0)
	(if (<> i #void)
		(loop for w from 0 until len do
			(setq v sjBckVectorIndex[(+ i (* w sjIndexBlockSize))])
			(if (<> v -1)
				(begin
				(setq rowVector[k] v)
				(setq k (iadd k 1))
				end)
			);if
		);i
	);if
	(resize rowVector k) 
	(setq recordCount (length rowVector))

	(SETTIME)
	(return (length rowVector))
	)

;************* indexViewBySJTableKey *******************
(defun indexViewBySJTableKey(argTableKey)
	vars:(w k i v )
	(STARTTIME posttime:)
	; get index of table based on supplied table key

	(setq w (member argTableKey memberTableDirectory)) 
	(if (= w false) 
		(__error "indexView -- starJoin index key value not found"))
	(setq startIndex (* w sjIndexBlockSize))
	(setq len (length sjUniqueIDs)) 
	(setq k 0)
	(loop for i from 0 until len do
		(setq v sjUniqueIDs[i 1])
		(setq v sjBckVectorIndex[(+ startIndex v)])
		(if (<> v -1)
			(begin
			(setq rowVector[k] v)
			(setq k (iadd k 1))
			end)
		);if
	);i  
	(resize rowVector k)
	(setq recordCount (length rowVector))
	(SETTIME)
	(return (length rowVector))
	)

;************* import *******************
	(defun import(aFilename ...)
	;; *******************************************************************
    ;; Summary:  Imports a comma or tab delimited file into the table cursor.
	;; *******************************************************************
		regs:(n N)
		vars:(aExtension _ImportType)
		(if (> (argCount) 1) 
            (begin ;; Check for import type specifier			  
			  (setq _ImportType (argFetch 1))
			  (if (and (<> _ImportType csv:) (<> _ImportType tab:)) (begin
	       		  (__error (append "import: " myObjectName " :bad import type: " _ImportType))))
		    ) else 
            (begin ;; guess at import type based on file extension
			  (setq n (- (length aFilename) 1))
			  (while (and (> n 1) (<> (mid aFilename n 1) ".")) (-- n))
			  (if (= n 0) ;; use default as no extension was found
				  (setq _ImportType importTab)
			      else 
                 (begin
				   (setq aExtension (mid aFilename n 10))
				   (cond
				    ((= aExtension ".csv") (setq _ImportType importCsv))
				    ((= aExtension ".tab") (setq _ImportType importTab))
				    ((= aExtension ".txt") (setq _ImportType importTab))
				    ((= aExtension ".xls") (setq _ImportType importTab))
				    (else (setq _ImportType importTab))
			    	) ; end cond
			     )) ; end inner if
		     )) ; end outter if

		;; Invoke the appropriate import method.
		(_ImportType aFilename)) ; end import

;************* importCsv *******************
    (defun importCsv(fileName)
    ;; *******************************************************************
    ;; Summary:  Imports a comma delimited file into the table cursor.
    ;; *******************************************************************
       ;; Make sure we catch all errors which might occur.

		vars:(enableState)
		

		(STARTTIME posttime:)
		
       (if (not myObjectState) (__errorState)); make sure object has been initialized

       (setq myDirtyFlag true)
       (if (<> myCursorType memory:)
       		(begin
	       ;;tm - Make sure we are not a meta table
	       (if (= myObjectType metaTable:) 
	       		(__error (append "importCsv: " myObjectName " :An attempt was made to import records into a meta table")))
	
			(if (and (<> metaTable #void) (not lastMemberTableSW))
				(__error (append "importCsv: " myObjectName
								" :An attempt was mad to import into a member table other than the last member table")))
			end)
		);if
	
		;; Make sure we are not a static cursor
		(if (= myCursorType static:)
			(__error (append "importCsv: " myObjectName 
							" :An attempt was made to import records on a static cursor")))

	   (if (<> myCursorType memory:) (__clearBuffer))

       (onError __errorStop)
		;; Make the current view the same as the bckVector.
		(__reset)	; For memory cursors this means that rowVector points to bckVector
				; For disk cursors this means that __makeRowVector will be called
       
       ;save state of indices
       (setq enableState (__getEnableIndicesSW))
       
       ;; Open the specified .csv file and import.
       
       (if enableState then (__disableIndices)); if indices were enabled then disable them
       
       (setq fileID (fileOpen fileName 0 0))
       (|Gv:importTab| fileID mySelf recordsOnly:) ;; NOTE recordsOnly Flag
       (fileClose fileID 1)
       (setq fileID #void)
       
       (if (<> myCursorType memory:) (__syncUp));; sync up all static cursors
       
       (if enableState then (__enableIndices)); if indices were enabled then re-enable them

	   (SETTIME)
       recordCount) ;; end importCsv


;************* importTab *******************
    (defun importTab(fileName)
    ;; *******************************************************************
    ;; Summary:  Imports a tab delimited file into the table cursor.
    ;; *******************************************************************
       ;; Make sure we catch all errors which might occur.

		vars:(enableState)
		

		(STARTTIME posttime:)
		
       (if (not myObjectState) (__errorState)); make sure object has been initialized

       (setq myDirtyFlag true)
       (if (<> myCursorType memory:)
       		(begin
	       ;;tm - Make sure we are not a meta table
	       (if (= myObjectType metaTable:) 
	       		(__error (append "importTab: " myObjectName " :An attempt was made to import records into a meta table")))
	
			(if (and (<> metaTable #void) (not lastMemberTableSW))
				(__error (append "importTab: " myObjectName
								" :An attempt was mad to import into a member table other than the last member table")))
			end)
		);if
	
		;; Make sure we are not a static cursor
		(if (= myCursorType static:)
			(__error (append "importTab: " myObjectName 
							" :An attempt was made to import records on a static cursor")))

	   (if (<> myCursorType memory:) (__clearBuffer))

       (onError __errorStop)
		;; Make the current view the same as the bckVector.
		(__reset)	; For memory cursors this means that rowVector points to bckVector
				; For disk cursors this means that __makeRowVector will be called
       
       ;save state of indices
       (setq enableState (__getEnableIndicesSW))
       
       ;; Open the specified .tab file and import.
       
       (if enableState then (__disableIndices)); if indices were enabled then disable them
       
       (setq fileID (fileOpen fileName 0 0))
       (|Gv:importTab| fileID mySelf recordVectors:)
       (fileClose fileID 1)
       (setq fileID #void)
       
       (if (<> myCursorType memory:) (__syncUp));; sync up all static cursors
       
       (if enableState then (__enableIndices)); if indices were enabled then re-enable them

	   (SETTIME)
       recordCount) ;; end importTab

;************ insert *******************
    (defun insert(row record)
    ;; *******************************************************************
    ;; Summary:  Insert a record into the table for update.
    ;; Args:     row      Row number of row to be written to the table.
    ;;           record   Record to be written to the table.
    ;; Return:   record   The record just written. 
    ;; *******************************************************************
		vars:(frameID bckRecordCount i j len)
		(STARTTIME posttime:)

       (if (not myObjectState) (__errorState)); make sure object has been initialized
       (setq myDirtyFlag true)

		(if (<> myCursorType memory:)
			(begin
	       ;;tm - Make sure we are not trying to insert a record to a meta table
	       	(if (= myObjectType metaTable:) 
	       		(__error (append "insert: " myObjectName 
	       						" :An attempt was made to insert a record in a meta table")))

			(if (and (<> metaTable #void) (not lastMemberTableSW))
				(__error (append "insert: " myObjectName 
								" :An attempt was mad to insert a record on a member table other than the last member table.")))
 			end)
 		);if
	
		;; Make sure we are not a static cursor
		(if (= myCursorType static:)
			(__error (append "insert: " myObjectName 
							" :An attempt was made to insert a record on a static cursor")))


		(setq bckRecordCount (length bckVector))

       	;; Make sure the specified row number is valid.
       	(if (or (< row 0) (> row bckRecordCount))
           (__error (append "insert: " myObjectName " - " row
                          ":An attempt was made to insert with a bad row number.")))

       	(setq bckRecordCount (addi bckRecordCount 1))
  
  
 		(if (= myCursorType memory:)
 			(begin
 			(setq recordCount bckRecordCount)
			(setq record (copy record))
			(setAttributes record colVector)
			(^insert bckVector row record); remember that rowVector points to bckVector at this point

			(if enableIndicesSW
				(begin 
				(setq frameID (__makeFakeFrameID))
				(setq bckVectorFrameID[row] frameID)

				(__insertIntoIndices frameID record)

				(setq len (length bckVectorIndex))
 				(loop for i from 0 until len do
 					(setq j (refDirValue bckVectorIndex i ))
 					(if (>= j row) 
 						(setDirValue bckVectorIndex i (iadd j 1)) )
 				);
 				(setq bckVectorIndex[(refObjVector bckVector row)] row)

;				; update bckVectorIndex for new row and each row higher than the newly inserted row
;				(loop for i from row until recordCount do
;					(setq bckVectorIndex[bckVectorFrameID[i]] i)
;				);

				end)
			else
				(setq indicesDirtySW true)
			);if
			
 			(__reset)

 			end)
 		else ; handle disk cursor
 			(begin
 			(setq recordCount bckRecordCount)
			(__clearBuffer)
			(setAttributes record #void)
			(^insert bckVector row -1.0) ; make a place for new record in bckVector

			(setNumVector bckVector row (setq myRepos[frame: #void] record)); write new table record to disk
			(setAttributes record colVector)
			(if myBuf.enableIndicesSW
				(begin

				(__insertIntoIndices (refNumVector bckVector row ) record)
				; update bckVectorIndex for new row and each row higher than the newly inserted row

				(setq len (length bckVectorIndex))
 				(loop for i from 0 until len do
 					(setq j (refDirValue bckVectorIndex i))
 					(if (>= j row) 
 						(setDirValue bckVectorIndex i (iadd j 1)) )
 				);
				(setq bckVectorIndex[(refNumVector bckVector row)] row)

;				(loop for i from row until recordCount do
;					(setq bckVectorIndex[bckVector[i]] i)
;				);

				end)
			else
				(setq myBuf.indicesDirtySW true)
			);if
			(^insert myBuf.buffer row #void)
			(__reset)
			(__syncUp); sync all static cursors

			end)
		);if
			
	   (SETTIME)
       ;; Return the record just inserted.
       record) ;; end of insert

;************* isView ****************
    ;; Returns true if the specified key is a saved view of this cursor.
    (defun isView(key) 
	    (if (not myObjectState) (__errorState)); make sure object has been initialized
        (<> viewDirectory[key] #void)) 

;************ maximum **************
    (defun maximum(totalLambda)
    ;; *******************************************************************
    ;; Summary:  Returns the max of a lambda value on each record of a table.
    ;; *******************************************************************
    ;;tm! we should consider maintaining a totals, min and max value for each
    ;;tm! column in the table to avoid iterations like the following one.
       vars:(rowIndex result)
       (STARTTIME posttime:)
       (if (not myObjectState) (__errorState)); make sure object has been initialized
       (if (<= recordCount 0) (begin (SETTIME) (return 0)))
       (setq result (totalLambda (__read 0)))
       ;; Compute the min lambda value for each record in the table.
       (loop for rowIndex from 1 until recordCount do
           (setq result (max result (totalLambda (__read rowIndex))))
       ); rowIndex

	   (SETTIME)
       result) ;; end maximum

;************* miniumum *****************
    (defun minimum(totalLambda)
    ;; *******************************************************************
    ;; Summary:  Returns the min of a lambda value on each record of a table.
    ;; *******************************************************************
       vars:(rowIndex result)
       (STARTTIME posttime:)
       (if (not myObjectState) (__errorState)); make sure object has been initialized
       (if (<= recordCount 0) (begin (SETTIME) (return 0)))
       (setq result (totalLambda (__read 0)))
       ;; Compute the min lambda value for each record in the table.
       (loop for rowIndex from 1 until recordCount do
           (setq result (min result (totalLambda (__read rowIndex))))
       ); rowIndex
           
	   (SETTIME)
       result) ;; end minimum


;************** omit ******************
    (defun omit(findLambda)
    ;; *******************************************************************
    ;; Summary:  Deletes those records from a table for which a lambda predicate is true.
    ;; *******************************************************************
       vars:(rowIndex enableState)
       (STARTTIME posttime:)
       (if (not myObjectState) (__errorState)); make sure object has been initialized
       (setq myDirtyFlag true)
		;; Make sure we are not a static cursor
		(if (= myCursorType static:)
			(__error (append "omit: " myObjectName 
							" :An attempt was made to omit records on a static cursor")))

		(if (<> myCursorType memory:)
			(begin
			(if (= myObjectType metaTable)
				(__error (append "omit: " myObjectName 
							" :An attempt was made to omit records on a meta table")))
	
			(if (and (<> metaTable #void) (not lastMemberTableSW))
				(__error (append "omit: " myObjectName 
							" :An attempt was made to omit records on a member table other than last member table")))
			end)
		);if

       
   		; eliminate any filtering
		;; Make the current view the same as the bckVector.
		(__reset)	; For memory cursors this means that rowVector points to bckVector
				; For disk cursors this means that __makeRowVector will be called

       (if (<= recordCount 0) (begin (SETTIME) (return recordCount)))

	   (__disableIndices)
       ;; Omit those rows for which the find Lambda returns true.
       (loop for rowIndex from (- recordCount 1) to 0 by -1 do
           (if (findLambda (__read rowIndex))
                  (__delete rowIndex))
       ) ;; end loop
       (setq recordCount (length rowVector))
       (if (= myCursorType disk:) (__syncUp))
       (__enableIndices)
	   (SETTIME)
       recordCount) ;; end omit

;************** read *********************
    (defun read (row)
    ;; *******************************************************************
    ;; Summary:  Reads a record from the table.
    ;; Args:     row      Row number of row to be read.
    ;; Return:   record   The record just read. 
    ;; *******************************************************************
		vars:(record)
		(STARTTIME posttime:)
		(if (or (< row 0) (> row recordCount))
		   (__error (append "read: " myObjectName " - " row
		                  ":An attempt was made to read with a bad row number.")))
		
			(if (= myCursorType memory:)
				(begin
				(if (= row recordCount) (begin (SETTIME) (return #void))) ;tm!!! This is a cludge to make the interface work like the original dataMineLib
			(setq record (refObjVector rowVector row))
		   	(setAttributes record colVector)
				end)
			else
			(setq record (copy (__read row))) ;; get COPY of record returned by internal read
		);if
		(setAttributes record colVector)
		(setCdr record #void)
	   	(SETTIME)
		record);__read

;************** __read **********************
    (defun __read(row ...)
       vars:(record bufferEntry i j temp bckVectorRow rec ptrs prevRec  tempRecord memberTableRepos
       		nextRec nextRecord prevRecord tailRecord _place bckVectorReadSW  buffer t0 t1 t2 t3 t4)
		(defun fmtNum (num)
			vars:(strFill)
			(setq strFill "          ")
			(right (+  strFill (string num)) 8)
		)
	  
		(defun showBufList ()
			vars:(record i)
			(setq i myBuf.bufHead)
			(while (and (<> i #void))
				(setq record myBuf.buffer[i])
				(writeln (fmtNum i) (fmtNum (floor (cdr record))) (fmtNum (* (fraction (cdr record)) NEXTRECMASK)) (fmtNum myBuf.bufHead) (fmtNum myBuf.bufTail))
				(if (= i myBuf.bufTail) (goto OUT:))
				(setq i (* (fraction (cdr record)) NEXTRECMASK)) 
			);i
		OUT::
		true)		
	
	 	(setq row (integer row))
  
       (if (not myObjectState) (__errorState)); make sure object has been initialized
       (if (= row #void) (setq row 0))

   		(setq bckVectorReadSW (and (= (argCount) 2) (= (argFetch 1) bckVector:)))

       (if (or (< row 0) (>= row (if bckVectorReadSW (length bckVector) recordCount)))
           (__error (append "read: " myObjectName " - " row
                          ":An attempt was made to read with a bad row number.")))

  		(setq _place #void)
	  	(if (= myCursorType memory:) 
	  		(begin
	  		(if bckVectorReadSW
	  			(setq record (refObjVector bckVector row))
	  			(setq record (refObjVector rowVector row)))
	       	(setAttributes record colVector)
	  		end)
	  	else ;; disk: or static: cursor read
	  		(begin 

	  		(if bckVectorReadSW
	  			(setq bckVectorRow row)
	  			(setq bckVectorRow (refIntVector rowVector row))); The elements of rowVector are indexes into bckVector!

	  		;; get and bump hit count by one 
	  		(if myBufferUseSW
	  			(begin
				;; Get record
				(setq buffer myBuf.buffer)	
	  			(if (<> (setq record (refObjVector buffer bckVectorRow)) #void) ; buffered record found
	  				(begin
	  				(++ dbNumBufHits)
					(cond
						((= myBuf.bufHead bckVectorRow) ; current record is the head of the list
							(if (<> myBuf.bufTail bckVectorRow)  ; if  current record is not also tail (ie: only one buffered record)
								(begin
								(setq _place CurrentRecordIsHeadOfList:)
;(writeln "buffered read at head of list:")
								(setq ptrs (cdr record)); get the composite pointers out of the records cdr
								(setq prevRec (integer (floor ptrs))) ; get index to the previous record
								(setq nextRec (integer (* (fraction ptrs) NEXTRECMASK))) ; get index to the next record 
								; new head will be current records next record 
								(setq nextRecord (refObjVector buffer nextRec))
								(setCdr nextRecord (+ nextRec (fraction (cdr nextRecord)))); prevRec of head points to itself
								(setq myBuf.bufHead nextRec)
								
								;; Moving current record to tail of list
								;; myBuf.bufTail -- new prevRec of buffer record will be current tail record
								;; bckVectorRow -- new tail's nextRec should point to itself!
								;; Note: Also need to check case where current record (bckVectorRow) is tail record's prevRec!

								(setq tailRecord (refObjVector buffer myBuf.bufTail))
								(setq ptrs (cdr tailRecord)) ; get current tail records ptrs
								(if (= (integer (floor ptrs)) bckVectorRow) ; special condition where record is second to last in buffer list
									(setq ptrs (+ prevRec (/ bckVectorRow NEXTRECMASK)))
								else ; condition where current record is not second to last
									(setq ptrs (+ (floor ptrs) (/ bckVectorRow NEXTRECMASK)))
								)
								(setCdr tailRecord ptrs) ;store new nextRec pointer in cdr of old tail
								(setCdr record (+ myBuf.bufTail (/ bckVectorRow NEXTRECMASK))); save cdr changes in current record object
								(setq myBuf.bufTail bckVectorRow) ; set new tail for list


								end)
							);if								
						)
						((<> myBuf.bufTail bckVectorRow) ; current record is not head or tail
							(begin                                   
							(setq _place CurrentRecordIsBetweenHeadAndTail:)
;							(writeln "buffered read between head and tail") 
							(setq ptrs (cdr record));get the composite pointers out of the records cdr
							(setq prevRec (integer (floor ptrs))); get index to the previous record
							(setq nextRec (integer (* (fraction ptrs) NEXTRECMASK))) ; get index to the next record
;					(display "current record's prevRec=" prevRec " nextRec=" nextRec )
							;Set the previous record's nextRec to point to the current records nextRec
							(setq prevRecord (refObjVector buffer prevRec))
;					(display ",  previous records prevRec=" (floor (cdr prevRecord))" nextRec=" (* (fraction (cdr prevRecord)) NEXTRECMASK) ) 
							(setCdr prevRecord (+ (floor (cdr prevRecord)) (fraction ptrs)))
							;Set the next records prevRec to point to the current records prevRec
							(setq nextRecord (refObjVector buffer nextRec))
							(setCdr nextRecord (+ prevRec (fraction (cdr nextRecord))))
							;Now make the current record the last record in list
							;; Handle special condition where current record is second to last in buffer list
							(setq tailRecord (refObjVector buffer myBuf.bufTail))
;					(display ", tail records prevRec=" (floor (cdr tailRecord)) " nextRec=" (* (fraction (cdr tailRecord)) NEXTRECMASK) _eol)							
							(setq ptrs (cdr tailRecord))
							(if (= (integer (floor ptrs)) bckVectorRow)
								(setCdr tailRecord (+ prevRec (/ bckVectorRow NEXTRECMASK)))
							else
								(setCdr tailRecord (+ (floor ptrs) (/ bckVectorRow NEXTRECMASK)))
							);if
							(setCdr record (+ myBuf.bufTail (/ bckVectorRow NEXTRECMASK)))
							(setq myBuf.bufTail bckVectorRow)
							end)
							
						)
					);cond
					end)
				else
					(begin ; read record from disk                    
					(setq _place ReadRecordFromDisk:)
					(++ dbNumDiskHits)         
;(debug traceon:)					
;(writeln "bckVector[" bckVectorRow"]=" bckVector[bckVectorRow])

					(if (= myObjectType metaTable:)
						(begin
						(setq memberTableRepos (__getRecordRepos bckVectorRow))
			  			(setq record memberTableRepos[frame: (refNumVector bckVector bckVectorRow)])
						end)
					else
			  			(setq record myRepos[frame: (refNumVector bckVector bckVectorRow)])
			  		);if

			  		(setCdr record #void); clear any Cdr that was saved with record
			  		;; Remove most recently read record from buffer list if we have reached buffer capacity
			  		(if (> myBuf.bufUsed myBuf.bufSize)
			  			(begin
;			  			(writeln "removing most recently added buffer item")
			  			(setq ptrs (cdr (refObjVector buffer myBuf.bufHead))) ; get ptrs from current head of list
			  			(setq nextRec (integer (* (fraction ptrs) NEXTRECMASK)))
			  			;; set the CDR in the new head of buffer list. Note that only the prevRec portion
			  			;; of its CDR changed. It's nextRec portion is still the same
			  			(setq tempRecord (refObjVector buffer nextRec))
			  			(setCdr tempRecord (+ nextRec (fraction (cdr tempRecord))))
			  			(setObjVector buffer myBuf.bufHead #void) ; clear the old buffered record out
			  			(setq myBuf.bufHead nextRec) ; point to the new head of buffer list
			  			(setq myBuf.bufUsed (iadd myBuf.BufUsed 1))
			  			end)
			  		)
			  		
			  		(setObjVector buffer bckVectorRow record) ;; buffer record
				    
				    ;; Insert record at beginning of list
				    (cond 
				    	((= myBuf.bufUsed 0) 
				    		(begin                      
				    		(setq _place CreateFirstBufferedRecord:)
;						    (writeln "create first bufferd record") 
				    		(setCdr record (+ bckVectorRow (/ bckVectorRow NEXTRECMASK)))
				    		(setq myBuf.bufHead bckVectorRow)
				    		(setq myBuf.bufTail bckVectorRow)
   						    (setq  myBuf.bufUsed (iadd myBuf.bufUsed 1))
   						    end)
				    	)
				    	(true
				    		(begin
				    		(setq _place CreateBufferedRecordAtBeginingOfList:)
;				    		(writeln "create buffered record at begining of list")
							(setCdr record (+ bckVectorRow (/ myBuf.bufHead NEXTRECMASK)))
							(setq nextRecord (refObjVector buffer myBuf.bufHead))
							(setCdr nextRecord (+ bckVectorRow (fraction (cdr nextRecord))))
							(setq myBuf.bufHead bckVectorRow)
   						    (setq  myBuf.bufUsed (iadd myBuf.bufUsed 1))
						    end)
  						)
					);cond
			  		end)
			  	); if 
			  	
			  	end)
			else
				(begin               
				(if (= myObjectType metaTable:)
					(begin 
					(setq memberTableRepos (__getRecordRepos bckVectorRow))
		  			(setq record memberTableRepos[frame: (refNumVector bckVector bckVectorRow)])
					end)
				else
		  			(setq record myRepos[frame: (refNumVector bckVector bckVectorRow)])
		  		);if
				end)
			);if
	       	(setAttributes record colVector)
	       	end)
	    );if
		    
;	   (SETTIME)
       record) ;; end of read


;************* refExport ***************       
    (defun refExport(row)
    ;; *******************************************************************
    ;; Summary:  Returns a record, for export, to the exportTab function.
    ;; *******************************************************************
        regs:(n N)
        vars:(aRecord temp recVector)

	 	(STARTTIME posttime:)
		(if (not myObjectState) (__errorState)); make sure object has been initialized

        ;; What type of records are we exporting?
        (if (= myExportTabSW true)
            (begin ;; We exporting tab delimited records.
		      (if (= row 0)  ;; If the row is zero, return the column names for the header record.
		          (setq recVector colVector)
                  else
                  (begin   ;; If the row is NOT zero, return the specified data record.
		            (-- row)  ;; Adjust the record index for the header record.
		            (if (>= row recordCount) (begin (SETTIME) (return false))) ;; If there are no more records, tell exportTab to stop exporting.
		            ;; Otherwise return the next record for export.
		            (setq recVector (__read row))
                  )) ; end row number if

              ;; Return the record vector as is and let the global exportTab function convert it to tab delimited.
              (setq aRecord recVector)
            ) else
            (begin ;; We exporting comma delimited records.
		      (if (= row 0)  ;; If the row is zero, return the column names for the header record.
		          (setq recVector colVector)
                  else
                  (begin   ;; If the row is NOT zero, return the specified data record.
		            (-- row)  ;; Adjust the record index for the header record.
		            (if (>= row recordCount) (begin (SETTIME) (return false))) ;; If there are no more records, tell exportTab to stop exporting.
		            ;; Otherwise return the next record for export.
		            (setq recVector (__read row))
                  )) ; end row number if

              ;; Convert the record vector into a comma delimited string.
              (setq N (length recVector))
              (loop for n from 0 until N do
                (setq temp recVector[n])
                (cond
                 ((isDictionary temp) (setq temp (string temp true)))
                 ((isDirectory temp) (setq temp (string temp true)))
                 ((isStructure temp) (setq temp (string temp true)))
                 ((isVector temp) (setq temp (string temp true)))
                 (else (setq temp (string temp)))        
                 ) ; end cond
                (if (= aRecord #void) (setq aRecord temp) (setq aRecord (append aRecord "," temp)))
                ) ; end convert loop
            )) ; end type of record if

        ;; Return the record to the global exportTab function for output.
        (SETTIME)
        aRecord) ;; end of refExport 


;*************** refImport *******************
    (defun refImport(row)
    ;; *******************************************************************
    ;; Summary:  Returns an empty record to the importTab function.
    ;; *******************************************************************
		vars:(result)
		(STARTTIME posttime:)		
		(if (not myObjectState) (__errorState)); make sure object has been initialized
		;; Make sure we are not a static cursor
		(if (= myCursorType static:)
			(__error (append "refImport: " myObjectName 
							" :An attempt was made to refImport a record on a static cursor")))
		
		;; If the row is zero, return an empty vector to hold the column names,
		;; Otherwise, return an empty record (use the recordStructure as a template).
		
		(if (= row 0)
		   (setq result (^new Vector: 0))
		   (setq result(copy recordStructure)))

		(SETTIME)
		result) ;; end of refImport

;************** reset *********************
    (defun reset()
    ;; *******************************************************************
    ;; Summary:  Resets the backup copy and views of the data mine table. 
    ;; Same as restore except that veiws are cleared
    ;; *******************************************************************
		(STARTTIME posttime:)
		(__reset)
		(SETTIME)
       	true) ;; end of reset

    (defun __reset() ;internal (no timer)
    ;; *******************************************************************
    ;; Summary:  Resets the backup copy and views of the data mine table. 
    ;; Same as restore except that veiws are cleared
    ;; *******************************************************************
;		(STARTTIME posttime:)
		(__restore)
		(setq viewDirectory (new Directory:))
;		(SETTIME)
		true) ;; end of reset


;************* restore ********************
    (defun restore()
    ;; *******************************************************************
    ;; Summary: Restores the backup copy of the data mine table.
    ;; *******************************************************************
		(STARTTIME posttime:)
		(__restore)
		(SETTIME)
       	true) ;; end of restore


    (defun __restore(); internal (no timer)
    ;; *******************************************************************
    ;; Summary: Restores the backup copy of the data mine table.
    ;; *******************************************************************
;		(STARTTIME posttime:)
		(if (not myObjectState) (__errorState)); make sure object has been initialized

		;; Make sure the backup records and views are restored.
		(setq recordCount (length bckVector))
		(if (= myCursorType memory:)
				(setq rowVector bckVector)
			else ; disk or static cursor 
		    (__makeRowVector)
		);if
;		(SETTIME)
		true) ;; end of restore


;************** restoreView ******************
    (defun restoreView(key)
    ;; *******************************************************************
    ;; Summary: Restores the current record view as specified.
    ;; *******************************************************************
		(STARTTIME posttime:)
		(if (not myObjectState) (__errorState)); make sure object has been initialized
		(if (<> viewDirectory[key] #void)
			(setq rowVector (copy viewDirectory[key])))
		(setq recordCount (length rowVector))
		(SETTIME)
		true) ;; end of restoreView


;**************** run *********************
    (defun run(filterString)
	;;************************************************************
    ;; Run the specified javaScript Lambda against this cursor. 
    ;;************************************************************
    ;Note: No timing in this routine as the filterLambdas often call other parts of the cursor API
		vars:(filterLambda result)
		(STARTTIME posttime:)
		(if (not myObjectState) (__errorState)); make sure object has been initialized
		(setq filterLambda (myParent.compileLambda filterString))
		(setq result (filterLambda mySelf))
		(SETTIME)
		result) ;; end of run

;*************** save *****************
    (defun save() 
    ;; *******************************************************************
    ;; Summary:  Save the contents of the memory cursor to disk (overwriting the existing table if any).
    ;; *******************************************************************
       vars:(record i cursor oldVector oldSchema oldRecordcount oldIndexDictionary oldIndexCount len oldValue value
       copyofIndexDictionary indexStruct copyOfIndexObjects)  
       (STARTTIME posttime:)
       (if (not myObjectState) (__errorState)); make sure object has been initialized
       
  		(if (<> myCursorType memory:) (begin (SETTIME) (return true)))

		(if (= myObjectType metaTable:) 
			(begin 
			(writeln "dataMineLib:__tableCursor:save: Warning! Can't save a memory cursor created on a meta table")
			(SETTIME) 
			(return true)))

		;; Test to see if a disk cursor exists
		(if (<> openTables[myObjectName] #void)
		  	(if (> (length openTables[myObjectName].cursorList) 0)
		  	 	(loop for i from 0 until (length openTables[myObjectName].cursorList) do
		  	 		(if (<> openTables[myObjectName].cursorList[i].myCursorType memory:)
						(__error (append "save: " myObjectName 
					  	  ":An attempt was made to save while a static or exclusive disk cursor was open."))
					);if
				); i
		  	)if
		);if

;(writeln "bckVector=" bckVector)
;(writeln "bckVectorIndex=" bckVectorIndex)
;(writeln "bckVectorFrameID=" bckVectorFrameID)
;(writeln "indexDictionary=" indexDictionary)

		(myParent.__beginTrans myExtIndex)
		;;Delete the current disk tables records
		(setq oldSchema myRepos[myObjectName]) ; load the old schema as the current schema may have changed!

		(if (<> oldSchema #void) ; Check if the old schema was deleted since the memory cursor was opened
			(begin

			;; Make sure table is not a member table
			(if (<> oldSchema.metaTable #void)
				(__error (append "save: " myObjectName 
						" :Attempt to save a memory cursor that is a member table")))

			;Delete the old table rows
			(setq oldVector myRepos[frame: oldSchema.bckVectorKey])
			(setq oldRecordCount (length oldVector))
			(if (<> oldVector #void)
				(loop for i from 0 until oldRecordCount do
					(setq myRepos[frame: (refNumVector oldVector i)] #void) ; delete table row on disk
				); i
			);if
			(setq oldVector #void)
			
			;;Delete the current disk tables views 
			(if (<> oldSchema.viewDirectoryKey #void)
				(setq myRepos[frame: oldSchema.viewDirectoryKey] #void)) ; delete the old disk cursor views if any

			;;Delete the old indices
			(if (<> oldSchema.indexDictionaryKey #void) 
				(begin
				(setq oldIndexDictionary myRepos[frame: oldSchema.indexDictionaryKey])
				(if (<> oldIndexDictionary #void)
					(begin
					(setq len (length oldIndexDictionary))
					(loop for i from 0 until len do 
						(setq indexStruct oldIndexDictionary[i 1])
						;(setq indexStruct.index (new index))
						;(indexStruct.index.Pv.init indexStruct.frameID myRepos transStruct)
						(setq indexStruct.index (new index indexStruct.frameID myRepos transStruct))
						(indexStruct.index.Pv.drop)
					);i
					end)
				);if
				(setq myRepos[frame: oldSchema.indexDictionaryKey] #void)
				end)
			);if

			end)
		);if 

		; Delete any current views from the memory cursor
		(setq viewDirectoryKey #void)

		; create new bckVector from contents of memory cursor rowVector
		; update indices for each record written if indices are enabled
		; delete indices if indices are disabled
        (setq bckVector (new Vector: number: recordCount))

        (if enableIndicesSW
        	(begin ;enabled indices
        	;; Note that we are saving the rowVector! This means we have to only
        	;; include in the indices those records in the current view.
        	(if (<> indexDictionary #void) ;clear current index entries
        		(begin
        		(setq len (length indexDictionary))
        		(loop for i from 0 until len do
        			(setq indexStruct indexDictionary[i 1])
        			(if (<> indexStruct.index #void) (indexStruct.index.Pv.clear))
        		);i
        		end)
        	);if

        	;Clear old bckVectorFrameID directory and bckVectorIndex
        	(resize bckVectorFrameID recordCount )
        	(resize bckVectorIndex 0)
            (loop for i from 0 until recordCount do
            	(setq record (refObjVector rowVector i))
            	(setAttributes record #void)
            	(setq value (setq myRepos[frame: #void] record))
            	(setObjVector bckVector i value)
            	(setAttributes record colVector)
            	(setq bckVectorIndex[value] i)
            	(setNumVector bckVectorFrameID i value)
            	(__insertIntoIndices value (refObjVector rowVector i))
            ); i
            ;; Close all open indices
            (setq len (length indexDictionary))
            (loop for i from 0 until len do
            	(setq indexStruct indexDictionary[i 1])
            	(if (<> indexStruct #void) (setq indexStruct.frameID (indexStruct.index.Pv.close)))
            );i
            end)
        else
        	(begin ;disabled indices
            (loop for i from 0 until recordCount do
            	(setq record (refObjVector rowVector i)) 
            	(setAttributes record #void)
            	(setNumVector bckVector i (setq myRepos[frame: #void] record))
            	(setAttributes record colVector)
            ); i
			;delete indices
			(if (<> indexDictionary #void)
				(setq len (length indexDictionary))
				(loop for i from 0 until len do
					(setq indexStruct indexDictionary[i 1].index)
					(if (<> indexStruct #void) (indexStruct.index.Pv.drop))
				);i
			);if
			(setq indexDictionary #void)
			end)
		);if	        	

		(setq bckVectorKey (setq myRepos[frame: #void] bckVector)) 
	
		(if enableIndicesSW 
			(begin
			(if  (and (<> indexDictionary #void) (> (length indexDictionary) 0))
				(begin
	
				;make a copy of the index object references
				(setq copyOfIndexObjects (new Vector: object:))
				(setq len (length indexDictionary))
				(loop for i from 0 until len do
					(setq copyOfIndexObjects[i] indexDictionary[i 1].index)
					(setq indexDictionary[i 1].index #void)
				);i 
	
				; save the index dictionary sans index object references
				(setq indexDictionaryKey (setq myRepos[frame: indexDictionaryKey] indexDictionary))
	
				;restore index object references
				(loop for i from 0 until len do
					(setq indexDictionary[i 1].index copyOfIndexObjects[i])
				);i
				end)
			);if
			end)
		else
			(setq indexDictionaryKey #void)
		);if
		
		(setq myRepos[myObjectName] (objectToStructure mySchema mySelf.Pv)) 
	
		;; reset the bckVector to reflect memory cursor usage (ie: full records)
		(setq bckVector rowVector)
	
		(myParent.__commitTrans myExtIndex)
		(SETTIME)
	    true) ;; end of save 


;**************** saveView ******************
    (defun saveView(key)
    ;; *******************************************************************
    ;; Summary: Saves the current record view as specified.
    ;; *******************************************************************
		(STARTTIME posttime:)
		(if (not myObjectState) (__errorState)); make sure object has been initialized
		(setq myDirtyFlag true)
		(setq viewDirectory[key] (copy rowVector))
		(SETTIME)
		true) ;; end of saveView

;****************** search ********************
    (defun search(findLambda ...)
    ;; *******************************************************************
    ;; Summary:  Returns the row index of the first record for which a lambda predicate is true.
    ;; *******************************************************************
       vars:(startIndex rowIndex n colName colValue (argc 2) record)
       (STARTTIME posttime:)
       (if (not myObjectState) (__errorState)); make sure object has been initialized
       ;; Define the temporary search lambda.
       (defun __searchLambda(x) vars:(colName colValue) (= x[colName] colValue))
       ;; If the findLambda is a columnName, we must make our own lambda predicate.
       (if (not (isLambda findLambda))
           (begin
              (setq colName findLambda)
              (setq colValue (argFetch 1))
              (++ argc)	
              (setq findLambda __searchLambda)
              (setq findLambda.Tv.colName colName)
              (setq findLambda.Tv.colValue colValue)
           )) ; end if

       ;; Make sure we capture the startIndex argument (if any).
       (if (= (argCount) argc)
           (setq startIndex (argFetch (sub1 argc)))
           (setq startIndex 0)
           ) ; end if
       (if (<= recordCount startIndex) (begin (SETTIME) (return false)))

       ;; Select the first row for which the find Lambda returns true.
       (setq n (length rowVector))
       (if (= myCursorType memory:)
	       (loop for rowIndex from startIndex until n do 
	           (setq record (refObjVector rowVector rowIndex))
	           (if (findLambda record) 
	           		(begin
	           		(SETTIME)
	           		(return rowIndex)
	           		end))
	           ) ;; end loop
		else
	       (loop for rowIndex from startIndex until n do 
	           (setq record (__read rowIndex))
	           (if (findLambda record) 
	           		(begin
	           		(SETTIME)
	           		(return rowIndex)
	           		end))
	           ) ;; end loop
		);if
		(SETTIME)
       	false) ;; end search

;**************** setImport *****************
    (defun setImport(row recordVector)
    ;; **********************************************************************
    ;; Summary:  Receives a single record from the global importTab function.
    ;; **********************************************************************
		regs:(n N)
        vars:(temp)
		(STARTTIME posttime:)
		(if (not myObjectState) (__errorState)); make sure object has been initialized
		(setq myDirtyFlag true)
		;; Make sure we are not a static cursor
		(if (= myCursorType static:)
			(__error (append "setImport: " myObjectName 
							" :An attempt was made to setImport on a static cursor")))
		
		;; Convert the imported record from comma delimited format (if necessary).
		(if (or (isString recordVector) (isByteVector recordVector))
            (begin
		      (setq recordVector (stringToVector recordVector "," true true))
		      (setq N (length recordVector))
		      (loop for n from 0 until N do
                 (setq temp (parse recordVector[n]))
                 (cond
                  ((isVector temp)(setq recordVector[n] (string temp true)))
                  ((isStructure temp)(setq recordVector[n] (string temp true)))
                  ((isDirectory temp)(setq recordVector[n] (string temp true)))
                  ((isDictionary temp)(setq recordVector[n] (string temp true)))
			      (else (setq recordVector[n] temp))
                 ) ; end cond
		      ) ; end loop
		   )) ;; end if
		
		;; Import the column header record (if necessary).
		(if (= row 0)
		   (if (= recordCount 0) ;; If this is an empty table then create the table header information
		       (begin
		          (setq validFields (objectToStructure recordVector #(#void)))
		          (setq colVector (refAttributes validFields))
		          (if (= (length colVector) 0)
		              (__error (append "importTab: " myObjectName " :An attempt was made to import a file with missing column names: ")))
		          (setq recordStructure (new Vector: (length colVector)))
		          (setAttributes recordStructure colVector) 
		          (setq colCount (length colVector))
		          (SETTIME)
		          (return true)) ;; end setting new column names.
		       (begin ;; check new column names against old column names
		          (setq recordVector (objectToStructure recordVector #(#void)))
		          (setq recordVector (refAttributes recordVector))
		          (if (<> colVector recordVector) 
		              (__error 
		                 (append "importTab: " myObjectName " :An attempt was made to import a file with mismatched column names: ")))
		          (SETTIME)
		          (return true)
		       end) ;; end checking new column names against old column names.
		   )) ;; end if

		;; Import all other data records at the end of the table.
		(write recordCount recordVector)
		(SETTIME)
		true) ;; end of setImport


;************** sharpe *****************
    (defun sharpe(totalLambda)
    ;; *******************************************************************
    ;; Summary:  Returns the sharpe ratio of a lambda value on each 
    ;;           record of a table.
    ;; *******************************************************************
    ;;tm! To add our new column stats the signature of this function would have to 
    ;;tm! change to allow the argument to be either an Lambda or a column name. This would
    ;;tm! not break any existing code because we can check if the argument is an Lambda. Of 
    ;;tm! course, passing an Lambda would require a full iteration.
		vars:(rowIndex (result 0) score aSum aSsq anAvg aStd)
		(STARTTIME posttime:)
		(if (not myObjectState) (__errorState)); make sure object has been initialized
		(if (<= recordCount 0) (begin (SETTIME) (return 0)))
		;; Compute the stadard devisation lambda value for each record in the table.
		(loop for rowIndex from 0 until recordCount do
		   (setq score (totalLambda (__read rowIndex)))
		   (setq aSum (+ aSum score))
		   (setq aSsq (+ aSsq (* score score)))
		   ) ;; end loop
		(setq anAvg (/ aSum recordCount))
		(setq aStd (sqrt (- (/ aSsq recordCount) (* anAvg anAvg))))
		(setq result (/ anAvg aStd))
		(SETTIME)
		result) ;; end sharpe


;******************** show ******************
    (defun show(startIndex) 
    ;; *******************************************************************
    ;; Summary:  Shows a group of records starting from the specified row.
    ;; *******************************************************************
    ;;tm! It would be nice to be able to pass a second argument that is the
    ;;tm! name of the output Lambda. The default would be writeln. This would
    ;;tm! let you dress the output in html etc. 
		vars:(i n)
		(STARTTIME posttime:) 
		(if (not myObjectState) (__errorState)); make sure object has been initialized
		(setq n (integer (min recordCount (addi startIndex showLimit))))
		(loop for i from startIndex until n do
		   (writeln "[" i "] " (__read i))
		) ;; end loop
		(SETTIME)
		true) ;; end show


;*************** sort ***********************
    (defun sort(sortLambda ...)
	;; *******************************************************************
	;; Summary:  Sorts each record in the table cursor.
	;; *******************************************************************
		vars:(backupSW numArgs i)
		(STARTTIME posttime:)
		(if (not myObjectState) (__errorState)); make sure object has been initialized
		
		; set optional argument defaults
		(setq backupSW false)
		
		; verify optional arguments
		(setq numArgs (argCount))
		(loop for i from 1 until numArgs do
			(cond
			((= i 1) 
				(if (<> (argFetch 1) backup:) 
					(__error (append "sort: Second argument must be symbol backup: '" (argFetch 1) "' was passed"))
					(setq backupSW true))
			)
			(true (__error "sort: Too many arguments"))
			);cond
		);i
		
		;; Are we sorting the table as well as the current view?
		(if backupSW
			(begin
			;; Make sure we are not a static cursor
			(if (= myCursorType static:)
				(__error (append "sort: " myObjectName " :An attempt was made to sort the backup view on a static cursor")))

			(if (and (= myObjectType metaTable:) (<> myCursorType memory:))
				(__error (append "sort: " myObjectName 	" :An attempt was made to sort the backup view on a metaTable")))

			(setq backupSW true)
			(setq myDirtyFlag true)
			(__reset);;tm!! This blows away views.
			end) ; end
		) ; end if
		
		(if (<= recordCount 0) (begin (SETTIME) (return recordCount)))
		
		(if (or (= myCursorType disk:) (= myCursorType static:))
			(begin
			(if backupSW
				(__clearBuffer))
			
			(setq mySortLambda sortLambda); save the sort Lambda so the sort wrapper can find it
			(setq __sortWrapper._oldFx #void)
			(setq __sortWrapper._oldFy #void)
			(^sort rowVector __sortWrapper)
		
			(if (and backupSW myBuf.enableIndicesSW)
				(__updateBckVectorIndex))
			end)
		else ;handle memory cursor
			(begin
			(if (and backupSW enableIndicesSW)
				(begin ; save row frameIDs in CDR of row objects during sort
				(loop for i from 0 until recordCount do
					(setCdr (refObjVector bckVector i) (refNumVector bckVectorFrameID i))
				);i
				end)
			);if
		
			(^sort rowVector sortLambda) ;note that if the backupSw was set that rowVector points to the same object vector
										;that bckVector does
			(if (and backupSW enableIndicesSW)
				(begin ; restore frameIDs to bckVectorFrameID vector from CDRs in row objects
				(loop for i from 0 until recordCount do
					(setq value (cdr (refObjVector bckVector i))) ;get the frameID out of the row objects CDR 
					(setNumVector bckVectorFrameID i value) ; bckVectorFrameID is a number vector storing the frameIDs for the row objects in bckVector
					(setq bckVectorIndex[value] i) ;update bckVectorIndex directory with new indices into bckVector
					(setCdr (refObjVector bckVector i) #void)
				); i
				end)
			);if
			end)
		);if
		
		(if (and (= backupSW true) (= myCursorType disk:)) (__syncUp))
		
		(SETTIME)
		recordCount) ;; end sort


;****************** tile ******************
    (defun tile(tileCount tileIndex)
    ;; *******************************************************************
    ;; Summary:  ;; Truncates a table to those records which are in the nth of N tiles.
    ;; *******************************************************************
		vars:(tileSize n N vec startIndex endIndex)
		(STARTTIME posttime:)
		(if (not myObjectState) (__errorState)); make sure object has been initialized
		(if (<= recordCount 0) (begin (SETTIME) (return recordCount)))
		(if (= myCursorType memory:)
				(setq rowVector (copy rowVector))) ;;tm! copy in case rowVector was pointing at bckVector
		;; Return only those records in the nth tile of N tiles.
		(setq tileCount (abs tileCount))
		(setq tileIndex (min tileCount (abs tileIndex)))
		(setq tileSize (/ recordCount tileCount))
		(setq startIndex (min recordCount (max 0 (integer (* tileSize (sub1 tileIndex)))))) 
		(setq endIndex (min recordCount (integer (+ tileSize startIndex)))) 
		(setq vec (new Vector: object: (- endIndex startIndex)))
		(loop for n from startIndex until endIndex do
		   (setq vec[(subi n startIndex)] rowVector[n])
		   ) ; end loop
		(setq rowVector vec)
		
		;; Return the number of records.
		(setq recordCount (length rowVector))
		(SETTIME)
		recordCount) ;; end tile

;******************** total ***************
    (defun total(totalLambda)
    ;; *******************************************************************
    ;; Summary:  Totals a lambda value on each record of a table.
    ;; *******************************************************************
    ;;tm! This is really a much more general routine than the name suggests.
    ;;tm! It would be possible to perform any kind of function on the rows
    ;;tm! of the table. It might be better to have a general purpose function
    ;;tm! called iterate(). This could be further enhanced by allowing two
    ;;tm! additional parameters specifying the iteration range eg: BegRow, EndRow
		vars:(result)
		(STARTTIME posttime:)
		(setq result (__total totalLambda))
		(SETTIME)
		result) ;; end total

    (defun __total(totalLambda); internal 
		vars:(rowIndex result)
		(if (not myObjectState) (__errorState)); make sure object has been initialized
		(if (<= recordCount 0) (begin (SETTIME) (return 0)))
		;; Compute the total lambda value for each record in the table.
		(loop for rowIndex from 0 until recordCount do
		   (+= result (totalLambda (__read rowIndex)))
		) ;; end loop
		result) ;; end total


;*************** totalForAll *******************
    (defun totalForAll(totalLambda)
    ;; *******************************************************************
    ;; Summary:  Totals a lambda value on ALL records of a table.
    ;; *******************************************************************
    ;;tm! This is a really badly named function. The comments about an iterate function also apply.
		vars:(result)
		(STARTTIME posttime:)
		(setq result (__totalForAll))
		(SETTIME)
		result) ;; end totalForAll

    (defun __totalForAll(totalLambda) ;internal with no timer
		vars:(lrowIndex result buffer record len)
		(if (not myObjectState) (__errorState)); make sure object has been initialized
		(setq len (length bckVector))
		(if (<= len 0) (begin (SETTIME) (return 0)))
		;; Compute the total lambda value for each record in the table.
		(if (= myCursorType memory:)
		   (loop for rowIndex from 0 until len do
					(+= result (totalLambda (refObjVector bckVector rowIndex)))
				) ;; end loop
		else; disk or static cursor 
			(begin
			(setq buffer myBuf.buffer)
				(loop for rowIndex from 0 until len do
					(setq record (refObjVector buffer rowIndex))
					(if (= record #void)
		   			(setq record myRepos[frame: (refNumVector bckVector rowIndex)])) ; read from disk
		   		(setAttributes record colVector)
		   		(+= result (totalLambda record))
		   	) ;; end loop
		    end)
			);if
		result) ;; end totalForAll

;****************** truncate ******************
    (defun truncate(findLambda)
    ;; *******************************************************************
    ;; Summary:  Truncates rows from the current view for those records for which a lambda predicate is true.
    ;; *******************************************************************
		vars:(rowIndex newIndex n vec maxRecords)
		(STARTTIME posttime:)
		(if (not myObjectState) (__errorState)); make sure object has been initialized
		(if (<= recordCount 0) (begin (SETTIME) (return recordCount)))
		(if (= myCursorType memory:)
				(setq rowVector (copy rowVector))) ;;tm! copy in case rowVector was pointing at bckVector
		;; If the selectLambda is a number, then keep only the first N records.
		(if (isNumber findLambda)
		   (begin
		      (setq maxRecords (min recordCount findLambda))
		      (setq rowVector (resize rowVector maxRecords))
		      (setq recordCount (length rowVector))
		      (SETTIME)
		      (return recordCount)
		   )) ; end if
		
		(setq newIndex -1)
		
		(if (= myCursorType memory:)
			(begin
			(setq vec (new Vector: object: 0))
			(setq n recordCount)
			(loop for rowIndex from 0 until n do
				(if (findLambda (refObjVector rowVector rowIndex)) 
					(setq vec[(setq newIndex (iadd newIndex 1))] (refObjVector rowVector rowIndex)))
			) ;; rowIndex
			end)
		else
			(begin
			(setq vec (new Vector: integer: 0))
			(setq n recordCount)
			(loop for rowIndex from 0 until n do
				(if (findLambda (__read rowIndex)) 
					(setq vec[(setq newIndex (iadd newIndex 1))] (refIntVector rowVector rowIndex)))
			) ;; rowIndex
			end)
		);if
		
		(setq rowVector vec)
		(setq recordCount (length rowVector))
		(SETTIME)
		recordCount) ;; end truncate

;**************** updateView *******************
    (defun updateView(updateLambda ...)
	;; *******************************************************************
	;; Summary:  Updates each record in the current view and restores the altered backup view.
	;; *******************************************************************
		vars:(_rowVector rowIndex n resetSW i numArgs result)
		(STARTTIME posttime:)
		(if (not myObjectState) (__errorState)); make sure object has been initialized
		(setq myDirtyFlag true)
		
		(if (= myCursorType static:)
			(__error (append "updateView: " myObjectName " :An attempt was made to update a view on a static cursor")))
		(if (= myCursorType disk:)
			(__error (append "updateView: " myObjectName " :An attempt was made to update a view on a disk cursor")))


		;set optional argument defaults
		(setq resetSW true)
		
		;verify each optional argument
		(setq numArgs (argCount))
		(loop for i from 1 until numArgs do
			(cond
			((= i 1)
				(if (<> (argFetch 1) noreset:)
					(__error (append "updateView: Second argument must be the symbol noreset: '" (argFetch 1) "' was passed."))
					(setq resetSW false))
			)
			(true (__error "updateView: Too many or bad arguments passed."))
			);cond
		);i
		
		;; Update each row in the current view.
		(setq _rowVector (copy rowVector))
		(setq n (length _rowVector))
		
		(if enableIndicesSW
			(begin
			(__disableIndices)
			(loop for rowIndex from 0 until n do
				(updateLambda (__read rowIndex))
			) ;; end loop   
			(__enableIndices reindex:) ; causes reindex
			end)
		else
			(begin
			(loop for rowIndex from 0 until n do
				(updateLambda (refObjVector _rowVector rowIndex))
			) ;; end loop
			end)
		);if
		
		(setq result (if (= resetSW true) (__restore) cursor))
		(SETTIME)
		result) ;; end updateView

;**************** viewMath ******************      
    (defun viewMath(operator key1 key2)
    ;; *******************************************************************
    ;; Summary: Combines two saved views using a mathematical operator.
    ;; *******************************************************************
    ;;tm! This is a strange name for this function. I suggest we have a new, much more powerful,
    ;;tm! join() function. join() would use the new schema information and indices we are planning to 
    ;;tm! generate an optimized query.
    ;;tm! This routine should check that the views specified by the keys exist and return an error if they don't.
		vars:(vecType)
		(STARTTIME posttime:)
		(if (= myCursorType memory:) (setq vecType object:) (setq vecType integer:))
		(if (not myObjectState) (__errorState)); make sure object has been initialized
		(if (= operator and:) 
		   (setq rowVector (myParent.__viewAND (new Vector: vecType 0) viewDirectory[key1] viewDirectory[key2])))
		(if (= operator or:) 
		   (setq rowVector (myParent.__viewOR (new Vector: vecType 0) viewDirectory[key1] viewDirectory[key2])))
		(if (= operator xor:) 
		   (setq rowVector (myParent.__viewXOR (new Vector: vecType 0) viewDirectory[key1] viewDirectory[key2])))
		(setq recordCount (length rowVector))
		(SETTIME)
		recordCount) ;; end of viewMath



    
;************** write ***********************    
    (defun write(row record)
    ;; *******************************************************************
    ;; Summary:  Write a record to the table for update.
    ;; Args:     row      Row number of row to be written to the table.
    ;;           record   Record to be written to the table.
    ;; Return:   record   The record just written. 
    ;; *******************************************************************
		vars:(record)
		(STARTTIME posttime:)	
		(if (not myObjectState) (__errorState)); make sure object has been initialized


		;; Make sure we are not a static cursor
		(if (= myCursorType static:)
			(__error (append "write: " myObjectName " :An attempt was made to write a record on a static cursor")))

		(setq record (__write row record #void)); pass a void table key if not writing on a meta table
		
		(SETTIME)
		record) ;; end of write



;************** writeBySJKey ***********************    
    (defun writeBySJKey(argTableKey record)
    ;; *******************************************************************
    ;; Summary:  Write a record to the table for update.
    ;; Args:     row      Row number of row to be written to the table.
    ;;           record   Record to be written to the table.
    ;; Return:   record   The record just written. 
    ;; *******************************************************************
		vars:(record)
		(STARTTIME posttime:)	
		(if (not myObjectState) (__errorState)); make sure object has been initialized
		(if (<> myObjecType metaTable:)
			(__error (append "writeBySJKey: " myObjectName " :An attempt was made to writeBySJKey on non meta table")))

		;; Make sure we are not a static cursor
		(if (= myCursorType static:)
			(__error (append "writeBySJKey: " myObjectName " :An attempt was made to write a record on a static cursor")))

		(setq row (getRecordRowBySJKey argTableKey record[sjRowKeyCol]))
		(if (<> row #void)
			(__write row record argTableKey)
			(__error (append "writeBySJKey: " myObjectName " :Could not find record to update.")))

		(SETTIME)
		record) ;; end of write


    ;; -------------------------------------
    ;; Private methods (not for public use).
    ;; -------------------------------------
;;;************* __buildBckVectorIndex ***********
    (defun __buildBckVectorIndex ()
    vars: (i value)
    (if (= myCursorType memory:)
    	(begin
		(setq bckVectorIndex (new Directory:))
		(setq bckVectorFrameID (new Vector: number: recordCount))
		(loop for i from 0 until recordCount do
			(setq value (__makeFakeFrameID))
			(setq bckVectorIndex[value] i)
			(setNumVector bckVectorFrameID i value)
		);i
    	end)
    else ; disk or static cursor
    	(begin
    	(^resize bckVectorIndex 0)
    	(loop for i from 0 until recordCount do
    		(setq bckVectorIndex[(refNumVector bckVector i)] i)
    	);i
    	end)
    );if
    
    true)


;************** __clear ******************
    (defun __clear()
       	(if (not myObjectState) (__errorState)); make sure object has been initialized
    	(if (= myCursorType memory:)
    		(begin
    		(setq rowVector (new Vector: object: 0))
    		(setq bckVector rowVector)
    		end)
    	else ; disk: or static: cursor
    		(begin
    		;;
    		end) 
    	);if
    	
       	(setq fileID #void)
	    (setq myRepos #void)
	    (setq myExtIndex #void)
	    (setq mySchema #void)
	    (setq myDatabaseName #void)
	    (setq myObjectName #void)
	    (setq myTableScore #void)
	    (setq myCursorNotes (new Dictionary:))
	    (setq viewDirectory (new Directory:))
	    (setq memberTableDirectory (new Directory:))
	    (setq memberTableExtents (new Vector: integer: 0))
	    (setq sjIndex #void)
	    (setq recordCount 0)
	    (setq colCount 0)
       true) ;; end of __clear


;*********** __close **********************
    (defun __close(...)
    ;; *******************************************************************
    ;; Summary:  Terminates an update transaction on the table cursor.
    ;; Args: 	
    ;;		save:		Save disk: cursor memopad even if other data has not been changed
    ;; *******************************************************************
		vars:(i j cursorIndex numCursors myNoCommitOption mySaveOption cursorType copyOfIndexDictionary len
			    metaTableExtent metaTableRepos metaTableSchema numTables makeMemberSW memberExtentsClosed memberTableExtent)
		(STARTTIME posttime:)
		(if (not myObjectState) (__errorState)); make sure object has been initialized

		
		;; Only disk cursors can have save: or nocommit: argument
		(if ( and (> (argCount) 0) (<> myCursorType disk:)) 
			(__error (append "__close: argument " (argFetch 0) " passed on memory or static cursor" )))
		
		(setq mySaveOption false); default
		(setq myNoCommitOption false); default
		(setq makeMemberSW false); default
		(loop for i from 0 until (argCount) do
			(cond
			 	((= (argFetch i) save:) (setq mySaveOption true))
			 	((= (argFetch i) nocommit:) (setq myNoCommitOption true))
			 	((= (argFetch i) makeMember:) (setq makeMemberSW true))
			 	((true) (__error (append "__close: argument " (argFetch i) " is invalid" )))
			);
		); i


		(if makeMemberSW (setq myDirtyFlag true)); for save of changes make by appendToMetaTable Lambda
		
		;; if we are not a memory cursor then find out how many non-memory cursors are open on this table
		(setq numCursors 0)
		(if (<> myCursorType memory:) 
			(loop for i from 0 until (length openTables[myObjectName].cursorList) do
				(if (<> openTables[myObjectName].cursorList[i].myCursorType memory:) (++ numCursors))
			); i
		);if
		
		; Cursor specific close activity
		(cond
			((= myCursorType memory:)
				(begin
				; No action required
				
				end)
			)
			((= myCursorType disk:)
				(begin
					(setq rowVectorKey #void) ; rowVector is no longer stored in repository! Make sure it is void.

					; Note that we have to save the viewDirectory here as any reindexing will blow
					; the views away.
					(if myDirtyFlag ; general save
						(begin
						(setq bckVectorKey (setq myRepos[frame: bckVectorKey] bckVector)) 
						(setq viewDirectoryKey (setq myRepos[frame: viewDirectoryKey] viewDirectory))

						;; Is this a meta table?
						(if (= myObjectType metaTable:)
							(begin   							
							(setq memberTableDirectoryKey (setq myRepos[frame: memberTableDirectoryKey] memberTableDirectory))
							(setq sjIndexKey (setq myRepos[frame: sjIndexKey] sjIndex)) 
							end)
						);if						
						
						
						;; Is this a member table?
						(if (and (<> metaTable #void) (not makeMemberSW)) ;skip this step if this table is being made a member table
							(begin ; Update the dirty flag in the metaTable
							(setq metaTableExtent (myParent.getTableExtent metaTable))
							(setq metaTableRepos myParent.dmExt[metaTableExtent].repos)
							(myParent.__beginTrans metaTableExtent)
							(setq metaTableSchema metaTableRepos[metaTable])
							(setq memberTableDirectory metaTableRepos[frame: metaTableSchema.memberTableDirectoryKey])

							(setq numTables (length memberTableDirectory))
							(loop for i from 0 until numTables do
								(begin
								(if (= myObjectName memberTableDirectory[i 1].tableName)
									(begin
									(setq memberTableDirectory[i 1].dirty true)
									(goto DONEWITHMEMBER:)
									end)
								);if  
								end)
							);i  
							DONEWITHMEMBER::
							(setq metaTableSchema.memberTableDirectoryKey (setq metaTableRepos[frame: metaTableSchema.memberTableDirectoryKey] memberTableDirectory))
							(setq metaTableRepos[metaTable] metaTableSchema)
							(myParent.__commitTrans metaTableExtent)
							end)
						);if
						
						end);myDirtyFlag
					);if
		
					(if myBuf.enableIndicesSW
						(begin ; make sure each existing index is up to date
						(__indexContent)
						end)
					else ;re-index cursors if they are disabled and table is dirty
						(if myDirtyFlag
							(begin 
							(__enableIndices reindex:)
							(__disableIndices)					
							end)
						);if
					);if
											
					;Close indices if necessary
					(if (= numCursors 1) ;close indices if this is the last cursor
						(loop for i from (isub (length indexDictionary) 1) to 0 by -1 do
							(if myDirtyFlag 
								(begin
								(setq indexDictionary[i 1].frameID (indexDictionary[i 1].index.Pv.close))
								end)
							else
								(begin
								(indexDictionary[i 1].index.Pv.close noSave:)
								end)
							);if
						);i
					else ; save indices if this is not the last cursor (leave the index for use by static cursors!)
						(if myDirtyFlag
							(loop for i from (isub (length indexDictionary) 1) to 0 by -1 do
								(setq indexDictionary[i 1].frameID (indexDictionary[i 1].index.Pv.save))
							);i
						);if
					);if
		
					(if myDirtyFlag ; general save
						(begin
		
						;Special handling of indexDirectory to ensure that we do not save instances of index objects pointed
						;to by the indexDictionary[i 1].index variable.
			
						(if myBuf.enableIndicesSW
							(begin
							(setq copyOfIndexDictionary (copy indexDictionary))
							(setq len (length indexDictionary))
							(loop for i from 0 until len do
								(setq indexDictionary[i 1].index #void)
							);i
		
							(setq indexDictionaryKey (setq myRepos[frame: indexDictionaryKey] indexDictionary))
							(loop for i from 0 until len do
								(setq indexDictionary[i 1].index copyOfIndexDictionary[i 1].index)
							);i
							end)
						else
							(setq indexDictionaryKey #void)
						);if
		
						(setq myRepos[myObjectName] (objectToStructure mySchema mySelf.Pv)) 
						end)
					else
						(if mySaveOption ; force save of memoPad even if rest of table is not dirty
							(begin
							;;tm!!! Note that the memo pad is in the schema -- probably a bad idea
							;; if someone stored big stuff in it we get hit when loading any kind of cursor!
							
							(setq myRepos[myObjectName] (objectToStructure mySchema mySelf.Pv)) 
							end)
						);if
					);if       
					
					
				(if (= numCursors 1); remove the table entry from openBufs 
					(begin
					(^delete openBufs myObjectName))
					(setq myBuf #void)
					end)

				(if (not myNoCommitOption) 
					(begin
					(myParent.__commitTrans myExtIndex)
					;close transactions on repositores member tables reside in
					(setq numTables (length memberTableDirectory))
					(setq memberExtentsClosed (new Directory:))
					(loop for j from 0 until numTables do
						(setq memberTableExtent (myParent.getTableExtent memberTableDirectory[j 1].tableName))
						(if (and (not (isMember memberTableExtent memberExtentsClosed)) (<> memberTableExtent myExtIndex))
							(begin
							(myParent.__commitTrans memberTableExtent)
							(setq memberExtentsClosed[memberTableExtent] true)
							end)
						);if
					);j
					end)
				);if

				end);disk cursors
			)
			((= myCursorType static:)
				(begin
				(if (= numCursors 1); remove the table entry from openBufs
					(begin
					(^delete openBufs myObjectName))
					(setq myBuf #void)
					end)
				end)
			)
		);cond	
		
		;; Clear the cursor object.
		(setq cursorIndex (inside mySelf openTables[myObjectName].cursorList))
		(if (>= cursorIndex 0)(setq openTables[myObjectName].cursorList (^delete openTables[myObjectName].cursorList cursorIndex)))
		
		; if no more cursors are open on table then remove it from openTables 
		(if (= (length openTables[myObjectName].cursorList) 0)
		   	(setq openTables[myObjectName] #void)
			);if
			
		(__clear)
		
		(setq myObjectState false) ;; prevent any further calls to this cursor 
		(SETTIME)
	    (POSTTIMES mySelf myCursorType "")
		true) ;; end of __close


;****************** __clearBuffer **********************
    (defun __clearBuffer()
    ;; *******************************************************************
    ;; Summary:  clearBuffer
    ;; *******************************************************************
    	vars:(j i buffer)
		;;Clear out buffers              
    	(if (not myObjectState) (__errorState)); make sure object has been initialized
		(setq buffer myBuf.buffer)

   		(if (> myBuf.bufUsed 0)
   			(begin                
   			(setq i myBuf.bufTail)
   			(while (<> i myBuf.bufHead)
   				(setq record (refObjVector buffer i))
   				(setq j (integer (floor (cdr record))))
   				(setObjVector buffer i #void)
   				(setq i j)
   			);while
   			(setObjVector buffer i #void)
   			end)
   		);if
   		(setq myBuf.bufHead #void)
   		(setq myBuf.bufTail #void)
   		(setq myBuf.bufUsed 0)
       	true) ;; end of clearBuffer

;************** __errorStop ******************
    (defun __errorStop(errMsg)
       (if (not myObjectState) (__errorState)); make sure object has been initialized
       (if (isNumber fileID) (fileClose fileID 1))
       (setq fileID #void)
       (__error (mid errMsg 1 (subi (length errMsg) 2)))) ;; end __errorStop

;*********** __getEnableIndicesSW ******************
    (defun __getEnableIndicesSW ()
	; Return the current enableIndicesSW from the appropriate location. For memory: cursors the enableIndicesSW is
	; a local pvar. For disk and static cursors the enableIndicesSw is in dataMineLib.openBufs[tablename].enableIndicesSW
	(if (= myCursorType memory:)
		(return enableIndicesSW)
		(return myBuf.enableIndicesSW))
    )


;*********** __getIndexDirtySW ******************
    (defun __getIndexDirtySW ()
	; Return the current indexDirtySW from the appropriate location. For memory: cursors the indexDirtySW is
	; a local pvar. For disk and static cursors the indexDirtySW is in dataMineLib.openBufs[tablename].indexDirtySW
	(if (= myCursorType memory:)
		(return indexDirtySW)
		(return myBuf.indexDirtySW))
    )

;************ __getRecordRepos ***************
    (defun __getRecordRepos (row)
		vars:(repos numTables i )   
		;find argTableKey by inspection
		(setq numTables (length memberTableOffsets))
		(loop for i from (- numTables 1) to 0 by -1 do
			(if (>= row memberTableOffsets[i]) 
				(return (setq repos myParent.dmExt[memberTableExtents[i]].repos)))
		);i 				
	(__error "__getRecordRepos: Failed to find extent for row")); __getRecordRepos

;*********** __indexContent ********************
    (defun __indexContent (...)
    ;; Index any indices with a state of OutOfDate: or all indices if the optional all: argument was supplied
   		vars:(indexList i len rowIndex numRows record numArgs indexAllSW key value)
;		(STARTTIME posttime:)
		;Set defaults for optional aguments
		(setq indexAllSW false)

		;Check for optional arguments
		(setq numArgs (argCount))
		(loop for i from 0 until numArgs do
			(cond
				((and (= i 0) (= (argFetch 0) all:)) (setq indexAllSW true))
				(true (__error "__indexContent: accepts only the all: optional argument. "))
			);cond
		); i

	    ;get list of indices to index
	    (setq indexList (new Vector:))
	    (setq len (length indexDictionary))
	    (loop for i from 0 until len do
	    	(if (or indexAllSW (= indexDictionary[i 1].state OutOfDate:))
	    		(setq indexList[(length indexList)] indexDictionary[i 1]))
	    );i 
		(setq len (length indexList))
		(if (= len 0) (return true)); return as there are no out of date indices

		;clear the content of each index in indexList
		(loop for i from 0 until len do
			(indexList[i].index.Pv.clear); clear existing keys from index
		);

		;iterate over bckVector inserting keys into each index in indexList
		(setq numRows (length bckVector))  
		(loop for rowIndex from 0 until numRows do
			(setq record (__read rowIndex bckVector:))
			; insert new key into each index
			(loop for i from 0 until len do 
				(if (= indexList[i].noUpdating false)
					(begin
					(setq key (indexList[i].getKey record))
					(if (= myCursorType memory:)
						(setq value (refNumVector bckVectorFrameID rowIndex))
					else
						(setq value (refNumVector bckVector rowIndex))
					);if
					(setq indexList[i].index[key] value)
					end)
				);if
			);i
		); rowIndex
    
    	;Set each updated index's state to current
		(loop for i from 0 until len do
			(setq indexList[i].state Current:)
		);i
;		(SETTIME)
	    true)

;************ __setEnableIndicesSW *******************
    (defun __setEnableIndicesSW (arg)
	(if (= myCursorType memory:)
		(setq enableIndicesSW arg)
		(setq myBuf.enableIndicesSW arg))
	arg)

;************ __setIndexDirtySW *******************
    (defun __setIndexDirtySW (arg)
	(if (= myCursorType memory:)
		(setq indexDirtySW arg)
		(setq myBuf.indexDirtySW arg))
	arg)
                          
;**************** __syncUp ********************
    (defun __syncUp()
    ;; *******************************************************************
    ;; Summary:  Synchronizes all open static cursors of this data mine table. 
    ;; *******************************************************************
		vars:(cursorVector cursorIndex scursor)
;		(STARTTIME posttime:)		
		(if (not myObjectState) (__errorState)); make sure object has been initialized
		;; Reset any open static cursors on this table.
		(setq cursorVector openTables[myObjectName].cursorList)
		(loop for cursorIndex from 0 until (length cursorVector) do
		  (setq scursor openTables[myObjectName].cursorList[cursorIndex])
		  (if (= scursor.myCursorType static:) (scursor.reset))
		  ) ; end loop
;		(SETTIME)
		true) ;; end of __syncUp

;************** __sortWrapper **********************
    (defun __sortWrapper(fx fy)
    ;; *******************************************************************
    ;; Summary:  Wrapper for sort Lambda during sort of a table.
    ;; NOTE: this Lambda is only used for disk cursors!!@!!
    ;; *******************************************************************
      vars:(_bckrx bckry)
       pvars:(buffer _oldFx _oldFy _rx _ry)
       (if (not myObjectState) (__errorState)); make sure object has been initialized
       (if (<> _oldFx fx) 
           (begin
              (setq _oldFx fx)       
              (setq buffer myBuf.buffer)
    		  (setq _bckrx (refIntVector rowVector fx))
    		  (if (= (setq _rx (refObjVector buffer _bckrx)) #void) ;try and read from buffer
    		  		(setq _rx myRepos[frame: (refNumVector bckVector _bckrx)])) ; otherwise read from disk 
              (setAttributes _rx colVector)
           )) ; end if
       (if (<> _oldFy fy) 
           (begin
              (setq _oldFy fy)
              (setq buffer myBuf.buffer)
              (setq _bckry (refIntVector rowVector fy))
              (if (= (setq _ry (refObjVector buffer _bckry)) #void) ; try and read from buffer
              		(setq _ry myRepos[frame: (refNumVector bckVector _bckry)])); otherwise read from disk
              (setAttributes _ry colVector)
           )) ; end if
       (mySortLambda _rx _ry)) ;; end __sortWrapper

;******************* __truncateEnds *********************
    (defun __truncateEnds(begRowLimit endRowLimit)
    ;; *******************************************************************
    ;; Summary:  Truncate records from both ends of a table.
    ;; *******************************************************************
		vars:(rowIndex newIndex n vec endRowIndex)
;		(STARTTIME posttime:)
		(if (not myObjectState) (__errorState)); make sure object has been initialized
		(if (<= recordCount 0) (return recordCount))
		;; Select only those rows in between the begin and end row limits.
		(setq n (length rowVector))
		(setq endRowIndex (subi n endRowLimit))
			(if (= myCursorType memory:) 
				(begin
		  	(setq vec (new Vector: object: recordCount))
			(setq newIndex (integer -1))
			(loop for rowIndex from (addi begRowLimit 1) until endRowIndex do
			   (setObjVector vec (setq newIndex (iadd newIndex 1)) (refObjVector rowVector rowIndex))
			); rowIndex
		  	end)
		else
			(begin
		  	(setq vec (new Vector: integer: 0))
			(setq newIndex (integer -1))
			(loop for rowIndex from (addi begRowLimit 1) until endRowIndex do
				(setIntVector vec (setq newIndex (iadd newIndex 1)) (refIntVector rowVector rowIndex))
			) ;; end loop
		  	end)
		);if
		(if (> newIndex 0)
				(resize vec newIndex)
				(resize vec 0))
		(setq rowVector vec)
		(setq recordCount (length rowVector))
;		(SETTIME)
		recordCount) ;; end truncateEnds

;******************* _truncateMid *******************
    (defun __truncateMid(begRowLimit endRowLimit)
    ;; *******************************************************************
    ;; Summary:  Truncate records from the center of a table.
    ;; *******************************************************************
		vars:(rowIndex newIndex n vec endRowIndex)
;		(STARTTIME posttime:)
		(if (not myObjectState) (__errorState)); make sure object has been initialized
		(if (<= recordCount 0) (return recordCount))
		;; Delete those rows in between the begin and end row limits.
		(setq n (length rowVector))
		(setq endRowIndex (subi n endRowLimit))
		(setq newIndex (integer -1))
			(if (= myCursorType memory:)
				(begin
		   	(setq vec (new Vector: object: 0))
			(loop for rowIndex from 0 until begRowLimit do
				(setObjVector vec (setq newIndex (iadd newIndex1)) (refObjVector rowVector rowIndex))
			) ;; end loop
		   	end)
		else
			(begin
		   	(setq vec (new Vector: integer: recordCount))
			(loop for rowIndex from 0 until begRowLimit do
				(setIntVector vec (setq newIndex (iadd newIndex 1)) (refIntVector rowVector rowIndex))
			) ;; end loop
			(if (> newIndex 0)
				(resize vec newIndex)
				(resize vec 0))
		   	end)
		);if
		(setq rowVector vec)
		(setq recordCount (length rowVector))
;		(SETTIME)
		recordCount) ;; end truncateMid

;********************* __updateKill *******************
    (defun __updateKill()
    ;; *******************************************************************
    ;; Summary:  Terminate an update transaction and delete the table object.
    ;; *******************************************************************
;		(STARTTIME posttime:)
		(if (not myObjectState) (__errorState)); make sure object has been initialized
		;; Make sure we are not a static cursor
		(if (= myCursorType static:)
			(__error (append "__updateKill: " myObjectName 
							" :An attempt was made to __updateKill on a static cursor")))

		(if (= myObjectType metaTable)
			(__error (append "__updateKill: " myObjectName 
							" :An attempt was made to __updateKill on a metaTable cursor")))


		(if (and (<> metaTable #void) (not lastMemberTableSW))
			(__error (append "__updateKill: " myObjectName 
							" :An attempt was made to __updateKill on a metaTable member other than last member.")))

		;; Drop all of the table records.
		(drop)
		;; Drop the table from the schema.
		(setq myRepos[myObjectName] #void)
;		(SETTIME)
		true) ;; end of __updateKill

;*************** __errorState ***************************
    (defun __errorState()
    ;; *******************************************************************
    ;; Summary:  Issue error state error message
    ;; *******************************************************************
		(if (= myObjectState #void)
			(error "dataMineLib:__tableCursor object not initialized")
			(error "dataMineLib:__tableCursor object closed")
		)
	true)

;******************** __deleteFromIndices *****************
    (defun __deleteFromIndices (value record)
    ;; value is the frameID at which the record is stored in the repository
		vars:(i len index)
;		(STARTTIME posttime:)
		(if (not (__getEnableIndicesSW)) (return true)); skip delete if indices are not enabled
		; delete key from each index		
		
		(setq len (length indexDictionary))
		(loop for i from 0 until len do
			(setq index indexDictionary[i 1])
			(if (= index.noUpdating false)
				(if index.unique
					(index.index.Pv.delete (index.getKey record))
					(index.index.Pv.delete (index.getKey record) value)))
		);i
;		(SETTIME)
		true)

;********************* __insertIntoIndices ********************
    (defun __insertIntoIndices (value record)
    ; value is the frameID at which the record is stored in the repository
		vars:(i len index)  
;		(STARTTIME posttime:)
		(if (not (__getEnableIndicesSW)) (return true)); skip update if indices are not enabled
		; insert new key into each index		
		(setq len (length indexDictionary))
		(loop for i from 0 until len do
			(setq index indexDictionary[i 1])
			(if (= index.noUpdating false)
				(setq index.index[(index.getKey record)] value))
		);i
;		(SETTIME)
		true)

;********************** __loadIndices ***********************
    (defun __loadIndices()
    ; Load the table indices for the cursor (disk and static cursors only)
		vars:(i len)
;		(STARTTIME posttime:)
		(if (<> indexDictionaryKey #void) ; load existing index definitions
			(begin
			; open each index with
			(setq len (length indexDictionary))
			(loop for i from 0 until len do
				(if (= indexDictionary[i 1].index #void)
					(begin
					;(setq indexDictionary[i 1].index (new index )) ; create the index object 
					;(indexDictionary[i 1].index.Pv.init indexDictionary[i 1].frameID myRepos transStruct) ;open the index
					(setq indexDictionary[i 1].index (new index indexDictionary[i 1].frameID myRepos transStruct)) ; create the index object 
					end)
				);if
			);i
			end)
		);if
;		(SETTIME)
		true)


;********************** __makeFakeFrameID ********************
    (defun __makeFakeFrameID ()
    (setq fakeFrameID (- fakeFrameID 1.0)))

;********************** _makeRowVector **********************
    (defun __makeRowVector()
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;; Make the rowVector with each element pointing to the
	;; corrosponding element in bckVector. This routine is
	;; only used for buffered cursors ie: disk: and static: cursors
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		vars:(i oldLen len) 
;		(STARTTIME posttime:)
		(setq recordCount (length bckVector))
		(setq len recordCount)
		(if (= rowVectorSeed #void) 
			(begin
			(setq rowVectorSeed (new Vector: integer: len))
			(loop for i from 0 until len do
				(setIntVector rowVectorSeed i i)
			);i    
			end)
		);if
		;change size of rowVectorSeed if necessary
		(setq oldLen (length rowVectorSeed))
		(if (<> oldLen len)
			(begin
			(resize rowVectorSeed len)
			;update offset values contained in rowVectorSeed for new elements (if any)
			(if (> oldLen 0)
				(loop for i from (isub oldLen 1) until len do
					(setIntVector rowVectorSeed i i)
				);i
			);if
			end)
		);if
		;create new rowVector from copy of rowVectorSeed
		(setq rowVector (copy rowVectorSeed))	
;		(SETTIME)
		true)    

;********************* __updateBckVectorIndex *************
    (defun __updateBckVectorIndex()
		vars:(i len)
;		(STARTTIME posttime:)
		(if (not (__getEnableIndicesSW)) (return true))
			(setq len (length bckVector)) 
			(resize bckVectorIndex 0)
			(loop for i from 0 until len do
				(setq bckVectorIndex[(refNumVector bckVector i)] i)
			); i
;		(SETTIME)
		true)
;********************* __updateIndices ********************
    (defun __updateIndices (value record oldValue oldRecord)
    ;value is the frameID at which the record is stored in the repository
		vars:(i len index newKey oldKey)
;		(STARTTIME posttime:)
		(if (not (__getEnableIndicesSW)) (return true)); skip update if indices are not enabled
		; insert new key into each index		
		(setq len (length indexDictionary))
		(loop for i from 0 until len do
			(setq index indexDictionary[i])
			(if (= index.noUpdating false)
				(begin
				(if index.unique
					(index.index.Pv.delete (index.getKey oldRecord))
					(index.index.Pv.delete (index.getKey oldRecord) oldValue))
				(setq index.index[(index.getKey record)] value)
				end)
			);if
		);i
;		(SETTIME)
		true)

;********************* __write ********************
    (defun __write(row record argTableKey)
    ;; *******************************************************************
    ;; Summary:  Write a record to the table for update.
    ;; Args:  row      		Row number of row to be written to the table.
    ;;        record   		Record to be written to the table.
    ;;		  argTableKey	Member table key if writing to a meta table. Optional. It can be discovered by inspection.
    ;; Return:   record   The record just written. 
    ;; *******************************************************************
		vars:(bufferEntry i j t saveRec appendSW oldRecord oldValue value record  timeStart wrtTime buffer
				memberTableName
				memberTableExtent
				memberTableRepos
				memberTableSchema
				memberTableBckVector
				numTables)

		(if (not myObjectState) (__errorState)); make sure object has been initialized

		(__reset);; Make sure the backup records are restored. 
				; Note for memory cursors, rowVector and bckVector now point to same object vector
		
		
		;; Make sure the specified row number is valid.
		(if (or (< row 0) (> row recordCount))
		   (__error (append "__write:" myObjectName " - " row " :An attempt was made to write with a bad row number.")))


		(if (= row recordCount) 
			(begin
			(setq appendSW true)
			(setq recordCount (addi recordCount 1))
			end)
		else
			(setq appendSW false)
		);if

		(if (<> myCursorType memory:)
			(begin
			(if (and appendSW (= myObjectType metaTable))
				(__error (append "__write: " myObjectName " :An attempt was made to append a record in a meta table.")))
	
			(if (and (= myObjectType metaTable:) (= argTableKey #void))
				(begin
				;find argTableKey by inspection
				(setq numTables (length memberTableOffsets))
				(loop for i from (- numTables 1) to 0 by -1 do
					(if (>= row memberTableOffsets[i]) 
						(begin
						(setq argTableKey memberTableDirectory[i 0])
						(goto EXIT1:)
						end))
				);i 				
				EXIT1::
				end)
			);if
			end)
		);if
								
		(setq record (copy record))
		(setAttributes record colVector)
		(setq oldValue #void)
		(setq oldRecord #void)
			
		(if (= myCursorType memory:)
			(begin ;; Handle memory cursor write

			(if (and enableIndicesSW (not appendSW))
					(setq oldRecord (copy (refObjVector bckVector row)))); if
			
			(setq myDirtyFlag true)

			;write record into table. For memory cursors this is a simple assignment into an object vector
			(if appendSW
				(setq bckVector[row] record)
				(setObjVector bckVector row record))

			(if enableIndicesSW
				(begin
				(if appendSW
					(begin
					(setq value (__makeFakeFrameID)); value now contains a pseudo frameID
					(setq bckVectorFrameID[row] value)
					(setq bckVectorIndex[value] row)
					(__insertIntoIndices value record)
					; Note that we DO NOT have to call __updateBckVectorIndex because none of the existing value
					; entries in bckVectorIndex will have changed! 
					end)
				else ; update existing record - note that the value does not change 
					 ; on memory cursor updates since we do not write to the repository!
					(begin
					(setq oldValue (refNumVector bckVectorFrameID row))
					(__updateIndices oldValue record oldValue oldRecord)
					end)
				);if
				end)
			);if
		
			end); memory cursor
		else ;; Handle disk cursor write
			(begin 
			(setq oldValue #void)
			(setq oldRecord #void)

			(setq myDirtyFlag true)

			;NOTE: appendSW will never be true for a metatable
			(if (not appendSW)
				(setq oldValue (refNumVector bckVector row)))

			(if (and myBuf.enableIndicesSW (not appendSW)) ; grab a COPY of the old record at row for updateIndices call
				(setq oldRecord (__read row bckVector:))) ; grab the old frameID assocaited with record at row

			;prepare the new record for update in table
			(setq buffer myBuf.buffer)
			(if (and (not appendSW) (<> (refObjVector buffer row) #void)) ;; We need to preserve contents of cdr of buffered record
				(begin ; remember that the cdr is used to manage the buffer records as a double linked list
				(setCdr record (cdr (refObjVector buffer row))); 
				(setObjVector buffer row record)
				end)
			);if

			(setAttributes record #void)

			(if appendSW
				(setq bckVector[row] (setq myRepos[frame: oldValue] record)) ; setq will extend existing vector
			else
				(begin
				(if (= myObjectType metaTable:)
					(begin
					;load member table schema
					(setq t (member argTableKey memberTableDirectory))
					(setq memberTableName memberTableDirectory[t 1].tableName)
					(setq memberTableExtent memberTableExtents[t])
					(setq memberTableRepos myParent.dmExt[memberTableExtent].repos)
					(myParent.__beginTrans memberTableExtent)
				  	(setNumVector bckVector row (setq memberTableRepos[frame: oldValue] record)) ; setNumVector is faster for existing row updates
					(setq memberTableSchema memberTableRepos[memberTableName])
					(setq memberTableBckVector memberTableRepos[frame: memberTableSchema.bckVectorKey])
					(setq j (- row memberTableOffsets[t]))
					(setq memberTableBckVector[j] bckVector[row])
					(setq memberTableSchema.bckVectorKey (setq memberTableRepos[frame: memberTableSchema.bckVectorKey] memberTableBckVector)) 
					(setq memberTableRepos[memberTableName] memberTableSchema)
					(myParent.__commitTrans memberTableExtent)
					end)
				else
				  	(setNumVector bckVector row (setq myRepos[frame: oldValue] record)) ; setNumVector is faster for existing row updates
				);if
			  	end)
			 );if
		
		  	(setq value (refNumVector bckVector row))
		  	(setAttributes record colVector)

			;handle updating the indices as necesary
			(if myBuf.enableIndicesSW
				(begin 
				(if appendSW
					(begin
					(setq bckVectorIndex[value] row)
					(__insertIntoIndices value record)
					end)
				else ; update of existing record
					(begin
					(^delete bckVectorIndex (member oldValue bckVectorIndex) )
					(setq bckVectorIndex[value] row)
					(__updateIndices value record oldValue oldRecord)
					end)
				);if
				end)
			);if		
		
			(__reset);; Make sure the backup records are restored. 
				; Note for memory cursors, rowVector and bckVector now point to same object vector
		
			(__syncUp); sync up static cursors
		
		  	end)
		);if
		
		;; Clean up and return the record just written.
		(if (= myCursorType disk:) (setq record (copy record)))
		(setAttributes record colVector)
		(setCdr record #void)	
		
		record) ;; end of write

    (defun __error(message ...)
    	vars:(numArgs transSW unwindSW cursorIndex)
    	(setq numArgs (argCount))
    	(setq transSW false)
    	(if (> numArgs 1) (setq transSW (argFetch 1)))
    	(if (> numArgs 2) (setq unwindSW (= (argFetch 2) unwind:)))
    	(if transSW
    		(myParent.__commitTrans myExtIndex)) ; commit existing transactions to reduce trans count
    	(if unwindSW ; remove cursor from cursorlist before return!
    		(begin
			;; Clear the cursor object.
			(setq cursorIndex (inside mySelf openTables[myObjectName].cursorList))
			(if (>= cursorIndex 0)(setq openTables[myObjectName].cursorList (^delete openTables[myObjectName].cursorList cursorIndex)))
			
			; if no more cursors are open on table then remove it from openTables 
			(if (= (length openTables[myObjectName].cursorList) 0)
			   	(setq openTables[myObjectName] #void)
				);if
    		end)
    	);if
    	(error (append "dataMineLib:__tableCursor:" message))
    true)


;************************ MAIN BODY ***********************


	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;; Initialize this data mine table cursor.
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	
    (setq mySelf (myself))
    
    
;    ;Fixup Intefaces structure for each child Lambda
;    (setq len (length mySelf.Pv))
;    (loop for i from 0 until len do
;    	(setq obj mySelf.Pv[i])
;    	(if (isLambda obj) 
;    		(setq obj[In:] (copy obj[In:]))
;    	);if
;    )
;	(setq mySelf[In:] (copy mySelf[In:]))

	(STARTTIME posttime:)

    
 	(if (<> myObjectState #void) (__errorState)); make sure object has never been initialized before!
    (setq myObjectState true) ; switch object to initialized state

	; Test for valid arguments
	(if (not (or (= argCursorType disk:) (= argCursorType static:) (= argCursorType memory:))) 
		(__error (append "init: -- invalid cursor type argument -" argCursorType "- Must be disk:, static: or memory:"))
	)


	; Process intilization arguments
	(setq enableIndicesSW false) 
	(setq withIndicesSW argWithIndices)
	(setq loadByIndexSW argLoadByIndex)
	(setq indexName	argIndexName)
	(setq indexKeyVal argIndexKeyVal)
	(setq partialKeyCompareSW argPartialKeyCompare)         


	;Init
	(setq showLimit 10)
	(setq myParent argParent)
	(setq myCursorType argCursorType)
	(__clear)

	;; create transaction structure argument that will be passed to the index init function calls
	(setq transStruct (new Structure:))
	(setq transStruct.__beginTrans myParent.__beginTrans)
	(setq transStruct.__commitTrans myParent.__commitTrans)
	(setq transStruct.__rollbackTrans myParent.__rollbackTrans)


	;; Initialize access to table information contained in the parent
	;; Multiple cursors may be opened on the same table. In addition, static: and disk:
	;; cursors share a common bckVector. This kind of shared table information is
	;; referenced in the parent Lambda. Below, we grab our own references to this
	;; information for faster access in our internal routines.
	(setq openTables myParent.openTables)
	(setq openBufs myParent.openBufs)

	(cond
		((= openTables #void) 	(setq errStr "openTables"))
		((= openBufs #void)  	(setq errStr "openBufs"))
		((true) (setq errStr ""))
	);cond
  	(if (<> errStr "") (__error (append "init: " errStr " not initilized in parent") false) )
	
 	;; Search for the data mine extent containing the specified table.
	(if (= (setq myExtIndex (myParent.getTableExtent argTable)) false)
	   (__error (append "init: " argTable " :An attempt was made to open an unknown table.")))

	;; Again we grab our own references to important information referenced in the parent Lambda.
	;; This speeds up our access and simplifies our programs syntax
	(setq myRepos myParent.dmExt[myExtIndex].repos) ; myRepos is the object repository in which the table is contained
	(setq mySchema myRepos[argTable]) ; mySchema is the table schema record loaded from the repository

	;; Was the table found?
	(if (= mySchema #void) 
	   (__error (append "init: " argTable " :An attempt was made to open an unknown table.")))

	(if (and loadByIndexSW (= myObjectType metaTable:))
			(__error "init: Attempt to load by index on a metaTable"))
	
	;; Expand mySchema with new data elements if necessary. These are elements that may have been added to the mySchema
	;; record after it was saved by a previous version of the dataMineLib.
	(if (not (isMember indexDictionaryKey: mySchema)) (setq mySchema.indexDictionaryKey #void))
	(if (not (isMember memberTableDirectoryKey: mySchema)) (setq mySchema.memberTableDirectoryKey #void))
	(if (not (isMember sjIndexKey: mySchema)) (setq mySchema.sjIndexKey #void)) 

	(objectToStructure mySelf.Pv mySchema) ;Update the schema items in this Lambdas pv structure with items from mySchema
	
	;; Initialize mySchema components that may not have been initilized yet
	(if (= myMemoPad #void) (setq myMemoPad (new Dictionary:)))

	;; Begin a transaction on the physical repository if disk cursor.
	(if (= myCursorType disk:) (myParent.__beginTrans myExtIndex))

	;;; Clean up tables created by old dataMineLib which may have stored rowVector
 	(if (<> rowVectorKey #void)
 		(begin
 		(setq myRepos[frame: rowVectorKey] #void)
 		(setq rowVectorKey #void)
 		end)
 	);if 

	;; static and disk cursors share a common bckVector maintained
	;; in myParent.Pv.openBufs[tablename].bckVector.  
	;; Memory cursors on the other hand maintain their own local
	;; bckVector, which is an object vector containing the actual 
	;; rows of data read off the disk.  

	(if (or (= myCursorType disk:) (= myCursorType static:))
		(begin

		;;Are we opening a member table?
		(if (<> metaTable #void)
			(begin
			(if (isMember metaTable openTables)
				(__error (append "init:" myObjectName " :Attempt to open a member table while metaTable is open. ") true)
			else ; we have a member table so check if it is the last member table in the metatable
				(begin
				(setq metaTableExtent (myParent.getTableExtent metaTable))
				(if (= metaTableExtent -1) 
					(__error "init: Error in member table, no meta table of specified name available.") true)

				(myParent.__beginTrans metaTableExtent)
				(setq metaTableRepos myParent.dmExt[metaTableExtent].repos)
				(setq metaTableSchema metaTableRepos[metaTable])
				(setq tempMemberTableDirectory metaTableRepos[frame: metaTableSchema.memberTableDirectoryKey])
				(setq i (inside metaTable tempMemberTableDirectory))
				(if (= i (- (length tempMemberTableDirectory)))
					(setq lastMemberTableSW true)
					(setq lastMemberTableSW false))
				(setq tempMemberTableDirectory #void)
				(myParent.__commitTrans metaTableExtent)			

				end)
			);if
			end)
		);if		

		(if (and (= myObjectType metaTable:) (= myCursorType static:))
			(error (append "tableCursor: Attempt to open a metaTable as a static cursor. " myObjectName)))

		;; If this is a meta table, make sure none of its member tables are currently opened as disk or static cursors
		(setq memberTableDirectory #void)
		(if (<> memberTableDirectoryKey #void)
			(setq memberTableDirectory myRepos[frame: memberTableDirectoryKey]))

		(if (= myObjectType metaTable:)
			(begin
			(loop for i from 0 until (length memberTableDirectory) do
				(setq j (member memberTableDirectory[i 1].tableName openTables))
				(if (<> j false)
					(begin
					(setq cursorList openTables[j 1].cursorList)
					(loop for k from 0 until (length cursorList) do
						(if (and (= cursorList[k].metaTable myObjectName) (<> cursorList[k].myCursorType memory:))
							(__error (append "init: Attempt to open a metaTable while one of its member tables is open.")))
					);k				
					end)
				);if
			);i
			end)
		);if

		;;NOTE: the following two actions must follow (not preceed) any "expected errors" so that we do not have 
		;; a partially constructed cursor.
		;; Make sure openTables has an entry for this table 
		(setq i (inside argTable openTables ))
		(if (= i false) (setq openTables[argTable] (makeStructure cursorList: (new Vector: object: 0))))

		;; Add this cursor to the cursorList for the table
		(setq i (inside  mySelf openTables[argTable].cursorList))
		(if (= i false) (setq openTables[argTable].cursorList[(length openTables[argTable].cursorList)] mySelf))

		;NOTE: If you encounter an expected error past this pont you must unwind the cursor entry before 
		;; return!!!!


		(if (not (isMember argTable openBufs)) ; Setup buffer entry if not already there
			(begin 
			(setq openBufs[argTable] 
				(new Structure: 
				bckVector: #void
				bckVectorIndex: (new Directory:)
				indexDictionary: #void
				enableIndicesSW argEnableIndices
				indicesDirtySW false			
				buffer: #void
				bufHead: #void
				bufTail: #void
				bufSize: 0
				bufUsed: 0
				))
			(setq myBuf openBufs[argTable])
			(if (= bckVectorKey #void)
				(setq myBuf.bckVector (new Vector: number: 0)) ; create a new empty bckVector
				(setq myBuf.bckVector myRepos[frame: bckVectorKey]) ; load the last saved bckVector for table
			)

			(if (= myBuf.bckVector #void) (setq myBuf.bckVector (new Vector: number: 0)))

			(setq bckVector myBuf.bckVector)
			(setq recordCount (length bckVector))
			(setq myBuf.buffer (new Vector: object: recordCount))
			(setq myBuf.bufSize argBufSize)
			(setq myBuf.bufUsed 0)

			;; Set up indices structures
			; Note that we load the indexDictionary even if indices are disabled. This allows each cursor to have
			; a valid pointer to the indexDictionary. If we wait to load it when needed, any cursors created prior
			; to the load of indexDicitonary would not have a valid cursor. This could be handled in (__enableIndices)
			; but it gets too messy to bother with. Note that loading the indexDictionary DOES NOT load the indices themselves.
			(if (<> indexDictionaryKey #void) ; always load indexDictionary - even if indices are disabled
				(setq myBuf.indexDictionary myRepos[frame: indexDictionaryKey]))
			(if (= myBuf.indexDictionary #void) (setq myBuf.indexDictionary (new Dictionary:)))

			(setq indexDictionary myBuf.indexDictionary); local reference for faster access
			(setq myBuf.bckVectorIndex (new Directory:))
			(setq bckVectorIndex myBuf.bckVectorIndex); local reference for faster access

			;; Load the star join index if one exists
			(setq sjIndex #void)
			(if (<> sjIndexKey #void)
				(setq sjIndex myRepos[frame: sjIndexKey]))
			
   			(if (<> sjIndex #void)
   				(objectToStructure mySelf.Pv sjIndex)) ;Update the local star join index management variables for faster access 

			;;Are we opening a metaTable?
			(if (= myObjectType metaTable:)
				(begin
				(setq numTables (length memberTableDirectory))
				(setq memberExtentsOpened (new Directory:))
				(loop for j from 0 until numTables do
					(setq memberTableExtent (myParent.getTableExtent memberTableDirectory[j 1].tableName))
					(if (and (not (isMember memberTableExtent memberExtentsOpened)) (<> memberTableExtent myExtIndex))
						(begin
						(myParent.__beginTrans memberTableExtent)
						(setq memberExtentsOpened[memberTableExtent] true)
						end)
					);if
				);j
				;Update the meta table with changes made in member tables
				(__updateMetaTable)
				
				end)
			);if
	
			;;Note: Notice that the enablement of indices comes after all other processing that could find an
			;; expected error! This allows the unwinding of cursors in the _error routine to be much
			;; simpler.

			;; Note on Index Enablement
			;; if the user has requested enabled indices but none have been defined on the table then
			;; indices will be disabled here -- overriding the users request. However, the first time
			;; a user calls createIndex on a cursor indices will be automatically enabled for the table
			;; regardless of any initial enableIndices: or disableIndices: argument.
    		(if argWithIndices
				(__enableIndices)); will enable indices only if there are currently defined indices

			end)
		else ; open cursor when table is already in openBufs
			(begin
			;Note: we don't check for meta table stuff in this section because we only allow metaTable cursors to be opened as disk cursors. This means
			; there will never be a second open static: or disk: cursor on a meta table.

			(setq myBuf openBufs[argTable])
			(setq bckVector myBuf.bckVector)
			(setq indexDictionary myBuf.indexDictionary); note that myBuf.indexDictionary is already loaded
			(setq bckVectorIndex myBuf.bckVectorIndex); note that the myBuf.bckVectorIndex vector alread exists
			(setq recordCount (length bckVector))
			
			;; Check if this cursor was opened with the indices enabled and indices are not already loaded
			(if (and (not myBuf.enableIndicesSW) argEnableIndices)
				(__enableIndices))
		
			end)
        );if

		(setq myBufferUseSW (<> argBufSize -1)) ; unbuffered cursor if -1
		(if (> argBufSize 0) (setq myBuf.bufSize argBufSize))

		;; Load the directory of saved view record vectors into the cursor
		(setq viewDirectory (new Directory:))
		(if (<> viewDirectoryKey #void)
			(setq viewDirectory myRepos[frame: viewDirectoryKey]))
		end)
	else ; initialize memory cursors bckVector by direct read
		(begin

		(if (and (= myObjectType metaTable:) (= argCreateEmpty false))
			(__error (append "init: " myObjectName " :Attempt to open a metaTable as a memory cursor.")))

		(if (and argCreateEmpty argWithIndicesSW) 
			(__error (append "init: " myObjectName " :Attempt to open memory cursor with withIndices option and createEmpty option")))

		(if (and argCreateEmpty and loadByIndexSW)
			(__error (append "init: " myObjectName " :Attempt to open memory cursor with loadByIndex option and createEmpty option")))
			
		;;NOTE: the following two actions must follow (not preceed) any "expected errors" so that we do not have 
		;; a partially constructed cursor.
		;; Make sure openTables has an entry for this table 
		(setq i (inside argTable openTables ))
		(if (= i false) (setq openTables[argTable] (makeStructure cursorList: (new Vector: object: 0))))
	
		;; Add this cursor to the cursorList for the table
		(setq i (inside  mySelf openTables[argTable].cursorList))
		(if (= i false) (setq openTables[argTable].cursorList[(length openTables[argTable].cursorList)] mySelf))

		;NOTE: If you encounter an expected error past this pont you must unwind the cursor entry before 
		;; return!!!!
	

		(myParent.__beginTrans myExtIndex)
		(setq viewDirectoryKey #void) ; memory cursors do not load views
		(setq myBufferUseSW false); memory cursors never use buffering

		(if (not argCreateEmpty)
			(if (<> bckVectorKey #void)
				(setq vec myRepos[frame: bckVectorKey])))
			
		(if (= vec #void) (new Vector: 0))
		(setq recordCount (length vec))		
		(setq bckVector (new Vector: object: recordCount)) 

		(setq savedColVector colVector)
		(if (<> argColumns #void)
			(begin ; load only columns specified by user 
			;; Note that you can pass the names of columns that don't exist
			;; in the underlying table! This is okay.
			(setq colVector (copy argColumns))
			(setq colCount (length colVector))
			(setq validFields (objectToStructure colVector #(#void)))
			(setq colVector (refAttributes validFields))
			(setq recordStructure (new Vector: colCount))
			(setAttributes recordStructure colVector)
			end)
		);if

		(if argWithIndicesSW
			(begin
			(setq bckVectorFrameID (new Vector: number: recordCount))
			(setq bckVectorIndex (new Directory:))
			end)
		);
		
		(if (not argCreateEmpty)
			(begin
			(loop for i from 0 until recordCount do
				(setq record myRepos[frame: vec[i]]) ; load record from disk
				(setAttributes record savedColVector)
				(setq record (objectToVector recordStructure record)); reduce to columns desired
				;;Note: The memory cursors bckVector is different than a disk: or static: cursor bckVector.
				;; It can not be buffered and contains record data instead of diskIDs.
				(setObjVector bckVector i (copy record)) ; save in memory cursors bckVector
				(if argWithIndicesSW 
					(begin
					(setNumVector bckVectorFrameID i vec[i])
					(setq bckVectorIndex[vec[i]] i)
					end)
				);if
				
			); i  
			end)
		);if
		
		;; Set up indices -- Note that you can load indices for memory cursors only when the cursor is first opened. This is because the
		;; underlying table may change later and any indices on that table would then become incompatible with the information initially
		;; loaded into the memory cursor. REMEMBER THAT MEMORY CURSORS ARE SNAPSHOTS OF A TABLE!!!!
		(if argWithIndicesSW ;set up index structures and load indices
			(begin
			(setq indexDictionary (new Dictionary:))
			(if (<> indexDictionaryKey #void) 
				(setq indexDictionary myRepos[frame: indexDictionaryKey])
				(setq indexDictionary (new Dictionary:)))
			(if (= indexDictionary #void) (setq indexDictionary (new Dictionary:)))

			; open each index with snapshot option -- we can't wait until we need the indices
			(loop for i from 0 until (length indexDictionary) do
				;(setq indexDictionary[i 1].index (new index)) ; create the index object
				;(indexDictionary[i 1].index.Pv.init indexDictionary[i 1].frameID myRepos transStruct snapshot: memory:) ;open the index
				(setq indexDictionary[i 1].index (new index indexDictionary[i 1].frameID myRepos transStruct snapshot: memory:)) ; create the index object
			);i
			
			(setq enableIndicesSW true)
			end)
		); if
		(setq vec #void)

		(myParent.__commitTrans myExtIndex)
		end)
 	);if
 	
	(__restore)

	(if (= viewDirectory #void) (setq viewDirectory (new Directory:)))

	(if loadByIndexSW ; filter the current view
		(if (= indexKeyVal #void) 
			(__indexView indexName)
			(__indexView indexName indexKeyVal))
	);if

	(SETTIME)
    mySelf) ;; end of __tableCursor




;;**EXPORTKEY**:dataMineLib.__tableCursor.__updateMetaTable
;*********** __updateMetaTable **************
(defchild dataMineLib.__tableCursor __updateMetaTable()
;;*******************************************************************
;; Summary: Update the current metaTable from all member table content
;; Args: none
;; Return: true
	vars: (i j k o
			endpos 
			indexEntryPos
			keyVal
			memberTableSchema
			memberTableRepos
			memberTableBckVector
			numRecs
			numTables
		 	rec
			totRecordCount
		 	)

	(if (<> myObjectType metaTable:)
		(__error "appendToMetaTable: Attemp to updateMetaTable on non-meta table"))

	(setq numTables (length memberTableDirectory))
	(setq memberTableExtents (new Vector: integer: numTables))
	(setq memberTableOffsets (new Vector: integer: numTables))
	(setq uniqueIDColNum (member sjRowKeyCol colVector)) 
	(setq totRecordCount 0)
;(writeln "uniqueIDColNum=" uniqueIDColNum " sjRowKeyCol=" sjRowKeyCol " sjUniqueIDs=" sjUniqueIDs)

	(loop for i from 0 until numTables do
		(setq memberTableExtents[i] (myParent.getTableExtent memberTableDirectory[i 1].tableName))
		(setq memberTableOffsets[i] totRecordCount)
		(if (not memberTableDirectory[i 1].dirty)
			(begin
			(setq totRecordCount (+ totRecordCount memberTableDirectory[i 1].recordCount))
			end)
		else ;table is dirty
			(begin 
			(setq myDirtyFlag true); ensure that changes will be saved on close of meta table
			
			(writeln _eol "metaTable open: processing dirty member table :" memberTableDirectory[i 1].tableName)
			(myParent.__beginTrans memberTableExtents[i])

			(setq memberTableRepos myParent.dmExt[memberTableExtents[i]].repos)
			;Load the memberTableSchema
			(setq memberTableSchema memberTableRepos[memberTableDirectory[i 1].tableName]) 

			;Load the member table bckVector
			(setq memberTableBckVector memberTableRepos[frame: memberTableSchema.bckVectorKey])
			(setq numRecs (length memberTableBckVector))
			(setq totRecordCount (+ totRecordCount numRecs))

			(if (> numRecs sjIndexBlockSize)
				(error (append "init: " memberTableDirectory[i 1].tableName " has more rows than allowed by the MaxRowsPerTable argument")))

			;Check that the number of bckVector entries match previous values
			;unless this is the very last entry in memberTableDirectory
			(if (< i (- numTables 1)) 
				(if (<> memberTableDirectory[i 1].recordCount numRecs)
					(error "init: Member table record count different than what is recorded in metaTable"))
       		else ; make sure the metatable index structures are the right size
				(begin
				;Make sure the bckVector is big enough for all the records in the underlying member table 
				(if (<> (length bckVector) totRecordCount)
					(resize bckVector totRecordCount))

				(if (<> (length sjBckVectorIndex) (* numTables sjIndexBlockSize))
					(resize sjBckVectorIndex (* numTables sjIndexBlockSize)))
				end)
			);if					
			(setq memberTableDirectory[i 1].recordCount numRecs) 
			(setq memberTableDirectory[i 1].dirty false)
	
			;clear the index entries for the current table in the sjBckVectorIndex
			(setq endPos (+ (* i sjIndexBlockSize) sjIndexBlockSize))
			(loop for j from (* i sjIndexBlockSize) until endPos (setq sjBckVectorIndex[j] -1)) 

			(setq k memberTableOffsets[i]); Set k to the starting position in the bckVector for the table

			;set the index entries for the current table in the sjBckVectorIndex and
			;update the meta tables bckVector from the member tables bckVector
			(loop for j from 0 until numRecs do
				;update metaTable bckVector
				(setq bckVector[k] memberTableBckVector[j])
				;read the current record
				(setq rec memberTableRepos[frame: memberTableBckVector[j]])
				(setq keyVal rec[uniqueIDColNum])

				(if (not (isMember keyVal sjUniqueIDs)) ;create a new entry if a new unique ID value was found
					(setq sjUniqueIDs[keyVal] (length sjUniqueIDs)))

				(if (> (length sjUniqueIDs) sjIndexBlockSize)
					(error (append "__updateMetaTable: sjUniqueIDs directories length has grown larger than sjIndexBlockSize. Rebuild metaTable with larger sjIndexBlockSize!")))
				
				;update sjBckVectorIndex for current row
				;Remember: sjBckVectorIndex is a vector of offsets into the metaTable bckVector.
				(setq indexEntryPos (* i sjIndexBlockSize)); indexEntryPos positioned to begining of table's range of entries in sjBckVectorIndex
				(setq indexEntryPos (+ indexEntryPos sjUniqueIDs[keyVal])); indexEntryPos now includes offset for current index column value
				(setq sjBckVectorIndex[indexEntryPos] k); sjBckVectorIndex now contains pointer to k'th element in the metaTable bckVector
				(setq k (addi k 1))
			);j
			
			(myParent.__commitTrans memberTableExtents[i])
			end)
		);if
	);i  
	
	(reset)
	
true)















;;**EXPORTKEY**:dataMineLib.__tableCursor.appendToMetaTable
;*********** appendToMetaTable **************
(defchild dataMineLib.__tableCursor appendToMetaTable(argTableKey argTableName)
;;*******************************************************************
;; Summary: Append a new member table to the current meta table
;; Args:
;;	argKey		; The table key
;;	argTable	; Then name of the member table
;; Return: true
;; 
	vars:( 
		cursor
		memberTableExtent
		memberTableRepos
		memberTableSchema
		);

	(if (<> myObjectType metaTable:)
		(__error "appendToMetaTable: Attempt to appendToMetaTable on a non-meta table"))
	
	(if (<> myCursorType disk:)
		(__error "appendToMetaTable: Attempt to appendToMetaTable using memory cursor"))
		
	;Make sure supplied key will fall at the end of the memberTableDirectory
	(setq numTables (length memberTableDirectory))
	(if (> numTables 0)
		(if (<= argTableKey memberTableDirectory[(- numTables 1) 0])
			(__error "appendToMetaTable: TableKey argument is less than current last TableKey in memberTableDirectory.")))

	;open member table as disk cursor so we have exclusive access to it
	(setq argTableName (symbol argTableName))
	(setq cursor (myParent.open argTableName))
	
	;make sure member table has the same schema definition as the metaTable
	(if (<> cursor.colVector colVector)
		(begin 
		(setq cursor (myParent.close cursor))
		(__error "appendToMetaTable: Member table column definitions differ from those of meta table")
		end)
	);if

	;make sure member table has no indices
	(if (> (length cursor.indexDictionary) 0)
		(begin
		(setq cursor (myParent.close cursor))
		(__error "appendToMetaTable: Member table has indices and cannot be added to meta table")
		end)
	);if
	
	;make sure member table does not belong to any other meta table
	(if (isMember metaTable: cursor.Pv) ;does the metaTable variable exist in the member table schema structure?
		(begin
		(if (= cursor.metaTable #void) ; has metaTable been set?
			(setq cursor.metaTable myObjectName)
		);if
		(if (<> cursor.metaTable myObjectName) ; make sure it is set to the current meta table!
			(begin
			(setq cursor (myParent.close cursor))
			(__error "appendToMetaTable: Member table belongs to another meta table"))
			end)
		end)
	else
		(setq cursor.metaTable myObjectName)
	);if
		
	;Now add the member table to the meta table
	(setq tableEntry (new Structure: tableName: argTableName recordCount: cursor.recordCount dirty: true))
	(setq memberTableDirectory[argTableKey] tableEntry)
	(setq lastMemberTable argTableName)
	(setq myDirtyFlag true) ; make sure the meta table header information will get saved.
	
(writeln "memberTableDirectory=" memberTableDirectory)	
	;Close the member table
	(setq cursor (myParent.close cursor makeMember:))

	(__updateMetaTable)
	
	(SETTIME)
	
	
true)















;;**EXPORTKEY**:dataMineLib.__tableCursor.createMemoryCursorFromView
;*********** createMemoryCursor **************
(defchild dataMineLib.__tableCursor createMemoryCursorFromView()
;;*******************************************************************
;; Summary: Create a memory cursor from the current view
;; Args: none
;; Return: The new cursor
    vars:(cursor rec)
   	(if (not myObjectState) (__errorState)); make sure object has been initialized
    (setq cursor (myParent.open myObjectName memory: createEmpty:))
    (loop for i from 0 until recordCount do
    (setq rec (read i))
    (cursor.write i rec) 
    );i
    
cursor)















;;**EXPORTKEY**:dataMineLib.__tableExtentMgrTemplate
(deforphan dataMineLib:__tableExtentMgrTemplate(extentName)  
;; *******************************************************************
;; summary:  Factory for creating browseLib extent manager Lambdas.
;;           Manages all Lambda source/binary code stored in each miner
;;           project table. Includes checking Lambda source/binary 
;;           in and out of the miner project, importing, exporting, 
;;           and deleting source from the miner project.
;;
;; Args:     extentName:          Name of the new file cabinet repository extent.
;; Return:   true
;;
;; Note:
;;           Manages the Filter Lambdas, in a datamine miner project table
;;           using the foreign extent manager API understood by browseLib.
;; *******************************************************************
    vars:(newMgr)
    pvars:(;; Public Child Lambdas
           abortFocus                 ;; Remove the specified extent from focus 
           abortTransaction           ;; Abort a transaction on the extent repository
           beginTransaction           ;; Begin a transaction on the extent repository
           commitTransaction          ;; Commit a transaction on the extent repository
           compileByIndex             ;; Compile the specified Lambda source using an index
           compileByKey               ;; Compile the specified Lambda source using a key
           deleteLambdaByIndex         ;; Delete an Lambda from the extent using an index
           deleteLambdaByKey           ;; Delete an Lambda from the extent using a key
           deleteBinaryByIndex        ;; Delete an Lambda binary value using an index
           deleteBinaryByKey          ;; Delete an Lambda binary value using a key
           deleteSourceByIndex        ;; Delete an Lambda source value using an index
           deleteSourceByKey          ;; Delete an Lambda source value using a key
           getNextLevel               ;; Return an XPath directory string list for the next repository directory level.
           getTypes                   ;; Return an XPath directory type list string for all possible types in a hierarchical descendents list
           inspectRepository          ;; Perform a complete inspection of my extent repository.
           memberIndex                ;; Return the index of a key in an extent
           refLambdaByIndex            ;; Retrieve an Lambda from an extent using an index
           refLambdaByKey              ;; Retrieve an Lambda from an extent using a key
           refBinaryByIndex           ;; Retrieve an Lambda binary value using an index
           refBinaryByKey             ;; Retrieve an Lambda binary value using a key
           refKeyByIndex              ;; Retrieve an extent key using an index
           refLength                  ;; Retrieve the number of items in an extent
           refSourceByIndex           ;; Retrieve an Lambda source value using an index
           refSourceByKey             ;; Retrieve an extent source value using a key
           refSourceExByIndex         ;; Retrieve an Lambda export source value using an index
           refSourceExByKey           ;; Retrieve an extent export source value using a key
           setLambdaByKey              ;; Store an extent value using a key
           setBinaryByKey             ;; Store an extent Lambda binary value using a key
           setFocus                   ;; Set the specified extent in focus 
           setSourceByKey             ;; Store an extent Lambda source value using a key
           ;; Private Child Lambdas
           _initialize                ;; Initialize the datamine miner project for editing.
           _noopErr                   ;; A no operation error function returning #void.
           _syncUp                    ;; Synchronize the datamine miner project after editing.
		   ;; Private Variables
           myLambdaName                ;; The name for the current Lambda being managed
           myExtentName               ;; The name for this extent
           myTableNameList            ;; The table name vector for the data mine
           myParent                   ;; The parent Lambda for this extent manager
           (tempFileName "__temp.xls");; The temporary file name for data transfer in the XML folder 
           ) ; end persistant variables
    ;;****************************************************************
    ;; Define Private Child Lambdas
    ;;****************************************************************
    ;; Abort a transaction on the extent repository 
    (defun _initialize()
        vars:(result)
        (setq result (setq myTableNameList (myParent.getTableNames)))
        result) ; end _initialize
    ;; A no operation error function returning #void. 
    (defun _noopErr(msg) #void)
    ;; Synchronize the datamine miner project after editing. 
    (defun _syncUp()
    	vars:(result)
        (setq result (if (= myTableNameList #void) (setq myTableNameList (myParent.getTableNames))))
        result) ; end _initialize
    ;;****************************************************************
    ;; Define Public Child Lambdas
    ;;****************************************************************
    ;; Remove the datamine miner project from focus 
    (defun abortFocus() 
       true)
    ;; Initialize the datamine miner project for editing. 
    (defun abortTransaction() 
       true)
    ;; Begin a transaction on the extent repository 
    (defun beginTransaction() 
    	  vars:(result) 
    	  (setq result (_initialize))
       result)
    ;; Commit a transaction on the extent repository 
    (defun commitTransaction() 
    	   true)
    ;; Compile the specified Lambda source using an index 
    (defun compileByIndex(index alwaysCompile) (_initialize) (setq result (compileByKey myTableNameList[index] alwaysCompile)))
    ;; Compile the specified Lambda source using a key 
    (defun compileByKey(key alwaysCompile)
       true) ; end compileByKey
    ;; Delete an Lambda from the extent using an index 
    (defun deleteLambdaByIndex(index) 
    	vars:(result)
    	  (_syncUp) 
    	  (setq result (deleteLambdaByKey myTableNameList[index]))
    	  result)
    ;; Delete an Lambda from the extent using a key 
    (defun deleteLambdaByKey(key) (deleteSourceByKey key))
    ;; Delete an Lambda binary value using an index 
    (defun deleteBinaryByIndex(index) 
    	  vars:(result)
    	  (setq result (deleteLambdaByKey myTableNameList[index]))
    	  result)
    ;; Delete an Lambda binary value using a key 
    (defun deleteBinaryByKey(key) (deleteSourceByKey key))
    ;; Delete an Lambda source value using an index 
    (defun deleteSourceByIndex(index)
    	  vars:(result) 
    	  (_syncUp) 
    	  (setq result (deleteSourceByKey myTableNameList[index]))
    	  result)
    ;; Delete an Lambda source value using a key 
    (defun deleteSourceByKey(key) 
       (_syncUp) 
       (myParent.dropTable key)
       true)
    ;; Return an XPath directory string list for the next repository directory level.
    (defun getNextLevel(extentName startLine lineCount ...)
        vars:(i I s cursor record)
        ;; Note1: Each XPath directory line has the following tab delimited fields:
        ;;         type    		{String}
        ;;         value   		{filterName for each node in the repository.}
        ;;         size			none
        ;;         date			none
        ;;         time			none
        ;;         version			{current AIS version}
        ;;         symbolicKey		{filterName for each node in the repository.}
        ;;         uniqueKey		{filterName for each node in the repository.}
        ;; Note2: We support only one hierarchy level of repository directory.
        ;;        Investor names may contain embedded colon (:) characters which
        ;;        signify TestSuite:investor relationships.
        (_initialize)
        (setq I (length myTableNameList))
        (setq I (min I (+ startLine lineCount)))
        (setq s (append "" startLine #\tab lineCount #\tab I )) 
        (setq s (append s #\newline "Current" #\tab extentName #\tab I #\tab "" #\tab "" #\tab (isVersion) #\tab extentName #\tab extentName)) 
        (loop for i from startLine until I do
            (setq s (append s #\newline "Table" #\tab myTableNameList[i] #\tab "" #\tab "" #\tab "" #\tab (isVersion) #\tab myTableNameList[i] #\tab myTableNameList[i]))
            ) ;; end of loop
        s)
    ;; Return an XPath directory type list string for all possible types in a hierarchical descendents list 
    (defun getTypes()
        vars:(result)
        (setq result (append ".default." #\tab "edit" #\newline
                             "Table" #\tab "edit,erase,checkin" #\newline
                      ))
        result)
	;; Perform a complete inspection of my extent repository.
	(defun inspectRepository() 
	   true) ; end inspectRepository
    ;; Return the index of a key in an extent 
    (defun memberIndex(key) 
    	   vars:(result)
    	   (_syncUp) 
    	   (setq result (member key myTableNameList))
    	   result)
    ;; Retrieve an extent value using an index 
    (defun refLambdaByIndex(index) 
       vars:(result)
       (_syncUp) 
    	  (setq result (refLambdaByKey myFilterNameList[index]))
    	  result)
    ;; Retrieve an extent value using a key 
    (defun refLambdaByKey(key)
       vars:(cursor result)
       (_syncUp) 
       (setq cursor (myParent.open key memory:))
       (setq result (myParent.tempTable key cursor.colVector))
       (setq result.bckVector (copy cursor.bckVector))
       (result.restore)
       (setq cursor (myParent.close cursor))
       result)
    ;; Retrieve an Lambda binary value using an index 
    (defun refBinaryByIndex(index) (refLambdaByIndex index)) 
    ;; Retrieve an Lambda binary value using a key 
    (defun refBinaryByKey(key) (refLambdaByKey key))
    ;; Retrieve an extent key using an index 
    (defun refKeyByIndex(index) 
       vars:(result)
       (_syncUp) 
       (setq result myTableNameList[index])
       result)
    ;; Retrieve the number of items in an extent 
    (defun refLength() 
       vars:(result)
       (_syncUp) 
       (setq result (length myTableNameList))
       result)
    ;; Retrieve an extent scource value using an index 
    (defun refSourceByIndex(index) 
       vars:(result)
       (_syncUp) 
       (setq result (refSourceByKey myTableNameList[index]))
       result) 
    ;; Retrieve an Lambda source value using a key 
    (defun refSourceByKey(key) 
       vars:(r R c C record cursor result)
       (onError _noopErr) 
       (_initialize)
       (setq cursor (myParent.open key memory:))
       (cursor.exportTab (append _ais.xmlDir tempFileName))
       (setq result (browseLib.readSourceFile (append _ais.xmlDir tempFileName)))
       (setq cursor (myParent.close cursor))
       result)
    ;; Retrieve an extent scource (for export) using an index 
    (defun refSourceExByIndex(index) (refSourceByIndex index)) 
    ;; Retrieve an Lambda source (for export) using a key 
    (defun refSourceExByKey(key) (refSourceByKey key))
    ;; Store an extent value using a key 
    (defun setLambdaByKey(key newLambda) 
       (if (or (isString newLambda) (isByteVector newLambda)) (return (setSourceByKey key newLambda)))
       (newLambda.exportTab (append _ais.xmlDir tempFileName))
       (if (not (myParent.isTable key)) (myParent.createTable key)) 
       (myParent.importTab key overwrite: (append _ais.xmlDir tempFileName))
       true)
    ;; Store an extent Lambda binary value using a key 
    (defun setBinaryByKey(key newBinary) (setLambdaByKey key newBinary)) 
    ;; Set the datamine miner project in focus 
    (defun setFocus() 
       vars:(result)
       (_initialize)
       true)
    ;; Store an extent Lambda source value using a key 
    (defun setSourceByKey(key newSource)
       vars:(cursor)
       (if (isLambda newSource) (setq newSource newSource.Sc))
       (browseLib.writeSourceFile (append _ais.xmlDir tempFileName) (string newSource))
       (if (not (myParent.isTable key)) (myParent.createTable key)) 
       (myParent.importTab key overwrite: (append _ais.xmlDir tempFileName))
       true)
    ;;****************************************************************
    ;; MAIN initialization section
    ;;****************************************************************
    (setq newMgr (^new (myself)))
    (setq newMgr.myExtentName extentName)
    (setq newMgr.myParent dataMineLib)
    newMgr) ; end __tableExtentMgrTemplate





;;**EXPORTKEY**:dataMineLib.__viewAND
(defchild dataMineLib:__viewAND(result vector1 vector2)
   ;; *******************************************************************
   ;; summary:  Combines two view vectors using the logical AND operation.
   ;; Args:     result       The result vector.
   ;;           vector1      The first input vector.
   ;;           vector2      The second input vector.
   ;; Return:   result       The result vector.
   ;; *******************************************************************
	vars:(time record1 record2 index1 index2)
	(STARTTIME)	
	(setq vector1 (sort (copy vector1) <))
	(setq vector2 (sort (copy vector2) <))
	(setq record1 vector1[(setq index1 0)])
	(setq record2 vector2[(setq index2 0)])
	(while (or (<> record1 #void) (<> record2 #void)) do
	  (cond ;; If records match they must be returned.
	        ((= record1 record2)
	         (begin
	            (setq result[(length result)] record1)
	            (setq record1 vector1[(++ index1)])
	            (setq record2 vector2[(++ index2)])
	            )) ; end equal case
	        ;; If the first record is #void, we must promote vector two.
	        ((= record1 #void)
	         (begin
	            (setq record2 vector2[(++ index2)])
	            )) ; end promote vector two case
	        ;; If the second record is #void, we must promote vector one.
	        ((= record2 #void)
	         (begin
	            (setq record1 vector1[(++ index1)])
	            )) ; end promote vector one case
	        ;; If the first record is low, we must promote vector one.
	        ((< record1 record2)
	         (setq record1 vector1[(++ index1)])
	            ) ; end promote vector one case
	        ;; If the second record is low, we must promote vector two.
	        (else
	         (setq record2 vector2[(++ index2)])
	            ) ; end promote vector two case
	        ) ; end of cond
	  ) ;; end while
	(SETTIME)
	result) ;; end of __viewAND


























































































;;**EXPORTKEY**:dataMineLib.__viewOR
(defchild dataMineLib:__viewOR(result vector1 vector2)
   ;; *******************************************************************
   ;; summary:  Combines two view vectors using the logical OR operation.
   ;; Args:     result       The result vector.
   ;;           vector1      The first input vector.
   ;;           vector2      The second input vector.
   ;; Return:   result       The result vector.
   ;; *******************************************************************
	vars:(time record1 record2 index1 index2)
	(STARTTIME)	
	(setq vector1 (sort (copy vector1) <))
	(setq vector2 (sort (copy vector2) <))
	(setq record1 vector1[(setq index1 0)])
	(setq record2 vector2[(setq index2 0)])
	(while (or (<> record1 #void) (<> record2 #void)) do
	  (cond ;; If records match only one may be returned.
	        ((= record1 record2)
	         (begin
	            (setq result[(length result)] record1)
	            (setq record1 vector1[(++ index1)])
	            (setq record2 vector2[(++ index2)])
	            )) ; end equal case
	        ;; If the first record is #void, we must promote vector two.
	        ((= record1 #void)
	         (begin
	            (setq result[(length result)] record2)
	            (setq record2 vector2[(++ index2)])
	            )) ; end promote vector two case
	        ;; If the second record is #void, we must promote vector one.
	        ((= record2 #void)
	         (begin
	            (setq result[(length result)] record1)
	            (setq record1 vector1[(++ index1)])
	            )) ; end promote vector one case
	        ;; If the first record is low, we must promote vector one.
	        ((< record1 record2)
	         (begin
	            (setq result[(length result)] record1)
	            (setq record1 vector1[(++ index1)])
	            )) ; end promote vector one case
	        ;; If the second record is low, we must promote vector two.
	        (else
	         (begin
	            (setq result[(length result)] record2)
	            (setq record2 vector2[(++ index2)])
	            )) ; end promote vector two case
	        ) ; end of cond
	  ) ;; end while
	(SETTIME)
	result) ;; end of __viewOR



















































































;;**EXPORTKEY**:dataMineLib.__viewXOR
(defchild dataMineLib:__viewXOR(result vector1 vector2)
   ;; *******************************************************************
   ;; summary:  Combines two view vectors using the logical XOR operation.
   ;; Args:     result       The result vector.
   ;;           vector1      The first input vector.
   ;;           vector2      The second input vector.
   ;; Return:   result       The result vector.
   ;; *******************************************************************
	vars:(time record1 record2 index1 index2)
	(STARTTIME)
	(setq vector1 (sort (copy vector1) <))
	(setq vector2 (sort (copy vector2) <))
	(setq record1 vector1[(setq index1 0)])
	(setq record2 vector2[(setq index2 0)])
	(while (or (<> record1 #void) (<> record2 #void)) do
	  (cond ;; If records match neither may be returned.
	        ((= record1 record2)
	         (begin
	            (setq record1 vector1[(++ index1)])
	            (setq record2 vector2[(++ index2)])
	            )) ; end equal case
	        ;; If the first record is #void, we must promote vector two.
	        ((= record1 #void)
	         (begin
	            (setq result[(length result)] record2)
	            (setq record2 vector2[(++ index2)])
	            )) ; end promote vector two case
	        ;; If the second record is #void, we must promote vector one.
	        ((= record2 #void)
	         (begin
	            (setq result[(length result)] record1)
	            (setq record1 vector1[(++ index1)])
	            )) ; end promote vector one case
	        ;; If the first record is low, we must promote vector one.
	        ((< record1 record2)
	         (begin
	            (setq result[(length result)] record1)
	            (setq record1 vector1[(++ index1)])
	            )) ; end promote vector one case
	        ;; If the second record is low, we must promote vector two.
	        (else
	         (begin
	            (setq result[(length result)] record2)
	            (setq record2 vector2[(++ index2)])
	            )) ; end promote vector two case
	        ) ; end of cond
	  ) ;; end while
	(SETTIME)
	result) ;; end of __viewXOR


























































































;;**EXPORTKEY**:dataMineLib.addTableColumn
(defchild dataMineLib:addTableColumn(tableName ...)
    ;; *******************************************************************
    ;; summary:  Add a new column to the specified table in the datamine.
    ;; Args:     tableName      Name of the table to create
    ;;           ...            zero or more column names
    ;; Return:   true
    ;; *******************************************************************
	vars:(i time indexOf extentIndex schema schemaRepo colVector colCount recordStructure validFields cursorTable)
	(STARTTIME)
	;; Load the schema for the specified table.
	(if (= (setq extentIndex (getTableExtent tableName)) false) 
	    (error "addTableColumn, The specified table does not exist."))
	    
	(setq schemaRepo dmExt[extentIndex].repos)
	(setq schema dmExt[extentIndex].repos[tableName]) ;tm! 

	(if (or (= schema.myObjectType schema.metaTable) (<> schema.metaTable #void))
		(error "addTableColumn: Attempt to modify schema of meta or member table"))

	(if (<> (setq cursorTable openTables[tableName]) #void)
	  (if (> (length cursorTable.cursorList) 0)
	       (loop for i from 0 until (length cursorTable.cursorList) do 
	          (if (= cursorTable.cursorList[i].myCursorType disk:)
	              (error (append "dataMineLib.addTableColumn: an exclusive disk cursor already exists on table " tableName))
	          )
	       );i
	  ); if
	); if
	
	;; Collect any column names.
	(if (and (= (argCount) 2) (= (type (argFetch 1)) ObjVector:))
	    (setq colVector (copy (argFetch 1)))
	    else
	    (begin
	       (setq colVector (new Vector: object: 0))
	       (loop for indexOf from 1 until (argCount) do
	           (setq colVector[(sub1 indexOf)] (symbol (argFetch indexOf)))
	           ) ;; end of column name loop
	    )) ; end if
	(setq schema.colVector (append schema.colVector colVector))
	(setq schema.colCount (length schema.colVector))
	(if (= schema.colCount 0)
	    (begin
	       (setq schema.recordStructure (new Vector: 0))
	       (setq schema.validFields (new Structure:))
	       ) ;; end then
	    (begin
	       (setq schema.recordStructure (new Vector: (length schema.colVector)))
	       (setAttributes schema.recordStructure schema.colVector)
	       (setq schema.validFields (objectToStructure schema.colVector #(#void)))
	       )) ;; end if
	(setq schemaRepo[(symbol tableName)] schema)
	(SETTIME)
	true) ;; end of addTableColumn




;;**EXPORTKEY**:dataMineLib.clearDataMine
(defchild dataMineLib:clearDataMine()
    ;; *******************************************************************
    ;; summary:  Clear the data mine.
    ;;           
    ;; Args:     none
    ;;
    ;; Return:   true
    ;; *******************************************************************
	vars:(time indexOf)
	(STARTTIME)
	;; Clear the multiple repositories and initialize.
	(setq currentTable #void)
	(loop for indexOf from 0 until (length dmExt) do
	    (abortTransaction dmExt[indexOf].repos)
	    (clear dmExt[indexOf].repos)
	    ) ;; end repository extent loop
	(usingForCreate dmExt[0 0] Default:)
	(setq openTables (new Dictionary:))
	(setq openBufs (new Dictionary:))
	(setq dataMineTables (new Dictionary:))
	(setq databaseNames (new Vector: 0))
	(__collectTableNames reload:)
	(SETTIME)
	true) ;; end of clearDataMine


























































































;;**EXPORTKEY**:dataMineLib.clearMemoPad
(defchild dataMineLib:clearMemoPad()
    ;; *******************************************************************
    ;; summary:  Clear the data mine memo pad
    ;;
    ;; Args:     none
    ;;
    ;; Return:   true
    ;; *******************************************************************
	(setq memoPad (new Directory:))
    true) ; end clearMemoPad
























































































;;**EXPORTKEY**:dataMineLib.createMetaTable
(defchild dataMineLib:createMetaTable(argMetaTable argMemberTables argKeyCol argMaxRowsPerTable)
	;; *******************************************************************
	;; Summary:  Creates a meta table. A meta table is a table comprised of
	;; one or more previously created member tables. Each of these member
	;; tables must have the same table schema definition. A meta table 
	;; can be opened as a Memory, Static or Disk cursor. 
	;; Return:   true 
	;; *******************************************************************
	pvars:(throwError
			memberTableExtent)
	vars:(
	    bckVector			;Vector containing frame ids of all underlying member table records
	    begTime			
		colVector
		colVector2
		indexDirectory
		keyVal
		len
	    metaTableSchema
	    memberTableSchema
	    memberTableBckVector
	    memberTableRepos		;; The repository the member table resides in
		numTables
		memberTableDirectory	    ;; directory of tables for metatable. 

	    sjIndex					;; Structure used to collect and save star join index information to the repository
	    	sjIndexBlockSize	;; maximum size of member table
		    sjTableDirectory	;directory of tableID/table pairs. TableID is the unique value assigned to each table
		    sjBckVectorIndex	;index into bckVector	
		    sjUniqueIDs			;directory for traslating from a unique column value to an offset value.

		tableEntry
	    i j s k m o p
	    )


	(defun throwError(message) 
		(if (<> memberTableExtent -1)
			(__rollbackTrans memberTableExtent))
		(__rollbackTrans usingExtIndex)
		(error (append "dataMineLib.createMetaTableWithsjBckVectorIndex Error: "  message))
	true)


	(STARTTIME)	
	(writeln "Creating meta table " argMetaTable " with Composite sjBckVectorIndex")

	(__beginTrans usingExtIndex)

	(setq begTime (getTickCount 0))
	(setq memberTableExtent -1)
	(setq bckVector (new Vector: number: 0))
	(setq sjBckVectorIndex (new Vector: integer: 0))
	(setq numTables (length argMemberTables))

	;; Validate arguments
	(if (= (length argMemberTables) 0) (throwError "missing table entries Directory"))

	;Create the memberTableDirectory to be updated when loading tables
	(setq memberTableDirectory (new Directory:)) ;the memberTableDirectory is keyed by the tableKey
	(loop for i from 0 until numTables do
		(setq tableEntry (new Structure: tableName: (symbol argMemberTables[i 1]) recordCount: 0 dirty: false))
		(setq memberTableDirectory[argMemberTables[i 0]] tableEntry)
	);i

	(setq sjIndexBlockSize argMaxRowsPerTable )

	(setq k 0) ; current metaTable bckVector sjBckVectorIndex
	(setq sjUniqueIDs (new Directory:))

	;; Append each member table sjBckVectorIndex content to the metaTables bckVector
	(loop for i from 0 until numTables do
		(writeln argMemberTables[i 1] " memstat=" (memstat false))

		;load member table's schema directly (avoid overhead of full cursor creation)	
		(setq memberTableExtent (getTableExtent (symbol argMemberTables[i 1])))
		(setq memberTableRepos dmExt[memberTableExtent].repos)

		(__beginTrans memberTableExtent)

		(setq memberTableSchema memberTableRepos[(symbol argMemberTables[i 1])]); load table schema from repository by tablename key

		; Grab intial schema for metaTable from first member table
		(if (= i 0) 
			(begin
			;Column Vector			
			(setq colVector memberTableSchema.colVector)
			(setq uniqueIDColNum (member argKeyCol colVector)) 
			end)
		);if

		; Grab column Vector of current table
		(setq colVector2 memberTableSchema.colVector)
		
		; Check that table has the same column vector as the first table loaded
		(if (<> colVector colVector2) then 
			(throwError (append argMemberTables[i 1] " has a different structure than that of the first member table")))

		;Check that member table does not have any indices defined on it
		(if (isMember indexDirectoryKey: metaTableSchema)
			(if (<> memberTableSchema.indexDirectoryKey #void)
				(begin
				(setq indexDirectory usingRepos[frame: memberTableSchema.indexDirectoryKey])
				(if (> (length indexDirectory) 0)
					(throwError (append argMemberTables[i 1] " has indices defined on it.")))
				end)
			);if
		);if

		;Check that member table does not already belong to some other metaTable and if it is available assign it to this metaTable
		(if (isMember metaTable: memberTableSchema) ;does the metaTable variable exist in the member table schema structure?
			(begin
			(if (= memberTableSchema.metaTable #void) ; has metaTable been set?
				(begin
				(setq memberTableSchema.metaTable argMetaTable)
				(setq memberTableRepos[(symbol argMemberTables[i 1])] memberTableSchema)
				end)
			);if
			(if (<> memberTableSchema.metaTable argMetaTable) ; make sure it is set to the current meta table!
				(throwError (append argMemberTables[i 1] " belongs to a different metaTable named " memberTableSchema.metaTable)))							
			end)
		else
			(begin ; we have a clean slate so just add the metaTable variable to the member table's schema structure
			(setq memberTableSchema.metaTable argMetaTable)
			(setq memberTableRepos[(symbol argMemberTables[i 1])] memberTableSchema)
			end)
		);if

		;Load the underlying tables bckVector
		(setq memberTableBckVector memberTableRepos[frame: memberTableSchema.bckVectorKey])
		(setq numRecs (length memberTableBckVector))
		(setq memberTableDirectory[i 1].recordCount numRecs)

		(if (> numRecs argMaxRowsPerTable)
			(throwError (append argMemberTables[i 1] " has more rows than allowed by the MaxRowsPerTable argument")))
	
		(resize bckVector (+ (length bckVector) numRecs))
		(resize sjBckVectorIndex (+ (length sjBckVectorIndex) argMaxRowsPerTable))

		(setq len (length sjBckVectorIndex))
		(loop for j from (- len argMaxRowsPerTable) until len (setq sjBckVectorIndex[j] -1))
		
		(loop for j from 0 until numRecs do
			(setq bckVector[k] memberTableBckVector[j])
			
			(setq r memberTableRepos[frame: memberTableBckVector[j]])
			(setq keyVal r[uniqueIDColNum])
			
			(if (not (isMember keyVal sjUniqueIDs))
				(setq sjUniqueIDs[keyVal] (length sjUniqueIDs)))

			(if (> (length sjUniqueIDs) sjIndexBlockSize)
				(begin
				(writeln _eol "sjIndexBlockSize=" sjIndexBlockSize _eol "sjUniqueIDs=" sjUniqueIDs)
				(error (append "__updateMetaTable: sjUniqueIDs directories length has grown larger than sjIndexBlockSize. Rebuild metaTable with larger sjIndexBlockSize!"))
				end)
			);if
						
			(setq sjBckVectorIndex[(+ (* i sjIndexBlockSize) sjUniqueIDs[keyVal]) ] k)

			(setq k (addi k 1))
		);j
		(__commitTrans memberTableExtent) 
		(setq memberTableExtent -1)
	); loop on i

	;; Create meta table
	(createTable argMetaTable colVector)

	(setq metaTableSchema usingRepos[(symbol argMetaTable)])
	(setq metaTableSchema.bckVectorKey (setq usingRepos[frame: #void] bckVector))
	(setq metaTableSchema.recordCount (length bckVector))
	(setq metaTableSchema.myObjectType metaTable:)
	(setq metaTableSchema.memberTableDirectoryKey (setq usingRepos[frame: #void] memberTableDirectory))

	(setq sjIndex (new Structure: 
		sjIndexBlockSize: sjIndexBlockSize 
		sjUniqueIDs: sjUniqueIDs 
		sjBckVectorIndex: sjBckVectorIndex
		sjRowKeyCol: argKeyCol))

	(setq metaTableSchema.sjIndexKey (setq usingRepos[frame: #void] sjIndex))

	(setq usingRepos[(symbol argMetaTable)] metaTableSchema)

	(__commitTrans usingExtIndex)

	(writeln " took " (getTickCount begTime) " seconds" _eol)	
	(SETTIME)
	
	true) ;; end of createsjBckVectorIndex
	














;;**EXPORTKEY**:dataMineLib.createMetaTableByIndex
(defchild dataMineLib:createMetaTableByIndex(argMetaTable argMemberTables argTableKeyName argIndexName)
	;; *******************************************************************
	;; Summary:  Creates a meta table. A meta table is a table comprised of
	;; one or more previously created member tables. Each of these member
	;; tables must have the same table schema definition. A meta table 
	;; can be opened as a Memory, Static or Disk cursor. However, no updates
	;; to the underlying records belonging to member tables are allowed. In
	;; addition, no addition or deletion of records are allowed. The Disk
	;; cursor (only cursor allowing updates) checks the type of table prior
	;; to performing any of these operations.
	;;
	;; createMetaTableByIndex creates the meta table by loading the specified index
	;; for each member table. Unlike createMetaTable, no sort/merge process is required
	;  for createMetaTableByIndex. Instead the newly created meta table consists of a 
	;; bckVector created by simple appends of each member table bckVector. An index on the
	;; metaTable is created that has the same key definitions specified in member table
	;; index, specified by the argIndexName argument, with the additional argTableKeyName
	;; column preprended in the metaTable version of the index.
	;;
	;; Limitations:
	;;	- No row level Updates
	;;	- All tables must be in the same extent
	;;	- The meta table is created in the same extent as the member tables
	;; 	- The meta table must be rebuilt if changes are made to the underlying
	;;		member tables
	;;
	;; Args:	argMetaTable    The name of the meta table to create
	;;        	argMemberTables	Vector of tableKey entries. Each tableKey entry is a vector
	;							of the form #(tableName tablekey).
	;;			argTableKeyName	The name of the key column whose value is specified in the tableKey entries.
	;;			argIndexName	The member table index name to use to build the meta table.		
	;; Return:   true 
	;; Test statement
	;; *******************************************************************
	pvars:(throwError)
	vars:(
		
		indexDictionary
		indexLen
		keySpec
		metaIndex
		memberKey
		memberIndex
	    myBckVector
	    myBegTime			
		myColVector
		myColVector2
		myIndexSpec
		myKey 
		myNumTables
		myMemberTableIndexDictionary
	    myMemberTableSchema
	    myTables
	    myTableSchema
	    myValue
	    numCols
	    tableKey
	    transStruct
	    vlen
	    i j s k m
	    )

	(defun throwError(message)
		(__rollbackTrans usingExtIndex)
		(error (append "dataMineLib.createMetaTableByIndex Error: "  message))
	true)


	(STARTTIME)	
(writeln "createMetaTable is vestigial -- use createMetaTableWithStarJoinIndex instead")
(return true)	


	(display "Creating meta table " argMetaTable " by using member table indices " argIndexName)

	(setq myBegTime (getTickCount 0))

	(setq myBckVector (new Vector: number: 10000))
	(setq k 0) ; First available index position in myBckVector

(writeln _eol "usingExtIndex=" usingExtIndex " usingExt=" usingExt)
(writeln "argMetaTable=" argMetaTable " argMemberTables=" argMemberTables " argTableKeyName=" argTableKeyName " argIndexName=" argIndexName)

	;setup transStruct for use when opening indices. transStruct allows the index object
	;to use the same transaction calls as the dataMineLib
	(setq transStruct (new Structure:))
	(setq transStruct.__beginTrans __beginTrans)
	(setq transStruct.__commitTrans __commitTrans)
	(setq transStruct.__rollbackTrans __rollbackTrans)
	
	;; Validate arguments
	(if (= (length argMemberTables) 0) (throwError "missing table entries list"))


	;; Create myTables vector	
	(setq myTables (new Vector:))
	(loop for i from 0 until (length argMemberTables) (setq myTables[i] argMemberTables[i][0]))
	
	
(writeln "myTables=" myTables)
	
	;; Validate that all named tables exist in the current extent
	(setq myNumTables (length myTables))
	(loop for i from 0 until myNumTables do
		(if (<> usingExtIndex (getTableExtent myTables[i])) then
			(throwError (append myTables[i] " table is not in the current extent: " usingExt )))
	); end of loop

	(__beginTrans usingExtIndex)


	;; Append each member tables index content to the new metaIndex
	(loop for i from 0 until myNumTables do

		(writeln myTables[i] " memstat=" (memstat false))
		;load member table's schema directly (avoid overhead of full cursor creation)	

		(setq myMemberTableSchema usingRepos[(symbol myTables[i])]); load table schema from repository by tablename key

		; Grab intial schema for metaTable from first member table
		(if (= i 0) 
			(begin
			;Column Vector			
			(setq myColVector myMemberTableSchema.colVector)
			end)
		);if

		; Grab column Vector of current table
		(setq myColVector2 myMemberTableSchema.colVector)
		
		; Check that table has the same column vector as the first table loaded
		(if (<> myColVector myColVector2) then 
			(throwError (append myTables[i] " has a different structure than that of the first member table")))
  
		; Check that the table has indices and then load the index directory if it exists
  		(if (= myMemberTableSchema.indexDictionaryKey #void)
  			(throwError (append myTables[i] " member table has no indices")))
  
		(setq myMemberTableIndexDictionary usingRepos[frame: myMemberTableSchema.indexDictionaryKey])
		
 		(if (or (= myMemberTableIndexDictionary #void) (= (length myMemberTableIndexDictionary) 0))
  			(throwError (append myTables[i] " member table has no indices")))

		; Make sure the index dictionary contains the index specified by the argIndexName argument
		(if (not (isMember argIndexName myMemberTableIndexDictionary))
			(throwError (append myTables[i] " member table has no index named " argIndexName)))
		
		; Grab the index specification if this is the first table
		(if (= i 0)
			(begin
			(setq myIndexSpec myMemberTableIndexDictionary[argIndexName])
			;; Create new index for metaTable
			;; Note that this is a raw index object here and is not yet part of a metaTable, we have not yet created the
			;; metaTable. It will be assigned to the metaTable later
			(setq numCols (+ myIndexSpec.numCols 1))
			(setq metaIndex (new index #void usingRepos create: numCols transStruct unique:))
			end)
		);if
		
		; Check that current table has the same index specification as first member table
		(setq myIndexSpec2 myMemberTableIndexDictionary[argIndexName])
		(if (or 
			(<> myIndexSpec.keySpec myIndexSpec2.keySpec) 
			(<> myIndexSpec.uniqueSW myIndexSpec2.uniqueSW)
			);or
			(throwError (append (myTables[i] " member table has different index specification than first member table"))))

		; Check to make sure that keySpec is a structure instead of a String
		(if (not (isStructure myIndexSpec2.keySpec))
			(throwError (append (myTables[i] " member table index keySpec must be a structure for use in a metaTable"))))

		; Create the member table index object
		(setq memberIndex (new index myMemberTableIndexDictionary[argIndexName].frameID usingRepos transStruct)) 

		; Loop through member tables index updating member index 
		(setq tableKey argMemberTables[i][1])
		(setq indexLen (memberIndex.Pv.length))
		(loop for j from 0 until indexLen do
			; Remember that keys are vectors of column values.
			(setq memberKey memberIndex[j 0])

			(if (= memberKey #void)
				(throwError (append "memberKey was void in table " myTables[i] " row=" j)))

			(if (> numCols 2)
				(begin
				(setq myKey (copy memberKey))
				(insert mKey 0 tableKey)
				end)
			else
				(begin
				(setq myKey (new Vector: 2 tableKey memberKey)); the member table has a single column index defined
				end)
			);if
				
			(setq myValue memberIndex[j 1])
			(setq metaIndex[myKey] myValue); insert key and value(s) into metaTable Index

			; increase the size of myBckVector in 1000 entry blocks
			(if (< (- (length myBckVector) k) 1000) (resize myBckVector (+ (length myBckVector) 10000)))
			
			;Append values to bckVector of metaTable
			(if (isVector myValue)
				(begin
				(setq vlen (length myValue))
				(loop for m from 0 until vlen 
					(setq myBckVector[k] myValue[m])
					(setq k (+ k 1))
				);m
				end)
			else
				(begin
				(setq myBckVector[k] myValue)
				(setq k (+ k 1))
				end)
			);if
		);j 
		(memberIndex.Pv.close); closing the index does not write to repository because we did not update anything  
		
	); loop on i

	(resize myBckVector k); resize myBckVector to only the number of entries actually used

 	;; Create index specification for meta table
	(setq myIndexSpec (copy myIndexSpec)); copy the index specification loaded from first member table
	(setq myIndexSpec.index #void)
	(setq myIndexSpec.frameID (metaIndex.Pv.close))
	(setq tableKeySpec (new Structure: argTableKeyName A:))
	(setq myIndexSpec.keySpec (append tableKeySpec myIndexSpec.keySpec)) 
	(setq myIndexSpec.state Current:)
	(setq myIndexSpec.getKey #void)
	(setq myIndexSpec.numCols numCols)
	(setq myIndexSpec.keyCompare #void)
	(setq myIndexSpec.noUpdating true) ;This flag disables modifications on the index specified by argIndexName

	
	;; Create meta table
	(createTable argMetaTable myColVector)

	(setq myTableSchema usingRepos[(symbol argMetaTable)])
	(setq myTableSchema.bckVectorKey (setq usingRepos[frame: #void] myBckVector))
	(setq myTableSchema.recordCount (length myBckVector))
	(setq myTableSchema.myObjectType metaTable:)
	(setq indexDictionary (new Dictionary:))
	(setq indexDictionary[argIndexName] myIndexSpec)
	(setq myTableSchema.indexDictionaryKey (setq usingRepos[frame: #void] indexDictionary))

	;; Save meta table schema
	(setq usingRepos[(symbol argMetaTable)] myTableSchema)

	(__commitTrans usingExtIndex)

	(display " took " (getTickCount myBegTime) " seconds" _eol)	
	(SETTIME)
	
	true) ;; end of createIndex
	





























;;**EXPORTKEY**:dataMineLib.createMetaTable_old
(defchild dataMineLib:createMetaTable_old(theMetaTable theTables theKeys ...)
	;; *******************************************************************
	;; Summary:  Creates a meta table. A meta table is a table comprised of
	;; one or more previously created member tables. Each of these member
	;; tables must have the same table schema definition. A meta table 
	;; can be opened as a Memory, Static or Disk cursor. However, no updates
	;; to the underlying records belonging to member tables are allowed. In
	;; addition, no addition or deletion of records are allowed. The Disk
	;; cursor (only cursor allowing updates) checks the type of table prior
	;; to performing any of these operations.
	;;  
	;; Limitations:
	;;	- No row level Updates
	;;	- All tables must be in the same extent
	;;	- The meta table is created in the same extent as the member tables
	;; 	- The meta table must be rebuilt if changes are made to the underlying
	;;		member tables
	;;
	;; Args:	theMetaTable     The name of the meta table to create
	;;        	theTables    	Vector of tables to index.
	;;         	theKeys      	Structure containing sort specification ie: #{column: A:|D: ...}
	;;                        	ex: #{ID: A: Cash: D:}
	;;							Pass an empty strcture if you don't want to specifiy any column sort. This will force nomerge: and nosort:
	;;			theOptions		Options (in any order)
	;;							nomerge:		 -- This option forces the metaTable to skip the merge step. The bckVectors of
	;;												member tables are simply appended to create the bckVector of the metaTable.
	;;												Note that the member tables will still be sorted if the argument theKeys
	;;												contains a sort specification structure. 
	;;							nosort:			 --	This option forces metaTable to assume that the underlying tables are sorted
	;;												as specified in the sort specifciation structure. This may compromise the 
	;;												merge process which assumse the member tables are sorted. When used in 
	;;												conjunction with the nomerge: option you get a simple append of the existing
	;;												member table bckVectors.
	;; Return:   true 
	;; Test statement
	;;(dataMineLib.clearCursors)(dataMineLib) (dataMineLib.usingForCreate test_a: "Test_A")(dataMineLib.createMetaTable myMetaTable3: #(Stocks19970827: Stocks19970903: ) #(ID: A: CurrentDate: D:))
	;; *******************************************************************
	vars:(
		time
	    myBckVector
	    myBegTime
		myColVector
		myColVector2 
	    myCursor 
	    myField
	    myInputCursor 
	    myInputRecord 
	    myMergeSource 
	    myMergeOption
	    myOption
	    myNewRecords
	    myNoSortOption
	    myNumArgs
		myNumTables
		myNumKeys
		myNumMergeRecs
	    myRecord 
	    myRecordCount
	    myRepos
	    mySortLambda
	    mySortVector
	    mySourceBckVector
	   	myStr
	    myTblExt
	    myTblExt2
	    myTableSchema
	    myValidfields
	    i iRow j k m t

	    )
	(STARTTIME)
	
(writeln "createMetaTable is vestigial -- use createMetaTableWithStarJoinIndex instead")
(return true)	
		
	(display "Creating meta table " theMetaTable)
	(setq myBegTime (getTickCount 0))
	
	;; Validate arguments
	(if (<= (length theTables) 0) (error "dataMineLib.createIndex -- missing table list"))
;	(if (<= (length theKeys) 0) (error "dataMineLib.createIndex -- missing key list"))
	;; Validate that all named tables exist in the same extent
	(loop for i from 0 until (length theTables) do
	(if (= i 0) then (setq myTblExt (getTableExtent theTables[i])))
	(setq myTblExt2 (getTableExtent theTables[i]))
		(if (<> myTblExt (getTableExtent theTables[i])) then
		(error (append "dataMineLib.createMetaTable --" theTables[i] " myTblExt=" myTblExt  " myTblExt2=" myTblExt2 " -- tables in different extents"))   		
 	); end of if
	); end of loop
	
	
	;; Validate that all named tables have identical schemas
	(setq myNumTables (length theTables))
	(loop for i from 0 until myNumTables do
	  (setq myCursor (open theTables[i] static:))
	  (if (= i 0) (begin
	  		(setq myColVector myCursor.colVector)
	  		(setq myValidFields myCursor.validFields)
	  		end)
	  ); if
	  (setq myColVector2 myCursor.colVector)
	  (setq myCursor (close myCursor))
	  (if (<> myColVector myColVector2) then 
	     (error "dataMineLib.createMetaTable -- mismatched table structures")
	  );if 
	); loop on i

	;; Validate that sort specifications structure against table schema
	(setq myNumKeys (length theKeys))
	(loop for i from 0 until myNumKeys do
		(if (= (isMember theKeys[i 0] myValidFields) false) ;; theKeys[i 0] is the columnname
	     	(error (append "dataMineLib:createMetaTable -- sort column " theKeys[i 0] " not found in table " theTable))
	  	)
	  	(if (= (isMember theKeys[i 1] #(A: D:)) false) ;; theKeys[i 1] is the sort A: or D: attribute
	  		(error (append "dataMineLib:createMetaTable -- sort attribute not A: or D: in sort column " theKeys[i 0] " in table " theTable))
	  	)       
	);end of loop

 	;; Collect and Validate options
 	(setq myNoSortOption false)	; set default
 	(setq myNoMergeOption false) ; set default
 	(setq myNumArgs (argCount))
	(loop for i from 2 until myNumArgs do
		(setq myOption (argFetch i))
	 	(cond
	 		((= myOption nosort:) (setq myNoSortOption true))
	 		((= myOption nomerge:) (setq myNoMergeOption true))
	 		else (error (append "dataMineLib:createMetaTable -- bad option " myOption " passed for table " theTable))
	 	)
	)

	;Overide sort and merge options if no keyspec provided
	(if (= (length theKeys) 0) (begin
	    (setq myNoSortOption true) ; can't sort if no columns provided
	    (setq myNoMergeOption true) ; assume a merge is not possible
		end)
	);if

	;tm - should we check that theMetaTable is in the current extent or does isTable respect the current usingExtent?
	(if (isTable theMetaTable) (dropTable theMetaTable) end)
	
	;; Create meta table
	(createTable theMetaTable myColVector)

	;; Build compare Lambda for merge process
	(if  (> (length theKeys) 0)
		(begin
	    (if (= myNumKeys 1)
	    	(begin
	   		(if (= theKeys[0 1] A:)
	      		(setq mySortLambda (lambda(x y) (< x[0] y[0])))
	      		(setq mySortLambda (lambda(x y) (> x[0] y[0])))
	   		);end if
	     		
	     	end)
	     ;; else Need a multi level sort
			(begin
			;; Create expression of the form shown below ex: #{column1: A: column2: A: column3: D:}
			;(lambda (x y)
			;	(cond 
			;	((< x[0] y[0]) true) 
			;	((and (= x[0] y[0]) (< x[1] y[1])) true)
			;	((and (= x[0] y[0]) (= x[1] y[1]) (> x[2] y[2])) true)
			;	else false)
			;	)
			(setq mySortLambda "")
	     	(setq mySortLambda (append mySortLambda "(lambda(x y) (cond ((" (if (= theKeys[0 1] A:) "<" ">") " x[0] y[0]) true) " ))
	     	(loop for i from 1 until myNumKeys do                                                      
				(setq myStr " ((and") ; ))
				(loop for j from 0 until i do ; create equality conditions for previous keys
					(setq myStr (append myStr "(= x[" j "] y[" j "]) ")) ;
				)
	       		(setq mySortLambda (append mySortLambda myStr "(" (if (= theKeys[i 1] A:) "<" ">") " x[" i "] y[" i "])) true)"))
		    ); end loop 
		  	(setq mySortLambda (append mySortLambda " else false))"))
	;		(writeln mySortLambda)
			(setq mySortLambda (eval mySortLambda))
	   		end)
	 	); end if
		end)
	);if
	
	;;Build the meta table bckVector from member table sort results 
	(loop for i from 0 until myNumTables do
		(if (= mySortVector #void)
			(begin
;			(writeln _eol "1. i="i " theTables[i]=" theTables[i])
			(if myNoSortOption
				(setq mySortVector (sortTable theTables[i] theKeys returnVectorAndKeys: noSort: noSave:))
				(setq mySortVector (sortTable theTables[i] theKeys returnVectorAndKeys: noSave:))
			); if
			end)
		else
			(begin ; Merge / Append 
			(if myNoMergeOption ; simple append of member table bckVectors (after sort) 
				(begin
;				(writeln _eol "2. i="i " theTables[i]=" theTables[i])
				(if myNoSortOption 
					(setq mySortVector (append mySortVector (sortTable theTables[i] theKeys returnVectorAndKeys: noSort: noSave:)))
					(setq mySortVector (append mySortVector (sortTable theTables[i] theKeys returnVectorAndKeys: noSave:)))
				);if
				end) 
			else
				(begin
;				(writeln _eol "3. i="i " theTables[i]=" theTables[i])
				(if myNoSortOption 
			  		(setq myMergeSource (sortTable theTables[i] theKeys returnVectorAndKeys: noSort: noSave:))
			  		(setq myMergeSource (sortTable theTables[i] theKeys returnVectorAndKeys: noSave:))
				);if			  		
		  		(setq myNumMergeRecs (length myMergeSource))
		  		(setq myRecordCount (length mySortVector))
		  		(setq t 0); initial position in mySortVector
		  		(setq m 0); initial position in myMergeSource
		  		(while (and (< m myNumMergeRecs)(< t myRecordCount)) do
		  			(if (or (> t myRecordCount) (mySortLambda mySortVector[t] myMergeSource[m] ))
		  				(begin
	;	  				(writeln "Skipping at mySortVector["t"]= " mySortVector[t])
	;	  				(writeln "           myMergeSource["m"]=" myMergeSource[m])
		  				(setq t (+ t 1))
		  				end)
					;else
		  				(begin
	;	  				(writeln "Inserting at mySortVector["t"]= " mySortVector[t])
	;	  				(writeln "            myMergeSource["m"]=" myMergeSource[m])
		  				(setq mySortVector (insert mySortVector t myMergeSource[m]))
		  				(setq t (+ t 1))
		  				(setq myRecordCount (+ myRecordCount 1))
		  				(setq m (+ m 1))
		  				end)
		  			);if
		  		); end while on m
				end)
			);if 
	  		end)
	  	);if
	); i          

; 	(setq k (if (> myRecordCount 10) 10 myRecordCount))
;	(setq k myRecordCount)
;	(loop for i from 0 until k do
;	 	(writeln mySortVector[i])
;	) 

	;; Create bckVector to save back into meta table
	(setq myRecordCount (length mySortVector))
	(setq myBckVector (new Vector: number: myRecordCount))
	(loop for i from 0 until myRecordCount do
	 	(setq myBckVector[i] mySortVector[i][myNumKeys])
	)

	(__beginTrans myTblExt)
	(setq myRepos dmExt[myTblExt].repos)
	(setq myTableSchema myRepos[(symbol theMetaTable)])
	(setq myTableSchema.bckVectorKey (setq myRepos[frame: #void] myBckVector))
	(setq myTableSchema.recordCount (length myBckVector))
	(setq myTableSchema.myObjectType metaTable:)
	(setq myRepos[(symbol theMetaTable)] myTableSchema)
	(__commitTrans myTblExt)

	(display " took " (getTickCount myBegTime) " seconds" _eol)	
	(SETTIME)
	
	true) ;; end of createIndex
	
	
	
	































;;**EXPORTKEY**:dataMineLib.createTable
(defchild dataMineLib:createTable(tableName ...)
    ;; *******************************************************************
    ;; summary:  Create the specified relational table in the datamine.
    ;; Args:     tableName      Name of the table to create
    ;;           ...            zero or more column specs
    ;; Return:   true
    ;; *******************************************************************
    vars:(indexOf schema colVector colCount recordStructure validFields extentIndex dbIndex compositeIndex)
    ;; Collect any column names.
	(STARTTIME)
    (if (and (= (argCount) 2) (= (type (argFetch 1)) ObjVector:))
        (setq colVector (copy (argFetch 1)))
        else
        (begin
           (setq colVector (new Vector: object: 0))
           (loop for indexOf from 1 until (argCount) do
               (setq colVector[(sub1 indexOf)] (symbol (argFetch indexOf)))
               ) ;; end of column name loop
        )) ; end if
    (setq colCount (length colVector))
    (if (= colCount 0)
        (begin
           (setq recordStructure (new Vector: 0))
           (setq validFields (new Structure:))
           ) ;; end then
        (begin
           (setq recordStructure (new Vector: (length colVector)))
           (setAttributes recordStructure colVector)
           (setq validFields (objectToStructure colVector #(#void)))
           )) ;; end if

    ;; Make sure the specified table does not exist
    (setq extentIndex (getTableExtent tableName))
    (if (<> extentIndex false) (error "dataMineLib:createTable" "The specified table already exists."))

	(if (=  usingExtIndex false)
		(error "dataMineLib:createTable: usingExtIndex is false"))
 
    ;; Save the new table schema record.
    (setq schema (new Structure:
                         bckVectorKey:       #void
                         colCount:           colCount
                         colVector:          colVector
                         myMemoPad:          (new Dictionary:)
                         myDatabaseName:     (symbol usingDatabase)
                         myObjectName:       (symbol tableName)
                         myObjectType:       table:
                         myTableName:        #void
                         myTableDate:        #void
                         recordCount:        0
                         recordStructure:    recordStructure
                         rowVectorKey:       #void
                         validFields:        validFields
                         viewDirectoryKey:   #void
                         )) ;; end table schema record

    ;; Save the new table schema into the extent repository.
    (__beginTrans usingExtIndex)
    (setq usingRepos[(symbol tableName)] schema)
    (__commitTrans usingExtIndex)

	(setq dmExt[usingExtIndex].extentTableNames[(length dmExt[usingExtIndex].extentTableNames)] tableName)
	(setq dmExt[usingExtIndex].extentDatabaseNames[(length dmExt[usingExtIndex].extentDatabaseNames)] usingDatabase)

	;; Update the dataMineTables dictionary and the databaseNames vector
	(setq dbIndex (member usingDatabase databaseNames))
	(if (not (isNumber dbIndex)) (begin
		(setq databaseNames[(length databaseNames)] usingDatabase)
		(setq dbIndex (member usingDatabase databaseNames))
		end))

	(setq compositeIndex (+ usingExtIndex (/ dbIndex 65536)))
	(setq dataMineTables[tableName] compositeIndex) 
	(SETTIME)
    true) ;; end of createTable


























































































;;**EXPORTKEY**:dataMineLib.dropTable
(defchild dataMineLib:dropTable(...)
    ;; *******************************************************************
    ;; summary:  Drop the specified object from the datamine.
    ;; Args:     ...      one or more object names to be dropped.
    ;; Return:   true
    ;; *******************************************************************
    vars:(time indexOf cursor extentIndex objectName i)
    ;; Collect any object names.
	(STARTTIME)
    (loop for indexOf from 0 until (argCount) do
        (setq objectName (symbol (argFetch indexOf)))
		; Make sure table is not open
		(if (<> openTables[objectName] #void)
			(error (append "dataMineLib:dropTable: " myObjectName " has open cursors.")))
     	
     	(if (<> metaTable #void)
     		(error (append "dataMineLib:dropTable: " myObjectName " is a member table")))
     	
        (if (<> (setq extentIndex (getTableExtent objectName)) false) 
            (begin
               (setq cursor (open objectName))  
  
               (setq cursor (cursor.destroyTable))
               (setq time (getTickCount 0))
;               (setq cursor (close cursor))
;               (__beginTrans extentIndex)
;               (setq dmExt[extentIndex].repos[objectName] #void) ; delete table from repository
;               (__commitTrans extentIndex)
              ;; Drop the table from the in-memory tables dictionary.
;               (^delete dataMineTables objectName)
;               (setq i (member objectName dmExt[extentIndex].extentTableNames))
;               (^delete dmExt[extentIndex].extentTableNames i)
;               (^delete dmExt[extentIndex].extentDatabaseNames i)
               (setq time (getTickCount 0))
               (__collectTableNames) 
            end)
    	);if
    ) ;; end of object name loop
	(SETTIME)
    true) ;; end of dropTable


























































































;;**EXPORTKEY**:dataMineLib.getDatabaseNames
(defchild dataMineLib:getDatabaseNames()
   ;; *******************************************************************
   ;; summary:  Returns the names of all databases in the datamine
   ;; Args:     none.
   ;; Return:   vector  vector of logical database names
   ;; Note:     
   ;; *******************************************************************
   vars:(nmGetDatabaseNames)
   (copy databaseNames)) ;; end of getDatabaseNames


























































































;;**EXPORTKEY**:dataMineLib.getExtentNames
(defchild dataMineLib:getExtentNames()
   ;; *******************************************************************
   ;; summary:  Returns the names of all physical extents currently 
   ;;           allocated to the data mine.
   ;; Args:     none.
   ;; Return:   vector  vector of logical database names
   ;; *******************************************************************
	vars:(time extentIndex extentCount schemaIndex schemaCount namesVector)
	(STARTTIME)	
	;; Create an empty result vector.
	(setq namesVector (new Vector: 0))
	;; Gather the names of all data mine extents.
	(setq extentCount (length dmExt))
	(loop for extentIndex from 0 until extentCount do
		(binaryInsert namesVector dmExt[extentIndex 0])
	) ; end extent loop
	(SETTIME)
	namesVector) ;; end of getExtentNames


























































































;;**EXPORTKEY**:dataMineLib.getTableDatabaseName
(defchild dataMineLib:getTableDatabaseName(tableName)
   ;; *******************************************************************
   ;; summary:  Returns the logical database name of the specified table 
   ;;           or view.
   ;; Args:     tableName     The table or view whose logical database name
   ;;                         is to be returned.
   ;; Return:   databasename  The logical database name of the table or view.
   ;; Note:     This function uses the in-memory extent table dictionary
   ;; *******************************************************************
	vars:(time databaseName compositeIndex dbIndex)
	(STARTTIME)
	;; Search of the table in the data mine.
	(if (not (isNumber (member tableName dataMineTables)))
	   (error (append "dataMineLib:getTableDatabaseNames: " tableName " :Could not find table of that name. "))
	   ) ; end if
	;; Return the logical database name of the table.
	(setq compositeIndex dataMineTables[tableName 1])
	(setq dbIndex (* (fraction compositeIndex) 65536))
	(SETTIME)
	databaseNames[dbIndex]) ;; end of getTableDatabaseName


























































































;;**EXPORTKEY**:dataMineLib.getTableNames
(defchild dataMineLib:getTableNames(...)
   ;; *******************************************************************
   ;; summary:  Returns the names of all tables 
   ;; Args:     databaseName(optional) Return only those tables which are
   ;;                                   associated with the specified 
   ;;                                   database name.
   ;; Return:   vector                 vector of table names
   ;; Note:     
   ;; *******************************************************************
	vars:(time i len dbIndex namesVector theDatabaseName)
	(STARTTIME)
	(setq namesVector (new Vector: 0))

	;; Retrieve the optional database name argument (if any).
	(setq dbIndex -1); default is invalid value
	(if (>= (argCount) 1)
		(begin
		(setq theDatabaseName (argFetch 0))
		;determine dbIndex
		(setq dbIndex (member theDatabaseName databaseNames))
		(if (not (isNumber dbIndex))
			(return namesVector))
		end)
	);if
	
	;; Fill the result vector.
	(setq len (length dataMineTables)) 

	(if (= dbIndex -1)
		(loop for i from 0 until len do
			(setq namesVector[(length namesVector)] dataMineTables[i 0])
		); i		
	else
		(loop for i from 0 until len do
			(if (= dbIndex (* (fraction dataMineTables[i 1]) 65536))
				(setq namesVector[(length namesVector)] dataMineTables[i 0])
			);if
		); i
	);if		
	(SETTIME)
	namesVector) ;; end of getTableNames



;;**EXPORTKEY**:dataMineLib.mergeTables
(defchild dataMineLib:mergeTables(tableName1 tableName2 mergeField mergeLambda)
   ;; *******************************************************************
   ;; summary:  Creates a new view of a table by logically combining two 
   ;;           other views of the same table.
   ;;
   ;; Args:     tableName1   1st table to merge
   ;;           tableName2   2nd table to merge
   ;;           mergeField   The name of the merge field
   ;;           mergeLambda   The merge Lambda.
   ;;
   ;; Note:     Both tables must already be sorted, in ascending order,
   ;;           on the specified merge field.
   ;;
   ;; Return:   true
   ;; *******************************************************************
	vars:(time cursor1 cursor2 record1 record2 index1 index2)
	(STARTTIME)	
	(setq cursor1 (open tableName1))
	(setq cursor2 (open tableName2))
	(setq record1 (cursor1.read (setq index1 0)))
	(setq record2 (cursor2.read (setq index2 0)))
	(while (and (<> record1 #void) (<> record2 #void)) do
	  (cond ;; If record match they must both be merged.
	        ((= record1[mergeField] record2[mergeField])
	         (begin
	            (mergeLambda record1 record2)
	            (cursor1.write index1 record1)
	            (cursor2.write index2 record2)
	            (setq record1 (cursor1.read (++ index1)))
	            (setq record2 (cursor2.read (++ index2)))
	            )) ; end merge case
	        ;; If the first record is low, we must promote table one.
	        ((< record1[mergeField] record2[mergeField])
	         (setq record1 (cursor1.read (++ index1)))
	            ) ; end promote table one case
	        ;; If the second record is low, we must promote table two.
	        (else
	         (setq record2 (cursor2.read (++ index2)))
	            ) ; end promote table two case
	        ) ; end of merge cond
	  ) ;; end merge while
	(close cursor1)
	(close cursor2)
	(SETTIME)
	true) ;; end of mergeTables





























































































;;**EXPORTKEY**:dataMineLib.mergeThreeTables
(defchild dataMineLib:mergeThreeTables(tableName1 tableName2 tableName3 mergeField mergeLambda)
   ;; *******************************************************************
   ;; summary:  Merges three tables based upon a matching merge field. 
   ;;           The first two tables are read searching for two records
   ;;           which match on the merge field. These two records and a
   ;;           blank template record from the third table are passed to
   ;;           the merge Lambda which merges data from the two matching
   ;;           input records into the record template which is written
   ;;           to the third table.
   ;;
   ;; Args:     tableName1   1st table to merge (input)
   ;;           tableName2   2nd table to merge (input)
   ;;           tableName3   3rd table to merge (output)
   ;;           mergeField   The name of the merge field
   ;;           mergeLambda   The merge Lambda.
   ;;
   ;; Note:     Both input tables must already be sorted, in ascending order,
   ;;           on the specified merge field.
   ;;
   ;; Return:   true
   ;; *******************************************************************
	vars:(time cursor1 cursor2 cursor3 record1 record2 record3 index1 index2 index3)
	(STARTTIME)
	(setq cursor1 (open tableName1 static: -1)) ;;tm changed to open cursor static unbuffered
	(setq cursor2 (open tableName2 static: -1)) ;;tm changed to open cursor static unbuffered
	(setq cursor3 (open tableName3 disk: -1)) ;;tim changed to ensure open unbuffered
	(setq record1 (cursor1.read (setq index1 0)))
	(setq record2 (cursor2.read (setq index2 0)))
	(setq index3 0)
	(while (and (<> record1 #void) (<> record2 #void)) do
	  (cond ;; If record match they must both be merged.
	        ((= record1[mergeField] record2[mergeField])
	         (begin
	            (setq record3 (copy cursor3.recordStructure))
	            (mergeLambda record1 record2 record3)
	            (cursor3.write index3 record3)
	            (++ index3)
	            (setq record1 (cursor1.read (++ index1)))
	            (setq record2 (cursor2.read (++ index2)))
	            )) ; end merge case
	        ;; If the first record is low, we must promote table one.
	        ((< record1[mergeField] record2[mergeField])
	         (setq record1 (cursor1.read (++ index1)))
	            ) ; end promote table one case
	        ;; If the second record is low, we must promote table two.
	        (else
	         (setq record2 (cursor2.read (++ index2)))
	            ) ; end promote table two case
	        ) ; end of merge cond
	  ) ;; end merge while
	(close cursor1)
	(close cursor2)
	(close cursor3)
	(SETTIME)
	true) ;; end of mergeThreeTables

















;;**EXPORTKEY**:dataMineLib.miner
(defriend dataMineLib:miner()
;; *******************************************************************
;;  summary:  The Miner Lambda manages data mining operations for the
;;            data mine. This includes creating mining projects, 
;;            testing filter Lambdas, and scoring the results.
;;
;;  Note1:    The miner operates on "projects" which must be loaded
;;            into memory to be edited, viewed, and backtested.
;;  Note2:    The miner projects are always associated with the
;;            "blackboard" database name.
;; *******************************************************************
   pvars:(;; Public variables
          currentProjectName         ;; The current project name being edited.
          currentProjectCursor       ;; The current project table cursor being edited.
          currentProjectIndex        ;; The current project index cursor being edited.
          currentTestingSW           ;; The current project is testing (true); otherwise false.
          testRunEndSW               ;; True iff this is the last table being tested in a miner test run.
          testRunFilterName          ;; The name of the filter being tested in a miner test run.
          testRunStartSW             ;; True iff this is the first table being tested in a miner test run.
          testRunProjectName         ;; The name of the project being tested in a miner test run.
          ;; Public methods
          abortProject               ;; Closes the current project without saving changes.
          addProjectFilter           ;; Adds or updates a filter Lambda to the project.
          addProjectScore            ;; Adds or updates the score Lambda for the project.
          addProjectTable            ;; Adds a new table name to the miner.
          clearProjectScores         ;; Clears all scores in the project.
          dropAutoFilters            ;; Deletes all auto filter Lambdas in the project.
          dropProject                ;; Erases the blackboard table for a miner project.
          dropProjectFilter          ;; Deletes a filter Lambda in the project.
          dropProjectTable           ;; Deletes a table name from the miner.
          getFilterLambda             ;; Returns the Lambda for the specified filter name.
          getFilterHistory           ;; Returns the history record for the specified filter name.
          getFilterNames             ;; Returns a vector of sorted filter names (by project and score).
          getFilterScores            ;; Returns the scores record for the specified filter name.
          getFilterSource            ;; Returns the source for the specified filter name.
          getProjectNames            ;; Returns a vector of all project names.
          getProjectTable            ;; Returns the name of the specified testing table in the project.
          getScoreName               ;; Returns score name for the specified project.
          getScoreSource             ;; Returns the source for the Score Lambda.
          newProject                 ;; Adds a new project to the miner.
          openProject                ;; Opens the current project as specified.
          registerProjectsAsExtents  ;; Register all available Blackboard Project Tables as browseLib cabinet extents.
          renameProjectFilter        ;; Renames a filter Lambda in the project.
          renameProjectScore         ;; Renames the score Lambda in the project.
          runProjectFilter           ;; Runs the named filter on the specified table cursor.
          saveFilterSource           ;; Adds or updates a filter Lambda script to the project.
          saveProject                ;; Saves/clears the current project as specified.
          search                     ;; Search the current project for the specified record.
          testRun                    ;; Starts testing all filter Lambdas in the project.
          ;; Private variables
          (_headerCols 14)           ;; Number of header columns before testing tables start.
          _isDirty                   ;; The current project has been altered and needs saving.
          _testingNameIndex          ;; The current project index of row indicies by Name.
          _testingProjectName        ;; The current project name during a testing run.
          _testingTableName          ;; The current table name during a testing run.
          ;; Private methods
          _createNameIndex           ;; Creates an index to Filter Lambdas by Name.
          _minorExtentMgrTemplate    ;; Template for creating datamine miner browseLib extent manager Lambdas.
          _isCurrentProjectOpen      ;; Returns an error is there is no current project.
          _loadSource                ;; Replaces source escape sequences with tabs and eols.
          _saveSource                ;; Replaces source tabs and eols with escape sequences.
         ) ;; end of persistent variables
   ;; ****************************************************************
   ;; Define private child Lambdas
   ;; ****************************************************************
   ;; Creates an index to Filter Lambdas by Name.
   (defun _createNameIndex()
      vars:(record rowIndex nameIndex n)
      (STARTTIME)
      ;; Make sure there is a current project open.
      (_isCurrentProjectOpen)
      (currentProjectIndex.restore)
      (setq _testingNameIndex (new Dictionary:))
      (loop for rowIndex from 0 until currentProjectIndex.recordCount do
          (setq record (currentProjectIndex.read rowIndex))
          (setq _testingNameIndex[record.Name] rowIndex)
          ) ; end record loop          
	  (SETTIME)
      true) ; end createNameIndex
   ;; Returns an error is there is no current project.
   (defun _isCurrentProjectOpen()
      (STARTTIME)
      (if (or (= currentProjectCursor #void) (= currentProjectIndex #void))
          (error (append "miner: please open a miner project first")))
	  (SETTIME)
      true) ; end _isCurrentProjectOpen
   ;; Replaces source escape sequences with tabs and eols.
   (defun _loadSource(source)
		vars:(result)
		(STARTTIME)
		(setq result (substitute (substitute source "#$tab$#" #\tab) "#$eol$#" _eol))
		(SETTIME)
		result)
   ;; Replaces source tabs and eols with escape sequences.
   (defun _saveSource(source) 
		vars: (result)
		(STARTTIME)
		(setq result (substitute (substitute source #\tab "#$tab$#") _eol "#$eol$#"))
		(SETTIME)
		result)
   ;; ****************************************************************
   ;; Define public child Lambdas
   ;; ****************************************************************
   ;; Closes the current project without saving.
   (defun abortProject()
      vars:(projectName testingSW clearSW)
      (STARTTIME)
      
      ;; Make sure we close the current project (if any).
      (if (<> currentProjectCursor #void)
          (setq currentProjectCursor (close currentProjectCursor))
          ) ; end if
      (if (<> currentProjectIndex #void)
          (setq currentProjectIndex (close currentProjectIndex))
          ) ; end if
      (setq currentProjectName #void)
      (setq currentTestingSW #void)
      (setq _isDirty false)
	  (SETTIME)
      true) ; end abortProject
   ;; Adds or updates a filter Lambda to the project.
   (defun addProjectFilter(filterName filterSource)
      vars:(record index filterIndex)
      (STARTTIME)
      ;; Make sure there is a current project opened for edit.
      (_isCurrentProjectOpen)
      ;; Validate the filter source, do not add filter if compilation returns an error.
      (javaScript filterSource)
      ;; Update the current project with the new Score Lambda information.
      (currentProjectCursor.restore)
      (currentProjectIndex.restore)
      (setq filterIndex (search Name: filterName))
      (if (isNumber filterIndex)
          then
          (begin
            (setq index (copy currentProjectIndex.recordStructure))
            (setq record (copy currentProjectCursor.recordStructure))
            )
          else
          (begin
             (setq index (copy currentProjectIndex.recordStructure))
             (setq record (copy currentProjectCursor.recordStructure))
             (setq filterIndex currentProjectCursor.recordCount))
          ) ; end if
      (setq index.Row filterIndex)
      (setq index.Name filterName)
      (setq record.Name filterName)
      (setq record.Source (_saveSource filterSource))
      (setq record.Lambda #void)
      (setq index.Type 1)
      (setq record.Type 1)
      (currentProjectIndex.write filterIndex index)
      (currentProjectCursor.write filterIndex record)
      (setq _isDirty true)
	  (SETTIME)
	true) ; end addProjectFilter
   ;; Adds or updates the score Lambda for the project.
   (defun addProjectScore(scoreName scoreSource)
      vars:(record index scoreIndex)
      (STARTTIME)
      ;; Make sure there is a current project opened for edit.
      (_isCurrentProjectOpen)
      ;; Validate the score source, do not add score if compilation returns an error.
      (javaScript scoreSource)
      ;; Update the current project with the new Score Lambda information.
      (currentProjectIndex.restore)
      (currentProjectCursor.restore)
      (setq scoreIndex (search Type: 0))
      (if (isNumber scoreIndex)
          then
          (begin
             (setq index (currentProjectIndex.read scoreIndex))
             (setq record (currentProjectCursor.read scoreIndex))
             )
          else
          (begin
             (setq index (copy currentProjectIndex.recordStructure))
             (setq record (copy currentProjectCursor.recordStructure))
             (setq scoreIndex 0))
          ) ; end if
      (setq index.Row scoreIndex)
      (setq index.Name scoreName)
      (setq record.Name scoreName)
      (setq record.Source (_saveSource scoreSource))
      (setq record.Lambda #void)
      (setq index.Type 0)
      (setq record.Type 0)
      (currentProjectIndex.write scoreIndex index)
      (currentProjectCursor.write scoreIndex record)
      (clearProjectScores)
      (setq _isDirty true)
	  (SETTIME)
      true) ; end addProjectScore
   ;; Adds a table name to the project in the miner.
   (defun addProjectTable(projectName newTable)
      vars:(record rowIndex colIndex n)
      (STARTTIME)
      ;; Clear the current project (if any).
      (saveProject clear:)
      ;; Add the new column to the project table.
      (addTableColumn projectName newTable)
	  (SETTIME)
      true) ; end addProjectTable
   ;; Clears the scores for the project.
   (defun clearProjectScores()
      vars:(record index rowIndex colIndex n)
      (STARTTIME)
      
      ;; Make sure there is a current project opened for edit.
      (_isCurrentProjectOpen)
      (currentProjectIndex.restore)
      (currentProjectCursor.restore)
      (loop for rowIndex from 0 until currentProjectIndex.recordCount do
          (setq index (currentProjectIndex.read rowIndex))
          (loop for colIndex from 2 until (sub1 currentProjectIndex.colCount) do
              (setq index[colIndex] #void)
              ) ; end column loop
          (currentProjectIndex.write rowIndex index)
          ) ; end index loop
      (loop for rowIndex from 0 until currentProjectCursor.recordCount do
          (setq record (currentProjectCursor.read rowIndex))
          (loop for colIndex from 4 until currentProjectCursor.colCount do
              (setq record[colIndex] #void)
              ) ; end column loop
          (currentProjectCursor.write rowIndex record)
          ) ; end record loop
      (setq _isDirty true)
	  (SETTIME)
      true) ; end clearProjectScores
   ;; Deletes all auto filters from the project in the miner.
   (defun dropAutoFilters()
      vars:(dropLambda i)
      (STARTTIME)
      
      ;; Make sure there is a current project opened for edit.
      (_isCurrentProjectOpen)
      (currentProjectIndex.restore)
      (currentProjectCursor.restore)
      ;; Drop all auto filters from the Miner's list of filters.
      (setq dropLambda (compileLambda (append "omit left(Name,5) == \"_Auto\";")))
      (dropLambda currentProjectIndex)
      (dropLambda currentProjectCursor)
      (loop for i from 0 until currentProjectIndex.recordCount do (setq currentProjectIndex.rowVector[i].Row i))
      (setq _isDirty true)
	  (SETTIME)
      true) ; end dropAutoFilters
   ;; Erases the blackboard table for a miner project.
   (defun dropProject(projectName)
      (STARTTIME)
      (saveProject clear:)
      (dropTable projectName)
	  (SETTIME)
      true) ; end dropProject
   ;; Drops a filter name from the project in the miner.
   (defun dropProjectFilter(filterName)
      vars:(record filterIndex i)
      (STARTTIME)
      ;; Make sure there is a current project opened for edit.
      (_isCurrentProjectOpen)
      ;; Update the current project with the new Score Lambda information.
      (currentProjectIndex.restore)
      (currentProjectCursor.restore)
      (setq filterIndex (search Name: filterName))
      (if (isNumber filterIndex)
          then
          (begin
             (currentProjectIndex.delete filterIndex)
             (currentProjectCursor.delete filterIndex)
             (loop for i from 0 until currentProjectIndex.recordCount do (setq currentProjectIndex.rowVector[i].Row i))
             )
          else
          (error (append "miner: unknown filter Lambda [" filterName "]"))
          ) ; end if
      (setq _isDirty true)
	  (SETTIME)
      true) ; end dropProjectFilter
   ;; Drops a table name from the project in the miner.
   (defun dropProjectTable(projectName oldTable)
      vars:(record rowIndex colIndex n)
      (STARTTIME)
      ;; Clear the current project (if any).
      (saveProject clear:)
      ;; Delete the old column from the project table.
      (dropTableColumn projectName oldTable)
	  (SETTIME)
      true) ; end dropProjectTable
   ;; Returns the Lambda for the specified Filter Lambda.
   (defun getFilterLambda(filterName)
      vars:(result record filterIndex)
      (STARTTIME)
      ;; Make sure there is a current project opened for edit.
      (_isCurrentProjectOpen)
      (currentProjectIndex.restore)
      (currentProjectCursor.restore)
      (setq filterIndex (search Name: filterName))
      (if (isNumber filterIndex)
          (setq record (currentProjectCursor.read filterIndex))
          else
          (error (append "miner: unknown filter Lambda [" filterName "]"))
          ) ; end if
      ;; Return the Lambda or compile the source (if necessary).
      (if (not (isLambda record.Lambda))
          (setq result (compileLambda (_loadSource record.Source)))
          (setq result record.Lambda)
          ) ; end if
	  (SETTIME)
      result) ; end getFilterLambda
   ;; Returns a Structure of the History record for the Filter Lambda.
   (defun getFilterHistory(filterName)
      vars:(record filterIndex result)
      (STARTTIME)
      ;; Make sure there is a current project opened for edit.
      (_isCurrentProjectOpen)
      (currentProjectIndex.restore)
      (currentProjectCursor.restore)
      (setq filterIndex (search Name: filterName))
      (if (isNumber filterIndex)
          (setq record (currentProjectCursor.read filterIndex))
          else
          (error (append "miner: unknown filter Lambda [" filterName "]"))
          ) ; end if
      ;; Return only the scores portion of the Results record.
      (setq result (objectToStructure record))
      (setq result (^delete (^delete (^delete (^delete (^delete result 0) 0) 0) 0) 0))
	  (SETTIME)
      result) ; end getFilterHistory
   ;; Summary:  Returns a vector of all Filter names.
   (defun getFilterNames(sortOrder sortName)
      vars:(record i result command commandLambda)
      (STARTTIME)
      ;; Make sure there is a current project opened for edit.
      (_isCurrentProjectOpen)
      (currentProjectIndex.restore)
      ;; Drop the Score Lambda record.
      (currentProjectIndex.truncate (lambda(x) (= x.Type 1))) 
      ;; Sort the Project table properly.
      ;; Note: Sort in sortOrder by sortName field.
      (setq command (append "checkoff; sort " sortOrder " " sortName ";"))
      (setq commandLambda (compileLambda command))
      (commandLambda currentProjectIndex)
      ;; Return a vector of all filter names.
      (setq result (new Vector: currentProjectIndex.recordCount))
      (loop for i from 0 until currentProjectIndex.recordCount do 
          (setq record (currentProjectIndex.read i))
          (setq result[i] record.Name)
          ) ; end loop
      (currentProjectIndex.restore)
	  (SETTIME)
      result) ; end getFilterNames
   ;; Returns a Structure of the Results record for the Filter Lambda.
   (defun getFilterScores(filterName)
      vars:(record filterIndex result)
      (STARTTIME)
      ;; Make sure there is a current project opened for edit.
      (_isCurrentProjectOpen)
      (currentProjectIndex.restore)
      (currentProjectCursor.restore)
      (setq filterIndex (search Name: filterName))
      (if (isNumber filterIndex)
          (setq record (currentProjectCursor.read filterIndex))
          else
          (error (append "miner: unknown filter Lambda [" filterName "]"))
          ) ; end if
      ;; Return only the scores portion of the Results record.
      (setq result (objectToStructure record))
      (setq result (^delete (^delete (^delete (^delete (^delete result 0) 0) 0) 0) 0))
      (setq result (map (lambda(y) (if (not (isNumber y)) y.Score y)) result))
	  (SETTIME)
      result) ; end getFilterScores
   ;; Returns the source for the specified Filter Lambda.
   (defun getFilterSource(filterName)
      vars:(result record filterIndex)
      (STARTTIME)
      ;; Make sure there is a current project opened for edit.
      (_isCurrentProjectOpen)
      (currentProjectIndex.restore)
      (currentProjectCursor.restore)
      (setq filterIndex (search Name: filterName))
      (if (isNumber filterIndex)
          (setq record (currentProjectCursor.read filterIndex))
          else
          (error (append "miner: unknown filter Lambda [" filterName "]"))
          ) ; end if
      (setq result (_loadSource record.Source))
	  (SETTIME)
      result) ; end getFilterSource
   ;; Returns a vector of all project names.
   (defun getProjectNames() 
   	vars:(result)
		(STARTTIME)
		(setq result (getTableNames "Blackboard"))
	   	(SETTIME)
		result)   		
   ;; Returns the name of the specified testing table in the project.
   (defun getProjectTable(tableIndex)
      vars:(result)
      (STARTTIME)
      ;; Make sure there is a current project open.
      (_isCurrentProjectOpen)
      ;; Return the specified testing table name.
      (setq result currentProjectCursor.colVector[(+ tableIndex _headerCols)])
	  (SETTIME)
      result) ; end getProjectTable
   ;; Returns the score name for the current project.
   (defun getScoreName()
      vars:(rowIndex record result)
      (STARTTIME)
      ;; Make sure there is a current project open.
      (_isCurrentProjectOpen)
      ;; Search for the Score Lambda record.
      (setq rowIndex (search Type: 0))
      ;; Return the Score Lambda name (if any).
      (if (isNumber rowIndex)
          (setq result (currentProjectCursor.read rowIndex)[Name:]) 
          (setq result "")
          ) ; end if
	  (SETTIME)
      result) ; end getScoreName
   ;; Returns the score source for the current project.
   (defun getScoreSource()
      vars:(rowIndex record result)
      (STARTTIME)
      ;; Make sure there is a current project open.
      (_isCurrentProjectOpen)
      ;; Search for the Score Lambda record.
      (setq rowIndex (search Type: 0))
      ;; Return the Score Lambda name (if any).
      (if (isNumber rowIndex)
          (setq result (_loadSource (currentProjectCursor.read rowIndex)[Source:])) 
          (setq result "")
          ) ; end if
	  (SETTIME)
      result) ; end getScoreSource
   ;; Adds a new project to the miner.
   (defun newProject(projectName tableObjVector scoreName scoreSource filterStructure)
      vars:(columnNames index record i record)
      (STARTTIME)
      ;; Make sure the table does not already exist.
      (if (= (isTable projectName) true) (error (append "miner: project already exists, [" projectName "]")))
      ;; Save and clear the current project (if necessary).
      (saveProject clear:)
      ;; Add the new project spreadsheet table to the data mine.
      ;; Note: Associate the new project table with the blackboard logical database.
      (setq columnNames (append 
                           (new Vector: object: 14 
                               Name: Source: Lambda: Type: Tested: Avg: Min: Max: Sum: Ssq: Std: Rar: Num: Cnt:)
                           tableObjVector)) 
      (createTable projectName columnNames)
      (setTableDatabaseName projectName "Blackboard")
      (openProject projectName)
      ;; Write the Score Lambda record to the project table.
      (setq index (copy currentProjectIndex.recordStructure))
      (setq record (copy currentProjectCursor.recordStructure))
      (setq index.Row 0)
      (setq index.Name scoreName)
      (setq record.Name scoreName)
      (setq record.Source (_saveSource scoreSource))
      (setq index.Type 0)
      (setq record.Type 0)
      (currentProjectIndex.write 0 index)
      (currentProjectCursor.write 0 record)
      ;; Write the Filter Lambda records to the project table.
      (loop for i from 0 until (length filterStructure) do
         (setq index (copy currentProjectIndex.recordStructure))
         (setq record (copy currentProjectCursor.recordStructure))
         (setq index.Row i)
         (setq index.Name filterStructure[i 0])
         (setq record.Name filterStructure[i 0])
         (setq record.Source (_saveSource filterStructure[i 1]))
         (setq index.Type 1)
         (setq record.Type 1)
         (currentProjectIndex.write (add1 i) index)
         (currentProjectCursor.write (add1 i) record)
         ) ; end loop
      (setq _isDirty true)
	  (SETTIME)
      true) ; end newProject
   ;; Opens the current project as specified.
   (defun openProject(projectName ...)
      vars:(testingSW record recordIndex recordCount)
      (STARTTIME)

      ;; Make sure we capture the testingSW argument (if any).
      (if (= (argCount) 2) 
          (setq testingSW (argFetch 1))
          (setq testingSW false)
          ) ; end if
      ;; If the requested project is already open, we are done.
      (if (= currentProjectName projectName) (return true))
      ;; Make sure we close and save the current project (if any).
      (if (<> currentProjectCursor #void) (saveProject clear:))
      (if (<> currentProjectIndex #void) (saveProject clear:))
      ;; Make sure the specified project exists.
      (if (<> (isTable projectName) true) (error (append "miner: attempt to open an unknown project, [" projectName "]")))
      ;; Create an in memory index for the project table.
      ;; Note: Use a memory cursor and fill the row numbers for the disk table.
      (setq currentProjectIndex (open projectName memory:
                           (new Vector: object: 13 Name: Type: Tested: Avg: Min: Max: Sum: Ssq: Std: Rar: Num: Cnt: Row:)
                           ))
      (setq recordCount currentProjectIndex.recordCount)
      (loop for recordIndex from 0 until recordCount do
          (setq record (currentProjectIndex.read recordIndex))
          (setq record.Row recordIndex)
          ) ; end row number loop  
      ;; Opening the current project table for exclusive use on disk.
      (setq currentProjectCursor (open projectName exclusive:))
      (setq currentTestingSW testingSW)
      (setq currentProjectName projectName)
      (setq _testingNameIndex #void)
      (setq _testingProjectName #void)
      (setq _isDirty false)
	  (SETTIME)
      true) ; end openProject
   ;; Register all available Blackboard Project Tables as browseLib cabinet extents.
   (defun registerProjectsAsExtents()
      vars:(projectNameList n N extMgr result)
      (STARTTIME)
	  (if (= (getDatabaseNames) false) (return true)) ;;TM Added to support initial load of environment
      (setq projectNameList (getTableNames "Blackboard"))
      (setq N (length projectNameList))
      (setq result (loop for n from 0 until N do
         (setq extMgr (copy _minorExtentMgrTemplate))
         (extMgr (append "~" projectNameList[n]) projectNameList[n] miner) 
         (browseLib.addExtent (append "~" projectNameList[n]) extMgr)
         )) ; end loop
	  (SETTIME)
      result) ; end registerProjectsAsExtents

   ;; Renames a filter Lambda in the project.
   (defun renameProjectFilter(oldName newName)
      vars:(record index filterIndex)
      (STARTTIME)
      ;; Make sure there is a current project opened for edit.
      (_isCurrentProjectOpen)
      ;; Update the current project with the new Score Lambda information.
      (currentProjectIndex.restore)
      (currentProjectCursor.restore)
      (setq filterIndex (search Name: oldName))
      (if (isNumber filterIndex)
          then
          (begin
             (setq index (currentProjectIndex.read filterIndex))
             (setq record (currentProjectCursor.read filterIndex))
             )
          else
          (error (append "miner: attempt to rename unknown filter [" oldName "]"))
          ) ; end if
      (setq index.Row filterIndex)
      (setq index.Name newName)
      (setq record.Name newName)
      (setq index.Type 1)
      (setq record.Type 1)
      (currentProjectIndex.write filterIndex index)
      (currentProjectCursor.write filterIndex record)
      (setq _isDirty true)
	  (SETTIME)
      true) ; end renameProjectFilter
   ;; Rename the score Lambda in the project in the miner.
   (defun renameProjectScore(scoreName)
      vars:(record index scoreIndex)
      (STARTTIME)
      ;; Make sure there is a current project opened for edit.
      (_isCurrentProjectOpen)
      ;; Update the current project with the new Score Lambda name.
      (currentProjectIndex.restore)
      (currentProjectCursor.restore)
      (setq scoreIndex (search Type: 0))
      (if (isNumber scoreIndex)
          then
          (begin
             (setq index (currentProjectIndex.read scoreIndex))
             (setq record (currentProjectCursor.read scoreIndex))
             )
          else
          (begin
             (setq index (copy currentProjectIndex.recordStructure))
             (setq record (copy currentProjectCursor.recordStructure))
             (setq scoreIndex 0))
          ) ; end if
      (setq index.Row scoreIndex)
      (setq index.Name scoreName)
      (setq record.Name scoreName)
      (setq index.Type 0)
      (setq record.Type 0)
      (currentProjectIndex.write scoreIndex index)
      (currentProjectCursor.write scoreIndex record)
      (setq _isDirty true)
	  (SETTIME)
      true) ; end renameProjectScore
   ;; Runs the named filter on the specified table cursor.
   (defun runProjectFilter(filterName tableCursor)
      vars:(filterIndex record filterLambda result)
      (STARTTIME)
      ;; Make sure there is a current project opened for edit.
      (if (= _testingNameIndex #void) (_isCurrentProjectOpen))
      ;; Find the Filter Lambda record in the current project table.
      (if (<> _testingNameIndex #void)
          (setq filterIndex _testingNameIndex[filterName])
          (setq filterIndex (currentProjectCursor.search Name: filterName))
          ) ; end if
      (if (isNumber filterIndex)
          then
          (if (<> currentProjectCursor #void)
              then
              (setq record (currentProjectCursor.read filterIndex))
              else
              (if (<> _testingProjectName #void)
                  (setq record (readRecord _testingProjectName filterIndex))
                  ) ; end inner inner if
              ) ; end inner if
          else
          (error (append "miner: unknown Filter Lambda [" filterName "]"))
          ) ; end if
      ;; Load the Filter Lambda (compiling if necessary).
      (setq filterLambda record.Lambda)
      (if (not (isLambda filterLambda))
          (setq filterLambda (compileLambda (_loadSource record.Source)))
          ) ; end if
      ;; Restore and filter the table cursor.
      (tableCursor.restore)
      (setq result (filterLambda tableCursor))
	  (SETTIME)
      result) ; end runProjectFilter
   ;; Adds or updates a filter Lambda script to the project.
   (defun saveFilterSource(filterName filterSource)
      vars:(record index filterIndex)
      (STARTTIME)
      ;; Make sure there is a current project opened for edit.
      (_isCurrentProjectOpen)
      ;; Validate the filter source, do not add filter if compilation returns an error.
      (javaScript filterSource)
      ;; Update the current project with the new Score Lambda information.
      (currentProjectCursor.restore)
      (currentProjectIndex.restore)
      (setq filterIndex (search Name: filterName))
      (if (isNumber filterIndex)
          then
          (begin
            (setq index (currentProjectIndex.read filterIndex))
            (setq record (currentProjectCursor.read filterIndex))
            )
          else
          (begin
             (setq index (copy currentProjectIndex.recordStructure))
             (setq record (copy currentProjectCursor.recordStructure))
             (setq filterIndex currentProjectCursor.recordCount))
          ) ; end if
      (setq index.Row filterIndex)
      (setq index.Name filterName)
      (setq record.Name filterName)
      (setq record.Source (_saveSource filterSource))
      (setq record.Lambda #void)
      (setq index.Type 1)
      (setq record.Type 1)
      (currentProjectIndex.write filterIndex index)
      (currentProjectCursor.write filterIndex record)
      (setq _isDirty true)
	  (SETTIME)
	true) ; end saveFilterSource
   ;; Saves the current project as specified.
   (defun saveProject(...)
      vars:(projectName testingSW clearSW)
      (STARTTIME)
      ;; Make sure we close and save the current project (if any).
      (if (<> currentProjectCursor #void)
          (begin
             (currentProjectCursor.restore)
             (if (= _isDirty true) (currentProjectCursor.save))
             (setq currentProjectCursor (close currentProjectCursor))
             )) ; end if
      (if (<> currentProjectIndex #void)
          (setq currentProjectIndex (close currentProjectIndex))
          ) ; end if
      (setq projectName currentProjectName)
      (setq testingSW currentTestingSW)
      (setq currentProjectName #void)
      (setq currentTestingSW #void)
      (setq _testingNameIndex #void)
      (setq _testingProjectName #void)
      (setq _isDirty false)
      ;; Make sure to reopen the project if requested.
      (if (= (argCount) 1) (setq clearSW (argFetch 0)))
      (if (and (<> clearSW clear:) (<> projectName #void)) (openProject projectName testingSW))
	  (SETTIME)
      true) ; end saveProject
   ;; Search the current project for the specified record.
   (defun search(fieldName value)
      vars:(recordIndex record)
      (STARTTIME)
      (_isCurrentProjectOpen)
      ;; Perform the search using any indices as a guide to search strategy.
      (cond
         ;; Are we searching for Name where there is a Name index?
         ((and (= fieldName Name:) (<> _testingNameIndex #void))
          (begin
             (setq recordIndex _testingNameIndex[value])
             )) ; end Name with Index search
         ;; Are we searching where there is a project index?
         ((<> currentProjectIndex #void)
          (begin
             (setq recordIndex (currentProjectIndex.search fieldName value))
             (if (isNumber recordIndex) (setq recordIndex currentProjectIndex.rowVector[recordIndex].Row))
             )) ; end project index search
         ;; Are we searching where there is no project index?
         (default
          (begin
             (setq recordIndex (currentProjectCursor.search fieldName value))
             )) ; end default search
         ) ; end cond
      ;; Return the record Index for this search.
	  (SETTIME)
      recordIndex) ; end search
   ;; Starts testing all filter Lambdas against all tables in the current project.
   (defun testRun(...)
      vars:(projectName filterIndex record
            tableName tableCursor tableIndex
            scoreLambda filterLambda 
            score count cursor
            tableNames filterCount colCount
            ) ; end temporary variables
      (STARTTIME)
      ;; Capture the project name, including optional argument (if any).
      (setq projectName currentProjectName)
      (if (= (argCount) 1) (setq projectName (argFetch 0)))
      (writeln _eol "Starting backtesting on project: " projectName)
      ;; Close every file in the data mine including any open projects to make room for testing.
      (writeln "Saving previous open miner project")
      (saveProject clear:)
      (clearCursors)
      ;; Sort the project table in ascending order by Name, Type, and Tested fields.
      ;; Note: When we begin, all scores will be #void and Tested will be #void.
      (writeln "Preparing testing indices on project spreadsheet table")
      (filterTable projectName "checkoff; sort backup append(Type,'~',Tested)")
      ;; Open the specified project for testing.
      (openProject projectName true)
      (setq tableNames currentProjectCursor.colVector)
      (setq filterCount currentProjectCursor.recordCount)
      (setq colCount currentProjectCursor.colCount)
      ;; Create a temporary index on the Name field so the run command will execute faster.
      (_createNameIndex)
      (abortProject)
      (setq _testingProjectName projectName)
      ;; Load the Score Lambda for this testing run.
      (setq record (readRecord projectName 0))
      (setq scoreLambda record.Lambda)
      (if (not (isLambda scoreLambda))
          (begin
             (setq scoreLambda (__compileLambda (_loadSource record.Source)))
             (setq record.Lambda scoreLambda)
             (writeRecord projectName 0 record)
             )) ; end if
      (if (not (isLambda scoreLambda)) 
          (begin
             (writeln "miner: got this error compiling scoreLambda " scoreLambda)
             (goto EndTestRun:)
          )) ; end if
      ;; Calculate the score for each filter Lambda and and each testing table.
      ;; Note: We place these scores in the project table.
      (loop for tableIndex from _headerCols until colCount do
          ;; Get the latest table which needs testing from the Tested field
          ;; of the first Filter Lambda record in the project table.
          (setq record (readRecord projectName 1))
          ;; Set the miner persistant variables for this test run.
          (setq testRunProjectName projectName)
          ;; Are we done testing records?
          (setq tableIndex (+ record.Tested _headerCols))
          (if (>= tableIndex colCount) (goto EndTestRun:))
          ;; Open the table to be tested against each Filter Lambda.
          (setq tableName tableNames[tableIndex])
          (setq _testingTableName tableName)
          (setq tableCursor (open tableName memory:))
          (writeln "Starting testing on table: " tableName)
          (loop for filterIndex from 1 until filterCount do
              ;; Load the Filter Lambda for this testing run.
              (setq record (readRecord projectName filterIndex))
              (setq testRunFilterName record.Name)
              (setq testRunStartSW (and (= tableIndex _headerCols) (= filterIndex 1)))
              (setq testRunEndSW (and (= tableIndex (sub1 colCount)) (= filterIndex (sub1 filterCount))))
              (if (<> (+ record.Tested _headerCols) tableIndex) (goto CloseTable:))
              (setq filterLambda record.Lambda)
              (if (not (isLambda filterLambda)) (setq filterLambda (__compileLambda (_loadSource record.Source))))
              (if (not (isLambda filterLambda)) 
                  (begin
                     (writeln "miner: got an error compiling filterLambda [" record.Name "] {" filterLambda "}")
                     (goto WriteFilterLambda:)
                  )) ; end if
              ;; Restore the current testing table and run the Filter Lambda
              (tableCursor.restore)
              (setq score ((lambda(f c) (onError (lambda(errString) errString)) (f c)) filterLambda tableCursor))
              ;; Make sure there was no error running the filter Lambda.
              (if (not (isLambda score)) 
                  (begin
                     (writeln "miner: got an error running filterLambda [" record.Name "] {" score "}")
                     (goto WriteFilterLambda:)
                  )) ; end if
              ;; Compute the newly filtered table score and save the project record.
              (setq score (scoreLambda tableCursor))
              (setq record[tableIndex] score)
              ;; Perform statistical computations for this score
              (if (not (isNumber score)) (setq score score.Score))
              (if (<> score #void)
                  (begin
                     (setq record.Cnt (+ record.Cnt 1))
                     (setq count record.Cnt)
                     (setq record.Num (+ record.Num tableCursor.recordCount))
                     (setq record.Sum (+ record.Sum score))
                     (setq record.Ssq (+ record.Ssq (* score score)))
                     (if (isPositive count) 
                         (begin
                            (setq record.Avg (/ record.Sum count))
                            (setq record.Std (sqrt (- (/ record.Ssq count) (* record.Avg record.Avg))))
                            )) ; end if
                     (if (isPositive record.Std) (setq record.Rar (/ record.Avg record.Std)))
                     (if (= record.Min #void) (setq record.Min score))                      
                     (if (= record.Max #void) (setq record.Max score))                      
                     (setq record.Min (min record.Min score))
                     (setq record.Max (max record.Max score))
                     )) ; end if
              ;; Save the project record including the improved Filter Lambda and all scores.
              ;; Note: Increment the tested counter so we know we have tested this table.
              WriteFilterLambda::
              (setq record.Tested (+ record.Tested 1))
              (setq record.Lambda filterLambda)
              (writeRecord projectName filterIndex record)
              ) ; end Filter Lambda loop
          CloseTable::
          ;; Close the table just tested.
          (setq tableName #void)
          (if (<> tableCursor #void) (setq tableCursor (close tableCursor)))
          ) ; end testing tables loop
      EndTestRun::
	  ;; Erase all Lambdas from the specified project in the Blackboard.
	  ;; Note: This is done only AFTER all back testing to save space
	  ;;       and time during the miner project editing and viewing.
	  ;;       If persistent Lambdas are required, across backtesting runs,
	  ;;       please use the investorColony which has been constructed
	  ;;       for that purpose.
      (loop for filterIndex from 0 until filterCount do
          ;; Erase the Filter Lambda for this record.
          (setq record (readRecord projectName filterIndex))
          (setq record.Lambda #void)
          (writeRecord projectName filterIndex record)
          ) ; end Filter Lambda loop
	  ;; Close the specified Blackboard project.
      (setq _isDirty true)
      (saveProject clear:)
      (writeln "Completed testing on project: " projectName)
	  (SETTIME)
      true) ; end testRun
   ;; ****************************************************************
   ;; Begin miner main code section
   ;; ****************************************************************
   vars:(i)
   true)  ;; end of miner


























;;**EXPORTKEY**:dataMineLib.miner.%NOTES
;#text#
;; *******************************************************************
;;  Summary: The dataMineLib miner Lambda stores all of its results
;;           in the data mine blackboard database. This design document
;;           describes the data model for the dataMineLib blackboard.
;; *******************************************************************

The dataMineLib blackboard is subdivided into projects. Each project
contains one score Lambda, a dynamic set of filter Lambdas, and a list 
of tables. The objective of a project is to find filter Lambdas which 
score well when run against the list of tables.

The list of tables must be a proper subset of the tables available in
the data mine. For each project, the score Lambda, filter Lambdas, and 
list of tables are all user choosen.

Each dataMineLib project is stored in the blackboard database as a
single table with the following format:

    ProjectName (associated with the blackboard database)
       Name Source Lambda Type Tested Avg Min Max Sum Ssq Std Rar Num Cnt TableName1 ... TableNameN

Notes:
   o Pro: Score Lambda (type 0) and Filter Lambdas (type 1) stored here.
   o Pro: Whole project is stored in one easy piece.
   o Con: Lambda source may not contain imbedded tabs or eols for export.
   o Ans: For editing/import/export scan all Lambda source replacing #\tab and eol with escape sequences,
             as follows: (setq vc (substitute (substitute v #\tab "#$tab$#") _eol "#$eol$#"))
   o Con: Need a whole new interface for project/Lambda editing.

Constraints:

   o We allow no more than 1000 rows per project.

Operations:

   clearProject
      o Deletes the project table from the dataMineLib database.

   newProject
      o Adds a new project table to the dataMineLib database.
      o Project names must be unique.

   addProjectTable
      o Adds a new table name to the end of the colVector in the project table.
      o Project table names must be unique.
      o Project table names matched against tables in the dataMineLib._dataTables.

   dropProjectTable
      o Deletes a table name from the colVector in the project table, and fixes each table row.

   addProjectScore
      o Updates the score Lambda name, and source in the project table. Clears all results in the project table.
      o There can only be one score Lambda per project.

   dropProjectScore (omit)
      o Deletes a score name in Scores and updates the Results table for the project.

   renameProjectScore
      o Updates the score Lambda name.
      o There can only be one score Lambda per project.

   clearProjectScores
      o Clears all scores in the project table.

   addProjectFilter
      o Adds a new filter Lambda to the project table.
      o Project filter names must be unique.
      o Project filter specifications must be unique.

   dropProjectFilter
      o Deletes a filter Lambda from the project table.

   renameProjectFilter
      o Renames a filter Lambda in the project.
      o Project filter names must be unique.
      o Project filter specifications must be unique.

   clearProjectAutoFilters
      o Clears all auto filters in the project table.

   backTest
      o Starts backtesting all filters and updates the project table.






























































;;**EXPORTKEY**:dataMineLib.miner._minorExtentMgrTemplate
(deforphan dataMineLib.miner _minorExtentMgrTemplate(extentName projectTableName parentLambda)  
;; *******************************************************************
;; summary:  Factory for creating browseLib extent manager Lambdas.
;;           Manages all Lambda source/binary code stored in each miner
;;           project table. Includes checking Lambda source/binary 
;;           in and out of the miner project, importing, exporting, 
;;           and deleting source from the miner project.
;;
;; Args:     extentName:          Name of the new file cabinet repository extent.
;;           projectTableName:    The name of the datamine miner project table.
;;           parentLambda:         The datamine miner Lambda for this extent manager.
;; Return:   true
;;
;; Note:
;;           Manages the Filter Lambdas, in a datamine miner project table
;;           using the foreign extent manager API understood by browseLib.
;; *******************************************************************
    pvars:(;; Public Child Lambdas
           abortFocus                 ;; Remove the specified extent from focus 
           abortTransaction           ;; Abort a transaction on the extent repository
           beginTransaction           ;; Begin a transaction on the extent repository
           commitTransaction          ;; Commit a transaction on the extent repository
           compileByIndex             ;; Compile the specified Lambda source using an index
           compileByKey               ;; Compile the specified Lambda source using a key
           deleteLambdaByIndex         ;; Delete an Lambda from the extent using an index
           deleteLambdaByKey           ;; Delete an Lambda from the extent using a key
           deleteBinaryByIndex        ;; Delete an Lambda binary value using an index
           deleteBinaryByKey          ;; Delete an Lambda binary value using a key
           deleteSourceByIndex        ;; Delete an Lambda source value using an index
           deleteSourceByKey          ;; Delete an Lambda source value using a key
           getNextLevel               ;; Return an XPath directory string list for the next repository directory level.
           getTypes                   ;; Return an XPath directory type list string for all possible types in a hierarchical descendents list
           inspectRepository          ;; Perform a complete inspection of my extent repository.
           memberIndex                ;; Return the index of a key in an extent
           refLambdaByIndex            ;; Retrieve an Lambda from an extent using an index
           refLambdaByKey              ;; Retrieve an Lambda from an extent using a key
           refBinaryByIndex           ;; Retrieve an Lambda binary value using an index
           refBinaryByKey             ;; Retrieve an Lambda binary value using a key
           refKeyByIndex              ;; Retrieve an extent key using an index
           refLength                  ;; Retrieve the number of items in an extent
           refSourceByIndex           ;; Retrieve an Lambda source value using an index
           refSourceByKey             ;; Retrieve an extent source value using a key
           refSourceExByIndex         ;; Retrieve an Lambda exportsource value using an index
           refSourceExByKey           ;; Retrieve an extent export source value using a key
           setLambdaByKey              ;; Store an extent value using a key
           setBinaryByKey             ;; Store an extent Lambda binary value using a key
           setFocus                   ;; Set the specified extent in focus 
           setSourceByKey             ;; Store an extent Lambda source value using a key
           ;; Private Child Lambdas
           _initialize                ;; Initialize the datamine miner project for editing.
           _noopErr                   ;; A no operation error function returning #void.
           _syncUp                    ;; Synchronize the datamine miner project after editing.
		   ;; Private Variables
           myLambdaName                ;; The name for the current Lambda being managed
           myExtentName               ;; The name for this extent
           myFilterNameList           ;; The filter name list for this extent
           myParent                   ;; The parent Lambda for this extent manager 
           myProjectName              ;; The object repository for this extent 
           ) ; end persistant variables
    ;;****************************************************************
    ;; Define Private Child Lambdas
    ;;****************************************************************
    ;; Abort a transaction on the extent repository 
    (defun _initialize()
        vars:(result)
        (STARTTIME)
        (myParent.openProject myProjectName)
        (setq result (if (= myFilterNameList #void) (setq myFilterNameList (myParent.getFilterNames up: Name:))))
        (SETTIME) 
        ) ; end _initialize
    ;; A no operation error function returning #void. 
    (defun _noopErr(msg) #void)
    ;; Synchronize the datamine miner project after editing. 
    (defun _syncUp()
    	vars:(time result)
    	(STARTTIME)
        (myParent.openProject myProjectName)
        (setq result (setq myFilterNameList (myParent.getFilterNames up: Name:)))
        (SETTIME) 
        result) ; end _initialize
    ;;****************************************************************
    ;; Define Public Child Lambdas
    ;;****************************************************************
    ;; Remove the datamine miner project from focus 
    (defun abortFocus() 
    	vars:(result)
    	(STARTTIME)
    	(setq result (myParent.saveProject clear))
        (SETTIME) 
    	result)
    ;; Initialize the datamine miner project for editing. 
    (defun abortTransaction() 
    	(STARTTIME)
        (SETTIME) 
    	(return true) 
    	)
    ;; Begin a transaction on the extent repository 
    (defun beginTransaction() 
    	vars:(result) 
    	(STARTTIME)
    	(setq result (_initialize))
        (SETTIME) 
    	result)
    ;; Commit a transaction on the extent repository 
    (defun commitTransaction() 
    	(STARTTIME)
        (SETTIME) 
    	(return true) 
    	)
    ;; Compile the specified Lambda source using an index 
    (defun compileByIndex(index alwaysCompile) 
    	vars:(result)
    	(STARTTIME)
    	(_initialize) 
    	(setq result (compileByKey myFilterNameList[index] alwaysCompile))
        (SETTIME) 
    	result)
    ;; Compile the specified Lambda source using a key 
    (defun compileByKey(key alwaysCompile)
       vars:(theSource theLambda anItem key)
    	(STARTTIME)
       ;; Capture and return any compile errors as strings
       (defun _compileItErr(err)
          vars:(msg) 
          (setq msg (append "miner: Compiling " myExtentName "[" myLambdaName "] returned this error: " err))
          (writeln msg) 
          msg)
       ;; Prepare to compile the named Filter Lambda and throw away the result.
       ;; Note: the Lambda will only be compiled if the always compile switch is true.
       (_initialize) 
       (onError _compileItErr)
       (setq myLambdaName key)
       (if alwaysCompile (setq theLambda (^compile (morph (dataMineLib (myParent.getFilterSource key))))))
       (SETTIME) 
       true) ; end compileByKey
    ;; Delete an Lambda from the extent using an index 
    (defun deleteLambdaByIndex(index) 
    	vars:(result)
    	(STARTTIME)
    	(_initialize) 
    	(setq result (deleteLambdaByKey myFilterNameList[index]))
        (SETTIME) 
    	result)
    ;; Delete an Lambda from the extent using a key 
    (defun deleteLambdaByKey(key) 
    	vars:(result)
    	(STARTTIME)
    	(_initialize) 
    	(myParent.dropProjectFilter key) 
    	(setq result (_syncUp))
        (SETTIME) 
    	result)
    ;; Delete an Lambda binary value using an index 
    (defun deleteBinaryByIndex(index) 
    	vars:(result)
    	(STARTTIME)
    	(_initialize) 
    	(setq result (deleteBinaryByKey myFilterNameList[index]))
        (SETTIME) 
    	result)
    ;; Delete an Lambda binary value using a key 
    (defun deleteBinaryByKey(key) 
    	(STARTTIME)
		(_initialize) 
        (SETTIME) 
    	true)
    ;; Delete an Lambda source value using an index 
    (defun deleteSourceByIndex(index)
    	vars:(result) 
    	(STARTTIME)
    	(_initialize) 
    	(setq result (deleteSourceByKey myFilterNameList[index]))
        (SETTIME) 
    	result)
    ;; Delete an Lambda source value using a key 
    (defun deleteSourceByKey(key) 
    	vars:(result)
    	(STARTTIME)
    	(setq result (deleteLambdaByKey key))
        (SETTIME) 
    	result)
    ;; Return an XPath directory string list for the next repository directory level.
    (defun getNextLevel(projectName startLine lineCount ...)
        vars:(i I s cursor record)
    	   (STARTTIME)
        ;; Note1: Each XPath directory line has the following tab delimited fields:
        ;;         type    		{String}
        ;;         value   		{filterName for each node in the repository.}
        ;;         size			none
        ;;         date			none
        ;;         time			none
        ;;         version			{current AIS version}
        ;;         symbolicKey		{filterName for each node in the repository.}
        ;;         uniqueKey		{filterName for each node in the repository.}
        ;; Note2: We support only one hierarchy level of repository directory.
        ;;        Investor names may contain embedded colon (:) characters which
        ;;        signify TestSuite:investor relationships.
        (if (= (length myFilterNameList) 0)
            (begin
               ;; Return a vector of all filter names in this miner project.
               (setq cursor (myParent.open myProjectName static:))
               (setq I cursor.recordCount)
               (setq myFilterNameList (new Vector: I))
               (loop for i from 0 until I do 
                 (setq record (cursor.read i))
                 (setq myFilterNameList[i] record.Name)
                 ) ; end loop
               (setq cursor (myParent.close cursor))
               (sort myFilterNameList <)
            )) ; end if
        (setq I (length myFilterNameList))
        (setq I (min I (+ startLine lineCount)))
        (setq s (append "" startLine #\tab lineCount #\tab I )) 
        (setq s (append s #\newline "Current" #\tab projectName #\tab I #\tab "" #\tab "" #\tab (isVersion) #\tab projectName #\tab projectName)) 
        (loop for i from startLine until I do
            (setq s (append s #\newline "Filter" #\tab myFilterNameList[i] #\tab "" #\tab "" #\tab "" #\tab (isVersion) #\tab myFilterNameList[i] #\tab myFilterNameList[i]))
            ) ;; end of loop
        (SETTIME) 
        s)
    ;; Return an XPath directory type list string for all possible types in a hierarchical descendents list 
    (defun getTypes()
        vars:(result)
        (setq result (append ".default." #\tab "edit" #\newline
                             "Filter" #\tab "edit,erase,export,import,checkin" #\newline
                      ))
        result)
	;; Perform a complete inspection of my extent repository.
	(defun inspectRepository() 
    	(STARTTIME)
        (SETTIME) 
		true) ; end inspectRepository
    ;; Return the index of a key in an extent 
    (defun memberIndex(key) 
    	vars:(result)
    	(STARTTIME)
    	(_initialize) 
    	(setq result (member key myFilterNameList))
        (SETTIME) 
    	result)
    ;; Retrieve an extent value using an index 
    (defun refLambdaByIndex(index) 
    	vars:(result)
    	(STARTTIME)
    	(_initialize)
    	(setq result (refLambdaByKey myFilterNameList[index]))
        (SETTIME) 
    	result)
    ;; Retrieve an extent value using a key 
    (defun refLambdaByKey(key)
    	vars:(result)
    	(STARTTIME)
    	(_initialize) 
    	(onError _noopErr) 
    	(setq result (myParent.getFilterSource key))
        (SETTIME) 
    	result)
    ;; Retrieve an Lambda binary value using an index 
    (defun refBinaryByIndex(index) 
    	(STARTTIME)
    	(_initialize) 
        (SETTIME) 
    	#void) 
    ;; Retrieve an Lambda binary value using a key 
    (defun refBinaryByKey(key) 
    	(STARTTIME)
    	(_initialize) 
        (SETTIME) 
    	#void)
    ;; Retrieve an extent key using an index 
    (defun refKeyByIndex(index) 
    	vars:(result)
    	(STARTTIME)
    	(_initialize) 
    	(setq result myFilterNameList[index])
        (SETTIME) 
    	result)
    ;; Retrieve the number of items in an extent 
    (defun refLength() 
    	vars:(result)
    	(STARTTIME)
    	(_initialize) 
    	(setq result (length myFilterNameList))
        (SETTIME) 
    	result)
    ;; Retrieve an extent scource value using an index 
    (defun refSourceByIndex(index) 
    	vars:(result)
    	(STARTTIME)
    	(_initialize) 
    	(setq result (refSourceByKey myFilterNameList[index]))
        (SETTIME) 
    	result) 
    ;; Retrieve an Lambda source value using a key 
    (defun refSourceByKey(key) 
    	vars:(result)
    	(STARTTIME)
    	(_initialize) 
    	(onError _noopErr) 
    	(setq result (myParent.getFilterSource key))
        (SETTIME) 
    	result)
    ;; Retrieve an extent scource (for export) using an index 
    (defun refSourceExByIndex(index) (refSourceByIndex index)) 
    ;; Retrieve an Lambda source (for export) using a key 
    (defun refSourceExByKey(key) (refSourceByKey key))
    ;; Store an extent value using a key 
    (defun setLambdaByKey(key newLambda) 
    	vars:(result)
    	(STARTTIME)
    	(_initialize) 
    	(setq result (myParent.saveFilterSource key newLambda))
        (SETTIME) 
    	result)
    ;; Store an extent Lambda binary value using a key 
    (defun setBinaryByKey(key newBinary) 
    	(STARTTIME)
        (SETTIME) 
    	(_initialize) 
    	true)
    ;; Set the datamine miner project in focus 
    (defun setFocus() 
    	vars:(result)
    	(STARTTIME)
    	(setq result (_initialize))
        (SETTIME) 
    	result)
    ;; Store an extent Lambda source value using a key 
    (defun setSourceByKey(key newSource)
    	vars:(result) 
    	(STARTTIME)
    	(_initialize) 
    	(setq result (myParent.saveFilterSource key newLambda))
        (SETTIME) 
    	result)
    ;;****************************************************************
    ;; MAIN initialization section
    ;;****************************************************************
    (setq myExtentName extentName)
    (setq myParent parentLambda)
    (setq myProjectName projectTableName)
    (setq myDM dataMineLib) ; required by profiling code
    myProjectName) ; end _minorExtentMgrTemplate





;;**EXPORTKEY**:dataMineLib.renameTable
(defchild dataMineLib:renameTable(tableName newName)
    ;; *******************************************************************
    ;; summary:  Rename the specified relational table in the datamine.
    ;; Args:     tableName      Old name of the table to rename
    ;;           newName        New name of the table to rename
    ;; Return:   true  
    ;; Notes: tm	This routine was modified extensively. It used to 
    ;; call clearCursors (undocumented behavior in references) and it
    ;; did not reset schema.myObjectName after rename. Note as well that 
    ;; it now calls __collectTableNames to ensure that we pick up
    ;; the deleted table name from other extents if it exists. 
    ;; eg: extent precedence is respected in this version.
    ;; *******************************************************************
	vars:(schema  extentIndex )
	(STARTTIME)	
	;; Make sure the specified table does exist
	(setq extentIndex (getTableExtent newName))
	(if (<> extentIndex false) (error "dataMineLib:renameTable: -- The specified new table name already exists."))
	
	(setq extentIndex (getTableExtent tableName))
	(if (= extentIndex false) (error "dataMineLib:renameTable: -- The specified old table name does not exist."))

	(if (<> metaTable #void)
		(error (append "dataMineLib:renameTable: " myObjectName " Can't rename a member table")))

	(if (= myObjectType metaTable)
		(error (append "dataMineLib:renameTable: " myObjectName " Can't rename a meta table")))
	
	;; Make sure the table has no open cursors
	(if (<> openTables[tableName] #void) 
		(error "dataMineLib:renameTable: -- The specified table has open cursors"))
	
	;; Load and erase the old table schema record from the database.
	(__beginTrans extentIndex)
	(setq schema dmExt[extentIndex].repos[tableName])
	(setq dmExt[extentIndex].repos[tableName] #void)
	
	;; Change the table name and save the new table schema into the extent repository.
	(setq schema.myObjectName newName)
	(setq dmExt[extentIndex].repos[(symbol newName)] schema)
	(__commitTrans extentIndex)
	
	; Note: It is necessary to do a full rebuild using __collectTableNames because
	; the removed table name might be in anothe extent! 
	(__collectTableNames reload:) ; recreate the dataMineTables dictionary and databaseNames vector
	(SETTIME)
	true) ;; end of renameTable























































































;;**EXPORTKEY**:dataMineLib.restoreTable
(defchild dataMineLib:restoreTable(tableName)
   ;; *******************************************************************
   ;; summary:  Restore a table to its back up view.
   ;;
   ;; Args:     tableName    table to be restored.
   ;;
   ;; Return:   true
   ;; *******************************************************************
	vars:(cursor record index)
	(STARTTIME)
	(setq cursor (open tableName))
	(cursor.restore)
	(close cursor)
	(SETTIME)
	true) ;; end of restoreTable





























































































;;**EXPORTKEY**:dataMineLib.selfTest
(deforphan dataMineLib:selfTest(recCount printSW)
	pvars:(
		addColumns
		combineMetaViews
		combineViews
		createBufferedRelation2
		createIndex
		createMemory
		createRelation
		deleteRelation
		deleteMemory
		exportRelation
		filterRelation
		importRelation
		makeProject
		makeRelation
		parseFilter
		persistentViews
		prepareExtent
		rollbackRelation
		sortRelation
		speedTest
		testIt
		testMetaTable
		testMetaTableByIndex
		testProject
 		)
	(prepareExtent)	(dataMineLib.systemCheck)

	(rollbackRelation recCount printSW)	(dataMineLib.systemCheck)
	(createRelation recCount printSW) (dataMineLib.systemCheck)
	(createMemory recCount printSW)	(dataMineLib.systemCheck)
	(sortRelation recCount printSW) (dataMineLib.systemCheck)
	(deleteRelation recCount printSW) (dataMineLib.systemCheck)
	(deleteMemory recCount printSW) (dataMineLib.systemCheck)
	(exportRelation recCount printSW) (dataMineLib.systemCheck)
	(filterRelation recCount printSW disk:)	(dataMineLib.systemCheck)
	(filterRelation recCount printSW static:) (dataMineLib.systemCheck)
	(combineViews recCount printSW) (dataMineLib.systemCheck)
	;;(persistentViews recCount printSW)
	(testProject recCount printSW) (dataMineLib.systemCheck)
	(importRelation recCount printSW) (dataMineLib.systemCheck)
	(testMetaTable 3 false) (dataMineLib.systemCheck)
	(createBufferedRelation2 10000 100 false) (dataMineLib.systemCheck)
	(createIndex recCount printSW) (dataMineLib.systemCheck)
	(speedTest recCount printSW) (dataMineLib.systemCheck)
	(addColumns 10 printSW) (dataMineLib.systemCheck)
	

true)




;;**EXPORTKEY**:dataMineLib.selfTest.addColumns
(defchild  dataMineLib.selfTest addColumns(recCount printSW)
	;; *******************************************************************
	;; summary:  Populate a small table with records, read them back.
	;;			Add columns and then repeat process.
	;; Args:	  recCount		 The number of records to create.
	;;			  printSW		  Switch to turn on/off debug printing.
	;; Return:	true
	;; *******************************************************************
	vars:(dbRec i j cursor startTime endTime)

	(dataMineLib.usingForCreate dataMineLibSelfTest: "testSuite")
	
	;; Write each record to the table.
	(writeln _eol "***************************") 
	(writeln ">>addColumns selftest")
	(writeln ">>createRelation: Populate a relation with records.") 
	(writeln ">>Droping table myTable")
	(dataMineLib.dropTable myTable:) 
	(writeln ">>Creating table myTable") 
	(dataMineLib.createTable myTable: Name: Title: Salary:)
	(setq startTime (getTickCount 0))
	(writeln ">>Opening cursor on table myTable")
	(setq cursor (dataMineLib.open myTable:))
	(if printSW (writeln "My cursor type is " cursor.myCursorType)
)
	(loop for i from 0 until recCount do
		(setq dbRec (new Vector: 3 (append "John" i) "Sales Rep" (money (+ $50000 i))))
		(cursor.write i dbRec)
		(if printSW (writeln "Writing record [" i "]: " dbRec))
		) ;; end loop
	(setq cursor (dataMineLib.close cursor))
	(setq endTime (getTickCount startTime))
	;; Read and check each record for validity.
	(setq startTime (getTickCount 0))
	(writeln "*****************************")
	(writeln "Opening cursor on table myTable for sequential compare validation")
	(setq cursor (dataMineLib.open myTable:))
	(loop for i from 0 until recCount do  
		(setq dbRec (new Vector: 3 (append "John" i) "Sales Rep" (money (+ $50000 i))))
		(setq x (cursor.read i))
		(testIt  "failed record compare " dbRec x)
		(if printSW (writeln "Reading record [" i "]: " x))
		) ;; end loop
	(setq cursor (dataMineLib.close cursor))
	(setq endTime (getTickCount startTime))
	(writeln "Validated records")
;; Add columns to table
	(writeln "*****************************")
 	(writeln "Test adding columns")
	(setq startTime (getTickCount 0))
	(writeln ">>calling dataMineLib.addTableColumn")
	(dataMineLib.addTableColumn myTable: "newColOne" "newColTwo")
	(writeln ">>opening table myTable")
	(setq cursor (dataMineLib.open myTable:))
	(if printSW (writeln "cursor.colCount=" cursor.colCount))
	(if printSW (writeln "(cursor.getColumnHeaders 0 4) ->" (cursor.getColumnHeaders 0 4)))
	(loop for i from 0 until recCount do
		(setq dbRec (cursor.read i))
		(setq dbRec[newColOne:] (append "test1 " i))
		(setq dbRec[newColTwo:] (append "test2 " i))
		(cursor.write i dbRec) 
		(if printSW (writeln "Writing record [" i "]: " dbRec))
		) ;; end loop
	(writeln ">>finished updating records")
;; Read the records back to see if data is there
	(writeln ">>reading records back in for compare")
	(loop for i from 0 until recCount do  
		(setq dbRec (new Vector: 5 (append "John" i) "Sales Rep" (money (+ $50000 i)) (append "test1 " i) (append "test2 " i) ))
		(setq x (cursor.read i))
		(if printSW (writeln "Reading record [" i "]: " x))
		(testIt  "failed record compare " dbRec x)
		) ;; end loop
	(setq cursor (dataMineLib.close cursor))
	(setq endTime (getTickCount startTime))
	(dataMineLib.systemCheck dataMineLib.usingExtIndex)
	(writeln ">>closing table myTable")

	;; Read and check each record for validity.
	(setq startTime (getTickCount 0))
	(writeln "*****************************")
	(writeln "Opening table myTable for sequential compare validation")
	(setq cursor (dataMineLib.open myTable:))
	(if printSW (writeln "cursor.colCount=" cursor.colCount))
	(if printSW (writeln "(cursor.getColumnHeaders 0 4) ->" (cursor.getColumnHeaders 0 4)))
	(loop for i from 0 until recCount do  
		(setq dbRec (new Vector: 5 (append "John" i) "Sales Rep" (money (+ $50000 i)) (append "test1 " i) (append "test2 " i) ))
		(setq x (cursor.read i))
		(if printSW (writeln "Reading record [" i "]: " x))
		(testIt  "failed record compare " dbRec x)
		) ;; end loop
	(setq cursor (dataMineLib.close cursor))
	(setq endTime (getTickCount startTime))
	(writeln "Validated records.")
	
	true) ;; end createRelation




;;**EXPORTKEY**:dataMineLib.selfTest.combineMetaViews
(defchild dataMineLib.selfTest combineMetaViews(theTable printSW)
   ;; *******************************************************************
   ;; summary:  Run a filter on a relational table, and then combine
   ;;           the resulting views logically.
   ;; Args:     recCount       The number of records to create.
   ;;           printSW        Switch to turn on/off debug printing.
   ;; Return:   true
   ;; *******************************************************************
   vars:(dbRec x i j cursor startTime endTime viewSize fx fSource recCount)
   (dataMineLib.usingForCreate dataMineLibSelfTest: "testSuite")
   
   (writeln _eol "***************************") 
   (writeln "combineViews: Combine filtered views on a relation.") 
   ;(makeRelation recCount printSW)
   (setq fSource (append "all Cash <= 1000"))
   (writeln "Filter source = " fSource)
   (setq fx (dataMineLib.compileLambda fSource))
   (setq cursor (dataMineLib.open theTable meta:))
   (setq startTime (getTickCount 0))
   (fx cursor)
   (setq endTime (getTickCount startTime))
   (cursor.saveView Low:)
   (writeln "Filtered low record count = " cursor.recordCount)
   (if printSW
      (loop for i from 0 until cursor.recordCount do  
         (setq x (cursor.read i))
         (writeln "Reading record [" i "]: " x)
         ) ;; end loop
      ) ; end if
   (setq recCount cursor.recordCount)
;   (setq cursor (dataMineLib.close cursor))
   (writeln "Running filter on " 
        (string theTable) " = " (text (/ (* (/ endTime 3600) 10000000) recCount) "#,###.#0") " hours.")
   ;; Run a second filter on the table.
   (setq fSource (append "all Cash > 1000"))
   (writeln "Filter source = " fSource)
   (setq fx (dataMineLib.compileLambda fSource))
;   (setq cursor (dataMineLib.open theTable meta:))
   (cursor.restore)
   (setq startTime (getTickCount 0))
   (fx cursor)
   (setq endTime (getTickCount startTime))
   (cursor.saveView High:)
   (testIt  "failed testing view " (cursor.isView High:) true)
   (writeln "Filtered high record count = " cursor.recordCount)
   (if printSW
      (loop for i from 0 until cursor.recordCount do  
         (setq x (cursor.read i))
         (writeln "Reading record [" i "]: " x)
         ) ;; end loop
      ) ; end if
   (setq recCount cursor.recordCount)
;   (setq cursor (dataMineLib.close cursor))
   (writeln "Running filter on " 
        (string theTable) " = " (text (/ (* (/ endTime 3600) 10000000) recCount) "#,###.#0") " hours.")
   ;; Read and check each record for validity.
;   (setq cursor (dataMineLib.open theTable meta:))
   (writeln "Starting logical OR of both filtered views.")
   (cursor.viewMath or: High: Low:)
   (cursor.sort (lambda(x y) (< x.Cash y.Cash)))
   (writeln "Combined record count = " cursor.recordCount)
   (loop for i from 0 until cursor.recordCount do  
      (setq dbRec (new Vector: 3 (append "John" i) "Sales Rep" (money (+ $50000 i))))
      (setq x (cursor.read i))
      ;(testIt  "failed record compare " dbRec x)
      (if printSW (writeln "Reading record [" i "]: " x))
      ) ;; end loop
   (setq cursor (dataMineLib.close cursor))
   (dataMineLib.systemCheck usingExtIdx)
   true) ;; end combineViews






































;;**EXPORTKEY**:dataMineLib.selfTest.combineViews
(defchild  dataMineLib.selfTest combineViews(recCount printSW)
   ;; *******************************************************************
   ;; summary:  Run a filter on a relational table, and then combine
   ;;           the resulting views logically.
   ;; Args:     recCount       The number of records to create.
   ;;           printSW        Switch to turn on/off debug printing.
   ;; Return:   true
   ;; *******************************************************************
   vars:(dbRec x i j cursor startTime endTime viewSize fx fSource)
   (dataMineLib.usingForCreate dataMineLibSelfTest: "testSuite")
   (writeln _eol "***************************") 
   (writeln "combineViews: Combine filtered views on a relation.") 
   (makeRelation recCount printSW)
   (setq fSource (append "all Salary < " (integer (+ 50000 (/ recCount 2)))))
   (writeln "Filter source = " fSource)
   (setq fx (dataMineLib.compileLambda fSource))
   (setq cursor (dataMineLib.open myTable:))
   (setq startTime (getTickCount 0))
   (fx cursor)
   (setq endTime (getTickCount startTime))
   (cursor.saveView Low:)
   (writeln "Filtered low record count = " cursor.recordCount)
   (if printSW
      (loop for i from 0 until cursor.recordCount do  
         (setq x (cursor.read i))
         (writeln "Reading record [" i "]: " x)
         ) ;; end loop
      ) ; end if
   (setq cursor (dataMineLib.close cursor))
   (writeln "Running filter on a 10Mx3C relation = " (text (/ (* (/ endTime 3600) 10000000) recCount) "#,###.#0") " hours.")
   ;; Run a second filter on the table.
   (setq fSource (append "all Salary >= " (integer (+ 50000 (/ recCount 2)))))
   (writeln "Filter source = " fSource)
   (setq fx (dataMineLib.compileLambda fSource))
   (setq cursor (dataMineLib.open myTable:))
   (cursor.restore)
   (setq startTime (getTickCount 0))
   (fx cursor)
   (setq endTime (getTickCount startTime))
   (cursor.saveView High:)
   (testIt  "failed testing view " (cursor.isView High:) true)
   (writeln "Filtered high record count = " cursor.recordCount)
   (if printSW
      (loop for i from 0 until cursor.recordCount do  
         (setq x (cursor.read i))
         (writeln "Reading record [" i "]: " x)
         ) ;; end loop
      ) ; end if
   
   (setq cursor (dataMineLib.close cursor)) 
   
   (writeln "Running filter on a 10Mx3C relation = " (text (/ (* (/ endTime 3600) 10000000) recCount) "#,###.#0") " hours.")
   ;; Read and check each record for validity.
   (setq cursor (dataMineLib.open myTable:))
   
   (writeln "Starting logical OR of both filtered views.")
   (cursor.viewMath or: High: Low:)
   (cursor.sort (lambda(x y) (< x.Salary y.Salary)))
   (writeln "Combined record count = " cursor.recordCount)
   (loop for i from 0 until cursor.recordCount do  
      (setq dbRec (new Vector: 3 (append "John" i) "Sales Rep" (money (+ $50000 i))))
      (setq x (cursor.read i))
      (testIt  "failed record compare " dbRec x)
      (if printSW (writeln "Reading record [" i "]: " x))
      ) ;; end loop
   (setq cursor (dataMineLib.close cursor))
   (dataMineLib.systemCheck dataMineLib.usingExtIndex)
   true) ;; end combineViews






































;;**EXPORTKEY**:dataMineLib.selfTest.createBufferedRelation2
(defchild  dataMineLib.selfTest createBufferedRelation2(readCount bufSize printSW)
   ;; *******************************************************************
   ;; summary:  Populate a table with records to test buffering.
   ;; Args:     recCount       The number of records to create.
   ;;           printSW        Switch to turn on/off debug printing.
   ;; Return:   true
   ;; *******************************************************************
   vars:(dbRec i j cursor totWriteSize avgRecSize startTime endTime noBufReadTime bufReadTime1 bufReadTime2 bufReadTime3 recCount
   bufSizes bufIndex diskCursor staticCursor x y k insertCount deleteCount writeCount appendCoount readCount thisOperation optCount nextVal templateRecord
   myBuf showBufSW)

   (dataMineLib.usingForCreate dataMineLibSelfTest: "testSuite")
                 
   (setq showBufSW false) ; set to true only for debugging

	(defun showBuffer(myBuf)
		vars:(i j)
		(if (not showBufSW) (return true))
		(writeln "myBuf.bufHead=" myBuf.bufHead " myBuf.bufTail=" myBuf.bufTail " myBuf.bufUsed=" myBuf.bufUsed " myBuf.bufSize=" myBuf.bufSize)
		(loop for i from 0 until (length myBuf.buffer) do
			(if (<> myBuf.buffer[i] #void)  
				(begin
				(setq j (* (fraction (cdr myBuf.buffer[i])) 16777216 ))
				(writeln "i=" i " prevRecord=" (floor (cdr myBuf.buffer[i])) " nextRecord=" j )
				end)
			)
		)
	);showBuffer


	(setq totWriteSize 0)
	(setq avgRecSize 0)
   
   ;; Write each record to the table.
      (dataMineLib.dropTable myTable:)
      (dataMineLib.createTable myTable:)
      (setq startTime (getTickCount 0))
      (setq recCount (dataMineLib.importTab myTable: overwrite: "Stocks980715.sbf"))
      (setq endTime (getTickCount startTime))
      (writeln "Importing Stocks980715.sbf into baseData: table took " endTime " seconds.")

   ;; Read and check each record randomly.
	(writeln "*****************************")
 	(writeln "Opening unbuffered cursor for random access time test")
   (setq startTime (getTickCount 0))
   (setq cursor (dataMineLib.open myTable: -1))
   (setq recCount cursor.recordCount)
;   (writeln "rowVector=" cursor.rowVector)
   (loop for i from 0 until readCount do  
      (setq j (integer (random recCount)))
      (setq x (cursor.read j))
      (if printSW (writeln "Reading record [" j "]: " x))
      ) ;; end loop
   (setq cursor (dataMineLib.close cursor))
   (setq endTime (getTickCount startTime))
   (writeln "Reading relation = " endTime " seconds")
   (setq noBufReadTime endTime)

   ;; Read and check each record randomly.
	(writeln "*****************************")
 	(writeln "Opening table for random access time test with buffer size=" bufSize)
   (setq startTime (getTickCount 0))
   (setq cursor (dataMineLib.open myTable: bufSize))
   (setq recCount cursor.recordCount)
;   (writeln "rowVector=" cursor.rowVector)
   (loop for i from 0 until readCount do
      (setq j (integer (random recCount)))
      (setq x (cursor.read j))
      (if printSW (writeln "Reading record [" j "]: " x))
      ) ;; end loop
   (writeln "diskHits=" cursor.dbNumDiskHits " bufferHits=" cursor.dbNumBufHits  )
   (setq cursor (dataMineLib.close cursor))
   (setq endTime (getTickCount startTime))
   (writeln "Reading relation = " endTime " seconds")
   (setq bufReadTime1 endTime)
   (dataMineLib.systemCheck dataMineLib.usingExtIndex)
    
    ;; Open table and get a record to use as a template for new record creation in the following test
    (setq diskCursor (dataMineLib.open myTable: -1))
    (setq templateRecord (diskCursor.read 0))
    (setq diskCursor (dataMineLib.close diskCursor))

	;; Open concurrent Static and Disk cursors on the same table and test insert, delete and update operations for different buffer sizes
	(writeln "******************************************")
	(writeln "Performing concurrent Static and Disk cursor manipulation of table with different buffer sizes")
	
	(setq bufSizes #( 5 -1 100))
	(loop for bufIndex from 0 until (length bufSizes) do
		;open disk cursor
		(writeln "Opening disk cusor on table with bufSize=" bufSizes[bufIndex])
		(setq diskCursor (dataMineLib.open myTable: bufSizes[bufIndex] disk:))
		(writeln "Opening static cursor on table with bufSize=" bufSizes[bufIndex])
		(setq staticCursor (dataMineLib.open myTable: bufSizes[bufIndex] static:))
		(if (<> diskCursor.recordCount staticCursor.recordCount)
			(begin
			(writeln "disk and static cursors on myTable: have different record counts!")
			(writeln "diskCursor.recordCount=" diskCursor.recordCount " staticCursor.recordCount="staticCursor.recordCount)
			end))
		(writeln "Performing random read against both cursors")
		(loop for i from 0 until readCount do
			(setq j (integer (random recCount)))
			(setq x (diskCursor.read j))
			(setq y (staticCursor.read j))
			(if (= (compare x y) false) (begin
				(writeln "Records read from cursors do not match")
				(writeln "diskCursor   record["j"]=" x)
				(writeln "staticCursor record["j"]=" y)
				end)
			);if
		); i
		(writeln "Peforming random insertions/deletions on disk cursor")
		;check buffer
		(setq myBuf diskCursor.myBuf)
		(setq optCount (integer (/ recCount 2))) 
		(setq deleteCount 0)
		(setq insertCount 0)
		(setq writeCount 0)
		(setq appendCount 0)
		(setq readCount 0)
		(setq nextVal recCount) ; nextVal used to create new unique records
		(loop for i from 0 until optCount do
			(setq j (integer (random (-- recCount))))
			(setq thisOperation (integer (random 7))); 0=delete, 1=insert, 2=write, 3=append, 5,6,7=read
			(cond
			((= thisOperation 0)
				(begin ; deletion
;				(writeln "delete " j)
				(diskCursor.delete j)
				(showBuffer myBuf)
				(++ deleteCount)
				end)
			);
			((= thisOperation 1)
				(begin ;insertion
;				(writeln "insert " j)
				(++ nextVal)
;				(debug traceon:)
				(setq x (copy templateRecord))
				(setq x[2] nextVal)
				(diskCursor.insert j x)
				(showBuffer myBuf)
				(++ insertCount)
				end)
			)
			((= thisOperation 2)
				(begin ;write
;				(writeln "write " j)
				(++ nextVal)
				(setq x (copy templateRecord))
;				(debug traceon:)
				(setq x[2] nextVal)
;				(if (= j recCount) (-- j))
				(diskCursor.write j x)
				(showBuffer myBuf)
				(++ writeCount)
				end)
			)
			((= thisOperation 3)
				(begin ;append
;				(writeln "append ")
				(++ nextVal)
;				(debug traceon:)
				(setq x (copy templateRecord))
				(setq x[2] nextVal)
				(diskCursor.write recCount x)
				(showBuffer myBuf)
				(++ appendCount)
				end)
			)
			((or (= thisOperation 4) (= thisOperation 5) (= thisOperation 6) (= thisOperation 7))
				(begin ;read
				(setq x (diskCursor.read j))
				(showBuffer myBuf)
				(++ readCount)
				end)
        	)
			);cond

				
		); i 		
	
		(setq recCount diskCursor.recordCount)
		(writeln "diskCursor.recordCount=" diskCursor.recordCount)
		(if (<> diskCursor.recordCount staticCursor.recordCount)
			(error "disk and static cursors have different record counts!"))
		;make sure disk and static cursor have identical content 
		(loop for k from 0 until recCount do
			(setq x (diskCursor.read k))  
			(setq y (staticCursor.read k))
			(if (= (compare x y) false) (begin
				(writeln "Records read from cursors do not match")
				(writeln "diskCursor   record["k"]=" x)
				(writeln "staticCursor record["k"]=" y)
				end)
			);if
	    ); k

		(setq diskCursor (dataMineLib.close diskCursor))
		(setq staticCursor (dataMineLib.close staticCursor))
		(writeln "insertCount=" insertCount " deleteCount="deleteCount " writeCount="writeCount " appendCount="appendCount " readCount="readCount)
	); bufIndex
	(writeln "********************************************")
	(dataMineLib.systemCheck dataMineLib.usingExtIndex)
   true) ;; end createRelation






































;;**EXPORTKEY**:dataMineLib.selfTest.createIndex
(defchild  dataMineLib.selfTest createIndex(recCount printSW)
   ;; *******************************************************************
   ;; summary:  Populate a small table with records, index them and then read them back by index
   ;; Args:     recCount       The number of records to create.
   ;;           printSW        Switch to turn on/off debug printing.
   ;; Return:   true
   ;; *******************************************************************
	vars:(dbRec i j c cursor startTime endTime Recs Rows departments empID dept)

   (dataMineLib.usingForCreate dataMineLibSelfTest: "testSuite")
	
	(setq departments (new Vector: 3 sales: admin: prod:))
	(setq empID recCount)

	(dataMineLib.systemCheck dataMineLib.usingExtIndex)
	
	
	(writeln _eol "***************************") 
	(writeln "******* getBestIndex function test ******")
	(writeln ">>(dataMineLib.dropTable IndexTest:)")
	(dataMineLib.dropTable IndexTest:) 
	
	(writeln ">>(dataMineLib.createTable IndexTest: Name: Dept: Salary: EmpID:)") 
	(dataMineLib.createTable IndexTest: Name: Dept: Salary: EmpID:)
	
	(writeln ">>(dataMineLib.open IndexTest: memory:)") 
	(setq cursor (dataMineLib.open IndexTest: memory:))

	(writeln ">>(cursor.createIndex Salary: #{Salary: A:})") 
	(cursor.createIndex Salary: #{Salary: A:}) 
	
	(writeln ">>(cursor.createIndex SalaryEmpID: #{Salary: A: EmpID: A:})") 
	(cursor.createIndex SalaryEmpID: #{Salary: A: EmpID: A:}) 

	(writeln "(cursor.getBestIndex #(Salary: EmpID:))")
	(writeln (cursor.getBestIndex #(Salary: EmpID:)))
	(writeln "(cursor.getBestIndex #(Salary:))")
	(writeln (cursor.getBestIndex #(Salary:)))
	(setq cursor (dataMineLib.close cursor))
	
	(writeln _eol "***************************") 
;;;;;;;;;;;;;;;;;
	(writeln "****** memory cursor index tests ******")
	(writeln ">>(dataMineLib.dropTable IndexTest:)")
	(dataMineLib.dropTable IndexTest:) 
	
	(writeln ">>(dataMineLib.createTable IndexTest: Name: Dept: Salary: EmpID:)") 
	(dataMineLib.createTable IndexTest: Name: Dept: Salary: EmpID:)
	
	(writeln ">>(dataMineLib.open IndexTest: memory:)") 
	(setq cursor (dataMineLib.open IndexTest: memory:))

	(setq dept 0)
	(loop for i from 0 until recCount do
		(setq dbRec (new Vector: 4 (append "John" i) departments[dept] (money (+ $50000 i)) empID))
		(setq empID (- empID 1))
		(setq dept (+ dept 1))
		(if (= dept (length departments)) (setq dept 0))
		(if printSW (writeln "Writing record [" i "]: " dbRec))
		(cursor.write i dbRec)
	);i 

	(writeln _eol "***************************") 
	(writeln ">>(cursor.createIndex Salary: #{Salary: A:})") 
	(cursor.createIndex Salary: #{Salary: A:}) 

;	(writeln cursor.indexDictionary[Salary:].index.Pv.blocks)
;	(writeln cursor.bckVectorFrameID)

	(writeln ">>(cursor.indexView Salary:)")
	(cursor.indexView Salary:)
	
	(loop for i from 0 until recCount do
		(setq dbRec (cursor.read i))
		(if printSW (writeln dbRec))
	);i

	(writeln _eol "***************************") 
	(writeln ">>(cursor.createIndex Dept: #{Dept: A:})")
	(cursor.createIndex Dept: #{Dept: A:}) 

;	(writeln cursor.indexDictionary[Dept:].index.Pv.blocks)
;	(writeln cursor.bckVectorFrameID)

	(writeln ">>(indexVeiw Dept:)")
	(cursor.indexView Dept:)
	(loop for i from 0 until recCount do
		(setq dbRec (cursor.read i))
		(if printSW (writeln dbRec))
	);i


;	(writeln cursor.indexDictionary[Dept:].index.Pv.blocks)
;	(writeln cursor.bckVectorFrameID)


	(writeln ">>(cursor.indexView Dept: admin:)")
	(cursor.indexView Dept: admin:) 
	(writeln ">>(writeln (cursor.recordCount))")
	(setq c cursor.recordCount)
	(writeln c)
	(loop for i from 0 until c do
		(setq dbRec (cursor.read i))
		(if printSW (writeln dbRec))
	);i
	

	(writeln _eol "***************************") 
	(writeln ">>(cursor.createIndex EmpID: #{EmpID: A:} unique:)")
	(cursor.createIndex EmpID: #{EmpID: A:} unique:)
	(writeln ">>(cursor.reset)")
	(cursor.reset)
	(loop for i from 1 to recCount do 
		;(writeln i)  
		(setq Recs (cursor.getRecordsByKey EmpID: i))
		(if (> (length Recs) 1) (writeln "ERROR! getRecordsByKey on unique index returned more than one record!"))
		(if printSW (writeln Recs[0]))
	);i
	
	
	(writeln _eol "***************************") 
	(writeln ">>(cursor.createIndex Dept&Salary: #{Dept: A: Salary: A:})")
	(cursor.createIndex Dept&Salary: #{Dept: A: Salary: A:}) 
	(writeln ">>(cursor.indexView Dept&Salary:)")
	(cursor.indexView Dept&Salary:) 
	(writeln ">>(writeln (cursor.recordCount))")
	(setq c cursor.recordCount)
	(writeln c)
	(loop for i from 0 until c do
		(setq dbRec (cursor.read i))
		(if printSW (writeln dbRec))
	);i


	(writeln _eol "***************************") 
	(writeln ">>(cursor.createIndex Dept&EmpID: #{Dept: A: EmpID: A:})")
	(cursor.createIndex Dept&EmpID: #{Dept: A: EmpID: A:}) 
	(writeln ">>(cursor.indexView Dept&EmpID:)")
	(cursor.indexView Dept&EmpID:) 
	(writeln ">>(writeln (cursor.recordCount))")
	(setq c cursor.recordCount)
	(writeln c)
	(loop for i from 0 until c do
		(setq dbRec (cursor.read i))
		(if printSW (writeln dbRec))
	);i



	(writeln ">>(setq cursor (dataMineLib.close cursor))")
	(setq cursor (dataMineLib.close cursor))
	(dataMineLib.systemCheck dataMineLib.usingExtIndex)
	



;;;;;;;;;;;;;;;;;
	(writeln ">>(dataMineLib.dropTable IndexTest:)")
	(dataMineLib.dropTable IndexTest:) 
	
	(writeln ">>(dataMineLib.createTable IndexTest: Name: Dept: Salary: EmpID:)") 
	(dataMineLib.createTable IndexTest: Name: Dept: Salary: EmpID:)
	
	(writeln ">>(dataMineLib.open IndexTest: memory:)") 
	(setq cursor (dataMineLib.open IndexTest: memory:))

	(writeln ">>(cursor.createIndex Salary: #{Salary: A:})")
	(cursor.createIndex Salary: #{Salary: A:}) 
	
	(setq dept 0)
	(loop for i from 0 until recCount do
		(setq dbRec (new Vector: 4 (append "John" i) departments[dept] (money (+ $50000 i)) empID))
		(setq empID (- empID 1))
		(setq dept (+ dept 1))
		(if (= dept (length departments)) (setq dept 0))
		(if printSW (writeln "Writing record [" i "]: " dbRec))
		(cursor.write i dbRec)
	);i 

	(writeln _eol "***************************") 

;	(writeln cursor.indexDictionary[Salary:].index.Pv.blocks)
;	(writeln cursor.bckVectorFrameID)

	(writeln ">>(cursor.indexView Salary:)")
	(cursor.indexView Salary:)
	
	(loop for i from 0 until recCount do
		(setq dbRec (cursor.read i))
		(if printSW (writeln dbRec))
	);i

	(writeln ">>(setq cursor (dataMineLib.close cursor))")
	(setq cursor (dataMineLib.close cursor))
	(dataMineLib.systemCheck dataMineLib.usingExtIndex)
	
	(writeln _eol "***************************") 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	(writeln "***** disk cursor index tests *******")
	(writeln ">>(dataMineLib.dropTable IndexTest:)")
	(dataMineLib.dropTable IndexTest:) 
	
	(writeln ">>(dataMineLib.createTable IndexTest: Name: Dept: Salary: EmpID:)") 
	(dataMineLib.createTable IndexTest: Name: Dept: Salary: EmpID:)
	
	(writeln ">>(setq cursor (dataMineLib.open IndexTest:))")
	(setq cursor (dataMineLib.open IndexTest:))

	(setq dept 0)
	(setq empID recCount)
	(loop for i from 0 until recCount do
		(setq dbRec (new Vector: 4 (append "John" i) departments[dept] (money (+ $50000 i)) empID))
		(setq empID (- empID 1))
		(setq dept (+ dept 1))
		(if (= dept (length departments)) (setq dept 0))
		(if printSW (writeln "Writing record [" i "]: " dbRec))
		(cursor.write i dbRec)
	);i 

	(writeln ">>(setq cursor (dataMineLib.close cursor))")
	(setq cursor (dataMineLib.close cursor))
	(setq endTime (getTickCount startTime))


	(writeln ">>(setq cursor (dataMineLib.open IndexTest:))")
	(setq cursor (dataMineLib.open IndexTest:))

	(writeln _eol "***************************") 
	(writeln ">>(cursor.createIndex Salary: #{Salary: A:})")
	(cursor.createIndex Salary: #{Salary: A:}) 

;	(writeln cursor.indexDictionary[Salary:].index.Pv.blocks)
;	(writeln cursor.bckVector)

 	(writeln ">>(cursor.indexView Salary:)")
	(cursor.indexView Salary:)
	(loop for i from 0 until recCount do
		(setq dbRec (cursor.read i))
		(if printSW (writeln dbRec))
	);i

	(writeln _eol "***************************") 
	(writeln ">>(cursor.createIndex Dept: #{Dept: A:})")
	(cursor.createIndex Dept: #{Dept: A:}) 

	(writeln ">>(cursor.indexView Dept:)")
	(cursor.indexView Dept:)
	(loop for i from 0 until recCount do
		(setq dbRec (cursor.read i))
		(if pirntSW (writeln dbRec))
	);i

	(writeln ">>(cursor.indexView Dept: admin:)")
	(cursor.indexView Dept: "admin") 
	(setq c cursor.recordCount)
	(writeln ">>(writeln (cursor.recordCount))")
	(writeln c)
	(loop for i from 0 until c do
		(setq dbRec (cursor.read i))
		(if printSW (writeln dbRec))
	);i
	

	(writeln _eol "***************************") 
	(writeln ">>(cursor.createIndex EmpID: #{EmpID: A:} unique:)")
	(cursor.createIndex EmpID: #{EmpID: A:} unique:)
	(writeln ">>(cursor.reset)")
	(cursor.reset)
	(loop for i from 1 to recCount do 
		;(writeln i)
		(setq Recs (cursor.getRecordsByKey EmpID: i))
		(if (> (length Recs) 1) (writeln "ERROR! getRecordsByKey on unique index returned more than one record!"))
		(if printSW (writeln Recs[0]))
	);i


	(writeln _eol "***************************") 
	(writeln ">>(cursor.createIndex Dept&Salary: #{Dept: A: Salary: A:})")
	(cursor.createIndex Dept&Salary: #{Dept: A: Salary: A:}) 
	(writeln ">>(cursor.indexView Dept&Salary:)")
	(cursor.indexView Dept&Salary:) 
	(writeln ">>(writeln (cursor.recordCount))")
	(setq c cursor.recordCount)
	(writeln c)
	(loop for i from 0 until c do
		(setq dbRec (cursor.read i))
		(if printSw (writeln dbRec))
	);i

	(writeln _eol "***************************") 
	(writeln ">>(cursor.createIndex Dept&EmpID: #{Dept: A: EmpID: A:})")
	(cursor.createIndex Dept&EmpID: #{Dept: A: EmpID: A:}) 
	(writeln ">>(cursor.indexView Dept&EmpID:)")
	(cursor.indexView Dept&EmpID:) 
	(writeln ">>(writeln (cursor.recordCount))")
	(setq c cursor.recordCount)
	(writeln c)
	(loop for i from 0 until c do
		(setq dbRec (cursor.read i))
		(if printSW (writeln dbRec))
	);i

	(writeln ">>(setq cursor (dataMineLib.close cursor))")
	(setq cursor (dataMineLib.close cursor))
	(dataMineLib.systemCheck dataMineLib.usingExtIndex)
	
	(writeln _eol "***************************") 
;;;;;;;;;;;;;;;;;;;
	(writeln "***** disk cursor save test test *******")
	(writeln "(setq cursor (dataMineLib.open IndexTest:))")
	(setq cursor (dataMineLib.open IndexTest:))

	(writeln ">>(cursor.createIndex Dept&EmpID&Salary: #{Dept: A: EmpID: A: Salary: A:})")
	(cursor.createIndex Dept&EmpID&Salary: #{Dept: A: EmpID: A: Salary: A:}) 
;(writeln cursor.myBuf.indexDictionary)

	(writeln ">>(setq cursor (dataMineLib.close cursor))")
	(setq cursor (dataMineLib.close cursor))

	(writeln "(setq cursor (dataMineLib.open IndexTest:))")
	(setq cursor (dataMineLib.open IndexTest:))
;(writeln cursor.myBuf.indexDictionary)

	(setq c cursor.recordCount)
	(writeln c)
	(loop for i from 0 until c do
		(setq dbRec (cursor.read i))
		(if printSW (writeln dbRec))
	);i

		
	(writeln ">>(cursor.indexView Dept&EmpID&Salary:)")
	(cursor.indexView Dept&EmpID&Salary:) 

	(writeln ">>(writeln (cursor.recordCount))")
	(setq c cursor.recordCount)
	(writeln c)
	(loop for i from 0 until c do
		(setq dbRec (cursor.read i))
		(if printSW (writeln dbRec))
	);i
	
	(writeln ">>(setq cursor (dataMineLib.close cursor))")
	(setq cursor (dataMineLib.close cursor))
	(dataMineLib.systemCheck dataMineLib.usingExtIndex)

	(writeln _eol "***************************") 
;;;;;;;;;;;;;;;;;;;
	(writeln "***** disk cursor load by index tests *******")
	(writeln "(setq cursor (dataMineLib.open IndexTest: loadByIndex: Dept:))")
	(setq cursor (dataMineLib.open IndexTest: loadByIndex: Dept:))

	(writeln ">>(writeln (cursor.recordCount))")
	(setq c cursor.recordCount)
	(writeln c)
	(loop for i from 0 until c do
		(setq dbRec (cursor.read i))
		(if printSW (writeln dbRec))
	);i
	
	(writeln ">>(setq cursor (dataMineLib.close cursor))")
	(setq cursor (dataMineLib.close cursor))
	(dataMineLib.systemCheck dataMineLib.usingExtIndex)

	(writeln "(setq cursor (dataMineLib.open IndexTest: loadByIndex: Dept: admin:))")
	(setq cursor (dataMineLib.open IndexTest: loadByIndex: Dept: admin:))

	(writeln ">>(writeln (cursor.recordCount))")
	(setq c cursor.recordCount)
	(writeln c)
	(loop for i from 0 until c do
		(setq dbRec (cursor.read i))
		(if printSW (writeln dbRec))
	);i
	
	(writeln ">>(setq cursor (dataMineLib.close cursor))")
	(setq cursor (dataMineLib.close cursor))
	(dataMineLib.systemCheck dataMineLib.usingExtIndex)


;;;;;;;;;;;;;;;;;;;
	(writeln "***** disk cursor delete tests *******")
	(writeln "(setq cursor (dataMineLib.open IndexTest: loadByIndex: Dept:))")
	(setq cursor (dataMineLib.open IndexTest: loadByIndex: Dept:))
	(writeln " use getRecordRowsByKey to delete each record in the admin department")

	(setq rows (cursor.getRecordRowsByKey Dept: admin:))
	(if printSW (writeln "deleting row=" rows))
	(setq len (- (length rows) 1))
	(loop for i from len to 0
		(cursor.delete rows[i])
	);i
	
	(writeln ">>(writeln (cursor.recordCount))")
	(setq c cursor.recordCount)
	(writeln c)
	(loop for i from 0 until c do
		(setq dbRec (cursor.read i))
		(if printSW (writeln dbRec))
	);i

	(writeln ">>(setq cursor (dataMineLib.close cursor))")
	(setq cursor (dataMineLib.close cursor))

	(dataMineLib.systemCheck dataMineLib.usingExtIndex)
	
true)	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	















;;**EXPORTKEY**:dataMineLib.selfTest.createMemory
(defchild dataMineLib.selfTest createMemory(recCount printSW)
   ;; *******************************************************************
   ;; summary:  Populate a small table with records and then read them back.
   ;; Args:     recCount       The number of records to create.
   ;;           printSW        Switch to turn on/off debug printing.
   ;; Return:   true
   ;; *******************************************************************
   vars:(dbRec i j cursor startTime endTime)

   (dataMineLib.usingForCreate dataMineLibSelfTest: "testSuite")

   (dataMineLib.dropTable myTable:)
   (dataMineLib.createTable myTable: Name: Title: Salary:)
   ;; Write each record to the table.
   (writeln _eol "***************************") 
   (writeln ">>createMemory: Populate a relation with records and then read them back.") 
   (setq startTime (getTickCount 0))
   (setq cursor (dataMineLib.open myTable: memory:))
   (writeln ">>Opening table myTable as memory cursor")
   (writeln "My cursor type is " cursor.myObjectType)
   (loop for i from 0 until recCount do
      (setq dbRec (new Vector: 3 (append "John" i) "Sales Rep" (money (+ $50000 i))))
      (cursor.write i dbRec) 
      (if printSW (writeln "Writing record [" i "] " dbRec ))
      ) ;; end loop
   (writeln ">>Calling save on the memory cursor") 
   (cursor.save) 
   (setq cursor (dataMineLib.close cursor))
   (setq endTime (getTickCount startTime))
   (writeln ">>Populating a 10Mx3C relation = " (text (/ (* (/ endTime 3600) 10000000) recCount) "#,###.#0") " hours.")

   ;; Read and check each record for validity.
   (writeln _eol "***************************") 
   (setq startTime (getTickCount 0))
	(writeln ">>Opening table myTable as memory cursor for sequential read validity test")
   (setq cursor (dataMineLib.open myTable: memory:))  
   (loop for i from (sub1 recCount) to 0 by -1 do  
      (if printSW (writeln "Reading record [" i "]"))
      (setq dbRec (new Vector: 3 (append "John" i) "Sales Rep" (money (+ $50000 i))))
      (setq x (cursor.read i))
      (testIt  "failed record compare " dbRec x)
      ) ;; end loop
   (setq cursor (dataMineLib.close cursor))
   (setq endTime (getTickCount startTime))
   (writeln ">>Validating a 10Mx3C relation = " (text (/ (* (/ endTime 3600) 10000000) recCount) "#,###.#0") " hours.")
   ;; Read and check each record randomly.
   (writeln _eol "***************************") 
   (setq startTime (getTickCount 0))
   (writeln ">>Opening table myTale as memory cursor random read validity test")
   (setq cursor (dataMineLib.open myTable: memory:))
   (loop for i from 0 until recCount do  
      (if printSW (writeln "Reading record [" j "]"))
      (setq j (integer (random recCount)))
      (setq dbRec (new Vector: 3 (append "John" j) "Sales Rep" (money (+ $50000 j))))
      (setq x (cursor.read j))
      (testIt  "failed record compare " dbRec x)
      ) ;; end loop
   (setq cursor (dataMineLib.close cursor))
   (setq endTime (getTickCount startTime))
   (writeln ">>Randomly checking a 10Mx3C relation = " (text (/ (* (/ endTime 3600) 10000000) recCount) "#,###.#0") " hours.")
   (dataMineLib.systemCheck dataMineLib.usingExtIndex)
   (writeln "***************************") 
   ;; Open Memory cursor with subset of columns
   (writeln _eol "***************************") 
   (setq startTime (getTickCount 0))
	(writeln ">>Opening table myTale as memory cursor with subset of columns")
   (setq cursor (dataMineLib.open myTable: memory: #(Name: Salary:)))
   (if (> recCount 10) (setq recCount 10))
   (loop for i from 0 until recCount do  
      (setq j (integer (random recCount)))
      (setq x (cursor.read j))
      (writeln "Reading record [" j "] =" x)
      ) ;; end loop
   (setq cursor (dataMineLib.close cursor))
   (setq endTime (getTickCount startTime))
   (writeln ">>Checking opening table with subset of columns= " (text (/ (* (/ endTime 3600) 10000000) recCount) "#,###.#0") " hours.")
   (dataMineLib.systemCheck dataMineLib.usingExtIndex)
   (writeln "***************************") 
   true) ;; end createMemory






































;;**EXPORTKEY**:dataMineLib.selfTest.createRelation
(defchild  dataMineLib.selfTest createRelation(recCount printSW)
   ;; *******************************************************************
   ;; summary:  Populate a small table with records and then read them back.
   ;; Args:     recCount       The number of records to create.
   ;;           printSW        Switch to turn on/off debug printing.
   ;; Return:   true
   ;; *******************************************************************
   vars:(dbRec i j cursor startTime endTime)

   (dataMineLib.usingForCreate dataMineLibSelfTest: "testSuite")
   
   ;; Write each record to the table.
   (writeln _eol "***************************") 
   (writeln ">>createRelation: Populate a relation with records and then read them back.") 
   (writeln ">>Droping table myTable")
   (dataMineLib.dropTable myTable:) 
   (writeln ">>Creating table myTable") 
   (dataMineLib.createTable myTable: Name: Title: Salary:)
   (setq startTime (getTickCount 0))
   (writeln ">>Creating table myTable")
   (setq cursor (dataMineLib.open myTable:))
   (writeln "My cursor type is " cursor.myCursorType)
   (loop for i from 1 until recCount do
      (setq j (subi i 1))
      (setq dbRec (new Vector: 3 (append "John" i) "Sales Rep" (money (+ $50000 i))))
      (cursor.write j dbRec)
      (if printSW (writeln "Writing record [" j "]: " dbRec))
      ) ;; end loop
   (setq dbRec (new Vector: 3 (append "John" 0) "Sales Rep" (money (+ $50000 0))))
   (cursor.insert 0 dbRec)
   (setq cursor (dataMineLib.close cursor))
   (setq endTime (getTickCount startTime))
   (writeln "Populating a 10Mx3C relation = " (text (/ (* (/ endTime 3600) 10000000) recCount) "#,###.#0") " hours.")
   ;; Read and check each record for validity.
   (setq startTime (getTickCount 0))
	(writeln "*****************************")
	(writeln "Opening table myTable for sequential compare validation")
   (setq cursor (dataMineLib.open myTable:))
   (loop for i from (sub1 recCount) to 0 by -1 do  
      (setq dbRec (new Vector: 3 (append "John" i) "Sales Rep" (money (+ $50000 i))))
      (setq x (cursor.read i))
      (testIt  "failed record compare " dbRec x)
      (if printSW (writeln "Reading record [" i "]: " x))
      ) ;; end loop
   (setq cursor (dataMineLib.close cursor))
   (setq endTime (getTickCount startTime))
   (writeln "Validating a 10Mx3C relation = " (text (/ (* (/ endTime 3600) 10000000) recCount) "#,###.#0") " hours.")
   ;; Read and check each record randomly.
	(writeln "*****************************")
 	(writeln "Opening table for random access compare validation")
   (setq startTime (getTickCount 0))
   (setq cursor (dataMineLib.open myTable:))
   (loop for i from 0 until recCount do  
      (setq j (integer (random recCount)))
      (setq dbRec (new Vector: 3 (append "John" j) "Sales Rep" (money (+ $50000 j))))
      (setq x (cursor.read j))
      (testIt  "failed record compare " dbRec x)
      (if printSW (writeln "Reading record [" j "]: " x))
      ) ;; end loop
   (setq cursor (dataMineLib.close cursor))
   (setq endTime (getTickCount startTime))
   (writeln "Randomly checking a 10Mx3C relation = " (text (/ (* (/ endTime 3600) 10000000) recCount) "#,###.#0") " hours.")
   (dataMineLib.systemCheck dataMineLib.usingExtIndex)
   true) ;; end createRelation






































;;**EXPORTKEY**:dataMineLib.selfTest.deleteMemory
(defchild  dataMineLib.selfTest  deleteMemory(recCount printSW)
   ;; *******************************************************************
   ;; summary:  Populate a relation with records and then delete them.
   ;; Args:     recCount       The number of records to create.
   ;;           printSW        Switch to turn on/off debug printing.
   ;; Return:   true
   ;; *******************************************************************
   vars:(dbRec i j cursor startTime endTime)

   (dataMineLib.usingForCreate dataMineLibSelfTest: "testSuite")

   (dataMineLib.dropTable myTable:)
   (dataMineLib.createTable myTable: Name: Title: Salary:)
   (writeln _eol "***************************") 

   (writeln ">>deleteMemory: Populate a relation with records and then read them back.") 
   (writeln "Note: Unique test records are created for each row.") 
   (setq startTime (getTickCount 0))
   (setq cursor (dataMineLib.open myTable: memory:))
   (writeln "My cursor type is " cursor.myCursorType)
   (loop for i from 0 until recCount do
      (setq dbRec (new Vector: 3 (append "John" i) "Sales Rep" (money (+ $50000 i))))
      (cursor.write i dbRec)
      (if printSW (writeln "Writing record [" i "]="dbRec))
      ) ;; end loop
   (cursor.save)
   (setq cursor (dataMineLib.close cursor))
   (setq endTime (getTickCount startTime))
   (writeln "Populating a 10Mx3C relation = " (text (/ (* (/ endTime 3600) 10000000) recCount) "#,###.#0") " hours.")

   (dataMineLib.systemCheck dataMineLib.usingExtIndex)



   ;; Read and check each record for validity.
   (setq startTime (getTickCount 0))
   (setq cursor (dataMineLib.open myTable: memory:))
   (loop for i from (sub1 recCount) to 0 by -1 do  
      (setq dbRec (new Vector: 3 (append "John" i) "Sales Rep" (money (+ $50000 i))))
      (setq x (cursor.read i))
      (if printSW (writeln "Reading record [" i "]=" x))
      (testIt  "failed record compare " dbRec x)
      ) ;; end loop

(writeln _eol "cursor.bckVector=" cursor.bckVector)
(writeln "cursor.rowVector=" cursor.rowVector)
(writeln "cursor.bckVectorIndex=" cursor.bckVectorIndex)
(writeln "cursor.bckVectorFrameID=" cursor.bckVectorFrameID _eol)      
      
      
   (setq cursor (dataMineLib.close cursor))
   (setq endTime (getTickCount startTime))
   (writeln "Validating a 10Mx3C relation = " (text (/ (* (/ endTime 3600) 10000000) recCount) "#,###.#0") " hours.")
   (dataMineLib.systemCheck dataMineLib.usingExtIndex)


	(writeln "simple open and close")
	(setq cursor (dataMineLib.open myTable: memory: ))
	(setq cursor (dataMineLib.close cursor))
   (dataMineLib.systemCheck dataMineLib.usingExtIndex)

	(writeln "simple open, save and close")
	(setq cursor (dataMineLib.open myTable: memory:))
	(cursor.save)
	(setq cursor (dataMineLib.close cursor))
   (dataMineLib.systemCheck dataMineLib.usingExtIndex)


   ;; Delete each record randomly.
   (setq startTime (getTickCount 0))  
   
;(writeln "(setq cursor (dataMineLib.open myTable: memory:))")
;   (setq cursor (dataMineLib.open myTable: memory:))
;   (loop for i from 0 until recCount  do  
;      (setq j (integer (random cursor.recordCount)))
;      (cursor.delete j)
;      (if printSW (writeln "Deleting record [" j "]"))
;      ) ;; end loop
;   (cursor.save)
;   (setq cursor (dataMineLib.close cursor))
;   (setq endTime (getTickCount startTime))
;   (writeln "Deleting a 10Mx3C relation = " (text (/ (* (/ endTime 3600) 10000000) recCount) "#,###.#0") " hours.")
;   (dataMineLib.systemCheck dataMineLib.usingExtIndex)
   true) ;; end deleteMemory






































;;**EXPORTKEY**:dataMineLib.selfTest.deleteRelation
(defchild  dataMineLib.selfTest deleteRelation(recCount printSW)
   ;; *******************************************************************
   ;; summary:  Populate a relation with records and then delete them.
   ;; Args:     recCount       The number of records to create.
   ;;           printSW        Switch to turn on/off debug printing.
   ;; Return:   true
   ;; *******************************************************************
   vars:(dbRec i j cursor startTime endTime)

   (dataMineLib.usingForCreate dataMineLibSelfTest: "testSuite")

   (dataMineLib.dropTable myTable:)
   (dataMineLib.createTable myTable: Name: Title: Salary:)
   (writeln _eol "***************************") 
   (writeln "deleteRelation: Populate a relation with records and then read them back.") 
   (writeln "Note: Unique test records are created for each row.") 
   (setq startTime (getTickCount 0))
   (setq cursor (dataMineLib.open myTable:))
   (writeln "My cursor type is " cursor.myObjectType)
   (loop for i from 0 until recCount do
      (setq dbRec (new Vector: 3 (append "John" i) "Sales Rep" (money (+ $50000 i))))
      (cursor.write i dbRec)
      (if printSW (writeln "Writing record [" i "]"))
      ) ;; end loop
   (setq cursor (dataMineLib.close cursor))
   (setq endTime (getTickCount startTime))
   (writeln "Populating a 10Mx3C relation = " (text (/ (* (/ endTime 3600) 10000000) recCount) "#,###.#0") " hours.")
   ;; Read and check each record for validity.
   (setq startTime (getTickCount 0))
   (setq cursor (dataMineLib.open myTable:))
   (loop for i from (sub1 recCount) to 0 by -1 do  
      (setq dbRec (new Vector: 3 (append "John" i) "Sales Rep" (money (+ $50000 i))))
      (setq x (cursor.read i))
      (testIt  "failed record compare " dbRec x)
      (if printSW (writeln "Reading record [" i "]"))
      ) ;; end loop
   (setq cursor (dataMineLib.close cursor))
   (setq endTime (getTickCount startTime))
   (writeln "Validating a 10Mx3C relation = " (text (/ (* (/ endTime 3600) 10000000) recCount) "#,###.#0") " hours.")
   (dataMineLib.systemCheck dataMineLib.usingExtIndex)
   ;; Delete each record randomly.
   (setq startTime (getTickCount 0))
   (setq cursor (dataMineLib.open myTable:))
   (loop for i from 0 until recCount do  
      (setq j (integer (random cursor.recordCount)))
      (if printSW (writeln "Deleting record [" j "]=" (cursor.read j) ))
      (cursor.delete j)
      ) ;; end loop
   (setq cursor (dataMineLib.close cursor))
   (setq endTime (getTickCount startTime))
   (writeln "Deleting a 10Mx3C relation = " (text (/ (* (/ endTime 3600) 10000000) recCount) "#,###.#0") " hours.")
   (dataMineLib.systemCheck dataMineLib.usingExtIndex)
   true) ;; end deleteRelation















;;**EXPORTKEY**:dataMineLib.selfTest.exportRelation
(defchild dataMineLib.selfTest  exportRelation(recCount printSW)
   ;; *******************************************************************
   ;; summary:  Create a small relational table and export it.
   ;; Args:     recCount       The number of records to create.
   ;;           printSW        Switch to turn on/off debug printing.
   ;; Return:   true
   ;; *******************************************************************
   vars:(dbRec i j cursor startTime endTime viewSize)

   (dataMineLib.usingForCreate dataMineLibSelfTest: "testSuite")

   (writeln _eol "***************************") 
   (writeln "exportRelation: Create a relational table and export it.") 
   (makeRelation recCount printSW)
   (writeln "Export a relation to a tab delimited file.") 
   (setq startTime (getTickCount 0))
   (dataMineLib.exportTab myTable: "mytable.tab")
   (setq endTime (getTickCount startTime))
   (writeln "Exporting a 10Mx3C relation = " (text (/ (* (/ endTime 3600) 10000000) recCount) "#,###.#0") " hours.")
   (writeln "Import a relation from a tab delimited file.") 
   (setq startTime (getTickCount 0))
   (dataMineLib.importTab myTable: overwrite: "mytable.tab")
   (setq endTime (getTickCount startTime))
   (writeln "Importing a 10Mx3C relation = " (text (/ (* (/ endTime 3600) 10000000) recCount) "#,###.#0") " hours.")
   ;; Read and check each record for validity.
   (setq startTime (getTickCount 0))
   (setq cursor (dataMineLib.open myTable:))
   (writeln "My cursor type is " cursor.myObjectType)
   (loop for i from (sub1 recCount) to 0 by -1 do  
      (setq dbRec (new Vector: 3 (append "John" i) "Sales Rep" (money (+ $50000 i))))
      (setq x (cursor.read i))
      (testIt  "failed record compare " dbRec x)
      (if printSW (writeln "Reading record [" i "] " x))
      ) ;; end loop
   (setq cursor (dataMineLib.close cursor))
   (setq endTime (getTickCount startTime))
   (writeln "Validating a 10Mx3C relation = " (text (/ (* (/ endTime 3600) 10000000) recCount) "#,###.#0") " hours.")
   (dataMineLib.systemCheck dataMineLib.usingExtIndex)
   true) ;; end exportRelation






































;;**EXPORTKEY**:dataMineLib.selfTest.filterRelation
(defchild  dataMineLib.selfTest filterRelation(recCount printSW cursorType)
   ;; *******************************************************************
   ;; summary:  Run a filter on a relational table.
   ;; Args:     recCount       The number of records to create.
   ;;           printSW        Switch to turn on/off debug printing.
   ;; Return:   true
   ;; *******************************************************************
   vars:(dbRec i j cursor startTime endTime viewSize fx fSource x)

   (dataMineLib.usingForCreate dataMineLibSelfTest: "testSuite")


   (writeln _eol "***************************") 
   (writeln "filterRelation: Run filter Lambda on a relation.") 
   (makeRelation recCount printSW)
   (setq fSource (append "all Salary < " (integer (+ 50000 (/ recCount 2)))))
   (writeln "Filter source = " fSource)
   (setq fx (dataMineLib.compileLambda fSource))
   (setq cursor (dataMineLib.open myTable: cursorType))
   (setq startTime (getTickCount 0))
   (fx cursor)
   (setq endTime (getTickCount startTime))
   (cursor.saveView Test:)

   (writeln "Filtered record count = " cursor.recordCount)
   (setq cursor (dataMineLib.close cursor))
   (writeln "Running filter on a 10Mx3C relation = " 
      (text (/ (* (/ endTime 3600) 10000000) recCount) "#,###.#0") " hours.")
   ;; Read and check each record for validity.
   (setq cursor (dataMineLib.open myTable: cursorType))
   (cursor.restore)
   (cursor.restoreView Test:)
   (writeln "Filtered record count = " cursor.recordCount)
   (loop for i from (subi cursor.recordCount 1) to 0 by -1 do  
      (setq dbRec (new Vector: 3 (append "John" i) "Sales Rep" (money (+ $50000 i))))
      (setq x (cursor.read i))
      (testIt  "failed record compare " dbRec x)
      (if printSW (writeln "Reading record [" i "]: " x))
      ) ;; end loop
   (setq cursor (dataMineLib.close cursor))
   (dataMineLib.systemCheck dataMineLib.usingExtIndex)
   true) ;; end filterRelation






































;;**EXPORTKEY**:dataMineLib.selfTest.importRelation
(defchild  dataMineLib.selfTest importRelation(recCount printSW)
   ;; *******************************************************************
   ;; summary:  Import a relational table and export it.
   ;; Args:     none.
   ;; Return:   true
   ;; *******************************************************************
	vars:(dbRec dbRec2 i j cursor cursor2 startTime endTime fileID s)
	
	(dataMineLib.usingForCreate dataMineLibSelfTest: "testSuite")
	
	(dataMineLib.systemCheck dataMineLib.usingExtIndex)
	(writeln _eol "***************************") 
	(writeln "importRelation: Import a relational table from a large tab delimited file.") 
	
	;write out a large text file to import
	(setq fileID (fileOpen "importText.tab" 1 0))
	(fwriteln fileID "ID A B C D E F G H I J K L M N O P Q R S T U V W X Y Z AA BB CC DD EE FF GG HH II JJ KK LL MM NN OO PP QQ RR SS TT UU VV WW XX YY ZZ")
	(setq s "")
	(loop for i from 0 until 52 do (setq s (append s (char 9) "test data")))
	(loop for i from 0 until 2000 do
		(fwriteln fileID (append (string i) s))
	);i
	(fileClose fileID 1)
	
	(dataMineLib.dropTable myTable:)
	(dataMineLib.createTable myTable:)

	(setq startTime (getTickCount 0))
	(dataMineLib.importTab myTable: overwrite: "importText.tab")
	
	(setq endTime (getTickCount startTime))
	(writeln "Importing a 10Mx240C relation = " (text (/ (* (/ endTime 3600) 10000000) 5000) "#,###.#0") " hours.")
	(dataMineLib.systemCheck dataMineLib.usingExtIndex)
	(writeln "Export a relation to a tab delimited file.") 
	(setq startTime (getTickCount 0))
	(dataMineLib.exportTab myTable: "mytable.tab")
	(setq endTime (getTickCount startTime))
	(writeln "Exporting a 10Mx240C relation = " (text (/ (* (/ endTime 3600) 10000000) 5000) "#,###.#0") " hours.")
	(dataMineLib.systemCheck dataMineLib.usingExtIndex)

	(writeln "Validating a relation after import.") 
	(dataMineLib.dropTable myTable2:)
	(dataMineLib.systemCheck dataMineLib.usingExtIndex)
	(dataMineLib.createTable myTable2:)
	(dataMineLib.systemCheck dataMineLib.usingExtIndex)
	(dataMineLib.importTab myTable2: overwrite: "mytable.tab")
	(dataMineLib.systemCheck dataMineLib.usingExtIndex)

	(setq cursor (dataMineLib.open myTable:))
	(setq cursor2 (dataMineLib.open myTable2:))
	(testIt "Files not the same size " cursor.recordCount cursor2.recordCount)

	(loop for i from 0 until cursor.recordCount do
	(setq dbRec (cursor.read i))
	(setq dbRec2 (cursor2.read i))
	(if printSW (writeln " dbRec[" i "]=" dbRec " " dbRec2))
	;       (if printSW (writeln "dbRec2[" i "]=" dbRec2))
	(testIt "Records do not match " dbRec dbRec2)
	) ;; end loop
	(dataMineLib.close cursor)
	(dataMineLib.close cursor2)
	(dataMineLib.systemCheck dataMineLib.usingExtIndex)
	true) ;; end importRelation
	
	
	
	
	
	
	
	
	
	
	



























;;**EXPORTKEY**:dataMineLib.selfTest.makeProject
(defchild  dataMineLib.selfTest makeProject(recCount printSW)
   ;; *******************************************************************
   ;; summary:  Make a small project.
   ;; Args:     recCount       The number of records to create.
   ;;           printSW        Switch to turn on/off debug printing.
   ;; Return:   true
   ;; *******************************************************************
   vars:(nmMakeProject dbRec i j cursor startTime endTime tables)

   (dataMineLib.usingForCreate dataMineLibSelfTest: "testSuite")

   ;; Make the first testing table.
   (dataMineLib.dropTable firstTable:)
   (dataMineLib.createTable firstTable: Name: Type: Salary:)
   (writeln _eol "Populate a small relation with records.") 
   (writeln "Note: Unique test records are created for each row.") 
   (setq startTime (getTickCount 0))
   (setq cursor (dataMineLib.open firstTable:))
   (writeln "My cursor type is " cursor.myObjectType)
   (loop for i from 0 until recCount do
      (setq dbRec (new Vector: 3 (append "John" i) (bitwiseAnd 1 i) (money (+ $50000 i))))
      (cursor.write i dbRec)
      (if printSW (writeln "Writing record [" i "] " dbRec))
      ) ;; end loop
   (setq cursor (dataMineLib.close cursor))
   (setq endTime (getTickCount startTime))
   (writeln "Populating a 10Mx3C relation = " (text (/ (* (/ endTime 3600) 10000000) recCount) "#,###.#0") " hours.")
   ;; Make the second testing table.
   (dataMineLib.dropTable nextTable:)
   (dataMineLib.createTable nextTable: Name: Type: Salary:)
   (writeln _eol "Populate a small relation with records.") 
   (writeln "Note: Unique test records are created for each row.") 
   (setq startTime (getTickCount 0))
   (setq cursor (dataMineLib.open nextTable:))
   (writeln "My cursor type is " cursor.myObjectType)
   (loop for i from 0 until recCount do
      (setq dbRec (new Vector: 3 (append "John" i) (bitwiseAnd 1 i) (money (+ $60000 i))))
      (cursor.write i dbRec)
      (if printSW (writeln "Writing record [" i "] " dbRec))
      ) ;; end loop
   (setq cursor (dataMineLib.close cursor))
   (setq endTime (getTickCount startTime))
   (writeln "Populating a 10Mx3C relation = " (text (/ (* (/ endTime 3600) 10000000) recCount) "#,###.#0") " hours.")
   ;; Make the third testing table.
   (dataMineLib.dropTable thirdTable:)
   (dataMineLib.createTable thirdTable: Name: Type: Salary:)
   (writeln _eol "Populate a small relation with records.") 
   (writeln "Note: Unique test records are created for each row.") 
   (setq startTime (getTickCount 0))
   (setq cursor (dataMineLib.open thirdTable:))
   (writeln "My cursor type is " cursor.myObjectType)
   (loop for i from 0 until recCount do
      (setq dbRec (new Vector: 3 (append "John" i) (bitwiseAnd 1 i) (money (+ $70000 i))))
      (cursor.write i dbRec)
      (if printSW (writeln "Writing record [" i "] " dbRec))
      ) ;; end loop
   (setq cursor (dataMineLib.close cursor))
   (setq endTime (getTickCount startTime))
   (writeln "Populating a 10Mx3C relation = " (text (/ (* (/ endTime 3600) 10000000) recCount) "#,###.#0") " hours.")
   ;; Make the miner project.
   (dataMineLib.miner.dropProject "Hedge")
   (setq tables (new Vector: object: 3 firstTable: nextTable: thirdTable:))
   (dataMineLib.miner.newProject "Hedge" 
                                   tables 
                                   "Income" "average Salary;" 
                                   #{EvenTypes:   "all Type == 0;"
                                     OddTypes:    "all Type == 1;"
                                     Composite:   {run "OddTypes";}
                                     CompileErr:  "all Type $@ 1;"
                                     RuntimeErr:  "error("bad");"
                                     AllTypes:    "all"
                                     }
                                   ) ; end new project
   (dataMineLib.miner.saveProject clear:) 
   true) ;; end makeProject






































;;**EXPORTKEY**:dataMineLib.selfTest.makeRelation
(defchild  dataMineLib.selfTest makeRelation(recCount printSW)
   ;; *******************************************************************
   ;; summary:  Populate a small relation with records.
   ;; Args:     recCount       The number of records to create.
   ;;           printSW        Switch to turn on/off debug printing.
   ;; Return:   true
   ;; *******************************************************************
   vars:(dbRec i j cursor startTime endTime)

   (dataMineLib.usingForCreate dataMineLibSelfTest: "testSuite")

   (dataMineLib.dropTable myTable:)
   (dataMineLib.createTable myTable: Name: Title: Salary:)
   (writeln _eol "Populate a small relation with records.") 
   (writeln "Note: Unique test records are created for each row.") 
   (setq startTime (getTickCount 0))
   (setq cursor (dataMineLib.open myTable:))
   (writeln "My cursor type is " cursor.myObjectType)
   (loop for i from 0 until recCount do
      (setq dbRec (new Vector: 3 (append "John" i) "Sales Rep" (money (+ $50000 i))))
      (cursor.write i dbRec)
      (if printSW (writeln "Writing record [" i "] " dbRec))
      ) ;; end loop
   (setq cursor (dataMineLib.close cursor))
   (setq endTime (getTickCount startTime))
   (writeln "Populating a 10Mx3C relation = " (text (/ (* (/ endTime 3600) 10000000) recCount) "#,###.#0") " hours.")
   true) ;; end makeRelation






































;;**EXPORTKEY**:dataMineLib.selfTest.parseFilter
(defchild  dataMineLib.selfTest parseFilter(filterSource ...)
   ;; *******************************************************************
   ;; summary:  Parse a filter on a relational table.
   ;; Args:     none.
   ;; Return:   true
   ;; *******************************************************************
   vars:(dbRec i j cursor startTime endTime viewSize fSource)
   (writeln _eol "Parse filter source on a table.") 

   (dataMineLib.usingForCreate dataMineLibSelfTest: "testSuite")

   (makeRelation 10 false)
   (setq fSource filterSource)
   (writeln "Filter source = " fSource)
   (if (= (argCount) 2) (setq dataMineLib.javaScript._verbose (argFetch 1)))
   (setq fx (dataMineLib fSource))
   (setq dataMineLib.javaScript._verbose false)
   (writeln _eol fx)
   true) ;; end parseFilter






































;;**EXPORTKEY**:dataMineLib.selfTest.persistentViews
(defchild dataMineLib.selfTest  persistentViews(recCount printSW)
   ;; *******************************************************************
   ;; summary:  Run a filter on a relational table, update the table
   ;;           then verify that the views are still valid.
   ;; Args:     recCount       The number of records to create.
   ;;           printSW        Switch to turn on/off debug printing.
   ;; Return:   true
   ;; *******************************************************************
   vars:(dbRec x i j cursor startTime endTime viewSize fx fSource)

   (dataMineLib.usingForCreate dataMineLibSelfTest: "testSuite")

   (writeln _eol "***************************") 
   (writeln "peristentViews: Update rows in persistent views on a relation.") 
   (makeRelation recCount printSW)
   (setq fSource (append "all Salary < " (integer (+ 50000 (/ recCount 2)))))
   (writeln "Filter source = " fSource)
   (setq fx (dataMineLib.compileLambda fSource))
   (setq cursor (dataMineLib.open myTable:))
   (setq startTime (getTickCount 0))
   (fx cursor)
   (setq endTime (getTickCount startTime))
   (cursor.saveView Low:)
   (writeln "Filtered low record count = " cursor.recordCount)
   (if printSW
      (loop for i from 0 until cursor.recordCount do  
         (setq x (cursor.read i))
         (writeln "Reading record [" i "]: " x)
      ) ;; end loop
   ) ; end if
   (setq cursor (dataMineLib.close cursor))
   (writeln "Running filter on a 10Mx3C relation = " 
      (text (/ (* (/ endTime 3600) 10000000) recCount) "#,###.#0") " hours.")
   ;; Run a second filter on the table.
   (setq fSource (append "all Salary >= " (integer (+ 50000 (/ recCount 2)))))
   (writeln "Filter source = " fSource)
   (setq fx (dataMineLib.compileLambda fSource))
   (setq cursor (dataMineLib.open myTable:))
   (cursor.restore)
   (setq startTime (getTickCount 0))
   (fx cursor)
   (setq endTime (getTickCount startTime))
   (cursor.saveView High:)
   (testIt  "failed testing view " (cursor.isView High:) true)
   (writeln "Filtered high record count = " cursor.recordCount)
   (if printSW
      (loop for i from 0 until cursor.recordCount do  
         (setq x (cursor.read i))
         (writeln "Reading record [" i "]: " x)
         ) ;; end loop
      ) ; end if
   (setq cursor (dataMineLib.close cursor))
   (writeln "Running filter on a 10Mx3C relation = " 
      (text (/ (* (/ endTime 3600) 10000000) recCount) "#,###.#0") " hours.")
   ;; Read and check each record for validity.
   (setq cursor (dataMineLib.open myTable:))
   (writeln "Writing a record to ensure that views are persistent...")
   (setq x (cursor.read 0))
   (cursor.write 0 x)
   (writeln "Starting logical OR of both filtered views.")
(writeln "line[57]" ) ;***debug insert***
   (cursor.viewMath or: High: Low:)
(writeln "line[59]" ) ;***debug insert*** 
   (cursor.sort (lambda(x y) (< x.Salary y.Salary)))
(writeln "line[61]" ) ;***debug insert***
   (writeln "Combined record count = " cursor.recordCount)
(writeln "line[63]" ) ;***debug insert***
   (loop for i from 0 until cursor.recordCount do  
      (setq dbRec (new Vector: 3 (append "John" i) "Sales Rep" (money (+ $50000 i))))
      (setq x (cursor.read i))
      (testIt  "failed record compare " dbRec x)
      (if printSW (writeln "Reading record [" i "]: " x))
      ) ;; end loop
   (setq cursor (dataMineLib.close cursor))
(writeln "line[71]" ) ;***debug insert***
   (dataMineLib.systemCheck dataMineLib.usingExtIndex)
   true) ;; end persistentViews






































;;**EXPORTKEY**:dataMineLib.selfTest.prepareExtent
(defchild dataMineLib.selfTest  prepareExtent()
	;; *******************************************************************
	;; summary:  Prepare the dataMineLibTest extent for use by the test suite
	;; Return:   true
	;; *******************************************************************
	vars: (i)
	(if (= _dataMineExtents #void) (error "prepareExtent called with void _dataMineExtents"))
	(dataMineLib.clearCursors)
	(setq i (member dataMineLibSelfTest: _dataMineExtents))
	(if (= i false) ;insert dataMineLibSelfTest extent
		(setq _dataMineExtents[dataMineLibSelfTest:] "DataMineLibSelfTest.db"))

	(setq i (member dataMineLibSelfTest2: _dataMineExtents))
	(if (= i false) ;insert dataMineLibSelfTest extent
		(setq _dataMineExtents[dataMineLibSelfTest2:] "DataMineLibSelfTest2.db"))
	
	(dataMineLib);reinitialize the dataMineLib so the new extent is opened 
	(dataMineLib.usingForCreate dataMineLibSelfTest: "DataMineLibSelfTest") 
	true) ;; end prepareExtent






































;;**EXPORTKEY**:dataMineLib.selfTest.rollbackRelation
(defchild dataMineLib.selfTest  rollbackRelation(recCount printSW)
   ;; *******************************************************************
   ;; summary:  Populate a relation with records and then delete them.
   ;; Args:     recCount       The number of records to create.
   ;;           printSW        Switch to turn on/off debug printing.
   ;; Return:   true
   ;; *******************************************************************
   vars:(dbRec i j cursor startTime endTime x)

   (dataMineLib.usingForCreate dataMineLibSelfTest: "testSuite")

   (dataMineLib.dropTable myTable:)
   (dataMineLib.createTable myTable: Name: Title: Salary:)
   
   (writeln _eol "***************************")
   (writeln ">>Test isTable function")
   (writeln "(dataMineLib.isTable myTable:) returns " (dataMineLib.isTable myTable:))
   (writeln "(if (dataMineLib.isTable myTable:) 1 0) returns " (if (dataMineLib.isTable myTable:) 1 0))
   
   (writeln _eol "***************************") 
   (writeln ">>rollbackRelation: Populate a relation with records and then read them back.") 
   (writeln ">>Note: Unique test records are created for each row.") 
    (setq startTime (getTickCount 0))
   (writeln "***************************")

	;; Create the table
   (setq cursor (dataMineLib.open myTable:))
   (writeln ">> Creating table of cursor type =" cursor.myCursorType)
   (loop for i from 0 until recCount do
      (setq dbRec (new Vector: 3 (append "John" i) "Sales Rep" (money (+ $50000 i))))
      (cursor.write i dbRec)
      (if printSW (writeln "Writing record [" i "]"))
      ) ;; end loop
   (setq cursor (dataMineLib.close cursor))
   (setq endTime (getTickCount startTime))
   (writeln ">>Populating a 10Mx3C relation = " (text (/ (* (/ endTime 3600) 10000000) recCount) "#,###.#0") " hours.")
   (writeln "***************************")

   ;; Delete each record randomly.
   (setq startTime (getTickCount 0))
	(writeln ">>Opening a table for random record deletion")
   (setq cursor (dataMineLib.open myTable:))
   (loop for i from 0 until recCount do  
      (setq j (integer (random cursor.recordCount)))
      (cursor.delete j)
      (if printSW (writeln "Deleting record [" j "]"))
      ) ;; end loop
   (dataMineLib.rollback dataMineLib.usingExtIndex)
;   (setq cursor (dataMineLib.close cursor rollback:)) Note: rollback: is no longer supported on individual tables!!!
   (setq endTime (getTickCount startTime))
   (writeln ">>Rollback of deleting a 10Mx3C relation = " (text (/ (* (/ endTime 3600) 10000000) recCount) "#,###.#0") " hours.")

   (dataMineLib.systemCheck dataMineLib.usingExtIndex)
   (writeln "***************************")

   ;; Read and check each record for validity.
   (setq startTime (getTickCount 0))
	(writeln ">>Opening a table for validation")
   (setq cursor (dataMineLib.open myTable:))
   (loop for i from (sub1 recCount) to 0 by -1 do  
      (setq dbRec (new Vector: 3 (append "John" i) "Sales Rep" (money (+ $50000 i))))
      (setq x (cursor.read i))
	  (setCdr x #void)
      (testIt  "failed record compare " dbRec x)
      (if printSW (writeln "Reading record [" i "]"))
      ) ;; end loop
   (setq cursor (dataMineLib.close cursor))
   (setq endTime (getTickCount startTime))
   (writeln ">>Validating a 10Mx3C relation = " (text (/ (* (/ endTime 3600) 10000000) recCount) "#,###.#0") " hours.")
   (writeln "***************************")
   (dataMineLib.systemCheck dataMineLib.usingExtIndex)
   (writeln ">>Finished with Rollback Relation Test")
   (writeln "")
   true) ;; end rollbackRelation






































;;**EXPORTKEY**:dataMineLib.selfTest.sortRelation
(defchild  dataMineLib.selfTest sortRelation(recCount printSW)
   ;; *******************************************************************
   ;; summary:  Populate a relation with records and then read them back.
   ;; Args:     recCount       The number of records to create.
   ;;           printSW        Switch to turn on/off debug printing.
   ;; Return:   true
   ;; *******************************************************************
   vars:(dbRec i j cursor startTime endTime scursor x y)

   (dataMineLib.usingForCreate dataMineLibSelfTest: "testSuite")

   (writeln _eol "***************************") 
	(writeln ">>Droping myTable")   
   (dataMineLib.dropTable myTable:)
   (writeln ">>Creating myTable")
   (dataMineLib.createTable myTable: Name: Title: Salary:)
   ;; Write each record to the table.
   (writeln _eol "***************************") 
   (writeln "sortRelation: Populate a relation with records and then sort them.") 
   (setq startTime (getTickCount 0))
   (writeln ">>Opening table myTable as disk cursor")
   (setq cursor (dataMineLib.open myTable:))
   (loop for i from 0 until recCount do
      (setq dbRec (new Vector: 3 (append "John" i) "Sales Rep" (money (+ $50000 i))))
      (cursor.write i dbRec)
      (if printSW (writeln "Writing record [" i "] " dbRec))
      ) ;; end loop
   (setq cursor (dataMineLib.close cursor))
   (setq endTime (getTickCount startTime))  
   (writeln ">>Populating a 10Mx3C relation = " (text (/ (* (/ endTime 3600) 10000000) recCount) "#,###.#0") " hours.")

   ;; Open an exclusive a disk cursor and sort it
	(writeln ">>Open a disk cursor on myTable")
   (setq cursor (dataMineLib.open myTable:))	
   ;; Sort the exclusive table cursor in descending order.
   (writeln ">>Sorting a 10Mx3C relation")
   (setq startTime (getTickCount 0))
   (cursor.sort (lambda(x y) (> x.Salary y.Salary)))
   (setq endTime (getTickCount startTime))
   (writeln ">>Sorting a 10Mx3C relation = " (text (/ (* (/ endTime 3600) 10000000) recCount) "#,###.#0") " hours.")

   ;; Read and check each sorted record for validity.
   (writeln ">>Validating the exclusive cursor after sort")
   (setq startTime (getTickCount 0))
   (loop for i from 0 until recCount do
      (setq j (- recCount 1 i))
      (setq dbRec (new Vector: 3 (append "John" j) "Sales Rep" (money (+ $50000 j))))
      (setq x (cursor.read i))
      (testIt  "failed record compare " dbRec x)
      (if printSW (writeln "Reading record [" i "] " x))
      ) ;; end loop
   (setq endTime (getTickCount startTime))
   (writeln ">>Validating a 10Mx3C relation = " (text (/ (* (/ endTime 3600) 10000000) recCount) "#,###.#0") " hours.")
	(setq cursor (dataMineLib.close cursor)	)


	(writeln ">>Droping myTable")   
   (dataMineLib.dropTable myTable:)
   (writeln ">>Creating myTable")
   (dataMineLib.createTable myTable: Name: Title: Salary:)
   ;; Write each record to the table.
   (writeln _eol "***************************") 
   (writeln "sortRelation: Populate a relation with records and then sort them.") 
   (setq startTime (getTickCount 0))
   (writeln ">>Opening table myTable as disk cursor")
   (setq cursor (dataMineLib.open myTable:))
   (loop for i from 0 until recCount do
      (setq dbRec (new Vector: 3 (append "John" i) "Sales Rep" (money (+ $50000 i))))
      (cursor.write i dbRec)
      (if printSW (writeln "Writing record [" i "] " dbRec))
      ) ;; end loop
   (setq cursor (dataMineLib.close cursor))
   (setq endTime (getTickCount startTime))  
   (writeln ">>Populating a 10Mx3C relation = " (text (/ (* (/ endTime 3600) 10000000) recCount) "#,###.#0") " hours.")


   ;; Open an exclusive and a static cursor the table simultaneously.
	(writeln ">>Open a static and disk cursor on myTable")
   (setq scursor (dataMineLib.open myTable: static:))
   (setq cursor (dataMineLib.open myTable:))
   ;; Sort the exclusive table cursor in descending order.
   (writeln ">>Sorting a 10Mx3C relation (on the disk cursor)")
   (setq startTime (getTickCount 0))
   (cursor.sort (lambda(x y) (> x.Salary y.Salary)))
   (setq endTime (getTickCount startTime))
   (writeln ">>Sorting a 10Mx3C relation = " (text (/ (* (/ endTime 3600) 10000000) recCount) "#,###.#0") " hours.")
   ;; Read and check each sorted record for validity.
   (writeln ">>Validating the exclusive cursor after sort")
   (setq startTime (getTickCount 0))
   (loop for i from 0 until recCount do
      (setq j (- recCount 1 i))
      (setq dbRec (new Vector: 3 (append "John" j) "Sales Rep" (money (+ $50000 j))))
      (setq x (cursor.read i))
      (testIt  "failed record compare " dbRec x)
      (if printSW (writeln "Reading record [" i "] " x))
      ) ;; end loop
   (setq endTime (getTickCount startTime))
   (writeln ">>Validating a 10Mx3C relation = " (text (/ (* (/ endTime 3600) 10000000) recCount) "#,###.#0") " hours.")
   ;; Read and check each record for validity (from the unsorted static cursor).
   (writeln ">>Validating the static cursor after sort")
   (setq startTime (getTickCount 0))
   (loop for i from 0 until recCount do
      (setq dbRec (new Vector: 3 (append "John" i) "Sales Rep" (money (+ $50000 i))))
      (setq x (scursor.read i))
      (testIt  "failed record compare " dbRec x)
      (if printSW (writeln "Reading record [" i "] " x))
      ) ;; end loop
   (setq endTime (getTickCount startTime))
   (writeln ">>Validating a 10Mx3C relation = " (text (/ (* (/ endTime 3600) 10000000) recCount) "#,###.#0") " hours.")
   ;; Sort the static table cursor in descending order.
   (writeln ">>Sorting a 10Mx3C relation (on the static cursor)")
   (setq startTime (getTickCount 0))
   (scursor.sort (lambda(x y) (> x.Salary y.Salary)))
   (setq endTime (getTickCount startTime))
   (writeln ">>Sorting a 10Mx3C relation = " (text (/ (* (/ endTime 3600) 10000000) recCount) "#,###.#0") " hours.")
   ;; Read and check each sorted record for validity.
   (writeln ">>Validating the static cursor after sort")
   (setq startTime (getTickCount 0))
   (loop for i from 0 until recCount do
      (setq j (- recCount 1 i))
      (setq dbRec (new Vector: 3 (append "John" j) "Sales Rep" (money (+ $50000 j))))
      (setq x (scursor.read i))
      (testIt  "failed record compare " dbRec x)
      (if printSW (writeln "Reading record [" i "] " x))
      ) ;; end loop
   (setq endTime (getTickCount startTime))
   (writeln ">>Validating a 10Mx3C relation = " (text (/ (* (/ endTime 3600) 10000000) recCount) "#,###.#0") " hours.")
   ;; Read and check each record for validity (from the restored static cursor).
   (writeln ">>Validating the exclusive cursor after restore")
   (cursor.restore)
   (setq startTime (getTickCount 0))
   (loop for i from 0 until recCount do
      (setq dbRec (new Vector: 3 (append "John" i) "Sales Rep" (money (+ $50000 i))))
      (setq x (cursor.read i))
      (testIt  "failed record compare " dbRec x)
      (if printSW (writeln "Reading record [" i "] " x))
      ) ;; end loop
   (setq endTime (getTickCount startTime))
   (writeln ">>Validating a 10Mx3C relation = " (text (/ (* (/ endTime 3600) 10000000) recCount) "#,###.#0") " hours.")
   ;; Close both cursors on the table.
   (writeln ">>Closing disk cursor")
   (setq cursor (dataMineLib.close cursor))
   (writeln ">>Closing static cursor")
   (setq scursor (dataMineLib.close scursor))
   (dataMineLib.systemCheck dataMineLib.usingExtIndex)        
   true) ;; end sortRelation






































;;**EXPORTKEY**:dataMineLib.selfTest.speedTest
(defchild  dataMineLib.selfTest speedTest(recCount printSW)
	;; *******************************************************************
	;; summary:  Perform basic performance testing for memory and disk cursors
	;; Args:     recCount       The number of records to use in testing.
	;;           printSW        Switch to turn on/off debug printing.
	;; Return:   true
	;; ******************************************************************* 
	pvars:(rawIOTest diskCursorTest memoryCursorTest myRepos dbRec)
	vars:( i j cursor startTime endTime)

  
    (defun rawIOTest(recCount printSW)
  		vars:(myRepos v i dbRec startTime)  
		(setq myRepos dataMineLib.usingRepos)

		(setq v (new Vector: number: recCount))

		(setq startTime (getTickCount 0))

		(beginTransaction myRepos)

		(loop for i from 0 until recCount do
		  (setq dbRec (new Vector: 3 (append "John" i) "Sales Rep" (money (+ $50000 i))))
			(setNumVector v i (setq myRepos[frame: #void] dbRec))		  
		); i
		(commitTransaction myRepos)
		(writeln ">>rawWrites:" (getTickCount startTime) " Trans open:" (inspect myRepos open:))

		(setq startTime (getTickCount 0))
		(beginTransaction myRepos)
		(loop for i from 0 until recCount do
			(setq dbRec myRepos[frame: (refNumVector v i)])		  
		); i
		(commitTransaction myRepos)
		(writeln ">>rawReads:" (getTickCount startTime) " Trans open:" (inspect myRepos open:))
		
		(setq startTime (getTickCount 0))
		(beginTransaction myRepos)
		(loop for i from 0 until recCount do
			(setq myRepos[frame: (refNumVector v i)] #void)		  
		); i
		(commitTransaction myRepos)
		(writeln ">>rawDeletes:" (getTickCount startTime) " Trans open:" (inspect myRepos open:))
   
   	true)

	(defun diskCursorTest(recCount printSW)
		vars:( i j dbRec startTime)
		;Test write speed

	
		(setq startTime (getTickCount 0))
		(setq cursor (dataMineLib.open myTable:))
		(writeln ">>Open   :" (getTickCount startTime)  " Trans open:" (inspect myRepos open:))
		
		(setq startTime (getTickCount 0))
		(loop for i from 0 until recCount do
		  (setq dbRec (new Vector: 3 (append "John" i) "Sales Rep" (money (+ $50000 i))))
		  (cursor.write i dbRec)
		); i
		(writeln ">>Appends:" (getTickCount startTime) " Trans open:" (inspect myRepos open:))
		(writeln ">>wrtTime0=" cursor.wrtTime0)
	
		(setq startTime (getTickCount 0))
		(setq cursor (dataMineLib.close cursor))
		(writeln ">>Close  :" (getTickCount startTime) " Trans open:" (inspect myRepos open:))
	
	;Test read speed
		(setq startTime (getTickCount 0))
		(setq cursor (dataMineLib.open myTable:))
		(writeln ">>Open   :" (getTickCount startTime) " Trans open:" (inspect myRepos open:))
		
		(setq startTime (getTickCount 0))
		(loop for i from 0 until recCount do
		  (setq dbRec (cursor.read i))
		); i
		(writeln ">>Reads  :" (getTickCount startTime) " Trans open:" (inspect myRepos open:))
	
		(setq startTime (getTickCount 0))
		(setq cursor (dataMineLib.close cursor))
		(writeln ">>Close  :" (getTickCount startTime) " Trans open:" (inspect myRepos open:))


	;Test delete speed
		(setq startTime (getTickCount 0))
		(setq cursor (dataMineLib.open myTable:))
		(writeln ">>Open   :" (getTickCount startTime) " Trans open:" (inspect myRepos open:))
		
		(setq startTime (getTickCount 0))
		(loop for i from 0 until recCount do
			(cursor.delete 0)
		); i
		(writeln ">>Deletes:" (getTickCount startTime) " Trans open:" (inspect myRepos open:))
	
		(setq startTime (getTickCount 0))
		(setq cursor (dataMineLib.close cursor))
		(writeln ">>Close  :" (getTickCount startTime)" Trans open:" (inspect myRepos open:))
	
	;Test insert speed
		(setq startTime (getTickCount 0))
		(setq cursor (dataMineLib.open myTable:))
		(writeln ">>Open   :" (getTickCount startTime) " Trans open:" (inspect myRepos open:))
		
		(setq startTime (getTickCount 0))
		(loop for i from 0 until recCount do
		  (setq dbRec (new Vector: 3 (append "John" i) "Sales Rep" (money (+ $50000 i))))
		  (cursor.insert 0 dbRec)
		); i
		(writeln ">>Inserts:" (getTickCount startTime) " Trans open:" (inspect myRepos open:))


		(setq startTime (getTickCount 0))
		(setq cursor (dataMineLib.close cursor))
		(writeln ">>Close  :" (getTickCount startTime) " Trans open:" (inspect myRepos open:))

	true); end of disk cursor test

	(defun memoryCursorTest (recCount printSW)
		vars:( i j dbRec startTime)
		(writeln "Memory Cursor Performance Tests")
	;Test read speed
		(setq startTime (getTickCount 0))
		(setq cursor (dataMineLib.open myTable: memory:))
		(writeln ">>Open   :" (getTickCount startTime) " Trans open:" (inspect myRepos open:))
		
		(setq startTime (getTickCount 0))
		(loop for i from 0 until recCount do
		  (setq dbRec (cursor.read i))
		); i
		(writeln ">>Reads  :" (getTickCount startTime) " Trans open:" (inspect myRepos open:))
	
		(setq startTime (getTickCount 0))
		(cursor.save)
		(writeln ">>save  :" (getTickCount startTime) " Trans open:" (inspect myRepos open:))

		(setq startTime (getTickCount 0))
		(setq cursor (dataMineLib.close cursor))
		(writeln ">>Close  :" (getTickCount startTime) " Trans open:" (inspect myRepos open:))

	true); end of memory cursor test

;;****** MAIN BODY ***************

   (dataMineLib.usingForCreate dataMineLibSelfTest: "testSuite")

	(setq myRepos dataMineLib.usingRepos)

	(writeln _eol "***************************") 
	(writeln ">>speedTest: raw IO performance testing.") 
	(dataMineLib.clearCursors) 
	(writeln ">>clearCursors" 	" Trans open:" (inspect myRepos open:))
	(rawIOTest recCount printSW)

	(writeln _eol "***************************") 
	(writeln ">>speedTest: basic performance testing.") 
	(dataMineLib.clearCursors) 
	(writeln ">>clearCursors" 	" Trans open:" (inspect myRepos open:))


	(writeln "Disk Cursor Performance Tests with no indices")

	(setq startTime (getTickCount 0))
	(dataMineLib.dropTable myTable:)
	(writeln ">>DropTable :" (getTickCount startTime) 	" Trans open:" (inspect myRepos open:))

	(setq startTime (getTickCount 0))
	(dataMineLib.createTable myTable: Name: Title: Salary:)
	(writeln ">>Create :" (getTickCount startTime) 	" Trans open:" (inspect myRepos open:))

	(diskCursorTest recCount printSW) 
	(memoryCursorTest recCount printSW)
	

	(writeln "Disk Cursor Performance Tests with with indices")

	(setq startTime (getTickCount 0))
	(dataMineLib.dropTable myTable:)
	(writeln ">>DropTable :" (getTickCount startTime) 	" Trans open:" (inspect myRepos open:))

	(setq startTime (getTickCount 0))
	(dataMineLib.createTable myTable: Name: Title: Salary:)
	(writeln ">>Create :" (getTickCount startTime) 	" Trans open:" (inspect myRepos open:))

	(setq startTime (getTickCount 0))
	(setq cursor (dataMineLib.open myTable:))
	(writeln ">>Open " (getTickCount startTime) " Trans open:" (inspect myRepos open:)) 

	(setq startTime (getTickCount 0))
	(cursor.createIndex Salary: #{Salary: A:})
	(writeln ">>Create Index :" (getTickCount startTime) " Trans open:" (inspect myRepos open:))

	(setq startTime (getTickCount 0))
	
	(setq cursor (dataMineLib.close cursor))
	(writeln ">>Close :" (getTickCount startTime) " Trans open:" (inspect myRepos open:))

 	(diskCursorTest recCount printSW) 

	(memoryCursorTest recCount printSW)


			
true)














;;**EXPORTKEY**:dataMineLib.selfTest.testIt
(defchild  dataMineLib.selfTest testIt(msgTxt result value)
;;************************************************************************
;;  Test Script diagnostic and utility functions
;;************************************************************************ 
;	(writeln "****************************")
   (if (<> result value) 
   		(begin
		(writeln (refAttributes result))
		(writeln result)
		(writeln (refAttributes value))
		(writeln value)
	   	(error "datamine" scriptName " *FAILURE* " msgTxt))
	   	end)
   true) ;; end testIt



















;;**EXPORTKEY**:dataMineLib.selfTest.testMetaTable
(defchild dataMineLib.selfTest  testMetaTable(numTables printSW)
;; *************************************************************
;; Summary: Lambda to test meta table
;; Args:
;;	numTables	- number of tables to merge
;;	printSW		- debug print switch
;; Returns 	true
;; *************************************************************
	vars:(	t i 
			startTime
			endTime
			cursor
			testTables
			rec
			record
			)
   pvars:(
   			numRecs
   			numTables 	
   			testAccess
   			testWrites
			myDM
         )

	(defun testWrites(testTables)
		vars:(	t i 
				cursor
				cursor2
				cursor3
				rec
				rec2
				record
				)

		(writeln "Testing regular writes to meta table")

		(loop for t from 0 until (length testTables)		
			(setq cursor2 (myDM.open testTables[t 1] memory:)); open the first member table in memory
			(loop for i from 0 until cursor2.recordCount 
				(setq rec (cursor2.read i))
			);i

			(setq cursor (myDM.open allTables:))
	
			(loop for i from 0 until cursor.recordCount do
				(setq record (cursor.read i))
				(if (= record.TableKey testTables[t 0]) 
					(begin
					(setq record.Price (+ record.Price 1) )
					(setq record.Volume (+ record.Volume 1))
					(cursor.write i record)
					(setq rec (cursor.read i))
					(if (<> rec record) (writeln "write failed"))
					end)
				);if
			);i
			(setq cursor (myDM.close cursor))	

			(setq cursor3 (myDM.open testTables[t 1])); open the first member table again!
			;Make sure the member table was updated properly
			(loop for i from 0 until cursor2.recordCount do
				(setq rec (cursor2.read i))
				(setq rec2 (cursor3.read i))
				(if (or (<> (+ rec.Price 1) rec2.Price)
						(<> (+ rec.Volume 1) rec2.Volume))
					(begin
					(writeln "regular table writes to meta table failed!")
					(setq cursor2 (myDM.close cursor2))
					(setq cursor3 (myDM.close cursor3))
					(goto EXIT1:)
					end)
				);if
			);i  
			EXIT1::
			(setq cursor2 (myDM.close cursor2))
			(setq cursor3 (myDM.close cursor3))
		);t
		(writeln "finished test regular writes on meta table")
	true)

	(defun testAccess(testTables)
		vars:(	t i j 
				cursor
				cursor2
				rec
				rec2
				)

		(setq numTables (length testTables))

		(setq cursor (myDM.open allTables:))
		(writeln "Testing getRecordBySJKey")
		(loop for i from 0 until cursor.recordCount do
			(setq rec (cursor.read i))
			(setq rec2 (cursor.getRecordBySJKey rec.TableKey rec.ID))
			(if (<> rec rec2) (begin (writeln "records returned by getRecordBySJKey not correct") (goto EXIT1:)))
		);i
		EXIT1::
	
		(writeln "Testing getRecordRowBySJKey")
		(loop for i from 0 until cursor.recordCount do
			(setq rec (cursor.read i))
			(setq recNum (cursor.getRecordRowBySJKey rec.TableKey rec.ID))
			(if (<> recNum i) (begin (writeln "record number returned by getRecordRowBySJKey not correct") (goto EXIT2:)))
		);i
		EXIT2::
	
		(writeln "Testing indexViewBySJTableKey")
		(loop for t from 0 until numTables do
			(cursor.indexViewBySJTableKey testTables[t 0])
			(if (<> numRecs cursor.recordCount)
				(begin (writeln "indexViewBySJTableKey failed as view does not contain expected number of records") (goto EXIT3:)))
	
			(loop for i from 0 until cursor.recordCount do
				(setq rec (cursor.read i))
				(if (<> rec.TableKey testTables[t 0]) 
					(begin (writeln "indexView failed as view contains excluded record") (goto EXIT3:)))
			);i
		);i
		EXIT3::
	
		(writeln "Testing indexViewBySJRowKey")
		(loop for i from 0 until numRecs do
			(cursor.indexViewBySJRowKey i)
			(if (<> cursor.recordCount numTables)
				(begin 
				(writeln "indexViewBySJRowKey failed as view does not contain expected number of records i=" i " recordCount=" cursor.recordCount " numTables=" numTables)
				(loop for j from 0 until cursor.recordCount 
				 (setq rec (cursor.read j))
				 (writeln rec)
				) 
				(goto EXIT4:)))
		);i
		EXIT4::
	
		(writeln "Testing createMemoryCursorFromView")
		(loop for t from 0 until numTables do
			(cursor.indexViewBySJTableKey testTables[t 0])
			(setq cursor2 (cursor.createMemoryCursorFromView))		
			(if (<> numRecs cursor2.recordCount)
				(begin (writeln "createMemoryCursorFromView failed as view does not contain expected number of records") (goto EXIT5:)))
	
			(loop for i from 0 until cursor2.recordCount do
				(setq rec (cursor.read i))
				(setq rec2 (cursor2.read i))
				(if (<> rec rec2)
					(begin 
					(writeln "createMemoryCursorFromView failed as records do not match in memory cursor.") 
					(setq cursor2 (myDM.close cursor2))
					(goto EXIT5:)))
			);i
			(setq cursor2 (myDM.close cursor2))
		);t
		EXIT5::
	
		(setq cursor (myDM.close cursor))
	true); testAccess
	
;; *************************************************************
;; Mainline code for this Lambda
;;
;; *************************************************************  
	(writeln "********** Basic MetaTable Test ******************")
	(setq testTables (new Directory:))

	(setq numRecs 10)
	(setq myDM dataMineLib)
	(myDM.usingForCreate dataMineLibSelfTest: "metacursortest")

	(myDM.dropTable allTables:)
(myDM.systemCheck)

	;create test tables 
	(loop for t from 0 until numTables do
		(setq tableName (symbol (append "metaSource" (integer t))))
		(setq tableNameKey (symbol (append tableName "key")))
		(myDM.dropTable tableName)
		(myDM.createTable tableName TableKey: ID: Price: Volume:) 
		(setq cursor (myDM.open tableName))

		(loop for i from 0 until numRecs do
			(setq record (cursor.getNewRecord))
			(setq record.TableKey tableNameKey)
			(setq record.ID i)
			(setq record.Price (random 50)) 
			(setq record.Volume (integer (random 10000)))
			(cursor.write i record)  
		);i
		(setq cursor (myDM.close cursor))
		(setq testTables[tableNameKey] tableName )
	);t
(myDM.systemCheck)

	;create tables in second extent
	(myDM.usingForCreate dataMineLibSelfTest2: "metacursortest")
	(setq numTables (integer (* numTables 2)))
	(loop for t from (integer (/ numTables 2)) until numTables do
		(setq tableName (symbol (append "metaSource" (integer t))))
		(setq tableNameKey (symbol (append tableName "key")))
		(myDM.dropTable tableName)
		(myDM.createTable tableName TableKey: ID: Price: Volume:) 
		(setq cursor (myDM.open tableName))

		(loop for i from 0 until numRecs do
			(setq record (cursor.getNewRecord))
			(setq record.TableKey tableNameKey)
			(setq record.ID i)
			(setq record.Price (random 50)) 
			(setq record.Volume (integer (random 10000)))
			(cursor.write i record)  
		);i
		(setq cursor (myDM.close cursor))
		(setq testTables[tableNameKey] tableName )
	);t


(myDM.systemCheck)

	(myDM.usingForCreate dataMineLibSelfTest: "metacursortest")
	(myDM.createMetaTable allTables: testTables ID: 15)
(myDM.systemCheck)
	(testAccess testTables)
(myDM.systemCheck)
    (testWrites testTables)
(myDM.systemCheck)
    (testAccess testTables)
(myDM.systemCheck)
	(writeln "Test of meta table complete")
	(writeln "*******************************************")




 
  ) ;; End of codeBaseLambda





















;;**EXPORTKEY**:dataMineLib.selfTest.testProject
(defchild dataMineLib.selfTest  testProject(recCount printSW)
   ;; *******************************************************************
   ;; summary:  Test a small project.
   ;; Args:     recCount       The number of records to create.
   ;;           printSW        Switch to turn on/off debug printing.
   ;; Return:   true
   ;; *******************************************************************
   (dataMineLib.usingForCreate dataMineLibSelfTest: "testSuite")
   (makeProject recCount printSW) 
   (dataMineLib.miner.openProject Hedge:) 
   (dataMineLib.miner.testRun) 
   (dataMineLib.systemCheck dataMineLib.usingExtIndex)
   true) ;; end testProject






































;;**EXPORTKEY**:dataMineLib.setTableDatabaseName
(defchild dataMineLib:setTableDatabaseName(tableName databaseName)
   ;; *******************************************************************
   ;; summary:  Sets the logical database name of the specified table 
   ;;           or view.
   ;; Args:     tableName     The table or view whose logical database name is to be set.
   ;;           databaseName  The logical database name of the table or view.
   ;; Return:   true
   ;; Notes: tm This routine was modified to ensure that it checked for 
   ;; any open cursors on the table before changing the databaseName.
   ;; *******************************************************************
	vars:(tableSchema extentIndex dbIndex compositeIndex i)
	(STARTTIME)
	;; Search of the table or view in the reference datamine schema.   
	(if (<> openTables[tableName] #void) 
			(error "dataMineLib:setTableDataBaseName: Could not set database name because " tableName " has open cursors"))
	
	(setq extentIndex (getTableExtent tableName))
	(if (<> extentIndex false)
		(begin
		(__beginTrans extentIndex)
		(setq tableSchema dmExt[extentIndex].repos[tableName])
		(setq tableSchema.myDatabaseName databaseName)
		(setq dmExt[extentIndex].repos[tableName] tableSchema)
		(__commitTrans extentIndex)
		(setq i (member tableName dmExt[extentIndex].extentTableNames))
		(setq dmExt[extentIndex].extentDatabaseNames[i] databaseName)
		;Make sure database name is in databaseNames
		(setq dbIndex (member databaseName databaseNames))
		(if (not (isNumber dbIndex)) (begin
				(setq databaseNames[(length databaseNames)] databaseName)
				(setq dbIndex (member databaseName databaseNames))
				end))
		(setq compositeIndex (+ extentIndex (/ dbIndex 65536)))
		(setq dataMineTables[tableName] compositeIndex)
		(SETTIME)
		(return true)
		)) ;; end if
	(error (append "dataMineLib: Could not find table named: " tableName))) ;; end of setTableDatabaseName





























































































;;**EXPORTKEY**:dataMineLib.sortTable
(defchild dataMineLib:sortTable (theTable theKeys ...)
	;; *******************************************************************
	;; Summary:  Sort a table 
	;;
	;; Args:	theTable     	The name of the table to sort
	;;          theKeys      	Structure of column sort specifications ie: #{column: A:}
	;;							ex: #{ID: A: Cash: D:}
	;;			options         
	;;				noSave:					Do not save the result of the sort
	;;				noSort:					Do not sort table by keys (I know this makes the Lambda name silly!)
	;;				returnVector:			Return a copy of the sorted bckVector instead of True
	;;				returnVectorAndKeys:	return a vector containing column values and recordkey 
	;;										eg: #(columnvalue1 ... recordkey)
	;; Return:   true  or copy of sorted bckVector
	;; Test: (dataMineLib)(dataMineLib.sortTable baseData: #{FinancialStrength: A: Cash: A: })
	;; History
	;; First Version	TMay	Sept 13, 00	Part of adding metaTables to dataMineLib
	;; *******************************************************************
	vars:( 	myBckVector	
			myCursorTable
			myExtentIndex
			myEndTime
			myKeysLambda
			myLine
			myNumKeys
			myNumArgs
			myNoSaveOption
			myNoSortOption
			myOption
			myRecordCount
			myRecord
			myRepos
			myResult
			myReturnVectorOption
			myReturnVectorAndKeysOption
			mySortLambda
			myStartTime
			myTableSchema
			;-----
			myMaxSize
			myMinSize
			MyAvgSize
			myTotSize
			myRecSize
			i k j
	     )
	(STARTTIME)
	(setq myExtentIndex (getTableExtent theTable))
	(if (= myExtentIndex false) (error (append "dataMineLib:sortTable -- " theTable " table does not exist")))
	(setq myRepos dmExt[myExtentIndex].repos); dmExt is created and maintained by dataMineLib
	(setq myTableSchema myRepos[(symbol theTable)]) ; grab table schema object using repository directory
	(setq myBckVector myRepos[frame: myTableSchema.bckVectorKey]) ; grab bckVector using direct repository access by key
	(if (= myBckVector #void) (error (append "dataMineLib:sortTable -- " theTable " table has no bckVector")))
;	(if (<= (length theKeys) 0) (error (append "dataMineLib:sortTable -- " theTable " missing sort specification")))
	;; Validate that sort specifications structure against table schema
	(loop for i from 0 until (length theKeys) do
		(if (= (isMember theKeys[i 0] myTableSchema.validFields) false) ;; theKeys[i 0] is the columnname
	     	(error (append "dataMineLib:sortTable -- sort column " theKeys[i 0] " not found in RecordStructure of table " theTable))
	  	)
	  	(if (= (isMember theKeys[i 1] #(A: D:)) false) ;; theKeys[i 1] is the sort A: or D: attribute
	  		(error (append "dataMineLib:sortTable -- sort attribute not A: or D: in sort column " theKeys[i 0] " in table " theTable))
	  	)       
	);end of loop
 	;; Collect and Validate options
 	(setq myNoSaveOption false)	; set default
 	(setq myReturnVectorOption false) ; set default
 	(setq myNoSortOption false); set default
 	(setq myReturnVectorAndKeysOption false) ; set default
 	(setq myNumArgs (argCount))
	(loop for i from 2 until myNumArgs do
		(setq myOption (argFetch i))
	 	(cond
	 		((= myOption noSave:) (setq myNoSaveOption true))
	 		((= myOption returnVector:) (setq myReturnVectorOption true))
	 		((= myOption returnVectorAndKeys:) (setq myReturnVectorAndKeysOption true))
	 		((= myOption noSort:) (setq myNoSortOption true))
	 		else (error (append "dataMineLib:sortTable -- bad option " myOption " passed for table " theTable))
	 	)
	)
 
	(if (= (length theKeys) 0) (setq myNoSortOption true)) ;overide option if necessary

	;; Make sure the table being sorted is not opened as a disk or static cursor 
	(if (and (not myNoSaveOption) (<> openTables #void)) do 
		(begin
		(setq myCursorTable openTables[theTable]) ; openTables defined in dataMineLib
		(if (<> myCursorTable #void) ; an open cursor on the table exists so test for non memory cursor use
        	(loop for i from 0 until (length cursorTable.cursorList) do 
               (if (<> cursorTable.cursorList[i].myCursorType memory:) ;; 
                   (error (append "dataMineLib:sortTable -- the table" theTable " has an exclusive disk: or static: cursor open.")))
            ); end loop
        ); end if
   		end)
    ); end if
 	
	;; 1. Create a vector of vectors to hold sortable rows
	(setq mySortVector (new Vector: (length myBckVector)))
	(setq myNumKeys (length theKeys))
	; Create an assignment Lambda for use inside record iteration and assignment loop
	(setq myKeysLambda (append "(lambda(x y) (new Vector: " (+ myNumKeys 1))) ;)) balance line
	(loop for i from 0 until myNumKeys 
		(setq myKeysLambda (append myKeysLambda " x[" (member theKeys[i 0] myTableSchema.validFields) "]"))
	); end of loop
	(setq myKeysLambda (append myKeysLambda " y ))"))

;(writeln "myKeysLambda=" myKeysLambda)

	(setq myKeysLambda (eval myKeysLambda))
	(setq myRecordCount (length myBckVector))


	(setq myTotSize 0)
	(setq myAvgSize 0)
	(setq myMinSize 0)
	(setq myMaxSize 0)
	(setq myRecSize 0)
	(setq myStartTime (getTickCount 0))
	(loop for i from 0 until myRecordCount do
		(setq myRecord myRepos[frame: myBckVector[i]]); load the tables full record from disk
;		(setq myRecSize (sizeof myRecord))
;		(if (> myRecSize myMaxSize) (setq myMaxSize myRecSize))
;		(if (= myMinSize 0) (setq myMinSize myRecSize))
;		(if (< myRecSize myMinSize) (setq myMinSize myRecSize))
;		(setq myTotSize (+ myTotSize myRecSize))
		(setq mySortVector[i] (myKeysLambda myRecord myBckVector[i]))
	); end of loop
	(setq myEndTime (getTickCount myStartTime))

;(writeln "line[103] Read Time for " theTable " was " myEndTime ) ;***debug insert***
;(writeln "minSize=" myMinSize " maxSize=" myMaxSize " totSize=" myTotSize " avgSize=" (/ myTotSize myRecordCount))

	;; 2. Create sort Lambda
	(if (not myNoSortOption)
		(begin
	    (if (= myNumKeys 1)
	    	(begin
	   		(if (= theKeys[0 1] A:)
	      		(setq mySortLambda (lambda(x y) (< x[0] y[0])))
	      		(setq mySortLambda (lambda(x y) (> x[0] y[0])))
	   		);end if
	     	end)
	  	else ;; Need a multi level sort
			(begin
			;; Create expression of the form shown below ex: #{column1: A: column2: A: column3: D:}
			;(lambda (x y)
			;	(cond 
			;	((< x[0] y[0]) true) 
			;	((and (= x[0] y[0]) (< x[1] y[1])) true)
			;	((and (= x[0] y[0]) (= x[1] y[1]) (> x[2] y[2])) true)
			;	else false)
			;	)
			(setq mySortLambda "")
	     	(setq mySortLambda (append mySortLambda "(lambda(x y) (cond ((" (if (= theKeys[0 1] A:) "<" ">") " x[0] y[0]) true) " ))
	     	(loop for i from 1 until myNumKeys do                                                      
				(setq myStr " ((and") ; ))
				(loop for j from 0 until i do ; create equality conditions for previous keys
					(setq myStr (append myStr "(= x[" j "] y[" j "]) ")) ;
				)
	       		(setq mySortLambda (append mySortLambda myStr "(" (if (= theKeys[i 1] A:) "<" ">") " x[" i "] y[" i "])) true)"))
		        ); end loop 
		  	(setq mySortLambda (append mySortLambda " else false))"))
	
	;		(writeln mySortLambda)
			(setq mySortLambda (eval mySortLambda))
	   		end)
	 	); end if
		end)
	);if
	
	;; 3. Sort vector of vectors
	(if (= myNoSortOption false)
		(begin 
;		(writeln "sorting table " theTable)
		(^sort mySortVector mySortLambda); sort the vector
		end)
  	);if
  	
;	(writeln "aftersort")
;	(loop for i from 0 until (length mySortVector) do
;		(writeln mySortVector[i])
;	)

    ;; 4. Create new bckVector if save or return of vector required
    (if (or (not myNoSaveOption) myReturnVectorOption)
    	(begin 
	    (setq myBckVector (new Vector: number: myRecordCount))
	    (loop for i from 0 until myRecordCount do
	     	(setq myBckVector[i] mySortVector[i][myNumKeys]) ;the record key is at the end of the record
	    )
		end)
	); end if	    
 	;; 5. Update table unless noSave option was passed
	(if (not myNoSaveOption) 
		(begin
		(__beginTrans myExtentIndex)
		(setq myTableSchema.bckVectorKey (setq myRepos[frame: myTableSchema.bckVectorKey] myBckVector))
		(setq myRepos[(symbol theTable)] myTableSchema)
		(__commitTrans myExtentIndex)
		end)
	); end if

	;;;;;;;;;;;;;;;;;;;;;;;;;
	;;;; NOTE: Not saving the last sort etc. in the shcema yet!
	;;;;;;;;;;;;;;;;;;;;;;;;; 	
 	
 	;; 6. Set correct return based on options 
 	(setq myResult
 		(cond
 		(myReturnVectorOption myBckVector)
 		(myReturnVectorAndKeysOption mySortVector)
 		(true true)
 		)
 	)
	(SETTIME)
	myResult) ;; end of sortTable
	
	
	
	































;;**EXPORTKEY**:dataMineLib.systemCheck
(defriend dataMineLib:systemCheck(...)
   ;; *******************************************************************
   ;; summary:  Performs a systems check of the data mine.
   ;;
   ;; Args:     none
   ;;
   ;; Return:   true
   ;; *******************************************************************
	vars:(i ss recordVector tableSchema extentIndex extentCount schemaIndex schemaCount numArgs arg)
	;; *******************************************************************
	;; Child Lambda definitions
	;; *******************************************************************
	;; Inspect a generic repository
	(defun inspectRepo(repo) 
		vars:(i ss)
		(inspect repo check:)
		(loop for i from 0 until (length repo) do
			(inspect repo check: repo[i 0])
		) ; end repo if
		true) ;; end inspectRepo
	
	;; Inspect a vector of table records
	(defun inspectRecords(repo recordVector bckVector) 
		vars:(i ss)
		(inspect repo check:)
		(if (> recordVector[0] (length bckVector))
			(loop for i from 0 until (length recordVector) do
				(inspect repo frameCheck: recordVector[i])
			) 
			(loop for i from 0 until (length recordVector) do
				(inspect repo frameCheck: bckVector[recordVector[i]])
			) 
		);if
		true) ;; end inspectRecords
	
	(STARTTIME)
	;; Cannot perform a data mine system check if there are open cursors.
	(if (> (length openTables) 0) 
	   (error "dataMineLib: Cannot perform a data mine system check if there are open cursors"))

	;; Inspect the repository extents
	(setq extentCount (length dmExt))  
	
	(setq numArgs (argCount))
	(if (= numArgs 1)
		(begin
		(setq extentIndex (argFetch 0))
		(writeln dmExt[extentIndex].name " transCount=" dmExt[extentIndex].transCount)
		(inspectRepo dmExt[extentIndex].repos)    
		(setq schemaCount (length dmExt[extentIndex].repos))
		
		(loop for schemaIndex from 0 until schemaCount do
			(setq tableSchema dmExt[extentIndex].repos[schemaIndex 1]) 
			
			(inspectRecords dmExt[extentIndex].repos dmExt[extentIndex].repos[frame: tableSchema.rowVectorKey] 
			dmExt[extentIndex].repos[frame: tableSchema.bckVectorKey])
		); schemaIndex
	  	end)
	else		
		(loop for extentIndex from 0 until extentCount do
			(writeln dmExt[extentIndex].name " transCount=" dmExt[extentIndex].transCount)
			(inspectRepo dmExt[extentIndex].repos)    
			(setq schemaCount (length dmExt[extentIndex].repos))
			
			(loop for schemaIndex from 0 until schemaCount do
				(setq tableSchema dmExt[extentIndex].repos[schemaIndex 1]) 
				
				(inspectRecords dmExt[extentIndex].repos dmExt[extentIndex].repos[frame: tableSchema.rowVectorKey] 
				dmExt[extentIndex].repos[frame: tableSchema.bckVectorKey])
			); schemaIndex
		); extentIndex
	);if
		  
	;(writeln "systemcheck finished")      
	(SETTIME)
	true) ;; end of systemsCheck















;;**EXPORTKEY**:dataMineLib.tempTable
(deforphan dataMineLib:tempTable(tableName colNames)
;; *******************************************************************
;; Summary:  The dataMineLib table cursor which manages a scratchpad
;;           in memory tables which are unassociated with any data mine
;;           tables. This table cursor is built on principles similar 
;;           to relational cursors, but loads a complete copy of the 
;;           table in memory.
;; Notes:    Each element in the table is called a record.
;;           The index of a table record is called its row (rows begin with zero).
;; Depends:  javaScript
;; args:     tableName      The name for this new in memory table.
;;           colNames       The column name vector for this new in memory table.
;; Return:   self           This newly initialized table cursor.
;; *******************************************************************
    ;; Persistent Variables
    pvars:(;; Public variables
           fileID              ;; The file id used during import and export.
           myCursorName        ;; The unique name of this investor colony table cursor.
           myCursorNotes       ;; The notes Dictionary of this investor colony table cursor.
           myCursorScore       ;; The score of this investor colony table cursor.
           myCursorType        ;; The type of this investor colony table cursor.
           myCursorTable       ;; The investor colony table underlying this cursor.
           myDataMine          ;; The investor colony owner of this cursor.
           myIndex             ;; The schema repository for investor colony tables.
           myParent            ;; This parent cursor Lambda.
           myTableCursor       ;; The cursor for the table of this view (views only).
           showLimit           ;; The maximum number of records to display with the show function.
           bckVectorKey        ;; The repository key of the backup record view vector (bckVector).
           colCount            ;; The number of columns in the table.
           colVector           ;; The table's vector of column names.
           myMemoPad           ;; The table's memo pad (always a Dictionary).
           myObjectName        ;; The name of the investor colony table or view object (#void if cursor is inactive).
           recordCount         ;; The table's current total record count.
           recordStructure     ;; The table's record structure.
           validFields         ;; The table's valid fields and types (necessary for query compilation).
           rowVector           ;; The current record view vector.
           bckVector           ;; The backup record view vector.
           viewDirectory       ;; The the Directory of saved table record view vectors.
           UPDIamDirty         ;; Update has occurred switch.
           ;; Public Child Lambdas
           average             ;; Averages a lambda value on each record of a table.
           averageForAll       ;; Averages a lambda value on ALL records of a table.
           close               ;; Terminate an update transaction on the table object.
           delete              ;; Delete the specified row from the table object.
           delimitedCells      ;; Returns a block of cells as a tab delimited string.
           deviation           ;; Returns the standard deviation of a lambda value on each record of a table.
           drop                ;; Drop all records from a table object.
           dropView            ;; Drops the specified record view.
           exportTab           ;; Exports ascii tab delimited records from the table.
           getColumnHeaders    ;; Returns the table headers as a tab delimited string.
           getNewRecord        ;; Returns a blank new record.
           importTab           ;; Imports ascii tab delimited records into the table.
           insert              ;; Insert a table row.
           isView              ;; Returns true iff the specified key is a saved view of this cursor.
           maximum             ;; Returns the max of a lambda value on each record of a table.
           minimum             ;; Returns the min of a lambda value on each record of a table.
           newIndex            ;; Creates unique index of each record in a table.
           omit                ;; Deletes those records from a table for which a lambda predicate is true.
           read                ;; Read a table row for later update.
           readByKey           ;; Read a table row for later update (using the specified index key).
           refExport           ;; Returns a record, for export, to the exportTab function.
           refImport           ;; Returns an empty record to the importTab function.
           reset               ;; Resets the backup copy and views of the investor colony table.
           restore             ;; Restores the backup copy of the investor colony table.
           restoreView         ;; Restore the specified record view.
           run                 ;; Run the specified javaFilter Lambda against this cursor.
           saveView            ;; Save the current record view for later retrieval.
           search              ;; Returns the row index for the first record for which a lambda predicate is true.
           setImport           ;; Receives a single record from the importTab function.
           sharpe              ;; Returns the sharpe ratio of a lambda value on each record of a table.
           show                ;; Shows a group of records starting from the specified row.
           sort                ;; Sort the table rows using a lambda sort function.
           total               ;; Totals a lambda value on each record of a table.
           totalForAll         ;; Totals a lambda value on ALL records of a table.
           truncate            ;; Truncates a table to those records for which a lambda predicate is true.
           updateView          ;; Updates each record in the current view and restores the altered backup view.
           viewMath            ;; Combines two saved views using a mathematical operator.
           write               ;; Write a table row, perhaps previously read, for update.
           writeByKey          ;; Write a table row (using the specified index key).
           ;; Private Child Lambdas
           __clear             ;; Clears the memory cursor.
           __errorStop         ;; Handles error conditions during sensitive operations.
           __open              ;; Begin an update transaction on a table object.
           __truncateEnds      ;; Truncate records from both ends of a table.
           __truncateMid       ;; Truncate records from the center of a table.
          ) ;; end of persistent variables
    vars:(newTable)
    ;; Initialize the inline child Lambdas.
    ;; *******************************************************************
    ;; Summary:  Averages a lambda value on each record of a table.
    ;; *******************************************************************
    (defun average(valueLambda)
       vars:(result) 
       (setq result (total valueLambda))
       (if (isPositive recordCount) 
           (/= result recordCount)
           (setq result #void)
           ) ; end if
       result) ; end average
    ;; *******************************************************************
    ;; Summary:  Averages a lambda value on ALL records of a table.
    ;; *******************************************************************
    (defun averageForAll(valueLambda)
       vars:(result) 
       (setq result (totalForAll valueLambda))
       (if (isPositive (length bckVector)) 
           (/= result (length bckVector))
           (setq result #void)
           ) ; end if
       result) ; end averageForAll
    (defun close()
    ;; *******************************************************************
    ;; Summary:  Terminates an update transaction on the table cursor.
    ;; *******************************************************************
       (__clear)
       (setq lastOperation #void)
       (setq myObjectName #void)
       (setq myIndex #void)
       (setq colVector #void)
       (setq colCount #void)
       (setq validFields #void)
       (setq colVector #void)
       (setq recordStructure #void)
       (setq myCursorType #void)
       (setq showLimit #void)
       (setq myParent #void)
       (setq myTableCursor #void)
       (setq myTableScore #void)
       (setq myCursorNotes #void)
       (setq viewDirectory #void)
       (setq myMemoPad #void)
       (setq colCount #void)
       true) ;; end of close
    (defun delete(row)
    ;; *******************************************************************
    ;; Summary:  Delete the specified row from the table object.
    ;; Args:     row      Row number of row to be deleted.
    ;; Return:   true     Always return true. 
    ;; *******************************************************************
       ;; Make sure the row number is valid.
       (if (or (< row 0) (>= row recordCount))
           (error (append "badRowIndex" 
                          "_memoryCursor:delete:" myObjectName " - " row
                          ":An attempt was made to delete with a bad row number.")))
       ;; Make sure the backup records are restored.
       (setq rowVector bckVector)
       (setq viewDirectory (new Directory:))
       (setq myIndex (new Directory:))
       (^delete rowVector row)
       (setq recordCount (length rowVector))
       true) ;; end of delete
    (defun delimitedCells(startRow endRow startCol endCol)
    ;; *******************************************************************
    ;; Summary:  Returns a block of cells as a tab delimited string.
    ;; *******************************************************************
       vars:(rowIndex colIndex dls tab cellValue record) 
       ;; Return a tab delimited string of cell values for
       ;; all cells between the following rows and columns.
       (setq dls "")
       (setq tab (append "" (char 09)))
       (if (> startRow endRow) (return dls))
       (loop for rowIndex from startRow to endRow do
          (if (< rowIndex recordCount)
              (setq record (read rowIndex))
              (setq record #void)
              ) ; end if
          (loop for colIndex from startCol to endCol do
              (setq cellValue record[colIndex])
              (if (isString cellValue) (setq cellValue (substitute cellValue tab " ")))
              (if (= cellValue #void) (setq cellValue ""))
              (if (isVector cellValue) (setq cellValue (string cellValue true)))
              (setq cellValue (append "" cellValue)) 
              (setq cellValue (substitute cellValue (char 09)  " "))
              (setq cellValue (substitute cellValue (char 13)  " "))
              (if (<> colIndex endCol) 
                  (setq dls (append dls cellValue (char 09)))
                  (setq dls (append dls cellValue))
                  ) ; end if
              ) ;;end column loop
          (if (<> rowIndex endRow) 
              (setq dls (append dls (char 13)))
              ) ; end if
          ) ;;end row loop
       (append dls "")) ;; end of delimitedCells
    (defun deviation(totalLambda)
    ;; *******************************************************************
    ;; Summary:  Returns the standard deviation of a lambda value on each 
    ;;           record of a table.
    ;; *******************************************************************
       vars:(rowIndex (result 0) score aSum aSsq anAvg)
       (if (<= recordCount 0) (return 0))
       ;; Compute the stadard devisation lambda value for each record in the table.
       (loop for rowIndex from 0 until recordCount do
           (setq score (totalLambda rowVector[rowIndex]))
           (setq aSum (+ aSum score))
           (setq aSsq (+ aSsq (* score score)))
           ) ;; end loop
       (setq anAvg (/ aSum recordCount))
       (setq result (sqrt (- (/ aSsq recordCount) (* anAvg anAvg))))
       result) ;; end deviation
    (defun drop()
    ;; *******************************************************************
    ;; Summary:  Drops all records from the table.
    ;; *******************************************************************
       (__clear)) ;; end of drop
    (defun dropView(key)
    ;; *******************************************************************
    ;; Summary: Drops the current record view as specified.
    ;; *******************************************************************
       (setq viewDirectory[key] 1)
       (delete viewDirectory key)
       true) ;; end of dropView
    (defun exportTab(fileName)
    ;; *******************************************************************
    ;; Summary:  Exports ascii tab delimited records from the table.
    ;; *******************************************************************
       ;; Make sure we catch all errors which might occur.
       (onError __errorStop)
       ;; Open the specified .tab file and export.
       (setq fileID (fileOpen fileName 1 0))
       (^exportTab fileID myParent recordVectors:)
       (fileClose fileID 1)
       (setq fileID #void)
       recordCount) ;; end exportTab
    (defun getColumnHeaders(startCol endCol) 
    ;; *******************************************************************
    ;; Summary:  Returns the table headers as a tab delimited string.
    ;; *******************************************************************
       vars:(colIndex dls colName)  
       (setq dls "")
       (loop for colIndex from startCol to endCol do
           (setq colName (string colVector[colIndex]))
           (if (= colName "#void") (setq colName ""))
           (if (= (left colName 1) "|") (setq colName (mid colName 1 (subi (length colName) 2))))
           (if (<> colIndex endCol) 
               (setq dls (append dls colName (char 09)))
               (setq dls (append dls colName))
               ) ; end if
           ) ;;end column loop
       (append dls "")) ;; end getColumnHeaders
    ;; Summary:  Returns a blank new record.
    (defun getNewRecord() (copy recordStructure))
    (defun importTab(fileName)
    ;; *******************************************************************
    ;; Summary:  Performs an import tab transaction on the table cursor.
    ;; *******************************************************************
       ;; Make sure we catch all errors which might occur.
       (onError __errorStop)
       ;; Make sure the backup records are restored.
       (setq rowVector bckVector)
       (setq recordCount (length rowVector))
       (setq viewDirectory (new Directory:))
       (setq myIndex (new Directory:))
       ;; Open the specified .tab file and import.
       (setq fileID (fileOpen fileName 0 0))
       (^importTab fileID myParent recordVectors:)
       (fileClose fileID 1)
       (setq fileID #void)
       recordCount) ;; end importTab
    (defun insert(row record)
    ;; *******************************************************************
    ;; Summary:  Insert a record into the table for update.
    ;; Args:     row      Row number of row to be inserted (before).
    ;;           record   Row to be written to the table database.
    ;; Return:   record   The row just written. 
    ;; *******************************************************************
       ;; Make sure the backup records are restored.
       (setq rowVector bckVector)
       (setq recordCount (length rowVector))
       (setq viewDirectory (new Directory:))
       (setq myIndex (new Directory:))
       ;; Make sure the row number is valid.
       (if (or (< row 0) (>= row recordCount))
           (error (append "badRowIndex" 
                          "_memoryCursor:insert:" myObjectName " - " row
                          ":An attempt was made to insert with a bad row number.")))
       (setq recordCount (addi recordCount 1))
       (setq record (copy record))
       (setAttributes record colVector)
       (^insert rowVector row record)
       ;; Return the record just inserted.
       record) ;; end of insert
    ;; Returns true iff the specified key is a saved view of this cursor.
    (defun isView(key) (<> viewDirectory[key] #void)) 
    (defun maximum(totalLambda)
    ;; *******************************************************************
    ;; Summary:  Returns the max of a lambda value on each record of a table.
    ;; *******************************************************************
       vars:(rowIndex result)
       (if (<= recordCount 0) (return 0))
       (setq result (totalLambda rowVector[0]))
       ;; Compute the min lambda value for each record in the table.
       (loop for rowIndex from 1 until recordCount do
           (setq result (max result (totalLambda rowVector[rowIndex])))
           ) ;; end loop
       result) ;; end maximum
    (defun minimum(totalLambda)
    ;; *******************************************************************
    ;; Summary:  Returns the min of a lambda value on each record of a table.
    ;; *******************************************************************
       vars:(rowIndex result)
       (if (<= recordCount 0) (return 0))
       (setq result (totalLambda rowVector[0]))
       ;; Compute the min lambda value for each record in the table.
       (loop for rowIndex from 1 until recordCount do
           (setq result (min result (totalLambda rowVector[rowIndex])))
           ) ;; end loop
       result) ;; end minimum
    (defun newIndex(colName)
    ;; *******************************************************************
    ;; Summary:  Creates unique index of each record in a table.
    ;; *******************************************************************
       vars:(record rowIndex numRecords colIndex)
       ;; Make sure the backup records are used to create the index.
       (setq colIndex (member (symbol colName) colVector))
       (if (= colIndex false) (error (append "_memoryCursor: unknown index field name[" colName "]")))
       (setq numRecords (length bckVector))
       (setq myIndex (new Directory:))
       (if (<= numRecords 0) (return numRecords))
       ;; Place all records in the index.
       (loop for rowIndex from 0 until numRecords do
           (setq record bckVector[rowIndex])
           (setq myIndex[record[colIndex]] rowIndex)
           ) ;; end loop
       numRecords) ;; end newIndex
    (defun omit(findLambda)
    ;; *******************************************************************
    ;; Summary:  Deletes those records from a table for which a lambda predicate is true.
    ;; *******************************************************************
       vars:(rowIndex)
       ;; Make sure the backup records are restored.
       (setq rowVector bckVector)
       (setq recordCount (length rowVector))
       (setq viewDirectory (new Directory:))
       (setq myIndex (new Directory:))
       (if (<= recordCount 0) (return recordCount))
       ;; Omit those rows for which the find Lambda returns true.
       (loop for rowIndex from 0 until (length rowVector) do
           (if (findLambda rowVector[rowIndex])
               (begin 
                  (delete rowIndex)
                  (-- rowIndex)
               )) ; end if
           ) ;; end loop
       (setq recordCount (length rowVector))
       recordCount) ;; end omit
    (defun read(row)
    ;; *******************************************************************
    ;; Summary:  Reads a record from the table.
    ;; Args:     row      Row number of row to be read.
    ;; Return:   record   The row just read. 
    ;; *******************************************************************
       (if (= row #void) (setq row 0))
       (if (or (< row 0) (> row recordCount))
           (error (append "badRowIndex" 
                          "_memoryCursor:read:" myObjectName " - " row
                          ":An attempt was made to read with a bad row number.")))
       rowVector[row]) ;; end of read
    (defun readByKey(key)
    ;; *******************************************************************
    ;; Summary:  Reads a record from the table (using the specified key).
    ;; Args:     key      Key of row to be read (found in index).
    ;; Return:   record   The row just read. 
    ;; *******************************************************************
       vars:(row)
       (setq row myIndex[key])
       (if (= row #void) (setq row -1))
       (if (or (< row 0) (> row recordCount))
           (error (append "badRowKey" 
                          "_memoryCursor:readByKey:" myObjectName "[" key
                          "] :An attempt was made to read with a bad index key.")))
       rowVector[row]) ;; end of readByKey
    (defun refExport(row)
    ;; *******************************************************************
    ;; Summary:  Returns a record, for export, to the exportTab function.
    ;; *******************************************************************
       ;; If the row is zero, return the column names for the header record.
       (if (= row 0) (return colVector))
       ;; Adjust the record index for the header record.
       (setq row (subi row 1))
       ;; If there are no more records, tell exportTab to stop exporting.
       (if (>= row recordCount) (return false))
       ;; Otherwise return the next record for export.
       (read row)) ;; end of refExport 
    (defun refImport(row)
    ;; *******************************************************************
    ;; Summary:  Returns an empty record to the importTab function.
    ;; *******************************************************************
       ;; If the row is zero, return an empty vector to hold the column names,
       ;; Otherwise, return an empty record (use the recordStructure as a template).
       (if (= row 0)
           (return (^new Vector: 0))
           (return (copy recordStructure)))) ;; end of refImport
    (defun reset()
    ;; *******************************************************************
    ;; Summary:  Resets the backup copy and views of the investor colony table. 
    ;; *******************************************************************
       ;; Make sure the backup records and views are restored.
       (setq rowVector bckVector)
       (setq recordCount (length rowVector))
       (setq viewDirectory (new Directory:))
       (setq myIndex (new Directory:))
       (setq UPDIamDirty true)
       true) ;; end of reset
    (defun restore()
    ;; *******************************************************************
    ;; Summary: Restores the backup copy of the investor colony table.
    ;; *******************************************************************
       (setq rowVector bckVector)
       (setq recordCount (length rowVector))
       true) ;; end of restore
    (defun restoreView(key)
    ;; *******************************************************************
    ;; Summary: Restores the current record view as specified.
    ;; *******************************************************************
       (setq rowVector viewDirectory[key])
       (if (= rowVector #void) (setq rowVector bckVector)) 
       (setq rowVector (copy rowVector))
       (setq recordCount (length rowVector))
       true) ;; end of restoreView
    ;; Run the specified javaFilter Lambda against this cursor.
    (defun run(filterString)
       vars:(filterLambda)
       (setq filterLambda (^dataMineLib.__compileLambda filterString))
       (filterLambda myParent)) ;; end of run
    (defun saveView(key)
    ;; *******************************************************************
    ;; Summary: Saves the current record view as specified.
    ;; *******************************************************************
       (setq viewDirectory[key] (copy rowVector))
       true) ;; end of saveView
    (defun search(findLambda ...)
    ;; *******************************************************************
    ;; Summary:  Returns the row index for the first record for which a lambda predicate is true.
    ;; *******************************************************************
       vars:(startIndex rowIndex n colName colValue (argc 2))
       ;; Define the temporary search lambda.
       (defun __searchLambda(x) vars:(colName colValue) (= x[colName] colValue))
       ;; If the findLambda is a columnName, we must make our own lambda predicate.
       (if (not (isLambda findLambda))
           (begin
              (setq colName findLambda)
              (setq colValue (argFetch 1))
              (++ argc)
              (setq findLambda __searchLambda)
              (setq findLambda.Tv.colName colName)
              (setq findLambda.Tv.colValue colValue)
           )) ; end if
       ;; Make sure we capture the startIndex argument (if any).
       (if (= (argCount) argc)
           (setq startIndex (argFetch (sub1 argc)))
           (setq startIndex 0)
           ) ; end if
       (if (<= recordCount 0) (return false))
       ;; Select the first row for which the find Lambda returns true.
       (setq n (length rowVector))
       (loop for rowIndex from startIndex until n do
           (if (findLambda rowVector[rowIndex]) (return rowIndex))
           ) ;; end loop
       false) ;; end search
    (defun setImport(row recordVector)
    ;; *******************************************************************
    ;; Summary:  Receives a single record from the importTab function.
    ;; *******************************************************************
       ;; Import the .tab column header record (if necessary).
       (if (= row 0)
           (if (= recordCount 0)
               (begin 
                  (setq validFields (objectToStructure recordVector #(#void)))
                  (setq colVector (refAttributes validFields))
                  (setq recordStructure (new Vector: (length colVector)))
                  (setAttributes recordStructure colVector)
                  (setq colCount (length colVector))
                  (return true)) ;; end setting new column names.
               (begin 
                 (setq recordVector (objectToStructure recordVector #(#void)))
                 (setq recordVector (refAttributes recordVector))
                 (if (<> colVector recordVector)
                      (error 
                         (append "importTab: An attempt was made to import a file with mismatched column names: "
                                 myObjectName)))
                  (return true)) ;; end checking new column names against old column names.
           )) ;; end if
       ;; Import all other .tab records at the end of the table.
       (write recordCount recordVector)
       true) ;; end of setImport
    (defun sharpe(totalLambda)
    ;; *******************************************************************
    ;; Summary:  Returns the sharpe ratio of a lambda value on each 
    ;;           record of a table.
    ;; *******************************************************************
       vars:(rowIndex (result 0) score aSum aSsq anAvg aStd)
       (if (<= recordCount 0) (return 0))
       ;; Compute the stadard devisation lambda value for each record in the table.
       (loop for rowIndex from 0 until recordCount do
           (setq score (totalLambda rowVector[rowIndex]))
           (setq aSum (+ aSum score))
           (setq aSsq (+ aSsq (* score score)))
           ) ;; end loop
       (setq anAvg (/ aSum recordCount))
       (setq aStd (sqrt (- (/ aSsq recordCount) (* anAvg anAvg))))
       (setq result (/ anAvg aStd))
       result) ;; end sharpe
    (defun show(startIndex) 
    ;; *******************************************************************
    ;; Summary:  Shows a group of records starting from the specified row.
    ;; *******************************************************************
       vars:(i n) 
       (setq n (integer (min recordCount (addi startIndex showLimit))))
       (loop for i from startIndex until n do
           (writeln "[" i "] " (read i))
           ) ;; end loop
       true) ;; end show
    (defun sort(sortLambda ...)
    ;; *******************************************************************
    ;; Summary:  Sorts each record in the table cursor.
    ;; *******************************************************************
       vars:(backupSW)
       ;; Are we sorting the backup view as well as the current view?
       (if (and (= (argCount) 2) (= (argFetch 1) backup:))
           then ;; Sort the backup view as well as the current view.
           (begin
              (setq backupSW true)
              (setq viewDirectory (new Directory:))
              (setq myIndex (new Directory:))
              (setq rowVector bckVector)
              (setq recordCount (length rowVector))
              ) ; end then
           else ;; Sort only the current view.
           (begin
              (setq backupSW false)
              (setq rowVector (copy rowVector))
              )) ; end if
       (if (<= recordCount 0) (return recordCount))       
       (^sort rowVector sortLambda)
       (setq UPDIamDirty true)
       recordCount) ;; end sort
    (defun total(totalLambda)
    ;; *******************************************************************
    ;; Summary:  Totals a lambda value on each record of a table.
    ;; *******************************************************************
       vars:(rowIndex result)
       (if (<= recordCount 0) (return 0))
       ;; Compute the total lambda value for each record in the table.
       (loop for rowIndex from 0 until recordCount do
           (+= result (totalLambda rowVector[rowIndex]))
           ) ;; end loop
       result) ;; end total
    (defun totalForAll(totalLambda)
    ;; *******************************************************************
    ;; Summary:  Totals a lambda value on ALL records of a table.
    ;; *******************************************************************
       vars:(rowIndex (result 0))
       (if (<= (length bckVector) 0) (return 0))
       ;; Compute the total lambda value for each record in the table.
       (loop for rowIndex from 0 until (length bckVector) do
           (+= result (totalLambda bckVector[rowIndex]))
           ) ;; end loop
       result) ;; end totalForAll
    (defun truncate(findLambda)
    ;; *******************************************************************
    ;; Summary:  Truncates a table to those records for which a lambda predicate is true.
    ;; *******************************************************************
       vars:(rowIndex newIndex n vec)
       (if (<= recordCount 0) (return recordCount))
       (setq rowVector (copy rowVector))
       (setq recordCount (length rowVector))
       ;; If the selectLambda is a number, then keep only the first N records.
       (if (isNumber findLambda)
           (begin
              (setq maxRecords (min recordCount findLambda))
              (setq rowVector (resize rowVector maxRecords))
              (setq recordCount (length rowVector))
              (return recordCount)
           )) ; end if
       ;; Select only those rows for which the find Lambda returns true.
       (setq n (length rowVector))
       (setq vec (^new Vector: object: 0))
       (setq newIndex -1)
       (loop for rowIndex from 0 until n do
           (if (findLambda rowVector[rowIndex]) (setq vec[(++ newIndex)] rowVector[rowIndex]))
           ) ;; end loop
       (setq rowVector vec)
       (setq recordCount (length rowVector))
       recordCount) ;; end truncate
    (defun updateView(updateLambda ...)
    ;; *******************************************************************
    ;; Summary:  Updates each record in the current view and restores the altered backup view.
    ;; *******************************************************************
       vars:(_rowVector rowIndex n resetSW)
       ;; Check for a no reset request.
       (setq resetSW true)
       (if (and (= (argCount) 2) (= (argFetch 1) noreset:)) (setq resetSW false))
       ;; Update each row in the current view.
       (setq _rowVector (copy rowVector))
       (setq n (length _rowVector))
       (loop for rowIndex from 0 until n do
           (updateLambda _rowVector[rowIndex])
           ) ;; end loop
       (if (= resetSW true) (restore) cursor)) ;; end updateView
    (defun viewMath(operator key1 key2)
    ;; *******************************************************************
    ;; Summary: Combines two saved views using a mathematical operator.
    ;; *******************************************************************
       (if (= operator and:) 
           (setq rowVector (__viewAND (new Vector: object: 0) viewDirectory[key1] viewDirectory[key2])))
       (if (= operator or:) 
           (setq rowVector (__viewOR (new Vector: object: 0) viewDirectory[key1] viewDirectory[key2])))
       (if (= operator xor:) 
           (setq rowVector (__viewXOR (new Vector: object: 0) viewDirectory[key1] viewDirectory[key2])))
       (setq recordCount (length rowVector))) ;; end of viewMath
    (defun write(row record)
    ;; *******************************************************************
    ;; Summary:  Write a record to the table for update.
    ;; Args:     row      Row number of row to be written.
    ;;           record   Row to be written to the table.
    ;; Return:   record   The row just written. 
    ;; *******************************************************************
       ;; Make sure the backup records are restored.
       (setq rowVector bckVector)
       (setq recordCount (length rowVector))
       (setq viewDirectory (new Directory:))
       (setq myIndex (new Directory:))
       ;; Make sure the row number is valid.
       (if (or (< row 0) (> row recordCount))
           (error (append "badRowIndex" 
                          "_memoryCursor:write:" myObjectName " - " row
                          ":An attempt was made to write with a bad row number.")))
       (if (= row recordCount) (setq recordCount (addi recordCount 1)))
       (setq record (copy record))
       (setAttributes record colVector)
       (setq rowVector[row] record)
       ;; Return the record just written.
       record) ;; end of write
    (defun writeByKey(key record)
    ;; *******************************************************************
    ;; Summary:  Write a record to the table for update (using the specified key).
    ;; Args:     key      Index key of row to be written.
    ;;           record   Row to be written to the table.
    ;; Return:   record   The row just written. 
    ;; *******************************************************************
       vars:(row)
       ;; Make sure the backup records are restored.
       (setq rowVector bckVector)
       (setq recordCount (length rowVector))
       (setq viewDirectory (new Directory:))
       (if (not (isDirectory myIndex)) (setq myIndex (new Directory:)))
       (setq row myIndex[key])
       (if (= row #void) (begin (setq row recordCount) (setq myIndex[key] row)))
       ;; Make sure the row number is valid.
       (if (or (< row 0) (> row recordCount))
           (error (append "badRowKey" 
                          "_memoryCursor:writeByKey:" myObjectName "[" key
                          "] :An attempt was made to write with a bad record key.")))
       (if (= row recordCount) (setq recordCount (addi recordCount 1)))
       (setq record (copy record))
       (setAttributes record colVector)
       (setq rowVector[row] record)
       ;; Return the record just written.
       record) ;; end of writeByKey
    ;; -------------------------------------
    ;; Private methods (not for public use).
    ;; -------------------------------------
    (defun __clear()
       (setq rowVector (new Vector: object: 0))
       (setq bckVector rowVector)
       (setq viewDirectory (new Directory:))
       (setq myIndex #void)
       (setq recordCount 0)
       (setq fileID #void)
       #void) ;; end of __clear
    (defun __open(tableName colNames)
    ;; *******************************************************************
    ;; Summary:  Begins an update transaction on the table cursor.
    ;; *******************************************************************
       ;; Search for the specified table in the reference and blackboard areas.
       vars:(indexOf record)
       (__clear)
       (setq myObjectName (symbol tableName))
       (setq colVector (copy colNames))
       (setq colCount (length colVector))
       (setq validFields (objectToStructure colVector #(#void)))
       (setq colVector (refAttributes validFields))
       (setq recordStructure (new Vector: colCount))
       (setAttributes recordStructure colVector)
       ;; Load the memo pad Dictionary into the cursor memory.
       (setq myCursorType memory:)
       (setq showLimit 10)
       (setq myIndex #void)
       (setq myObjectName (symbol tableName))
       (setq myTableCursor #void)
       (setq myTableScore #void)
       (setq myCursorNotes (new Dictionary:))
       (setq viewDirectory (new Directory:))
       (setq myMemoPad (new Dictionary:))
       (setq colCount 0)
       true) ;; end of __open
    (defun __errorStop(errMsg)
       (if (isNumber fileID) (fileClose fileID 1))
       (setq fileID #void)
       (error (mid errMsg 1 (subi (length errMsg) 2)))) ;; end __errorStop
    (defun __truncateEnds(begRowLimit endRowLimit)
    ;; *******************************************************************
    ;; Summary:  Truncate records from both ends of a table.
    ;; *******************************************************************
       vars:(rowIndex newIndex n vec endRowIndex)
       (if (<= recordCount 0) (return recordCount))
       ;; Select only those rows in between the begin and end row limits.
       (setq n (length rowVector))
       (setq endRowIndex (subi n endRowLimit))
       (setq vec (^new Vector: object: 0))
       (setq newIndex -1)
       (loop for rowIndex from (addi begRowLimit 1) until endRowIndex do
           (setq vec[(++ newIndex)] rowVector[rowIndex])
           ) ;; end loop
       (setq rowVector vec)
       (setq recordCount (length rowVector))
       recordCount) ;; end truncateEnds
    (defun __truncateMid(begRowLimit endRowLimit)
    ;; *******************************************************************
    ;; Summary:  Truncate records from the center of a table.
    ;; *******************************************************************
       vars:(rowIndex newIndex n vec endRowIndex)
       (if (<= recordCount 0) (return recordCount))
       ;; Delete those rows in between the begin and end row limits.
       (setq n (length rowVector))
       (setq endRowIndex (subi n endRowLimit))
       (setq vec (^new Vector: object: 0))
       (setq newIndex -1)
       (loop for rowIndex from 0 until begRowLimit do
           (setq vec[(++ newIndex)] rowVector[rowIndex])
           ) ;; end loop
       (loop for rowIndex from endRowIndex until n do
           (setq vec[(++ newIndex)] rowVector[rowIndex])
           ) ;; end loop
       (setq rowVector vec)
       (setq recordCount (length rowVector))
       recordCount) ;; end truncateMid
    ;; *******************************************************************
    ;; Begin MAIN logic section.
    ;; Note: Initialize this memory table cursor.
    ;; *******************************************************************
    Continue::
    (setq newTable (new (myself)))
    (setq newTable.myParent newTable)
    (setq newTable.myDataMine ^dataMineLib)
    (newTable.__open tableName colNames)
    newTable) ;; end of tempTable






;;**EXPORTKEY**:dataMineLib.updateTable
(defchild dataMineLib:updateTable(tableName updateLambda)
   ;; *******************************************************************
   ;; summary:  Updates a table by updating the contents of the
   ;;           fields in the records of the table.
   ;;
   ;; Args:     tableName    table to be fixed up.
   ;;           updateLambda  The record update Lambda.
   ;;
   ;; Return:   true
   ;; *******************************************************************
	vars:(cursor record index)
	(STARTTIME)
	(setq cursor (open tableName))
	(setq index 0)
	(cursor.restore)
	(loop for index from 0 until cursor.recordCount do      
		(setq record (cursor.read index))      
		(updateLambda record)
		(cursor.write index record)
	);index
	(close cursor)  
	(SETTIME)
	true) ;; end of updateTable



























































































